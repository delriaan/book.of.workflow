# ::::: EXTERNAL DATA MANAGEMENT
make_query <- function(this.conn, from, from.mod	= "", where	= "1=1 ", sel	= "*", sel.mod = "", group_by	= NULL, having = NULL, order_by	= NULL, ...){
#' SQL Query Maker
#'
#' \code{make_query} is a helper function to create a SQL query string with a focus on T-SQL.  For column selection, the following are valid:
#' \itemize{
#'   \item \code{"*"} The default, denoting all column names
#'   \item A vector of column names (e.g., \code{"col1, col2, col3"})
#'   \item \code{TRUE}: This invokes \code{\link[tcltk]{tk_select.list}} for interactive selection
#'   \item A string that, when parsed, becomes a single-argument mapper for use in \code{\link[purrr]{keep}} (e.g., \code{"~.x \%ilike\% \"(pat|bp)\""})
#'   \item A single-argument function serving as the mapper in \code{\link[purrr]{keep}}
#' }
#'
#' @param this.conn A \code{\link[DBI]{DBI-package}} connection object
#' @param from	(string) The object segment of a fully-qualified object name
#' @param from.mod (string) Table table hints (e.g., \code{WITH (NOLOCK)}) and hard-coded subsequent JOINs
#' @param sel		(string|logical | "*") The columns to select (see 'Details')
#' @param sel.mod	(string | "") Modifiers to the 'SELECT' statement (e.g., DISTINCT, TOP n, etc ...)
#' @param where	(string) "WHERE" criteria
#' @param group_by (string) "GROUP BY" criteria
#' @param having (string) "HAVING" criteria
#' @param order_by (string) "ORDER BY" criteria
#' @param ... (Not used)
#'
#' @return A (T-)SQL string for use in an RDBMS call
#'
#' @family Data transmission
#'
#' @export

	tbl_cols = DBI::dbListFields(this.conn, from);

	# Create and return the query string
	sprintf(
		"SELECT %s FROM %s WHERE %s%s%s%s%s"
		, paste(
				ifelse(grepl("MySQL", conn@info$dbms.name, ignore.case = TRUE), "", sel.mod)
				, switch(
						class(sel)[1]
						, "logical" 		= tcltk::tk_select.list(tbl_cols, multiple = TRUE, title = sprintf("%s: Choose columns", from))
						, "expression"	= purrr::keep(tbl_cols, purrr::as_mapper(eval(sel)))
						, "function"		= purrr::keep(tbl_cols, sel)
						, "character" 	= sel
						, tbl_cols
						) |> paste(collapse = ", ")
				, sep = " "
			)
		, paste(from, from.mod, sep = " ")
		, where
		, ifelse(rlang::is_empty(group_by), "", paste0(" GROUP BY "	, group_by))
		, ifelse(rlang::is_empty(having)	, "", paste0(" HAVING"		, having))
		, ifelse(rlang::is_empty(order_by), "", paste0(" ORDER BY "	, order_by))
		, ifelse(!grepl("MySQL", conn@info$dbms.name, ignore.case = TRUE), "", sel.mod)
		);
}
#
get_data <- function(this.conn, src.name = NULL, tgt.name = NULL, this.data = NULL, post.op = eval, chatty = FALSE, promise = FALSE, persist.conn = TRUE, ...){
#' Retrieve T-SQL Data
#'
#' The workflow for this function is to retrieve the data from SQL Server, post-process it according to the value of argument \code{post.op}, and, when \code{tgt.name} is not \code{NULL}, assign it to an object named according to the value of \code{tgt.name}
#'
#' @param this.conn A \code{\link[DBI]{DBI-package}} connection object
#' @param src.name (string) Data source name
#' @param tgt.name (string | \code{NULL}) When not \code{NULL}, the name of the R workspace object to use when assigning the output. Use format \code{obj_name@@env_name} to specify a target environment to assign the object
#' @param post.op (object) A function or list of functions to process the retrieved dataset (argument #2) before assigning to workspace
#' @param this.data (expression) When not \code{NULL}, an R object for post-operation only
#' @param chatty (logical | \code{FALSE}) Verbosity flag
#' @param promise (logical | \code{FALSE}) When \code{TRUE}, assignment is done via a call to \code{\link[future]{futureAssign}}
#' @param persist.conn (logical | \code{TRUE}) When \code{FALSE}, the connection will be closed at the end of execution
#' @param ... Arguments to be used by function \code{\link{make_query}}
#'
#' @return Invisibly, the retrieved, post-processed dataset
#'
#' @family Data transmission
#'
#' @export

	post_op.check = purrr::as_mapper(~{
		if (rlang::is_empty(post.op)){ .x	} else if (is.function(post.op)){	post.op(.x)	} else if (is.list(post.op)){	magrittr::freduce(.x, post.op)	} else { .x }
	});

	this.conn = check.db_conn(this.conn);

	# Execute data retrieval and (optionally) assign the output
	{ if (is.null(this.data)){
			# Get the data
			.callArgs = list(this.conn = this.conn, from = src.name) |> append(list(...));
			if (chatty) { print(list(call_args = .callArgs)) }

			DBI::dbGetQuery(conn = this.conn, statement = do.call(make_query, args = .callArgs))
		} else {
			eval(this.data)
		}
	} |>
	post_op.check() %>% {
			if (!persist.conn){ DBI::dbDisconnect(this.conn) }
			output = .;

			if (!is.null(tgt.name)) {
				stringi::stri_split_fixed(tgt.name, "@", n = 2, simplify = TRUE) %>% {  if (.[2] == ""){ c(.[1], ".GlobalEnv") } else { . } } |>
				rlang::set_names(c("obj", "env")) %>%
				as.list() %$% {
					substitute(
						future::futureAssign(x = obj, value = output
												 , assign.env = if (env %in% search()){ as.environment(env) } else { get(env) }, lazy = promise)
						, list(obj = obj, output = output, env = env, promise = promise)
						)
					} |> eval();
			} else { invisible(output) }
	}
}
#
export_data <- function(this.conn, out.data, tbl = NULL, sch = "dbo", append = FALSE, persist.conn = TRUE, ...){
#' Export Tabular Data From R to DBMS
#'
#' \code{export_data} serves as a wrapper for \code{\link[DBI]{dbWriteTable}} and \code{\link[DBI]{dbAppendTable}}. When using \code{append=FALSE}, the operation is destructive as the target table is dropped if it exists, so use this function with caution.
#'
#' @param this.conn A \code{\link[DBI]{DBI-package}} connection object
#' @param out.data The data to write to the database, preferably a data.frame or coercible
#' @param tbl (string) The name of the table to drop (if it exists) and create
#' @param sch (string) The name of the schema in which to create the table
#' @param append (logical)	Should the data be appended to existing data rather than replacing existing data?
#' @param persist.conn (logical | \code{TRUE}) When \code{FALSE}, the connection will be closed at the end of execution
#' @param ... Additional arguments passed to \code{\link[DBI]{dbWriteTable}}
#'
#' @family Data transmission
#'
#' @export

	if (!data.table::is.data.table(out.data)){ .out.data <- data.table::as.data.table(out.data) }
	regex.remove <- "([-][-])|(/[*])|([*]/)";
	this.conn <- check.db_conn(this.conn);
	.args <- list(conn = this.conn, name = DBI::Id(schema = sch, table = tbl), value = out.data);
	.args[...names()] <- rlang::list2(...);

	if (!append){
		if (DBI::dbExistsTable(conn = .args$conn, name = .args$name)){ DBI::dbRemoveTable(.args$conn, name = .args$name)}
		do.call(DBI::dbWriteTable, args = .args);
	} else {
		do.call(DBI::dbAppendTable, args = .args)
	}

	if (!persist.conn){ DBI::dbDisconnect(this.conn) }
}
#
check.db_conn <- function(this.conn, pass, ...){
#' Check a Database Connection (OBSOLETE)
#'
#' \code{check.db_conn} Validates a DBI connection and reconnects if it is invalid by using the connection's stored settings.
#'
#' @param this.conn A \code{\link[DBI]{DBI-package}} connection object
#' @param pass The password to use if required: raw input is converted to character
#' @param ... Not used
#'
#' @return An active DBI connection object
#'
#' @family Data transmission
#'
#' @export

	invisible(this.conn)
}

#' @export
do.make_query <- make_query

#' @export
do.get_data <- get_data

#' @export
do.export_data <- export_data