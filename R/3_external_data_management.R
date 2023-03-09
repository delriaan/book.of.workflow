# ::::: EXTERNAL DATA MANAGEMENT
do.make_query <- function(this.conn, from, from.mod	= "", where	= "1=1 ", sel	= "*", sel.mod = "", grp.by	= NULL, having = NULL, ord.by	= NULL, ...){
#' T-SQL Query Maker
#'
#' \code{do.make_query} creates a valid T-SQL query string.  For column selection, the following are valid:
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
#' @param grp.by (string) "GROUP BY" criteria
#' @param having (string) "HAVING" criteria
#' @param ord.by (string) "ORDER BY" criteria
#'
#' @return A (hopefully) valid T-SQL string
#'
#' @family Data transmission
#'
#' @export

	tbl_cols = DBI::dbListFields(this.conn, from);

	# Create the query string
	output = sprintf(
		"SELECT %s FROM %s WHERE %s%s%s%s"
		, paste(
				sel.mod
				, switch(
					class(sel)[1]
					, "logical" 		= tcltk::tk_select.list(tbl_cols, multiple = TRUE, title = sprintf("%s: Choose columns", from))
					, "expression"	= purrr::keep(tbl_cols, purrr::as_mapper(eval(sel)))
					, "function"		= purrr::keep(tbl_cols, sel)
					, "character" 	= sel
					, tbl_cols
					) %>% paste(collapse = ", ")
				, sep = " "
			)
		, paste(from, from.mod, sep = " ")
		, where
		, ifelse(is.null(grp.by), "", paste0(" GROUP BY "	, grp.by))
		, ifelse(is.null(having), "", paste0(" HAVING"		, having))
		, ifelse(is.null(ord.by), "", paste0(" ORDER BY "	, ord.by))
		);

	# Return the query
	output;
}
#
do.get_data <- function(this.conn, src.name = NULL, tgt.name = NULL, this.data = NULL, post.op = eval, verbose = FALSE, promise = FALSE, persist.conn = TRUE, ...){
#' Retrieve T-SQL Data
#'
#' The workflow for this function is to retrieve the data from SQL Server, post-process it according to the value of argument \code{post.op}, and, when \code{tgt.name} is not \code{NULL}, assign it to an object named according to the value of \code{tgt.name}
#'
#' @param this.conn A \code{\link[DBI]{DBI-package}} connection object
#' @param src.name (string) Data source name
#' @param tgt.name (string | \code{NULL}) When not \code{NULL}, the name of the R workspace object to use when assigning the output. Use format \code{obj_name@@env_name} to specify a target environment to assign the object
#' @param post.op (object) A function or list of functions to process the retrieved dataset (argument #1) before assigning to workspace
#' @param this.data (expression) When not \code{NULL}, an R object for post-operation only
#' @param verbose (logical | \code{FALSE}) When \code{TRUE}, interim output is printed to console
#' @param promise (logical | \code{FALSE}) When \code{TRUE}, assignment is done via a call to \code{\link[future]{futureAssign}}
#' @param persist.conn (logical | \code{TRUE}) When \code{FALSE}, the connection will be closed at the end of execution
#' @param ... Arguments to be used by function \code{\link{do.make_query}}
#'
#' @return Invisibly, the retrieved, post-processed dataset
#'
#' @family Data transmission
#'
#' @export

	post_op.check = purrr::as_mapper(~{
		if (rlang::is_empty(post.op)){
			.x
		} else if (is.function(post.op)){
			post.op(.x)
		} else if (is.list(post.op)){
			magrittr::freduce(.x, post.op)
		} else { .x }
	});

	this.conn = check.db_conn(this.conn);

	# Execute data retrieval and (optionally) assign the output
	{ if (is.null(this.data)){
		# Get the data
		.callArgs = list(this.conn = this.conn, from = src.name) |> append(list(...));
		if (verbose) { print(list(call_args = .callArgs)) }

		DBI::dbGetQuery(conn = this.conn, statement = do.call(do.make_query, args = .callArgs))
	} else { eval(this.data) } } %>% post_op.check %>% {
		if (!persist.conn){ DBI::dbDisconnect(this.conn) }
		output = .;

		if (!is.null(tgt.name)) {
			tgt.name %>%
				stringi::stri_split_fixed("@", n = 2, simplify = TRUE) %>% {
					if (.[2] == ""){ c(.[1], ".GlobalEnv") } else { . }
				} |>
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
do.export_data <- function(out.data, this.conn, tbl = NULL, sch = "dbo", post.cmd = NULL, append = FALSE, persist.conn = TRUE, ...){
#' Export Tabular Data From R to DBMS
#'
#' \code{do.export_data} serves as a wrapper for \code{\link[DBI]{dbWriteTable}}. When using \code{append=FALSE}, the operation is destructive as the target table is dropped if it exists, so use this function with caution.
#' The parameter `post.cmd` is provided
#'
#' @param out.data The data to write to the database, preferably a data.frame or coercible
#' @param this.conn A \code{\link[DBI]{DBI-package}} connection object
#' @param tbl (string) The name of the table to drop (if it exists) and create
#' @param sch (string) The name of the schema in which to create the table
#' @param post.cmd (string) When provided, a post-table creation call to sqlQuery() will be executed with `post.cmd` as the query string: use this for subsequent operations such as indexing, execution of additional stored procedures, etc.
#' @param append (logical)	Should the data be appended to existing data rather than replacing existing data?
#' @param persist.conn (logical | \code{TRUE}) When \code{FALSE}, the connection will be closed at the end of execution
#' @param ... Additional arguments passed to \code{\link[DBI]{dbWriteTable}}
#'
#' @family Data transmission
#'
#' @export

	if (!data.table::is.data.table(out.data)){ .out.data <- data.table::as.data.table(out.data) }
	regex.remove = "([-][-])|(/[*])|([*]/)";
	this.conn = check.db_conn(this.conn);

	.args = list(conn = this.conn, name = DBI::Id(schema = sch, table = tbl), value = out.data, append = append);
	.args[...names()] <- rlang::list2(...);

	do.call(DBI::dbWriteTable, args = .args);

	# Execute additional actions on DBMS post-transfer
	if (!is.null(post.cmd)) { DBI::dbSendQuery(conn = this.conn, statement = post.cmd %>% stringi::stri_replace_all_regex(regex.remove, "")) }

	if (!persist.conn){ DBI::dbDisconnect(this.conn) }
}
#
check.db_conn <- function(this.conn, pass, ...){
#' Check a Database Connection
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

	if (!any(class(this.conn) %in% c("ODBCConnection", "Microsoft SQL Server"))){
		message("Connection is not DBI-compatible: returning as-is ...");
		return(this.conn);
	}

	pass = if (missing(pass)){ NULL } else if (is.raw(pass)){ rawToChar(pass) } else { pass };

	invisible(switch(
		EXPR = class(this.conn)[1]
		, "ODBCConnection"  = {
				.args = this.conn@odbc %>% attr("call") %>% as.list() %$% {
								c(case = case
									, stringi::stri_split_fixed(connection, ";", simplify = TRUE) |>
											keep(~.x %like% "DSN|UID|case") |>
											stringi::stri_replace_first_regex("[A-Z]{3}[=]", "") |>
											rlang::set_names(c("dsn", "uid"))
									);
							}
				this.conn
			}, { if (!DBI::dbIsValid(this.conn)){ DBI::dbConnect(drv = odbc::odbc(), dsn = this.conn@info$sourcename) } else { this.conn } }
		))
}
