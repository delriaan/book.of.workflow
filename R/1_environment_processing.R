# ::::: ENVIRONMENT PROCESSING
do.load_unloaded <- function(..., libs = NULL, pattern = "[,|; ]", autoinstall = FALSE, chatty = FALSE) {
#' Load Unloaded Packages
#'
#' \code{do.load_unloaded}checks the packages provided in \code{libs} against a call to \code{\link[base]{search}} and only makes a call to \code{\link[base]{library}} for unloaded (and attached) libraries.
#'
#' Library names can be declared in a single, delimited string (e.g., \code{"name0 name1, name2|name3"}) or as a vector of strings (e.g. \code{c("name0", "name1", "name2")}).
#' Inclusions and exclusions can be declared using the following template:\cr  \code{"library_name{+name0+name1+...}"} for inclusions and \code{"library_name{-name0-name1-...}"}.  Since inclusions and exclusions cannot be used in the same call to \code{\link[base]{library}}(), trying to do so with this function will result in an error.
#'
#' @param ... \code{\link[rlang]{dots_list}} A vector of packages to load given as characters.  Delimited strings are allowed: \emph{DO NOT use \code{+} or \code{-}}.
#' @param libs (string[]) A vector of packages to load given as characters or symbols.  Delimited strings are allowed: \emph{DO NOT use \code{+} or \code{-}}.
#' @param pattern  (string | "[, ]") A regex delimiter pattern that operates on `str`: \emph{DO NOT use \code{+} or \code{-}}.
#' @param autoinstall:\cr
#' \itemize{
#'	\item{\code{FALSE} (default): A notification message is provided if \code{chatty} is \code{TRUE}; otherwise, no message is sent}
#'	\item{\code{TRUE}: If a library isn't installed, it will be using the default library installation path}
#'	\item{\code{list()}: Assumes \code{TRUE} and should be a list of arguments (excluding \code{pkgs}) to send to \code{\link[utils]{install.packages}}}
#'	}
#' @param chatty (logical | \code{FALSE}) Sets the \code{quietly} argument in the call to \code{\link[base]{library}}()
#'
#' @return See `library()`
#' @family Environment Processing
#' @export

	libs = c(as.character(rlang::enexprs(...)), libs);

	if (chatty){ cat(libs, sep = "\n") }

	library.cfg = stringi::stri_split_regex(libs, "[, |;]", simplify = TRUE, omit_empty = TRUE) %>%
		as.vector() %>%
		stringi::stri_replace_all_regex(., "([{].*.[}])?", replacement = "", simplify = TRUE) %>%
		as.vector() %>%
		rlang::set_names() %>%
		purrr::map(~{
			this.str = stringi::stri_split_regex(.x, pattern = "[{}]", omit_empty = TRUE, simplify = TRUE) %>% unlist();
			purrr::map(
				.x = rlang::set_names(c("[+]", "[-]"), c("include.only", "exclude"))
				, .f = ~{ stringi::stri_extract_all_regex(this.str, paste0(.x, "[a-zA-Z.0-9]+"), simplify = TRUE) %>% na.omit() %>%
									stringi::stri_replace_all_regex(pattern = "[+-]", replacement = "", vectorize_all = FALSE)
							})
		});

	if (chatty){ print(library.cfg) }

	new.libs = purrr::keep(names(library.cfg), ~!.x %in% rownames(installed.packages()));

	if (!rlang::is_empty(new.libs)){
		if (is.list(autoinstall)){
			rlang::inject(install.packages(pkgs = new.libs, !!!autoinstall))
		} else if (is.logical(autoinstall)==autoinstall){ install.packages(pkgs = new.libs) }
	}

	invisible(
		library.cfg[
			# Don't try to load libraries already loaded and attached via indexing against `library.cfg`
			purrr::map_lgl(names(library.cfg), ~{ not(any(grepl(.x, search()))) })
			] %>% purrr::imap(~tryCatch(expr = { do.call(
				what = "library"
				, args = append(
						.x[c(length(.x$include) > 0, length(.x[[2]]) > 0)] # <- User syntax error can lead to both being true, so let the call to 'library()' error out
						, list(package = .y, attach.required = TRUE, quietly = !chatty, warn.conflicts = FALSE)
						))
				}, error = function(e){ message(e) }))
		)
}
#
do.save_image <- function(..., safe = TRUE, env = .GlobalEnv, save.dir = getwd(), file.name = "", use.prefix = TRUE, use.timestamp = TRUE, prepare = NULL){
#' Manual Export of Workspace Objects
#'
#' The default value for \code{i} exports the entire workspace.  Unless `file` is \code{NULL}, when \code{i} is a vector of names or delimited string of names, the file name becomes 'multiObs'; otherwise, the file name is set to the value of \code{i}. When {i} contains 'all' or '*', regardless of the full content of \code{i}, the entire workspace is exported.
#'
#' @param ... (vector or list) Names of objects to save given as strings or symbols. Strings may be delimited (\code{c(',', ';', '|', ' ')})
#' @param safe	(logical | \code{TRUE}) Should the pending action be confirmed at the prompt?
#' @param env	The environment to search for items
#' @param save.dir (string | \code{getwd()}) The directory to save to (not the file name).  Use \code{TRUE} to interactively choose a save directory.
#' @param file.name	(string | "") The name of the file to save, or, when \code{NULL}, the value of \code{i} if atomic or a predefined name when \code{i} is a vector
#' @param use.prefix (logical | \code{TRUE}) When \code{TRUE} (the default), the file name is prefixed with the value of \code{env}
#' @param use.timestamp (logical | \code{TRUE}) When \code{TRUE} (the default), the file name is appended with a formatted value of \code{Sys.time()}
#' @param prepare	(language | \code{NULL}) A quoted expression that executes before the save action takes place.
#'
#' @return A `.rdata` file, the filename of which being suffixed with a timestamp formatted as "yyyy.mm.dd.hhmmss"
#'
#' @family Environment Processing
#' @export

	# :: Preliminary checks on supplied parameters
	obj.nms <- NULL;
	env_ref <- as.character(rlang::enexpr(env));

	if ("character" %in% class(env)){
		env <- if (env %in% search()) { env <- as.environment(env) } else { eval(as.symbol(env)) }
	}

	if (!is.null(prepare)) { eval(prepare, envir = env) }

	if (...length() == 0){ obj.nms <- ls(envir = env) }

	i =  purrr::map(as.character(rlang::enexprs(...)), ~stringi::stri_split_regex(.x, pattern = "[,;| ]", simplify = TRUE, omit_empty = TRUE)) |> unlist()

	if (rlang::is_empty(i)){ i <- "all" }

	if (save.dir == TRUE){ save.dir <- tcltk::tk_choose.dir(default = getwd(), caption = "Choose the 'save' directory:") }

	filecheck = is.null(file.name) | (stringi::stri_length(trimws(file.name)) == 0)

	# :: Environment reference from string
	namescheck = { c(
		all = { any(i %in% c("*", "all")) }
		, single.obj = { (length(i) == 1) & !any(i %in% c("*", "all")) & (stringi::stri_length(i) > 0)}
		, multi.as.vector = { (length(i) > 1) & !any(i %in% c("*", "all")) }
		, multi.as.string = { any((i %like% "[,;| ]") & !any(i %in% c("*", "all"))) }
		) %>%
		purrr::keep(~.x) %>% names() %>% .[1]
	}

	# :: Set the file name based on the values supplied to argument `i`
	switch(
		namescheck
		, "all" 						= { file.name <- ifelse(filecheck, "all", file.name); i <- ls(envir = env, all.names = TRUE) }
		, "multi.as.string" = { file.name <- ifelse(filecheck, "multiObs", file.name);
														i <- stringi::stri_split_regex(i, "[,;| ]", omit_empty = TRUE, simplify = TRUE) %>% c();
														}
		, "multi.as.vector" = { file.name <- ifelse(filecheck, "multiObs", file.name) }
		, 										{ file.name <- ifelse(filecheck, i, file.name) }
		);

	# :: Write the binary file to disk
	tstamp = ifelse(use.timestamp, paste0("_", format(Sys.time(), "%Y.%m.%d.%H%M%S")), "");

	tmp.file = sprintf("%s/%s%s%s.rdata", save.dir, if (use.prefix){ ifelse(env_ref %like% "Global", "", paste0(env_ref, "$")) } else { "" }, file.name, tstamp);

	if (safe) {
		msg = { sprintf("Preparing to save %s[%s] to %s: \nContinue? ", env_ref, paste(i, collapse = ", "), tmp.file) }
		if (askYesNo(msg, prompt = "Y/N/C")) {
			save(list = i, envir = env, file = tmp.file, compress = "bzip2", precheck = safe)
		} else { message("Canceling operation ...") }
	} else { save(list = i , envir = env, file = tmp.file, compress = "bzip2") }
}
#
do.copy_obj <- function(..., from.env = .GlobalEnv, to.env, keep.orig = TRUE, chatty = FALSE, .debug = FALSE) {
#' Replace, [C]opy, or [M]ove objects
#'
#' @description
#' \code{do.copy_obj} Facilitates the renaming, copying, and moving of objects within and across environments.
#' If \code{to.env} is \code{NULL}, the execution will simply replace the object under a new name.
#' If \code{to.env} has multiple values, the copy or move operations will populate each environment.
#'
#' @param ... (\code{\link[rlang]{dots_list}}) String(s) giving the names of the object(s) to be moved: may include environment prefix (e.g., \code{FROM_ENV$from.name}).  Elements given as a key-value pair will have the names of keys become the destination object names; otherwise, the value is (parsed and ) used as the destination name.  For example, \code{... = list(a = this, that, TO_ENV$the_other = other)} results in three destination objects named \code{a}, \code{that}, and \code{the_other} with \code{the_other} created in environment \code{TO_ENV}.
#' @param from.env (string| \code{.GlobalEnv}): The default source environment of the object(s) to be moved/copied
#' @param to.env (string| \code{.GlobalEnv}): The default target environment of the target object
#' @param keep.orig (logical | \code{TRUE}): When \code{FALSE}, the original is removed via \code{\link[base]{rm}}
#' @param chatty (logical | \code{FALSE}): Verbosity flag
#'
#' @export

	# :: Helper function to create the environment and object strings
	sub_fn = function(i, dflt){
		if (rlang::has_length(i, 1)){
			expand.grid(dflt, i, stringsAsFactors = FALSE)
		} else {
			list(paste(i[1:(length(i) - 1L)], collapse = "$"), i[length(i)])
		}
	}

	from.env = as.character(rlang::enexpr(from.env));

	to.env = if (missing(to.env)){
		from.env
	} else {
		.out <- rlang::enexpr(to.env) %>% as.character();
		if (length(.out) > 1){ .out[-1] } else { .out }
	}

	.args = rlang::list2(!!!purrr::map(rlang::exprs(...), as.character)) %>%
		purrr::map(~{
			.str = if(class(.x) %in% c("symbol", "name")){ as.character(.x) } else { .x }
			.str %>% stringi::stri_split_regex(pattern = c('[,;| ]'), simplify = TRUE) %>% trimws() %>% c()
		}) %>%
		unlist();

	# :: Source object strings
	.source = suppressWarnings({
		if (length(.args) == 1){
			stringi::stri_split_fixed(.args, "[$]", simplify = TRUE, omit_empty = FALSE) %>% sub_fn(from.env)
		} else { purrr::map_dfr(.args, ~{
			stringi::stri_split_fixed(.x, "[$]", simplify = TRUE, omit_empty = FALSE) %>% sub_fn(from.env) })
		}
	})

	# :: Target object strings
	.target = suppressWarnings({
		if (length(.args) == 1){
			stringi::stri_split_fixed(names(.args), "$", simplify = TRUE, omit_empty = FALSE) %>% sub_fn(to.env)
		} else {
			purrr::map(names(.args), ~{
				stringi::stri_split_fixed(.x, "$", simplify = TRUE, omit_empty = FALSE) %>% sub_fn(to.env)
			}) %>% purrr::reduce(rbind)
		}
	})

	# :: The plan of action along with the inputs
	.xfer_map = data.table::as.data.table(cbind(.source, .target));
	data.table::setnames(.xfer_map, c("FR_" %s+% c("ENV", "OBJ"), "TO_" %s+% c("ENV", "OBJ")));

	.xfer_map[
		, TO_OBJ := ifelse(TO_OBJ == "", FR_OBJ, TO_OBJ)
		][
		, `:=`(
			ACTION = purrr::pmap_chr(.SD, function(...){
				sprintf("%s$%s <- %s$%s", ...elt(3), ...elt(4), ...elt(1), ...elt(2))
			})
			, VALID = purrr::pmap(.SD, function(...){
					c(env_check = (...elt(2) %in% { parse(text = ...elt(1)) %>% eval(envir = globalenv()) %>% ls() })
						, obj_check = !is.null(parse(text = ...elt(3)) %>% eval(envir = globalenv()))) %>% t()
				})
			)
		];

	if (.debug){
		cat("Arguments\n");
		print(unlist(.args));
		cat("Unnamed targets\n");
		print(.args[which(names(.args) == "")]);
		cat("Transfer map\n");
		print(.xfer_map);
	}

	# :: ACTION!
	.xfer_map[!sapply(VALID, all), ACTION] %>% purrr::walk(~message("Skipping invalid operation: " %s+% .x));
	.xfer_map[sapply(VALID, all), ACTION] %>% purrr::walk(~{
		if (chatty){ message("Executing " %s+% .x, appendLF = FALSE) }
		parse(text = .x) %>% eval(envir = globalenv());
	});

	# :: If a 'move' action, remove the source objects from the corresponding environments
	if (!keep.orig){ .xfer_map[
			sapply(VALID, all)
			, rm(list = unique(FR_OBJ), envir = eval(parse(text = FR_ENV), envir = globalenv()))
			, by = FR_ENV
			]}

	return(invisible(0));
}
