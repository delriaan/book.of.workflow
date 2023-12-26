# ::::: ENVIRONMENT PROCESSING
load_unloaded <- load.unloaded <- function(..., libs = NULL, delim = "[,|; ]", autoinstall = FALSE, chatty = FALSE) {
#' Load Unloaded Packages
#'
#' \code{load.unloaded} checks the packages provided in \code{libs} against a call to \code{\link[base]{search}} and only makes a call to \code{\link[base]{library}} for unloaded (and attached) libraries.
#'
#' Library names can be declared in a single, delimited string (e.g., \code{"name0 name1, name2|name3"}) or as a vector of strings (e.g. \code{c("name0", "name1", "name2")}).
#' Inclusions and exclusions can be declared using the following template:\cr  \code{"library_name{+name0+name1+...}"} for inclusions and \code{"library_name{-name0-name1-...}"}.  Since inclusions and exclusions cannot be used in the same call to \code{\link[base]{library}}(), trying to do so with this function will result in an error.
#'
#' @param ... \code{\link[rlang]{dots_list}} A vector of packages to load given as characters.  Delimited strings are allowed: \emph{DO NOT use \code{+} or \code{-}}.
#' @param libs (string[]) A vector of packages to load given as characters or symbols.  Delimited strings are allowed: \emph{DO NOT use \code{+} or \code{-}}.
#' @param delim  (string | "[, ]") A regular expression delimiter pattern that operates on `str`: \emph{DO NOT use glyphs \code{+} or \code{-}}.
#' @param autoinstall
#' \itemize{
#'	\item{\code{FALSE} (default): A notification message is provided if \code{chatty} is \code{TRUE}; otherwise, no message is sent}
#'	\item{\code{TRUE}: If a library isn't installed, it will be using the default library installation path}
#'	\item{\code{list()}: Assumes \code{TRUE} and should be a list of arguments (excluding \code{pkgs}) to send to \code{\link[utils]{install.packages}}}
#'	}
#' @param chatty (logical | \code{FALSE}) Sets the \code{quietly} argument in the call to \code{\link[base]{library}}()
#'
#' @return See \code{\link[base]{library}}
#' @family Chapter 1 - Environment Processing
#' @export

	libs <- c(as.character(rlang::enexprs(...)), libs);

	if (chatty){ cat(libs, sep = "\n") }

	.tmp_str <- stringi::stri_split_regex(str = libs, pattern = delim, simplify = TRUE, omit_empty = TRUE) |> unlist() |> as.vector();
	.tmp_str <- rlang::set_names(
								.tmp_str
								, stringi::stri_replace_all_regex(.tmp_str, pattern = "([{].*.[}])?", replacement = "", vectorize_all = TRUE) |> unlist()
								);
	library.cfg <- purrr::map(.tmp_str, ~{
			ns_objs <- stringi::stri_split_regex(.x, pattern = "[{}]", omit_empty = TRUE, simplify = TRUE) |> unlist() |> magrittr::extract(-1)
			if (rlang::is_empty(ns_objs)){
				rlang::set_names(list(NULL, NULL), c("include.only", "exclude"))
			} else {
				purrr::map(
					.x = rlang::set_names(c("[+]", "[-]"), c("include.only", "exclude"))
					, .f = ~{ stringi::stri_extract_all_regex(ns_objs, pattern = paste0(.x, "[a-zA-Z0-9._]+"), simplify = TRUE) |>
									stats::na.omit() |>
									stringi::stri_replace_all_regex(pattern = "[+-]", replacement = "", vectorize_all = FALSE)
					})
			}
		});

	if (chatty){ print(library.cfg) }

	new.libs = purrr::keep(names(library.cfg), ~!.x %in% rownames(utils::installed.packages()));

	if (!rlang::is_empty(new.libs)){
		if (is.list(autoinstall)){
			rlang::inject(utils::install.packages(pkgs = new.libs, !!!autoinstall))
		} else if (is.logical(autoinstall)==autoinstall){ utils::install.packages(pkgs = new.libs) }
	}

	invisible(
		library.cfg[
			# Don't try to load libraries already loaded and attached via indexing against `library.cfg`
			purrr::map_lgl(names(library.cfg), ~{ magrittr::not(any(grepl(.x, search()))) })
			] |> purrr::imap(~tryCatch(expr = { do.call(
				what = "library"
				, args = append(
						.x[c(length(.x$include) > 0, length(.x[[2]]) > 0)] # <- User syntax error can lead to both being true, so let the call to 'library()' error out
						, list(package = .y, attach.required = TRUE, quietly = !chatty, warn.conflicts = FALSE)
						))
				}, error = function(e){ message(e) }))
		)
}
#
save_image <- save.obj <- function(..., safe = TRUE, env = .GlobalEnv, save.dir = getwd(), file.name = "", use.prefix = TRUE, use.timestamp = TRUE, prepare = NULL){
#' Manual Export of Workspace Objects
#'
#' The default value for \code{i} exports the entire workspace.  Unless `file` is \code{NULL}, when \code{i} is a vector of names or delimited string of names, the file name becomes 'multiObs'; otherwise, the file name is set to the value of \code{i}. When {i} contains 'all' or '*', regardless of the full content of \code{i}, the entire workspace is exported.
#'
#' @param ... (\code{\link[rlang]{dots_list}}) Names of objects to save given as strings or symbols. Strings may be delimited (\code{c(',', ';', '|', ' ')})
#' @param safe	(logical | \code{TRUE}) Should the pending action be confirmed at the prompt?
#' @param env	The environment to search for items
#' @param save.dir (string | \code{getwd()}) The directory to save to (not the file name).  Use \code{TRUE} to interactively choose a save directory.
#' @param file.name	(string | "") The name of the file to save, or, when \code{NULL}, the value of \code{i} if atomic or a predefined name when \code{i} is a vector
#' @param use.prefix (logical | \code{TRUE}) When \code{TRUE} (the default), the file name is prefixed with the value of \code{env}
#' @param use.timestamp (logical | \code{TRUE}) When \code{TRUE} (the default), the file name is appended with a formatted value of \code{Sys.time()}
#' @param prepare	(language | \code{NULL}) A quoted expression that executes before the save action takes place.
#'
#' @return A `.rdata` file, the filename of which being suffixed with a timestamp formatted as "yyyy.mm.dd.hhmmss"
#' @family Chapter 1 - Environment Processing
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
		) |>
		purrr::keep(~.x) |> names() |> magrittr::extract(1)
	}

	# :: Set the file name based on the values supplied to argument `i`
	switch(
		namescheck
		, "all" 						= { file.name <- ifelse(filecheck, "all", file.name); i <- ls(envir = env, all.names = TRUE) }
		, "multi.as.string" = { file.name <- ifelse(filecheck, "multiObs", file.name);
														i <- stringi::stri_split_regex(i, "[,;| ]", omit_empty = TRUE, simplify = TRUE) |> c();
													}
		, "multi.as.vector" = { file.name <- ifelse(filecheck, "multiObs", file.name) }
		, 										{ file.name <- ifelse(filecheck, i, file.name) }
		);

	# :: Write the binary file to disk
	tstamp = ifelse(use.timestamp, paste0("_", format(Sys.time(), "%Y.%m.%d.%H%M%S")), "");

	tmp.file = sprintf("%s/%s%s%s.rdata", save.dir, if (use.prefix){ ifelse(env_ref %like% "Global", "", paste0(env_ref, "$")) } else { "" }, file.name, tstamp);

	if (safe) {
		msg = { sprintf("Preparing to save %s[%s] to %s: \nContinue? ", env_ref, paste(i, collapse = ", "), tmp.file) }
		if (utils::askYesNo(msg, prompt = "Y/N/C")) {
			save(list = i, envir = env, file = tmp.file, compress = "bzip2", precheck = safe)
		} else { message("Canceling operation ...") }
	} else { save(list = i , envir = env, file = tmp.file, compress = "bzip2") }
}
#
copy_obj <- copy.obj <- function(..., from_env = .GlobalEnv, to_env = .GlobalEnv, keep.orig = TRUE, chatty = FALSE){
#' Replace, Copy, or Move Objects
#'
#' @description
#' \code{copy.obj} Facilitates the renaming, copying, and moving of objects within and across environments.
#' If \code{to.env} is \code{NULL}, the execution will simply replace the object under a new name.
#' If \code{to.env} has multiple values, the copy or move operations will populate each environment.
#'
#' @param ... (\code{\link[rlang]{dots_list}}) String(s) giving the names of the object(s) to be moved: may include environment prefix (e.g., \code{FROM_ENV$from.name}).  Elements given as a key-value pair will have the names of keys become the destination object names; otherwise, the value is (parsed and ) used as the destination name.  For example, \code{... = list(a = this, that, TO_ENV$the_other = other)} results in three destination objects named \code{a}, \code{that}, and \code{the_other} with \code{the_other} created in environment \code{TO_ENV}.
#' @param from_env (string| \code{.GlobalEnv}): The default source environment of the object(s) to be moved/copied
#' @param to_env (string| \code{.GlobalEnv}): The default target environment of the target object
#' @param keep.orig (logical | \code{TRUE}): When \code{FALSE}, the original is removed via \code{\link[base]{rm}}
#' @param chatty (logical | \code{FALSE}) Verbosity flag
#'
#' @family Chapter 1 - Environment Processing
#' @export

	`%||%` <- rlang::`%||%`;
	nms <- which(...names() != ".debug") |> stats::na.omit()
	if (identical(integer(), nms)){ nms <- sequence(...length()) }

	queue <- rlang::enquos(..., .named = FALSE)[nms]
	.debug <- rlang::list2(...)$.debug %||% FALSE

	from <- purrr::map(queue, ~{
		obj <- rlang::quo_get_expr(.x)
		if (is.character(obj)){ obj <- rlang::sym(obj) }

		.has_dollar <- grepl("[$]", rlang::as_label(obj));

		env <- if (.has_dollar){
						rlang::parse_expr(magrittr::extract(stringi::stri_split_fixed(rlang::as_label(obj), "$", n = 2, simplify = TRUE), 1)) |>
						eval(envir = .GlobalEnv)
					} else { rlang::quo_get_env(.x) }

		if (!identical(from_env, env)){ env <- from_env }

		if (!is.symbol(obj)){
			.tmp_sym <- as.list(obj)[[3]]
			.tmp_env <- as.list(obj)[[2]] |> eval(envir = env)

			rlang::as_quosure(.tmp_sym, env = .tmp_env)
		} else { .x }
	});

	to <- purrr::map2(names(queue), from, ~{
				.has_dollar <- grepl("[$]", .x);

				from_quo <- .y

				if (.has_dollar | identical(.x, rlang::as_label(rlang::quo_get_expr(from_quo)) )){
					# Fully-qualified target object
						rlang::parse_expr(.x)
				} else {
					.tmp_to_env <- rlang::enexprs(to_env)[[1]]

					# Check for multi-assign use case
					if (!rlang::has_length(.tmp_to_env, 1)){
						.obj <- .x
						.tmp_to_env[-1] |>
							purrr::map(~{
								env <- .x;
								obj <- if (!identical(.obj, rlang::as_label(rlang::quo_get_expr(from_quo))) & (.obj != "")){
									rlang::parse_expr(.obj)
								} else { rlang::quo_get_expr(from_quo) }
								as.call(list(rlang::expr(`$`), env, obj))
							})
						} else {
							env <- .tmp_to_env;
							obj <- if (!identical(.x, rlang::as_label(rlang::quo_get_expr(from_quo))) & (.x != "")){	rlang::parse_expr(.x)} else {	rlang::quo_get_expr(from_quo)}
							as.call(list(rlang::expr(`$`), env, obj))
						}
				}
			})

	action <- purrr::map2(to, from, ~{
			if (!rlang::is_list(.x)){
				list(`<-`, .x, rlang::eval_tidy(.y)) |> as.call() |> data.table::setattr("from", .y)
			} else {
				y <- .y
				purrr::map(.x, ~list(`<-`, .x, rlang::eval_tidy(y)) |> as.call() |> data.table::setattr("from", y))
			}
		});

	if (.debug){
		return(data.table::setattr(action, "call", rlang::caller_call()))
	} else {
		purrr::walk(action, ~{
			.check_clean <- rlang::expr({if (!keep.orig){
						obj <- attr(.x, "from") |> rlang::quo_get_expr()  |> rlang::as_label()
						env <- attr(.x, "from") |> rlang::quo_get_env()
						rm(list = obj, envir = env)
					}})

			if (rlang::is_call(.x)){
				eval(.x);
				eval(.check_clean);
			} else {
				.action <- .x;

				purrr::iwalk(.action, ~{
					eval(.x);
					if (.y == length(.action)){ eval(.check_clean); }
				})
			}
		})
	}
}
#
refer.to <- function(x){
	#' Refer to an Environment
	#'
	#' \code{refer.to} is a convenience function for referring to an environment. It is a wrapper for \code{\link{as.environment}} that allows for the use of character strings and environments as arguments.
	#'
	#' @param x An unquoted name of an environment or attached space (i.e., found in the output of \code{search()})
	#'
	#' @return Invisibly, the environment if it exists or an error message
	#'
	#' @family Chapter 1 - Environment Processing
	#'
	#' @export
	x <- rlang::enexpr(x) |> rlang::as_label();

	invisible(if (x %in% search()){
			as.environment(x)
		} else {
			xenv <- find(x)
			if (!rlang::is_empty(xenv)){
				xenv <- as.environment(xenv);
				if (is.environment(xenv[[x]])){
					xenv[[x]]
				} else {
					stop(glue::glue("'{x}' is not an environment"))
				}
			} else {
				x.expr <- rlang::parse_expr(x)
				if (is.environment(eval(x.expr))){
					eval(x.expr)
				} else {
					stop(glue::glue("'{x}' is not an environment"))
				}
			}
	})
}
