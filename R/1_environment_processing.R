# ::::: ENVIRONMENT PROCESSING
do.get_pkgs <- function(pkg.list, is.custom = FALSE, custom.root = "") {
	#' Install and load packages
	#'
	#' \code{do.get_pkgs} Serves as a wrapper for \code{\link[base]{install.packages}}.
	#'
	#' @param pkg.list (string[]) A string vector of package names or a delimited string ("[,|; ]")
	#' @param is.custom (logical | \code{FALSE}) When \code{TRUE}, the value of \code{custom.root} is used under a "local source" context
	#'
	#' @return Returns the success or failure of each package (down)load
	#' @export

	# :: Check the possible library installation paths to see if they exist and have write-access
	permissions.matrix <- data.table(
		path = { c(
			paste(R.home(), "/library", sep = "")
			,{ Sys.setenv(
				R_LIBS_USER = paste(
					Sys.getenv("USERPROFILE")
					, "/Documents/R/win-library/"
					, R.version.string %>% stri_extract_all_regex("[3-6][.][0-9]") %>% unlist
					, sep = ""
				)
			);
				Sys.getenv("R_LIBS_USER")
			}
		)}
	)[, c("exists", "execute", "read", "write") := file.access(path, c(0, 1, 4, 2)) == 0][]

	install.lib = permissions.matrix[
		(exists & write)
		, if (length(path) > 1){ tcltk::tk_select.list(path, "Choose an Installation Library")} else { path }
	];

	pkg.list %<>% stri_split_regex("[,|; ]", omit_empty = TRUE) %>% as.vector;

	# :: Process the package names
	walk(pkg.list, ~{
		# Retrieve and install the package(s)
		ifelse(
			is.custom
			, install.packages(sprintf("%s/%s.zip", custom.root, .x), repos = NULL, type = "source", lib = install.lib)
			, install.packages(.x, lib = install.lib)
		);
	});
}
#
do.load_unloaded <- function(..., libs = NULL, pattern = "[,|; ]", chatty = FALSE) {
	#' Load Unloaded Packages
	#'
	#' \code{do.load_unloaded} checks the packages provided in \code{libs} against a call to \code{\link[base]{search}} and only makes a call to \code{\link[base]{library}} for unloaded (and attached) libraries.
	#'
	#' Library names can be declared in a single, delimited string (e.g., \code{"name0 name1, name2|name3"}) or as a vector of strings (e.g. \code{c("name0", "name1", "name2")}).
	#' Inclusions and exclusions can be declared using the following template:\cr  \code{"library_name{+name0+name1+...}"} for inclusions and \code{"library_name{-name0-name1-...}"}.  Since inclusions and exclusions cannot be used in the same call to \code{\link[base]{library}}(), trying to do so with this function will result in an error.
	#'
	#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A vector of packages to load given as characters.  Delimited strings are allowed: \emph{DO NOT use '+' or '-'}.
	#' @param libs (string[]) A vector of packages to load given as characters or symbols.  Delimited strings are allowed: \emph{DO NOT use '+' or '-'}.
	#' @param pattern  (string | "[, ]") A regex delimiter pattern that operates on `str`: \emph{DO NOT use '+' or '-'}.
	#' @param chatty (logical|FALSE) Sets the \code{quietly} argument in the call to \code{\link[base]{library}}()
	#'
	#' @return See `library()`
	#'
	#' @export

	# library.cfg: list(list{include.only[], exclude[]})
	libs = c(rlang::list2(...), substitute(libs) %>% as.character()) %>% unlist();
	if (chatty){ cat(libs, sep = "\n") }

	library.cfg = stri_split_regex(libs, "[, |;]", simplify = TRUE, omit_empty = TRUE) %>% unlist() %>%
		stri_replace_all_regex(., "([{].*.[}])?", replacement = "", simplify = TRUE) %>% unlist() %>%
		rlang::set_names() %>%
		map(~{
			this.str = stri_split_regex(.x, pattern = "[{}]", omit_empty = TRUE, simplify = TRUE) %>% unlist();
			map(
				.x = rlang::set_names(c("[+]", "[-]"), c("include.only", "exclude"))
				, .f = ~stri_extract_all_regex(this.str, .x %s+% "[a-zA-Z.0-9]+", simplify = TRUE) %>% na.omit %>%
					stri_replace_all_regex(pattern = "[+-]", replacement = "", vectorize_all = FALSE)
			)
		}) %T>% { if (chatty){ print(.) }};

	invisible(
		library.cfg[
			sapply( # don't try to load libraries already loaded and attached via indexing against `library.cfg`
				names(library.cfg)
				, function(i){ not(any(search() %ilike% i)) }
			)
		] %>% imap(~try({ do.call(
			"library"
			, append(
				.x[c(length(.x$include) > 0, length(.x[[2]]) > 0)] # <- User syntax error can lead to both being true, so let the call to 'library()' error out
				, list(package = .y, attach.required = TRUE, quietly = !chatty, warn.conflicts = FALSE)
			)
		)
		}))
	);
}
#
do.save_image <- function(..., safe = TRUE, env = ".GlobalEnv", save.dir = getwd(), file.name = "", use.prefix = TRUE, prepare = NULL){
	#' Manual Export of Workspace Objects
	#'
	#' The default value for \code{i} exports the entire workspace.  Unless `file` is \code{NULL}, when \code{i} is a vector of names or delimited string of names, the file name becomes 'multiObs'; otherwise, the file name is set to the value of \code{i}. When {i} contains 'all' or '*', regardless of the full content of \code{i}, the entire workspace is exported.
	#'
	#' @param ... (vector or list) Names of objects to save given as strings or symbols. Strings may be delimited (\code{c(',', ';', '|', ' ')})
	#' @param safe	(logical | TRUE) Should the pending action be confirmed at the prompt?
	#' @param env	(string | ".GlobalEnv") The environment to search for items given as a string
	#' @param save.dir (string | getwd()) The directory to save to (not the file name).  Use \code{TRUE} to interactively choose a save directory.
	#' @param file.name	(string | "") The name of the file to save, or, when \code{NULL}, the value of \code{i} if atomic or a predefined name when \code{i} is a vector
	#' @param use.prefix (logical | TRUE) When \code{TRUE} (the default), the file name is prefixed with the value of \code{env}
	#' @param prepare	(language | NULL) A quoted expression that executes before the save action takes place.
	#'
	#' @return A `.rdata` file, the filename of which being suffixed with a timestamp formatted as "yyyy.mm.dd.hhmmss"
	#'
	#' @export

	# :: Preliminary checks on supplied parameters
	env <- substitute(env) %>% as.character();
	obj.nms <- NULL

	if (!is.character(env)){ message("'env' must be a single string or symbol"); return(invisible(0)); }
	env_ref <- if (env %in% search()) { as.environment(env) } else { get(env, envir = globalenv()) }

	if (!is.null(prepare)) { eval(prepare, envir = env_ref) }
	if (...length() == 0){ obj.nms <- ls(envir = env_ref); }
	# if (missing(obj.nms)){ obj.nms <- ls(envir = env_ref); }

	i = rlang::list2(...) %>%
		as.character() %>%
		map(~stri_split_regex(.x, pattern = "[,;| ]", simplify = TRUE, omit_empty = TRUE)) %>%
		unlist() %>%
		map(~if (exists(.x, envir = env_ref)){ .x } else { eval(as.symbol(.x), envir = env_ref) }) %>%
		unlist();

	if (rlang::is_empty(i)){ i <- "all" }

	if (save.dir == TRUE){ save.dir <- tcltk::tk_choose.dir(default = getwd(), caption = "Choose the 'save' directory:") }

	filecheck = is.null(file.name) | (stri_length(trimws(file.name)) == 0)

	# :: Environment reference from string
	env_ref = if (env %in% search()){ as.environment(env) } else { eval(parse(text = env), envir = globalenv()) }

	namescheck = { c(
		all = { any(i %in% c("*", "all")) }
		, single.obj = { (length(i) == 1) & !any(i %in% c("*", "all")) & (stri_length(i) > 0)}
		, multi.as.vector = { (length(i) > 1) & !any(i %in% c("*", "all")) }
		, multi.as.string = { any((i %like% "[,;| ]") & !any(i %in% c("*", "all"))) }
	) %>% keep(~ .x) %>% names() %>% .[1]
	}

	# :: Set the file name based on the values supplied to argument `i`
	switch(
		namescheck
		, "all" 						= { file.name <- ifelse(filecheck, "all", file.name); i <- ls(env_ref, all.names = TRUE) }
		, "multi.as.string" = { file.name <- ifelse(filecheck, "multiObs", file.name); i <- stri_split_regex(i, "[,;| ]", omit_empty = TRUE, simplify = TRUE) %>% c(); }
		, "multi.as.vector" = { file.name <- ifelse(filecheck, "multiObs", file.name) }
		, 										{ file.name <- ifelse(filecheck, i, file.name) }
	)

	# :: Write the binary file to disk
	tstamp = format(Sys.time(), "%Y.%m.%d.%H%M%S");

	tmp.file = sprintf("%s/%s%s_%s.rdata", save.dir, if (use.prefix){ ifelse(env %like% "Global", "", paste0(env, "$")) } else { "" }, file.name, tstamp);

	if (safe) {
		msg = { sprintf("Preparing to save %s[%s] to %s: \nContinue? ", env, paste(i, collapse = ", "), tmp.file) }
		if (askYesNo(msg, prompt = "Y/N/C")) {
			save(list = i, envir = env_ref, file = tmp.file, compress = "bzip2", precheck = safe)
		} else { message("Canceling operation ...") }
	} else { save(list = i , envir = env_ref, file = tmp.file, compress = "bzip2") }
}
#
do.copy_obj <- function(..., from.env = .GlobalEnv, to.env, keep.orig = TRUE, chatty = FALSE, .debug = FALSE) {
	#' Replace, [C]opy, or [M]ove objects
	#'
	#' \code{do.copy_obj} Facilitates the renaming, copying, and moving of objects within and across environments.
	#' If \code{to.env} is \code{NULL}, the execution will simply replace the object under a new name.
	#  If \code{to.env} has multiple values, the copy or move operations will populate each environment.
	#'
	#' @param ... (\code{\link[rlang]{dots_list}}) String(s) giving the names of the object(s) to be moved: may include environment prefix (e.g., FROM_ENV$from.name).  Elements given as a key-value pair will have the names of keys become the destination object names; otherwise, the value is (parsed and ) used as the destination name.  For example, \code{... = "a = this, that, `TO_ENV$the_other` = other"} results in three destination objects named \code{a}, \code{that}, and \code{the_other} with \code{the_other} created in environment \code{TO_ENV}.
	#' @param from.env (string|".GlobalEnv"): The default source environment of the object(s) to be moved/copied
	#' @param to.env (string|".GlobalEnv"): The default target environment of the target object
	#' @param keep.orig (logical | TRUE): When \code{FALSE}, the original is removed via \code{\link[base]{rm}}
	#' @param chatty (logical | FALSE): Verbosity flag
	#'
	#' @examples
	#' library(book.of.workflow)
	#' BLAH <- new.env(); BLEH <- new.env()
	#' set_names(letters[1:10], LETTERS[1:10]) %>% as.list %>% list2env(envir = globalenv())
	#' set_names(letters[11:20], LETTERS[11:20]) %>% as.list %>% list2env(envir = BLEH)
	#' do.copy_obj(A, keep.orig = TRUE, .debug = TRUE)
	#' ls()
	#'
	#' do.copy_obj(B, C, D, to.env = BLAH, keep.orig = TRUE, .debug = !TRUE)
	#' ls(BLAH)
	#'
	#' do.copy_obj(E, `F`, G, to.env = BLEH, keep.orig = TRUE, .debug = !TRUE)
	#' ls(BLAH); ls(BLEH)
	#'
	#' do.copy_obj(A, B, C, D, E, `F`, G, to.env = c(BLEH, BLAH), keep.orig = TRUE, .debug = !TRUE)
	#' ls(BLAH); ls(BLEH)
	#'
	#' do.copy_obj(!!ls(pattern = "^[A-Z]"), to.env = c(BLEH, BLAH), keep.orig = TRUE, .debug = !TRUE)
	#' ls(BLAH); ls(BLEH)
	#'
	#' @export

	# :: Helper function to create the environment and object strings
	sub_fn = function(i, dflt){
		if (rlang::has_length(i, 1)){ expand.grid(dflt, i, stringsAsFactors = FALSE)
		} else { list(
			paste(i[1:(length(i) - 1L)], collapse = "$")
			, i[length(i)]
		)
		}
	}

	from.env = substitute(from.env) %>% as.character();

	to.env = if (missing(to.env)){ from.env } else { .out = substitute(to.env) %>% as.character(); if (length(.out) > 1){ .out[-1] } else { .out }}

	.args = rlang::list2(!!!map(rlang::exprs(...), as.character)) %>%
		map(~{
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
		} else { purrr::map(names(.args), ~{
			stringi::stri_split_fixed(.x, "$", simplify = TRUE, omit_empty = FALSE) %>% sub_fn(to.env) }) %>%
				purrr::reduce(rbind)
		}
	})

	# :: The plan of action along with the inputs
	.xfer_map = { as.data.table(cbind(.source, .target))[
		, setnames(.SD, c("FR_" %s+% c("ENV", "OBJ"), "TO_" %s+% c("ENV", "OBJ")))
	][
		, TO_OBJ := ifelse(TO_OBJ == "", FR_OBJ, TO_OBJ)
	][
		, `:=`(
			ACTION = pmap_chr(.SD, function(...){ sprintf("%s$%s <- %s$%s", ...elt(3), ...elt(4), ...elt(1), ...elt(2)) })
			, VALID = pmap(.SD, function(...){
				t(c(env_check = (...elt(2) %in% { parse(text = ...elt(1)) %>% eval(envir = globalenv()) %>% ls() })
						, obj_check = !is.null(parse(text = ...elt(3)) %>% eval(envir = globalenv()))))
			})
		)
	]
	}

	if (.debug){
		cat("Arguments\n"); print(.args %>% unlist());
		cat("Unnamed targets\n"); print(.args[which(names(.args) == "")])
		cat("Transfer map\n"); print(.xfer_map);
	}

	# :: ACTION!
	.xfer_map[!sapply(VALID, all), ACTION] %>% walk(~message("Skipping invalid operation: " %s+% .x));
	.xfer_map[sapply(VALID, all), ACTION] %>% walk(~{
		if (chatty){ message("Executing " %s+% .x, appendLF = FALSE) };
		parse(text = .x) %>% eval(envir = globalenv());
		if (chatty){ if (!is.null(parse(text = sprintf()) %>% eval(envir = globalenv()))){ message(": ... success!", appendLF = TRUE) }}
	});

	# :: If a 'move' action, remove the source objects from the corresponding environments
	if (!keep.orig){ .xfer_map[sapply(VALID, all), rm(list = unique(FR_OBJ), envir = eval(parse(text = FR_ENV))), by = FR_ENV] }

	return(invisible(0));
}

debug(do.copy_obj)
undebug(do.copy_obj)