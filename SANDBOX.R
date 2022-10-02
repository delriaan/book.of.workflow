# ~ do.make_cluster() ====
library(book.of.workflow)
do.load_unloaded("magrittr, purrr, stringi data.table callr future doFuture");

test_fun <- function(
	master.host = sprintf("%s", Sys.getenv("COMPUTERNAME"))
	, worker.hosts = "ITDHPC02-D:2 ITDHPC03-D:2"
	, cluster.port = parallelly::freePort(3e4:4.4e4)
	, win_cmd = "\\\\%s %s --default-packages=datasets,utils,grDevices,graphics,stats -e \".libPaths(paste0(R.home(), '/library')); tryCatch(parallel:::.workRSOCK,error=function(e)parallel:::.slaveRSOCK)()\" MASTER=\\\\%s PORT=%s OUT=\"/dev/null\" SETUPTIMEOUT=3 TIMEOUT=2592000 XDR=TRUE SETUPSTRATEGY=sequential "
	, ...){
	gc(full = TRUE);

	worker.hosts %<>% {
		wh = stri_split_regex(., "[,; ]", omit_empty = TRUE) %>% unlist()
		hnms = c(
			wh[!wh %like% ":"]
			, wh[wh %like% ":"] %>% stri_split_fixed(":", omit_empty = TRUE) %>%
				sapply(function(i){ t(rep(i[1], as.integer(i[2]))) }) %>%
				unlist() %>% as.vector()
			)
		# Limit the number of participating hosts to 20 (max user connections, I think)
		if (length(hnms) > 20){ hnms[1:20] } else { hnms }
	}

	sys.actions <- sprintf(win_cmd, worker.hosts, "\"C:/PROGRA~1/R/R-41~1.2/bin/x64/Rscript.exe\"", master.host, cluster.port)

	schedule.workers <- new.env();
	purrr::iwalk(sys.actions, ~{
		.args = list(i = .x, j = .y*1.5)

		schedule.workers[[paste0("worker_", .y)]] %<-% {
			Sys.sleep(.args$j)
			message("starting worker ...");
			system2(command = 'psexec', args = .args$i, wait = !TRUE, minimized = TRUE, invisible = TRUE)
		} %lazy% TRUE %seed% sample(100000, 1) %globals% c(".args")
	})

	.args <- rlang::list2(..., worker.hosts = worker.hosts, master.host = master.host, cluster.port = cluster.port);

	list(
		workers = callr::r_bg(function(w){ print(w) %$% mget(ls()) }, args = list(w = schedule.workers))
		, main = callr::r_bg(function(..., worker.hosts, master.host, cluster.port){
			library(magrittr)
			library(purrr)
			library(future)
			list2env(list(...))
			parallelly::makeClusterPSOCK(
				workers 			= worker.hosts
				, master			= master.host
				, port				= cluster.port
				, manual			= TRUE
				, homogeneous	= TRUE
				, ...)
		}, args = .args)
	)
}
#
future.cluster <- test_fun()

# ~ do.load_unloaded() ====
library(magrittr)
library(stringi);
library(tictoc);
tic("[%s] Development cycle: work item ID 1532" %>% sprintf(Sys.time()));

test.str <- c(
	"purrr"
	, "purrr, data.table"
	, "purrr{-" %s+% paste("accumulate", "when", "flatten", "compact", "compose", "simplify", "transpose", "set_names", sep = "-") %s+% "}"
	, "purrr{-" %s+% paste("accumulate", "when", "flatten", "compact", "compose", "simplify", "transpose", "set_names", sep = "-") %s+% "} data.table"
	, "purrr{-" %s+% paste("accumulate", "when", "flatten", "compact", "compose", "simplify", "transpose", "set_names", sep = "-") %s+% "} " %s+%
		"lubridate{-" %s+% paste("hour", "isoweek", "mday", "minute", "month", "quarter", "second", "wday", "week", "yday", "year", "date", "intersect", "setdiff", "union", sep = "-") %s+% "} "
	, "purrr{-" %s+% paste("accumulate", "when", "flatten", "compact", "compose", "simplify", "transpose", "set_names", sep = "-") %s+% "} " %s+%
		"lubridate{-" %s+% paste("hour", "isoweek", "mday", "minute", "month", "quarter", "second", "wday", "week", "yday", "year", "date", "intersect", "setdiff", "union", sep = "-") %s+% "} " %s+%
		"dplyr{+select} " %s+%
		"book.of.workflow, book.of.features, RCurl, httr, stringi data.table, magrittr, odbc, DBI, igraph, stringi, text2vec, LDAvis, tokenizers, stopwords, textstem " %s+%
		"readxl, xlsx, tictoc, doFuture, doParallel, furrr, isofor, glmnetUtils htmlwidgets, visNetwork, plotly, htmltools, kableExtra, colorspace, jsonlite, xml2"
	)[6];


library.cfg <-
	stri_split_regex(test.str, "[, |;]", simplify = TRUE, omit_empty = TRUE) %>% purrr::reduce(c) %>%
	stri_replace_all_regex(., "([{].*.[}])?", replacement = "", simplify = TRUE) %>% purrr::reduce(c) %>%
	book.of.utilities::enlist() %>%
	purrr::map(~{
		this.str = stri_split_regex(.x, pattern = "[{}]", omit_empty = TRUE, simplify = TRUE) %>% purrr::reduce(c);
		purrr::map(
			.x = c("[+]", "[-]") %>% book.of.utilities::enlist("include.only", "exclude")
			, ~stri_extract_all_regex(this.str, .x %s+% "[a-zA-Z.0-9]+", simplify = TRUE) %>% na.omit %>%
				stri_replace_all_regex(pattern = "[+-]", replacement = "", vectorize_all = FALSE)
			)
	});

library.cfg[
	sapply( # <- existing functionality reproduced: don't try to load libraries already loaded and attached
		names(library.cfg)
		, function(i){ search() %>% data.table::like(i, ignore.case = TRUE) %>% any %>% not }
		)] %>%
	purrr::imap(~do.call(
		"library"
		, append(
				.x[c(length(.x$include) > 0, length(.x[[2]]) > 0)] # <- User syntax error can lead to both being true, so let the call to 'library()' error out
				, list(package = .y, attach.required = TRUE, quietly = !chatty, warn.conflicts = FALSE)
				)
		));

toc(log = TRUE)

library(book.of.workflow);
library(magrittr)
do.load_unloaded("odbc, book.of.features, RCurl, httr, stringi data.table, DBI, igraph, stringi, text2vec, LDAvis, tokenizers, stopwords, textstem readxl, xlsx, tictoc, doFuture, doParallel, furrr, isofor, glmnetUtils htmlwidgets, visNetwork, plotly, htmltools, kableExtra, colorspace, jsonlite, xml2 purrr{-accumulate-when-flatten-compact-compose-simplify-transpose-set_names} lubridate{-hour-isoweek-mday-minute-month-quarter-second-wday-week-yday-year-date-intersect-setdiff-union} dplyr{+select}")



# ~ do.get.data() ====
library(stringi)
library(magrittr)
library(purrr)
library(DBI)
ENV <- new.env();

do.get_data(dbConnect(odbc::odbc(), "EDW"), "", "object", this.data = mtcars, promise = TRUE)


# ~ do.copy_obj() ====
library(stringi)
stri_split_fixed("a$b", "$", simplify = TRUE);

stri_split_fixed(c("b", "C$d", "f"), "$", simplify = TRUE) %>% as.data.table %>%
	pmap_dfr(function(...){
		i = c(...);
		if (..2 == ""){ c(V1 = "ENV", V2 =  ..1) } else { i }
	}) %>% setnames(c("env", "obj")) %>% print

sprintf("ENV_%s", 1:4) %>% walk(assign, value = new.env(), envir = globalenv())

(function(..., obj.nms = NULL, from.env = .GlobalEnv, to.env = .GlobalEnv){
#' Replace, [C]opy, or [M]ove objects
#'
#' \code{do.copy_obj} Facilitates the renaming, copying, and moving of objects within and across environments.
#' If \code{to.env} is \code{NULL}, the execution will simply replace the object under a new name.
#  If \code{to.env} has multiple values, the copy or move operations will populate each environment.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> String(s) giving the names of the object(s) to be moved: may include environment prefix (e.g., FROM_ENV$from.name).  Elements given as a key-value pair will have the names of keys become the destination object names; otherwise, the value is (parsed and ) used as the destination name.  For example, \code{... = "a = this, that, `TO_ENV$the_other` = other"} results in three destination objects named \code{a}, \code{that}, and \code{the_other} with \code{the_other} created in environment \code{TO_ENV}.
#' @param obj.nms (string[]|".GlobalEnv"): A string vector of object names to be moved/copied, following the key-value convention of \code{...}
#' @param from.env (string|".GlobalEnv"): The default source environment of the object(s) to be moved/copied
#' @param to.env (string|".GlobalEnv"): The default target environment of the target object
#'
#' @export
		sub_fn = function(i, dflt){
			if (length(i) == 2L & i[2L] == ""){ list(dflt, i[1L]) } else {
					list(paste(i[1:(length(i) - 1L)], collapse = "$"), i[length(i)])
					}
			}
		from.env = substitute(from.env) %>% as.character();
		to.env = substitute(to.env) %>% as.character();
		.args = c(rlang::list2(...), obj.nms);

		# :: Source object strings
		.source = { stringi::stri_split_regex(.args, "[$]", simplify = TRUE, ) %>% print() %>%
				apply(1, sub_fn, from.env) %>% purrr::reduce(rbind)}

		# :: Target object strings
		.target = { stringi::stri_split_regex(names(.args), "[$]", simplify = TRUE, omit_empty = TRUE) %>%
				apply(1, sub_fn, to.env) %>% purrr::reduce(rbind)}

		# :: The plan of action along with the inputs
		.xfer_map = { as.data.table(cbind(.source, .target))[
			, setnames(.SD, c("FR_" %s+% c("ENV", "OBJ"), "TO_" %s+% c("ENV", "OBJ")))
			][
			, TO_OBJ := ifelse(TO_OBJ == "", FR_OBJ, TO_OBJ)
			][
			, `:=`(
					ACTION = pmap_chr(.SD, function(...){ sprintf("%s$%s <- %s$%s", ...elt(3), ...elt(4), ...elt(1), ...elt(2)) })
					, VALID = pmap(.SD, function(...){
							t(c(
								env_check = (...elt(2) %in% { parse(text = ...elt(1)) %>% eval(envir = globalenv()) %>% ls() })
								, obj_check = !is.null(parse(text = ...elt(3)) %>% eval(envir = globalenv()))
								))
					})
			)]}

		cat("Arguments\n"); print(.args %>% unlist());
		cat("Unnamed targets\n"); print(.args[which(names(.args) == "")])
		cat("Transfer map\n"); print(.xfer_map);

		.xfer_map[!sapply(VALID, all), ACTION] %>% walk(~message("Skipping invalid operation: " %s+% .x));
		.xfer_map[sapply(VALID, all), ACTION] %>% walk(~{
				if (chatty){ message("Executing " %s+% .x, appendLF = FALSE) };
				parse(text = .x) %>% eval(envir = globalenv());
				if (chatty){ if (!is.null(parse(text = sprintf()) %>% eval(envir = globalenv()))){ message(": ... success!", appendLF = TRUE) }}
			});

		# :: If a move method, remove the source objects from the corresponding environments
		if (method %ilike% "^m"){ .xfer_map[sapply(VALID, all), rm(list = FR_OBJ, envir = eval(as.symbol(FR_ENV), envir = globalenv())), by = FR_ENV] }

		return(invisible(0));
	})(
	`ENV_1$sub_this` = "this"
	, !! letters[25] := "ENV_3$that"
	, "bleh"
	, "ENV_4$blah"
	, .arg_list = { rlang::list2(
				!! sprintf("long_name_%s", Sys.Date()) := 50
				, !!! ls("package:magrittr")[19:21] %>% purrr::set_names("ENV_2$" %s+% .)
				)} %>% unlist()
	)


# -----
workflow_manager <- { R6::R6Class(
	classname = "workflow_manager"
	, public	= { list(
			queue = NULL
			, #' Initialize a Workflow Manager
			 initialize = function(){ self$queue <- new.env() }
			, #' Execute a Stored Workflow Step
				#'
				#' \code{$execute()} executes quoted expressions referencing \code{read.snippet} calls.
				#'
				#' @param wf (list) The workflow queue object containing the quoted workflow steps
				#' @param wf_step (string/symbol) The names of the steps to execute.  Tip: label the steps in the order they should execute.
				#' @param list.only (logical | FALSE) When \code{TRUE}, available workflows and workflow steps are printed to console before the function exits.
				#'
				#' @return When \code{list.only == TRUE}, the listing of workflows and corresponding steps invisibly; otherwise, nothing.
			 execute = function(wf, wf_step, list.only = FALSE, actions = rep.int("exec", length(wf_step))){
				if (list.only){
					.out = self$queue %$% mget(ls()) %>%
						imap(~{
							.prefix = .y %s+% ": \n- ";
							cat(.prefix);
							.x %$% { cat(paste(ls(), collapse = "\n- "), "\n\n") }
						});

					return(invisible(.out));
				}

				if (missing(wf)){
					wf <- tcltk::tk_select.list(ls(self$queue), title = "Choose a workflow to search:");
					if (rlang::is_empty(wf)){ message("No workflow selected: exiting ...");	return(invisible(0)) }
				}

				wf_step <- if (missing(wf_step)){
					wf %$% mget(tcltk::tk_select.list(ls(), title = "Choose the workflow steps to execute in order", multiple = TRUE))
					} else { rlang::expr(!!wf_step) %>% as.character() }

				walk(wf_step, eval, envir = globalenv());
			}
			, #' Define a Workflow
				#' \code{$define()} is a utility to help define the ordered structure for predefined \code{\link{read.snippet}} calls.
				#' @param ...
				#' @param name (string) The name for the workflow
				#' @param show (logical |FALSE) \code{TRUE} prints a list of the steps in the created workflow if successfu;
				#' @return
			 define = function(..., name = readline("Enter a name for the workflow: "), show = FALSE){
			 	name = make.names(name);
			 	.dup_name = name %in% ls(self$queue);
			 	if (name == ""){ message("No name provided: exiting ..."); return() }

			 	while(.dup_name){
			 		name <<- readline(sprintf("%s already exists -- enter a name for the workflow: "))
				 	if (name == ""){ message("No name provided: exiting ..."); return() }
			 	}

			 	.args = rlang::exprs(...) %>% map(~{ .x[[1]] <- rlang::sym("read.snippet"); .x })

			 	self$queue[[name]] <- rlang::set_names(.args, paste("step", stringi::stri_pad_left(seq_along(.args), width = 2, pad = "0"), sep = "_"))
			 	invisible(self)
			 }
		)}
	, private = { list()}
	, active	= { list()}
	)}

(function(..., name = NULL){
	.args = rlang::exprs(...) |> map(~{ .x[[1]] <- rlang::sym("read.snippet"); .x })
	rlang::set_names(
		.args
		, paste("step", stringi::stri_pad_left(seq_along(.args), width = 2, pad = "0"), sep = "_")
		)
})(list(a, b, action = exec), list(k, j, doc = "this_doc.R", action = "skip"))

