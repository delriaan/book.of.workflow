# dir("pkg/R", full.names = TRUE) |> purrr::walk(source)
library(purrr);
library(stringi);
library(parallelly);
library(future);
library(foreach);
library(data.table);
#
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


# ~ copy_obj() ====
# debug(copy_obj)

BLAH <- new.env()
BLEH <- new.env()

set_names(letters[1:10], LETTERS[1:10]) |> as.list() |> list2env(envir = globalenv())
set_names(letters[11:20], LETTERS[11:20]) |> as.list() |> list2env(envir = BLEH)
.pattern <- "^[A-Z]$"

test_fn <- function(..., from_env = .GlobalEnv, to_env = .GlobalEnv, keep.orig = TRUE, .debug = FALSE){
	queue <- rlang::enquos(..., .named = FALSE);
	nms <- ...names();

	from <- purrr::map(queue, ~{
		obj <- rlang::quo_get_expr(.x)
		.has_dollar <- grepl("[$]", rlang::as_label(obj));

		env <- if (.has_dollar){
						rlang::parse_expr(
							magrittr::extract(
								stringi::stri_split_fixed(rlang::as_label(obj), "$", n = 2, simplify = TRUE)
								, 1
								)) |>
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

				rlang::parse_expr(if (.has_dollar | identical(.x, rlang::as_label(rlang::quo_get_expr(.y)) )){
						.x
					} else {
						glue::glue("{rlang::as_label(rlang::enexpr(to_env))}${rlang::quo_get_expr(.y)}")
					})
			})

	action <- purrr::map2(to, from, ~{
							list(`<-`, .x, rlang::eval_tidy(.y)) |>
								as.call() |>
								data.table::setattr("from", .y)
						});
	if (.debug){
		action
	} else {
		purrr::walk(action, ~{
			eval(.x)
			if (!keep.orig){
				obj <- attr(.x, "from") |> rlang::quo_get_expr()  |> rlang::as_label()
				env <- attr(.x, "from") |> rlang::quo_get_env()
				rm(list = obj, envir = env)
			}
		})
	}
}

# debug(test_fn)

inspect <- test_fn(A, `BLAH$new_G` = G, BLEH$K, to_env = BLAH, keep.orig = !FALSE)
inspect <- test_fn(A, `BLAH$new_G` = G, BLEH$K, to_env = BLAH, keep.orig = FALSE)
inspect[[3]]
