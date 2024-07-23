# ::::: ENVIRONMENT INTEGRITY
check_env_arg <- function(env, env_nm = rlang::as_label(rlang::enexpr(env)), chatty = FALSE){
	if (chatty){
		msg_fun <- if (require(cli)){ cli::cli_alert_info } else { message }

		if (require(glue)){
			msg_fun(glue::glue("Checking `{env_nm}`"))
		} else {
			msg_fun(sprintf("Checking `%s`", env_nm))
		}
	}

	if (is.character(env)){
		if (env %in% search()){ as.environment(env) } else { rlang::parse_expr(env) |> eval() }
	} else if (rlang::is_quosure(env)){
		rlang::eval_tidy(env)
	} else {
		eval(env)
	}
}

# Check if environment has what is should
check.env <- function(...){
#' Environment Integrity Check
#'
#' \code{check.env} checks one or more environments for required objects
#'
#' @param ... One or more environments to check
#'
#' @section Changelog:
#' \describe{
#' \item{Version 0.1.2.1000}{
#'	\itemize{
#'		\item{Changed the output to a conditionally-attributed logical scalar}
#'		}
#'	}
#' }
#'
#' @return A logical scalar indicating success or failure. In the case of failure (\code{FALSE}), the missing objects are attached in an attribute named \code{"missing"}.
#' @family Chapter 2 - Environment Integrity
#' @export

	envs <- rlang::enquos(...) %>%
		rlang::set_names(purrr::map_chr(., \(x) rlang::as_label(rlang::quo_get_expr(x)))) |>
		purrr::imap(check_env_arg);

	purrr::iwalk(envs, \(x, y){
		.names <- attr(x, "must.have");

		if (rlang::is_empty(.names)){
			cat("No required values set: exiting ..;", sep = "\n");
			return(invisible(NA));
		}
		.test <- rlang::env_has(x, .names);
		.pass <- "PASS"
		.fail <- paste0("FAIL (missing ", paste(names(.test[!.test]) |> trimws(), collapse = ", "), ")");

		if (!all(.test)){
			cat(.fail, sep = "\n");
			res <- magrittr::set_attr(FALSE, "missing", .names[.test]);
			invisible(res);
		} else {
			invisible(TRUE);
		}
	})
}

# Environment must have ...
`%must.have%` <- function(env, x = ""){
#' Must Have
#'
#' \code{\%must.have\%} sets an attribute in the environment given by \code{env} with the name(s) of the object(s) that the environment must have.  Verification is done via \code{\link{check.env}}.
#'
#' @param env (object) An environment or name of an environment
#' @param x (string[]) A vector or strings containing the object names that \code{env} must have when checked.  Use the \code{\link[rlang]{!!}} operator when passing a vector or list.  Each string beginning with "-" or "!" will remove object names the target environment must have.
#'
#' @return The names of the objects that \code{env} must have
#' @family Chapter 2 - Environment Integrity
#' @export

	env <- check_env_arg(env, env_nm = rlang::as_label(rlang::enexpr(env)));
	x <- rlang::enexprs(x) |> purrr::compact();

	if (x == ""){
		if (rlang::is_empty(attr(env, "must.have"))){
			return()
		} else {
			return(attr(env, "must.have"))
		}
	} else {
		if (rlang::is_empty(attr(env, "must.have"))){
			attr(env, "must.have") <- purrr::map(x, as.character) |> unlist()
		} else {
			.names <- purrr::map(x, as.character) |>
				unlist() |>
				c(attr(env, "must.have")) |>
				unique();

			.not_have <- if (any(grepl("^[-!]", .names))){
					grep("^[-!].+", .names, value = TRUE) |>
						stringi::stri_replace_first_regex("[-!]", "")
				}

			attr(env, "must.have") <- .names[!(grepl("^[-!]", .names) | (.names %in% .not_have))]
		}
	}
}

# Add to environment
`%+=%`<- function(env, x = NULL){
#' Assignment Shorthand
#'
#' \code{\%+=\%} wraps \code{base::list2env()}
#'
#' @param env (environment, string) An environment or name of an environment
#' @param x	(list) A named list with the names serving as the object name to add to \code{env} and the values the object definitions/contents for each object.
#'
#' @return The target environment, invisibly
#' @family Chapter 2 - Environment Integrity
#' @export

	env <- suppressMessages(check_env_arg(env, ""))
	x <- as.list(x);
	suppressWarnings(if (!rlang::is_empty(x)){ list2env(x, envir = env) })

	invisible(env);
}

# Remove from environment
`%-=%` <- function(env, x = ""){
#' Remove Objects from an Environment
#'
#' \code{\%-=\%} wraps for \code{base::rm()}
#'
#' @param env (environment, string) An environment or name of an environment
#' @param x	(string[]) A collection of strings representing the names of the objects to remove from \code{env}.
#'
#' @return The target environment, invisibly
#' @family Chapter 2 - Environment Integrity
#' @export

	env <- suppressMessages(check_env_arg(env, ""))

	suppressWarnings(if (x != ""){ rm(list = x, envir = env); })

	invisible(env);
}
