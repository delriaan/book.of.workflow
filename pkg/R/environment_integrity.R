check_env_arg <- function(env, env_nm = rlang::as_label(rlang::enexpr(env)), chatty = FALSE){
	#' Resolve an Environment Argument
	#'
	#' `check_env_arg()` normalizes an environment reference given as an
	#' environment object, quoted/unquoted expression, quosure, or attached search
	#' path label.
	#'
	#' @param env An environment reference to resolve.
	#' @param env_nm A label used in informational messages.
	#' @param chatty Logical; when `TRUE`, emits a progress message.
	#'
	#' @return An environment object.
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

check.env <- function(...){
	#' Environment Integrity Check
	#'
	#' `check.env()` checks one or more environments for required objects.
	#'
	#' @param ... One or more environments to check.
	#'
	#' @section Changelog:
	#' \describe{
	#' \item{Version 0.1.2.1000}{
	#'   \itemize{
	#'     \item{Changed the output to a conditionally-attributed logical scalar.}
	#'   }
	#' }
	#' }
	#'
	#' @return A logical scalar indicating success or failure. In the case of
	#'   failure (`FALSE`), missing objects are attached in attribute
	#'   `"missing"`.
	#' @family Chapter 1 - Environment Integrity
	#' @export
	envs <- rlang::enquos(...)
	envs <- rlang::set_names(envs, purrr::map_chr(envs, \(x) rlang::as_label(rlang::quo_get_expr(x)))) |>
		purrr::imap(check_env_arg)

	purrr::iwalk(envs, \(x, y){
		.names <- attr(x, "must.have")

		if (rlang::is_empty(.names)){
			cat("No required values set: exiting ..;", sep = "\n")
			return(invisible(NA))
		}
		.test <- rlang::env_has(x, .names)
		.pass <- "PASS"
		.fail <- paste0("FAIL (missing ", paste(names(.test[!.test]) |> trimws(), collapse = ", "), ")")

		if (!all(.test)){
			cat(.fail, sep = "\n")
			res <- magrittr::set_attr(FALSE, "missing", .names[.test])
			invisible(res)
		} else {
			invisible(TRUE)
		}
	})
}

`%must.have%` <- function(env, x = ""){
	#' Set Required Object Names for an Environment
	#'
	#' `%must.have%` sets an attribute in `env` listing object names that the
	#' environment must contain when checked by [check.env()].
	#'
	#' @param env An environment or name of an environment.
	#' @param x Character vector of required object names. Use `!!` when passing a
	#'   vector or list. Strings beginning with `"-"` or `"!"` remove required
	#'   names.
	#'
	#' @return Character vector of required object names.
	#' @family Chapter 1 - Environment Integrity
	#' @export
	env <- check_env_arg(env, env_nm = rlang::as_label(rlang::enexpr(env)))
	x <- rlang::enexprs(x) |> purrr::compact()

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
				unique()

			.not_have <- if (any(grepl("^[-!]", .names))){
					grep("^[-!].+", .names, value = TRUE) |>
						stringi::stri_replace_first_regex("[-!]", "")
				}

			attr(env, "must.have") <- .names[!(grepl("^[-!]", .names) | (.names %in% .not_have))]
		}
	}
}

`%+=%`<- function(env, x = NULL){
	#' Add Objects to an Environment
	#'
	#' `%+=%` is a shorthand wrapper around [base::list2env()].
	#'
	#' @param env An environment or name of an environment.
	#' @param x A named list where names are object names and values are object
	#'   definitions.
	#'
	#' @return The target environment, invisibly.
	#' @family Chapter 1 - Environment Integrity
	#' @export
	env <- suppressMessages(check_env_arg(env, ""))
	x <- as.list(x)
	suppressWarnings(if (!rlang::is_empty(x)){ list2env(x, envir = env) })

	invisible(env)
}

`%-=%` <- function(env, x = ""){
	#' Remove Objects from an Environment
	#'
	#' `%-=%` is a shorthand wrapper around [base::rm()].
	#'
	#' @param env An environment or name of an environment.
	#' @param x Character vector of object names to remove from `env`.
	#'
	#' @return The target environment, invisibly.
	#' @family Chapter 1 - Environment Integrity
	#' @export
	env <- suppressMessages(check_env_arg(env, ""))

	suppressWarnings(if (x != ""){ rm(list = x, envir = env); })

	invisible(env)
}
