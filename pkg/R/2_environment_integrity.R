# ::::: ENVIRONMENT INTEGRITY
check_env_arg <- function(env){
	if (is.character(env)){
		if (env %in% search()){ as.environment(env) |> data.table::setattr("attached", TRUE) } else { rlang::parse_expr(env) |> eval() }
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
#' @return If not all objects that \code{env} \code{\link{\%must.have\%}} are present, an error message is produced; a corresponding logical is invisibly returned
#' @family Chapter 2 - Environment Integrity
#' @export

	envs <- rlang::enquos(..., .named = TRUE) |> purrr::map(check_env_arg);

	purrr::iwalk(envs, ~{
		.test <- rlang::env_has(.x, attr(.x, "must.have"));
		.pass <- "PASS"
		.fail <- paste0("FAIL (missing ", paste(names(.test[!.test]) |> trimws(), collapse = ", "), ")");
		cat(glue::glue("Checking `{.y}`: { ifelse(all(.test), .pass, .fail) }"), sep = "\n");
	})
}

# Environment must have ...
`%must.have%` <- function(env, x = ""){
#' Must Have
#'
#' \code{\%must.have\%} sets an attribute in the environment given by \code{env} with the name(s) of the object(s) that the environment must have.  Verification is done via \code{\link{check.env}}.
#'
#' @param env (object) An environment or name of an environment
#' @param x (string[]) A vector or strings containing the object names that \code{env} must have when checked. Start a string with "-" or "!" to remove an existing entry
#'
#' @return The names of the objects that \code{env} must have
#' @family Chapter 2 - Environment Integrity
#' @export

	env <- check_env_arg(env);

	x <- purrr::compact(x);

	if (identical(x, "")){
		if (rlang::is_empty(attr(env, "must.have"))){
			return()
		} else {
			return(attr(env, "must.have"))
		}
	} else {
		if (rlang::is_empty(attr(env, "must.have"))){
				attr(env, "must.have") <- purrr::discard(\(i) grepl("^[-!]", i)) |> unique()
		} else {
			attr(env, "must.have") <- c(attr(env, "must.have"), x) |> unique() |> purrr::discard(\(i) grepl("^[-!]", i))
		}
	}
}

# Add to environment
`%+=%`<- function(env, x = ""){
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

	env <- check_env_arg(env)
	x <- as.list(x);

	suppressWarnings(if (x != ""){ list2env(x, envir = env) })

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

	env <- check_env_arg(env)

	suppressWarnings(if (x != ""){ rm(list = x, envir = env); })

	invisible(env);
}
