# ::::: ENVIRONMENT INTEGRITY
check_env_arg <- function(env){
	if (is.character(env)){
		if (env %in% search()){ as.environment(env) } else { rlang::parse_expr(env) |> eval() }
	} else { eval(env) }
}

# Environment must have ...
`%must.have%` <- function(env, x = ""){
#' Must Have
#'
#' \code{\%must.have\%} sets an attribute in the environment given by \code{env} with the name(s) of the object(s) that the environment must have.  Verification is done via \code{\link{\%check\%}}.
#'
#' @param env (object) An environment or name of an environment
#' @param x (string[]) A string vector of the names that \code{env} must have when checked
#'
#' @return The names of the objects that \code{env} must have
#' @family Environmental Integrity
#' @export

	env <- check_env_arg(env);
	x <- rlang::ensyms(x) |> purrr::compact();

	if (x == ""){
		if (rlang::is_empty(attr(env, "must.have"))){ return() } else { return(attr(env, "must.have")) }
	} else {
		attr(env, "must.have") <- purrr::map_chr(x, as.character)
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
#' @return If all objects that \code{env} \code{\link{\%must.have\%}} are present, nothing is returned; otherwise, an error message is returned.
#' @family Environmental Integrity
#' @export

	envs <- purrr::map(rlang::enquos(..., .named = TRUE), check_env_arg);

	purrr::iwalk(envs, ~{
		message(glue::glue("Checking `{.y}`"))
		stopifnot(rlang::env_has(.x, attr(.x, "must.have")));
	})
}

# Add to environment
`%+=%`<- function(env, x){
#' Assignment Shorthand
#'
#' \code{\%+=\%} wraps \code{base::list2env()}
#'
#' @param env (environment, string) An environment or name of an environment
#' @param x	(list) A named list with the names serving as the object name to add to \code{env} and the values the object definitions/contents for each object.
#'
#' @return The target environment, invisibly
#'
#' @family Environmental Integrity
#' @export

	env <- check_env_arg(env)

	list2env(x, envir = env);

	invisible(env);
}

# Remote from environment
`%-=%` <- function(env, x){
#' Remove Objects from an Environment
#'
#' \code{\%-=\%} wraps for \code{base::rm()}
#'
#' @param env (environment, string) An environment or name of an environment
#' @param x	(string[]) A collection of strings representing the names of the objects to remove from \code{env}
#'
#' @return The target environment, invisibly
#'
#' @family Environmental Integrity
#' @export

	env <- check_env_arg(env)

	rm(list = x, envir = env);

	invisible(env);
}
