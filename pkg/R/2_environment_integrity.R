# ::::: ENVIRONMENT INTEGRITY
check_env_arg <- function(env){
		if (is.character(env)){
			if (env %in% search()){ as.environment(env) } else { parse(text = env) |> eval() }
		} else { eval(env) }
}

`%missing%` <- function(env, x){
#' Missing Environmental Objects
#'
#' \code{\%missing\%} is a wrapper for \code{base::exists} taking the environment or environment name as the \code{LHS} argument and the vector of object names on the \code{RHS}.
#'
#' @param env (object) An environment or name of an environment
#' @param x (string[]) A string vector of the names of objects in query in \code{env}
#'
#' @return A logical vector, the length of \code{x}
#' @family Environmental Integrity
#' @export

	env <- check_env_arg(env)


	if (rlang::is_empty(attr(env, "must.have"))){
		message("Environment not required to have anything: exiting ...")
		return(invisible(FALSE))
	} else {
		x.found <- intersect(attr(env, "must.have"), rlang::enexprs(!!!x) |> as.character()) |> purrr::set_names()
		x.new <- setdiff(rlang::enexprs(!!!x) |> as.character(), attr(env, "must.have"))
		x.new <- purrr::set_names(sprintf("not_req_%s", x.new))
		c(x.found, x.new) %in% ls(env, all.names = TRUE)
	}
};

#
`%must.have%` <- function(env, x){
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

	env <- check_env_arg(env)

	mh = attr(env, "must.have");

	x = (if (missing(x) || is.null(x)) { mh } else { x }) |> unique() |> sort() |> trimws();

	data.table::setattr(env, "must.have", x);

	return(x);
};

#
`%check%` <- function(env, x){
#' Environment Integrity Check
#'
#' \code{\%check\%} checks the environment for required objects
#'
#' @param env (object | string) An environment or name of an environment
#' @param x (string[]) The names of the objects that \code{env} must have
#'
#' @return If all objects that \code{env} \code{\link{\%must.have\%}} are present, nothing is returned; otherwise, an error message is returned.
#' @family Environmental Integrity
#' @export

	env <- check_env_arg(env)

	logi.vec <- stats::na.omit(env %missing% x);

	if (any(stats::na.omit(logi.vec))){ stop(x[logi.vec] |> paste(collapse = ", ") |> sprintf(fmt = "Missing objects: %s")) }
}

#
`%+must.have%` <- function(env, x){
#' Must Have in Addition
#'
#' \code{\%\+must.have\%} adds additional object names as "must haves" for the given environment
#'
#' @param env (object) An environment or name of an environment
#' @param x (string[]) A string vector of the names that \code{env} must have when checked
#'
#' @return The names of the objects that \code{env} must have in addition to existing required objects, invisibly
#' @family Environmental Integrity
#' @export

	env <- check_env_arg(env);

	invisible(env %must.have% { c(attr(env, "must.have"), x) });
}

#
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

#
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

	rm(list = x[sapply(x, rlang::has_name, x = env)], envir = env);

	invisible(env);
}
