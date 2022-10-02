# ::::: ENVIRONMENT INTEGRITY
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

	if (is.character(env)) {
		if (env %in% search()){ as.environment(env) } else { parse(text = env) %>% eval() }
	} else { eval(env) } %>% {
		!(is.null(must.have(.)) || (must.have(.) %in% ls(., all.names = TRUE)))
	}
};
#
must.have <- function(env, x){
#' Must Have
#'
#' \code{must.have} and related operator \code{\%must.have\%} sets an attribute in the environment given by \code{env} with the name(s) of the object(s) that the environment must have.  Verification is done via \code{\link{env.check}}.
#'
#' @param env (object) An environment or name of an environment
#' @param x (string[]) A string vector of the names that \code{env} must have when checked
#'
#' @return The names of the objects that \code{env} must have
#' @family Environmental Integrity
#' @export

	env = if (is.character(env)) {
		if (env %in% search()){ as.environment(env) } else { parse(text = env) %>% eval() }
	} else { eval(env) }

	mh = attr(env, "must.have");

	x = if (missing(x) || is.null(x)) { mh } else { x } %>% unique %>% sort %>% trimws;

	setattr(env, "must.have", x);
	message(paste(attr(env, "must.have"), collapse = ", "));

	return(x);
};
#
#' @export
`%must.have%` <- must.have;
#
env.check <- function(env, x){
#' Environment Integrity Check
#'
#' \code{env.check} and related operator \code{\%check\%} checks the environment for required objects
#'
#' @param env (object|string) An environment or name of an environment
#' @param x (string[]) The names of the objects that \code{env} must have
#'
#' @return If all objects that \code{env} \code{\link{must.have}} are present, nothing is returned; otherwise, an error message is returned.
#' @family Environmental Integrity
#' @export

	env = if (is.character(env)) {
		if (env %in% search()){ as.environment(env) } else { parse(text = env) %>% eval() }
	} else { eval(env) }

	logi.vec = na.omit(env %missing% x);

	if (any(na.omit(logi.vec))){ stop(x[logi.vec] %>% paste(collapse = ", ") %>% sprintf(fmt = "Missing objects: %s")) }
}
#
#' @export
`%check%` <- env.check;
#
`%+must.have%` <- function(env, x){
#' Must Have in Addition
#'
#' Operator \code{\%\+must.have\%} adds additional object names as "must haves" for the given environment
#'
#' @param env (object) An environment or name of an environment
#' @param x (string[]) A string vector of the names that \code{env} must have when checked
#'
#' @return The names of the objects that \code{env} must have in addition to existing required objects, invisibly
#' @family Environmental Integrity
#' @export

	env = if (is.character(env)) {
		if (env %in% search()){ as.environment(env) } else { parse(text = env) %>% eval() }
	} else { eval(env) }

	invisible(env %must.have% { c(attr(env, "must.have"), x) });
}
#
`%+=%` <- function(env, x){
#' Assignment Shorthand
#'
#' Operator \code{\%+=\%} is a wrapper for \code{base::list2env()}
#'
#' @param env (environment, string) An environment or name of an environment
#' @param x	(list) A named list with the names serving as the object name to add to \code{env} and the values the object definitions/contents for each object.
#'
#' @return The target environment, invisibly
#'
#' @family Environmental Integrity
#' @export

	env = if (is.character(env)) {
		if (env %in% search()){ as.environment(env) } else { parse(text = env) %>% eval() }
	} else { eval(env) }

	list2env(x, envir = env);
	# map(x, ~try({ assign(.x[[1]], eval(.x[[2]]), envir = env) }, silent = TRUE));

	invisible(env);
}
#
`%-=%` <- function(env, x){
#' Remove Objects from an Environment
#'
#' Operator \code{\%-=\%} is a wrapper for \code{base::rm()}
#'
#' @param env (environment, string) An environment or name of an environment
#' @param x	(string[]) A collection of strings representing the names of the objects to remove from \code{env}
#'
#' @return The target environment, invisibly
#'
#' @family Environmental Integrity
#' @export

	env = if (is.character(env)) {
		if (env %in% search()){ as.environment(env) } else { parse(text = env) %>% eval() }
	} else { eval(env) }

	rm(list = x[sapply(x, exists, envir = env)], envir = env);
	invisible(env);
}
#