% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_environment_integrity.R
\name{\%+=\%}
\alias{\%+=\%}
\title{Assignment Shorthand}
\usage{
env \%+=\% x
}
\arguments{
\item{env}{(environment, string) An environment or name of an environment}

\item{x}{(list) A named list with the names serving as the object name to add to \code{env} and the values the object definitions/contents for each object.}
}
\value{
The target environment, invisibly
}
\description{
\code{\%+=\%} wraps \code{base::list2env()}
}
\seealso{
Other Environmental Integrity: 
\code{\link{\%+must.have\%}()},
\code{\link{\%-=\%}()},
\code{\link{\%check\%}()},
\code{\link{\%missing\%}()},
\code{\link{\%must.have\%}()}
}
\concept{Environmental Integrity}
