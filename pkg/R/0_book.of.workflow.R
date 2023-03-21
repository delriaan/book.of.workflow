#' @title Book of Workflow Overview
#'
#' @description
#' The following functional families are covered in \code{book.of.workflow}:
#'
#' @section Chapter 1 - Environment-processing:
#'	The focus is on loading libraries and managing environment objects:
#'	\itemize{
#'		\item{\code{\link{load_unloaded}}}
#'		\item{\code{\link{save_image}}}
#'		\item{\code{\link{copy_obj}}}
#'	}
#'
#' @section Chapter 2 - Environment Integrity:
#'  This family of functions is focused on maintaining the integrity of object environments, primarily, to mitigate errors due to  missing object dependencies:
#'  \itemize{
#'  	\item{\code{\link{check.env}}}
#'  	\item{\code{\link{\%must.have\%}}}
#'  	\item{\code{\link{\%+=\%}}}
#'  	\item{\code{\link{\%-=\%}}}
#'  }
#'
#' @section Chapter 3 - Workflow Management:
#'  This family of functions focuses on code execution workflow:
#'  \itemize{
#'  	\item{\code{\link{read.snippet}}}
#'  	\item{\code{\link{make.snippet}}}
#'  	\item{\code{\link{snippets_toc}}}
#  	\item{\code{\link{workflow_manager}}}
#'  }
#'
# @section Chapter 4 - External Data Management (REMOVED):
#  This family of functions focuses on connecting to relational databases
#  \itemize{
#  	\item{\code{\link{make_query}}}
#  	\item{\code{\link{get_data}}}
#  	\item{\code{\link{export_data}}}
#  }
#'
#' @importFrom magrittr %>% %T>% %<>% %$% freduce not
#' @importFrom data.table %between% %ilike% %like% rbindlist last setattr
#' @importFrom stringi %s+%
#' @importFrom utils askYesNo installed.packages
#'
#' @name book.of.workflow
NULL