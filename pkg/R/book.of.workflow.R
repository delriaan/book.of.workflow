#' @title Book of Workflow
#
#' @description
#' `book.of.utilities` seeks to facilitate execution of those repetitive, ad-hoc tasks often encountered during data processing.
#'
#'
#' The following functional chapters are covered in \code{book.of.workflow}:\cr
#'
#' @section Chapter 1 - Environment Integrity: 
#' \itemize{
#' \item{\code{\link{?\%-=\%}}}
#' \item{\code{\link{?\%+=\%}}}
#' \item{\code{\link{?\%must.have\%}}}
#' \item{\code{\link{?check.env}}}
#' \item{\code{\link{?check_env_arg}}}
#' }
#'
#' @section Chapter 2 - Environment Processing: 
#' \itemize{
#' \item{\code{\link{?copy.obj}}}
#' \item{\code{\link{?copy_obj}}}
#' \item{\code{\link{?load.unloaded}}}
#' \item{\code{\link{?load_unloaded}}}
#' \item{\code{\link{?refer.to}}}
#' \item{\code{\link{?refer_to}}}
#' \item{\code{\link{?save.obj}}}
#' \item{\code{\link{?save_image}}}
#' }
#'
#' @section Chapter 3 - Workflow Management: 
#' \itemize{
#' \item{\code{\link{?check_action}}}
#' \item{\code{\link{?is_studio_audience}}}
#' \item{\code{\link{?make.snippet}}}
#' \item{\code{\link{?make_snippet}}}
#' \item{\code{\link{?read.snippet}}}
#' \item{\code{\link{?read_snippet}}}
#' \item{\code{\link{?snippets.toc}}}
#' \item{\code{\link{?snippets_toc}}}
#' }
#'
#'
#' @importFrom magrittr %>% %T>% or %<>%
#' @importFrom purrr map map_lgl map_chr reduce modify modify_if modify_at
#' @importFrom data.table %ilike% %like% like := .N .SD
#' @importFrom foreach %do% %dopar%
#' @importFrom methods new
#' @importFrom stats sd
#' @importFrom utils object.size
#' @name book.of.workflow
NULL
