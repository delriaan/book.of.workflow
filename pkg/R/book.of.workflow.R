#' @title Book of Workflow
#'
#' @description
#' `book.of.workflow` facilitates repetitive, ad hoc tasks commonly encountered
#' during data processing and workflow management.
#'
#' The following functional chapters are covered in `book.of.workflow`:
#'
#' @section Chapter 1 - Environment Integrity:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{\%-=\%}} \tab Removes named objects from an environment target.\cr
#' \code{\link{\%+=\%}} \tab Assigns/adds named objects into an environment target.\cr
#' \code{\link{\%must.have\%}} \tab Validates required objects and enforces presence checks.\cr
#' \code{\link{check.env}} \tab Checks environment state and object availability.\cr
#' \code{\link{check_env_arg}} \tab Validates environment argument inputs for workflow helpers.\cr
#' }
#'
#' @section Chapter 2 - Environment Processing:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{copy.obj}} \tab Copies selected objects between environments.\cr
#' \code{\link{copy_obj}} \tab Alias for \code{\link{copy.obj}}.\cr
#' \code{\link{load.unloaded}} \tab Loads objects that are currently not present in memory.\cr
#' \code{\link{load_unloaded}} \tab Alias for \code{\link{load.unloaded}}.\cr
#' \code{\link{refer.to}} \tab Creates or updates object references for reuse.\cr
#' \code{\link{refer_to}} \tab Alias for \code{\link{refer.to}}.\cr
#' \code{\link{save.obj}} \tab Saves selected objects to disk.\cr
#' \code{\link{save_image}} \tab Saves the working image/environment state.\cr
#' }
#'
#' @section Chapter 3 - Workflow Management:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{check_action}} \tab Validates and normalizes action declarations in workflows.\cr
#' \code{\link{is_studio_audience}} \tab Detects whether the session target is an RStudio audience.\cr
#' \code{\link{make.snippet}} \tab Creates reusable snippet definitions.\cr
#' \code{\link{make_snippet}} \tab Alias for \code{\link{make.snippet}}.\cr
#' \code{\link{read.snippet}} \tab Reads saved snippet content for reuse.\cr
#' \code{\link{read_snippet}} \tab Alias for \code{\link{read.snippet}}.\cr
#' \code{\link{snippets_toc}} \tab Produces a table of contents for available snippets.\cr
#' }
#'
#' @section Installation:
#' Use `remotes::install_github("delriaan/book.of.workflow", subdir = "pkg")` to install the latest version from GitHub.
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
