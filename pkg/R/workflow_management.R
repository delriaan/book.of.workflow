# ::::: WORKFLOW MANAGEMENT
is_studio_audience <- \() interactive() & "rstudioapi" %in% loadedNamespaces();
check_action <- \(action){
	if (!action %in% c('skip', 'parse', 'exec', 'save', 'html')){
		cli::cli_alert_warning("Invalid action ({action}): defaulting to 'skip'")
		cli::alert_info("Valid actions include 'skip', 'parse', 'exec', 'save', and 'html'")
		action <- "skip"
	}
	return(action)
} 

read_snippet <- read.snippet <- function(..., doc = NULL, action = "parse", chatty = FALSE){
#' Read a Snippet from Source Code
#'
#' \code{read_snippet} returns pre-defined sections (snippets) of larger source files marked with "tag"-like syntax (e.g., \code{<snippet: }label\code{>}...\code{<}/snippet\code{>})
#' Because of the parsing used, it is important that statements end with a semi-colon (;) as is the case with many other programming languages.
#'
#' @param ... (\code{\link[rlang]{dots_list}}) Keywords given as strings or symbols for which a matching snippet is sought. Use \code{!!!} when passing vectors/lists.
#'
#' @param doc The input source document name given as a string: defaults to the active document when the function is invoked with no argument
#' @param action (See \code{action} below.)
#' @param chatty (logical) Should execution message be generated?
#'
#' @section \code{action}:
#' \itemize{
#' \item{skip: No code is executed}
#' \item{parse: Contents of the code region are parsed and printed without execution}
#' \item{exec: Contents of the code region are parse and executed from the \emph{global} environment}
#' \item{save: Contents of the code region are saved to the current working directory using the keywords contained in \code{`...`} with file extension \code{'.snippet'}}
#' \item{html: Contents of the code region are rendered to the viewer using the keywords contained in \code{`...`}}
#' }
#'
#' @note Only the first matching position for the opening snippet tag is used; therefore, ensure that tag labels are unique.
#' 
#' @return The snippet text invisibly
#' @family Chapter 3 - Workflow Management
#' @aliases read.snippet
#' @export
#'
	if (...length() == 0){ stop("No snippet keywords provided in `...`") }

	if (rlang::is_empty(doc)){ 
		doc <- ifelse(
			interactive()
			, ifelse(
					is_studio_audience()
					, rstudioapi::getSourceEditorContext()$path
					, file.choose()
					)
			, NULL
			)
	}

	if (rlang::is_empty(doc)){
		stop("Value for 'doc' is empty.  Call this funciton from RStudio to default to current document: exiting ...")
	}

	action	<- as.character(rlang::enexpr(action)) |> check_action()

	label 	<- as.character(rlang::enexprs(...));
	pattern <- paste(label, collapse = ".+") |> sprintf(fmt = "<snippet[:].+%s.+");

	# ::
	do.this <- { rlang::exprs(
		skip	= { cli::cli_alert_info("Skipping snippet <{paste(label, collapse = ', ')}> ...") }
		, parse = { 
				cli::cli_alert_info("Parsed snippet <{paste(label, collapse = ', ')}> as:\n")
				paste(out, collapse = "\n") |> cat(sep = "\n")
			}
		, exec = { 
				cli::cli_alert_info("Executing snippet <{paste(label, collapse = ', ')}> ...")
				paste(out, collapse = "\n") |>
					rlang::parse_exprs() |> 
					purrr::walk(eval, envir = globalenv())
			}
		, save	= { 
				.outfile <- sprintf("%s.snippet", paste(label, collapse = "_"))
				cli::cli_alert_info("Saving snippet <{paste(label, collapse = ', ')}> to <{(.outfile)}> ...")
				cat(out, sep = "\n", file = .outfile, append = FALSE)
			}
		, html	= { 
				.outfile <- sprintf("%s.html", paste(label, collapse = "_"))
				cli::cli_alert_info("Saving snippet <{paste(label, collapse = ', ')}> to <{(.outfile)}> ...")
				
				purrr::modify_at(out, c(1, length(out)), htmltools::htmlEscape) |> 
					stringi::stri_replace_all_regex(
						c("\n", "(\t)|([[:space:]]{2})")
						, c("<br>", "&nbsp;&nbsp;&nbsp;")
						, vectorize_all = FALSE
						) |>
					paste(collapse = "<br>") |>
					htmltools::HTML() |>
					htmltools::tags$code(style = "font-size:16pt;") |>
					htmltools::html_print()
			}
		)
	}[[action]]

# Read in the text content:
cli::cli_alert_info("Searching <{doc}>")
file.data <- suppressWarnings(
	readtext::readtext(doc)$text |> 
		stringi::stri_split_regex("\n", simplify = TRUE)
	) |>
  stringi::stri_trim_right()

# Find the positions of matches containing the snippet patterns:
match.pos <- grepl(pattern, file.data)
	
if (!any(match.pos)){
	cli::cli_alert_warning("No valid snippet tags found: exiting ...")
	return(invisible())
}
	
# Check to see if the snippet can be skipped first:
if (action == "skip"){ 
	eval(do.this) 
	return(invisible())
}

# Only the first matching position for the opening snippet tag is used.
# The user should ensure that tag labels are unique.
match.pos <- which(match.pos)[1] |> 
	(\(x){
    from <- x
    to <- which(grepl("[[:space:]]+?</snippet", file.data))

    if (rlang::is_empty(to)){
      cli::cli_alert_danger("Closing snippet tag not detected.")
      stop("Ill-formed snippet tag.")
    } else {
      to <- to[to > from] |> min(na.rm = TRUE);
    }

		res <- seq(from, to)
		rs <- stringi::stri_detect_regex(file.data[res], "read[.]snippet[(].+[)][;]?")
		if (any(rs) & action == "exec"){ res <- res[!rs] }
		
		# Return 
		res
  })() |>
  unlist();

  out <- file.data[match.pos]

	eval(do.this)
	invisible(out)
}
#
make_snippet <- make.snippet <- function(..., include.read = TRUE, use.clipboard = FALSE){
#' Make a Snippet
#'
#' On a new line, \code{make_snippet} creates "tag"-like comments referenced by \code{\link{read.snippet}} (e.g., \code{<snippet: }label\code{>}...\code{<}/snippet\code{>}).  Because of the parsing used, it is important that statements end with a semi-colon (;) as is the case with many other programming languages.  The opening "tag" is created as a code section.
#'
#' @param ... (\code{\link[rlang]{dots_list}}) Symbols or words serving as keywords that, taken together, distinguish the snippet from others in the same source document
#'
#' @param include.read (logical) When \code{TRUE}, an associated \code{\link{read.snippet}} command is provided with argument \code{eval = parse}
#'
#' @param use.clipboard (logical) When \code{TRUE}, the snippet contents are saved to the clipboard and \code{`.Last.value`}
#'
#' @return When \code{use.clipboard} is \code{FALSE}, a pair of "tag"-like comments between which code is to be supplied; otherwise, the contents are saved to the clipboard (Windows OS only)
#' @family Chapter 3 - Workflow Management
#' @aliases make.snippet
#' @export

	if (!interactive()){ return(invisible(NULL)) }
	.args = as.character(rlang::enexprs(...));

	.text = { 
		sprintf(
			fmt = "# <snippet: %s> ----%s"
			, paste(.args, collapse = " ")
			, ifelse(include.read, sprintf("\nread.snippet(action = parse, doc = NULL, %s);\n", paste(.args, collapse = ", ")), "")
			)
		} |> 
		c("\n# </snippet>\n") |> paste(collapse = "");

	if (use.clipboard){
		cat(.text);
		message("The above snippet has been saved to the clipboard.  Paste the contents in desired editor location.")
		utils::writeClipboard(.text);
		.Last.value <- .text;
	} else if (is_studio_audience()){
		.tgt_pos = rstudioapi::getSourceEditorContext()$selection[[1]]$range$start;
		rstudioapi::insertText(location = .tgt_pos, text = .text);
	} else { 
		utils::readClipboard() 
	}
}
#
snippets_toc <- function(doc = NULL, choose = FALSE, action = "skip"){
#' Snippets Table of Contents
#'
#' \code{snippets_toc} creates a table of contents of snippet code sections and optionally invokes \code{\link{read_snippet}}
#'
#' @note An active session must be required to use this file if \code{doc} is \code{NULL} or \code{choose=TRUE}
#'
#' @param doc The path to a document containing code sections created via \code{\link{make_snippet}}()
#' @param choose (logical) Should snippets be interactively selected?
#' @param action (See \code{\link{read_snippet}}) 
#' 
#' @note When \code{action=skip} (default) or \code{choose=FALSE} (default), the snippets listing is sent to the console without execution.
#'
#' @return Invisibly, a listing of snippet code sections for the document provided
#' @family Chapter 3 - Workflow Management
#' @export
	if (rlang::is_empty(doc)){ 
		doc <- ifelse(
			interactive()
			, ifelse(
					is_studio_audience()
					, rstudioapi::getSourceEditorContext()$path
					, file.choose()
					)
			, NULL
			)
	}

	if (rlang::is_empty(doc)){
		stop("This function requires an active session when argument 'doc' is not provided: exiting ...");
	}

	action	<- as.character(rlang::enexpr(action)) |> check_action()

	.toc <- readLines(doc) |>
		purrr::keep(\(x) grepl("^[# <]+snippet[:].+[>]", x)) |>
		stringi::stri_replace_all_regex("([# <]+snippet[: ])|([> ]+[-]+)", "", vectorize_all = FALSE) |>
		trimws();

	if ((action == "skip") | !choose){
		sprintf(
			"Snippet Table of Contents [%s], \n%s"
			, doc
			, paste(paste0(seq_along(.toc), ". ", .toc), collapse = "\n")
			) |>
			cat()
	} else {
		snips <- stringi::stri_extract_first_regex(.toc, "([a-zA-Z0-9_[:space:]])+") |>
			trimws() |>
			stringi::stri_replace_all_fixed(" ", ", ", vectorize_all = FALSE)

		res <- svDialogs::dlg_list(
			choices = snips
			, title = glue::glue("Choose one or more snippets found in <{doc}>:")
			, multiple = TRUE
			)$res

		if (rlang::is_empty(res)){
			return(invisible(.toc))
		} else {
			purrr::walk(res, \(x){
				read.snippet(
					doc = doc
					, action = !!action
					, !!!stringi::stri_split_fixed(x, ", ", simplify = TRUE)
					)
			})
		}
	}
	invisible(.toc)
}
#
#' @export
read.snippet <- read_snippet

#' @export
make.snippet <- make_snippet
