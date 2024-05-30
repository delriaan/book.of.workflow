# ::::: WORKFLOW MANAGEMENT
read_snippet <- read.snippet <- function(..., doc, action){
#' Read a Snippet from Source Code
#'
#' \code{read.snippet} returns pre-defined sections (snippets) of larger source files marked with "tag"-like syntax (e.g., \code{<snippet: }label\code{>}...\code{<}/snippet\code{>})
#' Because of the parsing used, it is important that statements end with a semi-colon (;) as is the case with many other programming languages.
#'
#' With the exception of \code{action = goto}, the document cursor moves to the closing snippet tag (i.e., \code{"</snippet>"}) when using RStudio.
#'
#' @param ... (\code{\link[rlang]{dots_list}}) Keywords given as strings or symbols for which a matching snippet is sought. Use \code{!!!} when passing vectors/lists.
#'
#' @param doc The input source document name given as a string: defaults to the active document when the function is invoked with no argument
#' @param action One of \code{goto}, \code{skip}, \code{parse}, \code{exec}, \code{save}
#'
#' @section \code{action}:
#' \itemize{
#' \item{goto (RStudio-only): Editor cursor jumps to beginning of code region without execution}
#' \item{skip (RStudio-only): Editor jumps to end of code region without execution}
#' \item{parse: Contents of the code region are run through \code{\link[base]{cat}} without execution}
#' \item{exec: Contents of the code region are parse and executed from the global environment}
#' \item{save: Contents of the code region are saved to the current working directory using the keywords contained in \code{`...`} ending in '.snippet'}
#' }
#'
#' @section Changelog:
#' \definition{
#' \item{Version 0.1.2.1000}{
#'	\itemize{
#'		\item{Argumennt `...` changed to be processed with \code{\link[rlang]{enexprs}}}
#'		}
#'	}
#' }
#'
#' @return The snippet text invisibly
#' @family Chapter 3 - Workflow Management
#' @export
#'
	if (...length() == 0){ stop("No snippet keywords provided in `...`") }
	is_studio_audience <- interactive() & "rstudioapi" %in% loadedNamespaces();

	if (missing(doc)){ doc <- ifelse(
		interactive()
		, ifelse(
				is_studio_audience
				, rstudioapi::getSourceEditorContext()$path
				, do.call(file.choose, args = NULL)
				)
		, NULL
		)
	}

	if (rlang::is_empty(doc)){
		stop("Value for 'doc' is empty.  Call this funciton from RStudio to default to current document: exiting ...")
	}

	label 	<- as.character(rlang::enexprs(...));
	pattern <- paste(label, collapse = ".+") |> sprintf(fmt = "<snippet[:].+%s.+");
	action	<- if (missing(action)){ "goto" } else { as.character(rlang::enexpr(action)) }

	# ::
	do.this <- { rlang::exprs(
		goto		= { message(sprintf("Moved to snippet <%s> ..." , paste(label, collapse = ", "))); }
		, skip	= { message(sprintf("Skipping snippet <%s> ..." , paste(label, collapse = ", "))); }
		, parse = { message(sprintf("Parsed snippet <%s> as:\n" , paste(label, collapse = ", "))); cat(out, sep = "\n"); }
		, exec	= { message(sprintf("Executing snippet <%s> ...", paste(label, collapse = ", "))); parse(text = out) |> eval(envir = globalenv()) }
		, save	= { .outfile = sprintf("%s.snippet", paste(label, collapse = "_"));
								message(sprintf("Saving snippet <%s> to '%s'", paste(label, collapse = ", "), .outfile));
								cat(out, sep = "\n", file = .outfile);
							}
		)}[[action]];

	# ::
	message("Searching " %s+% doc);

	file.data <- suppressWarnings(readtext::readtext(doc)$text |> stringi::stri_split_regex("\n", simplify = TRUE)) |>
		stringi::stri_replace_all_regex("read[.]snippet[(].+[)][;]?", "", vectorize_all = FALSE) |>
		stringi::stri_trim_both();

	match.pos <- which(grepl(pattern, file.data)) |>
		purrr::map(\(x){
			from = x
			to = which(grepl("</snippet", file.data)) |>
				purrr::keep(\(x) x > from) |>
				min(na.rm = TRUE);

			if (action == "goto"){ x } else { seq(from, to); }
		}) |>
		unlist();

  out <- paste(file.data[match.pos] |> stringi::stri_replace_all_regex("\t", "", vectorize_all = FALSE), collapse = "\n");

	# ::
	if (is_studio_audience){
	  if (!action %in% c("goto", "parse")){
	  	if (doc == rstudioapi::getSourceEditorContext()$path){
	  		rstudioapi::setCursorPosition(rstudioapi::as.document_position(c(max(match.pos) + 1L, 1L)))
	  	}
	  } else {
	  	if (doc == rstudioapi::getSourceEditorContext()$path){
	  	rstudioapi::setCursorPosition(rstudioapi::as.document_position(c(min(match.pos), 1L))) }
	  }
	}

	eval(do.this);
	invisible(out);
}
#
make_snippet <- make.snippet <- function(..., include.read = TRUE, use.clipboard = FALSE){
#' Make a Snippet
#'
#' On a new line, \code{make.snippet} creates "tag"-like comments referenced by \code{\link{read.snippet}} (e.g., \code{<snippet: }label\code{>}...\code{<}/snippet\code{>}).  Because of the parsing used, it is important that statements end with a semi-colon (;) as is the case with many other programming languages.  The opening "tag" is created as a code section.
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
	is_studio_audience <- interactive() & "rstudioapi" %in% loadedNamespaces();

	.args = as.character(rlang::enexprs(...));

	.text = { sprintf(
							fmt = "# <snippet: %s> ----%s"
							, paste(.args, collapse = " ")
							, ifelse(include.read, sprintf("\nread.snippet(%s, action = parse);\n", paste(.args, collapse = ", ")), "")
							)
						} |> c("\n# </snippet>\n") |> paste(collapse = "");

	if (use.clipboard){
		cat(.text);
		message("The above snippet has been saved to the clipboard.  Paste the contents in desired editor location.")
		utils::writeClipboard(.text);
		.Last.value <- .text;
	} else if (is_studio_audience){
		.tgt_pos = rstudioapi::getSourceEditorContext()$selection[[1]]$range$start;
		rstudioapi::insertText(location = .tgt_pos, text = .text);
	} else { utils::readClipboard() }
}
#
snippets_toc <- function(doc, choose = FALSE){
#' Snippets Table of Contents
#'
#' \code{snippets_toc} creates a table of contents of snippet code sections.
#'
#' @note An active session must be required to use this file if \code{doc} is \code{NULL}
#'
#' @param doc The path to a document containing code sections created via \code{\link{make.snippet}}()
#' @param choose (logical) When \code{TRUE}, an interactive dialog allowing for selection is presented. Selected snippets are executed.
#'
#' @section Changelog:
#' \definition{
#' \item{Version 0.1.2.1000}{Added the \code{choose} argument}
#' }
#'
#' @return Invisibly, a listing of snippet code sections for the document provided
#' @family Chapter 3 - Workflow Management
#' @export

	if (missing(doc) || rlang::is_empty(doc)){
		doc <- ifelse(
			interactive()
			, ifelse(
					"rstudioapi" == (installed.packages())[grepl("rstudio", rownames(installed.packages())), "Package"]
					, rstudioapi::getSourceEditorContext()$path
					, ""
					)
			, { message("This function requires an active session when argument 'doc' is not provided: exiting ...");
					return()
				}
			)
	}

	if (doc == ""){
		stop("Unable to determine the document to use: provide a document path.")
	}

	.toc <- readLines(doc) |>
		purrr::keep(\(x) grepl("^[# <]+snippet[:].+[>]", x)) |>
		stringi::stri_replace_all_regex("([# <]+snippet[: ])|([> ]+[-]+)", "", vectorize_all = FALSE) |>
		trimws();

	if (!choose){
		sprintf(
			"Snippet Table of Contents [%s], \n%s"
			, doc
			, paste(paste0(seq_along(.toc), ". ", .toc), collapse = "\n")
			) |>
			cat();
	} else {
		res <- svDialogs::dlg_list(
			stringi::stri_extract_first_regex(.toc, "([a-zA-Z0-9_[:space:]])+") |>
				trimws() |>
				stringi::stri_replace_all_fixed(" ", ", ", vectorize_all = FALSE)
			, title = glue::glue("Choose the snippet(s) from '{doc}' to execute:")
			, multiple = TRUE
			)$res;

		if (rlang::is_empty(res)){
			return(invisible(.toc))
		} else {
			purrr::walk(res, \(x){
				read.snippet(
					!!!stringi::stri_split_fixed(x, ", ", simplify = TRUE)
					, doc = doc
					, action = exec
					)
			});
		}
	}
	invisible(.toc)
}
