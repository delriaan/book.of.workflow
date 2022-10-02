# ::::: WORKFLOW MANAGEMENT
read.snippet <- function(..., doc, action){
#' Read a Snippet from Source Code
#'
#' \code{read.snippet} returns pre-defined sections (snippets) of larger source files marked with "tag"-like syntax (e.g., \code{<snippet: }label\code{>}...\code{<}/snippet\code{>})
#' Because of the parsing used, it is important that statements end with a semi-colon (;) as is the case with many other programming languages.
#'
#' With the exception of \code{action = goto}, the document cursor moves to the closing snippet tag (i.e., \code{"</snippet>"}) when using RStudio.
#'
#' @param ... Keywords given as strings or symbols for which a matching snippet is sought
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
#' @return The snippet text invisibly
#'
#' @family Code Management
#' @export
#'
	if (...length() == 0){ stop("No snippet keywords provided in `...`") }
	is_studio_audience <- interactive() & "rstudioapi" %in% loadedNamespaces();

	if (missing(doc)){ doc <- ifelse(
		interactive()
		, ifelse(
				is_studio_audience
				, rstudioapi::getSourceEditorContext()$path
				, utils::file.choose()
				)
		, NULL
		)
	}
	if (rlang::is_empty(doc)){ stop("Value for 'doc' is empty.  Call this funciton from RStudio to default to current document: exiting ...") }

	action	= if (missing(action)){ "goto" } else { as.character(rlang::enexprs(action)) }
	label 	= as.character(rlang::exprs(...));
	pattern = paste(label, collapse = ".+") %>% sprintf(fmt = "<snippet[:].+%s.+");

	# ::
	do.this = { rlang::exprs(
		goto		= { message(sprintf("Moved to snippet <%s> ..." , paste(label, collapse = ", "))); }
		, skip	= { message(sprintf("Skipping snippet <%s> ..." , paste(label, collapse = ", "))); }
		, parse = { message(sprintf("Parsed snippet <%s> as:\n" , paste(label, collapse = ", "))); cat(out, sep = "\n"); }
		, exec	= { message(sprintf("Executing snippet <%s> ...", paste(label, collapse = ", "))); parse(text = out) %>% eval(envir = globalenv()) }
		, save	= { .outfile = sprintf("%s.snippet", paste(label, collapse = "_"));
								message(sprintf("Saving snippet <%s> to '%s'", paste(label, collapse = ", "), .outfile));
								cat(out, sep = "\n", file = .outfile);
							}
		)}[[action]];

	# ::
	message("Searching " %s+% doc);

	file.data = suppressWarnings(readtext::readtext(doc)$text %>% stri_split_regex("\n", simplify = TRUE)) %>%
		stri_replace_all_regex("read[.]snippet[(].+[)][;]?", "", vectorize_all = FALSE) %>%
		stri_trim_both();

	match.pos = map(which(file.data %ilike% pattern), ~{
		from = .x
		to = which(file.data %ilike% "</snippet") %>% keep(~.x > from) %>% min();

		if (action == "goto"){ .x } else { seq(from, to); }
	}) %>% unlist();

  out = paste(file.data[match.pos] %>% stri_replace_all_regex("\t", "", vectorize_all = FALSE), collapse = "\n");

	# ::
	if (is_studio_audience){
	  if (!action %in% c("goto", "parse")){
	  	if (doc == rstudioapi::getSourceEditorContext()$path){
	  		rstudioapi::setCursorPosition(rstudioapi::as.document_position(c(max(match.pos) + 1L, 1L)))
	  	}
	  } else { if (doc == rstudioapi::getSourceEditorContext()$path){
	  	rstudioapi::setCursorPosition(rstudioapi::as.document_position(c(min(match.pos), 1L)))}
	  }
	}

	eval(do.this);
	invisible(out);
}
#
make.snippet <- function(..., include.read = TRUE, use.clipboard = FALSE){
#' Make a Snippet
#'
#' On a new line, \code{make.snippet} creates "tag"-like comments referenced by \code{\link{read.snippet}} (e.g., \code{<snippet: }label\code{>}...\code{<}/snippet\code{>}).  Because of the parsing used, it is important that statements end with a semi-colon (;) as is the case with many other programming languages.  The opening "tag" is created as a code section.
#'
#' @param ... Symbols or words serving as keywords that, taken together, distinguish the snippet from others in the same source document
#' @param include.read (logical) When \code{TRUE}, an associated \code{\link{read.snippet}} command is provided with argument \code{eval = parse}
#' @param use.clipboard (logical) When \code{TRUE}, the snippet contents are saved to the clipboard and \code{`.Last.value`}
#'
#' @return When \code{use.clipboard} is \code{FALSE}, a pair of "tag"-like comments between which code is to be supplied; otherwise, the contents are saved to the clipboard (Windows OS only)
#'
#' @family Code Management
#' @export

	if (!interactive()){ return(invisible(NULL)) }
	is_studio_audience <- interactive() & "rstudioapi" %in% loadedNamespaces();

	.args = as.character(rlang::exprs(...));

	.text = { sprintf(
						fmt = "# <snippet: %s> ----%s"
						, paste(.args, collapse = " ")
						, ifelse(include.read, sprintf("\nread.snippet(%s, action = parse);\n", paste(.args, collapse = ", ")), "")
						)} %>% c("\n# </snippet>\n") %>% paste(collapse = "");

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
execute.workflow <- function(wf, wf_step, list.only = FALSE){
#' Execute a Stored Workflow Step
#'
#' \code{execute.workflow} executes quoted expressions referencing \code{read.snippet} calls.
#'
#' @param wf (list) The workflow queue object containing the quoted workflow steps
#' @param wf_step (string/symbol) The names of the steps to execute.  Tip: label the steps in the order they should execute.
#' @param list.only (logical | FALSE) When \code{TRUE}, available workflows and workflow steps are printed to console before the function exits.
#'
#' @return When \code{list.only == TRUE}, the listing of workflows and corresponding steps invisibly; otherwise, nothing.
#' @export

	if (list.only){
		.out = refer.to("workflow") %$%
			ls(pattern = "wf[_][0-9]+[_][a-z]+") %>%
			purrr::set_names() %>%
			imap(~{
				.prefix = .y %s+% ": \n- ";
				cat(.prefix);
				refer.to("workflow")[[.x]] %$% { cat(paste(ls(), collapse = "\n- "), "\n\n") }
			});

		return(invisible(.out));
	}

	if (missing(wf)){
		wf <- tcltk::tk_select.list(ls("workflow", pattern = "wf[_][0-9]+[_][a-z]+"), title = "Choose a workflow to search:");
		if (rlang::is_empty(wf)){ message("No workflow selected: exiting ...");	return(invisible(0)) }
	}

	wf_step <- if (missing(wf_step)){ wf %$% mget(tcltk::tk_select.list(ls(), title = "Choose the workflow steps to execute in order", multiple = TRUE))
	} else { rlang::expr(!!wf_step) %>% as.character() }

	walk(wf_step, eval, envir = globalenv());
}
#
make.workflow <- function(..., wf_name = "new_wf", envir = NULL, eval = TRUE){
#' Create a Workflow Object
#'
#' \code{make.workflow}() assigns an expression list to the target environment.  These objects can then be invoked by calling \code{execute.workflow}()
#'
#' @param ... Unquoted expressions to be executed in the workflow in the order given.  Named arguments are suggested for clarity
#' @param wf_name The name of the workflow list object for assignment
#' @param envir The target environment for assignment
#' @param eval (logical|TRUE) Should the assignment expression be evaluated after creation?
#'
#' @export
	.steps = rlang::exprs(..., .unquote_names = TRUE);
	wf_name = as.character(rlang::enexpr(wf_name))

	if (rlang::is_empty(wf_name)){ wf_name <- "new_wf"  }

	.dflt_nms = seq_along(.steps) |>
		stringi::stri_pad_left(width = 2, pad = "0") |>
		sprintf(fmt = "step_%s")


	.miss_nms = which(names(.steps) == "");

	names(.steps)[.miss_nms] <- .dflt_nms[.miss_nms];

	if (!rlang::is_empty(envir)){
		.out = rlang::expr(assign(!!wf_name, !!.steps, envir = !!envir))
		if (eval){ eval(.out, envir = globalenv()) } else { .out }
	} else { rlang::list2(!!wf_name := .steps) }
}
#
mgr_upgrade <- function(ref){
#' Upgrade a workflow Manager
#'
#' \code{mgr_upgrade} copies predefined class field 'workflows' to a new class instance of the latest (internally-tracked) version.  The contents are returned to an object with the original call defined in \code{ref}
#'
#' @param ref An unquoted call to a 'workflow_manager' object
#'
#' @return Possibly console messages, but nothing else.
#'
#' @export

	if (missing(ref)){ message("Nothing to upgrade: exiting with no action taken ..."); return(invisible()) }
	ref = deparse(substitute(ref));
	obj	= ref %>% str2lang() %>% eval();
	old_ver = obj$.__enclos_env__$private$version;

	if (rlang::is_empty(old_ver) || old_ver < workflow_manager$private_fields$version){
		xfer = "workflows";
		curr = obj[[xfer]] %$% mget(ls()) %>% map(identical, obj$current) %>% keep(~.x) %>% names();
		out  = workflow_manager$new();
		out$workflows = obj$workflows;
		substitute(out$get(curr), list(curr = curr)) %>% eval();

		parse(text = sprintf("%s <- out", ref)) %>% eval();
	} else { message("Object is up-to-date: exiting with no action taken ..."); return(invisible()) }
}
#
#' @title Workflow Manager
#' @description
#' \code{workflow_manager} is an R6 class that helps to manage workflow-related tasks
#'
#' @export
workflow_manager <- { R6::R6Class(
	lock_objects = FALSE
	, classname = "workflow_manager"
	, public = list(
			#' @field workflows Holds the workflow sets (expression lists)
			workflows = NULL,
			#' Initialize the Workflow  Manager
			#'
			#' \code{$new} initializes a new object
			#'
			#' @param ... Not used
			#'
			#' @return Invisibly, the class environment
			initialize = function(...){
				if (!"package:magrittr" %in% search()){ library(magrittr)}
				invisible(self); self$workflows <- new.env();
			},
			#' Manage Workflow Sets
			#'
			#' \code{$manage_sets} will add or remove workflow sets from \code{$workflows}  A set is an ordered, named expression list which can be manually defined or created with \code{make.workflow()}
			#'
			#' Each of the above items can be defined according to taste, but the names must be passed as show above when \code{action} is one of 'add', 'update', 'remove', 'delete'.
			#' In addition, each element of \code{...} \emph{must} be supplied as a list (e.g., \code{fitted = list(<objects>)})
			#'
			#' @param action One of \code{add}, \code{update}, \code{remove}, \code{delete}, or \code{copy} given as strings or symbols (see section 'Action')
			#' @param wf_name One or more names to use for saved workflow sets
			#' @param ... For each term expected in a set, the object data provided in a named list, each element being the same length as \code{wf_name}
			#' @param confirm.rm (logical) Set to \code{FALSE} if you're feeling lucky -- punk!
			#' @param chatty (logical \ FALSE) Verbosity flag
			#'
			#' Argument \code{action}:\cr
			#' \describe{
			#'	\item{add}{Populate objects into a \emph{new} object set stored in \code{$workflows}}
			#'	\item{update}{Similar to \code{add} but to an existing stored object set }
			#'	\item{remove, delete}{Remove a stored object set by name}
			#'	\item{copy, clone}{Copy a stored object set under a new name}
			#' }
			#'
			#' @return Invisibly, the class environment
			manage = function(action, wf_name = private$curnt, ..., confirm.rm = quote(askYesNo("Remove %s?" %>% sprintf(wf_name))), chatty = FALSE){
				action =  if (is.symbol(action)){ as.character(action)
				} else if (is.language(action)|is.call(action)|is.expression(action)){
					eval(action, envir = globalenv())
				} else { substitute(action) %>% as.character() }

				if (is.null(action)){ action <- tcltk::tk_select.list(
					choices = c("add", "update", "remove/delete")
					, multiple = TRUE
					, title = "Choose one or more actions to take:")
				}
				in_data = rlang::list2(...)

				action.queue = { rlang::exprs(
					add = {
						purrr::iwalk(!!wf_name, ~{
							this.name = .x;
							this.idx = .y;

							if (hasName(self$workflows, this.name)){
								this.name <- sprintf("%s_%s", this.name, stringi::stri_pad_left(length(ls(self$workflows, pattern = this.name)), width = 2, pad = "0")) }

							# Add objects if dots list is named and not empty
							if (chatty){
								message(sprintf("Adding %s to %s", paste(names(in_data[[this.idx]][[1]]), collapse = ", "), this.name))
							}
							self$workflows[[this.name]] <- in_data[[this.idx]][[1]]
						});
						if (length(ls(self$workflows)) == 1){ self$current <- ls(self$workflows) }
					}
					, update = {
						purrr::iwalk(!!wf_name, ~{
							this.name = .x;
							this.idx = .y;
							if (hasName(self$workflows, this.name)){
								# Add objects if dots list is named and not empty
								if (chatty){ message(sprintf(
									"Updating %s with %s: ", this.name, paste(names(in_data[this.idx]), collapse = ", ")
								))
								}
								purrr::walk(in_data[[this.idx]], ~{ self$workflows[[this.name]][names(.x)] <- .x })
							} else { if (chatty){ message("Nothing to update") }}
						})
					}
					, remove = { purrr::walk(!!wf_name, ~{
						this.name = .x;
						if (hasName(self$workflows, this.name)){
							if (eval(confirm.rm) == TRUE){
								rm(list = this.name, envir = self$workflows);
								if (length(ls(self$workflows)) == 1){ private$curnt <- ls(self$workflows) }
								if (length(ls(self$workflows)) == 0){
									if (chatty){ message("No workflows exist!") }
									private$curnt <- NULL
								}
							} else { message("No action taken ...")}
						} else { message(paste0(this.name, " not found to remove ...")) }
					})
					}
					, copy = {
						purrr::iwalk(!!wf_name, ~{
							this.idx = .y
							message("Pick an existing workflow set to copy")
							self$get();
							self$manage(action = add, wf_name = .x, in_data[this.idx], chatty = chatty);
						});

						do.call(self$get, args = list(wf_name = wf_name));
					}
				)} %>% append(list(delete = .$remove, clone = .$copy));

				queue.idx = which(stringi::stri_detect_regex(names(action.queue), action)) %>%
					unique() %>% .[1];

				eval(action.queue[[queue.idx]]);
				if (rlang::is_empty(private$curnt)){ self$current <- wf_name[1] }

				invisible(self);
			},
			#' Get a Saved Set of Workflow Objects
			#'
			#' \code{$get} will retrieve the object in \code{$workflows} named in \code{wf_name} out and set \code{$current} to the environment with the name of that in \code{wf_name}
			#'
			#' @param wf_name A string or symbol of the name of the set to get
			#'
			#' @return Invisibly, the class environment
			get = function(wf_name){
				if (rlang::is_empty(ls(self$workflows))){ message("No workflows exist, nothing to get ..."); return(invisible(self)) }

				if (missing(wf_name)){
					wf_name <- tcltk::tk_select.list(ls(self$workflows), title = "Choose a workflow set to make active: ", multiple = FALSE)
				} else {
					wf_name <- purrr::when(
						. = wf_name
						, is.integer(.) ~ ls(self$workflows)[abs(.)]
						, is.symbol(.) ~ substitute(wf_name) %>% as.character()
						, ~ .)
				}
				if (hasName(self$workflows, wf_name)){
					self$current <- wf_name;
					message(sprintf("'%s' is the current workflow set", wf_name));
				} else { message(wf_name %s+% " not found to retrieve ...") }

				invisible(self);
			},
			#' Execute a Stored Workflow Step
			#'
			#' \code{execute.workflow} executes quoted expressions referencing \code{read.snippet} calls.
			#'
			#' @param wf (list) The name of the workflow object containing the workflow step expressions
			#' @param wf_step (string/symbol) The names of the steps to execute.  Tip: label the steps in the order they should execute.
			#' @param list.only (logical | FALSE) When \code{TRUE}, available workflows and workflow steps are printed to console before the function exits.
			#' @param passive (logical | TRUE) When \code{FALSE}, workflow steps are interactively selected
			#'
			#' @return When \code{list.only == TRUE}, the listing of workflows and corresponding steps invisibly; otherwise, nothing.
			exec = function(wf, wf_step, list.only = FALSE, passive = TRUE){
				.workflows = ls(self$workflows)
				#
				if (list.only){
					.out = purrr::set_names(.workflows) %>%
						purrr::imap(~{
							.prefix = .y %s+% ": \n- ";
							cat(.prefix);
							(self$workflows) %$% { cat(paste(ls(), collapse = "\n- "), "\n\n") }
						});
					return(invisible(.out));
				}

				suppressMessages({
					if (missing(wf)){ wf <- self$current }
					if (rlang::is_empty(wf) || identical(list(), wf)){
						if (!interactive()){
							message("Argument 'wf' must be provided when running in a non-interactive session: exiting ...");
							return()
						}
						wf <- self$workflows[[
							tcltk::tk_select.list(.workflows, title = "Choose a workflow to search:")]];
						if (rlang::is_empty(wf)){
							message("No workflow selected: exiting ...");
							return(invisible(0))
						}
					}
					if (missing(wf_step)){
						wf_step <- if (!passive){
							wf %$% mget(tcltk::tk_select.list(
								ls()
								, preselect = ls()
								, title = "Choose the workflow steps to execute in order"
								, multiple = TRUE
							))
						} else { wf %$% mget(ls()) }
					} else {
						wf_step <- wf %$% mget(ls(pattern = as.character(rlang::exprs(!!!wf_step)) %>%
																				sprintf(fmt = "(%s)") %>% paste(collapse = "|")))
					}
				})

				invisible(purrr::map(wf_step, eval, envir = globalenv()));
				invisible(self)
			},
			#' Reset the Workflow Manager
			#'
			#' \code{$reset} will clear out '$workflows' and set '$current' to \code{NULL}
			#'
			#' @param confirm (logical | TRUE) Set to \code{FALSE} if you're feeling lucky -- punk!
			#'
			#' @return Invisibly, the class environment
			reset = function(confirm = TRUE){
				if (confirm){
					.clean_out = function(){ rm(list = ls(envir = self$workflows), envir = self$workflows); self$current <- NULL; }
					if (askYesNo("Clean out the manager?") == TRUE){ .clean_out() } else { message("No action taken ..."); }} else { .clean_out()  }
				invisible(self);
			}
		)
	, private = list(version = 1.0, curnt = NULL)
	, active = list(
			#' @field available Show Available workflow Sets\cr\code{$available} is an \code{\link[R6]{R6Class}} active binding that prints the objects in \code{$workflows}, indicating which one is the current workflow (use \code{$help(get) for related information})
			available = function(){
				c("Available workflow sets are as follows:"
					, mget(ls(self$workflows), envir = self$workflows) %>%
						purrr::imap_chr(~ifelse(identical(self$current, .x), paste0("*** ", .y), .y))
					, "\n*** indicates the current set (see '{workflow_manager}$help(get)')"
				) %>% cat(sep = "\n  - ")
				invisible()
			},
			#' @field current Show or set the current workflow
			current = function(i = NULL){
				if (rlang::is_empty(i)){ i <- private[["curnt"]] } else { private[["curnt"]] <- i }
				if (rlang::is_empty(i)){
					message("No workflows exist!")
					return(invisible())
				} else {
					message(sprintf("'%s' is the current workflow set", i))
					self$workflows[[i]]
				}
			}
			)
	)
}