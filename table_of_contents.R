# Read the source directory and name the paths of source files as titles
# The titles will show up in package documentation
pkg_desc <- desc::desc(file = "pkg/DESCRIPTION")
pkg_name <- pkg_desc$get_field("Package")
pkg_title <- pkg_desc$get_field("Title")
pkg_file <- glue::glue("pkg/R/{pkg_name}.R")
toc_file <- "pkg/README.md"

#
# Generation ----
pkg_doc_prep <- dir("pkg/R", pattern = "R$", recursive = TRUE, full.names = TRUE) |>
	(\(x){
		i <- which(grepl("miscell|other", x));
		if (rlang::is_empty(i)){ x } else { c(x[-i], x[i]) }
	})() |>
	purrr::discard(\(x) grepl(pkg_name, x)) |>
	purrr::map(\(x){
		list(path = x, title = { stringi::stri_replace_all_regex(x, c("pkg/R/", "[.]R$", "[_]"), c("", "", " "), vectorise_all = FALSE) |>
				stringi::stri_trans_totitle()
			})
	})

#
pkg_template <- c("#' \\itemize{", "%s", "#' }") |> paste(collapse = "\n")
pkg_doc_content <- c()
toc_content <- c()

# Populate content
pkg_doc_prep |> purrr::iwalk(\(x, y){
	env <- new.env();

	source(x$path, local = env)

	pkg_doc_content <<- pkg_doc_content |>
		c(paste(
			glue::glue("#' @section Chapter {y} - {x$title}: ")
			, sprintf("#' \\item{\\code{\\link{%s}}}", ls(env)) |>
					stringi::stri_replace_all_fixed("%", "\\%", vectorise_all = TRUE) |>
					paste(collapse = "\n") |>
					sprintf(fmt = pkg_template)
			, sep = "\n"
			)) |> unique()

		toc_content <<- toc_content |>
			c(paste(
				glue::glue("## Chapter {y} - {x$title}\n\n")
				, sprintf("- %s", ls(env)) |> paste(collapse = "\n")
				, sep = "\n"
				)) |> unique()
	})

#
pkg_doc_header <- { paste(
	glue::glue("#' @title {pkg_title}")
	, "#"
	, "#' @description"
	, glue::glue("#' The following functional chapters are covered in \\code{{{pkg_name}}}:\\cr")
	, sep = "\n"
	)}

#
toc_header <- { paste(
	glue::glue("# ![book](book_small.png) {pkg_title}\n\n")
	, glue::glue("The following functional families are covered in {pkg_name}:")
	, sep = "\n"
	)}

#
pkg_doc_footer <- { paste(
	"#'"
	, "#' @importFrom magrittr %>% %T>% %<>% %$% freduce not"
	, "#' @importFrom data.table %between% %ilike% %like% rbindlist last setattr"
	, "#' @importFrom stringi %s+%"
	, "#' @importFrom utils askYesNo installed.packages"
	, glue::glue("#' @name {pkg_name}")
	, "NULL"
	, sep = "\n"
	)}

#
# Output ----
unlink(pkg_file)
unlink(toc_file)

cat(pkg_doc_header, pkg_doc_content, pkg_doc_footer, sep = "\n#'\n", file = pkg_file)
cat(toc_header, toc_content, sep = "\n\n", file = toc_file, append = FALSE)

#
# Build Site ----
# usethis::use_pkgdown()
pkgdown::build_site(pkg = "pkg", lazy = TRUE, override = list(destination = "../docs"))
