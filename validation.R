# library(magick)
# library(jsonlite)
# library(sodium)
# magick::image_read("book-305126_1280.png") %>% serialize(connection = NULL) %>% base64enc::base64encode() %>% writeClipboard()

sapply(dir("pkg/R", full.names = TRUE), source)

library(purrr);
library(stringi);
library(parallelly);
library(future);
library(foreach);
library(data.table)

# library(book.of.workflow)
#

# ~ copy_obj() ====
unit_tests <- rlang::exprs(
	test_1 = {copy_obj(A, keep.orig = TRUE)
	identical(ls(pattern = .pattern), c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))}
	, test_2 = {copy_obj(B, C, D, keep.orig = TRUE);
	identical(ls(pattern = .pattern), c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))}
	, test_3 = {copy_obj(B, C, D, to_env = BLAH, keep.orig = TRUE);
	identical(ls(BLAH, pattern = .pattern) ,c("B", "C", "D"))
	}
	, test_4 = {copy_obj(B, C, D, from_env = BLAH, to_env = BLEH, keep.orig = TRUE);
	identical(ls(BLEH, pattern = .pattern), c("B", "C", "D", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"))}
	, test_5 = {copy_obj(E, `F`, G, to_env = BLEH, keep.orig = TRUE);
	identical(
		purrr::map(purrr::set_names(c("BLAH", "BLEH")), ~ls(pattern = .pattern, envir = eval(as.symbol(.x)))),
		list(BLAH = c("B", "C", "D")
		, BLEH = c("B", "C", "D", "E", "F", "G", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T")
		))}
	, test_6 = {copy_obj(A, B, C, D, E, `F`, G, to_env = c(BLEH, BLAH), keep.orig = TRUE);
	identical(
		map(purrr::set_names(c("BLAH", "BLEH")), ~ls(pattern = .pattern, envir = eval(as.symbol(.x)))),
		list(
			BLAH = c("A", "B", "C", "D", "E", "F", "G")
			, BLEH = c("A", "B", "C", "D", "E", "F", "G", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T")
			))}
	, test_7 = {copy_obj(!!!(LETTERS[1:7]), to_env = c(BLEH, BLAH), keep.orig = FALSE);
	identical(
		map(purrr::set_names(c("BLAH", "BLEH", ".GlobalEnv")), ~ls(pattern = .pattern, envir = eval(as.symbol(.x)))),
		list(
			BLAH = c("A", "B", "C", "D", "E", "F", "G")
			, BLEH = c("A", "B", "C", "D", "E", "F", "G", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T")
			, `.GlobalEnv` = c("H", "I", "J")
			))}
	, test_8 = {copy_obj(`BLAH$alpha` = H, `BLAH$beta` = I, `BLEH$gamma` = J, keep.orig = TRUE);
	.pattern <- "^[A-Z]$|(alpha|beta|gamma)"
	identical(
		map(purrr::set_names(c("BLAH", "BLEH", ".GlobalEnv")), ~ls(pattern = paste0(.pattern, "|(^a-z)"), envir = eval(as.symbol(.x)))),
		list(
			BLAH = c("A", "alpha", "B", "beta", "C", "D", "E", "F", "G")
			, BLEH = c("A", "B", "C", "D", "E", "F", "G", "gamma", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T")
			, `.GlobalEnv` = c("H", "I", "J")
			))}
	)
BLAH <- new.env()
BLEH <- new.env()

set_names(letters[1:10], LETTERS[1:10]) |> as.list() |> list2env(envir = globalenv())
set_names(letters[11:20], LETTERS[11:20]) |> as.list() |> list2env(envir = BLEH)
.pattern <- "^[A-Z]$"

undebug(copy_obj)

imap_lgl(unit_tests[7], ~{ message(.y); eval(.x, envir = globalenv()) })

# <snippet: keyword another> ----
read.snippet(keyword, another, action = parse);

# </snippet>
# ~ read.snippet(), make.snippet() ====
make.snippet(keyword, another, use.clipboard = TRUE)
make.snippet(keyword, another, include.read = TRUE, use.clipboard = FALSE)
# <snippet: keyword another> ----
read.snippet(keyword, another, action = parse);

# </snippet>
# undebug(read.snippet)
# <snippet: keyword another2> ----
read.snippet(keyword, another2, action = exec);
message("It works!")
# </snippet>
read.snippet(key, ano, 2, action = save);

# ~ save.image():: Only check the prompt to ensure the expected objects and environment return ====
# debug(save_image)
save_image();
save_image(!!!ls())
save_image(!!!LETTERS[1:6], env = BLEH)

# undebug(save_image)

# pkgdown ----
pkgdown::build_site()
# pkgdown::build_site_github_pages()