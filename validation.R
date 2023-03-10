# library(magick)
# library(jsonlite)
# library(sodium)
# magick::image_read("book-305126_1280.png") %>% serialize(connection = NULL) %>% base64enc::base64encode() %>% writeClipboard()

sapply(dir("R", full.names = TRUE), source)

library(purrr);
library(stringi);
library(parallelly);
library(future);
library(foreach);
library(data.table)

# library(book.of.workflow)
#
# ~ do.copy_obj() ====
BLAH <- new.env()
BLEH <- new.env()

set_names(letters[1:10], LETTERS[1:10]) |> as.list() |> list2env(envir = globalenv())
set_names(letters[11:20], LETTERS[11:20]) |> as.list() |> list2env(envir = BLEH)
.pattern <- "^[A-Z]$"

do.copy_obj(A, keep.orig = TRUE, .debug = !TRUE);
	ls(pattern = .pattern)
	# [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"

do.copy_obj(B, C, D, keep.orig = TRUE, .debug = !TRUE);
	ls(pattern = .pattern)
	# [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"

do.copy_obj(B, C, D, to.env = BLAH, keep.orig = TRUE, .debug = !TRUE);
	ls(BLAH, pattern = .pattern)
	# [1] "B" "C" "D"

do.copy_obj(B, C, D, from.env = BLAH, to.env = BLEH, keep.orig = TRUE, .debug = !TRUE);
	ls(BLEH, pattern = .pattern)
	# [1] "B" "C" "D" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"

do.copy_obj(E, `F`, G, to.env = BLEH, keep.orig = TRUE, .debug = !TRUE);
	purrr::map(purrr::set_names(c("BLAH", "BLEH")), ~ls(pattern = .pattern, envir = eval(as.symbol(.x))));
	# $BLAH
	# [1] "B" "C" "D"
	#
	# $BLEH
	# [1] "B" "C" "D" "E" "F" "G" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"

do.copy_obj(A, B, C, D, E, `F`, G, to.env = c(BLEH, BLAH), keep.orig = TRUE, .debug = !TRUE);
	map(purrr::set_names(c("BLAH", "BLEH")), ~ls(pattern = .pattern, envir = eval(as.symbol(.x))));
	# $BLAH
	# [1] "A" "B" "C" "D" "E" "F" "G"
	#
	# $BLEH
	# [1] "A" "B" "C" "D" "E" "F" "G" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"

do.copy_obj(!!!LETTERS[1:7], to.env = c(BLEH, BLAH), keep.orig = FALSE, .debug = !TRUE);
	map(purrr::set_names(c("BLAH", "BLEH", ".GlobalEnv")), ~ls(pattern = .pattern, envir = eval(as.symbol(.x))));
	# $BLAH
	# [1] "A" "B" "C" "D" "E" "F" "G"
	#
	# $BLEH
	# [1] "A" "B" "C" "D" "E" "F" "G" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
	#
	# $.GlobalEnv
	# [1] "H" "I" "J"

do.copy_obj(`BLAH$alpha` = H, `BLAH$beta` = I, `BLEH$gamma` = J, keep.orig = TRUE, .debug = !TRUE);
	.pattern <- "^[A-Z]$|(alpha|beta|gamma)"
	map(purrr::set_names(c("BLAH", "BLEH", ".GlobalEnv")), ~ls(pattern = paste0(.pattern, "|(^a-z)"), envir = eval(as.symbol(.x))));
	# BLAH
	# [1] "A" "alpha" "B" "beta" "C" "D" "E" "F" "G"
	#
	# $BLEH
	# [1] "A" "B" "C" "D" "E" "F" "G" "gamma" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
	#
	# $.GlobalEnv
	# [1] "H" "I" "J"

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

# ~ do.save.image():: Only check the prompt to ensure the expected objects and environment return ====
# debug(do.save_image)
do.save_image();
do.save_image(!!!ls())
do.save_image(!!!LETTERS[1:6], env = BLEH)

# undebug(do.save_image)

# pkgdown ----
pkgdown::build_site()
# pkgdown::build_site_github_pages()