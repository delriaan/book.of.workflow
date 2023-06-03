# library(magick)
# library(jsonlite)
# library(sodium)
# magick::image_read("book-305126_1280.png") %>% serialize(connection = NULL) %>% base64enc::base64encode() %>% writeClipboard()

sapply(dir("pkg/R", full.names = TRUE), source)

make_test_objs <- rlang::expr({
			BLAH <- new.env()
			BLEH <- new.env()

			rlang::set_names(letters[1:10], LETTERS[1:10]) |> as.list() |> list2env(envir = globalenv())
			rlang::set_names(letters[11:20], LETTERS[11:20]) |> as.list() |> list2env(envir = BLEH)
			.pattern <- "^[A-Z]$"
		})
make_test_objs |> eval()

unit_tests <- list()
unit_tests$copy_obj <- "unit_tests/copy_obj_test.R"
unit_tests$must_have <-"unit_tests/must_have_test.R"
unit_tests$snippets <-"unit_tests/snippets_test.R"
unit_tests$load_unloaded <-"unit_tests/load_unloaded_test.R"

# Unit Tests: copy_obj(), make.snippet(), read.snippet(), %must.have% ====
# library(book.of.workflow)
library(purrr);
library(stringi);
library(parallelly);
library(future);
library(foreach);
library(data.table);

(\(x, y = names(x)){ message(y); source(x[[1]]) })(unit_tests$copy_obj);
(\(x, y = names(x)){ message(y); source(x[[1]]) })(unit_tests$must_have);
(\(x, y = names(x)){ message(y); source(x[[1]]) })(unit_tests$snippets);

# ~ save.image():: Only check the prompt to ensure the expected objects and environment return ====
# debug(save_image)
save_image();
save_image(!!!ls());
save_image(!!!LETTERS[1:6], env = BLEH);
save_image(!!!LETTERS[1:6], env = BLEH, use.prefix = FALSE);
save_image(!!!LETTERS[1:6], env = BLEH, file.name = "all");
# undebug(save_image)

# ~ load_unloaded() :: Run from fresh session to ensure libraries are not loaded ----
(\(x, y = names(x)){ message(y); source(x[[1]]) })(unit_tests$load_unloaded);

#
#
# %+=%, %-=% ----
BLAH %+=% list(Z = 10)
rlang::env_has(BLAH, "Z") # TRUE

BLAH %+=% list(W = 30, Y = list(a = 1, b = 3))
rlang::env_has(BLAH, c("W", "Y")) # TRUE TRUE

rm(W, Y, Z, envir = BLAH)

BLAH %must.have% !!c("W", "Y", "Z")
`%must.have%`(BLAH)
check.env(BLAH) # Checking `BLAH`: FAIL: (missing W, Y, Z)

BLAH %+=% list(W = 30, Y = list(a = 1, b = 3), Z = 10)
check.env(BLAH) # Checking `BLAH`:

# undebug(`%-=%`)
# undebug(`%must.have%`)
BLAH %-=% c("W", "Y")
check.env(BLAH) # Checking `BLAH`: FAIL: (missing W, Y)

BLAH %must.have% !!c("-W", "-Y")
`%must.have%`(BLAH)

check.env(.GlobalEnv, BLAH, BLEH)
