# library(magick)
# library(jsonlite)
# library(sodium)
# magick::image_read("book-305126_1280.png") %>% serialize(connection = NULL) %>% base64enc::base64encode() %>% writeClipboard()
make_test_objs <- rlang::expr({
			BLAH <- new.env()
			BLEH <- new.env()

			rlang::set_names(letters[1:10], LETTERS[1:10]) |> as.list() |> list2env(envir = globalenv())
			rlang::set_names(letters[11:20], LETTERS[11:20]) |> as.list() |> list2env(envir = BLEH)
			.pattern <- "^[A-Z]$"
		})
unit_tests <- list()
# sapply(dir("pkg/R", full.names = TRUE), source)

# library(book.of.workflow)
library(purrr);
library(stringi);
library(parallelly);
library(future);
library(foreach);
library(data.table)
#
# ~ copy_obj() ====
unit_tests$copy_obj <- rlang::exprs(
		test_1 = {
				copy_obj(A, keep.orig = TRUE);
				list(`.GlobalEnv` = rlang::env_has(.GlobalEnv, c("A")));
			}
		, test_2 = {
				copy_obj(B, C, D, keep.orig = TRUE);
				list(`.GlobalEnv` = rlang::env_has(.GlobalEnv, c("B", "C", "D")));
			}
		, test_3 = {
				copy_obj(B, C, D, to_env = BLAH, keep.orig = TRUE);
				list(BLAH = rlang::env_has(env = BLAH, nms = c("B", "C", "D")));
			}
		, test_4 = {
				copy_obj(B, C, D, from_env = BLAH, to_env = BLEH, keep.orig = TRUE);
				list(BLEH = rlang::env_has(env = BLEH, nms = c("B", "C", "D")));
			}
		, test_5 = {
				copy_obj(E, `F`, G, to_env = BLEH, keep.orig = TRUE);
				list(BLEH = rlang::env_has(BLEH, c("E", "F", "G")))
			}
		, test_6 = {
				copy_obj(A, B, C, D, E, `F`, G, to_env = c(BLEH, BLAH), keep.orig = TRUE);
				list(
					BLAH = rlang::env_has(BLAH, c("A", "B", "C", "D", "E", "F", "G"))
					, BLEH = rlang::env_has(BLEH, c("A", "B", "C", "D", "E", "F", "G"))
					)
			}
		, test_7 = {
				copy_obj(!!!(LETTERS[1:7]), to_env = c(BLEH, BLAH), keep.orig = FALSE);
				list(
					BLAH = rlang::env_has(BLAH, LETTERS[1:7])
					, BLEH = rlang::env_has(BLEH, LETTERS[1:7])
					, `.GlobalEnv` = rlang::env_has(.GlobalEnv, c("H", "I", "J"))
					)
			}
		, test_8 = {
				copy_obj(`BLAH$alpha` = H, `BLAH$beta` = I, `BLEH$gamma` = J, keep.orig = !TRUE);
				list(
					BLAH = rlang::env_has(BLAH, c("alpha", "beta")) |> print()
					, BLEH = rlang::env_has(BLEH, c("gamma")) |> print()
					, `.GlobalEnv` = !rlang::env_has(.GlobalEnv, c("H", "I", "J")) |> print()
					)
			}
		);

attr(unit_tests, "init") |> eval()
purrr::imap(unit_tests$copy_obj[], ~{ message(.y); eval(.x, envir = globalenv()) })

# debug(copy_obj)
copy_obj(EEEE = A, to_env = .GlobalEnv, keep.orig = !FALSE)
copy_obj(DDDD = A, to_env = c(.GlobalEnv, BLAH), keep.orig = FALSE)
copy_obj(zz = EEEE, to_env = c(.GlobalEnv, BLAH), keep.orig = !FALSE)
copy_obj(WHY.WOULD.USE.USE.THIS.AS.AN.OBJECT.NAME = B, to_env = c(BLEH, BLAH), keep.orig = !FALSE)
# imap(unit_tests$copy_obj[8], ~{ message(.y); eval(.x, envir = globalenv())})
# undebug(copy_obj)

# ~ read.snippet(), make.snippet() ====
make.snippet(keyword, another_1)
# <snippet: keyword another_1> ----
read.snippet(keyword, another_1, action = parse);

# </snippet>
make.snippet(keyword, another_2, use.clipboard = TRUE)
make.snippet(keyword, another_3, include.read = FALSE, use.clipboard = FALSE)
# <snippet: keyword another_3> ----
TRUE
# </snippet>
read.snippet(key, another_3, action = save)
file.edit("key_another_3.snippet")

snippets_toc()
# ~ save.image():: Only check the prompt to ensure the expected objects and environment return ====
# debug(save_image)
save_image();
save_image(!!!ls())
save_image(!!!LETTERS[1:6], env = BLEH)
save_image(!!!LETTERS[1:6], env = BLEH, use.prefix = FALSE)
save_image(!!!LETTERS[1:6], env = BLEH, file.name = "all")
# undebug(save_image)

# ~ load_unloaded() :: Run from fresh session to ensure libraries are not loaded ----
sapply(dir("pkg/R", full.names = TRUE, pattern = "^1_"), source)

load_unloaded(purrr)
"package:purrr" %in% print(search())

load_unloaded(purrr, stringi, parallelly)
paste0("package:", c("purrr", "stringi", "parallelly")) %in% print(search())

load_unloaded("future?foreach?data.table", delim = "[?]")
paste0("package:", c("future", "foreach", "data.table")) %in% print(search())

# debug(load_unloaded)
load_unloaded("cachem{+cache_disk}")
"package:cachem" %in% print(search())
identical("cache_disk", ls(as.environment("package:cachem")))
# undebug(load_unloaded)

# debug(load_unloaded)
load_unloaded("furrr{-future_imap}|arules", autoinstall = TRUE)
paste0("package:", c("furrr", "arules")) %in% print(search())
"future_imap" %in% ls(as.environment("package:furrr"))
# undebug(load_unloaded)


# ~ %must.have% ----
eval(make_test_objs)
attr(.GlobalEnv, "must.have") <- NULL
`%must.have%`(.GlobalEnv) # NULL
# debug(`%must.have%`)
# undebug(`%must.have%`)

unit_tests$must_have <- rlang::exprs(
		test_1 = {
				environment() %must.have% A
				`%must.have%`(.GlobalEnv) # NULL
				copy_obj(A, keep.orig = TRUE);
				check.env(.GlobalEnv)
			}
		, test_2 = {
				.GlobalEnv %must.have% !!c("B", "C", "D", `%must.have%`(.GlobalEnv))
				`%must.have%`(.GlobalEnv) 	# NULL
				copy_obj(B, C, D, keep.orig = TRUE)
				check.env(.GlobalEnv)
			}
		, test_3 = {
				BLAH %must.have% !!c("B", "C", "D")
				`%must.have%`(BLAH)
				copy_obj(B, C, D, to_env = BLAH, keep.orig = TRUE);
				check.env(BLAH)
			}
		, test_4 = {
				BLEH %must.have% !!c("B", "C", "D")
				`%must.have%`(BLEH)
				copy_obj(B, C, D, from_env = BLAH, to_env = BLEH, keep.orig = TRUE);
				check.env(BLEH)
			}
		, test_5 = {
				BLEH %must.have% !!c("E", "F", "G")
				`%must.have%`(BLEH)
				copy_obj(E, `F`, G, to_env = BLEH, keep.orig = TRUE);
				check.env(BLEH)
			}
		, test_6 = {
				BLEH %must.have% !!c("A", "B", "C", "D", "E", "F", "G")
				BLEH %must.have% !!c("A", "B", "C", "D", "E", "F", "G")
				`%must.have%`(BLAH)
				`%must.have%`(BLEH)
				copy_obj(A, B, C, D, E, `F`, G, to_env = c(BLEH, BLAH), keep.orig = TRUE);
				check.env(BLAH, BLEH)
			}
		)
eval(make_test_objs)
unit_tests$must_have$test_1 |> eval()
unit_tests$must_have$test_2 |> eval()
unit_tests$must_have$test_3 |> eval()
unit_tests$must_have$test_4 |> eval()
unit_tests$must_have$test_5 |> eval()
unit_tests$must_have$test_6 |> eval()
check.env(BLAH, BLEH)
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
check.env(BLAH) # Checking `BLAH`: PASS

# undebug(`%-=%`)
BLAH %-=% c("W", "Y")
check.env(BLAH) # Checking `BLAH`: FAIL: (missing W, Y)

check.env(.GlobalEnv, BLAH, BLEH)
#
# pkgdown ----
# pkgdown::build_site(pkg = "pkg", override = list(destination = "../docs"))
