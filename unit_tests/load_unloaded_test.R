list(
	test_1 = {
			load_unloaded(purrr)
			"package:purrr" %in% search() |> print()
		}
	, test_2 = {
			load_unloaded(purrr, stringi, parallelly)
			paste0("package:", c("purrr", "stringi", "parallelly")) %in% search() |> print()
		}
	, test_3 = {
			load_unloaded("future?foreach?data.table", delim = "[?]")
			paste0("package:", c("future", "foreach", "data.table")) %in% search() |> print()
		}
	, test_4 = {
			load_unloaded("cachem{+cache_disk}")
			"package:cachem" %in% search() |> print()
			identical("cache_disk", ls(as.environment("package:cachem"))) |> print()
		}
	, test_5 = {
			load_unloaded("furrr{-future_imap}|arules", autoinstall = TRUE)
			paste0("package:", c("furrr", "arules")) %in% search() |> print()
			("future_imap" %in% ls(as.environment("package:furrr")) == FALSE) |> print()
		}
)