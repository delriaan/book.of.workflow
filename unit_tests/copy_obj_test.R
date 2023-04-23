list(
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
	, test_9 = {
			copy_obj(EEEE = A, to_env = .GlobalEnv, keep.orig = !FALSE)
			copy_obj(DDDD = A, to_env = c(.GlobalEnv, BLAH), keep.orig = FALSE)
			copy_obj(zz = EEEE, to_env = c(.GlobalEnv, BLAH), keep.orig = !FALSE)
			copy_obj(WHY.WOULD.USE.USE.THIS.AS.AN.OBJECT.NAME = B, to_env = c(BLEH, BLAH), keep.orig = !FALSE)
			list(
				BLAH = rlang::env_has(BLAH, c("WHY.WOULD.USE.USE.THIS.AS.AN.OBJECT.NAME", "DDDD")) |> print()
				, BLEH = rlang::env_has(BLEH, "WHY.WOULD.USE.USE.THIS.AS.AN.OBJECT.NAME") |> print()
				, `.GlobalEnv` = !rlang::env_has(.GlobalEnv, c("H", "I", "J")) |> print()
				)
		}
	)