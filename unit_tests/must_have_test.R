eval(make_test_objs)
attr(.GlobalEnv, "must.have") <- NULL

list(
	test_1 = {
			.GlobalEnv %must.have% "A"
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