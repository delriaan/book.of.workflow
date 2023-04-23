make.snippet(keyword, another_1)
read.snippet(keyword, another_1, action = parse);

make.snippet(keyword, another_2, use.clipboard = TRUE)
readClipboard()

make.snippet(keyword, another_3, include.read = FALSE, use.clipboard = FALSE)

read.snippet(key, another_3, action = save)
file.edit("key_another_3.snippet")

snippets_toc()