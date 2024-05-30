# make.snippet(keyword, another_1)
# <snippet: keyword another_1> ----
read.snippet(keyword, another_1, action = exec);
ls()
# </snippet>
# read.snippet(keyword, another_1, action = parse);
#
make.snippet(keyword, another_2, use.clipboard = TRUE)
readClipboard()
#
# make.snippet(keyword, another_3, include.read = FALSE, use.clipboard = FALSE)
# <snippet: keyword another_3> ----
message("Save test successful")
# </snippet>
#
snippets_toc(choose = TRUE);

read.snippet(key, another_3, action = save)
file.edit("key_another_3.snippet")
unlink("key_another_3.snippet")

