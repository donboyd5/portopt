
# use this program to load libraries and tools needed to develop portopt

library("devtools")
usethis::use_build_ignore(c("data-raw"))
usethis::use_build_ignore(c("docs"))

source("./data-raw/programs/libraries.r")

library("portopt")




