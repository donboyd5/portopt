
# use this program to load libraries and tools needed to develop portopt

library("devtools")
library("usethis")

source("./data-raw/programs/libraries.r")

library("portopt")

use_description()

# Run the following lines once, at the start of the package creation process:
usethis::use_build_ignore(c("data-raw"))
usethis::use_build_ignore(c("docs"))

usethis::use_package("tidyverse") # puts it in Imports of DESCRIPTION
usethis::use_package("scales")
usethis::use_package("Matrix")
usethis::use_package("quadprog")

use_pipe()

# if you are repeatedly using many functions from another package, you can import all of them using @import package.
document()

# if (!requireNamespace("pkg", quitely=TRUE)){
#   stop("Pkg needed for this function to work. Please install it.", call.=FALSE)
# }


# Namespace creation:
# 1. Add roxygen comments to your .R files.
# 2. Run devtools::document() (or press Ctrl/Cmd + Shift + D in RStudio) to convert roxygen comments to .Rd files.
# 3. Look at NAMESPACE and run tests to check that the specification is correct.
# 4. Rinse and repeat until the correct functions are exported.

# NAMESPACE also controls which external functions can be used by your package without having to use ::.
#
# It’s confusing that both DESCRIPTION (through the Imports field) and NAMESPACE (through import directives) seem to be involved in imports. This is just an unfortunate choice of names. The Imports field really has nothing to do with functions imported into the namespace: it just makes sure the package is installed when your package is. It doesn’t make functions available. You need to import functions in exactly the same way regardless of whether or not the package is attached.


