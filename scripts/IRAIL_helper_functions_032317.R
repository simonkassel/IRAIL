# IRAIL PROJECT HELPER FUNCTIONS ------------------------------------------

# Simon Kassel


# FUNCTIONS ---------------------------------------------------------------

# checks for availability of packages, if not there it installs them and 
# then it attaches all packages
#   takes:
#     a vector of package names in quotation marks
#   returns:
#     nothing
#   side effects:
#     attaches packages
packages <- function(package_vector) {
  for (lib in package_vector) {
    if (!requireNamespace(lib, quietly = TRUE))
      install.packages(lib)
    suppressMessages(library(lib, character.only = TRUE))
    remove(lib)
  } 
}
