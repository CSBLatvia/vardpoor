# Package test, check, build & publish to CRAN
options(encoding = "UTF-8")

# Package name
package_name <- "vardpoor"

# Extract version number from DESCRIPTION
description <- readLines(file.path(package_name, "DESCRIPTION"))
ver <- gsub("^.* ", "", grep("Version", description, value = T))
cat(ver)
rm(description)

# Style
# styler::style_pkg(pkg = package_name, dry = "on")

# Run tests from the test folder
devtools::test(package_name)

# Generate documentation
devtools::document(package_name, roclets = c("rd", "collate", "namespace"))

# # Spell Check
# devtools::spell_check(package_name)


# Check localy with devtools
# Light check
devtools::check(package_name, cran = FALSE)
# Extended check (testing all examples)
devtools::check(package_name, manual = TRUE, cran = TRUE, remote = TRUE,
                run_dont_test = TRUE, args = "--run-dontrun")


# Build source package
devtools::build(package_name)

# # Check localy wit R CMD
# system(paste0("R CMD check --as-cran ",
#               package_name, "_", ver, ".tar.gz"))


# # revdepcheck
# remotes::install_github("r-lib/revdepcheck")


# Build binary package
devtools::build(package_name, binary = TRUE, args = c('--preclean'))

# Build manual
# file.copy(from = paste0(package_name, ".Rcheck/", package_name, "-manual.pdf"),
#           to = paste0(package_name, "_", ver, ".pdf"), overwrite = TRUE)
devtools::build_manual(package_name)


# MD5
md5sums <- tools::md5sum(list.files(pattern = "zip$|tar.gz$|pdf$"))
data.table::fwrite(x = data.frame(md5 = md5sums, filename = names(md5sums)),
                   file = paste0(package_name, "_", ver, ".md5"),
                   sep = " ", col.names = FALSE)


# Unload if loaded
detach("package:vardpoor", unload = TRUE)

# Install source package
install.packages(paste0(package_name, "_", ver, ".tar.gz"), repos = NULL)

# Install Windows binary package
if (.Platform$OS.type == "windows") {
  install.packages(paste0(package_name, "_", ver, ".zip"), repos = NULL)
}

# Load package
library(package_name, character.only = TRUE)



# Remote checks
# Use only if local check are OK!!!

# Building and checking R source packages for Windows
# https://win-builder.r-project.org/
# devtools::check_win_oldrelease(package_name) # R previous release
devtools::check_win_release(package_name)    # R current release
devtools::check_win_devel(package_name)      # R devel version

# R-hub builder
# https://builder.r-hub.io/
devtools::check_rhub(package_name, email = "martins.liberts@csp.gov.lv")



# Publish to CRAN
# https://cran.r-project.org/web/packages/policies.html
# Do this only of all tests and checks are OK
devtools::release(package_name)
