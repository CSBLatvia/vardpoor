require(devtools)
require(tools)
options(encoding = "")

package_name <- "vardpoor"

description <- readLines(paste0(package_name, "/DESCRIPTION"))
ver <- gsub(" |:|[A-z]", "", grep("Version", description, value = T))
ver

# Documentation
devtools::document(package_name, roclets = c("rd", "collate", "namespace"))

devtools::build(package_name)

# Check (devtools)
devtools::check(package_name)

# Check (R CMD)
system(paste0("R CMD check ", package_name, "_", ver, ".tar.gz"))

# Build binary package
devtools::build(package_name, binary = TRUE, args = c('--preclean'))

# Copy manualR
file.copy(from = paste0(package_name, ".Rcheck/", package_name, "-manual.pdf"),
          to = paste0(package_name, "_", ver, "-manual.pdf"), overwrite = TRUE)

# MD5
md5sums <- md5sum(list.files(pattern = "zip$|tar.gz$|pdf$"))

df <- data.frame(md5 = md5sums, filename = names(md5sums))
write.table(df, file = paste0(package_name, "_", ver, "_checksums.md5"),
            sep = " ", row.names = F, col.names = F, quote = F)

# Install and load
detach("package:vardpoor", unload = TRUE)


# Installē bināro versiju
install.packages(paste0(package_name, "_", ver, ".zip"), repos = NULL)

# Installē no pirmkoda
install.packages(paste0(package_name, "_", ver, ".tar.gz"), repos = NULL)

require(vardpoor)

