# install_packages.R - Install all required packages for AI Health Atlas
message("Installing required R packages for AI Health Atlas...")

pkgs <- c(
  "shiny",
  "bslib",
  "shinyWidgets",
  "shinyjs",
  "leaflet",
  "plotly",
  "DT",
  "DBI",
  "RSQLite",
  "dplyr",
  "sf",
  "openssl",
  "testthat",
  "httr",
  "jsonlite",
  "writexl"
)

installed <- rownames(installed.packages())
to_install <- pkgs[!pkgs %in% installed]

if (length(to_install) > 0) {
  message(paste("Installing:", paste(to_install, collapse = ", ")))
  install.packages(to_install, repos = "https://cloud.r-project.org/", dependencies = TRUE)
} else {
  message("All packages already installed!")
}

message("Package installation complete. Ready to run AI Health Atlas.")
