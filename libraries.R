# paquetes "usual" y "project" -----------------------------------
# las usual no se llaman explicitamente en los Scripts
# las project si (se cargan o se usa ::)
lib_usual <- c("magrittr",
               "purrr",
               # "here",
               "conflicted",
               # "ggplot2",
               "dplyr",
               "stringr",
               "lubridate")
lib_project <- c("RCurl",
                 "tidyr",
                 "xlsx")

# not modify  ----------------------------------------------------------------
lib <- c(lib_usual, lib_project)
for (i in seq_along(lib)) {
  if (!(lib[i] %in% installed.packages()[,1])) {
    suppressMessages(install.packages(lib[i]))
  }
  if (lib[i] %in% lib_usual) {
    suppressMessages(library(lib[i],character.only=T))
  }
}
rm(lib, lib_usual, lib_project)

