library(here)
library(fs)

dir_create(here("data", "raw"))
dir_create(here("data", "processed"))
dir_create(here(c(
  "scripts",
  "figures",
  "output",
  "previous",
  "paper",
  "notes",
  "documents"
)))