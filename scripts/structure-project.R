library(here)
library(fs)

dir_create(here("data", "raw"))
dir_create(here("data", "processed"))
dir_create(here(c(
  "figures",
  "output",
  "previous",
  "paper"
  )
  )
  )