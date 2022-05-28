library(readr)

NISPUF14_VARS <- data.frame(key = names(NISPUF14), description = label(NISPUF14))

saveRDS(NISPUF14_VARS, "data/NISPUF14_VARS.RDS")
