## code to prep sysdata.rda goes here

# Set global functions or variables to NULL

race <- sex <- party <- posterior <- theta <- q1 <- q2 <- strike_seq <- ID <- ..scaled.. <- NULL

party_choices <- c("PP","PD")

# Define Cognitive Class choices

cog_choices <- c(0,1)
cog_c_levels <- c("race","gender")

#### attorney name choices ####

load(file = "data/dat0.rda")

atty_levels_p <- dat0 |>
  dplyr::pull("P_atty_l") |>
  unlist() |>
  unique() |>
  stringr::str_trim() |>
  factor()

atty_levels_d <- dat0 |>
  dplyr::pull("D_atty_l") |>
  unlist() |>
  unique() |>
  stringr::str_trim() |>
  factor()

atty_levels_p <- c("None",levels(atty_levels_p))
atty_levels_d <- c("None",levels(atty_levels_d))

# add to package as R objects for system use

usethis::use_data(df0, cog_c_levels, atty_levels_d, atty_levels_p, cog_choices, cog_c_levels,
party_choices, race, sex, strike_seq, theta, q1, q2, party, ID, posterior, internal = TRUE, overwrite = TRUE)
