#' helpers 
#'
#' @description A fct function to generate dummy strike data and cog/atty-name choices
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Set global functions or variables to NULL

race <- sex <- party <- posterior <- theta <- q1 <- q2 <- strike_seq <- ID <- scaled <- NULL

# dummy data for strike tally

df0 <- data.frame(round = c(1:10), 
                  num_cog = c(3, 4, 4, 3, 2, 2, 2, 2, 2,2),
                  total = rep(9, 10), 
                  cog = c(0, 1, 1, 0, 0, 1, 1, 0, 0,0),
                  party = rep(c("PP","PD"),5)
)

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