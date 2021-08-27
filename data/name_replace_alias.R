# Replace attorney names with alias names

library(dplyr)

load_path <- here::here("data","dat0.rda")
load(load_path)

dat0a <- dat0

## Prep to replace real attorney names with fake attorney names

p_atty <- unlist(dat0$P_atty_l) |>
  stringr::str_trim() |>
  unique()

d_atty <- unlist(dat0a$D_atty_l) |>
  stringr::str_trim() |>
  unique()

d_atty <- d_atty[! d_atty %in% c('pro se')] # remove "pro se" element from vector

## create dataframe with fake names for attorneys

atty_name <- unique(c(p_atty,d_atty)) |>
  data.frame() |>
  rename(atty_real = 1)

atty_name_fake <- charlatan::ch_name(n = nrow(atty_name), locale = "en_US")

atty_name$atty_fake <- atty_name_fake |>
  stringr::str_trim() |>
  stringr::str_remove("Mr. |Ms. |Mrs. |Miss |Dr. ") |>
  stringr::str_remove(" DDS| DVM| MD| PhD")

library(stringr)

# set up columns to fill

dat0a <- dat0a |>
  mutate(P_atty_l_fake = P_atty_l,
         D_atty_l_fake = D_atty_l)

for ( i in 1:nrow(dat0a)){
  dat0a$P_atty_l_fake[[i]]<- 
    ifelse(str_trim(dat0a$P_atty_l[[i]]) %in% atty_name$atty_real, 
       atty_name[atty_name$atty_real %in% str_trim(dat0a$P_atty_l[[i]]),"atty_fake"], 
       dat0a$P_atty_l_fake[[i]])
}

for ( i in 1:nrow(dat0a)){
  dat0a$D_atty_l_fake[[i]]<- 
    ifelse(str_trim(dat0a$D_atty_l[[i]]) %in% atty_name$atty_real, 
           atty_name[atty_name$atty_real %in% str_trim(dat0a$D_atty_l[[i]]),"atty_fake"], 
           dat0a$D_atty_l_fake[[i]])
}

# replace real names with alias names
 
dat0a <- dat0a |>
  select(!c(P_atty,D_atty, P_atty_l, D_atty_l)) |>
  rename(P_atty_l = P_atty_l_fake, D_atty_l = D_atty_l_fake)

comment(dat0a$P_atty_l) <- "alias names using charlatan pkg to de-identify attorneys"
comment(dat0a$D_atty_l) <- "alias names using charlatan pkg to de-identify attorneys"

save_path <- here::here("data","dat0.rda")
save(dat0a, file = save_path)

