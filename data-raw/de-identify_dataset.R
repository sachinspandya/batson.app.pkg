# De-identify historical strike data, including by replacing atty names with aliases

library(dplyr)

# load cleaned version of original dataset 

dat0  <- readRDS("~/Research/projects.active/batson_app/app/jury_data_cleaned_new.rds")

## Prep to replace real attorney names with fake attorney names

p_atty <- unlist(dat0$P_atty_l) |>
  stringr::str_trim() |>
  unique()

d_atty <- unlist(dat0$D_atty_l) |>
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

dat0 <- dat0 |>
  mutate(P_atty_l_fake = P_atty_l,
         D_atty_l_fake = D_atty_l)

for ( i in 1:nrow(dat0)){
  dat0$P_atty_l_fake[[i]]<- 
    ifelse(str_trim(dat0$P_atty_l[[i]]) %in% atty_name$atty_real, 
       atty_name[atty_name$atty_real %in% str_trim(dat0$P_atty_l[[i]]),"atty_fake"], 
       dat0$P_atty_l_fake[[i]])
}

for ( i in 1:nrow(dat0)){
  dat0$D_atty_l_fake[[i]]<- 
    ifelse(str_trim(dat0$D_atty_l[[i]]) %in% atty_name$atty_real, 
           atty_name[atty_name$atty_real %in% str_trim(dat0$D_atty_l[[i]]),"atty_fake"], 
           dat0$D_atty_l_fake[[i]])
}

# replace real names with alias names
 
dat0 <- dat0 |>
  select(!c(P_atty_l, D_atty_l)) |>
  rename(P_atty_l = P_atty_l_fake, D_atty_l = D_atty_l_fake)

comment(dat0$P_atty_l) <- "alias names using charlatan pkg to de-identify attorneys"
comment(dat0$D_atty_l) <- "alias names using charlatan pkg to de-identify attorneys"

# remove other attorney-identifying characteristics, including case name, docket number, jury selection date

dat0 <- dat0 |>
        select(ID, R_No, Pool_Seq, race, hisp, sex, Disp, strike_num, strike_seq,
               alternate, courthouse, juror, ID, raceb, P_atty_l, D_atty_l)

# add to package as exported data

usethis::use_data(dat0, overwrite = TRUE)

