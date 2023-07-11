## code to prepare `df0` dataset goes here

df0 <- data.frame(round = c(1:10), 
                  num_cog = c(5, 5, 4, 3, 2, 2, 2, 2, 2, 2),
                  total = c(21,20,19,18,17,16,15,14,13,12), 
                  cog = c(0, 1, 1, 0, 0, 1, 1, 0, 0, 0),
                  party = rep(c("PP","PD"),5)
)


# add to package

#usethis::use_data(df, internal = TRUE, overwrite = TRUE)
