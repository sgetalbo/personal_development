
# Libraries and data ------------------------------------------------------

library(tidyverse)

#AHAH <- read.csv("./allvariableslsoawdeciles.csv")

# Function 1 --------------------------------------------------------------

# One specified column in one dataframe 

calculate_mean <- function(df) {
 new <- mean(df$gpp_dist)
 return(new)
}

func1 <- calculate_mean(AHAH)

# Function 2a --------------------------------------------------------------

# One defined column in one dataframe

calc_mean_col <- function(df, i) {
  return(df[,i] %>% mean())
}

func2 <- calc_mean_col(AHAH, 3)

# Function 2b --------------------------------------------------------------

# Multiple columns using a for loop

col_outputs <- array() # define an empty array outside the function - this is when you have the square brackets!

k = 1 # you need to define k if your columns don't start at 1

## Example 1 
for (i in 3:6) {
  col_outputs[k] <- calc_mean_col(AHAH, i)
  k = k+1 
}

## Example 2
for (i in 1:5) {
  col_outputs[i] <- calc_mean_col(AHAH, i)
  k = k+1 
}

## Example 3
# This will give two NA columns before doing anything 
for (i in 3:6) {
  col_outputs[i] <- calc_mean_col(AHAH, i)
}

## Example 4 
col_outputs2 <- NULL # define an empty

for (i in 3:6) {
  col_outputs2 <- cbind(col_outputs2, calc_mean_col(AHAH, i))
}


# Function 3 --------------------------------------------------------------

# using apply to iterate over multiple columns
# this is instead of a for loop!! 

calc_mean_apply <- function(col) {
  return(col %>% mean())
}

# col wise
outputs <- apply(AHAH[,3:6], 2, calc_mean_apply)
# row wise 
outputs <- apply(AHAH[,3:6], 1, calc_mean_apply)


# Function 4 --------------------------------------------------------------

# using apply to iterate over multiple columns and add 10 

calc_mean_apply_plus <- function(col, val) {
  return(col %>% mean() + val)
}

# col wise
outputs <- apply(AHAH[,3:6], 2, calc_mean_apply_plus, val = 10)
# rowwise 
outputs <- apply(AHAH[,3:6], 1, calc_mean_apply_plus, val = 10)

# How would I state do it once with val = 10 and then again with val = 15? 

calc_mean_apply_plus2 <- function(col, val, val2) {
  return(list(mean1 = (col %>% mean() + val), # this list could also be set as a dataframe.
         mean2 = (col %>% mean() + val2)))
}

test_myself <- apply(AHAH[,3:6], 2, calc_mean_apply_plus2, val = 10, val2 = 15)

# Function 6 --------------------------------------------------------------

# return multiple stats for multiple columns

calc_summary_apply <- function(col) {
  return(list(mean1 = (col %>% mean()),
              var1 = (col %>%  var())))
}

outputs_summary <- apply(AHAH[,3:6], 2, calc_summary_apply)

#example 2
calc_summary_apply_df <- function(col) {
  return(data.frame(mean1 = (col %>% mean()),
                    var1 = (col %>%  var())))
}

tester <- calc_summary_apply_df(AHAH[,3])

#example 3 
col_outputs2 <- NULL

for (i in 3:6) {
  col_outputs2 <- rbind(col_outputs2, calc_summary_apply_df(AHAH[,i]))
}
