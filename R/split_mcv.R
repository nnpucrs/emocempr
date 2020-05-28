#' Takes a variable with multiple numeric variables and split into a data.frame with unique values in each col
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @author R.C.S
#' @param var Takes a string variable from a data frame with 'n' possible values
#' @param n a string with an integer which is the maximum number of possible values in a individual subject
#' @param dataf Data frame from which the variable will be taken
#' @return a data frame
#' @export

split_mcv <- function (var,n,dataf) {
  charc <- as.character(dataf[[var]])
  split <- str_split_fixed(charc, ",", n)
  df <- as.data.frame(split)
  # Change to numeric
  ncol = as.numeric(n)
  for (i in 1:n){
    df[,i] <- as.numeric(df[,i])
    df[,i]
    df[,i][is.na(df[,i])] <- 0
  }
  return(df)
}


