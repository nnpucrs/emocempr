#' Takes a variable with multiple numeric variables and split into a data.frame with unique values in each col
#' @import dplyr
#' @import stringr
#' @importFrom utils read.csv
#' @author R.C.S
#' @param var Takes a string variable from a data frame with 'n' possible values
#' @param n Integer with the maximum number of possible values in a individual subject
#' @param dataf Data frame from which the variable will be taken
#' @return a data frame
#' @export



split_mcv <- function (var,n,dataf) {
  # Check if n is integer
  if (!is.numeric(n)){
    return(print("Por favor insira um valor valido para n"))
  }
  if (!is.wholenumber(n)) {
    return(print("Por favor insira um valor valido para n"))
    }

  # Main
  charc <- as.character(dataf[[var]])
  split <- str_split_fixed(charc, ",", as.character(n))
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


