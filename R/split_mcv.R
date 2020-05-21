#' Takes a variable with multiple numeric variables and split into a data.frame with unique values in each col
#'
#' @author R.C.S
#' @param var Takes a string variable from a data frame with 'n' possible values
#' @param n a string with an integer which is the maximum number of possible values in a individual subject
#' @return a data frame
#' @export

split_mcv <- function (var,n) {
  charc <- as.character(emocemp[[var]])
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


mutate_mcv <- function (df, vec, n) {
  for (i in vec) {
    new_df <- df %>%
      mutate(i = ifelse(df[,c]))

  }

}
