#' Creates a new data.frame (new_df) with appropriated variable names and values (dummies)
#'
#' Takes a vector of strings and a data frame, usually resulted from split_mcv()
#' @author R.C.S
#' @param names a vector of strings in the order of correspondent numeric values
#' @param df a data frame with numeric values associated with the desired factor levels
#' @return New data frame with the values distributed across subjects in the approriate factor level
#' @export

mutate_mcv <- function (names, df){
  # Aceita uma lista de strings (names) e um data.frame resultante de split.mcv

  # Tests
  if (length(names) != ncol(df) ){
    print("Names e colunas do Data Frame com tamanhos diferentes")
    break
  }
  if (!is.data.frame(df)){
    print("df precisa ser um Data Frame")
    break
  }

  # Create empty data.frame
  new_df <- NULL
  # Loop variables
  for (i in 1:length(names)) {
    vec <- c()
    # Loop trought rows
    for (r in 1:nrow(df)) {
      vec[r] <- any(df[r,] == i )}
    # Bind logical vector to new_df and name column
    vec <- as.numeric(vec)
    new_df <- cbind(vec, new_df)
    colnames(new_df)[1] <- names[i]
  }

  return(new_df)

}
