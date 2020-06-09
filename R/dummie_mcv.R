#' Combines mutate_mcv and split_mcv to clean datasets
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @author R.C.S
#' @param names a vector of strings with names in the order of correspondent numeric values
#' @param dataf a data frame with numeric values associated with the desired factor
#' @param var a variable from the database with multiple choice answers
#' @param n string containing the number of possible answers for the selected var
#' @return New data frame with the values distributed across subjects in the approriate factor level
#' @export

dummie_mcv <- function(names, var, n, dataf) {
        tmp_df <- split_mcv(var, n, dataf)
        dummed_df <- mutate_mcv(names, tmp_df)
        return(dummed_df)
}
