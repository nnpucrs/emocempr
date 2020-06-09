#' Takes a cleaned data frame from MRI study and calculate cohen kappa for selected varibles
#'@param datafile a data frame resulted from clean_brain or clean_orbit
#'@param varnames a string vector with variable names from the datasets
#'@return a list with kappa statistics for each selected variable
#'@export
#'@import dplyr
#'@import irr



cohen_kappa_mri <- function (datafile, varnames) {
        #-----------------------------------------------------------------#
        list_k <- list()
        for (i in varnames){
        testdata_BKC <- datafile %>%
                mutate(bkc = factor(datafile[[i]])) %>%
                select(id_participante,avaliador,bkc) %>%
                filter(avaliador == "BKC")
        testdata_RP <- datafile %>%
                mutate(rp = datafile[[i]]) %>%
                select(id_participante,avaliador,rp) %>%
                filter(avaliador == "RP")
        #-----------------------------------------------------------------#
        data4kappa <- merge(testdata_BKC,testdata_RP,by = "id_participante")
        data4kappa <- select(data4kappa, bkc, rp)
        #-----------------------------------------------------------------#
        list_k[[i]] <- kappa2(data4kappa)

        }
        return(list_k)
}
