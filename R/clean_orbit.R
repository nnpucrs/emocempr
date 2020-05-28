#' Takes a emocemp orbit mri .csv file and clean it
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @param datafile File extracted from qualtrics in .csv format with the numeric option
#' @return a data frame with the input brain mri data cleaned
#' @author R.C.S
#' @export


clean_orbit <- function(datafile) {
        mridata <- read.csv(datafile, encoding = "UTF-8",
                            skip = 1)
        mridata <- mridata[-c(1),-c(1:17)]
        #--------------------------------------------------------------#
        # Rename
        names <- c("avaliador",
                   "data_exame",
                   "id_participante",
                   "rm_orbita",
                   "campo_mag",
                   "campo_mag_text",
                   "sequencias",
                   "lesao_nervo_o",
                   "lesao_nervo_o_text",
                   "local_lesao_t2_od",
                   "local_lesao_t2_oe",
                   "local_realce_od",
                   "local_realce_oe",
                   "alteracoes_orbita",
                   "outras_alteracoes_coments")

        colnames(mridata) <- names
        #--------------------------------------------------------------#
        # Factor magnetic field
        mridata$campo_mag <- factor(mridata$campo_mag)
        if (length(levels(mridata$campo_mag)) == 1){
                levels(mridata$campo_mag) <- 1.5
        } else if (length(levels(mridata$campo_mag)) == 2) {
                levels(mridata$campo_mag) <- c("1.5","3")
        } else {
                levels(mridata$campo_mag) <- c("1.5","3","< 1.5")
                }
        #--------------------------------------------------------------#
        # Dumming sequences
        name_seq <- c("stir_t2_axial",
                      "stir_t2_coronal",
                      "fatsat_t1_pos_axial",
                      "fatsat_t1_pos_coronal",
                      "axial_t2_brain",
                      "coronal_t2_brain",
                      "t1_pos")

        df_seq <- split_mcv("sequencias","7",mridata)
        ## Correct for qualtrics indexes
        if (any(df_seq == 8)) {df_seq[df_seq == 8] <- 7}
        ## Continue
        df_seq <- mutate_mcv(name_seq,df_seq)
        ## Exclude and bind
        mridata <- mridata[, names(mridata) != "sequencias"]
        mridata <- cbind(mridata, df_seq)
        #--------------------------------------------------------------#
        # Clean optic nerve lesion
        name_lesion <- c("presente",
                         "ausente",
                         "comentarios")

        df_lesion <- split_mcv("lesao_nervo_o","3",mridata)
        df_lesion <- mutate_mcv(name_lesion,df_lesion)
        df_lesion <- as.data.frame(df_lesion)
        df_lesion <- df_lesion %>%
                mutate(lesoes_no = ifelse(ausente == 1,0,1))%>%
                select(lesoes_no,comentarios)
        df_lesion$lesoes_no <- as.factor(df_lesion$lesoes_no)
        levels(df_lesion$lesoes_no) <- c("ausentes","presentes")
        ## Exclude and bind
        mridata <- mridata[, names(mridata) != "lesao_nervo_o"]
        mridata <- cbind(mridata, df_lesion)
        #--------------------------------------------------------------#
        # Clean lesion localization OE
        name_local_oe <- c("retrobulbar_oe",
                           "porcao_media_oe",
                           "posterior_oe",
                           "sem_alt_oe")

        df_local_oe <- split_mcv("local_lesao_t2_oe","4",mridata)
        df_local_oe <- mutate_mcv(name_local_oe, df_local_oe)
        ## Exclude and bind
        mridata <- mridata[, names(mridata) != "local_lesao_t2_oe"]
        mridata <- cbind(mridata, df_local_oe)
        #--------------------------------------------------------------#
        # Clean lesion location OD
        name_local_od <- c("retrobulbar_od",
                           "porcao_media_od",
                           "posterior_od",
                           "sem_alt_od")

        df_local_od <- split_mcv("local_lesao_t2_od","4",mridata)
        df_local_od <- mutate_mcv(name_local_od, df_local_od)
        ## Exclude and bind
        mridata <- mridata[, names(mridata) != "local_lesao_t2_od"]
        mridata <- cbind(mridata, df_local_od)
        #--------------------------------------------------------------#
        # Clean contrast enhancement location OE
        name_realce_oe <- c("realce_retrobulbar_oe",
                           "realce_porcao_media_oe",
                           "realce_posterior_oe",
                           "realce_sem_alt_oe")

        df_realce_oe <- split_mcv("local_realce_oe","4",mridata)
        df_realce_oe <- mutate_mcv(name_realce_oe, df_realce_oe)
        ## Exclude and bind
        mridata <- mridata[, names(mridata) != "local_realce_oe"]
        mridata <- cbind(mridata, df_realce_oe)
        #--------------------------------------------------------------#
        # Clean contrast enhancement location OD
        name_realce_od <- c("realce_retrobulbar_od",
                            "realce_porcao_media_od",
                            "realce_posterior_od",
                            "realce_sem_alt_od")

        df_realce_od <- split_mcv("local_realce_od","4",mridata)
        df_realce_od <- mutate_mcv(name_realce_od, df_realce_od)
        ## Exclude and bind
        mridata <- mridata[, names(mridata) != "local_realce_od"]
        mridata <- cbind(mridata, df_realce_od)
        #--------------------------------------------------------------#
        # Clean general orbit alterations
        name_alt <- c("hipersinal_quiasma",
                      "hipersinal_mais_2/3",
                      "realce_quiasma",
                      "realce_mais_2/3",
                      "realce_perineural",
                      "nenhuma_das_alt_acima")

        df_genalt <- split_mcv("alteracoes_orbita","6", mridata)
        ## Correct inconsistencies
        df_genalt[df_genalt == 7] <- 5
        df_genalt[df_genalt == 9] <- 6
        ## Continue
        df_genalt <- mutate_mcv(name_alt, df_genalt)
        ## Exclude and bind
        mridata <- mridata[, names(mridata) != "alteracoes_orbita"]
        mridata <- cbind(mridata, df_genalt)
        #--------------------------------------------------------------#
        orbit_dataset <- mridata

return(orbit_dataset)

}
