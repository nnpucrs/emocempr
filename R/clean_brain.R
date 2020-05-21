#' Takes a emocemp brain mri .csv file and clean it
#'
#' @param datafile File extracted from qualtrics in .csv format with the numeric option
#' @author R.C.S
#' @export

clean_brain <- function (datafile){
        mridata <- read.csv(datafile, encoding = "UTF-8",
                            skip = 1)
        mridata <- mridata[-c(1),-c(1:17)]

        # Renaming
        names <- c("avaliador",
                   "data_exame",
                   "id_participante",
                   "rm_encefalo",
                   "campo_mag",
                   "campo_mag_text",
                   "sequencias",
                   "num_lesoes_t2",
                   "alteracoes_todas",
                   "lesoes_difusas_bilat",
                   "num_lesoes_peri",
                   "alteracoes_se_presente",
                   "periependimarias",
                   "comentarios",
                   "realce",
                   "tipo_realce",
                   "tipo_realce_outros")

        colnames(mridata) <- names

        # Creating proper levels
        mridata$campo_mag <- factor(mridata$campo_mag)
        if (length(levels(mridata$campo_mag)) == 2){
                levels(mridata$campo_mag) <- c("1.5","3")
                } else {
                        levels(mridata$campo_mag) <- c("1.5","3","<1.5")
                }
        # Dumming sequences
        name_seq <- c("axial_t2",
                      "sagital_t2",
                      "axial_flair",
                      "sagital_flair",
                      "axial_t1_pre",
                      "axial_t1_pos",
                      "flair_vol")

        df_seq <- split_mcv("sequencias","7",mridata)
        df_seq <- mutate_mcv(name_seq,df_seq)
        # Exclude and bind
        mridata <- mridata[, names(mridata) != "sequencias"]
        mridata <- cbind(mridata, df_seq)

        # Dumming num lesions
        name_num_lesion <- c("1-5 (lesoes t2)",
                             "5-9 (lesoes t2)",
                             "> 9 (lesoes t2)",
                             "Nenhuma lesao t2")

        df_numles <- split_mcv("num_lesoes_t2","4",mridata)
        df_numles <- mutate_mcv(name_num_lesion,df_numles)
        # Exclude and bind
        mridata <- mridata[, names(mridata) != "num_lesoes_t2"]
        mridata <- cbind(mridata, df_numles)

        # Dumming brain alterations
        name_alt <- c("1_perpendicular_corpocaloso",
                      "lesoes_periventriculares",
                      "apenas_bem_delimitadas",
                      "lesoes_difusas",
                      "nenhuma_das_alteracoes_descritas")

        df_alt <- split_mcv("alteracoes_todas","5",mridata)
        # Correct for qualtrics indexes
        df_alt[df_alt == 6] <- 4
        if (any(df_alt == 7)) {df_alt[df_alt == 6] <- 5}
        # Continue
        df_alt <- mutate_mcv(name_alt,df_alt)
        # Exclude and bind
        mridata <- mridata[, names(mridata) != "alteracoes_todas"]
        mridata <- cbind(mridata, df_alt)

        # Bilateral lesions
        mridata$lesoes_difusas_bilat <- factor(as.numeric(mridata$lesoes_difusas_bilat))
        levels(mridata$lesoes_difusas_bilat) <- c("bilaterais","unilaterais")

        # Num periventricular lesions
        mridata$num_lesoes_peri <- factor(mridata$num_lesoes_peri)
        if (length(levels(mridata$num_lesoes_peri)) == 3){
                levels(mridata$num_lesoes_peri) <- c("0","1",">3")
        } else {
                levels(mridata$num_lesoes_peri) <- c("0","1","2",">3")
        }

        # Dumming alterations 2
        name_alt2 <-c("1_ou_mais_justac",
                      "1_ou_mais_infrat",
                      "lesao_tronco",
                      "black_hole",
                      "nenhuma_das_alt2")

        df_alt2 <- split_mcv("alteracoes_se_presente","5",mridata)
        df_alt2 <- mutate_mcv(name_alt2,df_alt2)
        # Exclude and bind
        mridata <- mridata[, names(mridata) != "alteracoes_se_presente"]
        mridata <- cbind(mridata, df_alt2)

        # Dumming periependim
        name_periep <-c("lesoes_periependi_junto_3_vent",
                      "lesoes_periependi_junto_4_vent",
                      "lesoes_periependi_junt_vent_lat",
                      "lesoes_periependi_ausentes")
        df_periep <- split_mcv("periependimarias","4",mridata)
        df_periep <- mutate_mcv(name_periep,df_periep)
        # Exclude and bind
        mridata <- mridata[, names(mridata) != "periependimarias"]
        mridata <- cbind(mridata, df_periep)

        # Contrast
        mridata$realce <- factor(mridata$realce)
        if (length(levels(mridata$realce)) == 3){
                levels(mridata$realce) <- c("ausente","presente","n_avaliado")
        } else {
                levels(mridata$realce) <- c("0","ausente","presente","n_avaliado")
        }

        # Dumming contrast type
        name_tpcontrast <-c("realce_perif_complet",
                        "realce_perif_incomplet",
                        "realce_pseudo_realce",
                        "realce_homogeneo",
                        "realce_heterogeneo",
                        "outros_tipos_realce")

        df_tpcontrast <- split_mcv("tipo_realce","6",mridata)
        df_tpcontrast <- mutate_mcv(name_tpcontrast,df_tpcontrast)
        # Exclude and bind
        mridata <- mridata[, names(mridata) != "tipo_realce"]
        mridata <- cbind(mridata, df_tpcontrast)

}
