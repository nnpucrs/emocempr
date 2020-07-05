#' Takes a EMOCEMP spinal cord MRI .csv (numeric) file and clean it
#' @import dplyr
#' @import stringr
#' @importFrom lubridate dmy
#' @importFrom utils read.csv
#' @param datafile File extracted from Qualtrics in .csv format with the numeric option
#' @return a data frame with the input brain MRI data cleaned
#' @author R.C.S
#' @export


clean_spinal <- function(datafile){
        mridata <- read.csv(datafile, encoding = "UTF-8", skip = 1)
        mridata <- mridata[-c(1),-c(1:17)]

        # Rename
        print("Renaming")
        colnames(mridata) <- c("avaliador","id_participante","data_exame",
                               "rm_medula","campo_mag","campo_mag_text",
                               "axialt2","sagitalt2","stirsagital",
                               "sagitalt1pre","axialt1pre","axialt1pos",
                               "sagitalt1pos","axialt2gre","seq_nao_realizadas",
                               "lesoes_medulares","lesoes_medulares_text",
                               "num_lesoes_cerv","num_lesoes_torac","num_lesoes_lomb",
                               "alt_presentes","letm","letm_text_cm","letm_text_seg",
                               "outras_alt_coments","realce","tipo_realce","tipo_realce_text")
        print ("Done")


        # Define Clean
        print("Dumming multiple choice variables")
        clean <- function(var,database,n,names){
                df <- dummie_mcv(names = names,
                                         var = var,
                                         n = n,
                                         dataf = database)
                database <- database[, names(database) != var]
                database <- cbind(database, df)
                return(database)
                }

        # Cleaning multiple choice variables
        names_at2 <- c("axialt2_cerv","axialt2_tor","axialt2_lomb")
        mridata <- clean("axialt2",mridata,3,names_at2)

        names_sagt2 <- c("sagt2_cerv","sagt2_tor","sagt2_lomb")
        mridata <- clean("sagitalt2",mridata,3,names_sagt2)

        names_stirsag <- c("stir_cerv","stir_tor","stir_lomb")
        mridata <- clean("stirsagital",mridata,3,names_stirsag)

        names_sagt1pr <- c("sagt1pr_cerv","sagt1pr_tor","sagt1pr_lomb")
        mridata <- clean("sagitalt1pre",mridata,3,names_sagt1pr)

        names_axt1pr <- c("axt1pr_cerv","axt1pr_tor","axt1pr_lomb")
        mridata <- clean("axialt1pre",mridata,3,names_axt1pr)

        names_axialt1pos <- c("axt1pos_cerv","axt1pos_tor","axt1pos_lomb")
        mridata <- clean("axialt1pos",mridata,3,names_axialt1pos)

        names_sagitalt1pos <- c("sgtt1pos_cerv","sgtt1pos_tor","sgtt1pos_lomb")
        mridata <- clean("sagitalt1pos",mridata,3,names_sagitalt1pos)

        names_axialt2gre <- c("axt2gre_cerv","axt2gre_tor","axt2gre_lomb")
        mridata <- clean("axialt2gre",mridata,3,names_axialt2gre)

        names_alt <- c("mt","mcm","mp","mhm","bright_spot")
        splited <- split_mcv("alt_presentes",5,mridata)
        splited[splited == 7] <- 5
        df <- mutate_mcv(names_alt, splited)
        mridata <- mridata[, names(mridata) != "alt_presentes"]
        mridata <- cbind(mridata, df)


        names_realce <- c("perif_anel_comp","perif_anel_inc","pseudorealce",
                          "realce_homogeneo","realce_heterog","realce_outros")
        mridata <- clean("tipo_realce",mridata,6,names_realce)

        # Clean numeric
        message("Cleaning numeric variables")

        for (i in c("num_lesoes_cerv","num_lesoes_torac","num_lesoes_lomb")){
        mridata[[i]] <- as.numeric(mridata[[i]])}

        message("Done")

        # Clean factors
        message("Cleaning factors")

        mridata[,"campo_mag"] <- as.numeric(mridata[,"campo_mag"])
        mridata[,"campo_mag"] <- as.factor(mridata[,"campo_mag"])
        levels(mridata[,"campo_mag"]) <- c("1.5T","3.0T","<1.5T")

        mridata[,"lesoes_medulares"] <- as.numeric(mridata[,"lesoes_medulares"])
        mridata[,"lesoes_medulares"] <- as.factor(mridata[,"lesoes_medulares"])
        levels(mridata[,"lesoes_medulares"]) <- c("lesoes_presentes","lesoes_ausentes")

        names_letm <- c("lesoes_presentes","tmp","lesoes_ausentes")
        df <- split_mcv("letm",3,mridata)
        df[df == 1 | df == 2] <- 1
        df <- mutate_mcv(names_letm,df)
        df <- df[,-2]
        mridata <- mridata[, names(mridata) != "letm"]
        mridata <- cbind(mridata, df)

        mridata[,"realce"] <- as.numeric(mridata[,"realce"])
        mridata[,"realce"] <- as.factor(mridata[,"realce"])
        levels(mridata[,"realce"]) <- c("realce_presente","realce_ausente")

        # Clean dates
        mridata[,"data_exame"] <- lubridate::dmy(mridata[,"data_exame"])


}
