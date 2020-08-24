#'Takes a raw dataset for emocemp teleconsults and clean it
#'@description The raw data need to be exported from qualtrics EMOCEMP Tele in .csv with the numeric values option checked
#'@import dplyr
#'@import stringr
#'@importFrom lubridate dmy
#'@importFrom utils read.csv
#'@param datafile a EMOCEMP teleconsult .csv file exported from qualtrics with numeric data option
#'@return a data.frame with cleaned data which may be used to merge with other visits
#'@export
#'@author R.C.S

clean_tele <- function(datafile){
        emocemp_messy <- read.csv(datafile,
                                  skip = 1,
                                  encoding = "UTF-8") # Load Data
        emocemptele <- emocemp_messy[-c(1),-c(1:17)] # Exclude irrelevant rows and collumns from table
        # -------------------------------------------------------------------------
        colnames(emocemptele)<-c("consetimento","acompanhante","nome_acompanhante",
                                "nome_acompanhante","visita_tele",
                                "instrumento_utilizado","id_centro","id_paciente",
                                "data_televisita","novo_surto_tele",
                                "novas_sind_clinica","data_nob",
                                "data_nou","data_mp",
                                "data_mt","data_adem",
                                "data_rombo","data_outras",
                                "lcr_cel","lcr_dif",
                                "lcr_prot","lcr_boc",
                                "lcr_igg",
                                "tto_fa","tto_fa_mpiv",
                                "tto_fa_co","tto_fa_igiv",
                                "tto_fa_plex","obsv_surtos",
                                "tabagismo","peso_kg",
                                "altura_m","dmd",
                                "dmd_ifb1a","dmd_ifb1a22",
                                "dmd_ifb1a44","dmd_ifb1b",
                                "dmd_glat","dmd_terif",
                                "dmd_dmf","dmd_fingo",
                                "dmd_nataliz","dmd_alent","dmd_ritux",
                                "dmd_aza","dmd_outros",
                                "ea","ea_outros",
                                "medica_continuo","fan",
                                "fr","vhs","ssa",
                                "ssb","vit_d","vit_b12",
                                "labs_outros","obs","data_encefalo",
                                "data_orbita","data_medula",
                                "DICOM","brain_mr","brain_realce_mr",
                                "brain_mr_outros", "orbit_mr",
                                "orbit_realce_mr","orbit_mr_outros",
                                "spinal_mr","spinal_mr_realce",
                                "spinal_mr_outros","mr_outros",
                                "deambul","sf_pira",
                                "sf_cereb","sf_sens",
                                "sf_tronco","sf_visual",
                                "sf_vesicalintest","sf_cerebral",
                                "cv_visual","cv_vesticalinstes",
                                "edss","crierio_diag_tele",
                                "criterio_outros_tele", "obsv_geral")

        # -------------------------------------------------------------------------
        message("Dumming multiple choice variables")
        # Define function
        clean <- function(var,database,n,names){
                df <- dummie_mcv(names = names,
                                 var = var,
                                 n = n,
                                 dataf = database)
                database <- database[, names(database) != var]
                database <- cbind(database, df)
                return(database)
        }
        # -------------------------------------------------------------------------
        # Clinical
        # -------------------------------------------------------------------------
        message("Clinical")
        name_clinical <- c("neurite_b",
                           "neurite_u",
                           "mielite_p",
                           "mielite_t",
                           "adem" ,
                           "romboencefalite",
                           "outras",
                           "nenhumas_das_sindromes")

        emocemptele <- clean("novas_sind_clinica",emocemptele,8,name_clinical)
        # MRI
        # -------------------------------------------------------------------------
        message("Image")

        # Brain
        name_brain_mr <- c("perp_cc","apenas_bem_deli","5ou_mais_T2",
                           "2ou_mais_peri","1_tronco","black_holes",
                           "difusas_bilat","uma_ou_mais_justa","uma_ou_mais_peri",
                           "1_infra","outros_realce",
                           "nenhuma_acima_brain_mr",
                           "brain_mr_nr",
                           "outras_brain_mr","realce_anel_incompleto")

        df_alt <- split_mcv("brain_mr",15,emocemptele)
        df_alt[df_alt == 17] <- 15
        df_alt <- mutate_mcv(name_brain_mr,df_alt)
        emocemptele <- emocemptele[, names(emocemptele) != "brain_mr"]
        emocemptele <- cbind(emocemptele, df_alt)

        # Spinal
        name_spinal_mr <- c("spinal_mr_mt","spinal_mr_mcm","spinal_mr_mp",
                           "spinal_mr_<3","spinal_mr_>3","spinal_mr_cervical",
                           "spinal_mr_dorsal","spinal_mr_lomb","spinal_mr_nenhuma_ac",
                           "spinal_mr_nr","spinal_mr_realce",
                           "spinal_mr_bright_spot",
                           "spinal_mr_outras")

        emocemptele <- clean("spinal_mr",emocemptele,13,name_spinal_mr)

        # Orbit
        name_orbit_mr <- c("hiper_t2_flair_retro","hiper_t2_flair_qui","hiper_t2_flair_med",
                           "hiper_t2_flair_ext","orbit_realce","orbit_mr_nenhuma_ac",
                           "orbit_mr_nr","orbit_mr_outras")

        emocemptele <- clean("orbit_mr",emocemptele,13,name_orbit_mr)

        # Treatment
        # -------------------------------------------------------------------------
        message("Treatment")

        name_dmd <- c("ifn_b1a_IM","ifn_b1a_22","ifn_b1a_44","ifn_b1b",
                      "ac_glat","teriflunomida","dmf","fingo","nataliz",
                      "alentuz","rituximab","azat","outros_dmd","nenhum_dmd")


        emocemptele <- clean("dmd",emocemptele,14,name_dmd)

        # Factors
        # -------------------------------------------------------------------------
        message("Cleaning factors")

        # Criteria
        emocemptele$crierio_diag_tele <- as.factor(emocemptele$crierio_diag_tele)
        if ("2" %in% emocemptele$crierio_diag_tele){
                levels(emocemptele$crierio_diag_tele) <- c("em","nmosd","isolated_event","outras","cis")
        } else {
                levels(emocemptele$crierio_diag_tele) <- c("em","isolated_event","outras","cis")}
        # Relapse
        emocemptele$novo_surto_tele <- as.factor(emocemptele$novo_surto_tele)
        levels(emocemptele$novo_surto_tele) <- c("sim","nao")

        # -------------------------------------------------------------------------
        # Dates
        # -------------------------------------------------------------------------
        message("Cleaning dates")
        dates <- c("data_televisita","data_nob",
                   "data_nou","data_mp",
                   "data_mt","data_adem",
                   "data_rombo","data_outras",
                   "labs_outros","data_encefalo",
                   "data_orbita","data_medula")

        for (i in dates) {
                emocemptele[[i]] <- suppressWarnings(dmy(emocemptele[[i]]))
        }
        # -------------------------------------------------------------------------
        # Numerics
        # -------------------------------------------------------------------------
        message("Cleaning Numerics")
        # EDSS
        message("EDSS")
        emocemptele$edss <- as.numeric(emocemptele$edss) / 2
        emocemptele$edss[emocemptele$edss == 0.5] <- 0
        emocemptele$peso <- as.numeric(str_replace(emocemptele$peso, ",","."))

        # Height
        message("Heigth")
        altura_i <- str_detect(emocemptele$altura, "^\\d{2}")
        emocemptele$altura <- as.numeric(str_replace(emocemptele$altura,",","."))
        altura_i[is.na(altura_i)] <- FALSE
        emocemptele$altura[altura_i] <- emocemptele$altura[altura_i] / 100

        # Weight
        message("Weigth")
        emocemptele$peso <- as.numeric(str_replace(emocemptele$peso, ",","."))

        # LCR
        emocemptele$lcr_boc[!str_detect(emocemptele$lcr_boc,"")] <- NA
        emocemptele$lcr_cel[!str_detect(emocemptele$lcr_cel,"")] <- NA
        emocemptele$lcr_prot[!str_detect(emocemptele$lcr_prot,"")] <- NA


        # Clean strings
        # Protein
        emocemptele$lcr_prot <- str_extract(emocemptele$lcr_prot,
                                          "^\\d{1,3}[\\. ,]?\\d{0,3}$")

        # Cel
        emocemptele$lcr_cel <- str_extract(emocemptele$lcr_cel,
                                         "^\\d{1,3}[\\. ,]?\\d{0,3}")
        emocemptele$lcr_cel <- as.numeric(str_replace(emocemptele$lcr_cel,
                                                    ",","."))

        return(emocemptele)
        }
