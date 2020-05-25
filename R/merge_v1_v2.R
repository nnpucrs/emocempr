#' Merge visit 1 and visit 2 data frames
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @import data.table
#' @param v1 data frame resulted from \link{clean_v1}
#' @param v2 data frame resulted from \link{clean_v2}
#' @param mode select "long" or "wide" for different merge options. If a single line for each subject is desired = "wide". If a line for each visit is desired ; = "long"
#' @param all.patients Default = FALSE. If TRUE is selected, all patients with visit 1 will be merged, and NA values will be inputted in specific visit 2 variables
#' @return a data frame with merged visit 1 and 2 as specified in the parameters
#' @author R.C.S
#' @export



merge_v1v2 <- function (v1, v2, mode = c("long","wide"), all.patients = FALSE) {

        v1_new <- v1 %>%
                select(c("id_paciente","data_visita","nascimento","idade_10",
                         "sex","etnic","peso","altura","imc","infeccao_2meses",
                         "data_vacina","tabagismo","edss","lcr_cel","lcr_prot",
                         "dmd","romboencefalite","adem","mielite_transversa",
                         "mielite_parcial","neurite_u","neurite_b"))
        v2_new <- v2 %>%
                mutate(dmd = ifelse(nenhum_dmd == 1,0,1)) %>%
                select(c("id_paciente","data_visita","peso","altura",
                         "imc","tabagismo","edss","criterio_diag_v2",
                         "novo_surto_v2","lcr_cel","lcr_prot","dmd"))


        # Check for patient value and merge datasets

        if (all.patients == TRUE) {

                v1v2_long <- merge(v1_new,v2_new,by = "id_paciente",suffixes = c("1","2"),
                                   all.x = TRUE)
        } else {
                v1v2_long <- merge(v1_new,v2_new,by = "id_paciente",suffixes = c("1","2"))
        }

        # Exit with long if mode = long

        if (mode == "long") {

                return(v1v2_long)


        # Process merged data if mode = wide

        } else if (mode == "wide") {

                # Create rep measures list
                repme <- list(c("edss1","edss2"),c("altura1","altura2"),
                        c("peso1","peso2"),c("data_visita1","data_visita2"),
                        c("imc1","imc2"),c("dmd1","dmd2"),
                        c("lcr_prot1","lcr_prot2"),
                        c("lcr_cel1","lcr_cel2"),c("tabagismo1","tabagismo2"))

                name_repme <- c("edss","altura","peso","data_visita",
                                "imc","dmd","lcr_prot","lcr_cel","tabagismo")
                # Melt data
                v1v2_wide <- suppressWarnings(data.table::melt(setDT(v1v2_long), measure.vars = repme,
                                 variable.name = "Visit", value.name = name_repme))

                v1v2_wide$Visit[is.na(v1v2_wide$data_visita) & (v1v2_wide$Visit != 1)] <- NA






                return(v1v2_wide)

        }

}




























