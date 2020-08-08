#' Merge data from different visits. Set at least two cleaned visit data files.
#' @import dplyr
#' @import stringr
#' @importFrom lubridate dmy
#' @importFrom data.table melt
#' @importFrom data.table setDT
#' @param v1 data frame resulted from \link{clean_v1}
#' @param v2 data frame resulted from \link{clean_v2}
#' @param v3 data frame resulted from \link{clean_v3} ; Default is = NULL
#' @param mode select "long" or "wide" for different merge options. If a single line for each subject is desired = "wide". If a line for each visit is desired ; = "long"
#' @param all.patients Default = FALSE. If TRUE is selected, all patients with visit 1 will be merged, and NA values will be inputted in specific visit 2 variables
#' @return a data frame with merged visit 1 and 2 as specified in the parameters
#' @author R.C.S
#' @export






merge_visits <- function (v1, v2, v3 = NULL, mode = c("long","wide"), all.patients = FALSE) {

        if ("mog" %in% colnames(v1)){
        v1_new <- v1 %>%
                select(c("id_paciente","data_visita","nascimento","idade_10","idade_onset",
                         "sex","etnic","peso","altura","imc","infeccao_2meses",
                         "data_vacina","tabagismo","edss","lcr_cel","lcr_prot",
                         "dmd","romboencefalite","adem","mielite_transversa",
                         "mielite_parcial","neurite_u","neurite_b","mog","obese",
                         "sobrepe","inf_ou_vac_rec","outros_clinic","bfaz"))}
        else {
        v1_new <- v1 %>%
                select(c("id_paciente","data_visita","nascimento","idade_10","idade_onset",
                         "sex","etnic","peso","altura","imc","infeccao_2meses",
                         "data_vacina","tabagismo","edss","lcr_cel","lcr_prot",
                         "dmd","romboencefalite","adem","mielite_transversa",
                         "mielite_parcial","neurite_u","neurite_b","obese",
                         "sobrepe","inf_ou_vac_rec","outros_clinic","bfaz"))
        }

        v2_new <- v2 %>%
                mutate(dmd = ifelse(nenhum_dmd == 1,0,1)) %>%
                select(c("id_paciente","data_visita","peso","altura",
                         "imc","tabagismo","edss","criterio_diag_v2",
                         "novo_surto_v2","lcr_cel","lcr_prot","dmd"))

        if(!is.null(v3)){
                v3_new <- v3 %>%
                mutate(dmd = ifelse(nenhum_dmd == 1,0,1)) %>%
                select(c("id_paciente","data_visita","peso","altura",
                        "imc","tabagismo","edss","criterio_diag_v3",
                        "novo_surto_v3","lcr_cel","lcr_prot","dmd"))
                colnames(v3_new) <- c('id_paciente','data_visita3','peso3','altura3',
                                      'imc3','tabagismo3','edss3','criterio_diag_v3',
                                      'novo_surto_v3','lcr_cel3','lcr_prot3','dmd3')
        }



        # Check for patient value and merge datasets
        if (is.null(v3)){
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
                v1v2_wide <- suppressWarnings(data.table::melt(data.table::setDT(v1v2_long), measure.vars = repme,
                                                               variable.name = "Visit", value.name = name_repme))

                v1v2_wide$Visit[is.na(v1v2_wide$data_visita) & (v1v2_wide$Visit != 1)] <- NA






                return(v1v2_wide)

        }
        } else {

                # Check for patient value and merge datasets
                if (all.patients == TRUE) {

                tmp_long <- merge(v1_new,v2_new,by = "id_paciente",suffixes = c("1","2"),
                                                   all.x = TRUE)
                merged_long <- merge(tmp_long, v3_new, by = "id_paciente", all.x = TRUE)

                } else {
                        tmp_long <- merge(v1_new,v2_new,by = "id_paciente",suffixes = c("1","2"))
                        merged_long <- merge(tmp_long, v3_new, by = "id_paciente")
                        }

                # Exit with long if mode = long

                if (mode == "long") {

                        return(merged_long)


                # Process merged data if mode = wide

                } else if (mode == "wide") {

                # Create rep measures list
                repme <- list(c("edss1","edss2","edss3"),c("altura1","altura2","altura3"),
                                c("peso1","peso2","peso3"),c("data_visita1","data_visita2","data_visita3"),
                                c("imc1","imc2","imc3"),c("dmd1","dmd2","dmd3"),
                                c("lcr_prot1","lcr_prot2","lcr_prot3"),
                                c("lcr_cel1","lcr_cel2","lcr_cel3"),c("tabagismo1","tabagismo2","tabagismo3"))

                name_repme <- c("edss","altura","peso","data_visita",
                                "imc","dmd","lcr_prot","lcr_cel","tabagismo")

                # Melt data
                merged_wide <- suppressWarnings(data.table::melt(setDT(merged_long), measure.vars = repme,
                                                                               variable.name = "Visit", value.name = name_repme))


                merged_wide$Visit[is.na(merged_wide$data_visita) & (merged_wide$Visit != 1)] <- NA






        return(merged_wide)

        }

        }

}
