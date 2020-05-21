#' Takes the raw .csv file exported from qualtrics and clean it
#'
#' @param datafile a V1 .csv file exported from qualtrics with numeric data option
#' @export
#' @author R.C.S

clean_v1 <- function (datafile){
  emocemp_messy <- read.csv(datafile,
                            skip = 1,
                            encoding = "UTF-8") # Load Data
  emocemp <- emocemp_messy[-c(1),-c(1:17)] # Exclude irrelevant rows and collumns from table
  emocemp_control <- emocemp

  # Renaming collumns
  emocemp <- emocemp %>%
    rename(data_visita = Data.da.visita ,
           id_centro = Identificação.do.Centro...Selecione.o.centro ,
           id_paciente = Identificação.do.paciente..número.na.pesquisa. ,
           registro_centro = Número.do.registro..no.centro. ,
           tcle = TCLE.Termo.de.assentimento ,
           inclusao = Critérios.de.inclusão ,
           exclusao = Critérios.de.exclusão ,
           nascimento = Data.de.nascimento..dia.mês.ano. ,
           sex = Sexo ,
           etnic = Raça..auto.determinação. ,
           peso = Peso.altura.IMC...Dados.antropométricos...Peso..kg. ,
           altura = Peso.altura.IMC...Dados.antropométricos...Altura..m. ,
           imc = Peso.altura.IMC...Dados.antropométricos...IMC.kg.m² ,
           data_onset = Data.de.início.dos.sintomas..dia.mês.ano. ,
           clinica_onset = Apresentação.clínica.inicial...Selected.Choice ,
           clinica_onset_outros = Apresentação.clínica.inicial...Outras...Texto ,
           historia_familiar = História.familiar.de.doenças.autoimunes...Selected.Choice ,
           historia_familiar_outros = História.familiar.de.doenças.autoimunes...Outras...Texto ,
           infeccao_2meses = Infecção.nos.últimos.2.meses..Especificar.se.resposta.positiva...Selected.Choice ,
           infeccao_especificar = Infecção.nos.últimos.2.meses..Especificar.se.resposta.positiva...Sim...Texto ,
           data_vacina = Data.da.última.vacina ,
           nome_vacina = Nome.da.última.s..vacinas.administradas.na.data.referida.na.questão.14 ,
           tabagismo = Tabagismo ,
           comorbidade = Comorbidades..especificar.doenças.sem.abreviações. ,
           medicacao = Medicações.de.uso.contínuo..nome.genérico..dose.e.posologia. ,
           sf_piramidal = Sistemas.funcionais...Piramidal ,
           sf_cerebelar = Sistemas.funcionais...Cerebelar ,
           sf_sensitivo = Sistemas.funcionais...Sensitivo ,
           sf_tronco = Sistemas.funcionais...Tronco.encefálico ,
           sf_visual = Sistemas.funcionais...Visual ,
           sf_vesical_intestinal = Sistemas.funcionais...Vesical.Intestinal ,
           sf_cerebral = Sistemas.funcionais...Cerebral ,
           conver_visual = Conversão.sistemas.funcionais...Visual ,
           conver_vesical_intestinal = Conversão.sistemas.funcionais...Vesical..Intestinal ,
           deambulacao = Deambulação...Avaliar.deambulação ,
           edss = EDSS ,
           data_brain_rm = Datas.exames.neuroimagem..especificar.para.exames.realizados....Encéfalo...Dia.Mês.Ano ,
           data_spinal_rm = Datas.exames.neuroimagem..especificar.para.exames.realizados....Medula...Dia.Mês.Ano ,
           data_orbit_rm = Datas.exames.neuroimagem..especificar.para.exames.realizados....Órbitas...Dia.Mês.Ano ,
           brain_mr = Neuroimagem...encéfalo..marcar.se.alteração.presente....Selected.Choice ,
           brain_mr_realce = Neuroimagem...encéfalo..marcar.se.alteração.presente....Outros.padrões.de.realce.pelo.Gd...descrever.localização.e.padrão.do.realce....Texto ,
           brain_outras = Neuroimagem...encéfalo..marcar.se.alteração.presente....Outras...Texto ,
           orbit_rm = Neuroimagem...RM.órbita...observar.nervo.óptico..NO...marcar.se.alteração.presente....Selected.Choice ,
           orbit_realce = Neuroimagem...RM.órbita...observar.nervo.óptico..NO...marcar.se.alteração.presente....Realce.pelo.gadolínio..descrever.localização.no.NO....Texto ,
           spinal_rm = Neuroimagem...RM.medular..marcar.se.alteração.presente....Selected.Choice ,
           spinal_realce = Neuroimagem...RM.medular..marcar.se.alteração.presente....Realce.pelo.gadolínio..descrever.localização.e.padrão.do.realce....Texto ,
           spinal_outros = Neuroimagem...RM.medular..marcar.se.alteração.presente....Outras...Texto ,
           rm_outros = Neuroimagem...outros ,
           fan = Exames.laboratoriais.séricos...FAN..título.e.padrão. ,
           fr = Exames.laboratoriais.séricos...FR..positivo.ou.negativo. ,
           vhs = Exames.laboratoriais.séricos...VHS..mm.h. ,
           ssa = Exames.laboratoriais.séricos...anti.SSA..UI. ,
           ssb = Exames.laboratoriais.séricos...anti.SSB..UI. ,
           vitd = Exames.laboratoriais.séricos...25.OH.vitamina.D..ng.mL. ,
           b12 = Exames.laboratoriais.séricos...Vitamina.B12..pg.mL. ,
           ebv = Exames.laboratoriais.séricos...EBV.IgG ,
           cmv = Exames.laboratoriais.séricos...CMV.IgG ,
           soro_outros = Exames.laboratoriais.séricos...Outras.sorologias ,
           lcr_cel = Líquido.cefalorraquidiano...Celularidade..céls.uL. ,
           lcr_dif = Líquido.cefalorraquidiano...Diferencial.... ,
           lcr_prot = Líquido.cefalorraquidiano...Proteínas..mg.dL. ,
           lcr_boc = Líquido.cefalorraquidiano...BOC..presentes.ausentes. ,
           lcr_igg = Líquido.cefalorraquidiano...IgG.índex..mg.dL. ,
           tto_fa = Tratamento.de.fase.aguda...dose.e.posologia...Selected.Choice ,
           tto_fa_ivmp = Tratamento.de.fase.aguda...dose.e.posologia...MP.IV...Texto ,
           tto_fa_co = Tratamento.de.fase.aguda...dose.e.posologia...Cortcoide.oral...Texto ,
           tto_fa_igg = Tratamento.de.fase.aguda...dose.e.posologia...Ig.IV...Texto ,
           tto_fa_plex = Tratamento.de.fase.aguda...dose.e.posologia...Plasmaférse...Texto ,
           dmd = Droga.modificadora.da.doença..dose.posologia....Selected.Choice ,
           dmd_ifb_im = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1a.IM...Texto ,
           dmd_ifb_sc22 = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1a.SC.22mcg...Texto ,
           dmd_ifb_sc44 = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1a.SC.44mcg...Texto ,
           dmd_ifb = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1b...Texto ,
           dmd_glatir = Droga.modificadora.da.doença..dose.posologia....Acetato.de.glatiramer...Texto ,
           dmd_teriflu = Droga.modificadora.da.doença..dose.posologia....Teriflunomida...Texto ,
           dmd_fuma = Droga.modificadora.da.doença..dose.posologia....Fumarato.de.dimetila...Texto ,
           dmd_fingo = Droga.modificadora.da.doença..dose.posologia....Fingolimode...Texto ,
           dmd_nat = Droga.modificadora.da.doença..dose.posologia....Natalizumab...Texto ,
           dmd_alen = Droga.modificadora.da.doença..dose.posologia....Alentuzumab...Texto ,
           dmd_ritux = Droga.modificadora.da.doença..dose.posologia....Rituximab...Texto ,
           dmd_azt = Droga.modificadora.da.doença..dose.posologia....Azatioprina...Texto ,
           dmd_outros = Droga.modificadora.da.doença..dose.posologia....Outros...Texto ,
           obsv = Observações
    )

  # HERE DO MUTATE SCRIPT
  # Clinical dumming
  name_c <- c("neurite_b",
              "neurite_u",
              "mielite_parcial",
              "adem",
              "mielite_transversa",
              "romboencefalite",
              "outros_clinic")

  df_clinica <- split_mcv("clinica_onset","7",emocemp)
  df_clinica <- mutate_mcv(name_c, df_clinica)

  # Acute treatment dumming
  name_ta <- c("ivmp",
               "corticode_vo",
               "igiv",
               "plex",
               "nenhum_ta")

  df_tta <- split_mcv("tto_fa","5",emocemp)
  df_tta <- mutate_mcv(name_ta, df_tta)

  # Brain MRI dumming
  name_brain_mri <- c("uma_ou_mais_perpendicular",
                      "apenas_bem_delimitadas",
                      "cinco_ou_mais_t2",
                      "duas_ou_mais_peri",
                      "uma_ou_mais_tronco",
                      "black_holes",
                      "difusas_bilat",
                      "uma_ou_mais_justa",
                      "uma_ou_mais_peri",
                      "uma_ou_mais_infra",
                      "realce_anel_inc",
                      "outro_realce",
                      "nenhum_brm",
                      "naorealizada_brm",
                      "outras_brm")

  df_brm <- split_mcv("brain_mr","15",emocemp)
  df_brm <- mutate_mcv(name_brain_mri, df_brm)

  # Spinal MRI dumming
  name_spinal_mri <- c("mielite_trans_mr",
                       "mielite_centromedular",
                       "mielite_perif",
                       "mielite_menos3",
                       "letm",
                       "mielite_cerv",
                       "mielite_dorsal",
                       "mielite_lombo",
                       "nenhuma_spinalmr",
                       "nao_realizada_srm",
                       "realce_medular",
                       "bright_spot",
                       "outras_srm")

  df_srm <- split_mcv("spinal_rm","13",emocemp)
  df_srm <- mutate_mcv(name_spinal_mri, df_srm)

  # Orbital MRI dumming
  name_orbital_mri <- c("hipersinal_retrob",
                        "hipersinal_quiasma",
                        "hipersinal_media",
                        "hipersinal_extensa",
                        "realce_orbit",
                        "nenhuma_orbitrm",
                        "nao_realizada_orbitrm",
                        "outras_orbitrm")

  df_orm <- split_mcv("orbit_rm","8",emocemp)
  df_orm <- mutate_mcv(name_orbital_mri, df_orm)

  # Merge dfs
  dummed <- cbind(df_orm,df_srm,df_brm,df_tta,df_clinica)
  emocemp <- cbind(emocemp, dummed)


  # -------------------------------------------
  # Demographics ------------------------------
  # -------------------------------------------
  # Etnic
  emocemp$etnic <- as.character(emocemp$etnic)
  emocemp$etnic <- as.numeric(emocemp$etnic)
  emocemp$etnic <- as.factor(emocemp$etnic)

  # Infecção últimos 2 meses
  emocemp$infeccao_2meses <- as.character(emocemp$infeccao_2meses)
  emocemp$infeccao_2meses <- as.numeric(emocemp$infeccao_2meses)
  emocemp$infeccao_2meses <- as.factor(emocemp$infeccao_2meses)
  levels(emocemp$infeccao_2meses) <- c("sim", "nao")

  # Sex
  emocemp$sex <- as.character(emocemp$sex)
  emocemp$sex <- as.numeric(emocemp$sex)
  emocemp$sex <- as.factor(emocemp$sex)
  levels(emocemp$sex) <- c("m", "f")

  # Altura
  emocemp$altura <- as.character(emocemp$altura)
  altura_cm <- str_detect(emocemp$altura, "^\\d{2}")
  emocemp$altura <- str_replace(emocemp$altura, ",", ".")
  emocemp$altura <- as.numeric(emocemp$altura)
  emocemp$altura[altura_cm] <- emocemp$altura[altura_cm] / 100 # Transform in meters

  # Peso
  emocemp$peso <- as.character(emocemp$peso)
  emocemp$peso <- as.numeric(str_replace(emocemp$peso, ",",","))

  # IMC
  emocemp$imc <- as.character(emocemp$imc)
  emocemp$imc <- as.numeric(str_replace(emocemp$imc, ",","."))

  # Tabagismo
  emocemp$tabagismo <- as.character(emocemp$tabagismo)
  emocemp$tabagismo <- as.numeric(emocemp$tabagismo)
  emocemp$tabagismo <- factor(emocemp$tabagismo)
  levels(emocemp$tabagismo) <- c("ativo","passivo","ausente")

  # -----------------------------------------------------
  # Clinical analysis -----------------------------------
  # -----------------------------------------------------

  # EDSS ------------------------------------------------
  # Change to numeric and divided by 2 (it is doubled from the exportation)
  emocemp$edss <- as.character(emocemp$edss)
  emocemp$edss <- as.numeric(emocemp$edss)
  emocemp$edss <- emocemp$edss / 2
  #Create index to verify 0.5 EDSS
  emocemp <- emocemp %>%
    mutate(edss_i = if_else(edss < 1, TRUE, FALSE))
  # Change 0.5 for zeros
  emocemp$edss[emocemp$edss_i] <- 0

  # Dates ------------------------------------------------
  # Nascimento
  emocemp$nascimento <- dmy(emocemp$nascimento)
  # Visit
  emocemp$data_visita <- dmy(emocemp$data_visita)
  # Onset
  emocemp$data_onset <- dmy(emocemp$data_onset)
  # Visit age
  emocemp <- emocemp %>%
    mutate(idade_visita1 = as.integer(
      (emocemp$data_visita - emocemp$nascimento)/365 ) )
  # Onset age
  emocemp <- emocemp %>%
    mutate(idade_onset = as.integer(
      (emocemp$data_onset - emocemp$nascimento)/365 ) )
  # Disease duration at visit (years)
  emocemp <- emocemp %>%
    mutate(disease_duration_y = as.integer(
      (emocemp$idade_visita1 - emocemp$idade_onset)
    ))
  # Disease duration at visit (months)
  emocemp <- emocemp %>%
    mutate(disease_duration_m = as.integer (
      (data_visita - data_onset) / 12 ) )
  # Replace negative values with NAs
  wrong_dd <- emocemp$disease_duration_m < 0
  emocemp$disease_duration_m[wrong_dd] <- NA
  wrong_ageonset <- emocemp$idade_onset < 0
  emocemp$idade_onset[wrong_ageonset] <- NA
  # Create age factor > | < 10
  emocemp <- emocemp %>%
    mutate(idade_10 = ifelse(idade_onset <= 10, "m", "M") )


  # -----------------------------------------
  # LCR -------------------------------------
  # -----------------------------------------
  lcr_messy <- emocemp %>%
    select(lcr_cel, lcr_dif, lcr_prot, lcr_boc)

  lcr_messy <- lcr_messy %>%
    rename (
      cel = lcr_cel ,
      dif = lcr_dif ,
      prot = lcr_prot ,
      boc = lcr_boc
    )

  lcr_messy$boc <- as.character(lcr_messy$boc)
  lcr_messy$cel <- as.character(lcr_messy$cel)
  lcr_messy$prot <- as.character(lcr_messy$prot)
  lcr_messy$dif <- as.character(lcr_messy$dif)

  # Inputting NAs ----------------------------------------------------------
  lcr_messy$boc[!str_detect(lcr_messy$boc, "")] <- NA
  lcr_messy$cel[!str_detect(lcr_messy$cel, "")] <- NA
  lcr_messy$prot[!str_detect(lcr_messy$prot, "")] <- NA
  # Cleaning strings --------------------------------------------------------
  # Protein LCR
  correct_prot <- str_extract(lcr_messy$prot, "^\\d{1,3}[\\. ,]?\\d{0,3}$")
  correct_prot <- str_replace(correct_prot, ",", ".")
  correct_prot <- as.numeric(correct_prot)
  emocemp$lcr_prot <- correct_prot
  # Cel LCR
  correct_cel <- str_extract(lcr_messy$cel, "^\\d{1,4}[\\. ,]?\\d{0,3}")
  correct_cel <- str_replace(correct_cel, "," , ".")
  correct_cel <- as.numeric(correct_cel)
  emocemp$lcr_cel <- correct_cel
  # Oligoclonal bands
  lcr_messy$boc <- tolower(lcr_messy$boc)
  # Create new IMC column with calculated from weight and height
  emocemp <- emocemp %>%
    mutate(imc_c = peso / altura^2)


  col_2_exclude <- c("edss_i",
                     "clinica_onset",
                     "tto_fa",
                     "brain_mr",
                     "orbit_mr",
                     "spinal_mr"
                     )

  cleaned <- emocemp[, ! names(emocemp) %in% col_2_exclude, drop = F]

  return(cleaned)
  #
  fwrite(cleaned, file = "cleaned.csv")
}
