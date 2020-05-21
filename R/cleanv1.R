library(stringr)
library(dplyr)
library(data.table)
library(lubridate)



cleanv1 <- function (datafile){
  emocemp_messy <- read.csv(datafile,
                            skip = 1,
                            encoding = "UTF-8") # Load Data
  emocemp <- emocemp_messy[-c(1),-c(1:17)] # Exclude irrelevant rows and collumns from table
  emocemp_control <- emocemp
  emocemp <- as.data.table(emocemp)

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
  emocemp$imc <- as.numeric(str_replace(emocemp$imc, ",",","))

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
  emocemp$edss <- emocemp$edss[!is.na(emocemp$edss)] / 2
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

  # Load autoantibodies data
  autoantibody <- read.csv(file_mog)
  autoantibody <- rename(autoantibody,
                         id_paciente = n ,
                         mog = mog.positivo)

  # Merge datasets
  emocemp_merged_complete <- merge(emocemp, autoantibody, by = "id_paciente", all.x = TRUE)
  emocemp_merged_quali <- merge(emocemp, autoantibody, by = "id_paciente") # Only patients already tested for ab

  # Sourcing V2 cleaning #
  data2 <- file_v2
  emocemp2_messy <- read.csv(data2,
                             skip = 1) # Load Data
  emocemp2 <- emocemp2_messy[-c(1),-c(1:17)] # Exclude irrelevant rows and collumns from table
  emocemp2 <- emocemp2[, -79]
  emocemp_control2 <- emocemp2
  emocemp2 <- as.data.table(emocemp2)

  # Rename columns
  emocemp2 <- emocemp2 %>%
    rename(data_visita2 = Data.da.consulta..dia.mês.ano. ,
           id_centro = Identificação.do.Centro...Selecione.o.centro ,
           id_paciente = Identificação.do.paciente..número.na.pesquisa. ,
           peso2 = Peso.altura.IMC...Dados.antropométricos...Peso..kg. ,
           altura2 = Peso.altura.IMC...Dados.antropométricos...Altura..m. ,
           imc2 = Peso.altura.IMC...Dados.antropométricos...IMC.kg.m. ,
           tabagismo2 = Tabagismo ,
           sf_piramidal2 = Sistemas.funcionais...Piramidal ,
           sf_cerebelar2 = Sistemas.funcionais...Cerebelar ,
           sf_sensitivo2 = Sistemas.funcionais...Sensitivo ,
           sf_tronco2 = Sistemas.funcionais...Tronco.encefálico ,
           sf_visual2 = Sistemas.funcionais...Visual ,
           sf_vesical_intestinal2 = Sistemas.funcionais...Vesical.Intestinal ,
           sf_cerebral2 = Sistemas.funcionais...Cerebral ,
           convers_vesical_intestinal2 = Conversão.sistemas.funcionais...Vesical..Intestinal ,
           convers_visual2 = Conversão.sistemas.funcionais...Visual ,
           deambulacao2 = Deambulação...Avaliar.deambulação ,
           edss2 = EDSS ,
           data_brain_mr2 = Datas.exames.neuroimagem..especificar.para.exames.realizados.desde.a.última.consulta....Encéfalo...Dia.Mês.Ano ,
           data_spinal_mr2 = Datas.exames.neuroimagem..especificar.para.exames.realizados.desde.a.última.consulta....Medula...Dia.Mês.Ano ,
           data_orbit_mr2 = Datas.exames.neuroimagem..especificar.para.exames.realizados.desde.a.última.consulta....Órbitas...Dia.Mês.Ano ,
           brain_mr2 = Neuroimagem...encéfalo..marcar.se.alteração.presente....incluir.apenas.novos.exames...Selected.Choice ,
           brain_mr2_text = Neuroimagem...encéfalo..marcar.se.alteração.presente....incluir.apenas.novos.exames...Outras...Text ,
           brain_mr2_realce = Neuroimagem...encéfalo..marcar.se.alteração.presente....incluir.apenas.novos.exames...Outros.padrões.de.realce..descrever.localização.e.padrão.do.realce....Text ,
           orbit_mr2 = Neuroimagem...RM.órbita...observar.nervo.óptico..NO...marcar.se.alteração.presente....incluir.apenas.novos.exames...Selected.Choice ,
           orbit_mr2_text = Neuroimagem...RM.órbita...observar.nervo.óptico..NO...marcar.se.alteração.presente....incluir.apenas.novos.exames...Outras...Text ,
           orbit_mr2_realce = Neuroimagem...RM.órbita...observar.nervo.óptico..NO...marcar.se.alteração.presente....incluir.apenas.novos.exames...Realce.pelo.gadolínio..descrever.localização.no.NO....Text ,
           spinal_mr2 = Neuroimagem...RM.medular..marcar.se.alteração.presente....incluir.apenas.novos.exames...Selected.Choice ,
           spinal_mr2_text = Neuroimagem...RM.medular..marcar.se.alteração.presente....incluir.apenas.novos.exames...Outras...Text ,
           spinal_mr2_realce = Neuroimagem...RM.medular..marcar.se.alteração.presente....incluir.apenas.novos.exames...Realce.pelo.gadolínio..descrever.localização.e.padrão.do.realce....Text ,
           image2_outros = Neuroimagem...outros..incluir.apenas.novos.exames. ,
           medic_cont2 = Medicações.de.uso.contínuo..nome.genérico..dose.e.posologia. ,
           criterio_diag2 = Preenche.critérios.diagnósticos.para...Selected.Choice ,
           criterio_diag2_outros = Preenche.critérios.diagnósticos.para...Outras...Text ,
           fan2 = Novos.exames.laboratoriais.séricos...FAN..título.e.padrão. ,
           fr2 = Novos.exames.laboratoriais.séricos...FR..positivo.ou.negativo. ,
           vhs2 = Novos.exames.laboratoriais.séricos...VHS..mm.h. ,
           ssa2 = Novos.exames.laboratoriais.séricos...anti.SSA..UI. ,
           ssb2 = Novos.exames.laboratoriais.séricos...anti.SSB..UI. ,
           vitd2 = Novos.exames.laboratoriais.séricos...25.OH.vitamina.D..ng.mL. ,
           vitb12_2 = Novos.exames.laboratoriais.séricos...Vitamina.B12..pg.mL. ,
           labs_outros2 = Novos.exames.laboratoriais.séricos...Outros.exames ,
           dmd2 = Droga.modificadora.da.doença..dose.posologia....Selected.Choice ,
           ifb_im_2 = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1a.IM...Text ,
           ifb_sc_22_2 = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1a.SC.22mcg...Text ,
           ifb_sc_44_2 = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1a.SC.44mcg...Text ,
           ifb_1b_2 = Droga.modificadora.da.doença..dose.posologia....Interferon.beta.1b...Text ,
           glatir2 = Droga.modificadora.da.doença..dose.posologia....Acetato.de.glatiramer...Text ,
           teriflu2 = Droga.modificadora.da.doença..dose.posologia....Teriflunomida...Text ,
           fumarato2 = Droga.modificadora.da.doença..dose.posologia....Fumarato.de.dimetila...Text ,
           fingo2 = Droga.modificadora.da.doença..dose.posologia....Fingolimode...Text ,
           nataliz2 = Droga.modificadora.da.doença..dose.posologia....Natalizumab...Text ,
           alentuz2 = Droga.modificadora.da.doença..dose.posologia....Alentuzumab...Text ,
           rituxi2 = Droga.modificadora.da.doença..dose.posologia....Rituximab...Text ,
           azat2 = Droga.modificadora.da.doença..dose.posologia....Azatioprina...Text ,
           dmd_outros2 = Droga.modificadora.da.doença..dose.posologia....Outros...Text ,
           ae_2 = Efeitos.adversos...Selected.Choice ,
           ae_2_text = Efeitos.adversos...Outros...Text ,
           obsv_2 = Observações ,
           surto2 = Novo.surto.desde.a.última.consulta..se.resposta.positiva.responder.as.questões.seguintes. ,
           surto_2_clinic = Novas.síndromes.clínicas..incluir.data....Selected.Choice ,
           nob_data_v2 = Novas.síndromes.clínicas..incluir.data....Neurite.óptica.bilateral...Text ,
           nou_data_v2 = Novas.síndromes.clínicas..incluir.data....Neurite.óptica.unilateral...Text ,
           mielite_p_data_v2 = Novas.síndromes.clínicas..incluir.data....Mielite.parcial...Text ,
           mielite_t_data_v2 = Novas.síndromes.clínicas..incluir.data....Mielite.transversa...Text ,
           adem_data_v2 = Novas.síndromes.clínicas..incluir.data....ADEM...Text ,
           rombo_data_v2 = Novas.síndromes.clínicas..incluir.data....Rombencefalite...Text ,
           outras_clinic_data_v2 = Novas.síndromes.clínicas..incluir.data....Outras...Text ,
           lcr_cel2 = Líquido.cefalorraquidiano...Celularidade..céls.uL. ,
           lcr_dif2 = Líquido.cefalorraquidiano...Diferencial.... ,
           lcr_prot2 = Líquido.cefalorraquidiano...Proteínas..mg.dL. ,
           lcr_boc2 = Líquido.cefalorraquidiano...BOC..presentes.ausentes. ,
           lcr_igg2 = Líquido.cefalorraquidiano...IgG.índex..mg.dL. ,
           tto_fa2 = Tratamento.de.fase.aguda...dose.e.posologia...Selected.Choice ,
           tto_ivmp2 = Tratamento.de.fase.aguda...dose.e.posologia...MP.IV...Text ,
           tto_cortivo2 = Tratamento.de.fase.aguda...dose.e.posologia...Cortcoide.oral...Text ,
           tto_igiv = Tratamento.de.fase.aguda...dose.e.posologia...Ig.IV...Text ,
           tto_plex = Tratamento.de.fase.aguda...dose.e.posologia...Plasmaférse...Text
    )

  # Edss
  emocemp2$edss2 <- as.character(emocemp2$edss2)
  emocemp2$edss2 <- as.numeric(emocemp2$edss2) # Convert and coerce NAs
  emocemp2$edss2 <- emocemp2$edss2[!is.na(emocemp2$edss2)] / 2 # Divide EDSS by 2
  #Create index to verify 0.5 EDSS
  emocemp2 <- emocemp2 %>%
    mutate(edss_i = if_else(edss2 < 1, TRUE, FALSE))
  # Change 0.5 for zeros
  emocemp2$edss2[emocemp2$edss_i] <- 0

  # Critérios diagnósticos
  emocemp2$criterio_diag2 <- as.character(emocemp2$criterio_diag2)
  emocemp2$criterio_diag2 <- as.numeric(emocemp2$criterio_diag2)
  emocemp2$criterio_diag2 <- factor(emocemp2$criterio_diag2)
  levels(emocemp2$criterio_diag2) <- c("em","nmosd","isolated","other")


  # Merge V2
  emocemp_quali <- merge(emocemp_merged_quali, emocemp2, by = "id_paciente", all.x = TRUE)

  emocemp_quali_patients <- emocemp_quali %>%
    filter(criterio_diag2 == "em" | mog == "1")



  quali_final <- emocemp_quali_patients %>%
    select(id_paciente ,
           mog ,
           criterio_diag2 ,
           sex ,
           etnic ,
           idade_onset ,
           idade_10 ,
           imc_c ,
           infeccao_2meses ,
           tabagismo ,
           data_vacina ,
           nome_vacina ,
           neurite_b ,
           neurite_u ,
           neurite_all ,
           mielite_parcial ,
           mielite_transversa ,
           mielite_all ,
           adem ,
           romboencefalite ,
           outros_clinic ,
           edss ,
           disease_duration_m ,
           lcr_cel ,
           lcr_prot ,
           lcr_boc
    )

  # Exclude patient 106
  quali_final <- quali_final[-3,]
  #
  fwrite(quali_final, file = "quali_final.csv")
  # write.xlsx(quali_final, file = "quali_final.xlsx")
}
