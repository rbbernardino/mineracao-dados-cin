install.packages("data.table")
#  library(data.table)
library(tidyverse)

# Funcoes de criacao de variaveis

# Funcao que identifica as disciplinas que o professor ministra
identifica_disciplinas <- function(ID_QUIMICA,
                                   ID_FISICA,
                                   ID_MATEMATICA,
                                   ID_BIOLOGIA,
                                   ID_CIENCIAS,
                                   ID_LINGUA_LITERAT_PORTUGUESA,
                                   ID_LINGUA_LITERAT_INGLES,
                                   ID_LINGUA_LITERAT_ESPANHOL,
                                   ID_LINGUA_LITERAT_OUTRA,
                                   ID_ARTES,
                                   ID_EDUCACAO_FISICA,
                                   ID_HISTORIA,
                                   ID_GEOGRAFIA,
                                   ID_FILOSOFIA,
                                   ID_ESTUDOS_SOCIAIS,
                                   ID_INFORMATICA_COMPUTACAO,
                                   ID_PROFISSIONALIZANTE,
                                   ID_FUNDAMENTOS_EDUCACAO,
                                   ID_DISC_ATENDIMENTO_ESPECIAIS,
                                   ID_DISC_DIVERSIDADE_SOCIO_CULT,
                                   ID_OUTRAS_DISCIPLINAS_PEDAG,
                                   ID_LIBRAS,
                                   ID_OUTRAS_DISCIPLINAS,
                                   ID_ESPECIALIZACAO,
                                   ID_MESTRADO,
                                   ID_DOUTORADO,
                                   ID_ESPECIFICO_CRECHE,
                                   ID_ESPECIFICO_PRE_ESCOLA,
                                   ID_ESPECIFICO_NEC_ESP,
                                   ID_ESPECIFICO_ED_INDIGENA,
                                   ID_INTERCULTURAL_OUTROS){
  d <- data.frame(da_aula = c(ID_QUIMICA,
                              ID_FISICA,
                              ID_MATEMATICA,
                              ID_BIOLOGIA,
                              ID_CIENCIAS,
                              ID_LINGUA_LITERAT_PORTUGUESA,
                              ID_LINGUA_LITERAT_INGLES,
                              ID_LINGUA_LITERAT_ESPANHOL,
                              ID_LINGUA_LITERAT_OUTRA,
                              ID_ARTES,
                              ID_EDUCACAO_FISICA,
                              ID_HISTORIA,
                              ID_GEOGRAFIA,
                              ID_FILOSOFIA,
                              ID_ESTUDOS_SOCIAIS,
                              ID_INFORMATICA_COMPUTACAO,
                              ID_PROFISSIONALIZANTE,
                              ID_FUNDAMENTOS_EDUCACAO,
                              ID_DISC_ATENDIMENTO_ESPECIAIS,
                              ID_DISC_DIVERSIDADE_SOCIO_CULT,
                              ID_OUTRAS_DISCIPLINAS_PEDAG,
                              ID_LIBRAS,
                              ID_OUTRAS_DISCIPLINAS,
                              ID_ESPECIALIZACAO,
                              ID_MESTRADO,
                              ID_DOUTORADO,
                              ID_ESPECIFICO_CRECHE,
                              ID_ESPECIFICO_PRE_ESCOLA,
                              ID_ESPECIFICO_NEC_ESP,
                              ID_ESPECIFICO_ED_INDIGENA,
                              ID_INTERCULTURAL_OUTROS),
                  materia = c("quimica","fisica","matematica","biologia",
                              "ciencias","portugues","ingles","espanhol","outra lingua",
                              "artes","educacao fisica","historia","geografia","filosofia",
                              "estudos sociais","informatica","profissionalizante",
                              "fundamentos da educacao (pedagogica)",
                              "atendimento de necessidades especiais (pedagogica)",
                              "diversidade socio-cultural (pedagogica)",
                              "outras disciplinas pedagogicas (pedagogica)",
                              "libras","outras","especializacao (pos)",
                              "mestrado","doutorado",
                              "creche","pre-escola","educacao especial",
                              "educacao indigena","intercultural/diversidade/outros"))
  
  res <- d %>% 
    filter(da_aula>0) %>% 
    select(materia) %>% 
    unlist(use.names = F) %>% 
    as.character() %>% 
    sort() %>% 
    str_flatten(collapse = " - ")
  
  if(res==""){ res <- "sem informacao"  }
  
  return(res)
}

##############################################################################
identifica_modalidade_ensino <- function(x){
  res <- data.frame(mod = x) %>% 
    mutate(desc = case_when(mod==1~"ensino regular",
                            mod==2~"educacao especial",
                            mod==3~"educacao de jovens e adultos",
                            TRUE~"nao especificado")) %>% 
    select(desc) %>% 
    unique() %>% 
    unlist(use.names = F) %>% 
    as.character() %>% 
    sort() %>% 
    str_flatten(collapse = " - ")
  
  return(res)
  
}

##############################################################################
tab_etapa <- readxl::read_excel("/DicionarioEtapaEnsino.xlsx")[,2:3]

##############################################################################
identifica_etapa_ensino <- function(x){
  res <- data.frame(cod = x) %>% 
    left_join(tab_etapa,by = "cod") %>% 
    select(-cod) %>% 
    unique() %>% 
    unlist(use.names = F) %>% 
    as.character() %>% 
    sort() %>% 
    str_flatten(collapse = " - ")
  return(res)
}


##############################################################################
identifica_localizacao_escola <- function(x){
  res <- data.frame(mod = x) %>% 
    mutate(desc = case_when(mod==1~"urbana",
                            mod==2~"rural",
                            TRUE~"nao especificado")) %>% 
    select(desc) %>% 
    unique() %>% 
    unlist(use.names = F) %>% 
    as.character() %>% 
    sort() %>% 
    str_flatten(collapse = " - ")
}

identifica_dependencia_adm_escola <- function(x){
  res <- data.frame(mod = x) %>% 
    mutate(desc = case_when(mod==1~"federal",
                            mod==2~"estadual",
                            mod==3~"municipal",
                            mod==4~"privada",
                            TRUE~"nao especificado")) %>% 
    select(desc) %>% 
    unique() %>% 
    unlist(use.names = F) %>% 
    as.character() %>% 
    sort() %>% 
    str_flatten(collapse = " - ")
}


identifica_categoria_escola_privada <- function(x){
  res <- data.frame(mod = x) %>% 
    mutate(desc = case_when(mod==1~"particular",
                            mod==2~"comunitaria",
                            mod==3~"confessional",
                            mod==4~"filantropica",
                            TRUE~"nao especificado")) %>% 
    select(desc) %>% 
    unique() %>% 
    unlist(use.names = F) %>% 
    as.character() %>% 
    sort() %>% 
    str_flatten(collapse = " - ")
}

##############################################################################
##############################################################################
##############################################################################
##############################################################################


##############################################################################
DATA_DIR_RDS = "https://filedn.com/lmOQJTFtYV1hIVR608T0lBY/mineracao/DOCENTES/"
read_docentes_rds = function(year) {
  file_rds = paste0(DATA_DIR_RDS, "DOCENTES_NORDESTE_", year, ".rds")
  temp <- tempfile()
  download.file(file_rds, temp)
  df = tibble(readRDS(temp))
  unlink(temp)
  df
}

##############################################################################
DATA_DIR_ZIP = "https://filedn.com/lmOQJTFtYV1hIVR608T0lBY/mineracao/docentes_zip/"
DEFAULT_FILENAME = "DOCENTES_NORDESTE.CSV"
read_docentes_zip = function(year) {
  zip_fpath = paste0(DATA_DIR_ZIP, year, "_DOCENTES_NORDESTE.zip")
  temp <- tempfile()
  download.file(zip_fpath, temp)
  df = read_delim(unz(temp, DEFAULT_FILENAME), delim="|")
  unlink(temp)
  # df 
  return(df)
}

##############################################################################
##############################################################################
##############################################################################
##############################################################################


##############################################################################
# df = read_docentes_zip("2009")
# print(df)
## head(tab_etapa,10)


##############################################################################
##############################################################################
##############################################################################
##############################################################################



doc <- list()
k <- 2007
for(i in 1:11){
  cat("Ano .........",k,"\n")
  #doc[[i]] <- data.table::fread(file = paste0("BDS/DOCENTES_NORDESTE_",k,".CSV"))
  #doc[[i]] <- sample_frac(data.table::fread(file = paste0("BDS/DOCENTES_NORDESTE_",k,".CSV")),size = 0.05)
  #doc[[i]] <- data.table::fread(file = paste0("BDS/DOCENTES_NORDESTE_",k,".CSV"),nrows = 20000)
  #doc[[i]] <- read_docentes_zip(as.character(k))

  # Tentando diminuir consumo memoria

  if(k==2007){
    doc[[i]] <- read_docentes_zip(as.character(k)) %>%
      filter(FK_COD_ESCOLARIDADE%in%c(3,4,5)) %>%
      filter(ID_TIPO_DOCENTE==0) %>% # Somente docentes
      mutate(ID_FUNDAMENTOS_EDUCACAO = 0,
             ID_OUTRAS_DISCIPLINAS_PEDAG = 0) %>% 
      group_by(FK_COD_DOCENTE) %>% 
      summarise(n_turmas_que_da_aula_2007 = n(),
                n_escolas_da_aula_2007 = length(table(PK_COD_ENTIDADE)),
                sexo =  mean(if_else(TP_SEXO=="M",1,0)),
                raca_cor = mean(TP_COR_RACA),
                cod_municipio_residencia_2007 = mean(FK_COD_MUNICIPIO_DEND),
                dia_nascimento = mean(NU_DIA),
                mes_nascimento = mean(NU_MES),
                ano_nascimento = mean(NU_ANO),
                disciplinas_que_da_aula_2007 = identifica_disciplinas(sum(ID_QUIMICA,na.rm = TRUE),
                                                                      sum(ID_FISICA,na.rm = TRUE),
                                                                      sum(ID_MATEMATICA,na.rm = TRUE),
                                                                      sum(ID_BIOLOGIA,na.rm = TRUE),
                                                                      sum(ID_CIENCIAS,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_PORTUGUESA,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_INGLES,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_ESPANHOL,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_OUTRA,na.rm = TRUE),
                                                                      sum(ID_ARTES,na.rm = TRUE),
                                                                      sum(ID_EDUCACAO_FISICA,na.rm = TRUE),
                                                                      sum(ID_HISTORIA,na.rm = TRUE),
                                                                      sum(ID_GEOGRAFIA,na.rm = TRUE),
                                                                      sum(ID_FILOSOFIA,na.rm = TRUE),
                                                                      sum(ID_ESTUDOS_SOCIAIS,na.rm = TRUE),
                                                                      sum(ID_INFORMATICA_COMPUTACAO,na.rm = TRUE),
                                                                      sum(ID_PROFISSIONALIZANTE,na.rm = TRUE),
                                                                      sum(ID_FUNDAMENTOS_EDUCACAO,na.rm = TRUE),
                                                                      sum(ID_DISC_ATENDIMENTO_ESPECIAIS,na.rm = TRUE),
                                                                      sum(ID_DISC_DIVERSIDADE_SOCIO_CULT,na.rm = TRUE),
                                                                      sum(ID_OUTRAS_DISCIPLINAS_PEDAG,na.rm = TRUE),
                                                                      sum(ID_LIBRAS,na.rm = TRUE),
                                                                      sum(ID_OUTRAS_DISCIPLINAS,na.rm = TRUE),
                                                                      sum(ID_ESPECIALIZACAO,na.rm = TRUE),
                                                                      sum(ID_MESTRADO,na.rm = TRUE),
                                                                      sum(ID_DOUTORADO,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_CRECHE,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_PRE_ESCOLA,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_NEC_ESP,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_ED_INDIGENA,na.rm = TRUE),
                                                                      sum(ID_INTERCULTURAL_OUTROS,na.rm = TRUE)),
                modalidade_ensino_2007 = identifica_modalidade_ensino(FK_COD_MOD_ENSINO),
                dependencia_adm_escola_2007 = identifica_dependencia_adm_escola(ID_DEPENDENCIA_ADM),
                categoria_escola_privada_2007 = if_else(str_detect(dependencia_adm_escola_2007,"privada"),
                                                        identifica_categoria_escola_privada(DESC_CATEGORIA_ESCOLA_PRIVADA),
                                                        "nao se aplica"),
                localizacao_escola_2007 = identifica_localizacao_escola(ID_LOCALIZACAO),
                etapa_ensino_2007 = identifica_etapa_ensino(FK_COD_ETAPA_ENSINO)) %>%
      ungroup() %>% 
      mutate(data_nascimento = paste(ano_nascimento,mes_nascimento,dia_nascimento,sep = "-") %>% lubridate::ymd()) %>% 
      select(-ano_nascimento,-mes_nascimento,-dia_nascimento)
  }
  if(k>2007){
    doc[[i]] <- read_docentes_zip(as.character(k)) %>%
      filter(FK_COD_ESCOLARIDADE%in%c(3,4,5)) %>%
      filter(ID_TIPO_DOCENTE==1) %>% # Somente docentes # Mudou para 1 e 2
      mutate(ID_FUNDAMENTOS_EDUCACAO = 0,
             ID_OUTRAS_DISCIPLINAS_PEDAG = 0) %>% 
      group_by(FK_COD_DOCENTE) %>% 
      summarise(n_turmas_que_da_aula_2008 = n(),
                n_escolas_da_aula_2008 = length(table(PK_COD_ENTIDADE)),
                sexo =  mean(if_else(TP_SEXO=="M",1,0)),
                raca_cor = mean(TP_COR_RACA),
                cod_municipio_residencia_2008 = mean(FK_COD_MUNICIPIO_DEND),
                dia_nascimento = mean(NU_DIA),
                mes_nascimento = mean(NU_MES),
                ano_nascimento = mean(NU_ANO),
                disciplinas_que_da_aula_2008 = identifica_disciplinas(sum(ID_QUIMICA,na.rm = TRUE),
                                                                      sum(ID_FISICA,na.rm = TRUE),
                                                                      sum(ID_MATEMATICA,na.rm = TRUE),
                                                                      sum(ID_BIOLOGIA,na.rm = TRUE),
                                                                      sum(ID_CIENCIAS,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_PORTUGUESA,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_INGLES,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_ESPANHOL,na.rm = TRUE),
                                                                      sum(ID_LINGUA_LITERAT_OUTRA,na.rm = TRUE),
                                                                      sum(ID_ARTES,na.rm = TRUE),
                                                                      sum(ID_EDUCACAO_FISICA,na.rm = TRUE),
                                                                      sum(ID_HISTORIA,na.rm = TRUE),
                                                                      sum(ID_GEOGRAFIA,na.rm = TRUE),
                                                                      sum(ID_FILOSOFIA,na.rm = TRUE),
                                                                      sum(ID_ESTUDOS_SOCIAIS,na.rm = TRUE),
                                                                      sum(ID_INFORMATICA_COMPUTACAO,na.rm = TRUE),
                                                                      sum(ID_PROFISSIONALIZANTE,na.rm = TRUE),
                                                                      sum(ID_FUNDAMENTOS_EDUCACAO,na.rm = TRUE),
                                                                      sum(ID_DISC_ATENDIMENTO_ESPECIAIS,na.rm = TRUE),
                                                                      sum(ID_DISC_DIVERSIDADE_SOCIO_CULT,na.rm = TRUE),
                                                                      sum(ID_OUTRAS_DISCIPLINAS_PEDAG,na.rm = TRUE),
                                                                      sum(ID_LIBRAS,na.rm = TRUE),
                                                                      sum(ID_OUTRAS_DISCIPLINAS,na.rm = TRUE),
                                                                      sum(ID_ESPECIALIZACAO,na.rm = TRUE),
                                                                      sum(ID_MESTRADO,na.rm = TRUE),
                                                                      sum(ID_DOUTORADO,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_CRECHE,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_PRE_ESCOLA,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_NEC_ESP,na.rm = TRUE),
                                                                      sum(ID_ESPECIFICO_ED_INDIGENA,na.rm = TRUE),
                                                                      sum(ID_INTERCULTURAL_OUTROS,na.rm = TRUE)),
                modalidade_ensino_2008 = identifica_modalidade_ensino(FK_COD_MOD_ENSINO),
                dependencia_adm_escola_2008 = identifica_dependencia_adm_escola(ID_DEPENDENCIA_ADM),
                categoria_escola_privada_2008 = if_else(str_detect(dependencia_adm_escola_2008,"privada"),
                                                        identifica_categoria_escola_privada(DESC_CATEGORIA_ESCOLA_PRIVADA),
                                                        "nao se aplica"),
                localizacao_escola_2008 = identifica_localizacao_escola(ID_LOCALIZACAO),
                etapa_ensino_2008 = identifica_etapa_ensino(FK_COD_ETAPA_ENSINO)) %>%
      ungroup() %>% 
      mutate(data_nascimento = paste(ano_nascimento,mes_nascimento,dia_nascimento,sep = "-") %>% lubridate::ymd()) %>% 
      select(-ano_nascimento,-mes_nascimento,-dia_nascimento)
    ############
    if(k%in%c(2012,2013)){
      doc[[i]] <- read_docentes_zip(as.character(k)) %>%
        filter(FK_COD_ESCOLARIDADE%in%c(3,4,5)) %>%
        filter(ID_TIPO_DOCENTE==1) %>% # Somente docentes # Mudou para 1 e 2
        mutate(ID_FUNDAMENTOS_EDUCACAO = 0,
               ID_OUTRAS_DISCIPLINAS_PEDAG = 0,
               ID_INTERCULTURAL_OUTROS = 0) %>% # Nao tinha ID_INTERCULTURAL_OUTROS 
        group_by(FK_COD_DOCENTE) %>% 
        summarise(n_turmas_que_da_aula_2012 = n(),
                  n_escolas_da_aula_2012 = length(table(PK_COD_ENTIDADE)),
                  sexo =  mean(if_else(TP_SEXO=="M",1,0)),
                  raca_cor = mean(TP_COR_RACA),
                  cod_municipio_residencia_2012 = mean(FK_COD_MUNICIPIO_DEND),
                  dia_nascimento = mean(NU_DIA),
                  mes_nascimento = mean(NU_MES),
                  ano_nascimento = mean(NU_ANO),
                  disciplinas_que_da_aula_2012 = identifica_disciplinas(sum(ID_QUIMICA,na.rm = TRUE),
                                                                        sum(ID_FISICA,na.rm = TRUE),
                                                                        sum(ID_MATEMATICA,na.rm = TRUE),
                                                                        sum(ID_BIOLOGIA,na.rm = TRUE),
                                                                        sum(ID_CIENCIAS,na.rm = TRUE),
                                                                        sum(ID_LINGUA_LITERAT_PORTUGUESA,na.rm = TRUE),
                                                                        sum(ID_LINGUA_LITERAT_INGLES,na.rm = TRUE),
                                                                        sum(ID_LINGUA_LITERAT_ESPANHOL,na.rm = TRUE),
                                                                        sum(ID_LINGUA_LITERAT_OUTRA,na.rm = TRUE),
                                                                        sum(ID_ARTES,na.rm = TRUE),
                                                                        sum(ID_EDUCACAO_FISICA,na.rm = TRUE),
                                                                        sum(ID_HISTORIA,na.rm = TRUE),
                                                                        sum(ID_GEOGRAFIA,na.rm = TRUE),
                                                                        sum(ID_FILOSOFIA,na.rm = TRUE),
                                                                        sum(ID_ESTUDOS_SOCIAIS,na.rm = TRUE),
                                                                        sum(ID_INFORMATICA_COMPUTACAO,na.rm = TRUE),
                                                                        sum(ID_PROFISSIONALIZANTE,na.rm = TRUE),
                                                                        sum(ID_FUNDAMENTOS_EDUCACAO,na.rm = TRUE),
                                                                        sum(ID_DISC_ATENDIMENTO_ESPECIAIS,na.rm = TRUE),
                                                                        sum(ID_DISC_DIVERSIDADE_SOCIO_CULT,na.rm = TRUE),
                                                                        sum(ID_OUTRAS_DISCIPLINAS_PEDAG,na.rm = TRUE),
                                                                        sum(ID_LIBRAS,na.rm = TRUE),
                                                                        sum(ID_OUTRAS_DISCIPLINAS,na.rm = TRUE),
                                                                        sum(ID_ESPECIALIZACAO,na.rm = TRUE),
                                                                        sum(ID_MESTRADO,na.rm = TRUE),
                                                                        sum(ID_DOUTORADO,na.rm = TRUE),
                                                                        sum(ID_ESPECIFICO_CRECHE,na.rm = TRUE),
                                                                        sum(ID_ESPECIFICO_PRE_ESCOLA,na.rm = TRUE),
                                                                        sum(ID_ESPECIFICO_NEC_ESP,na.rm = TRUE),
                                                                        sum(ID_ESPECIFICO_ED_INDIGENA,na.rm = TRUE),
                                                                        sum(ID_INTERCULTURAL_OUTROS,na.rm = TRUE)),
                  modalidade_ensino_2012 = identifica_modalidade_ensino(FK_COD_MOD_ENSINO),
                  dependencia_adm_escola_2012 = identifica_dependencia_adm_escola(ID_DEPENDENCIA_ADM),
                  categoria_escola_privada_2012 = if_else(str_detect(dependencia_adm_escola_2012,"privada"),
                                                          identifica_categoria_escola_privada(DESC_CATEGORIA_ESCOLA_PRIVADA),
                                                          "nao se aplica"),
                  localizacao_escola_2012 = identifica_localizacao_escola(ID_LOCALIZACAO),
                  etapa_ensino_2012 = identifica_etapa_ensino(FK_COD_ETAPA_ENSINO)) %>%
        ungroup() %>% 
        mutate(data_nascimento = paste(ano_nascimento,mes_nascimento,dia_nascimento,sep = "-") %>% lubridate::ymd()) %>% 
        select(-ano_nascimento,-mes_nascimento,-dia_nascimento)

    }###############      
  }
  k <- k + 1
}
