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
