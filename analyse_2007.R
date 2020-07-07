#' processa isso e aquilo etc....
#'
#' @param doc list | data.frame com dados DOCENTES brutos
analyse_2007 <- function(doc, k) {
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
