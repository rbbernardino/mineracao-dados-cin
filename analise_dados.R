## install.packages("data.table")
## install.packages("readxl")

## library(readxl)
## library(data.table)
library(tidyverse)

source("criacao_variaveis.R")
source("utils.R")

source("analyse_2007.R")
source("analyse_other_years.R")
source("analyse_extra_2012_13.R")

##############################################################################
# df = read_docentes_zip("2009")
# print(df)
## head(tab_etapa,10)
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
  if(k==2007) {
    analyse_2007(doc, k)
  }
  if(k>2007) {
    analyse_other_years(doc, k)
    if(k%in%c(2012,2013)) {
      analyse_extra_2012_13(doc, k)
    }
  }

  k <- k + 1
}
