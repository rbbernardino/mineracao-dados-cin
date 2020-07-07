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
