#' Also add complete information from a SNIB file.
#' 
#' Expect a file downloaded from http://www.snib.mx
#' 
#' Date: 18/11/21
#' Author: Juan M Barrios <juan.barrios@conabio.gob.mx>

library(here)
library(fs)
library(tidyverse)

# Auxiliary functions ----

#' Add SNIB extra info to occurrence file
#' 
#' @param occurrence_fp Path to occurrence file, must have urlejemplar column
#' @param snib_data SNIB data
#' 
#' @return none
attach_snib_data <- function(occurrence_fp, snib_data) {
  occ_data <- read_csv(occurrence_fp)
  
  occ_data <- occ_data %>%
    left_join(snib_data)
  
  write_csv(occ_data, occurrence_fp)
}

# Setup workspace ----
snib_data <- read_csv(here("00-data", 
                           "SNIBEjemplares_20181122_095605.zip"))
# Directory where occurrences files are
occurrences_dir <- here("02-preprocessed_data_files", 
                        "occurrences_files")

# Processing ----
occurrences_files <- dir_ls(occurrences_dir, glob = "*.csv")

info_snib_data <- snib_data %>% select(
  idejemplar, 
  # llavenombre, 
  ordenvalido, 
  # sistemaclasificacionfamiliavalido, 
  genero, 
  generovalido, 
  # sistemaclasificaciongenerocatvalido, 
  # autoraniogenerocatvalido, 
  estatustax, 
  # epitetoespecificooriginal, 
  # epitetoespecificocatvalido, 
  # catdiccespeciecatvalido, 
  # autoranioespecieoriginal, 
  # autoranioespeciecatvalido, 
  autor,
  autorvalido,
  # siglascoleccion, 
  coleccion, 
  # nombrecoleccion_1, 
  # siglasinstitucion, 
  institucion, 
  paiscoleccion, 
  ambiente, 
  # ambientenombre, 
  validacionambiente,
  localidad, 
  # observacionesejemplar, 
  subgrupobio, 
  grupobio, 
  # paisoriginal, 
  paismapa, 
  # estadooriginal, 
  estadomapa, 
  altitudmapa, 
  datum, 
  # latitud, 
  # longitud, 
  numcatalogo, 
  numcolecta, 
  procedenciaejemplar, 
  # altitudinicialejemplar, 
  determinador, 
  colector, 
  aniocolecta, 
  mescolecta, 
  diacolecta, 
  urlejemplar
)

# Update occurrences files
occurrences_files %>% 
  map(attach_snib_data, snib_data = info_snib_data)
