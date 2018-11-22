#' Create a file with specie name and specie ID from SPECIES platform.
#' 
#' Date: 18/11/21
#' Author: Juan M Barrios <juan.barrios@conabio.gob.mx>
library(here)
library(tidyverse)

# Auxiliary functions ----

#' Basic function to call diffrent actions on SPECIES API
#'
#' @param action Name of the action to call on SPECIES API
#' @param action_body List of parameters need it to call action
#' 
#' @return A response from SPECIES API
species_api <- function(action, action_body = list()) {
  species_baseurl <- 'http://species.conabio.gob.mx/api/niche/'
  api_path <- 'api/niche'
  url <- httr::modify_url(species_baseurl, path = file.path(api_path,
                                                            action, fsep = '/'))
  
  httr::POST(url, 
             body = action_body,
             encode = 'json')
}

#' Function to get a spid from a specie scientific name
#' 
#' @param spScientificName Specie scientific name
#' 
#' @return Scientific name spid
get_spid <- function(spScientificName) {
  if (!is.character(spScientificName) || 
      length(spScientificName) > 1){
    stop('spScientificName must be a string')
  }
  
  query <- list(
    limit = 'true',
    searchStr = spScientificName,
    source = 1
    )
  response <- species_api('especie/getEntList',
                          query)
  
  if (httr::status_code(response) == 200 &
      httr::http_type(response) == "application/json") {
    responseData <- jsonlite::fromJSON(httr::content(response, 
                                                     "text"),
                                       simplifyVector = FALSE)
  }
  
  # Match with species and subspecies
  species_with_spid <- responseData[['data']] %>% 
    bind_rows()
  
  if (nrow(species_with_spid) < 1) {
    return(NA)
  }
  
  return(pull(species_with_spid, 'spid')[1])
}

# Load species list ----
species_data_list <- readxl::read_xlsx(here('00-data', 
                                            'splistcor_Bedoya_092018.xlsx'))
species_data_list <- species_data_list %>% 
  filter(Estado_revision != 0) %>%
  select(scientificName = Nombre_fuente, 
         acceptedNameUsage = Nombre_corr,
         taxonomicStatus = Estado_revision,
         taxonRemarks = C_Bedoya)

# Find specie on SPECIES Platform and assign spid ----
species_data_list <- species_data_list %>% 
  mutate(spid = map_int(scientificName, get_spid))

# Write output file
species_data_list %>% write_csv(path = here("02-preprocessed_data_files", 
                                            "species_data_with_spid.csv"))
