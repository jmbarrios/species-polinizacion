#' Basic functionalities for SPECIES platform API
#' 
#' In this script we will present some basic examples on how to interact with
#' SPECIES platform API from R.
#' 
#' Date: 18/11/08
#' Author: Juan M Barrios <juan.barrios@conabio.gob.mx>

library(tidyverse)
library(sf)

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

# Find species included on SPECIES given a genus ----
# Find species from Salvia genus
query <- list(
  field = 'especievalidabusqueda',
  parentfield = 'generovalido',
  parentitem = 'Salvia' # Change with differente genus
)

response <- species_api('especie/getVariables',
                        query)
if (httr::status_code(response) == 200 &
    httr::http_type(response) == "application/json") {
  responseData <- jsonlite::fromJSON(httr::content(response, 
                                                   "text"),
                                     simplifyVector = FALSE)
}

species_list <- responseData[['data']] %>%
  map_chr(~.[['name']]) %>%
  list(scientificName = .)

# Save species list as csv
species_list %>% 
  as_tibble() %>% 
  write_csv('species_list.csv')

# Find spid (species ID) given a name ----
query <- list(
  limit = 'true',
  searchStr = 'Salvia hintonii', # change with a different scientific name
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

# Download all the observations for a given specie ----
idSpecie <- 63761  # occurrences for Salvia hintonii
query <- list(
  grid_res = 16,
  id = idSpecie,
  idtime = 1541701790375,
  sfecha = 'true',
  sfosil = 'true'
)

response <- species_api('especie/getSpecies', 
                        query)

if (httr::status_code(response) == 200 &
    httr::http_type(response) == "application/json") {
  responseData <- jsonlite::fromJSON(httr::content(response, 
                                                   "text"),
                                     simplifyVector = FALSE)
}
species_occurrences <- responseData[['data']] %>% 
  bind_rows() 

# create point geometries from response data
species_occurrence_sf <- species_occurrences %>% 
  pull(json_geom) %>% 
  map(jsonlite::fromJSON) %>% 
  map(~st_point(.x$coordinates)) %>%
  st_sfc()

# create dataset with geometry
species_occurrences <- species_occurrences %>% 
  st_sf(., species_occurrence_sf) %>% 
  select(-json_geom)

# Save the occurrences as csv
species_occurrences %>% st_write(dsn = paste0(idSpecie,"_occurrence_data.csv"),
         layer_options = "GEOMETRY=AS_XY",
         delete_dsn = TRUE)
# Save the occurrences as Shapefile
species_occurrences %>% st_write(paste0(idSpecie,"_occurrence_data.shp"))
