#' Script to generate a set of files with the occurrences for each specie
#' given in a list. 
#' 
#' It expects a csv with two columns, one for species scientific name and
#' the second with specie id on SPECIES platform.
#' 
#' Date: 18/11/21
#' Author: Juan M Barrios <juan.barrios@conabio.gob.mx>

library(here)
library(tidyverse)

# Auxiliary functions ----

#' Check if a number is integer
#' 
#' @param x Number to check
#' @param tol 
#' 
#' @return TRUE if x is an integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

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

#' Function to get ocurrences for spid
#' 
#' @param spid Specie spid
#' 
#' @return A tibble with urlejemplar, decimalLatitude, and 
#'         decimaLongitude
get_ocurrences <- function(spid) {
  if (length(spid) > 1) {
    stop('spid must be a integer')
  }
  
  if (!is.wholenumber(spid)) {
    stop('spid must be a integer')
  }
  
  query <- list(
    grid_res = 16,
    id = spid,
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
    map('coordinates')
  species_occurrence_sf <- species_occurrence_sf %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as_tibble()
  colnames(species_occurrence_sf) <- c('decimalLongitude',
                                       'decimalLatitude')
  
  # create dataset with geometry
  species_occurrences <- species_occurrences %>% 
    select(urlejemplar) %>%
    bind_cols(species_occurrence_sf)

  if (nrow(species_occurrences) < 1) {
    return(NA)
  }
  
  return(species_occurrences)
}

#' Save occurrences from specie name and data
#'
#' @param sp_name Specie scientific name
#' @param occ_data Data frame with the species data
#' 
#' @return none
save_occurrences_files <- function(sp_name, occ_data, out_dir) {
  # Make valid name
  sp_name <- sp_name %>% 
    str_to_lower() %>% 
    str_trim() %>% 
    str_replace('\\s', '_')
  
  dir.create(out_dir, showWarnings = FALSE)
  out_file <- file.path(out_dir,
                        str_c(sp_name,"_occurrences.csv"))
  
  occ_data %>%
    write_csv(out_file)
}

# Load data files ----
species_list <- read_csv(here("02-preprocessed_data_files", 
                              "species_data_with_spid.csv"))

# Processing data ----
species_occurrences_nested <- species_list %>% 
  mutate(occurrences = map(spid, get_ocurrences)) 

out_dir <- here("02-preprocessed_data_files", "occurrences_files")
data_to_save <- species_occurrences_nested %>% 
  select(scientificName, occurrences) 

# Writing data
map2(data_to_save$scientificName, 
     data_to_save$occurrences, 
     save_occurrences_files, out_dir=out_dir)
