library(tidyverse)

all_rmd <- list.files(path = ".",
                      recursive = TRUE,
                      full.names = TRUE,
                      pattern = ".Rmd")


## dummy:
path = all_rmd

get_rmd_dependencies <- function(path){

  reqlibs = sub(".*library\\(\"(.*?)\"\\).*","\\1",
              grep("library\\(", 
                   readLines(path),
                   value=TRUE))
  
  reqlibs <- stringr::str_replace_all(string = reqlibs,
                             pattern = "library\\(", 
                             replacement = "")
  reqlibs <- stringr::str_replace_all(string = reqlibs,
                             pattern = "\\)", 
                             replacement = "")
  
  reqlibs <- stringr::str_replace_all(string = reqlibs,
                                      pattern = "#", 
                                      replacement = "")
  
  reqlibs <- stringr::str_replace_all(string = reqlibs,
                                      pattern = " ", 
                                      replacement = "")
  
  reqlibs <- stringr::str_replace_all(string = reqlibs,
                                      pattern = "`", 
                                      replacement = "")
  
  reqlibs
  
 
}

purrr::map(all_rmd, get_rmd_dependencies)
packages <- map(as.list(all_rmd), get_rmd_dependencies) %>% 
  unlist() %>% 
  unique() %>%
  trimws()

packages


install_sytem_wide <- function(package_name){
  
  pacman::p_install(package = package_name, lib = .libPaths()[2])

}

## Login as sudo root user and start R



  
  

