## renders all Rmd in the current folder
own_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
rmd_files <- list.files(path = own_dir, pattern = "\\.Rmd", 
                        full.names = TRUE)
purrr::map(rmd_files, rmarkdown::render)

