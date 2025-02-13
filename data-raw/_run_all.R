# Run all files in data-raw

files <- list.files("data-raw")
files <- files[!stringr::str_detect(files, "_run_all")]

purrr::walk(glue::glue("data-raw/{files}"),
           source)

#save internal only datasets
usethis::use_data(anzsco_dictionary, anzsic_dictionary,
                  state_dict, state_table,
                  internal = TRUE, overwrite = TRUE)
