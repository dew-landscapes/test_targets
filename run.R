
library(targets)

envFunc::check_packages(yaml::read_yaml("settings/packages.yaml")$packages
                        , update_env = TRUE
                        )

tars <- envTargets::make_tars()

envTargets::write_tars(tars)

# clean up? -------
if(FALSE) {
  
  # PRUNE
  purrr::walk2(purrr::map(tars, "script")
               , purrr::map(tars, "store")
               , \(x, y) targets::tar_prune(script = x, store = y)
               )
  
}

# make everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars, "script")
             , purrr::map(tars, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
             )

if(FALSE) {
  
  # individual tar_make-------
  
  script <- "targets"
  
  tar_visnetwork(script = tars[[script]]$script
                 , store = tars[[script]]$store
                 , label = "time"
                 )
  
  # tar_invalidate(report, store = tars[[script]]$store)
  
  tar_make(script = tars[[script]]$script
           , store = tars[[script]]$store
           )
  
  tar_prune(script = tars[[script]]$script
            , store = tars[[script]]$store
  )
  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars[[script]]$store)
  tar_meta(fields = any_of("warnings"), complete_only = TRUE, store = tars[[script]]$store)
  
  ## check logs ------
  log_directory <- fs::path(tars[[script]]$store, "log")
  
  fs::dir_ls(log_directory) |>
    purrr::map(readLines) |>
    purrr::map(\(x) tibble::enframe(x, name = NULL, value = "line")) |>
    dplyr::bind_rows(.id = "file") |>
    dplyr::filter(grepl("error", tolower(line)))
  
}
