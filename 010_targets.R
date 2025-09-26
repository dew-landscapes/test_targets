library(targets)
library(tarchetypes)
library(crew)

# cores --------
use_cores <- floor(parallel::detectCores() * 0.95)

# target options --------
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml") |> unlist() |> unname() |> unique()
               , controller = crew_controller_local(workers = use_cores
                                                    , seconds_interval = 4
                                                    )
               )

download_zip_shp <- function(url, out_dir) {
  fs::dir_create(out_dir)
  dest_file <- fs::path(out_dir, basename(url))
  download.file(url, dest_file)
  unzip(dest_file, exdir = out_dir)
  return(out_dir)
}

tars <- yaml::read_yaml("_targets.yaml")

list(
  tar_target(taxa
             , tibble::tibble(taxa = unique(stringi::stri_rand_strings(n = 30000, length = 4, pattern = "[A-Za-z]"))) %>%
               dplyr::mutate(n = sample(1:20000, nrow(.), replace = TRUE)
                             , group = rep_len(1:use_cores, length.out = nrow(.))
                             , mcp_file = fs::path(tars$targets$store, "files", paste0("mcp_", taxa, ".parquet"))
                             , clip = sample(c("terr", "not_terr")
                                             , nrow(.)
                                             , replace = TRUE
                                             , prob = c(0.75, 0.25)
                                             )
                             ) |>
               dplyr::group_by(group) |>
               targets::tar_group()
             , iteration = "group"
             )
  , tar_target(aus_zip_url
               , "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/AUS_2021_AUST_SHP_GDA2020.zip"
               , format = "url"
               )
  , tar_target(aus_zip
               , download_zip_shp(aus_zip_url, "data")
               , format = "file"
               )
  , tar_target(terr
               , sf::st_read(fs::path(aus_zip, "AUS_2021_AUST_GDA2020.shp")) |>
                 dplyr::filter(AUS_CODE21 == "AUS")
               )
  , tar_target(not_terr
               , sf::st_read(fs::path(aus_zip, "AUS_2021_AUST_GDA2020.shp")) |>
                 dplyr::filter(AUS_CODE21 != "AUS")
               )
  , tar_target(points, runif(1e6))
  , tar_target(points_x, points * 2000000)
  , tar_target(points_y, sample(points) * 3000000)
  , tar_target(coords
               , taxa %>%
                 dplyr::mutate(coords = purrr::map(n
                                                   , \(z) tibble::tibble(x = sample(points_x, z)
                                                                         , y = sample(points_y, z)
                                                                         ) |>
                                                     dplyr::distinct()
                                                   )
                               )
               , pattern = map(taxa)
               )
  , tar_target(mcp
               , coords |>
                 dplyr::mutate(mcp = purrr::pmap(list(mcp_file
                                                      , coords
                                                      , clip
                                                      )
                                                 , \(a, b, c) {
                                                   
                                                   envDistribution::make_mcp(presence = b
                                                                             , out_file = a
                                                                             , force_new = FALSE
                                                                             , pres_x = "x"
                                                                             , pres_y = "y"
                                                                             , in_crs = 8059
                                                                             , out_crs = 8059
                                                                             , buf = 0
                                                                             , clip = if(c == "terr") terr else not_terr
                                                                             )
                                                   
                                                 }
                                                 )
                               )
               , pattern = map(coords)
               )
  )
