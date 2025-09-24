library(targets)
library(tarchetypes)
library(crew)

# cores --------
use_cores <- floor(parallel::detectCores() * 0.95)

# target options --------
tar_option_set(packages = c("magrittr", "sf")
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

list(
  tar_target(taxa
             , tibble::tibble(taxa = unique(stringi::stri_rand_strings(n = 20000, length = 4, pattern = "[A-Za-z]"))) %>%
               dplyr::mutate(n = sample(3:20000, nrow(.))
                             , group = rep_len(1:use_cores, length.out = nrow(.))
                             , clip = sample(c("st_intersection", "st_difference")
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
  , tar_target(aus
               , sf::st_read(fs::path(aus_zip, "AUS_2021_AUST_GDA2020.shp")) |>
                 dplyr::filter(AUS_CODE21 == "AUS")
               )
  , tar_target(points, runif(1e6))
  , tar_target(points_x, points * 2000000)
  , tar_target(points_y, sample(points) * 3000000)
  , tar_target(coords
               , taxa %>%
                 dplyr::mutate(coords = purrr::map(n
                                                   , \(z) tibble::tibble(x = sample(points_x, z)
                                                                         , y = sample(points_y, z)
                                                                         )
                                                   )
                               )
               , pattern = map(taxa)
               )
  , tar_target(points_sf
               , coords |>
                 dplyr::mutate(points_sf = purrr::map(coords
                                                      , \(x) sf::st_as_sf(x = x, coords = c("x", "y"), crs = 8059) |>
                                                        dplyr::summarise() |>
                                                        sf::st_transform(crs = sf::st_crs(aus))
                                                      )
                               ) 
               , pattern = map(coords)
               )
  , tar_target(mcp
               , points_sf |>
                 dplyr::mutate(mcp = purrr::pmap(list(taxa
                                                      , points_sf
                                                      , clip
                                                      )
                                                 , \(a, b, c) {
                                                   mcp <- sf::st_convex_hull(b) |>
                                                     terra::vect() |>
                                                     terra::densify(50000) |>
                                                     sf::st_as_sf() |>
                                                     get(c)(y = aus)
                                                   
                                                   out_file <- fs::path("files", paste0(a, ".parquet"))
                                                   
                                                   sfarrow::st_write_parquet(mcp
                                                                             , out_file
                                                                             )
                                                   
                                                   return(out_file)
                                                   
                                                 }
                                                 )
                               )
               , pattern = map(points_sf)
               )
  )