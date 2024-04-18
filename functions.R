fp <- file.path

mcet_check_installations <- function(stop_if_not = TRUE, autoinstall = FALSE) {
  status <- TRUE
  # check if all required packages are installed
  if (packageVersion("energyRt") < "0.11.1") {
    msg <- paste0(
      "Please update energyRt to version 0.11.1 or higher",
      "\n       ",
      "devtools::install_github('energyRt/energyRt', ref = 'dev')"
    )
    if (stop_if_not) {
      stop(msg)
    } else {
      message(msg)
      status <- FALSE
    }
  }

  # and install them if not
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Please install devtools", "\n       ",
         "install.packages('devtools')")
  }

  return(status)
}

# =============================================================================#
#### MERRA data ####

rev_merra_cl_filemask <-
  function(tol, wind = T, solar = !wind, dir = NULL, year = NULL) {
    # browser()WH
    # filemask <- glue("TOL", prettyNum(tol)) %>% str_replace_all("\\.", "")
    stopifnot(tol <= 1 & tol >= 0)
    if (tol == 1) return(paste0("TOL", 99))
    if (tol < .01) FORMAT = "f" else FORMAT = "d"
    # filemask <- glue("TOL", prettyNum(tol)) %>% str_replace_all("\\.", "")
    filemask <- glue("TOL", formatC(tol * 100, width = 2, flag = "0",
                                    format = FORMAT))
    # if ()
    # if (!is.null(year))
    # if (!is.null(dir))
    filemask
  }
if (F) {
  rev_merra_cl_filemask(.01)
  rev_merra_cl_filemask(.1)
  rev_merra_cl_filemask(1)
  rev_merra_cl_filemask(.0001) # reserved but not expected to be used
  rev_merra_cl_filemask(1e-10) # reserved but not expected to be used
  rev_merra_cl_filemask(1.1)

}


make_tech_name <- function(tech, cluster = NULL, cl_sfx = "", na.cluster = "") {
  # (status: interim solution)
  # goal: make names for technologies with clusters
  # `00` indicates "no" or "only one" cluster
  # `na` - NA replacement
  # browser()
  # cluster[is.na(cluster)] <- na
  # ii <- is.na(tech)
  if (!is.null(cluster) && length(unique(cluster[!is.na(cluster)])) > 1) {
    # nnum - number of digital places to indicate clusters, default = 2
    if (max(cluster, na.rm = T) > 99) nnum <- nchar(max(cluster, na.rm = T)) else nnum <- 2
    cl_sfx <- formatC(cluster, digits = 0, width = nnum, flag = "0")
    cl_sfx[grepl("NA$", cl_sfx)] <- na.cluster
  }
  tech_cl <- paste0(tech, cl_sfx)
  tech_cl[is.na(tech)] <- NA
  return(tech_cl)
}

if (F) {
  make_tech_name(rep("ESOL", 3), 1:3)
  make_tech_name(rep("ESOL", 3), rep(1, 3)) # `00` - no/one cluster
  make_tech_name(rep("ESOL", 3), c(1, NA, 1))
  make_tech_name(rep("ESOL", 3), c(1, NA, 1), na.cluster = "00")
  make_tech_name(c("ESOL", NA, "ESOL"), c(1, NA, 1))
  make_tech_name(rep("ESOL", 3), c(1, NA, 1), na = "")
}


# =============================================================================#
# Various function to use in the project
# =============================================================================#

#' Extract, snap_to_grid, and union polygons
#'
#' @description
#' A workaround to improve stability of `st_union` on geometry objects (`sfc`)
#' to use in aggregation of geometries. The function selects only `polygons`
#' type of geometries from `GEOMETRYCOLLECTION`, simplifies ("snaps to grid")
#' and returns aggregated polygon object.
#'
#' @param x geometry object in lon/lat coordinates
#' @param grid_size see `s2::s2_snap_to_grid` for details
#'
#' @return
#' @export
#'
#' @examples
rev_union_polygons <- function(x, grid_size = 1e-5) {
  # workaround to improve stability of aggregation of geometries
  # x - geometry
  # browser()
  ii <- sapply(x, function(g) {
    inherits(g, c("sfc_GEOMETRYCOLLECTION", "GEOMETRYCOLLECTION"))
  })
  if (any(ii)) {
    x <- try({
      suppressWarnings(
        sf::st_collection_extract(x, "POLYGON", warn = F)
      )
    }, silent = T)
    if (inherits(x, "try-error")) x <- sf::st_polygon(list())
  }

  x %>%
    s2::s2_snap_to_grid(grid_size) %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    sf::st_union() %>%
    sf::st_make_valid()
}

#' Suggest values for labeling integer axes
#'
#' @param n integer giving the desired number of intervals
#' @param ... further arguments for `base::pretty`
#'
#' @return
#' @export
#'
#' @examples
rev_integer_breaks <- function(n = 5, must_have = NULL, ...) {
  n_default <- n
  function(x, n = n_default) {
    breaks <- pretty(x, n, ...)
    # if (!is.null(must_have)) breaks <- c(must_have, breaks)
    breaks <- as.integer(c(must_have, breaks)) %>% unique() %>% sort()
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
}

## Translation functions ####
#' Translate names of `buses` in `PyPSA` to `regions` in `energyRt`
#' @description
#' `energyRt` is using names conversion suitable for several modeling languages
#' (R, GAMS, Python, Julia, GLPK/MathProg), therefore names in sets (such as `regions`)
#' cannot include white-spaces or any special characters. This function provides
#' a set of rules to map (or rename) `buses` to universal format, using
#' capital letters (not required) and
#' This is an ad-hoc function for a particular model(s) and project(s).
#' Adjustments might be required in the case of changes in the model(s).#'
#' @param x
#' @param num_width
#' @param sep
#' @param mod
#'
#' @return
#' @export
#'
#' @examples
rev_bus2reg <- function(x, num_width = 2, sep = "", mod = "mod_toy") {
  # browser()
  if (grepl("toy", mod)) { # toy model
    NN <- str_replace_all(x, " ", "") |> str_extract("[0-9]+$")
    # str_replace_all(x, "[A-Za-z ]+", "")
    # TXT <- str_replace(x, NN, "") %>% str_trim() %>% toupper()
    TXT <- str_extract_all(x, "[A-Za-z]+") |> toupper()
    if (any(grepl("[0-9]", TXT)))
      stop("Unrecognized (non-ending or multiple) numeric indexes in names")
    NN <- formatC(as.integer(NN), width = num_width, flag = "0", format = "d")
    return(paste(TXT, NN, sep = sep))
  } else if (grepl("adm", mod)) { # administrative regions
    # stop("Not implemented yet for other than `mod_toy` cases")
    rg_mp <- read_fst(fp(p$dir$data_mod, "reg_nodes.fst")) %>%
      select(bus:load_zones)
    d <- data.table(bus = x) %>% left_join(rg_mp, by = "bus")
    return(d$region)
  }
}

rev_reg2bus <- function(x, map = NULL) {
  # str_replace_all(x, " |-", sep)
}

#' Replace special characters in names
#'
#'@description a wrapper for `str_replace_all` replaceing all special characters by default.
#' @param x character vector with names potentially have characters to replace.
#' @param pattern pattern to replace, default `"[^[:alnum:]]"`
#' @param replacement a character to replace, `"_"` by default.
#'
#' @return character vector with applied replacements.
#' @export
#'
#' @examples
rev_repare_names <- function(x, pattern = "[^[:alnum:]]", replacement = "_") {
  str_replace_all(x, pattern, replacement)
}

#' Translate names of `carriers` in `PyPSA` to `commodities` in `energyRt`
#'
#' @description
#' `energyRt` is using names conversion suitable for several modeling languages
#' (R, GAMS, Python, Julia, GLPK/MathProg), therefore names in sets (such as commodities)
#' cannot include white-spaces or any special characters. This function provides
#' a set of rules to map (or rename) `carriers` to universal format, using
#' capital letters (not required) and
#' This is an ad-hoc function for a particular model(s) and project(s).
#' Adjustments might be required in the case of changes in the model(s).
#'
#' @param x string vector with `carriers` names
#'
#' @return string vector with `commodity` names to use in `energyRt``
#' @export
#'
#' @examples
carriers2comm <- function(x) {
  # mapping carriers to commodities
  x <- toupper(x)
  x[grepl("COA", x, ignore.case = TRUE)] <- "COA"
  x[grepl("OIL", x, ignore.case = TRUE)] <- "OIL"
  x[grepl("CGT", x, ignore.case = TRUE)] <- "GAS"
  x[grepl("WIND", x, ignore.case = TRUE)] <- "WIN"
  x[grepl("SOLAR", x, ignore.case = TRUE)] <- "SOL"
  x[grepl("LIGN", x, ignore.case = TRUE)] <- "LIG"
  x[grepl("BIO", x, ignore.case = TRUE)] <- "BIO"
  x[grepl("NUC", x, ignore.case = TRUE)] <- "NUC"
  x[grepl("GEO", x, ignore.case = TRUE)] <- "GEO"
  x[grepl("ROR", x, ignore.case = TRUE)] <- "ROR"
  x[grepl("PHS", x, ignore.case = TRUE)] <- "PHS"
  x[grepl("HYDRO", x, ignore.case = TRUE)] <- "HYD"
  x[grepl("BATTERY", x, ignore.case = TRUE)] <- "BTR" # or CHARGE (energy stored chemically for conversion into electricity)
  x[grepl("^H2$", x, ignore.case = TRUE)] <- "H2"
  x <- toupper(x)
  x <- str_trim(x)
  x
}

rev_extract_bus <- function(x, bus_names = reg_names$bus, check_length = TRUE) {
  # browser()
  # pat <- paste0(bus_names, collapse = "|")
  bus <- str_extract(x, "[A-Z]++( |)[0-9]++")
  if (check_length) stopifnot(length(bus) == length(x))
  bus
}

#### YAML & settings ####
#' Set country name to use in `rev_*` functions in the current project
#'
#' @param iso3c (character) country `ISO alpha-3` code
#'
#' @return R option `rev.iso3c` is set to the given value.
#' @export
#'
#' @examples
rev_set_country <- function(iso3c = params$iso3c, verbose = TRUE) {
  stopifnot(is.character(iso3c))
  stopifnot(length(iso3c) == 1)
  cn <- countrycode::countrycode(iso3c, "iso3c", "country.name", warn = F)
  if (is.na(cn)) stop("Unrecognized 'iso3c' code '", iso3c,
                      "'. See `?countrycode::countrycode` for details.")
  options(rev.iso3c = iso3c)
  # if (verbose) cat("Option 'rev.iso3c' set to '", iso3c, "'.", sep = "")
  if (verbose) cat("Country code set to '", iso3c, "' (", cn, ")", sep = "")
  return(invisible(TRUE))
}

if (F) {
  rev_set_country("CHN")
  rev_country()
  rev_set_country("BGD")
  rev_country()
  rev_set_country("BG")
  rev_country()
}

#' Read `rev.iso3c` option
#'
#' @description a wrapper for `options` function to read (preset) isocode of the current country.
#' @return (character)
#' @export
#'
#' @examples
rev_country <- function() {
  options()$rev.iso3c
}


#' Read the project yaml file
#'
#' @param iso3c country isocode alpha-2
#' @param path path to directory to save the configuration file
#' @param filename name of the file (default config_`iso3c`.yml)
#' @param raw logical, should the read yaml file be return without pre-processing (this includes merging paths for known directories of the project), FALSE by default.
#' @param projdir full path of the project; will be used as default path for not-provided path.
#'
#' @return list with yaml parameters
#' @export
#'
#' @examples
rev_read_yaml <- function(iso3c = NULL, path = NULL,
                          filename = paste0("config_", iso3c, ".yml"),
                          projdir = getwd(),
                          make_dirs = TRUE,
                          raw = FALSE) {
  # browser()
  if (is.null(iso3c)) iso3c <- rev_country()
  if (is.null(iso3c)) stop("Country code ", iso3c, " was not found.")
  if (!is.null(path)) {
    FILE <- file.path(path, filename)
  } else {
    FILE <- filename
  }
  p <- yaml::read_yaml(FILE)
  if (raw) {
    return(p)
  }

  p$dir <- lapply(p$dir, function(x) {unlist(x) %>% paste(collapse = "/")})
  p$files <- lapply(p$files, function(x) {unlist(x) %>% paste(collapse = "/")})

  if (make_dirs) rev_make_project_dirs(p)
  p
}

rev_get_gisfile <- function(
    iso3c = NULL,
    yaml_dir = "config",
    mod_shape = "mod_toy",
    filename = paste0("config_", iso3c, ".yml")
    ) {
  # browser()
  fl <- fp(yaml_dir, mod_shape, filename)
  if (!file.exists(fl)) {stop("File not found: ", fl)}

  fl <- rev_read_yaml(filename = fl, make_dirs = F)$files$gis
  if (!file.exists(fl)) {stop("File not found: ", fl)}

  ob <- load(fl)
  return(get(ob)$mod_map$sf)
  # gis
}

if (F) {
  rev_get_gisfile("VNM")
  p <- rev_read_yaml(paste0("config_COL.yml"))
  p
}

#' Create directories for the project
#'
#' @param prm list list with the projects' parameters (from yaml)
#'
#' @return checked/created directories
#' @export
#'
#' @examples
rev_make_project_dirs <- function(prm) {
  for (i in 1:length(prm$dir)) {
    if (!dir.exists(prm$dir[[i]])) {
      message("Creating directory: ", prm$dir[[i]])
      dir.create(prm$dir[[i]], recursive = T, showWarnings = T)
    } else {
      cat('"', prm$dir[[i]], '"\n', sep = "")
    }
  }
}

#### Figures / reporting ####
#' ggplot2 theme used in the project for maps
#'
#' @return ggplot theme object
#' @export
#'
#' @examples
rev_theme_map <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))
}

rev_theme_map2 <- function() {
  # ggplot2::theme_bw() +
  ggthemes::theme_map() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))
}


rev_theme_void <- function() {
  ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))
}

theme_no_ticks <- function() {
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
}

#### MCET scenarios ####
# Make MCET scenarios routine for an existing Switch model

make_mcet_scenarios_queue_1 <- function(
  ## Point to the Switch-model directory
  # switch_dir = "scenarios/ISO-test/mod_toy/scen_pypsa_2050_TOL99/switch",
  switch_dir,
  # point to Switch-MCET-modules
  copy_mcet_modules_from = "switch_modules/mcet_modules",
  mcet_modules_location = "switch_modules/mcet_modules",
  hydrogen_module = FALSE, # not implemented
  # Pattern or list of thermal generators to "mute" for new investments
  thermal_techs_pattern = "ECOA|EGAS|EOCG|EOIL|ECCG|ELIG|_CCS",
  fossil_fuels_pattern = "COA|GAS|OIL|CCG|OCG|LIG|CCS",
  H2_techs_pattern = "STG_H2",
  H2_pattern = "^H2$",
  base_year = 2050,
  # modules_file
  modules_file = "inputs/modules.txt",
  options_file = "options.txt",
  scenarios_file = "scenarios.txt",
  modules_default = c(
    "# Core Modules",
    "switch_model",
    "switch_model.timescales",
    "switch_model.financials",
    "switch_model.balancing.load_zones",
    "switch_model.energy_sources.properties",
    "switch_model.generators.core.build",
    "switch_model.generators.core.dispatch",
    "switch_model.reporting",
    "# Custom Modules",
    "# switch_model.transmission.local_td",
    "switch_model.generators.core.no_commit",
    "# switch_model.energy_sources.fuel_costs.markets",
    "switch_model.energy_sources.fuel_costs.simple",
    "switch_model.transmission.transport.build",
    "switch_model.transmission.transport.dispatch",
    "switch_model.balancing.unserved_load",
    "switch_model.generators.extensions.storage",
    "switch_model.policies.carbon_policies"
  ),
  # modules_mcet = c(
  #   "mcet_modules.dates",
  #   "mcet_modules.flexible_loads",
  #   "mcet_modules.new_transmission_limit"
  # )
  # mcet_scenarios = "All"
  overwrite = TRUE,
  verbose = TRUE
) {
  require("tidyverse")
  require("data.table")
  require("glue")
  fp <- file.path

  fWrt <- function(f, append = FALSE) {
    if (file.exists(f)) {
      if (!overwrite) {
        stop("File ", f, " already exists.\n",
             "use 'overwrite = TRUE' to overwrite it.")
      } else {
        verb <- if_else(append, "Appending", "Overwriting")
      }
    } else {
      verb <- "Writing"
    }
    if (verbose) message(verb, ": ", basename(f))
    return(TRUE)
  }

  append_lines <- function(new_lines, f, unique_only = TRUE) {
    if (file.exists(f)) {
      content <- read_lines(f) |> c(new_lines)
    } else {
      content <- new_lines
    }
    if (anyDuplicated(content)) {
      if (unique_only) {
        warning("Dropping duplicated rows found in ", basename(f))
        content <- unique(content)
      } else {
        warning("Duplicated rows found in ", basename(f))
      }
    }
    write_lines(content, f, append = FALSE)
  }
  # browser()
  if (!is.null(mcet_modules_location)) {
    stopifnot(dir.exists(mcet_modules_location))
    ff <- list.files(fp(mcet_modules_location), pattern = "\\.py$",
                     full.names = TRUE)
    message("Copying ", length(ff), " switch mcet_modules")
    if (!dir.exists(fp(switch_dir, "mcet_modules"))) {
      dir.create(fp(switch_dir, "mcet_modules"), showWarnings = F)
    }
    ff <- sapply(ff, function(x) {
      # cat(f, fp(switch_dir, "mcet_modules"), "\n")
      file.copy(x, fp(switch_dir, "mcet_modules", basename(x)), overwrite = T)
      })
  }

  # check if there are two versions of "modules.txt"
  if (file.exists(fp(switch_dir, "modules.txt")) &
      file.exists(fp(switch_dir, "inputs/modules.txt"))) {
    stop("Two versions of 'modules.txt' found:\n",
         switch_dir, "/modules.txt\n",
         switch_dir, "/inputs/modules.txt\n",
         "Delete or rename one of them to proceed."
    )
  }

  # check if there are two versions of "options.txt"
  if (file.exists(fp(switch_dir, "options.txt")) &
      file.exists(fp(switch_dir, "inputs/options.txt"))) {
    stop("Two versions of 'options.txt' found:\n",
         switch_dir, "/options.txt\n",
         switch_dir, "/inputs/options.txt\n",
         "Delete or rename one of them to proceed."
    )
  }

  if (!is.null(modules_file)) {
    # check if modules.txt exists
    if (file.exists(fp(switch_dir, modules_file))) {
      modules <- read_lines(fp(switch_dir, modules_file))
    } else {
      stop("File does not exists\n", fp(switch_dir, modules_file))
    }
  } else {
    message("Adding default 'modules.txt'")
  }
  # !!!ToDo: add modules' check

  # Initiate 'scenarios.txt'
  scenarios_no_tx.txt <- c(
    "# The first set of MCET project scenarios (without transmission constraints).",
    "# Run the following command to execute scenarios in the queue one by one",
    glue("# switch solve-scenarios --scenario-file {scenarios_file}"),
    ""
  )
  f_no_tx <- fp(switch_dir, scenarios_file)
  if (fWrt(f_no_tx)) write_lines(scenarios_no_tx.txt, f_no_tx)

  # No_policy ####
  # Scenario without carbon constraint.
  scen_name <- "No_policy"
  message("Creating '", scen_name,"' scenario")
  ii <- grepl("policies.carbon_policies$", modules)
  if (any(ii)) {
    ii <- ii & !grepl("^#", modules)
    if (sum(ii) > 1) stop("Duplicated 'carbon_policies' in 'modules.txt' file")
    modules[ii] <- paste0("#", modules[ii])
  }

  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "--exclude-modules", "switch_model.policies.carbon_policies",
    "\n"
  ),
  file = f_no_tx, append = TRUE)


  # Reference ####
  scen_name <- "Reference"
  message("Creating '", scen_name, "' scenario")
  # add/overwrite carbon_policies.csv
  carbon_policies.csv <- data.table(
    PERIOD = 2050,
    carbon_cap_tco2_per_yr = 0,
    carbon_cost_dollar_per_tco2 = 0
  )
  f <- fp(switch_dir, "inputs/carbon_policies.csv")
  if (fWrt(f)) write_lines(carbon_policies.csv, f)
  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "\n"),
    file = f_no_tx, append = TRUE)

  # RES_storage_H2 ####
  # Zero emissions by 2050 (capacity and transmission expansion and dispatch only for 2050), considering existing power plants in the region and their expected retirements, and allowing the following to be built: wind, solar, batteries, hydro, and hydrogen.
  message("Creating '", scen_name, "' scenario")
  scen_name <- "RES_storage_H2"
  gen_build_costs.csv <- fread(fp(switch_dir, "inputs/gen_build_costs.csv"))
  gen_info.csv <- fread(fp(switch_dir, "inputs/gen_info.csv"))
  # Create inputs for RES_storage_H2 (drop thermal techs)
  # identify projects (tech) to drop
  techs_to_drop <- NULL
  if (!is.null(thermal_techs_pattern) && thermal_techs_pattern != "") {
    ii <- grepl(thermal_techs_pattern, gen_build_costs.csv$GENERATION_PROJECT)
    techs_to_drop <- gen_build_costs.csv$GENERATION_PROJECT[ii] |> unique()
  }
  if (!is.null(fossil_fuels_pattern) && fossil_fuels_pattern != "") {
    # techs with fossil fuels
    jj <- grepl(fossil_fuels_pattern, gen_info.csv$gen_energy_source)
    techs_to_drop <- c(techs_to_drop, gen_info.csv$GENERATION_PROJECT[jj]) |>
      unique()
  }
  # drop fossil-fuels techs except projects built before base-year
  gen_build_costs_RES_storage_H2.csv <- gen_build_costs.csv |>
    filter(!(GENERATION_PROJECT %in% techs_to_drop) | build_year < base_year)
  f <- fp(switch_dir, "inputs/gen_build_costs_RES_storage_H2.csv")
  if (fWrt(f)) fwrite(gen_build_costs_RES_storage_H2.csv, f); rm(f)
  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--input-aliases", "gen_build_costs.csv=gen_build_costs_RES_storage_H2.csv",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "\n"),
    file = f_no_tx, append = TRUE)


  # RES_storage ####
  # Zero emissions by 2050 (capacity and transmission expansion and dispatch only for 2050), considering existing power plants in the region and their expected retirements, and allowing the following to be built: wind, solar, batteries, and hydro.
  scen_name <- "RES_storage"
  message("Creating '", scen_name, "' scenario")
  if (!is.null(H2_techs_pattern) && H2_techs_pattern != "") {
    ii <- grepl(H2_techs_pattern, gen_build_costs.csv$GENERATION_PROJECT)
    techs_to_drop <- c(techs_to_drop,
                       gen_build_costs.csv$GENERATION_PROJECT[ii]) |>
      unique()
  }
  if (!is.null(H2_pattern) && H2_pattern != "") {
    # techs with fossil fuels
    jj <- grepl(H2_pattern, gen_info.csv$gen_energy_source)
    techs_to_drop <- c(techs_to_drop, gen_info.csv$GENERATION_PROJECT[jj]) |>
      unique()
  }
  # Create inputs for RES_storage (drop thermal and H2 techs)
  gen_build_costs_RES_storage.csv <- gen_build_costs_RES_storage_H2.csv |>
    filter(!(GENERATION_PROJECT %in% techs_to_drop) | build_year < base_year)
  f <- fp(switch_dir, "inputs/gen_build_costs_RES_storage.csv")
  if (fWrt(f)) fwrite(gen_build_costs_RES_storage.csv, f); rm(f)
  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--input-aliases", "gen_build_costs.csv=gen_build_costs_RES_storage.csv",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "\n"),
    file = f_no_tx, append = TRUE)

  # DR_Ref ####
  # Demand response applied to the Reference case
  scen_name <- "DR_Ref"
  message("Creating '", scen_name, "' scenario")
  flexible_loads <- fread(fp("switch_modules/mcet_modules",
                             "flexible_loads.csv"))
  flexible_loads$PERIOD <- base_year
  f <- fp(switch_dir, "inputs/flexible_loads.csv")
  if (fWrt(f)) fwrite(flexible_loads,
                      fp(switch_dir, "inputs", "flexible_loads.csv"))
  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--include-modules", "mcet_modules.dates", "mcet_modules.flexible_loads",
    # "--input-aliases", "gen_build_costs.csv=gen_build_costs_RES_storage.csv",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "\n"),
    file = f_no_tx, append = TRUE)


  # DR_Ref_zonal ####
  # Demand response by zone applied to the Reference case
  scen_name <- "DR_Ref_zonal"
  message("Creating '", scen_name, "' scenario")
  load_zones.csv <- fread(fp(switch_dir, "inputs/load_zones.csv"))
  flexible_loads_by_zone_i <- fread(fp("switch_modules/mcet_modules",
                                       "flexible_loads_by_zone.csv"))[1,]
  flexible_loads_by_zone_i$PERIOD <- base_year

  flexible_loads_by_zone <- lapply(load_zones.csv$LOAD_ZONE, function(x) {
    flexible_loads_by_zone_i$ZONES <- x
    flexible_loads_by_zone_i
  }) |> rbindlist(); rm(flexible_loads_by_zone_i)

  f <- fp(switch_dir, "inputs/flexible_loads_by_zone.csv")
  if (fWrt(f)) fwrite(flexible_loads_by_zone,
                      fp(switch_dir, "inputs", "flexible_loads_by_zone.csv"))
  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--include-modules", "mcet_modules.dates", "mcet_modules.flexible_loads",
    "--input-aliases",
    # "gen_build_costs.csv=gen_build_costs_RES_storage.csv",
    "flexible_loads.csv=flexible_loads_by_zone.csv",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "\n"),
    file = f_no_tx, append = TRUE)

  # DR_RES ####
  # Demand response applied to the Reference case
  scen_name <- "DR_RES"
  message("Creating '", scen_name, "' scenario")
  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--include-modules", "mcet_modules.dates", "mcet_modules.flexible_loads",
    "--input-aliases",
    "gen_build_costs.csv=gen_build_costs_RES_storage.csv",
    # "flexible_loads.csv=flexible_loads_by_zone.csv",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "\n"),
    file = f_no_tx, append = TRUE)


  # DR_RES_zonal ####
  # Demand response applied to the Reference case
  scen_name <- "DR_RES_zonal"
  message("Creating '", scen_name, "' scenario")
  if (fWrt(f_no_tx, append = T)) write_lines(x = paste(
    glue("# {scen_name} ================================================== #"),
    "\n",
    "--scenario-name", scen_name,
    # "switch -solve",
    "--inputs-dir", "inputs",
    "--include-modules", "mcet_modules.dates", "mcet_modules.flexible_loads",
    "--input-aliases",
    "gen_build_costs.csv=gen_build_costs_RES_storage.csv",
    "flexible_loads.csv=flexible_loads_by_zone.csv",
    "--outputs-dir", glue("outputs-{scen_name}"),
    "\n"),
    file = f_no_tx, append = TRUE)

  if (T) {
    write_lines(c(
      "call activate switch",
      # glue("switch solve-scenarios --scenario-file {scenarios_file}"),
      glue("switch solve-scenarios"),
      "call conda deactivate",
      "cmd /k"
    ), file = fp(switch_dir, "run_scenarios.cmd"))
  }

}

if (F) {
  make_mcet_scenarios_queue_1(
    switch_dir = "scenarios/ISO-test/mod_toy/scen_pypsa_2050_TOL99/switch"
    )
}

# ============================================================================= #

make_mcet_scenarios_queue_2 <- function(
  ## Point to the Switch-model directory
  # switch_dir = "scenarios/ISO-test/mod_toy/scen_pypsa_2050_TOL99/switch",
  switch_dir,
  scenarios_file = "scenarios_tx.txt",
  reference_scenario = "Reference"
) {
  # Initiate 'scenarios.txt'
  scenarios_tx.txt <- c(
    "# The second set of MCET project scenarios (with transmission constraints).",
    "# Run the following command to execute scenarios in the queue one by one",
    glue("# switch solve-scenarios --scenario-file {scenarios_file}"),
    "# If this doesn't work, rename 'scenarios_tx.txt' to 'scenarios.txt'",
    glue("# and run switch solve-scenarios"),
    ""
  )
  f_tx <- fp(switch_dir, scenarios_file)
  if (fWrt(f_no_tx)) write_lines(scenarios_tx.txt, f_tx)

  # add csv-constraints
  transmission_lines.csv <-
    fread(fp(switch_dir, "inputs/transmission_lines.csv"))
  BuildTx.csv <- fread(fp(switch_dir, glue("outputs-{reference_scenario}"),
                          "BuildTx.csv"))

  ref_BuildTx <- transmission_lines.csv |>
    select(matches("TRANSMISSION|length")) |>
    left_join(BuildTx.csv,
              by = c(TRANSMISSION_LINE = "TRANS_BLD_YRS_1")) |>
    mutate(MW_km = BuildTx * trans_length_km)

  ref_BuildTx_MW_km <- ref_BuildTx$MW_km |> sum()
  stopifnot(ref_BuildTx_MW_km > 0)

  for (i in seq(0, .75, by = .25)) {
    scen_name <- glue("Ref_Tx_{i*100}")
    message("Creating scenarios {scen_name}")
    fname <- glue("transmission_limit_mw_km_{i*100}.csv")
    message(glue("   Writing {fname}"))
    tx_lim <- data.table(
      PERIOD = 2050,
      trans_expansion_limit_mw_km = i * ref_BuildTx_MW_km
    )
    fwrite(tx_lim, fp(switch_dir, "inputs", fname))
    rm(fname, tx_lim)

    if (fWrt(f_tx, append = T)) write_lines(x = paste(
      glue("# {scen_name} ================================================== #"),
      "\n",
      "--scenario-name", scen_name,
      # "switch -solve",
      "--inputs-dir", "inputs",
      "--include-modules", "mcet_modules.new_transmission_limit",
      "--input-aliases",
      glue("new_transmission_limit.csv=transmission_limit_mw_km_{i}.csv"),
      "--outputs-dir", glue("outputs-{scen_name}"),
      "\n"),
      file = f_tx, append = TRUE)

  }
  if (T) {
    write_lines(c(
      "call activate switch",
      glue("switch solve-scenarios --scenario-file {scenarios_file}"),
      # glue("switch solve-scenarios"),
      "call conda deactivate",
      "cmd /k"
    ), file = fp(switch_dir, "run_scenarios_tx.cmd"))
  }
}


if (F) {
  make_mcet_scenarios_queue_2(
    switch_dir = "scenarios/ISO-test/mod_toy/scen_pypsa_2050_TOL99/switch"
  )
}

countdown <- function(s) {
  for (i in rev(seq(s))) {
    cat("\rStarting in", i, "s. Press <Esc> to cancel")
    Sys.sleep(1)
  }
  cat("\n")
}
# countdown(5)

# ============================================================================= #
# Process results ####
switch_to_parquet <- function(
    # scen_path_pattern = "scenarios/(BGD|VNM)/mod_toy/scen_pypsa_2050_TOL99/switch/",
  scen_path = "scenarios",
  mod_shape = "mod_toy",
  mod_ver = "scen_pypsa_2050_TOL99",
  iso3c = c("BGD", "VNM", "THA", "KAZ", "IND", "CHN", "COL", "CHL"),
  switch_dir = "switch$",
  files_ext = c(".csv$", ".txt$", ".json$"),
  import_folders = c("inputs$", "output(s?)[-_ ]?"),
  save_format = "parquet",
  save_path = save_format
) {
  d <- list.dirs(path = scen_path)
  ii <-
    grepl(mod_shape, d, ignore.case = T) &
    grepl(mod_ver, d, ignore.case = T) &
    grepl(paste(iso3c, collapse = "|"), d, ignore.case = T) &
    grepl(switch_dir, d, ignore.case = T)
  nmod <- sum(ii)
  message("Found ", nmod, " models:")
  cat(d[ii], sep = "\n")
  message("Saving/appending to ", save_path)
  countdown(5)
  # d <- list.files(pattern = scen_pattern, include.dirs = T, recursive = T)
  # browser()
  for (i in d[ii]) {
    cont <- str_extract(i, iso3c)
    cont <- cont[!is.na(cont)]
    if (is.na(cont)) stop("Contry code not found in the path", i)
    if (length(cont) > 1) stop("Two country-codes found ", i, " ",
                               paste(cont, sep = ", "))
    # mod_name <- "scen_pypsa_2050_TOL99"
    sns <- lapply(import_folders, function(x) {
      list.files(i, pattern = x, full.names = T)
    }) |> unlist()
    sns <- c(i, sns) |> unique()
    # r <- sns[5]
    for (r in sns) {
      if (grepl("input", r)) {
        # browser()
        # inputs folder
        scen_name <- str_extract(r, "(?<=input(s|)[-_]).*")
        # if (is_empty(scen_name) || is.na(scen_name) || scen_name == "") {
        #   scen_name <- NULL
        # }
        dir_name <- "inputs"
      } else if (grepl("output", r)) {
        scen_name <- str_extract(r, "(?<=output(s?)[-_ ]).*")
        dir_name <- "outputs"
      } else if (grepl(switch_dir, r)) {
        scen_name <- NULL
        dir_name <- NULL
      } else {
        message("Skipping folder: ", r)
        next
      }
      if (is_empty(scen_name) || is.na(scen_name) || scen_name == "") {
        scen_name <- NULL
      }
      ff <- lapply(files_ext, function(x) {
        list.files(r, x, full.names = T, ignore.case = T)
      }) |> unlist()
      # ff <- list.files(r, ".csv$", full.names = T)
      message("Country: '", cont, "', Scenario: '", scen_name,
              "', ", length(ff), " files.")
      if (!is_empty(ff)) p <- progressr::progressor(along = ff)
      for (f in ff) {
        p(basename(f))
        # browser()
        if (grepl(".txt$", f)) {
          # browser()
          # csv_f <- arrow::read_csv_arrow(f, col_names = F)
          csv_f <- read_lines(f) |> as.data.table()
        } else if (grepl(".json$", f)) {
          # browser()
          csv_f <- read_lines(f) |> as.data.table()
          # csv_f <- arrow::read_json_arrow(f, col_names = F)
        } else if (grepl(".csv$", f)) {
          csv_f <- fread(f)
        } else {
          warning("Unknown file extention ", basename(f))
          csv_f <- arrow::read_csv_arrow(f)
        }
        csv_f <- csv_f |>
          mutate(
            # file = gsub(".csv", "", basename(f)),
            iso3c = cont,
            mod_shape = mod_shape,
            mod_ver = mod_ver,
            scen = scen_name,
            .before = 1
          ) |>
          group_by(iso3c, mod_shape, mod_ver)
        if (!is.null(scen_name)) csv_f <- csv_f |> group_by(scen, .add = TRUE)
        csv_f |>
          write_dataset(
            # path = save_path,
            # path = fp(save_path, paste0("file=", basename(f))),
            path = paste(save_path, dir_name, basename(f), sep = "/"),
            format = save_format,
            existing_data_behavior = "delete_matching")
      }
    }
  }
}

