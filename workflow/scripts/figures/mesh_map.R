snakemake@source("common.R")

suppressPackageStartupMessages(library(INLA))
suppressPackageStartupMessages(library(units))


survey_area <- read.delim(snakemake@input[["survey_area"]])
patch_data <- read.delim(snakemake@input[["patch_data"]])

mesh <- readRDS(snakemake@input[["mesh"]])

gshhg_ogr <- paste0(
    "/vsizip/",
    snakemake@input[["gshhg"]],
    "/GSHHS_shp/f/GSHHS_f_L1.shp"
)

sink("/dev/null")
land <- st_read(gshhg_ogr)
sink()

suppressMessages(sf_use_s2(FALSE))

aland_bounds <- unlist(snakemake@params[["aland_bounds"]])
aland_box <- st_as_sfc(st_bbox(aland_bounds, crs = 4326))

aland_land <- suppressMessages(st_intersection(aland_box, st_make_valid(land)))
aland <- suppressMessages(st_geometry(st_transform(aland_land, 3067)))
aland <- aland[st_area(aland) > set_units(1500^2, "m^2")]


excluded_areas <- snakemake@params[["excluded"]]

survey_area <- survey_area[!survey_area$description %in% excluded_areas, ]
survey_area <- st_set_crs(st_union(st_as_sfc(survey_area[, "boundary"])), 3067)

xlim_aland <- buffered_range(range(mesh$loc[, 1]), 0.025)
ylim_aland <- buffered_range(range(mesh$loc[, 2]), 0.025)

ratio <- diff(ylim_aland) / diff(xlim_aland)

pdf(
    file = snakemake@output[[1]],
    title = "Mesh triangulation",
    width = 4,
    height = 4 * ratio,
    family = font_family,
    onefile = FALSE
)

par(mar = rep(0, 4), xaxs = "i", yaxs = "i")

plot(
    aland,
    xlim = xlim_aland,
    ylim = ylim_aland,
    col = "grey",
    lty = 0,
    asp = 1
)

plot(
    survey_area,
    col = alpha("darkolivegreen3", 0.35),
    lty = 0,
    add = TRUE
)

plot(
    mesh,
    add = TRUE,
    lwd = 0.5,
    edge.col = alpha("black", 0.3)
)

points(
    patch_data[, c("x", "y")],
    pch = 16,
    cex = 0.075,
    col = "red"
)

invisible(dev.off())