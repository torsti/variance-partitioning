suppressPackageStartupMessages(library("INLA"))
suppressPackageStartupMessages(library("sf"))

snakemake@source("../lib/io.R")


## Data

patch_network <- read_zipfile("patch_network")
survey_area <- read_tsvfile("survey_area", encoding = "UTF8")


## Parameters

max.edge <- snakemake@params[["max_edge"]][["initial"]] * 1000
refined.edge <- snakemake@params[["max_edge"]][["refined"]] * 1000
min.angle <- snakemake@params[["min_angle"]][["initial"]]
refined.angle <- snakemake@params[["min_angle"]][["refined"]]
offset <- snakemake@params[["offset"]] * 1000
buffer <- snakemake@params[["buffer"]] * 1000


## Data preparation

excluded <- snakemake@params[["excluded"]]
survey_area <- survey_area[!survey_area$description %in% excluded, ]

patch_network[, c("x", "y")] <- patch_network[, c("x", "y")] * 1000

survey_area <- st_union(st_as_sf(survey_area, wkt = "boundary", crs = 3067))
patch_network <- st_as_sf(patch_network, coords = c("x", "y"), crs = 3067)


## Mesh preparation using survey areas

envelope <- as_Spatial(st_as_sfc(st_bbox(survey_area)))

initial_points <- inla.mesh.2d(
    boundary = envelope,
    max.edge = max.edge[[1]]
)$loc[, -3]

survey_idx <- st_within(
    st_as_sf(data.frame(initial_points), coords = 1:2, crs = 3067),
    survey_area,
    sparse = FALSE
)

initial_points <- initial_points[survey_idx, ]

mesh <- inla.mesh.2d(
    loc       = initial_points,
    offset    = offset,
    boundary  = inla.nonconvex.hull(st_coordinates(survey_area)),
    max.edge  = max.edge,
    min.angle = min.angle
)


## Mesh refinement using patches

mesh_points <- mesh$loc[, -3]

mesh_coordinates <- st_as_sf(data.frame(mesh_points), coords = 1:2, crs = 3067)

survey_idx <- st_within(mesh_coordinates, survey_area, sparse = FALSE)

distance <- st_distance(mesh_coordinates, patch_network)

min_median <- function(x, n) {
    return(median(sort(x)[1:n]))
}

min_distance <- apply(
    distance,
    1,
    min_median,
    n = 5
)

network_idx <- (min_distance < buffer[[1]]) & survey_idx
buffer_idx <- (min_distance < buffer[[2]]) & survey_idx

buffer_idx <- buffer_idx & !network_idx

loc_max_edge <- rep(Inf, mesh$n)
loc_max_edge[network_idx] <- refined.edge[[1]]
loc_max_edge[buffer_idx] <- refined.edge[[2]]

mesh <- inla.mesh.create(
    loc = mesh_points,
    tv = mesh$graph$tv,
    boundary = mesh$segm$bnd,
    interior = mesh$segm$int,
    refine = list(
        min.angle = refined.angle,
        max.edge = Inf
    ),
    quality.spec = list(
        loc = loc_max_edge
    )
)


## Output

write_rdsfile(mesh, "mesh")