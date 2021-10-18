suppressPackageStartupMessages(library(INLA))
suppressPackageStartupMessages(library(sf))
library(sp)

patch_data <- read.delim(snakemake@input[['patch_data']])
patch_coordinates <- patch_data[, c('x', 'y')]

survey_map <- read.delim(snakemake@input[['survey_map']])
survey_map <- st_union(st_as_sfc(survey_map[,'boundary'], crs = 3067))

mesh <- inla.mesh.2d(
    loc = as.matrix(patch_coordinates),
    offset = snakemake@params[['offset']],
    max.edge = snakemake@params[['max_edge']][['initial']],
    min.angle = snakemake@params[['min_angle']][['initial']],
    cutoff = snakemake@params[['cutoff']]
)

loc <- mesh$loc
loc_point <- st_as_sfc(SpatialPoints(loc[,-3] * 1000))
st_crs(loc_point) <- 3067

loc_inside <- st_within(loc_point, survey_map, sparse = FALSE)

loc_max_edge <- rep(Inf, mesh$n)
loc_max_edge[loc_inside] <- snakemake@params[['max_edge']][['refined']]

mesh <- inla.mesh.create(
    loc = loc,
    tv = mesh$graph$tv,
    boundary = mesh$segm$bnd,
    interior = mesh$segm$int,
    refine = list(
        min.angle = snakemake@params[['min_angle']][['refined']],
        max.edge = Inf
    ),
    quality.spec = list(
        loc = loc_max_edge
    )
)

saveRDS(mesh, snakemake@output[['mesh']])
