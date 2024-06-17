suppressPackageStartupMessages(library("sf"))

snakemake@source("../lib/io.R")
snakemake@source("../lib/connectivity.R")


## Data

patch_network <- read_zipfile("patch_network")
patch_area <- read_zipfile("patch_area")
survey_data <- read_zipfile("survey_data")

landuse_patch <- read_zipfile("landuse_patch")
landuse_edge <- read_zipfile("landuse_edge")
landuse_buffer <- read_zipfile("landuse_buffer")

covariates <- read_zipfile("covariates")

survey_area <- read_tsvfile("survey_area", encoding = "UTF8")
network_id <- read_tsvfile("network")
supplement <- read_tsvfile("supplement")

## Parameters

min_year <- snakemake@params[["minimum_year"]]
dispersal_scale <- snakemake@params[["dispersal_scale"]]
immigration_scale <- snakemake@params[["immigration_scale"]]
min_landuse_proportion <- snakemake@params[["min_landuse_proportion"]]
extinction_threshold <- snakemake@params[["extinction_threshold"]]


## Prepare data

### Filter land use data

landuse_data <- merge(landuse_patch, landuse_edge, by = "patch", all.x = TRUE)
landuse_data <- merge(landuse_data, landuse_buffer, by = "patch", all.x = TRUE)

filter_landuse_representation <- function(x) {
    filter_proportion <- function(x, proportion) {
        (length(unique(x)) / length(x)) > (proportion)
    }
    patch_col <- colnames(x) == "patch"
    landuse_proportion <- apply(
        x,
        2,
        filter_proportion,
        proportion = min_landuse_proportion
    )

    return(patch_col | landuse_proportion)
}

landuse_data <- landuse_data[, filter_landuse_representation(landuse_data)]


### Merge patch data with network and survey area identifiers

patch_network[, c("x", "y")] <- patch_network[, c("x", "y")] * 1000

supplement[, "viable"] <- as.integer(
    supplement[, "metapopulation_capacity"] > extinction_threshold
)

patch_data <- merge(
    patch_network,
    network_id,
    by = "patch",
    all.x = TRUE
)

patch_data <- merge(
    patch_data,
    supplement[, c("network", "viable")],
    by = "network",
    all.x = TRUE
)

patch_data <- st_as_sf(
    patch_data,
    coords = c("x", "y"),
    crs = 3067
)

excluded <- snakemake@params[["excluded"]]
survey_area <- survey_area[!survey_area$description %in% excluded, ]

survey_area <- st_as_sf(survey_area, wkt = "boundary", crs = 3067)
patch_data <- st_join(patch_data, survey_area, left = FALSE)

coordinates <- st_coordinates(patch_data)

patch_data <- as.data.frame(patch_data)
patch_data <- cbind(
    patch_data[,
        !colnames(patch_data) %in% c("area", "geometry", "description")
    ],
    coordinates
)

colnames(patch_data) <- tolower(colnames(patch_data))
colnames(patch_data)[colnames(patch_data) == "id"] <- "survey_area"


### Drop unsurveyed patches

common_patches <- intersect(
    intersect(patch_data$patch, survey_data$patch),
    patch_area$patch
)

patch_data <- patch_data[patch_data$patch %in% common_patches, ]

patch_area <- patch_area[
    patch_area$patch %in% common_patches &
    patch_area$year > min_year,
]

survey_data <- survey_data[
    survey_data$patch %in% common_patches &
    survey_data$year > min_year,
]

patches <- sort(common_patches)
years <- sort(unique(survey_data$year))

n_patch <- length(patches)
n_year <- length(years)


### Patch areas

patch_data <- patch_data[order(patches), ]
patch_area <- patch_area[order(patch_area$year, patch_area$patch), ]


## Connectivity

coordinates <- patch_data[, c("x", "y")] / 1000
D <- Matrix(as.matrix(dist(coordinates)))
population_connectivity <- matrix(0, nrow = n_patch, ncol = n_year)

for (y in years) {
    population <- rep.int(0, n_patch)
    area <- rep.int(0, n_patch)

    idx <- survey_data$year == y &
        is.finite(survey_data$previous_population) &
        survey_data$previous_population > 0

    patch_idx <- match(survey_data$patch[idx], patches)
    population[patch_idx] <- survey_data$previous_population[idx]

    idx <- patch_area$year == y
    patch_idx <- match(patch_area$patch[idx], patches)

    area[patch_idx] <- patch_area$area[idx]

    S <- population_connectivity_matrix(
        area,
        D,
        dispersal_scale,
        immigration_scale,
        population
    )

    population_connectivity[, match(y, years)] <- rowSums(S)
}

connectivity <- data.frame(
        year = rep(years, each = n_patch),
        patch = rep(patches, n_year),
        connectivity = c(population_connectivity)
)


## Clean and normalize survey data

survey_data$vegetation <- pmax(survey_data$plantago, survey_data$veronica)
survey_data$both_hosts <- as.integer(
    survey_data$plantago > 0 & survey_data$veronica > 0
)
survey_data$low_vegetation <-
    (survey_data$plantago_low + survey_data$veronica_low) /
    (survey_data$both_hosts + 1)
survey_data$dry_vegetation <-
    (survey_data$plantago_dry + survey_data$veronica_dry) /
    (survey_data$both_hosts + 1)
survey_data$presence <- as.integer(survey_data$population > 0)
survey_data$previous_presence <- as.integer(survey_data$previous_population > 0)


## Merge data

model_data <- expand.grid(patches, years)
colnames(model_data) <- c("patch", "year")

model_data <- merge(
    model_data,
    patch_area,
    by = c("patch", "year"),
    all.x = TRUE
)

model_data <- merge(
    model_data,
    patch_data[, c("patch", "network", "viable", "survey_area")],
    by = "patch",
    all.x = TRUE
)

model_data <- merge(
    model_data,
    connectivity,
    by = c("patch", "year"),
    all.x = TRUE
)

model_data <- merge(
    model_data,
    survey_data,
    by = c("patch", "year"),
    all.x = TRUE
)

model_data <- merge(
    model_data,
    landuse_data,
    by = c("patch"),
    all.x = TRUE
)


## Select columns

column_idx <- c(
    c("patch", "year", "network", "viable", "survey_area"),
    c("presence", "population"),
    covariates$name[covariates$name %in% colnames(model_data)]
)

model_data <- model_data[, column_idx]
model_data <- model_data[complete.cases(model_data), ]
model_data <- model_data[model_data$vegetation > 0, ]

covariate_idx <- colnames(model_data) %in% covariates$name

model_data <- model_data[, c(which(!covariate_idx), which(covariate_idx))]
model_data <- model_data[order(model_data$year, model_data$patch), ]

unscaled_data <- model_data


## Transform data

model_data$vegetation <- log(model_data$vegetation)
model_data$connectivity <- log(model_data$connectivity)
model_data$area <- log(model_data$area)
model_data$previous_population <- log1p(model_data$previous_population)


## Output

write_tsvfile(patch_data, "patch_data")
write_tsvfile(model_data, "model_data")
write_tsvfile(unscaled_data, "unscaled_data")