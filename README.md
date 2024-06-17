# Code supplement: Variance partitioning case study

[![DOI](https://zenodo.org/badge/418511320.svg)](https://zenodo.org/badge/latestdoi/418511320)

This README provides a brief summary of the *Code supplement*.

Preprint available on [biorXiv](https://doi.org/10.1101/2021.10.17.464682).

## Data

### Data Dryad

The main dataset for the analyses comes from (Schulz *et al*. 2019) and will be automatically downloaded by the scripts, if missing.
See Schulz *et al*. 2020 and the README-file included in the Dryad data archive (`results/data/ECOG-04799.zip`) for a description of the data.

- Schulz, T., J. Vanhatalo, and M. Saastamoinen. 2019. "Data From: Long‐term demographic surveys reveal a consistent relationship between average occupancy and abundance within local populations of a butterfly metapopulation." *Dryad*. Dataset. [doi:10.5061/dryad.ksn02v707](https://doi.org/10.5061/dryad.ksn02v707)
- Schulz, T., J. Vanhatalo, And M. Saastamoinen. 2020. "Long‐term demographic surveys reveal a consistent relationship between average occupancy and abundance within local populations of a butterfly metapopulation." *Ecography* 43: 306–17. [doi:10.1111/ecog.04799](https://doi.org/10.1111/ecog.04799).

### Survey areas

The figures and the mesh triangulation additionally use information on assignations of habitat patches to survey areas.
The data are included with the code in the following file:

`resources/data/survey_boundary.tsv`
: Boundaries of the survey areas.

### Patch networks

Data on properties of the habitat patch networks are taken from the study by Hanski *et al.* (2017). The patch assignations to the networks are also provided.

`resources/data/network_id.tsv`
: Patch assignations to survey areas.

`resources/data/ncomms14504_s2.tsv`
: Supplementary Data 1 from (Hanski *et al.* 2017; Data for the 125 patch networks) converted to a tab-separad values -file. The supplement is copyright by the authors of the study and distributed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

- Hanski, I., T. Schulz, S. Wong, V. Ahola., A. Ruokolainen, and S. P. Ojanen. 2017. "Ecological and genetic basis of metapopulation persistence of the Glanville fritillary butterfly in fragmented landscapes." *Nature Communnications* 8: 14504. [10.1038/ncomms14504](https://doi.org/10.1038/ncomms14504)

### Map data

The map figures use [Natural Earth](https://www.naturalearthdata.com/) and [GSHHG](https://www.soest.hawaii.edu/pwessel/gshhg/) data.
The necessary files are downloaded by the scripts, if missing.

The script `workflow/scripts/hemisphere.py` is inspired by the ClipToHemisphere QGIS-plugin.
See the [original plugin](https://github.com/jdugge/ClipToHemisphere) by Juernjakob Dugge (2016) and the [QGIS3 version](https://github.com/woravich-k/Clip-to-Hemisphere-QGIS3) by Woravich Kumthonkittikul (2019).

### Photograph

The photo of a *Melitaea cinxia* larval nest `resources/map/nest.jpg` is cropped from a 2018 original by Torsti Schulz and is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

## Dependencies

The analyses and scripts depend on the following software libraries and tools. The most crucial are the workflow management system [Snakemake](https://snakemake.github.io/) and the Bayesian inference framework [R-INLA](https://www.r-inla.org/).

### R-packages

The R code depends on the following packages, though many are only used for the figures or the *Programming supplement*:

- `abind`
- `colorspace`
- `grid`
- `gridGraphics`
- `Hmisc`
- `INLA`
- `jpeg`
- `knitr`
- `Matrix`
- `munsell`
- `parallel`
- `rmarkdown`
- `rstanarm`
- `scales`
- `sf`
- `units`
- `vioplot`

### Python libraries

Additionally scripts and `Snakefile`s involved in creating the map figures require these Python packages:

- `numpy`
- `osgeo`
- `scipy`

### CLI-tools

In addition the following programs are used in or by the Snakefiles:

- ogr2ogr
- pandoc (and a LaTeX distribution such as TeX Live or MiKTeX)
- pdftocairo
- wget

## Analyzes and workflow

The workflow has been implemented and tested in Linux environments.
To run the full worklfow in a Windows environment some modifications might be required to the `Snakefile`s and R-scripts.

The workflow is implemented using `Snakemake` and modelling component uses `R-INLA` for inference.

By default the workflow will generate the figures used in the study, first retrieving and processsing all required data and then running the model inference to generate the results presented in the figures.

### Dependency graphs

To visualize dependencies between the workflow steps, you can use the --dag or --rulegrapgh options to the `snakemake` command.

For example, to visualize the dependencies of the rules to create the figures as a `.svg` file run `snakemake --rulegraph | dot -Tsvg > figures.svg` in a `bash`-like shell environment.
