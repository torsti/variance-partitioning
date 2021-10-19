# Code supplement: Variance partitioning case study

This README provides a brief summary of the *Code supplement*.

Preprint available on [biorXiv](https://doi.org/10.1101/2021.10.17.464682).

## Data

### Data Dryad

The main dataset for the analyses comes from (Schulz *et al*. 2019) and will be automatically downloaded by the scripts, if missing.
See Schulz *et al*. 2020 and the README-file included in the Dryad data archive (`data/ECOG-04799.zip`) for a description of the data.

- Schulz, T., J. Vanhatalo, and M. Saastamoinen. 2019. “Data From: Long‐term demographic surveys reveal a consistent relationship between average occupancy and abundance within local populations of a butterfly metapopulation.” *Dryad*. Dataset. [doi:10.5061/dryad.ksn02v707](https://doi.org/10.5061/dryad.ksn02v707)
- Schulz, T., J. Vanhatalo, And M. Saastamoinen. 2020. “Long‐term demographic surveys reveal a consistent relationship between average occupancy and abundance within local populations of a butterfly metapopulation.” *Ecography* 43: 306–17. [doi:10.1111/ecog.04799](https://doi.org/doi:10.1111/ecog.04799).

### Survey areas

The figures and the mesh triangulation additionally use information on assignations of habitat patches to survey areas.
The data are included with the code in the following files:

`data/survey_map.tsv`
: Boundaries of the survey areas.

`data/survey_area.tsv`
: Patch assignations to survey areas.

### Map data

For use in the figures the scripts use the [Natural Earth](https://www.naturalearthdata.com/) and [GSHHG](https://www.soest.hawaii.edu/pwessel/gshhg/) data.
The necessary files are downloaded by the scripts, if missing.

The script `figures/data/hemisphere.py` is inspired by the ClipToHemisphere QGIS-plugin.
See the [original plugin](https://github.com/jdugge/ClipToHemisphere) by Juernjakob Dugge (2016) and the [QGIS3 version](https://github.com/woravich-k/Clip-to-Hemisphere-QGIS3) by Woravich Kumthonkittikul (2019).

### Photograph

The photo of a *Melitaea cinxia* larval nest `figures/data/nest.jpg` is cropped from a 2018 original by Torsti Schulz and is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

## Dependencies

The analyses and scripts depend on the following software libraries and tools. The most crucial are the workflow management system [Snakemake](https://snakemake.github.io/) and the Bayesian inference framework [R-INLA](https://www.r-inla.org/).

### R-packages

The R code depends on the following packages, though some are only used for the figures or the *Programming supplement*:

- `INLA`
- `Matrix`
- `abind`
- `colorspace`
- `grid`
- `gridGraphics`
- `jpeg`
- `knitr`
- `methods`
- `munsell`
- `parallel`
- `rmarkdown`
- `rstanarm`
- `scales`
- `sf`
- `sp`
- `units`
- `vioplot`

### Python libraries

Additionally scripts and `Snakefile`s involved in creating the map figures require these Python packages:

- `matplotlib`
- `numpy`
- `osgeo`
- `scipy`

### CLI-tools

In addition the following programs and `bash` functions are used in or by the Snakefiles:

- unzip
- wget
- pdftocairo
- org2ogr
- Rscript
- ln
- pandoc (and a LaTeX distribution such as TeX Live or MiKTeX)

## Analyzes and workflow

The workflow has been implemented and tested in Linux environments.
To run the full worklfow in a Windows environment some modifications might be required to the `Snakefile`s and R-scripts.

The workflow is implemented using `Snakemake` and modelling component uses `R-INLA` for inference.

The analysis steps is split into four groups, each in its own subdirectory, with a separate `Snakefile` for each:

`data`
: Code to download and process the data for model inference.

`model`
: Code to run model inference, sample from the joint distribution of the posterior approximation and calculate the linear term covariance matrices.

`variance`
: Code to implement the different form of variance partitioning.

`figures`
: Code to generate the figures for the article and download auxiliary data for the map figures.

Additionally, the `.Rmd` code to create the *Programming supplement* is provided in the `example`-directory.

### Running the workflow

The different groups of steps are linked via the `subworkflow` feature, such that any dependencies between the steps are resolved automatically.
Each `Snakefile` has an anonymous default `rule` that collects the crucial outputs of the steps in that group.

For example, to generate all figures (using a single core for processing), run ` snakemake -j1` inside the `figures`-directory.
If the previous steps have no been run, this might take a long time, as most of the figures depend on the model inference.

### (Partial) dependency graphs

To visualize dependencies between the workflow steps, you can use the --dag or --rulegrapgh options to the `snakemake` command.
Unfortunately the dependency graphs will not indicate links to rules in subworkflows.

For example, to visualize the dependencies of the rules to create the figures as a `.svg` file run `snakemake --rulegraph | dot -Tsvg > figures.svg` inside the `figures`-directory in a `bash`-like shell environment.
