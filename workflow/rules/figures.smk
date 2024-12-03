rule pdf_to_png:
    input:
        "{plot}.pdf",
    output:
        "{plot}.png",
    params:
        resolution=300,
    shell:
        "pdftocairo -r {params.resolution} -singlefile -png {input:q} {wildcards.plot:q}"


rule mesh_map:
    input:
        survey_area=rules.data_files.input.survey_area,
        patch_data=rules.prepare_data.output.patch_data,
        mesh=rules.generate_mesh.output.mesh,
        gshhg=rules.download_gshhg.output[0].format(
            format="shp", version="2.3.7", archive="zip"
        ),
    output:
        "results/figures/map/mesh_map.pdf",
    params:
        aland_bounds={"xmin": 19.25, "ymin": 59, "xmax": 21, "ymax": 61},
        excluded=config["model"]["excluded areas"],
    script:
        "../scripts/figures/mesh_map.R"


rule study_area:
    input:
        survey_area=rules.data_files.input.survey_area,
        patch_data=rules.prepare_data.output.patch_data,
        model_data=rules.prepare_data.output.unscaled_data,
        nest_photo=workflow.source_path("../../resources/map/nest.jpg"),
        sphere="results/data/map/sphere/60.2-20.1/110m/physical/ne_110m_land.gml",
        gshhg=rules.download_gshhg.output[0].format(
            format="shp", version="2.3.7", archive="zip"
        ),
    output:
        "results/figures/map/study_area.pdf",
    params:
        aland_bounds={"xmin": 19.25, "ymin": 59, "xmax": 21, "ymax": 61},
        excluded=config["model"]["excluded areas"],
        core_id=config["model"]["core area"],
        limits=False,
        figure={"width_cm": 18, "height_cm": 9.9, "pointsize": 10},
    script:
        "../scripts/figures/study_area.R"


rule panel_plot:
    input:
        survey_area=rules.data_files.input.survey_area,
        V_core_sample="results/variance/{response}/core/sample/complete/grouped/covariate_class/variance_partition.rds",
        M_core_sample="results/variance/{response}/core/sample/complete/grouped/covariate_class/marginal_variance_partition.rds",
        V_core_population="results/variance/{response}/core/population/complete/grouped/covariate_class/variance_partition.rds",
        M_core_population="results/variance/{response}/core/population/complete/grouped/covariate_class/marginal_variance_partition.rds",
        V_cond="results/variance/{response}/core/sample/conditional/vegetation/grouped/covariate_class/variance_partition.rds",
        V_wb="results/variance/{response}/core/sample/within-between/patch/grouped/covariate_class/variance_partition.rds",
    output:
        "results/figures/variance/{response}/panel.pdf",
    params:
        excluded=config["model"]["excluded areas"],
        core_id=config["model"]["core area"],
        figure={
            "width_cm": 18,
            "height_cm": 18,
            "pointsize": 10,
            "wh_adj": [0.0615, 0.075],
        },
    script:
        "../scripts/figures/panel_plot.R"


rule measure_plot:
    input:
        V="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/variance_partition.rds",
        P="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/partial_variance_partition.rds",
        M="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/marginal_variance_partition.rds",
    output:
        "results/figures/variance/{response}/{model_scope}/{partition_scope}/measure.pdf",
    params:
        figure={"width_cm": 8.5, "height_cm": 8.5, "pointsize": 10},
    script:
        "../scripts/figures/measure_plot.R"


rule covariance_plot:
    input:
        V="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/variance_partition.rds",
        R="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/correlation_matrix.rds",
        PK="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/partial_covariance_matrix.rds",
    output:
        "results/figures/variance/{response}/{model_scope}/{partition_scope}/covariance.pdf",
    params:
        figure={"width_cm": 18, "height_cm": 18, "pointsize": 10},
    script:
        "../scripts/figures/covariance_plot.R"
