rule retrieve_dryad_data:
    output:
        archive=protected(
            ensure(
                "results/data/ECOG-04799.zip",
                sha256="c70f43544f61e7273afebfa42ed1d488c3b945efe4d5c68e6557030087ee7fd9",
            )
        ),
    params:
        dryad_url="https://datadryad.org/stash/downloads/file_stream",
        stream_id=178579,
        user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    message:
        "Download data for Schulz et al. 2020 from Dryad (doi: 10.5061/dryad.ksn02v707)."
    shell:
        "wget -U {params.user_agent:q} --output-document={output.archive:q} '{params.dryad_url}/{params.stream_id}'"


rule data_files:
    message:
        "Placeholder rule for imported data files."
    input:
        survey_area=workflow.source_path("../../resources/data/survey_boundary.tsv"),
        network_id=workflow.source_path("../../resources/data/network_id.tsv"),
        supplement=workflow.source_path("../../resources/data/ncomms14504_s2.tsv"),


rule generate_mesh:
    input:
        archive=ancient(rules.retrieve_dryad_data.output.archive),
        survey_area=rules.data_files.input.survey_area,
    output:
        mesh="results/data/mesh.rds",
    params:
        max_edge={"initial": [15, 30.0], "refined": [2.5, 7.5]},
        min_angle={"initial": 21, "refined": 26},
        offset=[10.0, 20.0],
        buffer=[1, 5],
        excluded=config["model"]["excluded areas"],
    message:
        "Generate study domain triangulation of the survey area for SPDE-based spatial field approximation."
    script:
        "../scripts/generate_mesh.R"


rule prepare_data:
    input:
        archive=ancient(rules.retrieve_dryad_data.output.archive),
        survey_area=rules.data_files.input.survey_area,
        network=rules.data_files.input.network_id,
        supplement=rules.data_files.input.supplement,
    output:
        model_data="results/data/model_data.tsv",
        patch_data="results/data/patch_data.tsv",
        unscaled_data="results/data/unscaled_data.tsv",
    params:
        minimum_year=1999,
        dispersal_scale=2,
        immigration_scale=0.44,
        min_landuse_proportion=0.01,
        excluded=config["model"]["excluded areas"],
        extinction_threshold=5.47,
    message:
        "Prepare data for model inference, including connecticity calculation and data transformations."
    script:
        "../scripts/prepare_data.R"
