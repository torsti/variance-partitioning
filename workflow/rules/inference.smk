rule model_inference:
    input:
        mesh=rules.generate_mesh.output.mesh,
        archive=ancient(rules.retrieve_dryad_data.output.archive),
        patch_data=rules.prepare_data.output.patch_data,
        model_data=rules.prepare_data.output.model_data,
    output:
        fit="results/inference/{response}/{model_scope}/model.rds",
        data="results/inference/{response}/{model_scope}/data.rds",
    log:
        "results/inference/{response}/{model_scope}/inla.log",
    threads: workflow.cores
    params:
        quantiles=[0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975],
        fixed={"prec": 1, "prec.intercept": 0.01},
        iid_sigma=[1.0, 0.5],
        matern_range=[1, 0.05],
        matern_sigma=[1.0, 0.5],
        core_area=config["model"]["core area"],
    message:
        "Fit {wildcards.response} model for the {wildcards.model_scope} area."
    script:
        "../scripts/model_inference.R"


rule posterior_sampling:
    input:
        fit=rules.model_inference.output.fit,
    output:
        samples="results/inference/{response}/{model_scope}/samples.rds",
    message:
        "Sample from the approximated posterior of the {wildcards.response} model for the {wildcards.model_scope} area."
    threads: workflow.cores
    params:
        posterior_samples=config["model"]["posterior samples"],
    script:
        "../scripts/posterior_sampling.R"
