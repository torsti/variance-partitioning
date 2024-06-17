rule group_model_components:
    input:
        archive=ancient(rules.retrieve_dryad_data.output.archive),
        data=rules.model_inference.output.data,
    output:
        grouping="results/variance/{response}/{model_scope}/grouping/{group}.tsv",
    wildcard_constraints:
        group="(?:covariate_class|spatial_scope)",
    script:
        "../scripts/group_model_components.R"


rule linear_terms:
    input:
        patch_data=rules.prepare_data.output.patch_data,
        full_data=rules.prepare_data.output.model_data,
        unscaled_data=rules.prepare_data.output.unscaled_data,
        model_data=rules.model_inference.output.data,
        samples=rules.posterior_sampling.output.samples,
    output:
        linear_terms="results/variance/{response}/{model_scope}/{partition_scope}/linear_terms.rds",
        conditions="results/variance/{response}/{model_scope}/{partition_scope}/conditions.tsv",
    threads: config["model"]["posterior samples"]
    params:
        core_area=config["model"]["core area"],
    script:
        "../scripts/linear_terms.R"


rule covariance_matrix:
    input:
        linear_terms=rules.linear_terms.output.linear_terms,
    output:
        covariance_matrix="results/variance/{response}/{model_scope}/{partition_scope}/{partition}/covariance_matrix.rds",
    threads: config["model"]["posterior samples"]
    wildcard_constraints:
        partition="complete",
    script:
        "../scripts/covariance_matrix.R"


rule conditional_covariance_matrix:
    input:
        linear_terms=rules.linear_terms.output.linear_terms,
        conditions=rules.linear_terms.output.conditions,
    output:
        covariance_matrix="results/variance/{response}/{model_scope}/{partition_scope}/{partition}/{condition}/covariance_matrix.rds",
    threads: config["model"]["posterior samples"]
    wildcard_constraints:
        partition="conditional|within-between",
    script:
        "../scripts/conditional_covariance_matrix.R"


rule partition_variance:
    input:
        covariance_matrix=rules.covariance_matrix.output.covariance_matrix,
    output:
        measure="results/variance/{response}/{model_scope}/{partition_scope}/{partition}/{grouping}/{measure}.rds",
    wildcard_constraints:
        grouping="individual",
        partition="complete",
    threads: config["model"]["posterior samples"]
    script:
        "../scripts/partition_variance.R"


rule partition_grouped_variance:
    input:
        covariance_matrix=rules.covariance_matrix.output.covariance_matrix,
        grouping=rules.group_model_components.output.grouping,
    output:
        measure="results/variance/{response}/{model_scope}/{partition_scope}/{partition}/{grouping}/{group}/{measure}.rds",
    wildcard_constraints:
        grouping="grouped",
        partition="complete",
        group="(?:covariate_class|spatial_scope)",
    threads: config["model"]["posterior samples"]
    script:
        "../scripts/partition_grouped_variance.R"


rule partition_conditional_variance:
    input:
        covariance_matrix=rules.conditional_covariance_matrix.output.covariance_matrix,
    output:
        measure="results/variance/{response}/{model_scope}/{partition_scope}/{partition}/{condition}/{grouping}/{measure}.rds",
    wildcard_constraints:
        grouping="individual",
        partition="(?:conditional|within-between)",
    threads: config["model"]["posterior samples"]
    script:
        "../scripts/partition_conditional_variance.R"


rule partition_grouped_conditional_variance:
    input:
        covariance_matrix=rules.conditional_covariance_matrix.output.covariance_matrix,
        grouping=rules.group_model_components.output.grouping,
    output:
        measure="results/variance/{response}/{model_scope}/{partition_scope}/{partition}/{condition}/{grouping}/{group}/{measure}.rds",
    wildcard_constraints:
        grouping="grouped",
        partition="(?:conditional|within-between)",
        group="(?:covariate_class|spatial_scope)",
    threads: config["model"]["posterior samples"]
    script:
        "../scripts/partition_grouped_conditional_variance.R"
