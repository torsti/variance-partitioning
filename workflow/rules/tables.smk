rule section_vp_results:
    input:
        "results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/{measure}.rds",
    output:
        "results/table/section/{response}/{model_scope}/{partition_scope}/complete/{measure}.tsv",
    wildcard_constraints:
        measure="(?:variance_partition|marginal_variance_partition|partial_variance_partition)",
    script:
        "../scripts/tables/vp_results.R"


rule section_vp_ratio:
    input:
        V="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/variance_partition.rds",
        P="results/variance/{response}/{model_scope}/{partition_scope}/complete/grouped/covariate_class/partial_variance_partition.rds",
    output:
        "results/table/section/{response}/{model_scope}/{partition_scope}/complete/ratio.tsv",
    script:
        "../scripts/tables/vp_ratio.R"


rule section_partitioned_vp_results:
    input:
        "results/variance/{response}/{model_scope}/{partition_scope}/{partition}/{condition}/grouped/covariate_class/{measure}.rds",
    output:
        "results/table/section/{response}/{model_scope}/{partition_scope}/{partition}/{condition}/{measure}.tsv",
    wildcard_constraints:
        measure="(?:variance_partition|marginal_variance_partition|partial_variance_partition)",
        partition="(?:conditional|within-between)",
    script:
        "../scripts/tables/partitioned_vp_results.R"


rule table_model_vs_vp_scope:
    input:
        Core_Sample_V="results/variance/occupancy/core/sample/complete/grouped/covariate_class/{measure}.rds",
        Core_Prediction_V="results/variance/occupancy/core/prediction/complete/grouped/covariate_class/{measure}.rds",
        Core_Population_V="results/variance/occupancy/core/population/complete/grouped/covariate_class/{measure}.rds",
        Full_Sample_V="results/variance/occupancy/full/sample/complete/grouped/covariate_class/{measure}.rds",
        Full_Conditional_V="results/variance/occupancy/full/sample/conditional/core_area/grouped/covariate_class/{measure}.rds",
    output:
        "results/table/table_model_vs_vp_scope/{measure}.tsv",
    wildcard_constraints:
        measure="(?:variance_partition|marginal_variance_partition|partial_variance_partition)",
    script:
        "../scripts/tables/table_model_vs_vp_scope.R"


rule table_appendix_hyperparameters:
    input:
        "results/inference/occupancy/core/model.rds",
    output:
        "results/table/supplement/hyperparameters.tsv",
    script:
        "../scripts/tables/hyperparameters.R"


rule table_appendix_covariates:
    input:
        "results/inference/occupancy/core/model.rds",
    output:
        "results/table/supplement/covariates.tsv",
    script:
        "../scripts/tables/covariates.R"
