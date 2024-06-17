rule render_rmarkdown:
    input:
        rmd=workflow.source_path("../../resources/supplement/programming.Rmd"),
        archive=ancient(rules.retrieve_dryad_data.output.archive),
    output:
        "results/supplement/programming.{ext}",
    wildcard_constraints:
        ext="(?:pdf)",
    script:
        "../scripts/render_rmarkdown.R"
