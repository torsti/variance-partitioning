suppressPackageStartupMessages(library(INLA))

approximation <- snakemake@wildcards[['parameter']]
fit <- readRDS(snakemake@input[['fit']])

extract_posterior <- function(fit, approximation, n_samples, threads = 1) {

    model_attrs <- fit$misc$configs$contents

    param_selection <- as.list(setNames(rep(0, length(model_attrs$tag)), model_attrs$tag))
    param_selection$APredictor <- -seq.int(model_attrs$length[model_attrs$tag == 'APredictor'])
    param_selection$Predictor <- -seq.int(model_attrs$length[model_attrs$tag == 'Predictor'])

    attr_idx <- !model_attrs$tag %in% c('APredictor', 'Predictor')
    model_attrs <- lapply(model_attrs, function(x) {x[attr_idx]})

    param_names <- unname(unlist(mapply(rep, model_attrs$tag, model_attrs$length)))

    if (approximation == 'distribution') {

        samples <- inla.posterior.sample(
            n_samples,
            fit,
            selection = param_selection,
            add.names = FALSE,
            num.threads = threads
        )

        hyperpar <- Reduce(rbind, Map(function(x) {x$hyperpar}, samples))
        hyperpar <- data.frame(
            variance.u = 1 / hyperpar[,'Precision for u'],
            sd.u = sqrt(1 / hyperpar[,'Precision for u']),
            variance.v = 1 / hyperpar[,'Precision for v'],
            sd.v = sqrt(1 / hyperpar[,'Precision for v']),
            range.z = hyperpar[,'Range for z'],
            variance.z = hyperpar[,'Stdev for z']^2,
            sd.z = hyperpar[,'Stdev for z'],
            correlation.z = hyperpar[,'GroupRho for z']
        )

        latent <- t(Reduce(cbind, Map(function(x) {x$latent}, samples)))

        posterior <- cbind(
            hyperpar,
            latent
        )
    } else if (approximation == 'mean') {
        hyperpar_mean <- c(
            variance.u = inla.emarginal(function(x) { 1 / x }, fit$marginals.hyperpar[['Precision for u']]),
            sd.u = inla.emarginal(function(x) { sqrt(1 / x) }, fit$marginals.hyperpar[['Precision for u']]),
            variance.v = inla.emarginal(function(x) { 1 / x }, fit$marginals.hyperpar[['Precision for v']]),
            sd.v = inla.emarginal(function(x) { sqrt(1 / x) }, fit$marginals.hyperpar[['Precision for v']]),
            range.z = fit$summary.hyperpar['Range for z', 'mean'],
            variance.z = inla.emarginal(function(x) { x^2 }, fit$marginals.hyperpar[['Stdev for z']]),
            sd.z = fit$summary.hyperpar['Stdev for z', 'mean'],
            correlation.z = fit$summary.hyperpar['GroupRho for z', 'mean']
        )

        latent_mean <- setNames(
            c(Reduce(rbind, fit$summary.random)[,'mean'], fit$summary.fixed$mean),
            param_names
        )

        posterior <- matrix(
            c(
                hyperpar_mean,
                latent_mean
            ),
            nrow = 1
        )

    } else if (approximation == 'mode') {
        hyperpar_mode <- c(
            variance.u = inla.mmarginal(inla.tmarginal(function(x) { 1 / x }, fit$marginals.hyperpar[['Precision for u']])),
            sd.u = inla.mmarginal(inla.tmarginal(function(x) { sqrt(1 / x) }, fit$marginals.hyperpar[['Precision for u']])),
            variance.v = inla.mmarginal(inla.tmarginal(function(x) { 1 / x }, fit$marginals.hyperpar[['Precision for v']])),
            sd.v = inla.mmarginal(inla.tmarginal(function(x) { sqrt(1 / x) }, fit$marginals.hyperpar[['Precision for v']])),
            range.z = fit$summary.hyperpar['Range for z', 'mode'],
            variance.z = inla.mmarginal(inla.tmarginal(function(x) { x^2 }, fit$marginals.hyperpar[['Stdev for z']])),
            sd.z = fit$summary.hyperpar['Stdev for z', 'mode'],
            correlation.z = fit$summary.hyperpar['GroupRho for z', 'mode']
        )

        latent_mode <- setNames(tail(fit$mode$x, sum(model_attrs$length)), param_names)

        posterior <- matrix(
            c(
                hyperpar_mode,
                latent_mode
            ),
            nrow = 1
        )
    }

    hyperpar_names <- c('variance.u', 'sd.u', 'variance.v', 'sd.v', 'range.z', 'variance.z', 'sd.z', 'correlation.z')

    posterior <- as.matrix(posterior)
    attr(posterior, 'param_names') <- c(hyperpar_names, param_names)
    dimnames(posterior) <- list(iterations = NULL, parameters = NULL)

    return(posterior)
}



posterior <- extract_posterior(
    fit,
    approximation,
    snakemake@params[['samples']],
    snakemake@threads
)

saveRDS(posterior, snakemake@output[['parameters']])
