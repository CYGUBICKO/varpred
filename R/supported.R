# Some of the implemented methods

## Some models require prep
prepmod.default <- function(mod, ...) {
	return(mod)
}

## brms
prepmod.brmsfit <- function(mod, ...) {
	check_terms <- attr(mod, "terms")
	if (is.null(check_terms)) {
		bform <- formula(mod)
		## 2022 Nov 21 (Mon): namespace issues?
#		bform <- brms:::update_re_terms(bform, re_formula=NA)
		bterms <- brms::brmsterms(bform, resp_rhs_all = FALSE)
		bterms <- attr(model.frame(bterms$allvars, data = mod$data), "terms")
		mod$terms <- bterms
	}
	return(mod)
}

## default
vareffobj.default <- function(mod, ...){
	out <- list()
	out$coefficients <- coef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod)
	out$link <- family(mod)
	out$contrasts <- mod$contrasts
	class(out) <- "vareffobj"
	return(out)
}

## lm
vareffobj.lm <- function(mod, ...){
	out <- list()
	out$coefficients <- coef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod)
	out$link <- family(mod)
	out$contrasts <- mod$contrasts
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.default <- function(mod, ...) {
	mm <- model.matrix(mod, ...)
	return(mm)
}

get_contrasts.default <- function(mod, ...) {
	contr <- attr(get_model.mm(mod, ...), "contrasts")
	return(contr)
}

get_contrasts.lm <- function(mod, ...) {
#	mod$contrasts
	contr <- attr(get_model.mm(mod, ...), "contrasts")
	return(contr)
}

get_xlevels.lm <- function(mod) {
	return(mod$xlevels)
}

check_intercept.default <- function(mod, ...) {
	any(grepl("Intercept", names(coefficients(mod)), ignore.case=TRUE))
}

## brms
vareffobj.brmsfit <- function(mod, ...) {
	out <- list()
	out$coefficients <- brms::fixef(mod)[,"Estimate"]
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(terms(mod)) #as.formula(brms:::update_re_terms(formula(mod), re_formula=NA))
	out$link <- family(mod)
	out$contrasts <- get_contrasts(mod, ...)
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.brmsfit <- function(mod, ...) {
	mm <- model.matrix(terms(mod), mod$data)
	return(mm)
}

get_contrasts.brmsfit <- function(mod, ...) {
	contr <- attr(get_model.mm(mod, ...), "contrasts")
	return(contr)
}

get_xlevels.brmsfit <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

check_intercept.brmsfit <- function(mod, ...) {
	any(grepl("Intercept", names(brms::fixef(mod)[,"Estimate"]), ignore.case=TRUE))
}


## rstanarm

vareffobj.stanreg <- function(mod, ...) {
	out <- list()
	out$coefficients <- rstanarm::fixef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	out$contrasts <- get_contrasts(mod, ...)
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.stanreg <- function(mod, ...) {
	mm <- model.matrix(mod, ...)
	return(mm)
}

get_contrasts.stanreg <- function(mod, ...) {
	contr <- attr(get_model.mm(mod, ...), "contrasts")
	return(contr)
}

get_xlevels.stanreg <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

check_intercept.stanreg <- function(mod, ...) {
	any(grepl("Intercept", names(rstanarm::fixef(mod)), ignore.case=TRUE))
}


## glmmTMB

vareffobj.glmmTMB <- function(mod, ...) {
	out <- list()
	out$coefficients <- glmmTMB::fixef(mod)$cond
	out$variance_covariance <- vcov(mod)$cond
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	out$contrasts <- attr(glmmTMB::getME(mod, "X"), "contrasts")
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.glmmTMB <- function(mod, ...) {
	mm <- glmmTMB::getME(mod,"X")
	return(mm)
}

get_contrasts.glmmTMB <- function(mod, ...) {
	attr(glmmTMB::getME(mod, "X"), "contrasts")
}

get_xlevels.glmmTMB <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

check_intercept.glmmTMB <- function(mod, ...) {
	any(grepl("Intercept", names(glmmTMB::fixef(mod)$cond), ignore.case=TRUE))
}

## lme4
vareffobj.glmerMod <- function(mod, ...) {
	out <- list()
	out$coefficients <- lme4::fixef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	out$contrasts <- attr(lme4::getME(mod, "X"), "contrasts")
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.glmerMod <- function(mod, ...) {
	mm <- lme4::getME(mod,"X")
	return(mm)
}

get_contrasts.glmerMod <- function(mod, ...) {
	attr(lme4::getME(mod, "X"), "contrasts")
}

vareffobj.merMod <- function(mod, ...) {
	out <- list()
	out$coefficients <- lme4::fixef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	out$contrasts <- attr(lme4::getME(mod, "X"), "contrasts")
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.merMod <- function(mod, ...) {
	mm <- lme4::getME(mod,"X")
	return(mm)
}

get_contrasts.merMod <- function(mod, ...) {
	attr(lme4::getME(mod, "X"), "contrasts")
}

get_xlevels.glmerMod <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

get_xlevels.merMod <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

check_intercept.glmerMod <- function(mod, ...) {
	any(grepl("Intercept", names(lme4::fixef(mod)), ignore.case=TRUE))
}

check_intercept.merMod <- function(mod, ...) {
	any(grepl("Intercept", names(lme4::fixef(mod)), ignore.case=TRUE))
}

## Statistics
get_stats.brmsfit <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

get_stats.stanreg <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

get_stats.glmmTMB <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

get_stats.glmerMod <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

get_stats.merMod <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

## vcov
vcov.vareffobj <- function(x, ...)x$variance_covariance

## Statistics
get_stats.default <- function(mod, level, dfspec, ...) {
	df <- ifelse(
		grepl("df.residual", paste(names(mod), collapse=""))
		, mod$df.residual, dfspec
	)
	mult <- qt(1 - (1 - level)/2, df)
	return(mult)
}

## sigma
get_sigma.default <- function(mod, ...) {
	stats::sigma(mod, ...)
}

get_sigma.stanreg <- function(mod, ...) {
	rand_comp <- rstanarm::VarCorr(mod)
	sigma <- unlist(lapply(names(rand_comp), function(x)attr(rand_comp[[x]], "stddev")))
	total_sd <- sqrt(sum(sigma^2))
	total_sd
}

get_sigma.glmmTMB <- function(mod, ...) {
	rand_comp <- glmmTMB::VarCorr(mod)$cond
	sigma <- unlist(lapply(names(rand_comp), function(x)attr(rand_comp[[x]], "stddev")))
	total_sd <- sqrt(sum(sigma^2))
	total_sd
}

get_sigma.merMod <- function(mod, ...) {
	rand_comp <- lme4::VarCorr(mod)
	sigma <- unlist(lapply(names(rand_comp), function(x)attr(rand_comp[[x]], "stddev")))
	total_sd <- sqrt(sum(sigma^2))
	total_sd
}

get_sigma.glmerMod <- function(mod, ...) {
	rand_comp <- lme4::VarCorr(mod)
	sigma <- unlist(lapply(names(rand_comp), function(x)attr(rand_comp[[x]], "stddev")))
	total_sd <- sqrt(sum(sigma^2))
	total_sd
}

## Include random effects
includeRE.default <- function(mod, ...) {
	re <- 0
	return(re)
}

includeRE.glmmTMB <- function(mod, ...) {
	# ee <- mod1$obj$env
	# vv <- ee$last.par.best
	# vv[names(vv) == "b"]
#	ran_eff <- as.data.frame(ranef(mod))
#	ran_eff <- ran_eff[, "condval", drop=FALSE]
	ran_eff <- mod$obj$env$last.par.best
	ran_eff <- ran_eff[names(ran_eff)=="b"]
	re <- as.vector(glmmTMB::getME(mod, "Z") %*% ran_eff)
	return(re)	
}

includeRE.merMod <- function(mod, ...){
#	ran_eff <- as.data.frame(ranef(mod))
#	ran_eff <- ran_eff[, "condval", drop=FALSE]
	ran_eff <- lme4::getME(mod, "b") 
	re <- as.vector(lme4::getME(mod, "Z") %*% as.matrix(ran_eff))
	return(re)	
}
		
includeRE.glmerMod <- function(mod, ...){
	# getME(mod, "b")
#	ran_eff <- as.data.frame(ranef(mod))
	ran_eff <- lme4::getME(mod, "b") #ran_eff[, "condval", drop=FALSE]
	re <- as.vector(lme4::getME(mod, "Z") %*% as.matrix(ran_eff))
	return(re)	
}

includeRE.stanreg <- function(mod, ...){
	ran_eff <- as.data.frame(rstanarm::ranef(mod))
	ran_eff <- ran_eff[, "condval", drop=FALSE]
	re <- as.vector(rstanarm::get_z(mod) %*% as.matrix(ran_eff))
	return(re)	
}

includeRE.brmsfit <- function(mod, ...){
	Z <- brms::prepare_predictions(mod, ...)$dpars$mu$re$Z
	ran_eff <- brms::ranef(mod)
	out <- sapply(names(ran_eff), function(x){
		z <- Z[[x]]
		reff <- as.data.frame(ran_eff[[x]])
		reff <-as.list( reff[, grep("Estimate\\.", colnames(reff), value=TRUE)])
		reff <- as.matrix(do.call("c", reff))
		re <- as.vector(z %*% reff)
	}, simplify = FALSE)
	out <- do.call("cbind", out)
	out <- rowSums(out)
	return(out)
}
