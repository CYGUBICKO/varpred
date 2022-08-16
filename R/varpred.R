#' Prediction and effect estimates
#'
#' Computes central estimates, prediction and effect estimates for a particular focal predictor.
#'
#' @details
#' The central estimates (often called effect or prediction) describe how the fitted model responds to the changes in the focal predictor. The other associated quantities are the prediction and effect estimates:
#' * **prediction estimates**: incorporate all the sources of uncertainty in the model. Important if our goal is to do prediction. 
#' * **effect estimates**: incorporate uncertainty due to focal predictor only. Focus on visualizing the \emph{effect} of the focal predictor.
#'
#' The default approaches to compute these quantities involves averaging the non-focal linear predictors (columns of \code{\link[stats]{model.matrix}} corresponding to non-focal predictors) -- \emph{mean-based} approach. An alternative is the \emph{observed-value-based} approach which computes the estimates over the entire population of the non-focal predictors and then averages them over the levels of the focal predictors. The later approach is more appropriate for a model involving non-linear link function with non-focal predictors and/or random effects. See \code{vignette("vapred_intro", package="varpred")}) for more details.
#'
#' The current version supports:
#' * lm and glm
#' * lme4
#' * glmmTMB
#' * rstanarm
#'
#' objects.
#'
#' @param mod fitted model object. See details for supported class of models.
#' @param focal_predictors a character vector of one or more predictors. For a model with an interaction, the interacting variables are specified as a vector,  for example \code{~x1*x2} will be \code{c("x1", "x2")}. If no interactions are present in the model, specifying a vector of variables compares predictions between them.
#' @param x.var a character specifying the predictor to define the x variable (horizontal axis on the plot). The default is \code{NULL}, of which the first predictor in \code{focal_predictors} is used. Ignored if there is a single \code{focal_predictors}.
#' @param type a character specifying the desired prediction. \code{type = "response"} applies inverse transformation of the link functions, if exists. \code{type = "link"} requests the results as a linear predictor.
#' @param isolate logical. If \code{TRUE} (default), computes effect estimates otherwise it computes prediction estimates. See details.
#' @param isolate.value numeric (default \code{isolate.value = NULL}). If \code{isolate = TRUE}, otherwise ignored, is the value to use as the \emph{anchor}. The default value, computed internally, is the average of the linear predictor(s) corresponding to focal predictors.
#' @param level desired confidence interval for computing the confidence intervals. Default is \code{0.95}.
#' @param steps number of points to evaluate numerical predictors in \code{focal_predictors}. The default is \code{100}. Increase for smooth curves. Unique levels of \code{focal_predictors} are used in the case categorical predictors.  
#' @param at default \code{NULL}. Otherwise, is a named \code{list} specifying points to evaluate \code{focal_predictors}. The names in the list should match the names used in \code{focal_predictors}. If \code{NULL}, the levels are internally generated using quantile, see \code{\link[stats]{quantile}}.
#' @param dfspec default \code{100}. Specified degrees of freedom for a model which do not return \code{df}. This is used in computation of confidence intervals.
#' @param true.beta default \code{NULL}. If specified, used as model coefficient estimates and should be in the same order as the vector of coefficients from the model object. Useful when comparing model estimates to the "truth" (simulation values).
#' @param vcov. a function or a matrix. If a function, it is used to compute the variance-covariance matrix of the model coefficients. The function should take the model as it's first (or maybe only) argument. A matrix of variance-covariance matrix of the estimated coefficient can also be used. Otherwise \code{vcov(mod)} is used internally. Customized \code{vcov.} can be used to generate effect estimates if the columns corresponding to the non-focal predictors are all zero. However, with this approach, the predictors should be properly scaled. See examples.
#' @param internal logical. If \code{TRUE}, the entries of the non-focal predictor (see x.var) in the variance-covariance matrix are internally zeroed-out using \code{\link[varpred]{zero_vcov}}. Default is \code{FALSE}.
#' @param avefun the averaging scheme (function) to be used in generating reference point for non-focal predictors. Default is \code{mean}.
#' @param offset a function or a value.
#' @param bias.adjust specifies the bias correction method. If "none" (default), no bias correction method is applied; if "taylor", second-order Taylor approximation is used; if "observed", all the values of non-focal predictors are used. See details and examples.
#' @param sigma standard deviation used if \code{bias.adjust="taylor"}. If \code{NULL} (default), \code{\link[stats]{sigma}} or \code{VarCorr} is used.
#' @param include.re logical. Default is \code{FALSE}. If \code{TRUE}, the random effects components of mixed models is included.
#' @param modelname character string naming \code{varpred} objects. Useful when comparing several objects.
#' @param returnall logical. If \code{TRUE}, all other named computed quantities are also returned. 
#'
#' @seealso
#'\code{\link[varpred]{plot.varpred}}
#'
#' @examples
#'
#' # Set theme for ggplot. Comment out if not needed
#' varefftheme()
#' set.seed(101)
#' N <- 100
#' x1_min <- 1
#' x1_max <- 9
#' b0 <- 0.3
#' b1 <- 0.1
#' b2 <- -0.6
#' b3 <- 0.01
#' x2_levels <- factor(c("A", "B", "D"))
#' df <- expand.grid(x1u = runif(n=N, min=x1_min, max=x1_max)
#' 	, x2u = x2_levels
#' )
#' X <- model.matrix(~x1u + x2u, df)
#' betas <- c(b0, b1, b2, b3)
#' df$y <- rnorm(nrow(df), mean= X %*% betas, sd=1)
#' df2 <- df
#' df <- transform(df
#' 	, x1c = drop(scale(x1u, scale=FALSE))
#' )
#' head(df)
#'
#' # Unscaled model
#' m1u <- lm(y ~ x1u + x2u, df)
#' # Predictor rffects of x1u
#' pred1u <- varpred(m1u, "x1u")
#' plot(pred1u)
#'
#' # Scaled model (x1 centered = x1 - mean(x1))
#' m1c <- lm(y ~ x1c + x2u, df)
#' # All uncertainities included
#' pred1c <- varpred(m1c, "x1c")
#' plot(pred1c)
#'
#' # Centered predictor effects
#' # Results similar to m1c
#' # Using zero_vcov by specifying vcov.
#' vv <- zero_vcov(m1c, "x1c")
#' pred2c <- varpred(m1c, "x1c", vcov. = vv)
#' plot(pred2c)
#'
#' # Using mean centering (isolate)
#' pred3c <- varpred(m1u, "x1u", isolate = TRUE)
#' plot(pred3c)
#' all.equal(pred2c$pred[,-1], pred3c$pred[,-1], check.attributes = FALSE)
#'
#' # Compare across groups
#' pred4c <- varpred(m1c, c("x1c", "x2u"), x.var = "x1c", isolate = TRUE)
#' plot(pred4c)
#' 
#' @importFrom stats model.frame model.matrix vcov .getXlevels as.formula coef coefficients delete.response formula qt setNames terms
#'
#'
#' @export
#'
#' @docType package
#' @name varpred


varpred <- function(mod
	, focal_predictors
	, x.var = NULL
	, type = c("response", "link")
	, isolate = TRUE
	, isolate.value = NULL
	, level = 0.95
	, steps = 100
	, at = list()
	, dfspec = 100
	, true.beta=NULL
	, vcov. = NULL
	, internal = FALSE
	, avefun = mean
	, offset = NULL
	, bias.adjust = c("none", "taylor", "observed")
	, sigma = NULL 
	, include.re = FALSE
	, modelname = NULL
	, returnall = FALSE) {
	
	bias.adjust <- match.arg(bias.adjust)

	mod <- prepmod(mod)
	vareff_objects <- vareffobj(mod)
	betahat <- coef(vareff_objects)
	if (!is.null(true.beta)) {
		if (length(betahat) != length(true.beta)) {
			stop("true.beta must of the same length and order as the model coefficients")
		} else {
			betahat <- true.beta
		}
	}
	mod_names <- get_vnames(mod)
	vnames <- mod_names$vnames
	vnames_all <- mod_names$vnames_all
	termnames <- mod_names$termnames
	Terms <- mod_names$Terms
	focal.predictors <- NULL
	rTerms <- delete.response(Terms)
	for (focal in focal_predictors){
		if (any(vnames_all %in% focal)) {
			check_vars <- vnames_all %in% focal
		} else {
			check_vars <- termnames %in% focal
		}
		if (!any(check_vars)) stop(paste0(focal, " not in the model"))
		focal.predictors[[focal]] <- unique(vnames_all[check_vars])
	}
	
	if (!is.null(x.var) & !any(focal.predictors %in% x.var) & length(focal.predictors)>1L) 
		stop(paste0(x.var, " not in ", focal.predictors))


	n.focal <- length(focal.predictors)
	if (is.null(x.var) & n.focal>1L) {
		x.var <- focal.predictors[[2]]
		message(paste0("x.var was not specified, ", x.var, " is used instead."))
	} else if (is.null(x.var)) {
		x.var <- focal.predictors[[1]]
	}
	
	..focal.predictors <- focal.predictors
	use.new <- FALSE
	if (bias.adjust=="observed") {
		if (n.focal>1) {
			x.joint <- unlist(focal.predictors)[!unlist(focal.predictors) %in% x.var]
			..focal.predictors <- x.var
		} else {
			x.joint <- NULL
		}
		use.new <- TRUE
	}

	.contr <- vareff_objects$contrasts
	model_frame_objs <- clean_model(focal.predictors=..focal.predictors
		, mod = mod
		, xlevels=at
		, default.levels=NULL
		, formula.rhs=rTerms
		, steps=steps
		, x.var=x.var
		, typical=avefun
		, vnames=vnames
		, bias.adjust = bias.adjust
		, isolate.value=isolate.value
	)

	formula.rhs <- formula(vareff_objects)[c(1,3)]
	excluded.predictors <- model_frame_objs$excluded.predictors
	predict.data <- model_frame_objs$predict.data
	factor.levels <- model_frame_objs$factor.levels
	factor.cols <- model_frame_objs$factor.cols
	n.focal <- if(use.new) n.focal else model_frame_objs$n.focal
	x <- model_frame_objs$x
	X.mod <- model_frame_objs$X.mod
	cnames <- model_frame_objs$cnames
	X <- model_frame_objs$X
	x.var <- model_frame_objs$x.var
	typical <- avefun
	.link <- vareff_objects$link
	.family <- vareff_objects$link$family
	factor.weights <- model_frame_objs$factor.weights
	factor.type <- model_frame_objs$factor.type

	# Stats
	mult <- get_stats(mod, level, dfspec)
			
	## None or taylor bias adjustment
	if (bias.adjust %in% c("none", "taylor")) {
		mf <- model.frame(rTerms, predict.data, xlev = factor.levels, na.action=NULL)
		mod.matrix <- model.matrix(formula.rhs
			, data=mf
			, contrasts.arg=.contr
		)
		mm <- get_model_matrix(mod
			, mod.matrix
			, X.mod
			, factor.cols
			, cnames
			, focal.predictors
			, excluded.predictors
			, typical
			, apply.typical.to.factors=TRUE
			, factor.type=factor.type
			, x.var=x.var
			, factor.weights=factor.weights
			, vnames=vnames
		)
		
		# Predictions
		col_mean <- apply(mm, 2, typical)
		pse_var <- mult*get_sderror(mod=mod, vcov.=vcov., mm=mm, col_mean=col_mean, isolate=isolate
			, isolate.value=isolate.value, internal=internal, vareff_objects=vareff_objects, x.var=x.var
			, typical=typical, formula.rhs=formula.rhs
			, mf=predict.data # 2022 Mar 15 (Tue): re-evaluate within the isolate.value
			, rTerms=rTerms
			, factor.levels=factor.levels
			, bias.adjust=bias.adjust
			, X.mod=X.mod
			, factor.cols=factor.cols
			, cnames=cnames
			, focal.predictors=focal.predictors
			, excluded.predictors=excluded.predictors
			, apply.typical.to.factors=TRUE
			, factor.type=factor.type
			, factor.weights=factor.weights
			, vnames=vnames
			, .contr=.contr
		)
		off <- get_offset(offset, mf)
		pred <- off + as.vector(mm %*% betahat)
		if (include.re) {
			re <- includeRE(mod)
		}
		if (include.re && all(re!=0) && bias.adjust=="none") {
			predict.data <- predict.data[rep(1:length(pred), each=length(re)), 1:n.focal, drop=FALSE]
			pred <- pred[rep(1:length(pred), each=length(re))]
			pred <- pred + re
			lwr <- pred - pse_var
			upr <- pred + pse_var
		} else {
			lwr <- pred - pse_var
			upr <- pred + pse_var
		}
		
		if (bias.adjust == "taylor") {
			if (is.null(sigma)) {
				sigma <- get_sigma(mod)
			} else {
				if (!is.numeric(sigma)) stop ("sigma must be a number!")
			}
			.link <- .make.bias.adj.link(.link, sigma)
		} 
	} else if (bias.adjust=="observed") {
		
		x.focal <- predict.data$focal
		x.excluded <- predict.data$excluded
		pred_obj_all <- pop.bias.adjust(x.focal=x.focal
			, x.excluded=x.excluded
			, betahat=betahat
			, formula.rhs=formula.rhs
			, rTerms=rTerms
			, factor.levels=factor.levels
			, contr=.contr
			, mult=mult
			, vnames=vnames
			, offset=offset
			, mod=mod
			, vcov.=vcov.
			, isolate=isolate
			, isolate.value=isolate.value
			, internal=internal
			, vareff_objects=vareff_objects
			, x.var=x.var
			, typical=typical
			, include.re=include.re
			, x.joint=x.joint
		)
		pred_obj <- pred_obj_all$pred_df
		predict.data <- pred_obj[, colnames(pred_obj)[!colnames(pred_obj) %in% c("pred", "pse_var", "lwr", "upr")], drop=FALSE]
		pse_var <- pred_obj$pse_var
		off <- pred_obj_all$off
		pred <- pred_obj$pred 
		lwr <- pred_obj$lwr
		upr <- pred_obj$upr
		mm <- NULL
	} 

	
	attr(mm,"contrasts") <- NULL
	attr(mm, "assign") <- NULL
	out <- list(term = paste(focal.predictors, collapse="*")
		, formula = formula(mod)
		, response = get_response(mod)
		, variables = x
		, fit = pred
		, x = predict.data[, 1:n.focal, drop=FALSE]
		, model.matrix = mm
		, data = X
		, x.var=x.var
		, se = pse_var/mult
		, lwr = lwr
		, upr = upr
		, family = .family
		, link = .link
		, offset=off
		, bias.adjust.sigma = if (bias.adjust %in% c("none", "observed", "quantile")) NULL else sigma
	)

	## Organize
   type <- match.arg(type)
   linkinv <- if (is.null(out$link$linkinv)) I else out$link$linkinv
   linkmu.eta <- if(is.null(out$link$mu.eta)) I else out$link$mu.eta
   
	temp <- out$x
	for (var in names(temp)){
	 if (is.factor(temp[[var]])){
		# handle factors with "valid" NA level
		temp[[var]] <- addNA(temp[[var]]) 
	 }
	}
	
	out$x <- temp
	result <- switch(type
	  	, response= { if (is.null(out$se)) 
		  	data.frame(out$x, fit=as.vector(transform(out$fit)))
			else {
					data.frame(out$x, fit=as.vector(linkinv(out$fit))
						, se = as.vector(linkmu.eta(out$fit) * out$se)
						, lwr=as.vector(linkinv(out$lwr))
						, upr=as.vector(linkinv(out$upr))
					)
				}
		} , link = { if (is.null(out$se)) 
		  	data.frame(out$x, fit=as.vector(out$fit))
		 	else 
				data.frame(out$x, fit=as.vector(out$fit), se=as.vector(out$se)
					, lwr=as.vector(out$lwr), upr= as.vector(out$upr))}
	)

	if ((bias.adjust=="observed")||include.re||(include.re && all(re!=0) && bias.adjust=="none")){
		form <- as.formula(paste0(".~", paste0(colnames(out$x), collapse = "+")))
		result <- aggregate(form, result, FUN=function(x)mean(x, na.rm=TRUE))
	} 

	if (!is.null(modelname)) {
		result$model <- modelname
	}
	attr(result, "type") <- type
	attr(result, "focal") <- unlist(focal.predictors)
	attr(result, "response") <- out$response
	attr(result, "x.var") <- out$x.var 
	attr(result, "modelname") <- modelname
	if (returnall) {
		res <- list(preds = result, offset=out$offset, bias.adjust.sigma=out$bias.adjust.sigma, raw=out)
	} else {
		res <- list(preds = result, offset=out$offset, bias.adjust.sigma=out$bias.adjust.sigma)
	}
	res$call <- match.call()
	class(res) <- "varpred"
	return(res)
}


#' Zero-out entries of non-focal entries of variance-covariance matrix.
#'
#' Transforms entries of variance-covariance of the coefficients corresponding to the non-focal variables to zero.
#'
#' @param mod fitted model object or a full variance-covariance matrix with terms appearing the same way they appear in the coefficients.
#' @param focal_vars a character vector specifying the variable(s) appearing on the right side of the formula of the fitted model. The entries of this variable with be non-zero.
#' @param complete logical indicating if the full variance-covariance matrix should be returned.
#'
#' @return a variance-covariance matrix of the estimated parameters with non-zero entry corresponding to the focal variable.
#'
#' @examples
#' set.seed(4567)
#' x <- rnorm(100, 3, 5)
#' y <- 0.4 + 0.7*x + rnorm(100)
#' df <- data.frame(y = y, x = x)
#' m1 <- lm(y ~ x, df)
#' print(zero_vcov(m1, "x"))
#'
#' @export
zero_vcov <- function(m, focal_vars, complete) {
	if (is.matrix(m)|is.data.frame(m)) {
		v <- m
	} else {
		assign <- get_vnames(m)$vnames
		check_vars <-  grepl(paste0(focal_vars, collapse="|"), assign) # assign %in% focal_vars
		if (!any(check_vars)) stop(paste0(focal_vars, " not in the model formula"))
		focal_vars <- names(assign)[check_vars]
		v <- stats::vcov(m)
		if(inherits(m, "glmmTMB")){
			v <- v$cond
		}
	}
	focal_var <- v[focal_vars,focal_vars]
	v[] <- 0 ## set all to zero, preserving dims/names
	v[focal_vars, focal_vars] <- focal_var
	return(v)
}

#' Recover data from the data from the model 
#'
#' @param mod fitted model
#' @param optional character vector or formula specifying the predictors. Important when the transformation are applied in the formula. 
#' @param envir data environment 
#' @param ... for future implementations
#'
#' @details
#' It uses the fitted model and the global environment to reconstruct the data used in the model. If \code{data} option is specified in the model formula, a dataframe with columns corresponding to the variable in the formula is returned. Any transformation, e.g. \code{log} specified in the formula terms is not evaluated on the returned data frame. However, if no is provided, the dataframe is constructed from the formula terms with all transformations evaluated.
#'
#' @return a dataframe
#'
#' @examples
#' set.seed(4567)
#' x <- rnorm(100, 3, 5)
#' y <- 0.4 + 0.7*x + rnorm(100)
#' df <- data.frame(y = y, x = x)
#' m1 <- lm(y ~ x, df)
#' d1 <- recoverdata(m1)
#' head(d1)
#' m2 <- lm(y ~ x)
#' d2 <- recoverdata(m2)
#' head(d2)
#'
#' @export 
#'

recoverdata <- function(mod, extras = NULL, envir = environment(formula(mod)), ...) {
	f <- formula(mod)
	## brms does not have call object
	if (inherits(mod, "brmsfit")) {
		data <- eval(mod$data, envir)
	} else {
		data <- eval(getCall(mod)$data, envir)
	}
	if (is.null(data)) {
      if (is.null(extras)) {
         ## df <- eval(bquote(model.frame(.(f))), envir)
			df <- eval(call("model.frame", f), envir) 
		} else {
			df <- eval(call("expand.model.frame", f, extras = extras), envir) 
		}
	} else {
		df <- eval(call("model.frame", data = data, drop.unused.levels = TRUE), envir)
	}
	if (!is.null(extras) || !is.null(data)) {
		resp <- get_response(mod)
		xvars <- all.vars(delete.response(terms(mod)))
		df <- df[, colnames(df) %in% c(resp, xvars), drop=FALSE]
	}
	return(df)
}

#' Combine varpred objects
#'
#' Combines and plots comparison plots for more than two named varpred objects. 
#'
#' @export 
#'

combinevarpred <- function(vlist, lnames=NULL, plotit=FALSE, addmarginals=FALSE, margindex, ...) {
	# TODO: Automatically check the model names, if missing add automatically
	if (!is.list(vlist))stop("vlist should be a list of objects of class varpred")
	nobjs <- length(vlist)
	if (!is.null(lnames)) {
		if (nobjs==1) lnames <- rep(lnames, nobjs)
	} 
	preds <- lapply(1:nobjs, function(v){
		pp <- vlist[[v]]$preds
		if (!is.null(lnames)) {
			pp$.varpred <- lnames[[v]]
		}
		return(pp)
	})
	preds <- do.call("rbind", preds)
	## TODO: Use getmeans
	if (addmarginals) {
		if (missing(margindex)) margindex <- 1:nobjs
		marg_df <- lapply(margindex, function(i){
			pp <- vlist[[i]]$preds
			df <- data.frame(muy=mean(pp$fit)
#				, mux=mean(pp[[attr(pp, "x.var")]])
				, model=unique(pp$model)
			)
			if (!is.null(lnames)) {
				df$.varpred <- lnames[[i]]
			}
			return(df)
		})
		marg_df <- do.call("rbind", marg_df)
	}
	
	out <- vlist[[1]]
	out$preds <- preds
	if (plotit) {
		add_args <- list(...)
		if (length(add_args)) {
			out <- list(out)
			out[names(add_args)] <- add_args
			p <- do.call(plot, out)
		} else {
			p <- plot(out)
		}
		if (addmarginals && (class(preds[[attr(preds, "x.var")]]) %in% c("integer", "numeric", "logical"))) {
			p <- (p
				+ geom_hline(data=marg_df, aes(yintercept=muy, colour=model, linetype=model))
#				+ geom_vline(data=marg_df, aes(xintercept=mux, colour=model, linetype=model))
			)
		}
		return(p)
	} else {
		return(out)
	}
}

#' Get focal and prediction means for a varpred object
#'
#' @export

getmeans.varpred <- function(object, what=c("estimate", "focal"), focal=NULL, modelname=NULL) {
	what <- match.arg(what)
	preds <- object$preds
	if (is.null(modelname)) modelname <- attr(preds, "modelname")
	if (is.null(modelname)) modelname <- class(object)[1]
	if (what=="focal") {
		if(is.null(focal)) {
			focal <- attr(preds, "x.var")
		}
	} else {
		focal <- "fit"
	}
	out <- preds[[focal]]
	if (any(class(out) %in% c("factor", "character"))) {
		out <- as.character(unique(out))
	} else {
		out <- data.frame(fit=mean(out, na.rm=TRUE), model=modelname)
	}
	return(out)
}