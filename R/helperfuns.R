# Some of the condition code is based on R package effects (clean_model). Transfered and modified here because they are not exported in effects.

## Population-based bias adjustment
pop.bias.adjust <- function(x.focal, x.excluded, betahat, formula.rhs
	, rTerms, factor.levels, contr, offset, mult, vnames, ...
	, mod, vcov., isolate, isolate.value, internal, vareff_objects, x.var
	, typical, include.re, x.joint) {

	pred <- NULL
	mm <- get_model.mm(mod)
	if (!is.null(x.excluded)) {
		nM <- NROW(x.excluded)
		x.focal <- x.focal[rep(1:NROW(x.focal), each=nM), , drop=FALSE]
	} else {
		nM <- NROW(mm)
	}


#  	if (isolate) {
		## UPDATE: 2022 Jun 18 (Sat) 
		### Applies to both cases
		if (!is.null(x.excluded)) {
			factor.levels_update <- factor.levels
			if (!is.null(x.joint)) {
				x.focal.names <- c(colnames(x.focal), x.joint)
				x.excluded.names <- colnames(x.excluded)[!colnames(x.excluded) %in% x.joint]
			} else {
				x.focal.names <- colnames(x.focal)
				x.excluded.names <- colnames(x.excluded)
			}
			factor.levels_update[!names(factor.levels_update) %in% x.focal.names] <- NULL
			drop_index <- grep(paste(x.excluded.names, collapse = "|"), attr(rTerms, "term.labels"))
			rTerms_update <- drop.terms(rTerms, drop_index)
			formula.rhs_update <- drop.terms(terms(formula.rhs), drop_index)
			contr_update <- contr
			contr_update[!names(contr_update) %in% x.focal.names] <- NULL
			focal_mf <- model.frame(formula(rTerms_update), cbind.data.frame(x.focal, x.excluded[, x.joint, drop=FALSE]), xlev=factor.levels_update, na.action=NULL)
		} else {
			rTerms_update <- rTerms
			factor.levels_update <- factor.levels
			formula.rhs_update <- formula.rhs
			contr_update <- contr
			focal_mf <- model.frame(formula(rTerms_update), x.focal, xlev=factor.levels_update, na.action=NULL)
		}
		focal_mm <- model.matrix(formula.rhs_update, data = focal_mf, contrasts.arg = contr_update)
		focal_terms_update <- colnames(focal_mm) #attr(terms(formula.rhs_update), "term.labels")
#  	} else {
#  		focal_mf <- model.frame(mod)
#  		focal_mm <- mm
#  		focal_terms_update <- NULL
#  		contr_update <- contr
#  	}
	col_mean <- colMeans(focal_mm)
	
	pse_var <- mult*get_sderror(mod=mod
		, vcov.=vcov.
		, mm=focal_mm
		, col_mean=col_mean
		, isolate=isolate
		, isolate.value=isolate.value
		, internal=internal
		, vareff_objects=vareff_objects
		, x.var=x.var
		, typical=typical
		, formula.rhs=formula.rhs_update
		, mf=focal_mf
		, focal_terms=focal_terms_update
		, rTerms=rTerms_update
		, factor.levels=factor.levels_update
		, bias.adjust="observed"
		, .contr=contr_update
	)

	if (include.re) {
		re <- includeRE(mod)	
	} else {
		re <- 0
	}

	## Predictions
	if (!is.null(x.excluded)) {
		mf_i <- cbind.data.frame(x.focal, x.excluded)
		mf_i <- model.frame(rTerms, mf_i, xlev=factor.levels, na.action=NULL)
		mm_i <- model.matrix(formula.rhs, data = mf_i, contrasts.arg = contr)
		off <- get_offset(offset, mf_i)
		df <- data.frame(pred=off + as.vector(mm_i %*% betahat) + re
			, pse_var=as.vector(pse_var)
		)
		pred_df <- transform(df
			, lwr=pred - pse_var 
			, upr=pred + pse_var
		)
		if (is.null(x.joint)) {
			pred_df <- cbind.data.frame(x.focal, pred_df)
		} else {
			pred_df <- cbind.data.frame(mf_i[, c(x.joint, colnames(x.focal)), drop=FALSE], pred_df)
		}
	} else {
		mf_i <- model.frame(rTerms, x.focal, xlev=factor.levels, na.action=NULL)
		mm_i <- model.matrix(formula.rhs, data = mf_i, contrasts.arg = contr)
		off <- get_offset(offset, mf_i)
		df <- data.frame(pred=off + as.vector(mm_i %*% betahat) + re
			, pse_var=as.vector(pse_var)
		)
		pred_df <- transform(df
			, lwr=pred - pse_var 
			, upr=pred + pse_var
		)
		if (is.null(x.joint)) {
			pred_df <- cbind.data.frame(x.focal, pred_df)
		} else {
			pred_df <- cbind.data.frame(mf_i[, c(x.joint, colnames(x.focal)), drop=FALSE], pred_df)
		}
	}

	return(list(pred_df=pred_df, off=mean(off), dim=nM))
}

## Logistic normal approximation
logitnorm.bias.adjust <- function(pred, sigma, abs.tol=0, ...) {
	.logitnorm <- function (mu, sigma, abs.tol=abs.tol, ...) {
		fExp <- function(x) plogis(x) * dnorm(x, mean = mu, sd = sigma)
		.exp <- integrate(fExp, -Inf, Inf, abs.tol = abs.tol, ...)$value
		fVar <- function(x) (plogis(x) - .exp)^2 * dnorm(x, mean = mu, sd = sigma)
		.var <- integrate(fVar, -Inf, Inf, abs.tol = abs.tol, ...)$value
		return(list(mean = .exp, var = .var))
	}
	.pred <- numeric()
	.var <- numeric()
	for (i in 1:length(pred)) {
		out <- .logitnorm(pred[[i]], sigma[[i]], abs.tol)
		.pred[[i]] <- out$mean
		.var[[i]] <- out$var
	}
	out <- list(pred=.pred, var=.var)
	return(out)
}

## MCMCglmm vignettes: 
mcculloch.bias.adjust <- function(pred, sigma, ...) {
	out <- pred-0.5*sigma^2*tanh(pred*(1 + 2*exp(-0.5*sigma^2))/6)
	return(out)
}

diggle.bias.adjust <- function(pred, sigma, ...) {
	out <- pred/(sqrt(1 + (16*sqrt(3)/(15*pi))^2*sigma^2)) 
	return(out)
}

# Offset
get_offset <- function(offset, mf) {
	if (is.null(offset)) offset <- mean
	if (is.numeric(offset) && length(offset) == 1) {
		off <- offset
	} else if (is.function(offset)) {
		mod.off <- model.offset(mf)
		if (is.null(mod.off)) {
			off <- 0
		} else {
			off <- offset(mod.off)
		}
	} else {
		stop("offset must be a function or a number")
	}
	return(off)
}

# For bias-adjustment: coppied directry from emmeans
## Currently, we just use a 2nd-order approx for everybody:
## E(h(nu + E))  ~=  h(nu) + 0.5*h"(nu)*var(E)
.make.bias.adj.link <- function(link, sigma) {
	 if (is.null(sigma))
		  stop("Must specify 'sigma' to obtain bias-adjusted back transformations", call. = FALSE)
	 link$inv = link$linkinv
	 link$der = link$mu.eta
	 link$sigma22 = sigma^2 / 2
	 link$der2 = function(eta) with(link, 1000 * (der(eta + .0005) - der(eta - .0005)))
	 link$linkinv = function(eta) with(link, inv(eta) + sigma22 * der2(eta))
	 link$mu.eta = function(eta) with(link, der(eta) +
													  1000 * sigma22 * (der2(eta + .0005) - der2(eta - .0005)))
	 link
}

get_sderror <- function(mod, vcov., mm, col_mean, isolate, isolate.value, internal, vareff_objects, x.var, typical, formula.rhs, mf, focal_terms=NULL, rTerms, factor.levels, bias.adjust, ..., X.mod, factor.cols, cnames, focal.predictors, excluded.predictors, apply.typical.to.factors, factor.type, factor.weights, vnames, .contr, input_vars, poly_check) {
	
	if (is.null(vcov.)){
		vc <- vcov(vareff_objects)
	} else if (is.function(vcov.)) {
		vc <- vcov.(mod)
	} else if (internal) {
		vc <- zero_vcov(mod, focal_vars=x.var)	
	} else {
		vc <- vcov.
	}
  	
	if (!is.null(focal_terms)) {
		vc <- vc[focal_terms, focal_terms]
		mm <- mm[, focal_terms, drop=FALSE]
	}

	# (Centered) predictions for SEs
	## Center model matrix
	if (isolate) {
		if (bias.adjust %in% c("none", "taylor")) {
			mm_mean <- t(replicate(NROW(mm), col_mean))
		} else {
			.xx <- mf[[x.var]]
			if (is.factor(.xx)) {
				assign <- get_vnames(mod)$vnames
				check_vars <-  grepl(paste0(x.var, collapse="|"), assign)
				focal_vars <- names(assign)[check_vars]
				focal_vars <- focal_vars[focal_vars %in% names(col_mean)]
				mm_mean <- mm
				mm_mean[, focal_vars] <- col_mean[focal_vars]
			} else {
				mf[x.var] <- mean(.xx)
				mf <- model.frame(rTerms, mf, xlev = factor.levels, na.action=NULL)
				mm_mean <- model.matrix(formula.rhs, data = mf, contrasts.arg = .contr)
			}
		}
		
		if (!is.null(isolate.value) & (is.numeric(isolate.value)|is.integer(isolate.value))){
			mf[x.var] <- isolate.value
			mf <- model.frame(rTerms, mf, xlev = factor.levels, na.action=NULL)
			mod.matrix <- model.matrix(formula.rhs, data = mf, contrasts.arg = .contr)
			if (bias.adjust %in% c("none", "taylor")) {
				mm2 <- get_model_matrix(mod
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
					, input_vars=input_vars
					, poly_check=poly_check
				)
				col_mean <- apply(mm2$mm_temp, 2, typical)
				mm_mean <- t(replicate(NROW(mm), col_mean))
			} else {
				mm_mean <- mod.matrix # apply(mod.matrix, 2, typical)
			}
#			mm_mean <- t(replicate(NROW(mm), col_mean))
		}
		mm <- mm - mm_mean
	} 
#	pse_var <- sqrt(diag(mm %*% tcrossprod(data.matrix(vc), mm)))
	pse_var <- sqrt(rowSums(mm * t(tcrossprod(data.matrix(vc), mm))))
	return(pse_var)
}

get_vnames <- function(mod){
	mat <- get_model.mm(mod)
	coefnames <- colnames(mat)
	Terms <- terms(mod)
	vnames_all <- all.vars(parse(text=delete.response(Terms)))
	## FIX: 2022 Feb 01 (Tue) - Added order to pick only main effect
	order <- attr(Terms, "order")
	vnames <- vnames_all[order==1]

	termnames <- attr(Terms, "term.labels") # Model based names
	termnames <- gsub(" ", "", termnames)
	assign <- attr(mat, "assign")
	if(any(assign==0)) {
		vnames <- c("(Intercept)", vnames)
		termnames <- c("(Intercept)", termnames)
		assign <- assign + 1
	}
	
	## FIXME: assign doesn't not group _I(_ terms
	if (any(grepl("^I\\(", termnames))) {
		assign2 <- unlist(lapply(1:length(termnames), function(x){
			if(grepl(paste0(vnames, collapse="|"), termnames[[x]])){
				return(gsub(".*I\\(|\\^.*", "", termnames[[x]]))
			}
		}))
		vnames <- setNames(vnames[assign2], coefnames)
	} else {
		vnames <- setNames(vnames[assign], coefnames)
	}

	termnames <- setNames(termnames[assign], coefnames)
	# In case there are interactions
	vnames[is.na(vnames)] <- termnames[is.na(vnames)]
	return(list(vnames = vnames, termnames = termnames, Terms = Terms, vnames_all=vnames_all))
}

check_numeric <- function(xvar, mod) {
	xlevels <- get_xlevels(mod)
	is.null(xlevels[[xvar]])
}

check_factor <- function(xvar, mod) {
  xvar %in% names(get_contrasts(mod)) #names(attr(model.matrix(mod), "contrasts"))
}

is.numeric.predictor <- function(predictor, model) {
  is.null(model$xlevels[[predictor]])
}

# for character and logical predictors

is.factor <- function(x) inherits(x, "factor") || ((is.character(x) || is.logical(x)) && is.vector(x))

levels.character <- function(x) {
  levs <- unique(x)
  sort(levs[!is.na(levs)])
}

levels.logical <- function(x) {
  c("FALSE", "TRUE")
}

subscripts <- function(index, dims){
  subs <- function(dims, index){
    dim <- length(dims)
    if (dim == 0) return(NULL)
    cum <- c(1,cumprod(dims))[dim]
    i <- index %/% cum
    if (index %% cum != 0) i <- i + 1
    c(i, subs(dims[-dim], index - (i - 1)*cum))
  }
  rev(subs(dims, index))
}

get_response <- function (mod, ...) deparse(attr(terms(mod), "variables")[[2]])
## a <- attributes(terms(mod))
## deparse(a$variables[[1+a$response]])  ## offset of 1 to skip the head of the expression ...
## deparse(formula(mod)[[2]]) ???
## or ... use pcoxtime getResponse()!


get_termnames <- function (mod, ...) {
  termnames <- gsub(" ", "", labels(terms(mod)))
  if (check_intercept(mod)) c("(Intercept)", termnames)
  else termnames
}

match_vnames <- function(name, expressions){
  scratch <- "zAMIjw4RN3" # randomly generated string
  name <- gsub("\\.", scratch, name)
  expressions <- gsub("\\.", scratch, as.character(expressions))
  a <- !grepl(paste("[.]+", name, sep=""), expressions)
  b <- !grepl(paste(name, "[.]+", sep=""), expressions)
  c <- grepl(paste("\\b", name, "\\b", sep=""), expressions)
  a & b & c
}

get_strangers <- function(mod, focal.predictors, excluded.predictors){
  names <- get_termnames(mod)
  if (check_intercept(mod)) names <- names[-1]
  sel <- apply(sapply(excluded.predictors, match_vnames, expressions=names), 1, any)
  (1:length(sel))[sel]
}

matrix.to.df <- function(matrix, colclasses){
  opt <- options(warn = -1)
  on.exit(options(opt))
  ncol <- ncol(matrix)
  colnames <- colnames(matrix)
  colclasses[sapply(colclasses, function(x) "integer" %in% x)] <- "numeric"
  result <- vector(mode="list", length=ncol)
  names(result) <- colnames
  for (j in 1:ncol){
    result[[j]] <- matrix[, j]
    class <- colclasses[[colnames[j]]]
    result[[colnames[j]]] <- if ("numeric" %in% class) {
      decChar <- getOption('OutDec')
      if (decChar == '.') as.numeric(result[[colnames[j]]])
      else as.numeric(gsub(decChar, '.', matrix[,j]))
    }
    else if ("ordered" %in% class) ordered(result[[colnames[j]]])
    else if ("factor" %in% class) factor(result[[colnames[j]]]) 
    else result[[colnames[j]]]
  }
  as.data.frame(result)
}

clean_model <- function(focal.predictors, mod, xlevels
	, default.levels, formula.rhs, steps, x.var, typical
	, vnames, bias.adjust, isolate.value, input_vars){
  
  ## FIXME: How to assign NA to a lme4 object 
  if (!isS4(mod)) {
	  if ((!is.null(mod$nan.action)) && inherits(mod$na.action, "exclude"))
		 class(mod$na.action) <- "omit"
  }

# terms <- attr(formula.rhs, "term.labels")
  all.predictors <- all.vars(formula.rhs)
  if (is.numeric(xlevels)){
    if (length(xlevels) > 1 || round(xlevels != xlevels)) stop("xlevels must be a single whole number or a list")
    xlevs <- list()
    for (pred in all.predictors){
      xlevs[[pred]] <- xlevels
    }
    xlevels <- xlevs
  }

#  all.predictors <- all.vars(formula.rhs) #predictors
  check.vars <- !(focal.predictors %in% all.predictors)
  excluded.predictors <- setdiff(all.predictors, focal.predictors)
  number.bad <- sum(check.vars)
  if (any(check.vars)) {
    message <- if (number.bad == 1) paste("the following predictor is not in the model:", 
                                          focal.predictors[check.vars])
    else paste("the following predictors are not in the model:", 
               paste(focal.predictors[check.vars], collapse=", "))
    stop(message)
  }
  X.mod <- get_model.mm(mod)
  cnames <- colnames(X.mod)
  factor.cols <- rep(FALSE, length(cnames))
  names(factor.cols) <- cnames
  
  components <- unlist(strsplit(cnames, ':'))
  poly_check <- NULL
  for (name in all.predictors){
    ff_check <- check_factor(name, mod)
    if (ff_check) {
      factor.cols[grep(paste("^", name, sep=""), cnames)] <- TRUE
    } else if (ff_check && (!any(name %in% components))) {
	 	factor.cols[grep(paste0(":", name, "|", name, ":"), cnames)] <- FALSE
	 }
	 ## Handling model center for *((
	 if (!ff_check) {
	 	factor.cols[grep(paste0(":", name, "|", name, ":"), cnames)] <- TRUE # FALSE for product of means as opposed to mean of products
#	 	factor.cols[grep(paste0(":", name, "$"), cnames)] <- FALSE
      ## FIXME: Best way to identify all polynomial/function terms in a model
		all_poly <- grep(paste("^[aA-zZ]+\\(", name, sep=""), cnames)
		poly_check[[name]] <- length(all_poly) > 0
		## Start: 2023 Mar 01 (Wed): Fixing poly centering	
		## End: 2023 Mar 01 (Wed)
		factor.cols[all_poly] <- TRUE
	 } 
  }


if (input_vars) {
 	factor.cols[grep(":", cnames)] <- FALSE   
	## FIXME: For compatibility with emmeans. Otherwise uncomment above block and comment the block below
	  for (name in all.predictors){
	    if (check_factor(name, mod)) {
	      factor.cols[names(grep(name, vnames, value=TRUE))] <- TRUE
	    }
	  }
}

#  if (handle.inter=="effects") {
#     factor.cols[grep(":", cnames)] <- FALSE 
#  }

  X <- recoverdata(mod=mod, extras=all.predictors)
  
  which.matrices <- sapply(X, function(x) is.matrix(x) && ncol(x) == 1)
  if (any(which.matrices)){
    nms <- names(which.matrices[which.matrices])
    msg <- if (length(nms) > 1){
      paste("the predictors", paste(nms, collapse=", "), "are one-column matrices that were converted to vectors")
    } else {
      paste("the predictor", nms, "is a one-column matrix that was converted to a vector")
    }
    warning(msg)
    for (nm in nms){
      X[, nm] <- as.vector(X[, nm])
    }
  }
  
  for (name in all.predictors){
    if (check_factor(name, mod) && is.null(xlevels[[name]])) {
      xlevels[[name]] <- levels(X[, name]) 
    }
  }
  bad <- sapply(X[, all.predictors, drop=FALSE], function(x) !(is.factor(as.vector(x)) || is.numeric(as.vector(x))))
  if (any(bad)){
    message <- if (sum(bad) == 1) paste("the following predictor isn't a factor, logical, character, or numeric:", 
                                        all.predictors[bad])
    else paste("the following predictors aren't factors, logical, character, or numeric:", 
               paste(all.predictors[bad], collapse=", "))
    stop(message)
  }
  x <- list()
  factor.levels <- list()
  factor.weights <- list()
  if(length(xlevels)==0 & length(default.levels) == 1L) xlevels <- default.levels
  if(is.numeric(xlevels) & length(xlevels) == 1L){
    levs <- xlevels
    for(name in focal.predictors) xlevels[[name]] <- levs
  }

  oldlevels <- get_xlevels(mod)
  for (name in focal.predictors){
    levels <- oldlevels[[name]]
    if(is.null(levels)) levels <- oldlevels[[paste("factor(",name,")",sep="")]]
    fac <- !is.null(levels)
	 if (!fac) {
		levels <- if (is.null(xlevels[[name]])){
			if (steps==100) {
				steps <- min(c(steps, length(unique(X[,name]))))
			}
			if (x.var!=name){
				quant <- seq(0, 1, length.out=5)
			} else {
				quant <- seq(0, 1, length.out=steps)
			}
			## 2022 Jan 26 (Wed): Make sure they are unique
			.x <- unique(quantile(X[,name], quant, names=FALSE))
		} else {
		  if(length(xlevels[[name]]) == 1L) { 
			# seq(min(X[, name]), max(X[,name]), length=xlevels[[name]])
			  .x <- unique(xlevels[[name]])
			} else {
			# 2022 Oct 11 (Tue): We need focal as specified
			#  .x <- unique(xlevels[[name]])
			  .x <- xlevels[[name]]
			}
		}
	 } else {
		factor.levels[[name]] <- levels
		equal.weight <- 1/length(levels)
		proportional.weight <- mean(levels[1]==X[,name])
		factor.weights[[name]] <- list(name=name, equal=equal.weight
			, proportional=proportional.weight, level.prop=c(prop.table(table(X[,name])))
		)
	 }
    if (!fac) {
		if (!is.null(isolate.value) & (is.numeric(isolate.value)|is.integer(isolate.value))) {
			levels <- c(levels, isolate.value)
		} else {
			levels <- c(levels, typical(levels))
		}
	 }
	 x[[name]] <- list(name=name, is.factor=is.factor(X[, name]), levels=levels)
  }

  colclasses <- lapply(X, class)
  colclasses[colclasses == "matrix"] <- "numeric"
  colclasses[colclasses == "array"] <- "numeric"
  
  x.excluded <- list()
  for (name in excluded.predictors){
    levels <- oldlevels[[name]] 
    if (is.logical(X[, name])) levels <- c("FALSE", "TRUE")
    fac <- !is.null(levels)
    level <- if (fac) {
	 		if (bias.adjust=="observed") {
				as.vector(X[, name])
			} else {
	 			levels[1]
			}
	 } else {
	 	if (bias.adjust=="quantile") {
			steps <- min(c(steps, length(unique(X[,name]))))
			quant <- seq(0, 1, length.out=steps)
			unique(quantile(X[,name], quant, names=FALSE))
		} else if (bias.adjust=="observed") {
			as.vector(X[, name])
		} else {
			typical(X[, name])	
		}
	 }
	if (fac) { 
		factor.levels[[name]] <- levels
		equal.weight <- 1/length(levels)
		proportional.weight <- mean(levels[1]==X[,name])
		factor.weights[[name]] <- list(name=name, equal=equal.weight
			, proportional=proportional.weight, level.prop=c(prop.table(table(X[,name])))
		)
	}
    x.excluded[[name]] <- list(name=name, is.factor=fac,
                               level=level)
  }
  n.focal <- length(focal.predictors)
  n.excluded <- length(excluded.predictors)
  n.vars <- n.focal + n.excluded
  
  if (bias.adjust=="observed") {
	  if (n.excluded>0) {
		  ..excluded <- do.call("data.frame", sapply(x.excluded, function(x) x$level, simplify=FALSE))
		  ..col.excluded <- sapply(x.excluded, function(x) x$name)
		  ..excluded.data <-  matrix.to.df(..excluded, colclasses=..col.excluded)
	  } else {
	  	..excluded.data=NULL
	  }
	  ..focal <- do.call("expand.grid", sapply(focal.predictors, function(j) x[[j]]$levels, simplify=FALSE))
	  ..col.focal <- sapply(x, function(x) x$name)
	  ..focal.data <-  matrix.to.df(..focal, colclasses=..col.focal)
	  predict.data <-  list(focal=..focal.data, excluded=..excluded.data)
  } else {
	  dims <- sapply(x, function(x) length(x$levels))
	  len <- prod(dims)
	  predict.data <-matrix('', len, n.vars)
	  excluded <- sapply(x.excluded, function(x) x$level)
	  if (is.list(excluded)) excluded <- do.call("cbind", excluded)
	  for (i in 1:len){
		 subs <- subscripts(i, dims)
		 for (j in 1:n.focal){
			predict.data[i,j] <- x[[j]]$levels[subs[j]]
		 }
		 if (n.excluded > 0) {
				predict.data[i, (n.focal + 1):n.vars] <- excluded
		}
	  }
	  colnames(predict.data) <- c(sapply(x, function(x) x$name),
											sapply(x.excluded, function(x) x$name))
	  predict.data <-  matrix.to.df(predict.data, colclasses=colclasses)
  }
  factor.type <- c(sapply(x, function(x)x$is.factor, simplify = FALSE)
	, sapply(x.excluded, function(x)x$is.factor, simplify = FALSE)
  )

  list(predict.data=predict.data, 
       factor.levels=factor.levels, factor.weights=factor.weights, 
       factor.cols=factor.cols, focal.predictors=focal.predictors, n.focal=n.focal,
       excluded.predictors=excluded.predictors, n.excluded=n.excluded,
       x=x, X.mod=X.mod, cnames=cnames, X=X, x.var=x.var, factor.type=factor.type, poly_check=poly_check)  
}


get_model_matrix <- function(mod, mod.matrix, X.mod, factor.cols, cnames
	, focal.predictors, excluded.predictors, typical, apply.typical.to.factors
	, factor.type, x.var, factor.weights, vnames, input_vars, poly_check){
  attr(mod.matrix, "assign") <- attr(X.mod, "assign")
  if (length(excluded.predictors) > 0){
    strangers <- get_strangers(mod, focal.predictors, excluded.predictors)
    stranger.cols <- apply(outer(strangers, attr(mod.matrix,'assign'), '=='), 2, any)
  }  else stranger.cols <- rep(FALSE, ncol(mod.matrix))
  if (check_intercept(mod)) stranger.cols[1] <- TRUE
  if (any(stranger.cols)) {
    facs <- factor.cols & stranger.cols
    covs <- (!factor.cols) & stranger.cols
    if (check_intercept(mod)) covs[1] <- FALSE
    if (any(facs)){
#       mod.matrix[,facs] <-  matrix(apply(as.matrix(X.mod[,facs]), 2,
#                                          if (apply.typical.to.factors) typical else mean),
#                                    nrow=nrow(mod.matrix), ncol=sum(facs), byrow=TRUE)
    	## 2022 Jul 30 (Sat): Proper way to average over columns?
		mod.matrix[,facs] <- matrix(colMeans(X.mod[, facs, drop=FALSE])
			, nrow=nrow(mod.matrix)
			, ncol=sum(facs)
			, byrow=TRUE
		)
	 }
    for (name in cnames){
      components <- unlist(strsplit(name, ':'))
      components <- components[components %in% cnames]
		if (length(components)==1) components <- unique(c(components, name))
      if (length(components) > 1) {
			if (factor.type[[x.var]]) {
				mod.matrix[,name] <- apply(mod.matrix[,components], 1, prod)
			} else if (all(!factor.cols[components]) & factor.type[[x.var]]){
				mod.matrix[,name] <- apply(mod.matrix[,components], 1, prod)
			} else if (grepl(x.var, name)) {
				mod.matrix[,name] <- apply(mod.matrix[,components], 1, prod)
			} 
      }
    }
  }
	mm_temp <- mod.matrix	
	col_mean <- apply(mod.matrix, 2, typical)
	if (!input_vars & !any(unlist(poly_check))) {
		##	Start: 2023 Feb 09 (Thu): Focal interactions
		center_mean <- colMeans(X.mod)
		mod.matrix <- sweep(mod.matrix, 2, col_mean, FUN="/")
		mod.matrix <- sweep(mod.matrix, 2, center_mean, FUN="*")
		col_mean <- apply(mod.matrix, 2, typical)
		## End: 2023 Feb 09 (Thu): Focal interactions
	}
	list(mm=mod.matrix, col_mean=col_mean, mm_temp=mm_temp)
}


