#' Print varpred objects
#'
#' @param x varpred object.
#' @param ... additional aurguments passed to print.
#'
#' @method print varpred
#' @export
#' @export print.varpred

print.varpred <- function(x, ...){
	cat("Call:\n")
	print(x$call)
	cat("\n")
	df <- x$preds
	if(is.null(df)) df <- x
	print(df, ...)
}

#' Coerce varpred object to as.data.frame 
#'
#' @param x varpred object.
#' @param ... additional aurguments passed to as.data.frame.
#'
#' @export
#' @method as.data.frame varpred
#' @export as.data.frame.varpred

as.data.frame.varpred <- function(x, ...) {
	df <- x$preds
	if(is.null(df)) df <- x
	return(as.data.frame(df, ...))
}

#' Coerce varpred object to data.frame 
#'
#' @param x varpred object.
#' @param ... additional aurguments passed to data.frame.
#'
#' @export
#' @method data.frame varpred
#' @export data.frame.varpred

data.frame.varpred <- function(x, ...) {
	df <- x$preds
	if(is.null(df)) df <- x
	return(data.frame(df, ...))
}

#'
#' @keywords internal
vareffobj <- function(mod, ...)UseMethod("vareffobj")

#'
#' @keywords internal
get_xlevels <- function(mod)UseMethod("get_xlevels")

#'
#' @keywords internal
get_stats <- function(mod, level, dfspec, ...)UseMethod("get_stats")

#'
#' @keywords internal
get_sigma <- function(mod, ...)UseMethod("get_sigma")

#'
#' @keywords internal
includeRE <- function(mod, ...)UseMethod("includeRE")

#'
#' @keywords internal
check_intercept <- function(mod, ...)UseMethod("check_intercept")

#'
#' @keywords internal
get_contrasts <- function(mod, ...)UseMethod("get_contrasts")


#'
#' @keywords internal
get_model.mm <- function(mod, ...)UseMethod("get_model.mm")

#'
#' @keywords internal
prepmod <- function(mod, ...)UseMethod("prepmod")

#' Get focal and prediction means for a varpred object
#'
#' @export
getmeans <- function(object, what=c("estimate", "focal"), focal=NULL, modelname=NULL)UseMethod("getmeans")
