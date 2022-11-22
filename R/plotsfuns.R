#' Outcome plots
#' 
#' Plot \code{\link[varpred]{varpred}} object.
#'
#' @param x \code{\link[varpred]{varpred}} object.
#' @param ... for future implementations.
#' @param xlabs x-axis label. If \code{NULL}, default, \code{x.var} is used.
#' @param ylabs y-axis label. If \code{NULL}, default, the response label is used.
#' @param xtrans_fun function to transform x values to the original or other scales. Useful when x was transformed prior to model fitting. See examples.
#' @param pos spacing between labels of categorical variable on the plot. 
#' @param ci logical. If \code{TRUE} (default), the confidence intervals (indicating prediction or effect) are plotted, otherwise, only the central estimate is plotted.
#' @param facet_scales should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param facet_ncol number of facet columns.
#'
#' @return a \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso
#'\code{\link[varpred]{varpred}}
#'
#' @examples
#' set.seed(4567)
#' N <- 100
#' x <- rnorm(N, 3, 5)
#' y <- 5 + 0.2*x + rnorm(N)
#' df <- data.frame(y = y, x = x)
#' m1 <- lm(y ~ x, df)
#' pred1 <- varpred(m1, "x", modelname="original")
#' plot(pred1)
#'
#' ## We can transform the predictor, fit the model and then
#' ## back-transform the predictions in the plot
#' backtfun <- function(x, m, s) {
#'		x <- m + x*s
#'		return(x)
#'	}
#' x_scaled <- scale(df$x)
#' m <- attr(x_scaled, "scaled:center")
#' s <- attr(x_scaled, "scaled:scale")
#' df$x <- as.vector(x_scaled)
#' m2 <- lm(y ~ x, df)
#' pred2 <- varpred(m2, "x", modelname="scaled")
#' # Compare the predictions
#' combinevarpred(list(pred1, pred2), plotit=TRUE)
#'
#' # Display focal predictor on the original scale
#' plot(pred2, xtrans_fun=function(x)backtfun(x, m=m, s=s))
#'
#' @import ggplot2
#' @export

plot.varpred <- function(x, ..., xlabs = NULL, ylabs = NULL, xtrans_fun=NULL, pos = 0.5, ci = TRUE, facet_scales = "fixed", facet_ncol = NULL){

	lwr <- upr <- model <- NULL
	df <- x$preds
	if (is.null(df)) df <- x
	focal <- attr(df, "focal")
	if (!is.null(focal))	n.focal <- length(focal) else {n.focal <- 1L; focal <- "xvar"}
	x.var <- attr(df, "x.var")
	if (is.null(x.var)) x.var <- focal[[1]]
	
	if (!is.null(xtrans_fun) & is.function(xtrans_fun)) {
		df[[x.var]] <- xtrans_fun(df[[x.var]])
	}
	
	add_col <- df$.varpred
	if (n.focal>1L) {
		non.focal <- focal[!focal %in% x.var]
		df[, non.focal] <- sapply(non.focal, function(x){
			xx <- df[[x]]
			xx <- if (class(xx) %in% c("numeric", "integer")) round(xx,1) else xx
			ll <- paste0(x, ": ", xx)
			return(ll)
		})	
		if (is.null(add_col)) ff <- ".~" else ff <- ".varpred~"
		gform <- as.formula(paste0(ff, paste0(non.focal, collapse="+")))
	} else {
		if (!is.null(add_col)) {
			ff <- "~.varpred"
			gform <- as.formula(ff)
		}
	}
	if (is.null(xlabs)) xlabs <- x.var
	if (is.null(ylabs)) ylabs <- attr(df, "response")
	pos <- position_dodge(pos)
	nn <- unique(df$model)
	if (!(length(nn)>1)){
		p1 <- (ggplot(df, aes_string(x = x.var, y = "fit"), colour="black", alpha = 0.2)
			+ guides(fill = "none")
			+ theme(legend.position = "none")
		)
	} else {
		p1 <- (ggplot(df, aes_string(x = x.var, y = "fit", colour ="model"), alpha = 0.2)
			+ guides(fill = "none")
			+ theme(legend.position = "right")
		)
	}
	p1 <- p1 + labs(x = xlabs, y = ylabs)
	if (class(df[[x.var]]) %in% c("numeric", "integer")) {
		if (length(nn)>1) {
			p2 <- p1 + geom_line(aes(linetype=model))
		} else {
			p2 <- p1 + geom_line()
		}
		if (ci) {
#			if (length(nn)>1) {
#				p2 <- (p2
#					+ geom_line(aes(y = lwr))
#					+ geom_line(aes(y = upr))
#					+ scale_colour_viridis_d(option = "plasma")
#				)
#			} else {
				p2 <- (p2
					+ geom_line(aes(y = lwr), lty=2)
					+ geom_line(aes(y = upr), lty=2)
					+ scale_colour_viridis_d(option = "plasma")
				)
#			}
		}
	} else {
		if (ci) {
			if (length(nn) > 1) {
				p2 <- (p1 
	#				+ geom_point(aes(colour=model), position = pos, size = 0.6)
					+ geom_pointrange(aes(ymin = lwr, ymax = upr, colour=model), size=0.2, position = pos)
				)
			} else {
				p2 <- (p1 
	#				+ geom_point(position = pos, size = 0.6, colour="black")
					+ geom_pointrange(aes(ymin = lwr, ymax = upr), colour = "black", size=0.2, position = pos)
				)
			}
		} else {
			p2 <- p1 + geom_point()
		}
	}

	if (n.focal>1L || !is.null(add_col)) {
		p2 <- p2 + facet_wrap(gform, scales=facet_scales, ncol=facet_ncol)#, labeller = label_parsed)
	}
	return(p2)
}

#' Customized theme for varpred plots
#'
#' Sets a theme for varpred and other ggplot objects
#'
#' @param type a character string. If \code{minimal} simple theme is applied, otherwise \code{complete}.
#'
#' @examples
#' library(ggplot2)
#' varpredtheme()
#' set.seed(4567)
#' x <- rnorm(100, 3, 5)
#' y <- 0.4 + 0.7*x + rnorm(100)
#' df <- data.frame(y = y, x = x)
#' m1 <- lm(y ~ x, df)
#'
#' pred1 <- varpred(m1, "x")
#' plot(pred1)
#'
#' @import ggplot2
#' @export

varpredtheme <- function(type=c("minimal", "complete")){
	type=match.arg(type)
	if (type=="complete") {
		theme_set(theme_bw() +
			theme(panel.spacing = grid::unit(0,"lines")
				, plot.title = element_text(hjust = 0.5)
				, legend.position = "bottom"
				, axis.ticks.y = element_blank()
				, axis.text.x = element_text(size = 12)
				, axis.text.y = element_text(size = 12)
				, axis.title.x = element_text(size = 12)
				, axis.title.y = element_text(size = 12)
				, legend.title = element_text(size = 12, hjust = 0.5)
				, legend.text = element_text(size = 12)
				, panel.grid.major = element_blank()
				, legend.key.size = unit(0.8, "cm")
				, legend.key = element_rect(fill = "white")
				, panel.spacing.y = unit(0.3, "lines")
				, panel.spacing.x = unit(1, "lines")
				, strip.background = element_blank()
				, panel.border = element_rect(colour = "grey"
					, fill = NA
					, size = 0.8
				)
				, strip.text.x = element_text(size = 11
					, colour = "black"
					, face = "bold"
				)
			)
		)
	} else {
		theme_set(
			theme_bw(base_size=12)
			+ theme(
				strip.background = element_blank()
				, plot.title = element_text(hjust = 0.5)
				, legend.position = "bottom"
				, panel.border = element_rect(colour = "grey"
					, fill = NA
					, size = 0.8
				)
				, strip.text.x = element_text(size = 11
					, colour = "black"
					, face = "bold"
				)
			)
		)
	}
}

