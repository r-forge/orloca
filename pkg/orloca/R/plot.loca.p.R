#' plot of loca.p class objects
#'
#' This method provides a graphical representations of an object of class \code{loca.p}.
#'
#' The function plots demand points by evaluating limits automatically.
#'
#' @name plot
#' @docType methods
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' # The plot of loca object
#' plot(loca)
#' 
#' @seealso See also \code{\link{orloca-package}}, \code{\link{loca.p}} and \code{\link{plot}}.
#' @keywords classes hplot
#' @param x The \code{loca.p} object to plot.
#' @param xlab The label for x axis.
#' @param ylab The label for y axis.
#' @param main The main title for the plot.
#' @param img A raster image to plot on background.
#' @param xlim Limit over the x axes of the plot.
#' @param ylim Limit over the y axes of the plot.
#' @param xleft The left position of the image.
#' @param ybottom The bottom position of the image.
#' @param xright The right position of the image.
#' @param ytop The top position of the image.
#' @param \ldots Other graphical options.
#' @return The function plots the required graphics.
#' @export
plot.loca.p <- function(x, xlab="", ylab="", main=paste(gettext("Plot of loca.p"), ifelse(x@label == "", "", paste0(": \"", x@label, "\""))), img=NULL, xlim=c(min(xleft, min(x@x)), max(xright, max(x@x))), ylim=c(min(ybottom, min(x@y)), max(ytop, max(x@y))), xleft=min(x@x), ybottom=min(x@y), xright=max(x@x), ytop=max(x@y), ...)
{
    ## Compute graphical limits to avoid degenerated cases and wrong argument values
    .xmin = min(xlim[1], xlim[2])
    .xmax = max(xlim[1], xlim[2])
    deltax = max(.xmax - .xmin, .1)
    centerx = (.xmin + .xmax)/2
    .xmin = centerx - deltax/2
    .xmax = centerx + deltax/2
    .ymin = min(ylim[1], ylim[2])
    .ymax = max(ylim[1], ylim[2])
    deltay = max(.ymax - .ymin, .1)
    centery = (.ymin + .ymax)/2
    .ymin = centery - deltay/2
    .ymax = centery + deltay/2
    ## Plot it
    plot(x@x, x@y, xlab=xlab, ylab=ylab, main=main, xlim=c(.xmin, .xmax), ylim=c(.ymin, .ymax), ...)
    if (!is.null(img)) {
        if (is.raster(.img <- img) || is.raster(.img <- as.raster(img))) {
            rasterImage(.img, xleft, ybottom, xright, ytop)
        }
        else warning(gettext("The given img object is not a raster image and cannot be coerce to it."))
    }
    points(x@x, x@y)
}
