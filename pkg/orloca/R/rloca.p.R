#'
#' Random instances generator of loca.p class object
#'
#' \code{rloca.p} function returns a random instance of \code{loca.p} class object at a given rectangular region.
#'
#'
#' @keywords datagen
#' @examples
#' # A random loca.p object at unit square with 5 demand points
#' rloca.p(5)
#' # At another region
#' rloca.p(10, xmin=-2, xmax=2, ymin=-2, ymax=2)
#' # Five groups
#' rloca.p(48, groups=5)
#' # Three unequal size groups
#' rloca.p(1, groups=c(10, 7, 2))
#' @seealso See also \code{\link{orloca-package}} and \code{loca.p}.
#' @details
#' \code{n} must be at least 1.
#'
#' \code{xmin} must be less or equal than \code{xmax}.
#'
#' \code{ymin} must be less or equal than \code{ymax}.
#' 
#' If a non zero value is given for \code{groups} parameter, then a reference point for each group are generated. At second stage, the offset part for each demand point are generated, and added to the reference point generated at the first stage.
#'
#' Note that \code{groups = 1} is not equivalent to the default value \code{groups = 0}, because in the first case a reference point are generated at the first stage.
#' @param n The number of demand points.
#' @param xmin Minimum value for the x coordinates of the demand points.
#' @param xmax Maximum value for the x coordinates of the demand points.
#' @param ymin Minimum value for the y coordinates of the demand points.
#' @param ymax Maximum value for the y coordinates of the demand points.
#' @param wmin Minimum value for weights
#' @param wmax Maximum value for weights
#' @param label The label for the new loca.p object.
#' @param groups The number of (almost) equal size groups to generate, or a list size of the groups to generate. In the second case \code{n} will be ignored.
#' @param xgmin Minimum value for the x coordinate of demand points with respect to the group reference point.
#' @param xgmax Maximum value for the x coordinate of demand points with respect to the group reference point.
#' @param ygmin Minimum value for the y coordinate of demand points with respect to the group reference point.
#' @param ygmax Maximum value for the y coordinate of demand points with respect to the group reference point.
#' @return If the arguments are valid values, it returns a new object of \code{loca.p} class, else it returns an error.
#' @export
rloca.p <- function (n, xmin=0, xmax=1, ymin=0, ymax=1, wmin=1, wmax=1, label = '', groups=0, xgmin=xmin, xgmax=xmax, ygmin=ymin, ygmax=ymax)
{
   if (identical(groups, 0))
    {
        x <- runif(n, xmin, xmax)
        y <- runif(n, ymin, ymax)
        w <- runif(n, wmin, wmax) 
    }
    else if (isTRUE(length(groups) ==  1))
    {
        x = numeric(0)
        y = numeric(0)
        w = numeric(0)
        for(i in 1:groups)
        {
            gx <- runif(1, xgmin, xgmax)
            gy <- runif(1, ygmin, ygmax)
            ## Distribute the groups as uniform as possible
            gn <- floor(n/(groups-i+1))
            n <- n - gn
            x <- c(x, gx + runif(gn, xmin, xmax))
            y <- c(y, gy + runif(gn, ymin, ymax))
            w <- c(w, runif(gn, wmin, wmax))
        }
 
    }
    else
    {
        x = numeric(0)
        y = numeric(0)
        w = numeric(0)
        i <- 1
        for(gn in groups)
        {
            gx <- runif(1, xgmin, xgmax)
            gy <- runif(1, ygmin, ygmax)
            x <- c(x, gx + runif(gn, xmin, xmax))
            y <- c(y, gy + runif(gn, ymin, ymax))
            w <- c(w, runif(gn, wmin, wmax))
            i <- i + 1
        }
    }
        new("loca.p", x = x, y = y, w = w, label = label)
}
