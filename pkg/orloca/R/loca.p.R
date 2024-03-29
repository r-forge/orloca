#' loca.p class for Operations Research LOCational Analysis
#'
#' An object of class \code{loca.p} represents a weighted location problem with a finite demand points set.
#' The \code{\link{orloca-package}} is mainly devoted to deals with location problems.
#'
#' @aliases loca.p print summary
#' @docType class
#' @name loca.p
#' @details
#' The main generator of the loca.p class is \code{loca.p(x, y, w = numeric(0), label = "")}.
#' An alternative form is \code{new("loca.p", x, y, w = numeric(0), label = "")}.
#'
#' The lengths of \code{x} and \code{y} vector must be equals.
#' The length of \code{w} must be equal to the previous ones or must be 0, or should be omitted.
#' NA's values are not allowed at any of the arguments.
#' 
#' \code{summary(x)} returns a summary of the \code{x} \code{loca.p} object and \code{print(x)} prints the \code{x} \code{loca.p} object in table format.
#'
#' @keywords classes optimize
#'
#' @param x is a vector of the x coordinates of the demand points.
#' @param y is a vector of the y coordinates of the demand points.
#' @param w is a vector of weights of the demand points. If w is omitted then all weights are considered as 1.
#' @param label If given, it is the label of the new object.
#' @return If the arguments have valid values, it returns a new object of class \code{loca.p}, else it returns an error.
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' # or
#' loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#'
#' # An example with weights and name
#' locb <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1),
#' w = c(1, 2, 1, 2), label = "Weighted case")
#'
#' @seealso See also \code{\link{orloca-package}}.

## To ensure that orloca is included in pot file
gettext("orloca", domain="R-orloca")

setClass("loca.p",
         representation(x="numeric", y="numeric", w="numeric", label="character")
         )

##
## loca.p Validity method
##
setValidity("loca.p",
            function(object)
            {
                if(length(object@x)==length(object@y))
                {
                    if (length(object@x)==length(object@w) || length(object@w)==0)
                    {
                        if (!any(is.na(object@x)) && !any(is.na(object@y)) && !any(is.na(object@w))) TRUE
                        else paste(gettext("NA's values are not allowed"), sep="")
                    }
                    else paste(gettext("The length of w ("), length(object@w), gettext(") should be the same as the length of x, and y ("), length(object@x) ,gettext(") or 0"))
                }
                else paste(gettext("The length of x and y are different"), length(object@x), ", ", length(object@y))
            }
            )

##
## loca.p initialize method
##
setMethod("initialize", "loca.p",
          function(.Object, x, y, w = numeric(0), label="")
          {
              .Object@x <- x
              .Object@y <- y
              if (length(w) == 0) .Object@w <- rep(1,length(x))
              else .Object@w <- w
              .Object@label <- label
              validObject(.Object)
              .Object
          }
          )
#' @export
loca.p <- function(x, y, w = numeric(0), label="") new("loca.p", x, y, w, label)


##
## loca.p summary method
#' @export
summary.loca.p <- function(object, ...) {
    s <- list(label = object@label, n = length(object@x), xmin = min(object@x), xwmean = weighted.mean(object@x,object@w), xmax = max(object@x), ymin = min(object@y), ywmean = weighted.mean(object@y,object@w), ymax = max(object@y))
    class(s) <- 'summary.loca.p'
    s
}
## setMethod("summary", "loca.p", summary.loca.p)

##
## loca.p print method
#' @export
print.loca.p <- function(x, ...) {
    cat(gettext('A loca.p object with label:'), x@label, '\n')
    print(data.frame(x = x@x, y = x@y, w = x@w), ...)
    invisible(x)
}
## setMethod("print", "loca.p", print.loca.p)

#' @export
print.summary.loca.p <- function(x, ...) {
    print(data.frame(unclass(x)), row.names = FALSE)
    invisible(x)
}
