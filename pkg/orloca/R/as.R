#' Conversions between loca.p class and some others classes
#'
#' Methods to convert from and to \code{loca.p} class.
#' @aliases as.data.frame as.matrix as-methods as.loca.p,data.frame-method as.loca.p,matrix-method
#' @docType methods
#' @name as-methods
#' @title as-methods
#' @details
#' NA's  values are not allowed in any of the arguments.
#'
#' The \code{matrix} or \code{data.frame} to convert into \code{loca.p} must have at least two columns.
#' The first column will be consider as the x coordinates, the second as the y coordinates, and the third (if given) as the values of weights \code{w}.
#'
#' @return It returns a new object of the new class.
#' @keywords classes methods
#' @seealso See also \code{\link{loca.p}}
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#'
#' # Conversion to matrix
#' m <- as.matrix(loca)
#'
#' # Show matrix
#' m
#'
#' # Conversion from matrix
#' as.loca.p(m)
#' @param x is the object to convert to the new class object.
#' @param row.names Unused.
#' @param optional Unused.
#' @param ... Other arguments, unused.

setAs('loca.p', 'data.frame',
      function(from, to) {
          df <- data.frame('x' = from@x, 'y' = from@y, 'w' = from@w)
          label <- ifelse(is.null(attr(from, 'label')), '', attr(from, 'label'))
          attr(df, 'label') <- label
          df
      }
      )

setAs('data.frame', 'loca.p',
      function(from, to) {
          label <- ifelse(is.null(attr(from, 'label')), '', attr(from, 'label'))
          if (ncol(from) == 2) l <- loca.p(x=from[,1], y=from[,2], label=label)
          else if (ncol(from) == 3) l <- loca.p(x=from[,1], y=from[,2], w=from[,3], label=label)
          else stop(gettext('The data.frame to convert must have 2 or 3 columns.'))
      }
      )


setAs('loca.p', 'matrix',
      function(from, to) {
          m <- cbind(from@x, from@y, from@w)
          attr(m, 'label') <- from@label
          m
      }
      )

setAs('matrix', 'loca.p',
      function(from, to) {
          label <- ifelse(is.null(attr(from, 'label')), '', attr(from, 'label'))
          if (ncol(from) == 2) loca.p(x=from[,1], y=from[,2], label = label)
          else if (ncol(from) == 3) loca.p(x=from[,1], y=from[,2], w=from[,3], label = label)
          else stop(gettext('The matrix to convert must have 2 or 3 columns.'))
      }
      )

#' @name as.loca.p
#' @title as.loca.p 
#' The following is for S3 compatibility, mainly for documentation check
#' @inherit as-methods
#' @export
setGeneric('as.loca.p', function(x, ...) standardGeneric('as.loca.p'))

#' @name as.loca.p.matrix
#' @title as.loca.p.matrix
#' S3 method to convert from matrix to loca.p
#' @inherit as-methods
#' @export
as.loca.p.matrix <- function(x,...) as(x, 'loca.p')
setMethod('as.loca.p', 'matrix', as.loca.p.matrix)

#' @name as.loca.p.data.frame
#' @title as.loca.p.data.frame
#' S3 method to convert from data.frame to loca.p
#' @inherit as-methods
#' @export
as.loca.p.data.frame <- function(x, ...) as(x, 'loca.p')
setMethod('as.loca.p', 'data.frame', as.loca.p.data.frame)

#' @name as.matrix.loca.p
#' @title as.matrix.loca.p
#' S3 method to convert from loca.p to matrix
#' @inherit as-methods
#' @param rownames.force If True the rownames is setted
#' @export
as.matrix.loca.p <- function(x, rownames.force = NA, ...) as(x, 'matrix')
## setMethod('as.matrix', 'loca.p', as.matrix.loca.p)

#' @name as.data.frame.loca.p
#' @title as.data.frame.loca.p
#' S3 method to convert from loca.p to data.frame
#' @inherit as-methods
#' @export
as.data.frame.loca.p <- function(x, row.names = NULL, optional = FALSE, ...) as(x, 'data.frame')
## setMethod('as.data.frame', 'loca.p', as.data.frame.loca.p)
