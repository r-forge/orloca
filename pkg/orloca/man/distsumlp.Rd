% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distsumlp.R
\name{distsumlp}
\alias{distsumlp}
\alias{distsumlp,loca.p-method}
\alias{distsumlpgra}
\alias{distsumlpgra,loca.p-method}
\title{distsumlp and distsumlpgra at orloca package}
\usage{
distsumlp(o, x = 0, y = 0, p = 2)
}
\arguments{
\item{o}{An object of \code{loca.p} class.}

\item{x}{The x coordinate of the point to be evaluated.}

\item{y}{The y coordinate of the point to be evaluated.}

\item{p}{The \eqn{l_p} norm to use.}
}
\value{
\code{distsumlp} returns the objective function of the min-sum location problem with \eqn{l_p} norm, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the distances between \eqn{a_i} and the point \eqn{(x,y)} using \eqn{l_p} norm.

\code{distsumlpgra} returns the gradient vector of the function \code{distsumlp}. If \eqn{(x,y)} is a demand point \code{partial=T} means ignore such point to compute the gradient. This option is mainly for internal use.
}
\description{
distsum and distsumgra functions with \eqn{l_p} norm. Mainly for internal use.
}
\details{
If \eqn{p<1} then \eqn{l_p} is not a norm, so only \eqn{p>=1} are valid values.

Since \eqn{l_2} norm is the Euclidean norm, when \eqn{p=2} \code{distsumlp} are equal to \code{distsum}, and \code{distsumlpgra} are equal to \code{distsumgra}.
But the computations involved are greater for the firsts form.
}
\seealso{
See also \code{\link{distsum}}, \code{\link{orloca-package}} and \code{\link{distsumlpmin}}.
}
\keyword{classes}
\keyword{internal}
\keyword{optimize}
