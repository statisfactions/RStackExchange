\name{getAnswers}
\alias{getAnswers}
\title{
Functions to manipulate StackExchange answers
}
\description{
  These functions allow you to download and manipulate answers from
  a StackExchange site
}
\usage{
getAnswers(num=NULL, ids=NULL, fromDate=NULL, toDate=NULL,
           min=NULL, max=NULL, sort=NULL, order=NULL,
           site="stackoverflow")
}
\arguments{
  \item{num}{An upper bound on the number of results to return}
  \item{ids}{An optional vector of numeric IDs representing a specific
    subset of answers to search}
  \item{fromDate}{When present, an early bound on results to return,
    either as a \code{POSIXct} date or an Unix timestamp}
  \item{toDate}{When present, an upper bound on results to return,
    either as a \code{POSIXct} date or an Unix timestamp}
  \item{min}{When present, minimum value for the current sort}
  \item{max}{When present, maximum value for the current sort}
  \item{sort}{When present, how results are to be sorted, one of
    \code{activity} (default), \code{creation} or \code{votes}}
  \item{order}{When present, how results should be ordered, one of
    \code{desc} (default) or \code{asc}}
  \item{site}{Which StackExchange site to poll}
}
\value{
  The \code{getAnswers} function returns a list of objects of class
  \code{\link{seAnswer}}
}
\author{Jeff Gentry}
\seealso{\code{\link{seAnswer}}}
\examples{
     zz <- getAnswers(num=3, fromDate=as.POSIXct('2009-09-25'),
                      toDate=as.POSIXct('2010-05-30'))
}
\keyword{interface}