\name{getComments}
\alias{getComments}
\title{
  Functions to manipulate StackExchange comments
}
\description{
  These functions can be used to retrieve comments from a StackExchange site.
}
\usage{
getComments(num = NULL, ids = NULL, fromDate = NULL, toDate = NULL,
            min = NULL, max = NULL, sort = NULL, order = NULL,
            idsArePosts = FALSE, site = "stackoverflow") 
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
  \item{idsArePosts}{If \code{TRUE}, the \code{ids} argument are assumed
    to be ids of questions and answers, and will retrieve associated
    comments}
}
\value{
  Returns a list of \code{\link{seComment}} objects
}
\author{Jeff Gentry}
\seealso{
  \code{\link{seComment}}
}
\examples{
     zz <- getComments(fromDate=as.POSIXct('2009-09-25'),
                       toDate=as.POSIXct('2010-05-30'))
}
\keyword{interface}