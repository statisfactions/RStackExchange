\name{searchUsers}
\alias{searchUsers}
\alias{getUsers}
\title{
  Functions to manipulate StackExchange users
}
\description{
  These functions allow you to download and manipulate users from a
  StackExchange site
}
\usage{
searchUsers(num = NULL, filter = NULL, fromDate = NULL, toDate = NULL,
            min = NULL, max = NULL, sort = NULL, order = NULL,
            site = "stackoverflow")
getUsers(ids, num=NULL, site='stackoverflow')
}
\arguments{
  \item{num}{An upper bound on the number of results to return}
  \item{ids}{An optional vector of numeric IDs representing a specific
    subset of users to retrieve}
  \item{filter}{String that must appear in returned users' names}
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
  A list of \code{\link{seUser}} objects
}
\author{Jeff Gentry}
\seealso{\code{\link{seUser}}}
\examples{
   zz <- searchUsers(filter='geoff', num=25)
}
\keyword{interface}
