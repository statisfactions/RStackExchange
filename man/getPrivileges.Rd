\name{getPrivileges}
\alias{getPrivileges}
\title{Functions to manipulate StackExchange privileges}
\description{
  These functions allow you to download and manipulate privileges from a
  StackExchange site
}
\usage{
getPrivileges(num = NULL, site = "stackoverflow.com")
}
\arguments{
  \item{num}{An upper bound on the number of results to return}
  \item{site}{Which StackExchange site to poll}
}
\value{
  A list of \code{\link{sePrivilege}} objects
}
\author{Jeff Gentry}
\seealso{
  \code{\link{sePrivilege}}
}
\examples{
  zz <- getPrivileges(num=5)
}
\keyword{interface}
