\name{searchStackSite}
\alias{searchStackSite}
\title{Functions to search a StackExchange site}
\description{
  These functions will allow you to search a StackExchange site for
  data matching a particular query
}
\usage{
searchStackSite(num = NULL, intitle = NULL, notTagged = NULL,
                tagged = NULL, min = NULL, max = NULL, order = NULL,
                sort = NULL, site = "stackoverflow") 
}
\arguments{
  \item{num}{An upper bound on the number of results to return}
  \item{intitle}{A string that must appear verbatim in the title of the
    question}
  \item{tagged}{An optional vector of tags, of which at least one must
    be on the question}
  \item{notTagged}{An optional vector of gatgs that must not be on a question}
  \item{min}{When present, minimum value for the current sort}
  \item{max}{When present, maximum value for the current sort}
  \item{sort}{When present, how results are to be sorted, one of
    \code{activity} (default), \code{creation} or \code{votes}}
  \item{order}{When present, how results should be ordered, one of
    \code{desc} (default) or \code{asc}}
  \item{site}{Which StackExchange site to poll}
}
\details{
  As per the StackExchange documentation, this functionality is intended
  to be "quite limited".  This function is merely provided because the
  API call exists.
}
\value{
  A list of \code{\link{seQuestion}} objects
}
\references{
http://api.stackoverflow.com/1.1/usage/methods/search
}
\author{Jeff Gentry}
\seealso{
  \code{\link{seQuestion}}
}
\examples{
    zz <- searchStackSite(num=5, tagged='r', sort='votes')
}
\keyword{interface}
