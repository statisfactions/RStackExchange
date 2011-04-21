\name{getTags}
\alias{getTags}
\alias{getTagSynonyms}
\title{Functions to manipulate StackExchange tags}
\description{
  These functions allow you to download and manipulate tags from a
  StackExchange site
}
\usage{
getTags(num = NULL, filter = NULL, fromDate = NULL, toDate = NULL,
        min = NULL, max = NULL, sort = NULL, order = NULL,
        site = "stackoverflow")
getTagSynonyms(num = NULL, tags = NULL, fromDate = NULL, toDate = NULL,
               min = NULL, max = NULL, sort = NULL, order = NULL,
               site = 'stackoverflow')
}

\arguments{
  \item{num}{An upper bound on the number of results to return}
  \item{filter}{Required text in returned tags}
  \item{tags}{An optional vector of tags, see \code{details}}
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
\details{
  The \code{getTags} function will retrieve all tags that match the
  criteria specified.

  The \code{getTagSynonyms} will by default return all tag synonyms that
  match the criteria but one can supply a vector of tag names, in which
  case syonyms are returned for just the tags specified (which also meet
  any other criteria).

  As of this writing, tags don't seem to have a unique identifier and
  are only referenced by name.  
}
\value{
  A list of \code{\link{seTag}} objects
}
\author{Jeff Gentry}
\seealso{
  \code{\link{seTag}}
}
\examples{
    zz <- getTags(filter='java', num=25)
    xx <- getTagSynonyms(num=3, tags=zz, order='desc')
}
\keyword{interface}
