\name{seTag-class}
\Rdversion{1.1}
\docType{class}
\alias{seTag-class}
\alias{seTag}
\alias{seTagFactory}
\alias{show,seTag-method}
\title{Class \code{"seTag"}: A class to represent a StackExchange tag}
\description{
Provides a model representing a tag from a StackExchange site
}
\section{Fields}{
  \describe{
    \item{\code{name}:}{Name of the tag}
    \item{\code{count}:}{Number of times this tag has been used}
    \item{\code{fulfillsRequired}:}{Logical, indicates if tag is
      required to be on a post}
    \item{\code{site}:}{Object of class \code{character} ~~ }
  }
}

\section{Methods}{
  \describe{
    \item{\code{topAskers(period)}:}{Retrieves the \code{seUser} objects for the
      people who have asked the most questions with this tag for a given
      period, either \code{all-time} or \code{month}}
    \item{\code{topAnswerers(period)}:}{Retrieves the \code{seUser}
      objects for the people who have answered the most questions with this
      tag for a given  period, either \code{all-time} or \code{month}}
  }
}
\details{
  The \code{seTag} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{seTagFactory}.  Accessor set &
  get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}}).  As
  an example, the \code{count} field could be accessed with
  \code{object$getCount()} and \code{object$setCount()}.

  As noted in the \code{Methods} section, both methods take a
  \code{period} argument which must be one of either \code{all-time} or
  \code{month}.
}
\author{
Jeff Gentry
}
\seealso{
  \code{\link{getTags}}, \code{\link{getTagSynonyms}}, \code{setRefClass}
}
\examples{
   tags <- getTags(num=25)
   tags[[23]]$topAskers('all-time')
}
\keyword{classes}
