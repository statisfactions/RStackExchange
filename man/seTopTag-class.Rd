\name{seTopTag-class}
\Rdversion{1.1}
\docType{class}
\alias{seTopTag-class}
\alias{seTopTag}
\alias{seTopTagFactory}
\title{Class \code{"seTopTag"}: A class to represent a StackExchange top tag}
\description{
Provides a model representing a top tag from a StackExchange site
}
\section{Fields}{
  \describe{
    \item{\code{name}:}{Name of the tag}
    \item{\code{questionScore}:}{Cumulative score for this tag for the user}
    \item{\code{questionCount}:}{Number of questions this user has posed with this tag}
    \item{\code{answerScore}:}{Cumulative score for this tag for this user}
    \item{\code{answerCount}:}{Number of answers this user has posted with this tag}
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
  The \code{seTopTag} class is a subclass of the \code{seTag} class, and represents a frequently
  used tag for a specific user.

  The \code{seTopTag} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{seTopTagFactory}.  Accessor set &
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
  \code{seTag}, \code{\link{getTags}}, \code{\link{getTagSynonyms}}, \code{setRefClass}
}
\examples{
    zz <- searchUsers(num=2, filter='geoff')
    zz[[1]]$topTagsByQuestions()
}
\keyword{classes}
