\name{seComment-class}
\Rdversion{1.1}
\docType{class}
\alias{seComment-class}
\alias{seComment}
\alias{seCommentFactory}
\alias{show,seComment-method}
\title{Class \code{"seComment"}: A class to represent a StackExchange comment}
\description{
  Provides a model to represent a StackExchange comment
}
\section{Fields}{
  \describe{
    \item{\code{commentID}:}{Internal ID of the comment}
    \item{\code{creationDate}:}{POSIXct date of when the comment was created}
    \item{\code{postID}:}{ID of object this is commenting on}
    \item{\code{postType}:}{What type of post this is a comment on, e.g.
    question or answer}
    \item{\code{score}:}{Total score}
    \item{\code{body}:}{Text of the comment}
    \item{\code{owner}:}{The owner of the comment, a \code{seUser} object}
    \item{\code{site}:}{StackExchange site this comment belongs to}
  }
}
\details{
  The \code{seComment} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{seCommentFactory}.  Accessor set &
  get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}}).  As
  an example, the \code{body} field could be accessed with
  \code{object$getBody()} and \code{object$setBody()}.
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{getComments}}, \code{\link{getRefClass}}
}
\examples{
   zz <- getComments(num=5)
   sapply(zz, function(x) x$body)
}
\keyword{classes}
