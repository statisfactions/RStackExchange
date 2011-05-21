\name{seAnswer-class}
\Rdversion{1.1}
\docType{class}
\alias{seAnswer-class}
\alias{seAnswer}
\alias{seAnswerFactory}
\alias{show,seAnswer-method}
\title{Class "seAnswer":  A class to represent a StackExchange answer}
\description{ Provides a model representing answers from a StackExchange
  site}
\section{Fields}{
  \describe{
    \item{\code{answerID}:}{Internal ID of the answer}
    \item{\code{accepted}:}{Logical, was the answer accepted}
    \item{\code{questionID}:}{ID of the question this answer was given
      to}
    \item{\code{creationDate}:}{POSIXct date of when this question was
      asked}
    \item{\code{lastActivityDate}:}{POSIXct date of the last activity
      associated with this answer happened}
    \item{\code{upVoteCount}:}{Number of up votes given}
    \item{\code{downVoteCount}:}{Number of down votes given}
    \item{\code{score}:}{Total score}
    \item{\code{owner}:}{The owner of the question, a \code{seUser}
      object}
    \item{\code{communityOwned}:}{Logical, is this a community wiki}
    \item{\code{title}:}{Title of the question this is answering}
    \item{\code{comments}:}{List of \code{seComment} objects that have
      been attached to this answer}
    \item{\code{body}:}{Text of the answer}
    \item{\code{site}:}{StackExchange site this answer belongs to}
  }
}
\section{Methods}{
  \describe{
    \item{\code{getQuestion}:}{Retrieves the \code{seQuestion} object
      that this is answering}
  }
}
\details{
  The \code{seAnswer} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{seAnswerFactory}.  Accessor set &
  get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}}).  As
  an example, the \code{score} field could be accessed with
  \code{object$getScore()} and \code{object$setScore()}.
}
\author{Jeff Gentry}
\seealso{\code{\link{getAnswers}}, \code{\link{setRefClass}}}
\examples{
   zz <- getAnswers(num=2)
   bodies <- sapply(zz, function(x) x$getBody())
}