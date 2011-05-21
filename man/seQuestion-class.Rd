\name{seQuestion-class}
\Rdversion{1.1}
\docType{class}
\alias{seQuestion-class}
\alias{seQuestion}
\alias{seQuestionFactory}
\alias{show,seQuestion-method}
\title{Class \code{"seQuestion"}: A class to represent a StackExchange question}
\description{
Provides a model for a question from a StackExchange website
}
\section{Fields}{
  \describe{
    \item{\code{questionID}:}{Internal ID of the question}
    \item{\code{answerCount}:}{Number of answers for this question}
    \item{\code{favoriteCount}:}{How many times the question has been favorited}
    \item{\code{owner}:}{The owner of the question, a \code{seUser}
      object}
    \item{\code{creationDate}:}{POSIXct date of when this question was
      asked}
    \item{\code{lastActivityDate}:}{POSIXct date of the last activity on
    this question}
    \item{\code{upVoteCount}:}{Number of up votes given}
    \item{\code{downVoteCount}:}{Number of down votes given}
    \item{\code{viewCount}:}{Number of times the question has been viewed}
    \item{\code{score}:}{Total score of the question}
    \item{\code{communityOwned}:}{Logical, is this a community owned question}
    \item{\code{title}:}{Title of the question}
    \item{\code{comments}:}{List of \code{seComment} objects, providing
      all the comments on this question}
    \item{\code{tags}:}{List of \code{seTag} objects, providing all the
      tags associated with this question}
    \item{\code{answers}:}{List of \code{seAnswer} objects, providing
      all the answers for this question}
    \item{\code{site}:}{StackExchange site this answer belongs to}
  }
}
\details{
  The \code{seQuestion} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{seQuestionFactory}.  Accessor set &
  get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}}).  As
  an example, the \code{score} field could be accessed with
  \code{object$getScore()} and \code{object$setScore()}.
}
\author{
Jeff Gentry
}
\seealso{
  \code{\link{getQuestions}}, \code{\link{getUnansweredQuestions}},
  \code{\link{getNoAnswerQuestions}}, \code{\link{getQuestionLinks}},
  \code{\link{getQuestionRelated}}, \code{\link{setRefClass}}
}
\examples{
   zz <- getQuestions(num=5)
   sapply(zz, function(x) x$getScore())
}
\keyword{classes}
