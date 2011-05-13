\name{seUser-class}
\Rdversion{1.1}
\docType{class}
\alias{seUser-class}
\alias{seUser}
\alias{seUserFactory}
\alias{show,seUser-method}
\title{Class \code{"seUser"}: A class to represent a StackExchange user}
\description{
  This class models the user objects in the StackExchange family of websites
}
\section{Fields}{
  \describe{
    \item{\code{userID}:}{Internal ID of this user}
    \item{\code{userType}:}{Type of user, e.g. registered}
    \item{\code{creationDate}:}{POSIXct date of when this user was created}
    \item{\code{displayName}:}{Short handle for this user}
    \item{\code{reputation}:}{Total reputation score for this user}
    \item{\code{lastAccessDate}:}{POSIXct date for last access of this
      user account}
    \item{\code{aboutMe}:}{Personal description of this user}
    \item{\code{questionCount}:}{Number of questions this user has asked}
    \item{\code{answerCount}:}{Number of answers this user has given}
    \item{\code{viewCount}:}{Number of times this user profile has been viewed}
    \item{\code{upVoteCount}:}{Number of up votes this user has cast}
    \item{\code{downVoteCount}:}{Number of down votes this user has cast}
    \item{\code{acceptRate}:}{Rate at which this user accepts answers}
    \item{\code{goldBadges}:}{Number of gold ranked badges earned}
    \item{\code{silverBadges}:}{Number of silver ranked badges earned}
    \item{\code{bronzeBadges}:}{Number of bronze ranked badges earned}
    \item{\code{site}:}{StackExchange site this answer belongs to}
  }
}
\section{Methods}{
  \describe{
    \item{\code{topAnswersByTags(tags)}:}{Retrieves a list of
      \code{seAnswer} objects representing the top answers this user has
      given for a given set of tags}
    \item{\code{topQuestionsByTags(tags)}:}{Retrieves a list of
      \code{seQuestion} objects representing the top questions this user
      has posted for a given set of tags}
    \item{\code{topTagsByAnswer}:}{Gets the top 30 tags (by score) this
      user has posted answers in}
    \item{\code{topTagsByQuestion}:}{Gets the top 30 tags (by score)
      this user has asked a question in}
  }
}
\details{
  The \code{seUser} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{seUserFactory}.  Accessor set &
  get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}}).  As
  an example, the \code{displayName} field could be accessed with
  \code{object$getDisplayName()} and \code{object$setDisplayName()}.
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{getUsers}}, \code{\link{searchUsers}}, \code{\link{setRefClass}}
}
\examples{
   zz <- searchUsers(num=25, filter='foo')
   zz[[2]]$topTagsByAnswer()
}
\keyword{classes}
