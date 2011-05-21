\name{getQuestions}
\alias{getQuestions}
\alias{getUnansweredQuestions}
\alias{getNoAnswerQuestions}
\alias{getQuestionLinks}
\alias{getQuestionRelated}
\title{
Functions to manipulate StackExchange questions
}
\description{
These functions interact with the question functionality of the StackExchange API
}
\usage{
getQuestions(num = NULL, ids = NULL, fromDate = NULL, toDate = NULL, min = NULL, max = NULL,
             sort = NULL, order = NULL, tagged = NULL, site = "stackoverflow")
getUnansweredQuestions(num = NULL, fromDate = NULL, toDate = NULL, min = NULL, max = NULL,
             sort = NULL, order = NULL, tagged = NULL, site = "stackoverflow")
getNoAnswerQuestions(num = NULL, fromDate = NULL, toDate = NULL, min = NULL, max = NULL,
             sort = NULL, order = NULL, tagged = NULL, site = "stackoverflow")
getQuestionLinks(num = NULL, ids = NULL, fromDate = NULL, toDate = NULL, min = NULL, max = NULL,
             sort = NULL, order = NULL, site = "stackoverflow")
getQuestionRelated(num = NULL, ids = NULL, fromDate = NULL, toDate = NULL, min = NULL, max = NULL,
             sort = NULL, order = NULL, site = "stackoverflow")
}
\arguments{
  \item{num}{An upper bound on the number of results to return}
  \item{ids}{A vector of numeric IDs representing a specific subset of questions to search}
  \item{fromDate}{When present, an early bound on results to return, either as a \code{POSIXct} date or an Unix timestamp}
  \item{toDate}{When present, an upper bound on results to return, either as a \code{POSIXct} date or an Unix timestamp}
  \item{min}{When present, minimum value for the current sort}
  \item{max}{When present, maximum value for the current sort}
  \item{sort}{When present, how results are to be sorted, one of
    \code{activity} (default), \code{creation} or \code{votes}}
  \item{order}{When present, how results should be ordered, one of
    \code{desc} (default) or \code{asc}}
  \item{tagged}{An optional vector of tags to limit the search space}
  \item{site}{Which StackExchange site to query}
}
\details{
  The \code{getQuestions} function is a standard search for questions.

  The \code{getUnansweredQuestions} function will limit the search to questions which are considered unanswered.  This
  means that there are no answers with at least one upvote for the question.

  The \code{getNoAnswerQuestions} function will limit the search to questions which have no answers.

  The \code{getQuestionLinks} function will return any questions which link to the questions represented by \code{ids}

  The \code{getQuestionRelated} function will return questions which are deemed related to those identified by \code{ids}
}
\value{
  A list of \code{seQuestion} objects
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{seQuestion}}
}
\examples{
   zz <- getQuestions(num=5, fromDate=as.POSIXct('2009-09-25'), toDate=as.POSIXct('2010-05-30'))
   ids <- sapply(zz, function(x) x$getQuestionID())
   getQuestionRelated(num=3, ids=ids)

   xx <- getNoAnswerQuestions(num=3)
   yy <- getUnansweredQuestions(num=2, sort='views')
   getQuestionLinks(ids=sapply(yy, function(x) x$getQuestionID()))
 }
\keyword{interface}
