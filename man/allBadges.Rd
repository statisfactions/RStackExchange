\name{allBadges}
\alias{allBadges}
\alias{nameBadges}
\alias{tagBadges}
\alias{badgeRecipients}
\title{Functions to manipulate StackExchange badges}
\description{
  These functions allow you to download and manipulate badges from a
  StackExchange site
}
\usage{
allBadges(num=NULL, site='stackoverflow')
nameBadges(num=NULL, site='stackoverflow')
tagBadges(num=NULL, site='stackoverflow')
badgeRecipients(ids, fromDate=NULL, toDate=NULL, num=NULL,
                site='stackoverflow')
}
\arguments{
  \item{num}{An upper bound on the number of results to return}
  \item{ids}{A vector of badge IDs to look up}
  \item{fromDate}{When present, an early bound on results to return,
    either as a \code{POSIXct} date or an Unix timestamp}
  \item{toDate}{When present, an upper bound on results to return,
    either as a \code{POSIXct} date or an Unix timestamp}
  \item{site}{Which StackExchange site to poll}
}
\details{
  The \code{allBadges} function will return all badges in the site in
  alphabetical order

  The \code{nameBadges} function will return all non-tagged-based badges
  in alphabetical order.

  The \code{tagBadges} function will return all tagged-based badges in
  alphabetical order.
}
\value{
  All but \code{badgeRecipients} return a list of \code{\link{seBadge}}
  objects, while \code{badgeRecipients} returns a list of
  \code{\link{seUser}} objects.
}
\author{Jeff Gentry}
\seealso{\code{\link{seBadge}}, \code{\link{seUser}}}
\examples{
    zz <- allBadges(num=25)
    ids <- sapply(zz, function(x) x$getBadgeID())
    xx <- badgeRecipients(ids)
}