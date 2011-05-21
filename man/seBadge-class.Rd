\name{seBadge-class}
\Rdversion{1.1}
\docType{class}
\alias{seBadge-class}
\alias{seBadge}
\alias{seBadgeFactory}
\alias{show,seBadge-method}
\title{Class \code{"seBadge"}: A class to represent a StackExchange badge}
\description{
Provides a model representing a badge from a StackExchange site
}
\section{Fields}{
  \describe{
    \item{\code{badgeID}:}{Internal ID of the badge}
    \item{\code{rank}:}{The ranking of the badge, e.g. gold, silver}
    \item{\code{name}:}{The name of the badge}
    \item{\code{description}:}{A longer description of the badge}
    \item{\code{awardCount}:}{The number of times this badge has been awarded}
    \item{\code{tagBased}:}{Logical, is this badge tag based?}
    \item{\code{site}:}{StackExchange site this badge belongs to}
  }
}
\details{
  The \code{seBadge} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{seBadgeFactory}.  Accessor set &
  get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}}).  As
  an example, the \code{name} field could be accessed with
  \code{object$getName()} and \code{object$setName()}.
}
\author{
Jeff Gentry
}
\seealso{
  \code{\link{allBadges}}, \code{\link{nameBadges}},
  \code{\link{tagBadges}}, \code{\link{badgeRecipients}},
  \code{\link{setRefClass}}
}
\examples{
  zz <- allBadges(num=5)
  sapply(zz, function(x) x$getDescription())
}
\keyword{classes}
