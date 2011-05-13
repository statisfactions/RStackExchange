\name{sePrivilege-class}
\Rdversion{1.1}
\docType{class}
\alias{sePrivilege-class}
\alias{sePrivilege}
\alias{sePrivilegeFactory}
\alias{show,sePrivilege-method}
\title{Class \code{"sePrivilege"}: A class to represent a StackExhange
  privilege}
\description{
Provides a model representing a privilege from a StackExchange site
}
\section{Fields}{
  \describe{
    \item{\code{shortDescription}:}{Brief description of privilege}
    \item{\code{description}:}{In depth description of privilege}
    \item{\code{reputation}:}{Reputation required for privilege}
    \item{\code{site}:}{StackExchange site this badge belongs to}
  }
}
\details{
  The \code{sePrivilege} class is implemented as a reference class.  An
  instance of a generator for this class is provided as a convenience to
  the user as it is configured to handle most standard cases.  To access
  this generator, use the object \code{sePrivilegeFactory}.  Accessor set &
  get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}}).  As
  an example, the \code{description} field could be accessed with
  \code{object$getDescription()} and \code{object$setDescription()}.
}

\author{
Jeff Gentry
}
\seealso{
\code{\link{getPrivileges}}, \code{\link{setRefClass}}
}
\examples{
   zz <- getPrivileges(25)
   sapply(zz, function(x) x$getDescription())
}
\keyword{classes}
