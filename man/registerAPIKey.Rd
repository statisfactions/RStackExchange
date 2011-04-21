\name{registerAPIKey}
\alias{registerAPIKey}
\title{
Register an API key with the package
}
\description{
An API key will allow you to make many more requests with the
StackExchange servers.  Using this function will register your API key
with the package for the duration of your R session
}
\usage{
registerAPIKey(key)
}
\arguments{
  \item{key}{An API key provided from StackExchange}
}
\details{
  Individual IP addresses are capped at 300 requests per day (note that
  an individual request to an \code{RStackExchange} function might
  entail multiple requests to the StackExchange servers).  Using an API
  key, this gets bumped up to 10,000 requests.
}
\value{
  This function is called for its side effect
}
\references{
  http://stackapps.com/apps/register and
  http://stackapps.com/questions/67/how-api-keys-work
}
\author{
Jeff Gentry
}
\examples{
   \dontrun{
       registerAPIKey('SomeKeyProvidedToYou')
   }
}
\keyword{interface}
