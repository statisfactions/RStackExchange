getAPIStr <- function(site='stackoverflow.com') {
  paste('http://api.', site, '/1.1/', sep='')
}

convertDate <- function(stackDate) {
  ## converts a date from stackexchange format to POSIXct
  conv <- try(as.POSIXct(stackDate, origin='1970-01-01'), silent=TRUE)
  if (inherits(conv, 'try-error'))
    NULL
  else
    conv
}

buildCommonArgs <- function(filter=NULL, min=NULL,
                            max=NULL, sort=NULL, order=NULL,
                            fromdate=NULL, todate=NULL) {
  out <- list()

  if (!is.null(fromdate))
    fromdate <- as.numeric(fromdate)
  if (!is.null(todate))
    todate <- as.numeric(todate)
  for (arg in names(formals())) {
    val <- get(arg)
    if (length(val) > 0)
      out[[arg]] <- val
  }
  out
}
