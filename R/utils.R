getAPIStr <- function(site='stackoverflow') {
  paste('http://api.', site, '.com/1.1/', sep='')
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
                            fromDate=NULL, toDate=NULL) {
  out <- list()
  fromDate <- as.numeric(fromDate)
  toDate <- as.numeric(toDate)
  for (arg in c('filter', 'min', 'max', 'sort', 'order', 'fromDate', 'toDate')) {
    val <- get(arg)
    if (length(val) > 0)
      out[[arg]] <- val
  }
  out
}
