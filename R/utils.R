getAPIStr <- function(site='stackoverflow') {
  paste('http://api.', site, '.com/1.1/', sep='')
}

buildCommonArgs <- function(baseURL, filter, min, max, sort, order, fromDate, toDate) {
  for (arg in c('filter', 'min', 'max', 'sort', 'order')) {
    val <- get(arg)
    if (!is.null(val))
      baseURL <- paste(baseURL, '&', arg, '=', val, sep='')
  }
  if (!is.null(fromDate))
    baseURL <- paste(baseURL, '&fromdate=', as.numeric(fromDate),
                     sep='')
  if (!is.null(toDate))
    baseURL <- paste(baseURL, '&todate=', as.numeric(toDate), sep='')

  baseURL
}
