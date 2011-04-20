apiKey <- new.env(hash=TRUE)
requests <- new.env(hash=TRUE)

registerAPIKey <- function(key) {
  assign('key', key, envir=apiKey)
}

hasAPIKey <- function() {
  exists('key', envir=apiKey)
}

getAPIKey <- function() {
  get('key', envir=apiKey)
}

doAPICall <- function(url, ...) {
  ## FIXME:  Will get blocked if more than 30 requests in 5 seconds are made.
  ##   Detect if this happens and automagically limit things.
  key <- try(getAPIKey(), silent=TRUE)
  if (!inherits(key, 'try-error'))
    url <- paste(url, '&key=', key)
  json <- getURL(url, .opts=list(encoding="identity,gzip"))
  out <- fromJSON(json)

  if ('error' %in% names(out)) {
    stop("Error ", out$error$code, ": ", out$error$message)
  }
  
  out 
}

doTotalList <- function(baseURL, type, num=NULL) {
  ## Some responses have a total/page/pagesize parameter before
  ## the list requested.  This is a generic handler for those
  ## situations, getting the requested number of respones and
  ## returning the appropriate list
  if (!is.null(num) && (num < 0))
    stop("num argument must be a positive value")
  
  jsonList <- list()
  n <- 0
  while (TRUE) {
    n <- n + 1
    url <- paste(baseURL, "&page=", n, sep='')
    newJSON <- doAPICall(url)
    jsonList <- c(jsonList, newJSON[[type]])
    jlLen <- length(jsonList)
    if (((!is.null(num))&&(jlLen >= num)) ||
        (length(jsonList) == newJSON$total))
      break
  }

  if ((!is.null(num)) && (length(jsonList) > num))
    jsonList <- jsonList[1:num]

  jsonList
}
