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


## The apiCallQueue class is just a way to prevent rate throttling.
## We can't send more than 30 requests in a 5 second period.
setRefClass('apiCallQueue',
            fields = list(
              calls = 'list'
              ),
            methods = list(
              pushTime = function() {
                .self$calls <- c(Sys.time(), .self$calls)[1:29]
              },
              curDiff = function() {
                ## returns 7 if the calls list isn't full (29 elements) as
                ## by definition we don't need to rate limit
                if (is.na(.self$calls[[29]]))
                  return(7)
                as.numeric(.self$calls[[1]] - .self$calls[[29]])
              }
              )
            )

apiCallQueueFactory <- getRefClass('apiCallQueue')
callQueue <- apiCallQueueFactory$new()

doAPICall <- function(url, ...) {
  ## FIXME:  Will get blocked if more than 30 requests in 5 seconds are made.
  ##   Detect if this happens and automagically limit things.
  key <- try(getAPIKey(), silent=TRUE)
  if (!inherits(key, 'try-error'))
    url <- paste(url, '&key=', key)
  ## We need to be careful of throttling here, as 30 calls in 5 seconds will get us
  ## blocked.  Being conservative (queue holds only 29 elements, and we'll diff on 6 seconds)
  callQueue$pushTime()
  while (callQueue$curDiff() < 6) {
    TRUE
  }
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
