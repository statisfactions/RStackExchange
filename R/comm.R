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
              calls = 'list',
              maxCallsPerPeriod = 'integer',
              periodLength = 'integer'
              ),
            methods = list(
              initialize = function(...) {
                .self$maxCallsPerPeriod <- 29
                .self$periodLength <- 6
                callSuper(...)
              },
              pushTime = function() {
                .self$calls <- c(Sys.time(), .self$calls)[1:.self$maxCallsPerPeriod]
              },
              checkQueue = function() {
                if (!is.na(.self$calls[[.self$maxCallsPerPeriod]])) {
                  ## Block until enough time has passed
                  while (as.numeric(.self$calls[[1]] -
                                    .self$calls[[.self$maxCallsPerPeriod]]) > .self$periodLength) {
                    TRUE
                  }
                }
                TRUE
              }
              )
            )


setRefClass('seInterface',
            fields = list(
              callQueue = 'apiCallQueue'
              ),
            methods = list(
              initialize = function(...) {
                .self$callQueue <- getRefClass('apiCallQueue')$new()
                callSuper(...)
              },
              request = function(call, vectorized, postVectorized, params, type=NULL, num=NULL,
                site='stackoverflow') {
                key <- try(getAPIKey(), silent=TRUE)
                if (!inherits(key, 'try-error'))
                  params[key] <- key
                paramStr <- paste(paste(names(params), params, sep='='), collapse='&')

                if (length(vectorized) == 0) {
                  vectorStrs <- character()
                } else {
                  ## FIXME:
                  ## URLencode() does not encode for hyphens, but StackExchange requires encoded
                  ## hyphens when it comes to the vectorized inputs, for now, change these ourselves
                  vectorized <- gsub('-', '%3B', vectorized)
                  ## long vectorized strings can make overly long URLs, batch these if
                  ## necessary to keep sane URL lengths
                  ## The throttling in this regard is apparently aggressive, see:
                  ## http://stackapps.com/questions/619/url-length-limit-for-for-requests-taking-vectorised-ids-answers-id-question
                  ## FIXME:
                  ## For now, I'm going to put this extremely low just to get this working
                  vectorStrs <- sapply(split(vectorized, ceiling(seq_along(vectorized) / 10)),
                                       paste, collapse=';')
                }
                urls <- paste(getAPIStr(site), '/', call, '/', vectorStrs, '/',
                              postVectorized, '?', paramStr, sep='')
                out <- list()
                for (url in urls) {
                  ## We need to be careful of throttling here, as 30 calls in 5 seconds will
                  ## get us blocked.  Being conservative (queue holds only 29 elements and we'll
                  ## diff on 6 seconds)
                  .self$callQueue$pushTime()
                  .self$callQueue$checkQueue()
                  json <- getURL(url, .opts=list(encoding='identity,gzip'))
                  curResults <- fromJSON(json)
                  if ('error' %in% names(curResults)) {
                    stop("Error ", curResults$error$code, ': ', curResults$error$message)
                  }
                  if (!is.null(type))
                    curResults <- curResults[[type]]
                  out <- c(out, curResults)
                }
                if ((!is.null(num)) && (length(out) > num))
                  out <- out[1:num]
                out
              }
              )
            )

seInterfaceObj <- getRefClass('seInterface')$new()
              
## FIXME:  This should be a subclass of seInterface or at least another method, should help some
## of the special casing in the request() method and make it less goofy
doTotalList <- function(call, vectorized, postVectorized, params, type, num=NULL, site='stackoverflow') {
  ## Some responses have a total/page/pagesize parameter before
  ## the list requested.  This is a generic handler for those
  ## situations, getting the requested number of respones and
  ## returning the appropriate list
  params[['pagesize']] <- 100
  if (!is.null(num) && (num < 0))
    stop("num argument must be a positive value")
  
  jsonList <- list()
  n <- 0
  while (TRUE) {
    n <- n + 1
    params[['page']] <- n
    ## Note that in this function we'll be managing the 'type' and 'num' separately
    newJSON <- seInterfaceObj$request(call, vectorized, postVectorized, params, site=site)
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
