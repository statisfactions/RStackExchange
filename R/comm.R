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
                .self$calls <- c(Sys.time(),
                                 .self$calls)[1:.self$maxCallsPerPeriod]
              },
              checkQueue = function() {
                if (!is.na(.self$calls[[.self$maxCallsPerPeriod]])) {
                  ## Block until enough time has passed
                  while (as.numeric(Sys.time() -
                                    .self$calls[[.self$maxCallsPerPeriod]]) <
                         .self$periodLength) {
                    print("throttling")
                    TRUE
                  }
                }
                TRUE
              }
              )
            )

vectorizeArgs <- function(args) {
  ## NULL values are no good, remove those.  
  nulls <- which(sapply(args, is.null))
  if (length(nulls) > 0)
    args <- args[-nulls]

  ## FIXME:
  ## URLencode() does not encode for hyphens, but
  ## StackExchange requires encoded
  ## hyphens when it comes to the vectorized inputs, for now,
  ## change these ourselves
  args <- gsub('-', '%3B', args)
 
  ## long vectorized strings can make overly long URLs,
    ## batch these if necessary to keep sane URL lengths
  ## The throttling in this regard is apparently aggressive,
    ## see:
    ## http://stackapps.com/questions/619/url-length-limit-for-for-requests-taking-vectorised-ids-answers-id-question
    ## FIXME:
    ## For now, I'm going to put this extremely low just to get
    ## this working
    sapply(split(args, ceiling(seq_along(args) / 10)),
           paste, collapse=';')
}  

setRefClass('seInterface',
            fields = list(
              callQueue = 'apiCallQueue'
              ),
            methods = list(
              initialize = function(...) {
                .self$callQueue <- getRefClass('apiCallQueue')$new()
                callSuper(...)
              },
              request = function(call, vectorized, postVectorized, params,
                type=NULL, num=NULL, site='stackoverflow') {
                if (!is.null(num) && (num < 0))
                  stop("num argument must be a positive value")

                params[['pagesize']] <- 100
                key <- try(getAPIKey(), silent=TRUE)
                if (!inherits(key, 'try-error'))
                  params['key'] <- key
                paramStr <- paste(paste(names(params), params, sep='='),
                                  collapse='&')
                if (length(vectorized) == 0) {
                  vectorStrs <- character()
                } else {
                  if ((is.null(num)) ||
                      (num > length(vectorized))) {
                    num <-  length(vectorized)
                  }
                  vectorStrs <- vectorizeArgs(vectorized)
                }
                urls <- paste(getAPIStr(site), '/', call, '/', vectorStrs, '/',
                              postVectorized, '?', paramStr, sep='')
                out <- list()
                for (url in urls) {
                  page <- 0
                  while (TRUE) {
                    page <- page + 1
                    url <- paste(url, '&page=', page, sep='')
                    ## We need to be careful of throttling here, as 30 calls in
                    ## 5 seconds will get us blocked.  Being conservative
                    ## (queue holds only 29 elements and we'll diff on 6 seconds)
                    .self$callQueue$pushTime()
                    .self$callQueue$checkQueue()
                    json <- getURL(url, .opts=list(encoding='identity,gzip'))
                    curResults <- fromJSON(json)
                    if ('error' %in% names(curResults)) {
                      stop("Error ", curResults$error$code, ': ',
                           curResults$error$message)
                    }
                    curTypeResults <- curResults[[type]]
                    out <- c(out, curTypeResults)
                    if (((!is.null(num))&&(length(out) >= num)) ||
                        (is.null(curResults$total)) ||
                        (length(curTypeResults) == curResults$total))
                      break
                  }
                }
                
                if ((!is.null(num)) && (length(out) > num))
                  out <- out[1:num]
                
                out
              }
              )
            )

seInterfaceObj <- getRefClass('seInterface')$new()
