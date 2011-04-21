## FIXME:  Skipping wikis for now

getTop <- function(obj, type, period) {
  if (! period %in% c('month', 'all-time'))
    stop("period parameter must be 'month' or 'all-time'")
  json <- doAPICall(paste(getAPIStr(obj$site), 'tags/',
                          obj$name, '/', type, '/', period, sep=''))
  sapply(json[['top_users']], buildUser, obj$getSite())
}

setRefClass("seTag",
            fields = list(
              name = 'character',
              count = 'numeric',
              fulfillsRequired = 'logical',
              site = 'character'),
            methods = list(
              topAskers = function(period) {
                getTop(.self, 'top-askers', period)
              },
              topAnswerers = function(period) {
                getTop(.self, 'top-answerers', period)
              }
            )
            )

seTagFactory <- getRefClass('seTag')
seTagFactory$accessors(names(seTagFactory$fields()))

setMethod('show', signature('seTag'), function(object) {
  print(object$getName())
})

getTags <- function(num=NULL, filter=NULL, fromDate=NULL, toDate=NULL,
                       min=NULL, max=NULL, sort=NULL, order=NULL,
                    site='stackoverflow') {
  baseURL <- paste(getAPIStr(site), 'tags?pagesize=100', sep='')
  baseURL <- buildCommonArgs(baseURL, filter=filter, min=min, max=max,
                             sort=sort, order=order, fromDate=fromDate,
                             toDate=toDate)
  baseTags(baseURL, site, num)
}

getTagSynonyms <- function(num=NULL, tags=NULL, fromDate=NULL, toDate=NULL,
                           min=NULL, max=NULL, sort=NULL, order=NULL,
                           site='stackoverflow') {
  if (is.null(tags))
    tagStr <- ''
  else
    tagStr <- paste(paste(tags, collapse=';'), '/', sep='')
  
  baseURL <- paste(getAPIStr(site), 'tags/', tagStr, 'synonyms?pagesize=100', sep='')
  baseURL <- buildCommonArgs(baseURL, filter=filter, min=min, sort=sort,
                             order=order, fromDate=fromDate, toDate=toDate)
  baseTags(baseURL, site, num)
}

getTagWikis <- function(tags, num=NULL, site='stackoverflow') {
  ## FIXME:  Not implemented at the moment
}
            
baseTags <- function(url, site, num=NULL) {
  jsonList <- doTotalList(url, 'tags', num)
  sapply(jsonList, function(x) {
    seTagFactory$new(name = x[['name']],
                     count = x[['count']],
                     fulfillsRequired = x[['fulfills_required']],
                     site = site)
  })
}
