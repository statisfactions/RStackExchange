## FIXME:  Skipping wikis for now

getTop <- function(obj, type, period) {
  if (! period %in% c('month', 'all-time'))
    stop("period parameter must be 'month' or 'all-time'")
  call <- paste('tags', obj$getName(), type, period, sep='/')
  json <- seInterfaceObj$request(call, NULL, NULL, NULL, type='top_users',
                                 site=obj$getSite())
  ## These are partial user objects, get the IDs and retrieve the full ones
  ids <- sapply(json, function(x) x[['user']][['user_id']])
  getUsers(ids=ids)
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
  params <- buildCommonArgs(filter=filter, fromDate=fromDate, toDate=toDate,
                            min=min, max=max, sort=sort, order=order)
  jsonList <- seInterfaceObj$request('tags', NULL, NULL, params, 'tags', num=num,
                                     site=site)
  sapply(jsonList, function(x) {
    seTagFactory$new(name = x[['name']],
                     count = x[['count']],
                     fulfillsRequired = x[['fulfills_required']],
                     site = site)
  })

}

getTagSynonyms <- function(num=NULL, tags=NULL, fromDate=NULL, toDate=NULL,
                           min=NULL, max=NULL, sort=NULL, order=NULL,
                           site='stackoverflow') {
  ## FIXME:  This is all screwed up, need a separate tag synonym class
}

getTagWikis <- function(tags, num=NULL, site='stackoverflow') {
  ## FIXME:  Not implemented at the moment
}

