## FIXME:  DONE

setRefClass("seBadge",
            fields = list(
              badgeID = 'character',
              rank = 'character',
              name = 'character',
              description = 'character',
              awardCount = 'numeric',
              tagBased = 'logical'),
            )

seBadgeFactory <- getRefClass('seBadge')
seBadgeFactory$accessors(names(seBadgeFactory$fields()))

setMethod("show", signature('seBadge'), function(object) {
  print(object$getName())
})

allBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase(paste(getAPIStr(site), 'badges', sep=''), num)
}

nameBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase(paste(getAPIStr(site), 'badges/name', sep=''), num)
}
  
tagBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase(paste(getAPIStr(site), 'badges/tags', sep=''), num)
}

badgeRecipients <- function(ids, fromDate=NULL, toDate=NULL, num=NULL,
                            site='stackoverflow') {
  if (length(ids) < 1)
    stop("Must provide at least one badge ID")
  
  baseURL <- paste(getAPIStr(site), "badges/",
                   paste(ids, collapse=";"),
                   "?pagesize=100", sep='')

  if (!is.null(fromDate))
    baseURL <- paste(baseURL, '&fromdate=', as.numeric(fromDate),
                     sep='')
  if (!is.null(toDate))
    baseURL <- paste(baseURL, '&todate=', as.numeric(toDate), sep='')

  userBase(baseURL, num)
}

badgeBase <- function(url, num=NULL) {
  jsonList <- doAPICall(url)
  if ((!is.null(num)) && (length(jsonList) > num))
    jsonList <- jsonList[1:num]
  sapply(jsonList$badges, function(x) {
    seBadgeFactory$new(badgeID = x[['badge_id']],
                       rank = x[['rank']],
                       name = x[['name']],
                       description = x[['description']],
                       awardCount = x[['award_count']],
                       tagBased = x[['tag_based']])
  })
}
