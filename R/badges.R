## FIXME:  DONE

setRefClass("seBadge",
            fields = list(
              badgeID = 'character',
              rank = 'character',
              name = 'character',
              description = 'character',
              awardCount = 'numeric',
              tagBased = 'logical',
              site = 'character'),
            )

seBadgeFactory <- getRefClass('seBadge')
seBadgeFactory$accessors(names(seBadgeFactory$fields()))

setMethod("show", signature('seBadge'), function(object) {
  print(object$getName())
})

allBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase(paste(getAPIStr(site), 'badges', sep=''), site, num)
}

nameBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase(paste(getAPIStr(site), 'badges/name', sep=''), site, num)
}
  
tagBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase(paste(getAPIStr(site), 'badges/tags', sep=''), site, num)
}

badgeRecipients <- function(ids, fromDate=NULL, toDate=NULL, num=NULL,
                            site='stackoverflow') {
  if (length(ids) < 1)
    stop("Must provide at least one badge ID")
  
  baseURL <- paste(getAPIStr(site), "badges/",
                   paste(ids, collapse=";"),
                   "?pagesize=100", sep='')
  baseURL <- buildCommonArgs(baseURL, NULL, NULL, NULL, NULL, NULL, fromDate, toDate)
  userBase(baseURL, site, num)
}

badgeBase <- function(url, site, num=NULL) {
  jsonList <- doAPICall(url)
  if ((!is.null(num)) && (length(jsonList) > num))
    jsonList <- jsonList[1:num]
  sapply(jsonList$badges, function(x) {
    seBadgeFactory$new(badgeID = x[['badge_id']],
                       rank = x[['rank']],
                       name = x[['name']],
                       description = x[['description']],
                       awardCount = x[['award_count']],
                       tagBased = x[['tag_based']],
                       site = site)
  })
}
