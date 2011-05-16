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
  badgeBase('badges', num, site)
}

nameBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase('badges/name', num, site)
}
  
tagBadges <- function(num=NULL, site='stackoverflow') {
  badgeBase('badges/tags', num, site)
}

badgeRecipients <- function(ids, fromDate=NULL, toDate=NULL, num=NULL,
                            site='stackoverflow') {
  if (length(ids) < 1)
    stop("Must provide at least one badge ID")
  params <- buildCommonArgs(fromDate=fromDate, toDate=toDate)
  userBase(ids, params, num, site)
}

badgeBase <- function(call, num, site) {
  jsonList <- seInterfaceObj$request(call, NULL, NULL, NULL, 'badges', num=num, site)
  sapply(jsonList, function(x) {
    seBadgeFactory$new(badgeID = x[['badge_id']],
                       rank = x[['rank']],
                       name = x[['name']],
                       description = x[['description']],
                       awardCount = x[['award_count']],
                       tagBased = x[['tag_based']],
                       site = site)
  })
}
