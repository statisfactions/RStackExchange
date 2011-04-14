setRefClass("seUser",
            fields = list(
              userID = 'character',
              userType = 'character',
              creationDate = 'POSIXct',
              displayName = 'character',
              reputation = 'numeric',
              lastAccessDate = 'POSIXct',
              aboutMe = 'character',
              questionCount = 'numeric',
              answerCount = 'numeric',
              viewCount = 'numeric',
              upVoteCount = 'numeric',
              downVoteCount = 'numeric',
              acceptRate = 'numeric',
              goldBadges = 'numeric',
              silverBadges = 'numeric',
              bronzeBadges = 'numeric'),
            methods = list(
              ),
            )

seUserFactory <- getRefClass("seUser")
seUserFactory$accessors(names(seUserFactory$fields()))

setMethod("show", signature="seUser", function(object) {
  print(object$getDisplayName())
})

getUsers <- function(ids, num=NULL, site='stackoverflow') {
  ## FIXME:  See if parameters work on the site, if so
  ## work this into searchUsers
  remaining <- length(ids)
  baseURL <- paste(getAPIStr(site), "users/",
                   paste(ids, collapse=';'),
                   "?pagesize=100", sep='')
  userBase(baseURL, num)
}

searchUsers <- function(num=NULL, filter=NULL, fromDate=NULL, toDate=NULL,
                        min=NULL, max=NULL, sort=NULL, order=NULL,
                        site='stackoverflow') {
  baseURL <- paste(getAPIStr(site), "users?pagesize=100", sep='')

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
  userBase(baseURL, num)
}

userBase <- function(baseURL, num=NULL) {
  jsonList <- doTotalList(baseURL, 'users', num)
  sapply(jsonList, function(x) {
    seUserFactory$new(userID = x[['user_id']],
                      userType = x[['user_type']],
                      creationDate = as.POSIXct(x[['creation_date']],
                        origin='1970-01-01'),
                      displayName = x[['display_name']],
                      reputation = x[['reputation']],
                      lastAccessDate = as.POSIXct(x[['last_access_date']],
                        origin='1970-01-01'),
                      aboutMe = x[['about_me']],
                      questionCount = x[['question_count']],
                      answerCount = x[['answer_count']],
                      viewCount = x[['view_count']],
                      upVoteCount = x[['up_vote_count']],
                      downVoteCount = x[['down_vote_count']],
                      goldBadges = x[['badge_counts']][['gold']],
                      silverBadges = x[['badge_counts']][['silver']],
                      bronzeBadges = x[['badge_counts']][['bronze']])
  })
}
