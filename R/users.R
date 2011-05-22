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
              bronzeBadges = 'numeric',
              site = 'character'),
            methods = list(
              topAnswersByTags = function(tags, num=NULL) {
                call <- paste('users', .self$userID, 'tags', sep='/')
                json <- seInterfaceObj$request(call, tags, 'top-answers', NULL, 'answers', num=num,
                                               site=.self$site)
                buildAnswers(json, site=.self$site)
              },
              topQuestionsByTags = function(tags, num=NULL) {
                call <- paste('users', .self$userID, 'tags', sep='/')
                json <- seInterfaceObj$request(call, tags, 'top-questions', NULL, 'questions', num=num,
                                               site=.self$site)
                buildQuestions(json, site=.self$site)
              },
              topTagsByAnswers = function(num=NULL) {
                call <- paste('users', .self$userID, 'top-answer-tags', sep='/')
                json <- seInterfaceObj$request(call, NULL, NULL, NULL, 'top_tags', num=num, site=.self$site)
                buildTopTags(json, .self, site=.self$site)
              },
              topTagsByQuestions = function(num=NULL) {
                call <- paste('users', .self$userID, 'top-question-tags', sep='/')
                json <- seInterfaceObj$request(call, NULL, NULL, NULL, 'top_tags', num=num, site=.self$site)
                buildTopTags(json, .self, site=.self$site)
              }
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
  userBase(ids, params=NULL, num=num, site=site)
}

searchUsers <- function(num=NULL, filter=NULL, fromDate=NULL, toDate=NULL,
                        min=NULL, max=NULL, sort=NULL, order=NULL,
                        site='stackoverflow') {
  params <- buildCommonArgs(filter=filter, fromDate=fromDate, toDate=toDate,
                            min=min,
                           max=max, sort=sort, order=order)
  userBase(NULL, params, num, site)
}

userBase <- function(ids, params, num, site) {
  jsonList <- seInterfaceObj$request('users', ids, NULL, params, 'users',
                                     num=num, site=site)
  sapply(jsonList, buildUser, site)
}

buildUser <- function(x, site) {
  seUserFactory$new(userID = x[['user_id']],
                      userType = x[['user_type']],
                    creationDate = convertDate(x[['creation_date']]),
                    displayName = x[['display_name']],
                    reputation = x[['reputation']],
                    lastAccessDate = convertDate(x[['last_access_date']]),
                    aboutMe = x[['about_me']],
                    questionCount = x[['question_count']],
                    answerCount = x[['answer_count']],
                    viewCount = x[['view_count']],
                    upVoteCount = x[['up_vote_count']],
                    downVoteCount = x[['down_vote_count']],
                    goldBadges = x[['badge_counts']][['gold']],
                    silverBadges = x[['badge_counts']][['silver']],
                    bronzeBadges = x[['badge_counts']][['bronze']],
                    site = site)
}
