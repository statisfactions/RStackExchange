setRefClass("seAnswer",
            fields = list(
              answerID = 'character',
              accepted = 'logical',
              questionID = 'character',
              creationDate = 'POSIXct',
              lastActivityDate = 'POSIXct',
              upVoteCount = 'numeric',
              downVoteCount = 'numeric',
              score = 'numeric',
              owner = 'seUser',
              communityOwned = 'logical',
              title = 'character',
              comments = 'list',
              body = 'character',
              site = 'character'),
            methods = list(
              getQuestion = function() {
                getQuestions(num=1, ids=.self$questionID, site=.self$site)
              }
              )
            )

seAnswerFactory <- getRefClass('seAnswer')
seAnswerFactory$accessors(names(seAnswerFactory$fields()))

setMethod("show", signature="seAnswer", function(object) {
  print(object$getTitle())
})

getAnswers <- function(num=NULL, ids=NULL, fromDate=NULL, toDate=NULL,
                       min=NULL, max=NULL, sort=NULL, order=NULL,
                       site='stackoverflow') {
  params <- buildCommonArgs(fromDate=fromDate, toDate=toDate, min=min,
                            max=max, sort=sort, order=order)
  jsonList <- seInterfaceObj$request('answers', ids, NULL, params,
                                     'answers', num=num, site=site)
  buildAnswers(jsonList, site)
}

buildAnswers <- function(jsonList, site) {
  userIDs <- sapply(jsonList, function(x) x[['owner']][['user_id']])
  users <- getUsers(userIDs, length(userIDs), site)
  ## users might not necessarily match up with userIDs due to missing values,
  ## duplications, etc
  ## Attach the IDs as the names to the user list, and then pass both into
  ## the mapply(), this allows us to cycle through the IDs and then match
  ## the appropriate user
  names(users) <- sapply(users, function(x) x$getUserID())
  mapply(function(json, userID, users, site) {
   comments <- buildComments(json[['coments']], site)
   if (userID %in% names(users))
     curUser <- users[[as.character(userID)]]
   else
     curUser <- seUserFactory$new()
 
    seAnswerFactory$new(answerID = json[['answer_id']],
                        accepted = ifelse(json[['accepted']], TRUE, FALSE),
                        questionID = json[['question_id']],
                        creationDate = convertDate(json[['creation_date']])
                        lastActivityDate = convertDate(json[['last_activity_date']]),
                        upVoteCount = json[['up_vote_count']],
                        downVoteCount = json[['down_vote_count']],
                        score = json[['score']],
                        communityOwned = ifelse(json[['community_owned']],
                          TRUE, FALSE),
                        title = json[['title']],
                        body = json[['body']],
                        site = site, owner=curUser, comments=comments)   
  }, jsonList, userIDs, MoreArgs=list(site=site, users=users))
}

          
