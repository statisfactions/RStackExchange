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
  if (is.null(ids))
    idStr <- ''
  else
    idStr <- paste(paste(ids, collapse=';'), '/', sep='')

  baseURL <- paste(getAPIStr(site), '/answers', idStr,
                   '?pagesize=100&body=true&comments=true',sep='')
  baseURL <- buildCommonArgs(baseURL, min=NULL, max=NULL, sort=NULL,
                             order=NULL, fromDate=NULL, toDate=NULL)
  jsonList <- doTotalList(baseURL, 'answers', num)
  sapply(jsonList, buildAnswer)
}

buildAnswer <- function(x, site) {
  curUser <- getUsers(x[['owner']][['user_id']], num=1, site=site)
  if (length(curUser) == 0)
    curUser <- seUserFactory$new()
  else
    curUser <- curUser[[1]]
  comments <- lapply(x[['comments']], buildComment, site)  
  seAnswerFactory$new(answerID = x[['answer_id']],
                      accepted = ifelse(x[['accepted']], TRUE, FALSE),
                      questionID = x[['question_id']],
                      creationDate = as.POSIXct(x[['creation_date']],
                        origin='1970-01-01'),
                      lastActivityDate = as.POSIXct(x[['last_activity_date']],
                        origin='1970-01-01'),
                      upVoteCount = x[['up_vote_count']],
                      downVoteCount = x[['down_vote_count']],
                      score = x[['score']],
                      communityOwned = ifelse(x[['community_owned']],
                        TRUE, FALSE),
                      title = x[['title']],
                      body = x[['body']],
                      site = site, owner=curUser, comments=comments)
}

          
