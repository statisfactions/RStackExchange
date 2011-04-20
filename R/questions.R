setRefClass('seQuestion',
            fields = list(
              questionID = 'character',
              answerCount = 'numeric',
              favoriteCount = 'numeric',
              owner = 'seUser',
              creationDate = 'POSIXct',
              lastActivityDate = 'POSIXct',
              upVoteCount = 'numeric',
              downVoteCount = 'numeric',
              viewCount = 'numeric',
              score = 'numeric',
              communityOwned = 'logical',
              title = 'character',
              comments = 'list',
              tags = 'list',
              answers = 'list',
              site = 'character'),
            methods = list()
            )

seQuestionFactory <- getRefClass('seQuestion')
seQuestionFactory$accessors(names(seQuestionFactory$fields()))

setMethod("show", signature='seQuestion', function(object) {
  print(object$getTitle())
})

buildQuestion <- function(x, site) {    
  curUser <- getUsers(x[['owner']][['user_id']], num=1, site=site)
  if (length(curUser) == 0)
    curUser <- seUserFactory$new()
  else
    curUser <- curUser[[1]]
  comments <- lapply(x[['comments']], buildComment, site)
  answers <- lapply(x[['answers']], buildAnswer, site)
  ## FIXME:  Dont' see a way right now to convert the tags return into
  ## seTag objects
  seQuestionFactory$new(questionID= x[['question_id']],
                        creationDate = as.POSIXct(x[['creation_date']],
                          origin='1970-01-01'),
                        lastActivityDate = as.POSIXct(x[['last_activity_date']],
                          origin='1970-01-01'),
                        upVoteCount = x[['up_vote_count']],
                        downVoteCount = x[['down_vote_count']],
                        favoriteCount = x[['favorite_count']],
                        answerCount = x[['answer_count']],
                        viewCount = x[['view_count']],
                        score = x[['score']],
                        communityOwned = ifelse(x[['community_owned']],
                          TRUE, FALSE),
                        title = x[['title']],
                        site = site, owner=curUser, comments=comments,
                        tags = as.list(x[['tags']]),
                        answers = answers,
                        site = site)
}

getQuestions <- function(num=NULL, ids=NULL, fromDate=NULL, toDate=NULL,
                         min=NULL, max=NULL, sort=NULL, order=NULL,
                         tagged=NULL, site='stackoverflow') {
  jsonList <- questionBase(num=num, ids=NULL, fromDate=fromDate, toDate=toDate,
                           min=min, max=max, sort=sort, order=order,
                           tagged=tagged)
  sapply(jsonList, buildQuestion, site)
}

getUnansweredQuestions <- function(num=NULL, fromDate=NULL, toDate=NULL,
                                   min=NULL, max=NULL, sort=NULL, order=NULL,
                                   tagged=NULL, site='stackoverflow') {
  jsonList <- questionBase(num=num, ids=NULL, fromDate=fromDate, toDate=toDate,
                           min=min, max=max, sort=sort, order=order,
                           urlExtras='/unanswered', site=site, tagged=tagged)
  sapply(jsonList, buildQuestion, site)
}

getNoAnswerQuestions <- function(num=NULL, fromDate=NULL, toDate=NULL,
                                 min=NULL, max=NULL, sort=NULL, order=NULL,
                                 tagged=NULL, site='stackoverflow') {
  jsonList <- questionBase(num=num, ids=NULL, fromDate=fromDate, toDate=toDate,
                           min=min, max=max, sort=sort, order=order,
                           urlExtras='/no-answers', site=site, tagged=tagged)
  sapply(jsonList, buildQuestion, site)
}

getQuestionLinks <- function(num=NULL, ids=NULL, fromDate=NULL,
                             toDate=NULL, min=NULL, max=NULL, sort=NULL,
                             order=NULL, site='stackoverflow') { 
  jsonList <- questionBase(num=num, ids=NULL, fromDate=fromDate, toDate=toDate,
                           min=min, max=max, sort=sort, order=order,
                           urlExtras='/linked', site=site)
  sapply(jsonList, buildQuestion, site)
}

getQuestionRelated <- function(num=NULL, ids=NULL, fromDate=NULL,
                               toDate=NULL, min=NULL, max=NULL, sort=NULL,
                               order=NULL, site='stackoverflow') {
  jsonList <- questionBase(num=num, ids=NULL, fromDate=fromDate, toDate=toDate,
                           min=min, max=max, sort=sort, order=order, site=site,
                           urlExtras='/related')
  sapply(jsonList, buildQuestion, site)
}

## FIXME:  Skipping timeline for now

questionBase <- function(num=NULL, ids=NULL, fromDate=NULL, toDate=NULL,
                         min=NULL, max=NULL, sort=NULL, order=NULL,
                         tagged=NULL, site='stackoverflow', urlExtras='',
                         field='questions') {
  if (is.null(ids))
    idStr <- ''
  else
    idStr <- paste(paste(ids, collapse=';'), '/', sep='')
  
  baseURL <- paste(getAPIStr(site), '/questions', idStr, urlExtras,
                   '?pagesize=100&body=true&comments=true&answers=true',
                   sep='')
  baseURL <- buildCommonArgs(baseURL, NULL, min, max, sort, order,
                             fromDate, toDate)
  if (!is.null(tagged))
    baseURL <- paste(baseURL, '&tagged=', paste(tagged, collapse=';'), sep='')
  doTotalList(baseURL, field, num)
}

          
