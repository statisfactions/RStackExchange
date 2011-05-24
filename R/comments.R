setRefClass("seComment",
            fields = list(
              commentID = 'character',
              creationDate = 'POSIXct',
              postID = 'character',
              postType = 'character',
              score = 'numeric',
              body = 'character',
              owner = 'seUser',
              site = 'character'),
            methods = list()
            )

seCommentFactory <- getRefClass('seComment')
seCommentFactory$accessors(names(seCommentFactory$fields()))

setMethod('show', signature('seComment'), function(object) {
  print(paste(object$getOwner()$getDisplayName(), ": ",
              substr(object$getBody(), 1, 50), "...", sep=''))
})

getComments <- function(num=NULL, ids=NULL, fromDate=NULL, toDate=NULL,
                        min=NULL, max=NULL, sort=NULL, order=NULL,
                        idsArePosts=FALSE, site='stackoverflow.com') {
  params <- buildCommonArgs(fromDate=fromDate, toDate=toDate, min=min,
                            max=max, sort=sort, order=order)
  if (idsArePosts) {
    if (length(ids) < 1)
      stop("Must provide at least one post ID if idsArePosts=TRUE")
    call <- 'posts'
    postVec <- 'comments'
  } else {
    call <- 'comments'
    postVec <- NULL
  }
  jsonList <- seInterfaceObj$request(call, ids, postVec, params, 'comments',
                                     num=num, site=site)
  buildComments(jsonList, site)
}


buildComments <- function(jsonList, site) {
  userIDs <- sapply(jsonList, function(x) x[['owner']][['user_id']])
  users <- getUsers(userIDs, length(userIDs), site)
  ## users might not necessarily match up with userIDs due to missing values,
  ## duplications, etc
  ## Attach the IDs as the names to the user list, and then pass both into
  ## the mapply(), this allows us to cycle through the IDs and then match
  ## the appropriate user
  names(users) <- sapply(users, function(x) x$getUserID())
  mapply(function(json, userID, users, site) {
    if (userID %in% names(users))
      curUser <- users[[as.character(userID)]]
    else
      curUser <- seUserFactory$new()
    seCommentFactory$new(commentID = json[['comment_id']],
                         creationDate = convertDate(json[['creation_date']]),
                         postID = json[['post_id']],
                         postType = json[['post_type']],
                         score = json[['score']],
                         body = json[['body']],
                         owner = curUser,
                         site = site)  
  }, jsonList, userIDs, MoreArgs=list(site=site, users=users))
}
