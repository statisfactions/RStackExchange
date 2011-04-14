## FIXME:  DONE

setRefClass("seComment",
            fields = list(
              commentID = 'character',
              creationDate = 'POSIXct',
              postID = 'character',
              postType = 'character',
              score = 'numeric',
              body = 'character',
              owner = 'seUser'),
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
                        idsArePosts=FALSE, site='stackoverflow') {
  if (!is.null(ids))
    idStr <- paste(ids, collapse=';')
  else
    idStr <- ''
  
  if (idsArePosts) {
    if (length(ids) < 1)
      stop("Must provide at least one post ID if idsArePosts=TRUE")
    apiStr <- paste("posts/", idStr, '/comments', sep='')
  } else {
    apiStr <- paste('/comments/', idStr, sep='')
  }
  baseURL <- paste(getAPIStr(site), apiStr, '?pagesize=100', sep='')

  for (arg in c('min', 'max', 'sort', 'order')) {
    val <- get(arg)
    if (!is.null(val))
      baseURL <- paste(baseURL, '&', arg, '=', val, sep='')
  }
  if (!is.null(fromDate))
    baseURL <- paste(baseURL, '&fromdate=', as.numeric(fromDate),
                     sep='')
  if (!is.null(toDate))
    baseURL <- paste(baseURL, '&todate=', as.numeric(toDate), sep='')
  jsonList <- doTotalList(baseURL, 'comments', num)
  sapply(jsonList, function(x) {
    curUser <- getUsers(x[['owner']][['user_id']], num=1, site=site)
    if (length(curUser) == 0)
      curUser <- seUserFactory$new()
    else
      curUser <- curUser[[1]]
    
    seCommentFactory$new(commentID = x[['comment_id']],
                         creationDate = as.POSIXct(x[['creation_date']],
                           origin='1970-01-01'),
                         postID = x[['post_id']],
                         postType = x[['post_type']],
                         score = x[['score']],
                         body = x[['body']],
                         owner = curUser)
  })
}

