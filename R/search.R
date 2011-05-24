searchStackSite <- function(num=NULL, intitle=NULL, notTagged=NULL,
                            tagged=NULL, min=NULL, max=NULL, order=NULL,
                            sort=NULL, site='stackoverflow.com') {
  params <- buildCommonArgs(min=min, max=max, order=order, sort=sort)
  for (extraParam in c('intitle', 'notTagged', 'tagged')) {
    val <- get(extraParam)
    if (!is.null(val))
      params[[extraParam]] <- val
  }

  jsonList <- seInterfaceObj$request('search', NULL, NULL, params,
                                     'questions', num=num, site=site)
  ## FIXME: Here is the problem - these questions aren't complete.
  ## For now I'm going to do the horribly inefficient thing and regrab
  ## the questions based on the IDs to get the full versions
  ids <- sapply(jsonList, function(x) x[['question_id']])
  getQuestions(ids=ids, num=num)
}

## FIXME:  I'm intentionally skipping the 'similar' stuff for now
