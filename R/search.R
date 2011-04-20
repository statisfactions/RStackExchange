searchStackSite <- function(num=NULL, intitle=NULL, notTagged=NULL,
                            tagged=NULL, min=NULL, max=NULL, order=NULL,
                            sort=NULL, site='stackoverflow') {
  baseURL <- paste(getAPIStr(site), 'search?pagesize=100', sep='')
  baseURL <- buildCommonArgs(baseURL, min=min, max=max, order=order,
                             sort=sort)
  if (!is.null(intitle))
    baseURL <- paste(baseURL, '&intitle=', intitle, sep='')
  if (!is.null(notTagged))
    baseURL <- paste(baseURL, '&nottagged=', paste(notTagged, collapse=';'),
                     sep='')
  if (!is.null(tagged))
    baseURL <- paste(baseURL, '&tagged=', paste(tagged, collapse=';'), sep='')
  jsonList <- doTotalLIst(baseURL, 'questions', num)
  ## FIXME: Here is the problem - these questions aren't complete.
  ## For now I'm going to do the horribly inefficient thing and regrab
  ## the questions based on the IDs to get the full versions
  getQuestions(ids=sapply(jsonList, function(x) x[['question_id']]))
}

## FIXME:  I'm intentionally skipping the 'similar' stuff for now
