## FIXME:  DONE

setRefClass("sePrivilege",
            fields = list(
              shortDescription = 'character',
              description = 'character',
              reputation = 'numeric',
              site = 'character'),
            methods = list()
            )
sePrivilegeFactory <- getRefClass('sePrivilege')
sePrivilegeFactory$accessors(names(sePrivilegeFactory$fields()))

setMethod('show', signature('sePrivilege'), function(object) {
  print(object$getShortDescription())
})

getPrivileges <- function(num=NULL, site='stackoverflow') {
  jsonList <- doAPICall(paste(getAPIStr(site),
                              'privileges', sep=''))
  if ((!is.null(num)) && (length(jsonList) > num))
    jsonList <- jsonList[1:num]
  sapply(jsonList$privileges, function(x) {
    sePrivilegeFactory$new(shortDescription = x[['short_description']],
                           description = x[['description']],
                           reputation = x[['reputation']],
                           site = site)
  })
}
