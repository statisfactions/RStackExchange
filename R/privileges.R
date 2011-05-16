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
  jsonList <- seInterfaceObj$request('privileges', NULL, NULL, NULL, 'privileges', num=num, site)
  sapply(jsonList, function(x) {
    sePrivilegeFactory$new(shortDescription = x[['short_description']],
                           description = x[['description']],
                           reputation = x[['reputation']],
                           site = site)
  })
}
