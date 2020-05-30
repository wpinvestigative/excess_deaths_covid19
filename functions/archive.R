# Using ExcessILI's data archiving functions, returns the most recent copy of
# output obtained by running a function or formula \code{f}, unless this 
# copy doesn't exist or is older (by modification time) than \code{maxage}.
# In that case, \code{f} is run and the output is archived into the folder
# Data/'storeName' as an RDS file, using the function ExcessILI::storeRDS.
#
# @param storeName A string. The name of the folder to store output in
# @param f A function or formula taking no arguments. Formulas are coerced to
#   functions.
# @param maxage How old can any existing archived file be before \code{f} is 
#   called again?
runIfExpired <- function(storeName, f, maxage=hours(1)) {
  basepath <- "Data/"
  mostRecent <- mostRecentTimestamp(storeName, basepath=basepath)
  f <- rlang::as_function(f)
  
  runAndArchive <- function() {
    data <- f()
    storeRDS(data, storeName, basepath)
    data
  }
  
  if (is.na(mostRecent)) 
    return(runAndArchive())
  if (mostRecent %--% now() < maxage)
    return(retrieveRDS(storeName, basepath))
  runAndArchive()
}
