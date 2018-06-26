packages <- c("purrr", "kernlab", "data.table")
sapply(packages, function(packaage) 
{
  if(!require(package, character.only = TRUE))
  {
    install.packages(package)
    require(package, character.only = TRUE)
  }
})
