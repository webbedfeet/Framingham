reqpackages <- c(
  'tidyverse',
  'haven',
  'stringr',
  'nat.utils'
)

`%notin%` <- Negate(`%in%`)
for(r in reqpackages){
  if (r %notin% installed.packages()[,'Package']){
    install.packages(r, repos='http://cran.rstudio.com')
  }
  if(r=='tidyverse'){
    suppressMessages(library(r, character.only = TRUE))
  } else {
  library(r, character.only=TRUE)
  }
}
cat(paste0('Packages loaded: ', paste(sort(reqpackages), collapse=', '), '\n'))
