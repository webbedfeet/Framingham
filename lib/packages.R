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
  library(r, character.only=TRUE)
}

