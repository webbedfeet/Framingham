# Reload function

reload <- function(d = file.path(getwd(), 'lib'), cpp=FALSE) {
  if('fn' %in% search()) detach('fn')
  fn <- new.env()
  source(file.path(d, 'packages.R'))
  if(cpp){
    require(Rcpp)
    for(f in dir(file.path(d,'src'), pattern='cpp')){
      print(paste('Loading',f))
      sourceCpp(file.path(d,'src',f), env=fn, rebuild=FALSE)
    }
  }
  for(f in dir(file.path(d, 'R'), pattern='.R')){
    print(paste('Loading',f))
    source(file.path(d, 'R', f), local=fn)
  }
  attach(fn)
}
