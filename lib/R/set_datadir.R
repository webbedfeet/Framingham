#' Sets the directory where the data resides in a OS-specific manner
#'
#' @return The file path of the data dir
#' @export
#'
#' @examples
set_datadir <- function(){
  os <- Sys.info()['sysname']
  if (os == 'Windows'){
    datadir <- file.path('C:','Users','dasgupab','Dropbox','NIAMS','Bhattacharyya','Framingham','data')
  } else {
    datadir = path.expand('~/Dropbox/NIAMS/Bhattacharyya/Framingham/data')
  }
  return(datadir)
}
