# Automatically locate the data files in Dropbox

if (Sys.info()['sysname'] == 'Darwin') {
  info <- RJSONIO::fromJSON(
    file.path(path.expand("~"),'.dropbox','info.json'))
}
if (Sys.info()['sysname'] == 'Windows') {
  info <- RJSONIO::fromJSON(
    if (file.exists(file.path(Sys.getenv('APPDATA'), 'Dropbox','info.json'))) {
      file.path(Sys.getenv('APPDATA'), 'Dropbox', 'info.json')
    } else {
    file.path(Sys.getenv('LOCALAPPDATA'),'Dropbox','info.json')
    }
  )
}

dropbox_base <- info$personal$path
datadir <- normalizePath(file.path(dropbox_base, 'NIAMS','Bhattacharyya','Framingham','data'),
                         winslash='/')
