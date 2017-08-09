make_dir <- function(fullDirName) {

  if(file.exists(fullDirName) == FALSE) {
    dir.create(fullDirName)
  }
}