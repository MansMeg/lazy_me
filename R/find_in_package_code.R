#' Find specific code snippets in packages.
#'
#' @details Assumes that a .Rproj exists
#' 
#' @param pattern Same as in stringr::str_detect.
#' 
#' @return \code{data.frame} with file and line where the pattern is found.
#' @export
find_in_package_code <- function(pattern){
  checkmate::assert_character(search)
  files <- dir(getwd())
  rproj_file_idx <- which(stringr::str_detect(tolower(files), "\\.rproj$"))
  path <- parse_rproj(file_path = files[rproj_file_idx])$PackagePath
  if(is.null(path)) path <- "R" else path <- file.path(path, "R")
  file_names <- c(dir(path, full.names = TRUE), dir(path, full.names = TRUE))
  code_list <- list()
  for(f in seq_along(file_names)){
    code_lines <- readLines(file_names[f], warn = FALSE)
    code_list[[length(code_list) + 1]] <- 
      data.frame(file=rep(file_names[f], length(code_lines)),
                 line = 1:length(code_lines),
                 code = code_lines, stringsAsFactors = FALSE)
  }
  code_df <- do.call(rbind, code_list)
  code_df[stringr::str_detect(code_df$code, pattern=pattern),]
}


#' Function to parse Rproj files to R lists
#'
#' @param file_path Path to the .Rproj file
#' 
#' @export
parse_rproj <- function(file_path){
  checkmate::assert_file(file_path)
  res <- readLines(file_path)
  res <- stringr::str_split(res, ":")
  res <- res[unlist(lapply(res, function(X) length(X) == 2))]
  
  ret <- lapply(res, function(X) stringr::str_trim(X[2]))
  names(ret) <- unlist(lapply(res, function(X) stringr::str_trim(X[1])))
  ret
}