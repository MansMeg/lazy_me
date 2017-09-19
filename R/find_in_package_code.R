#' Find specific code snippets in packages.
#'
#' @details Assumes that a .Rproj exists
#' 
#' @param pattern Same as in stringr::str_detect.
#' @param sub_path Path to look in.
#' 
#' @return \code{data.frame} with file and line where the pattern is found.
find_in_package <- function(pattern, sub_path){
  checkmate::assert_character(pattern)
  files <- dir(getwd())
  rproj_file_idx <- which(stringr::str_detect(tolower(files), "\\.rproj$"))
  path <- parse_rproj(file_path = files[rproj_file_idx])$PackagePath
  if(is.null(path)) path <- sub_path else path <- file.path(path, sub_path)
  file_names <- dir(path, full.names = TRUE)
  code_list <- list()
  for(f in seq_along(file_names)){
    code_lines <- readLines(file_names[f], warn = FALSE)
    code_list[[length(code_list) + 1]] <- 
      data.frame(file=rep(file_names[f], length(code_lines)),
                 line = 1:length(code_lines),
                 code = code_lines, stringsAsFactors = FALSE)
  }
  code_df <- do.call(rbind, code_list)
  res <- code_df[stringr::str_detect(code_df$code, pattern=pattern),]
  rownames(res) <- NULL
  res
}



#' Find specific code snippets in packages.
#'
#' @details Assumes that a .Rproj exists
#' 
#' @param pattern Same as in stringr::str_detect.
#' 
#' @return \code{data.frame} with file and line where the pattern is found.
#' @export
find_in_package_code <- function(pattern){
  checkmate::assert_character(pattern)
  find_in_package(pattern, "R")
}
#' @rdname find_in_package_code
#' @export
find_in_package_vignette <- function(pattern){
  checkmate::assert_character(pattern)
  find_in_package(pattern, "vignettes")
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


#' Find non ASCII characters in packages.
#'
#' @details Assumes that a .Rproj exists
#' 
#' @param pattern Same as in stringr::str_detect.
#' @param sub_path Path to look in.
#' 
#' @return \code{data.frame} with file and line where the pattern is found.
find_nonascii_in_package <- function(sub_path = "R"){
  files <- dir(getwd())
  rproj_file_idx <- which(stringr::str_detect(tolower(files), "\\.rproj$"))
  path <- parse_rproj(file_path = files[rproj_file_idx])$PackagePath
  if(is.null(path)) path <- sub_path else path <- file.path(path, sub_path)
  file_names <- dir(path, full.names = TRUE)
  code_list <- list()
  for(f in seq_along(file_names)){ # f <-1
    code_lines <- readLines(file_names[f], warn = FALSE)
    code_list[[length(code_list) + 1]] <- 
      data.frame(file=rep(file_names[f], length(code_lines)),
                 line = 1:length(code_lines),
                 code = code_lines, stringsAsFactors = FALSE)
  }
  code_df <- do.call(rbind, code_list)
  non_ascii <- suppressMessages(tools::showNonASCII(code_df$code))
  res <- code_df[code_df$code %in% non_ascii,]
  rownames(res) <- NULL
  res
}



#' Find specific code snippets in packages.
#'
#' @details Assumes that a .Rproj exists
#' 
#' @param pattern Same as in stringr::str_detect.
#' @param path Path to look in.
#' @param file_pattern Regexp identifying the right files. 
#' 
#' @return \code{data.frame} with file and line where the pattern is found.
#' 
#' @export
find_in_path <- function(pattern, path = ".", file_pattern = "\\.[rR]$"){
  checkmate::assert_character(pattern)
  file_names <- dir(path, full.names = TRUE, recursive = TRUE)
  file_names <- file_names[stringr::str_detect(file_names, file_pattern)]
  code_list <- list()
  for(f in seq_along(file_names)){
    code_lines <- readLines(file_names[f], warn = FALSE)
    code_list[[length(code_list) + 1]] <- 
      data.frame(file=rep(file_names[f], length(code_lines)),
                 line = 1:length(code_lines),
                 code = code_lines, stringsAsFactors = FALSE)
  }
  code_df <- do.call(rbind, code_list)
  res <- code_df[stringr::str_detect(code_df$code, pattern=pattern),]
  rownames(res) <- NULL
  res
}

