################################################################################
## File : ezdev-rstudio-plugins.R
## Project : ezdev
## Package : ezdev
## Author : Brandon Loudermilk
##
## Description : Utilities for efficient package development in R Studio.
##
## Last Modified : 2016-12-08
##
################################################################################




#' @title Insert document header
#'
#' @description Insert document skeleton header. Extracts information from
#' current file, R Studio project, and package structures to populate header.
#'
#' @export
docHeader <- function() {
  LC <- paste0(rep("#", 78),collapse ='')
  COMMENT <- "##"
  FILE <- "File"
  PROJECT <- "Project"
  PACKAGE <- "Package"
  AUTHOR <- "Author"
  COLON <- ":"
  LASTMOD <- "Last Modified"
  LF <- "\n"
  DESCRIPTION <- "Description"
  LOREM <- "Lorem ipsum dolor sit amet"

  DESC_FILE <- "DESCRIPTION"

  proj <- rstudioapi::getActiveProject()
  proj_name <- if (!is.null(proj)) {basename(proj)} else {NA}

  sec <- rstudioapi::getSourceEditorContext()
  file <- sec$path
  file_name <- if(file != "") {basename(file)} else {NA}

  package_name <- author_name <- NA
  if (!is.na(proj_name)) {
    if (file.exists(file.path(proj, DESC_FILE))) {
      desc_content <- read.dcf(file.path(proj, DESC_FILE), fields = c("Package", "Author"))
      df <- data.frame(desc_content) # make it easier to work with
      package_name <- df$Package
      author_name <- df$Author
    }
  }

  insertLine <- function(txt, sep = ' ') {
    txt <- paste(COMMENT, txt, LF, sep=sep)
    out_dp <- rstudioapi::document_position(insert_row, 1)
    rstudioapi::insertText(location = out_dp, text = txt, id = sec$id)
    insert_row <<- insert_row + 1
  }

  insert_row <- 1
  insertLine(LC, sep = '')
  insertLine(paste(FILE, COLON, file_name))
  insertLine(paste(PROJECT, COLON, proj_name))
  insertLine(paste(PACKAGE, COLON, package_name))
  insertLine(paste(AUTHOR, COLON, author_name))
  insertLine("")
  insertLine(paste(DESCRIPTION, COLON, LOREM))
  insertLine("")
  insertLine(paste(LASTMOD, COLON, Sys.Date()))
  insertLine("")
  insertLine(LC, sep = '')

}


#' @title Insert function header
#'
#' @description With an R script open in R Studio, place the cursor on a
#' function you have written and call this function - it will insert a
#' roxygen2 skeleton function header template.
#'
#' @export
funcHeader <- function() {
  COMMENT <- "#'"
  TITLE <- "@title"
  DESCRIPTION <- "@description"
  PARAM <- "@param"
  EXPORT <- "@export"
  LF <- "\n"
  BLANK <- ""
  TYPE <- "(datatype)"
  DEF <- "default :"

  insertLine <- function(txt) {
    txt <- paste(COMMENT, txt, LF)
    out_dp <- rstudioapi::document_position(insert_row, 1)
    rstudioapi::insertText(location = out_dp, text = txt, id = dc$id)
    insert_row <<- insert_row + 1
  }

  isFunc <- function(row) {
    header <- dc$contents[row]
    stringr::str_detect(header, pattern = "function")
  }

  getTitleArgs <- function(row) {
    header <- dc$contents[row]
    pieces <- unlist(strsplit(x = header, "<-"))
    f_name <- stringr::str_trim(pieces[1])
    fun_piece <- stringr::str_trim(pieces[2])
    pieces <- unlist(strsplit(x = fun_piece, "\\("))
    fun_piece <- stringr::str_trim(pieces[2])
    pieces <- unlist(strsplit(x = fun_piece, "\\)"))
    args <- stringr::str_trim(stringr::str_trim(pieces[1]))
    args <- stringr::str_trim(unlist(strsplit(args, ",")))
    return(list(title = f_name, args = args))
  }

  insertArg <- function(a) {
    requireNamespace("stringr")
    if (!grepl(pattern = "=", a)) {
      insertLine(paste(PARAM, a, TYPE))
    } else {
      pieces <- stringr::str_trim(unlist(strsplit(x = a, split = "=")))
      aa <- pieces[1]
      def <- pieces[2]
      insertLine(paste(PARAM, aa, TYPE))
    }
    insertLine(BLANK)
  }

  dc <- rstudioapi::getActiveDocumentContext()
  start <- dc$selection[[1]]$range$start
  insert_row <- start_row <- start["row"]
  start_col <- start["column"]

  end <- dc$selection[[1]]$range$end

  if (!isFunc(start_row)) {
    print("Place cursor on function header")
    return(NULL)
  }
  ta <- getTitleArgs(start_row)


  insertLine(paste(TITLE, ta$title))
  insertLine(BLANK)
  insertLine(paste(DESCRIPTION, "Lorem ipsum dolor sit amet"))
  insertLine(BLANK)
  lapply(X = ta$args, insertArg)
  insertLine(EXPORT)

}


#' @title Create small random sample data.frame
#'
#' @description Create a small random data.frame for manipulation and testing.
#'
#' @return (data.frame)
#'
#' @export
DF <- function() {

  a <- "nrows <- 10;\n"
  b <- "df <- data.frame(c1 = 1:nrows,
  c2 = round(runif(n = nrows),2),
  c3 = factor(sample(month.abb[1:3], nrows, replace = T)),
  c4 = as.logical(rbinom(n = 10, 1, .5)),
  c5 = sapply(X = 1:nrows, function(i)paste0(sample(letters, sample(3:10), TRUE), collapse = '')),
  stringsAsFactors = F);"
  res <- paste(a,b)
  rstudioapi::sendToConsole(res, execute = FALSE)
}



