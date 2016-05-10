MUSTACHE <- "\\{\\{(.*?)\\}\\}"

#' Remove all mustaches from string, leaving their contents
#'
#' @param s string, e.g. "my name is {{name}}"
rm_mustache <- function(s){
  gsub(MUSTACHE, "\\1", s)
}

#' Get all unique expressions inside mustaches
#'
#' @param s string
#' @export
#' @examples
#' template <- "name: {{name}}, id: {{id}}"
#' uniq_mustache(template)     # c('name', 'id')
uniq_mustache <- function(s){
  matches <- unlist(regmatches(s, gregexpr(MUSTACHE, s) ))
  rm_mustache(unique(matches))
}

#' Fill in template using mustache syntax
#'
#' @param template string
#' @param args list (or coercible to list) used for context in expanding mustaches
#' @export
#' @examples
#' template <- "name: {{name}}, id: {{id}}"
#' inst_template(template, list(name='michael', id=1))
inst_template <- function(template, args){
  GetoptLong::qq(template, envir = as.list(args), code.pattern = "\\{\\{CODE\\}\\}")
}

