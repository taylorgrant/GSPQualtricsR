remove_html <- function(string) {
  return(gsub("<.*?>", "", string))
}
