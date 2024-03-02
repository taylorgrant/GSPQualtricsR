## function to convert from one shiny tab to another ##
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

remove_html <- function(string) {
  return(gsub("<.*?>", "", string))
}

survey_duration <- function(x){
  paste0(gsub("\\..*", "", x %% 60),":", gsub(".*\\.", "", round(x %% 60, 2)))
}

