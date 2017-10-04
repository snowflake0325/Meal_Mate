getepilink <- function(inlist) {
    itemchar = strsplit(as.character(inlist)," ")
    temp = character()
    for (items in itemchar){
        temp = c(temp,items)
    }
    itemlink = paste(temp,collapse = " ")
    startlink = ("https://www.epicurious.com/search/")
    links = sprintf('<a href="https://www.epicurious.com/search/%s" target="_blank" class="btn btn-link">Recipe!</a>',itemlink)
    #links = paste(startlink,itemlink,sep = "")
    return(links)
}
