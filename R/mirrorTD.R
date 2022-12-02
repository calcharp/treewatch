#'@title mirrorTD
#'@description Actively Binds a treeio treedata object to a treeplyr 
#'             treedata object for easier treeplyr interaction with ggtree
#'
#'@param varname Name of active treeio treedata variable tree create
#'@param td A treeplyr treedata object
#'
#'@return Creates an Active Binding in the environment the function was run in 
#'
#'@examples
#'\dontrun{
#'library(ggtree)
#'library(treeio)
#'
#'tree <- fishtree::fishtree_phylogeny(rank="Cyprinidae", 
#'                                     type="chronogram_mrca")
#'
#'data <- geiger::sim.char(tree, 0.5, 100) %>% matrix() %>% as.data.frame()
#'data$label <- tree$tip.label
#'
#'td <- make.treedata(tree, data)
#'
#'mirrorTD(plottingTD, td)
#'
#'plottingTD %>% 
#'  ggtree(aes(color=V1), 
#'         layout = "fan") +
#'  scale_color_continuous(low="lightblue", high="blue") +
#'  geom_tiplab(color="black", size=.5)
#'}
#'
#'
#'@import treeplyr
#'@import treeio
#'@import tidytree
#'@import tidyverse
#'
#'
#'@export

mirrorTD <- function(varname, td){
  
  #checking that class is correct
  if(!is(td, "treedata")){
    stop('td must be of class "treedata!"')
  }
  
  #getting variable name of td object as text
  name <- as.character(substitute(td))
  
  #Checking if td object table has labels to full_join with treeio tibble,
  #and creating temporary label column if not
  #
  #Making command for active binding
  if("label" %in% names(td$dat)){
    if(!all(td$dat$label == td$phy$tip.label)){
      stop('"label" column in td$dat does not match tip labels in td$phy$tip.labels')
    }
    command <- paste0("as.treedata(
                                 full_join(treeio::as_tibble(", name, "$phy), ",
                      name, "$dat, by = 'label'))")
  }else{
    command <- paste0("as.treedata(
                                 full_join(treeio::as_tibble(", name, "$phy), ",
                      "mutate(", name, "$dat, label=", name, "$phy$tip.label), by = 'label'))")
  }
  
  #Converting text command to an expression so it can be bound and evaluated
  binding <- parse(text = command)
  
  #Get proper environment to create Active Binding in
  envirst <- parent.env(environment())
  #Get variable name for Active Binding
  varname <- as.character(ensym(varname))
  
  #Create Active Binding in proper environment
  makeActiveBinding(varname, fun = function() eval(binding), env = envirst)
}



