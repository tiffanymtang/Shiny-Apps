convert2RangerTree <- function(tree_info) {
  # convert output of randomForest::getTree() to output of ranger::treeInfo()
  tree_info <- tree_info %>%
    rownames_to_column("nodeID") %>%
    rename("leftChild" = "left daughter",
           "rightChild" = "right daughter",
           "splitvarName" = "split var",
           "splitval" = "split point",
           "terminal" = "status") %>%
    mutate(splitval = ifelse(is.na(prediction), splitval, NA),
           leftChild = ifelse(is.na(prediction), leftChild-1, NA),
           rightChild = ifelse(is.na(prediction), rightChild-1, NA),
           nodeID = as.numeric(nodeID) - 1,
           terminal = terminal == -1)
  return(tree_info)
}
