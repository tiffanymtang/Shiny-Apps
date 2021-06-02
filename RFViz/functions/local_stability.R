library(tidyverse)
library(ranger)

source("ggplot_wrappers.R", chdir = T)

# example usage/test:
# source("~/R-Utility-Toolbox/eda_plotting_functions.R", chdir = T)
# rf.fit <- ranger(Species ~., data = iris)
# out <- localFeatureStabilityRF(rf.fit, X)
# str(out)
# plotHeatmap(out, x_text_angle = T)
# plotHclustHeatmap(out, y.groups = iris$Species, x_text_angle = T)

localFeatureStabilityRF <- function(rf.fit, X, features = NULL,
                                    feature.groups = NULL, first.only = FALSE,
                                    oob = FALSE) {
  #### Function Description ####
  # function to evaluate local feature stability in RF
  # 
  # input:
  # - rf.fit = output of ranger() or of class "ranger"
  # - X = data frame of sample design matrix to evaluate local feature staiblity
  # - features = vector of features to evaluate stability for
  # - feature.groups = data frame of feature to (superfeature) group mapping with columns "feature" and "group"
  # - first.only = logical; whether or not to only include first appearance of feature in tree
  # - oob = logical; whether or not to use oob samples if X is the training data
  # 
  # output:
  # - data frame of size n x p with local RF feature stability scores
  #############################
  ntrees <- rf.fit$num.trees
  if (is.null(feature.groups)) {
    tree_infos <- map(1:ntrees, ~treeInfo(rf.fit, .x))
  } else {
    tree_infos <- map(1:ntrees, 
                      ~treeInfo(rf.fit, .x) %>%
                        left_join(y = feature.groups, by = c("splitvarName" = "feature")) %>%
                        mutate(splitvarName = group) %>%
                        select(-group))
  }
  forest_paths <- getForestPaths(tree_infos)
  terminal_node_ids <- predict(rf.fit, X, type = "terminalNodes")$predictions
  
  if (is.null(features)) {
    if (is.null(feature.groups)) {
      features <- intersect(colnames(X), rf.fit$forest$independent.variable.names)
    } else {
      features <- feature.groups %>%
        filter(feature %in% intersect(colnames(X), rf.fit$forest$independent.variable.names)) %>%
        pull(group) %>%
        unique()
    }
    features <- c(paste0(features, c("+")), paste0(features, "-"))
  }
  
  if (oob) {
    oob_idx <- do.call(cbind, rf.fit$inbag.counts) == 0  # oob index
  }
  
  out <- map_dfr(1:nrow(terminal_node_ids),
                 function(i) {
                   x_terminal_nodes <- terminal_node_ids[i, ]
                   if (oob) {
                     x_terminal_nodes <- x_terminal_nodes[oob_idx[i, ]]
                     fpaths <- forest_paths[oob_idx[i, ]]
                   } else {
                     fpaths <- forest_paths
                   }
                   freqs <- map2(fpaths, x_terminal_nodes,
                                 function(x, y) {
                                   unique_path <- rev(unique(x[[as.character(y)]]))
                                   if (first.only) {
                                     unique_path <- unique_path[!duplicated(str_remove_all(unique_path, "[[+/-]]"))]
                                   }
                                   return(unique_path)
                                 }) %>%
                     reduce(c) %>%
                     factor(levels = features) %>%
                     table()
                   return(c(freqs / length(x_terminal_nodes)))
                 })
  
  return(out)
}

localIntStabilityRF <- function(rf.fit, X, ints, feature.groups = NULL,
                                first.only = FALSE, oob = FALSE) {
  #### Function Description ####
  # function to evaluate local feature interaction stability in RF
  # 
  # input:
  # - rf.fit = output of ranger() or of class "ranger"
  # - X = data frame of sample design matrix to evaluate local feature staiblity
  # - ints = vector of interactions to evaluate stability for
  # - feature.groups = data frame of feature to (superfeature) group mapping with columns "feature" and "group"
  # - first.only = logical; whether or not to only include first appearance of feature in tree
  # - oob = logical; whether or not to use oob samples if X is the training data
  # 
  # output:
  # - data frame of size n x p with local RF interaction stability scores
  #############################
  ntrees <- rf.fit$num.trees
  if (is.null(feature.groups)) {
    tree_infos <- map(1:ntrees, ~treeInfo(rf.fit, .x))
  } else {
    tree_infos <- map(1:ntrees, 
                      ~treeInfo(rf.fit, .x) %>%
                        left_join(y = feature.groups, by = c("splitvarName" = "feature")) %>%
                        mutate(splitvarName = group) %>%
                        select(-group))
  }
  forest_paths <- getForestPaths(tree_infos)
  terminal_node_ids <- predict(rf.fit, X, type = "terminalNodes")$predictions
  
  if (is.null(ints)) {
    stop("Must provide ints argument.")
  } else {
    ints_name <- ints
    ints <- str_split(ints, "_")
    names(ints) <- ints_name
  }
  
  if (oob) {
    oob_idx <- do.call(cbind, rf.fit$inbag.counts) == 0  # oob index
  }
  
  out <- map_dfr(1:nrow(terminal_node_ids),
                 function(i) {
                   x_terminal_nodes <- terminal_node_ids[i, ]
                   if (oob) {
                     x_terminal_nodes <- x_terminal_nodes[oob_idx[i, ]]
                     fpaths <- forest_paths[oob_idx[i, ]]
                   } else {
                     fpaths <- forest_paths
                   }
                   map2_dfr(fpaths, x_terminal_nodes,
                            function(x, y) {
                              unique_path <- rev(unique(x[[as.character(y)]]))
                              if (first.only) {
                                unique_path <- unique_path[!duplicated(str_remove_all(unique_path, "[[+/-]]"))]
                              }
                              map_lgl(ints, ~all(.x %in% unique_path))
                            }) %>%
                     colMeans()
                 })
  
  return(out)
}

runPermutationTest <- function(stab_df, y, feature, nperm = 1e4,
                               return.plot = F) {
  #### Function Description ####
  # function to evaluate permutation test for local stability scores using difference in means test statistic
  # 
  # input:
  # - stab_df = stability data frame; output of localFeatureStabilityRF() or localIntStabilityRF()
  # - y = vector of observed responses (in same order as rows of stab_df)
  # - feature = feature to test; character
  # - nperm = number of permutations
  # - return.plot = logical; whether or not to return permutation dist plot
  # 
  # output: list of 4
  # - pval = permutation p-value
  # - T_obs = observed test statistic
  # - perm_dist = permutation distribution (if return.plot = TRUE)
  # - plt = plot of permutation distribution
  #############################
  x <- stab_df[, feature]
  
  T_obs <- mean(x[y == 1]) - mean(x[y == 0])
  
  perm_out <- replicate(
    n = nperm,
    expr = {
      y_perm <- sample(y, size = length(y), replace = F)
      mean(x[y_perm == 1]) - mean(x[y_perm == 0])
    }
  )
  
  if (return.plot) {
    plt_df <- data.frame(`T` = perm_out)
    plt <- plotHistogram(data = plt_df, x.str = "T") +
      geom_vline(aes(xintercept = T_obs), color = "red") +
      labs(title = feature)
  } else {
    plt <- NULL
  }
  
  return(list(pval = mean(abs(perm_out) > abs(T_obs)),
              T_obs = T_obs,
              perm_dist = perm_out,
              plt = plt))
}

getForestPaths <- function(tree_infos) {
  #### Function Description ####
  # function to extract all root-to-leaf paths in a forest
  # 
  # input:
  # - tree_infos = list of size num.trees, with each entry being the output of 
  #     treeInfo()
  # 
  # output:
  # - list of size num.trees, with each entry being a list of decision paths in
  #     each tree
  #############################
  
  forest_paths <- map(tree_infos, ~getTreePaths(.x))
  return(forest_paths)
}

getTreePaths <- function(tree_info) {
  #### Function Description ####
  # function to extract all root-to-leaf paths in a tree
  # 
  # input:
  # - tree_info = output of treeInfo() for a single tree
  # 
  # output:
  # - list of decision paths in a single tree
  #############################
  
  terminal_node_ids <- tree_info$nodeID[tree_info$terminal]
  inner_tree_info <- tree_info %>%
    filter(!terminal)
  tree_paths <- list()
  for (terminal_node_id in terminal_node_ids) {
    node_id <- terminal_node_id
    tree_path <- c()
    while (node_id != 0) {
      if (node_id %in% inner_tree_info$leftChild) {
        idx <- inner_tree_info$leftChild == node_id
        node_id <- inner_tree_info$nodeID[idx]
        tree_path <- c(tree_path, 
                       paste0(inner_tree_info$splitvarName[idx], "-"))
      } else {
        idx <- inner_tree_info$rightChild == node_id
        node_id <- inner_tree_info$nodeID[idx]
        tree_path <- c(tree_path,
                       paste0(inner_tree_info$splitvarName[idx], "+"))
      }
    }
    tree_paths[[as.character(terminal_node_id)]] <- tree_path
  }
  return(tree_paths)
}