subsetReadForest <- function(read.forest, subset.idcs) {
  # Subset nodes from readforest output
  if (!is.null(read.forest$node.feature))
    read.forest$node.feature <- read.forest$node.feature[subset.idcs,]
  
  if (!is.null(read.forest$tree.info))
    read.forest$tree.info <- read.forest$tree.info[subset.idcs,]
  
  if (!is.null(read.forest$node.obs))
    read.forest$node.obs <- read.forest$node.obs[,subset.idcs]
  
  return(read.forest)
}

int2Id <- function(int, varnames.grp, signed=FALSE, split=FALSE) {
  # Determine integer index of named variable (signed or not)
  if (!split) int <- str_split(int, '_')[[1]]
  
  if (signed) {
    sgn <- grep('\\+$', int)
    varnames.grp <- str_remove_all(varnames.grp, '[\\+\\-]')
    int <- str_remove_all(int, '[\\+\\-]')
  }
  
  varnames.grp <- unique(varnames.grp)
  id <- sapply(int, function(i) which(varnames.grp == i))
  if (signed) {
    adjust <- rep(0, length(int))
    adjust[sgn] <- length(varnames.grp)
    id <- id + adjust
  }
  
  return(id)
}

int2IdGrouped <- function(int, varnames.grp, signed = FALSE, split = FALSE) {
  if (!split) int <- str_split(int, '_')[[1]]
  
  if (signed) {
    sgn <- grep('\\+$', int)
    varnames.grp <- str_remove_all(varnames.grp, '[\\+\\-]')
    int <- str_remove_all(int, '[\\+\\-]')
  }
  
  id <- which(varnames.grp == int)
  if (signed) {
    adjust <- rep(0, length(int))
    adjust[sgn] <- length(varnames.grp)
    id <- id + adjust
  }
  
  return(c(id))
}

sampleTree <- function(k, tree, size) { 
  # Sample a leaf node from specified tree
  tree.id <- which(tree == k)
  if (length(tree.id) == 1) return(tree.id)
  sample.id <- sample(tree.id, 1, prob=size[tree.id])
  return(sample.id)
}

myInteractPredict <- function (x, int, read.forest, varnames = NULL, min.nd = 1, 
                               default.pred = NULL, return.int.active = FALSE)
  # generate predictions from RF decision rules corresponding to a signed interaction 
  # (same as iRF::interactPredict but allows for grouping "hyper-features" for RIT search)
  #
  # Args:
  # - x = numeric feature matrix
  # - int = a signed interaction. Formatted as 'X1+_X2+_X3-_...'
  # - read.forest = output of readForest(rand.forest = rf.fit, x = x, varnames.grp = colnames(x))
  # - varnames = grouping "hyper-features" for RIT search
  # - min.nd = minimum leaf node size to extract decision rules from
  # - default.pred = default prediction when interaction is not active
  # - return.int.active = T/F; whether or not to return the number of times each
  #     interaction is active for each observation
  #
  # Output:
  # - a vnumeric vector of length nrow(x), entries indicating a predicted response for the 
  #   corresponding observation. Predictions are generated from RF decision rules using 
  #   only the features in int
{
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("X", 1:ncol(x))
  }
  if (is.null(varnames)) {
    varnames <- colnames(x)
  }
  p <- ncol(x)
  n <- nrow(x)
  hyper.feats <- p != length(unique(varnames))
  stopifnot(p == ncol(read.forest$node.feature)/2)
  
  class.rf <- all(read.forest$tree.info$prediction %in% 0:1)
  id.keep <- read.forest$tree.info$size.node >= min.nd
  if (class.rf) {
    id.keep <- id.keep & read.forest$tree.info$prediction == 1
  }
  read.forest <- subsetReadForest(read.forest, id.keep)
  tree.info <- read.forest$tree.info
  
  if (is.null(default.pred)) {
    if (class.rf) {
      default.pred <- 0
    } else {
      default.pred <- weighted.mean(x = tree.info$prediction,
                                    w = tree.info$size.node / sum(tree.info$size.node))
    }
  }
  
  int <- strsplit(int, "_")[[1]]
  stopifnot(length(int) > 1)
  int.clean <- str_remove_all(int, "[-\\+]")
  if (!hyper.feats) {  # no hyper-features
    int.nf <- int2Id(int, varnames, split = TRUE, signed = TRUE)
    int.x <- int.nf%%p + p * (int.nf%%p == 0)
    int.pos <- int.nf > p
    
    nf <- read.forest$node.feature[, int.nf]
    x <- x[, int.x]
    
    int.nds <- Matrix::rowMeans(nf != 0) == 1
    if (sum(int.nds) < 2) {
      warning("interaction does not appear on RF paths")
      if (!return.int.active) {
        out <- rep(NA, length(x))
      } else {
        out <- list(pred = rep(NA, length(x)),
                    feat.active = rep(NA, length(x)))
      }
      return(out)
    }
    nf <- nf[int.nds, ]

  } else {  # for hyper-features
    int.nf <- lapply(int, int2IdGrouped, 
                     varnames.grp = varnames, split = TRUE, signed = TRUE)
    int.x <- lapply(int.nf, FUN = function(int) {
      return(int %% p + p * (int %% p == 0))
    })
    int.pos <- lapply(int.nf, FUN = function(int) {
      return(int > p)
    })
    
    # get nodes and x of the interaction features
    nf.ls <- lapply(int.nf, FUN = function(int) {
      if (length(int) == 1) {
        return(as.matrix(read.forest$node.feature[, int]))
      } else {
        return(read.forest$node.feature[, int])
      }
    })
    x.ls <- lapply(int.x, FUN = function(int) {
      return(as.matrix(x[, int]))
      # if (length(int) == 1) {
      #   return(x %>% select(int))
      # } else {
      #   return(x[, int])
      # }
    })
    
    # get paths with the interactions
    int.nds <- mapply(nf.ls, FUN = function(nf) {
      return(Matrix::rowSums(nf != 0) > 0)
    })
    int.nds <- Matrix::rowMeans(int.nds != 0) == 1
    if (sum(int.nds) < 2) {
      warning("interaction does not appear on RF paths")
      return(rep(0, nrow(x)))
    }
    nf.ls <- lapply(nf.ls, FUN = function(nf) {
      return(nf[int.nds, ])
    })
    
    x <- do.call(what = cbind, args = x.ls)
    nf <- do.call(what = cbind, args = nf.ls)
    int.pos <- do.call(what = c, args = int.pos)
  }
  
  if (any(!int.pos)) {
    x[, !int.pos] <- -x[, !int.pos]
    nf[, !int.pos] <- -nf[, !int.pos]
  }
  
  tree.info <- tree.info[int.nds, ]
  y <- tree.info$prediction
  size <- tree.info$size.node
  trees <- unique(tree.info$tree)
  
  tx <- t(x)
  ss <- sapply(trees, sampleTree, tree = tree.info$tree, size = size)
  nrule <- length(ss)
  preds <- numeric(n)
  int.actives <- numeric(n)
  for (s in ss) {
    nf.s <- nf[s, ]
    idx.active <- which(nf.s != 0)
    int.active <- Matrix::colSums(tx[idx.active, ] > nf.s[idx.active]) == length(int)
    preds <- preds + (int.active * y[s] + (1 - int.active) * default.pred)
    if (return.int.active) {
      int.actives <- int.actives + int.active
    }
  }
  
  if (!return.int.active) {
    out <- preds / nrule
  } else {
    out <- list(pred = preds / nrule,
                int.active = int.actives / nrule)
  }
  return(out)
}

