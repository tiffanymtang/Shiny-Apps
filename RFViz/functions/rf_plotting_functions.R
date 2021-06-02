# functions to create rf and irf visualizations

library(tidyverse)
library(iRF)
library(R.utils)
library(plotly)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(patchwork)
library(RColorBrewer)
library(ggraph)
library(igraph)
library(ranger)
library(randomForest)

source("./irf_utilities.R")
source("./epitree_utilities.R")
source("./generate_int_surface.R")
source("./local_stability.R")
source("./eda_plotting_functions.R")
source("./myInteractPredict.R")
source("./eval_functions.R")
source("./ggplot_themes.R")

plotIntSurface <- function(X, int, y = NULL, fit = NULL, read.forest = NULL, 
                           varnames = colnames(X), col.pal = magma, 
                           xlab = NULL, ylab = NULL, zlab = NULL, slab = NULL,
                           z.range = NULL, nbin = 100, binFun = NULL, 
                           filter.rules = NULL, filterX = NULL,
                           wt.node = 'size', type = 'plotly',
                           title = paste0(int, collapse = "_")) {
  ####### Function Description ########
  #' Plot interaction surface plot
  #'
  #' @param X numeric feature matrix, with replicate features grouped
  #' @param int signed interaction to plot. If numeric, int is assumed to
  #'   correspond to column indices to be plotted for interaction. If character,
  #'   assumed to be formatted as 'X1+_X2+_X3-_...'
  #' @param y response vector.  
  #' @param fit a fitted random forest, from packages randomForest or ranger.
  #' @param read.forest output of readForest
  #' @param varnames character vector indicating feature names. By default,
  #'  colnames(x) are used as feature names.
  #' @param col.pal color palette for response surfaces. A function that takes an
  #'   integer input and returns colors to be use in the palette.
  #' @param xlab x-axis label
  #' @param ylab y-axis label
  #' @param zlab z-axis label
  #' @param slab for order 3 and 4 interactions, label for split plots
  #' @param z.range z-axix range
  #' @param nbin: number of bins to plot surface map over
  #' @param min.surface minimum number of observations required to generate a
  #'  response surface.
  #' @param filter.rules: a list of filtering functions to be applied to rf
  #'   decision paths. If NULL, default rules will filter to a random sample of
  #'   10% of leaf nodes with at least 5 observations.
  #' @param filterX a filtering function to be applied to data matrix. Takes as
  #'   arguments x (data matrix), int (numeric vector of interaciton ids), and
  #'   thresholds (numeric vector of rf thresholds, columns corresponding to
  #'   features in int), returns indicies of x to be kept.
  #' @param wt.node indicator for how nodes are to be weighted in response
  #'   surfaces. One of `size` - weighting proportional to leaf node size or
  #'   `none` - indicating uniform weighting.
  #' @param type one of `plotly` or `ggplot`
  #'   surface
  #' @param title plot title for response surfaces
  #######
  
  n <- nrow(X)
  p <- ncol(X)
  pred.prob <- is.null(y)
  title <- title
  
  # Check valididity of binning function
  if (is.null(binFun)) {
    binFun <- function(x) return(x)
    qt.bin <- FALSE
  } else if (is.character(binFun)) {
    stopifnot(binFun == 'quantile')
    qt.bin <- TRUE
  } else {
    stopifnot(is.function(binFun))
    qt.bin <- FALSE
  }
  
  # Check for one of read.forest/fit
  if (is.null(read.forest) & is.null(fit)) {
    stop('Specify one of `read.forest` or `fit`')
  }
  
  # Read out RF decision paths
  if (is.null(read.forest)) {
    read.forest <- readForest(fit, x = X, oob.importance = FALSE)
  }
  
  # Check whether read.forest is valid
  if (is.null(read.forest$node.feature)) {
    stop('read.forest missing node.feature')
  } 
  if (is.null(read.forest$node.obs)) {
    stop('read.forest missing node.obs')
  }
  
  # Set feature names and check for replicates
  varnames <- groupVars(varnames, X)
  if (is.null(colnames(X))) {
    colnames(X) <- paste0('X', 1:ncol(X))
    varnames <- colnames(X)
  }
  
  # Check for duplicate features
  if (any(duplicated(varnames))) {
    stop('Replicate features not supported')
  }
  
  # Convert binary factor
  if (is.factor(y)) {
    y <- as.numeric(y) - 1
  }
  
  # Set z-axis scaling
  if (is.null(z.range) & !pred.prob) {
    z.range <- range(y)
  } else if (is.null(z.range) & pred.prob) {
    z.range <- range(read.forest$tree.info$prediction)
  } 
  
  # Check for valid interaction and convert to numeric IDs
  if (!is.numeric(int)) {
    signed <- str_detect(int, '(\\+|-)')
    int <- int2Id(int, varnames, signed = signed)
    int <- int %% p + p * (int %% p == 0)
  }
  
  if (length(int) != 2) {
    stop("Interactions of order > 2 has not been implemented yet.")
  }
  
  # Collapse node feature matrix to unsigned
  if (ncol(read.forest$node.feature) == 2 * p) {
    read.forest$node.feature <- read.forest$node.feature[, 1:p] + 
      read.forest$node.feature[, (p + 1):(2 * p)]
  }
  
  # Generate grid of x/y values for surface maps
  bins <- NULL
  if (qt.bin) {
    bins <- quantileGrid(X, nbin, int[1:2])
  }
  
  # Extract hyperrectangles from RF decision paths
  rectangles <- forestHR(read.forest, int)
  
  # Filter data matrix if rules specified
  if (!is.null(filterX)) {
    id <- filterX(X, int, rectangles$splits)
    X <- X[id, ]
    if (!is.null(y)) {
      y <- y[id]
    }
  }
  
  # Generate surface for current plot
  surface <- genSurface(X, int[1:2],
                        y = y,
                        varnames = varnames, 
                        rectangles = rectangles, 
                        wt.node = wt.node,
                        filter.rules = filter.rules,
                        bins = bins,
                        nbin = nbin,
                        binFun = binFun)
  
  # Set quantile names for grid
  if (qt.bin) {
    colnames(surface) <- seq(0, 1, length.out = nrow(surface))
    rownames(surface) <- seq(0, 1, length.out = ncol(surface))
  }
  
  # Set axis names
  xlab <- ifelse(is.null(xlab), '', xlab)
  ylab <- ifelse(is.null(ylab), '', ylab)
  zlab <- ifelse(is.null(zlab), '', zlab)
  
  # Select plotting method, one of plotly or ggplot
  if (type == "plotly") {
    # Initialize color palette
    colors <- col.pal(100)
    quantiles <- seq(0, 1, length.out = 100)
    colorscale <- split(cbind(quantiles, colors), rep(1:100, 2))
    names(colorscale) <- NULL
    p <- plotly::plot_ly(z = ~surface, 
                         x = as.numeric(rownames(surface)), 
                         y = as.numeric(colnames(surface))) %>%
      plotly::add_surface(colorscale = colorscale) %>%
      plotly::layout(
        autosize = FALSE,
        title = title,
        scene = list(
          xaxis = list(title = xlab),
          yaxis = list(title = ylab),
          zaxis = list(title = zlab, range = z.range)
        )
      ) 
  } else if (type == "ggplot") {
    p <- suppressMessages(
      plotHeatmap(X = surface, size = 3,
                  x.labels.num = TRUE, y.labels.num = TRUE) +
        scale_fill_gradientn(colours = col.pal(100), limits = z.range) +
        scale_color_gradientn(colours = col.pal(100), limits = z.range) +
        labs(x = xlab, y = ylab, fill = zlab, title = title)
    )
  } else {
    stop("Unknown plot type.")
  }
  return(p)
}

plotiRFResults <- function(irf.out, int.df = NULL, ints = NULL, bar_width = 0.8, 
                           text_size = 0, hjust = 1.5, digits = 2,
                           metrics = c("prevalence", "precision", "stability"),
                           top_p = NULL, rank_by = NULL) {
  
  if (is.null(ints)) {
    ints <- irf.out$interaction$int
  }
  
  if (is.null(int.df)) {
    int.df <- irf.out$interaction
  }
  
  if (!is.null(rank_by)) {
    if (rank_by == "prevalence") {
      int.df <- int.df %>%
        arrange(-prevalence)
    } else {
      stop("rank_by has not been fully implemented yet.")
    }
  }
  
  if (!is.null(top_p)) {
    int.df <- int.df %>%
      slice(1:top_p)
  }
  
  int.df.long <- int.df %>%
    filter(int %in% ints) %>%
    gather(key = "metric", value = "value", -int) %>%
    filter(metric %in% metrics) %>% 
    mutate(metric = factor(metric, levels = metrics),
           int = rev(fct_inorder(rev(int))))
  
  plt <- ggplot(int.df.long) +
    aes(x = value, y = int) +
    facet_grid(~ metric, scales = "free") +
    geom_col(position = "identity", width = bar_width) + 
    myGGplotTheme(size_theme = "xlarge")
  
  if (text_size > 0) {
    plt <- plt + 
      geom_text(aes(label = round(value, digits)), 
                hjust = hjust, size = text_size, colour = "white")
  }
  
  return(plt)
}

plotEpitreeResults <- function(epitree.out, ints = NULL, ypreds = NULL,
                               bar_cols = c("#3B3B3B", "#E6AB02"),
                               bar_width = 0.8, text_size = 0, hjust = 1.5,
                               digits = 2, save = F, 
                               save.filename = 'epitree_plot.jpeg') {
  ####### Function Description ######
  #' Plot epitree results
  #'
  #' @param epitree.out output of runEpitree()
  #' @param ints vector of signed interactions to plot. If numeric, int is
  #'   assumed to correspond to column indices to be plotted for interaction. 
  #'   If character, assumed to be formatted as 'X1+_X2+_X3-_...'; if NULL,
  #'   all interactions in epitree.out will be plotted
  #' @param ypreds vector of predictions to serve as baseline prediction error
  #' @param bar_cols vector of colors to use for plotting
  #' @param bar_width width of bar
  #' @param save logical; whether or not to save plot
  #' @param save.filename name of file/path to save plot
  #######
  
  n <- length(epitree.out$data$ytest)
  epitree.results <- epitree.out$results %>%
    mutate(int = fct_rev(int))
  if (!is.null(ints)) {
    epitree.results <- epitree.results %>%
      filter(int %in% ints)
  }
  
  plt.df <- epitree.results %>%
    mutate(pC = -log10(pC),
           logLH_pC = -logLH_pC / n,
           logLA_pC = -logLA_pC / n) %>%
    gather(key = "metric", value = "value", pC, stab.pCart, logLA_pC, logLH_pC) %>%
    mutate(model = ifelse(metric == "logLH_pC", "No Epistasis", "Epistasis"),
           metric = ifelse(str_detect(metric, "logL"), "logL", metric)) %>%
    mutate_if(is.character, as.factor) %>%
    arrange(desc(model))
  
  plotPV <-  plt.df %>%
    filter(metric == "pC") %>%
    ggplot() +
    aes(y = int, x = value, fill = model) +
    geom_col(position = "identity", width = bar_width) + 
    facet_grid(order ~., space = "free", scales = "free") +
    scale_fill_manual(values = bar_cols) +
    labs(x = "-log10(PCS p-value)", y = "", fill = "") +
    guides(fill = FALSE) +
    myGGplotTheme(size_theme = "xlarge", 
                  strip.text.y = element_text(angle = 0))
  if (text_size > 0) {
    plotPV <- plotPV + 
      geom_text(aes(label = round(value, digits)), 
                hjust = hjust, size = text_size, colour = "white")
  }
  plotPV_minimal <- plotPV +
    theme(strip.text.y = element_blank())
  
  plotStab <- plt.df %>% 
    filter(metric == "stab.pCart") %>%
    ggplot() +
    aes(y = int, x = value, fill = model) +
    geom_col(position = "identity", width = bar_width) + 
    facet_grid(order ~., space = "free", scales = "free") +
    scale_fill_manual(values = bar_cols) +
    labs(x = "Stability", y = "", fill = "") +
    xlim(c(0, 1)) +
    guides(fill = FALSE) +
    myGGplotTheme(size_theme = "xlarge",
                  strip.text.y = element_text(angle = 0))
  if (text_size > 0) {
    plotStab <- plotStab + 
      geom_text(aes(label = round(value, digits)), 
                hjust = hjust, size = text_size, colour = "white")
  }
  plotStab_minimal <- plotStab +
    guides(fill = FALSE) +
    theme(strip.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  plotPA <- plt.df %>%
    filter(metric == "logL") %>%
    ggplot() +
    aes(y = int, x = value, fill = model) +
    geom_col(position = "dodge", width = bar_width) +
    facet_grid(order ~., space = "free", scales = "free") +
    scale_fill_manual(values = bar_cols) +
    labs(x = "Prediction Error \n - log P(Y|p) / n", y = "", fill = "") +
    myGGplotTheme(size_theme = "xlarge",
                  legend.position = "top",
                  legend.key.height = unit(2, "line"))
  if (!is.null(ypreds)) {
    ytest <- epitree.out$data$ytest
    baseline_err <- -sum(log(ypreds^ytest * (1 - ypreds)^(1 - ytest))) / n
    plotPA <- plotPA + 
      geom_vline(xintercept = baseline_err) +
      coord_cartesian(xlim = c(min(baseline_err, 
                                   min(plt.df$value[plt.df$metric == "logL"])),
                               NA))
  }
  if (text_size > 0) {
    plotPA <- plotPA + 
      geom_text(aes(label = round(value, digits), fill = model), 
                hjust = hjust, size = text_size, colour = "white",
                position = position_dodge(0.8))
  }
  plotPA_minimal <- plotPA +
    theme(axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.y = element_text(angle = 0))
  
  plot_combined <- plotPV_minimal + plotStab_minimal + plotPA_minimal + 
    patchwork::plot_layout(nrow = 1)
  if (save) {
    wt <- max(12/8 * length(ints), 15) * 0.5
    jpeg(file = save.filename, height = wt, width = 20, units = 'in', res = 200)
    print(plot_combined)
    dev.off()
  }
  
  return(list(plot = plot_combined, 
              plotPV = plotPV, plotStab = plotStab, plotPA = plotPA))
}

plotEpitreeInt <- function(epitree.out, int, z.range = NULL, 
                           snp = FALSE, signed = FALSE, text.size = 0,
                           col.pal = magma, save = FALSE, 
                           save.filename = paste0("response_", 
                                                  paste0(int, collapse = "_"), 
                                                  ".jpeg")) {
  ####### Function Description ######
  #' Plot epitree heatmap results for a single interaction
  #'
  #' @param epitree.out output of runEpitree()
  #' @param int signed interactions to plot. Assumed to be formatted as 
  #'   'X1+_X2+'
  #' @param z.range z axis range
  #' @param snp logical; whether or not snps are being plotted so scale is discrete
  #' @param col.pal color palette for response surfaces. A function that takes 
  #'   an integer input and returns colors to be use in the palette.
  #' @param save logical; whether or not to save plot
  #' @param save.filename name of file/path to save plot
  #######
  
  Xtrain <- epitree.out$data$Xtrain
  Xtest <- epitree.out$data$Xtest
  ytest <- epitree.out$data$ytest
  ytrain <- epitree.out$data$ytrain
  
  if (is.null(z.range)) {
    z.range <- c(min(c(ytest, ytrain)), max(c(ytest, ytrain)))
  }
  
  epitree.int.pvals <- epitree.out$pcs.pvals[[as.character(int)]]
  tree <- attr(epitree.int.pvals, "treeA")
  tree.comb <- attr(epitree.int.pvals, "treeH")
  # for tree plotting purposes only, add mean y for intepretability
  tree.comb.plt <- map(tree.comb,
                       function(x) {
                         x$frame$yval <- x$frame$yval + mean(ytrain)
                         return(x)
                       })
  varnames <- colnames(tree$model)[-1]
  
  tree.ls <- c(tree.comb, list(tree))
  names(tree.ls) <- c(varnames, "epitree")
  
  ## tree plots
  par(mfrow = c(1, 3))
  rpart.plot(tree.comb.plt[[1]], box.palette = "RdBu", 
             main = paste0("CART(", varnames[1], ")"))
  rpart.plot(tree.comb.plt[[2]], box.palette = "RdBu",
             main = paste0("CART(", varnames[2], ")"))
  rpart.plot(tree, box.palette = "RdBu", 
             main = paste0("CART(", paste(varnames, collapse = ", "), ")"))
  
  int.idx <- getIntIndex(int, colnames(Xtest), signed = signed)[[1]]
  
  if (snp) {
    surface.df <- expand.grid(0:2, 0:2) %>%
      setNames(varnames)
  } else {
    surface.df <- expand.grid(seq(min(Xtest[, int.idx[1]]), 
                                  max(Xtest[, int.idx[1]]), 
                                  length.out = 200),
                              seq(min(Xtest[, int.idx[2]]), 
                                  max(Xtest[, int.idx[2]]), 
                                  length.out = 200)) %>%
      setNames(varnames)
  }
  inter.pred <- predict(tree, newdata = surface.df)
  single.pred <- sapply(1:ncol(surface.df), 
                        function(i) {
                          predict(tree.comb[[i]], 
                                  newdata = surface.df[, i, drop = F])
                        })
  
  ## heatmaps
  epi.heatmap <- surface.df %>%
    mutate(prediction = inter.pred) %>%
    spread(key = varnames[1], value = prediction) %>%
    column_to_rownames(varnames[2]) %>%
    round(., 3) %>%
    plotHeatmap(y.labels.num = !snp, x.labels.num = !snp, size = 3, 
                text.size = text.size, position = "ordered") +
    labs(title = paste0('CART(A,B) with p-value = 10^(-', 
                        round(-log10(as.numeric(epitree.int.pvals)), 0), ')'),
         x = varnames[1], y = varnames[2], fill = "Prediction")
  
  noepi.heatmap <- surface.df %>%
    mutate(prediction = pmin(1, 
                             pmax(0, rowSums(single.pred) + mean(ytrain)))) %>%
    spread(key = varnames[1], value = prediction) %>%
    column_to_rownames(varnames[2]) %>%
    round(., 3) %>%
    plotHeatmap(y.labels.num = !snp, x.labels.num = !snp, size = 3,
                text.size = text.size, position = "ordered") +
    labs(title = 'CART(A) + CART(B)', x = varnames[1], y = varnames[2],
         fill = "Prediction")
  
  if (!snp) {
    tr.data.boot <- data.frame(Xtrain, y = ytrain)
    # tr.data.boot <- sample_n(data.frame(Xtrain, y = ytrain), 
    #                          size = length(ytrain), replace = TRUE)
    tr.data.heatmap <- ggplot(tr.data.boot) +
      aes_string(x = varnames[1], y = varnames[2], z = "y") +
      stat_summary_hex(bins = 25) +
      labs(title = "Training Data Smoothed") +
      myGGplotTheme()
    
    data.boot <- data.frame(Xtest, y = ytest)
    # data.boot <- sample_n(data.frame(Xtest, y = ytest), 
    #                       size = length(ytest), replace = TRUE)
    data.heatmap <- ggplot(data.boot) +
      aes_string(x = varnames[1], y = varnames[2], z = "y") +
      stat_summary_hex(bins = 25) +
      labs(title = "Validation Data Smoothed") +
      myGGplotTheme()
    tr.data.n.heatmap <- NULL
    data.n.heatmap <- NULL
  } else {
    tr.data.boot <- data.frame(Xtrain, y = ytrain) %>%
      # sample_n(size = length(ytrain), replace = TRUE) %>%
      select(all_of(varnames), y) %>%
      group_by(across(all_of(varnames))) %>%
      summarise(Class1 = mean(y),
                n = n())
    tr.data.heatmap <- tr.data.boot %>%
      select(-n) %>%
      spread(key = varnames[1], value = "Class1") %>%
      column_to_rownames(varnames[2]) %>%
      round(., 3) %>%
      plotHeatmap(size = 3, text.size = text.size, position = "ordered") +
      labs(title = "Training Data Smoothed", x = varnames[1], y = varnames[2]) +
      myGGplotTheme()
    tr.data.n.heatmap <- tr.data.boot %>%
      select(-Class1) %>%
      spread(key = varnames[1], value = "n") %>%
      column_to_rownames(varnames[2]) %>%
      round(., 3) %>%
      plotHeatmap(size = 3, text.size = text.size, position = "ordered") +
      labs(title = "Training Data Smoothed", x = varnames[1], y = varnames[2]) +
      myGGplotTheme()
    
    data.boot <- data.frame(Xtest, y = ytest) %>%
      # sample_n(size = length(ytest), replace = TRUE) %>%
      select(all_of(varnames), y) %>%
      group_by(across(all_of(varnames))) %>%
      summarise(Class1 = mean(y),
                n = n())
    data.heatmap <- data.boot %>%
      select(-n) %>%
      spread(key = varnames[1], value = "Class1") %>%
      column_to_rownames(varnames[2]) %>%
      round(., 3) %>%
      plotHeatmap(size = 3, text.size = text.size, position = "ordered") +
      labs(title = "Validation Data Smoothed", x = varnames[1], y = varnames[2]) +
      myGGplotTheme()
    data.n.heatmap <- data.boot %>%
      select(-Class1) %>%
      spread(key = varnames[1], value = "n") %>%
      column_to_rownames(varnames[2]) %>%
      round(., 3) %>%
      plotHeatmap(size = 3, text.size = text.size, position = "ordered") +
      labs(title = "Validation Data Smoothed", x = varnames[1], y = varnames[2]) +
      myGGplotTheme()
  }
  
  if (identical(col.pal, "temperature")) {
    col.scheme <- scale_color_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = sum(z.range) / 2,
      limit = z.range
    )
    fill.scheme <- scale_fill_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = sum(z.range) / 2,
      limit = z.range
    )
  } else {
    col.scheme <- scale_color_gradientn(colours = col.pal(100), 
                                        limits = z.range)
    fill.scheme <- scale_fill_gradientn(colours = col.pal(100),
                                        limits = z.range)
  }
  epi.heatmap <- suppressMessages(epi.heatmap + col.scheme + fill.scheme)
  noepi.heatmap <- suppressMessages(noepi.heatmap + col.scheme + fill.scheme)
  tr.data.heatmap <- suppressMessages(tr.data.heatmap + col.scheme + fill.scheme)
  data.heatmap <- suppressMessages(data.heatmap + col.scheme + fill.scheme)
  # tr.data.n.heatmap <- suppressMessages(tr.data.n.heatmap + col.scheme + fill.scheme)
  # data.n.heatmap <- suppressMessages(data.n.heatmap + col.scheme + fill.scheme)
  
  plot.combined <- ggarrange(noepi.heatmap, epi.heatmap, tr.data.heatmap, data.heatmap,
                             ncol = 4, nrow = 1, common.legend = T,
                             legend = "bottom")
  if (save) {
    jpeg(file = save.filename, height = 8, width = 25, units = 'in', res = 300)
    print(plot.combined)
    dev.off()
  }
  
  return(list(plot = plot.combined, 
              epi.heatmap = epi.heatmap, noepi.heatmap = noepi.heatmap,
              data.heatmap = data.heatmap, tr.data.heatmap = tr.data.heatmap, 
              data.n.heatmap = data.n.heatmap, tr.data.n.heatmap = tr.data.n.heatmap,
              tree.ls = tree.ls))
}

plotIntHeatmap <- function(irf.fit, X, y, eval.metrics = NULL, ints = NULL,
                           varnames.grp = colnames(X), read.forest = NULL,
                           clust.x = TRUE, clust.y = TRUE, 
                           col.pal = viridis, show.x.text = FALSE,
                           save = FALSE, 
                           save.filename = "interaction_heatmaps.rds") {
  ####### Function Description ######
  #' Plot heatmap of interaction evaluation metrics
  #'
  #' @param irf.fit output of iRF()
  #' @param X feature data matrix for evaluating interactions
  #' @param y response vector for evaluating interactions
  #' @param eval.metrics character vector of metrics to use for evaluating
  #'   predictions; see possible metrics in evalPreds()
  #' @param ints vector of signed interactions to plot. If numeric, ints is
  #'   assumed to correspond to column indices to be plotted for interaction. 
  #'   If character, assumed to be formatted as 'X1+_X2+_X3-_...'; if NULL,
  #'   all interactions from irf.fit will be plotted
  #' @param varnames.grp character vector indicating feature names. By default,
  #'   colnames(X) are used as feature names. Can group features using
  #'   duplicate variable names
  #' @param read.forest output of 
  #'   readForest(rand.forest = rf.fit, x = X, varnames.grp = colnames(X))
  #' @param clust.x logical; whether or not to cluster the samples
  #' @param clust.y logical; whether or not to cluster the interactions
  #' @param col.pal color palette for response surfaces. A function that takes an
  #'   integer input and returns colors to be use in the palette.
  #' @param show.x.text logical; whether or not to show x axis text
  #' @param save logical; whether or not to save plot
  #' @param save.filename name of file/path to save plot
  #######
  
  # set default arguments
  if (all(y %in% 0:1)) {
    binary.rf <- TRUE
    default.pred <- 0
    if (is.null(eval.metrics)) {
      eval.metrics <- c("MAE", "Class")
    }
  } else {
    binary.rf <- FALSE
    default.pred <- mean(y)
    if (is.null(eval.metrics)) {
      eval.metrics <- c("RMSE", "MAE")
    }
  }
  if (!show.x.text) {
    add_theme <- myGGplotTheme() +
      theme(axis.text.x = element_blank())
  } else {
    add_theme <- myGGplotTheme(x_text_angle = TRUE)
  }
  
  # extract interactions
  if (!is.null(ints)) {
    int.df <- irf.fit$interaction %>%
      filter(int %in% ints) %>%
      mutate(int = factor(int, levels = ints))
  } else {
    int.df <- irf.fit$interaction %>%
      mutate(int = fct_inorder(int))
  }
  
  # Read out RF decision paths
  if (is.null(read.forest)) {
    read.forest <- readForest(irf.fit, x = X, varnames.grp = colnames(X),
                              oob.importance = FALSE)
  }
  
  ints <- as.character(int.df$int)
  
  # predict using interactions only
  int.predict.out <- map(ints, function(int) {
    myInteractPredict(x = X, int = int, default.pred = default.pred,
                      read.forest = read.forest, varnames = varnames.grp,
                      return.int.active = TRUE)
  }) %>%
    setNames(ints)
  int.preds <- map_dfc(int.predict.out, "pred")
  int.active <- map_dfc(int.predict.out, "int.active") * 100
  
  # compute prediction error metrics
  int.errs <- map(eval.metrics, function(metric) {
    map_dfc(int.preds, function(preds) {
      if (metric == "Class") {
        evalPreds(y = y, yhat = preds >= 0.5, metric = metric,
                  group = as.factor(1:length(preds))) %>%
          filter(Group != "all") %>%
          arrange(as.numeric(Group)) %>%
          pull(Value)
      } else {
        evalPreds(y = y, yhat = preds, metric = metric,
                  group = as.factor(1:length(preds))) %>%
          filter(Group != "all") %>%
          arrange(as.numeric(Group)) %>%
          pull(Value)
      }
    })
  }) %>%
    setNames(eval.metrics)
  
  active.heatmap <- plotHclustHeatmap(as.data.frame(t(int.active)), x.groups = y,
                                      clust.y = clust.y, clust.x = clust.x) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 50, limit = c(0, 100)) +
    labs(fill = "% Active", x = "Interaction", y = "Sample") +
    add_theme
  
  pred.range <- range(int.preds)
  pred.heatmap <- plotHclustHeatmap(as.data.frame(t(int.preds)), x.groups = y,
                                    clust.x = clust.x, clust.y = clust.y) +
    scale_fill_gradientn(colours = col.pal(100), limits = pred.range) +
    scale_color_gradientn(colours = col.pal(100), limits = pred.range) +
    labs(fill = "Prediction", x = "Interaction", y = "Sample") +
    add_theme
  
  err.heatmaps <- map(eval.metrics, function(metric) {
    int.err <- int.errs[[metric]]
    err.range <- range(int.err)
    plotHclustHeatmap(as.data.frame(t(int.err)), x.groups = y,
                      clust.x = clust.x, clust.y = clust.y) +
      scale_fill_gradientn(colours = col.pal(100), limits = err.range) +
      scale_color_gradientn(colours = col.pal(100), limits = err.range) +
      labs(fill = metric, x = "Interaction", y = "Sample") +
      add_theme
  }) %>% setNames(eval.metrics)
  
  active.heatmap <- active.heatmap + labs(title = "")
  pred.heatmap <- pred.heatmap + labs(title = "")
  err.heatmaps <- ggarrange(plotlist = err.heatmaps, nrow = 1, ncol = length(err.heatmaps),
                            common.legend = T, legend = "bottom")
  
  int.active.long <- int.active %>%
    rownames_to_column("id") %>%
    gather(key = "int", value = "active", -id)
  int.preds.long <- int.preds %>%
    rownames_to_column("id") %>%
    gather(key = "int", value = "prediction", -id)
  int.errs.long <- map(eval.metrics, function(metric) {
    int.errs[[metric]] %>%
      rownames_to_column("id") %>%
      gather(key = "int", value = "metric", -id) %>%
      setNames(c("id", "int", paste0("error_", metric)))
  }) %>%
    purrr::reduce(left_join, by = c("id", "int"))
  int.eval.df <- int.active.long %>%
    left_join(int.preds.long, by = c("id", "int")) %>%
    left_join(int.errs.long, by = c("id", "int")) %>%
    mutate_if(is.character, as.factor)
  
  out <- list(active.heatmap = active.heatmap, 
              pred.heatmap = pred.heatmap,
              err.heatmaps = err.heatmaps,
              int.eval.df = int.eval.df,
              int.active.df = int.active,
              int.preds.df = int.preds,
              int.errs.ls = int.errs)
  
  if (save) saveRDS(out, file = save.filename)
  
  return(out)
}

plotTree <- function(rf.fit, tree.id) {
  ####### Function Description ######
  #' Plot tree from random forest ensemble
  #'
  #' @param rf.fit object of class ranger or randomForest
  #' @param tree.id idx of which tree to plot from ensemble
  #######
  
  if ("ranger" %in% class(rf.fit)) {
    tree_info <- treeInfo(rf.fit, tree.id)
  } else if ("randomForest" %in% class(rf.fit)) {
    tree_info <- getTree(rf.fit, tree.id, labelVar = TRUE) %>%
      rownames_to_column("nodeID") %>%
      rename("leftChild" = "left daughter",
             "rightChild" = "right daughter",
             "splitvarName" = "split var",
             "splitval" = "split point") %>%
      mutate(splitval = ifelse(is.na(prediction), splitval, NA),
             leftChild = ifelse(is.na(prediction), leftChild, NA),
             rightChild = ifelse(is.na(prediction), rightChild, NA))
  } else {
    stop("rf.fit must be of class ranger or randomForest.")
  }
  
  graph_df <- data.frame(from = rep(tree_info$nodeID, 2),
                         to = c(tree_info$leftChild, tree_info$rightChild))
  graph <- graph_from_data_frame(graph_df) %>%
    delete_vertices("NA")
  
  # set node labels
  V(graph)$node_label <- as.character(tree_info$splitvarName)
  V(graph)$leaf_label <- as.character(tree_info$prediction)
  V(graph)$split <- as.character(round(tree_info$splitval, 2))
  
  # set fill color
  if ("ranger" %in% class(rf.fit)) {
    if (rf.fit$treetype == "Classification") {
      fill_labels <- as.factor(V(graph)$leaf_label)
    } else {
      fill_labels <- V(graph)$leaf_label
    }
  } else if ("randomForest" %in% class(rf.fit)) {
    if (rf.fit$type == "classification") {
      fill_labels <- as.factor(V(graph)$leaf_label)
    } else {
      fill_labels <- V(graph)$leaf_label
    }
  }
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), 
                    vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label),
                    na.rm = TRUE, repel = TRUE, 
                    colour = "white", fontface = "bold", show.legend = FALSE) +
    theme_void() +
    myGGplotFill(fill = fill_labels)
  return(plot)
}

plotLocalStabilityRF <- function(rf.fit, X, y = NULL, 
                                 features = NULL, ints = NULL,
                                 feature.groups = NULL, first.only = FALSE,
                                 oob = FALSE, return.pval = FALSE, nperm = 1e4,
                                 point.size = 1, heights = c(1, 4), ...) {
  #### Function Description ####
  # function to evaluate and plot local feature/int stability in RF
  # 
  # input:
  # - rf.fit = output of ranger() or of class "ranger"
  # - X = data frame of sample design matrix to evaluate local feature staiblity
  # - y = vector of response; only used (and required) if return.pval = TRUE
  # - features = vector of features to evaluate stability for
  # - feature.groups = data frame of feature to (superfeature) group mapping with columns "feature" and "group"
  # - first.only = logical; whether or not to only include first appearance of feature in tree
  # - oob = logical; whether or not to use oob samples if X is the training data
  # - nperm = number of permutations; used only if return.pval = TRUE
  # - point.size = size of point in pvalue plot
  # - heights = vector of heights for plot_layout
  # - ... = additional arguments to plotHclustHeatmap()
  # 
  # output: list of 5:
  # - stab_df = data frame of size n x p with local RF feature stability scores
  # - pval_df = data frame of size p x 1 with permutation p-values
  # - stab_plt = heatmap of local feature stability scores
  # - pval_plt = bar plot of permutation p-values
  # - plot = stab_plt + pval_plt combined
  #############################
  
  if (!identical(features, "None")) {
    stab_df <- localFeatureStabilityRF(rf.fit = rf.fit,
                                       X = X, 
                                       features = features,
                                       feature.groups = feature.groups,
                                       first.only = first.only,
                                       oob = oob)
  }
  
  if (!is.null(ints)) {
    int_stab_df <- localIntStabilityRF(rf.fit = rf.fit,
                                       X = X,
                                       ints = ints,
                                       feature.groups = feature.groups,
                                       first.only = first.only,
                                       oob = oob)
    if (!identical(features, "None")) {
      stab_df <- cbind(stab_df, int_stab_df)
    } else {
      stab_df <- int_stab_df
    }
  }
  
  stab_df <- as.data.frame(stab_df)
  
  if (is.factor(y)) {
    stab_plt <- plotHclustHeatmap(X = stab_df, y.groups = y, ...) +
      labs(x = "Feature", y = "Sample", fill = "Signed\nFeature\nStability")
  } else {
    stab_plt <- plotHclustHeatmap(X = stab_df, ...) +
      labs(x = "Feature", y = "Sample", fill = "Signed\nFeature\nStability")
  }
  
  if (return.pval & is.factor(y) & (nlevels(y) == 2)) {
    test_features <- colnames(stab_df)
    names(test_features) <- test_features
    y_binary <- as.numeric(y) - 1
    pval_df <- map_dfr(test_features,
                       ~runPermutationTest(stab_df = stab_df, y = y_binary,
                                           feature = .x, nperm = nperm)$pval,
                       .id = "Feature")
    
    xtest_labs <- ggplot_build(stab_plt)$layout$panel_params[[1]]$x$get_labels()
    pval_plt <- pval_df %>%
      gather(key = "Feature", value = "Value") %>%
      mutate(Feature = factor(Feature, levels = xtest_labs),
             Value = -log10(Value + 1 / (nperm * 10))) %>%
      ggplot() +
      aes(x = Feature, y = Value) +
      geom_point(size = point.size) +
      labs(y = "-log10(p-value)") +
      myGGplotTheme() +
      theme(axis.line.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    plot_combined <- pval_plt + stab_plt + 
      patchwork::plot_layout(nrow = 2, ncol = 1, heights = heights)
    
    pval_plt <- pval_df %>%
      gather(key = "Feature", value = "Value") %>%
      arrange(desc(Value)) %>%
      mutate(Feature = fct_inorder(Feature),
             Value = -log10(Value + 1 / (nperm * 10))) %>%
      plotBarplot(x.str = "Value", y.str = "Feature", stat = "identity") +
      labs(x = "-log10(p-value)")
    
  } else {
    if (return.pval) {
      warning("Permutation test has not yet been implemented for response that are not binary.")
    }
    pval_df <- NULL
    pval_plt <- NULL
    plot_combined <- stab_plt
  }
    
  return(list(stab_df = stab_df, pval_df = pval_df, 
              stab_plt = stab_plt, pval_plt = pval_plt,
              plot = plot_combined))
}


plotFeatureImp <- function(rf.fit, min.imp.thr = NULL, sort = TRUE) {
  #### Function Description ####
  # function to plot feature importances
  # 
  # input:
  # - rf.fit = output of ranger() or of class "ranger"
  # - min.imp.thr = minimum importance threshold to filter variables for plot
  # - sort = logical; whether or not to sort vimp_df by importance
  # 
  # output: list of two:
  # - vimp_df = data frame of variable importance
  # - plot = bar plot of variable importance
  #############################
  
  vimp_df <- data.frame(Feature = names(rf.fit$variable.importance),
                        Importance = rf.fit$variable.importance)
  if (sort) {
    vimp_df <- vimp_df %>%
      arrange(desc(Importance))
  }
  if (!is.null(min.imp.thr)) {
    plt_df <- vimp_df %>%
      filter(Importance > min.imp.thr)
  } else {
    plt_df <- vimp_df
  }
  
  vimp_name <- paste(capitalize(rf.fit$importance.mode), "Importance")
  
  plt <- plt_df %>%
    arrange(desc(Importance)) %>%
    mutate(Feature = fct_inorder(Feature)) %>%
    plotBarplot(x.str = "Feature", y.str = "Importance", stat = "identity") +
    labs(y = vimp_name)
  
  colnames(vimp_df)[2] <- vimp_name
  
  return(list(vimp_df = vimp_df, plot = plt))
}


plotFeatureSplits <- function(rf.fit, X = NULL, y = NULL, features = NULL) {
  #### Function Description ####
  # function to plot feature splits in random forest
  # 
  # input:
  # - rf.fit = output of ranger() or of class "ranger"
  # - X = data matrix; if included, also plot distribution of X values
  # - y = response vector; if included and categorical/discrete, also plot
  #     distribution of X values for each y group
  # - features = vector of features to keep for plotting; if NULL, keep all
  # 
  # output: plof of feature splits distribution for each feature specified
  #############################
  
  ntrees <- rf.fit$num.trees
  tree_ids <- 1:ntrees
  names(tree_ids) <- tree_ids
  
  tree_infos <- map_dfr(1:ntrees, ~treeInfo(rf.fit, .x), .id = "Tree")
  
  if (is.null(features)) {
    features <- rf.fit$forest$independent.variable.names
  }
  
  if (is.null(X)) {
    plt <- tree_infos %>%
      filter(splitvarName %in% features) %>%
      mutate(splitvarName = factor(splitvarName, levels = features)) %>%
      plotDensity(x.str = "splitval") +
      facet_wrap(~ splitvarName) +
      labs(x = "RF Split Values")
  } else {
    X <- as.data.frame(X) %>%
      mutate_if(is.factor, as.numeric)
    if (!is.null(y)) {
      if (is.character(y) | (length(unique(y)) <= 10)) {
        y <- as.factor(y)
      }
      X_long <- X %>%
        select(features) %>%
        cbind(., y = y) %>%
        gather(key = "splitvarName", value = "splitval", -y)
      tree_infos <- bind_rows(tree_infos, X_long) %>%
        mutate(fill = as.factor(ifelse(!is.na(terminal),
                                       "RF Splits", as.character(y))))
    } else {
      X_long <- X %>%
        gather(key = "splitvarName", value = "splitval")
      tree_infos <- bind_rows(tree_infos, X_long) %>%
        mutate(fill = as.factor(ifelse(!is.na(terminal),
                                       "RF Splits", "X Values")))
    }
    
    plt <- tree_infos %>%
      filter(splitvarName %in% features) %>%
      mutate(splitvarName = factor(splitvarName, levels = features)) %>%
      plotDensity(x.str = "splitval", fill.str = "fill") +
      facet_wrap(~ splitvarName, scales = "free") +
      labs(fill = "", x = "Values")
  }
  
  return(plt)
}


plotIntCorrelation <- function() {
  
}

plotIntGraph <- function() {
  
}

