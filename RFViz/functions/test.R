library(iRF)
library(tidyverse)
library(viridis)

source('rf_plotting_functions.R')

set.seed(47)
n <- 1000
p <- 50

x <- matrix(rnorm(n * p), nrow=n, ncol=p)
colnames(x) <- str_c('X', 1:ncol(x))
y <- as.numeric(x[,1] > 0 & x[,2] > 0)

# Random swapping noise
id.swap <- sample(n, 100)
y[id.swap] <- 1 - y[id.swap]

if (!file.exists('fit.Rdata')) {
  fit <- iRF(x = x, y = as.factor(y), n.iter = 2, type = 'ranger', 
             int.return = 2)
  read.forest <- readForest(fit$rf.list, x = x, varnames.grp = colnames(x),
                            oob.importance = FALSE)
  save(file = 'fit.Rdata', fit, read.forest)
} else {
  load('fit.Rdata')
}

# Generate response surface for select rule list
plotIntSurface(X = x, int = 'X1+_X2+', 
               read.forest = read.forest, y = y, z.range = 0:1)
plotIntSurface(X = x, int = 'X1+_X2+', 
               read.forest = read.forest, y = y, z.range = 0:1, type = "ggplot")

epi.out <- runEpitree(Xtrain = x, ytrain = y, Xtest = x, ytest = y, 
                      ints = c("X1+_X2+", "X1+_X3-", "X3+_X2-"))
epi.plt <- plotEpitreeResults(epi.out)
epi.plt$plot
epi.int.plt <- plotEpitreeInt(epi.out, int = "X1+_X2+", z.range = c(0, 1),
                              col.pal = "temperature")
epi.int.plt$plot
epi.int.plt <- plotEpitreeInt(epi.out, int = "X3+_X2-", z.range = c(0, 1),
                              col.pal = "temperature")
epi.int.plt$plot

int.heatmap <- plotIntHeatmap(irf.fit = fit, X = x, y = y, 
                              read.forest = read.forest, 
                              clust.x = TRUE, clust.y = TRUE, show.x.text = F)
int.heatmap$int.eval.df %>%
  filter(int == "X1+_X2+") %>%
  plotPairs(columns = c("active", "prediction", "error_MAE"), 
            color = as.factor(y))

out <- plotLocalStabilityRF(irf.fit$rf.list, X = x)
str(out$stab_df)
out$plot +
  myGGplotTheme(x_text_angle = T)

out <- plotLocalStabilityRF(irf.fit$rf.list, X = x, y = y,
                            ints = irf.fit$interaction$int[1:2], return.pval = T)
out$plot +
  myGGplotTheme(x_text_angle = T)

plotTree(irf.fit$rf.list, 100)

plotFeatureSplits(rf.fit = irf.fit$rf.list, X = x, y = y)

#### Continuous response ####
set.seed(47)
x <- matrix(rnorm(n * p), nrow=n, ncol=p)
colnames(x) <- str_c('X', 1:ncol(x))
y <- rnorm(n, mean = 2) * as.numeric(x[,1] > 0 & x[,2] > 0)

if (!file.exists('fit_continuous.Rdata')) {
  fit <- iRF(x=x, y=as.factor(y), n.iter=2, type='ranger')
  read.forest <- readForest(fit$rf.list, x=x, oob.importance=FALSE)
  save(file='fit_continuous.Rdata', fit, read.forest)
} else {
  load('fit_continuous.Rdata')
}

# Generate response surface for select rule list
plotIntSurface(X = x, int = 'X1+_X2+', 
               read.forest = read.forest, y = y, z.range = c(min(y), max(y)))
plotIntSurface(X = x, int = 'X1+_X2+', 
               read.forest = read.forest, y = y, z.range = c(min(y), max(y)),
               type = "ggplot")

