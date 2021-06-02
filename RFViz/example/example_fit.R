library(iRF)
library(tidyverse)
set.seed(1)
source("../functions/epitree_utilities.R", chdir = T)

#### Load Data -----------------------------------------------------------------
load("enhancer.Rdata")
rit.params <- list(depth = 5, nchild = 2, ntree = 500, 
                   class.id = 1, class.cut = NULL)

# varnames.all$Predictor_collapsed <- str_replace(
#   string = varnames.all$Predictor_collapsed, 
#   pattern = "_", 
#   replacement = ""
# )

colnames(X) <- str_replace(colnames(X), pattern = "_", replacement = "")
X <- as.data.frame(X)
Y <- as.factor(Y)

Xtrain <- X[train.id, ]
ytrain <- Y[train.id]
Xtest <- X[test.id, ]
ytest <- Y[test.id]
saveRDS(Xtrain, "Xtrain.rds")
saveRDS(Xtest, "Xtest.rds")
saveRDS(ytrain, "ytrain.rds")
saveRDS(ytest, "ytest.rds")

#### Run RF --------------------------------------------------------------------
rf_fit <- ranger(x = Xtrain, y = ytrain, 
                 importance = "impurity", keep.inbag = T)
saveRDS(rf_fit, "rf_fit.rds")

#### Run iRF -------------------------------------------------------------------
irf_fit <-  iRF(x = Xtrain,
                y = as.factor(ytrain), 
                n.iter = 3,
                iter.return = 1:3, 
                int.return = 3, 
                signed = TRUE, 
                rit.param = rit.params,
                # varnames.grp = varnames.all$Predictor_collapsed, 
                # n.core = n.cores, 
                n.bootstrap = 30, 
                type = "ranger")
saveRDS(irf_fit, "irf_fit.rds")

#### Run epiTree ---------------------------------------------------------------
ints <- irf_fit$interaction %>%
  filter(stability >= 0.5) %>%
  pull(int)
epitree_fit <- runEpitree(Xtrain = Xtrain, 
                          ytrain = as.numeric(as.character(ytrain)), 
                          Xtest = Xtest, 
                          ytest = as.numeric(as.character(ytest)), 
                          ints = ints, B = 100)
saveRDS(epitree_fit, "epitree_fit.rds")
