# Compare ITR and ITE of different meta learner on the dataset "ist" using
# non-parametric models

#### Initialization ####

# setwd("my work directory")
rm(list = ls())
graphics.off()
set.seed(123)


# library import

library(rms) # cubic spline
library(magrittr) # pipeline
library(tidyverse) # data.frame manipulation
library(randomForest) # radom forest model


# import dataset

ist <- read.csv("C:/Users/etien/Documents/cours/Benchmark ITR/ist_partitioned.csv")
# ist <- read.csv("C:\\Users\\Florie BRION-BOUVIER\\Documents\\These\\Benchmark_ITR\\ist_partitioned.csv")
df <- ist


#### Pre Processing ####

# Changing the reference label of some categorical variables to match Collins
df <- df %>% mutate(Conscious = relevel(factor(Conscious), "Fully alert"),
                    StrokeType = relevel(factor(StrokeType), "PACS"),
                    region = relevel(factor(region), "Europe"))

# Train and Test set 

train = filter(df, group == "Test")
test = filter(df, group == "Test")
train.rf <- filter(df, group == "Train")
test.rf <- filter(df, group == "Test")


#### S Learner ####

fit.rf <- randomForest(deathdep ~ (SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region)*aspirin,
                       data = train.rf,
                       ntree = 100)


#ITR w/ test
test0 <- test.rf
test0$aspirin <- 0
test1 <- test.rf
test1$aspirin <- 1

pred0.test <- predict(fit.rf, newdata=test0, type="response")
pred1.test <- predict(fit.rf, newdata=test1, type="response")

test.rf$ITE.sl <- pred1.test-pred0.test
test.rf$ITR.sl <- ifelse(test.rf$ITE.sl<0,1,0)

mean(test.rf$ITR.sl) #ITR=1 => 56.2%

test.rf.eq.sl <- test.rf[test.rf$aspirin == test.rf$ITR.sl, ]
v.test.rf.sl <- mean(test.rf.eq.sl$deathdep)  #0.65

#ITR w/ train
train0 <- train.rf
train0$aspirin <- 0
train1 <- train.rf
train1$aspirin <- 1

pred0.train <- predict(fit.rf, newdata=train0, type="response")
pred1.train <- predict(fit.rf, newdata=train1, type="response")

train.rf$ITE.sl <- pred1.train - pred0.train
train.rf$ITR.sl <- ifelse(train.rf$ITE.sl<0, 1, 0)

mean(train.rf$ITR.sl) #ITR=1 => 53.0%

train.rf.eq.sl <- train.rf[train.rf$aspirin == train.rf$ITR.sl, ]
v.train.rf.sl <- mean(train.rf.eq.sl$deathdep)  #0.28


#### T Learner ####

train0 <- filter(train.rf, aspirin == 0)
train1 <- filter(train.rf, aspirin == 1)

fit.rf0 <- randomForest(deathdep ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                        data = train0,
                        ntree = 100)

fit.rf1 <- randomForest(deathdep ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                        data = train1,
                        ntree = 100)

#ITR w/ test
pred0.test <- predict(fit.rf0, newdata=test.rf, type="response")
pred1.test <- predict(fit.rf1, newdata=test.rf, type="response")

test.rf$ITE.tl <- pred1.test - pred0.test
test.rf$ITR.tl <- ifelse(test.rf$ITE.tl<0, 1, 0)

mean(test.rf$ITR.tl) #ITR=1 => 54.5%

test.rf.eq.tl <- test.rf[test.rf$aspirin == test.rf$ITR.tl, ]
v.test.rf.tl <- mean(test.rf.eq.tl$deathdep)  #0.66

#ITR w/ train
pred0.train <- predict(fit.rf0, newdata=train.rf, type="response")
pred1.train <- predict(fit.rf1, newdata=train.rf, type="response")

train.rf$ITE.tl <- pred1.train - pred0.train
train.rf$ITR.tl <- ifelse(train.rf$ITE.tl<0, 1, 0)

mean(train.rf$ITR.tl) #ITR=1 => 51.3%

train.rf.eq.tl <- train.rf[train.rf$aspirin == train.rf$ITR.tl, ]
v.train.rf.tl <- mean(train.rf.eq.tl$deathdep)  #0.21


#### X Learner ####

train0 <- filter(train.rf, aspirin == 0)
train1 <- filter(train.rf, aspirin == 1)

fit.rf0 <- randomForest(deathdep ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                        data = train0,
                        ntree = 100)

fit.rf1 <- randomForest(deathdep ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                        data = train1,
                        ntree = 100)

mu0 <- predict(fit.rf0, newdata=train1, type="response")
mu1 <- predict(fit.rf1, newdata=train0, type="response")

y0 <- train0$deathdep
y1 <- train1$deathdep

D1 <- y1  - mu0
D0 <- mu1 - y0

tau0 <- randomForest(D0 ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                     data = train0,
                     ntree = 100)

tau1 <- randomForest(D1~SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                     data = train1,
                     ntree = 100)

#ITR w/ test
pred0.test <- predict(tau0, newdata=test.rf, type="response")
pred1.test <- predict(tau1, newdata=test.rf, type="response")

g <- 0.5
test.rf$ITE.xl <- g*pred0.test + (1-g)*pred1.test
test.rf$ITR.xl <- ifelse(test.rf$ITE.xl<0, 1, 0)

mean(test.rf$ITR.xl) #ITR=1 => 58.6%

test.rf.eq.xl <- test.rf[test.rf$aspirin == test.rf$ITR.xl, ]
v.test.rf.xl <- mean(test.rf.eq.tl$deathdep)  # 0.66

#ITR w/ train
pred0.train <- predict(fit.rf0, newdata=train.rf, type="response")
pred1.train <- predict(fit.rf1, newdata=train.rf, type="response")

train.rf$ITE.xl <- pred1.train - pred0.train
train.rf$ITR.xl <- ifelse(train.rf$ITE.xl<0, 1, 0)

mean(train.rf$ITR.xl) #ITR=1 => 51.4%

train.rf.eq.xl <- train.rf[train.rf$aspirin == train.rf$ITR.xl, ]
v.train.rf.xl <- mean(train.rf.eq.xl$deathdep)  #0.21

#### Plots ####

### heatmap

heatmap = function(data, itr1, itr2, covariates)
{
  data = cbind(data[covariates], itr1==itr2)
  colnames(data) = c(covariates, 'concord')
  lapply(
    covariates,
    function(c1)
    {
      lapply(
        covariates,
        function(c2)
        {
          df = aggregate.data.frame(
            x = data['concord'],
            by = data[c(c1, c2)],
            FUN = mean)
          df[ ,1] = paste(c1,"=",df[ ,1])
          df[ ,2] = paste(c2,"=",df[ ,2])
          names(df)[1:2] = c("cov1", "cov2")
          return(df)
        })
    }) %>%
    do.call("c",.) %>%
    do.call("rbind",.) %>%
    ggplot(aes(x = cov1, y = cov2, fill = concord)) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}


covariates = select(test, -c("AGE", "RDELAY", "SBP", "group")) %>% names
heatmap(test, test.rf$ITR.tl, test.rf$ITR.xl, covariates)

mean(test.rf$ITR.tl == test.rf$ITR.xl)
mean(test.rf$ITR.xl)
###Scatter plot

ite_scatter_plot = function(data=NULL, ite1, ite2, label=c("ite1", "ite2"),
                            title = "ITE Distribution") {
  if (!is.null(data) & typeof(data) != "list" )
    stop("'data' must be a list")
  if (is.null(data)) data = list(ite1, ite2) %>% `names<-`(label)
  if (any(sapply(data, is.character)))
    stop("if 'data' is not given then 'ite1' and 'ite2' must be numeric")
  if (is.character(c(ite1,ite2))) label = c(ite1, ite2)
  
  ite1 = data[[label[1]]]
  ite2 = data[[label[2]]]
  
  window_size = sapply(data, range) %>%
    abs %>% max %>% c(-., .) * 1.05
  
  ggplot() +
    xlim(window_size) +
    ylim(window_size) +
    geom_point(aes(x=ite1, y=ite2), size=1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlab(label[1]) +
    ylab(label[2]) +
    ggtitle(label = title)
}


ite_scatter_plot(ite1 = test.rf$ITE.xl,
                 ite2 = test.rf$ITE.tl,
                 label = c("X learner", "T learner"))

ite_scatter_plot(ite1 = test.rf$ITE.sl,
                 ite2 = test.rf$ITE.tl,
                 label = c("S learner", "T learner"))


ite_scatter_plot(ite1 = test.rf$ITE.xl,
                 ite2 = test.rf$ITE.sl,
                 label = c("X learner", "S learner"))


#### Crossfited Functions ####

.idx_na_col = function(l)
  # get the index of the columns containing NA values
  apply(
    l, 2, function(col) any(is.na(col)) ) %>%
  unlist %>% which


add_nbr_required_split = function(Balancing_Method) {
  # add the required number of split to compute each nuisance function on each
  # method to Balancing_Method
  lapply(
    Balancing_Method,
    function(met) {
      
      args_name = lapply(met[["nuisance_fct"]],
                         function(fct){
                           fct[["train"]] %>%
                             formalArgs %>%
                             { .[. %in% names(met[["nuisance_fct"]])] } %>%
                             { if(length(.)==0) NULL else . }
                         })
      nbr_required_split = sapply(args_name,
                                  function(name) ifelse(is.null(name),1,NA))
      
      while(any(is.na(nbr_required_split))) {
        
        to_compute = nbr_required_split %>%
          is.na %>%
          nbr_required_split[.] %>%
          names
        
        for (name in to_compute) {
          nbr_required_split[name] = args_name[[name]] %>%
            nbr_required_split[.] %>%
            sum(1)
        }
      }
      
      for (fct_name in names(met[["nuisance_fct"]])) {
        met[["nuisance_fct"]][[fct_name]]["nbr required split"] = nbr_required_split[fct_name]
      }
      return(met)
    }
  )
}



simple_crossfit = function(X_tr, Y_tr, Z_tr, X_te, Y_te, Z_te, Balancing_Method, nbr_iter, nbr_split) {
  n = nrow(X_tr) # number of observations in train dataset
  p = ncol(X_tr) # number of covariates
  
  Balancing_Method %<>% add_nbr_required_split
  
  test_dataset = list(
    "X" = X_te,
    "Y" = Y_te,
    "Z" = Z_te)
  
  # split the dataset "nbr_iter" times
  
  replicate(
    n = nbr_iter,
    simplify = FALSE,
    expr =
    {
      # split the data evenly without overlap
      splited_data = cbind(X_tr, Y_tr, Z_tr) %>%
        # shuffle the rows
        .[sample(n), ] %>%
        # split [X_tr, Y_tr, Z_tr] into "nbr_split" splits
        split( rep_len(1:nbr_split, n) ) %>% 
        # unbind [X_tr, Y_tr, Z_tr]
        lapply( function(data) list(
          "X" = data[, 1:p],
          "Y" = data[, p+1],
          "Z" = data[, p+2])
        )
      
      # for each unique nuisance function, on each split train the nuisance
      # function and use a model fitted on a split to predict the values on the
      # other splits
      
      # get unique nuisance functions
      unique_nuisance = lapply(Balancing_Method, `[[`, "nuisance_fct") %>% 
        # concatenate the nuisance functions
        do.call(c,.) %>% 
        # remove duplicate
        unique %>%
        # order the list
        .[sort.list(sapply(., `[[`, "nbr required split"))]
      
      
      # assign the "correct" name to the function in unique_nuisance
      names(unique_nuisance) = sapply(
        unique_nuisance,
        function(nui) {
          # get the name of the first nuisance function in BalancingMethod
          # matching nui
          for(met in Balancing_Method) {
            name = match(list(nui), met[["nuisance_fct"]]) %>%
              names(met[["nuisance_fct"]])[.]
            if(!is.na(name)) return(name)
          }
        })
      
      
      # train the models on each split and predict the values on the other splits
      for (nui_idx in seq_along(unique_nuisance)) {
        
        nui = unique_nuisance[[nui_idx]]
        nui_name = names(unique_nuisance)[nui_idx]
        
        unique_nuisance[[nui_name]] = lapply(
          1:nbr_split,
          function(i) {
            out = list("trained.model" = NULL,
                       "predicted.value" = NULL)
            
            # get the index of the split used to train the required nuisance
            # function
            idx_required_nui = names(unique_nuisance) %>%
              .[. %in% formalArgs(nui[["train"]])] %>%
              unique_nuisance[.] %>%
              sapply(`[[`, "nbr required split") %>%
              c(0,.) %>% cumsum %>% .[-length(.)] %>%
              { i - . - 1 } %>%
              { (.-1) %% nbr_split + 1}
            
            # get the predicted values on the split i of the required nuisance
            # functions trained on the split idx_required_nui
            required_nui_input =  lapply(
              seq_along(idx_required_nui),
              function(j) {
                # get the name of the required nuisance function
                required_nui_name = names(unique_nuisance) %>%
                { .[. %in% formalArgs(nui[["train"]])][j] }
                
                # get the split on which the required nuisance function was
                # trained
                train_split = idx_required_nui[j]
                
                # get the prediction on the split i
                unique_nuisance[[required_nui_name]][[train_split]][["predicted.value"]][[i]] %>%
                  list %>%
                  `names<-`(required_nui_name)
              }) %>%
              do.call(c, .)
            
            
            # train the nuisance function on the split i
            out[["trained.model"]] = splited_data[[i]] %>%
              c(required_nui_input) %>%
              # remove unused args
              .[names(.) %in% formalArgs(nui[["train"]])] %>%
              { try(do.call(nui[["train"]], .)) } %>%
              # return NA if "nui" fail
              { if (any(class(.) == "try-error")) NA else . }
            
            # predicted the values on the other splits and test_dataset
            out[["predicted.value"]] = lapply(
              1:(nbr_split+1),
              function(j) {
                if (j==i) return(NULL)
                (if (j == nbr_split+1) test_dataset else splited_data[[j]]) %>%
                  c(out["trained.model"]) %>%
                  # remove unused args
                  .[names(.) %in% formalArgs(nui[["predict"]])] %>%
                  # preidict the values in the current split
                  { try(do.call(nui[["predict"]], .)) } %>%
                  # return NA if "mnui$predict" fail
                  { if (any(class(.) == "try-error")) NA else . }
              }) %>%
              # rename the predictions
              `names<-`(c(rep("",nbr_split),"test_dataset"))
            
            return(out)
          }) %>%
          c(nui)
      }
      
      
      # crossfit each method
      lapply(
        Balancing_Method,
        function(met) {
          output = lapply(
            1:nbr_split,
            function(split_idx) {
              
              # determine the index of the splits used to train
              # the nuisance functions for the current method
              nui_split_idx = names(met[["nuisance_fct"]]) %>%
                .[. %in% formalArgs(met[["method"]])] %>%
                unique_nuisance[.] %>%
                sapply(`[[`, "nbr required split") %>%
                c(0,.) %>% cumsum %>% .[-length(.)] %>%
                { split_idx - . - 1 } %>%
                { (.-1) %% nbr_split + 1}
              
              
              # create a list containing the arguments needed to compute
              # met[["method"]]
              input_nui = lapply(
                seq_along(nui_split_idx), 
                function(i) {
                  names(met[["nuisance_fct"]]) %>%
                    # get the names of the nuisance function called in met[["method"]]
                    .[. %in% formalArgs(met[["method"]])] %>%
                    # get the nuisance function
                    unique_nuisance[.] %>%
                    # get the predicted values of the i-th nuisance function
                    { .[[i]][[nui_split_idx[i]]][["predicted.value"]][["test_dataset"]] }
                }) %>%
                # rename according to the nuisance function name
                `names<-`(names(met[["nuisance_fct"]]) %>% .[. %in% formalArgs(met[["method"]])])
              
              
              c(input_nui, test_dataset) %>%
                # removed unused args
                .[names(.) %in% formalArgs(met[["method"]])] %>%
                { try(do.call(met[["method"]], .)) } %>%
                # return NA if "met$method" fail
                { if (any(class(.) == "try-error")) NA else . } %>%
                { if (any(is.na(.))) rep(NA, nrow(test_dataset[["X"]])) else . }
            }) %>%
            do.call(cbind, .) %>%
            # compute the mean for each method
            apply(1, mean)
        })
    }) %>%
    {
      # concatenate the result and set the names
      temp = lapply(
        names(.[[1]]),
        function(name) lapply(., `[[`,name) %>% do.call(cbind,.)
      )
      `names<-`(temp, names(.[[1]]))
    }
}


crossfit = function(X_tr, Y_tr, Z_tr, X_te, Y_te, Z_te, Balancing_Method,
                    default_nbr_iter=100, default_nbr_fail=NULL) {
  
  # set the default value if not specified
  for (i in seq_along(Balancing_Method)) {
    # default for nbr_iter
    if (is.null(Balancing_Method[[i]]$nbr_iter)) {
      Balancing_Method[[i]]$nbr_iter = default_nbr_iter
    }
    
    # default for max_fail
    if (is.null(Balancing_Method[[i]]$max_fail)) {
      Balancing_Method[[i]]$max_fail = ifelse(
        is.null(default_nbr_fail),
        Balancing_Method[[i]]$nbr_iter,
        default_nbr_fail
      )
    }
    
    # default for nbr_split
    if (is.null(Balancing_Method[[i]]$nbr_split)) {
      Balancing_Method[[i]]$nuisance_fct %>% length
    }
  }
  
  
  # sort method by "nbr_split"
  
  Sorted_Balancing_Method = Balancing_Method %>%
    split(x = .,
          f = sapply(., `[[`, "nbr_split")
    )
  
  
  # cross-fit the methods nbr_iter time
  
  lapply(
    Sorted_Balancing_Method,
    function(Bal_Met)
    {
      max_fail = sapply(Bal_Met, `[[`, "max_fail")
      nbr_iter = sapply(Bal_Met, `[[`, "nbr_iter")
      nbr_split = Bal_Met[[1]]$nbr_split
      
      # crossfit the methods to get the right format for output
      output = simple_crossfit(X_tr = X_tr, Y_tr = Y_tr, Z_tr = Z_tr,
                               X_te = X_te, Y_te = Y_te, Z_te = Z_te,
                               nbr_split = nbr_split,
                               nbr_iter = min(nbr_iter),
                               Balancing_Method = Bal_Met)
      
      # set the desired length for each element of output 
      for (i in seq_along(output)) {
        output[[i]] %<>% cbind(matrix(NA, nrow(X_te), nbr_iter[i]-min(nbr_iter)))
      }
      
      # index for which the methods failed
      idx_na = lapply(output, .idx_na_col)
      
      # number of NA per method
      nbr_na = sapply(idx_na, length)
      
      # number of failed exec per method
      nbr_fail = sapply(idx_na, length) - (nbr_iter - min(nbr_iter))
      
      # index of the method to compute
      idx_met = seq_along(Bal_Met)[(nbr_fail < max_fail) & (nbr_na != 0)]
      while (length(idx_met) > 0)
      {
        # compute method on new split if they failed
        out = simple_crossfit(
          nbr_iter = min(nbr_na[idx_met]),
          X_tr = X_tr, Y_tr = Y_tr, Z_tr = Z_tr,
          X_te = X_te, Y_te = Y_te, Z_te = Z_te,
          nbr_split = nbr_split,
          Balancing_Method = Bal_Met[idx_met])
        
        # save values in output
        for (i in seq_along(out)) {
          j = idx_met[i]
          output[[j]][ , idx_na[[j]][1:min(nbr_na[idx_met])]] = out[[i]]
        }
        
        # update var value
        idx_na = lapply(output, .idx_na_col)
        
        nbr_na = sapply(idx_na, length)
        
        nbr_fail[idx_met] = lapply(out, .idx_na_col) %>%
          sapply(length) + nbr_fail[idx_met]
        
        idx_met = seq_along(Bal_Met)[(nbr_fail < max_fail) & (nbr_na != 0)]
      }
      
      # compute the median
      output %<>% lapply(function(met) apply(met, 1, median))
      return(output)
    }) %>%
    # concatenate and rename results
    {
      temp = do.call(c, .)
      names(temp) = lapply(., names) %>%
        do.call(c,.)
      temp
    }
}

#### Crossfit Parameters ####


## learner
s_learn = function(mu_interac) {
  (mu_interac[["1"]] - mu_interac[["0"]]) %>% matrix
}

t_learn = function(mu0, mu1) {
  (mu1 - mu0) %>%  matrix
}

x_learn = function(tau0, tau1, g=0.5) {
  (g*tau0 + (1-g)*tau1) %>% matrix
}


## surface responses

mu_interac_train = function(X, Y, Z) {
  #no relevant interactions
  fit.rf <- randomForest(Y ~ (SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region) * Z,
                         data = cbind(X, Y, Z),
                         ntree = 100)
  return(fit.rf)
}

mu_interac_pred = function(X, Y, Z, trained.model) {
  mu = list("0" = NA, "1" = NA)
  mu[["0"]] = predict(trained.model, newdata=cbind(X,Y,Z=0), type="response")
  mu[["1"]] = predict(trained.model, newdata=cbind(X,Y,Z=1), type="response")
  return(mu)
}



mu0_train = function(X, Y, Z) {
  fit0 <- randomForest(Y ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                       data = X,
                       subset = Z == 0,
                       ntree = 100)
  return(fit0)
}

mu1_train = function(X, Y, Z) {
  fit1 <- randomForest(Y ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
                       data = X,
                       subset = Z == 1,
                       ntree = 100)
  return(fit1)
}

mu0_pred = function(X, Y, Z, trained.model) {
  predict(trained.model, newdata = X, type = "response")
}

mu1_pred = function(X, Y, Z, trained.model) {
  predict(trained.model, newdata = X, type = "response")
}



tau0_train = function(X, Y, Z, mu1) {
  D0 = mu1 - Y
  tau0 = randomForest(D0 ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
            data = X,
            subset = Z == 0,
            ntree = 100)
  return(tau0)
}

tau1_train = function(X, Y, Z, mu0) {
  D1 = Y - mu0
  tau1 = randomForest(D1 ~ SEX + AGE + SBP + RDELAY + CTbeforeHosp + CTInfarct + AtrialFib + Asp3d + FaceDef + ArmHandDef + LegFootDef + Dysphasia + Hemianopia + VSDisorder + CerebSigns + OtherDeficit + Conscious + StrokeType + region,
            data = X,
            subset = Z == 1,
            ntree = 100)
  return(tau1)
}

tau0_pred = function(X, Y, Z, trained.model) {
  predict(trained.model, newdata = X, type="response")
}

tau1_pred = function(X, Y, Z, trained.model) {
  predict(trained.model, newdata = X, type="response")
}




Balancing_Method = list(
  "S learner" = list(
    
    'method' = s_learn,
    
    'nuisance_fct' = list(
      
      'mu_interac' = list(
        'train' = mu_interac_train,
        'predict' = mu_interac_pred)
    ),
    
    'crossfit' = TRUE,
    'nbr_iter' = 30,
    'nbr_split' = 5,
    'max_fail' = 1000
  ),
  
  "T learner" = list(
    
    'method' = t_learn,
    
    'nuisance_fct' = list(
      
      'mu0' = list(
        'train' = mu0_train,
        'predict' = mu0_pred),
      
      'mu1' = list(
        'train' = mu1_train,
        'predict' = mu1_pred)
    ),
    
    'crossfit' = TRUE,
    'nbr_iter' = 30,
    'nbr_split' = 5,
    'max_fail' = 1000
  ),
  
  "X learner" = list(
    
    'method' = x_learn,
    
    'nuisance_fct' = list(
      
      'mu0' = list(
        'train' = mu0_train,
        'predict' = mu0_pred),
      
      'mu1' = list(
        'train' = mu1_train,
        'predict' = mu1_pred),
      
      'tau0' = list(
        'train' = tau0_train,
        'predict' = tau0_pred),
      
      'tau1' = list(
        'train' = tau1_train,
        'predict' = tau1_pred)
    ),
    
    'crossfit' = TRUE,
    'nbr_iter' = 30,
    'nbr_split' = 5,
    'max_fail' = 1000
  )
)

#### Crossfit Call & Plot ####

system.time(
  output <- crossfit(X_tr = select(train,-c("deathdep", "aspirin", "group")),
                     Y_tr = train[ ,"deathdep"],
                     Z_tr = train[ ,"aspirin"],
                     X_te = select(test,-c("deathdep", "aspirin", "group")),
                     Y_te = test[ ,"deathdep"],
                     Z_te = test[ ,"aspirin"],
                     Balancing_Method)
)


# density plot

ggplot() +
  geom_density(aes(x=output[["S learner"]], color="S learner")) +
  geom_density(aes(x=output[["T learner"]], color="T learner")) +
  geom_density(aes(x=output[["X learner"]], color="X learner")) +
  geom_vline(xintercept = 0) +
  xlab("ITE") +
  ylab("Density") +
  ggtitle("ITE Density")

# ITE scatter plot

ite_scatter_plot(data = output,
                 ite1 = "X learner",
                 ite2 = "T learner")

ite_scatter_plot(data = output,
                 ite1 = "S learner",
                 ite2 = "T learner")

ite_scatter_plot(data = output,
                 ite1 = "X learner",
                 ite2 = "S learner")

df = data.frame('a' = runif(5)<0.5,
           'b' = runif(5)<0.5,
           'c' = runif(5)<0.5)

mutate(df, 'complet' = ifelse(test = a & b & c,yes =  'yes',no = 'no'))


ifelse(test = c(T,T,F,F),
       yes = 1:4,
       no = 5:8)

d
d = TRUE
!d
d == FALSE

test1 = c('a','b','c')

ifelse(test1, 'yes', 'false')

ifelse(T, 1:4, 5:8)
if(T) 1:4 else 5:8

1:3 == c(1,2,3)
seq(1,3,by=1)

2.5:6.4
