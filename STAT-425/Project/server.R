library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)
library(e1071)
library(dplyr) # data manipulation
library(MASS)
library(lmtest)
library(leaps)
library(Metrics)
library(lars)
library(caret)
library(party)
library(randomForest)
library(xgboost)



# Visualization
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], price = data_in$price)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


shinyServer(function(input, output) {
  
  output$obs <- renderPrint({
    cat("The number of observations in the training set is ", floor(21613 * input$percent),".")
  })
  
  # EDA part
  output$plot <- renderPlot({
    # regular R coding
    house <- fread("kc_house_data.csv",colClasses = c('condition' = "character", 
                                                                                                 'grade' = 'character'))
    
    # categorical variables
    cat_var<-names(house)[which(sapply(house, is.character))]
    
    # numeric variables
    numeric_var <- names(house)[which(sapply(house, is.numeric))]
    
    # Since there are many zeros in the data, we replace them with NA
    house$waterfront[house$waterfront == 0] <- NA
    house$view[house$view == 0] <- NA
    
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house), floor(nrow(house) * percentage))
    train <- house[sub, ]
    test <- house[-sub, ]
    
    train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
    train_cat <- train[,.SD, .SDcols = cat_var]
    train_cont <- train[,.SD, .SDcols = numeric_var]
    
    # Explore the correlation
    correlations <- cor(subset(train_cont, select = -c(waterfront, view)))
    
    # correlations
    row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
    
    correlations<- correlations[row_indic ,row_indic]
    
    if (input$pType == "A"){
      doPlots(train_cat[, 3:6], fun = plotHist, ii = 1:4, ncol = 2)
    }
    
    if (input$pType == "B"){
      doPlots(train_cont, fun = plotHist, ii = 2:3, ncol = 2)
    }
    
    if (input$pType == "C"){
      corrplot.mixed(correlations, upper = "ellipse",lower = "number")
    }
    
    if (input$pType == "D"){
      p1<-ggplot(train, aes(x = bathrooms, y = price)) + geom_point()+geom_smooth() # bathrooms
      p2<-ggplot(train, aes(x = sqft_living, y = price)) + geom_point()+geom_smooth() # sqft_living
      p3<-ggplot(train, aes(x = sqft_above, y = price)) + geom_point()+geom_smooth() # sqft_above
      p4<-ggplot(train, aes(x = sqft_living15, y = price)) + geom_point()+geom_smooth()# sqft_living15
      multiplot(p1, p2, p3, p4, cols=2)
    }
    
  },height = 500)
  
  
  # print model summary
  output$summary <- renderPrint({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    model1 <- lm(price ~ ., data = train.reg)
    
    model2 <- lm(price ~ bathrooms + sqft_living + waterfront
                 + view + grade + yr_built + sqft_living15 + sqft_lot15,data = train.reg)
    
    if (input$mType == "m1"){
      summary(model1)      
    }
    
    else if (input$mType == "m2"){
      summary(model2)
    }
    
  })
  
  
  # Residual plot part
  output$residuals <- renderPlot({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    model1 <- lm(price ~ ., data = train.reg)
    
    model2 <- lm(price ~ bathrooms + sqft_living + waterfront
                 + view + grade + yr_built + sqft_living15 + sqft_lot15,data = train.reg)
    
    if (input$mType == "m1"){
      layout(matrix(c(1,2,3,4),2,2))
      plot(model1)      
    }
    
    else if (input$mType == "m2"){
      layout(matrix(c(1,2,3,4),2,2))
      plot(model2)
    }
    
  },height = 500)
  
  
  # Prediction part
  output$pred_res <- renderPlot({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    model1 <- lm(price ~ ., data = train.reg)
    
    model2 <- lm(price ~ bathrooms + sqft_living + waterfront
                 + view + grade + yr_built + sqft_living15 + sqft_lot15,data = train.reg)
    test.reg$pred1 <- predict(model1, newdata = test.reg)
    test.reg$pred2 <- predict(model2, newdata = test.reg)
    
    if (input$mType == "m1"){
      ggplot(data = test.reg, aes(x=pred1,y=pred1-price))+
        geom_point(alpha = 0.2, color = "black")+
        geom_smooth(aes(x = pred1, y= pred1-price), color="black")
    }
    
    else if (input$mType == "m2"){
      ggplot(data = test.reg, aes(x=pred2,y=pred2-price))+
        geom_point(alpha = 0.2, color = "black")+
        geom_smooth(aes(x = pred2, y= pred2-price), color="black")
    }
    
  },height = 400)
  
  
  
  output$rmse <- renderText({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    model1 <- lm(price ~ ., data = train.reg)
    
    model2 <- lm(price ~ bathrooms + sqft_living + waterfront
                 + view + grade + yr_built + sqft_living15 + sqft_lot15,data = train.reg)
    test.reg$pred1 <- predict(model1, newdata = test.reg)
    test.reg$pred2 <- predict(model2, newdata = test.reg)
    if (input$mType == "m1"){
      paste0("Root mean square error on test data: ",
             round(rmse(test.reg$price, test.reg$pred1),digits = 4))
    }
    
    else if (input$mType == "m2"){
      paste0("Root mean square error on test data: ",
             round(rmse(test.reg$price, test.reg$pred2),digits = 4))
    }
  })
  
  
  
  # xgBoost part
  output$var_sel <- renderText({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    # transform dummy variables into numeric form
    train.reg$waterfront <- as.integer(train.reg$waterfront)
    test.reg$waterfront <- as.integer(test.reg$waterfront)
    
    # transform the dataset into sparse matrix
    train.tmp <- as.matrix(train.reg, rownames.force=NA)
    test.tmp <- as.matrix(test.reg, rownames.force=NA)
    train.xgb <- as(train.tmp, "sparseMatrix")
    test.xgb <- as(test.tmp, "sparseMatrix")
    
    train.xgb <- xgb.DMatrix(data = train.xgb[,2:15], label = train.xgb[,"price"])
    
    g.xgb <- xgboost(data = train.xgb, max.depth = 4, eta = 1, nthread = 2,
                     nround = 10, objective = "reg:linear")
    
    importance<-xgb.importance(feature_names = names(train.reg)[2:15], 
                               model = g.xgb)
    
    
    ### random forest: just for illustration, we choose to conduct analysis on a small part of data set
    
    # random forest training set
    train.tree <- head(train.reg, n = floor(nrow(house.reg) * input$percent*0.05))
    
    # random forest test set
    test.tree <- tail(train.reg, n = floor(nrow(house.reg) * input$percent*0.05))
    
    obj = randomForest(price~., data = train.tree,importance=TRUE)
    
    obj1 <- cforest(price~.,data=train.tree) #if your predictor variables are highly correlated:
    
    imp <- as.data.frame(importance(obj, type=2))
    imp$feature <- rownames(imp)
    imp <- imp[order(-imp$IncNodePurity),]
    
    imp1<-as.data.frame(varimp(obj1))
    imp1$feature <- rownames(imp1)
    imp1 <- imp1[order(-imp1$`varimp(obj1)`),]
    
    if (input$predType == "m3"){
    paste0("You split the dataset into training (",input$percent*100,"%) and test (",
           (1-input$percent)*100,"%) sets. ","According to the plot, we can say that the most important 
          features in the training set to predict are: ",importance[1,1],", ",
           importance[2,1],", ",importance[3,1],", ",importance[4,1]," and ",
           importance[5,1],".")
    }
    else if (input$predType=="m4" && input$high_corr==FALSE){
      paste0("For illustration purpose, we manually reduce the number of observations in
             both training and test set. The measurement here is Gini importance. 
             In this case, both training and test set contain ",
             floor(nrow(house.reg) * input$percent*0.05)," samples based on your choice. The 
             top 5 important features are: ",imp[1,2],", ",imp[2,2],", ",imp[3,2],", ",imp[4,2]," and ",
             imp[5,2],".")
    }
    else if (input$predType=="m4" && input$high_corr==TRUE){
      paste0( "The measurement here is permutation importance. Please check the side bar for more information.
              The top 5 important features are: ",imp1[1,2],", ",imp1[2,2],", ",imp1[3,2],", ",imp1[4,2]," and ",
             imp1[5,2],".")
    }
  })
  
  
  output$imp <- renderPlot({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    # transform dummy variables into numeric form
    train.reg$waterfront <- as.integer(train.reg$waterfront)
    test.reg$waterfront <- as.integer(test.reg$waterfront)
    
    # transform the dataset into sparse matrix
    train.tmp <- as.matrix(train.reg, rownames.force=NA)
    test.tmp <- as.matrix(test.reg, rownames.force=NA)
    train.xgb <- as(train.tmp, "sparseMatrix")
    test.xgb <- as(test.tmp, "sparseMatrix")
    
    train.xgb <- xgb.DMatrix(data = train.xgb[,2:15], label = train.xgb[,"price"])
    
    g.xgb <- xgboost(data = train.xgb, max.depth = 4, eta = 1, nthread = 2,
                     nround = 10, objective = "reg:linear")
    
    importance<-xgb.importance(feature_names = names(train.reg)[2:15], 
                               model = g.xgb)
    
    
    
    ### random forest: just for illustration, we choose to conduct analysis on a small part of data set
    
    # random forest training set
    train.tree <- head(train.reg, n = floor(nrow(house.reg) * input$percent*0.05))
    
    # random forest test set
    test.tree <- tail(train.reg, n = floor(nrow(house.reg) * input$percent*0.05))
    
    obj = randomForest(price~., data = train.tree,importance=TRUE)
    
    obj1 <- cforest(price~.,data=train.tree) #if your predictor variables are highly correlated:
    
    imp <- as.data.frame(importance(obj, type=2))
    imp$feature <- rownames(imp)
    imp <- imp[order(-imp$IncNodePurity),]
    
    imp1<-as.data.frame(varimp(obj1))
    imp1$feature <- rownames(imp1)
    imp1 <- imp1[order(-imp1$`varimp(obj1)`),]
    
    
    
    # Plot the feature importance matrix
    library(Ckmeans.1d.dp)
    if (input$predType=="m3"){
    xgb.plot.importance(importance[1:10,])
    }
    
    else if (input$predType=="m4" && input$high_corr == FALSE){
        ggplot(imp, aes(x = reorder(feature,IncNodePurity), y = IncNodePurity))+geom_bar(stat="identity",width = 0.3,fill="coral1")+coord_flip()
    }
    
    else if (input$predType=="m4" && input$high_corr == TRUE){
      ggplot(imp1, aes(x = reorder(feature,imp1$`varimp(obj1)`), y = imp1$`varimp(obj1)`))+geom_bar(stat="identity",width = 0.3,fill="coral1")+coord_flip()
    }
    
  })
  
  
  output$plot1 <- renderPlot({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    # transform dummy variables into numeric form
    train.reg$waterfront <- as.integer(train.reg$waterfront)
    test.reg$waterfront <- as.integer(test.reg$waterfront)
    
    # transform the dataset into sparse matrix
    train.tmp <- as.matrix(train.reg, rownames.force=NA)
    test.tmp <- as.matrix(test.reg, rownames.force=NA)
    train.xgb <- as(train.tmp, "sparseMatrix")
    test.xgb <- as(test.tmp, "sparseMatrix")
    
    train.xgb <- xgb.DMatrix(data = train.xgb[,2:15], label = train.xgb[,"price"])
    
    g.xgb <- xgboost(data = train.xgb, max.depth = 4, eta = 1, nthread = 2,
                     nround = 10, objective = "reg:linear")
    
    pred.xgb = predict(g.xgb, data.matrix(test.xgb[,-1]))
    
    
   ### random forest: just for illustration, we choose to conduct analysis on a small part of data set

    # random forest training set
    train.tree <- head(train.reg, n = nrow(house.reg) * input$percent*0.05)

    # random forest test set
    test.tree <- tail(train.reg, n = nrow(house.reg) * input$percent*0.05)

    obj = randomForest(price~., data = train.tree,importance=TRUE)

    obj1 <- cforest(price~.,data=train.tree) #if your predictor variables are highly correlated:

    predtree = predict(obj, newdata = test.tree)
    # rmse(test.tree$price, predtree)

    predtree1 = as.vector(predict(obj1, newdata = test.tree))
    # rmse(test.tree$price, predtree1)
   #  

    if (input$predType=="m3"){
      ggplot(data = as.data.frame(as.matrix(test.xgb)), aes(x=pred.xgb,y=pred.xgb-price))+
        geom_point(alpha = 0.2, color = "black")+
        geom_smooth(aes(x = pred.xgb, y= pred.xgb-price), color="black")
    }
    
    else if (input$predType=="m4" && input$high_corr == FALSE){
      ggplot(data = test.tree, aes(x=predtree,y=predtree-price))+
        geom_point(alpha = 0.2, color = "black")+
        geom_smooth(aes(x = predtree, y= predtree-price), color="black")
    }
    
    else if (input$predType=="m4" && input$high_corr == TRUE){
      ggplot(data = test.tree, aes(x=predtree1,y=predtree1-price))+
        geom_point(alpha = 0.2, color = "black")+
        geom_smooth(aes(x = predtree1, y= predtree1-price), color="black")
    }
  })
  
  
  output$rmse1 <- renderText({
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    # transform dummy variables into numeric form
    train.reg$waterfront <- as.integer(train.reg$waterfront)
    test.reg$waterfront <- as.integer(test.reg$waterfront)
    
    # transform the dataset into sparse matrix
    train.tmp <- as.matrix(train.reg, rownames.force=NA)
    test.tmp <- as.matrix(test.reg, rownames.force=NA)
    train.xgb <- as(train.tmp, "sparseMatrix")
    test.xgb <- as(test.tmp, "sparseMatrix")
    
    train.xgb <- xgb.DMatrix(data = train.xgb[,2:15], label = train.xgb[,"price"])
    
    g.xgb <- xgboost(data = train.xgb, max.depth = 4, eta = 1, nthread = 2,
                     nround = 10, objective = "reg:linear")
    
    pred.xgb = predict(g.xgb, data.matrix(test.xgb[,-1]))
    
    
    ### random forest: just for illustration, we choose to conduct analysis on a small part of data set
    
    # random forest training set
    train.tree <- head(train.reg, n = nrow(house.reg) * input$percent*0.05)
    
    # random forest test set
    test.tree <- tail(train.reg, n = nrow(house.reg) * input$percent*0.05)
    
    obj = randomForest(price~., data = train.tree,importance=TRUE)
    
    obj1 <- cforest(price~.,data=train.tree) #if your predictor variables are highly correlated:
    
    predtree = predict(obj, newdata = test.tree)
    # rmse(test.tree$price, predtree)
    
    predtree1 = as.vector(predict(obj1, newdata = test.tree))
    # rmse(test.tree$price, predtree1)
    
    if (input$predType=="m3"){
      paste0("Root mean square error on test data: ",
             round(rmse(as.data.frame(as.matrix(test.xgb))$price, pred.xgb),digits = 4))
    }
    
    else if (input$predType=="m4" && input$high_corr == FALSE){
      paste0("Root mean square error on test data: ",
             round(rmse(test.tree$price, predtree),digits = 4))
    }
      
    else if (input$predType=="m4" && input$high_corr == TRUE){
        paste0("Root mean square error on test data: ",
               round(rmse(test.tree$price, predtree1),digits = 4))
      }
    
  })
  
  mytext <- eventReactive(input$button,{
    house.reg <- read.csv("kc_house_data.csv")
    
    # remove unnecessary columns
    house.reg <- house.reg[, -c(1:2, 16:19)]
    house.reg$waterfront <- as.factor(house.reg$waterfront)
    
    # Split the data set into train and test set
    percentage <- input$percent
    set.seed(1234)
    sub <- sample(nrow(house.reg), floor(nrow(house.reg) * percentage))
    train.reg <- house.reg[sub, ]
    test.reg <- house.reg[-sub, ]
    
    # for numeric feature with excessive skewness, perform log transformation
    # first get data type for each feature
    numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                      names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                      names(house.reg)[15])
    
    # determine skew for each numeric feature
    skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})
    
    # keep only features that exceed a threshold for skewness
    skewed_feats <- skewed_feats[skewed_feats > 0.75]
    
    # transform excessively skewed features with log(x + 1)
    for(x in names(skewed_feats)) {
      train.reg[[x]] <- log(train.reg[[x]] + 1)
      test.reg[[x]] <- log(test.reg[[x]] + 1)
    }
    
    
    
    model1 <- lm(price ~ ., data = train.reg)
    
    model2 <- lm(price ~ bathrooms + sqft_living + waterfront
                 + view + grade + yr_built + sqft_living15 + sqft_lot15,data = train.reg)
    
    mydata <- data.frame(price = mean(train.reg$price), bedrooms = input$bedroom, bathrooms = input$bathroom, sqft_living = log(input$sqft_living),
                         sqft_lot = mean(train.reg$sqft_lot), floors = mean(train.reg$floors),waterfront = as.factor("0"),
                         view = mean(train.reg$view),condition = mean(train.reg$condition), grade = input$grade, sqft_above = log(input$sqft_living-input$sqft_basement),
                         sqft_basement = ifelse(input$sqft_basement==0,0,log(input$sqft_basement)), yr_built = input$yr_built, sqft_living15 = mean(train.reg$sqft_living15),sqft_lot15=mean(train.reg$sqft_lot15))
    
    
    pred1 <- exp(predict(model1, newdata = mydata))
    pred2 <- exp(predict(model2, newdata = mydata))
    
    
    
    # transform dummy variables into numeric form
    train.reg$waterfront <- as.integer(train.reg$waterfront)
    mydata.xgb <- mydata
    mydata.xgb$waterfront <- as.integer(mydata.xgb$waterfront)
    
    # transform the dataset into sparse matrix
    train.tmp <- as.matrix(train.reg, rownames.force=NA)
    mydata.tmp <- as.matrix(mydata.xgb, rownames.force=NA)
    train.xgb <- as(train.tmp, "sparseMatrix")
    mydata.xgb <- as(mydata.tmp, "sparseMatrix")
    
    train.xgb <- xgb.DMatrix(data = train.xgb[,2:15], label = train.xgb[,"price"])
    
    g.xgb <- xgboost(data = train.xgb, max.depth = 4, eta = 1, nthread = 2,
                     nround = 10, objective = "reg:linear")
    
    pred3 = exp(predict(g.xgb, data.matrix(mydata.xgb[,-1]))[1])
    
    
    ### random forest: just for illustration, we choose to conduct analysis on a small part of data set
    
    # random forest training set
    train.tree <- head(train.reg, n = nrow(house.reg) * input$percent*0.05)
    
    obj = randomForest(price~., data = train.tree,importance=TRUE)
    
    pred4 = exp(predict(obj, newdata = mydata))
    # rmse(test.tree$price, predtree)
    
    # conditional output
    if (input$modelType=="model1"){
      paste0("The house was built in ",input$yr_built,", with total square footage ",input$sqft_living,
             ", and the size of basement is ",input$sqft_basement," square footage. It has ",
             input$bathroom," bathroom(s) and ",input$bedroom," bedroom(s), with building grade ",
             input$grade,". ","You choose to predict based on the full model, the predicted price of your house is $",
             round(pred1,1),".")
    }
    else if (input$modelType=="model2"){
      paste0("The house was built in ",input$yr_built,", with total square footage ",input$sqft_living,
             ", and the size of basement is ",input$sqft_basement," square footage. It has ",
             input$bathroom," bathroom(s) and ",input$bedroom," bedroom(s), with building grade ",
             input$grade,". ","You choose to predict based on the reduced model, the predicted price of your house is $",
             round(pred2,1),".")
    }
    
    else if (input$modelType=="model3"){
      paste0("The house was built in ",input$yr_built,", with total square footage ",input$sqft_living,
             ", and the size of basement is ",input$sqft_basement," square footage. It has ",
             input$bathroom," bathroom(s) and ",input$bedroom," bedroom(s), with building grade ",
             input$grade,". ","You choose to predict based on the XGBoost, the predicted price of your house is $",
             round(pred3,1),".")
    }
    
    else if (input$modelType=="model4"){
      paste0("The house was built in ",input$yr_built,", with total square footage ",input$sqft_living,
             ", and the size of basement is ",input$sqft_basement," square footage. It has ",
             input$bathroom," bathroom(s) and ",input$bedroom," bedroom(s), with building grade ",
             input$grade,". ","You choose to predict based on the random forest, the predicted price of your house is $",
             round(pred4,1),".")
    }
  })
  
  # Make your prediction
  output$prediction <- renderText({
    mytext()
  })
  

})

