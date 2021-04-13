#Lasso loop
elastic <- function(train_data, 
                           folds, 
                           id_col, 
                           featureset = featureset, 
                           language,
                           hold_set,
                           task,
                           demo_set){
  fold_train <- train_data %>% 
    groupdata2::fold(.,
                     k = folds,
                     id_col = id_col)  #new col called .fold

  #for i in nfolds
  feature_list = NULL
  for(i in (1:length(unique(fold_train$.folds)))){
    print(paste("now looping through fold", i, sep = " "))
    lasso_train <- fold_train  %>% 
      filter(.folds != i) %>% 
      select(-c(id_col)) #, .folds
    
    lasso_test <- fold_train  %>% 
      filter(.folds == i)
    
    ###DEFINING VARIABLES
    lasso_data <- lasso_train %>% select(-c(story_type, condition, Gender, 
                                            .folds, country, feature_set))
    x <- model.matrix(Diagnosis ~ ., data = lasso_data) #making a matrix from formula
    y <- lasso_train$Diagnosis #choosing the dependent variable
    
    #lambdas <- seq(0.0001, 1000, length = 65000)
    
    ###LASSO
    set.seed(2021)
    cv_lasso <- cv.glmnet(x, 
                          y, 
                          alpha = 0.5, # Setting alpha between 0 and 1 implements elastic
                          standardize = F,
                          #lambda = lambdas,
                          family = "binomial",
                          type.measure = "auc")
    #parallel = TRUE))
    
    
    ###EXTRACTING COEFFICIENTS
    lasso_coef <- tidy(cv_lasso$glmnet.fit) %>%  
      filter(lambda == cv_lasso$lambda.1se,
             term != "(Intercept)") %>% 
      select(term, estimate) %>%  #maybe it arranges with absolute values already
      mutate(abs = abs(estimate),
             term = str_remove_all(term, "`"), 
             lambda_1se = paste(cv_lasso$lambda.1se),
             test_fold = paste(i)) %>% 
      filter(abs > 0)
    
    # return(cv_lasso) # Do this if you want to get the lambda plot
    
    # #selecting columns to keep in csv file
    lists <- as.data.frame(lasso_coef$term[4:length(lasso_coef$term)])
    names(lists)[1] <- "features"
    lists$fold <- paste(i)
    feature_list <- rbind(feature_list, lists)
  }
  #writing the csvs
  write.csv(fold_train, paste(paste("data",featureset, language, task, sep = "_"), "csv", sep = "."))
  write.csv(feature_list, paste(paste("features", featureset, language, task, sep = "_"),"csv", sep = "."))
}