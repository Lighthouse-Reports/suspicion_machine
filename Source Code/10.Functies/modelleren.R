## train_model traint alle modellen die gespecificeerd staan in tibble 'modellen'.
## Resultaten wordt opgeslagen in kolom 'model'
## Eerder getrainde modellen worden overgeslagen tenzij overwrite=TRUE

train_model <- function(overwrite=FALSE){
  # kolommen aanmaken voor modellen, traintijd en varimp indien deze kolommen nog niet bestaat.
  if (!"model" %in% names(modellen)) modellen$model <<- vector("list", nrow(modellen))
  if (!"traintijd" %in% names(modellen)) modellen$traintijd <<- vector("integer", nrow(modellen))
  if (!"varimp" %in% names(modellen)) modellen$varimp <<- vector("list", nrow(modellen))
  for(i in 1:nrow(modellen)){
    if(!overwrite & !is.null(modellen$model[[i]])) next #skip to next
    cl <- makeCluster(cores) # create a cluster with 3 cores
    registerDoParallel(cl)
    cat(paste("Bezig met trainen model", i, "van", nrow(modellen), "    Tijd: ", Sys.time(), "\n"))
    data <- eval(parse(text = modellen$traindata[i])) %>%
      dplyr::select(eval(parse(text = modellen$featureselect[i]))) %>%
      dplyr::select(-c(persoon_id, melding_om, type, id))
    t0 <- Sys.time()
    result <- train(target_per~.,
                    method = modellen$methode[i],
                    data = data,
                    tuneGrid = modellen$params[[i]],
                    trControl = eval(parse(text=modellen$trControl[[i]])),
                    metric = modellen$metric[[i]])
    ### traintijd opslaan in kolom `traintijd`
    modellen$traintijd[[i]] <<- as.integer(difftime(Sys.time(), t0, units="mins"))
    ### model opslaan in kolom `model`
    modellen$model[[i]] <<- result
    ### variable importance opslaan in kolom `varimp` 
    imps <- varImp(result, scale = T)$importance
    modellen$varimp[[i]] <<- data.frame(features = row.names(imps), relatieve_belangrijkheid = imps$Overall)
    stopCluster(cl)
    registerDoSEQ()
  }
}


generate_prediction <- function(model, testdata){
  aantal = sum(eval(parse(text=testdata))$target_per == "Ja")
  result = bind_cols(
    predict.train(model, eval(parse(text=testdata)), type="prob"),
    class_pred = predict.train(model, eval(parse(text=testdata)), type="raw"),
    eval(parse(text=testdata)) %>% transmute(class_actual = factor(target_per))
  ) %>%
    arrange(desc(Ja)) %>%
    mutate(class_pred_adjusted = "Nee")
  result$class_pred_adjusted[1:aantal] <- "Ja"
  result %>% 
    mutate(class_pred_adjusted = as.factor(class_pred_adjusted)) %>%
    mutate(random = rnorm(nrow(.))) %>%
    arrange(-Ja, random) %>%
    mutate(positie=row_number(), 
           hitrate = cumsum(class_actual %in% "Ja") / cumsum(class_actual %in% c("Ja", "Nee"))) %>%
    select(-random)
}

generate_performance_metrics <- function(prediction){
  confusion <- confusionMatrix(prediction$class_pred, prediction$class_actual)
  confusion_adj <- confusionMatrix(prediction$class_pred_adjusted, prediction$class_actual)
  prediction <- prediction %>% arrange(desc(Ja))
  data.frame(Accuracy = confusion$overall[1], 
             Kappa = confusion$overall[2],
             hitrate_top050 = mean(prediction$class_actual[1:50] == "Ja"),
             hitrate_top100 = mean(prediction$class_actual[1:100] == "Ja"),
             hitrate_top200 = mean(prediction$class_actual[1:200] == "Ja"),
             hitrate_top500 = mean(prediction$class_actual[1:500] == "Ja")
  )
}

