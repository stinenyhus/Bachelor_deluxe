
svm_model_fn <- function(train_data, formula, hyperparameters) { 
  # Expected hyperparameters:
  # - kernel
  # - cost
  # if (!"kernel" %in% names(hyperparameters))
  #   stop("'hyperparameters' must include 'kernel'")
  # if (!"cost" %in% names(hyperparameters))
  #   stop("'hyperparameters' must include 'cost'")
  e1071::svm(
    formula = formula,
    data = train_data,
    kernel = hyperparameters[["kernel"]],
    cost = hyperparameters[["cost"]],
    scale = FALSE,
    type = "C-classification",
    probability = TRUE
  )
}

# Create predict function that returns the predictions
svm_predict_fn <- function(test_data, model, formula,
                           hyperparameters, train_data) {
  predictions <- stats::predict(
    object = model,
    newdata = test_data,
    allow.new.levels = TRUE,
    probability = TRUE
  )
  # Extract probabilities
  probabilities <- dplyr::as_tibble(
    attr(predictions, "probabilities")
  )
  # Return second column
  probabilities[[2]]
}





