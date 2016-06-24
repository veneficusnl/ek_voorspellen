library(h2o)
library(randomForest)

run_machine_learning <- function(x, ..., y = NULL, p = NULL, subset = NULL, model='rf') {
    y <- unlist(y)
    # create subset if necessary
    if (!is.null(p)) {
        if (!is.null(subset)) {
            stop("You cannot assign p and subset at the same time.")
        }
        if (is.null(y)) {
            subset <- sample(NROW(x), p * NROW(x))
        } else {
            subset <- createDataPartition(y, p = p)[[1]]
        }
    } else if (is.null(subset)) {
        subset <- 1:NROW(x)
    }
    # create randomforest or neural network model
    if (model=='rf') {
      output <- model_rf(x, y, subset, ...)
    } else if (model=='nn') {
      output <- model_nn(x, y, subset, ...)
    }
    # create some output (predictions and hitrates)
    if (class(output$predictions_train)=='factor') {
      output$predictions <- as.factor(c(as.character(output$predictions_train), as.character(output$predictions_test)))
    } else {
      output$predictions <- c(output$predictions_train, output$predictions_test)
    }
    output$hitrate_train <- mean(as.character(output$predictions_train)==as.character(y[subset]))
    if (!is.null(output$predictions_test) & !any(is.na(y[-subset]))) {
      output$hitrate_test <- mean(as.character(output$predictions_test)==as.character(y[-subset]))
      output$crosstable <- table(as.character(output$predictions_test), as.character(y[-subset]))
    } else {
      output$hitrate_test <- NA
      output$crosstable <- NA
    }
    
    return(output)
}

# create randomforest model
model_rf <- function(x, y, subset, ...) {
    output <- list()
    x_test <- x[-subset, ]
    y_test <- y[-subset] %>% na.omit()
    if (NROW(x_test) == 0) {
        x_test <- NULL
    }
    if (length(y_test) == 0) {
      y_test <- NULL
    }
    output$model <- randomForest(x[subset, ], y[subset], xtest = x_test, ytest = y_test, votes = TRUE, importance = TRUE)
    output$variable_importance <- importance(output$model)
    output$votes_test <- output$model$test$votes
    output$predictions_train <- output$model$predicted %>% as.character() %>% as.numeric()
    if (!is.null(output$model$test$predicted)) {
      output$predictions_test <- output$model$test$predicted %>% as.character() %>% as.numeric()
    } else {
      output$predictions_test <- NULL
    }
    
    return(output)
}

# create neural network model
model_nn <- function(x, y, subset, ...) {
  output <- list()
  h2o.init(nthreads = -1)
  x_test <- x[-subset, ]
  if (NROW(x_test) == 0) {
    x_test <- NULL
  }
  train <- bind_cols(data.frame(y[subset], stringsAsFactors = FALSE), x[subset, ])
  train_h2o <- as.h2o(train, "train")
  
  # Run model
  output$model <- h2o.deeplearning(x = 2:ncol(train),  # column numbers for predictors
                            y = 1,   # column number for label
                            training_frame  = train_h2o, # data in H2O format
                            model_id = "nn_model",
                            ...
  )
  # Make prediction
  prediction_h2o <- h2o.predict(output$model, train_h2o)
  output$predictions_train <- as.data.frame(prediction_h2o)$predict
  output$votes_train <- as.data.frame(prediction_h2o) %>% select(-predict)
  if(!is.null(x_test)) {
    test_h2o <- as.h2o(x_test, "test")
    # Make prediction
    prediction_h2o <- h2o.predict(output$model, test_h2o)
    output$predictions_test <- as.data.frame(prediction_h2o)$predict
    output$votes_test <- as.data.frame(prediction_h2o) %>% select(-predict)
  } else {
    output$predictions_test <- NULL
  }
  
  # Shutdown server -----------
  # h2o.shutdown(FALSE)
  
  return(output)
}
