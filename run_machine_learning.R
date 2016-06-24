#' @title Classify/Regress by using Machine Learning
#' @description An automated tool helping you to classify or regress
#' @importFrom caret createDataPartition
#' @importFrom caret rfe
#' @importFrom caret rfeControl
#' @importFrom randomForest randomForest
#' @importFrom randomForest rfcv
#' @importFrom randomForest importance
#'
#' @param x A data frame or a matrix of predictors.
#' @param y A response vector. If a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
#' @param p The percentage of data that goes to training.
#' @param subset An index vector specifying the cases to be used in the training sample. You cannot assign p and subset at the same time.
#' @param reduce A boolean specifying whether a cross-validation prediction algorithm should be used to reduce the number of features, in order to prevent overfitting.
#' @return The function returns a list. The \code{model} is the final model used for the \code{prediction}. The \code{variable_importance} shows the importance of every feature.
#' @examples
#' library(vfmodels)
#'
#' # # Classification
#' data(iris)
#' result <- run_machine_learning(x = iris[, !names(iris) %in% 'Species'], y = iris[, 'Species'], p = 0.8)
#'
#' # # Classification using subset and reducing the number of features
#' result <- run_machine_learning(x = iris[, !names(iris) %in% 'Species'], y = iris[, 'Species'], subset = 1:120, reduce = TRUE)
#'
#' # # Regression
#' result <- run_machine_learning(x = iris[, !names(iris) %in% 'Petal.Width'], y = iris[, 'Petal.Width'], p = 0.8)
#' @keywords vfmodels machine_learning run_machine_learning
#' @author Emiel Veersma
#' @export
run_machine_learning <- function(x, ..., y = NULL, p = NULL, subset = NULL, reduce = FALSE, model='rf') {
    y <- unlist(y)
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
    if (reduce) {
        x <- reduce(x, y, subset)
    }
    if (model=='rf') {
      output <- model_machine(x, y, subset, reduce, ...)
    } else if (model=='nn') {
      output <- model_nn(x, y, subset, reduce, ...)
    }
    
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

model_machine <- function(x, y, subset, reduce, ...) {
    output <- list()
    x_test <- x[-subset, ]
    if (NROW(x_test) == 0) {
        x_test <- NULL
    }
    output$model <- randomForest(x[subset, ], y[subset], xtest = x_test, ytest = y[-subset], votes = TRUE, importance = TRUE)
    output$variable_importance <- importance(output$model)
    output$votes <- output$model$test$votes
    output$predictions_train <- output$model$predicted %>% as.character() %>% as.numeric()
    if (!is.null(output$model$test$predicted)) {
      output$predictions_test <- output$model$test$predicted %>% as.character() %>% as.numeric()
    } else {
      output$predictions_test <- NULL
    }
    
    return(output)
}

model_nn <- function(x, y, subset, reduce, ...) {
  output <- list()
  h2o.init(nthreads = 4)
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

reduce <- function(x, y, subset) {
    if (is.null(y)) {
        stop("y cannot be NULL when reducing the number of features")
    }
    cv_model <- rfe(x[subset, ], y[subset], rfeControl = rfeControl())
    x <- x[, cv_model$optVariables]
    return(x)
} 
