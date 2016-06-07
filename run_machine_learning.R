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
run_machine_learning <- function(x, ..., y = NULL, p = NULL, subset = NULL, reduce = FALSE) {
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
    result <- model_machine(x, y, subset, reduce, ...)
    return(result)
}

model_machine <- function(x, y, subset, reduce, ...) {
    output <- list()
    x_test <- x[-subset, ]
    if (NROW(x_test) == 0) {
        x_test <- NULL
    }
    output$model <- randomForest(x[subset, ], y[subset], xtest = x_test, ytest = y[-subset], votes = TRUE, importance = TRUE, ...)
    output$variable_importance <- importance(output$model)
    output$votes <- output$model$test$votes
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
