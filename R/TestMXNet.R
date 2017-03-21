#' Test CNN model with MXNet
#'
#' @param basedir base directory
#' @param isGray isGray boolean flag whether convert image to gray or not
#' @importFrom stats predict
#' @source TrainMXNet.R
#' @export
TestMXNet <- function(basedir,isGray) {
    # read dataset
    train <- read.csv(paste0(basedir,"train.csv"),header = TRUE)
    test <- read.csv(paste0(basedir,"test.csv"),header = TRUE)
    # conversion and split data and labels
    train <- data.matrix(train)
    test <- data.matrix(test)
    train.x <- t(train[,-1])
    train.y <- train[,1]
    test.x <- t(test[,-1])
    test.y <- test[,1]
    dim <- 30
    if (isGray) {
        channels <- 1
    } else {
        channels <- 3
    }
    test_array <- test.x
    dim(test_array) <- c(dim,dim,channels,ncol(test.x))
    model <- trainCNN(train.x,train.y,dim,channels)
    #DrawNetwork(model)
    # predict
    preds <- predict(model,test_array)
    dim(preds)
    pred.label <- max.col(t(preds)) - 1
    table(pred.label)
    # evaluate
    confusion_matrix <- table(test.y,pred.label)
    print(confusion_matrix)
    prec <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    print(prec)
}