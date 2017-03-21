#' Train DNN model with MXNet
#' 
#' \code{trainDNN} returns trained model with DNN
#'
#' @param train.x training data
#' @param train.y training labels
#' @import mxnet
#' @return the model trained
trainDNN <- function(train.x, train.y) {
    data <- mx.symbol.Variable("data")
    fc1 <- mx.symbol.FullyConnected(data, name = "fc1", num_hidden = 128)
    act1 <- mx.symbol.Activation(fc1, name = "relu1", act_type = "relu")
    fc2 <- mx.symbol.FullyConnected(act1, name = "fc2", num_hidden = 64)
    act2 <- mx.symbol.Activation(fc2, name = "relu2", act_type = "relu")
    fc3 <- mx.symbol.FullyConnected(act2, name = "fc3", num_hidden = 10)
    softmax <- mx.symbol.SoftmaxOutput(fc3, name = "sm")
    devices <- mx.cpu()
    mx.set.seed(0)
    model <- mx.model.FeedForward.create(softmax, X = train.x, y = train.y,
                                            ctx = devices, num.round = 10, array.batch.size = 100,
                                            learning.rate = 0.07, momentum = 0.9, eval.metric = mx.metric.accuracy,
                                            initializer = mx.init.uniform(0.07),
                                            epoch.end.callback = mx.callback.log.train.metric(100))
    return(model)
}

#' Train CNN model with MXNet
#' 
#' \code{trainCNN} returns trained model with DNN
#'
#' @param train.x training data.
#' @param train.y training labels
#' @param dim dimension of training data
#' @param channels the number of channels (color = 3, grayscale = 1)
#' @return the model trained
trainCNN <- function(train.x,train.y,dim,channels) {
    train_array <- train.x
    dim(train_array) <- c(dim, dim, channels, ncol(train.x))
    # Model
    data <- mx.symbol.Variable('data')
    # 1st convolutional layer 5x5 kernel and 20 filters.
    conv_1 <- mx.symbol.Convolution(data= data, kernel = c(5,5), num_filter = 20)
    tanh_1 <- mx.symbol.Activation(data= conv_1, act_type = "tanh")
    pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2,2), stride = c(2,2))
    # 2nd convolutional layer 5x5 kernel and 50 filters.
    conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50)
    tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
    pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2,2), stride = c(2,2))
    # 1st fully connected layer
    flat <- mx.symbol.Flatten(data = pool_2)
    fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500)
    tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
    # 2nd fully connected layer
    fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
    # Output
    model <- mx.symbol.SoftmaxOutput(data = fcl_2)
    # Set seed for reproducibility
    mx.set.seed(100)
    # Device used. Sadly not the GPU :-(
    device <- mx.cpu()
    # Train on 1200 samples
    model <- mx.model.FeedForward.create(model, X = train_array, y = train.y,
                                         ctx = device,
                                         num.round = 30,
                                         array.batch.size = 100,
                                         learning.rate = 0.05,
                                         momentum = 0.9,
                                         wd = 0.00001,
                                         eval.metric = mx.metric.accuracy,
                                         epoch.end.callback = mx.callback.log.train.metric(100))
    return(model)
}

