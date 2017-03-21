#' Build CSV dataset from hierachical image collections
#' 
#' @param basedir base directory
#' @param labels list of labels
#' @param isGray boolean flag whether convert image to gray or not
buildDataset <- function(basedir, labels, isGray) {
    dataset <- data.frame()
    for(i in 1:length(labels)) {
      imagedf <- imageToDataset(paste0(basedir, labels[i], "/") , paste0(basedir, "sat" , labels[i], ".csv" ),  labels[i], isGray ) 
      dataset <- rbind(dataset,imagedf)
    }
    shuffled <- dataset[sample(1:nrow(dataset)),]
    # split dataset into train/test datasets
    half <- nrow(dataset) %/% 2
    train <- shuffled[1:half,]
    test <- shuffled[(half + 1):nrow(dataset),]
    write.csv(train, paste0(basedir, "train.csv"), row.names = FALSE)
    write.csv(test, paste0(basedir, "test.csv"), row.names = FALSE)
}

#' Convert images to CSV dataset
#'
#' @param basedir base directory
#' @param outputdir directory for dataset file for each label
#' @param label label
#' @param isGray boolean flag whether convert image to gray or not
#' @import EBImage
#' @return converted data.frame of labeled images
imageToDataset <- function(basedir, outputdir, label, isGray) {
    imagefiles <- list.files(basedir)
    w <- 30
    h <- 30
    img_size <- w*h
    df <- data.frame()
    for (i in 1:length(imagefiles)) {
        result <- tryCatch({
            imgname <- imagefiles[i]
            img <- readImage(paste0(basedir,"/",imgname)) 
            img_resized <- EBImage::resize(img, w,h)
            if (isGray) {
                img_resized <- channel(img_resized,"gray")
            }
            img_matrix <- img@.Data
            if(isGray) {
              img_vector <- as.vector(array(img_matrix,dim=c(1,w*h)))
            } else {
              img_vector <- as.vector(array(img_matrix,dim=c(1,w*h*3)))
            }
            vec <- c(label, img_vector)
            df <- rbind(df, vec)
        },
        error = function(e) { print(e) })
    }
    names(df) <- c("label", paste0("pixel", c(1:img_size)))
    write.csv(df, outputdir, row.names = FALSE)
    return(df)
}

