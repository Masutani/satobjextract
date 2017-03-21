#' Extract map patch from google maps 
#'
#' @param basedir base directory
#' @param df SpatialPointsDataFrame for target points
#' @param truth boolean selector positive negative position selection
#' @param start image index
#' @param trialinaday max trial in a day of google map query
#' @import maptools
#' @import sp
#' @import ggplot2
#' @import ggmap
extractMap <- function(basedir,df,truth,start,trialinaday) {
    if (truth) {
        label = "1"
    } else {
        label = "0"
    }
    lonlist <- df@coords[,1]
    latlist <- df@coords[,2]
    if (truth) {
        for (i in start:(start + trialinaday)) {
            filename <- paste0(basedir,label,sprintf("/sat_%04d.jpg",i))
            if (lonlist[i] != 0 & latlist[i] != 0) {
                map <- get_map(location = c(lon = lonlist[i], lat = latlist[i]), source = "google", maptype = "satellite", color = "color", zoom = 20, scale = 2)
                g <- ggmap(map, extent = "device")
                #plot(g)
                ggsave(file = filename, plot = g, width = 1, height = 1, scale = 1, dpi = 100)
            }
        }
    } else {
        lonrange <- 0.050 * 0.010966404715491394
        latrange <- 0.050 * 0.0090133729745762
        xmin <- min(lonlist)
        xmax <- max(lonlist)
        ymin <- min(latlist)
        ymax <- min(latlist)
        for (i in start:(start + trialinaday)) {
            filename <- paste0(basedir,label,sprintf("/sat_%04d.jpg",i))
            lon <- runif(1, min = xmin, max = xmax)
            lat <- runif(1, min = ymin, max = ymax)
            betweenlon <- lonlist > lon - lonrange & lonlist < lon + lonrange
            betweenlat <- latlist > lat - latrange & latlist < lat + latrange
            if (sum(betweenlon & betweenlat) == 0) {
                map <- get_map(location = c(lon = lon, lat = lat), source = "google", maptype = "satellite", color = "color", zoom = 20, scale = 2)
                g <- ggmap(map, extent = "device")
                #plot(g)
                ggsave(file = filename, plot = g, width = 1, height = 1, scale = 1, dpi = 100)
            }
        }
    }

}

