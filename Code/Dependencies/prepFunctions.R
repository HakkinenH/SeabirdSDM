



nearestSea <- function (points, raster, max_distance) {
  # get nearest non_na cells (within a maximum distance) to a set of points
  # points can be anything extract accepts as the y argument
  # max_distance is in the map units if raster is projected
  # or metres otherwise
  
  # function to find nearest of a set of neighbours or return NA
  nearest <- function (lis, raster) {
    neighbours <- matrix(lis[[1]], ncol = length(names(raster))+1)
    point <- lis[[2]]
    # neighbours is a two column matrix giving cell numbers and values
    #land <- !is.na(neighbours[, 2])
    
    if (nrow(neighbours )==1){
      land <- sum(is.na(neighbours[, 2:ncol(neighbours)]))==0
    }else{
      land <- rowSums(is.na(neighbours[, 2:ncol(neighbours)]))==0
    }
    
    
    if (!any(land)) {
      # if there is no land, give up and return NA
      return (c(rep(NA, (4+length(names(raster))))))
    } else{
      # otherwise get the land cell coordinates
      coords <- xyFromCell(raster, neighbours[land, 1])
      
      if (nrow(coords) == 1) {
        # if there's only one, return it
        # calculate distances
        dists <- sqrt((coords[1, 1] - point[1]) ^ 2 +
                        (coords[1, 2] - point[2]) ^ 2)
        
        resT<-c(coords[1, ], neighbours[land, 1], dists, neighbours[land, 2:ncol(neighbours)])
        names(resT)<-c("x","y","cellNum","dist", names(raster))
        return (resT)
      }
      
      # otherwise calculate distances
      dists <- sqrt((coords[, 1] - point[1]) ^ 2 +
                      (coords[, 2] - point[2]) ^ 2)

      landData<-neighbours[land, ]
      
      # and return the value of the closest
      resT<-c(coords[which.min(dists),], landData[which.min(dists), 1], min(dists), landData[which.min(dists), 2:ncol(landData)])
      names(resT)<-c("x","y","cellNum","dist", names(raster))
      
      return (resT)
      
      
    }
  }
  

  
  # extract cell values within max_distance of the points
  neighbour_list <- extract(raster, points,
                            buffer = max_distance,
                            cellnumbers = TRUE)
  
  # add the original point in there too
  neighbour_list <- lapply(1:nrow(points),
                           function(i) {
                             list(neighbours = neighbour_list[[i]],
                                  point = as.numeric(points[i, ]))
                           })
  
  #return (t(sapply(neighbour_list, nearest, raster)))
  
  bigRes<-(lapply(neighbour_list, nearest, raster))
  #print(bigRes)
  dd  <-  do.call(rbind,bigRes)
  return(dd)
}





getMarineVals<- function(tpoint, maxbuff, cellmin=NA, cellmax=NA){
  
  
  if (!is.na(cellmin)&!is.na(cellmax)){
    print(c(cellmin,cellmax))
    tpoint<-tpoint[cellmin:cellmax,]
  }

  dfr<-extract(MarCrop, rbind(tpoint), cellnumbers=TRUE, buffer=maxbuff, na.rm=T)

  allColFunc<-function(x1, fun_n){
    if(!is.na(x1[1])){
      #if there's only one row return that
      if (is.null(nrow(x1))){
        mres <- unlist(x1)
      }else{
        #otherwise work out the function
        mres<-apply(x1, 2, fun_n, na.rm=T)
      }
      
    }else{
      mres<-rep(NA, length(names(MarinePredictors))+1)
    }
    return(mres)
  }
  meanres<-lapply(dfr, allColFunc, fun_n=mean)
  sdres<-lapply(dfr, allColFunc, fun_n=sd)
  
  meanres_df<-do.call(rbind, meanres)
  sdres_df<-do.call(rbind, sdres)
  
  return(list(meanres_df,sdres_df))
}

