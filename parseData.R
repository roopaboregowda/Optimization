parseData <- function(data, firstcol, total_runs){
    col <- firstcol
    
    allstats <- (ncol(data)-1)/total_runs   #how many stats were collected.
    cols <- seq(col,total_runs*allstats, by=allstats)
    subdata <- data[,cols]
    #print(subdata)
    noGens <- nrow(data)
    pdata <- matrix(nrow = noGens, ncol = 3)
    for (i in 1:noGens){
      pdata[i,1] = i
      print(subdata[i,])
      pdata[i,2] =mean(subdata[i,], na.rm = TRUE)#mean(rowMeans(subdata[i,]))
      pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(total_runs)   
      #pdata[i,4] = min(subdata[i,])
       }
  
    return (pdata)
}


