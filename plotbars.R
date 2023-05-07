findminmax <- function(data, minimise = TRUE){
  minmax <- NA
  if (minimise) minmax <- min(data[,2])
  else minmax <- max(data[,2])
  
  rownum <- which(data[,2] == minmax)
  if (length(rownum) > 1) rownum <- rownum[1]
  
  if (minimise)
    return (minmax - data [rownum,3])
  else return (minmax + data [rownum,3])
}

plotbars<- function(data_a, data_b, data_c,a,b,c){
    data = data_a
    hues = c("green","blue","red")
    
    mn1 = findminmax(data_a)   
    mn2 = findminmax(data_b)   
    mn3 = findminmax(data_c)   
    
    mx1 = findminmax(data_a, FALSE)   
    mx2 = findminmax(data_b, FALSE)  
    mx3 = findminmax(data_c, FALSE)   
    
    minn = min(mn1, mn2, mn3)
    maxx = max(mx1, mx2, mx3)
    par(mar = c(5, 6, 4, 4) + 0.1) 
    
    
    dataf <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
    plot(dataf$x, dataf$y, type = "l", col = hues[1], ylim=c(minn, maxx), #ylim = c(0.96, 0.985),   #choose ylim CAREFULLY as per your data ranges
         main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
    print("ok1")
    
    segments(dataf$x, dataf$y - dataf$dy, dataf$x, dataf$y + dataf$dy, col = hues[1]);    #plot the error bars mean-errorbar, mean+errorbar
    print("ok2")
    
    data = data_b
    dataf <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
    lines(dataf$x, dataf$y, col = hues[2])
    segments(dataf$x, dataf$y - dataf$dy, dataf$x, dataf$y + dataf$dy, col = hues[2]); 
    
    data = data_c
    dataf <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
    lines(dataf$x, dataf$y, col = hues[3])
    segments(dataf$x, dataf$y - dataf$dy, dataf$x, dataf$y + dataf$dy, col = hues[3]); 
    
    legend(x="topright", legend = c(a, b, c), col = hues, lwd = 1,
           cex = 0.8,bty = "n", lty = 1)

    
}


