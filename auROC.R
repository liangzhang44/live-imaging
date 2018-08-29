auROC <- function(vector1, vector2){
    library(pracma)
    
    maxZ <- max(vector1, vector2)
    minZ <- min(vector1, vector2)
    criteria <- seq(round(minZ), round(maxZ))
    x <- vector()
    y <- vector()
    
    for (i in 1:length(criteria)) {
        x[i] <- length(vector1[vector1 >= criteria[i]])/length(vector1)
        y[i] <- length(vector2[vector2 >= criteria[i]])/length(vector2)
    }
    
    auc <- -1*trapz(x, y)
    auc
}