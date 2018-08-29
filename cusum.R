cusum<-function(files, d, d_cusum){
    # read each file, fit curve, take residuals, put into d_cusum
    fileone<-read.csv(files[1])
    for(i in 1:(ncol(fileone)-2)){
        for(j in 1:length(files)){
            f<-read.csv(files[j])
            d[,j+1]<-f[,i+2]
            # subset one column of data for model fitting
            thesub<-d[,c(1,j+1)]
            names(thesub)<-c('time','signal')
            # fit one phase decay curve with self-start
            d_model<-try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = thesub), silent = TRUE)
            # discard data if not fitted
            if(class(d_model)=='nls'){
                d[,j+1]<-resid(d_model)
            } else {
                d[,j+1]<-NA
            }
        }
        
        #calculate mean for each row
        d$mean<-rowMeans(d[,2:length(files)+1], na.rm = TRUE)
        # calculate baseline
        baseline<-mean(d$mean[d$time<10])
        # substract baseline from each datapoints
        d$a_b<-d$mean-baseline
        # calculate CuSum
        d$cusum<-cumsum(d$a_b)
        # put CuSum into the cusum data frame
        d_cusum[,i+1]<-d$cusum
    }
    d_cusum
}