#!/usr/bin/Rscript

### takes a uniformly distributed sample from a non-uniform data frame,
### using equally-spaced bivariate bins
# by Julius J. 2016 Nov 02

### in other words: flattens the bivariate distribution. designed for 
### height-gestAge study, where only the "outer" observations are the 
### most interesting ones, and gbm  takes too long to run on full sample.

# USAGE: makeUniformSample(df, nameX, nameY, binsX, binsY)
#     df      input data frame
#     nameX     name of first variable used to bin (string)
#     nameY     name of second variable used to bin (string)
#     binsX     number of bins to create from the first variable (integer)
#     binsY     number of bins to create from the second variable (integer)
###
###             Number of rows sampled from each bin can be either a constant,
###             size of the smallest bin, or average of several smallest bins.
###             Choose one of these by uncommenting the corresponding function.

library(dplyr)

makeUniformSample = function(df, nameX, nameY, binsX, binsY, nsample){
        df = data.frame(df)
        df$binX = cut(df[,nameX], binsX)
        df$binY = cut(df[,nameY], binsY)
        
        ns = group_by(df, binX, binY) %>% summarize(n = n())
        df = inner_join(df, ns, by=c("binX", "binY"))
        df = group_by(df, binX, binY)
        
        # uncomment the function of choice
        #nsample = min(ns$n)
        #nsample = 10 
        #nsample = round(mean(sort(ns$n)[1:5]))
        
        if(min(ns$n) < nsample){
                takeSample = filter(df, n>nsample) %>% sample_n(nsample)
                takeEverything = filter(df, n <= nsample)
                df = bind_rows(takeSample, takeEverything)
        } else {
                df = sample_n(df, nsample)
        }
        return(as.data.frame(ungroup(df)))
}

### takes a sample that is symmetric around a desired axis
# by Julius J. 2016 Nov 02

# USAGE: makeSymmetricSample(df, column, threshold)
#     df      input data frame
#     column    name of the column containing gest. age values (string)
#     threshold produced sample will be symmetric around x=threshold axis (integer)

makeSymmetricSample = function(df, column, threshold){
        colnames(df)[colnames(df) == column] = "filtcol"
        ns = filter(df, filtcol <= threshold) %>%
                group_by(filtcol) %>%
                summarize(n = n()) %>%
                mutate(filtcol = 2*threshold - filtcol)
        
        belowThreshold = filter(df, filtcol < threshold)
        df = inner_join(df, ns, by="filtcol") %>%
                group_by(filtcol) %>%
                do(sample_n(., .$n[1])) %>%
                bind_rows(belowThreshold)
        
        colnames(df)[colnames(df) == "filtcol"] = column
        return(as.data.frame(df))
}
