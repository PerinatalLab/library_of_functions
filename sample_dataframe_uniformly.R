#!/usr/bin/Rscript

### takes a uniformly distributed sample from a non-uniform data frame,
### using equally-spaced bivariate bins

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

makeUniformSample = function(df, nameX, nameY, binsX, binsY){
        df = data.frame(df)
        df$binX = cut(df[,nameX], binsX)
        df$binY = cut(df[,nameY], binsY)
        
        ns = group_by(df, binX, binY) %>% summarize(n=n())
        df = inner_join(df, ns, by=c("binX", "binY"))
        df = group_by(df, binX, binY)
        
        # uncomment the function of choice
        #nsample = min(ns$n)
        nsample = 10 
        #nsample = round(mean(sort(ns$n)[1:5]))
        
        if(min(ns$n) < nsample){
                takeSample = filter(df, n>nsample) %>% sample_n(nsample)
                takeEverything = filter(df, n<=nsample)
                df = bind_rows(takeSample, takeEverything)
        } else {
                df = sample_n(df, nsample)
        }
        return(as.data.frame(ungroup(df)))
}
