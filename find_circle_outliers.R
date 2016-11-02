### finds the most distant points in each segment of an ellipse
# by Julius J. 2016 Nov 02

# USAGE: findCircleOutliers(df, x, y, segments, outliers)
#     df        input data frame
#     x,y       two columns that will be used to create an ellipse (strings)
#     segments  numbers of segments of interest (integer)
#     outliers  number of outliers to return (integer)

library(dplyr)

findCircleOutliers = function(df, x, y, segments, outliers){
        colnames(df)[colnames(df) == x] = "xx"
        colnames(df)[colnames(df) == y] = "yy"
        k = coef(lm(yy ~ xx, data=df))[2]
        
        # normalize
        df = mutate(df, xn = (xx-mean(xx))/sd(xx), yr = yy-k*xx, yn = (yr-mean(yr))/sd(yr))
        
        # convert to polar
        df = mutate(df, theta = atan2(yn, xn)/pi, r = sqrt(yn^2 + xn^2), segm = cut(theta, segments))
        
        # find outliers
        df = top_n(df, outliers, r) %>%
                select(-theta, -r, -xn, -yn)

        colnames(df)[colnames(df) == "xx"] = x
        colnames(df)[colnames(df) == "yy"] = y
        return(df)
}
