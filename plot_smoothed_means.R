library(ggplot2)
library(dplyr)
library(Rcpp)

# by Julius, 2016 Nov 11

###     Two functions for plotting rolling means of X
###     over a range of Y in strata of B with variable window size 
###     (days for dailyPlot, samples for rollyPlot) used to compute mean.
###     Original use: X=LGA, Y=gestAge, B=matHgh

### NOTE: user must create the variable bins, e.g.:
###               mfr = read.table("~/Documents/various_scripts/lga_deleteme.dat", h=T)
###               mfr$MLANGDbin = cut(mfr$MLANGD, seq(150, 185, by=5))
###     and then run this, e.g.:
###               rollyPlot(mfr, "MLANGDbin", "LGA", "GRDBS", c(200, 700, 2000, 6000))

### USAGE: rollyPlot(df, colname.b, colname.x, colname.y, windows)
###        dailyPlot(df, colname.b, colname.x, colname.y, mincases)
#       df      input data frame
#       colname.b    name of the binned variable column, such as MLANGD (string)
#       colname.x    name of the variable column for which we calculate the mean, such as LGA (string)
#       colname.y    name of the variable column along which we roll, such as GA (string)
#       windows      values of the rolling window sizes, currently 4 (vector of 4 integers)
#       mincases     adjacent windows will be joined until this number of cases is reached (integer)


cppFunction("NumericVector rollmean(NumericVector x, int w){
        int lx = x.size();
        NumericVector out(lx, NA_REAL);
        int r = (w-1)/2;
        for(int l = 0; l < lx-w+1; l++){
                out[r] = std::accumulate(x.begin()+l, x.begin()+l+w, 0.0f)/w;
                r++;
        }
        return out;
}")

rollyPlot = function(df, colname.bins, colname.x, colname.y, windows){
        colnames(df)[colnames(df) == colname.bins] = "MH"
        colnames(df)[colnames(df) == colname.x] = "PHE"
        colnames(df)[colnames(df) == colname.y] = "GA"
        
        print("calculating means...")
        df = filter(df, !is.na(MH)) %>% group_by(MH) %>% arrange(GA) 
        dfm = summarize(df, m=mean(PHE))
        
        # sorry for this mess, but the C++ part somehow doesn't work with normal mutate:
        mfr_plot = bind_rows(w1 = do(df, r = rollmean(.$PHE, windows[1]), GA = .$GA),
                             w2 = do(df, r = rollmean(.$PHE, windows[2]), GA = .$GA),
                             w3 = do(df, r = rollmean(.$PHE, windows[3]), GA = .$GA),
                             w4 = do(df, r = rollmean(.$PHE, windows[4]), GA = .$GA),
                             .id="window") %>%
                apply(1, function(x){ data.frame(window = x$window, MH = x$MH, GA = x$GA, fr=unlist(x$r)) }) %>%
                bind_rows %>%
                mutate(window = factor(window, labels = windows))
        
        print("plotting...")
        ggplot(mfr_plot, aes(x=GA)) +
                geom_line(aes(y=fr, col=window, size=window)) +
                geom_hline(data=dfm, aes(yintercept=m), lty="dashed", col="firebrick", size=0.3) +
                facet_grid(.~MH) +
                scale_color_manual(values=c("grey60", "grey30", "orange", "red")) +
                scale_size_manual(values=c(0.3, 0.5, 0.7, 1)) + 
                scale_y_log10(breaks=c(0.001, 0.003, 0.01, 0.03, 0.1)) +
                coord_flip() + theme_bw() + xlab(colname.y) + ylab(colname.x)
}


cppFunction("NumericVector rollwindow(NumericVector x, int m){
        NumericVector out(x.size(), NA_REAL);
        int g = 1;
        int sum = 0;
        for(int r = 0; r < x.size(); r++){
                out[r] = g;
                sum += x[r];
                if(sum >= m){
                        g++;
                        sum = 0;
                }
        }
        return out;
}")

dailyPlot = function(df, colname.bins, colname.x, colname.y, mincases){
        colnames(df)[colnames(df) == colname.bins] = "MH"
        colnames(df)[colnames(df) == colname.x] = "PHE"
        colnames(df)[colnames(df) == colname.y] = "GA"
        
        print("calculating means...")
        df = filter(df, !is.na(MH))
        dfm = group_by(df, MH) %>% summarize(m = mean(PHE))
        df = group_by(df, MH, GA) %>%
                summarize(c = sum(PHE==1), n = n()) %>%
                mutate(w = rollwindow(c, mincases)) %>%
                group_by(MH, w) %>%
                summarize(fr = sum(c)/sum(n), GA = (min(GA) + max(GA))/2)
        
        print("plotting...")
        ggplot(df, aes(x=GA)) +
                geom_line(aes(y=fr), col="orange") +
                geom_hline(data = dfm, aes(yintercept=m), lty="dashed", col="firebrick", size=0.3) +
                facet_grid(.~MH) +
                scale_y_log10(breaks=c(0.001, 0.003, 0.01, 0.03, 0.1)) +
                coord_flip() + theme_bw() + xlab(colname.y) + ylab(colname.x)
}
