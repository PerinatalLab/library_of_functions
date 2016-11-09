library(ggplot2)
library(dplyr)
library(Rcpp)

mfr = read.table("~/Documents/various_scripts/lga_deleteme.dat", h=T)

cppFunction("NumericVector rollmeanC(NumericVector x, int w){
        int lx = x.size();
        NumericVector out(lx);
        w = w-1;
        for(int r = 0; r < lx; r++){
                int l = std::max(0, (2*r - w)/2);
                int u = std::min((lx-1), r + w/2)+1;
                if(r < (w+1)/2 || lx-1-r < w/2){
                        out[r] = -9;
                } else {
                        out[r] = std::accumulate(x.begin()+l, x.begin()+u, 0.0f)/(u-l);
                }
        }
        return out;
}")
rollmean = function(x, w){
        tmp = rollmeanC(x, w)
        tmp[which(tmp == -9)] = NA
        return(tmp)
}

mfr$MLANGDbin = cut(mfr$MLANGD, seq(150, 185, by=5))
mfr = group_by(mfr, MLANGDbin) %>%
        arrange(GRDBS) %>%
        mutate(roll1 = rollmean(LGA, 1),
               roll200 = rollmean(LGA, 200),
               roll700 = rollmean(LGA, 700),
               roll2000 = rollmean(LGA, 2000),
               roll5000 = rollmean(LGA, 5000))
        
ggplot(filter(mfr, !is.na(MLANGDbin)), aes(x=GRDBS)) +
#        geom_point(aes(y=roll1), col="grey") +
#        geom_line(aes(y=roll200), col="grey60") +
        geom_line(aes(y=roll700), col="grey20") +
        geom_line(aes(y=roll2000), col="orange", size=0.7) +
        geom_line(aes(y=roll5000), col="red", size=1) +
        facet_grid(.~MLANGDbin) + 
        scale_y_log10() + coord_flip() + theme_bw()


