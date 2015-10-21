#!/usr/local/Rscript

# by Jonas Bacelis
# University of Gothenburg
# 2015 Oct 20

# the original purpose was to use gene expression data of many (all?) genes in various tissues, 
# and to show where do the candidate genes (from GWAS) end up on the histogram of this data.
# the function contains "anti-clogging" feature that avoids plotting gene names on top of each other.


fun_histogramWithAnnotations = function(all_gene_expression,all_gene_ids,selected_genes,
                                        histgram_breaks_n,hist_title,hist_xlab,sd_thr,
                                        Nmax_genes_horizontally,Nmax_genes_vertically) {
        # all_gene_expression - values of all genes (histogram data)
        # all_gene_ids - names of all genes (to be matched against the selected gene list)
        # selected_genes -  a short vecor of gene names to be annotated on a histogram
        
        h_brk = histgram_breaks_n # histogram breaks
        
        # initial draw (will be overriden later)
        h = hist(all_gene_expression,breaks=h_brk,plot = F) # do not plot visually at first
        
        # subset of data with only selected genes
        tt1 = data.frame(all_gene_expression, all_gene_ids,stringsAsFactors = F)
        tt2 = tt1[which(all_gene_ids %in% selected_genes),]
        tt2 = tt2[order(tt2$all_gene_expression),]
        
        # reduce the subset to only genes-outliers
        center = mean(all_gene_expression)
        sd_val = sd(all_gene_expression)
        tt2 = tt2[which( (tt2$all_gene_expression<(center-sd_val*sd_thr))|
                             (tt2$all_gene_expression>(center+sd_val*sd_thr))),]
        
        # some parameters
        y_max = max(h$counts) # the top values of histogram (height)
        x_wid = max(all_gene_expression)-min(all_gene_expression) # the range of x-axis (plot width)
        x_stp = x_wid/Nmax_genes_horizontally # the fraction of plot-width that is a minimum requirement to plot genes on the same level
        y_stp = y_max/Nmax_genes_vertically # the fraction of the plot-height separating levels of gene names
        text_start = y_max*0.5 # stable, does not change. at what fraction of plot-height does the annotaion start
        text_hgh = text_start # this will be constantly incremented or reset to original value
        objs = NULL # annotation data
        last_zero_level_ix = 1
        for (i in 1:nrow(tt2)) {
                
                # should the y-axis ploting coordinate be updated or kept as it was?
                if(i>1) {   
                        if((tt2[i,1]-tt2[(i-1),1])<x_stp){
                                if (tt2[i,1]-tt2[last_zero_level_ix,1]<x_stp*2) {
                                text_hgh = text_hgh + y_stp
                                } else {
                                        last_zero_level_ix = i # reset the last time gene was plotted at zero level
                                        text_hgh = text_start
                                }
                                
                        } else {
                                last_zero_level_ix = i # reset the last time gene was plotted at zero level
                                text_hgh = text_start
                        }
                }
                
                # graphically annotate (only if you need to estimate thresholds or parameters)
                #segments(x0=tt2[i,1],x1=tt2[i,1],y0=0,y1=text_hgh,col="grey",lwd=0.8)
                #text(tt2[i,1],text_hgh+y_stp/2,tt2[i,2],cex=0.7)
                # save the annotation data for later use
                temp = data.frame(i=i,x=tt2[i,1],y1=text_hgh,y2=text_hgh+y_stp/2,
                                  gene=tt2[i,2],stringsAsFactors = F)
                objs = rbind(objs,temp); rm(temp)
        }
        
###########   plot from scratch, to deal with the order of objects (text on top of lines etc)
        # plot the frame first
        hist(all_gene_expression,breaks=h_brk,col="white",border="white",main=hist_title,xlab=hist_xlab)
        # FIRST add segments ...
        segments(x0=objs$x,x1=objs$x,
                 y0=rep(0,nrow(objs)),y1=objs$y1,col="grey",lwd=0.8)
        # and only THEN add text
        text(objs$x,objs$y2,objs$gene,cex=0.7)
        # plot-over the histogram body
        hist(all_gene_expression,breaks=h_brk,col="grey",border="white",add=T)

# add line which shows the threshold used for entering genes on the plot
left = center-sd_val*sd_thr
right = center+sd_val*sd_thr
segments(x0=c(left,right),x1=c(left,right),
         y0=c(-y_max*.03,-y_max*.03),
         y1=c(y_max*.03,y_max*.03),col="red",lwd=4)

} # end of function

