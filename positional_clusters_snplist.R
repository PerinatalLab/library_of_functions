
################################################
# finds representative SNP from clusters of positions on a single chr
#  (returns a list of SNPs to-be-used)

#  v2.0 final commercial release
#  by Julius and Jonas (2015 May 30, June 29)


findClusters_snplist(pos,pvals,thr)

findClusters_snplist = function(pos,pvals,thr) {
        pos1= sort(pos)  #  all positions of-interest (must be from a SINGLE chr!)
        position=1
        out=NULL
        repeat {
                grSize=1
                grStart=position
                while(position<length(pos1)) {
                        tst1=(pos1[position+1]-pos1[position]) < thr
                        position = position + 1
                        if (tst1) { 
                                grSize=grSize+1
                        }  else  break 
                }
                
                out=rbind(out,data.frame(grStart,grSize))
                
                if(sum(out$grSize)==length(pos1)) break
                
                
        }
        #out=out[ which(out$grSize>1),]
        
        lfts=rghs=NULL
        for (i in 1:dim(out)[1]) {
                lft=pos[out[i,"grStart"]]
                rgh=pos[ out[i,"grStart"]+out[i,"grSize"]-1  ]
                lfts=c(lfts,lft)
                rghs=c(rghs,rgh)
                rm(lft,rgh)
        }
        out=data.frame(out,left=lfts,right=rghs)
        
        # which SNP should be included in further-down analysis
        tst2 = rep(FALSE,length(pvals))
        for (i in 1:dim(out)[1]) {
        lft = out[i,"left"]
        rgh = out[i,"right"]
        ix1 = which((pos >= lft)&(pos <= rgh))
        cand_pvals = pvals[ix1]
        ix2 = which(cand_pvals == min(cand_pvals))
        ix3 = sample(ix2,1,replace=F)
        tst2[ix1[ix3]]=TRUE
        }
        
        tst2
}


