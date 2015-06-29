
################################################
# finds clusters among positions on a single chr
#  (returns a TABLE of clusters)

#  v2.0 final commercial release
#  by Julius and Jonas (2015 May 30)


findClusters_table = function(pos,thr) {
        pos= sort(pos)  #  all positions of-interest (must be from a SINGLE chr!)
        position=1
        out=NULL
        repeat {
                grSize=1
                grStart=position
                while(position<length(pos)) {
                        tst1=(pos[position+1]-pos[position]) < thr
                        position = position + 1
                        if (tst1) { 
                                grSize=grSize+1
                        }  else  break 
                }
                
                out=rbind(out,data.frame(grStart,grSize))
                
                if(sum(out$grSize)==length(pos)) break
                
                
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
        out
}

