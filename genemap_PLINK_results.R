
genemapPLINKresults = function(assoc,offset_kb) {  # CHR SNP BP P   columns must be present

ok_cols = grep("^CHR$|^SNP$|^BP$|^P$",colnames(assoc))
assoc = assoc[,ok_cols]

bfile="/Users/jb/Biostuff/MOBA_GENETIC_DATA/MoBa_QCed_5530/H_FINAL/MoBa_5530_EurQC_hg19"
temp_dir="/Users/jb/Biostuff/MOBA_GESTAGE_GWAS/PERMUT/temp_dir/"
tempFile1=paste(temp_dir,"temporaryFile_toPLINK_deleteME.txt",sep="")
tempFile2=paste(temp_dir,"temporaryFile_fromPLINK_deleteME",sep="")
write.table(assoc,tempFile1,row.names=F,col.names=T,sep="\t",quote=F)


genes_original_address = "/Users/jb/Biostuff/hg19_HUMAN_GENES/glist-hg19"
genes_pruned_address = paste(temp_dir,"temporaryFile_geneList_hg19_pruned.txt",sep="")
genes_original = read.table(genes_original_address,stringsAsFactors = F,h=F)
bad_lines = grep("^LINC[0-9]+|^LOC[0-9]+|^MIR[0-9]+|^ZNF[0-9]+|^SNOR[0-9]+|orf",genes_original$V4)
genes_pruned = genes_original[-bad_lines,]
write.table(genes_pruned,genes_pruned_address,col.names=F,row.names=F,sep="\t",quote=F)

# the following command contains parameters that should prevent clumping
# and only do gene-annotation work.
cmnd = paste("plink --bfile ",bfile," --noweb --clump ",tempFile1,
" --clump-p1 1 --clump-p2 1 --clump-r2 1 --clump-kb 0.01 --clump-range ",genes_pruned_address,
" --clump-range-border ",offset_kb," --out ",tempFile2,sep="") # note that "--make-founders require-2-missing " was omited
system(cmnd,ignore.stdout = F)
warning("note that NOT ALL GENES are used in annotation: LINC/LOC/MIR/ZNF/SNOR/orf are omited!")
fromPLINK = read.table(paste(tempFile2,".clumped.ranges",sep=""),h=T,stringsAsFactors = F)
fromPLINK
}

