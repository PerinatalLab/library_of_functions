

# this function annotates SNP based on their functionality or LD with functional SNPs


# only without PROM deliveries
#rez_dir = "~/Biostuff/MOBA_GESTAGE_GWAS/PERMUT/EXTRACTED_DIGESTED_RESULTS/nonPROM/"
#results =paste(rez_dir, "MomsKids_nonPROM_PermAsympt_5608_hg19.RData",sep="")
#load(file=results)
#a=get("moms")
#a=a[order(a$ePxtrm),];  a=a[which(!is.na(a$ePxtrm)),]
#out=a[,c("SNP","CHR","BP","ePxtrm")];  colnames(out)[4]="P"
#out=out[1:20,]
#head(out)


fun = function(input) {  # SNP CHR  BP P
        
        snps="/Users/jb/Biostuff/hg19_HUMAN_GENES/snp129.attrib"
        temp_dir="~/Biostuff/MOBA_GESTAGE_GWAS/PERMUT/temp_dir/"
        tempFile1=paste(temp_dir,"temporaryFile_toPLINK_deleteME.txt",sep="")
        tempFile2=paste(temp_dir,"temporaryFile_fromPLINK_deleteME",sep="")
        
write.table(input,tempFile1,row.names=F,col.names=T,sep="\t",quote=F)

# generate the clump/annotation data using PLINK
cmnd=paste("plink --annotate ",tempFile1," attrib=",snps," --out ",tempFile2,sep="")
system(cmnd,ignore.stdout = F)

# import the clumping/annotation results
annot=read.table(paste(tempFile2,".annot",sep=""),h=T,stringsAsFactors=F) # contains all snps overlapping FunctionalSNP list
annot

}
