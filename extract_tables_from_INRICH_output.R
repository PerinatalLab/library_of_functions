

getINRICHpvals = function(inrinchOutputFile) {
        temp88=read.table(pipe(paste("gawk -v OFS='\t' '$0~/Main Analysis Results/{pr=1} ",
        "$0~/------/{next} $0~/^$/{pr=0} pr==1{print $2,$3,$4,$5,$6,$7,$8}' ",
        inrinchOutputFile,sep="")),sep="\t",skip=1,h=T,stringsAsFactors=F)
        temp88
}
print("Function \"getINRICHpvals\" extracts a full table of pvalues from INRINCH output file.",quote=F)
print("(input required: full address and name of INRICH output file)",quote=F)



getINRICHgenes = function(inrinchOutputFile,GeneSetName) {
        temp99=read.table(pipe(paste("gawk -v OFS='\t' '$0~/Interval-Gene-Target/{pr=1} ",
        "$0~/------/{next} $0~/^$/{pr=0} pr==1{print $2,$5,$6,$7\" \"$8,$9}' ",
        inrinchOutputFile ," | awk '$0~/",GeneSetName,"/{print $1,$2,$3,$6}'",sep="")),
        sep=" ",stringsAsFactors=F)
        temp99
}
print("Function \"getINRICHgenes\" extracts names of enriched genes for a particular geneSet from INRINCH output file.",quote=F)
print("(input required:  full address and name of INRICH output file;  name of \'Target\' geneSet requested)",quote=F)




