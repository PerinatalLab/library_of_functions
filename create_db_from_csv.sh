#!/bin/bash

#### This script converts .csv files into MySQL databases
#### (suitable for SPSS->SQL conversion).
####
#### Created by Julius J. on 2015-09-21
### USAGE: ./create_db.sh input.csv 

## note that if you want to split a file into >1 table,
## do the splitting before this script, i.e.:
# cut -d ',' -f1-1000 > Q3a.csv
# cut -d ',' -f1,1001- > Q3b.csv

## set infile manually here. any comma separated CSV is fine
## table name is also set here
INFILE=$1
TABLENAME='q3b'
rm ${INFILE}_out
touch ${INFILE}_out

## here, number of cycles should be set to NCOLUMNS/100
## (recommend manually checking total variable number in SPSS)
for i in {1..9}
do
	## first, split files into smaller no of columns
	echo "starting chunk ${i}"
	CUTSTART=`echo $i \\* 100 - 99 | bc`
	CUTSTOP=`echo $i \\* 100 | bc`
	cut -d ',' -f ${CUTSTART}-${CUTSTOP} ${INFILE} > ${INFILE}_${i}

	## actual analysis:
	## data types and max lengths are assigned
	awk -F, 'NR==1{
		for (i=1; i<=NF; i++){
			n[i]=$i; t[i]="n"; l[i]=1 };
		next}
	{ for (i=1; i<=NF; i++){
		if (t[i]!~/s/){
			if($i~/[[:alpha:]]/){ t[i]="s" } else {
			if(t[i]!~/f/){
				if($i~/\./){ t[i]="f" }
			}
			}
		};
		m=length($i);
		if(m>l[i]){ l[i]=m }
	}
	}
	END { for (i=1; i<=NF; i++){ print n[i],t[i],l[i] }
	}' ${INFILE}_${i} >> ${INFILE}_out
done

## create table creator query from the above analysis
## manual corrections:
## add NOT NULL after PREG_ID_540 in second line
## add PRIMARY KEY(PREG_ID_540) if wanted
## remove last comma
## quote field and line delimiters and file path at the end

awk -v s=${INFILE} -v t=${TABLENAME} 'BEGIN{
	print "USE moba;"; print "CREATE TABLE " t " ("
}
{ switch($2){
  case "n":
	if($3<3) o="tinyint"
	else if($3<5) o="smallint"
	else if($3<7) o="mediumint"
	else if($3<10) o="int"
	else print "ERROR: oversized integer"
	break
  case "f":
	o="float"
	if($3>12) print "ERROR: oversized float"
	break
  case "s":
	o="varchar(" $3 ")"
	break
}; print $1, o "," }
  END {print " );";
  print "LOAD DATA LOCAL INFILE " s " INTO TABLE " t;
  print " FIELDS TERMINATED BY , LINES TERMINATED BY \\n"
  print " IGNORE 1 ROWS;"
}' ${INFILE}_out > ${INFILE}_query

## now, add the manual corrections;
## create a db named "moba" in mysql;
## launch mysql -uroot --local-infile=1 -p < ${INFILE}_query
