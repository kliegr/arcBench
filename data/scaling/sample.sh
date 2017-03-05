#remove header line
#The KDDCup99 dataset was retrieved from https://www.openml.org/d/1110 and converted from arff to csv
unzip KDDCup99_full.csv.zip
tail -n +2 KDDCup99_full.csv > KDDCup99_full_noheader.csv
#create test file
##create first header
head -n 1 KDDCup99_full.csv > KDDCup99_full_test.csv
#append 1000 random lines
shuf -n 1000 KDDCup99_full_noheader.csv >>KDDCup99_full_test.csv
#remove test lines

#create folds
#it is ok to use the original file with unremoved test rows because the file contains many dulicities anyway
for count in 1000 10000 20000 30000 40000 50000 100000 500000 1000000 3000000
do
head -n 1 KDDCup99_full.csv > KDDCup99_$count.csv
shuf -n $count KDDCup99_full_noheader.csv >>KDDCup99_$count.csv
done
rm KDDCup99_full_noheader.csv
cp KDDCup99_full.csv KDDCup99_4898431.csv