#This program is to remove rows which has value 'NA' in any of the categorical variable.
import csv

#REading merger.csv file and writing data into combined csv
with open('merge.csv', 'rb') as inp, open('combined.csv', 'wb') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
    	
    	#checking for Year, UniqueCarrier, TailNum, Origin, Dest and CancellationCode
        if row[0] != "NA" or row[8] != "NA" or row[10] != "NA" or row[16] != "NA" or row[17] != "NA" or row[22] != "NA":
            writer.writerow(row)
    inp.close()
    out.close() 
