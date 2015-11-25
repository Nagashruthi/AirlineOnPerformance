#This program is to replace all NA's to '0' in numeric variables. 
import sys

def replace():

	#command line takes 3 arguments python script name followed by 
	#input file name and output file name respectively
	if len(sys.argv)!= 3:
		sys.exit("Usage: python replace.py <old_file> <new_file>")
	else:
		old_file = sys.argv[1]
		new_file = sys_argv[2]

	print "Cleaning data in file %s and writing it into file %s \n" %(old_file,new_file)
	w = open(new_file,a)
	with open (old_file,r) as file:
		for line in file:
			w.write(line.replace('NA','0'))
	w.close()
	return 0

#main function
if __name__ == "__main__":
	replace()

