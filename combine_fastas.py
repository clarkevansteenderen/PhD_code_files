import os

##############################################################
# A quick script to combine single FASTA files into one large
# multiple-sequence file
##############################################################

DIR = input("\nInput folder path containing FASTA files to combine into one FASTA file: ")
os.chdir(DIR)
FILE_NAME = input("\nWhat would you like to name your output file (e.g. combo.fas)? Note: "
                  "Please add the .fas extension: ")
output_fas = open(FILE_NAME, 'w')
file_count = 0

print(len(os.listdir(DIR)))

for f in os.listdir(DIR):
    if f.endswith((".fas", ".fasta", ".FASTA")):
        file_count += 1
        fh = open(os.path.join(DIR, f))
        for line in fh:
            output_fas.write(line)
        fh.close()

output_fas.close()
print(str(file_count) + " FASTA files were merged into one file, which can be found here: " + DIR)
