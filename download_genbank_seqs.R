library(ape)

setwd("~/PhD_Tetramesa/Experiment Fasta Files")
#####################################################################
# tarantula COI sequences
#####################################################################

# or, since they are GenBank IDs KC664780 - KC665461:

start_taran = 664779
end_taran = 665461

tarantulas = c()

k = 0
for (i in start_taran:end_taran){
  tarantulas[k] = paste("KC", i, sep = "")
  k = k + 1
}

tarantula_seqs = ape::read.GenBank(tarantulas)
write.dna(tarantula_seqs, file ="tarantulas.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)
?write.dna

#####################################################################
# Madascinus lizards
#####################################################################

# JQ007903-JQ008811 for Madascincus Lizards: https://doi.org/10.1371/journal.pone.0068242 

start_liz = 7902 # start one early so that the loop begins at the next number
end_liz = 8811

madascinus_lizards = c()

j=0
for (i in start_liz:end_liz){
  madascinus_lizards[j] = paste("JQ00", i, sep = "")
  j = j + 1
}

madascinus_lizards

lizard_genbank_seqs = ape::read.GenBank(madascinus_lizards)
write.dna(tarantula_seqs, file ="madascinus_lizards.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)

# Note that these are not all the same gene

#####################################################################
# Water mites (Hydrachnidia)
#####################################################################

# MK889511-MK889751 (COI)
# MK889752-MK889992  (28S)

start_mites_coi = 889510
end_mites_coi = 889751

start_mites_28s = 889751
end_mites_28s = 889992

mites_coi = c()
mites_28s = c()

m =0 
for (i in start_mites_coi:end_mites_coi){
  mites_coi[m] = paste("MK", i, sep = "")
  m = m + 1
}

p =0 
for (i in start_mites_28s:end_mites_28s){
  mites_28s[p] = paste("MK", i, sep = "")
  p = p + 1
}

mites_coi
mites_28s

mites_coi_genbank_seqs = ape::read.GenBank(mites_coi)
mites_28s_genbank_seqs = ape::read.GenBank(mites_28s)

write.dna(mites_coi_genbank_seqs, file ="mites_coi.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)
write.dna(mites_28s_genbank_seqs, file ="mites_28s.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)

########################################################################################################
# Pleophylla chafers  https://bmcecolevol.biomedcentral.com/articles/10.1186/s12862-016-0659-3#MOESM1
########################################################################################################

# KC904098 - KC904207 COI

start_chafers = 904097
end_chafers = 904207

chafers = c()

q =0 
for (i in start_chafers:end_chafers){
  chafers[q] = paste("KC", i, sep = "")
  q = q + 1
}

chafers

chafers_genbank_seqs = ape::read.GenBank(chafers)
write.dna(chafers_genbank_seqs, file = "chafers_coi.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)


########################################################################################################
# ant-loving beetles # https://doi.org/10.3390/insects11010064
########################################################################################################

# COI MN536369 to MN536434

start_ant_beetles = 536368
end_ant_beetles = 536434

ant_beetles = c()

t = 0
for (i in start_ant_beetles:end_ant_beetles){
  ant_beetles[t] = paste("MN", i, sep = "")
  t = t + 1
}

ant_beetles

ant_beetles_genbank_seqs = ape::read.GenBank(ant_beetles)
write.dna(ant_beetles_genbank_seqs, file = "ant_beetles_coi.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)

########################################################################################################
# ants  https://zookeys.pensoft.net/article/29705/
########################################################################################################

#MH754200-MH754506

start_ants = 754199
end_ants = 754506

ants = c()

v = 0
for (i in start_ants:end_ants){
  ants[v] = paste("MH", i, sep = "")
  v = v + 1
}

ants

ants_genbank_seqs = ape::read.GenBank(ants)
write.dna(ants_genbank_seqs, file = "ants_coi.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)

########################################################################################################
# Eois Geometrid moths https://onlinelibrary.wiley.com/doi/full/10.1111/j.1744-7917.2010.01366.x
########################################################################################################

# GQ433499 to GQ433604
# GU725087 to GU725391

eois_start_gq = 433498
eois_end_gq = 433604

eois_start_gu = 725086
eois_end_gu = 725391

eois_gq = c()
eois_gu = c()

w=0
for (i in eois_start_gq:eois_end_gq){
  eois_gq[w] = paste("GQ", i, sep = "")
  w = w + 1
}

y=0
for (i in eois_start_gu:eois_end_gu){
  eois_gu[y] = paste("GU", i, sep = "")
  y = y + 1
}

eois = c(eois_gq, eois_gu) # combine them
eois

eois_genbank_seqs = ape::read.GenBank(eois)
write.dna(eois_genbank_seqs, file = "eois_coi.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)


######################
# caddis flies
######################

setwd("~/PhD_Tetramesa/SPEDE-SAMPLER TESTS/Caddisflies")

start_caddis = 513774
end_caddis = 513874
caddis_ids = c()

k = 0
for (i in start_caddis:end_caddis){
  caddis_ids[k] = paste("EF", i, sep = "")
  k = k + 1
}
  caddis_seqs = ape::read.GenBank(caddis_ids)
  write.dna(caddis_seqs, file ="caddis.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = "", colw = 10)
 
