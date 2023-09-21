setwd(dirname(rstudioapi::documentPath()))
raw_dat = read.csv('data_g.csv')

colnames(raw_dat)

raw_dat[1,]
View(raw_dat)
View(t(raw_dat[1,]))
