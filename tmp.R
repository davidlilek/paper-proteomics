########################get intersections of E4_D1 LFQ
### und dann suchen wir in den proteinen die über alle 9 fraktionen kommen die massen


library(UpSetR)

  
######################
###E4_D1 LFQ
######################
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_B_LFQ_combined_txt_proteinGroups.txt_LFQ.RDS")
data <- data[[1]]
data[] <- data > 0
data[is.na(data)]<- FALSE  
data_sub <- dplyr::select(data, contains("Pool_G"))
B_D1 <- data_sub*1
B_D1 <- sapply(seq(1,17,by=2),function(i) rowSums(B_D1[,i:(i+1)]))
B_D1[B_D1==0] <- NA

### analysis B
proteins <- as.data.frame(which(!is.na(B_D1), arr.ind=TRUE))

B1 <- proteins %>% dplyr::filter(col == 1)
B2 <- proteins %>% dplyr::filter(col == 2)
B3 <- proteins %>% dplyr::filter(col == 3)
B4 <- proteins %>% dplyr::filter(col == 4)
B5 <- proteins %>% dplyr::filter(col == 5)
B6 <- proteins %>% dplyr::filter(col == 6)
B7 <- proteins %>% dplyr::filter(col == 7)
B8 <- proteins %>% dplyr::filter(col == 8)
B9 <- proteins %>% dplyr::filter(col == 9)


intersct <- Reduce(intersect, list(B1$row,B2$row,B3$row,B4$row,B5$row,B6$row,B7$row,B8$row,B9$row))

data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_B_LFQ_combined_txt_proteinGroups.txt_LFQ.RDS")
data <- data[[1]]
data_sub <- dplyr::select(data, contains("Pool_G"))
data_sub <- data_sub[intersct,]
data_sub$FASTA <- data$FASTA[intersct]

for (i in 1:nrow(data_sub)){
  plot(rep(1:9,each=2),data_sub[i,-ncol(data_sub)], main=rownames(data_sub)[i])
}

library(UniprotR)
#test <- GetMiscellaneous(data$FASTA[intersect()])
#Obj <- GetMiscellaneous(data$FASTA[strsplit(data$FASTA[1],split='\\|')[[1]][2]]

fasta <- c()
for (i in 1:length(data_sub$FASTA)){
  tmp <- strsplit(data_sub$FASTA[i],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}

seq <- GetSequences(fasta)

for (i in 1:nrow(data_sub)){
  plot(rep(1:9,each=2),
       data_sub[i,-ncol(data_sub)],
       main=paste("Mass:",seq$Mass[i], " Row:",rownames(data_sub)[i],sep=""))
}

test <- GetSequences("O14520")


