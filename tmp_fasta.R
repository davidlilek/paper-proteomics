aj_raw <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_a-j_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
aj_raw <- aj_raw[[1]]
aj <- aj_raw[,-ncol(aj_raw)]
aj_FASTA <- aj_raw$FASTA

E3_D1_raw <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E3_D1_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
E3_D1_raw <- E3_D1_raw[[1]]
E3_FASTA <- E3_D1_raw$FASTA

FASTA_1 <- E3_FASTA
FASTA_2 <- aj_FASTA

FASTA_1 <- str_extract(FASTA_1,"(?<=tr\\|)[[:alnum:]]+")

res <- c()
for (i in 1:length(FASTA_1)){
  tmp <- strsplit(FASTA_1[i],split='\\|')[[1]][-c(1:3)]
  if (!(length(tmp)==0)){
    for (j in 1:length(tmp)){
      if (any(grepl(tmp[j],FASTA_2))){
        res<-c(res,i)
      }
    }
  }
}

#make unique
res <- unique(res)

remove <- c()
for (i in res){
  if (FASTA_1[i] == FASTA_2[i]){
    remove <- c(remove,i)
  }
}

res_clear <- res
#remove same FASTA files
res_clear <- res_clear[!res_clear %in% remove]
print(paste("Length res_clear:",length(res_clear)))
print(paste("Difference res and res clear:",length(res)-length(res_clear)))
print(paste("Percentage of total number of protein hits:",
            (length(res_clear)/mean(length(FASTA_1),length(FASTA_2))*100),"%")
      )





###############################
### all files
##############################


library(stringr)
#get all results
files <- list.files(path = "results_v002/", pattern="*.RDS")
files <- files[grep("_Unique",files)]
files <- files[!grepl("MBR",files)]
files
file_path <- paste("results_v002/",files,sep="")
#get D5
file_path <- c(file_path,"./27_20230201_FH/results/D5_extracts_Razor.RDS")
#get all combinations 1:2 defines that you get eine 2er combination
file_path_combinations <- lapply(1:2, function(y) combn(file_path, y))
file_path_combinations <- as.data.frame(file_path_combinations[[2]])
file_path_combinations <- subset(expand.grid(rep(list(file_path),2)), Var1 != Var2)
file_path_combinations <- as.data.frame(t(file_path_combinations))

#combine all file path and analyze 
results_2gether <- list()
results_2gether_deviation <- c()



###########new try
for (FILES in 1:ncol(file_path_combinations)){
  print(FILES)
  #read in files
  FASTA_1 <- readRDS(file_path_combinations[1,FILES])
  FASTA_1_raw <- FASTA_1[[1]]$FASTA
  # prepare fasta ids for getting the intersection
  fasta <- c()
  for (i in 1:length(FASTA_1_raw)){
    tmp <- strsplit(FASTA_1_raw[i],split='\\|')[[1]][2]
    fasta <- c(fasta,tmp)
  }
  fasta1 <- fasta
  
  FASTA_2 <- readRDS(file_path_combinations[2,FILES])
  FASTA_2 <- FASTA_2[[1]]$FASTA
  fasta <- c()
  for (i in 1:length(FASTA_2)){
    tmp <- strsplit(FASTA_2[i],split='\\|')[[1]][2]
    fasta <- c(fasta,tmp)
  }
  fasta2 <- fasta
  
  #determine length for caluclation
  length_FASTA_1 <- length(FASTA_1_raw)
  # which fasta ids at the first place are the same? -> this ids should be removed in the comparison
  
  FASTA_1_raw <- FASTA_1_raw[-which(fasta1 %in% fasta2)]
  
  
  FASTA <- str_match_all(FASTA_1_raw, "\\|(.*?)\\|")
  res<-list()
  for (fasta in 1:length(FASTA_1_raw)){
    res[[fasta]]<-FASTA[[fasta]][-1,2]
  }
  FASTA_1 <- res #this FASTA file is used to search for overlooked identifications

  result <- c()
  for (i in 1:length(FASTA_1)){
    if (any(str_detect(FASTA_2,paste0("\\b(", paste(FASTA_1[[i]], collapse="|"), ")\\b"))) && length(FASTA_1[[i]])>0){
      result<-c(result,i)
    }
  }

  results_2gether[[FILES]] <- result
  results_2gether_deviation <- c(results_2gether_deviation,
                                 ((length(result))/mean(length_FASTA_1,length(FASTA_2))*100))
}

boxplot(results_2gether_deviation)

# schritt 1 finden von intersections und die bei der analyse oben einfache weglassen!
which(intersect(fasta1,fasta2))
t <- which(fasta2 %in% fasta2)

t<-FASTA_1_raw[which(fasta2 %in% fasta1)]
a1 <- c("a","b","c","d")
a2 <- c("c","e","z","a")
which(a1 %in% a2)
intersect(a1,a2)
which(match(a1,a2))

t <- which(fasta1 %in% fasta2)
tt <- which(fasta2 %in% fasta1)


grep(paste('^',FASTA_1[[80]],'$',collapse="|"),FASTA_2)
grep(paste(FASTA_1[[159]],collapse="|"),FASTA_2)
#https://stackoverflow.com/questions/69258011/r-exact-match-for-multiple-patterns

for (i in result){
  t<-str_detect(FASTA_2,paste0("\\b(", paste(FASTA_1[[i]], collapse="|"), ")\\b"))
  FASTA_1[i]
  print(FASTA_1_raw[i])
  print(FASTA_2[which(t)])
  print("##################################################")
}


tt <- which(a %in% FASTA_2)
a<-FASTA_1_raw[-tt]
