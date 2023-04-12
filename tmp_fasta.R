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

### all files
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

#combine all file path and analyze 
results_2gether <- list()
results_2gether_deviation <- c()
for (FILES in 1:ncol(file_path_combinations)){
  print(FILES)
  #read in files
  FASTA_1 <- readRDS(file_path_combinations[1,FILES])
  FASTA_1 <- FASTA_1[[1]]$FASTA
  FASTA_1 <- str_extract(FASTA_1,"(?<=tr\\|)[[:alnum:]]+") # due to problems using the whole FASTA header only the unique identifieres are used | #https://stackoverflow.com/questions/61082486/how-to-get-the-id-codes-from-a-fasta-file-using-r
  FASTA_2 <- readRDS(file_path_combinations[2,FILES])
  FASTA_2 <- FASTA_2[[1]]$FASTA
  FASTA_2 <- str_extract(FASTA_2,"(?<=tr\\|)[[:alnum:]]+")
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
  
  results_2gether[[FILES]] <- res_clear
  results_2gether_deviation <- c(results_2gether_deviation,
                                 (length(res_clear)/mean(length(FASTA_1),length(FASTA_2))*100))
}



library(stringr)
a <- " anything goes here, | GET_ME |, anything goes here"
a <- " anything goes here, STR1 GET_ME STR2, anything goes here STR1 GET_ME2 STR2"
res <- str_match_all(a, "STR1\\s*(.*?)\\s*STR2")
FASTA <- str_match_all(E3_FASTA, "\\|(.*?)\\|")
res
t<-as.data.frame(unlist(str_match_all(E3_FASTA, "\\|\\s*(.*?)\\s*\\|")))
res<-c()
for (fasta in 1:length(FASTA_1)){
  if (length(FASTA[[fasta]])>2){
    res<-c(res,(FASTA[[fasta]][-1,2]))
  }
}
  

