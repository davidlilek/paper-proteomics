library(reshape2)
library(plotly)

pth <- "X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/ta_yeast/results/"
filelist = list.files(path = pth, pattern = ".*.RDS")

for (file in filelist[grep("Razor",filelist)]){
  tmp <- readRDS(paste(pth,file,sep=""))
  tmp <- tmp[[1]]
  
  dat <- tmp
  yeasx <- grep("YEASX",dat$FASTA)
  wheat <- grep("WHEAT",dat$FASTA)
  # write human oder trica in a column
  for (i in 1:nrow(dat)){
    if (grepl("YEASX",dat$FASTA[i])){
      dat$org[i] <- "YEASX"
    }
    else if (grepl("WHEAT",dat$FASTA[i])){
      dat$org[i] <- "WHEAT"
    }
    else {
      dat$org[i] <- NA
    }
  }
  
  dat$org[intersect(yeasx,wheat)] <- "YEASX/WHEAT"
  
  dat4plot <- dat[,-c(ncol(dat)-1)]
  dat4plot_long <- melt(dat4plot, id = c("org"))
  tmp <- regmatches(dat4plot_long$variable,gregexpr("(?<=des.).*",dat4plot_long$variable,perl=TRUE))
  tmp <- unlist(tmp)
  dat4plot_long$variable <- tmp
  
  p <- ggplot(data=dat4plot_long, aes(x=variable, y=as.numeric(value), fill=org)) +
    stat_summary(fun.y = sum, geom = "bar", position = "dodge") +
    xlab("sample name") +
    ylab("counts") +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = file)
  p <- ggplotly(p)
  print(p)
  
  for (i in 1:(ncol(dat)-2)){
    cat("\n")
    text <- paste(" Fasta Headers - Sample",unique(tmp[i]))
    cat("##",text,"\n")
    cat("\"n")
    print(dat$FASTA[which(dat[,i])])
  }
  
}






# compare TC - ta/wheat
res_TC <- c(
  "results_run1_mqpar_extracts_2gether_E3_D1_combined_txt_proteinGroups.txt_summary.csv",
  "results_run1_mqpar_extracts_2gether_E4_D1_a-j_combined_txt_proteinGroups.txt_summary.csv",
  "results_run1_mqpar_extracts_2gether_D2_combined_txt_proteinGroups.txt_summary.csv",
  "results_run1_mqpar_extracts_2gether_E4_D1_B_combined_txt_proteinGroups.txt_summary.csv",
  "results_run1_mqpar_extracts_2gether_E4_D1_AF_TF_combined_txt_proteinGroups.txt_summary.csv")

res_ta_yeast <- c(
  "E3_D1_ta_yeast_summary.csv",
  "E4_D1_a_j_ta_yeast_summary.csv",
  "D2_ta_yeast_summary.csv",	
  "E4_D1_B_ta_yeast_summary.csv",
  "E4_D1_AF_TF_ta_yeast_summary.csv")

name_comp <- c(
  "E3_D1",
  "E4_D1_a_j",
  "D2",
  "E4_D1_B",
  "E4_D1_AF_TF")

for (i in 1:length(name_comp)){
  TC <- read.csv2(paste("../results/",res_TC[i],sep=""))
  ta_yeast <- read.csv2(paste("./results/",res_ta_yeast[i],sep=""))
  comp <- ta_yeast/TC*100
  comp <- melt(comp[,c(2:25)])
  
  p <- ggplot(comp,aes(x = variable, y = value)) +
    geom_boxplot() + 
    xlab("Sample name") + 
    ylab("% Wheat/Triticum") +
    scale_x_discrete(labels = unique(tmp)) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = name_comp[i])
  p <- ggplotly(p)
  print(p)
}


test<-readRDS("X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/results/results_run1_mqpar_extracts_2gether_D2_combined_txt_proteinGroups.txt_Razor.RDS")
fsta <- test[[1]]$FASTA


fsta[grep("Histone",fsta)]



pth <- "X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/ta_yeast/results/"
filelist = list.files(path = pth, pattern = ".*.RDS")
for (file in filelist[grep("Razor",filelist)]){
  print(file)
}
