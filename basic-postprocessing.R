#!/usr/bin/Rscript

###############################################################################################################
# R script which reads in the file-path from run-maxquant.sh
# e.g. /proj/proteomics/11_20220713_FH/results/results_run1_mqpar_20220713_QC
# based on this path post-processing.Rmd file is rendered and saved in /proj/proteomics/<proj name>/evaluation
###############################################################################################################

# load libraries 
library(rmarkdown)

# render post-processing script
# args[6] is the path defined in the bash script
# e.g. /proj/proteomics/11_20220713_FH/results/results_run1_mqpar_20220713_QC_delete
# pth <- "/proj/proteomics/11_20220713_FH/results/results_run1_mqpar_20220713_QC_delete"
pth <- "X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/proteinGroups"
filelist = list.files(path = pth, pattern = ".*.txt")

for (file in filelist){
  print(file)
  rmarkdown::render("X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/post-processing.Rmd", 
                    params = list(
                      path = paste(pth,"/",file,sep=""),
                      pathRDS = paste("X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/results/",file,sep="")),
                    output_dir = "X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/results",
                    output_file = paste(file,"_post-processing.html",sep=""))
}
