if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("pcaMethods")
library(pcaMethods)
## Load a sample metabolite dataset (metaboliteData) with already 5\% of
## data missing
data(metaboliteData)
## Perform llsImpute using k = 10
## Set allVariables TRUE because there are very few complete variables
result <- llsImpute(metaboliteData, k = 10, correlation="pearson", allVariables=TRUE)
## Get the estimated complete observations
cObs <- completeObs(result)
test <- as.data.frame(metaboliteData)

data <- B_raw[,-ncol(B_raw)]
data <- lapply(data, as.numeric)
data <- as.data.frame(data)
rows <- apply(data, 1, any)
rows[is.na(rows)] <- FALSE
data_clear <- data[rows,]
data_clear <- t(as.matrix(data_clear))
result <- llsImpute(data_clear, 
                    k = 100,
                    correlation="pearson",
                    allVariables=FALSE)
cObs <- completeObs(result)
data_clear_missing <- as.data.frame(t(cObs))


checkData(data_clear, verbose = TRUE)



# get sum of TRUE values in a row
# hängt an dataframe die spalte sum an wo eben die summe drinnensteht
tb %>% mutate(sum = rowSums(across(where(is.logical))))


#rle 
#https://stackoverflow.com/questions/49664804/finding-rows-containing-more-than-two-sequential-true-logical-values
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_D2_MBR_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]     
Df <- data[,-c(1:4)]
#get rows with a least one TRUE value
rows <- apply(Df_new,1,any)
rows[is.na(rows)] <- FALSE
#re order data
Df_new <- Df[rows,c(1,2,5:21,3,4)]

Df$sum <- apply(Df[, -ncol(Df)], 1, function(x)
  as.numeric(any(rle(x)$lengths >= 2 & rle(x)$values)))
sum(Df$sum)
