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
TRESHHOLD <- 2
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_D2_MBR_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]
data <- data[,-c(1:4)]
data <- data %>% mutate(sum = rowSums(across(where(is.logical))))
rows <- data$sum>=TRESHHOLD

#rle 
#https://stackoverflow.com/questions/49664804/finding-rows-containing-more-than-two-sequential-true-logical-values
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_D2_MBR_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]     
Df <- data[,-c(1:4)]
Df <- data[,-ncol(Df)]
#get rows with a least one TRUE value
rows <- apply(Df,1,any)
rows[is.na(rows)] <- FALSE
#re order data
Df_new <- Df[rows,c(1,2,5:21,3,4)]
rows <- apply(Df_new[, -ncol(Df_new)], 1, function(x)
  as.numeric(any(rle(x)$lengths >= 2 & rle(x)$values)))
#transform 0 and 1 values to true and false
rows <- rows==1
sum(rows)
### sort data














################count data in intersection diagram which occur 2 times
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_D2_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]
data_sub <- dplyr::select(data, contains("Pool_20"))
B_D2 <- data_sub*1
B_D2[B_D2==0] <- NA


### analysis B
proteins <- as.data.frame(which(!is.na(B_D2), arr.ind=TRUE))

B1 <- proteins %>% dplyr::filter(col == 1 | col == 2)

# count how often a value occurs 
# then filter als values which occur 2 times n==2
# then use only the first column
tmp <- B1 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)
B1[,1]











# D2

```{r}
#############################
# D2
###############################
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_D2_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]
data_sub <- dplyr::select(data, contains("Pool_20"))
B_D2 <- data_sub*1
B_D2[B_D2==0] <- NA


### analysis B
proteins <- as.data.frame(which(!is.na(B_D2), arr.ind=TRUE))

B1 <- proteins %>% dplyr::filter(col == 1 | col == 2) # | col == 2 -> means or col2
B10 <- proteins %>% dplyr::filter(col == 3 | col == 4)
B2 <- proteins %>% dplyr::filter(col == 5| col == 6)
B3 <- proteins %>% dplyr::filter(col == 7 | col == 8)
B4 <- proteins %>% dplyr::filter(col == 9 | col == 10)
B5 <- proteins %>% dplyr::filter(col == 11 | col == 12)
B6 <- proteins %>% dplyr::filter(col == 13| col == 14)
B7 <- proteins %>% dplyr::filter(col == 15 | col == 16)
B8 <- proteins %>% dplyr::filter(col == 17 | col == 18)
B9 <- proteins %>% dplyr::filter(col == 19 | col == 20)
# combine to list
lst <- list(B1 = as.double(B1 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B2 = as.double(B2 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B3 = as.double(B3 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B4 = as.double(B4 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B5 = as.double(B5 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B6 = as.double(B6 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B7 = as.double(B7 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B8 = as.double(B8 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B9 = as.double(B9 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)),
            B10 = as.double(B10 %>% count(row) %>% dplyr::filter(n == 1 | n == 2) %>% pull(row)))
