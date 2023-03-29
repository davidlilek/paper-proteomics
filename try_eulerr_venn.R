

############## B
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_B_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]
data_sub <- dplyr::select(data, contains("Pool_G"))
#get rid of FALSE values
B <- data_sub*1
B[B==0] <- NA
ind <- apply(B, 1, function(x) all(is.na(x)))


############AF
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_AF_TF_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]
data_sub <- dplyr::select(data, contains("Pool_AF"))
#get rid of FALSE values
AF <- data_sub*1
AF[AF==0] <- NA
ind <- apply(AF, 1, function(x) all(is.na(x)))

############TF
data <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_AF_TF_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
data <- data[[1]]
data_sub <- dplyr::select(data, contains("Pool_TF"))
#get rid of FALSE values
TF <- data_sub*1
TF[TF==0] <- NA
ind <- apply(TF, 1, function(x) all(is.na(x)))


## compare AF TF B poster
AF_TF_raw <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_AF_TF_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
AF_TF_raw <- AF_TF_raw[[1]]
AF <- dplyr::select(AF_TF_raw, contains("AF"))
AF <- apply(AF, 1, any)
fasta <- c()
for (i in 1:length(AF_TF_raw$FASTA)){
  tmp <- strsplit(AF_TF_raw$FASTA[i],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}
AF <- as.data.frame(cbind(AF, fasta))
TF <- dplyr::select(AF_TF_raw, contains("TF"))
TF <- apply(TF, 1, any)
TF <- as.data.frame(cbind(TF, fasta))
B_raw <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_B_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
B_raw <- B_raw[[1]]
B <- dplyr::select(B_raw, contains("Pool_G"))
B <- apply(B, 1, any)
fasta <- c()
for (i in 1:length(B_raw$FASTA)){
  tmp <- strsplit(B_raw$FASTA[i],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}
B <- as.data.frame(cbind(B, fasta))

AF_TF_B_comparison  <- merge(B, AF,
                             all = TRUE,
                             by = "fasta")
AF_TF_B_comparison <- merge(AF_TF_B_comparison, TF,
                            all = TRUE,
                            by = "fasta")

AF_TF_B_comparison$B <- as.logical(AF_TF_B_comparison$B)
AF_TF_B_comparison$AF <- as.logical(AF_TF_B_comparison$AF)
AF_TF_B_comparison$TF <- as.logical(AF_TF_B_comparison$TF)
AF_TF_B_comparison <- AF_TF_B_comparison[,-1]*1
AF_TF_B_comparison[AF_TF_B_comparison==0] <- NA

# boxplot
boxplot(sum(AF_TF_B_comparison$B,na.rm = TRUE),
        sum(AF_TF_B_comparison$AF,na.rm = TRUE),
        sum(AF_TF_B_comparison$TF,na.rm = TRUE),
        names = c("B","AF","TF"))

proteins <- as.data.frame(which(!is.na(AF_TF_B_comparison), arr.ind=TRUE))

B <- proteins %>% dplyr::filter(col == 1)
AF <- proteins %>% dplyr::filter(col == 2)
TF <- proteins %>% dplyr::filter(col == 3)


# combine to list
lst <- list("B"=as.double(B[,1]),
            "AF"=as.double(AF[,1]),
            "TF"=as.double(TF[,1]))

upset(fromList(lst), 
      order.by = "freq",
      nsets = 10,
      nintersects = 15,
      keep.order = TRUE,
      sets=c("B", "AF", "TF"),
      mainbar.y.label = "Number of shared proteins",
      sets.x.label = "Number of proteins per fraction",
      set_size.show = TRUE,
      set_size.angles = 0,
      set_size.scale_max = 1800
)


#get colors
#total protein extract
total <- rgb(255,93,93, maxColorValue = 255)
#fractions high MW SEC
highMW <- rgb(169,208,142,maxColorValue = 255)
#fractions low MW SEC
lowMW <- rgb(255,217,102, maxColorValue = 255)
#in-gel digest
IG <- rgb(180,198,231,maxColorValue = 255)

ggvenn(lst,
       fill_color = c(IG,lowMW,highMW),
       stroke_size = 0.5, set_name_size = 5, text_size = 4.25, digits = 0, fill_alpha = 0.9, label_sep = )
#ggsave("./pics/unique_AF_TF_B.png")

################venn
grey <- grey_pal(0, 1)(25)
p<-plot(euler(lst),
        input="union",
        shape="ellipse",
        quantities=list(type = c("percent", 'counts'),
                        cex = c(0.5)), #schriftgröße für A, B, C bzw intersection
        key=TRUE,
        labels=FALSE,
        lty = 1:3,
        legend = list(labels = c("A", "B","C"),
                      side = "bottom",
                      nrow = 1, ncol = 3),
        edges=TRUE,
        fills = c(IG,lowMW,highMW), #color für A,B bzw intersection fills = grey[c(10,18,25)]
        alpha=0.8)
p
# https://stackoverflow.com/questions/75177293/how-to-change-the-label-position-when-plotting-venn-diagram-from-eulerr-package
tags <- v$children[[1]]$children[[1]]$children$tags$children
tags <- p$children[[2]]$children[[1]]$children$tags$children
tags <- do.call(grid::gList, lapply(tags, function(x) {
  x$children[[2]]$label <- sub(" \\(", "\n(", x$children[[2]]$label)
  x$children[[2]]$just <- NULL
  x$children[[2]]$hjust <- 0.5
  x$children[[2]]$vjust <- 0.5
  x}))

p$children[[2]]$children[[1]]$children$tags$children <- tags
p



#compare B to pool from 0609 
B_raw <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_B_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
B_raw <- B_raw[[1]]
B <- dplyr::select(B_raw, contains("Pool_G"))
B <- apply(B, 1, any)
fasta <- c()
for (i in 1:length(B_raw$FASTA)){
  tmp <- strsplit(B_raw$FASTA[i],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}
B <- as.data.frame(cbind(B, fasta))
aj_raw <- readRDS(file = "results_v002/results_run1_mqpar_extracts_2gether_E4_D1_a-j_LFQ_combined_txt_proteinGroups.txt_Unique.RDS")
aj_raw <- aj_raw[[1]]
aj <- dplyr::select(aj_raw, contains("Pool"))
aj <- apply(aj, 1, any)
fasta <- c()
for (i in 1:length(aj_raw$FASTA)){
  tmp <- strsplit(aj_raw$FASTA[i],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}
aj <- as.data.frame(cbind(aj, fasta))
B_aj_comparison  <- merge(B, aj,
                          all = TRUE,
                          by = "fasta")

B_aj_comparison$B <- as.logical(B_aj_comparison$B)
B_aj_comparison$aj <- as.logical(B_aj_comparison$aj)
B_aj_comparison <- B_aj_comparison[,-1]*1
B_aj_comparison[B_aj_comparison==0] <- NA
boxplot(sum(B_aj_comparison$B,na.rm = TRUE),
        sum(B_aj_comparison$aj,na.rm = TRUE),
        names = c("B","E4_D1_pool"))

proteins <- as.data.frame(which(!is.na(B_aj_comparison), arr.ind=TRUE))

B_inter <- proteins %>% dplyr::filter(col == 1)
aj_inter <- proteins %>% dplyr::filter(col == 2)

# combine to list
lst <- list("B"=as.double(B_inter[,1]),
            "E4_D1_pool"=as.double(aj_inter[,1]))

library(scales)

grey <- grey_pal(0, 1)(25)
p<-plot(euler(lst),
     input="union",
     shape="ellipse",
     quantities=list(type = c("percent", 'counts'),
                     cex = c(1,0.65,1)), #schriftgröße für A, B bzw intersection
     key=TRUE,
     labels=FALSE,
     lty = 1:2,
     legend = list(labels = c("A", "B"),
                   side = "bottom",
                   nrow = 1, ncol = 3),
     edges=TRUE,
     fills = c(IG,total), #color für A,B bzw intersection fills = grey[c(10,18,25)]
     alpha=0.8)
p
# https://stackoverflow.com/questions/75177293/how-to-change-the-label-position-when-plotting-venn-diagram-from-eulerr-package
tags <- v$children[[1]]$children[[1]]$children$tags$children
tags <- p$children[[2]]$children[[1]]$children$tags$children
tags <- do.call(grid::gList, lapply(tags, function(x) {
  x$children[[2]]$label <- sub(" \\(", "\n(", x$children[[2]]$label)
  x$children[[2]]$just <- NULL
  x$children[[2]]$hjust <- 0.5
  x$children[[2]]$vjust <- 1
  x}))

p$children[[2]]$children[[1]]$children$tags$children <- tags
p

#grey
#https://scales.r-lib.org/reference/grey_pal.html
show_col(grey_pal()(25))

show_col(grey_pal(0, 1)(25))
label_venn <- c("A","B")
color_venn <- c(IG,total)
euler_venn_2comp(lst, label_venn = label_venn, color_venn = color_venn, cex_venn = c(1,0.65,1), h_just = 0.5, v_just = 0.5)
euler_venn_2comp <- function(lst, label_venn, color_venn, cex_venn, grid, gList, h_just, v_just) {
  ######################################
  #################plot 2 venn diagrams
  ######################################
  p<-plot(euler(lst),
          input="union",
          shape="ellipse",
          quantities=list(type = c("percent", 'counts'),
                          cex = cex_venn), #schriftgröße für A, B bzw intersection
          key=TRUE,
          labels=FALSE,
          lty = 1:2,
          legend = list(labels = label_venn,
                        side = "bottom",
                        nrow = 1, ncol = 3),
          edges=TRUE,
          fills = color_venn, #color für A,B bzw intersection 
          alpha=0.8)
  # https://stackoverflow.com/questions/75177293/how-to-change-the-label-position-when-plotting-venn-diagram-from-eulerr-package
  tags <- p$children[[2]]$children[[1]]$children$tags$children
  tags <- do.call(grid::gList, lapply(tags, function(x) {
    x$children[[2]]$label <- sub(" \\(", "\n(", x$children[[2]]$label)
    x$children[[2]]$just <- NULL
    x$children[[2]]$hjust <- h_just
    x$children[[2]]$vjust <- v_just
    x}))
  
  p$children[[2]]$children[[1]]$children$tags$children <- tags
  print(p)
}


euler_venn_3comp <- function(lst, cex_venn, label_venn, color_venn, grid, gList, h_just, v_just) {
  ##################################
  ###### venn diagram 3 comparisons
  ##################################
  p<-plot(euler(lst),
          input="union",
          shape="ellipse",
          quantities=list(type = c("percent", 'counts'),
                          cex = cex_venn), #schriftgröße für A, B, C bzw intersection
          key=TRUE,
          labels=FALSE,
          lty = 1:3,
          legend = list(labels = label_venn,
                        side = "bottom",
                        nrow = 1, ncol = 3),
          edges=TRUE,
          fills = color_venn, #color für A,B bzw intersection fills = grey[c(10,18,25)]
          alpha=0.8)
  # https://stackoverflow.com/questions/75177293/how-to-change-the-label-position-when-plotting-venn-diagram-from-eulerr-packages
  tags <- p$children[[2]]$children[[1]]$children$tags$children
  tags <- do.call(grid::gList, lapply(tags, function(x) {
    x$children[[2]]$label <- sub(" \\(", "\n(", x$children[[2]]$label)
    x$children[[2]]$just <- NULL
    x$children[[2]]$hjust <- h_just
    x$children[[2]]$vjust <- v_just
    x}))
  
  p$children[[2]]$children[[1]]$children$tags$children <- tags
  print(p)
}
