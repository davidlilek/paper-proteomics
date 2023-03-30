#get colors
#total protein extract
total <- rgb(255,93,93, maxColorValue = 255)
#fractions high MW SEC
highMW <- rgb(169,208,142,maxColorValue = 255)
#fractions low MW SEC
lowMW <- rgb(255,217,102, maxColorValue = 255)
#in-gel digest
IG <- rgb(180,198,231,maxColorValue = 255)
################# pool_G with pool_20
#venn digarams
#get colors
D1_col <- rgb(255,93,93, maxColorValue = 255)
D2_col <- rgb(169,208,142,maxColorValue = 255)
################# D2 
#venn digarams
#get colors
D2_pool_col <- rgb(255,93,93, maxColorValue = 255)
D2_ingel_col <- rgb(169,208,142,maxColorValue = 255)
################# 
E4_col <- rgb(255,93,93, maxColorValue = 255)
E5_col <- rgb(169,208,142,maxColorValue = 255)

colors <- as.data.frame(rbind(total, highMW, lowMW, IG,
                    D1_col, D2_col,
                    D2_pool_col,D2_ingel_col,
                    E4_col,E5_col))
colnames(colors) <- "rgb"
colors$names <- rownames(colors)



# color with 5 different colors
barplot(rep(10,nrow(colors)), col=colors$rgb,
        ylab = "",
        names=colors$names,
        cex.names=0.5,
        horiz=T,
        las=1)
