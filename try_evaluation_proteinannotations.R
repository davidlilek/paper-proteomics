library(ggplot2)
library(reshape2)
library(dplyr)
# Create the DataFrame
data_d1 <- data.frame(
  Category = c("D1 to D2", "D1 to D2",
               "D1 to D1b", "D1 to D1b",
               "D1b to D2", "D1b to D2",
               "G to SEC", "G to SEC",
               "2D to 3D", "2D to 3D",
               "SEC low to high", "SEC low to high",
               "D1 pool to frac", "D1 pool to frac",
               "D2 pool to frac", "D2 pool to frac"),
  Percentage = c(79.6, 100 - 79.6,
                 100-9.8, 9.8,
                 100-3.9, 3.9,
                 100-2.8, 2.8,
                 100-16.6, 16.6,
                 100-6.7, 6.7,
                 100-47.9, 47.9,
                 100-37.4, 37.4),
  Variable = c("D1", "D2",
               "D1X", "D1b",
               "D1bX", "D2X",
               "G", "SEC",
               "2D", "3D",
               "SEC low", "SEC high",
               "D1 frac", "D1 pool",
               "D2 frac", "D2 pool")
)

col3 <- rgb(255,217,102, maxColorValue = 255)
col_X_dark <- rgb(204,0,0, maxColorValue = 255)
col_X_light <- rgb(255,100,100,maxColorValue = 255)
#fractions high MW SEC
col_X_lowMW  <- rgb(255,150,150,maxColorValue = 255)
#fractions low MW SEC
col_X_highMW <- rgb(255,230,230, maxColorValue = 255)
col_X_carbonate <- rgb(255,170,170, maxColorValue = 255)
# D2
col_Y_dark <- rgb(0,0,204, maxColorValue = 255)
col_Y_light <- rgb(100,100,255, maxColorValue = 255)

custom_colors <- c("D1" = col_X_dark,
                   "D1X" = col_X_dark,
                   "D2" = col_Y_dark,
                   "D2X" = col_Y_dark,
                   "D1b" = col3,
                   "D1bX" = col3,
                   "G" = col_X_light,
                   "2D" = "azure4",
                   "3D" = "azure2",
                   "SEC" = col_X_highMW,
                   "SEC low" = col_X_lowMW,
                   "SEC high" = col_X_highMW,
                   "D1 frac" = col_X_light,
                   "D1 pool" = col_X_dark,
                   "D2 frac" = col_Y_light,
                   "D2 pool" = col_Y_dark)


#data_d1 <- data_d1[order(-data_d1$Percentage), ]
d <- data_d1
d$Variable <- as.factor(d$Variable)
d$Color <- c(col_Y_dark, col_X_dark,
             col_Y_dark, col3, 
             col3, col_Y_dark,
             col_X_highMW, col_X_light,
             "azure4", "azure2",
             col_X_lowMW, col_X_highMW,
             col_X_dark, col_X_light,
             col_Y_dark, col_Y_light)
d$Variable <- factor(d$Variable, levels=c("D2", "D1",
                                          "D1b", "D1X",
                                          "D2X", "D1bX",
                                          "3D", "2D",
                                          "SEC", "G",
                                          "SEC high", "SEC low",
                                          "D1 pool", "D1 frac",
                                          "D2 pool", "D2 frac"))



d$Category <- factor(d$Category, levels=c("D1 to D2",
                                           "D1 to D1b",
                                           "D1b to D2",
                                           "G to SEC",
                                           "2D to 3D",
                                           "SEC low to high",
                                           "D1 pool to frac",
                                           "D2 pool to frac"))


             
  
 
ggplot(d, aes(x = Category, y = Percentage, fill = Variable, label = Percentage)) + 
  geom_bar(stat = "identity", position = "stack") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Stacked Bar Plot", x = "Category", y = "Percentage") + 
  scale_fill_manual(values = d$Color) + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), fontface = "bold")

#############################################################################################################
library(ggplot2)
library(reshape2)
# Create the DataFrame
data_d1 <- data.frame(
  Category = c("D1 to D2", "D1 to D2",
               "D1 to D1b", "D1 to D1b",
               "D1b to D2", "D1b to D2",
               "2D to 3D", "2D to 3D",
               "G to SEC", "G to SEC",
               "D1 pool to frac", "D1 pool to frac",
               "D2 pool to frac", "D2 pool to frac"),
  Percentage = c(79.6, 100-79.6,
                 9.8,100 - 9.8,
                 3.9,100 - 3.9,
                 16.6,100 - 16.6,
                 2.8,100 - 2.8, 
                 47.9,100 - 47.9,
                 37.4,100 - 37.4),
  Variable = c("D1", "D2",
               "D1b", "D1",
               "D2", "D1b",
               "SEC", "G",
               "3D", "2D",
               "D1", "D1 frac",
               "D2", "D2 frac")
)

col3 <- rgb(255,217,102, maxColorValue = 255)
col_X_dark <- rgb(204,0,0, maxColorValue = 255)
col_X_light <- rgb(255,100,100,maxColorValue = 255)
#fractions high MW SEC
col_X_lowMW  <- rgb(255,150,150,maxColorValue = 255)
#fractions low MW SEC
col_X_highMW <- rgb(255,230,230, maxColorValue = 255)
col_X_carbonate <- rgb(255,170,170, maxColorValue = 255)
# D2
col_Y_dark <- rgb(0,0,204, maxColorValue = 255)
col_Y_light <- rgb(100,100,255, maxColorValue = 255)

custom_colors <- c("D1" = col_X_dark,
                   "D2" = col_Y_dark,
                   "D1b" = col3,
                   "G" = col_X_light,
                   "2D" = "azure4",
                   "3D" = "azure2",
                   "SEC" = col_X_highMW,
                   "D1 frac" = col_X_light,
                   "D2 frac" = col_Y_light)

#data_d1 <- data_d1[order(-data_d1$Percentage), ]
d <- data_d1
d$Category <- factor(d$Category, levels=c("D1 to D2",
                                          "D1 to D1b",
                                          "D1b to D2",
                                          "2D to 3D",
                                          "G to SEC",
                                          "D1 pool to frac",
                                          "D2 pool to frac"))


ggplot(d, aes(x = Category, y = Percentage, fill = reorder(Variable), label = Percentage)) + 
  geom_bar(stat = "identity", position = "stack") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Stacked Bar Plot", x = "Category", y = "Percentage") + 
  scale_fill_manual(values = custom_colors) + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), fontface = "bold")







