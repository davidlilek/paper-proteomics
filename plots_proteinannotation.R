tmp <- c(rep("D1 vs. D2",3),
  rep("2D vs. 3D",3),
  rep("D1 frac/pool",3),
  rep("D2 frac/pool",3))
df <- data.frame(
  Category = rep(tmp, times = 3),
  Type = c(rep(c("bp", "mf", "cc"), each = 12)),
  Percentage = c(round(results_comp_bp*100,1),
                 round(results_comp*100,1),
                 round(results_comp_cc*100,1)),
  Variable = rep(c("D1/D2", "D1", "D2",
               "2D/3D", "2D", "3D",
               "D1 frac/pool", "D1 frac", "D1 pool",
               "D2 frac/pool", "D2 frac", "D2 pool"),3)
)

df_2 <- data.frame(
  Category = rep(tmp, times = 3),
  Type = c(rep(c("biological process", "molecular funciton", "cellular compartment"), each = 12)),
  Percentage = c(round(results_comp_bp*100,1),
                 round(results_comp*100,1),
                 round(results_comp_cc*100,1)),
  Variable = rep(c("D1-D2", "D1", "D2",
                   "2D-3D", "2D", "3D",
                   "D1 frac-pool", "D1 frac", "D1 pool",
                   "D2 frac-pool", "D2 frac", "D2 pool"),3)
)

df_2$Category <- factor(df_2$Category, levels = c("D1 vs. D2", "2D vs. 3D", "D1 frac/pool", "D2 frac/pool"))

desired_order <- c("D1", "2D", "D1 frac", "D2 frac",
                   "D1-D2", "2D-3D", "D1 frac-pool", "D2 frac-pool", "D2", "3D", "D1 pool", "D2 pool")


# Create the stacked bar plot with smaller text labels
ggplot(df_2, aes(x = Category, y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.15) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),
            size = ifelse(df_2$Variable == "2D" | df_2$Variable == "D1 pool" | df_2$Variable == "D2 pool", 1.5, 3)) +  # Adjust the size here
  facet_grid(Type ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "",
       y = "Percentage",
       fill = "") + 
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(size=0.5, linetype="solid"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        strip.text = element_text(margin = margin(20, unit = "pt"))) + # Adjust the margin value as per your requirement)
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  scale_fill_manual(values = custom_colors, breaks = desired_order)


#col X - D1
#dark: total protein extract
#light: fractionation
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
col_D1D2 <- rgb(160,0,108, maxColorValue = 255)
col_D1frac_pool <- rgb(231,63,52, maxColorValue = 255)
col_D2frac_pool <- rgb(67,59,229, maxColorValue = 255)

# Create a vector of custom colors for each "Variable" level
custom_colors <- c(
  "D1-D2" = col_D1D2,
  "D1" = col_X_dark,
  "D2" = col_Y_dark,
  "2D-3D" = "darkgreen",
  "2D" = "lightgreen",
  "3D" = "green",
  "D1 frac-pool" = col_D1frac_pool,
  "D1 frac" = col_X_light,
  "D1 pool" = col_X_dark,
  "D2 frac-pool" = col_D2frac_pool,
  "D2 frac" = col_Y_light,
  "D2 pool" = col_Y_dark)



tmp <- c(rep("D1 vs. D2",3),
         rep("2D vs. 3D",3),
         rep("D1 frac/pool",3),
         rep("D2 frac/pool",3))

df_2 <- data.frame(
  Category = rep(tmp, times = 3),
  Type = c(rep(c("biological process", "molecular funciton", "cellular compartment"), each = 12)),
  Percentage = c(round(results_comp_bp*100,1),
                 round(results_comp*100,1),
                 round(results_comp_cc*100,1)),
  Variable = rep(c("B", "A", "C",
                   "E", "D", "F",
                   "H", "G", "I",
                   "K", "J", "L"),3)
)


df_2$Category <- factor(df_2$Category, levels = c("D1 vs. D2", "2D vs. 3D", "D1 frac/pool", "D2 frac/pool"))

desired_order_2 <- c("B"="D1-D2","A"="D1",
                     "C"="D2",
                     "E"="2D-3D", "D"="2D", "F"="3D",
                     "H"="D1 frac-pool", "G"="D1 frac", "I"="D1 pool",
                     "K"="D2 frac-pool", "J"="D2 frac", "L"="D2 pool")

desired_order <- c("A", "D", "G", "J",
                   "B", "E", "H", "K",
                   "C", "F", "I", "L")




# Create the stacked bar plot with smaller text labels
ggplot(df_2, aes(x = Category, y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.15) +
  facet_grid(Type ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "",
       y = "Percentage",
       fill = "") + 
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(size=0.5, linetype="solid"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        strip.text = element_text(margin = margin(20, unit = "pt"))) + # Adjust the margin value as per your requirement)
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  scale_fill_manual(values = custom_colors, breaks = desired_order, labels = desired_order_2) +
  geom_bar_text(aes(label = paste0(Percentage,"%")),
                position = "stack",
                reflow = FALSE,
                min.size = 1,
                place = "middle",
                outside = TRUE,
                contrast = TRUE)



#col X - D1
#dark: total protein extract
#light: fractionation
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
col_D1D2 <- rgb(160,0,108, maxColorValue = 255)
col_D1frac_pool <- rgb(231,63,52, maxColorValue = 255)
col_D2frac_pool <- rgb(67,59,229, maxColorValue = 255)

# Create a vector of custom colors for each "Variable" level
custom_colors <- c(
  "B" = col_D1D2,
  "A" = col_X_dark,
  "C" = col_Y_dark,
  "E" = "darkgreen",
  "D" = "lightgreen",
  "F" = "green",
  "H" = col_D1frac_pool,
  "G" = col_X_light,
  "I" = col_X_dark,
  "K" = col_D2frac_pool,
  "J" = col_Y_light,
  "L" = col_Y_dark)



