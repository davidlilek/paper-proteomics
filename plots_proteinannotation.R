tmp <- c(rep("2A",3),
  rep("2C",3),
  rep("3B",3),
  rep("3C",3))
df <- data.frame(
  Category = rep(tmp, times = 3),
  Type = c(rep(c("bp", "mf", "cc"), each = 12)),
  Percentage = c(round(results_comp_bp*100,1),
                 round(results_comp*100,1),
                 round(results_comp_cc*100,1)),
  Variable = rep(c("D1+D2", "D1", "D2",
               "2D+3D", "2D", "3D",
               "D1frac+pool", "D1 frac", "D1 pool",
               "D2frac+pool", "D2 frac", "D2 pool"),3)
)

df_2 <- data.frame(
  Category = rep(tmp, times = 3),
  Type = c(rep(c("biological process", "molecular funciton", "cellular compartment"), each = 12)),
  Percentage = c(round(results_comp_bp*100,1),
                 round(results_comp*100,1),
                 round(results_comp_cc*100,1)),
  Variable = rep(c("D1+D2", "D1", "D2",
                   "2D+3D", "2D", "3D",
                   "D1frac+pool", "D1 frac", "D1",
                   "D2frac+pool", "D2 frac", "D2"),3)
)
# Create the stacked bar plot with smaller text labels
ggplot(df_2, aes(x = Category, y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 3) +  # Adjust the size here
  facet_grid(Type ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "Category",
       y = "Percentage",
       fill = "Legend")

# Create the stacked bar plot with smaller text labels and the legend at the bottom
ggplot(df_2, aes(x = Category, y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ Type, scales = "free", nrow = 1) +  # All plots in one row
  theme_minimal() +
  labs(x = "Category",
       y = "Percentage",
       fill = "Variable") +
  theme(legend.position = "bottom", legend.box = "horizontal")


