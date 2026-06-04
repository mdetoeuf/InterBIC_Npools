plot_pca <- function(site_data, variable_data, PC_percent) {
  plot <- site_data |> 
    ggplot(aes(x = PC1, y = PC2)) +
    theme_minimal() +
    geom_segment(
      data = variable_data,
      aes(xend = PC1, yend = PC2, x = 0, y =0),
      colour = "grey30", linetype = 1,
      arrow = arrow(length = unit(0.03, "npc"))
    ) +
    geom_text(
      data = variable_data,
      aes(x = PC1*0.7, y = PC2*1.1, label = variable),
      colour = "grey30"
    ) +
    #geom_point() +
    xlab(paste0("PC1 (", signif(PC_percent[1]*100, digits = 3), "%)")) +
    ylab(paste0("PC2 (", signif(PC_percent[2]*100, digits = 3), "%)")) 
  
  return(plot)
}
