.plot = function (metric = "unknown", plot_output = "Figures/", long_data = NULL, cutoff = 0, min = 0,
                  cbPalette = NULL, custom_names = NULL) 
{
  
  ## Divide the data into subsets for plotting/mapping ##
  overMin <- long_data[long_data$Value > min,]
  low <- long_data[long_data$Value <= cutoff,]
  high <- long_data[long_data$Value > cutoff,]
  
  setwd(plot_output)
  ## Histograms ##
  jpeg(paste0(metric,"_hist.jpeg"), width = 1800, height = 900, units = 'px', res = 100)
  hist(long_data$Value, breaks = 25, xlab = metric, 
       main = paste0("All ", metric))
  dev.off()
  
  jpeg(paste0(metric,"_High_hist.jpeg"), width = 1800, height = 900, units = 'px', res = 100)
  hist(high$Value, breaks = 25, xlab = metric, 
       main = paste0(paste0(metric, " >"), as.character(cutoff)))
  dev.off()
  
  jpeg(paste0(metric,"_Low_hist.jpeg"), width = 1800, height = 900, units = 'px', res = 100)
  hist(low$Value, breaks = 25, xlab = metric, 
       main = paste0(paste0(metric, " \u2264"), as.character(cutoff)))
  dev.off()
  
  jpeg(paste0(metric,"_overMin_hist.jpeg"), width = 1800, height = 900, units = 'px', res = 100)
  hist(overMin$Value, breaks = 25, xlab = metric, 
       main = paste0(paste0(metric, " >"), as.character(min)))
  dev.off()
  
  # plot labels
  if (!is.null(custom_names)) {
    plot_labels = custom_names
  } else {
    plot_labels = levels(long_data$type)
  }
  
  cat("Plot labels will be: ", paste(plot_labels, collapse = "\n"))

  ## violin plots ##
  jpeg(paste0(metric,"_overMin_violin.jpeg"), width = 1800, height = 900, units = 'px', res = 100)
  print({
    ggplot() + geom_violin(data = overMin, aes(x = overMin$type, y = overMin$Value, color = overMin$type)) + 
      scale_x_discrete(limits = levels(overMin$type), labels = plot_labels) + 
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +  
      scale_color_manual(values = cbPalette) + 
      scale_fill_manual(values = cbPalette)+
      labs(x = "Run Type", y = metric, 
           title = eval(substitute(paste0(paste0(metric, " >"),v), list(v=min)))) + 
      scale_color_manual(values = cbPalette)
  })
  dev.off()
  
  jpeg(paste0(metric,"_Low_violin.jpeg"), width = 1800, height = 900, units = 'px', res = 100)
  print({
    ggplot() + geom_violin(data = low, aes(x = low$type, y = low$Value, color = type)) + 
      scale_x_discrete(limits = levels(low$type), labels = plot_labels) + theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +  
      scale_color_manual(values = cbPalette) + 
      scale_fill_manual(values = cbPalette)+
      labs(x = "Run Type", y = metric,
           title=eval(substitute(paste0(paste0(metric, " \u2264"),v), list(v=cutoff)))) 
  })
  dev.off()
  
  jpeg(paste0(metric,"_High_violin.jpeg"), width = 1800, height = 900, units = 'px', res = 100)
  print({
    ggplot() + geom_violin(data = high, aes(x = high$type, y = high$Value, color = type)) + 
      scale_x_discrete(limits = levels(high$type), labels = plot_labels) + theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +  
      scale_color_manual(values = cbPalette) + 
      scale_fill_manual(values = cbPalette)+
      labs(x = "Run Type", y = metric,
           title=eval(substitute(paste0(paste0(metric, " >"),v, " timesteps"), list(v=cutoff))))
  })
  dev.off()
}
