############################################################################################################
#
# PRCC & Single model comparison panel
#
############################################################################################################
library(ggpubr)

plotOne <- prcc_percentIncrease +
  ggtitle("a)") +
  theme(plot.title = element_text(size = 30))

plotTwo <- SingleModel_PercentIncrease  + 
  ggtitle("b)") + 
  theme(plot.title = element_text(size = 30))

setwd(path_output)
jpeg(filename = "PRCC_SingleMod_PercentIncrease_Comparison.jpeg", width = 1440, height = 1200, units = 'px', res = 100)
ggarrange(plotOne, plotTwo, ncol = 1)
dev.off()

############################################################################################################
#
# PRCC & Single takeoff model comparison panel
#
############################################################################################################
plotOne <- prcc_percentIncrease_takeoff +
  ggtitle("a)") +
  theme(plot.title = element_text(size = 30))

plotTwo <- SingleModel_PercentIncrease_takeoff+ 
  ggtitle("b)") + 
  theme(plot.title = element_text(size = 30))

setwd(path_output)
jpeg(filename = "PRCC_SingleMod_PercentIncrease_Takeoff_Comparison.jpeg", width = 1440, height = 1200, units = 'px', res = 100)
ggarrange(plotOne, plotTwo, ncol = 1)
dev.off()

############################################################################################################
#
# PRCC & Single takeoff model comparison panel
#
############################################################################################################
plotOne <- prcc_percentIncrease_fadeout +
  ggtitle("a)") +
  theme(plot.title = element_text(size = 30))

plotTwo <- SingleModel_PercentIncrease_fadeout+ 
  ggtitle("b)") + 
  theme(plot.title = element_text(size = 30))

setwd(path_output)
jpeg(filename = "PRCC_SingleMod_PercentIncrease_fadeout_Comparison.jpeg", width = 1440, height = 1200, units = 'px', res = 100)
ggarrange(plotOne, plotTwo, ncol = 1)
dev.off()