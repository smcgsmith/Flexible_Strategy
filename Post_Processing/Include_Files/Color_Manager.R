.load_colorPalette = function (plot_color = "color_red", map_color = "color_red", num_colors = NULL) {
  #Colors
  if (!is.null(num_colors)){
    if (plot_color == "color_blue") {
      cbPalette <- usecol(pal = c(rev(pal_seeblau), "steelblue", pal_pinky), n = num_colors, alpha = 1)   # define color palette from 3 colors
    } else {
      cbPalette <- c("#D55E00", "#CC79A7", "#56B4E9", "#009E73", "#0072B2", "#000000", "#F0E442", "#E69F00", "#E495A5") # Violin plots
      cbPalette <- colorRampPalette(cbPalette)(num_colors)
    }
  } else {
    #old post-processing color pallette - could rename new one to keep cbPalette original colors
    cbPalette <- c("#D55E00", "#CC79A7", "#56B4E9", "#009E73", "#0072B2", "#000000", "#F0E442", "#E69F00", "#E495A5") # Violin plots
  }

  if (map_color == "color_red") {palette = brewer.pal(8, "OrRd")
  } else if(map_color == "color_bluepurple"){palette = brewer.pal(8, "BuPu")
  } else if(map_color == "color_blue"){palette = brewer.pal(8, "Blues")
  } else if(map_color == "color_yellow"){palette = c("ivory3","lightgoldenrodyellow","lightgoldenrod", "gold2", "goldenrod", "darkgoldenrod") 
  } else if(map_color == "color_pink"){palette = brewer.pal(8, "RdPu")
  } else if(map_color == "color_orange"){palette = brewer.pal(8, "Oranges")}

    if (plot_color == "Sensitivity"){
      # Sensitivity plot bar colors
      blue<-c('#2171b5')
      # Sensitivity plot labels (PRCC)
      colors.short = c("black","black",
                       cbPalette[12],cbPalette[12],cbPalette[12],
                       cbPalette[12],cbPalette[12],cbPalette[12],
                       "black","black","black","black")
      
      # Sensitivity plot labels (models)
      colors.long = c("black","black","black",cbPalette[12],
                      cbPalette[12],cbPalette[12],cbPalette[12],
                      cbPalette[12],cbPalette[12],cbPalette[12], 
                      cbPalette[12],cbPalette[12],cbPalette[12],
                      cbPalette[12],cbPalette[12], cbPalette[12],
                      cbPalette[12],cbPalette[12], cbPalette[12],
                      "black","black","black")
      
      # Sensitivity plot labels (models)
      # colors.long.full = c("black","black","black",cbPalette[12],
      #                 cbPalette[12],cbPalette[12],cbPalette[12],
      #                 cbPalette[12],cbPalette[12],cbPalette[12], 
      #                 cbPalette[12],cbPalette[12],cbPalette[12],
      #                 cbPalette[12],cbPalette[12], cbPalette[12],
      #                 cbPalette[12],cbPalette[12], cbPalette[12],
      #                 "black","black","black")
    }
  return(list(plot_color = cbPalette, map_color = palette))
}
