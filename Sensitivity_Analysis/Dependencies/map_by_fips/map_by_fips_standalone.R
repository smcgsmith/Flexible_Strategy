
#' Plot US Maps by County FIPS Code
#'
#' Plot national maps of the US (or a subset of states) with regions filled based on input dataset.
#'
#' @param data.to.map dataframe with two columns: FIPS codes and continuous values for plotting
#' @param state.border.col Use NA to remove state borders.
#' @param state.border.width
#' @param county.border.col Use NA to remove county borders.
#' @param state.to.plot Specify state name(s) in order to map only those states.
#' @param map.resolution Shapefiles of different resolution are available.  Specify "low", "medium", "high", "TL".
#' @param shapefile.year Specify either 2016 or 2010. Default is 2010.
#' @param continental.us Specify TRUE or FALSE to plot only continental US.
#' @param color.sequence Sequence of colors used to plot.
#' @param color.break.type Options are: quantiles, values, range.
#' @param color.break.values When legend.break.type is quantiles or values, a specify a vector to use as color breaks.
#'    When legend.break.type is range, then specify a vector containing three values: number of breaks, min of range, max of range.
#' @param missing.include Counties missing from the dataset (or NA) will be included as counties with 0 on the map.
#' @param color.break.digits The number of digits to use when rounding the color break values.
#' @param mask.fips Vector of FIPS codes for counties with values that should not be displayed on the map.
#' @param mask.color Default is black.
#' @param print.fips Will display the county FIPS codes at each county's centroid.
#' @param print.vals Will display the county value at each county's centroid.
#' @param plot.margins Reset the plot margins.  Default is c(1,1,1,4).
#' @param legend.spacing Spacing of the legend from the right side.
#' @param legend.digits The number of digits to include in legend.
#' @param scientific Notation to use in the legend (TRUE / FALSE).
#' @param remove.legend Indicates whether legend should be included.
#' @param legend.shrink An argument for legend control within image.plot(): Amount to shrink the size of legend relative to the full height or width of the plot.
#' @param cex.axis An argument for legend control within image.plot(): Character size within the axis lables on the legend.
#' @param legend.width An argument for legend control within image.plot(): Width in characters of the legend strip.
#' @param control.legend.digits Option to print all legend axis labels with the same number of digits.
#' Default is 1.2, a little bigger that the width of a character.
#'
#' @return Plots map of the U.S.
#'
#'
#' @author Send bug reports, suggestions, corrections, or comments to Clay Hallman.

#' @export
#'
#' @section Notes:
#' Detailed information on the shapefiles can be found in documentation for the following:
#' county_boundary_2010_20m
#' county_boundary_2010_5m
#' county_boundary_2010_500k
#' county_boundary_2016_20m
#' county_boundary_2016_5m
#' county_boundary_2016_500k
#' county_boundary_2016_TL
#' state_boundary_2016_20m
#' state_boundary_2016_5m
#' state_boundary_2016_500k
#' state_boundary_2016_TL



############## Function#########################################
map_by_fips = function(data.to.map,
                       state.border.col = "black",
                       state.border.width = 0.75,
                       county.border.col = "black",
                       county.border.width = 0.5,
                       state.to.plot = ".",
                       map.resolution = "medium",
                       shapefile.year = 2016,
                       continental.us = TRUE,
                       missing.include = FALSE,
                       color.break.type = "quantiles",
                       color.break.values = seq(0,1,by=0.1),
                       color.break.digits = 3,
                       color.sequence = c("blue", "green", "yellow", "red"),
                       legend.spacing=6,
                       legend.digits=0,
                       legend.width = 1.2,
                       legend.shrink = 0.5,
                       cex.axis = 1,
                       scientific=FALSE,
                       control.legend.digits = TRUE,
                       remove.legend=FALSE,
                       print.vals = FALSE,
                       print.fips = FALSE,
                       proj.info = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=km +no_defs",
                       mask.fips = NA,
                       mask.color = "black")
{


  ##################################################################################################################
  #Load some packages:
  # library(mapproj)
  # library(ggplot2)
  library(RColorBrewer)
  library(fields)
  library(maps)
  library(sf)
  library(sp)
  
  # Read Data
  load("./MAP_county_census2016_5m.RData") 
  load("./MAP_state_census2016_5m.RData") 
  #load("D:/Postdoc/Projects/USDOS_TestRun/USDOSPipeline/Post_processing/MAP_county_census2016_5m.RData")
  #load("D:/Postdoc/Projects/USDOS_TestRun/USDOSPipeline/Post_processing/MAP_state_census2016_5m.RData")

  map.shapefile=county_boundary_2016_5m
  state.shapefile=state_boundary_2016_5m

  ##################################################################################################################
  # #Load and format the appropriate shapefile:
  # if(shapefile.year == 2010){
  #   if(map.resolution == "low"){assign("map.shapefile", county_boundary_2010_20m)
  #                               assign("state.shapefile", state_boundary_2016_20m)}
  #   if(map.resolution == "medium"){assign("map.shapefile", county_boundary_2010_5m)
  #                                  assign("state.shapefile", state_boundary_2016_5m)}
  #   if(map.resolution == "high"){assign("map.shapefile", county_boundary_2010_500k)
  #                                assign("state.shapefile", state_boundary_2016_500k)}
  #   if(!map.resolution %in% c("low", "medium", "high")){return("Check the map.resolution argument.")}
  # }
  # if(shapefile.year == 2016){
  #   if(map.resolution == "low"){assign("map.shapefile", county_boundary_2016_20m)
  #                               assign("state.shapefile", state_boundary_2016_20m)}
  #   if(map.resolution == "medium"){assign("map.shapefile", county_boundary_2016_5m)
  #                                  assign("state.shapefile", state_boundary_2016_5m)}
  #   if(map.resolution == "high"){assign("map.shapefile", county_boundary_2016_500k)
  #                                assign("state.shapefile", state_boundary_2016_500k)}
  #   if(map.resolution == "TL"){assign("map.shapefile", county_boundary_2016_TL)
  #                                assign("state.shapefile", state_boundary_2016_TL)}
  #   if(!map.resolution %in% c("low", "medium", "high", "TL")){return("Check the map.resolution argument.")}
  # }

  #Filter shapefile to include only continental US (if necessary)
  if(continental.us){map.shapefile = map.shapefile[map.shapefile$CONTINENTAL,]
                     state.shapefile = state.shapefile[state.shapefile$CONTINENTAL,]}

  #Filter shapefile to include only specified states (if necessary)
  if(!"." %in% state.to.plot){map.shapefile = map.shapefile[map.shapefile$STATE %in% state.to.plot,]
                              state.shapefile = state.shapefile[state.shapefile$NAME %in% state.to.plot,]}

  #Reformat the projection:
  if(proj.info != "."){
    # map.shapefile = spTransform(map.shapefile,
    #       CRS(proj.info))
    # state.shapefile = spTransform(state.shapefile,
    #       CRS(proj.info))
    map.shapefile = st_transform(st_as_sf(map.shapefile),
                                 crs = st_crs(proj.info))
    state.shapefile = st_transform(st_as_sf(state.shapefile),
                                   crs = st_crs(proj.info))
    map.shapefile = as(map.shapefile, "Spatial")
    state.shapefile = as(state.shapefile, "Spatial") #sf/rgdal is depreciated, need to transform using sp and then translate back to sf 
  }
  ##################################################################################################################


  ##################################################################################################################
  #Reformat the data to map
  colnames(data.to.map) = c("FIPS", "val")
  data.to.map$FIPS = as.numeric(as.character(data.to.map$FIPS)) #ensure FIPS are formatted correctly (5 digits, text)
  data.to.map$FIPS = sprintf("%05d", data.to.map$FIPS)

  all.fips = map.shapefile$GEOID
  missing.fips = all.fips[!all.fips %in% data.to.map$FIPS]
  line1 = paste0("There are ", sum(is.na(data.to.map$val)), " NA values in the dataset.")
  line2 = paste0("There are ", length(missing.fips), " counties missing from the dataset.")
  cat(line1)
  cat("\n")
  cat(line2)

  if(missing.include){
    data.to.map$val[is.na(data.to.map$val)] = 0
    if(length(missing.fips) > 0){
      data.to.map = rbind(data.to.map, data.frame(FIPS = missing.fips, val=0))
    }
  }
  map.data = data.to.map
  ##################################################################################################################

  ##################################################################################################################
  #Assign colors to the values

  #Three interpretations of the break intervals:
  # QUANTILES
  if(color.break.type == "quantiles"){
    color.breaks = c(as.numeric(quantile(map.data$val,
                                          color.break.values,
                                          na.rm=TRUE)))
    color.breaks = round(color.breaks, color.break.digits)
    color.breaks = color.breaks[!duplicated(color.breaks)]
    groupings = round(color.breaks, color.break.digits)

    n.color.breaks = length(color.breaks)
    map.data$color.cats = cut(map.data$val, color.breaks, include.lowest=TRUE)
    col.ramp = colorRampPalette(color.sequence)
    col.seq = col.ramp(n.color.breaks - 1)
    map.data$col = col.seq[map.data$color.cats]
  }


  # VALUES
  if(color.break.type == "values") {
    color.breaks = color.break.values
    groupings = round(color.breaks, color.break.digits)
    n.color.breaks = length(color.breaks)
    map.data$color.cats = as.numeric(cut(map.data$val, color.breaks, include.lowest=TRUE))
    col.ramp = colorRampPalette(color.sequence)
    col.seq = col.ramp(n.color.breaks - 1)
    map.data$col = col.seq[map.data$color.cats]
  }

  # RANGE
  if(color.break.type == "range"){
    n.breaks = color.break.values[1]
    range.min = color.break.values[2]
    range.max = color.break.values[3]
    color.breaks = seq.int(from=range.min, to=range.max, length.out = n.breaks)
    color.breaks = color.breaks[!duplicated(color.breaks)]
    groupings = color.breaks
    n.color.breaks = length(color.breaks)
    map.data$color.cats = as.numeric(cut(map.data$val, color.breaks, include.lowest=TRUE))
    col.ramp = colorRampPalette(color.sequence)
    col.seq = col.ramp(n.color.breaks - 1)
    map.data$col = col.seq[map.data$color.cats]
  }
  ##################################################################################################################


  ##################################################################################################################
  #Mask the counties that should not be displayed
  if(length(mask.fips) > 0){
    map.data$col[map.data$FIPS %in% mask.fips] = mask.color
  }
  ##################################################################################################################

  ##################################################################################################################
  #Match colors to the corresponding polygon in the shapefile
  cols.matched = map.data$col[match(map.shapefile$GEOID, map.data$FIPS)]
  vals.matched = map.data$val[match(map.shapefile$GEOID, map.data$FIPS)]

  cols.matched[is.na(cols.matched)] = "white"
  ##################################################################################################################

  ##################################################################################################################
  #Plot the map
  plot(map.shapefile, col=cols.matched, lty=0)
  plot(map.shapefile, density=0, border=county.border.col, lwd=county.border.width, add=TRUE)
  plot(state.shapefile, density = 0,border=state.border.col, lwd=state.border.width, add=TRUE)
  ##################################################################################################################

  ##################################################################################################################
  # Print county information on the map:
  if(print.vals){
    text(coordinates(map.shapefile), labels = vals.matched, col="black", cex=0.5)
  }

  if(print.fips){
    text(coordinates(map.shapefile), labels = map.shapefile$GEOID, col="black", cex=0.5)
  }
  ##################################################################################################################


  ##################################################################################################################
  #Add the legend
  zr = range(map.data$val, na.rm=TRUE)

  if(scientific == TRUE){
    groupings = format(groupings, scientific=TRUE)
  }

  if(control.legend.digits == TRUE){
    groupings = formatC(groupings, digits=legend.digits, format="f")
  }

  if(remove.legend == FALSE){
    image.plot( legend.only=TRUE, zlim= zr, nlevel=length(col.seq)+1,
                col=col.seq, horizontal=FALSE,
                legend.shrink = legend.shrink,
                legend.width = legend.width,
                add=TRUE,
                graphics.reset = FALSE,
                legend.mar=c(legend.spacing),
                lab.breaks=groupings, verbose=FALSE, axis.args=list(cex.axis=cex.axis))
  }
  ##################################################################################################################


}

