#!/usr/bin/env Rscript
# ============================================================================ #
# OH-EpiCap Cross-System Analysis - Frederick T. A. Freeth          12/07/2023 |
# ============================================================================ #

# ---- Preamble ----
if(!require("plotrix")){install.packages("plotrix"); library(plotrix)}
if(!require("ggplot2")){install.packages("ggplot2"); library(ggplot2)}
if(!require("rstudioapi")){install.packages("rstudioapi"); library(rstudioapi)}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set WD to file location


# ---- Creates the radar plot ----
radar_plot <- function(indicators, num_sections, show_min_max, section_cols,
                       max_score, inner_labels, radial_labels, SVG_name){
  # indicators:    The indicators (questionnaire answers) to plot
  # num_sections:  The number of sections to create
  # show_min_max:  Show the minimum and maximum values of the indicators
  # section_cols:  The colours of the separate sections
  # max_score:     Highest possible score in questionnaire.
  # inner_labels:  Labels which to add around the innermost edge of the plot.
  # radial_labels: Labels which to add around the outermost edge of the plot.
  # SVG_name:      The file name to call the svg image.
  
  # Produce errors if invalid inputs are given
  if(num_sections < 1){
    stop("Error. num_sections must be greater than or equal to one.")
  }
  else if(num_sections != length(section_cols)){
    stop("Error. section_cols must have the same length as num_sections.")
  }
  else if(max_score < 1){
    stop("Error. max_score must be greater than or equal to one.")
  }
  
  # Generate the statistics used in the radar plots.
  num_indicators <- nrow(indicators)
  axes_col <- ggplot2::alpha(colour = "black", alpha = 0.1)
  section_cols_points <- unlist(lapply(section_cols,function(cl){rep(cl, num_indicators/num_sections)}))
  data_max <- unlist(lapply(1:num_indicators, function(i){max(indicators[i,-(1:2)], na.rm = T)}))
  data_upper <- unlist(lapply(1:num_indicators, function(i){quantile(indicators[i,-(1:2)], na.rm = T)[4]}))
  data_mean <- rowMeans(indicators[,-(1:2)], na.rm = TRUE)
  data_median <- unlist(lapply(1:num_indicators, function(i){median(unlist(indicators[i,-(1:2)]), na.rm = T)}))
  data_lower <- unlist(lapply(1:num_indicators, function(i){quantile(indicators[i,-(1:2)], na.rm = T)[2]}))
  data_min <- unlist(lapply(1:num_indicators, function(i){min(indicators[i,-(1:2)], na.rm = T)}))
  
  # Create the angles as to which we radially plot out targets. We generate angles
  # for the number of targets plus the number of dimensions as we will remove an
  # angle for each dimension to give the points some space.
  coord_angle <- unlist(lapply(X = 1:num_sections, FUN = function(i){
    angle_buffer <- 10 # Buffer of 15 degrees between zones
    angle_limits <- c((0:num_sections)*360/num_sections)[i:(i+1)] + c(angle_buffer, -angle_buffer)
    coord_angle_deg <- seq(from = angle_limits[1], to = angle_limits[2], length.out = num_indicators/num_sections)
    coord_angle_rad <- coord_angle_deg*base::pi/180
    # Added pi/2 to plot from the vertical and do the negative of coord_angle_rad
    # so that we plot clockwise.
    return(-coord_angle_rad + base::pi/2)
  }))
  
  # Create the figure
  plot.new()
  svg(paste0(SVG_name, ".svg"))
  par(mar = c(3, 3, 3, 3), las = 1, xpd = TRUE, bg = "transparent", pty = "s")
  plot(x = NULL, y = NULL, xlim = max_score*c(-1, 1), ylim = max_score*c(-1, 1),
       axes = FALSE, xlab = "", ylab = "")
  
  # Draw concentric grey circles
  for(i in 1:max_score){plotrix::draw.circle(x = 0, y = 0, nv = 512, radius = i, border = axes_col, col = NA)}
  
  # Add axis labels 1 to max_score and then radial lines from center outwards.
  # The first line is vertically upwards, then the others equally spaced around.
  text(x = 0, y = 1:max_score, labels = 1:max_score, col = axes_col, adj = c(1.2, 1.2))
  for(i in 0:(num_sections - 1)){
    lines(x = c(0, max_score*cos(base::pi/2 + 2*i*base::pi/num_sections)),
          y = c(0, max_score*sin(base::pi/2 + 2*i*base::pi/num_sections)), col = axes_col)
  }
  # Plot the target median points with lines of the min and max values 
  points(x = data_median*cos(coord_angle), y = data_median*sin(coord_angle), pch = 16, col = section_cols_points)
  
  counter <- 1 # Counter to count through the OH_cols and Dim./Tar. numbers
  for(i in seq(1, num_indicators, num_indicators/num_sections)){
    # Indices for targets in dimension
    dim_index <- i:(i + num_indicators/num_sections - 1)
    
    # Target minimum and target maximum score lines
    if(show_min_max == TRUE){
      polygon(x = c(data_min[dim_index]*cos(coord_angle[dim_index]),
                    rev(data_max[dim_index]*cos(coord_angle[dim_index]))),
              y = c(data_min[dim_index]*sin(coord_angle[dim_index]),
                    rev(data_max[dim_index]*sin(coord_angle[dim_index]))),
              border = ggplot2::alpha(section_cols_points[dim_index][1], alpha = 0.1),
              col = ggplot2::alpha(section_cols_points[dim_index][1], alpha = 0.1)
      )
    }
    # Target upper and target lower shaded polygon
    polygon(x = c(data_lower[dim_index]*cos(coord_angle[dim_index]),
                  rev(data_upper[dim_index]*cos(coord_angle[dim_index]))),
            y = c(data_lower[dim_index]*sin(coord_angle[dim_index]),
                  rev(data_upper[dim_index]*sin(coord_angle[dim_index]))),
            border = ggplot2::alpha(section_cols_points[dim_index][1], alpha = 0.2),
            col = ggplot2::alpha(section_cols_points[dim_index][1], alpha = 0.2)
            
    )
    # Add in the inner labels to the center of the plot
    text(labels = paste(inner_labels, counter),
         col = section_cols[counter],
         x = 0.6*cos(mean(coord_angle[dim_index])),
         y = 0.6*sin(mean(coord_angle[dim_index]))
    )
    counter <- counter  + 1 
  }
  # Add target labels
  text(x = (0.5 + max_score)*cos(coord_angle), y = (0.5 + max_score)*sin(coord_angle),
       labels = stringr::str_wrap(radial_labels, width = 8), cex = 0.8)
  rp <- recordPlot() # Save figure as returnable variable
  dev.off()
  return(rp)
}

# ---- Plots the OH EpiCap Score for each system
OH_score_plot <- function(targets, bar_cols, isLongitudinalStudy, SVG_name){
  # indicators:          The indicators (questionnaire answers) to plot.
  # bar_cols:            The colours of the barplot.
  # isLongitudinalStudy: Draw lines betwenn OH-EpiCap scores if study is over time.
  # SVG_name:            The file name to call the svg image.
  
  OH_EpiCap_Index <- (colMeans(targets[,-(1:2)], na.rm = TRUE) - 1)*100/3
  OH_EpiCap_Dim_Index <- do.call(rbind, lapply(
    seq(from = 1, to = nrow(targets), by = num_targets), FUN = function(i){
      return((colMeans(targets[i:(i + num_targets - 1),-(1:2)], na.rm = TRUE) - 1)*100/3)
  }))
  rownames(OH_EpiCap_Dim_Index) <- paste("Dimension", 1:num_dims)
  
  # We group then sort the combined data by who has the highest OH-EpiCap Index
  grouped_OH_EpiCap_Data <- rbind(OH_EpiCap_Dim_Index, OH_EpiCap_Index)
  grouped_OH_EpiCap_Data <- grouped_OH_EpiCap_Data[, names(sort(OH_EpiCap_Index))]
  num_systems <- ncol(grouped_OH_EpiCap_Data)
  
  # Make dimension bar colours lighter and keep the overall EpiCap score the same.
  # This is needed in case you want to make the overall score stand out more.
  dim_bar_opacity <- 0.1
  faded_bar_cols <- ggplot2::alpha(colour = bar_cols, alpha = c(rep(dim_bar_opacity, num_dims), 1))
  
  plot.new()
  svg(filename = paste0(SVG_name,".svg"), width = 14, height = 6)
  par(mar = c(4, 4, 1, 8), xpd = TRUE, las = TRUE, bg = "transparent")
  barplot(
    grouped_OH_EpiCap_Data, beside = TRUE, border = bar_cols, col = faded_bar_cols,
    names.arg = colnames(grouped_OH_EpiCap_Data), ylim = c(0, 100), cex.names = 0.8,
    ylab = "OneHealth-EpiCap Score (%)", xlab = "Epidemiological System", space = c(0.05, 1)
  )
  # Find the coordinates of the OH-EpiCap Index (green bar). We have a space
  # between each group of bars (number of groups - 1), with num_dims + 1 bars
  # in each group. We then center the lines by adding 0.5. Using this, we can
  # draw lines between systems (in the case of a longitudinal study)
  if(isLongitudinalStudy){
    bar_coords <- 0.5 + seq(from = num_dims + 1, to = (num_systems - 1) + (1 + num_dims)*num_systems, by = num_dims + 2)
    lines(x = bar_coords, y = grouped_OH_EpiCap_Data[num_dims + 1,], col = bar_cols[num_dims + 1], lwd = 2)
  }
  legend(
    "topright",
    legend = c(paste("Dimension", 1:num_dims, "Score"), "OH-EpiCap Score"),
    col = bar_cols, bty = "n", pch = 15, inset = c(-0.13, 0), pt.cex = 2
  )
  EpiCap_Score_plot <- recordPlot()
  dev.off()
  return(EpiCap_Score_plot)
}


# ---- Main Parameters ----
OHEC_data <- read.csv("System_Data.csv", check.names = FALSE)
num_dims <- 3  # How many dimensions in the OH-EpiCap Questionnaire.
num_targets <- 4 # Number of targets per dimension

# Official OneHealth EpiCap Tool Colours
OH_cols <- c("#13699f", "#ef7a33", "#777777", "#7fa057")

# Calculate the dimensions on a target level
OHEC_target <- as.data.frame(cbind(
  "Target_Number" = sort(matrix(outer(1:num_dims, (1:num_targets)*0.1, "+"), nrow = num_dims*num_targets)),
  "Target_Label" = c(
    "Formalization", "Coverage / Transdisciplinary", "Resources", "Evaluation and Resilience",
    "Data Collection / Methods Sharing", "Data Sharing", "Data Analysis and Interpretation", "Communication",
    "Technical Outputs", "Collaborative Added Value", "Immediate and Intermediate outcomes", "Ultimate Outcomes"
  ),
  apply(X = OHEC_data[-(1:2)], MARGIN = 2, FUN = function(column){
    unlist(lapply(seq(1, nrow(OHEC_data), nrow(OHEC_data)/(num_dims*num_targets)), function(i){
      return(mean(column[i:(i + num_targets - 1)], na.rm = TRUE))
    }))
  })
))
# As we added in character types to OHEC_target, this mutates all the numeric
# types to become characters also so we need to change them back
OHEC_target[,-(1:2)] <- apply(OHEC_target[,-(1:2)], MARGIN = 2, FUN = as.numeric)

# ---- Produce the Radar Plots ----
# All indicators on one plot
radar_plot(indicators = OHEC_data, num_sections = 3, show_min_max = FALSE,
           section_cols = OH_cols[1:3], max_score = 4, inner_labels = "Dim.",
           radial_labels = OHEC_data$Dimension_Number, SVG_name = "Indicators_All_Radar")

# Plot the dimension indicators at the target level
radar_plot(indicators = OHEC_target, num_sections = 3, show_min_max = FALSE,
           section_cols = OH_cols[1:3], max_score = 4, inner_labels = "Dim.",
           radial_labels = OHEC_target$Target_Label, SVG_name = "Indicators_Target_Radar")

# Plot the indicators across the four targets in Dimension 1
radar_plot(indicators = OHEC_data[1:16,], num_sections = 4, show_min_max = FALSE,
           section_cols = rep(OH_cols[1], 4), max_score = 4, inner_labels = "Tar.",
           radial_labels = OHEC_data$Dimension_Label[1:16], SVG_name = "Indicators_Dimension_1_Radar")

# Plot the indicators across the four targets in Dimension 2
radar_plot(indicators = OHEC_data[17:32,], num_sections = 4, show_min_max = FALSE,
           section_cols = rep(OH_cols[2], 4), max_score = 4, inner_labels = "Tar.",
           radial_labels = OHEC_data$Dimension_Label[17:32], SVG_name = "Indicators_Dimension_2_Radar")

# Plot the indicators across the four targets in Dimension 3
radar_plot(indicators = OHEC_data[33:48,], num_sections = 4, show_min_max = FALSE,
           section_cols = rep(OH_cols[3], 4), max_score = 4, inner_labels = "Tar.",
           radial_labels = OHEC_data$Dimension_Label[33:48], SVG_name = "Indicators_Dimension_3_Radar")

# Plot the OH-EpiCap Scores across all the systems
OH_score_plot(targets = OHEC_target, bar_cols = OH_cols,
              isLongitudinalStudy = FALSE, SVG_name = "EpiCap_Scores")


# ============================================================================ #
# OH-EpiCap Cross-System Analysis - Code End                                   |
# ============================================================================ #