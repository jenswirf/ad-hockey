

theme_adhockey <- function(base_size = 1.5, base_family = "Arial") {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n=9)
  color.background = "#f2f2f2" # palette[2] #f9f9f9
  color.grid.major = palette[3]
  color.axis.text = palette[5]
  color.axis.title = palette[6]
  color.title = palette[8]

  # Begin construction of chart
  theme_bw(base_size = base_size * 9) +

    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    #theme(legend.position = "none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size= base_size * 6, color=color.axis.title)) +
    theme(legend.title=element_blank()) +
    theme(legend.key = element_blank()) +

    # facets
    theme(strip.background = element_blank()) +

    # Set title and axis labels, and format these and tick marks
    theme(axis.text.x=element_text(size=base_size * 6, color=color.axis.text)) +
    theme(axis.text.y=element_text(size=base_size * 6, color=color.axis.text)) +
    theme(axis.title.x=element_text(size=base_size * 7, color=color.axis.title, vjust=2, margin=ggplot2::margin(t=15))) +
    theme(axis.title.y=element_text(size=base_size * 7, color=color.axis.title, vjust=1.25, margin=ggplot2::margin(r=10))) +
    theme(plot.title=element_text(color=color.title, size=16, vjust=1.25, family = base_family, face = "bold")) +
    #theme(plot.subtitle=element_text(hjust=0, size=11, margin=ggplot2::margin(b=10), family = base_family)) +
    #theme(plot.caption=element_text(hjust=1, size=12, margin=ggplot2::margin(t=2), family = base_family)) +

    # Plot margins
    theme(plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.35), "cm")) +
    theme(text=element_text(size=base_size*8, family = base_family))

}


scale_colour_discrete <- function(...) scale_color_manual(values = .adhoc, ...)

scale_fill_discrete <- function(...) scale_fill_manual(values = .adhoc, ...)

scale_color_continuous <- function(...) scale_color_gradient2(low = "#b5c2c6", mid = "#4a7b8f", high = "#0f4a60", ...)
scale_fill_continuous <- function(...) scale_color_gradient2(low = "#b5c2c6", mid = "#4a7b8f", high = "#0f4a60", ...)

