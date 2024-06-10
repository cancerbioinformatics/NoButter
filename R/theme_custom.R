#' Custom ggplot2 Theme
#'
#' A custom ggplot2 theme that sets specific styling options for various plot elements.
#'
#' This function creates a custom theme for ggplot2 plots with specified formatting for plot titles,
#' text, backgrounds, borders, axis titles, axis text, grid lines, legends, and strip elements.
#'
#' @return A ggplot2 theme object with the specified custom settings.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
#'   geom_point() + 
#'   theme_custom()
#' print(p)
theme_custom <- function() {
  theme(
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA, fill = "white"),
    plot.background = element_rect(colour = NA, fill = "white"),
    panel.border = element_blank(),
    axis.title = element_text(face = "bold", size = rel(1.1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(face = "italic"),
    legend.text = element_text(size = 10),
    plot.margin = unit(c(10, 5, 5, 5), "mm"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  )
}
