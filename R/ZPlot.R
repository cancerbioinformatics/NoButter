#' Plot Transcripts by Z-Stack
#'
#' This function visualizes the distribution of transcripts across z-stacks for specified fields of view (FOVs).
#' Optionally, the plots can be saved as image files.
#'
#' @param raw_transcripts A data frame containing raw transcript data or a character string representing the path to a CSV file containing the data.
#' @param fovs A vector of FOV identifiers for which the visualizations will be generated.
#' @param fig_save A logical value indicating whether to save the plots as image files. Defaults to FALSE.
#' @param out_dir A string specifying the path to the directory where the plots will be saved if \code{fig_save} is TRUE. If NULL and \code{fig_save} is TRUE, an error is raised.
#' @param fig_width A numeric value specifying the width of the saved plot images in inches. Defaults to 8.
#' @param fig_height A numeric value specifying the height of the saved plot images in inches. Defaults to 6.
#'
#' @details The function reads the raw transcript data from the specified CSV file or data frame. For each specified FOV, it plots the distribution of transcripts across z-stacks, showing their x and y coordinates and coloring them by code class.
#' The resulting plot shows the spatial distribution of transcripts in the specified FOVs, faceted by z-stack.
#'
#' If \code{fig_save} is TRUE and \code{out_dir} is not provided, an error message is displayed. The plots are saved as PNG files in the specified output directory if \code{fig_save} is TRUE.
#'
#' @return A list of ggplot objects representing the plots of transcript distributions for each specified FOV.
#'
#' @examples
#' \dontrun{
#' plots <- ZPlot(raw_transcripts, fovs = c(1:10))
#' plots <- ZPlot(raw_transcripts, fovs = c(1:10), fig_save = TRUE, out_dir = "path/to/output/directory")
#' }
#'
#' @export
ZPlot <- function(raw_transcripts,
                  fovs,
                  fig_save = FALSE,
                  out_dir = NULL,
                  fig_width = 8,
                  fig_height = 6) {

  if (is.character(raw_transcripts)) {
    raw_transcripts <- read.csv(raw_transcripts)
  } else if (!is.data.frame(raw_transcripts)) {
    stop("'raw_transcripts' must be either a character string representing a file path or a data frame.")
  }

  colnames(raw_transcripts) <- trimws(colnames(raw_transcripts))

  plot_list <- list()

  for(fovOI in fovs) {
    print(fovOI)
    unique_z <- unique(raw_transcripts$z[raw_transcripts$fov == fovOI])

    num_z <- length(unique_z)
    paste("Number of unique Z stacks:", num_z)

    p <- raw_transcripts %>% filter(fov == fovOI) %>%
      ggplot(aes(x = x, y = y, colour = codeclass)) +
      geom_point(size = 0.1) +
      ggtitle(paste("FOV", fovOI, "raw transcripts")) +
      xlim(c(0, 5000)) + ylim(c(0, 5000)) +
      facet_wrap(~z, nrow = ceiling(sqrt(num_z))) + theme_minimal()

    plot_list[[fovOI]] <- p

    if (fig_save) {
      if (is.null(out_dir)) {
        stop("Please specify out_dir when fig_save is TRUE.")
      }
      filename <- paste("raw_transcripts_per_fov_", fovOI, ".png", sep = "")
      ggsave(filename = file.path(out_dir, filename), plot = p, width = fig_width, height = fig_height)
    }
  }

  return(plot_list)
}
