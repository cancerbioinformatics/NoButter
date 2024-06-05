#' Visualize Z-Stack Distribution
#'
#' This function visualizes the distribution of transcripts across z-stacks for each field of view (FOV).
#' Optionally, the data can be binarized and the plot can be saved as an image file.
#'
#' @param raw_transcripts_file A character string representing the path to a CSV file containing raw transcript data, or a data frame with the transcript data.
#' @param binarised A logical value indicating whether to binarize the data. If TRUE, the data is binarized to indicate presence (1) or absence (0) of transcripts. Defaults to FALSE.
#' @param fig_save A logical value indicating whether to save the plot as an image file. Defaults to FALSE.
#' @param fig_width A numeric value specifying the width of the saved plot image in inches. Defaults to 10.
#' @param fig_height A numeric value specifying the height of the saved plot image in inches. Defaults to 8.
#' @param out_dir A string specifying the path to the directory where the plot will be saved if \code{fig_save} is TRUE. If NULL and \code{fig_save} is TRUE, an error is raised.
#'
#' @details The function reads the raw transcript data from the specified CSV file or data frame. It then calculates the number of transcripts per z-stack for each FOV.
#' The resulting heatmap shows the distribution of transcripts, either scaled or binarized, across z-stacks for each FOV.
#'
#' If \code{fig_save} is TRUE and \code{out_dir} is not provided, an error message is displayed. The plot is saved as a PNG file in the specified output directory if \code{fig_save} is TRUE.
#'
#' @return A pheatmap object representing the heatmap of transcript distribution across z-stacks for each FOV.
#'
#' @examples
#' \dontrun{
#' plt <- VisualizeZStackDistribution("path/to/raw_transcripts.csv")
#' plt <- VisualizeZStackDistribution(raw_transcripts, binarised = TRUE, fig_save = TRUE, out_dir = "path/to/output/directory")
#' }
#'
#' @export
VisualizeZStackDistribution <- function(raw_transcripts_file,
                                        binarised = FALSE,
                                        fig_save = FALSE,
                                        fig_width = 10,
                                        fig_height = 8,
                                        out_dir = NULL) {

  if (is.character(raw_transcripts_file)) {
    raw_transcripts <- read.csv(raw_transcripts_file)
  } else if (is.data.frame(raw_transcripts_file)) {
    raw_transcripts <- raw_transcripts_file
  } else {
    stop("'raw_transcripts_file' must be either a character string representing a file path or a data frame.")
  }

  colnames(raw_transcripts) <- trimws(colnames(raw_transcripts))
  transcripts_per_fov_per_z_stack <- as.data.frame.matrix(table(raw_transcripts$fov, raw_transcripts$z))

  if (binarised) {
    transcripts_per_fov_per_z_stack <- 1 * (transcripts_per_fov_per_z_stack > 0)
    p <- pheatmap(t(transcripts_per_fov_per_z_stack),
                  cluster_rows = FALSE,
                  cluster_cols = FALSE,
                  color = c("red", "darkgreen"),
                  main = "binarized transcripts per z stack per fov")
  } else {
    p <- pheatmap(t(transcripts_per_fov_per_z_stack),
                  cluster_rows = FALSE,
                  cluster_cols = FALSE,
                  scale = "column",
                  main = "scaled # of transcripts per z stack per fov")
  }

  if (fig_save) {
    if (is.null(out_dir)) {
      stop("Please specify out_dir when fig_save is TRUE.")
    }
    filename <- if (binarised) {
      "binarized_transcripts_per_zstack_per_fov.png"
    } else {
      "scaled_transcripts_per_zstack_per_fov.png"
    }
    ggsave(filename = file.path(out_dir, filename), plot = p, width = fig_width, height = fig_height)
  }

  return(p)
}
