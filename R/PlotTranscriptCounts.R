#' Plot Transcript Counts
#'
#' This function plots the counts of transcripts grouped by a specified column from a raw transcript data frame.
#' Optionally, the plot can be saved as an image file.
#'
#' @param raw_transcripts_file A character string representing the path to a CSV file containing raw transcript data, or a data frame with the transcript data.
#' @param group_by A string specifying the column name by which to group the transcript counts. Defaults to "fov".
#' @param colour_by A string specifying the column name to use for coloring the bars in the plot. If NULL, no coloring is applied. Defaults to NULL.
#' @param fig_save A logical value indicating whether to save the plot as an image file. Defaults to FALSE.
#' @param fig_width A numeric value specifying the width of the saved plot image in inches. Defaults to 10.
#' @param fig_height A numeric value specifying the height of the saved plot image in inches. Defaults to 8.
#' @param out_dir A string specifying the path to the directory where the plot will be saved if \code{fig_save} is TRUE. If NULL and \code{fig_save} is TRUE, an error is raised.
#'
#' @details The function reads the raw transcript data from the specified CSV file or data frame. It then groups the data by the specified column (\code{group_by}) and optionally colors the bars by another specified column (\code{colour_by}).
#' The resulting plot shows the number of transcripts for each group, with a red dashed line indicating the median value.
#'
#' If \code{fig_save} is TRUE and \code{out_dir} is not provided, an error message is displayed. The plot is saved as a PNG file in the specified output directory if \code{fig_save} is TRUE.
#'
#' @return A ggplot object representing the bar plot of transcript counts.
#'
#' @examples
#' \dontrun{
#' plt <- PlotTranscriptCounts("path/to/raw_transcripts.csv", group_by = "fov")
#' plt <- PlotTranscriptCounts(raw_transcripts, group_by = "fov", colour_by = "gene", fig_save = TRUE, out_dir = "path/to/output/directory")
#' }
#'
#' @export
PlotTranscriptCounts <- function(raw_transcripts_file,
                                 group_by = "fov",
                                 colour_by = NULL,
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
  group_by <- trimws(group_by)
  if (!is.null(colour_by)) {
    colour_by <- trimws(colour_by)
  }

  if (!group_by %in% colnames(raw_transcripts)) {
    stop(paste("The column", group_by, "does not exist in the raw transcripts data."))
  }

  if (!is.null(colour_by) && !colour_by %in% colnames(raw_transcripts)) {
    stop(paste("The column", colour_by, "does not exist in the raw transcripts data."))
  }

  if (is.null(colour_by)) {
    group_data <- as.data.frame(table(raw_transcripts[[group_by]]))
    colnames(group_data) <- c(group_by, "no_of_transcripts")

    median_value <- median(group_data$no_of_transcripts)

    p <- ggplot(group_data, aes_string(x = group_by, y = "no_of_transcripts")) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      geom_hline(yintercept = median_value, color = "red", linewidth = 1.2, linetype = "dashed") +
      ggtitle(paste("# of raw transcripts per", group_by))+ theme_custom()
  } else {
    group_data <- raw_transcripts %>%
      group_by_at(c(group_by, colour_by)) %>%
      summarise(no_of_transcripts = n(), .groups = 'drop')

    median_value <- median(group_data$no_of_transcripts)

    p <- ggplot(group_data, aes_string(x = group_by, y = "no_of_transcripts", fill = colour_by)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      geom_hline(yintercept = median_value, color = "red", linewidth = 1.2, linetype = "dashed") +
      ggtitle(paste("# of transcripts per", group_by)) + theme_custom()
  }

  if (fig_save) {
    if (is.null(out_dir)) {
      stop("Please specify out_dir when fig_save is TRUE.")
    }
    ggsave(filename = file.path(out_dir, paste0("transcripts_per_", group_by, ".png")), plot = p, width = fig_width, height = fig_height)
  }

  return(p)
}
