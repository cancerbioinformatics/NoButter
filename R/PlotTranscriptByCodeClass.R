#' Plot Transcripts by Code Class
#'
#' This function plots the counts of transcripts grouped by code class and optionally by another specified column from a raw transcript data file or data frame.
#' Optionally, the plot can be saved as an image file.
#'
#' @param raw_transcripts_file A character string representing the path to a CSV file containing raw transcript data, or a data frame with the transcript data.
#' @param group_by A string specifying the column name by which to group the transcript counts along with the code class. If NULL, the counts are grouped by 'fov' and 'codeclass'. Defaults to NULL.
#' @param fig_save A logical value indicating whether to save the plot as an image file. Defaults to FALSE.
#' @param fig_width A numeric value specifying the width of the saved plot image in inches. Defaults to 10.
#' @param fig_height A numeric value specifying the height of the saved plot image in inches. Defaults to 8.
#' @param out_dir A string specifying the path to the directory where the plot will be saved if \code{fig_save} is TRUE. If NULL and \code{fig_save} is TRUE, an error is raised.
#'
#' @details The function reads the raw transcript data from the specified CSV file or data frame. It then groups the data by code class and optionally by another specified column (\code{group_by}).
#' The resulting plot shows the proportion of transcripts for each code class within each group.
#'
#' If \code{fig_save} is TRUE and \code{out_dir} is not provided, an error message is displayed. The plot is saved as a PNG file in the specified output directory if \code{fig_save} is TRUE.
#'
#' @return A ggplot object representing the bar plot of transcript counts by code class.
#'
#' @examples
#' \dontrun{
#' plt <- PlotTranscriptByCodeClass("path/to/raw_transcripts.csv")
#' plt <- PlotTranscriptByCodeClass(raw_transcripts, group_by = "gene", fig_save = TRUE, out_dir = "path/to/output/directory")
#' }
#'
#' @export
PlotTranscriptByCodeClass <- function(raw_transcripts_file,
                                      group_by = NULL,
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
  if (!is.null(group_by)) {
    group_by <- trimws(group_by)
    if (!group_by %in% colnames(raw_transcripts)) {
      stop(paste("The column", group_by, "does not exist in the raw transcripts data."))
    }
  }

  if (is.null(group_by)) {
    transcripts <- raw_transcripts %>%
      count(fov, codeclass) %>%
      rename(no_of_transcripts = n) %>%
      mutate(fov = as.numeric(fov))

    p <- ggplot(transcripts, aes(x = fov, y = no_of_transcripts, fill = codeclass)) +
      geom_bar(stat = "identity", position = "fill") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggtitle("# of raw transcripts per codeclass") + theme_custom()
  } else {
    transcripts <- raw_transcripts %>%
      count(!!sym(group_by), codeclass) %>%
      rename(no_of_transcripts = n)

    p <- ggplot(transcripts, aes_string(x = group_by, y = "no_of_transcripts", fill = "codeclass")) +
      geom_bar(stat = "identity", position = "fill") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggtitle("# of raw transcripts per codeclass") + theme_custom()
  }

  if (fig_save) {
    if (is.null(out_dir)) {
      stop("Please specify out_dir when fig_save is TRUE.")
    }
    filename <- if (is.null(group_by)) {
      "transcripts_per_codeclass.png"
    } else {
      paste0("transcripts_per_codeclass.png")
    }
    ggsave(filename = file.path(out_dir, filename), plot = p, width = fig_width, height = fig_height)
  }

  return(p)
}

