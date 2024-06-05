#' Plot Transcript Loss Proportion
#'
#' This function visualizes the proportion of transcripts that are lost or retained across z-stacks for each field of view (FOV), filtered by code class.
#' Optionally, the plot can be saved as an image file.
#'
#' @param raw_transcripts A data frame containing raw transcript data.
#' @param codeclass_filter A string specifying the code class to filter transcripts by. Defaults to "Endogenous".
#' @param keep_z_stacks A vector of z-stacks to keep. If NULL, all z-stacks are kept. Defaults to NULL.
#' @param fig_save A logical value indicating whether to save the plot as an image file. Defaults to FALSE.
#' @param fig_width A numeric value specifying the width of the saved plot image in inches. Defaults to 10.
#' @param fig_height A numeric value specifying the height of the saved plot image in inches. Defaults to 8.
#' @param out_dir A string specifying the path to the directory where the plot will be saved if \code{fig_save} is TRUE. If NULL and \code{fig_save} is TRUE, an error is raised.
#'
#' @details The function filters the raw transcript data by the specified code class, calculates the number of transcripts per z-stack for each FOV, and determines which z-stacks to keep based on the provided \code{keep_z_stacks} parameter.
#' The resulting plot shows the proportion of transcripts in the selected z-stacks for each FOV.
#'
#' If \code{fig_save} is TRUE and \code{out_dir} is not provided, an error message is displayed. The plot is saved as a PNG file in the specified output directory if \code{fig_save} is TRUE.
#'
#' @return A ggplot object representing the bar plot of transcript loss proportions.
#'
#' @examples
#' \dontrun{
#' plt <- PlotTranscriptLossProportion(raw_transcripts)
#' plt <- PlotTranscriptLossProportion(raw_transcripts, codeclass_filter = "Endogenous", keep_z_stacks = c(1, 2, 3), fig_save = TRUE, out_dir = "path/to/output/directory")
#' }
#'
#' @export
PlotTranscriptLossProportion <- function(raw_transcripts,
                                         codeclass_filter = "Endogenous",
                                         keep_z_stacks = NULL,
                                         fig_save = FALSE,
                                         fig_width = 10,
                                         fig_height = 8,
                                         out_dir = NULL) {

  raw_transcripts_genes <- raw_transcripts %>%
    filter(codeclass == codeclass_filter)

  genes_per_z_per_fov <- table(raw_transcripts_genes$fov, raw_transcripts_genes$z)

  genes_per_z_per_fov_df <- as.data.frame(genes_per_z_per_fov) %>%
    rename(fov = Var1, z_stack = Var2, value = Freq) %>%
    mutate(fov = factor(fov, levels = as.character(seq(min(raw_transcripts_genes$fov), max(raw_transcripts_genes$fov)))))

  if (is.null(keep_z_stacks)) {
    genes_per_z_per_fov_df$keep <- TRUE
  } else {
    genes_per_z_per_fov_df$keep <- genes_per_z_per_fov_df$z_stack %in% keep_z_stacks
  }

  p <- ggplot(genes_per_z_per_fov_df, aes(x = fov, y = value, fill = keep)) +
    geom_bar(stat = "identity", position = "fill") +
    ggtitle(paste("Proportion of", codeclass_filter, "transcripts in selected z stacks")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_manual(values = c("red", "darkgreen")) +
    ylab("proportion") +
    theme_custom()

  if (fig_save) {
    if (is.null(out_dir)) {
      stop("Please specify out_dir when fig_save is TRUE.")
    }
    filename <- paste0("Distribution_", codeclass_filter, "_z_stacks.png")
    ggsave(filename = file.path(out_dir, filename), plot = p, width = fig_width, height = fig_height)
  }

  return(p)
}
