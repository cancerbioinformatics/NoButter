#' Visualize Transcripts for Fields of View (FOVs)
#'
#' This function generates scatter plots of transcript data for specified fields of view (FOVs) and optionally saves the plots as PNG files.
#'
#' @param raw_transcripts A data frame containing raw transcript data, including coordinates and FOV identifiers.
#' @param fov A vector of FOV identifiers for which the plots should be generated.
#' @param fig.save A logical value indicating whether to save the generated plots as PNG files. Defaults to \code{FALSE}.
#' @param out_dir A string specifying the directory where the plots should be saved if \code{fig.save} is \code{TRUE}. Defaults to \code{NULL}.
#' @param fig.width A numeric value specifying the width of the saved plot in inches. Defaults to 8.
#' @param fig.height A numeric value specifying the height of the saved plot in inches. Defaults to 6.
#' @return Generates and optionally saves scatter plots of transcript data for each specified FOV.
#' @details This function filters the raw transcript data to include only the specified FOVs, then generates scatter plots of the transcript coordinates. If \code{fig.save} is set to \code{TRUE}, the plots are saved to the specified output directory.
#' @examples
#' \dontrun{
#' # Example usage:
#' raw_transcripts <- read.csv("path/to/raw_transcripts.csv")
#' fov_list <- c("FOV1", "FOV2", "FOV3")
#' PlotTranscriptsByFOV(raw_transcripts, fov_list, fig.save = TRUE, out_dir = "path/to/output/directory")
#' }
#' @export
PlotTranscriptsByFOV <- function(raw_transcripts, 
                                 fov,
                                 fig.save = FALSE,
                                 out_dir = NULL, 
                                 fig.width = 8, 
                                 fig.height = 6) {
  
  for (FOV in fov) {
    filtered_data <- raw_transcripts %>% filter(fov == FOV)
    
    if (nrow(filtered_data) == 0) {
      message("No data found for FOV: ", FOV)
      next
    }
    
    plot <- ggplot(filtered_data, aes(x = x, y = y)) +
      geom_point(size = 0.05) +
      ggtitle(paste("FOV", FOV, "raw transcripts")) +
      theme_custom()
    
    if (fig.save) {
      if (!is.null(out_dir)) {
        if (!dir.exists(out_dir)) {
          dir.create(out_dir, recursive = TRUE)
        }
        ggsave(filename = file.path(out_dir, paste0("FOV_", FOV, "_raw_transcripts.png")), plot = plot, width = fig.width, height = fig.height)
        print(plot)
      } else {
        message("Output directory must be provided to save plots.")
      }
    } else {
      print(plot)
    }
  }
}
