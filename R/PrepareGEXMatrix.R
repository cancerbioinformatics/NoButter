#' Prepare Gene Expression (GEX) Matrix
#'
#' This function prepares a GEX matrix from raw transcript data, filtering by specified z-stacks and removing system control probes and empty cells.
#' Optionally, the matrix can be saved as a CSV file.
#'
#' @param raw_transcripts A data frame containing raw transcript data or a character string representing the path to a CSV file containing the data.
#' @param z_stacks A vector of z-stacks to keep in the analysis.
#' @param output_dir A string specifying the path to the directory where the GEX matrix will be saved if \code{save_matrix} is TRUE.
#' @param save_matrix A logical value indicating whether to save the GEX matrix as a CSV file. Defaults to FALSE.
#'
#' @details The function reads the raw transcript data from the specified CSV file or data frame. It filters out cells with \code{CellId} equal to 0 and keeps only the specified z-stacks.
#' It also removes probes labeled as 'SystemControl' and creates a unique cell identifier. The resulting GEX matrix is then generated and optionally saved as a CSV file.
#'
#' If \code{save_matrix} is TRUE and \code{output_dir} is not provided, an error message is displayed. The matrix is saved as a CSV file in the specified output directory if \code{save_matrix} is TRUE.
#'
#' @return A data frame representing the cleaned GEX matrix.
#'
#' @examples
#' \dontrun{
#' gex_matrix <- PrepareGEXMatrix(raw_transcripts, z_stacks = c(1, 2, 3), output_dir = "path/to/output/directory")
#' gex_matrix <- PrepareGEXMatrix(raw_transcripts, z_stacks = c(1, 2, 3), output_dir = "path/to/output/directory", save_matrix = FALSE)
#' }
#'
#' @export
PrepareGEXMatrix <- function(raw_transcripts,
                             z_stacks,
                             output_dir = NULL,
                             save_matrix = FALSE) {

  # Ensure necessary libraries are loaded
  library(dplyr)

  if (is.character(raw_transcripts)) {
    raw_transcripts <- read.csv(raw_transcripts)
  } else if (!is.data.frame(raw_transcripts)) {
    stop("'raw_transcripts' must be either a character string representing a file path or a data frame.")
  }

  # Remove cells with CellId == 0
  raw_transcripts <- raw_transcripts %>%
    filter(CellId != 0)

  # Keep only specified z stacks
  raw_transcripts <- raw_transcripts %>%
    filter(z %in% z_stacks)

  # Remove 'SystemControl' probes
  raw_transcripts <- raw_transcripts[
    grep("SystemControl", raw_transcripts$target, invert = TRUE), ]

  # Create unique cell identifier
  raw_transcripts$cell <- paste("c", raw_transcripts$Slide, raw_transcripts$fov, raw_transcripts$CellId, sep = "_")

  # Create GEX matrix from cleaned data and save
  GEX_clean_z_stack <- as.data.frame.matrix(table(raw_transcripts$cell, raw_transcripts$target))

  if (save_matrix) {
    if (is.null(output_dir)) {
      stop("Please specify output_dir when save_matrix is TRUE.")
    }
    csv_path <- file.path(output_dir, "exprMat_zclean.csv")
    write.csv(GEX_clean_z_stack, csv_path, row.names = TRUE)
    message("GEX matrix saved to: ", csv_path)
  }

  return(GEX_clean_z_stack)
}

