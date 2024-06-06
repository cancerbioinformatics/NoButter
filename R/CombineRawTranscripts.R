#' Combine Raw Transcripts
#'
#' This function combines raw transcript data from multiple FOV files in a specified input directory (typically found in AnalysisResults of raw Atomx export data)
#' and saves the combined data to a CSV file in a specified output directory.
#'
#' @param in_dir A string specifying the path to the input directory containing the raw transcript information for each FOV.
#' @param out_dir A string specifying the path to the output directory where the stitched CSV file will be saved.
#' @param file_name A string specifying the name of the output CSV file. Defaults to "raw_transcripts_stitched.csv".
#'
#' @details The function reads all the files from the specified input directory. For each subdirectory (representing
#' a field of view, FOV), it looks for files matching the pattern "complete_code_cell_target_call_coord".
#' If exactly one such file is found, its data is read and appended to a data frame that accumulates data from all FOVs.
#' The accumulated data is then written to a CSV file in the specified output directory.
#'
#' If the input or output directory does not exist, the function stops with an error.
#' If no matching file or multiple matching files are found in a subdirectory, a warning is issued.
#'
#' @return A data frame containing the stitched raw transcript data from all the files in the input directory.
#' The function also saves this data frame as a CSV file in the specified output directory.
#'
#' @examples
#' \dontrun{
#' raw_transcripts <- CombineRawTranscripts("path/to/input/directory", "path/to/output/directory")
#' raw_transcripts <- CombineRawTranscripts("path/to/input/directory", "path/to/output/directory", "custom_file_name.csv")
#' }
#'
#' @export
CombineRawTranscripts <- function(in_dir, out_dir, file_name = "raw_transcripts.csv") {

  if (!dir.exists(in_dir)) {
    stop("Input directory does not exist: ", in_dir)
  }
  if (!dir.exists(out_dir)) {
    stop("Output directory does not exist: ", out_dir)
  }

  output_file_path <- file.path(out_dir, file_name)
  list_of_FOVs <- list.files(in_dir)
  message("FOVs present:", length(list_of_FOVs))

  raw_transcripts <- data.frame()

  for(FOV in list_of_FOVs) {
    message("Stitching transcripts for: ", FOV)

    FOV_dir <- file.path(in_dir, FOV)
    complete_cell_target_file <- list.files(FOV_dir,
                                            pattern = "complete_code_cell_target_call_coord",
                                            full.names = TRUE)

    if (length(complete_cell_target_file) == 1) {
      message("Complete code cell target call coord file for ", FOV, " found")

      csv_data <- read.csv(complete_cell_target_file)

      raw_transcripts <- rbind(raw_transcripts, csv_data)
    } else {
      warning("No or multiple complete cell target files found for ", FOV)
    }
  }

  write.csv(raw_transcripts, output_file_path, row.names = FALSE)

  message("Stitching completed. Output saved to ", output_file_path)

  return(raw_transcripts)
}

