#' Update Metadata and Polygons Data Frames
#'
#' This function updates metadata and polygons data frames based on the provided clean_GEX data frame. It recalculates RNA counts and features, filters and arranges the metadata and polygons to match clean_GEX, and optionally saves the updated data frames to specified flat files.
#'
#' @param clean_GEX A data frame or a character string representing the file path to a CSV file containing the clean_GEX data. The data frame must have row names corresponding to cell identifiers.
#' @param metadata A data frame or a character string representing the file path to a CSV file containing the metadata. The data frame must have a column named 'cell'.
#' @param polygons A data frame or a character string representing the file path to a CSV file containing the polygons data (optional). The data frame must have a column named 'cell'.
#' @param save_flat_files A logical value indicating whether to save the updated metadata and polygons to flat files. Default is FALSE.
#' @param output_dir A character string representing the directory where the updated flat files will be saved. Required if save_flat_files is TRUE.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads the clean_GEX data from the provided data frame or file path.
#' 2. Reads the metadata from the provided data frame or file path.
#' 3. Ensures the metadata contains a column named 'cell'.
#' 4. Filters and arranges the metadata to match the row names of clean_GEX.
#' 5. Identifies negative probes (columns containing 'Neg') and regular probes.
#' 6. Recalculates nCount_RNA and nFeature_RNA excluding negative probes.
#' 7. Recalculates nCount_negprobes and nFeature_negprobes.
#' 8. Removes columns 'nCount_falsecode' and 'nFeature_falsecode' if they exist in the metadata.
#' 9. Optionally reads the polygons data from the provided data frame or file path, filters and arranges it to match the row names of clean_GEX.
#' 10. Optionally saves the updated metadata and polygons to the specified output directory.
#'
#' @return A list containing the updated metadata and polygons data frames.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' updated_data <- UpdateMetadataAndPolygons(clean_GEX = "path/to/clean_GEX.csv",
#'                                           metadata = "path/to/metadata.csv",
#'                                           polygons = "path/to/polygons.csv",
#'                                           save_flat_files = TRUE,
#'                                           output_dir = "path/to/output/dir")
#' }
#'
#' @export
UpdateMetadataAndPolygons <- function(clean_GEX, metadata, polygons = NULL, save_flat_files = FALSE, output_dir = NULL) {
  # Load required libraries
  library(dplyr)

  # Read clean_GEX
  if (is.character(clean_GEX)) {
    clean_GEX <- read.csv(clean_GEX, row.names = 1)
  } else if (!is.data.frame(clean_GEX)) {
    stop("'clean_GEX' must be either a character string representing a file path or a data frame.")
  }

  # Read metadata
  if (is.character(metadata)) {
    metadata <- read.csv(metadata)
  } else if (!is.data.frame(metadata)) {
    stop("'metadata' must be either a character string representing a file path or a data frame.")
  }

  # Ensure metadata has a column 'cell'
  if (!'cell' %in% colnames(metadata)) {
    stop("Metadata must contain a column named 'cell'.")
  }

  # Filter and arrange metadata to match clean_GEX
  metadata <- metadata %>% filter(cell %in% rownames(clean_GEX)) %>% arrange(match(cell, rownames(clean_GEX)))

  # Identify negative and regular probes
  neg_probes <- grep("Neg", colnames(clean_GEX), value = TRUE)
  reg_probes <- setdiff(colnames(clean_GEX), neg_probes)

  # Update metadata with recalculated counts and features
  metadata <- metadata %>%
    mutate(nCount_RNA = rowSums(clean_GEX[metadata$cell, reg_probes], na.rm = TRUE),
           nFeature_RNA = rowSums(clean_GEX[metadata$cell, reg_probes] > 0, na.rm = TRUE),
           nCount_negprobes = ifelse(length(neg_probes) > 0, rowSums(clean_GEX[metadata$cell, neg_probes], na.rm = TRUE), 0),
           nFeature_negprobes = ifelse(length(neg_probes) > 0, rowSums(clean_GEX[metadata$cell, neg_probes] > 0, na.rm = TRUE), 0)) %>%
    select(-contains("falsecode"))

  # Read polygons if provided
  if (!is.null(polygons)) {
    if (is.character(polygons)) {
      polygons <- read.csv(polygons)
    } else if (!is.data.frame(polygons)) {
      stop("'polygons' must be either a character string representing a file path or a data frame.")
    }

    # Filter and arrange polygons to match clean_GEX
    polygons <- polygons %>% filter(cell %in% rownames(clean_GEX)) %>% arrange(match(cell, rownames(clean_GEX)))
  }

  if (save_flat_files) {
    if (is.null(output_dir)) {
      stop("Please specify output_dir when save_flat_files is TRUE.")
    }

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Save updated metadata
    write.csv(metadata, file.path(output_dir, "updated_metadata.csv"), row.names = FALSE)
    message("Updated metadata saved to: ", file.path(output_dir, "updated_metadata.csv"))

    # Save updated polygons if available
    if (!is.null(polygons)) {
      write.csv(polygons, file.path(output_dir, "updated_polygons.csv"), row.names = FALSE)
      message("Updated polygons saved to: ", file.path(output_dir, "updated_polygons.csv"))
    }
  }

  return(list(metadata = metadata, polygons = polygons))
}
