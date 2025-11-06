# render_report.R

# Function to render report with timestamp and username
render_with_timestamp <- function(input_file = "0_master_yearfix.qmd",
                                  output_dir = "results",
                                  output_format = "docx",
                                  include_git_branch = FALSE,
                                  custom_name = NULL) {
  # Create results directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get username and timestamp
  user <- Sys.getenv("USERNAME", unset = Sys.getenv("USER", unset = "user"))
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Get git branch name if in a git repository
  branch_suffix <- ""
  if (isTRUE(include_git_branch)) {
    git_branch <- try({
      system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
    }, silent = TRUE)
    if (!inherits(git_branch, "try-error") && length(git_branch) == 1 && nzchar(git_branch)) {
      branch_suffix <- paste0("_", git_branch)
    }
  }
  
  # Generate output filename
  output_file <- if (!is.null(custom_name) && nzchar(custom_name)) {
    sprintf("report_%s_%s.%s", user, custom_name, output_format)
  } else {
    sprintf("report_%s_%s%s.%s", user, timestamp, branch_suffix, output_format)
  }
  output_path <- file.path(output_dir, output_file)
  
  # Render the document
  message("Rendering ", input_file)
  
  # Prefer quarto R package if available, else CLI fallback
  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_render(input = input_file, output_format = output_format)
  } else {
    # Fallback to Quarto CLI
    status <- system2("quarto", c("render", input_file, "--to", output_format))
    if (!identical(status, 0L)) stop("Quarto CLI render failed with status ", status)
  }
  
  # Move the output to desired location
  temp_file <- sub("\\.qmd$", paste0(".", output_format), input_file)
  if (file.exists(temp_file)) {
    file.rename(temp_file, output_path)
    message("Success! Report saved as: ", output_path)
  } else {
    stop("Rendering failed or output file not found")
  }
  
  # List files in results directory
  message("\nFiles in ", output_dir, " directory:")
  files <- list.files(output_dir, full.names = TRUE)
  file_info <- file.info(files)
  file_info$name <- basename(rownames(file_info))
  print(file_info[order(file_info$mtime, decreasing = TRUE), 
                 c("name", "mtime", "size")])
  
  return(output_path)
}

# If this script is run directly (not sourced), render the default file
if (sys.nframe() == 0) {
  render_with_timestamp(input_file = "0_master_yearfix.qmd", include_git_branch = FALSE)
}
