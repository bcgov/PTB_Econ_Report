## Minimal renderer: no functions, no git
## Usage (terminal): Rscript 0_render_peter.R

## Determine the directory where this script lives so that
## relative paths work no matter the current working directory.
get_script_dir <- function() {
  # When run with Rscript
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  idx <- grep(file_arg, cmd_args)
  if (length(idx) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", cmd_args[idx[1]]))))
  }

  # When sourced from another R session
  frame1 <- sys.frames()[[1]]
  if (!is.null(frame1$ofile)) {
    return(dirname(normalizePath(frame1$ofile)))
  }

  # Fallback: current working directory
  getwd()
}

script_dir <- get_script_dir()

input_file  <- file.path(script_dir, "0_master_peter.qmd")
output_dir  <- file.path(script_dir, "results")
output_type <- "docx"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

user <- Sys.getenv("USERNAME", unset = Sys.getenv("USER", unset = "user"))
ts   <- format(Sys.time(), "%Y%m%d_%H%M%S")

message("Rendering ", input_file, " ...")
status <- system2("quarto", c("render", input_file, "--to", output_type))
if (!identical(status, 0L)) stop("Quarto render failed with status ", status)

src  <- sub("\\.qmd$", paste0(".", output_type), input_file)
if (!file.exists(src)) stop("Expected output not found: ", src)
dest <- file.path(output_dir, sprintf("report_%s_%s.%s", user, ts, output_type))
if (!file.copy(src, dest, overwrite = TRUE)) stop("Failed to write ", dest)
message("Saved: ", dest)

# Keep only the most recent 10 results
keep_n <- 10L
files <- list.files(output_dir, pattern = "^report_.*\\.docx$", full.names = TRUE)
if (length(files) > keep_n) {
  finfo <- file.info(files)
  ord <- order(finfo$mtime, decreasing = TRUE)
  files_ord <- files[ord]
  to_remove <- files_ord[(keep_n + 1):length(files_ord)]
  for (f in to_remove) {
    ok <- FALSE
    suppressWarnings({ ok <- file.remove(f) })
    if (!ok) message("WARN: Could not remove old result: ", f)
  }
}
