## Minimal renderer: no functions, no git
## Usage (terminal): quarto run render_minimal.R

input_file  <- "0_master_peter.qmd"
output_dir  <- "results"
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
