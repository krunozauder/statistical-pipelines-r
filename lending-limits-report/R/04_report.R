# 04. REPORT ----
# Report (PDF with XeLaTeX, or HTML if XeLaTeX is not installed) and the Excel file of each lender,
# for the report quarter.
# Titles, captions, notes and sources come from config/texts.yml; figures and tables are numbered
# automatically.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
source(file.path(MODULE, "R", "03_charts_tables.R"))
start_step(4, paste("report for", RES$report_quarter))

n <- write_lender_files()
fmt <- S$report_format %||% "auto"
if (fmt == "auto") fmt <- if (nzchar(Sys.which("xelatex"))) "pdf" else "html"
if (fmt == "html" && (S$report_format %||% "auto") == "auto")
  message("XeLaTeX not found, so the report is written as HTML. For the PDF install TinyTeX once: ",
          "install.packages(\"tinytex\"); tinytex::install_tinytex()")
font <- S$font %||% if (.Platform$OS.type == "windows") "Arial" else ""
out_format <- if (fmt == "pdf") {
  rmarkdown::pdf_document(latex_engine = "xelatex", fig_caption = TRUE,
                          pandoc_args = c("-V", "geometry:margin=2cm", "-V", "fontsize=10pt", if (nzchar(font)) c("-V", paste0("mainfont=", font))))
} else rmarkdown::html_document(self_contained = TRUE, toc = TRUE, fig_caption = TRUE)
# LaTeX cannot handle backslashes or spaces in figure paths (e.g. Windows folders), so the report is
# built in a temporary folder and the result is copied to OUT
wd <- file.path(normalizePath(tempdir(), winslash = "/"), "report_build")
unlink(wd, recursive = TRUE); dir.create(wd)
file.copy(file.path(MODULE, "report", c("report.Rmd", "_lender.Rmd")), wd, overwrite = TRUE)
built <- rmarkdown::render(file.path(wd, "report.Rmd"), output_format = out_format, output_dir = wd,
                           output_file = paste0("Report_", RES$report_quarter, ".", fmt), intermediates_dir = wd,
                           knit_root_dir = wd, envir = environment(), quiet = TRUE)
f <- file.path(OUT, basename(built))
file.copy(built, f, overwrite = TRUE)
cat("Report ->", f, "| Excel files for", n, "lenders\n")
