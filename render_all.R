

quarto::quarto_render("0_intro", output_format = "docx")


source("setup.R")

# Render individual sections
quarto::quarto_render("0_intro.qmd")
quarto::quarto_render("1_trip_volume.qmd")