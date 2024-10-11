
# When happy with the qmd files, run these functions.

# Create Code
knitr::purl(input = "notes.qmd", output = "IMCC7_Code.R")

# Render the files so the latest .html files are available
quarto::quarto_render()

# Update zip with new bits and pieces
utils::zip(zipfile = "docs/resources/IMCC7_ConsPlanWorkshop.zip",
           files = c("IMCC7_Code.R",
                     "Output",
                     "Input",
                     "utils-functions.R",
                     "docs/notes.html",
                     "docs/data.html",
                     "Marxan_newMaPP_Tutorial_Galapagos_Oct2024.pdf"
                     ))
