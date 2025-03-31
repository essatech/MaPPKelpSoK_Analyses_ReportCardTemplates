# FILE: renderAllCards.R
# DESCRIPTION: An R script to render HTML report cards using RMarkdown templates;
#   each run of the script can call new data sheets with additional years of monitoring 
#   data.
# LAST MODIFIED: March 28, 2025
# AUTHOR: Mairin Deith (mdeith@essa.com)

# First, render a public report card; the same RMarkdown file is used to generate
#   both a public and private report card
rmarkdown::render(input = file.path("subregional_exampleBullKelpReportCard.Rmd"), 
    # Set the output file name to reflect the audience
    output_file = "egSubregionalReportCard_Public.html",
    # Identify how to render the file using input parameters
    params = list(
        # "public" or "internal" audiences - changes what types of information is shown
        # and whether data are masked
        audience = "public",
        # High or low map resolution; rendering a high resolution map can take several minutes.
        mapresolution = "low", 
        # Full file path to the most up-to-date datasheet in the standardized data 
        # entry format
        datasheet = file.path("eg_DataSheets/BullKelpForm_Full_Mod.xlsm")))

# Next, render an internal-facing public report card
rmarkdown::render(file.path("subregional_exampleBullKelpReportCard.Rmd"), 
    # Custom output file name 
    output_file = "egSubregionalReportCard_Internal.html",
    params = list(
        # "public" or "internal" audiences - changes what types of information is shown
        # and whether data are masked
        audience = "internal",
        # High or low map resolution; rendering a high resolution map can take several minutes.
        mapresolution = "low", 
        # Full file path to the most up-to-date datasheet in the standardized data 
        # entry format
        datasheet = file.path("eg_DataSheets/BullKelpForm_Full_Mod.xlsm")))

# Render a regional report card (using fake generated data)
rmarkdown::render(file.path("subregional_exampleBullKelpReportCard.Rmd"),
    output_file = "egRegionalReportCard_public.html")

