# ============================================================
# MOBACO Shiny App - Main Entry Point
# Multi-Objective Balance and Coverage Optimization
# Developed at Universitaet Ulm
# ============================================================

# Source files
source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run App
shinyApp(ui = ui, server = server)
