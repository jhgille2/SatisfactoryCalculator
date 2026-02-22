# Satisfactory Calculator
Exploring the recipe data for satisfactory and (maybe) making some sort of calculator to help with base design.

* **UPDATE**: I made some sort of calculator to help with base design. It's kinda scuffed though, but it's controlled with the \_targets.R script (or now with a **Shiny app** for a more user-friendly interface!).

## Quick Start

### Option 1: Shiny Web App (Recommended for most users)
A modern web-based interface for the calculator:

```r
shiny::runApp("app.R")
```

This launches an interactive web app where you can:
- Select products to produce and required quantities
- Specify available resources
- Enable alternate recipes you've unlocked
- View optimized factory networks in an interactive graph

📖 **Getting started?** See [QUICKSTART.md](QUICKSTART.md)
📚 **Full documentation**: See [SHINY_APP_README.md](SHINY_APP_README.md)
⚙️ **Installation help**: See [INSTALLATION_GUIDE.md](INSTALLATION_GUIDE.md)

### Option 2: Targets Pipeline (for advanced users)
Use the R targets workflow for programmatic control:

```r
# Edit _targets.R with your products, resources, and alternate recipes
# Then run:
tar_make()

# Load and view results
tar_load(CytoscapeReady_factoryCounts)
```

See the comments in `_targets.R` for detailed instructions.
