# System Requirements for Satisfactory Calculator Shiny App

## Required R Packages

The following packages are required to run the Shiny app. They should all be installed automatically when you run `renv::restore()` in the project, but here's the complete list:

### Core Packages
- `shiny` (>= 1.6.0) - Web application framework
- `shinydashboard` (>= 0.7.0) - Dashboard template for Shiny

### Data Manipulation
- `dplyr` - Data frame operations
- `tidyr` - Data tidying
- `stringr` - String manipulation
- `purrr` - Functional programming
- `tibble` - Modern data frames

### Optimization
- `lpSolve` - Linear programming solver
- `igraph` - Graph/network data structures

### Visualization
- `visNetwork` - Interactive network visualization (essential for viewing factory networks)
- `ggplot2` - Plot generation (optional for advanced visualizations)
- `ggthemes` - Themes for ggplot (optional)
- `ggrepel` - Smart text labels for plots (optional)

### File I/O & Utilities
- `jsonlite` - JSON parsing
- `here` - Path management
- `magrittr` - Pipe operations
- `conflicted` - Conflict resolution

### Secondary (used by dependencies but not directly)
- `targets` - Workflow orchestration (loaded but not used directly in app)
- `tarchetypes` - Extended targets machinery
- `RCy3` - Cytoscape interface (optional for advanced users)
- `pbapply` - Progress bar apply
- `lhs` - Latin hypercube sampling
- `furrr` - Parallel functional programming
- `job` - Background job processing

## Installation

### Option 1: Using renv (Recommended)
The project already has a lockfile at `renv.lock`. Simply run:

```r
renv::restore()
```

This will install all packages at the exact versions specified in the lockfile.

### Option 2: Manual Installation
If you don't use renv, install packages manually:

```r
# Core packages needed for the app
install.packages(c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "tibble",
  "lpSolve",
  "igraph",
  "visNetwork",
  "jsonlite",
  "here",
  "magrittr",
  "conflicted"
))

# Optional but recommended
install.packages(c(
  "ggplot2",
  "ggthemes",
  "ggrepel"
))
```

## System Requirements

- **R Version**: 4.1 or higher (4.4+ recommended)
- **Operating System**: Windows, macOS, or Linux
- **RAM**: At least 4GB (8GB+ recommended for large optimizations)
- **Network**: Not required (app runs locally)

## Optional: Cytoscape Integration

If you want to export results to Cytoscape for more advanced network analysis:

```r
install.packages("RCy3")
```

And ensure Cytoscape is installed on your system. However, the Shiny app provides good visualization without this.

## Testing the Installation

To verify everything is installed correctly, run this in R:

```r
required_packages <- c(
  "shiny", "shinydashboard", "dplyr", "tidyr", "stringr",
  "purrr", "tibble", "lpSolve", "igraph", "visNetwork",
  "jsonlite", "here", "magrittr", "conflicted"
)

missing <- required_packages[!sapply(required_packages, function(x) {
  suppressWarnings(require(x, character.only = TRUE, quietly = TRUE))
})]

if (length(missing) > 0) {
  cat("Missing packages:", paste(missing, collapse = ", "), "\n")
  cat("Install with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))\n")
} else {
  cat("All required packages are installed!\n")
}
```

## Memory Considerations

The optimization involves:
- Loading recipe data (~50-100MB): Loaded once at app startup
- Linear programming matrices: Depends on product/resource complexity
  - Simple optimization (2-5 products, 5-10 resources): <100MB
  - Complex optimization (10+ products, 15+ resources): 100-500MB

If you experience memory issues:
- Close other applications
- Use fewer products/resources in a single optimization
- Consider running on a machine with more RAM

## Troubleshooting Installation Issues

### Issue: "package 'igraph' is not available"
**Solution**: Make sure R is up to date. Run `updateR()` in R or update R manually.

### Issue: "Could not install package 'visNetwork'"
**Solution**: If using Windows, you may need to install RTools. Download from https://cran.r-project.org/bin/windows/Rtools/

### Issue: "Compilation failed for package 'lpSolve'"
**Solution**: This requires a C compiler. 
- **Windows**: Install RTools (see above)
- **macOS**: Install Xcode Command Line Tools: `xcode-select --install`
- **Linux**: Install build tools: `sudo apt-get install build-essential`

### Issue: Memory errors during optimization
**Solution**: The linear program might be too large. Try:
1. Reducing the number of products
2. Reducing the number of resources
3. Using integer factories = TRUE (simpler problem)
4. Restarting R to clear memory: `.rs.restartR()`
