# Troubleshooting Guide - Satisfactory Calculator Shiny App

## Table of Contents
1. [Installation Issues](#installation-issues)
2. [Runtime Errors](#runtime-errors)
3. [Optimization Failures](#optimization-failures)
4. [Visualization Problems](#visualization-problems)
5. [Performance Issues](#performance-issues)
6. [Data Issues](#data-issues)

## Installation Issues

### Issue: "Error: package '...' is not available"

**Cause**: A required package is not installed or the wrong version is installed.

**Solution**:
```r
# Install the missing package
install.packages("package_name")

# Or reinstall all dependencies using renv
renv::restore()
```

**Specific package help**:
- **igraph**: May require compilation. See INSTALLATION_GUIDE.md for compiler setup
- **visNetwork**: Usually installs without issues
- **shiny/shinydashboard**: Requires internet connection for first installation

---

### Issue: "Error in here::here(...): ..is not recognized"

**Cause**: Working directory is not set to project root.

**Solution**:
```r
# Set working directory to project root (where app.R is)
setwd("/path/to/SatisfactoryCalculator")

# Or open the .Rproj file in RStudio, which sets it automatically

# Then run:
shiny::runApp("app.R")
```

---

### Issue: "Error in source('./R/...'): file not found"

**Cause**: R functions in the /R folder cannot be found.

**Solution**:
1. Verify all files are in the `R/` subdirectory
2. Check that working directory is the project root
3. Manually source files:
```r
source("./R/get_recipe_data.R")
source("./R/clean_recipe_json.R")
# ... etc
```

---

### Issue: Multiple package version conflicts

**Cause**: Different versions of packages are installed than expected.

**Solution**:
```r
# Full reinstall
renv::restore(clean = TRUE)

# Or manually update packages
update.packages(ask = FALSE)
```

---

## Runtime Errors

### Issue: App starts but crashes immediately

**Cause**: Recipe data file not found or unreadable.

**Solution**:
1. Verify recipe JSON exists:
```r
file.exists("data/recipe_jsons/en-US_utf8.json")
```

2. Check file permissions (should be readable)

3. Verify JSON file is not corrupted:
```r
tryCatch(
  jsonlite::fromJSON("data/recipe_jsons/en-US_utf8.json"),
  error = function(e) print(paste("JSON Error:", e$message))
)
```

---

### Issue: Error loading recipe data at startup

**Cause**: JSON file encoding issue or missing fields.

**Solution**:
1. Check file encoding (should be UTF-16LE or UTF-8):
```r
readLines("data/recipe_jsons/en-US_utf8.json", n = 1, encoding = "UTF-16LE")
```

2. Try alternative encoding:
```r
# In clean_recipe_json.R, try:
docs_json <- fromJSON(readLines("data/recipe_jsons/en-US_utf8.json", 
                                 skipNul = T, 
                                 encoding = "UTF-8"))  # Change encoding here
```

---

### Issue: "... in .\<Operator\>: object not found"

**Cause**: Function reference issue or missing environment variable.

**Solution**:
```r
# Restart R and clear environment
.rs.restartR()

# Manually load functions again
source("./packages.R")
lapply(list.files("./R", full.names = TRUE), source)

# Re-run app
shiny::runApp("app.R")
```

---

## Optimization Failures

### Issue: "Error: Please select at least one product"

**Cause**: No product selected or quantity is 0.

**Solution**:
1. Click "Add Product"
2. Select a product from dropdown
3. Enter quantity > 0
4. Do NOT leave dropdown empty

---

### Issue: "Error: Please select at least one resource"

**Cause**: No resource selected.

**Solution**:
1. Click "Add Resource"
2. Select a resource from dropdown
3. Enter available quantity (positive number)
4. Can have multiple resources with same item (multiple rows)

---

### Issue: Optimization runs but produces no solution

**Cause**: The linear program is infeasible (impossible to satisfy constraints).

**Possible causes**:
- Resource constraints too tight
- Requested product ratio is mathematically impossible
- Alternate recipes don't help
- Missing required base resources

**Solutions**:
1. Increase available resource quantities
2. Try different product ratios
3. Remove alternate recipes (test if they're causing issues)
4. Use simpler product combinations first
5. Check if all required raw materials are available

---

### Issue: Solver takes very long time (>30 seconds)

**Cause**: Linear program is very large and complex.

**Solutions**:
1. **Reduce products**: Use fewer products (start with 1-3)
2. **Reduce resources**: Specify only necessary resources
3. **Use Integer Factories OFF**: Easier to solve
4. **Use Allow Slack ON**: Usually easier to solve
5. **Restart R**: Clear memory cache
```r
.rs.restartR()
```

---

### Issue: "Not Optimal" status in results

**Cause**: Solver didn't find perfect solution (rare).

**Solution**:
1. Try adjusting product quantities
2. Try adjusting resource quantities
3. Disable some alternate recipes
4. Run again (randomness in solver)

---

## Visualization Problems

### Issue: Graph shows no nodes/edges

**Cause**: Optimization didn't run or the solution is empty.

**Solution**:
1. Check status message for errors
2. Verify optimization actually ran (status ≠ "Ready to optimize")
3. Check if solution has valid data:
```r
# In browser console or R:
View(lp_result)  # See raw solution data
```

---

### Issue: Graph layout is very messy/overlapping

**Cause**: Network is complex and default layout doesn't work well.

**Solution**:
1. **Zoom in/out**: Use mouse wheel in graph
2. **Pan**: Click and drag the graph
3. **Simplify**: Try with fewer products to reduce complexity
4. **Wait**: Physics simulation might improve with time (takes a few seconds)

**Manual improvement**:
- Click on nodes to select them
- Drag nodes to organize layout
- Use visNetwork built-in layout tools (toolbar on graph)

---

### Issue: Node labels are cut off or hard to read

**Cause**: Default font size/spacing settings.

**Solution**:
1. Zoom in on graph to see better
2. Hover over nodes to see full names
3. In app.R, adjust node size:
```r
# In the renderVisNetwork function, change:
size = case_when(
  !is.na(nFactories) ~ as.numeric(nFactories) * 20 + 30,  # Increase multiplier
  TRUE ~ 50  # Increase base size
)
```

---

### Issue: Colors are wrong or not showing

**Cause**: CSS styling issue or color definition error.

**Solution**:
1. Hard refresh browser: Ctrl+Shift+R (or Cmd+Shift+R on Mac)
2. Clear browser cache: Settings → Clear Browsing Data
3. Try different browser

---

## Performance Issues

### Issue: Browser becomes sluggish/unresponsive during optimization

**Cause**: R process consuming CPU, blocking UI.

**Solution**:
1. This is expected - optimization blocks the UI
2. Wait for "Optimization complete!" message
3. Optimize simpler problems
4. Use fewer products/resources
5. Enable "Integer Factories" or "Allow Slack" for easier problems

**Future improvement**: Run optimization in background job:
```r
# Could be implemented using R 4.1+ futures
plan(multisession)
tar_future()
```

---

### Issue: Memory error during large optimization

**Cause**: Linear program size exceeds available RAM.

**Symptoms**: R crashes or becomes very slow.

**Solutions**:
1. **Immediate**: Restart R and try simpler problem
2. **Short-term**: Reduce number of products/resources
3. **Long-term**: Run on machine with more RAM
4. **Alternative**: Use integer factories (smaller problem)

**Monitor memory**:
```r
# Check available memory
gc()  # Garbage collection
memory.limit()  # Show memory limit
```

---

## Data Issues

### Issue: Expected products/resources don't appear in dropdowns

**Cause**: Items not in recipe database.

**Solution**:
1. Verify item exists in game (correct name spelling)
2. Check if it appears in the recipe data:
```r
# In R console:
tar_load(RecipeData)
View(RecipeData$AllRecipes)
# Search for missing item

# Or:
# available_products <- <from app>
# grep("search_term", available_products, value = TRUE)
```

3. Item might be a building, not a component
4. Item might be from DLC/Update not in current JSON

---

### Issue: Alternate recipe doesn't appear in dropdown or is rejected

**Cause**: Recipe slug might be wrong or recipe doesn't exist.

**Solution**:
1. Verify spelling of recipe slug
2. Get correct slug from recipe data:
```r
# In R:
recipe_data$AllRecipes %>%
  filter(alternate == TRUE) %>%
  select(slug, name) %>%
  distinct()
```

3. Try without alternate first to test basic functionality

---

### Issue: Solution uses recipes different than expected

**Cause**: Solver found a different optimal path (valid solution).

**Solution**:
This is normal! The solver finds the optimal solution mathematically, which might differ from expected intuition. It's still a valid solution.

If you want to force specific recipes:
1. Disable conflicting alternate recipes
2. Or use the targets pipeline for more control

---

## Still Having Issues?

### Debug Steps:

1. **Check logs** - R console shows detailed error messages
2. **Test standalone** - Try loading functions manually:
```r
source("./packages.R")
source("./R/clean_recipe_json.R")
source("./R/make_current_recipes.R")
# ... etc, then test functions directly
```

3. **Minimal reproduction** - Try with simplest possible inputs
4. **Check versions**:
```r
sessionInfo()  # Shows all package versions
```

5. **File a details issue** with:
   - Error message (full text)
   - Inputs that caused error
   - R version and OS
   - Package versions (from `sessionInfo()`)

---

## Quick Reference: Common Commands

```r
# Restart everything
.rs.restartR()

# Check working directory
getwd()

# Set working directory  
setwd("C:/path/to/SatisfactoryCalculator")

# List loaded packages
(.packages())

# Show R version
R.version$version.string

# Check specific file
file.exists("data/recipe_jsons/en-US_utf8.json")

# Free memory
gc()

# Test a function
try(clean_recipe_json("data/recipe_jsons/en-US_utf8.json"))
```

---

## Getting Help

If you still need help:

1. Run the diagnostic:
```r
source("./packages.R")
lapply(list.files("./R", full.names = TRUE), source)
message("Diagnostic: App should be ready")
```

2. Share error message + `sessionInfo()` output
3. Describe exact steps to reproduce
4. Note your OS and R version
