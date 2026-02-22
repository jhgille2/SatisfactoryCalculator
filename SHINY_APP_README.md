# Satisfactory Factory Calculator - Shiny App

## Overview

This is an interactive Shiny web application that provides a user-friendly interface for the Satisfactory Factory Calculator pipeline. Instead of editing `_targets.R` and running targets commands, users can now use this app to input their desired products, available resources, and unlocked alternate recipes, then get an optimized factory network visualization.

## Features

- **Interactive Product Selection**: Add and remove desired products dynamically
- **Resource Management**: Specify available resources (automatically negated as required)
- **Alternate Recipe Support**: Select any alternate recipes you've unlocked in the game
- **Real-time Optimization**: Run the linear programming solver with a single button click
- **Network Visualization**: View the resulting factory network with production rates and factory counts
- **Customizable Settings**: 
  - Integer Factories: Enforce whole factories (no underclocking)
  - Allow Slack: Permit production of extra intermediates beyond requirements

## Running the App

### Prerequisites
Make sure you have all required R packages installed. The app will use the packages loaded in `packages.R`, but you'll also need:
- `shiny`
- `shinydashboard`
- `visNetwork`

Install any missing packages with:
```r
install.packages(c("shiny", "shinydashboard", "visNetwork"))
```

### Starting the App

From the R console in the project directory, run:

```r
shiny::runApp("app.R")
```

The app will open in your default browser at `http://localhost:3838`.

## How to Use

### 1. **Select Products to Produce**
   - Click "Add Product" to add items
   - Select the product name from the dropdown
   - Enter the quantity required for that product
   - Remove products with the × button

### 2. **Specify Available Resources**
   - Click "Add Resource" to add resources
   - Select the resource type from the dropdown
   - Enter the available quantity (enter positive numbers; the app will negate them automatically)
   - The app shows resource quantities as negative values in the solver

### 3. **Enable Alternate Recipes**
   - Click "Add Recipe" to unlock alternate recipes you've discovered
   - Select alternate recipe slugs (e.g., "alternate:-iron-wire")
   - These will be used instead of default recipes where applicable

### 4. **Adjust Settings**
   - **Integer Factories**: Check if you want whole factories only (no underclocking)
   - **Allow Slack**: Check if you want to allow production of extra intermediate items

### 5. **Run Optimization**
   - Click the "Run Optimization" button
   - The app will solve the linear program and display results
   - The status message shows progress and any errors

### 6. **View Results**
   - **Factory Network**: Shows the optimized factory layout with:
     - Blue nodes: Items/recipes
     - Yellow nodes: Final products
     - Edge labels: Production rates
     - Node sizes: Relative to factory counts
   - **Solution Details**: Shows optimization status and objective value

## Understanding the Network Visualization

- **Nodes**: Represent items or recipes
- **Edges**: Show material flows between recipes
- **Edge Labels**: Production rates (items per minute)
- **Node Size**: Reflects the number of factories needed
- **Node Color**: 
  - Blue: Items being produced
  - Yellow: Final products being optimized

## Tips

- **Water Resources**: Water can be set to a very large number (9007199254740991) as it's effectively unlimited
- **No Products?**: The optimization requires at least one product to produce
- **No Resources?**: You must specify at least one available resource
- **Underclocking**: If not using integer factories, the solver may suggest fractional factories (e.g., 1.5 factories). You can underclock to achieve this in the game.
- **Slack**: When enabled, the solver will produce extra intermediates if resources allow. When disabled, it tries to maintain exact ratio requirements.

## Troubleshooting

- **"Error: Please select at least one product"**: Add at least one product and quantity
- **"Error: Please select at least one resource"**: Add at least one available resource
- **Blank Network Graph**: Either no optimization has been run, or there was an error - check the status message
- **Very small quantities**: Some optimal solutions may produce tiny quantities. You can adjust required quantities or available resources to get more reasonable numbers

## Technical Details

The app:
1. Loads the recipe data from `data/recipe_jsons/en-US_utf8.json` on startup
2. Uses the same functions from the `R/` folder as the targets pipeline
3. Runs `factory_binary_search_continuous()` to find optimal production rates
4. Visualizes results using `visNetwork` for interactive exploration
5. All heavy computation is done server-side in R

## Notes

- The app preserves the same mathematical optimization approach as the targets pipeline
- Results can be used directly to inform factory construction in Satisfactory
- The network can be exported from the visNetwork display using the built-in export tools
