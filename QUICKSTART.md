# Quick Start Guide for Satisfactory Factory Calculator Shiny App

## Getting Started

### Step 1: Launch the App

In your R console, navigate to the project directory and run:

```r
shiny::runApp("app.R")
```

The app will open at `http://localhost:3838` in your default browser.

### Step 2: Use the Example Below

Here's a simple example to test the app with **Space Elevator Parts (Update 6+)**:

#### Products to Produce:
- **Desc_SpaceElevatorPart_2_C**: quantity **1000**
- **Desc_SpaceElevatorPart_3_C**: quantity **100**

(The relative quantities determine the production rate ratio)

#### Available Resources:
- **Desc_OreIron_C**: **120**
- **Desc_OreCopper_C**: **60**
- **Desc_Coal_C**: **120**
- **Desc_Stone_C**: **60**
- **Desc_Water_C**: **999999** (effectively unlimited)

#### Alternate Recipes:
- **alternate:-iron-wire** (pre-unlocked as default)

#### Settings:
- Integer Factories: **OFF** (allows fractional factories/underclocking)
- Allow Slack: **ON** (produces extra intermediates if beneficial)

### Step 3: Click "Run Optimization"

The app will:
1. Load the recipe data
2. Apply your alternate recipes
3. Solve the linear program
4. Display the factory network

### Step 4: Examine the Results

The network shows:
- **Recipes and Items** as nodes
- **Material flows** as edges with production rates
- **Node sizes** indicating factory counts needed

## Different Example: Basic Tier 1

### Products to Produce:
- **Desc_IronPlate_C**: **300**
- **Desc_IronRod_C**: **80**
- **Desc_Copper_C**: **128**

### Available Resources:
- **Desc_OreIron_C**: **360**
- **Desc_OreCopper_C**: **200**

### Settings:
- Integer Factories: **ON** (no fractional factories)
- Allow Slack: **OFF** (strict ratios)

## Tips for Best Results

### 1. Understand Resource Constraint
The solver tries to find the factory that:
- Produces your desired items at rates proportional to the quantities you enter
- Uses all available resources (or constraints are hit)

**Tip**: If results seem small, you may need larger resource quantities or different product ratios.

### 2. Choose Realistic Quantities
Products like "Space Elevator Parts" might need quantities like 1000:100 (10:1 ratio)
Basic components like "Iron Plate" might be 100:50 (2:1 ratio)

**Experiment**: Try different ratios to see what makes sense for your game progress.

### 3. Toggle Integer Factories
- **ON**: Whole factories, but solver might reject impossible ratios
- **OFF**: Allows underclocking factories for exact ratios

### 4. Toggle Allow Slack
- **ON**: Produces extra intermediates (more realistic for actual gameplay)
- **OFF**: Exact production matching (mathematical exactness)

### 5. Water
Water is not a limiting resource in game. You can:
- Set it to very high (999999999)
- Or use it as a limiting material if you want to simulate water constraints

## Common Issues & Solutions

### "Error: Please select at least one product"
**Fix**: Make sure you've selected a product from the dropdown AND entered a quantity > 0

### "Error: Please select at least one resource"
**Fix**: Select at least one resource and ensure quantity > 0

### Graph shows no nodes
**Fix**: 
1. Check the status message for errors
2. Verify all products are valid (exist in recipe data)
3. Verify all resources are valid
4. Try with fewer products initially

### Results seem unrealistic (very small quantities)
**Fix**: 
1. Increase available resource quantities
2. Use larger product requirement quantities
3. Check that you're not asking for impossible ratios

### Solver says it found no solution
**Causes**:
1. Requested ratio is impossible with available recipes
2. Resources are too limited
3. An alternate recipe is broken or incompatible
4. Linear program is infeasible

**Fix**:
1. Increase resource quantities
2. Disable some alternate recipes to test
3. Try a simpler product combination first

## Finding Recipe Slugs

To find the correct product/recipe names:

1. Load recipe data in R console:
```r
tar_load(RecipeData)
View(RecipeData$AllRecipes)
```

2. Look for the `slug`, `product_item`, and `ingredient_item` columns

3. Use those exact names in the app

## Advanced Configuration

### Modifying Input Defaults
Edit the default values in app.R:

```r
# Change initial quantity for products
products <- reactiveVal(data.frame(id = 1, item = "", qty = 500, ...))

# Change initial quantity for resources  
resources <- reactiveVal(data.frame(id = 1, item = "", qty = 500, ...))
```

### Adding Custom Resource Lists
You can hardcode common resources by adding defaults to the UI. Look for these sections:

```r
# In the box for Available Resources
box(title = "Available Resources",
    # Add preset buttons here
    actionButton("preset_basic_resources", "Basic Resources"),
    # ...
)
```

### Batch Running Multiple Optimizations
You can run the app in a loop to test multiple configurations:

```r
# In R, after the app is running:
# Modify inputs programmatically and read results
# (requires accessing Shiny session object)
```

## Performance Tips

- **Fewer products** = Faster optimization
- **Fewer resources** = Faster optimization
- **Integer Factories OFF** = Faster optimization
- **Allow Slack ON** = Easier to solve (usually faster)

If optimization is slow (>10 seconds):
1. Reduce the number of products
2. Reduce the number of resources
3. Enable Integer Factories (may fail though)
4. Disable some alternate recipes

## Next Steps

1. **Try the example builds** provided above
2. **Experiment** with your current game progress items
3. **Compare results** with your actual factory builds
4. **Iterate** on product quantities to refine designs

## Getting Help

If you encounter issues:
1. Check the status message in the app
2. Review `INSTALLATION_GUIDE.md` for dependency issues
3. Check `SHINY_APP_README.md` for detailed feature documentation
4. Review the comments in `app.R` for implementation details
