
# Density Plots Script Documentation

## Overview
The `densityplots.r` script generates density plots for various metrics from cycling tour data collected via the My Smartbike app. It reads JSON files containing tour data, processes the data, and creates visualizations to compare the distributions of different metrics.

## Dependencies
- R (version 4.0.0 or higher)
- `jsonlite` package
- `ggplot2` package
- `cowplot` package

## Installation
To install the required R packages, use the following commands:
```R
install.packages("jsonlite")
install.packages("ggplot2")
install.packages("cowplot")
```

## Usage
1. Place the JSON files containing tour data in the `data` directory.
2. Run the script using an R environment.
3. The script will read the data, process it, and generate density plots, which will be saved in the `png` directory.

## Script Breakdown

### 1. Load Libraries
The script starts by loading the necessary libraries:
```R
library(jsonlite)
library(ggplot2)
library(cowplot)
```

### 2. Define Plotting Function
A custom function `create_density_plot` is defined to create and save density plots for given metrics:
```R
create_density_plot <- function(data, variable, label, color) {
    p <- ggplot(data, aes_string(x = variable)) +
        geom_density(fill = color, alpha = 0.5) +
        labs(title = paste("Density Plot of", label),
             x = label,
             y = "Density") +
        theme_minimal()
    return(p)
}
```

### 3. Read and Process Data
The script reads JSON files from the `data` directory and processes them:
```R
files <- list.files(path = "../data", pattern = "*.json", full.names = TRUE)
data_list <- lapply(files, function(x) fromJSON(file = x))
combined_data <- do.call(rbind, data_list)
```

### 4. Generate and Save Plots
Density plots are generated for each metric and saved as PNG files:
```R
metrics <- list(
    list("sp", "Speed", "blue"),
    list("cp", "Cadence", "red"),
    list("rw", "Rider Power", "green"),
    list("mw", "Motor Power", "purple")
)

for (metric in metrics) {
    p <- create_density_plot(combined_data, metric[[1]], metric[[2]], metric[[3]])
    ggsave(filename = paste0("../png/density_", metric[[1]], ".png"), plot = p)
}
```

## Output
The script produces density plots for the following metrics:
- Speed (`sp`)
- Cadence (`cp`)
- Rider Power (`rw`)
- Motor Power (`mw`)

These plots are saved in the `png` directory with filenames corresponding to the metric names.

## Notes
- Ensure that the `data` and `png` directories exist before running the script.
- The script assumes that the JSON files follow the structure provided by the My Smartbike app.

## License
This script is licensed under the GPL-3.0 License.

For further details, refer to the [GitHub repository](https://github.com/Byggvir/Radtouren).
