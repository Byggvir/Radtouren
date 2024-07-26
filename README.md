# Radtouren

## Overview
This repository contains R scripts for analyzing cycling tour data collected from e-bikes with Mahle E-Motors via the My Smartbike app. The app provides detailed tour data which can be exported in JSON format. The scripts in this repository facilitate the comparison of multiple tours by generating various plots and analyses.

## Features
- Import and process tour data exported as individual JSON files.
- Generate density plots, scatter plots and other visualizations for data comparison.
- Analyze various metrics such as support level, cadence, distance, elevation, latitude, longitude, motor power, rider power, slope, speed, torque, and timestamps.

## Usage
1. Export your tour data from the My Smartbike app and place the JSON files into the `data` directory.
2. Run the R scripts to read the tour data and generate plots, which will be saved in the `png` directory.

## Data Metrics
The following data points are extracted and analyzed from the JSON files:
- `al`: Support level
- `aw`: Note (e.g., "PAUSE")
- `cp`: Cadence
- `gd`: Distance
- `ge`: Elevation
- `la`: Latitude
- `lo`: Longitude
- `mw`: Motor power
- `or`: Unknown
- `rw`: Rider power
- `sl`: Slope
- `sp`: Speed
- `tm`: Torque
- `ts`: Timestamp

## Directory Structure
- `data`: Contains JSON files of individual tours.
- `png`: Stores the generated plots.
- `R`: Contains the R scripts for data processing and visualization.
- `bash`: Auxiliary scripts for automation.

## License
This project is licensed under the GPL-3.0 License. See the [LICENSE](LICENSE) file for details.

## Contributing
Contributions are welcome! Please submit pull requests or open issues for any suggestions or improvements.

## Contact
For questions or feedback, please reach out via the GitHub repository.

---

[Repository Link](https://github.com/Byggvir/Radtouren)
