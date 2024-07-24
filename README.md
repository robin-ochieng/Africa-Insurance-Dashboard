# Life Insurance in Africa Dashboard

## Overview

This Shiny dashboard provides an interactive overview of life insurers across African countries. It includes various visualizations and data tables to help users understand the implementation status of IFRS17, gross written premiums, and distribution of insurers based on premium categories.

## Features

- **Map Visualization**: 
  - Count of insurers by gross written premium categories (Below $40M and Above $40M).
  - Gross written premium distribution across Africa.
- **Dashboard**:
  - Key metrics such as total gross written premium, average gross written premium per insurer, and IFRS17 implementation status.
  - Interactive donut chart showing the IFRS17 implementation status by country.
  - Interactive table for life insurers filtered by country and gross written premium category.
  - Bar plot showing gross written premium by life insurers.

## Data

The data for this dashboard is sourced from an Excel file (`data.xlsx`) containing information about life insurers and IFRS17 implementation status. The Excel file consists of two sheets:
- `Insurers`: Contains information about life insurers, including their country, latitude, longitude, and gross written premium in USD.
- `IFRS17`: Contains information about the IFRS17 implementation status by country.

## Installation

1. **Clone the repository**:
    ```sh
    git clone https://github.com/yourusername/life-insurance-africa-dashboard.git
    ```

2. **Install the required packages**:
    ```r
    install.packages(c("shiny", "ggplot2", "plotly", "scales", "bs4Dash", "DT", "readxl", "dplyr", "bslib", "shinycssloaders", "leaflet", "sf", "rnaturalearth", "viridis", "tidyverse"))
    ```

3. **Run the Shiny app**:
    ```r
    shiny::runApp()
    ```

## Usage

- **Map Tab**: Visualize the distribution of life insurers based on gross written premium categories and total gross written premium across African countries.
- **Dashboard Tab**: View key metrics and visualizations, including the IFRS17 implementation status, gross written premium distribution, and an interactive table of life insurers.

## Custom Theme

The dashboard uses a custom theme defined using the `bslib` package. The theme includes a dark background with contrasting text and highlights for an improved user experience.

## Screenshots

![image](https://github.com/user-attachments/assets/07e73419-4c1f-488e-9302-64739781ec3a)

![image](https://github.com/user-attachments/assets/0edf4a3b-f76c-407c-b8d0-ea08e3b4ed77)


## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## Acknowledgements

- The `bs4Dash` package for creating a beautiful dashboard layout.
- The `plotly` package for interactive visualizations.
- The `leaflet` package for map visualizations.
- The `readxl` package for reading data from Excel files.
- The `rnaturalearth` package for natural earth data.

## Contact

For any questions or feedback, please contact [Robin Ochieng](:robinochieng73@gmail.com).

