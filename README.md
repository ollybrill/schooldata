# schooldata
Visualisation of data on schools in Waltham Forest.
This is a R Shiny app developed to help parents living in Waltham Forest in London to identify which secondary school to apply for.
The data is from various public sources:

Place allocation: https://www.walthamforest.gov.uk

School performance: www.compare-school-performance.service.gov.uk/

Waltham Forest boundaries with house price (mean data for year end 2017) from: https://data.london.gov.uk/dataset/average-house-prices

Crime data derived from: https://data.london.gov.uk/dataset/recorded_crime_summary 

**Schools List**:
This includes all the non specialist schools in the borough excluding private schools.  A single school can be selected from this list and data for the currently selected school will be highlighted throughout the application.

****Maps Tab****:
Shows a map of the London Borough of Waltham Forest with a marker showing each secondary school.  Clicking on the marker will show the name of the school.

**Cut off year**:
Selecting a year in this list displays a circle on the map with a radius equal to the 'cut off distance' of the selected school in that year.  Children living outside of the 'cut off distance' did not get a place in the school in that year (except children offered places on other criteria such as having a sibling at the school already).  Multiple years can be selected and cut off distances for other schools will remain until the 'Reset Map' button below the map is selected.  If there was no cut off distance in a particular year (no child was refused a place based on distance) then no circle is shown.

***Map Overlay*** -
various overlays can be added to the map:

**Nearest School**:
The shapes outlined in blue help identify the nearest school, depending on the child's sex and home address. Find a street on the map and see which shape outlined in blue the street sits within. The marker at the centre of that shape indicates the nearest school.

**Crime**:
The colour each area is shaded indicates the level of crime reported in that area.  The darker the colour the greater the level of crime reported.

**House Prices**:
The colour each area is shaded indicates the level of house prices in that area.  The darker the colour the greater the level of house prices.

**Allocation table**:
Each tile in the graph below represents a place in the school selected in 2024. The colour of the tile shows how it was allocated.

****Academic Tab****:
Shows data from www.compare-school-performance.service.gov.uk/ as a bar graph.  The currently selected school is highlighted.

****Finance Tab****:
Shows financial data from www.compare-school-performance.service.gov.uk/ as a bar graph.  The currently selected school is highlighted.
