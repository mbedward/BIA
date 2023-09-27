# BIA

An R package to query Building Impact Assessment data (spatial features and
associated photos) provided by NSW Rural Fire Service as an ESRI GeoDatabase.
Written for use by the Centre for Environmental Risk Management of Bushfire,
University of Wollongong, Australia.

## Installation

```r
install.packages("remotes")  # if you don't already have it
remotes::install_github("mbedward/BIA")
```

## Example

``` r
library(BIA)
library(sf)

# Read polygon(s) for the area to query
bounds <- st_read("conjola_boundary.shp")

# Search for BIA point features within the query bounds and
# save associated photos to disk as JPG files. The function 
# returns a spatial data frame of the point features with
# all fields from the BIA feature table plus an additional
# column 'num_photos'.
#
dat_query_features <- get_bia_photos(bounds, BIA_PATH = "data/BIA_SQL2008.gdb", dest_dir = "conjola")

```

