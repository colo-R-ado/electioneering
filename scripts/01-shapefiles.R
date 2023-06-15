###############################################################################
# 01-shapefiles.R                                                             #
# Colorado shapefile mapping for state counties, state zip codes, county      #
# voting districts, county tracts and county block groups                     #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr) # Data wrangling
library(leaflet) # Mapping
library(mapview) # Map exporting
library(purrr) # Data wrangling
library(sf) # Shapefiles
library(tigris) # Shapefiles

# Import data -----------------------------------------------------------------

"
Shapefile hierarchy:

Counties
 |-- Voting Districts
 |-- Tracts
      |-- Block Groups
Zips
"

statefp <- 08 # Colorado

cts <- counties(state = statefp)
vtd <- voting_districts(state = statefp)
trx <- tracts(state = statefp)
bgr <- block_groups(state = statefp)
zip <- zctas(state = statefp, year = 2010)

sfh <- lst(cts, vtd, trx, bgr, zip)
sfh <- map(sfh, ~ st_transform(.x, "+proj=longlat +datum=WGS84"))

# El Paso County ------------------------------------------

elp <- filter(sfh[["cts"]], NAME == "El Paso")

elp <- sfh %>%
  map(
    ~ st_intersection(.x, elp),
    .progress = TRUE
  ) %>%
  map(
    ~ st_make_valid(.x) # Fix shapefile oddities e.g. duplicate vertices
  ) %>%
  map(
    ~ mutate(.x, overlap = as.numeric(st_area(.x)))
  ) %>%
  map(
    ~ filter(.x, overlap > 0)
  ) %>%
  map(
    ~ mutate(.x, geometry = st_collection_extract(geometry))
  ) %>%
  map(
    ~ st_make_valid(.x)
  )

m01 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = elp[["zip"]],
    color = "grey",
    fillOpacity = .3,
    group = "Zips"
  ) %>%
  addPolygons(
    data = elp[["bgr"]],
    color = "#002868",
    weight = 1.5,
    fillOpacity = .3,
    group = "Block Groups"
  ) %>%
  addPolygons(
    data = elp[["trx"]],
    color = "#BF0A30",
    weight = 1.5,
    fillOpacity = .3,
    group = "Tracts"
  ) %>%
  addPolygons(
    data = elp[["vtd"]],
    color = "#FFD700",
    weight = 1.5,
    fillOpacity = .3,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = elp[["cts"]],
    color = "#007235",
    weight = 1.5,
    fillOpacity = .3,
    group = "Counties"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "Zips", 
      "Block Groups", 
      "Tracts", 
      "Voting Districts", 
      "Counties"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(
    c("Zips", "Block Groups", "Tracts", "Voting Districts")
  )

mapshot(
  m01, 
  file = "./output/m01-county.png", 
  remove_controls = c("zoomControl")
)

m02 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "Tracts"
  )
mapshot(
  m02, 
  file = "./output/m02-tracts.png", 
  remove_controls = c("zoomControl")
)

m03 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "Block Groups"
  )
mapshot(
  m03, 
  file = "./output/m03-block-groups.png", 
  remove_controls = c("zoomControl")
)

m04 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "Voting Districts"
  )
mapshot(
  m04, 
  file = "./output/m04-voting-districts.png", 
  remove_controls = c("zoomControl")
)

# Full State Overlaps -------------------------------------

b2v <- sfh[["bgr"]] %>%
  group_by(
    GEOID
  ) %>%
  group_split() %>%
  map(
    # Both block groups and voting districts are subsets within same county
    ~ lst(bgr = .x, vtd = filter(sfh[["vtd"]], COUNTYFP20 %in% .x$COUNTYFP))
  ) %>%
  map(
    # Retain only the voting district ID and geometry
    ~ lst(bgr = .x$bgr, vtd = select(.x$vtd, VTDST20, geometry))
  ) %>%
  map(
    # Retain overlap between block group and voting district geometries
    ~ tryCatch({
      st_intersection(.x$bgr, .x$vtd)
    },
      error = function(e) NA
    ),
    .progress = TRUE
  ) %>%
  map(
    # Clean up any degenerate edges
    ~ st_make_valid(.x)
  ) %>%
  map(
    ~ mutate(.x, area = as.numeric(st_area(.x)))
  ) %>%
  map(
    ~ filter(.x, area > 0)
  ) %>%
  compact() %>% # Remove null entries
  map(
    ~ st_collection_extract(.x) # Standardize mixed geometries
  )

zoom <- b2v[[97]]

leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = zoom,
    color = "red",
    group = "Overlap"
  ) %>%
  addPolygons(
    data = filter(sfh[["vtd"]], VTDST20 %in% zoom$VTDST20),
    color = "blue",
    popup = ~ VTDST20,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = filter(sfh[["bgr"]], GEOID %in% zoom$GEOID),
    color = "green",
    popup = ~ GEOID,
    group = "Block Groups"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "Overlap", 
      "Block Groups",
      "Voting Districts"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )
