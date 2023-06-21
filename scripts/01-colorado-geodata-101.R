###############################################################################
# 01-colorado-geodata-101.R                                                   #
# Colorado shapefile mapping for ZCTAs, state counties, county voting         #
# districts, county tracts and county block groups                            #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr) # Data wrangling
library(leaflet) # Mapping
library(mapview) # Map exporting
library(osfr) # Data files
library(purrr) # Data wrangling
library(sf) # Shapefiles
library(tidycensus) # Census data
library(tigris) # Shapefiles

# OSF personal access token: https://osf.io/m5qyk/
pat <- keyring::key_get("OSF")
osf_auth(token = pat)

# Import Colorado color palette
source("./scripts/colorado_cols.R")
col_pal <- as.list(colorado_cols())

# Import data -----------------------------------------------------------------

"
Shapefile hierarchy:

Counties
 |-- Voting Districts
 |-- Tracts
      |-- Block Groups
ZCTAs
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
    color = "grey50",
    weight = 1.5,
    fillOpacity = .3,
    group = "ZCTAs"
  ) %>%
  addPolygons(
    data = elp[["bgr"]],
    color = col_pal[["plate green"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Block Groups"
  ) %>%
  addPolygons(
    data = elp[["trx"]],
    color = col_pal[["flag red"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Tracts"
  ) %>%
  addPolygons(
    data = elp[["vtd"]],
    color = col_pal[["rockies purple"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = elp[["cts"]],
    color = col_pal[["nuggets yellow"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Counties"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "ZCTAs",
      "Block Groups", 
      "Tracts", 
      "Voting Districts", 
      "Counties"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(
    c("ZCTAs", "Block Groups", "Tracts", "Voting Districts")
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

m05 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "ZCTAs"
  )
mapshot(
  m05, 
  file = "./output/m05-zips.png", 
  remove_controls = c("zoomControl")
)

# Full Shape Overlaps -------------------------------------

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
  keep(
    # Strip out stragglers
    is.data.frame
  ) %>%
  compact() %>%
  map(
    # Standardize mixed geometries
    ~ st_collection_extract(.x)
  ) %>%
  map(
    # Clean up any degenerate edges
    ~ st_make_valid(.x)
  ) %>%
  map(
    ~ mutate(.x, area = as.numeric(st_area(.x)))
  ) %>%
  map(
    # Some overlap is only along boundary lines
    ~ filter(.x, area > 0)
  ) %>%
  map(
    ~ st_collection_extract(.x) 
  )

# Illustrative voting district
zoom <- b2v %>%
  bind_rows() %>%
  filter(
    VTDST20 == "041316"
  )

m06 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = filter(sfh[["vtd"]], VTDST20 %in% zoom$VTDST20),
    color = col_pal[["rockies purple"]],
    popup = ~ VTDST20,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = filter(sfh[["bgr"]], GEOID %in% zoom$GEOID),
    color = col_pal[["plate green"]],
    popup = ~ GEOID,
    group = "Block Groups"
  ) %>%
    addPolygons(
    data = zoom,
    color = col_pal[["broncos orange"]],
    group = "Overlap"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "Voting Districts",
      "Block Groups",
      "Overlap"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

m07 <- m06 %>% 
  hideGroup(
    c("Block Groups", "Overlap")
  )
mapshot(
  m07, 
  file = "./output/m07-example-voting-districts.png", 
  remove_controls = c("zoomControl")
)

m08 <- m06 %>% 
  hideGroup(
    c("Voting Districts", "Overlap")
  )
mapshot(
  m08, 
  file = "./output/m08-example-block-groups.png", 
  remove_controls = c("zoomControl")
)

m09 <- m06 %>% 
  hideGroup(
    c("Block Groups", "Voting Districts")
  )
mapshot(
  m09, 
  file = "./output/m09-example-overlap.png", 
  remove_controls = c("zoomControl")
)

# Income Data ---------------------------------------------

income <- get_acs(
  geography = "block group",
  variables = "B19301_001", # Per capita income prior 12m
  state = "CO",
  geometry = FALSE,
  year = 2020
)

# Map
elpincome <- sfh[["bgr"]] %>%
  filter(
    COUNTYFP %in% "041"
  ) %>%
  left_join(
    income,
    by = "GEOID"
  ) %>%
  mutate(
    median_income = median(estimate, na.rm = TRUE), # Impute median
    estimate = ifelse(is.na(estimate), median_income, estimate)
  )

pal_income <- colorNumeric("Greens", domain = elpincome$estimate)

m10 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = elpincome,
    color = ~ pal_income(estimate),
    weight = 1.5
  ) %>%
  addLegend(
    "bottomright",
    data = elpincome,
    pal = pal_income, 
    values = ~ estimate,
    title = "Per Capita Income",
    labFormat = labelFormat(prefix = "$"),
    opacity = 1
  )
mapshot(
  m10, 
  file = "./output/m10-el-paso-income.png"
)

# 2020 Election -------------------------------------------

edata <- osf_retrieve_node("m5qyk")
edata <- osf_ls_files(edata)

e20 <- edata[edata$name == "co_2020", ]
e20 <- osf_retrieve_file(e20$id)
e20 <- osf_download(e20, tempdir(), conflicts = "overwrite")
e20 <- read_sf(e20$local_path)
e20 <- st_zm(e20, drop = T, what = "ZM")
e20 <- st_transform(e20, "+proj=longlat +datum=WGS84")

e20 <- e20 %>%
  select(
    STATEFP:PRECINCT,
    contains("PRE")
  ) %>%
  mutate(
    total_votes = rowSums(pick(where(is.numeric)), na.rm = T),
    pct_gop = G20PRERTRU / total_votes
  ) %>%
  data.frame() %>%
  select(
    VTDST20 = VTDST, # For join
    pct_gop
  )

# Map
elpe20 <- sfh[["vtd"]] %>%
  filter(
    COUNTYFP20 %in% elpincome$COUNTYFP
  ) %>%
  left_join(
    e20,
    by = "VTDST20"
  )

pal_gop <- colorNumeric(c("blue", "white", "red"), domain = elpe20$pct_gop)

m11 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = elpe20,
    color = ~ pal_gop(pct_gop),
    weight = 1.5
  ) %>%
  addLegend(
    "bottomright",
    data = elpe20,
    pal = pal_gop, 
    values = ~ pct_gop,
    title = "% GOP President (2020)",
    labFormat = scales::percent,
    opacity = 1
  )

mapshot(
  m11, 
  file = "./output/m11-el-paso-e20.png"
)

# Income/Election Join ------------------------------------

# Highlight Front Range corridor
front_range <- c(
  "001", # Adams
  "005", # Arapahoe
  "014", # Broomfield
  "031", # Denver
  "013", # Boulder
  "035", # Douglas
  "041", # El Paso
  "059", # Jefferson
  "069", # Larimer
  "101", # Pueblo
  "123" # Weld
)

inc_e20 <- b2v %>%
  bind_rows() %>%
  filter(
    COUNTYFP %in% front_range
  ) %>%
  left_join(
    income,
    by = "GEOID"
  ) %>%
  arrange(
    VTDST20
  ) %>%
  mutate(
    pct_area = area / sum(area),
    wincome = round(weighted.mean(estimate, pct_area), 0),
    .by = VTDST20
  ) %>%
  mutate(
    wincome_pct = ecdf(wincome)(wincome),
    .by = COUNTYFP
  ) %>%
  left_join(
    e20,
    by = "VTDST20"
  ) %>%
  relocate(
    geometry,
    .after = last_col()
  )

# Illustrative table
inc_e20 %>%
  data.frame() %>%
  filter(
    VTDST20 %in% zoom$VTDST20
  ) %>%
  mutate(
    estimate = scales::dollar(estimate),
    pct_area = scales::percent(pct_area, accuracy = 1),
    wincome = scales::dollar(wincome),
    pct_gop = scales::percent(pct_gop, accuracy = 1)
  ) %>%
  select(
    STATEFP,
    COUNTYFP,
    VTDST20,
    GEOID,
    NAME,
    area,
    pct_area,
    estimate,
    wincome,
    pct_gop
  ) %>%
  write.csv(
    file = "./output/illustrative-vtd.csv",
    row.names = FALSE
  )

# Income v % GOP by County
ingopcty <- inc_e20 %>%
  data.frame() %>%
  select(
    COUNTYFP,
    VTDST20,
    wincome,
    wincome_pct,
    pct_gop
  ) %>%
  distinct() %>%
  inner_join(
    distinct(sfh[["cts"]], COUNTYFP, NAME),
    by = "COUNTYFP"
  )

p01 <- ggplot() +
  geom_point(
    data = ingopcty,
    aes(
      x = wincome,
      y = pct_gop
    ),
    size = 4
  ) +
  geom_smooth(
    data = ingopcty,
    aes(
      x = wincome,
      y = pct_gop
    ),
    size = 1.5,
    color = "green"
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(scale_cut = scales::cut_short_scale())
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  facet_wrap(
    ~ NAME,
    scales = "free_x"
  ) +
  labs(
    x = "Per Capita Income",
    y = "% GOP"
  ) +
  theme_minimal(
    base_size = 24
  )

ggsave(
  "./output/p01.png",
  width = 22,
  height = 18,
  dpi = 300
)

p02 <- ggplot() +
  geom_point(
    data = ingopcty,
    aes(
      x = wincome_pct,
      y = pct_gop
    ),
    size = 4
  ) +
  geom_smooth(
    data = ingopcty,
    aes(
      x = wincome_pct,
      y = pct_gop
    ),
    size = 1.5,
    color = col_pal[["broncos orange"]]
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  facet_wrap(
    ~ NAME,
    scales = "free_x"
  ) +
  labs(
    x = "Intra-County Per Capita Income Percentile",
    y = "% GOP"
  ) +
  theme_minimal(
    base_size = 24
  )

ggsave(
  "./output/p02.png",
  width = 22,
  height = 18,
  dpi = 300
)

# Push geofile to OSF ---------------------------------------------------------

saveRDS(b2v, file = paste0(tempdir(), "/b2v.rds"))

osf_upload(osf_retrieve_node("m5qyk"), paste0(tempdir(), "/b2v.rds"))