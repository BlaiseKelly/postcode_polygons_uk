library(sf)
library(mapview)
library(raster)
library(tmap)
library(dplyr)

# boundary polygons from https://osdatahub.os.uk/downloads/open/BoundaryLine (or use other shape file)
## import UK polygons
GB_counties <- st_read("dat/bdline_gpkg_gb/Data/bdline_gb.gpkg") |> 
  mutate(area = as.numeric(st_area(geometry))) |> 
  arrange(area)

#Download the centroids as a gpkg file from https://osdatahub.os.uk/downloads/open/CodePointOpen
GB_postcodes <- st_read("dat/codepo_gpkg_gb/Data/codepo_gb.gpkg")

# pick a county
cty <- GB_counties[1,]

# pick out the postcodes in the county
cty_pc <- GB_postcodes[cty,]

## make all points as one
d <- st_union(cty_pc)
## create voroni
v <- st_voronoi(d)

## create voroni polygons
p1 <- st_as_sf(st_intersection(st_cast(v), cty), crs = 27700)

## define the geometry as polygons
pc_vor <- st_cast(p1, "MULTIPOLYGON") |> 
  st_join(cty_pc)

# get basemap
bg <- basemaps::basemap_raster(cty, map_res = 0.99, map_service = "carto", map_type = "light")
# looks better if masked
bg_m <- mask(bg, st_transform(cty,3857))

# create tmap plot
tm2 <- tm_shape(bg_m)+
  tm_rgb()+
  tm_shape(cty) +
  tm_polygons(lwd = 3, fill_alpha = 0)+
  tm_shape(pc_vor)+
  tm_polygons(fill_alpha = 0)+
  tm_shape(cty_pc)+
  tm_dots(size = 0.1)+
  tm_layout(frame = FALSE)+
  tm_title(text = paste0(cty$Name, "\npostcode centroids\nand generated Voroni cells"),position = c(0.01,0.96),size = 1)

# save plot
tmap_save(tm2, "voroni.png")

# write them out
write_sf(pc_vor, "voronis.gpkg")