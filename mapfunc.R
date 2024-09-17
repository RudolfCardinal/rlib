# mapfunc.R

local({
    # Packages:
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        data.table,
        broom,
        ggplot2,
        maptools,
        raster,  # Linux: use "sudo apt install libgeos-dev" first
        rgdal,  # Linux: use "sudo apt install libgdal-dev libproj-dev" first
        rgeos,
        sp
    )
})

# Try this, for our local ones:
# requireNamespace("miscfile")  # nope, fails

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

mapfunc <- new.env()


#==============================================================================
# Coordinate systems
#==============================================================================

# Coordinate system isn't set by default
# - http://gis.stackexchange.com/questions/140106/crs-not-embedded-by-maptools-in-r
# - http://spatialreference.org/ref/epsg/osgb-1936-british-national-grid/
mapfunc$CRS_PROJ4STRING_BNG_OSGB36 <- sp::CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs ")
mapfunc$CRS_LAT_LON_WGS84 <- sp::CRS("+proj=longlat +datum=WGS84")


#==============================================================================
# UK / Cambridgeshire
#==============================================================================

mapfunc$get_lsoa_map_shapes <- function(
    cache_filename,
    geography_shape_file_dir,
    geography_shape_file_stem,
    edge_west_bng,  # British National Grid (BNG) coordinates
    edge_east_bng,
    edge_south_bng,
    edge_north_bng,
    wipe_cache = FALSE,
    verbose = TRUE
) {
    # - https://data.gov.uk/dataset/lower_layer_super_output_area_lsoa_boundaries
    #   -> 2011 Full_Extent
    #   -> Download SHP

    if (verbose) {
        cat("get_lsoa_map_shapes()\n")
    }
    if (!is.null(cache_filename) && file.exists(cache_filename) && !wipe_cache) {
        if (verbose) {
            cat(sprintf("Loading cached data from %s\n", cache_filename))
        }
        return(miscfile$read_rds(cache_filename))
    }

    filestem <- paste(geography_shape_file_dir, geography_shape_file_stem,
                      sep = .Platform$file.sep)
    if (verbose) {
        cat(sprintf("Using filestem %s [to which extensions are expected by maptools::readShapeSpatial, specifically .shp, .shx, and .dbf]...\n",
                    filestem))
    }

    # Read in shape files
    lsoa_shp_bng <- maptools::readShapeSpatial(
        filestem,
        IDvar = "lsoa11cd",
        proj4string = mapfunc$CRS_PROJ4STRING_BNG_OSGB36)

    # class(lsoa_shp_bng)  # should be SpatialPolygonsDataFrame
    # ... standard coordinate systm has x ~ 544000, y ~ 184000
    # ... which I think is British National Grid coordinates
    # ... yes:
    #     https://karlhennermann.wordpress.com/2015/02/16/how-to-make-lsoa-and-msoa-boundaries-from-uk-data-service-align-properly-in-arcgis/

    # Can plot directly:
    # plot(lsoa_shp_bng)  # SLOW

    # Crop:
    # - http://stackoverflow.com/questions/13982773/crop-for-spatialpolygonsdataframe
    # - https://gridreferencefinder.com/
    cambridgeshire_bng <- raster::extent(edge_west_bng, edge_east_bng,
                                         edge_south_bng, edge_north_bng)
    cambs_shp_bng <- raster::crop(lsoa_shp_bng, cambridgeshire_bng)
    # plot(cambs_shp_bng, axes = TRUE)

    # Convert to latitude/longitude:
    cambs_shp_latlon <- sp::spTransform(cambs_shp_bng, CRS_LAT_LON_WGS84)
    # plot(cambs_shp_latlon, axes = TRUE)

    # https://rpubs.com/ajlyons/rspatialdata

    cambs_df <- broom::tidy(cambs_shp_latlon)  # for polygon-based SHP files; VERY SLOW unless restricted
    # x.df <- as.data.frame(x.shp)  # for points-based SHP files

    # Convert to data table:
    setDT(cambs_df)

    if (!is.null(cache_filename)) {
        miscfile$write_rds(cambs_df, cache_filename)
    }
    return(cambs_df)

    # Columns:
    # lat
    # long
    # order: e.g. 1-1700 within one LSOA
    # hole
    # piece
    # group
    # id: LSOA code, e.g. E01013787; NOT UNIQUE; 1793 in our example clipping rectangle (cf. 32,482 LSOAs in England)
}


mapfunc$get_cambs_lsoa_map_shapes <- function(
    cache_filename = paste(path.expand("~"), "tmp",
                           "cambridgeshire_geography_lsoa.rds",
                           sep = .Platform$file.sep),
    # geography_shape_file_dir = "/srv/ons_postcode_database/shapes",
    geography_shape_file_dir = "/home/rudolf/dev/onspd",
    geography_shape_file_stem = "Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales",
    wipe_cache = FALSE,
    verbose = TRUE
) {
    return(mapfunc$get_lsoa_map_shapes(
        cache_filename = cache_filename,
        geography_shape_file_dir = geography_shape_file_dir,
        geography_shape_file_stem = geography_shape_file_stem,
        edge_west_bng = 485000,  # APPROX; in British National Grid (BNG) coordinates
        edge_east_bng = 585000,
        edge_south_bng = 220000,
        edge_north_bng = 340000,
        wipe_cache = wipe_cache,
        verbose = verbose
    ))
}


mapfunc$test_plot_cambridgeshire_map <- function()
{
    cambs_shp_df <- mapfunc$get_cambs_lsoa_map_shapes()
    test_locations_of_interest <- data.frame(
        long = c(-0.5, 0, 0.5),  # x
        lat = c(52.6, 52.4, 52.2)  # y
    )
    testplot <- (
        ggplot() +
        coord_fixed(ratio = 1) +
        geom_polygon(
            data = cambs_shp_df,
            aes(x = long, y = lat, group = group),
            colour = 'blue',
            fill = 'white'
        ) +
        geom_point(
            data = test_locations_of_interest,
            aes(x = long, y = lat),
            colour = 'red',
            size = 10,
        )
    )
    return(testplot)
}


mapfunc$HEATMAP_TESTDATA <- data.table(
    lsoa = c("E01013787", "E01013788", "E01013789", "E01013790", "E01013791",
             "E01013792", "E01013793", "E01013794", "E01013795", "E01013796"),
    y = 1:10
)


mapfunc$CAMBRIDGESHIRE_CITIES <- data.table(read.table(textConnection("
    place           lat         long
    Cambridge       52.2053     0.1218
    Peterborough    52.5695     -0.2405
    Ely             52.3995     0.2624
    Huntingdon      52.3315     -0.1826
"), header = TRUE))


mapfunc$geography_heatmap <- function(
    data,
    depvar = 'y',
    depvar_label = "y variable",
    shape_colname_in_data = 'lsoa',
    map_shapes = mapfunc$get_cambs_lsoa_map_shapes(),
    shape_colname_in_map_shapes = "id",
    points_of_interest = mapfunc$CAMBRIDGESHIRE_CITIES,
    pen_colour = 'black',
    pen_size = 0.1,
    x_label = "Longitude (°)",
    y_label = "Latitude (°)",
    fill_low = 'blue',
    fill_high = 'red',
    fill_missing = 'white',
    place_colour = 'yellow',
    place_size = 1
) {
    data_with_geography <- merge(x = data,
                                 y = map_shapes,
                                 by.x = shape_colname_in_data,
                                 by.y = shape_colname_in_map_shapes,
                                 all.y = TRUE)
    # ... all.y is TRUE: we show all of our map (even if parts have no data)
    # ... but not all.x: we don't show data that's off our map

    setDT(data_with_geography)
    setkey(data_with_geography, group, order)  # important for polygon drawing order
    p <- (
        ggplot()  +
        coord_fixed(ratio = 1) +
        geom_polygon(
            data = data_with_geography,
            aes_string(x = 'long', y = 'lat', group = 'group', fill = depvar),
            colour = pen_colour,
            size = pen_size
        ) +
        scale_fill_gradient(low = fill_low,
                            high = fill_high,
                            na.value = fill_missing) +
        labs(fill = depvar_label) +
        xlab(x_label) +
        ylab(y_label)
    )
    if (!is.null(points_of_interest)) {
        p <- p + geom_point(
            data = points_of_interest,
            aes(x = long, y = lat),
            colour = place_colour,
            size = place_size,
        )
    }
    return(p)
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("mapfunc" %in% search()) detach("mapfunc")
attach(mapfunc)  # subsequent additions not found, so attach at the end
