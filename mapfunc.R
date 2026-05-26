# mapfunc.R

local({
    # Packages:
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        broom,
        ggplot2,
        raster,  # geographic data analysis/modelling
            # Linux: use "sudo apt install libgdal-dev libgeos-dev" first
        sf,  # simple spatial features
        sp  # spatial package, for maps
    )
})
# Removed from CRAN 2023 (use sf or terra instead): maptools, rgdal, rgeos

library(data.table)
library(magrittr)  # for %>%

# Try this, for our local ones:
# requireNamespace("miscfile")  # nope, fails

# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

mapfunc <- new.env()


# =============================================================================
# Coordinate reference systems (CRSs)
# =============================================================================
#
# See:
# - http://gis.stackexchange.com/questions/140106/crs-not-embedded-by-maptools-in-r
# - http://spatialreference.org/ref/epsg/osgb-1936-british-national-grid/
# These are specified in "PROJ.4" string format; see
# - https://en.wikipedia.org/wiki/PROJ

# British National Grid (BNG):
mapfunc_EPSG_CODE_BNG_OSGB36 <- 27700  # https://epsg.io/27700
mapfunc$CRS_BNG_OSGB36 <- sf::st_crs(mapfunc_EPSG_CODE_BNG_OSGB36)

# Latitude/longitude (WGS84):
mapfunc$EPSG_CODE_WGS84 <- 4326  # https://epsg.io/4326
mapfunc$CRS_LAT_LON_WGS84 <- sf::st_crs(mapfunc$EPSG_CODE_WGS84)


# =============================================================================
# Other constants
# =============================================================================

TMP_DIR <- "/tmp"

mapfunc$CAMBRIDGESHIRE_BOUNDARY <- sf::st_bbox(
    # Approximate! Specified in BNG coordinates.
    c(
        xmin = 485000,  # West
        xmax = 585000,  # East
        ymin = 220000,  # South
        ymax = 340000   # North
    ),
    crs = mapfunc$CRS_BNG_OSGB36
)
mapfunc$CAMBRIDGESHIRE_CITIES <- sf::st_as_sf(
    data.frame(read.table(textConnection("
        place           lat         long
        Cambridge       52.2053     0.1218
        Peterborough    52.5695     -0.2405
        Ely             52.3995     0.2624
        Huntingdon      52.3315     -0.1826
    "), header = TRUE)),
    coords = c("long", "lat"),  # x, y
    crs = mapfunc$CRS_LAT_LON_WGS84
)
mapfunc$HEATMAP_LSOA_TESTDATA <- data.frame(
    # This is fictional.
    lsoa = c(
        "E01013787", "E01013788", "E01013789", "E01013790", "E01013791",
        # Leave a gap, and then:
        "E01013797", "E01013798", "E01013799", "E01013800", "E01013801"
    ),
    y = 1:10
)
mapfunc$HEATMAP_OUTWARDPCD_TESTDATA <- data.frame(
    # This is fictional.
    pcd_outward = c(
        "CB1", "CB2", "CB3", "CB4", "CB5",
        # Leave a gap, and then:
        "CB21", "CB22", "CB23"
    ),
    y = 1:8
)


# =============================================================================
# Generic shapefile handling
# =============================================================================

mapfunc$get_lsoa_map_shapes <- function(
    geography_shape_file,
    boundaries,
    cache_filename,
    target_crs = mapfunc$CRS_LAT_LON_WGS84,
    wipe_cache = FALSE,
    verbose = TRUE
) {
    # Loads a map shape file, e.g. UK Office for National Statistics (ONS)
    # boundary data.
    #
    # Parameters:
    #
    #   geography_shape_file
    #       Filename of the shapefile to be read.
    #   boundaries
    #       sf::st_bbox object, to which to clip the data.
    #   cache_filename
    #       Name of file to use as a cache.
    #   target_crs
    #       CRS object: the coordinate reference system in which to return
    #       results.
    #   wipe_cache
    #       Overwrite any previous cache.
    #   verbose
    #       Be verbose.
    #
    # Returns:
    #
    #   An sf::sf shape object.

    if (verbose) {
        cat("get_lsoa_map_shapes()\n")
    }
    if (!is.null(cache_filename)
            && file.exists(cache_filename)
            && !wipe_cache) {
        if (verbose) {
            cat(sprintf("Loading cached data from %s\n", cache_filename))
        }
        return(miscfile$read_rds(cache_filename))
    }

    # Read in shape (.shp) file.
    if (verbose) {
        cat(sprintf("Reading shape file: %s ...\n", geography_shape_file))
    }
    lsoa_shp_original <- sf::read_sf(
        geography_shape_file,
        stringsAsFactors = FALSE
    )
    # Note: passing the "crs" parameter here is saying "this is the CRS of the
    # source data", not "please convert to the CRS I have specified". Now:
    #       class(lsoa_shp_bng)  # sf, tbl_df, tbl, data.frame
    # One view of it is as a tibble: objectid, lsoa11cd, ..., geometry
    # (where "geometry" is a column of type <MULTIPOLYGON [°]>). See "?sf".
    # The Coordinate Reference System (CRS) is extracted via:
    #       sf::st_crs(lsoa_shp_bng)
    # You can transform CRS explicitly with:
    lsoa_shp_target <- sf::st_transform(lsoa_shp_original, crs = target_crs)

    # Crop:
    if (sf::st_crs(boundaries) != sf::st_crs(target_crs)) {
        # Note: the comparison does require st_crs() on BOTH sides.
        # And then the next bit requires bbox -> sf -> bbox:
        boundaries <- (
            boundaries
            %>% sf::st_as_sfc()
            %>% sf::st_transform(crs = target_crs)
            %>% sf::st_bbox()
        )
    }
    # To get around potential "Loop 0 is not valid" error, use sf_use_s2 to
    # disable spherical geometry:
    prev_s2 <- sf::sf_use_s2(FALSE)
    cropped_shp <- sf::st_crop(lsoa_shp_target, boundaries)
    sf::sf_use_s2(prev_s2)
    # plot(cropped_shp, axes = TRUE)  # quick plot

    if (!is.null(cache_filename)) {
        miscfile$write_rds(cropped_shp, cache_filename)
    }
    return(cropped_shp)  # This is an sf::sf object.

    # Columns in the sf object tibble for LSOA data:
    #   objectid
    #   lsoa11cd
    #       LSOA code, e.g. E01013787; 1792 in our example clipping rectangle
    #       (cf. 32,482 LSOAs in England)
    #   lsoa11nm
    #   lsoa11nmw
    #       LSOA name
    #   st_areasha
    #   st_lengths
    #   geometry
}


# =============================================================================
# UK / Cambridgeshire
# =============================================================================

mapfunc$get_cambs_lsoa_map_shapes <- function(
    geography_shape_file = file.path(
        "/data",  # e.g. home directory mounted via Docker
        "dev",
        "onspd",
        "shapes",
        "Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales",
        "Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp"
    ),
    # Other version:
    # "Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales",
    cache_filename = file.path(
        TMP_DIR,
        "cambridgeshire_geography_lsoa.rds"
    )
) {
    # The user should prefetch the shape data from
    # - https://data.gov.uk/dataset/lower_layer_super_output_area_lsoa_boundaries
    #   -> 2011 Full_Extent
    #   -> Download SHP
    # Alternatives:
    # - https://data.cambridgeshireinsight.org.uk/dataset/output-areas/resource/3bc4faef-38c7-417d-88a9-f302ad845ebe
    return(mapfunc$get_lsoa_map_shapes(
        cache_filename = cache_filename,
        geography_shape_file = geography_shape_file,
        boundaries = mapfunc$CAMBRIDGESHIRE_BOUNDARY
    ))
}


mapfunc$test_plot_cambridgeshire_map <- function()
{
    cambs_shp_df <- mapfunc$get_cambs_lsoa_map_shapes()
    test_locations_of_interest <- sf::st_as_sf(
        data.frame(
            long = c(-0.5, 0, 0.5),  # x
            lat = c(52.6, 52.4, 52.2)  # y
        ),
        coords = c("long", "lat"),  # x, y
        crs = mapfunc$CRS_LAT_LON_WGS84
    )
    testplot <- (
        ggplot(cambs_shp_df) +
        geom_sf() +
        geom_sf(
            data = test_locations_of_interest,
            colour = 'red',
            size = 10
        )
    )
    return(testplot)
}


mapfunc$geography_heatmap <- function(
    data,
    depvar = "y",
    depvar_label = "y variable",
    shape_colname_in_data = "lsoa",
    map_shapes = mapfunc$get_cambs_lsoa_map_shapes(),
    shape_colname_in_map_shapes = "lsoa11cd",
    points_of_interest = mapfunc$CAMBRIDGESHIRE_CITIES,
    pen_colour = "grey",
    pen_size = 0.1,
    x_label = "Longitude (°)",
    y_label = "Latitude (°)",
    fill_low = "blue",
    fill_high = "red",
    fill_missing = NA,
    place_colour = "darkgreen",
    place_size = 2,
    shape_labels = FALSE
) {
    # Check and fix up "data".
    stopifnot(depvar %in% colnames(data))
    stopifnot(shape_colname_in_data %in% colnames(data))
    # Check "map_shapes".
    stopifnot(shape_colname_in_map_shapes %in% colnames(map_shapes))

    data_with_geography <- merge(  # uses sf::merge.sf (not exported)
        x = map_shapes,
        y = data,
        by.x = shape_colname_in_map_shapes,
        by.y = shape_colname_in_data,
        all.x = TRUE
    )
    # ... all.x is TRUE: we show all of our map (even if parts have no data)
    # ... but not all.y: we don't show data that's off our map

    p <- (
        ggplot(data_with_geography) +
        geom_sf(
            # aes_string(fill = depvar),  # deprecated syntax
            aes(fill = .data[[depvar]]),  # replacement syntax
            colour = pen_colour,
            size = pen_size
        ) +
        scale_fill_gradient(
            low = fill_low,
            high = fill_high,
            na.value = fill_missing
        ) +
        labs(fill = depvar_label) +
        xlab(x_label) +
        ylab(y_label)
    )
    if (shape_labels) {
        p <- p + geom_sf_label(
            aes(label = .data[[shape_colname_in_map_shapes]])
        )
    }
    if (!is.null(points_of_interest)) {
        p <- p + geom_sf(
            data = points_of_interest,
            colour = place_colour,
            size = place_size,
        )
    }
    return(p)
}


# =============================================================================
# Postcode-type mapping
# =============================================================================

mapfunc$read_ons_postcode_database <- function(
    onspd_csv_filename = file.path(
        "/data",  # e.g. home directory mounted via Docker
        "dev",
        "onspd",
        "ONSPD_AUG_2025",
        "Data",
        "ONSPD_AUG_2025_UK.csv"
    ),
    cache_filename = file.path(TMP_DIR, "onspd.rds"),
    wipe_cache = FALSE,
    verbose = TRUE
) {
    # Reads the main Office for National Statistics Postcode Database (ONSPD)
    # file.
    #
    # Parameters:
    #
    #   onspd_csv_filename
    #       Filename of a CSV file containing the ONSPD data.
    # 
    if (!is.null(cache_filename)
            && file.exists(cache_filename)
            && !wipe_cache) {
        if (verbose) {
            cat(sprintf("Loading cached data from %s\n", cache_filename))
        }
        return(miscfile$read_rds(cache_filename))
    }
    if (verbose) {
        cat(sprintf("Reading full ONSPD from %s\n", onspd_csv_filename))
    }
    onspd <- data.table::fread(onspd_csv_filename)
    if (!is.null(cache_filename)) {
        miscfile$write_rds(onspd, cache_filename)
    }
    return(onspd)
}


mapfunc$get_lsoa_outward_postcode_map <- function(
    onspd = mapfunc$read_ons_postcode_database(),
    lsoa_colname = "lsoa11cd",
    cache_filename = file.path(TMP_DIR, "onspd_lsoa_to_outwardpcd.rds"),
    wipe_cache = FALSE,
    verbose = TRUE
) {
    # Makes a conversion table between LSOA and "outward" (top-level) postcodes
    # (the first half of a postcode).
    if (!is.null(cache_filename)
            && file.exists(cache_filename)
            && !wipe_cache) {
        if (verbose) {
            cat(sprintf("Loading cached data from %s\n", cache_filename))
        }
        return(miscfile$read_rds(cache_filename))
    }

    onspd[, pcd_outward := sub(" .*", "", pcds)]
    columns_to_select <- c(lsoa_colname, "pcd_outward")
    m <- unique(onspd[, ..columns_to_select])

    if (!is.null(cache_filename)) {
        miscfile$write_rds(m, cache_filename)
    }
    return(m)
}


mapfunc$convert_lsoa_shapes_to_toplevel_postcode_shapes <- function(
    lsoa_shapes = mapfunc$get_cambs_lsoa_map_shapes(),
    lsoa_outwardpcd_map = mapfunc$get_lsoa_outward_postcode_map(),
    lsoa_colname = "lsoa11cd",
    cache_filename = file.path(TMP_DIR, "outwardpcd_shapes.rds"),
    wipe_cache = TRUE,
    verbose = TRUE
) {
    # Makes shapes by merging LSOAs into outward postcode shapes.
    if (!is.null(cache_filename)
            && file.exists(cache_filename)
            && !wipe_cache) {
        if (verbose) {
            cat(sprintf("Loading cached data from %s\n", cache_filename))
        }
        return(miscfile$read_rds(cache_filename))
    }

    stopifnot(lsoa_colname %in% colnames(lsoa_shapes))
    stopifnot(lsoa_colname %in% colnames(lsoa_outwardpcd_map))

    lsoa_labelled_pcd <- merge(
        x = lsoa_shapes,
        y = lsoa_outwardpcd_map,
        by.x = lsoa_colname,
        by.y = lsoa_colname,
        all.x = TRUE  # all the shapes (but not all the postcodes)
    )
    # https://www.jla-data.net/eng/merging-geometry-of-sf-objects-in-r/
    # Quite magical.
    outwardpcd_shapes <- (
        lsoa_labelled_pcd %>% 
        group_by(pcd_outward) %>% 
        summarise()
    )

    if (!is.null(cache_filename)) {
        miscfile$write_rds(outwardpcd_shapes, cache_filename)
    }
    return(outwardpcd_shapes)
}


# =============================================================================
# Tests
# =============================================================================

mapfunc$test_geography_heatmap_1 <- function() {
    return(mapfunc$geography_heatmap(
        data = mapfunc$HEATMAP_LSOA_TESTDATA
    ))
}

mapfunc$test_geography_heatmap_2 <- function() {
    return(mapfunc$geography_heatmap(
        data = mapfunc$HEATMAP_OUTWARDPCD_TESTDATA,
        shape_colname_in_data = "pcd_outward",
        map_shapes = mapfunc$convert_lsoa_shapes_to_toplevel_postcode_shapes(),
        shape_colname_in_map_shapes = "pcd_outward",
        shape_labels = TRUE
    ))
}



#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("mapfunc" %in% search()) detach("mapfunc")
attach(mapfunc)  # subsequent additions not found, so attach at the end
