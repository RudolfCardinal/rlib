# mapfunc.R

# =============================================================================
# Description
# =============================================================================
# Provides basic functions to work with shape files, including UK Office for
# National Statistics (ONS) Lower Level Super Output Area (LSOA) shapefiles,
# and the ONS Postcode Database.
#
# 2026-05-26:
# - Improved to use the "sf" package properly. Much faster.
# - Mapping by LSOA.
# - Postcode definitions
#   (https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom):
#       Outward code	    Inward code
#       Area	District	Sector	Unit
#       CV	    34	        6	    EY
# - Note that an LSOA may span multiple postcode districts.
# - District is a useful level.


# =============================================================================
# Packages
# =============================================================================

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

# Directories:
TMP_DIR <- "/tmp"

# Cambridgeshire data:
mapfunc$CAMBRIDGESHIRE_BOUNDARY <- sf::st_bbox(
    # Approximate! Specified in latitude/longitude.
    c(
        xmin = -0.65,  # West
        xmax = 0.65,  # East
        ymin = 51.9,  # South
        ymax = 52.95   # North
    ),
    crs = mapfunc$CRS_LAT_LON_WGS84
)
mapfunc$PLACES_CAMBRIDGESHIRE_ENVIRONS <- sf::st_as_sf(
    data.frame(read.table(textConnection("
        place               lat         long        cpft_catchment
        Bedford             52.1387    -0.4668      FALSE
        Cambridge           52.2053     0.1218      TRUE
        Chatteris           52.4560     0.0575      TRUE
        Doddington          52.4954     0.0578      TRUE
        \"Downham Market\"  52.6009     0.3758      FALSE
        Ely                 52.3995     0.2624      TRUE
        Haverhill           52.0809     0.4445      FALSE
        Huntingdon          52.3315    -0.1826      TRUE
        \"King's Lynn\"     52.7500     0.3833      FALSE
        Letchworth          51.9791    -0.2266      FALSE
        March               52.5517     0.0886      TRUE
        Newmarket           52.2504     0.3979      FALSE
        Peterborough        52.5695    -0.2405      TRUE
        Royston             52.0471    -0.0202      TRUE
        \"Saffron Walden\"  52.0234     0.2423      FALSE
        \"St Ives\"         52.3346    -0.0763      TRUE
        \"St Neots\"        52.2279    -0.2614       TRUE
        Wisbech             52.6644     0.1619      TRUE
    "), header = TRUE)),  # West negative, East positive
    coords = c("long", "lat"),  # x, y
    crs = mapfunc$CRS_LAT_LON_WGS84
)
mapfunc$CPFT_CORE_POSTCODES <- c(
    # Unverified - from a CPFT R&D figure in ~2024.
    paste0("CB", c(1:11, 21:25)),  # Cambridge area
    paste0("PE", c(1:9, 12:16, 19, 26:29)),  # Peterborough area
    paste0("SG", c(7:9, 19))  # Stevenage area
)
mapfunc$CAMBRIDGESHIRE_PETERBOROUGH_CCG_CODES <- c("E38000026", "E38000260")
# Various historical versions of NHS "chunks":
# - LHA, local health authority.
# - SHA, strategic health authority.
# - PCT, primary care trust.
# - CCG, clinical commissioning group.
# - ICB, integrated care board.
# - STP, sustainability and transformation partnership.
# https://findthatpostcode.uk/areatypes/ccg.html
# grep Camb ccg.csv
#   E38000026,NHS Cambridgeshire and Peterborough CCG
#   E38000260,NHS Cambridgeshire and Peterborough ICB - 06H
# From inspection of a known postcode in the main ONSPD file, the CCG appears
# in "sicbl24cd", only. From the ONS Postcode Directory (February 2026) User
# Guide (ONSPD_Feb_2026_User_Guide.zip,
# https://geoportal.statistics.gov.uk/datasets/481c020d6e824b628b18359d733e8cd5/about),
# seems to confirm that SICBL24CD is correct, including section 36, "Sub ICB
# Locations (LOC) - formerly Clinical Commissioning Groups (CCG)".

# Fictional data:
mapfunc$TEST_LSOA_1 <- paste0(
    "E0",
    c(1013788:1013797, 1033451:1033454)
)
mapfunc$HEATMAP_LSOA_TESTDATA_1 <- data.frame(
    lsoa = mapfunc$TEST_LSOA_1,
    y = 1:length(mapfunc$TEST_LSOA_1)
)
mapfunc$HEATMAP_POSTCODEDISTRICT_TESTDATA_1 <- data.frame(
    pcd_district = paste0("CB", c(1:5, 21:23)),
    quantity = 1:8
)
mapfunc$HEATMAP_POSTCODEDISTRICT_TESTDATA_2 <- data.frame(
    pcd_district = c("PE19", "CB1", "SG8"),
    quantity = 1:3
)
mapfunc$HEATMAP_POSTCODEDISTRICT_TESTDATA_3 <- data.frame(
    pcd_district = mapfunc$CPFT_CORE_POSTCODES,
    quantity = 1:length(mapfunc$CPFT_CORE_POSTCODES)
)


# =============================================================================
# Generic shapefile handling
# =============================================================================

mapfunc$read_map_shapes <- function(
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
    #       sf::st_bbox object, to which to clip the data (or NULL).
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
        cat("read_map_shapes()\n")
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

    if (is.null(boundaries)) {
        # No cropping required
        cropped_shp <- lsoa_shp_target
    } else {
        # Crop
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
    }
    # plot(cropped_shp, axes = TRUE)  # quick plot

    if (!is.null(cache_filename)) {
        miscfile$write_rds(cropped_shp, cache_filename)
    }
    return(cropped_shp)  # This is an sf::sf object.

    # Columns in the sf object tibble for LSOA data:
    #   objectid
    #   lsoa11cd
    #       LSOA code, e.g. E01013787; ~1300 in our example clipping rectangle
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
    return(mapfunc$read_map_shapes(
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
    # Data:
    data,
    # Dependent variable:
    depvar = "y",
    depvar_label = "y variable",
    fill_low = "blue",
    fill_high = "red",
    fill_missing = "white",  # NA shows the grid lines; white shows the sea
    # Shapes:
    map_shapes = mapfunc$get_cambs_lsoa_map_shapes(),
    shape_colname_in_data = "lsoa",
    shape_colname_in_map_shapes = "lsoa11cd",
    pen_colour = "grey",
    pen_size = 0.1,
    shape_labels = FALSE,
    shape_label_nudge_x = 0,
    shape_label_nudge_y = 0,
    shape_label_size = 3,
    shape_label_colour = "black",
    shape_label_fill = "white",
    shape_label_alpha = 0.5,
    # Boundary shape:
    boundary_shape = mapfunc$read_ons_subicb_shapefile(),  # or NULL
    boundary_above_shapes = TRUE,  # FALSE only works if shapes are transparent
    boundary_colour = "darkslategrey",
    boundary_linewidth = 1,
    boundary_fill = NA,
    boundary_alpha = 0.1,
    # Points of interest:
    points_of_interest = mapfunc$PLACES_CAMBRIDGESHIRE_ENVIRONS,
    place_points = TRUE,
    place_point_colour = "black",
    place_point_size = 2,
    place_labels = TRUE,
    place_label_nudge_x = 0,
    place_label_nudge_y = -0.015,
    place_label_size = 3,
    place_label_border_size = 0,
    place_label_colour = "black",
    place_label_fill = NA,
    place_label_alpha = 0.5,
    # Figure overall:
    x_label = "Longitude (°)",
    y_label = "Latitude (°)"
) {
    # Check and fix up "data".
    stopifnot(depvar %in% colnames(data))
    stopifnot(shape_colname_in_data %in% colnames(data))
    # Check "map_shapes".
    stopifnot(shape_colname_in_map_shapes %in% colnames(map_shapes))
    stopifnot(!(depvar %in% colnames(map_shapes)))  # depvar shouldn't be there

    data_with_geography <- merge(  # uses sf::merge.sf (not exported)
        x = map_shapes,
        y = data,
        by.x = shape_colname_in_map_shapes,
        by.y = shape_colname_in_data,
        all.x = TRUE
    )
    # ... all.x is TRUE: we show all of our map (even if parts have no data)
    # ... but not all.y: we don't show data that's off our map

    p <- ggplot(data_with_geography)
    if (!is.null(boundary_shape)) {
        boundary_geom <- geom_sf(
            data = boundary_shape,
            linewidth = boundary_linewidth,
            colour = boundary_colour,  # line colour
            fill = boundary_fill,
            alpha = boundary_alpha
        )
        if (!boundary_above_shapes) {
            p <- p + boundary_geom
        }
    }
    p <- (
        p +
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
    if (!is.null(boundary_shape) && boundary_above_shapes) {
        p <- p + boundary_geom
    }
    if (!is.null(points_of_interest)) {
        if (place_points) {
            p <- p + geom_sf(
                data = points_of_interest,
                colour = place_point_colour,
                size = place_point_size
            )
        }
        if (place_labels) {
            p <- p + geom_sf_label(
                data = points_of_interest,
                aes(label = place),
                fun.geometry = sf::st_centroid,
                nudge_x = place_label_nudge_x,
                nudge_y = place_label_nudge_y,
                size = place_label_size,
                label.size = place_label_border_size,
                colour = place_label_colour,  # text and border
                fill = place_label_fill,
                alpha = place_label_alpha
            )
        }
    }
    if (shape_labels) {
        p <- p + geom_sf_label(
            aes(label = .data[[shape_colname_in_map_shapes]]),
            fun.geometry = sf::st_centroid,
            nudge_x = shape_label_nudge_x,
            nudge_y = shape_label_nudge_y,
            size = shape_label_size,
            colour = shape_label_colour,  # text and border
            fill = shape_label_fill,
            alpha = shape_label_alpha
        )
    }
    return(p)
}


# =============================================================================
# Reading ONS postcode data
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
    # file. Adds some extra columns.
    #
    # Parameters:
    #
    #   onspd_csv_filename
    #       Filename of a CSV file containing the ONSPD data.
    #   cache_filename
    #   wipe_cache
    #       Usual cache control.
    #   verbose
    #       Be verbose?
    #
    # Returns:
    #   The ONSPD table, as a data.table(), with additional fields:
    #       pcd_outward
    #           Outward postcode (postcode district).

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

    # Read the data:
    onspd <- data.table::fread(onspd_csv_filename)

    # Add fields:
    onspd[, pcd_outward := sub(" .*", "", pcds)]

    if (!is.null(cache_filename)) {
        miscfile$write_rds(onspd, cache_filename)
    }
    return(onspd)
}


# Don't attempt to map LSOAs to top-level postcodes (postcode districts).
# Some LSOAs fall into multiple top-level postcodes; for example,
# E01017943 maps to 10 top-level postcodes. That is equally true in the main
# ONSPD and in
# https://geoportal.statistics.gov.uk/datasets/f9c8996d451f44b79ab97ddd369ad5db/about.
#
# HOWEVER, note in passing that you can merge shapes in an "sf" object thus:
#     # https://www.jla-data.net/eng/merging-geometry-of-sf-objects-in-r/
#     # Quite magical.
#     outwardpcd_shapes <- (
#         lsoa_labelled_pcd %>% 
#         group_by(pcd_outward) %>% 
#         summarise()
#     )
#     # Note: some resolve to POLYGON objects; some remain as MULTIPOLYGON.
#     # For example, CB1 is MULTIPOLYGON.


mapfunc$read_ons_postcode_district_shapefile <- function(
    filename = file.path(
        "/data",  # e.g. home directory mounted via Docker
        "dev",
        "onspd",
        "shapes",
        "DS_10283_2597",
        "GB_Postcodes",
        "PostalDistrict.shp"
    ),
    boundaries = mapfunc$CAMBRIDGESHIRE_BOUNDARY,
    cache_filename = file.path(
        TMP_DIR,
        "cambridgeshire_postcode_district_shapes.rds"
    ),
    wipe_cache = FALSE,
    verbose = TRUE
) {
    # Download from:
    # - https://datashare.ed.ac.uk/handle/10283/2597
    
    return(mapfunc$read_map_shapes(
        geography_shape_file = filename,
        boundaries = boundaries,
        cache_filename = cache_filename,
        wipe_cache = wipe_cache,
        verbose = verbose
    ))
}


mapfunc$read_ons_subicb_shapefile <- function(
    filename = file.path(
        "/data",  # e.g. home directory mounted via Docker
        "dev",
        "onspd",
        "shapes",
        "Sub_Integrated_Care_Board_Locations_April_2026_Boundaries_EN_BFC_-5211212599618763317",
        "SICBL_APR_2026_EN_BFC.shp"
    ),
    cache_filename = file.path(
        TMP_DIR,
        "cambridgeshire_subicb_shapes.rds"
    ),
    boundaries = mapfunc$CAMBRIDGESHIRE_BOUNDARY,
    ccg_codes = mapfunc$CAMBRIDGESHIRE_PETERBOROUGH_CCG_CODES,
    simplify = TRUE,
    dTolerance = 250,  # in metres
    wipe_cache = FALSE,
    verbose = TRUE
) {
    # Download from: https://geoportal.statistics.gov.uk/
    #
    # The Cambridgeshire/Peterborough CCG has two intruding lines: the River
    # Nene (e.g. Peterborough -> Wisbech), and the New Bedford River/Old
    # Bedford River (to Downham Market). To trim that off, we simplify.
    # The best value of dTolerance is about 250, established empirically:
    #   ggplot(st_simplify(cchshapes, dTolerance = 250)) + geom_sf()
    # From ?st_simplify, for latitude/longitude coordinates, the units of
    # dTolerance are metres.

    shapes <- mapfunc$read_map_shapes(
        geography_shape_file = filename,
        boundaries = boundaries,
        cache_filename = cache_filename,
        wipe_cache = wipe_cache,
        verbose = verbose
    )
    if (simplify) {
        shapes <- sf::st_simplify(shapes, dTolerance = dTolerance)
    }
    if (is.null(ccg_codes)) {
        return(shapes)
    }
    return(shapes[shapes$SICBL26CD %in% ccg_codes, ])
}


# =============================================================================
# Processing ONS postcode data
# =============================================================================

mapfunc$get_postcode_districts_for_ccg <- function(
    onspd = mapfunc$read_ons_postcode_database(),
    ccg_codes = mapfunc$CAMBRIDGESHIRE_PETERBOROUGH_CCG_CODES
) {
    postcodes <- onspd[sicbl24cd %in% ccg_codes]
    return(sort(unique(postcodes$pcd_outward)))
}


# There is little point in reading individual postcodes by CCG, because the
# next stage for mapping requires postcode-level shape files and that is a
# commercial product: e.g. ONS "Code-Point with Polygons".
#
# How about the intermediate step: postcode sectors? Can't find those.
#
# Then CCGs themselves, or ICBs: they do exist.
# Sub-ICB may be helpful: https://geoportal.statistics.gov.uk/
# See shapefile above.


# =============================================================================
# Tests
# =============================================================================

mapfunc$test_geography_heatmap_1 <- function() {
    return(mapfunc$geography_heatmap(
        data = mapfunc$HEATMAP_LSOA_TESTDATA_1
    ))
}

mapfunc$test_geography_heatmap_2 <- function() {
    return(mapfunc$geography_heatmap(
        data = mapfunc$HEATMAP_POSTCODEDISTRICT_TESTDATA_3,
        depvar = "quantity",
        shape_colname_in_data = "pcd_district",
        map_shapes = mapfunc$read_ons_postcode_district_shapefile(),
        shape_colname_in_map_shapes = "PostDist",
        shape_labels = TRUE
    ))
}

mapfunc$run_tests <- function(
    output_dir = NULL,  # defaults to current working directory
    width_mm = 300,
    height_mm = 300
) {
    savefig <- function(fname, fig) {
        if (!is.null(output_dir)) {
            fname <- file.path(output_dir, fname)
        }
        cat(sprintf("Saving to: %s\n", fname))
        ggsave(
            fname,
            fig,
            width = width_mm,
            height = height_mm,
            units = "mm",
            title = fname
        )
    }
    p1 <- mapfunc$test_geography_heatmap_1()
    savefig("mapfunc_heatmap_test_1.pdf", p1)
    p2 <- mapfunc$test_geography_heatmap_2()
    savefig("mapfunc_heatmap_test_2.pdf", p2)
}



#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("mapfunc" %in% search()) detach("mapfunc")
attach(mapfunc)  # subsequent additions not found, so attach at the end
