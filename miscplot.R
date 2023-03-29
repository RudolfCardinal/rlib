# miscplot.R

tmp_require_package_namespace <- function(...) {
    packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    for (p in packages) if (!requireNamespace(p)) install.packages(p)
}
tmp_require_package_namespace(
    grid,  # for gpar, etc.
    ggplot2,
    gridExtra,
    # extrafont,  # install with sudo. Then (with sudo R) run font_import() then loadfonts(). Then view with fonts() or fonttable(). See https://github.com/wch/extrafont
    Cairo,
    ggplot2
)
rm(tmp_require_package_namespace)


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscplot <- new.env()


#==============================================================================
# ggplot extensions
#==============================================================================

# Rudolf Cardinal, March 2011
# Simple extensions to ggplot2 (v0.8.7); see http://pobox.com/~rudolf/statistics/R
# Modified 5 Jan 2013 for ggplot2 0.9.3 (NB: use sessionInfo() to find current package versions)
# - fetch ggplot2 source with: git clone https://github.com/hadley/ggplot2.git
# Changes, because ggplot2 has changed its internal calling mechanisms:
# - opts() deprecated in favour of theme()
# - "Element panel.border must be an element_rect object" (error from validate_element() in theme-elements.r)
#   ... so change all class = "theme" to class = c("element_rect", "element")
# - "cannot coerce type 'closure' to vector of type 'list'"
#   ... a closure is a function (see ?typeof)
#   ... change class to be of class c("MYCLASS", "element_rect", "element")
# - then element_grob.MYCLASS not called by element_render()/element_grob()/UseMethod()... environment/namespace problem
#   tried setMethod("element_grob", "theme_border", function(STUFF) { STUFF} , where = as.environment("package:ggplot2")
#   but the environment is locked
#   ggplot2's theme-elements.r defines e.g. element_rect (exported) and element_grob.element_rect (not exported, does the work)
#   However, we can't override an internal function:
#       ... e.g. rewrite "validate_element" to crash
#           set environment(validate_element) <- as.environment("package:ggplot2") -- doesn't break the plotting.
# - Upshot: now impossible to hack through this way (locked environment).
# - http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
# - http://stackoverflow.com/questions/8204008/redirect-intercept-function-calls-within-a-package-function
# - These don't fix it:
#   library(proto)
#   theme <- with(proto(environment(ggplot2::theme), theme = ggplot2::theme, element_grob.theme_border = my.element_grob.theme_border), theme) --- doesn't work
#   ggplot <- with(proto(environment(ggplot2::ggplot), ggplot = ggplot2::ggplot, element_grob.theme_border = my.element_grob.theme_border), ggplot) --- breaks!
# - Fix by Baptiste Auguie 8/1/2013: inherit from element_blank instead; then it works fine.

#------------------------------------------------------------------------------
# Code duplicated from ggplot2 source (not exposed to wider namespace) for convenience
#------------------------------------------------------------------------------

.pt <- 1 / 0.352777778


len0_null <- function(x) {
    if (length(x) == 0)  NULL
    else                 x
}


#------------------------------------------------------------------------------
# Generic panel border (can set any combination of left/right/top/bottom)
#------------------------------------------------------------------------------

theme_border <- function(
        type = c("left", "right", "bottom", "top", "none"),
        colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + opts( panel.border = theme_border(type = c("bottom","left")) ) + ...
    type <- match.arg(type, several.ok = TRUE)
    structure(
        list(type = type, colour = colour, size = size, linetype = linetype),
        class = c("theme_border", "element_blank", "element")
    )
}


element_grob.theme_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        type = NULL,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    if (is.null(type)) type <- element$type
    xlist <- c()
    ylist <- c()
    idlist <- c()
    if ("bottom" %in% type) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
    }
    if ("top" %in% type) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
    }
    if ("left" %in% type) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
    }
    if ("right" %in% type) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
    }
    if (length(type)==0 || "none" %in% type) { # blank; cannot pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
    }
    gp <- grid::gpar(lwd = len0_null(size * .pt),
                     col = colour,
                     lty = linetype)
    element_gp <- grid::gpar(lwd = len0_null(element$size * .pt),
                             col = element$colour,
                             lty = element$linetype)
    grid::polylineGrob(
        x = xlist, y = ylist, id = idlist, ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}


#------------------------------------------------------------------------------
# For convenience: "L" (left + bottom) border
#------------------------------------------------------------------------------

theme_L_border <- function(colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border = theme_L_border() ) + ...
    structure(
        list(colour = colour, size = size, linetype = linetype),
        class = c("theme_L_border", "element_blank", "element")
    )
}


element_grob.theme_L_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    gp <- grid::gpar(lwd = len0_null(size * .pt),
                     col = colour,
                     lty = linetype)
    element_gp <- grid::gpar(lwd = len0_null(element$size * .pt),
                             col = element$colour,
                             lty = element$linetype)
    grid::polylineGrob(
        x = c(x+width, x, x), y = c(y,y,y+height), ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}


#------------------------------------------------------------------------------
# For convenience: bottom border only
#------------------------------------------------------------------------------

theme_bottom_border <- function(colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border = theme_bottom_border() ) + ...
    structure(
        list(colour = colour, size = size, linetype = linetype),
        class = c("theme_bottom_border", "element_blank", "element")
    )
}


element_grob.theme_bottom_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    gp <- grid::gpar(lwd = len0_null(size * .pt),
                     col = colour,
                     lty = linetype)
    element_gp <- grid::gpar(lwd = len0_null(element$size * .pt),
                             col = element$colour,
                             lty = element$linetype)
    grid::polylineGrob(
        x = c(x, x + width), y = c(y, y), ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}


#------------------------------------------------------------------------------
# For convenience: left border only
#------------------------------------------------------------------------------

theme_left_border <- function(colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border = theme_left_border() ) + ...
    structure(
        list(colour = colour, size = size, linetype = linetype),
        class = c("theme_left_border", "element_blank", "element")
    )
}


element_grob.theme_left_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    gp <- grid::gpar(lwd = len0_null(size * .pt),
                     col = colour,
                     lty = linetype)
    element_gp <- grid::gpar(lwd = len0_null(element$size * .pt),
                             col = element$colour,
                             lty = element$linetype)
    grid::polylineGrob(
        x = c(x, x), y = c(y, y+height), ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}



#------------------------------------------------------------------------------
# Border selection by number
#------------------------------------------------------------------------------

theme_border_numerictype <- function(type, colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border = theme_border(type = 9) ) + ...
    structure(
        list(type = type, colour = colour, size = size, linetype = linetype),
        class = c("theme_border_numerictype", "element_blank", "element")
    )
}


element_grob.theme_border_numerictype <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        type = NULL,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    if (is.null(type)) type <- element$type
    # numerical types from: library(gridExtra); example(borderGrob)
    # 1 = none, 2 = bottom, 3 = right, 4 = top, 5 = left, 6 = B+R, 7 = T+R, 8 = T+L, 9 = B+L, 10 = T+B, 11 = L+R, 12 = T+B+R, 13 = T+L+R, 14 = T+B+L, 15 = B+L+R, 16 = T+B+L+R
    xlist <- c()
    ylist <- c()
    idlist <- c()
    if (type==2 || type==6 || type==9 || type==10 || type==12 || type==14 || type==15 || type==16) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
    }
    if (type==4 || type==7 || type==8 || type==10 || type==12 || type==13 || type==14 || type==16) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
    }
    if (type==5 || type==8 || type==9 || type==11 || type==13 || type==14 || type==15 || type==16) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
    }
    if (type==3 || type==6 || type==7 || type==11 || type==12 || type==13 || type==15 || type==16) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
    }
    if (type==1) { # blank; can't pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
    }
    gp <- grid::gpar(lwd = len0_null(size * .pt),
                     col = colour,
                     lty = linetype)
    element_gp <- grid::gpar(lwd = len0_null(element$size * .pt),
                             col = element$colour,
                             lty = element$linetype)
    grid::polylineGrob(
        x = xlist, y = ylist, id = idlist, ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}


#------------------------------------------------------------------------------
# Examples
#------------------------------------------------------------------------------

miscplot$rnc_ggplot2_border_themes_example_script <- '
    library(ggplot2)
    df = data.frame( x = c(1,2,3), y = c(4,5,6) )
    source("http://egret.psychol.cam.ac.uk/statistics/R/extensions/rnc_ggplot2_border_themes_2013_01.r")
    ggplot(data = df, aes(x = x, y = y)) + geom_point() + theme_bw() + theme( panel.border = theme_border( c("bottom","left") ) )
    ggplot(data = df, aes(x = x, y = y)) + geom_point() + theme_bw() + theme( panel.border = theme_left_border() )
    ggplot(data = df, aes(x = x, y = y)) + geom_point() + theme_bw() + theme( panel.border = theme_bottom_border() )
    ggplot(data = df, aes(x = x, y = y)) + geom_point() + theme_bw() + theme( panel.border = theme_L_border() )
    ggplot(data = df, aes(x = x, y = y)) + geom_point() + theme_bw() + theme( panel.border = theme_border_numerictype(12) ) # use 1:16 as possible values
'


#==============================================================================
# multiplot
#==============================================================================
# http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow = 2, byrow = TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
miscplot$multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots <- length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
    }
}


#==============================================================================
# Multi-way figures done properly with ggplot
#==============================================================================

# fontconfig (used by Cairo)
#   fc-list | sort
#   sudo fc-cache ...

# http://cairographics.org/manual/cairo-FreeType-Fonts.html

miscplot$which_cairo_fonts_available <- function() {
    CairoFontMatch(":", sort = TRUE)
}


miscplot$cairo_select_font_myriad <- function() {
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        windowsFonts(Myriad = windowsFont("Myriad"))
        par(family = "Myriad")
    } else {
        CairoFonts(
            regular = "Myriad Pro:style=Regular",
            bold = "Myriad Pro:style=Bold",
            italic = "Myriad Pro:style=Italic",
            bolditalic = "Myriad Pro:style=Bold Italic",
            # symbol = "Symbol:style=Regular"
            symbol = "Myriad Pro:style=Regular"
        )
    }
}


miscplot$cairo_select_font_arial <- function() {
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        windowsFonts(Arial = windowsFont("Arial"))
        par(family = "Arial")
    } else {
        CairoFonts(
            regular = "Arial:style=Regular",
            bold = "Arial:style=Bold",
            italic = "Arial:style=Italic",
            bolditalic = "Arial:style=Bold Italic",
            symbol = "Symbol:style=Regular"
        )
    }
}


miscplot$save_grob_to_pdf <- function(g, filename, width_mm, height_mm,
                             title = filename, use_ggsave = FALSE,
                             embed_fonts_from_ggsave = FALSE) {
    cat("Saving grob to PDF:", filename, "\n")
    if (use_ggsave) {
        # AVOID.
        ggsave(filename, g, width = width_mm, height = height_mm, units = "mm")
        if (embed_fonts_from_ggsave) {
            embed_fonts(filename)
        }
    } else {
        Cairo(filename, type = "pdf", width = width_mm, height = height_mm, units = "mm",
              title = title)
        # Cairo automatically embeds fonts:
        # http://cran.r-project.org/web/packages/Cairo/Cairo.pdf
        grid::grid.draw(g)  # not: print(g)
        dev.off()
    }
}


miscplot$INCHES_PER_MM <- 1/25.4
miscplot$A4_SMALL_MM <- 210
miscplot$A4_LARGE_MM <- 297
miscplot$POINTS_PER_INCH <- 72
miscplot$POINTS_PER_MM <- miscplot$POINTS_PER_INCH * miscplot$INCHES_PER_MM

miscplot$BLANK_GROB <- grid::rectGrob(gp = grid::gpar(fill = "white", alpha = 0))
# ... alpha = 0 makes it invisible
miscplot$NOLEGEND <- theme(legend.position = "none")
miscplot$MOVELEGEND_BOTTOMLEFT <- theme(legend.justification = c(0,0),
                               legend.position = c(0,0))
miscplot$MOVELEGEND_BOTTOMRIGHT <- theme(legend.justification = c(1,0),
                                legend.position = c(1,0))
miscplot$MOVELEGEND_TOPRIGHT <- theme(legend.justification = c(1,1),
                             legend.position = c(1,1))
miscplot$MOVELEGEND_TOPLEFT <- theme(legend.justification = c(0,1),
                            legend.position = c(0,1))
miscplot$LEFTTITLE <- theme(plot.title = element_text(hjust = 0, face = "bold"))

miscplot$BORDER_BOTTOM_AND_LEFT <- theme(panel.border = theme_border(c("left","bottom"))) # RNC hack, see above
# theme_blank() replaced by element_blank()
miscplot$NO_LEGEND_BOX <- theme(legend.key = element_blank())
miscplot$NO_GRID <- theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())


miscplot$ggplot_rnc_theme <- function(fontsize) {
    theme_bw() +
        theme(text = element_text(size = fontsize))
}


#==============================================================================
# Error bars
#==============================================================================

miscplot$free_floating_error_bar <- function(x, length, width = 0.2,
                                             y_centre = NULL, y_bottom = NULL,
                                             constraints = NULL,
                                             inherit.aes = FALSE) {
    if ((is.null(y_centre) && is.null(y_bottom))
            || (!is.null(y_centre) && !is.null(y_bottom))) {
        stop("Specify either y_centre or y_bottom")
    }
    if (is.null(y_centre)) {
        y_centre <- y_bottom + length/2
    }
    errbar_df <- data.frame(
        x = x,
        bottom = y_centre - length/2,
        top = y_centre + length/2
    )
    if (!is.null(constraints)) {
        errbar_df[, names(constraints)] <- constraints
    }
    ggplot2::geom_errorbar(
        data = errbar_df,
        aes(x = x, ymin = bottom, ymax = top),
        width = width,
        inherit.aes = inherit.aes
    )
}


miscplot$free_floating_label <- function(x, y, text, constraints = NULL,
                                         inherit.aes = FALSE, size = rel(2.5)) {
    label_df <- data.frame(
        x = x,
        y = y,
        text = text
    )
    if (!is.null(constraints)) {
        label_df[, names(constraints)] <- constraints
    }
    ggplot2::geom_text(
        data = label_df,
        aes(x = x, y = y, label = text),
        size = size,
        inherit.aes = inherit.aes
    )
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscplot" %in% search()) detach("miscplot")
attach(miscplot)  # subsequent additions not found, so attach at the end
