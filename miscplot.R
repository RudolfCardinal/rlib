# miscplot.R

source("http://egret.psychol.cam.ac.uk/statistics/R/extensions/rnc_ggplot2_border_themes_2013_01.r")

#===============================================================================
# multiplot
#===============================================================================
# http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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
                         ncol=cols, nrow=ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind=TRUE))
            print(plots[[i]], vp=viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
    }
}

#===============================================================================
# Multi-way figures done properly with ggplot
#===============================================================================

# fontconfig (used by Cairo)
#   fc-list | sort
#   sudo fc-cache ...

# http://cairographics.org/manual/cairo-FreeType-Fonts.html

library(gridExtra)
#library(extrafont) # install with sudo. Then (with sudo R) run font_import() then loadfonts(). Then view with fonts() or fonttable(). See https://github.com/wch/extrafont
library(Cairo)
library(ggplot2)

which_cairo_fonts_available <- function() {
    CairoFontMatch(":", sort=TRUE)
}

cairo_select_font_myriad <- function() {
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        windowsFonts(Myriad=windowsFont("Myriad"))
        par(family="Myriad")
    } else {
        CairoFonts(
            regular="Myriad Pro:style=Regular",
            bold="Myriad Pro:style=Bold",
            italic="Myriad Pro:style=Italic",
            bolditalic="Myriad Pro:style=Bold Italic",
            # symbol="Symbol:style=Regular"
            symbol="Myriad Pro:style=Regular"
        )
    }
}

cairo_select_font_arial <- function() {
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        windowsFonts(Arial=windowsFont("Arial"))
        par(family="Arial")
    } else {
        CairoFonts(
            regular="Arial:style=Regular",
            bold="Arial:style=Bold",
            italic="Arial:style=Italic",
            bolditalic="Arial:style=Bold Italic",
            symbol="Symbol:style=Regular"
        )
    }
}

save_grob_to_pdf <- function(g, filename, width_mm, height_mm,
                             title=filename, use_ggsave=FALSE,
                             embed_fonts_from_ggsave=FALSE) {
    cat("Saving grob to PDF:", filename, "\n")
    if (use_ggsave) {
        # AVOID.
        ggsave(filename, g, width=width_mm, height=height_mm, units="mm")
        if (embed_fonts_from_ggsave) {
            embed_fonts(filename)
        }
    } else {
        Cairo(filename, type="pdf", width=width_mm, height=height_mm, units="mm",
              title=title)
        # Cairo automatically embeds fonts:
        # http://cran.r-project.org/web/packages/Cairo/Cairo.pdf
        grid.draw(g)  # not: print(g)
        dev.off()
    }
}

INCHES_PER_MM <- 1/25.4
A4_SMALL_MM <- 210
A4_LARGE_MM <- 297
POINTS_PER_INCH <- 72
POINTS_PER_MM <- POINTS_PER_INCH * INCHES_PER_MM

BLANK_GROB <- rectGrob(gp=gpar(fill="white", alpha=0)) # alpha=0 makes it invisible
NOLEGEND <- theme(legend.position="none")
MOVELEGEND_BOTTOMLEFT <- theme(legend.justification=c(0,0),
                               legend.position=c(0,0))
MOVELEGEND_BOTTOMRIGHT <- theme(legend.justification=c(1,0),
                                legend.position=c(1,0))
MOVELEGEND_TOPRIGHT <- theme(legend.justification=c(1,1),
                             legend.position=c(1,1))
MOVELEGEND_TOPLEFT <- theme(legend.justification=c(0,1),
                            legend.position=c(0,1))
LEFTTITLE <- theme(plot.title=element_text(hjust=0, face="bold"))

BORDER_BOTTOM_AND_LEFT <- theme(panel.border=theme_border(c("left","bottom"))) # RNC hack, see above
# theme_blank() replaced by element_blank()
NO_LEGEND_BOX <- theme(legend.key=element_blank())
NO_GRID <- theme(panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank())

ggplot_rnc_theme <- function(fontsize) {
    theme_bw() +
        theme(text=element_text(size=fontsize))
}
