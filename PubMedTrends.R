# http://rpsychologist.com/an-r-script-to-automatically-look-at-pubmed-citation-counts-by-year-of-publication/

local({
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        RCurl,  # before RCurl installation: sudo apt-get install libcurl3-dev
        XML,
        plyr,  # THIS FILE MAY BE BROKEN: REMOVED PLYR BUT NOT UPDATED
        ggplot2,
        directlabels
    )
})


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

PubMedTrends <- new.env()


########################
# Download PubMed Data #
########################

PubMedTrends$PubMedTrend <- function(query, yrStart = 1950, yrMax = 2009,
                                     calculate_relative_frequencies = FALSE)
{
    ### Some error checking ###
    if (is.numeric(yrStart) == FALSE || is.numeric(yrMax) == FALSE) {
        stop("One of the year values is not numeric")
    }
    if (yrStart < 1800) {
        stop(paste("Sure you want to look for hits from the 17th century (yrStart = " ,
                   yrStart, ")?\n", sep = ""))
    }
    this.year <- Sys.time()
    this.year <- as.integer(format(this.year, "%Y"))
    if (yrMax > this.year) {
        stop(paste("Are you from the future? Please check your year interval; yrMax =",yrMax,"\n"))
    }
    if (yrMax < yrStart) {
        stop("yrMax is smaller than yrMin!")
    }

    ### Start main search function ###
    getCount <- function(query.term) {
        # convert spaces to '+'
        query.gsub <- gsub(" ", "+", query.term)
        # convert some characters to brower friendly text (better to be safe than sorry)
        query.gsub <- gsub('\\*', "%2A", query.gsub)
        query.gsub <- gsub('"', "%22", query.gsub)
        query.gsub <- gsub("\\[", "%5B", query.gsub)
        query.gsub <- gsub("\\]", "%5D", query.gsub)
        # add progressbar
        pb <- txtProgressBar(min = yrStart, max = yrMax, style = 3)
        # create empty data frame
        df <- data.frame(NULL)
        cat("Searching for: ", query.term,"\n")

        # Start retrieval loop
        for (i in yrStart:yrMax) {
            # tell progressbar how it's going
            setTxtProgressBar(pb, i)
            # add publication date [dp] to query
            query.parsed <- paste(query.gsub, "+AND+",i, "%5Bppdat%5D", sep = "")
            # Get XML with number of hits for query.parsed

            url <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&rettype=count&term=",
                         query.parsed, sep = "")
            # cat("\n", url, "\n", sep = "")
            pub.esearch <- getURL(url)
            # Parse XML
            # cat("\n", pub.esearch, "\n", sep = "")
            pub.esearch <- xmlTreeParse(pub.esearch, asText = TRUE)
            # Get number of hits from XML
            pub.count <- as.numeric(xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["Count"]]))
            # Don't add anything if count is 0
            if (pub.count != 0) {
                df <- rbind(df, data.frame("year" = i, "count" = pub.count))
            }
            # Wait 0.5 sec
            Sys.sleep(0.5)
        }
        # close progressbar
        close(pb)
        return(df)
    }
    # Run getCount() for all query terms
    df <- ldply(query, getCount)

    # RNC made conditional:
    if (calculate_relative_frequencies) {
        ### Calculate relative frequencies ###
        # load file with pubmed total citations from 1947-2009
        load(file = "total_table")
        # match year
        match <- match(df$year, total.table$year)
        # add total count
        df$total_count <- total.table$total_count[match]
        # compute relative count times 10 000, i.e. show number of matches per 1 million PubMed citations
        df$relative <- (df$count / df$total_count) * 10000
    }

    cat("\nAll done!")
    return(df)
}


#######################
### Show total hits ###
#######################
PubMedTrends$PubTotalHits <- function(args = FALSE)
{
  # Get column total for query 'x'
  GetCount <- function(x) {
    df <- data.frame("search_name" = x, "total_hits" = colSums(df[df$.id == x,][3]))
  }
  # Index all query names
  query.index <- unique(df$.id)
  # Use GetCount() for every term in 'query.index' and return as data.frame
  df <- ldply(query.index, GetCount)
  # if argument is 'query' add full query instead of query name.
  # if there is no argument specified both name and query will be shown
  if (args == "query" || args == FALSE) {
    # remove names
    names(query) <- NULL
    # add queries to df
    df <- cbind(df, "query" = query)
    # reorder columns
    df <- df[,c(1,3,2)]
    # remove 'names' if we only want queries
    if (args == "query") df <- df[-1]
  }
  return(df)
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("PubMedTrends" %in% search()) detach("PubMedTrends")
attach(PubMedTrends)  # subsequent additions not found, so attach at the end
