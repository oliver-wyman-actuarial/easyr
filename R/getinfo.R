#' Get Info
#' 
#' Get information about a Column in a Data Frame or Data Table. Use getdatadict to explore all columns in a dataset instead.
#' Author: Scott Sobel. Tech Review: Bryce Chamberlain.
#'
#' @param df Data Frame or Data Table.
#' @param colname (Character) Name of the column to get information about.
#' @param topn (Optional) Number of top values to print.
#' @param botn (Optional) Number of bottom values to print.
#' @param graph (Boolean Optional) Output a chart of the column.
#' @param ordered (Optional) 
#' @param display (Optional) 
#' @param cutoff (Optional) 
#' @param main (Optional) 
#' @param cex (Optional) 
#' @param xcex (Optional) 
#' @param bins (Optional) 
#' @param col (Optional) 
#'
#' @return Only if display = FALSE, returns information about the column. Otherwise information comes through the graphing pane and the console (via cat/print).
#' @export
#'
#' @examples
#' getinfo(iris,'Sepal.Width')
#' getinfo(iris,'Species')
getinfo <- function(
  
  df, 
  colname, 
  topn = 5, botn = 5, 
  graph = TRUE, 
  ordered = TRUE,
  display = TRUE, 
  cutoff = 20, 
  main = NULL, 
  cex = 0.9, xcex = 0.9,
  bins = 50, col = "light blue"

) {
  
  # Get the column as a data.table for faster processing.
  if ( "data.table" %in% class(df) ) {
    datacol <- subset( df, select = colname )
  } else {
    datacol <- data.table::data.table(df[, colname])
  }
  names(datacol) <- "Column"
  
  # Start building the info data frame.
  info <- as.data.frame( matrix( NA, nrow = 1, ncol = 22 ) )
  names(info) <- c("Col #","Column","Data Type","Check Numeric","# Distincts","Top Values",
                   "Bottom Values","# Missing","% Missing","# NULL","% NULL","# (blank)","% (blank)",
                   "# =0","% =0","# <0","% <0","Mode","Average","Minimum","Maximum",
                   "Sum")
  
  info[, "Data Type"] <- class( datacol[[1]] )
  
  # Limit characters to 50 characters.
  if ( info[, "Data Type"] == "character" ) datacol <- data.table::data.table( Column = substr( datacol[,1][[1]], 1, 50 ) )
  
  # Summarize values.
  tbl = data.frame( table( datacol$Column ), stringsAsFactors = FALSE )
    
  if( nrow( tbl ) > 0 ) {
    colnames( tbl ) <- c('Unique','Count')
  } else {
    tbl <- data.frame( Unique = 'no columns found', Count = 0, stringsAsFactors = FALSE ) # sometimes tbl comes back empty.
  }
  
  # Get distinct values.
  info[, "# Distincts"] <- nrow(tbl)
  if ( info[, "# Distincts"] <= 200 ) { tbl <- data.table::setorder( tbl, na.last = TRUE ) } # sort by the first column, in this case 'Unique'.
  sorteduniques <- tbl[,1] # save the sorted unique values.
  if ( info[, "# Distincts"] <= 200 ) { tbl <- data.table::setorderv( tbl, 'Count', order = -1, na.last = TRUE ) } # now sort by descending count.
  
  # Get the column number in the data and save the column name.
  info[, "Col #"] <- which( colnames(df) == colname )
  info$Column <- colname
  
  # Set check_numeric. 
  if ( ! info[ , "Data Type" ] %in% c( "character", "factor", "ordered factor" ) ) {
    info[, "Check Numeric"] <- "all numeric"
  } else {
    if ( suppressWarnings( sum( is.na( as.numeric( datacol[, 1][[1]] ) ) ) ) == sum( is.na( datacol[, 1] ) ) ) {
      info[, "Check Numeric"] <- "all numeric"
    } else {
      info[, "Check Numeric"] <- "not all numeric"
    }
  }
  
  # Set top and bottom values, with ... where not all data is shown.
  
  # Top values.
  info[, "Top Values"] <- paste( utils::head( sorteduniques, min( topn, info[, "# Distincts"] ) ), collapse = ", " )
  if ( info[, "# Distincts"] > topn ) { info[, "Top Values"] <- paste0( info[, "Top Values"], ", ..." ) }
  # Bottmo values.
  info[, "Bottom Values"] <- paste( utils::tail( sorteduniques, min( botn, info[, "# Distincts"] ) ), collapse = ", " )
  if (info[, "# Distincts"] > botn) { info[, "Bottom Values"] <- paste0( "..., ", info[, "Bottom Values"] ) }
  
  # Get number and % missing.
  info[, "# Missing"] <- sum( is.na(datacol) )
  info[, "% Missing"] <- info[, "# Missing"] / nrow( datacol )
  
  # null, blank, and zero info
  if ( info[, "Data Type"] %in% c("character","factor","ordered factor") ) {
    # For charcters/factors.
    info[, "# NULL"] <- sum( datacol == "NULL", na.rm = TRUE )
    info[, "# (blank)"] <- sum( datacol == "(blank)", na.rm = TRUE )
    info[, "# =0"] <- sum( datacol == "0", na.rm = TRUE )
    info[, "# <0"] <- 0
    info$Average <- NA
    info$Sum <- NA
  } else {
    # For others (numeric, date, etc.)
    info[, "# NULL"] <- 0
    info[, "# (blank)"] <- 0
    info[, "# =0"] <- sum( datacol  == 0, na.rm = TRUE )
    info[, "# <0"] <- sum( datacol < 0, na.rm = TRUE )
    info$Average <- mean( datacol[,1][[1]], na.rm = TRUE )
    info$Sum <- ifelse( info[, "Data Type"] == "Date", NA, sum( as.numeric(datacol[,1][[1]] ), na.rm = TRUE ) )
  }
  
  # %s of the same.
  info[, "% NULL"] <- info[, "# NULL"] / nrow( datacol )
  info[, "% (blank)"] <- info[, "# (blank)"] / nrow( datacol )
  info[, "% =0"] <- info[, "# =0"] / nrow( datacol )
  info[, "% <0"] <- info[, "# <0"] / nrow( datacol )
  info$Mode <- tbl[1,1][[1]]
  info$Minimum <- sorteduniques[1]
  info$Maximum <- ifelse(length(sorteduniques) > 0, sorteduniques[length(sorteduniques)], NA)
  
  # Display using cat if requested.
  if( display ) {
    
    # General info.
    cat(paste0("\n        Column: ", info$Column, "\n",
               "         Col #: ", info[, "Col #"], "\n",
               "     Data type: ", info[, "Data Type"], "\n",
               " Check Numeric: ", info[, "Check Numeric"], "\n",
               "   # Distincts: ", info[, "# Distincts"], " (out of ", nrow( datacol ), ")\n",
               "    Top Values: ", info[, "Top Values"], "\n",
               " Bottom Values: ", info[, "Bottom Values"], "\n",
               "       Missing: ", info[, "# Missing"], " (", round(info[, "% Missing"] * 100, 1),"%)\n",
               "        'NULL': ", info[, "# NULL"], " (", round(info[, "% NULL"] * 100, 1),"%)\n",
               "     '(blank)': ", info[, "# (blank)"], " (", round(info[, "% (blank)"] * 100, 1),"%)\n",
               "           = 0: ", info[, "# =0"], " (", round(info[, "% =0"] * 100, 1),"%)\n",
               "           < 0: ", info[, "# <0"], " (", round(info[, "% <0"] * 100, 1),"%)\n",
               "          Mode: ", info$Mode, " (", round((sum(datacol[,1][[1]] == info$Mode) / nrow( datacol ) ) * 100, 1), "%)\n",
               "       Average: ", info$Average, "\n",
               "       Minimum: ", info$Minimum, "\n",
               "       Maximum: ", info$Maximum, "\n",
               "           Sum: ", info$Sum, "\n\n" )
    )
    
    # Specific to characters and factors.
    if ( info[, "Data Type"] %in% c("character","factor","ordered factor") ) {
      if ( info[, "# Distincts"] <= 200 ) {
        summary <- data.table::data.table( t(tbl[,2]) )
        names(summary) <- as.character( tbl[,1] )
        print(summary, row.names = FALSE)
      } else {
        cat("Summary omitted (greater than 200 values)\n")
      }
      
      # Other data types.
    } else {
      
      print( summary( datacol ) )
      if ( !info[, "Data Type"] == "Date" ) { print( stats::quantile(datacol, c(0.01, 0.05, 0.10, 0.90, 0.95, 0.99), na.rm = TRUE) ) }
      
    }
    
    # Show graph if requested.
    if ( graph == TRUE & info[, "# Missing"] < nrow( datacol ) ) {
      
      # Characters and factors.
      if (info[, "Data Type"] %in% c("character","factor","ordered factor","logical")) {
        if (ordered == FALSE) { tbl <- data.table::setorder(tbl, tbl$Unique, na.last = TRUE) }
        tbl1 <- tbl
        if (nrow(tbl) > cutoff) {
          allothers <- data.table::data.table(Unique = "All Others", Count = sum(tbl$Count[(cutoff+1):length(tbl)]))
          tbl1 <- rbind(tbl[1:cutoff,], allothers)
        }
        if (is.null(main)) { main <- paste0("Distribution of ", colname) }
        bplt <- graphics::barplot(tbl1$Count, names.arg = tbl1$Unique, main = main, ylab = "Count", ylim = c(0, 1.15*max(tbl1$Count)), col = col, cex.names = xcex)
        graphics::text(x = bplt, y = tbl1$Count + 0.05*max(tbl1$Count), labels = sprintf(round((tbl1$Count / sum(tbl1$Count)) * 100, 3), fmt="%2.1f%%"), cex = cex)
        
        # Other data types.
      } else {
        if (is.null(main)) { main <- paste0("Histogram of ", colname) }
        graphics::hist(datacol[,1][[1]], breaks = bins, main = main, xlab = "", ylab = "Count", col = col)
      }
    }
    
    # If no graph was requested, return info.
  } else {
    
    return(info)
    
  }
  
}