
#' Bin by Volume
#' 
#' Bins a numerical column according to another numerical column's volume.
#' For example if I want to bin a column "Age" (of people) into 10 deciles according to "CountofPeople" then I will get Age breakpoints returned by my function such that there is 10% of CountofPeople volume in each binned Age group. 
#' This function handles NA's as their own separate bin, and handles any special values you want to separate out.
#' Author: Scott Sobel. Tech Review: Bryce Chamberlain.
#'
#' @param df (Data Frame) Your data.
#' @param groupby (Character) Name of the column you'll create cuts in. Must be the character name of a numeric column.
#' @param vol (Character) Name of the column for which which each cut will have an equal percentage of volume.
#' @param numbins  Number of bins to use.
#'
#' @return  Age breakpoints returned by my function such that there is 10% of CountofPeople volume in each binned Age group. 
#' @export
#'
#' @examples
#' # bin Sepal.Width according to Sepal.Length.
#' iris$bin <- binbyvol(iris, 'Sepal.Width', 'Sepal.Length', 5)
#' 
#' # check the binning success.
#' aggregate( Sepal.Length ~ bin, data = iris, sum )
binbyvol <- function (df, groupby, vol, numbins) {

  # Validation and errors.
  if( ! groupby %in% colnames( df ) ) stop( 'Column [', groupby, '] not found in [df] argument.' )
  if( ! vol %in% colnames( df ) ) stop( 'Column [', vol, '] not found in [df] argument.' )
  if( !is.numeric( df[[ groupby ]] ) ) stop( '[groupby] column must be numeric but [', groupby, '] is not.' )
  if( !is.numeric( df[[ vol ]] ) ) stop( '[vol] column must be numeric but [', vol, '] is not.' )
  if( !is.numeric(numbins) || floor(numbins) != numbins ) stop( '[numbins] argument must be a whole number.' )
  
  # Create bucket map. 
  
  temp0 <- df[, c(groupby, vol) ] # save dataset of just groupby and vol.
  temp0$id <- 1:nrow(temp0) # preserve initial order.
  
  # Aggregate by summed volume, to make later calcs faster.
  temp <- stats::aggregate( df[, vol] ~ df[, groupby], data = temp0, sum, na.action = stats::na.pass, na.rm = TRUE)
  names(temp) <- c("groupby","vol")
  
  # Get cumulative percentage of the volume metric.
  temp <- temp[ order( temp$groupby, temp$vol ), ] # must be sorted in order to get cumulative sum.
  temp$cumulative_vol_pct <- cumsum( temp$vol / sum(temp$vol, na.rm = TRUE) )
  
  # Group non-NA into bucket percentages.
  temp_notNA <- temp[ !is.na( temp$groupby ), ]
  temp_notNA$percentage_bucket <- Hmisc::cut2( temp_notNA$cumulative_vol_pct, cuts = c( 0:(numbins-1) )/numbins, oneval = FALSE ) # assign to percentage buckets.
  
  # Bring this into the overall temp map, including handling NAs.
  temp$percentage_bucket <- NA
  temp$percentage_bucket[ 1:nrow(temp_notNA) ] <- as.character(temp_notNA$percentage_bucket) # The non-NAs will be first due to ordering, set these equal to the values from the temp_notNA bucketing.
  if ( any( is.na( temp$groupby ) ) ) temp$percentage_bucket[ (nrow(temp_notNA)+1):nrow(temp) ] <- "(blank)" # Set NAs to (blank).
  
  # Identify where the percentage buckets match to group by, be determining the min/max of groupby variable for each bucket.
  agg <- data.frame( stats::xtabs( vol ~ as.factor(percentage_bucket), data = temp_notNA, na.action = stats::na.pass, addNA = TRUE, drop.unused.levels = FALSE ) ) # as factor to increase speed?
  names(agg) <- c("percentage_bucket", "vol")
  
  # Get the min and max for each bucket.
  aggminmax <- stats::aggregate( groupby ~ percentage_bucket, data = temp, min, na.action = stats::na.pass, na.rm = TRUE )
  names(aggminmax) <- c("percentage_bucket","min")
  aggminmax$max <- c( aggminmax$min[ 2:nrow(aggminmax) ], max( df[, groupby], na.rm = TRUE ) ) # calculate bucket max by taking the next min for each bucket and ending at the overall max.
  
  # Merge both aggregations and create bucket names.
  agg <- merge( agg, aggminmax, by.x = "percentage_bucket", by.y = "percentage_bucket", all.x = TRUE, all.y = FALSE )
  agg$range <- paste0( substr(agg$percentage_bucket, 1, 1), agg$min, ",", agg$max, substr( as.character(agg$percentage_bucket), nchar(as.character(agg$percentage_bucket) ), nchar(as.character(agg$percentage_bucket) ) ) )
  
  # Bring in groups via merge.
  temp <- merge( temp, agg, by.x = "percentage_bucket", by.y = "percentage_bucket", all.x = TRUE, all.y = FALSE, sort = FALSE )
  
  # Apply the bucket map to the data.
  
  temp0 <- merge( temp0, temp, by.x = groupby, by.y = "groupby", all.x = TRUE, all.y = FALSE, sort = FALSE )
  temp0 <- temp0[ order( temp0[, groupby ] ), ]
  temp0$range[ temp0$percentage_bucket == "(blank)" ] <- "(blank)"
  temp0$range <- factor( temp0$range, levels = unique(temp0$range) )
  temp0 <- temp0[ order(temp0$id), ] # return to initial ordering.
  
  # Return the final values. 
  return( temp0$range )

}