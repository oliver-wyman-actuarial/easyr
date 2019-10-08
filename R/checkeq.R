#' Check Value or Control Total
#'
#' Check actual versus expected values and get helpful metrics back.
#' Author: Bryce Chamberlain. Tech review: Lindsay Smeltzer.
#'
#' @param expected The expected value of the metric.
#' @param actual The actual value of the metric.
#' @param desc (Optional) Description of the metric being checked.
#' @param acceptable_pct_diff (Optional) Acceptable percentage difference when checking values. Checked as an absolute value.
#' @param digits (Optional) Digits to round to. Without rounding you get errors from floating values. Set to NA to avoid rounding.
#'
#' @return Message (via cat) indicating success or errors out in case of failure.
#' @export
#'
#' @examples
#' checkeq(expected=100,actual=100,desc='A Match')
checkeq <- function( expected, actual, desc = '', acceptable_pct_diff = .00000001, digits = 2 ){
  
  if( !is.numeric(expected) ) stop( 'Only numeric values for expected are allowed. You passed an expected value of type: ', typeof(expected) )
  if( !is.numeric(actual) ) stop( 'Only numeric values for actual are allowed. You passed an actual value of type: ', typeof(actual) )

  # Fix inputs.
  if(desc!='') desc = paste0(' [',desc,']')
  if(easyr::isval(digits)){
    expected = round( expected, digits )
    actual = round( actual )
  }

  # Check.
  if(expected==actual){  cat('control total success:',desc,' actual value matches expectation [',as.character(actual),']\n')

  } else if(!is.na(acceptable_pct_diff) && abs((actual-expected)/expected) <= abs(acceptable_pct_diff)) {

    cat(
       # use as.character so R doens't round output.
      'control total success:',desc,' actual value [',as.character(actual),'] is close to expectation [',as.character(expected),'] difference of [',
      as.character(actual-expected),'] or [',format((actual-expected)/expected*100,digits=3),'%] \n'
    ) 

  } else {
    stop(
       # use as.character so R doens't round output.
      'control total ERROR:',desc,' actual value [',as.character(actual),'] does not match expectation [',as.character(expected),'] difference of [',
      as.character(actual-expected),'] or [',format((actual-expected)/expected*100,digits=3, scientific = FALSE),'%]'
    ) 

  }
}