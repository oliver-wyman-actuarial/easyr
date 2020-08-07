# easyr 

Helpful functions from [Oliver Wyman Actuarial Consulting](https://www.oliverwyman.com/our-expertise/capabilities/actuarial.html).

**easyr makes difficult operations easy.** 

## Installation

You can install the latest version available on CRAN:

```r

install.packages('easyr')
require(easyr)

```

Or install the latest version from github:

```r

devtools::install_github( "oliver-wyman-actuarial/easyr" )
require(easyr)

```

## Getting Started

Tutorial: https://www.kaggle.com/brycechamberlain/easyr-tutorial.

Here is what a project looks like using easyr:

```r
# start with begin() to set up your workspace.
# begin will set the working directory to the location of this file and
#     run anything in fun/ or functions/ so put your functions there.
require(easyr)
begin()

# read.any reads in your data regardless of format, with powerful typing to get numbers and dates.
# use ?read.any to see the many options.
dt = read.any( 'path/to/file.extension' )

# let's look at a data dictionary to understand our data.
View( dict( dt ) )

# begin has already loaded dplyr and magrittr so you are ready to go.
dt %<>% 
  filter( !is.na(id) ) %>% 
  mutate( newcol = oldcol1 + oldcol 2 )

# use w to quickly write to out.csv'.
w( dt )

```

Function categories:

  * **shorthand**: protect your hands and move faster by typing less when using common functions.
  * **type conversion**: convert fields to dates, numbers, characters, and logical.
  * **data wrangling**: join and replace, explore data, factor-friendly joins and binds, etc.
  * **work flow**: cacheing, run folder, validate data, etc.

Data:

  * nastrings: common NA character values.
  * states: U.S. State abbreviations
  * cblind: color set built by and optimized for color-blind users.

Built, shared, and managed by Oliver Wyman Actuarial Consulting.

*Now accepting proposed contributions through GitHub!*

## Highlights

* **begin** is a one-line template for starting a project. It clears any pre-existing variables, loads dplyr, sets your working directory location, and more!
* **cc** (concatenate) replaces paste0 to reduce typing.
* **tonum**, **todate**, and **tobool** convert character vectors to numbers, dates, and logicals. R has functions that attempt this, but none that worked consistently for us. These functions handle many more incoming data formats.
* **read.any** automatically selects the best read function for you, and sets correct variable types on incoming data so you don't have to. Without easyr, R requires a different function for each file type.
* **jrepl** (join and replace) joins a mapping to add a column or replaces values. This usually takes multiple operations but jrepl combines it to one.
* **dict** returns information about a dataset's columns. It goes further than the str() function by flexibly determining variable types, counting unique and missing values, returning most and least common values, min/max, mode/median/mean, and more. fldict does the same for a folder of datasets.
* **eq** R users will understand that NAs often break equality checks. This check doesn’t mind them.
* **fmat** converts dates and numbers to pretty strings in a simpler way than the format function.
* *Check out the detailed list of functions below for more.*

## Philosophy

This packages comes from code we've written to make our daily work more efficient. We rely on it heavily in our organization.

It is built on the following tenets:

* **Writing code should be easy and fun**: minimize the amount of typing required to get to working code. Focus on common operations and make them easier and more flexible.

* **Your fingers are precious**: reduce the amount of typing and hand strain during coding. This means avoiding the shift key and choosing short names. Some function names won't be intuitive at first, but they’ll save you keystrokes later. 

* **Generic scope**: avoid functions that apply to domain-specific tasks. These belong in other packages.


## Make A Contribution

Any and all contributions are welcome. The easiest way to contribute is to add an [Issue](https://github.com/oliver-wyman-actuarial/easyr/issues). 
This can be a bug identified or even an idea you have on how we can improve easyr. Please be detailed and provide examples to make it easy for the community to resolve your issue/idea.

If you would like to make a more material contribution via Pull Request, please consider:
* The [Issue page](https://github.com/oliver-wyman-actuarial/easyr/issues) page lists open issues that we need your help to resolve.
* `build-install-test.R` is included to let you run tests. Please run this to ensure your changes don't cause tests or examples to fail.
* `tests/testthat` folder contains tests. Consider adding a test to validate your change and prevent someone else from breaking it in the future.
* `cmd-code-run-checks.txt` contains command-line scripts you can run to check if your changes will be acceptable to CRAN. If it isn't, it'll require extra work by us before we can submit to CRAN.

## Support

Submit an Issue or Pull Request via GitHub and the community will review it.

## Functions

Here are the functions in easyr by category. Use ?functionName to view detailed documentation for a function.

### Shorthand

Common operations shortened for elegance, simplicity, and speed.

| Name                       | Description                                                  |
| -------------------------- | ------------------------------------------------------------ |
| cc                         | Shorthand paste0/paste function to make typing these common function easier. Intuitively understands how to combine various-length inputs. |
|coalf| similar to dplyr function "coalesce" but handles factors appropriately. Checks each argument vector starting with the first until a non-null value is found.|
| crun                       | Concatenate arguments and run them as a command. Shorthand for eval( parse( text = paste0( ... ) ) ). Consider also using base::get() which can be used to get an object from a string, but only if it already exists. |
| ddiff                      | Date difference function plus shorthand mdiff, qdiff, ydiff. |
| eq                         | Vectorized flexible equality comparison which considers NAs as a value. Returns TRUE if both values are NA, and FALSE when only one is NA. |
| gr                         | Get the golden ratio.                                        |
| left/right/mid             | Behaves like Excel's LEFT, RIGHT, and MID functions.           |
| nanull                     | Facilitates checking for missing values. NULL values can cause errors on is.na checks, and is.na can cause warnings if it is inside if() and is passed multiple values. |
| %ni%                       | Not in. Opposite of %in% operator. Equivalent to `x %ni% y` is equivalent to `! x %in% y`. |
| isval                      | Opposite of nanull.                                          |
| read.txt                   | Read the text of a file into a character variable.           |
| other shorthand (multiple) | functions to save you keystrokes : na (is.na), nan (is.nan), null (is.null), ischar (is.character), isdate (is.Date), isnum (is.numeric), tochar (as.character) |
| pad0                       | Adds leading zeros to a character vector to make each value a specific length. For values shorter than length passed, leading zeros are removed. |
| spl                        | Extract a uniform random sample from a dataset or vector.             |
| strx                       | base::str (structure) function but only for names matching a character value (regex). |
| w                          | write function. Writes to csv without row names and automatically adds .csv to the file name if it isn't there already. Changes to .csv if another extension is passed. |

### Type Conversion

Helpful for setting or changing variable/vector data types.

| Name     | Description                                                  |
| -------- | ------------------------------------------------------------ |
| atype    | Auto-type a dataframe: automatically determine data types and perform conversions per column. Used by read.any to automatically set types. |
| char2fac, fac2char | Convert all character columns to factors and vice-versa.                    |
| match.factors |  Modifies two datasets so matching factor columns have the same levels. Typically this is used prior to joining or bind_rows in the easyr functions bindf, ijoinf, lfjoinf.|
|tobool | Flexible boolean conversion function.|
| todate|Flexible date conversion function using lubridate. Works with dates in many formats, without needing to know the format in advance. |
| tonum|Flexible number conversion for converting strings to numbers. Handles $ , ' and spaces. |
|xldate | Converts dates from Excel integers to something usable in R.|
| fmat | Format numbers and dates into character quickly and easily.|

### Data Wrangling

Help with reading and manipulating data.

| Name     | Description                                                  |
| -------- | ------------------------------------------------------------ |
| binbyvol | Bins a numerical column according to another numerical column's volume. |
| bindf    | dplyr's bind_rows doesn't work well when the data frame has factors. This function handles factors before binding rows. |
| dict     | Get information about a Data Frame or Data Table. Use getinfo to explore a single column instead. |
| drows    | Pull rows with a duplicated value.                           |
| getbetterint | Takes bucket names of binned values such as [1e3,2e3) or [0.1234567, 0.2) and formats the values nicely into values such as 1,000-2,000 or 0.12-0.20|
| fldict | Data dictionary for all data in a folder.|
| getinfo | Get information about a Column in a Data Frame or Data Table. Use getdatadict to explore all columns in a dataset instead.|
| namesx | Get column names that match a pattern.|
| ijoinf | dplyr's joins doesn't work well when the data frame has factors. This function handles factors before applying inner join via match. Also availalbe are ljoinf, rjoinf for left and right join.|
| jrepl | Join and replace. Joins to another dataset and replaces matched values on a given column. Good for quickly grabbing values from another dataset to fill in or replace.|
|read.any|Flexible read function to handle many types of files, data types, etc. Reduces downstream errors from read issues. Currently handles CSV, TSV, DBF, RDS, XLS (incl. when formatted as HTML), XLSX, and PDF. |
| sch | Search a data frame or vector. Attempts to replicate Excel search but with regex. |
| short_dollars|	Converts numeric plot axis dollars and attaches K and divides by 1000.|
| short_nums| Shortens axis numbering to thousands or millions and adds.|
| sumnum|Summarize all numeric columns in a dataset. |
| tcol| Transpose operation that sets column names equal to a column in the original data.|

### Workflow

Operations to run projects and organize code.

| Name     | Description                                                  |
| -------- | ------------------------------------------------------------ |
|begin|Perform common operations before running a script. Includes clearing environment variables, disabling scientific notation, loading common packages, and setting the working directory to the location of the current file.|
|caching|functions including cache.init, cache.ok, save.cache, and clear.cache.|
|check_equal|Check actual versus expected values and get helpful metrics back.|
|hashfiles|Create a hash uniquely representing the state of files or folders. Helpful for checking for changes.|
|runfolder|Run scripts in a folder. If an error occurs, it will tell you what file had the error. Helpful for running ordered scripts.|
|tcmsg|	Easy Try/Catch implementation to return the same message on error or warning. Makes it easier to write tryCatches.|
|tcwarn|Like tcmsg but returns a warning instead of an error when an error occurs, so code can continue to run.|
|validate.equal|Check that two data frames are equivalent.|

### Data

These data resources are also included.

| Name     | Description                                                  |
| -------- | ------------------------------------------------------------ |
|nastrings|List of strings considered NA by easyr. Includes blank strings, "NA", excel errors, etc.|
|states|Helpul dataset of U.S. State abbreviations and names.|
|cblind|Charting colors optimized for and selected by colorblind individuals.|
