% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecopy.R
\name{ecopy}
\alias{ecopy}
\title{Copy to Clipboard}
\usage{
ecopy(
  x,
  showrowcolnames = c("cols", "rows", "both", "none"),
  show = FALSE,
  buffer = 1024
)
}
\arguments{
\item{x}{Object you'd like to copy to the clipboard.}

\item{showrowcolnames}{(Optional) Show row and column names. Choose 'none', 'cols', 'rows', or 'both'.}

\item{show}{(Optional Boolean)  Set to 'show' if you want to also print the object to the console.}

\item{buffer}{(Optional) Set clipboard buffer size.}
}
\description{
Copies a data.frame or anything that can be converted into a data.frame. After running this, you can use ctrl+v or Edit > Paste to paste it to another program, typically Excel.
A simple use case would be ecopy(names(df)) to copy the names of a data frame to the clipboard to paste to Excel or Outlook.
Author: Scott Sobel. Tech Review: Bryce Chamberlain.
}
\examples{
ecopy( iris, showrowcolnames = "cols", show = 'show' )
ecopy(iris)
}
