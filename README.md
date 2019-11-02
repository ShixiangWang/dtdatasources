
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dtdatasources: Data Sources for Shiny Datatables

<!-- badges: start -->

<!-- badges: end -->

## Overview

`dtdatasources` provides Shiny [server-side
datatables](https://rstudio.github.io/DT/server.html) backends for the
DT package, allowing you to connect your datatables directly to
databases or APIs instead of just dataframes.

## Installation

You can install development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("grahamrp/dtdatasources")
```

## Available Data Sources

  - sqlite
      - paging
      - sorting

So far I’ve only written a basic sqlite implementation, but would like
to create more. If you would like to help, please do\! See the
[Contributing](#contributing) section.

## More Info

When you want to use `DT::renderDataTables`/`DT::renderDT` with large
datasets you can choose to render the table on the server with
`renderDT(big_dataset, server = TRUE)`. This will perform the dataset
filtering/sorting/paging on the server where Shiny is running, instead
of in the user’s web browser, speeding things up for large datasets.

The default DT implementation of server-side processing still requires
the data to be in a *dataframe* on the server. For very large datasets,
or where the data is stored outside the Shiny process, for example in a
database or API, we would not want to copy the entire dataset into a
datatable on the server, as it may not have enough resources to hold it
in memory, or enough processing power to perform the server-side
filtering/sorting/paging.

For this reason, `renderDT` provides a `funcFilter` parameter where we
can provide our own function that describes how to fetch, filter, sort,
and page our dataset. We can therefore plug datatables into wider
datasources that just dataframes

The `dtdatasources` package will provide implementations of `funcFilter`
for various datasources, for you to use directly, or as examples to
adapt to your own implementations.

## Example

This example shows how to connect a server-side datatable to a sqlite
database. Take a look at
[inst/app.R](https://github.com/grahamrp/dtdatasources/blob/master/inst/app.R)
for the shiny app, or run `shiny::runApp(system.file("app.R", package =
"dtdatasources"))`.

``` r
library(shiny)
library(DT)
library(DBI)
library(dtdatasources)

# Setup an example database and table
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "mtcars", mtcars)

ui <- fluidPage(
  DTOutput("tbl")
)

server <- function(input, output, session) {

  # Create an initial dataframe. This only needs to contain column names, and
  # need not have any rows.
  initial_df <- dbGetQuery(con, "SELECT * FROM mtcars LIMIT 0;")

  # Create a funcFilter function describing how to get data for a datatable.
  # The filter factory 
  mtcars_filter <- dtdatasources::sql_filter_factory(
    # Use the sqlite connection created above
    con = con,
    # Set the query_fun to query_sqlite, or implement your own version
    query_fun = dtdatasources::query_sqlite,
    # Any additional args are passed into query_sqlite. In query_sqlite's case
    # it accepts a `tbl` parameter for which table to use.
    tbl = "mtcars"  
  )

  # Call renderDT with our custom filter function
  output$tbl <- renderDT(
    initial_df,
    server = TRUE,  # Must be TRUE to perform processing in R, not in the browser
    rownames = FALSE,  # Must be FALSE for the query_sqlite implementation
    funcFilter = mtcars_filter  # Provide the sqlite function filter created above
  )
}

shinyApp(ui, server)
```

If you want to use this example to create your own implementations of DT
data sources, take a look at `sqlite.R` and provide your own version of
the `query_sqlite` function.

## Contributing

Feedback, bug reports, fixes, and feature requests are welcome; file
issues or seek support
[here](http://github.com/grahamrp/dtdatasources/issues).

## Some References for `funcFilter`

| description                                                   | url                                                                            |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| Open issue to make a funcFilter example                       | <https://github.com/rstudio/DT/issues/194>                                     |
| Custom filtering problem with filter ranges and `filterRow()` | <https://github.com/rstudio/DT/issues/50>                                      |
| Row selection                                                 | <https://github.com/rstudio/DT/issues/75>                                      |
| Description of the overall problem                            | <https://groups.google.com/forum/#!msg/shiny-discuss/zaPqkMdhwy4/jHGFwBfEBQAJ> |
