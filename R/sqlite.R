#' Sqlite implementation of a query_fun.
#'
#' Simple implementation showing how to implement a \code{query_fun} for a sqlite DB.
#'
#' This function can be provided to \code{sql_filter_factory} to describe how to
#' fetch a datatable payload from a sqlite table.
#'
#' This implementation provides paging and sorting, but filtering and row indices
#' filters are not yet implemented.
#'
#' This function will be called via \code{sql_filter_factory} with arguments
#'
#' \code{con}, \code{page}, and \code{...} (where \code{...} are any extra
#' arguments given to \code{sql_filter_factory}). With this example an additional
#' \code{tbl} parameter has been implemented, which would be expected to be
#' passed in when \code{sql_filter_factory} is called.
#'
#' @param con DBI database connection
#' @param params named list provided by a datatable containing sorting, filtering
#'   and pagination data.
#' @param tbl string, the table/view in sqlite.
#' @param id_field string, optional. Field used to identify a row when using \code{input$tbl_rows_selected}.
#' @export
#' @examples
#' \dontrun{
#' sql_filter_factory(con, query_sqlite, tbl = "mtcars")
#' }
query_sqlite <- function(con, params, tbl, id_field = NA) {

  q = params
  n = get_sqlite_count(con, tbl)
  data = get_sqlite_page(con, q, tbl)

  i = seq_len(n)
  if (length(i) && any((k <- q$search[["value"]]) != "")) {
    # Set search options
    if (length(q$columns) != ncol(data))
      return(list(draw = as.integer(q$draw), recordsTotal = n,
                  recordsFiltered = 0, data = list(), DT_rows_all = seq_len(n),
                  DT_rows_current = list()))
    searchable = logical(ncol(data))
    for (j in seq_len(ncol(data))) {
      if (q$columns[[j]][["searchable"]] == "true")
        searchable[j] = TRUE
    }
    global_opts = list(
      smart = !identical(q$search[["smart"]], "false"),
      regex = q$search[["regex"]] != "false",
      caseInsensitive = q$search[["caseInsensitive"]] == "true")

    # Obtain all data from SQL for filtering by search
    data = get_sqlite_page2(con, q, tbl)
    dg = data[i, searchable, drop = FALSE]
    i = i[DT::doGlobalSearch(dg, k, options = global_opts)]
    if (length(i) != n)
      data = data[i, , drop = FALSE]
  }

  DT_rows_all = i
  if (!is.na(id_field)) {
    DT_rows_current = data[[id_field]]
  } else {
    DT_rows_current = i
  }

  list(draw = as.integer(q$draw),
       recordsTotal = n, recordsFiltered = n,
       data = data,
       DT_rows_all = DT_rows_all, DT_rows_current = DT_rows_current)
}

# Get rows from tbl for the visible datatable page
get_sqlite_page <- function(con, params, tbl) {
  # Translate dt query params into a sql query
  query <- glue::glue_sql("SELECT * FROM {`tbl`} ",
                    order_by_clause(params),
                    " LIMIT {params$length} OFFSET {params$start}",
                    .con = con)
  DBI::dbGetQuery(con, query)
}

get_sqlite_page2 = function (con, params, tbl) {
  query <- glue::glue_sql("SELECT * FROM {`tbl`} ", order_by_clause(params), .con = con)
  DBI::dbGetQuery(con, query)
}

# Get the total record count in tbl
get_sqlite_count <- function(con, tbl) {
    query <- glue::glue_sql("SELECT COUNT (*) AS n FROM {tbl}", .con = con)
    result <- DBI::dbGetQuery(con, query)
    result$n
}

# Return ORDER BY clause or "", accounting for zero-based col indexing of q
order_by_clause <- function(params) {
  order <- params$order
  if (is.null(order)) return("")

  orderings <- purrr::map_chr(order, ~paste(as.integer(.x$column) + 1, .x$dir))
  orderings <- paste(orderings, collapse = ", ")

  paste("ORDER BY", orderings)
}

