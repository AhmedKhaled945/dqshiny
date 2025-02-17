#' Adds an uiOutput and renders an enhanced rhandsontable html widget
#'
#' @description dq_handsontable_output adds a fluidRow containing a column with
#' the given width, ready to support a dq_handsontable.
#'
#' @param id id of the element
#' @param width width of the table in bootstrap columns
#' @param offset optional offset of the column
#'
#' @return dq_handsontable_output: fluidRow containing the output fields
#' @rdname dq_render_handsontable
#' @export
dq_handsontable_output <- function(id, width = 12L, offset = 0L) {
  requireNamespace("rhandsontable", quietly = TRUE)
  requireNamespace("shiny", quietly = TRUE)
  if (is.null(id)) return(NULL)
  ns <- dq_NS(id)
  shiny::fluidRow(shiny::column(
    width, offset = offset,
    shiny::uiOutput(ns("pages")),
    shiny::uiOutput(ns("filters")),
    rhandsontable::rHandsontableOutput(id),
    init()
  ))
}

#' Adds an uiOutput and renders an enhanced rhandsontable html widget
#'
#' @description dq_render_handsontable renders a rhandsontable into the given
#' uiOutput id with the given data and parameters. Can also contain several
#' filters to filter the data and a feature to split the table into several
#' pages with a given page size. The function will also add all needed
#' observeEvents to establish the required functionalities. If table is not
#' readOnly, all user inputs will automatically stored and updated independent
#' from any filters, sortings or pages.
#'
#' @param data data to show in the table, should be a data.frame'ish object, can
#' also be reactive(Val) or a reactiveValues object holding the data under the
#' given id (e.g. myReactiveValues[[id]] <- data). In case of reactiveVal(ues)
#' data will always be in sync with user inputs.
#' @param context the context used to specify all ui elements used for this
#' table, can be omitted which ends up in a randomly generated context
#' NOTE: this parameter is deprecated and will be removed soon
#' @param filters optional, adds filters for each column, types must be one of
#' "Text", "Select", "Range", "Date", "Auto" or "" (can be abbreviated) to add a
#' Text-, Select-, Range-, DateRange-, AutocompleteInput or none, vectors of
#' length one will add a filter of this type for each column and NA will try to
#' guess proper filters, can also contain nested lists specifying type and
#' initial value (e.g. list(list(type = "T", value = "init"), NA, "T", ...))
#' @param reset optional logical, specify whether to add a button to reset
#' filters and sort buttons to initial values or not
#' @param page_size optional integer, number of items per page, can be one of
#' 10, 25, 50, 100 or any other value(s) which will be added to this list, first
#' value will be used initially, NULL will disable paging at all
#' @param sorting optional, specify whether to add sort buttons for every column
#' or not, as normal rhandsontable sorting won't work properly when table is
#' paged, value can be logical of length one or a vector specifying the initial
#' sort "col"umn and "dir"ection e.g. c(dir="down", col="Colname")
#' @param columns optional, specify which columns to show in the table, useful
#' in combination with reactive values, which will still hold all the data
#' @param width_align optional boolean to align filter widths with hot columns,
#' should only be used with either horizontal_scroll, stretchH = "all" or a
#' table fitting in its output element
#' @param horizontal_scroll optional boolean to scroll the filter row according
#' to the hot table, especially useful for tables with many columns
#' @param table_param optional list, specify parameters to hand to rhandsontable
#' table element
#' @param cols_param optional list, specify parameters to hand to rhandsontable
#' cols elements
#' @param col_param optional list of lists to specify parameters to hand to
#' rhandsontable col elements
#' @param cell_param optional list of lists to specify parameters to hand to
#' rhandsontable cells
#' @param session shiny session object
#'
#' @return dq_render_handsontable: the given data
#' @author richard.kunze
#' @export
#' @seealso \code{\link[rhandsontable:rhandsontable]{rhandsontable}},
#' \code{\link[rhandsontable:hot_cols]{hot_cols}} and
#' \code{\link[rhandsontable:hot_col]{hot_col}}
#'
#' @examples ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' shinyApp(
#'   ui = fluidPage(
#'     dq_handsontable_output("randomTable", 9L)
#'   ),
#'   server = function(input, output, session) {
#'     hw <- c("Hello", "my", "funny", "world!")
#'     data <- data.frame(A = rep(hw, 500), B = hw[c(2,3,4,1)],
#'       C = 1:500, D = Sys.Date() - 0:499, stringsAsFactors = FALSE)
#'     dq_render_handsontable("randomTable", data,
#'       filters = c("A", NA, NA, NA), sorting = c(dir = "up", col = "B"),
#'       page_size = c(17L, 5L, 500L, 1000L), width_align = TRUE,
#'       col_param = list(list(col = 1L, type = "dropdown", source = letters)),
#'       cell_param = list(list(row = 2:9, col = 1:2, readOnly = TRUE))
#'     )
#'   }
#' )
#'
#' }
dq_render_handsontable <- function(
  id, data, context = NULL, filters = "T", page_size = 25L, reset = TRUE,
  sorting = NULL, columns = NULL, width_align = FALSE, horizontal_scroll = FALSE,
  table_param = NULL, cols_param = NULL, col_param = NULL, cell_param = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  requireNamespace("rhandsontable", quietly = TRUE)
  requireNamespace("shiny", quietly = TRUE)

  # initial settings
  if (is.null(id) || is.null(data) || is.null(session)) return()
  if (!missing(context)) {
    warning("Context parameter is deprecated and will be removed soon!")
  }
  if (length(columns) == 0L) columns <- TRUE

  ns <- dq_NS(id)
  app_input <- session$input
  app_output <- session$output

  session <- session$makeScope(id)
  input <- session$input
  output <- session$output

  table_data <- data
  dqv <- shiny::reactiveValues()

  paged <- length(page_size) > 0L && any(page_size > 0L)
  to_sort <- (length(sorting) > 0L && !identical(sorting, FALSE))
  no_update <- FALSE

  filter_values <- shiny::reactive(get_filters(input))

  reduced <- shiny::reactive({
    if (is.null(dqv$full)) return()
    if (is.null(filters)) {
      dqv$full[, columns, drop = FALSE]
    } else {
      f_vals <- filter_values()
      if (length(f_vals) == 0) return()
      l <- vapply(f_vals, length, 0L)
      df <- text_filter(dqv$full[, columns, drop = FALSE], f_vals[l == 1L])
      range_filter(df, f_vals[l == 2L])
    }
  })

  sorted <- shiny::reactive({
    if (to_sort && length(reduced())) sort_data(reduced(), dqv$sorting)
    else reduced()
  })

  hot <- shiny::reactive({
    if (paged && length(sorted())) {
      sel <- as.integer(input$pageSize)
      update_page(sorted(), input$pageNum, sel, session)
    } else {
      sorted()
    }
  })

  if (shiny::is.reactivevalues(table_data)) {
    shiny::observeEvent(table_data[[id]], {
      if (no_update) {
        no_update <<- FALSE
      } else {
        dqv$full <- as.data.frame(table_data[[id]])
        if (!is.null(filters)) {
          update_filters(dqv$full[, columns, drop = FALSE], filters, session)
        }
      }
    }, ignoreInit = TRUE)
    dqv$full <- as.data.frame(shiny::isolate(table_data[[id]]))
  } else if (shiny::is.reactive(table_data)) {
    shiny::observeEvent(table_data(), {
      if (no_update) {
        no_update <<- FALSE
      } else {
        dqv$full <- as.data.frame(table_data())
        if (!is.null(filters)) {
          update_filters(dqv$full[, columns, drop = FALSE], filters, session)
        }
      }
    }, ignoreInit = TRUE)
    dqv$full <- as.data.frame(shiny::isolate(table_data()))
  } else {
    dqv$full <- as.data.frame(table_data)
  }

  # define page_id which is needed for table rendering and reduce data to first page
  sorting <- check_sorting(sorting, to_sort, shiny::isolate(names(dqv$full)))

  # render filter row and add observer for filters
  output$filters <- shiny::renderUI({
    if (is.null(filters)) return()
    # add names(dq$full) dependency
    if (TRUE || is.null(names(dqv$full))) {
      # correct filters according to (new?) dataset
      filters <<- correct_filters(filters, shiny::isolate(dqv$full[, columns, drop = FALSE]))
    }
    filter_row(ns, dqv, filters, columns, sorting, reset)
  })

  # merge default table/cols parameters with given ones
  table_default <- list(readOnly = FALSE, stretchH = "all", contextMenu = FALSE)
  table_default <- append(table_param, table_default)
  table_default <- table_default[!duplicated(names(table_default))]

  cols_default <- list(colWidths = 1L, highlightCol = TRUE, dateFormat = "YYYY-MM-DD",
                       highlightRow = TRUE, manualColumnResize = TRUE)
  cols_default <- append(cols_param, cols_default)
  cols_default <- cols_default[!duplicated(names(cols_default))]

  params <- list(table_default, cols_default, col_param, cell_param)
  params[[1L]] <- add_scripts(params[[1L]], isTRUE(width_align),
                              isTRUE(horizontal_scroll))

  # render dq_handsontable
  app_output[[id]] <- rhandsontable::renderRHandsontable({
    if (is.null(hot())) return()
    params[[1L]]$data <- hot()
    params[[2L]]$hot <- do.call(rhandsontable::rhandsontable, params[[1L]])
    res <- do.call(rhandsontable::hot_cols, params[[2L]])
    for (x in params[[3L]]) {
      res <- do.call(rhandsontable::hot_col, append(list(res), x))
    }
    for (x in params[[4L]]) {
      x$row <- match(x$row, rownames(hot()))
      x$row <- x$row[!is.na(x$row)]
      res <- do.call(dq_hot_cell, append(list(res), x))
    }
    res$dependencies <- append(res$dependencies, init())
    res
  })

  # render paging row and add observer for inputs
  page_sizes <- sort(unique(c(page_size, 10L, 25L, 50L, 100L)))
  output$pages <- shiny::renderUI({
    if (paged) paging_row(ns, page_size[1L], page_sizes)
  })
  output$maxPages <- shiny::renderText({
    s <- as.integer(input$pageSize)
    paste("of ", ceiling(max(NROW(reduced()) / s, 1L)))
  })

  # add sort buttons
  if (to_sort) {
    sorts <- add_sorting_observer(
      input, session, dqv, page_size,
      shiny::isolate(names(dqv$full[, columns, drop = FALSE]))
    )
  }

  # add reset button
  if (reset) {
    shiny::observeEvent(input[["filter-reset"]], {
      for (n in grep("^filter", names(input), value = TRUE)) {
        shiny::updateTextInput(session, n, value = "")
        reset_slider_input(n)
      }
      if (to_sort) {
        dqv$sorting <- list(dir = "", col = "")
        lapply(sorts, function(n) update_icon_state_button(session, n, value = 1L))
      }
    })
  }


  shiny::isolate(dqv$full)
}

#' @author richard.kunze
add_scripts <- function(params, width, scroll) {
  if (width || scroll) {
    params$afterRender <- htmlwidgets::JS(
      "function() {",
      "  var hider = $(this.rootElement).find('.wtHider');",
      "  var $filter = $('#' + this.rootElement.id + '-filters');",
      ifelse(scroll, "  $filter.css('overflow', 'hidden');", ""),
      "  var row = $filter.find('.row');",
      "  row.width(hider.width());",
      if (width) paste(
        "  var els = $filter.find('.form-group');",
        "  for (var i = 0; i < els.length; i++) {",
        "    $(els[i]).outerWidth($(this.getCell(0, i)).outerWidth());",
        "  }", sep = "\n"
      ),
      "}"
    )
  }
  if (scroll) {
    params$afterScrollHorizontally <- htmlwidgets::JS(
      "function() {
        var $f = $('#' + this.rootElement.id + '-filters');
        $f.scrollLeft($(this.rootElement).find('.wtHolder').scrollLeft());
      }"
    )
  }
  params
}
