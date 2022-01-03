#' @title Table
#'
#' @description
#' R6 Class representing a Datatable control.
#'
#' @details
#' An object that is designed to encapsulate
#' most of the standard plumbing in setting up
#' a Datatable in the UI. The html template
#'
#' @importFrom DT dataTableOutput
#' @export
Table <- R6::R6Class(
  classname = "PortalTable",
  inherit = Control,

  public = list(
    #' @field title table title
    title = NULL,

    #' @field export_btn export btn
    export_btn = NULL,

    #' @description initialize an instance of a
    #' portal table.
    #' @param title title of the table
    initialize = function(title) {

      private$id <- uuid::UUIDgenerate()

      stopifnot(is.character(title), length(title) == 1)

      self$title <- title
      self$export_btn <- PortalTableExport$new(private$id)
    },

    #' @description function to customize the table subtitle.
    subtitle = function() {},

    #' @description render the ui for the a portal styled data table.
    ui = function() {

      ns <- NS(private$id)

      tags$div(class="card card-custom gutter-b",
               tagList(
                 tags$div(class="card-header border-0 py-5",
                          tagList(
                            tags$h3(class="card-title align-items-start flex-column",
                                    tagList(
                                      tags$span(class="card-label font-weight-bolder", self$title),
                                      self$subtitle()
                                    )),
                            self$export_btn$ui()
                          )),
                 tags$div(class="card-body py-0",
                          tagList(
                            self$ui_extra(),
                            tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {color: #ffffff !important; background-color: #2B9263 !important;}')),
                            DT::dataTableOutput(outputId = ns("table")))
                 ),
                 tags$div(class="card-footer",
                          self$ui_footer())
               ))
    },

    #' @description pure virtual method for customizing the table UI.
    ui_extra = function() { },

    #' @description pure virtual method for customizing the table UI.
    ui_footer = function() { }
  ),
  private = list(
    id = NULL
  )
)
