Sys.setenv(R_CONFIG_ACTIVE = "default")

devtools::load_all()

logger::log_threshold()

logger::logger

layout <- logger::layout_glue_generator(format = '{}')

logger::log_level(logger::INFO)
logger::log_level(logger::TRACE)

lvl <- logger::INFO

glue::glue("level detail: {attr(lvl, c('level'), exact = T)}:{as.integer(lvl)}")

?logger::layout_glue_colors

id <- uuid::UUIDgenerate()

control_layout <- layout_glue_generator(format = '{self$name()}/{$id}/{namespace}/{fn} {time} {level}: {msg}')

element <- shiny.gentelella::UIElement$new()

sinew::makeOxygen(element$initialize)

class_name <- "test"
id <- uuid::UUIDgenerate()

f <- function() {
  logger::log_info("test")
}

layout <- logger::layout_glue_generator('{class_name}/{id}/{namespace}{fn} {time} {level}: {msg}')

logger::log_layout(layout)

f()
