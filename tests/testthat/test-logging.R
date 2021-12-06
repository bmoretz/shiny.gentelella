TestElement <- R6::R6Class(
  classname = "TestElement",
  inherit = Element,

  public = list(),
  active = list(),
  private = list()
)

element <- TestElement$new()


