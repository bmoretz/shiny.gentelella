test_that("layout_generates",{

  gen <- LogLayoutGenerator$new()

  gen$add_metric(crayon::bold, crayon::make_style("red1"),
                 metric = 'sysname')

  gen$add_metric(crayon::italic, crayon::make_style("darkorange"),
                 metric = 'release')

})
