
app <- shinytest::ShinyDriver$new(system.file("shiny", package = "Future"))

app$snapshotInit("test_runFutureApp_current")
Sys.sleep(1)

app$setInputs(`main_module-product01` = "Banan")
app$setInputs(`main_module-weight01` = 100)
app$setInputs(`main_module-click` = "click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,
  "Kalorycznosc posilku wynosi 74.3 kcal."
)

app$setInputs(`main_module-product02` = "Mango")
app$setInputs(`main_module-weight02` = 100)
app$setInputs(`main_module-add02` = "click")
app$setInputs(`main_module-product03` = "Awokado")
app$setInputs(`main_module-weight03` = 100)
app$setInputs(`main_module-add03` = "click")
app$setInputs(`main_module-product04` = "Orzechy laskowe")
app$setInputs(`main_module-weight04` = 100)
app$setInputs(`main_module-add04` = "click")
app$setInputs(`main_module-product05` = "Czekolada gorzka")
app$setInputs(`main_module-weight05` = 100)
app$setInputs(`main_module-click` = "click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,
  "Kalorycznosc posilku wynosi 1514.4 kcal."
)

app$setInputs(`main_module-remove05` = "click")
app$setInputs(`main_module-remove04` = "click")
app$setInputs(`main_module-remove03` = "click")
app$setInputs(`main_module-remove02` = "click")
app$setInputs(`main_module-click` = "click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,
  "Kalorycznosc posilku wynosi 74.3 kcal."
)


p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
