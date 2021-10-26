
app <- shinytest::ShinyDriver$new(system.file("shiny", package = "Future"))

app$snapshotInit("test_runFutureApp")
app$waitForShiny()

app$click("main_module-add_one")
app$waitForShiny()
app$setValue(name = "main_module-product1", "Banan", iotype = "input")
app$setValue(name = "main_module-weight1", 100, iotype = "input")
app$setInputs(`main_module-click` = "click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,
  "Kalorycznosc posilku wynosi 74.3 kcal."
)

app$setInputs(`main_module-add_one` = "click")
app$setInputs(`main_module-product2` = "Mango")
app$setInputs(`main_module-weight2` = 100)
app$setInputs(`main_module-add_one` = "click")
app$setInputs(`main_module-product3` = "Awokado")
app$setInputs(`main_module-weight3` = 100)
app$setInputs(`main_module-add_one` = "click")
app$setInputs(`main_module-product4` = "Orzechy laskowe")
app$setInputs(`main_module-weight4` = 100)
app$setInputs(`main_module-add_one` = "click")
app$setInputs(`main_module-product5` = "Czekolada gorzka")
app$setInputs(`main_module-weight5` = 100)
app$setInputs(`main_module-click` = "click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,
  "Kalorycznosc posilku wynosi 501.7 kcal."
)

app$setInputs(`main_module-remove5` = "click")
app$setInputs(`main_module-remove4` = "click")
app$setInputs(`main_module-remove3` = "click")
app$setInputs(`main_module-remove2` = "click")
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
