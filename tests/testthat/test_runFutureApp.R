
app <- shinytest::ShinyDriver$new(system.file("shiny", package = "Future"))

app$snapshotInit("test_runFutureApp")
app$waitForShiny()

app$click("main_module-add_one")
app$waitForShiny()
Sys.sleep(1)
app$setValue(name = "main_module-product1", value = "Banan")
app$setValue(name = "main_module-weight1", value = 100)
app$click("main_module-click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,
  "Kalorycznosc posilku wynosi 74.3 kcal."
)

app$click("main_module-add_one")
app$waitForShiny()
app$setValue(name = "main_module-product2", value = "Mango")
app$setValue(name = "main_module-weight2", value = 100)
app$click("main_module-add_one")
app$waitForShiny()
app$setValue(name = "main_module-product3", value = "Awokado")
app$setValue(name = "main_module-weight3", value = 100)
app$click("main_module-add_one")
app$waitForShiny()
app$setValue(name = "main_module-product4", value = "Orzechy laskowe")
app$setValue(name = "main_module-weight4", value = 100)
app$click("main_module-add_one")
app$waitForShiny()
app$setValue(name = "main_module-product5", value = "Czekolada gorzka")
app$setValue(name = "main_module-weight5", value = 100)
app$waitForShiny()
Sys.sleep(1)
app$click("main_module-click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,

  "Kalorycznosc posilku wynosi 1514.4 kcal."
)

app$setInputs(`main_module-remove5` = "click")
app$setInputs(`main_module-remove4` = "click")
app$setInputs(`main_module-remove3` = "click")
app$setInputs(`main_module-remove2` = "click")
app$click("main_module-click")

app$snapshot()
all_values <- app$getAllValues()
testthat::expect_equal(
  all_values$output$`main_module-kcalMeal`,
  "Kalorycznosc posilku wynosi 74.3 kcal."
)


p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
