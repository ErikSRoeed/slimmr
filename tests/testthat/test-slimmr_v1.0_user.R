#---- SETUP --------------------------------------------------------------------
sample_script <- c(
  "initialize() {",
  "  initializeMutationRate(1e-8);",
  "  initializeMutationType('m1', 0.5, 'f', 0.0);",
  "  initializeGenomicElementType('g1', m1, 1.0);",
  "  initializeGenomicElement(g1, 0, 1000);",
  "  initializeRecombinationRate(1e-9);",
  "}",
  "",
  "1 early() {",
  "  sim.addSubpop('p1', 1000);",
  "}",
  "",
  "10000 late() { sim.outputFixedMutations(); }",
  ""
)

sample_temp_file <- tempfile(fileext = ".slim")
writeLines(sample_script, sample_temp_file)


#---- TESTS --------------------------------------------------------------------
test_that("script_import_completes", {
  expect_no_error(import_slim_model(script_path = sample_temp_file, name = ""))
})

test_that("script_import_returns_eidosmodel", {
  model <- import_slim_model(script_path = sample_temp_file, name = "")
  expect_true("EidosModel" %in% class(model))
}
)
