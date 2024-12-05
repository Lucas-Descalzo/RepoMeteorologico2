test_that("la conversion de celsius a kelvin funciona", {
  expect_equal(object = celsius_to_kelvin(5), expected = 278.15)
})

test_that("la temperatura no es numerica", {
  expect_error(object = celsius_to_kelvin("5"), regexp = "La temperatura debe ser numerica.")
})
