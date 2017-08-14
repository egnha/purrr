context("compose")

test_that("composed functions are applied right to left", {
  expect_identical(!is.null(4), compose(`!`, is.null)(4))

  set.seed(1)
  x <- sample(1:4, 100, replace = TRUE)
  expect_identical(unname(sort(table(x))), compose(unname, sort, table)(x))
})

test_that("formals of composition matches formals of first function called", {
  foo <- function(x, y, ..., z = "z") NULL
  bar <- function(...) NULL
  baz <- function(a) NULL
  expect_identical(formals(compose(baz, bar, foo)), formals(foo))
})

test_that("first called function is called in calling environment", {
  expect_identical(
    compose(identity, function() parent.frame())(),
    environment()
  )
})

test_that("function-formulas are indeed interpreted as functions", {
  cmp1 <- compose(identity, ~ . + 1)
  cmp2 <- compose(~ . + 1, identity)
  set.seed(1)
  for (x in runif(10))
    (x + 1) %>% expect_equal(cmp1(x)) %>% expect_equal(cmp2(x))
})

test_that("list of functions can be spliced", {
  fns <- list(log, abs, `+`)
  compose(log, abs, `+`) %>%
    expect_equal(compose(!!! fns)) %>%
    expect_equal(compose(UQS(fns)))
})

test_that("composite function can be decomposed", {
  f <- compose(identity, log, abs, `+`)
  expect_equivalent(decompose(f), list(identity, log, abs, `+`))
})

test_that("compose() inverts decompose() (upon splicing)", {
  f <- compose(identity, log, abs, `+`)
  expect_equal(compose(!!! decompose(f)), f)
})

test_that("nested compositions are unnested", {
  compose(log, abs, sin, `+`) %>%
    expect_equal(
      compose(log, abs, sin, compose(`+`))
    ) %>%
    expect_equal(
      compose(log, abs, compose(sin,  compose(`+`)))
    ) %>%
    expect_equal(
      compose(log, compose(abs, compose(sin,  compose(`+`))))
    ) %>%
    expect_equal(
      compose(compose(log, compose(abs, compose(sin,  compose(`+`)))))
    )
})
