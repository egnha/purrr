#' Compose multiple functions
#'
#' @param ... n functions to apply in order from right to left.
#' @return A function
#' @export
#' @examples
#' not_null <- compose(`!`, is.null)
#' not_null(4)
#' not_null(NULL)
#'
#' add1 <- function(x) x + 1
#' compose(add1, add1)(8)
compose <- function(...) {
  fs <- lapply(dots_list(...), rlang::as_function)
  n <- length(fs)

  last <- as_closure(fs[[n]])
  `__call_last` <- function() {
    call_last <- mut_node_car(sys.call(-1), last)
    eval_bare(call_last, parent.frame(2))
  }
  `__rest` <- rev(fs[-n])

  set_attrs(
    `formals<-`(
      value = formals(last),
      function() {
        out <- `__call_last`()
        for (f in `__rest`)
          out <- f(out)
        out
      }
    ),
    class = "composite_function"
  )
}

#' @rdname compose
#' @param x Composite function, i.e., a composition of functions created by
#'   `compose()`.
#' @export
decompose <- function(x) {
  if (!inherits(x, "composite_function"))
    abort("Not a composite function")

  environment(x)$fs
}

#' @export
print.composite_function <- function(x, ...) {
  cat("<composite_function>\n")
  cat("(Listed in calling order)\n")

  for (f in rev(decompose(x))) {
    cat("\n")
    print(f)
  }

  invisible(x)
}
