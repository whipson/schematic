clean_c_wrapper <- function(x) {
  sub("^c\\((.*)\\)$", "\\1", x)
}
