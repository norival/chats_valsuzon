p_in_rectangle <- function(point, rectangle) {
  # point <- as.numeric(jah[1,])
  # point[2]
  # rectangle <- forest1
  # i <- 1

  area_tr <- numeric(4)
  for (i in 1:4) {
    if (i < 4) {
      tr <-
        data.frame(x = c(rectangle$x[i], rectangle$x[i+1], point[1]),
                   y = c(rectangle$y[i], rectangle$y[i+1], point[2]))
    } else {
      tr <-
        data.frame(x = c(rectangle$x[i], rectangle$x[i-3], point[1]),
                   y = c(rectangle$y[i], rectangle$y[i-3], point[2]))
    }

    area_tr[i] <-
      0.5*(abs(tr$x[1] * (tr$y[2] - tr$y[3]) +
               tr$x[2] * (tr$y[3] - tr$y[1]) +
               tr$x[3] * (tr$y[1] - tr$y[2])))
  }

  area_rect <-
    0.5*(abs((rectangle$y[1] - rectangle$y[3]) * (rectangle$x[4] - rectangle$x[2]) +
             (rectangle$y[2] - rectangle$y[4]) * (rectangle$x[1] - rectangle$x[3])))

  area_tr_sum <- sum(area_tr)

  if (area_tr_sum > area_rect) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

percent_in_forest <- function(the_cat, forest) {

  # the_cat <- jah
  # forest <- forest1
  in_forest <- logical(nrow(the_cat))

  for (i in 1:nrow(the_cat)) {
    point <- as.numeric(the_cat[i, ])

    in_forest[i] <- p_in_rectangle(point, forest)
  }

  percent <- length(in_forest[in_forest == TRUE]) / length(in_forest) * 100
  
  return(percent)
}
