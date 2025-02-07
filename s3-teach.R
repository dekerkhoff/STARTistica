## S3 generic function (returning teach objects)
teach <- function(object, ...) {
  UseMethod("teach")
}

## S3 print method for teach objects
print.teach <- function(x, ...) {
  ## print original object
  print(x$object)
  
  ## header for explanations
  cat(c(
    "Explanation of output",
    "---------------------"
  ), sep = "\n")
  
  ## explanations set up by teach() method
  cat(x$print, sep = "\n")
  
  ## return teach() object invvisibly
  invisible(x)
}

## S3 plot method for teach objects
plot.teach <- function(x, ...) {
  ## just call dedicated function set up by teach() method
  x$plot(...)
}

## S3 teach method for htest objects (really currently 1-sample t-test only)

htest_method <- function(object) {
  ## only applicable to htest objects
  if (!inherits(object, "htest")) stop("htest_method() should be applied to 'htest' objects only")

  ## extract method
  m <- object$method

  ## lower case and with underscores
  m <- tolower(make.names(m))
  m <- gsub(".", "_", m, fixed = TRUE)
  
  ## prefix with htest and return
  m <- paste("htest", m, sep = "_")
  return(m)
}

## htest method that dispatches to separate metho
teach.htest <- function(object, ...) {
  ## pretend htest class is actually its htest_method and then dispatch
  .Class <- htest_method(object)
  NextMethod()
}

## htest method for "One Sample t-test"
teach.htest_one_sample_t_test <- function(object, ...) {

  ## extract quantities of interest
  ## TODO: Return these as part of the teach object?
  x <- eval(parse(text = object$data.name)) ## FIXME: will fail if formula interface is used
  xmean <- object$estimate
  muv <- object$null.value
  alpha <- 1 - attr(object$conf.int, "conf.level")
  df <- object$parameter
  tval <- object$statistic
  nobs <- length(x)
  sdev <- sd(x, na.rm = TRUE)
  serr <- sdev/sqrt(nobs)

  ## collect explanations
  teach_print <- c(
    sprintf("The sample size is: n = %i", nobs),
    sprintf("The degrees of freedom are computed as: n-1 = %i", df),
    sprintf("The sample standard deviation is: sd = %.3f", sdev),
    sprintf("The estimated standard error is: s.e. = %.3f/sqrt(%i) = %.3f", sdev, nobs, serr),
    sprintf("The estimate is the sample mean: %.3f", xmean),
    sprintf("The empirical t-values is computed as: t_emp = (%.3f - %.3f)/%.3f", xmean, muv, serr),
    sprintf("The effect size Cohen's d (not shown in the output) is computed as:\nd = (%.3f - %.3f)/%.3f", xmean, muv, sdev)
  )

  ## plot function (taking many variables from lexical scope)
  teach_plot <- function(width = 0.2, color = palette.colors(), alpha = 0.5, size = 2, linewidth = 1.2, ...) {
    stopifnot(requireNamespace("ggplot2"))
    ggplot2::ggplot(data = data.frame(x), ggplot2::aes(x = "", y = x)) +
      ggplot2::geom_boxplot(...) +
      ggplot2::geom_jitter(width = width, color = color[1], alpha = alpha, size = size) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = muv, color = "Population value"), linewidth = linewidth) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = xmean, color = "Sample mean"), linewidth = linewidth) +
      ggplot2::scale_colour_manual(values = color[2:3]) +
      ggplot2::labs(y = "Value", x = "Boxplot with Dots") +
      ggplot2::theme_minimal()
  }

  ## collect everything, add class, and return
  teach_list <- list(
    object = object,
    print = teach_print,
    plot = teach_plot
  )
  class(teach_list) <- "teach"
  return(teach_list)
}

## small demo code
if(FALSE) {

set.seed(1)
x <- rnorm(100, 1, 1)
x_test <- t.test(x)
x_teach <- teach(x_test)
x_teach
plot(x_teach)

}
