#' @import ggplot2
#' @export
#'
teach_plot <- function(distribution, df = NULL, df1 = NULL, df2 = NULL, val_emp = NULL, val_crit = NULL) { # NULL for optional arguments

  distribution <- tolower(distribution) # making the function case-insensitive (so that people can also write "T" or "CHI" etc.)


  # input validation: ckecking whether dfs were provided ####
  # -> ensures that the function does not fail inside the dt/df/etc. functions -> makes the error more transparent to the user
  # also checking whether provided dfs are valid (i.e., a positive number)
  if (distribution == "t" || distribution == "chisq") {
    if (is.null(df)) stop("Argument 'df' is missing.") # if df1 was not provided
    if (!is.numeric(df) || df  <=0) stop("df must be a positive number") # if df1 is <= 0 or not a number
  }
  if (distribution == "f") {
    if (is.null(df1)) stop("Argument 'df1' is missing.") # if df1 is missing
    if (is.null(df2)) stop("Argument 'df2' is missing.") # if df2 is missing
    # i put df1 and df2 in separate if statements in case only df1 or only df2 are missing
    if (!is.numeric(df1) || df1 <=0) stop("df1 must be a positive number") # if df1 is <= 0 or not a number
    if (!is.numeric(df2) || df1 <=0) stop("df2 must be a positive number") # if df2 is <= 0 or not a number
  }

  # x axis values ####
  quantiles = seq(-4, 4, length.out = 1000)
  # adjusting upper x axis limit to critical / empirical value
  if (!is.null(val_emp) | !is.null(val_crit)) {
    quantiles = seq(-4, max(c(val_emp,val_crit))*1.2, length.out = 1000)
  }

  # creating a dispatch table  ####
  # (instead of many if statements; it's basically a dictionary that assigns keys (here: distribution names) to info, e.g., functions)
  ## then we can use these keys at a later point to retrieve all information associated with it and use it to drive behaviour of the function
  ## here we map the keys (distribution names) to a function (density function) and some plot metadata (x axis range)
  ## we can use this later for plotting
  dist_type <- list( # type of distribution
    z = list(density = function(x) dnorm(quantiles)),
    t = list(density = function(x) dt(quantiles, df)),
    f = list(density = function(x) df(quantiles, df1, df2)),
    chisq = list(density = function(x) dchisq(quantiles, df))
  )

  # error message if input to distribution argument is not z,t,f, or chi ####
  if (!distribution %in% names(dist_type)) {
    stop("Unsupported distribution. Use 'z', 't', 'f', or 'chisq'")
  }


  # creating title names ####
  # (is necessary as a separate step because of varying number of dfs)
  title <- switch(distribution, # switch tests an expression against elements of a list -> If the value evaluated from the expression matches an item from the list, the corresponding in the list value is returned.
                  z = "Standard Normal Distribution",
                  t = paste0("Density of t~(",df,") under the H0"),
                  chisq = paste0("Density of chisq~(",df,") under the H0"), # QUESTION: "chi" is not the correct name -> what shall we use instead
                  f = paste0("Density of F~(",df1, "," ,df2,") under the H0")
  )


  # creating plot data following the distributions ####
  data <- data.frame(x = quantiles, y = dist_type[[distribution]]$density(quantiles))


  # creating the plot ####
  plot <- ggplot(data, aes(x = x, y = y)) +
    geom_line() +
    ggtitle(title) +
    xlab("x") +
    ylab("density P(x)") +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))

  values <- c() # creating NULL object which will be appended in the if statement

  # adding empirical and critical value + legend
  if(!is.null(val_emp) | (!is.null(val_crit))) {
    if(!is.null(val_emp)) {
      plot <- plot + geom_vline(aes(xintercept = val_emp, color = "empirical"), linetype = "solid")
      values["empirical"] <- "coral"
    }
    if(!is.null(val_crit)) {
      plot <- plot + geom_vline(aes(xintercept = val_crit, color = "critical"), linetype = "solid")
      values["critical"] <- "turquoise"
    }
    plot <- plot + scale_colour_manual(values = values) +
      theme(legend.title = element_blank())
  }

  print(plot)
}
