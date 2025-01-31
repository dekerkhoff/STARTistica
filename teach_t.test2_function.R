
library(ggplot2)
library(broom)
library(gridExtra)


#### ------------- Function ---------------- ####

teach_t.test1 <- function(testobj, export = FALSE, ...) {
  test_result <- testobj
  args <- list(...)
  data_name <- testobj$data.name
  mu0 <- testobj$null.value[[1]]
  groups <- names(testobj$estimate)
  group1 <- gsub("mean in group ", "", groups[1])
  group2 <- gsub("mean in group ", "", groups[2])
  dframes <- strsplit(data_name, " by | and ")[[1]] # List variable names
  
  # --------------------------- #    
  
  checkform <- tryCatch(eval(parse(text = dframes[1])),
                        error = function(e) NULL)
  
  checkdata <- is.null(args$data)
  
  if (!is.null(checkform)) {
    df1 <- eval(parse(text = dframes[1]))
    df2 <- eval(parse(text = dframes[2]))
  }
  else if (is.null(checkform) & checkdata == F) {
    
    df1 <- args$data[names(args$data) %in% dframes[1]] # collect only values
    df2 <- args$data[names(args$data) %in% dframes[2]] # collect only groups
    
  }
  else{
    stop("Please provide a data frame using testfunc(testobj, data = ...)")
  }
  # --------------------------- #         
  
  
  if (grepl("by", data_name, fixed = T)) {
    df_combined <- data.frame(df1, df2)
    names(df_combined) <- c("value", "group")
  } else {
    df1 <- data.frame(df1, group = "Group 1")
    df2 <- data.frame(df2, group = "Group 2")
    names(df1) <- c("value", "group")
    names(df2) <- c("value", "group")
    
    df_combined <- rbind(df1, df2)
  }
  #return(df_combined)
  n1 <- by(df_combined, df_combined$group, nrow)[[1]] # use only n1 for paired
  n2 <- by(df_combined, df_combined$group, nrow)[[2]]
  var1 <- by(df_combined$value, df_combined$group, var)[[1]]
  var2 <- by(df_combined$value, df_combined$group, var)[[2]]
  muv <- test_result$null.value
  alpha <- 1-attr(test_result$conf.int,"conf.level")
  tval <- test_result$statistic
  pval <- test_result$p.value
  
  separator <- c("----------------------\n")
  
  maintext_top <- c(
    "Explanation of output:\n")
  
  if (length(test_result$estimate) == 2) {
    xmean1 <- test_result$estimate[[1]]
    xmean2 <- test_result$estimate[[2]]
    xmean <- xmean1 - xmean2
    df <- test_result$parameter # anpassen im Text: uncorrected
    df_raw <- n1+n2-2
    sigmasq <- ((n1-1)*var1+(n2-1)*var2)/((n1-1)+(n2-1))
    popsd <- round(sqrt(sigmasq),3)
    se <- sqrt(sigmasq*((1/n1)+(1/n2)))
    
    Indicator <- c("Degrees of freedom",
                   "Variance of group 1",
                   "Variance of group 2",
                   "Pooled within-variance",
                   "Estimated standard error",
                   "Empirical t-value",
                   "Effect size Cohen's d")
    
    Calculation <- c(paste0("| df = ",n1," + ",n2," -2"),
                     "| variance of measurements in group 1",
                     "| variance of measurements in group 2",
                     paste0("| var_pooled = ((",n1,"-1)*",round(var1,3),"+(",n2,"-1)*",round(var2,3),")/(",n1,"-1+",n2,"-1)"),
                     paste0("| s.e. = sqrt(",round(sigmasq,3),"*((1/",n1,")+(1/",n2,")))"),
                     paste0("| t_emp = (",round(xmean1,3),"-",round(xmean2,3),")/",round(se,3)),
                     paste0("| d = (",round(xmean1,3),"-",round(xmean2,3),")/sqrt(",round(sigmasq,3), ")")
    )
    
    Value <- c(round(df_raw,3), 
               round(var1,3), 
               round(var2,3),
               round(sigmasq,3), 
               round(se,3),
               round(tval,3), 
               round((xmean1-xmean2)/sqrt(sigmasq),3) )
    
    maintab <- data.frame(Indicator, Calculation, Value)
    
    
    maintext_bottom <- c(
      "Notes:\n",
      "- If homogeneity of variances is not assumed, the df shown in the output are adjusted\n",
      "- The estimate is the difference of group means\n",
      "- The effect size Cohen's d is not shown in the output\n",
      paste0("- The sample sizes are n1 = ",n1," and n2 = ",n2,"\n"),
      "\n")
    
  } else {
    xmean <- test_result$estimate
    df <- test_result$parameter
    firstlevel <- names(table(df_combined$group))[1]
    secondlevel <- names(table(df_combined$group))[2]
    diffvar <- df_combined[df_combined$group == firstlevel,"value"] -
      df_combined[df_combined$group == secondlevel,"value"]
    sigmasq <- var(diffvar)
    popsd <- round(sqrt(sigmasq),3)
    se <- sqrt(sigmasq)/sqrt(n1) # since n1 and n2 are the same, use n1
    
    
    maintext_bottom <- c(
      "Notes:\n",
      "- The estimate is the mean of the group differences\n",
      "- The effect size Cohen's d is not shown in the output\n",
      paste0("- The sample sizes are ",n1," observations per group\n"),
      "\n")
    
    Indicator <- c("Degrees of freedom",
                   "Variance in group differences",
                   "Estimated standard error",
                   "Empirical t-value",
                   "Effect size Cohen's d")
    
    Calculation <- c(paste0("| df = ",n1,"-1"),
                     "| variance of difference scores",
                     paste0("| s.e. = sqrt(",round(sigmasq,3),"/sqrt(",n1,")"),
                     paste0("| t_emp = (",round(xmean,3),"-",muv,")/",round(se,3)),
                     paste0("| d = (",round(xmean,3),")/sqrt(",round(sigmasq,3),")")
    )
    
    Value <- c(round(df,3), 
               round(sigmasq,3), 
               round(se,3), 
               round(tval,3), 
               round(xmean/sqrt(sigmasq),3) )
    
    maintab <- data.frame(Indicator, Calculation, Value)
    
  }
  
  # Plot values for null-distribution
  lowerlimit <- -abs(tval)-4
  upperlimit <- abs(tval)+4
  xaxis <- seq(lowerlimit, upperlimit, length.out = 1000)
  density <- dt(xaxis, df)
  data <- data.frame(x = xaxis, density = density)
  
  gplot <- ggplot(data, aes(x = xaxis, y = density)) +
    geom_line() +
    geom_vline(aes(xintercept = tval, color = "empirical value"), linetype = "solid") +
    ggtitle(paste0("Density of t~(",round(df,3),") under the H0")) +
    xlab("x") +
    ylab("density P(x)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Plot values for confidence interval
  plotLL <- mu0 - 4*popsd
  plotUL <- mu0 + 4*popsd
  xmu <- seq(plotLL, plotUL, length.out = 1000)
  CIdensity <- dnorm(xmu, mean = mu0, sd = popsd)
  data2 <- data.frame(x = xmu, density = CIdensity)
  
  ciplot <- ggplot(data2, aes(x = xmu, y = CIdensity)) +
    geom_line() +
    scale_x_continuous(breaks = c(round(plotLL,1),
                                  round(mu0-3*popsd,1),
                                  round(mu0-2*popsd,1),
                                  round(mu0-1*popsd,1),
                                  mu0,
                                  round(mu0+1*popsd,1),
                                  round(mu0+2*popsd,1),
                                  round(mu0+3*popsd,1),
                                  round(plotUL,1))) +
    labs(title = "Illustration of the confidence interval", x = expression(mu ["0"]),
         y = "Assumed density P(x)",
         caption = paste0("The illustration assumes that 
                        X~N(",mu0,",",round(sigmasq,3),") in the population"))
  
  if (test_result$alternative == "greater") {
    tkrit <- round(qt(1-alpha,df),3)
    LL <- round(test_result$conf.int[[1]],3)
    
    extratext_top <- c(
      "Information specific to the one-sided (right tailed) test:\n")
    
    xIndicator <- c("Critical value",
                    "Confidence interval lower limit",
                    "p-value")
    
    xCalculation <- c(paste0("| t_krit = qt(1-",alpha,", ",round(df,3),")"),
                      paste0("| LL = ",round(xmean,3)," - qt(1-",alpha,", ", round(df,3), ")*",round(se,3) ),
                      paste0("| p = 1-pt(",round(tval,3),", ",round(df,3), ")")
    )
    
    xValue <- c(round(tkrit,3),
                LL,
                round(pval,3))
    
    extratab <- data.frame(Indicator = xIndicator, 
                           Calculation = xCalculation, 
                           Value = xValue)
    
    
    extratext_bottom <- c("Notes:\n",
                          "The upper limit of the confidence interval is infinity due to the one-sidedness")
    
    
    gplot <- gplot + 
      geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
      scale_colour_manual(values = c("red", "blue")) +
      labs(colour = "key t-values") +
      geom_area(data = subset(data, x >= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5)
    
    ciplot <- ciplot +
      geom_segment(data = data2[1,], aes(x = LL, y = 0.01, xend = plotUL, yend = 0.01), 
                   linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  if (test_result$alternative == "less") {
    tkrit <- round(qt(alpha,df),3)
    UL <- round(test_result$conf.int[[2]],3)
    
    extratext_top <- c(
      "Information specific to the one-sided (left tailed) test:\n")
    
    
    
    xIndicator <- c("Critical value",
                    "Confidence interval upper limit",
                    "p-value")
    
    xCalculation <- c(paste0("| t_krit = qt(",alpha,", ",round(df,3),")"),
                      paste0("| UL = ",round(xmean,3)," + qt(1-",alpha,", ", round(df,3), ")*",round(se,3)),
                      paste0("| p = pt(",round(tval,3),", ",round(df,3), ")")
    )
    
    xValue <- c(round(tkrit,3),
                UL,
                round(pval,3))
    
    extratab <- data.frame(Indicator = xIndicator, 
                           Calculation = xCalculation, 
                           Value = xValue)
    
    extratext_bottom <- c("Notes:\n",
                          "The lower limit of the confidence interval is infinity due to the one-sidedness")
    
    
    gplot <- gplot + 
      geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
      scale_colour_manual(values = c("red", "blue")) +
      labs(colour = "key t-values") +
      geom_area(data = subset(data, x <= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5)
    
    ciplot <- ciplot +
      geom_segment(data = data2[1,], aes(x = plotLL, y = 0.01, xend = UL, yend = 0.01), 
                   linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  if (test_result$alternative == "two.sided") {
    tkrit <- round(qt(1-alpha/2,df),3)
    tkritb <- round(qt(alpha/2,df),3)
    LL <- round(test_result$conf.int[[1]],3)
    UL <- round(test_result$conf.int[[2]],3)
    
    extratext_top <- c(
      "Information specific to the two-sided test:\n")
    
    
    xIndicator <- c("Positive critical value",
                    "Negative critical value",
                    "Confidence interval lower limit",
                    "Confidence interval upper limit",
                    "p-value")
    
    xCalculation <- c(paste0("| t_krit = qt(1-",alpha,"/2, ",round(df,3),")"),
                      paste0("| t_krit = qt(",  alpha,"/2, ",round(df,3),")"),
                      paste0("| LL = ",round(xmean,3)," - qt(1-",alpha,"/2, ", round(df,3), ")*",round(se,3)),
                      paste0("| UL = ",round(xmean,3)," + qt(1-",alpha,"/2, ", round(df,3), ")*",round(se,3)),
                      paste0("| p = 2*(1-pt(",round(abs(tval),3),", ",round(df,3), "))")
    )
    
    xValue <- c(round(tkrit,3),
                round(tkritb,3),
                LL,
                UL,
                round(pval,3))
    
    
    extratab <- data.frame(Indicator = xIndicator, 
                           Calculation = xCalculation, 
                           Value = xValue)
    
    extratext_bottom <- c("Notes:\n",
                          "In the center of the confidence interval is the estimate.")
    
    gplot <- gplot + 
      geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
      geom_vline(aes(xintercept = tkritb, color = "critical value"), linetype = "solid") +
      scale_colour_manual(values = c("red", "blue")) +
      labs(colour = "key t-values") +
      geom_area(data = subset(data, x >= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5) +
      geom_area(data = subset(data, x <= tkritb), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5)
    
    ciplot <- ciplot +
      geom_segment(data = data2[1,], aes(x = LL, y = 0.01, xend = UL, yend = 0.01), 
                   linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  plot <- ggplot(df_combined, aes(x = group, y = value, fill = group)) +
    geom_boxplot() +
    labs(title = "Distribution of values", x = "Group", y = "Value") +
    scale_fill_manual(values = c(rgb(140, 50, 80, maxColorValue = 250), 
                                 rgb(242,242,242, maxColorValue = 250))) +
    theme_minimal() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5))
  
  
  
  if (export) {
    pdf("overview.pdf", width = 8.3, height = 11.7) # DIN A4 size
    
    testtab <- broom::tidy(test_result)
    numcols <- sapply(testtab, class) == "numeric"
    testtab[numcols] <- round(testtab[numcols],3)
    
    grid.arrange(
      arrangeGrob(plot, ciplot, ncol = 2),
      arrangeGrob(gplot, ncol = 1),
      tableGrob(testtab),
      tableGrob(maintab),
      tableGrob(extratab),
      nrow = 5
    )
    
    dev.off()
    message("PDF export completed.")
  }
  
  print(plot) 
  print(gplot)
  print(ciplot)
  
  cat("Note: Three plots have been created. Use the arrows to navigate.\n",
      "                  \n",
      sep = "")
  
  print(test_result)
  
  cat(separator)
  cat(maintext_top, sep = "")
  cat(separator)  
  print(maintab, right = F)
  cat(separator)  
  cat(maintext_bottom, sep = "")
  cat(separator)  
  cat(extratext_top, sep = "")
  cat(separator)  
  print(extratab, right = F)
  cat(separator) 
  cat(extratext_bottom, sep = "")
  
}
