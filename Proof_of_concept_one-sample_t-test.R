# ------------ Prüfen und Visualisieren eines Einstichproben-t-Tests ----------- #

# Die folgende Funktionion demonstriert die grundlegende Arbeitsweise der Funktionen
# im Startistica-Package. Es handelt sich um eine funktionsfähige Vorversion, nicht
# um die Funktion in der finalen Form. Output und Code werden im Rahmen des Projekts
# erweitert und angepasst.

require(ggplot2)

start_onesample_t.test <- function(obj) {
  test_result <- obj
  
  if (test_result$method != "One Sample t-test") {             # Mögliche Fehlspezifikationen werden aufgegriffen
    stop("It appears you did not specify a one sample t-test. 
         Consider using start_twosample_t.test().")
  }
  
  x <- eval(parse(text = test_result$data.name))
  xmean <- test_result$estimate
  muv <- test_result$null.value
  alpha <- 1-attr(test_result$conf.int,"conf.level")
  df <- test_result$parameter
  tval <- test_result$statistic
  se <- sd(x,na.rm=T)/sqrt(length(x))
  
  dplot <- ggplot(data= data.frame(x), aes(x = "", y = x)) +    # Daten werden visualisiert
    geom_boxplot() +
    geom_jitter(width = 0.2, color = "black", alpha = 0.5, size = 2) +
    geom_hline(aes(yintercept = muv, color = "Population value"), linewidth = 1.2) +
    geom_hline(aes(yintercept = xmean, color = "Sample mean"), linewidth = 1.2) +
    scale_colour_manual(values = c("red", "blue")) +
    labs(y = "Value", x = "Boxplot with Dots") +
    theme_minimal()
  
  # Plot values
  lowerlimit <- -abs(tval)-4
  upperlimit <- abs(tval)+4
  xaxis <- seq(lowerlimit, upperlimit, length.out = 1000)
  density <- dt(xaxis, df)
  data <- data.frame(x = xaxis, density = density)
  
  
  if (test_result$alternative == "greater") {                   # Verteilungsfunktion unter der H0 wird visualisiert
    tkrit <- qt(1-alpha,df)                                     # Diese Visualisierungen sind auf die Testspezifikationen abgestimmt
    
    gplot <- ggplot(data, aes(x = xaxis, y = density)) +
      geom_line() +
      geom_vline(aes(xintercept = tval, color = "empirical value"), linetype = "solid") +
      geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
      scale_colour_manual(values = c("red", "blue")) +
      geom_area(data = subset(data, x >= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5) +
      ggtitle(paste("Density of t-distribution with under the H0")) +
      xlab("x") +
      ylab("Density") +
      theme_minimal()
  }
  
  if (test_result$alternative == "less") {
    tkrit <- qt(alpha,df)
    
    gplot <- ggplot(data, aes(x = xaxis, y = density)) +
      geom_line() +
      geom_vline(aes(xintercept = tval, color = "empirical value"), linetype = "solid") +
      geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
      scale_colour_manual(values = c("red", "blue")) +
      geom_area(data = subset(data, x <= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5) +
      ggtitle(paste("Density of t-distribution under the H0")) +
      xlab("x") +
      ylab("Density") +
      theme_minimal()
  }
  
  if (test_result$alternative == "two.sided") {
    tkrit <- qt(1-alpha/2,df)
    tkritb <- qt(alpha/2,df)
    
    gplot <- ggplot(data, aes(x = xaxis, y = density)) +
      geom_line() +
      geom_vline(aes(xintercept = tval, color = "empirical value"), linetype = "solid") +
      geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
      geom_vline(aes(xintercept = tkritb, color = "critical value"), linetype = "solid") +
      scale_colour_manual(values = c("red", "blue")) +
      geom_area(data = subset(data, x >= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5) +
      geom_area(data = subset(data, x <= tkritb), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5) +
      ggtitle(paste("Density of t-distribution under the H0")) +
      xlab("x") +
      ylab("Density") +
      theme_minimal()
  }
  
    print(dplot)
    print(gplot)
    
    cat("Note: Two plots have been created. Use the arrows to navigate.\n",
        "                  \n",
        sep = "")
    
    print(test_result)
  
    cat("Explanation of output:\n",                                # Die Testspezifikationen werden aufgelistet und
        "----------------\n",                                      # Hintergrundinformationen werden gegeben.
        "The sample size is: n = ", length(x), "\n",               # In der finalen Version werden die Textbausteine anschaulich formatiert
        "The degrees of freedom are computed as: n-1 = ",df, "\n",
        "The sample standard deviation is: sd = ", sd(x,na.rm=T), "\n",
        "The estimated standard error is: s.e. = ", sd(x,na.rm=T),"/sqrt(",length(x),") = ",se, "\n",
        "The estimate is the sample mean: ",xmean, "\n",
        "The empirical t-values is computed as: t_emp = (",xmean,"-",muv,")/",se, "\n",
        "The effect size Cohen's d (not shown in the output) is computed as: d = (",xmean,"-",muv,")/",sd(x,na.rm=T), "\n",
        "                  \n",
        sep = "")
    
    if (test_result$alternative == "greater") {                     # Informationen sind auf die Testspezifikationen abgestimmt
      cat("Information specific to the one-sided (right tailed) test:\n",
          "----------------\n",
          "The critical value is: t_krit = qt(1-",alpha,", ",df,") = ",tkrit, "\n",
          "The lower limit of the confidence interval of the mean is: LL = ",xmean," - qt(1-",alpha,", ", df, ")*",se," = ",test_result$conf.int[[1]], "\n",
          "The upper limit of the confidence interval of the mean is infinity due to the one-sidedness. \n",
          "The p-value is computed as: 1-pt(",tval,", ",df, ") \n",
          "                  \n",
          sep = "")
    }
    
    if (test_result$alternative == "less") {
      cat("Information specific to the one-sided (left tailed) test:\n",
          "----------------\n",
          "The critical value is: t_krit = qt(",alpha,", ",df,") = ",tkrit, "\n",
          "The lower limit of the confidence interval of the mean is -infinity due to the one-sidedness. \n",
          "The upper limit of the confidence interval of the mean is: UL = ",xmean," + qt(1-",alpha,", ", df, ")*",se," = ",test_result$conf.int[[2]],  "\n",
          "The p-value is computed as: pt(",tval,", ",df, ") \n",
          "                  \n",
          sep = "")
    }
    
    if (test_result$alternative == "two.sided") {
    cat("Information specific to the two-sided test:\n",
        "----------------\n",
        "The positive critical value is: t_krit = qt(1-",alpha,"/2, ",df,") = ",tkrit, "\n",
        "The negative critical value is: t_krit = qt(",alpha,"/2, ",df,") = ",tkritb, "\n",        
        "The lower limit of the confidence interval of the mean is: LL = ",xmean," - qt(1-",alpha,"/2, ", df, ")*",se," = ",test_result$conf.int[[1]], "\n",
        "The upper limit of the confidence interval of the mean is: UL = ",xmean," + qt(1-",alpha,"/2, ", df, ")*",se," = ",test_result$conf.int[[2]], "\n",
        "The p-value is computed as: 2*(1-pt(",abs(tval),", ",df, ")) \n",
        "                  \n",
        sep = "")
    }
}


data <- rnorm(100, 1, 1)                                       # Das Testobjekt kann auf alle gängigen Arten spezifiziert werden
ver1 <- t.test(data,alternative = "two.sided", mu = 1)         # Es werden die gängigen Datentypen unterstützt.
start_onesample_t.test(ver1)

data <- data.frame(pred = rnorm(100, 2, 2))
ver2 <- t.test(data$pred, alternative = "two.sided",mu = 1)
start_onesample_t.test(ver2)