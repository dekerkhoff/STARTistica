#' @import dplyr
#' @import ggplot2
#' @import broom
#' @importFrom gridExtra grid.arrange arrangeGrob tableGrob
#' @import bazar
#' @import randomcoloR
#' @import ggExtra
#' @importFrom car leveneTest

## S3 generic function (returning teach objects)
#' @export

teach <- function(object, ...) {
  UseMethod("teach")
} # sucht nach method teach.class (?), FRAGE: hast du jetzt eine neue Klasse "teach" definiert (bzw. kommt das späer noch im Code?)
# -> später im Skript wird die Klasse erstellt

## S3 print method for teach objects
#' @export

print.teach <- function(x, ...) {
  ## print original object
  print(x$object)

  ## header for explanations
  cat(c(                     # cat = concatenate and print: printet Text in die Konsole (ohne Linienumbrüche)
    "Explanation of output",   # s. Hilfe: "cat is useful for producing output in user-defined functions. It converts its arguments to character vectors, concatenates them to a single character vector, appends the given sep = string(s) to each element and then outputs them.
    "---------------------"
  ), sep = "\n") # unter dem print-Output stehen jetzt die character strings untereinander

  ## explanations set up by teach() method
  cat(x$print, sep = "\n") # FRAGE: Kommen die Erklärungen dann "automatisch" von der teach-method?)

  ## return teach() object invisibly
  invisible(x) # invisibly returns a copy of the object so that the object is not printed twice + it makes the output available for assignment (d.h. ich kann den Output in einem Objekt speichern)
} # FRAGE: habe ich invisible richtig verstanden?



## S3 plot method for teach objects
#' @export

plot.teach <- function(x, ...) {
  ## just call dedicated function set up by teach() method
  x$plot(...)
} # FRAGE: habe ich das richtig verstanden, dass das einfach den Plot aufruft, der in dem teach-Objekt gespeichert ist und später im Code erstellt wird?



## S3 teach method for htest objects (really currently 1-sample t-test only)
htest_method <- function(object) { # FRAGE: warum ist da jetzt ein Unterstrich?
  ## only applicable to htest objects
  if (!inherits(object, "htest")) stop("htest_method() should be applied to 'htest' objects only")
  # Fragt ab, ob das Objekt die Klasse htest hat bzw. ob es es eine Klasse hat, die aus htest hervorgeht -> wenn nicht, dann wird die Ausführung der Funktion beendet

  ## extract method
  m <- object$method # die method ist im Objekt gespeichert

  ## lower case and with underscores
  m <- tolower(make.names(m)) # make.names: Make syntactically valid names out of character vectors (ensures that m is a valid name in r); tolower: alles Kleinbuchstaben
  m <- gsub(".", "_", m, fixed = TRUE) # gsub = global substitution: ersetzt hier alle Punkte in m durch underscore; fixed = TRUE: ensures that m is a string so that the period is treated as a literal character (not a regular expression)

  ## prefix with htest and return
  m <- paste("htest", m, sep = "_") # concatenates the string "htest" with the modified m, separating them with an underscore (_)
  return(m) # FRAGE: warum brauchen wir hier return?
}


## htest method that dispatches to separate method
#' @export

teach.htest <- function(object, ...) {  # FRAGE: warum ist es hier method.class (teach.class) und oben method.teach(print.teach)? -> sowohl die metod als auch class heißt "teach"
  ## pretend htest class is actually its htest_method and then dispatch
  .Class <- htest_method(object) # soll die Klasse (htest) temporär anders behandeln und zwar als das, was bei htest_method rauskommt -> d.h. teach.htest gibt weiter an teach.htest_method
  NextMethod()
}
# so this is what our generic function calls when passed to an htest-object, AM I RIGHT?

# htest method for all t-tests
#' @export
teach_test <- function(testobj, export = FALSE, ...) { # change function name i guess

  # same for all tests
  stopifnot(requireNamespace("ggplot2"))
  obj_name <- deparse(substitute(testobj)) # substitute returns the expression (testobj) without evaluating it (-> returns testobj instead of the output), deparse turns it into character string
  args <- list(...)
  data_name <- testobj$data.name
  method <- capture.output(testobj) # returns output as character string
  method <- paste0(method[1], method[2], method[3]) # concatenate lines 1, 2  and 3 to deal with the methods being in different lines in t-test and Anova
  mu0 <- testobj$null.value[[1]] # population mean

  indiv_values <- c("|------------------------------------------|",
                    "|-------- Obtain individual values:--------|",
                    "|------------------------------------------|")

  # decide which method to use
  if (grepl("One", method, fixed = TRUE)) {
    df_combined <- eval(parse(text = data_name)) # TODO: das müsste runter zum else if Block

    if (anyNA(df_combined) == TRUE) {  # exclude missing values
      missings <- TRUE
      df_combined <- na.omit(df_combined)
    }
    else missings <- FALSE

    # extract values #
    df_combined <- data.frame("x" = df_combined) # Variable heißt x; TODO s.o.
    n <- nrow(df_combined) # Stichprobengröße
    xmean <- testobj$estimate[[1]] # sample mean
    alpha <- 1-attr(testobj$conf.int,"conf.level")
    df <- testobj$parameter[[1]]
    tval <- testobj$statistic[[1]]
    sigmasq <- var(df_combined$x) # NOTE: muss man im Output anmerken, wie viele obs ausgeschlossen wurden?
    popsd <- round(sqrt(sigmasq),3)
    se <- sd(df_combined$x)/sqrt(n)


    vars <- c("n", "xmean", "variance", "df", "t_emp", "se",
              "h0", "alpha")
    vals <- c(n,
              paste0(obj_name, '[["estimate"]][["mean of x"]]'), # paste0 converts its arguments to character strings and concatenates them; brauchen wir später für den Output
              sigmasq,
              paste0(obj_name, '[["parameter"]]'),
              paste0(obj_name, '[["statistic"]]'),
              paste0(obj_name, '[["stderr"]]'),
              paste0(obj_name, '[["null.value"]]'),
              alpha)

    comments <- c(
      "sample size",
      "sample mean",
      "sample variance",
      "degrees of freedom",
      "empirical value",
      "standard error",
      "null value",
      "alpha-level"
    )

    # Combine into data frame
    comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE)

    # Format and print
    components <- with(comps, {
      var_fmt <- format(vars, width = max(nchar(vars)), justify = "left") # mit with kann direkt auf die Variablen im Datensatz ohne das Dollarzeichen zugreifen (es passiert alles innerhalb des comps data frames, ohne das jedes Mal dazusagen zu müssen)
      val_fmt <- format(vals, width = max(nchar(vals)), justify = "left") # width = max(nchar(vars)) = die Weite für die Auflistung der Parameter soll so groß sein wie die maximale Anzahl an characters in vars
      comment_fmt <- paste0("# ", comments)

      paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
    }) # das ist das, was später im Output steht

    Notes <- c(
      "",
      "Supplementary Information:",
      "- The estimate is the sample mean",
      "- The effect size Cohen's d is not shown in the output")

    Indicator <- c("Degrees of freedom",
                   "Estimated standard error",
                   "Empirical t-value",
                   "Effect size Cohen's d")

    Calculation <- c(paste0("df = n-1"),
                     paste0("se = sqrt(variance)/sqrt(n)"),
                     paste0("t_emp = (xmean - h0)/se"),
                     paste0("d = (xmean - h0)/sqrt(variance)") # für calculations tabelle
    )

    maintab <- data.frame(Indicator, Calculation)


    # Plot values
    lowerlimit <- -abs(tval)-4 # obere Grenze der x-Achse
    upperlimit <- abs(tval)+4 # untere Grenze der x-Achse; das mit abs ensures dass Verteilung symmetrisch gezeigt wird (also z.B. von -11 bis 11 statt von -11 bis 3)
    xaxis <- seq(lowerlimit, upperlimit, length.out = 1000)  # generates regular sequence from lower to upper limit with length of 1000 (with high sequence, the distribution in the plot is smoother)
    density <- dt(xaxis, df) # Dichte für jeden Wert auf der x-Achse
    data <- data.frame(x = xaxis, density = density)

    gplot <- ggplot(data, aes(x = xaxis, y = density)) + # FRAGE: stopifnot?;ggplot2:: hinzufügen?
      geom_line() +
      geom_vline(aes(xintercept = tval, color = "empirical value"), linetype = "solid") +
      ggtitle(paste0("Density of t~(",df,") under the H0")) +
      xlab("x") +
      ylab("density P(x)") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))

    # Plot values for confidence interval
    plotLL <- mu0 - 4*popsd # lower limit for CI Plot
    plotUL <- mu0 + 4*popsd # upper limit for CI Plot
    xmu <- seq(plotLL, plotUL, length.out = 1000) # generates regular sequence from lower to upper CI limit with length of 1000
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
      labs(title = "Illustration of the confidence interval", x = "mu_0",
           caption = paste0("The illustration assumes that
                          X~N(",mu0,",",round(sigmasq,3),") in the population"))


    if (testobj$alternative == "greater") { # rechtsseitig
      tkrit <- qt(1-alpha,df)
      LL <- round(testobj$conf.int[[1]],3) # CI lower limit

      xIndicator <- c("Critical value",
                      "Confidence interval lower limit",
                      "p-value")

      xCalculation <- c(paste0("t_krit = qt(1-alpha,df)"),
                        paste0("LL = xmean - qt(1-alpha,df)*se"),
                        paste0("p = 1-pt(t_emp,df)")
      )

      extratab <- data.frame(Indicator = xIndicator,
                             Calculation = xCalculation)

      maintab <- rbind(maintab, extratab) # ergänzt maintab mit den zusätzlichen Infos für den einseitigen Test (bzw. die Werte, die für jede Richtung des Tests anders sind)

      xNotes <- c("- The upper limit of the confidence interval is infinity due to the one-sidedness")

      Notes <- c(Notes, xNotes)

      gplot <- gplot +
        geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
        scale_colour_manual(values = c("red", "blue")) + # ergänzt den Plot um den kritischen Wert
        labs(colour = "key t-values") +
        geom_area(data = subset(data, x >= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5)

      ciplot <- ciplot +
        geom_segment(data = data2[1,], aes(x = LL, y = 0.01, xend = plotUL, yend = 0.01),
                     linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) + # horizontal line at y = 0.01 from lower limit (CI) until end of x-axis (plotUL)
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))

    }


    if (testobj$alternative == "less") { # linksseitig
      tkrit <- qt(alpha,df)
      UL <- round(testobj$conf.int[[2]],3) # CI upper limit

      xIndicator <- c("Critical value",
                      "Confidence interval upper limit",
                      "p-value")

      xCalculation <- c(paste0("t_krit = qt(alpha,df)"),
                        paste0("UL = xmean + qt(1-alpha,df)*se"),
                        paste0("p = pt(t_emp,df)")
      )

      extratab <- data.frame(Indicator = xIndicator,
                             Calculation = xCalculation)

      maintab <- rbind(maintab, extratab)

      xNotes <- c("- The lower limit of the confidence interval is infinity due to the one-sidedness")

      Notes <- c(Notes, xNotes)

      gplot <- gplot +
        geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
        scale_colour_manual(values = c("red", "blue")) + # ergänzt den Plot um den kritischen Wert
        labs(colour = "key t-values") +
        geom_area(data = subset(data, x <= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5)

      ciplot <- ciplot +
        geom_segment(data = data2[1,], aes(x = plotLL, y = 0.01, xend = UL, yend = 0.01),
                     linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) + # horizontal line at y = 0.01 from left end of x-axis until CI upper limit (UL)
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))

    }

    if (testobj$alternative == "two.sided") { # zweiseitig
      tkrit <- qt(1-alpha/2,df)
      tkritb <- qt(alpha/2,df)
      LL <- round(testobj$conf.int[[1]],3)
      UL <- round(testobj$conf.int[[2]],3)

      xIndicator <- c("Positive critical value",
                      "Negative critical value",
                      "Confidence interval lower limit",
                      "Confidence interval upper limit",
                      "p-value")

      xCalculation <- c(paste0("t_krit = qt(1-alpha/2,df)"),
                        paste0("t_krit = qt(alpha/2,df)"),
                        paste0("LL = xmean - qt(1-alpha/2,df)*se"),
                        paste0("UL = xmean + qt(1-alpha/2,df)*se"),
                        paste0("p = 2*(1-pt(abs(t_emp),df))")
      )

      extratab <- data.frame(Indicator = xIndicator,
                             Calculation = xCalculation)

      maintab <- rbind(maintab, extratab)

      xNotes <- c("- In the center of the confidence interval is the estimate.")

      Notes <- c(Notes, xNotes)

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

    dplot <- ggplot(data= df_combined, aes(x = "", y = x)) + # Boxplot for distribution
      geom_boxplot() +
      geom_jitter(width = 0.2, color = "black", alpha = 0.5, size = 2) +
      geom_hline(aes(yintercept = mu0, color = "H0 population value"), linewidth = 1.2) +
      geom_hline(aes(yintercept = xmean, color = "Sample mean"), linewidth = 1.2) +
      scale_colour_manual(values = c("red", "blue")) +
      labs(y = "values", x = "Distribution of values") +
      theme_minimal() +
      theme(legend.title = element_blank())



    print(dplot)
    print(gplot)
    print(ciplot)

    cat("Notes:\n",
        "- Two plots have been created. Use the arrows to navigate.\n",
        "- A table with calculations has been stored in the Global Environment.\n",
        "                  \n",
        sep = "")

    print(testobj)

    cat(indiv_values, sep = "\n")

    cat(components, sep = "\n")
    if (missings == TRUE) {
      xxNotes <- c("- One or more observations were excluded due to missingness.")
      Notes <- c(Notes, xxNotes)
    }
    cat(Notes, sep = "\n")
    assign(paste0("tab_one_t_",obj_name), maintab, envir = .GlobalEnv)

  }

  else if (grepl("Two", method, fixed = TRUE) | grepl("Paired", method, fixed = TRUE)) {
    ## htest method for "Two Sample t-test"
    groups <- names(testobj$estimate) # "mean in group g1" "mean in group g2"
    group1 <- gsub("mean in group ", "", groups[1]) # enthält den Namen von Gruppe 1
    group2 <- gsub("mean in group ", "", groups[2]) # enthält den Namen von Gruppe 2 aus
    dframes <- strsplit(data_name, " by | and ")[[1]] # List variable names as character strings (that's why we have a double bracket); strsplit: splits the elements of a character vector into substrings (hier: sobald and or by vorkommt)

    # --------------------------- #

    checkform <- tryCatch(eval(parse(text = dframes[1])), # trycatch: first argument =the block of code it should attempt to run for the input
                          error = function(e) NULL) # trycatch: second argument = what the function should do if an error is encountered; durch trycatch kann man problematischen Input skippen oder feststellen, wo genau am Input die Probleme sind
    # e ist das error object (kann man nennen, wie man will)
    # -> this function returns NULL in case of an error instead of crashing
    checkdata <- is.null(args$data) # additional arguments specified (TRUE/FALSE)?

    if (!is.null(checkform)) { # = when input is valid
      df1 <- eval(parse(text = dframes[1])) # accesses the values (AV)
      df2 <- eval(parse(text = dframes[2])) # accesses the groups (UV) (in case of long format; else: accesses the values in the second column)
    }
    else if (is.null(checkform) & checkdata == F) { # checkdata == F bedeutet, dass die args-Liste nicht NULL ist, wir also Zusatzargumente haben

      df1 <- args$data[names(args$data) %in% dframes[1]] # collect only values; what i think it does: ich glaube das ist ein anderer Weg auf die Daten zuzugreifen: wenn der obere Weg nicht geklappt hat (checkform = NULL), aber ein zusätzliches data-Argument eingetippt wurde (z.B. data = dlong), dann versucht die Funktion, hierüber auf die Daten zuzugreifen
      df2 <- args$data[names(args$data) %in% dframes[2]] # collect only groups
      # Update: ich glaube, das was im if-Block steht, funktioniert, wenn AV und UV direkt im environment geladen sind (z.B. als zwei separate data vectors)
      # das, was im else if Block steht, braucht man, wenn AV und UV in einem data frame sind und R daher nicht direkt auf die Daten zugreifen kann
      # STIMMT DAS? -> ja, in dem Fall müsste man in der teach-Funktion als user noch das Zusatzargument "data = " hinzufügen
    }
    else{
      stop("Please provide a data frame using testfunc(testobj, data = ...)")
    }
    # --------------------------- #


    if (grepl("by", data_name, fixed = T)) { # wenn formula interface benutzt wurde (also long format)
      df_combined <- data.frame(df1, df2)       # dann sind df1 und df2 values unf groups und man kann die binden
      names(df_combined) <- c("value", "group")
    } else { # TODO: vllt. hier else if und dann unten else (wenn data_name Länge 1 hat oder so)
      df1 <- data.frame(df1, group = "Group 1") # bei wide format (wenn data_name "and" enthält), dann haben wir nicht eine Spalte für values und eine für group, sondern zwei übergeordnete Gruppenspalten, in denen die values drin stehen
      df2 <- data.frame(df2, group = "Group 2") # also ist df1 die Spalte mit den Werten für Gruppe 1 und df2 für Gruppe 2; hier wird jetzt pro dataframe eine neue Spalte für die Gruppenvariable hinzugefügt
      names(df1) <- c("value", "group") # wir benennen die Spalten um
      names(df2) <- c("value", "group")

      df_combined <- rbind(df1, df2) # wir binden jetzt beide frames und erhalten einen einzigen frame mit je einer Spalte für values und group
    }
    if (anyNA(df_combined) == TRUE) {  # exclude missing values
      missings <- TRUE
      df_combined <- na.omit(df_combined)
    }
    else missings <- FALSE
    #return(df_combined)
    n1 <- by(df_combined, df_combined$group, nrow)[[1]] # use only n1 for paired -> FRAGE: was wenn wir z.B. Ehepaare erheben? dann ist n1 ja nur die Hälfte das samples, oder? Oder definiert man n dann als Anzahl an Paaren
    n2 <- by(df_combined, df_combined$group, nrow)[[2]] # "by" splits dataframe by group variable, nrow counts number of rows per group; [[1]] extracts number of rows as integer (per group)
    var1 <- by(df_combined$value, df_combined$group, var)[[1]] # variance group 1
    var2 <- by(df_combined$value, df_combined$group, var)[[2]] # variance group 1
    muv <- testobj$null.value # µ_0
    alpha <- 1-attr(testobj$conf.int,"conf.level") # alpha-niveau
    tval <- testobj$statistic # empirische Prüfgröße
    pval <- testobj$p.value # p-Wert

    if (length(testobj$estimate) == 2) { # wenn wir für beide Gruppen ein estimate (= mean) bekommen -> also: wenn t.Test für unabh. Stichproben
      xmean1 <- testobj$estimate[[1]]
      xmean2 <- testobj$estimate[[2]]
      xmean <- xmean1 - xmean2 # mean diference
      df <- testobj$parameter # anpassen im Text: uncorrected; Freiheitsgrade; wenn man die df so speichert, dann gibt der Output in der ersten Zeile noch "df" aus -> wollen wir das?
      df_raw <- n1+n2-2 # unkorrigierte Freiheitsgrade?
      sigmasq <- ((n1-1)*var1+(n2-1)*var2)/((n1-1)+(n2-1)) # gepoolte geschätzte Varianz
      popsd <- round(sqrt(sigmasq),3) # gepoolte geschätzte SD
      se <- sqrt(sigmasq*((1/n1)+(1/n2))) # SE

      vars <- c("n1", "n2", "mean1", "mean2", "var1", "var2", "df", "t_emp", "se",
                "alpha") # die Parameter links im Output
      vals <- c(n1, n2,
                paste0(obj_name, '[["estimate"]][[1]]'), # paste0 converts its arguments to character strings and concatenates them; brauchen wir später für den Output
                paste0(obj_name, '[["estimate"]][[2]]'), # durch diese Schreibweise im Output wissen die Studis, wie sie selbst mit dem Testobj. auf die Werte kommen können
                var1,
                var2,
                paste0(obj_name, '[["parameter"]]'),
                paste0(obj_name, '[["statistic"]]'),
                paste0(obj_name, '[["stderr"]]'),
                alpha)

      comments <- c(
        "sample size group 1",
        "sample size group 2",
        "mean group 1",
        "mean group 2",
        "variance group 1",
        "variance group 2",
        "final degrees of freedom",
        "empirical value",
        "standard error",
        "alpha-level"
      )
      # Combine into data frame
      comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE)

      # Format and print
      components <- with(comps, { # mit with kann direkt auf die Variablen im Datensatz ohne das Dollarzeichen zugreifen (es passiert alles innerhalb des comps data frames, ohne das jedes Mal dazusagen zu müssen)
        var_fmt <- format(vars, width = max(nchar(vars)), justify = "left") #width = max(nchar(vars)) = die Weite für die Auflistung der Parameter soll so groß sein wie die maximale Anzahl an characters in vars
        val_fmt <- format(vals, width = max(nchar(vals)), justify = "left") # justify = "left" = left-aligned
        comment_fmt <- paste0("# ", comments)

        paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
      })

      Notes <- c(
        "",
        "Supplementary Information:",
        "- If homogeneity of variances is not assumed, the df shown in the output are adjusted",
        "- The estimate is the difference of group means",
        "- The effect size Cohen's d is not shown in the output")

      Indicator <- c("Degrees of freedom",
                     "Pooled within-variance",
                     "Estimated standard error",
                     "Empirical t-value",
                     "Effect size Cohen's d")

      Calculation <- c(paste0("df = n1 + n2 -2"),
                       paste0("var_pooled = ((n1-1)*var1+(n2-1)*var2)/(n1-1+n2-1)"),
                       paste0("s.e. = sqrt(var_pooled*((1/n1)+(1/n2)))"),
                       paste0("t_emp = (mean1-mean2)/se"),
                       paste0("d = (mean1-mean2)/sqrt(var_pooled)")
      )

      maintab <- data.frame(Indicator, Calculation) # wo kommt das im Output vor? -> das ist für die calculations tabelle


    } else { # wenn wir nicht zwei estimates im Output bekommenn -> t-Test für abhängig Stichproben
      xmean <- testobj$estimate # mean difference
      df <- testobj$parameter
      firstlevel <- names(table(df_combined$group))[1] # Name der ersten Gruppe
      secondlevel <- names(table(df_combined$group))[2] # Name der zweite Gruppe
      diffvar <- df_combined[df_combined$group == secondlevel,"value"] -
        df_combined[df_combined$group == firstlevel,"value"] # Differenzvariable; FRAGE: warum rechnest du hier Gruppe 2 minus Gruppe 1, im Output ist das doch andersherum?
      sigmasq <- var(diffvar) # Varianz der Differenzvariablen
      popsd <- round(sqrt(sigmasq),3) # SD der Differenzvariablen
      se <- sqrt(sigmasq)/sqrt(n1) # since n1 and n2 are the same, use n1


      vars <- c("n", "mean_diff", "var_diff", "df", "t_emp", "se",
                "h0", "alpha")
      vals <- c(n1,
                paste0(obj_name, '[["estimate"]][["mean difference"]]'),
                sigmasq,
                paste0(obj_name, '[["parameter"]]'),
                paste0(obj_name, '[["statistic"]]'),
                paste0(obj_name, '[["stderr"]]'),
                paste0(obj_name, '[["null.value"]][["mean difference"]]'),
                alpha)

      comments <- c(
        "sample size",
        "mean of differences",
        "variance of differences",
        "final degrees of freedom",
        "empirical value",
        "standard error",
        "null value",
        "alpha-level"
      )

      # Combine into data frame
      comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE)

      # Format and print
      components <- with(comps, {
        var_fmt <- format(vars, width = max(nchar(vars)), justify = "left")
        val_fmt <- format(vals, width = max(nchar(vals)), justify = "left")
        comment_fmt <- paste0("# ", comments)

        paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
      })

      Notes <- c(
        "",
        "Supplementary Information:",
        "- The estimate is the mean of the group differences",
        "- The effect size Cohen's d is not shown in the output")

      Indicator <- c("Degrees of freedom",
                     "Estimated standard error",
                     "Empirical t-value",
                     "Effect size Cohen's d")

      Calculation <- c(paste0("df = n-1"),
                       paste0("se = sqrt(var_diff)/sqrt(n)"),
                       paste0("t_emp = (mean_diff - h0)/se"),
                       paste0("d = (mean_diff)/sqrt(var_diff)")
      )

      maintab <- data.frame(Indicator, Calculation) # wo ist das im Output? -> ist nur noch im export

    }

    # Plot values for null-distribution
    lowerlimit <- -abs(tval)-4 # untere Grenze der x-Achse
    upperlimit <- abs(tval)+4 # oberes Limit der x-Achse; das mit abs ensures dass Verteilung symmetrisch gezeigt wird (also z.B. von -11 bis 11 statt von -11 bis 3)
    xaxis <- seq(lowerlimit, upperlimit, length.out = 1000) # generates regular sequence from lower to upper limit with length of 1000
    density <- dt(xaxis, df) # Dichte für jeden Wert auf der x-Achse
    data <- data.frame(x = xaxis, density = density)

    gplot <-
      ggplot2::ggplot(data, aes(x = xaxis, y = density)) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(aes(xintercept = tval, color = "empirical value"), linetype = "solid") +
      ggplot2::ggtitle(paste0("Density of t~(",df,") under the H0")) +
      ggplot2::xlab("x") +
      ggplot2::ylab("density P(x)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    # Plot values for confidence interval
    plotLL <- mu0 - 4*popsd # lower limit for CI Plot
    plotUL <- mu0 + 4*popsd # upper limit for CI Plot
    xmu <- seq(plotLL, plotUL, length.out = 1000)
    CIdensity <- dnorm(xmu, mean = mu0, sd = popsd)
    data2 <- data.frame(x = xmu, density = CIdensity) # FRAGE: warum zeigen wir das mit einer NV (weil es um die Pop. geht? -> aber die Werte sind doch geschätzt) -> ah, es geht um die H0, oder?aber braucht man dann nicht trz die t-Verteilung

    ciplot <- ggplot2::ggplot(data2, aes(x = xmu, y = CIdensity)) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = c(round(plotLL,1),
                                             round(mu0-3*popsd,1),
                                             round(mu0-2*popsd,1),
                                             round(mu0-1*popsd,1),
                                             mu0,
                                             round(mu0+1*popsd,1),
                                             round(mu0+2*popsd,1),
                                             round(mu0+3*popsd,1),
                                             round(plotUL,1))) +
      ggplot2::labs(title = "Illustration of the confidence interval", x = "mu_0",
                    caption = paste0("The illustration assumes that
                          X~N(",mu0,",",round(sigmasq,3),") in the population"))

    if (testobj$alternative == "greater") { # wenn rechtsseitiger Test (mdiff > 0)
      tkrit <- round(qt(1-alpha,df),3) # tkrit = .95-Quantil
      LL <- round(testobj$conf.int[[1]],3) # CI lower limit

      xIndicator <- c("Critical value",
                      "Confidence interval lower limit",
                      "p-value")

      xCalculation <- c(paste0("t_krit = qt(1-alpha,df)"), # FRAGE: warum brauchen wir hier paste0?
                        paste0("LL = mean_diff - qt(1-alpha,df)*se"),
                        paste0("p = 1-pt(t_emp,df)")
      )

      extratab <- data.frame(Indicator = xIndicator,
                             Calculation = xCalculation)

      maintab <- rbind(maintab, extratab) # ergänzt maintab mit den zusätzlichen Infos für den einseitigen Test (bzw. die Werte, die für jede Richtung des Tests anders sind)

      xNotes <- c("- The upper limit of the confidence interval is infinity due to the one-sidedness")

      Notes <- c(Notes, xNotes)


      gplot <- gplot +
        ggplot2::geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
        ggplot2::scale_colour_manual(values = c("red", "blue")) +
        ggplot2::labs(colour = "key t-values") +
        ggplot2::geom_area(data = subset(data, x >= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5) # färbt den Ablehnungsbereich der H0 ein

      ciplot <- ciplot +
        ggplot2::geom_segment(data = data2[1,], aes(x = LL, y = 0.01, xend = plotUL, yend = 0.01),
                              linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) + # horizontal line at y = 0.01 from lower limit (CI) until end of x-axis (plotUL)
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5))
    }

    if (testobj$alternative == "less") { # linksseitiger Test
      tkrit <- round(qt(alpha,df),3)
      UL <- round(testobj$conf.int[[2]],3)

      xIndicator <- c("Critical value",
                      "Confidence interval upper limit",
                      "p-value")

      xCalculation <- c(paste0("t_krit = qt(alpha,df)"),
                        paste0("UL = mean_diff + qt(1-alpha,df)*se"),
                        paste0("p = pt(t_emp,df)")
      )


      extratab <- data.frame(Indicator = xIndicator,
                             Calculation = xCalculation)

      maintab <- rbind(maintab, extratab)


      xNotes <- c("- The upper limit of the confidence interval is infinity due to the one-sidedness")

      Notes <- c(Notes, xNotes)


      gplot <- gplot +
        ggplot2::geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
        ggplot2::scale_colour_manual(values = c("red", "blue")) +
        ggplot2::labs(colour = "key t-values") +
        ggplot2::geom_area(data = subset(data, x <= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5)

      ciplot <- ciplot +
        ggplot2::geom_segment(data = data2[1,], aes(x = plotLL, y = 0.01, xend = UL, yend = 0.01),
                              linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5))
    }

    if (testobj$alternative == "two.sided") { # zweiseitiger Test
      tkrit <- round(qt(1-alpha/2,df),3)
      tkritb <- round(qt(alpha/2,df),3)
      LL <- round(testobj$conf.int[[1]],3)
      UL <- round(testobj$conf.int[[2]],3)


      xIndicator <- c("Positive critical value",
                      "Negative critical value",
                      "Confidence interval lower limit",
                      "Confidence interval upper limit",
                      "p-value")

      xCalculation <- c(paste0("t_krit = qt(1-alpha/2,df)"),
                        paste0("t_krit = qt(alpha/2,df)"),
                        paste0("LL = mean_diff - qt(1-alpha/2,df)*se"),
                        paste0("UL = mean_diff + qt(1-alpha/2,df)*se"),
                        paste0("p = 2*(1-pt(abs(t_emp),df))")
      )


      extratab <- data.frame(Indicator = xIndicator,
                             Calculation = xCalculation)

      maintab <- rbind(maintab, extratab)

      xNotes <- c("- In the center of the confidence interval is the estimate.")

      Notes <- c(Notes, xNotes)

      gplot <- gplot +
        ggplot2::geom_vline(aes(xintercept = tkrit, color = "critical value"), linetype = "solid") +
        ggplot2::geom_vline(aes(xintercept = tkritb, color = "critical value"), linetype = "solid") +
        ggplot2::scale_colour_manual(values = c("red", "blue")) +
        ggplot2::labs(colour = "key t-values") +
        ggplot2::geom_area(data = subset(data, x >= tkrit), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5) +
        ggplot2::geom_area(data = subset(data, x <= tkritb), aes(x = x, y = density), fill = "lightcoral", alpha = 0.5)

      ciplot <- ciplot +
        ggplot2::geom_segment(data = data2[1,], aes(x = LL, y = 0.01, xend = UL, yend = 0.01),
                              linewidth = 2, color = rgb(140, 50, 80, maxColorValue = 250)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5))
    }

    dplot <- ggplot(df_combined, aes(x = group, y = value, fill = group)) + # Boxplots for distribution per group
      ggplot2::geom_boxplot() +
      ggplot2::labs(title = "Distribution of values", x = "Group", y = "Value") +
      ggplot2::scale_fill_manual(values = c(rgb(140, 50, 80, maxColorValue = 250),
                                            rgb(242,242,242, maxColorValue = 250))) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position="none",
                     plot.title = element_text(hjust = 0.5))



    if (export) { # = if export == TRUE???
      pdf("overview.pdf", width = 8.3, height = 11.7) # DIN A4 size

      testtab <- broom::tidy(testobj) # WHY?
      numcols <- sapply(testtab, class) == "numeric" # bewertet, ob die Werte im tibble numeric sind
      testtab[numcols] <- round(testtab[numcols],3) # die numerischen Einträge im tibble sollen gerundet werden

      gridExtra::grid.arrange(
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

    print(dplot)
    print(gplot)
    print(ciplot)

    cat("Notes:\n",
        "- Three plots have been created. Use the arrows to navigate.\n",
        "- A table with calculations has been stored in the Global Environment.\n",
        "                  \n",
        sep = "")

    print(testobj)

    cat(indiv_values, sep = "\n")

    cat(components, sep = "\n")
    if (missings == TRUE) {
      xxNotes <- c("- Note that one or more observations were excluded due to missingness.")
      Notes <- c(Notes, xxNotes)
    }
    cat(Notes, sep = "\n")
    assign(paste0("tab_two_t_",obj_name), maintab, envir = .GlobalEnv) # assigns a value to a name in an environment (hier: global environment)

  }
  else if (grepl("Anova", method, fixed = TRUE)) {
    # variables to check whether we can calculate either an independent or repeated measures ANOVA
    list_between <- length(attributes(testobj)$between) # checks number of between subjects factors
    list_within <- length(attributes(testobj)$within) # checks number of within subjects factors
    if (list_between & list_within > 0 ||
        list_between > 1 ||
        list_within > 1) stop("Multifactorial ANOVA is currently not supported")
    df_long <- testobj[["data"]][["long"]]
    names(df_long) <- c("id","factor","dv") # data always sorted
    # trying to merge the code of independent and repeated measures ANOVA
    if (is.null(ncol(testobj[["lm"]][["fitted.values"]]))) { # checks structure of fitted values (varies between within and between design)
      meanvals <- testobj[["lm"]][["fitted.values"]]
      meanvals <- data.frame(meanvals, factor = df_long$factor)
      meanvals$meanvals <- round(meanvals$meanvals, 12) # TODO: is this safe?
      meanvals <- meanvals %>%
        select(meanvals, factor)  %>%
        distinct(meanvals, .keep_all = TRUE)
    } else { # within design
      meanvals <- testobj[["lm"]][["fitted.values"]][1,]
      meanvals <- data.frame(meanvals, factor = names(meanvals))}

    femp <- testobj[["anova_table"]][["F"]] # empirischer F-Wert
    df_long <- merge(df_long, meanvals)
    df_long <- df_long %>%
      group_by(factor) %>% # group_by means that all subsequent operations are performed within the groups of the grouping variable
      arrange(factor) %>% # orders the rows of a data frame by the values of the selected columns; here we do it for aesthetic purposes
      mutate(             # creates new columns that are functions of existing variables; also modifies and deletes columns
        group_index = as.numeric(factor),  # assigns numbers to the groups numbers to place them on x-axis
        within_group_index = rank(dv, ties.method = "first"), # ranks subjects in ascending order; "ties.method = "first": in case of ties, the lowest rank is given to the subject that appears earliest in the data
        n_in_group = n(), # group size
        x_pos = group_index +
          (within_group_index - (n_in_group + 1) / 2) * (0.8 / n_in_group)  # even distribution within group
      ) %>% # within_group_index - (n_in_group + 1) / 2) spaces subjects around the center of each group; * (0.8 / n_in_group) determines spacing between points: 0.8 bc we want all points to be in a range of ± 0.4 of each group index; by dividing by n_in_group we make the spacing relative to the number of subjects
      ungroup() # Gruppierung aufheben, sodass die folgenden Operationen nicht mehr innerhalb der Gruppen stattfinden
    ntotal <- nrow(testobj[["data"]][["wide"]]) # missing values removed (aov_ez Funktion entfernt die automatisch)
    # between subjects ANOVA:
    if (list_within == 0) {
      # creating individual position for each participant on x axis for the plots
      df1 <- testobj[["anova_table"]][["num Df"]]
      df2 <- testobj[["anova_table"]][["den Df"]]
      groups <- df1+1 # Anzahl Gruppen
      SSB <- testobj[["Anova"]][["Sum Sq"]][2] # QS_zw (Treatment)
      SSW <- testobj[["Anova"]][["Sum Sq"]][3] # QS_inn (Fehler)
      fcrit <- qf(0.95, df1, df2)
      ges <- testobj[["anova_table"]][["ges"]] # generalised eta square

      vars <- c("n","groups","SSF","SSE") # SSF = SSB, SSE = SSW
      vals <- c(ntotal, groups, SSB, SSW)
      comments <- c("sample size", "number of groups",
                    "Factor sum of squares", "Error sum of squares")

      # Combine into data frame
      comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE)

      # Format and print
      components <- with(comps, {
        var_fmt <- format(vars, width = max(nchar(vars)), justify = "left")
        val_fmt <- format(vals, width = max(nchar(vals)), justify = "left")
        comment_fmt <- paste0("# ", comments)

        paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
      })

      # Check data characteristics
      heterosk <- car::leveneTest(dv ~ factor, df_long)$`Pr(>F)`[1] < 0.05 # output: TRUE or FALSE (p-Wert < 0.5?)
      group_sizes <- paste(as.character(table(df_long$factor)),collapse=", ") # group sizes

      Notes <- c(
        "",  paste0("Supplementary Information: The group sizes are ", group_sizes) # was machen die Anführungszeichen in der ersten Zeile? -> sind dafür da, dass da eine Lerrzeile zwischen den calculations und den Notes ist
      )
      if (heterosk == T) Notes <- c(Notes, "- Variances might be unequal") # sollte man nicht genauer sagen, dass ein Levene-Test gerechnet wurde?

      Indicator <- c("Factor degrees of freedom (num Df)",
                     "Error degrees of freedom (den Df)",
                     "Factor mean sum of squares",
                     "Error mean sum of squares",
                     "Empirical F-value",
                     "Critical F-value",
                     "p-value",
                     "generalized eta squared")

      Calculation <- c("df1 = groups-1",
                       "df2 = n-groups",
                       "MSF = SSF/df1",
                       "MSE = SSE/df2",
                       "F_emp = MSF/MSE",
                       "F_crit = qf(0.95, df1, df2)",
                       "p = 1-pf(F_emp, df1, df2)",
                       "ges = SSF/(SSF+SSE)")

      maintab <- data.frame(Indicator, Calculation)

      # dplot and bplot were moved below bc of overlapping code between independent and repeated measures ANOVA

      wplot <- ggplot2::ggplot(data = df_long) +
        ggplot2::geom_hline(aes(yintercept = mean(dv)), color = "lightgrey", linewidth = 1) +
        ggplot2::geom_point(aes(x = factor, y = meanvals,
                                color = "group means"),
                            size = 3, alpha = 0.9) +
        ggplot2::geom_point(aes(x = x_pos, y = dv),
                            color = "black", alpha = 0.5, size = 2) +
        ggplot2::scale_colour_manual(values = c("red","blue")) +
        ggplot2::geom_segment(aes(x = x_pos,
                                  y = dv, yend = meanvals, color = "Within-group variance"),
                              alpha = 0.5) +
        ggplot2::labs(y = "values", x = "groups") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = element_blank())


      # distribution under h0 plot
      # Plot values
      lowerlimit <- 0
      upperlimit <- femp+10
      xaxis <- seq(lowerlimit, upperlimit, length.out = 1000) # generates regular sequence from lower to upper limit with length of 1000
      density <- df(xaxis, df1=df1, df2=df2)
      data <- data.frame(x = xaxis, density = density)

      gplot <- ggplot2::ggplot(data, aes(x = xaxis, y = density)) + # Density function
        ggplot2::geom_line() +
        ggplot2::geom_vline(aes(xintercept = femp, color = "empirical value"), linetype = "solid") +
        ggplot2::ggtitle(paste0("Density of F~(0.95,",df1,",",df2,") under the H0")) +
        ggplot2::geom_vline(aes(xintercept = fcrit, color = "critical value"), linetype = "solid") +
        ggplot2::scale_colour_manual(values = c("red", "blue")) +
        ggplot2::labs(colour = "key F-values") +
        ggplot2::geom_area(data = subset(data, x >= fcrit),
                           aes(x = x, y = density),
                           fill = "lightcoral", alpha = 0.5) +
        ggplot2::xlab("x") + ylab("density P(x)") +
        ggplot2::theme_minimal()+
        ggplot2::theme(plot.title = element_text(hjust = 0.5))

      # Varicance components plot
      # Plot values
      expl <- data.frame(
        Component = factor(c("Factor variance (ges)", "Error variance")),
        Value = c(ges*100, (1-ges)*100)
      )

      vplot <- ggplot2::ggplot(expl, aes(x = "", y = Value, fill = Component)) +
        ggplot2::geom_bar(stat = "identity", color = "black") +
        ggplot2::scale_fill_manual(values = c("Factor variance (ges)" = "steelblue",
                                              "Error variance" = "lightgrey")) +
        ggplot2::geom_text(aes(label = paste0(round(ges*100,2), "%"), y = ges*100/2),
                           color = "black") +
        ggplot2::labs(x = NULL, y = "Percentage", fill = NULL) +
        ggplot2::ggtitle("Variance components") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       panel.grid = element_blank(),
                       plot.title = element_text(hjust = 0.5))

      print(gplot)
      print(wplot)
      print(vplot)
    }
    else { # repeated measurements ANOVA
      sumobj <- summary(testobj)
      # creating individual position for each participant on x axis for the plots
      df_wide <- testobj[["data"]][["wide"]]
      names(df_wide)[1] <- "id"
      df_wide$idmeans <- rowMeans(df_wide[,2:ncol(df_wide)]) # für Mittelwertberechnung werden erst alle columns ab columns 2 berücksichtigt
      df_long <- merge(df_long, df_wide[,c("id","idmeans")], by = "id")

      groups <- length(testobj[["Anova"]][["idata"]][["Bedingung"]]) # number of groups

      df1 <- groups-1 # Zähler-df
      df2 <- (groups-1)*(ntotal-1) # Nenner-df
      gg <- sumobj[["pval.adjustments"]][1,1] # Greenhouse-Geisser
      gg1 <- df1*gg # korrigierte Zähler-df
      gg2 <- df2*gg # korrigierte Nenner-df

      SSF <- sumobj[["univariate.tests"]][2,1] # SSF = QS_zw_Bedingungen
      SSR <- sumobj[["univariate.tests"]][2,3] # SSR = QS_res
      SSB <- sumobj[["univariate.tests"]][1,3] # SSB = QS_zwP
      MQSF <- SSF/df1 # MQS_zw_Bedingungen
      MQSR <- SSR/df2 # MQS_Res

      fcrit <- qf(0.95, df1, df2) # kritischer F-Wert
      fcorr <- qf(0.95, gg1, gg2) # korrigierter kritischer F-Wert
      ges <- testobj[["anova_table"]][["ges"]] # Effektgröße: generalised eta squared


      vars <- c("n","groups","SSF","SSR","SSB","GGeps")
      vals <- as.character(c(ntotal, groups,
                             round(SSF,3),
                             round(SSR,3),
                             round(SSB,3),
                             round(gg,3)))
      comments <- c("sample size", "number of groups",
                    "factor sum of squares", "residual sum of squares",
                    "between-individual sum of squares",
                    "Greenhouse-Geisser epsilon")

      comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE)
      components <- with(comps, {
        var_fmt <- format(vars, width = max(nchar(vars)), justify = "left")
        val_fmt <- format(vals, width = max(nchar(vals)), justify = "left")
        comment_fmt <- paste0("# ", comments)

        paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
      })

      Notes <- c() # just a placeholder so that we dont get an error message when the code at the end (for both ANOVAs) wants to insert notes (l. 1134)
      Indicator <- c("Factor degrees of freedom (numDf)",
                     "Residual degrees of freedom (denDf)",
                     "Factor mean sum of squares",
                     "Residual mean sum of squares",
                     "Empirical F-value",
                     "Critical F-value",
                     "p-value",
                     "Generalized eta squared",
                     "Greenhouse-Geisser corrected factor degrees of freedom",
                     "Greenhouse-Geisser corrected residual degrees of freedom",
                     "Greenhouse-Geisser corrected critical F-value",
                     "Greenhouse-Geisser corrected p-value")

      Calculation <- c("df1 = groups-1",
                       "df2 = (groups-1)*(n-1)",
                       "MSF = SSF/df1",
                       "MSR = SSR/df2",
                       "F_emp = MSF/MSR",
                       "F_crit = qf(0.95, df1, df2)",
                       "p = 1-pf(F_emp, df1, df2)",
                       "ges = SSF/(SSF+SSR+SSB)",
                       "gg1 = numDF*GGeps",
                       "gg2 = denDF*GGeps",
                       "ggF_crit = qf(0.95, gg1, gg2)",
                       "ggp = 1-pf(F_emp, gg1, gg2)")

      maintab <- data.frame(Indicator, Calculation)

      # dplot and bplot were moved below because of overlapping code between independent and repeated measurements ANOVA

      # HIER PRÜFEN, OB VERSETZTE PUNKTE UNÜBERSICHTLICH WERDEN
      lplot <- ggplot2::ggplot(aes(x = factor, y = dv, group = id, color = id), data = df_long) +
        ggplot2::geom_point(aes(x = factor, y = dv),
                            color = "black", alpha = 0.5, size = 2) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_hline(aes(yintercept = mean(dv)), color = "darkred", linewidth = 1) +
        ggplot2::labs(y = "values", x = "Groups") +
        ggplot2::scale_colour_manual(values = distinctColorPalette(ntotal)) + # scale_color_brewer did not work if n > 12
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = element_blank())


      wplot <- ggplot2::ggplot(data = df_long) +
        ggplot2::geom_segment(aes(x=id, xend=id,
                                  y=idmeans, yend=mean(dv),
                                  color = "Between-individual variance"),
                              linewidth = 1.2) +
        ggplot2::geom_point(aes(x = id, y = idmeans,
                                color = "Individual means"),
                            size = 3, alpha = 0.9) +
        ggplot2::geom_point(aes(x = id, y = dv),
                            color = "grey", alpha = 0.5, size = 2) +
        ggplot2::geom_hline(aes(yintercept = mean(dv), color = "Sample mean"), linewidth = 1) +
        ggplot2::scale_colour_manual(values = c("blue","red","darkred")) +
        ggplot2::labs(y = "values", x = "Individuals") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = element_blank())

      lowerlimit <- 0
      upperlimit <- femp+10
      xaxis <- seq(lowerlimit, upperlimit, length.out = 1000) # generates regular sequence from lower to upper limit with length of 1000
      density <- df(xaxis, df1=df1, df2=df2)
      data <- data.frame(x = xaxis, density = density)

      gplot <- ggplot2::ggplot(data, aes(x = xaxis, y = density)) + # Density function
        ggplot2::geom_line() +
        ggplot2::geom_vline(aes(xintercept = femp, color = "Empirical value"), linetype = "solid") +
        ggplot2::ggtitle(paste0("Density of F~(0.95,",df1,",",df2,") under the H0")) +
        ggplot2::geom_vline(aes(xintercept = fcrit, color = "Critical value"), linetype = "solid") +
        ggplot2::geom_vline(aes(xintercept = fcorr, color = "Corrected critical value"), linetype = "solid") +
        ggplot2::scale_colour_manual(values = c("orange","red", "blue")) +
        ggplot2::labs(colour = "key F-values") +
        ggplot2::geom_area(data = subset(data, x >= fcrit),
                           aes(x = x, y = density),
                           fill = "lightcoral", alpha = 0.5) +
        ggplot2::xlab("x") + ylab("density P(x)") +
        ggplot2::theme_minimal()+
        ggplot2::theme(plot.title = element_text(hjust = 0.5))

      expl <- data.frame(
        Component = factor(c("Factor variance (ges)",
                             "Between-individual variance",
                             "Residual variance"),
                           levels = c("Residual variance",
                                      "Between-individual variance",
                                      "Factor variance (ges)")),
        Value = c(ges*100, (SSB/(SSB+SSF+SSR))*100, (SSR/(SSB+SSF+SSR))*100)
      )

      vplot <- ggplot2::ggplot(expl, aes(x = "", y = Value, fill = Component)) + # variance components plot
        ggplot2::geom_bar(stat = "identity", color = "black") +
        ggplot2::scale_fill_manual(values = c("Factor variance (ges)" = "steelblue",
                                              "Between-individual variance" = "darkgrey",
                                              "Residual variance" = "lightgrey")) +
        ggplot2::geom_text(aes(label = paste0(round(ges*100,2), "%"), y = ges*100/2),
                           color = "black") +
        ggplot2::labs(x = NULL, y = "Percentage", fill = NULL) +
        ggplot2::ggtitle("Variance components") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       panel.grid = element_blank(),
                       plot.title = element_text(hjust = 0.5))

      print(lplot)
      print(gplot)
      print(wplot)
      print(vplot)

    }
    dplot <- ggplot2::ggplot(data = df_long) + # plotting group means, sample means, and distributions within groups
      ggplot2::geom_boxplot(aes(x = factor, y = dv), colour = "lightgrey") +
      ggplot2::geom_point(aes(x = factor, y = meanvals,
                              color = "group means"),
                          size = 3, alpha = 0.9) +
      ggplot2::geom_point(aes(x = x_pos, y = dv),
                          color = "black", alpha = 0.5, size = 2) +
      ggplot2::geom_hline(aes(yintercept = mean(dv), color = "sample mean"), linewidth = 1) +
      ggplot2::scale_colour_manual(values = c("red","darkred")) +
      ggplot2::labs(y = "values", x = "groups") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = element_blank())

    bplot <- ggplot2::ggplot(data = df_long) + #
      ggplot2::geom_segment(aes(x=factor, xend=factor,
                                y=meanvals, yend=mean(dv),
                                color = "Between-group variance"),
                            linewidth = 1.2) +
      ggplot2::geom_point(aes(x = factor, y = meanvals,
                              color = "group means"),
                          size = 3, alpha = 0.9) +
      ggplot2::geom_point(aes(x = x_pos, y = dv),
                          color = "lightgrey", alpha = 0.5, size = 2) +
      ggplot2::geom_hline(aes(yintercept = mean(dv), color = "sample mean"), linewidth = 1) +
      ggplot2::scale_colour_manual(values = c("blue","red","darkred")) +
      ggplot2::labs(y = "values", x = "groups") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = element_blank())


    print(bplot)
    print(dplot)

    cat("Notes:\n",
        "- Multiple plots have been created. Use the arrows to navigate.\n", # cat = concatenate and print: printet Text in die Konsole (ohne Linienumbrüche)
        "- A table with calculations has been stored in the Environment.\n",
        "                  \n",
        sep = "")

    suppressWarnings(print(summary(testobj))) # suppresses second warning message when HF > 1
    cat("\n")
    cat(indiv_values, sep = "\n")

    cat(components, sep = "\n")
    cat(Notes, sep = "\n")
    assign(paste0("tab_aov_",obj_name), maintab, envir = .GlobalEnv)
  }
  else if (grepl("Call", method, fixed = TRUE)) {
    model_formula <- capture.output(formula(testobj)) # extracting the formula to use it for differentiation between linear and moderated regression

    npred <- length(testobj[["coefficients"]])-1 # number of predictors (-1 wegen Intercept)
    if (npred < 2) stop(paste0("Please specify 2 predictors and an optional interaction term.")) # we already need an error message here in case subjects provided less than 2 predictors

    # converting non-numeric predictors to numeric
    pred1 <- testobj[["model"]][[2]]
    pred2 <- testobj[["model"]][[3]]
    if (!is.numeric(pred1) | !is.numeric(pred2)) { # run the if block if at leas one of the predictors is not numeric
      numeric <- FALSE
      pred1 <- as.numeric(as.character(pred1)) # we use "as.character" to ensure that the factor levels start with 0 instead of 1
      pred2 <- as.numeric(as.character(pred2))
    } else numeric <- TRUE# side note: if i place this if statement at the top (before obj_name), i get a warning message

    sumobj <- summary(testobj)
    nids <- length(testobj[["fitted.values"]]) # sample size
    sdy <- round(sd(testobj[["model"]][[1]]),3) # SD Y
    sdyhat <- round(sd(testobj[["fitted.values"]]),3) # SD fitted values

    rsq <- round(cor(testobj[["model"]][[1]],testobj[["fitted.values"]])^2,4) # R^2 (correlation between y and yhat)
    int <- round(testobj[["coefficients"]][["(Intercept)"]],4) # intercept value
    coef1 <- round(testobj[["coefficients"]][[2]],4) # b1
    coef2 <- round(testobj[["coefficients"]][[3]],4) # b2
    std1 <- round(sumobj[["coefficients"]][2,2],4) # standard error for b1
    std2 <- round(sumobj[["coefficients"]][3,2],4) # standard error for b2

    depname <- names(testobj[["model"]][1]) # name of dependendent variable
    coef1name <- names(testobj[["model"]][2]) # name predictor 1
    coef2name <- names(testobj[["model"]][3]) # name predictor 2

    df <- data.frame(dv = testobj[["model"]][,1], pred1, pred2) # creating a dataframe with all the relevant variables
    df_names <- c(depname, coef1name, coef2name)
    names(df) <- df_names

    pvalues_coefs <- sumobj$coefficients[,4] # extracting p-values of the coefficients (which are listed in column 4)
    # determining wether coefficients are statistically significant
    sig_list <- list()
    for (i in seq_along(pvalues_coefs)) { # creating index for each pvalue
      if (pvalues_coefs[i] < .05) sig_list[[i]] <- "(statistically significant, p < .05)"
      else sig_list[[i]] <- "(not statistically significant, p ≥ .05)"
    }

    # moderated regression
    if (grepl("*", model_formula, fixed = TRUE) | grepl(":", model_formula, fixed = TRUE)){

      mainmod <- lm(testobj[["model"]][[1]] ~ testobj[["model"]][[2]] + testobj[["model"]][[3]]) # multiple regression model without interaction
      mainrsq <- round(cor(mainmod[["model"]][[1]],mainmod[["fitted.values"]])^2,4) # R^2 in the model without interaction

      if (npred != 3) stop(paste0("Please specify 2 predictors and an optional interaction term."))
      coef3 <- round(testobj[["coefficients"]][[4]],4) # b3 (interaction)
      std3 <- round(sumobj[["coefficients"]][4,2],4) # standard error for b3
      coef3name <- paste0(names(testobj[["model"]][2]),"*",names(testobj[["model"]][3])) # name predictor 3 (interaction)

      vars <- c("n","k","rsq","var_predicted","var_observed")
      vals <- as.character(c(nids, npred, rsq,
                             round(sdyhat^2,4), round(sdy^2,4)))
      comments <- c("sample size", "number of predictors", "R squared",
                    paste("variance of predicted",depname,"values"),
                    paste("variance of observed",depname,"values"))

      comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE) # combine into dataframe
      components <- with(comps, {
        var_fmt <- format(vars, width = max(nchar(vars)), justify = "left")
        val_fmt <- format(vals, width = max(nchar(vals)), justify = "left")
        comment_fmt <- paste0("# ", comments)

        paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
      }) # der Abschnitt ist das, was später im Output zu sehen ist


      ints <- c(" ",
                "  Key model results:",
                paste("- If all predictors have a value of zero, we predict a",depname,"value of",int, sig_list[[1]]),
                paste("- For a",coef1name,"value of zero, the regression coefficient of",coef2name,"is",coef2, sig_list[[3]]),
                paste("- For a",coef2name,"value of zero, the regression coefficient of",coef1name,"is",coef1, sig_list[[2]]),
                paste0("- With each unit increase in ",coef1name,", the regression coefficient of ",coef2name," changes by ",coef3," units ", sig_list[[4]]),
                paste0("- With each unit increase in ",coef2name,", the regression coefficient of ",coef1name," changes by ",coef3," units ", sig_list[[4]]),
                paste0("- The model explains ",rsq*100,"% of variance in ",depname))
      names(ints) <- " " # ???
      ints <- format(ints, justify = "left")

      Indicator <- c("Global test results:",
                     "    df1",
                     "    df2",
                     "    Empirical F-value",
                     "    Critical F-value",
                     "    F-test p-value",
                     "    Variance explained",
                     "    effect size f-squared",

                     "Regression coefficient test results:",
                     "    df",
                     "    First predictor t-value (Estimate/Std.Err.)",
                     "    Second predictor t-value (Estimate/Std.Err.)",
                     "    Interaction t-value (Estimate/Std.Err.)",
                     "    Critical t-values",
                     "    First predictor p-value",
                     "    Second predictor p-value",
                     "    Interaction p-value")

      Calculation <- c(" ",
                       "df1 = k",
                       "df2 = n-k-1",
                       "F_emp = (df2/df1)*(rsq/(1-rsq))",
                       "F_crit = qf(0.95, df1, df2)",
                       "p = 1-pf(F_emp, df1, df2)",
                       "R_squared = var_predicted/var_observed",
                       "f2 = rsq/(1-rsq)",

                       " ",
                       "df = n-k-1",
                       paste0("t_emp1 = ",coef1,"/",std1),
                       paste0("t_emp2 = ",coef2,"/",std2),
                       paste0("t_emp3 = ",coef3,"/",std3),
                       "t_crit = qt(0.975,df) and qt(0.025,df)",
                       "p1 = 2*(1-pt(abs(t_emp1), df))", # bitte prüfen, ob Berechnung für negative Koeffizienten stimmt
                       "p2 = 2*(1-pt(abs(t_emp2), df))",
                       "p3 = 2*(1-pt(abs(t_emp3), df))") # bitte prüfen, ob Berechnung der standardisierten Werte stimmt

      p1 <- ggplot2::ggplot(df, aes(x = df[,2], y = df[,1])) + # scatterplot Y and X1
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(y = depname, x = coef1name) +
        ggplot2::theme_minimal()
      dplot1 <- ggExtra::ggMarginal(p1, type = "boxplot") # ggMarginal adds marginal plot (here: boxplot) to ggplot2 scatterplot

      p2 <- ggplot2::ggplot(df, aes(x = df[,3], y = df[,1])) + # scatterplot Y and X2
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(y = depname, x = coef2name) +
        ggplot2::theme_minimal()
      dplot2 <- ggExtra::ggMarginal(p2, type = "boxplot")

      p3 <- ggplot2::ggplot(df, aes(x = df[,3], y = df[,2])) + # scatterplot X1 and X2
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(y = coef1name, x = coef2name) +
        ggplot2::theme_minimal()
      dplot3 <- ggExtra::ggMarginal(p3, type = "boxplot")

      p4 <- ggplot2::ggplot(df, aes(x = df[,2]*df[,3], y = df[,1])) + # scatterplot Y and X3 (interaction term)
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(y = depname, x = coef3name) +
        ggplot2::theme_minimal()
      dplot4 <- ggExtra::ggMarginal(p4, type = "boxplot")

      dplot <- gridExtra::grid.arrange(dplot1, dplot2, dplot3, dplot4, nrow = 2)

      expl <- data.frame( # dataframe containing explained variance
        Component = factor(c("Variance explained without interaction",
                             "Variance explained with interaction"),
                           levels = c("Variance explained without interaction",
                                      "Variance explained with interaction")),
        Value = c(mainrsq*100, rsq*100)
      )

      vplot <- ggplot2::ggplot(expl, aes(x = Component, y = Value, fill = Component)) + # variance explained plot (with interaction vs. without interaction)
        ggplot2::geom_bar(stat = "identity", color = "black") +
        ggplot2::scale_fill_manual(values = c("Variance explained without interaction" = "lightblue",
                                              "Variance explained with interaction" = "black")) +
        ggplot2::geom_text(aes(label = paste0(round(Value,2),"%"), y = 100),
                           color = "black") +
        ggplot2::labs(x = NULL, y = "Percentage", fill = NULL) +
        ggplot2::ggtitle("Variance explained") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       #   panel.grid = element_blank(),
                       plot.title = element_text(hjust = 0.5))
    }
    # multiple linear regression
    else {
      if (npred != 2) stop(paste0("Please specify 2 predictors and an optional interaction term."))
      rxx <- round(cor(pred1,pred2),3) # Korrelation zwischen Prädiktoren
      rx1y <- round(cor(testobj[["model"]][[1]],pred1),3) # Korrelation zwischen Prädiktor 1 und AV
      rx2y <- round(cor(testobj[["model"]][[1]],pred2),3) # Korrelation zwischen Prädiktor 2 und AV


      vars <- c("n","k","rsq","var_predicted","var_observed", # k = number predictors
                "rxx","rx1y","rx2y")
      vals <- as.character(c(nids, npred, rsq,
                             round(sdyhat^2,4), round(sdy^2,4),
                             rxx, rx1y, rx2y))
      comments <- c("sample size", "number of predictors", "R squared",
                    paste("variance of predicted",depname,"values"),
                    paste("variance of observed",depname,"values"),
                    "correlation between predictors",
                    paste("correlation between",coef1name,"and",depname),
                    paste("correlation between",coef2name,"and",depname))

      comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE) # combine into dataframe
      components <- with(comps, {
        var_fmt <- format(vars, width = max(nchar(vars)), justify = "left")
        val_fmt <- format(vals, width = max(nchar(vals)), justify = "left")
        comment_fmt <- paste0("# ", comments)

        paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
      }) # der Abschnitt ist das, was später im Output zu sehen ist


      ints <- c(" ",
                "  Key model results:",
                paste("- If all predictors have a value of zero, we predict a",depname,"value of",int, sig_list[[1]]),
                paste("- If",coef1name,"increases by one unit, we predict a change of",coef1,"units for",depname, sig_list[[2]]),
                paste("- If",coef2name,"increases by one unit, we predict a change of",coef2,"units for",depname, sig_list[[3]]),
                paste0("- The model explains ",rsq*100,"% of variance in ",depname))
      names(ints) <- " "
      ints <- format(ints, justify = "left")

      Indicator <- c("Global test results:", # the space bars are used for formatting
                     "    df1",
                     "    df2",
                     "    Empirical F-value",
                     "    Critical F-value",
                     "    F-test p-value",
                     "    Variance explained",
                     "    effect size f-squared",

                     "Regression coefficient test results:",
                     "    df",
                     "    First predictor t-value (Estimate/Std.Err.)",
                     "    Second predictor t-value (Estimate/Std.Err.)",
                     "    Critical t-values",
                     "    First predictor p-value",
                     "    Second predictor p-value",
                     "    First standardized regression coefficient",
                     "    Second standardized regression coefficient")

      Calculation <- c(" ",
                       "df1 = k",
                       "df2 = n-k-1",
                       "F_emp = (df2/df1)*(rsq/(1-rsq))",
                       "F_crit = qf(0.95, df1, df2)",
                       "p = 1-pf(F_emp, df1, df2)",
                       "R_squared = var_predicted/var_observed",
                       "f2 = rsq/(1-rsq)",

                       " ",
                       "df = n-k-1",
                       paste0("t_emp1 = ",coef1,"/",std1),
                       paste0("t_emp2 = ",coef2,"/",std2),
                       "t_crit = qt(0.975,df) and qt(0.025,df)",
                       "p1 = 2*(1-pt(abs(t_emp1), df))", # bitte prüfen, ob Berechnung für negative Koeffizienten stimmt
                       "p2 = 2*(1-pt(abs(t_emp2), df))",
                       "b1s = (rx1y-rx2y*rxx)/(1-rxx^2)", # bitte prüfen, ob Berechnung der standardisierten Werte stimmt
                       "b2s = (rx2y-rx1y*rxx)/(1-rxx^2)") # bitte prüfen, ob Berechnung der standardisierten Werte stimmt

      p1 <- ggplot2::ggplot(df, aes(x = df[,2], y = df[,1])) + # scatterplot Y and X1
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(y = depname, x = coef1name) +
        ggplot2::theme_minimal()
      dplot1 <- ggExtra::ggMarginal(p1, type = "boxplot") # ggMarginal adds marginal plot (here: boxplot) to ggplot2 scatterplot

      p2 <- ggplot2::ggplot(df, aes(x = df[,3], y = df[,1])) + # scatterplot Y and X2
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(y = depname, x = coef2name) +
        ggplot2::theme_minimal()
      dplot2 <- ggExtra::ggMarginal(p2, type = "boxplot")

      p3 <- ggplot2::ggplot(df, aes(x = df[,3], y = df[,2])) + # scatterplot X1 and X2
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(y = coef1name, x = coef2name) +
        ggplot2::theme_minimal()
      dplot3 <- ggExtra::ggMarginal(p3, type = "boxplot")

      dplot <- gridExtra::grid.arrange(dplot1, dplot2, dplot3, nrow = 2) # Hier ist noch Platz für einen Plot. Ideen?

      expl <- data.frame(
        Component = factor(c("Variance explained by predictor 1",
                             "Variance explained by predictor 2",
                             "Full model variance explained"),
                           levels = c("Variance explained by predictor 1",
                                      "Variance explained by predictor 2",
                                      "Full model variance explained")), # specifying levels so that the bars in vplot are in intended order
        Value = c(rx1y^2*100, rx2y^2*100, rsq*100)
      )

      vplot <- ggplot2::ggplot(expl, aes(x = Component, y = Value, fill = Component)) +
        ggplot2::geom_bar(stat = "identity", color = "black") +
        ggplot2::scale_fill_manual(values = c("Variance explained by predictor 1" = "lightblue",
                                              "Variance explained by predictor 2" = "steelblue",
                                              "Full model variance explained" = "black")) +
        ggplot2::geom_text(aes(label = paste0(round(Value,2),"%"), y = 100),
                           color = "black") +
        ggplot2::labs(x = NULL, y = "Percentage", fill = NULL) +
        ggplot2::ggtitle("Variance explained") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       #   panel.grid = element_blank(),
                       plot.title = element_text(hjust = 0.5))
    }

    maintab <- data.frame(Indicator, Calculation)

    rplot <- plot(testobj, which = 1) # wird automatisch geplottet, ohne dass es extra angefordert wird
    q_plot <- plot(testobj, which = 2) # wird automatisch geplottet, ohne dass es extra angefordert wird
    print(vplot)


    cat("Notes:\n",
        "- Multiple plots have been created. Use the arrows to navigate.\n",
        "- A table with calculations has been stored in the Global Environment.\n",
        sep = "")
    if (numeric == FALSE) cat("- One or more predictors have been converted to numeric variables")
    print(summary(testobj))

    cat(indiv_values, sep = "\n")

    cat(components, sep = "\n")
    cat(ints, sep = "\n")
    assign(paste0("tab_reg_",obj_name), maintab, envir = .GlobalEnv)
  }
  else if (grepl("Chi-squared", method, fixed = TRUE)) {
    nobs <- sum(testobj[["observed"]]) # sample size N
    nvars <- ifelse(length(dim(testobj[["observed"]]))> 1, # if number of dimensions is >1, it's one variable, else two variables
                    "two variables",
                    "one variable")
    dim1 <- ifelse(nvars == "one variable", # number of categories of variable 1
                   length(testobj[["observed"]]),
                   dim(testobj[["observed"]])[1])
    dim2 <- ifelse(nvars == "one variable", # number of categories of variable 2 (if we just have one variable, dim2 is NA)
                   NA,
                   dim(testobj[["observed"]])[2])
    df <- ifelse(nvars == "one variable", dim1-1, (dim1-1)*(dim2-1)) # degrees of freedom
    critval <- qchisq(0.95, df) # critical value
    empval <- testobj[["statistic"]][["X-squared"]] # empirical value

    require1 <- mean(testobj[["observed"]]> 1)*100 # Voraussetzungsprüfung: sind die beobachteten Häufigkeiten in jeder Zelle > 1?
    # hier wird mit TRUE/FALSE Abfrage in jeder Zelle gearbeitet (0 = FALSE, 1 = TRUE)
    # dann wird der Mittelwert davon genommen -> wenn mean < 1, bzw. require 1 dann < 100 ist, sind die beobachteten Häufigkeiten nicht in jeder Zelle (also 100% der Zellen) > 1
    require2 <- mean(testobj[["observed"]]> 5)*100 # Voraussetzung 2: sind die beobachteten Häufigkeiten in mind. 80% Zelle > 5?
    # zeigt an, in wie viel Prozent der Zellen der die beobachteten WSKs > 5 sind

    restab <- testobj[["residuals"]] # residuals: (testobj$observed-testobj$expected)/sqrt(testobj$expected)

    indiv_values <- c("",
                      "|------------------------------------------|",
                      "|-------- Obtain individual values:--------|",
                      "|------------------------------------------|")
    # one-sample case
    if (nvars == "one variable"){
      vars <- c("n","k","df","chisq_crit")
      vals <- as.character(c(nobs, dim1, df, round(critval,3)))
      comments <- c("sample size", "number of categories", "degrees of freedom: k-1",
                    "critical value: qchisq(1-alpha,df)") # FRAGE: ich glaube, die Berechnungen für df und kritischen Wert waren bei den anderen Tests nicht drin -> hinzufügen?

      # Descriptive statistics plot
      plotdat <- data.frame( # plot data: number of observations per category
        Categories = c(1:length(testobj[["observed"]])),
        Frequencies = as.numeric(testobj[["observed"]]))
      dplot <- ggplot(plotdat, aes(x = Categories, y = Frequencies)) + # Säulendiagramm (Häufigkeitsverteilung)
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Frequency per category",
             x = "Categories",
             y = "Frequencies") +
        theme(legend.position = "none")

      # Residual Heatmap
      resdat <- data.frame(
        Categories = c(1:length(testobj[["stdres"]])),
        Res = as.numeric(testobj[["stdres"]]))
      resdat$Label <- sprintf("%.2f", resdat$Res) # returns character vector containing rounded standard error

      rplot <- ggplot(resdat, aes(x = Categories, y = 1, fill = Res)) +
        geom_tile(color = "black") +
        geom_text(aes(label = Label), color = "black") +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red",
          midpoint = 0,
          limits = c(-3, 3),
          oob = scales::squish
        ) +
        theme_minimal() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        coord_fixed() +
        labs(fill = "Std. Residual")

    } else {
      # two-sample case
      vars <- c("n","k","l","df","chisq_crit")
      vals <- as.character(c(nobs, dim1, dim2, df, critval))
      comments <- c("sample size", "categories in variable 1",
                    "categories in variable 2", "degrees of freedom: (k-1)*(l-1)",
                    "critical value: qchisq(1-alpha,df)")

      # Descriptive statistics plot
      plotdat <- as.data.frame(testobj[["observed"]])
      colnames(plotdat) <- c("Var1", "Var2", "Frequency")
      dplot <- ggplot(plotdat, aes(x = Var1, y = Frequency, fill = Var2)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ Var2, nrow = 1) +
        theme_minimal() +
        labs(title = "Frequency per category",
             x = "Var1",
             y = "Frequency") +
        theme(strip.text = element_text(size = 12))

      # Residual Heatmap
      resdat <- as.data.frame(testobj[["stdres"]])
      colnames(resdat) <- c("Var1", "Var2", "Res")
      resdat$Label <- sprintf("%.2f", resdat$Res)
      rplot <- ggplot(resdat, aes(x = Var2, y = Var1, fill = Res)) +
        geom_tile(color = "black") +
        geom_text(aes(label = Label), color = "black") +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red",
          midpoint = 0,
          limits = c(-3, 3),
          oob = scales::squish
        ) +
        theme_minimal() +
        coord_fixed() +
        labs(fill = "Std. Residual")

    }


    comps <- data.frame(vars, vals, comments, stringsAsFactors = FALSE)
    components <- with(comps, {
      var_fmt <- format(vars, width = max(nchar(vars)), justify = "left")
      val_fmt <- format(vals, width = max(nchar(vals)), justify = "left")
      comment_fmt <- paste0("# ", comments)

      paste0(var_fmt, " <- ", val_fmt, " ", comment_fmt)
    })


    ints <- c(" ",
              "  Key model information:",
              paste("- This is a Chi square test of frequencies with",nvars),
              paste0("- Percentage of categories with frequency greater 1: ",require1,"%"),
              paste0("- Percentage of categories with frequency greater 5: ",require2,"%"))

    names(ints) <- " "
    ints <- format(ints, justify = "left")

    margintable <- addmargins(as.matrix(testobj[["observed"]])) # forms sums over all margins in the matrix -> creates contingency table

    # Plots
    # dplot and rplot have been created above

    # distribution unter H0
    lowerlimit <- 0
    upperlimit <- max(c(empval,critval))*1.2
    xaxis <- seq(lowerlimit, upperlimit, length.out = 1000)
    density <- dchisq(xaxis, df)
    data <- data.frame(x = xaxis, density = density)

    gplot <- ggplot(data, aes(x = xaxis, y = density)) +
      geom_line() +
      geom_vline(aes(xintercept = empval, color = "empirical value"), linetype = "solid") +
      geom_vline(aes(xintercept = critval, color = "critical value"), linetype = "solid") +
      ggtitle(paste0("Density of chisq~(0.95, ",df,") under the H0")) +
      xlab("x") +
      ylab("density P(x)") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))

    print(dplot)
    print(gplot)
    print(rplot)

    cat("Notes:\n",
        "- Multiple plots have been created. Use the arrows to navigate.\n",
        "                  \n",
        sep = "")

    print(testobj)

    cat(indiv_values, sep = "\n")

    cat(components, sep = "\n")

    cat(ints, sep = "\n")
    cat("\n")
    cat("Contingency Table of Observed Frequencies", sep = "\n")
    print(margintable)
  }
}



