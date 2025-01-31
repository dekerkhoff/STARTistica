# Dataframe in wide format
dwide <- data.frame(cbind(g1 = rnorm(100, 1, 1), 
                          g2 = rnorm(100, 2, 1)))

# t-test using default settings (two-sided, two groups, mu = 0)
demo1 <- t.test(dwide$g1, dwide$g2)   

# paired t-test with H1: mu < mu0 = 2
demo2 <- t.test(dwide$g1, dwide$g2, 
                paired = T, 
                alternative = "less", 
                mu = 2)   

# demonstrate 
teach_t.test1(demo1) 
teach_t.test1(demo2, export = T) 

