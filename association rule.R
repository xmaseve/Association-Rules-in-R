library(magrittr)
library(dplyr)
library(WDI)
library(arules)

indicator.codes = c("BM.KLT.DINV.GD.ZS",
                    "BX.KLT.DINV.WD.GD.ZS",
                    "BX.TRF.PWKR.DT.GD.ZS",
                    "CM.MKT.LCAP.GD.ZS",
                    "CM.MKT.TRAD.GD.ZS",
                    "FD.AST.PRVT.GD.ZS",
                    "FM.LBL.BMNY.GD.ZS",
                    "FP.CPI.TOTL",
                    "FR.INR.RINR",
                    "FR.INR.RISK",
                    "FS.AST.CGOV.GD.ZS",
                    "FS.AST.DOMO.GD.ZS",
                    "FS.AST.DOMS.GD.ZS",
                    "FS.LBL.LIQU.GD.ZS")
df <- WDI(indicator=indicator.codes)

df$country = factor(df$country)
df$iso2c=factor(df$iso2c)
df$year=factor(df$year)
summary(df[indicator.codes])

make_factor_4levels = function(vec) {  
  quantile(vec,
           c(0.25, 0.50, 0.75),
           na.rm=TRUE  
  ) %>%
  { c(-Inf, ., Inf) } %>%
    cut(vec, 
        breaks=., 
        labels=c('Q1','Q2','Q3','Q4')  
    )
}



df %>%
  mutate(
    BM.KLT.DINV.GD.ZS.F   = make_factor_4levels(BM.KLT.DINV.GD.ZS),
    BX.KLT.DINV.WD.GD.ZS.F    = make_factor_4levels(BX.KLT.DINV.WD.GD.ZS),
    BX.TRF.PWKR.DT.GD.ZS.F    = make_factor_4levels(BX.TRF.PWKR.DT.GD.ZS),
    CM.MKT.LCAP.GD.ZS.F       = make_factor_4levels(CM.MKT.LCAP.GD.ZS), 
    CM.MKT.TRAD.GD.ZS.F   = make_factor_4levels(CM.MKT.TRAD.GD.ZS),
    FD.AST.PRVT.GD.ZS.F       = make_factor_4levels(FD.AST.PRVT.GD.ZS),
    FM.LBL.BMNY.GD.ZS.F       = make_factor_4levels(FM.LBL.BMNY.GD.ZS),
    FP.CPI.TOTL.F    = make_factor_4levels(FP.CPI.TOTL),
    FR.INR.RINR.F    = make_factor_4levels(FR.INR.RINR),
    FR.INR.RISK.F = make_factor_4levels(FR.INR.RISK),
    FS.AST.CGOV.GD.ZS.F       = make_factor_4levels(FS.AST.CGOV.GD.ZS),
    FS.AST.DOMO.GD.ZS.F    = make_factor_4levels(FS.AST.DOMO.GD.ZS),
    FS.AST.DOMS.GD.ZS.F = make_factor_4levels(FS.AST.DOMS.GD.ZS),
    FS.LBL.LIQU.GD.ZS.F    = make_factor_4levels(FS.LBL.LIQU.GD.ZS)
  ) %>%
  { . } -> df.arules

str(df.arules)
str(df.arules[18:31])

rules = apriori(df.arules[18:31], parameter=list(maxlen=2,minlen=2,support=0.01,confidence=0.01))
summary(rules)


rules %>%
  subset(lift>1) %>% # filter the rules
  sort(by='lift') %>% # sort the rules
  { .[1:20] } %>% # display only some of the rules
  inspect() # display the rules

rules %>%
  subset(lift>1) %>% # filter the rules
  sort(by='confidence') %>% # sort the rules
  { .[1:20] } %>% # display only some of the rules
  inspect() # display the rules

inspect(rules[3])

#lhs                         rhs                         support    confidence lift    
#3 {FS.LBL.LIQU.GD.ZS.F=Q2} => {BX.TRF.PWKR.DT.GD.ZS.F=Q4} 0.01036866 0.5        2.458924
#The support is 0.01036866, which means 1.036866% of total countries whose the sesond quartile of Liquid liabilities (M3) as % of GDP
#and  
df.arules %>%
  filter(FS.LBL.LIQU.GD.ZS.F=='Q2') %>%
  summarize(mean(FP.CPI.TOTL, na.rm=TRUE), 
            mean(FR.INR.RINR, na.rm=TRUE),
            mean(CM.MKT.TRAD.GD.ZS, na.rm=TRUE))
#mean(FP.CPI.TOTL, na.rm = TRUE) mean(FR.INR.RINR, na.rm = TRUE) mean(CM.MKT.TRAD.GD.ZS, na.rm = TRUE)
#1                    91.52558       6.442274                              6.967775
                   #90.92            6.682                                  46.7799

df.arules %>%
  filter(FS.LBL.LIQU.GD.ZS.F=='Q2',BX.TRF.PWKR.DT.GD.ZS.F=='Q4') %>%
  summarize(mean(FP.CPI.TOTL, na.rm=TRUE), 
            mean(FR.INR.RINR, na.rm=TRUE),
            mean(CM.MKT.TRAD.GD.ZS, na.rm=TRUE))
#mean(FP.CPI.TOTL, na.rm = TRUE) mean(FR.INR.RINR, na.rm = TRUE) mean(CM.MKT.TRAD.GD.ZS, na.rm = TRUE)
#1             92.98872                        8.807522                           0.004185611

inspect(rules[5])
#lhs                         rhs                      support   confidence lift    
#5 {FS.LBL.LIQU.GD.ZS.F=Q3} => {FM.LBL.BMNY.GD.ZS.F=Q3} 0.0109447 0.5428571  4.362963
df.arules %>%
  filter(FS.LBL.LIQU.GD.ZS.F=='Q3',FM.LBL.BMNY.GD.ZS.F=='Q3') %>%
  summarize(mean(FP.CPI.TOTL, na.rm=TRUE), 
            mean(FR.INR.RINR, na.rm=TRUE),
            mean(CM.MKT.TRAD.GD.ZS, na.rm=TRUE))
#mean(FP.CPI.TOTL, na.rm = TRUE) mean(FR.INR.RINR, na.rm = TRUE) mean(CM.MKT.TRAD.GD.ZS, na.rm = TRUE)
#1                        89.54673                         4.01372                              12.36064

df.arules %>%
  filter(FS.LBL.LIQU.GD.ZS.F=='Q3') %>%
  summarize(mean(FP.CPI.TOTL, na.rm=TRUE), 
            mean(FR.INR.RINR, na.rm=TRUE),
            mean(CM.MKT.TRAD.GD.ZS, na.rm=TRUE))
#mean(FP.CPI.TOTL, na.rm = TRUE) mean(FR.INR.RINR, na.rm = TRUE) mean(CM.MKT.TRAD.GD.ZS, na.rm = TRUE)
#1                        90.97186                        4.658291                              35.40572

#In the first rule, the support is 0.1134793, which means 11.34793% of total countries whose the first quartile of Domestic credit provided by financial sector (% of GDP) and the first quartile of Domestic credit to private sector by banks (% of GDP) occur together. The confidence of 0.9078341 indicates there is 90.78341% of the first quartile of Domestic credit to private sector by banks (% of GDP) in the countries which also contains the first quartile of Domestic credit provided by financial sector (% of GDP). The probability of finding the first quartile of Domestic credit to private sector by banks (% of GDP) in the countries which contain the first quartile of Domestic credit provided by financial sector (% of GDP) is greater than the normal probability of finding the first quartile of Domestic credit to private sector by banks (% of GDP) in the countries by 626.2673%.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          For the second rule, X is the first quartile of Domestic credit to private sector by banks (% of GDP), Y is the first quartile of Domestic credit provided by financial sector (% of GDP). Thus it is just the opposite way to explain the support, confidence and lift.






