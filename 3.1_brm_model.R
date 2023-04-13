
require(brms)
require(ggplot2)

load('brmdat.rda')

# Means
tapply(brmdat$Accuracy, list(brmdat$Model, brmdat$Data), mean)
#          Raw     Rand    Gauss
# MBL 62.91850 40.91450 29.46225
# WH  29.95625 26.72475 26.83425
# TD  29.95625 26.96025 26.83425

#################################################
### Bayesian Quantile Mixed Effect Regression ###
#################################################

summary(brm1.q <- brm(bf(Accuracy ~
    Model * Data +
    (1+Model|Phone),
    quantile=0.5),
    data=brmdat,
    family = asym_laplace()))
# Group-Level Effects: 
# ~Phone (Number of levels: 40) 
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)             14.47      2.09    10.69    19.03 1.00     1016     1811
# sd(ModelWH)               16.31      2.34    12.11    21.21 1.01      576     1090
# sd(ModelTD)               16.35      2.33    12.12    21.28 1.01      639      916
# cor(Intercept,ModelWH)    -0.51      0.14    -0.74    -0.20 1.01      684     1276
# cor(Intercept,ModelTD)    -0.50      0.14    -0.72    -0.19 1.01      646     1208
# cor(ModelWH,ModelTD)       0.99      0.01     0.97     1.00 1.00     2319     2878
# 
# Population-Level Effects: 
#                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept            60.59      3.10    54.43    66.85 1.00      742     1333
# ModelWH             -32.57      3.90   -40.05   -24.97 1.00      757     1510
# ModelTD             -32.49      3.91   -40.10   -25.02 1.00      770     1369
# DataRand            -20.18      2.62   -25.36   -15.02 1.00     1240     2226
# DataGauss           -31.30      2.57   -36.38   -26.40 1.00     1246     2026
# ModelWH:DataRand     18.94      3.52    11.88    25.75 1.00     1392     2299
# ModelTD:DataRand     18.98      3.51    12.15    25.84 1.00     1436     2133
# ModelWH:DataGauss    30.55      3.42    23.91    37.29 1.00     1411     2401
# ModelTD:DataGauss    30.51      3.54    23.79    37.59 1.00     1310     2412
# 
# Family Specific Parameters: 
#          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma        4.19      0.26     3.72     4.73 1.00     3175     2671
# quantile     0.50      0.00     0.50     0.50   NA       NA       NA

########################################
### Comparisons / Hypotheses testing ###
########################################

### 0 ### MBL > ECL
hy0.q = '((Intercept+(Intercept+DataRand)+(Intercept+DataGauss))/3) >
    (((Intercept+ModelWH)+(Intercept+ModelWH+DataRand+ModelWH:DataRand)+
        (Intercept+ModelWH+DataGauss+ModelWH:DataGauss)+
     (Intercept+ModelTD)+(Intercept+ModelTD+DataRand+ModelTD:DataRand)+
        (Intercept+ModelTD+DataGauss+ModelTD:DataGauss))/6)'
hypothesis(brm1.q, hy0.q, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (((Intercept+(Int... > 0    16.03      3.09     9.74    21.97        Inf         1    *

### 1 ### MBL Raw > Rand
hy1.q = '0 > DataRand'
hypothesis(brm1.q, hy1.q, alpha=0.025)
#           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (0)-(DataRand) > 0    20.18      2.62    15.02    25.36        Inf         1    *

### 2 ### MBL Rand > Gauss
hy2.q = 'DataRand > DataGauss'
hypothesis(brm1.q, hy2.q, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (DataRand)-(DataG... > 0    11.13      2.61     6.06    16.44        Inf         1    *

### 3 ### For Gaussian samples, differences are non-significant
hy3.q = '(Intercept+DataGauss) > (Intercept+ModelWH+DataGauss+ModelWH:DataGauss)'
hypothesis(brm1.q, hy3.q, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 ((Intercept+DataG... > 0     2.02      3.62    -5.43     9.23       2.56      0.72     

############
### Plot ###
############

cond_eff.q = conditional_effects(brm1.q, 'Model:Data')
cond_eff.q = as.data.frame(cond_eff.q$`Model:Data`[,c(1,2,8,10,11)])
colnames(cond_eff.q)[3:5] = c('Estimate', 'lo95CI', 'hi95CI')
# cond_eff.q
#   Model  Data Estimate   lo95CI   hi95CI
# 1   MBL   Raw 60.62419 54.42630 66.84792
# 2   MBL  Rand 40.37570 34.26873 46.57345
# 3   MBL Gauss 29.23677 23.31829 35.38128
# 4    WH   Raw 28.04706 21.77859 34.23135
# 5    WH  Rand 26.78813 21.15928 32.38713
# 6    WH Gauss 27.24138 21.95655 32.64605
# 7    TD   Raw 28.01653 21.81706 34.44509
# 8    TD  Rand 26.91414 21.44616 32.40428
# 9    TD Gauss 27.32748 21.85485 32.76000

# fixing the X-axis
cond_eff.q$Data = as.numeric(c(0.8, 1.8, 2.8, 1.0, 2.0, 3.0, 1.2, 2.2, 3.2))

p <- ggplot(data=cond_eff.q,
    aes(x=Data, y=Estimate, group=Model)) +
    coord_cartesian(ylim=c(2,70)) +
    geom_point(aes(shape=Model, colour=Model), size=3) +
    geom_errorbar(width=.12, linewidth=.5,
        aes(ymin=lo95CI, ymax=hi95CI, colour=Model)) +
    geom_hline(yintercept=2.5, linetype='dotted') +
    geom_hline(yintercept=9.51, linetype='dashed') +
    scale_colour_brewer('Learning model:', palette='Dark2',
        breaks=c('MBL', 'WH', 'TD'),
        labels=c('Memory-Based', 'Widrow-Hoff', 'Temporal Difference')) +
    scale_x_continuous('Input data',
        breaks=c(1, 2, 3), labels=c('Raw', 'Random', 'Gaussian')) +
    scale_y_continuous('Success (%)') +
    guides(shape='none') +
    theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=10)) +
    theme_classic()

plot(p)


