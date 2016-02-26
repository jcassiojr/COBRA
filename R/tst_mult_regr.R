# exemplo de regressão multipla
# Multiple Linear Regression Example
# created by John M. Quick
# http://www.johnmquick.com
# November 26, 2009

#read data into variable
    #datavar <- read.csv("dataset_enrollmentForecast.csv")
datavar <- read.xlsx2("./data/tst_mult_corr.xlsx",1)    
datavar <- 
    datavar %>%
        mutate(UNEM = as.numeric(as.character(UNEM)),
               YEAR = as.numeric(as.character(YEAR)),
               ROLL = as.numeric(as.character(ROLL)),
               HGRAD = as.numeric(as.character(HGRAD)),
               INC = as.numeric(as.character(INC)))
    #attach data variable
    attach(datavar)
    
    #display all data
    datavar
    #two predictor model
    #create a linear model using lm(FORMULA, DATAVAR)
    #predict the fall enrollment (ROLL) using the unemployment rate (UNEM) and number of spring high school graduates (HGRAD)
    twoPredictorModel <- lm(ROLL ~ UNEM + HGRAD, datavar)
    #display model
    twoPredictorModel
    
    #Call:
    #    lm(formula = ROLL ~ UNEM + HGRAD, data = datavar)
    
    #Coefficients:
    #    (Intercept)         UNEM        HGRAD  
    #-8255.7511     698.2681       0.9423  
    
    #what is the expected fall enrollment (ROLL) given this year's unemployment rate (UNEM) of 9% 
    # and spring high school graduating class (HGRAD) of 100,000
    -8255.8 + 698.2 * 9 + 0.9 * 100000
    #[1] 88028
    #the predicted fall enrollment, given a 9% unemployment rate and 100,000 student 
    # spring high school graduating class, is 88,028 students.
        
    #three predictor model
    #create a linear model using lm(FORMULA, DATAVAR)
    #predict the fall enrollment (ROLL) using the unemployment rate (UNEM), number of 
    # spring high school graduates (HGRAD), and per capita income (INC)
    threePredictorModel <- lm(ROLL ~ UNEM + HGRAD + INC, datavar)
    #display model
    threePredictorModel
    
    #Call:
    #    lm(formula = ROLL ~ UNEM + HGRAD + INC, data = datavar)
    
    #Coefficients:
    #    (Intercept)         UNEM        HGRAD          INC  
    #-9153.2545     450.1245       0.4065       4.2749  
    
    #what is the expected fall enrollment (ROLL) given this year's unemployment rate (UNEM) of 9%,
    # spring high school graduating class (HGRAD) of 100,000, and a per capita income (INC) of $30,000
    -9153.3 + 450.1 * 9 + 0.4 * 100000 + 4.3 * 30000
    # [1] 163897.6
    
    #generate model summaries
    summary(twoPredictorModel)
    
    #Call:
    #    lm(formula = ROLL ~ UNEM + HGRAD, data = datavar)
    
    #Residuals:
    #    Min      1Q  Median      3Q     Max 
    #-2102.2  -861.6  -349.4   374.5  3603.5 
    
    #Coefficients:
    #    Estimate Std. Error t value Pr(>|t|)    
    #(Intercept) -8.256e+03  2.052e+03  -4.023  0.00044 ***
    #    UNEM         6.983e+02  2.244e+02   3.111  0.00449 ** 
    #    HGRAD        9.423e-01  8.613e-02  10.941 3.16e-11 ***
    #    ---
    #    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    
    #Residual standard error: 1313 on 26 degrees of freedom
    #Multiple R-squared: 0.8489,	Adjusted R-squared: 0.8373 
    #F-statistic: 73.03 on 2 and 26 DF,  p-value: 2.144e-11 
    
    summary(threePredictorModel)
    
    #Call:
    #    lm(formula = ROLL ~ UNEM + HGRAD + INC, data = datavar)
    
    #Residuals:
    #    Min        1Q    Median        3Q       Max 
    #-1148.840  -489.712    -1.876   387.400  1425.753 
    
    #Coefficients:
    #    Estimate Std. Error t value Pr(>|t|)    
    #(Intercept) -9.153e+03  1.053e+03  -8.691 5.02e-09 ***
    #    UNEM         4.501e+02  1.182e+02   3.809 0.000807 ***
    #    HGRAD        4.065e-01  7.602e-02   5.347 1.52e-05 ***
    #    INC          4.275e+00  4.947e-01   8.642 5.59e-09 ***
    #    ---
    #    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    
    #Residual standard error: 670.4 on 25 degrees of freedom
    #Multiple R-squared: 0.9621,	Adjusted R-squared: 0.9576 
    #F-statistic: 211.5 on 3 and 25 DF,  p-value: < 2.2e-16 