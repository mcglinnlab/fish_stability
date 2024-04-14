library(piecewiseSEM)
# https://cran.r-project.org/web//packages/piecewiseSEM/vignettes/piecewiseSEM.html
dat <- data.frame(x1 = runif(50), y1 = runif(50), y2 = runif(50), y3 = runif(50))
model <- psem(lm(y1 ~ x1, dat), lm(y1 ~ y2, dat), lm(y2 ~ x1, dat), lm(y3 ~ y1, dat))
model <- psem(lm(y1 ~ x1 + y2, dat), lm(y2 ~ x1, dat), lm(y3 ~ y1, dat))
summary(model, .progressBar = F)


# how to adapt to moddat_S dataset - 10/15/23
  #models included
  

dat <- with(moddat_S, data.frame(y1 = F_bio, y2=F_stab, x1=Srich, x2=Sasym, x3=Shill,
                                 x5 = tempS, x6= salS))

model <- psem(lm(y1 ~ x2 + x5 + x6, dat), lm(y2~ x2 +x5 +x6, dat))
summary(model,.progressbar = T)

basisSet(model)
fisherC(model)


library(lavaan)
library(semPlot)
# https://lavaan.ugent.be/tutorial/mediation.html
#https://tutorials.methodsconsultants.com/posts/structural-equation-models-using-the-lavaan-package-in-r/
# for interpretation of fit statistics see: 
# https://m-clark.github.io/sem/sem.html


model1 <- ' # direct effect
             y1 ~ c*x2
           # mediator
             x5 ~ a*x2
             x6 ~ a*x2
             y1 ~ b*x5
             y1 ~ d*x6
           # indirect effect (a*b)
             abd := a*b*d
           # total effect
             total := c + (a*b*d)
         '
fit <- sem(model1, data = dat)
summary(fit, fit=T, standardized=T, rsquare=T)

semPaths(fit, 'std', layout='tree2')


with(dat, summary(lm(y1 ~ x2 + x5 + x6)))

model_dir <- 'y1 ~ x2 + x5 + x6'
fit_dir <- sem(model_dir, data = dat)
summary(fit_dir)

# compare true model with direct only model

AIC(fit)
AIC(fit_dir)

anova(fit, fit_dir)


# path analysis setup
# include intercept explicitly
model2 <- 'y1 ~ 1 + x2 + x5 + x6'

results <- sem(model2, data = dat)
summary(results, standardized=T, fit=T, rsquare=T)

# this returns the same result as the linear model on the simulated data

# let's add in an indirect effect of x2 on y via x1

x1 <- 1 + 100 * x2 + rnorm(100, 0, 10)

y <- 10 + 0.3 * x1 - 25 * x2 + rnorm(100, 0, 1)

plot(x1 ~ x2)
plot(y ~ x1)
plot(y ~ x2)

mod <- lm(y ~ x1 + x2)
summary(mod)

# again try no indirect effects model

model <- 'y ~ 1 + x1 + x2'

results <- sem(model, data = dat)
summary(results, standardized=T, fit=T, rsquare=T)

# now add indirect effect

model2 <- '
y ~ 1 + a*x1 + b*x2
indirect := a #the := indicates a defined var
direct := c
total := c + a*b'

model2<-'
int ~ att + sn + a*pbc #precede the variable w/path name
beh ~ b*int + c*pbc

indirect := a*b #the := indicates a defined var
direct := c
total := c + a*b
'

