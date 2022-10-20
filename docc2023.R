load("~/GitHub/CollegeTransitionStudies/ctsr.use.RData")
load( 'G:/My Drive/PrevSciLab/Students/PhdStudents/NicoleK/TransitionStudy/ctsr.use1.RData')
abs <- read.csv( 'ctsr.abs.csv', header = T )
abs[ abs == -999 ] <- NA


# 3, 9, 15, 17, 19


require( lavaan )


sel <-  c(1:22, 92:94 )
abs1 <- subset( abs, select = sel )

abswide <- reshape( abs1, 
                    direction = 'wide',
                    idvar = 'id',
                    v.names = colnames( abs1 )[3:25],
                    timevar = 'assess',
                    sep = '.'
)



config <- '
sa2 =~ l1*1*abs3.2 + l2a*abs17.2 + l3a*abs19.2 
sa3 =~ l1*1*abs3.3 + l2b*abs17.3 + l3b*abs19.3 
sa4 =~ l1*1*abs3.4 + l2c*abs17.4 + l3c*abs19.4
sa5 =~ l1*1*abs3.5 + l2d*abs17.5 + l3d*abs19.5
#
sa2 ~~ sa2 + sa3 + sa4 + sa5
sa3 ~~ sa3 + sa4 + sa5
sa4 ~~ sa4 + sa5
sa5 ~~ sa5
#
sa2 ~ 0*1
sa3 ~ 1
sa4 ~ 1
sa5 ~ 1
#
abs3.2 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3a*t3 + abs3.t4a*t4  
abs3.3 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3b*t3 + abs3.t4b*t4  
abs3.4 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3c*t3 + abs3.t4c*t4  
abs3.5 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3d*t3 + abs3.t4d*t4  
abs17.2 | abs17.t1*t1 + abs17.t2a*t2 + abs17.t3a*t3 + abs17.t4a*t4  
abs17.3 | abs17.t1*t1 + abs17.t2b*t2 + abs17.t3b*t3 + abs17.t4b*t4  
abs17.4 | abs17.t1*t1 + abs17.t2c*t2 + abs17.t3c*t3 + abs17.t4c*t4  
abs17.5 | abs17.t1*t1 + abs17.t2d*t2 + abs17.t3d*t3 + abs17.t4d*t4  
abs19.2 | abs19.t1*t1 + abs19.t2a*t2 + abs19.t3a*t3 + abs19.t4a*t4  
abs19.3 | abs19.t1*t1 + abs19.t2b*t2 + abs19.t3b*t3 + abs19.t4b*t4  
abs19.4 | abs19.t1*t1 + abs19.t2c*t2 + abs19.t3c*t3 + abs19.t4c*t4  
abs19.5 | abs19.t1*t1 + abs19.t2d*t2 + abs19.t3d*t3 + abs19.t4d*t4  
#
abs3.2 + abs17.2 + abs19.2 ~ 0*1
abs3.3 + abs17.3 + abs19.3  ~ 0*1
abs3.4 + abs17.4 + abs19.4  ~ 0*1
abs3.5 + abs17.5 + abs19.5  ~ 0*1
#
abs3.2 ~~ 1*abs3.2
abs17.2 ~~ 1*abs17.2
abs19.2 ~~ 1*abs19.2
abs3.3 ~~ NA*abs3.3
abs17.3 ~~ NA*abs17.3
abs19.3 ~~ NA*abs19.3
abs3.4 ~~ NA*abs3.4
abs17.4 ~~ NA*abs17.4
abs19.4 ~~ NA*abs19.4
abs3.5 ~~ NA*abs3.5
abs17.5 ~~ NA*abs17.5
abs19.5 ~~ NA*abs19.5
#
abs3.2 ~~ abs3.3 + abs3.4 + abs3.5
abs3.3 ~~ abs3.4 + abs3.5
abs3.4 ~~ abs3.5
abs17.2 ~~ abs17.3 + abs17.4 + abs17.5
abs17.3 ~~ abs17.4 + abs17.5
abs17.4 ~~ abs17.5
abs19.2 ~~ abs19.3 + abs19.4 + abs19.5
abs19.3 ~~ abs19.4 + abs19.5
abs19.4 ~~ abs19.5
'


summary( config.fit <- cfa( config, 
                          data = abswide,
                          ordered = c( 'abs3.2','abs17.2','abs19.2',
                                       'abs3.3','abs17.3','abs19.3',
                                       'abs3.4','abs17.4','abs19.4',
                                       'abs3.5','abs17.5','abs19.5'),
                          parameterization = "theta", 
                          estimator = "wlsmv"#,
                          #missing = "pairwise"
                          ), fit.measures = T 
         )





load <- '
sa2 =~ l1*1*abs3.2 + l2*abs17.2 + l3*abs19.2 
sa3 =~ l1*1*abs3.3 + l2*abs17.3 + l3*abs19.3 
sa4 =~ l1*1*abs3.4 + l2*abs17.4 + l3*abs19.4
sa5 =~ l1*1*abs3.5 + l2*abs17.5 + l3*abs19.5
#
sa2 ~~ sa2 + sa3 + sa4 + sa5
sa3 ~~ sa3 + sa4 + sa5
sa4 ~~ sa4 + sa5
sa5 ~~ sa5
#
sa2 ~ 0*1
sa3 ~ 1
sa4 ~ 1
sa5 ~ 1
#
abs3.2 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3a*t3 + abs3.t4a*t4  
abs3.3 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3b*t3 + abs3.t4b*t4  
abs3.4 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3c*t3 + abs3.t4c*t4  
abs3.5 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3d*t3 + abs3.t4d*t4  
abs17.2 | abs17.t1*t1 + abs17.t2a*t2 + abs17.t3a*t3 + abs17.t4a*t4  
abs17.3 | abs17.t1*t1 + abs17.t2b*t2 + abs17.t3b*t3 + abs17.t4b*t4  
abs17.4 | abs17.t1*t1 + abs17.t2c*t2 + abs17.t3c*t3 + abs17.t4c*t4  
abs17.5 | abs17.t1*t1 + abs17.t2d*t2 + abs17.t3d*t3 + abs17.t4d*t4  
abs19.2 | abs19.t1*t1 + abs19.t2a*t2 + abs19.t3a*t3 + abs19.t4a*t4  
abs19.3 | abs19.t1*t1 + abs19.t2b*t2 + abs19.t3b*t3 + abs19.t4b*t4  
abs19.4 | abs19.t1*t1 + abs19.t2c*t2 + abs19.t3c*t3 + abs19.t4c*t4  
abs19.5 | abs19.t1*t1 + abs19.t2d*t2 + abs19.t3d*t3 + abs19.t4d*t4  
#
abs3.2 + abs17.2 + abs19.2 ~ 0*1
abs3.3 + abs17.3 + abs19.3  ~ 0*1
abs3.4 + abs17.4 + abs19.4  ~ 0*1
abs3.5 + abs17.5 + abs19.5  ~ 0*1
#
abs3.2 ~~ 1*abs3.2
abs17.2 ~~ 1*abs17.2
abs19.2 ~~ 1*abs19.2
abs3.3 ~~ NA*abs3.3
abs17.3 ~~ NA*abs17.3
abs19.3 ~~ NA*abs19.3
abs3.4 ~~ NA*abs3.4
abs17.4 ~~ NA*abs17.4
abs19.4 ~~ NA*abs19.4
abs3.5 ~~ NA*abs3.5
abs17.5 ~~ NA*abs17.5
abs19.5 ~~ NA*abs19.5
#
abs3.2 ~~ abs3.3 + abs3.4 + abs3.5
abs3.3 ~~ abs3.4 + abs3.5
abs3.4 ~~ abs3.5
abs17.2 ~~ abs17.3 + abs17.4 + abs17.5
abs17.3 ~~ abs17.4 + abs17.5
abs17.4 ~~ abs17.5
abs19.2 ~~ abs19.3 + abs19.4 + abs19.5
abs19.3 ~~ abs19.4 + abs19.5
abs19.4 ~~ abs19.5
'


summary( load.fit <- cfa( load, 
                            data = abswide,
                            ordered = c( 'abs3.2','abs17.2','abs19.2',
                                         'abs3.3','abs17.3','abs19.3',
                                         'abs3.4','abs17.4','abs19.4',
                                         'abs3.5','abs17.5','abs19.5'),
                            parameterization = "theta", 
                            estimator = "wlsmv"#,
                            #missing = "pairwise"
), fit.measures = T 
)


anova( config.fit, load.fit )





thresh <- '
sa2 =~ l1*1*abs3.2 + l2*abs17.2 + l3*abs19.2 
sa3 =~ l1*1*abs3.3 + l2*abs17.3 + l3*abs19.3 
sa4 =~ l1*1*abs3.4 + l2*abs17.4 + l3*abs19.4
sa5 =~ l1*1*abs3.5 + l2*abs17.5 + l3*abs19.5
#
sa2 ~~ sa2 + sa3 + sa4 + sa5
sa3 ~~ sa3 + sa4 + sa5
sa4 ~~ sa4 + sa5
sa5 ~~ sa5
#
sa2 ~ 0*1
sa3 ~ 1
sa4 ~ 1
sa5 ~ 1
#
abs3.2 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs3.3 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs3.4 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs3.5 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs17.2 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs17.3 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs17.4 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs17.5 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs19.2 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
abs19.3 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
abs19.4 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
abs19.5 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
#
abs3.2 + abs17.2 + abs19.2 ~ 0*1
abs3.3 + abs17.3 + abs19.3  ~ 0*1
abs3.4 + abs17.4 + abs19.4  ~ 0*1
abs3.5 + abs17.5 + abs19.5  ~ 0*1
#
abs3.2 ~~ 1*abs3.2
abs17.2 ~~ 1*abs17.2
abs19.2 ~~ 1*abs19.2
abs3.3 ~~ NA*abs3.3
abs17.3 ~~ NA*abs17.3
abs19.3 ~~ NA*abs19.3
abs3.4 ~~ NA*abs3.4
abs17.4 ~~ NA*abs17.4
abs19.4 ~~ NA*abs19.4
abs3.5 ~~ NA*abs3.5
abs17.5 ~~ NA*abs17.5
abs19.5 ~~ NA*abs19.5
#
abs3.2 ~~ abs3.3 + abs3.4 + abs3.5
abs3.3 ~~ abs3.4 + abs3.5
abs3.4 ~~ abs3.5
abs17.2 ~~ abs17.3 + abs17.4 + abs17.5
abs17.3 ~~ abs17.4 + abs17.5
abs17.4 ~~ abs17.5
abs19.2 ~~ abs19.3 + abs19.4 + abs19.5
abs19.3 ~~ abs19.4 + abs19.5
abs19.4 ~~ abs19.5
'


summary( thresh.fit <- cfa( thresh, 
                          data = abswide,
                          ordered = c( 'abs3.2','abs17.2','abs19.2',
                                       'abs3.3','abs17.3','abs19.3',
                                       'abs3.4','abs17.4','abs19.4',
                                       'abs3.5','abs17.5','abs19.5'),
                          parameterization = "theta", 
                          estimator = "wlsmv"#,
                          #missing = "pairwise"
), fit.measures = T 
)


anova( load.fit, thresh.fit )




unique <- '
sa2 =~ l1*1*abs3.2 + l2*abs17.2 + l3*abs19.2 
sa3 =~ l1*1*abs3.3 + l2*abs17.3 + l3*abs19.3 
sa4 =~ l1*1*abs3.4 + l2*abs17.4 + l3*abs19.4
sa5 =~ l1*1*abs3.5 + l2*abs17.5 + l3*abs19.5
#
sa2 ~~ sa2 + sa3 + sa4 + sa5
sa3 ~~ sa3 + sa4 + sa5
sa4 ~~ sa4 + sa5
sa5 ~~ sa5
#
sa2 ~ 0*1
sa3 ~ 1
sa4 ~ 1
sa5 ~ 1
#
abs3.2 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs3.3 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs3.4 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs3.5 | abs3.t1*t1 + abs3.t2*t2 + abs3.t3*t3 + abs3.t4*t4  
abs17.2 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs17.3 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs17.4 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs17.5 | abs17.t1*t1 + abs17.t2*t2 + abs17.t3*t3 + abs17.t4*t4  
abs19.2 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
abs19.3 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
abs19.4 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
abs19.5 | abs19.t1*t1 + abs19.t2*t2 + abs19.t3*t3 + abs19.t4*t4  
#
abs3.2 + abs17.2 + abs19.2 ~ 0*1
abs3.3 + abs17.3 + abs19.3  ~ 0*1
abs3.4 + abs17.4 + abs19.4  ~ 0*1
abs3.5 + abs17.5 + abs19.5  ~ 0*1
#
abs3.2 ~~ 1*abs3.2
abs17.2 ~~ 1*abs17.2
abs19.2 ~~ 1*abs19.2
abs3.3 ~~ 1*abs3.3
abs17.3 ~~ 1*abs17.3
abs19.3 ~~ 1*abs19.3
abs3.4 ~~ 1*abs3.4
abs17.4 ~~ 1*abs17.4
abs19.4 ~~ 1*abs19.4
abs3.5 ~~ 1*abs3.5
abs17.5 ~~ 1*abs17.5
abs19.5 ~~ 1*abs19.5
#
abs3.2 ~~ abs3.3 + abs3.4 + abs3.5
abs3.3 ~~ abs3.4 + abs3.5
abs3.4 ~~ abs3.5
abs17.2 ~~ abs17.3 + abs17.4 + abs17.5
abs17.3 ~~ abs17.4 + abs17.5
abs17.4 ~~ abs17.5
abs19.2 ~~ abs19.3 + abs19.4 + abs19.5
abs19.3 ~~ abs19.4 + abs19.5
abs19.4 ~~ abs19.5
'


summary( unique.fit <- cfa( unique, 
                            data = abswide,
                            ordered = c( 'abs3.2','abs17.2','abs19.2',
                                         'abs3.3','abs17.3','abs19.3',
                                         'abs3.4','abs17.4','abs19.4',
                                         'abs3.5','abs17.5','abs19.5'),
                            parameterization = "theta", 
                            estimator = "wlsmv"#,
                            #missing = "pairwise"
), fit.measures = T 
)


anova( thresh.fit, unique.fit )







bis <- read.csv( 'ctsr.bisbas.csv', header = T )

# 8, 13, 19, 24


bis.cfa <- '
bis.f =~ basbis8 + basbis13 + basbis19 + basbis24
'


summary( bis.cfa.fit <- cfa( bis.cfa,
                             data = bis,
                             ordered = c('basbis8','basbis13','basbis19','basbis24')
                             ), fit.measures =  T
         )


abswide

df <- merge( ctsrwide, abswide, by = 'id', all.x = T  )
table( df$pastfin.low <- ifelse( df$pastfin == 'Very poor' | 
                            df$pastfin == 'Enough but not many extras', 1, 0 )
)

#Financial Comfort Growing Up, First Gen, International, Mental History History

df$phqsev3.3 <- df$phqsev.3
df$phqsev3.3[ df$phqsev3.3 == 'Severe' | df$phqsev3.3 == 'Moderately Severe' ] <- 'Moderate'
df$phqsev3.3 <- ordered( df$phqsev3.3 )


df$phqsev3.1 <- df$phqsev.3
df$phqsev3.1[ df$phqsev3.1 == 'Severe' | df$phqsev3.1 == 'Moderately Severe' ] <- 'Moderate'
df$phqsev3.1 <- ordered( df$phqsev3.1 )



df$int.num <- as.numeric( df$int )
df$college.num <- as.numeric( df$college )
df$txev.num <- as.numeric( df$txev ) 

med <- '
exp =~ basbis8 + basbis13
med =~ abs3.2 + abs17.2 + abs19.2 
out =~ sf1.3 + l2*sf2.3 + l3*sf3.3
dep1 =~ sf1.1 + l2*sf2.1 + l3*sf3.1
sf1.3 | t1*t1
sf2.3 | t2*t1
sf3.3 | t3*t1
sf1.1 | t1*t1
sf2.1 | t2*t1
sf3.1 | t3*t1
sf1.3 + sf2.3 + sf3.3 ~ 0*1
sf1.1 + sf2.1 + sf3.1 ~ 0*1
sf1.3 ~~ 1*sf1.3
sf2.3 ~~ 1*sf2.3
sf3.3 ~~ 1*sf3.3
sf1.1 ~~ 1*sf1.1
sf2.1 ~~ 1*sf2.1
sf3.1 ~~ 1*sf3.1
sf1.1 ~~ sf1.3
sf2.1 ~~ sf2.3
sf3.1 ~~ sf3.3
out ~ b*med + c*exp + dep1 + int.num + college.num + txev.num 
med ~ a*exp + dep1 + college.num + int.num + pastfin.low + txev.num 
dep1 ~~ exp
ind := a*b
tot := a*b+c
'


summary( med.out <- sem( med,
                         data = df,
                         ordered = c( 'abs3.2','abs17.2','abs19.2',
                         'sf1.1','sf2.1','sf3.1','sf1.3','sf2.3','sf3.3' ),
                         parameterization ='theta',
                         conditional.x = T ),
         fit.measures = T)



med.vcov <- vcov( med.out )
med.vcov[ 'a', 'a']


################################################
# This code can be edited in this window and   #
# submitted to Rweb, or for faster performance #
# and a nicer looking histogram, submit        #
# directly to R.                               #
################################################
require(MASS)
a=0.485
b=1.031
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
  0.02472204, -0.0156731,
  -0.0156731, 0.2761666
),2,2)
set.seed(1)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
################################################
# The number of columns in the histogram can   #
# be changed by replacing 'FD' below with      #
# an integer value.                            #
################################################
hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect')





require( mediation )
require( rms )

dfsub <- subset( df, !is.na( df$conflict ) )
dfsub1 <- subset( dfsub, !is.na(phqsev3.3 ) )
dfsub1$conflict.ln <- log( dfsub1$conflict )

dfsub1$pastfin.low

require( QuantPsyc )

med.lm1 <- lm( conflict.ln ~ rcs( biscent, 3 )+ college.num + int.num + pastfin.low + txev.num,
                 data = dfsub1 ) 

med.lm2 <- lm( conflict.ln ~ biscent + college.num + int.num + pastfin.low + txev.num,
                 data = dfsub1 ) 
anova( med.lm1, med.lm2 )

lm.beta( med.lm2 )

out.lm1 <- lm( phqln.3 ~ conflict.ln + rcs( biscent, 3 ) + phqln.1 + int.num + college.num + txev.num,
                  data = dfsub1 )

out.lm2 <- lm( phqln.3 ~ conflict.ln +biscent + phqln.1 + int.num + college.num + txev.num,
                  data = dfsub1 )


anova( out.lm1, out.lm2 )

set.seed( 202208071 )
med.out <- mediate( model.m = med.lm2, model.y = out.lm2, treat = "biscent", mediator = "conflict.ln",
                   sims = 5000,
                   boot = T,
                   boot.ci.type = 'bca' )

save( med.out, file='med.out.RData' )

set.seed( 202208072 )
sens.out <- medsens(med.out, 
                    rho.by = 0.1, 
                    effect.type = "indirect",
                    sims = 5000 )
 )

save( sens.out, file='sens.out.RData' )





med.lm1 <- glm.nb( conflict.ln ~ rcs( biscent, 3 )+ college.num + int.num + pastfin.low + txev.num,
               data = dfsub1 ) 

med.lm2 <- lm( conflict.ln ~ biscent + college.num + int.num + pastfin.low + txev.num,
               data = dfsub1 ) 
anova( med.lm1, med.lm2


out.po1 <- polr( phqsev3.3  ~ conflict.ln + rcs( biscent, 3 ) + phqln.1 + int.num + college.num + txev.num,
               data = dfsub1 )

out.po2 <- polr( phqsev3.3 ~ conflict.ln +biscent + phqln.1 + int.num + college.num + txev.num,
               data = dfsub1 )
anova( out.po1, out.po2 )

set.seed( 202208073 )
med.out2 <- mediate( model.m = med.lm2, model.y = out.po2, treat = "biscent", mediator = "conflict.ln",
                    sims = 5000,
                    boot = T,
                    boot.ci.type = 'bca' )

save( med.out, file='med.out.RData' )