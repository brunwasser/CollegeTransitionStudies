load( 'ctsr.RData' )
load( 'pro.RData' )


times.c1.a1 <- read.csv( 'ctsr.c1.a1.times.csv', header = T )
times.c2.a1 <- read.csv( 'ctsr.c2.a1.times.csv', header = T )
times.a1 <- rbind( times.c1.a1, times.c2.a1 )
which( duplicated( times.a1$id ) )

times.c1.a2 <- read.csv( 'ctsr.c1.a2.times.csv', header = T )
times.c2.a2 <- read.csv( 'ctsr.c2.a2.times.csv', header = T )
times.a2 <- rbind( times.c1.a2, times.c2.a2 )


times.c1.a3 <- read.csv( 'ctsr.c1.a3.times.csv', header = T )
times.c2.a3 <- read.csv( 'ctsr.c2.a3.times.csv', header = T )
times.a3 <- rbind( times.c1.a3, times.c2.a3 )

times.c1.a4 <- read.csv( 'ctsr.c1.a4.times.csv', header = T )
times.c2.a4 <- read.csv( 'ctsr.c2.a4.times.csv', header = T )
times.a4 <- rbind( times.c1.a4, times.c2.a4 )

times.c1.a5 <- read.csv( 'ctsr.c1.a5.times.csv', header = T )
times.c2.a5 <- read.csv( 'ctsr.c2.a5.times.csv', header = T )
times.a5 <- rbind( times.c1.a5, times.c2.a5 )


times <- rbind( times.a1, times.a2, times.a3, times.a4, times.a5 )
times[ times == -999 ] <- NA

times$start <- as.Date( times$starttime, format ="%d-%b-%y" )
times$end <- as.Date( times$finishtime, format ="%d-%b-%y" )


times2 <- times %>%
  group_by( id, assess ) %>%
  filter(start == max(start) )

times2 <- as.data.frame( times2 )
times2$assess <- factor( times2$assess )

ctsr$roleemot.o <- ordered( ctsr$roleemot, levels = 0:3 )
ctsr$socfunc4.o <- ordered( ctsr$socfunc4 - 5, levels = 0:3 )

ctsr1 <- subset( ctsr, !is.na( ctsr$transfer ), 
                 c( 'id','assess','week','campus','group','male','race2','parented','college','pastfin',
                    'curfin','relig','medsever','medscur','txcur','therever','thercur',
                    'txever','phq','bis','bas','transfer','roleemot','socactiv' ) 
)

ctsr1a <- merge( ctsr1, pro, by = 'id', all.x = T )

ctsr1b <- merge( ctsr1a, times2, by = c( 'id', 'assess' ) , all.x = T )

cohort <- read.csv( 'cohort.csv', header = T )

ctsr1c <- merge( ctsr1b, cohort, by = 'id' )


ids <- ctsr1b[ !duplicated( ctsr1b$id), 'id' ]

timeall <- data.frame( id = rep( ids, each = 5 ),
                       assess = rep( 1:5, length( ids ) ) 
)

ctsr.use <- merge( timeall, ctsr1c, by = c('id','assess'), all.x = T )

ctsr.use$cohort.f <- factor( ctsr.use$cohort,
                             levels = 1:2,
                             labels = c( '2010 Cohort','2011 Cohort' ) 
)

semesterStart2010 <- as.Date( '7-Sep-10', format = "%d-%b-%y" )
semesterStart2011 <- as.Date( '6-Sep-11', format = "%d-%b-%y" )

ctsr.use$days <- ifelse( ctsr.use$cohort.f == '2010 Cohort', 
                         ctsr.use$start - semesterStart2010,
                         ctsr.use$start - semesterStart2011 
)


ctsr.use$days[ ctsr.use$cohort == 1] <- ctsr.use$start[ ctsr.use$cohort == 1] - semesterStart2010 
ctsr.use$days[ ctsr.use$cohort == 2] <- ctsr.use$start[ ctsr.use$cohort == 2] - semesterStart2011 


label( ctsr.use$id ) <- 'Unique participant identifier'
label( ctsr.use$week ) <- 'Week of the semester (0=semester begins)'
label( ctsr.use$campus ) <- 'Does participant live on campus?'
label( ctsr.use$transfer ) <- 'Is the student a transfer?'
label( ctsr.use$group ) <- 'Transfer & campus housing status'
ctsr.use$sex <- factor( ctsr.use$male, levels = 0:1, labels = c('Female','Male') )
label( ctsr.use$male ) <- 'Participant self-reported sex'
label( ctsr.use$race2 ) <- 'Participant self-reported race'
label( ctsr.use$parented ) <- 'Level parent education'
ctsr.use$nocollege <- relevel( ctsr.use$college, ref = 'College Degree' )
label( ctsr.use$college ) <- 'Parent with a college degree?'
label( ctsr.use$pastfin ) <- 'Financial comfort growing up'
ctsr.use$curfinf <-  factor( ctsr.use$curfin, ordered = F )
ctsr.use$curfin <- relevel( ctsr.use$curfinf, ref = 'Tight but fine' )
label( ctsr.use$curfin ) <- 'Current financial comfort'
label( ctsr.use$relig ) <- 'Level of religiousness'
label( ctsr.use$medsever ) <- 'Ever taken medication for mental health?'
label( ctsr.use$medscur ) <- 'Currently take medication for mental health?'
label( ctsr.use$txcur ) <- 'Any current mental health treatment?'
label( ctsr.use$therever ) <- 'Ever had counseling or psychotherapy?'
label( ctsr.use$thercur ) <- 'Currently receive counseling or psychotherapy?'
ctsr.use$txever <- factor( ctsr.use$txever, levels = 0:1, labels = c( 'No','Yes' ) )
label( ctsr.use$txever ) <- 'Ever recive mental health treatment (medication or psychotherapy)?'
label( ctsr.use$phq ) <- 'PHQ-8 total depression score'
label( ctsr.use$procrast.t1 ) <- 'Procrastination Scale composite score'
label( ctsr.use$bis ) <- 'BIS/BAS: Behavioral inhibition composite'
label( ctsr.use$bas ) <- 'BIS/BAS: Behavioral activation composite'
label( ctsr.use$start ) <- 'Date Survey Started'
label( ctsr.use$start ) <- 'Date Survey Ended'

levels( ctsr.use$transfer ) <- c('First-Year','Transfer')

ctsr.use$bisct <- ctsr.use$bis - mean( ctsr$bis, na.rm = T )
ctsr.use$procrastct <- ctsr.use$procrast.t1 - mean( ctsr.use$procrast.t1, na.rm = T )

ctsr.use$campus[ ctsr.use$id == 29 ] <- 'Campus' ## participant wrote "on campus" in "other housing" explanation 
ctsr.use$campus[ ctsr.use$id == 53 ] <- 'Off Campus' ## participant wrote "living with family"
ctsr.use$campus[ ctsr.use$id == 60 ] <- 'Off Campus' ## participant wrote living off campus with family
ctsr.use$campus[ ctsr.use$id == 90 ] <- 'Campus' ## participant wrote "on campus apartment with roommates" in "other housing" explanation 
ctsr.use$campus[ ctsr.use$id == 216 ] <- 'Campus' ## participant wrote living in campus organization house "other housing" explanation 
ctsr.use$campus[ ctsr.use$id == 295 ] <- 'Off Campus' ## participant wrote "off campus with husband" in "other housing" explanation 
ctsr.use$campus[ ctsr.use$id == 369 ] <- 'Off Campus' ## participant wrote off campus in "other housing" explanation 
ctsr.use$campus[ ctsr.use$id == 370 ] <- 'Off Campus' ## participant wrote "off campus with husband" in "other housing" explanation 

ctsr.use$parented[ ctsr.use$id == 38 ] <- "Post-college degree" ## DVM
ctsr.use$parented[ ctsr.use$id == 47 ] <- "Some college" ## Technical school
ctsr.use$parented[ ctsr.use$id == 235 ] <- "Post-college degree" ##  Postdoc
ctsr.use$parented[ ctsr.use$id == 255 ] <- "College degree" ## Associated degree

ctsr.use$college[ ctsr.use$id == 38 ] <- "College Degree" ## DVM
ctsr.use$college[ ctsr.use$id == 47 ] <- "No College Degree" ## Technical school
ctsr.use$college[ ctsr.use$id == 235 ] <- "College Degree" ##  Postdoc
ctsr.use$college[ ctsr.use$id == 255 ] <- "College Degree" ## Associated degree

ctsr.use$race2[ ctsr.use$id == 34 ] <- 'Other' # Multiracial
ctsr.use$race2[ ctsr.use$id == 101 ] <- 'Other' # Multiracial
ctsr.use$race2[ ctsr.use$id == 231 ] <- 'Other' # Multiracial
ctsr.use$race2[ ctsr.use$id == 268 ] <- 'Other' # Multiracial
ctsr.use$race2[ ctsr.use$id == 291 ] <- 'Other' # Multiracial

ctsr.use$transfer.campus <- NULL
ctsr.use$transfer.campus[ ctsr.use$transfer == 'First-Year' ] <- 1
ctsr.use$transfer.campus[ ctsr.use$transfer == 'Transfer' & ctsr.use$campus == 'Campus' ] <- 2
ctsr.use$transfer.campus[ ctsr.use$transfer == 'Transfer' & ctsr.use$campus == 'Off Campus' ] <- 3
ctsr.use$transfer.campus.f <- factor( ctsr.use$transfer.campus, levels = 1:3, labels = c('First-Year','Transfer: On Campus','Transfer: Off Campus' ) )
label( ctsr.use$transfer.campus.f ) <- 'Student Type' 

ctsr.use$weeks <- ctsr.use$days/7
label( ctsr.use$weeks ) <- 'Week of the semester'

#save( ctsr, file = 'ctsr.RData' )
save( ctsr.use, file = 'ctsr.use.RData' )

