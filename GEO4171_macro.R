library(plotrix)
library(nsRFA)
library(evir)
library(lubridate)

"lagdato"<-function(x,nn)
{

if (nn==12){	
dato<-as.integer(x/100)
til<-(x -dato*100)/nn-1/nn
dato<-dato+til

}else
{
if (nn==365){	
dato<-as.integer(x/10000)
mnd<-as.integer(x/100)
dag<-x-mnd*100
mnd<-mnd-dato*100
ndag<-0
if (mnd==2) ndag<-31
if (mnd==3) ndag<-31+28
if (mnd==4) ndag<-31+28+31
if (mnd==5) ndag<-31+28+31+30
if (mnd==6) ndag<-31+28+31+30+31
if (mnd==7) ndag<-31+28+31+30+31+30
if (mnd==8) ndag<-31+28+31+30+31+30+31
if (mnd==9) ndag<-31+28+31+30+31+30+31+31
if (mnd==10) ndag<-31+28+31+30+31+30+31+31+30
if (mnd==11) ndag<-31+28+31+30+31+30+31+31+30+31
if (mnd==12) ndag<-31+28+31+30+31+30+31+31+30+31+30



til<-(dag+ndag-1)/nn
dato<-dato+til

}else
{
dato<-x

}}
dato
	
}



"see_data"<-function(x,nn,nr=1,nc=1,ptrend=F,yunits="streamflow")
{
tkt<-colnames(x)
if(yunits=="streamflow") ylab1=expression(Streamflow (m^3/s))
else ylab1=yunits
stor<-dim(x)
ant<-stor[2]
top<-stor[1]

if(ptrend){ 
trnd<-trend(x,nn)
xh<-seq(1:2)
yh<-seq(1:2)
}

top<-as.integer(top)
maksd<-lagdato(x[top,1],nn)
mind<-lagdato(x[1,1],nn)
xverdi<-seq(from=mind,to=maksd,length=top)
par(mfrow=c(nr,nc),ask=T)
for(i in 2:ant)
{
ii<-i-1
ttt<-tkt[i]
#xx1=xverdi[!is.na(x[,i])]
#yy1 = x[!is.na(x[,i]),i]							
#plot(xx1,yy1,type='l',xlab='YEAR',ylab=ylab1)
plot(xverdi,x[,i],type='l',xlab='YEAR',ylab=ylab1)
if(ptrend){
xh[1]<-trnd[ii,1]
xh[2]<-trnd[ii,2]
yh[1]<-trnd[ii,3]
yh[2]<-trnd[ii,3]+trnd[ii,5]*(trnd[ii,2]-trnd[ii,1])
lines(xh,yh,lty=3)
}
}	
par(mfrow=c(1,1),ask=F)
}



daily_to_monthly<-function(daily,...){
nr<-nrow(daily)
nc<-ncol(daily)
months_help<-as.integer(daily[,1]/100)
months<-unique(as.integer(daily[,1]/100))
nr2<-length(months)
monthly<-matrix(nrow=nr2,ncol=nc)
monthly[,1]<-months
for (i in 1:nr2){
monthly[i,2:nc]<-mean(daily[months_help==months[i],2:nc],...)
}
colnames(monthly)<-colnames(daily)
monthly
}
	

monthly_to_annual<-function(monthly,...){
nr<-nrow(monthly)
nc<-ncol(monthly)
years_help<-as.integer(monthly[,1]/100)
years<-unique(as.integer(monthly[,1]/100))
nr2<-length(years)
annual<-matrix(nrow=nr2,ncol=nc)
annual[,1]<-years
for (i in 1:nr2){
annual[i,2:nc]<-mean(monthly[years_help==years[i],2:nc],...)
}
colnames(annual)<-colnames(monthly)
annual
}
	

monthly_to_onemonth<-function(monthly,month){
nr<-nrow(monthly)
nc<-ncol(monthly)
months_help<-monthly[,1]-as.integer(monthly[,1]/100)*100
years<-unique(as.integer(monthly[,1]/100))
nr2<-length(years)
onemonth<-monthly[months_help==month,]
onemonth[,1]<-years
colnames(onemonth)<-colnames(monthly)
onemonth
}

monthly_to_monthly_average<-function(monthly){
nc<-ncol(monthly)
nrow<-12
m.average<-matrix(ncol=nc,nrow=12)
m.average[,1]<-seq(1:12)
month<-monthly[,1]-as.integer(monthly[,1]/100)*100
for (i in 1:12){
for (j in 2:nc){
m.average[i,j]<-mean(na.omit(monthly[month==i,j]))
}
}
colnames(m.average)<-colnames(monthly)
m.average
}



"plot_seasonal"<-function(monthly,monthly2=NA,nr=1,nc=1,yunits="streamflow")
{
if (yunits=="streamflow") {ylab1=expression(Streamflow (m^3/s))}
    else {ylab1=yunits}
tkt<-colnames(monthly)
nrr<-nrow(monthly)
ncc<-ncol(monthly)
xv<-seq(1:nrr)
par(mfrow=c(nr,nc),ask=T)

for(i in 2:ncc){
ttt<-tkt[i]							
ymi<-min(monthly[,i],na.rm=TRUE)
yma<-max(monthly[,i],na.rm=TRUE)
if(any(!is.na(monthly2))) {
ymi<-min(monthly2[,i],ymi,na.rm=TRUE)
yma<-max(monthly2[,i],yma,na.rm=TRUE)
}

plot(xv,monthly[,i],type='l',xlab='MONTH',ylab=ylab1,ylim=c(ymi,yma))
	
if(any(!is.na(monthly2))) {
 lines(xv,monthly2[,i],col=2)
 legend('topleft',lty=c(1,1),col=c(1,2),legend=c("First half","Second half"))
}
}
 par(mfrow=c(1,1),ask=F)
}



plot_season_floods<-function(daily_data,qth=0.95){
top<-length(daily_data[,1])
maksd<-lagdato(daily_data[top,1],365)
mind<-lagdato(daily_data[1,1],365)
xverdi<-seq(from=mind,to=maksd,length=top)

fth<-quantile(na.omit(daily_data[,2]),qth)
fmax<-max(na.omit(daily_data[,2]))
potmax = daily_data[daily_data[,2]>fth,2]
potdates=xverdi[daily_data[,2]>fth]

polar.plot(na.omit(potmax),na.omit((potdates-floor(potdates))*360),start=90,clockwise=TRUE,
label.pos=(1:12)/6*pi,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
radial.labels=NA, line.col="blue")
dmax = as.Date(as.character(daily_data[which.max(na.omit(daily_data[,2])),1]), format="%Y%m%d")
angle_max = yday(dmax)/365 * pi/2 - pi/2
text(fmax*cos(angle_max), fmax*sin(angle_max), round(fmax), col="blue")
}



get_ams<-function(daily_data){
myyears=as.integer(daily_data[,1]/10000)
top<-length(daily_data[,1])
maksd<-lagdato(daily_data[top,1],365)
mind<-lagdato(daily_data[1,1],365)
xverdi<-seq(from=mind,to=maksd,length=top)
ylist=unique(myyears)
mymax=ylist
maxdates=ylist
maxdates_plot=ylist
for(i in 1 : length(ylist)){
mymax[i]=max(daily_data[myyears==ylist[i],2])
ix = which(daily_data[,2]==mymax[i]&myyears==ylist[i])
if (length(ix) > 1) {ix = ix[1]}
if (length(ix) == 0){
maxdates_plot[i]=NA
maxdates[i]=NA
} else {
maxdates_plot[i]=xverdi[ix]
maxdates[i]=daily_data[ix,1]
}
}
out<-list()
out$ams<-mymax[!is.na(mymax)]
out$amsdates<-maxdates[!is.na(mymax)]
out$maxdates_plot<-maxdates_plot[!is.na(mymax)]
out
}




"trend" <- function(x,nn)
{
tkt<-names(x)
nc<-ncol(x)
nr<-nrow(x)
maksd<-lagdato(x[nr,1],nn)
mind<-lagdato(x[1,1],nn)

xverdi<-seq(from=mind,to=maksd,length=nr)

# xverdi<-xverdi-min(xverdi)


coe<-matrix(nrow=nc-1, ncol=6, dimnames=list(tkt[2:nc],c("Start_date","End_date","Intercept", "S.E.", "Slope", "S.E.")))

for (i in 2:nc)
{
ii<-i-1
yy<-x[,1:2]
yy[,1]<-xverdi	
yy[,2]<-x[,i]
yy<-na.omit(yy)
coe[ii,1]<-yy[1,1]
coe[ii,2]<-yy[nrow(yy),1]
yy[,1]<-yy[,1]-yy[1,1]
xy<-yy[,2]
xx<-yy[,1]
hh<-ktrend(xx,xy)
coe[ii,3]<-hh$co[1]
coe[ii,4]<-hh$se[1]
coe[ii,5]<-hh$co[2]
coe[ii,6]<-hh$se[2]
}
return(coe)
}





"ktrend" <- function(x,y)
{
pp<-lm(y~x)
pt<-summary(pp)
pt<-coef(pt)
co<-pt[1:2]
se<-pt[3:4]
return(list(co=co,se=se))  # changed 2011
}



myprior <- function (x) {
# x = vector of parameter values: c(location, scale, shape)
# I assume the shape parameter only has a prior with mean zero and standard deviation 0.2
dnorm(x[3], 0, 0.2)
}


Bayesian_GEV_param<-function(thefit){
parest<-matrix(ncol=3,nrow=3)
colnames(parest)<-c('Location','Scale','Shape')
rownames(parest)<-c('ML','LB','UB')
parest[1,]<-thefit$parametersML
for(i in 1 : 3){
ttp<-as.vector(Myfit$parameters[,i,])
parest[2,i]<-quantile(ttp,0.025)
parest[3,i]<-quantile(ttp,0.975)
}
return(parest)
}







gev.diag.bayes<-function(thefit) 
{
    n <- length(thefit$xcont)
    x <- (1:n)/(n + 1)
        oldpar <- par(mfrow = c(2, 2))
 
     postpar<-thefit$parametersML
	 postpar[3]<-postpar[3]*(-1.0)
		
        gev.pp(postpar, thefit$xcont)
        gev.qq(postpar, thefit$xcont)
        plot(thefit,ylab='x (m3/s)')
        gev.his(postpar, thefit$xcont)
    par(oldpar)
    invisible()
}
#<environment: namespace:ismev>



gev.pp<-function (a, dat) 
{
    plot((1:length(dat))/length(dat), gevf(a, sort(dat)), xlab = "Empirical", 
        ylab = "Model", main = "Probability Plot")
    abline(0, 1, col = 4)
}

gevf<-function (a, z) 
{
    if (a[3] != 0) 
        exp(-(1 + (a[3] * (z - a[1]))/a[2])^(-1/a[3]))
    else gum.df(z, a[1], a[2])
}


gev.qq<-function (a, dat) 
{
    plot(gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat), 
        ylab = "Empirical (m3/s)", xlab = "Model (m3/s)", main = "Quantile Plot")
    abline(0, 1, col = 4)
}

 gevq<-function (a, p) 
{
    if (a[3] != 0) 
        a[1] + (a[2] * ((-log(1 - p))^(-a[3]) - 1))/a[3]
    else gum.q(p, a[1], a[2])
}

gum.df<-function (x, a, b) 
{
    exp(-exp(-(x - a)/b))
}

gum.q<-function (x, a, b) 
{
    a - b * log(-log(1 - x))
}

gev.his<-function (a, dat) 
{
    h <- hist(dat, plot = FALSE)
    if (a[3] < 0) {
        x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] - 
            0.001)), length = 100)
    }
    else {
        x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)), 
            max(h$breaks), length = 100)
    }
    y <- gev.dens(a, x)
    hist(dat, freq = FALSE, ylim = c(0, max(max(h$density), max(y))), 
        xlab = "x (m3/s)", ylab = "f(x)", main = "Density Plot")
    points(dat, rep(0, length(dat)))
    lines(x, y)
}

 gev.dens<-function (a, z) 
{
    if (a[3] != 0) 
        (exp(-(1 + (a[3] * (z - a[1]))/a[2])^(-1/a[3])) * (1 + 
            (a[3] * (z - a[1]))/a[2])^(-1/a[3] - 1))/a[2]
    else {
        gum.dens(c(a[1], a[2]), z)
    }
}






#name<- daily_data_noNA

#get_POT(name, 40)

get_POT<-function(name,threshold=NA,TTP=NA){
  
  colnames(name) <- c("datum","vf")
  peaks_pr_yr <- NA
  #take negative values out
  name <- subset(name, vf > 0)

  
  # extract year, start at full year
  name$date <- as.POSIXct(paste(name$datum,"/1200",sep=""), format = "%Y%m%d",origin = "1960-01-01")
  name$year <- as.numeric(format(name$date, "%Y"))
  firstyear <- name$year[1]

  name$vf[which(name$vf == -9999)] <- NA
   
  if(is.na(threshold)) threshold <- quantile(name$vf, c(.98))
 #choose thresh_values
 thresh_value <- threshold
 # find lowest quantile (20% quantile to get baseflow - evt use WETSPP R package instead)
 low_quant <- quantile(name$vf, c(.20))
    
 # find first full year, cut incomplete year before
 sub_firstyear <- subset(name, year == firstyear)
 #check length of firstyear, give 5 day extra (so 360 days is be ok in case just five obs are missing)
 if (nrow(sub_firstyear) < 360) name <- subset(name, year > firstyear)
   
 #check length of other years, delete if < 360 observations
 unique_years <- unique(name$year)
 if(length(unique_years) > 0){
      for (m in 1:length(unique_years)){
        loc_uniqueyear <- which(name$year == unique_years[m])
        if (length(loc_uniqueyear) < 360) name <- name[-loc_uniqueyear,] 
     }
      
      #dont continue if name is then empty!
      if(nrow(name) > 0){
        start_year <- head(name$year, n = 1)
        end_year <- tail(name$year, n = 1)
         sub_year = unique(name$year)
 
        
        # get TTR for each year, find 5-10 shortest ones
        #df for writing in TTR per year
        #####################################################
        
        df_TTR <- data.frame(matrix(NA, nrow = length(sub_year), ncol = 2))
        colnames(df_TTR) <- c("year", "TTRdays")
        
        
        
        #### get TTR
        for (z in 1:length(sub_year)){
          #make annual subsets
          sub_name <- subset(name, year == sub_year[z])
          dates <- sub_name$date #sub_name$date  #name$date[6400:6500]
          vf <- sub_name$vf   #sub_name$vf  #as.numeric(name$vf[6400:6500])
          daynumbers <- as.numeric(seq(1,length(dates),1))
          ##interpolate vf to 30min resolution
          vf_interp <- approx(daynumbers,vf, n = length(dates)*24*2, method = "linear")
          #Find points where vf is above LOW_QUANT
          cross_quant <- vf_interp$y > low_quant
          # Points always intersect when above=TRUE, then FALSE or reverse
          cross.points <-which(diff(cross_quant)!=0)
          #get daynumber when "baseflow-line" is crossed
          baseline_cross <- vf_interp$x[cross.points]
          
          #Find points where vf crosses above threshold.
          above <- vf_interp$y > thresh_value
          # Points always intersect when above=TRUE, then FALSE or reverse
          intersect.points<-which(diff(above)!=0)
          nr_POT <- length(intersect.points)/2
          #intersect.points_odd <- intersect.points[seq(1, length(intersect.points), 2)]
          
              #only continue if vf exceeds threshold at least once => length(intersect.points) > 0
          # get dates of when threshold is crossed
          if (nr_POT >= 1 & length(cross.points) > 0 & length(intersect.points) > 0 ){
            daynumber_cross <- vf_interp$x[intersect.points]
            daynumber_cross_odd <- daynumber_cross[seq(1, length(daynumber_cross), 2)]
            daynumber_cross_even <- daynumber_cross[seq(2, length(daynumber_cross), 2)]
            
            
            #isolate POT event
            ## make LOOP if several flood peaks occur: 2 cross.points give 1 POT event
            nr_POT <- length(intersect.points)/2
            
            
            #make matrixes to fill
            POT_start <- (matrix(NA, nrow = nr_POT, ncol = 1))
            POT_end <- (matrix(NA, nrow = nr_POT, ncol = 1))
            duration_POT_DAY <- (matrix(NA, nrow = nr_POT, ncol = 1))
            df_TTRloop <- data.frame(matrix(NA, nrow = nr_POT, ncol = 1))
            colnames(df_TTRloop) <- c("TTRdays")
            
            #if nr_POT for year exists (one or several) than 1 then do this loop -   
            if (nr_POT >= 1 & length(cross.points) > 0){
              for (v in 1:nr_POT){
                #find POT event : first intersection w threshold
                loc_POT_start <- which(vf_interp$x == daynumber_cross_odd[v])
                POT_start[v] <- vf_interp$x[loc_POT_start]
                
                
                #find (in case of several cross.points) nearest cross.point to POT_start - nearest crosspoint BEFORE!
                #find cross.points smaller than POT_start
                smaller_crosspoints <- cross.points[which(cross.points < loc_POT_start)]
                #calculate only if smaller_crosspoint exists, otherwise go to next POT event
                if (length(smaller_crosspoints) > 0){
                  loc_ttr_start <- cross.points[which(abs(smaller_crosspoints-loc_POT_start)==min(abs(smaller_crosspoints-loc_POT_start)))]
                  daynum_start_ttr <- vf_interp$x[loc_ttr_start]
                  #find location of daynum_start_ttr and add 1 to get POT_end
                  #loc_daynum_start <- which(daynumber_cross == daynum_start_ttr)
                  loc_POT_end <- which(vf_interp$x == daynumber_cross_even[v])
                  POT_end[v] <- vf_interp$x[loc_POT_end]
                  #duration_POT_DAY[v] <- (abs(POT_start[v]-POT_end[v]))
                  
                  #find flood max of POT-event
                  max_POT <- max(vf_interp$y[loc_POT_start:loc_POT_end])
                  # find when POT-max of event happens
                  loc_max_POT <- which(vf_interp$y == max_POT)
                  # if flood peak is flat (due to sensor stuck etc) take FIRST value
                  if (length(loc_max_POT) > 1){loc_max_POT = loc_max_POT[1]}
                  max_POT_day <- vf_interp$x[loc_max_POT]
                  #fil TTR into dataframe to select the shortest TTRs 
                  #calculate TTR: 
                  #TTR <- abs(daynum_start_ttr - when_max_POT)
                  TTR_DAY <- (abs(daynum_start_ttr - max_POT_day))
                  ##write into df_TTR
                  #df_TTRloop$year[v] <- sub_year[z]
                  df_TTRloop$TTRdays[v] <- TTR_DAY
                }
              }
  
          
            #write min of year (TTR_DAY) into table for all years (df_TTR)
            #delete previous min_loop
            min_loop <- NULL
            min_loop <- min(df_TTRloop$TTRdays, na.rm = TRUE)
            df_TTR$year[z] <- sub_year[z]
            df_TTR$TTRdays[z] <- min_loop
            }
          } #newly added
        } #newly added 
  
            # take the 5 smallest values for df_TTR$TTRdays, average them to get TTR 
            tst <- df_TTR[with(df_TTR, order(TTRdays)), ]
            #if less than five TTR, take average of all smallest
            if(length(which(is.finite(tst$TTRdays))) <8 ){
              loc_finite <- which(is.finite(tst$TTRdays))
              TTR_smallest <- mean(head(tst$TTRdays[loc_finite], n = 3))
            }
            if(length(which(is.finite(tst$TTRdays)))>= 8){
              loc_finite <- which(is.finite(tst$TTRdays))
              TTR_smallest <- mean(head(tst$TTRdays[loc_finite], n = 5))
            }

            
          
          
        
        
        ##########################################################
        #### select POT-peaks which are independent
        # take entire time, not year by year  
        
        sub_name <- name
        dates <- sub_name$date                 #name$date[6400:6500]
        vf <- sub_name$vf                   
        daynumbers <- as.numeric(seq(1,length(dates),1))
        ##interpolate vf to 30min resolution
        vf_interp <- approx(daynumbers,vf, n = length(dates)*24*2, method = "linear")
        
        ##Find points where vf crosses above threshold.
        above <- vf_interp$y > thresh_value
        # Points always intersect when above=TRUE, then FALSE or reverse
        intersect.points<-which(diff(above)!=0)
        # get daynumber of POT start and end
        daynumber_cross <- vf_interp$x[intersect.points]
        daynumber_cross_odd <- daynumber_cross[seq(1, length(daynumber_cross), 2)]
        daynumber_cross_even <- daynumber_cross[seq(2, length(daynumber_cross), 2)]
        
        ##get max for each peak
        #2 cross.points give 1 POT event
        nr_POT <- length(intersect.points)/2
        intersect.points_odd <- intersect.points[seq(1, length(intersect.points), 2)]
        intersect.points_even <- intersect.points[seq(2, length(intersect.points), 2)]
        
        #make matrixes to fill
        POT_start <- (matrix(NA, nrow = nr_POT, ncol = 1))
        POT_end <- (matrix(NA, nrow = nr_POT, ncol = 1))
        df_POT <- data.frame(matrix(NA, nrow = nr_POT, ncol = 3))
        colnames(df_POT) <- c("max_POT","max_POT_day", "max_POT_date")
      
        
          # find max_POT for each POT-peak
        if (nr_POT >= 1 & length(intersect.points) > 0){
          for (v in 1:nr_POT){
            #find POT event : first intersection w threshold
            loc_POT_start <- which(vf_interp$x == daynumber_cross_odd[v])
            POT_start[v] <- vf_interp$x[loc_POT_start]
            loc_POT_end <- which(vf_interp$x == daynumber_cross_even[v])
            POT_end[v] <- vf_interp$x[loc_POT_end]
            #duration_POT_DAY[v] <- (abs(POT_start[v]-POT_end[v]))
            
            #find flood max of POT-event
            max_POT <- max(vf_interp$y[loc_POT_start:loc_POT_end])
            # find when POT-max of event happens
            loc_max_POT <- which(vf_interp$y == max_POT)

            # if flood peak is flat (due to sensor stuck etc) take FIRST value
            if (length(loc_max_POT) > 1){loc_max_POT = loc_max_POT[1]}
            #daynumber of POT-maximum
            max_POT_day <- vf_interp$x[loc_max_POT]
            #date of POT-maximum
            POT_date <- name$date[round(max_POT_day)]
            df_POT$max_POT_date[v] <- as.character(POT_date)
            #write max_POT_day and max_POT in df_POT
            df_POT$max_POT[v] <- max_POT
            df_POT$max_POT_day[v] <- max_POT_day
            #df_POT$start_POT[v] <- POT_start[v]
            #df_POT$end_POT[v] <- POT_end[v]
            
          }
        }
        
        #difference between max_POT_day 1 and following one -> check if it is larger than 3x its TTR
        df_POT$max_POT_diff_day <- c(NA, diff(df_POT$max_POT_day))
        #TTR between max_POT_day and NEXT (not previous) peak
        df_POT$max_POT_diff_day_2nd <- NA
        for (y in 1:nr_POT){
          df_POT$max_POT_diff_day_2nd[y] <- abs(df_POT$max_POT_day[y] - df_POT$max_POT_day[y+1])
        }
        
        
        # based on Lang et al., 1999
        # (1a) check independence based on max_POT_diff_day (in regard to previous peak) larger than 3*TTR 
        #adjust here the d !!!
		if (is.na(TTP))TTR_3x <- TTR_smallest*3
		else TTR_3x <- TTP*3		
        df_POT$indep_TTR <- NA
        df_POT$indep_TTR[which(df_POT$max_POT_diff_day > TTR_3x)] <- TRUE
        df_POT$indep_TTR[which(df_POT$max_POT_diff_day <= TTR_3x)] <- FALSE
        #check if first peak is independent: if >3xTTR until start of observations
        start_datum <- name$date[1]
        if ((df_POT$max_POT_day[1] - TTR_3x) >  0){df_POT$indep_TTR[1] <- TRUE}
        
        
        #check if min discharge in through between two peaks if < 2/3 of FIRST peak
        # part between POT1-end and POT2-start
        through_end <- intersect.points_odd[2:length(intersect.points_odd)]
        through_start <- intersect.points_even[1:length(intersect.points_even)]
        df_POT$indep_min <- NA
        for (p in 1:nr_POT){
          ##calculate minimum discharge
          #find through min of between POT-events
          if (is.na(through_end[p]) == FALSE) {
            df_POT$min_POT[p] <- min(vf_interp$y[through_start[p]:through_end[p]])
          }
          
        }
        ##check if min_POT is < 2/3 of FIRST peak
        df_POT$indep_min[which(df_POT$max_POT >= (2/3)*df_POT$min_POT)] <- TRUE
        
        # new column if both indep_TTR and indep_min are TRUE --> this defines the start of the "POT group event": consecutive events w > threshold
        df_POT$indep_PEAK <- FALSE
        df_POT$indep_PEAK[which(df_POT$indep_TTR == TRUE & df_POT$indep_min == TRUE)] <- TRUE
        
        # column for group: from indep_PEAK[n] to indep_PEAK[n+1]
        df_POT$group_nr <- NA
        #predefine group numbers
        groupnr <- c(1:nr_POT)
        
        #remove peaks that aren't indep_mean = TRUE
        loc_remove <- which(df_POT$indep_min == FALSE)
        if(length(loc_remove) >0){df_POT <- df_POT[-loc_remove,]}
        
        #loop from indep_PEAK[n] to indep_PEAK[n+1]-1 to define groups, then find each groups max
        startval <- 0
        for (v in 1:nr_POT){
          #find event, give group_nr
          if(df_POT$indep_PEAK[v] == TRUE){
            startval <- startval+1
            df_POT$group_nr[v] <- startval
          }
          #fill NA value with previous group number
          if (is.na(df_POT$group_nr[v]) & v > 1){
            df_POT$group_nr[v] <-  df_POT$group_nr[v-1]
          }
          if(is.na(df_POT$group_nr[v]) & v == 1){
            df_POT$group_nr[v] <- 1
          }
        }
        
        #find largest max_POT for each group
        #verfidied peaks
        df_POT$verified <- NA
        group_numbers <- c(1:max(df_POT$group_nr, na.rm = T))
        for (v in 1:length(group_numbers)){
          loc_group <- which(df_POT$group_nr == group_numbers[v])
          #find max for each group
          max_group <- max(df_POT$max_POT[loc_group])
          loc_max <- which(df_POT$max_POT == max_group)
          df_POT$verified[loc_max] <- TRUE
        }
        
        loc_verified_peaks <- which(df_POT$verified == TRUE)
        verified_peaks <- df_POT[,c("max_POT_date","max_POT")]
        verified_peaks <- verified_peaks[loc_verified_peaks,]
        


}
}
verified_peaks$TTP <- TTR_3x/3
return(verified_peaks)
}




quantiles_gp<-function(Thefit,rp) 
{
    mloc<-as.vector(Thefit$parameters[,1,])
    mscale<-as.vector(Thefit$parameters[,2,])
    mshape<-as.vector(Thefit$parameters[,3,])
	nc <- length(m)
    nr <- nrow(post)
    dn <- list(rownames(post), m)
    mat <- matrix(0, ncol = nc, nrow = nr, dimnames = dn)
    loc <- post[, "mu"]
    scale <- post[, "sigma"]
    shape <- post[, "xi"]

    for(i in 1:nq) Q[,i]<-invF.genpar ((1-1/rp[i]), mloc, mscale, mshape)
	
	return(Q)

}


get_parameter_ci<-function(thefit,q){
np=dim(thefit$parameters)[2]
nq<-length(q)
pn<-c('Par1','Par2','Par3')
qe<-matrix(ncol=np,nrow=nq)
colnames(qe)<-pn[1:np]
rownames(qe)<-as.character(q)
for(i in 1:np)qe[,i]<-quantile(as.vector(thefit$parameters[,i,]),q)
qe
}





gpprior <- function (x) {
# x = vector of parameter values: c(location, scale, shape)
# I assume the shape parameter only has a prior with mean zero and standard deviation 0.2
dnorm(x[3], 0, 0.2)*dnorm(x[1],th,0.001)
}







pointspos_gp <- function(x, a=0, nf=1, orient="Tx", ...) { 
 
# INPUT 
# x = colonna 
# ... = graphical parameters as xlab, ylab, main... 
# OUTPUT 
# rappresentazione in carta cartesiana della serie 
 
 
ordinato <- sort(x) 
n <- length(ordinato) 
i <- 1:n 
plotpos <- (i - a)/(n + 1 - 2*a) 
T=1/(1 - plotpos)
TA = T/nf 
 
x=ordinato 


if (orient=="xT") points(x,TA,...) else if (orient=="Tx") points(TA,x,...) 
else stop("pointspos(x, a, orient): orient unknown") 
} 


plotdiagnMCMC_GP <- function(xx,nf=1, ...) { 
#Plot of the frequency curve 
T <- c(1,1000) 
X <- c(0, 1.3*max(c(xx$xcont, xx$xhist, xx$infhist, xx$suphist), na.rm=TRUE)) 
plot(T, X, type="n", log="x", ...) 
grid(equilogs=FALSE)  
lines(xx$returnperiods/nf, xx$quantilesML) 
lines(xx$returnperiods/nf, xx$intervals[1,], lty=2) 
lines(xx$returnperiods/nf, xx$intervals[2,], lty=2) 

pointspos_gp(xx$xcont, a=0.4, nf=nf, orient="Tx", ...)
 
} 





gp.diag.bayes<-function(thefit,lth) 
{
    n <- length(thefit$xcont)
    x <- (1:n)/(n + 1)
        oldpar <- par(mfrow = c(2, 2))
 
     postpar<-thefit$parametersML
	 
		
        gp.pp(postpar, thefit$xcont)
        gp.qq(postpar, thefit$xcont)		
        plotdiagnMCMC_GP(thefit,lth,ylab='X (m3/s)')
        gp.his(postpar, thefit$xcont)
    par(oldpar)
    invisible()
}
#<environment: namespace:ismev>



gp.pp<-function (a, dat) 
{
    plot((1:length(dat))/(length(dat)+1), F.genpar(sort(dat),a[1],a[2],a[3]), xlab = "Empirical", 
        ylab = "Model", main = "Probability Plot")
    abline(0, 1, col = 4)
}


gp.qq<-function (a, dat) 
{
    plot(invF.genpar((1:length(dat)/(length(dat) + 1)),a[1],a[2],a[3]), sort(dat), 
        ylab = "Empirical (m3/s)", xlab = "Model (m3/s)", main = "Quantile Plot")
    abline(0, 1, col = 4)
}



gp.his<-function (a, dat) 
{
    h <- hist(dat, plot = FALSE)
    if (a[3] < 0) {
        x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] - 
            0.001)), length = 100)
    }
    else {
        x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)), 
            max(h$breaks), length = 100)
    }
    y <- f.genpar(x,a[1],a[2],a[3])
    hist(dat, freq = FALSE, ylim = c(0, max(max(h$density), max(y))), 
        xlab = "x (m3/s)", ylab = "f(x)", main = "Density Plot")
    points(dat, rep(0, length(dat)))
    lines(x, y)
}






plot_all_models <- function(xx_ams,xx_t1,xx_t2, nf1, nf2, ...) { 
#Plot of the frequency curve 
T <- c(1,1000) 
X <- c(min(xx_ams$xcont), 1.6*max(c(xx_ams$xcont, xx_ams$xhist, xx_ams$infhist, xx_ams$suphist), na.rm=TRUE)) 
plot(T, X, type="n", log="x", ...) 
grid(equilogs=FALSE)  
lines(xx_ams$returnperiods, xx_ams$quantilesML) 
lines(xx_ams$returnperiods, xx_ams$intervals[1,], lty=2) 
lines(xx_ams$returnperiods, xx_ams$intervals[2,], lty=2) 
ret <- .pointspos3 (xx_ams$xcont, xx_ams$xhist, xx_ams$infhist, xx_ams$suphist, xx_ams$nbans, xx_ams$seuil)


lines(xx_t1$returnperiods/nf1, xx_t1$quantilesML,col=2) 
lines(xx_t1$returnperiods/nf1, xx_t1$intervals[1,], lty=2, col=2) 
lines(xx_t1$returnperiods/nf1, xx_t1$intervals[2,], lty=2, col=2) 


nf = length(xx_t2$max_POT)/( diff(range(as.integer(substr(xx_t2$max_POT_date,1,4))))+1)	
lines(xx_t2$returnperiods/nf2, xx_t2$quantilesML,col=3) 
lines(xx_t2$returnperiods/nf2, xx_t2$intervals[1,], lty=2,col=3) 
lines(xx_t2$returnperiods/nf2, xx_t2$intervals[2,], lty=2,col=3) 
legend('bottomright',col=c(1,2,3),lty=c(1,1,1),legend=c("AMS", "POT_T1","POT_T2"))
} 



































