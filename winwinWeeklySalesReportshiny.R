  #### Weekly summary

processfile <- function(df){
  #newdoc <- file.choose()
  #wwsales <- read.csv(file)
  
  wwsales <<- df
  
  # wwsales <- read.csv("C:/Users/fL3xZ0n3/Documents/winwin/items-2016-05-03-2016-06-14.csv")
  
  
  
  # convert Gross, Net & Discounts to usable number
  wwsales$netnum <<- as.numeric(sub('$','',as.character(wwsales$Net.Sales),fixed=TRUE))
  wwsales$grossnum <<- as.numeric(sub('$','',as.character(wwsales$Gross.Sales),fixed=TRUE))
  wwsales$disnum <<- as.numeric(sub('$', '',as.character(wwsales$Discounts),fixed=TRUE))
  wwsales$disnum <<- as.numeric(sub('-', '',as.character(wwsales$disnum),fixed=TRUE))
  
  # convert Date to usable date, compute usable aadjusted Time & Date Columns
  wwsales$rDate <<- as.Date(wwsales$Date, format = "%m/%d/%y")
  wwsales$adjtime <<- as.numeric(substr(wwsales$Time, 1, 2))
  wwsales$adjdate <<- wwsales$rDate
  
  for (i in 1:nrow(wwsales)){
    if (wwsales$adjtime[i] <= 5){
      wwsales$adjdate[i] <<- wwsales$adjdate[i] - 1
    }
  }
  
  
  # Subset out Bar, Kitchen & Coffee data & compute new columns
  barBsales <<- subset(wwsales, Category == "Bar - Beer/Wine")
  barLsales <<- subset(wwsales, Category == "Bar - Liquor")
  cofsales <<- subset(wwsales, Category == "Coffee")
  kitsales <<- subset(wwsales, Category == "Kitchen")
  nonesales <<- subset(wwsales, Category == "None")
  dissales <<- subset(wwsales, disnum != 0)
  
  
  barBday <<- as.data.frame(tapply(barBsales$netnum, barBsales$adjdate, sum))
  names(barBday)[1] <<- "barBnet"
  barBday$barBitems <<- tapply(barBsales$Qty, barBsales$adjdate, sum)
  barBday$barBitemavg <<- barBday$barBnet / barBday$barBitems
  barBday$date <<- as.Date(rownames(barBday))
  
  barLday <<- as.data.frame(tapply(barLsales$netnum, barLsales$adjdate, sum))
  names(barLday)[1] <<- "barLnet"
  barLday$barLitems <<- tapply(barLsales$Qty, barLsales$adjdate, sum)
  barLday$barLitemavg <<- barLday$barLnet / barLday$barLitems
  barLday$date <<- as.Date(rownames(barLday))
  
  kitday <<- as.data.frame(tapply(kitsales$netnum, kitsales$adjdate, sum))
  names(kitday)[1] <<- "kitnet"
  kitday$kititems <<- tapply(kitsales$Qty, kitsales$adjdate, sum)
  kitday$kititemavg <<- kitday$kitnet / kitday$kititems
  kitday$date <<- as.Date(rownames(kitday))
  
  #newrow <- c(200, '2016-05-31')
  #kitday <- rbind(kitday, newrow)
  
  cofday <<- as.data.frame(tapply(cofsales$netnum, cofsales$adjdate, sum))
  names(cofday)[1] <<- "cofnet"
  cofday$cofitems <<- tapply(cofsales$Qty, cofsales$adjdate, sum)
  cofday$cofitemavg <<- cofday$cofnet / cofday$cofitems
  cofday$date <<- as.Date(rownames(cofday))
  
  noneday <<- as.data.frame(tapply(nonesales$netnum, nonesales$adjdate, sum))
  names(noneday)[1] <<- "nonenet"
  noneday$noneitems <<- tapply(nonesales$Qty, nonesales$adjdate, sum)
  noneday$noneitemavg <<- noneday$nonenet / noneday$noneitems
  noneday$date <<- as.Date(rownames(noneday))
  
  disday <<- as.data.frame(tapply(dissales$disnum, dissales$adjdate, sum))
  names(disday)[1] <<- "discounts"
  disday$disitems <<- tapply(dissales$Qty, dissales$adjdate, sum)
  disday$disitemavg <<- disday$discounts / disday$disitems
  disday$date <<- as.Date(rownames(disday))
  
  #daygrossk <- merge(daygross, kitday, all = TRUE, by.x = "date", by.y = "date")
  #daygrossk <- merge(daygross, cofday, all = TRUE, by.x = "date", by.y = "date")
  
  
  # create dataframe with daily net sales & compute columns
  daynet <<- as.data.frame(tapply(wwsales$netnum, wwsales$adjdate, sum))
  names(daynet)[1] <<- "net"
  daynet$date <<- as.Date(rownames(daynet))
  daynet$totitems <<- tapply(wwsales$Qty, wwsales$adjdate, sum)
  daynet$totitemavg <<- daynet$net / daynet$totitems
  daynet <<- merge(daynet, barBday, all = TRUE, by.x = "date", by.y = "date")
  daynet <<- merge(daynet, barLday, all = TRUE, by.x = "date", by.y = "date")
  daynet <<- merge(daynet, kitday, all = TRUE, by.x = "date", by.y = "date")
  daynet <<- merge(daynet, cofday, all = TRUE, by.x = "date", by.y = "date")
  daynet <<- merge(daynet, noneday, all = TRUE, by.x = "date", by.y = "date")
  daynet <<- merge(daynet, disday, all = TRUE, by.x = "date", by.y = "date")
  daynet$barBpct <<- daynet$barBnet / daynet$net
  daynet$barLpct <<- daynet$barLnet / daynet$net
  daynet$kitpct <<- daynet$kitnet / daynet$net
  daynet$cofpct <<- daynet$cofnet / daynet$net
  daynet$nonepct <<- daynet$nonenet / daynet$net
  daynet$dispct <<- daynet$discounts / (daynet$net + daynet$discounts) #calc discounts as % of gross sales
  daynet$day <<- weekdays(as.Date(daynet$date,'%Y-%m-%d'))
  daynet$month <<- months(as.Date(daynet$date,'%Y-%m-%d'))
  daynet[is.na(daynet)] <<- 0
  
  #daygross$bargross <- tapply(barsales$grossnum, barsales$adjdate, sum)
  #daygross$kitgross <- tapply(kitsales$grossnum, kitsales$adjdate, sum)
  #daygross$cofgross <- tapply(cofsales$grossnum, cofsales$adjdate, sum)
  
  # !!!!! issue with subsetting: currently based on indexes & reliees on there being no Mondays in the 
  # data.  Can try to implement by actual date ranges, maybe even using day of week + last observation
  
  finalsun <<- tail(daynet$date, 1)
  startlw <<- finalsun - 5
  endlw <<- finalsun + 1
  startpw <<- startlw - 7
  endpw <<- startlw - 1
  startppw <<- startpw - 7
  endppw <<- startpw - 1
  startpppw <<- startppw - 7
  endpppw <<- startppw - 1
  startfw <<- startlw - 28
  endfw <<- startlw - 1
  
  
  #subset & sum previous week (week before last week)
  #prevweek <- tail(daygross, 12)
  #prevweek <- prevweek[1:6,]
  prevweek <<- daynet[daynet$date >= startpw & daynet$date <= endpw,]
  prevweeksum <<- as.data.frame(colSums(prevweek[sapply(prevweek, is.numeric)], na.rm = TRUE))
  prevweeksum <<- as.data.frame(t(prevweeksum))
  
  pprevweek <<- daynet[daynet$date >= startppw & daynet$date <= endppw,]
  pprevweeksum <<- as.data.frame(colSums(pprevweek[sapply(pprevweek, is.numeric)], na.rm = TRUE))
  pprevweeksum <<- as.data.frame(t(pprevweeksum))
  
  ppprevweek <<- daynet[daynet$date >= startpppw & daynet$date <= endpppw,]
  ppprevweeksum <<- as.data.frame(colSums(ppprevweek[sapply(ppprevweek, is.numeric)], na.rm = TRUE))
  ppprevweeksum <<- as.data.frame(t(ppprevweeksum))
  
  
  #subset & sum 4 weeks prior (all 4 weeks prior to last week)
  fourweek <<- daynet[daynet$date >= startfw & daynet$date <= endfw,]
  fourweeksum <<- as.data.frame(colSums(fourweek[sapply(fourweek, is.numeric)], na.rm = TRUE))
  fourweeksum <<- as.data.frame(t(fourweeksum))
  
  
  #subset last week
  #lastweek <- tail(daygross, 6)
  lastweek <<- daynet[daynet$date >= startlw & daynet$date <= endlw,]
  
  # sum lastweek columns if numeric, then recompute avg's & pct's
  
  lastweeksum <<- as.data.frame(colSums(lastweek[sapply(lastweek, is.numeric)], na.rm = TRUE))
  lastweeksum <<- as.data.frame(t(lastweeksum))
  lastweeksum$totitemavg <<- lastweeksum$net / lastweeksum$totitems
  lastweeksum$barBitemavg <<- lastweeksum$barBnet / lastweeksum$barBitems
  lastweeksum$barLitemavg <<- lastweeksum$barLnet / lastweeksum$barLitems
  lastweeksum$kititemavg <<- lastweeksum$kitnet / lastweeksum$kititems
  lastweeksum$cofitemavg <<- lastweeksum$cofnet / lastweeksum$cofitems
  lastweeksum$noneitemavg <<- lastweeksum$nonenet / lastweeksum$noneitems
  lastweeksum$disitemavg <<- lastweeksum$discounts / lastweeksum$disitems
  lastweeksum$barBpct <<- lastweeksum$barBnet / lastweeksum$net
  lastweeksum$barLpct <<- lastweeksum$barLnet / lastweeksum$net
  lastweeksum$kitpct <<- lastweeksum$kitnet / lastweeksum$net
  lastweeksum$cofpct <<- lastweeksum$cofnet / lastweeksum$net
  lastweeksum$nonepct <<- lastweeksum$nonenet / lastweeksum$net
  lastweeksum$dispct <<- lastweeksum$discounts / (lastweeksum$net + lastweeksum$discounts)
  

  

#maketable <- function(){
  
  # format values & assign to interpretable abbreviations 
  # (net, change, four week change, items, item average)
  net <- sprintf("$%.2f", lastweeksum$net)
  netc <- sprintf("%.1f %%", 100*(lastweeksum$net/prevweeksum$net - 1))
  netfwc <- sprintf("%.1f %%", 100*(lastweeksum$net/(fourweeksum$net / 4) - 1))
  ti <- lastweeksum$totitems
  tia <- sprintf("$%.2f", lastweeksum$totitemavg)
  bbn <- sprintf("$%.2f", lastweeksum$barBnet)
  bbc <- sprintf("%.1f %%", 100*(lastweeksum$barBnet/prevweeksum$barBnet - 1))
  bbfwc <- sprintf("%.1f %%", 100*(lastweeksum$barBnet/(fourweeksum$barBnet / 4) - 1))
  bbi <- lastweeksum$barBitems
  bbia <- sprintf("$%.2f", lastweeksum$barBitemavg)
  bln <- sprintf("$%.2f", lastweeksum$barLnet)
  blc <- sprintf("%.1f %%", 100*(lastweeksum$barLnet/prevweeksum$barLnet - 1))
  blfwc <- sprintf("%.1f %%", 100*(lastweeksum$barLnet/(fourweeksum$barLnet / 4) - 1))
  bli <- lastweeksum$barLitems
  blia <- sprintf("$%.2f", lastweeksum$barLitemavg)
  kn <- sprintf("$%.2f", lastweeksum$kitnet)
  kc <- sprintf("%.1f %%", 100*(lastweeksum$kitnet/prevweeksum$kitnet - 1))
  kfwc <- sprintf("%.1f %%", 100*(lastweeksum$kitnet/(fourweeksum$kitnet / 4) - 1))
  ki <- lastweeksum$kititems
  kia <- sprintf("$%.2f", lastweeksum$kititemavg)
  cn <- sprintf("$%.2f", lastweeksum$cofnet)
  cc <- sprintf("%.1f %%", 100*(lastweeksum$cofnet/prevweeksum$cofnet - 1))
  cfwc <- sprintf("%.1f %%", 100*(lastweeksum$cofnet/(fourweeksum$cofnet / 4) - 1))
  ci <- lastweeksum$cofitems
  cia <- sprintf("$%.2f", lastweeksum$cofitemavg)
  d <- sprintf("$%.2f", lastweeksum$discounts)
  dc <- sprintf("%.1f %%", 100*(lastweeksum$discounts/prevweeksum$discounts - 1))
  dfwc <- sprintf("%.1f %%", 100*(lastweeksum$discounts/(fourweeksum$discounts / 4) - 1))
  di <- lastweeksum$disitems
  dia <- sprintf("$%.2f", lastweeksum$disitemavg)
  nn <- sprintf("$%.2f", lastweeksum$nonenet)
  nc <- sprintf("%.1f %%", 100*(lastweeksum$nonenet/prevweeksum$nonenet - 1))
  nfwc <- sprintf("%.1f %%", 100*(lastweeksum$nonenet/(fourweeksum$nonenet / 4) - 1))
  ni <- lastweeksum$noneitems
  nia <- sprintf("$%.2f", lastweeksum$noneitemavg)
  bbp <- sprintf("%.1f %%", 100*lastweeksum$barBpct)
  blp <- sprintf("%.1f %%", 100*lastweeksum$barLpct)
  kp <- sprintf("%.1f %%", 100*lastweeksum$kitpct)
  cp <- sprintf("%.1f %%", 100*lastweeksum$cofpct)
  np <- sprintf("%.1f %%", 100*lastweeksum$nonepct)
  dp <- sprintf("%.1f %%", 100*lastweeksum$dispct)
  #og <- sprintf("$%.2f", lastweeksum[1] - (lastweeksum[4]+lastweeksum[7]+lastweeksum[10]))
  #oc <- sprintf("%.1f %%", 100*((lastweeksum[1] - (lastweeksum[4]+lastweeksum[7]+lastweeksum[10]))/(prevweeksum[1] - (prevweeksum[4]+prevweeksum[7]+prevweeksum[10])  - 1)))
  #oi <- ti - (bi+ki+ci)
  #oia <- sprintf("$%.2f", og / oi)
  #op <- sprintf("%.1f %%", 100*(og/g))
                               
  
  # format as matrix / table
  lwtable <-matrix(c(#bn, bc, bfwc, bi, bia, bp,
                     bbn,bbc,bbfwc,bbi,bbia,bbp,
                     bln,blc,blfwc,bli,blia,blp,
                     cn, cc, cfwc, ci, cia, cp,
                     kn, kc, kfwc, ki, kia, kp,
                     nn, nc, nfwc, ni, nia, np,
                     d,  dc, dfwc, di, dia, dp, 
                     #og, oc, 0, oi, oia, op,
                     net, netc, netfwc, ti, tia, '-'),ncol=6,byrow=TRUE)
  rownames(lwtable)<-c("Beer/Wine", "Liquor", "Coffee", "Kitchen", "None", "Discounts", "Total")
  colnames(lwtable)<-c("Net","% Change", "vs 4wk Avg", "Items", "Item Avg", "% of Sales")
  lwtablet <- as.table(lwtable)
  lwtablet
  
  #lwtable
}


dayplot <- function(){
################ weekday weekend breakdown

  lastweekend <<- lastweek[lastweek$day == "Friday" | lastweek$day == "Saturday" , ]
  lastweekdays <<- lastweek[lastweek$day != "Friday" & lastweek$day != "Saturday" , ]
  
  brunchitems <<- wwsales[wwsales$adjdate >= (endlw-2),]
  brunchitems <<- brunchitems[brunchitems$adjtime > 6 & brunchitems$adjtime < 17,]
  
  brunchnet <<- tapply(brunchitems$netnum, brunchitems$adjdate, sum)
  
  
  
  
  ## dayplot
  
  brunchadded <<- c(lastweek$net[1], lastweek$net[2], lastweek$net[3], 
                   lastweek$net[4], brunchnet[1], lastweek$net[5]-brunchnet[1], 
                   brunchnet[2], lastweek$net[6]-brunchnet[2])
  
  cols <- c("green", "green", "green", "green", "magenta", "green", "magenta","green")
  
  par(cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  dp <- barplot(brunchadded, names.arg = c("Tues", "Weds", "Thurs", "Fri", "Sat", "Sat Nite", 
                                           "Sun", "Sun Nite"),
                col = cols, las = 1, main = "Net Sales Per Shift", ylim = c(0,1400),
                xlab = paste0(as.character.Date(startlw, format = "%m/%d/%y"), " - ", as.character.Date(endlw-1, format = "%m/%d/%y")))
  segments(0.1, 500, 3.7, 500,col = "red", lty=3, lwd=3)
  segments(3.75, 1250, 4.85, 1250,col = "red", lty=3, lwd=3)
  segments(4.95, 500, 6.1, 500,col = "red", lty=3, lwd=3)
  segments(6.15, 1250, 7.25, 1250,col = "red", lty=3, lwd=3)
  segments(7.35, 500, 9.75, 500,col = "red", lty=3, lwd=3)
  legend("topleft", legend= c("Night (> 5pm)", "Brunch"),
         bty = "n", fill = c("green", "magenta"),
         pt.cex = 2, inset=c(.012, 0),  x.intersp = 1.80)
  legend("topleft", legend= "Goal", lty = 3, lwd = 3,
         bty = "n", y.intersp=5.3, col="red", 
         x.intersp = .5, pt.cex = 2, inset=c(.012, 0))

}


weeklyplot <- function(){

  #par(pin = c(7, 5))
  ## weekly plot
  
  weeklyframe <- rbind(lastweeksum, prevweeksum, pprevweeksum, ppprevweeksum)
  weekending <- c(endlw, endpw, endppw, endpppw)
  weeklyframe <- cbind(weeklyframe, as.Date(weekending, origin = "1970-01-01"))
  rownames(weeklyframe) <- NULL
  weeklyframe <- as.data.frame(weeklyframe)
  colnames(weeklyframe)[28] = "weekending"


  # as.Date(weeklyframe[1, 16], origin = "1970-01-01")
  
  par(cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  wp <- plot(as.Date(weeklyframe$weekending, origin = "1970-01-01"), weeklyframe$net, type = "l", col = "green",
             lwd = 3, ylim = c(0, 6500), ylab='', xlab = 'Week Ending Date', las = 1, 
             main = "Weekly Net Sales")
  grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted",
       lwd = .5, equilogs = TRUE)
  lines(weeklyframe$weekending, weeklyframe$barBnet, type = "l", col = "orange", lwd = 3)
  lines(weeklyframe$weekending, weeklyframe$barLnet, type = "l", col = "purple", lwd = 3)
  lines(weeklyframe$weekending, weeklyframe$cofnet, type = "l", col = "brown", lwd = 3)
  lines(weeklyframe$weekending, weeklyframe$kitnet, type = "l", col = "magenta", lwd = 3)
  lines(weeklyframe$weekending, weeklyframe$nonenet, type = "l", col = "light blue", lwd = 3)
  lines(weeklyframe$weekending, c(5500, 5500, 5500, 5500), type = "l", col = "red", lty = 3, lwd = 3)
  legend("topright", legend= c("goal", "total", "beer/wine", "liquor",
                               "none", "kitchen", "coffee"),
         bty = "1", box.col = "white", cex = 1.1,
         col=c("red", "green", "orange", "purple", "light blue", "magenta", "brown"), lty = c(3,1,1,1,1,1,1),
         lwd = 2, x.intersp = 1, y.intersp = .90, inset=c(.015, .01))
  lines(weeklyframe$weekending, weeklyframe$net, type = "l", col = "green",
        lwd = 3)
  #lines(weeklyframe$weekending, weeklyframe$discounts, type = "l", col = "pink",
  #      lwd = 3, lty = 4)
  
}



goalplot <- function(){
  
  ####### goalplot
  
  
  lastfour <- rbind.data.frame(ppprevweek, pprevweek, prevweek, lastweek)
  
  
  lastfour$daygoal <- 500
  days500 <- c("Monday", "Tuesday", "Wednesday", "Thursday")
  
  
  
  for (i in 1:nrow(lastfour)){
    if (lastfour$day[i] %in% days500){
      lastfour$daygoal[i] <- 500
    } else if (lastfour$day[i] == "Friday") {
      lastfour$daygoal[i] <- 1250
    } else if (lastfour$day[i] == "Saturday") {
      lastfour$daygoal[i] <- 1750
    } else if (lasfour$day[i] == "Sunday") {
      lastfour$daygoal[i] <- 1000
    }
  }
  
  lastfour$daygoalmet <- (lastfour$net / lastfour$daygoal) - 1
  
  par(cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  par(mar=c(5,4.1,4.8,1))
  gm <- barplot(lastfour$daygoalmet, 
                ylim = c(-1,1), ylab='', xaxt = "n" , las = 2,
                col=ifelse(lastfour$daygoalmet>0,"light green","magenta"),
                main = "Daily % of Sales Goals Missed or Exceeded\nin Past Month",
                xlab = paste0(as.character.Date(startpppw, format = "%m/%d/%y"), " - ", as.character.Date(endlw-1, format = "%m/%d/%y")))
  text(cex = 1.25,x=gm-.1, y =-1.2, paste(substr(lastfour$day,1, 3)), srt = 90, xpd =T)  

}



itemplots <- function(){
  ################## Itms breakdown
  
  weekitems <- wwsales[wwsales$adjdate >= startlw & wwsales$adjdate <= endlw,]
  #weekbaritems <- subset(weekitems, Category == 'Bar')
  weekbbitems <- subset(weekitems, Category == 'Bar - Beer/Wine')
  weekblitems <- subset(weekitems, Category == 'Bar - Liquor')
  weekcofitems <- subset(weekitems, Category == 'Coffee')
  weekkititems <- subset(weekitems, Category == 'Kitchen')
  
  bbtable <- tapply(weekbbitems$Qty, weekbbitems$Item, sum)
  bbtop <- head(sort(bbtable, T))
  
  bltable <- tapply(weekblitems$Qty, weekblitems$Item, sum)
  bltop10 <- head(sort(bltable, T), 10)
  
  kittable <- tapply(weekkititems$Qty, weekkititems$Item, sum)
  kittop10 <- head(sort(kittable, T), 10)
  
  ### !!!!!!!!!!! need to find a way to index by name or just truncate all names instead
  ###             of renaming
  
  
  
  #par(mar=c(6.1,4.1,4.1,2.1))
  
  par(mar = c(10,5,4,0), cex.main = 1.75, cex.axis =1.5)
  par(mfrow=c(3,1))
  xx <- barplot(bbtop, xaxt="n", 
                main = "Top Beer/Wine Items for Week",
                col = "orange")
  #labs <- paste(names.arg = abbreviate(names(bartop20), minlength = 12))
  bblabs <- paste(names.arg = substr(names(bbtop), 1, 15))
  text(cex=1.8, x=xx+.2, y=-3, bblabs, xpd=TRUE, srt=55, pos=2)
  
  
  xy <- barplot(bltop10, xaxt="n", 
                main = "Top 10 Liquor Items for Week",
                col = "purple")
  #labs <- paste(names.arg = abbreviate(names(bartop20), minlength = 12))
  bllabs <- paste(names.arg = substr(names(bltop10), 1, 15))
  text(cex=1.8, x=xy+.33, y=-1, bllabs, xpd=TRUE, srt=55, pos=2)
  
  
  
  x <- barplot(kittop10, xaxt="n", 
               main = "Top 10 Kitchen Items for Week",
               col = "pink")
  labs <- paste(substr(names(kittop10), 1, 15))
  text(cex=1.8, x=x+.20, y=-.60, labs, xpd=TRUE, srt=50, pos=2)
  
}
