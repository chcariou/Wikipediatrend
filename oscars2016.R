# Best Picture - Oscars - Wikipedia
# Christophe Cariou
# February 2016

# Libraries

  library(wikipediatrend)
  library(stringr)
  library(extrafont)
	library(animation)

# Extract data from wikipedia

	liste <- c("The_Revenant_(2015_film)","The_Martian_(film)","The_Big_Short_(film)",
		"Bridge_of_Spies_(film)","Brooklyn_(film)","Mad_Max:_Fury_Road","Room_(2015_film)","Spotlight_(film)")

	n.pages <- dim(liste)[1]

	date_first <- "2016-02-01"
	date_last <- "2016-03-01"

	for (i in 1:n) {

		wiki00 <- wp_trend(page=liste[i],from=date_first,to=date_last,lang="en")
		if (i==1) { oscars <- wiki00[,c(1,2,4,5)] } else { oscars <- rbind(oscars,wiki00[,c(1,2,4,5)]) }

	}


# Date, title and color

	oscars$Date <- as.Date(oscars$date)
	oscars <- oscars[order(oscars$Date,decreasing=FALSE),]
	oscars <- oscars[,c(5,3,2)]

	liste <- unique(oscars$page)
	n <- length(liste)
	total <- aggregate(oscars$count,by=list(page=oscars$page),FUN=sum)
	total <- total[order(total$x,decreasing=TRUE),]
	total$id <- seq(1,n,1)

	a <- total$page
	a <- str_replace(a, "film","")
	a <- str_replace(a, "2015","")
	a <- str_replace_all(a, "[.]","")
	a <- str_replace_all(a, "[()]","")
	a <- str_replace_all(a, "[_]"," ")
	a <- str_trim(a)
	total$film  <- a

	total$col <- c("#0E9D58","#4285F4","#AB47BC","#F3B402","#DB4437","#FF7043","#9E9D24","#01ABC1")
	
# Rank

	days <- sort(unique(oscars$Date))
	for (i in 1:n) {
		a <- subset(oscars, page==total$page[i])
		if (i==1) { oscars1 <- a[,c(1,3)] 
		} else { oscars1 <- cbind(oscars1,a[,3]) }
	}
	colnames(oscars1)[2:(n+1)] <- total$film
	oscars2 <- oscars1[,2:(n+1)]

	rangs <- data.frame(t(apply(oscars2, 1, function(x) rank(x, ties.method="min"))))
	rangs$Date <- oscars1$Date


# Datavisualisation 01 - Number of page views

 	#quartz(width=29.7/2.6,height=21/2.6,bg="#FEFEFE")
	#par(mfrow=c(2,4))
	#for (i in 1:n) {
	#	a <- subset(oscars, page==total$page[i])
	#	plot(a$Date,a$count,type="l", main=total$film[i],ylim=c(0,250000), col=total$col[i], lwd=2)
	#}

# Datavisualisation 02 - Rank of page views

	#quartz(width=29.7/2.6,height=21/2.6,bg="#FEFEFE")
	#par(mar=c(0,0,0,0),oma=c(0,0,0,0))
	#plot(0,0,type="n",xlim=c(-5, 360),ylim=c(-2,11),xaxs="i",yaxs="i",axes=FALSE,xlab=NA,ylab=NA)
	#for (i in 1:n) {
	#	lines(seq(1,325,1), rangs[,i], col=total$col[i], lwd=5)
	#	points(325, rangs[325,i], pch=19, col=total$col[i], cex=2)
	#	text(335, rangs[325,i], total$film[i], col="#262D2D", family="Roboto", cex=0.5, pos=4, offset=0)
	#}


# Datavisualisation 03 - Animation
# Datavisualisation one by one: all inside a pdf file
# PDF for higher quality / resolution of my datavisualisation

	setwd("/Users/ChCariou/Desktop/R_Oscars.Wikipedia")

	#dev.off()
	quartz(width=21/2.6*16/9,height=21/2.6,bg="#FEFEFE",type="pdf")

	ndays <- length(rangs$Date)-49
 for (d in 1:ndays) {


	par(mar=c(0,0,0,0),oma=c(0,0,0,0))
	plot(0,0,type="n",xlim=c(-5, 63),ylim=c(-2,11),xaxs="i",yaxs="i",axes=FALSE,xlab=NA,ylab=NA)


	# Static components 
	
		# Header
	
			text(-2,10,"The Oscars: Best Picture",family="Roboto Light",pos=4,offset=0,cex=2, col="#262D2D")
			segments(-2,9.5,60,9.5,lwd=0.5,col="#262D2D")

		# Graph

			titre2 <- paste("Wikipedia Rank - daily page views from ", 
				format(rangs$Date[1],"%B %d, %Y")," to ",
				format(rangs$Date[length(rangs$Date)],"%B %d, %Y"),".", sep="")
			text(-2,9,titre2,family="Roboto",pos=4,offset=0,cex=0.75, col="#262D2D")

			text(-2,seq(1,8,1),paste("#",rev(seq(1,8,1)),sep=""),family="Roboto Light",pos=4,offset=0,cex=1)
	
		# Footer

			segments(-2,-0.5,60,-0.5,lwd=0.5,col="#262D2D")
			data <- "Data: Wikipedia trends"
			footer <- "Rstuff 16.15 — Christophe Cariou"
			text(-2,-1,toupper(footer),family="Roboto Light",pos=4,offset=0,cex=0.75, col="#262D2D")
			text(60,-1,toupper(data),family="Roboto Light",pos=2,offset=0,cex=0.75, col="#262D2D")


	# Animated component


		

			start <- d
			end <- d+49

			for (i in 1:n) {
				lines(seq(1,50,1), rangs[start:end,i], col=total$col[i], lwd=10)
				points(50, rangs[end,i], pch=19, col=total$col[i], cex=3)
				text(51, rangs[end,i], total$film[i], col="#262D2D", family="Roboto", cex=1, pos=4, offset=0)
			}

			text(1,0,format(rangs$Date[start],"%b %d %Y"), col="#262D2D", family="Roboto Light", cex=1, pos=4, offset=0)
			text(25,0,format(rangs$Date[start+24],"%b %d %Y"), col="#262D2D", family="Roboto Light", cex=1)
			text(50,0,format(rangs$Date[end],"%b %d %Y"), col="#262D2D", family="Roboto Light", cex=1, pos=2, offset=0)
			#ani.pause(0.05)
			#if (d!=276) {rect(-1,-0.5,60,8.5,col="#FEFEFE", border=NA)}

		}

dev.off()


# Transform pdf in gif via ImageMagick

	system("/opt/local/bin/convert -delay 10 Rplots.pdf oscars2016.gif")





