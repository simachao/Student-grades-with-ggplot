#<# Init -------------------------------------------------
csrClearAll()
csrSourceFunctions(getwd(),csrPathSep)
csrSourceFunctions('/Users/csima/Documents/R/A00',csrPathSep)

library(ggplot2)
library(gcookbook)
library(reshape2)
library(grid)
library(gridExtra)
#># Init -------------------------------------------------


#<# multiplot-------------------------------------------
# This block deals with multiplot
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#># multiplot-------------------------------------------

#<# Read csv data-------------------------------------------
# This block deals with Read csv data
grades <- c(6,8)
multiple_anwers <- list(grade6=c(6),grade8=c(6))
classes <- c('6K','6V','6P','8Se','8Cs','8Ni','8Sn','8Li')
legal_anwers = c('A','B','C','D','E')

survey<- list()
for (i in classes) {
	filename = paste('data',csrPathSep,'survey_',i,'.csv',sep='')
	survey[[i]] = read.table(filename, header = FALSE, sep = ",",stringsAsFactors=FALSE)
	numOfStudents = dim(survey[[i]])[2]
	survey[[i]] = survey[[i]][,2:numOfStudents]
	
}

#># Read csv data-------------------------------------------

#<# Count and check errors-------------------------------------------
# This block deals with Count and check errors
counts <- list()
for (i in classes) {
	numOfQs = dim(survey[[i]])[1]
	numOfStudents = dim(survey[[i]])[2]
	
	counts[[i]] = as.data.frame(matrix(0,nrow=numOfQs,ncol=6),row.names=paste('Q',seq(1,numOfQs),sep=''))
	# names(counts[[i]]) = c('A','B','C','D','E','NA')
	names(counts[[i]]) = c(legal_anwers,'NA')
	
	gradeNow = substr(i, 1, 1)
	q_allowed_mul = multiple_anwers[[paste('grade',gradeNow,sep='')]]
	
	for (j in 1:numOfQs) {
		for (k in 1:numOfStudents) {
			answer = trim_space(survey[[i]][j,k])
			# cat('class',i,'grade',gradeNow,'question',j,'student',k,'answer',answer,'\n',sep=' ')
			
			if (answer %in% legal_anwers) {
				counts[[i]][[j,answer]]= counts[[i]][[j,answer]]+1
			}
			else {
				if (j %in% q_allowed_mul) {
					answer2 = strsplit(answer, ' AND ', fixed = TRUE)
					# cat('class',i,'grade',gradeNow,'question',j,'student',k,'answer',answer,'\n',sep=' ')
					# only two types are allowed: "A AND B" or missing altoghter ==> update: 'A AND B AND C' also allowed
					answer3 = unlist(answer2[1])
					if (length(answer3) == 0) {
						counts[[i]][[j,'NA']]= counts[[i]][[j,'NA']]+1
					} 
					# else if (length(answer3) == 2) {
					# 	counts[[i]][[j,answer3[1]]]= counts[[i]][[j,answer3[1]]]+1
					# 	counts[[i]][[j,answer3[2]]]= counts[[i]][[j,answer3[2]]]+1
					# }
					# else {
					# 	cat('class',i,'grade',gradeNow,'question',j,'student',k,'answer',answer,'\n',sep=' ')
					# 	stop('')
					# }
					else {
						for (t in 1:length(answer3) ) {
							counts[[i]][[j,answer3[t]]]= counts[[i]][[j,answer3[t]]]+1
						}
						
					}

				}
				else {
					counts[[i]][[j,'NA']]= counts[[i]][[j,'NA']]+1
				}
			}
		}
		
		
	}

}
#># Count and check errors-------------------------------------------
# print(counts)



#<# Combine classes for the same grade -------------------------------------------
# This block deals with Combine classes for the same grade 
classesMore = paste(grades,'All',sep='')
countsMore = list()
for (g in grades) {
	className = paste(g,'All',sep='')
	bool_thisGrade = grepl(paste('^',g,sep=''),classes)
	firstClassThisGrade = which(bool_thisGrade)[1]
	countsMore[[className]] = counts[[firstClassThisGrade]]
	
	if (firstClassThisGrade < length(counts)) {
		for (k in (firstClassThisGrade+1):length(counts)) {
			if (bool_thisGrade[k]) {
				countsMore[[className]] = countsMore[[className]]+counts[[k]]
			}
		}
	}
}
counts = c(counts,countsMore)
classes = c(classes,classesMore)
#># Combine classes for the same grade -------------------------------------------

# classes = c('6All')
# library(plotrix)
#<# Plot-------------------------------------------
# This block deals with Plot
scores = c(legal_anwers,'NA')
for (i in classes) {
	newCount = as.data.frame(t(counts[[i]]))
	newCount = cbind(scores,newCount)
	
	newCountMelt = melt(newCount,id="scores",variable.name = "Q",value.name = "count")
	gg = ggplot(newCountMelt, aes(x=scores,y=count)) + geom_bar(stat="identity",fill="lightblue", colour="black")+ facet_grid(Q ~ .) + xlab("") + ylab("") + ggtitle(i)
	# print(gg)	
	# ggsave(paste(i,'.pdf',sep=''),gg,width=5,height=15)
	
	ggPie = ggplot(newCountMelt,aes(x=factor(1),y=count,fill = scores))+geom_bar(stat="identity",width = 1)+theme(axis.text.x=element_blank(),axis.text.y=element_blank())+ coord_polar(theta="y")+ facet_grid(Q ~ .) + ylab("")+ xlab("") + guides(fill=guide_legend(title=NULL)) #+ theme(legend.position="none")		
	# print(ggPie)
	
	pdf(paste(i,'.pdf',sep=''),width=5,height=10)
	ggBoth = grid.arrange(ggPie,gg,ncol=2,widths=c(1,2))
	dev.off()
	
	# ggBoth = multiplot(gg,ggPie,ncol=2)
	# print(ggBoth)
	# ggsave(paste(i,'.pdf',sep=''),ggBoth,width=5,height=10)
	
	# numPies = dim(newCount)[2]
	# ggPies = list()
	# for (j in 2:numPies) {
	# 	ggPies[[j-1]] = ggplot(newCount,aes_string(x='factor(1)',y=paste('Q',j-1,sep=""),fill = 'scores'))+geom_bar(stat="identity",width = 1)+ coord_polar(theta="y") + ylab("")+ xlab("") + guides(fill=guide_legend(title=NULL)) + theme(legend.position="none")		
	# 	# ggsave(paste(i,'pie_',paste('Q',j-1,sep=""),'.pdf',sep=''),ggPies[[j-1]],width=5,height=5)
	# }
	# plot5<-grid.arrange(gg, do.call(arrangeGrob,c(ggPies, ncol=1)), ncol=2, widths=c(2,1))
	# print(plot5)	
}


#># Plot-------------------------------------------
