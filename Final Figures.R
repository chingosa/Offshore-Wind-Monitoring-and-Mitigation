library(openxlsx) 
library(ggplot2)
library(ggrepel)
library(ggbreak) 
library(patchwork)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
library(gtable)
library(gridExtra)



file_path <- "C:/Users/24chi/Desktop/OffshoreWind/DA_Classifications_Final.xlsx"

classData <- read_excel(file_path, sheet = 1) #Grabbing the data from excel
metaData <- read_excel(file_path, sheet = 2) #Grabbing the data from excel

classData$prj <- substr(classData$Code,1,3)
classData$comit <- as.numeric(substr(classData$Code,4,4))

classData$date <- metaData$prjDate[match(classData$prj, metaData$prjCode)]
classData$date <- as.Date(classData$date, format = "%m/%d/%Y")

lineCounts <- sort(table(classData$prj))
  
  count_df <- data.frame(Prj = names(lineCounts), Count = as.vector(lineCounts))
  count_df$date <- metaData$prjDate[match(count_df$Prj, metaData$prjCode)]
  count_df$Dev <- metaData$Developer[match(count_df$Prj, metaData$prjCode)]
  count_df$Doc <- metaData$DocType[match(count_df$Prj, metaData$prjCode)]
  count_df$Consult <- metaData$Consultantcy[match(count_df$Prj, metaData$prjCode)]
  prjNames <- unique(classData$prj)
  for (i in prjNames){
    if (!(i %in% as.vector(count_df$Prj))){
      count_df <- rbind(count_df, data.frame(
        Prj = i, 
        Count = 0, 
        date = metaData$prjDate[match(i, metaData$prjCode)],
        Dev = metaData$Developer[match(i, metaData$prjCode)], 
        Doc = metaData$DocType[match(i, metaData$prjCode)],
        Consult = metaData$Consultantcy[match(i, metaData$prjCode)]))
    }
  }
  if ("BIW" %in% count_df$Prj){ 
    count_df$date[count_df$Prj == "BIW"] <- as.character('10/01/2020')
  }
  

  count_df$date <- as.Date(count_df$date, format = "%m/%d/%Y")
  count_df <- data.frame(count_df[order(count_df$date),], row.names = NULL)
  count_df$yr <- decimal_date(count_df$date)
  
  
  lm_model <- lm(count_df$Count[count_df$Prj != "BIW"] ~ count_df$yr[count_df$Prj != "BIW"])
  slope <- coef(lm_model)[2]
  intercept <- coef(lm_model)[1]
  line_formula <- paste("y =", round(slope, 2), "x +", round(intercept, 0))
  
  count_df$pred[count_df$Prj != "BIW"] <- predict(lm_model)
  
  plt<-ggplot(count_df, aes(date, Count, label = Prj, color = Dev, shape = Doc))+
    geom_point()+ geom_text_repel(show.legend = FALSE)+theme_classic()+
    geom_line(aes(date, pred, label=NULL, shape=NULL), color='black')+
    geom_text(aes(x = min(date), y = max(Count), label = line_formula),
              hjust = -0.1, vjust = 2, color = "black", size = 4) +
    labs(title = NULL, x ="Document release date", y = "Count", color = "Developer", shape = "Document")
  
  
  

  
  if ("BIW" %in% count_df$Prj){ 
    plt<-plt + geom_segment(aes(
      x=date[Prj == "BIW"], 
      y=Count[Prj == "BIW"], 
      xend=as.Date(-Inf), 
      yend=Count[Prj == "BIW"]),
      color = 'black',
      arrow = arrow(length=unit(0.5, 'cm')))

  }
  

width <- 8
height <- width*10/16
plt
ggsave("fig2.png", plt, height = height, width =width, dpi = 300)





library(dplyr)
difData <- data.frame(prj = c('Revolution Wind', "South Fork", "Vineyard Wind", "Ocean Winds 1", 'Revolution Wind', "South Fork", "Vineyard Wind", "Ocean Winds 1"),
                      Count = c(199, 173,33, 294, 134, 29, 65, 20),
                      Doc = c('DEIS', 'DEIS','DEIS','DEIS', 'FEIS','FEIS','FEIS','FEIS'))

difData2 <- data.frame(prj = c('Revolution Wind', "South Fork", "Vineyard Wind", "Ocean Winds 1", 'Revolution Wind', "South Fork", "Vineyard Wind", "Ocean Winds 1"),
                       Count = c(199/333, 173/202,33/98, 294/314, 134/333, 29/202, 65/98, 20/314),
                       Doc = c('DEIS', 'DEIS','DEIS','DEIS', 'FEIS','FEIS','FEIS','FEIS'))

difData2$prj <- factor(difData2$prj, levels = c('Revolution Wind', "South Fork", "Vineyard Wind", "Ocean Winds 1"))
difData2$prj2 <- factor(difData2$prj, levels = c("Vineyard Wind", "South Fork","Ocean Winds 1", 'Revolution Wind'))

difData2$Doc <- factor(difData2$Doc, levels = rev(c('DEIS', 'FEIS')))
difData2$Count <- difData2$Count*100
library(ggpubr)
p1<-ggplot(difData2, aes(prj, Count, fill = Doc))+ 
  geom_bar(stat = 'identity')+
  labs(title = NULL, x = "Project (in SAP start date order)", y = "%", fill = NULL)+
  theme_classic()+theme(legend.position = 'none')
p2<-ggplot(difData2, aes(prj2, Count, fill = Doc))+ 
  geom_bar(stat = 'identity')+
  labs(title = NULL, x = "Project (in FEIS release date order)", y = "%", fill = NULL)+
  theme_classic()
legend_gtable <- get_legend(p2)
p2<-p2 + theme(legend.position = 'none')

fig6<-ggdraw()+
  draw_plot(p1,x = 0,y = 0,width = 0.5,height = 1)+
  draw_plot(p2,x = 0.5,y = 0,width = 0.5,height = 1)+
  draw_plot(legend_gtable, x=.65, y=.14, width = .15, height=.15)

fig6<-ggdraw()+
  draw_plot(p2,x = 0,y = 0,width = 1,height = 1)+
  draw_plot(legend_gtable, x=.78, y=.12, width = .15, height=.15)

fig6

width <- 8
height <- width*10/16
ggsave("fig6.png", fig6, height = height, width =width, dpi = 300)









