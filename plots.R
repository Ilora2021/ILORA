pie_data <- read.csv("plot_files/species_categories.csv")

pie_data <- pie_data %>%
  arrange(desc(Invasion.status)) %>%
  mutate(lab.ypos = cumsum(Species) - 0.5*Species)
pie_data$fraction = pie_data$Species / sum(pie_data$Species)
pie_data$ymax = cumsum(pie_data$Species)
pie_data$ymin = c(0, head(pie_data$ymax, n=-1))
pie_data$labelPosition <- (pie_data$ymax + pie_data$ymin) / 2
pie_data$label <- paste0(pie_data$Invasion.status, "\n value: ", pie_data$Species)

# Growth form plot -------
gfdata <-read.csv("plot_files/species_categories1.csv")
gfdata <-data.frame(gfdata)
Species.category <- factor(gfdata$group,level=c('Invasive', 'Naturalized', 
                                                'Casual','Native','Cryptogenic'))
gfp <- ggplot(gfdata, aes(y=value, x=individual,fill=Species.category)) + 
  coord_flip()+
  xlab('Growth forms')+
  ylab('Number of species')+
  theme_light()+
  geom_bar(position="dodge", stat="identity",width=0.5,color="black")+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 12,color = "black"),
    axis.title = element_text(size = 15)
  ) + scale_fill_manual(values = c("Invasive" = "coral",
                                   "Naturalized" = "deepskyblue",
                                   "Casual" = "blue",
                                   "Native" = "forestgreen",
                                   "Cryptogenic"="cornsilk"))

# Classes plot -------
cdata <- read.csv("plot_files/species_lae.csv")
data <- data.frame(cdata)

tgc <- summarySE(cdata, measurevar="value", groupvars=c("occupancy","group"))
Species.category <- factor(tgc$group,level=c('Invasive', 'Naturalized', 
                                             'Casual','Native','Cryptogenic'))

clp <-
  ggplot(tgc, aes(y=value, x=occupancy,fill=Species.category)) + 
  coord_flip()+
  xlab('')+
  ylab('Number of classes occupied')+
  theme_light()+
  geom_bar(position="dodge", stat="identity",width=0.5,color="black")+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,position = position_dodge(0.5))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 12,color = "black"),
    axis.title = element_text(size = 15)
  ) + scale_fill_manual(values = c("Invasive" = "coral",
                                   "Naturalized" = "deepskyblue",
                                   "Casual" = "blue",
                                   "Native" = "forestgreen",
                                   "Cryptogenic"="cornsilk"))

# Eco Uses plot -------
edata <- read.csv("plot_files/species_economics.csv")
edata <- data.frame(
  x=edata$Econ_uses, 
  y=edata$Species
)

ecop <- edata %>%
  mutate(x = fct_reorder(x,y)) %>%
  ggplot( aes(x=x, y=y)) +
  xlab('Economic uses')+
  ylab('Number of species')+
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="black") +
  geom_point( color="red", size=4, alpha=0.6) +
  theme_light()+
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 12,color = "black"),
    axis.title = element_text(size = 15)
  )

# Market Dynamics plot ------
mdata <- read.csv("plot_files/species_market.csv")
mdata <- data.frame(
  x=mdata$name, 
  y=mdata$value)

mktp <- mdata %>%
  mutate(x = fct_relevel(x, 
                         "Native", "Cryptogenic", "Casual", 
                         "Naturalized", "Invasive")) %>%
  ggplot( aes(x=x, y=y, fill=x, color=x)) +
  geom_violin(width=1.2, size=0.2, color="black") +
  #scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_light()+
  coord_flip() +
  xlab("Invasion categories") +
  ylab("Average market price (INR)")+
  theme(
    panel.grid.major.x = element_line(color = "gray"),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "None",
    axis.text = element_text(size = 8,color = "black"),
    axis.title = element_text(size = 10)
  ) + scale_fill_manual(values = c("Invasive" = "coral",
                                   "Naturalized" = "deepskyblue",
                                   "Casual" = "blue",
                                   "Native" = "forestgreen",
                                   "Cryptogenic"="cornsilk"))


# chord plot -----
chdata <- read.csv("plot_files/Book4.csv",header = TRUE,na.strings = "",sep = ",",row.names = 1)

chdata1 <- as.matrix(chdata)

grid.col = c(S1 = "red", S2 = "green", 
             S3 = "blue", S4="cyan", S5="darkblue", S6="gold",
             S7="brown",S8="blueviolet",
             IP1.2="grey",IP1.5="grey",IP1.6="grey",IP1.7="grey",IP2.1="grey",IP2.others="grey",IP2.9="grey",
             IP3.4.5="grey")

chdf = data.frame(from = rep(rownames(chdata1), times = ncol(chdata1)),
                to = rep(colnames(chdata1), each = nrow(chdata1)),
                value = as.vector(chdata1),
                stringsAsFactors = FALSE)
