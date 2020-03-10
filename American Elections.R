rm(list=ls())
setwd("/home/arduin/Documents/Data Science/LinkdIN/American Elections 2016")

# Library -----------------------------------------------------------------
load_pkg <- c('dplyr','ggplot2','RColorBrewer','plotly',"readr", "rgeos", "rgdal", 
              "maptools", "maps", "RColorBrewer", "scales","nnet")

myInstall <- function(pkg, ...){
  if(!(pkg %in% installed.packages()[,1])){
    install.packages(pkg)
  }
  return(library(pkg,...))
  
}

purrr::walk(load_pkg, myInstall, character.only = TRUE, warn.conflicts = TRUE)


# Data --------------------------------------------------------------------
primary <- read.csv("primary_results.csv", stringsAsFactors = FALSE)
demographics <- read.csv("county_facts.csv", stringsAsFactors = FALSE)


demographics %<>% filter(state_abbreviation != "") %>% 
  select(state_abbreviation = state_abbreviation, county = area_name, 
         income = INC110213, hispanic = RHI725214,
         white= RHI825214, college = EDU685213, density = POP060210, 
         poverty = PVY020213, femsex = SEX255214, nfirms = SBO001207, age65 = AGE775214) %>% 
  mutate(county = gsub(" County", "", county), nocollege = (100 - college)) 


# Split dataframe by Parties
primaryDem <- primary %>%  #get the winners and the fraction of votes the won
  filter(party == "Democrat") %>% 
  group_by(county, state_abbreviation) %>% 
  summarize(winner = candidate[which.max(fraction_votes)],
            Vote = max(fraction_votes) * 100,
            votes = max(votes))

primaryRep <- primary %>%  #get the winners and the fraction of votes the won
  filter(party == "Republican") %>% 
  group_by(county, state_abbreviation) %>% 
  summarize(winner = candidate[which.max(fraction_votes)],
            Vote = max(fraction_votes) * 100,
            votes = max(votes))




# Data Analysis Republicans -------------------------------------------------
primElec_rep <- inner_join(primaryRep, demographics, by = c("county","state_abbreviation"))

# Direct Votes
primElec_rep_dv <- aggregate(primElec_rep[,'votes'], by=list(candidates = primElec_rep$winner), FUN = sum, na.rm=TRUE, na.action=NULL)

p <- primElec_rep_dv %>% ggplot(aes(x=reorder(candidates,-votes), y=votes, fill = candidates)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("#F86D7F", "#550000", "#F8001C", "#BD0000", "#942F14"), drop = FALSE) +
  labs(title = 'Direct Votes', x = 'Candidates', y = 'Votes') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.position = "none",
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))

p

# Counties by Hispanic and Fraction Votes
p <- primElec_rep %>% ggplot(aes(x = Vote, y = poverty, color = winner)) +
  geom_point(aes(colour = winner, size = poverty)) +
  scale_color_manual(values=c("#F86D7F", "#550000", "#F8001C", "#BD0000", "#942F14")) +
  labs(title = 'Counties by Winner - Votes x Poverty', x = 'Fraction Votes', y = 'Poverty') +
  guides(size=FALSE, shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))
p 

# Counties by Hispanic and Fraction Votes
p <- primElec_rep %>% ggplot(aes(x = Vote, y = white, color = winner)) +
  geom_point(aes(colour = winner, size = Vote)) +
  scale_color_manual(values=c("#F86D7F", "#550000", "#F8001C", "#BD0000", "#942F14")) +
  labs(title = 'Counties by Winner - Votes x Hispanics Population', x = 'Fraction Votes', y = 'Hispanic') +
  guides(size=FALSE, shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))
p  


# Boxplot Income
p <- primElec_rep %>% ggplot(aes(x = winner, y = income, fill = winner)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2, outlier.color = 'black') +
  scale_fill_manual(values=c("#F86D7F", "#550000", "#F8001C", "#BD0000", "#942F14")) +
  labs(title = 'Boxplot - County Income', x = 'Winners', y = 'Income') +
  guides(size=FALSE, shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))
p

# Map Election
winner_colors <- c("#F86D7F", "#550000", "#F8001C", "#BD0000", "#942F14")

map.county <- map_data('county')
counties <- map.county %>%
  select(long, lat, group, order, 
         state = region, 
         county = subregion)

primElec_rep[,1:2] <- lapply(primElec_rep[,1:2], tolower)

map.county <- inner_join(primElec_rep, counties, by = c('county'))
map.county$winner <- as.factor(map.county$winner)

p <- map.county %>% ggplot(aes(x = long, y = lat, group = group, fill = winner)) + 
  geom_polygon(color = 'gray90', size=0.5) +
  coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = winner_colors) +
  theme_light(base_size=12) +
  labs(title = 'Republican Primary Election 2016') +
  theme(strip.text.x = element_text(size=12, colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold"),
        legend.title = element_blank(),

                legend.position = c(.92, .50))
  

p

# Model Republicans - Winner
primaryRep_Mod <- primary %>%  #get the winners and the fraction of votes the won
  filter(party == "Republican") %>% 
  group_by(county, state_abbreviation)

primaryRep_Mod <- inner_join(primaryRep_Mod, demographics, by = c("county", "state_abbreviation"))
primaryRep_Mod$candidate <- as.factor(primaryRep_Mod$candidate)



repModel <- multinom(candidate ~ income + nocollege + hispanic + poverty, data = primaryRep_Mod)
repModel

# Data Analysis Democrats -----------------------------------------------
primElec_dem <- inner_join(primaryDem, demographics, by = c("county","state_abbreviation"))

# Direct Votes
primElec_dem_dv <- aggregate(primElec_dem[,'votes'], by=list(candidates = primElec_dem$winner), FUN = sum, na.rm=TRUE, na.action=NULL)

p <- primElec_dem_dv %>% ggplot(aes(x=reorder(candidates,-votes), y=votes, fill = candidates)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("#3E41CA", "#2B98ED"), drop = FALSE) +
  labs(title = 'Direct Votes', x = 'Candidates', y = 'Votes') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.position = "none",
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))

p

# Counties by Hispanic and Fraction Votes
p <- primElec_dem %>% ggplot(aes(x = Vote, y = poverty, color = winner)) +
  geom_point(aes(shape = winner, size = Vote)) +
  scale_color_manual(values=c("#3E41CA", "#2B98ED")) +
  labs(title = 'Counties by Winner - Votes x Poverty', x = 'Fraction Votes', y = 'Poverty') +
  guides(size=FALSE, shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))
p 

# Counties by Hispanic and Fraction Votes
p <- primElec_dem %>% ggplot(aes(x = Vote, y = hispanic, color = winner)) +
  geom_point(aes(shape = winner, size = Vote)) +
  scale_color_manual(values=c("#3E41CA", "#2B98ED")) +
  labs(title = 'Counties by Winner - Votes x Hispanics Population', x = 'Fraction Votes', y = 'Hispanic') +
  guides(size=FALSE, shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))
p  


# Boxplot Income
p <- primElec_dem %>% ggplot(aes(x = winner, y = income, fill = winner)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=2, outlier.color = 'black') +
  scale_fill_manual(values=c("#3E41CA", "#2B98ED")) +
  labs(title = 'Boxplot - County Income', x = 'Winners', y = 'Income') +
  guides(size=FALSE, shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=18))
p

# Map Election
winner_colors <- c("#3E41CA", "#2B98ED")

map.county <- map_data('county')
counties <- map.county %>%
  select(long, lat, group, order, 
         state = region, 
         county = subregion)

primElec_dem[,1:2] <- lapply(primElec_dem[,1:2], tolower)

map.county <- inner_join(primElec_dem, counties, by = c('county'))
map.county$winner <- as.factor(map.county$winner)

p <- map.county %>% ggplot(aes(x = long, y = lat, group = group, fill = winner)) + 
  geom_polygon(color = 'gray90', size=0.5) +
  coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = winner_colors) +
  theme_light(base_size=12) +
  labs(title = 'Democract Primary Election 2016') +
  theme(strip.text.x = element_text(size=12, colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold"),
        legend.title = element_blank(),
        legend.position = c(.92, .50))


p

# Model Democrats - Winner
primaryDem_Mod <- primary %>%  #get the winners and the fraction of votes the won
  filter(party == "Democrat") %>% 
  group_by(county, state_abbreviation)

primaryDem_Mod <- inner_join(primaryDem_Mod, demographics, by = c("county", "state_abbreviation"))
primaryDem_Mod$candidate <- as.factor(primaryDem_Mod$candidate)

demModel <- multinom(candidate ~ income + college + hispanic + poverty, data = priDT)
summary(demModel)
