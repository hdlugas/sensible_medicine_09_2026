
library(ggplot2)
library(ggpubr)
library(stringr)
library(flextable)
library(dplyr)
'%nin%' = Negate('%in%')


##### import datasets from CDC WONDER queried on 04/26/2026 #####
d1 = read.delim('opioid_analysis/substack_article/data/mcd_1999-2012.tsv', header=T, sep='\t')
d2 = read.delim('opioid_analysis/substack_article/data/mcd_2013-2020.tsv', header=T, sep='\t')
d3 = read.delim('opioid_analysis/substack_article/data/mcd_2018-2024.tsv', header=T, sep='\t')
d1$Notes=NULL; d2$Notes=NULL; d3$Notes=NULL
d3$Crude.Rate.Lower.95..Confidence.Interval=NULL; d3$Crude.Rate.Upper.95..Confidence.Interval=NULL
d = rbind(d1,d2,d3)
d = d[which(d$Five.Year.Age.Groups != 'Not Stated'),]
d = d[which(d$Five.Year.Age.Groups != ''),]
d = d[which(sapply(d$Crude.Rate,function(x){substr(x,1,1)}) %in% 0:9),]
d$Crude.Rate = as.numeric(d$Crude.Rate)
d$Population = as.integer(d$Population)
d$age.group = NA
d$age.group[which(d$Five.Year.Age.Groups %in% c('< 1 year', '1-4 years', '5-9 years'))] = '0-9'
d$age.group[which(d$Five.Year.Age.Groups %in% c('10-14 years', '15-19 years'))] = '10-19'
d$age.group[which(d$Five.Year.Age.Groups %in% c('20-24 years', '25-29 years'))] = '20-29'
d$age.group[which(d$Five.Year.Age.Groups %in% c('30-34 years', '35-39 years'))] = '30-39'
d$age.group[which(d$Five.Year.Age.Groups %in% c('40-44 years', '45-49 years'))] = '40-49'
d$age.group[which(d$Five.Year.Age.Groups %in% c('50-54 years', '55-59 years'))] = '50-59'
d$age.group[which(d$Five.Year.Age.Groups %in% c('60-64 years ', '65-69 years'))] = '60-69'
d$age.group[which(d$Five.Year.Age.Groups %in% c('70-74 years', '75-79 years'))] = '70-79'
d$age.group[which(d$Five.Year.Age.Groups %in% c('80-84 years', '85-89 years'))] = '80-89'
d$age.group[which(d$Five.Year.Age.Groups %in% c('90-94 years', '95-99 years'))] = '90-99'
d$age.group[which(d$Five.Year.Age.Groups %in% c('100+ years'))] = '>100'
# tmp = cbind(d$Multiple.Cause.of.death,d$Multiple.Cause.of.death.Code)
# tmp = tmp[!duplicated(tmp[,1]),]
# tmp = tmp[order(tmp[,2]),]
# View(tmp)
# mcds.of.interest = c(paste0('T40.',1:6),paste0('X',60:84))
mcds.of.interest = c(paste0('T40.',1:4),'T40.6')
d = d[which(d$Multiple.Cause.of.death.Code %in% mcds.of.interest),]
d = d[which(d$Year %in% 2012:2024),]
d.agg = aggregate(cbind(Deaths,Population) ~ Year + Multiple.Cause.of.death.Code, data=d, FUN=sum)
d.agg = d.agg[order(d.agg$Multiple.Cause.of.death.Code,d.agg$Year),]



######### generate barplot showing distribution of age for people with opioid overdose death in 2023 #########
d.2023 = d[which(d$Year==2023 & grepl("T",d$Multiple.Cause.of.death.Code)==T),]
d.2023.agg = d.2023 %>%
  group_by(age.group) %>%
  summarise(Deaths=sum(Deaths)) %>%
  mutate(percent=Deaths/sum(Deaths)*100)
d.2023.agg$age.group = factor(d.2023.agg$age.group, levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"))

ggplot(d.2023.agg, aes(x = age.group, y = Deaths)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent,1),"%")), vjust=-0.5, size=4) +
  labs(x="Age Group", y="Deaths", title="Deaths by Age Group, 2023") +
  theme(panel.background=element_blank(),
        panel.border=element_rect(fill=NA, color='black'),
        plot.title=element_text(hjust=0.5, size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave('opioid_analysis/substack_article/figures/2023_age_distribution.png', dpi=300)


##### include rows corresponding to any suicide (e.g. X60 through X84) #####
opioid.idxs = grepl("T", d.agg$Multiple.Cause.of.death.Code)
opioid.rows = aggregate(cbind(Deaths,Population) ~ Year, data=d.agg[opioid.idxs,], FUN=sum)
opioid.rows$Multiple.Cause.of.death.Code = "T40.1-T40.4, T40.6"
opioid.rows = opioid.rows[,c("Year", "Multiple.Cause.of.death.Code", "Deaths", "Population")]
d.agg = rbind(d.agg, opioid.rows)
d.agg = d.agg[which(d.agg$Multiple.Cause.of.death.Code %nin% c(paste0('X',60:84),paste0('T40.',1:6))),]


##### include mortality and opioid rx rates (pulled from https://www.ama-assn.org/system/files/opioid-prescription-by-state-trends.pdf on 04/26/2026) #####
d.agg$opioid.deaths.per.100k = d.agg$Deaths / d.agg$Population * 100000
d.rx = data.frame(Year=2012:2024,
                  n.retail.opioid.rxs = c(260457577,251760372,244476505,227800591,215990560,192688222,168850967,153956683,143377621,139614372,131896599,125920878,125658848),
                  MME.retail = c(239359537622,228867866407,221247729267,209628456283,197195514127,170498248813,141291026235,120451438593,110186994198,102562808527,94934969869,88290423082,83883087656))

d.agg$n.retail.opioid.rxs.per.100.million = d.rx$n.retail.opioid.rxs[match(d.agg$Year,d.rx$Year)] / (10^8)
d.agg$MME.retail.per.100.billion = d.rx$MME.retail[match(d.agg$Year,d.rx$Year)] / (10^11)
unique(d.agg$Multiple.Cause.of.death.Code)
d.agg = d.agg[which(d.agg$Multiple.Cause.of.death.Code != 'T40.5'),]
# heroin, other opioids, methadone, other synthetic narcotics, other and unspecified narcotics

d2 = data.frame(Year = 2012:2024,
                Rate = c(d.agg$opioid.deaths.per.100k,
                         d.agg$n.retail.opioid.rxs.per.100.million,
                         d.agg$MME.retail.per.100.billion),
                Type = c(rep('Opioid-related overdoses per 100k',13),
                         rep('Number of retail opioid prescriptions per 100 million',13),
                         rep('MME dispensed at retail pharmacies per 100 billion',13)))

ggplot(data=d2, aes(x=Year, y=Rate, group=Type, color=Type)) +
  geom_line(linewidth=1.2) +
  theme(panel.background=element_blank(),
        panel.border=element_rect(fill=NA,color='black'),
        axis.text.x=element_text(angle=45, vjust=0.5, size=12),
        axis.text.y=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=13),
        legend.key.width=unit(2,'cm')) +
  scale_x_continuous(breaks=2012:2024) +
  scale_color_manual(values=c('Opioid-related overdoses per 100k'='black',
                              'Number of retail opioid prescriptions per 100 million'='red',
                              'MME dispensed at retail pharmacies per 100 billion'='blue')) +
  labs(color='') +
  guides(color=guide_legend(override.aes=list(linewidth=1.2)))
ggsave(filename='opioid_analysis/substack_article/figures/rates.png', dpi=400, width=12, height=10)


d$MCD = paste0(d$Multiple.Cause.of.death.Code,' (',d$Multiple.Cause.of.death,')')

ggplot(d[which(d$Year %in% 2012:2024),], aes(x=Year, fill=MCD, y=Crude.Rate)) +
  geom_bar(stat='identity', position = position_dodge(width=0.9)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color='black'),
        legend.title = element_text(hjust=0.5, size=14),
        legend.text = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=16),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12)) +
  scale_x_continuous(breaks=2012:2024) +
  labs(x='Year', y='Death Rate per 100,000', fill='MCD Code')
ggsave(filename='opioid_analysis/substack_article/figures/dist_of_types_of_opioid_ODs.png', dpi=400, width=12, height=10)


mod1 = lm(opioid.deaths.per.100k ~ n.retail.opioid.rxs.per.100.million, data=d.agg)
mod2 = lm(opioid.deaths.per.100k ~ MME.retail.per.100.billion, data=d.agg)

sprintf('%0.3f', cor(d.agg$opioid.deaths.per.100k, d.agg$n.retail.opioid.rxs.per.100.million))
sprintf('%0.3f', cor(d.agg$opioid.deaths.per.100k, d.agg$MME.retail.per.100.billion))

paste0('Effect size: ',sprintf('%0.3f',summary(mod1)$coefficients[2,1]),
       ' [',sprintf('%0.3f',confint(mod1)[2,1]),',',
       sprintf('%0.3f',confint(mod1)[2,2]),']; P=',
       sprintf('%0.3f',summary(mod1)$coefficients[2,4]))

paste0('Effect size: ',sprintf('%0.3f',summary(mod2)$coefficients[2,1]),
       ' [',sprintf('%0.3f',confint(mod2)[2,1]),',',
       sprintf('%0.3f',confint(mod2)[2,2]),']; P=',
       sprintf('%0.3f',summary(mod2)$coefficients[2,4]))
