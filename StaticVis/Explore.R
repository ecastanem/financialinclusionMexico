library(here)
library(tidyverse)
library(readxl)
library(dbplyr)
library(dplyr)
library(ggridges)
library(viridis)
library(haven)

#Read the World Bank Global Findex data and the GDP per capita from the World Development Indicators data: 
WB_GFdata<-read_excel(path=here('Data','FINDEXEXCEL.xlsx'),sheet = 'Data'
                    ,range=cell_rows(1:142009)
                    ,col_names = TRUE
                    ,col_types=c(rep("text",4), rep("numeric", 3)))

WB_WDdata<-read_excel(path=here('Data','WDIEXCEL.xlsx'),sheet = 'Data2'
                      ,range=cell_rows(1:265)
                      ,col_names = TRUE
                      ,col_types=c(rep("text",4), rep("numeric", 58)))

#Graph 1: Evolution of the accounts indicator.

# From Global Findex data we filter the Account (% age 15+) indicator only for countries without NA.
WB_account<-WB_GFdata %>% 
  filter(`Indicator Code`=='account.t.d', !is.na(`2011`),!is.na(`2014`),!is.na(`2017`))

WLD_2011<-WB_account$`2011`[WB_account$`Country Code`=="WLD" & WB_account$`Indicator Code`=='account.t.d']
WLD_2014<-WB_account$`2014`[WB_account$`Country Code`=="WLD" & WB_account$`Indicator Code`=='account.t.d']
WLD_2017<-WB_account$`2017`[WB_account$`Country Code`=="WLD" & WB_account$`Indicator Code`=='account.t.d']
WLD<-c(WLD_2011,WLD_2014,WLD_2017)

#The first 20 records of the dataset belong to groups of countries, so we will exclude them for the first graph. 
regions<-c('ARB','EAS','EAP','EMU','ECS','ECA','HIC','LCN','LAC','LMY','LIC','LMC','MIC','NAC','OED','SAS','SSF','SSA','UMC','WLD')

WB_account<-WB_account %>% 
  group_by(`Country Code`) %>%
  filter(!(`Country Code` %in% regions)) %>% 
  gather(`2011`:`2017`, key='year', value='account.t.d') %>%
  arrange(year, .by_group = TRUE)

#Additionally we add a flag to indicate if the country indicator is above or below the world aggregate level for each year, 
#as well as a flag to identify Mexico.
WB_account$Group<-'1. Countries below the world mean level'
WB_account$Group[WB_account$year==2011 & WB_account$account.t.d>WLD[1]]<-'2. Countries above the world mean level'
WB_account$Group[WB_account$year==2014 & WB_account$account.t.d>WLD[2]]<-'2. Countries above the world mean level'
WB_account$Group[WB_account$year==2017 & WB_account$account.t.d>WLD[3]]<-'2. Countries above the world mean level'
WB_account$MEX<-0
WB_account$MEX[WB_account$`Country Code`=='MEX']<-1

#Make a graph to understand the evolution of the distribution for the accounts indicator. 
p1<-WB_account %>% ggplot(aes(y=year,x=account.t.d,fill=..x..))+
  facet_wrap(~Group,scales = 'free')+
  geom_density_ridges_gradient(scale =1.2, alpha = 0.9,)+
  scale_fill_viridis(name = "Percentage (%)", option = "C")+
  labs(subtitle = "Evolution of the distribution of account ownership indicator in 126 countries.",title="Account ownership at a financial institution or with a mobile-money-service provider", caption="World Bank Group: Global Findex database",y="Year",x="Proportion of account ownership per 100 inhabitants")

ggsave(here("output","plot1.pdf"), p1,width = 30, height = 25, units = "cm")


#Graph 2
# From World Development data we filter the Account (% age 15+) indicator and GDP percapita only for countries without NA.
# From Global Findex data we filter the Account (% age 15+) indicator only for countries without NA.
WB_account<-WB_GFdata %>% 
  filter(`Indicator Code`=='account.t.d', !is.na(`2011`),!is.na(`2014`),!is.na(`2017`))

WB_GDPpc<-WB_WDdata %>% 
  filter(!is.na(`2011`),!is.na(`2014`),!is.na(`2017`))


#We will filter the information at regional levels. 
regions<-c('HIC','LIC','MIC','WLD','MEX')

WB_account<-WB_account %>% 
  group_by(`Country Code`) %>%
  filter(`Country Code` %in% regions) %>% 
  gather(`2011`:`2017`, key='year', value='account.t.d') %>%
  arrange(year, .by_group = TRUE)

WB_GDPpc<-WB_GDPpc %>% 
  group_by(`Country Code`) %>%
  filter(`Country Code` %in% regions) %>% 
  gather(`1960`:`2017`, key='year', value='GDP per capita') %>%
  arrange(year, .by_group = TRUE) %>%
  filter(year %in% c(2011,2014,2017)) %>%
  select(`Country Code`,year,`GDP per capita`)


WB_account<-inner_join(WB_account,WB_GDPpc, by=c('Country Code', 'year'))
names(WB_account)[1]<-"Region Name"
names(WB_account)[2]<-"Region Code"
WB_account<-WB_account %>% 
  group_by(`Region Code`)


#Make a graph to compare Mexico with groups of countries by income. 
p2<-WB_account %>% ggplot(aes(x=year,y=account.t.d,group=`Region Name`))+
  geom_line(aes(color=`Region Name`))+
  geom_point(aes(size=`GDP per capita`,color=`Region Name`),alpha=0.5)+
  labs(subtitle = "Account ownership indicator and GDP per capita by Income Region and year.",title="While most countries have improved, Mexico has not.", caption="World Bank Group: Global Findex and World Development Indicators datasets.",x="Year",y="Proportion per 100 inhabitants")

ggsave(here("output","plot2.pdf"), p2,width = 30, height = 25, units = "cm")

#Read data from the National Financial Inclusion Survery 2018:
ENIF18_df1<-read_excel(path=here('Data','tmodulo.xlsx'),sheet = 'tmodulo'
                       ,range=cell_rows(1:12447)
                       ,col_names = TRUE
                       ,col_types=c(rep("text",221), rep("numeric", 1)))

ENIF18_df2<-read_excel(path=here('Data','tmodulo2.xlsx'),sheet = 'tmodulo2'
                       ,range=cell_rows(1:12447)
                       ,col_names = TRUE
                       ,col_types=c(rep("text",148), rep("numeric", 1)))

# Preparing the data.
#From the table ENIF18_df1 we will get the indicators of account ownership and credit holding, whereas from 
# table ENIF18_df2 we will get the insurance and retirement funds holdings. 

MEX_Inhabitants<-sum(ENIF18_df1$FAC_PER)
MEX_Inhabitants_d<-ENIF18_df1 %>% select('SEXO', 'TLOC', 'FAC_PER') %>%
  group_by(.dots=c('SEXO', 'TLOC')) %>%
  summarise(Inhabitants=sum(FAC_PER))

MEX_accounts<-ENIF18_df1 %>% 
  filter(P5_4=='1' | P5_5=='1') %>%
  select('SEXO', 'TLOC', 'FAC_PER') %>%
  group_by(.dots=c('SEXO', 'TLOC')) %>%
  summarise(account=sum(FAC_PER))

MEX_credits<-ENIF18_df1 %>% 
  filter(P6_3=='1' | P6_4=='1') %>%
  select('SEXO', 'TLOC', 'FAC_PER') %>%
  group_by(.dots=c('SEXO', 'TLOC')) %>%
  summarise(credits=sum(FAC_PER))

MEX_insurance<-ENIF18_df2 %>% 
  filter(P8_1=='1' | P8_2=='1') %>%
  select('SEXO', 'TLOC', 'FAC_PER') %>%
  group_by(.dots=c('SEXO', 'TLOC')) %>%
  summarise(insurance=sum(FAC_PER))

MEX_retirement<-ENIF18_df2 %>% 
  filter(P9_1=='1') %>%
  select('SEXO', 'TLOC', 'FAC_PER') %>%
  group_by(.dots=c('SEXO', 'TLOC')) %>%
  summarise(retirement=sum(FAC_PER))

MEX_indicators<-MEX_Inhabitants_d %>% 
  inner_join(MEX_accounts, by=c('SEXO','TLOC')) %>%
  inner_join(MEX_credits, by=c('SEXO','TLOC')) %>%
  inner_join(MEX_insurance, by=c('SEXO','TLOC')) %>%
  inner_join(MEX_retirement, by=c('SEXO','TLOC'))
  

MEX_indicators<-MEX_indicators %>% 
  mutate(accounts_p=100*account/Inhabitants) %>%
  mutate(credits_p=100*credits/Inhabitants) %>%
  mutate(insurance_p=100*insurance/Inhabitants) %>%
  mutate(retirement_p=100*retirement/Inhabitants) %>%
  select('SEXO', 'TLOC', 'accounts_p','credits_p','insurance_p','retirement_p')

names(MEX_indicators)<-c('SEXO', 'TLOC', 'Deposit account','Credit','Insurance','Retirement account')

MEX_indicators<-MEX_indicators %>% 
  group_by(.dots=c('SEXO', 'TLOC')) %>%
  gather(`Deposit account`:`Retirement account`, key='Financial Service', value='Indicator')

MEX_indicators$SEXO[MEX_indicators$SEXO=='1']<-'Male'
MEX_indicators$SEXO[MEX_indicators$SEXO=='2']<-'Female'
MEX_indicators$TLOC[MEX_indicators$TLOC=='1']<-'4. 100,000+'
MEX_indicators$TLOC[MEX_indicators$TLOC=='2']<-'3. 15,000-99,999'
MEX_indicators$TLOC[MEX_indicators$TLOC=='3']<-'2. 2,500-14,999'
MEX_indicators$TLOC[MEX_indicators$TLOC=='4']<-'1. < 2,500'

names(MEX_indicators)<-c('Gender', 'Community Size', 'Financial Service', 'Indicator')


p3<-MEX_indicators %>% ggplot(aes(y=Indicator,x=`Community Size`,fill=`Gender`))+
  facet_wrap(~`Financial Service`,scales = 'free')+
  ylim(0,75)+
  geom_bar(stat='Identity',position=position_dodge())+
  geom_text(aes(label=round(Indicator,digits=2)),position=position_dodge(0.9), vjust=-1)+
  labs(subtitle = "Finacial services adoption by gender, community size and services.",title="Financial Services adoption in Mexico as of 2018.", caption="INEGI: National Survery of Financial Inclusion 2018.",x="Community size by number of inhabitants",y="Percentage of population between 18 and 70 years with a financial service account.")

ggsave(here("output","plot3.pdf"), p3,width = 30, height = 25, units = "cm")
