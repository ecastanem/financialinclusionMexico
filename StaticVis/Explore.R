library(here)
library(tidyverse)
library(readxl)
library(dbplyr)
library(dplyr)
library(directlabels)
library(ggridges)
library(treemapify)
library(gridExtra)
library(grid)
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
p1<-WB_account %>% ggplot(aes(y=year,x=account.t.d/100,fill=..x..))+
  geom_density_ridges_gradient(scale =1.25, alpha = 0.9,)+
  scale_fill_viridis(name = "Accounts", option = "C")+
  labs(title = "Evolution of the distribution of account ownership indicator in 126 countries.",subtitle="Every year the account ownership at a financial institution or with a mobile-money-service provider in the world is increasing.", caption="World Bank Group: Global Findex database",y="Year",x="Average number of accounts per inhabitant")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.subtitle =element_text(color = "grey20", size = 15, angle = 0, hjust = 0, vjust = .5, face = "plain"),
        plot.title =element_text(color = "grey20", size = 20, angle = 0, hjust = 0, vjust = .5, face = "bold"))

ggsave(here("output","plot1.pdf"), p1,width = 35, height = 25, units = "cm")


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


region_colors<-c("High income" = "blue4","Middle income" = "blue3","Low income" = "blue2","World" = "grey50", "Mexico" = "green4")

#Make a graph to compare Mexico with groups of countries by income. 
p2<-WB_account %>% ggplot(aes(x=year,y=account.t.d/100, group=`Region Name`))+
  geom_line(aes(color=`Region Name`), show.legend = FALSE)+
  geom_dl(aes(label = `Region Name`), method=list(dl.trans(x = x + 0.5), "last.points", cex = 0.8))+
  geom_point(aes(size=`GDP per capita`,color=`Region Name`),alpha=0.5)+
  scale_colour_manual(values=region_colors, guide=FALSE)+
  labs(subtitle = "Account ownership indicator and GDP per capita by Income Region and year.",title="While most countries have improved, Mexico has not.", caption="World Bank Group: Global Findex and World Development Indicators datasets.",x="Year",y="Average number of accounts per inhabitant")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.subtitle =element_text(color = "grey20", size = 15, angle = 0, hjust = 0, vjust = .5, face = "plain"),
        plot.title =element_text(color = "grey20", size = 20, angle = 0, hjust = 0, vjust = .5, face = "bold"))


ggsave(here("output","plot2.pdf"), p2,width = 35, height = 25, units = "cm")

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

ENIF18_df1$TLOC[ENIF18_df1$TLOC==2]<-1
ENIF18_df1$TLOC[ENIF18_df1$TLOC==3]<-2
ENIF18_df1$TLOC[ENIF18_df1$TLOC==4]<-2
ENIF18_df2$TLOC[ENIF18_df2$TLOC==2]<-1
ENIF18_df2$TLOC[ENIF18_df2$TLOC==3]<-2
ENIF18_df2$TLOC[ENIF18_df2$TLOC==4]<-2

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
MEX_indicators$TLOC[MEX_indicators$TLOC=='1']<-'> 15,000'
MEX_indicators$TLOC[MEX_indicators$TLOC=='2']<-'<= 14,999'

names(MEX_indicators)<-c('Gender', 'Community Size', 'Financial Service', 'Indicator')

p3<-MEX_indicators %>% ggplot(aes(y=Indicator,x=`Community Size`,fill=`Gender`))+
  facet_wrap(~`Financial Service`,scales = 'free')+
  scale_y_continuous(expand = c(0,0),limits = c(0,70))+
  geom_bar(stat='Identity',position=position_dodge())+
  scale_x_discrete(limits=c('<= 14,999', '> 15,000'))+
  geom_text(aes(label=round(Indicator,digits=2)),position=position_dodge(0.9), vjust=-1)+
  labs(subtitle = "The critical gaps for the financial services adoption in the country depend more on the community size than in the gender.",title="In Mexico the gender is not the only gap for financial services adoption.", caption="INEGI: National Survery of Financial Inclusion 2018.",x="Community size by number of inhabitants",y="Percentage of population (between 18 and 70 years) with a financial service account")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.subtitle =element_text(color = "grey20", size = 15, angle = 0, hjust = 0, vjust = .5, face = "plain"),
        plot.title =element_text(color = "grey20", size = 19, angle = 0, hjust = 0, vjust = .5, face = "bold"))


ggsave(here("output","plot3.pdf"), p3,width = 35, height = 25, units = "cm")


# Graph 4
#Regroup the variables:
ENIF18_df1$P5_7_2[ENIF18_df1$P5_7==5|ENIF18_df1$P5_7==6]<-'1'
ENIF18_df1$P5_7_2[ENIF18_df1$P5_7==2]<-'2'
ENIF18_df1$P5_7_2[ENIF18_df1$P5_7==8]<-'3'
ENIF18_df1$P5_7_2[ENIF18_df1$P5_7==4|ENIF18_df1$P5_7==7]<-'4'
ENIF18_df1$P5_7_2[ENIF18_df1$P5_7==3]<-'5'
ENIF18_df1$P5_7_2[ENIF18_df1$P5_7==1|ENIF18_df1$P5_7==9]<-'6'

ENIF18_df1$P6_6_2[ENIF18_df1$P6_6==6|ENIF18_df1$P6_6==7]<-'1'
ENIF18_df1$P6_6_2[ENIF18_df1$P6_6==5]<-'2'
ENIF18_df1$P6_6_2[ENIF18_df1$P6_6==1]<-'4'
ENIF18_df1$P6_6_2[ENIF18_df1$P6_6==4]<-'5'
ENIF18_df1$P6_6_2[ENIF18_df1$P6_6==2|ENIF18_df1$P6_6==3|ENIF18_df1$P6_6==8]<-'6'

ENIF18_df2$P8_4_2[ENIF18_df2$P8_4==7]<-'1'
ENIF18_df2$P8_4_2[ENIF18_df2$P8_4==6]<-'2'
ENIF18_df2$P8_4_2[ENIF18_df2$P8_4==5]<-'3'
ENIF18_df2$P8_4_2[ENIF18_df2$P8_4==4]<-'4'
ENIF18_df2$P8_4_2[ENIF18_df2$P8_4==2]<-'5'
ENIF18_df2$P8_4_2[ENIF18_df2$P8_4==1|ENIF18_df2$P8_4==3|ENIF18_df2$P8_4==8]<-'6'

ENIF18_df2$P9_2_2[ENIF18_df2$P9_2==5]<-'1'
ENIF18_df2$P9_2_2[ENIF18_df2$P9_2==2|ENIF18_df2$P9_2==4]<-'3'
ENIF18_df2$P9_2_2[ENIF18_df2$P9_2==1|ENIF18_df2$P9_2==3]<-'4'
ENIF18_df2$P9_2_2[ENIF18_df2$P9_2==6]<-'5'
ENIF18_df2$P9_2_2[ENIF18_df2$P9_2==7|ENIF18_df2$P9_2==8]<-'6'

#Generate the tables 
MEX_accounts<-ENIF18_df1 %>% 
  filter(is.na(P5_7_2)==FALSE) %>%
  select('P5_7_2', 'FAC_PER') %>%
  group_by(.dots=c('P5_7_2')) %>%
  summarise(`Deposit account`=sum(FAC_PER))

MEX_credits<-ENIF18_df1 %>% 
  filter(is.na(P6_6_2)==FALSE) %>%
  select('P6_6_2', 'FAC_PER') %>%
  group_by(.dots=c('P6_6_2')) %>%
  summarise(`Credit`=sum(FAC_PER))

MEX_insurance<-ENIF18_df2 %>% 
  filter(is.na(P8_4_2)==FALSE) %>%
  select('P8_4_2', 'FAC_PER') %>%
  group_by(.dots=c('P8_4_2')) %>%
  summarise(`Insurance`=sum(FAC_PER))

MEX_retirement<-ENIF18_df2 %>% 
  filter(is.na(P9_2_2)==FALSE) %>%
  select('P9_2_2', 'FAC_PER') %>%
  group_by(.dots=c('P9_2_2')) %>%
  summarise(`Retirement account`=sum(FAC_PER))

names(MEX_accounts)[1]<-'Reasons'
names(MEX_credits)[1]<-'Reasons'
names(MEX_insurance)[1]<-'Reasons'
names(MEX_retirement)[1]<-'Reasons'

MEX_indicators<-MEX_accounts %>% 
  left_join(MEX_credits, by=c('Reasons')) %>%
  left_join(MEX_insurance, by=c('Reasons')) %>%
  left_join(MEX_retirement, by=c('Reasons'))

MEX_indicators$Reasons[MEX_indicators$Reasons=='1']<-'Lack of interest'
MEX_indicators$Reasons[MEX_indicators$Reasons=='2']<-'Unattractive offer (interest rates, prices, etc.)'
MEX_indicators$Reasons[MEX_indicators$Reasons=='3']<-'Ignorance regarding the financial service'
MEX_indicators$Reasons[MEX_indicators$Reasons=='4']<-'Non affordable'
MEX_indicators$Reasons[MEX_indicators$Reasons=='5']<-'Distrust in the financial institutions'
MEX_indicators$Reasons[MEX_indicators$Reasons=='6']<-'Others'

MEX_indicators2<-MEX_indicators

for (i in 2:5){
  MEX_indicators2[,i]<-MEX_indicators2[,i] %>% replace(is.na(MEX_indicators2[,i]),0)
  MEX_indicators2[,i]<-round(100*MEX_indicators2[,i]/sum(MEX_indicators2[,i]),2)
  }

MEX_indicators<-MEX_indicators %>% 
  group_by(Reasons) %>%
  gather(`Deposit account`:`Retirement account`, key='Financial Service', value='Respondents') %>%
  arrange(`Financial Service`, .by_group = TRUE)

MEX_indicators2<-MEX_indicators2 %>% 
  group_by(Reasons) %>%
  gather(`Deposit account`:`Retirement account`, key='Financial Service', value='Percentage') %>%
  arrange(`Financial Service`, .by_group = TRUE)

MEX_indicators<-MEX_indicators %>% 
  left_join(MEX_indicators2, by=c('Reasons','Financial Service'))
  


p4<-MEX_indicators %>% ggplot(aes(area=Respondents, fill=Reasons, subgroup=Reasons))+
  facet_wrap(~`Financial Service`)+
  geom_treemap()+
  geom_treemap_subgroup_border(color='grey100')+
  geom_treemap_text(aes(label=Percentage), color="black")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position="bottom")+
  labs(subtitle = "Main reasons for not acquire a financial service.",title="Affordability and Lack of Interest are the main reasons for not acquiring a financial service.", caption="INEGI: National Survery of Financial Inclusion 2018.",x="Percentage of respondents without the financial service.")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.subtitle =element_text(color = "grey20", size = 15, angle = 0, hjust = 0, vjust = .5, face = "plain"),
        plot.title =element_text(color = "grey20", size = 19, angle = 0, hjust = 0, vjust = .5, face = "bold"))

ggsave(here("output","plot4.pdf"), p4,width = 35, height = 25, units = "cm")

# Graph 5

#Read the captation and credit data from the CNBV:
CNBV_df1<-read_excel(path=here('Data','SH_BM_1118_1.xlsx'),sheet = 'Table_1'
                       ,range=cell_rows(1:217)
                       ,col_names = TRUE
                       ,col_types=c(rep("date",1), rep("numeric", 4)))

CNBV_df1$Year<-format(CNBV_df1$Date,"%Y")

CNBV_Average<-CNBV_df1 %>% 
  select('Year', 'Deposits','Credits','IMOR','USD') %>%
  group_by(.dots=c('Year')) %>%
  summarise_all(funs(mean))

CNBV_Average$`Deposits (USD)`<-CNBV_Average$Deposits/CNBV_Average$USD
CNBV_Average$`Credits (USD)`<-CNBV_Average$Credits/CNBV_Average$USD

CNBV_Average_Balance<-CNBV_Average %>% 
  select('Year', 'Deposits (USD)','Credits (USD)') %>%
  group_by(Year) %>%
  gather(`Deposits (USD)`:`Credits (USD)`, key='Variable', value='Amount')

CNBV_Average_Balance<-CNBV_Average_Balance %>% 
  group_by(Variable) %>%
  dplyr::mutate(last = dplyr::last(Amount))

CNBV_Average_Balance$Variable[CNBV_Average_Balance$Variable=='Credits (USD)']<-'Credits'
CNBV_Average_Balance$Variable[CNBV_Average_Balance$Variable=='Deposits (USD)']<-'Deposits'

CNBV_Average_Indicators<-CNBV_Average %>% 
  select('Year', 'IMOR') %>%
  group_by(Year) %>%
  gather(IMOR, key='Variable', value='Indicator') %>%
  arrange(Variable, .by_group = TRUE)

CNBV_Average_Indicators<-CNBV_Average_Indicators %>% 
  group_by(Variable) %>%
  dplyr::mutate(last = dplyr::last(Indicator))

CNBV_Average_Indicators$Variable[CNBV_Average_Indicators$Variable=='IMOR']<-'Credit Default Rate'

p5<-CNBV_Average_Balance %>% ggplot(aes(x = Year, y = Amount/1000,group=Variable)) + 
  geom_line(aes(color = Variable), alpha = 1, size=2)+
  geom_dl(aes(label = round(last/1000,0)), method=list(dl.trans(y=y+0.2,x = x-0.25), "last.points", cex = 0.8))+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(subtitle = "Assets and Liabilities Balance.", caption="   ",y="Billions of US dollars")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.subtitle =element_text(color = "grey20", size = 13, angle = 0, hjust = 0, vjust = .5, face = "plain"),
        plot.title =element_text(color = "grey20", size = 19, angle = 0, hjust = 0, vjust = .5, face = "bold"))

p6<-CNBV_Average_Indicators %>% ggplot(aes(x = Year, y = Indicator,group=Variable, color=Variable)) + 
  geom_line(alpha = 1, size=1.2)+
  geom_dl(aes(label = round(last,1)), method=list(dl.trans(y=y+0.3, x = x-0.25 ), "last.points", cex = 0.8))+
  theme(legend.position="bottom")+
  labs(subtitle = "Credit risk of the overall portfolio.", caption="CNBV: Balance sheet historical series.",y="Percentage (%)")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.subtitle =element_text(color = "grey20", size = 13, angle = 0, hjust = 0, vjust = .5, face = "plain"),
        plot.title =element_text(color = "grey20", size = 19, angle = 0, hjust = 0, vjust = .5, face = "bold"))

p7<-grid.arrange(p5, p6,ncol = 2, nrow = 1, top=textGrob("The financial inclusion in the country is stuck but the worth of the banking industry is growing.",gp=gpar(fontsize=20,fontface="bold"),x=0,hjust = 0))

  
ggsave(here("output","plot5.pdf"), p7,width = 35, height = 15, units = "cm")
