##############################
##Into the wild: understanding the breadth of host-microbiome interactions
##
## Christopher J. Greyson-Gaito, Timothy J. Bartley, Karl Cottenie, Will M.C. Jarvis, Amy E.M. Newman, Mason Stothart
##
## R script to create figures in manuscript
## 
## Beginning of coding - 2018-11-19
## Version 1.0 - 
##
## As of 2019-03-11, works with R version 3.5.2 (see packages for their versions)
##############################

theme_simple <- function() {
  theme_grey() %+replace%
    theme(
      axis.line = element_line(colour = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 28, face = "bold"),
      axis.text.x = element_text(size = 15, colour = "Black"),
      axis.text.y = element_text(size = 15, colour = "Black"),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks = element_line(size = 0.5, colour = "Black"),
      panel.border = element_rect(fill = FALSE, size = 0.5),
      legend.title = element_text(size = 15),
      legend.key = element_blank()
    )
}

library("tidyverse") #Version 1.2.1
theme_set(theme_simple())
library(viridis) #Version 0.5.1

## Read in data -----

df_Transplant <- read_csv("EcoRealTable-2019-03-25 - Data.csv")

## Data wrangling -----

summary(df_Transplant)

# find all the unique taxon IDs
df_Transplant %>% select(`Donor Taxon`, `Recipient Taxon`) %>% 
  unlist() %>% 
  unique() %>% .[order(.)]
  
## create lookup table more programmatically

labrodents <- c( "mouse", "rat")

donors <- df_Transplant %>%
  dplyr::select(`Donor Taxon`) %>%
  unique() %>%
  arrange(`Donor Taxon`) %>%
  mutate(LabRodent.Donor = ifelse(`Donor Taxon` %in% labrodents, "LabRodent", "Other"))

recipients <- df_Transplant %>%
  dplyr::select(`Recipient Taxon`) %>%
  unique() %>%
  arrange(`Recipient Taxon`) %>%
  mutate(LabRodent.Recip = ifelse(`Recipient Taxon` %in% labrodents, "LabRodent", "Other"))

## create new column for whether donor or recipient host was a labrodent or other
df_Transplant <- left_join(df_Transplant, recipients, by = "Recipient Taxon") %>%
  left_join(donors, by = "Donor Taxon")

# Calculation of number of papers (assuming PdFName are unique for each article)
length(unique(df_Transplant$PdFName)) #54 articles

# Calculation of number of transplant instances
length(df_Transplant$PdFName) #152 transplant instances (rows)

# Calculation of average number of transplant instances per article
summary(df_Transplant %>%
  group_by(PdFName) %>%
  summarise(TransplantCount = length(`Transplant Instance`)) %>%
 select(TransplantCount)) # mean 2.815

# Calculation of cumulative number of publications
df_Transplant %>%
  select(PdFName,Year) %>%
  group_by(Year) %>%
  summarise(Count = length(unique(PdFName))) %>%
  ggplot() +
  geom_line(aes(x = Year, y = cumsum(Count))) +
  ylab("cumulative sum of articles")

## Figures --------

#Figure 1 

# necessary data frame to add letters to the plot
ann_fig1 = data.frame(DonorRecip = c("Donor","Recipient"),
                      label = c("(A)", "(B)"))

df_Transplant %>%
  select(PdFName,Year,LabRodent.Donor,LabRodent.Recip) %>% 
  mutate(LabRodent.DonorBin = ifelse(LabRodent.Donor == "LabRodent",1,0),LabRodent.RecipBin = ifelse(LabRodent.Recip == "LabRodent",1,0)) %>%
  group_by(Year) %>%
  summarise(DonorLabRodent = sum(LabRodent.DonorBin==1),DonorOther = sum(LabRodent.DonorBin==0),RecipLabRodent = sum(LabRodent.RecipBin==1), RecipOther = sum(LabRodent.RecipBin==0)) %>%
  select(Year,DonorLabRodent,DonorOther,RecipLabRodent,RecipOther) %>%
  gather(Type, Total, -Year) %>%
  mutate(DonorRecip = ifelse(grepl("Donor", Type),"Donor", "Recipient")) %>%
  mutate(Type = str_remove(Type, "Donor")) %>%
  mutate(Type = str_remove(Type, "Recip")) %>%
  ggplot()+
  geom_col(aes(x = Year, y = Total, fill = Type )) +
  facet_grid(.~DonorRecip)+
  theme(strip.background=element_blank(),strip.text.x=element_text(size=10),strip.text.y=element_text(size=10),
        axis.title.y=element_text(hjust=0.5, vjust=1.5),legend.text=element_text(size=15)) +
  scale_fill_viridis(
    name = "Taxon Type", breaks = c("LabRodent", "Other"),
    labels = c("lab rodent", "other"), alpha = 1, begin = 0, end = 1,
    direction = 1, discrete = TRUE, option = "D"
  ) +
  ylab("Count") +
  geom_text(data = ann_fig1, 
            mapping = aes(x = 2007, y = 43, label = label))
            # hjust = -0.1,
            # vjust = -1)

ggsave(paste(Sys.Date(), "CountAnimals.pdf"),
       width = 18, height = 12, units = "cm"
)


## Figure 2

ann_fig2 = data.frame(Type = c("Taxon\nMatch","Donor\nEnvironment",
                               "Donor\nPhysiology","Transplanted\nMicrobiome",
                               "Transplant\nMethod","Recipient\nMicrobiome",
                               "Recipient\nEnvironment","Recipient\nPhysiology",
                               "Housing\nMethod"),
                      label = c("(A)", "(B)", "(C)", "(D)", "(E)",
                                "(F)", "(G)", "(H)", "(I)"))


ecoreality_conditions <- df_Transplant %>%
  dplyr::select(LabRodent.Recip, starts_with("Eco-Reality")) %>%
  # select the relevant columns for the plotting function
  mutate(`Eco-Reality of Taxon Match` = ifelse(`Eco-Reality Taxon Match`=="Match",2,1)) %>%
  select(-`Eco-Reality Taxon Match`) %>%
  gather(starts_with("Eco-Reality of"), key = "Type", value = "Eco-Reality") %>%
  mutate(Type = str_remove(Type, "Eco-Reality of ")) %>%
  mutate(Type = str_remove(Type, " \\(1-3\\)")) %>%
  mutate(Type = str_remove(Type, " \\(1-5\\)")) %>%
  mutate(Type = str_remove(Type, " \\(1-2\\)")) %>%
  mutate(Type = str_replace(Type, " ", "\n")) %>%
  # create the long format for ease of plotting
  # not necessary to create individual figures (see code below)
  mutate(`Eco-Reality` = as.numeric(`Eco-Reality`),
         Type = factor(Type,levels=c("Taxon\nMatch","Donor\nEnvironment","Donor\nPhysiology","Transplanted\nMicrobiome","Transplant\nMethod","Recipient\nMicrobiome","Recipient\nEnvironment","Recipient\nPhysiology","Housing\nMethod")))

AV_ecoreality_conditions <- ecoreality_conditions %>%
  group_by(Type)%>%
  summarise(AvEcoReality=mean(`Eco-Reality`, na.rm=TRUE))%>%
  mutate(EcoRealityMax=c(2,5,2,3,2,3,5,2,2), propEcoReality=AvEcoReality/EcoRealityMax)
  
AV_ecoreality_conditions_recipienttaxon <- ecoreality_conditions %>%
  group_by(LabRodent.Recip,Type)%>%
  summarise(AvEcoReality=mean(`Eco-Reality`, na.rm=TRUE))%>%
  mutate(EcoRealityMax=c(2,5,2,3,2,3,5,2,2), propEcoReality=AvEcoReality/EcoRealityMax)

  ggplot(ecoreality_conditions) +
  geom_bar(aes(x = `Eco-Reality`, 
                     fill = LabRodent.Recip)) + 
  facet_grid (.~ Type, scales = "free_x",space = "free_x") +
  theme(legend.position = "top",
        strip.background=element_blank(),strip.text.x=element_text(size=10),strip.text.y=element_text(size=10),
        axis.title.y=element_text(hjust=0.5, vjust=1.5),legend.text=element_text(size=15)) +
  scale_fill_viridis(
    name = "Recipient Taxon Type", breaks = c("LabRodent", "Other"),
    labels = c("lab rodent", "other"), alpha = 1, begin = 0, end = 1,
    direction = 1, discrete = TRUE, option = "D"
  ) +
  scale_x_continuous(breaks = function(x) pretty(x)[pretty(x) %% 1 == 0]) +
  # https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
  xlab("Eco-Reality") + 
  ylab("Count") +
  geom_text(data = ann_fig2, 
            mapping = aes(x = 1, y = 120, label = label))

ggsave(paste(Sys.Date(), "Eco-realityComparisons.pdf"),
  width = 25, height = 12, units = "cm"
)


#Figure 3
ecorealperpaper<-df_Transplant %>%
  dplyr::select(Year,PdFName, `Transplant Instance`, starts_with("Eco-Reality")) %>%
  mutate(`Eco-Reality Taxon Match` = ifelse(`Eco-Reality Taxon Match` == "Match",2,1))%>%
  # select the relevant columns for the plotting function
  gather(starts_with("Eco-Reality"), key = "Type", value = "EcoRealityAbs") %>%
  mutate(Type = str_remove(Type, "Eco-Reality ")) %>%
  mutate(Type = str_remove(Type, "of ")) %>%
  mutate(Type = str_remove(Type, " \\(1-3\\)")) %>%
  mutate(Type = str_remove(Type, " \\(1-5\\)")) %>%
  mutate(Type = str_remove(Type, " \\(1-2\\)")) %>%
  # create the long format for ease of plotting
  # not necessary to create individual figures (see code below)
  mutate(EcoRealityAbs=as.numeric(EcoRealityAbs))%>%
  mutate(EcoReality = case_when(
    Type=="Taxon Match" ~ EcoRealityAbs/2,
    Type=="Donor Environment" ~ EcoRealityAbs/5,
    Type=="Donor Physiology" ~ EcoRealityAbs/2,
    Type=="Housing Method" ~ EcoRealityAbs/2,
    Type=="Recipient Environment" ~ EcoRealityAbs/5,
    Type=="Recipient Microbiome" ~ EcoRealityAbs/3,
    Type=="Recipient Physiology" ~ EcoRealityAbs/2,
    Type=="Transplant Method" ~ EcoRealityAbs/2,
    Type=="Transplanted Microbiome" ~ EcoRealityAbs/3)) %>%
  select(-EcoRealityAbs)%>%
  spread(Type, EcoReality) %>%
  mutate(EcoRealSum = rowSums(.[4:11])) %>%
  group_by(Year,PdFName) %>%
  summarise(AvER = mean(EcoRealSum, na.rm=TRUE)) %>%
  arrange(AvER)


  ggplot(ecorealperpaper)+
    geom_hline(yintercept=3.933333)+
    geom_hline(yintercept=7.1)+
  geom_point(aes(Year, AvER))+
  geom_smooth(aes(Year, AvER), method=lm, se=TRUE)+
  geom_hline(yintercept=3.566667)+
  geom_hline(yintercept=9)+
  scale_y_continuous(limits=c(3.5,9))+ #3.5666667 is the lowest score a paper can have, 9 is the highest score a paper can have
  ylab("Average Standardized Eco-Reality")

ggsave(paste(Sys.Date(), "Eco-realityAverageStandardOverTime.pdf"),
       width = 18, height = 14, units = "cm"
)

##### Useful extra information

# How many of the recipient microbiomes 1 are gnobiotic bees.
bees <- df_Transplant %>%
  dplyr::select(`Recipient Taxon`, LabRodent.Recip, starts_with("Eco-Reality")) %>%
  # select the relevant columns for the plotting function
  mutate(`Eco-Reality of Taxon Match` = ifelse(`Eco-Reality Taxon Match`=="Match",2,1)) %>%
  select(-`Eco-Reality Taxon Match`) %>%
  gather(starts_with("Eco-Reality of"), key = "Type", value = "Eco-Reality") %>%
  mutate(Type = str_remove(Type, "Eco-Reality of ")) %>%
  mutate(Type = str_remove(Type, " \\(1-3\\)")) %>%
  mutate(Type = str_remove(Type, " \\(1-5\\)")) %>%
  mutate(Type = str_remove(Type, " \\(1-2\\)")) %>%
  mutate(Type = str_replace(Type, " ", "\n")) %>%
  # create the long format for ease of plotting
  # not necessary to create individual figures (see code below)
  mutate(`Eco-Reality` = as.numeric(`Eco-Reality`),
         Type = factor(Type,levels=c("Taxon\nMatch","Donor\nEnvironment","Donor\nPhysiology","Transplanted\nMicrobiome","Transplant\nMethod","Recipient\nMicrobiome","Recipient\nEnvironment","Recipient\nPhysiology","Housing\nMethod"))) %>%
  filter(Type=="Recipient\nMicrobiome") %>%
  filter(`Eco-Reality`==1)
  
table(bees$`Recipient Taxon`)

nrow(bees)
# Find articles for bees in recipient microbiom 1
beearticle <- df_Transplant %>%
  dplyr::select(PdFName,`Recipient Taxon`, `Eco-Reality of Recipient Microbiome (1-3)`)%>%
  filter(`Eco-Reality of Recipient Microbiome (1-3)`==1) %>%
  filter(`Recipient Taxon` %in% c("bee", "bumblebee"))

# Are zebrafish in Recipient microbiome 1 gnotobiotic too - find articles
zebrafish <- df_Transplant %>%
  dplyr::select(PdFName,`Recipient Taxon`, `Eco-Reality of Recipient Microbiome (1-3)`)%>%
  filter(`Eco-Reality of Recipient Microbiome (1-3)`==1) %>%
  filter(`Recipient Taxon`=="zebrafish")



