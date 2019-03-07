##############################
## Microbiome meta-analysis
##
## Mason Stothart, Tim Bartley, Karl Cottenie, Chris Greyson-Gaito, Will Jarvis, Amy Newman
##
## 2018-11-19
##
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

library("tidyverse")
theme_set(theme_simple())
library(viridis)
library(googlesheets)
# + scale_color/fill_viridis(discrete = T/F)
# theme_set(theme_light())

# Startup ends here

## Read in data -----

df_Transplant <- gs_url("https://docs.google.com/spreadsheets/d/1WhRjoIFJWAAGvsFNHbrf2GZp8K6pNtxoudrhZ6h82ZM/edit") %>%
  # create unique identifyer
  gs_read() %>%
  # reads the data into R
  write_csv(paste(Sys.Date(), "Microbiome_Literature_Summaries.csv"))
# and automatically create timestamped .csv for safety and repeatability

## execute line below if you want to repeat analysis from a certain date
# df_Transplant = read_csv("2018-11-20 Microbiome_Literature_Summaries.csv")

attr(df_Transplant, "CreatedOn") <- Sys.Date()
# add the same date as an attribute to the R object
glimpse(df_Transplant)
str(df_Transplant) # check last line for date attribute

## Data wrangling -----

summary(df_Transplant)

## create lookup table more programmatically

labrodents <- c("Mice", "mice", "mouse", "rat", "rats")

donors <- df_Transplant %>%
  dplyr::select(`Donor Taxon`) %>%
  unique() %>%
  arrange(`Donor Taxon`) %>%
  pull()

donors

donors <- tibble(`Donor Taxon` = donors) %>%
  mutate(LabRodent.Donor = ifelse(`Donor Taxon` %in% labrodents, "LabRodent", "Other"))

recipients <- df_Transplant %>%
  dplyr::select(`Recipient Taxon`) %>%
  unique() %>%
  arrange(`Recipient Taxon`) %>%
  pull()

recipients # check which are rodents



recipients <- tibble(`Recipient Taxon` = recipients) %>%
  mutate(LabRodent.Recip = ifelse(`Recipient Taxon` %in% labrodents, "LabRodent", "Other"))

recipients %>% View()
# TODO: after new import, check the above line if new recipient were added
# TODO: if necessary, adjust labrodent vector

## create new column with lookup table and left_join
df_Transplant <- left_join(df_Transplant, recipients, by = "Recipient Taxon") %>%
  left_join(donors, by = "Donor Taxon")

df_Transplant %>%
  dplyr::select(`Donor Taxon`, LabRodent.Donor,`Recipient Taxon`, LabRodent.Recip) %>%
  View()
# check if conversion was done correctly
# easiest to do by sorting the Rodent column in the viewer

# Calculation of number of papers (assuming PdFName are unique for each article)
length(unique(df_Transplant$PdFName)) #55 articles

# Calculation of number of transplant conditions
length(df_Transplant$PdFName) #150 transplant conditions (rows)

# Calculation of average number of transplant conditions per article
summary(df_Transplant %>%
  group_by(PdFName) %>%
  summarise(TransplantCount = length(`Transplant Interaction`)) %>%
 select(TransplantCount)) # mean 2.727

# Calculation of number of match and mismatch transplant conditions and number of lab rodents and other
df_Transplant %>%
  group_by(`Eco-Reality Taxon Match`) %>%
  summarise(MatchCount = length(`Number of Treatment Groups`))

df_Transplant %>%
  group_by(`Eco-Reality Taxon Match`, LabRodent.Donor, LabRodent.Recip) %>%
  summarise(MatchCount = length(`Number of Treatment Groups`))

## Figures --------

#Figure 1 
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
  ylab("Count")

ggsave(paste(Sys.Date(), "CountAnimals.pdf"),
       width = 18, height = 12, units = "cm"
)


## Figure 2
df_Transplant %>%
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
         Type = factor(Type,levels=c("Taxon\nMatch","Donor\nEnvironment","Donor\nPhysiology","Transplanted\nMicrobiome","Transplant\nMethod","Recipient\nMicrobiome","Recipient\nEnvironment","Recipient\nPhysiology","Housing\nMethod")))  %>% 
  # this is needed bc of the NAs and NS from the new articles
  # TODO rerun after all the data are added/verified
  ggplot() +
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
  xlab("Eco-Reality")+ylab("Count")

ggsave(paste(Sys.Date(), "Eco-realityComparisons.pdf"),
  width = 25, height = 12, units = "cm"
)
# save the figure so everyone does not have to run the script


#Figure 3 - Is there improvement of eco-reality over time?
ecorealperpaper<-df_Transplant %>%
  dplyr::select(Year,PdFName, `Transplant Interaction`, starts_with("Eco-Reality")) %>%
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

#Ideas for figure
#could find the transplant condition with highest/lowest ecoreality score - plot line at this value.
#find the paper with the highest lowest ecoreality score average - plot line at this value
#cone showing increasing variation

ggsave(paste(Sys.Date(), "Eco-realityAverageStandardOverTime.pdf"),
       width = 18, height = 14, units = "cm"
)


