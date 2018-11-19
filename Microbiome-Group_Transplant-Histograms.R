##############################
## Microbiome meta-analysis
##
## Mason Stothart, Tim Bartley, Karl Cottenie, Chris Greyson-Gaito, Will Jarvis, Amy Newman
##
## 2018-11-19
##
##############################

library(tidyverse)
library(viridis)
library(googlesheets)
# + scale_color/fill_viridis(discrete = T/F)
theme_set(theme_light())

# Startup ends here

## Read in data -----

df_Transplant = gs_url("https://docs.google.com/spreadsheets/d/1WhRjoIFJWAAGvsFNHbrf2GZp8K6pNtxoudrhZ6h82ZM/edit") %>% 
  # create unique identifyer
  gs_read()  %>%     
  # reads the data into R
  write_csv(paste(Sys.Date(),"Microbiome_Literature_Summaries.csv")) 
# and automatically create timestamped .csv for safety and repeatability

attr(df_Transplant, "CreatedOn") = Sys.Date() 
# add the same date as an attribute to the R object
glimpse(df_Transplant)
str(df_Transplant) # check last line for date attribute
  
## Data wrangling -----

summary(df_Transplant)

## create new rodent/other column

df_Transplant$Rodent.Recip <- df_Transplant$`Recipient Taxon` #Create a new column identical to recipient taxon column
df_Transplant$Rodent.Recip[df_Transplant$Rodent.Recip == 'mouse'] <- 'Rodent' #Replace various lab rodent models with 'Rodent'
df_Transplant$Rodent.Recip[df_Transplant$Rodent.Recip == 'Mice'] <- 'Rodent'
df_Transplant$Rodent.Recip[df_Transplant$Rodent.Recip == 'rat'] <- 'Rodent'
df_Transplant$Rodent.Recip[df_Transplant$Rodent.Recip == 'rats'] <- 'Rodent'
df_Transplant$Rodent.Recip[df_Transplant$Rodent.Recip != 'Rodent'] <- 'Other' #Replace all non-lab-rodent taxons to 'other'

df_Transplant$Rodent.Donor <- df_Transplant$`Donor Taxon` #Create a new column identical to donor taxon column
df_Transplant$Rodent.Donor[df_Transplant$Rodent.Donor == 'mouse'] <- 'Rodent' #Replace various lab rodent models with 'Rodent'
df_Transplant$Rodent.Donor[df_Transplant$Rodent.Donor == 'rat'] <- 'Rodent'
df_Transplant$Rodent.Donor[df_Transplant$Rodent.Donor != 'Rodent'] <- 'Other' #Replace all non-lab-rodent taxons to 'other'
#TODO: use tidyverse to create these two vectors

## Summary figures --------

df_Transplant %>% 
  dplyr::select(Rodent.Recip, starts_with("Eco-Reality")) %>% 
  # select the relevant columns for the plotting function
  gather(starts_with("Eco-reality of"), key = "Type", value = "Eco-Reality") %>% 
  # create the long format for ease of plotting
  # not necessary to create individual figures (see code below)
  mutate(`Eco-Reality` = as.numeric(`Eco-Reality`)) %>% 
  # this is probably only needed bc of the NAs from the new articles
  #TODO rerun after all the data are added/verified
  ggplot() +
  geom_histogram(aes(x = `Eco-Reality`, 
                     fill = Rodent.Recip), 
                 binwidth = 1) + 
  facet_grid (`Eco-Reality Taxon Match` ~ Type, scales = "free_x") +
  #TODO: create shorter lables for the facet columns
  theme(legend.position = "top")
  

## individual figures -------

# leave it here bc might need it in the future
# for simplicity's sake, just collapse the code in RStudio

## Histogram Donor Microbiome
df_Transplant %>% 
 ggplot () +
  geom_histogram (aes(x = `Eco-Reality of Donor Microbiome (1-3)`, 
                     fill = Rodent.Recip), 
                 binwidth = 1) + 
   facet_wrap (~ `Eco-Reality of Taxon Match`) 
#==> NA panel is caused by the 6 new articles
# TODO: rerun after all the data are added/verified
 
#Histogram Recipient Microbiome
df_Transplant %>% 
  ggplot () +
  geom_histogram (aes(x = `Eco-Reality of Recipient Microbiome (1-5)`, 
                      fill = Rodent.Recip), 
                  binwidth = 1) + 
  facet_wrap (~ `Eco-Reality of Taxon Match`) 

#Histogram Donor Environment
df_Transplant %>% 
  ggplot () +
  geom_histogram (aes(x = `Eco-Reality of Recipient Microbiome (1-5)`, 
                      fill = Rodent.Recip), 
                  binwidth = 1) + 
  facet_wrap (~ `Eco-Reality of Taxon Match`)

 ggplot () +
   geom_histogram (data = df_Transplant, 
                   aes(x = Eco.Reality.Donor.Environment..1.5., 
                       fill = Rodent.Recip), 
                   binwidth = 1) + 
   facet_wrap (~ Eco.Reality.of.Taxon.Match) 
 
 #Histogram Recipient Environment
 ggplot () +
   geom_histogram (data = df_Transplant, 
                   aes(x = Eco.Reality.of.Recipient.Environment..1.5., 
                       fill = Rodent.Recip), 
                   binwidth = 1) + 
   facet_wrap (~ Eco.Reality.of.Taxon.Match) 
 
 #Histogram Donor Physiology
 ggplot () +
   geom_histogram (data = df_Transplant, 
                   aes(x = Eco.Reality.Donor.Physiology..1.2., 
                       fill = Rodent.Recip), 
                   binwidth = 1) + 
   facet_wrap(~Eco.Reality.of.Taxon.Match)
 
 #Histogram Recipient Physiology
 ggplot () +
   geom_histogram (data = df_Transplant, 
                   aes(x = Eco.Reality.of.Recipient.Physiology..1.2., 
                  fill = Rodent.Recip), 
                  binwidth = 1) + 
   facet_wrap (~ Eco.Reality.of.Taxon.Match)
 
 #Histogram Transplant Method
 ggplot () +
   geom_histogram (data = df_Transplant, 
                   aes(x = Eco.Reality.of.Transplant.Method..1.2., 
                       fill = Rodent.Recip), 
                   binwidth = 1) + 
   facet_wrap (~ Eco.Reality.of.Taxon.Match)
 
 #Histogram Housing Method
  ggplot () +
   geom_histogram (data = df_Transplant, 
                   aes(x = Eco.Reality.of.Housing.Method..1.2., 
                   fill = Rodent.Recip), 
                   binwidth = 1) + 
   facet_wrap (~ Eco.Reality.of.Taxon.Match) 
 
