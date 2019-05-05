# README

## Into the wild: microbiome transplant studies need broader ecological reality


#### Authors

Christopher J. Greyson-Gaito<sup>*1,:heavy_plus_sign:</sup>, Timothy J. Bartley<sup>1,2,:heavy_plus_sign:</sup>, Karl Cottenie<sup>1,:heavy_plus_sign:</sup>, William Jarvis<sup>3,:heavy_plus_sign:</sup>, Amy E.M. Newman<sup>1,:heavy_plus_sign:</sup>, Mason R. Stothart<sup>4,:heavy_plus_sign:</sup>

#### Affiliations

*Corresponding author - christopher@greyson-gaito.com

1. University of Guelph, Department of Integrative Biology, Guelph, ON, Canada
2. University of Toronto Mississauga, Mississauga, ON, Canada
3. University of Ottawa, Department of Biology, Ottawa, ON,Canada
4. University of Calgary, Calgary, AB, Canada

:heavy_plus_sign: All authors contributed equally

### Folder and file structure
* data
    * EcoRealTable_2019-03-25_Data.csv
* figs
    * 2019-03-07 CountAnimals.pdf	Figure 2 in manuscript
    * 2019-03-07 Eco-realityComparisons.pdf  Figure 3 in manuscript
    * 2019-03-07 Eco-realityAverageStandardOverTime.pdf  Figure 4 in manuscript
    * 2019-04-25 CumulativeSumArticles.pdf  Supporting Information Figure 1
* .gitignore  File containing files or folders that git should ignore
* IntotheWilde_Microbiome_Greyson-Gaito_etal_2019.R  R script for analysis and figure creation
* LICENSE  Mozilla Public License 2.0
* README.md  Important information
* meta_transplant_microbiome.Rproj  R Project to increase ease of use



### Instructions for use
* Download the whole repository (either by forking and cloning or by downloading a ZIP folder)
* In RStudio, open the project called meta_transplant_microbiome.Rproj and open the file Intothewild_Microbiome_Greyson-Gaito_etal_2019.R
	* If not using RStudio, open the file Intothewild_Microbiome_Greyson-Gaito_etal_2019.R and edit the path to the data file called Microbiome_Literature_Summaries.csv to whatever path is required on your computer.
* Run the script in RStudio or however you normally run R scripts.

### Supporting Information

#### Search terms used for methodological literature search

* Google scholar
	* (transplant* AND microbio*) (gut OR fecal OR feces OR gastointestin* OR gasto-intestin* OR faecal OR faeces OR forces OR faecal) -human -patient -"homo sapiens" -man -woman -child*
* Web of Science
	* ((TOPIC:(transplant*) AND TOPIC: (microbio*))AND (TOPIC: ((((((((gut OR fecal) OR feces) OR gastointestin*) OR gasto-intestin*) OR faecal) OR faeces) OR forces) OR faecal) NOT TOPIC:(((((human OR patient) OR homo sapiens) OR man) OR woman) OR child*)))

#### Ordinal data scale (EcoReality score) for each experimental condition

| Experimental Condition  | Ordinal Data Scale                                                                                                                                                                                                                                                            |
|-------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Taxon Match             | 1 = Mismatch <br> 2 = Match                                                                                                                                                                                                                                                        |
| Donor Environment       | 1 = In-Lab in sterile environment <br> 2 = Lab animal host in Non-Sterile Lab <br>  3 = Captive Bred Wildlife (long time difference between capture and transplant) <br>  4 = Wildlife brought into captivity (short time difference between capture and transplant) <br> 5 = Free-Ranging Wildlife |
| Donor Physiology        | 1 = Gene knockout/disease harbouring (non-microbial) <br> 2 = Wildtype non-diseased |
| Transplanted Microbiome | 1 = Single Strain <br> 2 = Consortium, mixture of select strains <br> 3 = Whole community |
| Transplant Method       | 1 = Active (e.g., suppository, oral gavage) <br> 2 = Passive (e.g., mixed into food)                                                                                                                                                                                               |
| Recipient Microbiome    | 1 = Germ-Free <br> 2 = Antibiotic Perturbed/Pathologic <br> 3 = Whole community |
| Recipient Environment   | 1 = Lab animal host in Sterile Lab <br> 2 = Lab animal host in Non-Sterile Lab <br> 3 = Captive Bred Wildlife <br> 4 = Wildlife brought into captivity <br> 5 = Free-Ranging Wildlife |
| Recipient Physiology    | 1 = Gene knockout or disease harbouring (non-microbial) <br> 2 = Wildtype or non-diseased |
| Housing Conditions      | 1 = Housed Singly <br> 2 = Co-Housed |

![SI Figure 1](figs/2019-05-05CumulativeSumArticleslarge.png)

Figure 1 Cumulative sum of articles from our directed review between 2006 and 2018.

