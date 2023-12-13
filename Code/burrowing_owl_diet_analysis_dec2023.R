# Burrowing Owl Diet Analysis
# Madeleine Ostwald
# December 6, 2023


#### Load libraries, organize data ####

setwd("~/Dropbox/UCSB/Manuscripts/Burrowing Owl Diet")
diet_df <- read.csv("Owl Pellet Data Dec2023.csv")
library(ggplot2)
library(dplyr)

diet_df$numberOfIndividuals <- as.numeric(diet_df$numberOfIndividuals)

#### Calculate summary statistics ####

# Calculate the total number of individuals by species
total_individuals <- diet_df %>%
  group_by(species) %>%
  summarize(Total_Individuals = sum(`numberOfIndividuals`))

# Calculate the total number of prey items across all pellets
total_all_pellets <- sum(diet_df$numberOfIndividuals)

# Calculate the percent frequency
total_individuals <- total_individuals %>%
  mutate(Proportion = Total_Individuals / total_all_pellets)

# Count the number of pellets containing at least one item for each species
pellets_with_species <- diet_df %>%
  group_by(species, pelletID) %>%
  filter(sum(numberOfIndividuals) > 0) %>%
  distinct(species, pelletID) %>%
  group_by(species) %>%
  summarize(Pellets_With_Species = n())

# Merge the total_individuals dataframe with the pellets_with_species dataframe
summary_table <- merge(total_individuals, pellets_with_species, by = "species", all.x = TRUE)

# Calculate the percent frequency of occurrence
summary_table <- summary_table %>%
  mutate(Percent_Frequency = (Pellets_With_Species / n_distinct(diet_df$pelletID)) * 100)

# Count the number of pellets containing at least one vertebrate vs. invertebrate
vert_vs_invert <- diet_df %>%
  group_by(vertOrInvert, pelletID) %>%
  filter(sum(numberOfIndividuals) > 0) %>%
  distinct(vertOrInvert, pelletID) %>%
  group_by(vertOrInvert) %>%
  summarize(vert_vs_invert = n())

# Mean and std. error prey individuals across all pellets
mean_individuals <- mean(tapply(diet_df$numberOfIndividuals, diet_df$pelletID, sum))
se_individuals <- sd(tapply(diet_df$numberOfIndividuals, diet_df$pelletID, sum))/sqrt(n_distinct(diet_df$pelletID))

# Add a column for individual biomass
biomass_per_individual <- c(0, 0.098,0.11,0.076,10.6,8.695,8.695,103.8,0.074)
summary_table <- summary_table %>%
  mutate(biomass_per_individual = biomass_per_individual)

# Add a column for estimated total biomass
summary_table <- summary_table %>%
  mutate(total_biomass = biomass_per_individual*Total_Individuals)

# Add a column for vert vs. invert
vert_invert <- c("I","I","I","I","V","V","V","V","I")
summary_table <- summary_table %>%
  mutate(vert_invert = vert_invert)




#### Plot data (Figure 1) ####

plotting_df <- summary_table %>%
  filter(species != "Ammopelmatus sp. Tinkham, 1965")

n_plot <- ggplot(data = plotting_df, aes(x=species, y=Total_Individuals, fill=vert_invert)) + 
  geom_bar(stat="identity") + 
  theme_classic()  +
  scale_x_discrete(limits=c("Forficula auricularia Linnaeus, 1758","Calathus sp. Bonelli, 1810", "Armadillidium sp. Brandt, 1831","Vespula sp. Thomson, 1869","Reithrodontomys megalotis Baird, 1857","Rodentia sp.","Mus musculus Linnaeus, 1758","Thomomys bottae Eydoux and Gervais, 1836")) +
  scale_fill_manual(values=c("#66C2A5","#FC8D62"))+
  labs(x="Prey Item",y="Number of Individuals") +
  theme(axis.text.y=element_text(color="black",size=12),axis.text.x = element_text(face="italic",color="black", angle=45,vjust=1,hjust=1),text=element_text(size=12),legend.position="none")

freq_plot <- ggplot(data = plotting_df, aes(x=species, y=Percent_Frequency, fill=vert_invert)) + 
  geom_bar(stat="identity") + 
  theme_classic()  +
  scale_x_discrete(limits=c("Forficula auricularia Linnaeus, 1758","Calathus sp. Bonelli, 1810", "Armadillidium sp. Brandt, 1831","Vespula sp. Thomson, 1869","Reithrodontomys megalotis Baird, 1857","Rodentia sp.","Mus musculus Linnaeus, 1758","Thomomys bottae Eydoux and Gervais, 1836")) +
  scale_fill_manual(values=c("#66C2A5","#FC8D62"))+
  labs(x="Prey Item",y="Percent Frequency") +
  theme(axis.text.y=element_text(color="black",size=12),axis.text.x = element_text(face="italic",color="black", angle=45,vjust=1,hjust=1),text=element_text(size=12),legend.position="none")

biomass_plot <- ggplot(data = plotting_df, aes(x=species, y=total_biomass, fill=vert_invert)) + 
  geom_bar(stat="identity") + 
  theme_classic()  +
  scale_x_discrete(limits=c("Forficula auricularia Linnaeus, 1758","Calathus sp. Bonelli, 1810", "Armadillidium sp. Brandt, 1831","Vespula sp. Thomson, 1869","Reithrodontomys megalotis Baird, 1857","Rodentia sp.","Mus musculus Linnaeus, 1758","Thomomys bottae Eydoux and Gervais, 1836")) +
  scale_fill_manual(values=c("#66C2A5","#FC8D62"))+
  labs(x="Prey Item",y="Estimated Biomass (g)") +
  theme(axis.text.y=element_text(color="black",size=12),axis.text.x = element_text(face="italic",color="black", angle=45,vjust=1,hjust=1),text=element_text(size=12),legend.position="none")

