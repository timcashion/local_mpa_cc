
library(tidyverse)

sau <- read_csv("./data/sau_sesync_all.csv")
sau <- sau %>% 
  rename(eez_id=main_area_id,
         fao_id=sub_area_id)
sau <- sau %>% filter(!(eez_id==36 & fao_id != 71)) #Remove Australian fisheries outside of FAO Area 71 as this is the area the GBR is in. 

eezs <- tibble(eez_id = c(585, 36, 959),
               eez_name = c("Palau", "Australia", "Alaska"))

sectors <- tibble(sector_type_id =c(1,2,3,4),
                  sector_type = c("Industrial", "Subsistence", "Artisanal", "Recreational"))

#Combine relevant taxon information to one table:
taxon <- read_csv("./data/taxon.csv") %>% select(taxon_key:common_name, functional_group_id, commercial_group_id)
fgs <- read_csv("./data/sau_fgs.csv") %>% 
  select(functional_group_id, description) %>% 
  rename(fg_name=description)
cgs <- read_csv("./data/sau_cgs.csv") %>% 
  rename(cg_name=name)
taxon1 <-taxon  %>% 
  left_join(fgs) %>% 
  left_join(cgs)
assertthat::are_equal(nrow(taxon), nrow(taxon1))
write_csv(taxon1, "./output/taxon_key_improved.csv")
#Join Catch data to Taxon data:
sau1 <- sau %>% 
  left_join(eezs) %>% 
  left_join(taxon1) %>% 
  left_join(sectors)
assertthat::are_equal(nrow(sau), nrow(sau1))

#Write out file for temp storage:
write_csv(sau1, "./output/sau_sesync_clean.csv")

#Summarize data by eez, sector and taxon key 
summary_df <- sau1 %>% 
  filter(data_layer_id %in% c(1,3)) %>% #Filter out non-domestic non-tuna fisheries
  filter(catch_type_id==1) %>% #Filter out discards
  group_by(eez_name, sector_type, taxon_key, scientific_name, common_name, fg_name, cg_name) %>% 
  summarize(catch = sum(catch_sum), 
            value = sum(real_value)
  ) %>% 
  ungroup() %>% 
  group_by(eez_name, sector_type) %>% 
  mutate(catch_perc = 100 * catch / sum(catch),
         value_perc = 100 * value / sum(value)) %>% 
  arrange(eez_name, desc(catch))

#filter for species only
summary_df_species_only <- summary_df %>% 
  filter(taxon_key >= 600000) %>% 
  filter(catch_perc > 5)



#Summary data by eez, sector and functional/commercial group
summary_df2 <- sau1 %>% 
  filter(data_layer_id %in% c(1,3)) %>% #Filter out non-domestic non-tuna fisheries
  filter(catch_type_id==1) %>% #Filter out discards
  group_by(eez_name, sector_type, fg_name, cg_name) %>% 
  summarize(catch = sum(catch_sum), 
            value = sum(real_value)
  ) %>% 
  ungroup() %>% 
  group_by(eez_name, sector_type) %>% 
  mutate(catch_perc = 100 * catch / sum(catch),
         value_perc = 100 * value / sum(value)) %>% 
  arrange(eez_name, sector_type, desc(catch)) %>% 
  filter(catch_perc >=1) #Filter out small entries

top3_fisheries <- summary_df2 %>% 
  group_by(eez_name, sector_type) %>% 
  top_n(catch_perc, n=3)

write_csv(top3_fisheries, "./output/top3_fisheries.csv")
#Findings:
#Palau:
#Medium reef-associated fish account for >80% of landings for subsistence and artisanal sector
#Large pelagics account for >98% of industrial fisheries

#Australia:
#Medium reef-associated fish account for >80% of landings for subsistence and artisanal sector
#Large pelagics account for >98% of industrial fisheries


#Alaska:
#Medium benthopelagics and medium flatfish account for 66% and 12% (respectively) of landings for industrial fisheries 
#Large benthopelagics (i.e. salmon) and medium pelagics account for 49% and 16% of subsistence catch
#Large and medium demersals account for 33 and 19% (respectively) of artisanal fisheries.

#Therefore, we should grab the taxa associated with those

  

