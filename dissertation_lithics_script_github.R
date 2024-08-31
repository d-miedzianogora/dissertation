################################
###### KABWE DISSERTATION ######
####### LITHIC ANALYSIS ########
################################

##### SETUP #####

#### Working directory ####
setwd("C:/Users/Usuario/OneDrive - University College London/UCL/Degree/Dissertation")

if(!require(readxl)){install.packages("readxl")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggpubr)){install.packages("ggpubr")}

#### Graphical parameters ####
theme_set(theme_minimal())
theme_update(panel.grid.major.x = element_blank(), axis.text = element_text(size = 9),
             legend.text = element_text(size = 9), legend.title = element_text(size = 10),
             axis.title.y = element_text(size = 10), legend.key.size = unit(7, "mm"))
options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(12, "Set3"),
        ggplot2.discrete.colour = RColorBrewer::brewer.pal(12, "Set3"))
label_fix <- scale_x_discrete(labels=function(x) sub("-","-\n",x,fixed=TRUE))

#### Base data ####
data <- read_excel("kabwe_lithics_data_updated_0824.xlsx", sheet = "lithics_data")
data <- data %>% 
  mutate(Abrasion = case_match(Abrasion,c("Light", "Moderate", "Rolled") ~ "Abraded",
                               .default = Abrasion),
         `Weathering/staining` = case_match(`Weathering/staining`,
           c("Red staining, tertiary") ~ "Red staining",
           c("White staining, primary", "White staining, secondary",
             "White staining, tertiary") ~ "White staining",
           c("Yellow and red staining, secondary") ~ "Yellow/red staining",
           c("Yellow staining, secondary", "Yellow staining, tertiary") ~ "Yellow staining",
           .default= `Weathering/staining`),
         Breakage = case_match(
           Breakage,
           c("Distal", "Lateral", "Medial", "Notch", "Proximal") ~ "Broken",
           .default = Breakage),
         RawMat = fct_other(RawMat, drop = c("Chert, brown",
                                             "Chert, potlitted",
                                             "Chert, grey"),
                            other_level = "Cherts"),
         RawMat = if_else(RawMat == "Shale, baked",
                          true = "Shale",
                          false = RawMat),
         RawMat = if_else(RawMat == "Arkose/sandstone/quartzite",
                          true = "Arkose",
                          false = RawMat),
         Type = if_else(Type == "Retouched flake",
                        true = "Edge-modified flake",
                        false = Type),
         Cortex = factor(Cortex,
                         levels = c("Primary",
                                    "Secondary",
                                    "Tertiary")),
         RetPlan = if_else(RetPlan == "Nose",
                           true = "Nosed",
                           false = RetPlan),
         EdgeType = if_else(EdgeType == "Bifacial retouch",
                            true = "Bifacial",
                            false = EdgeType),
         PlatType = if_else(PlatType == "Simple",
                            true = "Flat",
                            false = PlatType),
         CoreType = if_else(CoreType == "Hierachical",
                            true = "Hierarchical",
                            false = CoreType),
         AreaArtComp = Length * Width,
         Elon = Length / Width,
         Thin = Thick / Width,
         SDI = ScarC / AreaArtComp * 645.16)

#### Ratio data ####
ratio_data <- read_excel("kabwe_lithics_data_updated_0824.xlsx", sheet = "NMDS")
ratio_data <- ratio_data %>% 
  filter(Country == "Zambia", Assemblage != "Kalambo Falls Site C") %>% 
  as_tibble() %>% 
  replace(is.na(.), 0) %>% 
  mutate(Flakes = rowSums(across(c(Flakes_complete, Segment_bipolar, Flakes_edgedam, Point_undiag, 
                                   Point_unifac, Point_bifac, Core_maint_flake, Flakes_edgedam, 
                                   Blade, Bladelet))),
         Tools = rowSums(across(c(Point_ret, LCT_other, LCT_contigret, LCT_biface, LCT_cleaverbifac,
                                  LCT_unifac, `Core-axe`, Burin_spall, Scrapers, Backed_piece, Adze,
                                  `Notch/denticulate`, Misc_coretool, `Awl/bec`, Borer, Pick, Burin,
                                  Misc_retouch, Tranchet))),
         Cores = rowSums(across(c(Core_chunk, Core_nonhierarch, Core_radial, Core_hierarch_bifac, Discoid,
                                  Core_hirearch_unifac, Core_bipolar, Core_on_flake, Core_fragment))),
         .after = Microdeb, .keep = "unused") 

#### Platform data ####
plat <- data %>% drop_na(PlatW, PlatT, PlatType) %>% 
  mutate(PlatType = if_else(PlatType == "Multifacetted",
                            true = "Multi-\nfacetted",
                            false = PlatType),
         PlatType = factor(PlatType, levels = c("Point", "Crushed", "Flat",
                                                "Cortical", "Dihedral", "Facetted",
                                                "Multi-\nfacetted"
  ))) 

edgemod <- data %>% 
  filter(Type == "Flake" | Type == "Broken flake" | Type == "Edge-modified flake") %>% 
  mutate(Type = replace_na(RetType, "Unmodified flake")) %>% 
  mutate(Type = if_else(Type == "Unmodified flake",
                        true = Type,
                        false = "Modified flake"))

#### Composition data ####
comp_data <- data %>% mutate(Sediments = replace_na(Sediments, "None"),
                             Sediments = factor(Sediments, levels =
                                                  c("Yellow clay",
                                                    "Dark clay",
                                                    "None")))
#### Zambian comparison data ####
z_rawmat <- read_excel("0824_zambia_data.xlsx", sheet = "raw_mat") %>% as_tibble()
z_t_cores <- read_excel("0824_zambia_data.xlsx", sheet = "core_typology") %>% as_tibble()
z_t_plat <- read_excel("0824_zambia_data.xlsx", sheet = "plat_types") %>% as_tibble()
z_scarpat <- read_excel("0824_zambia_data.xlsx", sheet = "scar_pats") %>% as_tibble()
z_s_plat <- read_excel("0824_zambia_data.xlsx", sheet = "plat_stats") %>% as_tibble()

z_rawmat <- z_rawmat %>% 
  rename(RawMat = `Raw material`) %>% 
  mutate(RawMat = if_else(RawMat == "Grey sandstone", true = "Sandstone", false = RawMat),
         RawMat = if_else(RawMat == "Quartz crystal",true = "Rock crystal quartz",false = RawMat),
         RawMat = if_else((RawMat %in% "Quartz") | (RawMat %in% "Granular quartz") |
                            (RawMat %in% "Blue quartz") | (RawMat %in% "Green quartz") |
                            (RawMat %in% "Vein quartz"),true = "Quartz",false = RawMat),
         RawMat = if_else((RawMat %in% "Dolomite") | (RawMat %in% "Galena") |
                            (RawMat %in% "Manganese doixide") | (RawMat %in% "Schist") |
                            (RawMat %in% "Amethyst") | (RawMat %in% "Other") | 
                            (RawMat %in% "Talc") | (RawMat %in% "Chalcedony") |
                            (RawMat %in% "Magnetite") | (RawMat %in% "Diorite") |
                            (RawMat %in% "Crystal") | (RawMat %in% "Dolerite") |
                            (RawMat %in% "Phyllite"),true = "Other",false = RawMat),
         RawMat = if_else(RawMat == "Red sandstone",true = "Sandstone",false = RawMat),
         RawMat = factor(RawMat))

##### FIGURES #####
#### Fig 5.1 ####
p_rawmat_sed <- comp_data %>% 
  select(RawMat, Sediments) %>% 
  group_by(Sediments, RawMat) %>% 
  mutate() %>% 
  ggplot(aes(x = Sediments, fill = fct_infreq(RawMat))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(y = "Count of raw material",
       x = "",
       fill = "Raw material categories") +
  scale_y_continuous(breaks = seq(0, 25, by = 6)) +
  theme(legend.position = "top",
        legend.title.position = "top")
p_type <- comp_data %>% 
  select(Type, Sediments) %>% 
  group_by(Sediments, Type) %>% 
  ggplot(aes(x = Sediments, fill = fct_infreq(Type))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(y = "Count of artefact types",
       x = "",
       fill = "Artefact categories") +
  scale_y_continuous(breaks = seq(0, 12, by = 3)) +
  theme(legend.position = "top",
        legend.title.position = "top")
p_5.1 <- ggarrange(p_rawmat_sed, p_type, labels = "AUTO", ncol = 2, nrow = 1,
                   font.label = list(size = 12))
ggsave("fig_rawmat_artefact_cats.tiff",
       path = "./../../../Illustrations/Dissertation/",
       width = 16, height = 8, dpi = 300, units = "cm", bg = "white")

#### Fig 5.3 ####
p_plat1 <- plat %>% 
  ggplot(aes(y = PlatW, x = PlatType, fill = PlatType)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(size = 1.2, alpha = 0.75) +
  labs(y = "Platform width (mm)",
       x = "Platform types") +
  theme(legend.position = "none")
p_plat2 <- plat %>% 
  filter(PlatType == "Flat" |
           PlatType == "Facetted" |
           PlatType == "Multi-\nfacetted") %>% 
  mutate(Sediments = replace_na(Sediments, "None")) %>% 
  mutate(Sediments = factor(Sediments, levels = c(
    "Dark clay",
    "Yellow clay",
    "None"
  ))) %>%
  mutate(PlatType = factor(PlatType, levels = c(
    "Flat",
    "Facetted",
    "Multi-\nfacetted"))) %>% 
  ggplot(aes(y = PlatW, x = PlatType, fill = Sediments)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(size = 1.2, alpha = 0.75) +
  labs(y = "Platform width (mm)",
       x = "Platform types",
       fill = "Sediments") +
  theme(legend.position = "right")
ggarrange(p_plat1, p_plat2, nrow = 1, ncol = 2,
          common.legend = FALSE, labels = "AUTO")
ggsave("fig_lithics_plat.tiff",
       path = "./../../../Illustrations/Dissertation/",
       width = 22, height = 7.5, dpi = 300, units = "cm",
       bg = "white")

#### Fig 5.4 ####
plat %>% 
  select(PlatType, Thin) %>% 
  mutate(PlatType = factor(PlatType, levels = c(
    "Point",
    "Crushed",
    "Facetted",
    "Multi-\nfacetted",
    "Dihedral",
    "Cortical",
    "Flat"
  ))) %>% 
  ggplot(aes(x = PlatType, y = Thin, fill = PlatType)) +
  geom_boxplot(outliers = FALSE) +
  geom_point() +
  labs(y = "Thinning index",
       x = "Platform type",
       fill = "") +
  theme(legend.position = "none")
ggsave("fig_plat_thin.tiff",
       path = "./../../../Illustrations/Dissertation/",
       width = 22, height = 7.5, dpi = 300, units = "cm",
       bg = "white")

#### Fig. 5.4 ####
edgemod %>% 
  ggplot(aes(x = Type, y = Thin, fill = Type)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(size = 1, alpha = 0.75) +
  labs(y = "Thinning index",
       x = "Flake type",
       fill = "") +
  theme(legend.position = "")
ggsave("fig_thin_diff.tiff",
       path = "./../../../Illustrations/Dissertation/",
       width = 12, height = 12, dpi = 300, units = "cm", bg = "white")

#### Fig. 6.2 ####
p_rawmat_z <- z_rawmat %>% 
  select(-Category, -Unit) %>% 
  group_by(Site, RawMat) %>% 
  summarise(Sums = sum(Count)) %>% 
  arrange(Sums)
p_rawmat_k <- data %>% 
  select(RawMat) %>% 
  mutate(RawMat = if_else((RawMat %in% "Chert, grey") |
                            (RawMat %in% "Chert, potlitted") | (RawMat %in% "Chert, brown"),
                          true = "Chert",false = RawMat),
         RawMat = if_else(RawMat == "Vein quartz",true = "Quartz",false = RawMat)) %>% 
  group_by(RawMat) %>% 
  summarise(Sums = sum(n())) %>% 
  arrange(Sums) %>% 
  add_column(Site = "Kabwe", .before = "RawMat")
p_rawmat <- full_join(p_rawmat_z, p_rawmat_k) %>% 
  group_by(Site) %>% 
  mutate(Sums_total = sum(Sums)) %>% 
  mutate(Perc = round(Sums/Sums_total*100, 1)) %>% 
  arrange(Perc)
p_rawmat_order <- reorder(p_rawmat$RawMat, p_rawmat$Sums, sum)

z_t_cores <- z_t_cores %>% 
  mutate(Type = if_else((Type %in% "2-platform, right") | (Type %in% "Single platform") |
                          (Type %in% "Multiple platforms") | (Type %in% "Core-on-core(?)") |
                          (Type %in% "Opposed platform"),true = "Non-hierarchical core",false = Type),
         Type = if_else(Type == "Split radial",true = "Fragment",false = Type),
         Type = if_else((Type %in% "Blade") | (Type %in% "Bladelet"),true = "Hierarchical unifacial",false = Type),
         Type = if_else(Type == "Bipolar",true = "Bipolar core",false = Type),
         Type = if_else(Type == "Prepared",true = "Hierarchical bifacial",false = Type),
         Type = if_else(Type == "Discoidal",true = "Discoid",false = Type))

p_coretype_z <-z_t_cores %>% 
  select(-Unit) %>% 
  group_by(Site, Type) %>% 
  summarise(Sums = sum(Count)) %>% 
  arrange(Sums)
p_coretype_k <-data %>% 
  select(NMDS_type, CoreType) %>% 
  na.omit() %>% 
  select(NMDS_type) %>% 
  mutate(Type = NMDS_type,
         Type = if_else(Type == "Radial core",true = "Radial",false = Type)) %>% 
  group_by(Type) %>% 
  summarise(Sums = sum(n())) %>% 
  arrange(Sums) %>% 
  add_column(Site = "Kabwe", .before = "Type")
p_coretypes <- full_join(p_coretype_z, p_coretype_k) %>% 
  group_by(Site) %>% 
  mutate(Sums_total = sum(Sums)) %>% 
  mutate(Perc = round(Sums/Sums_total*100, 1)) %>% 
  arrange(Perc) %>% 
  as.data.frame()
p_coretypes_order <- reorder(p_coretypes$Type, p_coretypes$Sums, sum)

p_comp1 <- p_rawmat %>% 
  ggplot(aes(x = Site, y = Perc, fill = p_rawmat_order))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(y = "Raw material %\n(square root transformed)",
       fill = "Raw material\ncategories",
       x = "") +
  theme(legend.position = "right",
        legend.title.position = "top",
        legend.key.size = unit(16, "pt")) +
  scale_y_sqrt()

p_comp2 <-
  p_coretypes %>% 
  ggplot(aes(x = Site, y = Perc, fill = p_coretypes_order))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(y = "Type % of all cores",
       fill = "Core types",
       x = "") +
  theme(legend.position = "right",
        legend.title.position = "top",
        legend.key.size = unit(16, "pt"))

ggarrange(p_comp1, p_comp2, ncol = 1, labels = "AUTO") 
ggsave("fig_comp_rawcore.tiff",
       path = "./../../../Illustrations/Dissertation/",
       width = 16, height = 20, dpi = 300, units = "cm", bg = "white")

#### Fig. 6.3 ####

z_t_plat <- z_t_plat %>% 
  mutate(Type = if_else(Type == "Simple facet",true = "Facetted",false = Type),
         Type = if_else(Type == "Plain",true = "Flat",false = Type),
         Type = if_else(Type == "Shattered",true = "Crushed",false = Type),
         Type = if_else(Type == "Missing",true = NA,false = Type)) %>% 
  na.omit()
z_t_plat <-z_t_plat %>% 
  select(-Unit) %>% 
  group_by(Site, Type) %>% 
  summarise(Sums = sum(Count)) %>% 
  arrange(Sums)
k_t_plat <- data %>% 
  select(PlatType) %>% 
  na.omit() %>% 
  mutate(Type = PlatType,
         Type = if_else(Type == "Dihedral",true = "Facetted",false = Type)) %>% 
  group_by(Type) %>% 
  summarise(Sums = sum(n())) %>% 
  arrange(Sums) %>% 
  add_column(Site = "Kabwe", .before = "Type")
p_t_plat <- full_join(z_t_plat, k_t_plat) %>% 
  group_by(Site) %>% 
  mutate(Sums_total = sum(Sums)) %>% 
  mutate(Perc = round(Sums/Sums_total*100, 1)) %>% 
  arrange(Perc)
p_t_plat_order <- reorder(p_t_plat$Type, p_t_plat$Sums, sum)

z_scarpat <- z_scarpat %>% 
  mutate(Type = Category,
         Type = if_else(Type == "Parallel",true = "Unidirectional",false = Type),
         Type = if_else(Type == "Opposed",true = "Irregular",false = Type),
         Type = if_else(Type == "Opposed butts",true = "Bidirectional",false = Type))
z_scarpat <-z_scarpat %>% 
  select(-Unit, -Category) %>% 
  group_by(Site, Type) %>% 
  summarise(Sums = sum(Count)) %>% 
  arrange(Sums)
k_scarpat <-data %>% 
  select(ScarPat) %>% 
  na.omit() %>% 
  mutate(Type = ScarPat,
         Type = if_else(Type == "Orthogonal",true = "Irregular",false = Type)) %>% 
  group_by(Type) %>% 
  summarise(Sums = sum(n())) %>% 
  arrange(Sums) %>% 
  add_column(Site = "Kabwe", .before = "Type")
p_scarpat <- full_join(z_scarpat, k_scarpat) %>% 
  group_by(Site) %>% 
  mutate(Sums_total = sum(Sums)) %>% 
  mutate(Perc = round(Sums/Sums_total*100, 1)) %>% 
  arrange(Perc)
p_scarpat_order <- reorder(p_scarpat$Type, p_scarpat$Sums, sum)

p_comp3 <- p_t_plat %>% 
  ggplot(aes(x = Site, y = Perc, fill = p_t_plat_order))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(y = "Type % of platforms",
       fill = "Platform\ntypes",
       x = "") +
  theme(legend.position = "right",
        legend.title.position = "top",
        legend.key.size = unit(16, "pt"))
p_comp4 <-p_scarpat %>% 
  ggplot(aes(x = Site, y = Perc, fill = p_scarpat_order))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(y = "Type % different scar patterns\n
       (square root transformed)",
       fill = "Scar pattern",
       x = "") +
  theme(legend.position = "right",
        legend.title.position = "top",
        legend.key.size = unit(16, "pt"))+
  scale_y_sqrt()

ggarrange(p_comp3, p_comp4, ncol = 1, labels = "AUTO")
ggsave("fig_comp_platscar.tiff",
       path = "./../../../Illustrations/Dissertation/",
       width = 16, height = 20, dpi = 300, units = "cm", bg="white")

#### Fig. 6.4 ####
z_plat_w <- z_s_plat %>% 
  filter(Category == "Width", Unit != "VIII-XII") %>% 
  select(-Category) %>% 
  as.data.frame()
k_plat_w <- plat %>% 
  select(PlatW) %>% 
  as.data.frame() %>% 
  summarise(Mean = mean(PlatW),Min = min(PlatW),Max = max(PlatW),`Std dev` = sd(PlatW)) %>% 
  mutate(Site = "Kabwe")
p_plat_w <- full_join(z_plat_w, k_plat_w) %>% 
  mutate(Sd = `Std dev`,Q1 = Mean-0.675*Sd,Q3 = Mean+0.675*Sd)
z_plat_t <- z_s_plat %>% 
  filter(Category == "Thickness", Unit != "VIII-XII") %>% 
  select(-Category) %>% 
  as.data.frame()
k_plat_t <- plat %>% 
  select(PlatT) %>% 
  as.data.frame() %>% 
  summarise(Mean = mean(PlatT),Min = min(PlatT),Max = max(PlatT),`Std dev` = sd(PlatT)) %>% 
  mutate(Site = "Kabwe")
p_plat_t <- full_join(z_plat_t, k_plat_t) %>% 
  mutate(Sd = `Std dev`,Q1 = Mean-0.675*Sd,Q3 = Mean+0.675*Sd)
p_plat_t["Min"][p_plat_t["Min"] == 0.00] <- 0.1

p_platcomp_1 <- p_plat_w %>% 
  ggplot(aes(x = Site)) +
  geom_boxplot(
    aes(ymin = Min, lower = Q1, middle = Mean, upper = Q3, max = Max, , fill = Site),
    stat = "identity") +
  geom_point(aes(y = Mean), shape = 18, size = 6) +
  geom_point(aes(y = Max), size  = 2) +
  geom_point(aes(y = Min), size = 2) +
  scale_y_log10() +
  labs(y = "Width, mm\n(log 10)",
       x = "") +
  theme(legend.position = "none")

p_platcomp_2 <- p_plat_t %>% 
  ggplot(aes(x = Site)) +
  geom_boxplot(
    aes(ymin = Min, lower = Q1, middle = Mean, upper = Q3, max = Max, , fill = Site),
    stat = "identity") +
  geom_point(aes(y = Mean), shape = 18, size = 6) +
  geom_point(aes(y = Max), size = 2) +
  geom_point(aes(y = Min), size = 2) +
  labs(y = "Thickness, mm",
       x = "") +
  theme(legend.position = "none")

ggarrange(p_platcomp_1, p_platcomp_2, labels = "AUTO", ncol = 2)

ggsave("fig_comp_platstats.tiff",
       path = "./../../../Illustrations/Dissertation/",
       width = 16, height = 8, dpi = 300, units = "cm", bg = "white")

##### STATISTICAL TESTS #####
#### Sediment taphonomy ####
taph_data <- data %>% mutate(Sediments = as.vector(Sediments))

chisq_abrasion <- taph_data %>% 
  select(Abrasion, Sediments) %>% 
  na.omit() %>% 
  group_by(Sediments, Abrasion) %>% 
  summarise(AbrasionN = n()) %>% 
  spread(Sediments, AbrasionN) %>% 
  as.data.frame() %>% 
  replace(is.na(.), 0)
chisq.test(fisher_abrasion[-1]) # p = 0.5995

fisher_weathering <- taph_data %>% 
  select(`Weathering/staining`, Sediments) %>% 
  na.omit() %>% 
  group_by(Sediments, `Weathering/staining`) %>% 
  summarise(WeatheringN = n()) %>% 
  spread(Sediments, WeatheringN) %>% 
  as.data.frame() %>% 
  replace(is.na(.), 0)
fisher.test(fisher_weathering[-1]) # p = 0.4486

chisq_completeness <- taph_data %>% 
  select(Breakage, Sediments) %>% 
  na.omit() %>% 
  group_by(Sediments, Breakage) %>% 
  summarise(CompleteN = n()) %>% 
  spread(Sediments, CompleteN) %>% 
  as.data.frame() %>% 
  replace(is.na(.), 0)
chisq.test(chisq_completeness[-1]) # p = 1

#### Platform differences ####
plat_test_data <- plat %>% 
  mutate(PlatType = if_else(PlatType == "Multi-\nfacetted",
                            true = "Multifacetted",
                            false = PlatType))

t_plat_layers <-  plat_test_data %>% 
  select(PlatType, Sediments) %>% 
  filter(PlatType == "Flat" |
           PlatType == "Facetted" |
           PlatType == "Multifacetted") %>% 
  drop_na(PlatType, Sediments) %>% 
  group_by(Sediments, PlatType) %>% 
  summarise(PlatTypeN = n()) %>% 
  spread(Sediments, PlatTypeN) %>% 
  as.data.frame()
fisher.test(t_plat_layers[-1])

t_plat_all <- plat_test_data %>% 
  select(PlatType, Thin) %>% 
  na.omit() %>% 
  kruskal.test(Thin ~ PlatType, data = .)

t_plat_3group <- plat_test_data %>% 
  select(PlatType, Thin) %>% 
  filter(PlatType == c("Facetted", "Multifacetted", "Flat")) %>% 
  na.omit() %>% 
  kruskal.test(Thin ~ PlatType, data = .)

#### Edgemodified differences ####
t_mod_rawmat <- edgemod %>% 
  select(RawMat, Type) %>%  
  group_by(Type, RawMat) %>% 
  summarise(RawMatN = n()) %>% 
  spread(Type, RawMatN) %>% 
  replace(is.na(.), 0) %>% 
  as.data.frame()
fisher.test(t_mod_rawmat[-1])

mod_thin <-
  with(edgemod, Thin[Type == "Modified flake"] - Thin[Type == "Unmodified flake"])
shapiro.test(mod_thin) #p <0.5, hence Wilcox
t_mod_thin <-
  edgemod %>% 
  select(Type, Thin) %>% 
  na.omit() %>% 
  wilcox.test(Thin ~ Type, data = .)
##### TABLES #####
#### Tab 5.1 ####
ratio_data %>% 
  select(Assemblage, Flakes, Cores, Tools) %>% 
  mutate(CorestoFlake = round(Cores/Flakes, 2),
         ToolstoFlakes = round(Tools/Flakes, 2)) %>% 
  select(Assemblage, CorestoFlake, ToolstoFlakes) %>% 
  write.csv(x = ., file = "ratios.csv")

#### Tab 5.2 ####
data %>% 
  filter(Type == "Core" | Type == "Core-on-flake") %>%  
  select(Length, Width, Thick, Area, Elon, Thin) %>% 
  map_df(.f = ~broom::tidy(summary(.x)), .id = "Variable") %>% 
  write.csv(x = ., file = "summary.cores.csv")

#### Tab 5.3 ####
data %>% 
  filter(Type == "Flake" | Type == "Broken flake",
         EdgeType == "Unmodified",
         Breakage == "Complete") %>% 
  select(Length, Width, Thick, Area, Elon, Thin, SDI) %>% 
  map_df(.f = ~broom::tidy(summary(.x)), .id = "Variable") %>% 
  write.csv(x = ., file = "summary.flakes.csv")

#### Tab. 5.4 ####
edgemod %>% 
  filter(EdgeType != "Unmodified", Breakage == "Complete", Type == "Modified flake") %>% 
  select(Length, Width, Thick, Area, Elon, Thin, SDI) %>% 
  map_df(.f = ~broom::tidy(summary(.x)), .id = "Variable") %>% 
  write.csv(x = ., file = "summary.retouched.csv")