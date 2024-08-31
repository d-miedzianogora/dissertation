##########################
###### DISSERTATION ###### 
####### ORDINATION #######
########### & ############
####### CLUSTERING #######
##########################

#####SETUP#####
setwd("C:/Users/Usuario/OneDrive - University College London/UCL/Degree/Dissertation")
set.seed(1)

#### Loading packages ####
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(vegan)){install.packages("vegan")}
if(!require(dendextend)){install.packages("dendextend")}
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
if(!require(pheatmap)){install.packages("pheatmap")}
if(!require(readxl)){install.packages("readxl")}
if(!require(gridExtra)){install.packages("gridExtra")}

#### Loading and standardising data ####
nmds_data <- read_excel("kabwe_lithics_data_updated_0824.xlsx", sheet = "NMDS")
nmds_data <- nmds_data %>% 
  as_tibble() %>% 
  replace(is.na(.), 0) %>% 
  mutate(Fragments = rowSums(across(c(Core_fragment, Chunks, Flake_fragment, Blade_frag))),
         Flakes_complete = rowSums(across(c(Flakes_complete, Segment_bipolar, Flakes_edgedam))),
         Points = rowSums(across(c(Point_undiag, Point_unifac, Point_bifac, Point_ret))),
         LCT = rowSums(across(c(LCT_other, LCT_contigret, LCT_biface, LCT_cleaverbifac, LCT_unifac))),
         .before = Flakes_complete,
         .keep = "unused")
nmds_data_stand <- subset(nmds_data[, 1:35]) %>% 
  mutate_if(is.numeric, ~1 * (. > 0))

####CLUSTER ANALYSIS####
#### Label assemblage ####
cl_labels <- nmds_data_stand$Assemblage
#### Distance matrix ####
cl_data_norm <- decostand(nmds_data_stand[, 2:35], "pa")
cluster_data <- vegdist(cl_data_norm, method = "jaccard")

#### Compute clustering ####
best_dend <- dend_expend(cluster_data, dist_methods = "binary")
best_dend$performance # Average linkage scores best, hence it is used

cl_average <- hclust(cluster_data, method = "average")
cl_average$labels <- cl_labels

cl_opt <- silhouette(cutree(cl_average, k = k), cluster_data)
cl_opt

#### Silhoutte plot to find optimal cluster
Si <- numeric(nrow(nmds_data_stand))
for (k in 2:(nrow(nmds_data_stand)-1))
{
  sil <- silhouette(cutree(cl_average, k = k), cluster_data)
  Si[k] <- summary(sil)$avg.width
}
svg("./../../../Illustrations/Dissertation/fig_sil.svg")
plot(1:nrow(nmds_data_stand),
     Si,
     type = "h",
     main = "Silhouette-optimal number of clusters",
     xlab = "K (number of clusters)",
     ylab = "Average silhouette width"
)
dev.off()
Si

#### Fig. 6.5 ####
tiff("./../../../Illustrations/Dissertation/fig_cldendo.tif",
     width = 8, height = 8, units = "in", compression = "none",
     res = 300)
par(mar=c(5,1,1,8)) #custom
cl_average %>% 
  as.dendrogram() %>% 
  set("labels_col", k = 8, brewer.pal(8, "Dark2")) %>% 
  set("branches_k_color", k = 8, brewer.pal(8, "Dark2")) %>% 
  set("branches_lwd", 3) %>% 
  plot(horiz = TRUE, xlab = "Height")
dev.off()
par(mar = c(5, 4, 4, 2) + 0.1) #default

#### Fig. 6.6. ####
pheatmap(cluster_data, border_color = NA, labels_row = cl_average$labels,
         clustering_method = "average", cutree_cols = 8, cutree_rows = 8,
         filename = "./../../../Illustrations/Dissertation/fig_clheatmap.tiff",
         width = 8, height = 6)
dev.off()

##### NMDS #####
#### Setup colouring by clusters in NMDS ####
nmds_data_cl <- cutree(cl_average, k = 8) %>% 
  as.data.frame() %>% 
  rownames_to_column("Assemblage")
colnames(nmds_data_cl)[2] <- "Cluster"

nmds_data_cats <- subset(nmds_data[,c(1, 36:39)]) %>% 
  mutate(Technocomplex = as.factor(Technocomplex),
         Rawmat_dominant = as.factor(Rawmat_dominant),
         Country = as.factor(Country)) %>% 
  right_join(., nmds_data_cl, by = join_by(Assemblage), keep = TRUE) %>% 
  mutate(Cluster = factor(Cluster))

#### Compute NMDS ####
nmds <- metaMDS(nmds_data_stand [2:35], distance = "jaccard", autotransform = FALSE,
                k = 3, try = 1000, trymax = 1000)
nmds$stress

#### Fig. 6.7 ####
tiff("./../../../Illustrations/Dissertation/fig_stressplot.tif",
     width = 4, height = 4, units = "in", compression = "none",
     res = 600)
stressplot(nmds, pch = 19, p.col = "#8dd3c7", l.col = "black", cex = 0.5)
dev.off()

#### Prep NMDS for plot ####
fort <- fortify(nmds)
fort <- fort %>% 
  mutate(label = if_else(label == "Core_chunk",true = "CC",false = label),
         label = if_else(label == "Core_on_flake",true = "CoF",false = label),
         label = if_else(label == "Discoid",true = "Di",false = label),
         label = if_else(label == "Core_bipolar",true = "CB",false = label),
         label = if_else(label == "Core_radial",true = "CR",false = label),
         label = if_else(label == "Core_hirearch_unifac",true = "HCU",false = label),
         label = if_else(label == "Core_hierarch_bifac",true = "HCB",false = label),
         label = if_else(label == "Core_maint_flake",true = "CMF",false = label),
         label = if_else(label == "Backed_piece",true = "Ba",false = label),
         label = if_else(label == "Flakes_complete",true = "Fl",false = label),
         label = if_else(label == "Core_nonhierarch",true = "NHC",false = label),
         label = if_else(label == "Misc_retouch",true = "MR",false = label),
         label = if_else(label == "Notch/denticulate",true = "No",false = label),
         label = if_else(label == "Burin_spall",true = "BuS",false = label),
         label = if_else(label == "Chunk_edgedam",true = "Ch",false = label),
         label = if_else(label == "Misc_coretool",true = "MC",false = label),
         label = if_else(label == "Adze",true = "Ad",false = label),
         label = if_else(label == "Tranchet",true = "Tr",false = label),
         label = if_else(label == "Grindstone",true = "Gr",false = label),
         label = if_else(label == "Borer",true = "Bo",false = label),
         label = if_else(label == "Burin",true = "Bu",false = label),
         label = if_else(label == "Spheroid",true = "Sp",false = label),
         label = if_else(label == "Points",true = "Po",false = label),
         label = if_else(label == "Pick",true = "Pi",false = label),
         label = if_else(label == "Microdeb",true = "Mi",false = label),
         label = if_else(label == "Core-axe",true = "CA",false = label),
         label = if_else(label == "Blade",true = "Bl",false = label),
         label = if_else(label == "Bladelet",true = "Blt",false = label),
         label = if_else(label == "Awl/bec",true = "Aw",false = label),
         label = if_else(label == "Hammerstone",true = "Ha",false = label),
         label = if_else(label == "Anvil",true = "An",false = label),
         label = if_else(label == "Scrapers",true = "Sc",false = label),
         label = if_else(label == "Fragments",true = "Fr",false = label))
  
#### Fig. 6.8
p1 <- ggplot() +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  geom_point(data = subset(fort, score=='sites'),
             mapping = aes(
               x = NMDS1, y = NMDS2, fill = nmds_data_cats$Country),
             shape = 21, size = 1.7, alpha = 0.75)+
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x=NMDS1*1.1, y=NMDS2*1.1),
            alpha = 0)+
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p2 <- ggplot() +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  geom_point(data = subset(fort, score=='sites'),
             mapping = aes(
               x = NMDS1, y = NMDS2, fill = nmds_data_cats$Country),
             shape = 21, size = 1.7, alpha = 0)+
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x=NMDS1, y=NMDS2),
            size = 2.25)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
p3 <- ggplot() +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  geom_point(data = subset(fort, score=='sites'),
             mapping = aes(x = NMDS1, y = NMDS3, 
                           fill = nmds_data_cats$Country),
             shape = 21, size = 1.7, alpha = 0.75) +
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x=NMDS1*1.1, y=NMDS3*1.1),
            alpha = 0)+
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "Country") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "top")
p4 <- ggplot() +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  geom_point(data = subset(fort, score=='sites'),
             mapping = aes(x = NMDS1, y = NMDS3, 
                           fill = nmds_data_cats$Country),
             shape = 21, size = 1.7, alpha = 0) +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x = NMDS1, y = NMDS3),
            size = 2.25)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
p5 <- ggplot() +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  geom_point(data = subset(fort, score=='sites'),
             mapping = aes(
               x = NMDS1, y = NMDS2, fill = nmds_data_cats$Cluster),
             shape = 21, size = 1.7, alpha = 0.75)+
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x=NMDS1*1.1, y=NMDS2*1.1),
            alpha = 0)+
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p6 <- ggplot() +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  geom_point(data = subset(fort, score=='sites'),
             mapping = aes(
               x = NMDS1, y = NMDS3, fill = nmds_data_cats$Cluster),
             shape = 21, size = 1.7, alpha = 0.75)+
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x=NMDS1*1.1, y=NMDS3*1.1),
            alpha = 0)+
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "Cluster") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom")
p_combined <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3,
          labels = c("A", "B", "C", "D", "E", "F"),
        common.legend = TRUE, legend.grob = get_legend(p3 + labs(fill = "Country")))
bottom_legend <- get_only_legend(p6)

tiff("./../../../Illustrations/Dissertation/fig_nmds.tif",
     width = 17, height = 19, units = "cm", compression = "none",
     res = 600)
grid.arrange(p_combined, bottom_legend, nrow = 2, heights = c(10, 1))
dev.off()
