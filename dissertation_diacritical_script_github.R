################################
###### KABWE DISSERTATION ######
##### DIACRITICAL ANALYSIS #####
################################

##### SETUP #####

#### Working directory ####
setwd("C:/Users/Usuario/OneDrive - University College London/UCL/Degree/Dissertation")

#### Packages ####
if(!require(readxl)){install.packages("readxl")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(DiagrammeR)){install.packages("DiagrammeR")}

#### Data ####
e705_nodes <-
  read_excel("reduction_seq_data.xlsx", sheet = 1)
e705_edges <-
  read_excel("reduction_seq_data.xlsx", sheet = 2)
e1108_nodes <-
  read_excel("reduction_seq_data.xlsx", sheet = 3)
e1108_edges <-
  read_excel("reduction_seq_data.xlsx", sheet = 4)
e908_nodes <- 
  read_excel("reduction_seq_data.xlsx", sheet = 5)
e908_edges <-
  read_excel("reduction_seq_data.xlsx", sheet = 6)

#####FIGURES#####
#### Fig. 5.9 ####
e705_matrix <-
  create_graph(graph_name = "E705 Reduction Sequence",
               attr_theme = "tb") %>% 
  add_nodes_from_table(
    table = e705_nodes,
    label_col = label) %>% 
  add_edges_from_table(
    table = e705_edges,
    from_col = from,
    to_col = to,
    from_to_map = label) %>% 
  drop_node_attrs(node_attr = id_external) %>% 
  add_global_graph_attrs(
    attr = c("style", "shape", "color", "color"),
    value = c("solid", "square", "grey45", "grey45"),
    attr_type = c("edge", "node", "node", "edge")) 
p_e705 <- render_graph(e705_matrix)

e1108_matrix <-
  create_graph(graph_name = "E1108 Reduction Sequence",
               attr_theme = "tb") %>% 
  add_nodes_from_table(
    table = e1108_nodes,
    label_col = label) %>% 
  add_edges_from_table(
    table = e1108_edges,
    from_col = from,
    to_col = to,
    from_to_map = label) %>% 
  drop_node_attrs(node_attr = id_external) %>% 
  add_global_graph_attrs(
    attr = c("style", "shape", "color", "color"),
    value = c("solid", "square", "grey45", "grey45"),
    attr_type = c("edge", "node", "node", "edge")) 
p_e1108 <- render_graph(e1108_matrix)

e908_matrix <-
  create_graph(graph_name = "E908 Reduction Sequence",
               attr_theme = "tb") %>% 
  add_nodes_from_table(
    table = e908_nodes,
    label_col = label) %>% 
  add_edges_from_table(
    table = e908_edges,
    from_col = from,
    to_col = to,
    from_to_map = label) %>% 
  drop_node_attrs(node_attr = id_external) %>% 
  add_global_graph_attrs(
    attr = c("style", "shape", "color", "color"),
    value = c("solid", "square", "grey45", "grey45"),
    attr_type = c("edge", "node", "node", "edge")) 
p_e908 <- render_graph(e908_matrix)

p_reduction <- combine_graphs(e705_matrix, e1108_matrix)
p_reduction <- combine_graphs(p_reduction, e908_matrix)
export_graph(p_reduction, "./../../../Illustrations/Dissertation/fig_reduction.svg", 
             file_type = "svg")

#### This figure is manually aligned and cleaned ####