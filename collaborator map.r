
library(ggplot2)
library(igraph)
library(tidygraph)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)


nodes <- data.frame(
  name = c(
    "Lijin Zhang", "Ben Domingue", "Esther Ulitzsch", "Johnny Zhang",
    "Charles Rahal", "Junhao Pan", "Klint Kanopka", "Thomas Caruso",
    "Edward Ip", "Joshua Gilbert", "Xinya Liang", "Leonie Vogelsmeier",
    "Wen Qu", "Dylan Molenaar"
  ),
  school = c(
    "Stanford University", "Stanford University", "University of Oslo", "University of Notre Dame",
    "University of Oxford", "Sun Yat-sen University", "New York University", "Stanford University",
    "Wake Forest University", "Harvard University", "University of Arkansas",
    "Tilburg University", "Fudan University", "University of Amsterdam"
  ),
  latitude = c(
    37.428230, 37.428230, 59.939900, 41.703200,
    51.754816, 23.097500, 40.729100, 37.428230,
    36.131400, 42.374443, 36.068700,
    51.561300, 31.2304, 52.3556
  ),
  longitude = c(
    -122.168861, -122.168861, 10.721100, -86.238500,
    -1.254367, 113.305400, -73.996500, -122.168861,
    -80.277000, -71.116943, -94.176600,
    5.040800, 121.4737, 4.9550
  )
)

# combine authors from same institute
nodes_no_lijin <- nodes %>%
  filter(name != "Lijin Zhang") %>%
  group_by(latitude, longitude, school) %>%
  summarise(
    name = paste(name, collapse = ", "),  
    .groups = "drop"
  )
nodes <- bind_rows(
  nodes %>% filter(name == "Lijin Zhang"),  
  nodes_no_lijin  
)

# from a to all author
edges <- data.frame(
  from = rep("Lijin Zhang", nrow(nodes) - 1),  
  to = nodes$name[-1]
)



# create igraph object
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# world map
world <- ne_countries(scale = "medium", returnclass = "sf")

node_positions <- nodes %>%
  mutate(school_color = as.factor(school))  

edge_positions <- edges %>%
  left_join(nodes, by = c("from" = "name")) %>%
  rename(lat_from = latitude, lon_from = longitude) %>%
  left_join(nodes, by = c("to" = "name")) %>%
  rename(lat_to = latitude, lon_to = longitude) %>%
  filter(!(lat_from == lat_to & lon_from == lon_to))  


ggplot() +
  # add map
  geom_sf(data = world, fill = "gray90", color = "white") +
  geom_curve(data = edge_positions, aes(x = lon_from, y = lat_from, 
                                        xend = lon_to, yend = lat_to),
             color = "grey", curvature = 0.3, alpha = 0.7,
             arrow = arrow(type = "closed", length = unit(0.05, "inches"))) +  
  
  geom_point(data = node_positions, aes(x = longitude, y = latitude, color = school_color), 
             size = 3, alpha = 0.8) +
  # label lijin
  geom_point(data = filter(node_positions, name == "Lijin Zhang"), aes(x = longitude, y = latitude), 
             size = 4, color = "black", shape = 18) +  
  # label collaborators
  geom_text_repel(data = filter(node_positions, name != "Lijin Zhang"), 
                  aes(x = longitude, y = latitude, label = name, color = school_color),  
                  size = 3, box.padding = 0.5, fontface = "bold",  point.padding = 0.2, max.overlaps = 20) +
  labs(title = "Collaboration Map",
       color = "Institution",
       x = "",
       y = "") +  
  theme_classic() +
  theme(legend.position = "right", 
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 10)) 


ggsave("Collaboration_Map.png", width = 8, height = 4, dpi = 300)



