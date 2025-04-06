



nodes <- read_csv("data/urbanos/filtered_nodes_f.csv") # Abro base completa
edges <- read_csv("data/urbanos/filtered_edges_f.csv") # Abro base completa


# Calcular el grado (número de conexiones) de cada nodo
node_degrees <- edges %>%
  pivot_longer(cols = c("id_0", "id_1"), values_to = "node") %>%
  count(node) %>%
  rename(spotify_id = node, degree = n)

# Añadir la información de grado a los nodos
nodes <- nodes %>%
  left_join(node_degrees, by = "spotify_id") %>%
  mutate(degree = ifelse(is.na(degree), 1, degree))  # Sustituir NA con 1


# Crear nodos para networkD3
nodes_d3 <- nodes %>%
  mutate(size = degree * 5,  # Escalar el tamaño del nodo según el grado
         group = 1) %>%      # Grupo base (se actualizará para las diferencias de color)
  select(id = spotify_id, name, size, group)

# Crear aristas para networkD3
edges_d3 <- edges %>%
  mutate(source = match(id_0, nodes$spotify_id) - 1,  # Convertir a índices de 0
         target = match(id_1, nodes$spotify_id) - 1)  # Convertir a índices de 0


# Grafo interactivo con tamaño y etiquetas
network <- forceNetwork(
  Links = edges_d3,
  Nodes = nodes_d3,
  Source = "source",
  Target = "target",
  NodeID = "name",
  Group = "group",
  Nodesize = "size",
  fontSize = 14,
  linkDistance = 100,
  zoom = TRUE,
  opacity = 0.85
)


# Guardar como archivo HTML
saveNetwork(network, "network_graph_urbanos.html")
