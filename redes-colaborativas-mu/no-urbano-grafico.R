nodesno <- read_csv("data/no-urbanos/filtered_nodes_no_urbanos.csv") # Abro base completa
edgesno <- read_csv("data/no-urbanos/filtered_edges_no_urbanos.csv") # Abro base completa


# Calcular el grado (número de conexiones) de cada nodo
node_degrees <- edgesno %>%
  pivot_longer(cols = c("id_0", "id_1"), values_to = "node") %>%
  count(node, name = "node_degree")  # Renombrar columna a 'node_degree'

# Verificar que node_degrees contiene las columnas necesarias
print(head(node_degrees))

# Añadir la información de grado a los nodos
nodesno <- nodesno %>%
  left_join(node_degrees, by = c("spotify_id" = "node")) %>%
  mutate(node_degree = ifelse(is.na(node_degree), 1, as.numeric(node_degree)))  # Convertir NA en 1 y asegurar tipo numérico

# Verificar que nodesno contiene la columna node_degree
print(head(nodesno))

# Crear nodos para networkD3
nodes_d3 <- nodesno %>%
  mutate(size = node_degree * 5, group = 1) %>%
  select(id = spotify_id, name, size, group)

# Crear aristas para networkD3
edges_d3 <- edgesno %>%
  mutate(source = match(id_0, nodesno$spotify_id) - 1,
         target = match(id_1, nodesno$spotify_id) - 1)

# Grafo interactivo
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
  opacity = 0.85,
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10)")
)

# Guardar como archivo HTML
saveNetwork(network, "network_graph_no_urbanos.html")