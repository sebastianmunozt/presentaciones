# Instalar y cargar las librerías necesarias
if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")

library(tidyverse)
library(igraph)
library(knitr)
library(openxlsx)

# Función para calcular todas las medidas de una red, manejando redes desconectadas
calcular_medidas_completas <- function(graph) {
  # Comprobar si la red está conectada
  if (is.connected(graph)) {
    # Métricas globales para redes conectadas
    medidas <- list(
      "Número de nodos" = vcount(graph),
      "Número de aristas" = ecount(graph),
      "Densidad" = edge_density(graph),
      "Diámetro" = diameter(graph),
      "Longitud promedio del camino" = mean_distance(graph),
      "Coeficiente de agrupamiento global" = transitivity(graph),
      "Modularidad" = modularity(cluster_louvain(graph)),
      "Centralización (grado)" = centralization.degree(graph)$centralization,
      "Robustez (tamaño mayor componente tras remover hubs)" = {
        graph_removed <- delete_vertices(graph, order(degree(graph), decreasing = TRUE)[1:5])
        max(components(graph_removed)$csize)
      }
    )
  } else {
    # Identificar el componente más grande
    largest_component <- induced_subgraph(graph, which(components(graph)$membership == which.max(components(graph)$csize)))
    
    # Métricas para el componente más grande
    medidas <- list(
      "Número de nodos" = vcount(graph),
      "Número de aristas" = ecount(graph),
      "Densidad" = edge_density(graph),
      "Diámetro (componente más grande)" = diameter(largest_component),
      "Longitud promedio del camino (componente más grande)" = mean_distance(largest_component),
      "Coeficiente de agrupamiento global" = transitivity(graph),
      "Modularidad" = modularity(cluster_louvain(graph)),
      "Centralización (grado)" = centralization.degree(graph)$centralization,
      "Robustez (tamaño mayor componente tras remover hubs)" = {
        graph_removed <- delete_vertices(graph, order(degree(graph), decreasing = TRUE)[1:5])
        max(components(graph_removed)$csize)
      }
    )
  }
  
  # Convertir medidas a data.frame
  return(data.frame(
    Métrica = names(medidas),
    Valor = unlist(medidas)
  ))
}

# Leer datos de músicos urbanos
nodes_urbanos <- read_csv("data/urbanos/filtered_nodes_f.csv")
edges_urbanos <- read_csv("data/urbanos/filtered_edges_f.csv")
graph_urbanos <- graph_from_data_frame(d = edges_urbanos, vertices = nodes_urbanos, directed = FALSE)
medidas_urbanos <- calcular_medidas_completas(graph_urbanos) %>% mutate(Red = "Urbanos")

# Leer datos de músicos no urbanos
nodes_no_urbanos <- read_csv("data/no-urbanos/filtered_nodes_no_urbanos.csv")
edges_no_urbanos <- read_csv("data/no-urbanos/filtered_edges_no_urbanos.csv")
graph_no_urbanos <- graph_from_data_frame(d = edges_no_urbanos, vertices = nodes_no_urbanos, directed = FALSE)
medidas_no_urbanos <- calcular_medidas_completas(graph_no_urbanos) %>% mutate(Red = "No Urbanos")

# Combinar las medidas en una sola tabla
medidas_comparadas <- bind_rows(medidas_urbanos, medidas_no_urbanos)

# Pivotar la tabla para comparación lado a lado
tabla_comparativa <- medidas_comparadas %>%
  pivot_wider(names_from = Red, values_from = Valor)

# Mostrar la tabla en formato bonito
kable(
  tabla_comparativa,
  col.names = c("Métrica", "Urbanos", "No Urbanos"),
  caption = "Comparación de Medidas entre Redes de Músicos Urbanos y No Urbanos"
)

# Imprimir la tabla en consola sin formato adicional
print(tabla_comparativa, row.names = FALSE)

## Descriptivos generales
# Eliminar el caso con spotify_id igual a "5AYSnRNynWULTIDjzAbgxV"
nodes_urbanos <- nodes_urbanos[nodes_urbanos$spotify_id != "5AYSnRNynWULTIDjzAbgxV", ]

table(nodes_urbanos$artist_type)


#
table(nodes_no_urbanos$artist_type)

unknown_cases <- nodes_no_urbanos[!is.na(nodes_no_urbanos$artist_type) & nodes_no_urbanos$artist_type == "unknown", ]


# Lista de spotify_id que deseas eliminar
ids_to_remove <- c("4MjgWorqPJ7J6Dnlm5POD1", "1fL9NVHVSobqodHIO2FO9C", "4vHhQoluojmIL2pm4rbzRu")

# Filtrar los casos que no están en la lista de ids_to_remove
nodes_no_urbanos <- nodes_no_urbanos[!nodes_no_urbanos$spotify_id %in% ids_to_remove, ]



#######

# Eliminar los NA: artist_type

######



# Filtrar los casos con NA en artist_type para no-urbanos
na_no_urbanos <- nodes_no_urbanos %>%
  filter(is.na(artist_type))

# Filtrar los casos con NA en artist_type para urbanos
na_urbanos <- nodes_urbanos %>%
  filter(is.na(artist_type))

# Guardar ambas bases de datos como archivos separados en formato Excel
write.xlsx(
  na_no_urbanos,
  file = "na_no_urbanos.xlsx",
  sheetName = "No_Urbanos_NA",
  row.names = FALSE
)

write.xlsx(
  na_urbanos,
  file = "na_urbanos.xlsx",
  sheetName = "Urbanos_NA",
  row.names = FALSE
)






####


# Poner artist_type faltantes

####




nodes_urbanos <- nodes_urbanos %>%
  mutate(artist_type = ifelse(spotify_id %in% c("7rOlQwf8OuFLFQp4aydjBt", 
                                                "7DXregrznS25AM30UY9sUU", 
                                                "1Mz5cZb7klbNKaLfxx61cU", 
                                                "27pMOKeO9n2qi5SgdfJ9Qh"), 
                              "solo", artist_type))


table(nodes_urbanos$artist_type)




# Actualizar artist_type en nodes_no_urbanos directamente con mutate
nodes_no_urbanos <- nodes_no_urbanos %>%
  mutate(
    artist_type = case_when(
      spotify_id %in% c("22QaKKmBIH3QiotuG30W3o", "1wyjMJTy6WbOvYBSDL9fMR", "1dJfjAj2mnpKrGm8ldBjNG",
                        "4IdI1p8OrVpot6dbdCl3wv", "0LOfrH82HIHcCXk3eFF7og", "6nX2IctRER6z2UkUmJvpE9",
                        "5CE7PcyVcEKMSuQA4IHF7d", "6v6v20hL82eZfIRpGeocjj", "7kwZDveB5qL9uZJcJF1Yds",
                        "2dK7lV2dmrsnMflpwJRPyT", "0E0FX8RYDfwseJAhwO9AK2", "26SwMaLuxWolyBYLyjG8NQ",
                        "1GGnADQvJeAIqtrjWc0CFc", "3ofeDjH3XD66EbnLIHWk8i", "4KyZLSFxk5EZnqkceVEvga",
                        "2L5b3yHGpn7RQi4wd3vcU2", "4KruP5THAhH04L3Gwmzm9w", "3SYksx5zqnl6TH0sh4qCwb",
                        "4833ZYc59o7ZJFIAp6SHhq", "6WUEX7OnXihiMxJM6HZcIR", "5s4SbJgMMtIyl0caPyZMmT",
                        "5il1LfQizqVTfjSzhi1bt7", "7cIyh6k1zccDIbM7Qyn5AY", "6g293XK6eqwkHOsP2m4ru1",
                        "6SBThA2w6qBakDSGVoSXPZ", "1KHw2rfinEItEDlOoP8Hp9", "2HGCzTmS2snalBdRCMNuER",
                        "3BnHs3tD86l2OGH6k6tpGv", "1jK9ucKKnrVI3OHzCPpE8g", "4ZLUNtFI5oNh5xiEew8X2v",
                        "3LbtNWm1EKviVRqC93rY7R", "7JVFObxbojmbv9I1q5PTJ2", "4Mn6wG1rphD2xDDUnGI6rw",
                        "0YeHqvVbslv60mX2q3bUZv", "0QlyCyYljfcdWtBNemiXXV", "1wJii6AWThPVePpQF0P4G6",
                        "5zoXAWbqiovx7MFrNvuteu") ~ "group",
      spotify_id %in% c("4LIR7XQRqn0CyXMYSjKoTX", "65nGmwT7Nj7FITg14ZV8s0", "79YjWaAoD88XGLETIsnnQV",
                        "4wO1Ld1CKjm4vFMNUn7E7k", "7IoWRnQriyj7YYLpvx00jv", "5z9PI0y6MRnL9BCHOI5ilL",
                        "0KJEHAoNtPaopqOHD6UIkY", "1yBSX25jjXRF6KqAKGygy2", "5yrOkBwyi9ePzOYQX4V3iW",
                        "4Wy8CFlIS11ODC9wVKBSz4", "5jyOjqLd2YRArrVlguvv4F", "7nriakmj7Rcq8KhhaLSgja",
                        "2v63mEwpd09o5Oo3ALq0l6", "64qyErWq0ieALSFbxDpScT", "6GyTAMonsoBJtojDYu7qg0",
                        "7FfKue3foqwcKAUi36xsh9", "6clzHoQL1WN2thmMeOalxc", "6GiuMj37ySz4NQxtKOTE19",
                        "5MbC0GPijb60evRCEQLJAz", "58JUcJgPMahuJHcVQM5CNL", "1t9D6kTl718QQilLBXKBoh",
                        "6pQc5n64sfB2qElrQCfE13", "3qWvnBe0WHixQgMLpMsf0T", "0i36EQVtjFoow9nw3UgYCl",
                        "34JOOFEs2qzslbI0YAGBXr", "73jlslw4ZXYZ9kgh0kY2mU", "5Lh7mxqZPi1OqWGY5A6g68",
                        "4W818VxLQukD34t6dFjoRO", "6TohVx7C5O24NqelCsIdfs", "0SQDhZLHzJq716WdfRodG7",
                        "5rJG5I7ZC5C51fXEyV5E6V", "0Dz64lyAnwZJDpF98j0ntV", "4BsdSj5TfV0HQiUwdRBQbk",
                        "5x24CqyDaYbM6OUjxLYkUo", "3b06h05NjiPizwnTeGybfG", "6TMOhNUfIadpsB0YAavKGN",
                        "5t6HRqxLXUFgNXV4598nxz", "3KKA5IFGqdkUw7i657IEL6", "1NBoDdFZArcr9I68YHFA5T",
                        "1xkgbANdCCYVt86KdT7hwP", "6VQxtEeYFtNENvE6Qsvlan", "4uzRQXp8Aavhq3PbPUozNF",
                        "16bFJLaCva0iyEiBRLPSx2", "2G6WYokWYd4regFlrrMx6p", "3gjqD5j7NaOCr4kLULEV0L",
                        "2R0KuBcEcUp6thf2HHLElR", "4IyZZfTjtcbbJfe8gvN1Pf", "2rV1GKAvHpXO40w4lLhplz",
                        "5qW2pOdPhEkkBlsUjcGdhD", "38RtVKKRJwYGkKJDuZtHJP", "0TpzkpozdbNjvorkh1gcdO",
                        "7cC4OFKSHYZuKU6cgbB4vf", "3K8xLmnOqUqGGaLzL0DvL3", "3xzi6cMt4zEXLJk7pQVSLQ",
                        "7kR5ApwlIHd7ket5zyDwTC", "5PS09CXdU7Lze7eAk2z3Yy", "7BzSI3dTllADHzoJ2crQov",
                        "6jhJHMFfgAmkO0VRyRvtro", "1s60AzQYm7kiHJJoqx7nDk", "2IunebW5WpbBHwXU5BBv08",
                        "3EQz0AUS2CMV6lOu3Zplm2", "1sjAiXfRnlaJk1zNhmHSpf", "4sWjBSpXZSBhodRXeVadNL",
                        "3JiStihkFwbEuL4nGuHWlM", "2dl2yCIYJNULyzn3stw3yI", "611NNAaTv1EVgf1066jhzQ",
                        "6RCzTQcz74WFBYsX0xz9Pj", "7LM1I2nIhWByOdM5S5xoyX", "4iTKW9n4HDGCfTud01OaFL") ~ "solo",
      TRUE ~ artist_type  # Mantener valores existentes
    )
  )


# Calcular porcentajes para no-urbanos
no_urbanos_percent <- nodes_no_urbanos %>%
  count(artist_type) %>%
  mutate(percentage = n / sum(n) * 100, category = "no-urbanos")

# Calcular porcentajes para urbanos
urbanos_percent <- nodes_urbanos %>%
  count(artist_type) %>%
  mutate(percentage = n / sum(n) * 100, category = "urbanos")

# Combinar ambos en una tabla
result_table <- bind_rows(no_urbanos_percent, urbanos_percent) %>%
  select(category, artist_type, percentage) %>%
  pivot_wider(names_from = artist_type, values_from = percentage, values_fill = 0)



####

# Estadísticos por nodo

####


