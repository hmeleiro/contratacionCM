## Scraper de contratos menores para 2017


setwd("")  #Poner ruta al directorio de trabajo

require(stringr)
require(httr)
require(rvest)
require(readr)

desktop_agents <-  c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0')


### Seleccionar número de paginas y rango de fechas
paginas <- 1:61286
#paginas <- 1:5

# Creo el índice de los links para cada pagina dentro del rango de fechas
menores2017 <- paste0("http://www.madrid.org/cs/Satellite?c=Page&cid=1224915242285&codigo=PCON_&fechaFormalizacionDesde=01%2F01%2F2017&fechaFormalizacionHasta=31%2F12%2F2017&idPagina=1224915242285&language=es&newPagina=", paginas,"&numPagListado=5&pagename=PortalContratacion%2FComunes%2FPresentacion%2FPCON_resultadoBuscadorAvanzado&paginaActual=1&paginasTotal=61286&procedimientoAdjudicacion=Contratos+Menores&rootelement=PortalContratacion%2FComunes%2FPresentacion%2FPCON_resultadoBuscadorAvanzado&site=PortalContratacion&tipoPublicacion=Contratos+Menores")

contratos_tot.2017 <- NA

contratos <- data.frame("links")
write_csv(contratos, path = "data/links2017.csv", append = FALSE, col_names = FALSE)

p <- length(menores2017)

for (menor in menores2017) {
  x <- GET(menor, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  contratos <- x %>% read_html() %>% html_nodes(".cajaBlanca a") %>% html_attr("href")
  contratos <- paste0("http://www.madrid.org/", contratos)

  contratos <- data.frame(contratos)
  write_csv(contratos, path = "data/links2017.csv", append = TRUE, col_names = FALSE)
  print(p)
  p <- p - 1
}

df <- read_csv("data/links2017.csv")

contratos_tot.2017 <-  unique(df$links)

## scrapeamos los contratos menores de 2017

line <- data.frame("tipo_publicacion", "objeto", "n_expediente", "referencia", "tipo_contrato", "adjudicadora", "procedimiento", "fecha_publicacion", "duracion", "adjudicatario", "NIF", "conIVA", "sinIVA", "url")
write_tsv(line, path = "data/prueba.tsv", append = FALSE, col_names = FALSE)




contratos_tot.2017 <- sin
for (url in contratos_tot.2017) {
  x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  listado <- x %>% read_html() %>% html_nodes(".listado li") %>% html_text()
  

  tipo_publicacion <- listado[str_detect(listado, "Tipo Publicación") == TRUE]
  tipo_publicacion <- str_remove(tipo_publicacion, "Tipo Publicación")

  objeto <- listado[str_detect(listado, "Objeto del contrato") == TRUE]
  objeto <- str_remove_all(objeto, 'Objeto del contrato|\"')

  n_expediente <- listado[str_detect(listado, "Número de expediente") == TRUE]
  n_expediente <- str_remove(n_expediente, "Número de expediente")

  referencia <- listado[str_detect(listado, "Referencia") == TRUE]
  referencia <- str_remove(referencia, "Referencia")

  tipo_contrato <- listado[str_detect(listado, "Tipo de contrato") == TRUE]
  tipo_contrato <- str_remove(tipo_contrato, "Tipo de contrato")

  adjudicadora <- listado[str_detect(listado, "Entidad adjudicadora") == TRUE]
  adjudicadora <- str_remove_all(adjudicadora, 'Entidad adjudicadora|\n|\"')
  adjudicadora <- str_replace_all(adjudicadora, "→ |→", "-")

  procedimiento <- listado[str_detect(listado, "Procedimiento Adjudicación") == TRUE]
  procedimiento <- str_remove_all(procedimiento, "Procedimiento Adjudicación|\n")

  fecha_publicacion <- listado[str_detect(listado, "Fecha del contrato") == TRUE]
  fecha_publicacion <- str_remove_all(fecha_publicacion, "Fecha del contrato")


  duracion <- listado[str_detect(listado, "Duración") == TRUE]
  duracion <- str_remove_all(duracion, "Duración del contrato")

  estado <- listado[str_detect(listado, "Estado") == TRUE]
  estado <- str_remove_all(estado, "Estado de la licitación")

  tabla <- x %>% read_html() %>% html_nodes("td") %>% html_text()

  NIF <- tabla[1]
  adjudicatario <- tabla[2]
  sinIVA <- tabla[3]
  conIVA <- tabla[4]

  sinIVA <- str_remove_all(sinIVA, "\\.")
  sinIVA <- str_replace(sinIVA, ",", ".")
  sinIVA <- as.numeric(sinIVA)

  conIVA <- str_remove_all(conIVA, "\\.")
  conIVA <- str_replace(conIVA, ",", ".")
  conIVA <- as.numeric(conIVA)

  if (length(tipo_publicacion) == 0) {
    tipo_publicacion <- NA
  }

  if (length(objeto) == 0) {
    objeto <- NA
  }

  if (length(n_expediente) == 0) {
    n_expediente <- NA
  }

  if (length(referencia) == 0) {
    referencia <- NA
  }

  if (length(tipo_contrato) == 0) {
    tipo_contrato <- NA
  }

  if (length(adjudicadora) == 0) {
    adjudicadora <- NA
  }

  if (length(procedimiento) == 0) {
    procedimiento <- NA
  }

  if (length(fecha_publicacion) == 0) {
    fecha_publicacion <- NA
  } else if (length(fecha_publicacion) > 1) {
    fecha_publicacion <- fecha_publicacion[1]
  }

  if (length(duracion) == 0) {
    duracion <- NA
  }

  if (length(adjudicatario) == 0) {
    adjudicatario <- NA
  }

  if (length(NIF) == 0) {
    NIF <- NA
  }

  if (length(conIVA) == 0) {
    conIVA <- NA
  }

  if (length(sinIVA) == 0) {
    sinIVA <- NA
  }


  line <- data.frame(tipo_publicacion, objeto, n_expediente, referencia, tipo_contrato, adjudicadora, procedimiento, fecha_publicacion, duracion, adjudicatario, NIF, conIVA, sinIVA, url)
  print(line)

  write_tsv(line, path = "data/prueba.tsv", append = TRUE, col_names = FALSE)
}
