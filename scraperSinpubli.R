setwd("/Users/meleiro/Dropbox/MASTER/DATOS/R/CONTRATOS MENORES")


require(stringr)
require(httr)
require(rvest)
require(readr)
require(DistributionUtils)

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
paginas <- 1:119
#paginas <- 1:5

# Creo el índice de los links para cada pagina dentro del rango de fechas
negociados_sin <- paste0("http://www.madrid.org/cs/Satellite?c=Page&cid=1224915242285&codigo=PCON_&fechaFormalizacionDesde=01%2F01%2F2015&fechaFormalizacionHasta=31%2F12%2F2017&idPagina=1224915242285&language=es&newPagina=", paginas,"&numPagListado=5&pagename=PortalContratacion%2FComunes%2FPresentacion%2FPCON_resultadoBuscadorAvanzado&paginaActual=1&paginasTotal=119&rootelement=PortalContratacion%2FComunes%2FPresentacion%2FPCON_resultadoBuscadorAvanzado&site=PortalContratacion&tipoPublicacion=Contratos+adjudicados+por+procedimientos+sin+publicidad")

negociados_sin_tot <- NA

contratos <- data.frame("links")
write_csv(contratos, path = "data/negociados_sin_links.csv", append = FALSE, col_names = FALSE)

p <- length(negociados_sin)

for (licitacion in negociados_sin) {
  x <- GET(licitacion, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  
  contratos <- x %>% read_html() %>% html_nodes(".cajaBlanca a") %>% html_attr("href")
  contratos <- paste0("http://www.madrid.org/", contratos)
  
  contratos <- data.frame(contratos)
  write_csv(contratos, path = "data/negociados_sin_links.csv", append = TRUE, col_names = FALSE)
  print(p)
  p <- p - 1
}

df <- read_csv("data/negociados_sin_links.csv")
negociados_sin.15.17 <-  df$links

## scrapeamos los contratos menores de 2015

line <- data.frame("estado", "tipo_publicacion", "objeto", "n_expediente", "referencia", "tipo_contrato", "adjudicadora", "procedimiento", "sin.impuestos", "importe.total", "fecha_publicacion", "fecha_formalizacion", "fecha_formalizacion_BOCM", "duracion", "adjudicatario", "NIF", "conIVA", "sinIVA", "url")
write_tsv(line, path = "data/prueba_negociados_sin.tsv", append = FALSE, col_names = FALSE)

for (url in 1:length(negociados_sin.15.17)) {
  x <- GET(negociados_sin.15.17[url], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  listado <- x %>% read_html() %>% html_nodes(".listado li") %>% html_text()
  
  estado <- listado[str_detect(listado, "Estado") == TRUE]
  estado <- str_remove(estado, "Estado de la licitación")
  
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
  
  presupuestos_base <- listado[str_detect(listado, "Presupuesto base") == TRUE]
  sin.impuestos <- presupuestos_base[str_detect(presupuestos_base, "sin impuestos|sin incluir IVA")]
  sin.impuestos <- str_remove_all(sin.impuestos, "Presupuesto base licitación| \\(sin impuestos\\)|sin incluir IVA\\.|\n|\\.|euros,|Importe total|Euros,")
  sin.impuestos <- str_remove_all(sin.impuestos, "\\.|euros")
  sin.impuestos <- str_replace(sin.impuestos, ",", ".")
  try(sin.impuestos <- as.numeric(sin.impuestos))
  
  
  importe.total <- presupuestos_base[str_detect(presupuestos_base, "Importe total")]
  importe.total <- str_remove(importe.total, "Presupuesto base licitación. Importe total|Euros,")
  importe.total <- str_remove_all(importe.total, "euros\\,|\\.|euros|\n|IVA incluido\\.")
  importe.total <- str_replace(importe.total, ",", ".")
  try(importe.total <- as.numeric(importe.total))
  
  fecha_publicacion <- listado[str_detect(listado, "Fecha publicación") == TRUE]
  fecha_publicacion <- str_remove_all(fecha_publicacion, "Fecha publicación de la licitación en el BOCM")
  
  fechas_formalizacion <- listado[str_detect(listado, "Formalización") == TRUE]
  fechas_formalizacion <- str_remove_all(fechas_formalizacion, "Formalización del contrato publicada el|Formalización del contrato publicada en BOCM el")
  try(fecha_formalizacion <- fechas_formalizacion[1])
  try(fecha_formalizacion_BOCM <- fechas_formalizacion[2])
  
  duracion <- listado[str_detect(listado, "Duración") == TRUE]
  duracion <- str_remove_all(duracion, "Duración del contrato")
  
  campos <- x %>% read_html() %>% html_nodes("tr+ tr span") %>% html_text()
  valores <- x %>% read_html() %>% html_nodes("td") %>% html_text()
  
  ## if loop para evitar errores cuando hay dos tablas en el contrato
  if (is.wholenumber(length(valores)/length(campos)) == FALSE | length(campos) > 7) {
    try(tabla <- data.frame(campos = campos[1:7], valores = valores[1:7]))
    
    NIF <- tabla$valores[str_detect(tabla$campos, "NIF")]
    adjudicatario <- tabla$valores[str_detect(tabla$campos, "Nombre|razón social")]
    sinIVA <- tabla$valores[str_detect(tabla$campos, "sin IVA")]
    conIVA <- tabla$valores[str_detect(tabla$campos, "con IVA")]
    
    sinIVA <- str_remove_all(sinIVA, "\\.")
    sinIVA <- str_replace(sinIVA, ",", ".")
    sinIVA <- as.numeric(sinIVA)
    
    conIVA <- str_remove_all(conIVA, "\\.")
    conIVA <- str_replace(conIVA, ",", ".")
    conIVA <- as.numeric(conIVA)
  } else if (is.wholenumber(length(valores)/length(campos)) == TRUE) {
    try(tabla <- data.frame(campos, valores))
    
    NIF <- tabla$valores[str_detect(tabla$campos, "NIF")]
    adjudicatario <- tabla$valores[str_detect(tabla$campos, "Nombre|razón social")]
    sinIVA <- tabla$valores[str_detect(tabla$campos, "sin IVA")]
    conIVA <- tabla$valores[str_detect(tabla$campos, "con IVA")]
    
    sinIVA <- str_remove_all(sinIVA, "\\.")
    sinIVA <- str_replace(sinIVA, ",", ".")
    sinIVA <- as.numeric(sinIVA)
    
    conIVA <- str_remove_all(conIVA, "\\.")
    conIVA <- str_replace(conIVA, ",", ".")
    conIVA <- as.numeric(conIVA)
  }
  
  
  if (length(estado) == 0) {
    estado <- NA
  }
  
  if (length(fecha_formalizacion) == 0) {
    fecha_formalizacion <- NA
  }
  if (length(fecha_formalizacion_BOCM) == 0) {
    fecha_formalizacion_BOCM <- NA
  }
  
  if (length(sin.impuestos) == 0) {
    sin.impuestos <- NA
  }
  
  if (length(importe.total) == 0) {
    importe.total <- NA
  }
  
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
  
  
  line <- data.frame(estado, tipo_publicacion, objeto, n_expediente, referencia, tipo_contrato, adjudicadora, procedimiento, sin.impuestos, importe.total, fecha_publicacion, fecha_formalizacion, fecha_formalizacion_BOCM, duracion, adjudicatario, NIF, conIVA, sinIVA, negociados_sin.15.17[url])
  print(line)
  
  try(write_tsv(line, path = "data/prueba_negociados_sin.tsv", append = TRUE, col_names = FALSE))
}
