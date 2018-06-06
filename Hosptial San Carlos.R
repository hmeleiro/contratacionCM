library(tidyverse)
library(ggthemes)
library(scales)
library(ggrepel)


## Primero descargar el dataset de este link del dropbox: https://www.dropbox.com/s/iqy3lz38eerpb5x/contratospublicos_15_17_completo.csv?dl=0
## Segundo poner el dataset en el directorio de trabajo o apuntar en la linea siguiente linea la ruta al dataset
contratos <- read_csv("contratospublicos_15_17_completo.csv")

contratos$fecha_publicacion <- as.Date(contratos$fecha_publicacion, "%d %B %Y")
menores <- contratos[contratos$procedimiento == "Contratos Menores" | 
                       contratos$procedimiento == "Otros procedimientos: Contrato Menor" |
                       contratos$procedimiento == "Otros procedimientos: Contrato menor",]


#####   Limpio el campo tipo de contrato

menores$objeto <- tolower(menores$objeto)
menores$adjudicadora<- tolower(menores$adjudicadora)
menores$NIF <- tolower(menores$NIF)

menores$tipo_contrato[menores$tipo_contrato == "OBRAS"] <- "Obras"
menores$tipo_contrato[menores$tipo_contrato == "Obra"] <- "Obras"
menores$tipo_contrato[menores$tipo_contrato == "obras"] <- "Obras"
menores$tipo_contrato[menores$tipo_contrato == "\nObras"] <- "Obras"

menores$tipo_contrato[menores$tipo_contrato == "SUMINISTROS"] <- "Suministros"
menores$tipo_contrato[menores$tipo_contrato == "SUMINISTRO"] <- "Suministros"
menores$tipo_contrato[menores$tipo_contrato == "Suministro"] <- "Suministros"
menores$tipo_contrato[menores$tipo_contrato == "suministro"] <- "Suministros"
menores$tipo_contrato[menores$tipo_contrato == "\nSuministros"] <- "Suministros"


menores$tipo_contrato[menores$tipo_contrato == "SERVICIOS"] <- "Servicios"
menores$tipo_contrato[menores$tipo_contrato == "Servicio"] <- "Servicios"
menores$tipo_contrato[menores$tipo_contrato == "servicios"] <- "Servicios"
menores$tipo_contrato[menores$tipo_contrato == "C. Servicios"] <- "Servicios"
menores$tipo_contrato[menores$tipo_contrato == "\nServicios"] <- "Servicios"


menores$tipo_contrato[menores$tipo_contrato == "GESTIÓN DE SERVICIOS PÚBLICOS"] <- "Gestión de Servicios Públicos"
menores$tipo_contrato[menores$tipo_contrato == "gestión de servicios públicos"] <- "Gestión de Servicios Públicos"
menores$tipo_contrato[menores$tipo_contrato == "Gestión de servicios públicos"] <- "Gestión de Servicios Públicos"

menores$tipo_contrato[menores$tipo_contrato == "\nAdministrativos Especiales"] <- "Administrativos Especiales"


NIFs <- menores %>% group_by(NIF, adjudicatario) %>% summarise(contratos.recibidos = n())

NIFs <- NIFs[!duplicated(NIFs$NIF),]


####  SAN CARLOS
objeto.dia <- menores %>% group_by(objeto, adjudicadora, NIF, tipo_contrato, fecha_publicacion) %>% summarise(N = n(), sinIVA = sum(sinIVA)) %>% arrange(desc(sinIVA))
objeto.dia <- objeto.dia[!is.na(objeto.dia$sinIVA),]

objeto.dia <- merge(NIFs, objeto.dia, by = "NIF", all.y = TRUE)


objeto.dia <- objeto.dia[(objeto.dia$tipo_contrato == "Obras" & objeto.dia$sinIVA > 50000) |  (objeto.dia$tipo_contrato != "Obras" & objeto.dia$sinIVA > 18000),]
objeto.dia <- objeto.dia[objeto.dia$adjudicadora == "consejería de sanidad-servicio madrileño de salud -hospital clínico san carlos",]
objeto.dia <- objeto.dia[objeto.dia$N > 1,]

objeto.dia$mth <- as.numeric(format(objeto.dia$fecha_publicacion, format = "%m"))
objeto.dia$yr <- as.numeric(format(objeto.dia$fecha_publicacion, format = "%Y"))

p <- ggplot(objeto.dia, aes(x = fecha_publicacion, y = sinIVA))
p + geom_point(aes(color = as.character(yr), size = N), alpha = 0.5) + 
  scale_y_log10(breaks = c(18000, 100000), labels = c("18.000€", "100.000€")) + 
  scale_x_date(breaks = c(as.Date("2015-10-01"), as.Date("2016-01-04"), as.Date("2017-01-01")), labels = c("Oct. 2015", "2016", "2017")) +
  scale_color_wsj() +
  labs(title = "Ocasiones en las que se suscriben contratos menores para un mismo objeto \nen un mismo día y a un mismo adjudicatario en el hospital San Carlos",
       subtitles = "Solo se contemplan las ocasiones en las que la suma del importe de los contratos supera los 18.000 €.",
       color = "Año",
       x = "Fecha",
       y = "Importe sin I.V.A.",
       size = "Número de contratos",
       caption = "Fuente: Dataset contratación pública en la Comunidad de Madrid (2015-2017) elaborado por @hmeleiros") +
  theme_minimal(base_family = "Roboto Condensed") + 
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(margin = margin(t = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))) + 
  geom_text_repel(data = objeto.dia[objeto.dia$N == 105,], label = "105 contratos para \nbombas de insulina \na Medtronic Ibérica S.A.", min.segment.length = 0.1, family = "Roboto Condensed", nudge_x = 155, nudge_y = 0.3, size = 3.4)





sancarlos <- menores[menores$adjudicadora == "consejería de sanidad-servicio madrileño de salud -hospital clínico san carlos",]

sancarlos$mth <- as.numeric(format(sancarlos$fecha_publicacion, format = "%m"))
sancarlos$yr <- as.numeric(format(sancarlos$fecha_publicacion, format = "%Y"))

#sancarlos <- sancarlos[(sancarlos$tipo_contrato == "Obras" & sancarlos$sinIVA > 49000) |  (sancarlos$tipo_contrato != "Obras" & sancarlos$sinIVA > 17000),]


p <- ggplot(sancarlos, aes(x = fecha_publicacion, y = sinIVA))
p + geom_point(aes(color = as.character(yr)), size = 0.2, alpha = 0.5) + 
  scale_y_log10(labels = dollar, breaks = c(18000)) + 
  scale_x_date(breaks = c(as.Date("2015-10-01"), as.Date("2016-01-04"), as.Date("2017-01-01")), labels = c("Oct. 2015", "2016", "2017")) +
  scale_color_wsj() +
  theme_minimal(base_family = "Roboto Condensed") + 
  labs(title = "",
       subtitles = "",
       color = "Año",
       x = "Fecha",
       y = "Importe sin I.V.A.")


