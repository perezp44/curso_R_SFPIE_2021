#- script para bajar la Estimación Mensual de Nacimientos
#- https://www.ine.es/experimental/nacimientos/experimental_nacimientos.htm
#- Tabla 4: Nacimientos mensuales, acumulados y diferencia absoluta interanual del acumulado. Total nacional y provincias
url_descarga <- "https://www.ine.es/jaxiT3/files/t/es/xlsx/46681.xlsx?nocab=1"
nombre_fichero <- "tabla_46681.xlsx"
fs::dir_create("tmp")   #- creo un directorio temporal
my_ruta <- here::here("tmp", nombre_fichero) #- ruta para guardar el fichero
curl::curl_download(url = url_descarga, destfile = my_ruta)

#- leo el df
df_ini <- readxl::read_xlsx(path = my_ruta, skip = 6)

#- comienzo a jugar
library(tidyverse)

#- guardo los totales (para luego comprobar)
df_totales <- df_ini %>% slice(2) %>% pivot_longer(-1)

#- quito totales y notas al pie
df <- df_ini %>% slice(6:265) #- quito Totales y notas al pie


#- quiero quitar las filas sin datos (solo tienen el nombre de las provincia)
vv <- seq(1, 52*5, by = 5)
df_xx <- df %>% slice(vv) %>% select(1)
df_xx <- janitor::clean_names(df_xx)

#- creo un df con los nombres provinciales (x4)
df_xx <- df_xx %>% mutate(nn = 4)
df_xx <- df_xx %>% tidyr::uncount()

#df_xx <- df_xx %>% tidyr::separate(x1, sep = " ", into = c("ine_prov", "ine_prov.n"))
#df_xx <- df_xx %>% tidyr::separate(x1, sep = " ", into = c("ine_prov", "ine_prov.n"), extra = "merge")

df <- df %>% slice(-vv)  #- quito las filas sin datos (nombres de las prov)
df <- df %>% rename(names_var = `...1`)

#- juntamos df_xx y df para poner el nombre de las provincias
df <- bind_cols(df_xx, df) %>% select(-nn)

#- separo código y nombre de las provincias
#df <- df %>% tidyr::separate(x1, sep = " ", into = c("ine_prov", "ine_prov.n"))
df <- df %>% tidyr::separate(x1, sep = " ", into = c("ine_prov", "ine_prov.n"), extra = "merge")

#- los años a largo
df <- df %>% pivot_longer(cols = 4:67, names_to = "periodo")



#- separamos periodo en anyo y mes
df <- df %>% tidyr::separate(periodo, sep = "M", into = c("anyo", "mes"), extra = "merge")


#- creo fecha
df <- df %>% mutate(fecha = lubridate::as_date(paste0(anyo, "-", mes, "-01")))
str(df)


#- selecciono 4 meses
zz_base <- df %>% filter(names_var == "Dato base") %>% filter(mes %in% c("01", "02", "03", "04"))


p <- zz_base %>% #filter(mes == "01") %>% 
  ggplot() + 
  geom_line(aes(x = anyo, y = value, color = ine_prov.n, group = ine_prov.n), size = 1.5) +
  geom_label(aes(x = anyo, y = value, label = value)) +
  labs(title = "Evolución del nº de nacimientos por provincia (mes de Enero)",
       subtitle = "",
       x = NULL,
       y = "Nº de bebes",
       caption = "INE Experimental: Estimación Mensual de Nacimientos. Visualización: @pjpv4444. PERO no he tenido tiempo ni ganas de adaptarlo. El plot original está en https://perezp44.github.io/pjperez.web/posts/2021-02-22-nomenclator-municipio-pueblo-o-entidad/", 
       color = NULL) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  scale_x_discrete(expand = expansion(mult = c(0.04, .225))) +
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"),
                          legend.position = "none",
                          text = element_text(family = "Ubuntu Regular", color = "#22211d"),
                          plot.background = element_rect(fill = "ghostwhite", color = NA),
                          plot.caption = element_text(hjust = 0.05))
p





p + facet_wrap(vars(mes), ncol = 2) #-


#- no he tenido tiempo para adaptarlo:
#- el plot original en: https://perezp44.github.io/pjperez.web/posts/2021-02-22-nomenclator-municipio-pueblo-o-entidad/



#- COMPROBACIONES: no suman, se van en 2020 y 2021 en 2-5 bebes, supongo por decimales de las estimaciones
zz <- df %>% filter(names_var == "Dato base") %>% group_by(mes, anyo) %>% 
  summarise(bebes = sum(value)) %>% ungroup


zz <- df %>% filter(names_var == "Dato base") %>% filter(mes == "04")  %>% filter(anyo == "2021") %>% summarise(bebes = sum(value))

#- veo totales
df_totales <- df_totales %>% tidyr::separate(name, sep = "M", into = c("anyo", "mes"), extra = "merge") %>% select(-1)

comprobacion <- left_join( zz, df_totales) %>% dplyr::mutate(dif = bebes - value)
