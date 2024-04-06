# Cargue de librerías ----
library(DBI)
library(RSQLite)
library(tidyverse)
library(naniar)
library(lubridate)
library(stringr)
library(dbplyr)
library(ggplot2)
library(plotly)
library(ggplotly)
library(ggplot2)



## Carga de datos de SQLite3 ----

data_base <-  dbConnect(SQLite(), dbname = 'tables.db')
dbListTables(data_base)

dbGetQuery(data_base, 'SELECT * FROM Prestadores')
dbGetQuery(data_base, 'SELECT * FROM Municipios')

prestadores_data <- dbReadTable(data_base, "Prestadores")
municipios_data <- dbReadTable(data_base, "Municipios")

# Limpieza de datos ----

### Primero corregire los errores de typo de ambas tablas 

glimpse(prestadores_data)
glimpse(municipios_data)

prestadores_data %>% distinct(depa_nombre)
# Después de correr el código de arriba noto que algunos departamentos están separados de sus capitales, tambíen corregire eso 
prestadores_data <- prestadores_data %>%
  mutate(depa_nombre = case_when(
    depa_nombre == "Atl�ntico" ~ "Atlántico",
    depa_nombre == "Barranquilla" ~ "Atlántico",
    depa_nombre == 'Buenaventura' ~ "Valle del cauca",
    depa_nombre == 'Cali' ~ "Valle del cauca",
    depa_nombre == "Santa Marta" ~ "Magdalena",
    depa_nombre == "Bol�var" ~  "Bolívar",
    depa_nombre == 'Cartagena' ~ 'Bolívar',
    depa_nombre == "Bogot� D.C" ~"Bogotá D.C",
    depa_nombre == "Boyac�"   ~ "Boyacá",
    depa_nombre == "Choc�"   ~ "Chocó",
    depa_nombre == "Caquet�"  ~  "Caquetá",
    depa_nombre == "C�rdoba" ~  "Chocó",
    depa_nombre == "Guain�a" ~  "Guainía",
    depa_nombre == "Nari�o"  ~  "Nariño",
    depa_nombre == "Quind�o" ~  "Quindío",
    depa_nombre == "San Andr�s y Providencia"  ~  "San Andrés y Providencia" ,
    depa_nombre == "Vaup�s"  ~  "Vaupés",
      TRUE ~ depa_nombre  
  ))

prestadores_data %>% distinct(depa_nombre)

length(unique(prestadores_data$muni_nombre[str_detect(prestadores_data$muni_nombre, "�")]))  ### No es viable cambiar los nombres de los municipios como arriba


# Ya que está columna  tenía demasiados errores y generaba más de 3 veces el mismo nombre lo cual entorpece el análisis, utilice ayuda de una AI para corregir todos los errores 
municipios_data <- municipios_data %>%
  mutate(Departamento = case_when(
    Departamento == "Ant%ioqUia" ~ "Antioquia",
    Departamento == "Ant%ioquia" ~ "Antioquia",
    Departamento == "Ant%io>qUia" ~ "Antioquia",
    Departamento == "AntioqUia" ~ "Antioquia",
    Departamento == "Antioq>Uia" ~ "Antioquia",
    Departamento == "Ant%ioq>uia" ~ "Antioquia",
    Departamento == "Ant%io>quia" ~ "Antioquia",
    Departamento == "Ant%ioq>Uia" ~ "Antioquia",
    Departamento == "Antioq>uia" ~ "Antioquia",
    Departamento == "Atl%�n>tico" ~ "Atlántico",
    Departamento == "Atl�ntico" ~ "Atlántico",
    Departamento == "Atl%�ntico" ~ "Atlántico",
    Departamento == "Atl�nt>ico" ~ "Atlántico",
    Departamento == "Atl%�nt>ico" ~ "Atlántico",
    Departamento == "Bogot�   D   C" ~ "Bogotá D.C.",
    Departamento == "Bol�var" ~ "Bolívar",
    Departamento == "Bo%l�>var" ~ "Bolívar",
    Departamento == "Bo%l�var" ~ "Bolívar",
    Departamento == "Bol�v>ar" ~ "Bolívar",
    Departamento == "Bol�>var" ~ "Bolívar",
    Departamento == "Boyac�" ~ "Boyacá",
    Departamento == "Boya%c�" ~ "Boyacá",
    Departamento == "Boya%c�   >" ~ "Boyacá",
    Departamento == "Boya%c�   >" ~ "Boyacá",
    Departamento == " Boyac�   >" ~ "Boyacá",
    Departamento == "Boya%c� >" ~ "Boyacá",
    Departamento == "Boyac� >" ~ "Boyacá",
    Departamento == "Boyac�   >" ~ "Boyacá",
    Departamento == "Ca%l>das" ~ "Caldas",
    Departamento == "Caldas" ~ "Caldas",
    Departamento == "Cald>as" ~ "Caldas",
    Departamento == "Ca%ldas" ~ "Caldas",
    Departamento == "Ca%ld>as" ~ "Caldas",
    Departamento == "Ca%qu>et�" ~ "Caquetá",
    Departamento == "Ca%qUet�" ~ "Caquetá",
    Departamento == "Caquet�" ~ "Caquetá",
    Departamento == "Caqu>et�" ~ "Caquetá",
    Departamento == "Caque>t�" ~ "Caquetá",
    Departamento == "CaqUet�" ~ "Caquetá",
    Departamento == "Ca%qU>et�" ~ "Caquetá",
    Departamento == "Ca%quet�" ~ "Caquetá",
    Departamento == "CaqUe>t�" ~ "Caquetá",
    Departamento == "Cau>ca" ~ "Cauca",
    Departamento == "CaUca" ~ "Cauca",
    Departamento == "Cauca" ~ "Cauca",
    Departamento == "Ca%uca" ~ "Cauca",
    Departamento == "Ca%Uca" ~ "Cauca",
    Departamento == "Ca%U>ca" ~ "Cauca",
    Departamento == "CaU>ca" ~ "Cauca",
    Departamento == "Ca%u>ca" ~ "Cauca",
    Departamento == "Cesar" ~ "Cesar",
    Departamento == "Ce%sar" ~ "Cesar",
    Departamento == "Ces>ar" ~ "Cesar",
    Departamento == "Ce%s>ar" ~ "Cesar",
    Departamento == "C�%rd>oba" ~ "Córdoba",
    Departamento == "C�rd>oba" ~ "Córdoba",
    Departamento == "C�rdoba" ~ "Córdoba",
    Departamento == "C�%rdoba" ~ "Córdoba",
    Departamento == "C�rdo>ba" ~ "Córdoba",
    Departamento == "Cundinamarca" ~ "Cundinamarca",
    Departamento == "CUndinamarca" ~ "Cundinamarca",
    Departamento == "Cund%inamarca" ~ "Cundinamarca",
    Departamento == "CUnd%inamarca" ~ "Cundinamarca",
    Departamento == "Cundinam>arca" ~ "Cundinamarca",
    Departamento == "CUndinam>arca" ~ "Cundinamarca",
    Departamento == "CUnd%ina>marca" ~ "Cundinamarca",
    Departamento == "CUnd%inam>arca" ~ "Cundinamarca",
    Departamento == "Cund%ina>marca" ~ "Cundinamarca",
    Departamento == "Cund%inam>arca" ~ "Cundinamarca",
    Departamento == "Choc�" ~ "Chocó",
    Departamento == "Ch%oc�" ~ "Chocó",
    Departamento == "Cho>c�" ~ "Chocó",
    Departamento == "Ch%o>c�" ~ "Chocó",
    Departamento == "Hu%i>la" ~ "Huila",
    Departamento == "HU%ila" ~ "Huila",
    Departamento == "HUila" ~ "Huila",
    Departamento == "Hui>la" ~ "Huila",
    Departamento == "Hu%ila" ~ "Huila",
    Departamento == "Huila" ~ "Huila",
    Departamento == "HU%i>la" ~ "Huila",
    Departamento == "HUi>la" ~ "Huila",
    Departamento == "La   GUajira" ~ "La Guajira",
    Departamento == "La   Guajira" ~ "La Guajira",
    Departamento == "La   GUa>jira" ~ "La Guajira",
    Departamento == "La%   Guajira" ~ "La Guajira",
    Departamento == "La%   Gua>jira" ~ "La Guajira",
    Departamento == "La GUaj>ira" ~ "La Guajira",
    Departamento == "La% Guajira" ~ "La Guajira",
    Departamento == "La GUa>jira" ~ "La Guajira",
    Departamento == "La %GUajira" ~ "La Guajira",
    Departamento == "La GUajira" ~ "La Guajira",
    Departamento == "Magdal>ena" ~ "Magdalena",
    Departamento == "Magdalena" ~ "Magdalena",
    Departamento == "Mag%dal>ena" ~ "Magdalena",
    Departamento == "Mag%dalena" ~ "Magdalena",
    Departamento == "Meta" ~ "Meta",
    Departamento == "Me>ta" ~ "Meta",
    Departamento == "M%eta" ~ "Meta",
    Departamento == "M%e>ta" ~ "Meta",
    Departamento == "Met>a" ~ "Meta",
    Departamento == "Na%ri�o" ~ "Nariño",
    Departamento == "Nari�o" ~ "Nariño",
    Departamento == "Na%r>i�o" ~ "Nariño",
    Departamento == "Nari>�o" ~ "Nariño",
    Departamento == "Na%ri>�o" ~ "Nariño",
    Departamento == "Norte   de   San>tander" ~ "Norte de Santander",
    Departamento == "Norte de San>tander" ~ "Norte de Santander",
    Departamento == "Norte% de San>tander" ~ "Norte de Santander",
    Departamento == "Norte de Santander" ~ "Norte de Santander",
    Departamento == "Norte   de   Santander" ~ "Norte de Santander",
    Departamento == "Norte% de Santander" ~ "Norte de Santander",
    Departamento == "Norte% de Sa>ntander" ~ "Norte de Santander",
    Departamento == "Norte%   de   Santander" ~ "Norte de Santander",
    Departamento == "QU%ind�o" ~ "Quindío",
    Departamento == "Quind�o" ~ "Quindío",
    Departamento == "Qu%ind�o" ~ "Quindío",
    Departamento == "Quind>�o" ~ "Quindío",
    Departamento == "QUind�o" ~ "Quindío",
    Departamento == "Qu%in>d�o" ~ "Quindío",
    Departamento == "QUind>�o" ~ "Quindío",
    Departamento == "Ris%ar>alda" ~ "Risaralda",
    Departamento == "Ris%aralda" ~ "Risaralda",
    Departamento == "Risaralda" ~ "Risaralda",
    Departamento == "Ris%ara>lda" ~ "Risaralda",
    Departamento == "Santan>der" ~ "Santander",
    Departamento == "Santander" ~ "Santander",
    Departamento == "San%tander" ~ "Santander",
    Departamento == "San%ta>nder" ~ "Santander",
    Departamento == "San%tan>der" ~ "Santander",
    Departamento == "Su%c>re" ~ "Sucre",
    Departamento == "SUc>re" ~ "Sucre",
    Departamento == "SU%c>re" ~ "Sucre",
    Departamento == "Sucre" ~ "Sucre",
    Departamento == "SUcre" ~ "Sucre",
    Departamento == "Su%cre" ~ "Sucre",
    Departamento == "Suc>re" ~ "Sucre",
    Departamento == "SU%cre" ~ "Sucre",
    Departamento == "Tolima" ~ "Tolima",
    Departamento == "To%lima" ~ "Tolima",
    Departamento == "Toli>ma" ~ "Tolima",
    Departamento == "To%l>ima" ~ "Tolima",
    Departamento == "To%li>ma" ~ "Tolima",
    Departamento == "Valle   %del   Cauca" ~ "Valle del Cauca",
    Departamento == "Valle   del   Cauca" ~ "Valle del Cauca",
    Departamento == "Valle del Cauca" ~ "Valle del Cauca",
    Departamento == "Valle del >Cauca" ~ "Valle del Cauca",
    Departamento == "Valle   del   >Cauca" ~ "Valle del Cauca",
    Departamento == "Valle del >CaUca" ~ "Valle del Cauca",
    Departamento == "Valle %del> CaUca" ~ "Valle del Cauca",
    Departamento == "Valle   %del   >Cauca" ~ "Valle del Cauca",
    Departamento == "Valle   %del   CaUca" ~ "Valle del Cauca",
    Departamento == "Valle   %del>   Cauca" ~ "Valle del Cauca",
    Departamento == "Valle   del   >CaUca" ~ "Valle del Cauca",
    Departamento == "Valle   del   CaUca" ~ "Valle del Cauca",
    Departamento == "Valle %del> Cauca" ~ "Valle del Cauca",
    Departamento == "Valle %del Cauca" ~ "Valle del Cauca",
    Departamento == "Valle del CaUca" ~ "Valle del Cauca",
    Departamento == "Valle   %del>   CaUca" ~ "Valle del Cauca",
    Departamento == "Valle %del Cauca" ~ "Valle del Cauca",
    Departamento == "Valle %del CaUca" ~"Valle del Cauca",
    Departamento == "AraU>ca" ~ "Arauca",
    Departamento == "Arauca" ~ "Arauca",
    Departamento == "Ar%auca" ~ "Arauca",
    Departamento == "Arau>ca" ~ "Arauca",
    Departamento == "Cas%anare" ~ "Casanare",
    Departamento == "Casanare" ~ "Casanare",
    Departamento == "Cas%an>are" ~ "Casanare",
    Departamento == "Casan>are" ~ "Casanare",
    Departamento == "PUtUmayo" ~ "Putumayo",
    Departamento == "PUt%Umayo" ~ "Putumayo",
    Departamento == "Putumayo" ~ "Putumayo",
    Departamento == "Put%umayo" ~ "Putumayo",
    Departamento == "PUtUm>ayo" ~ "Putumayo",
    Departamento == "San %An>dr�s" ~ "San Andrés",
    Departamento == "San   Andr�s" ~ "San Andrés",
    Departamento == "Ama%zonas" ~ "Amazonas",
    Departamento == "Amazonas" ~ "Amazonas",
    Departamento == "Amazo>nas" ~ "Amazonas",
    Departamento == "Ama%zo>nas" ~ "Amazonas",
    Departamento == "Guain>�a" ~ "Guainía",
    Departamento == "Guain�a" ~ "Guainía",
    Departamento == "Guai>n�a" ~ "Guainía",
    Departamento == "GU%ai>n�a" ~ "Guainía",
    Departamento == "GU%ain�a" ~ "Guainía",
    Departamento == "GUain�a" ~ "Guainía",
    Departamento == "GUavi>are" ~ "Guaviare",
    Departamento == "Guaviare" ~ "Guaviare",
    Departamento == "GUaviare" ~ "Guaviare",
    Departamento == "GUa%viare" ~ "Guaviare",
    Departamento == "Vaup�s" ~ "Vaupés",
    Departamento == "Va%Up>�s" ~ "Vaupés",
    Departamento == "Vaup>�s" ~ "Vaupés",
    Departamento == "Va%Up�s" ~ "Vaupés",
    Departamento == "Vi%ch>ada" ~ "Vichada",
    Departamento == "Vichada" ~ "Vichada",
    TRUE ~ Departamento
  ))

prestadores_data <- prestadores_data %>%
  mutate(clpr_nombre = case_when(
    clpr_nombre == "Objeto Social Diferente a la Prestaci�n de Servicios de Salud" ~ "Objeto Social Diferente a la Prestación de Servicios de Salud",
TRUE ~ clpr_nombre))
 
### Buscado valores nulos para decidir como tratarlos 

colSums(is.na(municipios_data))

colSums(is.na(prestadores_data))

# Teniendo en cuenta que posiblemente estemos hablando de hopitales y centros de salud, las columnas vacias puede proveer información relevante para el análisi más adelante

# Analisis exploratorio ----

## Histograma departamentos
his_departamentos <- ggplot(prestadores_data, aes(x = depa_nombre, text = depa_nombre)) +
  geom_bar(fill = "blue") +
  labs(title = "Presencia de centros de salud u hospitales en departamentos") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(hist_departamentos)

## Histograma clpr
hist_clpr <- ggplot(prestadores_data, aes(x = clpr_nombre, text = clpr_nombre)) +
  geom_bar(fill = "red") +
  labs(title = "Prestadores") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(hist_clpr)

# Gráfico de barras apiladas 
dep_clpre <- ggplot(data = prestadores_data, aes(x = depa_nombre, fill = clpr_nombre, text = paste(depa_nombre , clpr_nombre))) +
  geom_bar() +
  labs(title = "Distribución de Tipos de Instituciones de Salud por Departamento",
       x = "Departamento", y = "Número de Instituciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(dep_clpre)



