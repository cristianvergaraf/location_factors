# Assessment of location factors


**Descripción corta:** Análisis de factores de localización de la expansión de plantaciones forestales en dos cuencas costeras del centro-sur de Chile entre los años 1987-2015, 
con gestión de dependencias a través de `renv`.

![Estado](https://img.shields.io/github/actions/workflow/status/usuario/repositorio/ci.yml)
![Licencia](https://img.shields.io/github/license/usuario/repositorio)
![R Version](https://img.shields.io/badge/R-4.3.1-blue)

---

## 📝 Contenidos
- [Descripción](#descripción)
- [Requisitos](#requisitos)
- [Instalación](#instalación)
- [Uso](#uso)
- [Características](#características)
- [Ejemplo de análisis](#ejemplo-de-análisis)
- [Contribución](#contribución)
- [Licencia](#licencia)
- [Contacto](#contacto)

---

## 📌 Descripción
Este proyecto realiza análisis de datos espaciales, incluyendo:  
- Visualización de mapas y capas espaciales  
- Análisis de patrones geoespaciales  
- Estadísticas espaciales y geoprocesamiento  

Se utiliza `R` junto con paquetes especializados como `sf`, `raster`, `tidyverse` y `ggplot2`.  

El proyecto gestiona dependencias mediante `renv` para garantizar reproducibilidad.

---

## ⚙️ Requisitos

- [R](https://cran.r-project.org/) >= 4.0  
- [RStudio](https://www.rstudio.com/) recomendado  
- `renv` para manejo de paquetes (`install.packages("renv")`)

---

## 🛠 Instalación

Clona el repositorio y usa `renv` para restaurar el entorno:

```r
# Clonar el repositorio
git clone https://github.com/usuario/repositorio.git
setwd("repositorio")

# Instalar renv si no está instalado
if(!require(renv)) install.packages("renv")

# Restaurar dependencias del proyecto
renv::restore()
