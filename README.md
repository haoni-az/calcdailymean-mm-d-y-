library(dplyr)
library(ncdf4)
library(terra)
library(raster)
library(sf)
library(ggplot2)

# Definir o diretório contendo os arquivos NetCDF
directory <- "D:/CHIRPS"

# Listar todos os arquivos NetCDF no diretório
nc_files <- list.files(path = directory, pattern = "\\.nc$", full.names = TRUE)
    
# Definir os limites geográficos (longitude e latitude)
lon_min <- -43
lon_max <- -42
lat_min <- -22
lat_max <- -20

# Inicializar uma lista para armazenar os dados de precipitação
precip_data_list <- list()

# Loop através de cada arquivo NetCDF e ler os dados dentro dos limites
for (file in nc_files) {
  cat("Processando arquivo:", file, "\n")
  
  # Tentar abrir o arquivo NetCDF
  nc_data <- tryCatch(nc_open(file), error = function(e) {
    cat("Erro ao abrir o arquivo:", file, "\n", e, "\n")
    return(NULL)
  })
  
  # Se o arquivo não puder ser aberto, pule para o próximo arquivo
  if (is.null(nc_data)) next
  
  # Verificar as variáveis disponíveis no arquivo
  var_names <- names(nc_data$var)
  cat("Variáveis disponíveis no arquivo:", var_names, "\n")
  
  # Verificar se a variável 'precip' existe no arquivo
  if (!"precip" %in% var_names) {
    cat("Variável 'precip' não encontrada no arquivo:", file, "\n")
    nc_close(nc_data)
    next
  }
  
  # Ler as variáveis de longitude e latitude
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  
  # Encontrar os índices das longitudes e latitudes dentro dos limites especificados
  lon_idx <- which(lon >= lon_min & lon <= lon_max)
  lat_idx <- which(lat >= lat_min & lat <= lat_max)
  
  # Ler os dados de precipitação dentro dos limites especificados
  precip <- ncvar_get(nc_data, "precip", start = c(min(lon_idx), min(lat_idx), 1), 
                      count = c(length(lon_idx), length(lat_idx), -1))
  
  # Fechar o arquivo NetCDF
  nc_close(nc_data)
  
  # Adicionar os dados de precipitação à lista
  precip_data_list[[file]] <- precip
}

# Verificar se algum dado foi lido
if (length(precip_data_list) == 0) {
  stop("Nenhum dado de precipitação foi lido. Verifique os arquivos NetCDF e os limites geográficos.")
}

###########################################################
###########################################################
#CALCULO CONSIDERANDO MEDIA DIARIA PARA OS ANOS

###########################################################
###########################################################

# Calcular a média anual para cada pixel
mean_annual_precip_list <- list()

for (i in 1:length(precip_data_list)) {
  mean_annual_precip <- apply(precip_data_list[[i]], c(1, 2), mean, na.rm = TRUE)
  mean_annual_precip_list[[i]] <- mean_annual_precip
}

# Converter a lista de médias anuais para um stack de raster
raster_stack <- stack()

for (i in 1:length(mean_annual_precip_list)) {
  mean_precip_raster <- raster(mean_annual_precip_list[[i]])
  extent(mean_precip_raster) <- extent(lon_min, lon_max, lat_min, lat_max)
  projection(mean_precip_raster) <- CRS("+proj=longlat +datum=WGS84")
  raster_stack <- stack(raster_stack, mean_precip_raster)
}

# Nomear as camadas do raster stack com os anos correspondentes
years <- 1985:2022
names(raster_stack) <- as.character(years)

# Calcular a média final ao longo dos anos para cada pixel
mean_precip_final <- calc(raster_stack, fun = mean, na.rm = TRUE)

# PLOTAR O RASTER COM A MÉDIA DA PRECIPITAÇÃO ANUAL DO PERÍODO ESTUDADO
plot(mean_precip_final, main = "Média de Precipitação Anual (1985-2022)",
     xlab = "Longitude", ylab = "Latitude")

# Salvar o raster de média final como um arquivo GeoTIFF
output_raster <- "/home/usuario/MEDIAPRECIPITACAOdiaria1985-2022.tif"
writeRaster(mean_precip_final, filename = output_raster, format = "GTiff", overwrite = TRUE)

# Mensagem de confirmação
cat("Raster de média de precipitação anual exportado para:", output_raster, "\n")

# Calcular a média anual de precipitação para a região
region_mean_precip <- cellStats(raster_stack, stat = 'mean', na.rm = TRUE)

# Criar um dataframe para plotar a variação da média de precipitação ao longo dos anos
df <- data.frame(Year = years, MeanPrecip = region_mean_precip)

# Plotar a variação da média de precipitação ao longo dos anos
ggplot(df, aes(x = Year, y = MeanPrecip)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("Variação da Média de Precipitação Anual (1985-2022)") +
  xlab("Ano") +
  ylab("Média de Precipitação (mm)") +
  theme_minimal()
