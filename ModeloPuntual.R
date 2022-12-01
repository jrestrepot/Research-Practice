library(MASS)
library(car)
library(lmtest)
library(ggpubr)
library(GGally)
library(jtools)

data <- data.frame(data_total_s)
data$numFuerzaPub = NULL #Very sparse
data = sapply(data, as.numeric)
setwd('C:/Users/Asus/Downloads/Programas') 
espac <- st_read(
  "content/Espacio_Publico")
idx = espac3$X
for(i in 1:length(idx)){
  idx[i] = idx[i] +1
}
espac = espac[idx,]
espac = sf::st_make_valid(espac)
dataest <- data.frame(data)
dataest$usos <- NULL
new_df = cbind(dataest,espac$geometry)
new_df = na.omit(new_df)
dataest = na.omit(dataest)
#center = colMeans(data)
#cova = cov(data)
#dist = mahalanobis(data,center,cova, inverted = FALSE)
#p = quantile(dist,0.95)
#idx = which(dist>p)
#data <- data[-idx,]
dataest$iv_hom <-NULL
dataest$hom <- NULL
dataest$iv_hurt <- NULL
dataest$usos_..Espacio.Público.Existente.. <- NULL
dataest$comuna_SN01 <- NULL
hurtos <- dataest$hurt
ggdensity(hurtos, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")
mean(hurtos)
var(hurtos)
modelo = lm(hurt~.,data = dataest)
plot(resid(modelo))
st_as_sf(new_df)
to_plot <- new_df %>%
  mutate(olsresid = resid(modelo))
to_plot = st_as_sf(to_plot, sf_column_name = 'geometry')
tm_shape(to_plot, unit = "mi") +
  tm_polygons(col = "olsresid", style = "equal",palette = "Blues", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "Residuals from linear regression",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE,
            attr.outside = TRUE)
AIC(modelo)
summ(modelo)
bptest(modelo) #Existe
vif(modelo)
ggdensity(modelo$residuals, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")
shapiro.test(modelo$residuals)
modback <- stepAIC(modelo, trace=TRUE, direction="backward")
AIC(modback)
summary(modback)
bptest(modback) #Existe
vif(modback)
ggdensity(modback$residuals, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")
shapiro.test(modback$residuals)
