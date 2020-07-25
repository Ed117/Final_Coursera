library(ggplot2)

# Visualizamos los datos
ggplot(LI3,aes(x=Peso,y=VitaminaD)) + 
            geom_boxplot(notch=FALSE,fill="blue",alpha=0.5, outlier.shape = 23) + facet_wrap(~Grupo) +
            geom_jitter(alpha=0.2,width=.2) + 
            labs(y="Nivel de Vitamina D", 
                 title="Efecto del peso y nivel de vitamina D en intolerancia a la lactosa", 
                 subtitle = "Prueba en niños") + 
            theme_gray() + 
            theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Normalidad
ggplot(LI3,aes(x=VitaminaD, fill=Peso)) + geom_histogram(col=I("black"), alpha=0.5) + 
            labs(x="Nivel de Vitamina D", y="Frecuencia", 
                 title="Efecto del peso y nivel de vitamina D en intolerancia a la lactosa", 
                  subtitle = "Prueba en niños") + 
                  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Uso la prueba de Shaphiro Wilk para ver si es normal
shapiro.test(LI3$VitaminaD) # en este caso el valor p < 0.05 (no es una distribucion normal)

# homoscedasticidad
library(car)
leveneTest(LI3$Altura_z_score~LI3$Grupo*LI3$Peso) # en este caso el valor p > 0.05 hay homoscedasticidad

modelo1<-aov(VitaminaD~Grupo*Peso, data=LI3)
summary(modelo1) # aqui se aprecia que no hay diferencias significativas

# 4) Posthoc
TukeyHSD(modelo1) # los individuos de peso normal presentaron mayor nivel de vitamina D



#------------------------------------------------------------------------------------------------------#



# Visualizamos los datos
ggplot(LI3,aes(x=Peso,y=Altura_z_score)) + 
  geom_boxplot(notch=FALSE,fill="blue",alpha=0.5, outlier.shape = 23) + facet_wrap(~Grupo) +
  geom_jitter(alpha=0.2,width=.2) + 
  labs(y="z-score de estatura", 
       title="Efecto del peso y estatura en intolerancia a la lactosa", 
       subtitle = "Prueba en niños") + 
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Normalidad: se ve muy normal
ggplot(LI3,aes(x=Altura_z_score, fill=Peso)) + geom_histogram(col=I("black"), alpha=0.5) + 
  labs(x="Puntuación z de estatura", y="Frecuencia", 
       title="Efecto de la estatura y peso en intolerancia a la lactosa", 
       subtitle = "Prueba en niños") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Uso la prueba de Shaphiro Wilk para ver si es normal
shapiro.test(LI3$Altura_z_score) # en este caso el valor p > 0.05 (SI es una distribucion normal)

# homoscedasticidad
leveneTest(LI3$Altura_z_score~LI3$Peso*LI3$Peso) # en este caso el valor p > 0.05 hay homoscedasticidad

modelo2<-aov(Altura_z_score~Grupo*Peso, data=LI3)
summary(modelo2) # aqui se aprecia que no hay diferencias significativas

# 4) Posthoc
TukeyHSD(modelo2) # lo que se aprecia es que los individuos con sobrepeso son mas altos



#------------------------------------------------------------------------------------------------------#

glm1<-glm(VitaminaD ~ Peso*Grupo, data=LI3, family=poisson)
summary(glm1)

glm1<-glm(VitaminaD ~ BMI*Grupo, data=LI3, family=poisson)
summary(glm1)
