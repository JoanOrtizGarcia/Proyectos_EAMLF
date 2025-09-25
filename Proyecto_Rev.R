
# Limpiando el entorno de trabajo
rm(list = ls())

# Librerias ---------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyverse)
library(corrplot)
library(caret)

# -------------------------------------------------------------------------

# Seleeccionando el archivo para conocer su ruta
file.choose()

# Creando la variable que contenga los datos con la ruta obtenida

datosubicacion <- "D:\\Proyecto ML\\R Studio\\student_prediction.csv"

# Exportando la base de datos de la variable creada

datos <- read_csv(datosubicacion)

# Exploración --------------------------------------------------------------


# Visualizando los 6 primeros datos exceptuando 16 variables
head(datos)


# Viendo los datos por completo
View(datos)

# La única columna con "0" (cero) es "GRADE" dada la forma de construcción 
# de la muestra y constituye una categoria más para esta variable

# Obteniendo un resumen de los datos
summary(datos) 

# Se observan todos los datos ya numéricos excepto la columna "STUDENTID", aún 
# cuando son variables categóricas

# Observando el tipo de datos por columna
str(datos) 
glimpse(datos)

# Todas las variables son del mismo tipo (double), excepto "STUDENTID" (charac)

# Viendo si hay datos nulos en la base
sum(is.na(datos))

# No parecen haber datos nulos en ninguna columna

# Viendo la cantidad de datos con "0" en la categoria de notas
length(which(datos$GRADE==0)) 

# Se sabe que del total de la muestra, 8 perdieron el curso

# Observando la distribución por cada categoria de notas
table(datos$GRADE)  

# Muestra la cantidad de personas en cada rango de notas donde 8 perdieron el 
# curso y 17 lograron pasar con la mejor nota

# Distribucion por categoria de notas pero en porcentaje
prop.table(table(datos$GRADE)) 

# Transformación ------------------------------------------------------------

# Transformando a categóricas las variables correspondientes

datos$AGE <- as.factor(datos$AGE)
datos$GENDER <- as.factor(datos$GENDER)
datos$HS_TYPE <- as.factor(datos$HS_TYPE)
datos$SCHOLARSHIP <- as.factor(datos$SCHOLARSHIP)
datos$WORK <- as.factor(datos$WORK)
datos$ACTIVITY <- as.factor(datos$ACTIVITY)
datos$PARTNER <- as.factor(datos$PARTNER)
datos$SALARY <- as.factor(datos$SALARY)
datos$TRANSPORT <- as.factor(datos$TRANSPORT)
datos$LIVING <- as.factor(datos$LIVING)
datos$MOTHER_EDU <- as.factor(datos$MOTHER_EDU)
datos$FATHER_EDU <- as.factor(datos$FATHER_EDU)
datos$`#_SIBLINGS` <- as.factor(datos$`#_SIBLINGS`)
datos$KIDS <- as.factor(datos$KIDS)  # "Estado de los padres"
datos$MOTHER_JOB <- as.factor(datos$MOTHER_JOB)
datos$FATHER_JOB <- as.factor(datos$FATHER_JOB)
datos$STUDY_HRS <- as.factor(datos$STUDY_HRS)
datos$READ_FREQ <- as.factor(datos$READ_FREQ)
datos$READ_FREQ_SCI <- as.factor(datos$READ_FREQ_SCI)
datos$ATTEND_DEPT <- as.factor(datos$ATTEND_DEPT)
datos$IMPACT <- as.factor(datos$IMPACT)
datos$ATTEND <- as.factor(datos$ATTEND)
datos$PREP_STUDY <- as.factor(datos$PREP_STUDY)
datos$PREP_EXAM <- as.factor(datos$PREP_EXAM)
datos$NOTES <- as.factor(datos$NOTES)
datos$LISTENS <- as.factor(datos$LISTENS)
datos$LIKES_DISCUSS <- as.factor(datos$LIKES_DISCUSS)
datos$CLASSROOM <- as.factor(datos$CLASSROOM)
datos$GRADE <- as.factor(datos$GRADE)

# Comprobando que sean categóricas las transformadas
summary(datos)

# Creando una variable que contenga el nombre correspondiente y su valor 
# exluyendo las que no brinden información al modelo

datoseducacion_long <- datos %>% gather(key = "variable", value = "valor", -`COURSE ID`, -STUDENTID)

# Comprobando lo anterior
head(datoseducacion_long)

# Observando que efectivamente tome los "0" como otra categoria en la columna "Grade"
levels(datos$GRADE) 

# Revisando datos nulos de otras formas

# En caso de que hayan arrojará "True", caso contrario "False"
any(!complete.cases(datos)) 

#Revisando si hay vacíos por cada columna
datos %>% map_lgl(.f = function(x) {any(!is.na(x) & x == "")})

# Todas las columnas arrojaron "False" indicando que no hay datos vacíos

# Comprobando finalmente si hay nulos por columna pero de forma numérica
map_dbl(datos, .f = function(x) {sum(is.na(x))})

# Graficando ------------------------------------------------------------------

# Graficas por cada variable (observar diccionario al final del script) versus
# la variable de interés a investigar




ggplot(data = datos, aes(x = AGE, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = GENDER, fill = GRADE)) + geom_bar(alpha=0.7)
#Ninguna persona del grupo 1 (mujeres) sacó la mejor nota posible (pero si muy cercano)
#Ninguna persona del grupo 2 (hombres) reprobó el curso

ggplot(data = datos, aes(x = HS_TYPE, fill = GRADE)) + geom_bar(alpha=0.5)

ggplot(data = datos, aes(x = SCHOLARSHIP, fill = GRADE)) + geom_bar(alpha=0.7)
# Ninguna persona con el 100% de la beca, reprobó el curso y la proporción de
# pase condicional también es mas bajo en comparación al resto de categorias

ggplot(data = datos, aes(x = WORK, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = ACTIVITY, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = PARTNER, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = SALARY, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = TRANSPORT, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = LIVING, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = MOTHER_EDU, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = FATHER_EDU, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = `#_SIBLINGS`, fill = GRADE)) + geom_bar(alpha=0.3)
# Tienden a perder más quienes tienen solamente 1 hermano?

ggplot(data = datos, aes(x = KIDS, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = MOTHER_JOB, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = FATHER_JOB, fill = GRADE)) + geom_bar(alpha=0.7)
# Tienden a perder menos cuando la situación laboral del padre es retirado? 

ggplot(data = datos, aes(x = STUDY_HRS, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = READ_FREQ, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = READ_FREQ_SCI, fill = GRADE)) + geom_bar(alpha=0.7)
# Ambas graficas son muy similares, por lo que al tratarse de la misma 
#categoria "¿lee?" podría dejarse una de las dos y dejarla como un proxy de la otra

ggplot(data = datos, aes(x = ATTEND_DEPT, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = IMPACT, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = ATTEND, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = PREP_STUDY, fill = GRADE)) + geom_bar(alpha=0.7)

ggplot(data = datos, aes(x = PREP_EXAM, fill = GRADE)) + geom_bar(alpha=0.7)
# Mismo caso que la variable "READ_FREQ", la preparación para el examen podría 
# dejarse como solamente una

ggplot(data = datos, aes(x = NOTES, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = LISTENS, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = LIKES_DISCUSS, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = CLASSROOM, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = CUML_GPA, fill = GRADE)) + geom_bar(alpha=0.3)

ggplot(data = datos, aes(x = EXP_GPA, fill = GRADE)) + geom_bar(alpha=0.3)
# Al ver la gráfica se observa que una de las categorias fue eliminada

#Confirmando que una de las categorias para "EXP_GPA" (nota esperada al graduarse)
# cuenta con 0 datos en la muestra

length(which(datos$EXP_GPA==5)) 
# Ninguna de las personas encuestadas espera tener el promedio por encima de 3.49
# recordando que el máximo de nota posible es 4.00


# Viendo las tablas por cada una de las variables que más interesantes fueron

prop.table(table(datos$GENDER, datos$GRADE))
prop.table(table(datos$SCHOLARSHIP, datos$GRADE))
prop.table(table(datos$`#_SIBLINGS`, datos$GRADE))
prop.table(table(datos$FATHER_JOB, datos$GRADE))
prop.table(table(datos$READ_FREQ_SCI, datos$GRADE))
prop.table(table(datos$PREP_EXAM, datos$GRADE))


# Gráfico más específico de categorias a extremo, (reprobo/mejor nota)
ggplot(data = datos, aes(x = GENDER, fill = GRADE == 0)) + geom_bar(alpha=0.5)
# El 0 corresponde a reprobar el curso y muestra la distribución
# de la variable género, siendo 1 mujer, 2 hombre

ggplot(data = datos, aes(x = GENDER, fill = GRADE == 7)) + geom_bar(alpha=0.5)
# El 7 corresponde a aprobar con nota más alta el curso y muestra la distribución
# de la variable género, siendo 1 mujer, 2 hombre

ggplot(data = datos, aes(x = PREP_EXAM, fill = GRADE == 7)) + geom_bar(alpha=0.5)
# La mayoría de la muestra pareciera estudiar estando más cerca al parcial que
# durante el semestre, y parece que esta característica tampoco influye mucho 
#en si obtiene buenas notas

# Viendo si las personas que tienen pareja o no, tienen a obtener mejor o peor nota

ggplot(data = datos, aes(x = PARTNER, fill = GRADE == 7)) + geom_bar(alpha=0.5)
ggplot(data = datos, aes(x = PARTNER, fill = GRADE == 0)) + geom_bar(alpha=0.5)
# No pareciera en un primer momento afectar mucho a simple vista esta variable
# A la aprobación con alta nota o reprobación del curso, 1 tiene, 2 no tiene


# Tabla de frecuencias de las notas obtenidas con base en si tiene o no pareja
# 1 Sí tiene, 2 No tiene
prop.table(table(datos$PARTNER, datos$GRADE), margin = 1) %>% round(digits = 2)

# Tabla de frecuencias relativas donde se observa que para las personas en las que 
# su padre está retirado del trabajo tienden a no reprobar el curso
prop.table(table(datos$FATHER_JOB, datos$GRADE), margin = 1) %>% round(digits = 2)


prop.table(table(datos$MOTHER_JOB, datos$GRADE), margin = 1) %>% round(digits = 2)
ggplot(data = datos, aes(x = FATHER_JOB, y = ..count.., fill = GRADE)) + geom_bar()
# Si bien para la categoria de "1" en la situación laboral de la madre pareciera
# una muestra más pequeña, se tiene la misma tendencia que en el caso del padre

# A primera vista, cuando al menos alguno de los dos padres tiene como
# situación laboral "retirado" una persona podría tender a perder menos el curso 

# --------------------------------------------------------------------------------

datos2 <- select(datos, -STUDENTID, -`COURSE ID`, -CUML_GPA, -EXP_GPA)

summary(datos2)

# Transformando a variables numéricaso double para poder hacer correlaciones

datos2$AGE <- as.numeric(datos2$AGE)
datos2$GENDER <- as.numeric(datos2$GENDER) 
datos2$HS_TYPE <- as.numeric(datos2$HS_TYPE) 
datos2$SCHOLARSHIP <- as.numeric(datos2$SCHOLARSHIP) 
datos2$WORK <- as.numeric(datos2$WORK) 
datos2$ACTIVITY <- as.numeric(datos2$ACTIVITY) 
datos2$PARTNER <- as.numeric(datos2$PARTNER) 
datos2$SALARY <- as.numeric(datos2$SALARY) 
datos2$TRANSPORT <- as.numeric(datos2$TRANSPORT) 
datos2$LIVING <- as.numeric(datos2$LIVING) 
datos2$MOTHER_EDU <- as.numeric(datos2$MOTHER_EDU) 
datos2$FATHER_EDU <- as.numeric(datos2$FATHER_EDU) 
datos2$`#_SIBLINGS` <- as.numeric(datos2$`#_SIBLINGS`) 
datos2$KIDS <- as.numeric(datos2$KIDS) 
datos2$MOTHER_JOB <- as.numeric(datos2$MOTHER_JOB) 
datos2$FATHER_JOB <- as.numeric(datos2$FATHER_JOB) 
datos2$STUDY_HRS <- as.numeric(datos2$STUDY_HRS) 
datos2$READ_FREQ <- as.numeric(datos2$READ_FREQ) 
datos2$READ_FREQ_SCI <- as.numeric(datos2$READ_FREQ_SCI) 
datos2$ATTEND_DEPT <- as.numeric(datos2$ATTEND_DEPT) 
datos2$IMPACT <- as.numeric(datos2$IMPACT) 
datos2$ATTEND <- as.numeric(datos2$ATTEND ) 
datos2$PREP_STUDY <- as.numeric(datos2$PREP_STUDY) 
datos2$PREP_EXAM <- as.numeric(datos2$PREP_EXAM ) 
datos2$NOTES <- as.numeric(datos2$NOTES) 
datos2$LISTENS <- as.numeric(datos2$LISTENS ) 
datos2$LIKES_DISCUSS <- as.numeric(datos2$LIKES_DISCUSS) 
datos2$CLASSROOM <- as.numeric(datos2$CLASSROOM) 
datos2$GRADE <- as.numeric(datos2$GRADE ) 

# Confirmando que sean double
summary(datos2)

# Haciendo la matriz de correlaciones
corrplot(cor(datos2),method = "number",tl.cex = 0.5)

# Haciendo las particiones

Particiondatos2 <- createDataPartition(y = datos2$GRADE, p = 0.8, list = FALSE, times = 1)
datostrainX <- datos2[Particiondatos2, ] #80% para entrenamiento (todas las observaciones incluyendo Y)
datostrainY <- datos2$GRADE[Particiondatos2] #80% para entrenamiento (solo Y)

datostestX <- datos2[-Particiondatos2, ] #20% para testear/evaluar (todo, incluye Y)
datostestY <- datos2$GRADE[-Particiondatos2] #20% para testear/evaluar (solo Y)


plot(datostrainY, col="blue")
points(datostestY)

# --------------------------------------------------------------------------


#Diccionario de variables --------------------------------------------------

# Variables categóricas:
# Género - 1 Mujer, 2 Hombre 
# Escuela - 1 Privada, 2 Pública, 3 Otra 
# Tipo de beca - 1 No, 2 25%, 3 50%, 4 45%, 5 Completa
# Trabaja - 1 Si, 2 No
# Actividad regular artística o deporte - 1 Si, 2 No
# Tiene pareja: (1: Si, 2: No)
# Salario total si dispone - 1 135-200, 2 201-270, 3 271-340, 4 341-410, 5 Encima 410 (en dólares)
# Transporte a la universidad - 1 Bus, 2 Privado carro/taxi, 3 cicla, 4: Otro
# Vivienda: (1: rentada, 2: dormitorop, 3: Con familia, 4: otro
# Educación de la madre (1: Escuela primaria, 2: Escuela secundaria, 3: Preparatoria, 4: Universidad, 5: Maestria., 6: Doctorado)
# Educación del padre (1: Escuela primaria, 2: Escuela secundaria, 3: Preparatoria, 4: Universidad, 5: Maestria., 6: Doctorado)
# Número de hermanas/hermanos (si dispone) (1: 1, 2:, 2, 3: 3, 4: 4, 5: 5 o más)
# Estado de los padres: (1: Casados, 2: Divorciados, 3: Fallecido - Uno de ellos o ambos) # Está como "KIDS" por un error en el muestreo
# Ocupación de la madre (1: retirada, 2: Ama de casa, 3: Funcionaria del gobierno, 4: Empleada del sector privado, 5: Auto-empleo, 6: otra)
# Ocupación del padre (1: retirado, 2: Amo de casa, 3: Funcionario del gobierno, 4: Empleado del sector privado, 5: Auto-empleo, 6: otra)
# Horas de estudio semanales: (1: NInguna, 2: <5 horas, 3: 6-10 horas, 4: 11-20 horas, 5: más de 20 horas)
# Frecuencia de lectura (Libros no científicos/Revistas): (1: No, 2: Algunas veces, 3: A menudo)
# Frecuencia de lectura (Libros científicos/Revistas): (1: No, 2: Algunas veces, 3: A menudo)
# Asistencia a los seminarios/congresos relacionados con el departamento: (1: Si, 2: No)
# Impacto de sus proyectos/actividades en su éxito: (1: positivo, 2: negativo, 3: neutral)
# Asistencia a clases (1: Siempre, 2: Algunas veces, 3: Nunca)
# Preparación a los exámenes parciales 1: (1: Solo, 2: Con amigos, 3: No aplica)
# Preparación a los exámenes parciales 2: (1: Cerca al examen, 2: Regularmente durante el semestre, 3: Nunca)
# Toma notas en clases (1: nunca, 2: algunas veces, 3: siempre)
# Prestando atención en clase: (1: nunca, 2: algunas veces, 3: siempre)
# La discusión mejora el interés y éxito en el curso: (1: nunca, 2: algunas veces, 3: siempre)
# Flip-classroom: (1: No útil, 2: Útil, 3: No aplica)
# Promedio de calificaciones acumuladas en el último semestre (/4.00): (1: <2.00, 2: 2.00-2.49, 3: 2.50-2.99, 4: 3.00-3.49, 5: Encima de 3.49)
# Promedio de calificaciones acumulado esperado en la graduación (/4.00): (1: <2.00, 2: 2.00-2.49, 3: 2.50-2.99, 4: 3.00-3.49, 5: Encima 3.49)
# Identificación del curso
# Output grade (0: Fail, 1: DD, 2: DC, 3: CC, 4: CB, 5: BB, 6: BA, 7: AA)











