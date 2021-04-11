## Informe realizado por: Eduardo Morgado y Diego Zelada 









## estas son las liberías que se utilizarán##

library (tidyverse)
library(quanteda)
library(ggplot2)
library(utf8)

## para comenzar, se cargará la base de datos a utilizar de los restaurantes#



datatrabajo= read.csv(file.choose(), header = TRUE, sep = ";")

##hacemos summary para ver un resumen de los datos a utilizar##

summary(datatrabajo)

##posterior a esto, se puede apreciar, que la variable más importante para crear una receta que asegure una buena calificación es la variable notas, dado que, según esta se puntuan las distintas hamburguesas probadas ##

##después, verificamos cuales variables pueden ser más utiles para la realización de dicho informe, por lo que se limpiará la data##

## por lo que vamos a proceder a trabajar con las variables nota, ingredientes, local. Esto debido a que creemos que segun
##el problema que se nos plantea es conveniente trabajar con estas variables ##
##dato que, nota es para la calificación como recién se dijo, ingredientes es para saber que ingredientes son los puntuados y local, para saber el mombre del local que lo produce##
datareal1 = datatrabajo[,-c(1,3,4,7)]


## posterior a esto, eliminaremos los datos nulos para trabajar con una data mas precisa y así no fallar en las inferencias posteriores##

##con la función "sapply" podemos obtener la cantidad de valores NA por columna##
##con la funcion na.omit borramos los valores NA de la base de datos ##
sapply(datareal1, function(x)sum(is.na(x)))

datoslimpios= na.omit(datareal1)

##como podemos ver, ahora tenemos los datos limpios en la data -> datos limpios, sin valores NA en la base de datos##
## luego vamos a ordenar los datos de la nota mayor a la menor##
datosordenados= datoslimpios[order(datoslimpios$nota, decreasing = TRUE),]

## posteriormente, solo utilizaremos los datos que tengan nota 5, y no los con nota inferior, dado que, se desea obtener una buena receta o más bien una receta que asegure una buena calificación. Si vemos la Data, podemos ver que no existen notas mayores a 5 por lo que nos quedaremos solo con los 5##
notas5= filter(datosordenados, nota=="5")

##después que estan seleccionadas las variables y los datos filtrados, pasamos al análisis de los textos, es decir, de las recomendaciones que se obtuvieron después de probar los alimentos#

textoanalisis= notas5$Ingredientes

##enseguida, utilizamos la funcion char_tolower() para dejar todas las letras en minuscula y no tener problemas de como se hata escritura, es decir, unificamos o estandarizamos la escritura##

textoanalisis=char_tolower(textoanalisis)

## luego, convertimos el tipo de archivo

textoanalisis= iconv(textoanalisis, to = "ASCII//TRANSLIT")

##removemos palabras que no nos sirven y obtenemos una matriz que indica los ingredientes que tienen
## las hamburguesas que fueron calificadas  con 5 estrellas
##además removemos los iconos y terminaciones de las palabras, al igual que de como se dijo anteriormente,
##poder estandarizar la escritura o más bien las palabras ingresadas en la base de datos


palabras = dfm(textoanalisis, remove = c((stopwords("es")),("ones"), (","),("?"), ("."),("("),(")"),("!")))

## convertimos un archivo dfm (palabras) en un data frame para poder trabajar en el 
##y así hacer un contador de ingredientes


ingredientescontar= data.frame(palabras)

##como podemos ver, se obtiene una matriz en donde se verifica si el texto ingresado en la base de datos  posee o no dicho ingrediente, por ejemplo, en el caso de los tomtates, se pasa la palabra tomate a una variable llamada "tomate", indicando con un 1 si está presente en el texto y con un 0 si es que no está presente, lo que se demuestra en la matriz de más abajo##


ingredientescontar



## sumamos las columnas(ingredientes) y obtenemos la información de los ingredientes que más se repiten
##con el fin de luego llegar a una receta estandarizada

totalingredientes= colSums(ingredientescontar[, 2:56])
totalingredientes

##ordenamos de acuerdo a la importancia de los ingredientes de mayor a menor
ingredientesordenados = totalingredientes[order(totalingredientes, decreasing = TRUE)]
ingredientesordenados


ing = data.frame(ingredientesordenados)
ing= add_rownames(ing, var= "ingredientes")
ing

## ahora bien visualizamos los ingredientes de acuerdoa  la frecuencia que se presenta
gg= ggplot(data = ing, aes(x=ingredientes, y=ingredientesordenados))

gg + geom_bar(stat = "identity")



## como podemos apreciar, en el ggplot de frecuencia e ingredientes, se presentan muchos ingredientes con baja frecuencia, es decir, que se utilizan poco por los restaurantes, por lo que se procecederá a escoger sólo a los principales 14 ingredientes interesantes para obtener una buena receta

ing1= ing[-c(15:55),]
ing1
## como podemos verificar, se obtienen los ingredientes destacando el ingrediente queso como principal con 33 apariciones, luego el cebolla y mayonesa con 22 y 19 respectivamente, así hasta llegar a la carne de res. Si bien, podemos verificar que lo más importante en este caso son los vegetales, no deja de importar la carne, el rest y la hamburguesa, dado que con esto se elaboran las hamburguesas

##por otra parte, podemos verificar que no sólo se elaboran hamburguesas, sino que lo fuerte son los sandwiches, por lo que, con 14 ingredientes se pueden establecer bastantes configuraciones o mezclas de sandwiches que probablemente sean exitosos



##posterior a ello entrelazamos los puntos para ver la altitud alcanzada en popularidad



ggplot(ing1) + aes(ingredientes, ingredientesordenados) +
  geom_point() + theme(axis.text=element_text(size=12), axis.title=element_text(size=18))


##pero realmente, como se aprecia no es tan estetico visualizarlo así, por lo que más bien 
gg= ggplot(data = ing1, aes(x=ingredientes, y=ingredientesordenados))

gg + geom_bar(stat = "identity")
## ahora bien, como podemos ver  el queso es el ingrediente más relevante destacando por lejos, junto con la cebolla, la mayonesa y el tomate. Estos ingredientes deben estar incluidos si o si para asegurar el éxito, además del pan. 

##por otra parte, es extraño que el pan no sea el más relevente, ya que se trata de sandwiches, pero una razón lógica de este caso, puede ser la omisión de algunos comensales al escribir los comentarios, o bien, no lo tomaron como un ingrediente, sino que muchas veces se obvia dicho asunto.


##para concluir, podemos observar que a través del presente informe, se realizó un trabajo en donde se limpiaron la base de datos quitanto N/a, se crearon variables para así trabajar la data, y se llegó al resultado en donde se puede apreciar los ingredientes más relevantes.

