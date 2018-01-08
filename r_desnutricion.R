library(sets)


sets_options("universe", seq(1, 20, 0.1))
variables <- set(
  peso = fuzzy_partition(varnames = c(extremad.bajo =10, bajo = 15, normal =20),  sd = 1.5)
  
)
sets_options("universe", seq(80, 120, 0.1))
variables2 <- set(
  talla = fuzzy_partition(varnames = c(bajo = 80, medio = 90,alto = 100), sd =3.0)
)

sets_options("universe", seq(1, 10, 0.1))
variables3 <- set(
  escazes_alimentaria= fuzzy_partition(varnames = c(minima = 5, elevada = 7.5),sd=1.0),
  enfermizo = fuzzy_partition(varnames = c(nofrecuente=5,frecuente=7.5),sd = 1.0),
  palidez= fuzzy_partition(varnames = c(localizada = 5, generalizada = 7.5),sd=1.0),
  edad = fuzzy_partition(varnames = c(infante =3, niñez.temprana =5, niñez.intermedia = 7),sd = 1.0),
  
  desnutricion = fuzzy_partition(varnames = c(leve=3,moderada=5,grave=7),sd = 1.0)
)



reglas <- set(
  fuzzy_rule(peso %is% extremad.bajo && edad %is% niñez.intermedia&& talla%is% medio, desnutricion%is%moderada ),
  fuzzy_rule(peso %is% extremad.bajo && edad %is% niñez.intermedia && talla%is% alto, desnutricion %is% grave),
  
  fuzzy_rule(escazes_alimentaria %is% elevada && enfermizo %is% frecuente && palidez%is% generalizada, desnutricion %is% grave),
  fuzzy_rule(escazes_alimentaria %is% minima && enfermizo %is% nofrecuente && palidez%is% localizada, desnutricion %is% leve),
  fuzzy_rule(escazes_alimentaria %is% minima && enfermizo %is% frecuente && palidez%is% localizada, desnutricion %is% moderada)
  
  
)
modelo<-fuzzy_system(variables3, reglas)

print(modelo)

plot(modelo)

ejemplo<-fuzzy_inference(modelo, list(escazes_alimentaria =5, enfermizo = 10, palidez = 9))
gset_defuzzify(ejemplo, "centroid")
plot(ejemplo)



