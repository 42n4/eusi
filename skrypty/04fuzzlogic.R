#########################################################################################################################
#FUZZY LOGIC
#https://www.soa.org/library/newsletters/forecasting-futurism/2014/july/ffn-2014-iss9-heaton-fuzzy.aspx
#http://www.inside-r.org/packages/cran/sets/docs/fuzzy
#http://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/
#https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Fuzzy_Clustering_-_Fuzzy_C-means
library(sets)
##########################################################################################################
# Fuzzy reguły wpływu oprocentowania i bezrobocia na rezygnację z polisy ubezpieczeniowej
##########################################################################################################
#IF Oprocentowanie jest bardzo wysokie AND Bezrobocie jest niska THEN Rezygnacja jest wysoki
#IF Oprocentowanie jest niskie AND Bezrobocie jest średnia THEN Rezygnacja jest niski

library(sets)
sets_options('universe', seq(from = 1, to = 9, by = .5))
vars <- set(
  oprocentowanie = fuzzy_partition(varnames = c(
    niskie = 2,
    norm = 4,
    wysokie = 6,
    gigant = 8
  ), sd = 1),
  bezrobocie = fuzzy_partition(varnames = c(
    niskie = 3,
    norm = 4,
    wysokie = 5,
    gigant = 6
  ), sd = .8),
  rezygnacja = fuzzy_partition(varnames = c(
    niskie = 3, med = 5, wysokie = 9
  ), sd = 2)
)
rules <- set(
  fuzzy_rule(oprocentowanie %is% niskie && bezrobocie %is% niskie, rezygnacja %is% niskie),
  fuzzy_rule((oprocentowanie %is% wysokie ||
                oprocentowanie %is% gigant) && (bezrobocie %is% wysokie || bezrobocie %is% gigant),
             rezygnacja %is% niskie
  )
)
sys <- fuzzy_system(vars, rules)
plot(sys)
fz_inf <- fuzzy_inference(sys, list(oprocentowanie = 2.5, bezrobocie = 3))
plot(fz_inf)
#defuzzyfikacja to odpowiedź (szacowana wartość rezygnacji od najmniejszej do największej poprzez średnią)
#na zadane oprocentowanie i bezrobocie
gset_defuzzify(fz_inf, 'centroid')
gset_defuzzify(fz_inf, 'meanofmax')
gset_defuzzify(fz_inf, 'smallestofmax')
gset_defuzzify(fz_inf, 'largestofmax')
fz_inf <- fuzzy_inference(sys, list(oprocentowanie = 7, bezrobocie = 5))
plot(fz_inf)
gset_defuzzify(fz_inf, 'centroid')
gset_defuzzify(fz_inf, 'meanofmax')
gset_defuzzify(fz_inf, 'smallestofmax')
gset_defuzzify(fz_inf, 'largestofmax')
## resetuj domenę
sets_options("universe", NULL)


##########################################################################################################
# Fuzzy reguły wpływu jakości jedzenia i obsługi na wysokość napiwku
##########################################################################################################

# ustaw domenę
sets_options("universe", seq(from = 0, to = 25, by = 0.1))

## ustaw zmnienne fuzzy jedzenie w formie trapezów, usługa rozkładów normalnych, napiwek trójkątów
variables <-
  set(
    usluga = fuzzy_partition(
      varnames = c(
        kiepska = 0,
        dobra = 5,
        wybitna = 10
      ),
      sd = 1.5
    ),
    jedzenie = fuzzy_variable(
      zepsute = fuzzy_trapezoid(corners = c(-2, 0, 2, 4)),
      smaczne = fuzzy_trapezoid(corners = c(7, 9, 11, 13))
    ),
    napiwek = fuzzy_partition(
      varnames = c(
        mały = 5,
        przecietny = 12.5,
        wysoki = 20
      ),
      FUN = fuzzy_cone,
      radius = 5
    )
  )

## reguły
rules <-
  set(
    fuzzy_rule(usluga %is% kiepska || jedzenie %is% zepsute, napiwek %is% mały),
    fuzzy_rule(usluga %is% dobra, napiwek %is% przecietny),
    fuzzy_rule(usluga %is% wybitna || jedzenie %is% smaczne, napiwek %is% wysoki)
  )

## uruchom i wypisz system ekspertowy
system <- fuzzy_system(variables, rules)
print(system)
plot(system) ## wykres zmiennych

## odpal regułę ze zmiennymi z zakresu universe
fi <- fuzzy_inference(system, list(usluga = 3, jedzenie = 8))

## wynikowy zbiór fuzzy określający za pomocą defuzzify zakres wynikowej zmiennej będącej w konkluzji reguły
plot(fi)

## defuzzify
gset_defuzzify(fi, "centroid")

## resetuj universe
sets_options("universe", NULL)


##########################################################################################################
# Fuzzy reguły wpływu jakości codu, pasma, przyspieszenia na wykonanie
##########################################################################################################
U1 <- seq(from = 0, to = 1, by = 0.0001)
#DIMENSIONS
variables <-
  set(
    code =
      fuzzy_partition(
        varnames =
          c(
            ELow = 0.2 ,
            ENormal = 0.5,
            EHigh = 0.8
          ),
        FUN = fuzzy_cone,
        radius = 0.2,
        universe = U1
      ),
    bandwidth =
      fuzzy_partition(
        varnames =
          c(
            SLow = 0.2,
            SNormal = 0.5,
            SHigh = 0.8
          ),
        FUN = fuzzy_cone,
        radius = 0.2,
        universe = U1
      ),
    acceleration =
      fuzzy_partition(
        varnames =
          c(
            ALow = 0.2,
            ANormal = 0.5,
            AHigh = 0.8
          ),
        FUN = fuzzy_cone,
        radius = 0.2,
        universe = U1
      ),
    processing =
      fuzzy_partition(
        varnames =
          c(Local = 0.3, Remote = 0.7),
        FUN = fuzzy_cone,
        radius = 0.3,
        universe = U1
      )
  )

rules <-
  set(
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SHigh &&
        acceleration %is% AHigh,
      processing %is% Remote
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SHigh &&
        acceleration %is% ANormal,
      processing %is% Remote
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SHigh &&
        acceleration %is% ALow,
      processing %is% Local
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SNormal &&
        acceleration %is% AHigh,
      processing %is% Remote
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SNormal &&
        acceleration %is% ANormal,
      processing %is% Remote
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SNormal &&
        acceleration %is% ALow,
      processing %is% Local
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SLow &&
        acceleration %is% AHigh,
      processing %is% Remote
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SLow &&
        acceleration %is% ANormal,
      processing %is% Local
    ),
    fuzzy_rule(
      code %is% EHigh &&
        bandwidth %is% SLow &&
        acceleration %is% ALow,
      processing %is% Local
    ),
    fuzzy_rule(code %is% EHigh &&
                 bandwidth %is% SHigh, processing %is% Remote),
    fuzzy_rule(code %is% EHigh &&
                 bandwidth %is% SLow, processing %is% Local),
    fuzzy_rule(code %is% EHigh &&
                 bandwidth %is% SNormal, processing %is% Local),
    fuzzy_rule(code %is% ENormal &&
                 bandwidth %is% SLow, processing %is% Local),
    fuzzy_rule(code %is% ENormal &&
                 bandwidth %is% SHigh, processing %is% Remote),
    fuzzy_rule(code %is% ENormal &&
                 bandwidth %is% SNormal, processing %is% Local),
    fuzzy_rule(code %is% ELow &&
                 bandwidth %is% SLow, processing %is% Local),
    fuzzy_rule(code %is% ELow &&
                 bandwidth %is% SNormal, processing %is% Local),
    fuzzy_rule(code %is% ELow &&
                 bandwidth %is% SHigh, processing %is% Local)
  )


context <- fuzzy_system(variables, rules)
print(context)
plot(context)

#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912))
#0.7
#fi <- fuzzy_inference(context, list(code=0.5212, bandwidth=0.2121))
#0.3
#fi <- fuzzy_inference(context, list(code=0.1532, bandwidth=0.9321))
#0.3
#fi <- fuzzy_inference(context, list(code=0.2432, bandwidth=0.9323))
#0.3
#fi <- fuzzy_inference(context, list(code=0.4723, bandwidth=0.9542))
#0.7
#fi <- fuzzy_inference(context, list(code=0.2932, bandwidth=0.9484))
#0.3
#fi <- fuzzy_inference(context, list(code=0.2912, bandwidth=0.4324))
#0.3
#fi <- fuzzy_inference(context, list(code=0.2134, bandwidth=0.2194))
#0.3
#fi <- fuzzy_inference(context, list(code=0.4323, bandwidth=0.5345))
#0.3
#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912, acceleration = NA))
#There is a rule code is High AND bandwidth is High -> Remote.
#as well as a rule, code is High AND bandwidth is High and acceleration is High -> Remote.
#0.7
#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912, acceleration = 0.93))
#0.7
#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912, acceleration = 0.13))
#0.5
#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.2354, acceleration = 0.9243))
#0.4517523
#fi <- fuzzy_inference(context, list(code=0.4723, bandwidth=0.9542, acceleration=NA))
#This rule exists for the two variables provided
#0.7
#fi <- fuzzy_inference(context, list(code=0.8857, bandwidth=0.1194, acceleration=0.8921))
#0.4961897

fi <-
  fuzzy_inference(context,
                  list(
                    code = 0.8857,
                    bandwidth = 0.1194,
                    acceleration = 0.1184
                  ))
#0.3

#dev.new()
plot(fi)
gset_defuzzify(fi, "centroid")
U1 <- NULL
