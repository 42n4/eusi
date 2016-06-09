#########################################################################################################################
#FUZZY LOGIC
#https://www.soa.org/library/newsletters/forecasting-futurism/2014/july/ffn-2014-iss9-heaton-fuzzy.aspx
#http://www.inside-r.org/packages/cran/sets/docs/fuzzy
#http://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/
#https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Fuzzy_Clustering_-_Fuzzy_C-means
library(sets)
# set universe
sets_options("universe", seq(from = 0, to = 25, by = 0.1))

## set up fuzzy variables
variables <-
  set(
    service = fuzzy_partition(
      varnames = c(
        poor = 0,
        good = 5,
        excellent = 10
      ),
      sd = 1.5
    ),
    food = fuzzy_variable(
      rancid = fuzzy_trapezoid(corners = c(-2, 0, 2, 4)),
      delicious = fuzzy_trapezoid(corners = c(7, 9, 11, 13))
    ),
    tip = fuzzy_partition(
      varnames = c(
        cheap = 5,
        average = 12.5,
        generous = 20
      ),
      FUN = fuzzy_cone,
      radius = 5
    )
  )

## set up rules
rules <-
  set(
    fuzzy_rule(service %is% poor || food %is% rancid, tip %is% cheap),
    fuzzy_rule(service %is% good, tip %is% average),
    fuzzy_rule(service %is% excellent ||
                 food %is% delicious, tip %is% generous)
  )

## combine to a system
system <- fuzzy_system(variables, rules)
print(system)
plot(system) ## plots variables

## do inference
fi <- fuzzy_inference(system, list(service = 3, food = 8))

## plot resulting fuzzy set
plot(fi)

## defuzzify
gset_defuzzify(fi, "centroid")

## reset universe
sets_options("universe", NULL)
##########################################################################################################


#IF Oprocentowanie jest bardzo wysokie AND KaraOdejscia jest niska THEN ProcentOdejścia jest wysoki
#IF Oprocentowanie jest niskie AND KaraOdejscia jest średnia THEN ProcentOdejścia jest niski

library(sets)
sets_options('universe', seq(from = 1, to = 9, by = .5))
vars <- set(
  oprocentowanie = fuzzy_partition(varnames = c(
    niskie = 2,
    norm = 4,
    wysokie = 6,
    gigant = 8
  ), sd = 1),
  kara = fuzzy_partition(varnames = c(
    niskie = 3,
    norm = 4,
    wysokie = 5,
    gigant = 6
  ), sd = .8),
  lapse = fuzzy_partition(varnames = c(
    niskie = 3, med = 5, wysokie = 9
  ), sd = 2)
)
rules <- set(
  fuzzy_rule(oprocentowanie %is% niskie && kara %is% niskie, lapse %is% niskie),
  fuzzy_rule((oprocentowanie %is% wysokie ||
                oprocentowanie %is% gigant) && (kara %is% wysokie || kara %is% gigant),
             lapse %is% niskie
  )
)
sys <- fuzzy_system(vars, rules)
plot(sys)
fz_inf <- fuzzy_inference(sys, list(oprocentowanie = 2.5, kara = 3))
plot(fz_inf)
gset_defuzzify(fz_inf, 'centroid')
gset_defuzzify(fz_inf, 'meanofmax')
gset_defuzzify(fz_inf, 'smallestofmax')
gset_defuzzify(fz_inf, 'largestofmax')
fz_inf <- fuzzy_inference(sys, list(oprocentowanie = 7, kara = 5))
plot(fz_inf)
gset_defuzzify(fz_inf, 'centroid')
gset_defuzzify(fz_inf, 'meanofmax')
gset_defuzzify(fz_inf, 'smallestofmax')
gset_defuzzify(fz_inf, 'largestofmax')

##########################################################################################################
library(sets)
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
