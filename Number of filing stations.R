#Here is a simple Model to estimate the Number of Filling Stations my country Zambia.

#As a starting point I know the follwing SIMPLE parameters(ASSUMPTIONS) with a VERY  high degree of confidence



#I live in kabwe,a big town in the country of Zambia
#I know that my town had a population of 200000 in 2010 census
#I know that the population of Zambia at last census (2010) was 13 million
#I know that the population of Zambia today is 19 million
#As a motorist in kabwe I know that the town has JUST 7 filling stations
#As an actuarial analyst who has with expereince in mortality investigations am very confident in these population assumptions


# I have writen a function that will make it easy for me to quickly estimate number of filling stations called stationEstimator
# it takes two arguments with defualt value set to best estimates as indicated below
#Uncertainity in the model arises from these two parameters
# Read the body of the function for details how these two parameters or arguments arise..


stationsEstimator <- function(city_adjustment_factor=1.25,rural_adjustment_factor=.05){





#Storing the above assumptions in variables.....
number_of_fillingstations_kabwe <- 7
kabwe_population_2010  <- 200000
zambia_population_2010 <- 13000000
zambia_population_2021<- 19000000

#Census 2020 was not conducted so i dont know exaclty what my towns population is currently,but I can adjust what i know from 2010 census
#by a the growth factor in the national population since 2010. Am assumming that kabwe should have a growth factor similar to national growth rate
#although it may not be exact it is an execellent estimator
#therefore

kabwe_population_2021 <- zambia_population_2021/zambia_population_2010*kabwe_population_2010 

#Assumptions on the distribution of population in zambia

#The population in Zambia is spread across urban and rural
#With Urban in two main buckets ,city and town
#Therefore Zambia population = city population +  town population + rural population
#city population is spread over just lusaka + copperbelt(kitwe and ndola)
#I know that lusaka has a population of just over 3 million and copperbelt(rich mining province) has a population just over 2 million 
# The rest of the urban populations is in big towns
#So we can see that near 6 million people live in just the three biggest cities. 
#I know that 60% of the country population is rural(typical of subsahara countries).It has dropped from near 70% a decade ago.A metric I follow
#as am very interested in developemt and ending poverty
# I know the population in the three biggest cities in Zambia

#Mathematically we summarise these assumptions as below


lusaka_population <- 3100000
ndola_population <- 500000
kitwe_population <- 1000000
copperbelt_population <- ndola_population+kitwe_population  

#copperbelt city not province population although is very heavily weighted in these two cities

#because the  city population just across lusaka and copperbelt(ndola,kitwe)...the only three biggest cities with highest population densities
city_population <- lusaka_population + copperbelt_population

#to recap we have defined a basic probability mass function that has three main buckets for the countries population
#people live in either a rural area,a twon or a city

#I dont know how many people live in towns totally but we can calcualte that easily since we know the total country population
#we also know the city population 
#we also know that split of population in Zambia between urban and rural 
# Zambia population= urban population + rural....(40/60) split
#.....where urban population = city population + town population
#
#.......Zambia population= city population + town population + rural
#....From the (40/60) we know that the urban population must be Zambia population*.40

town_population <- zambia_population_2021 * .4-city_population

# we calcualte total rural population easily based on what we know about split between about and rural in my country.......

rural_population <- zambia_population_2021 * .6


#I know that my town has 7 filing stations as a motorist
# I have kabwe population and kabwe numbero of filling stations which I can use as 
# estimator for expected number of stations for towns.


#general equation for number of filling stations in ZAMBIA can be calculted as
# = average number of stations per 100000 *zambia population / 100000 ###in short number of stations is propertinal to population...


kabwe_average_per_100000 <- number_of_fillingstations_kabwe / kabwe_population_2021*100000


# we also assume that average number of stations per 100000 people in kabwe is good estimate for other towns as a typical  town

town_avearge_per_100000 <- kabwe_average_per_100000

#Know charecteristics of my town KABWE
#Kabwe is someehere between a city and rural but much closer to a city in charecteristics
# Has three major shopping mall..ONE top 3 university..two other top 10 universities..Largest Zinc mines..lead mines..commercial farm blocks

# estimating the number of stations per 100000 iN rural area
# Rural areas from personal experience has very very low population densities very  low energy demand due to under delopment

# assumming that rural areas are at only 5% to 10% energy demand compared to kabwe is a very reasonable estimator
#since kabwe is closer to city than rural
#  this lead us to the follwing estimate for rural stations density..



#rural_adjustment_factor <- .05  (our input in function) PLEASE DONT UNCOMMENT BY MISTAKE AS FUNCTION WILL GIVE FIXED OUPUT

rural_avearge_per_100000 <- rural_adjustment_factor * kabwe_average_per_100000
# there is very huge difference in urban rural devlopment hence the 5% TO 10% 
#The lower bound is 5% and upper bound is 10%..rural areas i travelled to can have just one to 2 stations for entire districts..
#Very very far from town and  city 



#I dont have  sample data to estimate the number of stations per 100 000 people  for cities but using information about kabwe I can
#estimate reasoanable ranges
# We know that kabwe is much closer to city and very far away from rural therefore 
#city could be in the range 10% to 30% multiple of kabwe..
# a factor intuitively corresponding to my perceived differences in devlopment hence energy demand difference between cities and town..

#city_adjustment_factor <- 1.2 as exmaple  (our input in function)


city_avearge_per_100000 <- city_adjustment_factor * kabwe_average_per_100000


#USING CREDIBILITY THOERY  we can calcacualte the country average by weighting population in  city ,town and rural 
#filling station averages according to population in each population bucket.# we shall call thse weighting factors credibility factor
#that is to say how credible is each as an estimate for country average.
# credibility of each population bucket estimator as an estimate for country average stations per 100 000 people
# remember we have our population distribution in three buckets,city,town and rural..


rural_credibility_factor  <- rural_population / zambia_population_2021
town_credibility_factor   <- town_population / zambia_population_2021
city_credibility_factor   <- city_population / zambia_population_2021

country_average_per_100000_popultion <- rural_avearge_per_100000 * rural_credibility_factor +
                                         town_avearge_per_100000 * town_credibility_factor +
                                         city_avearge_per_100000 * city_credibility_factor
  

country_number_of_filling_stations <- round(country_average_per_100000_popultion  * zambia_population_2021 / 100000, digits = 0)


return(paste("The number of filling stations in Zambia is estimated to be" ,as.character(country_number_of_filling_stations)))



# Assumptions for spread of our estimate             


##when we run this code the model esimates that there are 223 stations

##SENSITIVITY ANALYSIS can help us get a feel of how our results varis with changes in our assumption..



# What would be the range that gives me the highest probability for the number of stations being correct?

# To answer this question we can set city_adjustment_factor equal to kabwes meaning its lowest possible
# because we know that although close it ***CAN'T***  be less than kabwe stations per 1000000 as city has more demand

# so when city_adjustment_factor=1 (kabwe stations per 100000 = city stations per 100000)
#must be lowest number of stations..we also set lowest bound for rural credibility at 5% 


# we get lower bound.......... "Number of filling stations in Zambia is estiamted at 196"..........



# we can estimate the up bound for our model out when city credibility and rural credibility are at upper bounds
#thatis rural_adjustment_factor = 10% and city_adjustment_factor = 2 Which is a very generous estimate as its almost unlikely to be that far away

#...giving us..........maximum station count .."Number of filling stations in Zambia is estiamted at 319"




# Therefore am very confident that the number of filling stations in Zambia ranges from 196 to 319
#...With a center of 223( our best point estimator)......




## You can call the function ****stationsEstimator()***** with inputs you can play around to see how our estimate vary
##as we guess the credibility factor..


## I dont have GDP figures for city ,town and rural but i think i can be good estimator for these credibility factors
}



