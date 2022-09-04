library(nycflights13)
library(tidyverse)
opcion = "s"
entrada <- function(x){
  vuelos <-  flights

  if(x == 1){
    retraso <- vuelos %>%
      filter(arr_delay <= -2) %>%
      select(year, month, day, arr_delay) %>%
      arrange(arr_delay)
    print(retraso)
    IAH <-  vuelos %>%
      filter(dest == "IAH" | dest == "HOU") %>%
      select(year, month, day, dest) %>%
      arrange(year)
    print(IAH)
  }

  else if(x == 2){

    ordenados <- arrange(vuelos, desc(is.na(arr_delay))) %>%
      select(year, month, day, arr_delay)
    print(ordenados)

    retrasados <-  arrange(flights , desc(arr_delay)) %>%
      select(year, month, day, arr_delay ,flight)
    print(retrasados)

    temprano <- arrange(flights , desc(dep_delay))%>%
      select(year, month, day, arr_delay, flight)
    print(temprano)

    masVeloces1.0 <- arrange(flights, air_time)%>%
      select(year, month, day, air_time, flight)
    print(masVeloces1.0)

    masVeloces1.1 <- arrange(flights, hour)%>%
      select(year, month, day, hour, flight)
    print(masVeloces1.1)

    masCorto <- arrange(flights , distance) %>%
      select(year, month, day, distance, flight)
    print(masCorto)

    masLejos <- arrange(flights , desc(distance)) %>%
      select(year, month, day, distance, flight)
    print(masLejos)
  }

  else if(x ==3){
    trans1 <- transmute(flights,
                        dep_time,
                        hour = dep_time %/% 100,
                        minute = dep_time %% 100,
    )
    print(trans1)
    trans2 <- transmute(flights,
                        sched_dep_time,
                        hour = sched_dep_time %/% 100,
                        minute = sched_dep_time %% 100,
    )
    print(trans2)

  }

  else if(x ==4){
    trans3 <- transmute(flights,
                        air_time = air_time,
                        air_time_new = arr_time - dep_time)
    print(trans3)
  }

  else if (x ==5){
    flights <- flights
    minantes <- filter(flights, arr_delay == -15 | arr_delay == 15)
    print(minantes)

    diezmin <-  filter(flights, arr_delay == 10)
    print(diezmin)

    terinmin <- filter(flights, arr_delay == -30 | arr_delay == 30 )
    print(terinmin)
  }

  else if(x ==6){

    retraso3 <- arrange(flights, desc(dep_delay))
    print(retraso3)

  }else{

    print("Incorrect Opcion")
  }

}
while(opcion == "s"){
  number <- readline(prompt = "ingrese un numero: ")
  number  <- as.numeric(number)   # convert character into integer
  entrada(x = number)
  opcion <- readline("Do you want continue? s/n ")
  if(opcion == "n"){
    print("Bye :)")
  }
}
