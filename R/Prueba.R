library(nycflights13)
library(tidyverse)

flights <- flights

retraso <- arrange(flights, desc(dep_delay), tailnum)

retraso2 <- arrange(flights, tailnum) %>%
            group_by(desc(dep_delay))


retraso3 <- arrange(flights, desc(dep_delay)) %>%
  group_by(tailnum)

diezmin <-  filter(flights, arr_delay == 10)

terinmin <- filter(flights, arr_delay == -30 | arr_delay == 30 )


numero <- readline(prompt="Ingrese un numero")


elegir <- function(numero){

  if(numero == 1){

    vuelos <-  flights
    retraso = filter(flights, arr_delay <= -2)

    IAH <-  vuelos %>%
      filter(dest == "IAH" | dest == "HOU") %>%
      select(year, month, day, dest) %>%
      arrange(year)
  }

  else if(numero==2){

    ordenados <- arrange(vuelos, desc(is.na(arr_delay))) %>%
      select(arr_delay)

    retrasados <-  arrange(flights , desc(arr_delay)) %>%
      select(year, month, day, arr_delay ,flight)

    temprano <- arrange(flights , desc(dep_delay))%>%
      select(year, month, day, arr_delay, flight)


    masVeloces1.0 <- arrange(flights, air_time)%>%
      select(year, month, day, air_time, flight)


    masVeloces1.1 <- arrange(flights, hour)%>%
      select(year, month, day, hour, flight)

    masCorto <- arrange(flights , distance) %>%
      select(year, month, day, distance, flight)

    masLejos <- arrange(flights , desc(distance)) %>%
      select(year, month, day, distance, flight)
  }

  else if(numero ==3){
    transmute(flights,
              dep_time,
              hour = dep_time %/% 100,
              minute = dep_time %% 100,
    )
    transmute(flights,
              sched_dep_time,
              hour = sched_dep_time %/% 100,
              minute = sched_dep_time %% 100,
    )

  }

  else if(numero ==4){
    transmute(flights,
              air_time = air_time,
              air_time_new = arr_time - dep_time)

  }

  else if (numero ==5){
    flights <- flights %>%
    minantes <- filter(flights, arr_delay == -15 | arr_delay == 15) %>%
    diezmin <-  filter(flights, arr_delay == 10) %>%
      terinmin <- filter(flights, arr_delay == -30 | arr_delay == 30 )
  }

  else if(numero ==6){
    retraso3 <- arrange(flights, desc(dep_delay))
  }

}




