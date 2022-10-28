#' Visualizing the mean delays of the airports
#'
#' @return plot of the mean delays of different airports by longititude and latitude
#' @export
#' @importFrom dplyr %>%

Visualizing_flight_delay<-function(){
  # removing rows having NA values based on columns
  flights<-nycflights13::flights
  flights<-flights[!is.na(flights$dep_delay),]
  flights<-flights[!is.na(flights$arr_delay),]
  
  #calculating delays of destination airport
  fl_dest<- flights %>% dplyr::group_by(dest) %>%
    summarise(avg_delay_dest=mean(arr_delay+dep_delay),
              .groups = 'drop')
  colnames(fl_dest)<-c('airports','mean_delay')
  fl_dest
  
  #calculating delays of arrival airport
  fl_origin<-flights %>% dplyr::group_by(origin) %>%
    summarise(avg_delay_origin=mean(dep_delay),
              .groups = 'drop')
  colnames(fl_origin)<-c('airports','mean_delay')
  fl_origin
  
  #rbinding all the airports mean delay to create airports_mean_delay dataset
  airports_mean_delay<-rbind(fl_dest,fl_origin)
  airports_mean_delay
  
  #merging airports dataset and airports_mean_delay
  merging<-dplyr::left_join(airports,airports_mean_delay,by=c('faa'='airports'))
  merging
  merging<-merging[!is.na(merging$mean_delay),]
  merging
  colnames(merging)[1]<-c('airport')
  
  
  #hover over the points to see mean delay
  p<-ggplot2::ggplot(merging,aes(label1=mean_delay))+
    geom_point(aes(x=lat,y=lon,color=airport))+
    theme(legend.position = 'none')+xlab('latitude')+ylab('longitude')+
    ggtitle('Mean delay of flights for different airports by longitude and latitude.')
  return(plotly::ggplotly(p))
}
Visualizing_flight_delay()



  
