#' Visualizing the mean delays of the airports
#'
#' @return plot of the mean delays of different airports by longititude and latitude
#' @export
#' @import dplyr
#' @import ggplot2
#' @import nycflights13
#'
#' @examples
Visualizing_flight_delay<-function(){
  # removing rows having NA values based on columns
  flights<-nycflights13::flights
  airports<-nycflights13::airports
  flights<-flights[!is.na(flights$dep_delay),]
  flights<-flights[!is.na(flights$arr_delay),]
  
  #calculating delays of destination airport
  requireNamespace('dplyr')
  fl_dest<- flights %>% dplyr::group_by(dest) %>%
    dplyr::summarise(avg_delay_dest=mean(arr_delay+dep_delay))
  colnames(fl_dest)<-c('airports','mean_delay')
  fl_dest
  
  #calculating delays of arrival airport
  fl_origin<-flights %>% dplyr::group_by(origin) %>%
    dplyr::summarise(avg_delay_origin=mean(dep_delay))
  colnames(fl_origin)<-c('airports','mean_delay')
  fl_origin
  
  #rbinding all the airports mean delay to create airports_mean_delay dataset
  airports_mean_delay<-rbind(fl_dest,fl_origin)
  airports_mean_delay
  
  #merging airports dataset and airports_mean_delay
  merging<-dplyr::left_join(airports,airports_mean_delay,by=c('faa'='airports'))
  
  merging<-merging[!is.na(merging$mean_delay),]
  colnames(merging)[1]<-c('airport')
  
  
  #hover over the points to see mean delay
  requireNamespace('ggplot2')
  p<-ggplot2::ggplot(merging,ggplot2::aes(label1=mean_delay))+
    ggplot2::geom_point(ggplot2::aes(x=lat,y=lon,color=airport))+
    ggplot2::theme(legend.position = 'none')+xlab('latitude')+ylab('longitude')+
    ggplot2::ggtitle('Mean delay of flights for different airports by longitude and latitude.')
  return(plotly::ggplotly(p))
}
Visualizing_flight_delay()



  
