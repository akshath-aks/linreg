#' Title
#'
#' @field formula formula. 
#' @field data data.frame. 
#' @field B_h vector. 
#' @field y_h matrix. 
#' @field e matrix. 
#' @field df numeric. 
#' @field e_var matrix. 
#' @field B_h_var vector. 
#' @field t_value matrix. 
#' @field data_name character. 
#'
#' @return
#' @export
#' @import ggplot2
#' @import gridExtra
#' 
#' @examples
linreg<-setRefClass('linreg',fields=list(formula='formula',
                                         data ='data.frame',
                                         B_h='vector',
                                         y_h='matrix',
                                         e='matrix',
                                         df='numeric',
                                         e_var='matrix',
                                         B_h_var='vector',
                                         t_value='matrix',
                                         data_name='character'),
methods=list(
  initialize=function(formula=formula(),data=data.frame()){
    .self$formula<<-formula
    .self$data<<-data
    
    data_name_a<-deparse(substitute(data))
    
    X<-model.matrix(formula,data)
    y<-as.matrix(data[,all.vars(formula)[1]])
    B_h_a<-round(solve(t(X)%*%X)%*%(t(X)%*% y),2)
    
    y_h_a<-X%*%B_h_a
    e_a<-y-y_h_a
    df_a<-nrow(X)-nrow(B_h_a)
    e_var_a<-(t(e_a)%*%e_a)/df_a
    B_h_var_a<-c(e_var_a)*diag(solve(t(X)%*%X))
    t_value_a<-B_h_a/sqrt(B_h_var_a)
    
    B_h_a<-as.vector(B_h_a)
    names(B_h_a)<-colnames(X)
    
    .self$B_h<<-B_h_a
    .self$y_h<<-y_h_a
    .self$e<<-e_a
    .self$df<<-df_a
    .self$e_var<<-e_var_a
    .self$B_h_var<<-B_h_var_a
    .self$t_value<<-t_value_a
    .self$data_name<-data_name_a
  },
  
  resid=function(){return(c(e))},
  
  pred=function(){return(y_h)},
  
  coef=function(){return(B_h)},
  
  print=function(){
    cat('call:')
    cat(sep='\n')
    cat(paste('linreg(formula = ',format(formula),', ' ,'data = ',data_name ,')\n\n',sep=''))
    cat('Coffiecients:\n')
    print.default(format(B_h),
                  print.gap = 2L,quote=FALSE)},
  
  plot=function(){
    library(ggplot2)
    library(gridExtra)
    plot1=ggplot(data.frame(e,y_h),aes(y=e,x=y_h))+
      geom_point(size=2.5,shape=1)+
      geom_smooth(formula = y ~ x, method = 'lm',linetype='dotted',se=FALSE)+
      ggtitle('Residuals vs Fitted')+
      xlab(paste('Fitted values\n','linreg(',format(formula),')',''))+
      ylab('Residuals')+
      stat_summary(aes(y=e,x=y_h,group=1),
                   fun=median,colour='red',geom='line',group=1)+
      theme(plot.title=element_text(hjust=0.5),panel.background = element_rect(fill='white',color = 'black'))
    
    stand_e<-sqrt(abs((e-mean(e))/sd(e)))
    plot2=ggplot(data.frame(stand_e,y_h),aes(y=stand_e,x=y_h,group=1))+
      geom_point(size=3,shape=1)+
      ggtitle('Scale−Location')+
      xlab(paste('Fitted values\n','linreg(',format(formula),')',''))+
      ylab(expression(sqrt('|Standardized Residual|')))+
      theme(plot.title=element_text(hjust=0.5),panel.background = element_rect(fill='white',color = 'black'))+
      stat_summary(aes(y=stand_e,x=y_h,group=1),
                   fun=mean,colour='red',geom='line',group=1)
    
    grid.arrange(plot1,plot2,ncol=2)
    
  },
  
  summary=function(){
    p_value <- numeric(length(t_value))
    for (i in 1:length(t_value)) {
      if (t_value[i]<0) p_value[i] <- 2 * pt(t_value[i], df)
      else p_value[i] <- 2 * pt(-t_value[i], df)
    ('\n')}
    cat('call:')
    cat(sep='\n')
    cat(paste('linreg(formula=',format(formula),',' ,'data=',data_name ,')\n\n',sep=''))
    cat('Coffiecients:\n')
    col_name<-c('Std. Error', 't value', 'Pr(>|t|)')
    for(i in 1:length(col_name)) cat(sprintf('%*s', 20, col_name[i]))
    cat
    for(i in 1:length(B_h)) {
      cat(sprintf('%-*s', 20, names(B_h)[i]))
      cat(sprintf('%6.6f\t%6.6f\t%e\n',
                  sqrt(B_h_var)[i], t_value[i], p_value[i]  ))
    }
    cat(paste('\nResidual standard error: ', sprintf('%.4f', sqrt(e_var)),
              ' on ', df, ' degrees of freedom', sep='', '\n'))
  }
))

linreg_obj<-linreg$new(formula=Petal.Length ~ Species,data=iris)
linreg_obj$formula
linreg_obj$t_value
linreg_obj$resid()
linreg_obj$pred()
linreg_obj$coef()
linreg_obj$print()
linreg_obj$plot()
linreg_obj$summary()

#data(iris)
#summary(lm(Petal.Length~Species, data = iris))