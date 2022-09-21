#' Linreg Class calculates Multiple Linear Regression Model For Data frame.
#'
#' @field formula Dependent and independent columns in data frame
#' @field data Data frame for linear models
#' @field B_h Estimators matrix converted to vector
#' @field y_h (matrix). Estimated dependent values
#' @field e (matrix) Error calculated from actual minus calculated dependent varaiable
#' @field df degrees of freedom
#' @field e_var (matrix) Residual variance
#' @field B_h_var variance of the regression coefficients
#' @field t_value (matrix) T value
#' @field data_name (character) Data Frame name
#'
#' @return 
#' @exportClass linreg
#' @export linreg
#' @import ggplot2
#' @import gridExtra
#' @import methods
 

linreg<-setRefClass('linreg',fields=list(formula='formula',
                                         data ='data.frame',
                                         B_h='vector',
                                         y_h='matrix',
                                         e='matrix',
                                         df='numeric',
                                         e_var='matrix',
                                         B_h_var='vector',
                                         t_value='matrix',
                                         data_name='character'),#creating class and adding all the attributes used inside field list.
                    
#adding the methods list, first initiliaze function with formula,data.

methods=list(
  initialize=function(formula=formula(),data=data.frame()){
    
    .self$formula<<-formula # assigning the formula and data to attributes
    .self$data<<-data
    
    data_name_a<-deparse(substitute(data)) # getting data.frame name
    
    #creating independent variable matrix
    X<-model.matrix(formula,data) 
    
    #getting dependent variable matrix
    y<-as.matrix(data[,all.vars(formula)[1]]) 
    
    #calculating estimator coefficients.
    B_h_a<-round(solve(t(X)%*%X)%*%(t(X)%*% y),3)
    
    #calculating predicted y values, residuals(error),degree of freedom,
    #residual variance, variance of the regression coefficients and t value
    y_h_a<-X%*%B_h_a
    e_a<-y-y_h_a
    df_a<-nrow(X)-nrow(B_h_a)
    e_var_a<-(t(e_a)%*%e_a)/df_a
    B_h_var_a<-c(e_var_a)*diag(solve(t(X)%*%X))
    t_value_a<-B_h_a/sqrt(B_h_var_a)
    
    #converting estimators(B_h) from matrix to vector
    B_h_a<-as.vector(B_h_a)
    names(B_h_a)<-colnames(X)# adding names to estimators(B_h) vector
    
    #assignment to attributes
    .self$B_h<<-B_h_a
    .self$y_h<<-y_h_a
    .self$e<<-e_a
    .self$df<<-df_a
    .self$e_var<<-e_var_a
    .self$B_h_var<<-B_h_var_a
    .self$t_value<<-t_value_a
    .self$data_name<-data_name_a
  },
  
  #creating a function to return residuals
  resid=function(){return(c(e))},
  
  #getting predicted values
  pred=function(){return(y_h)},
  
  #returns coefficients(estimators)
  coef=function(){return(B_h)},
  
  #overwriting print function to get the output same as lm output.
  print=function(){
    cat('call:')
    cat(sep='\n')
    cat(paste('linreg(formula = ',format(formula),', ' ,'data = ',data_name ,')\n\n',sep=''))
    cat('Coffiecients:\n')
    print.default(format(B_h),
                  print.gap = 2L,quote=FALSE)},
  
   
  plot=function(){
    #install and load ggplot2.gridExtra
    library(ggplot2)
    library(gridExtra)
    
    #creating plots for residual Vs fitted value
    
    plot1=ggplot(data.frame(e,y_h),aes(y=e,x=y_h))+
      geom_point(size=2.5,shape=1)+
      geom_smooth( method = 'lm',linetype='dotted',se=FALSE)+
      ggtitle('Residuals vs Fitted')+
      xlab(paste('Fitted values\n','linreg(',format(formula),')',''))+
      ylab('Residuals')+
      stat_summary(aes(y=e,x=y_h,group=1),
                   fun=median,colour='red',geom='line',group=1)+
      theme(plot.title=element_text(hjust=0.5),panel.background = element_rect(fill='white',color = 'black'))
    
    #creating plot for fitted value vs sqrt of standardized residuals
    
    stand_e<-sqrt(abs((e-mean(e))/sd(e)))
    plot2=ggplot(data.frame(stand_e,y_h),aes(y=stand_e,x=y_h,group=1))+
      geom_point(size=3,shape=1)+
      ggtitle('Scale Location')+
      xlab(paste('Fitted values\n','linreg(',format(formula),')',''))+
      ylab(expression(sqrt('|Standardized Residual|')))+
      theme(plot.title=element_text(hjust=0.5),panel.background = element_rect(fill='white',color = 'black'))+
      stat_summary(aes(y=stand_e,x=y_h,group=1),
                   fun=mean,colour='red',geom='line',group=1)
    
    grid.arrange(plot1,plot2,ncol=2)
    
  },
  
  #summary returns matrix having estimate, standard error, t value, p value
  # and also returns residual standard error and degree of freedom
  
  summary=function(){
    n<-length(e)
    k<-length(B_h)
    resid_stand_error<-round(sqrt(sum(e**2)/(n-(k+1))),4)
    cat('call:')
    cat(sep='\n')
    cat(paste('linreg(formula = ',format(formula),', ' ,'data = ',data_name ,')\n\n',sep=''))
    cat('Coffiecients:\n')
    coefmatrix<-matrix(B_h)
    
    standard_error<-round(sqrt(B_h_var),5)
    p_value<-2*pt(abs(t_value),df=as.numeric(df),lower.tail = FALSE)
    stars_dashes<-p_value
    stars_dashes[stars_dashes>0 & stars_dashes<0.001]<- '***'
    stars_dashes[stars_dashes>0.001 & stars_dashes<0.01]<- '**'
    stars_dashes[stars_dashes>0.01 & stars_dashes<0.05]<- '*'
    stars_dashes[stars_dashes>0.05 & stars_dashes<0.1]<- '.'
    stars_dashes[stars_dashes>0.1 & stars_dashes<1]<- ''
    coefmatrix<-cbind(coefmatrix,standard_error,round(t_value,2),p_value,stars_dashes)
    colnames(coefmatrix)<-c('Estimate','Std.Error','t value','Pr(>|t|)','')
    print.default(coefmatrix,quote=FALSE)
    cat('\nResidual standard error:',format(resid_stand_error),'on',
        df,'degrees of freedom')
  }
))

# creating and initializing the object
linreg_obj<-linreg$new(formula=Petal.Length ~ Species,data=iris)
linreg_obj$formula
linreg_obj$t_value
linreg_obj$resid()
linreg_obj$pred()
linreg_obj$coef()
linreg_obj$print()
linreg_obj$plot()
linreg_obj$summary()
linreg_obj$B_h_var

