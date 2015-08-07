library('shiny')
library('foreign')
library('rms')
library('ggplot2')

# Load data in SPSS format 
training = read.spss("AFT_All.sav", to.data.frame = TRUE)

# Create SURV objects
surv <- Surv(training$SURVIVAL,training$CENS) 

# Decimate data
training = training[c(6,11,30,32,46)];

# ----------------- Generate Data for EOR vs Surv Prediction Graph ---------------#
fits <- survreg(surv~ as.factor(XRT) + as.factor(TMZ) + AGE + LNKPS + ENHANCEMENT_EOR, data = training, dist="loglogistic")
fit  <- psm(surv~ as.factor(XRT) + as.factor(TMZ) + AGE + LNKPS + ENHANCEMENT_EOR, data = training, dist="loglogistic", x=TRUE, y=TRUE)


# Make a big vector for sampling
n=1000;
shapevec = 0*(1:n);

# Define helper functions for building patients
getPatient = function(thisAge, thisKps, thisXrt, thisTmz, thisEor) {
  patient = list();
  patient$demo = data.frame( AGE=thisAge, KPS=thisKps, XRT=thisXrt, TMZ=thisTmz, ENHANCEMENT_EOR=thisEor );
  patient$demo$LNKPS = log(patient$demo$KPS);
  patient$surv = apply(patient$demo, 1, function(x) survest(fit, newdata=x, time=7*(1:521)));
  
  return(patient);

}

getEorPatient = function(thisAge, thisLnkps, thisXrt, thisTmz, bio) {
  thisEor  = ((1-bio):(n-bio))/(n-bio);
  patient  = data.frame(AGE=thisAge, LNKPS=thisLnkps, XRT=thisXrt, TMZ=thisTmz, ENHANCEMENT_EOR=thisEor);
  pred_raw = predict(fits, newdata=patient, se.fit=T);
  pred     = data.frame(fit=pred_raw$fit, se.fit=pred_raw$se.fit, EOR=thisEor);
                      
  return(pred);  
}

# Define cost function; returns a vector, accepts vector days, eor (same length)
costFunction <- function(days, eor, xrt, tmz){
  # Pro-rate initial treatment
  init_surv = days/(6*7)  # 6-week treatment
  init_surv[init_surv>1]=1;
  
  # Next 30 days?
  firstmonth=0*days;
  firstmonth[days> 6*7] = 1;
  
  # Total months after initial + first?
  aftermonths = floor( (days - 6*7 - 30)/30 )+1;
  aftermonths[aftermonths<0]=0;
  
  cost = 0*days;
  cost[eor == 0] = 15087; # cost of biopsy
  cost[eor > 0 & eor < 97] = 23382; # cost of incomplete resection
  cost[eor >= 97] = 24606; # cost of complete resection
  
  # Add steroids
  cost = cost + 40*floor(days/30) + 40;
  
  # Either or both chemo/radio
  cost = cost + 13098*xrt*init_surv;  # cost of radiotherapy, pro-rated
  cost = cost + 14835*tmz*init_surv;  # cost of initial chemo, pro-rated
  cost = cost + 3155*tmz*firstmonth;  # cost of next 30 days of chemo
  cost = cost + 4416*tmz*aftermonths; # cost of subsequent chemo
  
  return(cost);
}


# Define helper functions for simulating patient populations


shinyServer(function(input, output) {
  
  output$stackoutput = renderUI({
    if( is.null( pop1() ) ) {
      return(numericInput("thisPat", "Patient selector", value=1, min=1, max=1, step=1));
    } else {
      return(numericInput("thisPat", "Patient selector", value=1, min=1, max=length( pop1()$surv ), step=1))
    }
    
  });
  
  # Build the parameters for the patient population
  pop1_age = reactive({
    if( input$ageDist1 == "Normal" ) {
      agePop = rnorm(input$n, input$ageNormalMean1, input$ageNormalSD1 );
      
    } else if( input$ageDist1 == "Uniform" ) {
      agePop = runif(input$n, input$ageUniformMin, input$ageUniformMax );
      
    } else if( input$ageDist1 == "lognormal" ) {
      agePop = rlnorm(input$n, input$ageLognormalMean1, input$ageLognormalSD1);
#      agePop=rep(0,input$n);
      
    } else if( input$ageDist1 == "Exponential" ) {
      agePop = rexp(input$n, input$ageExponentialRate1);
    }
    
    return( sample(agePop) );
  });
  
  pop1_kps = reactive({
      if( input$kpsDist1 == "Normal" ) {
      kpsPop = rnorm(input$n, input$kpsNormalMean1, input$kpsNormalSD1 );
      
    } else if( input$kpsDist1 == "Uniform" ) {
      kpsPop = runif(input$n, input$kpsUniformMin, input$kpsUniformMax );
      
    } else if( input$kpsDist1 == "lognormal" ) {
      kpsPop = rlnorm(input$n, input$kpsLognormalMean1, input$kpsLognormalSD1 );
      
    } else if( input$kpsDist1 == "Exponential" ) {
      kpsPop = rexp(input$n, input$kpsExponentialRate1);
    }
    
    kpsPop[ kpsPop< 0   ] = 0;
    kpsPop[ kpsPop> 100 ] = 100;
    
    return( sample(kpsPop) );
  });

  pop1_eor = reactive({
      if( input$eorDist1 == "Normal" ) {
      eorPop = rnorm(input$n, input$eorNormalMean1, input$eorNormalSD1 );
      
    } else if( input$eorDist1 == "Uniform" ) {
      eorPop = runif(input$n, input$eorUniformMin, input$eorUniformMax );
      
    } else if( input$eorDist1 == "lognormal" ) {
      eorPop = rlnorm(input$n, input$eorLognormalMean1, input$eorLognormalSD1 );
      
    } else if( input$eorDist1 == "Exponential" ) {
      eorPop = rexp(input$n, input$eorExponentialRate1);
    }
    
    eorPop[ eorPop < 0 ] = 0;
    eorPop[ eorPop > 1 ] = 1;
    
    return( sample(eorPop) );
  });

  pop1_xrt = reactive({ 
    xrtPop = rep(0, input$n);
    xrtPop[ 1:floor(input$xrtPerc1*input$n) ] = 1;
    
    return( sample(xrtPop) );
  });
  
  pop1_tmz = reactive({ 
    tmzPop = rep(0, input$n);
    tmzPop[ 1:floor(input$tmzPerc1*input$n) ] = 1;
    
    return( sample(tmzPop) );
  });
  
  pop1 = reactive({
    getPatient( pop1_age(), log( as.numeric(pop1_kps()) ), pop1_xrt(), pop1_tmz(), pop1_eor() );
  });
  
  
  output$popPlot = renderPlot({
    
#    demo = pop1()$demo;
#    surv = pop1()$surv;
#    if( is.null( input$thisPat ) ) {
#      thisSurv = surv[[1]];
#    } else if ( !(input$thisPat > 1) ) {
#      thisSurv = surv[[1]];
#    } else {
#      thisSurv = surv[[input$thisPat]];
#    }

#    thisDf = as.data.frame(pop1()$surv[[input$thisPat]][c(1,2)]);
#    demo$surv_days_95 = unlist(lapply(newpat$surv, function(x) which.max( x$surv ) ));
g=    plot(1:20, 1:20);
#    demo$surv_days_90 = unlist(lapply(newpat$surv, function(x) min(x$time[ x$surv > 0.90 ]) ));
#    demo$surv_days_75 = unlist(lapply(newpat$surv, function(x) min(x$time[ x$surv > 0.75 ]) ));
#    demo$surv_days_50 = unlist(lapply(newpat$surv, function(x) min(x$time[ x$surv > 0.50 ]) ));
#    demo$surv_days_25 = unlist(lapply(newpat$surv, function(x) min(x$time[ x$surv > 0.25 ]) ));
    
 #   g = ggplot(demo, aes(x=AGE, y=survdays)) +
#        stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE);
    
    return(g);
  })
  
  output$popTable = renderTable({
#    demo = pop1()$demo;
#    surv = pop1()$surv;
    
#    demo[ input$thisPat, ];
  })
    

})
