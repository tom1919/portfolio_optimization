/* test arch effect */


/* Load Needed Data */
proc import datafile = 'C:\Users\tommy\Google Drive\Coursework\Spring 1\Financial Analytics\portfolio_optimization\data\stocks_r.csv'
	out = stocks dbms = csv replace;
	datarow = 3;
run;


/* Test for GARCH Effects and Normality */
proc autoreg data=Stocks ARCHTEST plots(unpack);
   model XOM_Close =/ archtest normal;
run;

/* Test for GARCH Effects and Normality */
proc autoreg data=Stocks ARCHTEST plots(unpack);
   model MRK_Close =/ archtest normal;
run;

/* Test for GARCH Effects and Normality */
proc autoreg data=Stocks ARCHTEST plots(unpack);
   model MSFT_Close =/ archtest normal;
run;

/* Test for GARCH Effects and Normality */
proc autoreg data=Stocks ARCHTEST plots(unpack);
   model CVX_Close =/ archtest normal;
run;

/* Test for GARCH Effects and Normality */
proc autoreg data=Stocks ARCHTEST plots(unpack);
   model UTX_Close =/ archtest normal;
run;


proc autoreg data=Stocks outest = param_estimates;
   model XOM_Close =/ noint garch=(p=1, q=1) dist =t method=ml; 
                            output out=garch_n ht=predicted_var;
run;

proc autoreg data=Stocks outest = param_estimates;
   model UTX_Close =/ noint garch=(p=1, q=1, type = QGARCH) dist = t method=ml; 
                            output out=garch_n ht=predicted_var;
run;

