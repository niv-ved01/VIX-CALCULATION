PROC IMPORT OUT=OPTION DATAFILE='/home/option.csv' 
	DBMS=CSV 
	REPLACE;
	GETNAMES=YES;
	DATAROW=2;
RUN;


/* using proc iml to estimate the vix for a particular date*/
proc iml;
	use OPTION;
	read all var {tau, k, call_option_price, r, put_option_price, s} into options;
	opt = {'tau', 'k', 'c', 'r', 'p', 's'};
	read all var {date} into date;	
	mattrib options c=opt;

	
	/*calculating the absolute difference between the call and put options*/
	diff = abs(options[,'c'] - options[,'p']); 
	
	/*declaring matrix to store the least difference */
	least_diff = j(nrow(options), 1, .);  
	
	/*declaring matrix to calculate and store the time of expiration*/
	exp = j(nrow(options), 4, .); 
	exp_col = {'near_date', 'next_date', 't1', 't2'};
	mattrib exp c=exp_col;
	
	do i = 1 to nrow(options);                    
	    read all var {call_option_price} into c where(date=(date[i]) & tau=(options[i, 'tau']));
	    read all var {put_option_price} into p where(date=(date[i]) & tau=(options[i, 'tau']));	   

	 	least_diff[i] = min(abs(c - p)); 

	 	read all var {tau} into tau
	        where(date=(date[i]));
	 	if min(tau) <= 30
	 	then
        	near_date = max(tau[loc(tau <= 30)]);
    	else
    		near_date = min(tau);  
 	   next_date = min(tau[loc(tau > near_date)]); 	   
 	   t1 = near_date/365; /*convert the time from days to years by dividing by 365*/
 	   t2 = next_date/365;
 	   exp[i, 1] = near_date;
 	   exp[i, 2] = next_date;
 	   exp[i, 3] = t1; 
 	   exp[i, 4] = t2;	 	
 	   
	end;
	
	print t1 t2 ;

	min = (diff=least_diff);
	
	/*combing the matrices*/
	matrix1 = options||diff||min||exp;
	matrix_colm = opt//{'diff', 'min'}//exp_col;
	mattrib matrix1 c=matrix_colm;
	
	/*filtering the matrix into near-term and next-term based on the tau values*/
	matrix2 = matrix1[loc(matrix1[,'tau']=matrix1[,'near_date'] |matrix1[,'tau']=matrix1[,'next_date']),];
	date_f = date[loc(matrix1[,'tau']=matrix1[,'near_date'] |matrix1[,'tau']=matrix1[,'next_date']),];
	mattrib matrix2 c=matrix_colm;
	

	row_x = matrix2[loc(matrix2[,'min']>0 ) ,]; 
	mattrib row_x c=matrix_colm;	
	colm_x = opt//{'diff'}//exp_col;
	row_x = row_x[,colm_x];
	mattrib row_x c=colm_x;

	date1 = date_f[loc(matrix2[,'min']),]; 
	mattrib date1 format=ddmmyy10.;

	f1  = j(nrow(row_x), 1, .);
	f2 	= j(nrow(row_x), 1, .);
	k01 = j(nrow(row_x), 1, .);
	k02 = j(nrow(row_x), 1, .);
	delta = j(nrow(matrix2), 1, .);
	mid_qoute_price = j(nrow(matrix2), 1, .);
	contributions_by_strike = j(nrow(matrix2), 1, .);
	option_type = j(nrow(matrix2), 1, '              ');
	do i = 1 to nrow(row_x);
		if row_x[i,'tau']<=30
		then
			do;
			f1[i]= row_x[i,'k'] + exp(row_x[i,'r']*row_x[i, 't1'])*(row_x[i,'c'] - row_x[i,'p']);
			k01[i] = max(options[loc(date = (date1[i]) & options[,'tau'] =  (row_x[i,'tau']) & options[,'k'] < (f1[i])), 'k']);
	        end;
		else
			do;
			f2[i]= row_x[i,'k'] + exp(row_x[i,'r']*row_x[i, 't2'])*(row_x[i,'c'] - row_x[i,'p']);		
			k02[i] = max(options[loc(date = (date1[i]) & options[,'tau'] =  (row_x[i,'tau']) & options[,'k'] < (f2[i])), 'k']);
	        end;
	    
	    cnt=0;    
		do j = 1 to nrow(matrix2);		
	   		if (date_f[j] = date1[i]) & (matrix2[j, 'tau'] = row_x[i, 'tau']) 
	   		then 
	   			do;
	   				cnt = cnt + 1;
	   				
		   			if cnt = 1 
		   			then
		   				delta[j] = (matrix2[j+1, 'k'] - matrix2[j, 'k']); /*lower edge*/
					
					else if j=nrow(matrix2) | matrix2[min(j+1, nrow(matrix2)), 'tau'] ^= matrix2[j, 'tau'] | date_f[min(j+1, nrow(matrix2))] ^= date_f[j]
		   			then
		   				delta[j]= (matrix2[j, 'k'] - matrix2[j-1, 'k']);/*upper edge*/	
					else
						delta[j] = (matrix2[j+1, 'k'] - matrix2[j-1, 'k'])/2;					
								
					if matrix2[j, 'k'] < coalesce(k01[i], k02[i])
					then do;
						mid_qoute_price[j] = matrix2[j,'p'];
						option_type[j] = 'PUT';
						end;
					else if matrix2[j, 'k'] > coalesce(k01[i], k02[i])
					then do;
						mid_qoute_price[j] = matrix2[j,'c'];
						option_type[j] = 'CALL';
						end;
					else if matrix2[j, 'k'] = coalesce(k01[i], k02[i])
					then do;	
						mid_qoute_price[j] =(matrix2[j,'p'] + matrix2[j,'c'])/2 ;
						option_type[j] = 'PUT/CALL AVG.';
						end;
					
					if row_x[i,'tau']<=30
						then
						contributions_by_strike[j] = delta[j]/(matrix2[j, 'k']##2)*exp(matrix2[j,'r']*row_x[i, 't1'])*mid_qoute_price[j];
					else
						contributions_by_strike[j] = delta[j]/(matrix2[j, 'k']##2)*exp(matrix2[j,'r']*row_x[i, 't2'])*mid_qoute_price[j];
				end;
		end;    	
	end;
	read all var {k} into k;
	read all var {call_option_price} into c;
	read all var {put_option_price} into p;
	print k option_type mid_qoute_price contributions_by_strike;
	
	total_contribution_near = j(nrow(row_x), 1, .);
	total_contribution_next = j(nrow(row_x), 1, .);
	volatility1  = j(nrow(row_x), 1, .);
	volatility2  = j(nrow(row_x), 1, .);
	vix		 = j(nrow(row_x), 1, .);
	do i = 1 to nrow(row_x);
		if row_x[i,'tau']<=30
		then do;
			total_contribution_near[i] = sum(contributions_by_strike[loc(date_f = date1[i] & matrix2[, 'tau'] = row_x[i, 'tau'] )]);
			volatility1[i] = (2/row_x[i, 't1'])*total_contribution_near[i];
			volatility1[i] = volatility1[i] - (1/row_x[i, 't1'])*(f1[i]/k01[i]-1)##2;
			end;
		else do;
			total_contribution_next[i] = sum(contributions_by_strike[loc(date_f = date1[i] & matrix2[, 'tau'] = row_x[i, 'tau'] )]);
			volatility2[i] = (2/row_x[i, 't2'])*total_contribution_next[i];
			volatility2[i] = volatility2[i] - (1/row_x[i, 't2'])*(f2[i]/k02[i]-1)##2;
			end;
		if mod(i,2)=0
		then do;
			vix[i] = row_x[i, 't1']*volatility1[i-1]*(row_x[i, 'next_date']-30)/(row_x[i, 'next_date'] - row_x[i, 'near_date']) +  row_x[i, 't2']*volatility2[i]*(30 - row_x[i, 'near_date'])/(row_x[i, 'next_date'] - row_x[i, 'near_date']);
			vix[i] = 100*sqrt(vix[i]*365/30);
		end;
	end;
	title'VIX estimation';
	print date1[l='date'] f1 f2 k01 k02;
	print date1[l='date'] (row_x[, 'tau'])[l='T'] (row_x[, 'T1'])[l='T1'] (row_x[, 'T2'])[l='T2']  f1 f2 k01 k02 total_contribution_near total_contribution_next volatility1 volatility2 vix[f = number5.2];	

CREATE vix_data var {date1 tau T f1 f2 k01 k02 c p r total_contribution_near total_contribution_next volatility1 volatility2 vix };
append;
close nextterm;
quit;

proc sgplot data=vix_data;
  series x=date1 y=VIX / markers markerattrs=(symbol=circlefilled);
  xaxis label="Date";
  yaxis label="VIX";
  title "VIX";
run;
