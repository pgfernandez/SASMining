
libname lib_data '/home/YOUR_FOLDER_HERE/data';

/* PASO DATA */

/* carga de datos */
DATA lib_data.babyWeigths;
	set lib_data.bweight;
run;

/* Análisis exploratorio */

title 'Primeros registros del dataset';
	proc print data= lib_data.babyWeigths (obs=15);
run;

/* elimino la variable momAge y creo una nueva con el valor real de la edad para hacerla más entendible */
DATA lib_data.babyWeigths; 
	set lib_data.babyWeigths;
	xMomAge=MomAge + 25;
	drop MomAge;
run;

title 'Registros de la edad de la madre';
proc print data= lib_data.babyWeigths (obs=10);
	var xMomAge;
run;


/* eliminación de duplicados */
proc sort nodupkey data=lib_data.babyWeigths;
by Weight Black Married Boy xMomAge MomSmoke CigsPerDay MomWtGain Visit MomEdLevel ;
run;

/* frecuencias */


/* Para hacer más entendibles las variables categóricas vamos a formatear los valores a 
las diferentes categorías */

proc format;
	value labelsRace 0 = "No raza negra"
					  1 = "Raza Negra";
	value labelsSex  0 = "Niña"
				      1 = "Niño";
	value labelsMarried  0 = "No Casada"
						  1 = "Casada";
	value labelsEdLevel  0 = "Sin estudios"
						  1 = "Estudios Básicos"
						  2 = "Estudios Medios"
						  3 = "Estudios Superiores";
							
	value labelsSmoke 0 = "No fuma"
					   1 = "Fuma";
				
run;


title 'Tablas de frecuencias variables Categóricas';
proc freq
	data= lib_data.babyWeigths;
	tables Black Boy Married MomSmoke MomEdLevel Visit / nocum plots=freqplot;
	format Black labelsRace.
		   Boy labelsSex.
		   Married labelsMarried.
		   MomSmoke labelsSmoke.
		   MomEdLevel labelsEdLevel.;
run;


/* saco los estadísiticos básicos de las variables continuas 
para ver sus observaciones, medias, mínimos y máximos y ver si hay missings */

title 'Estadísticos para las variables continuas (incluida objetivo)';
ods noproctitle;
proc means data= lib_data.babyWeigths n nmiss mean median std min max;
	var weight xMomAge CigsPerDay MomWtGain;
run;

title 'Distribución de MomWtGain';
/* Analisis de la variable MomWtGain*/
proc univariate data=lib_data.babyWeigths normal plot;
 var MomWtGain;
 qqplot MomWtGain / NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
 HISTOGRAM /NORMAL(COLOR=MAROON W=4) CFILL = BLUE CFRAME = LIGR;
 INSET MEAN STD /CFILL=BLANK FORMAT=5.2;
run;

/* elimino los registros con pérdida de peso de -30 y 68kg al considerarlos outliers */
data lib_data.babyWeigths;
   set lib_data.babyWeigths;
   if MomWtGain <= -29 or MomWtGain > 42 then delete;
run;

/* Volvemos a estudiarlo la variable de la ganancia de peso */
title 'Distribución de MomWtGain';
/* Analisis de la variable MomWtGain*/
proc univariate data=lib_data.babyWeigths normal plot;
 var MomWtGain;
 qqplot MomWtGain / NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
 HISTOGRAM /NORMAL(COLOR=MAROON W=4) CFILL = BLUE CFRAME = LIGR;
 INSET MEAN STD /CFILL=BLANK FORMAT=5.2;
run;


title 'Distribución de xMomAge';
/* Analisis de la variable xMomAge*/
proc univariate data=lib_data.babyWeigths normal plot;
 var xMomAge;
 qqplot xMomAge / NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
 HISTOGRAM /NORMAL(COLOR=MAROON W=4) CFILL = BLUE CFRAME = LIGR;
 INSET MEAN STD /CFILL=BLANK FORMAT=5.2;
run;


/* Analisis de la variable objetivo Weight*/
proc univariate data=lib_data.babyWeigths normal plot;
 var Weight;
 qqplot Weight / NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
 HISTOGRAM /NORMAL(COLOR=MAROON W=4) CFILL = BLUE CFRAME = LIGR;
 INSET MEAN STD /CFILL=BLANK FORMAT=5.2;
run;


/* elimino los registros menores a 470 gramos dado que no parece que puedan ser viables */
data lib_data.babyWeigths;
   set lib_data.babyWeigths;
   if Weight <= 470 then delete;
run;

/* Vuelvo a analizar la variable objetivo*/
proc univariate data=lib_data.babyWeigths normal plot;
 var Weight;
 qqplot Weight / NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
 HISTOGRAM /NORMAL(COLOR=MAROON W=4) CFILL = BLUE CFRAME = LIGR;
 INSET MEAN STD /CFILL=BLANK FORMAT=5.2;
run;

/* ordenamos los datos  */;
proc sort data=lib_data.babyWeigths;
	by Weight;
run;

/* Análisis de correlación */
proc corr data=lib_data.babyWeigths;   
      var _numeric_;
run;

/* eliminamos la variable CigsPerDay al estar altamente correlada con MomSmoke */
DATA lib_data.babyWeigths; 
	set lib_data.babyWeigths;
	drop CigsPerDay;
run;



/* MODELO */



/* Modelo básico */
proc glm data=lib_data.babyWeigths;
   class Married Boy Black xMomAge Visit MomSmoke MomEdLevel MomWtGain;
   model Weight = Married Boy Black xMomAge Visit MomSmoke MomEdLevel MomWtGain / solution e;
run;


%let lib1 = '/home/YOUR_FOLDER_HERE/data/macro.txt';

libname lib_data '/home/YOUR_FOLDER_HERE/data';
%let list_selection = stepwise(select=aic choose=validate)*stepwise(select=aic choose=cv)*backward*forward;

%macro regression (t_input, tipo,vardepen,sem_ini,sem_fin,varindep);
%local i nextSelectionType method;
%let i=1;
%do method=1 %to 4;
    %let nextSelectionType = %scan(&list_selection, &i,'*');
	%do frac=3 %to 5;
		data;fra=&frac/10;call symput('porcen',left(fra));run;
		%do semilla=&sem_ini. %to &sem_fin.;
			ods graphics on;
			ods output   SelectionSummary=modelos;
			ods output   SelectedEffects=efectos;
			ods output   Glmselect.SelectedModel.FitStatistics=ajuste;

			proc glmselect data=&t_input plots=all seed=&semilla;
			  partition fraction(validate=&porcen);
			  class Black Boy Married MomEdLevel MomSmoke Visit xMomAge;
			  model &vardepen = &varindep 
			  / selection=&nextSelectionType details=all stats=all;
			run;

			ods graphics off;   
			ods html close;   
			data union; i=12; set efectos; set ajuste point=i; run; 
			data  null;semilla=&semilla;tipo=&tipo; porcen=&porcen; 
			file '/home/YOUR_FOLDER_HERE/data/macroData.txt' mod dlm='~';
			set union;put tipo @15 nvalue1 @30 semilla @45 porcen @60 "&nextSelectionType" "~" @115 effects;run;
		%end;
	%end;
   %let i = %eval(&i + 1);
%end;
proc sql; drop table modelos,efectos,ajuste,union; quit;
%mend;

%regression(lib_data.babyWeigths, 'M', Weight, 12345, 12384,
Black Married Boy xMomAge MomSmoke MomWtGain Visit MomEdLevel
xMomAge*Black xMomAge*Married xMomAge*Boy xMomAge*MomSmoke xMomAge*MomWtGain xMomAge*Visit xMomAge*MomEdLevel);


proc import datafile= '/home/YOUR_FOLDER_HERE/data/macroData.txt'
            out=lib_data.resultados(rename=(VAR1=nombre VAR2=ase VAR3=semilla  VAR4=porcentaje VAR5=tipo_selec VAR6=modelo))
            dbms=dlm
            replace; 
       		getnames = no ;
			GUESSINGROWS=1000;
     		delimiter='~';
run;

*ordenar los resultados del modelo;
proc sort  data=lib_data.resultados; by nombre modelo;

*obtener la frecuencia de los modelos y ordenarlos por frecuencia descendiente;
proc freq  data=lib_data.resultados; tables modelo/ noprint out=lib_data.mejores; ; run;
proc sort data=lib_data.mejores out=lib_data.conteo; by descending count; run;

/* me salen cuatro modelos que destacan sobre el resto */

/*  
 1. Black Married Boy MomSmoke MomWtGain MomEdLevel
 2. Black Married Boy MomWtGain Visit MomEdLevel MomSmoke*xMomAge
 3. Married Boy MomWtGain Visit MomEdLevel Black*xMomAge MomSmoke*xMomAge
 4. Black Boy MomWtGain Visit MomEdLevel Married*xMomAge MomSmoke*xMomAge */


ods graphics on;

/* Modelos seleccionados */
/1/;
proc glm data=lib_data.babyWeigths;
  class Black Boy Married MomEdLevel MomSmoke MomWtGain;
  model Weight = Black Married Boy MomSmoke MomWtGain MomEdLevel	
/ solution e;
run;

/2/;
proc glm data=lib_data.babyWeigths;
  class Black Boy Married MomEdLevel MomSmoke Visit xMomAge MomWtGain;
  model Weight = Black Married Boy MomWtGain Visit MomEdLevel MomSmoke*xMomAge	
/ solution e;
run;

/3/;
proc glm data=lib_data.babyWeigths;
  class Black Boy married MomEdLevel MomSmoke Visit xMomAge MomWtGain;
  model Weight = Married Boy MomWtGain Visit MomEdLevel Black*xMomAge MomSmoke*xMomAge 	
/ solution e;
run;

/4/;
proc glm data=lib_data.babyWeigths;
  class Black Boy Married MomEdLevel MomSmoke Visit MomWtGain xMomAge;
  model Weight = Black Boy MomWtGain Visit MomEdLevel Married*xMomAge MomSmoke*xMomAge	
/ solution e;
run;

/* cogemos el mejor, que parece que es el primero por el R2 que tiene y mostramos la gráfica de los aciertos */
ods exclude CooksDPlot ObservedByPredicted QQPlot RStudentByLeverage   ;
proc glm data=lib_data.babyWeigths plots(only maxpoints=none)=(diagnostics(unpack) 
		intplot);
	class Black Boy Married MomEdLevel MomSmoke MomWtGain;
	model Weight = Black Married Boy MomSmoke MomWtGain MomEdLevel / solution ss3;
	run;

