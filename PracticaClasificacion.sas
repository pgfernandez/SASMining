libname lib_data '/home/YOUR_FOLDER_HERE/data';



/* PASO DATA */

/* carga de datos */
DATA lib_data.babyClassWeigths;
	set lib_data.birthwgt;
run;


/* Análisis exploratorio */

title 'Primeros registros del dataset';
	proc print data= lib_data.babyClassWeigths (obs=15);
run;

/* frecuencias */
proc freq data=lib_data.babyClassWeigths;
 tables LowBirthWgt AgeGroup Death Drinking Married Race Smoking SomeCollege / nocum plots=freqplot;
run;


/* prepraración de los datos */
/* dicomtomizar las variables categóricas y asignar missings a algunas de las categorías */

data lib_data.babyWDummy (drop=LowBirthWgt AgeGroup Death Married Race Smoking Drinking SomeCollege);

set lib_data.babyClassWeigths;

/* variable objetivo en numérica */
if LowBirthWgt='Yes' then lowBW=1; else lowBW=0;

/*variables de clsificación */

/*convierto los tres posibles grupos en variables dicotómicas */
if AgeGroup=1 then AgeGroup1=1; else AgeGroup1=0;
if AgeGroup=2 then AgeGroup2=1; else AgeGroup2=0;
if AgeGroup=3 then AgeGroup3=1; else AgeGroup3=0;

if Death='Yes' then xDeath=1; else xDeath=0;

if Married='Yes' then xMarried=1; else xMarried=0;

if Race='Asian' then RaceAsian=1; else RaceAsian=0;
if Race='Black' then RaceBlack=1; else RaceBlack=0;
if Race='Hispanic' then RaceHispanic=1; else RaceHispanic=0;
if Race='Native' then RaceNative=1; else RaceNative=0;
if Race='White' then RaceWhite=1; else RaceWhite=0;

/*en el caso de Drinking el No es lo más frecuente ppor lo que los missings se los apuntamos al No*/
if Drinking='Yes' then xDrinking=1; else if Drinking='No' then xDrinking=0;
	if Drinking=' ' then xDrinking=0;

if SomeCollege='Yes' then xSomeCollege=1; else if SomeCollege='No' then xSomeCollege=0; 
	/*en este caso los missings los tenemos que distribuir al 50%, ya que es muy alta la frecuencia que tienen */
	if xSomeCollege=. then do;
   		/*Si vamos por una fila impar, le asignamos 1, si es par 0, así distribuimos al 50%*/
   		if mod(_N_,2)=1 then xSomeCollege=1; else xSomeCollege=0;
    end;

/* los missing va a la mayoritaria, No */
if Smoking='Yes' then xSmoking=1; else if Smoking='No' then xSmoking=0;
	if Smoking=' ' then xSmoking=0;
run;



/* Para hacer más entendibles las variables que hemos dicotomizado vamos a formatear los valores a 
las diferentes categorías */

proc format;
	value labelsLow 0 = "Sin bajo Peso"
					  1 = "Con Bajo Peso";
	
	value labelsAgeOne  0 = "No Grupo 1"
				      1 = "Grupo 1";
	
	value labelsAgeTwo  0 = "No Grupo 2"
				      1 = "Grupo 2";
	
	value labelsAgeThree  0 = "No Grupo 3"
				      1 = "Grupo 3";
	
	value labelsDeath  0 = "Vivo"
						  1 = "Muerto";
	
	value labelsMarried 0 = "No Casada"
					   1 = "Casada";
	
	
	value labelsCollege  0 = "Sin Estudios superiores"
						  1 = "Con Estudios Superiores";
						  
	value labelsDrinking 0 = "No bebe"
					   1 = "Bebe";
					   
	value labelsSmoking 0 = "No fuma"
					   1 = "Fuma";
	
	value labelsRaceAsian 0 = "No Asiático"
					   1 = "Asiático";
	
	value labelsRaceBlack 0 = "No Negro"
					   1 = "Negro";
					   
	value labelsRaceHispanic 0 = "No Hispano"
					   1 = "Hispano";
	 
	value labelsRaceNative 0 = "No Nativo Americano"
					   1 = "Nativo Americano";				

	value labelsRaceWhite 0 = "No Blanco"
					   1 = "Blanco";				

run;

proc freq data=lib_data.babyWDummy;
 tables lowBW AgeGroup1 AgeGroup2 AgeGroup3 xDeath xDrinking xMarried RaceAsian RaceBlack RaceHispanic RaceNative RaceWhite xSmoking xSomeCollege / nocum plots=freqplot;
 format lowBW labelsLow.
		AgeGroup1 labelsAgeOne.
		AgeGroup2 labelsAgeTwo.
		AgeGroup3 labelsAgeThree.
		xDeath labelsDeath.
		xDrinking labelsDrinking.
		xMarried labelsMarried.
		RaceAsian labelsRaceAsian.
		RaceBlack labelsRaceBlack.
		RaceHispanic labelsRaceHispanic.
		RaceNative labelsRaceNative.
		RaceWhite labelsRaceWhite.
		xSmoking labelsSmoking.
		xSomeCollege labelsCollege.;
run;

/* ordenamos y eliminamos duplicados */
proc sort nodupkey data=lib_data.babyWDummy;  
	by lowBW AgeGroup1 AgeGroup2 AgeGroup3 xDeath xDrinking xMarried RaceAsian RaceBlack RaceHispanic RaceNative RaceWhite xSmoking xSomeCollege; 
run;

/* Analisis de correlacion */
proc corr data=lib_data.babyWDummy;
 var _numeric_;
run;




/* Clusters */

ods graphics on;

proc cluster data=lib_data.babyWDummy method=centroid 
					nonorm ccc pseudo rmsstd rsquare 
					
              out=lib_data.resultados plots=den(height=rsq) PRINT=20 plots(maxpoints=300);
             
		      var  lowBW AgeGroup1 AgeGroup2 AgeGroup3 xDeath xDrinking xMarried RaceAsian RaceBlack RaceHispanic RaceNative RaceWhite xSmoking xSomeCollege;
run; 


proc cluster data=lib_data.babyWDummy method=ward 
					nonorm ccc pseudo rmsstd rsquare 
					
              out=lib_data.resultados plots=den(height=rsq) PRINT=20 plots(maxpoints=300);
              
		      var  lowBW AgeGroup1 AgeGroup2 AgeGroup3 xDeath xDrinking xMarried RaceAsian RaceBlack RaceHispanic RaceNative RaceWhite xSmoking xSomeCollege;
run; 

proc varclus data=lib_data.babyWDummy outtree=tree centroid maxclusters=9
             plots=dendrogram(vertical height=ncl);
   var _numeric_;
run;
ods graphics off;



