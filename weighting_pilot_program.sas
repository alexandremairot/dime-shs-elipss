/*************************************************************/
/* name: weighting_pilot_version.sas                         */
/* author: Alexandre Mairot (alexandre.mairot@sciencespo.fr) */
/* title: weighting for ELIPSS survey (Pilot period)         */
/* program: SAS                                              */
/* version: 1.0                                              */
/*************************************************************/ 

/***************************************/
/* Definition of environment variables */
/***************************************/

/* Address of the survey data file */

%LET fichierIndividu = "[Address of the survey data file]";

/* Informations for the weighting file */

%LET piloteVar = [name of the weighting variable];
LIBNAME sortie "[folder of the weighting file]";
%LET final = [name of the weighting file];

/***************************/
/* Beginning of the script */
/***************************/
LIBNAME macro_c "[address of the macro file CALMAR programmed by INSEE]";
OPTIONS MSTORED SASMSTORE = macro_c;
LIBNAME source "[adress of the folder where is the initial weightling file]";

DATA base;
INFILE &fichierIndividu DLM='|' TRUNCOVER DSD LRECL=32767 FIRSTOBS=1;
INPUT
	uid : $7.;
RUN;

PROC SQL NOPRINT;		
	CREATE TABLE basePond AS
		SELECT	uid, pdsplt_init AS poids_init, age1 AS AGE, sexe, nat, dipl, zeat
		FROM source.[name of the initial weighting file]
		WHERE ((uid IN (SELECT uid FROM base))AND(pdsplt_init IS NOT NULL));
	SELECT count(uid) INTO: nrep
		FROM basePond;
QUIT;
DATA marges;
INPUT VAR $ N MAR1 MAR2 MAR3 MAR4 MAR5 MAR6 MAR7 MAR8;
CARDS;
AGE 6 10.99 16.20 17.50 19.56 17.20 18.55 . .
SEXE 2 49.02 50.98 . . . . . .
NAT 3 88.28 5.64 6.08 . . . . .
DIPL 4 27.83 23.35 33.64 15.18 . . . .
ZEAT 8 18.86 16.32 6.42 8.62 13.85 11.42 12.17 12.34
;
%CALMAR (DATA=basePond,
		 POIDS=poids_init,
		 IDENT=uid,
		 maxiter=30,
		 DATAMAR=marges,
		 PCT=oui,
		 EFFPOP=&nrep.,
		 M=2,
		 EDITPOI=oui,
		 OBSELI=oui,
		 DATAPOI=poidsTable,
		 POIDSFIN=poidsEnq);
PROC SQL NOPRINT;
	CREATE TABLE pilote AS
		SELECT	basePond.uid,
				basePond.age,
				basePond.sexe,
				basePond.nat,
				basePond.dipl,
				basePond.zeat,
				poidsTable.poidsEnq as &piloteVar.
		FROM basePond
		JOIN poidsTable ON basePond.uid = poidsTable.uid;
	DROP TABLE basePond, poidsTable ;
QUIT;
PROC SQL NOPRINT;
	CREATE TABLE sortie.&final. AS
		SELECT	uid,
				age AS AGE1,
				sexe,
				nat,
				dipl,
				zeat,
				&piloteVar.
		FROM pilote;
	DROP TABLE pilote;
QUIT;