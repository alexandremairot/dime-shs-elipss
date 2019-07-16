/*************************************************************/
/* name: longitudinal_weighting_program.sas                  */
/* author: Alexandre Mairot (alexandre.mairot@sciencespo.fr) */
/* title: longitudinal weighting for ELIPSS survey           */
/* program: SAS                                              */
/* version: 1.0                                              */
/*************************************************************/ 

/***************************************/
/* Definition of environment variables */
/***************************************/

/* Address of the survey data file */

%LET studyfile = "[Address of the survey data file]";

/* Address of the second data file */

%LET secondfile = "[Address of the second data file]";

/* Informations for the longitudinal weighting file */

%LET longvar = [name of the longitudinal weighting variable];
LIBNAME outfolder "[folder of the longitudinal weighting file]";
%LET outfile = [name of the longitudinal weighting file];

/*****************************************************/
/* Beginning of the script                           */
/*****************************************************/
LIBNAME macro_c "[Address of the CALMAR macro file released by INSEE]";
OPTIONS MSTORED SASMSTORE = macro_c;
LIBNAME source "[adress of the folder where is the initial weightling file]";

DATA base1;
INFILE &studyfile DLM='|' TRUNCOVER DSD LRECL=32767 FIRSTOBS=1;
INPUT
	uid : $7.;
RUN;
DATA base2;
INFILE &secondfile DLM='|' TRUNCOVER DSD LRECL=32767 FIRSTOBS=1;
INPUT
	uid : $7.;
RUN;
PROC SQL NOPRINT;
	CREATE TABLE base AS
		SELECT uid
		FROM base2
		WHERE uid IN (SELECT uid FROM base1);
	DROP TABLE base1, base2;
QUIT;

PROC SQL NOPRINT;		
	CREATE TABLE basePond AS
		SELECT	uid, poids_init, age2 as AGE, sexe, nat, dipl, zeat
		FROM source._poids2017
		WHERE ((uid IN (SELECT uid FROM base))AND(poids_init IS NOT NULL));
	SELECT count(uid) INTO: nrep
		FROM basePond;
QUIT;
DATA marges;
INPUT VAR $ N MAR1 MAR2 MAR3 MAR4 MAR5 MAR6 MAR7 MAR8;
CARDS;
AGE 7 8.06 19.14 17.50 19.56 17.20 14.64 3.90 .
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
	CREATE TABLE outfolder.&outfile. AS
		SELECT	basePond.uid,
				poidsTable.poidsEnq as &longvar.
		FROM basePond
		JOIN poidsTable ON basePond.uid = poidsTable.uid;
	DROP TABLE basePond, poidsTable ;
QUIT;
