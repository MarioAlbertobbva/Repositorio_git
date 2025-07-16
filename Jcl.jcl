//EPIPDLEV JOB (ACCT),'QPIP',CLASS=A,MSGCLASS=J,MSGLEVEL=(1,1),
//         COND=(4,LT),REGION=0M
//*OBPARM S=TC03
//LIBPROC  JCLLIB ORDER=EBDEXPR.PDSB111Z.PROCLIB
//**********************************************************
//* OBTIENE LA FECHA DE CORTE                              *
//*    (FECHAA ACTUAL + 5 DIAS) - 1 MES                    *
//**********************************************************
//SORT0001 EXEC PROC=EXPRP23D,EQUAL='NOEQUALS',SYNCSORT='S'
//SORTIN   DD *

/*
//SORTOUT  DD DSN=EBDQPIP.INPXD05X.QPIPRLEV.FCORTE,
//            DISP=(,CATLG,DELETE),
//            SPACE=(CYL,(1,1),RLSE),
//            RECFM=FB,LRECL=80,BLKSIZE=0
//SYSIN    DD *
 OPTION COPY
 INREC OVERLAY=(1:C'  WHERE FEC_EJEC <= ',
                  X'7D',DATE1,C'  ',X'7D',49X,
               22:22,8,Y4T,ADDDAYS,+5,TOGREG=Y4T,
               22:22,8,Y4T,SUBMONS,+1,TOGREG=Y4T(-))
 OUTFIL FNAMES=SORTOUT,
 BUILD=(1,80,/,
        C'  ORDER BY FEC_EJEC,',/,
        C'           HMS_EJEC,',/,
        C'           COD_BEAEVENT,',/,
        C'           COD_MAJOR,',/,
        C'           COD_MINOR,',/,
        C'           COD_EVENEJE,',/,
        C'           QNU_ORDEN')
/*
//**********************************************************
//* EXTRAE LAS FILAS DE TQPIPLEV QUE TENGAN UNA FEC-EJEC   *
//* MENOR O IGUAL QUE LA FECHA DE CORTE                    *
//**********************************************************
//UQPIPLEV EXEC PROC=EXPRP42D,SSID='DS0D',JOB='EPIPDLEV',QUIESCE=Y
//P42.SYSIN1   DD *
 SELECT COD_PARTDIA  ,
        COD_BEAEVENT ,
        COD_MAJOR    ,
        HMS_EJEC     ,
        COD_EVENEJE  ,
        QNU_ORDEN    ,
        COD_MINOR    ,
        FEC_EJEC     ,
        COD_TRANSAC  ,
        COD_TIPTRN   ,
        COD_SUBTRN   ,
        COD_VERTRN   ,
        COD_ERRAVI   ,
        COD_SEVERR   ,
        DES_TRACEID  ,
        DES_PARSPAID ,
        COD_UIDTR    ,
        DES_DATAEVEN ,
        AUD_USUALTA  ,
        AUD_TIMALTA
   INTO
        COD_PARTDIA  CHAR(6) FILL YES,
        COD_BEAEVENT ,
        COD_MAJOR CHAR (6) FILL YES,
        HMS_EJEC     ,
        COD_EVENEJE  ,
        QNU_ORDEN CHAR (6) FILL YES,
        COD_MINOR CHAR (6) FILL YES,
        FEC_EJEC     ,
        COD_TRANSAC  ,
        COD_TIPTRN   ,
        COD_SUBTRN   ,
        COD_VERTRN   ,
        COD_ERRAVI   ,
        COD_SEVERR   ,
        DES_TRACEID  ,
        DES_PARSPAID ,
        COD_UIDTR    ,
        DES_DATAEVEN ,
        AUD_USUALTA  ,
        AUD_TIMALTA
   FROM D318.TQPIPLEV

/*
//P42.SYSIN2  DD DSN=EBDQPIP.INPXD05X.QPIPRLEV.FCORTE,
//            DISP=(OLD,DELETE,KEEP)
//P42.SYSREC  DD DSN=EBDQPIP.APLXD00X.TQPIPLEV(+1),
//            DISP=(,PASS),
//            SPACE=(CYL,(1000,1000),RLSE),
//            DATACLAS=EXTCOMPS
//**********************************************************
//* COMPRUEBA SI HAY FILAS QUE COINCIDEN CON LA BUSQUEDA   *
//*        RC=00 DATASET CON DATOS                         *
//*        RC=04 DATASET VACIO                             *
//**********************************************************
//DSTEST01 EXEC PROC=EXPRP20D
//DD1      DD DISP=(SHR,PASS),DSN=EBDQPIP.APLXD00X.TQPIPLEV(+1)
//P20.SYSIN   DD *
 PRINT INFILE(DD1) CHARACTER COUNT(1)
/*
//**********************************************************
//* SI HAY DATOS, CATALOGA LA VERSION DEL GDG              *
//**********************************************************
//CATAL001 EXEC PROC=EXPRP27D,COND=(4,EQ,DSTEST01.P20)
//DD1      DD DISP=(OLD,CATLG),DSN=EBDQPIP.APLXD00X.TQPIPLEV(+1)
//**********************************************************
//* OBTIENE LAS PARTICIONES A CARGAR EN VACIO, SI HAY DATOS*
//* - OBTIENE LAS PARTICIONES SIN DUPLICADOS QUE SE HAYAN  *
//*   DESCARGADO Y LO DEJA EN ESTE FORMATO                 *
//*D318.TQPIPLEV PART XX                                   *
//*D318.TQPIPLEV PART YY                                   *
//*D318.TQPIPLEV PART ZZ                                   *
//*                                                        *
//*                                                        *
//* - EN TODOS LOS REGISTROS MENOS EL ULTIMO               *
//*   PONEMOS "REPLACE" AL FINAL                           *
//*D318.TQPIPLEV PART XX REPLACE                           *
//*D318.TQPIPLEV PART YY REPLACE                           *
//*D318.TQPIPLEV PART ZZ                                   *
//*                                                        *
//* - EN TODOS LOS REGISTROS MENOS EL PRIMERO              *
//*   PONEMOS "INTO TABLE " AL PRINCIPIO                   *
//*   (ESTE SERA EL FICHERO PARA LA LOAD DUMMY)            *
//*D318.TQPIPLEV PART XX REPLACE                           *
//*INTO TABLE D318.TQPIPLEV PART YY REPLACE                *
//*INTO TABLE D318.TQPIPLEV PART ZZ                        *
//*                                                        *
//**********************************************************
//SORT0002 EXEC PROC=EXPRP01D,COND=(4,EQ,DSTEST01.P20),
//         PRG=ICETOOL
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//IN       DD DISP=SHR,DSN=EBDQPIP.APLXD00X.TQPIPLEV(+1)
//OUT      DD DSN=&&TEMP1,DISP=(,PASS)
//OUT1     DD DSN=&&TEMP2,DISP=(,PASS)
//OUT2     DD DSN=EBDQPIP.INPXD05X.QPIPRLEV.SYSLOAD,
//            DISP=(,CATLG,DELETE),
//            SPACE=(CYL,(1,1),RLSE),
//            RECFM=FB,LRECL=80,BLKSIZE=0
//TOOLIN   DD *
COPY     FROM(IN)    TO(OUT)        USING(CTL1)
DATASORT FROM(OUT)   TO(OUT1) LAST  USING(CTL2)
DATASORT FROM(OUT1)  TO(OUT2) FIRST USING(CTL3)
/*
//* PONEMOS "D318.TQPILEV PART " POR DELANTE
//* DE LAS PARTICIONES PASADAS DE BINARIO A DISPLAY
//* Y ELIMINAMOS DUPLICADOS
//CTL1CNTL DD *
 INREC FIELDS=(5,2)
 SORT FIELDS=(1,2,CH,A)
 OUTREC FIELDS=(C'D318.TQPIPLEV PART ',1,2,59X)
 SUM FIELDS=NONE
/*
//* EN TODOS LOS REGISTROS MENOS EL ULTIMO
//* PONEMOS "REPLACE" DETRAS
//CTL2CNTL DD *
 INREC OVERLAY=(23:C'REPLACE')
 SORT FIELDS=(61,1,CH,A)
/*
//* EN TODOS LOS REGISTROS MENOS EL PRIMERO
//* PONEMOS "INTO TABLE " DELANTE
//CTL3CNTL DD *
 INREC OVERLAY=(12:1,29,
                1:C'INTO TABLE ')
 SORT FIELDS=(61,1,CH,A)
/*
//**********************************************************
//* LOAD DUMMY PARTICIONES GUARDADAS                       *
//**********************************************************
//LDTIPLEV EXEC PROC=EXPRP47D,COND=(4,EQ,DSTEST01.P20),
//         SSID='DS0D',JOB='EPIPDLEV'
//P47.SYSIN1 DD DSN=EBDQPIP.INPXD05X.QPIPRLEV.SYSLOAD,
//            DISP=(OLD,DELETE,KEEP)