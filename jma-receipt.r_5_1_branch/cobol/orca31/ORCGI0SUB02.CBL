      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGI0SUB02.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡ÅÐÏ¿
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¼ýÇ¼¾ðÊóÊÔ½¸¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  06/11/20    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-ÂÀÅÄ    07.05.15  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *****************************************************************
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYUNOU              PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-TUKISU              PIC 9(03).
           03  CNT-SYU                 PIC 9(03).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX-SYU                 PIC 9(05).
           03  IDX-MOT                 PIC 9(05).
           03  IDX-KON                 PIC 9(05).
           03  IDX-UPD                 PIC 9(05).
           03  IDX-SRT                 PIC 9(05).
           03  IDX-SUM                 PIC 9(05).
           03  IDX-HKN                 PIC 9(05).
           03  IDX-JIHI                PIC 9(05).
           03  IDX-COMB                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SYMD                PIC X(08).
           03  WRK-EDTYMD1             PIC X(10).
           03  WRK-EDTYMD3             PIC X(22).
           03  WRK-NYUFTN-MAX          PIC 9(05).
           03  WRK-STIDX               PIC 9(05).
           03  WRK-EDIDX               PIC 9(05).
           03  WRK-SKYSTYM             PIC X(06).
           03  WRK-HKNCOMBINUM         PIC X(04).
           03  WRK-HKNCOMBI            PIC X(100).
           03  WRK-SAGAKU              PIC S9(09).
           03  WRK-MOT-SKYMONEY        PIC S9(09).

      *
       01  MOTSYU-REC.
         02  MOTSYU-MAX                PIC 9(03).
         02  MOTSYU-OCC                OCCURS  10.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-//    BY  //MOTSYU-//.
         02  MOTSYU-OCC2               OCCURS  10.
           03  MOTSYU-CHKFLG           PIC 9(01).
      *
       01  KONSYU-REC.
         02  KONSYU-MAX                PIC 9(03).
         02  KONSYU-OCC                OCCURS  21.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-//    BY  //KONSYU-//.
         02  KONSYU-OCC2               OCCURS  21.
           03  KONSYU-UPD-SKYMONEY     PIC S9(08).
           03  KONSYU-UPD-TAX-SAI      PIC S9(08).
      *
           03  KONSYU-SRT-KEY.
             05  KONSYU-SRT-SRYYM      PIC X(08).
             05  KONSYU-SRT-DELFLG     PIC 9(01).
           03  KONSYU-UPDFLG           PIC 9(01).
      *
       01  KONSDAY-REC.
         02  KONSDAY-OCC               OCCURS  21.
           COPY    "CPSYUDAY.INC"      REPLACING
                                       //SDAY-//    BY  //KONSDAY-//.
      *
       01  CONST-AREA.
           03  CONST-RETSYU-MAX        PIC 9(03)   VALUE 10.
           03  CONST-MOTSYU-MAX        PIC 9(03)   VALUE 10.
           03  CONST-KONSYU-MAX        PIC 9(03)   VALUE 20.
           03  CONST-SYUNOU-MAX        PIC 9(03)   VALUE 23.
           03  CONST-HKNTEN-MAX        PIC 9(03)   VALUE 17.
           03  CONST-JIHI-MAX          PIC 9(03)   VALUE 10.
           03  CONST-ORCSHKN-MAX       PIC 9(03)   VALUE 100.
           03  CONST-H180401           PIC X(08)   VALUE "20060401".
      *
           COPY    "COMMON-SPA"        REPLACING
                                       //SPA-//    BY  //WRK-SPA-//.
      *    ¼ýÇ¼¹ç·×¥ï¡¼¥¯
       01  TTLSYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //TTLSYU-//.
           03  TTLSYU-UPD-SKYMONEY     PIC S9(08).
           03  TTLSYU-UPD-TAX-SAI      PIC S9(08).
      *
      *    ¼ýÇ¼½¸·×¥ï¡¼¥¯
       01  SUMSYUNOU-REC.
         02  SUMSYU-OCC                OCCURS  23.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //SUMSYU-//.
         02  SUMSYU-OCC2               OCCURS  23.
           03  SUMSYU-UPD-SKYMONEY     PIC S9(08).
           03  SUMSYU-UPD-TAX-SAI      PIC S9(08).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ÉéÃ´¶â·×»»¥µ¥Ö
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
           COPY    "CPORCSHKNSET.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
      *    Ê¬ÊÚÆþ±¡¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY  "CPORCSBUNBENCHK.INC".
      *
      *    Æþ±¡ÀÁµá´ü´Ö¼èÆÀ¥µ¥Ö
           COPY    "CPORCSS006.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPI0SUB02.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           I0SUB02-LNK.
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( I0SUB02-RC  =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       CNT-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       MOTSYU-REC
                                       KONSYU-REC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-ORCSNYUFTN-CALL-SEC
      *
           IF    ( I0SUB02-RC      =   ZERO )
               IF    ( I0SUB02-SYORIFLG    =   1 )
                   PERFORM 2001-MOTSYU-SEL-SEC
               END-IF
           END-IF
      *
           IF    ( I0SUB02-RC      =   ZERO )
               PERFORM 2001-ORCSHKNSET-CALL-SEC
           END-IF
      *
           IF    ( I0SUB02-RC      =   ZERO )
               PERFORM 2001-KONSYU-HEN-SEC
           END-IF
      *
           IF    ( I0SUB02-RC      =   ZERO )
               PERFORM 2001-I0SUB02-SYUNOU-HEN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ÉéÃ´¶â·×»»½èÍý
      *****************************************************************
       2001-ORCSNYUFTN-CALL-SEC        SECTION.
      *
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
                                       WRK-NYUFTN-MAX
      *
           INITIALIZE                  SBUNBENCHK-AREA
           MOVE    SPA-PTID        TO  SBUNBENCHK-PTID
           MOVE    I0SUB02-RRKNUM  TO  SBUNBENCHK-RRKNUM
           CALL    "ORCSBUNBENCHK" USING
                                   SBUNBENCHK-AREA
                                   SPA-AREA
      *
           INITIALIZE                  SS006-AREA
           MOVE    I0SUB02-SKYSTYMD    TO  SS006-SKYSTYMD
           MOVE    I0SUB02-SKYEDYMD    TO  SS006-SKYEDYMD
           MOVE    "1"                 TO  SS006-SKYKBN
           CALL    "ORCSS006"      USING
                                   SS006-AREA
                                   SPA-AREA
      *
           MOVE    SPA-HOSPNUM      TO  LNK-S02-HOSPNUM
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    SPA-PTID        TO  LNK-S02-PTID
           MOVE    I0SUB02-NISSU   TO  LNK-S02-NYUIN-TSUSAN-NISSU
      *
      *    ÀÁµá³«»ÏÆü¡ÊÆþ±¡Æü¡ÊÆ±ÆüºÆÆþ±¡¤Î¾ì¹ç¤ÏÆþ±¡ÆüÍâÆü¡Ë¤Þ¤¿¤Ï
      *    Á°²óÄê´üÀÁµá½ªÎ»Æü¤ÎÍâÆü¡Ë¤òÀßÄê
           MOVE    I0SUB02-SKYSTYMD TO LNK-S02-SKYSTYMD
           MOVE    I0SUB02-SKYEDYMD TO LNK-S02-SKYEDYMD
           IF    ( I0SUB02-SKYSTYMD <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ST  TO  LNK-S02-DOJITUFLG-ST
               MOVE    SS006-TIMEKBN-ST    TO  LNK-S02-TIMEKBN-ST
           END-IF
           IF    ( I0SUB02-SKYEDYMD <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ED  TO  LNK-S02-DOJITUFLG-ED
               MOVE    SS006-TIMEKBN-ED    TO  LNK-S02-TIMEKBN-ED
           END-IF
      *
           MOVE    SPA-SYSYMD      TO  LNK-S02-SYSYMD
           IF    ( SBUNBENCHK-BUNBEN   =   ZERO )
               MOVE    "1"         TO  LNK-S02-SYORI-KBN
           ELSE
               MOVE    "8"         TO  LNK-S02-SYORI-KBN
           END-IF
           CALL    "ORCSNYUFTN"    USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
                                   LNK-SYUDAY-REC-T
           IF    ( LNK-S02-RC      = ZERO )
      *
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU >   CONST-RETSYU-MAX )
                        OR   ( LNK-SYU-HOSPNUM (IDX-SYU)    = ZERO )
      *
                   COMPUTE WRK-NYUFTN-MAX  =   WRK-NYUFTN-MAX  +   1
      *
                   DISPLAY IDX-SYU ":LNK-SYU-SKYMONEY = "
                           LNK-SYU-SKYMONEY (IDX-SYU)
      *
               END-PERFORM
           ELSE
               MOVE    1               TO  I0SUB02-RC
               MOVE    LNK-S02-RC      TO  I0SUB02-S02-RC
           END-IF
      *
           .
       2001-ORCSNYUFTN-CALL-EXT.
           EXIT.
      *****************************************************************
      *    ÄûÀµ¸µ¼ýÇ¼¹¹¿·½èÍý
      *****************************************************************
       2001-MOTSYU-SEL-SEC             SECTION.
      *
           PERFORM 900-SYUNOU-KEY57-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( I0SUB02-RC  NOT =   ZERO )
      *
               COMPUTE MOTSYU-MAX  =   MOTSYU-MAX  +   1
      *
               MOVE    SYUNOU-REC  TO  MOTSYU-OCC (MOTSYU-MAX)
      *
               IF    ( MOTSYU-MAX      >=  CONST-MOTSYU-MAX )
                   MOVE    2       TO  I0SUB02-RC
               END-IF
      *
               PERFORM 900-SYUNOU-KEY57-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-MOTSYU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    º£²ó¼ýÇ¼ÊÔ½¸½èÍý
      *****************************************************************
       2001-KONSYU-HEN-SEC             SECTION.
      *
           MOVE    WRK-NYUFTN-MAX  TO  KONSYU-MAX
      *
           PERFORM VARYING IDX-SYU FROM    1   BY  1
                   UNTIL ( IDX-SYU >   WRK-NYUFTN-MAX )
      *
               MOVE    LNK-SYUNOU-REC (IDX-SYU)
                                   TO  KONSYU-OCC   (IDX-SYU)
      *
               MOVE    LNK-SYUDAY-REC (IDX-SYU)
                                   TO  KONSDAY-OCC  (IDX-SYU)
      *
               MOVE    LNK-SYU-SRYYMD (IDX-SYU)(1:6)
                                   TO  KONSYU-SRT-SRYYM (IDX-SYU)
      *
               MOVE    ZERO            TO  IDX-UPD
               PERFORM VARYING IDX-MOT FROM    1   BY  1
                       UNTIL ( IDX-MOT >   MOTSYU-MAX )
                        OR   ( IDX-UPD  NOT =   ZERO )
      *
                   IF   ( LNK-SYU-SRYYMD  (IDX-SYU)(1:6)
                                       =   MOTSYU-SRYYMD(IDX-MOT)(1:6))
                    AND ( LNK-SYU-HKNCOMBINUM (IDX-SYU)
                                       =   MOTSYU-HKNCOMBINUM (IDX-MOT))
                       MOVE    1       TO  MOTSYU-CHKFLG (IDX-MOT)
                       MOVE    IDX-MOT TO  IDX-UPD
                   END-IF
               END-PERFORM
      *
               IF    ( IDX-UPD         =   ZERO )
                   MOVE    1           TO  KONSYU-UPDFLG (IDX-SYU)
                   COMPUTE KONSYU-UPD-SKYMONEY (IDX-SYU)
                       =   KONSYU-SKYMONEY (IDX-SYU)
                   COMPUTE KONSYU-UPD-TAX-SAI (IDX-SYU)
                       =   KONSYU-SKYMONEY-TAX-SAI (IDX-SYU)
               ELSE
      *
                   MOVE    ZERO        TO  KONSYU-UPDFLG (IDX-SYU)
      *
                   COMPUTE WRK-SAGAKU
                       =   MOTSYU-SKYMONEY(IDX-UPD)
                       -   MOTSYU-CHOSEI  (IDX-UPD)
                       -   KONSYU-SKYMONEY(IDX-SYU)
      *
                   IF    ( WRK-SAGAKU      >   ZERO )
                       IF    ( WRK-SAGAKU  >   MOTSYU-SKYMONEY(IDX-UPD))
                           COMPUTE KONSYU-CHOSEI(IDX-SYU)
                               =   KONSYU-SKYMONEY(IDX-SYU)
                               *   -1
                           MOVE    ZERO    TO  KONSYU-SKYMONEY(IDX-SYU)
      *
                           IF    ( MOTSYU-CHOSEI2(IDX-UPD)
                                           =   ZERO )
                               COMPUTE KONSYU-CHOSEI2(IDX-SYU)
                                   =   MOTSYU-CHOSEI2(IDX-UPD)
      *
                               COMPUTE KONSYU-CHOSEI1(IDX-SYU)
                                   =   MOTSYU-CHOSEI1(IDX-UPD)
                                   -   MOTSYU-CHOSEI(IDX-UPD)
                                   +   KONSYU-CHOSEI(IDX-SYU)
                           ELSE
                               COMPUTE KONSYU-CHOSEI1(IDX-SYU)
                                   =   MOTSYU-CHOSEI1(IDX-UPD)
      *
                               COMPUTE KONSYU-CHOSEI2(IDX-SYU)
                                   =   MOTSYU-CHOSEI2(IDX-UPD)
                                   -   MOTSYU-CHOSEI(IDX-UPD)
                                   +   KONSYU-CHOSEI(IDX-SYU)
                           END-IF
      *
      *
                       ELSE
                           COMPUTE KONSYU-SKYMONEY(IDX-SYU)
                               =   MOTSYU-SKYMONEY(IDX-UPD)
                               -   WRK-SAGAKU
      *
                           MOVE    MOTSYU-CHOSEI(IDX-UPD)
                                           TO  KONSYU-CHOSEI(IDX-SYU)
                           MOVE    MOTSYU-CHOSEI1(IDX-UPD)
                                           TO  KONSYU-CHOSEI1(IDX-SYU)
                           MOVE    MOTSYU-CHOSEI2(IDX-UPD)
                                           TO  KONSYU-CHOSEI2(IDX-SYU)
                       END-IF
                   ELSE
      *
                       COMPUTE KONSYU-SKYMONEY(IDX-SYU)
                           =   KONSYU-SKYMONEY(IDX-SYU)
                           +   MOTSYU-CHOSEI(IDX-UPD)
      *
                       MOVE    MOTSYU-CHOSEI(IDX-UPD)
                                           TO  KONSYU-CHOSEI(IDX-SYU)
                       MOVE    MOTSYU-CHOSEI1(IDX-UPD)
                                           TO  KONSYU-CHOSEI1(IDX-SYU)
                       MOVE    MOTSYU-CHOSEI2(IDX-UPD)
                                           TO  KONSYU-CHOSEI2(IDX-SYU)
      *
                   END-IF
      *
                   MOVE    MOTSYU-DENPNUM (IDX-UPD)
                                           TO  KONSYU-DENPNUM (IDX-SYU)
      *
                   MOVE    MOTSYU-CREYMD  (IDX-UPD)
                                           TO  KONSYU-CREYMD  (IDX-SYU)
      *
                   COMPUTE KONSYU-NYUKIN-KAISU (IDX-SYU)
                       =   MOTSYU-NYUKIN-KAISU (IDX-UPD)
      *
                   COMPUTE KONSYU-NYUKIN-TOTAL (IDX-SYU)
                       =   MOTSYU-NYUKIN-TOTAL (IDX-UPD)
      *
                   COMPUTE KONSYU-UPD-SKYMONEY (IDX-SYU)
                       =   KONSYU-SKYMONEY (IDX-SYU)
                       -   MOTSYU-SKYMONEY (IDX-UPD)
      *
                   COMPUTE KONSYU-UPD-TAX-SAI (IDX-SYU)
                       =   KONSYU-SKYMONEY-TAX-SAI (IDX-SYU)
                       -   MOTSYU-SKYMONEY-TAX-SAI (IDX-UPD)
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-MOT FROM    1   BY  1
                   UNTIL ( IDX-MOT     >   MOTSYU-MAX )
                    OR   ( I0SUB02-RC  NOT =   ZERO )
      *
               IF    ( MOTSYU-CHKFLG (IDX-MOT) =   ZERO )
                   IF    ( KONSYU-MAX  >=  CONST-KONSYU-MAX )
                       MOVE    3       TO  I0SUB02-RC
                   ELSE
                       COMPUTE KONSYU-MAX  =   KONSYU-MAX  +   1
                       MOVE    MOTSYU-OCC (IDX-MOT)
                                       TO  KONSYU-OCC    (KONSYU-MAX)
                       MOVE    MOTSYU-SRYYMD (IDX-MOT)(1:6)
                                       TO  KONSYU-SRT-SRYYM (KONSYU-MAX)
                       MOVE    1       TO  KONSYU-SRT-DELFLG(KONSYU-MAX)
                       COMPUTE KONSYU-UPD-SKYMONEY (IDX-SYU)
                           =   KONSYU-SKYMONEY (IDX-SYU)
                           *   -1
                       COMPUTE KONSYU-UPD-TAX-SAI (IDX-SYU)
                           =   KONSYU-SKYMONEY-TAX-SAI (IDX-SYU)
                           *   -1
                       MOVE    2       TO  KONSYU-UPDFLG (KONSYU-MAX)
                   END-IF
               END-IF
           END-PERFORM
      *
           COMPUTE IDX-SRT     =   CONST-KONSYU-MAX    +   1
           COMPUTE WRK-EDIDX   =   KONSYU-MAX  -   1
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WRK-EDIDX )
                    OR   ( I0SUB02-RC  NOT =   ZERO )
      *
               COMPUTE WRK-STIDX   =   IDX1    +   1
               PERFORM VARYING IDX2    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX2    >   KONSYU-MAX )
      *
                   IF    ( KONSYU-SRT-KEY (IDX1)
                                   >   KONSYU-SRT-KEY (IDX2))
                       MOVE    KONSYU-OCC (IDX1)
                                   TO  KONSYU-OCC (IDX-SRT)
                       MOVE    KONSYU-OCC2 (IDX1)
                                   TO  KONSYU-OCC2 (IDX-SRT)
                       MOVE    KONSDAY-OCC (IDX1)
                                   TO  KONSDAY-OCC (IDX-SRT)
      *
                       MOVE    KONSYU-OCC (IDX2)
                                   TO  KONSYU-OCC (IDX1)
                       MOVE    KONSYU-OCC2 (IDX2)
                                   TO  KONSYU-OCC2 (IDX1)
                       MOVE    KONSDAY-OCC (IDX2)
                                   TO  KONSDAY-OCC (IDX1)
      *
                       MOVE    KONSYU-OCC (IDX-SRT)
                                   TO  KONSYU-OCC (IDX2)
                       MOVE    KONSYU-OCC2 (IDX-SRT)
                                   TO  KONSYU-OCC2 (IDX2)
                       MOVE    KONSDAY-OCC (IDX-SRT)
                                   TO  KONSDAY-OCC (IDX2)
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       2001-KONSYU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ÊÖµÑ¼ýÇ¼ÊÔ½¸½èÍý
      *****************************************************************
       2001-I0SUB02-SYUNOU-HEN-SEC     SECTION.
      *
           INITIALIZE                  I0SUB02-SYUNOU-TBL
           MOVE    LOW-VALUE       TO  WRK-SKYSTYM
           INITIALIZE                  TTLSYUNOU-REC
                                       SUMSYUNOU-REC
                                       CNT-TUKISU
                                       CNT-SYU
      *
           PERFORM VARYING IDX-KON   FROM    1   BY  1
                   UNTIL ( IDX-KON           >   KONSYU-MAX )
                   OR    ( CNT-SYU    >=  CONST-SYUNOU-MAX )
      *
               IF    ( KONSYU-SKYSTYMD (IDX-KON) (1 : 6)
                                       NOT =   WRK-SKYSTYM )
      *
                   MOVE    KONSYU-SKYSTYMD(IDX-KON) (1 : 6)
                                       TO      WRK-SKYSTYM
                   COMPUTE CNT-SYU     =   CNT-SYU +   1
                   MOVE    CNT-SYU     TO  IDX-SUM
      *
                   MOVE    1           TO  I0SUB02-SYUNOU-TTLDAT-FLG
                                                         (IDX-SUM)
      *
                   MOVE    KONSYU-SKYSTYMD (IDX-KON)
                                       TO   WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD1 (1 : 6)
                                       TO  I0SUB02-SYUNOU-YM-W (IDX-SUM)
                   MOVE    WRK-SYMD    (1 : 6)
                                       TO  I0SUB02-SYUNOU-YM-S (IDX-SUM)
      *
                   MOVE    ZERO        TO  I0SUB02-SYUNOU-HKNCOMBI-SU
                                                               (IDX-SUM)
      *
                   IF    ( KONSYU-UPDFLG (IDX-KON)
                                   NOT =   2 )
                       COMPUTE CNT-TUKISU  =   CNT-TUKISU  +   1
                   END-IF
      *
               END-IF
      *
               IF    ( CNT-SYU <   CONST-SYUNOU-MAX )
      *
                   COMPUTE CNT-SYU     =   CNT-SYU +   1
      *
                   IF    ( KONSYU-UPDFLG (IDX-KON) =   2 )
                       MOVE    3       TO  I0SUB02-SYUNOU-TTLDAT-FLG
                                                           (CNT-SYU)
                   ELSE
                       MOVE    0       TO  I0SUB02-SYUNOU-TTLDAT-FLG
                                                           (CNT-SYU)
                   END-IF
      *
                   MOVE    KONSYU-UPDFLG (IDX-KON)
                                       TO  I0SUB02-SYUNOU-UPDFLG
                                                           (CNT-SYU)
      *
                   MOVE    I0SUB02-SYUNOU-YM-W (IDX-SUM)
                                       TO  I0SUB02-SYUNOU-YM-W
                                                 (CNT-SYU)
                   MOVE    I0SUB02-SYUNOU-YM-S (IDX-SUM)
                                       TO  I0SUB02-SYUNOU-YM-S
                                                 (CNT-SYU)
      *
      *            ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»Ì¾¾Î¼èÆÀ
                   MOVE    KONSYU-HKNCOMBINUM (IDX-KON)
                                       TO  WRK-HKNCOMBINUM
                   PERFORM 20011-HKNCOMBI-EDIT-SEC
                   MOVE    WRK-HKNCOMBI
                                       TO  I0SUB02-SYUNOU-HKNCOMBI
                                                 (CNT-SYU)
      *
                   IF    ( I0SUB02-SYUNOU-UPDFLG (CNT-SYU)
                                   NOT =   2 )
                       COMPUTE I0SUB02-SYUNOU-HKNCOMBI-SU (IDX-SUM)
                           =   I0SUB02-SYUNOU-HKNCOMBI-SU (IDX-SUM)
                           +   1
                   END-IF
      *
                   MOVE    KONSYU-OCC (IDX-KON)
                                       TO  SUMSYU-OCC(CNT-SYU)
                   MOVE    KONSYU-UPD-SKYMONEY (IDX-KON)
                                       TO  SUMSYU-UPD-SKYMONEY (CNT-SYU)
                   MOVE    KONSYU-UPD-TAX-SAI (IDX-KON)
                                       TO  SUMSYU-UPD-TAX-SAI  (CNT-SYU)
                   MOVE    KONSDAY-OCC(IDX-KON)
                                       TO  I0SUB02-SYUDAY-DAT
                                                        (CNT-SYU)
                   IF    ( I0SUB02-SYUNOU-UPDFLG (CNT-SYU)
                                   NOT =   2 )
                       PERFORM 20011-I0SUB02-SYUNOU-HEN-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    ¥¼¥í·ï¤Î¾ì¹ç¤Ï¥¨¥é¡¼¤È¤¹¤ë
           IF    ( CNT-SYU         <=  ZERO )
               MOVE    4           TO  I0SUB02-RC
           END-IF
      *
           IF    ( I0SUB02-RC      =   ZERO )
               COMPUTE CNT-SYU     =   CNT-SYU +   1
      *
      *        ¥Æ¡¼¥Ö¥ëºÇÂç·ï¿ô¥ª¡¼¥Ð¡¼»þ¤Ï¥¨¥é¡¼¤È¤¹¤ë
               IF    ( CNT-SYU     >   CONST-SYUNOU-MAX )
                   MOVE    5       TO  I0SUB02-RC
               END-IF
           END-IF
      *
           IF    ( I0SUB02-RC      =   ZERO )
               MOVE    2               TO  I0SUB02-SYUNOU-TTLDAT-FLG
                                                       (CNT-SYU)
               MOVE    "999999"        TO  I0SUB02-SYUNOU-YM-S
                                                       (CNT-SYU)
               MOVE    "999999"        TO  I0SUB02-SYUNOU-YM-W
                                                       (CNT-SYU)
               MOVE    CNT-TUKISU      TO  I0SUB02-SYUNOU-TUKI-SU
                                                       (CNT-SYU)
               MOVE    TTLSYUNOU-REC   TO  SUMSYU-OCC (CNT-SYU)
               MOVE    TTLSYU-UPD-SKYMONEY
                                       TO  SUMSYU-UPD-SKYMONEY
                                                       (CNT-SYU)
               MOVE    TTLSYU-UPD-TAX-SAI
                                       TO  SUMSYU-UPD-TAX-SAI
                                                       (CNT-SYU)
      *
               MOVE    CNT-SYU         TO  I0SUB02-SYUNOU-CNT
      *
               PERFORM VARYING IDX-SYU   FROM    1   BY  1
                       UNTIL ( IDX-SYU >   I0SUB02-SYUNOU-CNT )
      *
                   MOVE    SUMSYU-OCC (IDX-SYU)
                                       TO  I0SUB02-SYUNOU-DAT (IDX-SYU)
                   MOVE    SUMSYU-UPD-SKYMONEY(IDX-SYU)
                                       TO  I0SUB02-UPD-SKYMONEY(IDX-SYU)
                   MOVE    SUMSYU-UPD-TAX-SAI (IDX-SYU)
                                       TO  I0SUB02-UPD-TAX-SAI (IDX-SYU)
      *
               END-PERFORM
           END-IF
      *
           .
       2001-I0SUB02-SYUNOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ÊÖµÑ¼ýÇ¼ÊÔ½¸½èÍý
      *****************************************************************
       20011-I0SUB02-SYUNOU-HEN-SEC    SECTION.
      *

      *    ÊÝ¸±ÎÁ
           PERFORM VARYING     IDX-HKN   FROM    1   BY  1
                   UNTIL     ( IDX-HKN   >   CONST-HKNTEN-MAX )
      *
               COMPUTE SUMSYU-HKNTEN (IDX-SUM IDX-HKN)
                   =   SUMSYU-HKNTEN (IDX-SUM IDX-HKN)
                   +   KONSYU-HKNTEN (IDX-KON IDX-HKN)
               COMPUTE TTLSYU-HKNTEN (IDX-HKN)
                   =   TTLSYU-HKNTEN (IDX-HKN)
                   +   KONSYU-HKNTEN (IDX-KON IDX-HKN)
      *
               COMPUTE SUMSYU-TGMONEY(IDX-SUM IDX-HKN)
                   =   SUMSYU-TGMONEY(IDX-SUM IDX-HKN)
                   +   KONSYU-TGMONEY(IDX-KON IDX-HKN)
               COMPUTE TTLSYU-TGMONEY(IDX-HKN)
                   =   TTLSYU-TGMONEY(IDX-HKN)
                   +   KONSYU-TGMONEY(IDX-KON IDX-HKN)
      *
               COMPUTE SUMSYU-TGMONEY-TAX(IDX-SUM IDX-HKN)
                   =   SUMSYU-TGMONEY-TAX(IDX-SUM IDX-HKN)
                   +   KONSYU-TGMONEY-TAX(IDX-KON IDX-HKN)
               COMPUTE TTLSYU-TGMONEY-TAX(IDX-HKN)
                   =   TTLSYU-TGMONEY-TAX(IDX-HKN)
                   +   KONSYU-TGMONEY-TAX(IDX-KON IDX-HKN)
           END-PERFORM
      *
      *    ÊÝ¸±Å¬ÍÑ¶â³Û
           COMPUTE SUMSYU-HKNTEKMONEY (IDX-SUM)
               =   SUMSYU-HKNTEKMONEY (IDX-SUM)
               +   KONSYU-HKNTEKMONEY (IDX-KON)
           COMPUTE TTLSYU-HKNTEKMONEY
               =   TTLSYU-HKNTEKMONEY
               +   KONSYU-HKNTEKMONEY (IDX-KON)
      *
           IF    ( KONSYU-HKNTEKKBN  (IDX-KON) =   "1" )
               COMPUTE SUMSYU-HKNTEKMONEY (IDX-SUM)
                   =   SUMSYU-HKNTEKMONEY (IDX-SUM)
                   +   KONSYU-KOH-FTN  (IDX-KON)
               COMPUTE TTLSYU-HKNTEKMONEY
                   =   TTLSYU-HKNTEKMONEY
                   +   KONSYU-KOH-FTN  (IDX-KON)
           END-IF
      *
      *    Ï·¿Í°ìÉôÉéÃ´¶â
           COMPUTE SUMSYU-RJN-FTN  (IDX-SUM)
               =   SUMSYU-RJN-FTN  (IDX-SUM)
               +   KONSYU-RJN-FTN  (IDX-KON)
           COMPUTE TTLSYU-RJN-FTN
               =   TTLSYU-RJN-FTN
               +   KONSYU-RJN-FTN  (IDX-KON)
      *
      *    ¸øÈñ°ìÉôÉéÃ´¶â
           IF    ( KONSYU-HKNTEKKBN  (IDX-KON) =   SPACE )
               COMPUTE SUMSYU-KOH-FTN  (IDX-SUM)
                   =   SUMSYU-KOH-FTN  (IDX-SUM)
                   +   KONSYU-KOH-FTN  (IDX-KON)
               COMPUTE TTLSYU-KOH-FTN
                   =   TTLSYU-KOH-FTN
                   +   KONSYU-KOH-FTN  (IDX-KON)
           END-IF
      *
      *    ¼«Èñ
           PERFORM VARYING IDX-JIHI    FROM    1   BY  1
                   UNTIL ( IDX-JIHI    >   CONST-JIHI-MAX )
      *
               COMPUTE SUMSYU-JIHI-TAXNASI (IDX-SUM IDX-JIHI)
                   =   SUMSYU-JIHI-TAXNASI (IDX-SUM IDX-JIHI)
                   +   KONSYU-JIHI-TAXNASI (IDX-KON IDX-JIHI)
               COMPUTE TTLSYU-JIHI-TAXNASI (IDX-JIHI)
                   =   TTLSYU-JIHI-TAXNASI (IDX-JIHI)
                   +   KONSYU-JIHI-TAXNASI (IDX-KON IDX-JIHI)
      *
               COMPUTE SUMSYU-JIHI-TAXARI (IDX-SUM IDX-JIHI)
                   =   SUMSYU-JIHI-TAXARI (IDX-SUM IDX-JIHI)
                   +   KONSYU-JIHI-TAXARI (IDX-KON IDX-JIHI)
               COMPUTE TTLSYU-JIHI-TAXARI (IDX-JIHI)
                   =   TTLSYU-JIHI-TAXARI (IDX-JIHI)
                   +   KONSYU-JIHI-TAXARI (IDX-KON IDX-JIHI)
           END-PERFORM
      *
      *    ¼«Èñ·×
           COMPUTE SUMSYU-JIHI-TOTAL (IDX-SUM)
               =   SUMSYU-JIHI-TOTAL (IDX-SUM)
               +   KONSYU-JIHI-TOTAL (IDX-KON)
           COMPUTE TTLSYU-JIHI-TOTAL
               =   TTLSYU-JIHI-TOTAL
               +   KONSYU-JIHI-TOTAL (IDX-KON)
      *
      *
      *    ¼«Èñ·×¡Ê¾ÃÈñÀÇ¤¢¤ê¡Ë
           COMPUTE SUMSYU-JIHI-TOTAL-TAX (IDX-SUM)
               =   SUMSYU-JIHI-TOTAL-TAX (IDX-SUM)
               +   KONSYU-JIHI-TOTAL-TAX (IDX-KON)
           COMPUTE TTLSYU-JIHI-TOTAL-TAX
               =   TTLSYU-JIHI-TOTAL-TAX
               +   KONSYU-JIHI-TOTAL-TAX (IDX-KON)
      *
      *    ¼«Èñ¾ÃÈñÀÇ
           COMPUTE SUMSYU-JIHI-TAX  (IDX-SUM)
               =   SUMSYU-JIHI-TAX  (IDX-SUM)
               +   KONSYU-JIHI-TAX  (IDX-KON)
           COMPUTE TTLSYU-JIHI-TAX
               =   TTLSYU-JIHI-TAX
               +   KONSYU-JIHI-TAX  (IDX-KON)
      *
      *    Ï«ºÒ½é¿Ç
           COMPUTE SUMSYU-RSISHOSHIN-MONEY (IDX-SUM)
               =   SUMSYU-RSISHOSHIN-MONEY (IDX-SUM)
               +   KONSYU-RSISHOSHIN-MONEY (IDX-KON)
           COMPUTE TTLSYU-RSISHOSHIN-MONEY
               =   TTLSYU-RSISHOSHIN-MONEY
               +   KONSYU-RSISHOSHIN-MONEY (IDX-KON)
      *
      *    Ï«ºÒºÆ¿Ç
           COMPUTE SUMSYU-RSISAISHIN-MONEY (IDX-SUM)
               =   SUMSYU-RSISAISHIN-MONEY (IDX-SUM)
               +   KONSYU-RSISAISHIN-MONEY (IDX-KON)
           COMPUTE TTLSYU-RSISAISHIN-MONEY
               =   TTLSYU-RSISAISHIN-MONEY
               +   KONSYU-RSISAISHIN-MONEY (IDX-KON)
      *
      *    Ï«ºÒ»ØÆ³
           COMPUTE SUMSYU-RSISDO-MONEY  (IDX-SUM)
               =   SUMSYU-RSISDO-MONEY  (IDX-SUM)
               +   KONSYU-RSISDO-MONEY (IDX-KON)
           COMPUTE TTLSYU-RSISDO-MONEY
               =   TTLSYU-RSISDO-MONEY
               +   KONSYU-RSISDO-MONEY (IDX-KON)
      *
      *    Ï«ºÒ¤½¤ÎÂ¾
           COMPUTE SUMSYU-RSIETC-MONEY (IDX-SUM)
               =   SUMSYU-RSIETC-MONEY (IDX-SUM)
               +   KONSYU-RSIETC-MONEY (IDX-KON)
           COMPUTE TTLSYU-RSIETC-MONEY
               =   TTLSYU-RSIETC-MONEY
               +   KONSYU-RSIETC-MONEY (IDX-KON)
      *
      *    ¿©»öÎÅÍÜÈñ¡ÊÊÝ¸±¡Ë
           COMPUTE SUMSYU-RYOYOHI-SKJ (IDX-SUM)
               =   SUMSYU-RYOYOHI-SKJ (IDX-SUM)
               +   KONSYU-RYOYOHI-SKJ (IDX-KON)
           COMPUTE TTLSYU-RYOYOHI-SKJ
               =   TTLSYU-RYOYOHI-SKJ
               +   KONSYU-RYOYOHI-SKJ (IDX-KON)
      *
      *    ¿©»öÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE SUMSYU-SKYMONEY-SKJ (IDX-SUM)
               =   SUMSYU-SKYMONEY-SKJ (IDX-SUM)
               +   KONSYU-SKYMONEY-SKJ (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-SKJ
               =   TTLSYU-SKYMONEY-SKJ
               +   KONSYU-SKYMONEY-SKJ (IDX-KON)
      *
      *    ¿©»öÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE SUMSYU-SKYMONEY-SKJ-TAX (IDX-SUM)
               =   SUMSYU-SKYMONEY-SKJ-TAX (IDX-SUM)
               +   KONSYU-SKYMONEY-SKJ-TAX (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-SKJ-TAX
               =   TTLSYU-SKYMONEY-SKJ-TAX
               +   KONSYU-SKYMONEY-SKJ-TAX (IDX-KON)
      *
      *    ¿©»öÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE SUMSYU-SKYMONEY-SKJ-KEI (IDX-SUM)
               =   SUMSYU-SKYMONEY-SKJ-KEI (IDX-SUM)
               +   KONSYU-SKYMONEY-SKJ-KEI (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-SKJ-KEI
               =   TTLSYU-SKYMONEY-SKJ-KEI
               +   KONSYU-SKYMONEY-SKJ-KEI (IDX-KON)
      *
      *    ¿©»öÎÅÍÜÈñ¡Ê¼«Èñ¡Ë
           COMPUTE SUMSYU-RYOYOHI-SKJ-JIHI (IDX-SUM)
               =   SUMSYU-RYOYOHI-SKJ-JIHI (IDX-SUM)
               +   KONSYU-RYOYOHI-SKJ-JIHI (IDX-KON)
           COMPUTE TTLSYU-RYOYOHI-SKJ-JIHI
               =   TTLSYU-RYOYOHI-SKJ-JIHI
               +   KONSYU-RYOYOHI-SKJ-JIHI (IDX-KON)
      *
      *    ¿©»öÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI (IDX-SUM)
               =   SUMSYU-SKYMONEY-SKJ-JIHI (IDX-SUM)
               +   KONSYU-SKYMONEY-SKJ-JIHI (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI
               =   TTLSYU-SKYMONEY-SKJ-JIHI
               +   KONSYU-SKYMONEY-SKJ-JIHI (IDX-KON)
      *
      *    ¿©»öÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI-TAX(IDX-SUM)
               =   SUMSYU-SKYMONEY-SKJ-JIHI-TAX(IDX-SUM)
               +   KONSYU-SKYMONEY-SKJ-JIHI-TAX(IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-TAX
               =   TTLSYU-SKYMONEY-SKJ-JIHI-TAX
               +   KONSYU-SKYMONEY-SKJ-JIHI-TAX(IDX-KON)
      *
      *    ¿©»öÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI-KEI(IDX-SUM)
               =   SUMSYU-SKYMONEY-SKJ-JIHI-KEI(IDX-SUM)
               +   KONSYU-SKYMONEY-SKJ-JIHI-KEI(IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-KEI
               =   TTLSYU-SKYMONEY-SKJ-JIHI-KEI
               +   KONSYU-SKYMONEY-SKJ-JIHI-KEI(IDX-KON)
      *
      *    À¸³èÎÅÍÜÈñ¡ÊÊÝ¸±¡Ë
           COMPUTE SUMSYU-RYOYOHI-LIFE (IDX-SUM)
               =   SUMSYU-RYOYOHI-LIFE (IDX-SUM)
               +   KONSYU-RYOYOHI-LIFE (IDX-KON)
           COMPUTE TTLSYU-RYOYOHI-LIFE
               =   TTLSYU-RYOYOHI-LIFE
               +   KONSYU-RYOYOHI-LIFE (IDX-KON)
      *
      *    À¸³èÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE SUMSYU-SKYMONEY-LIFE (IDX-SUM)
               =   SUMSYU-SKYMONEY-LIFE (IDX-SUM)
               +   KONSYU-SKYMONEY-LIFE (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-LIFE
               =   TTLSYU-SKYMONEY-LIFE
               +   KONSYU-SKYMONEY-LIFE (IDX-KON)
      *
      *    À¸³èÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE SUMSYU-SKYMONEY-LIFE-TAX (IDX-SUM)
               =   SUMSYU-SKYMONEY-LIFE-TAX (IDX-SUM)
               +   KONSYU-SKYMONEY-LIFE-TAX (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-LIFE-TAX
               =   TTLSYU-SKYMONEY-LIFE-TAX
               +   KONSYU-SKYMONEY-LIFE-TAX (IDX-KON)
      *
      *    À¸³èÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE SUMSYU-SKYMONEY-LIFE-KEI (IDX-SUM)
               =   SUMSYU-SKYMONEY-LIFE-KEI (IDX-SUM)
               +   KONSYU-SKYMONEY-LIFE-KEI (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-LIFE-KEI
               =   TTLSYU-SKYMONEY-LIFE-KEI
               +   KONSYU-SKYMONEY-LIFE-KEI (IDX-KON)
      *
      *    À¸³èÎÅÍÜÈñ¡Ê¼«Èñ¡Ë
           COMPUTE SUMSYU-RYOYOHI-LIFE-JIHI (IDX-SUM)
               =   SUMSYU-RYOYOHI-LIFE-JIHI (IDX-SUM)
               +   KONSYU-RYOYOHI-LIFE-JIHI (IDX-KON)
           COMPUTE TTLSYU-RYOYOHI-LIFE-JIHI
               =   TTLSYU-RYOYOHI-LIFE-JIHI
               +   KONSYU-RYOYOHI-LIFE-JIHI (IDX-KON)
      *
      *    À¸³èÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE SUMSYU-SKYMONEY-LIFE-JIHI (IDX-SUM)
               =   SUMSYU-SKYMONEY-LIFE-JIHI (IDX-SUM)
               +   KONSYU-SKYMONEY-LIFE-JIHI (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-LIFE-JIHI
               =   TTLSYU-SKYMONEY-LIFE-JIHI
               +   KONSYU-SKYMONEY-LIFE-JIHI (IDX-KON)
      *
      *    À¸³èÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE SUMSYU-SKYMONEY-LIFE-JIHI-TAX(IDX-SUM)
               =   SUMSYU-SKYMONEY-LIFE-JIHI-TAX(IDX-SUM)
               +   KONSYU-SKYMONEY-LIFE-JIHI-TAX(IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-LIFE-JIHI-TAX
               =   TTLSYU-SKYMONEY-LIFE-JIHI-TAX
               +   KONSYU-SKYMONEY-LIFE-JIHI-TAX(IDX-KON)
      *
      *    À¸³èÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE SUMSYU-SKYMONEY-LIFE-JIHI-KEI(IDX-SUM)
               =   SUMSYU-SKYMONEY-LIFE-JIHI-KEI(IDX-SUM)
               +   KONSYU-SKYMONEY-LIFE-JIHI-KEI (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY-LIFE-JIHI-KEI
               =   TTLSYU-SKYMONEY-LIFE-JIHI-KEI
               +   KONSYU-SKYMONEY-LIFE-JIHI-KEI (IDX-KON)
      *
      *    ¼¼ÎÁº¹³Û
           COMPUTE SUMSYU-RMSAGAKU (IDX-SUM)
               =   SUMSYU-RMSAGAKU (IDX-SUM)
               +   KONSYU-RMSAGAKU (IDX-KON)
           COMPUTE TTLSYU-RMSAGAKU
               =   TTLSYU-RMSAGAKU
               +   KONSYU-RMSAGAKU (IDX-KON)
      *
      *    Ä´À°¶â
           COMPUTE SUMSYU-CHOSEI   (IDX-SUM)
               =   SUMSYU-CHOSEI   (IDX-SUM)
               +   KONSYU-CHOSEI   (IDX-KON)
           COMPUTE TTLSYU-CHOSEI
               =   TTLSYU-CHOSEI
               +   KONSYU-CHOSEI   (IDX-KON)
      *
           COMPUTE SUMSYU-CHOSEI1   (IDX-SUM)
               =   SUMSYU-CHOSEI1   (IDX-SUM)
               +   KONSYU-CHOSEI1   (IDX-KON)
           COMPUTE TTLSYU-CHOSEI1
               =   TTLSYU-CHOSEI1
               +   KONSYU-CHOSEI1   (IDX-KON)
      *
           COMPUTE SUMSYU-CHOSEI2   (IDX-SUM)
               =   SUMSYU-CHOSEI2   (IDX-SUM)
               +   KONSYU-CHOSEI2   (IDX-KON)
           COMPUTE TTLSYU-CHOSEI2
               =   TTLSYU-CHOSEI2
               +   KONSYU-CHOSEI2   (IDX-KON)
      *
      *    ÀÁµá¶â³Û
           COMPUTE SUMSYU-SKYMONEY (IDX-SUM)
               =   SUMSYU-SKYMONEY (IDX-SUM)
               +   KONSYU-SKYMONEY (IDX-KON)
           COMPUTE TTLSYU-SKYMONEY
               =   TTLSYU-SKYMONEY
               +   KONSYU-SKYMONEY (IDX-KON)
      *
      *    ÀÁµá¶â³Û
           COMPUTE SUMSYU-UPD-SKYMONEY (IDX-SUM)
               =   SUMSYU-UPD-SKYMONEY (IDX-SUM)
               +   KONSYU-UPD-SKYMONEY (IDX-KON)
           COMPUTE TTLSYU-UPD-SKYMONEY
               =   TTLSYU-UPD-SKYMONEY
               +   KONSYU-UPD-SKYMONEY (IDX-KON)
      *
      *    ¾ÃÈñÀÇºÆ·Ç
           COMPUTE SUMSYU-UPD-TAX-SAI (IDX-SUM)
               =   SUMSYU-UPD-TAX-SAI (IDX-SUM)
               +   KONSYU-UPD-TAX-SAI (IDX-KON)
           COMPUTE TTLSYU-UPD-TAX-SAI
               =   TTLSYU-UPD-TAX-SAI
               +   KONSYU-UPD-TAX-SAI (IDX-KON)
      *
      *    ¸ºÌÈ¾ðÊó
           IF    ( KONSYU-DISCOUNT-TEIRITU (IDX-KON)   =   "1" OR "2" )
               MOVE    KONSYU-DISCOUNT-KBN     (IDX-KON)
                                   TO  SUMSYU-DISCOUNT-KBN     (IDX-SUM)
                                       TTLSYU-DISCOUNT-KBN
               MOVE    KONSYU-DISCOUNT-BODY    (IDX-KON)
                                   TO  SUMSYU-DISCOUNT-BODY    (IDX-SUM)
                                       TTLSYU-DISCOUNT-BODY
               MOVE    KONSYU-DISCOUNT-RATEKBN (IDX-KON)
                                   TO  SUMSYU-DISCOUNT-RATEKBN (IDX-SUM)
                                       TTLSYU-DISCOUNT-RATEKBN
               MOVE    KONSYU-DISCOUNT-TEIRITU (IDX-KON)
                                   TO  SUMSYU-DISCOUNT-TEIRITU (IDX-SUM)
                                       TTLSYU-DISCOUNT-TEIRITU
               MOVE    KONSYU-DISCOUNT-RATE    (IDX-KON)
                                   TO  SUMSYU-DISCOUNT-RATE    (IDX-SUM)
                                       TTLSYU-DISCOUNT-RATE
               MOVE    KONSYU-DISCOUNT-MONEY   (IDX-KON)
                                   TO  SUMSYU-DISCOUNT-MONEY   (IDX-SUM)
               COMPUTE TTLSYU-DISCOUNT-MONEY
                   =   TTLSYU-DISCOUNT-MONEY
                   +   KONSYU-DISCOUNT-MONEY   (IDX-KON)
           END-IF
      *
           .
       20011-I0SUB02-SYUNOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       20011-HKNCOMBI-EDIT-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
           PERFORM VARYING IDX-COMB    FROM    1  BY  1
                   UNTIL ( IDX-COMB    >   ORCSHKN-HKN-MAX )
                     OR  ( IDX-COMB    >   CONST-ORCSHKN-MAX )
      *
               IF    ( ORCSHKN-GMN-HKNCOMBINUM (IDX-COMB)
                                   =   WRK-HKNCOMBINUM )
                  IF    ( ORCSHKN-NAI-HKNJANUM (IDX-COMB) =   SPACE )
                      MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX-COMB)
                                          TO  WRK-HKNCOMBI
                  ELSE
                      MOVE    SPACE       TO  WRK-HKNCOMBI
                      STRING  ORCSHKN-GMN-HKNCOMBIMEI (IDX-COMB)
                                                  DELIMITED BY  "  "
                          " ("                    DELIMITED  BY  SIZE
                          ORCSHKN-NAI-HKNJANUM (IDX-COMB)
                                                  DELIMITED  BY  SPACE
                          ")"                     DELIMITED  BY  SIZE
                      INTO    WRK-HKNCOMBI
                      END-STRING
                      INITIALIZE               ORCSKANACHKAREA
                      MOVE    "1"              TO  KANACHK-SYORI
                      MOVE    WRK-HKNCOMBI     TO  KANACHK-MAE-INPUT
                      MOVE    50               TO  KANACHK-MAX-CNT
                      CALL    "ORCSKANACHK"    USING
                                               ORCSKANACHKAREA
                      MOVE    KANACHK-MAE-INPUT
                                               TO  WRK-HKNCOMBI
                  END-IF
                  MOVE    ORCSHKN-HKN-MAX      TO  IDX-COMB
               END-IF
      *
           END-PERFORM
      *
           .
       20011-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¼èÆÀ½èÍý
      *****************************************************************
       2001-ORCSHKNSET-CALL-SEC        SECTION.
      *
           INITIALIZE                  ORCSHKNSETAREA
      *
           MOVE    "2"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           .
       2001-ORCSHKNSET-CALL-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    ZERO            TO  I0SUB02-RC
      *
           PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ÊÖµÑÃÍÉ½¼¨
      *****************************************************************
       800-DISPLAY-SEC                 SECTION.
      *
               DISPLAY "==================="
      *
           DISPLAY "I0SUB02-SYUNOU-CNT:" I0SUB02-SYUNOU-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1 
                   UNTIL ( IDX0    >   I0SUB02-SYUNOU-CNT )
      *
               DISPLAY "vvvv " IDX0 " vvvv"
               DISPLAY "I0SUB02-SYUNOU-TTLDAT-FLG :"
                        I0SUB02-SYUNOU-TTLDAT-FLG (IDX0)
               DISPLAY "I0SUB02-SYUNOU-UPDFLG     :"
                        I0SUB02-SYUNOU-UPDFLG     (IDX0)
               DISPLAY "I0SUB02-SYUNOU-YM-S       :"
                        I0SUB02-SYUNOU-YM-S       (IDX0)
               DISPLAY "I0SUB02-SYUNOU-YM-W       :"
                        I0SUB02-SYUNOU-YM-W       (IDX0)
               DISPLAY "I0SUB02-SYUNOU-HKNCOMBI   :"
                        I0SUB02-SYUNOU-HKNCOMBI   (IDX0)
               DISPLAY "I0SUB02-SYUNOU-HKNCOMBI-SU:"
                        I0SUB02-SYUNOU-HKNCOMBI-SU(IDX0)
               DISPLAY "I0SUB02-SYUNOU-TUKI-SU    :"
                        I0SUB02-SYUNOU-TUKI-SU    (IDX0)
               DISPLAY "I0SUB02-UPD-SKYMONEY      :"
                        I0SUB02-UPD-SKYMONEY      (IDX0)
               DISPLAY "I0SUB02-UPD-TAX-SAI       :"
                        I0SUB02-UPD-TAX-SAI       (IDX0)
               DISPLAY "------------------"
      *
           END-PERFORM
      *
               DISPLAY "==================="
      *
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    À¾ÎñÆüËÜ¸ìÊÑ´¹½èÍý
      *****************************************************************
       800-SEIWA-HEN-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD        NOT =   SPACE )
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "¡¡"
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-SYUNOU-KEY57-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID        TO  SYU-PTID
           MOVE    "1"             TO  SYU-CREATEKBN
           MOVE    I0SUB02-RRKNUM  TO  SYU-NYUIN-RRKNUM
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY57-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿FETCH½èÍý
      *****************************************************************
       900-SYUNOU-KEY57-FET-SEC        SECTION.
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY57-FET-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤â¹Ô¤¦)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤Ï¹Ô¤ï¤Ê¤¤)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
