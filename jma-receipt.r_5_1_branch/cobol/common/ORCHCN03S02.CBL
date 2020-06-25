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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHCN03S02.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡ÀÁµá½ñ·óÎÎ¼ý½ñ¡¡¤Þ¤È¤á¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  09/07/03    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TARGET          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX-SYU             PIC 9(05).
           03  IDX-DENPNUM         PIC 9(05).
           03  IDX-SRYKBN          PIC 9(05).
           03  IDX-JIHI            PIC 9(05).
           03  IDX-TMP             PIC 9(05).
      *
       01  KEY-AREA.
           03  KEY-PTID            PIC 9(10).
           03  KEY-DENPNUM         PIC 9(07).
           03  KEY-RRKNUM          PIC 9(03).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-SYU             PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-COMP-KEY            OCCURS  2.
               05  WRK-COMP-SKYEDYMD   PIC X(08).
               05  WRK-COMP-DENPNUM    PIC 9(07).
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-STIDX               PIC  9(03).
      *
       01  CONST-AREA.
           03  CONST-SRYKBN-MAX        PIC 9(03)   VALUE 17.
           03  CONST-GRP-MAX           PIC 9(03)   VALUE 10.
           03  CONST-PRTDEF-G.
               05  CONST-PRTDEF0       PIC X(09)   VALUE   "000000000".
               05  CONST-PRTDEF1       PIC X(09)   VALUE   "111111111".
               05  CONST-PRTDEF2       PIC X(09)   VALUE   "211001100".
               05  CONST-PRTDEF3       PIC X(09)   VALUE   "311110000".
               05  CONST-PRTDEF4       PIC X(09)   VALUE   "410101010".
               05  CONST-PRTDEF5       PIC X(09)   VALUE   "510001000".
               05  CONST-PRTDEF6       PIC X(09)   VALUE   "610100000".
               05  CONST-PRTDEF7       PIC X(09)   VALUE   "711011101".
               05  CONST-PRTDEF8       PIC X(09)   VALUE   "801010101".
               05  CONST-PRTDEF9       PIC X(09)   VALUE   "901000100".
               05  CONST-PRTDEFA       PIC X(09)   VALUE   "A01010000".
           03  CONST-PRTDEF-R  REDEFINES   CONST-PRTDEF-G.
      *        0¡Á7 ´µ¼ÔÅÐÏ¿¶ÈÌ³¤ÎÀÁµá½ñÈ¯¹Ô¶èÊ¬
               05  CONST-PRTDEF-OCC                        OCCURS  11.
                   07  CONST-PRTDEF-PRTFLG     PIC  X(01).
      *            (1):¿·µ¬ (2)¡§ÄûÀµ»þ
                   07  CONST-PRTDEF-OCC2       OCCURS  2.
      *            (1):ÀÁµá³Û>0 (2)¡§ÀÁµá³Û=0
                       09  CONST-PRTDEF-OCC3   OCCURS  2.
                           11  CONST-SKYPRTOUT PIC X(01).
                           11  CONST-SRYPRTOUT PIC X(01).
           03  CONST-PRTDEF-MAX                PIC 9(05)   VALUE   11.
      *
       01  MYSYU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //MYSYU-//.
      *
      *    ¼ýÇ¼¾ðÊóÂàÈòÎÎ°è
       01  TSYU-AREA.
           03  TSYU-MAX                    PIC 9(03).
           03  TSYU-OCC                    OCCURS  11.
               05  TSYU-KEY.
                   07  TSYU-SRYYM          PIC X(06).
                   07  TSYU-DAY-KEY        PIC X(31).
                   07  TSYU-HKNCOMBINUM    PIC X(04).
               05  TSYU-DENPNUM            PIC 9(07).
      *
      *    Äê´üÀÁµáÍúÎò
       01  MYTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC"  REPLACING   //PTTEIKIRRK-//
                                       BY          //MYTEIKIRRK-//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    Æþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ´µ¼ÔÄê´üÀÁµáÍúÎò
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCHCN03.INC".
           COPY    "CPORCHCN03S02.INC".
       01  LNKSYU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //LNKSYU-//.
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCHCN03AREA
           ORCHCN03S02AREA
           LNKSYU-REC
           SPA-AREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       TSYU-AREA
      *
      *    ½é´ü½èÍý
           PERFORM 100-INIT-SEC
      *
      *    ¼ç½èÍý
           PERFORM 200-MAIN-SEC
      *
      *    ½ªÎ»½èÍý
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ½é´üÊÔ½¸½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ORCHCN03-PTID (ORCHCN03S02-IDX)
                                   TO  KEY-PTID
           MOVE    ORCHCN03-DENPNUM (ORCHCN03S02-IDX)
                                   TO  KEY-DENPNUM
           MOVE    LNKSYU-REC      TO  MYSYU-REC
      *
           INITIALIZE                  MYTEIKIRRK-REC
           IF    ( MYSYU-CREATEKBN =   "2" )
               PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
               MOVE    PTTEIKIRRK-REC
                                   TO  MYTEIKIRRK-REC
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
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
           IF    ( MYSYU-DENPJTIKBN    =   "3" )
            OR   ( MYSYU-SYUHKNNUM     =   "971" OR "973" OR "975" )
            OR   ( MYSYU-GRP-HAKHOUFLG =   0 )
            OR   ( ORCHCN03-SKYPRTFLG  =   1 )
               MOVE    MYSYU-REC       TO  SYUNOU-REC
               PERFORM 2001-SUM-SYUNOU-SEC
           ELSE
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU     >   ORCHCN03-CNT )
                        OR   ( FLG-END     =   1 )
      *
                   MOVE    ORCHCN03-PTID (IDX-SYU)
                                               TO  KEY-PTID
                   MOVE    ORCHCN03-DENPNUM (IDX-SYU)
                                               TO  KEY-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
                   INITIALIZE                      PTTEIKIRRK-REC
                   IF    ( SYU-CREATEKBN       =   "2" )
                       PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
                   END-IF
                   PERFORM 2001-CHK-TARGET-SEC
                   IF    ( FLG-TARGET  =   1 )
                       IF    ( IDX-SYU     <   ORCHCN03S02-IDX )
                           MOVE    1           TO  FLG-END
                       ELSE
                           MOVE    ORCHCN03-PTID (IDX-SYU)
                                       TO  KEY-PTID
                           MOVE    ORCHCN03-DENPNUM (IDX-SYU)
                                       TO  KEY-DENPNUM
                           PERFORM 2001-SUM-SYUNOU-SEC
      *
                           IF    ( IDX-SYU =    ORCHCN03S02-IDX)
                            OR   ( ORCHCN03-HAKKOFLG (ORCHCN03S02-IDX)
                                           =   1 )

                               CONTINUE
                           ELSE
                             COMPUTE ORCHCN03-SEIKYU-TAX-SAI
                                                 (ORCHCN03S02-IDX)
                                 =   ORCHCN03-SEIKYU-TAX-SAI
                                                 (ORCHCN03S02-IDX)
                                 +   ORCHCN03-SEIKYU-TAX-SAI (IDX-SYU)
      *
                             COMPUTE ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                                 =   ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                                 +   ORCHCN03-SEIKYU (IDX-SYU)
      *
                             COMPUTE ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                                 =   ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                                 +   ORCHCN03-NYUKIN (IDX-SYU)
      *
                             COMPUTE ORCHCN03-KONMISYU (ORCHCN03S02-IDX)
                                 =   ORCHCN03-KONMISYU (ORCHCN03S02-IDX)
                                 +   ORCHCN03-KONMISYU (IDX-SYU)
                           END-IF
                       END-IF
      *
                   END-IF
               END-PERFORM
      *
               IF      ( FLG-END    =  ZERO )
                   EVALUATE    MYSYU-CREATEKBN
                   WHEN    "1"
                       PERFORM 2001-CHK-TAIIN-SEC
                   WHEN    "2"
                       PERFORM 2001-CHK-TEIKI-SEC
                   END-EVALUATE
               END-IF
      *
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÂÐ¾Ý¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-CHK-TARGET-SEC             SECTION.
      *
           MOVE    ZERO        TO  FLG-TARGET
      *
           EVALUATE    MYSYU-GRP-HAKHOUFLG
           WHEN    1
               IF    ( SYU-PTID        =   MYSYU-PTID )
                AND  ( SYU-CREATEKBN   =   MYSYU-CREATEKBN )
                AND  ( SYU-SRYYMD(1:6) =   MYSYU-SRYYMD (1:6))
                   MOVE    1   TO  FLG-TARGET
      *
               END-IF
           WHEN    2
               IF    ( SYU-PTID        =   MYSYU-PTID )
                AND  ( SYU-CREATEKBN   =   MYSYU-CREATEKBN )
                AND  ( SYU-HKNCOMBINUM =   MYSYU-HKNCOMBINUM )
                   MOVE    1   TO  FLG-TARGET
               END-IF
           WHEN    3
               IF    ( SYU-PTID        =   MYSYU-PTID )
                AND  ( SYU-CREATEKBN   =   MYSYU-CREATEKBN )
                   MOVE    1   TO  FLG-TARGET
               END-IF
           END-EVALUATE
      *
           IF    ( MYSYU-CREATEKBN =   "2" )
               IF    ( PTTEIKIRRK-KBT  =   MYTEIKIRRK-KBT )
                   CONTINUE
               ELSE
                   MOVE    ZERO        TO  FLG-TARGET
               END-IF
           END-IF
      *
           IF    ( SYU-SYUHKNNUM       =   "971" OR "973" OR "975" )
                   MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( SYU-NYUIN-RRKNUM    =   MYSYU-NYUIN-RRKNUM )
               CONTINUE
           ELSE
                   MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( SYU-GRP-HAKHOUFLG   =   MYSYU-GRP-HAKHOUFLG )
               CONTINUE
           ELSE
                   MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           .
       2001-CHK-TARGET-EXT.
           EXIT.
      *****************************************************************
      *    Âà±¡ÀÁµá¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-CHK-TAIIN-SEC            SECTION.
      *
           PERFORM 900-SYUNOU-KEY71-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               MOVE    ZERO            TO IDX-DENPNUM
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU >   ORCHCN03-CNT )
                        OR   ( IDX-DENPNUM NOT =   ZERO )
      *
                   IF    ( SYU-PTID    =   ORCHCN03-PTID   (IDX-SYU))
                    AND  ( SYU-DENPNUM =   ORCHCN03-DENPNUM(IDX-SYU))
                       MOVE    IDX-SYU TO  IDX-DENPNUM
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDX-DENPNUM     =   ZERO )
                   INITIALIZE                  PTTEIKIRRK-REC
                   PERFORM 2001-CHK-TARGET-SEC
                   IF    ( FLG-TARGET  =   1 )
                       PERFORM 2001-SUM-SYUNOU-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-SYUNOU-KEY71-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key71"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-CHK-TAIIN-EXT.
           EXIT.
      *****************************************************************
      *    Äê´üÀÁµá¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-CHK-TEIKI-SEC            SECTION.
      *
           PERFORM 900-PTTEIKIRRK-KEY12-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK NOT =   ZERO )
      *
               MOVE    ZERO            TO IDX-DENPNUM
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU >   ORCHCN03-CNT )
                        OR   ( IDX-DENPNUM NOT =   ZERO )
      *
                   IF    ( PTTEIKIRRK-PTID
                                       =   ORCHCN03-PTID   (IDX-SYU))
                    AND  ( PTTEIKIRRK-DENPNUM
                                       =   ORCHCN03-DENPNUM(IDX-SYU))
                       MOVE    IDX-SYU TO  IDX-DENPNUM
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDX-DENPNUM     =   ZERO )
                   MOVE    PTTEIKIRRK-PTID         TO  KEY-PTID
                   MOVE    PTTEIKIRRK-DENPNUM      TO  KEY-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
                   PERFORM 2001-CHK-TARGET-SEC
                   IF    ( FLG-TARGET  =   1 )
                       PERFORM 2001-SUM-SYUNOU-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-PTTEIKIRRK-KEY12-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-CHK-TEIKI-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼½¸·×½èÍý
      *****************************************************************
       2001-SUM-SYUNOU-SEC             SECTION.
      *
           IF    ( SYU-HKNTEKKBN       =   "1" )
               MOVE    SYU-KOH-FTN     TO  SYU-HKNTEKMONEY
               MOVE    ZERO            TO  SYU-KOH-FTN
           END-IF
      *
           COMPUTE CNT-SYU =   CNT-SYU +   1
      *
           IF    ( CNT-SYU =   1 )
               MOVE    SYUNOU-REC      TO  LNKSYU-REC
               MOVE    LNKSYU-SKYEDYMD TO  WRK-COMP-SKYEDYMD (1)
               MOVE    LNKSYU-DENPNUM  TO  WRK-COMP-DENPNUM  (1)
      *
           ELSE
      *
               PERFORM VARYING IDX-SRYKBN  FROM    1   BY  1
                       UNTIL ( IDX-SRYKBN  >   CONST-SRYKBN-MAX )
      *
                   COMPUTE LNKSYU-HKNTEN (IDX-SRYKBN)
                       =   LNKSYU-HKNTEN (IDX-SRYKBN)
                       +   SYU-HKNTEN    (IDX-SRYKBN)
      *
                   COMPUTE LNKSYU-MONEY (IDX-SRYKBN)
                       =   LNKSYU-MONEY (IDX-SRYKBN)
                       +   SYU-MONEY    (IDX-SRYKBN)
      *
                   COMPUTE LNKSYU-TGMONEY (IDX-SRYKBN)
                       =   LNKSYU-TGMONEY (IDX-SRYKBN)
                       +   SYU-TGMONEY    (IDX-SRYKBN)
      *
                   COMPUTE LNKSYU-TGMONEY-TAX (IDX-SRYKBN)
                       =   LNKSYU-TGMONEY-TAX (IDX-SRYKBN)
                       +   SYU-TGMONEY-TAX (IDX-SRYKBN)
      *
               END-PERFORM
      *
               COMPUTE LNKSYU-SHOHOU-SAI
                   =   LNKSYU-SHOHOU-SAI
                   +   SYU-SHOHOU-SAI
      *
               COMPUTE LNKSYU-TGMONEY-TAX-SAI
                   =   LNKSYU-TGMONEY-TAX-SAI
                   +   SYU-TGMONEY-TAX-SAI
      *
               PERFORM VARYING IDX-JIHI    FROM   1    BY  1
                       UNTIL( IDX-JIHI    >   10 )
                   COMPUTE LNKSYU-JIHI-TAXNASI(IDX-JIHI)
                        =  LNKSYU-JIHI-TAXNASI(IDX-JIHI)
                        +  SYU-JIHI-TAXNASI   (IDX-JIHI)
      *
                   COMPUTE LNKSYU-JIHI-TAXARI(IDX-JIHI)
                        =  LNKSYU-JIHI-TAXARI(IDX-JIHI)
                        +  SYU-JIHI-TAXARI   (IDX-JIHI)
               END-PERFORM
      *
               COMPUTE LNKSYU-JIHI-TOTAL
                   =   LNKSYU-JIHI-TOTAL
                   +   SYU-JIHI-TOTAL
      *
               COMPUTE LNKSYU-JIHI-TOTAL-TAX
                   =   LNKSYU-JIHI-TOTAL-TAX
                   +   SYU-JIHI-TOTAL-TAX
      *
               COMPUTE LNKSYU-JIHI-TAX
                   =   LNKSYU-JIHI-TAX
                   +   SYU-JIHI-TAX
      *
               COMPUTE LNKSYU-RJN-FTN
                   =   LNKSYU-RJN-FTN
                   +   SYU-RJN-FTN
      *
               COMPUTE LNKSYU-KOH-FTN
                   =   LNKSYU-KOH-FTN
                   +   SYU-KOH-FTN
      *
               COMPUTE LNKSYU-YKZ-FTN
                   =   LNKSYU-YKZ-FTN
                   +   SYU-YKZ-FTN
      *
               COMPUTE LNKSYU-CHOSEI
                   =   LNKSYU-CHOSEI
                   +   SYU-CHOSEI
      *
               COMPUTE LNKSYU-CHOSEI1
                   =   LNKSYU-CHOSEI1
                   +   SYU-CHOSEI1
      *
               COMPUTE LNKSYU-CHOSEI2
                   =   LNKSYU-CHOSEI2
                   +   SYU-CHOSEI2
      *
               COMPUTE LNKSYU-HKNTEKMONEY
                   =   LNKSYU-HKNTEKMONEY
                   +   SYU-HKNTEKMONEY
      *
               COMPUTE LNKSYU-DISCOUNT-MONEY
                   =   LNKSYU-DISCOUNT-MONEY
                   +   SYU-DISCOUNT-MONEY
      *
               COMPUTE LNKSYU-RSISHOSHIN-MONEY
                   =   LNKSYU-RSISHOSHIN-MONEY
                   +   SYU-RSISHOSHIN-MONEY
      *
               COMPUTE LNKSYU-RSISAISHIN-MONEY
                   =   LNKSYU-RSISAISHIN-MONEY
                   +   SYU-RSISAISHIN-MONEY
      *
               COMPUTE LNKSYU-RSISDO-MONEY
                   =   LNKSYU-RSISDO-MONEY
                   +   SYU-RSISDO-MONEY
      *
               COMPUTE LNKSYU-RSIETC-MONEY
                   =   LNKSYU-RSIETC-MONEY
                   +   SYU-RSIETC-MONEY
      *
               COMPUTE LNKSYU-RSI-TAX-SAI
                   =   LNKSYU-RSI-TAX-SAI
                   +   SYU-RSI-TAX-SAI
      *
               COMPUTE LNKSYU-RSI-TOTAL
                   =   LNKSYU-RSI-TOTAL
                   +   SYU-RSI-TOTAL
      *
               COMPUTE LNKSYU-RSIJIBAI-SKYMONEY
                   =   LNKSYU-RSIJIBAI-SKYMONEY
                   +   SYU-RSIJIBAI-SKYMONEY
      *
               COMPUTE LNKSYU-SKYMONEY-TAX-SAI
                   =   LNKSYU-SKYMONEY-TAX-SAI
                   +   SYU-SKYMONEY-TAX-SAI
      *
               COMPUTE LNKSYU-SKYMONEY
                   =   LNKSYU-SKYMONEY
                   +   SYU-SKYMONEY
      *
               COMPUTE LNKSYU-RMSAGAKU
                   =   LNKSYU-RMSAGAKU
                   +   SYU-RMSAGAKU
      *
               COMPUTE LNKSYU-RMSAGAKU-TAX-SAI
                   =   LNKSYU-RMSAGAKU-TAX-SAI
                   +   SYU-RMSAGAKU-TAX-SAI
      *
               COMPUTE LNKSYU-RYOYOHI-SKJ
                   =   LNKSYU-RYOYOHI-SKJ
                   +   SYU-RYOYOHI-SKJ
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ
                   =   LNKSYU-SKYMONEY-SKJ
                   +   SYU-SKYMONEY-SKJ
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-TAX
                   =   LNKSYU-SKYMONEY-SKJ-TAX
                   +   SYU-SKYMONEY-SKJ-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-KEI
                   =   LNKSYU-SKYMONEY-SKJ-KEI
                   +   SYU-SKYMONEY-SKJ-KEI
      *
               COMPUTE LNKSYU-RYOYOHI-SKJ-JIHI
                   =   LNKSYU-RYOYOHI-SKJ-JIHI
                   +   SYU-RYOYOHI-SKJ-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-JIHI
                   =   LNKSYU-SKYMONEY-SKJ-JIHI
                   +   SYU-SKYMONEY-SKJ-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-JIHI-TAX
                   =   LNKSYU-SKYMONEY-SKJ-JIHI-TAX
                   +   SYU-SKYMONEY-SKJ-JIHI-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-JIHI-KEI
                   =   LNKSYU-SKYMONEY-SKJ-JIHI-KEI
                   +   SYU-SKYMONEY-SKJ-JIHI-KEI
      *
               COMPUTE LNKSYU-RYOYOHI-LIFE
                   =   LNKSYU-RYOYOHI-LIFE
                   +   SYU-RYOYOHI-LIFE
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE
                   =   LNKSYU-SKYMONEY-LIFE
                   +   SYU-SKYMONEY-LIFE
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-TAX
                   =   LNKSYU-SKYMONEY-LIFE-TAX
                   +   SYU-SKYMONEY-LIFE-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-KEI
                   =   LNKSYU-SKYMONEY-LIFE-KEI
                   +   SYU-SKYMONEY-LIFE-KEI
      *
               COMPUTE LNKSYU-RYOYOHI-LIFE-JIHI
                   =   LNKSYU-RYOYOHI-LIFE-JIHI
                   +   SYU-RYOYOHI-LIFE-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-JIHI
                   =   LNKSYU-SKYMONEY-LIFE-JIHI
                   +   SYU-SKYMONEY-LIFE-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-JIHI-TAX
                   =   LNKSYU-SKYMONEY-LIFE-JIHI-TAX
                   +   SYU-SKYMONEY-LIFE-JIHI-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-JIHI-KEI
                   =   LNKSYU-SKYMONEY-LIFE-JIHI-KEI
                   +   SYU-SKYMONEY-LIFE-JIHI-KEI
      *
           END-IF
      *
           IF    ( LNKSYU-HKNCOMBINUM  =   SYU-HKNCOMBINUM )
               CONTINUE
           ELSE
               MOVE    1               TO  ORCHCN03S02-FUKUSU-COMBI
           END-IF
      *
           IF    ( LNKSYU-DENPNUM      =   SYU-DENPNUM )
               CONTINUE
           ELSE
               IF    ( ORCHCN03-HAKKOFLG (ORCHCN03S02-IDX)  =   1 )
                   COMPUTE ORCHCN03-SEIKYU-TAX-SAI (ORCHCN03S02-IDX)
                       =   ORCHCN03-SEIKYU-TAX-SAI (ORCHCN03S02-IDX)
                       +   SYU-SKYMONEY-TAX-SAI
      *
                   COMPUTE ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                       =   ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                       +   SYU-SKYMONEY
      *
                   COMPUTE ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                       =   ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                       +   SYU-NYUKIN-TOTAL
               END-IF
      *
           END-IF
      *
           IF    ( ORCHCN03S02-GRP-CNT >=  CONST-GRP-MAX )
               CONTINUE
           ELSE
               COMPUTE ORCHCN03S02-GRP-CNT
                   =   ORCHCN03S02-GRP-CNT +   1
               MOVE    SYU-SRYYMD(1:6)
                                   TO  TSYU-SRYYM
                                              (ORCHCN03S02-GRP-CNT)
               MOVE    SYU-DAY-TBL TO  TSYU-DAY-KEY
                                              (ORCHCN03S02-GRP-CNT)
               INSPECT TSYU-DAY-KEY (ORCHCN03S02-GRP-CNT)
                       REPLACING   ALL " "    BY  "9"
               MOVE    SYU-HKNCOMBINUM
                                   TO  TSYU-HKNCOMBINUM 
                                              (ORCHCN03S02-GRP-CNT)
               MOVE    SYU-DENPNUM TO  TSYU-DENPNUM
                                              (ORCHCN03S02-GRP-CNT)
           END-IF
      *
           IF    ( WRK-DENPNUM         =   ZERO )
            OR   ( WRK-DENPNUM         >   SYU-DENPNUM )
               MOVE    SYU-DENPNUM     TO  WRK-DENPNUM
           END-IF
      *
           IF    ( LNKSYU-SKYSTYMD     >   SYU-SKYSTYMD )
               MOVE    SYU-SKYSTYMD    TO  LNKSYU-SKYSTYMD
           END-IF
      *
           IF    ( LNKSYU-SKYEDYMD     <   SYU-SKYEDYMD )
               MOVE    SYU-SKYEDYMD    TO  LNKSYU-SKYEDYMD
           END-IF
      *
           COMPUTE ORCHCN03S02-MYMISYU
               =   ORCHCN03S02-MYMISYU
               +   SYU-SKYMONEY
               -   SYU-NYUKIN-TOTAL
      *
           MOVE    SYU-SKYEDYMD    TO  WRK-COMP-SKYEDYMD(2)
           MOVE    SYU-DENPNUM     TO  WRK-COMP-DENPNUM (2)
      *
           IF    ( WRK-COMP-KEY (1)    <   WRK-COMP-KEY(2))
               MOVE    WRK-COMP-KEY(2) TO  WRK-COMP-KEY(1)
               MOVE    SYU-ROOMNUM     TO  LNKSYU-ROOMNUM
               MOVE    SYU-BYOTONUM    TO  LNKSYU-BYOTONUM
               MOVE    SYU-SRYKA       TO  LNKSYU-SRYKA
               IF    ( SYU-DISCOUNT-TEIRITU   =   "1" OR "2" )
                   MOVE    SYU-DISCOUNT-KBN
                                       TO  LNKSYU-DISCOUNT-KBN
                   MOVE    SYU-DISCOUNT-BODY
                                       TO  LNKSYU-DISCOUNT-BODY
                   MOVE    SYU-DISCOUNT-RATEKBN
                                       TO  LNKSYU-DISCOUNT-RATEKBN
                   MOVE    SYU-DISCOUNT-TEIRITU
                                       TO  LNKSYU-DISCOUNT-TEIRITU
                   MOVE    SYU-DISCOUNT-RATE
                                       TO  LNKSYU-DISCOUNT-RATE
               END-IF
           END-IF
      *
           .
       2001-SUM-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( CNT-SYU         >   ZERO )
      *
               MOVE    1           TO  ORCHCN03S02-PRTFLG
      *
               COMPUTE IDX-TMP =   ORCHCN03S02-GRP-CNT +   1
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1   >=   ORCHCN03S02-GRP-CNT )
      *
                   COMPUTE WRK-STIDX   =   IDX1    +   1
                   PERFORM VARYING IDX2    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX2    >   ORCHCN03S02-GRP-CNT)
                       IF    ( TSYU-KEY (IDX1) >   TSYU-KEY (IDX2))
                           MOVE    TSYU-OCC (IDX1)
                                       TO  TSYU-OCC (IDX-TMP)
                           MOVE    TSYU-OCC (IDX2)
                                       TO  TSYU-OCC (IDX1)
                           MOVE    TSYU-OCC (IDX-TMP)
                                       TO  TSYU-OCC (IDX2)
                       END-IF
                   END-PERFORM
               END-PERFORM
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   ORCHCN03S02-GRP-CNT )
                   MOVE    TSYU-DENPNUM (IDX1)
                                       TO  ORCHCN03S02-GRP-DENPNUM
                                                             (IDX1)
               END-PERFORM
      *
      *        MOVE    WRK-DENPNUM TO  LNKSYU-DENPNUM
               MOVE    ORCHCN03S02-GRP-DENPNUM (1)
                                   TO  LNKSYU-DENPNUM
      *
               IF    ( ORCHCN03S02-CHKPRT      =   1 )
      *
                   MOVE    ZERO    TO  IDX1
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   CONST-PRTDEF-MAX )
                            OR   ( IDX1    >   ZERO )
      *
                       IF    ( CONST-PRTDEF-PRTFLG (IDX0)
                                           =   PTINF-SKYPRTFLG )
                           MOVE    IDX0    TO  IDX1
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( IDX1                    NOT =   ZERO )
                    AND  ( ORCHCN03S02-PTINFREF        =   "1" )
                       IF    ( ORCHCN03-HAKKOFLG (ORCHCN03S02-IDX)
                                           NOT =   "2" )
                           MOVE    1           TO  IDX2
                       ELSE
                           MOVE    2           TO  IDX2
                       END-IF
                       IF    ( ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                                           >   0 )
                           MOVE    1           TO  IDX3
                       ELSE
                           MOVE    2           TO  IDX3
                       END-IF
                       IF    ( ORCHCN03S02-SKYPRT  =   "1" OR "2" )
                           MOVE    CONST-SKYPRTOUT (IDX1 IDX2 IDX3)
                                           TO  ORCHCN03S02-SKYPRT
                       ELSE
                           MOVE    "0"     TO  ORCHCN03S02-SKYPRT
                       END-IF
                       IF    ( ORCHCN03S02-SRYPRT  =   "1" OR "2" )
                           MOVE    CONST-SRYPRTOUT (IDX1 IDX2 IDX3)
                                           TO  ORCHCN03S02-SRYPRT
                       ELSE
                           MOVE    "0"     TO  ORCHCN03S02-SRYPRT
                       END-IF
      *
                   ELSE
                       IF    ( ORCHCN03S02-SKYPRT  =   "2" )
                           IF    ( ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                                               >   0 )
                               MOVE    "1"     TO  ORCHCN03S02-SKYPRT
                           ELSE
                               MOVE    "0"     TO  ORCHCN03S02-SKYPRT
                           END-IF
                       END-IF
                       IF    ( ORCHCN03S02-SRYPRT  =   "2" )
                           IF    ( ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                                               >   0 )
                               MOVE    "1"     TO  ORCHCN03S02-SRYPRT
                           ELSE
                               MOVE    "0"     TO  ORCHCN03S02-SRYPRT
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "1"             TO  SYU-NYUGAIKBN
           MOVE    KEY-PTID        TO  SYU-PTID
           MOVE    KEY-DENPNUM     TO  SYU-DENPNUM
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-SYUNOU-KEY71-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    MYSYU-REC       TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key71"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY71-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿FETCH½èÍý
      *****************************************************************
       900-SYUNOU-KEY71-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key71"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY71-FET-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÄê´üÀÁµáÍúÎò¸¡º÷½èÍý
      *****************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTTEIKIRRK-HOSPNUM
           MOVE    KEY-PTID        TO  PTTEIKIRRK-PTID
           MOVE    KEY-DENPNUM     TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       900-PTINF-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    KEY-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÄê´üÀÁµáÍúÎò¸¡º÷½èÍý
      *****************************************************************
       900-PTTEIKIRRK-KEY12-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTTEIKIRRK-HOSPNUM
           MOVE    MYSYU-PTID      TO  PTTEIKIRRK-PTID
           MOVE    MYSYU-DENPNUM   TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY12-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÄê´üÀÁµáÍúÎò¸¡º÷½èÍý
      *****************************************************************
       900-PTTEIKIRRK-KEY12-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY12-FET-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý
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
       910-DB-SELECT-EXT.
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
       910-DBCLOSECURSOR-SEC                 SECTION.
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
