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
       PROGRAM-ID.             ORCSS002.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¶¦ÄÌ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¼ýÇ¼ÍúÎò¥Æ¡¼¥Ö¥ë¹¹¿·
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/01/19    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/05/02  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYURRK          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX-SYU             PIC 9(05).
           03  IDX-JIHI            PIC 9(05).
           03  IDX-BF              PIC 9(05).
           03  IDX-AF              PIC 9(05).
           03  IDX-SRYKBN          PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-JOUTAIKBN       PIC X(01).
           03  WRK-SYURRKUPDKBN    PIC X(01).
           03  WRK-SYURRKNUM       PIC 9(05).
      *
       01  CONST-AREA.
           03  CONST-SRYKBN-MAX    PIC 9(05)   VALUE 17.
      *
      *    ¼ýÇ¼ÍúÎò½¸·×ÎÎ°è
       01  SUMSYU-AREA.
         02  SUMSYU-OCC            OCCURS  2.
           COPY    "CPSYURRK.INC"  REPLACING
                                   //SYURRK-// BY //SUMSYU-//.
      *
      *    ¼ýÇ¼ºî¶ÈÎÎ°è
       01  WKSYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                   //SYU-//    BY //WKSYU-//.
      *
      *    ¼ýÇ¼ÍúÎòºî¶ÈÎÎ°è
       01  WKSYURRK-REC.
           COPY    "CPSYURRK.INC"  REPLACING
                                   //SYURRK-// BY //WKSYURRK-//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¼ýÇ¼ÍúÎò
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY  "CPORCSS002.INC".
       01  LNKSYU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNKSYU-//.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SS002-AREA
           LNKSYU-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SS002-OT
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    SS002-UPDKBN
           WHEN    "2"
               PERFORM 2001-EDIT-SYURRK-CHOSEI-SEC
           WHEN    "3"
               PERFORM 2001-EDIT-SYURRK-RENEW-SEC
      *
           WHEN    OTHER
      *
               MOVE    1               TO  IDX-BF
               MOVE    2               TO  IDX-AF
      *
               PERFORM 900-SYURRK-KEY2-SEL-SEC
               IF    ( SYURRK-JOUTAIKBN    =   "1" )
                   COMPUTE WKSYURRK-SYURRKNUM
                       =   SYURRK-SYURRKNUM
                   MOVE    SYURRK-SYUEDANUM
                                       TO  WKSYURRK-SYUEDANUM
               ELSE
                   COMPUTE WRK-SYURRKNUM
                       =   SYURRK-SYURRKNUM    +   1
                   MOVE    WRK-SYURRKNUM
                                       TO  WKSYURRK-SYURRKNUM
                   MOVE    ZERO        TO  WKSYURRK-SYUEDANUM
               END-IF
      *
               PERFORM 2001-EDIT-SYURRK-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎòÊÔ½¸½èÍý
      *****************************************************************
       2001-EDIT-SYURRK-SEC            SECTION.
      *
           MOVE    SYURRK-JOUTAIKBN        TO  WRK-JOUTAIKBN
      *
           EVALUATE    TRUE
           WHEN  ( LNKSYU-DENPJTIKBN   =   "3" )
      *
               PERFORM 2001-TORIKESI-SEC
           WHEN  ( WRK-JOUTAIKBN       =   "1" )
      *
               PERFORM 2001-HKNIKKATUHENKOU-SEC
           WHEN    OTHER
      *
               PERFORM 2001-CHK-SYURRK-SEC
           END-EVALUATE
      *
           .
       2001-EDIT-SYURRK-EXT.
           EXIT.
      *****************************************************************
      *    ÀÁµá¼è¾Ã»þ¤Î½èÍý
      *****************************************************************
       2001-TORIKESI-SEC           SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           INITIALIZE              SUMSYU-AREA
           MOVE    IDX-BF      TO  IDX-SYU
           MOVE    SYUNOU-REC  TO  WKSYUNOU-REC
           PERFORM 2001-SUMSYU-SEC
           MOVE    SPACE       TO  WKSYURRK-JOUTAIKBN
           PERFORM 2001-INSERT-SYURRK-SEC
      *
           .
       2001-TORIKESI-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±°ì³çÊÑ¹¹¸å¤Î¹¹¿·½èÍý
      *****************************************************************
       2001-HKNIKKATUHENKOU-SEC    SECTION.
      *
           MOVE    "1"         TO  WRK-SYURRKUPDKBN
      *
           INITIALIZE              SUMSYU-AREA
           MOVE    IDX-AF      TO  IDX-SYU
           MOVE    LNKSYU-REC  TO  WKSYUNOU-REC
           PERFORM 2001-SUMSYU-SEC
           IF    ( SS002-UPDKBN    =   "1" )
               MOVE    "1"         TO  WKSYURRK-JOUTAIKBN
           ELSE
               MOVE    "2"         TO  WKSYURRK-JOUTAIKBN
           END-IF
           PERFORM 2001-UPDATE-SYURRK-SEC
      *
           .
       2001-HKNIKKATUHENKOU-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-CHK-SYURRK-SEC       SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( FLG-SYUNOU  =   ZERO )
               IF    ( LNKSYU-SRYKA        =   SYU-SRYKA )
                AND  ( LNKSYU-HKNCOMBINUM  =   SYU-HKNCOMBINUM )
                   INITIALIZE              SUMSYU-AREA
                   MOVE    IDX-BF      TO  IDX-SYU
                   MOVE    SYUNOU-REC  TO  WKSYUNOU-REC
                   PERFORM 2001-SUMSYU-SEC
      *
                   MOVE    IDX-AF      TO  IDX-SYU
                   MOVE    LNKSYU-REC  TO  WKSYUNOU-REC
                   PERFORM 2001-SUMSYU-SEC
                   MOVE    SPACE       TO  WKSYURRK-JOUTAIKBN
                   PERFORM 2001-INSERT-SYURRK-SEC
               ELSE
      *
                   MOVE    "1"         TO  WRK-SYURRKUPDKBN
      *
                   INITIALIZE              SUMSYU-AREA
                   MOVE    IDX-BF      TO  IDX-SYU
                   MOVE    SYUNOU-REC  TO  WKSYUNOU-REC
                   PERFORM 2001-SUMSYU-SEC
                   MOVE    SPACE       TO  WKSYURRK-JOUTAIKBN
                   PERFORM 2001-INSERT-SYURRK-SEC
      *
                   INITIALIZE              SUMSYU-AREA
                   MOVE    IDX-AF      TO  IDX-SYU
                   MOVE    LNKSYU-REC  TO  WKSYUNOU-REC
                   PERFORM 2001-SUMSYU-SEC
                   IF    ( SS002-UPDKBN    =   "1" )
                       MOVE    "1"     TO  WKSYURRK-JOUTAIKBN
                   END-IF
                   PERFORM 2001-INSERT-SYURRK-SEC
               END-IF
           ELSE
               INITIALIZE              SUMSYU-AREA
               MOVE    IDX-AF      TO  IDX-SYU
               MOVE    LNKSYU-REC  TO  WKSYUNOU-REC
               PERFORM 2001-SUMSYU-SEC
               MOVE    SPACE       TO  WKSYURRK-JOUTAIKBN
               PERFORM 2001-INSERT-SYURRK-SEC
           END-IF
      *
           .
       2001-CHK-SYURRK-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼½¸·×½èÍý
      *****************************************************************
       2001-SUMSYU-SEC             SECTION.
      *
           PERFORM VARYING IDX-SRYKBN  FROM    1   BY  1
                   UNTIL ( IDX-SRYKBN  >   CONST-SRYKBN-MAX )
               MOVE    WKSYU-HKNTEN (IDX-SRYKBN)
                                           TO  SUMSYU-HKNTEN
                                                  (IDX-SYU IDX-SRYKBN)
               MOVE    WKSYU-MONEY  (IDX-SRYKBN)
                                           TO  SUMSYU-MONEY
                                                  (IDX-SYU IDX-SRYKBN)
               MOVE    WKSYU-TGMONEY(IDX-SRYKBN)
                                           TO  SUMSYU-TGMONEY
                                                  (IDX-SYU IDX-SRYKBN)
               MOVE    WKSYU-TGMONEY-TAX
                                    (IDX-SRYKBN)
                                           TO  SUMSYU-TGMONEY-TAX
                                                  (IDX-SYU IDX-SRYKBN)
           END-PERFORM
      *
           MOVE    WKSYU-SHOHOU-SAI        TO  SUMSYU-SHOHOU-SAI
                                                             (IDX-SYU)
      *
           MOVE    WKSYU-TGMONEY-TAX-SAI   TO  SUMSYU-TGMONEY-TAX-SAI
                                                             (IDX-SYU)
      *
           PERFORM VARYING IDX-JIHI    FROM   1    BY  1
                   UNTIL ( IDX-JIHI    >   10 )
               MOVE    WKSYU-JIHI-TAXNASI (IDX-JIHI)
                                   TO  SUMSYU-JIHI-TAXNASI
                                                  (IDX-SYU IDX-JIHI)
               MOVE    WKSYU-JIHI-TAXARI  (IDX-JIHI)
                                   TO  SUMSYU-JIHI-TAXARI
                                                  (IDX-SYU IDX-JIHI)
           END-PERFORM
      *
           MOVE  WKSYU-JIHI-TOTAL          TO  SUMSYU-JIHI-TOTAL
                                                               (IDX-SYU)
           MOVE  WKSYU-JIHI-TOTAL-TAX      TO  SUMSYU-JIHI-TOTAL-TAX
                                                               (IDX-SYU)
           MOVE  WKSYU-JIHI-TAX            TO  SUMSYU-JIHI-TAX (IDX-SYU)
           MOVE  WKSYU-RJN-FTN             TO  SUMSYU-RJN-FTN  (IDX-SYU)
           MOVE  WKSYU-KOH-FTN             TO  SUMSYU-KOH-FTN  (IDX-SYU)
           MOVE  WKSYU-YKZ-FTN             TO  SUMSYU-YKZ-FTN  (IDX-SYU)
           MOVE  WKSYU-CHOSEI              TO  SUMSYU-CHOSEI   (IDX-SYU)
           MOVE  WKSYU-CHOSEI1             TO  SUMSYU-CHOSEI1  (IDX-SYU)
           MOVE  WKSYU-CHOSEI2             TO  SUMSYU-CHOSEI2  (IDX-SYU)
           MOVE  WKSYU-HKNTEKMONEY         TO  SUMSYU-HKNTEKMONEY
                                                               (IDX-SYU)
           MOVE  WKSYU-DISCOUNT-MONEY      TO  SUMSYU-DISCOUNT-MONEY
                                                               (IDX-SYU)
           MOVE  WKSYU-RSISHOSHIN-MONEY    TO  SUMSYU-RSISHOSHIN-MONEY
                                                               (IDX-SYU)
           MOVE  WKSYU-RSISAISHIN-MONEY    TO  SUMSYU-RSISAISHIN-MONEY
                                                               (IDX-SYU)
           MOVE  WKSYU-RSISDO-MONEY        TO  SUMSYU-RSISDO-MONEY
                                                               (IDX-SYU)
           MOVE  WKSYU-RSIETC-MONEY        TO  SUMSYU-RSIETC-MONEY
                                                               (IDX-SYU)
           MOVE  WKSYU-RSI-TAX-SAI         TO  SUMSYU-RSI-TAX-SAI
                                                               (IDX-SYU)
           MOVE  WKSYU-RSI-TOTAL           TO  SUMSYU-RSI-TOTAL(IDX-SYU)
           MOVE  WKSYU-RSIJIBAI-SKYMONEY   TO  SUMSYU-RSIJIBAI-SKYMONEY
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-TAX-SAI    TO  SUMSYU-SKYMONEY-TAX-SAI
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY            TO  SUMSYU-SKYMONEY (IDX-SYU)
           MOVE  WKSYU-RMSAGAKU            TO  SUMSYU-RMSAGAKU (IDX-SYU)
           MOVE  WKSYU-RMSAGAKU-TAX-SAI    TO  SUMSYU-RMSAGAKU-TAX-SAI
                                                               (IDX-SYU)
           MOVE  WKSYU-RYOYOHI-SKJ         TO  SUMSYU-RYOYOHI-SKJ
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-SKJ        TO  SUMSYU-SKYMONEY-SKJ
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-SKJ-TAX    TO  SUMSYU-SKYMONEY-SKJ-TAX
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-SKJ-KEI    TO  SUMSYU-SKYMONEY-SKJ-KEI
                                                               (IDX-SYU)
           MOVE  WKSYU-RYOYOHI-SKJ-JIHI    TO  SUMSYU-RYOYOHI-SKJ-JIHI
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-SKJ-JIHI   TO  SUMSYU-SKYMONEY-SKJ-JIHI
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-SKJ-JIHI-TAX
                                       TO SUMSYU-SKYMONEY-SKJ-JIHI-TAX
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-SKJ-JIHI-KEI
                                       TO SUMSYU-SKYMONEY-SKJ-JIHI-KEI
                                                               (IDX-SYU)
           MOVE  WKSYU-RYOYOHI-LIFE        TO  SUMSYU-RYOYOHI-LIFE
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-LIFE       TO  SUMSYU-SKYMONEY-LIFE
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-LIFE-TAX   TO  SUMSYU-SKYMONEY-LIFE-TAX
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-LIFE-KEI   TO  SUMSYU-SKYMONEY-LIFE-KEI
                                                               (IDX-SYU)
           MOVE  WKSYU-RYOYOHI-LIFE-JIHI   TO  SUMSYU-RYOYOHI-LIFE-JIHI
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-LIFE-JIHI  TO  SUMSYU-SKYMONEY-LIFE-JIHI
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-LIFE-JIHI-TAX
                                       TO SUMSYU-SKYMONEY-LIFE-JIHI-TAX
                                                               (IDX-SYU)
           MOVE  WKSYU-SKYMONEY-LIFE-JIHI-KEI
                                       TO SUMSYU-SKYMONEY-LIFE-JIHI-KEI
                                                               (IDX-SYU)
      *
           .
       2001-SUMSYU-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎòÄÉ²Ã½èÍý
      *****************************************************************
       2001-INSERT-SYURRK-SEC        SECTION.
      *
           COMPUTE WKSYURRK-SYUEDANUM  =   WKSYURRK-SYUEDANUM  +   1
      *
           PERFORM 20011-EDIT-SYURRK-SEC
      *
           MOVE    SMCNDATE-YMD        TO  SYURRK-CREYMD
                                           SYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYURRK-UPHMS
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1               TO  SS002-RC
           END-IF
      *
           .
       2001-INSERT-SYURRK-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎò¹¹¿·½èÍý
      *****************************************************************
       2001-UPDATE-SYURRK-SEC          SECTION.
      *
           PERFORM 20011-EDIT-SYURRK-SEC
      *
           MOVE    WKSYU-CREYMD        TO  SYURRK-CREYMD
           MOVE    SMCNDATE-YMD        TO  SYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYURRK-UPHMS
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    2               TO  SS002-RC
           END-IF
      *
           .
       2001-UPDATE-SYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÍúÎòÊÔ½¸½èÍý
      *****************************************************************
       20011-EDIT-SYURRK-SEC       SECTION.
      *
           INITIALIZE                      SYURRK-REC
           MOVE    WKSYU-HOSPNUM       TO  SYURRK-HOSPNUM
           MOVE    WKSYU-PTID          TO  SYURRK-PTID
           MOVE    WKSYU-NYUGAIKBN     TO  SYURRK-NYUGAIKBN
           MOVE    WKSYU-DENPNUM       TO  SYURRK-DENPNUM
           MOVE    WKSYURRK-SYURRKNUM  TO  SYURRK-SYURRKNUM
           MOVE    WKSYURRK-SYUEDANUM  TO  SYURRK-SYUEDANUM
           MOVE    WKSYURRK-JOUTAIKBN  TO  SYURRK-JOUTAIKBN
      *
           MOVE    WKSYU-SRYKA         TO  SYURRK-SRYKA
           MOVE    WKSYU-HKNCOMBINUM   TO  SYURRK-HKNCOMBINUM
           MOVE    WKSYU-DISCOUNT-KBN  TO  SYURRK-DISCOUNT-KBN


           PERFORM VARYING IDX-SRYKBN  FROM    1   BY  1
                   UNTIL ( IDX-SRYKBN  >   CONST-SRYKBN-MAX )
      *
               COMPUTE SYURRK-HKNTEN (IDX-SRYKBN)
                   =   SUMSYU-HKNTEN (IDX-AF IDX-SRYKBN)
                   -   SUMSYU-HKNTEN (IDX-BF IDX-SRYKBN)
      *
               COMPUTE SYURRK-MONEY (IDX-SRYKBN)
                   =   SUMSYU-MONEY (IDX-AF IDX-SRYKBN)
                   -   SUMSYU-MONEY (IDX-BF IDX-SRYKBN)
      *
               COMPUTE SYURRK-TGMONEY (IDX-SRYKBN)
                   =   SUMSYU-TGMONEY (IDX-AF IDX-SRYKBN)
                   -   SUMSYU-TGMONEY (IDX-BF IDX-SRYKBN)
      *
               COMPUTE SYURRK-TGMONEY-TAX (IDX-SRYKBN)
                   =   SUMSYU-TGMONEY-TAX (IDX-AF IDX-SRYKBN)
                   -   SUMSYU-TGMONEY-TAX (IDX-BF IDX-SRYKBN)
      *
           END-PERFORM
      *
           COMPUTE SYURRK-SHOHOU-SAI
               =   SUMSYU-SHOHOU-SAI(IDX-AF)
               -   SUMSYU-SHOHOU-SAI(IDX-BF)
      *
           COMPUTE SYURRK-TGMONEY-TAX-SAI
               =   SUMSYU-TGMONEY-TAX-SAI(IDX-AF)
               -   SUMSYU-TGMONEY-TAX-SAI(IDX-BF)
      *
           PERFORM VARYING IDX-JIHI    FROM   1    BY  1
                   UNTIL( IDX-JIHI    >   10 )
               COMPUTE SYURRK-JIHI-TAXNASI(IDX-JIHI)
                    =  SUMSYU-JIHI-TAXNASI(IDX-AF IDX-JIHI)
                    -  SUMSYU-JIHI-TAXNASI(IDX-BF IDX-JIHI)
      *
               COMPUTE SYURRK-JIHI-TAXARI(IDX-JIHI)
                    =  SUMSYU-JIHI-TAXARI(IDX-AF IDX-JIHI)
                    -  SUMSYU-JIHI-TAXARI(IDX-BF IDX-JIHI)
           END-PERFORM
      *
           COMPUTE SYURRK-JIHI-TOTAL
               =   SUMSYU-JIHI-TOTAL(IDX-AF)
               -   SUMSYU-JIHI-TOTAL(IDX-BF)
      *
           COMPUTE SYURRK-JIHI-TOTAL-TAX
               =   SUMSYU-JIHI-TOTAL-TAX(IDX-AF)
               -   SUMSYU-JIHI-TOTAL-TAX(IDX-BF)
      *
           COMPUTE SYURRK-JIHI-TAX
               =   SUMSYU-JIHI-TAX(IDX-AF)
               -   SUMSYU-JIHI-TAX(IDX-BF)
      *
           COMPUTE SYURRK-RJN-FTN
               =   SUMSYU-RJN-FTN(IDX-AF)
               -   SUMSYU-RJN-FTN(IDX-BF)
      *
           COMPUTE SYURRK-KOH-FTN
               =   SUMSYU-KOH-FTN(IDX-AF)
               -   SUMSYU-KOH-FTN(IDX-BF)
      *
           COMPUTE SYURRK-YKZ-FTN
               =   SUMSYU-YKZ-FTN(IDX-AF)
               -   SUMSYU-YKZ-FTN(IDX-BF)
      *
           COMPUTE SYURRK-CHOSEI
               =   SUMSYU-CHOSEI(IDX-AF)
               -   SUMSYU-CHOSEI(IDX-BF)
      *
           COMPUTE SYURRK-CHOSEI1
               =   SUMSYU-CHOSEI1(IDX-AF)
               -   SUMSYU-CHOSEI1(IDX-BF)
      *
           COMPUTE SYURRK-CHOSEI2
               =   SUMSYU-CHOSEI2(IDX-AF)
               -   SUMSYU-CHOSEI2(IDX-BF)
      *
           COMPUTE SYURRK-HKNTEKMONEY
               =   SUMSYU-HKNTEKMONEY(IDX-AF)
               -   SUMSYU-HKNTEKMONEY(IDX-BF)
      *
           COMPUTE SYURRK-DISCOUNT-MONEY
               =   SUMSYU-DISCOUNT-MONEY(IDX-AF)
               -   SUMSYU-DISCOUNT-MONEY(IDX-BF)
      *
           COMPUTE SYURRK-RSISHOSHIN-MONEY
               =   SUMSYU-RSISHOSHIN-MONEY(IDX-AF)
               -   SUMSYU-RSISHOSHIN-MONEY(IDX-BF)
      *
           COMPUTE SYURRK-RSISAISHIN-MONEY
               =   SUMSYU-RSISAISHIN-MONEY(IDX-AF)
               -   SUMSYU-RSISAISHIN-MONEY(IDX-BF)
      *
           COMPUTE SYURRK-RSISDO-MONEY
               =   SUMSYU-RSISDO-MONEY(IDX-AF)
               -   SUMSYU-RSISDO-MONEY(IDX-BF)
      *
           COMPUTE SYURRK-RSIETC-MONEY
               =   SUMSYU-RSIETC-MONEY(IDX-AF)
               -   SUMSYU-RSIETC-MONEY(IDX-BF)
      *
           COMPUTE SYURRK-RSI-TAX-SAI
               =   SUMSYU-RSI-TAX-SAI(IDX-AF)
               -   SUMSYU-RSI-TAX-SAI(IDX-BF)
      *
           COMPUTE SYURRK-RSI-TOTAL
               =   SUMSYU-RSI-TOTAL(IDX-AF)
               -   SUMSYU-RSI-TOTAL(IDX-BF)
      *
           COMPUTE SYURRK-RSIJIBAI-SKYMONEY
               =   SUMSYU-RSIJIBAI-SKYMONEY(IDX-AF)
               -   SUMSYU-RSIJIBAI-SKYMONEY(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-TAX-SAI
               =   SUMSYU-SKYMONEY-TAX-SAI(IDX-AF)
               -   SUMSYU-SKYMONEY-TAX-SAI(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY
               =   SUMSYU-SKYMONEY(IDX-AF)
               -   SUMSYU-SKYMONEY(IDX-BF)
      *
           COMPUTE SYURRK-RMSAGAKU
               =   SUMSYU-RMSAGAKU(IDX-AF)
               -   SUMSYU-RMSAGAKU(IDX-BF)
      *
           COMPUTE SYURRK-RMSAGAKU-TAX-SAI
               =   SUMSYU-RMSAGAKU-TAX-SAI(IDX-AF)
               -   SUMSYU-RMSAGAKU-TAX-SAI(IDX-BF)
      *
           COMPUTE SYURRK-RYOYOHI-SKJ
               =   SUMSYU-RYOYOHI-SKJ(IDX-AF)
               -   SUMSYU-RYOYOHI-SKJ(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-SKJ
               =   SUMSYU-SKYMONEY-SKJ(IDX-AF)
               -   SUMSYU-SKYMONEY-SKJ(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-SKJ-TAX
               =   SUMSYU-SKYMONEY-SKJ-TAX(IDX-AF)
               -   SUMSYU-SKYMONEY-SKJ-TAX(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-SKJ-KEI
               =   SUMSYU-SKYMONEY-SKJ-KEI(IDX-AF)
               -   SUMSYU-SKYMONEY-SKJ-KEI(IDX-BF)
      *
           COMPUTE SYURRK-RYOYOHI-SKJ-JIHI
               =   SUMSYU-RYOYOHI-SKJ-JIHI(IDX-AF)
               -   SUMSYU-RYOYOHI-SKJ-JIHI(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-SKJ-JIHI
               =   SUMSYU-SKYMONEY-SKJ-JIHI(IDX-AF)
               -   SUMSYU-SKYMONEY-SKJ-JIHI(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-SKJ-JIHI-TAX
               =   SUMSYU-SKYMONEY-SKJ-JIHI-TAX(IDX-AF)
               -   SUMSYU-SKYMONEY-SKJ-JIHI-TAX(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-SKJ-JIHI-KEI
               =   SUMSYU-SKYMONEY-SKJ-JIHI-KEI(IDX-AF)
               -   SUMSYU-SKYMONEY-SKJ-JIHI-KEI(IDX-BF)
      *
           COMPUTE SYURRK-RYOYOHI-LIFE
               =   SUMSYU-RYOYOHI-LIFE(IDX-AF)
               -   SUMSYU-RYOYOHI-LIFE(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-LIFE
               =   SUMSYU-SKYMONEY-LIFE(IDX-AF)
               -   SUMSYU-SKYMONEY-LIFE(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-LIFE-TAX
               =   SUMSYU-SKYMONEY-LIFE-TAX(IDX-AF)
               -   SUMSYU-SKYMONEY-LIFE-TAX(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-LIFE-KEI
               =   SUMSYU-SKYMONEY-LIFE-KEI(IDX-AF)
               -   SUMSYU-SKYMONEY-LIFE-KEI(IDX-BF)
      *
           COMPUTE SYURRK-RYOYOHI-LIFE-JIHI
               =   SUMSYU-RYOYOHI-LIFE-JIHI(IDX-AF)
               -   SUMSYU-RYOYOHI-LIFE-JIHI(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-LIFE-JIHI
               =   SUMSYU-SKYMONEY-LIFE-JIHI(IDX-AF)
               -   SUMSYU-SKYMONEY-LIFE-JIHI(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-LIFE-JIHI-TAX
               =   SUMSYU-SKYMONEY-LIFE-JIHI-TAX(IDX-AF)
               -   SUMSYU-SKYMONEY-LIFE-JIHI-TAX(IDX-BF)
      *
           COMPUTE SYURRK-SKYMONEY-LIFE-JIHI-KEI
               =   SUMSYU-SKYMONEY-LIFE-JIHI-KEI(IDX-AF)
               -   SUMSYU-SKYMONEY-LIFE-JIHI-KEI(IDX-BF)
      *
           .
       20011-EDIT-SYURRK-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎòÊÔ½¸½èÍý
      *****************************************************************
       2001-EDIT-SYURRK-CHOSEI-SEC     SECTION.
      *
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( FLG-SYUNOU  =   ZERO )
      *
               INITIALIZE                  SYURRK-REC
      *
               MOVE    SYU-HOSPNUM         TO  SYURRK-HOSPNUM
               MOVE    SYU-PTID            TO  SYURRK-PTID
               MOVE    SYU-NYUGAIKBN       TO  SYURRK-NYUGAIKBN
               MOVE    SYU-DENPNUM         TO  SYURRK-DENPNUM
               MOVE    SYU-CREYMD          TO  SYURRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYURRK-UPHMS
      *
               COMPUTE SYURRK-SKYMONEY
                   =   LNKSYU-SKYMONEY
                   -   SYU-SKYMONEY
               COMPUTE SYURRK-CHOSEI
                   =   LNKSYU-CHOSEI
                   -   SYU-CHOSEI
      *
               COMPUTE SYURRK-CHOSEI1
                   =   LNKSYU-CHOSEI1
                   -   SYU-CHOSEI1
      *
               COMPUTE SYURRK-CHOSEI2
                   =   LNKSYU-CHOSEI2
                   -   SYU-CHOSEI2
      *
               IF    ( SS002-IN-SYURRKUPDKBN   =   "1" )
                   COMPUTE SYURRK-SYURRKNUM
                       =   SS002-IN-SYURRKNUM  +   1
               ELSE
                   COMPUTE SYURRK-SYURRKNUM
                       =   SS002-IN-SYURRKNUM
               END-IF
grpsys*        MOVE    SPA-HOSPNUM         TO  -HOSPNUM
               MOVE    SYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_syurrk"        TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    3               TO  SS002-RC
               END-IF
      *
               IF    ( SS002-RC        =   ZERO )
                   COMPUTE SYURRK-SYURRKNUM
                       =   SS002-IN-SYURRKNUM  +   1
grpsys*            MOVE    SPA-HOSPNUM         TO  -HOSPNUM
                   MOVE    SYURRK-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syurrk"    TO  MCP-TABLE
                   MOVE    "upd2"          TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    4           TO  SS002-RC
                   END-IF
               END-IF
      *
               IF    ( SS002-RC        =   ZERO )
                   COMPUTE SYURRK-SYURRKNUM
                       =   SS002-IN-SYURRKNUM
                   COMPUTE SYURRK-SYUEDANUM
                       =   SS002-IN-SYUEDANUM
grpsys*            MOVE    SPA-HOSPNUM         TO  -HOSPNUM
                   MOVE    SYURRK-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syurrk"    TO  MCP-TABLE
                   MOVE    "upd3"          TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    5           TO  SS002-RC
                   END-IF
               END-IF
      *
           ELSE
                   MOVE    6               TO  SS002-RC
           END-IF
      *
           .
       2001-EDIT-SYURRK-CHOSEI-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎòÊÔ½¸½èÍý(²ñ·×¾È²ñ¤«¤é¼ýÇ¼¤òºîÀ®¤µ¤ì¤ë¾ì¹ç¤Î¹ÍÎ¸)
      *****************************************************************
       2001-EDIT-SYURRK-RENEW-SEC      SECTION.
      *
           MOVE    1               TO  IDX-BF
           MOVE    2               TO  IDX-AF
      *
           PERFORM 900-SYURRK-KEY2-SEL-SEC
           MOVE    SYURRK-SYURRKNUM
                                   TO  WKSYURRK-SYURRKNUM
           MOVE    SYURRK-SYUEDANUM
                                   TO  WKSYURRK-SYUEDANUM
           INITIALIZE              SUMSYU-AREA
           MOVE    IDX-AF      TO  IDX-SYU
           MOVE    LNKSYU-REC  TO  WKSYUNOU-REC
           PERFORM 2001-SUMSYU-SEC
           MOVE    SPACE       TO  WKSYURRK-JOUTAIKBN
           PERFORM 2001-UPDATE-SYURRK-SEC
      *
           .
       2001-EDIT-SYURRK-RENEW-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    WKSYURRK-SYURRKNUM  TO  SS002-SYURRKNUM
           MOVE    WKSYURRK-SYUEDANUM  TO  SS002-SYUEDANUM
           MOVE    WRK-SYURRKUPDKBN    TO  SS002-SYURRKUPDKBN
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¸¡º÷½èÍý
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    LNKSYU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎò¸¡º÷½èÍý
      *****************************************************************
       900-SYURRK-KEY2-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    LNKSYU-HOSPNUM      TO  SYURRK-HOSPNUM
           MOVE    LNKSYU-PTID         TO  SYURRK-PTID
           MOVE    LNKSYU-NYUGAIKBN    TO  SYURRK-NYUGAIKBN
           MOVE    LNKSYU-DENPNUM      TO  SYURRK-DENPNUM
      *
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               INITIALIZE                  SYURRK-REC
               MOVE    1               TO  FLG-SYURRK
           END-IF
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤â¹Ô¤¦)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÄÉ²Ã½èÍý
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¹¹¿·½èÍý
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
