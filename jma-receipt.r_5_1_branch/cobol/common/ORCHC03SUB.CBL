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
       PROGRAM-ID.             ORCHC03SUB.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¿ÇÎÅ¹Ô°Ù
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÀÁµá½ñ·óÎÎ¼ý½ñ¼ýÇ¼ÊÔ½¸
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  04/07/16    NACL¡ÝÂ¿¡¹Ç¼   ¿·µ¬ºîÀ®
      *  05/07/14    NACL¡Ý¾®Æ¦Âô   ¼«Èñ£±£°¹àÌÜÂÐ±þ
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/04/XX  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.01    NACL-Â¿¡¹Ç¼  07/08/XX  ÄûÀµ»þ¤Î¹ç·×¶â³ÛÂÐ±þ
      *  04.05.00    NACL-Â¿¡¹Ç¼  10/05/XX  ÀÁµá½ñ·óÌÀºÙ½ñÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUTTL          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDH3                PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDX-DN              PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-HKNCOMBI        PIC X(04).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-GRP-DENPNUM     PIC 9(07).
      *    ÀÁµá³ÛÅù
           03  WRK-SKYMONE-AREA.
               05  WRK-SKYMONEY           PIC S9(07).
               05  WRK-SKYMONEY-TAX       PIC S9(07).
               05  WRK-NYUKIN-TOTAL       PIC S9(07).
               05  WRK-HENKIN             PIC S9(07).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿¡Ý
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    ÊÝÂ¸ÍÑ¼ýÇ¼¥ï¡¼¥¯
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *    ÊÝÂ¸ÍÑ¼ýÇ¼¥ï¡¼¥¯
       01  TBL-SYUNOU-AREA.
         02  TBL-SYUNOU-REC        OCCURS   15.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //TBL-SYU-//.
      *    ÀÁµá³ÛÅù
       01  TBL-SEIKYU-AREA.
           03  TBL-SEIKYU-TBL       OCCURS   15.
               05  TBL-SKYMONEY           PIC S9(07).
               05  TBL-SKYMONEY-TAX       PIC S9(07).
               05  TBL-NYUKIN-TOTAL       PIC S9(07).
               05  TBL-HENKIN             PIC S9(07).
      *
      *    ÊÝÂ¸ÍÑ¼ýÇ¼¥ï¡¼¥¯£²
       01  LNK-SYUNOU-AREA.
         02  LNK-SYUNOU-REC        OCCURS   15.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //LNK-SYU-//.
       01  LNK-DOUJI-AREA.
      *    Æ±»þÆþÎÏ¿ÇÎÅ²Ê
           03  LNK-DOUJI-TBL       OCCURS   15.
               05  LNK-DOUJI-SRYKA-G.
                   07  LNK-DOUJI-SRYKA    PIC  X(02)
                                          OCCURS  10.
               05  LNK-DOUJI-HKNCOMBI-G.
                   07  LNK-DOUJI-HKNCOMBI     PIC  X(04)
                                          OCCURS  10.
       01  LNK-DOUJI-AREA2.
           03  LNK-DOUJI-TBL2       OCCURS   15.
               05  LNK-DOUJI-DENPNUM-G.
                   07  LNK-DOUJI-DENPNUM      PIC  9(07)
                                              OCCURS  15.
      *H27.8
      *¤Þ¤È¤áÍÑÊ£¿ô²ÊÊÝÂ¸
       01  WRK-DOUJI-SRYKA-G.
           05  WRK-DOUJI-SRYKA    PIC  X(02)
                                          OCCURS  10.
      *
      *    ÀÁµá³ÛÅù
       01  LNK-SEIKYU-AREA.
           03  LNK-SEIKYU-TBL       OCCURS   15.
               05  LNK-SKYMONEY           PIC S9(07).
               05  LNK-SKYMONEY-TAX       PIC S9(07).
               05  LNK-NYUKIN-TOTAL       PIC S9(07).
               05  LNK-HENKIN             PIC S9(07).
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
      *    ½èÊýäµ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "CPORCHC03.INC".
      *    ½èÊýäµ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "CPORCHC03SUB.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC03AREA
           ORCHC03SUBAREA
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
           INITIALIZE                  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-DOUJI-AREA
           INITIALIZE                  LNK-SEIKYU-AREA
           INITIALIZE                  ORCHC03SUBAREA
      *
           PERFORM 100-SYUNOU-KEN-SEC
           IF      SYU-GRP-DENPNUM     =   ZERO
      *        Ã±ÆÈÆþÎÏ
               PERFORM 200-ONLY-SYUNOU-HEN-SEC
           ELSE
      *        Ê£¿ôÆþÎÏ
               PERFORM 300-GRP-SYUNOU-HEN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ¼ç¼ýÇ¼¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       100-SYUNOU-KEN-SEC             SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    ORCHC03-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-SYUNOU-KEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÊÔ½¸½èÍý
      *****************************************************************
       200-ONLY-SYUNOU-HEN-SEC             SECTION.
      *
      *    ÄûÀµ»þ¤ÎÀÁµáº¹³Û
           PERFORM 2001-TEISEI-SYORI-SEC
      *
           ADD     1               TO  IDX-SYU
           MOVE    SYUNOU-REC      TO  ORCHC03SUB-SYUNOU-REC(IDX-SYU)
           MOVE    WRK-SKYMONEY    TO  ORCHC03SUB-SKYMONEY  (IDX-SYU)
           MOVE    WRK-SKYMONEY-TAX
                                   TO  ORCHC03SUB-SKYMONEY-TAX-SAI
                                                            (IDX-SYU)
           MOVE    WRK-NYUKIN-TOTAL
                                   TO  ORCHC03SUB-NYUKIN-TOTAL(IDX-SYU)
           MOVE    WRK-HENKIN      TO  ORCHC03SUB-HENKIN(IDX-SYU)
           MOVE    SYU-DENPNUM     TO  ORCHC03SUB-DOUJI-DENPNUM
                                                        (IDX-SYU 1)
           MOVE    IDX-SYU         TO  ORCHC03SUB-MAX
           .
       200-ONLY-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¡¦ÊÝ¸±ÁÈ¹ç¤»ÊÌÊÔ½¸½èÍý
      *****************************************************************
       2001-TEISEI-SYORI-SEC             SECTION.
      *
           MOVE    SYU-SKYMONEY            TO  WRK-SKYMONEY
           MOVE    SYU-SKYMONEY-TAX-SAI    TO  WRK-SKYMONEY-TAX
           MOVE    SYU-NYUKIN-TOTAL        TO  WRK-NYUKIN-TOTAL
           MOVE    ZERO                    TO  WRK-HENKIN
      *    ÄûÀµ»þ¤Ë¹ç·×¶â³ÛÉ½¼¨¤Î»þ¡¢º¹³Û·×»»¤Ê¤·
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               CONTINUE
           ELSE
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   15  )
      *DDD              OR    (ORCHC03-OLD-DENPNUM(IDY)    =   ZERO)
               IF     (ORCHC03-OLD-DENPNUM(IDY)    =   SYU-DENPNUM)
                  AND (SYU-SRYKA               NOT =   ZERO  )
      *            ÄûÀµ»þ¤ÎÀÁµá³Ûº¹³Û
                   COMPUTE WRK-SKYMONEY    =   WRK-SKYMONEY
                                           -   ORCHC03-OLD-SKYMONEY
                                                             (IDY)
                   COMPUTE WRK-SKYMONEY-TAX
                                           =   WRK-SKYMONEY-TAX
                                           -   ORCHC03-OLD-SKYMONEY-SAI
                                                             (IDY)
      *            º£²óÆþ¶â³Û
                   MOVE    ORCHC03-OLD-NYUKIN-TOTAL (IDY)
                                           TO  WRK-NYUKIN-TOTAL
      *            º£²óÊÖ¶â
                   MOVE    ORCHC03-KON-HENKIN (IDY)
                                           TO  WRK-HENKIN
                   MOVE    15              TO  IDY
               END-IF
           END-PERFORM
      *
           END-IF
           .
       2001-TEISEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¡¦ÊÝ¸±ÁÈ¹ç¤»ÊÌÊÔ½¸½èÍý
      *****************************************************************
       300-GRP-SYUNOU-HEN-SEC             SECTION.
      *
           INITIALIZE                  TBL-SYUNOU-AREA
           MOVE    ZERO                TO  IDX-TBL
           MOVE    SYU-GRP-DENPNUM     TO  WRK-GRP-DENPNUM
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-GRP-DENPNUM     TO  SYU-GRP-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           PERFORM
               UNTIL       FLG-SYUNOU      =   1
      *        ÄûÀµ»þ¤ÎÀÁµáº¹³Û
               PERFORM 2001-TEISEI-SYORI-SEC
      *        ¼ýÇ¼ÊÔ½¸
               ADD     1                   TO  IDX-TBL
      *!!!
      *        ¥Þ¥ëÄ¹¤Î¸øÈñÉéÃ´¤òÊÝ¸±Å¬ÍÑ¶â³Û¤Ø
               IF      SYU-HKNTEKKBN   =   "1"
                   MOVE    SYU-KOH-FTN         TO  SYU-HKNTEKMONEY
                   MOVE    ZERO                TO  SYU-KOH-FTN
               END-IF
      *!!!
               MOVE    SYUNOU-REC          TO  TBL-SYUNOU-REC(IDX-TBL)
               MOVE    WRK-SKYMONEY        TO  TBL-SKYMONEY  (IDX-TBL)
               MOVE    WRK-SKYMONEY-TAX
                                           TO  TBL-SKYMONEY-TAX
                                                            (IDX-TBL)
               MOVE    WRK-NYUKIN-TOTAL    TO  TBL-NYUKIN-TOTAL(IDX-TBL)
               MOVE    WRK-HENKIN          TO  TBL-HENKIN   (IDX-TBL)
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-DOUJI-AREA2
           MOVE    ZERO                TO  IDX-DN
           EVALUATE    ORCHC03-HAKHOUFLG
      *        ²Ê¡¦ÊÝ¸±ÊÌ
               WHEN    1
                   PERFORM 3001-SYORI-01-SEC
      *        ÊÝ¸±ÊÌ
               WHEN    2
                   PERFORM 3002-SYORI-02-SEC
      *        ²ÊÊÌ
               WHEN    3
                   PERFORM 3003-SYORI-03-SEC
      *        ¹ç·×
               WHEN    4
                   PERFORM 3004-SYORI-04-SEC
           END-EVALUATE
           .
       300-GRP-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²Ê¡¦ÊÝ¸±ÁÈ¹ç¤»ÊÌÊÔ½¸½èÍý
      *****************************************************************
       3001-SYORI-01-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-TBL
               ADD     1                   TO  IDX-SYU
               MOVE    TBL-SYUNOU-REC(IDX) TO  ORCHC03SUB-SYUNOU-REC
                                                           (IDX-SYU)
               MOVE    TBL-SKYMONEY (IDX)  TO  ORCHC03SUB-SKYMONEY
                                                           (IDX-SYU)
               MOVE    TBL-SKYMONEY-TAX (IDX)
                                       TO  ORCHC03SUB-SKYMONEY-TAX-SAI
                                                           (IDX-SYU)
               MOVE    TBL-NYUKIN-TOTAL (IDX)
                                           TO  ORCHC03SUB-NYUKIN-TOTAL
                                                           (IDX-SYU)
               MOVE    TBL-HENKIN       (IDX)
                                           TO  ORCHC03SUB-HENKIN
                                                           (IDX-SYU)
               MOVE    IDX-SYU             TO  ORCHC03SUB-MAX
           END-PERFORM
           .
       3001-SYORI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÌÊÔ½¸½èÍý
      *****************************************************************
       3002-SYORI-02-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-DOUJI-AREA
           INITIALIZE                  LNK-SEIKYU-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-TBL
               MOVE    TBL-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
               MOVE    TBL-SKYMONEY   (IDX)    TO  WRK-SKYMONEY
               MOVE    TBL-SKYMONEY-TAX (IDX)  TO  WRK-SKYMONEY-TAX
               MOVE    TBL-NYUKIN-TOTAL (IDX)  TO  WRK-NYUKIN-TOTAL
               MOVE    TBL-HENKIN       (IDX)  TO  WRK-HENKIN
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING  IDX-SYU    FROM    1   BY  1
                       UNTIL   (IDX-SYU    >   15  )  OR
                               (FLG-OK     =   1   )
                   IF      LNK-SYU-HKNCOMBINUM(IDX-SYU)    =   SPACE
                       IF      WRK-SYU-FUKU-DENPNUM    =   ZERO
                           MOVE    TBL-SYUNOU-REC(IDX)
                                                   TO  LNK-SYUNOU-REC
                                                       (IDX-SYU)
                           MOVE    TBL-SKYMONEY   (IDX)
                                                   TO  LNK-SKYMONEY
                                                       (IDX-SYU)
                           MOVE    TBL-SKYMONEY-TAX(IDX)
                                                   TO  LNK-SKYMONEY-TAX
                                                       (IDX-SYU)
                           MOVE    TBL-NYUKIN-TOTAL   (IDX)
                                                   TO  LNK-NYUKIN-TOTAL
                                                       (IDX-SYU)
                           MOVE    TBL-HENKIN    (IDX)
                                                   TO  LNK-HENKIN
                                                       (IDX-SYU)
                       ELSE
                           PERFORM 30022-SYUTOTAL-CHK-SEC
                       END-IF
                       MOVE    1                   TO  FLG-OK
                       MOVE    ZERO                TO  IDX-DN
                   ELSE
                       IF      LNK-SYU-HKNCOMBINUM(IDX-SYU)
                                           =   WRK-SYU-HKNCOMBINUM
      *                    ÅÁÉ¼ÈÖ¹æ
                           ADD     1               TO  IDX-DN
                           MOVE    WRK-SYU-DENPNUM
                                                   TO  LNK-DOUJI-DENPNUM
                                                       (IDX-SYU IDX-DN)
      *
                           IF      LNK-SYU-SRYKA (IDX-SYU)
                                               NOT =   ZERO
                               PERFORM 30021-SYUNOU-ADD-SEC
                               MOVE    WRK-SYU-SRYKA   TO  WRK-SRYKA
                               PERFORM 30041-DOUJI-SRYKA-SEC
                           END-IF
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    ¥Ñ¥é¥á¥¿¤ØÊÔ½¸
           PERFORM 30023-LNK-TBL-HENSYU-SEC
      *
           .
       3002-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¹ç·×¥ì¥³¡¼¥ÉÊÔ½¸½èÍý
      *****************************************************************
       30023-LNK-TBL-HENSYU-SEC             SECTION.
      *
           INITIALIZE                  ORCHC03SUBAREA
           MOVE    ZERO                TO  IDX-SYU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   15
               IF       LNK-SYU-PTID (IDX)  =   ZERO
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX-SYU
                   MOVE    LNK-SYUNOU-REC(IDX) TO  ORCHC03SUB-SYUNOU-REC
                                                           (IDX-SYU)
                   MOVE    LNK-SKYMONEY  (IDX) TO  ORCHC03SUB-SKYMONEY
                                                           (IDX-SYU)
                   MOVE    LNK-SKYMONEY-TAX  (IDX)
                                       TO  ORCHC03SUB-SKYMONEY-TAX-SAI
                                                           (IDX-SYU)
                   MOVE    LNK-NYUKIN-TOTAL  (IDX)
                                       TO  ORCHC03SUB-NYUKIN-TOTAL
                                                           (IDX-SYU)
                   MOVE    LNK-HENKIN        (IDX)
                                       TO  ORCHC03SUB-HENKIN
                                                           (IDX-SYU)
                   MOVE    LNK-DOUJI-SRYKA-G(IDX)
                                       TO  ORCHC03SUB-DOUJI-SRYKA-G
                                                           (IDX-SYU)
                   MOVE    LNK-DOUJI-HKNCOMBI-G(IDX)
                                       TO  ORCHC03SUB-DOUJI-HKNCOMBI-G
                                                           (IDX-SYU)
                   MOVE    LNK-DOUJI-DENPNUM-G(IDX)
                                       TO  ORCHC03SUB-DOUJI-DENPNUM-G
                                                           (IDX-SYU)
                   MOVE    IDX-SYU             TO  ORCHC03SUB-MAX
               END-IF
           END-PERFORM
      *
           .
       30023-LNK-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¹ç·×¥ì¥³¡¼¥ÉÊÔ½¸½èÍý
      *****************************************************************
       30022-SYUTOTAL-CHK-SEC             SECTION.
      *
           INITIALIZE                  SYUTTL-REC
           MOVE    WRK-SYU-HOSPNUM     TO  SYUTTL-HOSPNUM 
           MOVE    WRK-SYU-NYUGAIKBN   TO  SYUTTL-NYUGAIKBN 
           MOVE    WRK-SYU-PTID        TO  SYUTTL-PTID 
           MOVE    WRK-SYU-FUKU-DENPNUM
                                       TO  SYUTTL-DENPNUM 
      *
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 981-SYUTTL-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE              SYUTTL-REC
           END-IF
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!!!
      *    ¥Þ¥ëÄ¹¤Î¸øÈñÉéÃ´¤òÊÝ¸±Å¬ÍÑ¶â³Û¤Ø
           IF      SYUTTL-HKNTEKKBN    =   "1"
               MOVE    SYUTTL-KOH-FTN      TO  SYUTTL-HKNTEKMONEY
               MOVE    ZERO                TO  SYUTTL-KOH-FTN
           END-IF
      *!!!
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    TBL-SYUNOU-REC(IDX) TO  SYUNOU-REC
      *¿ÇÎÅ²Ê
           MOVE    SYUTTL-SRYKA        TO  SYU-SRYKA
      *ÅÁÉ¼ÈÖ¹æ
           MOVE    SYUTTL-DENPNUM      TO  SYU-DENPNUM
      *ÀÁµáÆâÍÆ
           MOVE    SYUTTL-SKY-NAIYOU   TO  SYU-SKY-NAIYOU
      *½èÊý¤»¤óÎÁºÆ·Ç
           MOVE    SYUTTL-SHOHOU-SAI   TO  SYU-SHOHOU-SAI
      *ÊÝ¸±Å¬ÍÑ³°¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE    SYUTTL-TGMONEY-TAX-SAI TO  SYU-TGMONEY-TAX-SAI
      *¼«Èñ£±¤«¤é£µ
           MOVE    SYUTTL-JIHI-NAIYOU     TO  SYU-JIHI-NAIYOU
      *¼«Èñ¾®·×¡Ê¾ÃÈñÀÇ¤Ê¤·¡Ë
           MOVE    SYUTTL-JIHI-TOTAL      TO  SYU-JIHI-TOTAL
           MOVE    SYUTTL-JIHI-TOTAL-TAX  TO  SYU-JIHI-TOTAL-TAX
           MOVE    SYUTTL-JIHI-TAX        TO  SYU-JIHI-TAX
      *Ï·¿Í°ìÉôÉéÃ´¶â
           MOVE    SYUTTL-RJN-FTN         TO  SYU-RJN-FTN
           MOVE    SYUTTL-KOH-FTN         TO  SYU-KOH-FTN
           MOVE    SYUTTL-KOH-FTN-ENTANI  TO  SYU-KOH-FTN-ENTANI
      *Ä´À°¶â
           MOVE    SYUTTL-CHOSEI          TO  SYU-CHOSEI
           MOVE    SYUTTL-CHOSEI1         TO  SYU-CHOSEI1
           MOVE    SYUTTL-CHOSEI2         TO  SYU-CHOSEI2
      *¤Þ¤È¤áÆþÎÏº¹³Û
           MOVE    SYUTTL-GRP-SGKMONEY    TO  SYU-GRP-SGKMONEY
      *ÊÝ¸±Å¬ÍÑ¶â³Û
           MOVE    SYUTTL-HKNTEKMONEY     TO  SYU-HKNTEKMONEY
      *ÉÂ±¡¸ºÌÈ»öÍ³¶èÊ¬
           MOVE    SYUTTL-DISCOUNT-KBN    TO  SYU-DISCOUNT-KBN
           MOVE    SYUTTL-DISCOUNT-BODY   TO  SYU-DISCOUNT-BODY
           MOVE    SYUTTL-DISCOUNT-RATEKBN
                                       TO  SYU-DISCOUNT-RATEKBN
           MOVE    SYUTTL-DISCOUNT-TEIRITU
                                       TO  SYU-DISCOUNT-TEIRITU
           MOVE    SYUTTL-DISCOUNT-RATE   TO  SYU-DISCOUNT-RATE
      *¸ºÌÈ¶â³Û
           MOVE    SYU-DISCOUNT-MONEY  TO  SYUTTL-DISCOUNT-MONEY
      *Ï«ºÒÊÝ¸±
           MOVE    SYUTTL-RSISHOSHIN-MONEY
                                       TO  SYU-RSISHOSHIN-MONEY
           MOVE    SYUTTL-RSISAISHIN-MONEY
                                       TO  SYU-RSISAISHIN-MONEY
           MOVE    SYUTTL-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           MOVE    SYUTTL-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           MOVE    SYUTTL-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           MOVE    SYUTTL-RSI-TOTAL       TO  SYU-RSI-TOTAL
           MOVE    SYUTTL-RSIJIBAI-SKYMONEY
                                       TO  SYU-RSIJIBAI-SKYMONEY
      *ÀÁµá¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           MOVE    SYUTTL-SKYMONEY-TAX-SAI
                                       TO  SYU-SKYMONEY-TAX-SAI
      *ÀÁµá¶â³Û
           MOVE    SYUTTL-SKYMONEY        TO  SYU-SKYMONEY
           MOVE    SYUTTL-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL
           MOVE    SYUTTL-NYUKIN-KAISU    TO  SYU-NYUKIN-KAISU
      *Ì¤¼ýÍýÍ³
           MOVE    SYUTTL-MISYURIYU    TO  SYU-MISYURIYU
      *ºßÁí¿Ç¶èÊ¬
           MOVE    SYUTTL-ZAITAKU      TO  SYU-ZAITAKU
      *¿ÇÎÅ¶èÊ¬ÊÌµëÉÕ³°ÅÀ¿ô
           MOVE    SYUTTL-KYUFUGAI     TO  SYU-KYUFUGAI
      *ÊÝ¸±Å¬ÍÑ¶èÊ¬
           MOVE    SYUTTL-HKNTEKKBN    TO  SYU-HKNTEKKBN
      *
      *    Æ±»þÆþÎÏ¿ÇÎÅ²Ê
           PERFORM VARYING     IDH2    FROM    1   BY  1
                   UNTIL      (IDH2    >   10  )  OR
                              (SYUTTL-DOUJI-SRYKA(IDH2) =   SPACE)
               MOVE    SYUTTL-DOUJI-SRYKA (IDH2)
                                       TO  LNK-DOUJI-SRYKA
                                                       (IDX-SYU IDH2)
           END-PERFORM
      *    ÄûÀµ»þ¤ÎÀÁµá³Ûº¹³Û
           MOVE    SYUTTL-SKYMONEY         TO  WRK-SKYMONEY
           MOVE    SYUTTL-SKYMONEY-TAX-SAI TO  WRK-SKYMONEY-TAX
           MOVE    SYUTTL-NYUKIN-TOTAL     TO  WRK-NYUKIN-TOTAL
           MOVE    ZERO                    TO  WRK-HENKIN
      *    ÄûÀµ»þ¤Ë¹ç·×¶â³ÛÉ½¼¨¤Î»þ¡¢º¹³Û·×»»¤Ê¤·
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               CONTINUE
           ELSE
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   15  )
      *D                 OR   (ORCHC03-OLD-DENPNUM(IDY)    =   ZERO)
               IF     (ORCHC03-OLD-SRYKA (IDY)     =   SYU-SRYKA  )
                 AND  (ORCHC03-OLD-HKNCOMBI(IDY)   =   SYU-HKNCOMBINUM)
      *            ÄûÀµ»þ¤ÎÀÁµá³Ûº¹³Û
                   COMPUTE WRK-SKYMONEY    =   WRK-SKYMONEY
                                           -   ORCHC03-OLD-SKYMONEY
                                                             (IDY)
                   COMPUTE WRK-SKYMONEY-TAX
                                           =   WRK-SKYMONEY-TAX
                                           -   ORCHC03-OLD-SKYMONEY-SAI
                                                             (IDY)
                   MOVE    ORCHC03-OLD-NYUKIN-TOTAL (IDY)
                                           TO  WRK-NYUKIN-TOTAL
                   MOVE    ORCHC03-KON-HENKIN (IDY)
                                           TO  WRK-HENKIN
                   MOVE    15              TO  IDY
               END-IF
           END-PERFORM
      *
           END-IF
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC
                                                       (IDX-SYU)
           MOVE    WRK-SKYMONEY        TO  LNK-SKYMONEY (IDX-SYU)
           MOVE    WRK-SKYMONEY-TAX    TO  LNK-SKYMONEY-TAX (IDX-SYU)
           MOVE    WRK-NYUKIN-TOTAL    TO  LNK-NYUKIN-TOTAL (IDX-SYU)
           MOVE    WRK-HENKIN          TO  LNK-HENKIN       (IDX-SYU)
      *
           .
       30022-SYUTOTAL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¹ç·×½èÍý
      *****************************************************************
       30021-SYUNOU-ADD-SEC             SECTION.
      *
           MOVE    LNK-SYUNOU-REC(IDX-SYU)     TO  SYUNOU-REC
      *
           ADD     WRK-SYU-TAX-TAISHOU TO  SYU-TAX-TAISHOU
           ADD     WRK-SYU-TAX-MONEY   TO  SYU-TAX-MONEY
           ADD     WRK-SYU-SKYGK       TO  SYU-SKYGK
           ADD     WRK-SYU-HKNTEKMONEY TO  SYU-HKNTEKMONEY
           ADD     WRK-SYU-SHOHOU-SAI      TO  SYU-SHOHOU-SAI
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   17
               ADD    WRK-SYU-HKNTEN (IDS1)    TO  SYU-HKNTEN(IDS1)
               ADD    WRK-SYU-MONEY (IDS1)     TO  SYU-MONEY (IDS1)
               ADD    WRK-SYU-TGMONEY (IDS1)   TO  SYU-TGMONEY(IDS1)
               ADD    WRK-SYU-TGMONEY-TAX (IDS1)
                                               TO  SYU-TGMONEY-TAX(IDS1)
           END-PERFORM
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   10
               ADD    WRK-SYU-JIHI-TAXNASI (IDS1)
                                           TO  SYU-JIHI-TAXNASI(IDS1)
               ADD    WRK-SYU-JIHI-TAXARI (IDS1)
                                           TO  SYU-JIHI-TAXARI(IDS1)
           END-PERFORM
           ADD     WRK-SYU-JIHI-TOTAL       TO  SYU-JIHI-TOTAL
           ADD     WRK-SYU-JIHI-TOTAL-TAX   TO  SYU-JIHI-TOTAL-TAX
           ADD     WRK-SYU-JIHI-TAX         TO  SYU-JIHI-TAX
           ADD     WRK-SYU-RJN-FTN          TO  SYU-RJN-FTN
           ADD     WRK-SYU-KOH-FTN          TO  SYU-KOH-FTN
           ADD     WRK-SYU-KOH-FTN-ENTANI   TO  SYU-KOH-FTN-ENTANI
           ADD     WRK-SYU-YKZ-FTN          TO  SYU-YKZ-FTN
      *ÊÝ¸±Å¬ÍÑ³°¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           ADD     WRK-SYU-TGMONEY-TAX-SAI  TO  SYU-TGMONEY-TAX-SAI
      *Ä´À°¶â
           ADD     WRK-SYU-CHOSEI           TO  SYU-CHOSEI
           ADD     WRK-SYU-CHOSEI1          TO  SYU-CHOSEI1
           ADD     WRK-SYU-CHOSEI2          TO  SYU-CHOSEI2
      *ÀÁµá¶â³Û¡Ý¾ÃÈñÀÇ¡ÊºÆ·Ç¡Ë
           ADD     WRK-SYU-SKYMONEY-TAX-SAI TO  SYU-SKYMONEY-TAX-SAI
      *ÀÁµá¶â³Û
           ADD     WRK-SYU-SKYMONEY         TO  SYU-SKYMONEY
      *Æþ¶â¹ç·×³Û
           ADD     WRK-SYU-NYUKIN-TOTAL     TO  SYU-NYUKIN-TOTAL 
      *Ï«ºÒÊÝ¸±
           ADD     WRK-SYU-RSISHOSHIN-MONEY
                                           TO  SYU-RSISHOSHIN-MONEY
           ADD     WRK-SYU-RSISAISHIN-MONEY
                                           TO  SYU-RSISAISHIN-MONEY
           ADD     WRK-SYU-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           ADD     WRK-SYU-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           ADD     WRK-SYU-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           ADD     WRK-SYU-RSI-TOTAL       TO  SYU-RSI-TOTAL
           ADD     WRK-SYU-RSIJIBAI-SKYMONEY
                                           TO  SYU-RSIJIBAI-SKYMONEY
      *¤Þ¤È¤áÆþÎÏº¹³Û
           ADD     WRK-SYU-GRP-SGKMONEY    TO  SYU-GRP-SGKMONEY
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC(IDX-SYU)
      *
           ADD     WRK-SKYMONEY        TO  LNK-SKYMONEY (IDX-SYU)
           ADD     WRK-SKYMONEY-TAX    TO  LNK-SKYMONEY-TAX (IDX-SYU)
           ADD     WRK-NYUKIN-TOTAL    TO  LNK-NYUKIN-TOTAL (IDX-SYU)
           ADD     WRK-HENKIN          TO  LNK-HENKIN       (IDX-SYU)
           .
       30021-SYUNOU-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÊÌÊÔ½¸½èÍý
      *****************************************************************
       3003-SYORI-03-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SEIKYU-AREA
           INITIALIZE                  LNK-DOUJI-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-TBL
               MOVE    TBL-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
               MOVE    TBL-SKYMONEY   (IDX)    TO  WRK-SKYMONEY
               MOVE    TBL-SKYMONEY-TAX (IDX)  TO  WRK-SKYMONEY-TAX
               MOVE    TBL-NYUKIN-TOTAL (IDX)  TO  WRK-NYUKIN-TOTAL
               MOVE    TBL-HENKIN       (IDX)  TO  WRK-HENKIN
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING  IDX-SYU    FROM    1   BY  1
                       UNTIL   (IDX-SYU    >   15  )  OR
                               (FLG-OK     =   1   )
                   IF      LNK-SYU-SRYKA(IDX-SYU)    =   SPACE
                       MOVE    TBL-SYUNOU-REC(IDX)
                                               TO  LNK-SYUNOU-REC
                                                       (IDX-SYU)
                       MOVE    TBL-SKYMONEY   (IDX)
                                                   TO  LNK-SKYMONEY
                                                       (IDX-SYU)
                       MOVE    TBL-SKYMONEY-TAX(IDX)
                                                   TO  LNK-SKYMONEY-TAX
                                                       (IDX-SYU)
                       MOVE    TBL-NYUKIN-TOTAL   (IDX)
                                                   TO  LNK-NYUKIN-TOTAL
                                                       (IDX-SYU)
                       MOVE    TBL-HENKIN         (IDX)
                                                   TO  LNK-HENKIN
                                                       (IDX-SYU)
                       MOVE    1                   TO  FLG-OK
                       MOVE    ZERO                TO  IDX-DN
                   ELSE
                       IF      LNK-SYU-SRYKA (IDX-SYU)
                                           =   WRK-SYU-SRYKA
                           MOVE    LNK-SYU-HKNCOMBINUM(IDX-SYU)
                                               TO  WRK-HKNCOMBI
                           PERFORM 30031-DOUJI-HKNCOMBI-SEC
      *                    ÅÁÉ¼ÈÖ¹æ
                           ADD     1               TO  IDX-DN
                           MOVE    WRK-SYU-DENPNUM TO  LNK-DOUJI-DENPNUM
                                                      (IDX-SYU IDX-DN)
      *                    Ê£¿ôÊÝ¸±
                           MOVE    ZERO        TO  LNK-SYU-HKNCOMBINUM
                                                           (IDX-SYU)
                           PERFORM 30021-SYUNOU-ADD-SEC
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    ¥Ñ¥é¥á¥¿¤ØÊÔ½¸
           PERFORM 30023-LNK-TBL-HENSYU-SEC
      *
           .
       3003-SYORI-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¤Þ¤È¤áÊ£¿ôÊÝ¸±ÊÔ½¸½èÍý
      *****************************************************************
       30031-DOUJI-HKNCOMBI-SEC               SECTION.
      *
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL       IDH3    >   10
               IF      LNK-DOUJI-HKNCOMBI (IDX-SYU IDH3)
                                       =   SPACE
                   MOVE    WRK-HKNCOMBI    TO   LNK-DOUJI-HKNCOMBI
                                                         (IDX-SYU IDH3)
               END-IF
               IF      LNK-DOUJI-HKNCOMBI (IDX-SYU IDH3)
                                       =   WRK-HKNCOMBI
                   MOVE    10          TO  IDH3
               END-IF
           END-PERFORM
      *
      *
           .
       30031-DOUJI-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¤Þ¤È¤á¹ç·×ÊÔ½¸½èÍý
      *****************************************************************
       3004-SYORI-04-SEC               SECTION.
      *
           INITIALIZE                  LNK-DOUJI-AREA2
           MOVE    ZERO                TO  IDX-DN
           PERFORM VARYING     IDX     FROM    2   BY  1
                   UNTIL       IDX     >   IDX-TBL
               MOVE    TBL-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
      *        ÅÁÉ¼ÈÖ¹æ
               ADD     1                   TO  IDX-DN
               MOVE    WRK-SYU-DENPNUM     TO  LNK-DOUJI-DENPNUM
                                                 (1 IDX-DN)
           END-PERFORM
      *
      *H27.8
      *    ¿ÇÎÅ²ÊÊÝÂ¸
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-TBL
               MOVE    TBL-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
      *        Ê£¿ô²Ê
               MOVE    1                   TO  IDX-SYU
               MOVE    WRK-SYU-SRYKA       TO  WRK-SRYKA
               PERFORM 30041-DOUJI-SRYKA-SEC
           END-PERFORM
      *    ¿ÇÎÅ²ÊÊÝÂ¸
           MOVE    LNK-DOUJI-SRYKA-G (1)   TO  WRK-DOUJI-SRYKA-G
      *
      *    ÊÝ¸±ÊÌ¤Ë½¸·×¤¹¤ë
           PERFORM 3002-SYORI-02-SEC
      *    ¹ç·×¤¹¤ë
           INITIALIZE                      TBL-SYUNOU-AREA
           MOVE    LNK-SYUNOU-AREA     TO  TBL-SYUNOU-AREA
           MOVE    LNK-SEIKYU-AREA     TO  TBL-SEIKYU-AREA
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SEIKYU-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15   )  OR
                              (IDX     >   ORCHC03SUB-MAX)
               MOVE    TBL-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
               MOVE    TBL-SKYMONEY   (IDX)    TO  WRK-SKYMONEY
               MOVE    TBL-SKYMONEY-TAX (IDX)  TO  WRK-SKYMONEY-TAX
               MOVE    TBL-NYUKIN-TOTAL (IDX)  TO  WRK-NYUKIN-TOTAL
               MOVE    TBL-HENKIN       (IDX)  TO  WRK-HENKIN
               MOVE    1                   TO  IDX-SYU
               IF      IDX                 =   1
                   MOVE    TBL-SYUNOU-REC(IDX)
                                           TO  LNK-SYUNOU-REC
                                                       (IDX-SYU)
                   MOVE    TBL-SKYMONEY   (IDX)
                                           TO  LNK-SKYMONEY
                                                       (IDX-SYU)
                   MOVE    TBL-SKYMONEY-TAX(IDX)
                                           TO  LNK-SKYMONEY-TAX
                                                       (IDX-SYU)
                   MOVE    TBL-NYUKIN-TOTAL (IDX)
                                           TO  LNK-NYUKIN-TOTAL
                                                       (IDX-SYU)
                   MOVE    TBL-HENKIN       (IDX)
                                           TO  LNK-HENKIN
                                                       (IDX-SYU)
                   IF      ORCHC03SUB-MAX      >  1
      *                Ê£¿ôÊÝ¸±
                       MOVE    WRK-SYU-HKNCOMBINUM TO  WRK-HKNCOMBI
                       PERFORM 30031-DOUJI-HKNCOMBI-SEC
                   END-IF
      ****         Ê£¿ô²Ê
      *            MOVE    WRK-SYU-SRYKA   TO  WRK-SRYKA
      ****         PERFORM 30041-DOUJI-SRYKA-SEC
               ELSE
                   PERFORM 30021-SYUNOU-ADD-SEC
      *            Ê£¿ôÊÝ¸±
                   MOVE    WRK-SYU-HKNCOMBINUM TO  WRK-HKNCOMBI
                   PERFORM 30031-DOUJI-HKNCOMBI-SEC
                   MOVE    ZERO            TO  LNK-SYU-HKNCOMBINUM
                                                           (IDX-SYU)
      ****         Ê£¿ô²Ê
      *            MOVE    WRK-SYU-SRYKA   TO  WRK-SRYKA
      ****         PERFORM 30041-DOUJI-SRYKA-SEC
               END-IF
           END-PERFORM
      *H27.8
      *    ¿ÇÎÅ²Ê
           MOVE    WRK-DOUJI-SRYKA-G   TO  LNK-DOUJI-SRYKA-G (IDX-SYU)
      *    ¥Ñ¥é¥á¥¿¤ØÊÔ½¸
           PERFORM 30023-LNK-TBL-HENSYU-SEC
      *
           .
       3004-SYORI-04-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æ±»þÆþÎÏ¿ÇÎÅ²ÊÊÔ½¸½èÍý
      *****************************************************************
       30041-DOUJI-SRYKA-SEC             SECTION.
      *
           IF      WRK-SRYKA           =   SPACE
                                       OR  ZERO
               CONTINUE
           ELSE
      *
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL       IDH3    >   10
               IF      LNK-DOUJI-SRYKA (IDX-SYU IDH3)
                                       =   SPACE
                   MOVE    WRK-SRYKA       TO   LNK-DOUJI-SRYKA
                                                         (IDX-SYU IDH3)
               END-IF
               IF      LNK-DOUJI-SRYKA (IDX-SYU IDH3)
                                       =   WRK-SRYKA
                   MOVE    10          TO  IDH3
               END-IF
           END-PERFORM
      *
           END-IF
           .
       30041-DOUJI-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¹ç·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       981-SYUTTL-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUTTL-REC
               MOVE    ZERO            TO  FLG-SYUTTL
           ELSE
               INITIALIZE                  SYUTTL-REC
               MOVE    1               TO  FLG-SYUTTL
           END-IF
      *
           .
       981-SYUTTL-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
