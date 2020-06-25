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
       PROGRAM-ID.             ORCSKARRK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ²ÊÍúÎò¡¡½é¿Ç»»ÄêÆü¹¹¿·½èÍý
      *  ÊÖµÑ¥¨¥é¡¼¥³¡¼¥É  : 00:Àµ¾ï¡¡10:¥Ñ¥é¥á¡¼¥¿ÉÔÀµ¡¦ÂÐ¾Ý¥Ç¡¼¥¿¤Ê¤·
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  01.00.01    MCC-Â¿¡¹Ç¼   02/05/15  Ï«ºÒÍÑÄÉ²Ã
      *  01.00.02    NACL-Â¿¡¹Ç¼  02/12/15  ½é¿Ç»»ÄêÆüÈ½Äê½¤Àµ
      *  01.00.03    NACL-Â¿¡¹Ç¼  03/02/13  ÄûÀµ½é¿Ç»»ÄêÆüÈ½Äê½¤Àµ
      *  01.00.04    NACL-Â¿¡¹Ç¼  05/11/22  MONFUNC ÂÐ±þ Â¾
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/05/28  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  13/03/XX  Ê¿À®£²£µÇ¯£´·î²þÀµÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  13/04/26  ÄûÀµ½é¿Ç»»Äê²ÊÈ½Äê½¤Àµ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SRYKARRK        PIC 9(01).
      *****03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-SRYKARRK-ARI    PIC 9(01).
           03  FLG-JYURRK-ARI      PIC 9(01).
           03  FLG-JYURRK-CHK      PIC 9(01).
           03  FLG-SRYACCT-CHK     PIC 9(01).
      *
           03  FLG-SRYACT-OK       PIC 9(01).
           03  FLG-TAKARRK         PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SRYCD               PIC X(09). 
           03  WRK-SYOSINYMD1          PIC X(08). 
           03  WRK-SYOSINYMD2          PIC X(08). 
           03  WRK-LST-SRYYMD          PIC X(08). 
           03  WRK-LST-SRYYMD2         PIC X(08). 
      *    Æþ±¡ÍúÎò
           03  WRK-NYULST-SRYYMD       PIC X(08). 
           03  WRK-NYULST-SRYYMD2      PIC X(08). 
      *
           03  WRK-SANTEIYMD           PIC X(08). 
           03  WRK-DAY                 PIC 9(02).
      ***  03  WRK-DD                  PIC 9(02).
      *
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SYOSINYMD-RRK       PIC X(08).
           03  WRK-SYOSINYMD-RRK2      PIC X(08).
      *
           03  WRK-YMD.
               05  WRK-YYMM.
                   07   WRK-YY             PIC X(04).
                   07   WRK-MM             PIC X(02).
               05  WRK-DD                  PIC 9(02).
           03  WRK-DD2                     PIC 9(02).
      *
           03  WRK-SRYKA-YMD.
               05  WRK-SRYKA-YM            PIC X(06).
               05  WRK-SRYKA-DD            PIC X(02).
      *
       01  TBL-SYOSIN-AREA.
           03  TBL-SYOSIN-OCC          OCCURS   100.
               05  TBL-SYOSIN-SRYKA    PIC X(02).
               05  TBL-SYOSIN-YMD      PIC X(08).
               05  TBL-SYOSIN-FST      PIC X(08).
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ÊÝ¸±ÊÌ»»ÄêÍúÎò¥Æ¡¼¥Ö¥ë
      *D   COPY   "CMSANTEITBL.INC".
      *
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
      **** COPY    "CPTENSU.INC".
      *
      *    ¿ÇÎÅ²ÊÍúÎò
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *    ¿ÇÎÅ²ÊÍúÎò
       01  WRK-SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC"  REPLACING
                                     //KARRK-// BY //WRK-KARRK-//.
      *    ¿ÇÎÅ²ÊÍúÎò
       01  HZN-SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC"  REPLACING
                                     //KARRK-// BY //HZN-KARRK-//.
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKARRK.INC".
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSKARRKAREA
           SPA-AREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  SKARRK-RC
           MOVE    SPACE               TO  SKARRK-ERRMSG
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ¹¹¿·Æü¼èÆÀ
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SKARRK-SRYYM        =   SPACE
               MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
           END-IF
      *
           IF      SKARRK-SRYKA        =   SPACE
               MOVE    SPA-SRYKA           TO  SKARRK-SRYKA
           END-IF
      *
      *    ¿ÇÎÅ²ÊÍúÎò½é´ü½èÍý
           PERFORM 100-SRYKA-SYOKI-SEC
           IF      SKARRK-RC           =   ZERO
      *        »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
      ******   PERFORM 200-SANTEIRRK-CHK-SEC
               PERFORM 2001-SANTEI-CHK-SEC
      *        ¿ÇÎÅ²ÊÍúÎò¹¹¿·½èÍý
               PERFORM 300-SRYKARRK-HEN-SEC
           END-IF
      *
           IF      SPA-ERRCD           NOT =   SPACE
               MOVE    "¿ÇÎÅ²ÊÍúÎò¹¹¿·¥¨¥é¡¼"      TO  SKARRK-ERRMSG
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    10                  TO  SKARRK-RC
           END-IF
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò½é´ü½èÍý
      *****************************************************************
       100-SRYKA-SYOKI-SEC                SECTION.
      *
      *    ¿ÇÎÅ²ÊÍúÎòÆÉ¹þ
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
           MOVE    SKARRK-SRYKA        TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    ¿ÇÎÅ²Ê¿·µ¬ºîÀ®
           IF      FLG-SRYKARRK           =   1
      *        ÅÐÏ¿¤Ê¤·
               MOVE    ZERO                TO  FLG-SRYKARRK-ARI
      *
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                      SRYKARRK-REC
               MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SKARRK-SRYKA        TO  KARRK-SRYKA
      *        ¿ÇÎÅ²Ê¤Î»ØÄê¤¬¤Ê¤¤¾ì¹ç¡¢¿·µ¬ÅÐÏ¿¤Ï¤·¤Ê¤¤
               IF      SKARRK-SRYKA        =   SPACE  OR  ZERO
                   MOVE    10              TO  SKARRK-RC
                   MOVE    "¿ÇÎÅ²Ê¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤Þ¤»¤ó"
                                           TO  SKARRK-ERRMSG
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK-ARI
           END-IF
           .
      *
       100-SRYKA-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-SANTEI-CHK-SEC                SECTION.
      *
           INITIALIZE                      TBL-SYOSIN-AREA
           MOVE    SPACE               TO  WRK-SRYKA-YMD
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    ALL "9"             TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *
      *        ¿ÇÎÅ²Ê·èÄê
               PERFORM 20011-SRYACT-SRYKA-SEC
      *
      *        ¿ÇÎÅ¹Ô°Ù¤Ê¤·¡Ê¼õ¿ÇÍúÎò¤Î¤ß¡Ë
               IF      FLG-SRYACT-OK       =   ZERO
      *
                   MOVE    ZERO                TO  WRK-DAY
                   PERFORM VARYING     IDY     FROM    31  BY  -1
                           UNTIL       IDY     <   1
                       IF      SANTEI-DAY (IDY)    NOT =   ZERO
                           MOVE    IDY             TO  WRK-DAY
                           MOVE    1               TO  IDY
                       END-IF
                   END-PERFORM
                   MOVE    SANTEI-SRYYM            TO  WRK-SANTEIYMD
                                                               (1:6)
                   MOVE    WRK-DAY                 TO  WRK-SANTEIYMD
                                                               (7:2)
                   IF     (WRK-SYOSINYMD1          =   SPACE  )  OR
                          (WRK-SANTEIYMD           <   WRK-SYOSINYMD1)
                       MOVE    WRK-SANTEIYMD       TO  WRK-SYOSINYMD1
                   END-IF
                   IF      WRK-SANTEIYMD           >   WRK-SYOSINYMD2
                       MOVE    WRK-SANTEIYMD       TO  WRK-SYOSINYMD2
                   END-IF
      *
               END-IF
      *
               IF     (WRK-SRYKA-YMD   NOT =   SPACE )   AND
                      (WRK-SRYKA-YM        >   SANTEI-SRYYM )
      *            ¿ÇÎÅ²Ê¤Î½é¿Ç»»ÄêÆü¤¬¤¢¤ê·îÆâ¤Î»»Äê¤¬½ªÎ»¤·¤¿»þ¡¢½ªÎ»
                   MOVE    1               TO  FLG-SANTEI
               END-IF
      *
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key11"             TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *test
      *    DISPLAY "SKARRK-SRYKA:" SKARRK-SRYKA
      *          ",WRK-SYOSINYMD1:" WRK-SYOSINYMD1
      *          ",WRK-SYOSINYMD2:" WRK-SYOSINYMD2
      *    DISPLAY "1 TBL-SYOSIN-SRYKA:" TBL-SYOSIN-SRYKA (1)
      *           ",TBL-SYOSIN-YMD:" TBL-SYOSIN-YMD (1)
      *           ",TBL-SYOSIN-FST:" TBL-SYOSIN-FST (1)
      *    DISPLAY "2 TBL-SYOSIN-SRYKA:" TBL-SYOSIN-SRYKA (2)
      *           ",TBL-SYOSIN-YMD:" TBL-SYOSIN-YMD (2)
      *           ",TBL-SYOSIN-FST:" TBL-SYOSIN-FST (2)
      *    DISPLAY "3 TBL-SYOSIN-SRYKA:" TBL-SYOSIN-SRYKA (3)
      *           ",TBL-SYOSIN-YMD:" TBL-SYOSIN-YMD (3)
      *           ",TBL-SYOSIN-FST:" TBL-SYOSIN-FST (3)
      *
           .
       2001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¹¹¿·½èÍý
      *****************************************************************
       20011-SRYACT-SRYKA-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT-OK
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼¤«¤é¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¤ò¸¡º÷¤¹¤ë
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SANTEI-SRYYM        TO  SRY-SRYYM
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (1)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (2)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (3)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (4)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT       =   1 )
      *
      *        ²ó¿ô¼èÆÀ
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SRY-PTID            TO  ACCT-PTID
               MOVE    SRY-SRYKA           TO  ACCT-SRYKA
               MOVE    SRY-SRYYM           TO  ACCT-SRYYM
               MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
               MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *        ¼ç¥­¡¼ÆÉ¤ß
               PERFORM 940-SRYACCT-KEYREAD-SEC
               IF     (FLG-SRYACCT         =   ZERO )
                 AND  (ACCT-HKNCOMBI   NOT =   "9999")
      *
                   MOVE    1               TO  FLG-SRYACT-OK
      *
                   MOVE    SPACE           TO  WRK-YMD
                   MOVE    ACCT-SRYYM      TO  WRK-YMD(1:6)
                   MOVE    ZERO            TO  WRK-DD
                   MOVE    ZERO            TO  WRK-DD2
      *            ºÇ¸å¤Î»»ÄêÆü
                   PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                           UNTIL       IDX-Y2  >   31
                       IF      ACCT-DAY (1 IDX-Y2) >   ZERO
                           MOVE    IDX-Y2          TO  WRK-DD
                           IF      WRK-DD2         =   ZERO
                               MOVE    IDX-Y2          TO  WRK-DD2
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              =   ZERO
                       MOVE    SPACE           TO  WRK-YMD
                   END-IF
      *            ¿ÇÎÅ²Ê¤Î½é¿Ç»»Äê¤¬¤¢¤ê¡¢»»Äê·î¤ÎÈ½Äê½ªÎ»»þ¤Ë½ªÎ»
                   IF     (SRY-SRYKA       =   SKARRK-SRYKA )
                       IF    (WRK-SRYKA-YMD    =   SPACE  )
                          OR (WRK-SRYKA-YMD    <   WRK-YMD)
                           MOVE    WRK-YMD         TO  WRK-SRYKA-YMD
                       END-IF
                   END-IF
      *
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2            >   100 )
                       IF      TBL-SYOSIN-SRYKA (IDX2) =   SPACE
                           MOVE    SRY-SRYKA       TO  TBL-SYOSIN-SRYKA
                                                               (IDX2)
                       END-IF
                       IF      TBL-SYOSIN-SRYKA (IDX2) =   SRY-SRYKA
                           IF     (TBL-SYOSIN-YMD (IDX2)  =   SPACE )
                              OR  (TBL-SYOSIN-YMD (IDX2)  <   WRK-YMD)
                               MOVE    WRK-YMD     TO  TBL-SYOSIN-YMD
                                                              (IDX2)
                           END-IF
      *
                           MOVE    WRK-DD2         TO  WRK-DD
                           IF     (TBL-SYOSIN-FST (IDX2)  =   SPACE )
                              OR  (TBL-SYOSIN-FST (IDX2)  >   WRK-YMD)
                               MOVE    WRK-YMD     TO  TBL-SYOSIN-FST
                                                              (IDX2)
                           END-IF
      *
                           MOVE    100             TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20011-SRYACT-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¹¹¿·½èÍý
      *****************************************************************
       300-SRYKARRK-HEN-SEC                SECTION.
      *
      *    ºÇ½ª¼õ¿ÇÍúÎò
           MOVE    SPACE               TO  WRK-LST-SRYYMD
           MOVE    SPACE               TO  WRK-LST-SRYYMD2
           MOVE    SPACE               TO  WRK-NYULST-SRYYMD
           MOVE    SPACE               TO  WRK-NYULST-SRYYMD2
           MOVE    SPACE               TO  WRK-SYOSINYMD-RRK
           MOVE    SPACE               TO  WRK-SYOSINYMD-RRK2
      *
           MOVE    ZERO                TO  FLG-JYURRK-ARI
           MOVE    ZERO                TO  FLG-JYURRK-CHK
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SKARRK-SRYKA        TO  JYURRK-SRYKA
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    ZERO                TO  FLG-JYURRK-ARI
           IF      FLG-JYURRK          =   ZERO
      *        ¿ÇÎÅ²Ê¤ÎºÇ½ª¼õ¿ÇÆü
               IF      JYURRK-NYUGAIKBN    =   "2"
                   MOVE    JYURRK-SRYYMD       TO  WRK-LST-SRYYMD
               ELSE
                   MOVE    JYURRK-SRYYMD       TO  WRK-NYULST-SRYYMD
               END-IF
               MOVE    1                   TO  FLG-JYURRK-ARI
           END-IF
      *
      *    ¿ÇÎÅ²Ê¤Î¼õ¿ÇÍúÎò¤¬¤Ê¤¤»þ¡¢Á´²Ê
           IF      FLG-JYURRK-ARI      =   ZERO
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                      JYURRK-REC
               MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    SPA-PTID            TO  JYURRK-PTID
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-JYURRK
               END-IF
               PERFORM
                   UNTIL      (FLG-JYURRK      =   1   )
      *            Êñ³ç¤ÏÂÐ¾Ý³°
                   IF      JYURRK-HKNCOMBINUM    NOT =   "9999"
      *
                       IF      JYURRK-NYUGAIKBN    =   "2"
                           MOVE    JYURRK-SRYYMD   TO  WRK-LST-SRYYMD2
                        ELSE
                           MOVE    JYURRK-SRYYMD   TO
                                                   WRK-NYULST-SRYYMD2
                       END-IF
                       MOVE    1               TO  FLG-JYURRK
                       MOVE    1               TO  FLG-JYURRK-ARI
                   END-IF
      *
                   IF      FLG-JYURRK          =   ZERO
                       MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 970-JYURRK-NEXT-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *TEST
      *    DISPLAY "WRK-LST-SRYYMD:" WRK-LST-SRYYMD
      *            ",WRK-LST-SRYYMD2:"  WRK-LST-SRYYMD2
      *    DISPLAY "WRK-NYULST-SRYYMD:" WRK-NYULST-SRYYMD
      *            ",WRK-NYULST-SRYYMD2:"  WRK-NYULST-SRYYMD2
      *TEST
      *
      *    ³°Íè¤¬¤Ê¤¯¡¢Æþ±¡¤Î¤Î¤ß¤Î»þ¡¢Æþ±¡¤ÎºÇ½ªÍúÎòÆü¤òÊÔ½¸¤¹¤ë
           IF     (WRK-LST-SRYYMD      =   SPACE )  AND
                  (WRK-LST-SRYYMD2     =   SPACE )
               IF     (WRK-NYULST-SRYYMD       =   SPACE )  AND
                      (WRK-NYULST-SRYYMD2      =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    WRK-NYULST-SRYYMD   TO  WRK-LST-SRYYMD
                   MOVE    WRK-NYULST-SRYYMD2  TO  WRK-LST-SRYYMD2
               END-IF
           ELSE
      *        Æþ±¡¤ÎºÇ½ªÍúÎòÆü¤òÊÔ½¸¤¹¤ë
               IF     (WRK-NYULST-SRYYMD   NOT =   SPACE ) AND
                      (WRK-LST-SRYYMD      <   WRK-NYULST-SRYYMD)
                   MOVE    WRK-NYULST-SRYYMD   TO  WRK-LST-SRYYMD
               END-IF
           END-IF
      *
      *    ºÇ½ª¼õ¿ÇÆü¡ÊÂà±¡Æü¡Ë
           IF      SKARRK-LASTYMD  NOT =   SPACE
               IF      SKARRK-LASTYMD      >=  WRK-LST-SRYYMD
                   MOVE    SKARRK-LASTYMD  TO  WRK-LST-SRYYMD
               END-IF
           END-IF
           MOVE    WRK-LST-SRYYMD      TO  KARRK-LASTYMD
      *
           MOVE    ZERO                TO  IDS1
           MOVE    ZERO                TO  IDS2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   100 )
                           OR (TBL-SYOSIN-SRYKA(IDX2)  =   SPACE)
               IF      TBL-SYOSIN-SRYKA (IDX2) =   SKARRK-SRYKA
                   MOVE    IDX2            TO  IDS1
               ELSE
                   MOVE    IDX2            TO  IDS2
               END-IF
           END-PERFORM
      *
      *    ¿ÇÎÅ²Ê¤Î½é¿Ç¤¢¤ê
           IF      IDS1                >   ZERO
      *        ¿ÇÎÅ²Ê¤Î½é¿ÇÎÁ¤¢¤ê
               IF      TBL-SYOSIN-YMD (IDS1)   <   WRK-SYOSINYMD2
      *            »»ÄêÍúÎò¤Î¤ß¤Î¾ì¹ç
                   MOVE    WRK-SYOSINYMD2          TO  KARRK-SYOSINYMD2
               ELSE
                   MOVE    TBL-SYOSIN-YMD (IDS1)   TO  KARRK-SYOSINYMD2
               END-IF
               IF     (KARRK-SYOSINYMD1        =   SPACE           ) OR
                      (TBL-SYOSIN-FST (IDS1)   <   KARRK-SYOSINYMD1)
                   MOVE    TBL-SYOSIN-FST (IDS1)   TO  KARRK-SYOSINYMD1
               END-IF
           ELSE
      *        ¿ÇÎÅ²Ê¤Î½é¿ÇÎÁ¤Ê¤·
               IF      WRK-LST-SRYYMD  NOT =   SPACE
      *            ¿ÇÎÅ²Ê¤Î¼õ¿Ç¤¢¤ê¡¢½é¿ÇÎÁ»»Äê¤¬¤Ê¤¤»þ¡¢»»ÄêÍúÎò¤ÎÆü
                   IF      IDS2                =   ZERO
                       MOVE    WRK-SYOSINYMD2      TO  KARRK-SYOSINYMD2
                   ELSE
      *                °Ü¹Ô½é¿ÇÆü¤¬½é¿ÇÆü¤Î»þ
                       IF     (WRK-SYOSINYMD1  NOT =   SPACE )
                       AND    (WRK-SYOSINYMD1      =   KARRK-SYOSINYMD1)
                       AND    (WRK-SYOSINYMD2      =   KARRK-SYOSINYMD2)
                           CONTINUE
                       ELSE
                           MOVE    SPACE           TO  KARRK-SYOSINYMD2
                       END-IF
                   END-IF
               END-IF
      *        Â¾²Ê¼õ¿Ç¤¢¤ê
               IF     (WRK-LST-SRYYMD      =   SPACE )   AND
                      (WRK-LST-SRYYMD2 NOT =   SPACE )
                   MOVE    SPACE           TO  KARRK-SYOSINYMD1
                   MOVE    SPACE           TO  KARRK-SYOSINYMD2
                   MOVE    SPACE           TO  KARRK-LASTYMD
               END-IF
      *        »»ÄêÍúÎò¤Î¤ß
               IF     (WRK-LST-SRYYMD      =   SPACE )   AND
                      (WRK-LST-SRYYMD2     =   SPACE )
      *            Â¾¤Î¿ÇÎÅ²ÊÍúÎò¥Á¥§¥Ã¥¯
                   PERFORM 3001-TAKARRK-CHK-SEC
                   IF      FLG-TAKARRK         =   ZERO
                       MOVE    WRK-SYOSINYMD1      TO  KARRK-SYOSINYMD1
                       MOVE    WRK-SYOSINYMD2      TO  KARRK-SYOSINYMD2
                       MOVE    WRK-SYOSINYMD2      TO  KARRK-LASTYMD
                   ELSE
                       MOVE    SPACE           TO  KARRK-SYOSINYMD1
                       MOVE    SPACE           TO  KARRK-SYOSINYMD2
                       MOVE    SPACE           TO  KARRK-LASTYMD
                   END-IF
               END-IF
           END-IF
      *    ½é¿Ç»»ÄêÆü£±¤Ï»»ÄêÆü£²¤ÎÁ°¤Ç¤¢¤ë¤³¤È
           IF     (KARRK-SYOSINYMD1    NOT =   SPACE )  AND
                  (KARRK-SYOSINYMD2    NOT =   SPACE )
               IF      KARRK-SYOSINYMD1    >   KARRK-SYOSINYMD2
                   MOVE    KARRK-SYOSINYMD2    TO  KARRK-SYOSINYMD1
               END-IF
           END-IF
      *
      *    ¤¹¤Ù¤Æ¤ÎÍúÎò¤¬¤Ê¤¤»þ¡¢Â¾¤Î¿ÇÎÅ²ÊÍúÎò¤âºï½ü¤¹¤ë
           IF     (FLG-JYURRK-ARI      =   ZERO  )  AND
                  (WRK-SYOSINYMD1      =   SPACE )  AND
                  (WRK-SYOSINYMD2      =   SPACE )
      *    ºÇ½ª¼õ¿ÇÆü¡ÊÂà±¡Æü¡Ë
               AND (SKARRK-LASTYMD     =   SPACE )
               PERFORM 3009-SRYKARRK-DEL-SEC
               GO      TO      300-SRYKARRK-HEN-EXT
           END-IF
      *
      *    ¤¹¤Ù¤Æ¤ÎÆüÉÕ¤¬¶õÇò¤Î»þ¡¢ºï½ü¤¹¤ë
           IF     (KARRK-SYOSINYMD1        =   SPACE )  AND
                  (KARRK-SYOSINYMD2        =   SPACE )  AND
                  (KARRK-LASTYMD           =   SPACE )
               IF      FLG-SRYKARRK-ARI        =   1
                   MOVE    SRYKARRK-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_srykarrk"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "ORCSCKARRK SRYKARRK DEL ERR:"
                                                    MCP-RC  "."
                       MOVE    "3000"          TO  SPA-ERRCD
                   END-IF
      *            Â¾¤Î¿ÇÎÅ²Ê¤ÎÆâÍÆ³ÎÇ§
                   PERFORM 3005-DEL-SYORI-SEC
              END-IF
              GO      TO      300-SRYKARRK-HEN-EXT
           END-IF
      *
           IF      FLG-SRYKARRK-ARI        =   1
      *        ¹¹¿·
               MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SKARRK-SRYKA        TO  KARRK-SRYKA
      *
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SPA-OPID            TO  KARRK-OPID
      *
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSCKARRK SRYKARRK UPD ERR:"  MCP-RC  "."
                   MOVE    "3000"          TO  SPA-ERRCD
               END-IF
           ELSE
      *        ¿·µ¬
               MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SKARRK-SRYKA        TO  KARRK-SRYKA
      *
               MOVE    SMCNDATE-YMD        TO  KARRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SPA-OPID            TO  KARRK-OPID
      *
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSCKARRK SRYKARRK INS ERR:"  MCP-RC  "."
                   MOVE    "3000"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       300-SRYKARRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾¤Î¿ÇÎÅ²ÊÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       3001-TAKARRK-CHK-SEC         SECTION.
      *
           MOVE    SRYKARRK-REC        TO  HZN-SRYKARRK-REC
      *
           MOVE    ZERO                TO  FLG-TAKARRK
      *    º£²ó¿ÇÎÅ²Ê°Ê³°¤Î¿ÇÎÅ²ÊÍúÎò
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
           MOVE    SKARRK-SRYKA        TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYKARRK        =   ZERO
               MOVE    1                   TO  FLG-TAKARRK
           END-IF
      *
           MOVE    HZN-SRYKARRK-REC    TO  SRYKARRK-REC
      *
           .
       3001-TAKARRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾¤Î¿ÇÎÅ²Ê¤ÎÆâÍÆ³ÎÇ§½èÍý
      *****************************************************************
       3005-DEL-SYORI-SEC         SECTION.
      *
      *    ¿ÇÎÅ²ÊÍúÎòÆÉ¹þ
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-SRYKARRK-REC
           PERFORM UNTIL    FLG-SRYKARRK   =   1
               IF      KARRK-SYOSINYMD2    NOT =   SPACE
                   MOVE    1                   TO  FLG-OK
                   MOVE    1                   TO  FLG-SRYKARRK
               ELSE
                   IF     (WRK-SRYKARRK-REC    =   SPACE )  OR
                          (KARRK-SYOSINYMD1    =   WRK-SYOSINYMD2)
                       MOVE    SRYKARRK-REC        TO  WRK-SRYKARRK-REC
                   END-IF
                   MOVE    "tbl_srykarrk"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 975-SRYKARRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF     (WRK-SRYKARRK-REC    NOT =   SPACE )  AND
                  (FLG-OK              =   ZERO      )
               MOVE    WRK-SRYKARRK-REC    TO  SRYKARRK-REC
               MOVE    WRK-SYOSINYMD2      TO  KARRK-SYOSINYMD2
      *        ½é¿Ç»»ÄêÆü£±¤Ï»»ÄêÆü£²¤ÎÁ°¤Ç¤¢¤ë¤³¤È
               IF     (KARRK-SYOSINYMD1    NOT =   SPACE )  AND
                      (KARRK-SYOSINYMD2    NOT =   SPACE )
                   IF      KARRK-SYOSINYMD1    >   KARRK-SYOSINYMD2
                       MOVE    KARRK-SYOSINYMD2    TO  KARRK-SYOSINYMD1
                   END-IF
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SPA-OPID            TO  KARRK-OPID
      *
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSCKARRK SRYKARRK UPD ERR:"  MCP-RC  "."
                   MOVE    "3000"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       3005-DEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎòºï½ü½èÍý
      *****************************************************************
       3009-SRYKARRK-DEL-SEC         SECTION.
      *
      *    ¿ÇÎÅ²ÊÍúÎò
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       3009-SRYKARRK-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
      *910-TENSU-READ-SEC         SECTION.
      *
      *    MOVE    SKARRK-SRYYM        TO  TNS-YUKOSTYMD(1:6)
      *    MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
      *    MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
      *    MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *    MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *    MOVE    "tbl_tensu"         TO  MCP-TABLE
      *    MOVE    "key22"             TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_tensu"         TO  MCP-TABLE
      *        MOVE    "key22"             TO  MCP-PATHNAME
      *        PERFORM 920-DBFETCH-SEC
      *        IF      MCP-RC              =   ZERO
      *            MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
      *            MOVE    ZERO                TO  FLG-TENSU
      *        ELSE
      *            INITIALIZE                  TNS-TENSU-REC
      *            MOVE    1                   TO  FLG-TENSU
      *        END-IF
      *    ELSE
      *        INITIALIZE                  TNS-TENSU-REC
      *        MOVE    1                   TO  FLG-TENSU
      *    END-IF
      *
      *    MOVE    "tbl_tensu"         TO  MCP-TABLE
      *    MOVE    "key22"             TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *
      *    .
      *910-TENSU-READ-EXT.
      **** EXIT.
      *
      *****************************************************************
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÎò¥Ç¡¼¥¿¼ç¥­¡¼¸¡º÷
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¼õ¿ÇÍúÎò½çÆÉ¤ß
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       920-SRYACT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷ÆÉ¹þ½èÍý
      *****************************************************************
       940-SRYACCT-KEYREAD-SEC         SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-SRYACCT-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
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
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
