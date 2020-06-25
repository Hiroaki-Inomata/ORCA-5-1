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
       PROGRAM-ID.             ORCGW32.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóÀßÄê
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Êñ³ç¿ÇÎÅ¹Ô°ÙÀßÄê¾ðÊó¡Ê£×£³£²¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/02/21    NACL¡ÝÂ¿¡¹Ç¼  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/05/02  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.04.00    NACL-Â¿¡¹Ç¼  08/12/10  ÎÅÍÜÉÂÅïÆþ±¡ÎÁÄÉ²Ã
      *  04.05.00    NACL-Â¿¡¹Ç¼  09/09/XX  ËýÀ­°Ý»ýÆ©ÀÏ´µ¼Ô³°Íè´ÉÍýÎÁ¡¢
      *                                     ¾®»ù²Ê³°Íè¿Ç»¡ÎÁÅùÄÉ²Ã
      *  04.06.00    NACL-Â¿¡¹Ç¼  10/10/XX  ºßÂð»þ°åÎÅÁí¹ç´ÉÍýÎÁ¡¦
      *                                     ºßÂðËö´üÁí¹ç¿Ç»¡ÎÁÄÉ²Ã
      *  04.06.00    NACL-Â¿¡¹Ç¼  11/01/XX  ÅÅ»ÒÅÀ¿ôÉ½ÂÐ±þ
      *                                     ³ÆÆþ±¡ÎÁÅùÄÉ²Ã
      *  04.07.00    NACL-Â¿¡¹Ç¼  14/03/XX  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.00    NACL-Â¿¡¹Ç¼  14/04/24  ¾®»ùÆÃÄê½¸Ãæ´ÉÍýÎÁÅùÄÉ²Ã
      *  04.08.00    NACL-Â¿¡¹Ç¼  16/03/XX  Ê¿À®£²£¸Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  16/04/XX  ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÊñ³ç³°ÂÐ±þÂ¾
      *                                     ¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  18/01/24  ¿ÇÎÅ¶èÊ¬Êñ³çÂÐ¾Ý²þÀµÂÐ±þÂ¾
      *  04.08.00    NACL-Â¿¡¹Ç¼  18/06/XX  Êñ³ç¿ÇÎÅ¥Æ¡¼¥Ö¥ë´ü´ÖÂÐ±þÂ¾
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóÀßÄêÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "W01COMMON-SPA".
      *
      *    ²èÌÌÍÑ£Ó£Ð£Á
           COPY    "W32SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKTSRYCD        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    Åº»úÎÎ°è
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *
           03  IDX2                PIC 9(02).
      *
      *    °ì»þÎÎ°è
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-PATH            PIC X(64).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    ¿¦°÷¶èÊ¬
           03  WRK-MAE-KBNCD-G.
               05  WRK-MAE-KBNCD       PIC X(03).
               05  WRK-MAE-KBNCDF      PIC X(01).
               05  WRK-MAE-KBNCDMEI    PIC X(38).
      *
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-CD              PIC X(02).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY  "CPSYSKANRI.INC".
      *
           COPY  "CPSK1014.INC".
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¥Æ¡¼¥Ö¥ë
       01  HKTSRYCD-REC.
           COPY  "CPHKTSRYCD.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÆüÉÕ¥µ¥Ö¥ë¡¼¥Á¥óÎÎ°è
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *   ²èÌÌÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSGDAY.INC".
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W32
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ²èÌÌÁ«°Ü
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ÆþÎÏ
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    ²èÌÌÁ«°Ü¤¬¤Ê¤¤»þ¡¢²èÌÌÉ½¼¨¤Ø
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W32         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ¥¨¥é¡¼²èÌÌ¤è¤ê
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ½é´ü²èÌÌ¥»¥Ã¥È
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            ²èÌÌÊÔ½¸
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W32"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é´ü²èÌÌ½èÍý
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    Â¾¤Î£Ì£Ä¤è¤ê
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    "W321"
                   MOVE    SPACE           TO  SPA-WID1-FLG
                   MOVE    SPACE           TO  SPA-WIDCD
                   MOVE    1               TO  SPA-GMN-CUR
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³ÎÇ§²èÌÌ¡Ê£Ã£É£Ä£±¡Ë£Ï£Ë½èÍý
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
               WHEN    "1003"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¹¥Ñ½é´üÊÔ½¸½èÍý
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W32
      *
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¤è¤ê´ÉÍýÈÖ¹æ·èÄê
      *    RENNUM=1 ¤ò¸¡º÷
           MOVE    SPACE               TO  HKTSRYCD-REC
           INITIALIZE                      HKTSRYCD-REC
      *    MOVE    SPA-SYSYMD          TO  HKTSRYCD-YUKOSTYMD
      *    MOVE    SPA-SYSYMD          TO  HKTSRYCD-YUKOEDYMD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKTSRYCD
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-HKTSRYCD   =   1  )
                       OR  (IDX            >=  200)
               IF     (IDX             >   ZERO )
                 AND  (HKTSRYCD-KANRICD
                                           =   SPA-GMN-TKBNCD (IDX))
                   IF      HKTSRYCD-YUKOEDYMD  >   SPA-NAIB-YUKOEDYMD
                                                             (IDX)
                       MOVE    HKTSRYCD-YUKOEDYMD
                                               TO  SPA-NAIB-YUKOEDYMD
                                                             (IDX)
                   END-IF
               ELSE
                   ADD    1                TO  IDX
                   MOVE    HKTSRYCD-KANRICD    TO  SPA-GMN-TKBNCD
                                                             (IDX)
                   MOVE    HKTSRYCD-YUKOSTYMD  TO  SPA-NAIB-YUKOSTYMD
                                                             (IDX)
                   MOVE    HKTSRYCD-YUKOEDYMD  TO  SPA-NAIB-YUKOEDYMD
                                                             (IDX)
                   PERFORM 3101-KBNCDMEI-HEN-SEC
      *H30.6
      *            ºÇÂç½ªÎ»ÆüÀßÄê
                   IF      HKTSRYCD-YUKOEDYMD  <   "99999999"
                       PERFORM 3102-HKTSYRCD-END-SEC
                   END-IF
      *
                   MOVE    IDX                 TO  SPA-KBNCD-MAX
               END-IF
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-HKTSRYCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    Êñ³ç»»ÄêÊýË¡
           MOVE    "0 Êñ³ç»»Äê¤·¤Ê¤¤"
                                       TO  SPA-GMN-SANTEIHOU-LST (1)
           MOVE    "1 Êñ³ç»»Äê¤¹¤ë¡Ê²ñ·×¥Ç¡¼¥¿ºîÀ®¡Ë"
                                       TO  SPA-GMN-SANTEIHOU-LST (2)
           MOVE    "2 Êñ³ç»»Äê¤¹¤ë¡Ê¥¨¥é¡¼°·¤¤¡Ë"
                                       TO  SPA-GMN-SANTEIHOU-LST (3)
           MOVE    3                   TO  SPA-SANTEIHOU-MAX
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    "1 ²ñ·×Æâ¤Ë¤ª¤¤¤ÆÊñ³ç¤¹¤ë"
                                       TO  SPA-GMN-SANTEIKBN-LST (1)
           MOVE    "2 »»ÄêÆü¤Ë¤ª¤¤¤ÆÊñ³ç¤¹¤ë"
                                       TO  SPA-GMN-SANTEIKBN-LST (2)
           MOVE    "3 »»Äê·î¤Ë¤ª¤¤¤ÆÊñ³ç¤¹¤ë"
                                       TO  SPA-GMN-SANTEIKBN-LST (3)
           MOVE    "4 »»ÄêÆü°Ê¹ß¤Ë¤ª¤¤¤ÆÊñ³ç¤¹¤ë"
                                       TO  SPA-GMN-SANTEIKBN-LST (4)
           MOVE    4                   TO  SPA-SANTEIKBN-MAX
      *    Æþ³°¶èÊ¬
           MOVE    "1 Æþ±¡¡¦³°ÍèÊÌ¤ËÊñ³ç¤¹¤ë" 
                                       TO  SPA-GMN-NYUGAIKBN-LST(1) 
           MOVE    "2 Æþ±¡¡¦³°Íè¤Ë´Ø·¸¤Ê¤¯Êñ³ç¤¹¤ë" 
                                       TO  SPA-GMN-NYUGAIKBN-LST(2) 
           MOVE    2                   TO  SPA-NYUGAIKBN-MAX
      *    ÊÝ¸±ÁÈ¹ç¶èÊ¬
           MOVE    "0 ÊÝ¸±ÁÈ¹ç¤»¤Ë´Ø·¸¤Ê¤¯Êñ³ç¤¹¤ë" 
                                       TO  SPA-GMN-HKNCOMBIKBN-LST(1) 
           MOVE    "1 Ï«ºÒ¡¦¼«ÇåÀÕ¡¦·òÊÝËè¤ËÊñ³ç¤¹¤ë" 
                                       TO  SPA-GMN-HKNCOMBIKBN-LST(2) 
           MOVE    2                   TO  SPA-HKNCOMBIKBN-MAX
      *    ¿ÇÎÅ¶èÊ¬²èÌÌ
           MOVE    13                  TO   SPA-GMN-SRYKBN-MAX
      *    ¿ÇÎÅ¶èÊ¬¡Ê¶¦ÄÌ¡Ë
           MOVE    "0 Êñ³ç¤·¤Ê¤¤"      TO  SPA-GMN-SRYKBN-LST (1)
           MOVE    "1 Êñ³ç¤¹¤ë"        TO  SPA-GMN-SRYKBN-LST (2)
           MOVE    2                   TO  SPA-SRYKBN-MAX
      *
      *    ½é´üÉ½¼¨
           MOVE    SPA-GMN-KBNCD-LST(1)   TO  SPA-GMN-KBNCD-G
           MOVE    SPA-NAIB-YUKOSTYMD (1) TO  SPA-HKT-YUKOSTYMD
           MOVE    SPA-NAIB-YUKOEDYMD (1) TO  SPA-HKT-YUKOEDYMD
           IF      SPA-KBNCD-MAX       =   1
               PERFORM 320-KANRICD-HENSYU-SEC
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÆâÍÆÊÔ½¸½èÍý
      *****************************************************************
       3101-KBNCDMEI-HEN-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-TKBNCD (IDX)
               WHEN    "001"
                   MOVE    "³°Íè¿ÇÎÅÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "501"
                   MOVE    "ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "502"
      *H22.3 ÇÑ»ß  MOVE    "¸å´ü¹âÎð¼ÔÆÃÄêÆþ±¡´ðËÜÎÁ" 
                   MOVE    "ÆÃÄêÆþ±¡´ðËÜÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "503"
                   MOVE    "Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *
               WHEN    "201"
                   MOVE    "ËýÀ­°Ý»ýÆ©ÀÏ´µ¼Ô³°Íè°å³Ø´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "202"
                   MOVE    "¾®»ù²Ê³°Íè¿ÇÎÅÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "203"
                   MOVE    "À¸³è½¬´·ÉÂ´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *ver.4.6
               WHEN    "204"
                   MOVE    "ºßÂð»þ°å³ØÁí¹ç´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "205"
      *H28.4       MOVE    "ÆÃÄê»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ" 
                   MOVE    "»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "206"
      *H24.4       MOVE    "ºßÂðËö´ü°åÎÅÁí¹ç¿ÇÎÅÎÁ" 
                   MOVE    "ºßÂð¤¬¤ó°åÎÅÁí¹ç¿ÇÎÅÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *H26.4
               WHEN    "207"
                   MOVE    "ÃÏ°èÊñ³ç¿ÇÎÅÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "208"
                   MOVE    "Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£±" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *H28.4
               WHEN    "209"
                   MOVE    "¾®»ù¤«¤«¤ê¤Ä¤±¿ÇÎÅÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *Æþ±¡
               WHEN    "504"
                   MOVE    "²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "505"
                   MOVE    "°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "506"
                   MOVE    "µßÌ¿µßµÞÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "507"
                   MOVE    "ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡¦¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "508"
                   MOVE    "Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "509"
                   MOVE    "¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "510"
                   MOVE    "Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "511"
                   MOVE    "¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "512"
                   MOVE    "°ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "513"
                   MOVE    "ÆÃ¼ì¼À´µÆþ±¡°åÎÅ´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "514"
                   MOVE    "¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡¢£²" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "515"
                   MOVE    "¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£³¡¢£´" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "516"
                   MOVE    "¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£µ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "517"
                   MOVE    "ÆÃ¼ì¼À´µÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "518"
                   MOVE    "´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "519"
                   MOVE    "Àº¿À²ÊµßµÞÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "520"
                   MOVE    "Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "521"
                   MOVE    "Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "522"
                   MOVE    "Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "523"
                   MOVE    "Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *H26.4
               WHEN    "524"
                   MOVE    "ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "525"
                   MOVE    "Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£²" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "526"
                   MOVE    "Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *H26.4
               WHEN    "527"
                   MOVE    "¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "528"
                   MOVE    "»ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍýÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "529"
                   MOVE    "ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *H28.4
               WHEN    "530"
                   MOVE    "ÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "531"
                   MOVE    "¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ¡Ê°åÎÅ¶èÊ¬»»Äê´µ¼Ô¡Ë"
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
           END-EVALUATE
           . 
       3101-KBNCDMEI-HEN-EXT.
           EXIT.
      *
      *H30.6
      *****************************************************************
      *    Êñ³ç»»Äê½ªÎ»ÆüÊÔ½¸½èÍý
      *****************************************************************
       3102-HKTSYRCD-END-SEC              SECTION.
      *
      *    ºÇ½ª½ªÎ»ÆüÀßÄê
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKTSRYCD
           END-IF
           PERFORM UNTIL   (FLG-HKTSRYCD   =   1  )
      *
               IF      HKTSRYCD-YUKOEDYMD  >   SPA-NAIB-YUKOEDYMD
                                                             (IDX)
                   MOVE    HKTSRYCD-YUKOEDYMD  TO  SPA-NAIB-YUKOEDYMD
                                                             (IDX)
               END-IF
               IF      HKTSRYCD-YUKOEDYMD      =   "99999999"
                   MOVE    1                   TO  FLG-HKTSRYCD
               ELSE
                   MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 900-HKTSRYCD-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-HKTSRYCD
           .
       3102-HKTSYRCD-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç»»Äê½é´üÉ½¼¨ÊÔ½¸½èÍý
      *****************************************************************
       3101-INIT-GMN-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYORI
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-NAIB-AREA
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       3101-INIT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç´ÉÍýÈÖ¹æ¤ÎÊÔ½¸½èÍý 
      *****************************************************************
       320-KANRICD-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-NAIB-AREA
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1014-KBNCD (1:3)
           MOVE    "%"                 TO  SYS-1014-KBNCD (4:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM  VARYING    IDX     FROM    1   BY  1
               UNTIL          (IDX     >       99    )  OR
                              (FLG-SYSKANRI    =   1 )
               MOVE    MCPDATA-REC         TO  SYS-1014-REC
               ADD     1                   TO  SPA-GMN-TBL-MAX
               MOVE    IDX                 TO  SPA-GMN-TBANGO    (IDX)
               MOVE    SYS-1014-STYUKYMD   TO  SPA-NAI-TSTYUKYMD (IDX)
               MOVE    SYS-1014-STYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD (IDX)
               MOVE    SYS-1014-EDYUKYMD   TO  SPA-NAI-TEDYUKYMD (IDX)
               MOVE    SYS-1014-EDYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ´ÉÍýÈÖ¹æ¤Ë¤è¤êÊñ³çÁªÂò¤Î·èÄê
           PERFORM 3201-INIT-TBL-SEC
           MOVE    ZERO                TO  SPA-NAI-KEIFLG
           IF      SPA-GMN-TBL-MAX     =   ZERO
      *        ¿·µ¬
               MOVE    1                   TO  SPA-GMN-SYORI
               MOVE    3                   TO  SPA-GMN-CUR
               PERFORM 200212-INIT-GMN-SEC
           ELSE
               MOVE    2                   TO  SPA-GMN-SYORI
               IF      SPA-GMN-TBL-MAX     =   1
      *            £±·ï¤òÁªÂò¤È¤¹¤ë
                   MOVE    1                   TO  SPA-GMN-SELNUM
                   MOVE    SPA-GMN-SELNUM      TO  W32-SELNUM
                   PERFORM 2003-SELNUM-CHK-SEC
               ELSE
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       320-KANRICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍýÈÖ¹æ¤Ë¤è¤êÊñ³çÁªÂò¤Î·èÄê½èÍý
      *****************************************************************
       3201-INIT-TBL-SEC              SECTION.
      *
      *    EVALUATE    SPA-GMN-KBNCD
      *        WHEN    "001"
      *            ³°Íè¿ÇÎÅÎÁ ¤¹¤Ù¤Æ¤òÊñ³ç¤·¤Ê¤¤
      *            MOVE    1               TO  SPA-SANTEIKBN-MAX
      *            MOVE    1               TO  SPA-NYUGAIKBN-MAX
      *            MOVE    1               TO  SPA-HKNCOMBIKBN-MAX
      *            MOVE    1               TO  SPA-SRYKBN-MAX
      *        WHEN    OTHER
      *            MOVE    4               TO  SPA-SANTEIKBN-MAX
      *            MOVE    2               TO  SPA-NYUGAIKBN-MAX
      *            MOVE    2               TO  SPA-HKNCOMBIKBN-MAX
      *            MOVE    2               TO  SPA-SRYKBN-MAX
      *    END-EVALUATE
      *
           . 
       3201-INIT-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÌÌÁ«°Ü½èÍý
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    Ìá¤ë
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    ¥¯¥ê¥¢
               WHEN    "CLICKED"      ALSO    "B02"
                   PERFORM  402-CLERA-SEC
      *    ºï½ü
               WHEN    "CLICKED"      ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    ÅÐÏ¿
               WHEN    "CLICKED"      ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    Êñ³ç°ìÍ÷¤Ø
               WHEN    "CLICKED"      ALSO    "B09"
                   PERFORM 4090-W321-ALL-SEC
      *    Êñ³ç°ìÍ÷¤Ø
               WHEN    "CLICKED"      ALSO    ANY
                   IF      MCP-WIDGET(1:2)    =   "BK"
                       MOVE    MCP-WIDGET         TO  WRK-MCP-WIDGET
                       PERFORM 4091-W321-SEL-SEC
                   END-IF
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ²ñÏÃ¡¡½èÍý
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        Êñ³ç´ÉÍýÈÖ¹æ
               WHEN    "ACTIVATE"      ALSO    "KBNCD"
                   PERFORM 2001-KBNCD-SYORI-SEC
      *        ¿¦°÷ÈÖ¹æÆþÎÏ
      *        ÈÖ¹æÁªÂò
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 2003-SELNUM-CHK-SEC
      *        ÌÀºÙÁªÂò
               WHEN    ANY             ALSO    "KIKAN_LIST"
                   PERFORM 2004-KIKANLIST-SEC
      *
               WHEN    OTHER
      *            ÆþÎÏ¥Á¥§¥Ã¥¯½èÍý
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç´ÉÍýÈÖ¹æÆþÎÏ½èÍý
      *****************************************************************
       2001-KBNCD-SYORI-SEC         SECTION.
      *
           MOVE    SPA-GMN-KBNCD-G     TO  WRK-MAE-KBNCD-G
      *
           MOVE    W32-KBNCD           TO  SPA-GMN-KBNCD-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KBNCD-MAX
               IF      SPA-GMN-KBNCD       =   SPA-GMN-TKBNCD(IDX)
                   MOVE    SPA-GMN-KBNCD-LST (IDX)
                                           TO  SPA-GMN-KBNCD-G
                   MOVE    SPA-NAIB-YUKOSTYMD(IDX)
                                           TO  SPA-HKT-YUKOSTYMD
                   MOVE    SPA-NAIB-YUKOEDYMD (IDX)
                                           TO  SPA-HKT-YUKOEDYMD
                   MOVE    SPA-KBNCD-MAX   TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               INITIALIZE                      SPA-GMN-AREA2
               INITIALIZE                      SPA-NAIB-AREA
      *        Êñ³ç´ÉÍýÈÖ¹æÊÔ½¸
               PERFORM 320-KANRICD-HENSYU-SEC
           END-IF
      *
           .
       2001-KBNCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿·µ¬½é´üÉ½¼¨½èÍý
      *****************************************************************
       200212-INIT-GMN-SEC       SECTION.
      *
      *    Êñ³ç»»ÄêÊýË¡
           MOVE    SPA-GMN-SANTEIHOU-LST (1)   TO  SPA-GMN-SANTEIHOU-G
      *    Êñ³ç»»Äê¶èÊ¬°Ê²¼¤Ï¶èÊ¬Ëè¤Î½é´üÉ½¼¨
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            ³°Íè¿ÇÎÅÎÁ
                   PERFORM 200211-KBNCD-001-SEC
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
      *            ÎÅÍÜÉÂÅïÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-501-SEC
               WHEN    "201"
      *            ËýÀ­°Ý»ýÆ©ÀÏ´µ¼Ô´ÉÍýÎÁ
                   PERFORM 200211-KBNCD-201-SEC
               WHEN    "202"
      *            ¾®»ù²Ê³°Íè¿ÇÎÅÎÁ
               WHEN    "209"
      *            ¾®»ù¤«¤«¤ê¤Ä¤±¿ÇÎÅÎÁ
                   PERFORM 200211-KBNCD-202-SEC
               WHEN    "203"
      *            À¸³è½¬´·ÉÂ´ÉÍýÎÁ
                   PERFORM 200211-KBNCD-203-SEC
               WHEN    "204"
      *            ºßÂð»þ°å³ØÁí¹ç´ÉÍýÎÁ
                   PERFORM 200211-KBNCD-204-SEC
               WHEN    "205"
      *            ÆÃÄê»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ
                   PERFORM 200211-KBNCD-205-SEC
               WHEN    "206"
      *            ºßÂðËö´ü°åÎÅÁí¹ç¿ÇÎÅÎÁ
                   PERFORM 200211-KBNCD-206-SEC
               WHEN    "207"
      *            ÃÏ°èÊñ³ç¿ÇÎÅÎÁ
                   PERFORM 200211-KBNCD-207-SEC
               WHEN    "504"
      *            ²óÉü´ü¥ê¥ÏÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-504-SEC
               WHEN    "505"
      *            °¡µÞÀ­´üÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-505-SEC
               WHEN    "513"
               WHEN    "517"
      *            Êñ³ç¤¢¤ê
                   PERFORM 200211-KBNCD-OK-SEC
      *H30.1
               WHEN    "518"
      *            ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-518-SEC
               WHEN    "514"
               WHEN    "515"
               WHEN    "516"
      *            ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ
                   PERFORM 200211-KBNCD-514-SEC
               WHEN    "519"
               WHEN    "520"
               WHEN    "521"
      *            Àº¿À²ÊµßµÞÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-519-SEC
               WHEN    "522"
               WHEN    "523"
      *            Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-522-SEC
               WHEN    "524"
      *            ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-524-SEC
               WHEN    "208"
               WHEN    "525"
      *            Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£±
      *            Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²
                   PERFORM 200211-KBNCD-525-SEC
               WHEN    "526"
      *            Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
                   PERFORM 200211-KBNCD-526-SEC
               WHEN    "527"
      *            ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
                   PERFORM 200211-KBNCD-527-SEC
               WHEN    "528"
      *            »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍýÎÁ
                   PERFORM 200211-KBNCD-528-SEC
               WHEN    "529"
      *            ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-529-SEC
      *H28.4
               WHEN    "530"
      *            ÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ
                   PERFORM 200211-KBNCD-530-SEC
               WHEN    "531"
      *            ¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ
                   PERFORM 200211-KBNCD-531-SEC
               WHEN    OTHER
      *            Êñ³ç¤Ê¤·
                   PERFORM 200211-KBNCD-999-SEC
           END-EVALUATE
      *
           IF      SPA-GMN-TBL-MAX     >   ZERO
               GO  TO   200212-INIT-GMN-EXT
           END-IF
      *
      *    Êñ³ç¥Ç¡¼¥¿¤ÎÍ­¸ú´ü´Ö
           MOVE    SPA-HKT-YUKOSTYMD   TO  SPA-NAI-STYUKYMD
           MOVE    SPA-HKT-YUKOEDYMD   TO  SPA-NAI-EDYUKYMD
      *
           MOVE    SPA-NAI-STYUKYMD    TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
           .
       200212-INIT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    Êñ³ç¤¢¤êÀßÄê½èÍý
      *****************************************************************
       200211-KBNCD-OK-SEC            SECTION.
      *
           MOVE    SPA-GMN-SANTEIKBN-LST (1)   TO  SPA-GMN-SANTEIKBN-G
           IF      SPA-GMN-KBNCD(1:1)  =   "5"
               MOVE    SPA-GMN-SANTEIKBN-LST (2)
                                               TO  SPA-GMN-SANTEIKBN-G
           END-IF
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (2)
                                       TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
      *
           .
       200211-KBNCD-OK-EXT.
           EXIT.
      *****************************************************************
      *    ¤½¤ÎÂ¾¡ÊÊñ³ç¤Ê¤·¡ËÀßÄê½èÍý
      *****************************************************************
       200211-KBNCD-999-SEC       SECTION.
      *
           MOVE    SPA-GMN-SANTEIKBN-LST (1)   TO  SPA-GMN-SANTEIKBN-G
           IF      SPA-GMN-KBNCD(1:1)  =   "5"
               MOVE    SPA-GMN-SANTEIKBN-LST (2)
                                               TO  SPA-GMN-SANTEIKBN-G
           END-IF
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (1)
                                       TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
      *
           .
       200211-KBNCD-999-EXT.
           EXIT.
      *****************************************************************
      *    ³°Íè¿ÇÎÅÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-001-SEC       SECTION.
      *
           MOVE    SPA-GMN-SANTEIKBN-LST (1)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (1)
                                       TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
      *
           .
       200211-KBNCD-001-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-501-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =   1
                                       OR  2
                                       OR  3
                                       OR  5
                                       OR  10
                                       OR  13
      *            ÅêÌô¡¦Ãí¼Í¡¦½èÃÖ¡¦¸¡ºº¡¦ÉÂÍý¿ÇÃÇ¤¬Êñ³ç
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-501-EXT.
           EXIT.
      *****************************************************************
      *    ²óÉü´ü¥ê¥ÏÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-504-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =   7
      *            ¥ê¥Ï¥Ó¥ê
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-504-EXT.
           EXIT.
      *****************************************************************
      *    °¡µÞÀ­´üÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-505-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =   1
                                       OR  2
                                       OR  5
                                       OR  6 
                                       OR  10
      *            ÅêÌô¡¦Ãí¼Í¡¦¸¡ºº¡¦ÉÂÍý¿ÇÃÇ
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
           .
       200211-KBNCD-505-EXT.
           EXIT.
      *****************************************************************
      *    ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-514-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =   1
                                       OR  2
                                       OR  4
                                       OR  10
      *H30.2   Êü¼ÍÀþ¼£ÎÅ¡¡ÄÉ²Ã
                                       OR  9
      *            ÅêÌô¡¢Ãí¼Í¡¢¼ê½Ñ¡¦Ëã¿ì¡¢ÉÂÍý¿ÇÃÇ¡¡¤Ê¤·
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
      *H30.1   ºßÂðÄÉ²Ã
               IF     (SPA-HKT-YUKOSTYMD   >=  "20160401")
                 AND  (IDX                  =   12      )
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
           .
       200211-KBNCD-514-EXT.
           EXIT.
      *****************************************************************
      *    ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-518-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
      *        Êñ³ç¤¹¤ë
               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
      *        H28/4¤«¤é¡ÖÊñ³ç¤·¤Ê¤¤¡×¤¢¤ê
               IF      SPA-HKT-YUKOSTYMD   >=  "20160401"
                   IF      IDX             =   9
                                           OR  12
      *                Êü¼ÍÀþ¡¢ºßÂð¡¡¤Ê¤·
                       MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                   END-IF
               END-IF
           END-PERFORM
           .
       200211-KBNCD-518-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿À²ÊµßµÞÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-519-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =   4
                                       OR  8
                                       OR  9
      *            ¼ê½Ñ¡¦Ëã¿ì¡¢Àº¿À²ÊÀìÌç¡¢Êü¼ÍÀþ¡¡¤Ê¤·
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
           .
       200211-KBNCD-519-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-522-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =   8
      *            Àº¿À²ÊÀìÌç¡¡¤Ê¤·
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
           .
       200211-KBNCD-522-EXT.
           EXIT.
      *****************************************************************
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-524-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
      *H28.4
      *        ¼ê½ÑÊñ³çÂÐ¾Ý³°
               IF      SPA-HKT-YUKOSTYMD   >=  "20160401"
                   IF      IDX             =    04
                       MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                   END-IF
               END-IF
           END-PERFORM
           .
       200211-KBNCD-524-EXT.
           EXIT.
      *****************************************************************
      *    ËýÀ­°Ý»ýÆ©ÀÏ´µ¼Ô³°Íè°å³Ø´ÉÍý½èÍý
      *****************************************************************
       200211-KBNCD-201-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (3)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
      ***      IF      IDX             =    5
      *                                OR   6
      *            ¸¡ºº¡¦²èÁü¿ÇÃÇ¤¬Êñ³ç
      *            MOVE    SPA-GMN-SRYKBN-LST (2)
      *                                    TO  SPA-GMN-SRYKBN-G (IDX)
      ***      END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-201-EXT.
           EXIT.
      *****************************************************************
      *    ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-202-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
      *        Êñ³ç
               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
      *
           .
       200211-KBNCD-202-EXT.
           EXIT.
      *****************************************************************
      *    À¸³è½¬´·ÉÂ´ÉÍý½èÍý
      *****************************************************************
       200211-KBNCD-203-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (3)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    1
                                       OR   2
                                       OR   5
                                       OR   10
      *            ÅêÌô¡¦Ãí¼Í¡¦¸¡ºº¡¦ÉÂÍý¿ÇÃÇ¤¬Êñ³ç
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-203-EXT.
           EXIT.
      *****************************************************************
      *    ºßÂð»þ°å³ØÁí¹ç´ÉÍýÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-204-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (3)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    1
      *            ÅêÌô¤Î¤ßÊñ³ç
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-204-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄê»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-205-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (3)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    1
      *        ÅêÌô¤¬Êñ³ç
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-205-EXT.
           EXIT.
      *****************************************************************
      *    ºßÂðËö´ü°åÎÅÁí¹ç¿ÇÎÅÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-206-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    12
                                       OR   13
      *            ºßÂð¤¬Êñ³ç³°
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-206-EXT.
           EXIT.
      *****************************************************************
      *    ÃÏ°èÊñ³ç¿ÇÎÅÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-207-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (3)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    01
                                       OR   12
      *            ÅêÌô¡¦ºßÂð¡¡Êñ³ç³°
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-207-EXT.
           EXIT.
      *****************************************************************
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£±¡¢£²½èÍý
      *****************************************************************
       200211-KBNCD-525-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
      *        Êñ³ç¤Ê¤·
               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
           .
       200211-KBNCD-525-EXT.
           EXIT.
      *****************************************************************
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³½èÍý
      *****************************************************************
       200211-KBNCD-526-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
      *        Êñ³ç¤¢¤ê
               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
           .
       200211-KBNCD-526-EXT.
           EXIT.
      *****************************************************************
      *    ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-527-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
      *        Êñ³ç¤Ê¤·
               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
           .
       200211-KBNCD-527-EXT.
           EXIT.
      *****************************************************************
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍýÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-528-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    01
                                       OR   02
                                       OR   04
      *            ÅêÌô¡¦Ãí¼Í¡¦¼ê½Ñ¡ÊËã¿ì¡ËÊñ³ç³°
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
           .
       200211-KBNCD-528-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-529-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    12
      *            ºßÂð Êñ³ç³°
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
           .
       200211-KBNCD-529-EXT.
           EXIT.
      *****************************************************************
      *    ÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ½èÍý
      *****************************************************************
       200211-KBNCD-530-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
      *        ¤¹¤Ù¤ÆÊñ³ç
               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
           .
       200211-KBNCD-530-EXT.
           EXIT.
      *****************************************************************
      *    ¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ¡Ê°åÎÅ¶èÊ¬»»Äê´µ¼Ô¡Ë½èÍý
      *****************************************************************
       200211-KBNCD-531-SEC       SECTION.
      *
      *    Êñ³ç»»Äê¶èÊ¬
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    01
                                       OR   02
                                       OR   03
                                       OR   05
                                       OR   06
                                       OR   10
      *            ÅêÌô¡¦Ãí¼Í¡¦¸¡ºº¡¦ÉÂÍý¿ÇÃÇ¡¦²èÁü¿ÇÃÇ¡¦½èÃÖ¡¡Êñ³ç
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
           .
       200211-KBNCD-531-EXT.
           EXIT.
      *****************************************************************
      *    ÈÖ¹æÁªÂò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2003-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    W32-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF     (SPA-GMN-SELNUM      >=  1   )  AND
                  (SPA-GMN-SELNUM      <=  SPA-GMN-TBL-MAX)
               MOVE    SPA-GMN-SELNUM      TO  IDX
               MOVE    SPA-GMN-TSTYUKYMD(IDX)  TO  SPA-GMN-STYUKYMD
               MOVE    SPA-GMN-TEDYUKYMD(IDX)  TO  SPA-GMN-EDYUKYMD
      *
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-NAI-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-NAI-EDYUKYMD
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-MAE-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-MAE-EDYUKYMD
      *        Êñ³ç»»ÄêÊÔ½¸
               PERFORM 2009-HOUKATU-HENSYU-SEC
               MOVE    6               TO  SPA-GMN-CUR
      *H28.7
               MOVE    2               TO  SPA-GMN-SYORI
           ELSE
               IF      SPA-GMN-SELNUM      =   ZERO
      *            ÁªÂòÈÖ¹æ¤¬Ìµ¤¤»þ¡¢¿·µ¬ÄÉ²Ã¤È¤¹¤ë
                   MOVE    SPACE           TO  SPA-MAE-STYUKYMD
                   MOVE    SPACE           TO  SPA-MAE-EDYUKYMD
                   MOVE    1               TO  SPA-GMN-SYORI
                   MOVE    3               TO  SPA-GMN-CUR
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       2003-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´ü´Ö¥ê¥¹¥ÈÁªÂò½èÍý
      *****************************************************************
       2004-KIKANLIST-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               IF      W32-KIKAN-SELECT(IDX)   =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-TBL-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM
      *
               MOVE    SPA-GMN-SELNUM      TO  W32-SELNUM
               PERFORM 2003-SELNUM-CHK-SEC
           END-IF
           .
        2004-KIKANLIST-EXT.
           EXIT.
      *****************************************************************
      *    Êñ³ç»»ÄêÊÔ½¸½èÍý
      *****************************************************************
       2009-HOUKATU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1014-KBNCD
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1014-STYUKYMD 
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1014-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1014-REC
               MOVE    SYS-1014-SANTEI-HOU     TO  SPA-GMN-SANTEIHOU
               MOVE    SYS-1014-SANTEI-KBN     TO  SPA-GMN-SANTEIKBN
               MOVE    SYS-1014-NYUGAIKBN      TO  SPA-GMN-NYUGAIKBN
               MOVE    SYS-1014-HKNCOMBI-KBN   TO  SPA-GMN-HKNCOMBIKBN
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-SRYKBN-MAX
                   MOVE    SYS-1014-SRYKBN-KBN (IDX)
                                           TO  SPA-GMN-SRYKBN (IDX)
                   IF      SYS-1014-SRYKBN-KBN (IDX)   =   SPACE
                       MOVE    "0"         TO  SPA-GMN-SRYKBN (IDX)
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-SANTEIHOU-MAX
                   IF      SPA-GMN-SANTEIHOU-LST (IDX)(1:1)
                                           =   SPA-GMN-SANTEIHOU
                       MOVE    SPA-GMN-SANTEIHOU-LST (IDX)
                                           TO  SPA-GMN-SANTEIHOU-G
                       MOVE    SPA-SANTEIHOU-MAX   TO  IDX
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-SANTEIKBN-MAX
                   IF      SPA-GMN-SANTEIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-SANTEIKBN
                       MOVE    SPA-GMN-SANTEIKBN-LST (IDX)
                                           TO  SPA-GMN-SANTEIKBN-G
                       MOVE    SPA-SANTEIKBN-MAX   TO  IDX
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-NYUGAIKBN-MAX
                   IF      SPA-GMN-NYUGAIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-NYUGAIKBN
                       MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
                       MOVE    SPA-NYUGAIKBN-MAX   TO  IDX
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-HKNCOMBIKBN-MAX
                   IF      SPA-GMN-HKNCOMBIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-HKNCOMBIKBN
                       MOVE    SPA-GMN-HKNCOMBIKBN-LST (IDX)
                                           TO  SPA-GMN-HKNCOMBIKBN-G
                       MOVE    SPA-HKNCOMBIKBN-MAX   TO  IDX
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-GMN-SRYKBN-MAX
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >   SPA-SRYKBN-MAX
                       IF      SPA-GMN-SRYKBN-LST (IDY)(1:1)
                                           =   SPA-GMN-SRYKBN (IDX)
                           MOVE    SPA-GMN-SRYKBN-LST (IDY)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           MOVE    SPA-SRYKBN-MAX   TO  IDY
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           . 
       2009-HOUKATU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ÆþÎÏ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    ²èÌÌ¤ò£Ó£Ð£Á¥»¥Ã¥È
           PERFORM 4101-GMN-SPA-SET-SEC
      *    ´ðËÜ¥Á¥§¥Ã¥¯½èÍý
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÌÌ¤ò£Ó£Ð£Á¥»¥Ã¥È½èÍý
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W32-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    W32-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
      *
           MOVE    W32-SANTEIHOU       TO  SPA-GMN-SANTEIHOU-G
           MOVE    W32-SANTEIKBN       TO  SPA-GMN-SANTEIKBN-G
           MOVE    W32-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
           MOVE    W32-HKNCOMBIKBN     TO  SPA-GMN-HKNCOMBIKBN-G
      *    ¿ÇÎÅ¶èÊ¬
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               MOVE    W32-SRYKBN(IDX)     TO  SPA-GMN-SRYKBN-G(IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ðËÜ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    ´ÉÍýÈÖ¹æ¥Á¥§¥Ã¥¯
           IF     (SPA-GMN-KBNCD       =   SPACE ) OR
                  (SPA-GMN-KBNCD       NOT NUMERIC )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-STYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-EDYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-SANTEIHOU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4112-SANTEIKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4113-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4114-HKNCOMBIKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4115-SRYKBN-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Í­¸ú³«»ÏÆü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       41021-STYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-STYUKYMD
      *
           IF     (SPA-GMN-STYUKYMD   =   SPACE )
              AND (SPA-GMN-TBL-MAX    =   ZERO  )
               IF      WRK-MCP-WIDGET     =   "STYUKYMD"
                   MOVE    SPA-HKT-YUKOSTYMD  TO  SPA-GMN-STYUKYMD
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
      *H30.6
      *    ¿·µ¬¤Î»þ½é´üÀßÄê
           IF     (SPA-ERRCD           =   SPACE  )
              AND (SPA-GMN-SELNUM      =   ZERO   )
              AND (SPA-GMN-STYUKYMD    NOT =   SPACE)
               IF      SPA-GMN-SANTEIHOU-G     =   SPACE
                   MOVE    1                   TO  SPA-GMN-SYORI
                   MOVE    4                   TO  SPA-GMN-CUR
                   PERFORM 200212-INIT-GMN-SEC
               END-IF
           END-IF
           .
        41021-STYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Í­¸ú½ªÎ»Æü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-EDYUKYMD
      *
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               IF      WRK-MCP-WIDGET     =   "EDYUKYMD"
                   MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
               END-IF
           END-IF
      *
           IF      SPA-GMN-EDYUKYMD    NOT =   SPACE
               MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD 
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-GMN-STYUKYMD    NOT =   SPACE ) AND
                  (SPA-GMN-EDYUKYMD    NOT =   SPACE )
               IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    4                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
        41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç»»ÄêÊýË¡¥Á¥§¥Ã¥¯
      *****************************************************************
       4100-SANTEIHOU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SANTEIHOU-MAX )   OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-SANTEIHOU   =   SPA-GMN-SANTEIHOU-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-SANTEIHOU-LST (IDX)
                                           TO  SPA-GMN-SANTEIHOU-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
       4100-SANTEIHOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Êñ³ç»»Äê¶èÊ¬¥Á¥§¥Ã¥¯
      *****************************************************************
       4112-SANTEIKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            ³°Íè¿ÇÎÅÎÁ
                   MOVE    SPA-GMN-SANTEIKBN-LST (1)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
               WHEN    "504"
      *            ÎÅÍÜÉÂÅïÆþ±¡ÎÁ »»ÄêÆü¤ÇÊñ³ç
                   MOVE    SPA-GMN-SANTEIKBN-LST (2)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "201"
      *            ËýÀ­°Ý»ýÆ©ÀÏ´ÉÍýÎÁ¡¡»»Äê·î¤ÇÊñ³ç
                   MOVE    SPA-GMN-SANTEIKBN-LST (3)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "202"
               WHEN    "209"
      *            ¾®»ù²Ê³°Íè¿Ç»¡ÎÁ¡¡»»ÄêÆü¤ÇÊñ³ç
                   MOVE    SPA-GMN-SANTEIKBN-LST (2)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "203"
      *            À¸³è½¬´·ÉÂ´ÉÍýÎÁ¡¡»»Äê·î¤ÇÊñ³ç
                   MOVE    SPA-GMN-SANTEIKBN-LST (3)
                                               TO  SPA-GMN-SANTEIKBN-G
           END-EVALUATE
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SANTEIKBN-MAX )   OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-SANTEIKBN   =   SPA-GMN-SANTEIKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-SANTEIKBN-LST (IDX)
                                           TO  SPA-GMN-SANTEIKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
           .
       4112-SANTEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ³°¶èÊ¬¥Á¥§¥Ã¥¯
      *****************************************************************
       4113-NYUGAIKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            ³°Íè¿ÇÎÅÎÁ
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
               WHEN    "504"
      *            ÎÅÍÜÉÂÅïÆþ±¡ÎÁ
               WHEN    "201"
      *            ËýÀ­°Ý»ýÆ©ÀÏ´ÉÍýÎÁ
               WHEN    "202"
      *            ¾®»ù²Ê³°Íè¿ÇÎÅÎÁ
               WHEN    "209"
      *            ¾®»ù¤«¤«¤ê¤Ä¤±¿ÇÎÅÎÁ
               WHEN    "203"
      *            À¸³è½¬´·ÉÂ´ÉÍýÎÁ
                   MOVE    SPA-GMN-NYUGAIKBN-LST (1)
                                               TO  SPA-GMN-NYUGAIKBN-G
           END-EVALUATE
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NYUGAIKBN-MAX )   OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
           .
       4113-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Á¥§¥Ã¥¯
      *****************************************************************
       4114-HKNCOMBIKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            ³°Íè¿ÇÎÅÎÁ
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
               WHEN    "504"
      *            ÎÅÍÜÉÂÅïÆþ±¡ÎÁ
               WHEN    "201"
      *            ËýÀ­°Ý»ýÆ©ÀÏ´ÉÍýÎÁ
               WHEN    "202"
      *            ¾®»ù²Ê³°Íè¿ÇÎÅÎÁ
               WHEN    "203"
      *            À¸³è½¬´·ÉÂ´ÉÍýÎÁ
               WHEN    "209"
      *            ¾®»ù¤«¤«¤ê¤Ä¤±¿ÇÎÅÎÁ
                   MOVE    SPA-GMN-HKNCOMBIKBN-LST (1)
                                               TO  SPA-GMN-HKNCOMBIKBN-G
           END-EVALUATE
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKNCOMBIKBN-MAX ) OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-HKNCOMBIKBN   =   SPA-GMN-HKNCOMBIKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-HKNCOMBIKBN-LST (IDX)
                                           TO  SPA-GMN-HKNCOMBIKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
           .
       4114-HKNCOMBIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¶èÊ¬¤»¥Á¥§¥Ã¥¯
      *****************************************************************
       4115-SRYKBN-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SRYKBN-MAX ) OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    SPA-GMN-KBNCD
                   WHEN    "001"
      *            ³°Íè¿ÇÎÅÎÁ
                       MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
      *H28.4
      *            ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ
                   WHEN    "524"
      ***              IF     (SPA-GMN-SELNUM  =   ZERO   )
                       IF     (SPA-GMN-SYORI   =   1      )
                          AND (IDX             =   04     )
      *                    H28.4¤«¤é¼ê½Ñ¤ÏÊñ³çÂÐ¾Ý³°
                           IF     (SPA-NAI-STYUKYMD    >=  "20160401")
                               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           END-IF
                       END-IF
      *H30.1
      *        H28.4²þÀµÊ¬
                   WHEN    "518"
      *            ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ
      ***              IF     (SPA-GMN-SELNUM  =   ZERO   )
                       IF     (SPA-GMN-SYORI   =   1      )
                        AND   (SPA-NAI-STYUKYMD    >=  "20160401")
                           IF      IDX             =   9
                                                   OR  12
      *                        Êü¼ÍÀþ¡¢ºßÂð¡¡¤Ê¤·
                               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           ELSE
                               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           END-IF
                       END-IF
      ****             IF     (SPA-GMN-SELNUM  =   ZERO   )
                       IF     (SPA-GMN-SYORI   =   1      )
                        AND   (SPA-NAI-STYUKYMD    <   "20160401")
                           MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                       END-IF
                   WHEN    "514"
                   WHEN    "515"
                   WHEN    "516"
      *            ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ
      ***              IF     (SPA-GMN-SELNUM  =   ZERO   )
                       IF     (SPA-GMN-SYORI   =   1      )
                        AND   (SPA-NAI-STYUKYMD    >=  "20160401")
                           IF      IDX             =   1
                                                   OR  2
                                                   OR  4
                                                   OR  9
                                                   OR  10
                                                   OR  12
      *                   ÅêÌô¡¢Ãí¼Í¡¢¼ê½Ñ¡¦Ëã¿ì¡¢Êü¼ÍÀþ¡¢ÉÂÍý¿ÇÃÇ¡¢ºßÂð¡¡¤Ê¤·
                               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           ELSE
                               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           END-IF
                       END-IF
      *
      ***              IF     (SPA-GMN-SELNUM  =   ZERO   )
                       IF     (SPA-GMN-SYORI   =   1      )
                        AND   (SPA-NAI-STYUKYMD    <   "20160401")
                           IF      IDX             =   1
                                                   OR  2
                                                   OR  4
                                                   OR  9
                                                   OR  10
      *                   ÅêÌô¡¢Ãí¼Í¡¢¼ê½Ñ¡¦Ëã¿ì¡¢Êü¼ÍÀþ¡¢ÉÂÍý¿ÇÃÇ¡¡¤Ê¤·
                               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           ELSE
                               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           END-IF
                       END-IF
      *
               END-EVALUATE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   SPA-SRYKBN-MAX ) OR
                                  (FLG-CHK     =   1    )
                   IF      SPA-GMN-SRYKBN (IDX)
                                           =   SPA-GMN-SRYKBN-LST
                                                        (IDY)(1:1)
                       MOVE    SPA-GMN-SRYKBN-LST (IDY)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0012"              TO  SPA-ERRCD
                   COMPUTE SPA-GMN-CUR         =   IDX
                                               +   20
               END-IF
           END-PERFORM
           .
       4115-SRYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ìá¤ë¡¡½èÍý
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W32"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¯¥ê¥¢½èÍý
      *****************************************************************
       402-CLERA-SEC                               SECTION.
      *
      *    ½é´üÉ½¼¨
           PERFORM 3101-INIT-GMN-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       402-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºï½ü³ÎÇ§²èÌÌÉ½¼¨½èÍý
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    "1003"              TO  SPA-WIDCD
           END-IF
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºï½ü½èÍý
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"                  TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD           TO  SYS-1014-KBNCD
           MOVE    SPA-MAE-STYUKYMD        TO  SYS-1014-STYUKYMD
           MOVE    SPA-MAE-EDYUKYMD        TO  SYS-1014-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "W32 DB DEL ERR SYSKANRI-KEY =>" SYS-1014-KEY 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        ½é´üÉ½¼¨
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       ÅÐÏ¿Á°½èÍý
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    ÆþÎÏ¹àÌÜ¤Î¥Á¥§¥Ã¥¯
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
              MOVE    "1002"           TO  SPA-WIDCD
      *       Í­¸ú³«»ÏÆü¤ÎÊÑ¹¹¤ÏÄÉ²Ã
              IF     (SPA-GMN-SELNUM       =   ZERO )  OR
                     (SPA-GMN-SYORI        =   1    )
                  MOVE    "1001"           TO  SPA-WIDCD
               END-IF
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       ÆþÎÏ¹àÌÜ¤Î´ØÏ¢¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    ´ÉÍý¥³¡¼¥É
           IF     (SPA-GMN-KBNCD       =   SPACE )  OR
                  (SPA-GMN-KBNCD       NOT NUMERIC)
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-KBNCD-MAX) OR
                                  (FLG-OK      =   1    )
                   IF      SPA-GMN-KBNCD    =   SPA-GMN-TKBNCD
                                                         (IDX)
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM
      *        ÆþÎÏºÑ¤ß¤Î»þ¡¢ÁªÂò¤¬¤¢¤ë¤³¤È
      *        IF      FLG-OK          =   1
      *            IF      SPA-GMN-TBL-MAX =   ZERO
      *                MOVE    "0021"              TO  SPA-ERRCD
      *                MOVE    2                   TO  SPA-GMN-CUR
      *            END-IF
      *        END-IF
           END-IF
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               MOVE    SPA-HKT-YUKOSTYMD  TO  SPA-GMN-STYUKYMD
               MOVE    SPA-HKT-YUKOSTYMD  TO  SPA-NAI-STYUKYMD
           END-IF
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               MOVE    SPA-HKT-YUKOEDYMD  TO  SPA-GMN-EDYUKYMD
               MOVE    SPA-HKT-YUKOEDYMD  TO  SPA-NAI-EDYUKYMD
           END-IF
           IF      SPA-ERRCD          =   SPACE
               IF      SPA-NAI-STYUKYMD   >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    3                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¤Î´ü´ÖÆâ¤Ç¤¢¤ë¤³¤È
           IF      SPA-ERRCD          =   SPACE
               IF     (SPA-NAI-STYUKYMD   <   SPA-HKT-YUKOSTYMD)
                   OR (SPA-NAI-EDYUKYMD   >   SPA-HKT-YUKOEDYMD )
                   MOVE    "0021"             TO  SPA-ERRCD
                   MOVE    3                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    ´ü´Ö½ÅÊ£¥Á¥§¥Ã¥¯
           IF     (SPA-GMN-SELNUM      =   ZERO  )  OR
                  (SPA-NAI-STYUKYMD    NOT =   SPA-MAE-STYUKYMD)
               MOVE    1               TO  SPA-GMN-SYORI
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN-TBL-MAX )
                              OR  (SPA-ERRCD   NOT =   SPACE )
                   IF      (SPA-NAI-STYUKYMD    >=  SPA-NAI-TSTYUKYMD
                                                               (IDX))
                     AND   (SPA-NAI-STYUKYMD    <=  SPA-NAI-TEDYUKYMD
                                                               (IDX))
                      MOVE    "0020"           TO  SPA-ERRCD
                      MOVE    3                TO  SPA-GMN-CUR
                   END-IF
                   IF      (SPA-NAI-EDYUKYMD    >=  SPA-NAI-TSTYUKYMD
                                                               (IDX))
                     AND  (SPA-NAI-EDYUKYMD    <=  SPA-NAI-TEDYUKYMD
                                                               (IDX))
                      MOVE    "0020"           TO  SPA-ERRCD
                      MOVE    3                TO  SPA-GMN-CUR
                   END-IF
                   IF     (SPA-ERRCD       NOT =   SPACE) AND
                          (SPA-GMN-SELNUM      >   ZERO ) AND
                          (IDX                 =   SPA-GMN-SELNUM)
                       MOVE    2               TO  SPA-GMN-SYORI
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
      *!
      *H30.1
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-SYORI       =    1
                   PERFORM 4115-SRYKBN-CHK-SEC
               END-IF
           END-IF
      *H30.1
      *    ´ü´ÖÆâ¤Ç¿ÇÎÅ¶èÊ¬¤¬ÊÑ¹¹¤È¤Ê¤ë»þ¡¢·Ù¹ðÉ½¼¨
           IF      SPA-ERRCD           =   SPACE
      *        H28.4 ¤«¤é²þÀµ
               IF      (SPA-NAI-STYUKYMD   <   "20160401")
                  AND  (SPA-NAI-EDYUKYMD   >   "20160401")
                   IF      (SPA-GMN-KBNCD      =   "518"
                                               OR  "514"
                                               OR  "515"
                                               OR  "516"
                                               OR  "524" )
                       AND  (SPA-NAI-KEIFLG    =   ZERO  )
                       MOVE    1               TO  SPA-NAI-KEIFLG
      *
      *                ¿·µ¬
                       IF      SPA-GMN-SELNUM      =   ZERO
                           MOVE    "K022"          TO  SPA-ERRCD
                           MOVE    03              TO  SPA-GMN-CUR
                       ELSE
                           MOVE    "K023"          TO  SPA-ERRCD
                           MOVE    99              TO  SPA-GMN-CUR
                       END-IF
                   END-IF
                END-IF
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       ÅÐÏ¿½èÍý
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    ¹¹¿·Æü¼èÆÀ
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1014-KBNCD
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1014-STYUKYMD
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1014-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1014-REC
           ELSE
               MOVE    SPACE                   TO  SYS-1014-REC
               INITIALIZE                      SYS-1014-REC
               MOVE   "1014"                   TO  SYS-1014-KANRICD
               MOVE    SPA-GMN-KBNCD           TO  SYS-1014-KBNCD
           END-IF
      *
           MOVE    SPA-NAI-STYUKYMD        TO  SYS-1014-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD        TO  SYS-1014-EDYUKYMD
           MOVE    SPA-GMN-SANTEIHOU       TO  SYS-1014-SANTEI-HOU 
           MOVE    SPA-GMN-SANTEIKBN       TO  SYS-1014-SANTEI-KBN 
           MOVE    SPA-GMN-NYUGAIKBN       TO  SYS-1014-NYUGAIKBN 
           MOVE    SPA-GMN-HKNCOMBIKBN     TO  SYS-1014-HKNCOMBI-KBN
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN (IDX)
                                           TO  SYS-1014-SRYKBN-KBN(IDX)
           END-PERFORM
      *
           IF     (FLG-SYSKANRI        =   ZERO ) AND
                  (SPA-GMN-SYORI       =   2    )
      *        ¹¹¿·
               MOVE    SMCNDATE-YMD        TO  SYS-1014-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1014-UPHMS
               MOVE    SYS-1014-REC        TO  SYSKANRI-REC
      *
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-MAE-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-MAE-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W32 DB UPD ERR KEY:" SYS-1014-KEY 
               END-IF
           ELSE
      *        ÄÉ²Ã
               MOVE    SMCNDATE-YMD        TO  SYS-1014-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1014-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1014-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
               MOVE    SYS-1014-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W32 DB INS ERR KEY:" SYS-1014-KEY 
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        ½é´üÉ½¼¨
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç°ìÍ÷°ì³ç½èÍý
      *****************************************************************
       4090-W321-ALL-SEC          SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    ÆþÎÏ¹àÌÜ¤Î¥Á¥§¥Ã¥¯
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-W321-AREA
               MOVE    "00"                TO  SPA-W321-SYORIKKBN
               PERFORM 4090-W321-SYORI-SEC
           END-IF
           .
       4090-W321-ALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç°ìÍ÷²èÌÌÁ«°Ü½èÍý
      *****************************************************************
       4090-W321-SYORI-SEC        SECTION.
      *
      *    ½é´ü²èÌÌ¥»¥Ã¥È
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W32             TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW321"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-W32
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
      *
           IF      SPA-W321-MAX        =   ZERO
               MOVE    "0109"              TO  SPA-ERRCD
               GO      TO      4090-W321-SYORI-EXT
           END-IF
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
           MOVE    "W321"              TO  SPA-SAKIPG
           MOVE    "W32"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "W321"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4090-W321-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç°ìÍ÷ÁªÂò½èÍý
      *****************************************************************
       4091-W321-SEL-SEC          SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    ÆþÎÏ¹àÌÜ¤Î¥Á¥§¥Ã¥¯
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-W321-AREA
               MOVE    WRK-MCP-WIDGET(3:2) TO  SPA-W321-SYORIKKBN
               PERFORM 4090-W321-SYORI-SEC
           END-IF
           .
       4091-W321-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«²èÌÌÊÔ½¸½èÍý
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W32    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÌÌÊÔ½¸½èÍý
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W32
           INITIALIZE                      W32
      *
           MOVE    SPA-GMN-KBNCD-G     TO  W32-KBNCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KBNCD-MAX
               MOVE    SPA-GMN-KBNCD-LST (IDX)
                                           TO  W32-KBNCD-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-KBNCD-MAX       TO  W32-KBNCD-COUNT
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W32-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W32-EDYUKYMD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               MOVE    SPA-GMN-TBANGO (IDX)
                                           TO  W32-TNUM (IDX)
               MOVE    SPA-GMN-TSTYUKYMD (IDX)
                                           TO  W32-TSTYUKYMD (IDX)
               MOVE    SPA-GMN-TEDYUKYMD (IDX)
                                           TO  W32-TEDYUKYMD (IDX)
      *H30.6
               IF     (SPA-GMN-SELNUM      >   ZERO )
                  AND (SPA-GMN-SELNUM      =   IDX  )
                   MOVE    "T"             TO  W32-KIKAN-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  W32-KIKAN-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-TBL-MAX         TO  W32-KIKAN-COUNT
           MOVE    SPA-GMN-SELNUM          TO  W32-SELNUM
      *
           MOVE    SPA-GMN-SANTEIHOU-G     TO  W32-SANTEIHOU
           MOVE    SPA-GMN-SANTEIKBN-G     TO  W32-SANTEIKBN
           MOVE    SPA-GMN-NYUGAIKBN-G     TO  W32-NYUGAIKBN
           MOVE    SPA-GMN-HKNCOMBIKBN-G   TO  W32-HKNCOMBIKBN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANTEIHOU-MAX
               MOVE    SPA-GMN-SANTEIHOU-LST(IDX)
                                       TO  W32-SANTEIHOU-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SANTEIHOU-MAX     TO  W32-SANTEIHOU-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANTEIKBN-MAX
               MOVE    SPA-GMN-SANTEIKBN-LST(IDX)
                                       TO  W32-SANTEIKBN-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SANTEIKBN-MAX     TO  W32-SANTEIKBN-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAIKBN-MAX
               MOVE    SPA-GMN-NYUGAIKBN-LST(IDX)
                                       TO  W32-NYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-NYUGAIKBN-MAX     TO  W32-NYUGAIKBN-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNCOMBIKBN-MAX
               MOVE    SPA-GMN-HKNCOMBIKBN-LST(IDX)
                                       TO  W32-HKNCOMBIKBN-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-HKNCOMBIKBN-MAX   TO  W32-HKNCOMBIKBN-COUNT
      *    ¿ÇÎÅ¶èÊ¬
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-G (IDX)  TO  W32-SRYKBN (IDX)
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-SRYKBN-MAX
                   MOVE    SPA-GMN-SRYKBN-LST (IDY) 
                                           TO  W32-SRYKBN-ITEM
                                                        (IDX IDY)
               END-PERFORM
               MOVE    SPA-SRYKBN-MAX       TO  W32-SRYKBN-COUNT(IDX)
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶ÈÌ³¸¢¸ÂÈó³èÀ­½èÍý
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           IF     (SPA-GMN-SYORI       =   1   ) AND
                  (SPA-GMN-SELNUM      =   ZERO)
               MOVE    WIDGET-INSENSITIVE  TO  W32-B03-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W32-B03-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸É½¼¨Íý
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "Êñ³ç´ÉÍý¶èÊ¬ÆþÎÏ¥¨¥é¡¼"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "Í­¸ú³«»ÏÆüÆþÎÏ¥¨¥é¡Ý"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "Í­¸ú½ªÎ»ÆüÆþÎÏ¥¨¥é¡Ý"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "Í­¸ú³«»ÏÆü¡ãÍ­¸ú½ªÎ»Æü¤ÇÆþÎÏ¤·¤Æ²¼¤µ¤¤"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "ÁªÂòÈÖ¹æ¤¬¤¢¤ê¤Þ¤»¤ó¡£"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "Êñ³ç»»ÄêÊýË¡¤¬¤¢¤ê¤Þ¤»¤ó¡£"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "Êñ³ç»»Äê¶èÊ¬¤¬¤¢¤ê¤Þ¤»¤ó¡£"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "Æþ³°¶èÊ¬¤¬¤¢¤ê¤Þ¤»¤ó¡£"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "ÊÝ¸±ÁÈ¹ç¤»¶èÊ¬¤¬¤¢¤ê¤Þ¤»¤ó¡£"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "¿ÇÎÅ¶èÊ¬¥³¡¼¥É¤¬¤¢¤ê¤Þ¤»¤ó¡£"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "´ü´Ö¤¬½ÅÊ£¤·¤Æ¤¤¤Þ¤¹¡£"
                                               TO  SPA-ERRMSG
               WHEN    "0021"
                   STRING  "Êñ³ç¿ÇÎÅ¥³¡¼¥É¤Î´ü´Ö³°¤Ç¤¹¡£"
                           "´ü´Ö¤òÊÑ¹¹¤·¤Æ²¼¤µ¤¤¡£"
                                               INTO  SPA-ERRMSG
                   END-STRING
      *H30.2
               WHEN    "K022"
                   STRING  "·Ù¹ð¡ª"
                           "Ê¿À®£²£¸Ç¯£´·î¤«¤é¿ÇÎÅ¶èÊ¬¤ÎÊñ³çÆâÍÆ¤Ë"
                           "ÊÑ¹¹¤¬¤¢¤ê¤Þ¤¹¡£´ü´ÖÊÑ¹¹¤·¤Æ²¼¤µ¤¤¡£"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K023"
                   STRING  "·Ù¹ð¡ª"
                           "Ê¿À®£²£¸Ç¯£´·î¤«¤é¿ÇÎÅ¶èÊ¬¤ÎÊñ³çÆâÍÆ¤Ë"
                           "ÊÑ¹¹¤¬¤¢¤ê¤Þ¤¹¡£´ü´ÖÄÉ²Ã¤·¤Æ²¼¤µ¤¤¡£"
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0109"
                   STRING  "Êñ³ç¥³¡¼¥É¤¬¤¢¤ê¤Þ¤»¤ó¡£"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "¹¹¿·¤¬¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "ºï½ü¤¬¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W32"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¬¥¤¥É¥á¥Ã¥»¡¼¥¸É½¼¨
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "Êñ³ç¿ÇÎÅ¹Ô°ÙÀßÄê¤òÄÉ²ÃÅÐÏ¿¤·¤Þ¤¹"
                                                     TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "Êñ³ç¿ÇÎÅ¹Ô°ÙÀßÄê¤ò¹¹¿·¤·¤Þ¤¹"
                                                     TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "Êñ³ç¿ÇÎÅ¹Ô°ÙÀßÄê¤òºï½ü¤·¤Þ¤¹" 
                                                     TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W32"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÌÌ¥«¡¼¥½¥ë¥»¥Ã¥È½èÍý    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "KBNCD"          TO   MCP-WIDGET
                WHEN    3
                    MOVE  "STYUKYMD"       TO   MCP-WIDGET
                WHEN    4
                    MOVE  "EDYUKYMD"       TO   MCP-WIDGET
                WHEN    5
                    MOVE  "SELNUM"         TO   MCP-WIDGET
                WHEN    6
                    MOVE  "SANTEIHOU"      TO   MCP-WIDGET
                WHEN    7
                    MOVE  "SANTEIKBN"      TO   MCP-WIDGET
                WHEN    8
                    MOVE  "NYUGAIKBN"      TO   MCP-WIDGET
                WHEN    9
                    MOVE  "HKNCOMBIKBN"    TO   MCP-WIDGET
      *         ¿ÇÎÅ¶èÊ¬
                WHEN    21   THRU  33
                    MOVE     SPA-GMN-CUR       TO  IDX2
                    MOVE     "SRYKBN"          TO  MCP-WIDGET
                    COMPUTE  IDX2              =   IDX2   -   20
                    MOVE     IDX2              TO  MCP-WIDGET(7:2)
      *
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥«¡¼¥½¥ëÊÔ½¸½èÍý
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "KBNCD"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SANTEIHOU"
                   MOVE    99              TO  SPA-GMN-CUR
               WHEN    "SANTEIKBN"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "HKNCOMBIKBN"
                   MOVE    21              TO  SPA-GMN-CUR
      *
               WHEN     OTHER
                   IF      WRK-MCP-WIDGET(1:6) =   "SRYKBN"
                       MOVE    WRK-MCP-WIDGET(7:2) TO  IDX2
                       COMPUTE SPA-GMN-CUR     =   IDX2
                                               +   21
      *                ¹¹¿·¤Ø
                       IF      IDX2            =   12
                           MOVE    99          TO  SPA-GMN-CUR
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆüÉÕ¥Á¥§¥Ã¥¯¡¦ÊÔ½¸½èÍý
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-YMD(1:8)
               WHEN    ZERO
                   MOVE    ZERO                TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)        TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"               TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)          TO  WRK-HENYMDG  
               WHEN    OTHER
      *            ÆüÉÕ²èÌÌ¥Á¥§¥Ã¥¯¥µ¥Ö
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-YMD             TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÏÂÎñÀ¾ÎñÊÑ´¹ÊÔ½¸½èÍý
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    ZERO
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¥­¡¼¡Ë
      *****************************************************************
       900-SYSKANRI-KEY-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
            .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       900-HKTSRYCD-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKTSRYCD
               MOVE    MCPDATA-REC     TO  HKTSRYCD-REC
           ELSE
               INITIALIZE                  HKTSRYCD-REC
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
      *
           .
       900-HKTSRYCD-READ-EXT.
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
      *****************************************************************
      *    £Ð£Õ£Ô¡¡½èÍý
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
