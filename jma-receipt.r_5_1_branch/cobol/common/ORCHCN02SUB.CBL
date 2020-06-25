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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCHCN02SUB.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Âà±¡¾ÚÌÀ½ñÍÑÆþ±¡¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  06/06/08    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/05/09  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *Ê¿À®£³£°Ç¯£´·î²þÄêÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/03/07  Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ¤ÎÈ½ÄêÊÑ¹¹
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-MATCH                   PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
           03  NYUIN-IDX                   PIC 9(04).
           03  ST-IDX                      PIC 9(04).
           03  ED-IDX                      PIC 9(04).
      *
      *    Æþ±¡ÆüÃÖ´¹¤¨ÍÑ
           03  WRK-DDX                     PIC X(02).
           03  WRK-DD9          REDEFINES  WRK-DDX
                                           PIC 9(02).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-TANKI-KBN               PIC X(01).
           03  WRK-TANKI-KBN2              PIC X(01).
      *
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡Ý
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý¤ÎÃ»´üÂÚºß¥Á¥§¥Ã¥¯ÍÑ¥ï¡¼¥¯¥ì¥³¡¼¥É
           COPY    "CPTENSU.INC"   REPLACING //TNS-//  BY  //TTNS-//.
      *
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö
           COPY "CPORCSTUSAN.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    Âà±¡¾ÚÌÀ½ñÍÑÆþ±¡¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCHCN02SUB.INC".
      *
       PROCEDURE                   DIVISION    USING
            ORCHCN02SUB-AREA
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
           INITIALIZE                  LNK-ORCHCN02SUB-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-ORCHCN02SUB-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-ORCHCN02SUB-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-ORCHCN02SUB-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *     DISPLAY "SYUBETUCD(1) = " LNK-ORCHCN02SUB-SYUBETUCD(1)
      *     DISPLAY "NISSU(1)     = " LNK-ORCHCN02SUB-NISSU(1)
      *     DISPLAY "MEI(1)       = " LNK-ORCHCN02SUB-SYUBETUMEI(1)
      *     DISPLAY "SYUBETUCD(2) = " LNK-ORCHCN02SUB-SYUBETUCD(2)
      *     DISPLAY "NISSU(2)     = " LNK-ORCHCN02SUB-NISSU(2)
      *     DISPLAY "MEI(2)       = " LNK-ORCHCN02SUB-SYUBETUMEI(2)
      *     DISPLAY "SYUBETUCD(3) = " LNK-ORCHCN02SUB-SYUBETUCD(3)
      *     DISPLAY "NISSU(3)     = " LNK-ORCHCN02SUB-NISSU(3)
      *     DISPLAY "MEI(3)       = " LNK-ORCHCN02SUB-SYUBETUMEI(3)
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    »»ÄêÇ¯·î¥Á¥§¥Ã¥¯
           IF    ( LNK-ORCHCN02SUB-HOSPNUM   =   ZERO  )   OR
                 ( LNK-ORCHCN02SUB-PTID      =   SPACE )   OR
                 ( LNK-ORCHCN02SUB-NYUINYMD  =   SPACE )   OR
                 ( LNK-ORCHCN02SUB-TAIINYMD  =   SPACE )
               MOVE    1               TO  LNK-ORCHCN02SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                       PTINF-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    1               TO  LNK-ORCHCN02SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *Æþ±¡ÍúÎò¤«¤é¼«±¡Îò¤ÎÆü¿ô¼èÆÀ¡ÊÆþ±¡ÎòºîÀ®µ¡Ç½¤Ë¤è¤ëÍúÎòÊ¬¤Î
      *                              Æü¿ô¼èÆÀ¡Ë
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    LNK-ORCHCN02SUB-NYUINYMD TO  PTNYUINRRK-NYUINYMD
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key48"                  TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK          =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key48"             TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               IF    FLG-PTNYUINRRK        =   ZERO
      *            ´ü´Ö»»½Ð½èÍý
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY5-AREA
                   MOVE    "51"                TO  LNK-DAY5-IRAI
                   MOVE    PTNYUINRRK-NYUINYMD TO  LNK-DAY5-START
                   MOVE    PTNYUINRRK-TAIINYMD TO  LNK-DAY5-END
                   CALL    "ORCSDAY"               USING
                                                   STS-AREA-DAY
                                                   LNK-DAY5-AREA
                   IF     STS-DAY-RC1      =   ZERO
                      ADD   LNK-DAY5-NISSU2
                                       TO   LNK-ORCHCN02SUB-NISSU(1)
                   END-IF
               END-IF
           END-IF
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key48"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Æþ±¡ÎÁ¤Î¼èÆÀ
           INITIALIZE                           NYUINACCT-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN02SUB-NYUINYMD(1:6)
                                            TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"          TO  MCP-TABLE
           MOVE    "key53"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key53"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *
                   IF   (LNK-ORCHCN02SUB-NYUINYMD(1:6)  <= NACCT-SRYYM)
                                         AND
                        (LNK-ORCHCN02SUB-TAIINYMD(1:6)  >= NACCT-SRYYM)
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *
                       INITIALIZE    TNS-TENSU-REC
                       MOVE   NSRY-SRYCD(1)   TO    TNS-SRYCD
                       MOVE   NACCT-SRYYM     TO    TNS-YUKOSTYMD(1:6)
                                                    TNS-YUKOEDYMD(1:6)
                       MOVE   "01"            TO    TNS-YUKOSTYMD(7:2)
                                                    TNS-YUKOEDYMD(7:2)
                       PERFORM    910-TENSU-READ-SEC
      *                Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ¤Î¾ì¹ç¡¢Æþ±¡ÎÁ¤ÎÌ¾¾Î¤«¤é¡¢
      *                µ­ºÜÂÐ¾Ý¤Ç¤¢¤ë¤«¤òºÆ¥Á¥§¥Ã¥¯
                       PERFORM    210-TANKI-NAMECHK-SEC
      *
                       IF     (NACCT-ZAIKAISU    =  ZERO)
                          IF  (WRK-TANKI-KBN     =      "1")  AND
                              (WRK-TANKI-KBN2    =      SPACE)
      *                      Æþ±¡´ðËÜÎÁÃ±°Ì¤ÎÆþ±¡Æü¿ô¥«¥¦¥ó¥È
                              PERFORM    210-NYUIN-COUNT-SEC
                          END-IF
                       END-IF
                       IF     (NACCT-ZAIKAISU    NOT =  ZERO)
                          IF  (WRK-TANKI-KBN     =      "1")  AND
                              (WRK-TANKI-KBN2    =      SPACE)
      *                      Æþ±¡´ðËÜÎÁÃ±°Ì¤ÎÆþ±¡Æü¿ô¥«¥¦¥ó¥È
                              PERFORM    210-NYUIN-COUNT-SEC
                          END-IF
                          IF  (WRK-TANKI-KBN     =      SPACE)
      *                      Æþ±¡´ðËÜÎÁÃ±°Ì¤ÎÆþ±¡Æü¿ô¥«¥¦¥ó¥È
                              PERFORM    210-NYUIN-COUNT-SEC
                          END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key53"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key53"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *ÁªÄêÂÐ¾ÝÆü¿ô¤Î¼èÆÀ
           INITIALIZE                       ORCSTUSANAREA
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  STUSAN-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  STUSAN-PTID
           MOVE    LNK-ORCHCN02SUB-TAIINYMD TO  STUSAN-KJNYMD
           CALL    "ORCSTUSAN"     USING        ORCSTUSANAREA
                                                SPA-AREA
           MOVE    STUSAN-NISSU05           TO  LNK-ORCHCN02SUB-TKIKAN
      *
      *
      *   ·ë²ÌÉ½¼¨
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     5)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "LNK-ORCHCN02SUB-SYUBETUCD = "
      *                    LNK-ORCHCN02SUB-SYUBETUCD(IDX1)
      *          DISPLAY  "LNK-ORCHCN02SUB-SYUBETUMEI = "
      *                    LNK-ORCHCN02SUB-SYUBETUMEI(IDX1)
      *          DISPLAY  "LNK-ORCHCN02SUB-NISSU = "
      *                    LNK-ORCHCN02SUB-NISSU(IDX1)
      *     END-PERFORM
            DISPLAY  "LNK-ORCHCN02SUB-TKIKAN = " LNK-ORCHCN02SUB-TKIKAN
            DISPLAY  "STUSAN-RC = " STUSAN-RC
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁÃ±°Ì¤ÎÆü¿ô¥«¥¦¥ó¥È
      *****************************************************************
       210-NYUIN-COUNT-SEC            SECTION.
      *
           MOVE       ZERO      TO    FLG-MATCH
      *
           PERFORM    VARYING   NYUIN-IDX   FROM   1   BY   1
             UNTIL   (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX) = SPACE) OR
                     (FLG-MATCH        =   1 )     OR
                     (NYUIN-IDX    >   20)
               DISPLAY  "NYUIN-IDX = " NYUIN-IDX
      *        ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÆþ±¡ÎÁÌ¾¾Î¤Î¼èÆÀ¡Ê¥ï¡¼¥¯¤È¤ÎÈæ³ÓÍÑ¡Ë
               INITIALIZE    TNS-TENSU-REC
               MOVE   NSRY-SRYCD(1)   TO    TNS-SRYCD
               MOVE   NACCT-SRYYM     TO    TNS-YUKOSTYMD(1:6)
                                            TNS-YUKOEDYMD(1:6)
               MOVE   "01"            TO    TNS-YUKOSTYMD(7:2)
                                            TNS-YUKOEDYMD(7:2)
      *         DISPLAY  "TNS-NAME = " TNS-NAME
               PERFORM    910-TENSU-READ-SEC
               EVALUATE   NSRY-SRYCD(1)
                   WHEN   "111111111"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO   TNS-NAME
                   WHEN   "222222222"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO   TNS-NAME
                   WHEN   "333333333"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO   TNS-NAME
                   WHEN   "111111119"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "222222229"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "333333339"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "111111199"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"
                                                       TO   TNS-NAME
                   WHEN   "222222299"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"
                                                       TO   TNS-NAME
                   WHEN   "333333399"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"
                                                       TO   TNS-NAME
                   WHEN   "111111999"
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "222222999"
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "333333999"
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "444444444"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´"  TO   TNS-NAME
                   WHEN   "555555555"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£µ"  TO   TNS-NAME
                   WHEN   "666666666"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£¶"  TO   TNS-NAME
                   WHEN   "444444499"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´"
                                                       TO   TNS-NAME
                   WHEN   "555555599"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£µ"
                                                       TO   TNS-NAME
                   WHEN   "666666699"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£¶"
                                                       TO   TNS-NAME
                   WHEN   "111119999"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO   TNS-NAME
                   WHEN   "222229999"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO   TNS-NAME
                   WHEN   "333339999"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO   TNS-NAME
                   WHEN   "111199999"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"
                                                       TO   TNS-NAME
                   WHEN   "222299999"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"
                                                       TO   TNS-NAME
                   WHEN   "333399999"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"
                                                       TO   TNS-NAME
               END-EVALUATE
      *
               IF   ((NACCT-SRYYM      >=   "200604")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190097010"  OR
                                            "190097110"  OR
                                            "190119510"  OR
                                            "190119610")
                   MOVE   "111111111"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "200604")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190097210"  OR
                                            "190097310"  OR
                                            "190119710"  OR
                                            "190119810")
                   MOVE   "222222222"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201004")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190135110"  OR
                                            "190135210"  OR
                                            "190135310")
                   MOVE   "333333333"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190708610"  OR
                                            "190708710"  OR
                                            "190708810")
                   MOVE   "111111119"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190708910"  OR
                                            "190709010"  OR
                                            "190709110")
                   MOVE   "222222229"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190709210"  OR
                                            "190709310"  OR
                                            "190709410")
                   MOVE   "333333339"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190153410"  OR
                                            "190153610"  OR
                                            "190153810")
                   MOVE   "111111199"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190154010"  OR
                                            "190154210"  OR
                                            "190154410")
                   MOVE   "222222299"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190154610"  OR
                                            "190154810"  OR
                                            "190155010")
                   MOVE   "333333399"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190153510"  OR
                                            "190153710"  OR
                                            "190153910")
                   MOVE   "111111999"     TO    NSRY-SRYCD(1)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190154110"  OR
                                            "190154310"  OR
                                            "190154510")
                   MOVE   "222222999"     TO    NSRY-SRYCD(1)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (NSRY-SRYCD(1)    =    "190154710"  OR
                                            "190154910"  OR
                                            "190155110")
                   MOVE   "333333999"     TO    NSRY-SRYCD(1)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
      *
      *        Ê¿À®£²£¶Ç¯£´·î¤«¤é¤Î¿·ÀßÊ¬¡Ê£±¤«¤é£³¡Ë
      *        ¤Ë³ºÅö
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190169610"  OR
                                            "190169710"  OR
                                            "190169810")
                   MOVE   "111119999"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190169910"  OR
                                            "190170010"  OR
                                            "190170110")
                   MOVE   "222229999"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190170210"  OR
                                            "190170310"  OR
                                            "190170410")
                   MOVE   "333339999"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190170810"  OR
                                            "190170910"  OR
                                            "190171010")
                   MOVE   "111199999"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190171110"  OR
                                            "190171210"  OR
                                            "190171310")
                   MOVE   "222299999"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190171410"  OR
                                            "190171510"  OR
                                            "190171610")
                   MOVE   "333399999"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"
                                          TO   TNS-NAME
               END-IF
      *
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´¤«¤é£¶¤ÏÊ¿À®£²£¶Ç¯£³·î¤Þ¤Ç¤Î£±¤«¤é£³
      *        ¤Ë³ºÅö
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190097010"  OR
                                            "190119510"  OR
                                            "190119610")
                   MOVE   "444444444"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190097210"  OR
                                            "190119710"  OR
                                            "190119810")
                   MOVE   "555555555"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£µ"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190135110"  OR
                                            "190135210"  OR
                                            "190135310")
                   MOVE   "666666666"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£¶"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190153410"  OR
                                            "190153610"  OR
                                            "190153810")
                   MOVE   "444444499"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (NSRY-SRYCD(1)    =    "190154010"  OR
                                            "190154210"  OR
                                            "190154410")
                   MOVE   "555555599"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£µ"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190154610"  OR
                                            "190154810"  OR
                                            "190155010")
                   MOVE   "666666699"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£¶"
                                          TO   TNS-NAME
               END-IF
      *
               IF   (NSRY-SRYCD(1)  =
                          LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX))  AND
                    (TNS-NAME(1:50)   = 
                          LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX))
                    MOVE   1      TO    FLG-MATCH
                    IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-NYUINYMD(1:6)
                         MOVE   LNK-ORCHCN02SUB-NYUINYMD(7:2) 
                                         TO   WRK-DDX
                         MOVE   WRK-DD9  TO   ST-IDX
                    ELSE
                         MOVE    1       TO   ST-IDX
                    END-IF
                    IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-TAIINYMD(1:6)
                         MOVE   LNK-ORCHCN02SUB-TAIINYMD(7:2) 
                                         TO   WRK-DDX
                         MOVE   WRK-DD9  TO   ED-IDX
                    ELSE
                         MOVE   31       TO   ED-IDX
                    END-IF
      *             Ã»´üÂÚºß¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤ò¼èÆÀ
                    IF    WRK-TANKI-KBN   =   "1"
                        PERFORM   220-TANKI-SEC
                    ELSE
                        INITIALIZE  ORCSNYUINDAY-AREA
                    END-IF
      *         DISPLAY "====(1)===================="
      *         DISPLAY "LNK-ORCHCN02SUB-SYUBETUMEI(1) = "
      *                  LNK-ORCHCN02SUB-SYUBETUMEI(1)
      *         DISPLAY "LNK-ORCHCN02SUB-SYUBETUMEI(2) = "
      *                  LNK-ORCHCN02SUB-SYUBETUMEI(2)
      *         DISPLAY "ST-IDX = " ST-IDX
      *         DISPLAY "ED-IDX = " ED-IDX
      *
                    PERFORM   VARYING   IDX1   FROM  ST-IDX  BY  1
                      UNTIL   IDX1      >   ED-IDX
                         IF   NACCT-DAY(IDX1)    NOT =  ZERO
                               ADD   1     TO
                                       LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                         ELSE
                           IF  LNK-NYUINDAY-TANKI2-KBN(IDX1) NOT = SPACE
                               ADD   1     TO
                                  LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                           END-IF
                         END-IF
                    END-PERFORM
               END-IF
           END-PERFORM
      *
      *     DISPLAY "FLG-MATCH = " FLG-MATCH
      *
           IF   (FLG-MATCH     =     ZERO)   AND
                (NYUIN-IDX     <=       20)
               MOVE  NSRY-SRYCD(1)   TO
                                 LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
      *        ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÆþ±¡ÎÁÌ¾¾Î¤Î¼èÆÀ
               INITIALIZE    TNS-TENSU-REC
               MOVE   LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                                            TO  TNS-SRYCD
               MOVE   NACCT-SRYYM     TO    TNS-YUKOSTYMD(1:6)
                                            TNS-YUKOEDYMD(1:6)
               MOVE   "01"            TO    TNS-YUKOSTYMD(7:2)
                                            TNS-YUKOEDYMD(7:2)
               PERFORM    910-TENSU-READ-SEC
               MOVE     TNS-NAME      TO
                             LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               IF   ((NACCT-SRYYM      >=   "200604")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097010"  OR
                                            "190097110"  OR
                                            "190119510"  OR
                                            "190119610"  OR
                                            "111111111")
                   MOVE   "111111111"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF   ((NACCT-SRYYM      >=   "200604")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097210"  OR
                                            "190097310"  OR
                                            "190119710"  OR
                                            "190119810"  OR
                                            "222222222")
                   MOVE   "222222222"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF   ((NACCT-SRYYM      >=   "201004")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190135110"  OR
                                            "190135210"  OR
                                            "190135310"  OR
                                            "333333333")
                   MOVE   "333333333"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190708610"  OR
                                            "190708710"  OR
                                            "190708810"  OR
                                            "111111119")
                   MOVE   "111111119"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190708910"  OR
                                            "190709010"  OR
                                            "190709110"  OR
                                            "222222229")
                   MOVE   "222222229"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190709210"  OR
                                            "190709310"  OR
                                            "190709410"  OR
                                            "333333339")
                   MOVE   "333333339"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190153410"  OR
                                            "190153610"  OR
                                            "190153810"  OR
                                            "111111199")
                   MOVE   "111111199"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154010"  OR
                                            "190154210"  OR
                                            "190154410"  OR
                                            "222222299")
                   MOVE   "222222299"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154610"  OR
                                            "190154810"  OR
                                            "190155010"  OR
                                            "333333399")
                   MOVE   "333333399"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190153510"  OR
                                            "190153710"  OR
                                            "190153910"  OR
                                            "111111999")
                   MOVE   "111111999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                       TO      LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154110"  OR
                                            "190154310"  OR
                                            "190154510"  OR
                                            "222222999")
                   MOVE   "222222999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                       TO      LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   ((NACCT-SRYYM      >=   "201204")    AND
                     (NACCT-SRYYM      <=   "201403"))   AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154710"  OR
                                            "190154910"  OR
                                            "190155110"  OR
                                            "333333999")
                   MOVE   "333333999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                       TO      LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´¤«¤é£¶¤ÏÊ¿À®£²£¶Ç¯£³·î¤Þ¤Ç¤Î£±¤«¤é£³
      *        ¤Ë³ºÅö
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097010"  OR
                                            "190119510"  OR
                                            "190119610"  OR
                                            "444444444")
                   MOVE   "444444444"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097210"  OR
                                            "190119710"  OR
                                            "190119810"  OR
                                            "555555555")
                   MOVE   "555555555"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£µ"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190135110"  OR
                                            "190135210"  OR
                                            "190135310"  OR
                                            "666666666")
                   MOVE   "666666666"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£¶"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190153410"  OR
                                            "190153610"  OR
                                            "190153810"  OR
                                            "444444499")
                   MOVE   "444444499"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£´"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154010"  OR
                                            "190154210"  OR
                                            "190154410"  OR
                                            "555555599")
                   MOVE   "555555599"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£µ"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154610"  OR
                                            "190154810"  OR
                                            "190155010"  OR
                                            "666666699")
                   MOVE   "666666699"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£¶"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
      *        Ê¿À®£²£¶Ç¯£´·î¤«¤é¤Î¿·ÀßÊ¬¡Ê£±¤«¤é£³¡Ë
      *        ¤Ë³ºÅö
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190169610"  OR
                                            "190169710"  OR
                                            "190169810"  OR
                                            "111119999")
                   MOVE   "111119999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190169910"  OR
                                            "190170010"  OR
                                            "190170110"  OR
                                            "222229999")
                   MOVE   "222229999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190170210"  OR
                                            "190170310"  OR
                                            "190170410"  OR
                                            "333339999")
                   MOVE   "333339999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190170810"  OR
                                            "190170910"  OR
                                            "190171010"  OR
                                            "111199999")
                   MOVE   "111199999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190171110"  OR
                                            "190171210"  OR
                                            "190171310"  OR
                                            "222299999")
                   MOVE   "222299999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201404")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190171410"  OR
                                            "190171510"  OR
                                            "190171610"  OR
                                            "333399999")
                   MOVE   "333399999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-NYUINYMD(1:6)
                    MOVE   LNK-ORCHCN02SUB-NYUINYMD(7:2) 
                                    TO   WRK-DDX
                    MOVE   WRK-DD9  TO   ST-IDX
               ELSE
                    MOVE    1       TO   ST-IDX
               END-IF
               IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-TAIINYMD(1:6)
                    MOVE   LNK-ORCHCN02SUB-TAIINYMD(7:2) 
                                    TO   WRK-DDX
                    MOVE   WRK-DD9  TO   ED-IDX
               ELSE
                    MOVE   31       TO   ED-IDX
               END-IF
      *        Ã»´üÂÚºß¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤ò¼èÆÀ
               IF    WRK-TANKI-KBN   =   "1"
                   PERFORM   220-TANKI-SEC
               ELSE
                   INITIALIZE  ORCSNYUINDAY-AREA
               END-IF
      *
      *         DISPLAY "====(2)===================="
      *         DISPLAY "LNK-ORCHCN02SUB-SYUBETUMEI(1) = "
      *                  LNK-ORCHCN02SUB-SYUBETUMEI(1)
      *         DISPLAY "LNK-ORCHCN02SUB-SYUBETUMEI(2) = "
      *                  LNK-ORCHCN02SUB-SYUBETUMEI(2)
      *         DISPLAY "ST-IDX = " ST-IDX
      *         DISPLAY "ED-IDX = " ED-IDX
               PERFORM   VARYING   IDX1   FROM  ST-IDX  BY  1
                 UNTIL   IDX1      >   ED-IDX
                     IF   NACCT-DAY(IDX1)    NOT =  ZERO
                          ADD   1     TO
                                  LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                     ELSE
                          IF  LNK-NYUINDAY-TANKI2-KBN(IDX1) NOT = SPACE
                            ADD   1     TO
                                  LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                          END-IF
                     END-IF
               END-PERFORM
           END-IF
      *
           .
       210-NYUIN-COUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÍúÎò¤«¤éÃ»´üÂÚºß¤Î¥³¡¼¥É¤ò¼èÆÀ¤·¡¢Æþ±¡²ñ·×»»Äê¤Î¥³¡¼¥É¤ÈÆÍ¹ç
      *    ¡ÊÉÔ°ìÃ×¤Î¾ì¹ç¤Ïµ­ºÜÂÐ¾Ý³°¡Ë
      *****************************************************************
       210-TANKI-NAMECHK-SEC            SECTION.
      *
           MOVE    SPACE                    TO  WRK-TANKI-KBN2
      *
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    LNK-ORCHCN02SUB-NYUINYMD TO  PTNYUINRRK-NYUINYMD
                                                PTNYUINRRK-TENNYUYMD
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key72"                  TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK          =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key72"             TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               IF    FLG-PTNYUINRRK        =   ZERO
      *             DISPLAY "PTNYUINRRK  KEY72  READ"
      *             DISPLAY "PTNYUINRRK-TOKUTEI-KBN = "
      *                      PTNYUINRRK-TOKUTEI-KBN
      *             DISPLAY "PTNYUINRRK-TOKUTEI-NYUIN = "
      *                      PTNYUINRRK-TOKUTEI-NYUIN
      *****             ADD   PTNYUINRRK-TAISYONISSU
      *****                                 TO   LNK-ORCHCN02SUB-NISSU(1)
                    CONTINUE
               END-IF
           END-IF
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key72"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤ò¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ
           IF    (PTNYUINRRK-TOKUTEI-KBN   NOT =   SPACE)   AND
                 (PTNYUINRRK-TOKUTEI-NYUIN NOT =   SPACE)
             INITIALIZE                           ORCSTOKNYUIN-AREA
             MOVE    PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
             MOVE    PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
             MOVE    PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
             MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                        TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
             MOVE    LNK-ORCHCN02SUB-NYUINYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
             CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                  SPA-AREA
             IF      LNK-TOKNYUIN-RC          =   ZERO
               IF    LNK-TOKNYUIN-GAI-SRYCD =  SPACE
                  INITIALIZE    TTNS-TENSU-REC
                  MOVE   LNK-TOKNYUIN-SRYCD  TO    TTNS-SRYCD
                  MOVE   LNK-ORCHCN02SUB-NYUINYMD(1:6)
                                             TO    TTNS-YUKOSTYMD(1:6)
                                                   TTNS-YUKOEDYMD(1:6)
                  MOVE   "01"                TO    TTNS-YUKOSTYMD(7:2)
                                                   TTNS-YUKOEDYMD(7:2)
                  PERFORM    910-TTENSU-READ-SEC
               END-IF
             END-IF
           END-IF
      *
           .
       210-TANKI-NAMECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ã»´üÂÚºß¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤ò¼èÆÀ
      *****************************************************************
       220-TANKI-SEC            SECTION.
      *
           INITIALIZE                            ORCSNYUINDAY-AREA
           MOVE    LNK-ORCHCN02SUB-HOSPNUM   TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID      TO  LNK-NYUINDAY-PTID
           MOVE    NACCT-SRYYM               TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"        USING   ORCSNYUINDAY-AREA
                                                 SPA-AREA
      **     DISPLAY   "==== ORCSNYUINDAY ===="
      **     DISPLAY   "LNK-NYUINDAY-RC = " LNK-NYUINDAY-RC
      **     DISPLAY   "NACCT-SRYYM = " NACCT-SRYYM
      **     DISPLAY   "----5----0----5----0----5----0-"
      **     DISPLAY   LNK-NYUINDAY-TANKI2-TBL
      *
           .
       220-TANKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-TANKI-KBN
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
      *            Ã»´üÂÚºß¤ÎÈ½ÄêÍÑ
                   IF    (TNS-CDKBN-KBN     =   "A")   AND
                         (TNS-CDKBN-KBNNUM  =   400)   AND
                         (TNS-CDKBN-KOUBAN  =   2  OR 3)
                       MOVE   "1"       TO  WRK-TANKI-KBN
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼¥ï¡¼¥¯ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TTENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TTNS-HOSPNUM
           MOVE    TTNS-TENSU-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TTNS-TENSU-REC
      *            Ã»´üÂÚºß¤ÎÈ½ÄêÍÑ
                   IF    (TTNS-CDKBN-KBN     =   "A")   AND
                         (TTNS-CDKBN-KBNNUM  =   400)   AND
                         (TTNS-CDKBN-KOUBAN  =   2  OR 3)
                       IF    TNS-SRYCD   =    TTNS-SRYCD
                          CONTINUE
                       ELSE
                          MOVE   "1"       TO  WRK-TANKI-KBN2
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TTENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
