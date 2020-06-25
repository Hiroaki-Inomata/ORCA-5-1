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
       PROGRAM-ID.                 ORCSEIYOUKKS.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ±ÉÍÜ´ÉÍý·Ð²áÁ¼ÃÖÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  12/03/12    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
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
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK5000.INC".
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
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ±ÉÍÜ´ÉÍý·Ð²áÁ¼ÃÖÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCSEIYOUKKS.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSEIYOUKKS-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  (LNK-EIYOUKKS-SANTEIYM    >=   "201204")   AND
               (LNK-EIYOUKKS-SANTEIYM    <=   "201403")
      *        ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡Ê±ÉÍÜ´ÉÍý·Ð²áÁ¼ÃÖ¤Î¶èÊ¬¼èÆÀ¡Ë
               MOVE    SPACE               TO  SYS-5000-REC
               INITIALIZE                      SYS-5000-REC
               MOVE    "5000"              TO  SYS-5000-KANRICD
               MOVE    "*"                 TO  SYS-5000-KBNCD
               MOVE    LNK-EIYOUKKS-SANTEIYM
                                           TO  SYS-5000-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-5000-STYUKYMD(7:2)
               MOVE    SYS-5000-STYUKYMD   TO  SYS-5000-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
               MOVE    SYS-5000-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    ZERO                TO  FLG-SYSKANRI
                       MOVE    MCPDATA-REC         TO  SYS-5000-REC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               ELSE
                   MOVE    SPACE               TO  SYS-5000-REC
                   INITIALIZE                  SYS-5000-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF    SYS-5000-EIYOUKSNKBN  =   "1"
                   IF    LNK-EIYOUKKS-SRYCD    NOT  =  SPACE
                       PERFORM    200-CDCHG1-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
      *    Ê¿À®£²£¶Ç¯£´·î¤«¤é£¶·îÊ¬¤Î¥ì¥»¥×¥ÈÎ¬¾Îµ­ºÜÍÑ¤ËÊÖµÑ
           IF  (LNK-EIYOUKKS-SANTEIYM    >=   "201404")   AND
               (LNK-EIYOUKKS-SANTEIYM    <=   "201406")
      *        ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷¡Ê±ÉÍÜ´ÉÍý·Ð²áÁ¼ÃÖ¤Î¶èÊ¬¼èÆÀ¡Ë
               MOVE    SPACE               TO  SYS-5000-REC
               INITIALIZE                      SYS-5000-REC
               MOVE    "5000"              TO  SYS-5000-KANRICD
               MOVE    "*"                 TO  SYS-5000-KBNCD
               MOVE    LNK-EIYOUKKS-SANTEIYM
                                           TO  SYS-5000-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-5000-STYUKYMD(7:2)
               MOVE    SYS-5000-STYUKYMD   TO  SYS-5000-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
               MOVE    SYS-5000-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    ZERO                TO  FLG-SYSKANRI
                       MOVE    MCPDATA-REC         TO  SYS-5000-REC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               ELSE
                   MOVE    SPACE               TO  SYS-5000-REC
                   INITIALIZE                  SYS-5000-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF    SYS-5000-EIYOUKSNKBN  =   "1"
                   IF    LNK-EIYOUKKS-SRYCD    NOT  =  SPACE
                       PERFORM    200-CDCHG1-SEC
                       IF   LNK-EIYOUKKS-CHGSRYCD  NOT =   SPACE
                         MOVE    LNK-EIYOUKKS-CHGSRYCD
                                        TO  LNK-EIYOUKKS-RECE-CHGSRYCD
                         MOVE    SPACE  TO  LNK-EIYOUKKS-CHGSRYCD
                       END-IF
                   END-IF
      *
               END-IF
           END-IF
      *
           IF    LNK-EIYOUKKS-CHGSRYCD   =     SPACE
               MOVE    1                 TO     LNK-EIYOUKKS-RC
           END-IF
      *
           IF  (LNK-EIYOUKKS-SANTEIYM    >=   "201404")   AND
               (LNK-EIYOUKKS-SANTEIYM    <=   "201406")
               IF   (SYS-5000-EIYOUKSNKBN  =   "1")    AND
                    (LNK-EIYOUKKS-RECE-CHGSRYCD  = SPACE)
                  MOVE    9              TO     LNK-EIYOUKKS-RC
               END-IF
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    ¥³¡¼¥ÉÃÖ¤­´¹¤¨¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
      *    Ê¿À®£²£´Ç¯£´·î¤«¤éÊ¿À®£²£¶Ç¯£³·î¤Þ¤Ç
      *****************************************************************
       200-CDCHG1-SEC                    SECTION.
      *
           EVALUATE    LNK-EIYOUKKS-SRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê£±£´Æü°ÊÆâ¡Ë
               WHEN    "190153410"
                  MOVE "190153510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê£±£µ¡Á£³£°Æü¡Ë
               WHEN    "190153610"
                  MOVE "190153710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190153810"
                  MOVE "190153910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê£±£´Æü°ÊÆâ¡Ë
               WHEN    "190154010"
                  MOVE "190154110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê£±£µ¡Á£³£°Æü¡Ë
               WHEN    "190154210"
                  MOVE "190154310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190154410"
                  MOVE "190154510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê£±£´Æü°ÊÆâ¡Ë
               WHEN    "190154610"
                  MOVE "190154710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê£±£µ¡Á£³£°Æü¡Ë
               WHEN    "190154810"
                  MOVE "190154910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190155010"
                  MOVE "190155110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Á
               WHEN    "190155210"
                  MOVE "190155310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Á¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190155410"
                  MOVE "190155510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Â 
               WHEN    "190155610"
                  MOVE "190155710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Â¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190155810"
                  MOVE "190155910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ã
               WHEN    "190156010"
                  MOVE "190156110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ã¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190156210"
                  MOVE "190156310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ä
               WHEN    "190156410"
                  MOVE "190156510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ä¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190156610"
                  MOVE "190156710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Å
               WHEN    "190156810"
                  MOVE "190156910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Å¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190157010"
                  MOVE "190157110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Á¡Ë
               WHEN    "190157210"
                  MOVE "190157310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Á¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190157410"
                  MOVE "190157510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Â¡Ë
               WHEN    "190157610"
                  MOVE "190157710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Â¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190157810"
                  MOVE "190157910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ã¡Ë
               WHEN    "190158010"
                  MOVE "190158110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ã¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190158210"
                  MOVE "190158310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ä¡Ë
               WHEN    "190158410"
                  MOVE "190158510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ä¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190158610"
                  MOVE "190158710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Å¡Ë
               WHEN    "190158810"
                  MOVE "190158910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Å¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190159010"
                  MOVE "190159110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¡Ë
               WHEN    "190159210"
                  MOVE "190159310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190159410"
                  MOVE "190159510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ç¡Ë
               WHEN    "190159610"
                  MOVE "190159710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ç¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190159810"
                  MOVE "190159910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£È¡Ë
               WHEN    "190160010"
                  MOVE "190160110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£È¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190160210"
                  MOVE "190160310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£É¡Ë
               WHEN    "190160410"
                  MOVE "190160510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£É¡¦À¸³èÎÅÍÜ¡Ë
               WHEN    "190160610"
                  MOVE "190160710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Á¡Ë
               WHEN    "190162210"
                  MOVE "190162310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Â¡Ë
               WHEN    "190162410"
                  MOVE "190162510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ã¡Ë
               WHEN    "190162610"
                  MOVE "190162710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ä¡Ë
               WHEN    "190162810"
                  MOVE "190162910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Å¡Ë
               WHEN    "190163010"
                  MOVE "190163110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¡Ë
               WHEN    "190163210"
                  MOVE "190163310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ç¡Ë
               WHEN    "190163410"
                  MOVE "190163510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£È¡Ë
               WHEN    "190163610"
                  MOVE "190163710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£É¡Ë
               WHEN    "190163810"
                  MOVE "190163910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190117710"
                  MOVE "190700210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190077410"
                  MOVE "190700310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190077510"
                  MOVE "190700410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190077610"
                  MOVE "190700510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ê·Ð²áÁ¼ÃÖ¡Ë
               WHEN    "190700110"
                  MOVE "190700610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
               WHEN    "190799410"
                  MOVE "190700810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë
               WHEN    "190121310"
                  MOVE "190700910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190123710"
                  MOVE "190701010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë
               WHEN    "190121410"
                  MOVE "190701110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190123810"
                  MOVE "190701210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë
               WHEN    "190121510"
                  MOVE "190701310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190123910"
                  MOVE "190701410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë
               WHEN    "190121610"
                  MOVE "190701510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190124010"
                  MOVE "190701610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë
               WHEN    "190121710"
                  MOVE "190701710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190124110"
                  MOVE "190701810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë
               WHEN    "190131610"
                  MOVE "190701910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190131710"
                  MOVE "190702010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë
               WHEN    "190131810"
                  MOVE "190702110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190131910"
                  MOVE "190702210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë
               WHEN    "190132010"
                  MOVE "190702310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190132110"
                  MOVE "190702410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë
               WHEN    "190132210"
                  MOVE "190702510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190132310"
                  MOVE "190702610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë
               WHEN    "190132410"
                  MOVE "190702710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190132510"
                  MOVE "190702810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë
               WHEN    "190132610"
                  MOVE "190702910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190132710"
                  MOVE "190703010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë
               WHEN    "190132810"
                  MOVE "190703110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190132910"
                  MOVE "190703210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë
               WHEN    "190133010"
                  MOVE "190703310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190133110"
                  MOVE "190703410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë
               WHEN    "190133210"
                  MOVE "190703510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190133310"
                  MOVE "190703610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë
               WHEN    "190133410"
                  MOVE "190703710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190133510"
                  MOVE "190703810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë
               WHEN    "190133610"
                  MOVE "190703910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190133710"
                  MOVE "190704010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë
               WHEN    "190133810"
                  MOVE "190704110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190133910"
                  MOVE "190704210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë
               WHEN    "190134010"
                  MOVE "190704310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190134110"
                  MOVE "190704410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ·ë³ËÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190117910"
                  MOVE "190704510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ·ë³ËÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190081710"
                  MOVE "190704610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ·ë³ËÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190081810"
                  MOVE "190704710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ·ë³ËÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190081910"
                  MOVE "190704810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ·ë³ËÉÂÅï£±£¸ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190082010"
                  MOVE "190704910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ·ë³ËÉÂÅï£²£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190082110"
                  MOVE "190705010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿ÀÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190083610"
                  MOVE "190705110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿ÀÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190134510"
                  MOVE "190705210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿ÀÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190083810"
                  MOVE "190705310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿ÀÉÂÅï£±£¸ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190083910"
                  MOVE "190705410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿ÀÉÂÅï£²£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190084010"
                  MOVE "190705510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190118610"
                  MOVE "190705710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190085910"
                  MOVE "190705810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ê·Ð²áÁ¼ÃÖ¡Ë
               WHEN    "190705610"
                  MOVE "190705910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡·ë³ËÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190118810"
                  MOVE "190706010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡·ë³ËÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190086410"
                  MOVE "190706110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡·ë³ËÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190086510"
                  MOVE "190706210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡·ë³ËÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190118910"
                  MOVE "190706310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190119010"
                  MOVE "190706410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190086910"
                  MOVE "190706510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190134710"
                  MOVE "190706610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190119110"
                  MOVE "190706710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÀìÌçÉÂ±¡£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190119310"
                  MOVE "190706910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÀìÌçÉÂ±¡£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190090210"
                  MOVE "190707010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÀìÌçÉÂ±¡£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190090310"
                  MOVE "190707110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÀìÌçÉÂ±¡£·ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ê·Ð²áÁ¼ÃÖ¡Ë
               WHEN    "190706810"
                  MOVE "190707210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾ã³²¼Ô»ÜÀßÅù£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190126310"
                  MOVE "190707310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾ã³²¼Ô»ÜÀßÅù£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190093710"
                  MOVE "190707410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾ã³²¼Ô»ÜÀßÅù£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190093810"
                  MOVE "190707510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾ã³²¼Ô»ÜÀßÅù£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190093910"
                  MOVE "190707610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê£±£´Æü°ÊÆâ¡Ë
               WHEN    "190097010"
                  MOVE "190708610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê£±£µÆü°Ê¾å£³£°Æü°ÊÆâ¡Ë
               WHEN    "190119510"
                  MOVE "190708710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190119610"
                  MOVE "190708810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê£±£´Æü°ÊÆâ¡Ë
               WHEN    "190097210"
                  MOVE "190708910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê£±£µÆü°Ê¾å£³£°Æü°ÊÆâ¡Ë
               WHEN    "190119710"
                  MOVE "190709010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190119810"
                  MOVE "190709110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê£±£´Æü°ÊÆâ¡Ë
               WHEN    "190135110"
                  MOVE "190709210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê£±£µÆü°Ê¾å£³£°Æü°ÊÆâ¡Ë
               WHEN    "190135210"
                  MOVE "190709310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190135310"
                  MOVE "190709410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Á
               WHEN    "190122010"
                  MOVE "190711410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Á¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190124310"
                  MOVE "190711510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Â
               WHEN    "190122110"
                  MOVE "190711610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Â¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190124410"
                  MOVE "190711710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ã
               WHEN    "190122210"
                  MOVE "190711810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ã¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190124510"
                  MOVE "190711910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ä
               WHEN    "190122310"
                  MOVE "190712010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ä¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190124610"
                  MOVE "190712110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Å
               WHEN    "190122410"
                  MOVE "190712210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Å¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190124710"
                  MOVE "190712310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£³Æü°ÊÆâ¡Ë
               WHEN    "190024510"
                  MOVE "190714410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£´¡Á£·Æü¡Ë
               WHEN    "190128610"
                  MOVE "190714510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£¸¡Á£±£´Æü¡Ë
               WHEN    "190074510"
                  MOVE "190714610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£³Æü°ÊÆâ¡Ë
               WHEN    "190024310"
                  MOVE "190714710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£´¡Á£·Æü¡Ë
               WHEN    "190128710"
                  MOVE "190714810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£¸¡Á£±£´Æü¡Ë
               WHEN    "190024410"
                  MOVE "190714910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞ¡Ë¡Ê£³Æü°ÊÆâ¡Ë
               WHEN    "190138110"
                  MOVE "190715010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞ¡Ë¡Ê£´¡Á£·Æü¡Ë
               WHEN    "190138210"
                  MOVE "190715110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞ¡Ë¡Ê£¸¡Á£±£´Æü¡Ë
               WHEN    "190138310"
                  MOVE "190715210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£³Æü°ÊÆâ¡Ë
               WHEN    "190138410"
                  MOVE "190715310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£´¡Á£·Æü¡Ë
               WHEN    "190138510"
                  MOVE "190715410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£¸¡Á£¶£°Æü¡Ë
               WHEN    "190138610"
                  MOVE "190715510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë¡Ê£³Æü°ÊÆâ¡Ë
               WHEN    "190138710"
                  MOVE "190715610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë¡Ê£´¡Á£·Æü¡Ë
               WHEN    "190138810"
                  MOVE "190715710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë¡Ê£¸¡Á£±£´Æü¡Ë
               WHEN    "190138910"
                  MOVE "190715810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£³Æü°ÊÆâ¡Ë
               WHEN    "190139010"
                  MOVE "190715910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£´¡Á£·Æü¡Ë
               WHEN    "190139110"
                  MOVE "190716010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£¸¡Á£±£´Æü¡Ë
               WHEN    "190139210"
                  MOVE "190716110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£±£µ¡Á£¶£°Æü¡Ë
               WHEN    "190139310"
                  MOVE "190716210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±¡Ê£·Æü°ÊÆâ¡Ë
               WHEN    "190116310"
                  MOVE "190717210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±¡Ê£¸Æü°Ê¾å£±£´Æü°ÊÆâ¡Ë
               WHEN    "190116410"
                  MOVE "190717310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¡¦£·Æü°ÊÆâ¡Ë
               WHEN    "190139810"
                  MOVE "190717410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¡¦£¸¡Á£±£´Æü¡Ë
               WHEN    "190139910"
                  MOVE "190717510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£·Æü°ÊÆâ¡Ë
               WHEN    "190140010"
                  MOVE "190717610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ý¡¦£¸¡Á£¶£°Æü¡Ë
               WHEN    "190140110"
                  MOVE "190717710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
               WHEN    "190117310"
                  MOVE "190717810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
               WHEN    "190120810"
                  MOVE "190717910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê£·Æü°ÊÆâ¡Ë
               WHEN    "190149910"
                  MOVE "190718010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê£¸Æü°Ê¾å£±£´Æü°ÊÆâ¡Ë
               WHEN    "190150010"
                  MOVE "190718110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±
               WHEN    "190024710"
                  MOVE "190718210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¿·À¸»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²
               WHEN    "190140410"
                  MOVE "190718310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡ÊÊìÂÎ¡¦ÂÛ»ù¡Ë
               WHEN    "190066710"
                  MOVE "190718410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Áí¹ç¼þ»º´üÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ê¿·À¸»ù¡Ë
               WHEN    "190066810"
                  MOVE "190718510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¿·À¸»ù¼£ÎÅ²óÉü¼¼Æþ±¡°åÎÅ´ÉÍýÎÁ
               WHEN    "190140510"
                  MOVE "190718610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£·Æü°ÊÆâ¡Ë
               WHEN    "190075510"
                  MOVE "190718710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£¸Æü¡Á£±£´Æü°ÊÆâ¡Ë
               WHEN    "190075610"
                  MOVE "190718810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃ¼ì¼À´µÆþ±¡°åÎÅ´ÉÍýÎÁ
               WHEN    "190075710"
                  MOVE "190718910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£±
               WHEN    "190129310"
                  MOVE "190719010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£²
               WHEN    "190140710"
                  MOVE "190719110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£³
               WHEN    "190111110"
                  MOVE "190719210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£´
               WHEN    "190111210"
                  MOVE "190719310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ£µ
               WHEN    "190075810"
                  MOVE "190719410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±
               WHEN    "190150110"
                  MOVE "190719510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190150210"
                  MOVE "190719610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²
               WHEN    "190141610"
                  MOVE "190719710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190141710"
                  MOVE "190719810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³
               WHEN    "190141810"
                  MOVE "190719910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë
               WHEN    "190141910"
                  MOVE "190720010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±
               WHEN    "190117410"
                  MOVE "190720110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
               WHEN    "190129910"
                  MOVE "190720210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë
               WHEN    "190150310"
                  MOVE "190720310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë
               WHEN    "190150410"
                  MOVE "190720410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃ¼ì¼À´µÉÂÅïÆþ±¡ÎÁ£±
               WHEN    "190055210"
                  MOVE "190720510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃ¼ì¼À´µÉÂÅïÆþ±¡ÎÁ£²
               WHEN    "190055310"
                  MOVE "190720610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190028910"
                  MOVE "190720710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ê£³£±Æü°Ê¾å£¶£°Æü°ÊÆâ¡Ë
               WHEN    "190150510"
                  MOVE "190720810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ê£¶£±Æü°Ê¾å¡Ë
               WHEN    "190150610"
                  MOVE "190720910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190121010"
                  MOVE "190721010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµßµÞÆþ±¡ÎÁ£±¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190111410"
                  MOVE "190721110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190130010"
                  MOVE "190721210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµßµÞÆþ±¡ÎÁ£²¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190130110"
                  MOVE "190721310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190121110"
                  MOVE "190721410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190066910"
                  MOVE "190721510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190121210"
                  MOVE "190721610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190067010"
                  MOVE "190721710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190130210"
                  MOVE "190721810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ¡Ê£³£±Æü°Ê¾å¡Ë
               WHEN    "190130310"
                  MOVE "190721910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍýÎÁ
               WHEN    "190151110"
                  MOVE "190722010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ
               WHEN    "190055010"
                  MOVE "190722110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190151610"
                  MOVE "190722310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡Ê£³£±Æü°Ê¾å£¶£°Æü°ÊÆâ¡Ë
               WHEN    "190739910"
                  MOVE "190722410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡Ê£¶£±Æü°Ê¾å¡Ë
               WHEN    "190740010"
                  MOVE "190722510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡Ê£³£°Æü°ÊÆâ¡Ë
               WHEN    "190151710"
                  MOVE "190722610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡Ê£³£±Æü°Ê¾å£¶£°Æü°ÊÆâ¡Ë
               WHEN    "190813410"
                  MOVE "190722710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²¡Ê£¶£±Æü°Ê¾å¡Ë
               WHEN    "190813510"
                  MOVE "190722810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ£±
               WHEN    "190151910"
                  MOVE "190722910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ£²
               WHEN    "190152010"
                  MOVE "190723010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë
               WHEN    "190152610"
                  MOVE "190723110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´ü¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
               WHEN    "190152710"
                  MOVE "190723210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Æþ±¡´ðËÜÎÁ¡Ë
               WHEN    "190160810"
                  MOVE "190751810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡ÊÀìÌçÉÂ±¡Æþ±¡´ðËÜÎÁ¡Ë
               WHEN    "190161010"
                  MOVE "190751910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ¡Ë
               WHEN    "190161210"
                  MOVE "190752010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡Ë°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190117810"
                  MOVE "190752830" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡Ë°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ê·Ð²áÁ¼ÃÖ¡Ë
               WHEN    "190752730"
                  MOVE "190752930" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡Ë°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190111810"
                  MOVE "190753030" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡Ë°ìÈÌÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190111910"
                  MOVE "190753130" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡Ë°ìÈÌÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190112010"
                  MOVE "190753230" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÆÃÄêÆþ±¡´ðËÜÎÁ
               WHEN    "190811210"
                  MOVE "190753330" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÆÃÄêµ¡Ç½ÉÂ±¡°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190118710"
                  MOVE "190753530" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÆÃÄêµ¡Ç½ÉÂ±¡°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ê·Ð²á¡Ë
               WHEN    "190753430"
                  MOVE "190753630" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÆÃÄêµ¡Ç½ÉÂ±¡°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190114710"
                  MOVE "190753730" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÀìÌçÉÂ±¡£·ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190119410"
                  MOVE "190753930" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÀìÌçÉÂ±¡£·ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ê·Ð²áÁ¼ÃÖ¡Ë
               WHEN    "190753830"
                  MOVE "190754030" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÀìÌçÉÂ±¡£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190115710"
                  MOVE "190754130" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ¡ÊÁª¡ËÀìÌçÉÂ±¡£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
               WHEN    "190115810"
                  MOVE "190754230" TO  LNK-EIYOUKKS-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       200-CDCHG1-EXT.
           .
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
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
