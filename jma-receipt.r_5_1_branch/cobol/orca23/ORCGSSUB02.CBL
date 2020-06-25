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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGSSUB02.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¼ýÇ¼
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¼ýÇ¼ÌÀºÙÊÔ½¸½èÍý
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  04/11/25    NACL-ÂÀÅÄ     ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô        ÆüÉÕ      ÆâÍÆ
      *  03.04.01    NACL-¿¹ÏÆ     06/12/19  ¥×¥ì¥Ó¥å¡¼ÄÉ²Ã
      *  03.05.00    NACL-ÂÀÅÄ     07/05/20  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYURRK          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDXM                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DENPNUM         PIC 9(07).
      *
       01  CONST-AREA.
           03  CONST-EDANUM-MAX    PIC 9(03)   VALUE 99.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¼ýÇ¼ÌÀºÙ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    ¼ýÇ¼ÍúÎò
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPSSUB02.INC".
       01  LNKSMEI-AREA.
         02  LNKSMEI-OCC           OCCURS    99.
           COPY "CPSYUMEI.INC"     REPLACING //SMEI-//
                                   BY        //LNKSMEI-//.
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SSUB02-AREA
           LNKSMEI-AREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              SSUB02-OT
                                   LNKSMEI-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-SYUMEI-KEY6-SEL-SEC
      *
           MOVE    ZERO                TO  IDXM
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( IDXM        >=  CONST-EDANUM-MAX )
      *
               COMPUTE IDXM    =   IDXM    +   1
               MOVE    IDXM            TO  SSUB02-MAX
      *
               MOVE    1               TO  SSUB02-VISIBLE (IDXM)
      *
               IF    ( SMEI-JOUTAIKBN      =   "2" OR "4" OR "6" )
                AND  ( SMEI-NYUHEN-MONEY   =   ZERO )
                       MOVE    ZERO    TO  SSUB02-VISIBLE (IDXM)
               END-IF
      *
               IF    ( SMEI-JOUTAIKBN      =   "4" OR  "6" )
                OR  (( SMEI-JOUTAIKBN      =   "8" )
                 AND ( SMEI-NYUHEN-MONEY   <   ZERO ))
                   MOVE    "1"         TO  SSUB02-MINUS (IDXM)
               END-IF
      *
               EVALUATE    SMEI-JOUTAIKBN
               WHEN    SPACE
                   MOVE   "ÀÁµá³Û¤Ê¤·"
                                       TO  SSUB02-JOUTAI (IDXM)
               WHEN    "1"
                   IF   ( SMEI-SKYMONEY    >   SMEI-NYUHEN-MONEY )
                       MOVE   "ÀÁµá¡¦Ì¤¼ý"
                                       TO  SSUB02-JOUTAI (IDXM)
                   ELSE
                       MOVE   "ÀÁµá¡¦Æþ¶â"
                                       TO  SSUB02-JOUTAI (IDXM)
                   END-IF
      *
               WHEN    "2"
                   MOVE    "Æþ¶â"      TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "3"
                   MOVE    "ºÆ·×»»"    TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "4"
                   MOVE    "ÊÖ¶â"      TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "5"
                   MOVE    "ÀÁµá¼è¾Ã"  TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "6"
                   MOVE    "Æþ¶â¼è¾Ã"  TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "7"
                   MOVE    "·ÑÂ³"      TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "8"
                   MOVE    "¿ÇÎÅÄûÀµ"  TO  SSUB02-JOUTAI (IDXM)
               WHEN    "9"
                   MOVE    "ÀÁµá"      TO  SSUB02-JOUTAI (IDXM)
               END-EVALUATE
      *
               EVALUATE    SMEI-JOUTAIKBN
               WHEN    " "
               WHEN    "1"
               WHEN    "9"
                   MOVE    1           TO  SSUB02-PROC-FLG (IDXM)
               WHEN    "3"
               WHEN    "7"
               WHEN    "8"
               WHEN    "5"
                   MOVE    2           TO  SSUB02-PROC-FLG (IDXM)
               WHEN    "2"
                   MOVE    3           TO  SSUB02-PROC-FLG (IDXM)
               WHEN    "4"
               WHEN    "6"
                   MOVE    4           TO  SSUB02-PROC-FLG (IDXM)
               END-EVALUATE
      *
               IF    ( SMEI-SYURRKNUM  NOT =   ZERO )
                   PERFORM 900-SYURRK-KEY-SEL-SEC
                   IF    ( FLG-SYURRK      =   ZERO )
                       MOVE    SYURRK-CHOSEI1
                                       TO  SSUB02-CHOSEI1 (IDXM)
                       MOVE    SYURRK-CHOSEI2
                                       TO  SSUB02-CHOSEI2 (IDXM)
                   ELSE
                       MOVE    ZERO    TO  SMEI-SYURRKNUM
                                           SMEI-SYUEDANUM
                   END-IF
               END-IF
      *
               MOVE    SYUMEI-REC      TO  LNKSMEI-OCC (IDXM)
      *
               PERFORM 900-SYUMEI-KEY6-FET-SEC
      *
           END-PERFORM
      *
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÌÀºÙ¸¡º÷½èÍý
      *****************************************************************
       900-SYUMEI-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-PTID            TO  SMEI-PTID
           MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SSUB02-DENPNUM      TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-SEL-EXT.
           EXIT.
      *
      **************************************************************
      *    ¼ýÇ¼ÌÀºÙFETCH½èÍý
      **************************************************************
       900-SYUMEI-KEY6-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÍúÎò¸¡º÷½èÍý
      *****************************************************************
       900-SYURRK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SMEI-SYUEDANUM      TO  SYURRK-SYUEDANUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY-SEL-EXT.
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý
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
       910-DBSELECT-EXT.
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
      *****************************************************************
      *    £Ä£Â£Ä£Å£Ì£Å£Ô£Å½èÍý
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Õ£Ð£Ä£Á£Ô£Å½èÍý
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¤¥ó¥µ¡¼¥È½èÍý
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
