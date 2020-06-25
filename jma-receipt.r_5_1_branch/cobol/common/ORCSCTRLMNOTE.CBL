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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCTRLMNOTE.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¤ªÌô¼êÄ¢¥Ç¡¼¥¿´ÉÍý
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¤ªÌô¼êÄ¢¥Ç¡¼¥¿À©¸æ
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  13/07/12    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-MNOTE-H         PIC 9(01).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¤ªÌô¼êÄ¢¥Ç¡¼¥¿´ÉÍý£Ä£Â
       01  MNOTE-H-REC.
           COPY    "CPMNOTE-H.INC".
      *
      *    ¤ªÌô¼êÄ¢¥Ç¡¼¥¿ÌÀºÙ£Ä£Â
       01  MNOTE-B-REC.
           COPY    "CPMNOTE-B.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *     
      *****************************************************************
      *
       LINKAGE                     SECTION.
      *    ¤ªÌô¼êÄ¢£Ã£Ó£ÖÀ©¸æ¥µ¥Ö
           COPY    "CPORCSMNOTE.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   ORCSMNOTEAREA
                                   SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    ½èÍý¥â¡¼¥É¤Î¥Á¥§¥Ã¥¯
           EVALUATE    SMNOTE-MODE
           WHEN    "INS"
               PERFORM 110-INS-SEC
           WHEN    "UPD"
               PERFORM 120-UPD-SEC
           WHEN    "DEL"
               PERFORM 140-DEL-SEC
           WHEN    OTHER
               MOVE  10        TO  SMNOTE-RETURN
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    Åý·×£Ã£Ó£Ö£Ä£ÂÄÉ²Ã½èÍý
      *****************************************************************
       110-INS-SEC                 SECTION.
      *
           PERFORM 1101-MNOTE-H-HENSYU-SEC
      *
           IF    ( MCP-RC      =   ZERO )
               PERFORM 1102-MNOTE-B-HENSYU-SEC
           END-IF
      *
           MOVE  MCP-RC        TO  SMNOTE-RETURN
      *
           .
       110-INS-EXT.
           EXIT.
      *****************************************************************
      *    Åý·×£Ã£Ó£Ö´ÉÍý£Ä£Â¹¹¿·½èÍý
      *****************************************************************
       1101-MNOTE-H-HENSYU-SEC SECTION.
      *
           PERFORM 900-MNOTE-H-KEY-SEL-SEC
           IF    ( FLG-MNOTE-H     =   ZERO )
               ADD     1              TO  MNOTE-H-CNT
           ELSE
               MOVE    SPACE              TO   MNOTE-H-REC
               INITIALIZE                      MNOTE-H-REC
               MOVE    SPA-HOSPNUM        TO   MNOTE-H-HOSPNUM
               MOVE    SMNOTE-TBL-KEY     TO   MNOTE-H-TBL-KEY
               MOVE    SMNOTE-RENNUM      TO   MNOTE-H-RENNUM
               MOVE    SMNOTE-TBL-GROUP
                                          TO   MNOTE-H-TBL-GROUP
               MOVE    SMNOTE-TBL-UUID    TO   MNOTE-H-TBL-UUID
               MOVE    SMNOTE-SHORI-RENNUM
                                          TO   MNOTE-H-SHORI-RENNUM
               MOVE    SMNOTE-GYOUMU-CD   TO   MNOTE-H-GYOUMU-CD
               MOVE    SMNOTE-LOCATION-CD TO   MNOTE-H-LOCATION-CD
               MOVE    SMNOTE-SHELLID     TO   MNOTE-H-SHELLID
               MOVE    SMNOTE-SRYYM       TO   MNOTE-H-SRYYM
               MOVE    SMNOTE-SRYYMD      TO   MNOTE-H-SRYYMD
               MOVE    1                  TO   MNOTE-H-CNT
               IF     ( SMNOTE-PUTFLG  =  "1" )
                   MOVE    "1"            TO  MNOTE-H-PUTFLG
               ELSE
                   MOVE    SPACE          TO  MNOTE-H-PUTFLG
               END-IF
               MOVE    SMNOTE-NYUGAIKBN   TO  MNOTE-H-NYUGAIKBN
               MOVE    SMNOTE-PTID        TO  MNOTE-H-PTID
               MOVE    SMNOTE-DRCD        TO  MNOTE-H-DRCD
               MOVE    SMNOTE-TO-FOLDER   TO  MNOTE-H-TO-FOLDER
               MOVE    SMNOTE-TO-DATA     TO  MNOTE-H-TO-DATA
               MOVE    SMNOTE-CODE-TYPE
                                          TO  MNOTE-H-CODE-TYPE
               MOVE    SMNOTE-TITLE       TO  MNOTE-H-TITLE
               MOVE    SMNOTE-DATA-VERSION
                                          TO  MNOTE-H-DATA-VERSION
               MOVE    SPA-OPID           TO  MNOTE-H-OPID
           END-IF
      *
           MOVE    SMNOTE-TBL-GROUP(1:8)  TO  MNOTE-H-CREYMD
      *
           MOVE    MNOTE-H-REC            TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"          TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           IF    ( FLG-MNOTE-H     =   ZERO )
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC         =   ZERO )
                   MOVE    MCP-RC         TO  SMNOTE-RETURN
               ELSE
                   DISPLAY "MNOTE-H UPDATE ERR KEY = ["
                           MCP-RC  " " MNOTE-H-KEY  "]"
                   MOVE  MCP-RC           TO  SMNOTE-RETURN
               END-IF
           ELSE
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC             =   ZERO )
                   MOVE  MCP-RC           TO  SMNOTE-RETURN
               ELSE
                   DISPLAY "MNOTE-H INSERT ERR KEY = ["
                           MCP-RC  " " MNOTE-H-KEY  "]"
                   MOVE  MCP-RC           TO  SMNOTE-RETURN
               END-IF
           END-IF
      *
           .
       1101-MNOTE-H-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åý·×£Ã£Ó£ÖÌÀºÙ£Ä£Â¹¹¿·½èÍý
      *****************************************************************
       1102-MNOTE-B-HENSYU-SEC SECTION.
      * 
           MOVE    SPACE               TO  MNOTE-B-REC
           INITIALIZE                      MNOTE-B-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-B-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-B-TBL-KEY
           MOVE    SMNOTE-RENNUM       TO  MNOTE-B-RENNUM
           MOVE    SMNOTE-TBL-GROUP
                                       TO  MNOTE-B-TBL-GROUP
           MOVE    SMNOTE-TBL-UUID     TO  MNOTE-B-TBL-UUID
           MOVE    SMNOTE-SHORI-RENNUM
                                       TO  MNOTE-B-SHORI-RENNUM
           MOVE    SMNOTE-SHELLID      TO  MNOTE-B-SHELLID
           MOVE    SMNOTE-SRYYM        TO  MNOTE-B-SRYYM
           MOVE    SMNOTE-SRYYMD       TO  MNOTE-B-SRYYMD
           MOVE    MNOTE-H-CNT         TO  MNOTE-B-SEQ-NO
           MOVE    SMNOTE-CSVDATA      TO  MNOTE-B-CSVDATA
           MOVE    SMNOTE-TBL-GROUP(1:8)
                                       TO  MNOTE-B-CREYMD
      *
           MOVE    MNOTE-B-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_b"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC     =   ZERO )
               MOVE  MCP-RC            TO  SMNOTE-RETURN
           ELSE
               DISPLAY "MNOTE-B INSERT ERR KEY = ["
                                  MCP-RC  "]"
               MOVE  MCP-RC            TO  SMNOTE-RETURN
           END-IF
      *
           .
       1102-MNOTE-B-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åý·×£Ã£Ó£Ö£Ä£Â¹¹¿·½èÍý
      *****************************************************************
       120-UPD-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
      *    Åý·×£Ã£Ó£Ö´ÉÍý£Ä£Â¹¹¿·½èÍý
           PERFORM 900-MNOTE-H-KEY-SEL-SEC
           IF    ( FLG-MNOTE-H      =   ZERO )
               MOVE    "1"                 TO  MNOTE-H-PUTFLG
               MOVE    SMCNDATE-YMD        TO  MNOTE-H-UPYMD
               MOVE    SMCNDATE-HMS        TO  MNOTE-H-UPHMS
      *
               MOVE    MNOTE-H-REC         TO  MCPDATA-REC
               MOVE    "tbl_mnote_h"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC     =   ZERO )
                   MOVE  MCP-RC            TO  SMNOTE-RETURN
               ELSE
                   DISPLAY "MNOTE-H UPDATE ERR MCP-RC = ["
                           MCP-RC  " " MNOTE-H-KEY  "]"
                   MOVE  MCP-RC            TO  SMNOTE-RETURN
               END-IF
           ELSE
               DISPLAY "MNOTE-H UPDATE NOT FOUND KEY = ["
                       MNOTE-H-KEY  "]"
               MOVE  9                     TO  SMNOTE-RETURN
           END-IF
      *
           .
       120-UPD-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ºï½ü½èÍý
      *****************************************************************
       140-DEL-SEC                 SECTION.
      *
      *
           .
       140-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åý·×£Ã£Ó£Ö´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-MNOTE-H-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           MOVE    SPACE               TO  MNOTE-H-REC
           INITIALIZE                      MNOTE-H-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-H-TBL-KEY
           MOVE    SMNOTE-RENNUM       TO  MNOTE-H-RENNUM
           MOVE    SMNOTE-TBL-GROUP    TO  MNOTE-H-TBL-GROUP
           MOVE    SMNOTE-TBL-UUID     TO  MNOTE-H-TBL-UUID
           MOVE    SMNOTE-SHORI-RENNUM TO  MNOTE-H-SHORI-RENNUM
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MNOTE-H-REC
           ELSE
               MOVE    1               TO  FLG-MNOTE-H
           END-IF
      *
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-MNOTE-H-KEY-SEL-EXT.
           EXIT.
           .
      *
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
       910-DBDELETE-SEC                 SECTION.
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
      *    £Ä£Â£É£Î£Ó£Å£Ò£Ô½èÍý
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
