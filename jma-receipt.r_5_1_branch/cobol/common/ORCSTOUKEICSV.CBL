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
       PROGRAM-ID.             ORCSTOUKEICSV.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Åý·×¥Ç¡¼¥¿£Ã£Ó£Ö´ÉÍý
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Åý·×¥Ç¡¼¥¿£Ã£Ó£ÖÀ©¸æ
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  09/09/17    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
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
           03  FLG-TOUKEICSV-H     PIC 9(01).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    Åý·×£Ã£Ó£Ö´ÉÍý£Ä£Â
       01  TOUKEICSV-H-REC.
           COPY    "CPTOUKEICSV-H.INC".
      *
      *    Åý·×£Ã£Ó£ÖÌÀºÙ£Ä£Â
       01  TOUKEICSV-B-REC.
           COPY    "CPTOUKEICSV-B.INC".          
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
      *   Åý·×£Ã£Ó£ÖÀ©¸æ¥µ¥Ö
           COPY    "CPORCSTOUKEICSV.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   ORCSTOUKEICSVAREA
                                   SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    ½èÍý¥â¡¼¥É¤Î¥Á¥§¥Ã¥¯
           EVALUATE    STOUKEICSV-MODE
               WHEN    "INS"
                       PERFORM 110-INS-SEC
               WHEN    "UPD"
                       PERFORM 120-UPD-SEC
               WHEN    "DEL"
                       PERFORM 140-DEL-SEC
               WHEN    OTHER
                       MOVE  10            TO  STOUKEICSV-RETURN  
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
           PERFORM 1101-TOUKEICSV-H-HENSYU-SEC
      *
           IF      MCP-RC          =   ZERO
               PERFORM 1102-TOUKEICSV-B-HENSYU-SEC
           END-IF         
      *
           IF      MCP-RC          =   ZERO
               CONTINUE
           ELSE
               MOVE  MCP-RC            TO  STOUKEICSV-RETURN
           END-IF
      *
           .
       110-INS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    Åý·×£Ã£Ó£Ö´ÉÍý£Ä£Â¹¹¿·½èÍý
      *****************************************************************
       1101-TOUKEICSV-H-HENSYU-SEC SECTION.
      * 
           MOVE    SPACE               TO  TOUKEICSV-H-REC
           INITIALIZE                      TOUKEICSV-H-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-H-HOSPNUM
           MOVE    STOUKEICSV-TBL-KEY  TO  TOUKEICSV-H-TBL-KEY
           MOVE    STOUKEICSV-RENNUM   TO  TOUKEICSV-H-RENNUM
           MOVE    STOUKEICSV-TBL-GROUP
                                       TO  TOUKEICSV-H-TBL-GROUP
           MOVE    STOUKEICSV-SHORI-RENNUM
                                       TO  TOUKEICSV-H-SHORI-RENNUM
           MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
           PERFORM 900-TOUKEICSV-H-INV-SEC
           IF      FLG-TOUKEICSV-H     =   ZERO
               ADD     1                   TO  TOUKEICSV-H-CNT
           ELSE
               MOVE    SPACE               TO  TOUKEICSV-H-REC
               INITIALIZE                      TOUKEICSV-H-REC
               MOVE    SPA-HOSPNUM         TO  TOUKEICSV-H-HOSPNUM
               MOVE    STOUKEICSV-TBL-KEY  TO  TOUKEICSV-H-TBL-KEY
               MOVE    STOUKEICSV-RENNUM   TO  TOUKEICSV-H-RENNUM
               MOVE    STOUKEICSV-TBL-GROUP
                                           TO  TOUKEICSV-H-TBL-GROUP
               MOVE    STOUKEICSV-SHORI-RENNUM
                                           TO  TOUKEICSV-H-SHORI-RENNUM
               MOVE    STOUKEICSV-SHELLID  TO  TOUKEICSV-H-SHELLID
               MOVE    STOUKEICSV-SRYYM    TO  TOUKEICSV-H-SRYYM
               MOVE    STOUKEICSV-SRYYMD   TO  TOUKEICSV-H-SRYYMD
               MOVE    1                   TO  TOUKEICSV-H-CNT
               IF      STOUKEICSV-PUTFLG   =   "1"
                   MOVE    "1"                 TO  TOUKEICSV-H-PUTFLG
               ELSE
                   MOVE    SPACE               TO  TOUKEICSV-H-PUTFLG
               END-IF
               MOVE    STOUKEICSV-NYUGAIKBN
                                           TO  TOUKEICSV-H-NYUGAIKBN
               MOVE    STOUKEICSV-TO-FOLDER
                                           TO  TOUKEICSV-H-TO-FOLDER
               MOVE    STOUKEICSV-TO-DATA  TO  TOUKEICSV-H-TO-DATA
               MOVE    STOUKEICSV-CODE-TYPE
                                           TO  TOUKEICSV-H-CODE-TYPE
               MOVE    STOUKEICSV-TITLE    TO  TOUKEICSV-H-TITLE
           END-IF
      *
           MOVE    STOUKEICSV-TBL-GROUP(1:8)
                                       TO  TOUKEICSV-H-CREYMD
      *
           MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-TOUKEICSV-H     =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCP-RC              TO  STOUKEICSV-RETURN
               ELSE
                   DISPLAY "TOUKEICSV-H UPDATE ERR KEY = ["
                                      MCP-RC  " " TOUKEICSV-H-KEY  "]" 
                   MOVE  MCP-RC                TO  STOUKEICSV-RETURN
               END-IF
           ELSE
               MOVE    "DBINSERT"              TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE  MCP-RC                TO  STOUKEICSV-RETURN
               ELSE
                   DISPLAY "TOUKEICSV-H INSERT ERR KEY = ["
                                      MCP-RC  " " TOUKEICSV-H-KEY  "]"   
                   MOVE  MCP-RC                TO  STOUKEICSV-RETURN
               END-IF
           END-IF    
      *
           .
       1101-TOUKEICSV-H-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åý·×£Ã£Ó£ÖÌÀºÙ£Ä£Â¹¹¿·½èÍý
      *****************************************************************
       1102-TOUKEICSV-B-HENSYU-SEC SECTION.
      * 
           MOVE    SPACE               TO  TOUKEICSV-B-REC
           INITIALIZE                      TOUKEICSV-B-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-B-HOSPNUM
           MOVE    STOUKEICSV-TBL-KEY  TO  TOUKEICSV-B-TBL-KEY
           MOVE    STOUKEICSV-RENNUM   TO  TOUKEICSV-B-RENNUM
           MOVE    STOUKEICSV-TBL-GROUP
                                       TO  TOUKEICSV-B-TBL-GROUP
           MOVE    STOUKEICSV-SHORI-RENNUM
                                       TO  TOUKEICSV-B-SHORI-RENNUM
           MOVE    STOUKEICSV-SHELLID  TO  TOUKEICSV-B-SHELLID
           MOVE    STOUKEICSV-SRYYM    TO  TOUKEICSV-B-SRYYM
           MOVE    STOUKEICSV-SRYYMD   TO  TOUKEICSV-B-SRYYMD
           MOVE    STOUKEICSV-PTID     TO  TOUKEICSV-B-PTID 
           MOVE    TOUKEICSV-H-CNT     TO  TOUKEICSV-B-SEQ-NO
           MOVE    STOUKEICSV-CSVDATA  TO  TOUKEICSV-B-CSVDATA
           MOVE    STOUKEICSV-TBL-GROUP(1:8)
                                       TO  TOUKEICSV-B-CREYMD
      *
           MOVE    TOUKEICSV-B-REC     TO  MCPDATA-REC
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *                                      
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  STOUKEICSV-RETURN
           ELSE
               DISPLAY "TOUKEICSV-B INSERT ERR KEY = ["
      ****                        MCP-RC  " " TOUKEICSV-B-KEY  "]"   
                                  MCP-RC  " "    
               MOVE  MCP-RC            TO  STOUKEICSV-RETURN
           END-IF
      *
           .
       1102-TOUKEICSV-B-HENSYU-EXT.
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
           MOVE    SPACE               TO  TOUKEICSV-H-REC
           INITIALIZE                      TOUKEICSV-H-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-H-HOSPNUM
           MOVE    STOUKEICSV-TBL-KEY  TO  TOUKEICSV-H-TBL-KEY
           MOVE    STOUKEICSV-RENNUM   TO  TOUKEICSV-H-RENNUM
           MOVE    STOUKEICSV-TBL-GROUP
                                       TO  TOUKEICSV-H-TBL-GROUP
           MOVE    STOUKEICSV-SHORI-RENNUM
                                       TO  TOUKEICSV-H-SHORI-RENNUM
           MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
           PERFORM 900-TOUKEICSV-H-INV-SEC
           IF      FLG-TOUKEICSV-H     =   ZERO
               MOVE    "1"                 TO  TOUKEICSV-H-PUTFLG
               MOVE    SMCNDATE-YMD        TO  TOUKEICSV-H-UPYMD
               MOVE    SMCNDATE-HMS        TO  TOUKEICSV-H-UPHMS
      *
               MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
               MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  STOUKEICSV-RETURN
               ELSE
                   DISPLAY "TOUKEICSV-H UPDATE ERR MCP-RC = ["
                                  MCP-RC  " " TOUKEICSV-H-KEY  "]"
                   MOVE  MCP-RC            TO  STOUKEICSV-RETURN
                   GO  TO  120-UPD-EXT
               END-IF
           ELSE
               DISPLAY "TOUKEICSV-H UPDATE NOT FOUND KEY = ["
                                           TOUKEICSV-H-KEY  "]"
               MOVE  9                TO  STOUKEICSV-RETURN
               GO  TO  120-UPD-EXT
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
       900-TOUKEICSV-H-INV-SEC         SECTION.
      *
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TOUKEICSV-H-REC
                   MOVE    ZERO                TO  FLG-TOUKEICSV-H
               ELSE
                   MOVE    1                   TO  FLG-TOUKEICSV-H
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TOUKEICSV-H
           END-IF
      *
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TOUKEICSV-H-INV-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
