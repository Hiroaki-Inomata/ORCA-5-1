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
       PROGRAM-ID.             ORCBDELMNOTE.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¤ªÌô¼êÄ¢¥Ç¡¼¥¿´ÉÍý
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¤ªÌô¼êÄ¢¥Ç¡¼¥¿°ì³çºï½ü
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  12/12/17    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
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
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-MNOTE-H         PIC 9(01).
           03  FLG-MNOTE-B         PIC 9(01).
      *
      *    ºî¶ÈÎÎ°è
       01  WRK-AREA.
           03  WRK-MNOTE-DELYMD    PIC X(08).
           03  WRK-DELYMD          PIC X(08).
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASS-SAVE//.
      *
       01  CONST-AREA.
           03  CONST-MNOTE         PIC X(08)   VALUE "MNOTE".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSYSKANRI.INC".
      *
      *    ¤ªÌô¼êÄ¢³ÊÇ¼´ÉÍý
       01  MNOTE-H-REC.
           COPY    "CPMNOTE-H.INC".
      *
      *    ¤ªÌô¼êÄ¢³ÊÇ¼ÌÀºÙ
       01  MNOTE-B-REC.
           COPY    "CPMNOTE-B.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ¤ªÌô¼êÄ¢À©¸æ¥µ¥Ö
           COPY    "CPORCSMNOTE.INC".
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
           COPY    "CPCOMMAND-PARAM.INC".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM
           .
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           .
      *
       000-PROC-EXT.
           STOP    RUN.
      *
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       ORCSMNOTEAREA
                                       ASS-SAVE
      *
           UNSTRING    COMMAND-PARAM   DELIMITED   BY  ","
           INTO                        SPA-TERMID
                                       SPA-HOSPNUM
                                       SMNOTE-MODE
                                       SMNOTE-TBL-KEY
                                       SMNOTE-RENNUM
                                       SMNOTE-TBL-GROUP
                                       SMNOTE-SHORI-RENNUM
                                       SMNOTE-SHELLID
                                       SMNOTE-SRYYM
                                       SMNOTE-SRYYMD
                                       SMNOTE-PUTFLG
                                       SMNOTE-NYUGAIKBN
                                       SMNOTE-PTID
                                       SMNOTE-DRCD
                                       SMNOTE-TO-FOLDER
                                       SMNOTE-TO-DATA
                                       SMNOTE-CODE-TYPE
                                       SMNOTE-TITLE
                                       ASS-SAVE
                                       SMNOTE-TBL-UUID
                                       SMNOTE-GYOUMU-CD
                                       SMNOTE-LOCATION-CD
                                       SMNOTE-DATA-VERSION
                                       SPA-OPID
           END-UNSTRING
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SMCNDATE-YMD    TO  SPA-SRYYMD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    SMCNDATE-YMD    TO  LNK-DAY6-KIJUN
           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
           COMPUTE LNK-DAY6-ZOGEN
               =   3
               *   -1
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  WRK-MNOTE-DELYMD
      *
           PERFORM 900-MNOTE-H-KEY10-SEL-SEC
           IF    ( MNOTE-H-HOSPNUM     =   SPA-HOSPNUM )
            AND  ( MNOTE-H-TBL-KEY     =   SMNOTE-TBL-KEY )
            AND  ( MNOTE-H-RENNUM      =   SMNOTE-RENNUM )
            AND  ( MNOTE-H-TBL-GROUP   =   SMNOTE-TBL-GROUP )
            AND  ( MNOTE-H-TBL-UUID    =   SMNOTE-TBL-UUID )
            AND  ( MNOTE-H-SHORI-RENNUM =  SMNOTE-SHORI-RENNUM )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
               MOVE    WRK-MNOTE-DELYMD TO  WRK-DELYMD
               PERFORM 2001-MNOTE-DEL-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åý·×£Ã£Ó£Ö³ÊÇ¼¾ðÊóºï½ü½èÍý
      *****************************************************************
       2001-MNOTE-DEL-SEC          SECTION.
      *
           INITIALIZE                      MNOTE-H-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-H-TBL-KEY
           MOVE    WRK-DELYMD          TO  MNOTE-H-SRYYMD
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           INITIALIZE                      MNOTE-B-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-B-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-B-TBL-KEY
           MOVE    WRK-DELYMD          TO  MNOTE-B-SRYYMD
           MOVE    MNOTE-B-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_b"       TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-MNOTE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¸¡º÷½èÍý(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Åý·×£Ã£Ó£Ö³ÊÇ¼´ÉÍý¸¡º÷½èÍý
      *****************************************************************
       900-MNOTE-H-KEY10-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           INITIALIZE                      MNOTE-H-REC
      *
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-H-TBL-KEY
           MOVE    SPA-SRYYMD          TO  MNOTE-H-CREYMD
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MNOTE-H-REC
           ELSE
               MOVE    1               TO  FLG-MNOTE-H
               INITIALIZE                  MNOTE-H-REC
           END-IF
      *
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-MNOTE-H-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
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
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý(FETCH¤Ï¤·¤Ê¤¤)
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Ã£Ì£Ï£Ó£Å£Ã£Õ£Ò£Ó£Ï£Ò½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
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
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡ÀÚÃÇ½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
