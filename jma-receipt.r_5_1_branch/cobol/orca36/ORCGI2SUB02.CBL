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
       PROGRAM-ID.             ORCGI2SUB02.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡´µ¼Ô¾È²ñ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¸¡º÷½èÍý
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-TEIKIRRK        PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-YMD                             PIC X(10).
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-KEISANYMD                       PIC X(08).
           03  WRK-DELYMDHMS.
               05  WRK-DELYMDHMS-YMD               PIC X(08).
               05  WRK-DELYMDHMS-HMS               PIC X(06).
           03  WRK-STYMD                           PIC X(08).
           03  WRK-EDYMD                           PIC X(08).
           03  WRK-SYMD                            PIC X(08).
           03  WRK-NAME                            PIC X(200).
           03  WRK-NAME-G.
               05  WRK-NAME1                       PIC X(100).
               05  WRK-NAME2                       PIC X(100).
           03  WRK-SYSYMD                          PIC 9(08).
           03  WRK-STBIRTHDAY-G.
               05  WRK-STBIRTHDAY                  PIC 9(08).
           03  WRK-EDBIRTHDAY-G.
               05  WRK-EDBIRTHDAY                  PIC 9(08).
           03  WRK-SRYYM                           PIC X(06).
           03  WRK-KBT                             PIC X(01).
           03  WRK-ADRS                            PIC X(150).
           03  WRK-ORDER-TEXT                      PIC X(200).
           03  WRK-ORDER-PNT                       PIC 9(05).
           03  WRK-ITEM-NAME                       PIC X(50).
           03  WRK-SORT-NAME                       PIC X(05).
           03  WRK-SQL                             PIC X(15000).
           03  WRK-SQL-PNT                         PIC 9(05).
           03  WRK-ORCSSTR-NUM1                    PIC 9(03).
           03  WRK-TERMID                          PIC X(64).
      *
       01  CONST-AREA.
           03  CONST-ORDER-G.
               05  FILLER              PIC X(13) VALUE
                                           ",BRMNUM   ASC".
               05  FILLER              PIC X(13) VALUE
                                           ",KANANAME ASC".
               05  FILLER              PIC X(13) VALUE
                                           ",PTNUM    ASC".
               05  FILLER              PIC X(13) VALUE
                                           ",NYUINYMD ASC".
               05  FILLER              PIC X(13) VALUE
                                           ",TAIINYMD ASC".
               05  FILLER              PIC X(13) VALUE
                                           ",RRKNUM   ASC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    £Á£Ð£É¥Ñ¥é¥á¥¿
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ´µ¼ÔÄê´üÀÁµáÍúÎò
       01  TEIKIRRK-REC.
           COPY    "CPTEIKIRRK.INC".
      *
      *    ´µ¼Ô¾ðÊó¾È²ñ¥Æ¡¼¥Ö¥ë
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ÆüÉÕÊÔ½¸¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    Ê¸»úÎóÁàºî¥µ¥Ö
           COPY    "CPORCSSTRING.INC".
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPI2SUB02.INC".
      *
       01  I001-REC.
           COPY    "CPI001.INC".
      *
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                       DIVISION    USING
           I2SUB02-AREA
           I001-REC
           SPA-AREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( I2SUB02-RC          =   ZERO )
               IF    ( I2SUB02-MODE    =   "O" )
                   PERFORM 200-MAIN-SEC
               ELSE
                   PERFORM 201-MAIN-SEC
               END-IF
           END-IF
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           IF    ( I2SUB02-MODE        =   SPACE )
               MOVE    "O"             TO  I2SUB02-MODE
           END-IF
      *
           IF    ( I2SUB02-BASEYMD     =   SPACE )
               MOVE    SPA-SYSYMD      TO  I2SUB02-BASEYMD
           END-IF
      *
           IF    ( I2SUB02-ORCA-UID    =   SPACE )
               MOVE    SPA-TERMID      TO  WRK-TERMID
           ELSE
               MOVE    I2SUB02-ORCA-UID TO WRK-TERMID
           END-IF
      *
           IF    ( I2SUB02-SEIKYUJOTAI =   "2" )
               PERFORM 900-TEIKIRRK-KEY2-SEL-SEC
               MOVE    TEIKIRRK-SRYYM  TO  I2SUB02-LAST-TEIKIYM
               MOVE    TEIKIRRK-KBT    TO  I2SUB02-LAST-KBT
           END-IF
      *
           IF    ( I2SUB02-MODE        =   "O" )
      *
               MOVE    I2SUB02-SELYMD  TO  WRK-YMD
               COMPUTE WRK-ZOGEN   =   -1
               MOVE    "1"             TO  WRK-ZOGENPTN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-DELYMDHMS-YMD
               MOVE    I2SUB02-SELHMS  TO  WRK-DELYMDHMS-HMS
               PERFORM 900-PTSRH-DEL4-SEC
               PERFORM 900-APIPARA-DEL5-SEC
      *
               INITIALIZE                  PTSRH-REC
               MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
               MOVE    WRK-TERMID      TO  PTSRH-TERMID
               MOVE    "1"             TO  PTSRH-DATAKBN
               MOVE    PTSRH-REC       TO  MCPDATA-REC
               MOVE    "tbl_ptsrh"     TO  MCP-TABLE
               MOVE    "del1"          TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    1           TO  I2SUB02-RC
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    ´µ¼Ô»áÌ¾
           IF    ( I2SUB02-NAME        NOT =   SPACE )
               MOVE    I2SUB02-NAME        TO  WRK-NAME
               PERFORM 2001-EDIT-NAME-SEC
           END-IF
      *
           IF    ( I2SUB02-API-FLG     =   ZERO )
      *        Ç¯Îð¤è¤êÀ¸Ç¯·îÆü¤ò·×»»
               MOVE    I2SUB02-BASEYMD     TO  WRK-SYSYMD
               COMPUTE WRK-EDBIRTHDAY
                   =   WRK-SYSYMD
                   -   I2SUB02-STAGE   *   10000
      *
               IF    ( I2SUB02-EDAGE           =   999 )
                   MOVE    ZERO                TO  WRK-STBIRTHDAY
               ELSE
                   COMPUTE WRK-STBIRTHDAY      =   WRK-SYSYMD
                                               - ( I2SUB02-EDAGE + 1 )
                                               *   10000
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-STBIRTHDAY      TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"               USING
                                                   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO  WRK-STBIRTHDAY
               END-IF
               MOVE    WRK-STBIRTHDAY-G        TO  I2SUB02-STBIRTHDAY
               MOVE    WRK-EDBIRTHDAY-G        TO  I2SUB02-EDBIRTHDAY
           END-IF
      *
      *    ½»½ê
           IF    ( I2SUB02-ADRS        NOT =   SPACE )
               MOVE    SPACE               TO  WRK-ADRS
               STRING  FUNCTION TRIM(I2SUB02-ADRS TRAILING)
                                                   DELIMITED BY SIZE
                       "%"                         DELIMITED BY SIZE
               INTO    WRK-ADRS
               END-STRING
               INSPECT WRK-ADRS
                       REPLACING   ALL "¡ö" BY  "%%"
           END-IF
      *
      *    £Ó£Ñ£ÌºîÀ®
           INITIALIZE                  WRK-SQL
                                       WRK-ORDER-TEXT
           MOVE    1               TO  WRK-SQL-PNT
                                       WRK-ORDER-PNT
      *
           STRING  " TRUE"         DELIMITED BY SIZE
           INTO    WRK-ORDER-TEXT
           WITH    POINTER   WRK-ORDER-PNT
           END-STRING
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               IF    ( I2SUB02-SORT-CD (IDX1)  NOT =   SPACE )
                   PERFORM 2001-EDIT-ORDER-SEC
               END-IF
      *
           END-PERFORM
      *
           STRING ",BRMNUM   ASC"              DELIMITED BY SIZE
                  ",KANANAME ASC"              DELIMITED BY SIZE
                  ",PTNUM    ASC"              DELIMITED BY SIZE
                  ",NYUINYMD ASC"              DELIMITED BY SIZE
                  ",TAIINYMD ASC"              DELIMITED BY SIZE
                  ",RRKNUM   ASC"              DELIMITED BY SIZE
           INTO    WRK-ORDER-TEXT
           WITH    POINTER   WRK-ORDER-PNT
           END-STRING
      *
           STRING  "INSERT INTO TBL_PTSRH ("       DELIMITED BY SIZE
                   "HOSPNUM,"                      DELIMITED BY SIZE
                   "TERMID,"                       DELIMITED BY SIZE
                   "PTID,"                         DELIMITED BY SIZE
                   "DATAKBN,"                      DELIMITED BY SIZE
                   "MEMO,"                         DELIMITED BY SIZE
                   "SEQNUM,"                       DELIMITED BY SIZE
                   "CREYMD,"                       DELIMITED BY SIZE
                   "UPYMD,"                        DELIMITED BY SIZE
                   "UPHMS"                         DELIMITED BY SIZE
                   ")SELECT "                      DELIMITED BY SIZE
                   "HOSPNUM,'"                     DELIMITED BY SIZE
                   FUNCTION TRIM(WRK-TERMID TRAILING)
                                                   DELIMITED BY SIZE
                   "',"                            DELIMITED BY SIZE
                   "PTID,"                         DELIMITED BY SIZE
                   "'1',"                          DELIMITED BY SIZE
                   "HOSPNUM::text||','||"          DELIMITED BY SIZE
                   "PTID::text||','||"             DELIMITED BY SIZE
                   "RRKNUM::text||','||"           DELIMITED BY SIZE
                   "RRKEDANUM::text||','||"        DELIMITED BY SIZE
                   "RRKEDANUM_MAX::text||','||"    DELIMITED BY SIZE
                   "PTNUM||','||"                  DELIMITED BY SIZE
                   "KANANAME||','||"               DELIMITED BY SIZE
                   "NAME||','||"                   DELIMITED BY SIZE
                   "SEX||','||"                    DELIMITED BY SIZE
                   "BIRTHDAY||','||"               DELIMITED BY SIZE
                   "HOME_POST||','||"              DELIMITED BY SIZE
                   "HOME_ADRS||','||"              DELIMITED BY SIZE
                   "HOME_BANTI||','||"             DELIMITED BY SIZE
                   "DEATHKBN||','||"               DELIMITED BY SIZE
                   "BTUNUM||','||"                 DELIMITED BY SIZE
                   "BRMNUM||','||"                 DELIMITED BY SIZE
                   "NYUINKA||','||"                DELIMITED BY SIZE
                   "NYUINYMD||','||"               DELIMITED BY SIZE
                   "TAIINYMD||','||"               DELIMITED BY SIZE
                   "TAIINCD||','||"                DELIMITED BY SIZE
                   "TENNYUYMD||','||"              DELIMITED BY SIZE
                   "TENSTUYMD||','||"              DELIMITED BY SIZE
                   "TSTPTNUMKBN,"                  DELIMITED BY SIZE
                   "ROW_NUMBER() OVER(ORDER BY "   DELIMITED BY SIZE
                   FUNCTION TRIM(WRK-ORDER-TEXT TRAILING)
                                                   DELIMITED BY SIZE
                   "),'"                           DELIMITED BY SIZE
                   I2SUB02-SELYMD                  DELIMITED BY SIZE
                   "','"                           DELIMITED BY SIZE
                   I2SUB02-SELYMD                  DELIMITED BY SIZE
                   "','"                           DELIMITED BY SIZE
                   I2SUB02-SELHMS                  DELIMITED BY SIZE
                   "' "                            DELIMITED BY SIZE
                   "FROM VIEW_I001 WHERE 1=1 "     DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( I2SUB02-JOTAI   =   "3" )
               STRING  " AND (HOSPNUM,PTID,RRKNUM ) IN ("
                                                   DELIMITED BY SIZE
                       "SELECT HOSPNUM,PTID,RRKNUM FROM "
                                                   DELIMITED BY SIZE
                       "view_I001 WHERE 1=1 "      DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  " AND HOSPNUM = "               DELIMITED BY SIZE
                   SPA-HOSPNUM                     DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( I2SUB02-PTID        >   ZERO )
      *
               STRING  " AND PTID = "              DELIMITED BY SIZE
                       I2SUB02-PTID                DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
      *
      *        ´µ¼Ô»áÌ¾¡ÊÁ°Êý°ìÃ×¡Ë
               IF    ( I2SUB02-NAME        NOT =   SPACE )
                   STRING  " AND (( NAME LIKE '"   DELIMITED BY SIZE
                           WRK-NAME                DELIMITED BY SPACE
                           "')OR( KANANAME LIKE '" DELIMITED BY SIZE
                           WRK-NAME                DELIMITED BY SPACE
                           "'))"                   DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
      *        Ç¯Îð
               IF    ( I2SUB02-STBIRTHDAY NOT =    SPACE )
                   STRING  " AND  BIRTHDAY >= '"   DELIMITED BY SIZE
                           I2SUB02-STBIRTHDAY      DELIMITED BY SIZE
                           "'"                     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
               IF    ( I2SUB02-EDBIRTHDAY NOT =    SPACE )
                   STRING  " AND  BIRTHDAY <= '"   DELIMITED BY SIZE
                           I2SUB02-EDBIRTHDAY      DELIMITED BY SIZE
                           "'"                     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
      *        ½»½ê¡ÊÁ°Êý°ìÃ×¡Ë
               IF    ( I2SUB02-ADRS        NOT =   SPACE )
                   STRING  " AND HOME_ADRS || HOME_BANTI LIKE '"
                                                   DELIMITED BY SIZE
                           FUNCTION TRIM(WRK-ADRS TRAILING)
                                                   DELIMITED BY SIZE
                           "'"                     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-IF
      *
      *    Æþ±¡Æü
           IF    ( I2SUB02-STNYUINYMD      NOT =   SPACE )
               STRING  " AND NYUINYMD >= '"        DELIMITED BY SIZE
                       I2SUB02-STNYUINYMD          DELIMITED BY SIZE
                       "'"                         DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
           IF    ( I2SUB02-EDNYUINYMD      NOT =   SPACE )
               STRING  " AND NYUINYMD <= '"        DELIMITED BY SIZE
                       I2SUB02-EDNYUINYMD          DELIMITED BY SIZE
                       "'"                         DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
      *    Âà±¡Æü
           IF    ( I2SUB02-API-FLG     =   1 )
               IF    ( I2SUB02-STTAIINYMD  NOT =  SPACE )
                   STRING  " AND  TAIINYMD <> '99999999'"
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-IF
           IF    ( I2SUB02-STTAIINYMD      NOT =   SPACE )
               STRING  " AND TAIINYMD >= '"        DELIMITED BY SIZE
                       I2SUB02-STTAIINYMD          DELIMITED BY SIZE
                       "'"                         DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
           IF    ( I2SUB02-EDTAIINYMD      NOT =   SPACE )
               STRING  " AND TAIINYMD <= '"        DELIMITED BY SIZE
                       I2SUB02-EDTAIINYMD          DELIMITED BY SIZE
                       "'"                         DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
      *    ¿ÇÎÅ²Ê
           IF    ( I2SUB02-SRYKA       =   SPACE )
               CONTINUE
           ELSE
               STRING  " AND NYUINKA = '"          DELIMITED BY SIZE
                       I2SUB02-SRYKA               DELIMITED BY SIZE
                       "'"                         DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
      *    ÉÂÅïÈÖ¹æ
           IF    ( I2SUB02-BTUNUM      =   SPACE )
               CONTINUE
           ELSE
               STRING  " AND BTUNUM = '"           DELIMITED BY SIZE
                       I2SUB02-BTUNUM              DELIMITED BY SIZE
                       "'"                         DELIMITED BY SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
      *    ¾õÂÖ
           EVALUATE    I2SUB02-JOTAI
           WHEN    "1"
               STRING  " AND TAIINYMD = '99999999'"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  " AND TAIINYMD <> '99999999'"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
      *
               MOVE    I2SUB02-BASEYMD  TO  WRK-STYMD
               MOVE    "01"             TO  WRK-STYMD (7:)
      *
               MOVE    I2SUB02-BASEYMD  TO  WRK-SYMD
               PERFORM 800-EOM-SEC
               MOVE    WRK-SYMD    TO  WRK-EDYMD
      *
               STRING  " AND (TENNYUYMD <= '"  DELIMITED BY SIZE
                       WRK-EDYMD               DELIMITED BY SIZE
                       "' AND TENSTUYMD >= '"  DELIMITED BY SIZE
                       WRK-STYMD               DELIMITED BY SIZE
                       "')"                    DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "4"
      *
               STRING  " AND (TENNYUYMD <= '"  DELIMITED BY SIZE
                       I2SUB02-BASEYMD         DELIMITED BY SIZE
                       "' AND TENSTUYMD >= '"  DELIMITED BY SIZE
                       I2SUB02-BASEYMD         DELIMITED BY SIZE
                       "')"                    DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN        "5"
               STRING  " AND (HOSPNUM,PTID,RRKNUM) IN"
                                               DELIMITED BY SIZE
                       "(SELECT HOSPNUM,PTID,RRKNUM FROM tbl_nrrksrh"
                                               DELIMITED BY SIZE
                       " WHERE  (HOSPNUM,PTID,ZAINUM) IN "
                                               DELIMITED BY SIZE
                       "(SELECT HOSPNUM,PTID,ZAINUM FROM tbl_nsrysrh"
                                               DELIMITED BY SIZE
                       " WHERE  SRYCD = '099999914' "
                                               DELIMITED BY SIZE
                       " AND HKNCOMBINUM = '0999' ))"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
      *    ¾õÂÖ
           EVALUATE    I2SUB02-JOTAI
           WHEN        "3"
               MOVE    I2SUB02-BASEYMD TO  WRK-STYMD
               MOVE    "01"            TO  WRK-STYMD (7:)
               STRING  " AND ((TENSTUYMD <> '" DELIMITED BY SIZE
                       WRK-STYMD               DELIMITED BY SIZE
                       "') AND (TENNYUYMD <> TENSTUYMD)"
                                               DELIMITED BY SIZE
                       "OR RRKEDANUM = RRKEDANUM_MAX)"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN        "4"
               STRING  " AND (TENSTUYMD <> '"  DELIMITED BY SIZE
                       I2SUB02-BASEYMD         DELIMITED BY SIZE
                       "' OR RRKEDANUM = RRKEDANUM_MAX)"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    OTHER
               STRING  " AND RRKEDANUM = RRKEDANUM_MAX"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
      *    ÀÁµá¾õÂÖ
           EVALUATE    I2SUB02-SEIKYUJOTAI
           WHEN    "1"
               PERFORM 2001-SQL-SYUNOU-SEC
           WHEN    "2"
               MOVE    TEIKIRRK-SRYYM  TO  WRK-SRYYM
               MOVE    TEIKIRRK-KBT    TO  WRK-KBT
               PERFORM 2001-SQL-PTTEIKIRRK-SEC
           END-EVALUATE
      *
           IF    ( I2SUB02-TEIKIYM     =   SPACE )
               CONTINUE
           ELSE
               MOVE    I2SUB02-TEIKIYM TO  WRK-SRYYM
               MOVE    I2SUB02-KBT     TO  WRK-KBT
               PERFORM 2001-SQL-PTTEIKIRRK-SEC
           END-IF
      *
      *    Âà±¡»öÍ³
           IF    ( I2SUB02-TAIINCD     =   SPACE )
               CONTINUE
           ELSE    
               STRING  " AND TAIINCD = '"      DELIMITED BY SIZE
                       I2SUB02-TAIINCD         DELIMITED BY SIZE
                       "'"                     DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
      *    »àË´¶èÊ¬
           IF      I2SUB02-DEATHKBN     =   SPACE
               CONTINUE
           ELSE
               IF    ( I2SUB02-DEATHKBN =   "0" )
                   STRING  " AND DEATHKBN <> '1'"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  " AND DEATHKBN = '1'"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-IF
      *
      *    ¥Æ¥¹¥È´µ¼Ô
           EVALUATE    I2SUB02-TSTPT
           WHEN    "a"
               STRING  " AND TSTPTNUMKBN <> '1'"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "b"
               CONTINUE
           WHEN    "1"
               STRING  " AND TSTPTNUMKBN =  '1'"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           IF    ( I2SUB02-JOTAI = "3" )
               STRING  ") AND RRKEDANUM = RRKEDANUM_MAX"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING " ORDER BY "                 DELIMITED BY SIZE
                   FUNCTION TRIM(WRK-ORDER-TEXT TRAILING)
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           INITIALIZE                      PTSRH-REC
           MOVE    WRK-SQL             TO  PTSRH-SQL
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "ins1"              TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    2               TO  I2SUB02-RC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       2001-SQL-SYUNOU-SEC             SECTION.
      *
           STRING    " AND (HOSPNUM,'1',PTID,RRKNUM) IN ("
                                           DELIMITED BY SIZE
               "SELECT a.HOSPNUM,a.NYUGAIKBN,a.PTID,b.NYUIN_RRKNUM"
                                           DELIMITED BY SIZE
               " FROM TBL_SYUNOU_MAIN AS a JOIN TBL_SYUNOU_NYUIN"
                                           DELIMITED BY SIZE
               " AS b USING(HOSPNUM, NYUGAIKBN, PTID,DENPNUM)"
                                           DELIMITED BY SIZE
               " WHERE a.CREATEKBN='1' AND a.DENPJTIKBN  <>  '3')"
                                           DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       2001-SQL-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÄê´üÀÁµáÍúÎò¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       2001-SQL-PTTEIKIRRK-SEC         SECTION.
      *
           STRING    " AND (HOSPNUM,PTID,RRKNUM) IN ("
                                           DELIMITED BY SIZE
               "SELECT HOSPNUM,PTID,RRKNUM"
                                           DELIMITED BY SIZE
               " FROM TBL_PTTEIKIRRK WHERE "
                                           DELIMITED BY SIZE
               " SRYYM = '"                DELIMITED BY SIZE
               WRK-SRYYM                   DELIMITED BY SIZE
               "'"                         DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( WRK-KBT         NOT =   SPACE )
               STRING    " AND KBT ='"     DELIMITED BY SIZE
                   WRK-KBT                 DELIMITED BY SIZE
                   "'"                     DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  " AND DELFLG=0)"        DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       2001-SQL-PTTEIKIRRK-EXT.
           EXIT.
      *****************************************************************
      *    »áÌ¾¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       2001-EDIT-NAME-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME-G
      *
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "¡ö¡¡"              TO  ORCSSTR-STRING2
           MOVE    "¡ö"                TO  ORCSSTR-STRING3
           CALL    "orcsstring" USING  ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  WRK-NAME
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "¡ö"                TO  ORCSSTR-STRING2
           CALL    "orcsstring" USING  ORCSSTRINGAREA
           MOVE    ORCSSTR-NUM1        TO  WRK-ORCSSTR-NUM1
           IF    ( ORCSSTR-NUM1        >   ZERO )
               IF    ( ORCSSTR-NUM1    =   1 )
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM1    =   2
                   COMPUTE ORCSSTR-NUM2    =   0
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME2
               ELSE
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM2    =   ORCSSTR-NUM1    -   1
                   COMPUTE ORCSSTR-NUM1    =   1
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME1
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM1    =   WRK-ORCSSTR-NUM1
                                           +   1
                   COMPUTE ORCSSTR-NUM2    =   0
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME2
               END-IF
           ELSE
               MOVE    WRK-NAME        TO  WRK-NAME1
           END-IF
      *
           MOVE    SPACE           TO  WRK-NAME
           IF    ( WRK-NAME1       NOT =   SPACE )
            AND  ( WRK-NAME2           =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
           IF    ( WRK-NAME1       NOT =   SPACE )
            AND  ( WRK-NAME2       NOT =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                       "¡¡"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
           IF    ( WRK-NAME1           =   SPACE )
            AND  ( WRK-NAME2       NOT =   SPACE )
               STRING  "%"             DELIMITED  BY  SIZE
                       "¡¡"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
      *
           .
       2001-EDIT-NAME-EXT.
           EXIT.
      *****************************************************************
      *    ÊÂ¤Ó½çÊÔ½¸½èÍý
      *****************************************************************
       2001-EDIT-ORDER-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ITEM-NAME
           MOVE    SPACE               TO  WRK-SORT-NAME
      *
           EVALUATE    I2SUB02-SORT-CD (IDX1)
           WHEN    "1"
               MOVE    "BTUNUM ASC,SUBSTR(BRMNUM,3,6) ASC"
                                       TO  WRK-ITEM-NAME
           WHEN    "2"
               MOVE    "PTNUM"         TO  WRK-ITEM-NAME
           WHEN    "3"
              MOVE     "KANANAME"      TO  WRK-ITEM-NAME
           WHEN    "4"
           WHEN    "5"
              MOVE     "NYUINYMD"      TO  WRK-ITEM-NAME
           WHEN    "6"
           WHEN    "7"
              MOVE     "TAIINYMD"      TO  WRK-ITEM-NAME
           END-EVALUATE
      *
           IF    ( WRK-ITEM-NAME       NOT =   SPACE )
      *
               IF    ( I2SUB02-SORT-CD (IDX1)      =   "1" )
                   IF    ( I2SUB02-SORT-ORDER (IDX1)   =   "2" )
                       MOVE    "BTUNUM DESC,SUBSTR(BRMNUM,3,6) DESC"
                                       TO  WRK-ITEM-NAME
                   END-IF
               ELSE
                   IF    ( I2SUB02-SORT-ORDER (IDX1)   =   "2" )
                       MOVE    "DESC"      TO  WRK-SORT-NAME
                   ELSE
                       MOVE    "ASC"       TO  WRK-SORT-NAME
                   END-IF
               END-IF
      *
               STRING  ","                 DELIMITED BY SIZE
                       FUNCTION TRIM(WRK-ITEM-NAME TRAILING)
                                           DELIMITED BY SIZE
                       " "                 DELIMITED BY SIZE
                       WRK-SORT-NAME       DELIMITED BY SPACE
               INTO    WRK-ORDER-TEXT
               WITH    POINTER   WRK-ORDER-PNT
               END-STRING
      *
           END-IF
      *
           .
       2001-EDIT-ORDER-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       201-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  I001-REC
           UNSTRING  I2SUB02-MEMO      DELIMITED BY ","
           INTO                        I001-HOSPNUM
                                       I001-PTID
                                       I001-RRKNUM
                                       I001-RRKEDANUM
                                       I001-RRKEDANUM-MAX
                                       I001-PTNUM
                                       I001-KANANAME
                                       I001-NAME
                                       I001-SEX
                                       I001-BIRTHDAY
                                       I001-HOME-POST
                                       I001-HOME-ADRS
                                       I001-HOME-BANTI
                                       I001-DEATHKBN
                                       I001-BTUNUM
                                       I001-BRMNUM
                                       I001-NYUINKA
                                       I001-NYUINYMD
                                       I001-TAIINYMD
                                       I001-TAIINCD
                                       I001-TENNYUYMD
                                       I001-TENSTUYMD
                                       I001-TSTPTNUMKBN
           END-UNSTRING
      *
           .
       201-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                    SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ·îËöÆüÊÔ½¸½èÍý
      *****************************************************************
       800-EOM-SEC                     SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-EOM-EXT.
           EXIT.
      *****************************************************************
      *    ¥«¥ì¥ó¥À¡¼½èÍý
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¾È²ñºï½ü½èÍý
      *****************************************************************
       900-PTSRH-DEL4-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    WRK-TERMID      TO  PTSRH-TERMID
           MOVE    WRK-DELYMDHMS   TO  PTSRH-VSTR (1)
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del4"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL4-EXT.
           EXIT.
      *****************************************************************
      *    Äê´üÀÁµáÍúÎò¸¡º÷¡ÊKEY2¡Ë
      *****************************************************************
       900-TEIKIRRK-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIRRK
           INITIALIZE                      TEIKIRRK-REC
      *
           MOVE    SPA-HOSPNUM         TO  TEIKIRRK-HOSPNUM
           MOVE    TEIKIRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikirrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-TEIKIRRK
               INITIALIZE                  TEIKIRRK-REC
           END-IF
      *
           MOVE    "tbl_teikirrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TEIKIRRK-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    £Á£Ð£É¥Ñ¥é¥á¥¿ºï½ü½èÍý
      *****************************************************************
       900-APIPARA-DEL5-SEC              SECTION.
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api036"        TO  APIPARA-GYOUMUID
           MOVE    WRK-DELYMDHMS   TO  APIPARA-DATA-REC
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "del5"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-APIPARA-DEL5-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    £Ä£Â£É£Î£Ó£Å£Ò£Ô½èÍý
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
