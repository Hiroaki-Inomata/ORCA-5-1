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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQSUB02.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ´µ¼Ô¾È²ñ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ´µ¼Ô¾È²ñ¥µ¥Ö¡Ê¸¡º÷£Ó£Ñ£ÌºîÀ®¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/06/18    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     NACL-ÂÀÅÄ    04/06/21  ºÇ½ª¼õ¿ÇÆü¸¡º÷¾ò·ï½¤Àµ
      * 03.05.00     NACL-ÂÀÅÄ    07.05.23  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-JOKEN                          PIC 9(01).
           03  FLG-JOKEN-AND                      PIC 9(01).
           03  FLG-JOKEN-OR                       PIC 9(01).
           03  FLG-PTBYOMEI-SQL                    PIC 9(01).
           03  FLG-TMPVQ001-SQL                    PIC 9(01).
           03  FLG-TBLNAME                         PIC 9(01).
           03  FLG-RECECOM-SQL                     PIC 9(01).
           03  FLG-HKNSELPTN                       PIC 9(01).
           03  FLG-UNION                           PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXS                                PIC 9(05).
           03  IDX-LEN                             PIC 9(05).
           03  IDX-SITM1                            PIC 9(05).
           03  IDX-SITM2                            PIC 9(05).
           03  IDX-SITM3                            PIC 9(05).
           03  IDX-SRT1                            PIC 9(05).
           03  IDX-SRT2                            PIC 9(05).
           03  IDX-SRT3                            PIC 9(05).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-JOKEN                          PIC 9(05).
           03  CNT-STR                             PIC 9(05).
           03  CNT-RECECOM                         PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-KIGO-LEN                        PIC 9(05).
           03  WRK-NUM-LEN                         PIC 9(05).
           03  WRK-STR                             PIC X(20000).
           03  WRK-STR-LEN                         PIC 9(05).
           03  WRK-SQL-TBL-NAME                    PIC X(30).
           03  WRK-STSRYYMD                        PIC X(08).
           03  WRK-EDSRYYMD                        PIC X(08).
           03  WRK-STBIRTHDAY                      PIC X(08).
           03  WRK-EDBIRTHDAY                      PIC X(08).
           03  WRK-STLASTYMD                       PIC X(08).
           03  WRK-EDLASTYMD                       PIC X(08).
           03  WRK-STBYOYMD                        PIC X(08).
           03  WRK-EDBYOYMD                        PIC X(08).
           03  WRK-STTENKIYMD                      PIC X(08).
           03  WRK-EDTENKIYMD                      PIC X(08).
           03  WRK-STTSYYMD                        PIC X(08).
           03  WRK-EDTSYYMD                        PIC X(08).
           03  WRK-KHYMD-G.
               05  WRK-STKHYMD                     PIC X(08).
               05  WRK-EDKHYMD                     PIC X(08).
           03  WRK-KNYMD-G.
               05  WRK-STKNYMD                     PIC X(08).
               05  WRK-EDKNYMD                     PIC X(08).
           03  WRK-AGE70DAY.
               05  WRK-AGE70DAY-Y                  PIC 9(04).
               05  WRK-AGE70DAY-M                  PIC X(02).
               05  WRK-AGE70DAY-D                  PIC X(02).
           03  WRK-SQL                             PIC X(20000).
           03  WRK-SQL-PNT                         PIC 9(06).
           03  WRK-PNT                             PIC 9(06).
           03  WRK-SQL-ORDER                       PIC X(500).
           03  WRK-SQL-ORDER-PNT                   PIC 9(06).
           03  WRK-NAME-SEQ                        PIC 9(01).
           03  WRK-KOHNUM                          PIC X(04).
           03  WRK-PAYKBN                          PIC X(02).
           03  WRK-YMD                             PIC X(08).
           03  WRK-RECECOMMENT                     PIC X(50).
           03  WRK-SORT-ITEM                       PIC X(50).
           03  WRK-ITEM-G.
               05  WRK-ITEM                        PIC X(50)
                                                   OCCURS  4.
           03  WRK-KOHTBLID                        PIC X(02).
           03  WRK-FUGOU                           PIC X(02).
           03  WRK-NOT                             PIC X(03).
           03  WRK-ANDOR                           PIC X(03).
           03  WRK-SRYCD-G.
               05  WRK-SRYCD                       PIC X(09) OCCURS  5.
           03  WRK-SKBT-G.
               05  WRK-SKBT                        PIC X(1).
               05  WRK-SKBT-STYMD                  PIC X(8).
               05  WRK-SKBT-EDYMD                  PIC X(8).
               05  WRK-SKBT-SEL-STYMD              PIC X(8).
               05  WRK-SKBT-SEL-EDYMD              PIC X(8).
           03  WRK-JIBAIKBN1                       PIC X(1).
           03  WRK-JIBAIKBN2                       PIC X(1).
      *
      *    ¸ÇÄêÃÍÎÎ°è
       01  CONST-AREA.
           03  CONST-AND                   PIC X(01) VALUE "1".
           03  CONST-OR                    PIC X(01) VALUE "2".
           03  CONST-NOT                   PIC X(01) VALUE "9".
           03  CONST-KMK-ITEM-AREA.
               05  FILLER          PIC X(12)   VALUE "PTNUM".
               05  FILLER          PIC X(12)   VALUE "KANANAME".
               05  FILLER          PIC X(12)   VALUE "SEX".
               05  FILLER          PIC X(12)   VALUE "BIRTHDAY".
               05  FILLER          PIC X(12)   VALUE "JKYSNUM".
           03  CONST-KMK-ITEM-R    REDEFINES   CONST-KMK-ITEM-AREA.
               05  CONST-KMK-ITEM  PIC X(12)   OCCURS  5.
      *
           03  CONST-TKMK-ITEM-AREA.
               05  FILLER          PIC X(12)   VALUE "TOTALTEN".
               05  FILLER          PIC X(12)   VALUE "NYUGAIKBN".
               05  FILLER          PIC X(12)   VALUE "PTNUM".
               05  FILLER          PIC X(12)   VALUE "KANANAME".
               05  FILLER          PIC X(12)   VALUE "SEX".
               05  FILLER          PIC X(12)   VALUE "BIRTHDAY".
           03  CONST-TKMK-ITEM-R   REDEFINES   CONST-TKMK-ITEM-AREA.
               05  CONST-TKMK-ITEM PIC X(12)   OCCURS  6.
      *
           03  CONST-SKMK-ITEM-AREA.
               05  FILLER      PIC X(30)   VALUE "PTNUM".
               05  FILLER      PIC X(30)   VALUE "KANANAME".
               05  FILLER      PIC X(30)   VALUE "SEX".
               05  FILLER      PIC X(30)   VALUE "BIRTHDAY".
               05  FILLER      PIC X(30)   VALUE "SRYCD".
               05  FILLER      PIC X(30)   VALUE "SRYKANANAME".
           03  CONST-SKMK-ITEM-R   REDEFINES   CONST-SKMK-ITEM-AREA.
               05  CONST-SKMK-ITEM PIC X(30)   OCCURS  6.
      *
           03  CONST-ORDER-AREA.
               05  FILLER          PIC X(5)    VALUE "ASC".
               05  FILLER          PIC X(5)    VALUE "DESC".
           03  CONST-ORDER-R       REDEFINES   CONST-ORDER-AREA.
               05  CONST-ORDER     PIC X(5)    OCCURS  2.
           03  CONST-ZENPOU        PIC X(01)   VALUE "1".
           03  CONST-BUBUN         PIC X(01)   VALUE "2".
           03  CONST-KOUHOU        PIC X(01)   VALUE "3".
           03  CONST-KANZEN        PIC X(01)   VALUE "4".
           03  CONST-LIKE           PIC X(01)   VALUE "L".
      *
      *    TBL°ìÍ÷
      *    A...TBL_PTINF
      *    B...TBL_SRYKARRK
      *    C...TBL_PTBYOMEI"
      *    D...TBL_PTHKNINF"
      *    E...TBL_PTKOHINF"
      *    F...TBL_RRKSRH"
      *    G...TBL_SRYSRH"
      *    H...TBL_PTNUM"
      *    I...TMPV_Q001"
      *    J...TBL_TSYRRK"
      *    K...TBL_TNKRRK"
      *    L...TBL_RECECOM"
      *    M...TBL_SKYSRH"
      *    N...TBL_SRYSRH(NOT)"
      *    O...TBL_PTBYOMEI(NOT)"
      *    P...TBL_PTNUM_PUBLIC"
      *    Q...TBL_PTCARE_HKNINF"
      *    R...TBL_PTCARE_NINTEI"
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    Ê¸»úÎóÁàºî¥µ¥Ö
           COPY    "CPORCSSTRING.INC".
      *
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "Q01SCR-SPA".
           COPY    "CPQSUB02.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-Q01FREE
           QSUB02-LNK.
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              QSUB02-OT-AREA
                                   FLG-AREA
                                   IDX-AREA
                                   CNT-AREA
                                   WRK-AREA
      *
           PERFORM 100-JOKEN-EDIT-SEC
           MOVE    WRK-SQL         TO  QSUB02-SQL
           MOVE    WRK-SQL-ORDER   TO  QSUB02-SQL-ORDER
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       100-JOKEN-EDIT-SEC          SECTION.
      *
           INITIALIZE                  WRK-SQL
                                       WRK-SQL-ORDER
      *
           MOVE    1               TO  WRK-SQL-PNT
                                       WRK-SQL-ORDER-PNT
      *
           IF    ( SPA-Q01-HKNKOH-JKN  =   CONST-OR )
            AND  ( SPA-Q01-GHKNINF     =   "1" )
            AND  ( SPA-Q01-GKOHINF     =   "1" )
               PERFORM 1001-JOKEN-EDIT-SEC
           ELSE
               PERFORM 1002-JOKEN-EDIT-SEC
           END-IF
      *
           STRING  ") q_reference"
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           EVALUATE    SPA-Q01-OUTKBN
           WHEN  "3"
               CONTINUE
           WHEN  "2"
               PERFORM 1002-GROUP-EDIT-SEC
           WHEN    OTHER
               PERFORM 1001-GROUP-EDIT-SEC
           END-EVALUATE
      *
           PERFORM 1001-ORDER-EDIT-SEC
      *
           IF    ( QSUB02-LIMITCONTROL     =   1 )
      *
               STRING  " LIMIT "                   DELIMITED   BY  SIZE
                       QSUB02-LIMIT                DELIMITED   BY  SIZE
                       " OFFSET "                  DELIMITED   BY  SIZE
                       QSUB02-OFFSET               DELIMITED   BY  SIZE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
           END-IF
      *
           MOVE    SPACE               TO  WRK-SQL (WRK-SQL-PNT:)
      *
           .
       100-JOKEN-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-JOKEN-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TBLNAME
           MOVE    ZERO            TO  FLG-HKNSELPTN
           MOVE    SPACE           TO  WRK-KOHTBLID
           PERFORM 10011-JOKEN-INIT-SEC
      *
           STRING  " FROM ("       DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           MOVE    WRK-SQL-PNT     TO  WRK-PNT
      *
           MOVE    "1"             TO  SPA-Q01-GHKNINF
           MOVE    SPACE           TO  SPA-Q01-GKOHINF
      *
           MOVE    "@@"            TO  WRK-KOHTBLID
           PERFORM 1001-JOKEN-INIT-SEC
      *
           PERFORM 1001-FROM-EDIT-SEC
      *
           PERFORM 1001-WHERE-EDIT-SEC
      *
           STRING  "UNION "        DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           INITIALIZE              FLG-AREA
                                   CNT-AREA
      *
           MOVE    SPACE           TO  SPA-Q01-GHKNINF
           MOVE    "1"             TO  SPA-Q01-GKOHINF
      *
           MOVE    "@@"            TO  WRK-KOHTBLID
           PERFORM 1001-JOKEN-INIT-SEC
      *
           PERFORM 1001-FROM-EDIT-SEC
      *
           PERFORM 1001-WHERE-EDIT-SEC
      *
           MOVE    "1"             TO  SPA-Q01-GHKNINF
           MOVE    "1"             TO  SPA-Q01-GKOHINF
      *
           .
       1001-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1002-JOKEN-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TBLNAME
           MOVE    ZERO            TO  FLG-HKNSELPTN
           MOVE    SPACE           TO  WRK-KOHTBLID
           PERFORM 10011-JOKEN-INIT-SEC
      *
           STRING  " FROM ("
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           MOVE    WRK-SQL-PNT     TO  WRK-PNT
      *
           MOVE    "@@"            TO  WRK-KOHTBLID
           IF    ( SPA-Q01-GKOHINF  =   "1" )
               IF    ( SPA-Q01-GJYURRK =   "1" )
                   IF    ( SPA-Q01-KOHNUM-JKN      =   CONST-AND )
                    AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL ( IDX2    >   5 )
                                OR   ( WRK-KOHTBLID    NOT =   "@@" )
      *
                           IF    (SPA-Q01-KOHNUM (IDX2)  NOT =   SPACE )
                               MOVE    SPACE   TO  WRK-KOHTBLID
                               MOVE    IDX2    TO  WRK-NAME-SEQ
                               STRING  "D"           DELIMITED  BY  SIZE
                                       WRK-NAME-SEQ  DELIMITED  BY  SIZE
                               INTO    WRK-KOHTBLID
                               END-STRING
                           END-IF
      *
                       END-PERFORM
                   ELSE
                       MOVE    "D"         TO  WRK-KOHTBLID
                   END-IF
               ELSE
                   IF    ( SPA-Q01-KOHNUM-JKN      =   CONST-AND )
                    AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL ( IDX2    >   5 )
                                OR   ( WRK-KOHTBLID    NOT =   "@@" )
      *
                           IF    (SPA-Q01-KOHNUM (IDX2)  NOT =   SPACE )
                               MOVE    SPACE   TO  WRK-KOHTBLID
                               MOVE    IDX2    TO  WRK-NAME-SEQ
                               STRING  "E"           DELIMITED  BY  SIZE
                                       WRK-NAME-SEQ  DELIMITED  BY  SIZE
                               INTO    WRK-KOHTBLID
                               END-STRING
                           END-IF
      *
                       END-PERFORM
                   ELSE
                       MOVE    "E"         TO  WRK-KOHTBLID
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 1001-JOKEN-INIT-SEC
      *
           PERFORM 1001-FROM-EDIT-SEC
      *
           PERFORM 1001-WHERE-EDIT-SEC
      *
           .
       1002-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½é´ü½èÍý
      *****************************************************************
       1001-JOKEN-INIT-SEC            SECTION.
      *
           MOVE    1               TO  FLG-TBLNAME
           PERFORM 10011-JOKEN-INIT-SEC
      *
           STRING  " FROM TBL_PTINF A,TBL_PTNUM H "
                                       DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           IF      ( SPA-Q01-STSRYYMD  =   SPACE )
               MOVE    ALL "0"     TO  WRK-STSRYYMD
           ELSE
               MOVE    SPA-Q01-STSRYYMD
                                   TO  WRK-STSRYYMD
           END-IF
      *
           IF      ( SPA-Q01-EDSRYYMD  =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDSRYYMD
           ELSE
               MOVE    SPA-Q01-EDSRYYMD
                                   TO  WRK-EDSRYYMD
           END-IF
      *
           IF      ( SPA-Q01-STBIRTHDAY
                                       =   SPACE )
               MOVE    ALL "0"     TO  WRK-STBIRTHDAY
           ELSE
               MOVE    SPA-Q01-STBIRTHDAY
                                   TO  WRK-STBIRTHDAY
           END-IF
      *
           IF      ( SPA-Q01-EDBIRTHDAY
                                       =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDBIRTHDAY
           ELSE
               MOVE    SPA-Q01-EDBIRTHDAY
                                   TO  WRK-EDBIRTHDAY
           END-IF
      *
           IF      ( SPA-Q01-STLASTYMD =   SPACE )
               MOVE    ALL "0"     TO  WRK-STLASTYMD
           ELSE
               MOVE    SPA-Q01-STLASTYMD
                                   TO  WRK-STLASTYMD
           END-IF
      *
           IF      ( SPA-Q01-EDLASTYMD =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDLASTYMD
           ELSE
               MOVE    SPA-Q01-EDLASTYMD
                                   TO  WRK-EDLASTYMD
           END-IF
      *
           IF      ( SPA-Q01-STBYOYMD =   SPACE )
               MOVE    ALL "0"     TO  WRK-STBYOYMD
           ELSE
               MOVE    SPA-Q01-STBYOYMD
                                   TO  WRK-STBYOYMD
           END-IF
      *
           IF      ( SPA-Q01-EDBYOYMD =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDBYOYMD
           ELSE
               MOVE    SPA-Q01-EDBYOYMD
                                   TO  WRK-EDBYOYMD
           END-IF
      *
           IF      ( SPA-Q01-STTENKIYMD =   SPACE )
               MOVE    ALL "0"     TO  WRK-STTENKIYMD
           ELSE
               MOVE    SPA-Q01-STTENKIYMD
                                   TO  WRK-STTENKIYMD
           END-IF
      *
           IF      ( SPA-Q01-EDTENKIYMD =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDTENKIYMD
           ELSE
               MOVE    SPA-Q01-EDTENKIYMD
                                   TO  WRK-EDTENKIYMD
           END-IF
      *
           MOVE    SPA-Q01-STTSYSKBT   TO  WRK-SKBT
           MOVE    SPA-Q01-STTSYYM     TO  WRK-SKBT-STYMD
           MOVE    SPA-Q01-EDTSYYM     TO  WRK-SKBT-EDYMD
           PERFORM 10012-SKBTYM-INIT-SEC
           MOVE    WRK-SKBT-SEL-STYMD  TO  WRK-STTSYYMD
           MOVE    WRK-SKBT-SEL-EDYMD  TO  WRK-EDTSYYMD
      *
           MOVE    SPA-Q01-KHSKBT      TO  WRK-SKBT
           MOVE    SPA-Q01-STKHYM      TO  WRK-SKBT-STYMD
           MOVE    SPA-Q01-EDKHYM      TO  WRK-SKBT-EDYMD
           PERFORM 10012-SKBTYM-INIT-SEC
           MOVE    WRK-SKBT-SEL-STYMD  TO  WRK-STKHYMD
           MOVE    WRK-SKBT-SEL-EDYMD  TO  WRK-EDKHYMD
      *
           MOVE    SPA-Q01-KNSKBT      TO  WRK-SKBT
           MOVE    SPA-Q01-STKNYM      TO  WRK-SKBT-STYMD
           MOVE    SPA-Q01-EDKNYM      TO  WRK-SKBT-EDYMD
           PERFORM 10012-SKBTYM-INIT-SEC
           MOVE    WRK-SKBT-SEL-STYMD  TO  WRK-STKNYMD
           MOVE    WRK-SKBT-SEL-EDYMD  TO  WRK-EDKNYMD
      *
           .
       1001-JOKEN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½é´ü½èÍý
      *****************************************************************
       10011-JOKEN-INIT-SEC           SECTION.
      *
           EVALUATE    SPA-Q01-OUTKBN
           WHEN    "3"
               PERFORM 100113-JOKEN-INIT-SEC
           WHEN    "2"
               PERFORM 100112-JOKEN-INIT-SEC
           WHEN    OTHER
               PERFORM 100111-JOKEN-INIT-SEC
           END-EVALUATE
      *
           .
       10011-JOKEN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½é´ü½èÍý¡ÊÄÌ¾ï¸¡º÷¡Ë
      *****************************************************************
       100111-JOKEN-INIT-SEC          SECTION.
      *
           IF    ( FLG-TBLNAME     =   ZERO )
      *
               STRING    "INSERT   INTO  TBL_PTSRH ("
                                              DELIMITED    BY SIZE
                         "HOSPNUM,"           DELIMITED    BY SIZE
                         "TERMID,"            DELIMITED    BY SIZE
                         "PTID,"              DELIMITED    BY SIZE
                         "PTNUM,"             DELIMITED    BY SIZE
                         "KANANAME,"          DELIMITED    BY SIZE
                         "SEX,"               DELIMITED    BY SIZE
                         "BIRTHDAY,"          DELIMITED    BY SIZE
                         "DATAKBN,"           DELIMITED    BY SIZE
                         "SEQNUM,",            DELIMITED    BY SIZE
                         "CREYMD,"            DELIMITED    BY SIZE
                         "UPYMD,"             DELIMITED    BY SIZE
                         "UPHMS,"             DELIMITED    BY SIZE
                         "JKYSNUM"            DELIMITED    BY SIZE
                         ")"                  DELIMITED    BY SIZE
                         "SELECT "            DELIMITED    BY SIZE
                         "HOSPNUM,'"          DELIMITED    BY SIZE
                         SPA-TERMID           DELIMITED    BY SPACE
                         "',PTID,"            DELIMITED    BY SIZE
                         "PTNUM,"             DELIMITED    BY SIZE
                         "KANANAME,"          DELIMITED    BY SIZE
                         "SEX,"               DELIMITED    BY SIZE
                         "BIRTHDAY,"          DELIMITED    BY SIZE
                         "'1',"               DELIMITED    BY SIZE
                         "0,"                 DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-YMD           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-YMD           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-HMS           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           ELSE
               STRING    "SELECT "            DELIMITED    BY SIZE
                         "A.HOSPNUM,"         DELIMITED    BY SIZE
                         "A.PTID,"            DELIMITED    BY SIZE
                         "H.PTNUM,"           DELIMITED    BY SIZE
                         "A.KANANAME,"        DELIMITED    BY SIZE
                         "A.SEX,"             DELIMITED    BY SIZE
                         "A.BIRTHDAY,"        DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           END-IF
      *
           EVALUATE    WRK-KOHTBLID
           WHEN    SPACE
               STRING  "SUBSTR(MAX(JKYSNUM),17,20) AS JKYSNUM"
                                                       DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           WHEN    "@@"
               STRING  "CAST('0' AS TEXT) AS JKYSNUM"
                                                       DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           WHEN    OTHER
               STRING  "CAST("              DELIMITED BY SIZE
                       WRK-KOHTBLID          DELIMITED BY SPACE
                       ".TEKSTYMD||SUBSTR("  DELIMITED BY SIZE
                       WRK-KOHTBLID          DELIMITED BY SPACE
                       ".FTNJANUM||'00000000',1,8)||"
                                             DELIMITED BY SIZE
                       WRK-KOHTBLID          DELIMITED BY SPACE
                       ".JKYSNUM AS TEXT)AS JKYSNUM"
                                             DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           .
       100111-JOKEN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½é´ü½èÍý¡ÊÅÀ¿ô¸¡º÷¡Ë
      *****************************************************************
       100112-JOKEN-INIT-SEC          SECTION.
      *
           IF    ( FLG-TBLNAME     =   ZERO )
      *
               STRING    "INSERT   INTO  TBL_PTSRH ("
                                              DELIMITED    BY SIZE
                         "HOSPNUM,"           DELIMITED    BY SIZE
                         "TERMID,"            DELIMITED    BY SIZE
                         "PTID,"              DELIMITED    BY SIZE
                         "PTNUM,"             DELIMITED    BY SIZE
                         "KANANAME,"          DELIMITED    BY SIZE
                         "SEX,"               DELIMITED    BY SIZE
                         "BIRTHDAY,"          DELIMITED    BY SIZE
                         "NYUGAIKBN,"         DELIMITED    BY SIZE
                         "HKNNUM,"            DELIMITED    BY SIZE
                         "KOHNUM1,"           DELIMITED    BY SIZE
                         "KOHNUM2,"           DELIMITED    BY SIZE
                         "TOTALTEN,"          DELIMITED    BY SIZE
                         "DATAKBN,"           DELIMITED    BY SIZE
                         "SEQNUM,",           DELIMITED    BY SIZE
                         "CREYMD,"            DELIMITED    BY SIZE
                         "UPYMD,"             DELIMITED    BY SIZE
                         "UPHMS"              DELIMITED    BY SIZE
                         ")"                  DELIMITED    BY SIZE
                         "SELECT "            DELIMITED    BY SIZE
                         "HOSPNUM,'"          DELIMITED    BY SIZE
                         SPA-TERMID           DELIMITED    BY SPACE
                         "',PTID,"            DELIMITED    BY SIZE
                         "PTNUM,"             DELIMITED    BY SIZE
                         "KANANAME,"          DELIMITED    BY SIZE
                         "SEX,"               DELIMITED    BY SIZE
                         "BIRTHDAY,"          DELIMITED    BY SIZE
                         "NYUGAIKBN,"         DELIMITED    BY SIZE
                         "HKNNUM,"            DELIMITED    BY SIZE
                         "KOHNUM1,"           DELIMITED    BY SIZE
                         "KOHNUM2,"           DELIMITED    BY SIZE
                         "TOTALTEN,"          DELIMITED    BY SIZE
                         "'1',"               DELIMITED    BY SIZE
                         "SEQNUM,",           DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-YMD           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-YMD           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-HMS           DELIMITED    BY SPACE
                         "'"                  DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           ELSE
               STRING    "SELECT "            DELIMITED    BY SIZE
                         "A.HOSPNUM,"         DELIMITED    BY SIZE
                         "A.PTID,"            DELIMITED    BY SIZE
                         "H.PTNUM,"           DELIMITED    BY SIZE
                         "A.KANANAME,"        DELIMITED    BY SIZE
                         "A.SEX,"             DELIMITED    BY SIZE
                         "A.BIRTHDAY,"        DELIMITED    BY SIZE
                         "L.NYUGAIKBN,"       DELIMITED    BY SIZE
                         "L.HKNNUM,"          DELIMITED    BY SIZE
                         "L.KOHNUM1,"         DELIMITED    BY SIZE
                         "L.KOHNUM2,"         DELIMITED    BY SIZE
                         "L.TOTALTEN,"        DELIMITED    BY SIZE
                         "L.SEQNUM,"          DELIMITED    BY SIZE
                         "L.OID"              DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       100112-JOKEN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½é´ü½èÍý¡Ê¿ÇÎÅ¹Ô°Ù¿ôÃÍ¸¡º÷¡Ë
      *****************************************************************
       100113-JOKEN-INIT-SEC          SECTION.
      *
           IF    ( FLG-TBLNAME     =   ZERO )
               STRING    "SELECT "            DELIMITED    BY SIZE
                         "DISTINCT *"         DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           ELSE
               STRING    "SELECT "
                         "G.*,G.OID,"
                         "H.PTNUM,"           DELIMITED    BY SIZE
                         "A.KANANAME,"        DELIMITED    BY SIZE
                         "A.SEX,"             DELIMITED    BY SIZE
                         "A.BIRTHDAY"         DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       100113-JOKEN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    Á°·î¼èÆÀ½èÍý
      *****************************************************************
       10011-GET-ZENGETU-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *
           .
       10011-GET-ZENGETU-EXT.
           EXIT.
      *****************************************************************
      *    ·îËö¼èÆÀ½èÍý
      *****************************************************************
       10011-GET-GETUMATU-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-YMD (1 : 6)     TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
      *
           .
       10011-GET-GETUMATU-EXT.
           EXIT.
      *****************************************************************
      *    ¼±ÊÌ´ü´ÖÊÔ½¸½é´ü½èÍý
      *****************************************************************
       10012-SKBTYM-INIT-SEC            SECTION.
      *
           MOVE    SPACE                   TO  WRK-SKBT-SEL-STYMD
                                               WRK-SKBT-SEL-EDYMD
      *
           EVALUATE    WRK-SKBT
           WHEN    "2"
               CONTINUE
           WHEN    "3"
               CONTINUE
           WHEN    OTHER
               EVALUATE    TRUE
               WHEN  ( WRK-SKBT-STYMD      NOT =   SPACE )
                   MOVE    WRK-SKBT-STYMD  TO  WRK-SKBT-SEL-STYMD
                   IF    ( WRK-SKBT   =   "1" )
                       MOVE    "01"        TO  WRK-SKBT-SEL-STYMD(7:2)
                   END-IF
               WHEN  ( SPA-Q01-STSRYYMD    NOT =   SPACE )
                   MOVE    SPA-Q01-STSRYYMD
                                           TO  WRK-SKBT-SEL-STYMD
               WHEN    OTHER
                   MOVE    ALL "0"         TO  WRK-SKBT-SEL-STYMD
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( WRK-SKBT-EDYMD     NOT =   SPACE )
                   IF    ( WRK-SKBT   =   "1" )
                       MOVE    WRK-SKBT-EDYMD  TO  WRK-YMD
                       PERFORM 10011-GET-GETUMATU-SEC
                       MOVE    LNK-DAY7-KOYOMI TO  WRK-SKBT-SEL-EDYMD
                   ELSE
                       MOVE    WRK-SKBT-EDYMD  TO  WRK-SKBT-SEL-EDYMD
                   END-IF
               WHEN  ( SPA-Q01-EDSRYYMD    NOT =   SPACE )
                   MOVE    SPA-Q01-EDSRYYMD    TO  WRK-SKBT-SEL-EDYMD
               WHEN    OTHER
                   MOVE    ALL "9"             TO  WRK-SKBT-SEL-EDYMD
               END-EVALUATE
      *
           END-EVALUATE
      *
           .
       10012-SKBTYM-INIT-EXT.
           EXIT.
      *****************************************************************
      *    FROM¶çÊÔ½¸½èÍý
      *****************************************************************
       1001-FROM-EDIT-SEC              SECTION.
      *
      *    ÃÏ°èÏ¢·È¾ðÊóÄó¶¡
           IF  ( SPA-Q01-GPTNUM-PUBLIC      =   "1" )
               PERFORM 1001-PTNUM-PUBLIC-JOKEN-SEC
           END-IF
      *
      *    ²ð¸î
           IF  ( SPA-Q01-GKAIGO              =   "1" )
               IF    ( SPA-Q01-KAIGO-HOKEN-G NOT   =   SPACE )
                   PERFORM 1001-PTCARE-HKNINF-JOKEN-SEC
               END-IF
               IF    ( SPA-Q01-KAIGO-NINTEI-G NOT   =   SPACE )
                   PERFORM 1001-PTCARE-NINTEI-JOKEN-SEC
               END-IF
           END-IF
      *
      *    Äã½êÆÀ
           IF  ( SPA-Q01-GTSY      =   "1" )
               PERFORM 1001-TSY-JOKEN-EDIT-SEC
           END-IF
      *
      *    ºÇ½ª¼õ¿ÇÆü
           IF  ( SPA-Q01-GLASTYMD  =   "1" )
               STRING  ",(SELECT HOSPNUM,PTID FROM "
                                                   DELIMITED  BY  SIZE
                       "TBL_SRYKARRK "             DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                   STRING  "WHERE SRYKA ='"        DELIMITED  BY  SIZE
                           SPA-Q01-SRYKA           DELIMITED  BY  SPACE
                           "' "                    DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               STRING  "GROUP BY HOSPNUM,PTID  "   DELIMITED  BY  SIZE
                       "HAVING MAX(LASTYMD) BETWEEN '"
                                                   DELIMITED  BY  SIZE
                       WRK-STLASTYMD               DELIMITED  BY  SPACE
                       "' AND '"                   DELIMITED  BY  SIZE
                       WRK-EDLASTYMD               DELIMITED  BY  SPACE
                       "') B"                      DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
      *    ÉÂÌ¾¥³¡¼¥É
           IF  ( SPA-Q01-GBYOMEI  =   "1" )
               IF    ( SPA-Q01-BYOMEI-JKN  NOT =   CONST-NOT )
                   PERFORM 1001-BYOMEI-JOKEN-EDIT-SEC
               END-IF
           END-IF
      *
           IF      ((  SPA-Q01-GHKNINF     =   "1" )
                OR  (  SPA-Q01-GKOHINF     =   "1" ))
            AND    (   SPA-Q01-GJYURRK     =   "1" )
               PERFORM 1001-TMPVQ001-JOKEN-EDIT-SEC
      *
           ELSE
      *        ÊÝ¸±µ­¹æ¡¦ÈÖ¹æ
               IF   ( SPA-Q01-GHKNINF  =   "1" )
                   MOVE    1               TO  FLG-HKNSELPTN
                   STRING  ",(SELECT AA.HOSPNUM,AA.PTID,AA.DELKBN"
                                                   DELIMITED  BY  SIZE
                        ",BB.HKNJANUM,BB.HONKZKKBN,BB.HOJOKBN,BB.HKNNUM"
                                                   DELIMITED  BY  SIZE
                           ",AA.HKNID,BB.KIGO,BB.NUM,AA.HKNCOMBINUM "
                                                   DELIMITED  BY  SIZE
                           ",BB.TEKEDYMD,CC.JIBAIKBN "
                                                   DELIMITED  BY  SIZE
                           "FROM TBL_HKNCOMBI AA"
                                                   DELIMITED  BY  SIZE
                           " LEFT JOIN TBL_PTHKNINF BB ON "
                                                   DELIMITED  BY  SIZE
                           "(AA.HOSPNUM=BB.HOSPNUM AND AA.PTID=BB.PTID"
                                                   DELIMITED  BY  SIZE
                           " AND AA.HKNID = BB.HKNID)"
                           " LEFT JOIN TBL_PTRSIINF CC ON "
                                                   DELIMITED  BY  SIZE
                           "(AA.HOSPNUM=CC.HOSPNUM AND AA.PTID=CC.PTID"
                                                   DELIMITED  BY  SIZE
                           " AND AA.HKNID = CC.HKNID)) D"
                                                   DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
      *        ¸øÈñÊÝ¸±µ­¹æÈÖ¹æ¡¦ÉéÃ´¼ÔÈÖ¹æ
               IF   ( SPA-Q01-GKOHINF  =   "1" )
                   PERFORM 1001-PTKOHINF-JOKEN-EDIT-SEC
               END-IF
           END-IF
      *
      *    ¿Ç»¡Ç¯·îÆü
           IF   ( SPA-Q01-GJYURRK   =   "1" )
               IF    ( SPA-Q01-GHKNINF   =   "1" )
                OR   ( SPA-Q01-GKOHINF   =   "1" )
                OR   ( SPA-Q01-GSRYACT   =   "1" )
                   CONTINUE
               ELSE
                   STRING  ",TBL_RRKSRH F"         DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-IF
      *
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É
           IF   ( SPA-Q01-GSRYACT  =   "1" )
               PERFORM 1001-SRYACT-JOKEN-EDIT-SEC
           END-IF
      *
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È
           IF   ( SPA-Q01-GRECECOM =   "1" )
               PERFORM 1001-RECECOM-JOKEN-EDIT-SEC
           END-IF
      *
      *    ÀÁµá´ÉÍý¥Æ¡¼¥Ö¥ë
           IF   ( SPA-Q01-GSEIKYU  =   "1" )
               PERFORM 1001-SEIKYU-MAIN-JOKEN-EDIT-SEC
           END-IF
      *
           .
       1001-FROM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    WHERE¶çÊÔ½¸½èÍý
      *****************************************************************
       1001-WHERE-EDIT-SEC             SECTION.
      *
           STRING  " WHERE A.HOSPNUM="          DELIMITED  BY  SIZE
                   SPA-HOSPNUM                  DELIMITED  BY  SIZE
                   " AND "                      DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
      *    ÃÏ°èÏ¢·È¾ðÊóÄó¶¡
           IF  ( SPA-Q01-GPTNUM-PUBLIC    =   "1" )
               MOVE    FLG-JOKEN      TO  FLG-JOKEN-AND
               PERFORM     1001-PTNUM-PUBLIC-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
           IF  ( SPA-Q01-GKAIGO        =   "1" )
      *        ²ð¸îÊÝ¸±¾ðÊóÄó¶¡
               IF    ( SPA-Q01-KAIGO-HOKEN-G  NOT  =   SPACE )
                   MOVE    FLG-JOKEN   TO  FLG-JOKEN-AND
                   PERFORM     1001-PTCARE-HOKEN-SQL-EDIT-SEC
                   MOVE    1           TO  FLG-JOKEN
               END-IF
      *        ²ð¸îÇ§Äê¾ðÊóÄó¶¡
               IF    ( SPA-Q01-KAIGO-NINTEI-G  NOT  =   SPACE )
                   MOVE    FLG-JOKEN   TO  FLG-JOKEN-AND
                   PERFORM     1001-PTCARE-NINTEI-SQL-EDIT-SEC
                   MOVE    1           TO  FLG-JOKEN
               END-IF
           END-IF
      *
      *    Äã½êÆÀ
           IF  ( SPA-Q01-GTSY  =   "1" )
               MOVE    FLG-JOKEN      TO  FLG-JOKEN-AND
               PERFORM     1001-TSY-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
      *    ¥«¥Ê»áÌ¾¡¦´Á»ú»áÌ¾
           IF  ( SPA-Q01-GNAME =   "1" )
               MOVE    FLG-JOKEN      TO  FLG-JOKEN-AND
               PERFORM     1001-PTINF-SQL-EDIT2-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
      *    ºÇ½ª¼õ¿ÇÆü
           IF  ( SPA-Q01-GLASTYMD  =   "1" )
               MOVE    ZERO            TO  FLG-JOKEN-AND
               PERFORM     1001-SRYKARRK-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
      *    ÉÂÌ¾¥³¡¼¥É
           IF  ( SPA-Q01-GBYOMEI   =   "1" )
               IF    ( SPA-Q01-BYOMEI-JKN  NOT =   CONST-NOT )
                   IF    ( SPA-Q01-BYOMEI-JKN  =   CONST-AND )
                     AND ( SPA-Q01-BYOMEI-G    NOT =   SPACE )
                       PERFORM 1001-PTBYOMEI-SQL-AND-EDIT-SEC
                   ELSE
                       MOVE    ZERO            TO  FLG-JOKEN-AND
                       PERFORM 1001-PTBYOMEI-SQL-OR-EDIT-SEC
                       MOVE    1               TO  FLG-JOKEN
                   END-IF
               END-IF
           END-IF
      *
      *    ÊÝ¸±µ­¹æ¡¦ÈÖ¹æ¡¦¸øÈñÊÝ¸±µ­¹æÈÖ¹æ¡¦ÉéÃ´¼ÔÈÖ¹æ
           IF      ((  SPA-Q01-GHKNINF     =   "1" )
                OR  (  SPA-Q01-GKOHINF     =   "1" ))
               IF  (   SPA-Q01-GJYURRK     =   "1" )
                   IF    ( SPA-Q01-KOHNUM-JKN      =   CONST-AND )
                    AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                       PERFORM 1001-TMPVQ001-SQL-AND-EDIT-SEC
                   ELSE
                       MOVE    ZERO        TO  FLG-JOKEN-AND
                       PERFORM 1001-TMPVQ001-SQL-OR-EDIT-SEC
                       MOVE    1           TO  FLG-JOKEN
                   END-IF
               ELSE
                   IF   ( SPA-Q01-GHKNINF  =   "1" )
                       MOVE    ZERO        TO  FLG-JOKEN-AND
                       PERFORM 1001-PTHKNINF-SQL-EDIT-SEC
                       MOVE    1           TO  FLG-JOKEN
                   END-IF
      *
                   IF   ( SPA-Q01-GKOHINF  =   "1" )
                       IF    ( SPA-Q01-KOHNUM-JKN      =   CONST-AND )
                        AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                           PERFORM 1001-PTKOHINF-SQL-AND-EDIT-SEC
                       ELSE
                           MOVE    ZERO        TO  FLG-JOKEN-AND
                           PERFORM 1001-PTKOHINF-SQL-OR-EDIT-SEC
                           MOVE    1           TO  FLG-JOKEN
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    À¸Ç¯·îÆü¡¦À­ÊÌ¡¦½»½ê¡¦¾õÂÖ£±¡¦¾õÂÖ£²¡¦¾õÂÖ£³¡¦¥³¥á¥ó¥È¡¦
      *    ÅÅÏÃÈÖ¹æ¡¦¸ºÌÈ»öÍ³¡¦¥Æ¥¹¥È´µ¼Ô
           IF   ( SPA-Q01-GPTINF       =   "1" )
            OR  ( SPA-Q01-CHKTSTPT     =   "a" OR "1" )
               MOVE    FLG-JOKEN      TO  FLG-JOKEN-AND
               PERFORM     1001-PTINF-SQL-EDIT3-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
      *    ¿Ç»¡Ç¯·îÆü
           IF   ( SPA-Q01-GJYURRK  =   "1" )
               IF    ( SPA-Q01-GHKNINF   =   "1" )
                OR   ( SPA-Q01-GKOHINF   =   "1" )
                OR   ( SPA-Q01-GSRYACT   =   "1" )
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-JOKEN-AND
                   PERFORM     1001-JYURRK-SQL-EDIT-SEC
                   MOVE    1               TO  FLG-JOKEN
               END-IF
           END-IF
      *
      *    ¿ÇÎÅ¶èÊ¬¡¦¿ÇÎÅ¹Ô°Ù¥³¡¼¥É
           IF   ( SPA-Q01-GSRYACT  =   "1" )
               IF    ( SPA-Q01-SRYACT-JKN  =   CONST-AND )
                AND  ( SPA-Q01-SRYCD-G NOT =   SPACE )
                   PERFORM     1001-SRYACT-SQL-AND-EDIT-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-JOKEN-AND
                   PERFORM     1001-SRYACT-SQL-OR-EDIT-SEC
                   MOVE    1               TO  FLG-JOKEN
               END-IF
           END-IF
      *
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È
           IF  ( SPA-Q01-GRECECOM      =   "1" )
               MOVE    FLG-JOKEN      TO  FLG-JOKEN-AND
               PERFORM     1001-RECECOM-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
      *    ÆÃµ­»ö¹à
           IF  ( SPA-Q01-GSEIKYU       =   "1" )
               MOVE    FLG-JOKEN      TO  FLG-JOKEN-AND
               PERFORM     1001-TOKKI-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
      *    ´µ¼ÔÈÖ¹æ
           MOVE    ZERO            TO  FLG-JOKEN-AND
           PERFORM 1001-PTNUM-SQL-EDIT-SEC
           MOVE    1               TO  FLG-JOKEN
      *
      *    NOT¾ò·ï¡ÊÉÂÌ¾¡Ë
           IF  ( SPA-Q01-GXBYOMEI  =   "1" )
               MOVE    ZERO            TO  FLG-JOKEN-AND
               PERFORM     1001-XBYOMEI-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
      *    NOT¾ò·ï¡Ê¿ÇÎÅ¹Ô°Ù¡Ë
           IF  ( SPA-Q01-GXSRYACT      =   "1" )
               MOVE    ZERO            TO  FLG-JOKEN-AND
               PERFORM     1001-XSRYACT-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JOKEN
           END-IF
      *
           .
       1001-WHERE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÃÏ°èÏ¢·È¾ðÊóÄó¶¡¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTNUM-PUBLIC-JOKEN-SEC        SECTION.
      *
           STRING  ",TBL_PTNUM_PUBLIC P "  DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-PTNUM-PUBLIC-JOKEN-EXT.
           EXIT.
      *****************************************************************
      *    ²ð¸îÊÝ¸±¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTCARE-HKNINF-JOKEN-SEC       SECTION.
      *
           STRING  ",TBL_PTCARE_HKNINF Q "  DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-PTCARE-HKNINF-JOKEN-EXT.
           EXIT.
      *****************************************************************
      *    ²ð¸îÇ§Äê¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTCARE-NINTEI-JOKEN-SEC        SECTION.
      *
           STRING  ",TBL_PTCARE_NINTEI R "  DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-PTCARE-NINTEI-JOKEN-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TSY-JOKEN-EDIT-SEC        SECTION.
      *
           EVALUATE    SPA-Q01-TSY
           WHEN    "1"
               STRING  ",TBL_TNKRRK J "            DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  ",TBL_TSYRRK J "            DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  ",(SELECT HOSPNUM,PTID,STYMD,EDYMD"
                                                   DELIMITED  BY  SIZE
                       " FROM TBL_TNKRRK UNION ALL SELECT HOSPNUM,PTID,"
                                                   DELIMITED  BY  SIZE
                       "NINSTYMD AS STYMD,NINEDYMD AS EDYMD"
                                                   DELIMITED  BY  SIZE
                       " FROM TBL_TSYRRK) J "
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           .
       1001-TSY-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾È²ñ¥Ó¥å¡¼¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TMPVQ001-JOKEN-EDIT-SEC   SECTION.
      *
           IF    ( SPA-Q01-KOHNUM-JKN      =   CONST-AND )
            AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
               MOVE    2               TO  FLG-HKNSELPTN
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    ( SPA-Q01-KOHNUM (IDX2)   NOT =   SPACE )
                       MOVE    IDX2    TO  WRK-NAME-SEQ
                       STRING  ",TMPV_Q001 D"      DELIMITED  BY  SIZE
                               WRK-NAME-SEQ        DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
           ELSE
               MOVE    1               TO  FLG-HKNSELPTN
               STRING  ",TMPV_Q001 D"          DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       1001-TMPVQ001-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊó¥Æ¡¼¥Ö¥ë¸¡º÷¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       1001-PTKOHINF-JOKEN-EDIT-SEC   SECTION.
      *
           IF    ( SPA-Q01-KOHNUM-JKN      =   CONST-AND )
            AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDX2)    NOT =   SPACE )
                       MOVE    IDX2    TO  WRK-NAME-SEQ
                       STRING  ",TBL_PTKOHINF E"   DELIMITED  BY  SIZE
                               WRK-NAME-SEQ        DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
           ELSE
               STRING  ",TBL_PTKOHINF E"   DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
      *
           .
       1001-PTKOHINF-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-BYOMEI-JOKEN-EDIT-SEC     SECTION.
      *
           IF    ( SPA-Q01-BYOMEI-JKN      =   CONST-AND )
             AND ( SPA-Q01-BYOMEI-G    NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    (SPA-Q01-BYOMEI (IDX2)   NOT =   SPACE )
                       MOVE    IDX2    TO  WRK-NAME-SEQ
                       STRING  ",TBL_PTBYOMEI C"   DELIMITED  BY  SIZE
                               WRK-NAME-SEQ        DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
           ELSE
               STRING  ",TBL_PTBYOMEI C"       DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       1001-BYOMEI-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-SRYACT-JOKEN-EDIT-SEC     SECTION.
      *
           IF    ( SPA-Q01-SRYACT-JKN  =   CONST-AND )
            AND  ( SPA-Q01-SRYCD-G NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    (SPA-Q01-SRYCD (IDX2) NOT =   SPACE )
                       IF    ( SPA-Q01-GJYURRK =   SPACE )
                           MOVE    IDX2    TO  WRK-NAME-SEQ
                           STRING  ",TBL_SRYSRH G" DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       ELSE
                           MOVE    IDX2    TO  WRK-NAME-SEQ
                           STRING  ",TBL_SRYSRH G"  DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( SPA-Q01-SRYVALITEM  NOT =   SPACE )
                   STRING  ",TBL_SRYSRH G"  DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
           ELSE
               IF    ( SPA-Q01-GJYURRK =   SPACE )
                   STRING  ",TBL_SRYSRH G"         DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  ",TBL_SRYSRH G"          DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-IF
      *
           .
       1001-SRYACT-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-RECECOM-JOKEN-EDIT-SEC     SECTION.
      *
           STRING  ",TBL_RECECOM K"         DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-RECECOM-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÀÁµá´ÉÍý¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-SEIKYU-MAIN-JOKEN-EDIT-SEC     SECTION.
      *
           STRING  ",TBL_SKYSRH L"     DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-SEIKYU-MAIN-JOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-RECECOM-SQL-EDIT-SEC       SECTION.
      *
           MOVE    "K"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           STRING  "(K.SRYYM >= '"         DELIMITED   BY  SIZE
                   WRK-STSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "' AND K.SRYYM <= '"    DELIMITED   BY  SIZE
                   WRK-EDSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "')"                    DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER    WRK-SQL-PNT
           END-STRING
      *
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               MOVE    1           TO  FLG-RECECOM-SQL
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-GHKNINF     =   "1" )
      *
               MOVE    1                   TO  CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               IF    ( FLG-HKNSELPTN           =   2 )
                   STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".HKNCOMBI in (0"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   5 )
      *
                       IF    (SPA-Q01-KOHNUM (IDX1)    NOT =   SPACE )
                           MOVE    IDX1    TO  WRK-NAME-SEQ
                           STRING  ",D"            DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                                   ".HKNCOMBINUM"  DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-PERFORM
                   STRING  ")"     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".HKNCOMBI = D.HKNCOMBINUM"
                                                   DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
           END-IF
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
      *
                MOVE    SPACE     TO  WRK-ITEM-G
      *
                MOVE   SPA-Q01-RECECOM (IDX1)
                                  TO   WRK-RECECOMMENT
      *
               IF    ( WRK-RECECOMMENT
                                   NOT =   SPACE )
                   MOVE    "gsub"          TO  ORCSSTR-COMMAND
                   MOVE    WRK-RECECOMMENT
                                           TO  ORCSSTR-STRING1
                   MOVE    "¡¡"            TO  ORCSSTR-STRING2
                   MOVE    "**"            TO  ORCSSTR-STRING3
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING1 TO  WRK-RECECOMMENT
               END-IF
               INSPECT WRK-RECECOMMENT REPLACING ALL "**"  BY  "  "
               UNSTRING    WRK-RECECOMMENT    DELIMITED  BY  ALL " "
                                   INTO       WRK-ITEM(1)
                                              WRK-ITEM(2)
                                              WRK-ITEM(3)
                                              WRK-ITEM(4)
               END-UNSTRING
      *
               IF  ( WRK-ITEM-G        NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                   STRING  "("                 DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                   MOVE    ZERO            TO  CNT-RECECOM
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   4 )
                       IF    ( WRK-ITEM (IDX2)     NOT =   SPACE )
                           COMPUTE CNT-RECECOM =   CNT-RECECOM +   1
                           IF    ( CNT-RECECOM =   1 )
                               CONTINUE
                           ELSE
                               STRING  "AND"   DELIMITED   BY  SIZE
                               INTO    WRK-SQL
                               WITH    POINTER WRK-SQL-PNT
                               END-STRING
                           END-IF
                           STRING  "("             DELIMITED   BY  SIZE
                               WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                               ".COMMENT "         DELIMITED   BY  SIZE
                               "LIKE '%"           DELIMITED   BY  SIZE
                               WRK-ITEM(IDX2)      DELIMITED   BY  SPACE
                               "%')"               DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-PERFORM
      *
                   STRING  ")"                 DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-RECECOM-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃµ­»ö¹à¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TOKKI-SQL-EDIT-SEC       SECTION.
      *
           MOVE    "L"         TO    WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           STRING  "(L.SRYYM >= '"         DELIMITED   BY  SIZE
                   WRK-STSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "' AND L.SRYYM <= '"    DELIMITED   BY  SIZE
                   WRK-EDSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "')"                    DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER    WRK-SQL-PNT
           END-STRING
      *
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-GHKNINF     =   "1" )
      *
               MOVE    1                   TO  CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               IF    ( FLG-HKNSELPTN           =   2 )
                   STRING  "L.HKNID in (null"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   5 )
      *
                       IF    (SPA-Q01-KOHNUM (IDX1)    NOT =   SPACE )
                           MOVE    IDX1    TO  WRK-NAME-SEQ
                           STRING  ",COALESCE(D"   DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                                   ".HKNID,0)"     DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-PERFORM
                   STRING  ")"     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  "L.HKNID in (null, COALESCE(D.HKNID,0))"
                                       DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
           END-IF
      *
           IF    ( SPA-Q01-TOKKICD     NOT =   SPACE )
            OR   ( SPA-Q01-TOKKINAME   NOT =   SPACE )
      *
               MOVE    ZERO                TO  CNT-JOKEN
      *
               IF    ( SPA-Q01-TOKKICD NOT =   SPACE )
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                   STRING  "'"                 DELIMITED BY SIZE
                       SPA-Q01-TOKKICD         DELIMITED BY SIZE
                       "'"                     DELIMITED BY SIZE
                      "in(L.TOKKI1,L.TOKKI2,L.TOKKI3,L.TOKKI4,L.TOKKI5)"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               IF    ( SPA-Q01-TOKKINAME   NOT =   SPACE )
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                   STRING  "'"                 DELIMITED BY SIZE
                       SPA-Q01-TOKKINAME       DELIMITED BY SPACE
                       "'"                     DELIMITED BY SIZE
                       "in(L.TOKKINM1,L.TOKKINM2,L.TOKKINM3"
                                               DELIMITED BY SIZE
                       ",L.TOKKINM4,L.TOKKINM5)"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           END-IF
      *
           IF    ( SPA-Q01-STTOTALTEN    >   ZERO )
      *
               MOVE    1           TO  CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               STRING  "L.TOTALTEN >= " DELIMITED BY SIZE
                       SPA-Q01-STTOTALTEN DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           END-IF
      *
           IF    ( SPA-Q01-EDTOTALTEN    >   ZERO )
      *
               MOVE    1           TO  CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               STRING  "L.TOTALTEN <= " DELIMITED BY SIZE
                       SPA-Q01-EDTOTALTEN DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           END-IF
      *
           IF    ( SPA-Q01-SAIGAIKBN   =   "1" OR "2" )
      *
               MOVE    1           TO  CNT-JOKEN
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               EVALUATE   SPA-Q01-SAIGAIKBN
               WHEN    "1"
                   STRING  "L.ETC_YKZFTN1"      DELIMITED BY SIZE
                       " in( 6,7,8)"            DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               WHEN    "2"
                   STRING  "L.ETC_YKZFTN1"      DELIMITED BY SIZE
                       " not in( 0, 6,7,8)"     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-EVALUATE
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           END-IF
      *
      *2
           IF    ( SPA-Q01-TENKENRECE   =   "1" )
      *
               MOVE    1           TO  CNT-JOKEN
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               STRING  "L.SKYKBN_RECEDEN='1'"  DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-TOKKI-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    NOT¾ò·ï¸¡º÷¾ò·ïÊÔ½¸½èÍý¡Ê¿ÇÎÅ¹Ô°Ù¡Ë
      *****************************************************************
       1001-XSRYACT-SQL-EDIT-SEC     SECTION.
      *
           MOVE    "N"         TO    WRK-SQL-TBL-NAME
      *
           IF  ( FLG-JOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "NOT EXISTS("              DELIMITED   BY  SIZE
                   "SELECT * FROM TBL_SRYSRH N WHERE "
                   "(N.HOSPNUM="               DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   "N.PTID=A.PTID"             DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   "N.UPYMD='"                 DELIMITED   BY  SIZE
                   QSUB02-YMD                  DELIMITED   BY  SIZE
                   "')AND("                    DELIMITED   BY  SIZE
                   "N.UPHMS='"                 DELIMITED   BY  SIZE
                   QSUB02-HMS                  DELIMITED   BY  SIZE
                   "')AND"                      DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           INITIALIZE          WRK-SRYCD-G
           MOVE    SPA-Q01-XSRYCD (1)      TO  WRK-SRYCD (1)
           MOVE    SPA-Q01-XSRYCD (2)      TO  WRK-SRYCD (2)
           MOVE    SPA-Q01-XSRYCD (3)      TO  WRK-SRYCD (3)
           MOVE    SPA-Q01-XSRYCD (4)      TO  WRK-SRYCD (4)
           MOVE    SPA-Q01-XSRYCD (5)      TO  WRK-SRYCD (5)
           PERFORM 10011-SRYACT-SQL-OR-EDIT-SEC
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-XSRYACT-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    NOT¾ò·ï¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊÉÂÌ¾¡Ë
      *****************************************************************
       1001-XBYOMEI-SQL-EDIT-SEC     SECTION.
      *
           MOVE    "O"         TO    WRK-SQL-TBL-NAME
      *
           IF  ( FLG-JOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "NOT EXISTS("              DELIMITED   BY  SIZE
                   "SELECT * FROM TBL_PTBYOMEI O WHERE "
                   "(O.HOSPNUM="               DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   "O.PTID=A.PTID"             DELIMITED   BY  SIZE
                   ")AND"                      DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           PERFORM 10011-PTBYOMEI-SQL-OR-EDIT-SEC
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-XBYOMEI-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÃÏ°èÏ¢·È¾ðÊóÄó¶¡¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTNUM-PUBLIC-SQL-EDIT-SEC           SECTION.
      *
           MOVE    "P"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           STRING  "(P.AGREEMENT   = '"    DELIMITED   BY  SIZE
                   SPA-Q01-AGREEMENT       DELIMITED   BY  SPACE
                   "')"                    DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER    WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-PTNUM-PUBLIC-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ²ð¸îÊÝ¸±¾ðÊó¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTCARE-HOKEN-SQL-EDIT-SEC      SECTION.
      *
           MOVE    "Q"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           STRING  "(1=1)"             DELIMITED   BY  SIZE
           INTO            WRK-SQL
           WITH POINTER    WRK-SQL-PNT
           END-STRING
      *
           EVALUATE    SPA-Q01-KHSKBT
           WHEN    "1"
           WHEN    "4"
               STRING  "AND(Q.TEKSTYMD <= '"       DELIMITED   BY  SIZE
                       WRK-EDKHYMD                 DELIMITED   BY  SPACE
                       "' AND Q.TEKEDYMD >= '"     DELIMITED   BY  SIZE
                       WRK-STKHYMD                 DELIMITED   BY  SPACE
                       "')"                        DELIMITED   BY  SIZE
               INTO            WRK-SQL
               WITH POINTER    WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  "AND(Q.TEKSTYMD LIKE '"     DELIMITED   BY  SIZE
                       SPA-Q01-STKHYM              DELIMITED   BY  SPACE
                       "%')"                       DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  "AND(Q.TEKEDYMD LIKE '"     DELIMITED   BY  SIZE
                       SPA-Q01-STKHYM              DELIMITED   BY  SPACE
                       "%')"                       DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           IF    ( SPA-Q01-KHHKNJANUM  NOT =       SPACE )
               STRING  "AND(Q.HKNJANUM = '"        DELIMITED   BY  SIZE
                       SPA-Q01-KHHKNJANUM          DELIMITED   BY  SPACE
                       "')"                        DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-PTCARE-HOKEN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ²ð¸îÇ§Äê¾ðÊó¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTCARE-NINTEI-SQL-EDIT-SEC      SECTION.
      *
           MOVE    "R"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           STRING  "(1=1)"             DELIMITED   BY  SIZE
           INTO            WRK-SQL
           WITH POINTER    WRK-SQL-PNT
           END-STRING
      *
           EVALUATE    SPA-Q01-KNSKBT
           WHEN    "1"
           WHEN    "4"
               STRING  "AND(R.TEKSTYMD <= '"       DELIMITED   BY  SIZE
                       WRK-EDKNYMD                 DELIMITED   BY  SPACE
                       "' AND R.TEKEDYMD >= '"     DELIMITED   BY  SIZE
                       WRK-STKNYMD                 DELIMITED   BY  SPACE
                       "')"                        DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH POINTER    WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  "AND(R.TEKSTYMD LIKE '"     DELIMITED   BY  SIZE
                       SPA-Q01-STKNYM              DELIMITED   BY  SPACE
                       "%')"                       DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  "AND(R.TEKEDYMD LIKE '"     DELIMITED   BY  SIZE
                       SPA-Q01-STKNYM              DELIMITED   BY  SPACE
                       "%')"                       DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           IF    ( SPA-Q01-KNJOTAI     NOT =       SPACE )
               EVALUATE    SPA-Q01-KNJOTAI
               WHEN    "1x"
                   STRING  "AND(R.CAREJOTAICD LIKE '1%')"
                                                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
               WHEN    "2x"
                   STRING  "AND(R.CAREJOTAICD LIKE '2%')"
                                                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
               WHEN    "xx"
                   STRING  "AND(R.CAREJOTAICD BETWEEN '10' AND '29')"
                                                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
               WHEN    OTHER
                   STRING  "AND(R.CAREJOTAICD = '" DELIMITED   BY  SIZE
                       SPA-Q01-KNJOTAI             DELIMITED   BY  SPACE
                       "')"                        DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
               END-EVALUATE
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-PTCARE-NINTEI-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¼Ô¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TSY-SQL-EDIT-SEC           SECTION.
      *
           IF    ( SPA-Q01-TSY     =   "2" )
               PERFORM 10011-TSY-SQL-EDIT-SEC
           ELSE
               PERFORM 10011-TNK-SQL-EDIT-SEC
           END-IF
      *
           .
       1001-TSY-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¼Ô£±(£ï£ò£²)¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-TNK-SQL-EDIT-SEC          SECTION.
      *
           MOVE    "J"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           EVALUATE    SPA-Q01-STTSYSKBT
           WHEN    "1"
           WHEN    "4"
               STRING  "(J.STYMD <= '"         DELIMITED   BY  SIZE
                       WRK-EDTSYYMD            DELIMITED   BY  SPACE
                       "' AND J.EDYMD >= '"    DELIMITED   BY  SIZE
                       WRK-STTSYYMD            DELIMITED   BY  SPACE
                       "')"                    DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER    WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  "(J.STYMD LIKE '"       DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  "(J.EDYMD LIKE '"       DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       10011-TNK-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¼Ô£²¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-TSY-SQL-EDIT-SEC          SECTION.
      *
           MOVE    "J"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           EVALUATE    SPA-Q01-STTSYSKBT
           WHEN    "1"
           WHEN    "4"
               STRING  "(J.NINSTYMD <= '"      DELIMITED   BY  SIZE
                       WRK-EDTSYYMD            DELIMITED   BY  SPACE
                       "' AND J.NINEDYMD >= '" DELIMITED   BY  SIZE
                       WRK-STTSYYMD            DELIMITED   BY  SPACE
                       "')"                    DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER    WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  "(J.NINSTYMD LIKE '"    DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  "(J.NINEDYMD LIKE '"    DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       10011-TSY-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý£²
      *****************************************************************
       1001-PTINF-SQL-EDIT2-SEC        SECTION.
      *
      *    ´Á»ú»áÌ¾¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-NAME-SELECT-EDIT-SEC
      *
           .
       1001-PTINF-SQL-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý£³
      *****************************************************************
       1001-PTINF-SQL-EDIT3-SEC        SECTION.
      *
      *    À¸Ç¯·îÆü¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-BIRTH-SELECT-EDIT-SEC
      *
      *    ÃÂÀ¸·î¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-BIRTHMON-SELECT-EDIT-SEC
      *
      *    À­ÊÌ¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-SEX-SELECT-EDIT-SEC
      *
      *    ½»½ê¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-HOME-ADRS-SELECT-EDIT-SEC
      *
      *    ¾õÂÖ£±¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-CONDITION1-SEL-EDIT-SEC
      *
      *    ¾õÂÖ£²¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-CONDITION2-SEL-EDIT-SEC
      *
      *    ¾õÂÖ£³¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-CONDITION3-SEL-EDIT-SEC
      *
      *    ¸ºÌÈ»öÍ³¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-GENMEN-SEL-EDIT-SEC
      *
      *    ¥³¥á¥ó¥È¸¡º÷¾ò·ïÊÔ½¸½èÍý
           IF    ( SPA-Q01-SELCOMMENT  =   "1" OR "2" )
               PERFORM 10011-COMMENT-SELECT-EDIT-SEC
           END-IF
      *
      *    ÅÅÏÃÈÖ¹æ¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-TEL-SELECT-EDIT-SEC
      *
      *    »àË´¶èÊ¬¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-DEATH-SELECT-EDIT-SEC
      *
      *    ¥Æ¥¹¥È´µ¼Ô¸¡º÷¾ò·ïÊÔ½¸½èÍý
           IF    ( SPA-Q01-CHKTSTPT    =   "a" OR "1" )
               PERFORM 10011-TSTPT-SELECT-EDIT-SEC
           END-IF
      *
           .
       1001-PTINF-SQL-EDIT3-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¥¹¥È´µ¼Ô¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-TSTPT-SELECT-EDIT-SEC     SECTION.
      *
          MOVE     1               TO  CNT-JOKEN
      *
          PERFORM  900-SQL-JOKEN-EDIT2-SEC
      *
          IF    (  SPA-Q01-CHKTSTPT    =   "1" )
              STRING   "(A.TSTPTNUMKBN =  '1')"    DELIMITED   BY  SIZE
              INTO     WRK-SQL
              WITH     POINTER     WRK-SQL-PNT
              END-STRING
          ELSE
              STRING   "(A.TSTPTNUMKBN <> '1')"    DELIMITED   BY  SIZE
              INTO     WRK-SQL
              WITH     POINTER     WRK-SQL-PNT
              END-STRING
          END-IF
      *
          PERFORM  900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-TSTPT-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-SRYKARRK-SQL-EDIT-SEC      SECTION.
      *
           MOVE    "B"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT8-SEC
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
      *
           .
       1001-SRYKARRK-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÉÂÌ¾¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊOR¡Ë
      *****************************************************************
       1001-PTBYOMEI-SQL-OR-EDIT-SEC       SECTION.
      *
           MOVE    "C"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           PERFORM 10011-PTBYOMEI-SQL-OR-EDIT-SEC
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-PTBYOMEI-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÉÂÌ¾¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊOR¡Ë
      *****************************************************************
       10011-PTBYOMEI-SQL-OR-EDIT-SEC      SECTION.
      *
           STRING "((("                DELIMITED   BY  SIZE
                  WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                  ".SRYYMD <= '"       DELIMITED   BY  SIZE
                  WRK-EDSRYYMD         DELIMITED   BY  SIZE
                  "') AND ("           DELIMITED   BY  SIZE
                  WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                  ".TENKIYMD >= '"     DELIMITED   BY  SIZE
                  WRK-STSRYYMD         DELIMITED   BY  SIZE
                  "')) OR "            DELIMITED   BY  SIZE
                  "(("                 DELIMITED   BY  SIZE
                  WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                  ".SRYYMD <= '"       DELIMITED   BY  SIZE
                  WRK-EDSRYYMD         DELIMITED   BY  SIZE
                  "') AND ("           DELIMITED   BY  SIZE
                  WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                  ".TENKIYMD = ''"     DELIMITED   BY  SIZE
                  "))) AND ("          DELIMITED   BY  SIZE
                  WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                  ".DLTFLG = ' ')AND " DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               MOVE    1           TO  FLG-PTBYOMEI-SQL
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-STBYOYMD    NOT =   SPACE )
            OR   ( SPA-Q01-EDBYOYMD    NOT =   SPACE )
               PERFORM 10011-BYOYMD-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-UTAGAIKBN   NOT =   SPACE )
               PERFORM 10011-UTAGAIFLG-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-KYUSEIKBN   NOT =   SPACE )
               PERFORM 10012-UTAGAIFLG-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-TENKIKBN   NOT =   SPACE )
               PERFORM 10011-TENKIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SYUBYOKBN   NOT =   SPACE )
               PERFORM 10012-SYUBYOFLG-SQL-EDIT-SEC
           END-IF
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-BYOMEI (IDX1)   NOT =   SPACE )
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *
      *            ÉÂÌ¾¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-BYOMEI (IDX1)
                                               DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÉÂÌ¾¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-BYOMEI (IDX1)
                                               TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN         TO  WRK-NUM-LEN
      *
                   EVALUATE    SPA-Q01-BYOCOMP (IDX1)
                   WHEN    CONST-BUBUN
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI LIKE '%"       DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDX1)   DELIMITED   BY  SPACE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    CONST-KOUHOU
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI LIKE '%"       DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDX1)   DELIMITED   BY  SPACE
                           "')"                    DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    CONST-KANZEN
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI =    '"        DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDX1)   DELIMITED   BY  SPACE
                           "')"                    DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    CONST-LIKE
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI LIKE '"        DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDX1)   DELIMITED   BY  SPACE
                           "')"                    DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    OTHER
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI LIKE '"        DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDX1)   DELIMITED   BY  SPACE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-PTBYOMEI-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÉÂÌ¾¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊAND¡Ë
      *****************************************************************
       1001-PTBYOMEI-SQL-AND-EDIT-SEC      SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
               UNTIL   (   IDXA    >   5 )
      *
               IF    (SPA-Q01-BYOMEI (IDXA)    NOT =   SPACE )
      *
                   MOVE    ZERO            TO  FLG-JOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "C"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
                   STRING "((("                    DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".SRYYMD <= '"           DELIMITED BY SIZE
                          WRK-EDSRYYMD             DELIMITED BY SIZE
                          "') AND ("               DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".TENKIYMD >= '"         DELIMITED BY SIZE
                          WRK-STSRYYMD             DELIMITED BY SIZE
                          "')) OR "                DELIMITED BY SIZE
                          "(("                     DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".SRYYMD <= '"           DELIMITED BY SIZE
                          WRK-EDSRYYMD             DELIMITED BY SIZE
                          "') AND ("               DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".TENKIYMD = ''"         DELIMITED BY SIZE
                          "))) AND ("              DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".DLTFLG = ' ') AND "    DELIMITED BY SIZE
                   INTO        WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
      *
                   IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                       MOVE    1           TO  FLG-PTBYOMEI-SQL
                       PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                       PERFORM 10011-SRYKA-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-STBYOYMD    NOT =   SPACE )
                    OR   ( SPA-Q01-EDBYOYMD    NOT =   SPACE )
                      PERFORM  10011-BYOYMD-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-UTAGAIKBN   NOT =   SPACE )
                       PERFORM 10011-UTAGAIFLG-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-KYUSEIKBN   NOT =   SPACE )
                       PERFORM 10012-UTAGAIFLG-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-TENKIKBN    NOT =   SPACE )
                       PERFORM 10011-TENKIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SYUBYOKBN   NOT =   SPACE )
                       PERFORM 10012-SYUBYOFLG-SQL-EDIT-SEC
                   END-IF
      *
                   MOVE    1               TO  CNT-JOKEN
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *
      *            ÉÂÌ¾¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-BYOMEI (IDXA)
                                               DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
      *
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÉÂÌ¾¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-BYOMEI (IDXA)
                                           TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN     TO  WRK-NUM-LEN
      *
                   EVALUATE    SPA-Q01-BYOCOMP (IDXA)
                   WHEN    CONST-BUBUN
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI LIKE '%"       DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDXA)   DELIMITED   BY  SPACE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    CONST-KOUHOU
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI LIKE '%"       DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDXA)   DELIMITED   BY  SPACE
                           "')"                    DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    CONST-KANZEN
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI = '"           DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDXA)   DELIMITED   BY  SPACE
                           "')"                    DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    OTHER
                       STRING
                           "("                     DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".BYOMEI LIKE '"        DELIMITED   BY  SIZE
                           SPA-Q01-BYOMEI (IDXA)   DELIMITED   BY  SPACE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-EVALUATE
      *
                   PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
                   PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JOKEN
      *
               END-IF
      *
           END-PERFORM
      *
           .
       1001-PTBYOMEI-SQL-AND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾È²ñ¥Ó¥å¡¼¸¡º÷¾ò·ïÊÔ½¸½èÍý(AND)
      *****************************************************************
       1001-TMPVQ001-SQL-AND-EDIT-SEC  SECTION.
      *
           MOVE    1                   TO  FLG-TMPVQ001-SQL
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
                   MOVE    ZERO            TO  FLG-JOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "D"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
                   STRING "(("             DELIMITED   BY  SIZE
                      WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                      ".SRYYMD <= '"       DELIMITED   BY  SIZE
                      WRK-EDSRYYMD         DELIMITED   BY  SIZE
                      "') AND ("           DELIMITED   BY  SIZE
                      WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                      ".SRYYMD >= '"       DELIMITED   BY  SIZE
                      WRK-STSRYYMD         DELIMITED   BY  SIZE
                      "')) AND"            DELIMITED   BY  SIZE
                   INTO        WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
      *
                   IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                       PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                       PERFORM 10011-SRYKA-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-DRCD      NOT =   SPACE )
                       PERFORM 10011-DRCD-SQL-EDIT-SEC
                   END-IF
      *
      *            ´µ¼Ô¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
                   MOVE    SPA-Q01-KOHNUM (IDXA)
                                           TO  WRK-KOHNUM
                   MOVE    SPA-Q01-PAYKBN (IDXA)
                                           TO  WRK-PAYKBN
                   PERFORM 10012-PTKOHINF-KOHNUM-EDIT-SEC
                   PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
      *
                   PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JOKEN
      *
               END-IF
      *
           END-PERFORM
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
           IF   ( SPA-Q01-GHKNINF   =   "1" )
      *
               MOVE    ZERO            TO  FLG-JOKEN-OR
      *
               PERFORM 900-SQL-JOKEN-EDIT5-SEC
      *
               PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
                       MOVE    ZERO        TO  FLG-JOKEN-AND
      *
                       MOVE    IDXA        TO  WRK-NAME-SEQ
                       MOVE    "D"         TO  WRK-SQL-TBL-NAME
                       MOVE    WRK-NAME-SEQ
                                           TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                       PERFORM 900-SQL-JOKEN-EDIT6-SEC
                       PERFORM 10011-PTHKNINF-SQL-EDIT-SEC
                       PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
               MOVE    1               TO  FLG-JOKEN
      *
           END-IF
      *
           .
       1001-TMPVQ001-SQL-AND-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾È²ñ¥Ó¥å¡¼¸¡º÷¾ò·ïÊÔ½¸½èÍý(OR)
      *****************************************************************
       1001-TMPVQ001-SQL-OR-EDIT-SEC   SECTION.
      *
           MOVE    "D"                 TO  WRK-SQL-TBL-NAME
      *
           MOVE    1                   TO  FLG-TMPVQ001-SQL
      *
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           STRING "((D.SRYYMD <= '"
                      WRK-EDSRYYMD
                      "') AND (D.SRYYMD >= '"
                      WRK-STSRYYMD
                      "')) AND"
           DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                   PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-DRCD      NOT =   SPACE )
               PERFORM 10011-DRCD-SQL-EDIT-SEC
           END-IF
      *
           IF   ( SPA-Q01-GHKNINF   =   "1" )
      *        ´µ¼ÔÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
               PERFORM 10011-PTHKNINF-SQL-EDIT-SEC
           END-IF
      *
           IF   ( SPA-Q01-GKOHINF   =   "1" )
      *        ´µ¼Ô¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
               PERFORM 10011-PTKOHINF-KOHNUM-EDIT-SEC
               PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
               PERFORM 10011-PTKOHINF-ETC-EDIT-SEC
           END-IF
      *
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-TMPVQ001-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
      *****************************************************************
       10011-PTHKNINF-SQL-EDIT-SEC     SECTION.
      *
           MOVE    ZERO                TO  CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-HKNJANUM (IDX1)
                                           NOT =   SPACE )
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   IF    ( CNT-JOKEN  =   1 )
                    OR   ( SPA-Q01-HKNJANUM-JKN    =   "2" )
                       PERFORM 900-SQL-JOKEN-EDIT2-SEC
                   ELSE
                       PERFORM 900-SQL-JOKEN-EDIT9-SEC
                   END-IF
      *
      *            ÊÝ¸±¼ÔÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-HKNJANUM (IDX1) 
                                           DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÊÝ¸±¼ÔÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-HKNJANUM (IDX1) TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN             TO  WRK-KIGO-LEN
      *
                   IF    ( SPA-Q01-HKNJANUM-JKN  =   CONST-NOT )
                       STRING  "(NOT"      DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
                   STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNJANUM = '"     DELIMITED   BY  SIZE
                           SPA-Q01-HKNJANUM (IDX1)(1: WRK-KIGO-LEN)
                                               DELIMITED   BY  SIZE
                           "')"                DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
                   IF    ( SPA-Q01-HKNJANUM-JKN  =   CONST-NOT )
                       STRING  "OR("           DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNJANUM is null" DELIMITED   BY  SIZE
                           "))"                DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           IF      ( SPA-Q01-HONKZKKBN NOT =   SPACE )
      *
               MOVE    1                   TO  CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               STRING  "("
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HONKZKKBN = '"        DELIMITED   BY  SIZE
                       SPA-Q01-HONKZKKBN       DELIMITED   BY  SIZE
                       "')"                    DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
           END-IF
      *
           IF      ( SPA-Q01-FTNRATE       NOT =   SPACE )
      *
               MOVE    1                   TO  CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               PERFORM 100111-FTNRATE-SQL-EDIT-SEC
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
           END-IF
      *
           IF      ( SPA-Q01-HKNEDYM   NOT =   SPACE )
      *
               MOVE    1                   TO  CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               IF    ( FLG-TMPVQ001-SQL    =   1 )
                   STRING  "("
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNEDYMD LIKE '"  DELIMITED   BY  SIZE
                           SPA-Q01-HKNEDYM     DELIMITED   BY  SIZE
                           "%')"               DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  "("
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".TEKEDYMD LIKE '"  DELIMITED   BY  SIZE
                           SPA-Q01-HKNEDYM     DELIMITED   BY  SIZE
                           "%')"               DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
      *        ##Àè¤Î´ü´Ö¤Ë·ÑÂ³Ãæ¤ÎÊÝ¸±¤¬¤¢¤Ã¤¿¤éÂÐ¾Ý¤«¤é½ü¤¯¾ì¹ç"
      *        ##(»ÃÄê¡Ë
      *        STRING  "AND (("            DELIMITED   BY  SIZE
      *                WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
      *                ".HOSPNUM,"         DELIMITED   BY  SIZE
      *                WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
      *                ".PTID,"            DELIMITED   BY  SIZE
      *                WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
      *                ".HKNNUM,"          DELIMITED   BY  SIZE
      *                WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
      *                ".HKNJANUM,"        DELIMITED   BY  SIZE
      *                WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
      *                ".KIGO,"            DELIMITED   BY  SIZE
      *                WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
      *                ".NUM"              DELIMITED   BY  SIZE
      *                ")NOT IN(SELECT HOSPNUM,PTID,HKNNUM,HKNJANUM,"
      *                                    DELIMITED   BY  SIZE
      *                "KIGO,NUM FROM TBL_PTHKNINF WHERE HOSPNUM="
      *                                    DELIMITED   BY  SIZE
      *                SPA-HOSPNUM         DELIMITED   BY  SIZE
      *                " AND PTID="        DELIMITED   BY  SIZE
      *                WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
      *                ".PTID AND TEKSTYMD>'"
      *                                    DELIMITED   BY  SIZE
      *                SPA-Q01-HKNEDYM     DELIMITED   BY  SIZE
      *                "31'))"             DELIMITED   BY  SIZE
      *        INTO    WRK-SQL
      *        WITH    POINTER WRK-SQL-PNT
      *        END-STRING
      *        ##########################
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
           END-IF
      *
           IF    ( SPA-Q01-HKNNUM-JKN    =   CONST-NOT )
                   MOVE    "<>"        TO  WRK-FUGOU
                   MOVE    "NOT"       TO  WRK-NOT
                   MOVE    "AND"       TO  WRK-ANDOR
           ELSE
                   MOVE    "="         TO  WRK-FUGOU
                   MOVE    SPACE       TO  WRK-NOT
                   MOVE    "OR"        TO  WRK-ANDOR
           END-IF
           MOVE    ZERO                TO  CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-HKNNUM (IDX1) NOT =   SPACE )
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   IF    ( CNT-JOKEN  =   1 )
                    OR   ( SPA-Q01-HKNNUM-JKN  =   "2" )
                       PERFORM 900-SQL-JOKEN-EDIT2-SEC
                   ELSE
                       PERFORM 900-SQL-JOKEN-EDIT9-SEC
                   END-IF
      *
                   EVALUATE    SPA-Q01-HKNNUM (IDX1)
                   WHEN    "SYA"
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM "          DELIMITED   BY  SIZE
                           WRK-NOT             DELIMITED   BY  SPACE
                           " in ('001','002','003','004'"
                                               DELIMITED   BY  SIZE
                           ",'006','007','031','032','033','034','063'"
                                               DELIMITED   BY  SIZE
                           ",'072','073','074','075','009'"
                                               DELIMITED   BY  SIZE
                           ")"                 DELIMITED   BY  SIZE
                           WRK-ANDOR           DELIMITED   BY  SPACE
                           "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM is "       DELIMITED   BY  SIZE
                           WRK-NOT             DELIMITED   BY  SPACE
                           " null"             DELIMITED   BY  SIZE
                           "))"                DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    "KOK"
                       STRING  "(("            DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM "          DELIMITED   BY  SIZE
                           WRK-NOT             DELIMITED   BY  SPACE
                           " in ('060','067','068','069')"
                                               DELIMITED   BY  SIZE
                           ")"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                       IF    ( SPA-Q01-HKNNUM-JKN  =   CONST-NOT )
                           STRING  "or ("          DELIMITED   BY  SIZE
                               WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                               ".HKNNUM "          DELIMITED   BY  SIZE
                               "is null"           DELIMITED   BY  SIZE
                               ")"                 DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                       STRING  ")"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    "KOU"
                       STRING  "(("            DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM "          DELIMITED   BY  SIZE
                           WRK-NOT             DELIMITED   BY  SPACE
                           " in ('039','040')" DELIMITED   BY  SIZE
                           ")"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                       IF    ( SPA-Q01-HKNNUM-JKN    =   CONST-NOT )
                           STRING  "or ("          DELIMITED   BY  SIZE
                               WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                               ".HKNNUM "          DELIMITED   BY  SIZE
                               "is null"           DELIMITED   BY  SIZE
                               ")"                 DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                       STRING  ")"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    "JB0"
                   WHEN    "JB1"
                   WHEN    "JB2"
                       EVALUATE    SPA-Q01-HKNNUM (IDX1)
                       WHEN    "JB0"
                           MOVE    " "         TO      WRK-JIBAIKBN1
                           MOVE    "0"         TO      WRK-JIBAIKBN2
                       WHEN    "JB1"
                           MOVE    "1"         TO      WRK-JIBAIKBN1
                           MOVE    "1"         TO      WRK-JIBAIKBN2
                       WHEN    "JB2"
                           MOVE    "2"         TO      WRK-JIBAIKBN1
                           MOVE    "2"         TO      WRK-JIBAIKBN2
                       END-EVALUATE
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-NOT             DELIMITED   BY  SPACE
                           "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM "          DELIMITED   BY  SIZE
                           "= '973' AND "      DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".JIBAIKBN IN ('"   DELIMITED   BY  SIZE
                           WRK-JIBAIKBN1       DELIMITED   BY  SPACE
                           "','"               DELIMITED   BY  SIZE
                           WRK-JIBAIKBN2       DELIMITED   BY  SPACE
                           "'))"               DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                       IF    ( SPA-Q01-HKNNUM-JKN  =   CONST-NOT )
                           STRING  "or ("          DELIMITED   BY  SIZE
                               WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                               ".HKNNUM "          DELIMITED   BY  SIZE
                               "is null"           DELIMITED   BY  SIZE
                               ")"                 DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                       STRING  ")"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    OTHER
                       STRING  "(("            DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM "          DELIMITED   BY  SIZE
                           WRK-FUGOU           DELIMITED   BY  SPACE
                           " '"                DELIMITED   BY  SIZE
                           SPA-Q01-HKNNUM (IDX1)
                                               DELIMITED   BY  SIZE
                           "')"                DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                       IF    ( SPA-Q01-HKNNUM-JKN    =   CONST-NOT )
                           STRING  " OR("        DELIMITED BY  SIZE
                               WRK-SQL-TBL-NAME  DELIMITED BY  SPACE
                               ".HKNNUM IS NULL )"
                                                 DELIMITED BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                       STRING  ")"               DELIMITED BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF      ( SPA-Q01-KIGO (IDX1)   NOT =   SPACE )
                OR     ( SPA-Q01-NUM  (IDX1)   NOT =   SPACE )
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                   STRING  "("           DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                   IF  ( SPA-Q01-KIGO (IDX1)   NOT =   SPACE )
      *                µ­¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                       MOVE    1               TO  WRK-STR-LEN
                       MOVE    SPACE           TO  WRK-STR
                       STRING  SPA-Q01-KIGO (IDX1) DELIMITED   BY  SIZE
                       INTO    WRK-STR
                       WITH POINTER WRK-STR-LEN
                       END-STRING
                       COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *                µ­¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                       MOVE    SPA-Q01-KIGO (IDX1) TO  WRK-STR
                       PERFORM 900-STR-LEN-GET-SEC
                       MOVE    WRK-STR-LEN         TO  WRK-KIGO-LEN
      *
                       STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".KIGO LIKE '"          DELIMITED   BY  SIZE
                           SPA-Q01-KIGO (IDX1)(1: WRK-KIGO-LEN)
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                       IF  ( SPA-Q01-NUM  (IDX1)   NOT =   SPACE )
                           STRING  "AND"           DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
      *
                   IF  ( SPA-Q01-NUM  (IDX1)   NOT =   SPACE )
      *                ÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                       MOVE    1               TO  WRK-STR-LEN
                       MOVE    SPACE           TO  WRK-STR
                       STRING  SPA-Q01-KIGO (IDX1) DELIMITED   BY  SIZE
                       INTO    WRK-STR
                       WITH POINTER WRK-STR-LEN
                       END-STRING
                       COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *                ÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                       MOVE    SPA-Q01-NUM (IDX1) TO  WRK-STR
                       PERFORM 900-STR-LEN-GET-SEC
                       MOVE    WRK-STR-LEN         TO  WRK-NUM-LEN
      *
                       STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                               ".NUM LIKE '"       DELIMITED   BY  SIZE
                               SPA-Q01-NUM  (IDX1)(1: WRK-NUM-LEN)
                                                   DELIMITED   BY  SIZE
                               "%')"               DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  ")"                     DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-PTHKNINF-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉéÃ´³ä¹ç£Ó£Ñ£ÌÊÔ½¸½èÍý
      *****************************************************************
       100111-FTNRATE-SQL-EDIT-SEC     SECTION.
      *
           MOVE    WRK-STSRYYMD        TO  WRK-AGE70DAY
           COMPUTE WRK-AGE70DAY-Y  =   WRK-AGE70DAY-Y - 70
           MOVE    "01"                TO  WRK-AGE70DAY-D
      *
           STRING  "("
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( SPA-Q01-FTNRATE     =   "1" OR "2" OR "3" )
               STRING
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HKNNUM BETWEEN '001'  AND '099' AND "
                                               DELIMITED   BY  SIZE
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HKNNUM NOT IN ('039','040','068','069') AND "
                                               DELIMITED   BY  SIZE
                       "A.BIRTHDAY <='"        DELIMITED   BY  SIZE
                       WRK-AGE70DAY            DELIMITED   BY  SPACE
                       "' AND "                DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HKNNUM IN ('039') AND "
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           EVALUATE    SPA-Q01-FTNRATE
           WHEN    "1"
               STRING
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HOJOKBN IN ('1','9','A','B','C')"
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HOJOKBN IN ('2','8','D','E','F')"
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HOJOKBN IN ('3','7','G','H','I')"
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "4"
               STRING
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HOJOKBN IN ('1')"
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "5"
               STRING
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HOJOKBN IN ('3')"
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           STRING  ")"
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       100111-FTNRATE-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¡Ê¸øÈñ¤Î¼ïÎà¡Ë¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *    ¡ÊOR,NOT¾ò·ïÍÑ¡Ë
      *****************************************************************
       10011-PTKOHINF-KOHNUM-EDIT-SEC  SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               OR      ( SPA-Q01-KOHNUM-JKN    =   CONST-NOT )
      *
               IF  ( SPA-Q01-KOHNUM (IDX1) NOT =   SPACE )
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   IF    ( CNT-JOKEN  =   1 )
                    OR   ( SPA-Q01-KOHNUM-JKN  =   "2" )
                       PERFORM 900-SQL-JOKEN-EDIT2-SEC
                   ELSE
                       PERFORM 900-SQL-JOKEN-EDIT9-SEC
                   END-IF
      *
      *            ÊÝ¸±ÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-KOHNUM (IDX1) 
                                           DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÊÝ¸±ÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-KOHNUM (IDX1)   TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN             TO  WRK-KIGO-LEN
      *
                   IF    ( SPA-Q01-KOHNUM (IDX1)   =   "027" )
                    AND  ( SPA-Q01-PAYKBN (IDX1)   =   "01" 
                                                        OR "02"
                                                        OR "03")
                       STRING  "(("            DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".KOHNUM ='"        DELIMITED   BY  SIZE
                           SPA-Q01-KOHNUM (IDX1)(1: WRK-KIGO-LEN)
                                               DELIMITED   BY  SIZE
                           "')AND("            DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".PAYKBN ='"        DELIMITED   BY  SIZE
                           SPA-Q01-PAYKBN (IDX1)
                                               DELIMITED   BY  SIZE
                           "'))"               DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   ELSE
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".KOHNUM ='"        DELIMITED   BY  SIZE
                           SPA-Q01-KOHNUM (IDX1)(1: WRK-KIGO-LEN)
                                               DELIMITED   BY  SIZE
                           "')"                DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( SPA-Q01-KOHNUM-JKN          =   CONST-NOT )
            AND  ( SPA-Q01-KOHNUM-G        NOT =   SPACE )
      *
               COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               STRING "(( "                    DELIMITED   BY  SIZE
                      WRK-SQL-TBL-NAME             DELIMITED   BY  SPACE
                      ".HOSPNUM,"                  DELIMITED   BY  SIZE
                      WRK-SQL-TBL-NAME             DELIMITED   BY  SPACE
                      ".PTID,"                     DELIMITED   BY  SIZE
                      WRK-SQL-TBL-NAME             DELIMITED   BY  SPACE
                      ".HKNCOMBINUM) NOT IN ("     DELIMITED   BY  SIZE
                      "SELECT HOSPNUM,PTID,HKNCOMBINUM"
                                                   DELIMITED   BY  SIZE
                      " FROM TMPV_Q001 WHERE KOHNUM IN ('@@@'"
                                                   DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   (   IDX1    >   5 )
      *
                   IF    ( SPA-Q01-KOHNUM (IDX1)   NOT =   SPACE )
                       STRING  ",'"                DELIMITED   BY  SIZE
                               SPA-Q01-KOHNUM (IDX1)
                                                   DELIMITED   BY  SPACE
                               "'"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
      *
               STRING ")))"                        DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-PTKOHINF-KOHNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¡Ê¸øÈñ¤Î¼ïÎà¡Ë¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *    ¡ÊAND¾ò·ïÍÑ¡Ë
      *****************************************************************
       10012-PTKOHINF-KOHNUM-EDIT-SEC  SECTION.
      *
           MOVE    1               TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *    ÊÝ¸±ÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
           MOVE    1               TO  WRK-STR-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  WRK-KOHNUM
                                           DELIMITED   BY  SIZE
           INTO    WRK-STR
           WITH POINTER WRK-STR-LEN
           END-STRING
           COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *    ÊÝ¸±ÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
           MOVE    WRK-KOHNUM      TO  WRK-STR
           PERFORM 900-STR-LEN-GET-SEC
           MOVE    WRK-STR-LEN             TO  WRK-KIGO-LEN
      *
           IF    ( WRK-KOHNUM          =   "027" )
            AND  ( WRK-PAYKBN          =   "01" OR "02" OR "03" )
               STRING  "(("            DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                   ".KOHNUM ='"        DELIMITED   BY  SIZE
                   WRK-KOHNUM (1: WRK-KIGO-LEN)
                                       DELIMITED   BY  SIZE
                   "')AND("            DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                   ".PAYKBN ='"        DELIMITED   BY  SIZE
                   WRK-PAYKBN          DELIMITED   BY  SIZE
                   "'))"               DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  "("             DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                   ".KOHNUM ='"        DELIMITED   BY  SIZE
                   WRK-KOHNUM (1: WRK-KIGO-LEN)
                                       DELIMITED   BY  SIZE
                   "')"                DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10012-PTKOHINF-KOHNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¡Ê¸øÈñ¤Î¼ïÎà¡Ë¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-PTKOHINF-FTNNUM-EDIT-SEC  SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-FTNJANUM  (IDX1)  NOT =   SPACE )
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *            ÉéÃ´¼ÔÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-FTNJANUM (IDX1)
                                               DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÉéÃ´¼ÔÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-FTNJANUM (IDX1) TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN         TO  WRK-NUM-LEN
      *
                   STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".FTNJANUM LIKE '"
                                               DELIMITED   BY  SIZE
                           SPA-Q01-FTNJANUM  (IDX1)(1: WRK-NUM-LEN)
                                               DELIMITED   BY  SIZE
                           "%')"               DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
      *
           .
       10011-PTKOHINF-FTNNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¡ÊÅ¬ÍÑ½ªÎ»Æü¡Ë¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-PTKOHINF-ETC-EDIT-SEC   SECTION.
      *
           IF    ( SPA-Q01-KOHEDYM     NOT =   SPACE )
      *
               MOVE    1       TO      CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               IF    ( FLG-TMPVQ001-SQL    =   1 )
                   STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".KOHEDYMD LIKE '"  DELIMITED   BY  SIZE
                           SPA-Q01-KOHEDYM     DELIMITED   BY  SIZE
                           "%')"               DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".TEKEDYMD LIKE '"  DELIMITED   BY  SIZE
                           SPA-Q01-KOHEDYM     DELIMITED   BY  SIZE
                           "%')"               DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           END-IF
      *
           .
       10011-PTKOHINF-ETC-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTHKNINF-SQL-EDIT-SEC      SECTION.
      *
           MOVE    "D"             TO  WRK-SQL-TBL-NAME
      *
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
      *    ÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
           PERFORM 10011-PTHKNINF-SQL-EDIT-SEC
      *
           STRING "AND (D.DELKBN = ' ')  "
           DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-PTHKNINF-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý(OR)
      *****************************************************************
       1001-PTKOHINF-SQL-OR-EDIT-SEC   SECTION.
      *
           MOVE    "E"                 TO  WRK-SQL-TBL-NAME
      *
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
      *    ¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
           PERFORM 10011-PTKOHINF-KOHNUM-EDIT-SEC
           PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
           PERFORM 10011-PTKOHINF-ETC-EDIT-SEC
      *
           STRING "AND (E.SAKJOKBN = ' ')  "
           DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-PTKOHINF-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý(AND)
      *****************************************************************
       1001-PTKOHINF-SQL-AND-EDIT-SEC   SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
                   MOVE    ZERO            TO  FLG-JOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "E"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
      *            ´µ¼Ô¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
                   MOVE    SPA-Q01-KOHNUM (IDXA)
                                           TO  WRK-KOHNUM
                   MOVE    SPA-Q01-PAYKBN (IDXA)
                                           TO  WRK-PAYKBN
                   PERFORM 10012-PTKOHINF-KOHNUM-EDIT-SEC
                   PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
                   PERFORM 10011-PTKOHINF-ETC-EDIT-SEC
                   MOVE    1           TO  CNT-JOKEN
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
                   STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".SAKJOKBN = ' ')"  DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
      *
                   PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
                   PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JOKEN
      *
               END-IF
      *
           END-PERFORM
      *
           .
       1001-PTKOHINF-SQL-AND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-JYURRK-SQL-EDIT-SEC        SECTION.
      *
           MOVE    "F"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           PERFORM 10011-UPYMD-SQL-EDIT-SEC
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-DRCD      NOT =   SPACE )
               PERFORM 10011-DRCD-SQL-EDIT-SEC
           END-IF
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *    ºÇ½ª¼õ¿ÇÆü¸¡º÷¾ò·ïÊÔ½¸
           STRING  "(F.SRYYMD >= '"            DELIMITED   BY  SIZE
                   WRK-STSRYYMD                DELIMITED   BY  SIZE
                   "') AND (F.SRYYMD <= '"     DELIMITED   BY  SIZE
                   WRK-EDSRYYMD                DELIMITED   BY  SIZE
                   "')"                        DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-JYURRK-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊOR¡Ë
      *****************************************************************
       1001-SRYACT-SQL-OR-EDIT-SEC     SECTION.
      *
           MOVE    "G"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
           INITIALIZE          WRK-SRYCD-G
           MOVE    SPA-Q01-SRYCD (1)       TO  WRK-SRYCD (1)
           MOVE    SPA-Q01-SRYCD (2)       TO  WRK-SRYCD (2)
           MOVE    SPA-Q01-SRYCD (3)       TO  WRK-SRYCD (3)
           MOVE    SPA-Q01-SRYCD (4)       TO  WRK-SRYCD (4)
           MOVE    SPA-Q01-SRYCD (5)       TO  WRK-SRYCD (5)
           PERFORM 10011-SRYACT-SQL-OR-EDIT-SEC
      *
           PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
           .
       1001-SRYACT-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊOR¡Ë
      *****************************************************************
       10011-SRYACT-SQL-OR-EDIT-SEC    SECTION.
      *
           IF    ( SPA-Q01-GJYURRK =   SPACE )
               CONTINUE
           ELSE
               STRING "(("                 DELIMITED BY SIZE
                      WRK-SQL-TBL-NAME     DELIMITED BY SPACE
                      ".SRYYMD <= '"       DELIMITED BY SIZE
                      WRK-EDSRYYMD         DELIMITED BY SIZE
                      "') AND ("           DELIMITED BY SIZE
                      WRK-SQL-TBL-NAME     DELIMITED BY SPACE
                      ".SRYYMD >= '"       DELIMITED BY SIZE
                      WRK-STSRYYMD         DELIMITED BY SIZE
                      "')) AND"            DELIMITED BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 10011-UPYMD-SQL-EDIT-SEC
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKBN  NOT =   SPACE )
               PERFORM 10011-SRYKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-HOKATSUKBN  =   "1" )
               PERFORM 10011-HOKATSUKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-GHKNINF     =   "1" )
               PERFORM 10011-HKNCOMBINUM-SQL-EDIT-SEC
           END-IF
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF      ( WRK-SRYCD (IDX1)  NOT =   SPACE )
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                   STRING  "('"                    DELIMITED   BY  SIZE
                           WRK-SRYCD (IDX1)        DELIMITED   BY  SIZE
                           "')IN("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".SRYCD1,"              DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".SRYCD2,"              DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".SRYCD3,"              DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".SRYCD4,"              DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".SRYCD5)"              DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-SRYACT-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊAND¡Ë
      *****************************************************************
       1001-SRYACT-SQL-AND-EDIT-SEC    SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
               UNTIL   (   IDXA    >   5 )
      *
               IF      ( SPA-Q01-SRYCD     (IDXA)  NOT =   SPACE )
      *
                   MOVE    ZERO            TO  FLG-JOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "G"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
                   PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
                   IF    ( SPA-Q01-GJYURRK =   SPACE )
                       CONTINUE
                   ELSE
                       STRING "(("                 DELIMITED BY SIZE
                              WRK-SQL-TBL-NAME     DELIMITED BY SPACE
                              ".SRYYMD <= '"       DELIMITED BY SIZE
                              WRK-EDSRYYMD         DELIMITED BY SIZE
                              "') AND ("           DELIMITED BY SIZE
                              WRK-SQL-TBL-NAME     DELIMITED BY SPACE
                              ".SRYYMD >= '"       DELIMITED BY SIZE
                              WRK-STSRYYMD         DELIMITED BY SIZE
                              "')) AND"            DELIMITED BY SIZE
                       INTO   WRK-SQL
                       WITH POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   PERFORM 10011-UPYMD-SQL-EDIT-SEC
      *
                   IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                       PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                       PERFORM 10011-SRYKA-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKBN  NOT =   SPACE )
                       PERFORM 10011-SRYKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-HOKATSUKBN  =   "1" )
                       PERFORM 10011-HOKATSUKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-GHKNINF     =   "1" )
                       PERFORM 10011-HKNCOMBINUM-SQL-EDIT-SEC
                   END-IF
      *
                   MOVE    1               TO      CNT-JOKEN
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                   STRING  "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD1 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD2 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD3 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD4 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD5 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "')"                    DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                   PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
                   PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JOKEN
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-Q01-SRYVALITEM  NOT =   SPACE )
      *
               MOVE    ZERO            TO  FLG-JOKEN-AND
      *
               MOVE    "G"             TO  WRK-SQL-TBL-NAME
               PERFORM 900-SQL-JOKEN-EDIT1-SEC
      *
               MOVE    1               TO      CNT-JOKEN
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               STRING    "G.OID IN (null"   DELIMITED BY SIZE
               INTO      WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL   (   IDXA    >   5 )
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "G"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   IF      ( SPA-Q01-SRYCD     (IDXA)  NOT =   SPACE )
                       STRING    ","               DELIMITED BY SPACE
                                 WRK-SQL-TBL-NAME  DELIMITED BY SPACE
                                 ".OID"
                       INTO      WRK-SQL
                       WITH POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
               END-PERFORM
      *
               STRING    ")"                       DELIMITED BY SIZE
               INTO      WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
               PERFORM 900-SQL-JOKEN-EDIT4-SEC
      *
               MOVE    1               TO  FLG-JOKEN
      *
           END-IF
      *
           .
       1001-SRYACT-SQL-AND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÈÖ¹æ¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTNUM-SQL-EDIT-SEC         SECTION.
      *
           IF  ( FLG-JOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "((H.HOSPNUM="              DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   "H.PTID=A.PTID) "   DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           IF  ( SPA-Q01-GPTNUM    =   "1" )
      *
               IF    ( SPA-Q01-STPTNUM     NOT =   SPACE )
                   STRING  "AND (H.PTNUM>='"   DELIMITED  BY  SIZE
                           SPA-Q01-STPTNUM     DELIMITED  BY  SIZE
                           "')"                DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               IF    ( SPA-Q01-EDPTNUM     NOT =   SPACE )
                   STRING  "AND (H.PTNUM<='"   DELIMITED  BY  SIZE
                           SPA-Q01-EDPTNUM     DELIMITED  BY  SIZE
                           "')"                DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
           END-IF
      *
           STRING  ")"         DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       1001-PTNUM-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´Á»ú»áÌ¾¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-NAME-SELECT-EDIT-SEC           SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-NAME (IDX1)   NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *            ¥«¥Ê»áÌ¾¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-NAME (IDX1)   DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ¥«¥Ê»áÌ¾¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-NAME (IDX1)   TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   STRING  "(A.NAME LIKE '"        DELIMITED   BY  SIZE
                           SPA-Q01-NAME (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%' OR A.KANANAME LIKE '"
                           SPA-Q01-NAME (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-NAME-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    À¸Ç¯·îÆü¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-BIRTH-SELECT-EDIT-SEC          SECTION.
      *
           IF      ( SPA-Q01-STBIRTHDAY    =   SPACE )
            AND    ( SPA-Q01-EDBIRTHDAY    =   SPACE )
               GO  TO  10011-BIRTH-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           STRING  "(BIRTHDAY >= '"        DELIMITED   BY  SIZE
                   WRK-STBIRTHDAY          DELIMITED   BY  SIZE
                   "') AND "               DELIMITED   BY  SIZE
                   "(BIRTHDAY  <= '"       DELIMITED   BY  SIZE
                   WRK-EDBIRTHDAY          DELIMITED   BY  SIZE
                   "')"                    DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
      *
           .
       10011-BIRTH-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÃÂÀ¸·î¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-BIRTHMON-SELECT-EDIT-SEC          SECTION.
      *
           IF      ( SPA-Q01-BIRTHMON      =   "00" )
               GO  TO  10011-BIRTHMON-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           STRING  "(SUBSTR(BIRTHDAY,5,2) = '" DELIMITED   BY  SIZE
                   SPA-Q01-BIRTHMON            DELIMITED   BY  SIZE
                   "')"                        DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
      *
           .
       10011-BIRTHMON-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    À­ÊÌ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-SEX-SELECT-EDIT-SEC            SECTION.
      *
           IF      ( SPA-Q01-SEX       =   SPACE )
               GO  TO  10011-SEX-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           STRING  "(SEX = '"       DELIMITED   BY  SIZE
                   SPA-Q01-SEX      DELIMITED   BY  SIZE
                   "')"             DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-SEX-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ½»½ê¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-HOME-ADRS-SELECT-EDIT-SEC      SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-JYUSHO (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *            ½»½ê¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-JYUSHO (IDX1)   DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ½»½ê¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-JYUSHO (IDX1)   TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   STRING  "(HOME_ADRS || HOME_BANTI LIKE '"
                                                   DELIMITED   BY  SIZE
                           SPA-Q01-JYUSHO (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-HOME-ADRS-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾õÂÖ£±¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-CONDITION1-SEL-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-CONDITION1 (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JOKEN  =   1 )
                       STRING  "CONDITION_1 IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-CONDITION1 (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-CONDITION1-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾õÂÖ£²¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-CONDITION2-SEL-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-CONDITION2 (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JOKEN  =   1 )
                       STRING  "CONDITION_2 IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-CONDITION2 (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-CONDITION2-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾õÂÖ£³¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-CONDITION3-SEL-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-CONDITION3 (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JOKEN  =   1 )
                       STRING  "CONDITION_3 IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-CONDITION3 (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-CONDITION3-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸ºÌÈ»öÍ³¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-GENMEN-SEL-EDIT-SEC       SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-GENMEN (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JOKEN  =   1 )
                       STRING  "DISCOUNT IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-GENMEN (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-GENMEN-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¥³¥á¥ó¥È¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-COMMENT-SELECT-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-COMMENT (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                   PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
      *            ¥³¥á¥ó¥È¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-COMMENT (IDX1)  DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ¥³¥á¥ó¥È¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-COMMENT (IDX1)  TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   IF    ( SPA-Q01-SELCOMMENT      =   "1" )
                       STRING  "(COMMENT_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           " (COMMENT_2 LIKE '"    DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   ELSE
                       STRING  "(TABOO_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(TABOO_2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(ALLERGY_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(ALLERGY_2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(KANSENSYO_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(KANSENSYO_2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
      *
           .
       10011-COMMENT-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÅÅÏÃÈÖ¹æ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-TEL-SELECT-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-TEL (IDX1) NOT =   SPACE)
      *
      *            ÅÅÏÃÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-TEL (IDX1)  DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÅÅÏÃÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-TEL (IDX1)  TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   INSPECT SPA-Q01-TEL (IDX1)  TALLYING
                           CNT-STR         FOR ALL "%"
      *
                   IF    ( SPA-Q01-TEL-TO-HOME1    =   1 )
      *
                       COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                       PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                       STRING  "(HOME_TEL1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-TEL (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
      *
                       IF    ( CNT-STR         =   ZERO )
                           STRING  "%')"           DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       ELSE
                           STRING  "')"            DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
      *
                   IF    ( SPA-Q01-TEL-TO-HOME2    =   1 )
      *
                       COMPUTE CNT-JOKEN  =   CNT-JOKEN  +   1
      *
                       PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
                       STRING  "(HOME_TEL2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-TEL (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
      *
                       IF    ( CNT-STR         =   ZERO )
                           STRING  "%')"           DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       ELSE
                           STRING  "')"            DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
      *
           .
       10011-TEL-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    »àË´¶èÊ¬¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-DEATH-SELECT-EDIT-SEC         SECTION.
      *
      *
           IF      ( SPA-Q01-DEATH         =   SPACE )
               GO  TO  10011-DEATH-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-DEATH    =  "0" )
               STRING  "(DEATHKBN   <>  '1')"   DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  "(DEATHKBN   =  '1')"   DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
      *
           .
       10011-DEATH-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ÊÊÔ½¸½èÍý
      *****************************************************************
       10011-SRYKA-SQL-EDIT-SEC            SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( FLG-RECECOM-SQL     =   1 )
               MOVE    ZERO            TO  FLG-RECECOM-SQL
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".SRYKA IN ('00','" DELIMITED BY SIZE
                       SPA-Q01-SRYKA       DELIMITED BY SIZE
                       "')"                DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".SRYKA ='"         DELIMITED BY SIZE
                       SPA-Q01-SRYKA       DELIMITED BY SIZE
                       "'"                 DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-SRYKA-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¥É¥¯¥¿¡¼ÊÔ½¸½èÍý
      *****************************************************************
       10011-DRCD-SQL-EDIT-SEC            SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".DRCD ='"          DELIMITED BY SIZE
                   SPA-Q01-DRCD        DELIMITED BY SIZE
                       "'"             DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-DRCD-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-SRYKBN-SQL-EDIT-SEC           SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           EVALUATE    SPA-Q01-SRYKBN
           WHEN    "20"
           WHEN    "30"
               IF   ( SPA-Q01-SRYSYUKBN    NOT =   SPACE )
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN ='"        DELIMITED BY SIZE
                           SPA-Q01-SRYSYUKBN   DELIMITED BY SIZE
                           "'"                 DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN LIKE'"     DELIMITED BY SIZE
                           SPA-Q01-SRYKBN(1:1) DELIMITED BY SIZE
                           "%'"                DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           WHEN    "90"
               IF   ( SPA-Q01-SRYSYUKBN    NOT =   SPACE )
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN ='"        DELIMITED BY SIZE
                           SPA-Q01-SRYSYUKBN   DELIMITED BY SIZE
                           "'"                 DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN in ('90','92')"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           WHEN    "80"
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN='80' AND " DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYSYUKBN NOT IN ('830','840')"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           WHEN    "83"
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN='80' AND " DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYSYUKBN='830'"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           WHEN    "84"
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN='80' AND " DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYSYUKBN='840'"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           WHEN    OTHER
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN ='"        DELIMITED BY SIZE
                           SPA-Q01-SRYKBN      DELIMITED BY SIZE
                           "'"                 DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           END-EVALUATE
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-SRYKBN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Êñ³ç¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-HOKATSUKBN-SQL-EDIT-SEC           SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".HOKATSUKBN =''"   DELIMITED BY SIZE
                   "AND "              DELIMITED BY SIZE
                   WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".HKNCOMBINUM <> 9999"
                                       DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-HOKATSUKBN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       10011-HKNCOMBINUM-SQL-EDIT-SEC  SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( FLG-HKNSELPTN           =   2 )
               STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                       ".HKNCOMBI in (0"  DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDX1)    NOT =   SPACE )
                       MOVE    IDX1    TO  WRK-NAME-SEQ
                       STRING  ",D"            DELIMITED  BY  SIZE
                               WRK-NAME-SEQ    DELIMITED  BY  SIZE
                               ".HKNCOMBINUM"  DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
               END-PERFORM
               STRING  ")"     DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                       ".HKNCOMBINUM = D.HKNCOMBINUM"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-HKNCOMBINUM-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       10012-HKNCOMBINUM-SQL-EDIT-SEC  SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( FLG-HKNSELPTN           =   2 )
               STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                       ".SRYSEQ   in (0"  DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDX1)    NOT =   SPACE )
                       MOVE    IDX1    TO  WRK-NAME-SEQ
                       STRING  ",D"            DELIMITED  BY  SIZE
                               WRK-NAME-SEQ    DELIMITED  BY  SIZE
                               ".HKNCOMBINUM"  DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
               END-PERFORM
               STRING  ")"     DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                       ".SRYSEQ = D.HKNCOMBINUM"
                                               DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10012-HKNCOMBINUM-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    UPYMDÊÔ½¸½èÍý
      * (TERMID¤À¤È¸¡º÷¤Ë»þ´Ö¤¬¤«¤«¤ë¤Î¤Ç¡¢UPYMD,UPHMS¤ÇÂåÍÑ¤¹¤ë¡Ë
      *****************************************************************
       10011-UPYMD-SQL-EDIT-SEC        SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".UPYMD ='"         DELIMITED BY SIZE
                   QSUB02-YMD          DELIMITED BY SIZE
                   "' AND "            DELIMITED BY SIZE
                   WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".UPHMS ='"         DELIMITED BY SIZE
                   QSUB02-HMS          DELIMITED BY SIZE
                   "' "                DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-UPYMD-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ³°¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-NYUGAIKBN-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( FLG-PTBYOMEI-SQL    =   1 )
      *
               MOVE    ZERO            TO  FLG-PTBYOMEI-SQL
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".NYUGAIKBN IN (' ','"
                                           DELIMITED BY SIZE
                       SPA-Q01-NYUGAIKBN   DELIMITED BY SIZE
                       "')"                DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".NYUGAIKBN ='"     DELIMITED BY SIZE
                       SPA-Q01-NYUGAIKBN   DELIMITED BY SIZE
                       "'"                 DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-NYUGAIKBN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾µ¿¤¤¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       10011-UTAGAIFLG-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-UTAGAIKBN   =   "1" )
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG IN ('1','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG NOT IN ('1','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-UTAGAIFLG-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾µ¿¤¤¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       10012-UTAGAIFLG-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-KYUSEIKBN   =   "1" )
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG IN ('2','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG NOT IN ('2','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10012-UTAGAIFLG-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾¼çÉÂÌ¾¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       10012-SYUBYOFLG-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-SYUBYOKBN   =   "0" )
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".SYUBYOFLG = ' '"      DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".SYUBYOFLG = '1'"      DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10012-SYUBYOFLG-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾Å¾µ¢¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-TENKIKBN-SQL-EDIT-SEC     SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           EVALUATE    SPA-Q01-TENKIKBN
           WHEN    "0"
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIKBN = ' '"   DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
           WHEN    "9"
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIKBN <> ' '"  DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
           WHEN    OTHER
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIKBN = '"     DELIMITED BY SIZE
                       SPA-Q01-TENKIKBN    DELIMITED BY SIZE
                       "'"                 DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
           END-EVALUATE
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           IF    ( SPA-Q01-TENKIKBN    =   "1"
                                        OR "2"
                                        OR "3"
                                        OR "8"
                                        OR "9"
                                               )
      *
               PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
               STRING  "("                 DELIMITED BY SIZE
                       WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIYMD >= '"    DELIMITED BY SIZE
                       WRK-STTENKIYMD      DELIMITED BY SIZE
                       "') AND ("          DELIMITED BY SIZE
                       WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIYMD  <= '"   DELIMITED BY SIZE
                       WRK-EDTENKIYMD      DELIMITED BY SIZE
                       "')"                DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           END-IF
      *
           .
       10011-TENKIKBN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾¿ÇÎÅ³«»ÏÆüÊÔ½¸½èÍý
      *****************************************************************
       10011-BYOYMD-SQL-EDIT-SEC       SECTION.
      *
           MOVE    1                   TO  CNT-JOKEN
      *
           PERFORM 900-SQL-JOKEN-EDIT2-SEC
      *
           STRING  "("                 DELIMITED BY SIZE
                   WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".SRYYMD >= '"      DELIMITED BY SIZE
                   WRK-STBYOYMD        DELIMITED BY SIZE
                   "') AND ("          DELIMITED BY SIZE
                   WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".SRYYMD <= '"      DELIMITED BY SIZE
                   WRK-EDBYOYMD        DELIMITED BY SIZE
                   "')"                DELIMITED BY SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JOKEN-EDIT3-SEC
      *
           .
       10011-BYOYMD-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    GROUP BYÊÔ½¸½èÍý(ÄÌ¾ï¸¡º÷)
      *****************************************************************
       1001-GROUP-EDIT-SEC                 SECTION.
      *
           STRING  " GROUP BY "        DELIMITED   BY  SIZE
                   "HOSPNUM,"          DELIMITED   BY  SIZE
                   "PTID,"             DELIMITED   BY  SIZE
                   "PTNUM,"            DELIMITED   BY  SIZE
                   "KANANAME,"         DELIMITED   BY  SIZE
                   "SEX,"              DELIMITED   BY  SIZE
                   "BIRTHDAY"          DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-GROUP-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    GROUP BYÊÔ½¸½èÍý(ÅÀ¿ô¸¡º÷)
      *****************************************************************
       1002-GROUP-EDIT-SEC                 SECTION.
      *
           STRING  " GROUP BY "        DELIMITED   BY  SIZE
                   "HOSPNUM,"          DELIMITED   BY  SIZE
                   "PTID,"             DELIMITED   BY  SIZE
                   "PTNUM,"            DELIMITED   BY  SIZE
                   "KANANAME,"         DELIMITED   BY  SIZE
                   "SEX,"              DELIMITED   BY  SIZE
                   "BIRTHDAY,"         DELIMITED   BY  SIZE
                   "NYUGAIKBN,"        DELIMITED   BY  SIZE
                   "HKNNUM,"           DELIMITED   BY  SIZE
                   "KOHNUM1,"          DELIMITED   BY  SIZE
                   "KOHNUM2,"          DELIMITED   BY  SIZE
                   "TOTALTEN,"         DELIMITED   BY  SIZE
                   "SEQNUM,"           DELIMITED   BY  SIZE
                   "OID"               DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1002-GROUP-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÂ¤ÙÂØ¤¨½çÊÔ½¸½èÍý(¿ÇÎÅ¹Ô°Ù¸¡º÷)
      *****************************************************************
       1001-ORDER-EDIT-SEC                 SECTION.
      *
           MOVE    SPA-Q01-SORTITEM (1)    TO  IDX-SITM1
           MOVE    SPA-Q01-SORTITEM (2)    TO  IDX-SITM2
           MOVE    SPA-Q01-SORTITEM (3)    TO  IDX-SITM3
      *
           MOVE    SPA-Q01-SORTKBN  (1)    TO  IDX-SRT1
           MOVE    SPA-Q01-SORTKBN  (2)    TO  IDX-SRT2
           MOVE    SPA-Q01-SORTKBN  (3)    TO  IDX-SRT3
      *
           EVALUATE    SPA-Q01-OUTKBN
           WHEN    "3"
      *
               STRING  " ORDER BY "                DELIMITED   BY  SIZE
                       CONST-SKMK-ITEM (IDX-SITM1) DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT1)
                                                   DELIMITED   BY  SPACE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
      *
               MOVE    CONST-SKMK-ITEM (IDX-SITM1)
                                           TO      WRK-SORT-ITEM
               PERFORM 10011-ORDER-EDIT-SEC
      *
               STRING  ","                         DELIMITED   BY  SIZE
                       CONST-SKMK-ITEM (IDX-SITM2) DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT2)
                                                   DELIMITED   BY  SPACE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
      *
               MOVE    CONST-SKMK-ITEM (IDX-SITM2)
                                           TO      WRK-SORT-ITEM
               PERFORM 10011-ORDER-EDIT-SEC
      *
               STRING  ","                         DELIMITED   BY  SIZE
                       CONST-SKMK-ITEM (IDX-SITM3) DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT3)
                                                   DELIMITED   BY  SPACE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
      *
               MOVE    CONST-SKMK-ITEM (IDX-SITM2)
                                           TO      WRK-SORT-ITEM
               PERFORM 10011-ORDER-EDIT-SEC
      *
               STRING  ",SRYCD  ASC,SUBSTR(MEMO,1,6)  ASC"
                                                   DELIMITED   BY  SIZE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
           WHEN    "2"
      *
               STRING  " ORDER BY "                DELIMITED   BY  SIZE
                       CONST-TKMK-ITEM (IDX-SITM1) DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT1)
                                                   DELIMITED   BY  SPACE
                       ","                         DELIMITED   BY  SIZE
                       CONST-TKMK-ITEM (IDX-SITM2) DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT2)
                                                   DELIMITED   BY  SPACE
                       ","                         DELIMITED   BY  SIZE
                       CONST-TKMK-ITEM (IDX-SITM3) DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT3)
                                                   DELIMITED   BY  SPACE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
           WHEN  OTHER
      *
               STRING  " ORDER BY "                DELIMITED   BY  SIZE
                       CONST-KMK-ITEM (IDX-SITM1)  DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT1)
                                                   DELIMITED   BY  SPACE
                       ","                         DELIMITED   BY  SIZE
                       CONST-KMK-ITEM (IDX-SITM2)  DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT2)
                                                   DELIMITED   BY  SPACE
                       ","                         DELIMITED   BY  SIZE
                       CONST-KMK-ITEM (IDX-SITM3)  DELIMITED   BY  SPACE
                       " "                         DELIMITED   BY  SIZE
                       CONST-ORDER (IDX-SRT3)
                                                   DELIMITED   BY  SPACE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
           END-EVALUATE
      *
           .
      *
       1001-ORDER-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÂ¤ÙÂØ¤¨½çÊÔ½¸½èÍý(¿ÇÎÅ¹Ô°Ù¸¡º÷)
      *****************************************************************
       10011-ORDER-EDIT-SEC                 SECTION.
      *
           IF    ( WRK-SORT-ITEM            =  "SRYCD" OR "srycd" )
               STRING    ", SUBSTR(MEMO,1,6)  ASC" DELIMITED   BY  SIZE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
           END-IF
      *
           .
      *
       10011-ORDER-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£±
      *****************************************************************
       900-SQL-JOKEN-EDIT1-SEC            SECTION.
      *
           IF  ( FLG-JOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "(("                        DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".HOSPNUM="                 DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".PTID=A.PTID) "            DELIMITED   BY  SIZE
                   "AND "                      DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JOKEN-EDIT1-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£²
      *****************************************************************
       900-SQL-JOKEN-EDIT2-SEC            SECTION.
      *
           IF  ( CNT-JOKEN    =   1 )
               IF  (   FLG-JOKEN-AND  =   "1" )
                   STRING  " AND "         DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               STRING  "("                 DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  "OR "               DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       900-SQL-JOKEN-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£³
      *****************************************************************
       900-SQL-JOKEN-EDIT3-SEC            SECTION.
      *
           IF  ( CNT-JOKEN    >   0 )
               STRING  ")"      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
               MOVE    1           TO  FLG-JOKEN-AND
           END-IF
      *
           .
       900-SQL-JOKEN-EDIT3-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£´
      *****************************************************************
       900-SQL-JOKEN-EDIT4-SEC            SECTION.
      *
           STRING  ")"         DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JOKEN-EDIT4-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£µ
      *****************************************************************
       900-SQL-JOKEN-EDIT5-SEC            SECTION.
      *
           IF  ( FLG-JOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "("             DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER         WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JOKEN-EDIT5-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£¶
      *****************************************************************
       900-SQL-JOKEN-EDIT6-SEC            SECTION.
      *
           IF  ( FLG-JOKEN-OR     =   1 )
               STRING " OR"        DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           ELSE
               MOVE    1           TO  FLG-JOKEN-OR
           END-IF
      *
           STRING  "("             DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JOKEN-EDIT6-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£·
      *****************************************************************
       900-SQL-JOKEN-EDIT7-SEC            SECTION.
      *
           IF  ( CNT-JOKEN    =   1 )
               IF  (   FLG-JOKEN-AND  =   "1" )
                   STRING  " AND "         DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           ELSE
               STRING  ","                 DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       900-SQL-JOKEN-EDIT7-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£¸
      *****************************************************************
       900-SQL-JOKEN-EDIT8-SEC            SECTION.
      *
           IF  ( FLG-JOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "(("                        DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".HOSPNUM="                 DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".PTID=A.PTID) "            DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JOKEN-EDIT8-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£¹
      *****************************************************************
       900-SQL-JOKEN-EDIT9-SEC                SECTION.
      *
           STRING  "AND "              DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JOKEN-EDIT9-EXT.
           EXIT.
      *****************************************************************
      *    Ê¸»úÎóÄ¹¼èÆÀ½èÍý
      *****************************************************************
       900-STR-LEN-GET-SEC                 SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
               UNTIL   ( IDX-LEN   <   1)
                OR     ( WRK-STR (IDX-LEN : 1) NOT =   SPACE)
      *
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN     TO  WRK-STR-LEN
      *
           .
       900-STR-LEN-GET-EXT.
           EXIT.
