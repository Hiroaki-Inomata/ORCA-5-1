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
       PROGRAM-ID.             ORCSSKYGET.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÊÝ¸±¼ÔÉéÃ´Åù¤ÎÀÁµá³Û¼è¤ê¹þ¤ß¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.05.01    NACL-ÌçÏÆ    11/06/29  ¥Æ¥¹¥È´µ¼Ô¤òÂÐ¾Ý³°¤È¤¹¤ë½¤Àµ
      *  04.05.02    NACL-ÌçÏÆ    11/11/09  Æ±ÆüºÆÆþ±¡¿©»öÂÐ±þ
      *  04.07.01    NACL-ÌçÏÆ    12/10/12  ÊÝ¸±ÊÌÀÁµá¥Á¥§¥Ã¥¯É½¡ÊÉ¸½àÄ¢É¼¡Ë
      *                                     ¤Îµ¡Ç½ÄÉ²ÃÂÐ±þ
      *  04.07.02    NACL-ÌçÏÆ    13/10/15  ¼«ÇåÀÕ¡ÊÂè»°¼Ô¹Ô°Ù¡ËÂÐ±þ
      *  04.08.01    NACL-ÌçÏÆ    15/08/27  ¼«ÇåÀÕ¥ì¥»¥×¥È¤ÎÀÁµá¶â³ÛÍóÃ¼¿ô
      *                                     ½èÍýÂÐ±þ
      *  04.08.02    NACL-ÌçÏÆ    16/09/28  ¼«ÇåÀÕ¡ÊÂè»°¼Ô¹Ô°Ù¡Ë
      *                                     ÊÝ¸±ÊÌÀÁµá¥Á¥§¥Ã¥¯É½¤Îµ¡Ç½ÄÉ²Ã
      *  04.08.03    NACL-ÌçÏÆ    17/03/17  ÊÝ¸±ÊÌÀÁµá¥Á¥§¥Ã¥¯É½¤Îµ¡Ç½ÄÉ²Ã
      *                                     ¡Ê¸øÈñÀÁµá½ñ¤Ë´Ø¤ï¤ëÂÐ±þ¡Ë
      *
      *  05.00.01    NACL-ÌçÏÆ    19/02/13  ¸øÈñÀÁµá½ñ¤Ë´Ø¤ï¤ëÂÐ±þ½¤Àµ
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
           03  FLG-KOHSKY                PIC 9(01).
           03  FLG-SYUNOU                PIC 9(01).
           03  FLG-SYUKEI                PIC 9(01).
           03  FLG-JIHIRANSYUKEI         PIC 9(01).
           03  FLG-PTINF                 PIC 9(01).
           03  FLG-OK                    PIC 9(01).
           03  FLG-TAISYO                PIC 9(01).
           03  FLG-TAISYO2               PIC 9(01).
           03  FLG-TAISYO3               PIC 9(01).
           03  FLG-SEIKYU-HENSYU-NG      PIC 9(01).
           03  FLG-KOHSKY-HENSYU-NG      PIC 9(01).
           03  FLG-KOHSKY-NASHI          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX1                      PIC 9(03).
           03  IDX2                      PIC 9(01).
           03  IDX3                      PIC 9(02).
      *
      *    ¥ï¡¼¥¯ÎÎ°è
       01  WRK-AREA.
           03  WRK-KEISAN                PIC S9(09).
           03  WRK-KEISAN2               PIC S9(09).
           03  WRK-KEISAN3               PIC  9(09).
      *
           03  WRK-TAIHI-AREA.
             05  WRK-TAIHI-PTID          PIC 9(10).
             05  WRK-TAIHI-HKNCOMBINUM   PIC X(04).
      *
           03  WRK-TAIHI-AREA2.
             05  WRK-TAIHI-DAY           PIC 9(01)   OCCURS   31.
      *
           03  WRK-PTFTNRATE             PIC 9(03).
      *
           03  WRK-DAISAN-DATA.
               05  WRK-DAISAN-FTNMONEY   PIC 9(09).
           03  WRK-DAISAN-DATA2.
               05  WRK-DAISAN-SKJFTN     PIC 9(06).
      *
           03  WRK-KOHSKY-DATA.
               05  WRK-KOHSKY-KOHFTN     PIC 9(09).
           03  WRK-KOHSKY-DATA2.
               05  WRK-KOHSKY-SKJFTN     PIC 9(06).
      *
           03  WRK-SKYIRYO-KOHFTN-KOHSKY PIC 9(09).
           03  WRK-SKYSKJ-KOHFTN-KOHSKY  PIC 9(06).
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ¸øÈñÀÁµá
           COPY    "CPKOHSKY.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
           COPY    "CPORCSAGECHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSKYGET.INC".
      *
           COPY    "CPRCF010.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSSKYGETAREA
           RECE10-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO    TO    LNK-SKYGET-RC
           INITIALIZE            LNK-SKYGET-OUT-AREA
           INITIALIZE            LNK-SKYGET-OUT2-AREA
      *
           INITIALIZE            FLG-AREA
           INITIALIZE            IDX-AREA
           INITIALIZE            WRK-AREA
      *
      *    ¥Ñ¥é¥á¥¿¥Á¥§¥Ã¥¯
           PERFORM   100-PRM-CHK-SEC
      *
           IF    LNK-SKYGET-RC    =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
           EVALUATE    LNK-SKYGET-KBN
      *      ¡ã¼ÒÊÝ¡¦¹ñÊÝ¡¦¸å´ü¹âÎð¤ÎÀÁµá´ÉÍý¥Ç¡¼¥¿Ã±°Ì¤ÎÊÔ½¸¡ä
             WHEN      "1"
      *      ¡ãÏ«ºÒ¤ÎÀÁµá´ÉÍý¥Ç¡¼¥¿Ã±°Ì¤ÎÊÔ½¸¡ä
             WHEN      "3"
      *      ¡ã¼«ÇåÀÕ¤ÎÀÁµá´ÉÍý¥Ç¡¼¥¿Ã±°Ì¤ÎÊÔ½¸¡ä
             WHEN      "4"
      *      ¡ã¸ø³²¤ÎÀÁµá´ÉÍý¥Ç¡¼¥¿Ã±°Ì¤ÎÊÔ½¸¡ä
             WHEN      "5"
      *      ¡ãÂè»°¼Ô¹Ô°Ù¤ÎÀÁµá´ÉÍý¥Ç¡¼¥¿Ã±°Ì¤ÎÊÔ½¸¡ä
             WHEN      "6"
      *      ¡ãÆÃÊÌÎÅÍÜÈñ¤ÎÀÁµá´ÉÍý¥Ç¡¼¥¿Ã±°Ì¤ÎÊÔ½¸¡ä
             WHEN      "7"
               IF      LNK-SKYGET-SRYYM  <  201004
                      MOVE    1     TO   LNK-SKYGET-RC
               ELSE
                 IF    RECE10-REC        =  SPACE
                      MOVE    2     TO   LNK-SKYGET-RC
                 ELSE
                   IF  RECE10-SRYYM      <  201004
                      MOVE    3     TO   LNK-SKYGET-RC
                   END-IF
                 END-IF
               END-IF
      *      ¡ã¼ÒÊÝ¡¦¹ñÊÝ¡¦¸å´ü¹âÎð¤Î¼«ÈñÊ¬°Ê²¼¤ÎÊÔ½¸¡ä
      *      ¡ãÆÃÊÌÎÅÍÜÈñ¡¢¼«ÈñÊÝ¸±¡¢¼£¸³¡¢Ï«ºÒ¡¢¼«ÇåÀÕ¡¢¸ø³²¤ÎÊÔ½¸¡ä
             WHEN      "2"
               IF      LNK-SKYGET-SRYYM  <  201004
                      MOVE    4     TO   LNK-SKYGET-RC
               END-IF
             WHEN      OTHER
                      MOVE    5     TO   LNK-SKYGET-RC
           END-EVALUATE
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
           EVALUATE    LNK-SKYGET-KBN
             WHEN      "1"
               PERFORM    2001-HENSYU1-SEC
             WHEN      "2"
               PERFORM    2002-HENSYU2-SEC
             WHEN      "3"
             WHEN      "4"
               PERFORM    2003-HENSYU3-SEC
             WHEN      "5"
               PERFORM    2005-HENSYU5-SEC
             WHEN      "6"
               PERFORM    2006-HENSYU6-SEC
             WHEN      "7"
               PERFORM    2007-HENSYU7-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£±¤ÎÊÔ½¸  ¡ã°åÊÝ¡ä
      *****************************************************************
       2001-HENSYU1-SEC            SECTION.
      *
           MOVE     ZERO         TO  FLG-TAISYO
           MOVE     ZERO         TO  FLG-TAISYO2
           MOVE     ZERO         TO  FLG-TAISYO3
           MOVE     ZERO         TO  FLG-SEIKYU-HENSYU-NG
           MOVE     ZERO         TO  FLG-KOHSKY-HENSYU-NG
           MOVE     ZERO         TO  FLG-KOHSKY-NASHI
      *
           MOVE     ZERO         TO  WRK-SKYIRYO-KOHFTN-KOHSKY
           MOVE     ZERO         TO  WRK-SKYSKJ-KOHFTN-KOHSKY
      *
           IF       RECE10-SRYYM  =  LNK-SKYGET-SRYYM
             IF     RECE10-SKYYM  =  "000000"
             OR     RECE10-SKYYM  =  LNK-SKYGET-SRYYM
                  MOVE   1       TO  FLG-TAISYO
             END-IF
           ELSE
             IF     RECE10-SKYYM  =  LNK-SKYGET-SRYYM
                  MOVE   1       TO  FLG-TAISYO
             END-IF
           END-IF
           IF       FLG-TAISYO    =  ZERO
                  MOVE   1       TO  FLG-SEIKYU-HENSYU-NG
           END-IF
      *
           IF     RECE10-NYUGAIKBN  =  "1"
               MOVE  RECE10-RESERVE-AREA(28:9)  TO  WRK-KOHSKY-DATA
               IF    WRK-KOHSKY-DATA   IS  NUMERIC
               AND   WRK-KOHSKY-KOHFTN    NOT = ZERO
                 MOVE   WRK-KOHSKY-KOHFTN
                               TO  WRK-SKYIRYO-KOHFTN-KOHSKY
               END-IF
               MOVE  RECE10-RESERVE-AREA(37:6)  TO  WRK-KOHSKY-DATA2
               IF    WRK-KOHSKY-DATA2  IS  NUMERIC
               AND   WRK-KOHSKY-SKJFTN    NOT = ZERO
                 MOVE   WRK-KOHSKY-SKJFTN
                               TO  WRK-SKYSKJ-KOHFTN-KOHSKY
               END-IF
           ELSE
               MOVE  RECE10-RESERVE-AREA(22:9)  TO  WRK-KOHSKY-DATA
               IF    WRK-KOHSKY-DATA   IS  NUMERIC
               AND   WRK-KOHSKY-KOHFTN    NOT = ZERO
                 MOVE   WRK-KOHSKY-KOHFTN
                               TO  WRK-SKYIRYO-KOHFTN-KOHSKY
               END-IF
           END-IF
      *
           INITIALIZE                KOHSKY-REC
           MOVE     RECE10-KEY   TO  KOHSKY-KEY
           PERFORM  900-KOHSKY-READ-SEC
           IF       FLG-KOHSKY  =  ZERO
             IF     KOHSKY-SRYYM  =  LNK-SKYGET-SRYYM
               IF   KOHSKY-SKYYM  =  "000000"
               OR   KOHSKY-SKYYM  =  LNK-SKYGET-SRYYM
                  MOVE   1       TO  FLG-TAISYO2
               END-IF
             ELSE
               IF   KOHSKY-SKYYM  =  LNK-SKYGET-SRYYM
                  MOVE   1       TO  FLG-TAISYO2
               END-IF
             END-IF
             IF     FLG-TAISYO2   =  ZERO
                  MOVE   1       TO  FLG-KOHSKY-HENSYU-NG
             END-IF
           ELSE
                  MOVE   1       TO  FLG-KOHSKY-NASHI
           END-IF
      *
           IF    FLG-SEIKYU-HENSYU-NG  =  ZERO
             IF  FLG-KOHSKY-NASHI      =  ZERO
               IF  FLG-KOHSKY-HENSYU-NG  =  ZERO
                    CONTINUE
               ELSE
      ***        IF  (WRK-SKYIRYO-KOHFTN-KOHSKY NOT =  ZERO)
      ***        OR  (WRK-SKYSKJ-KOHFTN-KOHSKY  NOT =  ZERO)
                    MOVE   1     TO  FLG-TAISYO3
      ***        END-IF
               END-IF
             END-IF
           ELSE
             IF  FLG-KOHSKY-NASHI      =  ZERO
               IF  FLG-KOHSKY-HENSYU-NG  =  ZERO
      ***        IF  (WRK-SKYIRYO-KOHFTN-KOHSKY NOT =  ZERO)
      ***        OR  (WRK-SKYSKJ-KOHFTN-KOHSKY  NOT =  ZERO)
                    MOVE   2     TO  FLG-TAISYO3
      ***        ELSE
      ***           MOVE   9     TO  FLG-TAISYO3
      ***        END-IF
               ELSE
                    MOVE   9     TO  FLG-TAISYO3
               END-IF
             ELSE
                    MOVE   9     TO  FLG-TAISYO3
             END-IF
           END-IF
      *
           IF   (RECE10-SKYIRYO-TOTALTEN    NOT = ZERO)
           OR   (RECE10-SKYSKJ-RYOYOHI      NOT = ZERO)
             IF   FLG-TAISYO3  =  9
                  MOVE   12      TO  LNK-SKYGET-RC
             END-IF
           ELSE
                  MOVE   11      TO  LNK-SKYGET-RC
           END-IF
      *
           IF    LNK-SKYGET-RC  =  ZERO
      *
      *      ÀÁµá³Û¾ðÊóÊÔ½¸
             PERFORM  20011-HENSYU11-SEC
      *
      *      ¸øÈñ¾ðÊóÊÔ½¸
             EVALUATE   FLG-TAISYO3
               WHEN      0
                 PERFORM  20012-HENSYU12-SEC
                 PERFORM  20013-HENSYU13-SEC
               WHEN      1
                 PERFORM  20012-HENSYU12-SEC
                 MOVE       1    TO  LNK-SKYGET-SEIKYUFLG
               WHEN      2
                 PERFORM  20013-HENSYU13-SEC
                 MOVE       2    TO  LNK-SKYGET-SEIKYUFLG
             END-EVALUATE
      *
           END-IF
      *
           .
       2001-HENSYU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá³Û¾ðÊóÊÔ½¸
      *****************************************************************
       20011-HENSYU11-SEC            SECTION.
      *
           MOVE   RECE10-SKYIRYO-INF   TO  LNK-SKYGET-IRYO-INF
           MOVE   RECE10-SKYSKJ-INF    TO  LNK-SKYGET-SKJ-INF
      *
           IF     LNK-SKYGET-PARA   =  "1"
             IF   RECE10-NYUGAIKBN  =  "1"
               MOVE  RECE10-RESERVE-AREA(13:9)  TO  WRK-DAISAN-DATA
               IF    WRK-DAISAN-DATA   IS  NUMERIC
               AND   WRK-DAISAN-FTNMONEY  NOT = ZERO
                 ADD   WRK-DAISAN-FTNMONEY
                               TO  LNK-SKYGET-IRYO-ICHIBUFTN
               END-IF
               MOVE  RECE10-RESERVE-AREA(22:6)  TO  WRK-DAISAN-DATA2
               IF    WRK-DAISAN-DATA2  IS  NUMERIC
               AND   WRK-DAISAN-SKJFTN    NOT = ZERO
                 ADD   WRK-DAISAN-SKJFTN
                               TO  LNK-SKYGET-SKJ-RYOYOHI
                                   LNK-SKYGET-SKJ-ICHIBUFTN
               END-IF
             ELSE
               MOVE  RECE10-RESERVE-AREA(13:9)  TO  WRK-DAISAN-DATA
               IF    WRK-DAISAN-DATA   IS  NUMERIC
               AND   WRK-DAISAN-FTNMONEY  NOT = ZERO
                 ADD   WRK-DAISAN-FTNMONEY
                               TO  LNK-SKYGET-IRYO-ICHIBUFTN
               END-IF
             END-IF
           END-IF
      *
           EVALUATE   FLG-TAISYO3
             WHEN      1
               COMPUTE   LNK-SKYGET-IRYO-KOHFTN =
                         LNK-SKYGET-IRYO-KOHFTN -
                         WRK-SKYIRYO-KOHFTN-KOHSKY
               COMPUTE   LNK-SKYGET-SKJ-RYOYOHI =
                         LNK-SKYGET-SKJ-RYOYOHI -
                         WRK-SKYSKJ-KOHFTN-KOHSKY
               COMPUTE   LNK-SKYGET-SKJ-KOHFTN  =
                         LNK-SKYGET-SKJ-KOHFTN  -
                         WRK-SKYSKJ-KOHFTN-KOHSKY
             WHEN      2
               MOVE   ZERO     TO  LNK-SKYGET-IRYO-NISSU
               MOVE   ZERO     TO  LNK-SKYGET-IRYO-TOTALTEN
               MOVE   ZERO     TO  LNK-SKYGET-IRYO-ICHIBUFTN
               MOVE   ZERO     TO  LNK-SKYGET-IRYO-HOKENJA
               MOVE   WRK-SKYIRYO-KOHFTN-KOHSKY
                               TO  LNK-SKYGET-IRYO-KOHFTN
               MOVE   ZERO     TO  LNK-SKYGET-SKJ-KAISU
               MOVE   WRK-SKYSKJ-KOHFTN-KOHSKY
                               TO  LNK-SKYGET-SKJ-RYOYOHI
               MOVE   ZERO     TO  LNK-SKYGET-SKJ-ICHIBUFTN
               MOVE   ZERO     TO  LNK-SKYGET-SKJ-HOKENJA
               MOVE   WRK-SKYSKJ-KOHFTN-KOHSKY
                               TO  LNK-SKYGET-SKJ-KOHFTN
           END-EVALUATE
      *
           .
       20011-HENSYU11-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¾ðÊóÊÔ½¸
      *****************************************************************
       20012-HENSYU12-SEC            SECTION.
      *
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                      UNTIL    IDX1     >   4
             IF   RECE10-KOHNUM(IDX1)  NOT = SPACE
               MOVE  RECE10-KOHNUM(IDX1)
                          TO  LNK-SKYGET-KOH-HKNNUM(IDX1)
               MOVE  RECE10-JNISSU(IDX1 + 1)
                          TO  LNK-SKYGET-KOH-NISSU(IDX1)
               MOVE  RECE10-TOTALTEN(IDX1 + 1)
                          TO  LNK-SKYGET-KOH-TOTALTEN(IDX1)
               MOVE  RECE10-FTNMONEY(IDX1 + 1)
                          TO  LNK-SKYGET-KOH-FTNMONEY(IDX1)
               MOVE  RECE10-SHOKUJINISSU(IDX1 + 1)
                          TO  LNK-SKYGET-KOH-KAISU(IDX1)
               MOVE  RECE10-SHOKUJIRYOYOHI(IDX1 + 1)
                          TO  LNK-SKYGET-KOH-RYOYOHI(IDX1)
               MOVE  RECE10-SHOKUJIFTN(IDX1 + 1)
                          TO  LNK-SKYGET-KOH-SHOKUJIFTN(IDX1)
             END-IF
           END-PERFORM
      *
           .
       20012-HENSYU12-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¾ðÊóÊÔ½¸
      *****************************************************************
       20013-HENSYU13-SEC            SECTION.
      *
           IF  FLG-KOHSKY-NASHI  =  ZERO
             PERFORM   VARYING   IDX1   FROM  1  BY  1
                        UNTIL    IDX1     >   4
               IF  (KOHSKY-KOHNUM(IDX1) NOT = SPACE                   )
               AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(1))
               AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(2))
               AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(3))
               AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(4))
                 MOVE  KOHSKY-KOHNUM(IDX1)
                            TO  LNK-SKYGET-KOHETC-HKNNUM(IDX1)
                 MOVE  KOHSKY-JNISSU(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-NISSU(IDX1)
                 MOVE  KOHSKY-TOTALTEN(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-TOTALTEN(IDX1)
                 MOVE  KOHSKY-FTNMONEY(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-FTNMONEY(IDX1)
                 MOVE  KOHSKY-SHOKUJINISSU(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-KAISU(IDX1)
                 MOVE  KOHSKY-SHOKUJIRYOYOHI(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-RYOYOHI(IDX1)
                 MOVE  KOHSKY-SHOKUJIFTN(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-SHOKUJIFTN(IDX1)
               END-IF
             END-PERFORM
           END-IF
      *
           .
       20013-HENSYU13-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£²¤ÎÊÔ½¸
      *****************************************************************
       2002-HENSYU2-SEC            SECTION.
      *
      *    Æþ±¡¡Ê²¾¼ýÇ¼¡Ë
           MOVE    ZERO                  TO   FLG-SYUKEI
           INITIALIZE                         WRK-TAIHI-AREA
           INITIALIZE                         WRK-TAIHI-AREA2
           INITIALIZE                         SYUNOU-REC
           MOVE    SPA-HOSPNUM           TO   SYU-HOSPNUM
           MOVE    "1"                   TO   SYU-NYUGAIKBN
           MOVE    LNK-SKYGET-SRYYM      TO   SYU-SRYYMD(1:6)
           MOVE    "__"                  TO   SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC            TO   MCPDATA-REC
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key76"               TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_syunou"        TO   MCP-TABLE
             MOVE    "key76"             TO   MCP-PATHNAME
             PERFORM 900-SYUNOU-READ-SEC
             PERFORM    UNTIL   (FLG-SYUNOU   =   1)
               IF  SYU-DENPJTIKBN    NOT =  "3"
                 MOVE      1             TO   IDX2
                 PERFORM   20021-CHECK-SEC
               END-IF
               MOVE    "tbl_syunou"      TO   MCP-TABLE
               MOVE    "key76"           TO   MCP-PATHNAME
               PERFORM 900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key76"               TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ³°Íè
           MOVE    ZERO                  TO   FLG-SYUKEI
           INITIALIZE                         WRK-TAIHI-AREA
           INITIALIZE                         WRK-TAIHI-AREA2
           INITIALIZE                         SYUNOU-REC
           MOVE    SPA-HOSPNUM           TO   SYU-HOSPNUM
           MOVE    "2"                   TO   SYU-NYUGAIKBN
           MOVE    LNK-SKYGET-SRYYM      TO   SYU-SRYYMD(1:6)
           MOVE    "__"                  TO   SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC            TO   MCPDATA-REC
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_syunou"        TO   MCP-TABLE
             MOVE    "key75"             TO   MCP-PATHNAME
             PERFORM 900-SYUNOU-READ-SEC
             PERFORM    UNTIL   (FLG-SYUNOU   =   1)
               IF  SYU-DENPJTIKBN    NOT =  "3"
                 MOVE      2             TO   IDX2
                 PERFORM   20021-CHECK-SEC
               END-IF
               MOVE    "tbl_syunou"      TO   MCP-TABLE
               MOVE    "key75"           TO   MCP-PATHNAME
               PERFORM 900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    Æþ±¡¡ÊËÜ¼ýÇ¼¡Ë
           MOVE    ZERO                  TO   FLG-SYUKEI
           INITIALIZE                         WRK-TAIHI-AREA
           INITIALIZE                         WRK-TAIHI-AREA2
           INITIALIZE                         SYUNOU-REC
           MOVE    SPA-HOSPNUM           TO   SYU-HOSPNUM
           MOVE    "1"                   TO   SYU-NYUGAIKBN
           MOVE    LNK-SKYGET-SRYYM      TO   SYU-SRYYMD(1:6)
           MOVE    "__"                  TO   SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC            TO   MCPDATA-REC
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_syunou"        TO   MCP-TABLE
             MOVE    "key75"             TO   MCP-PATHNAME
             PERFORM 900-SYUNOU-READ-SEC
             PERFORM    UNTIL   (FLG-SYUNOU   =   1)
               IF  SYU-DENPJTIKBN    NOT =  "3"
                 MOVE      3             TO   IDX2
                 PERFORM   20021-CHECK-SEC
               END-IF
               MOVE    "tbl_syunou"      TO   MCP-TABLE
               MOVE    "key75"           TO   MCP-PATHNAME
               PERFORM 900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2002-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£²¡Ê´µ¼Ô¾ðÊó¥Á¥§¥Ã¥¯¡Ë
      *****************************************************************
       20021-CHECK-SEC            SECTION.
      *
           IF  (WRK-TAIHI-PTID         =  SYU-PTID       )
           AND (WRK-TAIHI-HKNCOMBINUM  =  SYU-HKNCOMBINUM)
             CONTINUE
           ELSE
             IF   WRK-TAIHI-PTID   NOT =  SYU-PTID
                MOVE    ZERO            TO    FLG-OK
                INITIALIZE                    PTINF-REC
                MOVE    SYU-HOSPNUM     TO    PTINF-HOSPNUM
                MOVE    SYU-PTID        TO    PTINF-PTID
                PERFORM 900-PTINF-READ-SEC
                IF      FLG-PTINF    =   ZERO
                  IF  PTINF-TSTPTNUMKBN   =   "0"
                     MOVE     1         TO    FLG-OK
                  END-IF
                END-IF
             END-IF
             MOVE   1                TO    FLG-SYUKEI
             INITIALIZE                    WRK-TAIHI-AREA
             MOVE   SYU-PTID         TO    WRK-TAIHI-PTID
             MOVE   SYU-HKNCOMBINUM  TO    WRK-TAIHI-HKNCOMBINUM
             INITIALIZE                    WRK-TAIHI-AREA2
           END-IF
      *
           IF   FLG-OK  =   1
      *      ¶â³ÛÊÔ½¸
             PERFORM   200211-KINGAKU-HENSYU-SEC
           END-IF
      *
           .
       20021-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£²¡Ê¶â³ÛÊÔ½¸¡Ë
      *****************************************************************
       200211-KINGAKU-HENSYU-SEC            SECTION.
      *
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹ÊÔ½¸
           PERFORM   2002111-INDEX-HENSYU-SEC
      *
           MOVE     ZERO     TO    FLG-JIHIRANSYUKEI
           IF  (SYU-TOTAL-HKNTEN      =  ZERO)
           AND (SYU-TOTAL-MONEY   NOT =  ZERO)
           AND (IDX1                  =   5  )
           AND (SYU-JYO-COMPFTN   NOT =  ZERO)
           AND (LNK-SKYGET-SRYYM  >=   201104)
             MOVE    1       TO    FLG-JIHIRANSYUKEI
           END-IF
      *
           IF   (IDX1  =   4  OR  5  OR  6)
           AND  (IDX2  =   1  OR  2       )
      *
             IF    FLG-SYUKEI   =   1
               IF  SYU-TOTAL-HKNTEN    NOT =  ZERO
               OR  SYU-RYOYOHI-SKJ     NOT =  ZERO
               OR  SYU-RYOYOHI-LIFE    NOT =  ZERO
               OR  FLG-JIHIRANSYUKEI       =   1
                 COMPUTE  LNK-SKYGET-KENSU(IDX1 IDX2) =
                          LNK-SKYGET-KENSU(IDX1 IDX2) +
                          1
                 MOVE     ZERO    TO    FLG-SYUKEI
               END-IF
             END-IF
      *
             IF    SYU-TOTAL-HKNTEN    NOT =  ZERO
             OR    FLG-JIHIRANSYUKEI       =   1
               EVALUATE  SYU-NYUGAIKBN
                 WHEN      "1"
                   PERFORM   VARYING   IDX3   FROM  1  BY  1
                              UNTIL    IDX3    >   31
                     IF  (SYU-DAY(IDX3) =  "1" OR "2" OR "9")
                     AND (WRK-TAIHI-DAY(IDX3)  =  ZERO      )
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                     END-IF
                   END-PERFORM
                 WHEN      "2"
                   MOVE   SYU-SRYYMD(7:2)   TO   IDX3
                   IF     WRK-TAIHI-DAY(IDX3)  =  ZERO
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                   END-IF
               END-EVALUATE
             END-IF
             IF    SYU-TOTAL-HKNTEN    NOT =  ZERO
               COMPUTE    LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) =
                          LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) +
                          SYU-TOTAL-HKNTEN
               IF    IDX1  =   4  OR  5
                 COMPUTE  LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) =
                          LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) +
                          SYU-HKNTEKMONEY
               ELSE
                 COMPUTE  LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) =
                          LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) +
                          SYU-HKNTEKMONEY
               END-IF
             ELSE
               IF    FLG-JIHIRANSYUKEI   =   1
                 COMPUTE    LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) =
                            LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) +
                            SYU-TOTAL-MONEY
                 COMPUTE    LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) =
                            LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) +
                            SYU-JYO-COMPFTN
               END-IF
             END-IF
      *
             COMPUTE    LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) +
                        SYU-SYUSKJ-NISSU
             COMPUTE    LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) +
                        SYU-RYOYOHI-SKJ                    +
                        SYU-RYOYOHI-LIFE
             IF    IDX1  =   4  OR  5
               COMPUTE  LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) +
                        SYU-SKYMONEY-SKJ-KEI                 +
                        SYU-SKYMONEY-LIFE-KEI
             ELSE
               COMPUTE  LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) +
                        SYU-SKYMONEY-SKJ-KEI               +
                        SYU-SKYMONEY-LIFE-KEI
             END-IF
      *
           END-IF
      *
           IF   (IDX1  =   7  OR  8  OR  9)
           AND  (IDX2  =   1  OR  2       )
      *
             COMPUTE    WRK-KEISAN                 =
                        SYU-SYUSKJ-RYOYOHI         +
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX
             COMPUTE    WRK-KEISAN2           =
                        SYU-RSIJIBAI-SKYMONEY -
                        WRK-KEISAN
      *
             IF    FLG-SYUKEI   =   1
               IF  WRK-KEISAN2         NOT =  ZERO
               OR  SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
                 COMPUTE  LNK-SKYGET-KENSU(IDX1 IDX2) =
                          LNK-SKYGET-KENSU(IDX1 IDX2) +
                          1
                 MOVE     ZERO    TO    FLG-SYUKEI
               END-IF
             END-IF
      *
             IF    WRK-KEISAN2  NOT =  ZERO
               EVALUATE  SYU-NYUGAIKBN
                 WHEN      "1"
                   PERFORM   VARYING   IDX3   FROM  1  BY  1
                              UNTIL    IDX3    >   31
                     IF  (SYU-DAY(IDX3) =  "1" OR "2" OR "9")
                     AND (WRK-TAIHI-DAY(IDX3)  =  ZERO      )
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                     END-IF
                   END-PERFORM
                 WHEN      "2"
                   MOVE   SYU-SRYYMD(7:2)   TO   IDX3
                   IF     WRK-TAIHI-DAY(IDX3)  =  ZERO
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                   END-IF
               END-EVALUATE
             END-IF
             COMPUTE    LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) =
                        LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) +
                        SYU-TOTAL-HKNTEN
             IF    SYU-PTFTNRATE    =  100
               COMPUTE  LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) =
                        LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) +
                        WRK-KEISAN2
             ELSE
               COMPUTE  LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) =
                        LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) +
                        WRK-KEISAN2
             END-IF
      *
             COMPUTE    LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) +
                        SYU-SYUSKJ-NISSU
             COMPUTE    LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) +
                        SYU-SYUSKJ-RYOYOHI
             IF    SYU-PTFTNRATE    =  100
               COMPUTE  LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) +
                        SYU-SYUSKJ-RYOYOHI
             ELSE
               COMPUTE  LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) +
                        SYU-SYUSKJ-RYOYOHI
             END-IF
      *
           END-IF
      *
      ***
      *
           COMPUTE      LNK-SKYGET-JIHI(IDX1 IDX2) =
                        LNK-SKYGET-JIHI(IDX1 IDX2) +
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX
      *
           IF    FLG-JIHIRANSYUKEI   =   1
             COMPUTE    LNK-SKYGET-JIHI(IDX1 IDX2) =
                        LNK-SKYGET-JIHI(IDX1 IDX2) -
                        SYU-JYO-COMPFTN
           END-IF
      *
           COMPUTE      WRK-KEISAN2        =
                        SYU-DISCOUNT-MONEY *
                        -1
           COMPUTE      LNK-SKYGET-DISCOUNT-MONEY(IDX1 IDX2) =
                        LNK-SKYGET-DISCOUNT-MONEY(IDX1 IDX2) +
                        WRK-KEISAN2
      *
           COMPUTE      LNK-SKYGET-CHOSEI (IDX1 IDX2) =
                        LNK-SKYGET-CHOSEI (IDX1 IDX2) +
                        SYU-CHOSEI
      *
           COMPUTE      LNK-SKYGET-CHOSEI1(IDX1 IDX2) =
                        LNK-SKYGET-CHOSEI1(IDX1 IDX2) +
                        SYU-CHOSEI1
      *
           COMPUTE      LNK-SKYGET-CHOSEI2(IDX1 IDX2) =
                        LNK-SKYGET-CHOSEI2(IDX1 IDX2) +
                        SYU-CHOSEI2
      *
           IF    IDX1  =   7  OR  8  OR  9
             IF   SYU-PTFTNRATE   =  100
               COMPUTE  WRK-KEISAN            =
                        SYU-RSIJIBAI-SKYMONEY -
                        SYU-DISCOUNT-MONEY    +
                        SYU-CHOSEI
             ELSE
               COMPUTE  WRK-KEISAN                 =
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX         -
                        SYU-DISCOUNT-MONEY         +
                        SYU-CHOSEI
             END-IF
           ELSE
            IF   IDX1  =  10
             IF   SYU-PTFTNRATE   =  ZERO
               COMPUTE  WRK-KEISAN                 =
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX         -
                        SYU-DISCOUNT-MONEY         +
                        SYU-CHOSEI
             ELSE
               COMPUTE  WRK-KEISAN            =
                        SYU-RSIJIBAI-SKYMONEY -
                        SYU-DISCOUNT-MONEY    +
                        SYU-CHOSEI
             END-IF
            ELSE
             COMPUTE    WRK-KEISAN                 =
                        SYU-HKNTEKMONEY            +
                        SYU-KOH-FTN                +
                        SYU-SKYMONEY-SKJ-KEI       +
                        SYU-SKYMONEY-LIFE-KEI      +
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX         -
                        SYU-DISCOUNT-MONEY         +
                        SYU-CHOSEI
            END-IF
           END-IF
           COMPUTE      WRK-KEISAN2  =
                        SYU-SKYMONEY -
                        WRK-KEISAN
           COMPUTE      LNK-SKYGET-HASU(IDX1 IDX2) =
                        LNK-SKYGET-HASU(IDX1 IDX2) +
                        WRK-KEISAN2
      *
           .
       200211-KINGAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£²¡Ê¥¤¥ó¥Ç¥Ã¥¯¥¹ÊÔ½¸¡Ë
      *****************************************************************
       2002111-INDEX-HENSYU-SEC            SECTION.
      *
           MOVE      ZERO            TO    IDX1
      *    ¼«ÈñÊÝ¸±
           IF    SYU-SYUHKNNUM(1:2)   =   "98"
              MOVE     5             TO    IDX1
           END-IF
      *    ¼£¸³
           IF    SYU-SYUHKNNUM(1:2)   =   "90" OR "91"
              MOVE     6             TO    IDX1
           END-IF
      *    Ï«ºÒ
           IF    SYU-SYUHKNNUM        =   "971"
              MOVE     7             TO    IDX1
           END-IF
      *    ¼«ÇåÀÕ
           IF    SYU-SYUHKNNUM        =   "973"
              MOVE     8             TO    IDX1
           END-IF
      *    ¸ø³²
           IF    SYU-SYUHKNNUM        =   "975"
              MOVE     9             TO    IDX1
           END-IF
      *    ¹ñÊÝ
           IF    SYU-SYUHKNNUM   =  "060" OR "067"
              MOVE     2             TO    IDX1
           END-IF
      *    ¸å´ü¹âÎð
           IF    SYU-SYUHKNNUM   =  "039"
              MOVE     3             TO    IDX1
           END-IF
      *    ÆÃÊÌÎÅÍÜÈñ
           IF    SYU-SYUHKNNUM   =  "068" OR "069" OR "040"
              MOVE     4             TO    IDX1
           END-IF
      *    ¼ÒÊÝ
           IF    IDX1   =   ZERO
              MOVE     1             TO    IDX1
           END-IF
      *    Âè»°¼Ô¹Ô°Ù
           IF    SYU-KOH1HKNNUM       =   "970"
              MOVE    10             TO    IDX1
           END-IF
      *
           .
       2002111-INDEX-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£³¤ÎÊÔ½¸  ¡ãÏ«ºÒ¡ä
      *    ¶èÊ¬£´¤ÎÊÔ½¸  ¡ã¼«ÇåÀÕ¡ä
      *****************************************************************
       2003-HENSYU3-SEC            SECTION.
      *
           MOVE    ZERO                TO   WRK-PTFTNRATE
      *
           INITIALIZE                       ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO   AGECHK-HOSPNUM
           MOVE    RECE10-PTID         TO   AGECHK-PTID
           MOVE    RECE10-SRYYM        TO   AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO   AGECHK-SRYYMD(7:2)
           MOVE    RECE10-HKNCOMBI(1)  TO   AGECHK-HKNCOMBINUM
           MOVE    RECE10-NYUGAIKBN    TO   AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF    AGECHK-RC       =  ZERO
             MOVE  AGECHK-FTNRATE      TO   WRK-PTFTNRATE
           END-IF
      *
           MOVE    RECE10-JNISSU(1)         TO LNK-SKYGET-IRYO-NISSU
           MOVE    RECE10-TOTALTEN(1)       TO LNK-SKYGET-IRYO-TOTALTEN
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-ICHIBUFTN
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-HOKENJA
           IF    WRK-PTFTNRATE   =  100
      ***    IF    RECE10-NYUGAIKBN   =  "1"
      ***      COMPUTE   LNK-SKYGET-IRYO-ICHIBUFTN =
      ***                RECE10-FTNMONEY(1)        -
      ***                RECE10-SHOKUJIRYOYOHI(1)
      ***    ELSE
      ***      MOVE  RECE10-FTNMONEY(1)     TO LNK-SKYGET-IRYO-ICHIBUFTN
      ***    END-IF
             IF    RECE10-NYUGAIKBN   =  "1"
               COMPUTE   LNK-SKYGET-IRYO-ICHIBUFTN =
                         RECE10-FTNMONEY-FTN(1)    +
                         RECE10-FTNMONEY-KOJ(1)
             ELSE
               PERFORM   20031-SYUNOU-CHECK-SEC
             END-IF
           ELSE
             IF    RECE10-NYUGAIKBN   =  "1"
               COMPUTE   LNK-SKYGET-IRYO-HOKENJA   =
                         RECE10-FTNMONEY(1)        -
                         RECE10-SHOKUJIRYOYOHI(1)
             ELSE
               MOVE  RECE10-FTNMONEY(1)     TO LNK-SKYGET-IRYO-HOKENJA
             END-IF
           END-IF
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-KOHFTN
      *
           MOVE    RECE10-SHOKUJINISSU(1)   TO LNK-SKYGET-SKJ-KAISU
           MOVE    RECE10-SHOKUJIRYOYOHI(1) TO LNK-SKYGET-SKJ-RYOYOHI
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-ICHIBUFTN
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-HOKENJA
           IF    WRK-PTFTNRATE   =  100
             MOVE  RECE10-SHOKUJIRYOYOHI(1) TO LNK-SKYGET-SKJ-ICHIBUFTN
           ELSE
             MOVE  RECE10-SHOKUJIRYOYOHI(1) TO LNK-SKYGET-SKJ-HOKENJA
           END-IF
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-KOHFTN
      *
           .
       2003-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡ãÏ«ºÒ¡¦¼«ÇåÀÕ¡ä  ³°Íè¤ÇÉéÃ´³ä¹ç£±£°£°¡ó¤Î¾ì¹ç
      *****************************************************************
       20031-SYUNOU-CHECK-SEC            SECTION.
      *
           INITIALIZE                         SYUNOU-REC
           MOVE    RECE10-HOSPNUM        TO   SYU-HOSPNUM
           MOVE    RECE10-NYUGAIKBN      TO   SYU-NYUGAIKBN
           MOVE    RECE10-PTID           TO   SYU-PTID
           MOVE    RECE10-SRYYM          TO   SYU-SRYYMD(1:6)
           MOVE    "__"                  TO   SYU-SRYYMD(7:2)
           MOVE    RECE10-HKNCOMBI(1)    TO   SYU-HKNCOMBINUM
           MOVE    SYUNOU-REC            TO   MCPDATA-REC
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key41"               TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_syunou"        TO   MCP-TABLE
             MOVE    "key41"             TO   MCP-PATHNAME
             PERFORM 900-SYUNOU-READ-SEC
             PERFORM    UNTIL   (FLG-SYUNOU   =   1)
               IF  SYU-DENPJTIKBN    NOT =  "3"
                 COMPUTE    WRK-KEISAN                 =
                            SYU-SYUSKJ-RYOYOHI         +
                            SYU-RMSAGAKU               +
                            SYU-SKYMONEY-SKJ-JIHI-KEI  +
                            SYU-SKYMONEY-LIFE-JIHI-KEI +
                            SYU-TOTAL-TGMONEY          +
                            SYU-TOTAL-TGMONEY-TAX      +
                            SYU-JIHI-TOTAL             +
                            SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE    WRK-KEISAN2                =
                            SYU-RSIJIBAI-SKYMONEY      -
                            WRK-KEISAN
      *
                 COMPUTE    LNK-SKYGET-IRYO-ICHIBUFTN  =
                            LNK-SKYGET-IRYO-ICHIBUFTN  +
                            WRK-KEISAN2
               END-IF
               MOVE    "tbl_syunou"      TO   MCP-TABLE
               MOVE    "key41"           TO   MCP-PATHNAME
               PERFORM 900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key41"               TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20031-SYUNOU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£µ¤ÎÊÔ½¸  ¡ã¸ø³²¡ä
      *****************************************************************
       2005-HENSYU5-SEC            SECTION.
      *
           MOVE    RECE10-JNISSU(1)         TO LNK-SKYGET-IRYO-NISSU
           MOVE    RECE10-TOTALTEN(1)       TO LNK-SKYGET-IRYO-TOTALTEN
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-ICHIBUFTN
           IF    RECE10-NYUGAIKBN   =  "1"
             COMPUTE   WRK-KEISAN3  =  RECE10-YKZFTN(1) *  1.2
             COMPUTE   LNK-SKYGET-IRYO-HOKENJA   =
                       RECE10-FTNMONEY(1)        -
                       WRK-KEISAN3
           ELSE
             MOVE  ZERO                     TO WRK-KEISAN3
             MOVE  RECE10-FTNMONEY(1)       TO LNK-SKYGET-IRYO-HOKENJA
           END-IF
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-KOHFTN
      *
           MOVE    RECE10-SHOKUJINISSU(1)   TO LNK-SKYGET-SKJ-KAISU
           MOVE    WRK-KEISAN3              TO LNK-SKYGET-SKJ-RYOYOHI
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-ICHIBUFTN
           MOVE    WRK-KEISAN3              TO LNK-SKYGET-SKJ-HOKENJA
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-KOHFTN
      *
           .
       2005-HENSYU5-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£¶¤ÎÊÔ½¸  ¡ãÂè»°¼Ô¹Ô°Ù¡ä
      *****************************************************************
       2006-HENSYU6-SEC            SECTION.
      *
           MOVE    ZERO                TO   WRK-PTFTNRATE
      *
           INITIALIZE                       ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO   AGECHK-HOSPNUM
           MOVE    RECE10-PTID         TO   AGECHK-PTID
           MOVE    RECE10-SRYYM        TO   AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO   AGECHK-SRYYMD(7:2)
           MOVE    RECE10-HKNCOMBI(1)  TO   AGECHK-HKNCOMBINUM
           MOVE    RECE10-NYUGAIKBN    TO   AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF    AGECHK-RC       =  ZERO
             MOVE  AGECHK-FTNRATE      TO   WRK-PTFTNRATE
           END-IF
      *
           MOVE    RECE10-JNISSU(1)         TO LNK-SKYGET-IRYO-NISSU
           MOVE    RECE10-TOTALTEN(1)       TO LNK-SKYGET-IRYO-TOTALTEN
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-ICHIBUFTN
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-HOKENJA
           MOVE    ZERO                     TO LNK-SKYGET-IRYO-KOHFTN
      *
           MOVE    RECE10-SHOKUJINISSU(1)   TO LNK-SKYGET-SKJ-KAISU
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-RYOYOHI
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-ICHIBUFTN
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-HOKENJA
           MOVE    ZERO                     TO LNK-SKYGET-SKJ-KOHFTN
      *
           IF    LNK-SKYGET-PARA  =  "1"
             IF    WRK-PTFTNRATE  =  ZERO
               COMPUTE   LNK-SKYGET-IRYO-HOKENJA   =
                         RECE10-FTNMONEY-KOJ(1)
             ELSE
               COMPUTE   LNK-SKYGET-IRYO-ICHIBUFTN =
                         RECE10-FTNMONEY-KOJ(1)
             END-IF
           ELSE
             IF    WRK-PTFTNRATE  =  ZERO
               COMPUTE   LNK-SKYGET-IRYO-HOKENJA   =
                         RECE10-FTNMONEY-FTN(1)    +
                         RECE10-FTNMONEY-KOJ(1)
               MOVE   RECE10-SHOKUJIRYOYOHI(1)
                                     TO LNK-SKYGET-SKJ-RYOYOHI
                                        LNK-SKYGET-SKJ-HOKENJA
             ELSE
               COMPUTE   LNK-SKYGET-IRYO-ICHIBUFTN =
                         RECE10-FTNMONEY-FTN(1)    +
                         RECE10-FTNMONEY-KOJ(1)
               MOVE   RECE10-SHOKUJIRYOYOHI(1)
                                     TO LNK-SKYGET-SKJ-RYOYOHI
                                        LNK-SKYGET-SKJ-ICHIBUFTN
             END-IF
           END-IF
      *
           .
       2006-HENSYU6-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬£·¤ÎÊÔ½¸  ¡ãÆÃÊÌÎÅÍÜÈñ¡ä
      *****************************************************************
       2007-HENSYU7-SEC            SECTION.
      *
           MOVE   RECE10-SKYIRYO-INF   TO  LNK-SKYGET-IRYO-INF
           MOVE   RECE10-SKYSKJ-INF    TO  LNK-SKYGET-SKJ-INF
      *
           MOVE   ZERO                 TO  LNK-SKYGET-IRYO-HOKENJA
           MOVE   ZERO                 TO  LNK-SKYGET-IRYO-KOHFTN
           MOVE   ZERO                 TO  LNK-SKYGET-SKJ-HOKENJA
           MOVE   ZERO                 TO  LNK-SKYGET-SKJ-KOHFTN
      *
           .
       2007-HENSYU7-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÆÉ¹þ
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÀÁµá¸¡º÷
      *****************************************************************
       900-KOHSKY-READ-SEC        SECTION.
      *
           MOVE    KOHSKY-REC           TO  MCPDATA-REC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           MOVE    "DBSELECT"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_kohsky"     TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               MOVE    "DBFETCH"        TO  MCP-FUNC
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  KOHSKY-REC
                   MOVE    ZERO         TO  FLG-KOHSKY
               ELSE
                   INITIALIZE               KOHSKY-REC
                   MOVE    1            TO  FLG-KOHSKY
               END-IF
           ELSE
               INITIALIZE                   KOHSKY-REC
               MOVE    1                TO  FLG-KOHSKY
           END-IF
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC            TO  MCPDATA-REC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           MOVE    "DBSELECT"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptinf"      TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               MOVE    "DBFETCH"        TO  MCP-FUNC
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  PTINF-REC
                   MOVE    ZERO         TO  FLG-PTINF
               ELSE
                   INITIALIZE               PTINF-REC
                   MOVE    1            TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                   PTINF-REC
               MOVE    1                TO  FLG-PTINF
           END-IF
           MOVE     "tbl_ptinf"         TO  MCP-TABLE
           MOVE     "key"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
      *
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
      *
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
