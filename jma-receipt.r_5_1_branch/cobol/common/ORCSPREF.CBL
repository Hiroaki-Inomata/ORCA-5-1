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
       PROGRAM-ID.                     ORCSPREF.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¶¦ÄÌ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÅÔÆ»ÉÜ¸©Ì¾¼èÆÀ
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  08/11/23    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-ERR                 PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DUMMY               PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-PREF-G.
             05  CONST-PREF01 PIC X(22) VALUE "01ËÌ³¤Æ»    ËÌ³¤Æ»    ".
             05  CONST-PREF02 PIC X(22) VALUE "02ÀÄ¿¹¸©    ÀÄ¿¹      ".
             05  CONST-PREF03 PIC X(22) VALUE "03´ä¼ê¸©    ´ä¼ê      ".
             05  CONST-PREF04 PIC X(22) VALUE "04µÜ¾ë¸©    µÜ¾ë      ".
             05  CONST-PREF05 PIC X(22) VALUE "05½©ÅÄ¸©    ½©ÅÄ      ".
             05  CONST-PREF06 PIC X(22) VALUE "06»³·Á¸©    »³·Á      ".
             05  CONST-PREF07 PIC X(22) VALUE "07Ê¡Åç¸©    Ê¡Åç      ".
             05  CONST-PREF08 PIC X(22) VALUE "08°ñ¾ë¸©    °ñ¾ë      ".
             05  CONST-PREF09 PIC X(22) VALUE "09ÆÊÌÚ¸©    ÆÊÌÚ      ".
             05  CONST-PREF10 PIC X(22) VALUE "10·²ÇÏ¸©    ·²ÇÏ      ".
             05  CONST-PREF11 PIC X(22) VALUE "11ºë¶Ì¸©    ºë¶Ì      ".
             05  CONST-PREF12 PIC X(22) VALUE "12ÀéÍÕ¸©    ÀéÍÕ      ".
             05  CONST-PREF13 PIC X(22) VALUE "13ÅìµþÅÔ    ÅìµþÅÔ    ".
             05  CONST-PREF14 PIC X(22) VALUE "14¿ÀÆàÀî¸©  ¿ÀÆàÀî    ".
             05  CONST-PREF15 PIC X(22) VALUE "15¿·³ã¸©    ¿·³ã      ".
             05  CONST-PREF16 PIC X(22) VALUE "16ÉÙ»³¸©    ÉÙ»³      ".
             05  CONST-PREF17 PIC X(22) VALUE "17ÀÐÀî¸©    ÀÐÀî      ".
             05  CONST-PREF18 PIC X(22) VALUE "18Ê¡°æ¸©    Ê¡°æ      ".
             05  CONST-PREF19 PIC X(22) VALUE "19»³Íü¸©    »³Íü      ".
             05  CONST-PREF20 PIC X(22) VALUE "20Ä¹Ìî¸©    Ä¹Ìî      ".
             05  CONST-PREF21 PIC X(22) VALUE "21´ôÉì¸©    ´ôÉì      ".
             05  CONST-PREF22 PIC X(22) VALUE "22ÀÅ²¬¸©    ÀÅ²¬      ".
             05  CONST-PREF23 PIC X(22) VALUE "23°¦ÃÎ¸©    °¦ÃÎ      ".
             05  CONST-PREF24 PIC X(22) VALUE "24»°½Å¸©    »°½Å      ".
             05  CONST-PREF25 PIC X(22) VALUE "25¼¢²ì¸©    ¼¢²ì      ".
             05  CONST-PREF26 PIC X(22) VALUE "26µþÅÔÉÜ    µþÅÔÉÜ    ".
             05  CONST-PREF27 PIC X(22) VALUE "27ÂçºåÉÜ    ÂçºåÉÜ    ".
             05  CONST-PREF28 PIC X(22) VALUE "28Ê¼¸Ë¸©    Ê¼¸Ë      ".
             05  CONST-PREF29 PIC X(22) VALUE "29ÆàÎÉ¸©    ÆàÎÉ      ".
             05  CONST-PREF30 PIC X(22) VALUE "30ÏÂ²Î»³¸©  ÏÂ²Î»³    ".
             05  CONST-PREF31 PIC X(22) VALUE "31Ä»¼è¸©    Ä»¼è      ".
             05  CONST-PREF32 PIC X(22) VALUE "32Åçº¬¸©    Åçº¬      ".
             05  CONST-PREF33 PIC X(22) VALUE "33²¬»³¸©    ²¬»³      ".
             05  CONST-PREF34 PIC X(22) VALUE "34¹­Åç¸©    ¹­Åç      ".
             05  CONST-PREF35 PIC X(22) VALUE "35»³¸ý¸©    »³¸ý      ".
             05  CONST-PREF36 PIC X(22) VALUE "36ÆÁÅç¸©    ÆÁÅç      ".
             05  CONST-PREF37 PIC X(22) VALUE "37¹áÀî¸©    ¹áÀî      ".
             05  CONST-PREF38 PIC X(22) VALUE "38°¦É²¸©    °¦É²      ".
             05  CONST-PREF39 PIC X(22) VALUE "39¹âÃÎ¸©    ¹âÃÎ      ".
             05  CONST-PREF40 PIC X(22) VALUE "40Ê¡²¬¸©    Ê¡²¬      ".
             05  CONST-PREF41 PIC X(22) VALUE "41º´²ì¸©    º´²ì      ".
             05  CONST-PREF42 PIC X(22) VALUE "42Ä¹ºê¸©    Ä¹ºê      ".
             05  CONST-PREF43 PIC X(22) VALUE "43·§ËÜ¸©    ·§ËÜ      ".
             05  CONST-PREF44 PIC X(22) VALUE "44ÂçÊ¬¸©    ÂçÊ¬      ".
             05  CONST-PREF45 PIC X(22) VALUE "45µÜºê¸©    µÜºê      ".
             05  CONST-PREF46 PIC X(22) VALUE "46¼¯»ùÅç¸©  ¼¯»ùÅç    ".
             05  CONST-PREF47 PIC X(22) VALUE "47²­Æì¸©    ²­Æì      ".
           03    CONST-PREF-R    REDEFINES  CONST-PREF-G.
             05  CONST-PREF-OCC         OCCURS 47.
                 07  CONST-PREFNUM      PIC X(02).
                 07  CONST-PREFNAME1    PIC X(10).
                 07  CONST-PREFNAME2    PIC X(10).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSPREF.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SPREF-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
      *
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   47 )
      *
               MOVE    CONST-PREFNUM (IDX0)
                                   TO  SPREF-PREFNUM (IDX0)
               MOVE    CONST-PREFNAME1(IDX0)
                                   TO  SPREF-PREFNAME1(IDX0)
               MOVE    CONST-PREFNAME2(IDX0)
                                   TO  SPREF-PREFNAME2(IDX0)
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
