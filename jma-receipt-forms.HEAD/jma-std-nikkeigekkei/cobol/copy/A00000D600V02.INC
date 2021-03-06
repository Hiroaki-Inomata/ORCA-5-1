      *******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
      *  vOC³ð
      * Maj/Min/Rev  C³Ò       út      àe
      * 01.00.00     @@@@-c    11/04/04  VKì¬
      * 01.00.00     @@@@-c    11/09/29  WvÎÛúðÍÍwèÉÏX
      ********************************************************************
       01  D600.
      *    WubZ[W
           03  D600-JOBMSG            PIC X(60).
      *    Å
           03  D600-PAGE              PIC X(03).
      *    WvÎÛút
           03  D600-SYSYMD            PIC X(22).
      *    ©oµ
           03  D600-MIDASI            PIC X(10).
      *    üàÔàú
           03  D600-NYUHEN-YMD        PIC X(50).
      *    üOæª
           03  D600-NYUGAIKBN         PIC X(04).
      *    fÃÈ
           03  D600-SRYKA             PIC X(20).
      *    ü@¿iÚ¼Ìj
           03  D600-NYUINRYO          PIC X(08).
      *    Ã{èÄiÚ¼Ìj
           03  D600-TRYO              PIC X(08).
      *    AÔe[u
           03  D600-RENNUM-TBL        OCCURS 21 TIMES.
      *    AÔ
               05  D600-RENNUM        PIC X(07).
      *    ³ÒÔe[u
           03  D600-PTNUM-TBL         OCCURS 21 TIMES.
      *    ³ÒÔiPOÈºj
               05  D600-PTNUM         PIC X(10).
      *    ³ÒÔiPPÈãj
               05  D600-PTNUM2        PIC X(20).
      *    ³Ò¼e[u
           03  D600-NAME-TBL          OCCURS 21 TIMES.
      *    ³Ò¼
               05  D600-NAME          PIC X(18).
      *    «Êe[u
           03  D600-SEX-TBL           OCCURS  21 TIMES.
      *    «Ê
               05  D600-SEX           PIC X(02).
      *    Nîe[u
           03  D600-AGE-TBL           OCCURS  21 TIMES.
      *    Nî
               05  D600-AGE           PIC X(03).
      *    {lÆ°e[u
           03  D600-HKNKBN-TBL        OCCURS  21 TIMES.
      *    {lÆ°
               05  D600-HKNKBN        PIC X(02).
      *    Û¯íÞe[u
           03  D600-HKNS-TBL          OCCURS  21 TIMES.
      *    Û¯íÞ
               05  D600-HKNS          PIC X(08).
      *    Û¯_e[u
           03  D600-HKNTEN-TBL        OCCURS  21 TIMES.
      *    Û¯_
               05  D600-HKNTEN        PIC X(07).
      *    fÃæªe[u
           03  D600-TEN-HK-TBL        OCCURS  16 TIMES.
      *    Û¯_e[uifÃæªÊj
               05  D600-TEN-TBL       OCCURS  21 TIMES.
      *    Û¯_ifÃæªÊj
                   07  D600-TEN       PIC X(07).
      *    Û¯tze[u
           03  D600-HKNFTN-TBL        OCCURS  21 TIMES.
      *    Û¯tz
               05  D600-HKNFTN        PIC X(08).
      *    ©ïSze[u
           03  D600-JIHIFTN-TBL       OCCURS  21 TIMES.
      *    ©ïSz
               05  D600-JIHIFTN       PIC X(08).
      *    û¿ze[u
           03  D600-SEIKYU-TBL        OCCURS  21 TIMES.
      *    û¿z
               05  D600-SEIKYU        PIC X(08).
      *    ûÌûze[u
           03  D600-RYOSYU-TBL        OCCURS  21 TIMES.
      *    ûÌûz
               05  D600-RYOSYU        PIC X(08).
      *    Û¯_\viVj
           03  D600-G-HKNTEN          PIC X(07).
      *    Û¯_\viWj
           03  D600-GG-HKNTEN         PIC X(08).
      *    Û¯_\ve[uifÃæªÊj
           03  D600-G-TEN-HK-TBL      OCCURS  16 TIMES.
      *    Û¯_\vifÃæªÊjiVj
               05  D600-G-TEN         PIC X(07).
      *    Û¯_\ve[uifÃæªÊj
           03  D600-GG-TEN-HK-TBL     OCCURS  16 TIMES.
      *    Û¯_\vifÃæªÊjiWj
               05  D600-GG-TEN        PIC X(08).
      *    Û¯tz\viWj
           03  D600-G-HKNFTN          PIC X(08).
      *    Û¯tz\viXj
           03  D600-GG-HKNFTN         PIC X(09).
      *    ©ïSz\viWj
           03  D600-G-JIHIFTN         PIC X(08).
      *    ©ïSz\viXj
           03  D600-GG-JIHIFTN        PIC X(09).
      *    û¿z\viWj
           03  D600-G-SEIKYU          PIC X(08).
      *    û¿z\viXj
           03  D600-GG-SEIKYU         PIC X(09).
      *    ûÌûz\viWj
           03  D600-G-RYOSYU          PIC X(08).
      *    ûÌûz\viXj
           03  D600-GG-RYOSYU         PIC X(09).
