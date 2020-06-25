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
       PROGRAM-ID.             A00000M501.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ·î¼¡Ä¢É¼
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ·î·×É½¡Ê£È£Ã£±£·¡Ë
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô       µ­½Ò
      *  03/12/24    @@@@-È¾ÅÄ    ¿·µ¬ºÆºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      *  Maj/Min/Rev ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    @@@@-Ìç´Ö    10/12/07  ¥³¥á¥ó¥ÈÍúÎòºï½ü
      *                                     ½é¿Ç¡¢ºÆ¿Ç¡¢¤½¤ÎÂ¾¤ÎÈ½Äê¾ò·ï¤òÊÑ¹¹
      *                                     open-cobol1.0ÂÐ±þ
      *  01.00.02    @@@@-Ìç´Ö    11/04/18  ½èÍýÂ®ÅÙ¤Î²þÁ±¡¢¥³¥á¥ó¥ÈÍúÎòºï½ü
      *  01.00.03    @@@@-ÅÄÃæ    11/10/13  CSV¥Õ¥¡¥¤¥ëºîÀ®µ¡Ç½ÄÉ²Ã
      *  01.00.04    @@@@-ÅÄÃæ    12/02/02  ¸øÌ³ºÒ³²¡Ê·òÊÝ½àµò¡Ë»þ¤ÎÅÀ¿ôÃ±²ÁÉÔ¶ñ¹ç¤ò½¤Àµ
      *  01.00.05    @@@@-Ìç´Ö    12/07/03  ½é¿Ç¡¢ºÆ¿Ç¡¢¤½¤ÎÂ¾¤ÎÈ½Äê¾ò·ï¤òºÆÅÙÊÑ¹¹
      *  01.00.06    @@@@-Ìç´Ö    12/08/24  ¿·´µ·ï¿ô¤¬½ÅÊ£¥«¥¦¥ó¥È¤µ¤ì¤ëÉÔ¶ñ¹ç¤ò½¤Àµ
      *  01.00.07    @@@@-Ìç´Ö    13/09/30  ÊÇ¿ô¤Î°õ»ú½¤Àµ¡Ê½èÍý·ë²Ì²èÌÌ¡Ë
      *  01.00.08    @@@@-Ìç´Ö    14/06/23  ¼«Èñ¿ÇÎÅ¤ÎÃ¼¿ô½èÍý¤ò½¤Àµ
      *  02.00.01    @@@@-Ìç´Ö    14/10/31  ver4.8.0ÂÐ±þ¡Ê°ì»þ¥Ç¥£¥ì¥¯¥È¥êÊÑ¹¹¡Ë
      *  02.00.02    @@@@-Ìç´Ö    15/01/14  ¼«Èñ¿ÇÎÅ¤Ë¾®·×¤òÄÉ²Ã
      *  02.00.03    Ìç´Ö         16/02/01  °ì»þ¥Ç¥£¥ì¥¯¥È¥êÂÐ±þ(¥¨¥é¡¼¥Õ¥¡¥¤¥ë)
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    ¥·¥§¥ëÍÑÎÎ°è
           COPY    "CPCOMMONSHELL.INC".
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000M501.INC".
           COPY    "A00000M501V02.INC".
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-JYURRK2         PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  DEBUG-SW            PIC 9(01).
           03  FLG-ACCTEND         PIC 9(01).
      *    ·ï¿ôÍÑ¥Õ¥é¥°ÎÎ°è
           03  FLG-KENSUU.
             05  FLG-HKN           PIC 9(01).
             05  FLG-SYUEKI        PIC 9(01).
             05  FLG-SEIKYU        PIC 9(01).
             05  FLG-RYOSYU        PIC 9(01).
             05  FLG-JIHI-G.
               07  FLG-JIHI        PIC 9(01)  OCCURS  10.
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-SRYACT             PIC 9(01).
           03  FLG-JYURRK3         PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    ¥·¥¹¥Æ¥àÎÎ°è
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(10).
           03  IDY                 PIC 9(10).
           03  IDZ                 PIC 9(10).
           03  IDX-S               PIC 9(10).
           03  IDX-N               PIC 9(10).
           03  IDX-MAX             PIC 9(10).
      *
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(10).
           03  IDX3                PIC 9(10).
           03  IDX4                PIC 9(03).
           03  IDX7G.
              05  IDX7             PIC 9(02).
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(02).
           03  IDX10               PIC 9(02).
           03  IDX11               PIC 9(02).
           03  JIHIIDX             PIC 9(10).
           03  IDX-ST              PIC 9(03).
           03  IDX-ED              PIC 9(03).
           03  IDXP                PIC 9(05).
      *
      *    ¥Ñ¥é¥á¥¿ÎÎ°è
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PRTYM      PIC X(06).
           03  WRK-PARA-PRTSYMD    PIC X(08).
           03  WRK-PARA-PRTLYMD    PIC X(08).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
           03  WRK-PRTYMWH           PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-PTID              PIC 9(10).
      *
           03  WRK-MONEY             PIC S9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-JIHI3             PIC 9(08).
      *
           03  WRK-CREYMD            PIC X(08).
           03  WRK-SYUEKI            PIC S9(08).
           03  WRK-TJIHI-G           OCCURS  10.
               05  WRK-TJIHI         PIC X(08).
           03  WRK-TJIHIN-G          OCCURS  10.
               05  WRK-TJIHIN        PIC X(08).
           03  WRK-4001-TENTANKA     PIC 9(02)V9(01).
           03  WRK-4001-KOMUFLG      PIC X(01).
           03  WRK-4001-KOMUTENTANKA PIC 9(02).
           03  WRK-TENTNK            PIC 9(02)V9(01).
      *
      *    Æþ±¡ÍÑ°õºþ¿ÇÎÅ²Ê·ï¿ô
           03  WRK-PRNSRKA-CNT       PIC 9(03).
      *     ³°ÍèÍÑ°õºþ¿ÇÎÅ²Ê·ï¿ô
           03  WRK-PRGSRKA-CNT       PIC 9(03).
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC ZZZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZZZ.
           03  WRK-S9                PIC ---------9.
           03  WRK-SZ                PIC ---------Z.
           03  WRK-TMP-K             PIC X(08).
           03  WRK-COUNTS            PIC 9(04).
           03  WRK-COMPFTN           PIC S9(10).
           03  WRK-SYOSINYMD         PIC 9(08).
           03  WRK-DENPNUM           PIC 9(07).
           03  WRK-JIHI-TTL          PIC 9(07).
           03  WRK-HASU-KBN          PIC X(01).
           03  WRK-HASU-KBNJ         PIC X(01).
      *
      *    ¿ÇÎÅ²ÊÊÌ²Ã»»¥¨¥ê¥¢
       01  SUM-AREA                 VALUE   ZERO.
           03  SUM-HKNTEN           PIC 9(10).
           03  SUM-SHKNKEN          PIC 9(10).
           03  SUM-RHKNKEN          PIC 9(10).
           03  SUM-OHKNKEN          PIC 9(10).
           03  SUM-SAHKNKEN         PIC 9(10).
           03  SUM-OCC              OCCURS  20.
               05  SUM-MONEY        PIC S9(10).
               05  SUM-OKEN         PIC 9(10).
               05  SUM-RKEN         PIC 9(10).
               05  SUM-SKEN         PIC 9(10).
               05  SUM-SAKEN        PIC 9(10).
           03  SUM-SYUEKI           PIC S9(10).
           03  SUM-SEIKYU           PIC S9(10).
           03  SUM-RYOSYU           PIC S9(10).
           03  SUM-SSYUEKIKEN       PIC 9(10).
           03  SUM-SSEIKYUKEN       PIC 9(10).
           03  SUM-SRYOSYUKEN       PIC 9(10).
           03  SUM-RSYUEKIKEN       PIC 9(10).
           03  SUM-RSEIKYUKEN       PIC 9(10).
           03  SUM-RRYOSYUKEN       PIC 9(10).
           03  SUM-OSYUEKIKEN       PIC 9(10).
           03  SUM-OSEIKYUKEN       PIC 9(10).
           03  SUM-ORYOSYUKEN       PIC 9(10).
           03  SUM-JYUSHINKEN       PIC 9(10).
           03  SUM-SASYUEKIKEN      PIC 9(10).
           03  SUM-SASEIKYUKEN      PIC 9(10).
           03  SUM-SARYOSYUKEN      PIC 9(10).
           03  SUM-JIHI-MONEY       PIC S9(10).
           03  SUM-JIHI-OKEN        PIC 9(10).
           03  SUM-JIHI-RKEN        PIC 9(10).
           03  SUM-JIHI-SKEN        PIC 9(10).
           03  SUM-JIHI-SAKEN       PIC 9(10).
      *
      *    Æþ³°¶èÊ¬ÊÌ²Ã»»¥¨¥ê¥¢
       01  SUM-AREA-GK              VALUE   ZERO.
           03  SUM-HKNTEN-GK        PIC 9(10).
           03  SUM-SHKNKEN-GK       PIC 9(10).
           03  SUM-RHKNKEN-GK       PIC 9(10).
           03  SUM-OHKNKEN-GK       PIC 9(10).
           03  SUM-SAHKNKEN-GK      PIC 9(10).
           03  SUM-OCC-GK           OCCURS  20.
               05  SUM-MONEY-GK     PIC S9(10).
               05  SUM-OKEN-GK      PIC 9(10).
               05  SUM-RKEN-GK      PIC 9(10).
               05  SUM-SKEN-GK      PIC 9(10).
               05  SUM-SAKEN-GK     PIC 9(10).
           03  SUM-SYUEKI-GK        PIC S9(10).
           03  SUM-SEIKYU-GK        PIC S9(10).
           03  SUM-RYOSYU-GK        PIC S9(10).
           03  SUM-SSYUEKIKEN-GK    PIC 9(10).
           03  SUM-SSEIKYUKEN-GK    PIC 9(10).
           03  SUM-SRYOSYUKEN-GK    PIC 9(10).
           03  SUM-RSYUEKIKEN-GK    PIC 9(10).
           03  SUM-RSEIKYUKEN-GK    PIC 9(10).
           03  SUM-RRYOSYUKEN-GK    PIC 9(10).
           03  SUM-OSYUEKIKEN-GK    PIC 9(10).
           03  SUM-OSEIKYUKEN-GK    PIC 9(10).
           03  SUM-ORYOSYUKEN-GK    PIC 9(10).
           03  SUM-JYUSHINKEN-GK    PIC 9(10).
           03  SUM-SASYUEKIKEN-GK   PIC 9(10).
           03  SUM-SASEIKYUKEN-GK   PIC 9(10).
           03  SUM-SARYOSYUKEN-GK   PIC 9(10).
           03  SUM-JIHI-MONEY-GK    PIC S9(10).
           03  SUM-JIHI-OKEN-GK     PIC 9(10).
           03  SUM-JIHI-RKEN-GK     PIC 9(10).
           03  SUM-JIHI-SKEN-GK     PIC 9(10).
           03  SUM-JIHI-SAKEN-GK    PIC 9(10).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  300000.
               05  WRK-SMEI-HOSPNUM         PIC 9(02).
               05  WRK-SMEI-SORT-KEY.
                   07  WRK-SMEI-NYUGAIKBN   PIC X(01).
                   07  WRK-SMEI-SRYKA       PIC X(02).
                   07  WRK-SMEI-PTID        PIC 9(10).
                   07  WRK-SMEI-DENPNUM     PIC 9(07).
                   07  WRK-SMEI-DENPEDANUM  PIC 9(02).
               05  WRK-SMEI-SKYMONEY        PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY    PIC S9(07).
               05  WRK-SMEI-NYUHEN-YMD      PIC X(08).
               05  WRK-SMEI-PTINFCRE-YMD    PIC X(08).
      *
           03  WRK-SV-SMEI-TBL              PIC X(54).
      *
           03  WRK-SV-SMEI-DENPNUM          PIC 9(07).
      *
       01   WRK-JYUSHIN-AREA.
           03   WRK-JYUSHIN-PTID-TBL     OCCURS 10000.
               05   WRK-JYUSHIN-PTID        PIC  9(10).
               05   WRK-NYUHEN-YMD          PIC  X(02).
      *
       01  WRK-CONS-AREA.
      *    ¼«ÈñÊÝ¸±ÈÖ¹æ
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    Ï«ºÒÊÝ¸±ÈÖ¹æ
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    ¼«ÇåÀÕÊÝ¸±ÈÖ¹æ
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    ¸ø³²ÊÝ¸±ÈÖ¹æ
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975".
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)  VALUE 100.
           03  WRK-ITEM                PIC X(100)  VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
           03  CSV-SAKENSU             OCCURS 20 TIMES PIC X(09).
           03  CSV-SAHKNKEN            PIC X(09).
           03  CSV-SASYUEKIKEN         PIC X(09).
           03  CSV-JIHI-SAKENSU        PIC X(09).
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-HKNTEN        PIC X(50)
                     VALUE "ÊÝ¸±ÅÀ¿ô(ÅÀ)".
               07  CSV-HEAD-SYUEKI        PIC X(50)
                     VALUE "°åÎÅ¼ý±×(Áí·×³Û)".
               07  CSV-HEAD-HKN1      PIC X(50).
               07  CSV-HEAD-HKN2      PIC X(50).
               07  CSV-HEAD-HKN3      PIC X(50).
               07  CSV-HEAD-HKN4      PIC X(50).
               07  CSV-HEAD-HKN5      PIC X(50).
               07  CSV-HEAD-HKN6      PIC X(50).
               07  CSV-HEAD-JIHI-G.
                 09  CSV-HEAD-SKY-JIHI1   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI2   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI3   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI4   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI5   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI6   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI7   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI8   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI9   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI10  PIC X(50)  VALUE SPACE.
               07  CSV-HEAD-JIHI-R        REDEFINES CSV-HEAD-JIHI-G.
                 09  CSV-HEAD-JIHI-OCC    OCCURS  10.
                   11  CSV-HEAD-JIHI-NAME PIC X(50).
               07  CSV-HEAD-SKY-JIHIKEI   PIC X(50)
                                          VALUE "¼«Èñ¾®·×".
               07  CSV-HEAD-SKY-JIHI      PIC X(50)
                     VALUE "¼«Èñ".
               07  CSV-HEAD-SKY-ROSAI     PIC X(50)
                     VALUE "Ï«ºÒ".
               07  CSV-HEAD-SKY-JIBAI     PIC X(50)
                     VALUE "¼«ÇåÀÕ".
               07  CSV-HEAD-SKY-KOUGAI    PIC X(50)
                     VALUE "¸ø³²".
               07  CSV-HEAD-SKYMONEY      PIC X(50)
                     VALUE "Áë¸ýÀÁµá¶â³Û".
               07  CSV-HEAD-RSYMONEY      PIC X(50)
                     VALUE "Áë¸ýÎÎ¼ý¶â³Û".
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 25.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE 25.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó¾ðÊó
           COPY    "CPSK1001.INC".
      *
      *    ¿ÇÎÅ²ÊÌÜ¾ðÊó¾ðÊó
           COPY    "CPSK1005.INC".
      *
      *    ´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó
           COPY  "CPSK1009.INC".
      *
      *    ¼«Èñ´ÉÍý¾ðÊó
           COPY  "CPSK1013.INC".
      *
      *    ½ÐÎÏÀè¥×¥ê¥ó¥¿Ì¾³ä¤êÅö¤Æ¾ðÊó
           COPY  "CPSK1031.INC".
      *
      *    Ï«ºÒ°åÎÅµ¡´Ø¾ðÊó
           COPY  "CPSK4001.INC".
      *
      *    ´µ¼ÔÊÝ¸±¥Þ¥¹¥¿
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ´µ¼ÔÈÖ¹æÊÑ´¹
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¼ýÇ¼ÌÀºÙ¥Þ¥¹¥¿
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¼õ¿ÇÍúÎò¾ðÊó
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  JYURRK2-REC.
           COPY    "CPJYURRK.INC"      REPLACING  //JYURRK//
                                       BY         //JYURRK2//.
      *
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    Ï«ºÒÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    °õºþ£Ä£Â¹¹¿·¥µ¥Ö
           COPY    "CPORCSPRT.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *
      *    Åý·×£Ã£Ó£ÖÀ©¸æ¥µ¥Ö
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ½éºÆ¿Ç¼èÆÀ¥µ¥Ö
           COPY    "CPORCSS003.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    °ì»þ¥Õ¥¡¥¤¥ëÌ¾ÊÔ½¸
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END   =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SUM-AREA-GK
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  WRK-JYUSHIN-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-PRTYM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                 JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID     TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO   JOB-SHELLID
           MOVE    "A00000M501"       TO   JOB-PGID
           MOVE    "·î·×É½¹ç·×¡ÊÁë¸ýÎÎ¼ý¶â¡Ë"  TO   JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
      *    ½é¿Ç¡¿ºÆ¿ÇÈ½Äê¥µ¥Ö½é´ü½èÍý
           INITIALIZE                      SS003-AREA
           MOVE    ZERO            TO      SS003-SYORIKBN
           CALL    "ORCSS003"      USING   SS003-AREA
                                           SPA-AREA
      *
           INITIALIZE                     SGETTEMP-AREA
           MOVE    RECEERR                TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"          USING  SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)  TO  RECEERR
      *
      *    ¥Ñ¥é¥á¥¿ÊÔ½¸½èÍý
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¥¿ÊÔ½¸½èÍý
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    ¥·¥¹¥Æ¥àÆüÉÕ¥»¥Ã¥È
           MOVE    LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-SYSYMDWH
      *
      *    ÂÐ¾ÝÇ¯·îºÇ½éÆüÀßÄê
           MOVE    WRK-PARA-PRTYM        TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                  TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    ÂÐ¾ÝÇ¯·îºÇ½ªÆü¼èÆÀ
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM        TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD              TO   WRK-PARA-PRTLYMD
      *
      *    ÂÐ¾ÝÇ¯·îÆüÊÔ½¸
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTLYMD      TO   WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)     TO   WRK-PRTYMWH
      *
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           MOVE    SPACE                 TO   SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO   SYS-1001-KANRICD
           MOVE    "*"                   TO   SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO   SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD      TO   SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD      TO   SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC          TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC         TO   SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO   WRK-PARA-HOSPNUM
           ELSE
             MOVE    "°åÎÅµ¡´Ø£É£Ä¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                         TO   WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      *    ¼«Èñ¾ðÊó¼èÆÀ
      *    ¶èÊ¬11
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "11"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(1)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(1)
           END-IF
      *
      *    ¶èÊ¬12
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "12"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(2)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(2)
           END-IF
      *
      *    ¶èÊ¬13
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "13"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD      TO   SYS-1013-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(3)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(3)
           END-IF
      *
      *    ¶èÊ¬14
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "14"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(4)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(4)
           END-IF
      *
      *    ¶èÊ¬15
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "15"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2    =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(5)
           ELSE
             MOVE   SPACE                  TO   WRK-TJIHIN(5)
           END-IF
      *
      *    ¶èÊ¬16
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "16"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHIN(6)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(6)
           END-IF
      *
      *    ¶èÊ¬17
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "17"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(7)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(7)
           END-IF
      *
      *    ¶èÊ¬18
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "18"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD      TO   SYS-1013-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(8)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(8)
           END-IF
      *
      *    ¶èÊ¬19
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "19"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(9)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(9)
           END-IF
      *
      *    ¶èÊ¬10
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "10"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(10)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(10)
           END-IF
      *
      *    ¼«Èñ¾ðÊó¼èÆÀ
      *    ¶èÊ¬21
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "21"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(1)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(1)
           END-IF
      *
      *    ¶èÊ¬22
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "22"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(2)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(2)
           END-IF
      *
      *    ¶èÊ¬23
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "23"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(3)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(3)
           END-IF
      *
      *    ¶èÊ¬24
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "24"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(4)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(4)
           END-IF
      *
      *    ¶èÊ¬25
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "25"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(5)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(5)
           END-IF
      *
      *    ¶èÊ¬26
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "26"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(6)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(6)
           END-IF
      *
      *    ¶èÊ¬27
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "27"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2    =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(7)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(7)
           END-IF
      *
      *    ¶èÊ¬28
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "28"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(8)
           ELSE
             MOVE  SPACE               TO  WRK-TJIHI(8)
           END-IF
      *
      *    ¶èÊ¬29
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "29"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(9)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHI(9)
           END-IF
      *
      *    ¶èÊ¬20
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "20"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2       =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(10)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(10)
           END-IF
      *
      *    Ï«ºÒ°åÎÅµ¡´Ø¾ðÊó
           MOVE    SPACE               TO   SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
           MOVE    "4001"              TO   SYS-4001-KANRICD
           MOVE    "*"                 TO   SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-4001-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
               MOVE  MCPDATA-REC    TO  SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5    TO   WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12      TO   WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10      TO   WRK-4001-TENTANKA
             END-EVALUATE
      *
             IF  SYS-4001-KOMUKBN  =  "2"
      *    ¸øÌ³ºÒ³²(·òÊÝ´ð½à)»þ¤ÎÅÀ¿ôÃ±²ÁÀßÄê
               MOVE    SYS-4001-KOMUTENTANKA-X TO  WRK-4001-KOMUTENTANKA
      *    ¸øÌ³ºÒ³²¶èÊ¬¤¬·òÊÝ´ð½à¤Î¾ì¹ç¡¢£±¤ò³ÊÇ¼
               MOVE    "1"        TO  WRK-4001-KOMUFLG
             END-IF
           ELSE
             MOVE  10   TO  WRK-4001-TENTANKA
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM  310-DATA-SELECT-SEC
      *
           PERFORM  UNTIL   FLG-END   =   1
      *    ¼ýÇ¼¾ðÊó½¸·×½èÍý
             PERFORM 320-BODY-HEN-SEC
           END-PERFORM
      *
      *    Ä¢É¼ÊÔ½¸<¿ÇÎÅ²Ê¹ç·×¥Ú¡¼¥¸>°õºþ½èÍý
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               IF   WRK-PRNSRKA-CNT   >   1
                 PERFORM 330-TERM-KA-PRINT-SEC
               END-IF
             WHEN "2"
               IF   WRK-PRGSRKA-CNT   >   1
                 PERFORM 330-TERM-KA-PRINT-SEC
               END-IF
           END-EVALUATE
      *
      *    Ä¢É¼ÊÔ½¸<¹ç·×¥Ú¡¼¥¸>°õºþ½èÍý
           PERFORM 330-TERM-GK-PRINT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÂÐ¾Ý¥Ç¡¼¥¿ÁªÄê½èÍý
      *****************************************************************
       310-DATA-SELECT-SEC          SECTION.
      *
           MOVE    0     TO    IDX-S
      *
           PERFORM   910-DBSELECT-SEC
           PERFORM   UNTIL   FLG-SYUMEI   =   1
             PERFORM   900-SYUNOU-READ-SEC
      *
             IF      FLG-SYUNOU             =   ZERO
               AND   SYU-DENPJTIKBN     NOT =  "3"
               AND ((SMEI-JOUTAIKBN     NOT =  "8"
               AND   SMEI-JOUTAIKBN     NOT =  "3")
               OR    SMEI-SKYMONEY      NOT =  0
               OR    SMEI-NYUHEN-MONEY  NOT =  0)
      *
               ADD   1                  TO  IDX-S
               MOVE  SMEI-HOSPNUM       TO  WRK-SMEI-HOSPNUM(IDX-S)
               MOVE  SMEI-NYUGAIKBN     TO  WRK-SMEI-NYUGAIKBN(IDX-S)
               MOVE  SMEI-SRYKA         TO  WRK-SMEI-SRYKA(IDX-S)
               MOVE  SMEI-PTID          TO  WRK-SMEI-PTID(IDX-S)
               MOVE  SMEI-DENPNUM       TO  WRK-SMEI-DENPNUM(IDX-S)
               MOVE  SMEI-DENPEDANUM    TO  WRK-SMEI-DENPEDANUM(IDX-S)
               MOVE  SMEI-SKYMONEY      TO  WRK-SMEI-SKYMONEY(IDX-S)
               MOVE  SMEI-NYUHEN-MONEY  TO  WRK-SMEI-NYUHEN-MONEY(IDX-S)
               MOVE  SMEI-NYUHEN-YMD    TO  WRK-SMEI-NYUHEN-YMD(IDX-S)
               MOVE  PTINF-CREYMD       TO  WRK-SMEI-PTINFCRE-YMD(IDX-S)
             END-IF
             PERFORM   910-DBFETCH-SEC
           END-PERFORM
           PERFORM   910-DBCLOSE-SEC
      *
      *    ¥½¡¼¥È½èÍý
           MOVE IDX-S TO IDX-MAX
      *
           IF   IDX-MAX   >    1
             MOVE    1        TO   FLG-GET
             MOVE    ZERO     TO   FLG-END
             MOVE 1  TO  IDX-S
             MOVE 2  TO  IDX-N
             PERFORM UNTIL IDX-S NOT  <  IDX-MAX
               PERFORM  UNTIL IDX-N  >  IDX-MAX
                 IF WRK-SMEI-SORT-KEY(IDX-S)
                     >  WRK-SMEI-SORT-KEY(IDX-N)
                   MOVE   WRK-SMEI-TBL(IDX-N)   TO   WRK-SV-SMEI-TBL
                   MOVE   WRK-SMEI-TBL(IDX-S)   TO   WRK-SMEI-TBL(IDX-N)
                   MOVE   WRK-SV-SMEI-TBL       TO   WRK-SMEI-TBL(IDX-S)
                 END-IF
                 ADD    1      TO   IDX-N
               END-PERFORM
               ADD 1  TO  IDX-S
               COMPUTE IDX-N = IDX-S + 1
             END-PERFORM
           ELSE
             IF   IDX-MAX    =   ZERO
               MOVE   ZERO         TO   FLG-GET
               MOVE   1            TO   FLG-END
             ELSE
               MOVE   1            TO   FLG-GET
               MOVE   ZERO         TO   FLG-END
             END-IF
           END-IF
      *
           MOVE     1       TO   IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¹ç·×¥«¥¦¥ó¥È>½èÍý
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO     TO  CNT-LINE
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO  WRK-SRYKA
      *
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
      *    Æþ±¡¿ÇÎÅ²Ê¿ô¥«¥¦¥ó¥È¥¢¥Ã¥×
               MOVE   1    TO WRK-PRNSRKA-CNT
             WHEN "2"
      *    ³°Íè¿ÇÎÅ²Ê¿ô¥«¥¦¥ó¥È¥¢¥Ã¥×
               MOVE   1    TO WRK-PRGSRKA-CNT
           END-EVALUATE
      *
           PERFORM   UNTIL   FLG-END     =   1
      *    ·ï¿ôÍÑ¥Õ¥é¥°¤Î½é´ü²½
             INITIALIZE   FLG-KENSUU
      *
      *    ¼ýÇ¼¥Æ¡¼¥Ö¥ëºÆÆÉ¤ß¹þ¤ß
             PERFORM 901-SYUNOU-READ-SEC
             IF    FLG-SYUNOU   =   ZERO
      *
      *    Æþ³°¶èÊ¬¥Ö¥ì¥¤¥¯¥Á¥§¥Ã¥¯½èÍý
             IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT = WRK-NYUGAIKBN
      *    ¹ç·×¥«¥¦¥ó¥¿¡¼¤Ë²Ã»»
               ADD    SUM-HKNTEN      TO   SUM-HKNTEN-GK
               ADD    SUM-SHKNKEN     TO   SUM-SHKNKEN-GK
               ADD    SUM-RHKNKEN     TO   SUM-RHKNKEN-GK
               ADD    SUM-OHKNKEN     TO   SUM-OHKNKEN-GK
               ADD  SUM-SAHKNKEN      TO   SUM-SAHKNKEN-GK
               PERFORM  VARYING   IDX2   FROM  1  BY 1
                        UNTIL   IDX2   >   20
                 ADD   SUM-MONEY(IDX2)   TO   SUM-MONEY-GK(IDX2)
                 ADD   SUM-RKEN(IDX2)    TO   SUM-RKEN-GK(IDX2)
                 ADD   SUM-SKEN(IDX2)    TO   SUM-SKEN-GK(IDX2)
                 ADD   SUM-OKEN(IDX2)    TO   SUM-OKEN-GK(IDX2)
                 ADD   SUM-SAKEN(IDX2)   TO   SUM-SAKEN-GK(IDX2)
               END-PERFORM
               ADD    SUM-SYUEKI         TO   SUM-SYUEKI-GK
               ADD    SUM-SEIKYU         TO   SUM-SEIKYU-GK
               ADD    SUM-RYOSYU         TO   SUM-RYOSYU-GK
               ADD    SUM-SSYUEKIKEN     TO   SUM-SSYUEKIKEN-GK
               ADD    SUM-SSEIKYUKEN     TO   SUM-SSEIKYUKEN-GK
               ADD    SUM-SRYOSYUKEN     TO   SUM-SRYOSYUKEN-GK
               ADD    SUM-RSYUEKIKEN     TO   SUM-RSYUEKIKEN-GK
               ADD    SUM-RSEIKYUKEN     TO   SUM-RSEIKYUKEN-GK
               ADD    SUM-RRYOSYUKEN     TO   SUM-RRYOSYUKEN-GK
               ADD    SUM-OSYUEKIKEN     TO   SUM-OSYUEKIKEN-GK
               ADD    SUM-OSEIKYUKEN     TO   SUM-OSEIKYUKEN-GK
               ADD    SUM-ORYOSYUKEN     TO   SUM-ORYOSYUKEN-GK
               ADD    SUM-SASYUEKIKEN    TO   SUM-SASYUEKIKEN-GK
               ADD    SUM-SASEIKYUKEN    TO   SUM-SASEIKYUKEN-GK
               ADD    SUM-SARYOSYUKEN    TO   SUM-SARYOSYUKEN-GK
               ADD   SUM-JIHI-MONEY      TO  SUM-JIHI-MONEY-GK
               ADD   SUM-JIHI-OKEN       TO  SUM-JIHI-OKEN-GK
               ADD   SUM-JIHI-RKEN       TO  SUM-JIHI-RKEN-GK
               ADD   SUM-JIHI-SKEN       TO  SUM-JIHI-SKEN-GK
               ADD   SUM-JIHI-SAKEN      TO  SUM-JIHI-SAKEN-GK
               IF   WRK-NYUGAIKBN   =      "1"
                AND WRK-PRNSRKA-CNT >      1
                    PERFORM 330-TERM-KA-PRINT-SEC
               END-IF
               PERFORM 330-TERM-GK-PRINT-SEC
               INITIALIZE           SUM-AREA-GK
               INITIALIZE           SUM-AREA
      *
      *    ¥Ö¥ì¥¤¥¯¥­¡¼¹àÌÜÀßÄê
               MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
               MOVE   WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      *
      *    PTID¥Ö¥ì¥¤¥¯¥­¡¼¹àÌÜ¤ÎÀßÄê
               INITIALIZE  WRK-PTID
      *
               EVALUATE WRK-NYUGAIKBN
                 WHEN "1"
      *    Æþ±¡¿ÇÎÅ²Ê¿ô¥«¥¦¥ó¥È¥¢¥Ã¥×
                   ADD   1    TO   WRK-PRNSRKA-CNT
                 WHEN "2"
      *    ³°Íè¿ÇÎÅ²Ê¿ô¥«¥¦¥ó¥È¥¢¥Ã¥×
                   ADD   1    TO   WRK-PRGSRKA-CNT
               END-EVALUATE
      *
             END-IF
      *
      *    ¿ÇÎÅ²Ê¥Ö¥ì¥¤¥¯¥Á¥§¥Ã¥¯½èÍý
             IF  WRK-SMEI-SRYKA(IDX-S)  NOT = WRK-SRYKA
               PERFORM 330-TERM-KA-PRINT-SEC
      *    ¹ç·×¥«¥¦¥ó¥¿¡¼¤Ë²Ã»»
               ADD   SUM-HKNTEN       TO   SUM-HKNTEN-GK
               ADD   SUM-SHKNKEN      TO   SUM-SHKNKEN-GK
               ADD   SUM-RHKNKEN      TO   SUM-RHKNKEN-GK
               ADD   SUM-OHKNKEN      TO   SUM-OHKNKEN-GK
               ADD   SUM-SAHKNKEN     TO   SUM-SAHKNKEN-GK
               PERFORM   VARYING   IDX2   FROM  1  BY 1
                         UNTIL   IDX2   >   20
                 ADD   SUM-MONEY(IDX2)    TO   SUM-MONEY-GK(IDX2)
                 ADD   SUM-RKEN(IDX2)     TO   SUM-RKEN-GK(IDX2)
                 ADD   SUM-SKEN(IDX2)     TO   SUM-SKEN-GK(IDX2)
                 ADD   SUM-OKEN(IDX2)     TO   SUM-OKEN-GK(IDX2)
                 ADD   SUM-SAKEN(IDX2)    TO   SUM-SAKEN-GK(IDX2)
               END-PERFORM
               ADD   SUM-SYUEKI          TO   SUM-SYUEKI-GK
               ADD   SUM-SEIKYU          TO   SUM-SEIKYU-GK
               ADD   SUM-RYOSYU          TO   SUM-RYOSYU-GK
               ADD   SUM-SSYUEKIKEN      TO   SUM-SSYUEKIKEN-GK
               ADD   SUM-SSEIKYUKEN      TO   SUM-SSEIKYUKEN-GK
               ADD   SUM-SRYOSYUKEN      TO   SUM-SRYOSYUKEN-GK
               ADD   SUM-RSYUEKIKEN      TO   SUM-RSYUEKIKEN-GK
               ADD   SUM-RSEIKYUKEN      TO   SUM-RSEIKYUKEN-GK
               ADD   SUM-RRYOSYUKEN      TO   SUM-RRYOSYUKEN-GK
               ADD   SUM-OSYUEKIKEN      TO   SUM-OSYUEKIKEN-GK
               ADD   SUM-OSEIKYUKEN      TO   SUM-OSEIKYUKEN-GK
               ADD   SUM-ORYOSYUKEN      TO   SUM-ORYOSYUKEN-GK
               ADD   SUM-SASYUEKIKEN     TO   SUM-SASYUEKIKEN-GK
               ADD   SUM-SASEIKYUKEN     TO   SUM-SASEIKYUKEN-GK
               ADD   SUM-SARYOSYUKEN     TO   SUM-SARYOSYUKEN-GK
               ADD   SUM-JIHI-MONEY      TO  SUM-JIHI-MONEY-GK
               ADD   SUM-JIHI-OKEN       TO  SUM-JIHI-OKEN-GK
               ADD   SUM-JIHI-RKEN       TO  SUM-JIHI-RKEN-GK
               ADD   SUM-JIHI-SKEN       TO  SUM-JIHI-SKEN-GK
               ADD   SUM-JIHI-SAKEN      TO  SUM-JIHI-SAKEN-GK
               INITIALIZE                SUM-AREA
      *
      *    ¥Ö¥ì¥¤¥¯¥­¡¼¹àÌÜÀßÄê
               MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
               MOVE    WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      *
      *    PTID¥Ö¥ì¥¤¥¯¥­¡¼¹àÌÜ¤ÎÀßÄê
               INITIALIZE  WRK-PTID
               EVALUATE WRK-NYUGAIKBN
                 WHEN "1"
      *    Æþ±¡¿ÇÎÅ²Ê¿ô¥«¥¦¥ó¥È¥¢¥Ã¥×
                   ADD    1    TO   WRK-PRNSRKA-CNT
                 WHEN  "2"
      *    ³°Íè¿ÇÎÅ²Ê¿ô¥«¥¦¥ó¥È¥¢¥Ã¥×
                   ADD    1    TO   WRK-PRGSRKA-CNT
               END-EVALUATE
      *
             END-IF
      *
      *    ÅÁÉ¼ÈÖ¹æ»ÞÈÖ¤¬£±¤Î¾ì¹ç¤ÏÊÝ¸±ÅÀ¿ô¡¢ÊÝ¸±ÊÌÀÁµá³Û¤ÎÌÀºÙ¤ò°õ»ú(³«»Ï)
             IF   WRK-SMEI-DENPEDANUM(IDX-S)   =     1
               IF   FLG-SYUNOU  = ZERO
                 IF  WRK-PARA-PRTYM  <  "200804"
                   PERFORM   320-BODY-HEN-SUB-SEC
                 ELSE
                   PERFORM   330-BODY-HEN-SUB-SEC
                 END-IF
      *    ¼õ¿ÇÍúÎò½¸·×
                 IF SYU-NYUGAIKBN = "1"
      *    Æþ±¡¤Î¾ì¹ç¤Ï½é¿Ç¡¢ºÆ¿ÇÊÌ¤Ë·ï¿ô¤ò½¸·×¤¹¤ë¡£
                   PERFORM   320-CNT-SEC
                 ELSE
      *    ³°Íè¤Î¾ì¹ç¤Ï½é¿Ç¡¢ºÆ¿ÇÊÌ¤Ë·ï¿ô¤ò½¸·×¤¹¤ë¡£
                   PERFORM  320-JYURRK-CNT-SEC
                 END-IF
               END-IF
             END-IF
      *
      *    ÀÁµá¶â³Û
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-SKYMONEY(IDX-S)   TO   WRK-SZ
             IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =   0
               ADD   WRK-SMEI-SKYMONEY(IDX-S)   TO   SUM-SEIKYU
               MOVE    1           TO    FLG-SEIKYU
             END-IF
      *    ÎÎ¼ý¶â³Û
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-NYUHEN-MONEY(IDX-S) TO WRK-SZ
             IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
               ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)  TO   SUM-RYOSYU
               MOVE      1        TO   FLG-RYOSYU
             END-IF
      *
      *    ¼õ¿ÇÍúÎò½¸·×
           END-IF
      *    ¼ýÇ¼¥Æ¡¼¥Ö¥ë¼ýÇ¼¥Æ¡¼¥Ö¥ëºÆÆÉ¤ß¹þ¤ß
      *
      *    PTID¥Ö¥ì¥¤¥¯¥­¡¼¹àÌÜ¤ÎÀßÄê
           ADD  1     TO  IDX-S
      *    ÇÛÎó¤Î¥Ç¡¼¥¿¤òÁ´·ï½ÐÎÏ¤·¤¿¾ì¹ç¤Ï¹ç·×É½¤Î°õºþ¤ò¹Ô¤¦°Ù¤Ë¥ë¡¼¥×¤è¤êÃ¦½Ð
           IF IDX-S   >   IDX-MAX
             MOVE    1       TO   FLG-END
           END-IF
      *
           END-PERFORM
      *
      *    ¹ç·×¥«¥¦¥ó¥¿¡¼¤Ë²Ã»»
           ADD   SUM-HKNTEN      TO   SUM-HKNTEN-GK
           ADD   SUM-SHKNKEN     TO   SUM-SHKNKEN-GK
           ADD   SUM-RHKNKEN     TO   SUM-RHKNKEN-GK
           ADD   SUM-OHKNKEN     TO   SUM-OHKNKEN-GK
           ADD   SUM-SAHKNKEN    TO   SUM-SAHKNKEN-GK
           PERFORM   VARYING  IDX2   FROM  1  BY 1
                     UNTIL        IDX2   >     20
             ADD    SUM-MONEY(IDX2)     TO   SUM-MONEY-GK(IDX2)
             ADD    SUM-RKEN(IDX2)      TO   SUM-RKEN-GK(IDX2)
             ADD    SUM-SKEN(IDX2)      TO   SUM-SKEN-GK(IDX2)
             ADD    SUM-OKEN(IDX2)      TO   SUM-OKEN-GK(IDX2)
             ADD    SUM-SAKEN(IDX2)     TO   SUM-SAKEN-GK(IDX2)
           END-PERFORM
           ADD    SUM-SYUEKI            TO   SUM-SYUEKI-GK
           ADD    SUM-SEIKYU            TO   SUM-SEIKYU-GK
           ADD    SUM-RYOSYU            TO   SUM-RYOSYU-GK
           ADD    SUM-SSYUEKIKEN        TO   SUM-SSYUEKIKEN-GK
           ADD    SUM-SSEIKYUKEN        TO   SUM-SSEIKYUKEN-GK
           ADD    SUM-SRYOSYUKEN        TO   SUM-SRYOSYUKEN-GK
           ADD    SUM-RSYUEKIKEN        TO   SUM-RSYUEKIKEN-GK
           ADD    SUM-RSEIKYUKEN        TO   SUM-RSEIKYUKEN-GK
           ADD    SUM-RRYOSYUKEN        TO   SUM-RRYOSYUKEN-GK
           ADD    SUM-OSYUEKIKEN        TO   SUM-OSYUEKIKEN-GK
           ADD    SUM-OSEIKYUKEN        TO   SUM-OSEIKYUKEN-GK
           ADD    SUM-ORYOSYUKEN        TO   SUM-ORYOSYUKEN-GK
           ADD    SUM-SASYUEKIKEN       TO   SUM-SASYUEKIKEN-GK
           ADD    SUM-SASEIKYUKEN       TO   SUM-SASEIKYUKEN-GK
           ADD    SUM-SARYOSYUKEN       TO   SUM-SARYOSYUKEN-GK
           ADD    SUM-JIHI-MONEY        TO  SUM-JIHI-MONEY-GK
           ADD    SUM-JIHI-OKEN         TO  SUM-JIHI-OKEN-GK
           ADD    SUM-JIHI-RKEN         TO  SUM-JIHI-RKEN-GK
           ADD    SUM-JIHI-SKEN         TO  SUM-JIHI-SKEN-GK
           ADD    SUM-JIHI-SAKEN        TO  SUM-JIHI-SAKEN-GK
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¹ç·×¥«¥¦¥ó¥È>¥µ¥Ö½èÍý
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *    ÊÝ¸±ÈÖ¹æ¾ðÊóÆÉ¤ß¹þ¤ß
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =   1
             MOVE     10     TO   HKN-TENTNK
           END-IF
      *
      *    ÊÝ¸±ÅÀ¿ô
           MOVE     ZERO      TO   SYU-TOTAL-HKNTEN
      *
           PERFORM  VARYING IDZ  FROM  1   BY  1
                    UNTIL   IDZ   >   16
             ADD    SYU-HKNTEN(IDZ)     TO   SYU-TOTAL-HKNTEN
           END-PERFORM
      *
           MOVE    SYU-TOTAL-HKNTEN     TO   WRK-Z9
           ADD     SYU-TOTAL-HKNTEN     TO   SUM-HKNTEN
           MOVE     1                   TO   FLG-HKN
      *
      *    ÊÝ¸±ÊÌ¶â³Û¡Ê¼ÒÊÝËÜ¿Í¡Á¼«ÇåÀÕ¡Ë
           MOVE    ZERO                     TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                    TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5                  TO   IDX
               ELSE
                 MOVE    4                  TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    3                  TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1              TO   IDX
                   WHEN    "2"
                     MOVE    2              TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      * ÊÝ¸±ÊÌ¶èÊ¬Îã³°¾ò·ï(¼«Èñ¾ò·ï³ÈÄ¥)
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE    17          TO    IDX
           END-IF
      *
           IF   IDX   >   ZERO
      *
      *    ¼«Èñ°Ê³°¤Î¤È¤­¼«ÈñÊ¬¤À¤±½¸·×
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
               IF   SYU-SRYYMD  <=  "20070331"
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
               ELSE
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
               END-IF
      *

             COMPUTE WRK-MONEY   =   SYU-SKYMONEY   -   WRK-JIHI3
      *
             MOVE    WRK-MONEY     TO   WRK-Z9
             ADD     WRK-MONEY     TO   SUM-MONEY(IDX)
               IF   WRK-JIHI   >   ZERO
                 PERFORM  VARYING IDX2  FROM 1  BY  1
                          UNTIL    IDX2    >    10
                   INITIALIZE   WRK-JIHI2
                   COMPUTE   WRK-JIHI2   =   SYU-JIHI-TAXNASI(IDX2)
                                         +   SYU-JIHI-TAXARI(IDX2)
                   IF   WRK-JIHI2  >  ZERO
                     MOVE   WRK-JIHI2    TO   WRK-ZZ
                     COMPUTE  JIHIIDX  =  IDX2  +  6
                     ADD   WRK-JIHI2     TO   SUM-MONEY(JIHIIDX)
                     MOVE    1      TO   FLG-JIHI(IDX2)
                   END-IF
                 END-PERFORM
               END-IF
           END-IF
      *
           EVALUATE IDX
             WHEN 17
      *    ¼«Èñ¤Î¾ì¹ç
               MOVE  SYU-SKYMONEY  TO WRK-SYUEKI
             WHEN 18
      *    Ï«ºÒ¤Î¾ì¹ç
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó¼èÆÀ
               PERFORM  900-PTRSIINF-READ-SEC
      *    ÅÀ¿ôÃ±²Á¤òÀßÄê
               IF  PTRSI-HKNKBN  =  "5"
                   AND  WRK-4001-KOMUFLG  =  "1"
      *    ¸øÌ³ºÒ³²¡Ê·òÊÝ´ð½à¡Ë¤Î¾ì¹ç
                 MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
               ELSE
                 MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
               END-IF
      *
               COMPUTE  WRK-SYUEKI ROUNDED =
                          (SYU-TOTAL-HKNTEN  *  WRK-TENTNK)
                           +  WRK-JIHI
                           +  SYU-RSISHOSHIN-MONEY
                           +  SYU-RSISAISHIN-MONEY
                           +  SYU-RSISDO-MONEY
                           +  SYU-RSIETC-MONEY
                           +  SYU-CHOSEI
                           +  SYU-YKZ-FTN
                           +  SYU-RYOYOHI-SKJ
                           +  SYU-RYOYOHI-SKJ-JIHI
                           +  SYU-RYOYOHI-LIFE
                           +  SYU-RYOYOHI-LIFE-JIHI
                           +  SYU-RMSAGAKU
             WHEN 19
      *    ¼«ÇåÀÕ¤Î¾ì¹ç
               MOVE    SYU-RSIJIBAI-SKYMONEY     TO   WRK-SYUEKI
      *    ¸ø³²¤Î¾ì¹ç
             WHEN 20
               COMPUTE  WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      *    Ï«ºÒ¡¢¼«ÇåÀÕ°Ê³°¤Î¾ì¹ç
               COMPUTE  WRK-SYUEKI   =
                             (SYU-TOTAL-HKNTEN * HKN-TENTNK)
                              +  WRK-JIHI
                              +  SYU-CHOSEI
                              +  SYU-YKZ-FTN
                              +  SYU-RYOYOHI-SKJ
                              +  SYU-RYOYOHI-SKJ-JIHI
                              +  SYU-RYOYOHI-LIFE
                              +  SYU-RYOYOHI-LIFE-JIHI
                              +  SYU-RMSAGAKU
               IF  SYU-DISCOUNT-MONEY  >  0
                 COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                      -  SYU-DISCOUNT-MONEY
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                   WHEN  1
                     COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  2
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  3
                     IF   WRK-SYUEKI(8:1)  NOT =  0
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10  +  10
                     END-IF
                 END-EVALUATE
               END-IF
           END-EVALUATE
           ADD     WRK-SYUEKI   TO   SUM-SYUEKI
           MOVE    1            TO   FLG-SYUEKI
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¹ç·×¥«¥¦¥ó¥È>¥µ¥Ö½èÍý(H20Ç¯4·î°Ê¹ß)
      *****************************************************************
       330-BODY-HEN-SUB-SEC      SECTION.
      *
      *    ÊÝ¸±ÈÖ¹æ¾ðÊóÆÉ¤ß¹þ¤ß
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM   =   1
             MOVE   10     TO  HKN-TENTNK
           END-IF
      *
      *    ÊÝ¸±ÅÀ¿ô
           MOVE    ZERO  TO  SYU-TOTAL-HKNTEN
      *
           PERFORM  VARYING IDZ   FROM  1   BY  1
                    UNTIL   IDZ   >  16
             ADD   SYU-HKNTEN(IDZ)  TO   SYU-TOTAL-HKNTEN
           END-PERFORM
      *
           MOVE    SYU-TOTAL-HKNTEN   TO   WRK-Z9
           ADD     SYU-TOTAL-HKNTEN   TO   SUM-HKNTEN
           MOVE    1                  TO   FLG-HKN
      *
      *    ÊÝ¸±ÊÌ¶â³Û¡Ê¼ÒÊÝËÜ¿Í¡Á¼«ÇåÀÕ¡Ë
           MOVE    ZERO                TO  IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                    TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    4                    TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF      SYU-KOH1HKNNUM    =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 MOVE    3                  TO   IDX
               END-IF
             WHEN    OTHER
               IF      SYU-KOH1HKNNUM    =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1           TO   IDX
                   WHEN    "2"
                     MOVE    2           TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   SYU-SYUHKNNUM(1:2)  =  "98"
             MOVE    17    TO      IDX
           END-IF
      *
           IF    IDX   >   ZERO
      *
      *      ¼«Èñ¿ÇÎÅ  Ã¼¿ô½èÍý
             PERFORM  340-JIHIHASU-HEN-SEC
      *
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
               IF  SYU-SRYYMD  <=  "20070331"
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
               ELSE
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE WRK-JIHI3   =   WRK-JIHI-TTL
               END-IF
      *
             COMPUTE WRK-MONEY       =   SYU-SKYMONEY
                                     -   WRK-JIHI3
      *
             MOVE    WRK-MONEY     TO  WRK-Z9
             ADD     WRK-MONEY     TO  SUM-MONEY (IDX)
               IF   WRK-JIHI    >   ZERO
                 PERFORM  VARYING IDX2  FROM 1  BY  1
                          UNTIL    IDX2    >   10
                   INITIALIZE WRK-JIHI2
                   COMPUTE WRK-JIHI2  =
                               SYU-JIHI-TAXNASI(IDX2)
                            +  SYU-JIHI-TAXARI(IDX2)
                   IF  WRK-JIHI2  >  ZERO
                     MOVE WRK-JIHI2  TO  WRK-ZZ
                     COMPUTE  JIHIIDX  =  IDX2  +  6
                     ADD    WRK-JIHI2  TO     SUM-MONEY(JIHIIDX)
                     MOVE    1  TO   FLG-JIHI(IDX2)
                   END-IF
                 END-PERFORM
                 ADD    WRK-JIHI-TTL   TO  SUM-JIHI-MONEY
               END-IF
           END-IF
      *
           EVALUATE IDX
             WHEN 17
      *    ¼«Èñ¤Î¾ì¹ç
               MOVE  SYU-SKYMONEY    TO   WRK-SYUEKI
             WHEN 18
      *    Ï«ºÒ¤Î¾ì¹ç
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó¼èÆÀ
               PERFORM  900-PTRSIINF-READ-SEC
      *    ÅÀ¿ôÃ±²Á¤òÀßÄê
               IF  PTRSI-HKNKBN  =  "5"
                   AND  WRK-4001-KOMUFLG  =  "1"
      *    ¸øÌ³ºÒ³²¡Ê·òÊÝ´ð½à¡Ë¤Î¾ì¹ç
                 MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
               ELSE
                 MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
               END-IF
      *
               COMPUTE  WRK-SYUEKI ROUNDED =
                        (SYU-TOTAL-HKNTEN * WRK-TENTNK)
                            +  WRK-JIHI
                            +  SYU-RSISHOSHIN-MONEY
                            +  SYU-RSISAISHIN-MONEY
                            +  SYU-RSISDO-MONEY
                            +  SYU-RSIETC-MONEY
                            +  SYU-CHOSEI
                            +  SYU-YKZ-FTN
                            +  SYU-RYOYOHI-SKJ
                            +  SYU-RYOYOHI-SKJ-JIHI
                            +  SYU-RYOYOHI-LIFE
                            +  SYU-RYOYOHI-LIFE-JIHI
                            +  SYU-RMSAGAKU
             WHEN 19
      *    ¼«ÇåÀÕ¤Î¾ì¹ç
               MOVE  SYU-RSIJIBAI-SKYMONEY  TO  WRK-SYUEKI
             WHEN 20
      *    ¸ø³²¤Î¾ì¹ç
               COMPUTE   WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      *    Ï«ºÒ¡¢¼«ÇåÀÕ°Ê³°¤Î¾ì¹ç
               COMPUTE  WRK-SYUEKI =
                  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
                            +  WRK-JIHI
                            +  SYU-CHOSEI
                            +  SYU-YKZ-FTN
                            +  SYU-RYOYOHI-SKJ
                            +  SYU-RYOYOHI-SKJ-JIHI
                            +  SYU-RYOYOHI-LIFE
                            +  SYU-RYOYOHI-LIFE-JIHI
                            +  SYU-RMSAGAKU
               IF  SYU-DISCOUNT-MONEY  >  0
                 COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                      -  SYU-DISCOUNT-MONEY
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                   WHEN  1
                     COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  2
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  3
                     IF   WRK-SYUEKI(8:1)  NOT =  0
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10  +  10
                     END-IF
                 END-EVALUATE
               END-IF
           END-EVALUATE
           ADD     WRK-SYUEKI   TO   SUM-SYUEKI
           MOVE      1          TO   FLG-SYUEKI
      *
           .
       330-BODY-HEN-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Èñ¿ÇÎÅ  Ã¼¿ô½èÍý
      *****************************************************************
       340-JIHIHASU-HEN-SEC          SECTION.
      *
           COMPUTE  WRK-JIHI-TTL  =  SYU-JIHI-TOTAL
                                  +  SYU-JIHI-TOTAL-TAX
      *
           EVALUATE  SYU-SYUHKNNUM
      *    Ï«ºÒ
           WHEN   "971"
             MOVE    SYS-1001-DISCOUNT-HASU-KBN3   TO  WRK-HASU-KBN
             MOVE    SYS-1001-DISCOUNT-HASU-KBN3J  TO  WRK-HASU-KBNJ
      *    ¼«ÇåÀÕ
           WHEN   "973"
             MOVE    SYS-1001-DISCOUNT-HASU-KBN4   TO  WRK-HASU-KBN
             MOVE    SYS-1001-DISCOUNT-HASU-KBN4J  TO  WRK-HASU-KBNJ
      *    ¸ø³²
           WHEN   "975"
             MOVE    SYS-1001-DISCOUNT-HASU-KBN5J  TO  WRK-HASU-KBNJ
      *    °åÊÝ
           WHEN   OTHER
             MOVE    SYS-1001-DISCOUNT-HASU-KBN2   TO  WRK-HASU-KBN
             MOVE    SYS-1001-DISCOUNT-HASU-KBN2J  TO  WRK-HASU-KBNJ
      *      Âè»°¼Ô¹Ô°Ù
             IF  SYU-KOH1HKNNUM  =  "970"
               MOVE    SYS-1001-DISCOUNT-HASU-KBN6   TO  WRK-HASU-KBN
               MOVE    SYS-1001-DISCOUNT-HASU-KBN6J  TO  WRK-HASU-KBNJ
             END-IF
           END-EVALUATE
      *
      *    ¼«Èñ¿ÇÎÅ  Ã¼¿ô¥µ¥Ö½èÍý
           PERFORM  3401-JIHIHASU-SUB-SEC
      *
           .
       340-JIHIHASU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Èñ¿ÇÎÅ  Ã¼¿ô¥µ¥Ö½èÍý
      *****************************************************************
       3401-JIHIHASU-SUB-SEC          SECTION.
      *
      *    ÀÁµá³ÛÃ¼¿ôÀßÄê¡Ê¸ºÌÈÌµ¡¦¼«ÈñÊ¬¡Ë
           EVALUATE  WRK-HASU-KBNJ
      *    £±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ
           WHEN  "1"
             COMPUTE  WRK-JIHI-TTL ROUNDED = WRK-JIHI-TTL / 10
             COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *    £±£°±ßÌ¤ËþÀÚ¤ê¼Î¤Æ
           WHEN  "2"
             COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL / 10
             COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *    £±£°±ßÌ¤ËþÀÚ¤ê¾å¤²
           WHEN  "3"
             IF  WRK-JIHI-TTL(7:1)  NOT =  ZERO
               COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL / 10
               COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL * 10 + 10
             END-IF
      *    ÊÝ¸±Ê¬¤Ë½à¤º¤ë
           WHEN  "0"
      *      ÀÁµá³ÛÃ¼¿ôÀßÄê¡Ê¸ºÌÈÌµ¡¦ÊÝ¸±Ê¬¡Ë
             EVALUATE  WRK-HASU-KBN
      *      £±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ
             WHEN  "1"
               COMPUTE  WRK-JIHI-TTL ROUNDED = WRK-JIHI-TTL / 10
               COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *      £±£°±ßÌ¤ËþÀÚ¤ê¼Î¤Æ
             WHEN  "2"
               COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL / 10
               COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *      £±£°±ßÌ¤ËþÀÚ¤ê¾å¤²
             WHEN  "3"
               IF  WRK-JIHI-TTL(7:1)  NOT =  ZERO
                 COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL / 10
                 COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL * 10 + 10
               END-IF
             WHEN  OTHER
               CONTINUE
             END-EVALUATE
      *    £±£°±ßÌ¤ËþÃ¼¿ô½èÍý¤Ê¤·
           WHEN  OTHER
             CONTINUE
           END-EVALUATE
      *
           .
       3401-JIHIHASU-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ·ï¿ô½¸·×¥µ¥Ö½èÍý
      *****************************************************************
       320-CNT-SEC                    SECTION.
      *
      *    ÊÝ¸±ÊÌ·ï¿ô¥«¥¦¥ó¥È
           ADD     1   TO   SUM-RKEN(IDX)
      *    ¼«ÈñÊ¬·ï¿ô¥«¥¦¥ó¥È
           PERFORM  VARYING IDX2  FROM 1  BY  1
                     UNTIL  IDX2   >  10
             IF  FLG-JIHI(IDX2)  =  1
               COMPUTE  JIHIIDX  =  IDX2  +  6
               ADD    1   TO  SUM-RKEN(JIHIIDX)
               ADD    1   TO  SUM-JIHI-RKEN
             END-IF
           END-PERFORM
      *    ÊÝ¸±ÅÀ¿ô·ï¿ô¥«¥¦¥ó¥È
           IF  FLG-HKN = 1
               ADD   1  TO   SUM-RHKNKEN
           END-IF
      *    °åÎÅ¼ý±×·×·ï¿ô¥«¥¦¥ó¥È
           IF  FLG-SYUEKI = 1
               ADD   1  TO   SUM-RSYUEKIKEN
           END-IF
      *    Áë¸ýÀÁµá¶â³Û·ï¿ô¥«¥¦¥ó¥È
           IF  FLG-SEIKYU = 1
               ADD   1  TO   SUM-RSEIKYUKEN
           END-IF
      *    Áë¸ýÎÎ¼ý¶â³Û·ï¿ô¥«¥¦¥ó¥È
           IF  FLG-RYOSYU = 1
               ADD   1  TO   SUM-RRYOSYUKEN
           END-IF
           .
       320-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¾ðÊó½¸·×¥µ¥Ö½èÍý
      *****************************************************************
       320-JYURRK-CNT-SEC      SECTION.
      *
           MOVE JYURRK-SRYYMD(7:2) TO  IDX7G
           IF WRK-PARA-PRTYM  <  "200804"
             PERFORM 320-HOKEN-GROUPING-SEC
           ELSE
             PERFORM 330-HOKEN-GROUPING-SEC
           END-IF
      *
      *    ¿·´µ·ï¿ô
           PERFORM  900-JYURRK-SELECT-SEC
           IF     FLG-JYURRK     =  ZERO
             AND  JYURRK-SRYYMD  =  SYU-SRYYMD
             AND  JYURRK-SRYKA   =  SYU-SRYKA
             AND  JYURRK-DENPNUM =  SYU-DENPNUM
      *
             INITIALIZE                 WRK-SYOSINYMD
                                        FLG-SRYKARRK
             MOVE    SYU-SRYYMD         TO   WRK-SYOSINYMD
             PERFORM  900-SRYKARRK-SELECT-SEC
             PERFORM  UNTIL  FLG-SRYKARRK  =  1
               IF     KARRK-SYOSINYMD1      <  WRK-SYOSINYMD
                 AND  KARRK-SYOSINYMD1  NOT =  SPACE
                 MOVE    KARRK-SYOSINYMD1     TO   WRK-SYOSINYMD
               END-IF
               PERFORM  900-SRYKARRK-FETCH-SEC
             END-PERFORM
             PERFORM  900-SRYKARRK-CLOSE-SEC
      *
             IF   WRK-SYOSINYMD  =  SYU-SRYYMD
      *    ÊÝ¸±ÊÌ·ï¿ô¥«¥¦¥ó¥È
               ADD    1   TO   SUM-SKEN(IDX)
      *    ¼«ÈñÊ¬·ï¿ô¥«¥¦¥ó¥È
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL   IDX2   >  10
                 IF  FLG-JIHI(IDX2)  = 1
                   COMPUTE  JIHIIDX  =  IDX2  +  6
                   ADD   1   TO  SUM-SKEN(JIHIIDX)
                   ADD   1   TO  SUM-JIHI-SKEN
                 END-IF
               END-PERFORM
      *    ÊÝ¸±ÅÀ¿ô·ï¿ô¥«¥¦¥ó¥È
               IF   FLG-HKN = 1
                 ADD    1    TO   SUM-SHKNKEN
               END-IF
      *    °åÎÅ¼ý±×·×·ï¿ô¥«¥¦¥ó¥È
               IF   FLG-SYUEKI = 1
                 ADD    1    TO   SUM-SSYUEKIKEN
               END-IF
      *    Áë¸ýÀÁµá¶â³Û·ï¿ô¥«¥¦¥ó¥È
               IF   FLG-SEIKYU = 1
                 ADD    1    TO   SUM-SSEIKYUKEN
               END-IF
      *    Áë¸ýÎÎ¼ý¶â³Û·ï¿ô¥«¥¦¥ó¥È
               IF   FLG-RYOSYU = 1
                 ADD    1    TO   SUM-SRYOSYUKEN
               END-IF
             END-IF
           END-IF
           PERFORM  900-JYURRK-CLOSE-SEC
      *
      *    ½é¿Ç¡¿ºÆ¿ÇÈ½Äê¥µ¥Ö
           INITIALIZE                          SS003-AREA
           MOVE    "1"                 TO      SS003-SYORIKBN
           MOVE    SYU-HOSPNUM         TO      SS003-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO      SS003-NYUGAIKBN
           MOVE    SYU-PTID            TO      SS003-PTID
           MOVE    SYU-DENPNUM         TO      SS003-DENPNUM
           CALL    "ORCSS003"          USING   SS003-AREA
                                               SPA-AREA
      *
           EVALUATE      SS003-SYOSAISIN-KBN
      *    ½é¿Ç·ï¿ô¥«¥¦¥ó¥È
           WHEN    "1"
      *    ÊÝ¸±ÊÌ·ï¿ô¥«¥¦¥ó¥È
             ADD    1   TO   SUM-SAKEN(IDX)
             MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *      ¼«ÈñÊ¬·ï¿ô¥«¥¦¥ó¥È
             PERFORM  VARYING IDX2  FROM 1  BY  1
                      UNTIL    IDX2  >  10
               IF  FLG-JIHI(IDX2)  =  1
                 COMPUTE  JIHIIDX  =  IDX2  +  6
                 ADD   1   TO  SUM-SAKEN(JIHIIDX)
                 ADD   1   TO  SUM-JIHI-SAKEN
               END-IF
             END-PERFORM
      *      ÊÝ¸±ÅÀ¿ô·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-HKN = 1
               ADD   1   TO   SUM-SAHKNKEN
             END-IF
      *      °åÎÅ¼ý±×·×·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-SYUEKI = 1
               ADD   1   TO   SUM-SASYUEKIKEN
             END-IF
      *      Áë¸ýÀÁµá¶â³Û·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-SEIKYU = 1
               ADD   1   TO   SUM-SASEIKYUKEN
             END-IF
      *      Áë¸ýÎÎ¼ý¶â³Û·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-RYOSYU = 1
               ADD   1   TO   SUM-SARYOSYUKEN
             END-IF
      *
      *    ºÆ¿Ç·ï¿ô¥«¥¦¥ó¥È
           WHEN    "2"
      *      ÊÝ¸±ÊÌ·ï¿ô¥«¥¦¥ó¥È
             ADD     1   TO   SUM-RKEN(IDX)
             MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *      ¼«ÈñÊ¬·ï¿ô¥«¥¦¥ó¥È
             PERFORM  VARYING IDX2  FROM 1  BY  1
                      UNTIL    IDX2  >  10
               IF  FLG-JIHI(IDX2)  = 1
                 COMPUTE   JIHIIDX  =  IDX2  +  6
                 ADD     1      TO    SUM-RKEN(JIHIIDX)
                 ADD     1      TO    SUM-JIHI-RKEN
               END-IF
             END-PERFORM
      *      ÊÝ¸±ÅÀ¿ô·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-HKN = 1
                  ADD   1  TO   SUM-RHKNKEN
             END-IF
      *      °åÎÅ¼ý±×·×·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-SYUEKI = 1
                  ADD   1  TO   SUM-RSYUEKIKEN
             END-IF
      *      Áë¸ýÀÁµá¶â³Û·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-SEIKYU = 1
                  ADD   1  TO   SUM-RSEIKYUKEN
             END-IF
      *      Áë¸ýÎÎ¼ý¶â³Û·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-RYOSYU = 1
                  ADD   1  TO   SUM-RRYOSYUKEN
             END-IF
      *
      *    ¤½¤ÎÂ¾·ï¿ô¥«¥¦¥ó¥È
           WHEN    OTHER
      *      ÊÝ¸±ÊÌ·ï¿ô¥«¥¦¥ó¥È
             ADD     1   TO   SUM-OKEN(IDX)
      *      ¼«ÈñÊ¬·ï¿ô¥«¥¦¥ó¥È
             PERFORM  VARYING IDX2  FROM 1  BY  1
                      UNTIL    IDX2       >    10
               IF  FLG-JIHI(IDX2)  = 1
                 COMPUTE  JIHIIDX  =  IDX2  +  6
                 ADD   1   TO  SUM-OKEN(JIHIIDX)
                 ADD   1   TO  SUM-JIHI-OKEN
               END-IF
             END-PERFORM
      *      ÊÝ¸±ÅÀ¿ô·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-HKN = 1
               ADD   1  TO   SUM-OHKNKEN
             END-IF
      *      °åÎÅ¼ý±×·×·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-SYUEKI = 1
               ADD   1  TO   SUM-OSYUEKIKEN
             END-IF
      *      Áë¸ýÀÁµá¶â³Û·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-SEIKYU = 1
               ADD   1  TO   SUM-OSEIKYUKEN
             END-IF
      *      Áë¸ýÎÎ¼ý¶â³Û·ï¿ô¥«¥¦¥ó¥È
             IF  FLG-RYOSYU = 1
               ADD   1  TO   SUM-ORYOSYUKEN
             END-IF
           END-EVALUATE
      *
           MOVE     1        TO      IDX3
           PERFORM  UNTIL    IDX3    >      10000
             IF  (WRK-SMEI-PTID(IDX-S) =  WRK-JYUSHIN-PTID(IDX3))
                 AND (WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                                =  WRK-NYUHEN-YMD(IDX3))
      *
               MOVE   10000     TO      IDX3
             END-IF
             IF  WRK-JYUSHIN-PTID(IDX3)  =  ZERO
                 AND  WRK-NYUHEN-YMD(IDX3)  =  SPACE
                 AND  (IDX3 NOT = 10000)
               MOVE   WRK-SMEI-PTID(IDX-S)   TO  WRK-JYUSHIN-PTID(IDX3)
               MOVE   WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                                             TO  WRK-NYUHEN-YMD(IDX3)
               ADD    1         TO    SUM-JYUSHINKEN-GK
               MOVE   10000     TO    IDX3
             END-IF
             ADD   1          TO     IDX3
           END-PERFORM
      *
           .
       320-JYURRK-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¥°¥ë¡¼¥×È½ÊÌ½èÍý
      *****************************************************************
       320-HOKEN-GROUPING-SEC      SECTION.
      *
      *    ÊÝ¸±ÊÌ¶â³Û¡Ê¼ÒÊÝËÜ¿Í¡Á¼«ÇåÀÕ¡Ë
           MOVE    ZERO                TO  IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6               TO  IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO  IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO  IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO  IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO  IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF      SYU-KOH1HKNNUM   =   "027"
                 MOVE    5                   TO  IDX
               ELSE
                 MOVE    4                   TO  IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    3                   TO  IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1                   TO  IDX
                   WHEN    "2"
                     MOVE    2                   TO  IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      *    ÊÝ¸±ÊÌ¶èÊ¬Îã³°¾ò·ï(¼«Èñ¾ò·ï³ÈÄ¥)
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE    17    TO      IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±¥°¥ë¡¼¥×È½ÊÌ½èÍý(H20Ç¯4·î°Ê¹ß)
      *****************************************************************
       330-HOKEN-GROUPING-SEC      SECTION.
      *
      *    ÊÝ¸±ÊÌ¶â³Û¡Ê¼ÒÊÝËÜ¿Í¡Á¼«ÇåÀÕ¡Ë
           MOVE    ZERO                     TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                    TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    4                    TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF      SYU-KOH1HKNNUM      =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 MOVE    3                  TO   IDX
               END-IF
             WHEN    OTHER
               IF      SYU-KOH1HKNNUM      =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1              TO   IDX
                   WHEN    "2"
                     MOVE    2              TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE      17      TO      IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»É½¼¨½èÍý
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÆÉ¤ß½Ð¤·
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM         TO   COMB-HOSPNUM
           MOVE    SYU-PTID            TO   COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO   COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI   =   ZERO
      *    ´µ¼ÔÊÝ¸±¾ðÊóÆÉ¤ß¹þ¤ß
             INITIALIZE              PTHKNINF-REC
             MOVE    SYU-HOSPNUM       TO  PTHKN-HOSPNUM
             MOVE    SYU-PTID          TO  PTHKN-PTID
             MOVE    COMB-HKNID        TO  PTHKN-HKNID
             PERFORM   900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼<¿ÇÎÅ²Ê¹ç·×>°õºþ½èÍý
      *****************************************************************
      *
       330-TERM-KA-PRINT-SEC         SECTION.
      *
           INITIALIZE                HC17B
                                     HC17BV02
      *
           MOVE    WRK-SYSYMDWH      TO    HC17B-SYSYMD
                                           HC17BV02-SYSYMD
           MOVE    WRK-PRTYMWH       TO    HC17B-SRYYM
                                           HC17BV02-SRYYM
           MOVE    "ÂÐ¾ÝÇ¯·î"        TO    HC17B-MIDASI
                                           HC17BV02-MIDASI
      *
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               MOVE "Æþ±¡"         TO  HC17B-NYUGAIKBN
                                       HC17BV02-NYUGAIKBN
      *    Æþ±¡¼«ÈñÌ¾¾ÎÀßÄê
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHIN(IDX2)    TO   HC17B-TJIHI(IDX2)
                                                 HC17BV02-TJIHI(IDX2)
               END-PERFORM
               MOVE   "ÀÁµá·ï¿ô"    TO   HC17B-KENSU1(1)
                                         HC17BV02-KENSU1(1)
               MOVE   "ÀÁµá·ï¿ô"    TO   HC17B-KENSU1(2)
                                         HC17BV02-KENSU1(2)
             WHEN "2"
               MOVE   "³°Íè"        TO  HC17B-NYUGAIKBN
                                        HC17BV02-NYUGAIKBN
      *    ³°Íè¼«ÈñÌ¾¾ÎÀßÄê
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHI(IDX2)  TO   HC17B-TJIHI(IDX2)
                                              HC17BV02-TJIHI(IDX2)
               END-PERFORM
               MOVE   "ºÆ¿Ç·ï¿ô"    TO   HC17B-KENSU1(1)
                                         HC17BV02-KENSU1(1)
               MOVE   "½é¿Ç·ï¿ô"    TO   HC17B-KENSU2(1)
                                         HC17BV02-KENSU2(1)
               MOVE   "¤½¤ÎÂ¾·ï¿ô"  TO   HC17B-KENSU4(1)
                                         HC17BV02-KENSU4(1)
               MOVE   "(¿·´µ¿ô)"    TO   HC17B-KENSU3(1)
                                         HC17BV02-KENSU3(1)
               MOVE   "ºÆ¿Ç·ï¿ô"    TO   HC17B-KENSU1(2)
                                         HC17BV02-KENSU1(2)
               MOVE   "½é¿Ç·ï¿ô"    TO   HC17B-KENSU2(2)
                                         HC17BV02-KENSU2(2)
               MOVE   "¤½¤ÎÂ¾·ï¿ô"  TO   HC17B-KENSU4(2)
                                         HC17BV02-KENSU4(2)
               MOVE   "(¿·´µ¿ô)"    TO   HC17B-KENSU3(2)
                                         HC17BV02-KENSU3(2)
           END-EVALUATE
      *
      *    ¿ÇÎÅ²ÊÌ¾¾Î¼èÆÀ
           INITIALIZE                   SYS-1005-REC
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO   SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD     TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD     TO   SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2    =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1005-REC
             MOVE   SYS-1005-SRYKANAME  TO   HC17B-SRYKA
                                             HC17BV02-SRYKA
           ELSE
             MOVE   SPACE               TO   HC17B-SRYKA
                                             HC17BV02-SRYKA
           END-IF
      *
           IF  FLG-GET = ZERO
      *    ÂÐ¾Ý¥Ç¡¼¥¿¤¬£±·ï¤âÂ¸ºß¤·¤Ê¤«¤Ã¤¿¾ì¹ç
             MOVE  "¡ö¡ö¡ö  ÂÐ¾Ý¥Ç¡¼¥¿¤¬£±·ï¤âÂ¸ºß¤·¤Þ¤»¤ó¡£¡ö¡ö¡ö"
                                   TO  HC17B-JOBMSG
                                       HC17BV02-JOBMSG
             ADD     1         TO   CNT-PAGE
             MOVE    CNT-PAGE  TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
                                    HC17BV02-PAGE
           ELSE
      *    ÂÐ¾Ý¥Ç¡¼¥¿¤¬£±·ï°Ê¾åÂ¸ºß¤·¤¿¾ì¹ç
             ADD     1         TO   CNT-PAGE
             MOVE    CNT-PAGE  TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
                                    HC17BV02-PAGE
      *    ÊÝ¸±ÅÀ¿ô¡¢·ï¿ô¹ç·×
             MOVE    SUM-HKNTEN   TO  WRK-Z9
             MOVE    WRK-Z9   TO  HC17B-GHKNTEN
                                  HC17BV02-GHKNTEN
             IF   WRK-NYUGAIKBN   =   "2"
               MOVE    SUM-SAHKNKEN        TO   WRK-Z9
               MOVE    WRK-Z9              TO   HC17B-SHKNKEN
                                                HC17BV02-SHKNKEN
               MOVE    SUM-SHKNKEN         TO   WRK-Z9
               MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
               MOVE    1                   TO   WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K      TO   HC17B-SAHKNKEN(2:8)
                                           HC17BV02-SAHKNKEN(2:8)
               MOVE    WRK-TMP-K      TO   CSV-SAHKNKEN
               MOVE    "("            TO   HC17B-SAHKNKEN(WRK-COUNTS:1)
                                      HC17BV02-SAHKNKEN(WRK-COUNTS:1)
               MOVE    ")"            TO   HC17B-SAHKNKEN(10:1)
                                           HC17BV02-SAHKNKEN(10:1)
               MOVE    SUM-OHKNKEN    TO   WRK-Z9
               MOVE    WRK-Z9         TO   HC17B-OHKNTEN
                                           HC17BV02-OHKNTEN
             END-IF
             MOVE    SUM-RHKNKEN   TO   WRK-Z9
             MOVE    WRK-Z9        TO   HC17B-RHKNKEN
                                        HC17BV02-RHKNKEN
      *
      *    ÊÝ¸±ÊÌ¶â³Û¡¢·ï¿ô¹ç·×
             PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL    IDX   >    20
               IF    IDX = 7
                   OR  IDX = 8
                   OR  IDX = 9
                   OR  IDX = 10
                   OR  IDX = 11
                   OR  IDX = 12
                   OR  IDX = 13
                   OR  IDX = 14
                   OR  IDX = 15
                   OR  IDX = 16
                 COMPUTE IDX8 = IDX - 6
                 EVALUATE WRK-NYUGAIKBN
                   WHEN "1"
                     MOVE   SUM-MONEY(IDX)    TO   WRK-S9
                     MOVE   WRK-S9            TO   HC17B-GMONEY(IDX)
                                                   HC17BV02-GMONEY(IDX)
                     MOVE   SUM-RKEN(IDX)     TO   WRK-Z9
                     MOVE   WRK-Z9            TO   HC17B-RKENSU(IDX)
                                                   HC17BV02-RKENSU(IDX)
                   WHEN "2"
                     MOVE   SUM-MONEY(IDX)   TO   WRK-S9
                     MOVE   WRK-S9           TO   HC17B-GMONEY(IDX)
                                                  HC17BV02-GMONEY(IDX)
                     MOVE   SUM-RKEN(IDX)    TO   WRK-Z9
                     MOVE   WRK-Z9           TO   HC17B-RKENSU(IDX)
                                                  HC17BV02-RKENSU(IDX)
                     MOVE   SUM-OKEN(IDX)    TO   WRK-Z9
                     MOVE   WRK-Z9           TO   HC17B-OKENSU(IDX)
                                                  HC17BV02-OKENSU(IDX)
                     MOVE    SUM-SAKEN(IDX)    TO   WRK-Z9
                     MOVE    WRK-Z9            TO  HC17B-SKENSU (IDX)
                                                   HC17BV02-SKENSU (IDX)
                     MOVE    SUM-SKEN (IDX)    TO   WRK-Z9
                     MOVE    WRK-Z9(3:8)       TO   WRK-TMP-K
                     MOVE    1                 TO   WRK-COUNTS
                     INSPECT WRK-TMP-K  TALLYING
                             WRK-COUNTS FOR LEADING SPACE
                     MOVE    WRK-TMP-K   TO   HC17B-SAKENSU(IDX)(2:8)
                                              HC17BV02-SAKENSU(IDX)(2:8)
                     MOVE    WRK-TMP-K   TO   CSV-SAKENSU(IDX)
                     MOVE    "("   TO   HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                                   HC17BV02-SAKENSU(IDX)(WRK-COUNTS:1)
                     MOVE    ")"   TO   HC17B-SAKENSU(IDX)(10:1)
                                        HC17BV02-SAKENSU(IDX)(10:1)
                 END-EVALUATE
               ELSE
                 MOVE    SUM-MONEY(IDX)    TO   WRK-S9
                 MOVE    WRK-S9            TO   HC17B-GMONEY(IDX)
                                                HC17BV02-GMONEY(IDX)
                 MOVE    SUM-RKEN(IDX)     TO   WRK-Z9
                 MOVE    WRK-Z9            TO   HC17B-RKENSU(IDX)
                                                HC17BV02-RKENSU(IDX)
                 IF WRK-NYUGAIKBN =   "2"
                  MOVE    SUM-SAKEN(IDX)      TO   WRK-Z9
                   MOVE    WRK-Z9              TO   HC17B-SKENSU(IDX)
                                                    HC17BV02-SKENSU(IDX)
                   MOVE    SUM-SKEN (IDX)      TO   WRK-Z9
                   MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
                   MOVE    1                   TO   WRK-COUNTS
                   INSPECT WRK-TMP-K  TALLYING
                           WRK-COUNTS FOR LEADING SPACE
                   MOVE    WRK-TMP-K  TO  HC17B-SAKENSU(IDX)(2:8)
                                          HC17BV02-SAKENSU(IDX)(2:8)
                   MOVE    WRK-TMP-K  TO  CSV-SAKENSU(IDX)
                   MOVE    "("     TO  HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                                   HC17BV02-SAKENSU(IDX)(WRK-COUNTS:1)
                   MOVE    ")"     TO  HC17B-SAKENSU(IDX)(10:1)
                                       HC17BV02-SAKENSU(IDX)(10:1)
                   MOVE    SUM-OKEN (IDX)       TO  WRK-Z9
                   MOVE    WRK-Z9   TO  HC17B-OKENSU  (IDX)
                                        HC17BV02-OKENSU  (IDX)
                 END-IF
               END-IF
             END-PERFORM
      *
      *      ¼«Èñ¾®·×
             EVALUATE WRK-NYUGAIKBN
               WHEN "1"
                 MOVE    SUM-JIHI-MONEY  TO  WRK-S9
                 MOVE    WRK-S9          TO  HC17BV02-JIHI-GMONEY
                 MOVE    SUM-JIHI-RKEN   TO  WRK-Z9
                 MOVE    WRK-Z9          TO  HC17BV02-JIHI-RKENSU
               WHEN "2"
                 MOVE    SUM-JIHI-MONEY  TO  WRK-S9
                 MOVE    WRK-S9          TO  HC17BV02-JIHI-GMONEY
                 MOVE    SUM-JIHI-OKEN   TO  WRK-Z9
                 MOVE    WRK-Z9          TO  HC17BV02-JIHI-OKENSU
                 MOVE    SUM-JIHI-RKEN   TO  WRK-Z9
                 MOVE    WRK-Z9          TO  HC17BV02-JIHI-RKENSU
                 MOVE    SUM-JIHI-SKEN   TO  WRK-Z9
                 MOVE    WRK-Z9          TO  HC17BV02-JIHI-SKENSU
                 MOVE    SUM-JIHI-SAKEN  TO  WRK-Z9
                 MOVE    WRK-Z9(3:8)     TO  WRK-TMP-K
                 MOVE    1               TO  WRK-COUNTS
                 INSPECT  WRK-TMP-K   TALLYING
                          WRK-COUNTS  FOR  LEADING  SPACE
                 MOVE    WRK-TMP-K       TO  HC17BV02-JIHI-SAKENSU(2:8)
                 MOVE    WRK-TMP-K       TO  CSV-JIHI-SAKENSU
                 MOVE    "("     TO  HC17BV02-JIHI-SAKENSU(WRK-COUNTS:1)
                 MOVE    ")"     TO  HC17BV02-JIHI-SAKENSU(10:1)
             END-EVALUATE
      *
      *    °åÎÅ¼ý±×·×¡¢Áë¸ýÀÁµá¶â³Û¹ç·×¡¢Áë¸ýÎÎ¼ý¶â³Û¹ç·×
      *    ¶â³Û¹ç·×
             MOVE   SUM-SYUEKI     TO   WRK-S9
             MOVE   WRK-S9         TO   HC17B-GSYUEKI
                                        HC17BV02-GSYUEKI
             MOVE   SUM-SEIKYU     TO   WRK-S9
             MOVE   WRK-S9         TO   HC17B-GSEIKYU
                                        HC17BV02-GSEIKYU
             MOVE   SUM-RYOSYU     TO   WRK-S9
             MOVE   WRK-S9         TO   HC17B-GRYOSYU
                                        HC17BV02-GRYOSYU
      *
      *    ºÆ¿Ç
             MOVE  SUM-RSYUEKIKEN    TO   WRK-Z9
             MOVE  WRK-Z9            TO   HC17B-RSYUEKIKEN
                                          HC17BV02-RSYUEKIKEN
      *
      *    ½é¿Ç
             IF WRK-NYUGAIKBN   =  "2"
               MOVE    SUM-SASYUEKIKEN     TO  WRK-Z9
               MOVE    WRK-Z9              TO  HC17B-SSYUEKIKEN
                                               HC17BV02-SSYUEKIKEN
               MOVE    SUM-SSYUEKIKEN      TO  WRK-Z9
               MOVE    WRK-Z9(3:8)         TO  WRK-TMP-K
               MOVE    1                   TO  WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K  TO  HC17B-SASYUEKIKEN(2:8)
                                      HC17BV02-SASYUEKIKEN(2:8)
               MOVE    WRK-TMP-K  TO  CSV-SASYUEKIKEN
               MOVE    "("        TO  HC17B-SASYUEKIKEN(WRK-COUNTS:1)
                                      HC17BV02-SASYUEKIKEN(WRK-COUNTS:1)
               MOVE    ")"        TO  HC17B-SASYUEKIKEN(10:1)
                                      HC17BV02-SASYUEKIKEN(10:1)
      *    ¤½¤ÎÂ¾
               MOVE  SUM-OSYUEKIKEN    TO   WRK-Z9
               MOVE  WRK-Z9            TO   HC17B-OSYUEKIKEN
                                            HC17BV02-OSYUEKIKEN
             END-IF
      *
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *
           IF   WRK-PARA-PRTYM  >=  "201502"
             MOVE   "A00000M501NV02.red"         TO   SPRT-PRTID
             MOVE    "·î·×É½¹ç·×¡ÊÁë¸ýÎÎ¼ý¶â¡Ë"  TO   SPRT-TITLE
             MOVE    HC17BV02   TO  SPRT-PRTDATA
           ELSE
             IF  WRK-PARA-PRTYM   <  "200804"
               MOVE   "A00000M501O.red"          TO   SPRT-PRTID
             ELSE
               MOVE   "A00000M501N.red"          TO   SPRT-PRTID
             END-IF
             MOVE    "·î·×É½¹ç·×¡ÊÁë¸ýÎÎ¼ý¶â¡Ë"  TO   SPRT-TITLE
             MOVE    HC17B                       TO  SPRT-PRTDATA
           END-IF
      *
           MOVE    LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF   SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE    "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                          TO  WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      *    CSV¥Ø¥Ã¥ÀÊÔ½¸½èÍý
           PERFORM  800-CSV-HEAD-SEC
      *
      *    CSVÊÔ½¸
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-TERM-KA-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼<Æþ³°¶èÊ¬ÊÌÁ´²Ê¹ç·×>°õºþ½èÍý
      *****************************************************************
      *
       330-TERM-GK-PRINT-SEC         SECTION.
      *
           INITIALIZE            HC17B
                                 HC17BV02
      *
           MOVE    WRK-SYSYMDWH      TO   HC17B-SYSYMD
                                          HC17BV02-SYSYMD
           MOVE    WRK-PRTYMWH       TO   HC17B-SRYYM
                                          HC17BV02-SRYYM
           MOVE    "ÂÐ¾ÝÇ¯·î"        TO   HC17B-MIDASI
                                          HC17BV02-MIDASI
      *
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               MOVE   "Æþ±¡"         TO  HC17B-NYUGAIKBN
                                         HC17BV02-NYUGAIKBN
      *    Æþ±¡¼«ÈñÌ¾¾ÎÀßÄê
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHIN(IDX2)   TO   HC17B-TJIHI(IDX2)
                                                HC17BV02-TJIHI(IDX2)
               END-PERFORM
               MOVE   "ÀÁµá·ï¿ô"            TO  HC17B-KENSU1(1)
                                                HC17BV02-KENSU1(1)
               MOVE   "ÀÁµá·ï¿ô"            TO  HC17B-KENSU1(2)
                                                HC17BV02-KENSU1(2)
             WHEN "2"
               MOVE "³°Íè"         TO  HC17B-NYUGAIKBN
                                       HC17BV02-NYUGAIKBN
      *    ³°Íè¼«ÈñÌ¾¾ÎÀßÄê
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE     WRK-TJIHI(IDX2)   TO   HC17B-TJIHI(IDX2)
                                                 HC17BV02-TJIHI(IDX2)
               END-PERFORM
               MOVE    "ºÆ¿Ç·ï¿ô"    TO   HC17B-KENSU1(1)
                                          HC17BV02-KENSU1(1)
               MOVE    "½é¿Ç·ï¿ô"    TO   HC17B-KENSU2(1)
                                          HC17BV02-KENSU2(1)
               MOVE    "¤½¤ÎÂ¾·ï¿ô"  TO   HC17B-KENSU4(1)
                                          HC17BV02-KENSU4(1)
               MOVE    "(¿·´µ¿ô)"    TO   HC17B-KENSU3(1)
                                          HC17BV02-KENSU3(1)
               MOVE    "ºÆ¿Ç·ï¿ô"    TO   HC17B-KENSU1(2)
                                          HC17BV02-KENSU1(2)
               MOVE    "½é¿Ç·ï¿ô"    TO   HC17B-KENSU2(2)
                                          HC17BV02-KENSU2(2)
               MOVE    "¤½¤ÎÂ¾·ï¿ô"  TO   HC17B-KENSU4(2)
                                          HC17BV02-KENSU4(2)
               MOVE    "(¿·´µ¿ô)"    TO   HC17B-KENSU3(2)
                                          HC17BV02-KENSU3(2)
               MOVE    "¼õ¿Ç¼Ô¿ô"    TO   HC17B-TJYUSHIN
                                          HC17BV02-TJYUSHIN
           END-EVALUATE
      *    ¿ÇÎÅ²ÊÌ¾¾Î
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               IF   WRK-PRNSRKA-CNT  >   1
      *    ¿ÇÎÅ²Ê¤¬£²¤Ä°Ê¾å¤Î¾ì¹ç¤ÏÁ´²Ê¹ç·×¤òÀßÄê¤¹¤ë¡£
                 MOVE   "Á´²Ê¹ç·×"    TO  HC17B-SRYKA
                                          HC17BV02-SRYKA
               ELSE
      *    ¿ÇÎÅ²ÊÌ¾¾Î¼èÆÀ
      *    ¿ÇÎÅ²Ê¤¬£±¤Ä¤Î¾ì¹ç¤Ï¿ÇÎÅ²ÊÌ¾¾Î¤òÀßÄê¤¹¤ë¡£
                 INITIALIZE                 SYS-1005-REC
                 MOVE  "1005"               TO   SYS-1005-KANRICD
                 MOVE  WRK-SRYKA            TO   SYS-1005-KBNCD
                 MOVE  SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
                 MOVE  WRK-PARA-PRTLYMD     TO   WRK-SYMD
                 MOVE  WRK-PARA-PRTSYMD     TO   SYS-1005-EDYUKYMD
                 MOVE  WRK-PARA-PRTLYMD     TO   SYS-1005-STYUKYMD
                 MOVE  SYS-1005-REC         TO   MCPDATA-REC
                 PERFORM 820-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI2    =   ZERO
                   MOVE   MCPDATA-REC        TO   SYS-1005-REC
                   MOVE   SYS-1005-SRYKANAME TO   HC17B-SRYKA
                                                  HC17BV02-SRYKA
                 ELSE
                   MOVE  SPACE               TO   HC17B-SRYKA
                                                  HC17BV02-SRYKA
                 END-IF
               END-IF
             WHEN "2"
               IF  WRK-PRGSRKA-CNT  >   1
      *    ¿ÇÎÅ²Ê¤¬£²¤Ä°Ê¾å¤Î¾ì¹ç¤ÏÁ´²Ê¹ç·×¤òÀßÄê¤¹¤ë¡£
                 MOVE  "Á´²Ê¹ç·×"    TO  HC17B-SRYKA
                                         HC17BV02-SRYKA
               ELSE
      *    ¿ÇÎÅ²ÊÌ¾¾Î¼èÆÀ
      *    ¿ÇÎÅ²Ê¤¬£±¤Ä¤Î¾ì¹ç¤Ï¿ÇÎÅ²ÊÌ¾¾Î¤òÀßÄê¤¹¤ë¡£
                 INITIALIZE                SYS-1005-REC
                 MOVE  "1005"              TO   SYS-1005-KANRICD
                 MOVE  WRK-SRYKA           TO   SYS-1005-KBNCD
                 MOVE  SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
                 MOVE  WRK-PARA-PRTLYMD    TO   WRK-SYMD
                 MOVE  WRK-PARA-PRTSYMD    TO   SYS-1005-EDYUKYMD
                 MOVE  WRK-PARA-PRTLYMD    TO   SYS-1005-STYUKYMD
                 MOVE  SYS-1005-REC        TO  MCPDATA-REC
                 PERFORM 820-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI2        =   ZERO
                   MOVE  MCPDATA-REC       TO   SYS-1005-REC
                   MOVE  SYS-1005-SRYKANAME TO   HC17B-SRYKA
                                                 HC17BV02-SRYKA
                 ELSE
                   MOVE  SPACE              TO   HC17B-SRYKA
                                                 HC17BV02-SRYKA
                 END-IF
               END-IF
           END-EVALUATE
      *
           IF  FLG-GET = ZERO
      *    ÂÐ¾Ý¥Ç¡¼¥¿¤¬£±·ï¤âÂ¸ºß¤·¤Ê¤«¤Ã¤¿¾ì¹ç
             MOVE  "¡ö¡ö¡ö  ÂÐ¾Ý¥Ç¡¼¥¿¤¬£±·ï¤âÂ¸ºß¤·¤Þ¤»¤ó¡£¡ö¡ö¡ö"
                               TO   HC17B-JOBMSG
                                    HC17BV02-JOBMSG
             ADD     1         TO   CNT-PAGE
             MOVE    CNT-PAGE  TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
                                    HC17BV02-PAGE
           ELSE
      *
      *    ÂÐ¾Ý¥Ç¡¼¥¿¤¬£±·ï°Ê¾åÂ¸ºß¤·¤¿¾ì¹ç
             ADD     1         TO   CNT-PAGE
             MOVE    CNT-PAGE  TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
                                    HC17BV02-PAGE
      *    ÊÝ¸±ÅÀ¿ô¡¢·ï¿ô¹ç·×
             MOVE    SUM-HKNTEN-GK    TO   WRK-Z9
             MOVE    WRK-Z9           TO   HC17B-GHKNTEN
                                           HC17BV02-GHKNTEN
             IF WRK-NYUGAIKBN   =  "2"
               MOVE    SUM-SAHKNKEN-GK     TO   WRK-Z9
               MOVE    WRK-Z9              TO   HC17B-SHKNKEN
                                                HC17BV02-SHKNKEN
               MOVE    SUM-SHKNKEN-GK      TO   WRK-Z9
               MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
               MOVE    1                   TO   WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K  TO   HC17B-SAHKNKEN(2:8)
                                       HC17BV02-SAHKNKEN(2:8)
               MOVE    WRK-TMP-K  TO   CSV-SAHKNKEN
               MOVE    "("        TO   HC17B-SAHKNKEN(WRK-COUNTS:1)
                                       HC17BV02-SAHKNKEN(WRK-COUNTS:1)
               MOVE    ")"        TO   HC17B-SAHKNKEN(10:1)
                                       HC17BV02-SAHKNKEN(10:1)
               MOVE    SUM-OHKNKEN-GK   TO   WRK-Z9
               MOVE    WRK-Z9           TO   HC17B-OHKNTEN
                                             HC17BV02-OHKNTEN
             END-IF
             MOVE    SUM-RHKNKEN-GK   TO   WRK-Z9
             MOVE    WRK-Z9           TO   HC17B-RHKNKEN
                                           HC17BV02-RHKNKEN
      *
      *    ÊÝ¸±ÊÌ¶â³Û¡¢·ï¿ô¹ç·×
             PERFORM  VARYING IDX FROM    1   BY  1
                      UNTIL    IDX   >    20
               IF    IDX = 7
                 OR  IDX = 8
                 OR  IDX = 9
                 OR  IDX = 10
                 OR  IDX = 11
                 OR  IDX = 12
                 OR  IDX = 13
                 OR  IDX = 14
                 OR  IDX = 15
                 OR  IDX = 16
                 COMPUTE IDX8 = IDX - 6
                 EVALUATE WRK-NYUGAIKBN
                   WHEN "1"
                     MOVE   SUM-MONEY-GK(IDX)  TO   WRK-S9
                     MOVE   WRK-S9             TO   HC17B-GMONEY(IDX)
                                                    HC17BV02-GMONEY(IDX)
                     MOVE   SUM-RKEN-GK(IDX)   TO   WRK-Z9
                     MOVE   WRK-Z9             TO   HC17B-RKENSU(IDX)
                                                    HC17BV02-RKENSU(IDX)
                   WHEN "2"
                     MOVE   SUM-MONEY-GK (IDX) TO   WRK-S9
                     MOVE   WRK-S9             TO   HC17B-GMONEY(IDX)
                                                    HC17BV02-GMONEY(IDX)
                     MOVE   SUM-RKEN-GK (IDX)  TO   WRK-Z9
                     MOVE   WRK-Z9             TO   HC17B-RKENSU(IDX)
                                                    HC17BV02-RKENSU(IDX)
                     MOVE   SUM-OKEN-GK (IDX)  TO   WRK-Z9
                     MOVE   WRK-Z9   TO  HC17B-OKENSU(IDX)
                                         HC17BV02-OKENSU(IDX)
                     MOVE    SUM-SAKEN-GK(IDX)   TO   WRK-Z9
                     MOVE    WRK-Z9              TO   HC17B-SKENSU(IDX)
                                                    HC17BV02-SKENSU(IDX)
                     MOVE    SUM-SKEN-GK (IDX)   TO   WRK-Z9
                     MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
                     MOVE    1                   TO   WRK-COUNTS
                     INSPECT WRK-TMP-K  TALLYING
                             WRK-COUNTS FOR LEADING SPACE
                     MOVE    WRK-TMP-K  TO   HC17B-SAKENSU(IDX)(2:8)
                                             HC17BV02-SAKENSU(IDX)(2:8)
                     MOVE    WRK-TMP-K  TO   CSV-SAKENSU(IDX)
                     MOVE    "("   TO   HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                                   HC17BV02-SAKENSU(IDX)(WRK-COUNTS:1)
                     MOVE    ")"   TO   HC17B-SAKENSU(IDX)(10:1)
                                   HC17BV02-SAKENSU(IDX)(10:1)
                 END-EVALUATE
               ELSE
                 MOVE    SUM-MONEY-GK(IDX)   TO   WRK-S9
                 MOVE    WRK-S9              TO   HC17B-GMONEY(IDX)
                                                  HC17BV02-GMONEY(IDX)
                 MOVE    SUM-RKEN-GK(IDX)    TO   WRK-Z9
                 MOVE    WRK-Z9              TO   HC17B-RKENSU(IDX)
                                                  HC17BV02-RKENSU(IDX)
                 IF   WRK-NYUGAIKBN   =   "2"
                   MOVE    SUM-SAKEN-GK(IDX)   TO   WRK-Z9
                   MOVE    WRK-Z9              TO   HC17B-SKENSU(IDX)
                                                    HC17BV02-SKENSU(IDX)
                   MOVE    SUM-SKEN-GK (IDX)   TO   WRK-Z9
                   MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
                   MOVE    1                   TO   WRK-COUNTS
                   INSPECT WRK-TMP-K  TALLYING
                           WRK-COUNTS FOR LEADING SPACE
                   MOVE    WRK-TMP-K  TO  HC17B-SAKENSU(IDX)(2:8)
                                          HC17BV02-SAKENSU(IDX)(2:8)
                   MOVE    WRK-TMP-K  TO  CSV-SAKENSU(IDX)
                   MOVE    "("    TO  HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                                  HC17BV02-SAKENSU(IDX)(WRK-COUNTS:1)
                   MOVE    ")"    TO  HC17B-SAKENSU(IDX)(10:1)
                                  HC17BV02-SAKENSU(IDX)(10:1)
                   MOVE    SUM-OKEN-GK(IDX)       TO  WRK-Z9
                   MOVE    WRK-Z9    TO  HC17B-OKENSU(IDX)
                                         HC17BV02-OKENSU(IDX)
                 END-IF
               END-IF
             END-PERFORM
      *
      *      ¼«Èñ¾®·×
             EVALUATE WRK-NYUGAIKBN
               WHEN "1"
                 MOVE    SUM-JIHI-MONEY-GK  TO  WRK-S9
                 MOVE    WRK-S9             TO  HC17BV02-JIHI-GMONEY
                 MOVE    SUM-JIHI-RKEN-GK   TO  WRK-Z9
                 MOVE    WRK-Z9             TO  HC17BV02-JIHI-RKENSU
               WHEN "2"
                 MOVE    SUM-JIHI-MONEY-GK  TO  WRK-S9
                 MOVE    WRK-S9             TO  HC17BV02-JIHI-GMONEY
                 MOVE    SUM-JIHI-OKEN-GK   TO  WRK-Z9
                 MOVE    WRK-Z9             TO  HC17BV02-JIHI-OKENSU
                 MOVE    SUM-JIHI-RKEN-GK   TO  WRK-Z9
                 MOVE    WRK-Z9             TO  HC17BV02-JIHI-RKENSU
                 MOVE    SUM-JIHI-SKEN-GK   TO  WRK-Z9
                 MOVE    WRK-Z9             TO  HC17BV02-JIHI-SKENSU
                 MOVE    SUM-JIHI-SAKEN-GK  TO  WRK-Z9
                 MOVE    WRK-Z9(3:8)        TO  WRK-TMP-K
                 MOVE    1                  TO  WRK-COUNTS
                 INSPECT  WRK-TMP-K   TALLYING
                          WRK-COUNTS  FOR  LEADING  SPACE
                 MOVE    WRK-TMP-K        TO  HC17BV02-JIHI-SAKENSU(2:8)
                 MOVE    WRK-TMP-K        TO  CSV-JIHI-SAKENSU
                 MOVE    "("    TO  HC17BV02-JIHI-SAKENSU(WRK-COUNTS:1)
                 MOVE    ")"    TO  HC17BV02-JIHI-SAKENSU(10:1)
             END-EVALUATE
      *
      *    °åÎÅ¼ý±×·×¡¢Áë¸ýÀÁµá¶â³Û¹ç·×¡¢Áë¸ýÎÎ¼ý¶â³Û¹ç·×
      *    ¶â³Û¹ç·×
             MOVE  SUM-SYUEKI-GK      TO   WRK-S9
             MOVE  WRK-S9             TO   HC17B-GSYUEKI
                                           HC17BV02-GSYUEKI
             MOVE  SUM-SEIKYU-GK      TO   WRK-S9
             MOVE  WRK-S9             TO   HC17B-GSEIKYU
                                           HC17BV02-GSEIKYU
             MOVE  SUM-RYOSYU-GK      TO   WRK-S9
             MOVE  WRK-S9             TO   HC17B-GRYOSYU
                                           HC17BV02-GRYOSYU
      *
      *    ºÆ¿Ç
             MOVE  SUM-RSYUEKIKEN-GK    TO   WRK-Z9
             MOVE  WRK-Z9               TO   HC17B-RSYUEKIKEN
                                             HC17BV02-RSYUEKIKEN
      *
      *    ½é¿Ç
             IF WRK-NYUGAIKBN   =   "2"
               MOVE    SUM-SASYUEKIKEN-GK  TO   WRK-Z9
               MOVE    WRK-Z9              TO   HC17B-SSYUEKIKEN
                                                HC17BV02-SSYUEKIKEN
               MOVE    SUM-SSYUEKIKEN-GK   TO   WRK-Z9
               MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
               MOVE    1                   TO   WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K   TO  HC17B-SASYUEKIKEN(2:8)
                                       HC17BV02-SASYUEKIKEN(2:8)
               MOVE    WRK-TMP-K   TO  CSV-SASYUEKIKEN
               MOVE    "("         TO  HC17B-SASYUEKIKEN(WRK-COUNTS:1)
                                   HC17BV02-SASYUEKIKEN(WRK-COUNTS:1)
               MOVE    ")"         TO  HC17B-SASYUEKIKEN(10:1)
                                   HC17BV02-SASYUEKIKEN(10:1)
      *
      *    ¤½¤ÎÂ¾
               MOVE  SUM-OSYUEKIKEN-GK    TO   WRK-Z9
               MOVE  WRK-Z9               TO   HC17B-OSYUEKIKEN
                                               HC17BV02-OSYUEKIKEN
      *
               MOVE  SUM-JYUSHINKEN-GK TO   WRK-Z9
               MOVE  WRK-Z9            TO   HC17B-JYUSHINCNT
                                            HC17BV02-JYUSHINCNT
      *
             END-IF
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *
           IF   WRK-PARA-PRTYM  >=  "201502"
             MOVE   "A00000M501NV02.red"          TO   SPRT-PRTID
             MOVE    "·î·×É½¹ç·×¡ÊÁë¸ýÎÎ¼ý¶â¡Ë"   TO   SPRT-TITLE
             MOVE    HC17BV02                     TO   SPRT-PRTDATA
           ELSE
             IF  WRK-PARA-PRTYM  <  "200804"
               MOVE   "A00000M501O.red"           TO   SPRT-PRTID
             ELSE
               MOVE   "A00000M501N.red"           TO   SPRT-PRTID
             END-IF
             MOVE    "·î·×É½¹ç·×¡ÊÁë¸ýÎÎ¼ý¶â¡Ë"   TO   SPRT-TITLE
             MOVE    HC17B                        TO   SPRT-PRTDATA
           END-IF
      *
           MOVE    LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF    SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE    "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"   TO  WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      *    CSV¥Ø¥Ã¥ÀÊÔ½¸½èÍý
           PERFORM  800-CSV-HEAD-SEC
      *
      *    CSVÊÔ½¸
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-TERM-GK-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV¥Ø¥Ã¥ÀÊÔ½¸½èÍý
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           IF   WRK-PARA-PRTYM  >=  "201502"
             PERFORM  800-CSV-HEAD-V02-SEC
           ELSE
             PERFORM  800-CSV-HEAD-V01-SEC
           END-IF
      *
           .
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV¥Ø¥Ã¥ÀÊÔ½¸½èÍý(201502°ÊÁ°)
      *****************************************************************
       800-CSV-HEAD-V01-SEC            SECTION.
      *
           MOVE    1                TO  IDXP
      *    ÂÐ¾ÝÇ¯·î
           INITIALIZE                WRK-ITEM
           STRING    "ÂÐ¾ÝÇ¯·î¡§"    DELIMITED  BY  SIZE
                     HC17B-SRYYM     DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Æþ³°¶èÊ¬¥»¥Ã¥È
           IF  WRK-NYUGAIKBN  =  1
             MOVE    "Æþ³°¶èÊ¬¡§Æþ±¡"           TO  WRK-ITEM
           ELSE
             MOVE    "Æþ³°¶èÊ¬¡§³°Íè"           TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ¿ÇÎÅ²Ê¥»¥Ã¥È
           STRING    "¿ÇÎÅ²Ê¡§"      DELIMITED  BY  SIZE
                     HC17B-SRYKA     DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ¥·¥¹¥Æ¥àÆüÉÕ¥»¥Ã¥È
           MOVE    WRK-SYSYMDWH     TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    ÊÝ¸±Ì¾¾Î¥»¥Ã¥È
           MOVE    "¼ÒÊÝËÜ¿Í"       TO  CSV-HEAD-HKN1
           MOVE    "¼ÒÊÝ²ÈÂ²"       TO  CSV-HEAD-HKN2
           MOVE    "¹ñÊÝ"           TO  CSV-HEAD-HKN3
           MOVE    "¸å´ü¹âÎð"       TO  CSV-HEAD-HKN4
           MOVE    SPACE            TO  CSV-HEAD-HKN5
           MOVE    "¸øÈñÃ±ÆÈ"       TO  CSV-HEAD-HKN6
      *
      *    ¼«Èñ¿ÇÎÅ¹Ô°ÙÌ¾¾Î¥»¥Ã¥È
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  10
             IF  WRK-NYUGAIKBN  =  1
               MOVE    WRK-TJIHIN(IDX4)
                                    TO  CSV-HEAD-JIHI-OCC(IDX4)
             ELSE
               MOVE    WRK-TJIHI(IDX4)
                                    TO  CSV-HEAD-JIHI-OCC(IDX4)
             END-IF
           END-PERFORM
      *
           MOVE     SPACE           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    ¹àÌÜÌ¾¾Î¥»¥Ã¥È
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-HEAD-MAX
             IF  IDX4  NOT =  19
               IF  IDX4  NOT =  7
                 MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
                 IF   IDX4  >=  CSV-HEAD-MAX
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-IF
           END-PERFORM
           .
      *
       800-CSV-HEAD-V01-EXT.
           EXIT.
      *****************************************************************
      *    CSV¥Ø¥Ã¥ÀÊÔ½¸½èÍý(201502°Ê¹ß)
      *****************************************************************
       800-CSV-HEAD-V02-SEC            SECTION.
      *
           MOVE    1                TO  IDXP
      *    ÂÐ¾ÝÇ¯·î
           INITIALIZE                WRK-ITEM
           STRING    "ÂÐ¾ÝÇ¯·î¡§"    DELIMITED  BY  SIZE
                     HC17BV02-SRYYM  DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Æþ³°¶èÊ¬¥»¥Ã¥È
           IF  WRK-NYUGAIKBN  =  1
             MOVE    "Æþ³°¶èÊ¬¡§Æþ±¡"           TO  WRK-ITEM
           ELSE
             MOVE    "Æþ³°¶èÊ¬¡§³°Íè"           TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ¿ÇÎÅ²Ê¥»¥Ã¥È
           STRING    "¿ÇÎÅ²Ê¡§"      DELIMITED  BY  SIZE
                     HC17BV02-SRYKA  DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ¥·¥¹¥Æ¥àÆüÉÕ¥»¥Ã¥È
           MOVE    WRK-SYSYMDWH     TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    ÊÝ¸±Ì¾¾Î¥»¥Ã¥È
           MOVE    "¼ÒÊÝËÜ¿Í"       TO  CSV-HEAD-HKN1
           MOVE    "¼ÒÊÝ²ÈÂ²"       TO  CSV-HEAD-HKN2
           MOVE    "¹ñÊÝ"           TO  CSV-HEAD-HKN3
           MOVE    "¸å´ü¹âÎð"       TO  CSV-HEAD-HKN4
           MOVE    SPACE            TO  CSV-HEAD-HKN5
           MOVE    "¸øÈñÃ±ÆÈ"       TO  CSV-HEAD-HKN6
      *
      *    ¼«Èñ¿ÇÎÅ¹Ô°ÙÌ¾¾Î¥»¥Ã¥È
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  10
             IF  WRK-NYUGAIKBN  =  1
               MOVE    WRK-TJIHIN(IDX4)
                                    TO  CSV-HEAD-JIHI-OCC(IDX4)
             ELSE
               MOVE    WRK-TJIHI(IDX4)
                                    TO  CSV-HEAD-JIHI-OCC(IDX4)
             END-IF
           END-PERFORM
      *
           MOVE     SPACE           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    ¹àÌÜÌ¾¾Î¥»¥Ã¥È
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-HEAD-MAX
             IF  IDX4  NOT =  7
               MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
               IF   IDX4  >=  CSV-HEAD-MAX
                 PERFORM  810-CSVDATA-EDIT-SEC
               ELSE
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
             END-IF
           END-PERFORM
           .
      *
       800-CSV-HEAD-V02-EXT.
           EXIT.
      *****************************************************************
      *    CSVÊÔ½¸½èÍý
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           IF   WRK-PARA-PRTYM  >=  "201502"
             PERFORM  400-CSV-DATA-V02-SEC
           ELSE
             PERFORM  400-CSV-DATA-V01-SEC
           END-IF
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVÊÔ½¸½èÍý(201502°ÊÁ°)
      *****************************************************************
       400-CSV-DATA-V01-SEC            SECTION.
      *
      *    Áí¹ç·×
           MOVE    "Áí¹ç·×"               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí¹ç·×¡²ÊÝ¸±ÅÀ¿ô
           MOVE     HC17B-GHKNTEN         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí¹ç·×¡²°åÎÅ¼ý±×
           MOVE    HC17B-GSYUEKI          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí¹ç·×¡²Áë¸ýÀÁµá¶â³Û
           PERFORM  VARYING  IDX9  FROM  1  BY  1
                    UNTIL  IDX9  >  20
             IF  IDX9  NOT =  5
               MOVE    HC17B-GMONEY(IDX9)   TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²Áë¸ýÀÁµá¶â³Û
           MOVE    HC17B-GSEIKYU          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²Áë¸ýÎÎ¼ý¶â³Û
           MOVE    HC17B-GRYOSYU          TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           IF  WRK-NYUGAIKBN  =  1
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô
             MOVE    "Áí·ï¿ô¡²ÀÁµá·ï¿ô"     TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17B-RHKNKEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²°åÎÅ¼ý±×
             MOVE    HC17B-RSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17B-RKENSU(IDX9)   TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²½é¿Ç·ï¿ô
           ELSE
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾·ï¿ô
             MOVE    "Áí·ï¿ô¡²¤½¤ÎÂ¾·ï¿ô"   TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17B-OHKNTEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾¡²°åÎÅ¼ý±×
             MOVE    HC17B-OSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17B-OKENSU(IDX9)     TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç·ï¿ô
             MOVE    "Áí·ï¿ô¡²ºÆ¿Ç·ï¿ô"     TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17B-RHKNKEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç¡²°åÎÅ¼ý±×
             MOVE    HC17B-RSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17B-RKENSU(IDX9)   TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²½é¿Ç·ï¿ô
             MOVE    "Áí·ï¿ô¡²½é¿Ç·ï¿ô"     TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²½é¿Ç¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17B-SHKNKEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²½é¿Ç¡²°åÎÅ¼ý±×
             MOVE    HC17B-SSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²½é¿Ç¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17B-SKENSU(IDX9)   TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô
             MOVE    "Áí·ï¿ô¡²¿·´µ¿ô"       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô¡²ÊÝ¸±ÅÀ¿ô
             MOVE    CSV-SAHKNKEN         TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô¡²°åÎÅ¼ý±×
             MOVE    CSV-SASYUEKIKEN      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    CSV-SAKENSU(IDX9)    TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-PERFORM
      *
      *    ¼õ¿Ç¼Ô¿ô
             MOVE    "¼õ¿Ç¼Ô¿ô"         TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
             MOVE     HC17B-JYUSHINCNT  TO  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
      *
           END-IF
      *
      *    £Ã£Ó£Ö½ÐÎÏ½èÍý
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       400-CSV-DATA-V01-EXT.
           EXIT.
      *****************************************************************
      *    CSVÊÔ½¸½èÍý(201502°Ê¹ß)
      *****************************************************************
       400-CSV-DATA-V02-SEC            SECTION.
      *
      *    Áí¹ç·×
           MOVE    "Áí¹ç·×"               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí¹ç·×¡²ÊÝ¸±ÅÀ¿ô
           MOVE     HC17BV02-GHKNTEN         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí¹ç·×¡²°åÎÅ¼ý±×
           MOVE    HC17BV02-GSYUEKI          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí¹ç·×¡²Áë¸ýÀÁµá¶â³Û
           PERFORM  VARYING  IDX9  FROM  1  BY  1
                    UNTIL  IDX9  >  20
             IF  IDX9  NOT =  5
               MOVE    HC17BV02-GMONEY(IDX9)   TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
             IF  IDX9  =  16
               MOVE    HC17BV02-JIHI-GMONEY    TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²Áë¸ýÀÁµá¶â³Û
           MOVE    HC17BV02-GSEIKYU          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²Áë¸ýÎÎ¼ý¶â³Û
           MOVE    HC17BV02-GRYOSYU          TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           IF  WRK-NYUGAIKBN  =  1
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô
             MOVE    "Áí·ï¿ô¡²ÀÁµá·ï¿ô"     TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17BV02-RHKNKEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²°åÎÅ¼ý±×
             MOVE    HC17BV02-RSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ÀÁµá·ï¿ô¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17BV02-RKENSU(IDX9)   TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
               IF  IDX9  =  16
                 MOVE    HC17BV02-JIHI-RKENSU  TO  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²½é¿Ç·ï¿ô
           ELSE
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾·ï¿ô
             MOVE    "Áí·ï¿ô¡²¤½¤ÎÂ¾·ï¿ô"   TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17BV02-OHKNTEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾¡²°åÎÅ¼ý±×
             MOVE    HC17BV02-OSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¤½¤ÎÂ¾¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17BV02-OKENSU(IDX9)     TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
               IF  IDX9  =  16
                 MOVE    HC17BV02-JIHI-OKENSU  TO  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç·ï¿ô
             MOVE    "Áí·ï¿ô¡²ºÆ¿Ç·ï¿ô"     TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17BV02-RHKNKEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç¡²°åÎÅ¼ý±×
             MOVE    HC17BV02-RSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²ºÆ¿Ç¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17BV02-RKENSU(IDX9)   TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
               IF  IDX9  =  16
                 MOVE    HC17BV02-JIHI-RKENSU  TO  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²½é¿Ç·ï¿ô
             MOVE    "Áí·ï¿ô¡²½é¿Ç·ï¿ô"     TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²½é¿Ç¡²ÊÝ¸±ÅÀ¿ô
             MOVE    HC17BV02-SHKNKEN          TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²½é¿Ç¡²°åÎÅ¼ý±×
             MOVE    HC17BV02-SSYUEKIKEN       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²½é¿Ç¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    HC17BV02-SKENSU(IDX9)   TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
               IF  IDX9  =  16
                 MOVE    HC17BV02-JIHI-SKENSU  TO  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
             END-PERFORM
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô
             MOVE    "Áí·ï¿ô¡²¿·´µ¿ô"       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô¡²ÊÝ¸±ÅÀ¿ô
             MOVE    CSV-SAHKNKEN         TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô¡²°åÎÅ¼ý±×
             MOVE    CSV-SASYUEKIKEN      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Áí·ï¿ô¡²¿·´µ¿ô¡²Áë¸ýÀÁµá¶â³Û¡ÊÊÝ¸±ÊÌÌÀºÙ¡Ë
             PERFORM  VARYING  IDX9  FROM  1  BY  1
                      UNTIL  IDX9  >  20
               IF  IDX9  NOT =  5
                 MOVE    CSV-SAKENSU(IDX9)    TO  WRK-ITEM
                 IF  IDX9  =  20
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
               IF  IDX9  =  16
                 MOVE    CSV-JIHI-SAKENSU     TO  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
             END-PERFORM
      *
      *    ¼õ¿Ç¼Ô¿ô
             MOVE    "¼õ¿Ç¼Ô¿ô"         TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
             MOVE     HC17BV02-JYUSHINCNT  TO  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
      *
           END-IF
      *
      *    £Ã£Ó£Ö½ÐÎÏ½èÍý
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       400-CSV-DATA-V02-EXT.
           EXIT.
      *****************************************************************
      *    CSV¥Ç¡¼¥¿ÊÔ½¸½èÍý
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSV¥Ç¡¼¥¿ÊÔ½¸½èÍý
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSV¥Ç¡¼¥¿ÊÔ½¸½èÍý
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¶èÀÚ¤êÊ¸»úÊÔ½¸½èÍý
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","   DELIMITED   BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    ²þ¹ÔÊ¸»úÊÔ½¸½èÍý
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"   DELIMITED BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    ¹àÌÜ¥µ¥¤¥º¼èÆÀ½èÍý
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX1:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX1:1)  NOT =  SPACE
                 MOVE    IDX1      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    £Ã£Ó£Ö½ÐÎÏ½èÍý
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           IF  WRK-PARA-PRTYM  >=  "200804"
             INITIALIZE                     ORCSTOUKEICSVAREA
             MOVE    "INS"                  TO  STOUKEICSV-MODE
             MOVE    LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
             MOVE    LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
             MOVE    LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
             MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
             MOVE    LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
             MOVE    LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
             MOVE    ZERO                   TO  STOUKEICSV-PTID
             MOVE    "/var/tmp/"            TO  STOUKEICSV-TO-FOLDER
      *
             STRING  SPA-HOSPNUM            DELIMITED  BY  SIZE
                     "gekkei_madoguchi_"    DELIMITED  BY  SIZE
                     WRK-PARA-PRTYM         DELIMITED  BY  SIZE
                     ".csv"                 DELIMITED  BY  SIZE
                                            INTO  STOUKEICSV-TO-DATA
             END-STRING
             MOVE    "2"                    TO  STOUKEICSV-CODE-TYPE
             MOVE    WRK-CSVDATA            TO  STOUKEICSV-CSVDATA
             MOVE    "·î·×É½¹ç·×¡ÊÁë¸ýÎÎ¼ý¶â¡Ë"    TO  STOUKEICSV-TITLE
      *
             CALL    "ORCSTOUKEICSV"     USING
                                         ORCSTOUKEICSVAREA
                                         SPA-AREA
             IF  ( STOUKEICSV-RETURN  =  ZERO )
               CONTINUE
             ELSE
               MOVE    "Åý·×£Ã£Ó£Ö£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                         TO  WRK-RECEERR
               PERFORM  500-ERR-HENSYU-SEC
             END-IF
      *
             INITIALIZE                  WRK-CSVDATA
           END-IF
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF    STS-RECEERR    =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT              RECEERR-FILE
      *
             MOVE    WRK-RECEERR     TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
             MOVE    "JBE"           TO   SJOBKANRI-MODE
             INITIALIZE                   JOBKANRI-REC
             MOVE    SPA-HOSPNUM     TO   JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID  TO   JOB-JOBID
             MOVE    WRK-PARA-SHELLID
                                     TO   JOB-SHELLID
             MOVE    WRK-RECEERR     TO   JOB-YOBI
             MOVE    "9999"          TO   JOB-ERRCD
             CALL    "ORCSJOB"       USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                     SPA-AREA
           END-IF
      *
           MOVE    1                 TO   FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"                TO   SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO   JOB-SHELLID
           MOVE    CNT-PAGE             TO   JOB-UPDCNT
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    À¾ÎñÆüËÜ¸ìÊÑ´¹½èÍý
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "¡¡"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ·îËöÆü»»½Ð½èÍý
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE               STS-AREA-DAY
           INITIALIZE               LNK-DAY7-AREA
           MOVE    "71"             TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD         TO   LNK-DAY7-YM
           CALL    "ORCSDAY"        USING
                                    STS-AREA-DAY
                                    LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI  TO   WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÌÀºÙ¾ðÊó
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM   TO   SMEI-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO   SMEI-NYUHEN-YMD
           MOVE    WRK-PARA-PRTLYMD   TO   SMEI-UPNYUHEN-YMD
      *
           MOVE    SYUMEI-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE   "tbl_syumei"        TO  MCP-TABLE
           MOVE   "st_key3"           TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF    MCP-RC   =   ZERO
             PERFORM 910-DBFETCH-SEC
           ELSE
             MOVE    1           TO  FLG-SYUMEI
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     ¼ýÇ¼ÌÀºÙ¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
           MOVE   "tbl_syumei"      TO  MCP-TABLE
           MOVE   "st_key3"         TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    ZERO           TO   FLG-SYUMEI
             MOVE    MCPDATA-REC    TO   SYUMEI-REC
           ELSE
             MOVE    1              TO   FLG-SYUMEI
           END-IF
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE   "tbl_syumei"      TO  MCP-TABLE
           MOVE   "st_key3"         TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¼õ¿ÇÍúÎòSELECT
      ******************************************************************
       900-JYURRK-SELECT-SEC                 SECTION.
      *
           MOVE    SYU-HOSPNUM            TO   JYURRK-HOSPNUM
           MOVE    SYU-PTID               TO   JYURRK-PTID
           MOVE    SYU-NYUGAIKBN          TO   JYURRK-NYUGAIKBN
           MOVE    "00000000"             TO   JYURRK-SRYYMD
           MOVE    SYU-SRYYMD             TO   JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"             TO   MCP-FUNC
           MOVE    "tbl_jyurrk"           TO   MCP-TABLE
           MOVE    "key14"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  900-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                    TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¼õ¿ÇÍúÎòFETCH
      ******************************************************************
       900-JYURRK-FETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC       TO   JYURRK-REC
             MOVE    ZERO              TO   FLG-JYURRK
           ELSE
             MOVE    1                 TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-FETCH-EXT.
           EXIT.

      ******************************************************************
      *    ¼õ¿ÇÍúÎòCLOSE
      ******************************************************************
       900-JYURRK-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key14"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE               SYUNOU-REC
           MOVE    SMEI-HOSPNUM     TO    SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN   TO   SYU-NYUGAIKBN
           MOVE    SMEI-PTID        TO   SYU-PTID
           MOVE    SMEI-DENPNUM     TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"       TO   MCP-FUNC
           MOVE   "tbl_syunou"      TO  MCP-TABLE
           MOVE   "key"             TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    "DBFETCH"      TO   MCP-FUNC
             MOVE   "tbl_syunou"    TO   MCP-TABLE
             MOVE   "key"           TO   MCP-PATHNAME
             CALL   "ORCDBMAIN"     USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC    TO   SYUNOU-REC
               MOVE    ZERO           TO   FLG-SYUNOU
             ELSE
               MOVE    1              TO   FLG-SYUNOU
             END-IF
           ELSE
               MOVE    1              TO   FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE   "tbl_syunou"        TO  MCP-TABLE
           MOVE   "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷
      *****************************************************************
       901-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)     TO   SYU-HOSPNUM
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)        TO   SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)     TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC                  TO   MCPDATA-REC
           MOVE    "DBSELECT"                  TO   MCP-FUNC
           MOVE    "tbl_syunou"                TO   MCP-TABLE
           MOVE    "key"                       TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF  MCP-RC    =   ZERO
             MOVE    "DBFETCH"              TO   MCP-FUNC
             MOVE   "tbl_syunou"            TO   MCP-TABLE
             MOVE   "key"                   TO   MCP-PATHNAME
             CALL   "ORCDBMAIN"             USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
             IF   MCP-RC    =   ZERO
               MOVE    MCPDATA-REC          TO   SYUNOU-REC
               MOVE    ZERO                 TO   FLG-SYUNOU
             ELSE
               MOVE    1                    TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                      TO   FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"          TO   MCP-FUNC
           MOVE   "tbl_syunou"              TO   MCP-TABLE
           MOVE   "key"                     TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           .
       901-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¾ðÊó¸¡º÷
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SYU-HOSPNUM         TO   HKN-HOSPNUM
           MOVE    SYU-SYUHKNNUM       TO   HKN-HKNNUM
           MOVE    WRK-PARA-PRTSYMD    TO   HKN-TEKSTYMD
           MOVE    ALL "9"             TO   HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE   "tbl_hknnum"       TO  MCP-TABLE
             MOVE   "key5"             TO  MCP-PATHNAME
             CALL   "ORCDBMAIN"        USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF      MCP-RC          =  ZERO
               MOVE    MCPDATA-REC     TO   HKNNUM-REC
               MOVE    ZERO            TO   FLG-HKNNUM
             ELSE
               MOVE    1               TO   FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1                 TO   FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_hknnum"        TO   MCP-TABLE
           MOVE    "key5"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_syskanri"    TO   MCP-TABLE
             MOVE    "key10"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF    MCP-RC   =   ZERO
               MOVE    ZERO            TO   FLG-SYSKANRI
             ELSE
               MOVE    1               TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       820-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       820-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_ptinf"       TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO   PTINF-REC
               MOVE    ZERO            TO   FLG-PTINF
             ELSE
               MOVE    1               TO   FLG-PTINF
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-PTINF
             INITIALIZE                PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
            MOVE    "tbl_pthkninf"     TO   MCP-TABLE
            MOVE    "key"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_pthkninf"    TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   PTHKNINF-REC
               MOVE    ZERO            TO   FLG-PTHKNINF
             ELSE
               MOVE    1               TO   FLG-PTHKNINF
               INITIALIZE              PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-PTHKNINF
             INITIALIZE                PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_pthkninf"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_hkncombi"     TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF    MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   HKNCOMBI-REC
               MOVE    ZERO             TO   FLG-HKNCOMBI
             ELSE
               MOVE    1                TO   FLG-HKNCOMBI
               INITIALIZE               HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNCOMBI
             INITIALIZE                 HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¾ðÊóÆÉ¹þ½èÍý
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_sryacct"        TO   MCP-TABLE
           MOVE    "key3"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_sryacct"      TO   MCP-TABLE
             MOVE    "key3"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   SRYACCT-REC
               MOVE    ZERO             TO   FLG-SRYACCT
             ELSE
               MOVE    1                TO   FLG-SRYACCT
               INITIALIZE               SRYACCT-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SRYACCT
             INITIALIZE                 SRYACCT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_sryacct"        TO   MCP-TABLE
           MOVE    "key3"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       900-JYURRK2-READ-SEC                SECTION.
      *
           INITIALIZE                  JYURRK2-REC
      *
           MOVE    JYURRK-HOSPNUM      TO   JYURRK2-HOSPNUM
           MOVE    JYURRK-PTID         TO   JYURRK2-PTID
           MOVE    JYURRK-NYUGAIKBN    TO   JYURRK2-NYUGAIKBN
           MOVE    "00000000"          TO   JYURRK2-SRYYMD
           MOVE    "99999999"          TO   JYURRK2-UPSRYYMD
      *
           MOVE    JYURRK2-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_jyurrk"      TO   MCP-TABLE
             MOVE    "key14"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   JYURRK2-REC
               MOVE    ZERO            TO   FLG-JYURRK2
             ELSE
               MOVE    1               TO   FLG-JYURRK2
             END-IF
           ELSE
             MOVE    1                 TO   FLG-JYURRK2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-JYURRK2-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       900-SRYKARRK-SELECT-SEC            SECTION.
      *
           INITIALIZE               SRYKARRK-REC
      *
           MOVE    SYU-HOSPNUM      TO   KARRK-HOSPNUM
           MOVE    SYU-PTID         TO   KARRK-PTID
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBSELECT"       TO   MCP-FUNC
           MOVE    "tbl_srykarrk"  TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM  900-SRYKARRK-FETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       900-SRYKARRK-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"         TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key2"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO   SRYKARRK-REC
             MOVE    ZERO            TO   FLG-SRYKARRK
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       900-SRYKARRK-CLOSE-SEC            SECTION.
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBCLOSECURSOR"  TO   MCP-FUNC
           MOVE    "tbl_srykarrk"  TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       900-SRYKARRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÆÉ¤ß½Ð¤·
           INITIALIZE                    HKNCOMBI-REC
           MOVE    SYU-HOSPNUM           TO  COMB-HOSPNUM
           MOVE    SYU-PTID              TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM       TO  COMB-HKNCOMBINUM
           PERFORM  900-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI  =  ZERO
      *    ´µ¼ÔÊÝ¸±¾ðÊóÆÉ¤ß¹þ¤ß
             INITIALIZE                  PTRSIINF-REC
             MOVE    SYU-HOSPNUM         TO  PTRSI-HOSPNUM
             MOVE    SYU-PTID            TO  PTRSI-PTID
             MOVE    COMB-HKNID          TO  PTRSI-HKNID
             MOVE    PTRSIINF-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    "DBFETCH"         TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF   MCP-RC  =  ZERO
                 MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               ELSE
                 INITIALIZE              PTRSIINF-REC
               END-IF
             END-IF
      *
             MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           END-IF
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
