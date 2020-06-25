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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSODRNACCT.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÅÅ»Ò¥«¥ë¥ÆÆþ±¡²ñ·×ºîÀ®
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  02.07.01    NACL-¾®Æ¦Âô  05.12.06  Æþ±¡ÅÐÏ¿»þ¤Î¿©»ö¤È¿©Æ²²Ã»»¤Î
      *                                     ¥«¥ì¥ó¥À¡¼¥¯¥ê¥¢ÂÐ±þ¤È¡¢Ä«¿©Í¥Àè
      *                                     ¤Î¾ò·ïÄÉ²Ã
      *  02.07.02    NACL-¾®Æ¦Âô  05.12.12  Å¾²Ê¡¢Å¾Åï¡¢Å¾¼¼ÂÐ±þ
      *  02.07.03    NACL-¾®Æ¦Âô  05.12.13  Æþ±¡¼è¾ÃÂÐ±þ
      *  03.05.00    NACL-ÂÀÅÄ    07/05/28  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.03.00    NACL-¾®Æ¦Âô  08/08/11  ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¤ò¥µ¥Ö¥×¥í¤ËÊÑ¹¹
      *  05.00.00    NACL-¾®Æ¦Âô  17/10/27  »ØÄê·îÊ¬¤ÎÆþ±¡²ñ·×ºîÀ®½èÍýÄÉ²Ã
      *  05.00.00    NACL-¾®Æ¦Âô  17/12/14  ¼¼ÎÁº¹³Û¤Î½èÍýÄÉ²Ã
      *Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/03/05  ÎÅÍÜÉÂÅï¤ÎÆÉ¤ßÂØ¤¨¥³¡¼¥ÉÄÉ²Ã
      *  05.00.00    NACL-¾®Æ¦Âô  18/03/13  Â¾°å¼õ¿Ç¤ÎÄÉ²Ã
      *  05.00.00    NACL-¾®Æ¦Âô  18/03/20  ºßÂð´µ¼Ô»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
      *                                     ºßÂð´µ¼Ô»Ù±çÉÂ¾²½é´ü²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë
      *  05.00.00    NACL-¾®Æ¦Âô  18/04/27  ¿©»ö·ÑÂ³¶èÊ¬ÂÐ±þ
     *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ÆþÂà±¡ÅÐÏ¿¶¦ÄÌÎÎ°è
           COPY    "I0COMMON-SPA".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-ORCSDAY                 PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-PTCONF                  PIC 9(01).
           03  FLG-IDOKAIKEI-CANCEL        PIC 9(01).
           03  FLG-NYUINACCT-CHK           PIC 9(01).
           03  FLG-NYUINACCT-MAE-CHK       PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX10                       PIC 9(05).
           03  IDX11                       PIC 9(05).
           03  IDX12                       PIC 9(05).
           03  IDX13                       PIC 9(05).
           03  IDX14                       PIC 9(05).
           03  IDX15                       PIC 9(05).
           03  IDX16                       PIC 9(05).
           03  START-IDX                   PIC 9(05).
           03  END-IDX                     PIC 9(05).
           03  IDX-SKJ-TAIHI               PIC 9(02).
           03  IDX-KSN                     PIC 9(05).
      *
      *    ¥·¥¹¥Æ¥àÎÎ°è
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-ERRCD                   PIC X(04).
           03  WRK-NAI-I01-IDOYMD          PIC X(08).
           03  WRK-NAI-I01-IDOYMD-MAE      PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
      *
           03  WRK-AGE-YY                  PIC S9(04).
           03  WRK-AGE-MM                  PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ---9.
               05  WRK-AGE-MEI             PIC X(04).
           03  WRK-BIRTHDAY                PIC X(08).
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-NUM          PIC ZZZZ9.
               05  WRK-SAGAKU-EN           PIC X(02).
           03  WRK-HKNCOMBI                PIC X(100).
           03  WRK-BRMNUM                  PIC X(06).
           03  WRK-BRMNUM-IN               PIC X(06).
           03  WRK-BRMNUM-EDT              PIC X(06).
           03  WRK-BRMNUM-OT               PIC X(06).
           03  WRK-NYUINKA-KEY             PIC X(02).
           03  WRK-BTUNUM-KEY              PIC X(02).
           03  WRK-BRMNUM-KEY              PIC X(08).
           03  WRK-HKNCOMBI-KEY            PIC X(04).
           03  WRK-TOKUTEI-KEY.
               05  WRK-TOKUTEI-KBN-KEY     PIC X(01).
               05  WRK-TOKUTEI-NYUIN-KEY   PIC X(02).
      *
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
           03  WRK-LASTSRYYM                   PIC X(06).
           03  WRK-NISSU                       PIC 9(05).
           03  WRK-NISSU1                      PIC 9(05).
           03  WRK-NISSU2                      PIC 9(05).
           03  WRK-TSUSAN                      PIC 9(05).
           03  WRK-STUSAN-NISSU03              PIC 9(05).
           03  WRK-NYUINNISSU                  PIC 9(05).
           03  WRK-SANTEISTYMD                 PIC X(08).
           03  WRK-SENTEI-STYMD                PIC X(08).
           03  WRK-TOKUTEI-NYUIN               PIC X(02).
           03  WRK-BIRTHDAY-15                 PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y            PIC 9(04).
               05  WRK-BIRTHDAY-15MD           PIC 9(04).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-SAGAKU-KBN              PIC X(02).
           03  WRK-SAGAKU-KBN9  REDEFINES  WRK-SAGAKU-KBN
                                           PIC 9(02).
           03  WRK-TAIHI-SRYKA             PIC X(02).
           03  WRK-TENNYUYMD               PIC X(08).
           03  WRK-TENNYUYMD9   REDEFINES  WRK-TENNYUYMD.
               05  WRK-TENNYUYMD-YY9       PIC 9(04).
               05  WRK-TENNYUYMD-MM9       PIC 9(02).
               05  WRK-TENNYUYMD-DD9       PIC 9(02).
      *
      *    °åÎÅµ¡´ØÀßÄê¤ÎÆþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯ÍÑ
           03  WRK-NYUKHNKSN-SRYCD         PIC X(09)  OCCURS 20.
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-BTU-NYUKSNKBN           PIC X(03).
           03  WRK-BTU-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-BTU-CDKBN-KBNNUM-EDA    PIC 9(02).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-KSN-CDKBN-KBNNUM-EDA    PIC 9(02).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-TAIINYMD.
               05  WRK-TAIINYM             PIC X(06).
               05  WRK-TAIINDD             PIC 9(02).
           03  WRK-MCP-PUTTYPE             PIC X(08).
           03  WRK-ORCHCN01-SYORIKBN       PIC X(01).
           03  WRK-ORCHCN01-PRTKBN         PIC X(01).
           03  WRK-AGECHK-SRYYMD           PIC X(08).
           03  WRK-NACCT-SRYYM             PIC X(08).
           03  WRK-PRINT-PG                PIC X(20).
           03  WRK-ORCSHKN-HKNCOMBINUM     PIC X(04).
           03  WRK-TOKUBETSU-NYUIN         PIC X(01).
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»Î²Ã»»²Ã»»ÂàÈòÎÎ°è
           03  WRK-BTU-HIKSISUKBN          PIC X(01).
           03  WRK-BTU-KNGTEK-KBN          PIC X(01).
           03  WRK-BTU-DRTEK-KBN           PIC X(01).
      *    ²ñ·×ºîÀ®ÍÑÂàÈòÎÎ°è
           03  WRK-TAIHI-HKN               PIC 9(03).
           03  WRK-TAIHI-SHOKUJI           PIC 9(03).
           03  IDX-SHOKUJIX                PIC X(02).
           03  IDX-SHOKUJI9   REDEFINES    IDX-SHOKUJIX
                                           PIC 9(02).
           03  IDX-SKJ                     PIC 9(02).
           03  WRK-NYUINCHU-TAIINYMD       PIC X(08).
           03  WRK-ORCHCN01-HKNCOMBINUM    PIC 9(04).
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-MIN-YMD.
               05  WRK-MIN-YM              PIC X(06).
               05  WRK-MIN-DD              PIC 9(02).
      *    ÆÃÄêÆþ±¡ÎÁ¤Î²ñ·×ºîÀ®¸å¤Ë°ìÈÌ¤ÎÆþ±¡ÎÁ¤Ç²ñ·×¤òºîÀ®¤¹¤ë¤¿¤á¤Î
      *    ÎÎ°è
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *    ÆÃÄêÆþ±¡ÎÁ¤Î²ñ·×ºîÀ®ÍÑ¤Ë¿ÇÎÅ¥³¡¼¥É¤òÂàÈò¤¹¤ëÎÎ°è
           03  WRK-TOKUTEI-SRYCD1          PIC X(09).
           03  WRK-TOKUTEI-SRYCD2          PIC X(09).
           03  WRK-TOKUTEI-SRYCD3          PIC X(09).
           03  WRK-TOKUTEI-SRYCD4          PIC X(09).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-SHOKAI-NYUINYMD-W       PIC X(10).
           03  WRK-CHK-SHONUM              PIC 9(03).
           03  WRK-I01-I04-PROCKBN         PIC X(01).
           03  WRK-FTN-NYUINYMD            PIC X(08).
           03  WRK-FTN-TAIINYMD            PIC X(08).
           03  WRK-FTN-RRKNUM              PIC 9(03).
           03  WRK-NYUREKILST-SEL          PIC 9(05).
      *
      *    Å¾²Ê¡¿Å¾Åï¡¿Å¾¼¼»þ¤Ë¿©»ö²ñ·×¤òÂàÈò¤¹¤ë¹àÌÜ¡ÊºÇÂç£²¥«·îÊ¬¡Ë
           03  WRK-TENROOM-SKJ-SRYYMX.
               05  WRK-TENROOM-SKJ-SRYYY9  PIC 9(04).
               05  WRK-TENROOM-SKJ-SRYMM9  PIC 9(02).
           03  WRK-TENROOM-SKJ-TABLE.
               05  WRK-TENROOM-SKJ-TBL     OCCURS  2.
                   07  WRK-TENROOM-SKJ-SRYYM
                                           PIC X(06).
                   07  WRK-TENROOM-SKJ-DD  PIC 9(02).
                   07  WRK-TENROOM-SKJ     PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ18   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ19   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ20   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-GAIHAKU PIC 9(03)  OCCURS  31.
           03  WRK-NAI-NYUINYMD.
               05  WRK-NAI-NYUINYM         PIC X(06).
               05  WRK-NAI-NYUINDD         PIC 9(02).
      *
           03  WRK-MAEIDOYMD               PIC X(08).
      *
      *    ½ÐÀ¸»ùÂÎ½Å
           03  WRK-WEIGHT                  PIC 9(04).
      *
      *    Á°·îÆþ±¡²ñ·×Â¸ºß¥Á¥§¥Ã¥¯ÍÑ
           03  WRK-CHK-SRYYM               PIC X(06).
           03  WRK-CHK-SRYYMR      REDEFINES  WRK-CHK-SRYYM.
               05  WRK-CHK-SRYYR           PIC 9(04).
               05  WRK-CHK-SRYMR           PIC 9(02).
      *
      *    ¼¼ÎÁº¹³Û
           03  WRK-ROOMSAGAKU              PIC 9(02).
      *
      *    ´µ¼ÔÇ¯Îð»»½ÐÍÑ¥¨¥ê¥¢
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                  PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1                PIC 9(04).
               05  WRK-MMS1                PIC 9(02).
               05  WRK-DDS1                PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2                PIC 9(04).
               05  WRK-MMS2                PIC 9(02).
               05  WRK-DDS2                PIC 9(02).
      *
      *
      *    Âà±¡Æü¤ÎÆ±ÆüºÆÆþ±¡ÍÑ¥ï¡¼¥¯ÎÎ°è
       01  WRK-DOU-AREA.
           03  WRK-DOU-NAI-I01-NYUINYMD      PIC X(08).
           03  WRK-DOU-PTNYUINRRK-TENNYUYMD  PIC X(08).
           03  WRK-DOU-TENNYUYMD             PIC X(08).
           03  FLG-JIINRRK                   PIC 9(01).
       01  WRK-PTNYU-REC.
           COPY  "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                            BY  //WRK-PTNYU-//.
      *
      *    ¿©Æ²²Ã»»ÀßÄêÍÑ¤ÎÂàÈòÎÎ°è
       01   WRK-SHOKUDOU                   PIC 9(02).
      *
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ¡ÊÅ¾²Ê¡¢Å¾Åï¡Ë·×»»¥¨¥ê¥¢
       01  TN-IDX                          PIC 9(02).
       01  TN-IDX2                         PIC 9(02).
       01  WK-TOKUTEI-DAY                  PIC 9(04).
       01  WK-TOKUTEI-DAY2                 PIC 9(04).
      *
      *    ³°Çñ¿©»ö¥ª¡¼¥À¡¼¤ÎÆüÉÕÂàÈò
       01  WRK-STARTYMD.
           03  WRK-STARTYMD-YM.
               05  WRK-STARTYMD-Y          PIC 9(04).
               05  WRK-STARTYMD-M          PIC 9(02).
           03  WRK-STARTYMD-D              PIC 9(02).
       01  WRK-ENDYMD.
           03  WRK-ENDYMD-YM.
               05  WRK-ENDYMD-Y            PIC 9(04).
               05  WRK-ENDYMD-M            PIC 9(02).
           03  WRK-ENDYMD-D                PIC 9(02).
       01  WRK-GETSUMATSU-YMD.
           03  WRK-GETSUMATSU-Y            PIC 9(04).
           03  WRK-GETSUMATSU-M            PIC 9(02).
           03  WRK-GETSUMATSU-D            PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-H260401               PIC X(08)   VALUE "20140401".
           03  CONST-H241001               PIC X(08)   VALUE "20121001".
           03  CONST-H240401               PIC X(08)   VALUE "20120401".
           03  CONST-H220401               PIC X(08)   VALUE "20100401".
           03  CONST-H211001               PIC X(08)   VALUE "20091001".
           03  CONST-H181001               PIC X(08)   VALUE "20061001".
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
           03  CONST-H180701               PIC X(08)   VALUE "20060701".
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
      *
       01  WRK-ROW.
           03  WRK-NYUREKILST-ROW          PIC S9(9)   BINARY.
      *
      *    ¥¹¥ÑÎÎ°è ¥ï¡¼¥¯
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    Æþ±¡ÍúÎò¥ï¡¼¥¯
       01  NRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //NRRK-//.
      *
      *    Æþ±¡ÍúÎòÂàÈòÍÑ¥ï¡¼¥¯
       01  WK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //WK-PTNYUINRRK-//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    ¡Ê°åÎÅµ¡´Ø´ðËÜ¾ðÊó¡Ë
           COPY    "CPSK1001.INC".
      *    ¡Ê¿ÇÎÅ²ÊÆþ±¡²Ê¾ðÊó¡Ë
           COPY    "CPSK1005.INC".
      *    ¡Ê¿¦°÷¾ðÊó¡Ë
           COPY    "CPSK1010.INC".
      *
      *    Ä¢É¼¥×¥í¥°¥é¥àÌ¾
           COPY    "CPSK1035.INC".
      *    ¡Ê°åÎÅµ¡´Ø¾ðÊó¡Ë
           COPY    "CPSK5000.INC".
      *    ¡ÊÉÂÅï¾ðÊó¡Ë
           COPY    "CPSK5001.INC".
      *    ¡ÊÉÂ¼¼¾ðÊó¡Ë
           COPY    "CPSK5002.INC".
      *    ¡Ê¼¼ÎÁº¹³Û¡Ë
           COPY    "CPSK5113.INC".
      *    ¡ÊÆÃÄêÆþ±¡ÎÁ¡ÝÉÂÅï¡Ë
           COPY    "CPSK5110.INC".
      *    ¡ÊÆÃÄêÆþ±¡ÎÁ¡ÝÉÂ¼¼¡Ë
           COPY    "CPSK5111.INC".
      *    
           COPY    "CPSK5013.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ´µ¼ÔÈÖ¹æ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ´µ¼ÔÂ¾±¡ÎòÉÕ²Ã¾ðÊó
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *    Ï·¿Í°ìÈÌ¥³¡¼¥ÉÊÑ´¹
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    ÁªÄêÎÅÍÜÈñ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ´µ¼Ô¸ÄÊÌÀßÄê
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    Æþ±¡ÅÐÏ¿¥µ¥Ö
           COPY    "CPI0SUB01.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTNUM.INC".
      *
      *    ¿ôÃÍ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *
      *    ½é´ü²Ã»»»»Äê¥µ¥Ö
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö
           COPY    "CPORCSNYUACCT.INC".
      *
      *    ÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö
           COPY "CPORCSTUSAN.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸¥µ¥Ö
           COPY    "CPORCSHKNSET.INC".
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¾È²ñ¥Æ¡¼¥Ö¥ë
           COPY  "CPORCSS010.INC".
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    °åÎÅ´Ñ»¡Æþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥Ñ¥é¥á¥¿
           COPY  "CPORCSIRYOKNS.INC".
      *
      *    ÆÃÄêÆþ±¡ÎÁ»»Äê´ü´Ö¼èÆÀ¥Ñ¥é¥á¥¿
           COPY  "CPORCSTOKNYUIN2.INC".
      *
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁÈ½Äê¥Ñ¥é¥á¥¿
           COPY  "CPORCSAKYUSEI.INC".
      *
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¥Ñ¥é¥á¥¿
           COPY  "CPORCSNYUKSNGET.INC".
      *
      *    Ìë¶Ð»þ´ÖÄ¶²á¸º»»Æþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
           COPY "CPORCSYAKINGEN.INC".
      *
      *    Ìë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
           COPY "CPORCSYAKINTOKU.INC".
      *
      *    ´Ç¸î¿¦°÷¿ôÅùÆþ±¡´ðËÜÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
           COPY "CPORCSKANGOCHG.INC".
      *
      *    Æ±ÆüºÆÆþ±¡È½Äê¥Ñ¥é¥á¥¿
           COPY  "CPORCSDOUNYUIN.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *    Æþ±¡¥ª¡¼¥À¡¼²ñ·×ºîÀ®
           COPY    "CPORCSODRNACCT.INC".
      *    Æþ±¡ÍúÎò
       01  LNK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"   REPLACING //PTNYUINRRK-//
                                        BY        //LNK-PTNYUINRRK-//.
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SODRNACCT-AREA
                                               LNK-PTNYUINRRK-REC
                                               SPA-AREA.
      *****************************************************************
      *    ¼ç    ½è    Íý
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 1001-SPA-SET-SEC
      *
           DISPLAY "SODRNACCT-ORDERID = " SODRNACCT-ORDERID
           MOVE    SPACE           TO  WRK-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    SODRNACCT-ORDERID
      *       ¡ÊÆþ±¡¡Ë
               WHEN    "0101"
                   PERFORM   4801-NYUKAIKEI-EDIT-SEC
      *       ¡ÊÂà±¡¡Ë
               WHEN    "0102"
                   PERFORM   4801-TAIINKAIKEI-EDIT-SEC
      *       ¡ÊÅ¾²Ê¡¿Å¾Åï¡¿Å¾¼¼¡Ë
               WHEN    "0103"
               WHEN    "0104"
               WHEN    "0105"
                   PERFORM   4801-TENROOM-EDIT-SEC
                   IF    IPN-NYUKAIKEI-SAKUSEI-FLG   =  "1"
                       PERFORM   4801-TENROOM-EDIT-SEC
                   END-IF
      *       ¡ÊÆþ±¡¼è¾Ã¡Ë
               WHEN    "0107"
      *       ¡ÊÆþ±¡¼è¾Ã¡Ý²ñ·×´Þ¤à¡Ë
               WHEN    "0108"
                   PERFORM   4801-NYUKAIKEI-ERASE-SEC
      *       ¡ÊÂà±¡¼è¾Ã¤·¡Ë
               WHEN    "0115"
                   PERFORM   4801-TAIINKAIKEI-CANCEL-SEC
      *       ¡Ê°ÛÆ°¼è¾Ã¤·¡Ë
               WHEN    "0116"
                   PERFORM   4801-IDOKAIKEI-CANCEL-SEC
      *       ¡Ê³°Çñ¡Ë
      *        0201:³°Çñ
      *        0202:¼£ÎÅ¤Î°Ù¤Î³°Çñ
      *        0203:ÁªÄêÆþ±¡Ãæ¤Î³°Çñ
      *        0205:µ¢±¡
               WHEN    "0201"
               WHEN    "0202"
               WHEN    "0203"
               WHEN    "0205"
                   IF   SODRNACCT-ENDYMD   =   SPACE
                       PERFORM   4801-GAIHAKU-SEC
                   ELSE
                       PERFORM   4801-GAIHAKU-KIKAN-SEC
                   END-IF
      *       ¡Ê¼è¤ê¾Ã¤·¡¦Â¾°åÎÅµ¡´Ø¼õ¿Ç¡¦ÆÃÄê»þ´ÖÂà±¡¸º»»¡¦ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡Ë
      *        0200:¼è¤ê¾Ã¤·
      *        0207:Â¾°åÎÅµ¡´Ø£·£°¡ó¸º
      *        0208:Â¾°åÎÅµ¡´Ø£³£°¡ó¸º
      *        0209:Â¾°åÎÅµ¡´Ø£µ£µ¡ó¸º
      *        0210:Â¾°åÎÅµ¡´Ø£±£µ¡ó¸º
      *        0211:ÆÃÄê»þ´ÖÂà±¡¸º»»
      *        0212:ÆÃÄêÍËÆüÆþÂà±¡¸º»»
      *        0213:ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø£±£°¡ó¸º
      *        0214:Â¾°åÎÅµ¡´Ø£³£µ¡ó¸º
      *        0215:Â¾°åÎÅµ¡´Ø£±£µ¡ó¸º
      *        0216:Â¾°åÎÅµ¡´Ø£µ¡ó¸º
      *        0217:ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø£µ¡ó¸º
               WHEN    "0200"
               WHEN    "0207"
               WHEN    "0208"
               WHEN    "0209"
               WHEN    "0210"
               WHEN    "0211"
               WHEN    "0212"
               WHEN    "0213"
               WHEN    "0214"
               WHEN    "0215"
               WHEN    "0216"
               WHEN    "0217"
                   IF   SODRNACCT-ENDYMD   =   SPACE
                       PERFORM   4801-GENSAN-SEC
                   ELSE
                       PERFORM   4801-GAIHAKU-KIKAN-SEC
                   END-IF
      *       ¡Ê¿©»ö¡Ë
      *        0301:¿©»ö
      *        0302:¿©»ö¡ÜÆÃÊÌ¿©²Ã»»
      *        0303:¿©»ö¡ÜÊ£¿ô¸¥Î©¡ÊÁªÂò¿©¡Ë²Ã»»¡ÊÊ¿À®£±£¸Ç¯£³·îËø¡Ë
      *        0304:¿©»ö¡ÜÆÃÊÌ¿©²Ã»»¡ÜÊ£¿ô¸¥Î©¡ÊÁªÂò¿©¡Ë²Ã»»¡ÊÊ¿À®£±£¸Ç¯£³·îËø¡Ë
      *        0305:¿©»ö¤Ê¤·
      *        0307:Î®Æ°¿©
               WHEN    "0301"
               WHEN    "0302"
               WHEN    "0303"
               WHEN    "0304"
               WHEN    "0305"
               WHEN    "0307"
      *
                   IF   SODRNACCT-STARTYMD(1:6)  >= "201604"
                       PERFORM   4801-SHOKUJI-201604-SEC
                   ELSE
                     IF   SODRNACCT-STARTYMD(1:6)  >= "200604"
                         PERFORM   4801-SHOKUJI-200604-SEC
                     ELSE
                         PERFORM   4801-SHOKUJI-SEC
                     END-IF
                   END-IF
      *
      *       ¡Ê»ØÄê·îÊ¬¤ÎÆþ±¡²ñ·×ºîÀ®¡Ë
               WHEN    "9901"
                   DISPLAY "nyuin-kaikei-sakusei-1"
                   PERFORM   4801-NYUKAIKEI-MONTH-SEC
           END-EVALUATE
      *
           EVALUATE    SODRNACCT-ORDERID(1:2)
      *       ¡Ê¼¼ÎÁº¹³Û¡Ë
               WHEN    "06"
                   IF   SODRNACCT-ENDYMD   =   SPACE
                       PERFORM   4801-ROOMSAGAKU-SEC
                   ELSE
                       PERFORM   4801-ROOMSAGAKU-KIKAN-SEC
                   END-IF
      *
      *       ¡Ê»ØÄê·îÊ¬¤ÎÆþ±¡²ñ·×ºîÀ®¡Ë
               WHEN    "9901"
                   DISPLAY "nyuin-kaikei-sakusei-2"
                   PERFORM   4801-NYUKAIKEI-MONTH-SEC
           END-EVALUATE
      *
           PERFORM 4900-ORCSS010-SEC
      *
           MOVE      ZERO    TO   SODRNACCT-RC
           .
      *
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           MOVE    NACCT-SRYYM         TO  WRK-LASTSRYYM
           MOVE    LNK-PTNYUINRRK-REC  TO  NRRK-REC
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    SPA´ðËÜ¾ðÊóÀßÄê½èÍý
      *****************************************************************
       1001-SPA-SET-SEC                SECTION.
      *
      *    INITIALIZE                          SPA-AREA
      *
           INITIALIZE                          SYSKANRI-REC
           MOVE    "1001"                  TO  SYS-KANRICD
           MOVE    "*"                     TO  SYS-KBNCD
           MOVE    LNK-PTNYUINRRK-TENNYUYMD TO SYS-STYUKYMD
                                               SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC            TO  SYS-1001-REC
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE    SYS-1001-PREFNUM        TO  SPA-PREFNUM
           MOVE    "1"                     TO  SPA-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
           MOVE    PTINF-BIRTHDAY          TO  SPA-BIRTHDAY
      *
           .
      *
       1001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-NYUKAIKEI-EDIT-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *---------------Âà±¡Æü¤ÎÆ±ÆüÆþ±¡½èÍý¤³¤³¤«¤é----------------------
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    Æ±ÆüºÆÆþ±¡ÍÑ¤ËÆþ±¡Æü¡ÊÅ¾ÆþÆü¡Ë¤òÂàÈò
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                       TO  WRK-DOU-NAI-I01-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  WRK-DOU-PTNYUINRRK-TENNYUYMD
      *    Æþ±¡ÍúÎò¤ÎÂàÈò
           MOVE   LNK-PTNYUINRRK-REC   TO  WRK-PTNYU-REC
      *    ºÇ½ªÆþ±¡ÍúÎò¤¬¼«±¡Îò¤«¤òÈ½Äê¤¹¤ë
           MOVE    ZERO                TO  FLG-JIINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM  910-DBFETCH-SEC
           END-IF
           IF    ( MCP-RC      =   ZERO  )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM  910-DBFETCH-SEC
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN   NOT =   "5"  AND "6"
                   IF  PTNYUINRRK-TAIINYMD  = WRK-DOU-NAI-I01-NYUINYMD                   
                       MOVE   1        TO  FLG-JIINRRK
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    Æþ±¡ÍúÎò¤ÎÉü¸µ
           MOVE    WRK-PTNYU-REC       TO  LNK-PTNYUINRRK-REC
      *    Á°Îò¤¬¼«±¡Îò¡ÊÆ±ÆüÂà±¡Ê¬¡Ë¤Î¾ì¹ç¡¢Æþ±¡²ñ·×ºîÀ®Æü¤ò£±Æü¤º¤é¤¹
           IF     FLG-JIINRRK    =     1
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    LNK-PTNYUINRRK-NYUINYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-PTNYUINRRK-NYUINYMD
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-DOU-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-DOU-TENNYUYMD
           END-IF
      *---------------Âà±¡Æü¤ÎÆ±ÆüÆþ±¡½èÍý¤³¤³¤Þ¤Ç----------------------
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    ÄÌ»»Æü¿ô¡¢»»Äê³«»ÏÆü¤òµá¤á¤ë
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD    NOT =  SPACE )
               GO  TO  4801-NYUKAIKEI-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
           PERFORM  4801-BTUSEL-SEC
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¤Î¾ì¹ç¤Ï´ü´Ö²Ã»»¤¬Â¸ºß¤¹¤ë¤¿¤á¡¢
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÎÅÍÜÉÂ¾²¤ÇÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤òÁê¸ß»»Äê¤¹¤ë¾ì¹ç¤â
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Ë»»Äê¤Ë¤¹¤ë°Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¶Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦°ìÈÌÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¸Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£³£°Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£²
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£³
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£´
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£µ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£¶
      *        ¡¦ÎÅÍÜÉÂÅï¡¦ÃÏ°è°ìÈÌÆþ±¡ÎÁ£±
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Î»»Äê
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         Ê¿À®£²£¸Ç¯²þÀµÂÐ±þ
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         Ê¿À®£³£°Ç¯²þÀµÂÐ±þ
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  WRK-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4018"      TO  WRK-ERRCD
                   GO  TO  4801-NYUKAIKEI-EDIT-EXT
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                            TO   LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    °åÎÅ´Ñ»¡Æþ±¡¥»¥Ã¥È
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     LNK-PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     LNK-PTNYUINRRK-TOKUTEI-KBN
                                             TO  LNK-IRYOKNS-KBN
           MOVE     LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                             TO  LNK-IRYOKNS-NYUIN
           MOVE     LNK-PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                        TO  LNK-SCNYUACCT-STSRYYMD
               MOVE   "99999999"        TO  LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    ÆÃÄêÆþ±¡¤ÇÆþ±¡ÎÁ¤¬ÊÑ¹¹¤Ë¤Ê¤ë¥³¡¼¥É¤ÎÂÐ±þ
      *
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                        TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   LNK-PTNYUINRRK-SHONUM TO  LNK-TOKNYUIN2-SHONUM
           MOVE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF     LNK-AKYUSEI-RC          =   1
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           END-IF
      *
      *
      *-------ÆÃÄêÆþ±¡ÂÐ±þ¤³¤³¤Þ¤Ç-----------------------------------
      *
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁÅù¤Ï
      *        ÆÃÄêÆþ±¡ÎÁ°·¤¤¤«¤é³°¤¹
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            ÆÃÄêÆþ±¡´ðËÜÎÁ¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤Ï¥Þ¥¹¥¿¤¬
      *            Ê¿À®£²£¶Ç¯£¹·î£³£°Æü¤Þ¤Ç¤Î°Ù¡¢»²¾È»þ¤ÎÆüÉÕ¤òÊÑ¹¹
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    ³°Çñ
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   0             TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    ¿©Æ²²Ã»»
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     LNK-PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
           MOVE  LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE  LNK-PTNYUINRRK-PTID     TO  SPA-PTID
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨¡ÊÊ¿À®£²£¶Ç¯²þÄê¡Ë
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡´ðËÜÎÁ¤Î¥³¡¼¥ÉºÙÊ¬²½ÃÖ¤­´¹¤¨
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            ÆÃÄêµ¡Ç½ÉÂ±¡¤ÏÉÂÅïÀßÄê¤µ¤ì¤¿Æþ±¡¥³¡¼¥É¤«¤éÈ½Äê
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        ÆÃÊÌÆþ±¡´ðËÜÎÁ
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     LNK-PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
           MOVE   LNK-PTNYUINRRK-BTUNUM
                                       TO  LNK-SCNYUACCT-BTU-NUM
      *
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   WRK-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
                   PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-NYUKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¤Î¼è¤ê¹þ¤ß½èÍý
      *****************************************************************
       4801-NYUKSN-GET-SEC         SECTION.
      *
      *
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¥µ¥Ö
           INITIALIZE                       ORCSNYUKSNGET-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-NYUKSNGET-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-NYUKSNGET-PTID
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                        TO  LNK-NYUKSNGET-SANTEIYM
               WHEN   OTHER
                   MOVE    SPA-SYSYMD   TO  LNK-NYUKSNGET-SANTEIYM
           END-EVALUATE
           MOVE   LNK-PTNYUINRRK-BTUNUM TO  LNK-NYUKSNGET-BTUNUM
           MOVE   LNK-PTNYUINRRK-BRMNUM TO  LNK-NYUKSNGET-BRMNUM
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1) NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           END-IF
      *
      *    ÆÃÄêÆþ±¡ÎÁ»»ÄêÍ×·ï³°¤ÇÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£É¤ò»»Äê»þ¤Ë¡¢Æþ±¡ÎÁ
      *    ²Ã»»¤òÆÃÊÌÈ½Äê¤¹¤ë¾ì¹ç¤ÎÈ½Äê¶èÊ¬¥»¥Ã¥È
           IF      LNK-TOKNYUIN-GAI-NYUKSNKBN   =    "1"
               MOVE   "1"               TO  LNK-NYUKSNGET-GAI-NYUKSNKBN
           END-IF
      *
      *    °ú¿ô¤ÇÀßÄê¤µ¤ì¤ëÆþ±¡ÎÁ²Ã»»¤Î¥»¥Ã¥È
           MOVE    ZERO                 TO  IDX
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN      >   SODRNACCT-KSN-MAX )
               IF    ( SODRNACCT-KSN-SRYCD (IDX-KSN)
                                        NOT =   SPACE )
                    ADD     1           TO     IDX
                    MOVE    SODRNACCT-KSN-SRYCD (IDX-KSN)
                                TO     LNK-NYUKSNGET-I01-KSN-SRYCD(IDX)
               END-IF
           END-PERFORM
      *
           CALL    "ORCSNYUKSNGET"      USING  ORCSNYUKSNGET-AREA
                                               SPA-AREA
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¥µ¥Ö¤«¤é¤ÎÊÖµÑÃÍ¥»¥Ã¥È
           PERFORM    VARYING    IDX8   FROM   1   BY   1
             UNTIL    (LNK-NYUKSNGET-KSNSRYCD(IDX8)  =  SPACE)  OR
                      (IDX8   >     20)
                 MOVE   LNK-NYUKSNGET-NYUKSNKBN(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-KBN(IDX8)
                 MOVE   LNK-NYUKSNGET-KSNSRYCD(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
           END-PERFORM
           MOVE    LNK-NYUKSNGET-TOKUBETSU-NYUIN
                                        TO  WRK-TOKUBETSU-NYUIN
      *
           .
      *
       4801-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð»»½Ð
      *****************************************************************
       4801-PT-AGE-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF    ( MCP-RC       =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF      FLG-PTINF           =   1
               MOVE    ZERO            TO  WRK-NENREI
           ELSE
               MOVE   PTINF-BIRTHDAY   TO  WRK-YMDS1
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-YMDS2
               MOVE   ZERO             TO  WRK-NENREI
               COMPUTE  WRK-NENREI     =   WRK-YYS2
                                       -   WRK-YYS1
               IF   WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4)
                    ADD   -1           TO  WRK-NENREI
               END-IF
           END-IF
      *
           .
      *
       4801-PT-AGE-EXT.
           EXIT.
      *
      *****************************************************************
      *     ÁªÄêÎÅÍÜÈñ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE              SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                             TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD    NOT =  SPACE
               MOVE    "099999916"   TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                     TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF    ( MCP-RC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    £±£µºÍÌ¤Ëþ¤Î´µ¼Ô¤ÏÁªÄêÆþ±¡ÂÐ¾Ý³°
           DISPLAY "PTINF-BIRTHDAY = " PTINF-BIRTHDAY
           MOVE     PTINF-BIRTHDAY   TO  WRK-BIRTHDAY-15
           ADD      15               TO  WRK-BIRTHDAY-15Y
           DISPLAY  "WRK-SENTEI-STYMD = " WRK-SENTEI-STYMD
           DISPLAY  "WRK-BIRTHDAY-15 = " WRK-BIRTHDAY-15
           IF       WRK-SENTEI-STYMD <=  WRK-BIRTHDAY-15
               MOVE   WRK-BIRTHDAY-15  TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
      *
           .
       4801-SENTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄÌ»»Æü¿ô¼èÆÀ½èÍý
      *****************************************************************
       4801-TSUSAN-SANTEI-GET-SEC      SECTION.
      *
           INITIALIZE                  WRK-TSUSAN
                                       WRK-SANTEISTYMD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  STUSAN-PTID
           IF   IPN-NYUKAIKEI-SAKUSEI-FLG  =   "1"
               MOVE   IPN-SANTEISTYMD  TO  STUSAN-KJNYMD
           ELSE
      *---------------Âà±¡Æü¤ÎÆ±ÆüÆþ±¡½èÍý¤³¤³¤«¤é----------------------
               IF   WRK-DOU-TENNYUYMD    NOT =   SPACE
                   MOVE    WRK-DOU-TENNYUYMD
                                       TO  LNK-PTNYUINRRK-TENNYUYMD
               END-IF
      *---------------Âà±¡Æü¤ÎÆ±ÆüÆþ±¡½èÍý¤³¤³¤Þ¤Ç----------------------
               MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  STUSAN-KJNYMD
           END-IF
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF    ( STUSAN-RC       NOT =   ZERO )
      *         MOVE    "4025"      TO  WRK-ERRCD
               DISPLAY  "WRK-ERRCD = " WRK-ERRCD
      *         GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
      *    Â¾±¡¡ÊÆÃÊÌ¤Ê´Ø·¸¤Ë¤¢¤ë°åÎÅµ¡´Ø¡Ë¤ÎÆü¿ô¤ò²Ã»»
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU03
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU05
      *
           COMPUTE WRK-TSUSAN  =   STUSAN-NISSU03  -   1
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    STUSAN-KJNYMD   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN       =   180 - ( STUSAN-NISSU04 )
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂÅï¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    LNK-PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      **         WHEN   "08"
      **             MOVE   WRK-NAI-I01-IDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
      **         WHEN   "09"
      **             MOVE   SPA-NAI-I01-MAEIDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-EVALUATE
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                   TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5001-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-BTUSEL-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂ¼¼¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       4801-BRMSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    LNK-PTNYUINRRK-BRMNUM
                                   TO  SYS-KBNCD
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      **         WHEN   "08"
      **             MOVE   WRK-NAI-I01-IDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
      **         WHEN   "09"
      **             MOVE   SPA-NAI-I01-MAEIDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-EVALUATE
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                   TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5002-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-BRMSEL-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¥»¥Ã¥È½èÍý
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤ò¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ
           INITIALIZE                           ORCSTOKNYUIN-AREA
           MOVE    LNK-PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
           MOVE    LNK-PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
           MOVE    LNK-PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
           MOVE    LNK-PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                        TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
           MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF      LNK-TOKNYUIN-RC          =   ZERO
               IF    LNK-TOKNYUIN-GAI-SRYCD =  SPACE
                   MOVE   LNK-TOKNYUIN-SRYCD   TO  SYOKIKSN-IN-KHNSRYCD
               ELSE
                   MOVE   LNK-TOKNYUIN-GAI-SRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ìë¶Ð»þ´ÖÄ¶²á¸º»»¥³¡¼¥É¤Ø¤ÎÃÖ¤­´¹¤¨½èÍý
      *    Ìë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¥³¡¼¥É¤Ø¤ÎÃÖ¤­´¹¤¨½èÍý
      *    ´Ç¸î¿¦°÷¿ôÅùÆþ±¡´ðËÜÎÁ¥³¡¼¥É¤Ø¤ÎÃÖ¤­´¹¤¨½èÍý
      *****************************************************************
       4801-YAKINGEN-CHG-SEC   SECTION.
      *
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20140401"
      *        Ìë¶Ð»þ´ÖÄ¶²á¸º»»Æþ±¡ÎÁ¥³¡¼¥É¼èÆÀ
               INITIALIZE    ORCSYAKINGEN-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINGEN-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINGEN-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINGEN-SRYCD
               CALL   "ORCSYAKINGEN"    USING   ORCSYAKINGEN-AREA
                                                SPA-AREA
               IF   LNK-YAKINGEN-RC     =       ZERO
                    MOVE LNK-YAKINGEN-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20160401"
      *        Ìë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¥³¡¼¥É¼èÆÀ
               INITIALIZE    ORCSYAKINTOKU-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINTOKU-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINTOKU-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINTOKU-SRYCD
               CALL   "ORCSYAKINTOKU"    USING   ORCSYAKINTOKU-AREA
                                                 SPA-AREA
               IF   LNK-YAKINTOKU-RC     =       ZERO
                    MOVE LNK-YAKINTOKU-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
      *        ´Ç¸î¿¦°÷¿ôÅùÆþ±¡´ðËÜÎÁ¥³¡¼¥É¼èÆÀ
               INITIALIZE    ORCSKANGOCHG-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-KANGOCHG-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-KANGOCHG-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-KANGOCHG-SRYCD
               CALL   "ORCSKANGOCHG"    USING   ORCSKANGOCHG-AREA
                                                SPA-AREA
               IF   LNK-KANGOCHG-RC     =       ZERO
                    MOVE LNK-KANGOCHG-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-YAKINGEN-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ°ìÈÌ¥³¡¼¥É¤«¤éÏ·¿Í¥³¡¼¥É¤Ø¤ÎÃÖ¤­´¹¤¨½èÍý
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  WRK-AGECHK-SRYYMD
           PERFORM 4801-RJN-TAISYO-CHK-SEC
           IF    ( FLG-RJN         =   1 )    AND
                 ( COMB-HKNNUM     NOT =   "971" )  AND
                 ( COMB-HKNNUM     NOT =   "973" )
      *
      *        Ê¿À®£±£¸Ç¯£´·î²þÄêÂÐ±þ
      *        Ï·¿Í¥³¡¼¥ÉÃÖ¤­´¹¤¨¤ÏÎÅÍÜÉÂÅï¡¢ÎÅÍÜÉÂ¾²¤Î¤ß
               IF   SYOKIKSN-IN-SANTEISTYMD(1:6)  >=  "200604"
                   IF   SYOKIKSN-IN-KHNSRYCD  =  "190080610" OR
                                                 "190080710" OR
                                                 "190081310" OR
                                                 "190081410" OR
                                                 "190098210" OR
                                                 "190098310"
                       INITIALIZE              SRYCDCHG-REC
                       MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                       MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DB-SEL-FETCH-SEC 
                       IF    ( MCP-RC      =   ZERO )
                           MOVE    MCPDATA-REC
                                               TO   SRYCDCHG-REC
                           IF   CHG-RJNSRYCD   NOT =   SPACE
                                MOVE   CHG-RJNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                           END-IF
                       END-IF
      *
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DBCLOSECURSOR-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC 
                   IF    ( MCP-RC      =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   SRYCDCHG-REC
                       IF   CHG-RJNSRYCD   NOT =   SPACE
                            MOVE   CHG-RJNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *****************************************************************
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
      *****************************************************************
       4801-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    Æþ±¡´µ¼Ô¤ÎÏ·¿ÍÊÝ¸±È½Äê
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE              HKNCOMBI-REC
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO   COMB-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID
                                       TO   COMB-PTID
           IF      SODRNACCT-ORDERID(3:2) =   "07"  OR  "09"
               MOVE    WRK-TAIHI-HKN   TO   COMB-HKNCOMBINUM
           ELSE
               MOVE    LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO   COMB-HKNCOMBINUM
           END-IF
           MOVE    HKNCOMBI-REC        TO   MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC 
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                       TO   HKNCOMBI-REC
           ELSE
               MOVE    1               TO   FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM  4801-PT-AGE-SEC
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð·×»»
           INITIALIZE ORCSAGECHKAREA
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  AGECHK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID
                                       TO  AGECHK-PTID
           MOVE    WRK-AGECHK-SRYYMD
                                       TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY
                                       TO  AGECHK-BIRTHDAY
           DISPLAY "AGECHK-SRYYMD" AGECHK-SRYYMD
           DISPLAY "AGECHK-BIRTHDAY" AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼ÔÈ½Äê
           IF    ((FLG-HKNCOMBI     =   ZERO)    AND
                  (COMB-KOH1HKNNUM  =   "027"))      OR
                 ( AGECHK-NENREI   >=    75)         OR
                 ((AGECHK-NENREI   >=    70)     AND
                  (PTINF-BIRTHDAY  <=   "19320930")) OR
                  (AGECHK-ROUJIN    =    1)
               MOVE    1            TO  FLG-RJN
           END-IF
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó¤«¤éÏ·¿ÍÊÝ·ò¤ÎÅÐÏ¿¥Á¥§¥Ã¥¯
           INITIALIZE                    PTKOHINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO   PTKOH-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID    TO   PTKOH-PTID
           MOVE    "027"            TO   PTKOH-KOHNUM
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKSTYMD
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKEDYMD
           MOVE    PTKOHINF-REC     TO   MCPDATA-REC
           MOVE    "tbl_ptkohinf"   TO   MCP-TABLE
           MOVE    "key6"           TO   MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC 
           IF    ( MCP-RC    =   ZERO )
               MOVE    1            TO  FLG-RJN
           END-IF
           MOVE    "tbl_ptkohinf"   TO   MCP-TABLE
           MOVE    "key6"           TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-RJN-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¼èÆÀ½èÍý
      *****************************************************************
       490-HKNCOMBI-GET-SEC            SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
                                       ORCSHKNSETAREA
      *
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTINF-BIRTHDAY
                                       TO  SPA-BIRTHDAY
           MOVE    "2"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
               GO  TO  490-HKNCOMBI-GET-EXT
           END-IF
      *
           .
       490-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸½èÍý
      *****************************************************************
       490-HKNCOMBI-EDIT-SEC           SECTION.
      *
           MOVE    SPACE                   TO  WRK-HKNCOMBI
           PERFORM VARYING IDX15   FROM 1  BY  1
                   UNTIL ( IDX15   >   ORCSHKN-HKN-MAX )
                     OR  ( IDX15   >   100 )
      *
               IF    ( ORCSHKN-GMN-HKNCOMBINUM (IDX15)
                                   =   WRK-ORCSHKN-HKNCOMBINUM )
                   MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX15)
                                           TO  WRK-HKNCOMBI
                   MOVE    ORCSHKN-HKN-MAX TO  IDX15
               END-IF
      *
           END-PERFORM
      *
           .
       490-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕ¥Á¥§¥Ã¥¯¡¦ÊÔ½¸½èÍý
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    ÆüÉÕ²èÌÌ¥Á¥§¥Ã¥¯¥µ¥Ö
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  WRK-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´ü´Ö»»½Ð½èÍý
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSDAY
                                           WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1   NOT =   ZERO )
               MOVE    1           TO  FLG-ORCSDAY
               GO  TO              5002-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    ¥«¥ì¥ó¥À¡¼½èÍý
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    Å¾²Ê¡¿Å¾Åï¡¿Å¾¼¼»þ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-TENROOM-EDIT-SEC         SECTION.
      *
           DISPLAY "4801-TENROOM-EDIT-SEC"
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
           MOVE    SPACE   TO   SPA-ERRCD
      *    ÄÌ»»Æü¿ô¡¢»»Äê³«»ÏÆü¤òµá¤á¤ë
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-TENROOM-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
      *     DISPLAY "====AZU  TENROOM-EDIT =================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    Ê¿À®£²£´Ç¯£³·î£³£±Æü»þÅÀ¤Ç°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¤ò»»Äê
      *    ¤·¤Æ¤¤¤ë¾õÂÖ¤«È½Äê¤¹¤ë¥µ¥Ö¤ò¸Æ¤Ó½Ð¤·
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF  LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE LNK-PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE LNK-PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE LNK-PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *    Æ±ÆüºÆÆþ±¡Æü¤Ç¤ÎÅ¾²Ê¡¿Å¾Åï¡¿Å¾¼¼½èÍý¤«È½Äê¤¹¤ë¥µ¥Ö¤ò¸Æ¤Ó½Ð¤·
           INITIALIZE                   ORCSDOUNYUIN-AREA
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  LNK-DOUNYUIN-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  LNK-DOUNYUIN-PTID
           MOVE    LNK-PTNYUINRRK-RRKNUM    TO  LNK-DOUNYUIN-RRKNUM
           MOVE    LNK-PTNYUINRRK-TENNYUYMD TO  LNK-DOUNYUIN-SAINYUINYMD
           CALL    "ORCSDOUNYUIN"    USING  ORCSDOUNYUIN-AREA
                                            SPA-AREA
      *     DISPLAY  "LNK-DOUNYUIN-RC = " LNK-DOUNYUIN-RC
      *     DISPLAY  "LNK-DOUNYUIN-SANTEIYMD = " LNK-DOUNYUIN-SANTEIYMD
      *
      *    Æ±ÆüºÆÆþ±¡Ê¬¤Î³«»ÏÆüÊÑ¹¹
           IF     (LNK-DOUNYUIN-RC = 1)    AND
                  (LNK-DOUNYUIN-SANTEIYMD  NOT =  SPACE)
               MOVE   LNK-DOUNYUIN-SANTEIYMD
                           TO   SYOKIKSN-IN-SANTEISTYMD
           END-IF
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¤Î¾ì¹ç¤Ï´ü´Ö²Ã»»¤¬Â¸ºß¤¹¤ë¤¿¤á¡¢
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÎÅÍÜÉÂ¾²¤ÇÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤òÁê¸ß»»Äê¤¹¤ë¾ì¹ç¤â
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Ë»»Äê¤Ë¤¹¤ë°Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¶Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦°ìÈÌÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¸Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£³£°Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£²
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£³
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£´
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£µ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£¶
      *        ¡¦ÎÅÍÜÉÂÅï¡¦ÃÏ°è°ìÈÌÆþ±¡ÎÁ£±
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Î»»Äê
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         Ê¿À®£²£¸Ç¯²þÀµÂÐ±þ
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         Ê¿À®£³£°Ç¯²þÀµÂÐ±þ
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4021"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4021"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4022"      TO  SPA-ERRCD
                   GO  TO  4801-TENROOM-EDIT-EXT
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®Á°¤Ë²ñ·×¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤òÂàÈò
      *
           MOVE      SPACE                 TO  WRK-TENROOM-SKJ-TABLE
           INITIALIZE                          WRK-TENROOM-SKJ-TABLE
      *
           IF  LNK-PTNYUINRRK-TENNYUYMD(1:6)  >= "200604"
      *      ¡Ê°ÛÆ°·îÊ¬¡Ë
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(1)
               END-IF
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
      *      ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               IF   IDX-SKJ-TAIHI     =    1
                   IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                       MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
                   ELSE
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
                   END-IF
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM       TO  WRK-TENROOM-SKJ-SRYYM(2)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ18(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  2
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ19(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  3
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ20(2 IDX-SKJ)
                       END-PERFORM
                 END-EVALUATE
               ELSE
                   MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
               END-IF
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
           ELSE
      *      ¡Ê°ÛÆ°·îÊ¬¡Ë
             INITIALIZE                      NYUINACCT-REC
             MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
             MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  NACCT-SRYYM
                                             WRK-TENROOM-SKJ-SRYYMX
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DB-SEL-FETCH-SEC
             IF      MCP-RC              =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   ZERO        TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             END-IF
      *      Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *      ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
             INITIALIZE                      NYUINACCT-REC
             MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
             IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                 MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
             ELSE
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
             END-IF
             MOVE    WRK-TENROOM-SKJ-SRYYMX
                                         TO  NACCT-SRYYM
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DB-SEL-FETCH-SEC
             IF      MCP-RC              =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
                 PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
             END-IF
      *      Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®Á°¤Ë²ñ·×¥Ç¡¼¥¿¤Î³°Çñ¶èÊ¬¤òÂàÈò
      *    ¡Ê³°Çñ°ÛÆ°·îÊ¬¡Ë
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                       TO  NACCT-SRYYM
                                           WRK-TENROOM-SKJ-SRYYMX
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        ³°Çñ¤òÂàÈò
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           ELSE
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                       TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   ZERO        TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    ¡Ê³°Çñ°ÛÆ°Íâ·îÊ¬¡Ë
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           IF      WRK-TENROOM-SKJ-SRYMM9  =  12
               ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
               MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
           ELSE
               ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
           END-IF
           MOVE    WRK-TENROOM-SKJ-SRYYMX
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        ³°Çñ¤òÂàÈò
               PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE                     ORCSNYUACCT-AREA
           MOVE    "8"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                TO  SPA-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE    LNK-PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                           TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                           TO   LNK-SCNYUACCT-STSRYYMD
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                           TO   LNK-SCNYUACCT-EDSRYYMD
      *
      *    Æ±ÆüºÆÆþ±¡Ê¬¤Î³«»ÏÆüÊÑ¹¹
           IF     (LNK-DOUNYUIN-RC = 1)    AND
                  (LNK-DOUNYUIN-SANTEIYMD  NOT =  SPACE)
               MOVE   LNK-DOUNYUIN-SANTEIYMD
                           TO   LNK-SCNYUACCT-STSRYYMD
                                LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    °åÎÅ´Ñ»¡Æþ±¡¥»¥Ã¥È
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     LNK-PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     LNK-PTNYUINRRK-TOKUTEI-KBN
                                             TO  LNK-IRYOKNS-KBN
           MOVE     LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                             TO  LNK-IRYOKNS-NYUIN
           MOVE     LNK-PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                        TO  LNK-SCNYUACCT-STSRYYMD
               MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    ÆÃÄêÆþ±¡¤ÇÆþ±¡ÎÁ¤¬ÊÑ¹¹¤Ë¤Ê¤ë¥³¡¼¥É¤ÎÂÐ±þ
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                        TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-TOKNYUIN2-SANTEIYMD
      *    Æ±ÆüºÆÆþ±¡Ê¬¤Î³«»ÏÆüÊÑ¹¹
           IF     (LNK-DOUNYUIN-RC = 1)    AND
                  (LNK-DOUNYUIN-SANTEIYMD  NOT =  SPACE)
               MOVE   LNK-DOUNYUIN-SANTEIYMD
                                        TO   LNK-TOKNYUIN2-SANTEIYMD
           END-IF
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   LNK-PTNYUINRRK-SHONUM TO  LNK-TOKNYUIN2-SHONUM
           MOVE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF     LNK-AKYUSEI-RC          =   1
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
      *    ÎÅÍÜÉÂÅï¡ÊÉÂ¾²¡ËÆþ±¡¤Î¥Õ¥é¥°¥»¥Ã¥È
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-PTNYUINRRK-BTUNUM
                                       TO  SYS-5001-KBNCD
           MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                       TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                       MOVE    "1"     TO
                                         LNK-TOKNYUIN2-TOKUBETSU-FLG(2)
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                IF    LNK-TOKNYUIN2-RC  =   1
                    MOVE    SPACE      TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                           SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                END-IF
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *-------ÆÃÄêÆþ±¡ÂÐ±þ¤³¤³¤Þ¤Ç-----------------------------------
      *
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁÅù¤Ï
      *        ÆÃÄêÆþ±¡ÎÁ°·¤¤¤«¤é³°¤¹
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            ÆÃÄêÆþ±¡´ðËÜÎÁ¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤Ï¥Þ¥¹¥¿¤¬
      *            Ê¿À®£²£¶Ç¯£¹·î£³£°Æü¤Þ¤Ç¤Î°Ù¡¢»²¾È»þ¤ÎÆüÉÕ¤òÊÑ¹¹
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    ³°Çñ
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
                  MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           ELSE
                  MOVE   0      TO  LNK-SCNYUACCT-SHOKUJIKBN
           END-IF
           IF     LNK-PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"      TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   LNK-PTNYUINRRK-SKJKBN-ASA
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   LNK-PTNYUINRRK-SKJKBN-HIRU
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   LNK-PTNYUINRRK-SKJKBN-YU
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    ¿©Æ²²Ã»»
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    Å¾²Ê¸µÆþ±¡²Ê
           MOVE   "00"          TO  LNK-SCNYUACCT-MOTO-NYUINKA
      **     MOVE   SPA-GMN-I01-NYUINKA-MOT (1 : 2)
      **                          TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     LNK-PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨¡ÊÊ¿À®£²£¶Ç¯²þÄê¡Ë
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡´ðËÜÎÁ¤Î¥³¡¼¥ÉºÙÊ¬²½ÃÖ¤­´¹¤¨
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            ÆÃÄêµ¡Ç½ÉÂ±¡¤ÏÉÂÅïÀßÄê¤µ¤ì¤¿Æþ±¡¥³¡¼¥É¤«¤éÈ½Äê
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        ÆÃÊÌÆþ±¡´ðËÜÎÁ
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     LNK-PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
           MOVE   LNK-PTNYUINRRK-BTUNUM
                                       TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¸å¤ËÂàÈò¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤òÉü¸µ
           IF  LNK-PTNYUINRRK-TENNYUYMD(1:6)  >= "200604"
      *       ¡Ê°ÛÆ°·îÊ¬¡Ë
               IF   WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE
                PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                 MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                             TO  NACCT-SRYYM
      *
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
                     MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *              ¿©»ö¤òÉü¸µ
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ18(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ19(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ20(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                     END-EVALUATE
      *              ºÞ²ó¿ôºÆ¥»¥Ã¥È
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM   
                 END-IF
                 MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 PERFORM 910-DBUPDATE-SEC
      *          Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 910-DBCLOSECURSOR-SEC
                END-PERFORM
               END-IF
      *
      *       ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
                PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
      *
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                ¿©»ö¤òÉü¸µ
                       EVALUATE  IDX-SKJ-TAIHI
                           WHEN  1
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ18(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  2
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ19(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  3
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ20(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                       END-EVALUATE
      *                ºÞ²ó¿ôºÆ¥»¥Ã¥È
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
      *            Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM 910-DBCLOSECURSOR-SEC
                END-PERFORM
               END-IF
           ELSE
      *       ¡Ê°ÛÆ°·îÊ¬¡Ë
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DB-SEL-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ¿©»ö¤òÉü¸µ
                   MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
                   PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                             UNTIL     IDX-SKJ   >   31
                         MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
      *       ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM   910-DB-SEL-FETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                ¿©»ö¤òÉü¸µ
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                       END-PERFORM
      *                ºÞ²ó¿ôºÆ¥»¥Ã¥È
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¸å¤ËÂàÈò¥Ç¡¼¥¿¤Î³°Çñ¶èÊ¬¤òÉü¸µ
      *    ¡Ê°ÛÆ°·îÊ¬¡Ë
           INITIALIZE                        NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        ³°Çñ¤òÉü¸µ
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                     MOVE   WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
                                       TO  NACCT-DAY(IDX-SKJ)
               END-PERFORM
      *        ºÞ²ó¿ôºÆ¥»¥Ã¥È
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                        UNTIL     IDX-SKJ   >   31
                   IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                       ADD   1        TO  NACCT-ZAIKAISU
                   END-IF
               END-PERFORM   
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *   ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
           IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               INITIALIZE                       NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM   910-DB-SEL-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ³°Çñ¤òÉü¸µ
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       4801-TENROOM-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âà±¡¼è¾Ã¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-TAIINKAIKEI-CANCEL-SEC         SECTION.
      *
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ»»Äê»þ¤ËÂà±¡Æü¤ò£Ó£Ð£Á¡Ý°ÛÆ°Æü¤ÈÅ¾ÆþÆü¤Ë¥»¥Ã¥È
      *    °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤âÆ±ÍÍ
      *      µßÌ¿µßµÞÆþ±¡ÎÁ£±
      *      µßÌ¿µßµÞÆþ±¡ÎÁ£²
      *      °ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
      *      ¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
      *      °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ
      *      Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
      *      °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
      *      °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë
      *      °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë
      *      µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *      µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *      µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *      µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *      ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
      *      ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *      ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *      ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë
      *      ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
      *      ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡ÊÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍý£±¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *      ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡ÊÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍý£²¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF LNK-PTNYUINRRK-TOKUTEI-NYUIN = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19" OR "54" OR "55" OR
                                                "56" OR "57" OR "58" OR
                                                "59" OR "60" OR "91" OR
                                                "92" OR "93" OR "95" OR
                                                "96" OR "AH" OR "AI" OR
                                                "AT" OR "AJ" OR "AK" OR
                                                "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS" OR "AU" OR
                                                "AV"
                    MOVE   LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
               END-IF
      *        ÎÅÍÜÉÂÅï¤ÈÎÅÍÜÉÂ¾²¤âÈ½Äê¾ò·ï¤ËÄÉ²Ã
               IF    ((LNK-PTNYUINRRK-TOKUTEI-NYUIN >=   "25")  AND
                      (LNK-PTNYUINRRK-TOKUTEI-NYUIN <=   "48"))
                                             OR
                     ((LNK-PTNYUINRRK-TOKUTEI-NYUIN >=   "63")  AND
                      (LNK-PTNYUINRRK-TOKUTEI-NYUIN <=   "88"))
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                               LNK-PTNYUINRRK-TENNYUYMD
               END-IF
           ELSE
               IF  LNK-PTNYUINRRK-TOKUTEI-KBN   =   SPACE
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                               LNK-PTNYUINRRK-TENNYUYMD
      *             MOVE   SPA-NAI-I01-TAIINYMD
      *                                 TO   PTNYUINRRK-TENNYUYMD
               END-IF
      *        ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡¢Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡¢Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *        ¤ò»»Äê»þ¤ËÂà±¡Æü¤ò£Ó£Ð£Á¡Ý°ÛÆ°Æü¤ÈÅ¾ÆþÆü¤Ë¥»¥Ã¥È
      *        °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤âÆ±ÍÍ
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *        ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
               IF   (PTNYUINRRK-TOKUTEI-KBN   =   "2")  AND
                    (PTNYUINRRK-TOKUTEI-NYUIN  = "09" OR "05" OR "15"
                                              OR "41" OR "42" OR "43"
                                              OR "44" OR "45" OR "46"
                                              OR "47" OR "48" )
                    MOVE   LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
               END-IF
           END-IF
      *
      *    Âà±¡¼è¾ÃÁ°¤Î²ñ·×¸µ¥Ç¡¼¥¿ºîÀ®
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    Æþ±¡²ñ·×ºï½üÁ°¤Ë²ñ·×¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤ÈÊÝ¸±ÁÈ¹ç¤»¤òÂàÈò
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    LNK-PTNYUINRRK-TAIINYMD(1:6)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       MCP-RC       NOT =   ZERO
      *
                   MOVE     MCPDATA-REC   TO   NYUINACCT-REC
      *
      *            ¿©»ö¤òÂàÈò
                   IF     (NACCT-ZAISKBKBN     =   "4")   AND
                          (NACCT-SRYCDTOTAL    =   99999920)
                       MOVE    LNK-PTNYUINRRK-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-SHOKUJI
                       END-IF
                   END-IF
      *            ÊÝ¸±¤òÂàÈò
                   IF      NACCT-ZAISKBKBN     =   "5"
                       MOVE   LNK-PTNYUINRRK-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-HKN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key16"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           DISPLAY "=========KAKUNIN =================="
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO  LNK-PTNYUINRRK-TENNYUYMD
      *    ÄÌ»»Æü¿ô¡¢»»Äê³«»ÏÆü¤òµá¤á¤ë
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
      *     DISPLAY "====AZU  TAIIN-CANCEL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    Ê¿À®£²£´Ç¯£³·î£³£±Æü»þÅÀ¤Ç°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¤ò»»Äê
      *    ¤·¤Æ¤¤¤ë¾õÂÖ¤«È½Äê¤¹¤ë¥µ¥Ö¤ò¸Æ¤Ó½Ð¤·
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE  LNK-PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE  LNK-PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE  LNK-PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
           PERFORM  4801-BTUSEL-SEC
           MOVE     SYS-5001-BTU-KHNSRYCD
                          TO    SYOKIKSN-IN-KHNSRYCD
      *
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¤Î¾ì¹ç¤Ï´ü´Ö²Ã»»¤¬Â¸ºß¤¹¤ë¤¿¤á¡¢
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÎÅÍÜÉÂ¾²¤ÇÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤òÁê¸ß»»Äê¤¹¤ë¾ì¹ç¤â
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Ë»»Äê¤Ë¤¹¤ë°Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¶Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦°ìÈÌÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¸Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£³£°Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£²
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£³
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£´
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£µ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£¶
      *        ¡¦ÎÅÍÜÉÂÅï¡¦ÃÏ°è°ìÈÌÆþ±¡ÎÁ£±
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Î»»Äê
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         Ê¿À®£²£¸Ç¯²þÀµÂÐ±þ
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         Ê¿À®£³£°Ç¯²þÀµÂÐ±þ
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4019"      TO  SPA-ERRCD
                       GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
                   END-IF
               ELSE
                   MOVE    "4020"      TO  SPA-ERRCD
                   GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE     "7"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID
                                       TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA
                                       TO  SPA-SRYKA
           MOVE     LNK-PTNYUINRRK-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *Âà±¡Æü¤«¤é¤Î²ñ·×ºÆºîÀ®¤ËÊÑ¹¹ ---------------------------------
      *///     MOVE     SPA-NAI-I01-TAIINYMD
      *///                                 TO   LNK-SCNYUACCT-STSRYYMD
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *h26.05.16 ADD
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
      *--------------------------------------------------------------
      *
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    ÆÃÄêÆþ±¡¤ÇÆþ±¡ÎÁ¤¬ÊÑ¹¹¤Ë¤Ê¤ë¥³¡¼¥É¤ÎÂÐ±þ
      *
           DISPLAY "===  TEST TAIINKAIKEI-CANCEL ==="
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                        TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TAIINYMD
                                        TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   LNK-PTNYUINRRK-SHONUM TO  LNK-TOKNYUIN2-SHONUM
           MOVE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF    (LNK-AKYUSEI-RC        =   1)           AND
                 (LNK-PTNYUINRRK-TAIINYMD  >=   "20120401")
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
      *    ÎÅÍÜÉÂÅï¡ÊÉÂ¾²¡ËÆþ±¡¤Î¥Õ¥é¥°¥»¥Ã¥È
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-PTNYUINRRK-BTUNUM
                                       TO  SYS-5001-KBNCD
           MOVE    LNK-PTNYUINRRK-TAIINYMD(1:6)
                                       TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                       MOVE    "1"     TO
                                         LNK-TOKNYUIN2-TOKUBETSU-FLG(2)
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                IF    LNK-TOKNYUIN2-RC  =   1
                    MOVE    SPACE      TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                           SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                END-IF
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *-------ÆÃÄêÆþ±¡ÂÐ±þ¤³¤³¤Þ¤Ç-----------------------------------
      *
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    Âà±¡Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÉÂ±¡¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN1-SEC
      *
      *    Âà±¡Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡Ê¿ÇÎÅ½ê¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN2-SEC
      *
      *    Âà±¡Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN3-SEC
      *
      *    Âà±¡Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN4-SEC
      *
      *    Âà±¡Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN5-SEC
      *
      *    Âà±¡Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN6-SEC
      *
      *    Âà±¡Æü¤ËÍ­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-YUUSHOUIPPAN-KSN-SEC
      *
      *    Âà±¡Æü¤ËºßÂð´µ¼Ô»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î°·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-ZAITAKU-KSN1-SEC
      *
      *    Âà±¡Æü¤ËºßÂð´µ¼Ô»Ù±çÉÂ¾²½é´ü²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î°·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-ZAITAKU-KSN2-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁÅù¤Ï
      *        ÆÃÄêÆþ±¡ÎÁ°·¤¤¤«¤é³°¤¹
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            ÆÃÄêÆþ±¡´ðËÜÎÁ¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤Ï¥Þ¥¹¥¿¤¬
      *            Ê¿À®£²£¶Ç¯£¹·î£³£°Æü¤Þ¤Ç¤Î°Ù¡¢»²¾È»þ¤ÎÆüÉÕ¤òÊÑ¹¹
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    ³°Çñ
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           IF     LNK-PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   LNK-PTNYUINRRK-SKJKBN-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   LNK-PTNYUINRRK-SKJKBN-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   LNK-PTNYUINRRK-SKJKBN-YU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    ¿©Æ²²Ã»»
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           IF     WRK-TAIHI-HKN    NOT =   ZERO
                  MOVE   WRK-TAIHI-HKN
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           ELSE
                  MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
                  MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           END-IF
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     LNK-PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨¡ÊÊ¿À®£²£¶Ç¯²þÄê¡Ë
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡´ðËÜÎÁ¤Î¥³¡¼¥ÉºÙÊ¬²½ÃÖ¤­´¹¤¨
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            ÆÃÄêµ¡Ç½ÉÂ±¡¤ÏÉÂÅïÀßÄê¤µ¤ì¤¿Æþ±¡¥³¡¼¥É¤«¤éÈ½Äê
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        ÆÃÊÌÆþ±¡´ðËÜÎÁ
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     LNK-PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
           MOVE   LNK-PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
      **         MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-TAIINKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÉÂ±¡¡Ë¤Î»»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-KYUKYU-KSN1-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key44"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190134270"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡Ê¿ÇÎÅ½ê¡Ë¤Î»»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-KYUKYU-KSN2-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING    STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key45"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190135670"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ë¤Î
      *    »»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-KYUKYU-KSN3-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key48"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190143370"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤Î
      *    »»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-KYUKYU-KSN4-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key49"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190142970"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN4-EXT.
           EXIT.
      *
      *****************************************************************
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ë¤Î
      *    »»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-KYUKYU-KSN5-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key50"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190152470"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN5-EXT.
           EXIT.
      *
      *****************************************************************
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë¤Î
      *    »»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-KYUKYU-KSN6-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key52"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190177170"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN6-EXT.
           EXIT.
      *
      *****************************************************************
      *    Í­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»¤Î»»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-YUUSHOUIPPAN-KSN-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING    STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key46"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190135470"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-YUUSHOUIPPAN-KSN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼Ô»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤Î»»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-ZAITAKU-KSN1-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key60"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190201870"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-ZAITAKU-KSN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼Ô»Ù±çÉÂ¾²½é´ü²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë¤Î»»ÄêÍ­Ìµ¥Á¥§¥Ã¥¯
      *****************************************************************
       210-ZAITAKU-KSN2-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    Å¾ÆþÆüÉÕ¤Î¥Þ¥¤¥Ê¥¹£±Æü
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  LNK-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  WRK-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key61"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190212470"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-ZAITAKU-KSN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    °ÛÆ°¼è¾Ã»þ²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-IDOKAIKEI-CANCEL-SEC       SECTION.
      *
           MOVE   SPACE                 TO  WRK-DOU-TENNYUYMD
                                            WRK-MAEIDOYMD
      *     DISPLAY "=============AZU1  SAKUJO ========"
      *     DISPLAY "LNK-PTNYUINRRK-TOKUTEI-KBN = " 
      *              LNK-PTNYUINRRK-TOKUTEI-KBN
      *     DISPLAY "LNK-PTNYUINRRK-TOKUTEI-NYUIN= "
      *              LNK-PTNYUINRRK-TOKUTEI-NYUIN
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·¡Ê°ÛÆ°Ê¬¤Î²ñ·×ºï½ü¡Ë
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "4"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                 TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA
                                        TO  SPA-SRYKA
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"                 TO  LNK-DAY6-IRAI
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-DAY6-KIJUN
                                            WRK-MAEIDOYMD
           MOVE    "1"                  TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                   TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"              USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN      TO  LNK-SCNYUACCT-EDSRYYMD
      *
      *    Æþ±¡²ñ·×ºï½üÁ°¤Ë²ñ·×¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤ÈÊÝ¸±ÁÈ¹ç¤»¤òÂàÈò
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    "1"                      TO  NACCT-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    "00"                     TO  NACCT-SRYKA
           MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                        TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key16"              TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       MCP-RC       NOT =   ZERO
      *
                   MOVE     MCPDATA-REC   TO   NYUINACCT-REC
      *
      *            ¿©»ö¤òÂàÈò
                   IF      NACCT-ZAISKBKBN     =   "4"
                       MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-SHOKUJI
                       END-IF
                   END-IF
      *            ÊÝ¸±¤òÂàÈò
                   IF      NACCT-ZAISKBKBN     =   "5"
                       MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-HKN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key16"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    Æþ±¡²ñ·×ºîÀ®Á°¤Ë²ñ·×¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤òÂàÈò
           IF  LNK-SCNYUACCT-EDSRYYMD(1:6)  >= "200604"
      *      ¡Ê°ÛÆ°·îÊ¬¡Ë
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(1)
               END-IF
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
      *     ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               IF   IDX-SKJ-TAIHI     =    1
                   IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                       MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
                   ELSE
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
                   END-IF
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
      *          ¿©»ö¤òÂàÈò
                 MOVE    NACCT-SRYYM       TO  WRK-TENROOM-SKJ-SRYYM(2)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ18(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  2
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ19(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  3
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ20(2 IDX-SKJ)
                       END-PERFORM
                 END-EVALUATE
               ELSE
                   MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
               END-IF
      *        Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
           ELSE
      *        ¡Ê°ÛÆ°·îÊ¬¡Ë
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DB-SEL-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ¿©»ö¤òÂàÈò
                   MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                   MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                           TO  IDX-SHOKUJIX
                   MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                           TO  WRK-TENROOM-SKJ-DD(1)
                   PERFORM  VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                            UNTIL     IDX-SKJ   >   31
                        MOVE   NACCT-DAY(IDX-SKJ)
                                       TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *        ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
               IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                   ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                   MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
               ELSE
                   ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DB-SEL-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ¿©»ö¤òÂàÈò
                   MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®Á°¤Ë²ñ·×¥Ç¡¼¥¿¤Î³°Çñ¶èÊ¬¤òÂàÈò
      *    ¡Ê³°Çñ°ÛÆ°·îÊ¬¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                       TO  NACCT-SRYYM
                                           WRK-TENROOM-SKJ-SRYYMX
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        ³°Çñ¤òÂàÈò
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  WRK-TENROOM-SKJ-DD(1)
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    ¡Ê³°Çñ°ÛÆ°Íâ·îÊ¬¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           IF      WRK-TENROOM-SKJ-SRYMM9  =  12
               ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
               MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
           ELSE
               ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
           END-IF
           MOVE    WRK-TENROOM-SKJ-SRYYMX
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        ³°Çñ¤òÂàÈò
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
               PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA            TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     LNK-PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
      ***     IF     PTNYUINRRK-NYUCALKBN   =   "1"
      ***            MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
      ***     ELSE
      ***            MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
      ***     END-IF
      *
           MOVE   LNK-PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·¡Ê°ÛÆ°Á°¤Î²ñ·×ºîÀ®¡Ë
      *
           MOVE    SPACE               TO  IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *     DISPLAY "=============AZU2  IDO-MAE-SAISAKUSEI========"
      *     DISPLAY "PTNYUINRRK-TOKUTEI-KBN = " PTNYUINRRK-TOKUTEI-KBN
      *     DISPLAY "PTNYUINRRK-TOKUTEI-NYUIN= "PTNYUINRRK-TOKUTEI-NYUIN
      *     DISPLAY "WRK-MAEIDOYMD = " WRK-MAEIDOYMD
      *
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ»»Äê»þ¤ËÂà±¡Æü¤ò£Ó£Ð£Á¡Ý°ÛÆ°Æü¤ÈÅ¾ÆþÆü¤Ë¥»¥Ã¥È
      *    °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤âÆ±ÍÍ
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£±
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£²
      *    °ìÎà´¶À÷¾É´µ¼ÔÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    ¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *    ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    Ç¾Â´Ãæ¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡Ê»ØÄêÃÏ°è¡Ë
      *    °¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡Ê»ØÄêÃÏ°è¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£³¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡ÊµßÌ¿µßµÞÆþ±¡ÎÁ¡Ë
      *    µßÌ¿µßµÞÆþ±¡ÎÁ£´¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡Ê¹­ÈÏ°ÏÇ®½ýÆÃÄê½¸Ãæ¼£ÎÅ¡Ë
      *    ¾®»ùÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡Ë
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ê°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍý¡¦¥ê¥Ï¥Ó¥ê»»Äê´µ¼Ô¡Ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ£²¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ¥Ï¥¤¥±¥¢¥æ¥Ë¥Ã¥ÈÆþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡ÊÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍý£±¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡ÊÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍý£²¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19" OR "54" OR "55" OR
                                                "56" OR "57" OR "58" OR
                                                "59" OR "60" OR "91" OR
                                                "92" OR "93" OR "95" OR
                                                "96" OR "AH" OR "AI" OR
                                                "AJ" OR "AK" OR
                                                "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS" OR "AU" OR
                                                "AV"
                    MOVE   WRK-MAEIDOYMD
                                       TO   PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
               END-IF
           END-IF
      *
      *    ´ËÏÂ¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡¢Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±¡¢Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *    ¤ò»»Äê»þ¤ËÂà±¡Æü¤ò£Ó£Ð£Á¡Ý°ÛÆ°Æü¤ÈÅ¾ÆþÆü¤Ë¥»¥Ã¥È
      *    °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤âÆ±ÍÍ
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë¡ÊÊ¿À®£²£¶Ç¯²þÀµ¡Ë
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "2"
              IF    PTNYUINRRK-TOKUTEI-NYUIN  = "09" OR "05" OR "15"
                                             OR "41" OR "42" OR "43"
                                             OR "44" OR "45" OR "46"
                                             OR "47" OR "48"
                    MOVE   WRK-MAEIDOYMD
                                       TO   PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
              END-IF
           END-IF
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
           DISPLAY "################################################"
           DISPLAY "#            IDOKAIKEI-CANCEL                  #"
           DISPLAY "################################################"
           DISPLAY   "WRK-MAEIDOYMD = " WRK-MAEIDOYMD
           DISPLAY   "PTNYUINRRK-TOKUTEI-KBN = " PTNYUINRRK-TOKUTEI-KBN
           DISPLAY   "PTNYUINRRK-TOKUTEI-NYUIN = "
                      PTNYUINRRK-TOKUTEI-NYUIN
      *
      *    ÄÌ»»Æü¿ô¡¢»»Äê³«»ÏÆü¤òµá¤á¤ë
           MOVE    WRK-MAEIDOYMD  TO   PTNYUINRRK-TENNYUYMD
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-IDOKAIKEI-CANCEL-EXT
           END-IF
      *
      *    ²ð¸îÉÂÅï¤«¤é¤ÎµÞÀ­Áý°­¤Ë¤è¤ëÆþ±¡»þ
           IF      PTNYUINRRK-KAIGOKBN    =    "2"
               COMPUTE    WRK-TSUSAN   =   STUSAN-NISSU01  -  1
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD TO  SYOKIKSN-IN-SANTEISTYMD
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    WRK-MAEIDOYMD   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *     DISPLAY "====AZU  TAIIN-CANCEL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    Ê¿À®£²£´Ç¯£³·î£³£±Æü»þÅÀ¤Ç°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¤ò»»Äê
      *    ¤·¤Æ¤¤¤ë¾õÂÖ¤«È½Äê¤¹¤ë¥µ¥Ö¤ò¸Æ¤Ó½Ð¤·
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
           IF      PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               MOVE    PTNYUINRRK-TOKUTEI-KBN
                                      TO  LNK-PTNYUINRRK-TOKUTEI-KBN
               MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                      TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    PTNYUINRRK-BTUNUM
                                      TO  LNK-PTNYUINRRK-BTUNUM
               MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                               TO  LNK-PTNYUINRRK-TOKU-SANTEIYOUKENKBN
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¤Î¾ì¹ç¤Ï´ü´Ö²Ã»»¤¬Â¸ºß¤¹¤ë¤¿¤á¡¢
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÎÅÍÜÉÂ¾²¤ÇÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤òÁê¸ß»»Äê¤¹¤ë¾ì¹ç¤â
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Ë»»Äê¤Ë¤¹¤ë°Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¶Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦°ìÈÌÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¸Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£³£°Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£²
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£³
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£´
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£µ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£¶
      *        ¡¦ÎÅÍÜÉÂÅï¡¦ÃÏ°è°ìÈÌÆþ±¡ÎÁ£±
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Î»»Äê
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         Ê¿À®£²£¸Ç¯²þÀµÂÐ±þ
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         Ê¿À®£³£°Ç¯²þÀµÂÐ±þ
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           ELSE
               IF    ( WRK-MAEIDOYMD     =  CONST-H180701 )
                AND  ( SYS-5001-BTU-SBT        =  "04"  OR "09" )
                   GO  TO  4801-IDOKAIKEI-CANCEL-EXT
               ELSE
                   IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                       MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                       PERFORM   4801-RJNCD-CHG-SEC
                       PERFORM   4801-YAKINGEN-CHG-SEC
                       CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                       SPA-AREA
                       IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                           MOVE    "4023"      TO  SPA-ERRCD
                           GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                       END-IF
                   ELSE
                       MOVE    "4024"      TO  SPA-ERRCD
                       GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                            TO   LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    °åÎÅ´Ñ»¡Æþ±¡¥»¥Ã¥È
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS IDOKAIKEI-CANCEL-CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
      **     DISPLAY  "SPA-NAI-I01-NYUINYMD    = " SPA-NAI-I01-NYUINYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    ÆÃÄêÆþ±¡¤ÇÆþ±¡ÎÁ¤¬ÊÑ¹¹¤Ë¤Ê¤ë¥³¡¼¥É¤ÎÂÐ±þ
      *
           DISPLAY "===  TEST IDOKAIKEI-CANCEL ==="
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   WRK-MAEIDOYMD         TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *-------ÆÃÄêÆþ±¡ÂÐ±þ¤³¤³¤Þ¤Ç-----------------------------------
      *
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM    4801-NYUKSN-GET-SEC
      *
           MOVE       1           TO    FLG-IDOKAIKEI-CANCEL
           MOVE      "1"          TO    LNK-SCNYUACCT-IDOSAKUSEI-KBN
      *
      *    °ÛÆ°Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÉÂ±¡¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN1-SEC
      *
      *    °ÛÆ°Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡Ê¿ÇÎÅ½ê¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN2-SEC
      *
      *    °ÛÆ°Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN3-SEC
      *
      *    °ÛÆ°Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN4-SEC
      *
      *    °ÛÆ°Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN5-SEC
      *
      *    °ÛÆ°Æü¤ËµßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-KYUKYU-KSN6-SEC
      *
      *    °ÛÆ°Æü¤ËÍ­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î
      *    °·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-YUUSHOUIPPAN-KSN-SEC
      *
      *    °ÛÆ°Æü¤ËºßÂð´µ¼Ô»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î°·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-ZAITAKU-KSN1-SEC
      *
      *    °ÛÆ°Æü¤ËºßÂð´µ¼Ô»Ù±çÉÂ¾²½é´ü²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë¤¬»»Äê
      *    ¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯¤·¤Æ¡¢»»Äê¤µ¤ì¤Æ¤¤¤ì¤ÐÆþ±¡ÎÁ²Ã»»¤Î°·¤¤¤Ë´Þ¤á¤ë
           PERFORM   210-ZAITAKU-KSN2-SEC
      *
           MOVE      ZERO         TO   FLG-IDOKAIKEI-CANCEL
      *
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           IF      PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁÅù¤Ï
      *        ÆÃÄêÆþ±¡ÎÁ°·¤¤¤«¤é³°¤¹
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            ÆÃÄêÆþ±¡´ðËÜÎÁ¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤Ï¥Þ¥¹¥¿¤¬
      *            Ê¿À®£²£¶Ç¯£¹·î£³£°Æü¤Þ¤Ç¤Î°Ù¡¢»²¾È»þ¤ÎÆüÉÕ¤òÊÑ¹¹
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    ³°Çñ
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
                  MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           ELSE
                  MOVE   1      TO  LNK-SCNYUACCT-SHOKUJIKBN
           END-IF
           IF     PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"      TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   PTNYUINRRK-SKJKBN-ASA
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   PTNYUINRRK-SKJKBN-HIRU
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   PTNYUINRRK-SKJKBN-YU
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    ¿©Æ²²Ã»»
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           IF     WRK-TAIHI-HKN    NOT =   ZERO
                  MOVE   WRK-TAIHI-HKN
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           ELSE
                  MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
                  MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           END-IF
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨¡ÊÊ¿À®£²£¶Ç¯²þÄê¡Ë
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡´ðËÜÎÁ¤Î¥³¡¼¥ÉºÙÊ¬²½ÃÖ¤­´¹¤¨
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            ÆÃÄêµ¡Ç½ÉÂ±¡¤ÏÉÂÅïÀßÄê¤µ¤ì¤¿Æþ±¡¥³¡¼¥É¤«¤éÈ½Äê
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        ÆÃÊÌÆþ±¡´ðËÜÎÁ
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¸å¤ËÂàÈò¥Ç¡¼¥¿¤Î¿©»ö¶èÊ¬¤òÉü¸µ
      *   ¡Ê°ÛÆ°·îÊ¬¡Ë
           IF  (WRK-TENROOM-SKJ-SRYYM(1) >= "200604")
            IF (WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE)
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                 MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                             TO  NACCT-SRYYM
      *
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
                     MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *              ¿©»ö¤òÉü¸µ
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ18(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ19(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ20(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                     END-EVALUATE
      *              ºÞ²ó¿ôºÆ¥»¥Ã¥È
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM   
                 END-IF
                 MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 PERFORM 910-DBUPDATE-SEC
      *          Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 910-DBCLOSECURSOR-SEC
              END-PERFORM
            END-IF
           END-IF
      *   ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
           IF  (WRK-TENROOM-SKJ-SRYYM(2) >= "200604")
            IF (WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE)
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                ¿©»ö¤òÉü¸µ
                       EVALUATE  IDX-SKJ-TAIHI
                           WHEN  1
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ18(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  2
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ19(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  3
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ20(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                       END-EVALUATE
      *                ºÞ²ó¿ôºÆ¥»¥Ã¥È
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
      *            Æþ±¡²ñ·×¥Þ¥¹¥¿¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM 910-DBCLOSECURSOR-SEC
              END-PERFORM
            END-IF
           END-IF
      *   ¡Ê°ÛÆ°·îÊ¬¡Ë
           IF  (WRK-TENROOM-SKJ-SRYYM(1) < "200603")   AND
               (WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE)
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DB-SEL-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ¿©»ö¤òÉü¸µ
                   MOVE    WRK-TENROOM-SKJ-DD(1)
                                       TO  IDX-SHOKUJIX
                   PERFORM  VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                            UNTIL     IDX-SKJ   >   31
                       MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *   ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
           IF  (WRK-TENROOM-SKJ-SRYYM(2) < "200603")   AND
               (WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE)
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DB-SEL-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ¿©»ö¤òÉü¸µ
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¸å¤ËÂàÈò¥Ç¡¼¥¿¤Î³°Çñ¶èÊ¬¤òÉü¸µ
      *    ¡Ê³°Çñ°ÛÆ°·îÊ¬¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        ³°Çñ¤òÉü¸µ
               MOVE    WRK-TENROOM-SKJ-DD(1)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
                                       TO  NACCT-DAY(IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    ¡Ê°ÛÆ°Íâ·îÊ¬¡Ë
           IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM   910-DB-SEL-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            ³°Çñ¤òÉü¸µ
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *     DISPLAY "=============AZU3========================"
      *     DISPLAY "IPN-NYUKAIKEI-SAKUSEI-FLG = "
      *              IPN-NYUKAIKEI-SAKUSEI-FLG
      *     DISPLAY "IPN-SANTEISTYMD = " IPN-SANTEISTYMD
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
      **         MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    SPACE            TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD  TO  WRK-SANTEISTYMD
      **        MOVE    IPN-SANTEISTYMD  TO  SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD  TO  LNK-PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-IDOKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¼è¾Ã¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-NYUKAIKEI-ERASE-SEC         SECTION.
      *
           DISPLAY "4801-NYUKAIKEI-ERASE-SEC"
           DISPLAY "PTNYUINRRK-PTID      = " LNK-PTNYUINRRK-PTID
           DISPLAY "PTNYUINRRK-NYUINYMD  = " LNK-PTNYUINRRK-NYUINYMD
           DISPLAY "PTNYUINRRK-TAIINYMD  = " LNK-PTNYUINRRK-TAIINYMD
           DISPLAY "PTNYUINRRK-HKNCOMBINUM  = "
                                         LNK-PTNYUINRRK-HKNCOMBINUM
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE            ORCSNYUACCT-AREA
           EVALUATE   SODRNACCT-ORDERID
               WHEN   "0107"
                      MOVE   "5"       TO   LNK-SCNYUACCT-SYORI-KBN
               WHEN   "0108"
                      MOVE   "6"       TO   LNK-SCNYUACCT-SYORI-KBN
           END-EVALUATE
           DISPLAY  "LNK-SCNYUACCT-SYORI-KBN = " 
                     LNK-SCNYUACCT-SYORI-KBN
           MOVE     LNK-PTNYUINRRK-HOSPNUM  TO  SPA-HOSPNUM
           MOVE     "1"                    TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                        TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9      TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     LNK-PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
           MOVE   LNK-PTNYUINRRK-BTUNUM TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-NYUKAIKEI-ERASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âà±¡¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-TAIINKAIKEI-EDIT-SEC         SECTION.
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "2"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-HOSPNUM  TO  SPA-HOSPNUM
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
      *     MOVE   SPA-SRYKA           TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
           MOVE   LNK-PTNYUINRRK-BTUNUM
                                       TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
      *     MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-TAIINKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-GAIHAKU-SEC         SECTION.
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TAIINYMD =   "99999999"
               PERFORM    4801-GAIHAKU-1-SEC
           ELSE
               PERFORM    4801-GAIHAKU-2-SEC
           END-IF
      *
           .
       4801-GAIHAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÆþ±¡Ãæ¡Ë
      *****************************************************************
       4801-GAIHAKU-1-SEC         SECTION.
      *
      *    Æþ±¡Ãæ´µ¼Ô¤Î³°Çñ¥Ç¡¼¥¿¹¹¿·¤Ê¤Î¤Ç¡¢Æþ±¡²ñ·×¤¬Â¸ºß¤¹¤ë²ñ·×¥Ç¡¼¥¿¤Ë
      *    ¤Ä¤¤¤ÆÁ´¤Æ¹¹¿·¤ò¹Ô¤¦
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM    =   SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            ·îËöÆü¤ò»»Äê
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            ³°Çñ¶èÊ¬¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
      *                0201:³°Çñ
      *                0202:¼£ÎÅ¤Î°Ù¤Î³°Çñ
      *                0203:ÁªÄêÆþ±¡Ãæ¤Î³°Çñ
      *                0205:µ¢±¡
                       EVALUATE   SODRNACCT-ORDERID(3:2)
                           WHEN   "01"
                             MOVE   1    TO   NACCT-DAY(IDX)
                           WHEN   "02"
                             MOVE   2    TO   NACCT-DAY(IDX)
                           WHEN   "03"
                             MOVE   3    TO   NACCT-DAY(IDX)
                           WHEN   "05"
                             MOVE   0    TO   NACCT-DAY(IDX)
                       END-EVALUATE
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key17"             TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "2"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GAIHAKU-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÂà±¡ºÑ¡Ë
      *****************************************************************
       4801-GAIHAKU-2-SEC         SECTION.
      *
      *    Âà±¡ºÑ´µ¼Ô¤Î³°Çñ¥Ç¡¼¥¿¹¹¿·¤Ê¤Î¤Ç¡¢Âà±¡Æü¤Þ¤Ç¤Î²ñ·×¥Ç¡¼¥¿¤Ë¤Ä¤¤¤Æ
      *    ¹¹¿·¤ò¹Ô¤¦
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)  OR
                      (NACCT-SRYYM    > LNK-PTNYUINRRK-TAIINYMD(1:6))
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM      =  SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            ·îËöÆü¤ò»»Äê
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            Âà±¡·î¤Ë¤Ä¤¤¤Æ¤ÏÂà±¡Æü¤ò¥»¥Ã¥È
                   IF   NACCT-SRYYM    =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                       MOVE    LNK-PTNYUINRRK-TAIINYMD
                                             TO    WRK-GETSUMATSU-YMD
                   END-IF
      *            ³°Çñ¶èÊ¬¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
      *                0201:³°Çñ
      *                0202:¼£ÎÅ¤Î°Ù¤Î³°Çñ
      *                0203:ÁªÄêÆþ±¡Ãæ¤Î³°Çñ
      *                0205:µ¢±¡
                       EVALUATE   SODRNACCT-ORDERID(3:2)
                           WHEN   "01"
                             MOVE   1    TO   NACCT-DAY(IDX)
                           WHEN   "02"
                             MOVE   2    TO   NACCT-DAY(IDX)
                           WHEN   "03"
                             MOVE   3    TO   NACCT-DAY(IDX)
                           WHEN   "05"
                             MOVE   0    TO   NACCT-DAY(IDX)
                       END-EVALUATE
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE     "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "2"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GAIHAKU-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡Êµ¯»»Æü¡Ý½ªÎ»Æü¤Î´ü´Ö»ØÄê¡Ë
      *****************************************************************
       4801-GAIHAKU-KIKAN-SEC         SECTION.
      *
      *    µ¯»»Æü¡Ý½ªÎ»Æü¤Î²ñ·×¥Ç¡¼¥¿¤Ë¤Ä¤¤¤Æ¹¹¿·¤ò¹Ô¤¦
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               MOVE    SODRNACCT-ENDYMD    TO  WRK-ENDYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)  OR
                                 (NACCT-SRYYM >
                                     LNK-PTNYUINRRK-TAIINYMD(1:6))  OR
                                 (NACCT-SRYYM >   WRK-ENDYMD(1:6))
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM      =  SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            ½ªÎ»Æü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM      =  SODRNACCT-ENDYMD(1:6)
                      MOVE   SODRNACCT-ENDYMD TO   WRK-GETSUMATSU-YMD
      *               Âà±¡·î¤Ë¤Ä¤¤¤Æ¤ÏÂà±¡Æü¤¬¾å¸Â
                      IF   NACCT-SRYYM   =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                          IF   WRK-GETSUMATSU-YMD  > 
                                              LNK-PTNYUINRRK-TAIINYMD
                             MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO    WRK-GETSUMATSU-YMD
                          END-IF
                      END-IF
                   ELSE
      *               ·îËöÆü¤ò»»Äê
                      INITIALIZE               STS-AREA-DAY
                      INITIALIZE               LNK-DAY7-AREA
                      MOVE    "71"             TO  LNK-DAY7-IRAI
                      MOVE   NACCT-SRYYM       TO  LNK-DAY7-YM
                      CALL  "ORCSDAY"          USING STS-AREA-DAY
                                                     LNK-DAY7-AREA
                      MOVE   LNK-DAY7-KOYOMI   TO    WRK-GETSUMATSU-YMD
      *               Âà±¡·î¤Ë¤Ä¤¤¤Æ¤ÏÂà±¡Æü¤¬¾å¸Â
                      IF   NACCT-SRYYM   =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                          MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO    WRK-GETSUMATSU-YMD
                      END-IF
                   END-IF
      *
      *            ³°Çñ¶èÊ¬¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
      *                0200:¼è¤ê¾Ã¤·
      *                0201:³°Çñ
      *                0202:¼£ÎÅ¤Î°Ù¤Î³°Çñ
      *                0203:ÁªÄêÆþ±¡Ãæ¤Î³°Çñ
      *                0205:µ¢±¡
      *                0207:Â¾°åÎÅµ¡´Ø£·£°¡ó¸º
      *                0208:Â¾°åÎÅµ¡´Ø£³£°¡ó¸º
      *                0209:Â¾°åÎÅµ¡´Ø£µ£µ¡ó¸º
      *                0210:Â¾°åÎÅµ¡´Ø£±£µ¡ó¸º
      *                0211:ÆÃÄê»þ´ÖÂà±¡¸º»»
      *                0212:ÆÃÄêÍËÆüÆþÂà±¡¸º»»
      *                0213:ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø£±£°¡ó¸º
      *                0214:Â¾°åÎÅµ¡´Ø£³£µ¡ó¸º
      *                0215:Â¾°åÎÅµ¡´Ø£±£µ¡ó¸º
      *                0216:Â¾°åÎÅµ¡´Ø£µ¡ó¸º
      *                0217:ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø£µ¡ó¸º
                       EVALUATE   SODRNACCT-ORDERID(3:2)
                           WHEN   "00"
                             MOVE   0    TO   NACCT-DAY(IDX)
                           WHEN   "01"
                             MOVE   1    TO   NACCT-DAY(IDX)
                           WHEN   "02"
                             MOVE   2    TO   NACCT-DAY(IDX)
                           WHEN   "03"
                             MOVE   3    TO   NACCT-DAY(IDX)
                           WHEN   "05"
                             MOVE   0    TO   NACCT-DAY(IDX)
                           WHEN   "07"
                             MOVE   4    TO   NACCT-DAY(IDX)
                           WHEN   "08"
                             MOVE   5    TO   NACCT-DAY(IDX)
                           WHEN   "09"
                             MOVE   6    TO   NACCT-DAY(IDX)
                           WHEN   "10"
                             MOVE   7    TO   NACCT-DAY(IDX)
                           WHEN   "11"
                             MOVE   8    TO   NACCT-DAY(IDX)
                           WHEN   "12"
                             MOVE   9    TO   NACCT-DAY(IDX)
                           WHEN   "13"
                             MOVE  13    TO   NACCT-DAY(IDX)
                           WHEN   "14"
                             MOVE  15    TO   NACCT-DAY(IDX)
                           WHEN   "15"
                             MOVE  16    TO   NACCT-DAY(IDX)
                           WHEN   "16"
                             MOVE  14    TO   NACCT-DAY(IDX)
                           WHEN   "17"
                             MOVE  17    TO   NACCT-DAY(IDX)
                       END-EVALUATE
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE     "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "2"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GAIHAKU-KIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿ÇÅù¤Î¸º»»¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-GENSAN-SEC         SECTION.
      *
      *    ¼è¤ê¾Ã¤·¤È¸º»»¤Ê¤Î¤Çµ¯»»Æü¤Î¤ßÆþ±¡²ñ·×¤ò¹¹¿·¤¹¤ë
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
      *        ³«»ÏÆü¤Î¥»¥Ã¥È
               MOVE    WRK-STARTYMD-D      TO  IDX
      *        ³°Çñ¶èÊ¬¤Î¥»¥Ã¥È
      *        0200:¼è¤ê¾Ã¤·
      *        0207:Â¾°åÎÅµ¡´Ø£·£°¡ó¸º
      *        0208:Â¾°åÎÅµ¡´Ø£³£°¡ó¸º
      *        0209:Â¾°åÎÅµ¡´Ø£µ£µ¡ó¸º
      *        0210:Â¾°åÎÅµ¡´Ø£±£µ¡ó¸º
      *        0211:ÆÃÄê»þ´ÖÂà±¡¸º»»
      *        0212:ÆÃÄêÍËÆüÆþÂà±¡¸º»»
      *        0213:ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø£±£°¡ó¸º
      *        0214:Â¾°åÎÅµ¡´Ø£³£µ¡ó¸º
      *        0215:Â¾°åÎÅµ¡´Ø£±£µ¡ó¸º
      *        0216:Â¾°åÎÅµ¡´Ø£µ¡ó¸º
      *        0217:ÆÃÄêÍËÆüÆþÂà±¡¸º»»¡ÜÂ¾°åÎÅµ¡´Ø£µ¡ó¸º
               EVALUATE   SODRNACCT-ORDERID(3:2)
                   WHEN   "00"
                     MOVE   0    TO   NACCT-DAY(IDX)
                   WHEN   "07"
                     MOVE   4    TO   NACCT-DAY(IDX)
                   WHEN   "08"
                     MOVE   5    TO   NACCT-DAY(IDX)
                   WHEN   "09"
                     MOVE   6    TO   NACCT-DAY(IDX)
                   WHEN   "10"
                     MOVE   7    TO   NACCT-DAY(IDX)
                   WHEN   "11"
                     MOVE   8    TO   NACCT-DAY(IDX)
                   WHEN   "12"
                     MOVE   9    TO   NACCT-DAY(IDX)
                   WHEN   "13"
                     MOVE  13    TO   NACCT-DAY(IDX)
                   WHEN   "14"
                     MOVE  15    TO   NACCT-DAY(IDX)
                   WHEN   "15"
                     MOVE  16    TO   NACCT-DAY(IDX)
                   WHEN   "16"
                     MOVE  14    TO   NACCT-DAY(IDX)
                   WHEN   "17"
                     MOVE  17    TO   NACCT-DAY(IDX)
               END-EVALUATE
      *
      *        ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
               MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *        ¹¹¿·Ç¯·îÆü
               MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *        ¹¹¿·»þ´Ö
               MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
               MOVE    NYUINACCT-REC   TO   MCPDATA-REC
               MOVE    "DBUPDATE"      TO   MCP-FUNC
               MOVE    "tbl_nyuinacct" TO   MCP-TABLE
               MOVE    "key"           TO   MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GENSAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼¼ÎÁº¹³Û¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý
      *****************************************************************
       4801-ROOMSAGAKU-SEC         SECTION.
      *
      *    ¼¼ÎÁº¹³Û¤Î¥»¥Ã¥È
           IF      LNK-PTNYUINRRK-TAIINYMD =   "99999999"
               PERFORM    4801-ROOMSAGAKU-1-SEC
           ELSE
               PERFORM    4801-ROOMSAGAKU-2-SEC
           END-IF
      *
           .
       4801-ROOMSAGAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼¼ÎÁº¹³Û¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÆþ±¡Ãæ¡Ë
      *****************************************************************
       4801-ROOMSAGAKU-1-SEC         SECTION.
      *
      *    Æþ±¡Ãæ´µ¼Ô¤Î¼¼ÎÁº¹³Û¥Ç¡¼¥¿¹¹¿·¤Ê¤Î¤Ç¡¢Æþ±¡²ñ·×¤¬Â¸ºß¤¹¤ë²ñ·×¥Ç¡¼¥¿¤Ë
      *    ¤Ä¤¤¤ÆÁ´¤Æ¹¹¿·¤ò¹Ô¤¦
      *
      *    ¼¼ÎÁº¹³Û¤Î¥»¥Ã¥È
           DISPLAY "4801-ROOMSAGAKU-1-SEC"
           DISPLAY "SODRNACCT-STARTYMD = " SODRNACCT-STARTYMD
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "3"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   DISPLAY "NACCT-SRYYM = " NACCT-SRYYM
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM    =   SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *             DISPLAY "START-IDX = " START-IDX
      *            ·îËöÆü¤ò»»Äê
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            ¼¼ÎÁº¹³Û¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
      *                 DISPLAY "IDX = " IDX
                       IF   SODRNACCT-ORDERID(3:2)   =   "01"
                            MOVE    LNK-PTNYUINRRK-RM-SAGAKU
                                             TO  WRK-ROOMSAGAKU
                            MOVE    WRK-ROOMSAGAKU
                                             TO   NACCT-DAY(IDX)
                       END-IF
                       IF   SODRNACCT-ORDERID(3:2)   =   "02"
                            MOVE    ZERO     TO   NACCT-DAY(IDX)
                       END-IF
                   END-PERFORM
      *
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                            UNTIL     IDX   >   31
                       IF   NACCT-DAY(IDX)   NOT =  0
                           ADD   1          TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key17"             TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "3"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-ROOMSAGAKU-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼¼ÎÁº¹³Û¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÂà±¡ºÑ¡Ë
      *****************************************************************
       4801-ROOMSAGAKU-2-SEC         SECTION.
           DISPLAY "4801-ROOMSAGAKU-2-SEC"
      *
      *    Âà±¡ºÑ´µ¼Ô¤Î¼¼ÎÁº¹³Û¥Ç¡¼¥¿¹¹¿·¤Ê¤Î¤Ç¡¢Âà±¡Æü¤Þ¤Ç¤Î²ñ·×¥Ç¡¼¥¿¤Ë¤Ä¤¤¤Æ
      *    ¹¹¿·¤ò¹Ô¤¦
      *
      *    ³°Çñ¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "3"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)  OR
                      (NACCT-SRYYM    > LNK-PTNYUINRRK-TAIINYMD(1:6))
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM      =  SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            ·îËöÆü¤ò»»Äê
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            Âà±¡·î¤Ë¤Ä¤¤¤Æ¤ÏÂà±¡Æü¤ò¥»¥Ã¥È
                   IF   NACCT-SRYYM    =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                       MOVE    LNK-PTNYUINRRK-TAIINYMD
                                             TO    WRK-GETSUMATSU-YMD
                   END-IF
      *            ¼¼ÎÁº¹³Û¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
                       IF   SODRNACCT-ORDERID(3:2)   =   "01"
                            MOVE    LNK-PTNYUINRRK-RM-SAGAKU
                                             TO  WRK-ROOMSAGAKU
                            MOVE    WRK-ROOMSAGAKU
                                             TO   NACCT-DAY(IDX)
                       END-IF
                       IF   SODRNACCT-ORDERID(3:2)   =   "02"
                            MOVE    ZERO     TO   NACCT-DAY(IDX)
                       END-IF
                   END-PERFORM
      *
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                            UNTIL     IDX   >   31
                       IF   NACCT-DAY(IDX)   NOT =  0
                           ADD   1          TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE     "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "3"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-ROOMSAGAKU-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼¼ÎÁº¹³Û¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡Êµ¯»»Æü¡Ý½ªÎ»Æü¤Î´ü´Ö»ØÄê¡Ë
      *****************************************************************
       4801-ROOMSAGAKU-KIKAN-SEC         SECTION.
      *
      *    µ¯»»Æü¡Ý½ªÎ»Æü¤Î²ñ·×¥Ç¡¼¥¿¤Ë¤Ä¤¤¤Æ¹¹¿·¤ò¹Ô¤¦
      *
      *    ¼¼ÎÁº¹³Û¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "3"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               MOVE    SODRNACCT-ENDYMD    TO  WRK-ENDYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)  OR
                                 (NACCT-SRYYM >
                                     LNK-PTNYUINRRK-TAIINYMD(1:6))  OR
                                 (NACCT-SRYYM >   WRK-ENDYMD(1:6))
      *            ³«»ÏÆü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM      =  SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            ½ªÎ»Æü¤Î¥»¥Ã¥È
                   IF      NACCT-SRYYM      =  SODRNACCT-ENDYMD(1:6)
                      MOVE   SODRNACCT-ENDYMD TO   WRK-GETSUMATSU-YMD
      *               Âà±¡·î¤Ë¤Ä¤¤¤Æ¤ÏÂà±¡Æü¤¬¾å¸Â
                      IF   NACCT-SRYYM   =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                          IF   WRK-GETSUMATSU-YMD  > 
                                              LNK-PTNYUINRRK-TAIINYMD
                             MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO    WRK-GETSUMATSU-YMD
                          END-IF
                      END-IF
                   ELSE
      *               ·îËöÆü¤ò»»Äê
                      INITIALIZE               STS-AREA-DAY
                      INITIALIZE               LNK-DAY7-AREA
                      MOVE    "71"             TO  LNK-DAY7-IRAI
                      MOVE   NACCT-SRYYM       TO  LNK-DAY7-YM
                      CALL  "ORCSDAY"          USING STS-AREA-DAY
                                                     LNK-DAY7-AREA
                      MOVE   LNK-DAY7-KOYOMI   TO    WRK-GETSUMATSU-YMD
      *               Âà±¡·î¤Ë¤Ä¤¤¤Æ¤ÏÂà±¡Æü¤¬¾å¸Â
                      IF   NACCT-SRYYM   =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                          MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO    WRK-GETSUMATSU-YMD
                      END-IF
                   END-IF
      *
      *            ¼¼ÎÁº¹³Û¤Î¥»¥Ã¥È
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
                       IF   SODRNACCT-ORDERID(3:2)   =   "01"
                            MOVE    LNK-PTNYUINRRK-RM-SAGAKU
                                             TO  WRK-ROOMSAGAKU
                            MOVE    WRK-ROOMSAGAKU
                                             TO   NACCT-DAY(IDX)
                       END-IF
                       IF   SODRNACCT-ORDERID(3:2)   =   "02"
                            MOVE    ZERO     TO   NACCT-DAY(IDX)
                       END-IF
                   END-PERFORM
      *
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                            UNTIL     IDX   >   31
                       IF   NACCT-DAY(IDX)   NOT =  0
                           ADD   1          TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE     "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            ¼¡·î¤Î²ñ·×ÆÉ¤ß¹þ¤ß
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "3"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-ROOMSAGAKU-KIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÊ¿À®£±£¸Ç¯£³·î°ÊÁ°¡Ë
      *****************************************************************
       4801-SHOKUJI-SEC         SECTION.
      *
      *    ¿©»ö¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key28"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key28"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               IF        FLG-NYUINACCT     =   ZERO
                 IF   NACCT-SRYCDTOTAL  =  99999913
      *            ¿©»ö¶èÊ¬¤Î¥»¥Ã¥È
                   MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
                   IF   SODRNACCT-SKJTIME   =  "01"
                     EVALUATE   SODRNACCT-ORDERID(3:2)
                       WHEN   "01"
                           MOVE  1  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "02"
                           MOVE  2  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "03"
                           MOVE  3  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "04"
                           MOVE   4     TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "05"
                           MOVE  0  TO   NACCT-DAY(WRK-STARTYMD-D)
                     END-EVALUATE
                   ELSE
                     EVALUATE   SODRNACCT-ORDERID(3:2)
                       WHEN   "01"
                           IF     NACCT-DAY(WRK-STARTYMD-D)  =  0
                               MOVE  1  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "02"
                           IF     NACCT-DAY(WRK-STARTYMD-D) =  0    OR
                                                               1    OR
                                                               3
                               MOVE  2  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "03"
                           IF     NACCT-DAY(WRK-STARTYMD-D) =  0  OR 1
                               MOVE  3  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "04"
                           MOVE   4     TO   NACCT-DAY(WRK-STARTYMD-D)
                     END-EVALUATE
                   END-IF
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *            ¿©Æ²²Ã»»¤ÎÊÔ½¸ÍÑ¤ËÂàÈò
                   MOVE    NACCT-DAY(WRK-STARTYMD-D)  TO WRK-SHOKUDOU
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 END-IF
               END-IF
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key28"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *-------------------¿©Æ²²Ã»»¤³¤³¤«¤é----------------------------
      *    ¿©»ö¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL  =  197000570
      *            ¿©Æ²²Ã»»¤Î¥»¥Ã¥È
                   MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
                   IF    WRK-SHOKUDOU    NOT =  ZERO
                         MOVE   1   TO   NACCT-DAY(WRK-STARTYMD-D)
                   ELSE
                         MOVE   0   TO   NACCT-DAY(WRK-STARTYMD-D)
                   END-IF
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 END-IF
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key17"             TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *-------------------¿©Æ²²Ã»»¤³¤³¤Þ¤Ç----------------------------
           .
       4801-SHOKUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       4801-SHOKUJI-200604-SEC         SECTION.
      *
      *    ¿©»ö¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   SODRNACCT-SKJTIME
                   WHEN    "01"
                       MOVE   "key30"      TO  MCP-PATHNAME
                   WHEN    "02"
                       MOVE   "key31"      TO  MCP-PATHNAME
                   WHEN    "03"
                       MOVE   "key32"      TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM   910-NYUINACCT-READ-SEC
               IF        FLG-NYUINACCT     =   ZERO
      *            ¿©»ö¶èÊ¬¤Î¥»¥Ã¥È
      *             MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
      *             EVALUATE   SODRNACCT-ORDERID(3:2)
      *                 WHEN   "01"
      *                     MOVE   1     TO   NACCT-DAY(WRK-STARTYMD-D)
      *                 WHEN   "02"
      *                     MOVE   2     TO   NACCT-DAY(WRK-STARTYMD-D)
      *                 WHEN   "05"
      *                     MOVE   0     TO   NACCT-DAY(WRK-STARTYMD-D)
      *             END-EVALUATE
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   SODRNACCT-DAY(IDX-SKJ)   =   1
                           EVALUATE   SODRNACCT-ORDERID(3:2)
                               WHEN   "01"
                                   MOVE   1     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "02"
                                   MOVE   2     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "05"
                                   MOVE   0     TO   NACCT-DAY(IDX-SKJ)
                           END-EVALUATE
                       END-IF
                   END-PERFORM
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
      *            ¿©Æ²²Ã»»¤ÎÊÔ½¸ÍÑ¤ËÂàÈò
      *            MOVE    NACCT-DAY(WRK-STARTYMD-D)  TO WRK-SHOKUDOU
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
               END-IF
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *-------------------¿©Æ²²Ã»»¤³¤³¤«¤é----------------------------
      *    ¿©»ö¤Î¥»¥Ã¥È
      *     INITIALIZE                          NYUINACCT-REC
      *     MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
      *     MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
      *     MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
      *     MOVE    "4"                     TO  NACCT-ZAISKBKBN
      *     MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE    "key17"                 TO  MCP-PATHNAME
      *     PERFORM 910-NYUINACCT-SELECT-SEC
      *     IF      FLG-NYUINACCT           =   ZERO
      *         MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
      *         MOVE     "key17"            TO  MCP-PATHNAME
      *         PERFORM   910-NYUINACCT-READ-SEC
      *         PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *           IF   NACCT-SRYCDTOTAL  =  197000570
      *            ¿©Æ²²Ã»»¤Î¥»¥Ã¥È
      *             MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
      *             IF    WRK-SHOKUDOU    NOT =  ZERO
      *                   MOVE   1   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             ELSE
      *                   MOVE   0   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             END-IF
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
      *             MOVE  ZERO           TO  NACCT-ZAIKAISU
      *             PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
      *                      UNTIL     IDX-SKJ   >   31
      *                 IF   NACCT-DAY(IDX-SKJ)   NOT =  0
      *                     ADD   1        TO  NACCT-ZAIKAISU
      *                 END-IF
      *             END-PERFORM   
      **
      *             MOVE    NYUINACCT-REC   TO   MCPDATA-REC
      *             MOVE    "DBUPDATE"      TO   MCP-FUNC
      *             MOVE    "tbl_nyuinacct" TO   MCP-TABLE
      *             MOVE    "key"           TO   MCP-PATHNAME
      *             CALL    "ORCDBMAIN"         USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
      *                                         SPA-AREA
      *
      *           END-IF
      *           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
      *           MOVE    "key17"             TO  MCP-PATHNAME
      *           PERFORM 910-NYUINACCT-READ-SEC
      *         END-PERFORM
      *     END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
      *     MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE     "key17"                 TO  MCP-PATHNAME
      *     PERFORM   910-DBCLOSECURSOR-SEC
      *-------------------¿©Æ²²Ã»»¤³¤³¤Þ¤Ç----------------------------
           .
       4801-SHOKUJI-200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¡¡²ñ·×¥Æ¡¼¥Ö¥ëÊÔ½¸½èÍý¡ÊÊ¿À®£²£¸Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       4801-SHOKUJI-201604-SEC         SECTION.
      *
           DISPLAY "4801-SHOKUJI-201604-SEC"
      *
      *    ¿©»ö¤Î¥»¥Ã¥È
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   SODRNACCT-SKJTIME
                   WHEN    "01"
                       MOVE   "key30"      TO  MCP-PATHNAME
                   WHEN    "02"
                       MOVE   "key31"      TO  MCP-PATHNAME
                   WHEN    "03"
                       MOVE   "key32"      TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM   910-NYUINACCT-READ-SEC
               IF        FLG-NYUINACCT     =   ZERO
      *            ¿©»ö¶èÊ¬¤Î¥»¥Ã¥È
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   SODRNACCT-DAY(IDX-SKJ)   =   1
                           EVALUATE   SODRNACCT-ORDERID(3:2)
                               WHEN   "01"
                                   MOVE   1     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "02"
                                   MOVE   2     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "05"
                                   MOVE   0     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "07"
                                   MOVE   3     TO   NACCT-DAY(IDX-SKJ)
                           END-EVALUATE
                       END-IF
                   END-PERFORM
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM
      *
      *            ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                   MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *            ¹¹¿·Ç¯·îÆü
                   MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *            ¹¹¿·»þ´Ö
                   MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
      *            ¿©Æ²²Ã»»¤ÎÊÔ½¸ÍÑ¤ËÂàÈò
      *            MOVE    NACCT-DAY(WRK-STARTYMD-D)  TO WRK-SHOKUDOU
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
               END-IF
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *-------------------¿©Æ²²Ã»»¤³¤³¤«¤é----------------------------
      *    ¿©»ö¤Î¥»¥Ã¥È
      *     INITIALIZE                          NYUINACCT-REC
      *     MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
      *     MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
      *     MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
      *     MOVE    "4"                     TO  NACCT-ZAISKBKBN
      *     MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE    "key17"                 TO  MCP-PATHNAME
      *     PERFORM 910-NYUINACCT-SELECT-SEC
      *     IF      FLG-NYUINACCT           =   ZERO
      *         MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
      *         MOVE     "key17"            TO  MCP-PATHNAME
      *         PERFORM   910-NYUINACCT-READ-SEC
      *         PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *           IF   NACCT-SRYCDTOTAL  =  197000570
      *            ¿©Æ²²Ã»»¤Î¥»¥Ã¥È
      *             MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
      *             IF    WRK-SHOKUDOU    NOT =  ZERO
      *                   MOVE   1   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             ELSE
      *                   MOVE   0   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             END-IF
      *            ºÞ²ó¿ôºÆ¥»¥Ã¥È
      *             MOVE  ZERO           TO  NACCT-ZAIKAISU
      *             PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
      *                      UNTIL     IDX-SKJ   >   31
      *                 IF   NACCT-DAY(IDX-SKJ)   NOT =  0
      *                     ADD   1        TO  NACCT-ZAIKAISU
      *                 END-IF
      *             END-PERFORM   
      **
      *             MOVE    NYUINACCT-REC   TO   MCPDATA-REC
      *             MOVE    "DBUPDATE"      TO   MCP-FUNC
      *             MOVE    "tbl_nyuinacct" TO   MCP-TABLE
      *             MOVE    "key"           TO   MCP-PATHNAME
      *             CALL    "ORCDBMAIN"         USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
      *                                         SPA-AREA
      *
      *           END-IF
      *           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
      *           MOVE    "key17"             TO  MCP-PATHNAME
      *           PERFORM 910-NYUINACCT-READ-SEC
      *         END-PERFORM
      *     END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
      *     MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE     "key17"                 TO  MCP-PATHNAME
      *     PERFORM   910-DBCLOSECURSOR-SEC
      *-------------------¿©Æ²²Ã»»¤³¤³¤Þ¤Ç----------------------------
      *
      *    ¿©»ö·ÑÂ³¶èÊ¬¤Ë¤è¤ë¼¡·î°Ê¹ß¤Î¿©»ö¥»¥Ã¥È¡ÊÂà±¡·î¤ÏÂà±¡Æü¤Þ¤Ç¡Ë
           IF    SODRNACCT-SAME-AFTER      =   "1"
             INITIALIZE                          NYUINACCT-REC
             MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
             MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
             MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
             MOVE    "4"                     TO  NACCT-ZAISKBKBN
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             EVALUATE   SODRNACCT-SKJTIME
                WHEN    "01"
                    MOVE    "key63"          TO  MCP-PATHNAME
                WHEN    "02"
                    MOVE    "key64"          TO  MCP-PATHNAME
                WHEN    "03"
                    MOVE    "key65"          TO  MCP-PATHNAME
             END-EVALUATE
             PERFORM 910-NYUINACCT-SELECT-SEC
             IF      FLG-NYUINACCT           =   ZERO
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 EVALUATE   SODRNACCT-SKJTIME
                     WHEN    "01"
                         MOVE   "key63"      TO  MCP-PATHNAME
                     WHEN    "02"
                         MOVE   "key64"      TO  MCP-PATHNAME
                     WHEN    "03"
                         MOVE   "key65"      TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM   910-NYUINACCT-READ-SEC
                 PERFORM    UNTIL   FLG-NYUINACCT   =   1
                     IF   LNK-PTNYUINRRK-TAIINYMD(1:6)  =  NACCT-SRYYM
                       MOVE   LNK-PTNYUINRRK-TAIINYMD
                                                 TO   WRK-GETSUMATSU-YMD
                     ELSE
      *                ·îËöÆü¤Î»»Äê
                       INITIALIZE                STS-AREA-DAY
                       INITIALIZE                LNK-DAY7-AREA
                       MOVE    "71"              TO  LNK-DAY7-IRAI
                       MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                       CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                       LNK-DAY7-AREA
                       MOVE   LNK-DAY7-KOYOMI    TO   WRK-GETSUMATSU-YMD
                     END-IF
      *              ¿©»ö¶èÊ¬¤Î¥»¥Ã¥È
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   WRK-GETSUMATSU-D
                        EVALUATE   SODRNACCT-ORDERID(3:2)
                            WHEN   "01"
                                MOVE   1     TO   NACCT-DAY(IDX-SKJ)
                            WHEN   "02"
                                MOVE   2     TO   NACCT-DAY(IDX-SKJ)
                            WHEN   "05"
                                MOVE   0     TO   NACCT-DAY(IDX-SKJ)
                            WHEN   "07"
                                MOVE   3     TO   NACCT-DAY(IDX-SKJ)
                        END-EVALUATE
                     END-PERFORM
      *              ºÞ²ó¿ôºÆ¥»¥Ã¥È
                     MOVE  ZERO           TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM
      *
      *              ¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
                     MOVE   SODRNACCT-OPID   TO   NACCT-OPID
      *              ¹¹¿·Ç¯·îÆü
                     MOVE   SODRNACCT-UPYMD  TO   NACCT-UPYMD
      *              ¹¹¿·»þ´Ö
                     MOVE   SODRNACCT-UPHMS  TO   NACCT-UPHMS
      *
                     MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                     MOVE    "DBUPDATE"      TO   MCP-FUNC
                     MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                     MOVE    "key"           TO   MCP-PATHNAME
                     CALL    "ORCDBMAIN"          USING
                                                  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
      *
      *
                     MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                     EVALUATE   SODRNACCT-SKJTIME
                         WHEN    "01"
                             MOVE   "key63"      TO  MCP-PATHNAME
                         WHEN    "02"
                             MOVE   "key64"      TO  MCP-PATHNAME
                         WHEN    "03"
                             MOVE   "key65"      TO  MCP-PATHNAME
                     END-EVALUATE
                     PERFORM   910-NYUINACCT-READ-SEC
                 END-PERFORM
             END-IF
      *      Æþ±¡²ñ·×¥¯¥í¡¼¥º
             MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
             EVALUATE   SODRNACCT-SKJTIME
                WHEN    "01"
                    MOVE    "key63"          TO  MCP-PATHNAME
                WHEN    "02"
                    MOVE    "key64"          TO  MCP-PATHNAME
                WHEN    "03"
                    MOVE    "key65"          TO  MCP-PATHNAME
             END-EVALUATE
             PERFORM   910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       4801-SHOKUJI-201604-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    »ØÄê·îÊ¬¤ÎÆþ±¡²ñ·×ºîÀ®½èÍý
      *****************************************************************
       4801-NYUKAIKEI-MONTH-SEC         SECTION.
           DISPLAY "4801-NYUKAIKEI-MONTH-SEC"
      *
      *    Æþ±¡²ñ·×Â¸ºß¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡Ë
           MOVE    ZERO                    TO  FLG-NYUINACCT-CHK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           DISPLAY "LNK-PTNYUINRRK-HOSPNUM = " LNK-PTNYUINRRK-HOSPNUM
           DISPLAY "LNK-PTNYUINRRK-PTID = " LNK-PTNYUINRRK-PTID
           DISPLAY "SODRNACCT-STARTYMD = " SODRNACCT-STARTYMD
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          =   ZERO
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-NYUINACCT-CHK
               END-IF
           END-IF
      *
           IF     FLG-NYUINACCT-CHK   =  ZERO
               DISPLAY "FLG-NYUINACCT-CHK = 0"
           END-IF
           IF     FLG-NYUINACCT-CHK   =  1
               DISPLAY "FLG-NYUINACCT-CHK = 1"
           END-IF
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *    Æþ±¡²ñ·×¤¬Â¸ºß¤·¤Ê¤¤¾ì¹ç¤Î²ñ·×ºîÀ®
           IF   FLG-NYUINACCT-CHK      =   1
      *        Á°·îÊ¬Æþ±¡²ñ·×¤ÎÂ¸ºß¥Á¥§¥Ã¥¯
               PERFORM     2001-NYUINACCT-MAECHK-SEC
               IF     FLG-NYUINACCT-MAE-CHK   =  ZERO
                   DISPLAY "FLG-NYUINACCT-MAE-CHK = 0"
               END-IF
               IF     FLG-NYUINACCT-MAE-CHK   =  1
                   DISPLAY "FLG-NYUINACCT-MAE-CHK = 1"
               END-IF
               IF   FLG-NYUINACCT-MAE-CHK     =    1
      *            Æþ±¡²ñ·×ºîÀ®½èÍý
                   PERFORM     4801-NYUINACCT-INS-SEC
               ELSE
                   GO  TO  4801-NYUKAIKEI-MONTH-EXT
               END-IF
           END-IF
      *
           .
       4801-NYUKAIKEI-MONTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×Á°·î¥Ç¡¼¥¿Â¸ºß¥Á¥§¥Ã¥¯½èÍý
      *    ¡ÊÊÝ¸±ÁÈ¹ç¤»¤ÎÂàÈò½èÍý¤âÊ»¤»¤Æ¹Ô¤¦¡Ë
      *****************************************************************
       2001-NYUINACCT-MAECHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-TAIHI-HKN
      *
           MOVE    ZERO                TO  FLG-NYUINACCT-MAE-CHK
      *
      *    Á°·îÊ¬²ñ·×¼èÆÀÍÑ¤ÎÇ¯·î»»½Ð
           MOVE    SODRNACCT-STARTYMD(1:6)
                                       TO  WRK-CHK-SRYYM
           IF      WRK-CHK-SRYMR        =   1
               COMPUTE   WRK-CHK-SRYYR =   WRK-CHK-SRYYR - 1
               MOVE      12            TO  WRK-CHK-SRYMR
           ELSE
               COMPUTE   WRK-CHK-SRYMR =   WRK-CHK-SRYMR - 1
           END-IF
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÂàÈòÍÑ¤Ë·îËöÆü¤ò»»Äê
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-CHK-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE   LNK-DAY7-KOYOMI      TO    WRK-GETSUMATSU-YMD
      *
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-CHK-SRYYM           TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          =   ZERO
      *            ÊÝ¸±ÁÈ¹ç¤»¤ÎÂàÈò
                   MOVE    NACCT-DAY(WRK-GETSUMATSU-D)
                                           TO  WRK-TAIHI-HKN
                   MOVE    1               TO  FLG-NYUINACCT-MAE-CHK
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       2001-NYUINACCT-MAECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×ºîÀ®½èÍý
      *****************************************************************
       4801-NYUINACCT-INS-SEC          SECTION.
      *
           IF    FLG-NYUINACCT-MAE-CHK     =    1
               DISPLAY "CRENYU PTNUM = " PTNUM-PTNUM
                       " NYUIN-ACCT CREATE"
               PERFORM     4801-KAIKEI-SAKUSEI-SEC
           ELSE
               DISPLAY "CRENYU PTNUM = " PTNUM-PTNUM
                       " NYUIN-ACCT ZENGETSU NOTHING"
           END-IF
      *
           .
       4801-NYUINACCT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *   ²ñ·×ºîÀ®½èÍý
      *****************************************************************
       4801-KAIKEI-SAKUSEI-SEC            SECTION.
      *
      *    Æþ±¡²ñ·×¥Æ¡¼¥Ö¥ëºîÀ®½èÍý
           PERFORM 410191-NYUACCT-SAKUSEI-SEC
           IF      IPN-NYUKAIKEI-SAKUSEI-FLG   =   "1"
               PERFORM 410191-IPNNYUACCT-SAKUSEI-SEC
           END-IF
      *
           .
      *
       4801-KAIKEI-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¥Æ¡¼¥Ö¥ëºîÀ®½èÍý¡Ê£±¥õ·îÊ¬¡Ë                                    
      *****************************************************************
       410191-NYUACCT-SAKUSEI-SEC   SECTION.
      *
           MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
           INITIALIZE                      ORCSSYOKIKSNAREA
           MOVE    SODRNACCT-STARTYMD  TO  LNK-PTNYUINRRK-TENNYUYMD
      *
      *    ÄÌ»»Æü¿ô¤òµá¤á¤ë¡ÊÆþ±¡ÍúÎò¤ò¸¡º÷¡Ë
           MOVE    ZERO                TO  WRK-TSUSAN
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM  910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key62"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
      **         MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                           TO  WRK-TOKUTEI-NYUIN
      *        Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µÎÅÍÜÉÂÅïÆþ±¡ÎÁ¤ÏÊ¿À®£±£¸Ç¯£³·îËø
               IF     (PTNYUINRRK-TOKUTEI-KBN   =   "2")   AND
                      (PTNYUINRRK-TOKUTEI-NYUIN =   "06")  AND
                      (SODRNACCT-STARTYMD(1:6)  >=  "200604")
                       MOVE      SPACE     TO  WRK-TOKUTEI-NYUIN
               END-IF
               MOVE    PTNYUINRRK-REC      TO  WK-PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    ÄÌ»»Æü¿ô¤òµá¤á¤ë¡ÊÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö¤ò»ÈÍÑ¡Ë
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD       NOT =  SPACE )
               GO  TO  410191-NYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYUINRRK-REC   TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    SODRNACCT-STARTYMD(1:6)
                                       TO  SYOKIKSN-IN-SANTEISTYMD(1:6)
           MOVE    "01"                TO  SYOKIKSN-IN-SANTEISTYMD(7:2)
      *
      *     DISPLAY "====AZU  KAIKEI-SAKUSEIL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    Ê¿À®£²£´Ç¯£³·î£³£±Æü»þÅÀ¤Ç°¡µÞÀ­´üÆþ±¡°åÎÅ´ÉÍýÎÁ£±¤ò»»Äê
      *    ¤·¤Æ¤¤¤ë¾õÂÖ¤«È½Äê¤¹¤ë¥µ¥Ö¤ò¸Æ¤Ó½Ð¤·
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
           IF      WRK-TOKUTEI-NYUIN           NOT =   SPACE
               PERFORM 4801-TOKTEI-NYUIN-SET-SEC
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ¤Î¾ì¹ç¤Ï´ü´Ö²Ã»»¤¬Â¸ºß¤¹¤ë¤¿¤á¡¢
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÎÅÍÜÉÂ¾²¤ÇÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤òÁê¸ß»»Äê¤¹¤ë¾ì¹ç¤â
      *        ½é´ü²Ã»»¥µ¥Ö¤ò¸Æ¤Ó½Ð¤¹¡ÊÊ¿À®£²£´Ç¯²þÀµ¤ÇÄÉ²Ã¡Ë
      *        ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Ë»»Äê¤Ë¤¹¤ë°Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¶Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦°ìÈÌÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦Àº¿ÀÉÂÅïÆÃÊÌÆþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£²£¸Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£·ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦°ìÈÌÉÂÅï£±£µÂÐ£±Æþ±¡´ðËÜÎÁ
      *        °Ê²¼Æþ±¡ÎÁ¤òÄÉ²Ã¡ÊÊ¿À®£³£°Ç¯²þÀµ¤ÇÂÐ±þ¡Ë
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£²
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£³
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£´
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£µ
      *        ¡¦ÎÅÍÜÉÂÅï¡¦µÞÀ­´ü°ìÈÌÆþ±¡ÎÁ£¶
      *        ¡¦ÎÅÍÜÉÂÅï¡¦ÃÏ°è°ìÈÌÆþ±¡ÎÁ£±
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         ÆÃÄêÆþ±¡ÎÁ¤Î»»ÄêÍ×·ï³°¤Î»»Äê
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         Ê¿À®£²£¸Ç¯²þÀµÂÐ±þ
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         Ê¿À®£³£°Ç¯²þÀµÂÐ±þ
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"      TO  SPA-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               END-IF
           ELSE
               PERFORM 4801-BTUSEL-SEC
               IF      SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   PERFORM 4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  WRK-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"          TO  WRK-ERRCD
                   GO  TO  410191-NYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·¡Ê½èÍý¶èÊ¬£¹¤Ë¤è¤ê£±¥õ·îÊ¬¤Î¤ßºîÀ®¡Ë
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-EDSRYYMD
           PERFORM VARYING IDX6        FROM    1   BY   1
                   UNTIL  (IDX6            >     7 )    OR
                          (SYOKIKSN-OT-KGNSRYCD(IDX6)   =  SPACE)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (IDX6)
           END-PERFORM
      *
      *    °åÎÅ´Ñ»¡Æþ±¡¥»¥Ã¥È
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    ÆÃÄêÆþ±¡¤ÇÆþ±¡ÎÁ¤¬ÊÑ¹¹¤Ë¤Ê¤ë¥³¡¼¥É¤ÎÂÐ±þ
      *
      *    ===  TEST ===
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "2"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   SYOKIKSN-IN-SANTEISTYMD  TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF    LNK-AKYUSEI-RC          =   1
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
      *    ÎÅÍÜÉÂÅï¡ÊÉÂ¾²¡ËÆþ±¡¤Î¥Õ¥é¥°¥»¥Ã¥È
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                     MOVE    "1"     TO
                                  LNK-TOKNYUIN2-TOKUBETSU-FLG(2)
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                IF    LNK-TOKNYUIN2-RC  =   1
                    MOVE    SPACE         TO  WRK-TOKUTEI-NYUIN
                                              SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                END-IF
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           IF      WRK-TOKUTEI-NYUIN   =   SPACE
               MOVE     "0"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        ÆÃÄêÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡¢ÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁÅù¤Ï
      *        ÆÃÄêÆþ±¡ÎÁ°·¤¤¤«¤é³°¤¹
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   LNK-PTNYUINRRK-HOSPNUM
                                            TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            ÆÃÄêÆþ±¡´ðËÜÎÁ¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë¤Ï¥Þ¥¹¥¿¤¬
      *            Ê¿À®£²£¶Ç¯£¹·î£³£°Æü¤Þ¤Ç¤Î°Ù¡¢»²¾È»þ¤ÎÆüÉÕ¤òÊÑ¹¹
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   LNK-PTNYUINRRK-HOSPNUM
                                            TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DB-SEL-FETCH-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    ³°Çñ
           MOVE   "099999911"      TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE   "099999912"      TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           DISPLAY "////////////////////////////////////////////"
           DISPLAY "410191-NYUACCT-SAKUSEI-SEC "
           DISPLAY "WRK-TAIHI-SHOKUJI = " WRK-TAIHI-SHOKUJI
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   WRK-TAIHI-SHOKUJI    TO  LNK-SCNYUACCT-SHOKUJIKBN
           IF     PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   PTNYUINRRK-SKJKBN-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   PTNYUINRRK-SKJKBN-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   PTNYUINRRK-SKJKBN-YU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    ¿©Æ²²Ã»»
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI        =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570"     TO
                                           LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1               TO  LNK-SCNYUACCT-SHOKUDO-KBN
      **             IF      WRK-TAIHI-SHOKUDOU   =   0
      **                  MOVE   0           TO  LNK-SCNYUACCT-SHOKUDO-KBN
      **             END-IF
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    WRK-TAIHI-HKN   TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    Å¾²Ê¸µÆþ±¡²Ê
           MOVE   "00"             TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      *
      *     DISPLAY   "****4801SENTEI-SET-SEC MAE **** "
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1         TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO      TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¤ÎÁê¸ß»»Äê¥³¡¼¥ÉÃÖ¤­´¹¤¨¡ÊÊ¿À®£²£¶Ç¯²þÄê¡Ë
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    ÆÃÄêÆþ±¡´ðËÜÎÁ¤Î¥³¡¼¥ÉºÙÊ¬²½ÃÖ¤­´¹¤¨
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            ÆÃÄêµ¡Ç½ÉÂ±¡¤ÏÉÂÅïÀßÄê¤µ¤ì¤¿Æþ±¡¥³¡¼¥É¤«¤éÈ½Äê
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        ÆÃÊÌÆþ±¡´ðËÜÎÁ
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                ÀìÌçÉÂ±¡
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                ¾ã³²¼Ô»ÜÀßÅù
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
      *     DISPLAY   "********ORCSNYUACCT MAE **********"
      **     MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  LNK-SCNYUACCT-BTU-NUM
           CALL    "ORCSNYUACCT"       USING   SPA-AREA
                                               ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA      TO  SPA-SRYKA
      *
           .
      *
       410191-NYUACCT-SAKUSEI-EXT.
           EXIT.
      *
      *21118
      *****************************************************************
      *    Æþ±¡²ñ·×¥Æ¡¼¥Ö¥ëºîÀ®½èÍý¡ÊÆÃÄêÆþ±¡ÎÁ¸å¤Î°ìÈÌÆþ±¡ÎÁ¡Ë
      *****************************************************************
       410191-IPNNYUACCT-SAKUSEI-SEC   SECTION.
      *
      *     DISPLAY   "410191-NYUACCT-SAKUSEI-SEC"
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    ÄÌ»»Æü¿ô¤òµá¤á¤ë¡ÊÆþ±¡ÍúÎò¤ò¸¡º÷¡Ë
           MOVE    ZERO                TO  WRK-TSUSAN
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC               =  ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key62"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
      **         MOVE    PTNYUINRRK-SHONUM
      **                                 TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-REC  TO  WK-PTNYUINRRK-REC
      *         DISPLAY  "PTNYUINRRK-RRKNUM = " PTNYUINRRK-RRKNUM
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    ÄÌ»»Æü¿ô¤òµá¤á¤ë¡ÊÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö¤ò»ÈÍÑ¡Ë
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD    NOT =  SPACE )
               GO  TO  410191-IPNNYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYUINRRK-REC   TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    IPN-SANTEISTYMD     TO  SYOKIKSN-IN-SANTEISTYMD
      *
           MOVE    SPACE               TO  WRK-TOKUTEI-NYUIN
           IF      WRK-TOKUTEI-NYUIN   NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   PERFORM 4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  WRK-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"          TO  WRK-ERRCD
                   GO  TO  410191-NYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×ºîÀ®¥µ¥Ö¤Î¸Æ½Ð¤·
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
      *     DISPLAY  "PTNYUINRRK-PTID = " PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE      SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
                                           LNK-SCNYUACCT-EDSRYYMD
      *     DISPLAY "LNK-SCNYUACCT-STSRYYMD = " LNK-SCNYUACCT-STSRYYMD
           PERFORM VARYING     IDX6    FROM    1   BY   1
               UNTIL          (IDX6        >   7   )   OR
                              (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE)
               MOVE    SYOKIKSN-OT-KGNSRYCD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                             (IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (IDX6)
           END-PERFORM
      *    ÄÌ»»»»ÄêºÑÆü¿ô
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *    Æþ±¡ÎÁ²Ã»»¿ÇÎÅ¥³¡¼¥É¼è¤ê¹þ¤ß
           PERFORM 4801-NYUKSN-GET-SEC
      *    ÆÃÄêÆþ±¡ÎÁ¶èÊ¬¤Î¥»¥Ã¥È
           MOVE    "0"             TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *    ³°Çñ
           MOVE    "099999911"     TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    ¼¼ÎÁº¹³Û
           MOVE    "099999912"     TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    ¿©»ö¶èÊ¬
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE    WRK-TAIHI-SHOKUJI   TO  LNK-SCNYUACCT-SHOKUJIKBN
           IF     PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   PTNYUINRRK-SKJKBN-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   PTNYUINRRK-SKJKBN-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   PTNYUINRRK-SKJKBN-YU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    ¿©Æ²²Ã»»
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI           =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570"     TO
                                           LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1               TO  LNK-SCNYUACCT-SHOKUDO-KBN
      **             IF      WRK-TAIHI-SHOKUDOU   =   0
      **                  MOVE   0           TO  LNK-SCNYUACCT-SHOKUDO-KBN
      **             END-IF
               END-IF
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    WRK-TAIHI-HKN   TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    Å¾²Ê¸µÆþ±¡²Ê
           MOVE    "00"            TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      **   
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    Æþ±¡¥«¥ì¥ó¥À¡¼Ì¤ÀßÄê¤ÎÈ½Äê
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1         TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO      TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      **     MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"    USING   SPA-AREA
                                           ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA  TO  SPA-SRYKA
      *
           .
      *
       410191-IPNNYUACCT-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¾È²ñ¥Æ¡¼¥Ö¥ëÊÔ½¸¥µ¥Ö¸Æ¤Ó½Ð¤·
      *****************************************************************
       4900-ORCSS010-SEC         SECTION.
      *
           MOVE    NRRK-TENNYUYMD  TO  WRK-STYMD
           MOVE    WRK-LASTSRYYM   TO  WRK-EDYMD
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( WRK-EDYMD       =   SPACE )
            OR   ( NACCT-SRYYM >=  WRK-EDYMD (1:6) )
               MOVE    NACCT-SRYYM TO  WRK-EDYMD
           END-IF
      *
           EVALUATE    TRUE
           WHEN      ( SODRNACCT-ORDERID(1:2)  =  "03" )
      *
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STYMD
      *
               IF    ( SODRNACCT-SAME-AFTER    =   "1" )
                   CONTINUE
               ELSE
                   MOVE    WRK-STYMD       TO  WRK-EDYMD
               END-IF
           WHEN      ( SODRNACCT-ORDERID(1:2)  =  "02" OR "06" )
           WHEN      ( SODRNACCT-ORDERID       =  "9901" )
               MOVE    SODRNACCT-STARTYMD
                                   TO  WRK-STYMD
               MOVE    SODRNACCT-STARTYMD (1:6)
                                   TO  WRK-EDYMD
           END-EVALUATE
      *
           IF    ( WRK-EDYMD       =   SPACE )
               DISPLAY "SELECT FAILED [NYUINACCT] PTID:"
                        LNK-PTNYUINRRK-PTID
               MOVE    1               TO  FLG-END
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 910-DB-SEL-FETCH-SEC
           END-IF
      *
      D    DISPLAY "ORCSS010 " SODRNACCT-ORDERID " "
      D             WRK-STYMD "-" WRK-EDYMD
      *
           PERFORM UNTIL ( WRK-STYMD (1:6) >   WRK-EDYMD (1:6))
                    OR   ( FLG-END     NOT =   ZERO )
      *
               INITIALIZE                      SS010-AREA
               MOVE    LNK-PTNYUINRRK-PTID TO  SS010-PTID
               MOVE    WRK-STYMD (1:6)     TO  SS010-SRYYM
               CALL    "ORCSS010"      USING   SS010-AREA
                                               SPA-AREA
               IF    ( SS010-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-END
               END-IF
      *
               MOVE    WRK-STYMD       TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE     1              TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-STYMD
      *
           END-PERFORM
      *
           .
      *
       4900-ORCSS010-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¾ðÊó¸¡º÷½èÍý(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACCT-READ-SEC          SECTION.
      *
           PERFORM    910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID TO  NACCT-PTID
           MOVE    "999999"            TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DB-SEL-FETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY19-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸ÄÊÌÀßÄê¥Æ¡¼¥Ö¥ëÆÉ¹þ½èÍý
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-PTCONF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý¡ÊFHETCH¤â¹Ô¤¦)
      *****************************************************************
       910-DB-SEL-FETCH-SEC              SECTION.
      *
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DB-SEL-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-ORCDBMAIN-SEC
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 910-ORCDBMAIN-SEC
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       910-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       910-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¹¹¿·½èÍý
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE     "DBUPDATE"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
