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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSNINF02.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : APIÏ¢·ÈÍÑ¥â¥¸¥å¡¼¥ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Âà±¡ÀÁµá³ÛÊÖµÑ
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  14/04/23    NACL¡ÝÂÀÅÄ¡¡¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-PTTEIKIRRK          PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXC                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDXJ                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXS                    PIC 9(05).
           03  IDXT                    PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-S02MSG              PIC X(100).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-SKYINF-KBN          PIC X(01).
           03  WRK-KINGAKU             PIC S9(10).
           03  WRK-KINGAKUM9           PIC ---------9.
           03  WRK-KINGAKUMM           PIC ----------.
           03  WRK-DENPNUM-G.
               05  WRK-DENPNUM         PIC 9(07).
      *
       01  KEY-AREA.
           03  KEY-SKYSTYMD            PIC X(08).
           03  KEY-SKYEDYMD            PIC X(08).
      *
       01  TSYU-AREA.
         02  TSYU-OCC                  OCCURS  10.
           COPY  "CPSYUNOU.INC"        REPLACING   //SYU-//
                                       BY          //TSYU-//.
      *
       01  TTLSYU-AREA.
         02  TTLSYU-OCC                OCCURS  3.
           COPY  "CPSYUNOU.INC"        REPLACING   //SYU-//
                                       BY          //TTLSYU-//.
      *
      *    ÀÁµáÅÀ¿ôÌ¾¾Î
       01  TBL-HKNTEN-MEI-AREA.
           03  FILLER              PIC X(30)   VALUE   "½é¡¦ºÆ¿ÇÎÁ".
           03  FILLER              PIC X(30)   VALUE   "°å³Ø´ÉÍýÅù".
           03  FILLER              PIC X(30)   VALUE   "ºßÂðÎÅÍÜ".
           03  FILLER              PIC X(30)   VALUE   "ÅêÌô".
           03  FILLER              PIC X(30)   VALUE   "Ãí¼Í".
           03  FILLER              PIC X(30)   VALUE   "½èÃÖ".
           03  FILLER              PIC X(30)   VALUE   "¼ê½Ñ".
           03  FILLER              PIC X(30)   VALUE   "Ëã¿ì".
           03  FILLER              PIC X(30)   VALUE   "¸¡ºº".
           03  FILLER              PIC X(30)   VALUE   "²èÁü¿ÇÃÇ".
           03  FILLER              PIC X(30)   VALUE   "¥ê¥Ï¥Ó¥ê".
           03  FILLER              PIC X(30)   VALUE   "Àº¿À²ÊÀìÌç".
           03  FILLER              PIC X(30)   VALUE   "Êü¼ÍÀþ¼£ÎÅ".
           03  FILLER              PIC X(30)   VALUE   "ÉÂÍý¿ÇÃÇ".
           03  FILLER              PIC X(30)   VALUE   "Æþ±¡ÎÁ".
           03  FILLER              PIC X(30)   VALUE   "ÎÅÍÜÃ´Åö¼êÅö".
       01  TBL-HKNTEN-MEI-RES      REDEFINES   TBL-HKNTEN-MEI-AREA.
           03  TBL-HKNTEN-MEI      PIC X(30)   OCCURS   16.
      *
       01  CONST-AREA.
           03  CONST-H190401           PIC X(08)   VALUE   "20070401".
           03  CONST-JIHI-MAX          PIC 9(03)   VALUE   10.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSKANACHK.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ÊÝ¸±ÁÈ¹ç¤»¥»¥Ã¥ÈÎÎ°è
           COPY    "CPORCSAPIHKN.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ´µ¼ÔÄê´üÀÁµáÍúÎò
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSNINF02.INC".
      *
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNINF02-AREA
           LNK-SYUNOU-REC-T
           SPA-AREA.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
      *
      *    ½é´ü½èÍý
           PERFORM 100-INIT-SEC
      *
      *    ¼ç½èÍý
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    ÊÖµÑÎÎ°èÊÔ½¸
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    SNINF02-IN-SKYSTYMD   TO  KEY-SKYSTYMD
           MOVE    SNINF02-IN-SKYEDYMD   TO  KEY-SKYEDYMD
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-TSYU-EDIT-SEC
      *
           PERFORM 2001-SYUNOU-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÂàÈòÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       2001-TSYU-EDIT-SEC              SECTION.
      *
           INITIALIZE                  TSYU-AREA
                                       TTLSYU-AREA
      *
           PERFORM VARYING IDXS    FROM    1   BY  1
                   UNTIL ( IDXS    >   10 )
                    OR   ( LNK-SYU-SKYSTYMD (IDXS) =   SPACE )
      *
               MOVE    LNK-SYUNOU-REC (IDXS)
                                   TO  TSYU-OCC (IDXS)
      *
               MOVE    ZERO        TO  TSYU-FUKU-DENPNUM     (IDXS)
                                       TSYU-GRP-DENPNUM (IDXS)
      *
               IF    ( IDXS        >=  10 )
                OR   ( LNK-SYU-SKYSTYMD (IDXS + 1) =   SPACE )
                OR   ( LNK-SYU-SKYSTYMD (IDXS + 1)(1:6)
                               NOT =   TSYU-SKYSTYMD (IDXS)(1:6))
                   IF    ( TSYU-SKYSTYMD (IDXS)(1:6)
                                   =   KEY-SKYSTYMD (1:6) )
                       MOVE    1   TO  TSYU-FUKU-DENPNUM (IDXS)
                   ELSE
                       MOVE    2   TO  TSYU-FUKU-DENPNUM (IDXS)
                   END-IF
                   IF    ( IDXS        >=  10 )
                    OR   ( LNK-SYU-SKYSTYMD (IDXS + 1) =   SPACE )
                       MOVE    3   TO  TSYU-GRP-DENPNUM (IDXS)
                   END-IF
               END-IF
      *
               IF    ( TSYU-HKNTEKKBN (IDXS)   =   "1" )
                   MOVE    TSYU-KOH-FTN (IDXS)
                                   TO  TSYU-HKNTEKMONEY (IDXS)
                   MOVE    ZERO    TO  TSYU-KOH-FTN     (IDXS)
               END-IF
      *
      *
               IF    ( TSYU-SKYSTYMD (IDXS) (1:6)
                                   =   KEY-SKYSTYMD (1:6) )
                   MOVE    1      TO   IDXT
                   PERFORM 20011-TTLSYU-EDIT-SEC
                   MOVE    3      TO   IDXT
                   PERFORM 20011-TTLSYU-EDIT-SEC
               ELSE
                   MOVE    2      TO   IDXT
                   PERFORM 20011-TTLSYU-EDIT-SEC
                   MOVE    3      TO   IDXT
                   PERFORM 20011-TTLSYU-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       2001-TSYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼¹ç·×ÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       20011-TTLSYU-EDIT-SEC           SECTION.
      *
           MOVE    TSYU-OCC (IDXS)     TO  SYUNOU-REC
      *
           IF    ( TTLSYU-SKYSTYMD (IDXT)
                                       =   SPACE )
            OR   ( TTLSYU-SKYSTYMD (IDXT)
                                   >   SYU-SKYSTYMD )
               MOVE    SYU-SKYSTYMD    TO  TTLSYU-SKYSTYMD (IDXT)
           END-IF
      *
           IF    ( TTLSYU-SKYEDYMD (IDXT)
                                       =   SPACE )
            OR   ( TTLSYU-SKYEDYMD (IDXT)
                                   <   SYU-SKYEDYMD )
               MOVE    SYU-SKYEDYMD    TO  TTLSYU-SKYEDYMD (IDXT)
           END-IF
      *
           COMPUTE TSYU-DENPLASTNUM (IDXT)
               =   TSYU-DENPLASTNUM (IDXT)
               +   1
      *
      *    ÊÝ¸±ÎÁ
           PERFORM VARYING     IDX1   FROM    1   BY  1
                   UNTIL       IDX1   >   17
      *
               COMPUTE TTLSYU-HKNTEN (IDXT IDX1)
                   =   TTLSYU-HKNTEN (IDXT IDX1)
                   +   SYU-HKNTEN (IDX1)
      *
               COMPUTE TTLSYU-TGMONEY   (IDXT IDX1)
                   =   TTLSYU-TGMONEY   (IDXT IDX1)
                   +   SYU-TGMONEY (IDX1)
      *
               COMPUTE TTLSYU-TGMONEY-TAX (IDXT IDX1)
                   =   TTLSYU-TGMONEY-TAX (IDXT IDX1)
                   +   SYU-TGMONEY-TAX (IDX1)
           END-PERFORM
      *
      *    ÊÝ¸±Å¬ÍÑ¶â³Û
           COMPUTE TTLSYU-HKNTEKMONEY   (IDXT)
               =   TTLSYU-HKNTEKMONEY   (IDXT)
               +   SYU-HKNTEKMONEY
      *
      *    Ï·¿Í°ìÉôÉéÃ´¶â
           COMPUTE TTLSYU-RJN-FTN    (IDXT)
               =   TTLSYU-RJN-FTN    (IDXT)
               +   SYU-RJN-FTN
      *
      *    ¸øÈñ°ìÉôÉéÃ´¶â
           COMPUTE TTLSYU-KOH-FTN    (IDXT)
               =   TTLSYU-KOH-FTN    (IDXT)
               +   SYU-KOH-FTN
      *
      *    ¼«Èñ
           PERFORM VARYING IDXJ    FROM    1   BY  1
                   UNTIL ( IDXJ    >   CONST-JIHI-MAX )
      *
               COMPUTE TTLSYU-JIHI-TAXNASI (IDXT IDXJ)
                   =   TTLSYU-JIHI-TAXNASI (IDXT IDXJ)
                   +   SYU-JIHI-TAXNASI(IDXJ)
      *
               COMPUTE TTLSYU-JIHI-TAXARI  (IDXT IDXJ)
                   =   TTLSYU-JIHI-TAXARI  (IDXT IDXJ)
                   +   SYU-JIHI-TAXARI (IDXJ)
      *
           END-PERFORM
      *
      *    ¼«Èñ·×
           COMPUTE TTLSYU-JIHI-TOTAL   (IDXT)
               =   TTLSYU-JIHI-TOTAL   (IDXT)
               +   SYU-JIHI-TOTAL
      *
      *    ¼«Èñ·×¡Ê¾ÃÈñÀÇ¤¢¤ê¡Ë
           COMPUTE TTLSYU-JIHI-TOTAL-TAX (IDXT)
               =   TTLSYU-JIHI-TOTAL-TAX (IDXT)
               +   SYU-JIHI-TOTAL-TAX
      *
      *    ¼«Èñ¾ÃÈñÀÇ
           COMPUTE TTLSYU-JIHI-TAX  (IDXT)
               =   TTLSYU-JIHI-TAX  (IDXT)
               +   SYU-JIHI-TAX
      *
      *    Ï«ºÒ½é¿Ç
           COMPUTE TTLSYU-RSISHOSHIN-MONEY (IDXT)
               =   TTLSYU-RSISHOSHIN-MONEY (IDXT)
               +   SYU-RSISHOSHIN-MONEY
      *
      *    Ï«ºÒºÆ¿Ç
           COMPUTE TTLSYU-RSISAISHIN-MONEY (IDXT)
               =   TTLSYU-RSISAISHIN-MONEY (IDXT)
               +   SYU-RSISAISHIN-MONEY
      *
      *    Ï«ºÒ»ØÆ³
           COMPUTE TTLSYU-RSISDO-MONEY  (IDXT)
               =   TTLSYU-RSISDO-MONEY  (IDXT)
               +   SYU-RSISDO-MONEY
      *
      *    Ï«ºÒ¤½¤ÎÂ¾
           COMPUTE TTLSYU-RSIETC-MONEY (IDXT)
               =   TTLSYU-RSIETC-MONEY (IDXT)
               +   SYU-RSIETC-MONEY
      *
      *    ¿©»öÎÅÍÜÈñ¡ÊÊÝ¸±¡Ë
           COMPUTE TTLSYU-RYOYOHI-SKJ (IDXT)
               =   TTLSYU-RYOYOHI-SKJ (IDXT)
               +   SYU-RYOYOHI-SKJ
      *
      *    ¿©»öÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE TTLSYU-SKYMONEY-SKJ (IDXT)
               =   TTLSYU-SKYMONEY-SKJ (IDXT)
               +   SYU-SKYMONEY-SKJ
      *
      *    ¿©»öÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE TTLSYU-SKYMONEY-SKJ-TAX (IDXT)
               =   TTLSYU-SKYMONEY-SKJ-TAX (IDXT)
               +   SYU-SKYMONEY-SKJ-TAX
      *
      *    ¿©»öÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE TTLSYU-SKYMONEY-SKJ-KEI (IDXT)
               =   TTLSYU-SKYMONEY-SKJ-KEI (IDXT)
               +   SYU-SKYMONEY-SKJ-KEI
      *
      *    ¿©»öÎÅÍÜÈñ¡Ê¼«Èñ¡Ë
           COMPUTE TTLSYU-RYOYOHI-SKJ-JIHI (IDXT)
               =   TTLSYU-RYOYOHI-SKJ-JIHI (IDXT)
               +   SYU-RYOYOHI-SKJ-JIHI
      *
      *    ¿©»öÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI (IDXT)
               =   TTLSYU-SKYMONEY-SKJ-JIHI (IDXT)
               +   SYU-SKYMONEY-SKJ-JIHI
      *
      *    ¿©»öÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-TAX  (IDXT)
               =   TTLSYU-SKYMONEY-SKJ-JIHI-TAX  (IDXT)
               +   SYU-SKYMONEY-SKJ-JIHI-TAX
      *
      *    ¿©»öÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-KEI (IDXT)
               =   TTLSYU-SKYMONEY-SKJ-JIHI-KEI (IDXT)
               +   SYU-SKYMONEY-SKJ-JIHI-KEI
      *
      *    À¸³èÎÅÍÜÈñ¡ÊÊÝ¸±¡Ë
           COMPUTE TTLSYU-RYOYOHI-LIFE (IDXT)
               =   TTLSYU-RYOYOHI-LIFE (IDXT)
               +   SYU-RYOYOHI-LIFE
      *
      *    À¸³èÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE TTLSYU-SKYMONEY-LIFE (IDXT)
               =   TTLSYU-SKYMONEY-LIFE (IDXT)
               +   SYU-SKYMONEY-LIFE
      *
      *    À¸³èÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE TTLSYU-SKYMONEY-LIFE-TAX (IDXT)
               =   TTLSYU-SKYMONEY-LIFE-TAX (IDXT)
               +   SYU-SKYMONEY-LIFE-TAX
      *
      *    À¸³èÉéÃ´³Û¡ÊÊÝ¸±¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE TTLSYU-SKYMONEY-LIFE-KEI (IDXT)
               =   TTLSYU-SKYMONEY-LIFE-KEI (IDXT)
               +   SYU-SKYMONEY-LIFE-KEI
      *
      *    À¸³èÎÅÍÜÈñ¡Ê¼«Èñ¡Ë
           COMPUTE TTLSYU-RYOYOHI-LIFE-JIHI (IDXT)
               =   TTLSYU-RYOYOHI-LIFE-JIHI (IDXT)
               +   SYU-RYOYOHI-LIFE-JIHI
      *
      *    À¸³èÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¡Ë
           COMPUTE TTLSYU-SKYMONEY-LIFE-JIHI (IDXT)
               =   TTLSYU-SKYMONEY-LIFE-JIHI (IDXT)
               +   SYU-SKYMONEY-LIFE-JIHI
      *
      *    À¸³èÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¾ÃÈñÀÇ¡Ë
           COMPUTE TTLSYU-SKYMONEY-LIFE-JIHI-TAX (IDXT)
               =   TTLSYU-SKYMONEY-LIFE-JIHI-TAX (IDXT)
               +   SYU-SKYMONEY-LIFE-JIHI-TAX
      *
      *    À¸³èÉéÃ´³Û¡Ê¼«Èñ¡§¼«¸ÊÉéÃ´¹ç·×¡Ë
           COMPUTE TTLSYU-SKYMONEY-LIFE-JIHI-KEI (IDXT)
               =   TTLSYU-SKYMONEY-LIFE-JIHI-KEI (IDXT)
               +   SYU-SKYMONEY-LIFE-JIHI-KEI
      *
      *    ¼¼ÎÁº¹³Û
           COMPUTE TTLSYU-RMSAGAKU (IDXT)
               =   TTLSYU-RMSAGAKU (IDXT)
               +   SYU-RMSAGAKU
      *
      *    ÀÁµá¶â³Û
           COMPUTE TTLSYU-SKYMONEY (IDXT)
               =   TTLSYU-SKYMONEY (IDXT)
               +   SYU-SKYMONEY
      *
      *    ¸ºÌÈ¶â³Û
           COMPUTE TTLSYU-DISCOUNT-MONEY   (IDXT)
               =   TTLSYU-DISCOUNT-MONEY   (IDXT)
               +   SYU-DISCOUNT-MONEY
      *
           .
       20011-TTLSYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÊÔ½¸½èÍý
      *****************************************************************
       2001-SYUNOU-EDIT-SEC            SECTION.
      *
           PERFORM VARYING IDXS    FROM    1   BY  1
                   UNTIL ( IDXS    >   10 )
                    OR   ( TSYU-SKYSTYMD (IDXS)    =   SPACE )
      *
               MOVE    TSYU-OCC (IDXS) TO  SYUNOU-REC
               MOVE    "0"             TO  WRK-SKYINF-KBN
               PERFORM 20011-SYUNOU-EDIT-SEC
      *
               IF    ( TSYU-FUKU-DENPNUM (IDXS) =   1 OR 2 )
                   MOVE    TSYU-FUKU-DENPNUM (IDXS)
                                       TO  IDXT
                   MOVE    TTLSYU-OCC (IDXT)
                                       TO  SYUNOU-REC
                   MOVE    "1"         TO  WRK-SKYINF-KBN
                   PERFORM 20011-SYUNOU-EDIT-SEC
               END-IF
      *
               IF    ( TSYU-GRP-DENPNUM (IDXS) =   3 )
                   MOVE    TSYU-GRP-DENPNUM (IDXS)
                                       TO  IDXT
                   MOVE    TTLSYU-OCC (IDXT)
                                       TO  SYUNOU-REC
                   MOVE    "2"         TO  WRK-SKYINF-KBN
                   PERFORM 20011-SYUNOU-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       2001-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ýÇ¼ÊÔ½¸½èÍý
      *****************************************************************
       20011-SYUNOU-EDIT-SEC           SECTION.
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           MOVE    IDXA            TO  SNINF02-SKYINF-MAX
      *
           MOVE    "ÀÁµá¾ðÊó¼ïÊÌ"  TO  SNINF02-SKYINF-KBN-LBL  (IDXA)
           MOVE    WRK-SKYINF-KBN  TO  SNINF02-SKYINF-KBN-DATA (IDXA)
           EVALUATE    WRK-SKYINF-KBN
           WHEN    "0"
               MOVE    "ÌÀºÙ"      TO  SNINF02-SKYINF-KBN-NAME (IDXA)
           WHEN    "1"
               MOVE    "·î¹ç·×"    TO  SNINF02-SKYINF-KBN-NAME (IDXA)
           WHEN    "2"
               MOVE    "Áí¹ç·×"    TO  SNINF02-SKYINF-KBN-NAME (IDXA)
           END-EVALUATE
      *
           IF    ( WRK-SKYINF-KBN      =   "0" OR "1" )
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE (1:7)  TO  SNINF02-SRYYM (IDXA)
           END-IF
      *
           IF    ( WRK-SKYINF-KBN      =   "0" )
      *
               MOVE    SYU-DENPNUM     TO  WRK-DENPNUM
               MOVE    WRK-DENPNUM-G   TO  SNINF02-DENPNUM (IDXA)
      *
               PERFORM 900-PTNYUINRRK-KEY42-SEL-SEC
      *
               PERFORM 200111-SRYKA-EDIT-SEC
      *
               PERFORM 200111-HKN-EDIT-SEC
      *
           END-IF
      *
           MOVE    SYU-SKYMONEY        TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  SNINF02-SKYMONEY (IDXA)
           MOVE    SYU-HKNTEKMONEY     TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  SNINF02-HKNMONEY (IDXA)
      *
           COMPUTE WRK-KINGAKU         =   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYU-SKYMONEY-LIFE-JIHI-KEI
                                       +   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
           IF    ( SYU-SRYYMD  <   CONST-H190401 )
               COMPUTE WRK-KINGAKU
                   =   WRK-KINGAKU +   SYU-JIHI-TAX
           END-IF
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  SNINF02-JIHIMONEY (IDXA)
      *
           MOVE    SYU-RJN-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  SNINF02-RJNMONEY  (IDXA)
      *
           MOVE    SYU-KOH-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  SNINF02-KOHMONEY  (IDXA)
      *
           COMPUTE WRK-KINGAKU         =   SYU-SKYMONEY-SKJ-KEI
                                       +   SYU-SKYMONEY-LIFE-KEI
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  SNINF02-SKJMONEY (IDXA)
      *
           COMPUTE WRK-KINGAKU         =   SYU-RSISHOSHIN-MONEY
                                       +   SYU-RSISAISHIN-MONEY
                                       +   SYU-RSISDO-MONEY
                                       +   SYU-RSIETC-MONEY
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  SNINF02-RSIMONEY (IDXA)
      *
           MOVE    SYU-DISCOUNT-MONEY  TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  SNINF02-GENMEN (IDXA)
      *
           MOVE    SYU-TOTAL-HKNTEN    TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  SNINF02-TOTAL-TEN (IDXA)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   16 )
               IF    ( IDX0    =   16 )
                   IF    ( SYU-HKNTEN (IDX0)   NOT =   ZERO )
                       MOVE    TBL-HKNTEN-MEI (IDX0)
                               TO  SNINF02-SRYKBN-NAME  (IDXA IDX0)
                       MOVE    SYU-HKNTEN (IDX0)
                               TO  WRK-KINGAKUMM
                       MOVE    WRK-KINGAKUMM
                               TO  SNINF02-SRYKBN-TENSU (IDXA IDX0)
                   END-IF
               ELSE
                   MOVE    TBL-HKNTEN-MEI (IDX0)
                               TO  SNINF02-SRYKBN-NAME  (IDXA IDX0)
                   MOVE    SYU-HKNTEN (IDX0)
                               TO  WRK-KINGAKUM9
                   MOVE    WRK-KINGAKUM9
                               TO  SNINF02-SRYKBN-TENSU (IDXA IDX0)
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-KINGAKU         =   SYU-RYOYOHI-SKJ
                                       +   SYU-RYOYOHI-LIFE
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  SNINF02-SKJRYOYO (IDXA)
      *
           .
       20011-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ÊÊÔ½¸½èÍý
      *****************************************************************
       200111-SRYKA-EDIT-SEC          SECTION.
      *
           MOVE    PTNYUINRRK-NYUINKA  TO  SNINF02-SRYKA (IDXA)
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-NYUINKA  TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME  TO  SNINF02-SRYKANAME (IDXA)
      *
           .
       200111-SRYKA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       200111-HKN-EDIT-SEC             SECTION.
      *
           MOVE    SYU-HKNCOMBINUM TO  SNINF02-HKNCOMBI (IDXA)
      *
           MOVE    SPA-AREA        TO  TSPA-AREA
           MOVE    SYU-SKYSTYMD    TO  SPA-SYSYMD
           INITIALIZE                  ORCSAPIHKNAREA
           MOVE    "2"             TO  ORCSAPIHKN-KBN2
           CALL    "ORCSAPIHKN"    USING
                                   SPA-AREA
                                   ORCSAPIHKNAREA
           MOVE    TSPA-AREA       TO  SPA-AREA
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   20 )
                     OR  ( ORCSAPIHKN-THKNCOMBI (IDXC)
                                       =   SPACE  )
               IF    ( SYU-HKNCOMBINUM =   ORCSAPIHKN-THKNCOMBI (IDXC))
                   PERFORM 2001111-HKN-EDIT-SEC
                   MOVE    20      TO  IDXC
               END-IF
      *
           END-PERFORM
      *
          .
       200111-HKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       2001111-HKN-EDIT-SEC           SECTION.
      *
      *    ÊÝ¸±¤Î¼ïÎà
           MOVE    ORCSAPIHKN-HKNNUM (IDXC)
                                   TO  SNINF02-HKNNUM   (IDXA)
      *
      *    ÊÝ¸±¼ÔÈÖ¹æ
           MOVE    ORCSAPIHKN-HKNJANUM (IDXC)
                                   TO  SNINF02-HKNJANUM (IDXA)
      *
      *    ÊÝ¸±¤ÎÌ¾¾Î
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDXC)
                                   TO  SNINF02-HKNJANAME(IDXA)
      *
      *    µ­¹æ
           MOVE    ORCSAPIHKN-KIGO  (IDXC)
                                   TO  SNINF02-HKIGO (IDXA)
      *
      *    ÈÖ¹æ
           MOVE    ORCSAPIHKN-NUM  (IDXC)
                                   TO  SNINF02-HNUM  (IDXA)
      *
      *    ¸øÈñ¾ðÊó
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
      *
      *        ¸øÈñ¤Î¼ïÎà
               MOVE    ORCSAPIHKN-KOHNUM (IDXC IDXK)
                                   TO  SNINF02-KOHNUM  (IDXA IDXK)
      *        ¸øÈñ¤Î¼ïÎàÌ¾¾Î
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDXC IDXK)
                                   TO  SNINF02-KOHNAME (IDXA IDXK)
      *        ÉéÃ´¼ÔÈÖ¹æ
               MOVE    ORCSAPIHKN-FTNJANUM (IDXC IDXK)
                                   TO  SNINF02-FTNJANUM(IDXA IDXK)
      *        ¼õµë¼ÔÈÖ¹æ
               MOVE    ORCSAPIHKN-JKYSNUM (IDXC IDXK)
                                   TO  SNINF02-JKYSNUM (IDXA IDXK)
           END-PERFORM
      *
          .
       2001111-HKN-EDIT-EXT.
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
      *
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    »þ¹ïÊÔ½¸½èÍý
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥«¥ì¥ó¥À¡¼½èÍý
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    ·îËöÆü¼èÆÀ½èÍý
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
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
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
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
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
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
      *    Æþ±¡ÍúÎò¸¡º÷½èÍý
      *****************************************************************
       900-PTNYUINRRK-KEY42-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           IF    ( KEY-SKYSTYMD (1:6)  NOT =   KEY-SKYEDYMD (1:6))
            AND  ( KEY-SKYSTYMD (1:6)      =   SYU-SKYSTYMD (1:6))
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-GETUMATU-SEC
               MOVE    WRK-SYMD        TO  PTNYUINRRK-TENNYUYMD
                                           PTNYUINRRK-TENSTUYMD
           ELSE
               MOVE    KEY-SKYEDYMD    TO  PTNYUINRRK-TENNYUYMD
                                           PTNYUINRRK-TENSTUYMD
           END-IF
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY42-SEL-EXT.
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
