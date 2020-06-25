      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
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
       PROGRAM-ID.             ORCR1220.
      *****************************************************************
      *  •∑•π•∆•‡Ãæ        : £œ£“£√£¡
      *  •µ•÷•∑•π•∆•‡Ãæ    : •Ï•ª•◊•»
      *  •≥•Û•›°º•Õ•Û•»Ãæ  : ∏¯≥≤•Ï•ª•◊•»° ∆˛±°°À
      *                      ø«Œ≈∂Ë ¨ Ã≈¿øÙΩ∏∑◊
      *  ¥…Õ˝º‘            : 
      *  ∫Ó¿Æ∆¸…’    ∫Ó∂»º‘        µ≠Ω“
      *  07/11/16    NACL°›∆£∏∂    ø∑µ¨∫Ó¿Æ
      *****************************************************************
      *  •◊•Ì•∞•È•‡Ω§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  05.00.01    NACL-ÃÁœ∆    19/03/11  ÕΩÃÛ•≥°º•…∆…§ﬂ»Ù§–§∑Ω§¿µ
      *  05.00.02    NACL-ÃÁœ∆    19/04/03  •≥•·•Û•»•ﬁ•π•ø§Œ≤˛∏µ¬–±˛§À
      *                                     »º§¶Ω§¿µ
      *  05.00.03    NACL-ÃÁœ∆    19/05/16  ¬‡±°ª˛∂¶∆±ªÿ∆≥Œ¡£≤•≥•·•Û•»º´∆∞
      *                                     µ≠∫‹¬–±˛
      *  05.00.04    NACL-ÃÁœ∆    19/07/01  •Í•œ•”•Í»Øæ…∆¸•Ï•ª•◊•»µ≠∫‹¬–±˛
      *                                     ° µ°«Ωƒ…≤√°À
      *  05.00.05    NACL-ÃÁœ∆    19/12/26  ¥Ô∫‡≈¿øÙ£∞≈¿ª˛§Œ•≥•·•Û•»•≥°º•…
      *                                     §ŒºË§Í∞∑§§Ω§¿µ
      *  05.00.06    NACL-ÃÁœ∆    20/02/10  ÃÙ∫ﬁ°¶¥Ô∫‡§Œ√±∞Ã ‘Ω∏° ≈¿øÙ…’≤√°À
      *  05.00.07    NACL-ÃÁœ∆    20/03/09  Œ·œ¬£≤«Ø£¥∑Ó≤˛¿µ
      *                                     ∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨ ‘Ω∏
      *  05.00.08    NACL-ÃÁœ∆    20/03/16  ¬æ∞ÂŒ≈µ°¥ÿºıø«°‹…∏∑Á§Œ∑◊ªªΩ§¿µ
      *  05.00.09    NACL-ÃÁœ∆    20/03/30  Œ·œ¬£≤«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  05.00.10    NACL-ÃÁœ∆    20/04/16  ¥Ô∫‡§Œ√±∞Ã ‘Ω∏Ω§¿µ
      *  05.00.11    NACL-ÃÁœ∆    20/04/28  °÷¬æ ›∏±§À§∆ø«ª°Œ¡ªªƒÍ∫—°◊§ŒÃææŒ
      *                                      ‘Ω∏Ω§¿µ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE200-FILE     ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
           SELECT  RECE220-FILE     ASSIGN  RECE220PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE220-KEY
                                   FILE    STATUS  IS  STS-RECE220.
           SELECT  RECE230-FILE     ASSIGN  RECE230PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE230-KEY
                                   FILE    STATUS  IS  STS-RECE230.
      *    •®•È°º•’•°•§•Î
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE200-FILE.
       01  RECE200-REC. 
           COPY    "CPRCF200.INC".
      *
       FD  RECE220-FILE.
       01  RECE220-REC. 
           COPY    "CPRCF220.INC".
      *
       FD  RECE230-FILE.
       01  RECE230-REC. 
           COPY    "CPRCF230.INC".
      *
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    •Ï•ª•◊•»Ã¿∫Ÿ•’•°•§•ÎÃææŒ
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE200//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE220//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE230//.
      *
      *    •®•È°º•’•°•§•ÎÃææŒŒŒ∞Ë
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    •’•°•§•ÎÃææŒŒŒ∞Ë
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    •’•È•∞ŒŒ∞Ë
       01  STS-AREA.
           03  STS-RECE200         PIC X(02).
           03  STS-RECE220         PIC X(02).
           03  STS-RECE230         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE200         PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARI5            PIC 9(01).
           03  FLG-DAY21           PIC 9(01).
           03  FLG-DAY22           PIC 9(01).
           03  FLG-NYUKHN          PIC 9(01).
           03  FLG-TENTEKI         PIC 9(01).
           03  FLG-WRITE230        PIC 9(01).
           03  FLG-WRITE230X       PIC 9(01).
           03  FLG-SIKIBETU        PIC 9(01).
           03  FLG-ZANHAIKI        PIC 9(01).
           03  FLG-FORMAL-KISAI    PIC 9(01).
           03  FLG-RECE230-ZAN     PIC 9(01).
           03  FLG-REWRITE         PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-NYUINRYO        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HOUSIJI-COM     PIC 9(01).
           03  FLG-OSIN-COM        PIC 9(01).
           03  FLG-SYOBYO-COM      PIC 9(01).
           03  FLG-RYOYO-COM       PIC 9(01).
           03  FLG-SYODOU-COM      PIC 9(01).
           03  FLG-KENSA-COM       PIC 9(01).
           03  FLG-ZAIKYO-COM      PIC 9(01).
           03  FLG-BOKOHO-COM      PIC 9(01).
           03  FLG-HAIJRT-COM      PIC 9(01).
           03  FLG-TAIKYO-COM      PIC 9(01).
           03  FLG-ZAIHANKO-COM    PIC 9(01).
           03  FLG-KIATU-COM       PIC 9(01).
           03  FLG-SCPRO-COM       PIC 9(01).
           03  FLG-HOUSEI-COM      PIC 9(01).
           03  FLG-HOUMON-COM      PIC 9(01).
           03  FLG-KENKSN-COM      PIC 9(01).
           03  FLG-KINGAZ-COM      PIC 9(01).
           03  FLG-HAIJSK-COM      PIC 9(01).
           03  FLG-CYOJKSN-COM     PIC 9(01).
           03  FLG-CYOJKSN-OK      PIC 9(01).
           03  FLG-RINYAK          PIC 9(01).
           03  FLG-TENTEKIMATOME   PIC 9(01).
           03  FLG-TAHOKEN         PIC 9(01).
           03  FLG-KHNRYO-SKJ      PIC 9(01).
           03  FLG-SYUNOU-ARI      PIC 9(01).
           03  FLG-FORMALNAME      PIC 9(01).
           03  FLG-DOJITU          PIC 9(01).
           03  FLG-HENSYU-NG       PIC 9(01).
           03  FLG-RIHA-OKIKAE     PIC 9(02).
           03  FLG-IRYORENKEI      PIC 9(01).
           03  FLG-YKZ-PRTKBN      PIC 9(01).
           03  FLG-KIZ-PRTKBN      PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
      *
      *    •´•¶•Û•»ŒŒ∞Ë
       01  CNT-AREA.
           03  CNT-IN-COMMIT       PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT1            PIC 9(06).
           03  CNT-OUT2            PIC 9(06).
           03  CNT-IN              PIC 9(06).
           03  CNT-OUT3            PIC 9(06).
           03  CNT-DAY21           PIC 9(06).
           03  CNT-DAY21-X         PIC 9(06).
           03  CNT-DAY21-Y         PIC 9(06).
           03  CNT-DAY22           PIC 9(06).
      *
      *    ≈∫ª˙ŒŒ∞Ë
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03).
           03  IDX4                PIC 9(03).
           03  IDX5                PIC 9(03).
           03  IDX6                PIC 9(03).
           03  IDX8                PIC 9(02).
           03  IDX9                PIC 9(02).
           03  IDX11               PIC 9(03).
           03  IDX15               PIC 9(03).
           03  IDX17               PIC 9(03).
           03  IDX25               PIC 9(02).
           03  IDX26               PIC 9(02).
           03  IDX30               PIC 9(03).
           03  IDX31               PIC 9(03).
           03  IDX35               PIC 9(03).
           03  IDX40               PIC 9(02).
           03  IDX41               PIC 9(02).
           03  IDXA                PIC 9(01).
           03  IDXB                PIC 9(03).
           03  IDXX                PIC 9(03).
           03  IDY                 PIC 9(03).
           03  IDY3                PIC 9(03).
           03  IDW                 PIC 9(03).
           03  IDW2                PIC 9(03).
           03  IDW3                PIC 9(03).
           03  IDW4                PIC 9(03).
           03  IDW5                PIC 9(03).
           03  IDW6                PIC 9(03).
           03  IDW9                PIC 9(02).
           03  ID1                 PIC 9(03).
           03  ID4                 PIC 9(03).
           03  IDV                 PIC 9(02).
           03  IDV2                PIC 9(03).
           03  IDU                 PIC 9(03).
           03  IDZ1                PIC 9(03).
           03  IDZ2                PIC 9(03).
           03  NUM-IDX             PIC 9(01).
           03  SYUKEI-IDX          PIC 9(03).
           03  SYUKEI-IDX2         PIC 9(03).
           03  SYUKEI-IDX3         PIC 9(03).
           03  SYUKEI-IDX4         PIC 9(03).
           03  SYSKAN-IDX          PIC 9(03).
           03  SYSKAN-IDX2         PIC 9(03).
           03  FUKA-IDX            PIC 9(03).
      *
      *    ∫ﬁæ Û¬‡»ÚŒŒ∞Ë
       01  WK-ZAIKAISU             PIC S9(07).
       01  WK-ZAITEN               PIC S9(08).
       01  WK-SYUTEN               PIC S9(08).
       01  WK-KIZAITEN             PIC S9(08).
       01  WK-YKZTEN               PIC S9(08).
       01  WK-YKZTEN2              PIC S9(08).
      *
      *    •®•È°º•·•√•ª°º•∏ ‘Ω∏ŒŒ∞Ë
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM           PIC X(20).
           03  ERR-ERRMSG          PIC X(80).
           03  ERR-HEN-ERRMSG      PIC X(200).
      *
      *    ≈¿øÙ•∆°º•÷•Î∏°∫˜Õ—¬–æ›∆¸ŒŒ∞Ë
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY        PIC 9(02).
           03  WK-TAIHI-FLG        PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM  PIC 9(06).
               05  WK-TAIHI-SRYD   PIC 9(02).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY1-HYOKETU-SRYCD              PIC 9(09).
               05  WRK-DAY1-HYOKETU-TEN                PIC 9(09)V99.
               05  WRK-DAY1-KSN-SRYCD                  PIC 9(09).
               05  WRK-DAY1-NYUTENTTLKBN               PIC 9(03).
               05  WRK-DAY1-KHNTEN                     PIC 9(08)V9(5).
               05  WRK-DAY1-ZAITEN-GEN                 PIC 9(08)V9(5).
               05  WRK-DAY1-KSNTEN                     PIC S9(08).
               05  WRK-DAY1-NYUKHNKSNKBN               PIC 9(03).
               05  WRK-DAY1-CHUKSNCD                   PIC X(04).
               05  WRK-DAY1-NYUKHNKBN                  PIC 9(03).
               05  WRK-DAY1-NYUINKHN                   PIC X(09).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY4X                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
       01  WRK-DAY-TABLE2.
           03  WRK-DAY-T2                              OCCURS   31.
               05  WRK-DAY21                           PIC 9(04).
               05  WRK-DAY21-X                         PIC 9(04).
               05  WRK-DAY21-Y                         PIC 9(04).
               05  WRK-DAY22                           PIC 9(04).
               05  WRK-DAY22-NYUTENTTLKBN              PIC 9(03).
               05  WRK-DAY22-NYUKHNRYO                 PIC 9(08).
               05  WRK-DAY22-NYUKHNKSNKBN              PIC 9(03).
               05  WRK-DAY22-CHUKSNCD                  PIC X(04).
               05  WRK-DAY22-NYUKHNKBN                 PIC 9(03).
               05  WRK-DAY22-NYUINKHN                  PIC X(09).
               05  WRK-DAY24                           PIC 9(04).
      *
       01  WRK-GAI-TABLE.
           03  WRK-GAI-TBL                             OCCURS   62.
             04  WRK-GAI-T                             OCCURS   10.
               05  WRK-GAI-DAY                         PIC 9(02).
               05  WRK-GAI-ZAITEN                      PIC 9(08).
               05  WRK-GAI-KANGOGEN-SRYCD              PIC X(09).
               05  WRK-GAI-ZAIKAISU                    PIC 9(04).
               05  WRK-GAI-KAISU-T.
                06  WRK-GAI-KAISU-OCC                  OCCURS   31.
                 07  WRK-GAI-KAISU                     PIC 9(01).
      *
       01  WRK-GAI-TABLE2.
           03  WRK-GAI-T2                              OCCURS   20.
               05  WRK-GAI2-DAY                        PIC 9(02).
               05  WRK-GAI2-KANGOGEN-SRYCD.
                   07  WRK-GAI2-KANGOGEN-SRYCD-9       PIC 9(09).
               05  WRK-GAI2-SRYCD.
                   07  WRK-GAI2-SRYCD-9                PIC 9(09).
               05  WRK-GAI2-SRYCD2.
                   07  WRK-GAI2-SRYCD2-9               PIC 9(09).
               05  WRK-GAI2-ZAITEN                     PIC 9(08).
               05  WRK-GAI2-ZAIKAISU                   PIC 9(04).
               05  WRK-GAI2-SRYKBNFLG                  PIC 9(01).
               05  WRK-GAI2-KAISU-T.
                06  WRK-GAI2-KAISU-OCC                 OCCURS   31.
                 07  WRK-GAI2-KAISU                    PIC 9(01).
      *
       01  WRK-GAI-DATA                                PIC X(73).
      *
       01  WRK-SKJ200604-TABLE.
           03  WRK-SKJ200604-T                         OCCURS   31.
               05  WRK-SKJ200604-KBN                   PIC 9(01).
               05  WRK-SKJ200604-SRYCDTOTAL            PIC 9(14).
               05  WRK-SKJ200604-MEISAISU              PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL           PIC 9(02).
      **>      05  WRK-SKJ200604-T3                    OCCURS    3.
               05  WRK-SKJ200604-T3                    OCCURS    4.
                   07  WRK-SKJ200604-DAY               PIC 9(04).
               05  WRK-SKJ200604-DAY-KAN               PIC 9(01).
      *
       01  WRK-SKJ-DOJITU-TABLE.
           03  WRK-SKJ-DOJITU-T                        OCCURS   31.
               05  WRK-SKJ-DOJITU-KBN                  PIC 9(01).
               05  WRK-SKJ-DOJITU-SHOKUDO              PIC 9(04).
               05  WRK-SKJ-DOJITU-ASA                  PIC 9(04).
               05  WRK-SKJ-DOJITU-HIRU                 PIC 9(04).
               05  WRK-SKJ-DOJITU-YORU                 PIC 9(04).
      *
       01  WRK-NYULIFE-TABLE.
           03  WRK-NYULIFE-AREA                        OCCURS   31.
               05  WRK-NYULIFE-KBN                     PIC X(01).
      *
       01  WRK-SKJHKN-TABLE.
           03  WRK-SKJHKN-AREA                         OCCURS   31.
               05  WRK-SKJHKN-HKNCOMBI                 PIC X(04).
               05  WRK-SKJHKN-HKNCOMBI-DOU             PIC X(04).
      *
       01  WRK-TENTEKI2-TABLE.
           03  WRK-TENTEKI2-HENKYAKU-AREA.
      *
               05  WRK-TENTEKI2-ZAINUM                 PIC 9(08)
                                                       OCCURS   40.
      *
               05  WRK-TENTEKI2-T-AREA                 OCCURS   31.
                 06  WRK-TENTEKI2-T-SRYKA              PIC X(02).
                 06  WRK-TENTEKI2-T-MEISAISU           PIC 9(03).
                 06  WRK-TENTEKI2-T-ZAITEN             PIC 9(07).
                 06  WRK-TENTEKI2-T-SRYCDTOTAL         PIC 9(13).
                 06  WRK-TENTEKI2-T-SRYCD-X.
                   07  WRK-TENTEKI2-T-SRYCD-X-OCC      OCCURS    5.
                     08  WRK-TENTEKI2-T-SRYCD            PIC X(09).
                     08  WRK-TENTEKI2-T-COM-MEISAISU     PIC 9(03).
                     08  WRK-TENTEKI2-T-COM-AREA       OCCURS    5.
                         10  WRK-TENTEKI2-T-COM-FLG      PIC X(01).
                         10  WRK-TENTEKI2-T-COM-SRYCD    PIC X(09).
                         10  WRK-TENTEKI2-T-COM-MOJI     PIC X(100).
               05  WRK-TENTEKI2-T-ERRCD                PIC 9(01).
      *
               05  WRK-TENTEKI2-C-AREA                 OCCURS   31.
                 06  WRK-TENTEKI2-C-SRYKA              PIC X(02).
                 06  WRK-TENTEKI2-C-MEISAISU           PIC 9(03).
                 06  WRK-TENTEKI2-C-ZAITEN             PIC 9(07).
                 06  WRK-TENTEKI2-C-SRYCDTOTAL         PIC 9(13).
                 06  WRK-TENTEKI2-C-SRYCD-X.
                   07  WRK-TENTEKI2-C-SRYCD-X-OCC      OCCURS    5.
                     08  WRK-TENTEKI2-C-SRYCD            PIC X(09).
                     08  WRK-TENTEKI2-C-COM-MEISAISU     PIC 9(03).
                     08  WRK-TENTEKI2-C-COM-AREA       OCCURS    5.
                         10  WRK-TENTEKI2-C-COM-FLG      PIC X(01).
                         10  WRK-TENTEKI2-C-COM-SRYCD    PIC X(09).
                         10  WRK-TENTEKI2-C-COM-MOJI     PIC X(100).
               05  WRK-TENTEKI2-C-ERRCD                PIC 9(01).
      *
       01  WRK-ORCSTEN-TABLE.
           03  WRK-ORCSTEN-T                           OCCURS  350.
               05  WRK-ORCSTEN-SRYCD                   PIC X(09).
               05  WRK-ORCSTEN-SURYO                   PIC 9(05)V9(05).
               05  WRK-ORCSTEN-COM-MEI                 PIC X(80).
           03  WRK-ORCSTEN-T2                          OCCURS   31.
               05  WRK-ORCSTEN-DAY                     PIC 9(03).
               05  WRK-ORCSTEN-KIZ-SRYKA               PIC X(02).
           03  WRK-ORCSTEN-ZAIKAISU                    PIC 9(04).
      *
      *    ±°≥∞ΩË ˝∞Ï»ÃÃæ ‘Ω∏æ Û¬‡»ÚŒŒ∞Ë
       01  WRK-ORCSGETGE-OUT-AREA.
           03  WRK-ORCSGETGE-OUT-TABLE                 OCCURS  50.
               05  WRK-ORCSGETGE-YAKKAKJNCD            PIC X(12).
               05  WRK-ORCSGETGE-GENERIC-KBN           PIC X(01).
               05  WRK-ORCSGETGE-GENERICNAME           PIC X(200).
      *
      *    ª‹¿ﬂ¥Ω‡æ Û¬‡»ÚÕ—ŒŒ∞Ë
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10062                        PIC X(610).
               05  TBL-SYS10067                        PIC X(610).
      *    ∞ÂŒ≈µ°¥ÿ° ∆˛±°¥À‹°À¬‡»ÚÕ—ŒŒ∞Ë
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC                         OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *    ƒ¢…º ‘Ω∏∂Ë ¨æ Û¬‡»ÚÕ—ŒŒ∞Ë
       01  TBL-SYS1030-T.
           03  TBL-SYS1030-OCC                         OCCURS  100.
               05  TBL-SYS1030                         PIC X(610).
      *
      *    ∞Ïª˛ŒŒ∞Ë
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM  PIC X(06).
               05  WRK-PARA-TERMID PIC X(16).
               05  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-JOBID  PIC 9(07).
               05  WRK-PARA-SHELLID
                                   PIC X(08).
           03  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
           03  WRK-SYS-SRYYMD      PIC X(08).
      *
           03  WRK-ZENKAKU         PIC X(80).
           03  WRK-NUM.
               05  WRK-NUM-X       PIC X(10).
               05  FILLER          PIC X(70).
           03  WRK-NUM-9           PIC 9(10). 
           03  WRK-STIDX           PIC 9(02).
           03  WRK-EDIDX           PIC 9(02).
           03  WRK-SRYCD-X.
               05  WRK-SRYCD       PIC 9(09).
           03  WRK-SRYCD2-X.
               05  WRK-SRYCD2      PIC 9(09).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-ZAINUM-CNT      PIC 9(03).
           03  WRK-ZAINUM-CNT2     PIC 9(03).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-RENNUM2         PIC 9(02).
           03  WRK-TANICD          PIC 9(03).
           03  WRK-TANIMOJISU      PIC 9(01).
           03  WRK-TANINAME        PIC X(24).
      *
           03  WRK-SRYSURYO        PIC 9(05)V9(05).
           03  WRK-SRYSURYO2       PIC 9(05).
           03  WRK-SRYSURYO3       PIC 9(05).
           03  WRK-KZI058-TEN      PIC 9(09)V99.
           03  WRK-KZI058-TANICD   PIC 9(03).
           03  WRK-KZI058-TANIMOJISU
                                   PIC 9(01).
           03  WRK-KZI058-TANINAME PIC X(24).
      *
           03  WRK-FILMTEN         PIC 9(07)V9(02).
           03  WRK-FILMTEN2        PIC 9(07)V9(02).
      *
           03  WRK-SRYYM-5000      PIC X(06).
      *
           03  WRK-SYS5000-COUNT   PIC 9(02).
      *
           03  WRK-HYOKETU-TEN     PIC 9(09)V99.
           03  WRK-HYOKETU-SRYCD   PIC 9(09).
           03  WRK-KSN-SRYCD       PIC 9(09).
      *
           03  WRK-NYUTENTTLKBN    PIC 9(03).
      *
           03  WRK-KHNTEN          PIC 9(08)V9(5).
           03  WRK-KHNTEN-MOTO     PIC 9(08).
           03  WRK-KHNTEN-GEN      PIC 9(08)V9(5).
      *****03  WRK-KHN-ZAITEN      PIC 9(08)V9(5).
           03  WRK-KHN-ZAITEN-MOTO PIC 9(08).
           03  WRK-KHN-ZAITEN-GEN  PIC 9(08)V9(5).
           03  WRK-KSNTEN          PIC S9(08).
           03  WRK-ZAITEN-W        PIC 9(08).
           03  WRK-KANGOGEN-SRYCD-W
                                   PIC 9(09).
           03  WRK-GAIHAKU-SRYCD   PIC X(09).
           03  WRK-GAIHAKU-KHNTEN  PIC 9(08).
           03  WRK-NYUKHNKSNKBN    PIC 9(03).
           03  WRK-CHUKSNCD        PIC X(04).
           03  WRK-NYUKHNKBN       PIC 9(03).
           03  WRK-NYUINKHN        PIC X(09).
      *
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO   PIC 9(01)
                                      OCCURS 31.
      *
           03  WRK-SISETU613       PIC 9(01).
           03  WRK-SISETU695       PIC 9(01).
           03  WRK-SISETU3055      PIC 9(01).
           03  WRK-SISETU3056      PIC 9(01).
           03  WRK-SISETU3057      PIC 9(01).
           03  WRK-SISETU3058      PIC 9(01).
           03  WRK-SISETU3168      PIC 9(01).
           03  WRK-SISETU3169      PIC 9(01).
      *
           03  WRK-RYOYOTEATE      PIC 9(07).
      *
      *    øÙª˙»æ≥—¡¥≥— —¥πÕ—
           03  WRK-CHAR-IDX PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(20).
               05  WRK-CHAR-N                  PIC X(40).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *
      *    √±≤¡ ‘Ω∏Õ—
           03  WRK-TANKA                       PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA.
               05  WRK-TANKA-S1                PIC 9(09).
               05  WRK-TANKA-S2                PIC 9(02).
           03  WRK-TANKA-X.
               05  WRK-TANKA-Z1                PIC ZZZZZZZZ9.
               05  WRK-TANKA-Z2                PIC X(01).
               05  WRK-TANKA-Z3                PIC ZZ.
      *
      ***  03  WRK-FUKA-KINGAKU                PIC 9(09).
           03  WRK-FUKA-KINGAKU                PIC 9(13)V9(05).
           03  WRK-FUKA-VALUE                  PIC 9(01).
      *
           03  WRK-TENTEKI2-SRYCD-X.
               05  WRK-TENTEKI2-SRYCD-X-OCC        OCCURS    5.
                 06  WRK-TENTEKI2-SRYCD            PIC X(09).
                 06  WRK-TENTEKI2-COM-MEISAISU     PIC 9(03).
                 06  WRK-TENTEKI2-COM-AREA         OCCURS    5.
                     08  WRK-TENTEKI2-COM-FLG      PIC X(01).
                     08  WRK-TENTEKI2-COM-SRYCD    PIC X(09).
                     08  WRK-TENTEKI2-COM-MOJI     PIC X(100).
      *
           03  WRK-TENTEKI2-SRYCD2-X.
               05  WRK-TENTEKI2-SRYCD2-X-OCC       OCCURS  100.
                 06  WRK-TENTEKI2-SRYCD2           PIC X(09).
                 06  WRK-TENTEKI2-COM-FLG2         PIC X(01).
                 06  WRK-TENTEKI2-COM-MOJI2        PIC X(100).
      *
           03  WRK-SYOKUJIKEI                  PIC  9(07).
      *
           03  WRK-STR                         PIC  X(200).
           03  WRK-LEN                         PIC  9(05).
           03  WRK-SHO                         PIC  9(05).
           03  WRK-AMARI                       PIC  9(05).
      *
           03  WRK-TOTALTEN1                   PIC  9(08).
           03  WRK-TOTALTEN2                   PIC  9(08).
      *
           03  WRK-COL                         PIC  9(03).
      *
           03  WRK-RIHA-YMD                    PIC  X(07).
           03  WRK-RIHA-YMD-SEI                PIC  X(08).
      *
           03  WRK-1030-KOUHATUKA              PIC  X(01).
      *
           03  WRK-NYUSAI-SRYKBN               PIC  X(02).
      *
      *    øÙŒÃ ‘Ω∏Õ—
           03  WRK-SURYOKETA                   PIC  9(02).
           03  WRK-SURYOKETA1                  PIC  9(02).
           03  WRK-SURYOKETA2                  PIC  9(02).
           03  WRK-SURYOST                     PIC  9(02).
           03  WRK-SURYO                       PIC  9(05)V9(05).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1                PIC  9(05).
               05  WRK-SURYO-S2                PIC  9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                PIC  ZZZZZ.
               05  WRK-SURYO-Z2                PIC  X(01).
               05  WRK-SURYO-Z3                PIC  ZZZZZ.
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1                PIC  X(10).
               05  WRK-SURYO-N2                PIC  X(02).
               05  WRK-SURYO-N3                PIC  X(10).
           03  WRK-SRYSURYOW                   PIC  X(24).
      *
           03  WRK-KEISAN2                     PIC  9(08).
           03  WRK-TENSU-KEISAN2               PIC  9(08)V9(5).
      *
      *    ∫ﬁ§Œø«Œ≈∆‚Õ∆•¡•ß•√•Ø•∆°º•÷•Î
       01  TBL-ZAICHECK-AREA.
      *    ° •µ°À
           03  FILLER          PIC X(09)   VALUE    "820000046".
      *    ° ∏°¡™°À
           03  FILLER          PIC X(09)   VALUE    "820000053".
      *    ° •Í•œ¡™°À
           03  FILLER          PIC X(09)   VALUE    "820000054".
      *    ° ¿∫¡™°À
           03  FILLER          PIC X(09)   VALUE    "820000055".
      *
       01  TBL-ZAICHECK-R      REDEFINES   TBL-ZAICHECK-AREA.
           03  TBL-ZAICHECK    PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAICHECK-MAX    PIC 9(02)   VALUE     4.
      *
      *    ª¿¡« 
           COPY    "CMSANSOTBL.INC".
      *
      *   ∆¸À‹∏ÏøÙª˙
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "£±£≤£≥£¥£µ£∂£∑£∏£π£∞".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
       01  TBL-SUJI-AREA-X.
           03  FILLER          PIC X(10)   VALUE
              "1234567890".
       01  TBL-SUJI-X-R        REDEFINES   TBL-SUJI-AREA-X.
           03  TBL-SUJI-X      PIC X(01)   OCCURS   10.
      *
      *    ≤˛π‘•≥°º•…
       01  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    ≈¿øÙ•ﬁ•π•ø¬‡»Ú•Ô°º•Ø
           COPY    "CPTTNS.INC".
      *
       01  WRK-RECE200-REC. 
           COPY    "CPRCF200.INC"  REPLACING  //RECE200//
                                   BY         //WRK-RECE200//.
      *
       01  WRK-RECE200G-REC. 
           COPY    "CPRCF200.INC"  REPLACING  //RECE200//
                                   BY         //WRK-RECE200G//.
      *
       01  WRK-RECE200C-REC. 
           COPY    "CPRCF200.INC"  REPLACING  //RECE200//
                                   BY         //WRK-RECE200C//.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    •®•È°º•’•°•§•ÎÃææŒŒŒ∞Ë 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME∆‚§ŒBASENAME≥´ªœ∞Ã√÷
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *****************************************************************
      *    •’•°•§•Î•Ï•§•¢•¶•»
      *****************************************************************
      *
      *    •∏•Á•÷¥…Õ˝•ﬁ•π•ø
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ø«Œ≈≤Ò∑◊
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ø«Œ≈π‘∞Ÿ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ∆˛±°≤Ò∑◊
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ∆˛±°ø«Œ≈π‘∞Ÿ
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ≈¿øÙ
           COPY    "CPTENSU.INC".
      *
      *    º˝«º
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    •∑•π•∆•‡¥…Õ˝
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK1030.INC".
      *
      *    ¥µº‘•≥•·•Û•»
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *     ›∏±¡»§ﬂπÁ§Ô§ª
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    •µ•÷•◊•ÌÕ—ŒŒ∞Ë
      *****************************************************************
      *
      *    »æ≥—•¡•ß•√•Ø•µ•÷
           COPY    "CPORCSKANACHK.INC".
      *
      *    ∆¸…’ —¥π•µ•÷
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ¥µº‘»÷πÊ —¥π•µ•÷
           COPY    "CPORCSPTID.INC".
      *    øÙ√Õ —¥π•µ•÷
           COPY    "CPORCSNUM.INC".
      *   •∏•Á•÷¥…Õ˝£ƒ£¬¿©∏Ê•µ•÷
           COPY    "CPORCSJOBKANRI.INC".
      *    ∆˛±°≈¿≈©∞Ï≥Áππø∑•µ•÷
           COPY    "CPORCSTENTEK.INC".
      *    ∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€ºË∆¿•µ•÷
           COPY    "CPORCSNYULIFE.INC".
      *    ∆˛±°≈¿≈©ºÍµª≤√ªª§ﬁ§»§·•µ•÷
           COPY    "CPORCSTENTEK2.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ±°≥∞ΩË ˝∞Ï»ÃÃæ ‘Ω∏•µ•÷
           COPY    "CPORCSGETGENERIC.INC".
      *    ∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€ºË∆¿•µ•÷£≤
           COPY    "CPORCSNYULIFE2.INC".
      *    ∆˛±°Œ¡§»∞€§ §Îø©ªˆªªƒÍ ›∏±¡»§ﬂπÁ§Ô§ªºË∆¿•µ•÷
           COPY    "CPORCSSKJHKN.INC".
      *    ∆˛±°Œ¡Ã§ªªƒÍ∆¸ºË∆¿•µ•÷
           COPY    "CPORCSNYUINDAY.INC".
      *    ∆˛±°≈¿øÙ•ﬁ•π•ø…’≤√æ Û•µ•÷
           COPY    "CPORCSNTENSU.INC".
      *
      *    £ƒ£¬∏°∫˜
           COPY    "MCPDATA.INC".
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
      *    ºÁΩËÕ˝
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF
           .
      *****************************************************************
      *    ΩÈ¥¸°°ΩËÕ˝
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       TTNS-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE700"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE200PARA
      *
           MOVE    "RECE720"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE220PARA
      *
           MOVE    "RECE730"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE230PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        £ƒ£¬•™°º•◊•ÛΩËÕ˝
               PERFORM 100-DBOPEN-SEC
      *
      *        •π•∆•√•◊¥…Õ˝≥´ªœΩËÕ˝
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1220"      TO  JOB-PGID
               MOVE    "ø«Œ≈∂Ë ¨ Ã≈¿øÙΩ∏∑◊"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        ΩËÕ˝√Êªﬂ¿ﬂƒÍΩËÕ˝
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       ª‹¿ﬂ¥Ω‡æ Û
              MOVE     ZERO     TO     WRK-SISETU613
              MOVE     ZERO     TO     WRK-SISETU695
              MOVE     ZERO     TO     WRK-SISETU3055
              MOVE     ZERO     TO     WRK-SISETU3056
              MOVE     ZERO     TO     WRK-SISETU3057
              MOVE     ZERO     TO     WRK-SISETU3058
              MOVE     ZERO     TO     WRK-SISETU3168
              MOVE     ZERO     TO     WRK-SISETU3169
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1006"              TO  SYS-KANRICD
                MOVE    IDXA                TO  SYS-KBNCD
                MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
                MOVE    "01"                TO  SYS-STYUKYMD (7:2)
                MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
                MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM   900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key10"         TO  MCP-PATHNAME
                    PERFORM 960-KANRIMST-READ-SEC
                    IF      FLG-SYSKANRI     =   ZERO
                       MOVE   MCPDATA-REC   TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     2
                           MOVE  SYS-1006-SSTKIJUN(113)
                                            TO  WRK-SISETU613
                           MOVE  SYS-1006-SSTKIJUN(195)
                                            TO  WRK-SISETU695
                         WHEN     7
                           MOVE  SYS-1006-SSTKIJUN(55)
                                            TO  WRK-SISETU3055
                           MOVE  SYS-1006-SSTKIJUN(56)
                                            TO  WRK-SISETU3056
                           MOVE  SYS-1006-SSTKIJUN(57)
                                            TO  WRK-SISETU3057
                           MOVE  SYS-1006-SSTKIJUN(58)
                                            TO  WRK-SISETU3058
                           MOVE  SYS-1006-SSTKIJUN(168)
                                            TO  WRK-SISETU3168
                           MOVE  SYS-1006-SSTKIJUN(169)
                                            TO  WRK-SISETU3169
                       END-EVALUATE
                    END-IF
                END-IF
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM   900-CLOSE-SEC
               END-IF
              END-PERFORM
      *       ƒ¢…º ‘Ω∏∂Ë ¨æ Û
              MOVE    SPACE               TO  WRK-1030-KOUHATUKA
              IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1030"              TO  SYS-KANRICD
                MOVE    "*"                 TO  SYS-KBNCD
                MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
                MOVE    "01"                TO  SYS-STYUKYMD (7:2)
                MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
                MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM   900-DBSELECT-SEC
                IF      MCP-RC  =  ZERO
                  MOVE  "tbl_syskanri"      TO  MCP-TABLE
                  MOVE  "key10"             TO  MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF    FLG-SYSKANRI  =  ZERO
                    MOVE  MCPDATA-REC       TO  SYS-1030-REC
                    MOVE  SYS-1030-KOUHATUKA
                                            TO  WRK-1030-KOUHATUKA
                  END-IF
                END-IF
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM   900-CLOSE-SEC
              END-IF
           ELSE
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
                 MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                     MOVE    "tbl_syskanri"   TO  MCP-TABLE
                     MOVE    "key8"           TO  MCP-PATHNAME
                     PERFORM  960-KANRIMST-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE    MCPDATA-REC   TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     2
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10062(SYSKAN-IDX2)
                         WHEN     7
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10067(SYSKAN-IDX2)
                       END-EVALUATE
      *
                       MOVE    "tbl_syskanri"   TO  MCP-TABLE
                       MOVE    "key8"           TO  MCP-PATHNAME
                       PERFORM  960-KANRIMST-READ-SEC
      *
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
               END-IF
              END-PERFORM
      *
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1030-T
              INITIALIZE                      TBL-SYS1030-T
              IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1030"              TO  SYS-KANRICD
                MOVE    "*"                 TO  SYS-KBNCD
                MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key8"              TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"    TO  MCP-TABLE
                  MOVE    "key8"            TO  MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC     TO  SYS-1030-REC
                    ADD     1               TO  SYSKAN-IDX
                    MOVE    SYS-1030-REC    TO  TBL-SYS1030(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM 960-KANRIMST-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key8"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
              END-IF
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
             MOVE      WRK-PARA-SRYYM    TO  WRK-SRYYM-5000
             PERFORM   1001-SYS-5000-CHK-SEC
      *      ø©ªˆæ Û° •∑•π•∆•‡¥…Õ˝•∆°º•÷•Î§Ë§Í°À
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-STYUKYMD (7:2)
             MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key10"         TO  MCP-PATHNAME
                 PERFORM  960-KANRIMST-READ-SEC
                 IF      FLG-SYSKANRI        =   ZERO
                   MOVE     MCPDATA-REC  TO  SYS-5000-REC
                   PERFORM  1002-SYSKAN-HENSYU-SEC
                   IF        WRK-SYS5000-COUNT    =     ZERO
                      CONTINUE
                   ELSE
                      PERFORM  1003-SYS-5000-GET-SEC
                   END-IF
                 ELSE
                   MOVE "∞ÂŒ≈µ°¥ÿæ Û°›∆˛±°¥À‹§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£"
                                         TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                 END-IF
             ELSE
                  MOVE "∞ÂŒ≈µ°¥ÿæ Û°›∆˛±°¥À‹§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£"
                                         TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
           ELSE
      *
             MOVE     ZERO               TO  SYSKAN-IDX
             MOVE     SPACE              TO  TBL-SYS5000-T
             INITIALIZE                      TBL-SYS5000-T
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key8"          TO  MCP-PATHNAME
                 PERFORM 960-KANRIMST-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
      *
                   MOVE    MCPDATA-REC  TO  SYS-5000-REC
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM 960-KANRIMST-READ-SEC
      *
                 END-PERFORM
             ELSE
                  MOVE "∞ÂŒ≈µ°¥ÿæ Û°›∆˛±°¥À‹§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£"
                                         TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC     TO  MCPDATA-REC
           MOVE    "tbl_syskanri"   TO  MCP-TABLE
           MOVE    "key10"          TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI     =   ZERO
                   MOVE   MCPDATA-REC  TO  SYS-200501-REC
               ELSE
                   INITIALIZE              SYS-200501-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-200501-REC
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO-KSIKBN
                                    =   SPACE
               MOVE    "1"   TO  SYS-200501-COMMENT-NYUINRYO-KSIKBN
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                                    =   SPACE
               MOVE    "2"   TO  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
           END-IF
           IF      SYS-200501-INSPECT-GENERAL-KBN   =   SPACE
               MOVE    "0"   TO  SYS-200501-INSPECT-GENERAL-KBN
           END-IF
           IF      SYS-200501-15-YKZ-PRTKBN         =   SPACE
               MOVE    "0"   TO  SYS-200501-15-YKZ-PRTKBN
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           OPEN    OUTPUT              RECE220-FILE
           CLOSE                       RECE220-FILE
           OPEN    OUTPUT              RECE230-FILE
           CLOSE                       RECE230-FILE
      *
           OPEN    I-O                 RECE200-FILE
           OPEN    OUTPUT              RECE220-FILE
           OPEN    I-O                 RECE230-FILE
      *
           PERFORM RECE200-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ∑Ó∆‚§À £øÙ§Œ∞ÂŒ≈¥¸¥÷æ Û§¨§ §§§´∏°∫˜° ∞ÂŒ≈µ°¥ÿ∏∫ªª§Œ≥Œ«ß°À  *
      ******************************************************************
       1001-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-SYS5000-COUNT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1             TO  WRK-SYS5000-COUNT
                   MOVE   "tbl_syskanri" TO  MCP-TABLE
                   MOVE   "key11"        TO  MCP-PATHNAME
                   PERFORM 960-KANRIMST-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       1001-SYS-5000-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∑•π•´•Û§Œ ‘Ω∏
      *****************************************************************
       1002-SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM   VARYING   IDX8   FROM  1  BY  1
                      UNTIL    IDX8    >   31
              EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *         ∆˛±°ø©ªˆŒ≈Õ‹° £±°À
                WHEN         "197000110"
                    MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX8)
      *         ∆˛±°ø©ªˆŒ≈Õ‹° £≤°À
                WHEN         "197000710"
                    MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX8)
              END-EVALUATE
           END-PERFORM
      *
           .
       1002-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ∑Ó∆‚§Œ∞ÂŒ≈¥¸¥÷æ ÛºË∆¿° ∞ÂŒ≈µ°¥ÿ∏∫ªªæ Û°À                  *
      ******************************************************************
       1003-SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   MCPDATA-REC  TO  SYS-5000-REC
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
      *
                 PERFORM    VARYING  IDX8  FROM  IDV  BY  1
                             UNTIL   IDX8    >   31
                      EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                ∆˛±°ø©ªˆŒ≈Õ‹° £±°À
                       WHEN         "197000110"
                          MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX8)
      *                ∆˛±°ø©ªˆŒ≈Õ‹° £≤°À
                       WHEN         "197000710"
                          MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX8)
                      END-EVALUATE
                 END-PERFORM
      *
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key11"          TO  MCP-PATHNAME
                 PERFORM 960-KANRIMST-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       1003-SYS-5000-GET-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ºÁΩËÕ˝
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM             UNTIL   FLG-RECE200 =   1
                               OR      FLG-END     =   1
      *       ‘Ω∏ΩËÕ˝
             PERFORM 210-HENSYU-SEC
      *
             IF   FLG-RECE200  =  ZERO
               PERFORM RECE200-READ-SEC
               IF      CNT-IN-COMMIT   =   300
                   MOVE  ZERO              TO  CNT-IN-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
      *        ΩËÕ˝√Êªﬂ¿ﬂƒÍΩËÕ˝
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *     ‘Ω∏ΩËÕ˝
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE          WRK-DAY-TABLE
                               WRK-SKJ200604-TABLE
                               WRK-NYULIFE-TABLE
                               WRK-TENTEKI2-TABLE
                               WRK-SKJHKN-TABLE
      *
           INITIALIZE          WRK-RECE200G-REC
           INITIALIZE          WRK-RECE200C-REC
      *
           PERFORM    2111-TENTEKIMATOME-HENSYU-SEC
      *
           PERFORM    2101-RECE220-HENSYU-SEC
           PERFORM    2102-RECE220-HENSYU2-SEC
           PERFORM    2103-RECE220-HENSYU3-SEC
           PERFORM    2106-GENTEN-HENSYU-SEC
           PERFORM    2104-FTN-SET-SEC
           PERFORM    2105-GOKEI-KEISAN-SEC
      *
           PERFORM    RECE200-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿≈©ºÍµª≤√ªª§ﬁ§»§· ‘Ω∏
      *****************************************************************
       2111-TENTEKIMATOME-HENSYU-SEC      SECTION.
      *
           INITIALIZE                   ORCSTENTEK2AREA
           MOVE    RECE200-HOSPNUM  TO  LNK-ORCSTEN2-HOSPNUM
           MOVE    RECE200-PTID     TO  LNK-ORCSTEN2-PTID
           MOVE    RECE200-SRYYM    TO  LNK-ORCSTEN2-SRYYM
           MOVE    RECE200-HKNCOMBI TO  LNK-ORCSTEN2-HKNCOMBI
           CALL    "ORCSTENTEK2"        USING
                                        ORCSTENTEK2AREA
                                        SPA-AREA
           IF      LNK-ORCSTEN2-RC  =   ZERO
               IF     (LNK-ORCSTEN2-TENTEK-ERRCD  = ZERO)
               OR     (LNK-ORCSTEN2-CHUJOU-ERRCD  = ZERO)
                   MOVE  LNK-ORCSTEN2-OUT-AREA
                                    TO  WRK-TENTEKI2-HENKYAKU-AREA
               END-IF
           END-IF
           .
       2111-TENTEKIMATOME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£¥°À
      *****************************************************************
       2101-RECE220-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE200-HOSPNUM      TO      ACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN    TO      ACCT-NYUGAIKBN
           MOVE    RECE200-PTID         TO      ACCT-PTID
           MOVE    RECE200-SRYYM        TO      ACCT-SRYYM
           MOVE    RECE200-HKNCOMBI     TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *
                   MOVE   ZERO          TO  FLG-TENTEKIMATOME
                   IF    ACCT-SRYKBN  =  "33"
                      PERFORM   21015-TENTEKIMATOME-CHECK-SEC
                   END-IF
      *
                   PERFORM   21016-FLG-KURIA-SEC
      *
                   IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (FLG-TENTEKIMATOME  =    ZERO ) AND
                        (WRK-PARA-PRTKBN    =
                                    "1" OR "2" OR "3" ) AND
                        (ACCT-JIHOKBN       =    SPACE))
                   OR  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (FLG-TENTEKIMATOME  =    ZERO ) AND
                        (WRK-PARA-PRTKBN    =
                                           "4" OR "5" ))
                      IF  (ACCT-SRYKBN     = "97"     )
                      AND (ACCT-SRYCDTOTAL = 197000470
                                          OR 197001570
                                          OR 197001210)
                        IF  (RECE200-SRYYM  >= "200604" )
                        AND (ACCT-SRYCDTOTAL = 197000470
                                            OR 197001570)
                          PERFORM   21011-RECE220-TENSOU-SEC
                          PERFORM   21012-RECE230-HENSYU-SEC
                          IF    FLG-WRITE230  =   1
                            PERFORM RECE220-WRITE-SEC
                          END-IF
                        END-IF
                      ELSE
                        PERFORM   21011-RECE220-TENSOU-SEC
                        PERFORM   21012-RECE230-HENSYU-SEC
                        IF    FLG-WRITE230  =   1
                          PERFORM RECE220-WRITE-SEC
                        END-IF
                      END-IF
                   ELSE
                      IF   (ACCT-ZAIKAISU     NOT =  ZERO )
                      AND  (FLG-TENTEKIMATOME     =  ZERO )
                      AND  (ACCT-JIHOKBN          =  SPACE)
      **>             IF  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                  (FLG-TENTEKIMATOME  =    ZERO ) AND
      **>                  (WRK-PARA-PRTKBN    =
      **>                              "1" OR "2" OR "3" ) AND
      **>                  (ACCT-JIHOKBN       =    SPACE))
      **>             OR  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                  (FLG-TENTEKIMATOME  =    ZERO ) AND
      **>                  (WRK-PARA-PRTKBN    =
      **>                                     "4" OR "5" ))
                         IF ( ACCT-SRYKBN  NOT =
                              "12" AND "25" AND "93" AND "98")
                         OR ( ACCT-ZAIKBN  =   1             )
                            IF  (ACCT-SRYCDTOTAL(1:12)  =
                                               "000000999901")
                            AND (ACCT-MEISAISU      =   1    )
                                 CONTINUE
                            ELSE
                                 IF    ACCT-SRYKBN  =
                                       "60" OR "64" OR "80"
                                   PERFORM 21014-SINRYO-CHECK-SEC
                                 END-IF
                                 PERFORM   21011-RECE220-TENSOU-SEC
                                 PERFORM   21012-RECE230-HENSYU-SEC
                                 IF    FLG-WRITE230  =   1
                                   PERFORM RECE220-WRITE-SEC
                                 END-IF
                            END-IF
                         END-IF
                      ELSE
                         IF   (ACCT-ZAIKAISU     NOT =  ZERO )
                         AND  (FLG-TENTEKIMATOME     =  ZERO )
                         AND  (ACCT-JIHOKBN      NOT =  SPACE)
                         AND  (ACCT-ZAIKBN       =  3 OR 4   )
                                 PERFORM   21011-RECE220-TENSOU-SEC
                                 PERFORM   21012-RECE230-HENSYU-SEC
                                 IF    FLG-WRITE230  =   1
                                   PERFORM RECE220-WRITE-SEC
                                 END-IF
                         END-IF
                      END-IF
                   END-IF
                   MOVE    "tbl_sryacct"     TO      MCP-TABLE
                   MOVE    "key22"           TO      MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACCT SELECT ERR"
           END-IF
      *
           MOVE    "tbl_sryacct"      TO      MCP-TABLE
           MOVE    "key22"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2101-RECE220-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       21011-RECE220-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           MOVE    ACCT-SRYKBN          TO      RECE220-SRYKBN
           IF     (ACCT-SRYKBN      =  "90"     )
           AND    (ACCT-SRYCDTOTAL  =  190182690)
             MOVE    "92"               TO      RECE220-SRYKBN
           END-IF
           IF      ACCT-SRYKBN = "90" OR "92" OR "12"
                 MOVE    "6"            TO      RECE220-ZAISKBKBN
           ELSE 
              IF   ACCT-SRYKBN = "97"
      ***        MOVE    "4"            TO      RECE220-ZAISKBKBN
                 MOVE    "9"            TO      RECE220-ZAISKBKBN
                 IF   ACCT-ZAITEN  =  ZERO
                   MOVE  "A"            TO      RECE220-ZAISKBKBN
                 END-IF
              END-IF
           END-IF
           MOVE    ACCT-ZAINUM          TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           MOVE    ACCT-SRYKA           TO      RECE220-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE220-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE220-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE220-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE220-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE220-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE220-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE220-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE220-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE220-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE220-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE220-KIZAITEN
           IF      ACCT-SRYKBN    =  "70"
             COMPUTE  RECE220-SYUTEN1    =      RECE220-SYUTEN1
                                         -      RECE220-SYUTEN2
             COMPUTE  RECE220-KIZAITEN   =      RECE220-KIZAITEN
                                         +      RECE220-SYUTEN2
           END-IF
           MOVE    ACCT-DRCD            TO      RECE220-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE220-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE220-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >      31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE220-KAISU (IDX3 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE220-KAISU(1 IDX2)  NOT =  ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK      TO      RECE220-SAFLG
           END-IF
      *
           .
       21011-RECE220-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       21012-RECE230-HENSYU-SEC               SECTION.
      *
           INITIALIZE     WRK-ORCSGETGE-OUT-AREA
           MOVE    ZERO                TO      FLG-IRYORENKEI
           IF     (WRK-PARA-PRTKBN       =  "2" OR "3" OR "4" OR "5")
           AND    (SYS-200501-INSPECT-GENERAL-KBN  =  "1"           )
           AND    (RECE200-SRYYM        >=  "201204"                )
             PERFORM 210123-IRYORENKEI-HENSYU-SEC
           END-IF
           MOVE    ZERO                TO      IDX17
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE    ZERO         TO      FLG-ZANHAIKI
                                                FLG-FORMAL-KISAI
                   MOVE    ZERO         TO      WRK-RENNUM
                                                WRK-RENNUM2
                   MOVE    ZERO         TO      FLG-WRITE230X
                   PERFORM   210121-RECE230-TENSOU-SEC
                   IF   FLG-TENTEKI   =  ZERO
                   AND  FLG-WRITE230X =   1
                     IF  ACCT-JIHOKBN  =  SPACE
                       PERFORM 210122-RECE200-HENSYU-SEC
                     END-IF
                     PERFORM RECE230-WRITE-SEC
                     MOVE  1            TO      FLG-WRITE230
                   END-IF
                   IF   FLG-ZANHAIKI  =  1
                      PERFORM  RECE230-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  RECE230-REWRITE2-SEC
                   END-IF
                   MOVE    "tbl_sryact"      TO      MCP-TABLE
                   MOVE    "key9"            TO      MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"      TO      MCP-TABLE
           MOVE    "key9"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF     WRK-FILMTEN  NOT =  ZERO
                COMPUTE    RECE220-SYUTEN2  =  WRK-FILMTEN + 0.5
           END-IF
      *
           .
       21012-RECE230-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       210121-RECE230-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE230-REC
           INITIALIZE                           RECE230-REC
           MOVE    RECE200-KEY          TO      RECE230-KEY1
           MOVE    SRY-ZAINUM           TO      RECE230-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO      RECE230-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE230-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE230-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE230-SRYSYUKBN
           IF     (ACCT-SRYKBN      =  "90"     )
           AND    (ACCT-SRYCDTOTAL  =  190182690)
             MOVE    "92"               TO      RECE230-SRYKBN
             MOVE    "920"              TO      RECE230-SRYSYUKBN
           END-IF
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
            ADD    1                    TO      IDX17
      *
            MOVE   SRY-SRYCD(IDX2)      TO      WRK-SRYCD
            MOVE   RECE230-SRYYM        TO      WK-TAIHI-SRYYM
            MOVE   WK-TAIHI-DAY         TO      WK-TAIHI-SRYD
            PERFORM    900-TENSU-TBL-SEC
      *
            IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE220-MEISAISU  =  RECE220-MEISAISU
                                              -  1
            END-IF
      *
            IF       SRY-SRYCD(IDX2)   =  "099999909" OR "099999910" OR
                                          "099999921" OR "099999922" OR
                                          "099999923" OR "099999924" OR
                                          "099999925" OR "099999929" OR
                                          "099999930" OR "099999931"
                  MOVE    1     TO    FLG-HENSYU-NG
            END-IF
      *
            IF       SRY-SRYCD(IDX2)(1:5)  =
                     "09960" OR "09970" OR "09980" OR "09983"
                  MOVE    1     TO    FLG-HENSYU-NG
            END-IF
            IF       SRY-SRYCD(IDX2)  =  "099999935" OR "099999936" OR
                                         "099999937" OR "099999938" OR
                                         "099999939" OR "099999940" OR
                                         "099999941" OR "099999942" OR
                                         "099999943"
               IF    ACCT-JIHOKBN     =   SPACE
                  EVALUATE  SRY-SRYCD(IDX2)
                    WHEN     "099999935"
                      IF  RECE200-RIHA2-SINDAI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    1     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999936"
                      IF  RECE200-RIHA2-NOUKE    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    2     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999937"
                      IF  RECE200-RIHA2-UNDOKI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    3     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999938"
                      IF  RECE200-RIHA2-KOKYUKI  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    4     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999939"
                      IF  RECE200-RIHA2-SESSYOKU = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    5     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999940"
                      IF  RECE200-RIHA2-HAIYO    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    6     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999941"
                      IF  RECE200-RIHA2-SINDAI9  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    7     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999942"
                      IF  RECE200-RIHA2-KOKYUKI9 = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    8     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999943"
                      IF  RECE200-RIHA2-HAIYO9   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    9     TO    FLG-RIHA-OKIKAE
                      END-IF
                  END-EVALUATE
               ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
               END-IF
            END-IF
            IF       SRY-SRYCD(IDX2)   =  "099140004" OR "099400001"
                                       OR "099400002" OR "099400003"
                                       OR "099140011" OR "099140012"
                  MOVE    1     TO    FLG-HENSYU-NG
            END-IF
      *
            IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
            OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
            OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                    (TNS-TEN               =  ZERO )       )
            OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
            OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                           OR "0086"       )
            OR     ((SRY-SRYSYUKBN  = "330" OR "331" OR
                                      "332" OR "350" OR
                                      "352"               ) AND
                    ((SRY-SRYCD(IDX2)(1:1) =   "8"       ) OR
                     (SRY-SRYCD(IDX2)(1:3) =   "008"     )) AND
                    ( ACCT-ZAITEN      NOT =   ZERO       ) AND
                    ( ACCT-SYUTEN1         =   ZERO       ) AND
                    ( ACCT-ZAIKAISU    NOT =   ZERO       ) AND
                    ( ACCT-JIHOKBN         =   SPACE      ))
            OR     ( FLG-RIHA-OKIKAE   NOT =  ZERO         )
            OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
            OR     ( SRY-SRYCD(IDX2)       =
                     "099209902" OR "099209903" OR
                     "099209904" OR "099209905" OR
                     "099209906" OR "099209907" OR
                     "099209908" OR
                     "099999903" OR "099999904" OR
                     "099999905" OR "099999906" OR
                     "099999907" OR "099999908" OR
                     "099200101" OR "099999926" OR
                     "099999927" OR "099999928" OR
                     "099999934" OR
                     "099209811" OR "099209812" OR
                     "099209821" OR "099209822" OR
                     "099209711" OR "099209712" OR
                     "099209721" OR "099209722" OR
                     "099208101" OR
                     "840000136" OR "840000140" OR
                     "099114001" OR "099140001" OR
                     "099140003" OR "099409905" OR
                     "099409906" OR "099409907" OR
                     "099409908" OR "099409909" OR
                     "099200301"                           )
      *
                  PERFORM   2101213-MEISAISU-HENSYU-SEC
      *
            END-IF
      *
            IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                 "6" OR  "7" OR
                                                 "8"
      *
             IF  (  SRY-SRYSYUKBN            = "330" OR "331"  OR
                                               "332" OR "350"  OR
                                               "352" )
             AND (( SRY-SRYCD(IDX2)(1:3)     = "059"             ) OR
                  ( SRY-SRYCD(IDX2)(1:1)     = "7"               ) OR
                  ( SRY-SRYCD(IDX2)          = "099309901"       ) OR
                  ( SRY-SRYCD(IDX2)(1:1)     = "6"               ))
             AND (  ACCT-ZAITEN          NOT = ZERO  )
      *******AND (  ACCT-SYUTEN1             = ZERO  )
             AND (  ACCT-ZAIKAISU        NOT = ZERO  )
             AND (  ACCT-JIHOKBN             = SPACE )
      *
              MOVE    1                  TO    FLG-TENTEKI
      *
             ELSE
      *
              IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
              OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
              OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                             OR "0086"       )
              OR     ((SRY-SRYSYUKBN  = "330" OR "331" OR
                                        "332" OR "350" OR
                                        "352"               ) AND
                      ((SRY-SRYCD(IDX2)(1:1) =   "8"       ) OR
                       (SRY-SRYCD(IDX2)(1:3) =   "008"     )) AND
                      ( ACCT-ZAITEN      NOT =   ZERO       ) AND
                      ( ACCT-SYUTEN1         =   ZERO       ) AND
                      ( ACCT-ZAIKAISU    NOT =   ZERO       ) AND
                      ( ACCT-JIHOKBN         =   SPACE      ))
              OR     ( FLG-RIHA-OKIKAE   NOT =  ZERO         )
              OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
              OR     ( SRY-SRYCD(IDX2)       =
                       "099209902" OR "099209903" OR
                       "099209904" OR "099209905" OR
                       "099209906" OR "099209907" OR
                       "099209908" OR
                       "099999903" OR "099999904" OR
                       "099999905" OR "099999906" OR
                       "099999907" OR "099999908" OR
                       "099200101" OR "099999926" OR
                       "099999927" OR "099999928" OR
                       "099999934" OR
                       "099209811" OR "099209812" OR
                       "099209821" OR "099209822" OR
                       "099209711" OR "099209712" OR
                       "099209721" OR "099209722" OR
                       "099208101" OR
                       "840000136" OR "840000140" OR
                       "099114001" OR "099140001" OR
                       "099140003" OR "099409905" OR
                       "099409906" OR "099409907" OR
                       "099409908" OR "099409909" OR
                       "099200301"                           )
      *
               IF   FLG-RIHA-OKIKAE  NOT =  ZERO
                     PERFORM   2101215-RIHAYMD-HENSYU-SEC
               END-IF
      *
               IF   (ACCT-SRYKBN = "40" OR "50" OR "54" OR "60" OR
                                   "64" OR "70" OR "80"           )
               AND  (ACCT-KIZAITEN =  ZERO                        )
                 IF    SRY-SRYCD(IDX2)(1:3)  =  "059"
                 AND   TNS-TEN               =  ZERO
                   MOVE    1    TO    FLG-KIZ-PRTKBN
                 END-IF
               END-IF
      *
               IF      SRY-SRYCD(IDX2)    =     "099209811"
                  MOVE   "1"    TO    RECE220-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209812"
                  MOVE   "2"    TO    RECE220-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209821"
                  MOVE   "3"    TO    RECE220-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209822"
                  MOVE   "4"    TO    RECE220-JISANYAKKBN
               END-IF
      *
               IF      SRY-SRYCD(IDX2)    =     "099209711"
                  MOVE   "5"    TO    RECE220-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209712"
                  MOVE   "6"    TO    RECE220-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209721"
                  MOVE   "7"    TO    RECE220-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209722"
                  MOVE   "8"    TO    RECE220-JISANYAKKBN
               END-IF
      *
              ELSE
      *
               ADD     1                  TO    IDX4
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE230-SRYCD(IDX4)
      *
               IF      SRY-SRYKBN         =     "13"
               AND     ACCT-JIHOKBN       =     SPACE
                   IF   SRY-SRYCD(IDX2)   =  "113001810" OR "113001910"
                                          OR "113002010" OR "113700610"
                                          OR "113700710" OR "113700810"
                                          OR "113029010"
                        MOVE   "1"        TO    RECE200-BEDSUFLG
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)      =    "160177770"
                  MOVE   "1"              TO    RECE200-KENTAIKASANFLG
                  MOVE   99               TO    RECE220-KNSJISGRPKBN
               END-IF
               IF     RECE220-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE220-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN         =    "60" OR "64" )
               AND   ( TNS-KNSJISKBN      =     2           )
                   MOVE    TNS-KNSJISKBN        TO  RECE220-KNSJISKBN
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE220-ZAITEN    =     ZERO                 )
               AND   ( RECE220-SAFLG     =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE220-SAFLG
                  END-IF
               END-IF
      *
               IF      ACCT-JIHOKBN  NOT =     SPACE
                  IF     RECE220-SAFLG   NOT =  SPACE
                        MOVE    "3"       TO    RECE220-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   RECE220-TOYAKUFLG = SPACE
                          MOVE  "4"       TO    RECE220-TOYAKUFLG
                        END-IF
                    ELSE
                        MOVE    "3"       TO    RECE220-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE220-ZAITEN    =     ZERO         )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
               AND   ( ACCT-JIHOKBN      =     SPACE        )
                  IF     RECE220-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE220-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "212" OR "292" OR
                                               "222" OR "232" OR
                                               "295"
                          IF   RECE220-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE220-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE220-TOYAKUFLG
                        IF   (RECE230-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                   NOT =  SPACE)
                          MOVE    "1"  TO  RECE230-INGAIFLG-RECE(IDX4)
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO  RECE230-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO  RECE230-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO  RECE230-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE230-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE230-GENERICYUKOKETA(IDX4) =
                                       IDX15  /  2
                               MOVE    1   TO  IDX15
                            END-IF
                          END-PERFORM
                        END-IF
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14"  )
               AND   ( RECE220-ZAITEN    =     ZERO  )
               AND   ( ACCT-ZAIKBN   NOT =     1     )
               AND   ( ACCT-JIHOKBN      =     SPACE )
                  IF     RECE220-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE220-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          IF   RECE220-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE220-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE220-TOYAKUFLG
                        IF   (RECE230-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                   NOT =  SPACE)
                          MOVE    "1"  TO  RECE230-INGAIFLG-RECE(IDX4)
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO  RECE230-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO  RECE230-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO  RECE230-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE230-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE230-GENERICYUKOKETA(IDX4) =
                                       IDX15  /  2
                               MOVE    1   TO  IDX15
                            END-IF
                          END-PERFORM
                        END-IF
                    END-IF
                  END-IF
               END-IF
      *
               IF   (ACCT-SRYKBN = "40" OR "50" OR "54" OR "60" OR
                                   "64" OR "70" OR "80"           )
               AND  (ACCT-YKZTEN =  ZERO                          )
                 IF  ( SYS-200501-15-YKZ-PRTKBN  =  "0" )
                 OR  ((SYS-200501-15-YKZ-PRTKBN  =  "1") AND
                      (RECE200-SRYYM  <  "201204"      ))
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE230-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-YKZ-PRTKBN
                         PERFORM   2101213-MEISAISU-HENSYU-SEC
                   ELSE
                     IF  FLG-YKZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE230-RECEPTFLG(IDX4)
                         PERFORM   2101213-MEISAISU-HENSYU-SEC
                       ELSE
                         MOVE   ZERO   TO  FLG-YKZ-PRTKBN
                       END-IF
                     END-IF
                   END-IF
                 ELSE
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE220-YKZKISAIFLG
                   END-IF
                 END-IF
               END-IF
      *
               IF   (ACCT-SRYKBN = "40" OR "50" OR "54" OR "60" OR
                                   "64" OR "70" OR "80"           )
               AND  (ACCT-KIZAITEN =  ZERO                        )
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "7"
                   OR    SRY-SRYCD(IDX2)(1:3)  =  "058"
                     IF  ((ACCT-SRYKBN     = "70"                 ) AND
                          (TNS-DATAKBN     = "3"                  ))
                     OR  ((ACCT-SRYKBN     = "70"                 ) AND
                          (SRY-SRYCD(IDX2) = "799990070"          ))
                         MOVE   ZERO   TO  FLG-KIZ-PRTKBN
                     ELSE
                         MOVE   "1"    TO  RECE230-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-KIZ-PRTKBN
                         PERFORM   2101213-MEISAISU-HENSYU-SEC
                     END-IF
                   ELSE
                     IF  FLG-KIZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE230-RECEPTFLG(IDX4)
                         PERFORM   2101213-MEISAISU-HENSYU-SEC
                       ELSE
                         MOVE   ZERO   TO  FLG-KIZ-PRTKBN
                       END-IF
                     END-IF
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
               AND    FLG-YKZ-PRTKBN   =   ZERO
               AND    FLG-KIZ-PRTKBN   =   ZERO
      *          IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)   TO  WRK-SRYCD
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >    9
                     MOVE   SRY-SRYCD(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  RECE220-RUIKEITOTAL  =
                                RECE220-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       COMPUTE  WRK-FUKA-KINGAKU    =
                                WRK-FUKA-KINGAKU    +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE220-RUIKEITOTAL  =
                            RECE220-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >   10
                     MOVE   SRY-SRYSURYO(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX)
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE220-RUIKEITOTAL  =
                            RECE220-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2)  TO   RECE230-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2)  TO   RECE230-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE230-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)   TO   RECE230-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2)  TO   RECE230-INPUTNUM(IDX4)
      *
               IF      SRY-SRYKBN = "40" OR "50" OR "54" OR "60" OR "64"
                  IF   SRY-SRYCD(IDX2)(1:1)  =  "7"
                  AND  TNS-DATAKBN           =  "3"
                         COMPUTE WRK-FILMTEN2   =
                                (TNS-TEN  * SRY-SRYSURYO(IDX2) / 10)
                         IF   TNS-JGNTEN  NOT =  ZERO
                         AND  TNS-JGNTEN   <  WRK-FILMTEN2
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      TNS-JGNTEN
                         ELSE
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      WRK-FILMTEN2
                         END-IF
                  END-IF
               END-IF
      *
      *        Œ≈Õ‹√¥≈ˆºÍ≈ˆ
               IF      SRY-SRYCD(IDX2)  =  "199000510"
                  COMPUTE  WRK-RYOYOTEATE    =  WRK-RYOYOTEATE
                                             +  TNS-TEN
                  COMPUTE  RECE220-SYUTEN1   =  RECE220-SYUTEN1
                                             -  TNS-TEN
                  COMPUTE  RECE220-KIZAITEN  =  RECE220-KIZAITEN
                                             +  TNS-TEN
               END-IF
      *
               IF      TNS-SRYKBN         =     "80"
                   IF  TNS-NYUTENTTLKBN   =     130
                       MOVE   "13"        TO    RECE220-SRYKBN
                                                RECE230-SRYKBN
                   END-IF
               END-IF
      *
               IF      TNS-SRYKBN         =     "12"
                 PERFORM  VARYING  IDX31  FROM  1  BY  1
                           UNTIL   IDX31    >  31
                   IF   ACCT-DAY(1 IDX31)   NOT =   ZERO
                     IF   RECE200-NYUDAY-SRYKBN(IDX31) = "90"
                       MOVE   "90"        TO    RECE220-SRYKBN
                                                RECE230-SRYKBN
                                                WRK-NYUSAI-SRYKBN
                     ELSE
                       MOVE   "92"        TO    RECE220-SRYKBN
                                                RECE230-SRYKBN
                                                WRK-NYUSAI-SRYKBN
                     END-IF
                     MOVE     "1"         TO    RECE220-NYUSAIFLG
                     MOVE      31         TO    IDX31
                   END-IF
                 END-PERFORM
               END-IF
      *
               IF      ACCT-JIHOKBN       =     SPACE
                  PERFORM  2101211-RECE230-COMCNT-HEN-SEC
               ELSE
                  IF   RECE220-SRYKBN     =     "50"
                     MOVE   "1"   TO    RECE220-FLG-SHUJUTU-COMMENT
                  END-IF
                  IF   RECE220-SRYKBN     =     "54"
                     MOVE   "1"   TO    RECE220-FLG-MASUI-COMMENT
                  END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE230-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE230-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO
                                          +     0.99999
                  COMPUTE RECE230-SRYSURYO(IDX4)
                                          =     WRK-SRYSURYO2
                                          -     WRK-SRYSURYO
                  IF  RECE230-SRYSURYO(IDX4)  NOT = ZERO
                    MOVE  WRK-TANICD      TO    RECE230-TANICD(IDX4)
                    MOVE  WRK-TANIMOJISU  TO    RECE230-TANIMOJISU(IDX4)
                    MOVE  WRK-TANINAME    TO    RECE230-TANINAME(IDX4)
                    IF    RECE230-RENNUM  >  1
                    AND   IDX4            =  1
                      MOVE    1           TO    FLG-ZANHAIKI
                      COMPUTE  WRK-RENNUM  =  RECE230-RENNUM  -  1
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3
                    ELSE
                      MOVE WRK-SRYSURYO2  TO  RECE230-SRYSURYO(IDX4 - 1)
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE230-RECEPTFLG(IDX4)
                  END-IF
                END-IF
               ELSE
                  MOVE    SRY-SRYSURYO(IDX2)
                                          TO  WRK-SRYSURYO
                  IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  RECE230-TANICD(IDX4)
                                              WRK-TANICD
                  END-IF
                  MOVE    TNS-TANIMOJISU  TO  RECE230-TANIMOJISU(IDX4)
                                              WRK-TANIMOJISU
                  MOVE    TNS-TANINAME    TO  RECE230-TANINAME(IDX4)
                                              WRK-TANINAME
               END-IF
               MOVE    TNS-TENSIKIBETU    TO  RECE230-TENSIKIBETU(IDX4)
               MOVE    TNS-TEN            TO  RECE230-TEN(IDX4)
               MOVE    TNS-NYUTENTTLKBN   TO  RECE230-TENTTLKBN(IDX4)
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO  RECE230-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO  RECE230-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO  RECE230-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO  RECE230-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO  RECE230-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO  RECE230-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO  RECE230-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO  RECE230-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM
                                      TO RECE230-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE230-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE230-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM
                                      TO RECE230-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE230-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE230-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE230-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE230-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE230-CHUKSNTUBAN    (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE230-KENTAICOMMENT  (IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE230-KNSJISGRPKBN   (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE230-TOKUKIZAISBT1  (IDX4)
               MOVE    TNS-SANSOKBN   TO RECE230-SANSOKBN       (IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE200-SRYYM  <=  "201003"   )
                      MOVE   202      TO  RECE230-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE230-CDKBN-KOUBAN(IDX4)
               END-IF
               IF     TNS-SRYCD       =  "170016010"  OR
                                         "170020270"  OR  "170024470" OR
                                         "170024910"  OR  "170025010" OR
                                         "170025210"  OR  "170025310" OR
                                         "170025410"  OR  "170025510" OR
                                         "170025610"  OR  "170025710" OR
                                         "170025810"  OR  "170025910" OR
                                         "170026010"  OR  "170026110" OR
                                         "170026210"  OR  "170026310" OR
                                         "170026810"  OR
                                         "170000210"  OR  "170016910" OR
                                         "170017010"  OR  "170026710" OR
                                         "170028810"  OR
                                         "170035710"  OR  "170035810" OR
                                         "170035910"  OR  "170036010"
                      MOVE   203      TO  RECE230-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE230-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE200-SRYYM  >=  "201004"   )
                      MOVE   "1"      TO  RECE220-GAZOGEN
               END-IF
      *
               IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO   RECE230-NAME(IDX4)
                                                    RECE230-INPUTCOMENT
                                                                (IDX4)
      *
                   IF       SRY-SRYCD (IDX2)(1:1) = "8"
                       IF  (SRY-SRYCD (IDX2)(1:2) = "84" OR "85")
                       OR  (SRY-SRYCD (IDX2)(1:4) = "8311"      )
                         IF  PTCOM-INPUTCHI1  NOT =  SPACE
                           INITIALIZE              ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           STRING  PTCOM-INPUTCHI1 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI2 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI3 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI4 DELIMITED  BY  SPACE
                                   INTO   KANACHK-MAE-INPUT
                           END-STRING
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF      KANACHK-RC      =   ZERO
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO
                                           RECE230-INPUTCOMENT   (IDX4)
                           END-IF
                         ELSE
                               MOVE    SPACE       TO
                                           RECE230-INPUTCOMENT   (IDX4)
                         END-IF
                       ELSE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           MOVE    TNS-NAME        TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-MAX     >   ZERO )
                               COMPUTE IDXX    =   KANACHK-MAX +   1
                               MOVE    PTCOM-INPUTCOMENT (IDXX:)
                                                       TO
                                           RECE230-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE230-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
      ***          MOVE   "1"                  TO    RECE220-COMCDFLG
               ELSE
                   MOVE    TNS-NAME        TO    RECE230-NAME(IDX4)
                                                 RECE230-INPUTCOMENT
                                                             (IDX4)
                   MOVE    TNS-YUKOKETA    TO    RECE230-YUKOKETA(IDX4)
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                  IF  RECE230-SRYSURYO(IDX4)  NOT = ZERO
                    PERFORM  2101214-RECE230-SURYO-HEN-SEC
      *
                    MOVE    SPACE      TO  RECE230-NAME(IDX4)
                    MOVE    SPACE      TO  RECE230-INPUTCOMENT(IDX4)
      ***           STRING  TNS-NAME          DELIMITED  BY  " "
      ***                   "°°"              DELIMITED  BY  SIZE
      ***                   WRK-SRYSURYOW     DELIMITED  BY  " "
      ***                   RECE230-TANINAME(IDX4)
      ***                                     DELIMITED  BY  " "
      ***                   INTO              RECE230-NAME(IDX4)
      ***           END-STRING
                    STRING  "° ª»Õ—ŒÃ°°"      DELIMITED  BY  SIZE
                            WRK-SRYSURYOW     DELIMITED  BY  " "
                            RECE230-TANINAME(IDX4)
                                              DELIMITED  BY  " "
                            "°°ªƒŒÃ«—¥˛°À"    DELIMITED  BY  SIZE
                            INTO              RECE230-NAME(IDX4)
                    END-STRING
                    MOVE  RECE230-NAME(IDX4)
                                       TO  RECE230-INPUTCOMENT(IDX4)
      *
      ***           MOVE  "810000001"  TO  RECE230-SRYCD(IDX4)
                    MOVE  ZERO         TO  RECE230-SRYSURYO(IDX4)
                    MOVE  ZERO         TO  RECE230-TANICD(IDX4)
                    MOVE  ZERO         TO  RECE230-TANIMOJISU(IDX4)
                    MOVE  SPACE        TO  RECE230-TANINAME(IDX4)
                    MOVE  ZERO         TO  RECE230-YUKOKETA(IDX4)
                  END-IF
               END-IF
      *
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                        MOVE     "1"       TO    RECE220-COMCDFLG
               END-IF
      *
               IF  ( SRY-SRYKBN     =  "60" OR "64" )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-KENSA-COM  =   ZERO        )
                 IF  SRY-SRYCD(IDX2) = "840000085" OR "840000087"
                    CONTINUE
                 ELSE
                    PERFORM  VARYING  IDV2  FROM  1  BY  2
                              UNTIL  (IDV2           >   300)
                               OR    (FLG-KENSA-COM  =    1 )
                      IF  RECE230-NAME(IDX4)(IDV2:2)   =  "∆¸"
                        MOVE   SPACE  TO  RECE220-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE230-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE230-NAME(IDX4)
      *
                 MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                 IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                    MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                    MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                 END-IF
                 MOVE     ZERO            TO  RECE230-TEN(IDX4)
                 MOVE     ZERO            TO  RECE230-TANICD(IDX4)
                 MOVE     ZERO            TO  RECE230-TANIMOJISU(IDX4)
                 MOVE     SPACE           TO  RECE230-TANINAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE230-RENNUM  >  1
                    AND   IDX4            =  1
                      MOVE    1       TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE230-RENNUM  -  1
                    ELSE
                      MOVE   "1"      TO  RECE230-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  "1"         TO  RECE230-RECEHENFLG(IDX4)
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                      TO  RECE230-TEN(IDX4)
                    END-IF
                    IF   (WRK-KZI058-TANICD  NOT = ZERO)
                    AND  (RECE230-TANICD(IDX4)   = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                      TO  RECE230-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                      TO  RECE230-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                      TO  RECE230-TANINAME(IDX4)
                    END-IF
                 END-IF
               END-IF
      *
               IF  (SRY-SRYKBN         =  "99"       )
               AND (SRY-RENNUM         =   1         )
               AND (IDX2               =   1         )
               AND (SRY-SRYCD(IDX2)    =  "099999902")
                    MOVE      1         TO  FLG-TAHOKEN
               END-IF
               IF  (FLG-TAHOKEN           =   1              )
               AND (SRY-RENNUM            =   1              )
               AND (IDX2                  =   2              )
               AND (SRY-SRYCD(IDX2)(1:4)  =  "0082"          )
               AND (RECE230-NAME(IDX4)(1:14)
                                          =  " ›∏±ÃææŒ —ππ°ß")
                  MOVE     "1"          TO  RECE230-RECEPTFLG(IDX4)
      *
                  MOVE      ZERO        TO  IDW3
                  PERFORM   VARYING   IDW   FROM   300   BY  -1
                             UNTIL    IDW     <    15
                       IF  RECE230-NAME(IDX4)(IDW:1)  NOT =  SPACE
                           COMPUTE IDW6       =  (IDW / 2) - 7
                           MOVE    IDW        TO  IDW3
                           MOVE    1          TO  IDW
                       END-IF
                  END-PERFORM
                  IF     IDW3    NOT =  ZERO
                    MOVE  SPACE TO RECE230-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  RECE230-NAME(IDX4)(15:IDW4)
                                TO RECE230-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "§À§∆ø«ª°Œ¡ªªƒÍ∫—"
                                TO RECE230-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE230-YUKOKETA(IDX4 - 1) =
                              RECE230-YUKOKETA(IDX4 - 1) -
                              3                          +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE230-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   300   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE230-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE230-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        ∆√ƒÍ¥Ô∫‡° ≤Ë¡¸ø«√«§Œ•’•£•Î•‡∞ ≥∞°À§Œ§»§≠√±≤¡§Ú ‘Ω∏
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
                   IF   (  SRY-SRYCD (IDX2)         =
                                    "799990070"            OR
                                    "770020070"            OR
                                    "770030070"               )
      ***          OR   (  TNS-DATAKBN              =   "3"   )
                       CONTINUE
                   ELSE
                     IF      RECE230-TEN(IDX4)  =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 2101212-RECE230-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE230-YUKOKETA(IDX4)  *  2
                                                   +   3
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE230-NAME(IDX4)(IDW:)
                       COMPUTE RECE230-YUKOKETA(IDX4)
                                           =   RECE230-YUKOKETA(IDX4)
                                               +   1
                                               +
                                              (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE220-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE230-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE230-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE230-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE230-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE220-COMCDFLG
                     MOVE  "810000001"  TO  RECE230-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE230-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE230-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE230-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE230-TANINAME(IDX4)
               END-IF
      *
               IF   (RECE200-SRYYM           >= "201810")
               AND  (RECE230-SRYCD(IDX4)(1:4) = "8201"  )
                 MOVE   "1"         TO  RECE220-SENTAKUCOMFLG
               END-IF
      *
              END-IF
             END-IF
            END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE230X
           END-IF
      *
           .
       210121-RECE230-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»•≥•·•Û•»•’•È•∞ ‘Ω∏ΩËÕ˝
      *****************************************************************
       2101211-RECE230-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE220-SRYKBN      =     "11" OR "90" OR "92"
              IF   SRY-SRYCD(IDX2)    =  "111012970" OR "111013070"
                                      OR "111013170" OR "111013270"
                                      OR "111013370" OR "111013470"
                                      OR "111013570" OR "111013670"
                                      OR "112019990" OR "112020090"
                                      OR "112020190" OR "112020290"
                                      OR "112020390" OR "112020490"
                                      OR "112020590" OR "112020690"
                                      OR "112020790" OR "112020890"
                                      OR "112020990" OR "112021090"
                                      OR "112021190" OR "112021290"
                 EVALUATE   RECE200-NINPUFLG
                    WHEN        0
                            MOVE    1    TO    RECE200-NINPUFLG
                    WHEN        2
                            MOVE    3    TO    RECE200-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
           IF      RECE220-SRYKBN      =     "99"
              IF   SRY-SRYCD(IDX2)    =     "820100348"
                 EVALUATE   RECE200-NINPUFLG
                    WHEN        0
                            MOVE    2    TO    RECE200-NINPUFLG
                    WHEN        1
                            MOVE    3    TO    RECE200-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "11"
      *       ΩÈø«° ∆±∆¸ £øÙø«Œ≈≤ °À
              IF  (SRY-SRYCD(IDX2)    =     "111011810"
                                      OR    "111012610"
                                      OR    "111012810"         )
              AND (FLG-SYODOU-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE220-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE220-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "830000024"
                   MOVE  SPACE  TO    RECE220-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE220-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "40"
      *       ≤√£∏£∞˙° »Øæ…∆¸µ≠∫‹°À
              IF   RECE200-SRYYM      >=    "200604"
              AND  RECE200-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     "140048010"
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE200-SRYYM     >=    "201004"
      *         øÕπ©ø’¬°≈˘
                IF   SRY-SRYCD(IDX2)  =  "140007710" OR "140036710"
                                      OR "140051010" OR "140051110"
                                      OR "140008510" OR "140029850"
                                      OR "140008210" OR "140039210"
                                      OR "140052810" OR "140057810"
                                      OR "140057910" OR "140058010"
                                      OR "140058110" OR "140058210"
                                      OR "140058310" OR "140058410"
                                      OR "140058510" OR "140058610"
                                      OR "140059310" OR "140059410"
                                      OR "140059510"
                                      OR "140060210" OR "140060310"
                                      OR "140060410" OR "140060510"
                                      OR "140060610" OR "140060710"
                                      OR "140060810" OR "140060910"
                                      OR "140061010"
                        MOVE   "1"    TO    RECE220-FLG-JINZOU-COMMENT
                END-IF
      *         ∂…ΩÍ±¢∞µ ƒ∫øΩË√÷
                IF   RECE200-SRYYM     >=    "201204"
                  IF   SRY-SRYCD(IDX2)  =     "140052170"
                                          OR  "140052270"
                                          OR  "140052370"
                        MOVE   "1"    TO    RECE220-FLG-KYOKUIN-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =     "140052410"
                                          OR  "140051810"
                                          OR  "140051910"
                                          OR  "140052010"
                        MOVE   "1"    TO    RECE220-FLG-KYOKUIN-COMMENT
                  END-IF
                END-IF
              END-IF
      *       ºßµ§§À§Ë§ÎÁØÊ˘≈˘ª…∑„À°
              IF   RECE200-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "140055110"         )
                AND (FLG-BOKOHO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE220-FLG-BOKOHO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000085"
                   MOVE  SPACE  TO    RECE220-FLG-BOKOHO-COMMENT
                   MOVE    1    TO    FLG-BOKOHO-COM
                END-IF
              END-IF
      *       π‚µ§∞µª¿¡«º£Œ≈
              IF  (SRY-SRYCD(IDX2)    =     "140057510"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "1"   TO    RECE220-FLG-KIATU-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "140057610"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "2"   TO    RECE220-FLG-KIATU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000085"
                                      OR    "840000086"
                                      OR    "840000045"
                   MOVE  SPACE  TO    RECE220-FLG-KIATU-COMMENT
                   MOVE    1    TO    FLG-KIATU-COM
              END-IF
      *
      *       ø¥…‘¡¥§À¬–§π§Î±Û¿÷≥∞¿˛≤π«ÆŒ≈À°
              IF   RECE200-SRYYM     >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =    "140061210")
                AND (FLG-SCPRO-COM    =    ZERO       )
                   MOVE   "5"   TO    RECE220-FLG-SCPRO-COMMENT
                END-IF
              END-IF
              IF    SRY-SRYCD(IDX2)   =    "840000076"
                   MOVE  SPACE  TO    RECE220-FLG-SCPRO-COMMENT
                   MOVE    1    TO    FLG-SCPRO-COM
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "80"
      *       •Í•œ•”•Í
              IF   SRY-SRYCD(IDX2)    =     "180706910"   OR
                                            "180022210"   OR
                                            "180706810"   OR
                                            "180022110"   OR
                                            "180707110"   OR
                                            "180022410"   OR
                                            "180707010"   OR
                                            "180022310"   OR
                                            "180707310"   OR
                                            "180022610"   OR
                                            "180707210"   OR
                                            "180022510"   OR
                                            "180707510"   OR
                                            "180022810"   OR
                                            "180707410"   OR
                                            "180022710"   OR
                                            "180708210"   OR
                                            "180023710"   OR
                                            "180708410"   OR
                                            "180023910"   OR
                                            "180708110"   OR
                                            "180023610"   OR
                                            "180708310"   OR
                                            "180023810"   OR
                                            "180024910"   OR
                                            "180024810"   OR
                                            "180025110"   OR
                                            "180025010"   OR
                                            "180025510"   OR
                                            "180025410"
      ****              MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-RIHABIRI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "180027410"           OR
                                            "180027510"           OR
                                            "180029110"           OR
                                            "180029210"           OR
                                            "180028010"           OR
                                            "180028110"           OR
                                            "180029710"           OR
                                            "180029810"           OR
                                            "180027610"           OR
                                            "180027710"           OR
                                            "180029310"           OR
                                            "180029410"           OR
                                            "180030810"           OR
                                            "180032410"           OR
                                            "180032510"           OR
                                            "180032610"           OR
                                            "180033910"           OR
                                            "180034010"           OR
                                            "180034110"           OR
                                            "180034210"           OR
                                            "180034310"           OR
                                            "180034410"           OR
                                            "180044310"           OR
                                            "180044410"           OR
                                            "180044510"           OR
                                            "180050330"           OR
                                            "180050430"           OR
                                            "180050530"           OR
                                            "180050630"           OR
                                            "180050830"           OR
                                            "180051030"           OR
                                            "180027810"           OR
                                            "180027910"           OR
                                            "180029510"           OR
                                            "180029610"           OR
                                            "180032710"           OR
                                            "180034510"           OR
                                            "180034610"           OR
                                            "180034710"           OR
                                            "180045810"           OR
                                            "180045910"           OR
                                            "180046010"           OR
                                            "180052730"           OR
                                            "180052830"           OR
                                            "180052930"           OR
                                            "180053030"           OR
                                            "180053230"           OR
                                            "180053430"           OR
                                            "180028210"           OR
                                            "180028310"           OR
                                            "180028410"           OR
                                            "180044610"           OR
                                            "180044710"           OR
                                            "180044810"           OR
                                            "180044910"           OR
                                            "180045110"           OR
                                            "180045310"           OR
                                            "180051530"           OR
                                            "180051630"           OR
                                            "180051730"           OR
                                            "180051830"           OR
                                            "180052030"           OR
                                            "180052230"           OR
                                            "180033110"
      ****              MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-RIHABIRI-COMMENT
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     "180029910"
                        MOVE   "2"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     "180030010"
                        MOVE   "3"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     "180030510"
                        MOVE   "4"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     "180030610"
                        MOVE   "5"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     "180030110"
                        MOVE   "6"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     "180030210"
                        MOVE   "7"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     "180030310"
                        MOVE   "8"    TO    RECE220-RIHABIRI-COMMENT
                WHEN     "180030410"
                        MOVE   "9"    TO    RECE220-RIHABIRI-COMMENT
              END-EVALUATE
      *       •Í•œ•”•Í° »Øæ…∆¸µ≠∫‹°À
              IF   RECE200-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     "180027410"
                                      OR    "180027510"
                                      OR    "180029110"
                                      OR    "180029210"
                                      OR    "180029910"
                                      OR    "180030010"
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180028010"
                                      OR    "180028110"
                                      OR    "180029710"
                                      OR    "180029810"
                                      OR    "180030510"
                                      OR    "180030610"
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-KOKYUKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180027610"
                                      OR    "180027710"
                                      OR    "180029310"
                                      OR    "180029410"
                                      OR    "180030110"
                                      OR    "180030210"
                                      OR    "180030810"
                                      OR    "180032410"
                                      OR    "180032510"
                                      OR    "180032610"
                                      OR    "180033910"
                                      OR    "180034010"
                                      OR    "180034110"
                                      OR    "180034210"
                                      OR    "180034310"
                                      OR    "180034410"
                                      OR    "180044310"
                                      OR    "180044410"
                                      OR    "180044510"
                                      OR    "180050330"
                                      OR    "180050430"
                                      OR    "180050530"
                                      OR    "180050630"
                                      OR    "180050830"
                                      OR    "180051030"
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-NOUKE-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180027810"
                                      OR    "180027910"
                                      OR    "180029510"
                                      OR    "180029610"
                                      OR    "180030310"
                                      OR    "180030410"
                                      OR    "180032710"
                                      OR    "180034510"
                                      OR    "180034610"
                                      OR    "180034710"
                                      OR    "180045810"
                                      OR    "180045910"
                                      OR    "180046010"
                                      OR    "180052730"
                                      OR    "180052830"
                                      OR    "180052930"
                                      OR    "180053030"
                                      OR    "180053230"
                                      OR    "180053430"
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-UNDOKI-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180017910"        )
                AND (RECE200-SRYYM   <=     "201803"           )
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-NANBYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180028210"
                                      OR    "180028310"
                                      OR    "180028410"         )
                AND (RECE200-SRYYM   <=     "201803"            )
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180016610"
                                      OR    "180054310"
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-SESSYOKU-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180044610"
                                      OR    "180044710"
                                      OR    "180044810"
                                      OR    "180044910"
                                      OR    "180045110"
                                      OR    "180045310"
                                      OR    "180051530"
                                      OR    "180051630"
                                      OR    "180051730"
                                      OR    "180051830"
                                      OR    "180052030"
                                      OR    "180052230"
                        MOVE   "1"    TO    RECE200-COMCNTFLG
                        MOVE   "1"    TO    RECE220-HAIYO-COMMENT
                END-IF
              END-IF
      *
              IF   (RECE200-SRYYM   >=   "201604"             )
              AND  (SRY-SRYCD(IDX2)  =   "180033870"
                                     OR  "180030770"          )
                IF   RECE220-SINDAI-COMMENT = "1"
                    MOVE   "1"    TO    RECE220-SINDAI9-COMMENT
                END-IF
                IF   RECE220-KOKYUKI-COMMENT = "1"
                    MOVE   "1"    TO    RECE220-KOKYUKI9-COMMENT
                END-IF
                IF   RECE220-HAIYO-COMMENT = "1"
                    MOVE   "1"    TO    RECE220-HAIYO9-COMMENT
                END-IF
              END-IF
      *
      *       º¿¥µ Ã≈˘¿ÏÃÁ•◊•Ì•∞•È•‡≤√ªª
      *       ∑–∆¨≥∏ºßµ§ª…∑„Œ≈À°
              IF    (SRY-SRYCD(IDX2)  =    "180056570")
              AND   (FLG-SCPRO-COM    =    ZERO       )
                    MOVE   "2"    TO    RECE220-FLG-SCPRO-COMMENT
              END-IF
              IF   RECE200-SRYYM     >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =    "180058650")
                AND (FLG-SCPRO-COM    =    ZERO       )
                    MOVE   "4"    TO    RECE220-FLG-SCPRO-COMMENT
                END-IF
              END-IF
              IF    SRY-SRYCD(IDX2)   =    "840000076"
                    MOVE  SPACE   TO    RECE220-FLG-SCPRO-COMMENT
                    MOVE    1     TO    FLG-SCPRO-COM
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     "113010710"           OR
                                            "113010810"           OR
                                            "120001610"
                   MOVE   "1"   TO    RECE220-FLG-YAKUKAN-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "113003310"           OR
                                            "113014810"           OR
                                            "113014910"           OR
                                            "113017610"           OR
                                            "113017710"           OR
                                            "113017810"           OR
                                            "113017910"            )
              AND (RECE200-SRYYM      <=    "202003"               )
                   MOVE   "1"   TO    RECE220-FLG-NYUEIYO-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "190030310"           OR
                                            "190723610"         )
              AND (FLG-HOUMON-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE220-FLG-HOUMON-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000055"
                   MOVE  SPACE  TO    RECE220-FLG-HOUMON-COMMENT
                   MOVE    1    TO    FLG-HOUMON-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "180000710"
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000011"
                                      OR    "840000012"
                                      OR    "840000511"
                                      OR    "840000512"
                   MOVE  SPACE  TO    RECE220-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "113004310"
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000078"
                                      OR    "840000578"
                   MOVE  SPACE  TO    RECE220-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "113006270"
                   MOVE   "1"   TO    RECE220-FLG-TAIIN-COMMENT
              END-IF
      *
              IF   RECE200-SRYYM     >=    "201004"
      *         ≤∏ÓªŸ±Áœ¢∑»ªÿ∆≥Œ¡
                IF   SRY-SRYCD(IDX2)  =     "113011710"
                        MOVE   "1"    TO    RECE220-FLG-KAISIEN-COMMENT
                END-IF
              END-IF
              IF   RECE200-SRYYM     >=    "201004"
              AND  RECE200-SRYYM     <=    "201803"
      *         Ω∏√ƒ±…Õ‹ø©ªˆªÿ∆≥Œ¡
                IF   SRY-SRYCD(IDX2)  =     "113003410"
                        MOVE   "1"    TO    RECE220-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
      *       øÕπ©∆‚º™µ°¥Ôƒ¥¿∞≤√ªª
              IF   SRY-SRYCD(IDX2)   =  "113030670"
                   MOVE   "1"   TO    RECE220-FLG-JINNAI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)   =  "113030770"
                   MOVE   "2"   TO    RECE220-FLG-JINNAI-COMMENT
              END-IF
      *       ø¥¬°•⁄°º•π•·°º•´°ºªÿ∆≥¥…Õ˝Œ¡° √ÂÕ—∑øº´∆∞Ω¸∫Ÿ∆∞¥Ô°À
      *       ø¥¬°•⁄°º•π•·°º•´°ºªÿ∆≥¥…Õ˝Œ¡° •Ì°¶ø¢π˛∑øΩ¸∫Ÿ∆∞¥Ô∞‹π‘¥¸≤√ªª¥ﬁ§‡°À
      *       §¨§Û¥µº‘ªÿ∆≥¥…Õ˝Œ¡£≤°¶£≥
              IF   RECE200-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2) =  "113015010"           )
                AND (RECE200-SRYYM  <=  "201803"              )
                   MOVE   "1"   TO    RECE220-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015130"
                   MOVE   "2"   TO    RECE220-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015310"
                   MOVE   "1"   TO    RECE220-FLG-GANSIDO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015410"
                   MOVE   "2"   TO    RECE220-FLG-GANSIDO-COMMENT
                END-IF
              END-IF
      *       «”«¢º´Œ©ªÿ∆≥Œ¡
              IF   RECE200-SRYYM    >=     "201604"
              AND  RECE200-SRYYM    <=     "202003"
                IF  (SRY-SRYCD(IDX2)  =     "113022810"        )
                AND (FLG-HAIJRT-COM   =     ZERO               )
                        MOVE   "1"    TO    RECE220-FLG-HAIJRT-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "840000097"
                                      OR    "840000086"
                        MOVE  SPACE   TO    RECE220-FLG-HAIJRT-COMMENT
                        MOVE    1     TO    FLG-HAIJRT-COM
                END-IF
              END-IF
      *       ¬‡±°ª˛∂¶∆±ªÿ∆≥Œ¡£≤
              IF   RECE200-SRYYM    >=     "201804"
                IF  (SRY-SRYCD(IDX2)  =     "113008910"        )
                AND (FLG-TAIKYO-COM   =     ZERO               )
                        MOVE   "1"    TO    RECE220-FLG-TAIKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "840000110"
                        MOVE  SPACE   TO    RECE220-FLG-TAIKYO-COMMENT
                        MOVE    1     TO    FLG-TAIKYO-COM
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     "114008010"
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000073"
                   MOVE  SPACE  TO    RECE220-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "114009410"
                   MOVE   "1"   TO    RECE220-FLG-HINKAI-COMMENT
              END-IF
      *
      ***     IF  (SRY-SRYCD(IDX2)    =     "114000110"
      ***                             OR    "114001610"       )
      ***     AND (FLG-OSIN-COM       =     ZERO              )
      ***          MOVE   "1"   TO    RECE220-FLG-OSIN-COMMENT
      ***     END-IF
      ***     IF   SRY-SRYCD(IDX2)    =     "840000039"
      ***          MOVE  SPACE  TO    RECE220-FLG-OSIN-COMMENT
      ***          MOVE    1    TO    FLG-OSIN-COM
      ***     END-IF
      *
              IF   RECE200-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114000110"
                                    OR  "114001610"
                   IF    (WRK-SISETU3168  =  1)
                   OR    (WRK-SISETU3055  =  1)
                   OR    (WRK-SISETU3056  =  1)
                       MOVE   "1"   TO    RECE200-COMCNTFLG
                       MOVE   "1"   TO    RECE220-FLG-ZAISIEN-COMMENT
                   END-IF
                   IF  RECE220-FLG-ZAISIEN-COMMENT = SPACE
                     IF  (WRK-SISETU3169  =  1)
                     OR  (WRK-SISETU3057  =  1)
                     OR  (WRK-SISETU3058  =  1)
                       MOVE   "1"   TO    RECE200-COMCNTFLG
                       MOVE   "2"   TO    RECE220-FLG-ZAISIEN-COMMENT
                     END-IF
                   END-IF
                END-IF
              ELSE
                IF   WRK-SISETU613   =  1
                  IF   RECE200-SRYYM   >=  "200604"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE200-COMCNTFLG
                        MOVE   "1"   TO    RECE220-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                ELSE
                  IF   WRK-SISETU695   =  1
                    IF   RECE200-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE200-COMCNTFLG
                        MOVE   "2"   TO    RECE220-FLG-ZAISIEN-COMMENT
                      END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF   RECE200-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114020910"
                   MOVE   "1"   TO    RECE220-FLG-KAISYOKU-COMMENT
                END-IF
      ***       IF   SRY-SRYCD(IDX2) =  "114009710"
      ***          MOVE   "1"   TO    RECE220-FLG-YOUATU-COMMENT
      ***       END-IF
              END-IF
      *       ∫ﬂ¬¥µº‘∂¶∆±ø«Œ≈Œ¡
              IF   RECE200-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "114027610"
                                        OR    "114027710"
                                        OR    "114027810"
                                        OR    "114027910"         )
                AND (FLG-ZAIKYO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE220-FLG-ZAIKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000097"
                                        OR    "840000086"
                   MOVE  SPACE  TO    RECE220-FLG-ZAIKYO-COMMENT
                   MOVE    1    TO    FLG-ZAIKYO-COM
                END-IF
              END-IF
      *       ±Û≥÷•‚•À•ø•Í•Û•∞≤√ªª° ∫ﬂ¬ª¿¡«Œ≈À°ªÿ∆≥¥…Õ˝Œ¡° §Ω§Œ¬æ°À°À
      *       ±Û≥÷•‚•À•ø•Í•Û•∞≤√ªª° ∫ﬂ¬ª˝¬≥Õ€∞µ∏∆µ€Œ≈À°ªÿ∆≥¥…Õ˝Œ¡£≤°À
              IF   SRY-SRYCD(IDX2)    =     "114045470"
                   MOVE   "A"   TO    RECE220-FLG-SINPESU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114045670"
                   MOVE   "B"   TO    RECE220-FLG-SINPESU-COMMENT
              END-IF
      *       ∫ﬂ¬ª¿¡«Œ≈À°ªÿ∆≥¥…Õ˝Œ¡° §Ω§Œ¬æ°À
      *       ∫ﬂ¬ª˝¬≥Õ€∞µ∏∆µ€Œ≈À°ªÿ∆≥¥…Õ˝Œ¡£≤
              IF   SRY-SRYCD(IDX2)    =     "114003710"
                   MOVE   "A"   TO    RECE220-FLG-SINPESUR-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114040810"
                   MOVE   "B"   TO    RECE220-FLG-SINPESUR-COMMENT
              END-IF
      *       ∫ﬂ¬»æ∏«∑¡±…Õ‹∑–¥…±…Õ‹À°ªÿ∆≥¥…Õ˝Œ¡
              IF  (SRY-SRYCD(IDX2)    =     "114045510"          )
              AND (FLG-ZAIHANKO-COM   =     ZERO                 )
                   MOVE   "9"    TO   RECE220-FLG-ZAIHANKO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000097"
                   MOVE  SPACE   TO   RECE220-FLG-ZAIHANKO-COMMENT
                   MOVE    1     TO   FLG-ZAIHANKO-COM
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "50"
              IF   RECE220-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE220-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000042"
                   MOVE   "1"   TO    RECE220-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE200-SRYYM     >=    "201004"
            IF     RECE220-SRYKBN     =     "54"
              IF   RECE220-SYUTEN1    =     ZERO
                   MOVE   "1"   TO    RECE220-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "70"
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170011910" OR "170012110"
                                      OR "170015550" OR "170015210"
                                      OR "170020110" OR "170022110"
                                      OR "170022290" OR "170022390"
                                      OR "170022490" OR "170023010"
                                      OR "170023110" OR "170023210"
                                      OR "170023310" OR "170023410"
                                      OR "170023510"               )
              AND (RECE200-SRYYM      <     "201004"               )
                   MOVE   "1"   TO    RECE220-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170012110" OR "170015210"
                                      OR "170020110" OR "170028610"
                                      OR "170033410" OR "170033510"
                                      OR "170034910" OR "170035010")
              AND (RECE200-SRYYM      >=    "201004"               )
              AND (RECE200-SRYYM      <=    "202003"               )
                   MOVE   "1"   TO    RECE220-FLG-CTMRI-COMMENT
              END-IF
      *
              IF   RECE200-SRYYM  >= "201810"
                IF  (SRY-SRYCD(IDX2)  =     "170016010"        )
                AND (FLG-KINGAZ-COM   =     ZERO               )
                     MOVE   "1"    TO   RECE220-FLG-KINGAZ-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "820100129"
                     MOVE  SPACE   TO   RECE220-FLG-KINGAZ-COMMENT
                     MOVE    1     TO   FLG-KINGAZ-COM
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "80"
      ***     IF   SRY-SRYCD(IDX2)    =     "180018310"
      ***     AND  FLG-HOUSEI-COM     =     ZERO
      ***          MOVE   "1"   TO    RECE220-FLG-HOUSEI-COMMENT
      ***     END-IF
      ***     IF   SRY-SRYCD(IDX2)    =     "840000079"
      ***          MOVE  SPACE  TO    RECE220-FLG-HOUSEI-COMMENT
      ***          MOVE    1    TO    FLG-HOUSEI-COM
      ***     END-IF
      *
              IF   RECE200-SRYYM     >=    "201204"
                IF   SRY-SRYCD(IDX2)  =  "180039030" OR "180036030"
                                      OR "180039130" OR "180039230"
                                      OR "180039330" OR "180036130"
                                      OR "180039430" OR "180039530"
                                      OR "180048930" OR "180049030"
                   MOVE   "1"   TO    RECE220-FLG-SEIKEA-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "90"  OR  "92"
              IF   SRY-SRYCD(IDX2)    =     "190107070"
                   MOVE   "1"   TO    RECE220-FLG-KAKURI-COMMENT
              END-IF
      *
              IF   RECE200-SRYYM     >=    "201004"
      *         ±…Õ‹•µ•›°º•»•¡°º•‡≤√ªª
                IF   SRY-SRYCD(IDX2)  =     "190136810"
                        MOVE   "1"    TO    RECE220-FLG-EISAPO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190147110"
                        MOVE   "2"    TO    RECE220-FLG-EISAPO-COMMENT
                END-IF
      *         ∏∆µ€•±•¢•¡°º•‡≤√ªª
                IF   SRY-SRYCD(IDX2)  =     "190137910"
                        MOVE   "1"    TO    RECE220-FLG-KOKKEA-COMMENT
                END-IF
      *         Ω≈æ…º‘≤√ªª° ¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡°À
                IF   SRY-SRYCD(IDX2)  =     "190141370"
                        MOVE   "1"    TO    RECE220-FLG-JYUSYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190151470"
                        MOVE   "2"    TO    RECE220-FLG-JYUSYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190195770"
                        MOVE   "3"    TO    RECE220-FLG-JYUSYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190195870"
                        MOVE   "4"    TO    RECE220-FLG-JYUSYO-COMMENT
                END-IF
              END-IF
      *
              IF   RECE200-SRYYM     >=    "201204"
      *         ¿∫ø¿≤ •Í•®•æ•Û•¡°º•‡≤√ªª
                IF   SRY-SRYCD(IDX2)  =     "190147010"
                        MOVE   "1"    TO    RECE220-FLG-SEIRIE-COMMENT
                END-IF
      *         …¬≈ÔÃÙ∫ﬁ∂»Ã≥º¬ª‹≤√ªª
                IF   SRY-SRYCD(IDX2)  =     "190149110"
                        MOVE   "1"    TO    RECE220-FLG-BYOYAK-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190192110"
                        MOVE   "2"    TO    RECE220-FLG-BYOYAK-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190192210"
                        MOVE   "3"    TO    RECE220-FLG-BYOYAK-COMMENT
                END-IF
              END-IF
      *       ƒ∂Ω≈æ…ª˘° º‘°À∆˛±°ø«Œ≈≤√ªª°¶Ω‡ƒ∂Ω≈æ…ª˘° º‘°À∆˛±°ø«Œ≈≤√ªª
              IF   RECE200-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2) =  "190076570"
                                    OR  "190076670"
                                    OR  "190127510"
                                    OR  "190127610"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                  MOVE     ZERO       TO    FLG-CYOJKSN-OK
                  IF   RECE200-SRYYM  =  RECE200-NYUINYMD(1:6)
                    MOVE  RECE200-NYUINYMD(7:2)  TO  IDX25
                    PERFORM  VARYING  IDX26  FROM  1  BY  1
                              UNTIL   IDX26   >   31
                      IF  (ACCT-DAY(1 IDX26) NOT = ZERO )
                      AND (IDX26                >= IDX25)
                        MOVE     1    TO    FLG-CYOJKSN-OK
                      END-IF
                    END-PERFORM
                  ELSE
                        MOVE     1    TO    FLG-CYOJKSN-OK
                  END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2) =  "190076570"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                 IF  FLG-CYOJKSN-OK  =   1
                   MOVE   "1"   TO    RECE220-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE200-COMCNTFLG
                   EVALUATE  RECE200-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "X"    TO   RECE200-CYOJKSNFLG
                     WHEN     "Y"
                        MOVE   "Z"    TO   RECE200-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "5"   TO    RECE220-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2) =  "190076670"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                 IF  FLG-CYOJKSN-OK  =   1
                   MOVE   "2"   TO    RECE220-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE200-COMCNTFLG
                   EVALUATE  RECE200-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "Y"    TO   RECE200-CYOJKSNFLG
                     WHEN     "X"
                        MOVE   "Z"    TO   RECE200-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "6"   TO    RECE220-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2) =  "190127510"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                 IF  FLG-CYOJKSN-OK  =   1
                   MOVE   "3"   TO    RECE220-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE200-COMCNTFLG
                   EVALUATE  RECE200-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "X"    TO   RECE200-CYOJKSNFLG
                     WHEN     "Y"
                        MOVE   "Z"    TO   RECE200-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "7"   TO    RECE220-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2) =  "190127610"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                 IF  FLG-CYOJKSN-OK  =   1
                   MOVE   "4"   TO    RECE220-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE200-COMCNTFLG
                   EVALUATE  RECE200-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "Y"    TO   RECE200-CYOJKSNFLG
                     WHEN     "X"
                        MOVE   "Z"    TO   RECE200-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "8"   TO    RECE220-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF   SRY-SRYCD(IDX2) =  "840000056"
                                     OR "840000556"
                   MOVE    1    TO    FLG-CYOJKSN-COM
                   IF  RECE220-FLG-CYOJKSN-COMMENT =
                                           "1" OR "2" OR "3" OR "4"
                        MOVE   "1"    TO   RECE230-RECEPTFLG(IDX4)
                   END-IF
                END-IF
              END-IF
      *       «”«¢º´Œ©ªŸ±Á≤√ªª
              IF   RECE200-SRYYM     >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =     "190219010"        )
                AND (FLG-HAIJSK-COM   =     ZERO               )
                  MOVE   "1"    TO    RECE220-FLG-HAIJSK-COMMENT
                  PERFORM  VARYING  IDX40  FROM  1  BY  1
                            UNTIL   IDX40    >  31
                    IF  ACCT-DAY(1 IDX40) NOT = ZERO
                      IF  RECE200-HAIJSKYMD  =  SPACE
                        MOVE  RECE200-SRYYM TO  RECE200-HAIJSKYMD(1:6)
                        MOVE  IDX40         TO  RECE200-HAIJSKYMD(7:2)
                      ELSE
                        MOVE  RECE200-HAIJSKYMD(7:2)  TO  IDX41
                        IF    IDX40  <  IDX41
                          MOVE  IDX40       TO  RECE200-HAIJSKYMD(7:2)
                        END-IF
                      END-IF
                      MOVE    31    TO   IDX40
                    END-IF
                  END-PERFORM
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "840000097"
                                      OR    "840000086"
                  MOVE  SPACE   TO    RECE220-FLG-HAIJSK-COMMENT
                  MOVE    1     TO    FLG-HAIJSK-COM
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     "160162350"
                                      OR    "160181250"
                                      OR    "160116310"        )
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE220-FLG-KENSA-COMMENT
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160162350"
                   MOVE    1    TO    RECE200-KENSA-BNPFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160181250"
                   MOVE    1    TO    RECE200-KENSA-NTPROFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160116310"
                   MOVE    1    TO    RECE200-KENSA-ANPFLG
              END-IF
      *
              IF   RECE200-SRYYM  >= "201810"
                IF  (SRY-SRYCD(IDX2)  =     "160000210"
                                      OR    "160177770"         )
                AND (FLG-KENKSN-COM   =     ZERO                )
                     MOVE   "1"    TO   RECE220-FLG-KENKSN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "820100129"
                     MOVE  SPACE   TO   RECE220-FLG-KENKSN-COMMENT
                     MOVE    1     TO   FLG-KENKSN-COM
                END-IF
              END-IF
           END-IF
      *
           IF      RECE220-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                    OR "60" OR "70" OR "80" OR "64"
              SET     TBL-SANSO    TO      1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE220-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       2101211-RECE230-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ª¿¡«§Œ√±≤¡ ‘Ω∏ΩËÕ˝
      *****************************************************************
       2101212-RECE230-TANKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      *
      *    øÙª˙…Ωº®Õ— ‘Ω∏° »æ≥—°À
           MOVE    RECE230-TEN(IDX4)
                                   TO  WRK-TANKA
           MOVE    WRK-TANKA-S1    TO  WRK-TANKA-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY3   FROM    2   BY  -1
                     UNTIL     IDY3           =   ZERO
             IF  WRK-TANKA-S2(IDY3:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-TANKA-S2(IDY3:1)
                                         TO  WRK-TANKA-Z3(IDY3:1)
             END-IF
           END-PERFORM
           IF  WRK-TANKA-Z3          NOT =   SPACE
               MOVE    "."               TO  WRK-TANKA-Z2
           END-IF
      *
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-TANKA-X         TO  WRK-CHAR-X
           MOVE    12                  TO  WRK-CHAR-CNT
           MOVE    1                   TO  WRK-CHAR-IDX
           MOVE    ZERO                TO  WRK-CHAR-N-CNT
      *
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   WRK-CHAR-CNT
               IF      WRK-CHAR-X (IDW:1)  =   SPACE
                   CONTINUE
               ELSE
                   IF    WRK-CHAR-X (IDW:1)  IS  NUMERIC
                       IF      WRK-CHAR-X (IDW:1)  =   "0"
                           MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    WRK-CHAR-X (IDW:1)  TO  NUM-IDX
                           MOVE    TBL-SUJI (NUM-IDX)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF
                   ELSE
                       IF    WRK-CHAR-X (IDW:1)  =   "."
                           MOVE    "°•"   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    "  "   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
                                           WRK-CHAR-N-CNT
               END-IF
           END-PERFORM
      *
           IF      WRK-CHAR-N-CNT      >   ZERO
                   MOVE    "±ﬂ"        TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   ADD     2           TO  WRK-CHAR-N-CNT
           END-IF
      *
           .
       2101212-RECE230-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ã¿∫ŸøÙ≈˘§Œ ‘Ω∏
      *****************************************************************
       2101213-MEISAISU-HENSYU-SEC                 SECTION.
      *
           COMPUTE   RECE220-MEISAISU    =  RECE220-MEISAISU
                                         -  1
           MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
           COMPUTE   RECE220-SRYCDTOTAL  =  RECE220-SRYCDTOTAL
                                         -  WRK-SRYCD
           COMPUTE   RECE220-SURYOUTOTAL =  RECE220-SURYOUTOTAL
                                         -  SRY-SRYSURYO(IDX2)
      *
           .
       2101213-MEISAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ªƒŒÃ«—¥˛§ŒøÙŒÃ ‘Ω∏ΩËÕ˝
      *****************************************************************
       2101214-RECE230-SURYO-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      *
      *    øÙª˙…Ωº®Õ— ‘Ω∏° »æ≥—°À
      ***  MOVE    RECE230-SRYSURYO(IDX4) TO  WRK-SURYO
           MOVE    WRK-SRYSURYO           TO  WRK-SURYO
           MOVE    WRK-SURYO-S1           TO  WRK-SURYO-Z1
      *
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDW   FROM    5   BY  -1
                      UNTIL    IDW   =   ZERO
             IF  WRK-SURYO-S2(IDW:1) NOT =   ZERO
                 MOVE    1         TO  FLG-SP
             END-IF
             IF  FLG-SP  =   1
                 MOVE    WRK-SURYO-S2(IDW:1)
                                   TO  WRK-SURYO-Z3(IDW:1)
             END-IF
           END-PERFORM
           IF  WRK-SURYO-Z3   NOT =  SPACE
               MOVE    "."         TO  WRK-SURYO-Z2
           END-IF
      *    …Ωº® ∏ª˙øÙ•´•¶•Û•»
           PERFORM   VARYING   IDW   FROM    1   BY  1
                      UNTIL    IDW     >    11
             IF  WRK-SURYO-X(IDW:1)  NOT =  SPACE
                 IF  WRK-SURYOST           =   ZERO
                     MOVE    IDW   TO  WRK-SURYOST
                 END-IF
      *          øÙŒÃ∑ÂøÙ•´•¶•Û•»(£∂∑ÂÃ‹°ß•‘•Í•™•…)
                 ADD     1         TO  WRK-SURYOKETA
                 IF  IDW    <   6
                     ADD     1     TO  WRK-SURYOKETA1
                 END-IF
                 IF  IDW    >   6
                     ADD     1     TO  WRK-SURYOKETA2
                 END-IF
             END-IF
           END-PERFORM
           IF  WRK-SURYOST  =   6
               MOVE   "0"          TO  WRK-SURYO-Z1(5:1)
               MOVE    5           TO  WRK-SURYOST
               MOVE    1           TO  WRK-SURYOKETA1
               ADD     1           TO  WRK-SURYOKETA
           END-IF
      *    ¡¥≥— —¥π
           IF  WRK-SURYOKETA1   >   ZERO
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z1(WRK-SURYOST:WRK-SURYOKETA1)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA1  TO  WRK-CHAR-CNT
      *
               MOVE    1               TO  WRK-CHAR-IDX
               PERFORM  VARYING  IDW   FROM    1   BY  1
                         UNTIL   IDW     >   WRK-CHAR-CNT
                   IF    WRK-CHAR-X(IDW:1)  IS  NUMERIC
                     IF    WRK-CHAR-X(IDW:1)  =   "0"
                        MOVE  TBL-SUJI(10)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     ELSE
                        MOVE  WRK-CHAR-X(IDW:1)  TO  NUM-IDX
                        MOVE  TBL-SUJI(NUM-IDX)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     END-IF
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
               END-PERFORM
      *
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N1
           END-IF
           IF  WRK-SURYOKETA2   >   ZERO
               MOVE    "°•"            TO  WRK-SURYO-N2
      *
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z3(1:WRK-SURYOKETA2)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA2  TO  WRK-CHAR-CNT
      *
               MOVE    1               TO  WRK-CHAR-IDX
               PERFORM  VARYING  IDW   FROM    1   BY  1
                         UNTIL   IDW     >   WRK-CHAR-CNT
                   IF    WRK-CHAR-X(IDW:1)  IS  NUMERIC
                     IF    WRK-CHAR-X(IDW:1)  =   "0"
                        MOVE  TBL-SUJI(10)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     ELSE
                        MOVE  WRK-CHAR-X(IDW:1)  TO  NUM-IDX
                        MOVE  TBL-SUJI(NUM-IDX)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     END-IF
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
               END-PERFORM
      *
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N3
           END-IF
      *
           STRING      WRK-SURYO-N1    DELIMITED   BY  " "
                       WRK-SURYO-N2    DELIMITED   BY  " "
                       WRK-SURYO-N3    DELIMITED   BY  " "
                       INTO            WRK-SRYSURYOW
           END-STRING
      *
           .
       2101214-RECE230-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Í•œ≥´ªœ∆¸ ‘Ω∏
      *****************************************************************
       2101215-RIHAYMD-HENSYU-SEC                 SECTION.
      *
           MOVE      SPACE   TO  WRK-RIHA-YMD
           MOVE      SPACE   TO  WRK-RIHA-YMD-SEI
      *
           IF    SRY-SRYCD(IDX2) = "840000061" OR "840000561"
             MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
             MOVE    SRY-PTID             TO  PTCOM-PTID
             MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
             MOVE    SRY-SRYCD    (IDX2)  TO  PTCOM-SRYCD
             MOVE    SRY-INPUTNUM (IDX2)  TO  PTCOM-RENNUM
             PERFORM 950-PTCOM-READ-SEC
      *
             IF    SRY-SRYCD(IDX2) = "840000061"
               MOVE     "4"    TO    WRK-RIHA-YMD(1:1)
             END-IF
             IF    SRY-SRYCD(IDX2) = "840000561"
               MOVE     "5"    TO    WRK-RIHA-YMD(1:1)
             END-IF
             MOVE  PTCOM-INPUTCHI1(1:2)  TO  WRK-RIHA-YMD(2:2)
             MOVE  PTCOM-INPUTCHI2(1:2)  TO  WRK-RIHA-YMD(4:2)
             MOVE  PTCOM-INPUTCHI3(1:2)  TO  WRK-RIHA-YMD(6:2)
      *
             IF    WRK-RIHA-YMD  IS  NUMERIC
               INITIALIZE                   LNK-DAY1-AREA
               INITIALIZE                   STS-AREA-DAY
               MOVE    "12"           TO    LNK-DAY1-IRAI
               MOVE    WRK-RIHA-YMD   TO    LNK-DAY1-WYMD
               CALL    "ORCSDAY"      USING STS-AREA-DAY
                                            LNK-DAY1-AREA
               IF      STS-DAY-RC1    =     ZERO
                 MOVE  LNK-DAY1-YMD   TO  WRK-RIHA-YMD-SEI
               ELSE
                 MOVE  SPACE          TO  WRK-RIHA-YMD
               END-IF
             ELSE
               MOVE    SPACE          TO  WRK-RIHA-YMD
             END-IF
           END-IF
      *
           EVALUATE    FLG-RIHA-OKIKAE
             WHEN         1
               MOVE       1     TO  RECE200-RIHA2-SINDAI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-SINYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE200-RIHA2-SINYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-SINYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-SINYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         2
               MOVE       1     TO  RECE200-RIHA2-NOUKE
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-NOUYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE200-RIHA2-NOUYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-NOUYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-NOUYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         3
               MOVE       1     TO  RECE200-RIHA2-UNDOKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-UNDYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE200-RIHA2-UNDYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-UNDYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-UNDYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         4
               MOVE       1     TO  RECE200-RIHA2-KOKYUKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-KOKYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE200-RIHA2-KOKYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-KOKYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-KOKYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         5
               MOVE       1     TO  RECE200-RIHA2-SESSYOKU
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-SESYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE200-RIHA2-SESYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-SESYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-SESYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         6
               MOVE       1     TO  RECE200-RIHA2-HAIYO
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-HAIYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE200-RIHA2-HAIYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-HAIYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-HAIYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         7
               MOVE       1     TO  RECE200-RIHA2-SINDAI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-SIN9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE200-RIHA2-SIN9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-SIN9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-SIN9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         8
               MOVE       1     TO  RECE200-RIHA2-KOKYUKI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-KOK9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE200-RIHA2-KOK9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-KOK9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-KOK9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         9
               MOVE       1     TO  RECE200-RIHA2-HAIYO9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE200-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE200-RIHA2-HAI9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE200-RIHA2-HAI9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE200-RIHA2-HAI9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE200-RIHA2-HAI9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
           END-EVALUATE
      *
           .
       2101215-RIHAYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       210122-RECE200-HENSYU-SEC               SECTION.
      *
           MOVE      ZERO       TO     WK-ZAIKAISU
           MOVE      ZERO       TO     WK-ZAITEN
           MOVE      ZERO       TO     WK-SYUTEN
           MOVE      ZERO       TO     WK-KIZAITEN
           MOVE      ZERO       TO     WK-YKZTEN
           MOVE      ZERO       TO     WK-YKZTEN2
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF    SRY-SRYKBN  =  "31" OR "32" OR "33"
                           MOVE   ACCT-YKZTEN    TO   WK-YKZTEN2
                       END-IF
                       IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                            "40" OR "50" OR "54" OR
                                            "60" OR "70" OR "80" OR
                                            "14" OR "64"
                           MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                           COMPUTE  WK-YKZTEN  =  ACCT-KIZAITEN
                                               +  ACCT-YKZTEN
                           IF  SRY-SRYKBN  =  "70"
                             COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                 -  ACCT-SYUTEN2
                             COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                 +  ACCT-SYUTEN2
                           END-IF
                           IF  WRK-RYOYOTEATE  NOT =  ZERO
                             COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                 -  WRK-RYOYOTEATE
                             COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                 +  WRK-RYOYOTEATE
                           END-IF
                       END-IF
                   ELSE
                       IF   SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"  OR  "64" OR
                                            "12"
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
                           MOVE  ZERO          TO   WK-YKZTEN2
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
                     MOVE    ZERO     TO    WK-YKZTEN2
               END-IF
               IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)
                     PERFORM   2101221-RECE-LEFT-SET-SEC
               END-IF
               IF    SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "54"   OR
                                      "60"   OR   "70"   OR   "13"   OR
                                      "14"   OR   "64"   OR   "12"
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           .
      *
       210122-RECE200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       2101221-RECE-LEFT-SET-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE230-SRYYM       TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ΩÈø«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "11"
                   IF   TNS-CHUKSNTUBAN        =   "0"
                   AND  SRY-SRYCD(IDX3)(1:1)   =   "1"
                        ADD   WK-ZAIKAISU      TO   RECE200-KAISU (1)
                   END-IF
      *************£±°ßª˛¥÷≥∞°°£≤°ßøºÃÎ°°£≥°ßµŸ∆¸°°£¥°ßª˛¥÷≥∞°¶øºÃÎ
      *************£µ°ßª˛¥÷≥∞°¶µŸ∆¸°°£∂°ßµŸ∆¸°¶øºÃÎ
      *************£∑°ßª˛¥÷≥∞°¶µŸ∆¸°¶øºÃÎ
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             EVALUATE   RECE200-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "1"     TO   RECE200-SHOSHINTIME
                               WHEN   "1"
                                  CONTINUE
                               WHEN   "2"
                                  MOVE  "4"     TO   RECE200-SHOSHINTIME
                               WHEN   "3"
                                  MOVE  "5"     TO   RECE200-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  MOVE  "7"     TO   RECE200-SHOSHINTIME
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    4
                             EVALUATE   RECE200-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "2"     TO   RECE200-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "4"     TO   RECE200-SHOSHINTIME
                               WHEN   "2"
                                  CONTINUE
                               WHEN   "3"
                                  MOVE  "6"     TO   RECE200-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  MOVE  "7"     TO   RECE200-SHOSHINTIME
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    3
                             EVALUATE   RECE200-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "3"     TO   RECE200-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "5"     TO   RECE200-SHOSHINTIME
                               WHEN   "2"
                                  MOVE  "6"     TO   RECE200-SHOSHINTIME
                               WHEN   "3"
                                  CONTINUE
                               WHEN   "4"
                                  MOVE  "7"     TO   RECE200-SHOSHINTIME
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                     END-EVALUATE
                   END-IF
                   COMPUTE    RECE200-TENSU(1 1)  =
                              RECE200-TENSU(1 1)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
      *    ∫∆ø«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "12"
                   COMPUTE        RECE200-NYUSAI-TENSU  =
                                  RECE200-NYUSAI-TENSU  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   EVALUATE  WRK-NYUSAI-SRYKBN
                     WHEN      "90"
                       COMPUTE    RECE200-NYUKHNKEI2    =
                                  RECE200-NYUKHNKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                     WHEN      "92"
                       COMPUTE    RECE200-NYUTOKKEI2    =
                                  RECE200-NYUTOKKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-EVALUATE
      *    ∞Â≥ÿ¥…Õ˝§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "13"
                 IF  ACCT-ZAIKBN   =    2
                       COMPUTE    WRK-RECE200G-TENSU(2 1)  =
                                  WRK-RECE200G-TENSU(2 1)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                       COMPUTE    RECE200-TENSU(2 1)  =
                                  RECE200-TENSU(2 1)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
      *    ∫ﬂ¬§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                       COMPUTE    WRK-RECE200G-TENSU(3 1)  =
                                  WRK-RECE200G-TENSU(3 1)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                       COMPUTE    RECE200-TENSU(3 1)  =
                                  RECE200-TENSU(3 1)  +
                                 (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE    RECE200-TENSU(3 2)  =
                                  RECE200-TENSU(3 2)  +
                                 (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ≈ÍÃÙ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)     =    "630010002"
                           COMPUTE   RECE200-TENSU(4 2)  =
                                     RECE200-TENSU(4 2)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE200-KAISU(4)
                           COMPUTE   RECE200-TENSU(4 2)  =
                                     RECE200-TENSU(4 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    "099209901"
                           ADD   WK-ZAIKAISU   TO   RECE200-KAISU(4)
                           COMPUTE   RECE200-TENSU(4 2)  =
                                     RECE200-TENSU(4 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE200-KAISU(5)
                   COMPUTE   RECE200-TENSU(5 2)  =
                             RECE200-TENSU(5 2)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE200-KAISU(6)
                   COMPUTE   RECE200-TENSU(6 2)  =
                             RECE200-TENSU(6 2)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ƒ¥∫ﬁ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE200-KAISU(7)
                   END-IF
                   COMPUTE   RECE200-TENSU(7 1)  =
                             RECE200-TENSU(7 1)  +
                            (TNS-TEN     *   ACCT-ZAIKAISU)
      *    À„∆«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE200-KAISU(8)
                   END-IF
                   COMPUTE   RECE200-TENSU(8 1)  =
                             RECE200-TENSU(8 1)  +
                            (TNS-TEN     *   ACCT-ZAIKAISU)
      *    ƒ¥¥§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "27"
                   COMPUTE   RECE200-TENSU(9 1)  =
                             RECE200-TENSU(9 1)  +
                            (TNS-TEN     *   ACCT-ZAIKAISU)
      *    √ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "31"
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE200-KAISU(10)
                   COMPUTE   RECE200-TENSU(10 1)  =
                             RECE200-TENSU(10 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(11 2)  =
                             RECE200-TENSU(11 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE200C-TENSU(11 2)  =
                             WRK-RECE200C-TENSU(11 2)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE200G-TENSU(11 2)  =
                             WRK-RECE200G-TENSU(11 2)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE200-KAISU(10)
                   COMPUTE   RECE200-TENSU(10 1)  =
                             RECE200-TENSU(10 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(11 2)  =
                             RECE200-TENSU(11 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE200C-TENSU(11 2)  =
                             WRK-RECE200C-TENSU(11 2)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ΩË√÷§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(12)
                   END-IF
                   COMPUTE   RECE200-TENSU(12 1)  =
                             RECE200-TENSU(12 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(13 2)  =
                             RECE200-TENSU(13 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ºÍΩ—§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(14)
                   END-IF
                   COMPUTE   RECE200-TENSU(14 1)  =
                             RECE200-TENSU(14 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(15 2)  =
                             RECE200-TENSU(15 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ∏°∫∫§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(16)
                   END-IF
                   COMPUTE   RECE200-TENSU(16 1)  =
                             RECE200-TENSU(16 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                         COMPUTE   RECE200-KAISU(16)  =
                                   RECE200-KAISU(16)  -
                                   WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   "160101210"     OR
                                              "160095710"     OR
                                              "160095810"
                         IF   RECE200-KAISU(16)  NOT =  ZERO
                              COMPUTE   RECE200-KAISU(16)  =
                                        RECE200-KAISU(16)  -
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE200-TENSU(17 2)  =
                             RECE200-TENSU(17 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ≤Ë¡¸ø«√«§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE200-KAISU(18)
                   END-IF
                   COMPUTE   RECE200-TENSU(18 1)  =
                             RECE200-TENSU(18 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE200-TENSU(19 2)  =
                             RECE200-TENSU(19 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    §Ω§Œ¬æ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
               WHEN    "80"
                   COMPUTE     RECE200-TENSU(20 1) =
                               RECE200-TENSU(20 1) +
                              (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-NYUTENTTLKBN   =    130
                     COMPUTE   RECE200-TENSU(2 1)  =
                               RECE200-TENSU(2 1)  +
                              (TNS-TEN     *   ACCT-ZAIKAISU)
                     COMPUTE   RECE200-TENSU(20 1) =
                               RECE200-TENSU(20 1) -
                              (TNS-TEN     *   ACCT-ZAIKAISU)
                   END-IF
                   COMPUTE     RECE200-TENSU(21 2)  =
                               RECE200-TENSU(21 2)  +
                              (WK-YKZTEN   *   WK-ZAIKAISU)
           END-EVALUATE
           .
      *
       2101221-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∞ÂŒ≈æ Ûœ¢∑» ‘Ω∏
      *****************************************************************
       210123-IRYORENKEI-HENSYU-SEC               SECTION.
      *
           IF   (ACCT-SRYKBN       =  "14" OR "21" OR "22" OR "23")
           AND  (ACCT-ZAITEN       =  ZERO                        )
           AND  (ACCT-ZAIKBN   NOT =   1                          )
           AND  (ACCT-JIHOKBN      =  SPACE                       )
      *
             INITIALIZE                   SRYACT-REC
             MOVE    ACCT-HOSPNUM     TO  SRY-HOSPNUM
             MOVE    ACCT-NYUGAIKBN   TO  SRY-NYUGAIKBN
             MOVE    ACCT-PTID        TO  SRY-PTID
             MOVE    ACCT-SRYYM       TO  SRY-SRYYM
             MOVE    ACCT-ZAINUM      TO  SRY-ZAINUM
             MOVE    "820000046"      TO  SRY-SRYCD(1)
             MOVE    "820000046"      TO  SRY-SRYCD(2)
             MOVE    "820000046"      TO  SRY-SRYCD(3)
             MOVE    "820000046"      TO  SRY-SRYCD(4)
             MOVE    "820000046"      TO  SRY-SRYCD(5)
             MOVE    SRYACT-REC       TO  MCPDATA-REC
             MOVE    "tbl_sryact"     TO  MCP-TABLE
             MOVE    "key8"           TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC   =   ZERO
               MOVE    "tbl_sryact"   TO  MCP-TABLE
               MOVE    "key8"         TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               IF   FLG-SRYACT  =  ZERO
                 CONTINUE
               ELSE
                 MOVE    1            TO  FLG-IRYORENKEI
               END-IF
             END-IF
             MOVE    "tbl_sryact"     TO  MCP-TABLE
             MOVE    "key8"           TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
           IF    FLG-IRYORENKEI   =   1
      *
             INITIALIZE                      ORCSGETGENERICAREA
             MOVE    RECE200-HOSPNUM     TO  ORCSGETGE-HOSPNUM
             MOVE    RECE200-PTID        TO  ORCSGETGE-PTID
             MOVE    RECE200-SRYYM       TO  ORCSGETGE-SRYYM
             MOVE    "1"                 TO  ORCSGETGE-NYUGAIKBN
             MOVE    ACCT-ZAINUM         TO  ORCSGETGE-ZAINUM
             MOVE    WRK-1030-KOUHATUKA  TO  ORCSGETGE-KOUHATUKA
             CALL    "ORCSGETGENERIC"    USING
                                         ORCSGETGENERICAREA
                                         SPA-AREA
             IF      ORCSGETGE-ERRCD     =   ZERO
               PERFORM   VARYING  IDX15    FROM    1   BY  1
                          UNTIL   IDX15     >     50
                 MOVE  ORCSGETGE-YAKKAKJNCD(IDX15)
                            TO  WRK-ORCSGETGE-YAKKAKJNCD(IDX15)
                 MOVE  ORCSGETGE-GENERIC-KBN(IDX15)
                            TO  WRK-ORCSGETGE-GENERIC-KBN(IDX15)
                 MOVE  ORCSGETGE-GENERICNAME(IDX15)
                            TO  WRK-ORCSGETGE-GENERICNAME(IDX15)
               END-PERFORM
             END-IF
      *
           END-IF
      *
           .
      *
       210123-IRYORENKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∫ﬁ§Œø«Œ≈∆‚Õ∆•¡•ß•√•Ø
      *****************************************************************
       21014-SINRYO-CHECK-SEC                 SECTION.
      *
      ***  PERFORM   VARYING   IDU   FROM   1   BY   1
           PERFORM   VARYING   IDU   FROM   2   BY   1
                      UNTIL  ( IDU   >   TBL-ZAICHECK-MAX )
                       OR    ( FLG-RINYAK  NOT =  ZERO    )
      *
              IF  ( IDU  =  1                                  )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60" OR "64"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"        ))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"        ))
      *
                 INITIALIZE                  SRYACT-REC
                 MOVE RECE200-HOSPNUM    TO  SRY-HOSPNUM
                 MOVE RECE200-NYUGAIKBN  TO  SRY-NYUGAIKBN
                 MOVE RECE200-PTID       TO  SRY-PTID
                 MOVE RECE200-SRYYM      TO  SRY-SRYYM
                 MOVE ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE SRYACT-REC         TO  MCPDATA-REC
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC        =  ZERO
                     MOVE "tbl_sryact"   TO  MCP-TABLE
                     MOVE "key8"         TO  MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
                     IF     FLG-SRYACT   =  ZERO
                         EVALUATE   IDU
                           WHEN      1
                             MOVE   1   TO  FLG-RINYAK
                           WHEN      2
                           WHEN      3
                           WHEN      4
                             MOVE   2   TO  FLG-RINYAK
                         END-EVALUATE
                     END-IF
                 END-IF
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       21014-SINRYO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿≈©ºÍµª≤√ªª§ﬁ§»§·¬–æ›§´•¡•ß•√•Ø
      *****************************************************************
       21015-TENTEKIMATOME-CHECK-SEC                 SECTION.
      *
           PERFORM  VARYING  ID4    FROM   1   BY   1
                     UNTIL  (ID4                      >   40 )
                       OR   (WRK-TENTEKI2-ZAINUM(ID4) =  ZERO)
                       OR   (FLG-TENTEKIMATOME        =    1 )
      *
             IF    ACCT-ZAINUM  =  WRK-TENTEKI2-ZAINUM(ID4)
                  MOVE     1    TO   FLG-TENTEKIMATOME
             END-IF
      *
           END-PERFORM
      *
           .
       21015-TENTEKIMATOME-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    •’•È•∞≈˘§Œ•Ø•Í•¢
      *****************************************************************
       21016-FLG-KURIA-SEC                 SECTION.
      *
           MOVE   ZERO   TO   FLG-RINYAK
      *
           MOVE   ZERO   TO   WRK-SRYSURYO
           MOVE   ZERO   TO   WRK-SRYSURYO2
           MOVE   ZERO   TO   WRK-SRYSURYO3
           MOVE   ZERO   TO   WRK-TANICD
           MOVE   ZERO   TO   WRK-TANIMOJISU
           MOVE   SPACE  TO   WRK-TANINAME
           MOVE   ZERO   TO   FLG-HOUSIJI-COM
                              FLG-SYOBYO-COM
                              FLG-RYOYO-COM
                              FLG-SYODOU-COM
                              FLG-OSIN-COM
                              FLG-HOUSEI-COM
                              FLG-HOUMON-COM
                              FLG-TAHOKEN
                              FLG-KENSA-COM
                              FLG-ZAIKYO-COM
                              FLG-BOKOHO-COM
                              FLG-CYOJKSN-COM
                              FLG-HAIJRT-COM
                              FLG-TAIKYO-COM
                              FLG-ZAIHANKO-COM
                              FLG-KIATU-COM
                              FLG-SCPRO-COM
                              FLG-KENKSN-COM
                              FLG-KINGAZ-COM
                              FLG-HAIJSK-COM
      *
           MOVE   ZERO   TO   WRK-FILMTEN
      *
           MOVE   ZERO   TO   WRK-KZI058-TEN
                              WRK-KZI058-TANICD
                              WRK-KZI058-TANIMOJISU
           MOVE   SPACE  TO   WRK-KZI058-TANINAME
      *
           MOVE   ZERO   TO   WRK-RYOYOTEATE
      *
           MOVE   ZERO   TO   FLG-WRITE230
           MOVE   ZERO   TO   FLG-TENTEKI
           MOVE   ZERO   TO   FLG-RIHA-OKIKAE
           MOVE   ZERO   TO   FLG-HENSYU-NG
           MOVE   ZERO   TO   FLG-YKZ-PRTKBN
           MOVE   ZERO   TO   FLG-KIZ-PRTKBN
           MOVE   SPACE  TO   WRK-NYUSAI-SRYKBN
      *
           .
       21016-FLG-KURIA-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£¥°À
      *****************************************************************
       2102-RECE220-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =  1)
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  
                   IF     NACCT-SRYCDTOTAL   NOT =    099999916         
                       PERFORM      210201-NYUINKHN-GET-SEC
                   END-IF                                               
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                               UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                        IF     NACCT-SRYCDTOTAL    =    099999916       
                           CONTINUE
                        ELSE                                            
                           IF    RECE200-KANGOGEN-SRYCD(IDY)  =  SPACE
                             MOVE  WRK-KHNTEN-MOTO  TO  WRK-KHNTEN
                           ELSE
                             MOVE  WRK-KHNTEN-GEN   TO  WRK-KHNTEN
                           END-IF
                           MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDY) 
                           IF (NACCT-SRYKBN     =  "90"       )
                           OR (NACCT-SRYCDTOTAL =   190799410 )
                           OR (NACCT-SRYCDTOTAL =   190799510 )
                              MOVE  903  TO  WRK-DAY1-NYUTENTTLKBN(IDY) 
                           ELSE                                         
                              MOVE  920  TO  WRK-DAY1-NYUTENTTLKBN(IDY) 
                           END-IF                                       
                           MOVE   WRK-KHNTEN
                                         TO  WRK-DAY1-KHNTEN(IDY) 
                           MOVE   WRK-KHN-ZAITEN-GEN
                                         TO  WRK-DAY1-ZAITEN-GEN(IDY)
                           MOVE   WRK-KSNTEN
                                         TO  WRK-DAY1-KSNTEN(IDY)
                           MOVE   WRK-NYUKHNKSNKBN                      
                                         TO  WRK-DAY1-NYUKHNKSNKBN(IDY)
                           MOVE   WRK-CHUKSNCD
                                         TO  WRK-DAY1-CHUKSNCD(IDY)
                           MOVE   WRK-NYUKHNKBN
                                         TO  WRK-DAY1-NYUKHNKBN(IDY)
                           MOVE   WRK-NYUINKHN
                                         TO  WRK-DAY1-NYUINKHN(IDY)
                           MOVE   WRK-HYOKETU-SRYCD
                                         TO  WRK-DAY1-HYOKETU-SRYCD
                                                               (IDY)
                           MOVE   WRK-KSN-SRYCD
                                         TO  WRK-DAY1-KSN-SRYCD
                                                               (IDY)
                           MOVE   WRK-HYOKETU-TEN
                                         TO  WRK-DAY1-HYOKETU-TEN
                                                               (IDY)
                        END-IF                                          
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "2"
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                               UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY2(IDY)
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 IF   ( NACCT-ZAISKBKBN    =    "4"       )
                 AND  ( NACCT-SRYCDTOTAL   =    197000570 )
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                               UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY4(IDY)
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY4X(IDY)
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "5"
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                               UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY5(IDY)
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
             IF    WRK-DAY2(IDY) >=  4
                CONTINUE
             ELSE
                MOVE   ZERO    TO   WRK-DAY1-KSN-SRYCD(IDY)
                MOVE   ZERO    TO   WRK-DAY1-HYOKETU-SRYCD(IDY)
             END-IF
           END-PERFORM
      *
      *    ∆˛±°Œ¡§»∞€§ §Îø©ªˆªªƒÍ ›∏±¡»§ﬂπÁ§Ô§ªºË∆¿
           PERFORM      210225-SKJHKN-GET-SEC
      *
           MOVE    ZERO    TO    WRK-ZAINUM-CNT2
           MOVE    ZERO    TO    FLG-DOJITU
           INITIALIZE            WRK-SKJ-DOJITU-TABLE
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =  1)
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR
                                                "4"  OR  "6"  OR
                                                "7"
      *
                      IF   NACCT-ZAISKBKBN  = "6" OR "7"
                         ADD    1         TO    WRK-ZAINUM-CNT2
                      END-IF
      *
                      INITIALIZE          WRK-DAY-TABLE2
                                          WRK-GAI-TABLE
                                          WRK-GAI-TABLE2
                      MOVE     ZERO       TO    CNT-DAY21
                                                CNT-DAY21-X
                                                CNT-DAY21-Y
                                                CNT-DAY22
                      MOVE     ZERO       TO    FLG-DAY21
                                                FLG-DAY22
                      MOVE     ZERO       TO    WRK-ZAITEN-W
                                                WRK-KANGOGEN-SRYCD-W
                      MOVE     ZERO       TO    WRK-NYUTENTTLKBN
      *
                      PERFORM  VARYING  IDY    FROM    1   BY  1
                                UNTIL   IDY     >     31
      *
                         IF  RECE200-HKNCOMBI    =  WRK-DAY5(IDY)
                            IF   NACCT-ZAITEN    NOT =  ZERO
                            AND  NACCT-DAY(IDY)  NOT =  ZERO
                            AND  NACCT-ZAISKBKBN NOT =  "4"
                              IF NACCT-ZAISKBKBN     =  "1"
                               IF  WRK-DAY2(IDY) =  ZERO
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
                                    IF RECE200-KANGOGEN-SRYCD(IDY)
                                                            =  SPACE
                                      MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21-X(IDY)
                                      ADD  1   TO   CNT-DAY21-X
                                    ELSE
                                      MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21-Y(IDY)
                                      COMPUTE  WRK-KEISAN2    =
                                               WRK-DAY1-ZAITEN-GEN(IDY)
                                                              +  0.5
                                      MOVE   WRK-KEISAN2
                                           TO   WRK-ZAITEN-W
                                      MOVE   RECE200-KANGOGEN-SRYCD(IDY)
                                           TO   WRK-KANGOGEN-SRYCD-W
                                      ADD  1   TO   CNT-DAY21-Y
                                    END-IF
                               END-IF
                              ELSE
                               IF  ((WRK-DAY1(IDY)   NOT  =  ZERO   ) OR
                                    (RECE200-NYUINDAY-KBN(IDY) = "1"))
                               AND ((WRK-DAY2(IDY)   =  ZERO        ) OR
                                    (WRK-DAY2(IDY)  >=   4          ))
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    MOVE   WRK-DAY1-NYUTENTTLKBN(IDY)
                                           TO   WRK-NYUTENTTLKBN
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
                               END-IF
                              END-IF
                            END-IF
      *                     *≥∞«Ò
                            IF  (WRK-DAY1(IDY)   NOT  =  ZERO)
                            AND (NACCT-ZAITEN         =  ZERO)
                            AND (NACCT-ZAISKBKBN      =  "2" )
                            AND (NACCT-DAY(IDY)  NOT  =  ZERO)
                               MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY22(IDY) 
                               MOVE   WRK-DAY1-NYUTENTTLKBN(IDY)
                                        TO  WRK-DAY22-NYUTENTTLKBN(IDY)
                               MOVE   WRK-DAY1-NYUKHNKSNKBN(IDY)        
                                        TO  WRK-DAY22-NYUKHNKSNKBN(IDY) 
                               MOVE   WRK-DAY1-CHUKSNCD(IDY)
                                        TO  WRK-DAY22-CHUKSNCD(IDY)
                               MOVE   WRK-DAY1-NYUKHNKBN(IDY)
                                        TO  WRK-DAY22-NYUKHNKBN(IDY)
                               MOVE   WRK-DAY1-NYUINKHN(IDY)
                                        TO  WRK-DAY22-NYUINKHN(IDY)
                               PERFORM      210203-NYUINACCT-GET-SEC
                            END-IF
      *                     *ø©ªˆ
                            PERFORM      210226-SKJ-HENSYU-SEC
                         ELSE
      *                     *ø©ªˆ
                            PERFORM      210227-SKJ-HENSYU2-SEC
                         END-IF
      *
                      END-PERFORM
      *
                      IF   FLG-DAY21    =   1
                        IF   RECE200-SRYYM       >= "201804"
                        AND  NACCT-ZAISKBKBN      = "1"
                        AND  CNT-DAY21-Y      NOT = ZERO
                         IF  CNT-DAY21 = CNT-DAY21-Y
                           MOVE      2    TO    FLG-HENSYU
                           PERFORM   210205-RECE220-TENSOU2-SEC
                           PERFORM   210206-RECE230-HENSYU2-SEC
                           PERFORM   RECE220-WRITE-SEC
                         ELSE
                           MOVE      3    TO    FLG-HENSYU
                           PERFORM   210205-RECE220-TENSOU2-SEC
                           PERFORM   210206-RECE230-HENSYU2-SEC
                           PERFORM   RECE220-WRITE-SEC
                           MOVE      4    TO    FLG-HENSYU
                           PERFORM   210205-RECE220-TENSOU2-SEC
                           PERFORM   210206-RECE230-HENSYU2-SEC
                           PERFORM   RECE220-WRITE-SEC
                         END-IF
                        ELSE
                         MOVE      1      TO    FLG-HENSYU
                         PERFORM   210205-RECE220-TENSOU2-SEC
                         PERFORM   210206-RECE230-HENSYU2-SEC
                         PERFORM   RECE220-WRITE-SEC
                        END-IF
                      END-IF
      *
                      IF   FLG-DAY22    =   1
                         PERFORM   210209-NYUINACCT-HEN-SEC
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           > 20
                             OR    WRK-GAI2-DAY(SYUKEI-IDX3) = ZERO
                           PERFORM   210210-RECE220-TENSOU2-GAI-SEC
                           PERFORM   210211-RECE230-HENSYU2-GAI-SEC
                           PERFORM   RECE220-WRITE-SEC
                         END-PERFORM
                      END-IF
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    ∆±∆¸∫∆∆˛±°
      *
           IF   RECE200-SRYYM   >=  "200604"
              IF   FLG-DOJITU   =   1
                    PERFORM   210223-NYUINACCT-SYU2-DOU-SEC
              END-IF
           END-IF
      *
      *    ∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€ºË∆¿
           IF     RECE200-SRYYM   >=  "201604"
             INITIALIZE                    ORCSNYULIFE2-AREA
             MOVE    RECE200-HOSPNUM   TO  LNK-NYULIFE2-HOSPNUM
             MOVE    RECE200-PTID      TO  LNK-NYULIFE2-PTID
             MOVE    RECE200-SRYYM     TO  LNK-NYULIFE2-SANTEIYM
             CALL    "ORCSNYULIFE2"        USING
                                           ORCSNYULIFE2-AREA
                                           SPA-AREA
             IF      LNK-NYULIFE2-RC  =   ZERO
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                         UNTIL   SYUKEI-IDX3   >   31
                 MOVE  LNK-NYULIFE2-KBN(SYUKEI-IDX3)
                             TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
               END-PERFORM
             END-IF
           ELSE
            IF    RECE200-SRYYM   >=  "200610"
             INITIALIZE                    ORCSNYULIFE-AREA
             MOVE    RECE200-HOSPNUM   TO  LNK-NYULIFE-HOSPNUM
             MOVE    RECE200-PTID      TO  LNK-NYULIFE-PTID
             MOVE    RECE200-SRYYM     TO  LNK-NYULIFE-SANTEIYM
             CALL    "ORCSNYULIFE"         USING
                                           ORCSNYULIFE-AREA
                                           SPA-AREA
             IF      LNK-NYULIFE-RC   =   ZERO
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                         UNTIL   SYUKEI-IDX3   >   31
                 MOVE  LNK-NYULIFE-KBN(SYUKEI-IDX3)
                             TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
               END-PERFORM
             END-IF
            END-IF
           END-IF
      *
      *    £≤£∞£∞£∂«Ø£¥∑Ó∞ πﬂ§Œø©ªˆŒ≈Õ‹
           IF   RECE200-SRYYM   >=  "200604"
      *
              PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL   SYUKEI-IDX3    >  31
      *
                 IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                                                    NOT =  ZERO
                 OR   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)
                                                    NOT =  ZERO
                    PERFORM 210213-RECE220-TENSOU2-SKJ2-SEC
                    PERFORM 210214-RECE230-HENSYU2-SKJ2-SEC
                    PERFORM RECE230-WRITE-SEC
                    PERFORM RECE220-WRITE-SEC
                 END-IF
      *
                 IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                   IF    WRK-SKJ200604-DAY-KAN(SYUKEI-IDX3)
                                                    NOT =  ZERO
                    PERFORM 210219-RECE220-TENSOU2-KAN-SEC
                    PERFORM 210220-RECE230-HENSYU2-KAN-SEC
                    PERFORM RECE230-WRITE-SEC
                    PERFORM RECE220-WRITE-SEC
                   END-IF
                 END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       2102-RECE220-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ∆˛±°¥À‹Œ¡§ŒºË∆¿
      **************************************************************
       210201-NYUINKHN-GET-SEC           SECTION.
      *
           MOVE    ZERO               TO   WRK-KHNTEN
                                           WRK-KHNTEN-MOTO
                                           WRK-KHNTEN-GEN
      *****                                WRK-KHN-ZAITEN
                                           WRK-KHN-ZAITEN-MOTO
                                           WRK-KHN-ZAITEN-GEN
                                           WRK-NYUKHNKSNKBN
                                           WRK-KSNTEN
                                           WRK-KSN-SRYCD
      *
           MOVE    SPACE              TO   WRK-CHUKSNCD
           MOVE    ZERO               TO   WRK-NYUKHNKBN
           MOVE    SPACE              TO   WRK-NYUINKHN
      *
           MOVE    SPACE              TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO               TO   WRK-GAIHAKU-KHNTEN
      *
           MOVE    ZERO               TO   WRK-HYOKETU-TEN
                                           WRK-HYOKETU-SRYCD
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =    ZERO
               MOVE    "tbl_nyuinact"      TO      MCP-TABLE
               MOVE    "key9"              TO      MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
           IF      FLG-NYUINACT        =   ZERO
               MOVE      ZERO         TO   FLG-NYUKHN
               PERFORM   VARYING  IDX2  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =      1)    OR
                        (IDX2               >      5)    OR
                        (NSRY-SRYCD(IDX2)   =  SPACE)
                     MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
                     MOVE    NACCT-SRYYM         TO  WK-TAIHI-SRYYM
                     MOVE    1                   TO  WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF   FLG-TENSU      =    ZERO
                       IF   TNS-DATAKBN    =    1
                          IF  (RECE200-SRYYM   >= "202004")
                          AND (TNS-SRYCD    NOT = SPACE   )
                          AND (TNS-NYUKHNKSNKBN = ZERO    )
                            INITIALIZE              SNTENSU-AREA
                            MOVE  TNS-SRYCD     TO  SNTENSU-SRYCD
                            MOVE  RECE200-SRYYM
                                         TO   SNTENSU-BASEYMD(1:6)
                            MOVE  "01"   TO   SNTENSU-BASEYMD(7:2)
                            CALL  "ORCSNTENSU"  USING
                                                SNTENSU-AREA
                                                TNS-TENSU-REC
                                                SPA-AREA
                          END-IF
                          MOVE   TNS-TEN   TO   WRK-KHNTEN-MOTO
      *                   ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª
                          COMPUTE  WRK-KHNTEN-GEN   =
                                  (WRK-KHNTEN-MOTO  * 0.95)
                          MOVE   TNS-NYUKHNKSNKBN
                                           TO   WRK-NYUKHNKSNKBN
      *                   ∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨√÷§≠¥π§®
                          IF  RECE200-SRYYM >= "201804"
      *                     Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
      *                                   ° ∆√ Ã∆˛±°¥À‹Œ¡°À
                            IF   (TNS-NYUKHNKSNKBN = 27 )
                            AND  (TNS-CDKBN-KBNNUM = 101)
                              MOVE   901   TO   WRK-NYUKHNKSNKBN
                            END-IF
      *                     Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
      *                                   ° ∆√ Ã∆˛±°¥À‹Œ¡°À
                            IF   (TNS-NYUKHNKSNKBN = 27 )
                            AND  (TNS-CDKBN-KBNNUM = 109)
                              MOVE   909   TO   WRK-NYUKHNKSNKBN
                            END-IF
                          END-IF
                          MOVE   TNS-CHUKSNCD
                                           TO   WRK-CHUKSNCD
                          MOVE   TNS-NYUKHNKBN
                                           TO   WRK-NYUKHNKBN
                          MOVE   NSRY-SRYCD(IDX2)
                                           TO   WRK-NYUINKHN
      *                   ¥«∏Ó»ÊŒ®¥Ω‡Ã§√£ª˛§Œ∆˛±°¥À‹≈¿øÙ§ŒºË∆¿
                          IF    RECE200-SRYYM  <=  "201203"
                           PERFORM 2102011-GAIHAKU-NYUINKHN-SEC
                          END-IF
                          IF   WRK-GAIHAKU-KHNTEN  NOT =  ZERO
                                 MOVE   WRK-GAIHAKU-KHNTEN
                                           TO   WRK-KHNTEN-MOTO
                          END-IF
      ***                 MOVE   1         TO   FLG-NYUKHN
                       ELSE
                          EVALUATE  TNS-TENSIKIBETU
                            WHEN     3
                                MOVE  TNS-TEN  TO  WRK-KSNTEN
                                MOVE  NSRY-SRYCD(IDX2)
                                               TO  WRK-KSN-SRYCD
                            WHEN     8
                                COMPUTE  WRK-KSNTEN = TNS-TEN * -1
                                MOVE  NSRY-SRYCD(IDX2)
                                               TO  WRK-KSN-SRYCD
                            WHEN     6
                                MOVE  TNS-TEN  TO  WRK-HYOKETU-TEN
                                MOVE  NSRY-SRYCD(IDX2)
                                               TO  WRK-HYOKETU-SRYCD
                          END-EVALUATE
                       END-IF
                     END-IF
               END-PERFORM
      *
               COMPUTE  WRK-KHN-ZAITEN-MOTO     =
                        NACCT-ZAITEN
               COMPUTE  WRK-KHN-ZAITEN-GEN      =
                        WRK-KHNTEN-GEN          +
                        WRK-KSNTEN
               IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *          …∏∑Á°ß°Û∏∫ªª
                 COMPUTE    WRK-KEISAN2             =
                          ( WRK-KHN-ZAITEN-GEN      -
                           (WRK-KHN-ZAITEN-GEN      *
                            (WRK-HYOKETU-TEN   /  100))) + 0.5
                 MOVE   WRK-KEISAN2   TO   WRK-KHN-ZAITEN-GEN
               END-IF
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO      MCP-TABLE
           MOVE    "key9"              TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210201-NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ≥∞«Òª˛§Œ∆˛±°¥À‹Œ¡ºË∆¿
      **************************************************************
       2102011-GAIHAKU-NYUINKHN-SEC           SECTION.
      *
           EVALUATE  TNS-SRYCD
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *        ø«Œ≈ΩÍœ∑øÕ∞ÂŒ≈¥…Õ˝Œ¡
               WHEN  "190774210"
                     MOVE  "190774110"      TO   WRK-GAIHAKU-SRYCD
      *        œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£±
               WHEN  "190740010"
                     MOVE  "190739910"      TO   WRK-GAIHAKU-SRYCD
      *        œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£≤
               WHEN  "190813510"
                     MOVE  "190813410"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (RECE200-SRYYM      >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-SRYCD
               MOVE    NACCT-SRYYM         TO  WK-TAIHI-SRYYM
               MOVE    1                   TO  WK-TAIHI-SRYD
               PERFORM    900-TENSU-TBL-SEC
               IF   FLG-TENSU      =    ZERO
                     MOVE     TNS-TEN      TO  WRK-GAIHAKU-KHNTEN
               END-IF
           END-IF
      *
           .
       2102011-GAIHAKU-NYUINKHN-EXT.
           EXIT.
      *
      **************************************************************
      *    ≥∞«Ò§Œ∆˛±°¥À‹Œ¡∑◊ªª
      **************************************************************
       210203-NYUINACCT-GET-SEC           SECTION.
      *
      *    ∆˛±°Œ¡§»≥∞«Ò§Ú»Ê≥”§∑§∆≈¿øÙªªƒÍ
      *
           EVALUATE   WRK-DAY22-NYUTENTTLKBN(IDY)
               WHEN   903
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    1     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   2
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         MOVE    2     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   4
                         IF    RECE200-SRYYM    >=    "201604"
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.6 )
                         ELSE
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.3 )
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    4     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   5
                         IF    RECE200-SRYYM    >=    "201604"
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.9 )
                         ELSE
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.7 )
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    11    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   6
                         IF    RECE200-SRYYM    >=    "201604"
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.8 )
                         ELSE
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.45)
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    15    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   7
                       IF    RECE200-SRYYM    >=    "201604"
                         CONTINUE
                       ELSE
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.85)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    19    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                       END-IF
                     WHEN   8
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    23    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   9
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    27    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
      ***            WHEN   11
      ***                COMPUTE    WRK-TENSU-KEISAN2 =
      ***                          (WRK-DAY1-KHNTEN(IDY) * 0.92
      ***                                                * 0.7 )
      ***                PERFORM    2102032-NYUINACCT-HEN1-SEC
      ***                MOVE    35    TO    SYUKEI-IDX
      ***                PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   13
                         IF    RECE200-SRYYM    >=    "201604"
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92
                                                         * 0.9 )
                         ELSE
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92
                                                         * 0.7 )
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    43    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   14
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.95)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    47    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   15
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.65)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    51    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   16
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.85)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    55    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   17
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92
                                                         * 0.95)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    59    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                    END-EVALUATE
               WHEN   920
                    EVALUATE   WRK-DAY22(IDY)
                     WHEN   1
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.15)
                                                        +  0.5
                         MOVE    6     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   2
                         COMPUTE  WRK-DAY22-NYUKHNRYO(IDY) =
                                 (WRK-DAY1-KHNTEN(IDY)  *  0.3)
                                                        +  0.5
                         MOVE    7     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   4
                         IF    RECE200-SRYYM    >=    "201604"
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.6 )
                         ELSE
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.3 )
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    9     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   5
                         IF    RECE200-SRYYM    >=    "201604"
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.9 )
                         ELSE
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.7 )
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    13    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   6
                         IF    RECE200-SRYYM    >=    "201604"
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.8 )
                         ELSE
                            COMPUTE WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.45)
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    16    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   7
                       IF    RECE200-SRYYM    >=    "201604"
                         CONTINUE
                       ELSE
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.85)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    20    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                       END-IF
                     WHEN   8
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    24    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   9
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    28    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
      ***            WHEN   11
      ***                COMPUTE    WRK-TENSU-KEISAN2 =
      ***                          (WRK-DAY1-KHNTEN(IDY) * 0.92
      ***                                                * 0.7 )
      ***                PERFORM    2102032-NYUINACCT-HEN1-SEC
      ***                MOVE    36    TO    SYUKEI-IDX
      ***                PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   13
                         IF    RECE200-SRYYM    >=    "201604"
                           COMPUTE  WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92
                                                         * 0.9 )
                         ELSE
                           COMPUTE  WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92
                                                         * 0.7 )
                         END-IF
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE    44    TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   14
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.95)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE   48     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   15
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.65)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE   52     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   16
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.85)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE   56     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                     WHEN   17
                         COMPUTE    WRK-TENSU-KEISAN2 =
                                   (WRK-DAY1-KHNTEN(IDY) * 0.92
                                                         * 0.95)
                         PERFORM    2102032-NYUINACCT-HEN1-SEC
                         MOVE   60     TO    SYUKEI-IDX
                         PERFORM    2102031-NYUINACCT-SYU-SEC
                    END-EVALUATE
           END-EVALUATE
      *
           .
       210203-NYUINACCT-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ≥∞«Ò§Œ∆˛±°¥À‹Œ¡Ω∏∑◊
      **************************************************************
       2102031-NYUINACCT-SYU-SEC           SECTION.
      *
           IF  ( WRK-DAY22-NYUKHNRYO(IDY)  NOT =  ZERO     )
      *
             MOVE     ZERO     TO    FLG-ARI
             PERFORM  VARYING  SYUKEI-IDX2    FROM    1   BY  1
                  UNTIL  SYUKEI-IDX2           > 10
                   OR    WRK-GAI-DAY(SYUKEI-IDX SYUKEI-IDX2) = ZERO
                   OR    FLG-ARI               = 1
              IF  (WRK-GAI-ZAITEN(SYUKEI-IDX SYUKEI-IDX2)
                                        =  WRK-DAY22-NYUKHNRYO(IDY)   )
              AND (WRK-GAI-KANGOGEN-SRYCD(SYUKEI-IDX SYUKEI-IDX2)
                                        =  RECE200-KANGOGEN-SRYCD(IDY))
                 ADD   1  TO   WRK-GAI-ZAIKAISU(SYUKEI-IDX SYUKEI-IDX2)
                 MOVE  1  TO                                            
                           WRK-GAI-KAISU   (SYUKEI-IDX SYUKEI-IDX2 IDY) 
                 MOVE  1  TO   FLG-ARI
              END-IF
             END-PERFORM  
             IF  FLG-ARI  =  ZERO  
                 MOVE  IDY 
                          TO   WRK-GAI-DAY     (SYUKEI-IDX SYUKEI-IDX2)
                 MOVE  WRK-DAY22-NYUKHNRYO(IDY)
                          TO   WRK-GAI-ZAITEN  (SYUKEI-IDX SYUKEI-IDX2) 
                 MOVE  RECE200-KANGOGEN-SRYCD(IDY)
                          TO   WRK-GAI-KANGOGEN-SRYCD
                                               (SYUKEI-IDX SYUKEI-IDX2)
                 ADD   1  TO   WRK-GAI-ZAIKAISU(SYUKEI-IDX SYUKEI-IDX2)
                 MOVE  1  TO                                            
                           WRK-GAI-KAISU   (SYUKEI-IDX SYUKEI-IDX2 IDY) 
             END-IF
      *
             ADD    1     TO   CNT-DAY22
             MOVE   1     TO   FLG-DAY22
      *
           END-IF
      *
           .
      *
       2102031-NYUINACCT-SYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ≥∞«Ò§Œ∆˛±°¥À‹Œ¡ ‘Ω∏£±
      **************************************************************
       2102032-NYUINACCT-HEN1-SEC           SECTION.
      *
           COMPUTE   WRK-TENSU-KEISAN2           =
                     WRK-TENSU-KEISAN2           +
                     WRK-DAY1-KSNTEN(IDY)
           IF  WRK-DAY1-HYOKETU-TEN(IDY)  NOT =  ZERO
             COMPUTE WRK-DAY22-NYUKHNRYO(IDY)    =
                   ( WRK-TENSU-KEISAN2           -
                    (WRK-TENSU-KEISAN2           *
                    (WRK-DAY1-HYOKETU-TEN(IDY)   / 100)))
                                                 + 0.5
           ELSE
             COMPUTE WRK-DAY22-NYUKHNRYO(IDY)    =
                     WRK-TENSU-KEISAN2           + 0.5
           END-IF
      *
           .
      *
       2102032-NYUINACCT-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°Œ¡§»∞€§ §Îø©ªˆªªƒÍ ›∏±¡»§ﬂπÁ§Ô§ªºË∆¿
      *****************************************************************
       210225-SKJHKN-GET-SEC                SECTION.
      *
           IF   RECE200-SRYYM  >=  "201710"
             INITIALIZE                    ORCSNYUINDAY-AREA
             PERFORM    VARYING   IDX35   FROM   1   BY   1
                         UNTIL    IDX35    >    31
               MOVE  RECE200-DOUJITSU-HKNCOMBI(IDX35)
                           TO  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX35)
             END-PERFORM
      *
             INITIALIZE                    ORCSSKJHKN-AREA
             MOVE    RECE200-HOSPNUM   TO  LNK-SKJHKN-HOSPNUM
             MOVE    RECE200-PTID      TO  LNK-SKJHKN-PTID
             MOVE    RECE200-SRYYM     TO  LNK-SKJHKN-SANTEIYM
             CALL    "ORCSSKJHKN"      USING   ORCSSKJHKN-AREA
                                               ORCSNYUINDAY-AREA
                                               SPA-AREA
             IF      LNK-SKJHKN-RC     =   ZERO
                PERFORM    VARYING   IDY   FROM   1   BY   1
                            UNTIL    IDY    >    31
                  MOVE  LNK-SKJHKN-HKNCOMBINUM(IDY 1) TO
                                          WRK-SKJHKN-HKNCOMBI(IDY)
                  MOVE  LNK-SKJHKN-HKNCOMBINUM(IDY 2) TO
                                          WRK-SKJHKN-HKNCOMBI-DOU(IDY)
                END-PERFORM
             END-IF
           END-IF
      *
           .
       210225-SKJHKN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø©ªˆ ‘Ω∏
      *****************************************************************
       210226-SKJ-HENSYU-SEC                SECTION.
      *
           IF  ((WRK-DAY1(IDY)         NOT =  ZERO) OR
                (RECE200-NYUINDAY-KBN(IDY) =  "1" ))
           AND ( NACCT-ZAITEN              =  ZERO )
           AND ( NACCT-ZAISKBKBN           =  "4"  )
           AND ( NACCT-DAY(IDY)        NOT =  ZERO )
           AND ( WRK-SHOKUJIRYOYO(IDY) NOT =  ZERO )
           AND ((WRK-DAY2(IDY)             =  ZERO) OR
                (WRK-DAY2(IDY)            >=   4  ))
             IF   RECE200-SRYYM  >= "200604"
               IF  NACCT-SRYCDTOTAL = 099999918 OR 099999919
                                                OR 099999920
                 IF    WRK-SKJHKN-HKNCOMBI(IDY)  =  SPACE
                   MOVE  NACCT-DAY(IDY)  TO  WRK-DAY24(IDY)
                   PERFORM  210204-NYUINACCT-SYU2-SEC
                 ELSE
                   PERFORM  210227-SKJ-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
           END-IF
           IF  ( RECE200-SRYYM            >=  "200610"     )
           AND ((WRK-DAY1(IDY)         NOT =  ZERO        ) OR
                (RECE200-NYUINDAY-KBN(IDY) =  "1"         ))
           AND ( NACCT-ZAITEN              =  ZERO         )
           AND ( NACCT-ZAISKBKBN           =  "4"          )
      ***  AND ( NACCT-DAY(IDY)        NOT =  ZERO         )
           AND ( WRK-SHOKUJIRYOYO(IDY) NOT =  ZERO         )
           AND ( NACCT-SRYCDTOTAL = 099999918 OR 099999919
                                              OR 099999920 )
           AND ((WRK-DAY2(IDY)             =  ZERO        ) OR
                (WRK-DAY2(IDY)            >=   4          ))
               PERFORM  210218-NYUINACCT-SYU2-KAN-SEC
           END-IF
      *
           .
       210226-SKJ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø©ªˆ ‘Ω∏
      *****************************************************************
       210227-SKJ-HENSYU2-SEC                SECTION.
      *
           IF  ( RECE200-SRYYM            >=  "200604"     )
           AND ( NACCT-ZAITEN              =  ZERO         )
           AND ( NACCT-ZAISKBKBN           =  "4"          )
           AND ( NACCT-DAY(IDY)        NOT =  ZERO         )
           AND ( WRK-SHOKUJIRYOYO(IDY) NOT =  ZERO         )
           AND ((WRK-DAY2(IDY)             =  ZERO        ) OR
                (WRK-DAY2(IDY)            >=   4          ))
           AND ((WRK-DAY1(IDY)         NOT =  ZERO        ) OR
                (RECE200-NYUINDAY-KBN(IDY) =  "1"         ))
           AND ( NACCT-SRYCDTOTAL = 099999918 OR 099999919
                                              OR 099999920 )
      *
             IF  ( WRK-SKJHKN-HKNCOMBI(IDY)      = SPACE )
             OR  ((WRK-SKJHKN-HKNCOMBI(IDY)  NOT = SPACE) AND
                  (RECE200-HKNCOMBI          NOT =
                                WRK-SKJHKN-HKNCOMBI(IDY)))
               IF ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (RECE200-HKNCOMBI             =
                                     RECE200-DOUJITSU-HKNCOMBI(IDY)) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)        = SPACE    ))
               OR ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)    NOT = SPACE    ) AND
                   (RECE200-HKNCOMBI             =
                                     WRK-SKJHKN-HKNCOMBI-DOU(IDY)  ))
                 MOVE    1    TO  FLG-DOJITU
                 MOVE    1    TO  WRK-SKJ-DOJITU-KBN (IDY)
                 EVALUATE   NACCT-SRYCDTOTAL
                   WHEN  099999918
                     MOVE  NACCT-DAY(IDY)
                              TO  WRK-SKJ-DOJITU-ASA (IDY)
                   WHEN  099999919
                     MOVE  NACCT-DAY(IDY)
                              TO  WRK-SKJ-DOJITU-HIRU(IDY)
                   WHEN  099999920
                     MOVE  NACCT-DAY(IDY)
                              TO  WRK-SKJ-DOJITU-YORU(IDY)
                 END-EVALUATE
               END-IF
             END-IF
      *
             IF   (WRK-SKJHKN-HKNCOMBI(IDY)  NOT = SPACE)
             AND  (RECE200-HKNCOMBI              =
                                WRK-SKJHKN-HKNCOMBI(IDY))
               IF ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (RECE200-HKNCOMBI             =
                                     RECE200-DOUJITSU-HKNCOMBI(IDY)) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)        = SPACE    ))
               OR ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)    NOT = SPACE    ) AND
                   (RECE200-HKNCOMBI             =
                                     WRK-SKJHKN-HKNCOMBI-DOU(IDY)  ))
               OR ( RECE200-DOUJITSU-HKNCOMBI(IDY)      = ZERO      )
                 MOVE  NACCT-DAY(IDY)  TO  WRK-DAY24(IDY)
                 PERFORM  210204-NYUINACCT-SYU2-SEC
               END-IF
      *
               IF ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (RECE200-HKNCOMBI         NOT =
                                     RECE200-DOUJITSU-HKNCOMBI(IDY)) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)        = SPACE    ))
               OR ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)    NOT = SPACE    ) AND
                   (RECE200-HKNCOMBI         NOT =
                                     WRK-SKJHKN-HKNCOMBI-DOU(IDY)  ))
                 MOVE    1    TO  FLG-DOJITU
                 MOVE    2    TO  WRK-SKJ-DOJITU-KBN (IDY)
                 EVALUATE   NACCT-SRYCDTOTAL
                   WHEN  099999918
                     MOVE  NACCT-DAY(IDY)
                              TO  WRK-SKJ-DOJITU-ASA (IDY)
                   WHEN  099999919
                     MOVE  NACCT-DAY(IDY)
                              TO  WRK-SKJ-DOJITU-HIRU(IDY)
                   WHEN  099999920
                     MOVE  NACCT-DAY(IDY)
                              TO  WRK-SKJ-DOJITU-YORU(IDY)
                 END-EVALUATE
               END-IF
             END-IF
      *
           END-IF
      *
           .
       210227-SKJ-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§ŒΩ∏∑◊
      **************************************************************
       210204-NYUINACCT-SYU2-SEC           SECTION.
      *
           IF  ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                (RECE200-HKNCOMBI        NOT =
                                  RECE200-DOUJITSU-HKNCOMBI(IDY)) AND
                (WRK-SKJHKN-HKNCOMBI-DOU(IDY)        = SPACE    ))
           OR  ((RECE200-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                (WRK-SKJHKN-HKNCOMBI-DOU(IDY)    NOT = SPACE    ) AND
                (RECE200-HKNCOMBI        NOT =
                                  WRK-SKJHKN-HKNCOMBI-DOU(IDY)  ))
            MOVE    1    TO  FLG-DOJITU
            MOVE    2    TO  WRK-SKJ-DOJITU-KBN (IDY)
            EVALUATE   NACCT-SRYCDTOTAL
              WHEN  099999918
                MOVE  NACCT-DAY(IDY) TO  WRK-SKJ-DOJITU-ASA (IDY)
              WHEN  099999919
                MOVE  NACCT-DAY(IDY) TO  WRK-SKJ-DOJITU-HIRU(IDY)
              WHEN  099999920
                MOVE  NACCT-DAY(IDY) TO  WRK-SKJ-DOJITU-YORU(IDY)
            END-EVALUATE
           ELSE
            EVALUATE   WRK-SHOKUJIRYOYO(IDY)
              WHEN  1
                 EVALUATE   WRK-DAY24(IDY)
                   WHEN  1
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE  ZERO    TO  WRK-DAY4(IDY)
                     END-IF
                   WHEN  2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 2)
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE  ZERO    TO  WRK-DAY4(IDY)
                     END-IF
                   WHEN  3
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE  ZERO    TO  WRK-DAY4(IDY)
                     END-IF
                 END-EVALUATE
              WHEN  2
                 EVALUATE   WRK-DAY24(IDY)
                   WHEN  1
                   WHEN  2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                   WHEN  3
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                 END-EVALUATE
            END-EVALUATE
           END-IF
      *
           .
       210204-NYUINACCT-SYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§ŒΩ∏∑◊° ∆±∆¸∫∆∆˛±° ¨°À
      **************************************************************
       210223-NYUINACCT-SYU2-DOU-SEC           SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
             EVALUATE  WRK-SKJ-DOJITU-KBN(IDY)
               WHEN         1
                 MOVE  WRK-DAY4X(IDY)
                                   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                 EVALUATE  RECE200-DOUJITSU-SKJKBN(IDY)
                   WHEN     "0"
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                   WHEN     "1"
                     CONTINUE
                   WHEN     "2"
                     IF   WRK-SKJ-DOJITU-ASA (IDY)  NOT = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                   WHEN     "3"
                     IF   WRK-SKJ-DOJITU-ASA (IDY)  NOT = ZERO
                     OR   WRK-SKJ-DOJITU-HIRU(IDY)  NOT = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                 END-EVALUATE
               WHEN         2
                 MOVE  WRK-DAY4X(IDY)
                                   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                 EVALUATE  RECE200-DOUJITSU-SKJKBN(IDY)
                   WHEN     "0"
                     CONTINUE
                   WHEN     "1"
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-ASA (IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                   WHEN     "2"
                     IF   WRK-SKJ-DOJITU-ASA (IDY) = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-HIRU(IDY)
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                   WHEN     "3"
                     IF   WRK-SKJ-DOJITU-ASA (IDY) = ZERO
                     AND  WRK-SKJ-DOJITU-HIRU(IDY) = ZERO
                       MOVE ZERO   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                     MOVE   ZERO   TO  WRK-SKJ-DOJITU-YORU(IDY)
                 END-EVALUATE
             END-EVALUATE
           END-PERFORM
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
             IF   WRK-SKJ-DOJITU-KBN(IDY)  NOT = ZERO
               EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                 WHEN  1
                   IF   WRK-SKJ-DOJITU-ASA (IDY) =  1 OR 2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF   WRK-SKJ-DOJITU-ASA (IDY) =  2
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 2)
                     END-IF
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                   IF   WRK-SKJ-DOJITU-ASA (IDY) =  3
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                   IF   WRK-SKJ-DOJITU-HIRU(IDY) =  1 OR 2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF   WRK-SKJ-DOJITU-HIRU(IDY) =  2
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 2)
                     END-IF
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                   IF   WRK-SKJ-DOJITU-HIRU(IDY) =  3
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                   IF   WRK-SKJ-DOJITU-YORU(IDY) =  1 OR 2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF   WRK-SKJ-DOJITU-YORU(IDY) =  2
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 2)
                     END-IF
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                   IF   WRK-SKJ-DOJITU-YORU(IDY) =  3
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                     IF   WRK-SKJ-DOJITU-SHOKUDO(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE ZERO  TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                     END-IF
                   END-IF
                 WHEN  2
                   IF   WRK-SKJ-DOJITU-ASA (IDY) =  1 OR 2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                   END-IF
                   IF   WRK-SKJ-DOJITU-ASA (IDY) =  3
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                   END-IF
                   IF   WRK-SKJ-DOJITU-HIRU(IDY) =  1 OR 2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                   END-IF
                   IF   WRK-SKJ-DOJITU-HIRU(IDY) =  3
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                   END-IF
                   IF   WRK-SKJ-DOJITU-YORU(IDY) =  1 OR 2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                   END-IF
                   IF   WRK-SKJ-DOJITU-YORU(IDY) =  3
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 4)
                   END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
      *
           .
       210223-NYUINACCT-SYU2-DOU-EXT.
           EXIT.
      *
      **************************************************************
      *    ø©ªˆŒ≈Õ‹»Ò§ŒΩ∏∑◊° ¥ƒ∂≠°À
      **************************************************************
       210218-NYUINACCT-SYU2-KAN-SEC           SECTION.
      *
           EVALUATE   WRK-SHOKUJIRYOYO(IDY)
             WHEN  1
               MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
               MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
             WHEN  2
               MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
               MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
           END-EVALUATE
      *
           .
       210218-NYUINACCT-SYU2-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       210205-RECE220-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO   RECE220-REC
           INITIALIZE                        RECE220-REC
           MOVE    RECE200-KEY          TO   RECE220-KEY1
           MOVE    NACCT-SRYKBN         TO   RECE220-SRYKBN
           IF      NACCT-ZAISKBKBN  =  "1"
             IF  (NACCT-SRYCDTOTAL =   190799410)
             OR  (NACCT-SRYCDTOTAL =   190799510)
                MOVE    "90"            TO   RECE220-SRYKBN
             END-IF
           END-IF
           IF      NACCT-ZAISKBKBN  =  "6"
             EVALUATE  WRK-NYUTENTTLKBN
               WHEN      903
                MOVE    "90"            TO   RECE220-SRYKBN
               WHEN      920
                MOVE    "92"            TO   RECE220-SRYKBN
             END-EVALUATE
           END-IF
           MOVE    NACCT-ZAISKBKBN      TO   RECE220-ZAISKBKBN
           IF      NACCT-ZAISKBKBN  =  "7"
                MOVE     "80"           TO   RECE220-SRYKBN
                MOVE    SPACE           TO   RECE220-ZAISKBKBN
           END-IF
      *
           EVALUATE  FLG-HENSYU
             WHEN      1
             WHEN      2
               MOVE    ZERO               TO   WRK-ZAINUM-CNT
               EVALUATE  NACCT-ZAISKBKBN
                 WHEN    "1"
                   PERFORM  VARYING  IDX2  FROM  1  BY  1
                             UNTIL   IDX2   >   31
                              OR     WRK-ZAINUM-CNT  NOT =  ZERO
                     IF  WRK-DAY21(IDX2)  NOT =  ZERO
                         MOVE    IDX2     TO   WRK-ZAINUM-CNT
                     END-IF
                   END-PERFORM
                   COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
                   MOVE    WRK-ZAINUM     TO   RECE220-ZAINUM
                WHEN     "6"
                WHEN     "7"
                   COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
                   MOVE    WRK-ZAINUM     TO   RECE220-ZAINUM
                WHEN     OTHER
                   MOVE    NACCT-ZAINUM   TO   RECE220-ZAINUM
               END-EVALUATE
             WHEN      3
               MOVE    ZERO               TO   WRK-ZAINUM-CNT
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                         UNTIL   IDX2   >   31
                          OR     WRK-ZAINUM-CNT  NOT =  ZERO
                 IF  WRK-DAY21-X(IDX2)  NOT =  ZERO
                     MOVE    IDX2         TO   WRK-ZAINUM-CNT
                 END-IF
               END-PERFORM
               COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
               MOVE    WRK-ZAINUM         TO   RECE220-ZAINUM
             WHEN      4
               MOVE    ZERO               TO   WRK-ZAINUM-CNT
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                         UNTIL   IDX2   >   31
                          OR     WRK-ZAINUM-CNT  NOT =  ZERO
                 IF  WRK-DAY21-Y(IDX2)  NOT =  ZERO
                     MOVE    IDX2         TO   WRK-ZAINUM-CNT
                 END-IF
               END-PERFORM
               COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
               MOVE    WRK-ZAINUM         TO   RECE220-ZAINUM
           END-EVALUATE
      *
           MOVE    RECE200-PTNUM        TO   RECE220-PTNUM
           MOVE    NACCT-SRYKA          TO   RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO   RECE220-HKNCOMBI
           MOVE    SPACE                TO   RECE220-JIHOKBN
      *
           EVALUATE  FLG-HENSYU
             WHEN      1
               MOVE  NACCT-ZAITEN       TO   RECE220-ZAITEN
               MOVE  NACCT-MEISAISU     TO   RECE220-MEISAISU
               MOVE  CNT-DAY21          TO   RECE220-ZAIKAISU
      *********MOVE  NACCT-SYUTEN1      TO   RECE220-SYUTEN1
               MOVE  NACCT-ZAITEN       TO   RECE220-SYUTEN1
               MOVE  NACCT-SRYCDTOTAL   TO   RECE220-SRYCDTOTAL
               IF    NACCT-ZAISKBKBN  =  "7"
                   MOVE   199000510     TO   RECE220-SRYCDTOTAL
               END-IF
             WHEN      2
               MOVE  WRK-ZAITEN-W       TO   RECE220-ZAITEN
               COMPUTE  RECE220-MEISAISU   =  NACCT-MEISAISU  +  1
               MOVE  CNT-DAY21          TO   RECE220-ZAIKAISU
               MOVE  WRK-ZAITEN-W       TO   RECE220-SYUTEN1
               COMPUTE  RECE220-SRYCDTOTAL =  NACCT-SRYCDTOTAL
                                           +  WRK-KANGOGEN-SRYCD-W
             WHEN      3
               MOVE  NACCT-ZAITEN       TO   RECE220-ZAITEN
               MOVE  NACCT-MEISAISU     TO   RECE220-MEISAISU
               MOVE  CNT-DAY21-X        TO   RECE220-ZAIKAISU
      *********MOVE  NACCT-SYUTEN1      TO   RECE220-SYUTEN1
               MOVE  NACCT-ZAITEN       TO   RECE220-SYUTEN1
               MOVE  NACCT-SRYCDTOTAL   TO   RECE220-SRYCDTOTAL
             WHEN      4
               MOVE  WRK-ZAITEN-W       TO   RECE220-ZAITEN
               COMPUTE  RECE220-MEISAISU   =  NACCT-MEISAISU  +  1
               MOVE  CNT-DAY21-Y        TO   RECE220-ZAIKAISU
               MOVE  WRK-ZAITEN-W       TO   RECE220-SYUTEN1
               COMPUTE  RECE220-SRYCDTOTAL =  NACCT-SRYCDTOTAL
                                           +  WRK-KANGOGEN-SRYCD-W
           END-EVALUATE
      *
      *****MOVE    NACCT-SURYOUTOTAL    TO   RECE220-SURYOUTOTAL
           MOVE    RECE220-MEISAISU     TO   RECE220-SURYOUTOTAL
           MOVE    NACCT-SYUTEN2        TO   RECE220-SYUTEN2
           MOVE    NACCT-YKZTEN         TO   RECE220-YKZTEN
           MOVE    NACCT-KIZAITEN       TO   RECE220-KIZAITEN
           IF      NACCT-ZAISKBKBN  =  "7"
      *******MOVE  NACCT-SYUTEN1        TO   RECE220-KIZAITEN
             MOVE  NACCT-ZAITEN         TO   RECE220-KIZAITEN
             MOVE  ZERO                 TO   RECE220-SYUTEN1
           END-IF
           MOVE    NACCT-DRCD           TO   RECE220-DRCD
           MOVE    SPACE                TO   RECE220-ZAIREQFLG
           MOVE    ZERO                 TO   RECE220-TEIGENRATE
      *
           MOVE    ZERO                 TO   WK-TAIHI-DAY
           MOVE    ZERO                 TO   WK-TAIHI-FLG
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                     UNTIL   IDX2   >   31
             EVALUATE  FLG-HENSYU
               WHEN      1
               WHEN      2
                 MOVE   WRK-DAY21(IDX2) TO   RECE220-KAISU (1 IDX2)
               WHEN      3
                 MOVE   WRK-DAY21-X(IDX2)
                                        TO   RECE220-KAISU (1 IDX2)
               WHEN      4
                 MOVE   WRK-DAY21-Y(IDX2)
                                        TO   RECE220-KAISU (1 IDX2)
             END-EVALUATE
             IF    (WK-TAIHI-FLG               =  ZERO)
             AND   (RECE220-KAISU(1 IDX2)  NOT =  ZERO)
                 MOVE   IDX2            TO   WK-TAIHI-DAY
                 MOVE   1               TO   WK-TAIHI-FLG
             END-IF
           END-PERFORM
      *
           .
       210205-RECE220-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       210206-RECE230-HENSYU2-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
      *
               MOVE    "tbl_nyuinact"   TO   MCP-TABLE
               MOVE    "key9"           TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =  1)
                   PERFORM   2102061-RECE230-TENSOU2-SEC
                   IF      NACCT-ZAISKBKBN      =       "7"
                      PERFORM   2102062-RECE200-HENSYU2-SEC
                   END-IF
                   PERFORM   RECE230-WRITE-SEC
                   MOVE    "tbl_nyuinact"     TO   MCP-TABLE
                   MOVE    "key9"             TO   MCP-PATHNAME
                   PERFORM        900-NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210206-RECE230-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       2102061-RECE230-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE230-REC
           INITIALIZE                           RECE230-REC
           MOVE    RECE200-KEY          TO      RECE230-KEY1
      *
           EVALUATE  FLG-HENSYU
             WHEN      1
             WHEN      2
               EVALUATE  NACCT-ZAISKBKBN
                 WHEN    "1"
                   COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
                   MOVE    WRK-ZAINUM   TO      RECE230-ZAINUM
                WHEN     "6"
                WHEN     "7"
                   COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
                   MOVE    WRK-ZAINUM   TO      RECE230-ZAINUM
                WHEN     OTHER
                   MOVE    NSRY-ZAINUM  TO      RECE230-ZAINUM
               END-EVALUATE
             WHEN      3
               COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
               MOVE    WRK-ZAINUM       TO      RECE230-ZAINUM
             WHEN      4
               COMPUTE WRK-ZAINUM  = 99999800  +  WRK-ZAINUM-CNT
               MOVE    WRK-ZAINUM       TO      RECE230-ZAINUM
           END-EVALUATE
      *
           MOVE    NSRY-RENNUM          TO      RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO      RECE230-PTNUM
           MOVE    NSRY-SRYKA           TO      RECE230-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE230-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE230-SRYSYUKBN
           IF      NACCT-ZAISKBKBN  =  "1"
             IF  (NACCT-SRYCDTOTAL =   190799410)
             OR  (NACCT-SRYCDTOTAL =   190799510)
                MOVE    "90"            TO      RECE230-SRYKBN
                MOVE    "900"           TO      RECE230-SRYSYUKBN
             END-IF
           END-IF
           IF      NACCT-ZAISKBKBN  =  "6"
             EVALUATE  WRK-NYUTENTTLKBN
               WHEN      903
                MOVE    "90"            TO      RECE230-SRYKBN
                MOVE    "900"           TO      RECE230-SRYSYUKBN
               WHEN      920
                MOVE    "92"            TO      RECE230-SRYKBN
                MOVE    "920"           TO      RECE230-SRYSYUKBN
             END-EVALUATE
           END-IF
           IF      NACCT-ZAISKBKBN  =  "7"
                MOVE     "80"           TO      RECE230-SRYKBN
                MOVE     "800"          TO      RECE230-SRYSYUKBN
           END-IF
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF    NSRY-SRYCD(IDX2)(1:1)  =  "0" OR "1" OR "8"
               ADD     1                   TO  IDX4
               MOVE    NSRY-SRYCD(IDX2)    TO  RECE230-SRYCD(IDX4)
               IF      NACCT-ZAISKBKBN  =  "7"
                  MOVE     "199000510"     TO  RECE230-SRYCD(IDX4)
               END-IF
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE230-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE230-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE230-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE230-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE230-INPUTNUM(IDX4)
               MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN  =  "7"
                  MOVE     199000510       TO  WRK-SRYCD
               END-IF
               MOVE    RECE230-SRYYM       TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM 900-TENSU-TBL-SEC
               PERFORM 21020611-RECE230-TENSOU2X-SEC
               IF    ((NSRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (NSRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( NSRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    NSRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE230-NAME(IDX4)
               END-IF
      *
               IF    NACCT-ZAISKBKBN    NOT =    "1"
                 MOVE    NSRY-SRYCD(IDX2)  TO  WRK-SRYCD
                 IF      NACCT-ZAISKBKBN  =  "7"
                    MOVE     199000510     TO  WRK-SRYCD
                 END-IF
                 MOVE    ZERO              TO  WRK-FUKA-KINGAKU
                 PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                           UNTIL   FUKA-IDX   >    9
                   MOVE   WRK-SRYCD(FUKA-IDX:1)
                                           TO  WRK-FUKA-VALUE
                   IF  WRK-FUKA-VALUE  =  ZERO
                     CONTINUE
                   ELSE
                     COMPUTE  RECE220-RUIKEITOTAL  =
                              RECE220-RUIKEITOTAL  +
                            ( WRK-FUKA-VALUE    * WRK-SRYCD)
                     COMPUTE  WRK-FUKA-KINGAKU    =
                              WRK-FUKA-KINGAKU    +
                            ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                * NSRY-SRYSURYO(IDX2))
                   END-IF
                 END-PERFORM
                 COMPUTE  RECE220-RUIKEITOTAL  =
                          RECE220-RUIKEITOTAL  +
                          WRK-FUKA-KINGAKU
      *
                 MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                 PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                           UNTIL   FUKA-IDX   >    8
                   MOVE   NSRY-SRYSURYO(IDX2)(FUKA-IDX:1)
                                  TO    WRK-FUKA-VALUE
                   IF  WRK-FUKA-VALUE  =  ZERO
                     CONTINUE
                   ELSE
                     COMPUTE  WRK-FUKA-KINGAKU  =
                              WRK-FUKA-KINGAKU  +
                            ((WRK-FUKA-VALUE^4) * FUKA-IDX)
                   END-IF
                 END-PERFORM
                 COMPUTE  RECE220-RUIKEITOTAL  =
                          RECE220-RUIKEITOTAL  +
                          WRK-FUKA-KINGAKU
               END-IF
             END-IF
      *
             IF    (IDX2  =  1             )
             AND   (FLG-HENSYU  =  2  OR  4)
               ADD     1                     TO  IDX4
               MOVE    WRK-KANGOGEN-SRYCD-W  TO  RECE230-SRYCD(IDX4)
               MOVE    1                     TO  RECE230-SRYSURYO(IDX4)
               MOVE    ZERO                  TO  RECE230-SRYKAISU(IDX4)
               MOVE    SPACE                 TO  RECE230-MEISKYFLG(IDX4)
               MOVE    SPACE                 TO  RECE230-AUTOKBN(IDX4)
               MOVE    ZERO                  TO  RECE230-INPUTNUM(IDX4)
               MOVE    WRK-KANGOGEN-SRYCD-W  TO  WRK-SRYCD
               MOVE    RECE230-SRYYM         TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY          TO  WK-TAIHI-SRYD
               PERFORM 900-TENSU-TBL-SEC
               PERFORM 21020611-RECE230-TENSOU2X-SEC
             END-IF
           END-PERFORM
      *
           .
       2102061-RECE230-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       21020611-RECE230-TENSOU2X-SEC                SECTION.
      *
           MOVE  TNS-TANICD           TO  RECE230-TANICD(IDX4)
           MOVE  TNS-TANIMOJISU       TO  RECE230-TANIMOJISU(IDX4)
           MOVE  TNS-TANINAME         TO  RECE230-TANINAME(IDX4)
           MOVE  TNS-TENSIKIBETU      TO  RECE230-TENSIKIBETU(IDX4)
           MOVE  TNS-TEN              TO  RECE230-TEN(IDX4)
           MOVE  TNS-NYUTENTTLKBN     TO  RECE230-TENTTLKBN(IDX4)
           MOVE  TNS-YUKOKETA         TO  RECE230-YUKOKETA(IDX4)
           MOVE  TNS-DATAKBN          TO  RECE230-DATAKBN(IDX4)
           MOVE  TNS-HOUKNSKBN        TO  RECE230-HOUKNSKBN(IDX4)
           MOVE  TNS-KNSJISKBN        TO  RECE230-KNSJISKBN(IDX4)
           MOVE  TNS-CDKBN-KBN        TO  RECE230-CDKBN-KBN(IDX4)
           MOVE  TNS-CDKBN-SYO        TO  RECE230-CDKBN-SYO(IDX4)
           MOVE  TNS-CDKBN-BU         TO  RECE230-CDKBN-BU(IDX4)
           MOVE  TNS-CDKBN-KBNNUM     TO  RECE230-CDKBN-KBNNUM(IDX4)
           MOVE  TNS-CDKBN-KBNNUM-EDA TO  RECE230-CDKBN-KBNNUM-EDA(IDX4)
           MOVE  TNS-CDKBN-KOUBAN     TO  RECE230-CDKBN-KOUBAN(IDX4)
           MOVE  TNS-KOUHYOJYUNNUM    TO  RECE230-KOUHYOJYUNNUM(IDX4)
           MOVE  TNS-SRYSYUKBN        TO  RECE230-SRYSYUKBN-X(IDX4)
           MOVE  TNS-KOKUJISKBKBN1    TO  RECE230-KOKUJISKBKBN1(IDX4)
           MOVE  TNS-KZMCOMPSIKIBETU  TO  RECE230-KZMCOMPSIKIBETU(IDX4)
           MOVE  TNS-CHUKSNCD         TO  RECE230-CHUKSNCD(IDX4)
           MOVE  TNS-CHUKSNTUBAN      TO  RECE230-CHUKSNTUBAN(IDX4)
           MOVE  TNS-KENTAICOMMENT    TO  RECE230-KENTAICOMMENT(IDX4)
           MOVE  TNS-KNSJISGRPKBN     TO  RECE230-KNSJISGRPKBN(IDX4)
           MOVE  TNS-TOKUKIZAISBT1    TO  RECE230-TOKUKIZAISBT1(IDX4)
           MOVE  TNS-SANSOKBN         TO  RECE230-SANSOKBN(IDX4)
           MOVE  TNS-NAME             TO  RECE230-NAME(IDX4)
      *
           .
       21020611-RECE230-TENSOU2X-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° Œ≈Õ‹√¥≈ˆºÍ≈ˆ°À
      *****************************************************************
       2102062-RECE200-HENSYU2-SEC                SECTION.
      *
      *    §Ω§Œ¬æ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
           COMPUTE   RECE200-TENSU(21 2)  =
                     RECE200-TENSU(21 2)  +
                    (NACCT-ZAITEN   *   CNT-DAY21)
      *
           .
       2102062-RECE200-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ≥∞«Ò§Œ∆˛±°¥À‹Œ¡ ‘Ω∏
      **************************************************************
       210209-NYUINACCT-HEN-SEC           SECTION.
      *
           MOVE     ZERO     TO     SYUKEI-IDX3
      *
           PERFORM    VARYING  SYUKEI-IDX     FROM    1   BY  1
                  UNTIL  SYUKEI-IDX            > 62
      *
             PERFORM  VARYING  SYUKEI-IDX2    FROM    1   BY  1
                  UNTIL  SYUKEI-IDX2           > 10
                   OR    WRK-GAI-DAY(SYUKEI-IDX SYUKEI-IDX2) = ZERO
      *
               ADD      1      TO     SYUKEI-IDX3
      *
               MOVE   WRK-GAI-DAY(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-DAY     (SYUKEI-IDX3)
               IF     WRK-GAI-KANGOGEN-SRYCD(SYUKEI-IDX SYUKEI-IDX2)
                                                NOT = SPACE
                 MOVE   WRK-GAI-KANGOGEN-SRYCD(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-KANGOGEN-SRYCD-9
                                                          (SYUKEI-IDX3)
               ELSE
                 MOVE   ZERO        TO   WRK-GAI2-KANGOGEN-SRYCD-9
                                                          (SYUKEI-IDX3)
               END-IF
               EVALUATE  SYUKEI-IDX
                  WHEN     1
                     MOVE   "190107290" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     2
                     MOVE   "190107390" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     6
                     MOVE   "190107490" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     7
                     MOVE   "190107590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN     4
                  WHEN     9
                  WHEN    11
                  WHEN    13
                  WHEN    15
                  WHEN    16
                  WHEN    19
                  WHEN    20
                  WHEN    47
                  WHEN    48
                  WHEN    51
                  WHEN    52
                  WHEN    55
                  WHEN    56
                     PERFORM  2102091-NYUINACCT-HEN-TAIN-SEC
                  WHEN    23  THRU  24
                  WHEN    27  THRU  28
      ***         WHEN    35  THRU  36
                  WHEN    43  THRU  44
                  WHEN    59  THRU  60
                     PERFORM  2102093-NYUINACCT-HEN-GEN-SEC
               END-EVALUATE
               MOVE   WRK-GAI-ZAITEN(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-ZAITEN  (SYUKEI-IDX3)
               MOVE   WRK-GAI-ZAIKAISU(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-ZAIKAISU(SYUKEI-IDX3)
               EVALUATE  SYUKEI-IDX
                  WHEN     1
                  WHEN     2
                  WHEN     4
                  WHEN    11
                  WHEN    15
                  WHEN    19
                  WHEN    23
                  WHEN    27
      ***         WHEN    35
                  WHEN    43
                  WHEN    47
                  WHEN    51
                  WHEN    55
                  WHEN    59
                     MOVE    1      TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)  TO   SYUKEI-IDX4
                     IF WRK-GAI2-SRYCD2-9(SYUKEI-IDX3)       NOT = ZERO
                        MOVE    3   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                     IF WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
                        MOVE    3   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                     IF WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
                        MOVE    3   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                  WHEN     6
                  WHEN     7
                  WHEN     9
                  WHEN    13
                  WHEN    16
                  WHEN    20
                  WHEN    24
                  WHEN    28
      ***         WHEN    36
                  WHEN    44
                  WHEN    48
                  WHEN    52
                  WHEN    56
                  WHEN    60
                     MOVE    2      TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)  TO   SYUKEI-IDX4
                     IF WRK-GAI2-SRYCD2-9(SYUKEI-IDX3)       NOT = ZERO
                        MOVE    4   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                     IF WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
                        MOVE    4   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
                     IF WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
                        MOVE    4   TO   WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)
                     END-IF
               END-EVALUATE
               MOVE  WRK-GAI-KAISU-T(SYUKEI-IDX SYUKEI-IDX2)
                                    TO   WRK-GAI2-KAISU-T(SYUKEI-IDX3)
      *
             END-PERFORM
      *
           END-PERFORM
      *
      *    ∆¸…’ΩÁ§À ¬§”¬ÿ§®
      *
           PERFORM    VARYING   SYUKEI-IDX     FROM    1   BY  1
                  UNTIL  ( SYUKEI-IDX                   >   20  )
                    OR   ( WRK-GAI2-DAY(SYUKEI-IDX)     =  ZERO )
      *
             COMPUTE    SYUKEI-IDX2  =  SYUKEI-IDX  +  1
      *
             PERFORM  VARYING  SYUKEI-IDX3  FROM  SYUKEI-IDX2  BY  1
                        UNTIL  ( SYUKEI-IDX3                >   20  )
                          OR   ( WRK-GAI2-DAY(SYUKEI-IDX3)  =  ZERO )
      *
                IF       WRK-GAI2-DAY(SYUKEI-IDX)  >
                                           WRK-GAI2-DAY(SYUKEI-IDX3)
                   MOVE  WRK-GAI-T2(SYUKEI-IDX)  
                                           TO  WRK-GAI-DATA
                   MOVE  WRK-GAI-T2(SYUKEI-IDX3)  
                                           TO  WRK-GAI-T2(SYUKEI-IDX)
                   MOVE  WRK-GAI-DATA      TO  WRK-GAI-T2(SYUKEI-IDX3)
                END-IF
      *
             END-PERFORM
      *
           END-PERFORM
      *
           .
      *
       210209-NYUINACCT-HEN-EXT.
           EXIT.
      *
      **************************************************************
      *    ¬æ∞ÂŒ≈µ°¥ÿºıø«ª˛§Œø«Œ≈π‘∞Ÿ•≥°º•…ºË∆¿
      **************************************************************
       2102091-NYUINACCT-HEN-TAIN-SEC           SECTION.
      *
           MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)   TO   SYUKEI-IDX4
      *
           IF    RECE200-SRYYM   >=  "201804"
              EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
      *        ∆√ƒÍ∆˛±°Œ¡
               WHEN   200  THRU  257
               WHEN   260  THRU  263
      *          «ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡°¢¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡
      *          √œ∞Ë∞‹π‘µ°«Ω∂Ø≤Ω…¬≈Ô∆˛±°Œ¡
                 IF  WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
                                         = 255 OR 254 OR 257
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                      MOVE "190108790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                      MOVE "190142090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    15
                    WHEN    16
                      MOVE "190182990" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    47
                    WHEN    48
                      MOVE "190214390" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    51
                    WHEN    52
                      MOVE "190213990" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    55
                    WHEN    56
                      MOVE "190214190" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 ELSE
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                      MOVE "190108790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                      MOVE "190142090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    47
                    WHEN    48
                      MOVE "190214390" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    51
                    WHEN    52
                      MOVE "190213990" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 END-IF
      *        Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
               WHEN    11  THRU   14
               WHEN    51
               WHEN   901
                 EVALUATE   SYUKEI-IDX
                  WHEN     4
                  WHEN     9
                    MOVE   "190108890" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    11
                  WHEN    13
                    MOVE   "190142190" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    47
                  WHEN    48
                    MOVE   "190214290" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    51
                  WHEN    52
                    MOVE   "190213890" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
               WHEN    26
               WHEN    53
               WHEN   909
                 EVALUATE   SYUKEI-IDX
                  WHEN     4
                  WHEN     9
                    MOVE   "190108890" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    11
                  WHEN    13
                    MOVE   "190142190" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    15
                  WHEN    16
                    MOVE   "190109090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    47
                  WHEN    48
                    MOVE   "190214290" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    51
                  WHEN    52
                    MOVE   "190213890" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    55
                  WHEN    56
                    MOVE   "190214090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤°¶√ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
               WHEN   258  THRU  259
                 CONTINUE
               WHEN   OTHER
                 EVALUATE   SYUKEI-IDX
                  WHEN    11
                  WHEN    13
                    MOVE   "190141590" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    47
                  WHEN    48
                    MOVE   "190213790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
              END-EVALUATE
           ELSE
            IF   RECE200-SRYYM   >=  "201604"
              EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
      *        ∆√ƒÍ∆˛±°Œ¡
               WHEN   201  THRU  206
               WHEN   208  THRU  210
               WHEN   212  THRU  217
               WHEN   221
               WHEN   227  THRU  229
               WHEN   232  THRU  236
               WHEN   238  THRU  239
               WHEN   241  THRU  249
               WHEN   250  THRU  254
               WHEN   257  THRU  267
      *        --- H28.4 TUIKA ---
               WHEN   268  THRU  270
      *          «ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡°¢¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡
      *          √œ∞Ë∞‹π‘µ°«Ω∂Ø≤Ω…¬≈Ô∆˛±°Œ¡
                 IF  WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
                                         = 221 OR 232 OR 217 OR 268
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                      MOVE "190108790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                      MOVE "190142090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    15
                    WHEN    16
                      MOVE "190182990" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 ELSE
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                      MOVE "190108790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                      MOVE "190142090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 END-IF
      *        Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
               WHEN   004  THRU  008
               WHEN   010  THRU  012
               WHEN   152
               WHEN   173  THRU  190
               WHEN   196
               WHEN   199
      *        --- H28.4 TUIKA ---
               WHEN   048  THRU  049
               WHEN   051  THRU  054
               WHEN   057  THRU  068
                 EVALUATE   SYUKEI-IDX
                  WHEN     4
                  WHEN     9
                    MOVE   "190108890" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    11
                  WHEN    13
                    MOVE   "190142190" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
               WHEN   153  THRU  158
                 EVALUATE   SYUKEI-IDX
                  WHEN     4
                  WHEN     9
                    MOVE   "190108890" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    11
                  WHEN    13
                    MOVE   "190142190" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                  WHEN    15
                  WHEN    16
                    MOVE   "190109090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤°¶√ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
               WHEN   226
               WHEN   240
                 CONTINUE
               WHEN   OTHER
                 EVALUATE   SYUKEI-IDX
                  WHEN    11
                  WHEN    13
                    MOVE   "190141590" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
              END-EVALUATE
            ELSE
              EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
      *        ∆√ƒÍ∆˛±°¥À‹Œ¡
               WHEN   099  THRU  100
                 IF   RECE200-SRYYM  >=  "201004"
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                     MOVE "190108890" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                     MOVE "190142190" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 END-IF
      *        ∆√ƒÍ∆˛±°Œ¡
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  229
               WHEN   230  THRU  239
               WHEN   241  THRU  249
               WHEN   250  THRU  255
      *        (H26-ADD-ST)
               WHEN   256
               WHEN   257  THRU  259
               WHEN   260  THRU  261
               WHEN   262  THRU  265
               WHEN   266  THRU  267
      *        (H26-ADD-ED)
      *          «ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡
                 IF    WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4) = 221 OR 232
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                        MOVE "190108790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                        MOVE "190142090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    15
                    WHEN    16
                        MOVE "190153190" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    19
                    WHEN    20
                        MOVE "190153390" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 ELSE
      *            ¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡°¢¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡
      *            ¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡°¢¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡
                   IF  WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4) =
                          229 OR 238 OR 215 OR 216 OR 217 OR 239 OR 252
                     EVALUATE   SYUKEI-IDX
                      WHEN     4
                      WHEN     9
                        MOVE "190108790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                      WHEN    11
                      WHEN    13
                        MOVE "190142090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                      WHEN    15
                      WHEN    16
                        MOVE "190153190" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                     END-EVALUATE
                   ELSE
                     EVALUATE   SYUKEI-IDX
                      WHEN     4
                      WHEN     9
                        MOVE "190108790" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                      WHEN    11
                      WHEN    13
                        MOVE "190142090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                     END-EVALUATE
                   END-IF
                 END-IF
      *        Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
               WHEN   013  THRU  021
               WHEN   101  THRU  109
               WHEN   147  THRU  152
               WHEN   173  THRU  190
               WHEN   196
      *        (H26-ADD-ST)
               WHEN   199
               WHEN   004  THRU  008
               WHEN   010  THRU  012
      *        (H26-ADD-ED)
                 EVALUATE   SYUKEI-IDX
                   WHEN     4
                   WHEN     9
                     MOVE "190108890" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   WHEN    11
                   WHEN    13
                     MOVE "190142190" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *
               WHEN   075  THRU  080
                 EVALUATE   SYUKEI-IDX
                   WHEN     4
                   WHEN     9
                     MOVE "190108990" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
               WHEN   085  THRU  086
               WHEN   132  THRU  133
               WHEN   153  THRU  158
                 IF    WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4) =
                          153 OR 154 OR 155 OR 156 OR 157 OR 158
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                        MOVE "190109090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                        MOVE "190142290" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    19
                    WHEN    20
                        MOVE "190153290" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 ELSE
                   EVALUATE   SYUKEI-IDX
                    WHEN     4
                    WHEN     9
                        MOVE "190109090" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    WHEN    11
                    WHEN    13
                        MOVE "190142290" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 END-IF
      *        √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤°¶√ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
               WHEN   226
               WHEN   240
                 CONTINUE
      *        ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡
               WHEN   031
               WHEN   033
               WHEN   034
               WHEN   035
               WHEN   038
               WHEN   170
               WHEN   191
      *        (H26-ADD-ST)
               WHEN   017  THRU  020
      *        (H26-ADD-ED)
                 EVALUATE   SYUKEI-IDX
                   WHEN    11
                   WHEN    13
                     MOVE "190141590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   WHEN    19
                   WHEN    20
                     MOVE "190153090" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        ∑Î≥À…¬≈Ô∆˛±°¥À‹Œ¡
               WHEN   022
               WHEN   023
               WHEN   024
               WHEN   025
               WHEN   026
               WHEN   029
               WHEN   136
               WHEN   168
               WHEN   169
      *        (H26-ADD-ST)
               WHEN   013  THRU  016
      *        (H26-ADD-ED)
                 EVALUATE   SYUKEI-IDX
                   WHEN    11
                   WHEN    13
                     MOVE "190141590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   WHEN    19
                   WHEN    20
                     MOVE "190153090" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡
               WHEN   081
               WHEN   083
               WHEN   172
      *        (H26-ADD-ST)
               WHEN   032  THRU  037
      *        (H26-ADD-ED)
                 EVALUATE   SYUKEI-IDX
                   WHEN    11
                   WHEN    13
                     MOVE "190141590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   WHEN    19
                   WHEN    20
                     MOVE "190153090" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
      *        ¬æ
               WHEN   OTHER
                 EVALUATE   SYUKEI-IDX
                   WHEN    11
                   WHEN    13
                     MOVE "190141590" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 END-EVALUATE
              END-EVALUATE
            END-IF
           END-IF
           .
      *
       2102091-NYUINACCT-HEN-TAIN-EXT.
           EXIT.
      *
      **************************************************************
      *    ∏∫ªª§Œø«Œ≈π‘∞Ÿ•≥°º•…ºË∆¿
      **************************************************************
       2102093-NYUINACCT-HEN-GEN-SEC           SECTION.
      *
           MOVE   WRK-GAI2-DAY(SYUKEI-IDX3)   TO   SYUKEI-IDX4
      *
           IF    RECE200-SRYYM   >=  "201804"
             EVALUATE  SYUKEI-IDX
              WHEN     23
              WHEN     24
      ***     WHEN     35
      ***     WHEN     36
               EVALUATE   WRK-DAY22-CHUKSNCD(SYUKEI-IDX4)
                 WHEN   "9017"
                   EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
                     WHEN    27
                     WHEN    40
                       MOVE "190161570" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                     WHEN   OTHER
                       MOVE "190161470" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 WHEN   "9026"
                     MOVE   "190161870" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9030"
                     MOVE   "190162070" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9090"
                     MOVE   "190198570" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
               END-EVALUATE
               IF   SYUKEI-IDX = 35 OR 36
                     MOVE   "190141590" TO WRK-GAI2-SRYCD2(SYUKEI-IDX3)
               END-IF
              WHEN     27
              WHEN     28
              WHEN     43
              WHEN     44
              WHEN     59
              WHEN     60
               EVALUATE   WRK-DAY22-CHUKSNCD(SYUKEI-IDX4)
                 WHEN   "9017"
                   EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
                     WHEN    27
                     WHEN    40
                       MOVE "190161770" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                     WHEN   OTHER
                       MOVE "190161670" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 WHEN   "9026"
                     MOVE   "190161970" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9030"
                     MOVE   "190162170" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9090"
                     MOVE   "190198670" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
               END-EVALUATE
               IF   SYUKEI-IDX = 43 OR 44
                     MOVE   "190141590" TO WRK-GAI2-SRYCD2(SYUKEI-IDX3)
               END-IF
               IF   SYUKEI-IDX = 59 OR 60
                     MOVE   "190213790" TO WRK-GAI2-SRYCD2(SYUKEI-IDX3)
               END-IF
             END-EVALUATE
           ELSE
            IF   RECE200-SRYYM   >=  "201604"
             EVALUATE  SYUKEI-IDX
              WHEN     23
              WHEN     24
      ***     WHEN     35
      ***     WHEN     36
               EVALUATE   WRK-DAY22-CHUKSNCD(SYUKEI-IDX4)
                 WHEN   "9017"
                   EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
                     WHEN     9
                     WHEN    42   THRU   44
                     WHEN    47
                     WHEN   166   THRU  167
                     WHEN   197   THRU  198
                       MOVE "190161570" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                     WHEN   OTHER
                       MOVE "190161470" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 WHEN   "9026"
                     MOVE   "190161870" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9030"
                     MOVE   "190162070" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9090"
                     MOVE   "190198570" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
               END-EVALUATE
               IF   SYUKEI-IDX = 35 OR 36
                     MOVE   "190141590" TO WRK-GAI2-SRYCD2(SYUKEI-IDX3)
               END-IF
              WHEN     27
              WHEN     28
              WHEN     43
              WHEN     44
               EVALUATE   WRK-DAY22-CHUKSNCD(SYUKEI-IDX4)
                 WHEN   "9017"
                   EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
                     WHEN     9
                     WHEN    42   THRU   44
                     WHEN    47
                     WHEN   166   THRU  167
                     WHEN   197   THRU  198
                       MOVE "190161770" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                     WHEN   OTHER
                       MOVE "190161670" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-EVALUATE
                 WHEN   "9026"
                     MOVE   "190161970" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9030"
                     MOVE   "190162170" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9090"
                     MOVE   "190198670" TO WRK-GAI2-SRYCD(SYUKEI-IDX3)
               END-EVALUATE
               IF   SYUKEI-IDX = 43 OR 44
                     MOVE   "190141590" TO WRK-GAI2-SRYCD2(SYUKEI-IDX3)
               END-IF
             END-EVALUATE
            ELSE
             EVALUATE  SYUKEI-IDX
              WHEN     23
              WHEN     24
      ***     WHEN     35
      ***     WHEN     36
               EVALUATE   WRK-DAY22-CHUKSNCD(SYUKEI-IDX4)
                 WHEN   "9017"
                   IF   RECE200-SRYYM   >=  "201404"
                    IF  (WRK-DAY22-NYUKHNKBN(SYUKEI-IDX4)
                                             =   79 OR 88  )
                    OR  (WRK-DAY22-NYUINKHN(SYUKEI-IDX4)
                                             =  "190799510")
                      MOVE "190161570" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    ELSE
                      MOVE "190161470" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    END-IF
                   ELSE
                      MOVE "190161470" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-IF
                 WHEN   "9018"
                   MOVE   "190161570"  TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9026"
                   MOVE   "190161870"  TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9030"
                   MOVE   "190162070"  TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               END-EVALUATE
               IF   SYUKEI-IDX = 35 OR 36
                 EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
      *           ∆√ƒÍ∆˛±°¥À‹Œ¡
                  WHEN   099  THRU  100
                   MOVE   "190142190"  TO  WRK-GAI2-SRYCD2(SYUKEI-IDX3)
      *           ¬æ
                  WHEN   OTHER
                   MOVE   "190141590"  TO  WRK-GAI2-SRYCD2(SYUKEI-IDX3)
                 END-EVALUATE
               END-IF
              WHEN     27
              WHEN     28
              WHEN     43
              WHEN     44
               EVALUATE   WRK-DAY22-CHUKSNCD(SYUKEI-IDX4)
                 WHEN   "9017"
                   IF   RECE200-SRYYM   >=  "201404"
                    IF  (WRK-DAY22-NYUKHNKBN(SYUKEI-IDX4)
                                             =   79 OR 88  )
                    OR  (WRK-DAY22-NYUINKHN(SYUKEI-IDX4)
                                             =  "190799510")
                      MOVE "190161770" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    ELSE
                      MOVE "190161670" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                    END-IF
                   ELSE
                      MOVE "190161670" TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                   END-IF
                 WHEN   "9018"
                   MOVE   "190161770"  TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9026"
                   MOVE   "190161970"  TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
                 WHEN   "9030"
                   MOVE   "190162170"  TO  WRK-GAI2-SRYCD(SYUKEI-IDX3)
               END-EVALUATE
               IF   SYUKEI-IDX = 43 OR 44
                 EVALUATE   WRK-DAY22-NYUKHNKSNKBN(SYUKEI-IDX4)
      *           ∆√ƒÍ∆˛±°¥À‹Œ¡
                  WHEN   099  THRU  100
                   MOVE   "190142190"  TO  WRK-GAI2-SRYCD2(SYUKEI-IDX3)
      *           ¬æ
                  WHEN   OTHER
                   MOVE   "190141590"  TO  WRK-GAI2-SRYCD2(SYUKEI-IDX3)
                 END-EVALUATE
               END-IF
             END-EVALUATE
            END-IF
           END-IF
           .
      *
       2102093-NYUINACCT-HEN-GEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       210210-RECE220-TENSOU2-GAI-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 3
              MOVE    "90"              TO      RECE220-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE220-SRYKBN
           END-IF
           MOVE    NACCT-ZAISKBKBN      TO      RECE220-ZAISKBKBN
           COMPUTE WRK-ZAINUM    =  99999900  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO      RECE220-HKNCOMBI
           MOVE    SPACE                TO      RECE220-JIHOKBN
           MOVE    WRK-GAI2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE220-ZAITEN
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 2
              MOVE    1                 TO      RECE220-MEISAISU
              MOVE    WRK-GAI2-SRYCD-9 (SYUKEI-IDX3)
                                        TO      RECE220-SRYCDTOTAL
              MOVE    1                 TO      RECE220-SURYOUTOTAL
           ELSE
              MOVE    1                 TO      RECE220-MEISAISU
              MOVE    WRK-GAI2-SRYCD-9 (SYUKEI-IDX3)
                                        TO      RECE220-SRYCDTOTAL
              MOVE    1                 TO      RECE220-SURYOUTOTAL
              MOVE    WRK-GAI2-DAY(SYUKEI-IDX3)  TO   SYUKEI-IDX4
              IF WRK-GAI2-SRYCD2-9(SYUKEI-IDX3)       NOT = ZERO
                ADD   1   TO   RECE220-MEISAISU
                ADD   WRK-GAI2-SRYCD2-9(SYUKEI-IDX3)
                          TO   RECE220-SRYCDTOTAL
                ADD   1   TO   RECE220-SURYOUTOTAL
              END-IF
              IF WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
                ADD   1   TO   RECE220-MEISAISU
                ADD   WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)
                          TO   RECE220-SRYCDTOTAL
                ADD   1   TO   RECE220-SURYOUTOTAL
              END-IF
              IF WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
                ADD   1   TO   RECE220-MEISAISU
                ADD   WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)
                          TO   RECE220-SRYCDTOTAL
                ADD   1   TO   RECE220-SURYOUTOTAL
              END-IF
           END-IF
           IF  WRK-GAI2-KANGOGEN-SRYCD-9(SYUKEI-IDX3) NOT = ZERO
                ADD   1   TO   RECE220-MEISAISU
                ADD   WRK-GAI2-KANGOGEN-SRYCD-9(SYUKEI-IDX3)
                          TO   RECE220-SRYCDTOTAL
                ADD   1   TO   RECE220-SURYOUTOTAL
           END-IF
           MOVE    ZERO                 TO      RECE220-RUIKEITOTAL
           MOVE    WRK-GAI2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE220-ZAIKAISU
           MOVE    WRK-GAI2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE220-SYUTEN1
           MOVE    ZERO                 TO      RECE220-SYUTEN2
           MOVE    ZERO                 TO      RECE220-YKZTEN
           MOVE    ZERO                 TO      RECE220-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE220-DRCD
           MOVE    SPACE                TO      RECE220-ZAIREQFLG
           MOVE    ZERO                 TO      RECE220-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                        UNTIL    IDX2   >       31
                   MOVE    WRK-GAI2-KAISU(SYUKEI-IDX3 IDX2)
                                        TO    RECE220-KAISU (1 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE220-KAISU(1 IDX2)  NOT =  ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF
           END-PERFORM
           .
       210210-RECE220-TENSOU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       210211-RECE230-HENSYU2-GAI-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
           ELSE
               MOVE     1               TO      FLG-NYUINACT
           END-IF
           IF  FLG-NYUINACT   =   ZERO
               PERFORM   2102111-RECE230-TENSOU2-GAI-SEC
               PERFORM   RECE230-WRITE-SEC
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210211-RECE230-HENSYU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       2102111-RECE230-TENSOU2-GAI-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE230-REC
           INITIALIZE                           RECE230-REC
           MOVE    RECE200-KEY          TO      RECE230-KEY1
           COMPUTE WRK-ZAINUM    =  99999900  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE230-ZAINUM
           MOVE    NSRY-RENNUM          TO      RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO      RECE230-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE230-SRYKA
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 3
              MOVE    "90"              TO      RECE230-SRYKBN
           ELSE
              MOVE    "92"              TO      RECE230-SRYKBN
           END-IF 
           IF      WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =   1 OR 3
              MOVE    "900"             TO      RECE230-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE230-SRYSYUKBN
           END-IF 
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO  IDX11
           IF  WRK-GAI2-KANGOGEN-SRYCD-9(SYUKEI-IDX3) NOT = ZERO
             ADD     1                  TO  IDX11
             MOVE    WRK-GAI2-KANGOGEN-SRYCD-9(SYUKEI-IDX3)
                                        TO  WRK-SRYCD2
             PERFORM 21021111-RECE230-TENSOU2-G2-SEC
           END-IF
      *
           ADD       1                  TO  IDX11
           MOVE      WRK-GAI2-SRYCD(SYUKEI-IDX3)
                                        TO  WRK-SRYCD2
           PERFORM   21021111-RECE230-TENSOU2-G2-SEC
      *
           IF  WRK-GAI2-SRYKBNFLG(SYUKEI-IDX3)  =  3 OR 4
             MOVE    WRK-GAI2-DAY(SYUKEI-IDX3)
                                        TO  SYUKEI-IDX4
             IF   WRK-GAI2-SRYCD2-9(SYUKEI-IDX3)       NOT = ZERO
               ADD       1              TO  IDX11
               MOVE      WRK-GAI2-SRYCD2-9(SYUKEI-IDX3)
                                        TO  WRK-SRYCD2
               PERFORM   21021111-RECE230-TENSOU2-G2-SEC
             END-IF
             IF   WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)      NOT = ZERO
               ADD       1              TO  IDX11
               MOVE      WRK-DAY1-KSN-SRYCD(SYUKEI-IDX4)
                                        TO  WRK-SRYCD2
               PERFORM   21021111-RECE230-TENSOU2-G2-SEC
             END-IF
             IF   WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)  NOT = ZERO
               ADD       1              TO  IDX11
               MOVE      WRK-DAY1-HYOKETU-SRYCD(SYUKEI-IDX4)
                                        TO  WRK-SRYCD2
               PERFORM   21021111-RECE230-TENSOU2-G2-SEC
             END-IF
           END-IF
      *
           .
       2102111-RECE230-TENSOU2-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       21021111-RECE230-TENSOU2-G2-SEC                SECTION.
      *
           MOVE    WRK-SRYCD2       TO  RECE230-SRYCD(IDX11)
           MOVE    1                TO  RECE230-SRYSURYO(IDX11)
           MOVE    ZERO             TO  RECE230-SRYKAISU(IDX11)
           MOVE    SPACE            TO  RECE230-MEISKYFLG(IDX11)
           MOVE    SPACE            TO  RECE230-AUTOKBN(IDX11)
           MOVE    ZERO             TO  RECE230-INPUTNUM(IDX11)
           MOVE    WRK-SRYCD2       TO  WRK-SRYCD
           MOVE    RECE230-SRYYM    TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY     TO  WK-TAIHI-SRYD
           PERFORM 900-TENSU-TBL-SEC
           MOVE    IDX11            TO  IDX4
           PERFORM 21020611-RECE230-TENSOU2X-SEC
      *
           .
       21021111-RECE230-TENSOU2-G2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       210213-RECE220-TENSOU2-SKJ2-SEC               SECTION.
      *
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
              ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           END-IF
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
              ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           END-IF
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
              ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           END-IF
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
              ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 4)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           END-IF
      *
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
             EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
              WHEN      1
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)  NOT =  ZERO
                  ADD   197001310
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
                  ADD   197001570
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
                  ADD   197001670
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)  NOT =  ZERO
                  ADD   197003510
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
              WHEN      2
                ADD     197001710
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
             END-EVALUATE
           ELSE
             EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
              WHEN      1
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)  NOT =  ZERO
                  ADD   197000110
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
                  ADD   197000470
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
                  ADD   197000570
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)  NOT =  ZERO
                  ADD   197003110
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
              WHEN      2
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)  NOT =  ZERO
                  ADD   197000710
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
                IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)  NOT =  ZERO
                  ADD   197003210
                     TO   WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                END-IF
             END-EVALUATE
           END-IF
      *
           MOVE    SPACE                TO  RECE220-REC
           INITIALIZE                       RECE220-REC
           MOVE    RECE200-KEY          TO  RECE220-KEY1
           MOVE    "97"                 TO  RECE220-SRYKBN
           MOVE    "4"                  TO  RECE220-ZAISKBKBN
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE220-ZAINUM
           ELSE
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE220-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO  RECE220-PTNUM
      *
           MOVE    "00"                 TO  RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO  RECE220-HKNCOMBI
           MOVE    SPACE                TO  RECE220-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                                        TO  RECE220-SRYCDTOTAL
           MOVE    WRK-SKJ200604-MEISAISU   (SYUKEI-IDX3)
                                        TO  RECE220-MEISAISU
           MOVE    1                    TO  RECE220-ZAIKAISU
           MOVE    WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                                        TO  RECE220-SURYOUTOTAL
           MOVE    ZERO                 TO  RECE220-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE220-SYUTEN2
           MOVE    ZERO                 TO  RECE220-YKZTEN
           MOVE    ZERO                 TO  RECE220-KIZAITEN
           MOVE    SPACE                TO  RECE220-DRCD
           MOVE    SPACE                TO  RECE220-ZAIREQFLG
           MOVE    ZERO                 TO  RECE220-TEIGENRATE
           MOVE    1                    TO  RECE220-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
           .
       210213-RECE220-TENSOU2-SKJ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° ¥ƒ∂≠°À° £∆£∞£∞£¥°À
      *****************************************************************
       210219-RECE220-TENSOU2-KAN-SEC               SECTION.
      *
           MOVE    SPACE                TO  RECE220-REC
           INITIALIZE                       RECE220-REC
           MOVE    RECE200-KEY          TO  RECE220-KEY1
           MOVE    "97"                 TO  RECE220-SRYKBN
           MOVE    "4"                  TO  RECE220-ZAISKBKBN
           COMPUTE WRK-ZAINUM  =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO  RECE220-PTNUM
      *
           MOVE    "00"                 TO  RECE220-SRYKA
           MOVE    RECE200-HKNCOMBI     TO  RECE220-HKNCOMBI
           MOVE    SPACE                TO  RECE220-JIHOKBN
      *
           EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
             WHEN      1
                MOVE    197001410       TO  RECE220-SRYCDTOTAL
             WHEN      2
                MOVE    197001810       TO  RECE220-SRYCDTOTAL
           END-EVALUATE
           MOVE    1                    TO  RECE220-MEISAISU
           MOVE    1                    TO  RECE220-ZAIKAISU
           MOVE    1                    TO  RECE220-SURYOUTOTAL
           MOVE    ZERO                 TO  RECE220-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE220-SYUTEN2
           MOVE    ZERO                 TO  RECE220-YKZTEN
           MOVE    ZERO                 TO  RECE220-KIZAITEN
           MOVE    SPACE                TO  RECE220-DRCD
           MOVE    SPACE                TO  RECE220-ZAIREQFLG
           MOVE    ZERO                 TO  RECE220-TEIGENRATE
           MOVE    1                    TO  RECE220-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
           .
       210219-RECE220-TENSOU2-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£µ°À
      *****************************************************************
       210214-RECE230-HENSYU2-SKJ2-SEC               SECTION.
      *
           MOVE    SPACE                TO   RECE230-REC
           INITIALIZE                        RECE230-REC
           MOVE    RECE200-KEY          TO   RECE230-KEY1
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO   RECE230-ZAINUM
           ELSE
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO   RECE230-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
           MOVE    1                    TO   RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
           MOVE    "00"                 TO   RECE230-SRYKA
           MOVE    "97"                 TO   RECE230-SRYKBN
           MOVE    "970"                TO   RECE230-SRYSYUKBN
           MOVE    ZERO                 TO   RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
      **>              UNTIL     IDX2     >     3
                       UNTIL     IDX2     >     4
      *
             EVALUATE   IDX2
               WHEN    1
                 MOVE     1     TO   IDX30
               WHEN    2
                 MOVE     4     TO   IDX30
               WHEN    3
                 MOVE     2     TO   IDX30
               WHEN    4
                 MOVE     3     TO   IDX30
             END-EVALUATE
      *
      **>    IF  WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2)  NOT =  ZERO
             IF  WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX30) NOT =  ZERO
      *
               ADD     1                    TO  IDX4
               IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
                 WHEN      1
      **>          EVALUATE    IDX2
      **>            WHEN       1
      **>                MOVE  "197001310"  TO  RECE230-SRYCD(IDX4)
      **>            WHEN       2
      **>                MOVE  "197001570"  TO  RECE230-SRYCD(IDX4)
      **>            WHEN       3
      **>                MOVE  "197001670"  TO  RECE230-SRYCD(IDX4)
      **>          END-EVALUATE
                   EVALUATE    IDX30
                     WHEN       1
                         MOVE  "197001310"  TO  RECE230-SRYCD(IDX4)
                     WHEN       2
                         MOVE  "197001570"  TO  RECE230-SRYCD(IDX4)
                     WHEN       3
                         MOVE  "197001670"  TO  RECE230-SRYCD(IDX4)
                     WHEN       4
                         MOVE  "197003510"  TO  RECE230-SRYCD(IDX4)
                   END-EVALUATE
                 WHEN      2
                         MOVE  "197001710"  TO  RECE230-SRYCD(IDX4)
                END-EVALUATE
               ELSE
                EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
                 WHEN      1
      **>          EVALUATE    IDX2
      **>            WHEN       1
      **>                MOVE  "197000110"  TO  RECE230-SRYCD(IDX4)
      **>            WHEN       2
      **>                MOVE  "197000470"  TO  RECE230-SRYCD(IDX4)
      **>            WHEN       3
      **>                MOVE  "197000570"  TO  RECE230-SRYCD(IDX4)
      **>          END-EVALUATE
                   EVALUATE    IDX30
                     WHEN       1
                         MOVE  "197000110"  TO  RECE230-SRYCD(IDX4)
                     WHEN       2
                         MOVE  "197000470"  TO  RECE230-SRYCD(IDX4)
                     WHEN       3
                         MOVE  "197000570"  TO  RECE230-SRYCD(IDX4)
                     WHEN       4
                         MOVE  "197003110"  TO  RECE230-SRYCD(IDX4)
                   END-EVALUATE
                 WHEN      2
      **>                MOVE  "197000710"  TO  RECE230-SRYCD(IDX4)
                   EVALUATE    IDX30
                     WHEN       1
                         MOVE  "197000710"  TO  RECE230-SRYCD(IDX4)
                     WHEN       4
                         MOVE  "197003210"  TO  RECE230-SRYCD(IDX4)
                   END-EVALUATE
                END-EVALUATE
               END-IF
      **>      MOVE    WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2)
               MOVE    WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX30)
                                            TO  RECE230-SRYSURYO(IDX4)
               MOVE    ZERO                 TO  RECE230-SRYKAISU(IDX4)
               MOVE    SPACE                TO  RECE230-MEISKYFLG(IDX4)
               MOVE    SPACE                TO  RECE230-AUTOKBN(IDX4)
               MOVE    ZERO                 TO  RECE230-INPUTNUM(IDX4)
      *
               MOVE    RECE230-SRYCD(IDX4)  TO  WRK-SRYCD
               MOVE    RECE230-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
               PERFORM 900-TENSU-TBL-SEC
               PERFORM 21020611-RECE230-TENSOU2X-SEC
      *
               COMPUTE RECE220-ZAITEN =
                       RECE220-ZAITEN +
      **>         (TNS-TEN * WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2))
                  (TNS-TEN * WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX30))
               COMPUTE RECE220-SYUTEN1 =
                       RECE220-SYUTEN1 +
      **>         (TNS-TEN * WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX2))
                  (TNS-TEN * WRK-SKJ200604-DAY(SYUKEI-IDX3 IDX30))
             END-IF
      *
           END-PERFORM
      *
           .
       210214-RECE230-HENSYU2-SKJ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° ¥ƒ∂≠°À° £∆£∞£∞£µ°À
      *****************************************************************
       210220-RECE230-HENSYU2-KAN-SEC               SECTION.
      *
           MOVE    SPACE                TO   RECE230-REC
           INITIALIZE                        RECE230-REC
           MOVE    RECE200-KEY          TO   RECE230-KEY1
           COMPUTE WRK-ZAINUM   =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
           MOVE    1                    TO   RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
           MOVE    "00"                 TO   RECE230-SRYKA
           MOVE    "97"                 TO   RECE230-SRYKBN
           MOVE    "970"                TO   RECE230-SRYSYUKBN
           MOVE    ZERO                 TO   RECE230-JIHIMONEY
      *
      *
           EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
             WHEN      1
                 MOVE  "197001410"      TO  RECE230-SRYCD(1)
             WHEN      2
                 MOVE  "197001810"      TO  RECE230-SRYCD(1)
           END-EVALUATE
           MOVE    1                    TO  RECE230-SRYSURYO(1)
           MOVE    ZERO                 TO  RECE230-SRYKAISU(1)
           MOVE    SPACE                TO  RECE230-MEISKYFLG(1)
           MOVE    SPACE                TO  RECE230-AUTOKBN(1)
           MOVE    ZERO                 TO  RECE230-INPUTNUM(1)
      *
           MOVE    RECE230-SRYCD(1)     TO  WRK-SRYCD
           MOVE    RECE230-SRYYM        TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
           PERFORM 900-TENSU-TBL-SEC
           MOVE    1                    TO  IDX4
           PERFORM 21020611-RECE230-TENSOU2X-SEC
      *
           MOVE    TNS-TEN              TO  RECE220-ZAITEN
           MOVE    TNS-TEN              TO  RECE220-SYUTEN1
      *
           .
       210220-RECE230-HENSYU2-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œ∫Ó¿Æ° £∆£∞£∞£¥°À
      *****************************************************************
       2103-RECE220-HENSYU3-SEC                SECTION.
      *
      *    ≈¿≈©§Œ∑◊ªª
      *
           MOVE    RECE200-HOSPNUM      TO  SPA-HOSPNUM
      *
           INITIALIZE                      ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    RECE200-PTID        TO  ORCSTEN-PTID
           MOVE    RECE200-SRYYM       TO  ORCSTEN-SRYYM
           MOVE    RECE200-HKNCOMBI    TO  ORCSTEN-HKNCOMBI
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
      *    IF      ORCSTEN-ERRCD       =   ZERO
      *
      *           ÃÙ∫ﬁ
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >     31
      *
                    MOVE   ZERO      TO   FLG-FORMALNAME
                    IF  ORCSTEN-ZAITEN(IDY)  NOT =  ZERO
      *
                       MOVE   1      TO   FLG-SIKIBETU
      *
                       INITIALIZE         WRK-ORCSTEN-TABLE
                       MOVE   ZERO   TO   IDX6
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       50
                                OR    ORCSTEN-SRYCD(IDY IDX5)  = SPACE
                          ADD   1   TO   IDX6
                          MOVE  ORCSTEN-SRYCD(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SRYCD(IDX6)
                          MOVE  ORCSTEN-SURYO(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SURYO(IDX6)
                          IF  ORCSTEN-HAIK-SRYCD(IDY IDX5) NOT = SPACE
                             ADD   1   TO   IDX6
                             MOVE  ORCSTEN-HAIK-SRYCD(IDY IDX5)
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  ORCSTEN-HAIK-SURYO(IDY IDX5)
                                       TO   WRK-ORCSTEN-SURYO(IDX6)
                          END-IF 
                          PERFORM  VARYING  IDX9    FROM    1   BY  1
                            UNTIL  IDX9     >      5
                              OR   ORCSTEN-COM-SRYCD(IDY IDX5 IDX9)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-COM-MEI(IDY IDX5 IDX9)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
      *
                       PERFORM   21032-RECE220-TENSOU3-SEC
      *
                       MOVE      350  TO   IDX5
                       PERFORM   21033-RECE230-TENSOU3-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
      *
                       PERFORM   RECE220-WRITE-SEC
      *
                    END-IF
      *
                  END-PERFORM
      *
      *           ¥Ô∫‡
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >     50
                              OR    ORCSTEN-KIZ-ZAITEN(IDY)  =  ZERO
      *
                       MOVE   ZERO   TO   FLG-FORMALNAME
                       MOVE   2      TO   FLG-SIKIBETU
      *
                       INITIALIZE         WRK-ORCSTEN-TABLE
                       MOVE   ZERO   TO   IDX6
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       10
                                OR    ORCSTEN-KIZ-SRYCD(IDY IDX5)
                                                               = SPACE
                          ADD   1   TO   IDX6
                          MOVE  ORCSTEN-KIZ-SRYCD(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SRYCD(IDX6)
                          MOVE  ORCSTEN-KIZ-SURYO(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SURYO(IDX6)
                          PERFORM  VARYING  IDX9    FROM    1   BY  1
                            UNTIL  IDX9     >      5
                              OR   ORCSTEN-KIZ-COM-SRYCD(IDY IDX5 IDX9)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-KIZ-COM-MEI(IDY IDX5 IDX9)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       31
                          MOVE  ORCSTEN-KIZ-DAY  (IDY IDX5)  TO  
                                      WRK-ORCSTEN-DAY  (IDX5)
                          ADD   ORCSTEN-KIZ-DAY  (IDY IDX5)  TO
                                      WRK-ORCSTEN-ZAIKAISU
                          MOVE  ORCSTEN-KIZ-SRYKA(IDY IDX5)  TO
                                      WRK-ORCSTEN-KIZ-SRYKA(IDX5)
                       END-PERFORM
      *
                       PERFORM   21032-RECE220-TENSOU3-SEC
      *
                       MOVE      60   TO   IDX5
                       PERFORM   21033-RECE230-TENSOU3-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
      *
                       PERFORM   RECE220-WRITE-SEC
      *
                  END-PERFORM
      *
      *    END-IF
      *
      ***
      *
      *    ≈¿≈©ºÍµª≤√ªª
      *
           IF   WRK-TENTEKI2-T-ERRCD     =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    IF  WRK-TENTEKI2-T-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      3   TO   FLG-SIKIBETU
                       PERFORM   21032X-RECE220-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-T-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   21035-RECE230-TENTEKI2-HEN-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
                       PERFORM   RECE220-WRITE-SEC
                    END-IF
                 END-PERFORM
           END-IF
      *
           IF   WRK-TENTEKI2-C-ERRCD     =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    IF  WRK-TENTEKI2-C-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      4   TO   FLG-SIKIBETU
                       PERFORM   21032X-RECE220-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-C-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   21035-RECE230-TENTEKI2-HEN-SEC
                       PERFORM   21034-RECE200-HENSYU3-SEC
                       PERFORM   RECE220-WRITE-SEC
                    END-IF
                 END-PERFORM
           END-IF
      *
           .
       2103-RECE220-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       21032-RECE220-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           MOVE    "33"                 TO      RECE220-SRYKBN
           MOVE    SPACE                TO      RECE220-ZAISKBKBN
           IF    FLG-SIKIBETU  =  1
             COMPUTE WRK-ZAINUM    =  99997000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99997400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-YK-SRYKA(IDY)
                                        TO      RECE220-SRYKA
           ELSE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL   IDX2   >   31
               IF   WRK-ORCSTEN-DAY(IDX2)  NOT =   ZERO
                  MOVE  WRK-ORCSTEN-KIZ-SRYKA(IDX2)
                                        TO      RECE220-SRYKA
                  MOVE  31              TO      IDX2
               END-IF
             END-PERFORM
           END-IF
      *
           MOVE    RECE200-HKNCOMBI     TO      RECE220-HKNCOMBI
           MOVE    SPACE                TO      RECE220-JIHOKBN
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE220-ZAITEN
           ELSE
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE220-ZAITEN
           END-IF
           MOVE    ZERO                 TO      RECE220-SRYCDTOTAL
           MOVE    ZERO                 TO      RECE220-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE220-MEISAISU
           IF    FLG-SIKIBETU  =  1
             MOVE  1                    TO      RECE220-ZAIKAISU
           ELSE
             MOVE  WRK-ORCSTEN-ZAIKAISU TO      RECE220-ZAIKAISU
           END-IF
           MOVE    ZERO                 TO      RECE220-SYUTEN1
           MOVE    ZERO                 TO      RECE220-SYUTEN2
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE220-YKZTEN
             MOVE  ZERO                 TO      RECE220-KIZAITEN
           ELSE
             MOVE  ZERO                 TO      RECE220-YKZTEN
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE220-KIZAITEN
           END-IF
           MOVE    SPACE                TO      RECE220-DRCD
           MOVE    SPACE                TO      RECE220-ZAIREQFLG
           MOVE    ZERO                 TO      RECE220-TEIGENRATE
      *
           IF    FLG-SIKIBETU  =  1
             MOVE    1                  TO      RECE220-KAISU (1 IDY)
             MOVE    IDY                TO      WK-TAIHI-DAY
           ELSE
             MOVE        ZERO           TO      WK-TAIHI-DAY
             MOVE        ZERO           TO      WK-TAIHI-FLG
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                          UNTIL    IDX2     >    31
                   MOVE    WRK-ORCSTEN-DAY(IDX2)  TO
                                            RECE220-KAISU (1 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE220-KAISU(1 IDX2)  NOT =  ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF
             END-PERFORM
           END-IF
           .
       21032-RECE220-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£¥°À
      *****************************************************************
       21032X-RECE220-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE220-REC
           INITIALIZE                           RECE220-REC
           MOVE    RECE200-KEY          TO      RECE220-KEY1
           MOVE    "33"                 TO      RECE220-SRYKBN
           MOVE    SPACE                TO      RECE220-ZAISKBKBN
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE220-ZAINUM
           MOVE    RECE200-PTNUM        TO      RECE220-PTNUM
      *
           IF    FLG-SIKIBETU  =  3
             MOVE  WRK-TENTEKI2-T-SRYKA(IDY)
                                        TO      RECE220-SRYKA
           ELSE
             MOVE  WRK-TENTEKI2-C-SRYKA(IDY)
                                        TO      RECE220-SRYKA
           END-IF
      *
           MOVE    RECE200-HKNCOMBI     TO      RECE220-HKNCOMBI
           MOVE    SPACE                TO      RECE220-JIHOKBN
      *
           IF    FLG-SIKIBETU  =  3
             MOVE  WRK-TENTEKI2-T-ZAITEN(IDY)
                                        TO      RECE220-ZAITEN
                                                RECE220-SYUTEN1
           ELSE
             MOVE  WRK-TENTEKI2-C-ZAITEN(IDY)
                                        TO      RECE220-ZAITEN
                                                RECE220-SYUTEN1
           END-IF
           MOVE    1                    TO      RECE220-ZAIKAISU
      *
           MOVE    ZERO                 TO      RECE220-SYUTEN2
           MOVE    ZERO                 TO      RECE220-YKZTEN
           MOVE    ZERO                 TO      RECE220-KIZAITEN
           MOVE    SPACE                TO      RECE220-DRCD
           MOVE    SPACE                TO      RECE220-ZAIREQFLG
           MOVE    ZERO                 TO      RECE220-TEIGENRATE
      *
           MOVE    1                    TO      RECE220-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       21032X-RECE220-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       21033-RECE230-TENSOU3-SEC                SECTION.
      *
           MOVE    1                       TO   IDX2
           MOVE    ZERO                    TO   WRK-RENNUM
           MOVE    ZERO                    TO   WRK-TANICD
           MOVE    ZERO                    TO   WRK-TANIMOJISU
           MOVE    SPACE                   TO   WRK-TANINAME
           MOVE    ZERO                    TO   WRK-KZI058-TEN
           MOVE    ZERO                    TO   WRK-KZI058-TANICD
           MOVE    ZERO                    TO   WRK-KZI058-TANIMOJISU
           MOVE    SPACE                   TO   WRK-KZI058-TANINAME
      *
           MOVE    ZERO                    TO   WRK-SRYSURYO
           MOVE    ZERO                    TO   WRK-SRYSURYO2
      *
           PERFORM
                  UNTIL     (IDX2      >   IDX5)    OR
                            (WRK-ORCSTEN-SRYCD(IDX2)  =   SPACE)
      *
              MOVE    SPACE                TO   RECE230-REC
              INITIALIZE                        RECE230-REC
              MOVE    RECE200-KEY          TO   RECE230-KEY1
              IF    FLG-SIKIBETU  =  1
                COMPUTE WRK-ZAINUM    =  99997000  +  IDY
              ELSE
                COMPUTE WRK-ZAINUM    =  99997400  +  IDY
              END-IF
              MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
              ADD     1                    TO   WRK-RENNUM
              MOVE    WRK-RENNUM           TO   RECE230-RENNUM
              MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
              MOVE    RECE220-SRYKA        TO   RECE230-SRYKA
              MOVE    "33"                 TO   RECE230-SRYKBN
              MOVE    "330"                TO   RECE230-SRYSYUKBN
              MOVE    ZERO                 TO   RECE230-JIHIMONEY
      *
              MOVE    ZERO                 TO      IDX4
              PERFORM
                     UNTIL  (IDX4      >=   5  )    OR
                            (IDX2      >   IDX5)    OR
                            (WRK-ORCSTEN-SRYCD(IDX2)  =   SPACE)
      *
                IF   WRK-ORCSTEN-SRYCD(IDX2)(1:1) =   "0" OR  "6" OR
                                                      "7" OR  "8"
      *
                  ADD   1                   TO  IDX4
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  RECE230-SRYCD(IDX4)
                  MOVE  WRK-ORCSTEN-SURYO(IDX2) 
                                            TO  RECE230-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE230-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE230-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE230-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE230-INPUTNUM(IDX4)
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  WRK-SRYCD
                  MOVE  RECE230-SRYYM       TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  "099309901"
                    IF  RECE230-SRYSURYO(IDX4)  NOT = ZERO
                      COMPUTE  WRK-SRYSURYO  =  WRK-SRYSURYO2
                                             -  RECE230-SRYSURYO(IDX4)
                      MOVE   WRK-TANICD     TO  RECE230-TANICD(IDX4)
                      MOVE   WRK-TANIMOJISU TO  RECE230-TANIMOJISU(IDX4)
                      MOVE   WRK-TANINAME   TO  RECE230-TANINAME(IDX4)
                    ELSE
                      MOVE   "1"            TO  RECE230-RECEPTFLG(IDX4)
                    END-IF
                  ELSE
                      COMPUTE WRK-SRYSURYO2  =  WRK-ORCSTEN-SURYO(IDX2)
                                             +  0.99999
                      IF   TNS-TANICD  NOT = SPACE AND "000"
                       MOVE TNS-TANICD      TO  RECE230-TANICD(IDX4)
                                                WRK-TANICD
                      END-IF
                      MOVE  TNS-TANIMOJISU  TO  RECE230-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU
                      MOVE  TNS-TANINAME    TO  RECE230-TANINAME(IDX4)
                                                WRK-TANINAME
                  END-IF
                  MOVE  TNS-TENSIKIBETU     TO  RECE230-TENSIKIBETU
                                                                 (IDX4)
                  MOVE  TNS-TEN             TO  RECE230-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE230-TENTTLKBN(IDX4)
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE230-YUKOKETA(IDX4)
                  ELSE
                     MOVE   TNS-YUKOKETA    TO  RECE230-YUKOKETA(IDX4)
                  END-IF
                  MOVE  TNS-DATAKBN         TO  RECE230-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE230-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE230-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE230-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE230-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE230-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM
                                      TO RECE230-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO RECE230-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO RECE230-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM
                                      TO RECE230-KOUHYOJYUNNUM(IDX4)
      *
                  MOVE  TNS-SRYSYUKBN TO RECE230-SRYSYUKBN-X(IDX4)
                  MOVE  TNS-KOKUJISKBKBN1
                                      TO RECE230-KOKUJISKBKBN1(IDX4)
                  MOVE  TNS-KZMCOMPSIKIBETU
                                      TO RECE230-KZMCOMPSIKIBETU(IDX4)
                  MOVE  TNS-CHUKSNCD  TO RECE230-CHUKSNCD(IDX4)
                  MOVE  TNS-CHUKSNTUBAN
                                      TO RECE230-CHUKSNTUBAN(IDX4)
                  MOVE  TNS-KENTAICOMMENT
                                      TO RECE230-KENTAICOMMENT(IDX4)
                  MOVE  TNS-KNSJISGRPKBN
                                      TO RECE230-KNSJISGRPKBN(IDX4)
                  MOVE  TNS-TOKUKIZAISBT1
                                      TO RECE230-TOKUKIZAISBT1(IDX4)
                  MOVE  TNS-SANSOKBN  TO RECE230-SANSOKBN     (IDX4)
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   WRK-ORCSTEN-COM-MEI(IDX2)
                                      TO RECE230-NAME(IDX4)
                                         RECE230-INPUTCOMENT(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO RECE230-NAME(IDX4)
                                         RECE230-INPUTCOMENT(IDX4)
                  END-IF
      *
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  "099309901"
                     IF  RECE230-SRYSURYO(IDX4)  NOT = ZERO
                       PERFORM  2101214-RECE230-SURYO-HEN-SEC
      *
                       MOVE    SPACE      TO  RECE230-NAME(IDX4)
                       MOVE    SPACE      TO  RECE230-INPUTCOMENT(IDX4)
      ***              STRING  TNS-NAME          DELIMITED  BY  " "
      ***                      "°°"              DELIMITED  BY  SIZE
      ***                      WRK-SRYSURYOW     DELIMITED  BY  " "
      ***                      RECE230-TANINAME(IDX4)
      ***                                        DELIMITED  BY  " "
      ***                      INTO              RECE230-NAME(IDX4)
      ***              END-STRING
                       STRING  "° ª»Õ—ŒÃ°°"      DELIMITED  BY  SIZE
                               WRK-SRYSURYOW     DELIMITED  BY  " "
                               RECE230-TANINAME(IDX4)
                                                 DELIMITED  BY  " "
                               "°°ªƒŒÃ«—¥˛°À"    DELIMITED  BY  SIZE
                               INTO              RECE230-NAME(IDX4)
                       END-STRING
                       MOVE  RECE230-NAME(IDX4)
                                          TO  RECE230-INPUTCOMENT(IDX4)
      *
      ***              MOVE  "810000001"  TO  RECE230-SRYCD(IDX4)
                       MOVE  ZERO         TO  RECE230-SRYSURYO(IDX4)
                       MOVE  ZERO         TO  RECE230-TANICD(IDX4)
                       MOVE  ZERO         TO  RECE230-TANIMOJISU(IDX4)
                       MOVE  SPACE        TO  RECE230-TANINAME(IDX4)
                       MOVE  ZERO         TO  RECE230-YUKOKETA(IDX4)
                     END-IF
                  END-IF
      *
                  IF   WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "058"
                    MOVE  TNS-FORMALYUKOKETA TO  RECE230-YUKOKETA(IDX4)
                    MOVE  TNS-FORMALNAME     TO  RECE230-NAME(IDX4)
                    MOVE  1                  TO  FLG-FORMALNAME
      *
                    MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                    IF   TNS-TANICD  NOT = SPACE AND "000"
                       MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                       MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                       MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                    END-IF
                    MOVE     ZERO         TO  RECE230-TEN(IDX4)
                    MOVE     ZERO         TO  RECE230-TANICD(IDX4)
                    MOVE     ZERO         TO  RECE230-TANIMOJISU(IDX4)
                    MOVE     SPACE        TO  RECE230-TANINAME(IDX4)
                  END-IF
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7")
                  AND (FLG-FORMALNAME                =    1 )
                    MOVE  "1"             TO  RECE230-RECEHENFLG(IDX4)
                    MOVE  ZERO            TO  FLG-FORMALNAME
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                          TO  RECE230-TEN(IDX4)
                    END-IF
                    IF   (RECE230-TANICD(IDX4)   = ZERO)
                    AND  (WRK-KZI058-TANICD  NOT = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                          TO  RECE230-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                          TO  RECE230-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                          TO  RECE230-TANINAME(IDX4)
                    END-IF
                  END-IF
      *
                  IF   RECE230-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   300   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE230-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE230-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE220-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2)  TO  WRK-SRYCD
                  COMPUTE RECE220-SRYCDTOTAL  =  RECE220-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE RECE220-SURYOUTOTAL =  RECE220-SURYOUTOTAL
                                              +  WRK-ORCSTEN-SURYO(IDX2)
                  COMPUTE RECE220-MEISAISU    =  RECE220-MEISAISU
                                              +  1
      *
                  MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-ORCSTEN-SRYCD(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE220-RUIKEITOTAL  =
                               RECE220-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                   * WRK-ORCSTEN-SURYO(IDX2))
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE220-RUIKEITOTAL  =
                           RECE220-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
      *           ∆√ƒÍ¥Ô∫‡§Œ√±≤¡ ‘Ω∏
                  IF      WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7"
                    IF  WRK-ORCSTEN-SRYCD(IDX2)       =
                                       "799990070"            OR
                                       "770020070"            OR
                                       "770030070"
                      CONTINUE
                    ELSE
                      IF      RECE230-TEN(IDX4)  =   ZERO
                        CONTINUE
                      ELSE
                        PERFORM 2101212-RECE230-TANKA-HEN-SEC
                        COMPUTE IDW =   RECE230-YUKOKETA(IDX4)  *  2
                                    +   3
                        MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                   TO   RECE230-NAME(IDX4)(IDW:)
                        COMPUTE RECE230-YUKOKETA(IDX4)  =
                                RECE230-YUKOKETA(IDX4)  +
                                  1                    +
                               (WRK-CHAR-N-CNT  /  2)
                      END-IF
                    END-IF
                  END-IF
      *
                END-IF
      *
                ADD        1                TO  IDX2
      *
              END-PERFORM
      *
              PERFORM   RECE230-WRITE-SEC
      *
           END-PERFORM
      *
           .
       21033-RECE230-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       21035-RECE230-TENTEKI2-HEN-SEC                SECTION.
      *
           INITIALIZE             WRK-TENTEKI2-SRYCD2-X
      *
           MOVE      ZERO    TO   IDX4
           PERFORM  VARYING  IDX2   FROM    1    BY   1
                     UNTIL  (IDX2                     >      5  )
                       OR   (WRK-TENTEKI2-SRYCD(IDX2)  =   SPACE)
                       OR   (IDX4                     >=   100  )
               ADD    1      TO   IDX4
               MOVE   WRK-TENTEKI2-SRYCD(IDX2)
                             TO   WRK-TENTEKI2-SRYCD2(IDX4)
               PERFORM  VARYING  IDX5   FROM    1    BY   1
                         UNTIL  (IDX5                       >     5  )
                           OR   (WRK-TENTEKI2-COM-SRYCD(IDX2 IDX5)
                                                             =  SPACE)
                           OR   (IDX4                       >=  100  )
                     ADD    1   TO   IDX4
                     MOVE  "810000001"
                                TO   WRK-TENTEKI2-SRYCD2(IDX4)
                     MOVE  WRK-TENTEKI2-COM-FLG(IDX2 IDX5)
                                TO   WRK-TENTEKI2-COM-FLG2(IDX4)
                     MOVE  WRK-TENTEKI2-COM-MOJI(IDX2 IDX5)
                                TO   WRK-TENTEKI2-COM-MOJI2(IDX4)
               END-PERFORM
           END-PERFORM
      *
           MOVE    1                       TO   IDX2
           MOVE    ZERO                    TO   WRK-RENNUM
      *
           PERFORM   UNTIL  (WRK-TENTEKI2-SRYCD2(IDX2)  =   SPACE)
                      OR    (IDX2                       >   100  )
      *
              PERFORM   21033X-RECE230-TENSOU3-SEC
      *
           END-PERFORM
      *
           .
       21035-RECE230-TENTEKI2-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜° £∆£∞£∞£µ°À
      *****************************************************************
       21033X-RECE230-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO   RECE230-REC
           INITIALIZE                        RECE230-REC
           MOVE    RECE200-KEY          TO   RECE230-KEY1
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO   RECE230-ZAINUM
           ADD     1                    TO   WRK-RENNUM
           MOVE    WRK-RENNUM           TO   RECE230-RENNUM
           MOVE    RECE200-PTNUM        TO   RECE230-PTNUM
      *
           MOVE    RECE220-SRYKA        TO   RECE230-SRYKA
           MOVE    "33"                 TO   RECE230-SRYKBN
           IF    FLG-SIKIBETU  =  3
              MOVE    "330"             TO   RECE230-SRYSYUKBN
           ELSE
              MOVE    "340"             TO   RECE230-SRYSYUKBN
           END-IF
           MOVE    ZERO                 TO   RECE230-JIHIMONEY
      *
           MOVE    ZERO                 TO   IDX4
           PERFORM   UNTIL  (WRK-TENTEKI2-SRYCD2(IDX2)  =   SPACE)
                      OR    (IDX4                      >=     5  )
                      OR    (IDX2                      >    100  )
      *
                  ADD   1                   TO  IDX4
      *
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)
                                            TO  RECE230-SRYCD(IDX4)
                  MOVE  1                   TO  RECE230-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE230-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE230-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE230-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE230-INPUTNUM(IDX4)
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)
                                            TO  WRK-SRYCD
                  MOVE  RECE230-SRYYM       TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM 900-TENSU-TBL-SEC
                  PERFORM 21020611-RECE230-TENSOU2X-SEC
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE230-YUKOKETA(IDX4)
                  END-IF
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                     MOVE   WRK-TENTEKI2-COM-MOJI2(IDX2)
                                      TO RECE230-NAME(IDX4)
                                         RECE230-INPUTCOMENT(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO RECE230-INPUTCOMENT(IDX4)
                  END-IF
      *
                  IF   RECE230-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   300   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE230-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE230-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE220-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE220-SRYCDTOTAL  =  RECE220-SRYCDTOTAL
                                               +  WRK-SRYCD
                  COMPUTE  RECE220-SURYOUTOTAL =  RECE220-SURYOUTOTAL
                                               +  1
                  COMPUTE  RECE220-MEISAISU    =  RECE220-MEISAISU
                                               +  1
      *
                  MOVE  ZERO          TO WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-TENTEKI2-SRYCD2(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE220-RUIKEITOTAL =
                               RECE220-RUIKEITOTAL +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                 * 1)
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE220-RUIKEITOTAL  =
                           RECE220-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
                  ADD        1               TO  IDX2
      *
           END-PERFORM
      *
           IF     IDX4    NOT =  ZERO
              PERFORM   RECE230-WRITE-SEC
           END-IF
      *
           .
       21033X-RECE230-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜£≤° £∆£∞£∞£≤°À
      *****************************************************************
       21034-RECE200-HENSYU3-SEC               SECTION.
      *
      *    §Ω§Œ¬æ√ÌºÕ§Œ•Ï•ª≈¿øÙÕÛ§Œ ‘Ω∏
           ADD   RECE220-ZAIKAISU    TO   RECE200-KAISU(10)
      *
           COMPUTE   RECE200-TENSU(10 1)  =
                     RECE200-TENSU(10 1)  +
                    (RECE220-SYUTEN1   *  RECE220-ZAIKAISU)
           COMPUTE   RECE200-TENSU(11 2)  =
                     RECE200-TENSU(11 2)  +
                    (RECE220-YKZTEN    *  RECE220-ZAIKAISU)
           COMPUTE   RECE200-TENSU(11 2)  =
                     RECE200-TENSU(11 2)  +
                    (RECE220-KIZAITEN  *  RECE220-ZAIKAISU)
           COMPUTE   WRK-RECE200C-TENSU(11 2)  =
                     WRK-RECE200C-TENSU(11 2)  +
                    (RECE220-YKZTEN    *  RECE220-ZAIKAISU)
      *
           .
      *
       21034-RECE200-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§ÿ§Œ…È√¥∂‚•ª•√•»° £∆£∞£∞£≤°À
      *****************************************************************
       2104-FTN-SET-SEC                SECTION.
      *
           MOVE    ZERO                 TO      FLG-KHNRYO-SKJ
      *
           MOVE    ZERO                 TO      FLG-NYUINRYO
      *
           MOVE    ZERO                 TO      FLG-SYUNOU-ARI
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE200-HOSPNUM      TO      SYU-HOSPNUM
           MOVE    RECE200-NYUGAIKBN    TO      SYU-NYUGAIKBN
           MOVE    RECE200-PTID         TO      SYU-PTID
           MOVE    RECE200-SRYYM        TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    RECE200-HKNCOMBI     TO      SYU-HKNCOMBINUM
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_syunou"     TO      MCP-TABLE
               MOVE    "key19"          TO      MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                  (FLG-SYUNOU             =    1)
      *
                   IF  SYU-DENPJTIKBN    NOT =  "3"
                       PERFORM   21044-RECE200-FTN-HENSYU-SEC
                       MOVE   1   TO   FLG-SYUNOU-ARI
                   END-IF
      *
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key19"      TO      MCP-PATHNAME
                   PERFORM  900-SYUNOU-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SYUNOU SELECT ERR"
           END-IF
      *
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    ø©ªˆæ Û•Ø•Í•¢
           IF   RECE200-SRYYM  >= "201604"
             IF   RECE200-SHOKUJISYUBETU-KHN    =  "£π"
                MOVE   SPACE  TO   RECE200-SHOKUJISYUBETU-KHN
             END-IF
             IF   RECE200-SHOKUJIKHNRYO(1)      =  999999999
                MOVE   ZERO   TO   RECE200-SHOKUJIKHNRYO(1)
             END-IF
           END-IF
      *
           IF   FLG-SYUNOU-ARI  =  ZERO
              DISPLAY "<SYUNOU NG STOP>"
              MOVE    "≤æº˝«º•«°º•ø§¨§¢§Í§ﬁ§ª§Û"  TO  ERR-ERRMSG
              PERFORM 600-ERRMSG-EDIT-SEC
              MOVE    ERR-HEN-ERRMSG    TO  WRK-RECEERR
              PERFORM 500-ERR-HENSYU-SEC
              MOVE    1                 TO  FLG-RECE200
           END-IF
      *
           .
       2104-FTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    •Ï•ª•◊•»√Ê¥÷•’•°•§•Î§Œπ‡Ã‹≈æ¡˜°›…È√¥∂‚° £∆£∞£∞£≤°À
      *****************************************************************
       21044-RECE200-FTN-HENSYU-SEC               SECTION.
      *
           IF   RECE200-NYUINRYOFLG       =   "1"
           AND  FLG-NYUINRYO              =   ZERO
               PERFORM         210441-NYUINRYOFLG-SET-SEC
           END-IF
      *
           IF   RECE200-SRYYM   >=  "201604"
               PERFORM         210445-SKJ-KINGAKU-HENSYU2-SEC
           ELSE
               PERFORM         210445-SKJ-KINGAKU-HENSYU-SEC
           END-IF
      *
           IF  SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
             ADD   SYU-SYUSKJ-NISSU   TO   RECE200-SHOKUJITOTALDAY
           END-IF
      *
           .
       21044-RECE200-FTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¬æ ›∏±§À§∆∆˛±°Œ¡§ÚªªƒÍ§∑§∆§§§ÎæÏπÁ§Œ ›∏±∑ËƒÍ
      *****************************************************************
       210441-NYUINRYOFLG-SET-SEC               SECTION.
      *
           MOVE     SPACE    TO    RECE200-NYUINRYOFLG
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
      *
             IF      SYU-DAY(IDY)    =    "2"
      *
                IF         WRK-DAY5(IDY)      NOT =  ZERO
                AND        WRK-DAY1(IDY)      NOT =  ZERO
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE200-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "6"     TO   RECE200-NYUINRYOFLG
                       WHEN     "2"
                          PERFORM  2104411-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                ELSE
                   IF      WRK-DAY5(IDY)               NOT =  ZERO
                   AND     RECE200-NYUINDAY-KBN(IDY)   NOT =  "1"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE200-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "1"     TO   RECE200-NYUINRYOFLG
                       WHEN     "2"
                          MOVE    "6"     TO   RECE200-NYUINRYOFLG
                       WHEN     "3"
                          PERFORM  2104411-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                   END-IF
                   IF      WRK-DAY5(IDY)               NOT =  ZERO
                   AND     RECE200-NYUINDAY-KBN(IDY)       =  "1"
                   AND     RECE200-SRYYM                  >=  "200904"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE200-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "6"     TO   RECE200-NYUINRYOFLG
                       WHEN     "2"
                          PERFORM  2104411-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                   END-IF
                END-IF
      *
                MOVE      1       TO     FLG-NYUINRYO
                MOVE     31       TO     IDY
      *
             END-IF
      *
           END-PERFORM
      *
           .
       210441-NYUINRYOFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     ›∏± ‘Ω∏
      *****************************************************************
       2104411-NYUINRYOFLG-HENSYU-SEC               SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    RECE200-HOSPNUM TO  COMB-HOSPNUM
           MOVE    RECE200-PTID    TO  COMB-PTID
           MOVE    WRK-DAY5(IDY)   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 900-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI    =   ZERO
              IF   COMB-HKNNUM(1:2)  =  "98"
              OR   COMB-HKNNUM       =  "971"
              OR   COMB-HKNNUM       =  "973"
              OR   COMB-HKNNUM       =  "975"
                IF   COMB-HKNNUM(1:2)  =  "98"
                   MOVE    "1"   TO   RECE200-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "971"
                   MOVE    "2"   TO   RECE200-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "973"
                   MOVE    "3"   TO   RECE200-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "975"
                   CONTINUE
                END-IF
              ELSE
                EVALUATE  COMB-HKNNUM
                  WHEN     "060"
                  WHEN     "067"
                  WHEN     "068"
                  WHEN     "069"
                    MOVE   "4"   TO   RECE200-NYUINRYOFLG
                  WHEN     "039"
                  WHEN     "040"
                    MOVE   "8"   TO   RECE200-NYUINRYOFLG
                  WHEN      OTHER
                    MOVE   "5"   TO   RECE200-NYUINRYOFLG
                    IF   COMB-HKNNUM(1:2)  =  "90" OR "91"
                     MOVE SPACE  TO   RECE200-NYUINRYOFLG
                    END-IF
                END-EVALUATE
              END-IF
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2104411-NYUINRYOFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø©ªˆ∂‚≥€ ‘Ω∏
      *****************************************************************
       210445-SKJ-KINGAKU-HENSYU-SEC               SECTION.
      *
           IF    RECE200-SRYYM  >= "200610"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >    10
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       5
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE200-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE200-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE200-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE200-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£±"   TO  RECE200-SHOKUJISYUBETU-KHN
                     WHEN       2
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE200-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE200-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE200-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE200-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£≤"   TO  RECE200-SHOKUJISYUBETU-KHN
                     WHEN       3
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(2)
                      MOVE  "∆√ Ã" TO  RECE200-SHOKUJISYUBETU(1)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(3)
                      MOVE  "ø©∆≤" TO  RECE200-SHOKUJISYUBETU(2)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(4)
                      MOVE  "¥ƒ∂≠" TO  RECE200-SHOKUJISYUBETU(3)
                   END-EVALUATE
               END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           IF    RECE200-SRYYM  >= "200604"
           AND   RECE200-SRYYM  <  "200610"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     4
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                 CONTINUE
               ELSE
                 EVALUATE  ID1
                   WHEN       1
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(1)
                     MOVE  "£±"   TO  RECE200-SHOKUJISYUBETU-KHN
                   WHEN       2
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(1)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(1)
                     MOVE  "£≤"   TO  RECE200-SHOKUJISYUBETU-KHN
                   WHEN       3
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(2)
                     MOVE  "∆√ Ã" TO  RECE200-SHOKUJISYUBETU(1)
                   WHEN       4
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE200-SHOKUJIKHNNISSU(3)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE200-SHOKUJIKHNRYO(3)
                     MOVE  "ø©∆≤" TO  RECE200-SHOKUJISYUBETU(2)
                 END-EVALUATE
               END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           .
       210445-SKJ-KINGAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø©ªˆ∂‚≥€ ‘Ω∏° £≤£∞£±£∂«Ø£¥∑Ó°¡°À
      *****************************************************************
       210445-SKJ-KINGAKU-HENSYU2-SEC               SECTION.
      *
           PERFORM  VARYING  ID1    FROM    1   BY  1
                     UNTIL   ID1      >    14
      *
             IF  SYU-SHOKUJI(ID1)   =   ZERO
               CONTINUE
             ELSE
               EVALUATE    ID1
                 WHEN       1
                 WHEN       2
                 WHEN       5
                 WHEN       6
                 WHEN      11
                 WHEN      12
                 WHEN      13
                   COMPUTE  RECE200-SHOKUJIKHNRYO-KEI =
                            RECE200-SHOKUJIKHNRYO-KEI +
                            (SYU-SHOKUJI-NISSU(ID1) * SYU-SHOKUJI(ID1))
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                    RECE200-SHOKUJIKHNNISSU(1)
                   IF    RECE200-SHOKUJIKHNRYO(1) = ZERO
                       MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE200-SHOKUJIKHNRYO(1)
                   ELSE
                     IF  RECE200-SHOKUJIKHNRYO(1)  NOT =
                                                SYU-SHOKUJI(ID1)
                       MOVE  999999999
                                     TO  RECE200-SHOKUJIKHNRYO(1)
                     END-IF
                   END-IF
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       5
                       IF    RECE200-SHOKUJISYUBETU-KHN = SPACE
                           MOVE  "£±"  TO  RECE200-SHOKUJISYUBETU-KHN
                       ELSE
                         IF  RECE200-SHOKUJISYUBETU-KHN  NOT = "£±"
                           MOVE  "£π"  TO  RECE200-SHOKUJISYUBETU-KHN
                         END-IF
                       END-IF
                     WHEN       2
                     WHEN       6
                       IF    RECE200-SHOKUJISYUBETU-KHN = SPACE
                           MOVE  "£≤"  TO  RECE200-SHOKUJISYUBETU-KHN
                       ELSE
                         IF  RECE200-SHOKUJISYUBETU-KHN  NOT = "£≤"
                           MOVE  "£π"  TO  RECE200-SHOKUJISYUBETU-KHN
                         END-IF
                       END-IF
                     WHEN      11
                     WHEN      13
                       IF    RECE200-SHOKUJISYUBETU-KHN = SPACE
                           MOVE  "£≥"  TO  RECE200-SHOKUJISYUBETU-KHN
                       ELSE
                         IF  RECE200-SHOKUJISYUBETU-KHN  NOT = "£≥"
                           MOVE  "£π"  TO  RECE200-SHOKUJISYUBETU-KHN
                         END-IF
                       END-IF
                     WHEN      12
                       IF    RECE200-SHOKUJISYUBETU-KHN = SPACE
                           MOVE  "£¥"  TO  RECE200-SHOKUJISYUBETU-KHN
                       ELSE
                         IF  RECE200-SHOKUJISYUBETU-KHN  NOT = "£¥"
                           MOVE  "£π"  TO  RECE200-SHOKUJISYUBETU-KHN
                         END-IF
                       END-IF
                   END-EVALUATE
                 WHEN       3
                 WHEN       7
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                    RECE200-SHOKUJIKHNNISSU(2)
                   MOVE  SYU-SHOKUJI(ID1)        TO
                                    RECE200-SHOKUJIKHNRYO(2)
                   MOVE  "∆√ Ã" TO  RECE200-SHOKUJISYUBETU(1)
                 WHEN       4
                 WHEN       8
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                    RECE200-SHOKUJIKHNNISSU(3)
                   MOVE  SYU-SHOKUJI(ID1)        TO
                                    RECE200-SHOKUJIKHNRYO(3)
                   MOVE  "ø©∆≤" TO  RECE200-SHOKUJISYUBETU(2)
                 WHEN       9
                 WHEN      10
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                    RECE200-SHOKUJIKHNNISSU(4)
                   MOVE  SYU-SHOKUJI(ID1)        TO
                                    RECE200-SHOKUJIKHNRYO(4)
                   MOVE  "¥ƒ∂≠" TO  RECE200-SHOKUJISYUBETU(3)
               END-EVALUATE
             END-IF
      *
           END-PERFORM
      *
           .
       210445-SKJ-KINGAKU-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    πÁ∑◊∑◊ªª
      *****************************************************************
       2105-GOKEI-KEISAN-SEC                SECTION.
      *
      *    æÆ∑◊
      ***  PERFORM VARYING IDX    FROM    1   BY  1
      ***          UNTIL   IDX    >      21
      ***      ADD    RECE200-TENSU(IDX 1)   TO   RECE200-SYOUKEI(1)
      ***      ADD    RECE200-TENSU(IDX 2)   TO   RECE200-SYOUKEI(2)
      ***  END-PERFORM
      *
           MOVE      WRK-TOTALTEN1     TO    RECE200-SYOUKEI(1)
           MOVE      WRK-TOTALTEN2     TO    RECE200-SYOUKEI(2)
      *
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >      20
               ADD    RECE200-NYUKHNKEI(IDX) TO   RECE200-SYOUKEI(1)
               ADD    RECE200-NYUTOKKEI(IDX) TO   RECE200-SYOUKEI(1)
           END-PERFORM
           ADD        RECE200-NYUKHNKEI2     TO   RECE200-SYOUKEI(1)
           ADD        RECE200-NYUTOKKEI2     TO   RECE200-SYOUKEI(1)
      *
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >       3
               ADD    RECE200-KOGAISIDOKEI(IDX)
                                             TO   RECE200-SYOUKEI(2)
           END-PERFORM
           ADD        RECE200-SEIKANKEI      TO   RECE200-SYOUKEI(2)
      *
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >       4
               IF      RECE200-SHOKUJIKHNNISSU (IDX) >   ZERO
                 IF   RECE200-SRYYM   >=  "201604"
                 AND  IDX              =   1
                   ADD     RECE200-SHOKUJIKHNRYO-KEI
                                             TO   RECE200-SYOUKEI(3)
                 ELSE
                   COMPUTE WRK-SYOKUJIKEI = RECE200-SHOKUJIKHNRYO  (IDX)
                                          * RECE200-SHOKUJIKHNNISSU(IDX)
                   ADD     WRK-SYOKUJIKEI    TO   RECE200-SYOUKEI(3)
                 END-IF
               END-IF
           END-PERFORM
      *
      *    πÁ∑◊° £±£≤±ﬂ°ﬂºÍµª≈¿°À
           COMPUTE    RECE200-TOTALTEN(1)  =  RECE200-SYOUKEI(1)
                                           *  12
      *    πÁ∑◊° £±£∞±ﬂ°ﬂÃÙ∫ﬁ≈˘≈¿°À
           COMPUTE    RECE200-TOTALTEN(2)  =  RECE200-SYOUKEI(2)
                                           *  10
      *    πÁ∑◊° £±°•£≤°ﬂø©ªˆ°À
           COMPUTE    RECE200-TOTALTEN(3)  =  RECE200-SYOUKEI(3)
                                           *  1.2
      *    ¡ÌπÁ∑◊
           COMPUTE    RECE200-TOTALTEN(4)  =  RECE200-TOTALTEN(1)
                                           +  RECE200-TOTALTEN(2)
                                           +  RECE200-TOTALTEN(3)
      *
           .
       2105-GOKEI-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *     Ò≥Á≈¿øÙ§Œº£∏≥∏∫≈¿ ¨ ‘Ω∏
      *****************************************************************
       2106-GENTEN-HENSYU-SEC      SECTION.
      *
           MOVE     ZERO    TO    WRK-TOTALTEN1
           MOVE     ZERO    TO    WRK-TOTALTEN2
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX    >      21
               ADD    RECE200-TENSU(IDX 1)   TO   WRK-TOTALTEN1
               ADD    RECE200-TENSU(IDX 2)   TO   WRK-TOTALTEN2
           END-PERFORM
           COMPUTE   WRK-TOTALTEN1  =  WRK-TOTALTEN1
                                    -  WRK-RECE200G-TENSU(2 1)
                                    -  WRK-RECE200G-TENSU(3 1)
           COMPUTE   WRK-TOTALTEN2  =  WRK-TOTALTEN2
                                    -  WRK-RECE200G-TENSU(11 2)
      *
      *    ªÿ∆≥§Œ•Ï•ª≈¿øÙ§Œ ‘Ω∏° ∏∫≈¿°À
           IF   RECE200-TENSU(2 1)   >=  WRK-RECE200G-TENSU(2 1)
             COMPUTE  RECE200-TENSU(2 1)  =
                      RECE200-TENSU(2 1)  - WRK-RECE200G-TENSU(2 1)
           ELSE
             MOVE     ZERO    TO   RECE200-TENSU(2 1)
           END-IF
      *
      *    ∫ﬂ¬§Œ•Ï•ª≈¿øÙ§Œ ‘Ω∏° ∏∫≈¿°À
           IF   RECE200-TENSU(3 1)   >=  WRK-RECE200G-TENSU(3 1)
             COMPUTE  RECE200-TENSU(3 1)  =
                      RECE200-TENSU(3 1)  - WRK-RECE200G-TENSU(3 1)
           ELSE
             MOVE     ZERO    TO   RECE200-TENSU(3 1)
           END-IF
      *
      *    §Ω§Œ¬æ√ÌºÕ§Œ•Ï•ª≈¿øÙ§Œ ‘Ω∏° ∏∫≈¿°À
           IF   WRK-RECE200C-TENSU(11 2)  >=  WRK-RECE200G-TENSU(11 2)
             COMPUTE  RECE200-TENSU(11 2)  =
                      RECE200-TENSU(11 2)  -  WRK-RECE200G-TENSU(11 2)
           ELSE
             COMPUTE  RECE200-TENSU(11 2)  =
                      RECE200-TENSU(11 2)  -  WRK-RECE200C-TENSU(11 2)
           END-IF
      *
           .
       2106-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡¥≥—¢™øÙ√Õ»æ≥— —¥πΩËÕ˝
      *****************************************************************
       800-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-STIDX
                                           WRK-EDIDX 
           MOVE    SPACE               TO  WRK-NUM
           MOVE    ZERO                TO  WRK-NUM-9
      *
      *    ∂ı«Ú§Œ§»§≠§œ•º•Ì 
           IF      WRK-ZENKAKU          =       SPACE
               MOVE    ZERO                TO  WRK-NUM
               MOVE    10                  TO  IDY   
           ELSE
      *
      *         ∏ª˙§Œ≥´ªœ∑Â
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX         >   80
                       OR      WRK-STIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-STIDX
                   END-IF     
               END-PERFORM
      *         ∏ª˙§ŒΩ™Œª∑Â
               PERFORM VARYING IDX FROM    80  BY  -1
                       UNTIL   IDX         <   1
                       OR      WRK-EDIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-EDIDX
                   END-IF     
               END-PERFORM
      *
      *        ¡¥≥—øÙª˙§Œ§»§≠»æ≥—øÙª˙§À —¥π° øÙª˙∞ ≥∞§Œ§»§≠§œ•º•Ì°À
               IF      WRK-ZENKAKU (WRK-STIDX:1)   >=  X"A1"
                   MOVE    ZERO                TO  IDX3
                   PERFORM VARYING     IDX     FROM    WRK-STIDX  BY  2
                           UNTIL       IDX     >       WRK-EDIDX
                       IF      WRK-ZENKAKU (IDX:2)  >= "£∞"  AND
                                                   <=  "£π"
                           ADD     1               TO  IDX3
                           EVALUATE    WRK-ZENKAKU (IDX:2)
                               WHEN    "£∞"
                                   MOVE    "0"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£±"
                                   MOVE    "1"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£≤"
                                   MOVE    "2"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£≥"
                                   MOVE    "3"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£¥"
                                   MOVE    "4"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£µ"
                                   MOVE    "5"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£∂"
                                   MOVE    "6"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£∑"
                                   MOVE    "7"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£∏"
                                   MOVE    "8"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£π"
                                   MOVE    "9"     TO  WRK-NUM (IDX3:1)
                           END-EVALUATE
      *****************ELSE
      *****************    MOVE    ZERO            TO  WRK-NUM
      *****************    MOVE    WRK-EDIDX       TO  IDX
      *****************    MOVE    10              TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    IDX3                TO  IDY     
               ELSE
      *        »æ≥—øÙª˙§À —¥π° øÙª˙∞ ≥∞§Œ§»§≠§œ•º•Ì°À
                   INSPECT WRK-ZENKAKU(WRK-STIDX:)  REPLACING   ALL " "
                                                                BY  "0"
                   IF      WRK-ZENKAKU(WRK-STIDX:)     NOT NUMERIC
                       PERFORM VARYING IDX     FROM    WRK-STIDX  BY  1
                               UNTIL   IDX     >       WRK-EDIDX
                           IF      WRK-ZENKAKU (IDX:1) >=  "0" AND
                                                       <=  "9"
                               ADD     1           TO  IDX3
                               MOVE    WRK-ZENKAKU (IDX:1)
                                                   TO  WRK-NUM (IDX3:1)
                           END-IF
                       END-PERFORM    
                       MOVE    IDX3                TO  IDY     
                   ELSE
                       COMPUTE IDY     =   WRK-EDIDX   -
                                           WRK-STIDX   +   1
                       MOVE    WRK-ZENKAKU (WRK-STIDX:IDY) TO  WRK-NUM
                   END-IF     
               END-IF
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-NUM-X           TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               MOVE    SNUM-OUTNUM         TO  WRK-NUM-9
           ELSE
               MOVE    ZERO                TO  WRK-NUM-9 
           END-IF
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∞Ïª˛•’•°•§•ÎÃæºË∆¿ΩËÕ˝
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ•∆°º•÷•Î§Œ∏°∫˜
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           MOVE    ZERO                     TO      FLG-TENSU
           MOVE    WRK-SRYCD                TO      TTNS-CHK-SRYCD
           MOVE    WK-TAIHI-SRYYMD          TO      TTNS-CHK-YMD
      *
      *    ≈¿øÙ•ﬁ•π•ø¬‡»Ú•Ô°º•Ø•¡•ß•√•Ø
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    ¬‡»Ú•Ô°º•Ø§À¬∏∫ﬂ§∑§ §§æÏπÁ§œ£ƒ£¬§Ú∏°∫˜
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                           TNS-TENSU-REC
               MOVE    SPA-HOSPNUM          TO      TNS-HOSPNUM
               MOVE    WRK-SRYCD            TO      TNS-SRYCD
               MOVE    WK-TAIHI-SRYYMD      TO      TNS-YUKOSTYMD
                                                    TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
               MOVE    "tbl_tensu"          TO      MCP-TABLE
               MOVE    "key"                TO      MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC               =       ZERO
                   MOVE    "tbl_tensu"      TO      MCP-TABLE
                   MOVE    "key"            TO      MCP-PATHNAME
                   PERFORM  910-TENSU-READ-SEC
                   IF       FLG-TENSU       =       1
                       DISPLAY   "TENSU FETCH ERR"  
                   ELSE
      *                ≈¿øÙ•ﬁ•π•ø¬‡»Ú•Ô°º•Ø§À ‘Ω∏§π§Î
                       PERFORM 9001-TTNS-HEN-SEC
                   END-IF
               ELSE
                   DISPLAY   "TENSU SELECT ERR" 
                   INITIALIZE       TNS-TENSU-REC
                   MOVE    1   TO   FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"          TO      MCP-TABLE
               MOVE    "key"                TO      MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ•ﬁ•π•ø¬‡»Ú•Ô°º•Ø•¡•ß•√•ØΩËÕ˝
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT(TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ•ﬁ•π•ø¬‡»Ú•Ô°º•Ø ‘Ω∏ΩËÕ˝
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF      WRK-SRYCD(1:1)       = "6" OR "7"
               INITIALIZE                   TNSUP-KEY
               MOVE    WK-TAIHI-SRYYMD  TO  TNSUP-YUKOSTYMD
                                            TNSUP-YUKOEDYMD
               CALL    "ORCSSETTANI"    USING
                                        TNS-TENSU-REC
                                        SPA-AREA
           END-IF
      *
           IF      TNS-TANIMOJISU    =   ZERO
           AND     TNS-TANINAME  NOT =   SPACE
               PERFORM   VARYING   IDW2   FROM   24   BY  -1
                          UNTIL    IDW2    <      1
                  IF   TNS-TANINAME(IDW2:1)  NOT =   SPACE
                      COMPUTE  TNS-TANIMOJISU   =   IDW2   /   2
                      MOVE    1     TO    IDW2
                  END-IF
               END-PERFORM
           END-IF
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD(TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD(TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD(TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT(TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ∆…π˛
      *****************************************************************
       910-TENSU-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈≤Ò∑◊∆…π˛
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               IF        ACCT-SRYKBN        =    "95"   OR  "96"
                   GO  TO  900-SRYACCT-READ-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-SRYACCT
               END-IF
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈π‘∞Ÿ∆…π˛
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°≤Ò∑◊•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ∆˛±°ø«Œ≈π‘∞Ÿ•ﬁ•π•ø°º∆…π˛
      **************************************************************
       900-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    º˝«º∆…π˛
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *     ›∏±¡»πÁ§ª•ﬁ•π•ø∆…π˛
      *****************************************************************
       900-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1               TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                       TO  FLG-HKNCOMBI
               INITIALIZE                          HKNCOMBI-REC
           END-IF
      *
           .
       900-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø∆…π˛
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥µº‘•≥•·•Û•»•ﬁ•π•ø∆…§ﬂπ˛§ﬂ
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-PTCOM           =   ZERO
               IF      PTCOM-INPUTCOMENT   NOT =   SPACE
                   MOVE    ZERO            TO  WRK-COL
                                               IDZ1
                                               IDZ2 
                   INSPECT PTCOM-INPUTCOMENT   TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       DISPLAY "PTCOM=" PTCOM-KEY
                       PERFORM VARYING IDZ1    FROM    1   BY  1
                               UNTIL   IDZ1    >       140
                           IF      PTCOM-INPUTCOMENT(IDZ1:1)
                                               =   WRK-CONS-RETURN
                               COMPUTE IDZ2    =   IDZ1    -   1
                               MOVE    140     TO  IDZ1
                           END-IF
                       END-PERFORM
      *
                       CALL    "coblog" USING   "ptcom returncd err"
                       MOVE    SPACE               TO  WRK-RECEERR
                       MOVE    SPACE               TO  WRK-ERRMSG
                       STRING
                        "•≥•·•Û•» ∏ª˙•≥°º•……‘¿µ°°¥µº‘»÷πÊ="
                                                   DELIMITED  BY  SIZE
                               RECE200-PTNUM       DELIMITED  BY  SPACE
                               "°°ø«Œ≈∂Ë ¨="       DELIMITED  BY  SIZE
                               SRY-SRYKBN          DELIMITED  BY  SPACE
                                                   INTO    WRK-ERRMSG
                       END-STRING
                       IF      IDZ2        >   1    
                           STRING   WRK-ERRMSG     DELIMITED  BY  SPACE
                                   "°°•≥•·•Û•»="   DELIMITED  BY  SIZE
                                   PTCOM-INPUTCOMENT(1:IDZ2)
                                                   DELIMITED  BY  SPACE
                                                   INTO    WRK-RECEERR
                           END-STRING
                       ELSE
                           STRING   WRK-ERRMSG     DELIMITED  BY  SPACE
                                    "°°•≥•·•Û•»="  DELIMITED  BY  SIZE
                                    PTCOM-SRYCD    DELIMITED  BY  SPACE
                                                   INTO    WRK-RECEERR
                           END-STRING
                       END-IF
                       PERFORM 500-ERR-HENSYU-SEC
      *
                       MOVE    SPACE               TO  WRK-ERRMSG
                       STRING  "ORCR1220 "         DELIMITED  BY  SIZE
                               WRK-RECEERR         DELIMITED  BY  SIZE
                               LOW-VALUE           DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                       END-STRING
                       PERFORM 500-COBABORT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    √Ê¥÷•’•°•§•Î£“£≈£¡£ƒΩËÕ˝
      *****************************************************************
       RECE200-READ-SEC            SECTION.
      *
           READ    RECE200-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE200
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE200 "  " PTNUM=" RECE200-PTNUM
                                       " (PTID=" RECE200-PTID ")"
      *
                   MOVE  RECE200-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"          TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       RECE200-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ø«Œ≈«Ø∑Ó§Œ¬–æ›§»§ §Î•∑•π¥…Õ˝§ÚºË∆¿° ∏ƒ ÃΩËÕ˝§Œ§ﬂ
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
               CONTINUE
           ELSE
      *        ª‹¿ﬂ¥Ω‡æ Û
               MOVE  ZERO          TO  FLG-SET
               PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                         UNTIL ( SYSKAN-IDX2               >  100  )
                          OR   ( TBL-SYS10062(SYSKAN-IDX2) = SPACE )
                          OR   ( FLG-SET                   =   1   )
                   MOVE    TBL-SYS10062(SYSKAN-IDX2)  TO  SYS-1006-REC
                   IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                   AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                       MOVE     1                  TO  FLG-SET
                   END-IF
               END-PERFORM
               IF      FLG-SET    =   ZERO
                   MOVE  ZERO                    TO  WRK-SISETU613
                   MOVE  ZERO                    TO  WRK-SISETU695
               ELSE
                   MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
                   MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
               END-IF
      *
               MOVE  ZERO          TO  FLG-SET
               PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                         UNTIL ( SYSKAN-IDX2               >  100  )
                          OR   ( TBL-SYS10067(SYSKAN-IDX2) = SPACE )
                          OR   ( FLG-SET                   =   1   )
                   MOVE    TBL-SYS10067(SYSKAN-IDX2)  TO  SYS-1006-REC
                   IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                   AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                       MOVE     1                  TO  FLG-SET
                   END-IF
               END-PERFORM
               IF      FLG-SET    =   ZERO
                   MOVE  ZERO                    TO  WRK-SISETU3055
                   MOVE  ZERO                    TO  WRK-SISETU3056
                   MOVE  ZERO                    TO  WRK-SISETU3057
                   MOVE  ZERO                    TO  WRK-SISETU3058
                   MOVE  ZERO                    TO  WRK-SISETU3168
                   MOVE  ZERO                    TO  WRK-SISETU3169
               ELSE
                   MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
                   MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
                   MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
                   MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
                   MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
                   MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
               END-IF
      *
      *        ƒ¢…º ‘Ω∏∂Ë ¨æ Û
               MOVE    ZERO                TO  FLG-SET
               MOVE    SPACE               TO  WRK-1030-KOUHATUKA
      *
               IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
                 PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                         UNTIL ( SYSKAN-IDX              >  100  )
                         OR    ( TBL-SYS1030(SYSKAN-IDX) = SPACE )
                         OR    ( FLG-SET                 =   1   )
                   MOVE    TBL-SYS1030(SYSKAN-IDX) TO  SYS-1030-REC
                   IF  ( SYS-1030-STYUKYMD       <= WRK-SYS-SRYYMD    )
                   AND ( WRK-SYS-SRYYMD          <= SYS-1030-EDYUKYMD )
                       MOVE     1            TO  FLG-SET
                       MOVE    SYS-1030-KOUHATUKA
                                             TO  WRK-1030-KOUHATUKA
                   END-IF
                 END-PERFORM
               END-IF
      *
               MOVE      RECE200-SRYYM   TO  WRK-SRYYM-5000
               PERFORM   1001-SYS-5000-CHK-SEC
      *        ø©ªˆæ Û° •∑•π•∆•‡¥…Õ˝•∆°º•÷•Î§Ë§Í°À
               INITIALIZE             WRK-SHOKUJIRYOYO-OCC
      *
               MOVE  ZERO          TO  FLG-SET
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                  MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                  IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                  AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                      MOVE     1       TO    FLG-SET
                      PERFORM  1002-SYSKAN-HENSYU-SEC
                      IF        WRK-SYS5000-COUNT    =     ZERO
                          CONTINUE
                      ELSE
                          PERFORM  1003-SYS-5000-GET-SEC
                      END-IF
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
              "∞ÂŒ≈µ°¥ÿæ Û°›∆˛±°¥À‹§¨ºË∆¿§«§≠§ﬁ§ª§Û§«§∑§ø°£ø«Œ≈«Ø∑Ó°·"
                                               DELIMITED  BY  SIZE 
                       RECE200-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1              TO  FLG-RECE200
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øππø∑ΩËÕ˝° £∆£∞£∞£≤°À
      *****************************************************************
       RECE200-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE200-REC
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       RECE200-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£¥°À
      *****************************************************************
       RECE220-WRITE-SEC                SECTION.
      *
           MOVE    RECE220-ZAITEN      TO  RECE220-HOKATU-ZAITEN
           MOVE    RECE220-SYUTEN1     TO  RECE220-HOKATU-SYUTEN1
           MOVE    RECE220-SYUTEN2     TO  RECE220-HOKATU-SYUTEN2
           MOVE    RECE220-YKZTEN      TO  RECE220-HOKATU-YKZTEN
           MOVE    RECE220-KIZAITEN    TO  RECE220-HOKATU-KIZAITEN
      *
           WRITE   RECE220-REC
           ADD     1                   TO  CNT-OUT1
           .
       RECE220-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£µ°À
      *****************************************************************
       RECE230-WRITE-SEC                SECTION.
      *
           WRITE   RECE230-REC
           ADD     1                   TO  CNT-OUT2
           .
       RECE230-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£µ°À
      *****************************************************************
       RECE230-REWRITE-SEC              SECTION.                        
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE230-REC
              MOVE    RECE220-KEY1         TO  RECE230-KEY1
              MOVE    RECE220-ZAINUM       TO  RECE230-ZAINUM
              START   RECE230-FILE
                    KEY IS    >=   RECE230-KEY
                    INVALID
                        MOVE    1          TO  FLG-RECE230-ZAN
                    NOT INVALID
                        MOVE    ZERO       TO  FLG-RECE230-ZAN
                        READ    RECE230-FILE   NEXT
                          AT  END
                            MOVE    1      TO  FLG-RECE230-ZAN
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE220-KEY1         NOT =  RECE230-KEY1  ) OR
                  (RECE220-ZAINUM       NOT =  RECE230-ZAINUM) OR
                  (FLG-RECE230-ZAN          =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE230-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE230-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE230-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE230-REC
                   END-IF
                   READ    RECE230-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE230-ZAN
                   END-READ
              END-PERFORM
           .
       RECE230-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    •«°º•øΩ–ŒœΩËÕ˝° £∆£∞£∞£µ°À
      *****************************************************************
       RECE230-REWRITE2-SEC              SECTION.
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE230-REC
              MOVE    RECE220-KEY1         TO  RECE230-KEY1
              MOVE    RECE220-ZAINUM       TO  RECE230-ZAINUM
              START   RECE230-FILE
                    KEY IS    >=   RECE230-KEY
                    INVALID
                        MOVE    1          TO  FLG-RECE230-ZAN
                    NOT INVALID
                        MOVE    ZERO       TO  FLG-RECE230-ZAN
                        READ    RECE230-FILE   NEXT
                          AT  END
                            MOVE    1      TO  FLG-RECE230-ZAN
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE220-KEY1         NOT =  RECE230-KEY1  ) OR
                  (RECE220-ZAINUM       NOT =  RECE230-ZAINUM) OR
                  (FLG-RECE230-ZAN          =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE230-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE230-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE230-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE230-REC
                   END-IF
                   READ    RECE230-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE230-ZAN
                   END-READ
              END-PERFORM
      *
           .
       RECE230-REWRITE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    •®•È°ºΩ–ŒœΩËÕ˝
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            •∏•Á•÷¥…Õ˝Ω™ŒªΩËÕ˝
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM   900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •®•È°ºª˛Ω™ŒªΩËÕ˝
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ΩËÕ˝√ÊªﬂΩËÕ˝
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "ΩËÕ˝§¨√Êªﬂ§µ§Ï§ﬁ§∑§ø"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                •∏•Á•÷¥…Õ˝Ω™ŒªΩËÕ˝
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
                       PERFORM   900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    •®•È°º•·•√•ª°º•∏ ‘Ω∏ΩËÕ˝
      *****************************************************************
       600-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
      *
           MOVE    RECE200-PTNUM       TO  ERR-PTNUM
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "°Œ¥µº‘»÷πÊ°ß"      DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "°œ"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           ELSE
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "°Œ¥µº‘»÷πÊ:"       DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "°œ"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           END-IF
      *
           .
       600-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ω™ŒªΩËÕ˝
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE200-FILE
                   RECE220-FILE
                   RECE230-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        •π•∆•√•◊¥…Õ˝Ω™ŒªΩËÕ˝
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE200  CNT-IN  :" CNT-IN
           DISPLAY "RECE220  CNT-OUT :" CNT-OUT1
           DISPLAY "RECE230  CNT-OUT :" CNT-OUT2
           DISPLAY "RECE200  CNT-OUT :" CNT-OUT3
           DISPLAY "*** ORCR1220 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∆°º•÷•Î•Ø•Ì°º•∫ΩËÕ˝
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
      *
      *****************************************************************
      *    •∏•Á•÷¥…Õ˝£ƒ£¬¿©∏ÊΩËÕ˝
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬£”£≈£Ã£≈£√£‘ΩËÕ˝
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
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬£∆£≈£‘£√£»ΩËÕ˝
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
      *    •∆°º•÷•Î•¢•Ø•ª•πΩËÕ˝
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬°°•™°º•◊•ÛΩËÕ˝
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬°°•Ø•Ì°º•∫ΩËÕ˝
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬°°£√£œ£Õ£Õ£…£‘ΩËÕ˝
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £ƒ£¬°°£”£‘£¡£“£‘ΩËÕ˝
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
