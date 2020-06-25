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
       PROGRAM-ID.             ORCR0420.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ·î¼¡¶ÈÌ³¡¡Ï«ºÒ¼«ÇåÀÕÌÀºÙ½ñ¥Õ¥¡¥¤¥ëºîÀ®
      *                      ¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/06/18    MCC¡ÝÌçÏÆ¡¡   ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  05.00.01    NACL-ÌçÏÆ    19/03/11  ¸øÌ³ºÒ³²¡ÊÏ«ºÒ½àµò¡Ë¤ÎÃÏ¸øºÒÆÃÊÌ
      *                                     ²Ã»»ÂÐ±þ¡ÊµþÅÔÉÜ¡Ë½¤Àµ
      *  05.00.02    NACL-ÌçÏÆ    19/03/11  Í½Ìó¥³¡¼¥ÉÆÉ¤ßÈô¤Ð¤·½¤Àµ
      *  05.00.03    NACL-ÌçÏÆ    19/04/03  ¥³¥á¥ó¥È¥Þ¥¹¥¿¤Î²þ¸µÂÐ±þ¤Ë
      *                                     È¼¤¦½¤Àµ
      *  05.00.04    NACL-ÌçÏÆ    19/07/01  ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆü¥ì¥»¥×¥Èµ­ºÜÂÐ±þ
      *                                     ¡Êµ¡Ç½ÄÉ²Ã¡Ë
      *  05.00.05    NACL-ÌçÏÆ    19/08/28  ÎáÏÂ¸µÇ¯£±£°·î²þÀµ¡Ê¾ÃÈñÀÇ¡Ë
      *  05.00.06    NACL-ÌçÏÆ    19/10/11  Âè»°¼Ô¹Ô°Ù¤Î´µ¼ÔÉéÃ´³Ûµ­ºÜ½¤Àµ
      *  05.00.07    NACL-ÌçÏÆ    19/12/26  ´ïºàÅÀ¿ô£°ÅÀ»þ¤Î¥³¥á¥ó¥È¥³¡¼¥É
      *                                     ¤Î¼è¤ê°·¤¤½¤Àµ
      *  05.00.08    NACL-ÌçÏÆ    20/02/10  ÌôºÞ¡¦´ïºà¤ÎÃ±°ÌÊÔ½¸¡ÊÅÀ¿ôÉÕ²Ã¡Ë
      *  05.00.09    NACL-ÌçÏÆ    20/03/30  ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.10    NACL-ÌçÏÆ    20/04/16  ´ïºà¤ÎÃ±°ÌÊÔ½¸½¤Àµ
      *  05.00.11    NACL-ÌçÏÆ    20/04/28  ¡ÖÂ¾ÊÝ¸±¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ¡×¤ÎÌ¾¾Î
      *                                     ÊÔ½¸½¤Àµ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE4A-FILE     ASSIGN  RECE4APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE4A-KEY
                                   FILE    STATUS  IS  STS-RECE4A.
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
           SELECT  RECE4B-FILE     ASSIGN  RECE4BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE4B-KEY
                                   FILE    STATUS  IS  STS-RECE4B.
      *    ¥ì¥»ÅÅÍÑ
           SELECT  RECE44C-FILE    ASSIGN  RECE44CPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44C-KEY
                                   FILE    STATUS  IS  STS-RECE44C.
           SELECT  RECE45C-FILE    ASSIGN  RECE45CPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE45C-KEY
                                   FILE    STATUS  IS  STS-RECE45C.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC. 
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC. 
           COPY    "CPRCF044.INC".
      *
       FD  RECE4A-FILE.
       01  RECE4A-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE4A//.
      *
       FD  RECE45-FILE.
       01  RECE45-REC. 
           COPY    "CPRCF045.INC".
      *
       FD  RECE4B-FILE.
       01  RECE4B-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE4B//.
      *
      *    ¥ì¥»ÅÅÍÑ
       FD  RECE44C-FILE.
       01  RECE44C-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE44C//.
      *
       FD  RECE45C-FILE.
       01  RECE45C-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE45C//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE4A//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE4B//.
      *    ¥ì¥»ÅÅÍÑ
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44C//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45C//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE41                              PIC X(02).
           03  STS-RECE44                              PIC X(02).
           03  STS-RECE45                              PIC X(02).
           03  STS-RECE4A                              PIC X(02).
           03  STS-RECE4B                              PIC X(02).
           03  STS-RECE44C                             PIC X(02).
           03  STS-RECE45C                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-DAY                                 PIC 9(01).
           03  FLG-SP                                  PIC 9(01). 
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-FORMAL-KISAI                        PIC 9(01).
           03  FLG-RECE45                              PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-OSIN-COM                            PIC 9(01).
           03  FLG-ZAIHOU-COM                          PIC 9(01).
           03  FLG-SYOUNI-COM                          PIC 9(01).
           03  FLG-HOUTEN-COM                          PIC 9(01).
           03  FLG-SYOBYO-COM                          PIC 9(01).
           03  FLG-RYOYO-COM                           PIC 9(01).
           03  FLG-SYODOU-COM                          PIC 9(01).
           03  FLG-SAIDOU-COM                          PIC 9(01).
           03  FLG-NIKOCHIN-COM                        PIC 9(01).
           03  FLG-ZAIKYO-COM                          PIC 9(01).
           03  FLG-BOKOHO-COM                          PIC 9(01).
           03  FLG-IZONSYO-COM                         PIC 9(01).
           03  FLG-NINRYO-COM                          PIC 9(01).
           03  FLG-ZENSOKU-COM                         PIC 9(01).
           03  FLG-KENSA-COM                           PIC 9(01).
           03  FLG-ZAIRIHA-COM                         PIC 9(01).
           03  FLG-NYUSEN-COM                          PIC 9(01).
           03  FLG-KIATU-COM                           PIC 9(01).
           03  FLG-GAIJIR-COM                          PIC 9(01).
           03  FLG-TAIGAI                              PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
           03  FLG-SRYACCTPLUS                         PIC 9(01).
           03  FLG-WRITE45                             PIC 9(01).
           03  FLG-WRITE45X                            PIC 9(01).
           03  FLG-TAHOKEN                             PIC 9(01).
           03  FLG-SRYKBN-HEN                          PIC 9(01).
           03  FLG-COUNT-ARI                           PIC 9(01).
           03  FLG-HENSYU-NG                           PIC 9(01).
           03  FLG-SETCOM                              PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-ZAITAKU-CHECK                       PIC 9(01).
           03  FLG-DAISAN-GENBUTU                      PIC 9(01).
           03  FLG-KIN-OKIKAE                          PIC 9(01).
           03  FLG-KIN-OKIKAE2                         PIC 9(01).
           03  FLG-RIHA-OKIKAE                         PIC 9(02).
           03  FLG-IRYORENKEI                          PIC 9(01).
           03  FLG-SIPPU-COM                           PIC 9(01).
           03  FLG-SIPPU-ARI                           PIC 9(01).
           03  FLG-YKZ-PRTKBN                          PIC 9(01).
           03  FLG-KIZ-PRTKBN                          PIC 9(01).
           03  FLG-ARI5                                PIC 9(01).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°
       01  FLG-KHNTEN-AREA.
           03  FLG-KHNTEN2                             PIC 9(01).
           03  FLG-KHNTEN3                             PIC 9(01).
           03  FLG-KHNTEN4                             PIC 9(01).
           03  FLG-KHNTEN5                             PIC 9(01).
           03  FLG-KHNTEN6                             PIC 9(01).
           03  FLG-KHNTEN15                            PIC 9(01).
           03  FLG-KHNTEN18                            PIC 9(01).
           03  FLG-KHNTEN19                            PIC 9(01).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°¡Ê¼«ÇåÀÕÍÑ¡Ë
       01  FLG-KHNTEN-JI-AREA.                                          
           03  FLG-KHNTEN10-JI                         PIC 9(01).       
           03  FLG-KHNTEN13-JI                         PIC 9(01).       
           03  FLG-KHNTEN14-JI                         PIC 9(01).       
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°¡Ê¼«ÇåÀÕÍÑ¡§½¾ÍèÍÍ¼°ÍÑ¡Ë
       01  FLG-KHNTEN-JI2-AREA.                                          
           03  FLG-KHNTEN2-JI2                         PIC 9(01).
           03  FLG-KHNTEN3-JI2                         PIC 9(01).
           03  FLG-KHNTEN4-JI2                         PIC 9(01).
           03  FLG-KHNTEN5-JI2                         PIC 9(01).
           03  FLG-KHNTEN6-JI2                         PIC 9(01).
           03  FLG-KHNTEN11-JI2                        PIC 9(01).       
           03  FLG-KHNTEN14-JI2                        PIC 9(01).       
           03  FLG-KHNTEN15-JI2                        PIC 9(01).       
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°¡Ê¸øÌ³ºÒ³²¡Ë
       01  FLG-KHNTEN-KO-AREA.
           03  FLG-KHNTEN2-KO                          PIC 9(01).
           03  FLG-KHNTEN3-KO                          PIC 9(01).
           03  FLG-KHNTEN4-KO                          PIC 9(01).
           03  FLG-KHNTEN5-KO                          PIC 9(01).
           03  FLG-KHNTEN6-KO                          PIC 9(01).
           03  FLG-KHNTEN7-KO                          PIC 9(01).
           03  FLG-KHNTEN8-KO                          PIC 9(01).
           03  FLG-KHNTEN17-KO                         PIC 9(01).
           03  FLG-KHNTEN19-KO                         PIC 9(01).
           03  FLG-KHNTEN21-KO                         PIC 9(01).
           03  FLG-KHNTEN22-KO                         PIC 9(01).
      *
      *    ºÆ¿ÇÎÁ´ðËÜÅÀ¤Ë´Ø¤¹¤ë¥Õ¥é¥°
       01  FLG-SAISIN-AREA.
           03  FLG-SAISIN-ARI                          PIC 9(01).
           03  FLG-SAISIN-GAI-ARI                      PIC 9(01).
           03  FLG-JIKANTAI1-ARI                       PIC 9(01).
           03  FLG-JIKANTAI2-ARI                       PIC 9(01).
           03  FLG-JIKANTAI3-ARI                       PIC 9(01).
           03  FLG-MESAIKSN-ARI                        PIC 9(01).
           03  FLG-CHIHOKATU-ARI                       PIC 9(01).
           03  FLG-CHIHOKAT2-ARI                       PIC 9(01).
           03  FLG-SAITOKKSN-ARI                       PIC 9(01).
           03  FLG-SAITOKKSN-ARI2                      PIC 9(01).
           03  FLG-CHIHOKAT3-ARI                       PIC 9(01).
           03  FLG-CHIHOKAT4-ARI                       PIC 9(01).
           03  FLG-YAKRENKEI-ARI                       PIC 9(01).
           03  FLG-NINPU-ARI                           PIC 9(01).
           03  FLG-NINPU2-ARI                          PIC 9(01).
      *
      *    ºÆ¿ÇÎÁ´ðËÜÅÀ¤Ë´Ø¤¹¤ë¥«¥¦¥ó¥È
       01  CNT-SAISIN-AREA.
           03  CNT-JIKANTAI1                           PIC 9(02).
           03  CNT-JIKANTAI2                           PIC 9(02).
           03  CNT-JIKANTAI3                           PIC 9(02).
           03  CNT-MESAIKSN                            PIC 9(02).
           03  CNT-CHIHOKATU                           PIC 9(02).
           03  CNT-CHIHOKAT2                           PIC 9(02).
           03  CNT-SAITOKKSN                           PIC 9(02).
           03  CNT-SAITOKKSN2                          PIC 9(02).
           03  CNT-NYUYOJI2                            PIC 9(02).
           03  CNT-CHIHOKAT3                           PIC 9(02).
           03  CNT-CHIHOKAT4                           PIC 9(02).
           03  CNT-YAKRENKEI                           PIC 9(02).
           03  CNT-NINPU                               PIC 9(02).
           03  CNT-NINPU2                              PIC 9(02).
           03  CNT-SAISIN-DOUJITU                      PIC 9(02).
           03  CNT-SAISIN-GAI-DOUJITU                  PIC 9(02).
           03  CNT-SAISIN-ONLINE                       PIC 9(02).
           03  CNT-SAISIN-ETC                          PIC 9(02).
           03  CNT-SAISIN-GAI-ETC                      PIC 9(02).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
           03  CNT-OUT4                                PIC 9(06).
           03  CNT-OUT5                                PIC 9(06).
           03  CNT-OUT6                                PIC 9(06).
           03  CNT-OUT7                                PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDX15                                   PIC 9(03).
           03  IDX17                                   PIC 9(03).
           03  IDX19                                   PIC 9(03).
           03  IDX20                                   PIC 9(03).
           03  IDX21                                   PIC 9(03).
           03  IDX22                                   PIC 9(02).
           03  IDX33                                   PIC 9(03).
           03  IDX34                                   PIC 9(03).
           03  IDX40                                   PIC 9(02).
           03  IDX41                                   PIC 9(02).
           03  IDXA                                    PIC 9(01).
           03  IDXB                                    PIC 9(03).
           03  IDXX                                    PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDYY                                    PIC 9(02).
           03  IDW                                     PIC 9(03).
           03  IDW2                                    PIC 9(03).
           03  IDW3                                    PIC 9(03).
           03  IDW4                                    PIC 9(03).
           03  IDW5                                    PIC 9(03).
           03  IDW6                                    PIC 9(03).
           03  IDW7                                    PIC 9(03).
           03  IDW8                                    PIC 9(03).
           03  IDW9                                    PIC 9(02).
           03  ID1                                     PIC 9(03). 
           03  IDV                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  IDZ1                                    PIC 9(03).
           03  IDZ2                                    PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-IDX2                             PIC 9(03).
           03  FUKA-IDX                                PIC 9(03).
      *
      *    ºÞ¾ðÊóÂàÈòÎÎ°è
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
      *
      *    °åÎÅµ¡´Ø¡Ê´ðËÜ¾ðÊó¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *    »ÜÀß´ð½à¾ðÊóÂàÈòÍÑÎÎ°è
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10062                        PIC X(610).
               05  TBL-SYS10067                        PIC X(610).
      *    Ä¢É¼ÊÔ½¸¶èÊ¬¾ðÊóÂàÈòÍÑÎÎ°è
       01  TBL-SYS1030-T.
           03  TBL-SYS1030-OCC                         OCCURS  100.
               05  TBL-SYS1030                         PIC X(610).
      *
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¸¡º÷ÍÑÂÐ¾ÝÆüÎÎ°è
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC 9(02).
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¾ðÊóÂàÈòÎÎ°è
       01  WRK-TNS-TAIHI-AREA.
           03  WRK-TNS-TAIHI-TABLE                     OCCURS  5.
               05  WRK-TNS-CHUKSNTUBAN                 PIC X(01).
               05  WRK-TNS-DATAKBN                     PIC 9(01).
               05  WRK-TNS-TIMEKSNKBN                  PIC 9(01).
               05  WRK-TNS-TEN                         PIC 9(09)V99.    
               05  WRK-TNS-GAITENTTLKBN                PIC 9(03).
               05  WRK-TNS-OSINKBN                     PIC 9(01).
               05  WRK-TNS-KNSJISKBN                   PIC 9(01).
               05  WRK-TNS-SRYCD                       PIC X(09).
               05  WRK-TNS-JIBAISYUKEIKBN              PIC 9(01).
      *
      *    ±¡³°½èÊý°ìÈÌÌ¾ÊÔ½¸¾ðÊóÂàÈòÎÎ°è
       01  WRK-ORCSGETGE-OUT-AREA.
           03  WRK-ORCSGETGE-OUT-TABLE                 OCCURS  50.
               05  WRK-ORCSGETGE-YAKKAKJNCD            PIC X(12).
               05  WRK-ORCSGETGE-GENERIC-KBN           PIC X(01).
               05  WRK-ORCSGETGE-GENERICNAME           PIC X(200).
      *
      *    ¼êµ»ÅÀ¡¦ÌôºÞÅùÅÀ¿ôÍÑÎÎ°è
       01  TBL-TENSUG-JI.
           03  TBL-TENSU-JI                            OCCURS   50.
               05  TBL-SYUTEN-JI                       PIC  9(09).
               05  TBL-YKZTEN-JI                       PIC S9(09).
      *
       01  TBL-ZAITAKU-KAISU-XXX.
           03  TBL-ZAITAKU-KAISU-XXX2                  OCCURS   6.
             05  TBL-ZAITAKU-KAISU-XXX3                OCCURS   9.
               07  TBL-ZAITAKU-KAISU-XXX4              OCCURS  31.
                 09  TBL-ZAITAKU-KAISU                 PIC 9(03).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-RECEKBN                    PIC X(01).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN                    PIC X(01).
           03  WRK-PARA-PRTKBN                         PIC X(01).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-KEISAN                              PIC 9(09).
           03  WRK-KEISAN-KG                           PIC 9(09).
           03  WRK-KEISAN-KG2                          PIC 9(09).
           03  WRK-TEN                                 PIC 9(09).
           03  WRK-TEN-KG                              PIC 9(09).
           03  WRK-TEN-KG2                             PIC 9(09).
           03  WRK-MEISAI-CNT                          PIC 9(04).
           03  WRK-SRYCDTOTAL                          PIC 9(14).
           03  WRK-SURYOUTOTAL                         PIC 9(07)V9(05).
           03  WRK-KSNRATE                             PIC 9V99.    
           03  WRK-SRYCD                               PIC 9(09).   
           03  WRK-YOUBUKASAN                          PIC 9(07).
           03  WRK-KYOBUKASAN                          PIC 9(07).
           03  WRK-KEIBUKASAN                          PIC 9(07).
           03  WRK-SOGAIKASAN                          PIC 9(07).
           03  WRK-RYOYOTEATE                          PIC 9(07).
           03  WRK-RYOYOTEATE2                         PIC 9(07).
           03  WRK-ZAI-KINGAKU                         PIC 9(09).
      *
           03  WRK-KYUGYOFLG                           PIC X(01).
      *
           03  WRK-DAY                                 PIC 9(02).
      *
           03  WRK-ZAIKAISU-XXX                        PIC 9(03).
      *
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
           03  WRK-SRYSURYO                            PIC 9(05)V9(05).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-KZI058-TEN                          PIC 9(09)V99.
           03  WRK-KZI058-TANICD                       PIC 9(03).
           03  WRK-KZI058-TANIMOJISU                   PIC 9(01).
           03  WRK-KZI058-TANINAME                     PIC X(24).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-RENNUM2                             PIC 9(02).
      *
           03  WRK-TEN-KZI                             PIC 9(09).
           03  WRK-TEN-BUN                             PIC 9(09).
      *
           03  WRK-SYUTEN-JI                           PIC 9(09).
           03  WRK-YKZTEN-JI                           PIC 9(09).
           03  WRK-KINGAKU-JI                          PIC 9(09).
      *
      *    ¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
           03  WRK-TAX1001-KBN                         PIC X(01).
           03  WRK-HASU1001-JIBAI                      PIC X(01).
           03  WRK-HASU1001-DAISAN                     PIC X(01).
      *
           03  WRK-SISETU613                           PIC 9(01).
           03  WRK-SISETU695                           PIC 9(01).
           03  WRK-SISETU3055                          PIC 9(01).
           03  WRK-SISETU3056                          PIC 9(01).
           03  WRK-SISETU3057                          PIC 9(01).
           03  WRK-SISETU3058                          PIC 9(01).
           03  WRK-SISETU3168                          PIC 9(01).
           03  WRK-SISETU3169                          PIC 9(01).
      *
      *    ¿ô»úÈ¾³ÑÁ´³ÑÊÑ´¹ÍÑ     
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(20).
               05  WRK-CHAR-N                  PIC X(40).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *     
      *    Ã±²ÁÊÔ½¸ÍÑ     
           03  WRK-TANKA-9                     PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA-9.
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
           03  WRK-ZAITENNASHI                 PIC 9(09).
      *
           03  WRK-SURYOU-COUNT                PIC 9(09).
      *
      *    ¿ôÎÌÊÔ½¸ÍÑ
           03  WRK-SURYOKETA                   PIC 9(02).
           03  WRK-SURYOKETA1                  PIC 9(02).
           03  WRK-SURYOKETA2                  PIC 9(02).
           03  WRK-SURYOST                     PIC 9(02).
           03  WRK-SURYO                       PIC 9(05)V9(05).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1                PIC 9(05).
               05  WRK-SURYO-S2                PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                PIC ZZZZZ.
               05  WRK-SURYO-Z2                PIC X(01).
               05  WRK-SURYO-Z3                PIC ZZZZZ.
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1                PIC X(10).
               05  WRK-SURYO-N2                PIC X(02).
               05  WRK-SURYO-N3                PIC X(10).
           03  WRK-SRYSURYOW                   PIC X(24).
      *
           03  WRK-FORMALNAME                  PIC X(300).
      *
           03  WRK-SETCOM-JIDO                 PIC 9(03).
           03  WRK-SETCOM-JIDO2                PIC 9(03).
      *
           03  WRK-CHECK-SRYCD                 PIC X(09).
      *
           03  WRK-FUTANWARI                   PIC 9V99.
      *
           03  WRK-DAISAN-FTNMONEY             PIC 9(08).
           03  WRK-DAISAN-FTNMONEY2            PIC 9(08).
           03  WRK-DAISAN-DAISANKBN            PIC X(01).
           03  WRK-DAISAN-TOTALTEN-SYUNOU      PIC 9(08).
           03  WRK-DAISAN-ACCT-UPDKBN          PIC X(01).
           03  WRK-DAISAN-TOTALTEN             PIC 9(08).
      *
           03  WRK-S1                          PIC 9(03).
           03  WRK-S2                          PIC 9(03).
           03  WRK-S3                          PIC 9V99999.
           03  WRK-S4                          PIC 9V99.
           03  WRK-S5                          PIC 9V99.
      *
           03  WRK-COL                         PIC 9(03).
      *
           03  WRK-RIHA-YMD                    PIC X(07).
           03  WRK-RIHA-YMD-SEI                PIC X(08).
      *
           03  WRK-TAIHI-ZAINUM                PIC 9(08).
           03  WRK-TAIHI-ZAINUM2               PIC 9(08).
      *
           03  WRK-INPUTCOMENT                 PIC X(140).
           03  WRK-INPUTCOMENT2                PIC X(14).
           03  WRK-INPUTCOMENT3                PIC X(30).
           03  WRK-INPUTCOMENT-KIN-T.
               05  WRK-INPUTCOMENT-KIN         PIC 9(07).
           03  WRK-INPUTCOMENT-KIN2-T.
               05  WRK-INPUTCOMENT-KIN2        PIC X(07).
      *
           03  WRK-JIBAI-SKYKIN-A              PIC 9(09).
           03  WRK-JIBAI-SKYKIN-B              PIC 9(09).
      *
           03  WRK-1030-KOUHATUKA              PIC X(01).
      *
      *---------------------------------------------------------------
      *  ²ó¿ô¸º»»ÍÑÂàÈòÎÎ°è
      *    ½èÊý¸º²ó¿ô
           03  WRK-SYOHOU-KAISU                PIC 9(03).
      *    ½èÊý¤»¤ó¸º²ó¿ô
           03  WRK-SYOSEN-KAISU                PIC 9(03).
      *    ½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë
           03  WRK-MADOKU-SHOHO                PIC 9(03).
      *---------------------------------------------------------------
      *
      *    ºÞ¤Î¿ÇÎÅÆâÍÆ¥Á¥§¥Ã¥¯¥Æ¡¼¥Ö¥ë
       01  TBL-ZAICHECK-AREA.
      *    ¡Ê¥µ¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000046".
      *    ¡Ê¸¡Áª¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000053".
      *    ¡Ê¥ê¥ÏÁª¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000054".
      *    ¡ÊÀºÁª¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000055".
      *
       01  TBL-ZAICHECK-R      REDEFINES   TBL-ZAICHECK-AREA.
           03  TBL-ZAICHECK    PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAICHECK-MAX    PIC 9(02)   VALUE     4.
      *
      *    »ÀÁÇ 
           COPY    "CMSANSOTBL.INC".
      *
      *   ÆüËÜ¸ì¿ô»ú
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "£±£²£³£´£µ£¶£·£¸£¹£°".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    ²þ¹Ô¥³¡¼¥É
       01  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           COPY    "CPTTNS.INC".
      *
      *    £Ò£Å£Ã£Å£´£±¤Î¥ï¡¼¥¯¥ì¥³¡¼¥É
       01  WRK-RECE4G-REC. 
           COPY    "CPRCF041.INC"  REPLACING  //RECE41//
                                   BY         //WRK-RECE4G//.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ëÌ¾¾ÎÎÎ°è 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊó¥Þ¥¹¥¿¡Ý
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
       COPY    "CPPTCOM.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý´ðËÜ
           COPY    "CPSK1001.INC".
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK4003.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK1030.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    È¾³Ñ¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY    "CPORCSKANACHK.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *    ¿ôÃÍÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *    ¥Á¥§¥Ã¥¯¥Ç¥£¥¸¥Ã¥È¡Ê¥â¥¸¥å¥é¥¹£±£±ÍÑ¡Ë»»½Ð¥µ¥Ö
           COPY    "CPORCCHKDGT11.INC".
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *    ±¡³°½èÊý°ìÈÌÌ¾ÊÔ½¸¥µ¥Ö
           COPY    "CPORCSGETGENERIC.INC".
      *
      *    £Ä£Â¸¡º÷
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
      *    ¼ç½èÍý
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
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       TTNS-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE41PARA
           INITIALIZE                      RECE41PARA
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE44PARA
           INITIALIZE                      RECE44PARA
           MOVE    "RECE440"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
           MOVE    SPACE               TO  RECE45PARA
           INITIALIZE                      RECE45PARA
           MOVE    "RECE450"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE4APARA
           INITIALIZE                      RECE4APARA
           MOVE    "RECE441"           TO  RECE4APARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE4APARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE4APARA-HOSPNUM
           MOVE    SPACE               TO  RECE4BPARA
           INITIALIZE                      RECE4BPARA
           MOVE    "RECE451"           TO  RECE4BPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE4BPARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE4BPARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE44CPARA
           INITIALIZE                      RECE44CPARA
           MOVE    "RECE44C"           TO  RECE44CPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44CPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE44CPARA-HOSPNUM
           MOVE    SPACE               TO  RECE45CPARA
           INITIALIZE                      RECE45CPARA
           MOVE    "RECE45C"           TO  RECE45CPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45CPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE45CPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE44PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE45PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE4APARA-BASENAME TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE4BPARA-BASENAME TO  SGETTEMP-BASENAMES  (6)
           MOVE    RECE44CPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           MOVE    RECE45CPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (8)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE41PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE44PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE45PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE4APARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE4BPARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE44CPARA
           MOVE    SGETTEMP-FULLNAMES (8)
                                       TO  RECE45CPARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0420"      TO  JOB-PGID
               MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       °åÎÅµ¡´Ø¾ðÊó¡Ý´ðËÜ
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
              MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO  SYS-1001-REC
                  MOVE    SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
                  IF   WRK-TAX1001-KBN = SPACE
                     MOVE   "2"           TO  WRK-TAX1001-KBN
                  END-IF
                  MOVE    SYS-1001-DAISANKBN
                                          TO  WRK-DAISAN-DAISANKBN
                  IF  WRK-DAISAN-DAISANKBN = SPACE
                    MOVE  "2"             TO  WRK-DAISAN-DAISANKBN
                  END-IF
                  MOVE    SYS-1001-DISCOUNT-HASU-KBN4
                                          TO  WRK-HASU1001-JIBAI
                  IF  WRK-HASU1001-JIBAI = SPACE
                    MOVE  "4"             TO  WRK-HASU1001-JIBAI
                  END-IF
                  MOVE    SYS-1001-DISCOUNT-HASU-KBN6
                                          TO  WRK-HASU1001-DAISAN
                  IF  WRK-HASU1001-DAISAN = SPACE
                    MOVE  WRK-HASU1001-JIBAI
                                          TO  WRK-HASU1001-DAISAN
                  END-IF
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                   TO  FLG-END
              END-IF
      *       »ÜÀß´ð½à¾ðÊó
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
                 INITIALIZE                    SYS-1006-REC
                 MOVE    "1006"            TO  SYS-1006-KANRICD
                 MOVE    IDXA              TO  SYS-1006-KBNCD
                 MOVE    WRK-PARA-SRYYM    TO  SYS-1006-STYUKYMD(1:6)
                 MOVE    "01"              TO  SYS-1006-STYUKYMD(7:2)
                 MOVE    SYS-1006-STYUKYMD TO  SYS-1006-EDYUKYMD
grpsys           MOVE    SPA-HOSPNUM       TO  SYS-1006-HOSPNUM
                 MOVE    SYS-1006-REC      TO  MCPDATA-REC
                 PERFORM 800-SYSKANRI-READ-SEC
                 IF      FLG-SYSKANRI     =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1006-REC
                   EVALUATE  IDXA
                     WHEN     2
                       MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
                       MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
                     WHEN     7
                       MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
                       MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
                       MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
                       MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
                       MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
                       MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
                   END-EVALUATE
                 END-IF
               END-IF
              END-PERFORM
      *       Ä¢É¼ÊÔ½¸¶èÊ¬¾ðÊó
              MOVE    SPACE               TO  WRK-1030-KOUHATUKA
      **>     IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
                INITIALIZE                      SYS-1030-REC
                MOVE    "1030"              TO  SYS-1030-KANRICD
                MOVE    "*"                 TO  SYS-1030-KBNCD
                MOVE    WRK-PARA-SRYYM      TO  SYS-1030-STYUKYMD(1:6)
                MOVE    "01"                TO  SYS-1030-STYUKYMD(7:2)
                MOVE    SYS-1030-STYUKYMD   TO  SYS-1030-EDYUKYMD
                MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
                MOVE    SYS-1030-REC        TO  MCPDATA-REC
                PERFORM 800-SYSKANRI-READ-SEC
                IF      FLG-SYSKANRI     =   ZERO
                  MOVE  MCPDATA-REC         TO  SYS-1030-REC
                  MOVE  SYS-1030-KOUHATUKA  TO  WRK-1030-KOUHATUKA
                END-IF
      **>     END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM 810-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM 810-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYS-1006-REC
                 MOVE    "1006"           TO  SYS-1006-KANRICD
                 MOVE    IDXA             TO  SYS-1006-KBNCD
grpsys           MOVE    SPA-HOSPNUM      TO  SYS-1006-HOSPNUM
                 MOVE    SYS-1006-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                     MOVE    "tbl_syskanri"   TO  MCP-TABLE
                     MOVE    "key8"           TO  MCP-PATHNAME
                     PERFORM 810-SYSKANRI-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE    MCPDATA-REC    TO  SYS-1006-REC
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
                       PERFORM 810-SYSKANRI-READ-SEC
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key8"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
              END-PERFORM
      *
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1030-T
              INITIALIZE                      TBL-SYS1030-T
      **>     IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
                INITIALIZE                      SYS-1030-REC
                MOVE    "1030"              TO  SYS-1030-KANRICD
                MOVE    "*"                 TO  SYS-1030-KBNCD
                MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
                MOVE    SYS-1030-REC        TO  MCPDATA-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key8"              TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"    TO  MCP-TABLE
                  MOVE    "key8"            TO  MCP-PATHNAME
                  PERFORM 810-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC     TO  SYS-1030-REC
                    ADD     1               TO  SYSKAN-IDX
                    MOVE    SYS-1030-REC    TO  TBL-SYS1030(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM 810-SYSKANRI-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key8"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
      **>     END-IF
           END-IF
      *
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-200501-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-200501-STYUKYMD (7:2)
           MOVE    SYS-200501-STYUKYMD TO  SYS-200501-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-200501-REC
           ELSE
               INITIALIZE                  SYS-200501-REC
           END-IF
           IF      SYS-200501-ROURECEKBN  =   SPACE
               MOVE    "0"             TO  SYS-200501-ROURECEKBN
           END-IF
           IF      SYS-200501-JIBAI-FRACTION-KBN(1)  =   SPACE
               MOVE    "0"   TO  SYS-200501-JIBAI-FRACTION-KBN(1)
           END-IF
           IF      SYS-200501-JIBAI-FRACTION-KBN(2)  =   SPACE
               MOVE    "0"   TO  SYS-200501-JIBAI-FRACTION-KBN(2)
           END-IF
           IF      SYS-200501-INSPECT-GENERAL-KBN   =   SPACE
               MOVE    "0"   TO  SYS-200501-INSPECT-GENERAL-KBN
           END-IF
           IF      SYS-200501-COMPRESS-KBN          =   SPACE
               MOVE    "0"   TO  SYS-200501-COMPRESS-KBN
           END-IF
           IF      SYS-200501-15-YKZ-PRTKBN         =   SPACE
               MOVE    "0"   TO  SYS-200501-15-YKZ-PRTKBN
           END-IF
      *                             
      *    ¥ì¥»¥×¥È½èÍý
      *
           OPEN    OUTPUT              RECE44-FILE
           CLOSE                       RECE44-FILE
      *
           OPEN    OUTPUT              RECE45-FILE
           CLOSE                       RECE45-FILE
      *
           OPEN    OUTPUT              RECE4A-FILE
           CLOSE                       RECE4A-FILE
      *
           OPEN    OUTPUT              RECE4B-FILE
           CLOSE                       RECE4B-FILE
      *
           OPEN    OUTPUT              RECE44C-FILE
           CLOSE                       RECE44C-FILE
      *
           OPEN    OUTPUT              RECE45C-FILE
           CLOSE                       RECE45C-FILE
      *
           OPEN    OUTPUT              RECE44-FILE
      ***                              RECE45-FILE
           OPEN    I-O                 RECE45-FILE
      *
           OPEN    OUTPUT              RECE4A-FILE
                                       RECE4B-FILE
      *
           OPEN    OUTPUT              RECE44C-FILE
           OPEN    I-O                 RECE45C-FILE
      *
           OPEN    I-O                 RECE41-FILE
      *
           PERFORM     900-RECE41-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM     210-HENSYU-SEC
      *      
           PERFORM     900-RECE41-READ-SEC
           IF      CNT-IN-COMMIT   =   300
               MOVE  ZERO          TO      CNT-IN-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
      *    ¾ÃÈñÀÇ·×»»¤Î·¸¿ôÀßÄê
           IF    RECE41-SRYYM >= "201910"
               MOVE      10      TO  WRK-S1
               MOVE     110      TO  WRK-S2
               MOVE     0.10     TO  WRK-S4
               MOVE     1.10     TO  WRK-S5
           ELSE
             IF  RECE41-SRYYM >= "201404"
               MOVE       8      TO  WRK-S1
               MOVE     108      TO  WRK-S2
               MOVE     0.08     TO  WRK-S4
               MOVE     1.08     TO  WRK-S5
             ELSE
               MOVE       5      TO  WRK-S1
               MOVE     105      TO  WRK-S2
               MOVE     0.05     TO  WRK-S4
               MOVE     1.05     TO  WRK-S5
             END-IF
           END-IF
           EVALUATE  WRK-TAX1001-KBN
               WHEN      "1"
                 MOVE   0.50000  TO  WRK-S3
               WHEN      "2"
                 MOVE   0.00000  TO  WRK-S3
               WHEN      "3"
                 MOVE   0.99999  TO  WRK-S3
           END-EVALUATE
      *
           INITIALIZE                   WRK-RECE4G-REC
      *
           MOVE       ZERO       TO     WRK-ZAITENNASHI
      *
           MOVE       ZERO       TO     WRK-SURYOU-COUNT
      *
           INITIALIZE                   FLG-SAISIN-AREA
           INITIALIZE                   CNT-SAISIN-AREA
      *
           INITIALIZE                   TBL-ZAITAKU-KAISU-XXX
      *
           MOVE       ZERO       TO     WRK-TAIHI-ZAINUM
           MOVE       ZERO       TO     WRK-TAIHI-ZAINUM2
      *
           IF      RECE41-HKNKBN   NOT  =   "3"
      *
              INITIALIZE                   FLG-KHNTEN-AREA
              INITIALIZE                   FLG-KHNTEN-JI-AREA           
              INITIALIZE                   FLG-KHNTEN-JI2-AREA
              INITIALIZE                   FLG-KHNTEN-KO-AREA
              INITIALIZE                   TBL-TENSUG-JI
      *
              MOVE       ZERO       TO     WRK-KINGAKU-JI
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE41-SRYKA(IDX)   =   SPACE)
                  PERFORM     2101-RECE44-HENSYU-SEC
              END-PERFORM
      *
              EVALUATE   RECE41-HKNKBN
                WHEN       "1"
                WHEN       "2"
                  PERFORM     2102-KHNTEN-CLEAR-SEC
                WHEN       "4"
                WHEN       "6"
                  PERFORM     2103-KHNTEN-JI-CLEAR-SEC                       
                WHEN       "5"
                  PERFORM     2106-KHNTEN-KO-CLEAR-SEC
              END-EVALUATE
      *
           ELSE
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE41-SRYKA(IDX)   =   SPACE)
                  PERFORM     2104-RECE44-HENSYU2-SEC
              END-PERFORM
      *
           END-IF
      *
           PERFORM     2107-GENTEN-HENSYU-SEC
      *
      *    ºßÂð²ó¿ô¤ÎºÆÊÔ½¸
           PERFORM     2108-ZAITAKU-KAISU-HENSYU-SEC
      *
      *    ¼«ÇåÀÕ¡¦ÀÁµá¶â³ÛÍóÃ¼¿ô½èÍý¶èÊ¬ÊÔ½¸
           PERFORM     2109-JIBAI-HASUKBN-HENSYU-SEC
      *
           PERFORM     2105-RECE41-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®
      *****************************************************************
       2101-RECE44-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE41-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE41-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE41-PTID          TO      ACCT-PTID
           MOVE    RECE41-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE41-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE41-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key24"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key24"          TO      MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL     FLG-SRYACCT   =   1 
      *
                 MOVE   ZERO          TO  FLG-RINYAK
      *
                 IF    ACCT-SRYKBN       =  "95"  OR  "96"
                   CONTINUE
                 ELSE
      *
                   MOVE     ZERO    TO    FLG-TAISYO
                   IF   (ACCT-SRYKBN         =   "80" )
                   AND  (ACCT-ZAITEN         =   ZERO )
                   AND  (ACCT-ZAIKAISU   NOT =   ZERO )
                   AND  (ACCT-JIHOKBN        =   SPACE)
                     PERFORM  21015-SRYACCTPLUS-KENSAKU-SEC
                   END-IF
      *
                   IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (WRK-PARA-PRTKBN    =
                                    "1" OR "2" OR "3" ) AND
                        (ACCT-JIHOKBN       =    SPACE))
                   OR  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (WRK-PARA-PRTKBN    =
                                           "4" OR "5" ))
                   OR  ( FLG-TAISYO         =      1   )
                               MOVE    ZERO    TO      WRK-TEN
                                                       WRK-MEISAI-CNT
                                                       WRK-SRYCDTOTAL
                                                       WRK-SURYOUTOTAL
                                                       WRK-YOUBUKASAN
                                                       WRK-KYOBUKASAN
                                                       WRK-KEIBUKASAN
                                                       WRK-SOGAIKASAN
                                                       WRK-RYOYOTEATE
                                                       WRK-ZAI-KINGAKU
      *
                               MOVE    ZERO    TO      WRK-SRYSURYO
                                                       WRK-SRYSURYO2
                                                       WRK-SRYSURYO3
                                                       WRK-TANICD
                                                       WRK-TANIMOJISU
                               MOVE    SPACE   TO      WRK-TANINAME
                               MOVE    ZERO    TO      FLG-HOUSIJI-COM
                                                       FLG-HOUTEN-COM
                                                       FLG-SYOBYO-COM
                                                       FLG-RYOYO-COM
                                                       FLG-SYODOU-COM
                                                       FLG-SAIDOU-COM
                                                       FLG-NIKOCHIN-COM
                                                       FLG-ZENSOKU-COM
                                                       FLG-OSIN-COM
                                                       FLG-ZAIHOU-COM
                                                       FLG-TAHOKEN
                                                       FLG-SYOUNI-COM
                                                       FLG-SRYKBN-HEN
                                                       FLG-KENSA-COM
                                                       FLG-ZAIRIHA-COM
                                                       FLG-ZAIKYO-COM
                                                       FLG-BOKOHO-COM
                                                       FLG-IZONSYO-COM
                                                       FLG-NINRYO-COM
                                                       FLG-NYUSEN-COM
                                                       FLG-KIATU-COM
                                                       FLG-GAIJIR-COM
      *
                               MOVE    SPACE   TO      WRK-KYUGYOFLG
                               MOVE    ZERO    TO      FLG-COUNT-ARI
      *
                               MOVE    SPACE   TO      WRK-FORMALNAME
                               MOVE    ZERO    TO
                                               WRK-KZI058-TEN
                                               WRK-KZI058-TANICD
                                               WRK-KZI058-TANIMOJISU
                               MOVE    SPACE   TO
                                               WRK-KZI058-TANINAME
      *
                               MOVE    ZERO    TO      FLG-WRITE45
                               MOVE    ZERO    TO      FLG-RIHA-OKIKAE
                               MOVE    ZERO    TO      FLG-HENSYU-NG
                               MOVE    ZERO    TO      FLG-SETCOM
                               MOVE    ZERO    TO      WRK-SETCOM-JIDO
                               MOVE    ZERO    TO      WRK-SETCOM-JIDO2
                               MOVE    ZERO    TO      FLG-ZAITAKU-CHECK
                               MOVE    ZERO    TO      FLG-KIN-OKIKAE
                               MOVE    ZERO    TO      FLG-KIN-OKIKAE2
                               MOVE    ZERO    TO      FLG-SIPPU-COM
                               MOVE    ZERO    TO      FLG-SIPPU-ARI
                               MOVE    ZERO    TO      FLG-YKZ-PRTKBN
                               MOVE    ZERO    TO      FLG-KIZ-PRTKBN
                               PERFORM   21011-RECE44-TENSOU-SEC
                               PERFORM   21012-RECE45-HENSYU-SEC
                               IF    FLG-WRITE45  =   1
                                 PERFORM 21013-RECE44-WRITE-SEC
                               END-IF
                   ELSE
                             IF    ACCT-ZAIKAISU  NOT =    ZERO
                             AND   ACCT-JIHOKBN       =    SPACE
      **>                    IF  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                         (WRK-PARA-PRTKBN    =
      **>                                     "1" OR "2" OR "3" ) AND
      **>                         (ACCT-JIHOKBN       =    SPACE))
      **>                    OR  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                         (WRK-PARA-PRTKBN    =
      **>                                            "4" OR "5" ))
                               IF ( ACCT-SRYKBN  NOT =
                                    "24" AND "90" AND "92" AND
                                    "93" AND "97"                 )
                               OR ( ACCT-ZAIKBN  =   1            )
                                 IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                    "000000999901")
                                 AND (ACCT-MEISAISU      =   1    )
                                      CONTINUE
                                 ELSE
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-KEIBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
                                                      WRK-ZAI-KINGAKU
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
                                                      FLG-SYOBYO-COM
                                                      FLG-RYOYO-COM
                                                      FLG-SYODOU-COM
                                                      FLG-SAIDOU-COM
                                                      FLG-NIKOCHIN-COM
                                                      FLG-ZENSOKU-COM
                                                      FLG-OSIN-COM
                                                      FLG-ZAIHOU-COM
                                                      FLG-TAHOKEN
                                                      FLG-SYOUNI-COM
                                                      FLG-SRYKBN-HEN
                                                      FLG-KENSA-COM
                                                      FLG-ZAIRIHA-COM
                                                      FLG-ZAIKYO-COM
                                                      FLG-BOKOHO-COM
                                                      FLG-IZONSYO-COM
                                                      FLG-NINRYO-COM
                                                      FLG-NYUSEN-COM
                                                      FLG-KIATU-COM
                                                      FLG-GAIJIR-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
                                      MOVE  ZERO  TO  FLG-COUNT-ARI
      *
                                      MOVE  SPACE TO  WRK-FORMALNAME
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO  FLG-WRITE45
                                      MOVE  ZERO  TO  FLG-RIHA-OKIKAE
                                      MOVE  ZERO  TO  FLG-HENSYU-NG
                                      MOVE  ZERO  TO  FLG-SETCOM
                                      MOVE  ZERO  TO  WRK-SETCOM-JIDO
                                      MOVE  ZERO  TO  WRK-SETCOM-JIDO2
                                      MOVE  ZERO  TO  FLG-ZAITAKU-CHECK
                                      MOVE  ZERO  TO  FLG-KIN-OKIKAE
                                      MOVE  ZERO  TO  FLG-KIN-OKIKAE2
                                      MOVE  ZERO  TO  FLG-SIPPU-COM
                                      MOVE  ZERO  TO  FLG-SIPPU-ARI
                                      MOVE  ZERO  TO  FLG-YKZ-PRTKBN
                                      MOVE  ZERO  TO  FLG-KIZ-PRTKBN
                                      IF    ACCT-SRYKBN  =
                                            "60" OR "64" OR "80"
                                        PERFORM 21014-SINRYO-CHECK-SEC
                                      END-IF
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      IF    FLG-WRITE45  =   1
                                        PERFORM 21013-RECE44-WRITE-SEC
                                      END-IF
                                 END-IF
                               END-IF
                             END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_sryacct"    TO      MCP-TABLE
                 MOVE    "key24"          TO      MCP-PATHNAME
                 PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2101-RECE44-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       21011-RECE44-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE44-REC
           INITIALIZE                           RECE44-REC
           MOVE    RECE41-HOSPNUM       TO      RECE44-HOSPNUM
           MOVE    RECE41-SRYYM         TO      RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE44-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE44-PTID
           MOVE    RECE41-RECEKA        TO      RECE44-RECEKA
           MOVE    RECE41-HKNID         TO      RECE44-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE44-TEKSTYMD
           MOVE    RECE41-PTNUM         TO      RECE44-PTNUM 
           MOVE    ACCT-ZAINUM          TO      RECE44-ZAINUM
           MOVE    ACCT-SRYKBN          TO      RECE44-SRYKBN
           MOVE    ACCT-SRYKA           TO      RECE44-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE44-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE44-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE44-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE44-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE44-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE44-MEISAISU
           IF      RECE41-HKNKBN   NOT  =   "3"
              MOVE    ACCT-ZAIKAISU     TO      RECE44-ZAIKAISU
           ELSE
              MOVE    WRK-ZAIKAISU-XXX  TO      RECE44-ZAIKAISU
           END-IF
           MOVE    ACCT-SYUTEN1         TO      RECE44-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE44-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE44-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE44-KIZAITEN
           IF   RECE41-HKNKBN     =  "4" OR "6"
              IF  ACCT-SRYKBN     =  "70"
                  COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                            -  RECE44-SYUTEN2
                  COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                            +  RECE44-SYUTEN2
              END-IF
           END-IF
           MOVE    ACCT-DRCD            TO      RECE44-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE44-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE44-TEIGENRATE
      *
           IF      RECE41-HKNKBN   NOT  =   "3"
               MOVE        ZERO             TO      WK-TAIHI-DAY
               MOVE        ZERO             TO      WK-TAIHI-FLG
               PERFORM     VARYING   IDX3   FROM    1    BY   1
                           UNTIL     IDX3   >      10
                   PERFORM     VARYING   IDX2   FROM    1    BY   1
                               UNTIL     IDX2   >       31
                       MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE44-KAISU (IDX3 IDX2)
                       IF    (WK-TAIHI-FLG               =  ZERO)
                       AND   (RECE44-KAISU(1 IDX2)   NOT =  ZERO)
                           MOVE     IDX2    TO      WK-TAIHI-DAY
                           MOVE     1       TO      WK-TAIHI-FLG
                       END-IF    
                   END-PERFORM
               END-PERFORM
           ELSE
               PERFORM     VARYING   IDX3   FROM    1    BY   1
                           UNTIL     IDX3   >      10
                  MOVE    ACCT-DAY(IDX3 WRK-DAY)  
                                       TO    RECE44-KAISU(IDX3 WRK-DAY)
              END-PERFORM                         
              MOVE    WRK-DAY          TO    WK-TAIHI-DAY
           END-IF
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK     TO    RECE44-SAFLG
           END-IF
      *
           MOVE    RECE41-RECESYUBETU  TO  RECE44-RECESYUBETU
      *
           .
       21011-RECE44-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®
      *****************************************************************
       21012-RECE45-HENSYU-SEC               SECTION.
      *
           IF    (RECE41-SRYYM            >=  "201604")
           AND   (ACCT-SRYKBN              =  "23"    )
           AND   (ACCT-JIHOKBN             =   SPACE  )
             PERFORM 210126-SIPPU-CHECK-SEC
           END-IF
      *
           INITIALIZE     WRK-ORCSGETGE-OUT-AREA
           MOVE    ZERO                 TO      FLG-IRYORENKEI
           IF    ((WRK-PARA-PRTKBN  =  "2" OR "3" OR "4" OR "5") AND
                  (SYS-200501-INSPECT-GENERAL-KBN  =   "1"     ) AND
                  (RECE41-SRYYM         >=   "201204"          ))
           OR    ( FLG-SIPPU-ARI         =     1                )
             PERFORM 210125-IRYORENKEI-HENSYU-SEC
           END-IF
           MOVE    ZERO                 TO      IDX17
      *
           MOVE    SPACE                TO      WRK-CHECK-SRYCD
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
      *
               IF    WRK-CHECK-SRYCD  =  SPACE
                 MOVE   SRY-SRYCD(1)    TO      WRK-CHECK-SRYCD
               END-IF
      *
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE   ZERO   TO  FLG-ZANHAIKI
                                     FLG-FORMAL-KISAI
                   MOVE   ZERO   TO  WRK-RENNUM
                                     WRK-RENNUM2
                   MOVE   ZERO   TO  FLG-WRITE45X
                   PERFORM   210121-RECE45-TENSOU-SEC
                   IF  ACCT-JIHOKBN  =  SPACE
                     PERFORM 210122-RECE41-HENSYU-SEC
                   END-IF
                   IF  FLG-WRITE45X  =  1
                      PERFORM  210123-RECE45-WRITE-SEC
                      MOVE  1    TO  FLG-WRITE45
                   END-IF
                   IF  FLG-ZANHAIKI  =  1
                      PERFORM  210124-RECE45-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  210124-RECE45-REWRITE2-SEC
                   END-IF
                   MOVE     "tbl_sryact"   TO   MCP-TABLE
                   MOVE     "key2"         TO   MCP-PATHNAME
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       21012-RECE45-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       210121-RECE45-TENSOU-SEC                SECTION.
      *
           INITIALIZE                   WRK-TNS-TAIHI-AREA
      *
           MOVE    SPACE                TO      RECE45-REC
           INITIALIZE                           RECE45-REC
           MOVE    RECE41-HOSPNUM       TO      RECE45-HOSPNUM
           MOVE    RECE41-SRYYM         TO      RECE45-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE45-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE45-PTID
           MOVE    RECE41-RECEKA        TO      RECE45-RECEKA
           MOVE    RECE41-HKNID         TO      RECE45-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE45-TEKSTYMD
           MOVE    RECE41-PTNUM         TO      RECE45-PTNUM
           MOVE    SRY-ZAINUM           TO      RECE45-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE45-RENNUM
           MOVE    SRY-SRYKA            TO      RECE45-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE45-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE45-SRYSYUKBN
                                                RECE44-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE45-JIHIMONEY
      * 
           MOVE    ZERO                 TO      FLG-OSINKBN
                                                IDX4
      *
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
             ADD     1                  TO  IDX17
      *
      *      ³°Íè´ÉÍý²Ã»»¡ÊÆÉ¤ßÂØ¤¨²Ã»»¡Ë¤ÎÌ¤ÄûÀµÊ¬¤¬¤¢¤ë¾ì¹ç¤ÎÆÃÎãÁ¼ÃÖ
      *      ¡Ê³°Íè´ÉÍý²Ã»»¤¬¤¢¤ì¤Ð¡¢³°Íè´ÉÍý²Ã»»¡ÊÆÉ¤ßÂØ¤¨²Ã»»¡Ë¤Ø¡Ë
             IF      RECE41-YOBIETC(1)  =  1
               IF    SRY-SRYCD(IDX2)  =  "112011010"
                  MOVE   "101120020"    TO  SRY-SRYCD(IDX2)
                  COMPUTE   RECE44-SRYCDTOTAL  =  RECE44-SRYCDTOTAL
                                               -  112011010
                                               +  101120020
               END-IF
             END-IF
      *
             PERFORM    800-TENSU-TBL-SEC
      **     ÅÀ¿ô¥Þ¥¹¥¿¾ðÊóÂàÈò
             MOVE    TNS-CHUKSNTUBAN    TO  WRK-TNS-CHUKSNTUBAN(IDX2)
             MOVE    TNS-DATAKBN        TO  WRK-TNS-DATAKBN(IDX2)
             MOVE    TNS-TIMEKSNKBN     TO  WRK-TNS-TIMEKSNKBN(IDX2)
      *
             IF     (RECE41-HKNKBN        =  "4" OR "6"        )
             AND    (ACCT-SRYKBN          =  "80"              )
             AND    (SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                     OR "09593")
               IF    SRY-SRYCD(IDX2)(1:5) =  "09593"
                 PERFORM   2101213-JIBAI-KIZAI-TENHEN-SEC
                 MOVE   WRK-TEN-KZI     TO  WRK-TNS-TEN(IDX2)
               ELSE
                 PERFORM   2101214-JIBAI-BUNSYO-TENHEN-SEC
                 MOVE   WRK-TEN-BUN     TO  WRK-TNS-TEN(IDX2)
               END-IF
             ELSE
               IF   (ACCT-SRYKBN          =  "80"              )
               AND  (SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                     OR "09593")
                 IF  SRY-SRYSYUKBN = "809"
                   MOVE SRY-JIHIMONEY(IDX2)
                                        TO  WRK-TNS-TEN(IDX2)
                 ELSE
                   MOVE TNS-TEN         TO  WRK-TNS-TEN(IDX2)
                 END-IF
               ELSE
                 MOVE   TNS-TEN         TO  WRK-TNS-TEN(IDX2)
               END-IF
             END-IF
      *
             MOVE    TNS-GAITENTTLKBN   TO  WRK-TNS-GAITENTTLKBN(IDX2)
             MOVE    TNS-OSINKBN        TO  WRK-TNS-OSINKBN(IDX2)
             MOVE    TNS-KNSJISKBN      TO  WRK-TNS-KNSJISKBN(IDX2)
             MOVE    TNS-SRYCD          TO  WRK-TNS-SRYCD(IDX2)
      *
             IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE44-MEISAISU  =  RECE44-MEISAISU
                                             -  1
             END-IF
      *
             IF       SRY-SRYCD(IDX2)  =  "099999909" OR "099999910" OR
                                          "099999921" OR "099999925" OR
                                          "099999929" OR "099999931"
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
      *
             IF       SRY-SRYCD(IDX2)  =  "099999922"
               IF    (ACCT-JIHOKBN     =   SPACE)
               AND   (RECE41-HKNKBN    =   "6"  )
                  MOVE    1     TO    FLG-KIN-OKIKAE
               ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
               END-IF
             END-IF
      *
             IF       SRY-SRYCD(IDX2)  =  "099999923"
               IF    (ACCT-JIHOKBN          =  SPACE)
               AND   (RECE41-HKNKBN         =  "4"  )
               AND   (WRK-PARA-RECEKBN      =  "4"  )
               AND   (WRK-PARA-JIBAIPRTKBN  =  "2"  )
                  MOVE    1     TO    FLG-KIN-OKIKAE2
               ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
               END-IF
             END-IF
      *
             IF       SRY-SRYCD(IDX2)  =  "099999924"
               IF    (ACCT-JIHOKBN          =  SPACE       )
               AND   (RECE41-HKNKBN         =  "1"  OR  "2")
               AND   (RECE41-SYOKAIFLG      =  "1"         )
                  MOVE   SPACE  TO    RECE41-SYOKAIFLG
               END-IF
               MOVE       1     TO    FLG-HENSYU-NG
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
               IF     ACCT-JIHOKBN     =   SPACE
                  EVALUATE  SRY-SRYCD(IDX2)
                    WHEN     "099999935"
                      IF  RECE41-RIHA2-SINDAI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    1     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999936"
                      IF  RECE41-RIHA2-NOUKE    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    2     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999937"
                      IF  RECE41-RIHA2-UNDOKI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    3     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999938"
                      IF  RECE41-RIHA2-KOKYUKI  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    4     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999939"
                      IF  RECE41-RIHA2-SESSYOKU = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    5     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999940"
                      IF  RECE41-RIHA2-HAIYO    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    6     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999941"
                      IF  RECE41-RIHA2-SINDAI9  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    7     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999942"
                      IF  RECE41-RIHA2-KOKYUKI9 = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    8     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999943"
                      IF  RECE41-RIHA2-HAIYO9   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    9     TO    FLG-RIHA-OKIKAE
                      END-IF
                  END-EVALUATE
               ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
               END-IF
             END-IF
             IF       SRY-SRYCD(IDX2)  =  "099140004" OR "099400001"
                                       OR "099400002" OR "099400003"
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
             IF      (SRY-SRYCD(IDX2)  =  "099140011" OR "099140012")
             AND     (RECE41-SRYYM    >=  "201804"                  )
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
      *
             IF     ACCT-SRYKBN      =  "98"
               IF  (SRY-RENNUM       =   1         )
               AND (IDX2             =   1         )
               AND (SRY-SRYCD(IDX2)  =  "830000052")
                  MOVE    1     TO    FLG-SIPPU-COM
               ELSE
                 IF  FLG-SIPPU-COM   =   1
                  CONTINUE
                 ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
                 END-IF
               END-IF
               IF  (SRY-RENNUM       =   1         )
               AND (IDX2             =   1         )
               AND (SRY-SRYCD(IDX2)  =  "830000052"
                                    OR  "099200201")
                 PERFORM  VARYING  IDX33  FROM  1  BY  1
                           UNTIL   IDX33   >   10
                   PERFORM  VARYING  IDX34  FROM  1  BY  1
                             UNTIL   IDX34   >   31
                     IF   ACCT-DAY(IDX33 IDX34)  NOT =  ZERO
                       MOVE    1    TO   RECE41-SIPPUCHKFLG
                       MOVE    1    TO   RECE41-SIPPUCHK(IDX33 IDX34)
                     END-IF
                   END-PERFORM
                 END-PERFORM
               END-IF
             END-IF
      *
             IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                     (FLG-SIPPU-ARI         =  ZERO )       )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                     (FLG-SIPPU-ARI         =   1   ) AND
                     (SYS-200501-COMPRESS-KBN
                                        NOT =  "1"  )       )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                     (TNS-TEN               =  ZERO )       )
             OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
             OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                            OR "0086"       )
             OR     ( FLG-KIN-OKIKAE    NOT =  ZERO         )
             OR     ( FLG-KIN-OKIKAE2   NOT =  ZERO         )
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
                      "099208101" OR
                      "840000136" OR "840000140" OR
                      "099114001" OR "099140001" OR
                      "099140003" OR "099409905" OR
                      "099409906" OR "099409907" OR
                      "099409908" OR "099409909" OR
                      "099200301"                           )
             OR     ( SRY-SRYCD(IDX2)       =  "099509901"  )
      *
                  PERFORM   2101219-MEISAISU-HENSYU-SEC
      *
             END-IF
      *
             IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                  "6" OR  "7" OR
                                                  "8"
      *
              IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                      (FLG-SIPPU-ARI         =  ZERO )       )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                      (FLG-SIPPU-ARI         =   1   ) AND
                      (SYS-200501-COMPRESS-KBN
                                         NOT =  "1"  )       )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
              OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                             OR "0086"       )
              OR     ( FLG-KIN-OKIKAE    NOT =  ZERO         )
              OR     ( FLG-KIN-OKIKAE2   NOT =  ZERO         )
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
                       "099208101" OR
                       "840000136" OR "840000140" OR
                       "099114001" OR "099140001" OR
                       "099140003" OR "099409905" OR
                       "099409906" OR "099409907" OR
                       "099409908" OR "099409909" OR
                       "099200301"                           )
              OR     ( SRY-SRYCD(IDX2)       =  "099509901"  )
      *
               IF   FLG-KIN-OKIKAE   NOT =  ZERO
                 IF  WRK-TAIHI-ZAINUM  =  ZERO
                   MOVE   ACCT-ZAINUM     TO   WRK-TAIHI-ZAINUM
                 END-IF
                 IF  ACCT-ZAINUM  =  WRK-TAIHI-ZAINUM
                   IF  SRY-SRYCD(IDX2) = "099999922"
                     CONTINUE
                   ELSE
                     PERFORM   2101217-ICHIBUCOM-HENSYU-SEC
                   END-IF
                 END-IF
               END-IF
               IF   FLG-KIN-OKIKAE2  NOT =  ZERO
                 IF  WRK-TAIHI-ZAINUM2 =  ZERO
                   MOVE   ACCT-ZAINUM     TO   WRK-TAIHI-ZAINUM2
                 END-IF
                 IF  ACCT-ZAINUM  =  WRK-TAIHI-ZAINUM2
                   IF  SRY-SRYCD(IDX2) = "099999923"
                     CONTINUE
                   ELSE
                     PERFORM   2101218-ICHIBUCOM-HENSYU2-SEC
                   END-IF
                 END-IF
               END-IF
               IF   FLG-RIHA-OKIKAE  NOT =  ZERO
                     PERFORM   210121A-RIHAYMD-HENSYU-SEC
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
              ELSE
      *
               ADD     1                  TO    IDX4
               MOVE    SRY-SRYCD(IDX2)    TO    RECE45-SRYCD(IDX4)
      *
               IF    ((SRY-SRYCD(IDX2)    =     "101130030"  ) AND
                      (RECE41-SRYYM      <=     "201306"     ))
               OR    ((SRY-SRYCD(IDX2)    =     "101800470"  ) AND
                      (RECE41-SRYYM      >=     "201307"     ))
                        MOVE   "1"        TO    RECE41-KYUGYOFLG
                                                WRK-KYUGYOFLG
               END-IF
      *
               IF      SRY-SRYKBN         =     "12"
               AND     ACCT-JIHOKBN       =     SPACE
                   IF   SRY-SRYCD(IDX2)   =     "112007170"    OR
                                                "112702270"
                        MOVE   "1"      TO    RECE41-KEIZOKU-PRTKBN
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =  "112011310" OR "112705510"
                                          OR "112011710" OR "112016310"
                                          OR "112016550" OR "112017310"
                                          OR "112017450"
                                          OR "112016210" OR "112016410"
                                          OR "112017610"
                        MOVE   "2"      TO    RECE41-KEIZOKU-PRTKBN
                   END-IF
                   IF  (SRY-SRYCD(IDX2)   =     "101120060"    )
                   AND (RECE41-SRYYM     <=     "201306"       )
                        MOVE   "1"      TO    RECE41-SAISHIN-PRTKBN2
                   END-IF
                   IF  (SRY-SRYCD(IDX2)   =     "101120090"    )
                   AND (RECE41-SRYYM     <=     "201306"       )
                        MOVE   "1"      TO    RECE41-GAIRAIKSN-PRTKBN
                   END-IF
               END-IF
      *
               IF      SRY-SRYKBN         =     "13"
               AND     ACCT-JIHOKBN       =     SPACE
                   IF   SRY-SRYCD(IDX2)   =  "113001810" OR "113001910"
                                          OR "113002010" OR "113700610"
                                          OR "113700710" OR "113700810"
                                          OR "113029010"
                        MOVE   "1"        TO    RECE41-BEDSUFLG
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)      =    "160177770"
                  MOVE   "1"              TO    RECE41-KENTAIKASANFLG
                  MOVE   99               TO    RECE44-KNSJISGRPKBN
               END-IF
               IF     RECE44-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE44-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN         =     "60" OR "64" )
               AND   ( TNS-KNSJISKBN      =      2           )
                  MOVE  TNS-KNSJISKBN     TO    RECE44-KNSJISKBN
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE44-ZAITEN     =     ZERO                 )
               AND   ( RECE44-SAFLG      =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE44-SAFLG
                  END-IF
               END-IF
      *
               IF      ACCT-JIHOKBN  NOT =     SPACE
                  IF     RECE44-SAFLG   NOT =  SPACE
                        MOVE    "3"       TO    RECE44-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   RECE44-TOYAKUFLG = SPACE
                          MOVE  "4"       TO    RECE44-TOYAKUFLG
                        END-IF
                    ELSE
                        MOVE    "3"       TO    RECE44-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE44-ZAITEN     =     ZERO         )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
               AND   ( ACCT-JIHOKBN      =     SPACE        )
                IF   ACCT-SRYCDTOTAL(6:9) = "630010002"
                                         OR "630010005"
                                         OR "630010006"
                                         OR "630010007"
                  CONTINUE
                ELSE
                  IF     RECE44-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE44-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "212" OR "292" OR
                                               "222" OR "232" OR
                                               "295"
                          IF   RECE44-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE44-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE44-TOYAKUFLG
                        IF   (RECE45-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                  NOT =  SPACE)
                          IF  (WRK-PARA-PRTKBN  =
                                           "2" OR "3" OR "4" OR "5")
                          AND (SYS-200501-INSPECT-GENERAL-KBN = "1")
                             MOVE  "1"  TO  RECE45-INGAIFLG-RECE(IDX4)
                          END-IF
                          IF   FLG-SIPPU-ARI    =  1
                             MOVE  "1"  TO  RECE45-INGAIFLG-SIPPU(IDX4)
                          END-IF
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO    RECE45-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO    RECE45-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO    RECE45-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE45-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE45-GENERICYUKOKETA(IDX4) =
                                       IDX15  /  2
                               MOVE    1   TO  IDX15
                            END-IF
                          END-PERFORM
                        END-IF
                    END-IF
                  END-IF
                END-IF
               END-IF
      *
               IF     FLG-SIPPU-ARI  =  1
      **********  £±¡§±¡³°Ê¬¡¡£²¡§±¡ÆâÊ¬
                  IF   RECE44-ZAITEN  =   ZERO
                      MOVE  "1"         TO    RECE44-SIPPUFLG
                  ELSE
                      MOVE  "2"         TO    RECE44-SIPPUFLG
                  END-IF
                  IF  (SRY-SRYCD(IDX2)(1:1)  =  "6")
                  AND (TNS-MEIUSESKB         =   1 )
                      MOVE  "1"         TO    RECE45-SIPPUFLG(IDX4)
                  END-IF
                  IF   SRY-SRYCD(IDX2)(1:3)  =  "001"
                      MOVE  "810000001" TO    RECE45-SRYCD(IDX4)
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14"  )
               AND   ( RECE44-ZAITEN     =     ZERO  )
               AND   ( ACCT-ZAIKBN   NOT =     1     )
               AND   ( ACCT-JIHOKBN      =     SPACE )
                  IF     RECE44-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE44-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          IF   RECE44-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE44-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE44-TOYAKUFLG
                        IF   (RECE45-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                  NOT =  SPACE)
                          MOVE    "1"  TO  RECE45-INGAIFLG-RECE(IDX4)
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO    RECE45-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO    RECE45-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO    RECE45-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE45-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE45-GENERICYUKOKETA(IDX4) =
                                       IDX15  /  2
                               MOVE    1   TO  IDX15
                            END-IF
                          END-PERFORM
                        END-IF
                    END-IF
                  END-IF
               END-IF
      *
               IF      SRY-SRYKBN   =   "99"
                  IF      SRY-SRYCD(IDX2)     =   "099140011"
                      MOVE     "1"        TO    RECE44-HOMONCOMFLG
                      MOVE  "810000001"   TO    RECE45-SRYCD(IDX4)
                  END-IF
                  IF      SRY-SRYCD(IDX2)     =   "099140012"
                      MOVE     "2"        TO    RECE44-HOMONCOMFLG
                      MOVE  "810000001"   TO    RECE45-SRYCD(IDX4)
                  END-IF
                  IF      RECE44-HOMONCOMFLG  =   "2"
                      MOVE     "1"        TO    RECE41-COMCNTFLG
                      MOVE     "1"        TO    RECE45-RECEDENFLG(IDX4)
                      MOVE     "1"        TO    RECE45-RECEPTFLG(IDX4)
                  END-IF
               END-IF
      *
               IF   (ACCT-SRYKBN = "40" OR "50" OR "54" OR "60" OR
                                   "64" OR "70" OR "80"           )
               AND  (ACCT-YKZTEN =  ZERO                          )
                 IF  ( SYS-200501-15-YKZ-PRTKBN  =  "0" )
                 OR  ((SYS-200501-15-YKZ-PRTKBN  =  "1") AND
                      (RECE41-SRYYM  <  "201204"       ))
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE45-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE45-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-YKZ-PRTKBN
                         PERFORM   2101219-MEISAISU-HENSYU-SEC
                   ELSE
                     IF  FLG-YKZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE45-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE45-RECEPTFLG(IDX4)
                         PERFORM   2101219-MEISAISU-HENSYU-SEC
                       ELSE
                         MOVE   ZERO   TO  FLG-YKZ-PRTKBN
                       END-IF
                     END-IF
                   END-IF
                 ELSE
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE44-YKZKISAIFLG
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
                         MOVE   "1"    TO  RECE45-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE45-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-KIZ-PRTKBN
                         PERFORM   2101219-MEISAISU-HENSYU-SEC
                     END-IF
                   ELSE
                     IF  FLG-KIZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE45-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE45-RECEPTFLG(IDX4)
                         PERFORM   2101219-MEISAISU-HENSYU-SEC
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
                       COMPUTE  RECE44-RUIKEITOTAL  =
                                RECE44-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       IF  (ACCT-SRYKBN          =  "80" )
                       AND (SRY-SRYCD(IDX2)(1:5) =
                            "09591" OR "09592" OR "09593")
                       AND (SRY-SRYSYUKBN        =  "809")
                        COMPUTE WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-JIHIMONEY(IDX2))
                       ELSE
                        COMPUTE WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                       END-IF
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE44-RUIKEITOTAL  =
                            RECE44-RUIKEITOTAL  +
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
                   COMPUTE  RECE44-RUIKEITOTAL  =
                            RECE44-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE45-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE45-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE45-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE45-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE45-INPUTNUM(IDX4)
      **
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                  IF ((SRY-SRYCD(IDX2)  =  "101130010"     ) AND
                      (RECE41-SRYYM     <  "200804"        ))
                  OR ((SRY-SRYCD(IDX2)  =
                       "101110030"   OR "101120030"   OR
                       "101130020"   OR "101130030"   OR
                       "101130040"   OR "101130050"   OR
                       "101130060"   OR "101130070"   OR
                       "101130080"   OR "101130090"   OR
                       "101130100"                         ) AND
                      (RECE41-SRYYM    <=  "201306"        ))
                  OR ((SRY-SRYCD(IDX2)  =
                       "101800890"   OR "101130190"   OR
                       "101800870"   OR "101800470"   OR
                       "101800480"   OR "101800490"   OR
                       "101800500"   OR "101800510"   OR
                       "101800520"   OR "101800530"   OR
                       "101800540"   OR "101800960"   OR
                       "101800970"   OR "101800980"   OR
                       "101801040"                         ) AND
                      (RECE41-SRYYM    >=  "201307"        ))
                    IF   ((SRY-SRYCD(IDX2)  =   "101120030"  ) AND
                          (RECE41-SRYYM    <=   "201306"     ))
                    OR   ((SRY-SRYCD(IDX2)  =   "101130190"  ) AND
                          (RECE41-SRYYM    >=   "201307"     ))
                          MOVE   "13"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                    ELSE
                          MOVE   "80"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                    END-IF
                  ELSE
                    IF      TNS-SRYKBN         =     "80"
                    OR      RECE44-SRYKBN      =     "80"
                      IF  TNS-GAITENTTLKBN   =     130
                          MOVE   "13"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                      ELSE
                       IF SRY-SRYCD(IDX2)    =     "101989060"
                          MOVE   "11"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                       END-IF
                      END-IF
                    END-IF
                  END-IF
               ELSE                                                     
                  IF      SRY-SRYCD(IDX2)    =     "140037490"
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-YOUBUKASAN   =  WRK-YOUBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     "140040110"
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KYOBUKASAN   =  WRK-KYOBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     "140052610"
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KEIBUKASAN   =  WRK-KEIBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     "150266970"
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-SOGAIKASAN   =  WRK-SOGAIKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
      *           ¼«ÇåÀÕ¡¦ÎÅÍÜÃ´Åö¼êÅöÍÑÆÃÎã½èÍý
      *           ¡Ê¼«ÇåÀÕ¤Î¾ì¹ç¡¢£·ÅÀ¤ò£¸£´±ß¤ËÆÉ¤ßÂØ¤¨¤ë¡Ë
      *           ¡ÊÎÅÍÜÃ´Åö¼êÅö¡§£±·î£±²ó¤Î»»Äê¡Ë
                  IF      SRY-SRYCD(IDX2)    =     "199000610"
                    IF   (RECE41-HKNKBN    =  "4")
                    AND  (RECE41-JIBAIKBN  =  "1")
                        MOVE     84     TO     WRK-RYOYOTEATE2
                        COMPUTE  WRK-RYOYOTEATE   =
                                 WRK-RYOYOTEATE   +
                                 WRK-RYOYOTEATE2
                    ELSE
                        COMPUTE  WRK-RYOYOTEATE   =  WRK-RYOYOTEATE
                                                  +  TNS-TEN
                        COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                                  -  TNS-TEN
                        COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                                  +  TNS-TEN
                    END-IF
                  END-IF
      *
                  IF ( ACCT-SRYKBN          =  "80"               ) AND
                     ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592" )
      ***            EVALUATE  SRY-SRYCD(IDX2)(1:5)
      ***             WHEN      "09591"
      ***               IF  RECE41-SINDAN-TUU < 2
      ***                 MOVE   "95"   TO    RECE44-SRYKBN
      ***                                     RECE45-SRYKBN
      ***               END-IF
      ***             WHEN      "09592"
      ***               IF  RECE41-MEISAI-TUU < 2
      ***                 MOVE   "95"   TO    RECE44-SRYKBN
      ***                                     RECE45-SRYKBN
      ***               END-IF
      ***            END-EVALUATE
                     MOVE    1          TO    FLG-SRYKBN-HEN
                  ELSE
                   IF ( SRY-SRYCD(IDX2)(1:5) = "09593"       )
                   OR ((SRY-SRYCD(IDX2)  =  "101130010"     ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                   OR ((RECE41-HKNKBN    =  "4"             ) AND
                       (RECE41-JIBAIKBN  =  "1"             ) AND
                       (SRY-SRYCD(IDX2)  =  "199000610"     ))
                   OR ((SRY-SRYCD(IDX2)  =
                        "101110030"   OR "101120030"   OR
                        "101130020"   OR "101130030"   OR
                        "101130040"   OR "101130050"   OR
                        "101130060"   OR "101130070"   OR
                        "101130080"   OR "101130090"   OR
                        "101130100"                         ) AND
                       (RECE41-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX2)  =
                        "101800890"   OR "101130190"   OR
                        "101800870"   OR "101800470"   OR
                        "101800480"   OR "101800490"   OR
                        "101800500"   OR "101800510"   OR
                        "101800520"   OR "101800530"   OR
                        "101800540"   OR "101800960"   OR
                        "101800970"   OR "101800980"   OR
                        "101801040"                         ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)  >= "101801010" AND
                                         <= "101801020"     ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)   = "101801030"     ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                     IF  SRY-SRYCD(IDX2)(1:5) = "09593"
                       MOVE  TNS-NAMECHGKBN
                                      TO  WRK-TNS-JIBAISYUKEIKBN(IDX2)
                                          RECE44-JIBAISYUKEIKBN
                       EVALUATE  TNS-NAMECHGKBN
                         WHEN      0
                         WHEN      4
                         WHEN      5
                            MOVE   "80"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                         WHEN      1
                            MOVE   "11"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                         WHEN      2
                            MOVE   "12"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                         WHEN      3
                            MOVE   "13"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                       END-EVALUATE
                     ELSE
                       IF   ((SRY-SRYCD(IDX2)  =   "101120030"  ) AND
                             (RECE41-SRYYM    <=   "201306"     ))
                       OR   ((SRY-SRYCD(IDX2)  =   "101130190"  ) AND
                             (RECE41-SRYYM    >=   "201307"     ))
                            MOVE   "13"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                       ELSE
                            MOVE   "80"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                       END-IF
                     END-IF
                     MOVE    1          TO    FLG-SRYKBN-HEN
                   ELSE                                                 
                     IF      TNS-SRYKBN         =   "80"
                     OR      RECE44-SRYKBN      =   "80"
                         IF  TNS-GAITENTTLKBN   =   130
                             MOVE   "13"    TO  RECE44-SRYKBN
                                                RECE45-SRYKBN
                         END-IF
                     END-IF
                     IF  (WRK-PARA-JIBAIPRTKBN =  "0" )
                     AND (FLG-SRYKBN-HEN       =  ZERO)
                      IF (( RECE44-SRYKBN     =  "80" ) AND
                          ( RECE44-OKIKAEFLG  =  SPACE))
                      OR (  RECE44-OKIKAEFLG  =  "1"   )
                       IF   SRY-SRYCD(IDX2)   =     "120002710"    OR   
                                                    "120002810"    OR   
                                                    "120002910"    OR   
                                                    "120003010"    OR   
                                                    "120003710"    OR
                                                    "120002470"    OR   
                                                    "120002570"    OR   
                                                    "120003270"    OR
                                                    "120003470"    OR
                                                    "120003570"    OR
                                                    "120003970"    OR
                                                    "120004270"    OR
                                                    "120004570"    OR
                                                    "120004610"    OR
                                                    "199000610"
                              CONTINUE                                  
                       ELSE                                             
                              MOVE   "1"    TO  RECE44-OKIKAEFLG
                              MOVE   "40"   TO  RECE44-SRYKBN
                                                RECE45-SRYKBN
                       END-IF                                           
                      END-IF
                     END-IF
                   END-IF                                               
                  END-IF                                                
      *
               END-IF                                                   
      *
               IF      FLG-SIPPU-COM      =     1
                       MOVE   "1"         TO    RECE44-SIPPUCOMFLG
                       MOVE   "80"        TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                       MOVE   "800"       TO    RECE44-SRYSYUKBN
                                                RECE45-SRYSYUKBN
               END-IF
      *
               IF      ACCT-JIHOKBN       =     SPACE
                  PERFORM  2101211-RECE45-COMCNT-HEN-SEC
               ELSE
                  IF   RECE44-SRYKBN      =     "50"
                     MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
                  END-IF
                  IF   RECE44-SRYKBN      =     "54"
                     MOVE   "1"   TO    RECE44-FLG-MASUI-COMMENT
                  END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE45-RECEDENFLG(IDX4)
                  MOVE    "1"             TO  RECE45-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE45-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.99999                   
                  COMPUTE RECE45-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE45-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE45-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE45-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE45-TANINAME(IDX4)   
                    IF    RECE45-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE45-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE45-SRYSURYO(IDX4 - 1) 
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE45-RECEDENFLG(IDX4)
                    MOVE  "1"             TO  RECE45-RECEPTFLG(IDX4)
                  END-IF                                                
                END-IF
               ELSE                                                     
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO    RECE45-TANICD(IDX4)
                                                WRK-TANICD
                  END-IF
                  MOVE    TNS-TANIMOJISU  TO    RECE45-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE45-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF ( ACCT-SRYKBN          =  "80"              ) AND     
                  ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                    OR "09593")         
                    MOVE  1               TO  RECE45-TENSIKIBETU(IDX4)  
      ***>          MOVE  TNS-TEN         TO  RECE45-TEN(IDX4)          
                    MOVE  WRK-TNS-TEN(IDX2)  TO  RECE45-TEN(IDX4)
               ELSE                                                     
                 IF   RECE41-HKNKBN     =  "4"
                 AND  RECE41-JIBAIKBN   =  "1"
                 AND  SRY-SRYCD(IDX2)   =  "199000610"
                    MOVE  1               TO  RECE45-TENSIKIBETU(IDX4)  
                    MOVE  WRK-RYOYOTEATE2 TO  RECE45-TEN(IDX4)
                 ELSE
                    MOVE  TNS-TENSIKIBETU TO  RECE45-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN         TO  RECE45-TEN(IDX4)
                 END-IF
               END-IF                                                   
               IF      RECE41-NYUGAIKBN   =     "1"
                   MOVE  TNS-NYUTENTTLKBN TO    RECE45-TENTTLKBN(IDX4)
               ELSE
                   MOVE  TNS-GAITENTTLKBN TO    RECE45-TENTTLKBN(IDX4)
               END-IF
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE45-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE45-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE45-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE45-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE45-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE45-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE45-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE45-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE45-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE45-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE45-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE45-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE45-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE45-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE45-KNSJISGRPKBN   (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE45-KENTAICOMMENT  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE45-KOKUJISKBKBN1  (IDX4)
               MOVE    TNS-KOKUJISKBKBN2
                                      TO RECE45-KOKUJISKBKBN2  (IDX4)
               MOVE    TNS-TUSOKUGAIKBN
                                      TO RECE45-TUSOKUGAIKBN   (IDX4)
               MOVE    TNS-TIMEKSNKBN TO RECE45-TIMEKSNKBN     (IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE45-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE45-CHUKSNTUBAN    (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE45-TOKUKIZAISBT1  (IDX4)
               MOVE    TNS-SANSOKBN   TO RECE45-SANSOKBN       (IDX4)
               IF      TNS-SRYCD (1:2)   =   "84"
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       10
                       MOVE    TNS-SSTKIJUNCD (IDYY)
                                      TO RECE45-SSTKIJUNCD(IDX4 IDYY)
                   END-PERFORM
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE41-SRYYM   <=  "201003"   )
                      MOVE   202      TO  RECE45-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE45-CDKBN-KOUBAN(IDX4)
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
                      MOVE   203      TO  RECE45-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE45-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               MOVE   TNS-SRYKBN      TO  RECE45-TNSSRYKBN(IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE41-SRYYM   >=  "201004"   )
                      MOVE   "1"      TO  RECE44-GAZOGEN
               END-IF
      *
               IF   (((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008")) AND
                     ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  ))
               OR   (( FLG-SIPPU-ARI                =    1    ) AND
                     ( SRY-SRYCD    (IDX2)(1:3)     =   "001" ) AND
                     ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  ))
                   MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 800-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT
                                           TO  RECE45-NAME(IDX4)
                                               RECE45-INPUTCOMENT(IDX4)
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
                                           RECE45-INPUTCOMENT   (IDX4)
                           END-IF
                         ELSE
                               MOVE    SPACE       TO
                                           RECE45-INPUTCOMENT   (IDX4)
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
                                           RECE45-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE45-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
      ***          IF    RECE44-COMCDFLG   NOT =   "3"
      ***             MOVE   "1"           TO    RECE44-COMCDFLG
      ***          END-IF
      *
                   IF      WRK-KYUGYOFLG   =   "1"
                   AND     SRY-SRYCD(IDX2) = "840800001"
                        MOVE   PTCOM-INPUTCHI1(1:2)
                                            TO  RECE41-KYUGYO-STR(1:2)
                        MOVE   PTCOM-INPUTCHI2(1:2)
                                            TO  RECE41-KYUGYO-STR(3:2)
                        MOVE   PTCOM-INPUTCHI3(1:2)
                                            TO  RECE41-KYUGYO-END(1:2)
                        MOVE   PTCOM-INPUTCHI4(1:2)
                                            TO  RECE41-KYUGYO-END(3:2)
                        MOVE   SPACE        TO  WRK-KYUGYOFLG
                   END-IF
      *
               ELSE
                   MOVE    TNS-NAME        TO  RECE45-NAME(IDX4)
                                               RECE45-INPUTCOMENT(IDX4)
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                  IF  RECE45-SRYSURYO(IDX4)  NOT = ZERO
                    PERFORM  2101215-RECE45-SURYO-HEN-SEC
      *
                    MOVE    SPACE      TO  RECE45-NAME(IDX4)
                    MOVE    SPACE      TO  RECE45-INPUTCOMENT(IDX4)
      ***           STRING  TNS-NAME          DELIMITED  BY  " "
      ***                   "¡¡"              DELIMITED  BY  SIZE
      ***                   WRK-SRYSURYOW     DELIMITED  BY  " "
      ***                   RECE45-TANINAME(IDX4)
      ***                                     DELIMITED  BY  " "
      ***                   INTO              RECE45-NAME(IDX4)
      ***           END-STRING
                    STRING  "¡Ê»ÈÍÑÎÌ¡¡"      DELIMITED  BY  SIZE
                            WRK-SRYSURYOW     DELIMITED  BY  " "
                            RECE45-TANINAME(IDX4)
                                              DELIMITED  BY  " "
                            "¡¡»ÄÎÌÇÑ´þ¡Ë"    DELIMITED  BY  SIZE
                            INTO              RECE45-NAME(IDX4)
                    END-STRING
                    MOVE  RECE45-NAME(IDX4)
                                       TO  RECE45-INPUTCOMENT(IDX4)
      *
      ***           MOVE  "810000001"  TO  RECE45-SRYCD(IDX4)
                    MOVE  ZERO         TO  RECE45-SRYSURYO(IDX4)
                    MOVE  ZERO         TO  RECE45-TANICD(IDX4)
                    MOVE  ZERO         TO  RECE45-TANIMOJISU(IDX4)
                    MOVE  SPACE        TO  RECE45-TANINAME(IDX4)
                    MOVE  ZERO         TO  RECE45-YUKOKETA(IDX4)
                    MOVE  "2"          TO  RECE45-MEISKYFLG(IDX4)
                  END-IF
               END-IF
      *
               IF          SRY-SRYKBN       =  "21" OR "22" OR "23"
                    IF    SRY-SRYCD(IDX2)  =  "630010002"
                                           OR "630010005"
                                           OR "630010006"
                                           OR "630010007"
                                           OR "820000047"
                                           OR "820000166"
                        MOVE     "3"       TO    RECE44-COMCDFLG
                    END-IF
               END-IF
               IF   ( SRY-SRYCD(IDX2)(1:1)  =   "8"   )
               OR   ( SRY-SRYCD(IDX2)(1:3)  =   "008" )
               OR   ((FLG-SIPPU-ARI         =    1   ) AND
                     (SRY-SRYCD(IDX2)(1:3)  =   "001"))
                    IF    RECE44-COMCDFLG   NOT =   "3"
                        MOVE     "1"       TO    RECE44-COMCDFLG
                    END-IF
               END-IF
      *
      ***      IF    RECE44-COMCDFLG       =   "3"
      ***        IF    SRY-SRYCD(IDX2)  =  "630010002"
      ***                               OR "630010005"
      ***                               OR "630010006"
      ***                               OR "630010007"
      ***                               OR "820000047"
      ***                               OR "820000166"
      ***          CONTINUE
      ***        ELSE
      ***          ADD   999               TO  WRK-SURYOU-COUNT
      ***          ADD   WRK-SURYOU-COUNT  TO  RECE44-SURYOUTOTAL
      ***        END-IF
      ***      END-IF
      *
               IF   (SRY-SRYKBN       =  "31" OR "32")
               AND  (ACCT-JIHOKBN     =  SPACE       )
                IF  (RECE41-HKNKBN         =  "4")
                AND (WRK-PARA-RECEKBN      =  "4")
                AND (WRK-PARA-JIBAIPRTKBN  =  "2")
                 CONTINUE
                ELSE
                 IF        SRY-SRYCD(IDX2)  =    "130000510"
      ***                                  OR    "130000110"
      ***                                  OR    "130000310"
                                           OR    "130003510"
      ***                                  OR    "130003670"
                   MOVE    SPACE           TO    RECE45-NAME(IDX4)
                 END-IF
                END-IF
               END-IF
               IF  ( SRY-SRYSYUKBN         =  "334"       )
               AND ((SRY-SRYCD(IDX2)       =  "810000001") OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"      ))
               AND ( FLG-HOUTEN-COM        =   ZERO       )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                            UNTIL  (IDV            >   300 )
                             OR    (FLG-HOUTEN-COM =     1 )
                      IF   RECE45-NAME(IDX4)(IDV:2)   =   "Ë¬"
                         MOVE   SPACE  TO    RECE44-FLG-HOUTEN-COMMENT
                         MOVE    1     TO    FLG-HOUTEN-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  ( SRY-SRYKBN     =  "60" OR "64" )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-KENSA-COM  =   ZERO        )
                 IF  SRY-SRYCD(IDX2) = "840000085" OR "840000087"
                    CONTINUE
                 ELSE
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                              UNTIL  (IDV            >   300)
                               OR    (FLG-KENSA-COM  =    1 )
                      IF  RECE45-NAME(IDX4)(IDV:2)   =  "Æü"
                        MOVE   SPACE  TO  RECE44-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  ( SRY-SRYKBN       =  "14"       )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-ZAIRIHA-COM  =   ZERO      )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                              UNTIL  (IDV              >   300)
                               OR    (FLG-ZAIRIHA-COM  =    1 )
                      IF  RECE45-NAME(IDX4)(IDV:2)   =  "Æü"
                        MOVE   SPACE  TO  RECE44-FLG-ZAIRIHA-COMMENT
                        MOVE     1    TO  FLG-ZAIRIHA-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  ( SRY-SRYKBN = "12" OR "13" OR
                                  "14" OR "80"      )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
                 IF (RECE45-NAME(IDX4)(1:20) = "´ÉÍýÎÁÅù»»Äê³«»ÏÇ¯·î")
                 OR (RECE45-NAME(IDX4)(1:18) = "´ÉÍýÎÁ»»Äê³«»ÏÇ¯·î"  )
                   MOVE   "1"   TO   RECE44-FLG-ONLINE3-COMMENT
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE45-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE45-NAME(IDX4)
                                              WRK-FORMALNAME
                 MOVE  "1"                TO  RECE45-RECEDENFLG(IDX4)
      *
                 MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                 IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                    MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                    MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                 END-IF
                 MOVE     ZERO            TO  RECE45-TEN(IDX4)
                 MOVE     ZERO            TO  RECE45-TANICD(IDX4)
                 MOVE     ZERO            TO  RECE45-TANIMOJISU(IDX4)
                 MOVE     SPACE           TO  RECE45-TANINAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE45-RENNUM  >  1
                    AND   IDX4           =  1
                      MOVE    1           TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE45-RENNUM  -  1
                    ELSE
                      MOVE  "1"           TO  RECE45-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  WRK-FORMALNAME  TO  RECE45-RECEDENNAME(IDX4)
                    MOVE  "1"             TO  RECE45-RECEHENFLG(IDX4)
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                          TO  RECE45-TEN(IDX4)
                    END-IF
                    IF   (WRK-KZI058-TANICD  NOT = ZERO)
                    AND  (RECE45-TANICD(IDX4)    = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                          TO  RECE45-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                          TO  RECE45-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                          TO  RECE45-TANINAME(IDX4)
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
               AND (RECE45-NAME(IDX4)(1:14)
                                          =  "ÊÝ¸±Ì¾¾ÎÊÑ¹¹¡§")
                  MOVE     "1"          TO  RECE45-RECEPTFLG(IDX4)
                  MOVE     "1"          TO  RECE45-RECEDENFLG(IDX4)
      *
                  MOVE      ZERO        TO  IDW3
                  PERFORM   VARYING   IDW   FROM   300   BY  -1
                             UNTIL    IDW     <    15
                       IF  RECE45-NAME(IDX4)(IDW:1)  NOT =  SPACE
                           COMPUTE IDW6       =  (IDW / 2) - 7
                           MOVE    IDW        TO  IDW3
                           MOVE    1          TO  IDW
                       END-IF
                  END-PERFORM
                  IF     IDW3    NOT =  ZERO
                    MOVE  SPACE TO RECE45-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  RECE45-NAME(IDX4)(15:IDW4)
                                TO RECE45-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ"
                                TO RECE45-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE45-YUKOKETA(IDX4 - 1) =
                              RECE45-YUKOKETA(IDX4 - 1) -
                              3                         +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE45-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   300   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE45-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE45-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF (  SRY-SRYCD(IDX2) = "101110010"  OR
                                       "101110040"     )
               OR (( ACCT-SRYKBN          =  "80"     )   AND           
                   ( SRY-SRYCD(IDX2)(1:5) =  "09591"
                                          OR "09592"
                                          OR "09593"  ))                
               OR (( SRY-SRYCD(IDX2) = "101130010"    )   AND
                   ( RECE41-SRYYM    < "200804"       ))
               OR (( SRY-SRYCD(IDX2) =
                     "101110030"   OR "101120030"   OR
                     "101120040"   OR "101120050"   OR
                     "101120060"   OR "101120100"   OR
                     "101130020"   OR "101130030"   OR
                     "101130040"   OR "101130050"   OR
                     "101130060"   OR "101130070"   OR
                     "101130080"   OR "101130090"   OR
                     "101130100"                      )   AND
                   ( RECE41-SRYYM   <= "201306"       ))
               OR (( SRY-SRYCD(IDX2)  =
                     "101800890"   OR "101130190"   OR
                     "101120010"   OR "101120040"   OR
                     "101800870"   OR "101800470"   OR
                     "101800480"   OR "101800490"   OR
                     "101800500"   OR "101800510"   OR
                     "101800520"   OR "101800530"   OR
                     "101800540"   OR "101800960"   OR
                     "101800970"   OR "101800980"   OR
                     "101120050"   OR "101120060"   OR
                     "101120070"   OR "101120080"   OR
                     "101801040"                      )   AND
                   ( RECE41-SRYYM   >= "201307"       ))
               OR (( SRY-SRYCD(IDX2) >= "101801010" AND
                                     <= "101801020"   )   AND
                   ( RECE41-SRYYM   >=  "201307"      ))
               OR (( SRY-SRYCD(IDX2)  = "101801030"   )   AND
                   ( RECE41-SRYYM   >=  "201307"      ))
      *
      *           IF     TNS-TENSIKIBETU   =   1
                     ADD      1          TO  WRK-MEISAI-CNT
                     IF  SRY-SRYSYUKBN = "809"
                        COMPUTE  WRK-TEN = WRK-TEN +
                                          (SRY-JIHIMONEY(IDX2) / 10)
                     ELSE
                      IF   SRY-SRYCD(IDX2)  =  "101801030"
                        COMPUTE  WRK-FUTANWARI  = TNS-TEN / 100
                        COMPUTE  WRK-KEISAN-KG  =
                                (WRK-TEN-KG  * WRK-FUTANWARI) + 0.5
                        COMPUTE  WRK-TEN = WRK-TEN + WRK-KEISAN-KG
                        COMPUTE  WRK-KEISAN-KG2 =
                                (WRK-TEN-KG2 * WRK-FUTANWARI)
                      ELSE
                        COMPUTE  WRK-TEN = WRK-TEN +
                                          (TNS-TEN / 10)
                        COMPUTE  WRK-TEN-KG  = TNS-TEN / 10
                        COMPUTE  WRK-TEN-KG2 = TNS-TEN
                      END-IF
                     END-IF
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *           END-IF
      *
                  MOVE     "2"     TO    RECE45-SIKFLG(IDX4)
      *
                  IF  (ACCT-SRYKBN          = "80"   )
                  AND (SRY-SRYCD(IDX2)(1:5) =
                        "09591" OR "09592" OR "09593")
                        COMPUTE  WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                                 + WRK-TNS-TEN(IDX2)
                  ELSE
                      IF  SRY-SRYCD(IDX2)  =  "101801030"
                        COMPUTE  WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                                 + WRK-KEISAN-KG2
                        MOVE     WRK-KEISAN-KG2  TO  RECE45-TEN(IDX4)
                      ELSE
                        COMPUTE  WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                                 + TNS-TEN
                      END-IF
                  END-IF
      *
               ELSE
      *
                  IF   RECE41-HKNKBN     =  "4"
                  AND  RECE41-JIBAIKBN   =  "1"
                  AND  SRY-SRYCD(IDX2)   =  "199000610"
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                             TNS-TEN
      *
                     COMPUTE  WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                              + WRK-RYOYOTEATE2
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO  RECE45-SIKFLG(IDX4)
      *
                  ELSE
      *
                    IF  ((RECE41-HKNKBN      =   "4") AND
                         (RECE41-JIBAIKBN    =   "2"))
                    OR  ((RECE41-HKNKBN      =   "5") AND
                         (RECE41-KOMUKBN     =   "2"))
                    OR  ( RECE41-HKNKBN      =   "6" )
                      IF (( ACCT-SRYKBN            =  "80"      ) AND
                          ( WRK-CHECK-SRYCD(1:5)   =  "09591"
                                                   OR "09592"
                                                   OR "09593"   ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( WRK-CHECK-SRYCD(1:5)   =  "09591"
                                                   OR "09592"
                                                   OR "09593"   ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE45-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE44-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE44-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE45-SIKFLG(IDX4)
                      END-IF
                    ELSE
                      IF (  SRY-SRYCD(IDX2) = "840800001"  OR
                                              "840800002"        )
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000020"   ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "12"              ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000021"   ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( WRK-CHECK-SRYCD =  "101130010"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE41-SRYYM    <  "200804"         ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( WRK-CHECK-SRYCD(1:5)   =  "09591"
                                                   OR "09592"
                                                   OR "09593"   ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ))
                      OR (( WRK-CHECK-SRYCD =  "101110030"      ) AND
                          ( RECE41-SRYYM   <=  "201306"         ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( WRK-CHECK-SRYCD =
                            "101130020"   OR  "101130030"   OR
                            "101130040"   OR  "101130050"   OR
                            "101130060"   OR  "101130070"   OR
                            "101130080"   OR  "101130090"   OR
                            "101130100"                         ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE41-SRYYM   <=  "201306"         ))
                      OR (( WRK-CHECK-SRYCD =  "101800890"      ) AND
                          ( RECE41-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD =
                            "101800870"   OR  "101800470"   OR
                            "101800480"   OR  "101800490"   OR
                            "101800500"   OR  "101800510"   OR
                            "101800520"   OR  "101800530"   OR
                            "101800540"   OR  "101800960"   OR
                            "101800970"   OR  "101800980"   OR
                            "101801040"                         ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE41-SRYYM   >=  "201307"         ))
                      OR (( WRK-CHECK-SRYCD =  "101130190"      ) AND
                          ( RECE41-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD >= "101801010" AND
                                            <= "101801020"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE41-SRYYM   >=  "201307"         ))
                       IF  (WRK-CHECK-SRYCD(1:6) = "101989"   )
                       OR  (WRK-CHECK-SRYCD      = "112023210")
                         MOVE  "1"     TO    RECE45-SIKFLG(IDX4)
                       ELSE
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE45-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE44-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE44-COMCDFLG
                         END-IF
                       END-IF
                      ELSE
                         MOVE  "1"     TO    RECE45-SIKFLG(IDX4)
                      END-IF
                    END-IF
      *
                  END-IF
      *
               END-IF
      *
      *        ÆÃÄê´ïºà¡Ê²èÁü¿ÇÃÇ¤Î¥Õ¥£¥ë¥à°Ê³°¡Ë¤Î¤È¤­Ã±²Á¤òÊÔ½¸
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
                   IF   (  SRY-SRYCD (IDX2)         =
                                    "799990070"            OR
                                    "770020070"            OR
                                    "770030070"               )
      ***          OR   (  TNS-DATAKBN              =   "3"   )
                       CONTINUE
                   ELSE
      **               SET     TBL-SANSO           TO  1
      **               SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
      **                   AT  END
      **                       MOVE    ZERO            TO  FLG-SANSO-OK
      **                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
      **                                                        (TBL-SANSO)
      **                       MOVE    1               TO  FLG-SANSO-OK
      **               END-SEARCH
      **               IF      FLG-SANSO-OK        =   "1"
                     IF      RECE45-TEN(IDX4)  =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 2101212-RECE45-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE45-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE45-NAME(IDX4)(IDW:)   
                       COMPUTE RECE45-YUKOKETA(IDX4)
                                               =   RECE45-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF        
      *       
      *----------------------------------------------------------------
      *        ºßÂðÉôÊ¬¤Î¥ì¥»¥×¥ÈÊÔ½¸ÍÑ¤Ë±ý¿Ç¡ÊÆÃÊÌ±ý¿Ç¡Ë¡¦Ìë´Ö¡¦
      *        ¿¼Ìë¡¦¶ÛµÞ¤ÎºÞÈ½Äê¤òÄÉ²Ã
      *----------------------------------------------------------------
               IF    SRY-SRYKBN     =  "14"
      *
                 IF  SRY-SRYCD(IDX2) =  "114001110"
                                    OR  "114012910"
                                    OR  "114018010"
                                    OR  "114030310"
                                    OR  "114042110"
                                    OR  "114042210"
                                    OR  "114042810"
                                    OR  "114046310"
                                    OR  "114001470"
                    MOVE        9         TO    FLG-OSINKBN
                 END-IF
      *
                 EVALUATE    SRY-SRYCD(IDX2)
      *±ý¿Ç¡ÊÆÃÊÌ±ý¿Ç¡Ë
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   WHEN    "114002470"
      ***          WHEN    "114002970"
                   IF  FLG-OSINKBN        NOT = 2  AND  3  AND  9
                       MOVE     1         TO    FLG-OSINKBN
                   END-IF
      *Ìë´Ö
                   WHEN    "114000470"
                   WHEN    "114001970"
                   WHEN    "114011670"
                   WHEN    "114011970"
                   WHEN    "114017570"
                   WHEN    "114017870"
      *
                   WHEN    "114029270"
                   WHEN    "114029370"
                   WHEN    "114029470"
                   WHEN    "114029570"
                   WHEN    "114029970"
                   WHEN    "114030070"
                   WHEN    "114030170"
                   WHEN    "114030270"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     2         TO    FLG-OSINKBN
                   END-IF
      *¿¼Ìë¡¦¶ÛµÞ
                   WHEN    "114000370"
                   WHEN    "114000570"
                   WHEN    "114001870"
                   WHEN    "114002070"
                   WHEN    "114011570"
                   WHEN    "114011770"
                   WHEN    "114011870"
                   WHEN    "114012070"
                   WHEN    "114017470"
                   WHEN    "114017770"
                   WHEN    "114017670"
                   WHEN    "114017970"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     3         TO    FLG-OSINKBN
                   END-IF
                 END-EVALUATE
      *
                 IF    RECE41-SRYYM   >=  "201404"
                  EVALUATE    SRY-SRYCD(IDX2)
      *            ±ý¿Ç
                   WHEN    "114000110"
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(1 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                     MOVE  1  TO  FLG-ZAITAKU-CHECK
      *            ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡Ê±ý¿Ç¡Ë
                   WHEN    "114000970"
                    IF  FLG-ZAITAKU-CHECK = ZERO
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(2 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                    END-IF
      *            ÆÃÊÌ±ý¿Ç
                   WHEN    "114001610"
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(3 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                     MOVE  1  TO  FLG-ZAITAKU-CHECK
      *            ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡ÊÆÃÊÌ±ý¿Ç¡Ë
                   WHEN    "114002470"
                    IF  FLG-ZAITAKU-CHECK = ZERO
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(4 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                    END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ
                   WHEN    "114001110"
                   WHEN    "114012910"
                   WHEN    "114018010"
                   WHEN    "114030310"
                   WHEN    "114042110"
                   WHEN    "114042210"
                   WHEN    "114042810"
                   WHEN    "114046310"
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(5 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                     MOVE  1  TO  FLG-ZAITAKU-CHECK
      *            ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡ÊºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ë
                   WHEN    "114001470"
                    IF  FLG-ZAITAKU-CHECK = ZERO
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(6 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                    END-IF
                  END-EVALUATE
                 END-IF
      *
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE44-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE45-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE45-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE45-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE45-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE44-COMCDFLG
                     MOVE  "810000001"  TO  RECE45-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE45-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE45-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE45-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE45-TANINAME(IDX4)
               END-IF
      *
               PERFORM  2101216-SETCOM-HENSYU-SEC
      *
               IF   (RECE41-SRYYM           >= "201810")
               AND  (RECE45-SRYCD(IDX4)(1:4) = "8201"  )
                 MOVE   "1"         TO  RECE44-SENTAKUCOMFLG
               END-IF
      *
      *        £°£¹£µ£¹£³¤Ç»Ï¤Þ¤ë¥³¡¼¥É¤¬¤¢¤ë¾ì¹ç¡¢Ï«ºÒ¥ì¥»ÅÅ¤ÎºîÀ®ÂÐ¾Ý³°
               IF    (RECE41-HKNKBN           =   "1" OR "2")
               AND   (RECE41-SRYYM           >=   "201307"  )
               AND   (RECE41-RSIIRYOKBN   NOT =   "0"       )
               AND   (SYS-200501-ROURECEKBN   =   "1" OR "3")
                 IF  (ACCT-SRYKBN          =  "80"   )
                 AND (SRY-SRYCD(IDX2)(1:5) =  "09593")
                    MOVE     1      TO  RECE41-ORIGINALCDKBN
                 END-IF
               END-IF
      *
               IF    (RECE41-SRYYM         >=  "201704")
               AND   (SRY-SRYCD(IDX2)(1:3)  =  "059"   )
                 IF   (RECE41-HKNKBN   NOT =  "1" AND "2" )
                 OR   (RECE41-RECESYUBETU  =  "0000"      )
                 OR   (RECE41-ERR-ERRKBN   >   ZERO       )
                 OR   (SRY-SRYKBN          =  "95" OR "96")
      ***        OR   ((WRK-PARA-PRTKBN    =  "1"   ) AND
      ***              (ACCT-ZAITEN        =   ZERO )     )
                   CONTINUE
                 ELSE
                   MOVE     15      TO  RECE41-ERR-ERRKBN
                   DISPLAY "059 ERR:SRYCD=" SRY-SRYCD(IDX2)
                 END-IF
               END-IF
      *
              END-IF
             END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE45X
           END-IF
      *
           .
       210121-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       2101211-RECE45-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE44-SRYKBN      =     "11" OR "12" OR "13"
              IF   SRY-SRYCD(IDX2)    =  "111012970" OR "111013070"
                                      OR "111013170" OR "111013270"
                                      OR "111013370" OR "111013470"
                                      OR "111013570" OR "111013670"
                                      OR "112021370" OR "112021470"
                                      OR "112021570" OR "112021670"
                                      OR "112022070" OR "112022170"
                                      OR "112022270" OR "112022370"
                                      OR "112022870" OR "112022470"
                                      OR "112022570" OR "112022670"
                                      OR "112022770" OR "112022970"
                                      OR "112023070" OR "112023170"
                                      OR "113024970" OR "113025070"
                                      OR "113025170" OR "113025270"
                                      OR "113025370" OR "113025470"
                                      OR "113025570" OR "113025870"
                                      OR "113025970" OR "113026070"
                                      OR "113026170" OR "113026270"
                                      OR "113026370" OR "113026470"
                 EVALUATE   RECE41-NINPUFLG
                    WHEN        0
                            MOVE    1    TO    RECE41-NINPUFLG
                    WHEN        2
                            MOVE    3    TO    RECE41-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
           IF      RECE44-SRYKBN      =     "99"
              IF   SRY-SRYCD(IDX2)    =     "820100348"
                 EVALUATE   RECE41-NINPUFLG
                    WHEN        0
                            MOVE    2    TO    RECE41-NINPUFLG
                    WHEN        1
                            MOVE    3    TO    RECE41-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "11"
      *       ½é¿ÇÎÁ»»Äê²Ê¡§
              IF ( SRY-SRYCD(IDX2)    =     "830000020"          )
              OR ((SRY-SRYCD(IDX2)    =     "111011810"         ) AND
                  (RECE41-SRYYM      >=     "201204"            ))
              OR ((SRY-SRYCD(IDX2)    =     "101110040"         ) AND
                  (RECE41-SRYYM      >=     "201204"            ))
              OR ( SRY-SRYCD(IDX2)    =     "111012610"
                                     OR     "111012810"          )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SYOSANTEIKA-COMMENT
              END-IF
      *       ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
              IF  (SRY-SRYCD(IDX2)    =     "111011810"
                                      OR    "101110040"
                                      OR    "111012610"
                                      OR    "111012810"         )
              AND (FLG-SYODOU-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE44-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "830000024"
                   MOVE  SPACE  TO    RECE44-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE44-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "12"
      *       ºÆ¿ÇÎÁ»»Äê²Ê¡§
              IF ( SRY-SRYCD(IDX2)    =     "830000021"
                                      OR    "112015810"
                                      OR    "112017010"
                                      OR    "112015950"
                                      OR    "112017150"
                                      OR    "112023550"
                                      OR    "112023850"
                                      OR    "112016210"
                                      OR    "112016410"
                                      OR    "112017610"      )
              OR ((SRY-SRYCD(IDX2)    =     "101120100"     ) AND
                  (RECE41-SRYYM      <=     "201306"        ))
              OR ((SRY-SRYCD(IDX2)    =     "101120040"
                                      OR    "101120080"     ) AND
                  (RECE41-SRYYM      >=     "201307"        ))
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SAISANTEIKA-COMMENT
              END-IF
      *       ÅÅÏÃºÆ¿Ç
              IF  (SRY-SRYCD(IDX2)    =     "112009750"   OR
                                            "112009850"   OR
                                            "112009950"   OR
                                            "112010050"   OR
                                            "112012750"   OR
                                            "112704150"   OR
                                            "112704250"   OR
                                            "112704350"   OR
                                            "112706750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112007950"   OR
                                            "112008050"   OR
                                            "112008150"   OR
                                            "112008250"   OR
                                            "112012450"   OR
                                            "112702750"   OR
                                            "112702850"   OR
                                            "112702950"   OR
                                            "112706450"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"     )
              AND (RECE41-SRYYM       <     "201004"        )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DENWASAI-COMMENT
              END-IF
              IF  ((SRY-SRYCD(IDX2)   =     "112007950"    ) AND
                   (RECE41-SRYYM      >=    "201004"       ))
              OR  ((SRY-SRYCD(IDX2)   =     "101120050"    ) AND
                   (RECE41-SRYYM      >=    "201307"       ))
              OR  ((SRY-SRYCD(IDX2)   =     "112016750"    ) AND
                   (RECE41-SRYYM      >=    "201501"       ))
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DENWASAI-COMMENT
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "112023350"
                                         OR "112023650"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DENWASAI2-COMMENT
              END-IF
      *       Æ±ÆüºÆ¿Ç
              IF  (SRY-SRYCD(IDX2)    =     "112010150"   OR
                                            "112010250"   OR
                                            "112010350"   OR
                                            "112010450"   OR
                                            "112010550"   OR
                                            "112704450"   OR
                                            "112704550"   OR
                                            "112704650"   OR
                                            "112704750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"   OR
                                            "112008350"   OR
                                            "112008450"   OR
                                            "112008550"   OR
                                            "112008650"   OR
                                            "112008750"   OR
                                            "112703050"   OR
                                            "112703150"   OR
                                            "112703250"   OR
                                            "112703350"   OR
      *
                                            "112011710"   OR
                                            "112705850"     )
              AND (RECE41-SRYYM       <     "201004"        )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUJITUSAI-COMMENT
              END-IF
              IF  ((SRY-SRYCD(IDX2)   =     "112008350"   OR
                                            "112011710"   OR
                                            "112016550"    ) AND
                   (RECE41-SRYYM      >=    "201004"       ))
              OR  ((SRY-SRYCD(IDX2)   =     "101120060"    ) AND
                   (RECE41-SRYYM      >=    "201307"       ))
              OR  ((SRY-SRYCD(IDX2)   =     "112016850"    ) AND
                   (RECE41-SRYYM      >=    "201501"       ))
              OR  ((SRY-SRYCD(IDX2)   =     "112017450"    ) AND
                   (RECE41-SRYYM      >=    "201501"       ))
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUJITUSAI-COMMENT
              END-IF
      *       Æ±ÆüÅÅÏÃºÆ¿Ç
              IF  ((SRY-SRYCD(IDX2)   =     "112008850"    ) AND
                   (RECE41-SRYYM      >=    "201004"       ))
              OR  ((SRY-SRYCD(IDX2)   =     "101120070"    ) AND
                   (RECE41-SRYYM      >=    "201307"       ))
              OR  ((SRY-SRYCD(IDX2)   =     "112016950"    ) AND
                   (RECE41-SRYYM      >=    "201501"       ))
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUDENSAI-COMMENT
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "112023450"
                                         OR "112023750"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUDENSAI2-COMMENT
              END-IF
      *
      *       ºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë
              IF ( SRY-SRYCD(IDX2)    =     "112015810"
                                      OR    "112017010"
                                      OR    "112015950"
                                      OR    "112017150"
                                      OR    "112023550"
                                      OR    "112023850"
                                      OR    "112016210"
                                      OR    "112016410"
                                      OR    "112017610"      )
              OR ((SRY-SRYCD(IDX2)    =     "101120100"     ) AND
                  (RECE41-SRYYM      <=     "201306"        ))
              OR ((SRY-SRYCD(IDX2)    =     "101120040"
                                      OR    "101120080"     ) AND
                  (RECE41-SRYYM      >=     "201307"        ))
               IF   FLG-SAIDOU-COM   =     ZERO
                 MOVE   "1"        TO   RECE44-FLG-SAIDOU-COMMENT
                 MOVE   SRY-SRYKA  TO   RECE44-SAIDOU-SRYKA(1)
               END-IF
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "830000051"
                 MOVE   SPACE      TO   RECE44-FLG-SAIDOU-COMMENT
                 MOVE   SPACE      TO   RECE44-SAIDOU-SRYKA(1)
                 MOVE    1         TO   FLG-SAIDOU-COM
              END-IF
      *
      *       ¥ª¥ó¥é¥¤¥ó¿ÇÎÅÎÁ
              IF   SRY-SRYCD(IDX2)    =     "112023210"
                 MOVE   "1"        TO   RECE44-FLG-ONLINE-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100006"
                 MOVE    1         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100007"
                 MOVE    2         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100008"
                 MOVE    3         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100009"
                 MOVE    4         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100010"
                 MOVE    5         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100011"
                 MOVE    6         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100012"
                 MOVE    7         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100013"
                 MOVE    8         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100014"
                 MOVE    9         TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100015"
                 MOVE    10        TO   RECE44-FLG-ONLINE2-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "40"
      *       ²ðÃ£¸£°ú¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE41-SRYYM      >=    "200604"
              AND  RECE41-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     "140048010"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM      >=    "201004"
      *         ¿Í¹©¿ÕÂ¡Åù
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
                        MOVE   "1"    TO    RECE44-FLG-JINZOU-COMMENT
                END-IF
      *         ¶É½ê±¢°µÊÄº¿½èÃÖ
                IF    RECE41-SRYYM      >=    "201404"
                  IF   SRY-SRYCD(IDX2)  =     "140054270"
                                          OR  "140054370"
                                          OR  "140054470"
                        MOVE   "1"    TO    RECE44-FLG-KYOKUIN-COMMENT
                  END-IF
                ELSE
                 IF   RECE41-SRYYM      >=    "201204"
                  IF   SRY-SRYCD(IDX2)  =     "140052170"
                                          OR  "140052270"
                                          OR  "140052370"
                        MOVE   "1"    TO    RECE44-FLG-KYOKUIN-COMMENT
                  END-IF
                 ELSE
                  IF   SRY-SRYCD(IDX2)  =     "140052410"
                                          OR  "140051810"
                                          OR  "140051910"
                                          OR  "140052010"
                        MOVE   "1"    TO    RECE44-FLG-KYOKUIN-COMMENT
                  END-IF
                 END-IF
                END-IF
              END-IF
      *       ¼§µ¤¤Ë¤è¤ëç¯æùÅù»É·ãË¡
              IF   RECE41-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "140055110"         )
                AND (FLG-BOKOHO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-BOKOHO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000085"
                   MOVE  SPACE  TO    RECE44-FLG-BOKOHO-COMMENT
                   MOVE    1    TO    FLG-BOKOHO-COM
                END-IF
              END-IF
      *       ¹âµ¤°µ»ÀÁÇ¼£ÎÅ
              IF  (SRY-SRYCD(IDX2)    =     "140057510"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "1"   TO    RECE44-FLG-KIATU-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "140057610"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "2"   TO    RECE44-FLG-KIATU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000085"
                                      OR    "840000086"
                                      OR    "840000045"
                   MOVE  SPACE  TO    RECE44-FLG-KIATU-COMMENT
                   MOVE    1    TO    FLG-KIATU-COM
              END-IF
           END-IF
      *
           IF    ((RECE41-HKNKBN    NOT  =  "4" AND "6" ) AND
                  (RECE44-SRYKBN         =  "80"        ))
           OR    ((RECE41-HKNKBN         =  "4"  OR "6" ) AND
                  (RECE44-SRYKBN         =  "40" OR "80"))
      *       ¥ê¥Ï¥Ó¥ê
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
      *****             MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-RIHABIRI-COMMENT
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
                                            "180043430"           OR
                                            "180043530"           OR
                                            "180043630"           OR
                                            "180043730"           OR
                                            "180043830"           OR
                                            "180043930"           OR
                                            "180050330"           OR
                                            "180050430"           OR
                                            "180050530"           OR
                                            "180050730"           OR
                                            "180050930"           OR
                                            "180051130"           OR
                                            "180051230"           OR
                                            "180051330"           OR
                                            "180051430"           OR
                                            "180027810"           OR
                                            "180027910"           OR
                                            "180029510"           OR
                                            "180029610"           OR
                                            "180032710"           OR
                                            "180034510"           OR
                                            "180034610"           OR
                                            "180034710"           OR
                                            "180044030"           OR
                                            "180044130"           OR
                                            "180044230"           OR
                                            "180052730"           OR
                                            "180052830"           OR
                                            "180052930"           OR
                                            "180053130"           OR
                                            "180053330"           OR
                                            "180053530"           OR
                                            "180053630"           OR
                                            "180053730"           OR
                                            "180053830"           OR
                                            "180028210"           OR
                                            "180028310"           OR
                                            "180028410"           OR
                                            "180044610"           OR
                                            "180044710"           OR
                                            "180044810"           OR
                                            "180045010"           OR
                                            "180045210"           OR
                                            "180045410"           OR
                                            "180045530"           OR
                                            "180045630"           OR
                                            "180045730"           OR
                                            "101801110"           OR
                                            "101801120"           OR
                                            "101801130"           OR
                                            "180051530"           OR
                                            "180051630"           OR
                                            "180051730"           OR
                                            "180051930"           OR
                                            "180052130"           OR
                                            "180052330"           OR
                                            "180052430"           OR
                                            "180052530"           OR
                                            "180052630"           OR
                                            "180033110"
      *****             MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-RIHABIRI-COMMENT
              END-IF
              IF   RECE41-SRYYM      >=    "201307"
                IF   SRY-SRYCD(IDX2)  =     "101800230"            OR
                                            "101800240"            OR
                                            "101800340"            OR
                                            "101800350"            OR
                                            "101800250"            OR
                                            "101800260"            OR
                                            "101800270"            OR
                                            "101800280"            OR
                                            "101800290"            OR
                                            "101800330"            OR
                                            "101801070"            OR
                                            "101801080"
      *****             MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-RIHABIRI-COMMENT
                END-IF
              ELSE
                IF   SRY-SRYCD(IDX2)  =     "101800080"            OR
                                            "101800090"            OR
                                            "101800150"            OR
                                            "101800160"            OR
                                            "101800100"            OR
                                            "101800110"            OR
                                            "101800120"            OR
                                            "101800130"            OR
                                            "101800140"            OR
                                            "101800125"
      *****             MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-RIHABIRI-COMMENT
                END-IF
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     "180029910"
                        MOVE   "2"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     "180030010"
                        MOVE   "3"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     "180030510"
                        MOVE   "4"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     "180030610"
                        MOVE   "5"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     "180030110"
                        MOVE   "6"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     "180030210"
                        MOVE   "7"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     "180030310"
                        MOVE   "8"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     "180030410"
                        MOVE   "9"    TO    RECE44-RIHABIRI-COMMENT
              END-EVALUATE
      *       ¥ê¥Ï¥Ó¥ê¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE41-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     "180027410"
                                      OR    "180027510"
                                      OR    "180029110"
                                      OR    "180029210"
                                      OR    "180029910"
                                      OR    "180030010"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180028010"
                                      OR    "180028110"
                                      OR    "180029710"
                                      OR    "180029810"
                                      OR    "180030510"
                                      OR    "180030610"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-KOKYUKI-COMMENT
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
                                      OR    "180043430"
                                      OR    "180043530"
                                      OR    "180043630"
                                      OR    "180043730"
                                      OR    "180043830"
                                      OR    "180043930"
                                      OR    "180050330"
                                      OR    "180050430"
                                      OR    "180050530"
                                      OR    "180050730"
                                      OR    "180050930"
                                      OR    "180051130"
                                      OR    "180051230"
                                      OR    "180051330"
                                      OR    "180051430"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-NOUKE-COMMENT
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
                                      OR    "180044030"
                                      OR    "180044130"
                                      OR    "180044230"
                                      OR    "180052730"
                                      OR    "180052830"
                                      OR    "180052930"
                                      OR    "180053130"
                                      OR    "180053330"
                                      OR    "180053530"
                                      OR    "180053630"
                                      OR    "180053730"
                                      OR    "180053830"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-UNDOKI-COMMENT
                END-IF
                IF   RECE41-SRYYM      >=    "201307"
                  IF   SRY-SRYCD(IDX2)  =   "101800230"
                                      OR    "101800240"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SINDAI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800340"
                                      OR    "101800350"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-KOKYUKI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800250"
                                      OR    "101800260"
                                      OR    "101800270"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-NOUKE-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800280"
                                      OR    "101800290"
                                      OR    "101800330"
                                      OR    "101801070"
                                      OR    "101801080"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-UNDOKI-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =   "101800080"
                                      OR    "101800090"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SINDAI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800150"
                                      OR    "101800160"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-KOKYUKI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800100"
                                      OR    "101800110"
                                      OR    "101800120"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-NOUKE-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800130"
                                      OR    "101800140"
                                      OR    "101800125"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-UNDOKI-COMMENT
                  END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180017910"        )
                AND (RECE41-SRYYM    <=     "201803"           )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-NANBYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180028210"
                                      OR    "180028310"
                                      OR    "180028410"         )
                AND (RECE41-SRYYM    <=     "201803"            )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180016610"
                                      OR    "180054310"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SESSYOKU-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180044610"
                                      OR    "180044710"
                                      OR    "180044810"
                                      OR    "180045010"
                                      OR    "180045210"
                                      OR    "180045410"
                                      OR    "180045530"
                                      OR    "180045630"
                                      OR    "180045730"
                                      OR    "101801110"
                                      OR    "101801120"
                                      OR    "101801130"
                                      OR    "180051530"
                                      OR    "180051630"
                                      OR    "180051730"
                                      OR    "180051930"
                                      OR    "180052130"
                                      OR    "180052330"
                                      OR    "180052430"
                                      OR    "180052530"
                                      OR    "180052630"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-HAIYO-COMMENT
                END-IF
              END-IF
      *
              IF   (RECE41-SRYYM    >=   "201604"             )
              AND  (SRY-SRYCD(IDX2)  =   "180033870"
                                     OR  "180030770"          )
                IF   RECE44-SINDAI-COMMENT = "1"
                    MOVE   "1"    TO    RECE44-SINDAI9-COMMENT
                END-IF
                IF   RECE44-KOKYUKI-COMMENT = "1"
                    MOVE   "1"    TO    RECE44-KOKYUKI9-COMMENT
                END-IF
                IF   RECE44-HAIYO-COMMENT = "1"
                    MOVE   "1"    TO    RECE44-HAIYO9-COMMENT
                END-IF
              END-IF
      *
      *       Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡
              IF   RECE41-SRYYM      >=    "201004"
                IF  (SRY-SRYCD(IDX2)  =     "180033210"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "1"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180035910"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "2"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180047810"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "5"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180056410"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "8"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
              END-IF
      *       Àº¿À²Ê½Å¾É´µ¼ÔÁá´ü½¸Ãæ»Ù±ç´ÉÍýÎÁ
      *       Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ
              IF   RECE41-SRYYM      >=    "201604"
                IF  (SRY-SRYCD(IDX2)  =     "180057210"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "A"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180057310"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "B"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180057810"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "G"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180057910"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "H"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF   RECE41-SRYYM    >=    "202004"
                  IF  (SRY-SRYCD(IDX2)  =     "180057410"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "C"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180057510"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "D"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180057610"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "E"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180057710"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "F"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180058010"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "I"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180058110"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "J"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180064010"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "P"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180064110"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "Q"    TO    RECE44-FLG-NINRYO-COMMENT
                  END-IF
                END-IF
      *
                IF  (SRY-SRYCD(IDX2)  =     "180042110"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "K"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180042410"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "L"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180048710"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "M"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180048810"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "N"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
              END-IF
              IF     SRY-SRYCD(IDX2)  =     "840000097"
                        MOVE  SPACE   TO    RECE44-FLG-NINRYO-COMMENT
                        MOVE    1     TO    FLG-NINRYO-COM
              END-IF
      *
      *       ÄÌ±¡¡¦ºßÂðÀº¿ÀÎÅË¡¡Ê£²£°ºÐÌ¤Ëþ¡Ë²Ã»»
              IF   RECE41-SRYYM      >=    "201404"
      ***       IF   SRY-SRYCD(IDX2)  =     "180020570"
                IF    (TNS-DATAKBN             =    1 )
                AND   (TNS-CDKBN-KBN           =   "I")
                AND   (TNS-CDKBN-KBNNUM        =    2 )
                AND   (TNS-CDKBN-KBNNUM-EDA    =    0 )
                AND   (RECE41-AGE              <    20)
                  IF    (RECE41-AGE           =      19           )
                  AND   (RECE41-BIRTHDAY(5:2) =  RECE41-SRYYM(5:2))
                  AND   (RECE41-BIRTHDAY(7:2) NOT = "01"          )
                    PERFORM VARYING IDX22 FROM 1 BY 1
                             UNTIL (IDX22                      >  31)
                              OR   (RECE44-FLG-TUINZAI-COMMENT = "1")
                      IF   ACCT-DAY(1 IDX22)  NOT = ZERO
                        IF   IDX22 < RECE41-BIRTHDAY(7:2)
                          MOVE "1"    TO    RECE44-FLG-TUINZAI-COMMENT
                        END-IF
                      END-IF
                    END-PERFORM
                  ELSE
                        MOVE   "1"    TO    RECE44-FLG-TUINZAI-COMMENT
                  END-IF
                END-IF
              END-IF
              IF   RECE41-SRYYM      >=    "201604"
                IF   SRY-SRYCD(IDX2)  =     "180020570"
                                      OR    "180047270"
                                      OR    "180047370"
                        MOVE   "1"    TO    RECE44-FLG-TUINZAI2-COMMENT
                END-IF
              END-IF
      *       °ÍÂ¸¾É½¸ÃÄÎÅË¡
      *       ¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»
      *       ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡
              IF   RECE41-SRYYM      >=    "201604"
                IF  (SRY-SRYCD(IDX2)  =    "180047910")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "1"    TO    RECE44-FLG-IZONSYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =    "180056570")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "2"    TO    RECE44-FLG-IZONSYO-COMMENT
                END-IF
              END-IF
              IF   RECE41-SRYYM      >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =    "180058710")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "3"    TO    RECE44-FLG-IZONSYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =    "180058650")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "4"    TO    RECE44-FLG-IZONSYO-COMMENT
                END-IF
              END-IF
              IF   SRY-SRYCD(IDX2)  =     "840000076"
                        MOVE  SPACE   TO    RECE44-FLG-IZONSYO-COMMENT
                        MOVE    1     TO    FLG-IZONSYO-COM
              END-IF
      *
      *       Àº¿À²Ê¥ª¥ó¥é¥¤¥óºßÂð´ÉÍýÎÁ
              IF   SRY-SRYCD(IDX2)    =     "180058270"
                 MOVE   "4"        TO   RECE44-FLG-ONLINE-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     "113006510"
                   MOVE   "1"   TO    RECE44-FLG-SHIDOU-COMMENT
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "180000710"
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000011"
                                      OR    "840000012"
                                      OR    "840000511"
                                      OR    "840000512"
                   MOVE  SPACE  TO    RECE44-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "113004310"
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000078"
                                      OR    "840000578"
                   MOVE  SPACE  TO    RECE44-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "113008310" OR "113008410"
                                      OR    "113008510" OR "113031610")
              AND (FLG-NIKOCHIN-COM   =     ZERO                      )
                   MOVE   "1"   TO    RECE44-FLG-NIKOCHIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000097"
                   MOVE  SPACE  TO    RECE44-FLG-NIKOCHIN-COMMENT
                   MOVE    1    TO    FLG-NIKOCHIN-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "113008070"
                                      OR    "113008170"          )
              AND (FLG-ZENSOKU-COM    =     ZERO                 )
                   MOVE   "1"   TO    RECE44-FLG-ZENSOKU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000098"
                   MOVE  SPACE  TO    RECE44-FLG-ZENSOKU-COMMENT
                   MOVE    1    TO    FLG-ZENSOKU-COM
              END-IF
      *
              IF   RECE41-SRYYM    >=     "200804"
                IF   (SRY-SRYCD(IDX2)   =     "113000810"         )
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-SYOUNI-COMMENT
                   MOVE   "1"   TO    RECE41-SYOUNIFLG
                END-IF
                IF   (SRY-SRYCD(IDX2)   =     "113009910"
                                        OR    "113029410"         )
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "2"   TO    RECE44-FLG-SYOUNI-COMMENT
                END-IF
                IF    SRY-SRYCD(IDX2)   =     "840000104"
                                        OR    "840000604"
                   MOVE  SPACE  TO    RECE44-FLG-SYOUNI-COMMENT
                   MOVE    1    TO    FLG-SYOUNI-COM
                END-IF
              END-IF
      *
              IF  (RECE41-SRYYM      >=    "201004")
              AND (RECE41-SRYYM      <=    "202003")
                IF   RECE41-SRYYM    >=    "201604"
      *           ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»
                  IF   SRY-SRYCD(IDX2) =  "113018070"
                        MOVE   "1"    TO    RECE44-FLG-SINPESU-COMMENT
                  END-IF
                ELSE
      *           ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°¡Ë
                  IF   SRY-SRYCD(IDX2) =  "113001510"
                        MOVE   "1"    TO    RECE44-FLG-SINPESU-COMMENT
                  END-IF
                END-IF
              END-IF
              IF   RECE41-SRYYM      >=    "202004"
      *           ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡Ê¥Ú¡¼¥¹¥á¡¼¥«¡¼¡Ë
                  IF   SRY-SRYCD(IDX2) =  "113030470"
                        MOVE   "4"    TO    RECE44-FLG-SINPESU-COMMENT
                  END-IF
      *           ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡Ê¿¢¹þ·¿½üºÙÆ°´ïÅù¡Ë
                  IF   SRY-SRYCD(IDX2) =  "113030570"
                        MOVE   "5"    TO    RECE44-FLG-SINPESU-COMMENT
                  END-IF
              END-IF
              IF   RECE41-SRYYM      >=    "201004"
              AND  RECE41-SRYYM      <=    "201803"
      *         ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     "113003410"
                        MOVE   "1"    TO    RECE44-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM      >=    "201204"
                IF   SRY-SRYCD(IDX2)  =     "113014610"
                        MOVE   "1"    TO    RECE44-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113024310"
                        MOVE   "2"    TO    RECE44-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113027910"
                        MOVE   "3"    TO    RECE44-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113028310"
                        MOVE   "4"    TO    RECE44-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113008710"
                                      OR    "113008610"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-FLG-TAKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113014370"
                        MOVE   "1"    TO    RECE44-FLG-TAKYOKSN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113013910"
                                      OR    "113014010"
                                      OR    "113014110"
                        MOVE   "1"    TO    RECE44-FLG-GAIRIHA-COMMENT
                END-IF
              END-IF
      *       ¿Í¹©Æâ¼ªµ¡´ïÄ´À°²Ã»»
              IF   SRY-SRYCD(IDX2)   =  "113030670"
                   MOVE   "1"   TO    RECE44-FLG-JINNAI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)   =  "113030770"
                   MOVE   "2"   TO    RECE44-FLG-JINNAI-COMMENT
              END-IF
      *       ³°ÍèÇÓÇ¢¼«Î©»ØÆ³ÎÁ
              IF  (SRY-SRYCD(IDX2)   =  "113032810"        )
              AND (FLG-GAIJIR-COM    =  ZERO               )
                   MOVE   "1"   TO    RECE44-FLG-GAIJIR-COMMENT
                   PERFORM  VARYING  IDX40  FROM  1  BY  1
                             UNTIL   IDX40    >  31
                     IF  ACCT-DAY(1 IDX40) NOT = ZERO
                       IF  RECE41-GAIJIRYMD  =  SPACE
                         MOVE  RECE41-SRYYM  TO  RECE41-GAIJIRYMD(1:6)
                         MOVE  IDX40         TO  RECE41-GAIJIRYMD(7:2)
                       ELSE
                         MOVE  RECE41-GAIJIRYMD(7:2)  TO  IDX41
                         IF    IDX40  <  IDX41
                           MOVE  IDX40       TO  RECE41-GAIJIRYMD(7:2)
                         END-IF
                       END-IF
                       MOVE    31    TO   IDX40
                     END-IF
                   END-PERFORM
              END-IF
              IF   SRY-SRYCD(IDX2)   =  "840000097"
                                     OR "840000086"
                   MOVE  SPACE  TO    RECE44-FLG-GAIJIR-COMMENT
                   MOVE    1    TO    FLG-GAIJIR-COM
              END-IF
      *       ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡ÊÃåÍÑ·¿¼«Æ°½üºÙÆ°´ï¡Ë
      *       ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥í¡¦¿¢¹þ·¿½üºÙÆ°´ï°Ü¹Ô´ü²Ã»»´Þ¤à¡Ë
      *       ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ£²¡¦£³
              IF   RECE41-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2) =  "113015010"           )
                AND (RECE41-SRYYM   <=  "201803"              )
                   MOVE   "1"   TO    RECE44-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015130"
                   MOVE   "2"   TO    RECE44-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015310"
                   MOVE   "1"   TO    RECE44-FLG-GANSIDO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015410"
                   MOVE   "2"   TO    RECE44-FLG-GANSIDO-COMMENT
                END-IF
              END-IF
              IF  (RECE41-SRYYM    >=     "201604")
              AND (RECE41-SRYYM    <=     "202003")
                IF   SRY-SRYCD(IDX2) =  "113001610"
                   MOVE   "3"   TO    RECE44-FLG-SINPESUR-COMMENT
                END-IF
              END-IF
              IF   RECE41-SRYYM    >=     "202004"
                IF   SRY-SRYCD(IDX2) =  "113030210"
                   MOVE   "4"   TO    RECE44-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113030310"
                   MOVE   "5"   TO    RECE44-FLG-SINPESUR-COMMENT
                END-IF
              END-IF
      *       ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê½é²ó¡Ë
      *       ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë
              IF   SRY-SRYCD(IDX2)    =     "113024410"
              AND  FLG-NYUSEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-NYUSEN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "113024510"
              AND  FLG-NYUSEN-COM     =     ZERO
                   MOVE   "2"   TO    RECE44-FLG-NYUSEN-COMMENT
                   MOVE   "1"   TO    RECE41-NYUSENFLG
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000086"
                   MOVE  SPACE  TO    RECE44-FLG-NYUSEN-COMMENT
                   MOVE    1    TO    FLG-NYUSEN-COM
              END-IF
      *
      *       ¥ª¥ó¥é¥¤¥ó°å³Ø´ÉÍýÎÁ
              IF   SRY-SRYCD(IDX2)    =     "113023890"
                 MOVE   "2"        TO   RECE44-FLG-ONLINE-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     "114008010"
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000073"
                   MOVE  SPACE  TO    RECE44-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "114009410"
                   MOVE   "1"   TO    RECE44-FLG-HINKAI-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "114000110"
                                      OR    "114001610"       )
              AND (FLG-OSIN-COM       =     ZERO              )
                   MOVE   "1"   TO    RECE44-FLG-OSIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000039"
                   MOVE  SPACE  TO    RECE44-FLG-OSIN-COMMENT
                   MOVE    1    TO    FLG-OSIN-COM
              END-IF
              IF   SRY-SRYCD(IDX2)    =  "114012210" OR "114012310"
                                      OR "114012410" OR "114007510"
                                      OR "114013010" OR "114013110"
                                      OR "114013210" OR "114013310"
                                      OR "114018710" OR "114018810"
                                      OR "114018910" OR "114019010"
                                      OR "114019110" OR "114019210"
                                      OR "114019310" OR "114019410"
                                      OR "114023110" OR "114023210"
                                      OR "114023310" OR "114023410"
                                      OR "114023510" OR "114023610"
                                      OR "114023710" OR "114023810"
                                      OR "114023910" OR "114024010"
                                      OR "114024110" OR "114024210"
                                      OR "114024310" OR "114024410"
                                      OR "114024510" OR "114024610"
                                      OR "114024910" OR "114025010"
                                      OR "114025110" OR "114025210"
                                      OR "114025310" OR "114025410"
                                      OR "114025510" OR "114025610"
                                      OR "114025710" OR "114025810"
                                      OR "114025910" OR "114026010"
                                      OR "114026110" OR "114026210"
                                      OR "114026310" OR "114026410"
                                      OR "114030710" OR "114030810"
                                      OR "114030910" OR "114031010"
                                      OR "114031110" OR "114031210"
                                      OR "114031310" OR "114031410"
                                      OR "114031510" OR "114031610"
                                      OR "114031710" OR "114031810"
                                      OR "114031910" OR "114032010"
                                      OR "114032110" OR "114032210"
                                      OR "114032310" OR "114032410"
                                      OR "114032510" OR "114032610"
                                      OR "114032710" OR "114032810"
                                      OR "114032910" OR "114033010"
                                      OR "114033110" OR "114033210"
                                      OR "114033310" OR "114033410"
                                      OR "114033510" OR "114033610"
                                      OR "114033710" OR "114033810"
                                      OR "114033910" OR "114034010"
                                      OR "114034110" OR "114034210"
      *
                                      OR "114035510" OR "114035610"
                                      OR "114035710" OR "114035810"
                                      OR "114035910" OR "114036010"
                                      OR "114036110" OR "114036210"
                                      OR "114036310" OR "114036410"
                                      OR "114036510" OR "114036610"
                                      OR "114036710" OR "114036810"
                                      OR "114036910" OR "114037010"
                                      OR "114037110" OR "114037210"
                                      OR "114037310" OR "114037410"
                                      OR "114037510" OR "114037610"
                                      OR "114037710" OR "114037810"
                                      OR "114037910" OR "114038010"
                                      OR "114038110" OR "114038210"
                                      OR "114038310" OR "114038410"
                                      OR "114038510" OR "114038610"
                                      OR "114038710" OR "114038810"
                                      OR "114038910" OR "114039010"
      *
                                      OR "114001110" OR "114012910"
                                      OR "114018010" OR "114030310"
                                      OR "114042110" OR "114042210"
                                      OR "114042810" OR "114046310"
                   MOVE   "1"   TO    RECE41-COMCNTFLG
                   MOVE   "2"   TO    RECE44-FLG-OSIN-COMMENT
              END-IF
      *
              IF   (SRY-SRYCD(IDX2)   =     "114001110"
                                      OR    "114012910"
                                      OR    "114018010"
                                      OR    "114030310"
                                      OR    "114042110"
                                      OR    "114042210"
                                      OR    "114042810"
                                      OR    "114046310"         )
              AND  (FLG-ZAIHOU-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-ZAIHOU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000066"
                   MOVE  SPACE  TO    RECE44-FLG-ZAIHOU-COMMENT
                   MOVE    1    TO    FLG-ZAIHOU-COM
              END-IF
      *
              IF   RECE41-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114000110"
                                    OR  "114001610"
                   IF    (WRK-SISETU3168  =  1)
                   OR    (WRK-SISETU3055  =  1)
                   OR    (WRK-SISETU3056  =  1)
                       MOVE   "1"   TO    RECE41-COMCNTFLG
                       MOVE   "1"   TO    RECE44-FLG-ZAISIEN-COMMENT
                   END-IF
                   IF  RECE44-FLG-ZAISIEN-COMMENT = SPACE
                     IF  (WRK-SISETU3169  =  1)
                     OR  (WRK-SISETU3057  =  1)
                     OR  (WRK-SISETU3058  =  1)
                       MOVE   "1"   TO    RECE41-COMCNTFLG
                       MOVE   "2"   TO    RECE44-FLG-ZAISIEN-COMMENT
                     END-IF
                   END-IF
                END-IF
              ELSE
                IF   WRK-SISETU613   =  1
                  IF   RECE41-SRYYM   >=  "200604"
                  AND  RECE41-SRYYM   <=  "200803"
                      IF SRY-SRYCD(IDX2) =  "114001110"         OR
                                            "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE41-COMCNTFLG
                        MOVE   "1"   TO    RECE44-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                  IF   RECE41-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE41-COMCNTFLG
                        MOVE   "1"   TO    RECE44-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                ELSE
                  IF   WRK-SISETU695   =  1
                    IF   RECE41-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE41-COMCNTFLG
                        MOVE   "2"   TO    RECE44-FLG-ZAISIEN-COMMENT
                      END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114020910"
                   MOVE   "1"   TO    RECE44-FLG-KAISYOKU-COMMENT
                END-IF
      ***       IF   SRY-SRYCD(IDX2) =  "114009710"
      ***          MOVE   "1"   TO    RECE44-FLG-YOUATU-COMMENT
      ***       END-IF
              END-IF
      *       ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ
              IF   RECE41-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "114027610"
                                        OR    "114027710"
                                        OR    "114027810"
                                        OR    "114027910"         )
                AND (FLG-ZAIKYO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-ZAIKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000097"
                                        OR    "840000086"
                   MOVE  SPACE  TO    RECE44-FLG-ZAIKYO-COMMENT
                   MOVE    1    TO    FLG-ZAIKYO-COM
                END-IF
              END-IF
      *       ºßÂð´µ¼ÔË¬Ìä¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó»ØÆ³´ÉÍýÎÁ
              IF   RECE41-SRYYM    >=     "201611"
                IF  (SRY-SRYCD(IDX2)    =     "114006410"
                                        OR    "114015010"          )
                AND (FLG-ZAIRIHA-COM    =     ZERO                 )
                   MOVE   "1"   TO    RECE44-FLG-ZAIRIHA-COMMENT
                END-IF
              END-IF
      *       ºßÂð´µ¼ÔË¬ÌäÌôºÞ´ÉÍý»ØÆ³ÎÁ
              IF   SRY-SRYCD(IDX2)    =     "114044710"
                                      OR    "114044810"
                                      OR    "114044910"
                   MOVE   "1"   TO    RECE44-FLG-ZAIYAK-COMMENT
              END-IF
      *       ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡ÊºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë¡Ë
      *       ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡ÊºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²¡Ë
              IF   SRY-SRYCD(IDX2)    =     "114045470"
                   MOVE   "A"   TO    RECE44-FLG-SINPESU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114045670"
                   MOVE   "B"   TO    RECE44-FLG-SINPESU-COMMENT
              END-IF
      *       ºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë
      *       ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²
              IF   SRY-SRYCD(IDX2)    =     "114003710"
                   MOVE   "A"   TO    RECE44-FLG-SINPESUR-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114040810"
                   MOVE   "B"   TO    RECE44-FLG-SINPESUR-COMMENT
              END-IF
      *       ºßÂðÈ¾¸Ç·Á±ÉÍÜ·Ð´É±ÉÍÜË¡»ØÆ³´ÉÍýÎÁ
              IF  (SRY-SRYCD(IDX2)    =     "114045510"          )
              AND (FLG-NINRYO-COM     =     ZERO                 )
                   MOVE   "9"   TO    RECE44-FLG-NINRYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000097"
                   MOVE  SPACE  TO    RECE44-FLG-NINRYO-COMMENT
                   MOVE    1    TO    FLG-NINRYO-COM
              END-IF
      *
      *       ¥ª¥ó¥é¥¤¥óºßÂð´ÉÍýÎÁ¡Êºß°åÁí´É¡Ê·î£±²óË¬Ìä¿ÇÎÅ¡Ë¡Ë
              IF   SRY-SRYCD(IDX2)    =     "114043970"
                 MOVE   "3"        TO   RECE44-FLG-ONLINE-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "33"
              IF   SRY-SRYSYUKBN      =     "334"
              AND  FLG-HOUTEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-HOUTEN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "50"
              IF   RECE44-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000042"
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE41-SRYYM      >=    "201004"
            IF     RECE44-SRYKBN      =     "54"
              IF   RECE44-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "70"
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170011910" OR "170012110"
                                      OR "170015550" OR "170015210"
                                      OR "170020110" OR "170022110"
                                      OR "170022290" OR "170022390"
                                      OR "170022490" OR "170023010"
                                      OR "170023110" OR "170023210"
                                      OR "170023310" OR "170023410"
                                      OR "170023510"               )
              AND (RECE41-SRYYM       <     "201004"               )
                   MOVE   "1"   TO    RECE44-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170012110" OR "170015210"
                                      OR "170020110" OR "170028610"
                                      OR "170033410" OR "170033510"
                                      OR "170034910" OR "170035010")
              AND (RECE41-SRYYM       >=    "201004"               )
              AND (RECE41-SRYYM       <=    "202003"               )
                   MOVE   "1"   TO    RECE44-FLG-CTMRI-COMMENT
              END-IF
      *
              IF    SRY-SRYCD(IDX2)  =
                    "700010000" OR "700030000" OR "700050000" OR
                    "700080000" OR "700110000" OR "700140000" OR
                    "700290000" OR "700310000" OR "700330000" OR
                    "700710000" OR "700730000" OR "700750000" OR
                    "700780000" OR "700810000" OR "700840000" OR
                    "700870000" OR "700160000" OR "700180000" OR
                    "700190000" OR "700200000" OR "700210000" OR
                    "700220000" OR "700230000" OR "700240000" OR
                    "700250000" OR "700260000" OR "700270000" OR
                    "700280000" OR "700590000" OR "700600000" OR
                    "700630000" OR "700640000" OR "700650000" OR
                    "700680000" OR "729780000"
                IF    SRY-SRYKAISU(IDX2)  >   1
                   MOVE   "1"   TO    RECE44-FLG-FILM-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     "160162350"
                                      OR    "160181250"
                                      OR    "160116310"        )
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE44-FLG-KENSA-COMMENT
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160162350"
                   MOVE    1    TO    RECE41-KENSA-BNPFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160181250"
                   MOVE    1    TO    RECE41-KENSA-NTPROFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160116310"
                   MOVE    1    TO    RECE41-KENSA-ANPFLG
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN   =  "14" OR "40" OR "50" OR "54"
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
                   MOVE   "1"   TO    RECE44-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN   =  "40" OR "54" OR "60" OR "80"
              IF      SRY-MEISKYFLG(IDX2)   =   "R"
                   MOVE   "1"   TO    RECE44-FLG-GAIKAN-COMMENT
              END-IF
           END-IF
      *
           .
       2101211-RECE45-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       2101212-RECE45-TANKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    ¿ô»úÉ½¼¨ÍÑÊÔ½¸¡ÊÈ¾³Ñ¡Ë
           MOVE    RECE45-TEN(IDX4)
                                   TO  WRK-TANKA-9
           MOVE    WRK-TANKA-S1    TO  WRK-TANKA-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY   FROM    2   BY  -1
                     UNTIL     IDY           =   ZERO
             IF  WRK-TANKA-S2(IDY:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-TANKA-S2(IDY:1)
                                         TO  WRK-TANKA-Z3(IDY:1)
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
                           MOVE    "¡¥"   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
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
                   MOVE    "±ß"        TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   ADD     2           TO  WRK-CHAR-N-CNT
           END-IF                             
      *
           .
       2101212-RECE45-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       2101213-JIBAI-KIZAI-TENHEN-SEC     SECTION.
      *
           MOVE     ZERO                   TO    WRK-TEN-KZI
           MOVE     ZERO                   TO    FLG-TAIGAI
      *
           IF   RECE41-TAXKBN-KZI   =   "2"
      *
      *       ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£³¡Ë¤Ë¤Æ¾ÃÈñÀÇÂÐ¾Ý³°¥³¡¼¥É¤«È½ÊÌ
              INITIALIZE                      SYS-4003-REC
              MOVE    "4003"              TO  SYS-4003-KANRICD
              MOVE    "*"                 TO  SYS-4003-KBNCD
              MOVE    RECE41-SRYYM        TO  SYS-4003-STYUKYMD(1:6)
              MOVE    "01"                TO  SYS-4003-STYUKYMD(7:2)
              MOVE    SYS-4003-STYUKYMD   TO  SYS-4003-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-4003-HOSPNUM
              MOVE    SYS-4003-REC        TO  MCPDATA-REC
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO  SYS-4003-REC
                  PERFORM   VARYING   IDX5    FROM   1   BY   1
                             UNTIL   (IDX5                 >    50 )
                               OR    (SYS-4003-SRYCD(IDX5) =  SPACE)
                               OR    (FLG-TAIGAI           =     1 )
                      IF   SYS-4003-SRYCD(IDX5)  =  SRY-SRYCD(IDX2)
                          MOVE     1       TO    FLG-TAIGAI
                      END-IF
                  END-PERFORM
              END-IF
              IF      TNS-NAMECHGKBN  =  ZERO OR 5
                  CONTINUE
              ELSE
                  MOVE     1       TO    FLG-TAIGAI
              END-IF
      *
      *       ÆÃÄê´ïºàÅù¾ÃÈñÀÇ·×»»
              IF   FLG-TAIGAI  =  ZERO
                   IF  SRY-SRYSYUKBN = "809"
                     COMPUTE  WRK-TEN-KZI  =
                             (SRY-JIHIMONEY(IDX2)
                                       * WRK-S5) + WRK-S3
                   ELSE
                     COMPUTE  WRK-TEN-KZI  =
                             (TNS-TEN  * WRK-S5) + WRK-S3
                   END-IF
              ELSE
                   IF  SRY-SRYSYUKBN = "809"
                      MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-KZI
                   ELSE
                      MOVE  TNS-TEN              TO   WRK-TEN-KZI
                   END-IF
              END-IF
           ELSE
              IF  SRY-SRYSYUKBN = "809"
                      MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-KZI
              ELSE
                      MOVE  TNS-TEN              TO   WRK-TEN-KZI
              END-IF
           END-IF
      *
           .
       2101213-JIBAI-KIZAI-TENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕÊ¸½ñÎÁÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       2101214-JIBAI-BUNSYO-TENHEN-SEC     SECTION.
      *
           MOVE        ZERO          TO    WRK-TEN-BUN
      *
           IF   RECE41-TAXKBN   =   "2"
               IF  SRY-SRYSYUKBN = "809"
                  COMPUTE  WRK-TEN-BUN  =
                          (SRY-JIHIMONEY(IDX2)
                                    * WRK-S5) + WRK-S3
               ELSE
                  COMPUTE  WRK-TEN-BUN  =
                          (TNS-TEN  * WRK-S5) + WRK-S3
               END-IF
           ELSE
               IF  SRY-SRYSYUKBN = "809"
                  MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-BUN
               ELSE
                  MOVE  TNS-TEN              TO   WRK-TEN-BUN
               END-IF
           END-IF
      *
           .
       2101214-JIBAI-BUNSYO-TENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÄÎÌÇÑ´þ¤Î¿ôÎÌÊÔ½¸½èÍý
      *****************************************************************
       2101215-RECE45-SURYO-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      *
      *    ¿ô»úÉ½¼¨ÍÑÊÔ½¸¡ÊÈ¾³Ñ¡Ë
      ***  MOVE    RECE45-SRYSURYO(IDX4)  TO  WRK-SURYO
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
      *    É½¼¨Ê¸»ú¿ô¥«¥¦¥ó¥È
           PERFORM   VARYING   IDW   FROM    1   BY  1
                      UNTIL    IDW     >    11
             IF  WRK-SURYO-X(IDW:1)  NOT =  SPACE
                 IF  WRK-SURYOST           =   ZERO
                     MOVE    IDW   TO  WRK-SURYOST
                 END-IF
      *          ¿ôÎÌ·å¿ô¥«¥¦¥ó¥È(£¶·åÌÜ¡§¥Ô¥ê¥ª¥É)
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
      *    Á´³ÑÊÑ´¹
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
               MOVE    "¡¥"            TO  WRK-SURYO-N2
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
       2101215-RECE45-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2101216-SETCOM-HENSYU-SEC     SECTION.
      *
           IF   RECE45-MEISKYFLG(IDX4)  =  "2"
              IF   (FLG-SETCOM              =  1   )
              AND  (RECE45-RECEDENFLG(IDX4) = SPACE)
                 MOVE  "1"            TO  RECE44-SETCOMFLG
              ELSE
                 MOVE  SPACE          TO  RECE45-MEISKYFLG(IDX4)
              END-IF
           ELSE
              IF   (RECE45-SRYCD(IDX4)(1:1) = "1" OR "6" OR "7")
              OR   (RECE45-SRYCD(IDX4)(1:3) = "059"            )
                IF  RECE45-SRYCD(IDX4)(1:2) = "63"
                 CONTINUE
                ELSE
                 MOVE   1             TO  FLG-SETCOM
                END-IF
              END-IF
           END-IF
      *
      *****
      *
      *    ÆÃÄêÌôºÞ¼£ÎÅ´ÉÍýÎÁ
           IF    RECE45-SRYCD(IDX4)   = "113000410" OR "113000510" OR
                                        "180000110" OR "180000210" OR
                                        "113023910"
                 MOVE   1             TO  WRK-SETCOM-JIDO
           END-IF
      *    °­À­¼ðáçÆÃ°ÛÊª¼Á¼£ÎÅ´ÉÍýÎÁ
           IF    RECE45-SRYCD(IDX4)   = "113001210" OR "113001310" OR
                                        "113002110"
                 MOVE   3             TO  WRK-SETCOM-JIDO
           END-IF
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           IF    RECE45-SRYCD(IDX4)   = "111011810"
                                     OR "111012610"
                                     OR "111012810"
                 MOVE   4             TO  WRK-SETCOM-JIDO
           END-IF
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ
           IF    RECE45-SRYCD(IDX4)   = "180000710"
                 MOVE   6             TO  WRK-SETCOM-JIDO
           END-IF
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ
           IF    RECE45-SRYCD(IDX4)   = "113004310"
                 MOVE   7             TO  WRK-SETCOM-JIDO
           END-IF
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê½é²ó¡Ë
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê£µ²óÌÜ¡Ë
           IF    RECE45-SRYCD(IDX4)   = "113008310" OR "113008410"
                                     OR "113008510" OR "113031610"
                 MOVE   8             TO  WRK-SETCOM-JIDO
           END-IF
      *    ½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¡Ê£±·îÌÜ¡Ë
      *    ½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¡Ê£²·îÌÜ°Ê¹ß£¶·îÌÜ¤Þ¤Ç¡Ë
           IF    RECE45-SRYCD(IDX4)   = "113008070"
                                     OR "113008170"
                 MOVE   9             TO  WRK-SETCOM-JIDO
           END-IF
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê£±²óÌÜ¡Ë
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê£²²óÌÜ¡Ë
           IF    RECE45-SRYCD(IDX4)   = "113000810"
                                     OR "113009910" OR "113029410"
                 MOVE   10            TO  WRK-SETCOM-JIDO
           END-IF
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ
           IF    RECE45-SRYCD(IDX4)   = "114008010"
                 MOVE   11            TO  WRK-SETCOM-JIDO
           END-IF
      *    ±ý¿Ç
      *    ÆÃÊÌ±ý¿Ç
           IF    RECE45-SRYCD(IDX4)   = "114000110"
                                     OR "114001610"
                 MOVE   13            TO  WRK-SETCOM-JIDO
           END-IF
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Êµï½»·Ï¡Ë
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë¡ÊÆÃÄê»ÜÀßÅùÆþµï¼Ô¡Ë
           IF    RECE45-SRYCD(IDX4)   = "114001110"
                                     OR "114012910"
                                     OR "114018010"
                                     OR "114030310"
                                     OR "114042110"
                                     OR "114042210"
                                     OR "114042810"
                                     OR "114046310"
                 MOVE   14            TO  WRK-SETCOM-JIDO
           END-IF
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡
      *    Àº¿À²Ê½Å¾É´µ¼ÔÁá´ü½¸Ãæ»Ù±ç´ÉÍýÎÁ
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ
      *    ºßÂðÈ¾¸Ç·Á±ÉÍÜ·Ð´É±ÉÍÜË¡»ØÆ³´ÉÍýÎÁ
           IF    RECE45-SRYCD(IDX4)   = "180033210"
                                     OR "180035910"
                                     OR "180047810"
                                     OR "180056410"
                                     OR "180042110"
                                     OR "180042410"
                                     OR "180048710"
                                     OR "180048810"
                                     OR "180057210"
                                     OR "180057310"
                                     OR "180057810"
                                     OR "180057910"
                                     OR "114045510"
                 MOVE   19            TO  WRK-SETCOM-JIDO
           END-IF
           IF   (RECE45-SRYCD(IDX4)   = "180057410" OR "180057510"
                                     OR "180057610" OR "180057710"
                                     OR "180058010" OR "180058110"
                                     OR "180064010" OR "180064110")
           AND  (RECE41-SRYYM        >= "202004"                  )
                 MOVE   19            TO  WRK-SETCOM-JIDO
           END-IF
      *    ºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë
           IF    RECE45-SRYCD(IDX4)   = "112015810"
                                     OR "112017010"
                                     OR "112015950"
                                     OR "112017150"
                                     OR "112023550"
                                     OR "112023850"
                                     OR "112016210"
                                     OR "112016410"
                                     OR "112017610"
                 MOVE   25            TO  WRK-SETCOM-JIDO
           END-IF
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ
           IF    RECE45-SRYCD(IDX4)   = "114027610"
                                     OR "114027710"
                                     OR "114027810"
                                     OR "114027910"
                 MOVE   29            TO  WRK-SETCOM-JIDO
           END-IF
      *    °ÍÂ¸¾É½¸ÃÄÎÅË¡
           IF    RECE45-SRYCD(IDX4)   = "180047910"
                                     OR "180058710"
                 MOVE   33            TO  WRK-SETCOM-JIDO
           END-IF
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ
           IF    RECE45-SRYCD(IDX4)   = "140057510"
                                     OR "140057610"
                 MOVE   35            TO  WRK-SETCOM-JIDO
           END-IF
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë
           IF    RECE45-SRYCD(IDX4)   = "113024510"
                                     OR "113024410"
                 MOVE   37            TO  WRK-SETCOM-JIDO
           END-IF
      *    ¥ª¥ó¥é¥¤¥ó¿ÇÎÅÎÁ
           IF    RECE45-SRYCD(IDX4)   = "112023210"
                 MOVE   38            TO  WRK-SETCOM-JIDO
           END-IF
      *    »þ´Ö³°¶ÛµÞ±¡Æâ¸¡ºº²Ã»»
           IF   (RECE45-SRYCD(IDX4)   = "160000210")
           AND  (RECE41-SRYYM        >= "201810"   )
                 MOVE   43            TO  WRK-SETCOM-JIDO
           END-IF
      *    »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»
           IF   (RECE45-SRYCD(IDX4)   = "170016010")
           AND  (RECE41-SRYYM        >= "201810"   )
                 MOVE   44            TO  WRK-SETCOM-JIDO
           END-IF
      *    ³°ÍèÇÓÇ¢¼«Î©»ØÆ³ÎÁ
           IF    RECE45-SRYCD(IDX4)   = "113032810"
                 MOVE   46            TO  WRK-SETCOM-JIDO
           END-IF
      *    ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡
           IF   (RECE45-SRYCD(IDX4)   = "180058650")
           AND  (RECE41-SRYYM        >= "202004"   )
                 MOVE   47            TO  WRK-SETCOM-JIDO
           END-IF
      *
      *    ÆÃÄêÌôºÞ¼£ÎÅ´ÉÍý²Ã»»¡ÊÂ¡´ï°Ü¿¢·î¤«¤é£³·î¡Ë
           IF    RECE45-SRYCD(IDX4)   = "113000670" OR "113000770"
                                     OR "113029170" OR "113029270"
                                     OR "113029370"
                 MOVE   1             TO  WRK-SETCOM-JIDO2
           END-IF
      *    ¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»
           IF    RECE45-SRYCD(IDX4)   = "180056570"
                 MOVE   2             TO  WRK-SETCOM-JIDO2
           END-IF
      *
      *    <Ï«ºÒÍÑ>
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           IF    RECE45-SRYCD(IDX4)   = "101110040"
                 MOVE   204           TO  WRK-SETCOM-JIDO
           END-IF
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           IF  ((RECE45-SRYCD(IDX4)   = "101120100"  ) AND
                (RECE41-SRYYM        <= "201306"     ))
           OR  ((RECE45-SRYCD(IDX4)   = "101120040"
                                     OR "101120080"  ) AND
                (RECE41-SRYYM        >= "201307"     ))
                 MOVE   225           TO  WRK-SETCOM-JIDO
           END-IF
      *
           IF  ((WRK-SETCOM-JIDO    =  1                        ) AND
                (RECE45-SRYCD(IDX4) = "830000014" OR "840000038"
                                                  OR "840000538"))
           OR  ((WRK-SETCOM-JIDO    =  3                        ) AND
                (RECE45-SRYCD(IDX4) = "830000015"               ))
           OR  ((WRK-SETCOM-JIDO    =  4                        ) AND
                (RECE45-SRYCD(IDX4) = "830000024"               ))
           OR  ((WRK-SETCOM-JIDO    =  6                        ) AND
                (RECE45-SRYCD(IDX4) = "840000011"
                                   OR "840000012"
                                   OR "840000511"
                                   OR "840000512"               ))
           OR  ((WRK-SETCOM-JIDO    =  7                        ) AND
                (RECE45-SRYCD(IDX4) = "840000078"
                                   OR "840000578"               ))
           OR  ((WRK-SETCOM-JIDO    =  8                        ) AND
                (RECE45-SRYCD(IDX4) = "840000097"               ))
           OR  ((WRK-SETCOM-JIDO    =  9                        ) AND
                (RECE45-SRYCD(IDX4) = "840000098"               ))
           OR  ((WRK-SETCOM-JIDO    =  10                       ) AND
                (RECE45-SRYCD(IDX4) = "840000104"
                                   OR "840000604"               ))
           OR  ((WRK-SETCOM-JIDO    =  11                       ) AND
                (RECE45-SRYCD(IDX4) = "840000073"               ))
           OR  ((WRK-SETCOM-JIDO    =  13                       ) AND
                (RECE45-SRYCD(IDX4) = "840000039"               ))
           OR  ((WRK-SETCOM-JIDO    =  14                       ) AND
                (RECE45-SRYCD(IDX4) = "840000066"               ))
           OR  ((WRK-SETCOM-JIDO    =  19                       ) AND
                (RECE45-SRYCD(IDX4) = "840000097"               ))
           OR  ((WRK-SETCOM-JIDO    =  25                       ) AND
                (RECE45-SRYCD(IDX4) = "830000051"               ))
           OR  ((WRK-SETCOM-JIDO    =  29                       ) AND
                (RECE45-SRYCD(IDX4) = "840000097"
                                   OR "840000086"               ))
           OR  ((WRK-SETCOM-JIDO    =  33                       ) AND
                (RECE45-SRYCD(IDX4) = "840000076"               ))
           OR  ((WRK-SETCOM-JIDO    =  35                       ) AND
                (RECE45-SRYCD(IDX4) = "840000085"
                                   OR "840000086"
                                   OR "840000045"               ))
           OR  ((WRK-SETCOM-JIDO    =  37                       ) AND
                (RECE45-SRYCD(IDX4) = "840000086"               ))
           OR  ((WRK-SETCOM-JIDO    =  38                       ) AND
                (RECE45-SRYCD(IDX4) = "820100006"
                                   OR "820100007"
                                   OR "820100008"
                                   OR "820100009"
                                   OR "820100010"
                                   OR "820100011"
                                   OR "820100012"
                                   OR "820100013"
                                   OR "820100014"
                                   OR "820100015"               ))
           OR  ((WRK-SETCOM-JIDO    =  43                       ) AND
                (RECE45-SRYCD(IDX4) = "840000044"               ))
           OR  ((WRK-SETCOM-JIDO    =  44                       ) AND
                (RECE45-SRYCD(IDX4) = "840000071"               ))
           OR  ((WRK-SETCOM-JIDO    =  46                       ) AND
                (RECE45-SRYCD(IDX4) = "840000097"
                                   OR "840000086"               ))
           OR  ((WRK-SETCOM-JIDO    =  47                       ) AND
                (RECE45-SRYCD(IDX4) = "840000076"               ))
      *
           OR  ((WRK-SETCOM-JIDO2   =  1                        ) AND
                (RECE45-SRYCD(IDX4) = "840000077"               ))
           OR  ((WRK-SETCOM-JIDO2   =  2                        ) AND
                (RECE45-SRYCD(IDX4) = "840000076"               ))
      *
      *    <Ï«ºÒÍÑ>
           OR  ((WRK-SETCOM-JIDO    =  204                      ) AND
                (RECE45-SRYCD(IDX4) = "830000024"               ))
           OR  ((WRK-SETCOM-JIDO    =  225                      ) AND
                (RECE45-SRYCD(IDX4) = "830000051"               ))
      *
                 MOVE  "1"              TO  RECE44-SETCOMFLG
                 MOVE  "2"              TO  RECE45-MEISKYFLG(IDX4)
                 MOVE  WRK-SETCOM-JIDO  TO  RECE45-SETCOMATFLG(IDX4)
                 IF  (WRK-SETCOM-JIDO2   =  1         )
                 AND (RECE45-SRYCD(IDX4) = "840000077")
                   MOVE    2            TO  RECE45-SETCOMATFLG(IDX4)
                 END-IF
                 IF  (WRK-SETCOM-JIDO2   =  2                   )
                 AND (RECE45-SRYCD(IDX4) = "840000076"          )
                   MOVE    40           TO  RECE45-SETCOMATFLG(IDX4)
                 END-IF
      *
           END-IF
      *
           .
       2101216-SETCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ú¥ì¥»¥×¥È¡Û¡ÊÂè»°¼Ô¹Ô°Ù¡Ë°ìÉôÉéÃ´¶â¶â³ÛÃÖ´¹¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2101217-ICHIBUCOM-HENSYU-SEC     SECTION.
      *
           MOVE    SPACE                  TO  WRK-INPUTCOMENT
           MOVE    SPACE                  TO  WRK-INPUTCOMENT2
           MOVE    ZERO                   TO  WRK-INPUTCOMENT-KIN
           MOVE    SPACE                  TO  WRK-INPUTCOMENT-KIN2
      *
           IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                  (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
           AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
      *
             MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
             MOVE    SRY-PTID             TO  PTCOM-PTID
             MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
             MOVE    SRY-SRYCD    (IDX2)  TO  PTCOM-SRYCD
             MOVE    SRY-INPUTNUM (IDX2)  TO  PTCOM-RENNUM
             PERFORM 800-PTCOM-READ-SEC
             MOVE    PTCOM-INPUTCOMENT    TO  WRK-INPUTCOMENT
      *
             IF  WRK-INPUTCOMENT(1:12)  =  "°ìÉôÉéÃ´³Û¡§"
               MOVE  WRK-INPUTCOMENT(13:14)
                                          TO  WRK-INPUTCOMENT2
               IF  WRK-INPUTCOMENT2 = "¡¡¡¡¡¡¡¡¡¡¡¡¡¡" OR SPACE
                 CONTINUE
               ELSE
                 PERFORM VARYING IDW7 FROM  1  BY  2
                          UNTIL  IDW7   >  14
                   COMPUTE  IDW8 = (IDW7 + 1) / 2
                   EVALUATE  WRK-INPUTCOMENT2(IDW7:2)
                     WHEN     "£°"
                        MOVE   "0"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£±"
                        MOVE   "1"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£²"
                        MOVE   "2"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£³"
                        MOVE   "3"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£´"
                        MOVE   "4"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£µ"
                        MOVE   "5"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£¶"
                        MOVE   "6"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£·"
                        MOVE   "7"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£¸"
                        MOVE   "8"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£¹"
                        MOVE   "9"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     OTHER
                        MOVE    14  TO  IDW7
                   END-EVALUATE
                 END-PERFORM
                 IF  WRK-INPUTCOMENT-KIN2-T NOT = SPACE
                   MOVE    8    TO   IDW8
                   PERFORM VARYING IDW7 FROM  7  BY -1
                            UNTIL  IDW7   <   1
                     IF  WRK-INPUTCOMENT-KIN2-T(IDW7:1) = SPACE
                        CONTINUE
                     ELSE
                        COMPUTE  IDW8 = IDW8 - 1
                        MOVE  WRK-INPUTCOMENT-KIN2-T(IDW7:1)
                                TO   WRK-INPUTCOMENT-KIN-T(IDW8:1)
                     END-IF
                   END-PERFORM
                   MOVE    1    TO   RECE41-DAISAN-CHIKAN
                   MOVE  WRK-INPUTCOMENT-KIN
                                TO   RECE41-FTNMONEY
                 END-IF
               END-IF
             END-IF
      *
           END-IF
      *
           .
       2101217-ICHIBUCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ú¥ì¥»¥×¥È¡Û¡Ê¼«ÇåÀÕ¡Ë¼õÎÎ¶â³ÛÍóÃÖ´¹¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2101218-ICHIBUCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    SPACE                  TO  WRK-INPUTCOMENT
           MOVE    SPACE                  TO  WRK-INPUTCOMENT2
           MOVE    SPACE                  TO  WRK-INPUTCOMENT3
           MOVE    ZERO                   TO  WRK-INPUTCOMENT-KIN
           MOVE    SPACE                  TO  WRK-INPUTCOMENT-KIN2
      *
           IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                  (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
           AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
      *
             MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
             MOVE    SRY-PTID             TO  PTCOM-PTID
             MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
             MOVE    SRY-SRYCD    (IDX2)  TO  PTCOM-SRYCD
             MOVE    SRY-INPUTNUM (IDX2)  TO  PTCOM-RENNUM
             PERFORM 800-PTCOM-READ-SEC
             MOVE    PTCOM-INPUTCOMENT    TO  WRK-INPUTCOMENT
      *
             IF  WRK-INPUTCOMENT(1:10)  =  "¼õÎÎ¶â³Û¡§"
               MOVE  WRK-INPUTCOMENT(11:14)
                                          TO  WRK-INPUTCOMENT2
               IF  WRK-INPUTCOMENT2 = "¡¡¡¡¡¡¡¡¡¡¡¡¡¡" OR SPACE
                 CONTINUE
               ELSE
                 PERFORM VARYING IDW7 FROM  1  BY  2
                          UNTIL  IDW7   >  14
                   COMPUTE  IDW8 = (IDW7 + 1) / 2
                   EVALUATE  WRK-INPUTCOMENT2(IDW7:2)
                     WHEN     "£°"
                        MOVE   "0"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£±"
                        MOVE   "1"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£²"
                        MOVE   "2"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£³"
                        MOVE   "3"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£´"
                        MOVE   "4"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£µ"
                        MOVE   "5"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£¶"
                        MOVE   "6"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£·"
                        MOVE   "7"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£¸"
                        MOVE   "8"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     "£¹"
                        MOVE   "9"  TO  WRK-INPUTCOMENT-KIN2-T(IDW8:1)
                     WHEN     OTHER
                        MOVE    14  TO  IDW7
                   END-EVALUATE
                 END-PERFORM
                 IF  WRK-INPUTCOMENT-KIN2-T NOT = SPACE
                   MOVE    8    TO   IDW8
                   PERFORM VARYING IDW7 FROM  7  BY -1
                            UNTIL  IDW7   <   1
                     IF  WRK-INPUTCOMENT-KIN2-T(IDW7:1) = SPACE
                        CONTINUE
                     ELSE
                        COMPUTE  IDW8 = IDW8 - 1
                        MOVE  WRK-INPUTCOMENT-KIN2-T(IDW7:1)
                                TO   WRK-INPUTCOMENT-KIN-T(IDW8:1)
                     END-IF
                   END-PERFORM
                   MOVE    1    TO   RECE41-JIBAI-CHIKANKINFLG
                   MOVE  WRK-INPUTCOMENT-KIN
                                TO   RECE41-JIBAI-CHIKANKIN
                 END-IF
               END-IF
             END-IF
      *
             IF  WRK-INPUTCOMENT(1:8)  =  "»ÙÊ§¸µ¡§"
               MOVE  WRK-INPUTCOMENT(9:30)
                                          TO  WRK-INPUTCOMENT3
               IF  WRK-INPUTCOMENT3 =
                   "¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡" OR SPACE
                 CONTINUE
               ELSE
                 MOVE    1      TO   RECE41-JIBAI-CHIKANNAMEFLG
                 MOVE  WRK-INPUTCOMENT3
                                TO   RECE41-JIBAI-CHIKANNAME
               END-IF
             END-IF
      *
           END-IF
      *
           .
       2101218-ICHIBUCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌÀºÙ¿ôÅù¤ÎÊÔ½¸
      *****************************************************************
       2101219-MEISAISU-HENSYU-SEC                 SECTION.
      *
           COMPUTE   RECE44-MEISAISU    =  RECE44-MEISAISU
                                        -  1
           MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
           COMPUTE   RECE44-SRYCDTOTAL  =  RECE44-SRYCDTOTAL
                                        -  WRK-SRYCD
           COMPUTE   RECE44-SURYOUTOTAL =  RECE44-SURYOUTOTAL
                                        -  SRY-SRYSURYO(IDX2)
      *
           .
       2101219-MEISAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï³«»ÏÆüÊÔ½¸
      *****************************************************************
       210121A-RIHAYMD-HENSYU-SEC                 SECTION.
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
             PERFORM 800-PTCOM-READ-SEC
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
               MOVE       1     TO  RECE41-RIHA2-SINDAI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-SINYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE41-RIHA2-SINYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-SINYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-SINYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         2
               MOVE       1     TO  RECE41-RIHA2-NOUKE
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-NOUYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE41-RIHA2-NOUYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-NOUYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-NOUYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         3
               MOVE       1     TO  RECE41-RIHA2-UNDOKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-UNDYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE41-RIHA2-UNDYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-UNDYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-UNDYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         4
               MOVE       1     TO  RECE41-RIHA2-KOKYUKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-KOKYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE41-RIHA2-KOKYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-KOKYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-KOKYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         5
               MOVE       1     TO  RECE41-RIHA2-SESSYOKU
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-SESYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE41-RIHA2-SESYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-SESYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-SESYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         6
               MOVE       1     TO  RECE41-RIHA2-HAIYO
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-HAIYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE41-RIHA2-HAIYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-HAIYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-HAIYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         7
               MOVE       1     TO  RECE41-RIHA2-SINDAI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-SIN9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE41-RIHA2-SIN9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-SIN9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-SIN9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         8
               MOVE       1     TO  RECE41-RIHA2-KOKYUKI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-KOK9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE41-RIHA2-KOK9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-KOK9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-KOK9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         9
               MOVE       1     TO  RECE41-RIHA2-HAIYO9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE41-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE41-RIHA2-HAI9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE41-RIHA2-HAI9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE41-RIHA2-HAI9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE41-RIHA2-HAI9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
           END-EVALUATE
      *
           .
       210121A-RIHAYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       210122-RECE41-HENSYU-SEC               SECTION.
      *
      *    ½èÊý¸º²ó¿ô¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-SYOHOU-KAISU
      *    ½èÊý¤»¤ó¸º²ó¿ô¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-SYOSEN-KAISU
      *    ½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-MADOKU-SHOHO
      *
           MOVE   ZERO              TO   WK-ZAIKAISU
           MOVE   ZERO              TO   WK-ZAITEN
           MOVE   ZERO              TO   WK-SYUTEN
           MOVE   ZERO              TO   WK-KIZAITEN
           MOVE   ZERO              TO   WK-YKZTEN
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       IF      RECE41-HKNKBN   NOT  =   "3"
                          MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       ELSE
                          MOVE  WRK-ZAIKAISU-XXX  TO   WK-ZAIKAISU
                       END-IF 
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                         IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                              "40" OR "50" OR "54" OR
                                              "60" OR "70" OR "80" OR
                                              "64"
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                  COMPUTE   WK-YKZTEN =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                         END-IF
                       ELSE                                             
                         IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                              "40" OR "50" OR "54" OR
                                              "60" OR "70" OR "80" OR
                                              "64"
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                                  IF  SRY-SRYKBN  =  "70"
                                     COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                         -  ACCT-SYUTEN2
                                     COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                         +  ACCT-SYUTEN2
                                  END-IF
                                  IF  WRK-YOUBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-YOUBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-YOUBUKASAN
                                  END-IF
                                  IF  WRK-KYOBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KYOBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KYOBUKASAN
                                  END-IF
                                  IF  WRK-KEIBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KEIBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KEIBUKASAN
                                  END-IF
                                  IF  WRK-SOGAIKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-SOGAIKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-SOGAIKASAN
                                  END-IF
                                  IF  WRK-RYOYOTEATE  NOT =  ZERO
                                   IF ((RECE41-HKNKBN     =  "4") AND
                                       (RECE41-JIBAIKBN   =  "2"))
                                   OR ( RECE41-HKNKBN     =  "6" )
                                    COMPUTE WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-RYOYOTEATE
                                    COMPUTE WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-RYOYOTEATE
                                   END-IF
                                  END-IF
                         END-IF
                       END-IF                                           
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                           IF   RECE41-HKNKBN   =  "4" OR "6"
                             IF  WRK-YOUBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-YOUBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-YOUBUKASAN
                             END-IF
                             IF  WRK-KYOBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KYOBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KYOBUKASAN
                             END-IF
                             IF  WRK-KEIBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KEIBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KEIBUKASAN
                             END-IF
                             IF  WRK-SOGAIKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-SOGAIKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-SOGAIKASAN
                             END-IF
                           END-IF
                       END-IF
                   ELSE
                       IF ( SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"  OR  "64" OR
                                            "25"                     )
                       OR ((ACCT-ZAIKBN  =   2  ) AND
                           (SRY-SRYKBN   =  "12"))
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
                           MOVE  ZERO          TO   FLG-OSINKBN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
                     MOVE    ZERO     TO    FLG-OSINKBN
               END-IF
               IF  ((ACCT-ZAITEN    NOT =    ZERO) AND
                    (ACCT-ZAIKAISU  NOT =    ZERO))
               OR  ( FLG-TAISYO         =     1   )
                  EVALUATE   RECE41-HKNKBN
                    WHEN       "1"
                    WHEN       "2"
                        PERFORM   2101221-RECE-LEFT-SET-SEC
                    WHEN       "3"
                        PERFORM   2101222-RECE-LEFT-SET2-SEC
                    WHEN       "4"
                        EVALUATE  WRK-PARA-RECEKBN
                          WHEN      "4"
      ********************* ¡Ê¿·ÍÍ¼°¡Ë
                            PERFORM   2101223-RECE-LEFT-SET3-SEC
                          WHEN      "5"
      ********************* ¡Ê½¾ÍèÍÍ¼°¡Ë
                            PERFORM   2101224-RECE-LEFT-SET4-SEC
                        END-EVALUATE
                    WHEN       "5"
                        PERFORM   2101225-RECE-LEFT-SET5-SEC
                    WHEN       "6"
                        PERFORM   2101224-RECE-LEFT-SET4-SEC
                  END-EVALUATE
               END-IF
               IF  ( SRY-SRYKBN   =  "21"   OR   "22"   OR   "23"   OR
                                     "31"   OR   "32"   OR   "33"   OR
                                     "40"   OR   "50"   OR   "54"   OR
                                     "60"   OR   "70"   OR   "14"   OR
                                     "64"          )
               OR  ((ACCT-ZAIKBN  =    2          ) AND
                    (SRY-SRYKBN   =   "12" OR "13"))
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           EVALUATE   RECE41-HKNKBN
             WHEN       "1"
             WHEN       "2"
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)
                                              -    WRK-SYOHOU-KAISU
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)
                                              -    WRK-SYOSEN-KAISU
      *         ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                COMPUTE    RECE41-KAISU(20)   =    RECE41-KAISU(20)
                                              -    WRK-MADOKU-SHOHO
             WHEN       "3"
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)
                                              -    WRK-SYOHOU-KAISU
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)
                                              -    WRK-SYOSEN-KAISU
      *         ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                COMPUTE    RECE41-KAISU(13)   =    RECE41-KAISU(13)
                                              -    WRK-MADOKU-SHOHO
             WHEN       "4"
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************¡Ê¿·ÍÍ¼°¡Ë
                   IF   WRK-PARA-JIBAIPRTKBN  =  "2"
      *             ½èÊý¤Î»»Äê²ó¿ô¤«¤é¸º»»
                    COMPUTE    RECE41-KAISU(14)  =  RECE41-KAISU(14)
                                                 -  WRK-SYOHOU-KAISU
      *             ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¸º»»
                    COMPUTE    RECE41-KAISU(31)  =  RECE41-KAISU(31)
                                                 -  WRK-SYOSEN-KAISU
      *             ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)
                                                 -  WRK-MADOKU-SHOHO
                   ELSE
      *             ½èÊý¤Î»»Äê²ó¿ô¤«¤é¸º»»
                    COMPUTE    RECE41-KAISU(14)  =  RECE41-KAISU(14)
                                                 -  WRK-SYOHOU-KAISU
      *             ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¸º»»
                    COMPUTE    RECE41-KAISU(33)  =  RECE41-KAISU(33)
                                                 -  WRK-SYOSEN-KAISU
      *             ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)
                                                 -  WRK-MADOKU-SHOHO
                   END-IF
                  WHEN      "5"
      **************¡Ê½¾ÍèÍÍ¼°¡Ë
      *             ½èÊý¤Î»»Äê²ó¿ô¤«¤é¸º»»
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)
                                                 -  WRK-SYOHOU-KAISU
      *             ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¸º»»
                    COMPUTE    RECE41-KAISU(35)  =  RECE41-KAISU(35)
                                                 -  WRK-SYOSEN-KAISU
      *             ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                    COMPUTE    RECE41-KAISU(16)  =  RECE41-KAISU(16)
                                                 -  WRK-MADOKU-SHOHO
                END-EVALUATE
             WHEN       "5"
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(22)   =    RECE41-KAISU(22)
                                              -    WRK-SYOHOU-KAISU
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(36)   =    RECE41-KAISU(36)
                                              -    WRK-SYOSEN-KAISU
      *         ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                COMPUTE    RECE41-KAISU(23)   =    RECE41-KAISU(23)
                                              -    WRK-MADOKU-SHOHO
             WHEN       "6"
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(15)   =    RECE41-KAISU(15)
                                              -    WRK-SYOHOU-KAISU
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¸º»»
                COMPUTE    RECE41-KAISU(35)   =    RECE41-KAISU(35)
                                              -    WRK-SYOSEN-KAISU
      *         ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                COMPUTE    RECE41-KAISU(16)   =    RECE41-KAISU(16)
                                              -    WRK-MADOKU-SHOHO
           END-EVALUATE
      *
           .
      *
       210122-RECE41-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101221-RECE-LEFT-SET-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
                   IF  (SRY-SRYCD(IDX3) = "101110030" )
                   AND (RECE41-SRYYM   <=   "201306"  )
                        COMPUTE    RECE41-KINGAKU2(4)  =
                                   RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                   WHEN  120
                     IF ((SRY-SRYCD(IDX3) =  "101120040"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120100"  ) AND
                         (RECE41-SRYYM   <=    "201306"   ))
                     OR ((SRY-SRYCD(IDX3) =  "101120010"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120070"   OR
                                             "101120080"   OR
                                             "101120040"  ) AND
                         (RECE41-SRYYM   >=    "201307"   ))
                         ADD   WK-ZAIKAISU      TO   RECE41-KAISU2(2)
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     ELSE
                       IF     RECE41-SRYYM  >=  "201804"
                         IF    RECE41-KHNTEN(2) = ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO  RECE41-KHNTEN(2)
                         ELSE
                           IF  RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO     TO  RECE41-KHNTEN(2)
                             MOVE   1        TO  FLG-KHNTEN2
                           END-IF
                         END-IF
                       ELSE
                         IF  (SRY-SRYCD(IDX3) =  "112016070"     OR
                                                 "112015670"     OR
                                                 "112016170"     OR
                                                 "112015770"     OR
                                                 "112017270"     OR
                                                 "112017570"       )
                         AND (RECE41-SRYYM   >=  "201004"          )
                           IF  SRY-SRYCD(IDX3)  =  "112016070"
                             COMPUTE  CNT-JIKANTAI1 = CNT-JIKANTAI1
                                                    + WK-ZAIKAISU
                             IF   FLG-JIKANTAI1-ARI  =  ZERO
                                ADD  WRK-TNS-TEN(IDX3)
                                               TO  RECE41-KHNTEN(2)
                                MOVE     1     TO  FLG-JIKANTAI1-ARI
                             END-IF
                           END-IF
                           IF  SRY-SRYCD(IDX3)  =  "112015670"
                             COMPUTE  CNT-JIKANTAI2 = CNT-JIKANTAI2
                                                    + WK-ZAIKAISU
                             IF   FLG-JIKANTAI2-ARI   =  ZERO
                                ADD  WRK-TNS-TEN(IDX3)
                                               TO  RECE41-KHNTEN(2)
                                MOVE     1     TO  FLG-JIKANTAI2-ARI
                             END-IF
                           END-IF
                           IF  SRY-SRYCD(IDX3)  =  "112016170"
                             COMPUTE  CNT-JIKANTAI3 = CNT-JIKANTAI3
                                                    + WK-ZAIKAISU
                             IF   FLG-JIKANTAI3-ARI  =  ZERO
                                ADD  WRK-TNS-TEN(IDX3)
                                               TO  RECE41-KHNTEN(2)
                                MOVE     1     TO  FLG-JIKANTAI3-ARI
                             END-IF
                           END-IF
                           IF  SRY-SRYCD(IDX3)  =  "112015770"
                             COMPUTE  CNT-MESAIKSN =  CNT-MESAIKSN
                                                   +  WK-ZAIKAISU
                             IF   FLG-MESAIKSN-ARI   =  ZERO
                                ADD  WRK-TNS-TEN(IDX3)
                                               TO  RECE41-KHNTEN(2)
                                MOVE     1     TO  FLG-MESAIKSN-ARI
                             END-IF
                           END-IF
                           IF  SRY-SRYCD(IDX3)  =  "112017270"
                             COMPUTE  CNT-CHIHOKATU = CNT-CHIHOKATU
                                                    + WK-ZAIKAISU
                             IF   FLG-CHIHOKATU-ARI  =  ZERO
                                ADD  WRK-TNS-TEN(IDX3)
                                               TO  RECE41-KHNTEN(2)
                                MOVE     1     TO  FLG-CHIHOKATU-ARI
                             END-IF
                           END-IF
                           IF  SRY-SRYCD(IDX3)  =  "112017570"
                             COMPUTE  CNT-CHIHOKAT2 = CNT-CHIHOKAT2
                                                    + WK-ZAIKAISU
                             IF   FLG-CHIHOKAT2-ARI  =  ZERO
                                ADD  WRK-TNS-TEN(IDX3)
                                               TO  RECE41-KHNTEN(2)
                                MOVE     1     TO  FLG-CHIHOKAT2-ARI
                             END-IF
                           END-IF
                         ELSE
                           IF   FLG-SAISIN-ARI   =  ZERO
                                ADD  WRK-TNS-TEN(IDX3)
                                               TO  RECE41-KHNTEN(2)
                                MOVE     1     TO  FLG-SAISIN-ARI
                           END-IF
                         END-IF
                       END-IF
                       IF   FLG-COUNT-ARI  =  ZERO
                         ADD    WK-ZAIKAISU   TO   RECE41-KAISU(2)
                         MOVE   1             TO   FLG-COUNT-ARI
                       END-IF
                       COMPUTE    RECE41-TENSU(2)  =
                                  RECE41-TENSU(2)  +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF
      *
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                   WHEN  122
                        IF    RECE41-KHNTEN(3)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                        ELSE
                           IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO  RECE41-KHNTEN(3)
                              MOVE   1    TO  FLG-KHNTEN3
                           END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE    RECE41-TENSU(3)  =
                                   RECE41-TENSU(3)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                   WHEN  123
                        IF    RECE41-KHNTEN(4)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(4)
                        ELSE
                           IF RECE41-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(4)
                              MOVE   1    TO  FLG-KHNTEN4
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                   WHEN  124
                        IF    RECE41-KHNTEN(5)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                        ELSE
                           IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(5)
                              MOVE   1     TO   FLG-KHNTEN5
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU   TO
                                              RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5) =
                                 RECE41-TENSU(5) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¿¼Ìë¡Ë
                   WHEN  125
                        IF    RECE41-KHNTEN(6)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                        ELSE
                           IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(6)
                              MOVE   1       TO   FLG-KHNTEN6
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6) =
                                 RECE41-TENSU(6) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¤½¤ÎÂ¾¡Ë
                   WHEN  OTHER 
                        IF  (SRY-SRYCD(IDX3) =  "101120030" )
                        AND (RECE41-SRYYM   <=  "201306"    )
                           ADD   WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                           COMPUTE    RECE41-KINGAKU2(3)  =
                                      RECE41-KINGAKU2(3)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                        IF   ACCT-ZAIKBN     =  2
                           MOVE   ZERO         TO   RECE41-KHNTEN(2)
                           MOVE   1            TO   FLG-KHNTEN2
                           COMPUTE    WRK-RECE4G-TENSU(2)  =
                                      WRK-RECE4G-TENSU(2)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                 IF   (SRY-SRYCD(IDX3) =  "101130190"  )
                 AND  (RECE41-SRYYM   >=  "201307"     )
                         ADD   WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                         COMPUTE    RECE41-KINGAKU2(3)  =
                                    RECE41-KINGAKU2(3)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                 ELSE
                   IF ((SRY-SRYCD(IDX3) =
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"                         ) AND
                       (RECE41-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX3)  =  "101130010"     ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                         COMPUTE    RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE4G-TENSU(7)  =
                                    WRK-RECE4G-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)  =
                                    RECE41-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
                 END-IF
      *
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE4G-TENSU(12)  =
                              WRK-RECE4G-TENSU(12)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(8)
                       COMPUTE   RECE41-TENSU(8)  =
                                 RECE41-TENSU(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU(9)  =
                                 RECE41-TENSU(9)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029270")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030170")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030270")))
                   OR (  FLG-OSINKBN           =     9           )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)  =
                                 RECE41-TENSU(11)  +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(12)  =
                                 RECE41-TENSU(12)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE41-TENSU(13)  =
                             RECE41-TENSU(13)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010002"
                                            OR "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    "099209901"
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(15)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(15)
                       ELSE
                          IF   RECE41-KHNTEN(15) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(15)
                               MOVE   1       TO    FLG-KHNTEN15
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(15)
                       COMPUTE   RECE41-TENSU(15)  =
                                 RECE41-TENSU(15)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                 IF    SRY-SRYCD(IDX3)      =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  -
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(16)
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(17)  =
                                 RECE41-TENSU(17)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(17)
                       COMPUTE   RECE41-TENSU(17)  =
                                 RECE41-TENSU(17)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                       IF    RECE41-KHNTEN(18)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(18)
                       ELSE
                          IF   RECE41-KHNTEN(18) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(18)
                               MOVE   1       TO    FLG-KHNTEN18
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                       COMPUTE   RECE41-TENSU(18)  =
                                 RECE41-TENSU(18)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF  SRY-SRYCD(IDX3) = "120002170" OR "120002270" OR
                                         "120003170" OR "120003370" OR
                                         "120003870" OR
                                         "120004070" OR "120004170" OR
                                         "120004370" OR "120004470"
                      COMPUTE  WRK-SYOHOU-KAISU  =  WRK-SYOHOU-KAISU
                                                 +  ACCT-ZAIKAISU
                   END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(19)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(19)
                     ELSE
                      IF   RECE41-KHNTEN(19) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(19)
                            MOVE   1      TO   FLG-KHNTEN19
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    ACCT-ZAIKAISU   TO    RECE41-KAISU(19)
                   END-IF
                   COMPUTE   RECE41-TENSU(19)  =
                             RECE41-TENSU(19)  +
                            (WK-ZAITEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(20)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(22)
                   COMPUTE   RECE41-TENSU(22)  =
                             RECE41-TENSU(22)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(23)
                   COMPUTE   RECE41-TENSU(23)  =
                             RECE41-TENSU(23)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(24)
                   COMPUTE   RECE41-TENSU(24)  =
                             RECE41-TENSU(24)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(25)
                   END-IF
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(28)  =
                             RECE41-TENSU(28)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   "160101210"     OR
                                                  "160095710"     OR
                                                  "160095810"
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(30)  =
                             RECE41-TENSU(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(31)
                   END-IF
                   COMPUTE   RECE41-TENSU(31)  =
                             RECE41-TENSU(31)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(32)  =
                             RECE41-TENSU(32)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                 IF   ((SRY-SRYCD(IDX3) =
                        "101800890"   OR
                        "101800870"   OR  "101800470"   OR
                        "101800480"   OR  "101800490"   OR
                        "101800500"   OR  "101800510"   OR
                        "101800520"   OR  "101800530"   OR
                        "101800540"   OR  "101800960"   OR
                        "101800970"   OR  "101800980"   OR
                        "101801040"                         ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                       COMPUTE    RECE41-KINGAKU2(4)  =
                                  RECE41-KINGAKU2(4)  +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                 ELSE
                   IF ((ACCT-SRYKBN          =  "80"    )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09593" ))
                   OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                        <= "101801020"  )  AND
                       (RECE41-SRYYM    >= "201307"     ))
                   OR ((SRY-SRYCD(IDX3)  = "101801030"  )  AND
                       (RECE41-SRYYM    >= "201307"     ))
                     IF   SRY-SRYCD(IDX3)(1:5) = "09593"
                       COMPUTE    RECE41-KINGAKU2(4)  =                 
                                  RECE41-KINGAKU2(4)  +                 
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)  
                       IF   FLG-TAISYO   =   1
                          COMPUTE WRK-ZAITENNASHI  =
                                  WRK-ZAITENNASHI  +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                     ELSE
                       IF  SRY-SRYCD(IDX3)  =  "101801030"
                         COMPUTE  WRK-FUTANWARI  =
                                  WRK-TNS-TEN(IDX3) / 100
                         COMPUTE  WRK-KEISAN-KG2 =
                                 (WRK-TEN-KG2       * WRK-FUTANWARI)
                         COMPUTE  RECE41-KINGAKU2(4)  =
                                  RECE41-KINGAKU2(4)  +
                                 (WRK-KEISAN-KG2    * ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE  RECE41-KINGAKU2(4)  =
                                  RECE41-KINGAKU2(4)  +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE  WRK-TEN-KG2 = WRK-TNS-TEN(IDX3)
                       END-IF
                     END-IF
                   ELSE               
                    IF  SRY-SRYCD(IDX3) =  "120002470"
                                        OR "120002570"
                                        OR "120003270"
                                        OR "120003470"
                                        OR "120003570"
                                        OR "120003970"
                                        OR "120004270"
                                        OR "120004570"
                       COMPUTE  WRK-SYOSEN-KAISU  =  WRK-SYOSEN-KAISU
                                                  +  ACCT-ZAIKAISU
                    END-IF
                    IF   SRY-SRYCD(IDX3)   =  "120002710"    OR
                                              "120002810"    OR
                                              "120002910"    OR
                                              "120003010"    OR
                                              "120003710"    OR
                                              "120002470"    OR
                                              "120002570"    OR
                                              "120003270"    OR
                                              "120003470"    OR
                                              "120003570"    OR
                                              "120003970"    OR
                                              "120004270"    OR
                                              "120004570"    OR
                                              "120004610"
                         ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(33)
                         COMPUTE   RECE41-TENSU(33)  =
                                   RECE41-TENSU(33)  +
                                  (WK-ZAITEN  *   ACCT-ZAIKAISU)
                    ELSE
                         COMPUTE   RECE41-TENSU(34)  =
                                   RECE41-TENSU(34)  +
                                  (WK-SYUTEN  *   ACCT-ZAIKAISU)
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(7)  =
                                     RECE41-TENSU(7)  +
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                           COMPUTE   RECE41-TENSU(34) =
                                     RECE41-TENSU(34) -
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(35)  =
                              RECE41-TENSU(35)  +
                             (WK-YKZTEN   *   ACCT-ZAIKAISU)
                   END-IF
                 END-IF
           END-EVALUATE
      *
           .
      *
       2101221-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101222-RECE-LEFT-SET2-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     IF   SRY-SRYCD(IDX3) = "111012370"
                       CONTINUE
                     ELSE
                       ADD   WK-ZAIKAISU   TO   RECE41-KAISU(1)
                     END-IF
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                        MOVE    RECE41-TEKSTYMD    
                                           TO   RECE41-SYOSAIYMD2(1)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                   WHEN  120
                     IF ((SRY-SRYCD(IDX3) =  "101120040"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120100"  ) AND
                         (RECE41-SRYYM   <=    "201306"   ))
                     OR ((SRY-SRYCD(IDX3) =  "101120010"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120070"   OR
                                             "101120080"   OR
                                             "101120040"  ) AND
                         (RECE41-SRYYM   >=    "201307"   ))
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         MOVE    RECE41-TEKSTYMD    
                                           TO   RECE41-SYOSAIYMD2(2)
                     ELSE
                         IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                            COMPUTE    RECE41-TENSU(3)  =
                                       RECE41-TENSU(3)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         ELSE
                            IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                            END-IF
                            COMPUTE    RECE41-TENSU(2)  =
                                       RECE41-TENSU(2)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         END-IF
                     END-IF
      *      ¡Ê»þ´Ö³°¡Ë
                   WHEN  123
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "1"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(4)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "1"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      ¡ÊµÙÆü¡Ë
                   WHEN  124
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "2"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(4)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "2"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      ¡Ê¿¼Ìë¡Ë
                   WHEN  125
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "3"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(4)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "3"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      ¡Ê¤½¤ÎÂ¾¡Ë
                   WHEN  OTHER 
                     IF   ACCT-ZAIKBN     =  2
                        COMPUTE WRK-RECE4G-TENSU(3)  =
                                WRK-RECE4G-TENSU(3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE4G-TENSU(5)  =
                                    WRK-RECE4G-TENSU(5)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE41-TENSU(5)  =
                                    RECE41-TENSU(5)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE4G-TENSU(6)  =
                              WRK-RECE4G-TENSU(6)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029270")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030170")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030270")))
                   OR (  FLG-OSINKBN           =     9           )
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE41-TENSU(6)  =
                             RECE41-TENSU(6)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010002"
                                            OR "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(7)
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    "099209901"
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(7)
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(8)
                       COMPUTE   RECE41-TENSU(8)  =
                                 RECE41-TENSU(8)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF   
                   END-IF
               WHEN    "22"
                 IF    SRY-SRYCD(IDX3)      =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                   COMPUTE   RECE41-TENSU(9)  =
                             RECE41-TENSU(9)  -
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(9)
                   COMPUTE   RECE41-TENSU(9)  =
                             RECE41-TENSU(9)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)  =
                                 RECE41-TENSU(11)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF  SRY-SRYCD(IDX3) = "120002170" OR "120002270" OR
                                         "120003170" OR "120003370" OR
                                         "120003870" OR
                                         "120004070" OR "120004170" OR
                                         "120004370" OR "120004470"
                      COMPUTE  WRK-SYOHOU-KAISU  =  WRK-SYOHOU-KAISU
                                                 +  ACCT-ZAIKAISU
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WRK-ZAIKAISU-XXX   TO   RECE41-KAISU(12)
                   END-IF
                   COMPUTE   RECE41-TENSU(12)  =
                             RECE41-TENSU(12)  +
                            (WK-ZAITEN   *   WRK-ZAIKAISU-XXX)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(13)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(13)  =
                             RECE41-TENSU(13)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(14)  =
                             RECE41-TENSU(14)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(15)
                   COMPUTE   RECE41-TENSU(15)  =
                             RECE41-TENSU(15)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(16)
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   COMPUTE   RECE41-TENSU(17)  =
                             RECE41-TENSU(17)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                   END-IF
                   COMPUTE   RECE41-TENSU(18)  =
                             RECE41-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(21)
                   END-IF
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(23)  =
                             RECE41-TENSU(23)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(24)
                   END-IF
                   COMPUTE   RECE41-TENSU(24)  =
                             RECE41-TENSU(24)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(24)  =
                                   RECE41-KAISU(24)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   "160101210"     OR
                                                  "160095710"     OR
                                                  "160095810"
                         IF   RECE41-KAISU(24)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(24)  =
                                        RECE41-KAISU(24)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF ((ACCT-SRYKBN          =  "80"    )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09593" ))
                   OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                        <= "101801020"  )  AND
                       (RECE41-SRYYM    >= "201307"     ))
                   OR ((SRY-SRYCD(IDX3)  = "101801030"  )  AND
                       (RECE41-SRYYM    >= "201307"     ))
      ***              CONTINUE
                       IF   (FLG-TAISYO           =   1     )
                       AND  (SRY-SRYCD(IDX3)(1:5) =  "09593")
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                       END-IF
                   ELSE               
                    IF   SRY-SRYCD(IDX3)   =      "120002710"    OR
                                                  "120002810"    OR
                                                  "120002910"    OR
                                                  "120003010"    OR
                                                  "120003710"    OR
                                                  "120002470"    OR
                                                  "120002570"    OR
                                                  "120003270"    OR
                                                  "120003470"    OR
                                                  "120003570"    OR
                                                  "120003970"    OR
                                                  "120004270"    OR
                                                  "120004570"    OR
                                                  "120004610"
                         COMPUTE   RECE41-TENSU(30)  =
                                   RECE41-TENSU(30)  +
                                  (WK-ZAITEN  *   WRK-ZAIKAISU-XXX)
                    ELSE
                         COMPUTE   RECE41-TENSU(31)  =
                                   RECE41-TENSU(31)  +
                                  (WK-SYUTEN  *   WRK-ZAIKAISU-XXX)
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(5)  =
                                     RECE41-TENSU(5)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                           COMPUTE   RECE41-TENSU(31) =
                                     RECE41-TENSU(31) -
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(32)  =
                              RECE41-TENSU(32)  +
                             (WK-YKZTEN   *   WRK-ZAIKAISU-XXX)
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101222-RECE-LEFT-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101223-RECE-LEFT-SET3-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  "111000370"   OR
                                             "111011470"   OR
                                             "111011970"   OR
                                             "111012070"   OR
                                             "111012170"   OR
                                             "111012270"   OR
                                             "111011570"   OR
                                             "111011670"   OR
                                             "111011770"
                             MOVE  "1"     TO   RECE41-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
                   IF  (SRY-SRYCD(IDX3) = "101110030" )
                   AND (RECE41-SRYYM   <=   "201306"  )
                        COMPUTE    RECE41-KINGAKU2(4)  =
                                   RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                   WHEN  120
                     IF ((SRY-SRYCD(IDX3) =  "101120040"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120100"  ) AND
                         (RECE41-SRYYM   <=    "201306"   ))
                     OR ((SRY-SRYCD(IDX3) =  "101120010"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120070"   OR
                                             "101120080"   OR
                                             "101120040"  ) AND
                         (RECE41-SRYYM   >=    "201307"   ))
                         IF ((SRY-SRYCD(IDX3) =  "101120100"  ) AND
                             (RECE41-SRYYM   <=  "201306"     ))
                         OR ((SRY-SRYCD(IDX3) =  "101120040"
                                              OR "101120080"  ) AND
                             (RECE41-SRYYM   >=  "201307"     ))
                           CONTINUE
                         ELSE
                           ADD    WK-ZAIKAISU   TO   RECE41-KAISU2(2)
                         END-IF
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)  =
                                    RECE41-TENSU(7)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF
      *
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                   WHEN  122
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(2)
                        COMPUTE    RECE41-TENSU(2)  =
                                   RECE41-TENSU(2)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                   WHEN  123
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE RECE41-TENSU(3) =
                                RECE41-TENSU(3) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                   WHEN  124
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(4)
                        COMPUTE  RECE41-TENSU(4) =
                                 RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¿¼Ìë¡Ë
                   WHEN  125
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5) =
                                 RECE41-TENSU(5) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¤½¤ÎÂ¾¡Ë
                   WHEN  OTHER 
                        IF  (SRY-SRYCD(IDX3) =  "101120030" )
                        AND (RECE41-SRYYM   <=  "201306"    )
                           ADD   WK-ZAIKAISU TO   RECE41-KAISU2(3)
                           COMPUTE    RECE41-KINGAKU2(3)  =
                                      RECE41-KINGAKU2(3)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                        IF   ACCT-ZAIKBN     =  2
                           COMPUTE    WRK-RECE4G-TENSU(7)  =
                                      WRK-RECE4G-TENSU(7)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                 IF   (SRY-SRYCD(IDX3) =  "101130190"  )
                 AND  (RECE41-SRYYM   >=  "201307"     )
                         ADD   WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                         COMPUTE    RECE41-KINGAKU2(3)  =
                                    RECE41-KINGAKU2(3)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                 ELSE
                   IF ((SRY-SRYCD(IDX3) =
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"                         ) AND
                       (RECE41-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX3)  =  "101130010"     ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE4G-TENSU(6)  =
                                    WRK-RECE4G-TENSU(6)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(6)  =
                                    RECE41-TENSU(6)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
                 END-IF
      *
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE4G-TENSU(7)  =
                              WRK-RECE4G-TENSU(7)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029270")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030170")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030270")))
                   OR (  FLG-OSINKBN           =     9           )
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE41-TENSU-YAK(7)  =
                             RECE41-TENSU-YAK(7)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010002"
                                            OR "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    "099209901"
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(10)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(10)
                       ELSE
                          IF   RECE41-KHNTEN(10) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(10)
                               MOVE   1       TO    FLG-KHNTEN10-JI
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                 IF    SRY-SRYCD(IDX3)      =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                   COMPUTE   RECE41-TENSU-YAK(11)  =
                             RECE41-TENSU-YAK(11)  -
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                   COMPUTE   RECE41-TENSU-YAK(11)  =
                             RECE41-TENSU-YAK(11)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU-YAK(12)  =
                                 RECE41-TENSU-YAK(12)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(12)
                       COMPUTE   RECE41-TENSU-YAK(12)  =
                                 RECE41-TENSU-YAK(12)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                       IF    RECE41-KHNTEN(13)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(13)
                       ELSE
                          IF   RECE41-KHNTEN(13) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(13)
                               MOVE   1       TO    FLG-KHNTEN13-JI
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(13)
                       COMPUTE   RECE41-TENSU(13)  =
                                 RECE41-TENSU(13)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF  SRY-SRYCD(IDX3) = "120002170" OR "120002270" OR
                                         "120003170" OR "120003370" OR
                                         "120003870" OR
                                         "120004070" OR "120004170" OR
                                         "120004370" OR "120004470"
                      COMPUTE  WRK-SYOHOU-KAISU  =  WRK-SYOHOU-KAISU
                                                 +  ACCT-ZAIKAISU
                   END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(14)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(14)
                     ELSE
                      IF   RECE41-KHNTEN(14) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(14)
                            MOVE   1      TO   FLG-KHNTEN14-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    ACCT-ZAIKAISU   TO    RECE41-KAISU(14)
                   END-IF
                   COMPUTE   RECE41-TENSU(14)  =
                             RECE41-TENSU(14)  +
                            (WK-ZAITEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(15)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(15)  =
                             RECE41-TENSU(15)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(16)
                   END-IF
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(18)
                   COMPUTE   RECE41-TENSU(18)  =
                             RECE41-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(21)  =
                             RECE41-TENSU-YAK(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(19)
                   COMPUTE   RECE41-TENSU(19)  =
                             RECE41-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(19)
                   COMPUTE   RECE41-TENSU(19)  =
                             RECE41-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(21)  =
                             RECE41-TENSU-YAK(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(20)
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(20)
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(21)  =
                             RECE41-TENSU-YAK(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(21)
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(23)
                   END-IF
                   COMPUTE   RECE41-TENSU(23)  =
                             RECE41-TENSU(23)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(24)  =
                             RECE41-TENSU-YAK(24)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(25)
                   END-IF
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(26)  =
                             RECE41-TENSU-YAK(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(25)
                   END-IF
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(26)  =
                             RECE41-TENSU-YAK(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(28)  =
                             RECE41-TENSU-YAK(28)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(27)  =
                                   RECE41-KAISU(27)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   "160101210"     OR
                                                  "160095710"     OR
                                                  "160095810"
                         IF   RECE41-KAISU(27)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(27)  =
                                        RECE41-KAISU(27)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU-YAK(28)  =
                             RECE41-TENSU-YAK(28)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   "160101210"     OR
                                                  "160095710"     OR
                                                  "160095810"
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU-YAK(30)  =
                             RECE41-TENSU-YAK(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(30)  =
                             RECE41-TENSU-YAK(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(31)
                   END-IF
                   COMPUTE   RECE41-TENSU(31)  =
                             RECE41-TENSU(31)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(32)  =
                             RECE41-TENSU-YAK(32)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                 IF   ((SRY-SRYCD(IDX3) =
                        "101800890"   OR
                        "101800870"   OR  "101800470"   OR
                        "101800480"   OR  "101800490"   OR
                        "101800500"   OR  "101800510"   OR
                        "101800520"   OR  "101800530"   OR
                        "101800540"   OR  "101800960"   OR
                        "101800970"   OR  "101800980"   OR
                        "101801040"                         ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = "101800890"
                        COMPUTE     RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR
                                                "09592" )
                        CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"    )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593" ))
                    OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                         <= "101801020"  )  AND
                        (RECE41-SRYYM    >= "201307"     ))
                    OR ((SRY-SRYCD(IDX3)  = "101801030"  )  AND
                        (RECE41-SRYYM    >= "201307"     ))
                     IF   SRY-SRYCD(IDX3)(1:5) = "09593"
                      EVALUATE  WRK-TNS-JIBAISYUKEIKBN(IDX3)
                       WHEN       0
                       WHEN       5
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       1
                         COMPUTE    RECE41-KINGAKU2(1)  =
                                    RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       2
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       3
                         ADD    WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                         COMPUTE    RECE41-KINGAKU2(3)  =
                                    RECE41-KINGAKU2(3)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       4
                         COMPUTE    RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                      IF  SRY-SRYCD(IDX3)  =  "101801030"
                         COMPUTE    WRK-FUTANWARI  =
                                    WRK-TNS-TEN(IDX3) / 100
                         COMPUTE    WRK-KEISAN-KG2 =
                                   (WRK-TEN-KG2       * WRK-FUTANWARI)
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-KEISAN-KG2    * ACCT-ZAIKAISU)
                      ELSE
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE    WRK-TEN-KG2 = WRK-TNS-TEN(IDX3)
                      END-IF
                     END-IF
                    ELSE
                     IF  SRY-SRYCD(IDX3)      =    "199000610"
                       COMPUTE    RECE41-KINGAKU2(6)  =
                                  RECE41-KINGAKU2(6)  +
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)
                     ELSE
                      IF  SRY-SRYCD(IDX3) =  "120002470"
                                          OR "120002570"
                                          OR "120003270"
                                          OR "120003470"
                                          OR "120003570"
                                          OR "120003970"
                                          OR "120004270"
                                          OR "120004570"
                         COMPUTE  WRK-SYOSEN-KAISU  =  WRK-SYOSEN-KAISU
                                                    +  ACCT-ZAIKAISU
                      END-IF
                      IF   SRY-SRYCD(IDX3)    =    "120002710"    OR
                                                   "120002810"    OR
                                                   "120002910"    OR
                                                   "120003010"    OR
                                                   "120003710"    OR
                                                   "120002470"    OR
                                                   "120002570"    OR
                                                   "120003270"    OR
                                                   "120003470"    OR
                                                   "120003570"    OR
                                                   "120003970"    OR
                                                   "120004270"    OR
                                                   "120004570"    OR
                                                   "120004610"
                          ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(31)
                          COMPUTE   RECE41-TENSU(31)  =
                                    RECE41-TENSU(31)  +
                                   (WK-ZAITEN  *   ACCT-ZAIKAISU)
                      ELSE
                          COMPUTE   RECE41-TENSU(32)  =
                                    RECE41-TENSU(32)  +
                                   (WK-SYUTEN  *   ACCT-ZAIKAISU)
                          IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                            COMPUTE   RECE41-TENSU(6)  =
                                      RECE41-TENSU(6)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            COMPUTE   RECE41-TENSU(32)  =
                                      RECE41-TENSU(32)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                          END-IF
                      END-IF
                      COMPUTE   RECE41-TENSU-YAK(33)  =
                                RECE41-TENSU-YAK(33)  +
                               (WK-YKZTEN   *   ACCT-ZAIKAISU)
                     END-IF
                    END-IF
                   END-IF
                 END-IF
                ELSE
                 IF   ((SRY-SRYCD(IDX3) =
                        "101800890"   OR
                        "101800870"   OR  "101800470"   OR
                        "101800480"   OR  "101800490"   OR
                        "101800500"   OR  "101800510"   OR
                        "101800520"   OR  "101800530"   OR
                        "101800540"   OR  "101800960"   OR
                        "101800970"   OR  "101800980"   OR
                        "101801040"                         ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = "101800890"
                        COMPUTE     RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                        CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"    )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593" ))
                    OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                         <= "101801020"  )  AND
                        (RECE41-SRYYM    >= "201307"     ))
                    OR ((SRY-SRYCD(IDX3)  = "101801030"  )  AND
                        (RECE41-SRYYM    >= "201307"     ))
                     IF   SRY-SRYCD(IDX3)(1:5) = "09593"
                      EVALUATE  WRK-TNS-JIBAISYUKEIKBN(IDX3)
                       WHEN       0
                       WHEN       5
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       1
                         COMPUTE    RECE41-KINGAKU2(1)  =
                                    RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       2
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       3
                         ADD    WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                         COMPUTE    RECE41-KINGAKU2(3)  =
                                    RECE41-KINGAKU2(3)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       4
                         COMPUTE    RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                      IF  SRY-SRYCD(IDX3)  =  "101801030"
                         COMPUTE    WRK-FUTANWARI  =
                                    WRK-TNS-TEN(IDX3) / 100
                         COMPUTE    WRK-KEISAN-KG2 =
                                   (WRK-TEN-KG2       * WRK-FUTANWARI)
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-KEISAN-KG2    * ACCT-ZAIKAISU)
                      ELSE
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE    WRK-TEN-KG2 = WRK-TNS-TEN(IDX3)
                      END-IF
                     END-IF
                    ELSE                                                
                     IF  SRY-SRYCD(IDX3)      =    "199000610"
                       COMPUTE    RECE41-KINGAKU2(6)  =                 
                                  RECE41-KINGAKU2(6)  +                 
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF  SRY-SRYCD(IDX3) =  "120002470"
                                          OR "120002570"
                                          OR "120003270"
                                          OR "120003470"
                                          OR "120003570"
                                          OR "120003970"
                                          OR "120004270"
                                          OR "120004570"
                         COMPUTE  WRK-SYOSEN-KAISU  =  WRK-SYOSEN-KAISU
                                                    +  ACCT-ZAIKAISU
                      END-IF
                      IF   SRY-SRYCD(IDX3)    =    "120002710"    OR
                                                   "120002810"    OR
                                                   "120002910"    OR
                                                   "120003010"    OR
                                                   "120003710"    OR
                                                   "120002470"    OR
                                                   "120002570"    OR
                                                   "120003270"    OR
                                                   "120003470"    OR
                                                   "120003570"    OR
                                                   "120003970"    OR
                                                   "120004270"    OR
                                                   "120004570"    OR
                                                   "120004610"
                          ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(33)
                          COMPUTE   RECE41-TENSU(33)  =
                                    RECE41-TENSU(33)  +
                                   (WK-ZAITEN  *   ACCT-ZAIKAISU)
                      ELSE
                          EVALUATE   WRK-PARA-JIBAIPRTKBN
                            WHEN      "0"
                             COMPUTE   RECE41-TENSU(25)  =
                                       RECE41-TENSU(25)  +
                                      (WK-SYUTEN   *   ACCT-ZAIKAISU)
                            WHEN      "1"
                             COMPUTE   RECE41-TENSU(34)  =
                                       RECE41-TENSU(34)  +
                                      (WK-SYUTEN   *   ACCT-ZAIKAISU)
                          END-EVALUATE
                          IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                            COMPUTE   RECE41-TENSU(6)  =
                                      RECE41-TENSU(6)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            EVALUATE  WRK-PARA-JIBAIPRTKBN
                             WHEN      "0"
                              COMPUTE   RECE41-TENSU(25)  =
                                        RECE41-TENSU(25)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                             WHEN      "1"
                              COMPUTE   RECE41-TENSU(34)  =
                                        RECE41-TENSU(34)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            END-EVALUATE
                          END-IF
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
                          COMPUTE   RECE41-TENSU-YAK(26)  =
                                    RECE41-TENSU-YAK(26)  +
                                   (WK-YKZTEN   *   ACCT-ZAIKAISU)
                       WHEN      "1"
                          COMPUTE   RECE41-TENSU-YAK(35)  =
                                    RECE41-TENSU-YAK(35)  +
                                   (WK-YKZTEN   *   ACCT-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                 END-IF
                END-IF
           END-EVALUATE
      *
           .
      *
       2101223-RECE-LEFT-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101224-RECE-LEFT-SET4-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                 IF   (RECE41-HKNKBN    =  "4")
                 AND  (RECE41-JIBAIKBN  =  "1")
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)    =
                                RECE41-TENSU(1)    +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     COMPUTE    TBL-SYUTEN-JI(1)   =
                                TBL-SYUTEN-JI(1)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  "111000370"   OR
                                             "111011470"   OR
                                             "111011970"   OR
                                             "111012070"   OR
                                             "111012170"   OR
                                             "111012270"   OR
                                             "111011570"   OR
                                             "111011670"   OR
                                             "111011770"
                             MOVE  "1"     TO   RECE41-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"
      ****              COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                         RECE41-SONOTAKIN-JYU  +
      ****                     (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                               (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                   END-IF
                   IF  (SRY-SRYCD(IDX3) = "101110030" )
                   AND (RECE41-SRYYM   <=   "201306"  )
      ****              COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                         RECE41-SONOTAKIN-JYU  +
      ****                     (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                               (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     IF  SRY-SRYCD(IDX3)  =  "111000370"   OR
                                             "111011470"   OR
                                             "111011970"   OR
                                             "111012070"   OR
                                             "111012170"   OR
                                             "111012270"   OR
                                             "111011570"   OR
                                             "111011670"   OR
                                             "111011770"
                             MOVE  "1"     TO   RECE41-SHOSHIN-NYU
                     END-IF
                   END-IF
                   COMPUTE    RECE41-TENSU(1)    =
                              RECE41-TENSU(1)    +
                              (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   COMPUTE    TBL-SYUTEN-JI(1)   =
                              TBL-SYUTEN-JI(1)   +
                              (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                 END-IF
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 IF   (RECE41-HKNKBN    =  "4")
                 AND  (RECE41-JIBAIKBN  =  "1")
                   EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                    WHEN  120
                     IF ((SRY-SRYCD(IDX3) =  "101120040"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120100"  ) AND
                         (RECE41-SRYYM   <=    "201306"   ))
                     OR ((SRY-SRYCD(IDX3) =  "101120010"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120070"   OR
                                             "101120080"   OR
                                             "101120040"  ) AND
                         (RECE41-SRYYM   >=    "201307"   ))
      ****               ADD   WK-ZAIKAISU      TO   RECE41-KAISU(2)
      ****               COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                          RECE41-SONOTAKIN-JYU  +
      ****                     (WRK-TNS-TEN(IDX3)    * ACCT-ZAIKAISU)
                         COMPUTE    WRK-KINGAKU-JI   =
                                    WRK-KINGAKU-JI   +
                               (WRK-TNS-TEN(IDX3)    * ACCT-ZAIKAISU)
                     ELSE
                      IF  (SRY-SRYCD(IDX3) =  "112016070"
                                           OR "112015670"
                                           OR "112016170"
                                           OR "112015770"
                                           OR "112017270"
                                           OR "112017570"
                                           OR "112021770"
                                           OR "112021870"
                                           OR "112021970"
                                           OR "112022070"
                                           OR "112022870"         )
                      AND (RECE41-SRYYM   >=  "201204"            )
                         COMPUTE    RECE41-TENSU(8)   =
                                    RECE41-TENSU(8)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(8)  =
                                    TBL-SYUTEN-JI(8)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      ELSE
      **                 IF ( SRY-SRYCD(IDX3)     =   "112007170"  OR
      **                                              "112702270"   )
      **                 OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"  )
      **                 OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1"  )
      **                 OR ( SRY-SRYCD(IDX3) = "112021970"         )
      **                     CONTINUE
      **                 ELSE
                             IF   FLG-COUNT-ARI  =  ZERO
                               ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                               MOVE  1            TO  FLG-COUNT-ARI
                             END-IF
      **                 END-IF
                         IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(2)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                          ELSE
                            IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                            END-IF
                          END-IF
                         END-IF
                         COMPUTE    RECE41-TENSU(2)   =
                                    RECE41-TENSU(2)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2)  =
                                    TBL-SYUTEN-JI(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      END-IF
                     END-IF
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                    WHEN  122
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(3)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                          ELSE
                            IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(3)
                              MOVE   1       TO    FLG-KHNTEN3-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE  RECE41-TENSU(3)   =
                                 RECE41-TENSU(3)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(3)  =
                                 TBL-SYUTEN-JI(3)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                    WHEN  123
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(4)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(4)
                          ELSE
                            IF RECE41-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(4)
                              MOVE   1       TO    FLG-KHNTEN4-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(4)
                        COMPUTE  RECE41-TENSU(4)   =
                                 RECE41-TENSU(4)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(4)  =
                                 TBL-SYUTEN-JI(4)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                    WHEN  124
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(5)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                          ELSE
                            IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(5)
                              MOVE   1       TO    FLG-KHNTEN5-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5)   =
                                 RECE41-TENSU(5)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(5)  =
                                 TBL-SYUTEN-JI(5)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê¿¼Ìë¡Ë
                    WHEN  125
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(6)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                          ELSE
                            IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(6)
                              MOVE   1       TO    FLG-KHNTEN6-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6)   =
                                 RECE41-TENSU(6)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(6)  =
                                 TBL-SYUTEN-JI(6)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê¤½¤ÎÂ¾¡Ë
                    WHEN  OTHER
                        IF  (SRY-SRYCD(IDX3) =  "101120030" )
                        AND (RECE41-SRYYM   <=  "201306"    )
      ****                 COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                            RECE41-SONOTAKIN-JYU  +
      ****                       (WRK-TNS-TEN(IDX3)   *  ACCT-ZAIKAISU)
                           COMPUTE    WRK-KINGAKU-JI  =
                                      WRK-KINGAKU-JI  +
                                 (WRK-TNS-TEN(IDX3)   *  ACCT-ZAIKAISU)
                        END-IF
                        IF   ACCT-ZAIKBN     =  2
                           IF  WRK-PARA-JIBAIPRTKBN = "1"
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                           END-IF
                           COMPUTE    WRK-RECE4G-TENSU(2)  =
                                      WRK-RECE4G-TENSU(2)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                   END-EVALUATE
                 ELSE
                   EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                    WHEN  120
                      IF    SRY-SRYCD(IDX3) =  "112015810"        OR
                                               "112017010"        OR
                                               "112015950"        OR
                                               "112017150"        OR
                                               "112023550"        OR
                                               "112023850"        OR
                                               "112016210"        OR
                                               "112016410"        OR
                                               "112017610"
                        IF ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0" )
                        OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1" )
                        OR ( SRY-SRYCD(IDX3) = "112021970"        )
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE41-KAISU(2)
                        END-IF
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(2)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                          ELSE
                            IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                            END-IF
                          END-IF
                        END-IF
                        COMPUTE    RECE41-TENSU(2)   =
                                   RECE41-TENSU(2)   +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE    TBL-SYUTEN-JI(2)  =
                                   TBL-SYUTEN-JI(2)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      ELSE
                       IF  (SRY-SRYCD(IDX3) =  "112016070"
                                            OR "112015670"
                                            OR "112016170"
                                            OR "112015770"
                                            OR "112017270"
                                            OR "112017570"
                                            OR "112021770"
                                            OR "112021870"
                                            OR "112021970"
                                            OR "112022070"
                                            OR "112022870"         )
                       AND (RECE41-SRYYM   >=  "201204"            )
                        COMPUTE    RECE41-TENSU(8)   =
                                   RECE41-TENSU(8)   +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE    TBL-SYUTEN-JI(8)  =
                                   TBL-SYUTEN-JI(8)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       ELSE
                        IF   RECE41-SRYYM  >= "200604"
                          IF  SRY-SRYCD(IDX3)  =  "112000970" OR
                                                  "112006270"
                            COMPUTE  CNT-NYUYOJI2 =  CNT-NYUYOJI2
                                                  +  WK-ZAIKAISU
                          END-IF
                        END-IF
                        IF ( SRY-SRYCD(IDX3)     =   "112007170"  OR
                                                     "112702270"   )
                        OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"  )
                        OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1"  )
                        OR ( SRY-SRYCD(IDX3) = "112021970"         )
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE41-KAISU(2)
                            ADD   WK-ZAIKAISU  TO   CNT-SAISIN-ETC
                        END-IF
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(2)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                          ELSE
                            IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                            END-IF
                          END-IF
                        END-IF
                        COMPUTE    RECE41-TENSU(2)   =
                                   RECE41-TENSU(2)   +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE    TBL-SYUTEN-JI(2)  =
                                   TBL-SYUTEN-JI(2)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                      END-IF
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                    WHEN  122
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(3)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                          ELSE
                            IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(3)
                              MOVE   1       TO    FLG-KHNTEN3-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE  RECE41-TENSU(3)   =
                                 RECE41-TENSU(3)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(3)  =
                                 TBL-SYUTEN-JI(3)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                    WHEN  123
                        PERFORM   21012241-NYUYOJI-COUNT-SEC
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(4)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(4)
                          ELSE
                            IF RECE41-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(4)
                              MOVE   1       TO    FLG-KHNTEN4-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(4)
                        COMPUTE  RECE41-TENSU(4)   =
                                 RECE41-TENSU(4)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(4)  =
                                 TBL-SYUTEN-JI(4)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                    WHEN  124
                        PERFORM   21012241-NYUYOJI-COUNT-SEC
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(5)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                          ELSE
                            IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(5)
                              MOVE   1       TO    FLG-KHNTEN5-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5)   =
                                 RECE41-TENSU(5)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(5)  =
                                 TBL-SYUTEN-JI(5)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê¿¼Ìë¡Ë
                    WHEN  125
                        PERFORM   21012241-NYUYOJI-COUNT-SEC
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(6)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                          ELSE
                            IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(6)
                              MOVE   1       TO    FLG-KHNTEN6-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6)   =
                                 RECE41-TENSU(6)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(6)  =
                                 TBL-SYUTEN-JI(6)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê¤½¤ÎÂ¾¡Ë
                    WHEN  OTHER
                        IF   ACCT-ZAIKBN     =  2
                           IF  WRK-PARA-JIBAIPRTKBN = "1"
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                           END-IF
                           COMPUTE    WRK-RECE4G-TENSU(2)  =
                                      WRK-RECE4G-TENSU(2)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                   END-EVALUATE
                 END-IF
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                 IF   (RECE41-HKNKBN    =  "4")
                 AND  (RECE41-JIBAIKBN  =  "1")
                     IF   (SRY-SRYCD(IDX3) =  "101130190"  )
                     AND  (RECE41-SRYYM   >=  "201307"     )
                             COMPUTE    WRK-KINGAKU-JI  =
                                        WRK-KINGAKU-JI  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     ELSE
                       IF ((SRY-SRYCD(IDX3) =
                            "101130020"   OR  "101130030"   OR
                            "101130040"   OR  "101130050"   OR
                            "101130060"   OR  "101130070"   OR
                            "101130080"   OR  "101130090"   OR
                            "101130100"                         ) AND
                           (RECE41-SRYYM    <=  "201306"        ))
                       OR ((SRY-SRYCD(IDX3)  =  "101130010"     ) AND
                           (RECE41-SRYYM     <  "200804"        ))
                             COMPUTE    RECE41-SONOTAKIN-JYU  =
                                        RECE41-SONOTAKIN-JYU  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       ELSE
                         IF   ACCT-ZAIKBN     =  2
                             COMPUTE    WRK-RECE4G-TENSU(7)  =
                                        WRK-RECE4G-TENSU(7)  +
                                       (WK-ZAITEN   *   WK-ZAIKAISU)
                         ELSE
                             COMPUTE    RECE41-TENSU(7)   =
                                        RECE41-TENSU(7)   +
                                       (WK-ZAITEN   *   WK-ZAIKAISU)
                             COMPUTE    TBL-SYUTEN-JI(7)  =
                                        TBL-SYUTEN-JI(7)  +
                                       (WK-ZAITEN   *   WK-ZAIKAISU)
                         END-IF
                       END-IF
                     END-IF
                 ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE4G-TENSU(7)  =
                                    WRK-RECE4G-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)   =
                                    RECE41-TENSU(7)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(7)  =
                                    TBL-SYUTEN-JI(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                 END-IF
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE4G-TENSU(8)  =
                              WRK-RECE4G-TENSU(8)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029270")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030170")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030270")))
                   OR (  FLG-OSINKBN           =     9           )
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE41-TENSU(8)   =
                             RECE41-TENSU(8)   +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(8)  =
                             TBL-YKZTEN-JI(8)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010002"
                                            OR "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(10)  =
                                 TBL-YKZTEN-JI(10)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(10)  =
                                 TBL-YKZTEN-JI(10)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    "099209901"
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(10)  =
                                 TBL-YKZTEN-JI(10)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(11)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(11)
                       ELSE
                          IF   RECE41-KHNTEN(11) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(11)
                               MOVE   1       TO    FLG-KHNTEN11-JI2
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)   =
                                 RECE41-TENSU(11)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(11)  =
                                 TBL-SYUTEN-JI(11)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                 IF    SRY-SRYCD(IDX3)      =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                   COMPUTE   RECE41-TENSU(12)   =
                             RECE41-TENSU(12)   -
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(12)  =
                             TBL-YKZTEN-JI(12)  -
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(12)
                   COMPUTE   RECE41-TENSU(12)   =
                             RECE41-TENSU(12)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(12)  =
                             TBL-YKZTEN-JI(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(13)   =
                                 RECE41-TENSU(13)   -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(13)  =
                                 TBL-YKZTEN-JI(13)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(13)
                       COMPUTE   RECE41-TENSU(13)   =
                                 RECE41-TENSU(13)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(13)  =
                                 TBL-YKZTEN-JI(13)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                       IF    RECE41-KHNTEN(14)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(14)
                       ELSE
                          IF   RECE41-KHNTEN(14) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(14)
                               MOVE   1       TO    FLG-KHNTEN14-JI2
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)   =
                                 RECE41-TENSU(14)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(14)  =
                                 TBL-SYUTEN-JI(14)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF  SRY-SRYCD(IDX3) = "120002170" OR "120002270" OR
                                         "120003170" OR "120003370" OR
                                         "120003870" OR
                                         "120004070" OR "120004170" OR
                                         "120004370" OR "120004470"
                      COMPUTE  WRK-SYOHOU-KAISU  =  WRK-SYOHOU-KAISU
                                                 +  ACCT-ZAIKAISU
                   END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(15)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(15)
                     ELSE
                      IF   RECE41-KHNTEN(15) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(15)
                            MOVE   1      TO   FLG-KHNTEN15-JI2
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    ACCT-ZAIKAISU   TO    RECE41-KAISU(15)
                   END-IF
                   COMPUTE   RECE41-TENSU(15)   =
                             RECE41-TENSU(15)   +
                            (WK-ZAITEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(15)  =
                             TBL-SYUTEN-JI(15)  +
                            (WK-ZAITEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(16)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(16)   =
                             RECE41-TENSU(16)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(16)  =
                             TBL-SYUTEN-JI(16)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(17)
                   END-IF
                   COMPUTE   RECE41-TENSU(17)   =
                             RECE41-TENSU(17)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(17)  =
                             TBL-SYUTEN-JI(17)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(19)
                   COMPUTE   RECE41-TENSU(19)   =
                             RECE41-TENSU(19)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(19)  =
                             TBL-SYUTEN-JI(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(19)   =
                             RECE41-TENSU(19)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(19)  =
                             TBL-YKZTEN-JI(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(20)
                   COMPUTE   RECE41-TENSU(20)   =
                             RECE41-TENSU(20)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(20)  =
                             TBL-SYUTEN-JI(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(20)   =
                             RECE41-TENSU(20)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(20)  =
                             TBL-YKZTEN-JI(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(21)
                   COMPUTE   RECE41-TENSU(21)   =
                             RECE41-TENSU(21)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(21)  =
                             TBL-SYUTEN-JI(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(21)   =
                             RECE41-TENSU(21)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(21)  =
                             TBL-YKZTEN-JI(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(23)
                   END-IF
                   COMPUTE   RECE41-TENSU(23)   =
                             RECE41-TENSU(23)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(23)  =
                             TBL-SYUTEN-JI(23)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(24)   =
                             RECE41-TENSU(24)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(24)  =
                             TBL-YKZTEN-JI(24)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(26)
                   END-IF
                   COMPUTE   RECE41-TENSU(26)   =
                             RECE41-TENSU(26)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(26)  =
                             TBL-SYUTEN-JI(26)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(27)   =
                             RECE41-TENSU(27)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(27)  =
                             TBL-YKZTEN-JI(27)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)   =
                             RECE41-TENSU(29)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(29)  =
                             TBL-SYUTEN-JI(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   "160101210"     OR
                                                  "160095710"     OR
                                                  "160095810"
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(30)   =
                             RECE41-TENSU(30)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(30)  =
                             TBL-YKZTEN-JI(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(32)
                   END-IF
                   COMPUTE   RECE41-TENSU(32)   =
                             RECE41-TENSU(32)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(32)  =
                             TBL-SYUTEN-JI(32)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(33)   =
                             RECE41-TENSU(33)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(33)  =
                             TBL-YKZTEN-JI(33)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                 IF   ((SRY-SRYCD(IDX3) =
                        "101800890"   OR
                        "101800870"   OR  "101800470"   OR
                        "101800480"   OR  "101800490"   OR
                        "101800500"   OR  "101800510"   OR
                        "101800520"   OR  "101800530"   OR
                        "101800540"   OR  "101800960"   OR
                        "101800970"   OR  "101800980"   OR
                        "101801040"                         ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = "101800890"
                             COMPUTE  WRK-KINGAKU-JI   =
                                      WRK-KINGAKU-JI   +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                             COMPUTE  RECE41-SONOTAKIN-JYU  =
                                      RECE41-SONOTAKIN-JYU  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                        CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"    )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593" ))
                    OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                         <= "101801020"  )  AND
                        (RECE41-SRYYM    >= "201307"     ))
                    OR ((SRY-SRYCD(IDX3)  = "101801030"  )  AND
                        (RECE41-SRYYM    >= "201307"     ))
                     IF   SRY-SRYCD(IDX3)(1:5) = "09593"
                       IF   (RECE41-HKNKBN    =  "4")
                       AND  (RECE41-JIBAIKBN  =  "1")
                         EVALUATE  WRK-TNS-JIBAISYUKEIKBN(IDX3)
                           WHEN       0
                           WHEN       5
                             COMPUTE  RECE41-SONOTAKIN-JYU  =
                                      RECE41-SONOTAKIN-JYU  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                           WHEN       1
                           WHEN       2
                           WHEN       3
                           WHEN       4
                             COMPUTE  WRK-KINGAKU-JI   =
                                      WRK-KINGAKU-JI   +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         END-EVALUATE
                       ELSE
                             COMPUTE  RECE41-SONOTAKIN-JYU  =
                                      RECE41-SONOTAKIN-JYU  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                     ELSE
                       IF  SRY-SRYCD(IDX3)  =  "101801030"
                             COMPUTE  WRK-FUTANWARI  =
                                      WRK-TNS-TEN(IDX3) / 100
                             COMPUTE  WRK-KEISAN-KG2 =
                                     (WRK-TEN-KG2       * WRK-FUTANWARI)
                             COMPUTE  RECE41-SONOTAKIN-JYU  =
                                      RECE41-SONOTAKIN-JYU  +
                                     (WRK-KEISAN-KG2    * ACCT-ZAIKAISU)
                       ELSE
                             COMPUTE  RECE41-SONOTAKIN-JYU  =
                                      RECE41-SONOTAKIN-JYU  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                             COMPUTE  WRK-TEN-KG2 = WRK-TNS-TEN(IDX3)
                       END-IF
                     END-IF
                    ELSE                                                
                     IF  RECE41-HKNKBN        =    "4"
                     AND RECE41-JIBAIKBN      =    "1"
                     AND SRY-SRYCD(IDX3)      =    "199000610"
                       COMPUTE    RECE41-SONOTAKIN-JYU  =               
                                  RECE41-SONOTAKIN-JYU  +               
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF  SRY-SRYCD(IDX3) =  "120002470"
                                          OR "120002570"
                                          OR "120003270"
                                          OR "120003470"
                                          OR "120003570"
                                          OR "120003970"
                                          OR "120004270"
                                          OR "120004570"
                         COMPUTE  WRK-SYOSEN-KAISU  =  WRK-SYOSEN-KAISU
                                                    +  ACCT-ZAIKAISU
                      END-IF
                      IF   SRY-SRYCD(IDX3)    =    "120002710"    OR
                                                   "120002810"    OR
                                                   "120002910"    OR
                                                   "120003010"    OR
                                                   "120003710"    OR
                                                   "120002470"    OR
                                                   "120002570"    OR
                                                   "120003270"    OR
                                                   "120003470"    OR
                                                   "120003570"    OR
                                                   "120003970"    OR
                                                   "120004270"    OR
                                                   "120004570"    OR
                                                   "120004610"
                          ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(35)
                          COMPUTE   RECE41-TENSU(35)   =
                                    RECE41-TENSU(35)   +
                                   (WK-ZAITEN  *   ACCT-ZAIKAISU)
                          COMPUTE   TBL-SYUTEN-JI(35)  =
                                    TBL-SYUTEN-JI(35)  +
                                   (WK-ZAITEN  *   ACCT-ZAIKAISU)
                      ELSE
                          EVALUATE   WRK-PARA-JIBAIPRTKBN
                           WHEN      "0"
                             COMPUTE   RECE41-TENSU(23)   =
                                       RECE41-TENSU(23)   +
                                      (WK-SYUTEN  *   ACCT-ZAIKAISU)
                             COMPUTE   TBL-SYUTEN-JI(23)  =
                                       TBL-SYUTEN-JI(23)  +
                                      (WK-SYUTEN  *   ACCT-ZAIKAISU)
                           WHEN      "1"
                             COMPUTE   RECE41-TENSU(36)   =
                                       RECE41-TENSU(36)   +
                                      (WK-SYUTEN  *   ACCT-ZAIKAISU)
                             COMPUTE   TBL-SYUTEN-JI(36)  =
                                       TBL-SYUTEN-JI(36)  +
                                      (WK-SYUTEN  *   ACCT-ZAIKAISU)
                          END-EVALUATE
                          IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                            COMPUTE   RECE41-TENSU(7)   =
                                      RECE41-TENSU(7)   +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(7)  =
                                      TBL-SYUTEN-JI(7)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            EVALUATE  WRK-PARA-JIBAIPRTKBN
                             WHEN      "0"
                              COMPUTE   RECE41-TENSU(23)   =
                                        RECE41-TENSU(23)   -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(23)  =
                                        TBL-SYUTEN-JI(23)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                             WHEN      "1"
                              COMPUTE   RECE41-TENSU(36)   =
                                        RECE41-TENSU(36)   -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(36)  =
                                        TBL-SYUTEN-JI(36)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            END-EVALUATE
                          END-IF
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
                        IF  SRY-SRYCD(IDX3) = "199000610"
                         COMPUTE   RECE41-TENSU(37)   =
                                   RECE41-TENSU(37)   +
                                  (WK-YKZTEN   *   ACCT-ZAIKAISU)
                         COMPUTE   TBL-YKZTEN-JI(37)  =
                                   TBL-YKZTEN-JI(37)  +
                                  (WK-YKZTEN   *   ACCT-ZAIKAISU)
                        ELSE
                         COMPUTE   RECE41-TENSU(24)   =
                                   RECE41-TENSU(24)   +
                                  (WK-YKZTEN   *   ACCT-ZAIKAISU)
                         COMPUTE   TBL-YKZTEN-JI(24)  =
                                   TBL-YKZTEN-JI(24)  +
                                  (WK-YKZTEN   *   ACCT-ZAIKAISU)
                        END-IF
                       WHEN      "1"
                         COMPUTE   RECE41-TENSU(37)   =
                                   RECE41-TENSU(37)   +
                                  (WK-YKZTEN   *   ACCT-ZAIKAISU)
                         COMPUTE   TBL-YKZTEN-JI(37)  =
                                   TBL-YKZTEN-JI(37)  +
                                  (WK-YKZTEN   *   ACCT-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                 END-IF
           END-EVALUATE
      *
           .
      *
       2101224-RECE-LEFT-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆýÍÄ»ù²Ã»»¤Î¥«¥¦¥ó¥È
      *****************************************************************
       21012241-NYUYOJI-COUNT-SEC               SECTION.
      *
           IF   RECE41-SRYYM  >= "200604"
              IF  SRY-SRYCD(IDX3) = "112015170" OR
                                    "112015270" OR
                                    "112015370" OR
                                    "112015470" OR
                                    "112014470" OR
                                    "112014570" OR
                                    "112014670" OR
                                    "112014770" OR
                                    "112014870" OR
                                    "112014970" OR
                                    "112015070" OR
                                    "112014170" OR
                                    "112014270" OR
                                    "112014370"
                     COMPUTE  CNT-NYUYOJI2  =  CNT-NYUYOJI2
                                            +  WK-ZAIKAISU
              END-IF
           END-IF
           .
      *
       21012241-NYUYOJI-COUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101225-RECE-LEFT-SET5-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF ( SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"   )
                   OR ((SRY-SRYCD(IDX3) = "101110030"  ) AND
                       (RECE41-SRYYM   <=   "201306"   ))
                      CONTINUE
                   ELSE
                      IF   WRK-TNS-DATAKBN(IDX3)  =   2
                        EVALUATE  WRK-TNS-TIMEKSNKBN(IDX3)
                          WHEN    1
                          WHEN    5
                          WHEN    6
                              MOVE  "1"     TO   RECE41-SHOSHINTIME
                          WHEN    3
                              MOVE  "2"     TO   RECE41-SHOSHINTIME
                          WHEN    4
                              MOVE  "3"     TO   RECE41-SHOSHINTIME
                        END-EVALUATE
                      END-IF
                      IF    RECE41-KOMUKBN  =   "2"
                        IF  WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"
                        OR  SRY-SRYCD(IDX3)(1:1)       NOT = "1"
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE41-KAISU(1)
                        END-IF
                      ELSE
                        IF  WRK-TNS-DATAKBN(IDX3)  =   2
                          IF   SRY-SRYCD(IDX3) = "111012370"
                            CONTINUE
                          ELSE
                            ADD   WK-ZAIKAISU  TO   RECE41-KAISU(1)
                          END-IF
                        END-IF
                      END-IF
                      COMPUTE    RECE41-TENSU(1)    =
                                 RECE41-TENSU(1)    +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                   EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                    WHEN  120
                     IF ((SRY-SRYCD(IDX3) =  "101120040"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120100"  ) AND
                         (RECE41-SRYYM   <=    "201306"   ))
                     OR ((SRY-SRYCD(IDX3) =  "101120010"   OR
                                             "101120050"   OR
                                             "101120060"   OR
                                             "101120070"   OR
                                             "101120080"   OR
                                             "101120040"  ) AND
                         (RECE41-SRYYM   >=    "201307"   ))
                         CONTINUE
                     ELSE
                       IF SRY-SRYCD(IDX3) =  "112011310"
                                          OR "112011710" OR "112016310"
                                          OR "112016550" OR "112017310"
                                          OR "112017450" 
                                          OR "112016210" OR "112016410"
                                          OR "112017610" OR "112022870"
                        IF    RECE41-SRYYM  >= "201804"
                           PERFORM  21012251-RECE-LEFT12-HEN1-SEC
                        ELSE
                         IF    RECE41-KHNTEN(3)   =   ZERO
                           MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                         ELSE
                           IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO    TO    RECE41-KHNTEN(3)
                             MOVE   1       TO    FLG-KHNTEN3-KO
                           END-IF
                         END-IF
                         IF  WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"
                         OR  SRY-SRYCD(IDX3)(1:1)       NOT = "1"
                             CONTINUE
                         ELSE
                             ADD   WK-ZAIKAISU  TO   RECE41-KAISU(3)
                         END-IF
                         COMPUTE    RECE41-TENSU(3)   =
                                    RECE41-TENSU(3)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                       ELSE
                        IF    RECE41-SRYYM  >= "201804"
                           PERFORM  21012251-RECE-LEFT12-HEN2-SEC
                        ELSE
                         IF     RECE41-KOMUKBN  =   "2"
                           IF ( SRY-SRYCD(IDX3)   =   "112007170"  OR
                                                      "112702270"    )
                           OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0" )
                           OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1" )
                             CONTINUE
                           ELSE
                             ADD    WK-ZAIKAISU  TO  RECE41-KAISU(2)
                             IF  SRY-SRYCD(IDX3)  =  "112015810"
                                                 OR  "112017010"
                                                 OR  "112015950"
                                                 OR  "112017150"
                               ADD  WK-ZAIKAISU  TO  CNT-SAISIN-DOUJITU
                             ELSE
                               ADD  WK-ZAIKAISU  TO  CNT-SAISIN-ETC
                             END-IF
                           END-IF
                         ELSE
                           IF   FLG-COUNT-ARI  =  ZERO
                             ADD    WK-ZAIKAISU  TO  RECE41-KAISU(2)
                             MOVE   1            TO  FLG-COUNT-ARI
                           END-IF
                         END-IF
                         IF ( SRY-SRYCD(IDX3)     =   "112007170"  OR
                                                      "112702270" )
                           MOVE   ZERO    TO    RECE41-KHNTEN(2)
                           MOVE   1       TO    FLG-KHNTEN2-KO
                         ELSE
                           IF  (SRY-SRYCD(IDX3) =  "112016070"     OR
                                                   "112015670"     OR
                                                   "112016170"     OR
                                                   "112015770"     OR
                                                   "112017270"     OR
                                                   "112017570"     OR
                                                   "101989030"     OR
                                                   "101989070"       )
                           AND (RECE41-SRYYM   >=  "201004"          )
                             IF  SRY-SRYCD(IDX3)  =  "112016070"
                               COMPUTE  CNT-JIKANTAI1 = CNT-JIKANTAI1
                                                      + WK-ZAIKAISU
                               IF   FLG-JIKANTAI1-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-JIKANTAI1-ARI
                               END-IF
                             END-IF
                             IF  SRY-SRYCD(IDX3)  =  "112015670"
                               COMPUTE  CNT-JIKANTAI2 = CNT-JIKANTAI2
                                                      + WK-ZAIKAISU
                               IF   FLG-JIKANTAI2-ARI   =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-JIKANTAI2-ARI
                               END-IF
                             END-IF
                             IF  SRY-SRYCD(IDX3)  =  "112016170"
                               COMPUTE  CNT-JIKANTAI3 = CNT-JIKANTAI3
                                                      + WK-ZAIKAISU
                               IF   FLG-JIKANTAI3-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-JIKANTAI3-ARI
                               END-IF
                             END-IF
                             IF  SRY-SRYCD(IDX3)  =  "112015770"
                               COMPUTE  CNT-MESAIKSN =  CNT-MESAIKSN
                                                     +  WK-ZAIKAISU
                               IF   FLG-MESAIKSN-ARI   =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-MESAIKSN-ARI
                               END-IF
                             END-IF
                             IF  SRY-SRYCD(IDX3)  =  "112017270"
                               COMPUTE  CNT-CHIHOKATU = CNT-CHIHOKATU
                                                      + WK-ZAIKAISU
                               IF   FLG-CHIHOKATU-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-CHIHOKATU-ARI
                               END-IF
                             END-IF
                             IF  SRY-SRYCD(IDX3)  =  "112017570"
                               COMPUTE  CNT-CHIHOKAT2 = CNT-CHIHOKAT2
                                                      + WK-ZAIKAISU
                               IF   FLG-CHIHOKAT2-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-CHIHOKAT2-ARI
                               END-IF
                             END-IF
                             IF  SRY-SRYCD(IDX3)  =  "101989030"
                               COMPUTE  CNT-SAITOKKSN = CNT-SAITOKKSN
                                                      + WK-ZAIKAISU
                               IF   FLG-SAITOKKSN-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-SAITOKKSN-ARI
                               END-IF
                             END-IF
                             IF  SRY-SRYCD(IDX3)  =  "101989070"
                               COMPUTE  CNT-SAITOKKSN2
                                                      = CNT-SAITOKKSN2
                                                      + WK-ZAIKAISU
                               IF   FLG-SAITOKKSN-ARI2 =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-SAITOKKSN-ARI2
                               END-IF
                             END-IF
                           ELSE
                             IF   FLG-SAISIN-ARI   =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                 TO  RECE41-KHNTEN(2)
                                  MOVE     1     TO  FLG-SAISIN-ARI
                             END-IF
                           END-IF
                         END-IF
                         COMPUTE    RECE41-TENSU(2)   =
                                    RECE41-TENSU(2)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                       END-IF
                     END-IF
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                    WHEN  122
                        IF    RECE41-KHNTEN(5)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                        ELSE
                          IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(5)
                            MOVE   1       TO    FLG-KHNTEN5-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5)   =
                                 RECE41-TENSU(5)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                    WHEN  123
                        IF    RECE41-KHNTEN(6)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                        ELSE
                          IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(6)
                            MOVE   1       TO    FLG-KHNTEN6-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6)   =
                                 RECE41-TENSU(6)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                    WHEN  124
                        IF    RECE41-KHNTEN(7)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(7)
                        ELSE
                          IF RECE41-KHNTEN(7) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(7)
                            MOVE   1       TO    FLG-KHNTEN7-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(7)
                        COMPUTE  RECE41-TENSU(7)   =
                                 RECE41-TENSU(7)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê¿¼Ìë¡Ë
                    WHEN  125
                        IF    RECE41-KHNTEN(8)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(8)
                        ELSE
                          IF RECE41-KHNTEN(8) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(8)
                            MOVE   1       TO    FLG-KHNTEN8-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(8)
                        COMPUTE  RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê¤½¤ÎÂ¾¡Ë
                    WHEN  OTHER 
                        IF   ACCT-ZAIKBN     =  2
                           MOVE   ZERO    TO    RECE41-KHNTEN(3)
                           MOVE   1       TO    FLG-KHNTEN3-KO
                           COMPUTE    WRK-RECE4G-TENSU(3)  =
                                      WRK-RECE4G-TENSU(3)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                   END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF ((SRY-SRYCD(IDX3) =
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"                         ) AND
                       (RECE41-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX3)  =  "101130010"     ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                   OR ((SRY-SRYCD(IDX3)  =  "101130190"     ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                         CONTINUE
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE4G-TENSU(9)  =
                                    WRK-RECE4G-TENSU(9)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(9)   =
                                    RECE41-TENSU(9)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE4G-TENSU(14)  =
                              WRK-RECE4G-TENSU(14)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)   =
                                 RECE41-TENSU(11)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(12)
                       COMPUTE   RECE41-TENSU(12)   =
                                 RECE41-TENSU(12)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029270")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114029970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030170")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114030270")))
                   OR (  FLG-OSINKBN           =     9           )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(13)
                       COMPUTE   RECE41-TENSU(13)   =
                                 RECE41-TENSU(13)   +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(14)   =
                                 RECE41-TENSU(14)   +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE41-TENSU(15)   =
                             RECE41-TENSU(15)   +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010002"
                                            OR "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(16)   =
                                 RECE41-TENSU(16)   -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(16)
                       COMPUTE   RECE41-TENSU(16)   =
                                 RECE41-TENSU(16)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    "099209901"
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(16)
                       COMPUTE   RECE41-TENSU(16)   =
                                 RECE41-TENSU(16)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(17)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(17)
                       ELSE
                          IF   RECE41-KHNTEN(17) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(17)
                               MOVE   1       TO    FLG-KHNTEN17-KO
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(17)
                       COMPUTE   RECE41-TENSU(17)   =
                                 RECE41-TENSU(17)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                 IF    SRY-SRYCD(IDX3)      =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                   COMPUTE   RECE41-TENSU(18)   =
                             RECE41-TENSU(18)   -
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                   COMPUTE   RECE41-TENSU(18)   =
                             RECE41-TENSU(18)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE41-TENSU(20)   =
                                 RECE41-TENSU(20)   -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(20)
                       COMPUTE   RECE41-TENSU(20)   =
                                 RECE41-TENSU(20)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                       IF    RECE41-KHNTEN(21)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(21)
                       ELSE
                          IF   RECE41-KHNTEN(21) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(21)
                               MOVE   1       TO    FLG-KHNTEN21-KO
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(21)
                       COMPUTE   RECE41-TENSU(21)   =
                                 RECE41-TENSU(21)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF  SRY-SRYCD(IDX3) = "120002170" OR "120002270" OR
                                         "120003170" OR "120003370" OR
                                         "120003870" OR
                                         "120004070" OR "120004170" OR
                                         "120004370" OR "120004470"
                      COMPUTE  WRK-SYOHOU-KAISU  =  WRK-SYOHOU-KAISU
                                                 +  ACCT-ZAIKAISU
                   END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(22)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(22)
                     ELSE
                      IF   RECE41-KHNTEN(22) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(22)
                            MOVE   1      TO   FLG-KHNTEN22-KO
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    ACCT-ZAIKAISU   TO    RECE41-KAISU(22)
                   END-IF
                   COMPUTE   RECE41-TENSU(22)   =
                             RECE41-TENSU(22)   +
                            (WK-ZAITEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(23)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(23)   =
                             RECE41-TENSU(23)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(24)   =
                             RECE41-TENSU(24)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(25)
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(26)
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(27)
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(28)
                   END-IF
                   COMPUTE   RECE41-TENSU(28)   =
                             RECE41-TENSU(28)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(29)   =
                             RECE41-TENSU(29)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(30)
                   END-IF
                   COMPUTE   RECE41-TENSU(30)   =
                             RECE41-TENSU(30)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(31)   =
                             RECE41-TENSU(31)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(32)
                   END-IF
                   COMPUTE   RECE41-TENSU(32)   =
                             RECE41-TENSU(32)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(32)  =
                                   RECE41-KAISU(32)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   "160101210"     OR
                                                  "160095710"     OR
                                                  "160095810"
                         IF   RECE41-KAISU(32)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(32)  =
                                        RECE41-KAISU(32)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(33)   =
                             RECE41-TENSU(33)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(34)
                   END-IF
                   COMPUTE   RECE41-TENSU(34)   =
                             RECE41-TENSU(34)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(35)   =
                             RECE41-TENSU(35)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                 IF   ((SRY-SRYCD(IDX3) =
                        "101800890"   OR
                        "101800870"   OR  "101800470"   OR
                        "101800480"   OR  "101800490"   OR
                        "101800500"   OR  "101800510"   OR
                        "101800520"   OR  "101800530"   OR
                        "101800540"   OR  "101800960"   OR
                        "101800970"   OR  "101800980"   OR
                        "101801040"                         ) AND
                       (RECE41-SRYYM    >=  "201307"        ))
                       CONTINUE
                 ELSE
                   IF ((ACCT-SRYKBN          =  "80"    )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09593" ))
                   OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                        <= "101801020"  )  AND
                       (RECE41-SRYYM    >= "201307"     ))
                   OR ((SRY-SRYCD(IDX3)  = "101801030"  )  AND
                       (RECE41-SRYYM    >= "201307"     ))
                       IF   (FLG-TAISYO           =   1     )
                       AND  (SRY-SRYCD(IDX3)(1:5) =  "09593")
                          COMPUTE WRK-ZAITENNASHI  =
                                  WRK-ZAITENNASHI  +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                   ELSE               
                    IF  SRY-SRYCD(IDX3) =  "120002470"
                                        OR "120002570"
                                        OR "120003270"
                                        OR "120003470"
                                        OR "120003570"
                                        OR "120003970"
                                        OR "120004270"
                                        OR "120004570"
                       COMPUTE  WRK-SYOSEN-KAISU  =  WRK-SYOSEN-KAISU
                                                  +  ACCT-ZAIKAISU
                    END-IF
                    IF   SRY-SRYCD(IDX3)   =  "120002710"    OR
                                              "120002810"    OR
                                              "120002910"    OR
                                              "120003010"    OR
                                              "120003710"    OR
                                              "120002470"    OR
                                              "120002570"    OR
                                              "120003270"    OR
                                              "120003470"    OR
                                              "120003570"    OR
                                              "120003970"    OR
                                              "120004270"    OR
                                              "120004570"    OR
                                              "120004610"
                         ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(36)
                         COMPUTE   RECE41-TENSU(36)  =
                                   RECE41-TENSU(36)  +
                                  (WK-ZAITEN  *   ACCT-ZAIKAISU)
                    ELSE
                         COMPUTE   RECE41-TENSU(37)  =
                                   RECE41-TENSU(37)  +
                                  (WK-SYUTEN  *   ACCT-ZAIKAISU)
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(9)  =
                                     RECE41-TENSU(9)  +
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                           COMPUTE   RECE41-TENSU(37) =
                                     RECE41-TENSU(37) -
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         ELSE
                           IF  SRY-SRYCD(IDX3) = "101989060"
                            COMPUTE  RECE41-TENSU(1)  =
                                     RECE41-TENSU(1)  +
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            COMPUTE  RECE41-TENSU(37) =
                                     RECE41-TENSU(37) -
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                           END-IF
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(38)  =
                              RECE41-TENSU(38)  +
                             (WK-YKZTEN   *   ACCT-ZAIKAISU)
                   END-IF
                 END-IF
           END-EVALUATE
      *
           .
      *
       2101225-RECE-LEFT-SET5-EXT.
           EXIT.
      *
      *****************************************************************
      *
      *****************************************************************
       21012251-RECE-LEFT12-HEN1-SEC               SECTION.
      *
             IF ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0" )
             OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1" )
             OR ( SRY-SRYCD(IDX3) = "112021970"        )
               CONTINUE
             ELSE
               ADD   WK-ZAIKAISU   TO  RECE41-KAISU(3)
               IF    SRY-SRYCD(IDX3)  =  "112016210"
                                     OR  "112016410"
                                     OR  "112017610"
                 ADD  WK-ZAIKAISU  TO  CNT-SAISIN-GAI-DOUJITU
               ELSE
                 ADD  WK-ZAIKAISU  TO  CNT-SAISIN-GAI-ETC
               END-IF
             END-IF
      *
             IF   SRY-SRYCD(IDX3)  =  "112022870"
               COMPUTE  CNT-NINPU2    =  CNT-NINPU2
                                      +  WK-ZAIKAISU
             END-IF
      *
             IF   SRY-SRYCD(IDX3)  =  "112022870"
                IF   FLG-NINPU2-ARI  =  ZERO
                  ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                  MOVE     1             TO FLG-NINPU2-ARI
                END-IF
             ELSE
                IF   FLG-SAISIN-GAI-ARI   =  ZERO
                  ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                  MOVE     1             TO FLG-SAISIN-GAI-ARI
                END-IF
             END-IF
      *
             COMPUTE    RECE41-TENSU(3)   =
                        RECE41-TENSU(3)   +
                       (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *
           .
      *
       21012251-RECE-LEFT12-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *
      *****************************************************************
       21012251-RECE-LEFT12-HEN2-SEC               SECTION.
      *
           IF     RECE41-KOMUKBN  =   "2"
             IF ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0" )
             OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1" )
             OR ( SRY-SRYCD(IDX3) = "112021970"        )
               CONTINUE
             ELSE
               ADD   WK-ZAIKAISU     TO  RECE41-KAISU(2)
               IF    SRY-SRYCD(IDX3)  =  "112015810"
                                     OR  "112017010"
                                     OR  "112015950"
                                     OR  "112017150"
                                     OR  "112023550"
                                     OR  "112023850"
                   ADD  WK-ZAIKAISU  TO  CNT-SAISIN-DOUJITU
               ELSE
                 IF  SRY-SRYCD(IDX3)  =  "112023210"
                   ADD  WK-ZAIKAISU  TO  CNT-SAISIN-ONLINE
                 ELSE
                   ADD  WK-ZAIKAISU  TO  CNT-SAISIN-ETC
                 END-IF
               END-IF
             END-IF
      *
             IF   SRY-SRYCD(IDX3)  =  "112016070"
               COMPUTE  CNT-JIKANTAI1 =  CNT-JIKANTAI1
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112015670"
               COMPUTE  CNT-JIKANTAI2 =  CNT-JIKANTAI2
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112016170"
               COMPUTE  CNT-JIKANTAI3 =  CNT-JIKANTAI3
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112015770"
               COMPUTE  CNT-MESAIKSN  =  CNT-MESAIKSN
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112017270"
               COMPUTE  CNT-CHIHOKATU =  CNT-CHIHOKATU
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112017570"
               COMPUTE  CNT-CHIHOKAT2 =  CNT-CHIHOKAT2
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112021770"
               COMPUTE  CNT-CHIHOKAT3 =  CNT-CHIHOKAT3
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112021870"
               COMPUTE  CNT-CHIHOKAT4 =  CNT-CHIHOKAT4
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112021970"
               COMPUTE  CNT-YAKRENKEI =  CNT-YAKRENKEI
                                      +  WK-ZAIKAISU
             END-IF
             IF   SRY-SRYCD(IDX3)  =  "112022070"
               COMPUTE  CNT-NINPU     =  CNT-NINPU
                                      +  WK-ZAIKAISU
             END-IF
      *
             IF   SRY-SRYCD(IDX3)  =  "112016070"
                                  OR  "112015670"
                                  OR  "112016170"
                                  OR  "112015770"
                                  OR  "112017270"
                                  OR  "112017570"
                                  OR  "112021770"
                                  OR  "112021870"
                                  OR  "112021970"
                                  OR  "112022070"
                EVALUATE  SRY-SRYCD(IDX3)
                  WHEN     "112016070"
                    IF   FLG-JIKANTAI1-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-JIKANTAI1-ARI
                    END-IF
                  WHEN     "112015670"
                    IF   FLG-JIKANTAI2-ARI   =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-JIKANTAI2-ARI
                    END-IF
                  WHEN     "112016170"
                    IF   FLG-JIKANTAI3-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-JIKANTAI3-ARI
                    END-IF
                  WHEN     "112015770"
                    IF   FLG-MESAIKSN-ARI   =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-MESAIKSN-ARI
                    END-IF
                  WHEN     "112017270"
                    IF   FLG-CHIHOKATU-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKATU-ARI
                    END-IF
                  WHEN     "112017570"
                    IF   FLG-CHIHOKAT2-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKAT2-ARI
                    END-IF
                  WHEN     "112021770"
                    IF   FLG-CHIHOKAT3-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKAT3-ARI
                    END-IF
                  WHEN     "112021870"
                    IF   FLG-CHIHOKAT4-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKAT4-ARI
                    END-IF
                  WHEN     "112021970"
                    IF   FLG-YAKRENKEI-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-YAKRENKEI-ARI
                    END-IF
                  WHEN     "112022070"
                    IF   FLG-NINPU-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-NINPU-ARI
                    END-IF
                END-EVALUATE
             ELSE
                IF   FLG-SAISIN-ARI   =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                      MOVE     1             TO FLG-SAISIN-ARI
                END-IF
             END-IF
           ELSE
             IF   FLG-COUNT-ARI  =  ZERO
               ADD    WK-ZAIKAISU       TO  RECE41-KAISU(2)
               MOVE   1                 TO  FLG-COUNT-ARI
             END-IF
      *
             IF    RECE41-KHNTEN(2) = ZERO
               MOVE   WRK-TNS-TEN(IDX3) TO  RECE41-KHNTEN(2)
             ELSE
               IF  RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                 MOVE   ZERO     TO  RECE41-KHNTEN(2)
                 MOVE   1        TO  FLG-KHNTEN2-KO
               END-IF
             END-IF
           END-IF
           COMPUTE    RECE41-TENSU(2)   =
                      RECE41-TENSU(2)   +
                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *
           .
      *
       21012251-RECE-LEFT12-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý
      *****************************************************************
       210123-RECE45-WRITE-SEC                SECTION.
      *
           MOVE    RECE45-REC          TO   RECE4B-REC      
           MOVE    RECE45-REC          TO   RECE45C-REC
      *
           WRITE   RECE45-REC
           ADD     1                   TO   CNT-OUT2
      *
           WRITE   RECE4B-REC
           ADD     1                   TO   CNT-OUT5
      *
           IF  (RECE41-SRYYM        <   201307              )
           OR  (RECE41-PTNUM        =   SPACE               )
           OR  (RECE41-TSTPTNUMKBN  =   "1"                 )
           OR  (RECE41-RECESYUBETU  =   ZERO OR 7021 OR 7051)
           OR  (RECE41-RSIIRYOKBN   =   "0"                 )
             CONTINUE
           ELSE
             WRITE RECE45C-REC
             ADD   1                   TO   CNT-OUT7
           END-IF
      *
           .
       210123-RECE45-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý
      *****************************************************************
       210124-RECE45-REWRITE-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE45
                 NOT INVALID
                     MOVE    ZERO      TO  FLG-RECE45
                     READ    RECE45-FILE   NEXT
                       AT  END
                         MOVE    1     TO  FLG-RECE45
                     END-READ
           END-START
           PERFORM   UNTIL
               (RECE44-HOSPNUM      NOT =  RECE45-HOSPNUM)     OR
               (RECE44-SRYYM        NOT =  RECE45-SRYYM)       OR
               (RECE44-NYUGAIKBN    NOT =  RECE45-NYUGAIKBN)   OR
               (RECE44-PTID         NOT =  RECE45-PTID)        OR
               (RECE44-RECEKA       NOT =  RECE45-RECEKA)      OR
               (RECE44-HKNID        NOT =  RECE45-HKNID)       OR
               (RECE44-TEKSTYMD     NOT =  RECE45-TEKSTYMD)    OR
               (RECE44-ZAINUM       NOT =  RECE45-ZAINUM)      OR
               (FLG-RECE45                =      1        )    OR
               (FLG-REWRITE               =      1        )
                IF    RECE45-RENNUM   =   WRK-RENNUM
                   MOVE    1              TO  FLG-REWRITE
                   PERFORM  VARYING  IDXB  FROM  5   BY  -1
                             UNTIL   IDXB   <    1
                     IF  RECE45-SRYCD(IDXB)  NOT = SPACE
                       MOVE WRK-SRYSURYO3 TO RECE45-SRYSURYO(IDXB)
                       MOVE 1             TO IDXB
                     END-IF
                   END-PERFORM
                   REWRITE   RECE45-REC
                END-IF
                READ    RECE45-FILE   NEXT
                  AT  END
                    MOVE    1     TO  FLG-RECE45
                END-READ
           END-PERFORM
      *
      *****
      *
           IF  (RECE41-SRYYM        <   201307              )
           OR  (RECE41-PTNUM        =   SPACE               )
           OR  (RECE41-TSTPTNUMKBN  =   "1"                 )
           OR  (RECE41-RECESYUBETU  =   ZERO OR 7021 OR 7051)
           OR  (RECE41-RSIIRYOKBN   =   "0"                 )
             CONTINUE
           ELSE
      *
             MOVE    ZERO                TO  FLG-REWRITE
      *
             INITIALIZE                      RECE45C-REC
             MOVE    RECE44-HOSPNUM      TO  RECE45C-HOSPNUM
             MOVE    RECE44-SRYYM        TO  RECE45C-SRYYM
             MOVE    RECE44-NYUGAIKBN    TO  RECE45C-NYUGAIKBN
             MOVE    RECE44-PTID         TO  RECE45C-PTID
             MOVE    RECE44-RECEKA       TO  RECE45C-RECEKA
             MOVE    RECE44-HKNID        TO  RECE45C-HKNID
             MOVE    RECE44-TEKSTYMD     TO  RECE45C-TEKSTYMD
             MOVE    RECE44-ZAINUM       TO  RECE45C-ZAINUM
             START   RECE45C-FILE
                 KEY IS    >=   RECE45C-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE45
                 NOT INVALID
                     MOVE    ZERO      TO  FLG-RECE45
                     READ    RECE45C-FILE   NEXT
                       AT  END
                         MOVE    1     TO  FLG-RECE45
                     END-READ
             END-START
             PERFORM   UNTIL
               (RECE44-HOSPNUM      NOT =  RECE45C-HOSPNUM)     OR
               (RECE44-SRYYM        NOT =  RECE45C-SRYYM)       OR
               (RECE44-NYUGAIKBN    NOT =  RECE45C-NYUGAIKBN)   OR
               (RECE44-PTID         NOT =  RECE45C-PTID)        OR
               (RECE44-RECEKA       NOT =  RECE45C-RECEKA)      OR
               (RECE44-HKNID        NOT =  RECE45C-HKNID)       OR
               (RECE44-TEKSTYMD     NOT =  RECE45C-TEKSTYMD)    OR
               (RECE44-ZAINUM       NOT =  RECE45C-ZAINUM)      OR
               (FLG-RECE45                =      1        )    OR
               (FLG-REWRITE               =      1        )
                IF    RECE45C-RENNUM   =   WRK-RENNUM
                   MOVE    1              TO  FLG-REWRITE
                   PERFORM  VARYING  IDXB  FROM  5   BY  -1
                             UNTIL   IDXB   <    1
                     IF  RECE45C-SRYCD(IDXB)  NOT = SPACE
                       MOVE WRK-SRYSURYO3 TO RECE45C-SRYSURYO(IDXB)
                       MOVE 1             TO IDXB
                     END-IF
                   END-PERFORM
                   REWRITE   RECE45C-REC
                END-IF
                READ    RECE45C-FILE   NEXT
                  AT  END
                    MOVE    1     TO  FLG-RECE45
                END-READ
             END-PERFORM
      *
           END-IF
      *
           .
       210124-RECE45-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý
      *****************************************************************
       210124-RECE45-REWRITE2-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE45
                 NOT INVALID
                     MOVE    ZERO      TO  FLG-RECE45
                     READ    RECE45-FILE   NEXT
                       AT  END
                         MOVE    1     TO  FLG-RECE45
                     END-READ
           END-START
           PERFORM   UNTIL
               (RECE44-HOSPNUM      NOT =  RECE45-HOSPNUM)     OR
               (RECE44-SRYYM        NOT =  RECE45-SRYYM)       OR
               (RECE44-NYUGAIKBN    NOT =  RECE45-NYUGAIKBN)   OR
               (RECE44-PTID         NOT =  RECE45-PTID)        OR
               (RECE44-RECEKA       NOT =  RECE45-RECEKA)      OR
               (RECE44-HKNID        NOT =  RECE45-HKNID)       OR
               (RECE44-TEKSTYMD     NOT =  RECE45-TEKSTYMD)    OR
               (RECE44-ZAINUM       NOT =  RECE45-ZAINUM)      OR
               (FLG-RECE45                =      1        )    OR
               (FLG-REWRITE               =      1        )
                IF    RECE45-RENNUM   =   WRK-RENNUM2
                   MOVE    1              TO  FLG-REWRITE
                   PERFORM  VARYING  IDXB  FROM  5   BY  -1
                             UNTIL   IDXB   <    1
                     IF  RECE45-SRYCD(IDXB)  NOT = SPACE
                       MOVE   "1"  TO   RECE45-RECEPTFLG(IDXB)
                       MOVE    1   TO   IDXB
                     END-IF
                   END-PERFORM
                   REWRITE   RECE45-REC
                END-IF
                READ    RECE45-FILE   NEXT
                  AT  END
                    MOVE    1     TO  FLG-RECE45
                END-READ
           END-PERFORM
      *
      *****
      *
           IF  (RECE41-SRYYM        <   201307              )
           OR  (RECE41-PTNUM        =   SPACE               )
           OR  (RECE41-TSTPTNUMKBN  =   "1"                 )
           OR  (RECE41-RECESYUBETU  =   ZERO OR 7021 OR 7051)
           OR  (RECE41-RSIIRYOKBN   =   "0"                 )
             CONTINUE
           ELSE
      *
             MOVE    ZERO                TO  FLG-REWRITE
      *
             INITIALIZE                      RECE45C-REC
             MOVE    RECE44-HOSPNUM      TO  RECE45C-HOSPNUM
             MOVE    RECE44-SRYYM        TO  RECE45C-SRYYM
             MOVE    RECE44-NYUGAIKBN    TO  RECE45C-NYUGAIKBN
             MOVE    RECE44-PTID         TO  RECE45C-PTID
             MOVE    RECE44-RECEKA       TO  RECE45C-RECEKA
             MOVE    RECE44-HKNID        TO  RECE45C-HKNID
             MOVE    RECE44-TEKSTYMD     TO  RECE45C-TEKSTYMD
             MOVE    RECE44-ZAINUM       TO  RECE45C-ZAINUM
             START   RECE45C-FILE
                 KEY IS    >=   RECE45C-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE45
                 NOT INVALID
                     MOVE    ZERO      TO  FLG-RECE45
                     READ    RECE45C-FILE   NEXT
                       AT  END
                         MOVE    1     TO  FLG-RECE45
                     END-READ
             END-START
             PERFORM   UNTIL
               (RECE44-HOSPNUM      NOT =  RECE45C-HOSPNUM)     OR
               (RECE44-SRYYM        NOT =  RECE45C-SRYYM)       OR
               (RECE44-NYUGAIKBN    NOT =  RECE45C-NYUGAIKBN)   OR
               (RECE44-PTID         NOT =  RECE45C-PTID)        OR
               (RECE44-RECEKA       NOT =  RECE45C-RECEKA)      OR
               (RECE44-HKNID        NOT =  RECE45C-HKNID)       OR
               (RECE44-TEKSTYMD     NOT =  RECE45C-TEKSTYMD)    OR
               (RECE44-ZAINUM       NOT =  RECE45C-ZAINUM)      OR
               (FLG-RECE45                =      1        )    OR
               (FLG-REWRITE               =      1        )
                IF    RECE45C-RENNUM   =   WRK-RENNUM2
                   MOVE    1              TO  FLG-REWRITE
                   PERFORM  VARYING  IDXB  FROM  5   BY  -1
                             UNTIL   IDXB   <    1
                     IF  RECE45C-SRYCD(IDXB)  NOT = SPACE
                       MOVE   "1"  TO   RECE45C-RECEPTFLG(IDXB)
                       MOVE    1   TO   IDXB
                     END-IF
                   END-PERFORM
                   REWRITE   RECE45C-REC
                END-IF
                READ    RECE45C-FILE   NEXT
                  AT  END
                    MOVE    1     TO  FLG-RECE45
                END-READ
             END-PERFORM
      *
           END-IF
      *
           .
       210124-RECE45-REWRITE2-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    °åÎÅ¾ðÊóÏ¢·ÈÊÔ½¸
      *****************************************************************
       210125-IRYORENKEI-HENSYU-SEC               SECTION.
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
               PERFORM  800-SRYACT-READ-SEC
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
             MOVE    RECE41-HOSPNUM      TO  ORCSGETGE-HOSPNUM
             MOVE    RECE41-PTID         TO  ORCSGETGE-PTID
             MOVE    RECE41-SRYYM        TO  ORCSGETGE-SRYYM
             MOVE    "2"                 TO  ORCSGETGE-NYUGAIKBN
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
       210125-IRYORENKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼¾ÉÛÌô¥Á¥§¥Ã¥¯
      *****************************************************************
       210126-SIPPU-CHECK-SEC               SECTION.
      *
           INITIALIZE                     SRYACT-REC
           MOVE    ACCT-HOSPNUM       TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO  SRY-PTID
           MOVE    ACCT-SRYKA         TO  SRY-SRYKA
           MOVE    ACCT-SRYYM         TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO  SRY-ZAINUM
           MOVE    SRYACT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE    "tbl_sryact"     TO  MCP-TABLE
             MOVE    "key2"           TO  MCP-PATHNAME
             PERFORM  800-SRYACT-READ-SEC
             PERFORM  UNTIL (FLG-SRYACT     =  1)
                       OR   (FLG-SIPPU-ARI  =  1)
      *
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                         UNTIL  (IDX2            >   5   )
                          OR    (SRY-SRYCD(IDX2) =  SPACE)
                          OR    (FLG-SIPPU-ARI   =   1   )
                  PERFORM    800-TENSU-TBL-SEC
                  IF  (SRY-SRYCD(IDX2)(1:1)  =  "6")
                  AND (TNS-MEIUSESKB         =   1 )
                     MOVE    1        TO  FLG-SIPPU-ARI
                  END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"   TO  MCP-TABLE
               MOVE    "key2"         TO  MCP-PATHNAME
               PERFORM   800-SRYACT-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       210126-SIPPU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý
      *****************************************************************
       21013-RECE44-WRITE-SEC                SECTION.
      *
           MOVE    RECE44-REC          TO  RECE4A-REC
           MOVE    RECE44-REC          TO  RECE44C-REC
      *
           COMPUTE  RECE44-ZAITEN      =  RECE44-ZAITEN     -  
                                          WRK-TEN
           COMPUTE  RECE44-MEISAISU    =  RECE44-MEISAISU   -
                                          WRK-MEISAI-CNT
           COMPUTE  RECE44-SYUTEN1     =  RECE44-SYUTEN1    -
                                          WRK-TEN
           COMPUTE  RECE44-SRYCDTOTAL  =  RECE44-SRYCDTOTAL -
                                          WRK-SRYCDTOTAL
           COMPUTE  RECE44-SURYOUTOTAL =  RECE44-SURYOUTOTAL -
                                          WRK-SURYOUTOTAL     
      *
           MOVE     WRK-TEN         TO  RECE4A-ZAITEN
           MOVE     WRK-MEISAI-CNT  TO  RECE4A-MEISAISU
           MOVE     WRK-TEN         TO  RECE4A-SYUTEN1  
           MOVE     WRK-SRYCDTOTAL  TO  RECE4A-SRYCDTOTAL
           MOVE     WRK-SURYOUTOTAL TO  RECE4A-SURYOUTOTAL
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *      ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿ÉÕ²Ã¾ðÊóÀßÄê
             PERFORM   920-SRYACCTPLUS-HENSYU-SEC
           END-IF
      *
           MOVE    RECE44-ZAITEN    TO  RECE44-HOKATU-ZAITEN
           MOVE    RECE44-SYUTEN1   TO  RECE44-HOKATU-SYUTEN1
           MOVE    RECE44-SYUTEN2   TO  RECE44-HOKATU-SYUTEN2
           MOVE    RECE44-YKZTEN    TO  RECE44-HOKATU-YKZTEN
           MOVE    RECE44-KIZAITEN  TO  RECE44-HOKATU-KIZAITEN
      *
           MOVE    RECE4A-ZAITEN    TO  RECE4A-HOKATU-ZAITEN
           MOVE    RECE4A-SYUTEN1   TO  RECE4A-HOKATU-SYUTEN1
           MOVE    RECE4A-SYUTEN2   TO  RECE4A-HOKATU-SYUTEN2
           MOVE    RECE4A-YKZTEN    TO  RECE4A-HOKATU-YKZTEN
           MOVE    RECE4A-KIZAITEN  TO  RECE4A-HOKATU-KIZAITEN
      *
           WRITE   RECE44-REC
           ADD     1                TO  CNT-OUT1
      *
           WRITE   RECE4A-REC
           ADD     1                TO  CNT-OUT4
      *
           IF  (RECE41-SRYYM        <   201307              )
           OR  (RECE41-PTNUM        =   SPACE               )
           OR  (RECE41-TSTPTNUMKBN  =   "1"                 )
           OR  (RECE41-RECESYUBETU  =   ZERO OR 7021 OR 7051)
           OR  (RECE41-RSIIRYOKBN   =   "0"                 )
             CONTINUE
           ELSE
      *
             COMPUTE  RECE44C-ZAI-TENSU  =  RECE44C-ZAITEN
                                         -  WRK-TEN
             MOVE  WRK-ZAI-KINGAKU  TO  RECE44C-ZAI-KINGAKU
      *
             COMPUTE  RECE44C-ZAITEN     =  RECE44C-ZAITEN
                                         -  WRK-TEN
             COMPUTE  RECE44C-SYUTEN1    =  RECE44C-SYUTEN1
                                         -  WRK-TEN
      *
             MOVE  RECE44C-ZAITEN   TO  RECE44C-HOKATU-ZAITEN
             MOVE  RECE44C-SYUTEN1  TO  RECE44C-HOKATU-SYUTEN1
             MOVE  RECE44C-SYUTEN2  TO  RECE44C-HOKATU-SYUTEN2
             MOVE  RECE44C-YKZTEN   TO  RECE44C-HOKATU-YKZTEN
             MOVE  RECE44C-KIZAITEN TO  RECE44C-HOKATU-KIZAITEN
      *
             WRITE RECE44C-REC
             ADD   1                TO  CNT-OUT6
      *
           END-IF
      *
           .
       21013-RECE44-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÞ¤Î¿ÇÎÅÆâÍÆ¥Á¥§¥Ã¥¯
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
                 MOVE RECE41-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE RECE41-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE RECE41-PTID        TO  SRY-PTID
                 MOVE RECE41-SRYYM       TO  SRY-SRYYM
                 MOVE ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE SRYACT-REC         TO  MCPDATA-REC
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC        =  ZERO
                     MOVE "tbl_sryact"   TO  MCP-TABLE
                     MOVE "key8"         TO  MCP-PATHNAME
                     PERFORM   800-SRYACT-READ-SEC
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
                 MOVE "tbl_sryact"   TO  MCP-TABLE
                 MOVE "key8"         TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       21014-SINRYO-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊó¸¡º÷
      **************************************************************
       21015-SRYACCTPLUS-KENSAKU-SEC           SECTION.
      *
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM       TO   ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO   ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID          TO   ACCTP-PTID
           MOVE    ACCT-SRYKA         TO   ACCTP-SRYKA
           MOVE    ACCT-SRYYM         TO   ACCTP-SRYYM
           MOVE    ACCT-ZAINUM        TO   ACCTP-ZAINUM
           MOVE    1                  TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC    TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
               MOVE    "key"              TO   MCP-PATHNAME
               PERFORM  800-SRYACCTPLUS-READ-SEC
               IF        FLG-SRYACCTPLUS   =   ZERO
                 IF   ACCTP-RSIKINGAKU  NOT = ZERO
                    MOVE    1   TO   FLG-TAISYO
                 END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       21015-SRYACCTPLUS-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý
      *****************************************************************
       2102-KHNTEN-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN15        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(15)
           END-IF
           IF        FLG-KHNTEN18        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(18)
           END-IF
           IF        FLG-KHNTEN19        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(19)
           END-IF
      *
           IF   RECE41-SRYYM   >=  "201004"
           AND  RECE41-SRYYM   <=  "201803"
             IF  CNT-JIKANTAI1     NOT = ZERO
               IF   CNT-JIKANTAI2  NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT =  CNT-JIKANTAI2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI3  NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT =  CNT-JIKANTAI3
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-MESAIKSN   NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT =  CNT-MESAIKSN
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKATU  NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT =  CNT-CHIHOKATU
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKAT2  NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT =  CNT-CHIHOKAT2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
             END-IF
             IF  CNT-JIKANTAI2     NOT = ZERO
               IF   CNT-JIKANTAI1  NOT = ZERO
                 IF   CNT-JIKANTAI2  NOT =  CNT-JIKANTAI1
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI3  NOT = ZERO
                 IF   CNT-JIKANTAI2  NOT =  CNT-JIKANTAI3
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-MESAIKSN   NOT = ZERO
                 IF   CNT-JIKANTAI2  NOT =  CNT-MESAIKSN
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKATU  NOT = ZERO
                 IF   CNT-JIKANTAI2  NOT =  CNT-CHIHOKATU
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKAT2  NOT = ZERO
                 IF   CNT-JIKANTAI2  NOT =  CNT-CHIHOKAT2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
             END-IF
             IF  CNT-JIKANTAI3     NOT = ZERO
               IF   CNT-JIKANTAI2  NOT = ZERO
                 IF   CNT-JIKANTAI3  NOT =  CNT-JIKANTAI2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI3  NOT = ZERO
                 IF   CNT-JIKANTAI3  NOT =  CNT-JIKANTAI3
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-MESAIKSN   NOT = ZERO
                 IF   CNT-JIKANTAI3  NOT =  CNT-MESAIKSN
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKATU  NOT = ZERO
                 IF   CNT-JIKANTAI3  NOT =  CNT-CHIHOKATU
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKAT2  NOT = ZERO
                 IF   CNT-JIKANTAI3  NOT =  CNT-CHIHOKAT2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
             END-IF
             IF  CNT-MESAIKSN      NOT = ZERO
               IF   CNT-JIKANTAI1  NOT = ZERO
                 IF   CNT-MESAIKSN   NOT =  CNT-JIKANTAI1
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI2  NOT = ZERO
                 IF   CNT-MESAIKSN   NOT =  CNT-JIKANTAI2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI3  NOT = ZERO
                 IF   CNT-MESAIKSN   NOT =  CNT-JIKANTAI3
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKATU  NOT = ZERO
                 IF   CNT-MESAIKSN   NOT =  CNT-CHIHOKATU
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKAT2  NOT = ZERO
                 IF   CNT-MESAIKSN   NOT =  CNT-CHIHOKAT2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
             END-IF
             IF  CNT-CHIHOKATU     NOT = ZERO
               IF   CNT-JIKANTAI1  NOT = ZERO
                 IF   CNT-CHIHOKATU  NOT =  CNT-JIKANTAI1
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI2  NOT = ZERO
                 IF   CNT-CHIHOKATU  NOT =  CNT-JIKANTAI2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI3  NOT = ZERO
                 IF   CNT-CHIHOKATU  NOT =  CNT-JIKANTAI3
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-MESAIKSN   NOT = ZERO
                 IF   CNT-CHIHOKATU  NOT =  CNT-MESAIKSN
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKAT2  NOT = ZERO
                 IF   CNT-CHIHOKATU  NOT =  CNT-CHIHOKAT2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
             END-IF
             IF  CNT-CHIHOKAT2     NOT = ZERO
               IF   CNT-JIKANTAI1  NOT = ZERO
                 IF   CNT-CHIHOKAT2  NOT =  CNT-JIKANTAI1
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI2  NOT = ZERO
                 IF   CNT-CHIHOKAT2  NOT =  CNT-JIKANTAI2
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-JIKANTAI3  NOT = ZERO
                 IF   CNT-CHIHOKAT2  NOT =  CNT-JIKANTAI3
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-MESAIKSN   NOT = ZERO
                 IF   CNT-CHIHOKAT2  NOT =  CNT-MESAIKSN
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
               IF   CNT-CHIHOKATU  NOT = ZERO
                 IF   CNT-CHIHOKAT2  NOT =  CNT-CHIHOKATU
                   MOVE   ZERO         TO   RECE41-KHNTEN(2)
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       2102-KHNTEN-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý¡Ê¼«ÇåÀÕ¡Ë
      *****************************************************************
       2103-KHNTEN-JI-CLEAR-SEC               SECTION.
      *
           IF     RECE41-HKNKBN   =   "4"
            EVALUATE  WRK-PARA-RECEKBN
              WHEN      "4"
      **********¡Ê¿·ÍÍ¼°¡Ë
                IF        FLG-KHNTEN10-JI    =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(10)
                END-IF
                IF        FLG-KHNTEN13-JI    =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(13)
                END-IF
                IF        FLG-KHNTEN14-JI    =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(14)
                END-IF
              WHEN      "5"
      **********¡Ê½¾ÍèÍÍ¼°¡Ë
                IF        FLG-KHNTEN2-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(2)
                END-IF
                IF        FLG-KHNTEN3-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(3)
                END-IF
                IF        FLG-KHNTEN4-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(4)
                END-IF
                IF        FLG-KHNTEN5-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(5)
                END-IF
                IF        FLG-KHNTEN6-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(6)
                END-IF
                IF        FLG-KHNTEN11-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(11)
                END-IF
                IF        FLG-KHNTEN14-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(14)
                END-IF
                IF        FLG-KHNTEN15-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(15)
                END-IF
      *
                IF    (RECE41-JIBAIKBN      =  "2"         )
                AND   (CNT-NYUYOJI2     NOT =  ZERO        )
                AND   (CNT-SAISIN-ETC   NOT =  CNT-NYUYOJI2)
                AND   (RECE41-SRYYM        >=  "200604"    )
                  MOVE    "1"              TO   RECE41-NYUYOJIFLG
                END-IF
            END-EVALUATE
           END-IF
      *
           IF     RECE41-HKNKBN   =   "6"
                IF        FLG-KHNTEN2-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(2)
                END-IF
                IF        FLG-KHNTEN3-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(3)
                END-IF
                IF        FLG-KHNTEN4-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(4)
                END-IF
                IF        FLG-KHNTEN5-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(5)
                END-IF
                IF        FLG-KHNTEN6-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(6)
                END-IF
                IF        FLG-KHNTEN11-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(11)
                END-IF
                IF        FLG-KHNTEN14-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(14)
                END-IF
                IF        FLG-KHNTEN15-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(15)
                END-IF
      *
                IF    (CNT-NYUYOJI2     NOT =  ZERO        )
                AND   (CNT-SAISIN-ETC   NOT =  CNT-NYUYOJI2)
                AND   (RECE41-SRYYM        >=  "200604"    )
                  MOVE    "1"              TO   RECE41-NYUYOJIFLG
                END-IF
           END-IF
      *
           .
       2103-KHNTEN-JI-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®
      *****************************************************************
       2104-RECE44-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE41-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE41-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE41-PTID          TO      ACCT-PTID
           MOVE    RECE41-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE41-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE41-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key24"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key24"          TO      MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL     FLG-SRYACCT   =   1 
      *
                 MOVE   ZERO          TO  FLG-RINYAK
      *
                 IF    ACCT-SRYKBN       =  "95"  OR  "96"
                   CONTINUE
                 ELSE
      *
                   MOVE     ZERO    TO    FLG-TAISYO
                   IF   (ACCT-SRYKBN         =   "80" )
                   AND  (ACCT-ZAITEN         =   ZERO )
                   AND  (ACCT-ZAIKAISU   NOT =   ZERO )
                   AND  (ACCT-JIHOKBN        =   SPACE)
                     PERFORM  21015-SRYACCTPLUS-KENSAKU-SEC
                   END-IF
      *
                   MOVE   RECE41-TEKSTYMD(7:2)   TO   WRK-DAY
                   IF  ACCT-DAY(1 WRK-DAY)   NOT =    ZERO
                        MOVE    1       TO   FLG-DAY
                   ELSE
                        MOVE    ZERO    TO   FLG-DAY  
                   END-IF
      *
                   IF      FLG-DAY         =   1
                           IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                                (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                                (WRK-PARA-PRTKBN    =
                                            "1" OR "2" OR "3" ) AND
                                (ACCT-JIHOKBN       =    SPACE))
                           OR  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                                (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                                (WRK-PARA-PRTKBN    =
                                                   "4" OR "5" ))
                           OR  ( FLG-TAISYO         =      1   )
                               MOVE  ACCT-DAY(1 WRK-DAY)  
                                               TO    WRK-ZAIKAISU-XXX
                               MOVE    ZERO    TO    WRK-TEN
                                                     WRK-MEISAI-CNT
                                                     WRK-SRYCDTOTAL
                                                     WRK-SURYOUTOTAL
                                                     WRK-YOUBUKASAN
                                                     WRK-KYOBUKASAN
                                                     WRK-KEIBUKASAN
                                                     WRK-SOGAIKASAN
                                                     WRK-RYOYOTEATE
                                                     WRK-ZAI-KINGAKU
      *
                               MOVE    ZERO    TO    WRK-SRYSURYO
                                                     WRK-SRYSURYO2
                                                     WRK-SRYSURYO3
                                                     WRK-TANICD
                                                     WRK-TANIMOJISU
                               MOVE    SPACE   TO    WRK-TANINAME
                               MOVE    ZERO    TO    FLG-HOUSIJI-COM
                                                     FLG-HOUTEN-COM
                                                     FLG-SYOBYO-COM
                                                     FLG-RYOYO-COM
                                                     FLG-SYODOU-COM
                                                     FLG-SAIDOU-COM
                                                     FLG-NIKOCHIN-COM
                                                     FLG-ZENSOKU-COM
                                                     FLG-OSIN-COM
                                                     FLG-ZAIHOU-COM
                                                     FLG-TAHOKEN
                                                     FLG-SYOUNI-COM
                                                     FLG-SRYKBN-HEN
                                                     FLG-KENSA-COM
                                                     FLG-ZAIRIHA-COM
                                                     FLG-ZAIKYO-COM
                                                     FLG-BOKOHO-COM
                                                     FLG-IZONSYO-COM
                                                     FLG-NINRYO-COM
                                                     FLG-NYUSEN-COM
                                                     FLG-KIATU-COM
                                                     FLG-GAIJIR-COM
      *
                               MOVE    SPACE   TO    WRK-KYUGYOFLG
                               MOVE    ZERO    TO    FLG-COUNT-ARI
      *
                               MOVE    SPACE   TO    WRK-FORMALNAME
                               MOVE    ZERO    TO
                                               WRK-KZI058-TEN
                                               WRK-KZI058-TANICD
                                               WRK-KZI058-TANIMOJISU
                               MOVE    SPACE   TO
                                               WRK-KZI058-TANINAME
      *
                               MOVE    ZERO    TO    FLG-WRITE45
                               MOVE    ZERO    TO    FLG-RIHA-OKIKAE
                               MOVE    ZERO    TO    FLG-HENSYU-NG
                               MOVE    ZERO    TO    FLG-SETCOM
                               MOVE    ZERO    TO    WRK-SETCOM-JIDO
                               MOVE    ZERO    TO    WRK-SETCOM-JIDO2
                               MOVE    ZERO    TO    FLG-ZAITAKU-CHECK
                               MOVE    ZERO    TO    FLG-KIN-OKIKAE
                               MOVE    ZERO    TO    FLG-KIN-OKIKAE2
                               MOVE    ZERO    TO    FLG-SIPPU-COM
                               MOVE    ZERO    TO    FLG-SIPPU-ARI
                               MOVE    ZERO    TO    FLG-YKZ-PRTKBN
                               MOVE    ZERO    TO    FLG-KIZ-PRTKBN
                               PERFORM   21011-RECE44-TENSOU-SEC
                               PERFORM   21012-RECE45-HENSYU-SEC
                               IF    FLG-WRITE45  =   1
                                 PERFORM 21013-RECE44-WRITE-SEC
                               END-IF
                           ELSE
                             IF    ACCT-ZAIKAISU  NOT =    ZERO
                             AND   ACCT-JIHOKBN       =    SPACE
      **>                    IF  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                         (WRK-PARA-PRTKBN    =
      **>                                     "1" OR "2" OR "3" ) AND
      **>                         (ACCT-JIHOKBN       =    SPACE))
      **>                    OR  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                         (WRK-PARA-PRTKBN    =
      **>                                            "4" OR "5" ))
                               IF ( ACCT-SRYKBN  NOT =
                                    "24" AND "90" AND "92" AND
                                    "93" AND "97"                 )
                               OR ( ACCT-ZAIKBN  =   1            )
                                 IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                    "000000999901")
                                 AND (ACCT-MEISAISU      =   1    )
                                      CONTINUE
                                 ELSE
                                      MOVE  ACCT-DAY(1 WRK-DAY)  
                                                  TO  WRK-ZAIKAISU-XXX
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-KEIBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
                                                      WRK-ZAI-KINGAKU
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
                                                      FLG-SYOBYO-COM
                                                      FLG-RYOYO-COM
                                                      FLG-SYODOU-COM
                                                      FLG-SAIDOU-COM
                                                      FLG-NIKOCHIN-COM
                                                      FLG-ZENSOKU-COM
                                                      FLG-OSIN-COM
                                                      FLG-ZAIHOU-COM
                                                      FLG-TAHOKEN
                                                      FLG-SYOUNI-COM
                                                      FLG-SRYKBN-HEN
                                                      FLG-KENSA-COM
                                                      FLG-ZAIRIHA-COM
                                                      FLG-ZAIKYO-COM
                                                      FLG-BOKOHO-COM
                                                      FLG-IZONSYO-COM
                                                      FLG-NINRYO-COM
                                                      FLG-NYUSEN-COM
                                                      FLG-KIATU-COM
                                                      FLG-GAIJIR-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
                                      MOVE  ZERO  TO  FLG-COUNT-ARI
      *
                                      MOVE  SPACE TO  WRK-FORMALNAME
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO  FLG-WRITE45
                                      MOVE  ZERO  TO  FLG-RIHA-OKIKAE
                                      MOVE  ZERO  TO  FLG-HENSYU-NG
                                      MOVE  ZERO  TO  FLG-SETCOM
                                      MOVE  ZERO  TO  WRK-SETCOM-JIDO
                                      MOVE  ZERO  TO  WRK-SETCOM-JIDO2
                                      MOVE  ZERO  TO  FLG-ZAITAKU-CHECK
                                      MOVE  ZERO  TO  FLG-KIN-OKIKAE
                                      MOVE  ZERO  TO  FLG-KIN-OKIKAE2
                                      MOVE  ZERO  TO  FLG-SIPPU-COM
                                      MOVE  ZERO  TO  FLG-SIPPU-ARI
                                      MOVE  ZERO  TO  FLG-YKZ-PRTKBN
                                      MOVE  ZERO  TO  FLG-KIZ-PRTKBN
                                      IF    ACCT-SRYKBN  =
                                            "60" OR "64" OR "80"
                                        PERFORM 21014-SINRYO-CHECK-SEC
                                      END-IF
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      IF    FLG-WRITE45  =   1
                                        PERFORM 21013-RECE44-WRITE-SEC
                                      END-IF
                                 END-IF
                               END-IF
                             END-IF
                           END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"    TO      MCP-TABLE
                 MOVE     "key24"          TO      MCP-PATHNAME
                 PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2104-RECE44-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý
      *****************************************************************
       2105-RECE41-REWRITE-SEC                SECTION.
      *
           IF   RECE41-HKNKBN     NOT  =   "4" AND "6"
             COMPUTE  RECE41-TOTALKIN2  =  RECE41-TOTALKIN2
                                        +  WRK-ZAITENNASHI
             IF    RECE41-HKNKBN   =   "5"
             AND   RECE41-KOMUKBN  =   "2"
               COMPUTE  RECE41-TOTALKIN   =   RECE41-TOTALTEN   *
                                              RECE41-KOMUTENTANKA
             ELSE
               COMPUTE  RECE41-TOTALKIN   =   RECE41-TOTALTEN   *
                                              RECE41-TANKA
             END-IF
           END-IF
      *
           IF   RECE41-HKNKBN     =   "4"                               
      *
             EVALUATE  WRK-PARA-RECEKBN
              WHEN      "4"
      ********¡Ê¿·ÍÍ¼°¡Ë
               IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                 COMPUTE   RECE41-TENSU(8)    =  RECE41-TENSU(1)
                                              +  RECE41-TENSU(2)
                                              +  RECE41-TENSU(3)
                                              +  RECE41-TENSU(4)
                                              +  RECE41-TENSU(5)
                                              +  RECE41-TENSU(6)
                                              +  RECE41-TENSU(7)
      *
                 COMPUTE   RECE41-TENSU-YAK(8)  =  RECE41-TENSU-YAK(7)
      *
                 COMPUTE   RECE41-TENSU(17)   =  RECE41-TENSU(10)
                                              +  RECE41-TENSU(13)
                                              +  RECE41-TENSU(14)
                                              +  RECE41-TENSU(15)
                                              +  RECE41-TENSU(16)
      *
                 COMPUTE   RECE41-TENSU-YAK(17) =  RECE41-TENSU-YAK(9)
                                                +  RECE41-TENSU-YAK(11)
                                                +  RECE41-TENSU-YAK(12)
      *
                 COMPUTE   RECE41-TENSU(22)     =  RECE41-TENSU(18)
                                                +  RECE41-TENSU(19)
                                                +  RECE41-TENSU(20)
      *
                 COMPUTE   RECE41-TENSU-YAK(22) =  RECE41-TENSU-YAK(21)
      *
                 COMPUTE   WRK-KSNRATE          =  RECE41-KSNRATE / 100
      *
                 PERFORM    VARYING    ID1    FROM    1   BY   1
                             UNTIL     ID1     >     31
      *
                   ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
                   ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN-YAK
      *
                 END-PERFORM
      *
      *          ¸ºÅÀÊÔ½¸
                 COMPUTE   RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                            -  WRK-RECE4G-TENSU(2)
                                            -  WRK-RECE4G-TENSU(3)
                                            -  WRK-RECE4G-TENSU(5)
                                            -  WRK-RECE4G-TENSU(6)
                                            -  WRK-RECE4G-TENSU(7)
                                            -  WRK-RECE4G-TENSU(8)
                                            -  WRK-RECE4G-TENSU(9)
                                            -  WRK-RECE4G-TENSU(12)
                                            -  WRK-RECE4G-TENSU(14)
      *
                 COMPUTE   RECE41-TOTALKIN   =
                    (RECE41-TOTALTEN  *  RECE41-TANKA  *  WRK-KSNRATE)
                                                       +  0.5
      *
                 COMPUTE   RECE41-TOTALKIN-YAK =
                    (RECE41-TOTALTEN-YAK  *  RECE41-TANKA)  +  0.5
      *
                 COMPUTE   RECE41-KINGAKU2(5)   =  RECE41-KINGAKU2(1)
                                                +  RECE41-KINGAKU2(2)
                                                +  RECE41-KINGAKU2(3)
                                                +  RECE41-KINGAKU2(4)
      *
      ***        COMPUTE   RECE41-TOTALKIN2     =  RECE41-KINGAKU2(5)
      ***                                       *  WRK-KSNRATE
      *
                 COMPUTE   RECE41-TOTALKIN2     =
                    (RECE41-KINGAKU2(5)  *  WRK-KSNRATE)  +  0.5
      *
                 COMPUTE   RECE41-TOTALKIN2-ETC =  RECE41-KINGAKU2(6)
                                                +  RECE41-SINDAN-KIN
                                                +  RECE41-MEISAI-KIN
      *
                 PERFORM   21054-JIBAI-KINGAKU-HENSYU-SEC
               ELSE
                 COMPUTE   RECE41-TENSU(8)    =  RECE41-TENSU(1)
                                              +  RECE41-TENSU(2)
                                              +  RECE41-TENSU(3)
                                              +  RECE41-TENSU(4)
                                              +  RECE41-TENSU(5)
                                              +  RECE41-TENSU(6)
                                              +  RECE41-TENSU(7)
      *
                 COMPUTE   RECE41-TENSU-YAK(8)  =  RECE41-TENSU-YAK(7)
      *
                 COMPUTE   RECE41-TENSU(17)   =  RECE41-TENSU(10)
                                              +  RECE41-TENSU(13)
                                              +  RECE41-TENSU(14)
                                              +  RECE41-TENSU(15)
                                              +  RECE41-TENSU(16)
      *
                 COMPUTE   RECE41-TENSU-YAK(18) =  RECE41-TENSU-YAK(9)
                                                +  RECE41-TENSU-YAK(11)
                                                +  RECE41-TENSU-YAK(12)
      *
                 COMPUTE   RECE41-TENSU(23)     =  RECE41-TENSU(19)
                                                +  RECE41-TENSU(20)
                                                +  RECE41-TENSU(21)
      *
                 COMPUTE   RECE41-TENSU-YAK(24) =  RECE41-TENSU-YAK(22)
      *
                 COMPUTE   WRK-KSNRATE          =  RECE41-KSNRATE / 100
      *
                 PERFORM    VARYING    ID1    FROM    1   BY   1
                             UNTIL     ID1     >     31
      *
                   ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
                   ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN-YAK
      *
                 END-PERFORM
      *
      *          ¸ºÅÀÊÔ½¸
                 COMPUTE   RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                            -  WRK-RECE4G-TENSU(2)
                                            -  WRK-RECE4G-TENSU(3)
                                            -  WRK-RECE4G-TENSU(5)
                                            -  WRK-RECE4G-TENSU(6)
                                            -  WRK-RECE4G-TENSU(7)
                                            -  WRK-RECE4G-TENSU(8)
                                            -  WRK-RECE4G-TENSU(9)
                                            -  WRK-RECE4G-TENSU(12)
                                            -  WRK-RECE4G-TENSU(14)
      *
                 COMPUTE   RECE41-TOTALKIN   =
                    (RECE41-TOTALTEN  *  RECE41-TANKA  *  WRK-KSNRATE)
                                                       +  0.5
      *
                 COMPUTE   RECE41-TOTALKIN-YAK =
                    (RECE41-TOTALTEN-YAK  *  RECE41-TANKA)  +  0.5
      *
                 COMPUTE   RECE41-KINGAKU2(5)   =  RECE41-KINGAKU2(1)
                                                +  RECE41-KINGAKU2(2)
                                                +  RECE41-KINGAKU2(3)
                                                +  RECE41-KINGAKU2(4)
      *
      ***        COMPUTE   RECE41-TOTALKIN2     =  RECE41-KINGAKU2(5)
      ***                                       *  WRK-KSNRATE
      *
                 COMPUTE   RECE41-TOTALKIN2     =
                    (RECE41-KINGAKU2(5)  *  WRK-KSNRATE)  +  0.5
      *
                 COMPUTE   RECE41-TOTALKIN2-ETC =  RECE41-KINGAKU2(6)
                                                +  RECE41-SINDAN-KIN
                                                +  RECE41-MEISAI-KIN
               END-IF
      *
              WHEN      "5"
      ********¡Ê½¾ÍèÍÍ¼°¡Ë
      *
               COMPUTE   RECE41-TENSU(9)      =  RECE41-TENSU(1)
                                              +  RECE41-TENSU(2)
                                              +  RECE41-TENSU(3)
                                              +  RECE41-TENSU(4)
                                              +  RECE41-TENSU(5)
                                              +  RECE41-TENSU(6)
                                              +  RECE41-TENSU(7)
                                              +  RECE41-TENSU(8)
      *
               COMPUTE   RECE41-TENSU(18)     =  RECE41-TENSU(10)
                                              +  RECE41-TENSU(11)
                                              +  RECE41-TENSU(12)
                                              +  RECE41-TENSU(13)
                                              +  RECE41-TENSU(14)
                                              +  RECE41-TENSU(15)
                                              +  RECE41-TENSU(16)
                                              +  RECE41-TENSU(17)
      *
               COMPUTE   RECE41-TENSU(22)     =  RECE41-TENSU(19)
                                              +  RECE41-TENSU(20)
                                              +  RECE41-TENSU(21)
      *
               COMPUTE   RECE41-TENSU(25)     =  RECE41-TENSU(23)
                                              +  RECE41-TENSU(24)
      *
               COMPUTE   RECE41-TENSU(28)     =  RECE41-TENSU(26)
                                              +  RECE41-TENSU(27)
      *
               COMPUTE   RECE41-TENSU(31)     =  RECE41-TENSU(29)
                                              +  RECE41-TENSU(30)
      *
               COMPUTE   RECE41-TENSU(34)     =  RECE41-TENSU(32)
                                              +  RECE41-TENSU(33)
      *
               COMPUTE   RECE41-TENSU(38)     =  RECE41-TENSU(35)
                                              +  RECE41-TENSU(36)
                                              +  RECE41-TENSU(37)
      *
      ***      COMPUTE   RECE41-TOTALTEN      =  RECE41-TENSU(9)
      ***                                     +  RECE41-TENSU(18)
      ***                                     +  RECE41-TENSU(22)
      ***                                     +  RECE41-TENSU(25)
      ***                                     +  RECE41-TENSU(28)
      ***                                     +  RECE41-TENSU(31)
      ***                                     +  RECE41-TENSU(34)
      ***                                     +  RECE41-TENSU(38)
      *
               MOVE       ZERO         TO        WRK-SYUTEN-JI
                                                 WRK-YKZTEN-JI
               PERFORM    VARYING    ID1    FROM    1   BY   1
                           UNTIL     ID1     >     31
                  ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
                                                   WRK-SYUTEN-JI
                  ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN
                                                   WRK-YKZTEN-JI
               END-PERFORM
      *        ¸ºÅÀÊÔ½¸
               COMPUTE   RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                          -  WRK-RECE4G-TENSU(2)
                                          -  WRK-RECE4G-TENSU(3)
                                          -  WRK-RECE4G-TENSU(5)
                                          -  WRK-RECE4G-TENSU(6)
                                          -  WRK-RECE4G-TENSU(7)
                                          -  WRK-RECE4G-TENSU(8)
                                          -  WRK-RECE4G-TENSU(9)
                                          -  WRK-RECE4G-TENSU(12)
                                          -  WRK-RECE4G-TENSU(14)
               COMPUTE   WRK-SYUTEN-JI    =  WRK-SYUTEN-JI
                                          -  WRK-RECE4G-TENSU(2)
                                          -  WRK-RECE4G-TENSU(3)
                                          -  WRK-RECE4G-TENSU(5)
                                          -  WRK-RECE4G-TENSU(6)
                                          -  WRK-RECE4G-TENSU(7)
                                          -  WRK-RECE4G-TENSU(8)
                                          -  WRK-RECE4G-TENSU(9)
                                          -  WRK-RECE4G-TENSU(12)
                                          -  WRK-RECE4G-TENSU(14)
      *
               IF   TBL-SYUTEN-JI(2)  >=  WRK-RECE4G-TENSU(2)
                 COMPUTE  TBL-SYUTEN-JI(2) =
                          TBL-SYUTEN-JI(2) -  WRK-RECE4G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(2)
               END-IF
               IF   TBL-SYUTEN-JI(7)  >=  WRK-RECE4G-TENSU(7)
                 COMPUTE  TBL-SYUTEN-JI(7) =
                          TBL-SYUTEN-JI(7) -  WRK-RECE4G-TENSU(7)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(7)
               END-IF
               IF   TBL-SYUTEN-JI(8)  >=  WRK-RECE4G-TENSU(8)
                 COMPUTE  TBL-SYUTEN-JI(8) =
                          TBL-SYUTEN-JI(8) -  WRK-RECE4G-TENSU(8)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(8)
               END-IF
      *
               EVALUATE   RECE41-JIBAIKBN
      *
                 WHEN       "1"
      *
                   COMPUTE   WRK-KSNRATE   =  RECE41-KSNRATE / 100
      *
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-SYUTEN-JI  *  RECE41-TANKA  *  WRK-KSNRATE)
                                                       +  0.5
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-YKZTEN-JI  *  RECE41-TANKA) +  0.5
      *
                   COMPUTE  RECE41-SONOTAKIN-JYU  =
                            RECE41-SONOTAKIN-JYU  +
                      (WRK-KINGAKU-JI *  WRK-KSNRATE)  +  0.5
      ***
                   PERFORM    VARYING    ID1    FROM    1   BY   1
                               UNTIL     ID1     >     38
                     IF   ID1  =   9  OR  18  OR  22  OR  25  OR
                                  28  OR  31  OR  34  OR  38
                        CONTINUE
                     ELSE
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-SYUTEN-JI(ID1) * RECE41-TANKA
                                            * WRK-KSNRATE )
                                                           + 0.5
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-YKZTEN-JI(ID1) * RECE41-TANKA)
                                                           + 0.5
                     END-IF
                   END-PERFORM
      *
                 WHEN       "2"
      *
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-SYUTEN-JI  *  RECE41-SYUTENTANKA)
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-YKZTEN-JI  *  RECE41-YKZTENTANKA)
      ***
                   PERFORM    VARYING    ID1    FROM    1   BY   1
                               UNTIL     ID1     >     38
                     IF   ID1  =   9  OR  18  OR  22  OR  25  OR
                                  28  OR  31  OR  34  OR  38
                        CONTINUE
                     ELSE
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-SYUTEN-JI(ID1) * RECE41-SYUTENTANKA)
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-YKZTEN-JI(ID1) * RECE41-YKZTENTANKA)
                     END-IF
                   END-PERFORM
      *
               END-EVALUATE
      *
               COMPUTE   RECE41-KINGAKU-JYU(9)  =
                         RECE41-KINGAKU-JYU(1)  +
                         RECE41-KINGAKU-JYU(2)  +
                         RECE41-KINGAKU-JYU(3)  +
                         RECE41-KINGAKU-JYU(4)  +
                         RECE41-KINGAKU-JYU(5)  +
                         RECE41-KINGAKU-JYU(6)  +
                         RECE41-KINGAKU-JYU(7)  +
                         RECE41-KINGAKU-JYU(8)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(18)  =
                         RECE41-KINGAKU-JYU(10)  +
                         RECE41-KINGAKU-JYU(11)  +
                         RECE41-KINGAKU-JYU(12)  +
                         RECE41-KINGAKU-JYU(13)  +
                         RECE41-KINGAKU-JYU(14)  +
                         RECE41-KINGAKU-JYU(15)  +
                         RECE41-KINGAKU-JYU(16)  +
                         RECE41-KINGAKU-JYU(17)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(22)  =
                         RECE41-KINGAKU-JYU(19)  +
                         RECE41-KINGAKU-JYU(20)  +
                         RECE41-KINGAKU-JYU(21)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(25)  =
                         RECE41-KINGAKU-JYU(23)  +
                         RECE41-KINGAKU-JYU(24)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(28)  =
                         RECE41-KINGAKU-JYU(26)  +
                         RECE41-KINGAKU-JYU(27)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(31)  =
                         RECE41-KINGAKU-JYU(29)  +
                         RECE41-KINGAKU-JYU(30)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(34)  =
                         RECE41-KINGAKU-JYU(32)  +
                         RECE41-KINGAKU-JYU(33)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(38)  =
                         RECE41-KINGAKU-JYU(35)  +
                         RECE41-KINGAKU-JYU(36)  +
                         RECE41-KINGAKU-JYU(37)   
      *
             END-EVALUATE
      *
           END-IF                                                       
      *
           IF   RECE41-HKNKBN     =   "6"
      *
               COMPUTE   RECE41-TENSU(9)      =  RECE41-TENSU(1)
                                              +  RECE41-TENSU(2)
                                              +  RECE41-TENSU(3)
                                              +  RECE41-TENSU(4)
                                              +  RECE41-TENSU(5)
                                              +  RECE41-TENSU(6)
                                              +  RECE41-TENSU(7)
                                              +  RECE41-TENSU(8)
      *
               COMPUTE   RECE41-TENSU(18)     =  RECE41-TENSU(10)
                                              +  RECE41-TENSU(11)
                                              +  RECE41-TENSU(12)
                                              +  RECE41-TENSU(13)
                                              +  RECE41-TENSU(14)
                                              +  RECE41-TENSU(15)
                                              +  RECE41-TENSU(16)
                                              +  RECE41-TENSU(17)
      *
               COMPUTE   RECE41-TENSU(22)     =  RECE41-TENSU(19)
                                              +  RECE41-TENSU(20)
                                              +  RECE41-TENSU(21)
      *
               COMPUTE   RECE41-TENSU(25)     =  RECE41-TENSU(23)
                                              +  RECE41-TENSU(24)
      *
               COMPUTE   RECE41-TENSU(28)     =  RECE41-TENSU(26)
                                              +  RECE41-TENSU(27)
      *
               COMPUTE   RECE41-TENSU(31)     =  RECE41-TENSU(29)
                                              +  RECE41-TENSU(30)
      *
               COMPUTE   RECE41-TENSU(34)     =  RECE41-TENSU(32)
                                              +  RECE41-TENSU(33)
      *
               COMPUTE   RECE41-TENSU(38)     =  RECE41-TENSU(35)
                                              +  RECE41-TENSU(36)
                                              +  RECE41-TENSU(37)
      *
               MOVE       ZERO         TO        WRK-SYUTEN-JI
                                                 WRK-YKZTEN-JI
               PERFORM    VARYING    ID1    FROM    1   BY   1
                           UNTIL     ID1     >     31
                  ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
                                                   WRK-SYUTEN-JI
                  ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN
                                                   WRK-YKZTEN-JI
               END-PERFORM
      *        ¸ºÅÀÊÔ½¸
               COMPUTE   RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                          -  WRK-RECE4G-TENSU(2)
                                          -  WRK-RECE4G-TENSU(3)
                                          -  WRK-RECE4G-TENSU(5)
                                          -  WRK-RECE4G-TENSU(6)
                                          -  WRK-RECE4G-TENSU(7)
                                          -  WRK-RECE4G-TENSU(8)
                                          -  WRK-RECE4G-TENSU(9)
                                          -  WRK-RECE4G-TENSU(12)
                                          -  WRK-RECE4G-TENSU(14)
               COMPUTE   WRK-SYUTEN-JI    =  WRK-SYUTEN-JI
                                          -  WRK-RECE4G-TENSU(2)
                                          -  WRK-RECE4G-TENSU(3)
                                          -  WRK-RECE4G-TENSU(5)
                                          -  WRK-RECE4G-TENSU(6)
                                          -  WRK-RECE4G-TENSU(7)
                                          -  WRK-RECE4G-TENSU(8)
                                          -  WRK-RECE4G-TENSU(9)
                                          -  WRK-RECE4G-TENSU(12)
                                          -  WRK-RECE4G-TENSU(14)
      *
               IF   TBL-SYUTEN-JI(2)  >=  WRK-RECE4G-TENSU(2)
                 COMPUTE  TBL-SYUTEN-JI(2) =
                          TBL-SYUTEN-JI(2) -  WRK-RECE4G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(2)
               END-IF
               IF   TBL-SYUTEN-JI(7)  >=  WRK-RECE4G-TENSU(7)
                 COMPUTE  TBL-SYUTEN-JI(7) =
                          TBL-SYUTEN-JI(7) -  WRK-RECE4G-TENSU(7)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(7)
               END-IF
               IF   TBL-SYUTEN-JI(8)  >=  WRK-RECE4G-TENSU(8)
                 COMPUTE  TBL-SYUTEN-JI(8) =
                          TBL-SYUTEN-JI(8) -  WRK-RECE4G-TENSU(8)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(8)
               END-IF
      *
               COMPUTE  RECE41-TOTALKIN-JYU   =
                        RECE41-TOTALKIN-JYU   +
                       (WRK-SYUTEN-JI  *  RECE41-SYUTENTANKA)
               COMPUTE  RECE41-TOTALKIN-JYU   =
                        RECE41-TOTALKIN-JYU   +
                       (WRK-YKZTEN-JI  *  RECE41-YKZTENTANKA)
      ***
               PERFORM    VARYING    ID1    FROM    1   BY   1
                           UNTIL     ID1     >     38
                 IF   ID1  =   9  OR  18  OR  22  OR  25  OR
                              28  OR  31  OR  34  OR  38
                    CONTINUE
                 ELSE
                    COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                             RECE41-KINGAKU-JYU(ID1) +
                            (TBL-SYUTEN-JI(ID1) * RECE41-SYUTENTANKA)
                    COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                             RECE41-KINGAKU-JYU(ID1) +
                            (TBL-YKZTEN-JI(ID1) * RECE41-YKZTENTANKA)
                 END-IF
               END-PERFORM
      *
               COMPUTE   RECE41-KINGAKU-JYU(9)  =
                         RECE41-KINGAKU-JYU(1)  +
                         RECE41-KINGAKU-JYU(2)  +
                         RECE41-KINGAKU-JYU(3)  +
                         RECE41-KINGAKU-JYU(4)  +
                         RECE41-KINGAKU-JYU(5)  +
                         RECE41-KINGAKU-JYU(6)  +
                         RECE41-KINGAKU-JYU(7)  +
                         RECE41-KINGAKU-JYU(8)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(18)  =
                         RECE41-KINGAKU-JYU(10)  +
                         RECE41-KINGAKU-JYU(11)  +
                         RECE41-KINGAKU-JYU(12)  +
                         RECE41-KINGAKU-JYU(13)  +
                         RECE41-KINGAKU-JYU(14)  +
                         RECE41-KINGAKU-JYU(15)  +
                         RECE41-KINGAKU-JYU(16)  +
                         RECE41-KINGAKU-JYU(17)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(22)  =
                         RECE41-KINGAKU-JYU(19)  +
                         RECE41-KINGAKU-JYU(20)  +
                         RECE41-KINGAKU-JYU(21)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(25)  =
                         RECE41-KINGAKU-JYU(23)  +
                         RECE41-KINGAKU-JYU(24)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(28)  =
                         RECE41-KINGAKU-JYU(26)  +
                         RECE41-KINGAKU-JYU(27)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(31)  =
                         RECE41-KINGAKU-JYU(29)  +
                         RECE41-KINGAKU-JYU(30)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(34)  =
                         RECE41-KINGAKU-JYU(32)  +
                         RECE41-KINGAKU-JYU(33)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(38)  =
                         RECE41-KINGAKU-JYU(35)  +
                         RECE41-KINGAKU-JYU(36)  +
                         RECE41-KINGAKU-JYU(37)   
      *
               IF    RECE41-DAISAN-CHIKAN   =   1
                 IF  RECE41-TOTALKIN-JYU < RECE41-FTNMONEY
                   MOVE   RECE41-TOTALKIN-JYU   TO
                                           RECE41-FTNMONEY
                 END-IF
               END-IF
      *
      *        Âè»°¼Ô¹Ô°Ù¤Î¸½ÊªµëÉÕÈ½Äê
               PERFORM   21053-DAISAN-GENBUTU-HANTEI-SEC
      *
               EVALUATE  RECE41-DAISAN-GENBUTU-GAITO
                 WHEN      ZERO
                   IF    RECE41-DAISAN-CHIKAN   =  ZERO
                     IF   WRK-DAISAN-DAISANKBN  =  "1"
                       PERFORM   21051-DAISAN-CHECK-SEC
                       MOVE  WRK-DAISAN-FTNMONEY   TO  RECE41-FTNMONEY
                     ELSE
                       IF   RECE41-TOTALKIN-JYU  NOT =  ZERO
                       AND  RECE41-FTNRATE       NOT =  ZERO
                         COMPUTE RECE41-FTNMONEY = RECE41-TOTALKIN-JYU
                                                 * RECE41-FTNRATE
                                                 / 100
                       END-IF
                     END-IF
                   END-IF
                 WHEN       1
                   IF    RECE41-DAISAN-CHIKAN   =  ZERO
                     PERFORM     21052-DAISAN-CHECK2-SEC
                     MOVE    WRK-DAISAN-FTNMONEY   TO  RECE41-FTNMONEY
                   END-IF
               END-EVALUATE
               COMPUTE   RECE41-SKYMONEY = RECE41-TOTALKIN-JYU
                                         - RECE41-FTNMONEY
      *
           END-IF                                                       
      *
           REWRITE   RECE41-REC
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       2105-RECE41-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âè»°¼Ô¹Ô°Ù¤Î°ìÉôÉéÃ´¶â·×»»¡Ê£±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¤Î¾ì¹ç¡Ë
      *****************************************************************
       21051-DAISAN-CHECK-SEC      SECTION.
      *
           MOVE     ZERO     TO    WRK-DAISAN-TOTALTEN-SYUNOU
           MOVE     ZERO     TO    WRK-DAISAN-FTNMONEY
           MOVE     SPACE    TO    WRK-DAISAN-ACCT-UPDKBN
      *
           INITIALIZE                       SYUNOU-REC
           MOVE  RECE41-HOSPNUM         TO  SYU-HOSPNUM
           MOVE  RECE41-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE  RECE41-PTID            TO  SYU-PTID
           MOVE  RECE41-SRYYM           TO  SYU-SRYYMD(1:6)
           MOVE  "__"                   TO  SYU-SRYYMD(7:2)
           MOVE  RECE41-HKNCOMBI        TO  SYU-HKNCOMBINUM
           MOVE  SYUNOU-REC             TO  MCPDATA-REC
           MOVE  "tbl_syunou"           TO  MCP-TABLE
           MOVE  "key41"                TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    MCP-RC   =   ZERO
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    "key41"            TO  MCP-PATHNAME
             PERFORM  800-SYUNOU-READ-SEC
             PERFORM  UNTIL  (FLG-SYUNOU  =  1)
               IF    SYU-DENPJTIKBN    NOT =  "3"
                 IF  SYU-TOTAL-HKNTEN  NOT = ZERO
                   ADD  SYU-TOTAL-HKNTEN
                              TO  WRK-DAISAN-TOTALTEN-SYUNOU
      *
      ***          COMPUTE  WRK-DAISAN-FTNMONEY2 = SYU-TOTAL-HKNTEN
      ***                                        * 10
      ***                                        * RECE41-FTNRATE
      ***                                        / 100
      ***          COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***                   WRK-DAISAN-FTNMONEY2 +  5
      ***          COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***                   WRK-DAISAN-FTNMONEY2 /  10
      ***          COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***                   WRK-DAISAN-FTNMONEY2 *  10
      *            Ê£¿ô²Ê¤Þ¤È¤á¤ÎÂÐ±þ
                   COMPUTE  WRK-DAISAN-FTNMONEY2 =
                            SYU-SYUCOMPFTN       +  5
                   COMPUTE  WRK-DAISAN-FTNMONEY2 =
                            WRK-DAISAN-FTNMONEY2 /  10
                   COMPUTE  WRK-DAISAN-FTNMONEY2 =
                            WRK-DAISAN-FTNMONEY2 *  10
      *
                   COMPUTE  WRK-DAISAN-FTNMONEY  =
                            WRK-DAISAN-FTNMONEY  +
                            WRK-DAISAN-FTNMONEY2
                 END-IF
                 IF  SYU-ACCT-UPDKBN  NOT =  SPACE
                   MOVE  "1"  TO  WRK-DAISAN-ACCT-UPDKBN
                 END-IF
               END-IF
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key41"          TO  MCP-PATHNAME
               PERFORM  800-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE      "tbl_syunou"       TO  MCP-TABLE
           MOVE      "key41"            TO  MCP-PATHNAME
           PERFORM    900-CLOSE-SEC
      *
      *    ¥ê¥¢¥ë¥¿¥¤¥à¥×¥ì¥Ó¥å¡¼»þ
      ***  IF   (WRK-PARA-SHELLID        =   "RECEPTX")
      ***  AND  (WRK-DAISAN-ACCT-UPDKBN  =    SPACE   )
      *
      ***    COMPUTE WRK-DAISAN-TOTALTEN = RECE41-TOTALKIN-JYU
      ***                                / 10
      ***    IF   WRK-DAISAN-TOTALTEN  >  WRK-DAISAN-TOTALTEN-SYUNOU
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***         (WRK-DAISAN-TOTALTEN - WRK-DAISAN-TOTALTEN-SYUNOU)
      ***                                    * 10
      ***                                    * RECE41-FTNRATE
      ***                                    / 100
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***               WRK-DAISAN-FTNMONEY2 +  5
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***               WRK-DAISAN-FTNMONEY2 /  10
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***               WRK-DAISAN-FTNMONEY2 *  10
      *
      ***      COMPUTE  WRK-DAISAN-FTNMONEY  =
      ***               WRK-DAISAN-FTNMONEY  +
      ***               WRK-DAISAN-FTNMONEY2
      ***    END-IF
      *
      ***  END-IF
      *
           .
       21051-DAISAN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âè»°¼Ô¹Ô°Ù¤Î°ìÉôÉéÃ´¶â·×»»¡ã¸½ÊªµëÉÕ¡ä
      *****************************************************************
       21052-DAISAN-CHECK2-SEC      SECTION.
      *
           MOVE     ZERO     TO    WRK-DAISAN-TOTALTEN-SYUNOU
           MOVE     ZERO     TO    WRK-DAISAN-FTNMONEY
           MOVE     SPACE    TO    WRK-DAISAN-ACCT-UPDKBN
      *
           INITIALIZE                       SYUNOU-REC
           MOVE  RECE41-HOSPNUM         TO  SYU-HOSPNUM
           MOVE  RECE41-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE  RECE41-PTID            TO  SYU-PTID
           MOVE  RECE41-SRYYM           TO  SYU-SRYYMD(1:6)
           MOVE  "__"                   TO  SYU-SRYYMD(7:2)
           MOVE  RECE41-HKNCOMBI        TO  SYU-HKNCOMBINUM
           MOVE  SYUNOU-REC             TO  MCPDATA-REC
           MOVE  "tbl_syunou"           TO  MCP-TABLE
           MOVE  "key41"                TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    MCP-RC   =   ZERO
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    "key41"            TO  MCP-PATHNAME
             PERFORM  800-SYUNOU-READ-SEC
             PERFORM  UNTIL  (FLG-SYUNOU  =  1)
               IF    SYU-DENPJTIKBN    NOT =  "3"
                 IF  SYU-TOTAL-HKNTEN  NOT = ZERO
                   ADD  SYU-TOTAL-HKNTEN
                              TO  WRK-DAISAN-TOTALTEN-SYUNOU
      *
                   COMPUTE  WRK-DAISAN-FTNMONEY  =
                            WRK-DAISAN-FTNMONEY  +
                            SYU-SYUCOMPFTN
                 END-IF
                 IF  SYU-ACCT-UPDKBN  NOT =  SPACE
                   MOVE  "1"  TO  WRK-DAISAN-ACCT-UPDKBN
                 END-IF
               END-IF
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key41"          TO  MCP-PATHNAME
               PERFORM  800-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE      "tbl_syunou"       TO  MCP-TABLE
           MOVE      "key41"            TO  MCP-PATHNAME
           PERFORM    900-CLOSE-SEC
      *
      *    ¥ê¥¢¥ë¥¿¥¤¥à¥×¥ì¥Ó¥å¡¼»þ
      ***  IF   (WRK-PARA-SHELLID        =   "RECEPTX")
      ***  AND  (WRK-DAISAN-ACCT-UPDKBN  =    SPACE   )
      *
      ***    COMPUTE WRK-DAISAN-TOTALTEN = RECE41-TOTALKIN-JYU
      ***                                / 10
      ***    IF   WRK-DAISAN-TOTALTEN  >  WRK-DAISAN-TOTALTEN-SYUNOU
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***         (WRK-DAISAN-TOTALTEN - WRK-DAISAN-TOTALTEN-SYUNOU)
      ***                                    * 10
      ***                                    * RECE41-FTNRATE
      ***                                    / 100
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***               WRK-DAISAN-FTNMONEY2 +  5
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***               WRK-DAISAN-FTNMONEY2 /  10
      ***      COMPUTE  WRK-DAISAN-FTNMONEY2 =
      ***               WRK-DAISAN-FTNMONEY2 *  10
      *
      ***      COMPUTE  WRK-DAISAN-FTNMONEY  =
      ***               WRK-DAISAN-FTNMONEY  +
      ***               WRK-DAISAN-FTNMONEY2
      ***    END-IF
      *
      ***  END-IF
      *
           .
       21052-DAISAN-CHECK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âè»°¼Ô¹Ô°Ù¤Î¸½ÊªµëÉÕÈ½Äê
      *****************************************************************
       21053-DAISAN-GENBUTU-HANTEI-SEC      SECTION.
      *
      *    ¸½ÊªµëÉÕ·×»»ÂÐ¾Ý¤«¥Á¥§¥Ã¥¯
      *    Á´¤Æ¤Î¼ýÇ¼¤¬¸½ÊªµëÉÕ·×»»¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯
           MOVE     ZERO       TO     FLG-DAISAN-GENBUTU
           IF    RECE41-DAISAN-GENBUTU-GAITO   =   1
      *
             INITIALIZE                       SYUNOU-REC
             MOVE  RECE41-HOSPNUM         TO  SYU-HOSPNUM
             MOVE  RECE41-NYUGAIKBN       TO  SYU-NYUGAIKBN
             MOVE  RECE41-PTID            TO  SYU-PTID
             MOVE  RECE41-SRYYM           TO  SYU-SRYYMD(1:6)
             MOVE  "__"                   TO  SYU-SRYYMD(7:2)
             MOVE  RECE41-HKNCOMBI        TO  SYU-HKNCOMBINUM
             MOVE  SYUNOU-REC             TO  MCPDATA-REC
             MOVE  "tbl_syunou"           TO  MCP-TABLE
             MOVE  "key41"                TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF    MCP-RC   =   ZERO
               MOVE    "tbl_syunou"       TO  MCP-TABLE
               MOVE    "key41"            TO  MCP-PATHNAME
               PERFORM  800-SYUNOU-READ-SEC
               PERFORM  UNTIL  (FLG-SYUNOU  =  1)
                 IF    SYU-DENPJTIKBN    NOT =  "3"
                   IF  SYU-TOTAL-HKNTEN  NOT = ZERO
                   AND SYU-SKJ-FTNGAKU(2)    = ZERO
                     MOVE     1           TO  FLG-DAISAN-GENBUTU
                   END-IF
                 END-IF
                 MOVE    "tbl_syunou"     TO  MCP-TABLE
                 MOVE    "key41"          TO  MCP-PATHNAME
                 PERFORM  800-SYUNOU-READ-SEC
               END-PERFORM
             END-IF
             MOVE      "tbl_syunou"       TO  MCP-TABLE
             MOVE      "key41"            TO  MCP-PATHNAME
             PERFORM    900-CLOSE-SEC
      *
             IF   FLG-DAISAN-GENBUTU    =   1
               MOVE     ZERO       TO     RECE41-DAISAN-GENBUTU-GAITO
             END-IF
      *
           END-IF
      *
           .
       21053-DAISAN-GENBUTU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕ¡Ê¿·ÍÍ¼°¡Ë¡Ê£Ï£Ã£ÒÍÍ¼°¡Ë¤Î¶â³ÛÊÔ½¸
      *****************************************************************
       21054-JIBAI-KINGAKU-HENSYU-SEC      SECTION.
      *
           COMPUTE  WRK-JIBAI-SKYKIN-A   =  RECE41-TOTALKIN
                                         +  RECE41-TOTALKIN-YAK
                                         +  RECE41-TOTALKIN2
                                         +  RECE41-TOTALKIN2-ETC
      *
           MOVE     WRK-JIBAI-SKYKIN-A  TO  RECE41-JIBAI-GOKEI-BE
                                            RECE41-JIBAI-GOKEI-AF
      *
           IF       WRK-JIBAI-SKYKIN-A   >  ZERO
             EVALUATE   RECE41-JIBAI-HASUKBN
               WHEN       "1"
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  +   5
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  /  10
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  *  10
               WHEN       "2"
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  /  10
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  *  10
               WHEN       "3"
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  +   9
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  /  10
                  COMPUTE  WRK-JIBAI-SKYKIN-A  =
                           WRK-JIBAI-SKYKIN-A  *  10
             END-EVALUATE
             IF   RECE41-SEIKYUKBN    =  "1"
               MOVE  1                   TO  RECE41-JIBAI-JYUKINFLG
               MOVE  WRK-JIBAI-SKYKIN-A  TO  RECE41-JIBAI-JYUKIN
             ELSE
               MOVE  1                   TO  RECE41-JIBAI-SKYKINFLG
               MOVE  WRK-JIBAI-SKYKIN-A  TO  RECE41-JIBAI-SKYKIN
             END-IF
             MOVE    WRK-JIBAI-SKYKIN-A  TO  RECE41-JIBAI-GOKEI-AF
           END-IF
      *
           IF       RECE41-JIBAI-CHIKANKINFLG  =  1
             IF  WRK-JIBAI-SKYKIN-A  >  ZERO
               IF   WRK-JIBAI-SKYKIN-A  <  RECE41-JIBAI-CHIKANKIN
                 MOVE    WRK-JIBAI-SKYKIN-A  TO  RECE41-JIBAI-CHIKANKIN
               END-IF
               IF   RECE41-JIBAI-CHIKANKIN  >  ZERO
                 MOVE    1                   TO  RECE41-JIBAI-JYUKINFLG
                 MOVE    RECE41-JIBAI-CHIKANKIN
                                             TO  RECE41-JIBAI-JYUKIN
                 COMPUTE WRK-JIBAI-SKYKIN-B = WRK-JIBAI-SKYKIN-A
                                            - RECE41-JIBAI-CHIKANKIN
                 IF    WRK-JIBAI-SKYKIN-B  >  ZERO
                   MOVE  1                   TO  RECE41-JIBAI-SKYKINFLG
                   MOVE  WRK-JIBAI-SKYKIN-B  TO  RECE41-JIBAI-SKYKIN
                 ELSE
                   MOVE  ZERO                TO  RECE41-JIBAI-SKYKINFLG
                   MOVE  ZERO                TO  RECE41-JIBAI-SKYKIN
                 END-IF
               ELSE
                 MOVE    ZERO                TO  RECE41-JIBAI-JYUKINFLG
                 MOVE    ZERO                TO  RECE41-JIBAI-JYUKIN
                 MOVE    1                   TO  RECE41-JIBAI-SKYKINFLG
                 MOVE    WRK-JIBAI-SKYKIN-A  TO  RECE41-JIBAI-SKYKIN
               END-IF
             ELSE
               MOVE      2        TO  RECE41-JIBAI-CHIKANKINFLG
             END-IF
           END-IF
      *
           IF       RECE41-JIBAI-CHIKANNAMEFLG  =  1
             IF  WRK-JIBAI-SKYKIN-A  >  ZERO
               IF     RECE41-JIBAI-JYUKINFLG      =  1
                 MOVE    1        TO  RECE41-JIBAI-JYUNAMEFLG
                 MOVE    RECE41-JIBAI-CHIKANNAME
                                  TO  RECE41-JIBAI-JYUNAME
               ELSE
                 MOVE    2        TO  RECE41-JIBAI-CHIKANNAMEFLG
               END-IF
             ELSE
                 MOVE    2        TO  RECE41-JIBAI-CHIKANNAMEFLG
             END-IF
           END-IF
      *
           .
       21054-JIBAI-KINGAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý¡Ê¸øÌ³ºÒ³²¡Ë
      *****************************************************************
       2106-KHNTEN-KO-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN7-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(7)
           END-IF
           IF        FLG-KHNTEN8-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(8)
           END-IF
           IF        FLG-KHNTEN17-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(17)
           END-IF
           IF        FLG-KHNTEN19-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(19)
           END-IF
           IF        FLG-KHNTEN21-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(21)
           END-IF
           IF        FLG-KHNTEN22-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(22)
           END-IF
      *
           IF     RECE41-KOMUKBN  =   "2"
             IF   ((CNT-SAISIN-ETC      NOT =  ZERO) AND
                   (CNT-SAISIN-DOUJITU  NOT =  ZERO))
             OR   ((CNT-SAISIN-ETC      NOT =  ZERO) AND
                   (CNT-SAISIN-ONLINE   NOT =  ZERO))
             OR   ((CNT-SAISIN-DOUJITU  NOT =  ZERO) AND
                   (CNT-SAISIN-ONLINE   NOT =  ZERO))
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
             END-IF
             IF   RECE41-SRYYM   >=  "201004"
               IF    (CNT-JIKANTAI1    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-JIKANTAI1)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-JIKANTAI2    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-JIKANTAI2)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-JIKANTAI3    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-JIKANTAI3)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-MESAIKSN     NOT =  ZERO        )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-MESAIKSN)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKATU    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKATU)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKAT2    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKAT2)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKAT3    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKAT3)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKAT4    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKAT4)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-YAKRENKEI    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-YAKRENKEI)
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
               IF    (CNT-NINPU        NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-NINPU    )
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
               END-IF
             END-IF
           ELSE
             IF   RECE41-SRYYM   >=  "201004"
             AND  RECE41-SRYYM   <=  "201803"
               IF  CNT-JIKANTAI1     NOT = ZERO
                 IF   CNT-JIKANTAI2  NOT = ZERO
                   IF   CNT-JIKANTAI1  NOT =  CNT-JIKANTAI2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-JIKANTAI1  NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-MESAIKSN   NOT = ZERO
                   IF   CNT-JIKANTAI1  NOT =  CNT-MESAIKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKATU  NOT = ZERO
                   IF   CNT-JIKANTAI1  NOT =  CNT-CHIHOKATU
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKAT2  NOT = ZERO
                   IF   CNT-JIKANTAI1  NOT =  CNT-CHIHOKAT2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN  NOT = ZERO
                   IF   CNT-JIKANTAI1  NOT =  CNT-SAITOKKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN2 NOT = ZERO
                   IF   CNT-JIKANTAI1  NOT =  CNT-SAITOKKSN2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
               IF  CNT-JIKANTAI2     NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT = ZERO
                   IF   CNT-JIKANTAI2  NOT =  CNT-JIKANTAI1
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-JIKANTAI2  NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-MESAIKSN   NOT = ZERO
                   IF   CNT-JIKANTAI2  NOT =  CNT-MESAIKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKATU  NOT = ZERO
                   IF   CNT-JIKANTAI2  NOT =  CNT-CHIHOKATU
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKAT2  NOT = ZERO
                   IF   CNT-JIKANTAI2  NOT =  CNT-CHIHOKAT2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN  NOT = ZERO
                   IF   CNT-JIKANTAI2  NOT =  CNT-SAITOKKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN2 NOT = ZERO
                   IF   CNT-JIKANTAI2  NOT =  CNT-SAITOKKSN2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
               IF  CNT-JIKANTAI3     NOT = ZERO
                 IF   CNT-JIKANTAI2  NOT = ZERO
                   IF   CNT-JIKANTAI3  NOT =  CNT-JIKANTAI2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-JIKANTAI3  NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-MESAIKSN   NOT = ZERO
                   IF   CNT-JIKANTAI3  NOT =  CNT-MESAIKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKATU  NOT = ZERO
                   IF   CNT-JIKANTAI3  NOT =  CNT-CHIHOKATU
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKAT2  NOT = ZERO
                   IF   CNT-JIKANTAI3  NOT =  CNT-CHIHOKAT2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN  NOT = ZERO
                   IF   CNT-JIKANTAI3  NOT =  CNT-SAITOKKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN2 NOT = ZERO
                   IF   CNT-JIKANTAI3  NOT =  CNT-SAITOKKSN2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
               IF  CNT-MESAIKSN      NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT = ZERO
                   IF   CNT-MESAIKSN   NOT =  CNT-JIKANTAI1
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI2  NOT = ZERO
                   IF   CNT-MESAIKSN   NOT =  CNT-JIKANTAI2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-MESAIKSN   NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKATU  NOT = ZERO
                   IF   CNT-MESAIKSN   NOT =  CNT-CHIHOKATU
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKAT2  NOT = ZERO
                   IF   CNT-MESAIKSN   NOT =  CNT-CHIHOKAT2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN  NOT = ZERO
                   IF   CNT-MESAIKSN   NOT =  CNT-SAITOKKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN2 NOT = ZERO
                   IF   CNT-MESAIKSN   NOT =  CNT-SAITOKKSN2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
               IF  CNT-CHIHOKATU     NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT = ZERO
                   IF   CNT-CHIHOKATU  NOT =  CNT-JIKANTAI1
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI2  NOT = ZERO
                   IF   CNT-CHIHOKATU  NOT =  CNT-JIKANTAI2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-CHIHOKATU  NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-MESAIKSN   NOT = ZERO
                   IF   CNT-CHIHOKATU  NOT =  CNT-MESAIKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKAT2  NOT = ZERO
                   IF   CNT-CHIHOKATU  NOT =  CNT-CHIHOKAT2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN  NOT = ZERO
                   IF   CNT-CHIHOKATU  NOT =  CNT-SAITOKKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN2 NOT = ZERO
                   IF   CNT-CHIHOKATU  NOT =  CNT-SAITOKKSN2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
               IF  CNT-CHIHOKAT2     NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT = ZERO
                   IF   CNT-CHIHOKAT2  NOT =  CNT-JIKANTAI1
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI2  NOT = ZERO
                   IF   CNT-CHIHOKAT2  NOT =  CNT-JIKANTAI2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-CHIHOKAT2  NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-MESAIKSN   NOT = ZERO
                   IF   CNT-CHIHOKAT2  NOT =  CNT-MESAIKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKATU  NOT = ZERO
                   IF   CNT-CHIHOKAT2  NOT =  CNT-CHIHOKATU
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN  NOT = ZERO
                   IF   CNT-CHIHOKAT2  NOT =  CNT-SAITOKKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN2 NOT = ZERO
                   IF   CNT-CHIHOKAT2  NOT =  CNT-SAITOKKSN2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
               IF  CNT-SAITOKKSN     NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT = ZERO
                   IF   CNT-SAITOKKSN  NOT =  CNT-JIKANTAI1
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI2  NOT = ZERO
                   IF   CNT-SAITOKKSN  NOT =  CNT-JIKANTAI2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-SAITOKKSN  NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-MESAIKSN   NOT = ZERO
                   IF   CNT-SAITOKKSN  NOT =  CNT-MESAIKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKATU  NOT = ZERO
                   IF   CNT-SAITOKKSN  NOT =  CNT-CHIHOKATU
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKAT2  NOT = ZERO
                   IF   CNT-SAITOKKSN  NOT =  CNT-CHIHOKAT2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN2 NOT = ZERO
                   IF   CNT-SAITOKKSN  NOT =  CNT-SAITOKKSN2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
               IF  CNT-SAITOKKSN2    NOT = ZERO
                 IF   CNT-JIKANTAI1  NOT = ZERO
                   IF   CNT-SAITOKKSN2 NOT =  CNT-JIKANTAI1
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI2  NOT = ZERO
                   IF   CNT-SAITOKKSN2 NOT =  CNT-JIKANTAI2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-JIKANTAI3  NOT = ZERO
                   IF   CNT-SAITOKKSN2 NOT =  CNT-JIKANTAI3
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-MESAIKSN   NOT = ZERO
                   IF   CNT-SAITOKKSN2 NOT =  CNT-MESAIKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKATU  NOT = ZERO
                   IF   CNT-SAITOKKSN2 NOT =  CNT-CHIHOKATU
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-CHIHOKAT2  NOT = ZERO
                   IF   CNT-SAITOKKSN2 NOT =  CNT-CHIHOKAT2
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
                 IF   CNT-SAITOKKSN  NOT = ZERO
                   IF   CNT-SAITOKKSN2 NOT =  CNT-SAITOKKSN
                     MOVE   ZERO         TO   RECE41-KHNTEN(2)
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           IF    (CNT-SAISIN-GAI-ETC      NOT =  ZERO)
           AND   (CNT-SAISIN-GAI-DOUJITU  NOT =  ZERO)
                   MOVE   ZERO         TO   RECE41-KHNTEN(3)
           END-IF
           IF    (CNT-NINPU2           NOT =  ZERO      )
           AND   (CNT-SAISIN-GAI-ETC   NOT =  CNT-NINPU2)
                   MOVE   ZERO         TO   RECE41-KHNTEN(3)
           END-IF
      *
           .
       2106-KHNTEN-KO-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÅÀ¿ô¤Î¼£¸³¸ºÅÀÊ¬ÊÔ½¸
      *****************************************************************
       2107-GENTEN-HENSYU-SEC      SECTION.
      *
           IF   RECE41-TENSU(2)  >=  WRK-RECE4G-TENSU(2)
             COMPUTE  RECE41-TENSU(2)  =
                      RECE41-TENSU(2)  -  WRK-RECE4G-TENSU(2)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(2)
           END-IF
           IF   RECE41-TENSU(3)  >=  WRK-RECE4G-TENSU(3)
             COMPUTE  RECE41-TENSU(3)  =
                      RECE41-TENSU(3)  -  WRK-RECE4G-TENSU(3)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(3)
           END-IF
           IF   RECE41-TENSU(5)  >=  WRK-RECE4G-TENSU(5)
             COMPUTE  RECE41-TENSU(5)  =
                      RECE41-TENSU(5)  -  WRK-RECE4G-TENSU(5)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(5)
           END-IF
           IF   RECE41-TENSU(6)  >=  WRK-RECE4G-TENSU(6)
             COMPUTE  RECE41-TENSU(6)  =
                      RECE41-TENSU(6)  -  WRK-RECE4G-TENSU(6)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(6)
           END-IF
           IF   RECE41-TENSU(7)  >=  WRK-RECE4G-TENSU(7)
             COMPUTE  RECE41-TENSU(7)  =
                      RECE41-TENSU(7)  -  WRK-RECE4G-TENSU(7)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(7)
           END-IF
           IF   RECE41-TENSU(8)  >=  WRK-RECE4G-TENSU(8)
             COMPUTE  RECE41-TENSU(8)  =
                      RECE41-TENSU(8)  -  WRK-RECE4G-TENSU(8)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(8)
           END-IF
           IF   RECE41-TENSU(9)  >=  WRK-RECE4G-TENSU(9)
             COMPUTE  RECE41-TENSU(9)  =
                      RECE41-TENSU(9)  -  WRK-RECE4G-TENSU(9)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(9)
           END-IF
           IF   RECE41-TENSU(12) >=  WRK-RECE4G-TENSU(12)
             COMPUTE  RECE41-TENSU(12) =
                      RECE41-TENSU(12) -  WRK-RECE4G-TENSU(12)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(12)
           END-IF
           IF   RECE41-TENSU(14) >=  WRK-RECE4G-TENSU(14)
             COMPUTE  RECE41-TENSU(14) =
                      RECE41-TENSU(14) -  WRK-RECE4G-TENSU(14)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(14)
           END-IF
      *
      *    ¹ç·×ÅÀ¿ô¤ÎÊÔ½¸
           IF   RECE41-HKNKBN     NOT  =   "4" AND "6"
              COMPUTE  RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                        -  WRK-RECE4G-TENSU(2)
                                        -  WRK-RECE4G-TENSU(3)
                                        -  WRK-RECE4G-TENSU(5)
                                        -  WRK-RECE4G-TENSU(6)
                                        -  WRK-RECE4G-TENSU(7)
                                        -  WRK-RECE4G-TENSU(8)
                                        -  WRK-RECE4G-TENSU(9)
                                        -  WRK-RECE4G-TENSU(12)
                                        -  WRK-RECE4G-TENSU(14)
           END-IF
      *
           .
       2107-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð²ó¿ô¤ÎºÆÊÔ½¸
      *****************************************************************
       2108-ZAITAKU-KAISU-HENSYU-SEC               SECTION.
      *
           IF    RECE41-SRYYM   >=  "201404"
             EVALUATE   RECE41-HKNKBN
               WHEN      "1"
               WHEN      "2"
      *          ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡Ê±ý¿Ç¡Ë¤Ë´Ø¤¹¤ë²ó¿ô¤ÎºÆÊÔ½¸
                 PERFORM  VARYING  IDX19  FROM  1  BY  1
                           UNTIL   IDX19   >    9
                   PERFORM  VARYING  IDX20  FROM  1  BY  1
                             UNTIL   IDX20   >   31
                     IF  TBL-ZAITAKU-KAISU(1 IDX19 IDX20) NOT = ZERO
                     AND TBL-ZAITAKU-KAISU(2 IDX19 IDX20) NOT = ZERO
                       COMPUTE  RECE41-KAISU(8) =
                                RECE41-KAISU(8) -
                                TBL-ZAITAKU-KAISU(2 IDX19 IDX20)
                     END-IF
                   END-PERFORM
                 END-PERFORM
      *          ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡ÊÆÃÊÌ±ý¿Ç¡Ë¤Ë´Ø¤¹¤ë²ó¿ô¤ÎºÆÊÔ½¸
                 PERFORM  VARYING  IDX19  FROM  1  BY  1
                           UNTIL   IDX19   >    9
                   PERFORM  VARYING  IDX20  FROM  1  BY  1
                             UNTIL   IDX20   >   31
                     IF  TBL-ZAITAKU-KAISU(3 IDX19 IDX20) NOT = ZERO
                     AND TBL-ZAITAKU-KAISU(4 IDX19 IDX20) NOT = ZERO
                       COMPUTE  RECE41-KAISU(8) =
                                RECE41-KAISU(8) -
                                TBL-ZAITAKU-KAISU(4 IDX19 IDX20)
                     END-IF
                   END-PERFORM
                 END-PERFORM
      *          ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡ÊºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ë¤Ë´Ø¤¹¤ë²ó¿ô¤ÎºÆÊÔ½¸
                 PERFORM  VARYING  IDX19  FROM  1  BY  1
                           UNTIL   IDX19   >    9
                   PERFORM  VARYING  IDX20  FROM  1  BY  1
                             UNTIL   IDX20   >   31
                     IF  TBL-ZAITAKU-KAISU(5 IDX19 IDX20) NOT = ZERO
                     AND TBL-ZAITAKU-KAISU(6 IDX19 IDX20) NOT = ZERO
                       COMPUTE  RECE41-KAISU(11) =
                                RECE41-KAISU(11) -
                                TBL-ZAITAKU-KAISU(6 IDX19 IDX20)
                     END-IF
                   END-PERFORM
                 END-PERFORM
               WHEN      "5"
      *          ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡Ê±ý¿Ç¡Ë¤Ë´Ø¤¹¤ë²ó¿ô¤ÎºÆÊÔ½¸
                 PERFORM  VARYING  IDX19  FROM  1  BY  1
                           UNTIL   IDX19   >    9
                   PERFORM  VARYING  IDX20  FROM  1  BY  1
                             UNTIL   IDX20   >   31
                     IF  TBL-ZAITAKU-KAISU(1 IDX19 IDX20) NOT = ZERO
                     AND TBL-ZAITAKU-KAISU(2 IDX19 IDX20) NOT = ZERO
                       COMPUTE  RECE41-KAISU(10) =
                                RECE41-KAISU(10) -
                                TBL-ZAITAKU-KAISU(2 IDX19 IDX20)
                     END-IF
                   END-PERFORM
                 END-PERFORM
      *          ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡ÊÆÃÊÌ±ý¿Ç¡Ë¤Ë´Ø¤¹¤ë²ó¿ô¤ÎºÆÊÔ½¸
                 PERFORM  VARYING  IDX19  FROM  1  BY  1
                           UNTIL   IDX19   >    9
                   PERFORM  VARYING  IDX20  FROM  1  BY  1
                             UNTIL   IDX20   >   31
                     IF  TBL-ZAITAKU-KAISU(3 IDX19 IDX20) NOT = ZERO
                     AND TBL-ZAITAKU-KAISU(4 IDX19 IDX20) NOT = ZERO
                       COMPUTE  RECE41-KAISU(10) =
                                RECE41-KAISU(10) -
                                TBL-ZAITAKU-KAISU(4 IDX19 IDX20)
                     END-IF
                   END-PERFORM
                 END-PERFORM
      *          ´µ²È¿ÇÎÅ»þ´Ö²Ã»»¡ÊºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ë¤Ë´Ø¤¹¤ë²ó¿ô¤ÎºÆÊÔ½¸
                 PERFORM  VARYING  IDX19  FROM  1  BY  1
                           UNTIL   IDX19   >    9
                   PERFORM  VARYING  IDX20  FROM  1  BY  1
                             UNTIL   IDX20   >   31
                     IF  TBL-ZAITAKU-KAISU(5 IDX19 IDX20) NOT = ZERO
                     AND TBL-ZAITAKU-KAISU(6 IDX19 IDX20) NOT = ZERO
                       COMPUTE  RECE41-KAISU(13) =
                                RECE41-KAISU(13) -
                                TBL-ZAITAKU-KAISU(6 IDX19 IDX20)
                     END-IF
                   END-PERFORM
                 END-PERFORM
             END-EVALUATE
           END-IF
      *
           .
       2108-ZAITAKU-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕ¡¦ÀÁµá¶â³ÛÍóÃ¼¿ô½èÍý¶èÊ¬ÊÔ½¸
      *****************************************************************
       2109-JIBAI-HASUKBN-HENSYU-SEC               SECTION.
      *
           EVALUATE   RECE41-HKNKBN
             WHEN       "4"
               EVALUATE   SYS-200501-JIBAI-FRACTION-KBN(1)
                 WHEN      "1"
                   IF   WRK-HASU1001-JIBAI   NOT = "4"
                     MOVE  WRK-HASU1001-JIBAI   TO  RECE41-JIBAI-HASUKBN
                   END-IF
                 WHEN      "2"
                   IF   RECE41-SEIKYUKBN         = "1"
                   AND  WRK-HASU1001-JIBAI   NOT = "4"
                     MOVE  WRK-HASU1001-JIBAI   TO  RECE41-JIBAI-HASUKBN
                   END-IF
               END-EVALUATE
             WHEN       "6"
               EVALUATE   SYS-200501-JIBAI-FRACTION-KBN(2)
                 WHEN      "1"
                   IF   WRK-HASU1001-DAISAN  NOT = "4"
                     MOVE  WRK-HASU1001-DAISAN  TO  RECE41-JIBAI-HASUKBN
                   END-IF
                 WHEN      "2"
                   IF   RECE41-SEIKYUKBN         = "1"
                   AND  WRK-HASU1001-DAISAN  NOT = "4"
                     MOVE  WRK-HASU1001-DAISAN  TO  RECE41-JIBAI-HASUKBN
                   END-IF
               END-EVALUATE
           END-EVALUATE
      *
           .
       2109-JIBAI-HASUKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE41-FILE
                   RECE44-FILE
                   RECE45-FILE
                   RECE4A-FILE
                   RECE4B-FILE
                   RECE44C-FILE
                   RECE45C-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE41-IN  :" CNT-IN   
           DISPLAY "RECE41-MOD :" CNT-OUT3  
           DISPLAY "RECE44-OUT :" CNT-OUT1 
           DISPLAY "RECE45-OUT :" CNT-OUT2  
           DISPLAY "RECE4A-OUT :" CNT-OUT4 
           DISPLAY "RECE4B-OUT :" CNT-OUT5  
           DISPLAY "RECE44C-OUT :" CNT-OUT6 
           DISPLAY "RECE45C-OUT :" CNT-OUT7  
           DISPLAY "*** ORCR0420 END ***"
      * 
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
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
      *            ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼»þ½ªÎ»½èÍý
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
      *    ½èÍýÃæ»ß½èÍý
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "½èÍý¤¬Ãæ»ß¤µ¤ì¤Þ¤·¤¿"
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
      *                ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
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
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       810-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       800-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       800-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       800-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       800-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊóÆÉ¹þ
      *****************************************************************
       800-SRYACCTPLUS-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       800-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       800-TENSU-TBL-SEC               SECTION.
      *
           MOVE      ZERO                TO   FLG-TENSU
           MOVE      SRY-SRYCD(IDX2)     TO   TTNS-CHK-SRYCD
           MOVE      SRY-SRYYM           TO   WK-TAIHI-SRYYM
           MOVE      WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           MOVE      WK-TAIHI-SRYYMD     TO   TTNS-CHK-YMD
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯
           PERFORM   8001-TTNS-CHK-SEC
      *
      *    ÂàÈò¥ï¡¼¥¯¤ËÂ¸ºß¤·¤Ê¤¤¾ì¹ç¤Ï£Ä£Â¤ò¸¡º÷
           IF    TTNS-FLG  =   ZERO
             INITIALIZE                       TNS-TENSU-REC
             MOVE    SPA-HOSPNUM         TO   TNS-HOSPNUM
             MOVE    SRY-SRYCD(IDX2)     TO   TNS-SRYCD
             MOVE    WK-TAIHI-SRYYMD     TO   TNS-YUKOSTYMD
                                              TNS-YUKOEDYMD
             MOVE    TNS-TENSU-REC       TO   MCPDATA-REC
             MOVE    "tbl_tensu"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF      MCP-RC     =  ZERO
                MOVE    "tbl_tensu"      TO   MCP-TABLE
                MOVE    "key"            TO   MCP-PATHNAME
                PERFORM  810-TENSU-READ-SEC
                IF    FLG-TENSU  =  1
                  DISPLAY  "TENSU FETCH ERR"
                ELSE
      *           ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¤ËÊÔ½¸¤¹¤ë
                  PERFORM   8001-TTNS-HEN-SEC
                END-IF
             ELSE
                DISPLAY    "TENSU SELECT ERR"
                INITIALIZE                    TNS-TENSU-REC
                MOVE    1                TO   FLG-TENSU
             END-IF
             MOVE    "tbl_tensu"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
           END-IF
      *
           .
       800-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       8001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM  VARYING  TTNS-IDX  FROM  1  BY  1
                     UNTIL  (TTNS-IDX    >   TTNS-MAX)
                      OR    (TTNS-FLG    =   1       )
             IF   (TTNS-KEY-SRYCD(TTNS-IDX)      =   TTNS-CHK-SRYCD)
             AND  (TTNS-KEY-YUKOSTYMD(TTNS-IDX) <=   TTNS-CHK-YMD  )
             AND  (TTNS-KEY-YUKOEDYMD(TTNS-IDX) >=   TTNS-CHK-YMD  )
               MOVE   1                    TO  TTNS-FLG
               MOVE   TTNS-DAT(TTNS-IDX)   TO  TNS-TENSU-REC
             END-IF
           END-PERFORM
      *
           .
       8001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       8001-TTNS-HEN-SEC               SECTION.
      *
           IF      SRY-SRYCD(IDX2)(1:1) = "6" OR "7"
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
       8001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       810-TENSU-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       810-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       800-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTCOM-REC
                   MOVE    ZERO            TO  FLG-PTCOM
               ELSE
                   MOVE    1               TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                      PTCOM-REC
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
                        "¥³¥á¥ó¥ÈÊ¸»ú¥³¡¼¥ÉÉÔÀµ¡¡´µ¼ÔÈÖ¹æ="
                                                   DELIMITED  BY  SIZE
                               RECE41-PTNUM        DELIMITED  BY  SPACE
                               "¡¡¿ÇÎÅ¶èÊ¬="       DELIMITED  BY  SIZE
                               SRY-SRYKBN          DELIMITED  BY  SPACE
                                                   INTO    WRK-ERRMSG
                       END-STRING
                       IF      IDZ2        >   1    
                           STRING   WRK-ERRMSG     DELIMITED  BY  SPACE
                                   "¡¡¥³¥á¥ó¥È="   DELIMITED  BY  SIZE
                                   PTCOM-INPUTCOMENT(1:IDZ2)
                                                   DELIMITED  BY  SPACE
                                                   INTO    WRK-RECEERR
                           END-STRING
                       ELSE
                           STRING   WRK-ERRMSG     DELIMITED  BY  SPACE
                                    "¡¡¥³¥á¥ó¥È="  DELIMITED  BY  SIZE
                                    PTCOM-SRYCD    DELIMITED  BY  SPACE
                                                   INTO    WRK-RECEERR
                           END-STRING
                       END-IF
                       PERFORM 500-ERR-HENSYU-SEC
      *
                       MOVE    SPACE               TO  WRK-ERRMSG
                       STRING  "ORCR0420 "         DELIMITED  BY  SIZE
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
       800-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÆÉ¹þ
      *****************************************************************
       800-SYUNOU-READ-SEC           SECTION.
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
       800-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       900-RECE41-READ-SEC            SECTION.
      *
           READ    RECE41-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                   MOVE  RECE41-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¿ÇÎÅÇ¯·î¤ÎÂÐ¾Ý¤È¤Ê¤ë¥·¥¹´ÉÍý¤ò¼èÆÀ¡Ê¸ÄÊÌ½èÍý¤Î¤ß
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
             CONTINUE
           ELSE
      *      °åÎÅµ¡´Ø£É£ÄÊÔ½¸
             MOVE  ZERO           TO  FLG-SET
             PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                   UNTIL  ( SYSKAN-IDX              >  100  )
                    OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                    OR    ( FLG-SET                 =   1   )
      *
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                   MOVE     1       TO    FLG-SET
                   MOVE    SYS-1001-TAX-HASU-KBN
                                    TO    WRK-TAX1001-KBN
                   IF   WRK-TAX1001-KBN = SPACE
                      MOVE   "2"    TO    WRK-TAX1001-KBN
                   END-IF
                   MOVE    SYS-1001-DAISANKBN
                                    TO    WRK-DAISAN-DAISANKBN
                   IF  WRK-DAISAN-DAISANKBN = SPACE
                     MOVE  "2"      TO    WRK-DAISAN-DAISANKBN
                   END-IF
                   MOVE    SYS-1001-DISCOUNT-HASU-KBN4
                                    TO    WRK-HASU1001-JIBAI
                   IF  WRK-HASU1001-JIBAI = SPACE
                     MOVE  "4"      TO    WRK-HASU1001-JIBAI
                   END-IF
                   MOVE    SYS-1001-DISCOUNT-HASU-KBN6
                                    TO    WRK-HASU1001-DAISAN
                   IF  WRK-HASU1001-DAISAN = SPACE
                     MOVE  WRK-HASU1001-JIBAI
                                    TO    WRK-HASU1001-DAISAN
                   END-IF
                END-IF
      *
             END-PERFORM
             IF     FLG-SET  =  ZERO
      *
               STRING "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                          DELIMITED  BY  SIZE 
                       RECE41-SRYYM       DELIMITED  BY  SIZE
                                      INTO    WRK-RECEERR
               END-STRING
      *
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-END
               GO  TO  910-SYSKANRI-HENSYU-EXT
             END-IF
      *
      *      »ÜÀß´ð½à¾ðÊó
             MOVE    ZERO                TO  FLG-SET
      *
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
             MOVE    ZERO                TO  FLG-SET
      *
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
      *      Ä¢É¼ÊÔ½¸¶èÊ¬¾ðÊó
             MOVE    ZERO                TO  FLG-SET
             MOVE    SPACE               TO  WRK-1030-KOUHATUKA
      *
      **>    IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
               PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                       UNTIL ( SYSKAN-IDX              >  100  )
                       OR    ( TBL-SYS1030(SYSKAN-IDX) = SPACE )
                       OR    ( FLG-SET                 =   1   )
                 MOVE    TBL-SYS1030(SYSKAN-IDX) TO  SYS-1030-REC
                 IF    ( SYS-1030-STYUKYMD       <= WRK-SYS-SRYYMD    )
                 AND   ( WRK-SYS-SRYYMD          <= SYS-1030-EDYUKYMD )
                     MOVE     1            TO  FLG-SET
                     MOVE    SYS-1030-KOUHATUKA
                                           TO  WRK-1030-KOUHATUKA
                 END-IF
               END-PERFORM
      **>    END-IF
      *
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿ÉÕ²Ã¾ðÊóÀßÄê½èÍý
      *****************************************************************
       920-SRYACCTPLUS-HENSYU-SEC     SECTION.
      *
           IF  RECE44-SRYKBN  =  "11" OR "12" OR "13" OR "14"
             CONTINUE
           ELSE
             GO  TO  920-SRYACCTPLUS-HENSYU-EXT
           END-IF
           IF  RECE44-ZAIREQFLG  =  "1"
             CONTINUE
           ELSE
             GO  TO  920-SRYACCTPLUS-HENSYU-EXT
           END-IF
      *
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    RECE44-HOSPNUM     TO   ACCTP-HOSPNUM
           MOVE    RECE44-NYUGAIKBN   TO   ACCTP-NYUGAIKBN
           MOVE    RECE44-PTID        TO   ACCTP-PTID
           MOVE    RECE44-SRYKA       TO   ACCTP-SRYKA
           MOVE    RECE44-SRYYM       TO   ACCTP-SRYYM
           MOVE    RECE44-ZAINUM      TO   ACCTP-ZAINUM
           MOVE    1                  TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC    TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
             MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             PERFORM  800-SRYACCTPLUS-READ-SEC
             IF        FLG-SRYACCTPLUS   =   ZERO
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE44-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE44-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE44-ACCTP-CHISOTEN
      *
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE4A-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE4A-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE4A-ACCTP-CHISOTEN
      *
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE44C-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE44C-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE44C-ACCTP-CHISOTEN
             END-IF
           END-IF
      *
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       920-SRYACCTPLUS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ½èÍý
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡£Ã£Ï£Í£Í£É£Ô½èÍý
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¡¡£Ó£Ô£Á£Ò£Ô½èÍý
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
