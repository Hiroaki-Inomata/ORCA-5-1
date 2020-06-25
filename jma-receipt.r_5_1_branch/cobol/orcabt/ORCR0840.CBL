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
       PROGRAM-ID.             ORCR0840.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥ÈÆþ±¡¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/06/01    NaCl¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  05.00.01    NACL-ÌçÏÆ    19/03/11  ¸øÌ³ºÒ³²¡ÊÏ«ºÒ½àµò¡Ë¤ÎÃÏ¸øºÒÆÃÊÌ
      *                                     ²Ã»»ÂÐ±þ¡ÊµþÅÔÉÜ¡Ë½¤Àµ
      *  05.00.02    NACL-ÌçÏÆ    19/03/11  Í½Ìó¥³¡¼¥ÉÆÉ¤ßÈô¤Ð¤·½¤Àµ
      *  05.00.03    NACL-ÌçÏÆ    19/04/03  ¥³¥á¥ó¥È¥Þ¥¹¥¿¤Î²þ¸µÂÐ±þ¤Ë
      *                                     È¼¤¦½¤Àµ
      *  05.00.04    NACL-ÌçÏÆ    19/05/16  Âà±¡»þ¶¦Æ±»ØÆ³ÎÁ£²¥³¥á¥ó¥È¼«Æ°
      *                                     µ­ºÜÂÐ±þ
      *  05.00.05    NACL-ÌçÏÆ    19/07/01  ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆü¥ì¥»¥×¥Èµ­ºÜÂÐ±þ
      *                                     ¡Êµ¡Ç½ÄÉ²Ã¡Ë
      *  05.00.06    NACL-ÌçÏÆ    19/08/28  ÎáÏÂ¸µÇ¯£±£°·î²þÀµ¡Ê¾ÃÈñÀÇ¡Ë
      *  05.00.07    NACL-ÌçÏÆ    19/12/26  ´ïºàÅÀ¿ô£°ÅÀ»þ¤Î¥³¥á¥ó¥È¥³¡¼¥É
      *                                     ¤Î¼è¤ê°·¤¤½¤Àµ
      *  05.00.08    NACL-ÌçÏÆ    20/02/10  ÌôºÞ¡¦´ïºà¤ÎÃ±°ÌÊÔ½¸¡ÊÅÀ¿ôÉÕ²Ã¡Ë
      *  05.00.09    NACL-ÌçÏÆ    20/03/16  Â¾°åÎÅµ¡´Ø¼õ¿Ç¡ÜÉ¸·ç¤Î·×»»½¤Àµ
      *  05.00.10    NACL-ÌçÏÆ    20/03/30  ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.11    NACL-ÌçÏÆ    20/04/16  ´ïºà¤ÎÃ±°ÌÊÔ½¸½¤Àµ
      *  05.00.12    NACL-ÌçÏÆ    20/04/28  ¡ÖÂ¾ÊÝ¸±¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ¡×¤ÎÌ¾¾Î
      *                                     ÊÔ½¸½¤Àµ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
           SELECT  RECE84-FILE     ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE8A-FILE     ASSIGN  RECE8APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8A-KEY
                                   FILE    STATUS  IS  STS-RECE8A.
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE8B-FILE     ASSIGN  RECE8BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8B-KEY
                                   FILE    STATUS  IS  STS-RECE8B.
      *    ¥ì¥»ÅÅÍÑ
           SELECT  RECE84C-FILE    ASSIGN  RECE84CPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84C-KEY
                                   FILE    STATUS  IS  STS-RECE84C.
           SELECT  RECE85C-FILE    ASSIGN  RECE85CPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85C-KEY
                                   FILE    STATUS  IS  STS-RECE85C.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE84-FILE.
       01  RECE84-REC. 
           COPY    "CPRCF084.INC".
      *
       FD  RECE8A-FILE.
       01  RECE8A-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE8A//.
      *
       FD  RECE85-FILE.
       01  RECE85-REC. 
           COPY    "CPRCF085.INC".
      *
       FD  RECE8B-FILE.
       01  RECE8B-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE8B//.
      *
      *    ¥ì¥»ÅÅÍÑ
       FD  RECE84C-FILE.
       01  RECE84C-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE84C//.
      *
       FD  RECE85C-FILE.
       01  RECE85C-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE85C//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE82//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE84//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE8A//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE85//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE8B//.
      *    ¥ì¥»ÅÅÍÑ
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE84C//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE85C//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE82                              PIC X(02).
           03  STS-RECE84                              PIC X(02).
           03  STS-RECE85                              PIC X(02).
           03  STS-RECE8A                              PIC X(02).
           03  STS-RECE8B                              PIC X(02).
           03  STS-RECE84C                             PIC X(02).
           03  STS-RECE85C                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SP                                  PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-ARI2                                PIC 9(01).
           03  FLG-ARI3                                PIC 9(01).
           03  FLG-ARI5                                PIC 9(01).
           03  FLG-DAY21                               PIC 9(01).
           03  FLG-DAY24                               PIC 9(01).
           03  FLG-TENTEKI                             PIC 9(01).
           03  FLG-WRITE85                             PIC 9(01).
           03  FLG-WRITE85X                            PIC 9(01).
           03  FLG-SIKIBETU                            PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-FORMAL-KISAI                        PIC 9(01).
           03  FLG-RECE85-ZAN                          PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-OKIKAE                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HEYA                                PIC 9(01).
           03  FLG-KHN                                 PIC 9(01).
           03  FLG-NYUINRYO                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-OSIN-COM                            PIC 9(01).
           03  FLG-SYOBYO-COM                          PIC 9(01).
           03  FLG-RYOYO-COM                           PIC 9(01).
           03  FLG-SYODOU-COM                          PIC 9(01).
           03  FLG-KENSA-COM                           PIC 9(01).
           03  FLG-ZAIKYO-COM                          PIC 9(01).
           03  FLG-BOKOHO-COM                          PIC 9(01).
           03  FLG-HAIJRT-COM                          PIC 9(01).
           03  FLG-TAIKYO-COM                          PIC 9(01).
           03  FLG-ZAIHANKO-COM                        PIC 9(01).
           03  FLG-KIATU-COM                           PIC 9(01).
           03  FLG-SCPRO-COM                           PIC 9(01).
           03  FLG-HOUSEI-COM                          PIC 9(01).
           03  FLG-HOUMON-COM                          PIC 9(01).
           03  FLG-KENKSN-COM                          PIC 9(01).
           03  FLG-KINGAZ-COM                          PIC 9(01).
           03  FLG-HAIJSK-COM                          PIC 9(01).
           03  FLG-CYOJKSN-COM                         PIC 9(01).
           03  FLG-CYOJKSN-OK                          PIC 9(01).
           03  FLG-TAIGAI                              PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-KHNRYO-SKJ                          PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
           03  FLG-TENTEKIMATOME                       PIC 9(01).
           03  FLG-SRYACCTPLUS                         PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-TAHOKEN                             PIC 9(01).
           03  FLG-SRYKBN-HEN                          PIC 9(01).
           03  FLG-SYUNOU-ARI                          PIC 9(01).
           03  FLG-JYOGAI                              PIC 9(01).
           03  FLG-DOJITU                              PIC 9(01).
           03  FLG-HENSYU-NG                           PIC 9(01).
           03  FLG-SETCOM                              PIC 9(01).
           03  FLG-HENSYU-OK                           PIC 9(01).
           03  FLG-DAISAN-GENBUTU                      PIC 9(01).
           03  FLG-KIN-OKIKAE                          PIC 9(01).
           03  FLG-KIN-OKIKAE2                         PIC 9(01).
           03  FLG-RIHA-OKIKAE                         PIC 9(02).
           03  FLG-IRYORENKEI                          PIC 9(01).
           03  FLG-YKZ-PRTKBN                          PIC 9(01).
           03  FLG-KIZ-PRTKBN                          PIC 9(01).
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
           03  CNT-DAY21                               PIC 9(06).
           03  CNT-DAY24                               PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDX6                                    PIC 9(03).
           03  IDX7                                    PIC 9(03).
           03  IDX8                                    PIC 9(03).
           03  IDX9                                    PIC 9(03).
           03  IDX10                                   PIC 9(03).
           03  IDX11                                   PIC 9(02).
           03  IDX12                                   PIC 9(02).
           03  IDX13                                   PIC 9(02).
           03  IDX14                                   PIC 9(02).
           03  IDX15                                   PIC 9(03).
           03  IDX17                                   PIC 9(03).
           03  IDX25                                   PIC 9(02).
           03  IDX26                                   PIC 9(02).
           03  IDX30                                   PIC 9(03).
           03  IDX31                                   PIC 9(03).
           03  IDX35                                   PIC 9(03).
           03  IDX40                                   PIC 9(02).
           03  IDX41                                   PIC 9(02).
           03  IDXA                                    PIC 9(01).
           03  IDXB                                    PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDY3                                    PIC 9(03).
           03  IDYY                                    PIC 9(02).
           03  IDXX                                    PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDZ1                                    PIC 9(03).
           03  IDZ2                                    PIC 9(03).
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
           03  ID2                                     PIC 9(03).
           03  ID3                                     PIC 9(03).
           03  ID4                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  IDV                                     PIC 9(02).
           03  IDV2                                    PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  SYUKEI-IDX                              PIC 9(03).
           03  SYUKEI-IDX2                             PIC 9(03).
           03  SYUKEI-IDX3                             PIC 9(03).
           03  SYUKEI-IDX4                             PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-IDX2                             PIC 9(03).
           03  FUKA-IDX                                PIC 9(03).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°¡Ê¼«ÇåÀÕÍÑ¡Ë
       01  FLG-KHNTEN-JI-AREA.                                          
           03  FLG-KHNTEN7-JI                          PIC 9(01).       
           03  FLG-KHNTEN8-JI                          PIC 9(01).       
           03  FLG-KHNTEN9-JI                          PIC 9(01).       
      *
      *    ºÞ¾ðÊóÂàÈòÎÎ°è
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
       01  WK-YKZTEN2                                  PIC S9(08).
      *
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸ÊÔ½¸ÎÎ°è
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM                               PIC X(20).
           03  ERR-ERRMSG                              PIC X(80).
           03  ERR-HEN-ERRMSG                          PIC X(200).
      *
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¸¡º÷ÍÑÂÐ¾ÝÆüÎÎ°è
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC X(02).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY4X                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
       01  WRK-DAY-TABLE2.
           03  WRK-DAY-T2                              OCCURS   31.
               05  WRK-DAY21                           PIC 9(04).
               05  WRK-DAY24                           PIC 9(04).
      *
       01  WRK-SKJ-TABLE.
           03  WRK-SKJ-T                               OCCURS   31.
               05  WRK-SKJ-ZAITEN                      PIC 9(08).
               05  WRK-SKJ-ZAIKAISU                    PIC 9(04).
               05  WRK-SKJ-SRYCD-T.
                06  WRK-SKJ-SRYCD-OCC                  OCCURS   15.
                 07  WRK-SKJ-SRYCD                     PIC 9(09).
      *
       01  WRK-SKJ-TABLE2.
           03  WRK-SKJ-T2                              OCCURS   31.
               05  WRK-SKJ2-ZAITEN                     PIC 9(08).
               05  WRK-SKJ2-ZAIKAISU                   PIC 9(04).
               05  WRK-SKJ2-MEISAISU                   PIC 9(04).
               05  WRK-SKJ2-SRYCDTOTAL                 PIC 9(14).
               05  WRK-SKJ2-SRYCD-T.
                06  WRK-SKJ2-SRYCD-OCC                 OCCURS   15.
                 07  WRK-SKJ2-SRYCD                    PIC 9(09).
               05  WRK-SKJ2-KAISU-T.
                06  WRK-SKJ2-KAISU-OCC                 OCCURS   31.
                 07  WRK-SKJ2-KAISU                    PIC 9(01).
      *
       01  WRK-SKJ200604-TABLE.
           03  WRK-SKJ200604-T                         OCCURS   31.
               05  WRK-SKJ200604-KBN                   PIC 9(01).
               05  WRK-SKJ200604-SRYCDTOTAL            PIC 9(14).
               05  WRK-SKJ200604-MEISAISU              PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL           PIC 9(02).
      **>      05  WRK-SKJ200604-T2                    OCCURS    3.
               05  WRK-SKJ200604-T2                    OCCURS    4.
                   07  WRK-SKJ200604-DAY               PIC 9(04).
      **>      05  WRK-SKJ200604-T3                    OCCURS    9.
               05  WRK-SKJ200604-T3                    OCCURS   12.
                   07  WRK-SKJ200604-SRYCD             PIC 9(09).
                   07  WRK-SKJ200604-SURYOU            PIC 9(02).
      *
               05  WRK-SKJ200604-SRYCDTOTAL-KAN        PIC 9(14).
               05  WRK-SKJ200604-MEISAISU-KAN          PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL-KAN       PIC 9(02).
               05  WRK-SKJ200604-DAY-KAN               PIC 9(01).
               05  WRK-SKJ200604-T4                    OCCURS    3.
                   07  WRK-SKJ200604-SRYCD-KAN         PIC 9(09).
                   07  WRK-SKJ200604-SURYOU-KAN        PIC 9(02).
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
       01  WRK-SKJKEEP-TABLE.
           03  WRK-SKJKEEP-AREA                        OCCURS   31.
               05  WRK-SKJKEEP-KBN                     PIC X(01).
      *
       01  WRK-SKJHKN-TABLE.
           03  WRK-SKJHKN-AREA                         OCCURS   31.
               05  WRK-SKJHKN-HKNCOMBI                 PIC X(04).
               05  WRK-SKJHKN-HKNCOMBI-DOU             PIC X(04).
      *
       01  WRK-TENTEKI2-TABLE.
      *
           03  WRK-TENTEKI2-HENKYAKUARI                PIC 9(01).
      *
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
       01  WRK-CHUKSNSRYCD-TABLE.
           03  WRK-CHUKSNSRYCD                         PIC X(09)
                                                       OCCURS   100.
      *
      *    °åÎÅµ¡´Ø¡ÊÆþ±¡´ðËÜ¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *    °åÎÅµ¡´Ø¡Ê´ðËÜ¾ðÊó¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
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
               05  TBL-SYUTEN-JI                       PIC 9(09).
               05  TBL-YKZTEN-JI                       PIC 9(09).
               05  TBL-YKZTEN-JI-C                     PIC 9(09).
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
           03  WRK-SRYYM-5000                          PIC X(06).
           03  WRK-SYS5000-COUNT                       PIC 9(02).
      *
           03  WRK-TEN                                 PIC 9(09).
           03  WRK-TEN-KG                              PIC 9(09).
           03  WRK-TEN-KG2                             PIC 9(09).
           03  WRK-MEISAI-CNT                          PIC 9(04).
           03  WRK-SRYCDTOTAL                          PIC 9(14).
           03  WRK-SURYOUTOTAL                         PIC 9(07)V9(05).
           03  WRK-KSNRATE                             PIC 9V99.    
           03  WRK-YOUBUKASAN                          PIC 9(07).
           03  WRK-KYOBUKASAN                          PIC 9(07).
           03  WRK-KEIBUKASAN                          PIC 9(07).
           03  WRK-SOGAIKASAN                          PIC 9(07).
           03  WRK-RYOYOTEATE                          PIC 9(07).
           03  WRK-RYOYOTEATE2                         PIC 9(07).
           03  WRK-ZAI-KINGAKU                         PIC 9(09).
      *
           03  WRK-HEYA-NINZU                          PIC 9(02).
      *
           03  WRK-SRYCD-X.
               05  WRK-SRYCD                           PIC 9(09).
           03  WRK-ZAINUM                              PIC 9(08).
           03  WRK-ZAINUM-CNT2                         PIC 9(03).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-RENNUM2                             PIC 9(02).
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
      *
           03  WRK-SRYSURYO                            PIC 9(05)V9(05).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-KZI058-TEN                          PIC 9(09)V99.
           03  WRK-KZI058-TANICD                       PIC 9(03).
           03  WRK-KZI058-TANIMOJISU                   PIC 9(01).
           03  WRK-KZI058-TANINAME                     PIC X(24).
      *
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO                    PIC 9(01)
                                                       OCCURS 31.
           03  WRK-TOKUKANRIKASAN-OCC.
               05  WRK-TOKUKANRIKASAN                  PIC 9(01)
                                                       OCCURS 31.
      *
           03  WRK-KEISAN                              PIC 9(09).
           03  WRK-KEISAN2                             PIC 9(09).
           03  WRK-KEISAN-KG                           PIC 9(09).
           03  WRK-KEISAN-KG2                          PIC 9(09).
      *
           03  WRK-SROUNYUIN-SRYCDTOTAL                PIC 9(14).
           03  WRK-SROUNYUIN-MEISAISU                  PIC 9(07).
           03  WRK-SROUNYUIN-SRYCD                     PIC 9(09).
           03  WRK-SROUNYUIN-SRYCD2                    PIC 9(09).
      *
           03  WRK-TEN-KZI                             PIC 9(09).
           03  WRK-TEN-BUN                             PIC 9(09).
      *
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅù¤ÎÅÀ¿ôÂàÈò
           03  WRK-JIBAIKZI-TNS-TAIHI-TABLE.
               05  WRK-JIBAIKZI-TNS-TAIHI-OCC          OCCURS  5.
                 06  WRK-JIBAIKZI-TNS                  PIC 9(09).
                 06  WRK-JIBAIKZI-SYUKEIKBN            PIC 9(01).
      *
           03  WRK-SYUTEN-JI                           PIC S9(09).
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
           03  WRK-ZAITENNASHI                 PIC 9(09).
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
           03  WRK-STR                         PIC X(200).
           03  WRK-LEN                         PIC 9(05).
           03  WRK-SHO                         PIC 9(05).
           03  WRK-AMARI                       PIC 9(05).
      *
           03  WRK-GENTEN-YKZTEN                   PIC 9(08).
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
           03  WRK-FUTANWARI                   PIC 9V99.
      *
           03  WRK-CHECK-SRYCD                 PIC X(09).
      *
           03  WRK-DAISAN-FTNMONEY             PIC 9(08).
           03  WRK-DAISAN-DAISANKBN            PIC X(01).
      *
           03  WRK-COL                         PIC 9(03).
      *
           03  WRK-S1                          PIC 9(03).
           03  WRK-S2                          PIC 9(03).
           03  WRK-S3                          PIC 9V99999.
           03  WRK-S4                          PIC 9V99.
           03  WRK-S5                          PIC 9V99.
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
           03  WRK-NYUSAI-SRYKBN               PIC X(02).
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
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAMEÆâ¤ÎBASENAME³«»Ï°ÌÃÖ
       01  WRK-SGETTEMP-ST         PIC 9(05).
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
       01  TBL-SUJI-AREA-X.
           03  FILLER          PIC X(10)   VALUE
              "1234567890".
       01  TBL-SUJI-X-R        REDEFINES   TBL-SUJI-AREA-X.
           03  TBL-SUJI-X      PIC X(01)   OCCURS   10.
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
      *    £Ò£Å£Ã£Å£¸£²¤Î¥ï¡¼¥¯¥ì¥³¡¼¥É
       01  WRK-RECE8G-REC. 
           COPY    "CPRCF082.INC"  REPLACING  //RECE82//
                                   BY         //WRK-RECE8G//.
      *
       01  WRK-RECE8C-REC. 
           COPY    "CPRCF082.INC"  REPLACING  //RECE82//
                                   BY         //WRK-RECE8C//.
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
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSYSKANRI.INC".
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý´ðËÜ
           COPY    "CPSK1001.INC".
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK4003.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK1030.INC".
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    Æþ±¡ÅÀÅ©°ì³ç¹¹¿·¥µ¥Ö
           COPY    "CPORCSTENTEK.INC".
      *    Ï«ºÒÆþ±¡²ñ·×ºîÀ®¥µ¥Ö
           COPY    "CPORCSROUNYUIN.INC".
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYULIFE.INC".
      *    Æþ±¡ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤á¥µ¥Ö
           COPY    "CPORCSTENTEK2.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ±¡³°½èÊý°ìÈÌÌ¾ÊÔ½¸¥µ¥Ö
           COPY    "CPORCSGETGENERIC.INC".
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö£²
           COPY    "CPORCSNYULIFE2.INC".
      *    ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ¥µ¥Ö
           COPY    "CPORCSSKJKEEP.INC".
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ë¿©»ö»»ÄêÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¼èÆÀ¥µ¥Ö
           COPY    "CPORCSSKJHKN.INC".
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *    ´µ¼Ô½êÆÀ¾ðÊóÊÔ½¸¥µ¥Ö
           COPY    "CPORCSSYOTOKU.INC".
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
      *
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       TTNS-AREA
                                       FLG-AREA
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
           MOVE    SPACE               TO  RECE82PARA
           INITIALIZE                      RECE82PARA
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
           MOVE    SPACE               TO  RECE84PARA
           INITIALIZE                      RECE84PARA
           MOVE    "RECE840"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE84PARA-HOSPNUM
           MOVE    SPACE               TO  RECE85PARA
           INITIALIZE                      RECE85PARA
           MOVE    "RECE850"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE85PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE8APARA
           INITIALIZE                      RECE8APARA
           MOVE    "RECE841"           TO  RECE8APARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8APARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE8APARA-HOSPNUM
           MOVE    SPACE               TO  RECE8BPARA
           INITIALIZE                      RECE8BPARA
           MOVE    "RECE851"           TO  RECE8BPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8BPARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE8BPARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE84CPARA
           INITIALIZE                      RECE84CPARA
           MOVE    "RECE84C"           TO  RECE84CPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84CPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE84CPARA-HOSPNUM
           MOVE    SPACE               TO  RECE85CPARA
           INITIALIZE                      RECE85CPARA
           MOVE    "RECE85C"           TO  RECE85CPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85CPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE85CPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE82PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE84PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE85PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE8APARA-BASENAME TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE8BPARA-BASENAME TO  SGETTEMP-BASENAMES  (6)
           MOVE    RECE84CPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           MOVE    RECE85CPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (8)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE82PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE84PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE85PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE8APARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE8BPARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE84CPARA
           MOVE    SGETTEMP-FULLNAMES (8)
                                       TO  RECE85CPARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0840"          TO  JOB-PGID
               MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£±£°£°£±¡Ë¤«¤é¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
      *       ¤Î¼èÆÀ
              MOVE    SPACE               TO  WRK-TAX1001-KBN
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE  MCPDATA-REC         TO  SYS-1001-REC
                MOVE  SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
                IF   WRK-TAX1001-KBN = SPACE
                   MOVE   "2"             TO  WRK-TAX1001-KBN
                END-IF
                MOVE  SYS-1001-DAISANKBN  TO  WRK-DAISAN-DAISANKBN
                IF  WRK-DAISAN-DAISANKBN = SPACE
                  MOVE  "2"               TO  WRK-DAISAN-DAISANKBN
                END-IF
                MOVE  SYS-1001-DISCOUNT-HASU-KBN4
                                          TO  WRK-HASU1001-JIBAI
                IF  WRK-HASU1001-JIBAI = SPACE
                  MOVE  "4"               TO  WRK-HASU1001-JIBAI
                END-IF
                MOVE  SYS-1001-DISCOUNT-HASU-KBN6
                                          TO  WRK-HASU1001-DAISAN
                IF  WRK-HASU1001-DAISAN = SPACE
                  MOVE  WRK-HASU1001-JIBAI
                                          TO  WRK-HASU1001-DAISAN
                END-IF
              ELSE
                MOVE  "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                PERFORM 500-ERR-HENSYU-SEC
                MOVE    1                 TO  FLG-END
              END-IF
      *
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
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1006"              TO  SYS-KANRICD
                MOVE    IDXA                TO  SYS-KBNCD
                MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
                MOVE    "01"                TO  SYS-STYUKYMD (7:2)
                MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys          MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                PERFORM 910-SYSKANRI-INV-SEC
                IF      FLG-SYSKANRI     =   ZERO
                   MOVE   MCPDATA-REC       TO  SYS-1006-REC
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
              END-PERFORM
      *       Ä¢É¼ÊÔ½¸¶èÊ¬¾ðÊó
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
                PERFORM 910-SYSKANRI-INV-SEC
                IF      FLG-SYSKANRI     =   ZERO
                  MOVE  MCPDATA-REC         TO  SYS-1030-REC
                  MOVE  SYS-1030-KOUHATUKA  TO  WRK-1030-KOUHATUKA
                END-IF
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
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
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
grpsys           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                     MOVE    "tbl_syskanri"   TO  MCP-TABLE
                     MOVE    "key8"           TO  MCP-PATHNAME
                     PERFORM  900-SYSKANRI-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE   MCPDATA-REC     TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     2
                           ADD     1      TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                          TO  TBL-SYS10062(SYSKAN-IDX2)
                         WHEN     7
                           ADD     1      TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                          TO  TBL-SYS10067(SYSKAN-IDX2)
                       END-EVALUATE
                       MOVE    "tbl_syskanri" TO  MCP-TABLE
                       MOVE    "key8"         TO  MCP-PATHNAME
                       PERFORM  900-SYSKANRI-READ-SEC
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"       TO  MCP-TABLE
                 MOVE    "key8"               TO  MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
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
                  PERFORM 900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC     TO  SYS-1030-REC
                    ADD     1               TO  SYSKAN-IDX
                    MOVE    SYS-1030-REC    TO  TBL-SYS1030(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
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
             PERFORM   120-SYS-5000-CHK-SEC
      *
      *      ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-STYUKYMD (7:2)
             MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             PERFORM 910-SYSKANRI-INV-SEC
             IF      FLG-SYSKANRI        =   ZERO
               MOVE     MCPDATA-REC      TO  SYS-5000-REC
               PERFORM  110-SYSKAN-HENSYU-SEC
               IF        WRK-SYS5000-COUNT    =     ZERO
                  CONTINUE
               ELSE
                  PERFORM  130-SYS-5000-GET-SEC
               END-IF
             ELSE
               MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1              TO  FLG-END
             END-IF
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
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key8"          TO  MCP-PATHNAME
                 PERFORM 900-SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
      *
                   MOVE    MCPDATA-REC  TO  SYS-5000-REC
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"   TO  MCP-TABLE
                   MOVE    "key8"           TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
                 END-PERFORM
             ELSE
                 MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                        TO  WRK-RECEERR
                 PERFORM 500-ERR-HENSYU-SEC
                 MOVE    1              TO  FLG-END
             END-IF
             MOVE    "tbl_syskanri"     TO  MCP-TABLE
             MOVE    "key8"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
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
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
                MOVE  MCPDATA-REC   TO  SYS-200501-REC
           ELSE
                INITIALIZE              SYS-200501-REC
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO-KSIKBN
                                    =   SPACE
               MOVE    "1"   TO  SYS-200501-COMMENT-NYUINRYO-KSIKBN
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                                    =   SPACE
               MOVE    "2"   TO  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
           END-IF
           IF      SYS-200501-ROURECEKBN  =   SPACE
               MOVE    "0"   TO  SYS-200501-ROURECEKBN
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
           IF      SYS-200501-15-YKZ-PRTKBN         =   SPACE
               MOVE    "0"   TO  SYS-200501-15-YKZ-PRTKBN
           END-IF
      *
           OPEN    OUTPUT              RECE84-FILE
           CLOSE                       RECE84-FILE
      *
           OPEN    OUTPUT              RECE85-FILE
           CLOSE                       RECE85-FILE
      *
           OPEN    OUTPUT              RECE8A-FILE
           CLOSE                       RECE8A-FILE
      *
           OPEN    OUTPUT              RECE8B-FILE
           CLOSE                       RECE8B-FILE
      *
           OPEN    OUTPUT              RECE84C-FILE
           CLOSE                       RECE84C-FILE
      *
           OPEN    OUTPUT              RECE85C-FILE
           CLOSE                       RECE85C-FILE
      *
           OPEN    OUTPUT              RECE84-FILE
           OPEN    I-O                 RECE85-FILE
      *
           OPEN    OUTPUT              RECE8A-FILE
                                       RECE8B-FILE
      *
           OPEN    OUTPUT              RECE84C-FILE
           OPEN    I-O                 RECE85C-FILE
      *
           OPEN    I-O                 RECE82-FILE
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           PERFORM    900-RECE82-READ-SEC
      *
           IF  FLG-END =  ZERO
              INITIALIZE  WRK-CHUKSNSRYCD-TABLE
              IF  RECE82-SRYYM  >= "201004"
                IF  (  RECE82-SRYYM   >= "201204" )
                AND (((RECE82-HKNKBN   = "4") AND
                      (RECE82-JIBAIKBN = "2")) OR
                     ((RECE82-HKNKBN   = "5") AND
                      (RECE82-KOMUKBN  = "2")) OR
                     ( RECE82-HKNKBN   = "6" )    )
                  CONTINUE
                ELSE
                  PERFORM  140-NYUINKASAN-JYOGAI-SEC
                END-IF
              END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥«¥ó¤ÎÊÔ½¸
      *****************************************************************
       110-SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM   VARYING   IDX11  FROM  1  BY  1
                      UNTIL    IDX11   >   31
              EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                WHEN         "197000110"
                    MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX11)
                    EVALUATE  SYS-5000-SHOKUKBN(1)
                      WHEN     "1"
                        MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX11)
                      WHEN     "2"
                        MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX11)
                    END-EVALUATE
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                WHEN         "197000710"
                    MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX11)
              END-EVALUATE
           END-PERFORM
      *
           .
       110-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       120-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-SYS5000-COUNT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
                   ADD     1     TO        WRK-SYS5000-COUNT
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key11"         TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       120-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       130-SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   MCPDATA-REC  TO  SYS-5000-REC
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM    VARYING  IDX11  FROM  IDV  BY  1
                             UNTIL   IDX11    >   31
                      EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                       WHEN         "197000110"
                          MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX11)
                          EVALUATE  SYS-5000-SHOKUKBN(1)
                            WHEN     "1"
                              MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX11)
                            WHEN     "2"
                              MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX11)
                          END-EVALUATE
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                       WHEN         "197000710"
                          MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX11)
                          MOVE    ZERO    TO   WRK-TOKUKANRIKASAN(IDX11)
                      END-EVALUATE
                 END-PERFORM
      *
                 MOVE    "tbl_syskanri"   TO   MCP-TABLE
                 MOVE    "key11"          TO   MCP-PATHNAME
                 PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"         TO   MCP-TABLE
           MOVE    "key11"                TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       130-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»½ü³°¥³¡¼¥ÉÃê½Ð
      *****************************************************************
       140-NYUINKASAN-JYOGAI-SEC      SECTION.
      *
           MOVE   ZERO               TO  IDX14
      *
           INITIALIZE                    TNS-TENSU-REC
           MOVE   SPA-HOSPNUM        TO  TNS-HOSPNUM
           MOVE   RECE82-SRYYM       TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"               TO  TNS-YUKOSTYMD(7:2)
           MOVE   RECE82-SRYYM       TO  TNS-YUKOEDYMD(1:6)
           MOVE   "01"               TO  TNS-YUKOEDYMD(7:2)
           MOVE   TNS-TENSU-REC      TO  MCPDATA-REC
           MOVE   "tbl_tensu"        TO  MCP-TABLE
           MOVE   "key30"            TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC            =   ZERO
              MOVE    "tbl_tensu"    TO  MCP-TABLE
              MOVE    "key30"        TO  MCP-PATHNAME
              PERFORM  910-TENSU-READ-SEC
              PERFORM  UNTIL  (FLG-TENSU  =     1)
                        OR    (IDX14     >=   100)
                 IF   TNS-SRYCD  =  "190134270"
                                 OR "190135470"
                                 OR "190135670"
      *
                                 OR "190142970"
                                 OR "190143370"
      *
                                 OR "190142870"
                                 OR "190143270"
                                 OR "190143470"
                                 OR "190144770"
                                 OR "190144870"
                                 OR "190144970"
                                 OR "190145070"
                                 OR "190145170"
                                 OR "190145270"
      *
                                 OR "190135070"
      *
                                 OR "190161770"
                                 OR "190162170"
                                 OR "190161470"
                                 OR "190161570"
                                 OR "190161670"
                                 OR "190162070"
                                 OR "190161970"
                                 OR "190161870"
      *
                                 OR "190166070"
                                 OR "190169070"
                                 OR "190169170"
      *
                                 OR "190198570"
                                 OR "190198670"
      *
                                 OR "190201770"
                                 OR "190201870"
      *
                                 OR "190205570"
                                 OR "190205670"
                                 OR "190205770"
      *
                                 OR "190205970"
                                 OR "190206070"
                   CONTINUE
                 ELSE
                   ADD    1          TO  IDX14
                   MOVE   TNS-SRYCD  TO  WRK-CHUKSNSRYCD(IDX14)
                 END-IF
      *
                 MOVE    "tbl_tensu" TO  MCP-TABLE
                 MOVE    "key30"     TO  MCP-PATHNAME
                 PERFORM  910-TENSU-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_tensu"       TO  MCP-TABLE
           MOVE    "key30"           TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    Ï«ºÒÍÑ¥³¡¼¥ÉÊÔ½¸
           ADD     1          TO  IDX14
           MOVE  "101900100"  TO  WRK-CHUKSNSRYCD(IDX14)
           IF    RECE82-SRYYM  <=  "201803"
            ADD    1          TO  IDX14
            MOVE "101900110"  TO  WRK-CHUKSNSRYCD(IDX14)
           END-IF
           ADD     1          TO  IDX14
           MOVE  "101900120"  TO  WRK-CHUKSNSRYCD(IDX14)
      *
           ADD     1          TO  IDX14
           MOVE  "101900130"  TO  WRK-CHUKSNSRYCD(IDX14)
           ADD     1          TO  IDX14
           MOVE  "101900140"  TO  WRK-CHUKSNSRYCD(IDX14)
      *
           ADD     1          TO  IDX14
           MOVE  "101900150"  TO  WRK-CHUKSNSRYCD(IDX14)
           ADD     1          TO  IDX14
           MOVE  "101900160"  TO  WRK-CHUKSNSRYCD(IDX14)
      *
           ADD     1          TO  IDX14
           MOVE  "190182690"  TO  WRK-CHUKSNSRYCD(IDX14)
      *
           ADD     1          TO  IDX14
           MOVE  "190212670"  TO  WRK-CHUKSNSRYCD(IDX14)
      *
           ADD     1          TO  IDX14
           MOVE  "101900170"  TO  WRK-CHUKSNSRYCD(IDX14)
      *
           .
       140-NYUINKASAN-JYOGAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 210-HENSYU-SEC
      *
           IF   FLG-END = ZERO
             PERFORM 900-RECE82-READ-SEC
      *
             IF  CNT-IN-COMMIT   =   300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
             END-IF
      *
      *      ½èÍýÃæ»ßÀßÄê½èÍý
             IF      CNT-IN-CANCEL       =   50
                 MOVE    ZERO                TO  CNT-IN-CANCEL
                 PERFORM 500-CANCEL-HENSYU-SEC
             END-IF
           END-IF
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
           IF    RECE82-SRYYM >= "201910"
               MOVE      10      TO  WRK-S1
               MOVE     110      TO  WRK-S2
               MOVE     0.10     TO  WRK-S4
               MOVE     1.10     TO  WRK-S5
           ELSE
             IF  RECE82-SRYYM >= "201404"
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
           INITIALIZE          WRK-DAY-TABLE
      *
           INITIALIZE          FLG-KHNTEN-JI-AREA
      *
           INITIALIZE          TBL-TENSUG-JI
      *
           INITIALIZE          WRK-SKJ200604-TABLE
      *
           INITIALIZE          WRK-NYULIFE-TABLE
                               WRK-SKJKEEP-TABLE
                               WRK-SKJHKN-TABLE
      *
           INITIALIZE          WRK-TENTEKI2-TABLE
      *
           MOVE   ZERO   TO    FLG-HEYA
      *
           MOVE   ZERO   TO    WRK-KINGAKU-JI
      *
           MOVE   ZERO   TO    WRK-ZAITENNASHI
      *
           INITIALIZE          WRK-RECE8G-REC
           INITIALIZE          WRK-RECE8C-REC
      *
           MOVE   ZERO   TO    WRK-TAIHI-ZAINUM
           MOVE   ZERO   TO    WRK-TAIHI-ZAINUM2
      *
           PERFORM    290-TENTEKIMATOME-HENSYU-SEC
      *
           PERFORM    220-RECE84-HENSYU-SEC
           PERFORM    230-RECE84-HENSYU2-SEC
           PERFORM    240-RECE84-HENSYU3-SEC
           PERFORM    250-RECE84-HENSYU4-SEC
           PERFORM    260-FTN-SET-SEC
      *
           IF      RECE82-HKNKBN    =   "4" OR "6"
             PERFORM  270-KHNTEN-JI-CLEAR-SEC
           END-IF                            
      *
           PERFORM    2A-GENTEN-HENSYU-SEC
      *
      *    ¼«ÇåÀÕ¡¦ÀÁµá¶â³ÛÍóÃ¼¿ô½èÍý¶èÊ¬ÊÔ½¸
           PERFORM    2B-JIBAI-HASUKBN-HENSYU-SEC
      *
           PERFORM    280-RECE82-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       220-RECE84-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *
                PERFORM   2206-FLG-KURIA-SEC
      *
                IF    ACCT-SRYKBN       =  "95"  OR  "96"
                 CONTINUE
                ELSE
      *
                 MOVE     ZERO    TO    FLG-JYOGAI
                 IF   (ACCT-SRYKBN  =  "90" OR "92")
                 AND  (ACCT-JIHOKBN =  SPACE       )
                   PERFORM  VARYING  ID4    FROM   1   BY   1
                             UNTIL  (ID4                  >   100 )
                               OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                               OR   (FLG-JYOGAI           =   1   )
                     IF  ACCT-SRYCDTOTAL(6:9) = WRK-CHUKSNSRYCD(ID4)
                        MOVE     1    TO   FLG-JYOGAI
                     END-IF
                   END-PERFORM
                 END-IF
      *
      *          Ì¤¼Â»Ü¸º»»¡¦Æü¾ïÀ¸³è¾ã³²²Ã»»¡¦ÃÔÊò²Ã»»¡¦
      *          êóáì´µ¼Ô´ÉÍý²Ã»»¤ò½ü¤¯
                 IF ((ACCT-SRYCDTOTAL  =  190080470  OR  190799670
                                      OR  190080570  OR  190799770
                                      OR  190111690  OR  190809090
                                      OR  190111790  OR  190809190
                                      OR  190807570  OR  190808670
                                      OR  190116990  OR  190813390) AND
                     (ACCT-JIHOKBN     =  SPACE                   ) AND
                     (RECE82-SRYYM     <= "200603"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190109170  OR  190109270) AND
                     (ACCT-JIHOKBN     =  SPACE                   ) AND
                     (RECE82-SRYYM     <= "200606"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190117070  OR  190109670) AND
                     (ACCT-JIHOKBN     =  SPACE                   ) AND
                     (RECE82-SRYYM     >= "200604"                ) AND
                     (RECE82-SRYYM     <= "200803"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190121970  OR  190122670) AND
                     (ACCT-JIHOKBN     =  SPACE                   ) AND
                     (RECE82-SRYYM     >= "200607"                ) AND
                     (RECE82-SRYYM     <= "201203"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190119210               ) AND
                     (ACCT-JIHOKBN     =  SPACE                   ) AND
                     (RECE82-SRYYM     >= "200804"                ) AND
                     (RECE82-SRYYM     <= "201203"                ))
                 OR ( FLG-JYOGAI       =  1                        )
                   CONTINUE
                 ELSE
      *
                   MOVE     ZERO    TO    FLG-TAISYO
                   IF   (ACCT-SRYKBN         =   "80" )
                   AND  (ACCT-ZAITEN         =   ZERO )
                   AND  (ACCT-ZAIKAISU   NOT =   ZERO )
                   AND  (ACCT-JIHOKBN        =   SPACE)
                     PERFORM  2204-SRYACCTPLUS-KENSAKU-SEC
                   END-IF
      *
                   MOVE     ZERO    TO    FLG-TENTEKIMATOME
                   IF    ACCT-SRYKBN  =  "33"
                     PERFORM  2205-TENTEKIMATOME-CHECK-SEC
                   END-IF
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
                   OR  ( FLG-TAISYO         =      1   )
                       IF  (ACCT-SRYKBN     = "97"     )
                       AND (ACCT-SRYCDTOTAL = 197000470
                                           OR 197001570
                                           OR 197001210)
                         IF  (RECE82-SRYYM   >= "200604" )
                         AND (ACCT-SRYCDTOTAL = 197000470
                                             OR 197001570)
                           PERFORM   2201-RECE84-TENSOU-SEC
                           PERFORM   2202-RECE85-HENSYU-SEC
                           IF    FLG-WRITE85  =   1
                               PERFORM 900-RECE84-WRITE-SEC
                           END-IF
                         END-IF
                       ELSE
                         PERFORM   2201-RECE84-TENSOU-SEC
                         PERFORM   2202-RECE85-HENSYU-SEC
                         IF    FLG-WRITE85  =   1
                             PERFORM 900-RECE84-WRITE-SEC
                         END-IF
                       END-IF
                   ELSE
                       IF   (ACCT-ZAIKAISU     NOT =  ZERO )
                       AND  (FLG-TENTEKIMATOME     =  ZERO )
                       AND  (ACCT-JIHOKBN          =  SPACE)
      **>              IF  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                   (FLG-TENTEKIMATOME  =    ZERO ) AND
      **>                   (WRK-PARA-PRTKBN    =
      **>                               "1" OR "2" OR "3" ) AND
      **>                   (ACCT-JIHOKBN       =    SPACE))
      **>              OR  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                   (FLG-TENTEKIMATOME  =    ZERO ) AND
      **>                   (WRK-PARA-PRTKBN    =
      **>                                      "4" OR "5" ))
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
                                   PERFORM 2203-SINRYO-CHECK-SEC
                                 END-IF
                                 PERFORM   2201-RECE84-TENSOU-SEC
                                 PERFORM   2202-RECE85-HENSYU-SEC
                                 IF    FLG-WRITE85  =   1
                                    PERFORM 900-RECE84-WRITE-SEC
                                 END-IF
                               END-IF
                            END-IF
                       ELSE
                            IF   (ACCT-ZAIKAISU     NOT =  ZERO )
                            AND  (FLG-TENTEKIMATOME     =  ZERO )
                            AND  (ACCT-JIHOKBN      NOT =  SPACE)
                            AND  (ACCT-ZAIKBN       =  3 OR 4   )
                                 PERFORM   2201-RECE84-TENSOU-SEC
                                 PERFORM   2202-RECE85-HENSYU-SEC
                                 IF    FLG-WRITE85  =   1
                                    PERFORM 900-RECE84-WRITE-SEC
                                 END-IF
                            END-IF
                       END-IF
                   END-IF
                 END-IF
                END-IF
                MOVE    "tbl_sryacct"    TO      MCP-TABLE
                MOVE    "key22"          TO      MCP-PATHNAME
                PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"         TO      MCP-TABLE
           MOVE    "key22"               TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       220-RECE84-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2201-RECE84-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           MOVE    ACCT-ZAINUM          TO      RECE84-ZAINUM
           MOVE    ACCT-SRYKBN          TO      RECE84-SRYKBN
           IF      ACCT-SRYKBN = "90" OR "92" OR "12"
                 MOVE    "6"            TO      RECE84-ZAISKBKBN
           ELSE 
              IF   ACCT-SRYKBN = "97"
      ***        MOVE    "4"            TO      RECE84-ZAISKBKBN
                 MOVE    "9"            TO      RECE84-ZAISKBKBN
                 IF   ACCT-ZAITEN  =  ZERO
                   MOVE  "A"            TO      RECE84-ZAISKBKBN
                 END-IF
              END-IF
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
           MOVE    ACCT-SRYKA           TO      RECE84-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE84-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE84-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE84-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE84-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE84-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE84-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE84-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE84-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE84-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE84-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE84-KIZAITEN
           IF   RECE82-HKNKBN     =  "4" OR "6"
              IF  ACCT-SRYKBN     =  "70"
                  COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                            -  RECE84-SYUTEN2
                  COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                            +  RECE84-SYUTEN2
              END-IF
           END-IF
           MOVE    ACCT-DRCD            TO      RECE84-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE84-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE84-KAISU (IDX3 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE84-KAISU(1 IDX2)   NOT =  ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           MOVE      ZERO     TO    FLG-OKIKAE
           IF   ( RECE82-AGE       >=  75        )
           OR   ( RECE82-BIRTHDAY  <  "19321001" )
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL    ( IDX2        >    31 )
                             OR     ( FLG-OKIKAE  =     1 )
                  IF  ( RECE82-BYOTOSYUBETU(IDX2) =  "02" OR "03" )
                  AND ( RECE84-KAISU(1 IDX2)  NOT =  ZERO         )
                        MOVE   1   TO   FLG-OKIKAE
                  END-IF    
               END-PERFORM
           END-IF
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK   TO    RECE84-SAFLG
           END-IF
      *
           .
       2201-RECE84-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2202-RECE85-HENSYU-SEC               SECTION.
      *
           INITIALIZE     WRK-ORCSGETGE-OUT-AREA
           MOVE    ZERO                 TO      FLG-IRYORENKEI
           IF     (WRK-PARA-PRTKBN       =  "2" OR "3" OR "4" OR "5")
           AND    (SYS-200501-INSPECT-GENERAL-KBN  =  "1"           )
           AND    (RECE82-SRYYM         >=  "201204"                )
             PERFORM 22023-IRYORENKEI-HENSYU-SEC
           END-IF
           MOVE    ZERO                 TO      IDX17
      *
           MOVE    SPACE                TO      WRK-CHECK-SRYCD
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
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
      *
               IF    WRK-CHECK-SRYCD  =  SPACE
                 MOVE   SRY-SRYCD(1)    TO      WRK-CHECK-SRYCD
               END-IF
      *
               MOVE     ZERO    TO    FLG-JYOGAI
               IF   (ACCT-SRYKBN  =  "90" OR "92")
               AND  (ACCT-JIHOKBN =  SPACE       )
                 PERFORM  VARYING  ID4    FROM   1   BY   1
                           UNTIL  (ID4                  >   100 )
                             OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                             OR   (FLG-JYOGAI           =   1   )
                   IF  SRY-SRYCD(1) = WRK-CHUKSNSRYCD(ID4)
                      MOVE     1    TO   FLG-JYOGAI
                   END-IF
                 END-PERFORM
               END-IF
      *
      *        Ï«ºÒÆþ±¡¤Î²Ã»»ÂÐ¾Ý¥³¡¼¥É¤ò¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÇÈ½Äê
               IF ((SRY-SRYCD(1)  =  "190080470"  OR  "190799670"
                                 OR  "190080570"  OR  "190799770"
                                 OR  "190111690"  OR  "190809090"
                                 OR  "190111790"  OR  "190809190"
                                 OR  "190807570"  OR  "190808670"
                                 OR  "190116990"  OR  "190813390") AND
                   (ACCT-JIHOKBN  =   SPACE                      ) AND
                   (RECE82-SRYYM     <= "200603"                 ))
               OR ((SRY-SRYCD(1)  =  "190109170"  OR  "190109270") AND
                   (ACCT-JIHOKBN  =   SPACE                      ) AND
                   (RECE82-SRYYM     <= "200606"                 ))
               OR ((SRY-SRYCD(1)  =  "190117070"  OR  "190109670") AND
                   (ACCT-JIHOKBN  =   SPACE                      ) AND
                   (RECE82-SRYYM     >= "200604"                 ) AND
                   (RECE82-SRYYM     <= "200803"                 ))
               OR ((SRY-SRYCD(1)  =  "190121970"  OR  "190122670") AND
                   (ACCT-JIHOKBN  =   SPACE                      ) AND
                   (RECE82-SRYYM     >= "200607"                 ) AND
                   (RECE82-SRYYM     <= "201203"                 ))
               OR ((SRY-SRYCD(1)  =  "190119210"                 ) AND
                   (ACCT-JIHOKBN  =   SPACE                      ) AND
                   (RECE82-SRYYM     >= "200804"                 ) AND
                   (RECE82-SRYYM     <= "201203"                 ))
               OR ( FLG-JYOGAI    =   1                           )
                 MOVE   ZERO         TO   FLG-WRITE85
               ELSE
                 PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE    ZERO         TO   FLG-ZANHAIKI
                                             FLG-FORMAL-KISAI
                   MOVE    ZERO         TO   WRK-RENNUM
                                             WRK-RENNUM2
                   MOVE    ZERO         TO   FLG-WRITE85X
                   PERFORM   22021-RECE85-TENSOU-SEC
                   IF   FLG-TENTEKI  =  ZERO
                   AND  FLG-WRITE85X =   1
                     IF  ACCT-JIHOKBN  =  SPACE
                       PERFORM 22022-RECE82-HENSYU-SEC
                     END-IF
                     PERFORM 900-RECE85-WRITE-SEC
                     MOVE  1            TO   FLG-WRITE85
                   END-IF
                   IF   FLG-ZANHAIKI  =  1
                      PERFORM  900-RECE85-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  900-RECE85-REWRITE2-SEC
                   END-IF
                   MOVE    "tbl_sryact" TO   MCP-TABLE
                   MOVE    "key9"       TO   MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
                 END-PERFORM
               END-IF
           END-IF
           MOVE    "tbl_sryact"         TO   MCP-TABLE
           MOVE    "key9"               TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2202-RECE85-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       22021-RECE85-TENSOU-SEC                SECTION.
      *
           INITIALIZE          WRK-JIBAIKZI-TNS-TAIHI-TABLE
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           MOVE    SRY-ZAINUM           TO      RECE85-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE85-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE85-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE85-SRYSYUKBN
                                                RECE84-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           MOVE    ZERO                 TO      FLG-OSINKBN
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
            ADD    1                    TO      IDX17
      *
      *     Ï·¿ÍÀº¿ÀÉÂÅïÅù¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÃÖ¤­´¹¤¨
            IF   RECE82-SRYYM  >= "200604"
              CONTINUE
            ELSE
              IF   FLG-OKIKAE   =   1
                EVALUATE  SRY-SRYCD(IDX2)
      *           ·ì±Õ³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160061810"
                      MOVE  "160703610"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£±¡Ë
                  WHEN  "160170170"
                      MOVE  "160710070"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  539900
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£²¡Ë
                  WHEN  "160161610"
                      MOVE  "160709310"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  547700
      *           À¸²½³ØÅª¸¡ºº¡Ê£±¡ËÈ½ÃÇÎÁ
                  WHEN  "160061910"
                      MOVE  "160703710"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           À¸²½³ØÅª¸¡ºº¡Ê£²¡ËÈ½ÃÇÎÁ
                  WHEN  "160062010"
                      MOVE  "160703810"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           Ç¢¡¦ÊµÊØÅù¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160061710"
                      MOVE  "160703510"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ÈùÀ¸Êª³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062210"
                      MOVE  "160704010"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ÌÈ±Ö³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062110"
                      MOVE  "160703910"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
                END-EVALUATE
              END-IF
            END-IF
      *
            MOVE    SRY-SRYCD(IDX2)    TO    WRK-SRYCD
            MOVE    RECE85-SRYYM       TO    WK-TAIHI-SRYYM
            MOVE    WK-TAIHI-DAY       TO    WK-TAIHI-SRYD
            PERFORM    900-TENSU-TBL-SEC
      *
            IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE84-MEISAISU  =  RECE84-MEISAISU
                                             -  1
            END-IF
      *
            IF       SRY-SRYCD(IDX2)  =  "099999909" OR "099999910" OR
                                         "099999921" OR "099999925" OR
                                         "099999929" OR "099999930" OR
                                         "099999931"
                  MOVE    1     TO    FLG-HENSYU-NG
            END-IF
      *
            IF       SRY-SRYCD(IDX2)  =  "099999922"
               IF    (ACCT-JIHOKBN     =   SPACE)
               AND   (RECE82-HKNKBN    =   "6"  )
                  MOVE    1     TO    FLG-KIN-OKIKAE
               ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
               END-IF
            END-IF
      *
            IF       SRY-SRYCD(IDX2)  =  "099999923"
               IF    (ACCT-JIHOKBN          =  SPACE)
               AND   (RECE82-HKNKBN         =  "4"  )
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
               AND   (RECE82-HKNKBN         =  "1"  OR  "2")
               AND   (RECE82-SYOKAIFLG      =  "1"         )
                  MOVE   SPACE  TO    RECE82-SYOKAIFLG
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
               IF    ACCT-JIHOKBN     =   SPACE
                  EVALUATE  SRY-SRYCD(IDX2)
                    WHEN     "099999935"
                      IF  RECE82-RIHA2-SINDAI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    1     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999936"
                      IF  RECE82-RIHA2-NOUKE    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    2     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999937"
                      IF  RECE82-RIHA2-UNDOKI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    3     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999938"
                      IF  RECE82-RIHA2-KOKYUKI  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    4     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999939"
                      IF  RECE82-RIHA2-SESSYOKU = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    5     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999940"
                      IF  RECE82-RIHA2-HAIYO    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    6     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999941"
                      IF  RECE82-RIHA2-SINDAI9  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    7     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999942"
                      IF  RECE82-RIHA2-KOKYUKI9 = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    8     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999943"
                      IF  RECE82-RIHA2-HAIYO9   = 1
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
            OR     ( SRY-SRYCD(IDX2)       =  "099509901"  )
      *
                  PERFORM   22021A-MEISAISU-HENSYU-SEC
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
                     PERFORM   220218-ICHIBUCOM-HENSYU-SEC
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
                     PERFORM   220219-ICHIBUCOM-HENSYU2-SEC
                   END-IF
                 END-IF
               END-IF
               IF   FLG-RIHA-OKIKAE  NOT =  ZERO
                     PERFORM   22021B-RIHAYMD-HENSYU-SEC
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
                  MOVE   "1"    TO    RECE84-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209812"
                  MOVE   "2"    TO    RECE84-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209821"
                  MOVE   "3"    TO    RECE84-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209822"
                  MOVE   "4"    TO    RECE84-JISANYAKKBN
               END-IF
      *
               IF      SRY-SRYCD(IDX2)    =     "099209711"
                  MOVE   "5"    TO    RECE84-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209712"
                  MOVE   "6"    TO    RECE84-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209721"
                  MOVE   "7"    TO    RECE84-JISANYAKKBN
               END-IF
               IF      SRY-SRYCD(IDX2)    =     "099209722"
                  MOVE   "8"    TO    RECE84-JISANYAKKBN
               END-IF
      *
              ELSE
      *
               ADD     1                  TO    IDX4    
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE85-SRYCD(IDX4)
      *
               IF      SRY-SRYCD(IDX2)    =     "101910060"
               AND     ACCT-JIHOKBN       =     SPACE
                  EVALUATE  RECE82-HOSPSBT
                     WHEN     "1"
                          MOVE     "3"    TO    RECE82-HOSPSBT
                     WHEN     "2"
                          MOVE     "4"    TO    RECE82-HOSPSBT
                  END-EVALUATE
               END-IF
      *
               IF      SRY-SRYKBN         =     "13"
               AND     ACCT-JIHOKBN       =     SPACE
                   IF   SRY-SRYCD(IDX2)   =  "113001810" OR "113001910"
                                          OR "113002010" OR "113700610"
                                          OR "113700710" OR "113700810"
                                          OR "113029010"
                        MOVE   "1"        TO    RECE82-BEDSUFLG
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)      =    "160177770"
                  MOVE   "1"              TO    RECE82-KENTAIKASANFLG
                  MOVE   99               TO    RECE84-KNSJISGRPKBN
               END-IF
               IF     RECE84-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE84-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN         =     "60" OR "64" )
               AND   ( TNS-KNSJISKBN      =      2           )
                  MOVE  TNS-KNSJISKBN     TO    RECE84-KNSJISKBN
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE84-ZAITEN     =     ZERO                 )
               AND   ( RECE84-SAFLG      =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE84-SAFLG
                  END-IF
               END-IF
      *
               IF      ACCT-JIHOKBN  NOT =     SPACE
                  IF     RECE84-SAFLG   NOT =  SPACE
                        MOVE    "3"       TO    RECE84-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   RECE84-TOYAKUFLG = SPACE
                          MOVE  "4"       TO    RECE84-TOYAKUFLG
                        END-IF
                    ELSE
                        MOVE    "3"       TO    RECE84-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE84-ZAITEN     =     ZERO         )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
               AND   ( ACCT-JIHOKBN      =     SPACE        )
                  IF     RECE84-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE84-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "212" OR "292" OR
                                               "222" OR "232" OR
                                               "295"
                          IF   RECE84-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE84-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE84-TOYAKUFLG
                        IF   (RECE85-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                  NOT =  SPACE)
                          MOVE    "1"  TO  RECE85-INGAIFLG-RECE(IDX4)
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO    RECE85-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO    RECE85-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO    RECE85-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE85-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE85-GENERICYUKOKETA(IDX4) =
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
               AND   ( RECE84-ZAITEN     =     ZERO  )
               AND   ( ACCT-ZAIKBN   NOT =     1     )
               AND   ( ACCT-JIHOKBN      =     SPACE )
                  IF     RECE84-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE84-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          IF   RECE84-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE84-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE84-TOYAKUFLG
                        IF   (RECE85-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                  NOT =  SPACE)
                          MOVE    "1"  TO  RECE85-INGAIFLG-RECE(IDX4)
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO    RECE85-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO    RECE85-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO    RECE85-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE85-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE85-GENERICYUKOKETA(IDX4) =
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
                      (RECE82-SRYYM  <  "201204"       ))
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE85-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE85-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-YKZ-PRTKBN
                         PERFORM   22021A-MEISAISU-HENSYU-SEC
                   ELSE
                     IF  FLG-YKZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE85-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE85-RECEPTFLG(IDX4)
                         PERFORM   22021A-MEISAISU-HENSYU-SEC
                       ELSE
                         MOVE   ZERO   TO  FLG-YKZ-PRTKBN
                       END-IF
                     END-IF
                   END-IF
                 ELSE
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE84-YKZKISAIFLG
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
                         MOVE   "1"    TO  RECE85-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE85-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-KIZ-PRTKBN
                         PERFORM   22021A-MEISAISU-HENSYU-SEC
                     END-IF
                   ELSE
                     IF  FLG-KIZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE85-RECEDENFLG(IDX4)
                         MOVE   "1"    TO  RECE85-RECEPTFLG(IDX4)
                         PERFORM   22021A-MEISAISU-HENSYU-SEC
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
                       COMPUTE  RECE84-RUIKEITOTAL  =
                                RECE84-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       IF  (ACCT-SRYKBN          =  "80" )
                       AND (SRY-SRYCD(IDX2)(1:5) =
                            "09591" OR "09592" OR "09593"
                                               OR "09594")
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
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
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
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE85-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE85-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE85-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE85-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE85-INPUTNUM(IDX4)
      *
               IF   RECE82-HKNKBN    NOT  =  "4" AND "6"
                  IF ((SRY-SRYCD(IDX2)  =  "101130010"     ) AND
                      (RECE82-SRYYM     <  "200804"        ))
                  OR ((SRY-SRYCD(IDX2)  =
                       "101110020"   OR
                       "101130020"   OR  "101130030"   OR
                       "101130040"   OR  "101130050"   OR
                       "101130060"   OR  "101130070"   OR
                       "101130080"   OR  "101130090"   OR
                       "101130100"                         ) AND
                      (RECE82-SRYYM    <=  "201306"        ))
                  OR ((SRY-SRYCD(IDX2)  =
                       "101800880"   OR
                       "101800870"   OR  "101800470"   OR
                       "101800480"   OR  "101800490"   OR
                       "101800500"   OR  "101800510"   OR
                       "101800520"   OR  "101800530"   OR
                       "101800540"   OR  "101800960"   OR
                       "101800970"   OR  "101800980"   OR
                       "101801040"                         ) AND
                      (RECE82-SRYYM    >=  "201307"        ))
                          MOVE   "80"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                  ELSE
                    IF      TNS-SRYKBN         =     "80"
                    OR      RECE84-SRYKBN      =     "80"
                      IF  TNS-NYUTENTTLKBN   =     130
                          MOVE   "13"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                      ELSE
                       IF SRY-SRYCD(IDX2)    =     "101989050"
                          MOVE   "11"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                       END-IF
                      END-IF
                    END-IF
                  END-IF
               ELSE                                                     
                  IF      SRY-SRYCD(IDX2)    =     "140037490"
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-YOUBUKASAN   =  WRK-YOUBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     "140040110"
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KYOBUKASAN   =  WRK-KYOBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     "140052610"
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KEIBUKASAN   =  WRK-KEIBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     "150266970"
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-SOGAIKASAN   =  WRK-SOGAIKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
      *           ¼«ÇåÀÕ¡¦ÎÅÍÜÃ´Åö¼êÅöÍÑÆÃÎã½èÍý
      *           ¡Ê¼«ÇåÀÕ¤Î¾ì¹ç¡¢£±£°ÅÀ¤ò£±£²£°±ß¤ËÆÉ¤ßÂØ¤¨¤ë¡Ë
      *           ¡ÊÎÅÍÜÃ´Åö¼êÅö¡§£±·î£±²ó¤Î»»Äê¡Ë
                  IF      SRY-SRYCD(IDX2)    =     "199000510"
                    IF   (RECE82-HKNKBN    =  "4")
                    AND  (RECE82-JIBAIKBN  =  "1")
                        MOVE     120    TO     WRK-RYOYOTEATE2
                        COMPUTE  WRK-RYOYOTEATE   =
                                 WRK-RYOYOTEATE   +
                                 WRK-RYOYOTEATE2
                    ELSE
                        COMPUTE  WRK-RYOYOTEATE   =  WRK-RYOYOTEATE
                                                  +  TNS-TEN
                        COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                                  -  TNS-TEN
                        COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                                  +  TNS-TEN
                    END-IF
                  END-IF
      *
                  IF ( ACCT-SRYKBN          =  "80"               ) AND
                     ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592" )
      ***            EVALUATE  SRY-SRYCD(IDX2)(1:5)
      ***             WHEN      "09591"
      ***               IF  RECE82-SINDAN-TUU < 2
      ***                 MOVE   "95"   TO    RECE84-SRYKBN
      ***                                     RECE85-SRYKBN
      ***               END-IF
      ***             WHEN      "09592"
      ***               IF  RECE82-MEISAI-TUU < 2
      ***                 MOVE   "95"   TO    RECE84-SRYKBN
      ***                                     RECE85-SRYKBN
      ***               END-IF
      ***            END-EVALUATE
                     MOVE    1          TO    FLG-SRYKBN-HEN
                  ELSE
                   IF ( SRY-SRYCD(IDX2)(1:5) = "09593"       )
                   OR ( SRY-SRYCD(IDX2)(1:5) = "09594"       )
                   OR ((SRY-SRYCD(IDX2)  =  "101130010"     ) AND
                       (RECE82-SRYYM     <  "200804"        ))
                   OR ((RECE82-HKNKBN    =  "4"             ) AND
                       (RECE82-JIBAIKBN  =  "1"             ) AND
                       (SRY-SRYCD(IDX2)  =  "199000510"     ))
                   OR ((SRY-SRYCD(IDX2)  =
                        "101110020"   OR
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"   OR
                        "101910010"   OR  "101910020"   OR
                        "101910030"   OR  "101910040"   OR
                        "101910070"   OR  "101910080"   OR
                        "101910090"   OR  "101910100"       ) AND
                       (RECE82-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX2)  >= "101910110" AND
                                         <= "101910400"     ) AND
                       (RECE82-SRYYM    <=  "201306"        ))
                   OR ((SRY-SRYCD(IDX2)  =
                        "101800880"   OR
                        "101800390"   OR  "101800400"   OR
                        "101800410"   OR  "101800420"   OR
                        "101800430"   OR  "101800440"   OR
                        "101800450"   OR  "101800460"   OR
                        "101800870"   OR  "101800470"   OR
                        "101800480"   OR  "101800490"   OR
                        "101800500"   OR  "101800510"   OR
                        "101800520"   OR  "101800530"   OR
                        "101800540"   OR  "101800960"   OR
                        "101800970"   OR  "101800980"   OR
                        "101801040"   OR  "101801150"   OR
                        "101801160"   OR  "101801170"   OR
                        "101801180"                         ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)  >= "101800570" AND
                                         <= "101800860"     ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)  >= "101801010" AND
                                         <= "101801020"     ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)  >= "101801190" AND
                                         <= "101801420"     ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                   OR ((SRY-SRYCD(IDX2)   = "101800550" OR
                             "101800560" OR "101800990" OR
                             "101801030"                    ) AND
                       (RECE82-SRYYM    >=  "201307"        ))
                     IF   SRY-SRYCD(IDX2)(1:5) =  "09593"
                       MOVE  TNS-KANANAMECHGKBN
                                      TO  WRK-JIBAIKZI-SYUKEIKBN(IDX2)
                                          RECE84-JIBAISYUKEIKBN
                       EVALUATE  TNS-KANANAMECHGKBN
                         WHEN      0
                         WHEN      4
                         WHEN      5
                            MOVE   "80"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                         WHEN      1
                            MOVE   "11"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                       END-EVALUATE
                     ELSE
                       MOVE   "80"        TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                     END-IF
                     MOVE      1          TO  FLG-SRYKBN-HEN
                   ELSE                                                 
                     IF     TNS-SRYKBN         =   "80"
                     OR     RECE84-SRYKBN      =   "80"
                       IF   TNS-NYUTENTTLKBN   =   130
                            MOVE   "13"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                       END-IF
                     END-IF
                     IF  (WRK-PARA-JIBAIPRTKBN =  "0" )
                     AND (FLG-SRYKBN-HEN       =  ZERO)
                       IF (( RECE84-SRYKBN     =  "80" ) AND
                           ( RECE84-OKIKAEFLG  =  SPACE))
                       OR (  RECE84-OKIKAEFLG  =  "1"   )
                        IF   SRY-SRYCD(IDX2)   =  "199000510"
                            CONTINUE
                        ELSE
                            MOVE   "1"    TO  RECE84-OKIKAEFLG
                            MOVE   "40"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                        END-IF
                       END-IF
                     END-IF
                     IF (( RECE84-SRYKBN     =  "14" ) AND
                         ( RECE84-OKIKAEFLG  =  SPACE))
                     OR (  RECE84-OKIKAEFLG  =  "2"   )
                            MOVE   "2"    TO  RECE84-OKIKAEFLG
                            MOVE   "80"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                     END-IF
                   END-IF                                               
                  END-IF                                                
      *
               END-IF                                                   
      *
               IF      TNS-SRYKBN         =     "12"
                 PERFORM  VARYING  IDX31  FROM  1  BY  1
                           UNTIL   IDX31    >  31
                   IF   ACCT-DAY(1 IDX31)   NOT =   ZERO
                     IF   RECE82-NYUDAY-SRYKBN(IDX31) = "90"
                       MOVE   "90"        TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                                              WRK-NYUSAI-SRYKBN
                     ELSE
                       MOVE   "92"        TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                                              WRK-NYUSAI-SRYKBN
                     END-IF
                     MOVE     "1"         TO  RECE84-NYUSAIFLG
                     MOVE      31         TO  IDX31
                   END-IF
                 END-PERFORM
               END-IF
      *
               IF      ACCT-JIHOKBN       =     SPACE
                  PERFORM  220211-RECE85-COMCNT-HEN-SEC
               ELSE
                  IF   RECE84-SRYKBN      =     "50"
                     MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
                  END-IF
                  IF   RECE84-SRYKBN      =     "54"
                     MOVE   "1"   TO    RECE84-FLG-MASUI-COMMENT
                  END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE85-RECEDENFLG(IDX4)
                  MOVE    "1"             TO  RECE85-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE85-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.99999                   
                  COMPUTE RECE85-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE85-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE85-TANINAME(IDX4)   
                    IF    RECE85-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE85-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE85-SRYSURYO(IDX4 - 1) 
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE85-RECEDENFLG(IDX4)
                    MOVE  "1"             TO  RECE85-RECEPTFLG(IDX4)
                  END-IF                                                
                END-IF
               ELSE                                                     
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO    RECE85-TANICD(IDX4)
                                                WRK-TANICD
                  END-IF
                  MOVE    TNS-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE85-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF ( ACCT-SRYKBN          =  "80"              ) AND     
                  ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                         OR "09593" OR "09594")         
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    IF     (RECE82-HKNKBN        =  "4" OR "6"        )
                    AND    (SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                            OR "09593")
                      IF    SRY-SRYCD(IDX2)(1:5) =  "09593"
                        PERFORM   220212-JIBAI-KIZAI-TENHEN-SEC
                        MOVE  WRK-TEN-KZI TO  RECE85-TEN(IDX4)
                        MOVE  WRK-TEN-KZI TO  WRK-JIBAIKZI-TNS(IDX2)
                      ELSE
                        PERFORM   220214-JIBAI-BUNSYO-TENHEN-SEC
                        MOVE  WRK-TEN-BUN TO  RECE85-TEN(IDX4)
                        MOVE  WRK-TEN-BUN TO  WRK-JIBAIKZI-TNS(IDX2)
                      END-IF
                    ELSE
                      IF  SRY-SRYSYUKBN = "809"
                        MOVE   SRY-JIHIMONEY(IDX2)
                                          TO  RECE85-TEN(IDX4)
                        MOVE   SRY-JIHIMONEY(IDX2)
                                          TO  WRK-JIBAIKZI-TNS(IDX2)
                      ELSE
                        MOVE   TNS-TEN    TO  RECE85-TEN(IDX4)
                        MOVE   TNS-TEN    TO  WRK-JIBAIKZI-TNS(IDX2)
                      END-IF
                    END-IF
               ELSE                                                     
                 IF   RECE82-HKNKBN     =  "4"
                 AND  RECE82-JIBAIKBN   =  "1"
                 AND  SRY-SRYCD(IDX2)   =  "199000510"
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    MOVE  WRK-RYOYOTEATE2 TO  RECE85-TEN(IDX4)
                 ELSE
                    MOVE  TNS-TENSIKIBETU TO  RECE85-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN         TO  RECE85-TEN(IDX4)
                 END-IF
               END-IF                                                   
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE85-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE85-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE85-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE85-KNSJISGRPKBN   (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE85-KENTAICOMMENT  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1
                                      TO RECE85-KOKUJISKBKBN1  (IDX4)
               MOVE    TNS-KOKUJISKBKBN2
                                      TO RECE85-KOKUJISKBKBN2  (IDX4)
               MOVE    TNS-TUSOKUGAIKBN
                                      TO RECE85-TUSOKUGAIKBN   (IDX4)
               MOVE    TNS-TIMEKSNKBN TO RECE85-TIMEKSNKBN     (IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE85-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE85-CHUKSNTUBAN    (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE85-TOKUKIZAISBT1  (IDX4)
               MOVE    TNS-SANSOKBN   TO RECE85-SANSOKBN       (IDX4)
               IF      TNS-SRYCD (1:2)   =   "84"
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       10
                       MOVE    TNS-SSTKIJUNCD (IDYY)
                                      TO RECE85-SSTKIJUNCD(IDX4 IDYY)
                   END-PERFORM
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE82-SRYYM   <=  "201003"   )
                      MOVE   202      TO  RECE85-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE85-CDKBN-KOUBAN(IDX4)
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
                      MOVE   203      TO  RECE85-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE85-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE82-SRYYM   >=  "201004"   )
                      MOVE   "1"      TO  RECE84-GAZOGEN
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
                   PERFORM 900-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT
                                           TO  RECE85-NAME(IDX4)
                                               RECE85-INPUTCOMENT(IDX4)
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
                                           RECE85-INPUTCOMENT   (IDX4)
                           END-IF
                         ELSE
                               MOVE    SPACE       TO
                                           RECE85-INPUTCOMENT   (IDX4)
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
                                           RECE85-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE85-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
      ***          MOVE   "1"                  TO    RECE84-COMCDFLG
               ELSE
                   MOVE    TNS-NAME        TO  RECE85-NAME(IDX4)
                                               RECE85-INPUTCOMENT(IDX4)
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                  IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO
                    PERFORM  220215-RECE85-SURYO-HEN-SEC
      *
                    MOVE    SPACE      TO  RECE85-NAME(IDX4)
                    MOVE    SPACE      TO  RECE85-INPUTCOMENT(IDX4)
      ***           STRING  TNS-NAME          DELIMITED  BY  " "
      ***                   "¡¡"              DELIMITED  BY  SIZE
      ***                   WRK-SRYSURYOW     DELIMITED  BY  " "
      ***                   RECE85-TANINAME(IDX4)
      ***                                     DELIMITED  BY  " "
      ***                   INTO              RECE85-NAME(IDX4)
      ***           END-STRING
                    STRING  "¡Ê»ÈÍÑÎÌ¡¡"      DELIMITED  BY  SIZE
                            WRK-SRYSURYOW     DELIMITED  BY  " "
                            RECE85-TANINAME(IDX4)
                                              DELIMITED  BY  " "
                            "¡¡»ÄÎÌÇÑ´þ¡Ë"    DELIMITED  BY  SIZE
                            INTO              RECE85-NAME(IDX4)
                    END-STRING
                    MOVE  RECE85-NAME(IDX4)
                                       TO  RECE85-INPUTCOMENT(IDX4)
      *
      ***           MOVE  "810000001"  TO  RECE85-SRYCD(IDX4)
                    MOVE  ZERO         TO  RECE85-SRYSURYO(IDX4)
                    MOVE  ZERO         TO  RECE85-TANICD(IDX4)
                    MOVE  ZERO         TO  RECE85-TANIMOJISU(IDX4)
                    MOVE  SPACE        TO  RECE85-TANINAME(IDX4)
                    MOVE  ZERO         TO  RECE85-YUKOKETA(IDX4)
                    MOVE  "2"          TO  RECE85-MEISKYFLG(IDX4)
                  END-IF
               END-IF
      *
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                        MOVE     "1"       TO    RECE84-COMCDFLG
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
                      IF  RECE85-NAME(IDX4)(IDV2:2)   =  "Æü"
                        MOVE   SPACE  TO  RECE84-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE85-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE85-NAME(IDX4)
                                              WRK-FORMALNAME
                 MOVE  "1"                TO  RECE85-RECEDENFLG(IDX4)
      *
                 MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                 IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                    MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                    MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                 END-IF
                 MOVE     ZERO            TO  RECE85-TEN(IDX4)
                 MOVE     ZERO            TO  RECE85-TANICD(IDX4)
                 MOVE     ZERO            TO  RECE85-TANIMOJISU(IDX4)
                 MOVE     SPACE           TO  RECE85-TANINAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE85-RENNUM  >  1
                    AND   IDX4           =  1
                      MOVE    1           TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE85-RENNUM  -  1
                    ELSE
                      MOVE  "1"           TO  RECE85-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  WRK-FORMALNAME  TO  RECE85-RECEDENNAME(IDX4)
                    MOVE  "1"             TO  RECE85-RECEHENFLG(IDX4)
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                          TO  RECE85-TEN(IDX4)
                    END-IF
                    IF   (WRK-KZI058-TANICD  NOT = ZERO)
                    AND  (RECE85-TANICD(IDX4)    = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                          TO  RECE85-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                          TO  RECE85-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                          TO  RECE85-TANINAME(IDX4)
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
               AND (RECE85-NAME(IDX4)(1:14)
                                          =  "ÊÝ¸±Ì¾¾ÎÊÑ¹¹¡§")
                  MOVE     "1"          TO  RECE85-RECEPTFLG(IDX4)
                  MOVE     "1"          TO  RECE85-RECEDENFLG(IDX4)
      *
                  MOVE      ZERO        TO  IDW3
                  PERFORM   VARYING   IDW   FROM   300   BY  -1
                             UNTIL    IDW     <    15
                       IF  RECE85-NAME(IDX4)(IDW:1)  NOT =  SPACE
                           COMPUTE IDW6       =  (IDW / 2) - 7
                           MOVE    IDW        TO  IDW3
                           MOVE    1          TO  IDW
                       END-IF
                  END-PERFORM
                  IF     IDW3    NOT =  ZERO
                    MOVE  SPACE TO RECE85-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  RECE85-NAME(IDX4)(15:IDW4)
                                TO RECE85-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ"
                                TO RECE85-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE85-YUKOKETA(IDX4 - 1) =
                              RECE85-YUKOKETA(IDX4 - 1) -
                              3                         +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE85-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   300   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
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
                                         OR  "09592"
                                         OR  "09593"
                                         OR  "09594"  ))                
               OR (( SRY-SRYCD(IDX2) = "101130010"    )   AND
                   ( RECE82-SRYYM    < "200804"       ))
               OR (( SRY-SRYCD(IDX2) =
                     "101110020"   OR
                     "101130020"   OR  "101130030"   OR
                     "101130040"   OR  "101130050"   OR
                     "101130060"   OR  "101130070"   OR
                     "101130080"   OR  "101130090"   OR
                     "101130100"   OR
                     "101910010"   OR  "101910020"   OR
                     "101910030"   OR  "101910040"   OR
                     "101910070"   OR  "101910080"   OR
                     "101910090"   OR  "101910100"    )   AND
                   ( RECE82-SRYYM   <= "201306"       ))
               OR (( SRY-SRYCD(IDX2) >= "101910110" AND
                                     <= "101910400"   )   AND
                   ( RECE82-SRYYM   <=  "201306"      ))
               OR (( SRY-SRYCD(IDX2) =
                     "101800880"   OR
                     "101800390"   OR  "101800400"   OR
                     "101800410"   OR  "101800420"   OR
                     "101800430"   OR  "101800440"   OR
                     "101800450"   OR  "101800460"   OR
                     "101800870"   OR  "101800470"   OR
                     "101800480"   OR  "101800490"   OR
                     "101800500"   OR  "101800510"   OR
                     "101800520"   OR  "101800530"   OR
                     "101800540"   OR  "101800960"   OR
                     "101800970"   OR  "101800980"   OR
                     "101801040"   OR  "101801150"   OR
                     "101801160"   OR  "101801170"   OR
                     "101801180"                      )   AND
                   ( RECE82-SRYYM   >= "201307"       ))
               OR (( SRY-SRYCD(IDX2) >= "101800570" AND
                                     <= "101800860"   )   AND
                   ( RECE82-SRYYM   >=  "201307"      ))
               OR (( SRY-SRYCD(IDX2) >= "101801010" AND
                                     <= "101801020"   )   AND
                   ( RECE82-SRYYM   >=  "201307"      ))
               OR (( SRY-SRYCD(IDX2) >= "101801190" AND
                                     <= "101801420"   )   AND
                   ( RECE82-SRYYM   >=  "201307"      ))
               OR (( SRY-SRYCD(IDX2)  = "101800550" OR
                         "101800560" OR "101800990" OR
                         "101801030"                  )   AND
                   ( RECE82-SRYYM   >=  "201307"      ))
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     IF  SRY-SRYSYUKBN = "809"
                        COMPUTE  WRK-TEN = WRK-TEN +
                                          (SRY-JIHIMONEY(IDX2) / 10)
                     ELSE
                      IF SRY-SRYCD(IDX2)  =  "101800550" OR
                              "101800560" OR "101800990" OR
                              "101801030"
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
      *
                     MOVE     "2"        TO    RECE85-SIKFLG(IDX4)
      *
                     IF  (ACCT-SRYKBN          = "80"              )
                     AND (SRY-SRYCD(IDX2)(1:5) =
                           "09591" OR "09592" OR "09593" OR "09594")
                       COMPUTE WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                               + WRK-JIBAIKZI-TNS(IDX2)
                     ELSE
                      IF  SRY-SRYCD(IDX2)  =  "101800550" OR
                               "101800560" OR "101800990" OR
                               "101801030"
                       COMPUTE WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                               + WRK-KEISAN-KG2
                       MOVE    WRK-KEISAN-KG2  TO  RECE85-TEN(IDX4)
                      ELSE
                       COMPUTE WRK-ZAI-KINGAKU = WRK-ZAI-KINGAKU
                                               + TNS-TEN
                      END-IF
                     END-IF
      *
               ELSE
      *
                  IF   RECE82-HKNKBN     =  "4"
                  AND  RECE82-JIBAIKBN   =  "1"
                  AND  SRY-SRYCD(IDX2)   =  "199000510"
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
                     MOVE     "2"        TO  RECE85-SIKFLG(IDX4)
      *
                  ELSE
      *
                    IF  ((RECE82-HKNKBN      =   "4") AND
                         (RECE82-JIBAIKBN    =   "2"))
                    OR  ((RECE82-HKNKBN      =   "5") AND
                         (RECE82-KOMUKBN     =   "2"))
                    OR  ( RECE82-HKNKBN      =   "6" )
                      IF (( ACCT-SRYKBN          =  "80"    ) AND
                          ( WRK-CHECK-SRYCD(1:5) =  "09591"
                                                 OR "09592"
                                                 OR "09593"
                                                 OR "09594" ) AND
                          ( SRY-SRYCD(IDX2)(1:1) =   "8"    ))
                      OR (( ACCT-SRYKBN          =  "80"    ) AND
                          ( WRK-CHECK-SRYCD(1:5) =  "09591"
                                                 OR "09592"
                                                 OR "09593"
                                                 OR "09594" ) AND
                          ( SRY-SRYCD(IDX2)(1:3) =   "008"  ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE85-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE84-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE84-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                      END-IF
                    ELSE
                      IF (  SRY-SRYCD(IDX2) = "840800001" OR "840800002"
                                           OR "820800001" OR "820800002"
                                           OR "820800003" OR "820800004"
                                           OR "830800001"        )
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000020"   ) AND
                          ( ACCT-ZAIKBN         =  ZERO         ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( WRK-CHECK-SRYCD =  "101130010"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM    <  "200804"         ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( WRK-CHECK-SRYCD(1:5)   =  "09591"
                                                   OR "09592"
                                                   OR "09593"
                                                   OR "09594"   ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ))
                      OR (( WRK-CHECK-SRYCD = "101110020"       ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( WRK-CHECK-SRYCD =
                            "101130020"   OR  "101130030"   OR
                            "101130040"   OR  "101130050"   OR
                            "101130060"   OR  "101130070"   OR
                            "101130080"   OR  "101130090"   OR
                            "101130100"                         ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD =
                            "101910010"   OR  "101910020"   OR
                            "101910030"   OR  "101910040"   OR
                            "101910070"   OR  "101910080"   OR
                            "101910090"   OR  "101910100"       ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD >= "101910110" AND
                                            <= "101910400"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   <=  "201306"         ))
                      OR (( WRK-CHECK-SRYCD = "101800880"       ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD =
                            "101800390"   OR  "101800400"   OR
                            "101800410"   OR  "101800420"   OR
                            "101800430"   OR  "101800440"   OR
                            "101800450"   OR  "101800460"   OR
                            "101800870"   OR  "101800470"   OR
                            "101800480"   OR  "101800490"   OR
                            "101800500"   OR  "101800510"   OR
                            "101800520"   OR  "101800530"   OR
                            "101800540"   OR  "101800960"   OR
                            "101800970"   OR  "101800980"   OR
                            "101801040"   OR  "101801150"   OR
                            "101801160"   OR  "101801170"   OR
                            "101801180"                         ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD >= "101800570" AND
                                            <= "101800860"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD >= "101801010" AND
                                            <= "101801020"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( WRK-CHECK-SRYCD >= "101801190" AND
                                            <= "101801420"      ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM   >=  "201307"         ))
                       IF   WRK-CHECK-SRYCD(1:6) = "101989"
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                       ELSE
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE85-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE84-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE84-COMCDFLG
                         END-IF
                       END-IF
                      ELSE
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
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
                     IF      RECE85-TEN(IDX4)  =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 220213-RECE85-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE85-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE85-NAME(IDX4)(IDW:)   
                       COMPUTE RECE85-YUKOKETA(IDX4)
                                               =   RECE85-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF
      *
               IF    SRY-SRYKBN     =  "14"
      *
                 EVALUATE    SRY-SRYCD(IDX2)
      *±ý¿Ç¡ÊÆÃÊÌ±ý¿Ç¡Ë
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   WHEN    "114002470"
      ***          WHEN    "114002970"
                   IF  FLG-OSINKBN        NOT = 2  AND  3
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
                       MOVE     2         TO    FLG-OSINKBN
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
                       MOVE     3         TO    FLG-OSINKBN
                 END-EVALUATE
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE84-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE85-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE85-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE85-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE85-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE84-COMCDFLG
                     MOVE  "810000001"  TO  RECE85-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE85-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE85-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE85-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE85-TANINAME(IDX4)
               END-IF
      *
               PERFORM  220216-SETCOM-HENSYU-SEC
      *
               IF   (RECE82-SRYYM           >= "201810")
               AND  (RECE85-SRYCD(IDX4)(1:4) = "8201"  )
                 MOVE   "1"         TO  RECE84-SENTAKUCOMFLG
               END-IF
      *
      *        ¥ì¥»ÅÅ¡ÊÅÀÅ©¤Îµ­Ï¿½çÊÂ¤ÓÂØ¤¨ÍÑ¡Ë
               IF      SRY-SRYKBN         =     "33"
               AND     FLG-HENSYU-OK      =     ZERO
                 PERFORM  220217-TENTEKI-CDKBN-HEN-SEC
                 MOVE     1     TO    FLG-HENSYU-OK
               END-IF
      *
      *        £°£¹£µ£¹£³¤Ç»Ï¤Þ¤ë¥³¡¼¥É¤¬¤¢¤ë¾ì¹ç¡¢Ï«ºÒ¥ì¥»ÅÅ¤ÎºîÀ®ÂÐ¾Ý³°
               IF    (RECE82-HKNKBN           =   "1" OR "2")
               AND   (RECE82-SRYYM           >=   "201307"  )
               AND   (RECE82-RSIIRYOKBN   NOT =   "0"       )
               AND   (SYS-200501-ROURECEKBN   =   "2" OR "3")
                 IF  (ACCT-SRYKBN          =  "80"   )
                 AND (SRY-SRYCD(IDX2)(1:5) =  "09593")
                    MOVE     1      TO  RECE82-ORIGINALCDKBN
                 END-IF
               END-IF
      *
               IF    (RECE82-SRYYM         >=  "201704")
               AND   (SRY-SRYCD(IDX2)(1:3)  =  "059"   )
                 IF   (RECE82-HKNKBN   NOT =  "1" AND "2" )
                 OR   (RECE82-RECESYUBETU  =  "0000"      )
                 OR   (RECE82-ERR-ERRKBN   >   ZERO       )
                 OR   (SRY-SRYKBN          =  "95" OR "96")
      ***        OR   ((WRK-PARA-PRTKBN    =  "1"   ) AND
      ***              (ACCT-ZAITEN        =   ZERO )     )
                   CONTINUE
                 ELSE
                   MOVE     15      TO  RECE82-ERR-ERRKBN
                   DISPLAY "059 ERR:SRYCD=" SRY-SRYCD(IDX2)
                 END-IF
               END-IF
      *
              END-IF
             END-IF
            END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE85X
           END-IF
      *
           .
       22021-RECE85-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       220211-RECE85-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE84-SRYKBN      =     "11" OR "90" OR "92"
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
                 EVALUATE   RECE82-NINPUFLG
                    WHEN        0
                            MOVE    1    TO    RECE82-NINPUFLG
                    WHEN        2
                            MOVE    3    TO    RECE82-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
           IF      RECE84-SRYKBN      =     "99"
              IF   SRY-SRYCD(IDX2)    =     "820100348"
                 EVALUATE   RECE82-NINPUFLG
                    WHEN        0
                            MOVE    2    TO    RECE82-NINPUFLG
                    WHEN        1
                            MOVE    3    TO    RECE82-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "11"
      *       ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
              IF  (SRY-SRYCD(IDX2)    =     "111011810"
                                      OR    "111012610"
                                      OR    "111012810"
                                      OR    "101110040"         )
              AND (FLG-SYODOU-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE84-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "830000024"
                   MOVE  SPACE  TO    RECE84-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE84-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "40"
      *       ²ðÃ£¸£°ú¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE82-SRYYM      >=    "200604"
              AND  RECE82-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     "140048010"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
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
                        MOVE   "1"    TO    RECE84-FLG-JINZOU-COMMENT
                END-IF
      *         ¶É½ê±¢°µÊÄº¿½èÃÖ
                IF   RECE82-SRYYM      >=    "201204"
                  IF   SRY-SRYCD(IDX2)  =     "140052170"
                                          OR  "140052270"
                                          OR  "140052370"
                        MOVE   "1"    TO    RECE84-FLG-KYOKUIN-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =     "140052410"
                                          OR  "140051810"
                                          OR  "140051910"
                                          OR  "140052010"
                        MOVE   "1"    TO    RECE84-FLG-KYOKUIN-COMMENT
                  END-IF
                END-IF
              END-IF
      *       ¼§µ¤¤Ë¤è¤ëç¯æùÅù»É·ãË¡
              IF   RECE82-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "140055110"         )
                AND (FLG-BOKOHO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-BOKOHO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000085"
                   MOVE  SPACE  TO    RECE84-FLG-BOKOHO-COMMENT
                   MOVE    1    TO    FLG-BOKOHO-COM
                END-IF
              END-IF
      *       ¹âµ¤°µ»ÀÁÇ¼£ÎÅ
              IF  (SRY-SRYCD(IDX2)    =     "140057510"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "1"   TO    RECE84-FLG-KIATU-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "140057610"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "2"   TO    RECE84-FLG-KIATU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000085"
                                      OR    "840000086"
                                      OR    "840000045"
                   MOVE  SPACE  TO    RECE84-FLG-KIATU-COMMENT
                   MOVE    1    TO    FLG-KIATU-COM
              END-IF
      *
      *       ¿´ÉÔÁ´¤ËÂÐ¤¹¤ë±óÀÖ³°Àþ²¹Ç®ÎÅË¡
              IF   RECE82-SRYYM      >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =    "140061210")
                AND (FLG-SCPRO-COM    =    ZERO       )
                   MOVE   "5"   TO    RECE84-FLG-SCPRO-COMMENT
                END-IF
              END-IF
              IF    SRY-SRYCD(IDX2)   =    "840000076"
                   MOVE  SPACE  TO    RECE84-FLG-SCPRO-COMMENT
                   MOVE    1    TO    FLG-SCPRO-COM
              END-IF
           END-IF
      *
           IF    ((RECE82-HKNKBN    NOT  =  "4" AND "6" ) AND
                  (RECE84-SRYKBN         =  "80"        ))
           OR    ((RECE82-HKNKBN         =  "4"  OR "6" ) AND
                  (RECE84-SRYKBN         =  "40" OR "80"))
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
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
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
                                            "101801110"           OR
                                            "101801120"           OR
                                            "101801130"           OR
                                            "180051530"           OR
                                            "180051630"           OR
                                            "180051730"           OR
                                            "180051830"           OR
                                            "180052030"           OR
                                            "180052230"           OR
                                            "180033110"
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
              END-IF
              IF   RECE82-SRYYM      >=    "201307"
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
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
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
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
                END-IF
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     "180029910"
                        MOVE   "2"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     "180030010"
                        MOVE   "3"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     "180030510"
                        MOVE   "4"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     "180030610"
                        MOVE   "5"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     "180030110"
                        MOVE   "6"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     "180030210"
                        MOVE   "7"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     "180030310"
                        MOVE   "8"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     "180030410"
                        MOVE   "9"    TO    RECE84-RIHABIRI-COMMENT
              END-EVALUATE
      *       ¥ê¥Ï¥Ó¥ê¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE82-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     "180027410"
                                      OR    "180027510"
                                      OR    "180029110"
                                      OR    "180029210"
                                      OR    "180029910"
                                      OR    "180030010"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180028010"
                                      OR    "180028110"
                                      OR    "180029710"
                                      OR    "180029810"
                                      OR    "180030510"
                                      OR    "180030610"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KOKYUKI-COMMENT
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
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NOUKE-COMMENT
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
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-UNDOKI-COMMENT
                END-IF
                IF   RECE82-SRYYM      >=    "201307"
                  IF   SRY-SRYCD(IDX2)  =   "101800230"
                                      OR    "101800240"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SINDAI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800340"
                                      OR    "101800350"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KOKYUKI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800250"
                                      OR    "101800260"
                                      OR    "101800270"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NOUKE-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800280"
                                      OR    "101800290"
                                      OR    "101800330"
                                      OR    "101801070"
                                      OR    "101801080"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-UNDOKI-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =   "101800080"
                                      OR    "101800090"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SINDAI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800150"
                                      OR    "101800160"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KOKYUKI-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800100"
                                      OR    "101800110"
                                      OR    "101800120"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NOUKE-COMMENT
                  END-IF
                  IF   SRY-SRYCD(IDX2)  =   "101800130"
                                      OR    "101800140"
                                      OR    "101800125"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-UNDOKI-COMMENT
                  END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180017910"        )
                AND (RECE82-SRYYM    <=     "201803"           )
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NANBYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180028210"
                                      OR    "180028310"
                                      OR    "180028410"         )
                AND (RECE82-SRYYM    <=     "201803"            )
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180016610"
                                      OR    "180054310"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SESSYOKU-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180044610"
                                      OR    "180044710"
                                      OR    "180044810"
                                      OR    "180044910"
                                      OR    "180045110"
                                      OR    "180045310"
                                      OR    "101801110"
                                      OR    "101801120"
                                      OR    "101801130"
                                      OR    "180051530"
                                      OR    "180051630"
                                      OR    "180051730"
                                      OR    "180051830"
                                      OR    "180052030"
                                      OR    "180052230"
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-HAIYO-COMMENT
                END-IF
              END-IF
      *
              IF   (RECE82-SRYYM    >=   "201604"             )
              AND  (SRY-SRYCD(IDX2)  =   "180033870"
                                     OR  "180030770"          )
                IF   RECE84-SINDAI-COMMENT = "1"
                    MOVE   "1"    TO    RECE84-SINDAI9-COMMENT
                END-IF
                IF   RECE84-KOKYUKI-COMMENT = "1"
                    MOVE   "1"    TO    RECE84-KOKYUKI9-COMMENT
                END-IF
                IF   RECE84-HAIYO-COMMENT = "1"
                    MOVE   "1"    TO    RECE84-HAIYO9-COMMENT
                END-IF
              END-IF
      *
      *       ¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»
      *       ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡
              IF    (SRY-SRYCD(IDX2)  =    "180056570")
              AND   (FLG-SCPRO-COM    =    ZERO       )
                    MOVE   "2"    TO    RECE84-FLG-SCPRO-COMMENT
              END-IF
              IF   RECE82-SRYYM      >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =    "180058650")
                AND (FLG-SCPRO-COM    =    ZERO       )
                    MOVE   "4"    TO    RECE84-FLG-SCPRO-COMMENT
                END-IF
              END-IF
              IF    SRY-SRYCD(IDX2)   =    "840000076"
                    MOVE  SPACE   TO    RECE84-FLG-SCPRO-COMMENT
                    MOVE    1     TO    FLG-SCPRO-COM
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     "113010710"           OR
                                            "113010810"           OR
                                            "120001610"
                   MOVE   "1"   TO    RECE84-FLG-YAKUKAN-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "113003310"           OR
                                            "113014810"           OR
                                            "113014910"           OR
                                            "113017610"           OR
                                            "113017710"           OR
                                            "113017810"           OR
                                            "113017910"            )
              AND (RECE82-SRYYM       <=    "202003"               )
                   MOVE   "1"   TO    RECE84-FLG-NYUEIYO-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "190030310"           OR
                                            "190723610"         )
              AND (FLG-HOUMON-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-HOUMON-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000055"
                   MOVE  SPACE  TO    RECE84-FLG-HOUMON-COMMENT
                   MOVE    1    TO    FLG-HOUMON-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "180000710"
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000011"
                                      OR    "840000012"
                                      OR    "840000511"
                                      OR    "840000512"
                   MOVE  SPACE  TO    RECE84-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "113004310"
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000078"
                                      OR    "840000578"
                   MOVE  SPACE  TO    RECE84-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "113006270"
                   MOVE   "1"   TO    RECE84-FLG-TAIIN-COMMENT
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         ²ð¸î»Ù±çÏ¢·È»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     "113011710"
                        MOVE   "1"    TO    RECE84-FLG-KAISIEN-COMMENT
                END-IF
              END-IF
              IF   RECE82-SRYYM      >=    "201004"
              AND  RECE82-SRYYM      <=    "201803"
      *         ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     "113003410"
                        MOVE   "1"    TO    RECE84-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
      *       ¿Í¹©Æâ¼ªµ¡´ïÄ´À°²Ã»»
              IF   SRY-SRYCD(IDX2)   =  "113030670"
                   MOVE   "1"   TO    RECE84-FLG-JINNAI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)   =  "113030770"
                   MOVE   "2"   TO    RECE84-FLG-JINNAI-COMMENT
              END-IF
      *       ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡ÊÃåÍÑ·¿¼«Æ°½üºÙÆ°´ï¡Ë
      *       ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥í¡¦¿¢¹þ·¿½üºÙÆ°´ï°Ü¹Ô´ü²Ã»»´Þ¤à¡Ë
      *       ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ£²¡¦£³
              IF   RECE82-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2) =  "113015010"           )
                AND (RECE82-SRYYM   <=  "201803"              )
                   MOVE   "1"   TO    RECE84-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015130"
                   MOVE   "2"   TO    RECE84-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015310"
                   MOVE   "1"   TO    RECE84-FLG-GANSIDO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015410"
                   MOVE   "2"   TO    RECE84-FLG-GANSIDO-COMMENT
                END-IF
              END-IF
      *       ÇÓÇ¢¼«Î©»ØÆ³ÎÁ
              IF   RECE82-SRYYM    >=     "201604"
              AND  RECE82-SRYYM    <=     "202003"
                IF  (SRY-SRYCD(IDX2)  =     "113022810"        )
                AND (FLG-HAIJRT-COM   =     ZERO               )
                        MOVE   "1"    TO    RECE84-FLG-HAIJRT-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "840000097"
                                      OR    "840000086"
                        MOVE  SPACE   TO    RECE84-FLG-HAIJRT-COMMENT
                        MOVE    1     TO    FLG-HAIJRT-COM
                END-IF
              END-IF
      *       Âà±¡»þ¶¦Æ±»ØÆ³ÎÁ£²
              IF   RECE82-SRYYM    >=     "201804"
                IF  (SRY-SRYCD(IDX2)  =     "113008910"        )
                AND (FLG-TAIKYO-COM   =     ZERO               )
                        MOVE   "1"    TO    RECE84-FLG-TAIKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "840000110"
                        MOVE  SPACE   TO    RECE84-FLG-TAIKYO-COMMENT
                        MOVE    1     TO    FLG-TAIKYO-COM
                END-IF
              END-IF
           END-IF
      *
           IF    ((RECE82-HKNKBN    NOT  =  "4" AND "6") AND
                  (RECE84-SRYKBN         =  "14"       ))
           OR    ((RECE82-HKNKBN         =  "4"  OR "6") AND
                  (RECE84-SRYKBN         =  "80"       ))
              IF   SRY-SRYCD(IDX2)    =     "114008010"
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000073"
                   MOVE  SPACE  TO    RECE84-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "114009410"
                   MOVE   "1"   TO    RECE84-FLG-HINKAI-COMMENT
              END-IF
      *
      ***     IF  (SRY-SRYCD(IDX2)    =     "114000110"
      ***                             OR    "114001610"       )
      ***     AND (FLG-OSIN-COM       =     ZERO              )
      ***          MOVE   "1"   TO    RECE84-FLG-OSIN-COMMENT
      ***     END-IF
      ***     IF   SRY-SRYCD(IDX2)    =     "840000039"
      ***          MOVE  SPACE  TO    RECE84-FLG-OSIN-COMMENT
      ***          MOVE    1    TO    FLG-OSIN-COM
      ***     END-IF
      *
              IF   RECE82-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114000110"
                                    OR  "114001610"
                   IF    (WRK-SISETU3168  =  1)
                   OR    (WRK-SISETU3055  =  1)
                   OR    (WRK-SISETU3056  =  1)
                       MOVE   "1"   TO    RECE82-COMCNTFLG
                       MOVE   "1"   TO    RECE84-FLG-ZAISIEN-COMMENT
                   END-IF
                   IF  RECE84-FLG-ZAISIEN-COMMENT = SPACE
                     IF  (WRK-SISETU3169  =  1)
                     OR  (WRK-SISETU3057  =  1)
                     OR  (WRK-SISETU3058  =  1)
                       MOVE   "1"   TO    RECE82-COMCNTFLG
                       MOVE   "2"   TO    RECE84-FLG-ZAISIEN-COMMENT
                     END-IF
                   END-IF
                END-IF
              ELSE
                IF   WRK-SISETU613   =  1
                  IF   RECE82-SRYYM   >=  "200604"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE82-COMCNTFLG
                        MOVE   "1"   TO    RECE84-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                ELSE
                  IF   WRK-SISETU695   =  1
                    IF   RECE82-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE82-COMCNTFLG
                        MOVE   "2"   TO    RECE84-FLG-ZAISIEN-COMMENT
                      END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114020910"
                   MOVE   "1"   TO    RECE84-FLG-KAISYOKU-COMMENT
                END-IF
      ***       IF   SRY-SRYCD(IDX2) =  "114009710"
      ***          MOVE   "1"   TO    RECE84-FLG-YOUATU-COMMENT
      ***       END-IF
              END-IF
      *       ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ
              IF   RECE82-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "114027610"
                                        OR    "114027710"
                                        OR    "114027810"
                                        OR    "114027910"         )
                AND (FLG-ZAIKYO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-ZAIKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000097"
                                        OR    "840000086"
                   MOVE  SPACE  TO    RECE84-FLG-ZAIKYO-COMMENT
                   MOVE    1    TO    FLG-ZAIKYO-COM
                END-IF
              END-IF
      *       ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡ÊºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë¡Ë
      *       ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡ÊºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²¡Ë
              IF   SRY-SRYCD(IDX2)    =     "114045470"
                   MOVE   "A"   TO    RECE84-FLG-SINPESU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114045670"
                   MOVE   "B"   TO    RECE84-FLG-SINPESU-COMMENT
              END-IF
      *       ºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë
      *       ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²
              IF   SRY-SRYCD(IDX2)    =     "114003710"
                   MOVE   "A"   TO    RECE84-FLG-SINPESUR-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114040810"
                   MOVE   "B"   TO    RECE84-FLG-SINPESUR-COMMENT
              END-IF
      *       ºßÂðÈ¾¸Ç·Á±ÉÍÜ·Ð´É±ÉÍÜË¡»ØÆ³´ÉÍýÎÁ
              IF  (SRY-SRYCD(IDX2)    =     "114045510"          )
              AND (FLG-ZAIHANKO-COM   =     ZERO                 )
                   MOVE   "9"    TO   RECE84-FLG-ZAIHANKO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000097"
                   MOVE  SPACE   TO   RECE84-FLG-ZAIHANKO-COMMENT
                   MOVE    1     TO   FLG-ZAIHANKO-COM
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "50"
              IF   RECE84-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000042"
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE82-SRYYM      >=    "201004"
            IF     RECE84-SRYKBN      =     "54"
              IF   RECE84-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "70"
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170011910" OR "170012110"
                                      OR "170015550" OR "170015210"
                                      OR "170020110" OR "170022110"
                                      OR "170022290" OR "170022390"
                                      OR "170022490" OR "170023010"
                                      OR "170023110" OR "170023210"
                                      OR "170023310" OR "170023410"
                                      OR "170023510"               )
              AND (RECE82-SRYYM       <     "201004"               )
                   MOVE   "1"   TO    RECE84-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170012110" OR "170015210"
                                      OR "170020110" OR "170028610"
                                      OR "170033410" OR "170033510"
                                      OR "170034910" OR "170035010")
              AND (RECE82-SRYYM       >=    "201004"               )
              AND (RECE82-SRYYM       <=    "202003"               )
                   MOVE   "1"   TO    RECE84-FLG-CTMRI-COMMENT
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
                   MOVE   "1"   TO    RECE84-FLG-FILM-COMMENT
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM  >= "201810"
                IF  (SRY-SRYCD(IDX2)  =     "170016010"        )
                AND (FLG-KINGAZ-COM   =     ZERO               )
                     MOVE   "1"    TO   RECE84-FLG-KINGAZ-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "820100129"
                     MOVE  SPACE   TO   RECE84-FLG-KINGAZ-COMMENT
                     MOVE    1     TO   FLG-KINGAZ-COM
                END-IF
              END-IF
           END-IF
      *
           IF    ((RECE82-HKNKBN    NOT  =  "4" AND "6" ) AND
                  (RECE84-SRYKBN         =  "80"        ))
           OR    ((RECE82-HKNKBN         =  "4"  OR "6" ) AND
                  (RECE84-SRYKBN         =  "40" OR "80"))
      ***     IF   SRY-SRYCD(IDX2)    =     "180018310"
      ***     AND  FLG-HOUSEI-COM     =     ZERO
      ***          MOVE   "1"   TO    RECE84-FLG-HOUSEI-COMMENT
      ***     END-IF
      ***     IF   SRY-SRYCD(IDX2)    =     "840000079"
      ***          MOVE  SPACE  TO    RECE84-FLG-HOUSEI-COMMENT
      ***          MOVE    1    TO    FLG-HOUSEI-COM
      ***     END-IF
      *
              IF   RECE82-SRYYM      >=    "201204"
                IF   SRY-SRYCD(IDX2)  =  "180039030" OR "180036030"
                                      OR "180039130" OR "180039230"
                                      OR "180039330" OR "180036130"
                                      OR "180039430" OR "180039530"
                                      OR "180048930" OR "180049030"
                   MOVE   "1"   TO    RECE84-FLG-SEIKEA-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "90"  OR  "92"
              IF   SRY-SRYCD(IDX2)    =     "190107070"
                   MOVE   "1"   TO    RECE84-FLG-KAKURI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "101910050"
                   MOVE   "1"   TO    RECE84-FLG-CHIRYO-COMMENT
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         ±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»
                IF   SRY-SRYCD(IDX2)  =     "190136810"
                        MOVE   "1"    TO    RECE84-FLG-EISAPO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190147110"
                        MOVE   "2"    TO    RECE84-FLG-EISAPO-COMMENT
                END-IF
      *         ¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»
                IF   SRY-SRYCD(IDX2)  =     "190137910"
                        MOVE   "1"    TO    RECE84-FLG-KOKKEA-COMMENT
                END-IF
      *         ½Å¾É¼Ô²Ã»»¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë
                IF   SRY-SRYCD(IDX2)  =     "190141370"
                        MOVE   "1"    TO    RECE84-FLG-JYUSYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190151470"
                        MOVE   "2"    TO    RECE84-FLG-JYUSYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190195770"
                        MOVE   "3"    TO    RECE84-FLG-JYUSYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190195870"
                        MOVE   "4"    TO    RECE84-FLG-JYUSYO-COMMENT
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201204"
      *         Àº¿À²Ê¥ê¥¨¥¾¥ó¥Á¡¼¥à²Ã»»
                IF   SRY-SRYCD(IDX2)  =     "190147010"
                        MOVE   "1"    TO    RECE84-FLG-SEIRIE-COMMENT
                END-IF
      *         ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»
                IF   SRY-SRYCD(IDX2)  =     "190149110"
                        MOVE   "1"    TO    RECE84-FLG-BYOYAK-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190192110"
                        MOVE   "2"    TO    RECE84-FLG-BYOYAK-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "190192210"
                        MOVE   "3"    TO    RECE84-FLG-BYOYAK-COMMENT
                END-IF
              END-IF
      *       Ä¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡¦½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»
              IF   RECE82-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2) =  "190076570"
                                    OR  "190076670"
                                    OR  "190127510"
                                    OR  "190127610"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                  MOVE     ZERO       TO    FLG-CYOJKSN-OK
                  IF   RECE82-SRYYM  =  RECE82-NYUINYMD(1:6)
                    MOVE  RECE82-NYUINYMD(7:2)  TO  IDX25
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
                   MOVE   "1"   TO    RECE84-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE82-COMCNTFLG
                   EVALUATE  RECE82-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "X"    TO   RECE82-CYOJKSNFLG
                     WHEN     "Y"
                        MOVE   "Z"    TO   RECE82-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "5"   TO    RECE84-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2) =  "190076670"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                 IF  FLG-CYOJKSN-OK  =   1
                   MOVE   "2"   TO    RECE84-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE82-COMCNTFLG
                   EVALUATE  RECE82-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "Y"    TO   RECE82-CYOJKSNFLG
                     WHEN     "X"
                        MOVE   "Z"    TO   RECE82-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "6"   TO    RECE84-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2) =  "190127510"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                 IF  FLG-CYOJKSN-OK  =   1
                   MOVE   "3"   TO    RECE84-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE82-COMCNTFLG
                   EVALUATE  RECE82-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "X"    TO   RECE82-CYOJKSNFLG
                     WHEN     "Y"
                        MOVE   "Z"    TO   RECE82-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "7"   TO    RECE84-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF  (SRY-SRYCD(IDX2) =  "190127610"          )
                AND (FLG-CYOJKSN-COM =  ZERO                 )
                 IF  FLG-CYOJKSN-OK  =   1
                   MOVE   "4"   TO    RECE84-FLG-CYOJKSN-COMMENT
                   MOVE   "1"   TO    RECE82-COMCNTFLG
                   EVALUATE  RECE82-CYOJKSNFLG
                     WHEN     SPACE
                        MOVE   "Y"    TO   RECE82-CYOJKSNFLG
                     WHEN     "X"
                        MOVE   "Z"    TO   RECE82-CYOJKSNFLG
                   END-EVALUATE
                 ELSE
                   MOVE   "8"   TO    RECE84-FLG-CYOJKSN-COMMENT
                 END-IF
                END-IF
                IF   SRY-SRYCD(IDX2) =  "840000056"
                                     OR "840000556"
                   MOVE    1    TO    FLG-CYOJKSN-COM
                   IF  RECE84-FLG-CYOJKSN-COMMENT =
                                           "1" OR "2" OR "3" OR "4"
                        MOVE   "1"    TO   RECE85-RECEPTFLG(IDX4)
                                           RECE85-RECEDENFLG(IDX4)
                   END-IF
                   IF  RECE84-FLG-CYOJKSN-COMMENT =
                                           "5" OR "6" OR "7" OR "8"
                        MOVE   "1"    TO   RECE85-RECEDENFLG(IDX4)
                   END-IF
                END-IF
              END-IF
      *       ÇÓÇ¢¼«Î©»Ù±ç²Ã»»
              IF   RECE82-SRYYM      >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =     "190219010"        )
                AND (FLG-HAIJSK-COM   =     ZERO               )
                  MOVE   "1"    TO    RECE84-FLG-HAIJSK-COMMENT
                  PERFORM  VARYING  IDX40  FROM  1  BY  1
                            UNTIL   IDX40    >  31
                    IF  ACCT-DAY(1 IDX40) NOT = ZERO
                      IF  RECE82-HAIJSKYMD  =  SPACE
                        MOVE  RECE82-SRYYM  TO  RECE82-HAIJSKYMD(1:6)
                        MOVE  IDX40         TO  RECE82-HAIJSKYMD(7:2)
                      ELSE
                        MOVE  RECE82-HAIJSKYMD(7:2)  TO  IDX41
                        IF    IDX40  <  IDX41
                          MOVE  IDX40       TO  RECE82-HAIJSKYMD(7:2)
                        END-IF
                      END-IF
                      MOVE    31    TO   IDX40
                    END-IF
                  END-PERFORM
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "840000097"
                                      OR    "840000086"
                  MOVE  SPACE   TO    RECE84-FLG-HAIJSK-COMMENT
                  MOVE    1     TO    FLG-HAIJSK-COM
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     "160162350"
                                      OR    "160181250"
                                      OR    "160116310"        )
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE84-FLG-KENSA-COMMENT
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160162350"
                   MOVE    1    TO    RECE82-KENSA-BNPFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160181250"
                   MOVE    1    TO    RECE82-KENSA-NTPROFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160116310"
                   MOVE    1    TO    RECE82-KENSA-ANPFLG
              END-IF
      *
              IF   RECE82-SRYYM  >= "201810"
                IF  (SRY-SRYCD(IDX2)  =     "160000210"
                                      OR    "160177770"         )
                AND (FLG-KENKSN-COM   =     ZERO                )
                     MOVE   "1"    TO   RECE84-FLG-KENKSN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "820100129"
                     MOVE  SPACE   TO   RECE84-FLG-KENKSN-COMMENT
                     MOVE    1     TO   FLG-KENKSN-COM
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN   =  "14" OR "40" OR "50" OR "54"
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
                   MOVE   "1"   TO    RECE84-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       220211-RECE85-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       220212-JIBAI-KIZAI-TENHEN-SEC     SECTION.
      *
           MOVE     ZERO                   TO    WRK-TEN-KZI
           MOVE     ZERO                   TO    FLG-TAIGAI
      *
           IF   RECE82-TAXKBN-KZI   =   "2"
      *
      *       ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£³¡Ë¤Ë¤Æ¾ÃÈñÀÇÂÐ¾Ý³°¥³¡¼¥É¤«È½ÊÌ
              INITIALIZE                         SYSKANRI-REC
              MOVE    "4003"               TO    SYS-KANRICD
              MOVE    "*"                  TO    SYS-KBNCD
              MOVE    RECE82-SRYYM         TO    SYS-STYUKYMD (1:6)
              MOVE    "01"                 TO    SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD         TO    SYS-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM          TO    SYS-HOSPNUM
              MOVE    SYSKANRI-REC         TO    MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE      MCPDATA-REC      TO    SYS-4003-REC
                PERFORM   VARYING   IDX10    FROM   1   BY   1
                           UNTIL   (IDX10                 >    50 )
                            OR     (SYS-4003-SRYCD(IDX10) =  SPACE)
                            OR     (FLG-TAIGAI            =     1 )
                  IF   SYS-4003-SRYCD(IDX10)  =  SRY-SRYCD(IDX2)
                      MOVE     1           TO    FLG-TAIGAI
                  END-IF
                END-PERFORM
              END-IF
              IF      TNS-KANANAMECHGKBN  =  ZERO OR 5
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
       220212-JIBAI-KIZAI-TENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       220213-RECE85-TANKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    ¿ô»úÉ½¼¨ÍÑÊÔ½¸¡ÊÈ¾³Ñ¡Ë
           MOVE    RECE85-TEN(IDX4)
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
       220213-RECE85-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕÊ¸½ñÎÁÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       220214-JIBAI-BUNSYO-TENHEN-SEC     SECTION.
      *
           MOVE        ZERO          TO    WRK-TEN-BUN
      *
           IF   RECE82-TAXKBN   =   "2"
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
       220214-JIBAI-BUNSYO-TENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÄÎÌÇÑ´þ¤Î¿ôÎÌÊÔ½¸½èÍý
      *****************************************************************
       220215-RECE85-SURYO-HEN-SEC     SECTION.
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
      ***  MOVE    RECE85-SRYSURYO(IDX4)  TO  WRK-SURYO
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
       220215-RECE85-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       220216-SETCOM-HENSYU-SEC     SECTION.
      *
           IF   RECE85-MEISKYFLG(IDX4)  =  "2"
              IF   (FLG-SETCOM              =  1   )
              AND  (RECE85-RECEDENFLG(IDX4) = SPACE)
                 MOVE  "1"            TO  RECE84-SETCOMFLG
              ELSE
                 MOVE  SPACE          TO  RECE85-MEISKYFLG(IDX4)
              END-IF
           ELSE
              IF   (RECE85-SRYCD(IDX4)(1:1) = "1" OR "6" OR "7")
              OR   (RECE85-SRYCD(IDX4)(1:3) = "059"            )
                IF  RECE85-SRYCD(IDX4)(1:2) = "63"
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
           IF    RECE85-SRYCD(IDX4)   = "113000410" OR "113000510" OR
                                        "180000110" OR "180000210" OR
                                        "113023910"
                 MOVE   1             TO  WRK-SETCOM-JIDO
           END-IF
      *    °­À­¼ðáçÆÃ°ÛÊª¼Á¼£ÎÅ´ÉÍýÎÁ
           IF    RECE85-SRYCD(IDX4)   = "113001210" OR "113001310" OR
                                        "113002110"
                 MOVE   3             TO  WRK-SETCOM-JIDO
           END-IF
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           IF    RECE85-SRYCD(IDX4)   = "111011810"
                                     OR "111012610"
                                     OR "111012810"
                 MOVE   4             TO  WRK-SETCOM-JIDO
           END-IF
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ
           IF    RECE85-SRYCD(IDX4)   = "180000710"
                 MOVE   6             TO  WRK-SETCOM-JIDO
           END-IF
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ
           IF    RECE85-SRYCD(IDX4)   = "113004310"
                 MOVE   7             TO  WRK-SETCOM-JIDO
           END-IF
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ
           IF    RECE85-SRYCD(IDX4)   = "114008010"
                 MOVE   11            TO  WRK-SETCOM-JIDO
           END-IF
      *    ºßÂðÈ¾¸Ç·Á±ÉÍÜ·Ð´É±ÉÍÜË¡»ØÆ³´ÉÍýÎÁ
           IF    RECE85-SRYCD(IDX4)   = "114045510"
                 MOVE   19            TO  WRK-SETCOM-JIDO
           END-IF
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ
           IF    RECE85-SRYCD(IDX4)   = "114027610"
                                     OR "114027710"
                                     OR "114027810"
                                     OR "114027910"
                 MOVE   29            TO  WRK-SETCOM-JIDO
           END-IF
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ
           IF    RECE85-SRYCD(IDX4)   = "140057510"
                                     OR "140057610"
                 MOVE   35            TO  WRK-SETCOM-JIDO
           END-IF
      *    »þ´Ö³°¶ÛµÞ±¡Æâ¸¡ºº²Ã»»
      *    ³°Íè¿×Â®¸¡ÂÎ¸¡ºº²Ã»»
           IF   (RECE85-SRYCD(IDX4)   = "160000210"
                                     OR "160177770"         )
           AND  (RECE82-SRYYM        >= "201810"            )
                 MOVE   43            TO  WRK-SETCOM-JIDO
           END-IF
      *    »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»
           IF   (RECE85-SRYCD(IDX4)   = "170016010"         )
           AND  (RECE82-SRYYM        >= "201810"            )
                 MOVE   44            TO  WRK-SETCOM-JIDO
           END-IF
      *    ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡
           IF   (RECE85-SRYCD(IDX4)   = "180058650")
           AND  (RECE82-SRYYM        >= "202004"   )
                 MOVE   47            TO  WRK-SETCOM-JIDO
           END-IF
      *    ÇÓÇ¢¼«Î©»ØÆ³ÎÁ
           IF    RECE85-SRYCD(IDX4)   = "113022810"
                 MOVE   100           TO  WRK-SETCOM-JIDO
           END-IF
      *    Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           IF    RECE85-SRYCD(IDX4)   = "190030310"
                 MOVE   103           TO  WRK-SETCOM-JIDO
           END-IF
      *    Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
      ***  IF    RECE85-SRYCD(IDX4)   = "180018310"
      ***        MOVE   105           TO  WRK-SETCOM-JIDO
      ***  END-IF
      *    Âà±¡»þ¶¦Æ±»ØÆ³ÎÁ£²
           IF    RECE85-SRYCD(IDX4)   = "113008910"
                 MOVE   108           TO  WRK-SETCOM-JIDO
           END-IF
      *    ¿´ÉÔÁ´¤ËÂÐ¤¹¤ë±óÀÖ³°Àþ²¹Ç®ÎÅË¡
           IF    RECE85-SRYCD(IDX4)   = "140061210"
                 MOVE   110           TO  WRK-SETCOM-JIDO
           END-IF
      *
      *    ÆÃÄêÌôºÞ¼£ÎÅ´ÉÍý²Ã»»¡ÊÂ¡´ï°Ü¿¢·î¤«¤é£³·î¡Ë
           IF    RECE85-SRYCD(IDX4)   = "113000670" OR "113000770"
                                     OR "113029170" OR "113029270"
                                     OR "113029370"
                 MOVE   1             TO  WRK-SETCOM-JIDO2
           END-IF
      *    ¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»
           IF    RECE85-SRYCD(IDX4)   = "180056570"
                 MOVE   2             TO  WRK-SETCOM-JIDO2
           END-IF
      *
      *    <Ï«ºÒÍÑ>
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           IF    RECE85-SRYCD(IDX4)   = "101110040"
                 MOVE   204           TO  WRK-SETCOM-JIDO
           END-IF
      *
           IF  ((WRK-SETCOM-JIDO    =  1                        ) AND
                (RECE85-SRYCD(IDX4) = "830000014" OR "840000038"
                                                  OR "840000538"))
           OR  ((WRK-SETCOM-JIDO    =  3                        ) AND
                (RECE85-SRYCD(IDX4) = "830000015"               ))
           OR  ((WRK-SETCOM-JIDO    =  4                        ) AND
                (RECE85-SRYCD(IDX4) = "830000024"               ))
           OR  ((WRK-SETCOM-JIDO    =  6                        ) AND
                (RECE85-SRYCD(IDX4) = "840000011"
                                   OR "840000012"
                                   OR "840000511"
                                   OR "840000512"               ))
           OR  ((WRK-SETCOM-JIDO    =  7                        ) AND
                (RECE85-SRYCD(IDX4) = "840000078"
                                   OR "840000578"               ))
           OR  ((WRK-SETCOM-JIDO    =  11                       ) AND
                (RECE85-SRYCD(IDX4) = "840000073"               ))
           OR  ((WRK-SETCOM-JIDO    =  19                       ) AND
                (RECE85-SRYCD(IDX4) = "840000097"               ))
           OR  ((WRK-SETCOM-JIDO    =  29                       ) AND
                (RECE85-SRYCD(IDX4) = "840000097"
                                   OR "840000086"               ))
           OR  ((WRK-SETCOM-JIDO    =  35                       ) AND
                (RECE85-SRYCD(IDX4) = "840000085"
                                   OR "840000086"
                                   OR "840000045"               ))
           OR  ((WRK-SETCOM-JIDO    =  43                       ) AND
                (RECE85-SRYCD(IDX4) = "820100129"
                                   OR "840000044"               ))
           OR  ((WRK-SETCOM-JIDO    =  44                       ) AND
                (RECE85-SRYCD(IDX4) = "820100129"
                                   OR "840000071"               ))
           OR  ((WRK-SETCOM-JIDO    =  47                       ) AND
                (RECE85-SRYCD(IDX4) = "840000076"               ))
           OR  ((WRK-SETCOM-JIDO    =  100                      ) AND
                (RECE85-SRYCD(IDX4) = "840000097"
                                   OR "840000086"               ))
           OR  ((WRK-SETCOM-JIDO    =  103                      ) AND
                (RECE85-SRYCD(IDX4) = "840000055"               ))
      ***  OR  ((WRK-SETCOM-JIDO    =  105                      ) AND
      ***       (RECE85-SRYCD(IDX4) = "840000079"               ))
           OR  ((WRK-SETCOM-JIDO    =  108                      ) AND
                (RECE85-SRYCD(IDX4) = "840000110"               ))
           OR  ((WRK-SETCOM-JIDO    =  110                      ) AND
                (RECE85-SRYCD(IDX4) = "840000076"               ))
      *
           OR  ((WRK-SETCOM-JIDO2   =  1                        ) AND
                (RECE85-SRYCD(IDX4) = "840000077"               ))
           OR  ((WRK-SETCOM-JIDO2   =  2                        ) AND
                (RECE85-SRYCD(IDX4) = "840000076"               ))
      *
      *    <Ï«ºÒÍÑ>
           OR  ((WRK-SETCOM-JIDO    =  204                      ) AND
                (RECE85-SRYCD(IDX4) = "830000024"               ))
      *
                 MOVE  "1"              TO  RECE84-SETCOMFLG
                 MOVE  "2"              TO  RECE85-MEISKYFLG(IDX4)
                 MOVE  WRK-SETCOM-JIDO  TO  RECE85-SETCOMATFLG(IDX4)
                 IF  (WRK-SETCOM-JIDO2   =  1         )
                 AND (RECE85-SRYCD(IDX4) = "840000077")
                   MOVE    2            TO  RECE85-SETCOMATFLG(IDX4)
                 END-IF
                 IF  (WRK-SETCOM-JIDO2   =  2                 )
                 AND (RECE85-SRYCD(IDX4) = "840000076"        )
                   MOVE    40           TO  RECE85-SETCOMATFLG(IDX4)
                 END-IF
      *
           END-IF
      *
           .
       220216-SETCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©¥³¡¼¥ÉÉ½ÍÑÈÖ¹æ¡Ý¶èÊ¬ÈÖ¹æÊÔ½¸
      *****************************************************************
       220217-TENTEKI-CDKBN-HEN-SEC     SECTION.
      *
      *    É½ÍÑ¡Ý¶èÊ¬ÈÖ¹æ
      *    £´:ÅÀÅ©
      *    £µ¡§Ãæ¿´ÀÅÌ®
      *    £¶¡§Ãæ¿´ÀÅÌ®±ÉÍÜ
      *    £·°Ê¾å¡¡¤½¤ÎÂ¾Ãí¼Í
           IF     RECE85-CDKBN-KBNNUM(IDX4)   NOT =    ZERO
              IF     RECE85-CDKBN-KBNNUM(IDX4)  <=    6
                EVALUATE    RECE85-CDKBN-KBNNUM(IDX4)
                  WHEN     5
                    MOVE   6     TO   RECE84-CDKBN-KBNNUM
                  WHEN     6
                    MOVE   8     TO   RECE84-CDKBN-KBNNUM
                  WHEN  OTHER
                    MOVE   RECE85-CDKBN-KBNNUM(IDX4)
                                 TO   RECE84-CDKBN-KBNNUM
                END-EVALUATE
              ELSE
                MOVE   RECE85-CDKBN-KBNNUM(IDX4)
                                 TO   RECE84-CDKBN-KBNNUM
                ADD    10        TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       ÅÀÅ©Ãí¼Í¡Ê·ìÞùÀ®Ê¬À½ºÞÊ¸½ñ¡Ë²Ã»»
              IF     RECE85-SRYCD(IDX4)    =   "130010570"
                    MOVE   5     TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       Ãæ¿´ÀÅÌ®Ãí¼Í¡Ê·ìÞùÀ®Ê¬À½ºÞÊ¸½ñ¡Ë²Ã»»
              IF     RECE85-SRYCD(IDX4)    =   "130010670"
                    MOVE   7     TO   RECE84-CDKBN-KBNNUM
              END-IF
           ELSE
      *       ÌôºÞ
              IF     RECE84-YKZTEN     NOT =   ZERO
                    MOVE   9     TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       ´ïºà
              IF     RECE84-KIZAITEN   NOT =   ZERO
                    MOVE   10    TO   RECE84-CDKBN-KBNNUM
              END-IF
      *       ¥³¥á¥ó¥È
              IF    (RECE85-SRYCD(IDX4)(1:1)    =   "8"  )
              OR    (RECE85-SRYCD(IDX4)(1:3)    =   "008")
                    MOVE   999   TO   RECE84-CDKBN-KBNNUM
              END-IF
           END-IF
      *
           .
       220217-TENTEKI-CDKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ú¥ì¥»¥×¥È¡Û¡ÊÂè»°¼Ô¹Ô°Ù¡Ë°ìÉôÉéÃ´¶â¶â³ÛÃÖ´¹¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       220218-ICHIBUCOM-HENSYU-SEC     SECTION.
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
             PERFORM 900-PTCOM-READ-SEC
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
                   MOVE    1    TO   RECE82-DAISAN-CHIKAN
                   MOVE  WRK-INPUTCOMENT-KIN
                                TO   RECE82-FTNMONEY
                 END-IF
               END-IF
             END-IF
      *
           END-IF
      *
           .
       220218-ICHIBUCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ú¥ì¥»¥×¥È¡Û¡Ê¼«ÇåÀÕ¡Ë¼õÎÎ¶â³ÛÍóÃÖ´¹¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       220219-ICHIBUCOM-HENSYU2-SEC     SECTION.
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
             PERFORM 900-PTCOM-READ-SEC
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
                   MOVE    1    TO   RECE82-JIBAI-CHIKANKINFLG
                   MOVE  WRK-INPUTCOMENT-KIN
                                TO   RECE82-JIBAI-CHIKANKIN
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
                 MOVE    1      TO   RECE82-JIBAI-CHIKANNAMEFLG
                 MOVE  WRK-INPUTCOMENT3
                                TO   RECE82-JIBAI-CHIKANNAME
               END-IF
             END-IF
      *
           END-IF
      *
           .
       220219-ICHIBUCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌÀºÙ¿ôÅù¤ÎÊÔ½¸
      *****************************************************************
       22021A-MEISAISU-HENSYU-SEC                  SECTION.
      *
           COMPUTE   RECE84-MEISAISU    =  RECE84-MEISAISU
                                        -  1
           MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
           COMPUTE   RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                        -  WRK-SRYCD
           COMPUTE   RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                        -  SRY-SRYSURYO(IDX2)
      *
           .
       22021A-MEISAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï³«»ÏÆüÊÔ½¸
      *****************************************************************
       22021B-RIHAYMD-HENSYU-SEC                 SECTION.
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
             PERFORM 900-PTCOM-READ-SEC
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
               MOVE       1     TO  RECE82-RIHA2-SINDAI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-SINYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE82-RIHA2-SINYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-SINYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-SINYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         2
               MOVE       1     TO  RECE82-RIHA2-NOUKE
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-NOUYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE82-RIHA2-NOUYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-NOUYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-NOUYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         3
               MOVE       1     TO  RECE82-RIHA2-UNDOKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-UNDYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE82-RIHA2-UNDYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-UNDYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-UNDYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         4
               MOVE       1     TO  RECE82-RIHA2-KOKYUKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-KOKYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE82-RIHA2-KOKYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-KOKYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-KOKYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         5
               MOVE       1     TO  RECE82-RIHA2-SESSYOKU
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-SESYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE82-RIHA2-SESYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-SESYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-SESYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         6
               MOVE       1     TO  RECE82-RIHA2-HAIYO
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-HAIYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE82-RIHA2-HAIYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-HAIYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-HAIYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         7
               MOVE       1     TO  RECE82-RIHA2-SINDAI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-SIN9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE82-RIHA2-SIN9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-SIN9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-SIN9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         8
               MOVE       1     TO  RECE82-RIHA2-KOKYUKI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-KOK9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE82-RIHA2-KOK9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-KOK9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-KOK9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         9
               MOVE       1     TO  RECE82-RIHA2-HAIYO9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE82-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE82-RIHA2-HAI9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE82-RIHA2-HAI9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE82-RIHA2-HAI9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE82-RIHA2-HAI9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
           END-EVALUATE
      *
           .
       22021B-RIHAYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       22022-RECE82-HENSYU-SEC               SECTION.
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
                       IF      SRY-SRYKBN   =  "31" OR "32" OR "33"
                                MOVE   ACCT-YKZTEN    TO   WK-YKZTEN2
                       END-IF
                       IF   RECE82-HKNKBN    NOT  =  "4" AND "6"
                         IF    SRY-SRYKBN   =  "31" OR "32" OR "33" OR
                                               "40" OR "50" OR "54" OR
                                               "60" OR "70" OR "80" OR
                                               "64"
                                MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                COMPUTE   WK-YKZTEN   =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                         END-IF
                       ELSE
                         IF    SRY-SRYKBN   =  "31" OR "32" OR "33" OR
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
                                 IF ((RECE82-HKNKBN     =  "4") AND
                                     (RECE82-JIBAIKBN   =  "2"))
                                 OR ( RECE82-HKNKBN     =  "6" )
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-RYOYOTEATE
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-RYOYOTEATE
                                 END-IF
                                END-IF
                         END-IF
                       END-IF
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                           IF   RECE82-HKNKBN   =  "4" OR "6"
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
                           MOVE  ZERO          TO   FLG-OSINKBN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
                     MOVE    ZERO     TO    WK-YKZTEN2
                     MOVE    ZERO     TO    FLG-OSINKBN
               END-IF
               IF  ((ACCT-ZAITEN    NOT =    ZERO) AND
                    (ACCT-ZAIKAISU  NOT =    ZERO))
               OR  ( FLG-TAISYO         =     1   )
                  EVALUATE   RECE82-HKNKBN
                    WHEN       "1"
                    WHEN       "2"
                        PERFORM   220221-RECE-LEFT-SET-SEC
                    WHEN       "4"
                        EVALUATE  WRK-PARA-RECEKBN
                          WHEN      "4"
      ********************* ¡Ê¿·ÍÍ¼°¡Ë
                            PERFORM   220222-RECE-LEFT-SET2-SEC
                          WHEN      "5"
      ********************* ¡Ê½¾ÍèÍÍ¼°¡Ë
                            PERFORM   220223-RECE-LEFT-SET3-SEC
                        END-EVALUATE
                    WHEN       "5"
                        PERFORM   220224-RECE-LEFT-SET4-SEC
                    WHEN       "6"
                        PERFORM   220223-RECE-LEFT-SET3-SEC
                  END-EVALUATE
               END-IF
               IF  ( SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "54"   OR
                                      "60"   OR   "70"   OR   "14"   OR
                                      "64"   OR   "12")
               OR  ((ACCT-ZAIKBN  =    2  ) AND
                    (SRY-SRYKBN   =   "13")           )
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           .                                           
      *
       22022-RECE82-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220221-RECE-LEFT-SET-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF  (SRY-SRYCD(IDX3) = "101110020" )
                   AND (RECE82-SRYYM   <=   "201306"  )
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "12"
                   COMPUTE        RECE82-NYUSAI-TENSU  =
                                  RECE82-NYUSAI-TENSU  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   EVALUATE  WRK-NYUSAI-SRYKBN
                     WHEN      "90"
                       COMPUTE    RECE82-NYUKHNKEI2    =
                                  RECE82-NYUKHNKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                     WHEN      "92"
                       COMPUTE    RECE82-NYUTOKKEI2    =
                                  RECE82-NYUTOKKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-EVALUATE
                   COMPUTE        RECE82-NYUTOTALTEN   =
                                  RECE82-NYUTOTALTEN   +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF ((SRY-SRYCD(IDX3) =
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"                         ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   "101130010"     ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-KINGAKU2(2)  =
                                    RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE8G-TENSU(2)  =
                                    WRK-RECE8G-TENSU(2)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE8G-TENSU(3)  =
                              WRK-RECE8G-TENSU(3)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   COMPUTE    RECE82-TENSU(3)  =
                              RECE82-TENSU(3)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    "630010002"
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    "099209901"
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU(4)  =
                                      RECE82-TENSU(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU(5)  =
                             RECE82-TENSU(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU(6)  =
                             RECE82-TENSU(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(10)
                   COMPUTE   RECE82-TENSU(10)  =
                             RECE82-TENSU(10)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(10)  =
                             WRK-RECE8C-TENSU(10)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)  =
                             RECE82-TENSU(11)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(11)  =
                             WRK-RECE8C-TENSU(11)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE8G-TENSU(12)  =
                             WRK-RECE8G-TENSU(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(12)  =
                             WRK-RECE8C-TENSU(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                   END-IF
                   COMPUTE   RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)  =
                             RECE82-TENSU(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(17)
                   END-IF
                   COMPUTE   RECE82-TENSU(17)  =
                             RECE82-TENSU(17)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(17)  =
                                RECE82-KAISU(17)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   "160101210"     OR
                                              "160095710"     OR
                                              "160095810"
                         IF   RECE82-KAISU(17)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(17)  =
                                        RECE82-KAISU(17)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(19)
                   END-IF
                   COMPUTE   RECE82-TENSU(19)  =
                             RECE82-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                 IF ((SRY-SRYCD(IDX3) =
                      "101800880"   OR
                      "101800870"   OR  "101800470"   OR
                      "101800480"   OR  "101800490"   OR
                      "101800500"   OR  "101800510"   OR
                      "101800520"   OR  "101800530"   OR
                      "101800540"   OR  "101800960"   OR
                      "101800970"   OR  "101800980"   OR
                      "101801040"                         ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                           COMPUTE RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN  *   ACCT-ZAIKAISU)
                 ELSE
                   IF ((SRY-SRYCD(IDX3) =
                        "101910010"    OR  "101910020"    OR
                        "101910030"    OR  "101910040"    OR
                        "101910070"    OR  "101910080"    OR
                        "101910090"    OR  "101910100"   )  AND
                       (RECE82-SRYYM   <=   "201306"     ))
                   OR ((SRY-SRYCD(IDX3) =
                        "101800390"    OR  "101800400"    OR
                        "101800410"    OR  "101800420"    OR
                        "101800430"    OR  "101800440"    OR
                        "101800450"    OR  "101800460"   )  AND
                       (RECE82-SRYYM   >=   "201307"     ))
                   OR ((SRY-SRYCD(IDX3) =
                        "101801150"    OR  "101801160"    OR
                        "101801170"    OR  "101801180"   )  AND
                       (RECE82-SRYYM   >=   "201307"     ))
                   OR ((ACCT-SRYKBN          =  "80"     )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                         IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                           COMPUTE RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                            *   ACCT-ZAIKAISU)
                           IF   FLG-TAISYO  =   1
                            COMPUTE  WRK-ZAITENNASHI  =
                                     WRK-ZAITENNASHI  +
                                    (WRK-JIBAIKZI-TNS(IDX3)
                                            *   ACCT-ZAIKAISU)
                           END-IF
                         ELSE
                           COMPUTE RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN  *   ACCT-ZAIKAISU)
                         END-IF
                   ELSE
                     IF ((ACCT-SRYKBN          =  "80"   )  AND
                         (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                     OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                          <= "101910400" )  AND
                         (RECE82-SRYYM   <=  "201306"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                          <= "101800860" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                          <= "101801020" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101801190" AND
                                          <= "101801420" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990" OR
                              "101801030"                )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                       IF SRY-SRYCD(IDX3)(1:5) =  "09593"
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                         IF   FLG-TAISYO  =   1
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                         END-IF
                       ELSE
                        IF  SRY-SRYCD(IDX3)  = "101800550" OR
                                "101800560" OR "101800990" OR
                                "101801030"
                         COMPUTE   WRK-FUTANWARI  = TNS-TEN / 100
                         COMPUTE   WRK-KEISAN-KG2 =
                                  (WRK-TEN-KG2 * WRK-FUTANWARI)
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (WRK-KEISAN-KG2 * ACCT-ZAIKAISU)
                        ELSE
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE   WRK-TEN-KG2 = TNS-TEN
                        END-IF
                       END-IF
                     ELSE                
                       COMPUTE     RECE82-TENSU(21) =
                                   RECE82-TENSU(21) +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)  =
                                   RECE82-TENSU(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE   RECE82-TENSU(21) =
                                   RECE82-TENSU(21) -
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                       END-IF
                       COMPUTE       RECE82-TENSU(22)  =
                                     RECE82-TENSU(22)  +
                                    (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
                 END-IF
           END-EVALUATE
           .
      *
       220221-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220222-RECE-LEFT-SET2-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  "111000370"   OR
                                             "111011470"   OR
                                             "111011970"   OR
                                             "111012070"   OR
                                             "111012170"   OR
                                             "111012270"   OR
                                             "111011570"   OR
                                             "111011670"   OR
                                             "111011770"
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF  (SRY-SRYCD(IDX3) = "101110020" )
                   AND (RECE82-SRYYM   <=   "201306"  )
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "12"
                   COMPUTE        RECE82-NYUSAI-TENSU  =
                                  RECE82-NYUSAI-TENSU  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   EVALUATE  WRK-NYUSAI-SRYKBN
                     WHEN      "90"
                       COMPUTE    RECE82-NYUKHNKEI2    =
                                  RECE82-NYUKHNKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                     WHEN      "92"
                       COMPUTE    RECE82-NYUTOKKEI2    =
                                  RECE82-NYUTOKKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-EVALUATE
                   COMPUTE        RECE82-NYUTOTALTEN   =
                                  RECE82-NYUTOTALTEN   +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF ((SRY-SRYCD(IDX3) =
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"                         ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   "101130010"     ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                     IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                         COMPUTE    RECE82-KINGAKU3(2)  =
                                    RECE82-KINGAKU3(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-KINGAKU3(3)  =
                                    RECE82-KINGAKU3(3)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE8G-TENSU(2)  =
                                    WRK-RECE8G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE8G-TENSU(21) =
                              WRK-RECE8G-TENSU(21) +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   COMPUTE    RECE82-TENSU(21)  =
                              RECE82-TENSU(21)  +
                             (WK-SYUTEN     *   WK-ZAIKAISU)
                   COMPUTE    RECE82-TENSU-YAK(22)  =
                              RECE82-TENSU-YAK(22)  +
                             (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                             (WK-YKZTEN     *   WK-ZAIKAISU)
                 END-IF
                ELSE
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE8G-TENSU(22) =
                              WRK-RECE8G-TENSU(22) +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    RECE82-TENSU(22)  =
                                  RECE82-TENSU(22)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU-YAK(22)  =
                                  RECE82-TENSU-YAK(22)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    RECE82-TENSU(22)  =
                                  RECE82-TENSU(22)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU-YAK(23)  =
                                  RECE82-TENSU-YAK(23)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 END-IF
                END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    "630010002"
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    "099209901"
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU-YAK(4)  =
                                      RECE82-TENSU-YAK(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU-YAK(5)  =
                             RECE82-TENSU-YAK(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU-YAK(6)  =
                             RECE82-TENSU-YAK(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF    RECE82-KHNTEN(7)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(7)
                   ELSE
                      IF   RECE82-KHNTEN(7) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(7)
                           MOVE   1       TO    FLG-KHNTEN7-JI
                      END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                    IF    RECE82-KHNTEN(8)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(8)
                    ELSE
                      IF   RECE82-KHNTEN(8) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(8)
                           MOVE   1       TO    FLG-KHNTEN8-JI
                      END-IF
                    END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF    RECE82-KHNTEN(9)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(9)
                   ELSE
                      IF   RECE82-KHNTEN(9) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(9)
                           MOVE   1       TO    FLG-KHNTEN9-JI
                      END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(9)
                   END-IF
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)  =
                             RECE82-TENSU(11)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(12)  =
                             RECE82-TENSU-YAK(12)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU-YAK(12)  =
                             WRK-RECE8C-TENSU-YAK(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU-YAK(13)  =
                             WRK-RECE8C-TENSU-YAK(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)  =
                             RECE82-TENSU(11)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(12)  =
                             RECE82-TENSU-YAK(12)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU-YAK(12)  =
                             WRK-RECE8C-TENSU-YAK(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU-YAK(13)  =
                             WRK-RECE8C-TENSU-YAK(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE8G-TENSU-YAK(12)  =
                             WRK-RECE8G-TENSU-YAK(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)  =
                             RECE82-TENSU(11)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(12)  =
                             RECE82-TENSU-YAK(12)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU-YAK(12)  =
                             WRK-RECE8C-TENSU-YAK(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
                ELSE
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE8G-TENSU-YAK(13)  =
                             WRK-RECE8G-TENSU-YAK(13)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU-YAK(13)  =
                             WRK-RECE8C-TENSU-YAK(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
                END-IF
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                   END-IF
                   COMPUTE   RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(14)  =
                             RECE82-TENSU-YAK(14)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(14)
                   END-IF
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(15)  =
                             RECE82-TENSU-YAK(15)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)  =
                             RECE82-TENSU(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(16)  =
                             RECE82-TENSU-YAK(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(16)
                   END-IF
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(17)  =
                             RECE82-TENSU-YAK(17)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(17)
                   END-IF
                   COMPUTE   RECE82-TENSU(17)  =
                             RECE82-TENSU(17)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(17)  =
                                RECE82-KAISU(17)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   "160101210"     OR
                                              "160095710"     OR
                                              "160095810"
                         IF   RECE82-KAISU(17)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(17)  =
                                        RECE82-KAISU(17)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU-YAK(18)  =
                             RECE82-TENSU-YAK(18)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(18)  =
                                RECE82-KAISU(18)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   "160101210"     OR
                                              "160095710"     OR
                                              "160095810"
                         IF   RECE82-KAISU(18)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(18)  =
                                        RECE82-KAISU(18)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU-YAK(19)  =
                             RECE82-TENSU-YAK(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(19)
                   END-IF
                   COMPUTE   RECE82-TENSU(19)  =
                             RECE82-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(20)  =
                             RECE82-TENSU-YAK(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 ELSE
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(20)
                   END-IF
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(21)  =
                             RECE82-TENSU-YAK(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                 IF ((SRY-SRYCD(IDX3) =
                      "101800880"   OR
                      "101800870"   OR  "101800470"   OR
                      "101800480"   OR  "101800490"   OR
                      "101800500"   OR  "101800510"   OR
                      "101800520"   OR  "101800530"   OR
                      "101800540"   OR  "101800960"   OR
                      "101800970"   OR  "101800980"   OR
                      "101801040"                         ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = "101800880"
                         COMPUTE RECE82-KINGAKU2(2)  =
                                 RECE82-KINGAKU2(2)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE RECE82-KINGAKU3(2)  =
                                 RECE82-KINGAKU3(2)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                  IF ((SRY-SRYCD(IDX3) =
                       "101910010"    OR  "101910020"    OR
                       "101910030"    OR  "101910040"    OR
                       "101910070"    OR  "101910080"    OR
                       "101910090"    OR  "101910100"   )  AND
                      (RECE82-SRYYM   <=   "201306"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       "101800390"    OR  "101800400"    OR
                       "101800410"    OR  "101800420"    OR
                       "101800430"    OR  "101800440"    OR
                       "101800450"    OR  "101800460"   )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       "101801150"    OR  "101801160"    OR
                       "101801170"    OR  "101801180"   )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                     IF ((SRY-SRYCD(IDX3) =
                          "101801150"    OR  "101801160"  OR
                          "101801170"    OR  "101801180"   )  AND
                         (RECE82-SRYYM   >=   "201307"     ))
                     OR ((ACCT-SRYKBN          =  "80"     )  AND
                         (SRY-SRYCD(IDX3)(1:7) =
                               "0959421" OR "0959422" OR
                               "0959423" OR "0959424"      )  AND
                         (RECE82-SRYYM   >=   "201604"     ))
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE RECE82-KINGAKU3(2)  =
                                 RECE82-KINGAKU3(2)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE RECE82-KINGAKU3(2)  =
                                 RECE82-KINGAKU3(2)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                       END-IF
                     ELSE
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE RECE82-KINGAKU3(1)  =
                                 RECE82-KINGAKU3(1)  +
                                (WRK-JIBAIKZI-TNS(IDX3)
                                           *   ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE RECE82-KINGAKU3(1)  =
                                 RECE82-KINGAKU3(1)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                       END-IF
                       MOVE    ZERO   TO   WRK-HEYA-NINZU
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   "101910010"
                              WHEN   "101910020"
                              WHEN   "101800390"
                              WHEN   "101800400"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "101910030"
                              WHEN   "101910040"
                              WHEN   "101800410"
                              WHEN   "101800420"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "101910070"
                              WHEN   "101910080"
                              WHEN   "101800430"
                              WHEN   "101800440"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "101910090"
                              WHEN   "101910100"
                              WHEN   "101800450"
                              WHEN   "101800460"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   RECE82-HEYA-NISSU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                     END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"   )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                    OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                         <= "101910400" )  AND
                        (RECE82-SRYYM   <=  "201306"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                         <= "101800860" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                         <= "101801020" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101801190" AND
                                         <= "101801420" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                             "101800560" OR "101800990" OR
                             "101801030"                )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                     IF  SRY-SRYCD(IDX3)(1:5) =  "09593"
                      EVALUATE  WRK-JIBAIKZI-SYUKEIKBN(IDX3)
                       WHEN       0
                       WHEN       5
                         COMPUTE RECE82-KINGAKU3(2)  =
                                 RECE82-KINGAKU3(2)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       1
                         COMPUTE RECE82-KINGAKU2(1)  =
                                 RECE82-KINGAKU2(1)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       4
                         COMPUTE RECE82-KINGAKU2(2)  =
                                 RECE82-KINGAKU2(2)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                      IF  SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990" OR
                              "101801030"
                         COMPUTE WRK-FUTANWARI  = TNS-TEN / 100
                         COMPUTE WRK-KEISAN-KG2 =
                             (WRK-TEN-KG2 * WRK-FUTANWARI)
                         COMPUTE RECE82-KINGAKU3(2)  =
                                 RECE82-KINGAKU3(2)  +
                             (WRK-KEISAN-KG2 * ACCT-ZAIKAISU)
                      ELSE
                         COMPUTE RECE82-KINGAKU3(2)  =
                                 RECE82-KINGAKU3(2)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE WRK-TEN-KG2 = TNS-TEN
                      END-IF
                     END-IF
                    ELSE
                     IF  SRY-SRYCD(IDX3)      =    "199000510"
                       COMPUTE    RECE82-KINGAKU3(2)  =
                                  RECE82-KINGAKU3(2)  +
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)
                     ELSE
                      COMPUTE      RECE82-TENSU(21)  =
                                   RECE82-TENSU(21)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                      IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)  =
                                   RECE82-TENSU(2)  +
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         COMPUTE   RECE82-TENSU(21) =
                                   RECE82-TENSU(21) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                      END-IF
                      COMPUTE      RECE82-TENSU-YAK(22)  =
                                   RECE82-TENSU-YAK(22)  +
                                  (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                ELSE
                 IF ((SRY-SRYCD(IDX3) =
                      "101800880"   OR
                      "101800870"   OR  "101800470"   OR
                      "101800480"   OR  "101800490"   OR
                      "101800500"   OR  "101800510"   OR
                      "101800520"   OR  "101800530"   OR
                      "101800540"   OR  "101800960"   OR
                      "101800970"   OR  "101800980"   OR
                      "101801040"                         ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = "101800880"
                         COMPUTE RECE82-KINGAKU2(2)  =
                                 RECE82-KINGAKU2(2)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                  IF ((SRY-SRYCD(IDX3) =
                       "101910010"    OR  "101910020"    OR
                       "101910030"    OR  "101910040"    OR
                       "101910070"    OR  "101910080"    OR
                       "101910090"    OR  "101910100"   )  AND
                      (RECE82-SRYYM   <=   "201306"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       "101800390"    OR  "101800400"    OR
                       "101800410"    OR  "101800420"    OR
                       "101800430"    OR  "101800440"    OR
                       "101800450"    OR  "101800460"   )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       "101801150"    OR  "101801160"    OR
                       "101801170"    OR  "101801180"   )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                     IF ((SRY-SRYCD(IDX3) =
                          "101801150"    OR  "101801160"  OR
                          "101801170"    OR  "101801180"   )  AND
                         (RECE82-SRYYM   >=   "201307"     ))
                     OR ((ACCT-SRYKBN          =  "80"     )  AND
                         (SRY-SRYCD(IDX3)(1:7) =
                               "0959421" OR "0959422" OR
                               "0959423" OR "0959424"      )  AND
                         (RECE82-SRYYM   >=   "201604"     ))
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                       END-IF
                     ELSE
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE RECE82-KINGAKU3(1)  =
                                 RECE82-KINGAKU3(1)  +
                                (WRK-JIBAIKZI-TNS(IDX3)
                                           *   ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE RECE82-KINGAKU3(1)  =
                                 RECE82-KINGAKU3(1)  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                       END-IF
                       MOVE    ZERO   TO   WRK-HEYA-NINZU
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   "101910010"
                              WHEN   "101910020"
                              WHEN   "101800390"
                              WHEN   "101800400"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "101910030"
                              WHEN   "101910040"
                              WHEN   "101800410"
                              WHEN   "101800420"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "101910070"
                              WHEN   "101910080"
                              WHEN   "101800430"
                              WHEN   "101800440"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "101910090"
                              WHEN   "101910100"
                              WHEN   "101800450"
                              WHEN   "101800460"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   RECE82-HEYA-NISSU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                     END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"   )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                    OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                         <= "101910400" )  AND
                        (RECE82-SRYYM   <=  "201306"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                         <= "101800860" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                         <= "101801020" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101801190" AND
                                         <= "101801420" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                             "101800560" OR "101800990" OR
                             "101801030"                )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                     IF  SRY-SRYCD(IDX3)(1:5) =  "09593"
                      EVALUATE  WRK-JIBAIKZI-SYUKEIKBN(IDX3)
                       WHEN       0
                       WHEN       5
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       1
                         COMPUTE RECE82-KINGAKU2(1)  =
                                 RECE82-KINGAKU2(1)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       4
                         COMPUTE RECE82-KINGAKU2(2)  =
                                 RECE82-KINGAKU2(2)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                      IF  SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990" OR
                              "101801030"
                         COMPUTE WRK-FUTANWARI  = TNS-TEN / 100
                         COMPUTE WRK-KEISAN-KG2 =
                             (WRK-TEN-KG2 * WRK-FUTANWARI)
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (WRK-KEISAN-KG2 * ACCT-ZAIKAISU)
                      ELSE
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE WRK-TEN-KG2 = TNS-TEN
                      END-IF
                     END-IF
                    ELSE                                                
                     IF  SRY-SRYCD(IDX3)      =    "199000510"
                       COMPUTE    RECE82-KINGAKU3(3)  =                 
                                  RECE82-KINGAKU3(3)  +                 
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
      **>                IF    WK-SYUTEN       NOT =   ZERO
      **>                   ADD  WK-ZAIKAISU  TO  RECE82-KAISU(14)
      **>                END-IF
                         COMPUTE   RECE82-TENSU(14)  =
                                   RECE82-TENSU(14)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       WHEN      "1"
                         COMPUTE   RECE82-TENSU(22)  =
                                   RECE82-TENSU(22)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                      IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)  =
                                   RECE82-TENSU(2)  +
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         EVALUATE  WRK-PARA-JIBAIPRTKBN
                          WHEN      "0"
                           COMPUTE RECE82-TENSU(14) =
                                   RECE82-TENSU(14) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                          WHEN      "1"
                           COMPUTE RECE82-TENSU(22) =
                                   RECE82-TENSU(22) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         END-EVALUATE
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                        WHEN      "0"
                          COMPUTE   RECE82-TENSU-YAK(15)  =
                                    RECE82-TENSU-YAK(15)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                        WHEN      "1"
                          COMPUTE   RECE82-TENSU-YAK(23)  =
                                    RECE82-TENSU-YAK(23)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                  END-IF
                 END-IF
                END-IF
           END-EVALUATE
           .
      *
       220222-RECE-LEFT-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220223-RECE-LEFT-SET3-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                 IF   (RECE82-HKNKBN    =  "4")
                 AND  (RECE82-JIBAIKBN  =  "1")
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)    =
                                RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     COMPUTE    TBL-SYUTEN-JI(1)   =
                                TBL-SYUTEN-JI(1)   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  "111000370"   OR
                                             "111011470"   OR
                                             "111011970"   OR
                                             "111012070"   OR
                                             "111012170"   OR
                                             "111012270"   OR
                                             "111011570"   OR
                                             "111011670"   OR
                                             "111011770"
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = "101110010"  OR
                                          "101110040"
     ****               COMPUTE    RECE82-SONOTAKIN-JYU  =
     ****                          RECE82-SONOTAKIN-JYU  +
     ****                          (TNS-TEN         *  ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                                   (TNS-TEN         *  ACCT-ZAIKAISU)
                   END-IF
                   IF  (SRY-SRYCD(IDX3) = "101110020" )
                   AND (RECE82-SRYYM   <=   "201306"  )
     ****               COMPUTE    RECE82-SONOTAKIN-JYU  =
     ****                          RECE82-SONOTAKIN-JYU  +
     ****                          (TNS-TEN         *  ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                                   (TNS-TEN         *  ACCT-ZAIKAISU)
                   END-IF  
                 ELSE
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
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
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   COMPUTE    RECE82-TENSU(1)    =
                              RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE    TBL-SYUTEN-JI(1)   =
                              TBL-SYUTEN-JI(1)   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                 END-IF
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "12"
                 COMPUTE        RECE82-NYUSAI-TENSU  =
                                RECE82-NYUSAI-TENSU  +
                               (WK-ZAITEN   *   WK-ZAIKAISU)
                 EVALUATE  WRK-NYUSAI-SRYKBN
                   WHEN      "90"
                     COMPUTE    RECE82-NYUKHNKEI2    =
                                RECE82-NYUKHNKEI2    +
                               (WK-ZAITEN   *   WK-ZAIKAISU)
                   WHEN      "92"
                     COMPUTE    RECE82-NYUTOKKEI2    =
                                RECE82-NYUTOKKEI2    +
                               (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-EVALUATE
                 COMPUTE        RECE82-NYUTOTALTEN   =
                                RECE82-NYUTOTALTEN   +
                               (WK-ZAITEN   *   WK-ZAIKAISU)
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                 IF   (RECE82-HKNKBN    =  "4")
                 AND  (RECE82-JIBAIKBN  =  "1")
                   IF ((SRY-SRYCD(IDX3) =
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"                         ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   "101130010"     ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-SONOTAKIN-JYU  =
                                    RECE82-SONOTAKIN-JYU  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE8G-TENSU(2)  =
                                    WRK-RECE8G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2) =
                                    TBL-SYUTEN-JI(2) +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
                 ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE8G-TENSU(2)  =
                                    WRK-RECE8G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2) =
                                    TBL-SYUTEN-JI(2) +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                 END-IF
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    WRK-RECE8G-TENSU(28) =
                                  WRK-RECE8G-TENSU(28) +
                                 (WK-ZAITEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    WRK-RECE8G-TENSU(27) =
                                  WRK-RECE8G-TENSU(27) +
                                 (WK-ZAITEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 ELSE
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-SYUTEN-JI(28) =
                                  TBL-SYUTEN-JI(28) +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-YKZTEN-JI(28) =
                                  TBL-YKZTEN-JI(28) +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    RECE82-TENSU(27)  =
                                  RECE82-TENSU(27)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-SYUTEN-JI(27) =
                                  TBL-SYUTEN-JI(27) +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-YKZTEN-JI(28) =
                                  TBL-YKZTEN-JI(28) +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    "630010002"
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                           COMPUTE   TBL-YKZTEN-JI(4) =
                                     TBL-YKZTEN-JI(4) -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                           COMPUTE   TBL-YKZTEN-JI(4) =
                                     TBL-YKZTEN-JI(4) +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    "099209901"
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU(4)  =
                                      RECE82-TENSU(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                            COMPUTE   TBL-YKZTEN-JI(4) =
                                      TBL-YKZTEN-JI(4) +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU(5)  =
                             RECE82-TENSU(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(5) =
                             TBL-YKZTEN-JI(5) +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU(6)  =
                             RECE82-TENSU(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(6) =
                             TBL-YKZTEN-JI(6) +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(7)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(7)
                     ELSE
                      IF   RECE82-KHNTEN(7) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(7)
                           MOVE   1       TO    FLG-KHNTEN7-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(7) =
                             TBL-SYUTEN-JI(7) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(8)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(8)
                     ELSE
                      IF   RECE82-KHNTEN(8) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(8)
                           MOVE   1       TO    FLG-KHNTEN8-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(8) =
                             TBL-SYUTEN-JI(8) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(9)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(9)
                     ELSE
                      IF   RECE82-KHNTEN(9) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(9)
                           MOVE   1       TO    FLG-KHNTEN9-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(9)
                   END-IF
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(9) =
                             TBL-SYUTEN-JI(9) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)   =
                             RECE82-TENSU(11)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(11)  =
                             TBL-SYUTEN-JI(11)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(11)   =
                             RECE82-TENSU(11)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(11)  =
                             TBL-YKZTEN-JI(11)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(11)  =
                             WRK-RECE8C-TENSU(11)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI-C(11)  =
                             TBL-YKZTEN-JI-C(11)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)   =
                             RECE82-TENSU(12)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(12)  =
                             TBL-SYUTEN-JI(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(12)   =
                             RECE82-TENSU(12)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(12)  =
                             TBL-YKZTEN-JI(12)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(12)  =
                             WRK-RECE8C-TENSU(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI-C(12)  =
                             TBL-YKZTEN-JI-C(12)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE8G-TENSU(13)   =
                             WRK-RECE8G-TENSU(13)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(13)
                   COMPUTE   RECE82-TENSU(13)   =
                             RECE82-TENSU(13)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(13)  =
                             TBL-SYUTEN-JI(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(13)   =
                             RECE82-TENSU(13)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(13)  =
                             TBL-YKZTEN-JI(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(13)  =
                             WRK-RECE8C-TENSU(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI-C(13)  =
                             TBL-YKZTEN-JI-C(13)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)   =
                             RECE82-TENSU(15)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(15)  =
                             TBL-SYUTEN-JI(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(16)   =
                             RECE82-TENSU(16)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(16)  =
                             TBL-YKZTEN-JI(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   END-IF
                   COMPUTE   RECE82-TENSU(18)   =
                             RECE82-TENSU(18)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(18)  =
                             TBL-SYUTEN-JI(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(19)   =
                             RECE82-TENSU(19)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(19)  =
                             TBL-YKZTEN-JI(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(21)
                   END-IF
                   COMPUTE   RECE82-TENSU(21)   =
                             RECE82-TENSU(21)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(21)  =
                             TBL-SYUTEN-JI(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(21)  =
                                RECE82-KAISU(21)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   "160101210"     OR
                                              "160095710"     OR
                                              "160095810"
                         IF   RECE82-KAISU(21)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(21)  =
                                        RECE82-KAISU(21)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(22)   =
                             RECE82-TENSU(22)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(22)  =
                             TBL-YKZTEN-JI(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(24)
                   END-IF
                   COMPUTE   RECE82-TENSU(24)   =
                             RECE82-TENSU(24)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(24)  =
                             TBL-SYUTEN-JI(24)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(25)   =
                             RECE82-TENSU(25)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(25)  =
                             TBL-YKZTEN-JI(25)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                 IF ((SRY-SRYCD(IDX3) =
                      "101800880"   OR
                      "101800870"   OR  "101800470"   OR
                      "101800480"   OR  "101800490"   OR
                      "101800500"   OR  "101800510"   OR
                      "101800520"   OR  "101800530"   OR
                      "101800540"   OR  "101800960"   OR
                      "101800970"   OR  "101800980"   OR
                      "101801040"                         ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                   IF   SRY-SRYCD(IDX3) = "101800880"
                             COMPUTE  WRK-KINGAKU-JI   =
                                      WRK-KINGAKU-JI   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                             COMPUTE  RECE82-SONOTAKIN-JYU  =
                                      RECE82-SONOTAKIN-JYU  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                 ELSE
                  IF ((SRY-SRYCD(IDX3) =
                       "101910010"    OR  "101910020"    OR
                       "101910030"    OR  "101910040"    OR
                       "101910070"    OR  "101910080"    OR
                       "101910090"    OR  "101910100"   )  AND
                      (RECE82-SRYYM   <=   "201306"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       "101800390"    OR  "101800400"    OR
                       "101800410"    OR  "101800420"    OR
                       "101800430"    OR  "101800440"    OR
                       "101800450"    OR  "101800460"   )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((SRY-SRYCD(IDX3) =
                       "101801150"    OR  "101801160"    OR
                       "101801170"    OR  "101801180"   )  AND
                      (RECE82-SRYYM   >=   "201307"     ))
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                     IF ((SRY-SRYCD(IDX3) =
                          "101801150"    OR  "101801160"  OR
                          "101801170"    OR  "101801180"   )  AND
                         (RECE82-SRYYM   >=   "201307"     ))
                     OR ((ACCT-SRYKBN          =  "80"     )  AND
                         (SRY-SRYCD(IDX3)(1:7) =
                               "0959421" OR "0959422" OR
                               "0959423" OR "0959424"      )  AND
                         (RECE82-SRYYM   >=   "201604"     ))
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE  RECE82-SONOTAKIN-JYU =
                                  RECE82-SONOTAKIN-JYU +
                           (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE  RECE82-SONOTAKIN-JYU  =
                                  RECE82-SONOTAKIN-JYU  +
                           (TNS-TEN   *   ACCT-ZAIKAISU)
                       END-IF
                     ELSE
                       IF   SRY-SRYCD(IDX3)(1:5) =  "09594"
                         COMPUTE RECE82-SITURYO-JYU  =
                                 RECE82-SITURYO-JYU  +
                                (WRK-JIBAIKZI-TNS(IDX3)
                                           *   ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE RECE82-SITURYO-JYU  =
                                 RECE82-SITURYO-JYU  +
                                (TNS-TEN   *   ACCT-ZAIKAISU)
                       END-IF
                       MOVE    ZERO   TO   WRK-HEYA-NINZU
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   "101910010"
                              WHEN   "101910020"
                              WHEN   "101800390"
                              WHEN   "101800400"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "101910030"
                              WHEN   "101910040"
                              WHEN   "101800410"
                              WHEN   "101800420"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "101910070"
                              WHEN   "101910080"
                              WHEN   "101800430"
                              WHEN   "101800440"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "101910090"
                              WHEN   "101910100"
                              WHEN   "101800450"
                              WHEN   "101800460"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   RECE82-HEYA-NISSU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                     END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"   )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                    OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                         <= "101910400" )  AND
                        (RECE82-SRYYM   <=  "201306"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                         <= "101800860" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                         <= "101801020" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3) >= "101801190" AND
                                         <= "101801420" )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                    OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                             "101800560" OR "101800990" OR
                             "101801030"                )  AND
                        (RECE82-SRYYM   >=  "201307"    ))
                     IF  SRY-SRYCD(IDX3)(1:5) =  "09593"
                      IF   (RECE82-HKNKBN    =  "4")
                      AND  (RECE82-JIBAIKBN  =  "1")
                         EVALUATE  WRK-JIBAIKZI-SYUKEIKBN(IDX3)
                           WHEN       0
                           WHEN       5
                             COMPUTE  RECE82-SONOTAKIN-JYU =
                                      RECE82-SONOTAKIN-JYU +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                           WHEN       1
                           WHEN       4
                             COMPUTE  WRK-KINGAKU-JI =
                                      WRK-KINGAKU-JI +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                         END-EVALUATE
                      ELSE
                             COMPUTE  RECE82-SONOTAKIN-JYU =
                                      RECE82-SONOTAKIN-JYU +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                      END-IF
                     ELSE
                      IF  SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990" OR
                              "101801030"
                             COMPUTE  WRK-FUTANWARI  = TNS-TEN / 100
                             COMPUTE  WRK-KEISAN-KG2 =
                               (WRK-TEN-KG2 * WRK-FUTANWARI)
                             COMPUTE  RECE82-SONOTAKIN-JYU  =
                                      RECE82-SONOTAKIN-JYU  +
                               (WRK-KEISAN-KG2 * ACCT-ZAIKAISU)
                      ELSE
                             COMPUTE  RECE82-SONOTAKIN-JYU  =
                                      RECE82-SONOTAKIN-JYU  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                             COMPUTE  WRK-TEN-KG2 = TNS-TEN
                      END-IF
                     END-IF
                    ELSE                                                
                     IF  RECE82-HKNKBN        =    "4"
                     AND RECE82-JIBAIKBN      =    "1"
                     AND SRY-SRYCD(IDX3)      =    "199000510"
                       COMPUTE    RECE82-SONOTAKIN-JYU  =               
                                  RECE82-SONOTAKIN-JYU  +               
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
      **>                IF    WK-SYUTEN       NOT =   ZERO
      **>                   ADD  WK-ZAIKAISU  TO  RECE82-KAISU(15)
      **>                END-IF
                         COMPUTE   RECE82-TENSU(15)   =
                                   RECE82-TENSU(15)   +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                         COMPUTE   TBL-SYUTEN-JI(15)  =
                                   TBL-SYUTEN-JI(15)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       WHEN      "1"
                         COMPUTE   RECE82-TENSU(27)   =
                                   RECE82-TENSU(27)   +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                         COMPUTE   TBL-SYUTEN-JI(27)  =
                                   TBL-SYUTEN-JI(27)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                      IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)   =
                                   RECE82-TENSU(2)   +
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         COMPUTE   TBL-SYUTEN-JI(2)  =
                                   TBL-SYUTEN-JI(2)  +
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         EVALUATE  WRK-PARA-JIBAIPRTKBN
                          WHEN      "0"
                            COMPUTE   RECE82-TENSU(15)  =
                                      RECE82-TENSU(15)  -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(15) =
                                      TBL-SYUTEN-JI(15) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                          WHEN      "1"
                            COMPUTE   RECE82-TENSU(27)  =
                                      RECE82-TENSU(27)  -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(27) =
                                      TBL-SYUTEN-JI(27) -
                                  (TNS-TEN   *   ACCT-ZAIKAISU)
                         END-EVALUATE
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                        WHEN      "0"
                         IF  SRY-SRYCD(IDX3) = "199000510"
                          COMPUTE   RECE82-TENSU(28)  =
                                    RECE82-TENSU(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(28)  =
                                    TBL-YKZTEN-JI(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                         ELSE
                          COMPUTE   RECE82-TENSU(16)  =
                                    RECE82-TENSU(16)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(16)  =
                                    TBL-YKZTEN-JI(16)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                         END-IF
                        WHEN      "1"
                          COMPUTE   RECE82-TENSU(28)  =
                                    RECE82-TENSU(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(28)  =
                                    TBL-YKZTEN-JI(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                  END-IF
                 END-IF
           END-EVALUATE
           .
      *
       220223-RECE-LEFT-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220224-RECE-LEFT-SET4-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF  ( SRY-SRYCD(IDX3) = "101110010"  OR
                                           "101110040"   )
                   OR  ((SRY-SRYCD(IDX3) = "101110020"  ) AND
                        (RECE82-SRYYM   <=   "201306"   ))
                      CONTINUE
                   ELSE
                      IF   TNS-DATAKBN  =   2
                        EVALUATE    TNS-TIMEKSNKBN
                            WHEN    1
                            WHEN    5
                            WHEN    6
                                MOVE  "1"     TO   RECE82-SHOSHINTIME
                            WHEN    3
                                MOVE  "2"     TO   RECE82-SHOSHINTIME
                            WHEN    4
                                MOVE  "3"     TO   RECE82-SHOSHINTIME
                        END-EVALUATE
                      END-IF
                      IF    RECE82-KOMUKBN  =   "2"
                        IF  TNS-CHUKSNTUBAN       NOT = "0"
                        OR  SRY-SRYCD(IDX3)(1:1)  NOT = "1"
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(1)
                        END-IF
                      ELSE
                        IF  TNS-DATAKBN  =   2
                          IF   SRY-SRYCD(IDX3) = "111012370"
                            CONTINUE
                          ELSE
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(1)
                          END-IF
                        END-IF
                      END-IF
                      COMPUTE   RECE82-TENSU(1)    =
                                RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "12"
                   COMPUTE        RECE82-NYUSAI-TENSU  =
                                  RECE82-NYUSAI-TENSU  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   EVALUATE  WRK-NYUSAI-SRYKBN
                     WHEN      "90"
                       COMPUTE    RECE82-NYUKHNKEI2    =
                                  RECE82-NYUKHNKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                     WHEN      "92"
                       COMPUTE    RECE82-NYUTOKKEI2    =
                                  RECE82-NYUTOKKEI2    +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-EVALUATE
                   COMPUTE        RECE82-NYUTOTALTEN   =
                                  RECE82-NYUTOTALTEN   +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF ((SRY-SRYCD(IDX3) =
                        "101130020"   OR  "101130030"   OR
                        "101130040"   OR  "101130050"   OR
                        "101130060"   OR  "101130070"   OR
                        "101130080"   OR  "101130090"   OR
                        "101130100"                         ) AND
                       (RECE82-SRYYM   <=   "201306"        ))
                   OR ((SRY-SRYCD(IDX3) =   "101130010"     ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         CONTINUE
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE8G-TENSU(9)  =
                                    WRK-RECE8G-TENSU(9)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(9)   =
                                    RECE82-TENSU(9)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE8G-TENSU(14) =
                              WRK-RECE8G-TENSU(14) +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(10)
                       COMPUTE   RECE82-TENSU(10)   =
                                 RECE82-TENSU(10)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(11)
                       COMPUTE   RECE82-TENSU(11)   =
                                 RECE82-TENSU(11)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(12)
                       COMPUTE   RECE82-TENSU(12)   =
                                 RECE82-TENSU(12)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF  ( TNS-OSINKBN           =     1          )  AND
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
                        (SRY-SRYCD(IDX3)   NOT =    "114030270"))
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                       COMPUTE   RECE82-TENSU(13)   =
                                 RECE82-TENSU(13)   +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE82-TENSU(14)   =
                                 RECE82-TENSU(14)   +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE82-TENSU(15)   =
                             RECE82-TENSU(15)   +
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
                     IF   SRY-SRYCD(IDX3)         =    "630010002"
                           COMPUTE   RECE82-TENSU(16)  =
                                     RECE82-TENSU(16)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(16)
                           COMPUTE   RECE82-TENSU(16)  =
                                     RECE82-TENSU(16)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    "099209901"
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(16)
                            COMPUTE   RECE82-TENSU(16)  =
                                      RECE82-TENSU(16)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(20)
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(22)
                   END-IF
                   COMPUTE   RECE82-TENSU(22)  =
                             RECE82-TENSU(22)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(23)
                   END-IF
                   COMPUTE   RECE82-TENSU(23)  =
                             RECE82-TENSU(23)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE82-TENSU(24)  =
                             RECE82-TENSU(24)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(25)
                   COMPUTE   RECE82-TENSU(25)  =
                             RECE82-TENSU(25)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(25)  =
                             WRK-RECE8C-TENSU(25)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(26)
                   COMPUTE   RECE82-TENSU(26)  =
                             RECE82-TENSU(26)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(26)  =
                             WRK-RECE8C-TENSU(26)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE   WRK-RECE8G-TENSU(27)   =
                             WRK-RECE8G-TENSU(27)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(27)
                   COMPUTE   RECE82-TENSU(27)  =
                             RECE82-TENSU(27)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   WRK-RECE8C-TENSU(27)  =
                             WRK-RECE8C-TENSU(27)  +
                            (WK-YKZTEN2  *   WK-ZAIKAISU)
                 END-IF
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(28)
                   END-IF
                   COMPUTE   RECE82-TENSU(28)   =
                             RECE82-TENSU(28)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(29)   =
                             RECE82-TENSU(29)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(30)
                   END-IF
                   COMPUTE   RECE82-TENSU(30)   =
                             RECE82-TENSU(30)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(31)   =
                             RECE82-TENSU(31)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(32)
                   END-IF
                   COMPUTE   RECE82-TENSU(32)   =
                             RECE82-TENSU(32)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                         COMPUTE   RECE82-KAISU(32)  =
                                   RECE82-KAISU(32)  -
                                   WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD  =   "160101210"     OR
                                        "160095710"     OR
                                        "160095810"
                         IF   RECE82-KAISU(32)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(32)  =
                                        RECE82-KAISU(32)  -
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(33)   =
                             RECE82-TENSU(33)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(34)
                   END-IF
                   COMPUTE   RECE82-TENSU(34)   =
                             RECE82-TENSU(34)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(35)   =
                             RECE82-TENSU(35)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                 IF ((SRY-SRYCD(IDX3) =
                      "101800880"   OR
                      "101800870"   OR  "101800470"   OR
                      "101800480"   OR  "101800490"   OR
                      "101800500"   OR  "101800510"   OR
                      "101800520"   OR  "101800530"   OR
                      "101800540"   OR  "101800960"   OR
                      "101800970"   OR  "101800980"   OR
                      "101801040"                         ) AND
                     (RECE82-SRYYM    >=  "201307"        ))
                     CONTINUE
                 ELSE
                   IF ((SRY-SRYCD(IDX3) =
                        "101910010"    OR  "101910020"    OR
                        "101910030"    OR  "101910040"    OR
                        "101910070"    OR  "101910080"    OR
                        "101910090"    OR  "101910100"   )  AND
                       (RECE82-SRYYM   <=   "201306"     ))
                   OR ((SRY-SRYCD(IDX3) =
                        "101800390"    OR  "101800400"    OR
                        "101800410"    OR  "101800420"    OR
                        "101800430"    OR  "101800440"    OR
                        "101800450"    OR  "101800460"   )  AND
                       (RECE82-SRYYM   >=   "201307"     ))
                   OR ((SRY-SRYCD(IDX3) =
                        "101801150"    OR  "101801160"    OR
                        "101801170"    OR  "101801180"   )  AND
                       (RECE82-SRYYM   >=   "201307"     ))
                   OR ((ACCT-SRYKBN          =  "80"     )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                     IF   (FLG-TAISYO           =   1     )
                     AND  (SRY-SRYCD(IDX3)(1:5) =  "09594")
                         COMPUTE   WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                     END-IF
                   ELSE
                     IF ((ACCT-SRYKBN          =  "80"   )  AND
                         (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                     OR ((SRY-SRYCD(IDX3) >= "101910110" AND
                                          <= "101910400" )  AND
                         (RECE82-SRYYM   <=  "201306"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101800570" AND
                                          <= "101800860" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101801010" AND
                                          <= "101801020" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3) >= "101801190" AND
                                          <= "101801420" )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                     OR ((SRY-SRYCD(IDX3)  = "101800550" OR
                              "101800560" OR "101800990" OR
                              "101801030"                )  AND
                         (RECE82-SRYYM   >=  "201307"    ))
                       IF   (FLG-TAISYO           =   1     )
                       AND  (SRY-SRYCD(IDX3)(1:5) =  "09593")
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-JIBAIKZI-TNS(IDX3)
                                               *   ACCT-ZAIKAISU)
                       END-IF
                     ELSE                
                       COMPUTE     RECE82-TENSU(37) =
                                   RECE82-TENSU(37) +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(9)  =
                                   RECE82-TENSU(9)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         COMPUTE   RECE82-TENSU(37) =
                                   RECE82-TENSU(37) -
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                       ELSE
                         IF  SRY-SRYCD(IDX3) = "101989050"
                          COMPUTE  RECE82-TENSU(1)  =
                                   RECE82-TENSU(1)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                          COMPUTE  RECE82-TENSU(37) =
                                   RECE82-TENSU(37) -
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         END-IF
                       END-IF
                       COMPUTE       RECE82-TENSU(38)  =
                                     RECE82-TENSU(38)  +
                                    (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
                 END-IF
           END-EVALUATE
           .
      *
       220224-RECE-LEFT-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅ¾ðÊóÏ¢·ÈÊÔ½¸
      *****************************************************************
       22023-IRYORENKEI-HENSYU-SEC               SECTION.
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
             MOVE    RECE82-HOSPNUM      TO  ORCSGETGE-HOSPNUM
             MOVE    RECE82-PTID         TO  ORCSGETGE-PTID
             MOVE    RECE82-SRYYM        TO  ORCSGETGE-SRYYM
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
       22023-IRYORENKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÞ¤Î¿ÇÎÅÆâÍÆ¥Á¥§¥Ã¥¯
      *****************************************************************
       2203-SINRYO-CHECK-SEC                 SECTION.
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
                 MOVE RECE82-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE RECE82-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE RECE82-PTID        TO  SRY-PTID
                 MOVE RECE82-SRYYM       TO  SRY-SRYYM
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
                 IF      MCP-RC  =  ZERO
                     MOVE    "tbl_sryact"   TO  MCP-TABLE
                     MOVE    "key8"         TO  MCP-PATHNAME
                     PERFORM  900-SRYACT-READ-SEC
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
       2203-SINRYO-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊó¸¡º÷
      **************************************************************
       2204-SRYACCTPLUS-KENSAKU-SEC           SECTION.
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
               PERFORM  900-SRYACCTPLUS-READ-SEC
               IF        FLG-SRYACCTPLUS   =   ZERO
                 IF   ACCTP-RSIKINGAKU  NOT = ZERO
                    MOVE    1   TO   FLG-TAISYO
                 END-IF
               END-IF
           END-IF
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       2204-SRYACCTPLUS-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤áÂÐ¾Ý¤«¥Á¥§¥Ã¥¯
      *****************************************************************
       2205-TENTEKIMATOME-CHECK-SEC                 SECTION.
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
       2205-TENTEKIMATOME-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥é¥°Åù¤Î¥¯¥ê¥¢
      *****************************************************************
       2206-FLG-KURIA-SEC                 SECTION.
      *
           MOVE   ZERO   TO   FLG-RINYAK
      *
           MOVE   ZERO   TO   WRK-TEN
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
                              FLG-SRYKBN-HEN
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
           MOVE   SPACE  TO   WRK-FORMALNAME
           MOVE   ZERO   TO   WRK-KZI058-TEN
                              WRK-KZI058-TANICD
                              WRK-KZI058-TANIMOJISU
           MOVE   SPACE  TO   WRK-KZI058-TANINAME
      *
           MOVE   ZERO   TO   FLG-WRITE85
           MOVE   ZERO   TO   FLG-TENTEKI
           MOVE   ZERO   TO   FLG-RIHA-OKIKAE
           MOVE   ZERO   TO   FLG-HENSYU-NG
           MOVE   ZERO   TO   FLG-HENSYU-OK
           MOVE   ZERO   TO   FLG-SETCOM
           MOVE   ZERO   TO   WRK-SETCOM-JIDO
           MOVE   ZERO   TO   WRK-SETCOM-JIDO2
           MOVE   ZERO   TO   FLG-KIN-OKIKAE
           MOVE   ZERO   TO   FLG-KIN-OKIKAE2
           MOVE   ZERO   TO   FLG-YKZ-PRTKBN
           MOVE   ZERO   TO   FLG-KIZ-PRTKBN
           MOVE   SPACE  TO   WRK-NYUSAI-SRYKBN
      *
           .
       2206-FLG-KURIA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       230-RECE84-HENSYU2-SEC                SECTION.
      *
           MOVE      ZERO      TO    WRK-TEN
                                     WRK-MEISAI-CNT
                                     WRK-SRYCDTOTAL
                                     WRK-SURYOUTOTAL
                                     WRK-ZAI-KINGAKU
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =  1)
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  
                   IF     NACCT-SRYCDTOTAL    =    099999916
                       CONTINUE
                   ELSE
                     PERFORM    VARYING   IDY   FROM   1   BY   1
                                      UNTIL    IDY    >    31
                       IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDY) 
                       END-IF
                     END-PERFORM
                   END-IF
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
                 MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                 MOVE    "key7"            TO      MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"         TO      MCP-TABLE
           MOVE    "key7"                  TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ë¿©»ö»»ÄêÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¼èÆÀ
           PERFORM      230A-SKJHKN-GET-SEC
      *
           MOVE    ZERO    TO    WRK-ZAINUM-CNT2
           MOVE    ZERO    TO    FLG-DOJITU
           INITIALIZE            WRK-SKJ-DOJITU-TABLE
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =  1)
      *
                 IF     NACCT-ZAISKBKBN    =     "4"  OR  "6"  OR "7"
      *
                   MOVE     ZERO    TO    FLG-JYOGAI
                   IF    NACCT-ZAISKBKBN    =  "6"
                     PERFORM  VARYING  ID4    FROM   1   BY   1
                               UNTIL  (ID4                  >   100 )
                                 OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                                 OR   (FLG-JYOGAI           =   1   )
                       IF  NACCT-SRYCDTOTAL(6:9) = WRK-CHUKSNSRYCD(ID4)
                          MOVE     1    TO   FLG-JYOGAI
                       END-IF
                     END-PERFORM
                   END-IF
      *
                   IF ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190117070 OR 190109670) AND
                       (RECE82-SRYYM      >=  "200604"             ) AND
                       (RECE82-SRYYM      <=  "201203"             ))
                   OR ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190126670 OR 190126770
                                          OR 190126570 OR 190126470) AND
                       (RECE82-SRYYM      >=  "200804"             ) AND
                       (RECE82-SRYYM      <=  "201203"             ))
                   OR ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190129870             ) AND
                       (RECE82-SRYYM      >=  "200804"             ) AND
                       (RECE82-SRYYM      <=  "201003"             ))
                   OR ( FLG-JYOGAI         = 1                      )
      *
                      CONTINUE
      *
                   ELSE
      *
                      IF   NACCT-ZAISKBKBN  = "6" OR "7"
                         ADD    1         TO    WRK-ZAINUM-CNT2
                      END-IF
      *
                      INITIALIZE          WRK-DAY-TABLE2
                                          WRK-SKJ-TABLE
                                          WRK-SKJ-TABLE2
                      MOVE     ZERO       TO    CNT-DAY21
                                                CNT-DAY24
                      MOVE     ZERO       TO    FLG-DAY21
                                                FLG-DAY24
      *
                      MOVE     ZERO       TO    WRK-TEN
                                                WRK-MEISAI-CNT
                                                WRK-SRYCDTOTAL
                                                WRK-SURYOUTOTAL
                                                WRK-ZAI-KINGAKU
      *
                      PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                         IF  RECE82-HKNCOMBI     =  WRK-DAY5(IDY)
                            IF  ((WRK-DAY1(IDY)   NOT  =  ZERO  ) OR
                                 (RECE82-NYUINDAY-KBN(IDY) = "1"))
                            AND ( NACCT-ZAITEN    NOT  =  ZERO   )
                            AND ( NACCT-ZAISKBKBN NOT  =  "4"    )
                            AND ( NACCT-DAY(IDY)  NOT  =  ZERO   )
                               IF (WRK-DAY2(IDY)  =  ZERO)
                               OR (WRK-DAY2(IDY) >=   4  )
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
                               END-IF
                            END-IF
      *                     *¿©»ö
                            PERFORM      230B-SKJ-HENSYU-SEC
                         ELSE
      *                     *¿©»ö
                            PERFORM      230C-SKJ-HENSYU2-SEC
                         END-IF
      *
                      END-PERFORM
      *
                      IF   FLG-DAY21    =   1
                         PERFORM   2302-RECE84-TENSOU2-SEC
                         PERFORM   2303-RECE85-HENSYU2-SEC
                         PERFORM   900-RECE84-WRITE-SEC
                      END-IF
      *
                      IF   FLG-DAY24    =   1
                       IF   RECE82-SRYYM  >= "200604"
                         CONTINUE
                       ELSE
                         PERFORM    2304-NYUINACCT-HEN-SEC
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           >  31
                             OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) = ZERO
      *
                             PERFORM   2305-RECE84-TENSOU2-SKJ-SEC
                             PERFORM   2306-RECE85-HENSYU2-SKJ-SEC
                             PERFORM   900-RECE84-WRITE-SEC
      *
                         END-PERFORM
                       END-IF
                      END-IF
      *
                   END-IF
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                 MOVE    "key7"            TO      MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"         TO      MCP-TABLE
           MOVE    "key7"                  TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    Æ±ÆüºÆÆþ±¡
      *
           IF   RECE82-SRYYM   >=  "200604"
              IF   FLG-DOJITU   =   1
                    PERFORM   230D-NYUINACCT-SYU-DOJITU-SEC
              END-IF
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ
           IF     RECE82-SRYYM   >=  "201604"
             INITIALIZE                   ORCSNYULIFE2-AREA
             MOVE    RECE82-HOSPNUM   TO  LNK-NYULIFE2-HOSPNUM
             MOVE    RECE82-PTID      TO  LNK-NYULIFE2-PTID
             MOVE    RECE82-SRYYM     TO  LNK-NYULIFE2-SANTEIYM
             CALL    "ORCSNYULIFE2"       USING
                                          ORCSNYULIFE2-AREA
                                          SPA-AREA
             IF      LNK-NYULIFE2-RC  =   ZERO
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                         UNTIL   SYUKEI-IDX3   >   31
                 MOVE  LNK-NYULIFE2-KBN(SYUKEI-IDX3)
                                  TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
                 IF  ((RECE82-HKNKBN      =   "4") AND
                      (RECE82-JIBAIKBN    =   "2"))
                 OR  ((RECE82-HKNKBN      =   "5") AND
                      (RECE82-KOMUKBN     =   "2"))
                 OR  ( RECE82-HKNKBN      =   "6" )
                    CONTINUE
                 ELSE
                    MOVE  SPACE   TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
                 END-IF
               END-PERFORM
             END-IF
      *
      *      ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ
             IF   RECE82-HKNKBN  =  "6"
               INITIALIZE                   ORCSSKJKEEP-AREA
               MOVE    RECE82-HOSPNUM   TO  LNK-SKJKEEP-HOSPNUM
               MOVE    RECE82-PTID      TO  LNK-SKJKEEP-PTID
               MOVE    RECE82-SRYYM     TO  LNK-SKJKEEP-SANTEIYM
               CALL    "ORCSSKJKEEP"        USING
                                            ORCSSKJKEEP-AREA
                                            SPA-AREA
               IF      LNK-SKJKEEP-RC   =   ZERO
                 MOVE   ZERO   TO  FLG-ARI3
                 PERFORM  VARYING  IDX30  FROM  1  BY  1
                           UNTIL   IDX30   >   31
                   MOVE  LNK-SKJKEEP-KBN(IDX30)
                               TO  WRK-SKJKEEP-KBN(IDX30)
                   IF    WRK-SKJKEEP-KBN(IDX30) = "2"
                   AND   RECE82-SRYYM          >= "201710"
                      MOVE  1  TO  FLG-ARI3
                   END-IF
                 END-PERFORM
                 MOVE    LNK-SKJKEEP-SEI-NYUINYMD
                               TO  RECE82-SEISIN-NYUINYMD
                 IF   FLG-ARI3  =  1
                   INITIALIZE                ORCSSYOTOKUAREA
                   MOVE  RECE82-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
                   MOVE  RECE82-PTID     TO  LNK-SYOTOKU-PTID
                   MOVE  RECE82-SRYYM    TO  LNK-SYOTOKU-SRYYM
                   MOVE  "1"             TO  LNK-SYOTOKU-SKJCHKFLG
                   CALL  "ORCSSYOTOKU"   USING  ORCSSYOTOKUAREA
                                                SPA-AREA
                   IF    LNK-SYOTOKU-RC  =   ZERO
                     PERFORM  VARYING  IDX30  FROM  1  BY  1
                               UNTIL   IDX30   >   31
                       IF  LNK-SYOTOKU-SKJKBN(IDX30) NOT = SPACE
                       AND WRK-SKJKEEP-KBN(IDX30)        = "2"
                         MOVE  SPACE   TO  WRK-SKJKEEP-KBN(IDX30)
                       END-IF
                     END-PERFORM
                   END-IF
                 END-IF
               END-IF
             END-IF
           ELSE
            IF    RECE82-SRYYM   >=  "200610"
             INITIALIZE                   ORCSNYULIFE-AREA
             MOVE    RECE82-HOSPNUM   TO  LNK-NYULIFE-HOSPNUM
             MOVE    RECE82-PTID      TO  LNK-NYULIFE-PTID
             MOVE    RECE82-SRYYM     TO  LNK-NYULIFE-SANTEIYM
             CALL    "ORCSNYULIFE"        USING
                                          ORCSNYULIFE-AREA
                                          SPA-AREA
             IF      LNK-NYULIFE-RC   =   ZERO
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                         UNTIL   SYUKEI-IDX3   >   31
                 MOVE  LNK-NYULIFE-KBN(SYUKEI-IDX3)
                                  TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
                 IF  ((RECE82-HKNKBN      =   "4") AND
                      (RECE82-JIBAIKBN    =   "2"))
                 OR  ((RECE82-HKNKBN      =   "5") AND
                      (RECE82-KOMUKBN     =   "2"))
                 OR  ( RECE82-HKNKBN      =   "6" )
                    CONTINUE
                 ELSE
                    MOVE  SPACE   TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
                 END-IF
               END-PERFORM
             END-IF
            END-IF
           END-IF
      *
      *    £²£°£°£¶Ç¯£´·î°Ê¹ß¤Î¿©»öÎÅÍÜ
           IF   RECE82-SRYYM   >=  "200604"
      *
              PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL   SYUKEI-IDX3    >  31
      *
                 IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                                                    NOT =  ZERO
                 OR   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)
                                                    NOT =  ZERO
                      PERFORM  2305X-RECE84-TENSOU2-SKJ2-SEC
                      PERFORM  2306X-RECE85-HENSYU2-SKJ2-SEC
                      PERFORM  900-RECE84-WRITE-SEC
                      IF  (RECE82-SRYYM                >=  "201604")
                      AND (WRK-SKJKEEP-KBN(SYUKEI-IDX3) =  "2"     )
                         MOVE    1   TO  RECE82-SEISIN-SKJFLG
                      END-IF
                 END-IF
      *
                 IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                   IF    WRK-SKJ200604-DAY-KAN(SYUKEI-IDX3)
                                                    NOT =  ZERO
                      PERFORM  2308-RECE84-TENSOU2-KAN-SEC
                      PERFORM  2309-RECE85-HENSYU2-KAN-SEC
                      PERFORM  900-RECE84-WRITE-SEC
                   END-IF
                 END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       230-RECE84-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ë¿©»ö»»ÄêÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¼èÆÀ
      *****************************************************************
       230A-SKJHKN-GET-SEC                SECTION.
      *
           IF   RECE82-SRYYM  >=  "201710"
             INITIALIZE                   ORCSNYUINDAY-AREA
             PERFORM    VARYING   IDX35   FROM   1   BY   1
                         UNTIL    IDX35    >    31
               MOVE  RECE82-DOUJITSU-HKNCOMBI(IDX35)
                           TO  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX35)
             END-PERFORM
      *
             INITIALIZE                   ORCSSKJHKN-AREA
             MOVE    RECE82-HOSPNUM   TO  LNK-SKJHKN-HOSPNUM
             MOVE    RECE82-PTID      TO  LNK-SKJHKN-PTID
             MOVE    RECE82-SRYYM     TO  LNK-SKJHKN-SANTEIYM
             CALL    "ORCSSKJHKN"     USING   ORCSSKJHKN-AREA
                                              ORCSNYUINDAY-AREA
                                              SPA-AREA
             IF      LNK-SKJHKN-RC    =   ZERO
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
       230A-SKJHKN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»öÊÔ½¸
      *****************************************************************
       230B-SKJ-HENSYU-SEC                SECTION.
      *
           IF  ((WRK-DAY1(IDY)         NOT =  ZERO) OR
                (RECE82-NYUINDAY-KBN(IDY)  =  "1" ))
           AND ( NACCT-ZAITEN              =  ZERO )
           AND ( NACCT-ZAISKBKBN           =  "4"  )
           AND ( NACCT-DAY(IDY)        NOT =  ZERO )
           AND ( WRK-SHOKUJIRYOYO(IDY) NOT =  ZERO )
           AND ((WRK-DAY2(IDY)             =  ZERO) OR
                (WRK-DAY2(IDY)            >=   4  ))
             IF   RECE82-SRYYM  >= "200604"
               IF  NACCT-SRYCDTOTAL = 099999918 OR 099999919
                                                OR 099999920
                 IF    WRK-SKJHKN-HKNCOMBI(IDY)  =  SPACE
                   MOVE  NACCT-DAY(IDY)  TO  WRK-DAY24(IDY)
                   PERFORM  2301-NYUINACCT-SYU-SEC
                 ELSE
                   PERFORM  230C-SKJ-HENSYU2-SEC
                 END-IF
               END-IF
             ELSE
               IF  NACCT-SRYCDTOTAL = 099999913
                 MOVE  NACCT-DAY(IDY)  TO  WRK-DAY24(IDY)
                 PERFORM  2301-NYUINACCT-SYU-SEC
                 ADD    1     TO   CNT-DAY24
                 MOVE   1     TO   FLG-DAY24
               END-IF
             END-IF
           END-IF
           IF  ( RECE82-SRYYM             >=  "200610"     )
           AND ((WRK-DAY1(IDY)         NOT =  ZERO        ) OR
                (RECE82-NYUINDAY-KBN(IDY)  =  "1"         ))
           AND ( NACCT-ZAITEN              =  ZERO         )
           AND ( NACCT-ZAISKBKBN           =  "4"          )
      ***  AND ( NACCT-DAY(IDY)        NOT =  ZERO         )
           AND ( WRK-SHOKUJIRYOYO(IDY) NOT =  ZERO         )
           AND ( NACCT-SRYCDTOTAL = 099999918 OR 099999919
                                              OR 099999920 )
           AND ((WRK-DAY2(IDY)             =  ZERO        ) OR
                (WRK-DAY2(IDY)            >=   4          ))
               PERFORM  2307-NYUINACCT-SYU-KAN-SEC
           END-IF
      *
           .
       230B-SKJ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»öÊÔ½¸
      *****************************************************************
       230C-SKJ-HENSYU2-SEC                SECTION.
      *
           IF  ( RECE82-SRYYM             >=  "200604"     )
           AND ( NACCT-ZAITEN              =  ZERO         )
           AND ( NACCT-ZAISKBKBN           =  "4"          )
           AND ( NACCT-DAY(IDY)        NOT =  ZERO         )
           AND ( WRK-SHOKUJIRYOYO(IDY) NOT =  ZERO         )
           AND ((WRK-DAY2(IDY)             =  ZERO        ) OR
                (WRK-DAY2(IDY)            >=   4          ))
           AND ((WRK-DAY1(IDY)         NOT =  ZERO        ) OR
                (RECE82-NYUINDAY-KBN(IDY)  =  "1"         ))
           AND ( NACCT-SRYCDTOTAL = 099999918 OR 099999919
                                              OR 099999920 )
      *
             IF  ( WRK-SKJHKN-HKNCOMBI(IDY)      = SPACE )
             OR  ((WRK-SKJHKN-HKNCOMBI(IDY)  NOT = SPACE) AND
                  (RECE82-HKNCOMBI           NOT =
                                WRK-SKJHKN-HKNCOMBI(IDY)))
               IF ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (RECE82-HKNCOMBI              =
                                     RECE82-DOUJITSU-HKNCOMBI(IDY)) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)       = SPACE    ))
               OR ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)   NOT = SPACE    ) AND
                   (RECE82-HKNCOMBI              =
                                     WRK-SKJHKN-HKNCOMBI-DOU(IDY) ))
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
             AND  (RECE82-HKNCOMBI               =
                                WRK-SKJHKN-HKNCOMBI(IDY))
               IF ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (RECE82-HKNCOMBI              =
                                     RECE82-DOUJITSU-HKNCOMBI(IDY)) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)       = SPACE    ))
               OR ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)   NOT = SPACE    ) AND
                   (RECE82-HKNCOMBI              =
                                     WRK-SKJHKN-HKNCOMBI-DOU(IDY) ))
               OR ( RECE82-DOUJITSU-HKNCOMBI(IDY)      = ZERO      )
                 MOVE  NACCT-DAY(IDY)  TO  WRK-DAY24(IDY)
                 PERFORM  2301-NYUINACCT-SYU-SEC
               END-IF
      *
               IF ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (RECE82-HKNCOMBI          NOT =
                                     RECE82-DOUJITSU-HKNCOMBI(IDY)) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)       = SPACE    ))
               OR ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                   (WRK-SKJHKN-HKNCOMBI-DOU(IDY)   NOT = SPACE    ) AND
                   (RECE82-HKNCOMBI          NOT =
                                     WRK-SKJHKN-HKNCOMBI-DOU(IDY) ))
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
       230C-SKJ-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×¡ÊÆ±ÆüºÆÆþ±¡Ê¬¡Ë
      **************************************************************
       230D-NYUINACCT-SYU-DOJITU-SEC           SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
             EVALUATE  WRK-SKJ-DOJITU-KBN(IDY)
               WHEN         1
                 MOVE  WRK-DAY4X(IDY)
                                   TO  WRK-SKJ-DOJITU-SHOKUDO(IDY)
                 EVALUATE  RECE82-DOUJITSU-SKJKBN(IDY)
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
                 EVALUATE  RECE82-DOUJITSU-SKJKBN(IDY)
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
       230D-NYUINACCT-SYU-DOJITU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×
      **************************************************************
       2301-NYUINACCT-SYU-SEC           SECTION.
      *
           IF   RECE82-SRYYM  >= "200604"
      *
            IF  ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                 (RECE82-HKNCOMBI         NOT =
                                   RECE82-DOUJITSU-HKNCOMBI(IDY)) AND
                 (WRK-SKJHKN-HKNCOMBI-DOU(IDY)       = SPACE    ))
            OR  ((RECE82-DOUJITSU-HKNCOMBI(IDY)  NOT = ZERO     ) AND
                 (WRK-SKJHKN-HKNCOMBI-DOU(IDY)   NOT = SPACE    ) AND
                 (RECE82-HKNCOMBI         NOT =
                                   WRK-SKJHKN-HKNCOMBI-DOU(IDY) ))
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
           ELSE
      *
             MOVE       ZERO                TO   IDY2
      *
             EVALUATE   WRK-DAY24(IDY)
              WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                  WHEN  1
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
                  WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000710"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000710"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                END-EVALUATE
              WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
              WHEN  3
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
              WHEN  4
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
             END-EVALUATE
      *
           END-IF
      *
           .
       2301-NYUINACCT-SYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î·×»»
      **************************************************************
       23011-NYUINACCT-SYU-KEISAN-SEC           SECTION.
      *
           COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
           IF      RECE82-HKNKBN    =   "4"
           AND     RECE82-SKJKBN    =   "2"
               COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
           END-IF
           ADD      WRK-KEISAN  TO   WRK-SKJ-ZAITEN(IDY)
      *
           .
       23011-NYUINACCT-SYU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤ÎÏ«ºÒ¾è¿ô¥»¥Ã¥È
      **************************************************************
       23012-NYUINACCT-SYU-JYOSU-SEC           SECTION.
      *
           ADD    1             TO   IDY2
           MOVE  "101970010"    TO   WRK-SKJ-SRYCD-OCC (IDY IDY2)
           IF      RECE82-HKNKBN    =   "4"
           AND     RECE82-SKJKBN    =   "2"
      *       ¼«ÇåÀÕ¡Ê£±¡¥£²ÇÜ¡Ë
              ADD    1             TO   IDY2
      *       MOVE  "101970010"    TO   WRK-SKJ-SRYCD-OCC(IDY IDY2)
              MOVE  "099609901"    TO   WRK-SKJ-SRYCD-OCC(IDY IDY2)
           END-IF
      *
           .
       23012-NYUINACCT-SYU-JYOSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2302-RECE84-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE84-ZAINUM
            WHEN    OTHER
             MOVE    NACCT-ZAINUM       TO      RECE84-ZAINUM
           END-EVALUATE
      *
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           IF      NACCT-ZAISKBKBN      =       "7"
      ****    IF   RECE82-HKNKBN        =       "4"
      ****      MOVE     "40"           TO      RECE84-SRYKBN
      ****    ELSE
                MOVE     "80"           TO      RECE84-SRYKBN
      ****    END-IF
              MOVE      SPACE           TO      RECE84-ZAISKBKBN
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    NACCT-ZAITEN         TO      RECE84-ZAITEN
           MOVE    NACCT-SRYCDTOTAL     TO      RECE84-SRYCDTOTAL
           IF      NACCT-ZAISKBKBN      =       "7"
                MOVE    199000510       TO      RECE84-SRYCDTOTAL
           END-IF
           MOVE    NACCT-SURYOUTOTAL    TO      RECE84-SURYOUTOTAL
           MOVE    NACCT-MEISAISU       TO      RECE84-MEISAISU
           MOVE    CNT-DAY21            TO      RECE84-ZAIKAISU
      *****MOVE    NACCT-SYUTEN1        TO      RECE84-SYUTEN1
           MOVE    NACCT-ZAITEN         TO      RECE84-SYUTEN1
           MOVE    NACCT-SYUTEN2        TO      RECE84-SYUTEN2
           MOVE    NACCT-YKZTEN         TO      RECE84-YKZTEN
           MOVE    NACCT-KIZAITEN       TO      RECE84-KIZAITEN
           IF  ((RECE82-HKNKBN    =  "4") AND
                (RECE82-JIBAIKBN  =  "2") AND
                (NACCT-ZAISKBKBN  =  "7"))
           OR  ((RECE82-HKNKBN    =  "6") AND
                (NACCT-ZAISKBKBN  =  "7"))
      *******MOVE  NACCT-SYUTEN1        TO      RECE84-KIZAITEN
             MOVE  NACCT-ZAITEN         TO      RECE84-KIZAITEN
             MOVE  ZERO                 TO      RECE84-SYUTEN1
           END-IF
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-DAY21(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE84-KAISU(1 IDX2)   NOT =  ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2302-RECE84-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2303-RECE85-HENSYU2-SEC               SECTION.
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
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =  1)
                   PERFORM   23031-RECE85-TENSOU2-SEC
                   IF      NACCT-ZAISKBKBN      =       "7"
                      PERFORM   23032-RECE82-HENSYU2-SEC
                   END-IF
                   PERFORM   900-RECE85-WRITE-SEC
      *
                   MOVE    "tbl_nyuinact" TO   MCP-TABLE
                   MOVE    "key9"         TO   MCP-PATHNAME
                   PERFORM  900-NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinact"         TO   MCP-TABLE
           MOVE    "key9"                 TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2303-RECE85-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23031-RECE85-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE85-ZAINUM
            WHEN    OTHER
             MOVE    NSRY-ZAINUM        TO      RECE85-ZAINUM
           END-EVALUATE
      *
           MOVE    NSRY-RENNUM          TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           IF      NACCT-ZAISKBKBN      =       "7"
      ****    IF    RECE82-HKNKBN       =       "4"
      ****       MOVE     "40"          TO      RECE85-SRYKBN
      ****    ELSE
                 MOVE     "80"          TO      RECE85-SRYKBN
      ****    END-IF
              MOVE       "800"          TO      RECE85-SRYSYUKBN
           END-IF
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF      NSRY-SRYCD(IDX2)(1:1)    =   "0"  OR  "1" OR  
                                                  "8"
               ADD     1                   TO  IDX4    
               MOVE    NSRY-SRYCD(IDX2)    TO  RECE85-SRYCD(IDX4)
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     "199000510"     TO  RECE85-SRYCD(IDX4)
               END-IF
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE85-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE85-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE85-INPUTNUM(IDX4)
               MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     199000510       TO  WRK-SRYCD
               END-IF
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM 900-TENSU-TBL-SEC
               PERFORM 25021-RECE85-TENSOU2-KHNX-SEC
               IF    RECE82-HKNKBN    =  "4"
               AND   RECE82-JIBAIKBN  =  "1"
               AND   NACCT-ZAISKBKBN  =  "7"
                   MOVE  1                 TO  RECE85-TENSIKIBETU(IDX4)
                   MOVE  120               TO  RECE85-TEN(IDX4)
               END-IF
               IF    ((NSRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (NSRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( NSRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    NSRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 900-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE85-NAME(IDX4)
               END-IF
               IF    RECE82-HKNKBN    =  "4"
               AND   RECE82-JIBAIKBN  =  "1"
               AND   NACCT-ZAISKBKBN  =  "7"
      *
                 MOVE     1                 TO    WRK-MEISAI-CNT
                 MOVE     TNS-TEN           TO    WRK-TEN
                 MOVE     1                 TO    WRK-SURYOUTOTAL
                 MOVE     199000510         TO    WRK-SRYCDTOTAL
      *
                 MOVE     "2"               TO    RECE85-SIKFLG(IDX4)
               ELSE
                 MOVE     "1"               TO    RECE85-SIKFLG(IDX4)
               END-IF
      *
               MOVE    NSRY-SRYCD(IDX2)  TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     199000510     TO  WRK-SRYCD
               END-IF
               MOVE     ZERO    TO    WRK-FUKA-KINGAKU
               PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                         UNTIL   FUKA-IDX   >    9
                 MOVE   WRK-SRYCD(FUKA-IDX:1)
                                         TO  WRK-FUKA-VALUE
                 IF  WRK-FUKA-VALUE  =  ZERO
                   CONTINUE
                 ELSE
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                          ( WRK-FUKA-VALUE    * WRK-SRYCD)
                   COMPUTE  WRK-FUKA-KINGAKU    =
                            WRK-FUKA-KINGAKU    +
                          ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                              * NSRY-SRYSURYO(IDX2))
                 END-IF
               END-PERFORM
               COMPUTE  RECE84-RUIKEITOTAL  =
                        RECE84-RUIKEITOTAL  +
                        WRK-FUKA-KINGAKU
      *
               MOVE     ZERO    TO    WRK-FUKA-KINGAKU
               PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                         UNTIL   FUKA-IDX   >    8
                 MOVE   NSRY-SRYSURYO(IDX2)(FUKA-IDX:1)
                                         TO  WRK-FUKA-VALUE
                 IF  WRK-FUKA-VALUE  =  ZERO
                   CONTINUE
                 ELSE
                   COMPUTE  WRK-FUKA-KINGAKU  =
                            WRK-FUKA-KINGAKU  +
                          ((WRK-FUKA-VALUE^4) * FUKA-IDX)
                 END-IF
               END-PERFORM
               COMPUTE  RECE84-RUIKEITOTAL  =
                        RECE84-RUIKEITOTAL  +
                        WRK-FUKA-KINGAKU
      *
             END-IF  
           END-PERFORM
      *
           .
       23031-RECE85-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÊÎÅÍÜÃ´Åö¼êÅö¡Ë
      *****************************************************************
       23032-RECE82-HENSYU2-SEC                SECTION.
      *
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           EVALUATE   RECE82-HKNKBN
             WHEN      "1"
             WHEN      "2"
                COMPUTE    RECE82-TENSU(21)  =
                           RECE82-TENSU(21)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
             WHEN      "4"
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************¡Ê¿·ÍÍ¼°¡Ë
                   IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                     COMPUTE    RECE82-KINGAKU3(2)  =
                                RECE82-KINGAKU3(2)  +
                               (120            *   CNT-DAY21)
                   ELSE
                     COMPUTE    RECE82-KINGAKU3(3)  =
                                RECE82-KINGAKU3(3)  +
                               (120            *   CNT-DAY21)
                   END-IF
                  WHEN      "5"
      **************¡Ê½¾ÍèÍÍ¼°¡Ë
                   EVALUATE  RECE82-JIBAIKBN
                    WHEN   "1"
                     COMPUTE    RECE82-SONOTAKIN-JYU  =
                                RECE82-SONOTAKIN-JYU  +
                               (120            *   CNT-DAY21)
                    WHEN   "2"
                     COMPUTE   RECE82-TENSU(28)  =
                               RECE82-TENSU(28)  +
                               (NACCT-ZAITEN   *   CNT-DAY21)
                     COMPUTE   TBL-YKZTEN-JI(28)  =
                               TBL-YKZTEN-JI(28)  +
                               (NACCT-ZAITEN   *   CNT-DAY21)
                   END-EVALUATE
                END-EVALUATE
             WHEN      "5"
                COMPUTE    RECE82-TENSU(37)  =
                           RECE82-TENSU(37)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
             WHEN      "6"
                COMPUTE    RECE82-TENSU(28)  =
                           RECE82-TENSU(28)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
                COMPUTE    TBL-YKZTEN-JI(28)  =
                           TBL-YKZTEN-JI(28)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
           END-EVALUATE
      *
           .
       23032-RECE82-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤ÎÊÔ½¸
      **************************************************************
       2304-NYUINACCT-HEN-SEC           SECTION.
      *
           PERFORM    VARYING  SYUKEI-IDX     FROM    1   BY  1
                  UNTIL  SYUKEI-IDX            >  31
      *
             IF    WRK-SKJ-ZAITEN(SYUKEI-IDX)  NOT =  ZERO
      *
               MOVE     ZERO     TO    FLG-ARI
               PERFORM  VARYING  SYUKEI-IDX2    FROM    1   BY  1
                      UNTIL  SYUKEI-IDX2           > 31
                       OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX2) = ZERO
                       OR    FLG-ARI               = 1
                  IF  ( WRK-SKJ2-ZAITEN   (SYUKEI-IDX2)
                                     =  WRK-SKJ-ZAITEN   (SYUKEI-IDX) )
                  AND ( WRK-SKJ2-SRYCD-T  (SYUKEI-IDX2)
                                     =  WRK-SKJ-SRYCD-T  (SYUKEI-IDX) )
                     ADD   WRK-SKJ-ZAIKAISU(SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAIKAISU (SYUKEI-IDX2)
                     MOVE  1  TO  WRK-SKJ2-KAISU(SYUKEI-IDX2 SYUKEI-IDX)
                     MOVE  1  TO   FLG-ARI
                  END-IF
               END-PERFORM  
               IF  FLG-ARI  =  ZERO  
                     MOVE  WRK-SKJ-ZAITEN   (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAITEN   (SYUKEI-IDX2)
                     ADD   WRK-SKJ-ZAIKAISU (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAIKAISU (SYUKEI-IDX2)
                     MOVE  WRK-SKJ-SRYCD-T  (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-SRYCD-T  (SYUKEI-IDX2)
                     PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL ( SYUKEI-IDX3     >   15 )
                         OR   ( WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3) 
                                                               =  ZERO)
      *
                        ADD   1  TO  WRK-SKJ2-MEISAISU(SYUKEI-IDX2)
                        ADD   WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3)
                                 TO  WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX2)
      *
                     END-PERFORM
                     MOVE  1  TO  WRK-SKJ2-KAISU(SYUKEI-IDX2 SYUKEI-IDX)
               END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
      *
       2304-NYUINACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2305-RECE84-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE84-ZAITEN
           MOVE    WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX3)
                                        TO      RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-MEISAISU
           MOVE    WRK-SKJ2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE84-ZAIKAISU
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                        TO   RECE84-KAISU (1 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE84-KAISU(1 IDX2)   NOT =  ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2305-RECE84-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2305X-RECE84-TENSOU2-SKJ2-SEC                SECTION.
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
           IF  ((RECE82-HKNKBN      =   "4") AND
                (RECE82-JIBAIKBN    =   "2"))
           OR  ((RECE82-HKNKBN      =   "5") AND
                (RECE82-KOMUKBN     =   "2"))
           OR  ( RECE82-HKNKBN      =   "6" )
             CONTINUE
           ELSE
             IF      RECE82-HKNKBN    =   "4"
             AND     RECE82-SKJKBN    =   "2"
                COMPUTE  WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) * 3
             ELSE
                COMPUTE  WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) * 2
             END-IF
           END-IF
      *
           MOVE       ZERO     TO     IDX12
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
            EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
      *
            WHEN      1
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)  NOT =  ZERO
               ADD  197001310 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197001310 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                              TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF      RECE82-HKNKBN    =   "4"
                AND     RECE82-SKJKBN    =   "2"
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                  ADD   099609901
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  099609901
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)  NOT =  ZERO
               ADD  197003510 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197003510 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 4)
                              TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF      RECE82-HKNKBN    =   "4"
                AND     RECE82-SKJKBN    =   "2"
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                  ADD   099609901
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  099609901
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
               ADD  197001570 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197001570 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
               ADD  197001670 TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO  IDX12
               MOVE 197001670 TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                            TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
            WHEN      2
      *
             ADD  197001710 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197001710 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
             OR  ( RECE82-HKNKBN      =   "6" )
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
            END-EVALUATE
           ELSE
            EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
      *
            WHEN      1
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)  NOT =  ZERO
               ADD  197000110 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197000110 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                              TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF      RECE82-HKNKBN    =   "4"
                AND     RECE82-SKJKBN    =   "2"
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                  ADD   099609901
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  099609901
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                 ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)  NOT =  ZERO
               ADD  197003110 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197003110 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 4)
                              TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF      RECE82-HKNKBN    =   "4"
                AND     RECE82-SKJKBN    =   "2"
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                  ADD   099609901
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  099609901
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                 ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
               ADD  197000470 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197000470 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
               ADD  197000570 TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO  IDX12
               MOVE 197000570 TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                            TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
            WHEN      2
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)  NOT =  ZERO
               ADD  197000710 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197000710 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                              TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF      RECE82-HKNKBN    =   "4"
                AND     RECE82-SKJKBN    =   "2"
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                  ADD   099609901
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  099609901
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 4)  NOT =  ZERO
               ADD  197003210 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197003210 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 4)
                              TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
               OR  ( RECE82-HKNKBN      =   "6" )
                CONTINUE
               ELSE
                IF      RECE82-HKNKBN    =   "4"
                AND     RECE82-SKJKBN    =   "2"
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                  ADD   099609901
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  099609901
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                  ADD   101970010
                             TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                  ADD   1    TO  IDX12
                  MOVE  101970010
                             TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                  MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                  ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
            END-EVALUATE
           END-IF
      ***
           MOVE    SPACE                TO  RECE84-REC
           INITIALIZE                       RECE84-REC
           MOVE    RECE82-HOSPNUM       TO  RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE84-PTID
           MOVE    RECE82-RECEKA        TO  RECE84-RECEKA
           MOVE    RECE82-HKNID         TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE84-TEKSTYMD
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
      ***       COMPUTE WRK-ZAINUM  =  99999731  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE84-ZAINUM
           ELSE
      ***       COMPUTE WRK-ZAINUM  =  99999700  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE84-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE84-ZAINUM
           MOVE    "97"                 TO  RECE84-SRYKBN
           MOVE    "4"                  TO  RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO  RECE84-PTNUM
      *
           MOVE    "00"                 TO  RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO  RECE84-HKNCOMBI
           MOVE    SPACE                TO  RECE84-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                                        TO  RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                                        TO  RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ200604-MEISAISU   (SYUKEI-IDX3)
                                        TO  RECE84-MEISAISU
           MOVE    1                    TO  RECE84-ZAIKAISU
      *
           MOVE    ZERO                 TO  RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE84-SYUTEN2
           MOVE    ZERO                 TO  RECE84-YKZTEN
           MOVE    ZERO                 TO  RECE84-KIZAITEN
           MOVE    SPACE                TO  RECE84-DRCD
           MOVE    SPACE                TO  RECE84-ZAIREQFLG
           MOVE    ZERO                 TO  RECE84-TEIGENRATE
           MOVE    1                    TO  RECE84-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
      *
           .
       2305X-RECE84-TENSOU2-SKJ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2306-RECE85-HENSYU2-SKJ-SEC               SECTION.
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
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
           ELSE
               MOVE     1               TO      FLG-NYUINACT
           END-IF
           IF  FLG-NYUINACT   =   ZERO
               MOVE      1      TO     IDX7
               MOVE      1      TO     IDX8
               MOVE      5      TO     IDX9
               PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
               PERFORM   900-RECE85-WRITE-SEC
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 6)  NOT =   ZERO
                  MOVE      2      TO     IDX7
                  MOVE      6      TO     IDX8
                  MOVE     10      TO     IDX9
                  PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   900-RECE85-WRITE-SEC
               END-IF
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 11)  NOT =   ZERO
                  MOVE      3      TO     IDX7
                  MOVE     11      TO     IDX8
                  MOVE     15      TO     IDX9
                  PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   900-RECE85-WRITE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2306-RECE85-HENSYU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23061-RECE85-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID      TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
           MOVE    IDX7                 TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    IDX8    BY   1
                  UNTIL  (IDX2      >    IDX9)    
                    OR   (WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)  =   ZERO)
      *
                ADD     1                    TO    IDX4  
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  RECE85-SRYCD(IDX4)
                MOVE    1                    TO  RECE85-SRYSURYO(IDX4)
                MOVE    ZERO                 TO  RECE85-SRYKAISU(IDX4)
                MOVE    SPACE                TO  RECE85-MEISKYFLG(IDX4)
                MOVE    SPACE                TO  RECE85-AUTOKBN(IDX4)
                MOVE    ZERO                 TO  RECE85-INPUTNUM(IDX4)
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  WRK-SRYCD
                MOVE    RECE85-SRYYM         TO  WK-TAIHI-SRYYM
                MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
                PERFORM 900-TENSU-TBL-SEC
                PERFORM 25021-RECE85-TENSOU2-KHNX-SEC
                MOVE    "1"                  TO  RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       23061-RECE85-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2306X-RECE85-HENSYU2-SKJ2-SEC               SECTION.
      *
           MOVE         1      TO     IDX7
           MOVE         1      TO     IDX8
           MOVE         5      TO     IDX9
           PERFORM   2306X1-RECE85-TENSOU2-SKJ2-SEC
           PERFORM   900-RECE85-WRITE-SEC
      *
           IF   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)  >  5
              MOVE      2      TO     IDX7
              MOVE      6      TO     IDX8
              MOVE      10     TO     IDX9
              PERFORM   2306X1-RECE85-TENSOU2-SKJ2-SEC
              PERFORM   900-RECE85-WRITE-SEC
           END-IF
      *
           IF   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)  >  10
              MOVE      3      TO     IDX7
              MOVE      11     TO     IDX8
              MOVE      12     TO     IDX9
              PERFORM   2306X1-RECE85-TENSOU2-SKJ2-SEC
              PERFORM   900-RECE85-WRITE-SEC
           END-IF
      *
           .
       2306X-RECE85-HENSYU2-SKJ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2306X1-RECE85-TENSOU2-SKJ2-SEC                SECTION.
      *
           MOVE    SPACE                TO  RECE85-REC
           INITIALIZE                       RECE85-REC
           MOVE    RECE82-HOSPNUM       TO  RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE85-PTID
           MOVE    RECE82-RECEKA        TO  RECE85-RECEKA
           MOVE    RECE82-HKNID         TO  RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE85-TEKSTYMD
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
      ***       COMPUTE WRK-ZAINUM  =  99999731  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE85-ZAINUM
           ELSE
      ***       COMPUTE WRK-ZAINUM  =  99999700  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE85-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE85-ZAINUM
           MOVE    IDX7                 TO  RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO  RECE85-PTNUM
      *
           MOVE    "00"                 TO  RECE85-SRYKA
           MOVE    "97"                 TO  RECE85-SRYKBN
           MOVE    "970"                TO  RECE85-SRYSYUKBN
           MOVE    ZERO                 TO  RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO  IDX4
           PERFORM  VARYING  IDX2   FROM   IDX8   BY   1
                     UNTIL  (IDX2     >    IDX9)
                       OR   (WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)
                                                            =   ZERO)
      *
             ADD     1                   TO  IDX4
             MOVE    WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYCD(IDX4)
             MOVE    WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYSURYO(IDX4)
             MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
             MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
             MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
             MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
             MOVE    RECE85-SRYCD(IDX4)  TO  WRK-SRYCD
             MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
             MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
             PERFORM 900-TENSU-TBL-SEC
             PERFORM 25021-RECE85-TENSOU2-KHNX-SEC
             MOVE    "1"                 TO  RECE85-SIKFLG(IDX4)
      *
             IF    WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)  =
                            101970010   OR   099609901
               CONTINUE
             ELSE
              IF  ((RECE82-HKNKBN      =   "4") AND
                   (RECE82-JIBAIKBN    =   "2"))
              OR  ((RECE82-HKNKBN      =   "5") AND
                   (RECE82-KOMUKBN     =   "2"))
              OR  ( RECE82-HKNKBN      =   "6" )
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (TNS-TEN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (TNS-TEN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
              ELSE
               IF    RECE82-HKNKBN    =   "4"
               AND   RECE82-SKJKBN    =   "2"
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
               ELSE
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
               END-IF
              END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       2306X1-RECE85-TENSOU2-SKJ2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×¡Ê´Ä¶­¡Ë
      **************************************************************
       2307-NYUINACCT-SYU-KAN-SEC           SECTION.
      *
           EVALUATE   WRK-SHOKUJIRYOYO(IDY)
             WHEN   1
                MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
             WHEN   2
                MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
           END-EVALUATE
      *
           .
       2307-NYUINACCT-SYU-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2308-RECE84-TENSOU2-KAN-SEC                SECTION.
      *
           ADD     1    TO   WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3)
           IF  ((RECE82-HKNKBN      =   "4") AND
                (RECE82-JIBAIKBN    =   "2"))
           OR  ((RECE82-HKNKBN      =   "5") AND
                (RECE82-KOMUKBN     =   "2"))
           OR  ( RECE82-HKNKBN      =   "6" )
             CONTINUE
           ELSE
             IF      RECE82-HKNKBN    =   "4"
             AND     RECE82-SKJKBN    =   "2"
                COMPUTE  WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) * 3
             ELSE
                COMPUTE  WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) * 2
             END-IF
           END-IF
      ***
           ADD     1    TO   WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
      ***
           MOVE       ZERO     TO     IDX12
           EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
            WHEN      1
             ADD  197001410
                        TO WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
             ADD  1     TO IDX12
             MOVE 197001410
                        TO WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
             MOVE 1     TO WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
             OR  ( RECE82-HKNKBN      =   "6" )
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                ADD   099609901
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  099609901
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              END-IF
             END-IF
            WHEN      2
             ADD  197001810
                        TO WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
             ADD  1     TO IDX12
             MOVE 197001810
                        TO WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
             MOVE 1     TO WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
             OR  ( RECE82-HKNKBN      =   "6" )
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                ADD   099609901
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  099609901
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              END-IF
             END-IF
           END-EVALUATE
      ***
           MOVE    SPACE                TO  RECE84-REC
           INITIALIZE                       RECE84-REC
           MOVE    RECE82-HOSPNUM       TO  RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE84-PTID
           MOVE    RECE82-RECEKA        TO  RECE84-RECEKA
           MOVE    RECE82-HKNID         TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE84-TEKSTYMD
      ***  COMPUTE WRK-ZAINUM  =  99999762  +  SYUKEI-IDX3
           COMPUTE WRK-ZAINUM  =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE84-ZAINUM
           MOVE    "97"                 TO  RECE84-SRYKBN
           MOVE    "4"                  TO  RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO  RECE84-PTNUM
      *
           MOVE    "00"                 TO  RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO  RECE84-HKNCOMBI
           MOVE    SPACE                TO  RECE84-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL-KAN (SYUKEI-IDX3)
                                        TO  RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                                        TO  RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ200604-MEISAISU-KAN   (SYUKEI-IDX3)
                                        TO  RECE84-MEISAISU
           MOVE    1                    TO  RECE84-ZAIKAISU
      *
           MOVE    ZERO                 TO  RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE84-SYUTEN2
           MOVE    ZERO                 TO  RECE84-YKZTEN
           MOVE    ZERO                 TO  RECE84-KIZAITEN
           MOVE    SPACE                TO  RECE84-DRCD
           MOVE    SPACE                TO  RECE84-ZAIREQFLG
           MOVE    ZERO                 TO  RECE84-TEIGENRATE
           MOVE    1                    TO  RECE84-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
      *
           .
       2308-RECE84-TENSOU2-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2309-RECE85-HENSYU2-KAN-SEC               SECTION.
      *
           MOVE         1      TO     IDX7
           MOVE         1      TO     IDX8
           MOVE         3      TO     IDX9
           PERFORM   23091-RECE85-TENSOU2-KAN-SEC
           PERFORM   900-RECE85-WRITE-SEC
      *
           .
       2309-RECE85-HENSYU2-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23091-RECE85-TENSOU2-KAN-SEC                SECTION.
      *
           MOVE    SPACE                TO  RECE85-REC
           INITIALIZE                       RECE85-REC
           MOVE    RECE82-HOSPNUM       TO  RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE85-PTID
           MOVE    RECE82-RECEKA        TO  RECE85-RECEKA
           MOVE    RECE82-HKNID         TO  RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE85-TEKSTYMD
      ***  COMPUTE WRK-ZAINUM   =  99999762  +  SYUKEI-IDX3
           COMPUTE WRK-ZAINUM   =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE85-ZAINUM
           MOVE    IDX7                 TO  RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO  RECE85-PTNUM
      *
           MOVE    "00"                 TO  RECE85-SRYKA
           MOVE    "97"                 TO  RECE85-SRYKBN
           MOVE    "970"                TO  RECE85-SRYSYUKBN
           MOVE    ZERO                 TO  RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO  IDX4
           PERFORM  VARYING  IDX2   FROM   IDX8   BY   1
                     UNTIL  (IDX2     >    IDX9)
                       OR   (WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)
                                                            =   ZERO)
      *
             ADD     1                   TO  IDX4
             MOVE    WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYCD(IDX4)
             MOVE    WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYSURYO(IDX4)
             MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
             MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
             MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
             MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
             MOVE    RECE85-SRYCD(IDX4)  TO  WRK-SRYCD
             MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
             MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
             PERFORM 900-TENSU-TBL-SEC
             PERFORM 25021-RECE85-TENSOU2-KHNX-SEC
             MOVE    "1"                 TO  RECE85-SIKFLG(IDX4)
      *
             IF    WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)  =
                            101970010   OR   099609901
               CONTINUE
             ELSE
              IF  ((RECE82-HKNKBN      =   "4") AND
                   (RECE82-JIBAIKBN    =   "2"))
              OR  ((RECE82-HKNKBN      =   "5") AND
                   (RECE82-KOMUKBN     =   "2"))
              OR  ( RECE82-HKNKBN      =   "6" )
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (TNS-TEN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (TNS-TEN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
              ELSE
               IF    RECE82-HKNKBN    =   "4"
               AND   RECE82-SKJKBN    =   "2"
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
               ELSE
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
               END-IF
              END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       23091-RECE85-TENSOU2-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       240-RECE84-HENSYU3-SEC                SECTION.
      *
      *    ÅÀÅ©¤Î·×»»
      *
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    RECE82-PTID         TO  ORCSTEN-PTID
           MOVE    RECE82-SRYYM        TO  ORCSTEN-SRYYM
           MOVE    RECE82-HKNCOMBI     TO  ORCSTEN-HKNCOMBI
      ***  MOVE    RECE82-HOSPNUM      TO  SPA-HOSPNUM
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
      *    IF      ORCSTEN-ERRCD       =   ZERO
      *
      *           ÌôºÞ
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                              UNTIL   IDY     >       31
      *
                    MOVE   SPACE  TO   WRK-FORMALNAME
                    MOVE   ZERO   TO   FLG-SETCOM
                    MOVE   ZERO   TO   FLG-HENSYU-OK
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
                          PERFORM  VARYING  IDX13   FROM    1   BY  1
                            UNTIL  IDX13    >      5
                              OR   ORCSTEN-COM-SRYCD(IDY IDX5 IDX13)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-COM-MEI(IDY IDX5 IDX13)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
      *
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      350  TO   IDX5
                       PERFORM   2402-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
      *
                    END-IF
      *
                  END-PERFORM
      *
      *           ´ïºà
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                              UNTIL   IDY     >       50
                                OR    ORCSTEN-KIZ-ZAITEN(IDY)  =  ZERO
      *
                       MOVE   SPACE  TO   WRK-FORMALNAME
                       MOVE   ZERO   TO   FLG-SETCOM
                       MOVE   ZERO   TO   FLG-HENSYU-OK
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
                          PERFORM  VARYING  IDX13   FROM    1   BY  1
                            UNTIL  IDX13    >      5
                              OR   ORCSTEN-KIZ-COM-SRYCD(IDY IDX5 IDX13)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-KIZ-COM-MEI(IDY IDX5 IDX13)
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
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      60   TO   IDX5
                       PERFORM   2402-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
      *
                  END-PERFORM
      *
      *    END-IF
      *
      ***
      *
      *    ÅÀÅ©¼êµ»²Ã»»
      *
           IF   WRK-TENTEKI2-HENKYAKUARI   NOT =  ZERO
      *
               IF   WRK-TENTEKI2-T-ERRCD       =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    MOVE    ZERO     TO   FLG-SETCOM
                    MOVE    ZERO     TO   FLG-HENSYU-OK
                    IF  WRK-TENTEKI2-T-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      3   TO   FLG-SIKIBETU
                       PERFORM   2401X-RECE84-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-T-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   2404-RECE85-TENTEKI2-HENSYU-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
                    END-IF
                 END-PERFORM
               END-IF
      *
               IF   WRK-TENTEKI2-C-ERRCD       =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    MOVE    ZERO     TO   FLG-SETCOM
                    MOVE    ZERO     TO   FLG-HENSYU-OK
                    IF  WRK-TENTEKI2-C-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      4   TO   FLG-SIKIBETU
                       PERFORM   2401X-RECE84-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-C-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   2404-RECE85-TENTEKI2-HENSYU-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
                    END-IF
                 END-PERFORM
               END-IF
      *
           END-IF
      *
           .
       240-RECE84-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2401-RECE84-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           IF    FLG-SIKIBETU  =  1
             COMPUTE WRK-ZAINUM    =  99997000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99997400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    "33"                 TO      RECE84-SRYKBN
           MOVE    SPACE                TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-YK-SRYKA(IDY)
                                        TO      RECE84-SRYKA
           ELSE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL   IDX2   >   31
               IF   WRK-ORCSTEN-DAY(IDX2)  NOT =   ZERO
                  MOVE  WRK-ORCSTEN-KIZ-SRYKA(IDX2)
                                        TO      RECE84-SRYKA
                  MOVE  31              TO      IDX2
               END-IF
             END-PERFORM
           END-IF
      *
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-ZAITEN
           ELSE
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)  
                                        TO      RECE84-ZAITEN
           END-IF
           MOVE    ZERO                 TO      RECE84-SRYCDTOTAL
           MOVE    ZERO                 TO      RECE84-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE84-MEISAISU
           IF    FLG-SIKIBETU  =  1
             MOVE  1                    TO      RECE84-ZAIKAISU
           ELSE
             MOVE  WRK-ORCSTEN-ZAIKAISU TO      RECE84-ZAIKAISU
           END-IF
           MOVE    ZERO                 TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-YKZTEN
             MOVE  ZERO                 TO      RECE84-KIZAITEN
           ELSE
             MOVE  ZERO                 TO      RECE84-YKZTEN
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE84-KIZAITEN
           END-IF
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           IF    FLG-SIKIBETU  =  1
             MOVE    1                  TO      RECE84-KAISU (1 IDY)
             MOVE    IDY                TO      WK-TAIHI-DAY
           ELSE
             MOVE        ZERO           TO      WK-TAIHI-DAY
             MOVE        ZERO           TO      WK-TAIHI-FLG
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-ORCSTEN-DAY(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE84-KAISU(1 IDX2)   NOT =  ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
             END-PERFORM
           END-IF
           .
       2401-RECE84-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2401X-RECE84-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    "33"                 TO      RECE84-SRYKBN
           MOVE    SPACE                TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           IF    FLG-SIKIBETU  =  3
             MOVE  WRK-TENTEKI2-T-SRYKA(IDY)
                                        TO      RECE84-SRYKA
           ELSE
             MOVE  WRK-TENTEKI2-C-SRYKA(IDY)
                                        TO      RECE84-SRYKA
           END-IF
      *
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           IF    FLG-SIKIBETU  =  3
             MOVE  WRK-TENTEKI2-T-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
                                                RECE84-SYUTEN1
           ELSE
             MOVE  WRK-TENTEKI2-C-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
                                                RECE84-SYUTEN1
           END-IF
           MOVE    1                    TO      RECE84-ZAIKAISU
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           MOVE    1                    TO      RECE84-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       2401X-RECE84-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2402-RECE85-TENSOU3-SEC                SECTION.
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
              MOVE    SPACE                TO   RECE85-REC
              INITIALIZE                        RECE85-REC
              MOVE    RECE82-HOSPNUM       TO   RECE85-HOSPNUM
              MOVE    RECE82-SRYYM         TO   RECE85-SRYYM
              MOVE    RECE82-NYUGAIKBN     TO   RECE85-NYUGAIKBN
              MOVE    RECE82-PTID          TO   RECE85-PTID
              MOVE    RECE82-RECEKA        TO   RECE85-RECEKA
              MOVE    RECE82-HKNID         TO   RECE85-HKNID
              MOVE    RECE82-TEKSTYMD      TO   RECE85-TEKSTYMD
              IF    FLG-SIKIBETU  =  1
                COMPUTE WRK-ZAINUM    =  99997000  +  IDY
              ELSE
                COMPUTE WRK-ZAINUM    =  99997400  +  IDY
              END-IF
              MOVE    WRK-ZAINUM           TO   RECE85-ZAINUM
              ADD     1                    TO   WRK-RENNUM
              MOVE    WRK-RENNUM           TO   RECE85-RENNUM
              MOVE    RECE82-PTNUM         TO   RECE85-PTNUM
      *
              MOVE    RECE84-SRYKA         TO   RECE85-SRYKA
              MOVE    "33"                 TO   RECE85-SRYKBN
              MOVE    "330"                TO   RECE85-SRYSYUKBN
              MOVE    ZERO                 TO   RECE85-JIHIMONEY
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
                                            TO  RECE85-SRYCD(IDX4)
                  MOVE  WRK-ORCSTEN-SURYO(IDX2) 
                                            TO  RECE85-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE85-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE85-MEISKYFLG(IDX4)
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)  =  "099309901"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"    )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008"  )
                    MOVE  "2"               TO  RECE85-MEISKYFLG(IDX4)
                  END-IF
                  MOVE  SPACE               TO  RECE85-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE85-INPUTNUM(IDX4)
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  WRK-SRYCD
                  MOVE  RECE85-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  "099309901"
                    IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO
                      COMPUTE  WRK-SRYSURYO  =  WRK-SRYSURYO2
                                             -  RECE85-SRYSURYO(IDX4)
                      MOVE   WRK-TANICD     TO  RECE85-TANICD(IDX4)
                      MOVE   WRK-TANIMOJISU TO  RECE85-TANIMOJISU(IDX4)
                      MOVE   WRK-TANINAME   TO  RECE85-TANINAME(IDX4)
                    ELSE
                      MOVE   "1"            TO  RECE85-RECEDENFLG(IDX4)
                      MOVE   "1"            TO  RECE85-RECEPTFLG(IDX4)
                    END-IF
                  ELSE
                      IF      TNS-TANIMOJISU    =   ZERO
                      AND     TNS-TANINAME  NOT =   SPACE
                         PERFORM   VARYING   IDW   FROM   24   BY  -1
                                    UNTIL    IDW    <      1
                          IF   TNS-TANINAME(IDW:1)  NOT =   SPACE
                              COMPUTE  TNS-TANIMOJISU   =   IDW   /   2
                              MOVE    1     TO    IDW
                          END-IF
                         END-PERFORM
                      END-IF
                      COMPUTE WRK-SRYSURYO2  =  WRK-ORCSTEN-SURYO(IDX2)
                                             +  0.99999
                      IF   TNS-TANICD  NOT = SPACE AND "000"
                       MOVE TNS-TANICD      TO  RECE85-TANICD(IDX4)
                                                WRK-TANICD
                      END-IF
                      MOVE  TNS-TANIMOJISU  TO  RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU
                      MOVE  TNS-TANINAME    TO  RECE85-TANINAME(IDX4)
                                                WRK-TANINAME
                  END-IF
                  MOVE  TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
                  MOVE  TNS-TEN             TO  RECE85-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE85-YUKOKETA(IDX4)
                  ELSE
                     MOVE   TNS-YUKOKETA    TO  RECE85-YUKOKETA(IDX4)
                  END-IF
                  MOVE  TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM    
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM  
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                  MOVE  TNS-SRYSYUKBN TO  RECE85-SRYSYUKBN-X  (IDX4)
                  MOVE  TNS-KZMCOMPSIKIBETU
                                      TO  RECE85-KZMCOMPSIKIBETU(IDX4)
                  MOVE  TNS-KNSJISGRPKBN
                                      TO  RECE85-KNSJISGRPKBN (IDX4)
                  MOVE  TNS-KENTAICOMMENT
                                      TO  RECE85-KENTAICOMMENT(IDX4)
                  MOVE  TNS-KOKUJISKBKBN1
                                      TO  RECE85-KOKUJISKBKBN1(IDX4)
                  MOVE  TNS-KOKUJISKBKBN2
                                      TO  RECE85-KOKUJISKBKBN2(IDX4)
                  MOVE  TNS-TUSOKUGAIKBN
                                      TO  RECE85-TUSOKUGAIKBN (IDX4)
                  MOVE  TNS-TIMEKSNKBN
                                      TO  RECE85-TIMEKSNKBN   (IDX4)
                  MOVE  TNS-CHUKSNCD  TO  RECE85-CHUKSNCD     (IDX4)
                  MOVE  TNS-CHUKSNTUBAN
                                      TO  RECE85-CHUKSNTUBAN  (IDX4)
                  MOVE  TNS-TOKUKIZAISBT1
                                      TO  RECE85-TOKUKIZAISBT1(IDX4)
                  MOVE  TNS-SANSOKBN  TO  RECE85-SANSOKBN     (IDX4)
                  IF      TNS-SRYCD (1:2)   =   "84"
                      PERFORM VARYING IDYY    FROM    1   BY  1
                              UNTIL   IDYY    >       10
                          MOVE    TNS-SSTKIJUNCD (IDYY)
                                      TO  RECE85-SSTKIJUNCD(IDX4 IDYY)
                      END-PERFORM
                  END-IF
      *
                  MOVE     "1"              TO  RECE85-SIKFLG(IDX4)
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   WRK-ORCSTEN-COM-MEI(IDX2)
                                      TO RECE85-NAME(IDX4)
                                         RECE85-INPUTCOMENT(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO RECE85-NAME(IDX4)
                                         RECE85-INPUTCOMENT(IDX4)
                  END-IF
      *
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  "099309901"
                     IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO
                       PERFORM  220215-RECE85-SURYO-HEN-SEC
      *
                       MOVE    SPACE      TO  RECE85-NAME(IDX4)
                       MOVE    SPACE      TO  RECE85-INPUTCOMENT(IDX4)
      ***              STRING  TNS-NAME          DELIMITED  BY  " "
      ***                      "¡¡"              DELIMITED  BY  SIZE
      ***                      WRK-SRYSURYOW     DELIMITED  BY  " "
      ***                      RECE85-TANINAME(IDX4)
      ***                                        DELIMITED  BY  " "
      ***                      INTO              RECE85-NAME(IDX4)
      ***              END-STRING
                       STRING  "¡Ê»ÈÍÑÎÌ¡¡"      DELIMITED  BY  SIZE
                               WRK-SRYSURYOW     DELIMITED  BY  " "
                               RECE85-TANINAME(IDX4)
                                                 DELIMITED  BY  " "
                               "¡¡»ÄÎÌÇÑ´þ¡Ë"    DELIMITED  BY  SIZE
                               INTO              RECE85-NAME(IDX4)
                       END-STRING
                       MOVE  RECE85-NAME(IDX4)
                                          TO  RECE85-INPUTCOMENT(IDX4)
      *
      ***              MOVE  "810000001"  TO  RECE85-SRYCD(IDX4)
                       MOVE  ZERO         TO  RECE85-SRYSURYO(IDX4)
                       MOVE  ZERO         TO  RECE85-TANICD(IDX4)
                       MOVE  ZERO         TO  RECE85-TANIMOJISU(IDX4)
                       MOVE  SPACE        TO  RECE85-TANINAME(IDX4)
                       MOVE  ZERO         TO  RECE85-YUKOKETA(IDX4)
                     END-IF
                  END-IF
      *
                  IF   WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "058"
                    MOVE  TNS-FORMALYUKOKETA TO  RECE85-YUKOKETA(IDX4)
                    MOVE  TNS-FORMALNAME     TO  RECE85-NAME(IDX4)
                                                 WRK-FORMALNAME
                    MOVE  "1"                TO  RECE85-RECEDENFLG(IDX4)
      *
                    MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                    IF   TNS-TANICD  NOT = SPACE AND "000"
                       MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                       MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                       MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                    END-IF
                    MOVE     ZERO            TO  RECE85-TEN(IDX4)
                    MOVE     ZERO            TO  RECE85-TANICD(IDX4)
                    MOVE     ZERO            TO  RECE85-TANIMOJISU(IDX4)
                    MOVE     SPACE           TO  RECE85-TANINAME(IDX4)
                  END-IF
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7")
                  AND (WRK-FORMALNAME     NOT =  SPACE      )
                    MOVE  WRK-FORMALNAME  TO  RECE85-RECEDENNAME(IDX4)
                    MOVE  "1"             TO  RECE85-RECEHENFLG(IDX4)
                    MOVE  SPACE           TO  WRK-FORMALNAME
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                          TO  RECE85-TEN(IDX4)
                    END-IF
                    IF   (RECE85-TANICD(IDX4)    = ZERO)
                    AND  (WRK-KZI058-TANICD  NOT = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                          TO  RECE85-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                          TO  RECE85-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                          TO  RECE85-TANINAME(IDX4)
                    END-IF
                  END-IF
      *
                  IF   RECE85-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   300   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE84-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                              +  WRK-ORCSTEN-SURYO(IDX2)
                  COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU
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
                      COMPUTE  RECE84-RUIKEITOTAL  =
                               RECE84-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                   * WRK-ORCSTEN-SURYO(IDX2))
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE84-RUIKEITOTAL  =
                           RECE84-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
      *           ÆÃÄê´ïºà¤ÎÃ±²ÁÊÔ½¸
                  IF      WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7"
                    IF  WRK-ORCSTEN-SRYCD(IDX2)       =
                                       "799990070"            OR
                                       "770020070"            OR
                                       "770030070"
                      CONTINUE
                    ELSE
                      IF      RECE85-TEN(IDX4)  =   ZERO
                        CONTINUE
                      ELSE
                        PERFORM 220213-RECE85-TANKA-HEN-SEC
                        COMPUTE IDW =   RECE85-YUKOKETA(IDX4)  *  2
                                    +   3
                        MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                   TO   RECE85-NAME(IDX4)(IDW:)
                        COMPUTE RECE85-YUKOKETA(IDX4)  =
                                RECE85-YUKOKETA(IDX4)  +
                                  1                    +
                               (WRK-CHAR-N-CNT  /  2)
                      END-IF
                    END-IF
                  END-IF
      **
                  IF   RECE85-MEISKYFLG(IDX4)  =  "2"
                     IF   (FLG-SETCOM              =  1   )
                     AND  (RECE85-RECEDENFLG(IDX4) = SPACE)
                        MOVE  "1"            TO  RECE84-SETCOMFLG
                     ELSE
                        MOVE  SPACE          TO  RECE85-MEISKYFLG(IDX4)
                     END-IF
                  ELSE
                     IF   (RECE85-SRYCD(IDX4)(1:1) = "1" OR "6" OR "7")
                     OR   (RECE85-SRYCD(IDX4)(1:3) = "059"            )
                        MOVE   1             TO  FLG-SETCOM
                     END-IF
                  END-IF
      **
                  IF      FLG-HENSYU-OK      =     ZERO
                    PERFORM  220217-TENTEKI-CDKBN-HEN-SEC
                    MOVE     1     TO    FLG-HENSYU-OK
                  END-IF
      *
                END-IF  
      *
                ADD        1                TO  IDX2
      *
              END-PERFORM
      *
              PERFORM   900-RECE85-WRITE-SEC
      *
           END-PERFORM
      *
           .
       2402-RECE85-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2402X-RECE85-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO   RECE85-REC
           INITIALIZE                        RECE85-REC
           MOVE    RECE82-HOSPNUM       TO   RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO   RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO   RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO   RECE85-PTID
           MOVE    RECE82-RECEKA        TO   RECE85-RECEKA
           MOVE    RECE82-HKNID         TO   RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO   RECE85-TEKSTYMD
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO   RECE85-ZAINUM
           ADD     1                    TO   WRK-RENNUM
           MOVE    WRK-RENNUM           TO   RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO   RECE85-PTNUM
      *
           MOVE    RECE84-SRYKA         TO   RECE85-SRYKA
           MOVE    "33"                 TO   RECE85-SRYKBN
           IF    FLG-SIKIBETU  =  3
              MOVE    "330"             TO   RECE85-SRYSYUKBN
           ELSE
              MOVE    "340"             TO   RECE85-SRYSYUKBN
           END-IF
           MOVE    ZERO                 TO   RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO   IDX4
           PERFORM   UNTIL  (WRK-TENTEKI2-SRYCD2(IDX2)  =   SPACE)
                      OR    (IDX4                      >=     5  )
                      OR    (IDX2                      >    100  )
      *
                  ADD   1                   TO  IDX4
      *
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)
                                            TO  RECE85-SRYCD(IDX4)
                  MOVE  1                   TO  RECE85-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE85-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE85-MEISKYFLG(IDX4)
                  IF   WRK-TENTEKI2-COM-FLG2(IDX2) = "1"
                    MOVE  "2"               TO  RECE85-MEISKYFLG(IDX4)
                  END-IF
                  MOVE  SPACE               TO  RECE85-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE85-INPUTNUM(IDX4)
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)
                                            TO  WRK-SRYCD
                  MOVE  RECE85-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM 900-TENSU-TBL-SEC
                  IF      TNS-TANIMOJISU    =   ZERO
                  AND     TNS-TANINAME  NOT =   SPACE
                     PERFORM   VARYING   IDW   FROM   24   BY  -1
                                UNTIL    IDW    <      1
                      IF   TNS-TANINAME(IDW:1)  NOT =   SPACE
                          COMPUTE  TNS-TANIMOJISU   =   IDW   /   2
                          MOVE    1     TO    IDW
                      END-IF
                     END-PERFORM
                  END-IF
                  PERFORM 25021-RECE85-TENSOU2-KHNX-SEC
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE85-YUKOKETA(IDX4)
                  END-IF
                  IF      TNS-SRYCD (1:2)   =   "84"
                      PERFORM VARYING IDYY    FROM    1   BY  1
                              UNTIL   IDYY    >       10
                          MOVE    TNS-SSTKIJUNCD (IDYY)
                                      TO  RECE85-SSTKIJUNCD(IDX4 IDYY)
                      END-PERFORM
                  END-IF
                  MOVE     "1"        TO  RECE85-SIKFLG(IDX4)
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                     MOVE   WRK-TENTEKI2-COM-MOJI2(IDX2)
                                      TO  RECE85-NAME(IDX4)
                                          RECE85-INPUTCOMENT(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO  RECE85-INPUTCOMENT(IDX4)
                  END-IF
      *
                  IF   RECE85-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   300   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-TENTEKI2-SRYCD2(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-TENTEKI2-SRYCD2(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE84-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-TENTEKI2-SRYCD2(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                              +  1
                  COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU
                                              +  1
      *
                  MOVE     ZERO       TO  WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-TENTEKI2-SRYCD2(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE84-RUIKEITOTAL  =
                               RECE84-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                 * 1)
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE84-RUIKEITOTAL  =
                           RECE84-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
                  IF   RECE85-MEISKYFLG(IDX4)  =  "2"
                     IF   (FLG-SETCOM              =  1   )
                     AND  (RECE85-RECEDENFLG(IDX4) = SPACE)
                        MOVE  "1"            TO  RECE84-SETCOMFLG
                     ELSE
                        MOVE  SPACE          TO  RECE85-MEISKYFLG(IDX4)
                     END-IF
                  ELSE
                     IF   (RECE85-SRYCD(IDX4)(1:1) = "1" OR "6" OR "7")
                     OR   (RECE85-SRYCD(IDX4)(1:3) = "059"            )
                        MOVE   1             TO  FLG-SETCOM
                     END-IF
                  END-IF
      *
                  IF      FLG-HENSYU-OK      =     ZERO
                    PERFORM  220217-TENTEKI-CDKBN-HEN-SEC
                    MOVE     1     TO    FLG-HENSYU-OK
                  END-IF
      *
                  ADD        1               TO  IDX2
      *
           END-PERFORM
      *
           IF     IDX4    NOT =  ZERO
              PERFORM   900-RECE85-WRITE-SEC
           END-IF
      *
           .
       2402X-RECE85-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2403-RECE82-HENSYU3-SEC               SECTION.
      *
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           EVALUATE   RECE82-HKNKBN
            WHEN       "1"
            WHEN       "2"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
             COMPUTE   RECE82-TENSU(12)  =
                       RECE82-TENSU(12)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
             COMPUTE   WRK-RECE8C-TENSU(12)  =
                       WRK-RECE8C-TENSU(12)  +
                       (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
            WHEN       "4"
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************¡Ê¿·ÍÍ¼°¡Ë
                   IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                    ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(11)
      *
                    IF    FLG-SIKIBETU  =  3 OR 4
                       COMPUTE   RECE82-TENSU(11)  =
                                 RECE82-TENSU(11)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    ELSE
                       COMPUTE   RECE82-TENSU-YAK(12)  =
                                 RECE82-TENSU-YAK(12)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   WRK-RECE8C-TENSU-YAK(12)  =
                                 WRK-RECE8C-TENSU-YAK(12)  +
                                 (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                    END-IF
                   ELSE
                    ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
                    IF    FLG-SIKIBETU  =  3 OR 4
                       COMPUTE   RECE82-TENSU(12)  =
                                 RECE82-TENSU(12)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    ELSE
                       COMPUTE   RECE82-TENSU-YAK(13)  =
                                 RECE82-TENSU-YAK(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   WRK-RECE8C-TENSU-YAK(13)  =
                                 WRK-RECE8C-TENSU-YAK(13)  +
                                 (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                    END-IF
                   END-IF
                  WHEN      "5"
      **************¡Ê½¾ÍèÍÍ¼°¡Ë
                    ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(13)
      *
                    IF    FLG-SIKIBETU  =  3 OR 4
                       COMPUTE   RECE82-TENSU(13)  =
                                 RECE82-TENSU(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(13)  =
                                 TBL-SYUTEN-JI(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    ELSE
                       COMPUTE   RECE82-TENSU(13)  =
                                 RECE82-TENSU(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(13) =
                                 TBL-YKZTEN-JI(13) +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   WRK-RECE8C-TENSU(13)  =
                                 WRK-RECE8C-TENSU(13)  +
                                 (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI-C(13)  =
                                 TBL-YKZTEN-JI-C(13)  +
                                 (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                    END-IF
                END-EVALUATE
            WHEN       "5"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(27)
      *
             COMPUTE   RECE82-TENSU(27)  =
                       RECE82-TENSU(27)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
             COMPUTE   WRK-RECE8C-TENSU(27)  =
                       WRK-RECE8C-TENSU(27)  +
                       (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
            WHEN       "6"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(13)
      *
             IF    FLG-SIKIBETU  =  3 OR 4
                COMPUTE   RECE82-TENSU(13)  =
                          RECE82-TENSU(13)  +
                          (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                COMPUTE   TBL-SYUTEN-JI(13)  =
                          TBL-SYUTEN-JI(13)  +
                          (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
             ELSE
                COMPUTE   RECE82-TENSU(13)  =
                          RECE82-TENSU(13)  +
                          (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                COMPUTE   TBL-YKZTEN-JI(13) =
                          TBL-YKZTEN-JI(13) +
                          (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                COMPUTE   WRK-RECE8C-TENSU(13)  =
                          WRK-RECE8C-TENSU(13)  +
                          (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
                COMPUTE   TBL-YKZTEN-JI-C(13)  =
                          TBL-YKZTEN-JI-C(13)  +
                          (RECE84-YKZTEN  *  RECE84-ZAIKAISU)
             END-IF
           END-EVALUATE
           .                                           
      *
       2403-RECE82-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2404-RECE85-TENTEKI2-HENSYU-SEC                SECTION.
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
              PERFORM   2402X-RECE85-TENSOU3-SEC
      *
           END-PERFORM
      *
           .
       2404-RECE85-TENTEKI2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       250-RECE84-HENSYU4-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KHN
                                           WRK-SROUNYUIN-SRYCDTOTAL
                                           WRK-SROUNYUIN-MEISAISU
                                           WRK-SROUNYUIN-SRYCD
                                           WRK-SROUNYUIN-SRYCD2
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    RECE82-HOSPNUM      TO  LNK-SROUNYUIN-HOSPNUM
           MOVE    RECE82-PTID         TO  LNK-SROUNYUIN-PTID
           MOVE    RECE82-SRYYM        TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    RECE82-HKNCOMBI     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    SPACE               TO  LNK-SROUNYUIN-RYAKUKBN
           IF   (RECE82-HKNKBN  =  "6"    )
           AND  (RECE82-SRYYM  >= "201604")
             MOVE  "1"                 TO  LNK-SROUNYUIN-DAISANFLG
           END-IF
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
                                               SPA-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                          UNTIL    IDY    >    31
      *
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDY)    =  "1"
                AND  LNK-SROUNYUIN-SRYCD(IDY 1)  NOT =  SPACE
      *
                   MOVE     ZERO        TO    WRK-RENNUM
      *
                   IF   LNK-SROUNYUIN-GAIHAKU(IDY)  =  ZERO
      *
                     MOVE     1     TO    FLG-KHN
      *
                     PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                     MOVE     1     TO    SYUKEI-IDX
                     MOVE     5     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE     6     TO    SYUKEI-IDX
                     MOVE    10     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    11     TO    SYUKEI-IDX
                     MOVE    15     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    16     TO    SYUKEI-IDX
                     MOVE    20     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     PERFORM   900-RECE84-WRITE-SEC
      *
                   ELSE
      *
                     MOVE     2     TO    FLG-KHN
      *
                     EVALUATE  LNK-SROUNYUIN-SENTEI-FLG(IDY)
                       WHEN      SPACE
      *
                         MOVE      LNK-SROUNYUIN-SRYCD(IDY 1)
                                      TO    WRK-SROUNYUIN-SRYCD
                         COMPUTE   WRK-SROUNYUIN-SRYCDTOTAL      =
                                   LNK-SROUNYUIN-SRYCDTOTAL(IDY) -
                                   WRK-SROUNYUIN-SRYCD
                         COMPUTE   WRK-SROUNYUIN-MEISAISU        =
                                   LNK-SROUNYUIN-MEISAISU(IDY)   -
                                   1
      *
                         PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                         MOVE      2     TO    SYUKEI-IDX
                         MOVE      6     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
                         MOVE      7     TO    SYUKEI-IDX
                         MOVE     11     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
                         MOVE     12     TO    SYUKEI-IDX
                         MOVE     16     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
                         MOVE     17     TO    SYUKEI-IDX
                         MOVE     20     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
      *
                         PERFORM   900-RECE84-WRITE-SEC
      *
                       WHEN      "1"
      *
                         MOVE      LNK-SROUNYUIN-SRYCD(IDY 1)
                                      TO    WRK-SROUNYUIN-SRYCD
                         MOVE      LNK-SROUNYUIN-SRYCD(IDY 2)
                                      TO    WRK-SROUNYUIN-SRYCD2
                         COMPUTE   WRK-SROUNYUIN-SRYCDTOTAL      =
                                   LNK-SROUNYUIN-SRYCDTOTAL(IDY) -
                                   WRK-SROUNYUIN-SRYCD           -
                                   WRK-SROUNYUIN-SRYCD2
                         COMPUTE   WRK-SROUNYUIN-MEISAISU        =
                                   LNK-SROUNYUIN-MEISAISU(IDY)   -
                                   2
      *
                         PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                         MOVE      3     TO    SYUKEI-IDX
                         MOVE      7     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
                         MOVE      8     TO    SYUKEI-IDX
                         MOVE     12     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
                         MOVE     13     TO    SYUKEI-IDX
                         MOVE     17     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
                         MOVE     18     TO    SYUKEI-IDX
                         MOVE     20     TO    SYUKEI-IDX2
                         IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)
                                                         NOT = SPACE
                            PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                            PERFORM   900-RECE85-WRITE-SEC
                         END-IF
      *
                         PERFORM   900-RECE84-WRITE-SEC
      *
                     END-EVALUATE
      *
                   END-IF
      *
                END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       250-RECE84-HENSYU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2501-RECE84-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM  =  99999800  +  IDY
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
      *
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE84-SRYKBN
           IF      FLG-KHN  =  1
              MOVE    "1"               TO      RECE84-ZAISKBKBN
           ELSE
              MOVE    "2"               TO      RECE84-ZAISKBKBN
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
           IF      FLG-KHN  =  1
              MOVE    LNK-SROUNYUIN-SRYCDTOTAL(IDY)
                                        TO      RECE84-SRYCDTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-SURYOUTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-MEISAISU
           ELSE
              MOVE    WRK-SROUNYUIN-SRYCDTOTAL
                                        TO      RECE84-SRYCDTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-SURYOUTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-MEISAISU
           END-IF
           MOVE    1                    TO      RECE84-ZAIKAISU
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           MOVE    1                    TO      RECE84-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       2501-RECE84-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2502-RECE85-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM   =   99999800   +   IDY
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
      *
           ADD     1                    TO      WRK-RENNUM
           MOVE    WRK-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    "00"                 TO      RECE85-SRYKA
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE85-SRYKBN
           IF      LNK-SROUNYUIN-SRYKBN(IDY) =  "90"
              MOVE    "900"             TO      RECE85-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE85-SRYSYUKBN
           END-IF 
           MOVE    ZERO                 TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM   SYUKEI-IDX   BY   1
               UNTIL (IDX2      >    SYUKEI-IDX2)
                OR   (LNK-SROUNYUIN-SRYCD(IDY IDX2)  =   SPACE)
      *
               ADD     1                   TO  IDX4    
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  RECE85-SRYCD(IDX4)
               MOVE    1                   TO  RECE85-SRYSURYO(IDX4)
               MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
               MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  WRK-SRYCD
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM 900-TENSU-TBL-SEC
               PERFORM 25021-RECE85-TENSOU2-KHNX-SEC
               MOVE    "1"                 TO  RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       2502-RECE85-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       25021-RECE85-TENSOU2-KHNX-SEC                SECTION.
      *
           MOVE  TNS-TANICD           TO  RECE85-TANICD(IDX4)
           MOVE  TNS-TANIMOJISU       TO  RECE85-TANIMOJISU(IDX4)
           MOVE  TNS-TANINAME         TO  RECE85-TANINAME(IDX4)
           MOVE  TNS-TENSIKIBETU      TO  RECE85-TENSIKIBETU(IDX4)
           MOVE  TNS-TEN              TO  RECE85-TEN(IDX4)
           MOVE  TNS-NYUTENTTLKBN     TO  RECE85-TENTTLKBN(IDX4)
           MOVE  TNS-YUKOKETA         TO  RECE85-YUKOKETA(IDX4)
           MOVE  TNS-DATAKBN          TO  RECE85-DATAKBN(IDX4)
           MOVE  TNS-HOUKNSKBN        TO  RECE85-HOUKNSKBN(IDX4)
           MOVE  TNS-KNSJISKBN        TO  RECE85-KNSJISKBN(IDX4)
           MOVE  TNS-CDKBN-KBN        TO  RECE85-CDKBN-KBN(IDX4)
           MOVE  TNS-CDKBN-SYO        TO  RECE85-CDKBN-SYO(IDX4)
           MOVE  TNS-CDKBN-BU         TO  RECE85-CDKBN-BU(IDX4)
           MOVE  TNS-CDKBN-KBNNUM     TO  RECE85-CDKBN-KBNNUM(IDX4)
           MOVE  TNS-CDKBN-KBNNUM-EDA TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
           MOVE  TNS-CDKBN-KOUBAN     TO  RECE85-CDKBN-KOUBAN(IDX4)
           MOVE  TNS-KOUHYOJYUNNUM    TO  RECE85-KOUHYOJYUNNUM(IDX4)
           MOVE  TNS-SRYSYUKBN        TO  RECE85-SRYSYUKBN-X(IDX4)
           MOVE  TNS-KZMCOMPSIKIBETU  TO  RECE85-KZMCOMPSIKIBETU(IDX4)
           MOVE  TNS-KNSJISGRPKBN     TO  RECE85-KNSJISGRPKBN (IDX4)
           MOVE  TNS-KENTAICOMMENT    TO  RECE85-KENTAICOMMENT(IDX4)
           MOVE  TNS-KOKUJISKBKBN1    TO  RECE85-KOKUJISKBKBN1(IDX4)
           MOVE  TNS-KOKUJISKBKBN2    TO  RECE85-KOKUJISKBKBN2(IDX4)
           MOVE  TNS-TUSOKUGAIKBN     TO  RECE85-TUSOKUGAIKBN (IDX4)
           MOVE  TNS-TIMEKSNKBN       TO  RECE85-TIMEKSNKBN(IDX4)
           MOVE  TNS-CHUKSNCD         TO  RECE85-CHUKSNCD(IDX4)
           MOVE  TNS-CHUKSNTUBAN      TO  RECE85-CHUKSNTUBAN(IDX4)
           MOVE  TNS-TOKUKIZAISBT1    TO  RECE85-TOKUKIZAISBT1(IDX4)
           MOVE  TNS-SANSOKBN         TO  RECE85-SANSOKBN(IDX4)
           MOVE  TNS-NAME             TO  RECE85-NAME(IDX4)
      *
           .
       25021-RECE85-TENSOU2-KHNX-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Ø¤ÎÉéÃ´¶â¥»¥Ã¥È¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       260-FTN-SET-SEC                SECTION.
      *
           MOVE    ZERO                 TO      FLG-KHNRYO-SKJ
      *
           MOVE    ZERO                 TO      FLG-NYUINRYO
      *
           MOVE    ZERO                 TO      FLG-SYUNOU-ARI
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE82-HOSPNUM       TO      SYU-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      SYU-NYUGAIKBN
           MOVE    RECE82-PTID          TO      SYU-PTID
           MOVE    RECE82-SRYYM         TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    RECE82-HKNCOMBI      TO      SYU-HKNCOMBINUM
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_syunou"     TO      MCP-TABLE
               MOVE    "key19"          TO      MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                  (FLG-SYUNOU             =    1)
      *
                   IF  SYU-DENPJTIKBN    NOT =  "3"
                       PERFORM   2601-RECE82-FTN-HENSYU-SEC
                       MOVE   1   TO   FLG-SYUNOU-ARI
                   END-IF
      *
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key19"      TO      MCP-PATHNAME
                   PERFORM  900-SYUNOU-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    ¿©»ö¾ðÊó¥¯¥ê¥¢
           IF   RECE82-SRYYM  >= "201604"
             IF   RECE82-SHOKUJISYUBETU(1)      =  "£¹¡¡"
                MOVE   SPACE  TO   RECE82-SHOKUJISYUBETU(1)
             END-IF
             IF   RECE82-SHOKUJISYUBETU-SEI     =  "£¹"
                MOVE   SPACE  TO   RECE82-SHOKUJISYUBETU-SEI
             END-IF
             IF   RECE82-SHOKUJIKHNRYO(1)       =  999999999
                MOVE   ZERO   TO   RECE82-SHOKUJIKHNRYO(1)
             END-IF
             IF   RECE82-SHOKUJIKHNRYO-SEI(1)   =  999999999
                MOVE   ZERO   TO   RECE82-SHOKUJIKHNRYO-SEI(1)
             END-IF
           END-IF
      *
      *    ¿©»öÎ¬¾ÎÊÔ½¸
      *
           IF   RECE82-SRYYM  >= "200604"
           AND  RECE82-SRYYM  <  "200610"
                PERFORM    2602-SKJ-RAYKU-HENSYU-SEC
           END-IF
      *
           IF   FLG-SYUNOU-ARI  =  ZERO
              DISPLAY "<SYUNOU NG STOP>"
              MOVE    "²¾¼ýÇ¼¥Ç¡¼¥¿¤¬¤¢¤ê¤Þ¤»¤ó"  TO  ERR-ERRMSG
              PERFORM 600-ERRMSG-EDIT-SEC
              MOVE    ERR-HEN-ERRMSG    TO  WRK-RECEERR
              PERFORM 500-ERR-HENSYU-SEC
              MOVE    1                 TO  FLG-END
           END-IF
      *
           .
       260-FTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÝÉéÃ´¶â¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2601-RECE82-FTN-HENSYU-SEC               SECTION.
      *
           IF   RECE82-NYUINRYOFLG       =   "1"
           AND  FLG-NYUINRYO             =   ZERO
               PERFORM         26011-NYUINRYOFLG-SET-SEC
           END-IF
      *
           IF   RECE82-SRYYM   >=  "201604"
               PERFORM         26012-SKJ-KINGAKU-HENSYU2-SEC
           ELSE
               PERFORM         26012-SKJ-KINGAKU-HENSYU-SEC
           END-IF
      *
           IF      SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
                   ADD   SYU-SYUSKJ-NISSU     TO  
                                       RECE82-SHOKUJITOTALDAY
                   ADD   SYU-SYUSKJ-RYOYOHI   TO
                                       RECE82-SHOKUJITOTALKIN
                   IF    RECE82-HKNKBN  =  "6"
                     ADD SYU-SYUSKJ-FTN-RECE  TO
                                       RECE82-SKJFTNMONEY
                   END-IF
           END-IF
      *
           .
       2601-RECE82-FTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤ë¾ì¹ç¤ÎÊÝ¸±·èÄê
      *****************************************************************
       26011-NYUINRYOFLG-SET-SEC               SECTION.
      *
           MOVE     SPACE    TO    RECE82-NYUINRYOFLG
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >       31
      *
             IF      SYU-DAY(IDY)    =    "2"
      *
                IF         WRK-DAY5(IDY)      NOT =  ZERO
                AND        WRK-DAY1(IDY)      NOT =  ZERO
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE82-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "6"     TO   RECE82-NYUINRYOFLG
                       WHEN     "2"
                          PERFORM  260111-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                ELSE
                   IF      WRK-DAY5(IDY)              NOT =  ZERO
                   AND     RECE82-NYUINDAY-KBN(IDY)   NOT =  "1"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE82-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "1"     TO   RECE82-NYUINRYOFLG
                       WHEN     "2"
                          MOVE    "6"     TO   RECE82-NYUINRYOFLG
                       WHEN     "3"
                          PERFORM   260111-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                   END-IF
                   IF      WRK-DAY5(IDY)              NOT =  ZERO
                   AND     RECE82-NYUINDAY-KBN(IDY)       =  "1"
                   AND     RECE82-SRYYM                  >=  "200904"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE82-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "6"     TO   RECE82-NYUINRYOFLG
                       WHEN     "2"
                          PERFORM  260111-NYUINRYOFLG-HENSYU-SEC
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
       26011-NYUINRYOFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÊÔ½¸
      *****************************************************************
       260111-NYUINRYOFLG-HENSYU-SEC               SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    RECE82-HOSPNUM  TO  COMB-HOSPNUM
           MOVE    RECE82-PTID     TO  COMB-PTID
           MOVE    WRK-DAY5(IDY)   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 900-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI    =   ZERO
              IF   COMB-HKNNUM(1:2)  =  "98"
              OR   COMB-HKNNUM       =  "971"
              OR   COMB-HKNNUM       =  "973"
              OR   COMB-HKNNUM       =  "975"
                IF   COMB-HKNNUM(1:2)  =  "98"
                   MOVE    "1"   TO   RECE82-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "971"
                   IF   RECE82-HKNNUM  =  "971"
                     CONTINUE
                   ELSE
                     MOVE   "2"  TO   RECE82-NYUINRYOFLG
                   END-IF
                END-IF
                IF   COMB-HKNNUM       =  "973"
                   IF   RECE82-HKNNUM  =  "973"
                     CONTINUE
                   ELSE
                     MOVE   "3"  TO   RECE82-NYUINRYOFLG
                   END-IF
                END-IF
                IF   COMB-HKNNUM       =  "975"
                   MOVE    "7"   TO   RECE82-NYUINRYOFLG
                END-IF
              ELSE
                EVALUATE  COMB-HKNNUM
                  WHEN     "060"
                  WHEN     "067"
                  WHEN     "068"
                  WHEN     "069"
                    MOVE   "4"   TO   RECE82-NYUINRYOFLG
                  WHEN     "039"
                  WHEN     "040"
                    MOVE   "8"   TO   RECE82-NYUINRYOFLG
                  WHEN      OTHER
                    MOVE   "5"   TO   RECE82-NYUINRYOFLG
                    IF   COMB-HKNNUM(1:2)  =  "90" OR "91"
                     MOVE SPACE  TO   RECE82-NYUINRYOFLG
                    END-IF
                END-EVALUATE
              END-IF
           END-IF
      *
           .
       260111-NYUINRYOFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¶â³ÛÊÔ½¸
      *****************************************************************
       26012-SKJ-KINGAKU-HENSYU-SEC               SECTION.
      *
           IF    RECE82-SRYYM  >= "200610"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >    10
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                 IF  ((RECE82-HKNKBN         = "4") AND
                      (WRK-PARA-RECEKBN      = "5") AND
                      (WRK-PARA-JIBAIPRTKBN  = "1"))
                 OR  ((RECE82-HKNKBN         = "6") AND
                      (WRK-PARA-JIBAIPRTKBN  = "1"))
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       2
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(1)
                      EVALUATE  ID1
                        WHEN     1
                          MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU(1)
                        WHEN     2
                          MOVE  "£²"   TO  RECE82-SHOKUJISYUBETU(1)
                      END-EVALUATE
                     WHEN       3
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(2)
                      MOVE  "ÆÃÊÌ" TO  RECE82-SHOKUJISYUBETU(2)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
                      MOVE  "¿©Æ²" TO  RECE82-SHOKUJISYUBETU(3)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
                      MOVE  "´Ä¶­" TO  RECE82-SHOKUJISYUBETU(4)
                     WHEN       5
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-SEI(1)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-SEI(1)
                      EVALUATE  ID1
                        WHEN     5
                          MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU-SEI
                        WHEN     6
                          MOVE  "£²"   TO  RECE82-SHOKUJISYUBETU-SEI
                      END-EVALUATE
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-SEI(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-SEI(2)
                   END-EVALUATE
                 ELSE
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       5
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE82-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE82-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE82-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE82-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU(1)
                     WHEN       2
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE82-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE82-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE82-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE82-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£²"   TO  RECE82-SHOKUJISYUBETU(1)
                     WHEN       3
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(2)
                      MOVE  "ÆÃÊÌ" TO  RECE82-SHOKUJISYUBETU(2)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
                      MOVE  "¿©Æ²" TO  RECE82-SHOKUJISYUBETU(3)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
                      MOVE  "´Ä¶­" TO  RECE82-SHOKUJISYUBETU(4)
                   END-EVALUATE
                 END-IF
               END-IF
      *
             END-PERFORM
      *
           ELSE
      *
            IF   RECE82-SRYYM  >= "200604"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     4
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(ID1)
                   MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(ID1)
               END-IF
      *
             END-PERFORM
      *
            ELSE
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     5
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                 CONTINUE
               ELSE
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID2    FROM    1   BY  1
                           UNTIL   ID2                       > 2
                             OR    RECE82-SHOKUJIKHNRYO(ID2) = ZERO
                             OR    FLG-ARI                   = 1
                     IF  SYU-SHOKUJI(ID1) = RECE82-SHOKUJIKHNRYO(ID2)
                           ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                          RECE82-SHOKUJIKHNNISSU(ID2)
                           MOVE  1   TO   FLG-ARI
                     END-IF
                 END-PERFORM
                 IF  FLG-ARI  =  ZERO
                 AND ID2     <=   2
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                          RECE82-SHOKUJIKHNNISSU(ID2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                          RECE82-SHOKUJIKHNRYO(ID2)
                 END-IF
      *
                 IF  FLG-ARI  =  ZERO
                 AND ID2      >   2
                   MOVE     ZERO     TO    FLG-ARI2
                   PERFORM  VARYING  ID4    FROM    1   BY  1
                             UNTIL ID4                           > 3
                               OR  RECE82-SHOKUJIKHNRYO-ETC(ID4) = ZERO
                               OR  FLG-ARI2                      = 1
                       IF  SYU-SHOKUJI(ID1)   =
                                       RECE82-SHOKUJIKHNRYO-ETC(ID4)
                             ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-ETC(ID4)
                             MOVE  1   TO   FLG-ARI2
                       END-IF
                   END-PERFORM
                   IF  FLG-ARI2  =  ZERO
                   AND ID4      <=   3
                       ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-ETC(ID4)
                       MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-ETC(ID4)
                   END-IF
                 END-IF
               END-IF
      *
             END-PERFORM
      *
             IF      SYU-SHOKUJI(6)  NOT =  ZERO
                   ADD   SYU-SHOKUJI-NISSU(6)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                   MOVE  SYU-SHOKUJI(6)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
             END-IF
             IF      SYU-SHOKUJI(7)  NOT =  ZERO
                   ADD   SYU-SHOKUJI-NISSU(7)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                   MOVE  SYU-SHOKUJI(7)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
             END-IF
      *
            END-IF
      *
           END-IF
      *
           .
       26012-SKJ-KINGAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¶â³ÛÊÔ½¸¡Ê£²£°£±£¶Ç¯£´·î¡Á¡Ë
      *****************************************************************
       26012-SKJ-KINGAKU-HENSYU2-SEC               SECTION.
      *
           PERFORM  VARYING  ID1    FROM    1   BY  1
                     UNTIL   ID1      >    14
      *
             IF  SYU-SHOKUJI(ID1)   =   ZERO
               CONTINUE
             ELSE
               IF  ((RECE82-HKNKBN         = "4") AND
                    (WRK-PARA-RECEKBN      = "5") AND
                    (WRK-PARA-JIBAIPRTKBN  = "1"))
               OR  ((RECE82-HKNKBN         = "6") AND
                    (WRK-PARA-JIBAIPRTKBN  = "1"))
                 EVALUATE    ID1
                   WHEN       1
                   WHEN       2
                   WHEN      11
                   WHEN      12
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(1)
                     IF    RECE82-SHOKUJIKHNRYO(1) = ZERO
                         MOVE  SYU-SHOKUJI(ID1)
                                       TO  RECE82-SHOKUJIKHNRYO(1)
                     ELSE
                       IF  RECE82-SHOKUJIKHNRYO(1)  NOT =
                                                  SYU-SHOKUJI(ID1)
                         MOVE  999999999
                                       TO  RECE82-SHOKUJIKHNRYO(1)
                       END-IF
                     END-IF
                     EVALUATE    ID1
                       WHEN       1
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£±¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£±¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                       WHEN       2
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£²¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£²¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                       WHEN      11
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£³¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£³¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                       WHEN      12
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£´¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£´¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                     END-EVALUATE
                   WHEN       3
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                      RECE82-SHOKUJIKHNRYO(2)
                     MOVE  "ÆÃÊÌ" TO  RECE82-SHOKUJISYUBETU(2)
                   WHEN       4
                   WHEN       8
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(3)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                      RECE82-SHOKUJIKHNRYO(3)
                     MOVE  "¿©Æ²" TO  RECE82-SHOKUJISYUBETU(3)
                   WHEN       9
                   WHEN      10
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(4)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                      RECE82-SHOKUJIKHNRYO(4)
                     MOVE  "´Ä¶­" TO  RECE82-SHOKUJISYUBETU(4)
                   WHEN       5
                   WHEN       6
                   WHEN      13
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU-SEI(1)
                     IF    RECE82-SHOKUJIKHNRYO-SEI(1) = ZERO
                         MOVE  SYU-SHOKUJI(ID1)
                                       TO  RECE82-SHOKUJIKHNRYO-SEI(1)
                     ELSE
                       IF  RECE82-SHOKUJIKHNRYO-SEI(1)  NOT =
                                                  SYU-SHOKUJI(ID1)
                         MOVE  999999999
                                       TO  RECE82-SHOKUJIKHNRYO-SEI(1)
                       END-IF
                     END-IF
                     EVALUATE    ID1
                       WHEN       5
                         IF    RECE82-SHOKUJISYUBETU-SEI = SPACE
                            MOVE "£±" TO  RECE82-SHOKUJISYUBETU-SEI
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU-SEI NOT = "£±"
                            MOVE "£¹" TO  RECE82-SHOKUJISYUBETU-SEI
                           END-IF
                         END-IF
                       WHEN       6
                         IF    RECE82-SHOKUJISYUBETU-SEI = SPACE
                            MOVE "£²" TO  RECE82-SHOKUJISYUBETU-SEI
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU-SEI NOT = "£²"
                            MOVE "£¹" TO  RECE82-SHOKUJISYUBETU-SEI
                           END-IF
                         END-IF
                       WHEN      13
                         IF    RECE82-SHOKUJISYUBETU-SEI = SPACE
                            MOVE "£³" TO  RECE82-SHOKUJISYUBETU-SEI
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU-SEI NOT = "£³"
                            MOVE "£¹" TO  RECE82-SHOKUJISYUBETU-SEI
                           END-IF
                         END-IF
                     END-EVALUATE
                   WHEN       7
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU-SEI(2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                      RECE82-SHOKUJIKHNRYO-SEI(2)
                   WHEN      14
                     CONTINUE
                 END-EVALUATE
               ELSE
                 EVALUATE    ID1
                   WHEN       1
                   WHEN       2
                   WHEN       5
                   WHEN       6
                   WHEN      11
                   WHEN      12
                   WHEN      13
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(1)
                     IF    RECE82-SHOKUJIKHNRYO(1) = ZERO
                         MOVE  SYU-SHOKUJI(ID1)
                                       TO  RECE82-SHOKUJIKHNRYO(1)
                     ELSE
                       IF  RECE82-SHOKUJIKHNRYO(1)  NOT =
                                                  SYU-SHOKUJI(ID1)
                         MOVE  999999999
                                       TO  RECE82-SHOKUJIKHNRYO(1)
                       END-IF
                     END-IF
                     EVALUATE    ID1
                       WHEN       1
                       WHEN       5
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£±¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£±¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                       WHEN       2
                       WHEN       6
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£²¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£²¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                       WHEN      11
                       WHEN      13
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£³¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£³¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                       WHEN      12
                         IF    RECE82-SHOKUJISYUBETU(1) = SPACE
                             MOVE  "£´¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                         ELSE
                           IF  RECE82-SHOKUJISYUBETU(1)  NOT = "£´¡¡"
                             MOVE  "£¹¡¡"  TO  RECE82-SHOKUJISYUBETU(1)
                           END-IF
                         END-IF
                     END-EVALUATE
                   WHEN       3
                   WHEN       7
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                      RECE82-SHOKUJIKHNRYO(2)
                     MOVE  "ÆÃÊÌ" TO  RECE82-SHOKUJISYUBETU(2)
                   WHEN       4
                   WHEN       8
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(3)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                      RECE82-SHOKUJIKHNRYO(3)
                     MOVE  "¿©Æ²" TO  RECE82-SHOKUJISYUBETU(3)
                   WHEN       9
                   WHEN      10
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                      RECE82-SHOKUJIKHNNISSU(4)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                      RECE82-SHOKUJIKHNRYO(4)
                     MOVE  "´Ä¶­" TO  RECE82-SHOKUJISYUBETU(4)
                   WHEN      14
                     CONTINUE
                 END-EVALUATE
               END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       26012-SKJ-KINGAKU-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»öÎ¬¾ÎÊÔ½¸
      *****************************************************************
       2602-SKJ-RAYKU-HENSYU-SEC               SECTION.
      *
           PERFORM  VARYING  ID1    FROM    1   BY  1
                     UNTIL   ID1      >     4
      *
               IF  RECE82-SHOKUJIKHNNISSU(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   EVALUATE   ID1
                     WHEN      1
                       MOVE   "£±"    TO   RECE82-SHOKUJISYUBETU(1)
                     WHEN      2
                       MOVE   "£²"    TO   RECE82-SHOKUJISYUBETU(1)
                     WHEN      3
                       MOVE   "ÆÃÊÌ"  TO   RECE82-SHOKUJISYUBETU(2)
                     WHEN      4
                       MOVE   "¿©Æ²"  TO   RECE82-SHOKUJISYUBETU(3)
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           IF    RECE82-SHOKUJIKHNNISSU(1)      =  ZERO
           AND   RECE82-SHOKUJIKHNNISSU(2)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(2)
                              TO  RECE82-SHOKUJIKHNNISSU(1)
               MOVE    RECE82-SHOKUJIKHNRYO(2)
                              TO  RECE82-SHOKUJIKHNRYO(1)
      *
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(2)
                                  RECE82-SHOKUJIKHNRYO(2)
           END-IF
      *
           IF      RECE82-SHOKUJIKHNNISSU(2)      =  ZERO
             IF    RECE82-SHOKUJIKHNNISSU(3)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(3)
                              TO  RECE82-SHOKUJIKHNNISSU(2)
               MOVE    RECE82-SHOKUJIKHNRYO(3)
                              TO  RECE82-SHOKUJIKHNRYO(2)
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(3)
                                  RECE82-SHOKUJIKHNRYO(3)
             END-IF
             IF    RECE82-SHOKUJIKHNNISSU(4)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(4)
                              TO  RECE82-SHOKUJIKHNNISSU(3)
               MOVE    RECE82-SHOKUJIKHNRYO(4)
                              TO  RECE82-SHOKUJIKHNRYO(3)
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(4)
                                  RECE82-SHOKUJIKHNRYO(4)
             END-IF
           END-IF
      *
           .
       2602-SKJ-RAYKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý¡Ê¼«ÇåÀÕ¡Ë
      *****************************************************************
       270-KHNTEN-JI-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN7-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(7)
           END-IF
           IF        FLG-KHNTEN8-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(8)
           END-IF
           IF        FLG-KHNTEN9-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(9)
           END-IF
           .
       270-KHNTEN-JI-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       280-RECE82-REWRITE-SEC                SECTION.
      *
           IF   RECE82-HKNKBN     NOT  =   "4" AND "6"
             COMPUTE  RECE82-KINGAKU2-SYO  =  RECE82-KINGAKU2-SYO
                                           +  WRK-ZAITENNASHI
           END-IF
      *
           IF   RECE82-HKNKBN     =   "4"                               
      *
             EVALUATE  WRK-PARA-RECEKBN
              WHEN      "4"
      ********¡Ê¿·ÍÍ¼°¡Ë
               IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                 COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)
                                                +  RECE82-TENSU(2)
      *
                 COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(7)
                                                +  RECE82-TENSU(8)
                                                +  RECE82-TENSU(9)
      *
                 COMPUTE   RECE82-TENSU-YAK(10) =  RECE82-TENSU-YAK(4)
                                                +  RECE82-TENSU-YAK(5)
                                                +  RECE82-TENSU-YAK(6)
      *
                 MOVE      ZERO              TO    RECE82-KINGAKU2-SYO
                                                   RECE82-TOTALKIN2
                                                   RECE82-KINGAKU3-SYO
      *
                 COMPUTE   RECE82-KINGAKU2-SYO  =  RECE82-KINGAKU2(1)
                                                +  RECE82-KINGAKU2(2)
                                                +  RECE82-KINGAKU2(3)
                                                +  RECE82-KINGAKU2(4)
                                                +  RECE82-KINGAKU2(5)
      *
                 COMPUTE   WRK-KSNRATE  =  RECE82-KSNRATE  /  100
                 COMPUTE   RECE82-TOTALKIN2     =
                          (RECE82-KINGAKU2-SYO  *  WRK-KSNRATE)  +  0.5
      *
                 COMPUTE   RECE82-KINGAKU3-SYO  =  RECE82-KINGAKU3(1)
                                                +  RECE82-KINGAKU3(2)
                                                +  RECE82-KINGAKU3(3)
                                                +  RECE82-KINGAKU3(4)
                                                +  RECE82-KINGAKU3(5)
      *
                 COMPUTE   RECE82-TOTALKIN-ETC      =
                           RECE82-KINGAKU3-SYO      +
                           RECE82-SHOKUJITOTALKIN   +
                           RECE82-SINDAN-KIN        +
                           RECE82-MEISAI-KIN
      *
                 PERFORM   2803-JIBAI-KINGAKU-HENSYU-SEC
               ELSE
                 COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)
                                                +  RECE82-TENSU(2)
      *
                 COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(7)
                                                +  RECE82-TENSU(8)
                                                +  RECE82-TENSU(9)
      *
                 COMPUTE   RECE82-TENSU-YAK(11) =  RECE82-TENSU-YAK(4)
                                                +  RECE82-TENSU-YAK(5)
                                                +  RECE82-TENSU-YAK(6)
      *
                 MOVE      ZERO              TO    RECE82-KINGAKU2-SYO
                                                   RECE82-TOTALKIN2
                                                   RECE82-KINGAKU3-SYO
      *
                 COMPUTE   RECE82-KINGAKU2-SYO  =  RECE82-KINGAKU2(1)
                                                +  RECE82-KINGAKU2(2)
                                                +  RECE82-KINGAKU2(3)
                                                +  RECE82-KINGAKU2(4)
                                                +  RECE82-KINGAKU2(5)
      *
                 COMPUTE   WRK-KSNRATE  =  RECE82-KSNRATE  /  100
                 COMPUTE   RECE82-TOTALKIN2     =
                          (RECE82-KINGAKU2-SYO  *  WRK-KSNRATE)  +  0.5
      *
                 COMPUTE   RECE82-KINGAKU3-SYO  =  RECE82-KINGAKU3(1)
                                                +  RECE82-KINGAKU3(2)
                                                +  RECE82-KINGAKU3(3)
                                                +  RECE82-KINGAKU3(4)
                                                +  RECE82-KINGAKU3(5)
      *
                 COMPUTE   RECE82-TOTALKIN-ETC      =
                           RECE82-KINGAKU3-SYO      +
                           RECE82-SHOKUJITOTALKIN   +
                           RECE82-SINDAN-KIN        +
                           RECE82-MEISAI-KIN
               END-IF
      * 
              WHEN      "5"
      ********¡Ê½¾ÍèÍÍ¼°¡Ë
      *
               MOVE      ZERO              TO    RECE82-TOTALTEN
                                                 RECE82-TOTALTEN-YAK
                                                 RECE82-KINGAKU2-SYO
                                                 RECE82-KINGAKU3-SYO
                                                 RECE82-TOTALKIN
                                                 RECE82-TOTALKIN-YAK
                                                 RECE82-TOTALKIN2
                                                 RECE82-TOTALKIN-ETC
      *
               COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)
                                              +  RECE82-TENSU(2)
      *
               COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(4)
                                              +  RECE82-TENSU(5)
                                              +  RECE82-TENSU(6)
                                              +  RECE82-TENSU(7)
                                              +  RECE82-TENSU(8)
                                              +  RECE82-TENSU(9)
      *
               COMPUTE   RECE82-TENSU(14)     =  RECE82-TENSU(11)
                                              +  RECE82-TENSU(12)
                                              +  RECE82-TENSU(13)
      *
               COMPUTE   RECE82-TENSU(17)     =  RECE82-TENSU(15)
                                              +  RECE82-TENSU(16)
      *
               COMPUTE   RECE82-TENSU(20)     =  RECE82-TENSU(18)
                                              +  RECE82-TENSU(19)
      *
               COMPUTE   RECE82-TENSU(23)     =  RECE82-TENSU(21)
                                              +  RECE82-TENSU(22)
      *
               COMPUTE   RECE82-TENSU(26)     =  RECE82-TENSU(24)
                                              +  RECE82-TENSU(25)
      *
               COMPUTE   RECE82-TENSU(29)     =  RECE82-TENSU(27)
                                              +  RECE82-TENSU(28)
      *
      ***      COMPUTE   RECE82-TOTALTEN      =  RECE82-NYUTOTALTEN
      ***                                     +  RECE82-TENSU(3)
      ***                                     +  RECE82-TENSU(10)
      ***                                     +  RECE82-TENSU(14)
      ***                                     +  RECE82-TENSU(17)
      ***                                     +  RECE82-TENSU(20)
      ***                                     +  RECE82-TENSU(23)
      ***                                     +  RECE82-TENSU(26)
      ***                                     +  RECE82-TENSU(29)
      *
               MOVE     ZERO              TO   WRK-SYUTEN-JI
               MOVE     ZERO              TO   WRK-YKZTEN-JI
               PERFORM    VARYING    ID3    FROM    1   BY   1
                           UNTIL     ID3     >     31
                  COMPUTE  WRK-SYUTEN-JI  =  WRK-SYUTEN-JI
                                          +  RECE82-SYUTEN-JI(ID3)
                  COMPUTE  WRK-YKZTEN-JI  =  WRK-YKZTEN-JI
                                          +  RECE82-YKZTEN-JI(ID3)
               END-PERFORM
      ****     LEFT½èÍý¤ÇºÆ¿Ç²Ã»»¡ÊÆþ±¡¡Ë¤ÎÅÀ¿ô¤ò¡ÖRECE82-NYUTOTALTEN¡×
      ****     ¤Ë²Ã»»¤·¤¿¤Î¤Ç¡ÖWRK-SYUTEN-JI¡×¤«¤é²Ã»»¤·¤¿ÅÀ¿ô¤òº¹¤·°ú¤¯
               COMPUTE   WRK-SYUTEN-JI    =  WRK-SYUTEN-JI
                                          -  RECE82-NYUSAI-TENSU
               COMPUTE   RECE82-TOTALTEN  =  RECE82-NYUTOTALTEN
                                          +  WRK-SYUTEN-JI
                                          +  WRK-YKZTEN-JI
      *
               IF   TBL-SYUTEN-JI(2)   >=  WRK-RECE8G-TENSU(2)
                 COMPUTE  TBL-SYUTEN-JI(2)  =
                          TBL-SYUTEN-JI(2)  -  WRK-RECE8G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(2)
               END-IF
               IF   TBL-SYUTEN-JI(27)  >=  WRK-RECE8G-TENSU(27)
                 COMPUTE  TBL-SYUTEN-JI(27) =
                          TBL-SYUTEN-JI(27) -  WRK-RECE8G-TENSU(27)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(27)
               END-IF
               IF   TBL-SYUTEN-JI(28)  >=  WRK-RECE8G-TENSU(28)
                 COMPUTE  TBL-SYUTEN-JI(28) =
                          TBL-SYUTEN-JI(28) -  WRK-RECE8G-TENSU(28)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(28)
               END-IF
               IF   TBL-YKZTEN-JI-C(13)  >=  WRK-GENTEN-YKZTEN
                 COMPUTE  TBL-YKZTEN-JI(13) =
                          TBL-YKZTEN-JI(13) -  WRK-GENTEN-YKZTEN
               ELSE
                 COMPUTE  TBL-YKZTEN-JI(13) =
                          TBL-YKZTEN-JI(13) -  TBL-YKZTEN-JI-C(13)
                 COMPUTE  WRK-GENTEN-YKZTEN =
                          WRK-GENTEN-YKZTEN -  TBL-YKZTEN-JI-C(13)
                 IF  TBL-YKZTEN-JI-C(12)   >=  WRK-GENTEN-YKZTEN
                   COMPUTE  TBL-YKZTEN-JI(12) =
                            TBL-YKZTEN-JI(12) -  WRK-GENTEN-YKZTEN
                 ELSE
                   COMPUTE  TBL-YKZTEN-JI(12) =
                            TBL-YKZTEN-JI(12) -  TBL-YKZTEN-JI-C(12)
                   COMPUTE  WRK-GENTEN-YKZTEN =
                            WRK-GENTEN-YKZTEN -  TBL-YKZTEN-JI-C(12)
                   IF  TBL-YKZTEN-JI-C(11)  >=   WRK-GENTEN-YKZTEN
                     COMPUTE  TBL-YKZTEN-JI(11) =
                              TBL-YKZTEN-JI(11) -  WRK-GENTEN-YKZTEN
                   ELSE
                     COMPUTE  TBL-YKZTEN-JI(11) =
                              TBL-YKZTEN-JI(11) -  TBL-YKZTEN-JI-C(11)
                   END-IF
                 END-IF
               END-IF
      *
               EVALUATE   RECE82-JIBAIKBN
      *
                WHEN       "1"
      *
                  COMPUTE   WRK-KSNRATE  =  RECE82-KSNRATE  /  100
      *
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ((WRK-SYUTEN-JI +  RECE82-NYUTOTALTEN)
                                     *  RECE82-TANKA  *  WRK-KSNRATE)
                                                      +  0.5
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     (WRK-YKZTEN-JI  *  RECE82-TANKA) +  0.5
      *
                  COMPUTE  RECE82-SONOTAKIN-JYU  =
                           RECE82-SONOTAKIN-JYU  +
                     (WRK-KINGAKU-JI *  WRK-KSNRATE)  +  0.5
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     29
                    IF   ID3  =   3  OR  10  OR  14  OR  17  OR
                                 20  OR  23  OR  26  OR  29
                       CONTINUE
                    ELSE
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                       (TBL-SYUTEN-JI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                       (TBL-YKZTEN-JI(ID3) * RECE82-TANKA)
                                                          + 0.5
                    END-IF
                  END-PERFORM
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     20
      *
                    COMPUTE  RECE82-NYUKHNKIN-JYU(ID3) =
                             RECE82-NYUKHNKIN-JYU(ID3) +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                    COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                             RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
      *
                    COMPUTE  RECE82-NYUTOKKIN-JYU(ID3) =
                             RECE82-NYUTOKKIN-JYU(ID3) +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                    COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                             RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
      *
                  END-PERFORM
      *
                  MOVE   RECE82-NYUKHNKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUKHNKIN2-JYU  =
                           RECE82-NYUKHNKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-TANKA * WRK-KSNRATE)
                                                       + 0.5
                  MOVE   RECE82-NYUTOKKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUTOKKIN2-JYU  =
                           RECE82-NYUTOKKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-TANKA * WRK-KSNRATE)
                                                       + 0.5
                  IF     RECE82-NYUKHNKEI2  <   ZERO
                      COMPUTE  RECE82-NYUKHNKIN2-JYU   =
                               RECE82-NYUKHNKIN2-JYU   *  -1
                  END-IF
                  IF     RECE82-NYUTOKKEI2  <   ZERO
                      COMPUTE  RECE82-NYUTOKKIN2-JYU   =
                               RECE82-NYUTOKKIN2-JYU   *  -1
                  END-IF
      *
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUKHNKIN2-JYU
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUTOKKIN2-JYU
      *
                WHEN       "2"
      *
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ((WRK-SYUTEN-JI +  RECE82-NYUTOTALTEN)
                                     *  RECE82-SYUTENTANKA )
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ( WRK-YKZTEN-JI *  RECE82-YKZTENTANKA )
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     29
                    IF   ID3  =   3  OR  10  OR  14  OR  17  OR
                                 20  OR  23  OR  26  OR  29
                       CONTINUE
                    ELSE
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                         (TBL-SYUTEN-JI(ID3) * RECE82-SYUTENTANKA)
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                         (TBL-YKZTEN-JI(ID3) * RECE82-YKZTENTANKA)
                    END-IF
                  END-PERFORM
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     20
      *
                     COMPUTE  RECE82-NYUKHNKIN-JYU(ID3) =
                              RECE82-NYUKHNKIN-JYU(ID3) +
                       (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
                     COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                              RECE82-NYUTOTALKIN-JYU    +
                       (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
      *
                     COMPUTE  RECE82-NYUTOKKIN-JYU(ID3) =
                              RECE82-NYUTOKKIN-JYU(ID3) +
                       (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
                     COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                              RECE82-NYUTOTALKIN-JYU    +
                       (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
      *
                  END-PERFORM
      *
                  MOVE   RECE82-NYUKHNKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUKHNKIN2-JYU  =
                           RECE82-NYUKHNKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-SYUTENTANKA)
                  MOVE   RECE82-NYUTOKKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUTOKKIN2-JYU  =
                           RECE82-NYUTOKKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-SYUTENTANKA)
                  IF     RECE82-NYUKHNKEI2  <   ZERO
                      COMPUTE  RECE82-NYUKHNKIN2-JYU   =
                               RECE82-NYUKHNKIN2-JYU   *  -1
                  END-IF
                  IF     RECE82-NYUTOKKEI2  <   ZERO
                      COMPUTE  RECE82-NYUTOKKIN2-JYU   =
                               RECE82-NYUTOKKIN2-JYU   *  -1
                  END-IF
      *
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUKHNKIN2-JYU
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUTOKKIN2-JYU
      *
               END-EVALUATE
      *
               COMPUTE   RECE82-KINGAKU-JYU(3)  =
                         RECE82-KINGAKU-JYU(1)  +
                         RECE82-KINGAKU-JYU(2)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(10)  =
                         RECE82-KINGAKU-JYU(4)   +
                         RECE82-KINGAKU-JYU(5)   +
                         RECE82-KINGAKU-JYU(6)   +
                         RECE82-KINGAKU-JYU(7)   +
                         RECE82-KINGAKU-JYU(8)   +
                         RECE82-KINGAKU-JYU(9)    
      *
               COMPUTE   RECE82-KINGAKU-JYU(14)  =
                         RECE82-KINGAKU-JYU(11)  +
                         RECE82-KINGAKU-JYU(12)  +
                         RECE82-KINGAKU-JYU(13)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(17)  =
                         RECE82-KINGAKU-JYU(15)  +
                         RECE82-KINGAKU-JYU(16)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(20)  =
                         RECE82-KINGAKU-JYU(18)  +
                         RECE82-KINGAKU-JYU(19)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(23)  =
                         RECE82-KINGAKU-JYU(21)  +
                         RECE82-KINGAKU-JYU(22)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(26)  =
                         RECE82-KINGAKU-JYU(24)  +
                         RECE82-KINGAKU-JYU(25)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(29)  =
                         RECE82-KINGAKU-JYU(27)  +
                         RECE82-KINGAKU-JYU(28)   
      *
             END-EVALUATE
      *
           END-IF                                                       
      *
           IF   RECE82-HKNKBN     =   "6"
      *
               MOVE      ZERO              TO    RECE82-TOTALTEN
                                                 RECE82-TOTALTEN-YAK
                                                 RECE82-KINGAKU2-SYO
                                                 RECE82-KINGAKU3-SYO
                                                 RECE82-TOTALKIN
                                                 RECE82-TOTALKIN-YAK
                                                 RECE82-TOTALKIN2
                                                 RECE82-TOTALKIN-ETC
      *
               COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)
                                              +  RECE82-TENSU(2)
      *
               COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(4)
                                              +  RECE82-TENSU(5)
                                              +  RECE82-TENSU(6)
                                              +  RECE82-TENSU(7)
                                              +  RECE82-TENSU(8)
                                              +  RECE82-TENSU(9)
      *
               COMPUTE   RECE82-TENSU(14)     =  RECE82-TENSU(11)
                                              +  RECE82-TENSU(12)
                                              +  RECE82-TENSU(13)
      *
               COMPUTE   RECE82-TENSU(17)     =  RECE82-TENSU(15)
                                              +  RECE82-TENSU(16)
      *
               COMPUTE   RECE82-TENSU(20)     =  RECE82-TENSU(18)
                                              +  RECE82-TENSU(19)
      *
               COMPUTE   RECE82-TENSU(23)     =  RECE82-TENSU(21)
                                              +  RECE82-TENSU(22)
      *
               COMPUTE   RECE82-TENSU(26)     =  RECE82-TENSU(24)
                                              +  RECE82-TENSU(25)
      *
               COMPUTE   RECE82-TENSU(29)     =  RECE82-TENSU(27)
                                              +  RECE82-TENSU(28)
      *
               MOVE     ZERO              TO   WRK-SYUTEN-JI
               MOVE     ZERO              TO   WRK-YKZTEN-JI
               PERFORM    VARYING    ID3    FROM    1   BY   1
                           UNTIL     ID3     >     31
                  COMPUTE  WRK-SYUTEN-JI  =  WRK-SYUTEN-JI
                                          +  RECE82-SYUTEN-JI(ID3)
                  COMPUTE  WRK-YKZTEN-JI  =  WRK-YKZTEN-JI
                                          +  RECE82-YKZTEN-JI(ID3)
               END-PERFORM
      ****     LEFT½èÍý¤ÇºÆ¿Ç²Ã»»¡ÊÆþ±¡¡Ë¤ÎÅÀ¿ô¤ò¡ÖRECE82-NYUTOTALTEN¡×
      ****     ¤Ë²Ã»»¤·¤¿¤Î¤Ç¡ÖWRK-SYUTEN-JI¡×¤«¤é²Ã»»¤·¤¿ÅÀ¿ô¤òº¹¤·°ú¤¯
               COMPUTE   WRK-SYUTEN-JI    =  WRK-SYUTEN-JI
                                          -  RECE82-NYUSAI-TENSU
               COMPUTE   RECE82-TOTALTEN  =  RECE82-NYUTOTALTEN
                                          +  WRK-SYUTEN-JI
                                          +  WRK-YKZTEN-JI
      *
               IF   TBL-SYUTEN-JI(2)   >=  WRK-RECE8G-TENSU(2)
                 COMPUTE  TBL-SYUTEN-JI(2)  =
                          TBL-SYUTEN-JI(2)  -  WRK-RECE8G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(2)
               END-IF
               IF   TBL-SYUTEN-JI(27)  >=  WRK-RECE8G-TENSU(27)
                 COMPUTE  TBL-SYUTEN-JI(27) =
                          TBL-SYUTEN-JI(27) -  WRK-RECE8G-TENSU(27)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(27)
               END-IF
               IF   TBL-SYUTEN-JI(28)  >=  WRK-RECE8G-TENSU(28)
                 COMPUTE  TBL-SYUTEN-JI(28) =
                          TBL-SYUTEN-JI(28) -  WRK-RECE8G-TENSU(28)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(28)
               END-IF
               IF   TBL-YKZTEN-JI-C(13)  >=  WRK-GENTEN-YKZTEN
                 COMPUTE  TBL-YKZTEN-JI(13) =
                          TBL-YKZTEN-JI(13) -  WRK-GENTEN-YKZTEN
               ELSE
                 COMPUTE  TBL-YKZTEN-JI(13) =
                          TBL-YKZTEN-JI(13) -  TBL-YKZTEN-JI-C(13)
                 COMPUTE  WRK-GENTEN-YKZTEN =
                          WRK-GENTEN-YKZTEN -  TBL-YKZTEN-JI-C(13)
                 IF  TBL-YKZTEN-JI-C(12)   >=  WRK-GENTEN-YKZTEN
                   COMPUTE  TBL-YKZTEN-JI(12) =
                            TBL-YKZTEN-JI(12) -  WRK-GENTEN-YKZTEN
                 ELSE
                   COMPUTE  TBL-YKZTEN-JI(12) =
                            TBL-YKZTEN-JI(12) -  TBL-YKZTEN-JI-C(12)
                   COMPUTE  WRK-GENTEN-YKZTEN =
                            WRK-GENTEN-YKZTEN -  TBL-YKZTEN-JI-C(12)
                   IF  TBL-YKZTEN-JI-C(11)  >=   WRK-GENTEN-YKZTEN
                     COMPUTE  TBL-YKZTEN-JI(11) =
                              TBL-YKZTEN-JI(11) -  WRK-GENTEN-YKZTEN
                   ELSE
                     COMPUTE  TBL-YKZTEN-JI(11) =
                              TBL-YKZTEN-JI(11) -  TBL-YKZTEN-JI-C(11)
                   END-IF
                 END-IF
               END-IF
      *
               COMPUTE  RECE82-TOTALKIN-JYU   =
                        RECE82-TOTALKIN-JYU   +
                  ((WRK-SYUTEN-JI +  RECE82-NYUTOTALTEN)
                                  *  RECE82-SYUTENTANKA )
               COMPUTE  RECE82-TOTALKIN-JYU   =
                        RECE82-TOTALKIN-JYU   +
                  ( WRK-YKZTEN-JI *  RECE82-YKZTENTANKA )
      ***
               PERFORM    VARYING    ID3    FROM    1   BY   1
                           UNTIL     ID3     >     29
                 IF   ID3  =   3  OR  10  OR  14  OR  17  OR
                              20  OR  23  OR  26  OR  29
                    CONTINUE
                 ELSE
                    COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                             RECE82-KINGAKU-JYU(ID3) +
                      (TBL-SYUTEN-JI(ID3) * RECE82-SYUTENTANKA)
                    COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                             RECE82-KINGAKU-JYU(ID3) +
                      (TBL-YKZTEN-JI(ID3) * RECE82-YKZTENTANKA)
                 END-IF
               END-PERFORM
      ***
               PERFORM    VARYING    ID3    FROM    1   BY   1
                           UNTIL     ID3     >     20
      *
                  COMPUTE  RECE82-NYUKHNKIN-JYU(ID3) =
                           RECE82-NYUKHNKIN-JYU(ID3) +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
      *
                  COMPUTE  RECE82-NYUTOKKIN-JYU(ID3) =
                           RECE82-NYUTOKKIN-JYU(ID3) +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
      *
               END-PERFORM
      *
               MOVE   RECE82-NYUKHNKEI2  TO  WRK-KEISAN2
               COMPUTE  RECE82-NYUKHNKIN2-JYU  =
                        RECE82-NYUKHNKIN2-JYU  +
                        (WRK-KEISAN2 * RECE82-SYUTENTANKA)
               MOVE   RECE82-NYUTOKKEI2  TO  WRK-KEISAN2
               COMPUTE  RECE82-NYUTOKKIN2-JYU  =
                        RECE82-NYUTOKKIN2-JYU  +
                        (WRK-KEISAN2 * RECE82-SYUTENTANKA)
               IF     RECE82-NYUKHNKEI2  <   ZERO
                   COMPUTE  RECE82-NYUKHNKIN2-JYU   =
                            RECE82-NYUKHNKIN2-JYU   *  -1
               END-IF
               IF     RECE82-NYUTOKKEI2  <   ZERO
                   COMPUTE  RECE82-NYUTOKKIN2-JYU   =
                            RECE82-NYUTOKKIN2-JYU   *  -1
               END-IF
      *
               COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                        RECE82-NYUTOTALKIN-JYU    +
                        RECE82-NYUKHNKIN2-JYU
               COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                        RECE82-NYUTOTALKIN-JYU    +
                        RECE82-NYUTOKKIN2-JYU
      *
               COMPUTE   RECE82-KINGAKU-JYU(3)  =
                         RECE82-KINGAKU-JYU(1)  +
                         RECE82-KINGAKU-JYU(2)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(10)  =
                         RECE82-KINGAKU-JYU(4)   +
                         RECE82-KINGAKU-JYU(5)   +
                         RECE82-KINGAKU-JYU(6)   +
                         RECE82-KINGAKU-JYU(7)   +
                         RECE82-KINGAKU-JYU(8)   +
                         RECE82-KINGAKU-JYU(9)    
      *
               COMPUTE   RECE82-KINGAKU-JYU(14)  =
                         RECE82-KINGAKU-JYU(11)  +
                         RECE82-KINGAKU-JYU(12)  +
                         RECE82-KINGAKU-JYU(13)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(17)  =
                         RECE82-KINGAKU-JYU(15)  +
                         RECE82-KINGAKU-JYU(16)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(20)  =
                         RECE82-KINGAKU-JYU(18)  +
                         RECE82-KINGAKU-JYU(19)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(23)  =
                         RECE82-KINGAKU-JYU(21)  +
                         RECE82-KINGAKU-JYU(22)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(26)  =
                         RECE82-KINGAKU-JYU(24)  +
                         RECE82-KINGAKU-JYU(25)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(29)  =
                         RECE82-KINGAKU-JYU(27)  +
                         RECE82-KINGAKU-JYU(28)   
      *
               IF    RECE82-DAISAN-CHIKAN   =   1
                 IF  RECE82-TOTALKIN-JYU < RECE82-FTNMONEY
                   MOVE   RECE82-TOTALKIN-JYU   TO
                                           RECE82-FTNMONEY
                 END-IF
               END-IF
      *
      *        Âè»°¼Ô¹Ô°Ù¤Î¸½ÊªµëÉÕÈ½Äê
               PERFORM   2802-DAISAN-GENBUTU-HANTEI-SEC
      *
               EVALUATE  RECE82-DAISAN-GENBUTU-GAITO
                 WHEN      ZERO
                   IF    RECE82-DAISAN-CHIKAN   =  ZERO
                     IF   RECE82-TOTALKIN-JYU  NOT =  ZERO
                     AND  RECE82-FTNRATE       NOT =  ZERO
                       COMPUTE RECE82-FTNMONEY = RECE82-TOTALKIN-JYU
                                               * RECE82-FTNRATE
                                               / 100
                     END-IF
                     IF   WRK-DAISAN-DAISANKBN  =  "1"
                       COMPUTE  RECE82-FTNMONEY  =
                                RECE82-FTNMONEY  +  5
                       COMPUTE  RECE82-FTNMONEY  =
                                RECE82-FTNMONEY  /  10
                       COMPUTE  RECE82-FTNMONEY  =
                                RECE82-FTNMONEY  *  10
                     END-IF
                   END-IF
                 WHEN       1
                   IF    RECE82-DAISAN-CHIKAN   =  ZERO
                     PERFORM     2801-DAISAN-CHECK-SEC
                     MOVE    WRK-DAISAN-FTNMONEY   TO  RECE82-FTNMONEY
                   END-IF
               END-EVALUATE
               COMPUTE   RECE82-SKYMONEY = RECE82-TOTALKIN-JYU
                                         - RECE82-FTNMONEY
      *
               COMPUTE   RECE82-SKYMONEY = RECE82-SKYMONEY +
                 (RECE82-SHOKUJITOTALKIN - RECE82-SKJFTNMONEY)
      *
           END-IF
      *
           REWRITE   RECE82-REC
      *
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       280-RECE82-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âè»°¼Ô¹Ô°Ù¤Î°ìÉôÉéÃ´¶â·×»»¡ã¸½ÊªµëÉÕ¡ä
      *****************************************************************
       2801-DAISAN-CHECK-SEC      SECTION.
      *
           MOVE     ZERO     TO    WRK-DAISAN-FTNMONEY
      *
           INITIALIZE                       SYUNOU-REC
           MOVE  RECE82-HOSPNUM         TO  SYU-HOSPNUM
           MOVE  RECE82-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE  RECE82-PTID            TO  SYU-PTID
           MOVE  RECE82-SRYYM           TO  SYU-SRYYMD(1:6)
           MOVE  "__"                   TO  SYU-SRYYMD(7:2)
           MOVE  RECE82-HKNCOMBI        TO  SYU-HKNCOMBINUM
           MOVE  SYUNOU-REC             TO  MCPDATA-REC
           MOVE  "tbl_syunou"           TO  MCP-TABLE
           MOVE  "key19"                TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    MCP-RC   =   ZERO
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    "key19"            TO  MCP-PATHNAME
             PERFORM  900-SYUNOU-READ-SEC
             PERFORM  UNTIL  (FLG-SYUNOU  =  1)
               IF    SYU-DENPJTIKBN    NOT =  "3"
                 IF  SYU-TOTAL-HKNTEN  NOT = ZERO
                   COMPUTE  WRK-DAISAN-FTNMONEY  =
                            WRK-DAISAN-FTNMONEY  +
                            SYU-SYUCOMPFTN
                 END-IF
               END-IF
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key19"          TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE      "tbl_syunou"       TO  MCP-TABLE
           MOVE      "key19"            TO  MCP-PATHNAME
           PERFORM    900-CLOSE-SEC
      *
           .
       2801-DAISAN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âè»°¼Ô¹Ô°Ù¤Î¸½ÊªµëÉÕÈ½Äê
      *****************************************************************
       2802-DAISAN-GENBUTU-HANTEI-SEC      SECTION.
      *
      *    ¸½ÊªµëÉÕ·×»»ÂÐ¾Ý¤«¥Á¥§¥Ã¥¯
      *    Á´¤Æ¤Î¼ýÇ¼¤¬¸½ÊªµëÉÕ·×»»¤µ¤ì¤Æ¤¤¤ë¤«¥Á¥§¥Ã¥¯
           MOVE     ZERO       TO     FLG-DAISAN-GENBUTU
           IF    RECE82-DAISAN-GENBUTU-GAITO   =   1
      *
             INITIALIZE                       SYUNOU-REC
             MOVE  RECE82-HOSPNUM         TO  SYU-HOSPNUM
             MOVE  RECE82-NYUGAIKBN       TO  SYU-NYUGAIKBN
             MOVE  RECE82-PTID            TO  SYU-PTID
             MOVE  RECE82-SRYYM           TO  SYU-SRYYMD(1:6)
             MOVE  "__"                   TO  SYU-SRYYMD(7:2)
             MOVE  RECE82-HKNCOMBI        TO  SYU-HKNCOMBINUM
             MOVE  SYUNOU-REC             TO  MCPDATA-REC
             MOVE  "tbl_syunou"           TO  MCP-TABLE
             MOVE  "key19"                TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF    MCP-RC   =   ZERO
               MOVE    "tbl_syunou"       TO  MCP-TABLE
               MOVE    "key19"            TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL  (FLG-SYUNOU  =  1)
                 IF    SYU-DENPJTIKBN    NOT =  "3"
                   IF  SYU-TOTAL-HKNTEN  NOT = ZERO
                   AND SYU-SKJ-FTNGAKU(2)    = ZERO
                     MOVE     1           TO  FLG-DAISAN-GENBUTU
                   END-IF
                 END-IF
                 MOVE    "tbl_syunou"     TO  MCP-TABLE
                 MOVE    "key19"          TO  MCP-PATHNAME
                 PERFORM  900-SYUNOU-READ-SEC
               END-PERFORM
             END-IF
             MOVE      "tbl_syunou"       TO  MCP-TABLE
             MOVE      "key19"            TO  MCP-PATHNAME
             PERFORM    900-CLOSE-SEC
      *
             IF   FLG-DAISAN-GENBUTU    =   1
               MOVE     ZERO       TO     RECE82-DAISAN-GENBUTU-GAITO
             END-IF
      *
           END-IF
      *
           .
       2802-DAISAN-GENBUTU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕ¡Ê¿·ÍÍ¼°¡Ë¡Ê£Ï£Ã£ÒÍÍ¼°¡Ë¤Î¶â³ÛÊÔ½¸
      *****************************************************************
       2803-JIBAI-KINGAKU-HENSYU-SEC      SECTION.
      *
           COMPUTE  WRK-JIBAI-SKYKIN-A   =  RECE82-TOTALKIN
                                         +  RECE82-TOTALKIN-YAK
                                         +  RECE82-TOTALKIN2
                                         +  RECE82-TOTALKIN-ETC
      *
           MOVE     WRK-JIBAI-SKYKIN-A  TO  RECE82-JIBAI-GOKEI-BE
                                            RECE82-JIBAI-GOKEI-AF
      *
           IF       WRK-JIBAI-SKYKIN-A   >  ZERO
             EVALUATE   RECE82-JIBAI-HASUKBN
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
             IF   RECE82-SEIKYUKBN    =  "1"
               MOVE  1                   TO  RECE82-JIBAI-JYUKINFLG
               MOVE  WRK-JIBAI-SKYKIN-A  TO  RECE82-JIBAI-JYUKIN
             ELSE
               MOVE  1                   TO  RECE82-JIBAI-SKYKINFLG
               MOVE  WRK-JIBAI-SKYKIN-A  TO  RECE82-JIBAI-SKYKIN
             END-IF
             MOVE    WRK-JIBAI-SKYKIN-A  TO  RECE82-JIBAI-GOKEI-AF
           END-IF
      *
           IF       RECE82-JIBAI-CHIKANKINFLG  =  1
             IF  WRK-JIBAI-SKYKIN-A  >  ZERO
               IF   WRK-JIBAI-SKYKIN-A  <  RECE82-JIBAI-CHIKANKIN
                 MOVE    WRK-JIBAI-SKYKIN-A  TO  RECE82-JIBAI-CHIKANKIN
               END-IF
               IF   RECE82-JIBAI-CHIKANKIN  >  ZERO
                 MOVE    1                   TO  RECE82-JIBAI-JYUKINFLG
                 MOVE    RECE82-JIBAI-CHIKANKIN
                                             TO  RECE82-JIBAI-JYUKIN
                 COMPUTE WRK-JIBAI-SKYKIN-B = WRK-JIBAI-SKYKIN-A
                                            - RECE82-JIBAI-CHIKANKIN
                 IF    WRK-JIBAI-SKYKIN-B  >  ZERO
                   MOVE  1                   TO  RECE82-JIBAI-SKYKINFLG
                   MOVE  WRK-JIBAI-SKYKIN-B  TO  RECE82-JIBAI-SKYKIN
                 ELSE
                   MOVE  ZERO                TO  RECE82-JIBAI-SKYKINFLG
                   MOVE  ZERO                TO  RECE82-JIBAI-SKYKIN
                 END-IF
               ELSE
                 MOVE    ZERO                TO  RECE82-JIBAI-JYUKINFLG
                 MOVE    ZERO                TO  RECE82-JIBAI-JYUKIN
                 MOVE    1                   TO  RECE82-JIBAI-SKYKINFLG
                 MOVE    WRK-JIBAI-SKYKIN-A  TO  RECE82-JIBAI-SKYKIN
               END-IF
             ELSE
               MOVE      2        TO  RECE82-JIBAI-CHIKANKINFLG
             END-IF
           END-IF
      *
           IF       RECE82-JIBAI-CHIKANNAMEFLG  =  1
             IF  WRK-JIBAI-SKYKIN-A  >  ZERO
               IF     RECE82-JIBAI-JYUKINFLG      =  1
                 MOVE    1        TO  RECE82-JIBAI-JYUNAMEFLG
                 MOVE    RECE82-JIBAI-CHIKANNAME
                                  TO  RECE82-JIBAI-JYUNAME
               ELSE
                 MOVE    2        TO  RECE82-JIBAI-CHIKANNAMEFLG
               END-IF
             ELSE
                 MOVE    2        TO  RECE82-JIBAI-CHIKANNAMEFLG
             END-IF
           END-IF
      *
           .
       2803-JIBAI-KINGAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤áÊÔ½¸
      *****************************************************************
       290-TENTEKIMATOME-HENSYU-SEC      SECTION.
      *
           INITIALIZE                  ORCSTENTEK2AREA
           MOVE    RECE82-HOSPNUM  TO  LNK-ORCSTEN2-HOSPNUM
           MOVE    RECE82-PTID     TO  LNK-ORCSTEN2-PTID
           MOVE    RECE82-SRYYM    TO  LNK-ORCSTEN2-SRYYM
           MOVE    RECE82-HKNCOMBI TO  LNK-ORCSTEN2-HKNCOMBI
           CALL    "ORCSTENTEK2"  USING
                                  ORCSTENTEK2AREA
                                  SPA-AREA
           IF      LNK-ORCSTEN2-RC  =   ZERO
             IF   (LNK-ORCSTEN2-TENTEK-ERRCD  = ZERO)
             OR   (LNK-ORCSTEN2-CHUJOU-ERRCD  = ZERO)
               MOVE  1            TO  WRK-TENTEKI2-HENKYAKUARI
               MOVE  LNK-ORCSTEN2-OUT-AREA
                                  TO  WRK-TENTEKI2-HENKYAKU-AREA
             END-IF
           END-IF
      *
           .
       290-TENTEKIMATOME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÅÀ¿ô¤Î¼£¸³¸ºÅÀÊ¬ÊÔ½¸
      *****************************************************************
       2A-GENTEN-HENSYU-SEC      SECTION.
      *
           EVALUATE   RECE82-HKNKBN
             WHEN       "1"
             WHEN       "2"
               IF   RECE82-TENSU(2)   >=  WRK-RECE8G-TENSU(2)
                 COMPUTE  RECE82-TENSU(2)   =
                          RECE82-TENSU(2)   -  WRK-RECE8G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(2)
               END-IF
               IF   RECE82-TENSU(3)   >=  WRK-RECE8G-TENSU(3)
                 COMPUTE  RECE82-TENSU(3)   =
                          RECE82-TENSU(3)   -  WRK-RECE8G-TENSU(3)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(3)
               END-IF
               IF   WRK-RECE8C-TENSU(12)  >=  WRK-RECE8G-TENSU(12)
                 COMPUTE  RECE82-TENSU(12)  =
                          RECE82-TENSU(12)  -  WRK-RECE8G-TENSU(12)
               ELSE
                 COMPUTE  RECE82-TENSU(12)  =
                          RECE82-TENSU(12)  -  WRK-RECE8C-TENSU(12)
                 COMPUTE  WRK-RECE8G-TENSU(12) =
                          WRK-RECE8G-TENSU(12) -
                          WRK-RECE8C-TENSU(12)
                 IF  WRK-RECE8C-TENSU(11)  >=   WRK-RECE8G-TENSU(12)
                   COMPUTE  RECE82-TENSU(11)  =
                            RECE82-TENSU(11)  -  WRK-RECE8G-TENSU(12)
                 ELSE
                   COMPUTE  RECE82-TENSU(11)  =
                            RECE82-TENSU(11)  -  WRK-RECE8C-TENSU(11)
                   COMPUTE  WRK-RECE8G-TENSU(12) =
                            WRK-RECE8G-TENSU(12) -
                            WRK-RECE8C-TENSU(11)
                   IF  WRK-RECE8C-TENSU(10)  >=   WRK-RECE8G-TENSU(12)
                     COMPUTE  RECE82-TENSU(10)  =
                              RECE82-TENSU(10)  -  WRK-RECE8G-TENSU(12)
                   ELSE
                     COMPUTE  RECE82-TENSU(10)  =
                              RECE82-TENSU(10)  -  WRK-RECE8C-TENSU(10)
                   END-IF
                 END-IF
               END-IF
             WHEN       "4"
               EVALUATE  WRK-PARA-RECEKBN
                WHEN      "4"
                 IF   WRK-PARA-JIBAIPRTKBN  =  "2"
                  IF   RECE82-TENSU(2)   >=  WRK-RECE8G-TENSU(2)
                    COMPUTE  RECE82-TENSU(2)   =
                             RECE82-TENSU(2)   -  WRK-RECE8G-TENSU(2)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(2)
                  END-IF
                  IF   RECE82-TENSU(21)  >=  WRK-RECE8G-TENSU(21)
                    COMPUTE  RECE82-TENSU(21)  =
                             RECE82-TENSU(21)  -  WRK-RECE8G-TENSU(21)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(21)
                  END-IF
                  IF   WRK-RECE8C-TENSU-YAK(12)  >=
                                              WRK-RECE8G-TENSU-YAK(12)
                    COMPUTE  RECE82-TENSU-YAK(12)  =
                             RECE82-TENSU-YAK(12)  -
                             WRK-RECE8G-TENSU-YAK(12)
                  ELSE
                    COMPUTE  RECE82-TENSU-YAK(12)  =
                             RECE82-TENSU-YAK(12)  -
                             WRK-RECE8C-TENSU-YAK(12)
                  END-IF
                 ELSE
                  IF   RECE82-TENSU(2)   >=  WRK-RECE8G-TENSU(2)
                    COMPUTE  RECE82-TENSU(2)   =
                             RECE82-TENSU(2)   -  WRK-RECE8G-TENSU(2)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(2)
                  END-IF
                  IF   RECE82-TENSU(22)  >=  WRK-RECE8G-TENSU(22)
                    COMPUTE  RECE82-TENSU(22)  =
                             RECE82-TENSU(22)  -  WRK-RECE8G-TENSU(22)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(22)
                  END-IF
                  IF   WRK-RECE8C-TENSU-YAK(13)  >=
                                              WRK-RECE8G-TENSU-YAK(13)
                    COMPUTE  RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  -
                             WRK-RECE8G-TENSU-YAK(13)
                  ELSE
                    COMPUTE  RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  -
                             WRK-RECE8C-TENSU-YAK(13)
                  END-IF
                 END-IF
                WHEN      "5"
                  IF   RECE82-TENSU(2)   >=  WRK-RECE8G-TENSU(2)
                    COMPUTE  RECE82-TENSU(2)   =
                             RECE82-TENSU(2)   -  WRK-RECE8G-TENSU(2)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(2)
                  END-IF
                  IF   RECE82-TENSU(27)  >=  WRK-RECE8G-TENSU(27)
                    COMPUTE  RECE82-TENSU(27)  =
                             RECE82-TENSU(27)  -  WRK-RECE8G-TENSU(27)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(27)
                  END-IF
                  IF   RECE82-TENSU(28)  >=  WRK-RECE8G-TENSU(28)
                    COMPUTE  RECE82-TENSU(28)  =
                             RECE82-TENSU(28)  -  WRK-RECE8G-TENSU(28)
                  ELSE
                    MOVE     ZERO    TO   RECE82-TENSU(28)
                  END-IF
                  MOVE   WRK-RECE8G-TENSU(13)  TO   WRK-GENTEN-YKZTEN
                  IF   WRK-RECE8C-TENSU(13)  >=  WRK-RECE8G-TENSU(13)
                    COMPUTE  RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  -  WRK-RECE8G-TENSU(13)
                  ELSE
                    COMPUTE  RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  -  WRK-RECE8C-TENSU(13)
                    COMPUTE  WRK-RECE8G-TENSU(13) =
                             WRK-RECE8G-TENSU(13) -
                             WRK-RECE8C-TENSU(13)
                    IF  WRK-RECE8C-TENSU(12)  >= WRK-RECE8G-TENSU(13)
                      COMPUTE  RECE82-TENSU(12)  =
                               RECE82-TENSU(12)  -
                               WRK-RECE8G-TENSU(13)
                    ELSE
                      COMPUTE  RECE82-TENSU(12)  =
                               RECE82-TENSU(12)  -
                               WRK-RECE8C-TENSU(12)
                      COMPUTE  WRK-RECE8G-TENSU(13) =
                               WRK-RECE8G-TENSU(13) -
                               WRK-RECE8C-TENSU(12)
                      IF  WRK-RECE8C-TENSU(11)  >=
                                                  WRK-RECE8G-TENSU(13)
                        COMPUTE  RECE82-TENSU(11)  =
                                 RECE82-TENSU(11)  -
                                 WRK-RECE8G-TENSU(13)
                      ELSE
                        COMPUTE  RECE82-TENSU(11)  =
                                 RECE82-TENSU(11)  -
                                 WRK-RECE8C-TENSU(11)
                      END-IF
                    END-IF
                  END-IF
               END-EVALUATE
             WHEN       "5"
               IF   RECE82-TENSU(9)   >=  WRK-RECE8G-TENSU(9)
                 COMPUTE  RECE82-TENSU(9)   =
                          RECE82-TENSU(9)   -  WRK-RECE8G-TENSU(9)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(9)
               END-IF
               IF   RECE82-TENSU(14)  >=  WRK-RECE8G-TENSU(14)
                 COMPUTE  RECE82-TENSU(14)  =
                          RECE82-TENSU(14)  -  WRK-RECE8G-TENSU(14)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(14)
               END-IF
               IF   WRK-RECE8C-TENSU(27)  >=  WRK-RECE8G-TENSU(27)
                 COMPUTE  RECE82-TENSU(27)  =
                          RECE82-TENSU(27)  -  WRK-RECE8G-TENSU(27)
               ELSE
                 COMPUTE  RECE82-TENSU(27)  =
                          RECE82-TENSU(27)  -  WRK-RECE8C-TENSU(27)
                 COMPUTE  WRK-RECE8G-TENSU(27) =
                          WRK-RECE8G-TENSU(27) -
                          WRK-RECE8C-TENSU(27)
                 IF  WRK-RECE8C-TENSU(26)  >=   WRK-RECE8G-TENSU(27)
                   COMPUTE  RECE82-TENSU(26)  =
                            RECE82-TENSU(26)  -  WRK-RECE8G-TENSU(27)
                 ELSE
                   COMPUTE  RECE82-TENSU(26)  =
                            RECE82-TENSU(26)  -  WRK-RECE8C-TENSU(26)
                   COMPUTE  WRK-RECE8G-TENSU(27) =
                            WRK-RECE8G-TENSU(27) -
                            WRK-RECE8C-TENSU(26)
                   IF  WRK-RECE8C-TENSU(25)  >=   WRK-RECE8G-TENSU(27)
                     COMPUTE  RECE82-TENSU(25)  =
                              RECE82-TENSU(25)  -  WRK-RECE8G-TENSU(27)
                   ELSE
                     COMPUTE  RECE82-TENSU(25)  =
                              RECE82-TENSU(25)  -  WRK-RECE8C-TENSU(25)
                   END-IF
                 END-IF
               END-IF
             WHEN       "6"
               IF   RECE82-TENSU(2)   >=  WRK-RECE8G-TENSU(2)
                 COMPUTE  RECE82-TENSU(2)   =
                          RECE82-TENSU(2)   -  WRK-RECE8G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(2)
               END-IF
               IF   RECE82-TENSU(27)  >=  WRK-RECE8G-TENSU(27)
                 COMPUTE  RECE82-TENSU(27)  =
                          RECE82-TENSU(27)  -  WRK-RECE8G-TENSU(27)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(27)
               END-IF
               IF   RECE82-TENSU(28)  >=  WRK-RECE8G-TENSU(28)
                 COMPUTE  RECE82-TENSU(28)  =
                          RECE82-TENSU(28)  -  WRK-RECE8G-TENSU(28)
               ELSE
                 MOVE     ZERO    TO   RECE82-TENSU(28)
               END-IF
               MOVE   WRK-RECE8G-TENSU(13)  TO   WRK-GENTEN-YKZTEN
               IF   WRK-RECE8C-TENSU(13)  >=  WRK-RECE8G-TENSU(13)
                 COMPUTE  RECE82-TENSU(13)  =
                          RECE82-TENSU(13)  -  WRK-RECE8G-TENSU(13)
               ELSE
                 COMPUTE  RECE82-TENSU(13)  =
                          RECE82-TENSU(13)  -  WRK-RECE8C-TENSU(13)
                 COMPUTE  WRK-RECE8G-TENSU(13) =
                          WRK-RECE8G-TENSU(13) -
                          WRK-RECE8C-TENSU(13)
                 IF  WRK-RECE8C-TENSU(12)  >= WRK-RECE8G-TENSU(13)
                   COMPUTE  RECE82-TENSU(12)  =
                            RECE82-TENSU(12)  -
                            WRK-RECE8G-TENSU(13)
                 ELSE
                   COMPUTE  RECE82-TENSU(12)  =
                            RECE82-TENSU(12)  -
                            WRK-RECE8C-TENSU(12)
                   COMPUTE  WRK-RECE8G-TENSU(13) =
                            WRK-RECE8G-TENSU(13) -
                            WRK-RECE8C-TENSU(12)
                   IF  WRK-RECE8C-TENSU(11)  >=
                                               WRK-RECE8G-TENSU(13)
                     COMPUTE  RECE82-TENSU(11)  =
                              RECE82-TENSU(11)  -
                              WRK-RECE8G-TENSU(13)
                   ELSE
                     COMPUTE  RECE82-TENSU(11)  =
                              RECE82-TENSU(11)  -
                              WRK-RECE8C-TENSU(11)
                   END-IF
                 END-IF
               END-IF
           END-EVALUATE
      *
           .
       2A-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕ¡¦ÀÁµá¶â³ÛÍóÃ¼¿ô½èÍý¶èÊ¬ÊÔ½¸
      *****************************************************************
       2B-JIBAI-HASUKBN-HENSYU-SEC               SECTION.
      *
           EVALUATE   RECE82-HKNKBN
             WHEN       "4"
               EVALUATE   SYS-200501-JIBAI-FRACTION-KBN(1)
                 WHEN      "1"
                   IF   WRK-HASU1001-JIBAI   NOT = "4"
                     MOVE  WRK-HASU1001-JIBAI   TO  RECE82-JIBAI-HASUKBN
                   END-IF
                 WHEN      "2"
                   IF   RECE82-SEIKYUKBN         = "1"
                   AND  WRK-HASU1001-JIBAI   NOT = "4"
                     MOVE  WRK-HASU1001-JIBAI   TO  RECE82-JIBAI-HASUKBN
                   END-IF
               END-EVALUATE
             WHEN       "6"
               EVALUATE   SYS-200501-JIBAI-FRACTION-KBN(2)
                 WHEN      "1"
                   IF   WRK-HASU1001-DAISAN  NOT = "4"
                     MOVE  WRK-HASU1001-DAISAN  TO  RECE82-JIBAI-HASUKBN
                   END-IF
                 WHEN      "2"
                   IF   RECE82-SEIKYUKBN         = "1"
                   AND  WRK-HASU1001-DAISAN  NOT = "4"
                     MOVE  WRK-HASU1001-DAISAN  TO  RECE82-JIBAI-HASUKBN
                   END-IF
               END-EVALUATE
           END-EVALUATE
      *
           .
       2B-JIBAI-HASUKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       900-RECE84-WRITE-SEC                SECTION.
      *
           MOVE     RECE84-REC      TO  RECE8A-REC
           MOVE     RECE84-REC      TO  RECE84C-REC
      *
           COMPUTE  RECE84-ZAITEN      =  RECE84-ZAITEN     -  
                                          WRK-TEN
           COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU   -
                                          WRK-MEISAI-CNT
           COMPUTE  RECE84-SYUTEN1     =  RECE84-SYUTEN1    -
                                          WRK-TEN
           COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL -
                                          WRK-SRYCDTOTAL
           COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL -
                                          WRK-SURYOUTOTAL     
      *
           MOVE     WRK-TEN         TO  RECE8A-ZAITEN
           MOVE     WRK-MEISAI-CNT  TO  RECE8A-MEISAISU
           MOVE     WRK-TEN         TO  RECE8A-SYUTEN1  
           MOVE     WRK-SRYCDTOTAL  TO  RECE8A-SRYCDTOTAL
           MOVE     WRK-SURYOUTOTAL TO  RECE8A-SURYOUTOTAL
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *      ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿ÉÕ²Ã¾ðÊóÀßÄê
             PERFORM   920-SRYACCTPLUS-HENSYU-SEC
           END-IF
      *
           MOVE    RECE84-ZAITEN    TO  RECE84-HOKATU-ZAITEN
           MOVE    RECE84-SYUTEN1   TO  RECE84-HOKATU-SYUTEN1
           MOVE    RECE84-SYUTEN2   TO  RECE84-HOKATU-SYUTEN2
           MOVE    RECE84-YKZTEN    TO  RECE84-HOKATU-YKZTEN
           MOVE    RECE84-KIZAITEN  TO  RECE84-HOKATU-KIZAITEN
      *
           MOVE    RECE8A-ZAITEN    TO  RECE8A-HOKATU-ZAITEN
           MOVE    RECE8A-SYUTEN1   TO  RECE8A-HOKATU-SYUTEN1
           MOVE    RECE8A-SYUTEN2   TO  RECE8A-HOKATU-SYUTEN2
           MOVE    RECE8A-YKZTEN    TO  RECE8A-HOKATU-YKZTEN
           MOVE    RECE8A-KIZAITEN  TO  RECE8A-HOKATU-KIZAITEN
      *
           WRITE   RECE84-REC
      *
           IF      STS-RECE84          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE84PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE84          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                TO  CNT-OUT1
      *
           WRITE   RECE8A-REC
      *
           IF      STS-RECE8A          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE8APARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE8A          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                TO  CNT-OUT4
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
      *
             COMPUTE  RECE84C-ZAI-TENSU  =  RECE84C-ZAITEN
                                         -  WRK-TEN
             MOVE  WRK-ZAI-KINGAKU  TO  RECE84C-ZAI-KINGAKU
      *
             COMPUTE  RECE84C-ZAITEN     =  RECE84C-ZAITEN
                                         -  WRK-TEN
             COMPUTE  RECE84C-SYUTEN1    =  RECE84C-SYUTEN1
                                         -  WRK-TEN
      *
             MOVE  RECE84C-ZAITEN   TO  RECE84C-HOKATU-ZAITEN
             MOVE  RECE84C-SYUTEN1  TO  RECE84C-HOKATU-SYUTEN1
             MOVE  RECE84C-SYUTEN2  TO  RECE84C-HOKATU-SYUTEN2
             MOVE  RECE84C-YKZTEN   TO  RECE84C-HOKATU-YKZTEN
             MOVE  RECE84C-KIZAITEN TO  RECE84C-HOKATU-KIZAITEN
      *
             WRITE RECE84C-REC
      *
             IF      STS-RECE84C    =   "00"
                 CONTINUE
             ELSE
                 MOVE    SPACE               TO  WRK-ERR-AREA
                 INITIALIZE                      WRK-ERR-AREA
      *
                 MOVE    3                   TO  WRK-ERR-FLG
                 MOVE    RECE84CPARA         TO  WRK-ERR-FILE-NM
                 MOVE    STS-RECE84C         TO  WRK-ERR-FILE-STS
                 PERFORM 500-FILE-ERR-ABORT-SEC
             END-IF
      *
             ADD   1                TO  CNT-OUT6
      *
           END-IF
      *
           .
       900-RECE84-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       900-RECE85-WRITE-SEC                SECTION.
      *
           MOVE    RECE85-REC          TO   RECE8B-REC      
           MOVE    RECE85-REC          TO   RECE85C-REC
      *
           WRITE   RECE85-REC
       *
           IF      STS-RECE85          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE85PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE85          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO   CNT-OUT2
      *
           WRITE   RECE8B-REC
      *
           IF      STS-RECE8B          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE8BPARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE8B          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO   CNT-OUT5
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
             WRITE RECE85C-REC
      *
             IF      STS-RECE85C    =   "00"
                 CONTINUE
             ELSE
                 MOVE    SPACE               TO  WRK-ERR-AREA
                 INITIALIZE                      WRK-ERR-AREA
      *
                 MOVE    3                   TO  WRK-ERR-FLG
                 MOVE    RECE85CPARA         TO  WRK-ERR-FILE-NM
                 MOVE    STS-RECE85C         TO  WRK-ERR-FILE-STS
                 PERFORM 500-FILE-ERR-ABORT-SEC
             END-IF
      *
             ADD   1                   TO   CNT-OUT7
           END-IF
      *
           .
       900-RECE85-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       900-RECE85-REWRITE-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPNUM      TO  RECE85-HOSPNUM
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID     TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                    KEY IS    >=   RECE85-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
           END-START
           PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85-HOSPNUM)     OR
               (RECE84-SRYYM        NOT =  RECE85-SRYYM)       OR
               (RECE84-NYUGAIKBN    NOT =  RECE85-NYUGAIKBN)   OR
               (RECE84-PTID         NOT =  RECE85-PTID)        OR
               (RECE84-RECEKA       NOT =  RECE85-RECEKA)      OR
               (RECE84-HKNID        NOT =  RECE85-HKNID)    OR
               (RECE84-TEKSTYMD     NOT =  RECE85-TEKSTYMD)    OR
               (RECE84-ZAINUM       NOT =  RECE85-ZAINUM)      OR
               (FLG-RECE85-ZAN            =      1        )    OR
               (FLG-REWRITE               =      1        )
                   IF    RECE85-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE85-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85-REC
      *
                      IF      STS-RECE85          =   "00"
                          CONTINUE
                      ELSE
                          MOVE    SPACE               TO  WRK-ERR-AREA
                          INITIALIZE                      WRK-ERR-AREA
      *
                          MOVE    4                   TO  WRK-ERR-FLG
                          MOVE    RECE85PARA          TO
                                                      WRK-ERR-FILE-NM
                          MOVE    STS-RECE85          TO
                                                      WRK-ERR-FILE-STS
                          PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
      *
                   END-IF
                   READ    RECE85-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
           END-PERFORM
      *
      *****
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
      *
             MOVE    ZERO                TO  FLG-REWRITE
      *
             INITIALIZE                      RECE85C-REC
             MOVE    RECE84-HOSPNUM      TO  RECE85C-HOSPNUM
             MOVE    RECE84-SRYYM        TO  RECE85C-SRYYM
             MOVE    RECE84-NYUGAIKBN    TO  RECE85C-NYUGAIKBN
             MOVE    RECE84-PTID         TO  RECE85C-PTID
             MOVE    RECE84-RECEKA       TO  RECE85C-RECEKA
             MOVE    RECE84-HKNID        TO  RECE85C-HKNID
             MOVE    RECE84-TEKSTYMD     TO  RECE85C-TEKSTYMD
             MOVE    RECE84-ZAINUM       TO  RECE85C-ZAINUM
             START   RECE85C-FILE
                    KEY IS    >=   RECE85C-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85C-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
             END-START
             PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85C-HOSPNUM  ) OR
               (RECE84-SRYYM        NOT =  RECE85C-SRYYM    ) OR
               (RECE84-NYUGAIKBN    NOT =  RECE85C-NYUGAIKBN) OR
               (RECE84-PTID         NOT =  RECE85C-PTID     ) OR
               (RECE84-RECEKA       NOT =  RECE85C-RECEKA   ) OR
               (RECE84-HKNID        NOT =  RECE85C-HKNID    ) OR
               (RECE84-TEKSTYMD     NOT =  RECE85C-TEKSTYMD ) OR
               (RECE84-ZAINUM       NOT =  RECE85C-ZAINUM   ) OR
               (FLG-RECE85-ZAN          =      1            ) OR
               (FLG-REWRITE             =      1            )
                   IF    RECE85C-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85C-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE85C-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85C-REC
      *
                      IF      STS-RECE85C    =   "00"
                        CONTINUE
                      ELSE
                        MOVE    SPACE        TO  WRK-ERR-AREA
                        INITIALIZE               WRK-ERR-AREA
      *
                        MOVE    4            TO  WRK-ERR-FLG
                        MOVE    RECE85CPARA  TO  WRK-ERR-FILE-NM
                        MOVE    STS-RECE85C  TO  WRK-ERR-FILE-STS
                        PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
                   END-IF
                   READ    RECE85C-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
             END-PERFORM
      *
           END-IF
      *
           .
       900-RECE85-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       900-RECE85-REWRITE2-SEC              SECTION.                        
      *REWRITE
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPNUM      TO  RECE85-HOSPNUM
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID     TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                    KEY IS    >=   RECE85-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
           END-START
           PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85-HOSPNUM)     OR
               (RECE84-SRYYM        NOT =  RECE85-SRYYM)       OR
               (RECE84-NYUGAIKBN    NOT =  RECE85-NYUGAIKBN)   OR
               (RECE84-PTID         NOT =  RECE85-PTID)        OR
               (RECE84-RECEKA       NOT =  RECE85-RECEKA)      OR
               (RECE84-HKNID        NOT =  RECE85-HKNID)    OR
               (RECE84-TEKSTYMD     NOT =  RECE85-TEKSTYMD)    OR
               (RECE84-ZAINUM       NOT =  RECE85-ZAINUM)      OR
               (FLG-RECE85-ZAN            =      1        )    OR
               (FLG-REWRITE               =      1        )
                   IF    RECE85-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE85-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85-REC
      *
                      IF      STS-RECE85          =   "00"
                          CONTINUE
                      ELSE
                          MOVE    SPACE               TO  WRK-ERR-AREA
                          INITIALIZE                      WRK-ERR-AREA
      *
                          MOVE    4                   TO  WRK-ERR-FLG
                          MOVE    RECE85PARA          TO
                                                      WRK-ERR-FILE-NM
                          MOVE    STS-RECE85          TO
                                                      WRK-ERR-FILE-STS
                          PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
      *
                   END-IF
                   READ    RECE85-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
           END-PERFORM
      *
      *****
      *
           IF  (RECE82-SRYYM        <   201307      )
           OR  (RECE82-PTNUM        =   SPACE       )
           OR  (RECE82-TSTPTNUMKBN  =   "1"         )
           OR  (RECE82-RECESYUBETU  =   ZERO OR 7061)
           OR  (RECE82-RSIIRYOKBN   =   "0"         )
             CONTINUE
           ELSE
      *
             MOVE    ZERO                TO  FLG-REWRITE
      *
             INITIALIZE                      RECE85C-REC
             MOVE    RECE84-HOSPNUM      TO  RECE85C-HOSPNUM
             MOVE    RECE84-SRYYM        TO  RECE85C-SRYYM
             MOVE    RECE84-NYUGAIKBN    TO  RECE85C-NYUGAIKBN
             MOVE    RECE84-PTID         TO  RECE85C-PTID
             MOVE    RECE84-RECEKA       TO  RECE85C-RECEKA
             MOVE    RECE84-HKNID        TO  RECE85C-HKNID
             MOVE    RECE84-TEKSTYMD     TO  RECE85C-TEKSTYMD
             MOVE    RECE84-ZAINUM       TO  RECE85C-ZAINUM
             START   RECE85C-FILE
                    KEY IS    >=   RECE85C-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85C-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
             END-START
             PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85C-HOSPNUM  ) OR
               (RECE84-SRYYM        NOT =  RECE85C-SRYYM    ) OR
               (RECE84-NYUGAIKBN    NOT =  RECE85C-NYUGAIKBN) OR
               (RECE84-PTID         NOT =  RECE85C-PTID     ) OR
               (RECE84-RECEKA       NOT =  RECE85C-RECEKA   ) OR
               (RECE84-HKNID        NOT =  RECE85C-HKNID    ) OR
               (RECE84-TEKSTYMD     NOT =  RECE85C-TEKSTYMD ) OR
               (RECE84-ZAINUM       NOT =  RECE85C-ZAINUM   ) OR
               (FLG-RECE85-ZAN          =      1            ) OR
               (FLG-REWRITE             =      1            )
                   IF    RECE85C-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85C-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE85C-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85C-REC
      *
                      IF      STS-RECE85C      =   "00"
                          CONTINUE
                      ELSE
                          MOVE    SPACE        TO  WRK-ERR-AREA
                          INITIALIZE               WRK-ERR-AREA
      *
                          MOVE    4            TO  WRK-ERR-FLG
                          MOVE    RECE85CPARA  TO  WRK-ERR-FILE-NM
                          MOVE    STS-RECE85C  TO  WRK-ERR-FILE-STS
                          PERFORM 500-FILE-ERR-ABORT-SEC
                      END-IF
                   END-IF
                   READ    RECE85C-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
             END-PERFORM
      *
           END-IF
      *
           .
       900-RECE85-REWRITE2-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
                   RECE84-FILE
                   RECE85-FILE
                   RECE8A-FILE
                   RECE8B-FILE
                   RECE84C-FILE
                   RECE85C-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82-IN  :" CNT-IN   
           DISPLAY "RECE82-MOD :" CNT-OUT3  
           DISPLAY "RECE84-OUT :" CNT-OUT1 
           DISPLAY "RECE85-OUT :" CNT-OUT2  
           DISPLAY "RECE8A-OUT :" CNT-OUT4 
           DISPLAY "RECE8B-OUT :" CNT-OUT5  
           DISPLAY "RECE84C-OUT :" CNT-OUT6 
           DISPLAY "RECE85C-OUT :" CNT-OUT7  
           DISPLAY "*** ORCR0840 END ***"
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
      *            ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
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
      *    ¥Õ¥¡¥¤¥ë¥¨¥é¡¼½èÍý
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "¥Õ¥¡¥¤¥ëÆÉ¤ß¹þ¤ß¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "¥Õ¥¡¥¤¥ë½ñ¤­¹þ¤ß¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "¥Õ¥¡¥¤¥ë¹¹¿·¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0840 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
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
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸ÊÔ½¸½èÍý
      *****************************************************************
       600-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
      *
           MOVE    RECE82-PTNUM        TO  ERR-PTNUM
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
                      "¡Î´µ¼ÔÈÖ¹æ¡§"      DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "¡Ï"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           ELSE
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "¡Î´µ¼ÔÈÖ¹æ:"       DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "¡Ï"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           END-IF
      *
           .
       600-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       900-RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   IF      STS-RECE82          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE82 "  " PTNUM=" RECE82-PTNUM
                                      " (PTID=" RECE82-PTID ")"
      *
                   MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE82-READ-EXT.
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
      *        ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£±£°£°£±¡Ë¤«¤é¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
      *        ¤Î¼èÆÀ
               MOVE    SPACE      TO  WRK-TAX1001-KBN
      *
               MOVE  ZERO         TO  FLG-SET
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
                  STRING
                  "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
      *
      *        »ÜÀß´ð½à¾ðÊó
               MOVE    ZERO         TO  FLG-SET
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
               MOVE    ZERO         TO  FLG-SET
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
      *        Ä¢É¼ÊÔ½¸¶èÊ¬¾ðÊó
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
               MOVE      RECE82-SRYYM   TO  WRK-SRYYM-5000
               PERFORM   120-SYS-5000-CHK-SEC
      *
      *        ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
               INITIALIZE             WRK-SHOKUJIRYOYO-OCC
                                      WRK-TOKUKANRIKASAN-OCC
      *
               MOVE  ZERO         TO  FLG-SET
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                  MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                  IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                  AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                     MOVE     1       TO    FLG-SET
                     PERFORM  110-SYSKAN-HENSYU-SEC
                     IF        WRK-SYS5000-COUNT    =     ZERO
                        CONTINUE
                     ELSE
                        PERFORM  130-SYS-5000-GET-SEC
                     END-IF
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
              "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
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
           IF  RECE84-SRYKBN  =  "13" OR "14"
             CONTINUE
           ELSE
             GO  TO  920-SRYACCTPLUS-HENSYU-EXT
           END-IF
           IF  RECE84-ZAIREQFLG  =  "1"
             CONTINUE
           ELSE
             GO  TO  920-SRYACCTPLUS-HENSYU-EXT
           END-IF
      *
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    RECE84-HOSPNUM     TO   ACCTP-HOSPNUM
           MOVE    RECE84-NYUGAIKBN   TO   ACCTP-NYUGAIKBN
           MOVE    RECE84-PTID        TO   ACCTP-PTID
           MOVE    RECE84-SRYKA       TO   ACCTP-SRYKA
           MOVE    RECE84-SRYYM       TO   ACCTP-SRYYM
           MOVE    RECE84-ZAINUM      TO   ACCTP-ZAINUM
           MOVE    1                  TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC    TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
             MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             PERFORM  900-SRYACCTPLUS-READ-SEC
             IF        FLG-SRYACCTPLUS   =   ZERO
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE84-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE84-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE84-ACCTP-CHISOTEN
      *
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE8A-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE8A-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE8A-ACCTP-CHISOTEN
      *
               MOVE   ACCTP-SYUGI-TBL-G
                                      TO   RECE84C-ACCTP-SYUGI-TBL-G
               MOVE   ACCTP-SANSOTEN  TO   RECE84C-ACCTP-SANSOTEN
               MOVE   ACCTP-CHISOTEN  TO   RECE84C-ACCTP-CHISOTEN
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
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                      SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
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
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACCTPLUS-READ-SEC           SECTION.
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
       900-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
               INITIALIZE                      NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       900-NYUINACT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACT
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
               INITIALIZE                      NYUINACT-REC
           END-IF
      *
           .
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           MOVE      ZERO                TO   FLG-TENSU
           MOVE      WRK-SRYCD           TO   TTNS-CHK-SRYCD
           MOVE      WK-TAIHI-SRYYMD     TO   TTNS-CHK-YMD
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯
           PERFORM   9001-TTNS-CHK-SEC
      *
      *    ÂàÈò¥ï¡¼¥¯¤ËÂ¸ºß¤·¤Ê¤¤¾ì¹ç¤Ï£Ä£Â¤ò¸¡º÷
           IF    TTNS-FLG  =   ZERO
             INITIALIZE                       TNS-TENSU-REC
             MOVE    SPA-HOSPNUM         TO   TNS-HOSPNUM
             MOVE    WRK-SRYCD           TO   TNS-SRYCD
             MOVE    WK-TAIHI-SRYYMD     TO   TNS-YUKOSTYMD
                                              TNS-YUKOEDYMD
             MOVE    TNS-TENSU-REC       TO   MCPDATA-REC
             MOVE    "tbl_tensu"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF      MCP-RC     =  ZERO
                MOVE    "tbl_tensu"      TO   MCP-TABLE
                MOVE    "key"            TO   MCP-PATHNAME
                PERFORM  910-TENSU-READ-SEC
                IF    FLG-TENSU  =  1
                  DISPLAY  "TENSU FETCH ERR"
                ELSE
      *           ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¤ËÊÔ½¸¤¹¤ë
                  PERFORM   9001-TTNS-HEN-SEC
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
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
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
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
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
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       910-TENSU-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-TENSU
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÆÉ¹þ
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUNOU
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
               INITIALIZE                      SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1           TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
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
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
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
                               RECE82-PTNUM        DELIMITED  BY  SPACE
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
                       STRING  "ORCR0840 "         DELIMITED  BY  SIZE
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
       900-PTCOM-READ-EXT.
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
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "£Ä£ÂÆÉ¤ß¹þ¤ß¥¨¥é¡¼¡¡TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0840 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
