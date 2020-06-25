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
       PROGRAM-ID.             SOKATU3415.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ·î¼¡Ä¢É¼
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¹­Åç¡¦¹­°èÏ¢¹ç¡¦¿ÇÎÅÊó½·ÀÁµá½ñ
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  08/05/01    µÈÀî          ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô  ÆüÉÕ        ÆâÍÆ
      *  01.00.01    µÈÀî    2008/05/03  ÊÝ¸±¼ÔÈÖ¹æÄÉ²Ã
      *                                  ºßÂð¤Ï¤Þ¤È¤á¤Æ°õ»ú¤¹¤ë
      *  01.00.02    µÈÀî    2008/05/20  ¤Þ¤È¤á¸øÈñ¤ò»ÈÍÑ¤·¤¿»þ¤Î
      *                                  ¤Þ¤È¤á¸øÈñ°Ê³°¤Î¸øÈñÌ¾¾Î½¤Àµ
      *  01.00.03    µÈÀî    2008/08/04  Àº¿ÀÊä½õ¡Ê£±£¹£³¡Ë¤òºÆ·ÇÍó¤Ë
      *                                  µ­ºÜ¤·¤Ê¤¤¤è¤¦¤Ë½¤Àµ
      *  01.00.04    µÈÀî    2009/12/25  ¥·¥¹´É¡Ö2007¡×¤Î¤Þ¤È¤á¸øÈñ¤Ç
      *                                  6¤ÄÌÜ°Ê¹ß¤¬¤Þ¤È¤Þ¤é¤Ê¤¤ÉÔ¶ñ¹ç¤ò½¤Àµ
      *  01.00.05    µÈÀî    2010/03/31  ¹­°èÏ¢¹ç¤ÎÊÝ¸±¼ÔÈÖ¹æ»ØÄêÂÐ±þ
      *  01.00.06    µÈÀî    2010/04/07  ¥ª¥ó¥é¥¤¥óÊÖÌáÂÐ±þ
      *  01.00.07    µÈÀî    2012/11/19  PERFORM¤òGOTO¤Ë½¤Àµ
      *  01.00.08    µÈÀî    2013/02/06  °åÎÅµ¡´Ø¾ðÊóÊÔ½¸ÂÐ±þ
      *  01.00.09    µÈÀî    2014/10/27  ver4.8.0ÂÐ±þ¡Ê°ì»þ¥Ç¥£¥ì¥¯¥È¥êÊÑ¹¹¡Ë
      *  01.00.10    Ìç´Ö    2016/02/04  °åÎÅµ¡´ØÊÔ½¸¾ðÊó¤Ø¤Î³«Àß¼ÔÌ¾ÄÉ²ÃÂÐ±þ
      *  01.00.11    ÇëÌî    2017/11/08  ¸øÈñÈÖ¹æ¤Þ¤È¤á¾ðÊó¤Î¸øÈñÈÖ¹æ¤ÎÄÉ²Ã
      *                                  °åÎÅµ¡´Ø¥³¡¼¥ÉÊÑ¹¹»þ¡¢
      *                                  °åÎÅµ¡´Ø¥³¡¼¥ÉÊÌ¤ÎºîÀ®½èÍýÂÐ±þ
      *                                  SITEKBNÊÑ¹¹¡ÊÆ°ºî¤ËÊÑ¹¹¤Ê¤·¡Ë
      *  01.00.12    °æ¾å    2019/01/09  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPRECEERR.INC".
      *     
      *    ¥·¥§¥ëÍÑÎÎ°è
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "SKT3415.INC".
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KOH-END         PIC 9(01).
           03  FLG-UPD             PIC 9(01).
      *     
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01). 
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01). 
           03  FLG-PTINF           PIC 9(01).
      *     
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
      *
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
      *     
           03  IDZ                 PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-SYOKBN1    PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *  
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-CNT             PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-HKNJANAME       PIC X(10).
      *
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-KOHNUM9         PIC Z9.
           03  WRK-PAGE            PIC ZZZ9.
      *
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
      *
           03  WRK-RECEERR         PIC X(200). 
           03  WRK-ERRMSG          PIC X(300). 
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SYOKBN          PIC X(01).
           03  WRK-INDEX           PIC 9(02).
      *
      *    ¸øÈñÌ¾¾ÎÊÔ½¸
           03  WRK-KOHKBN.
               05  WRK-KOHNUM        PIC X(03).
               05  WRK-HKNNAME       PIC X(20). 
           03  WRK-MEISYO          PIC X(44).
      *
      *    °åÎÅµ¡´Ø¾ðÊó
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
           03  WRK-KAISETUNAME       PIC X(80).
      *
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX        PIC 9(03).
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-KOH             PIC 9(02).
      *
      *    ½¸·×ÎÎ°è
       01  SUM-AREA.
           03  SUM-TBL-SRYKA       PIC X(02).
           03  SUM-TABLE.
      *        ¸å´ü¹âÎð¼Ô
               05  SUM-TBL             OCCURS  2.
                   07  SUM-NYUGAI-TBL       OCCURS  2.
                       09  SUM-KENSU           PIC  9(06).
                       09  SUM-NISSU           PIC  9(06).
                       09  SUM-TENSU           PIC  9(10).
                       09  SUM-ITBFTN          PIC  9(10).
                   07  SUM-S-KENSU             PIC  9(06).
                   07  SUM-S-NISSU             PIC  9(06).
                   07  SUM-S-KINGK             PIC  9(10).
                   07  SUM-S-FTNGK             PIC  9(10).
      *
      *        ¸¶Çú
               05  SUM-GENBK-TBL.
                   07  SUM-GNYUGAI-TBL      OCCURS  2.
                       09  SUM-GKENSU          PIC  9(06).
                       09  SUM-GNISSU          PIC  9(06).
                       09  SUM-GTENSU          PIC  9(10).
                       09  SUM-GITBFTN         PIC  9(10).
                   07  SUM-S-GKENSU            PIC  9(06).
                   07  SUM-S-GNISSU            PIC  9(06).
                   07  SUM-S-GKINGK            PIC  9(10).
                   07  SUM-S-GFTNGK            PIC  9(10).
      *        Ê¡»ã
               05  SUM-FUKUSI-TBL     OCCURS  3.
                   07  SUM-FNYUGAI-TBL      OCCURS  2.
                       09  SUM-FKENSU          PIC  9(06).
                       09  SUM-FNISSU          PIC  9(06).
                       09  SUM-FTENSU          PIC  9(10).
                       09  SUM-FITBFTN         PIC  9(10).
                   07  SUM-S-FKENSU            PIC  9(06).
                   07  SUM-S-FNISSU            PIC  9(06).
                   07  SUM-S-FKINGK            PIC  9(10).
                   07  SUM-S-FFTNGK            PIC  9(10).
      *        ¸øÈñÉéÃ´ 
               05  SUM-KOH-TBL         OCCURS  100.
                   07  SUM-KOHNUM              PIC  X(03).
                   07  SUM-KOH-NYUGAI-TBL   OCCURS  2.
                       09  SUM-KKENSU          PIC  9(06).
                       09  SUM-KNISSU          PIC  9(06).
                       09  SUM-KTENSU          PIC  9(10).
                       09  SUM-KITBFTN         PIC  9(10).
                   07  SUM-S-KKENSU            PIC  9(06).
                   07  SUM-S-KNISSU            PIC  9(06).
                   07  SUM-S-KKINGK            PIC  9(10).
                   07  SUM-S-KFTNGK            PIC  9(10).
                   07  SUM-KOH-MEISYO          PIC  X(44).
      *
      *    ¥­¡¼ÎÎ°è
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM      PIC X(08).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM      PIC X(08). 
      *
      *    ¸ÇÄê¹àÌÜ
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ´µ¼Ô
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±ÈÖ¹æ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ÊÝ¸±¼Ô¾ðÊó
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó¾ðÊó
           COPY    "CPSK1001.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ¡¢Ï¢ÍíÀè 
           COPY    "CPSK1002.INC".
      *
      *    ¿ÇÎÅ²ÊÌÜ¾ðÊó
           COPY    "CPSK1005.INC".
      *
      *    °åÎÅµ¡´ØÊÔ½¸¾ðÊó
           COPY    "CPSK1900.INC".
      *
      *    °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸¾ðÊó
           COPY    "CPSK1901.INC".
      *
      *    °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊó
           COPY    "CPSK1902.INC".
      *    °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊó¡Ê£²¡Ë
           COPY    "CPSK1903.INC".
      *
      *    ¥ì¥»¥×¥È¡¦Áí³ç°õºþ¾ðÊó¡Ê¹­°èÏ¢¹ç¡Ë
           COPY    "CPSK200501.INC".
      *
      *    ¸øÈñÈÖ¹æ¤Þ¤È¤á¾ðÊó
           COPY    "CPSK2007.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      * 
      ******************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    °õºþ£Ä£Â¹¹¿·¥µ¥Ö
           COPY    "CPORCSPRT.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   È¾³Ñ¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY  "CPORCSKANACHK.INC".
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
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
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
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-SYOKBN1
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU3415"    TO  JOB-PGID
           MOVE    "¹­°èÏ¢¹ç¡¦¿ÇÎÅÊó½·ÀÁµá½ñ"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    ¥µ¥Ö¥×¥í¥°¥é¥à¤è¤ê°ì»þ¥Õ¥¡¥¤¥ëÌ¾ÊÔ½¸
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ¡¢Ï¢ÍíÀè
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
               END-IF    
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¡Ê½êºßÃÏ¡Ë¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    °åÎÅµ¡´ØÊÔ½¸¾ðÊó
           PERFORM 900-1900-READ-SEC
      *
      *    ¸øÈñÈÖ¹æ¤Þ¤È¤á¾ðÊó
           INITIALIZE                      WRK-2007-AREA
      *
           INITIALIZE                      SYS-2007-REC
           MOVE    "2007"              TO  SYS-2007-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-2007-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2007-STYUKYMD (7:2)
           MOVE    SYS-2007-STYUKYMD   TO  SYS-2007-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2007-HOSPNUM
           MOVE    SYS-2007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX            >=  100
               MOVE    MCPDATA-REC         TO  SYS-2007-REC
      *
               ADD     1                   TO  IDX
               MOVE    SYS-2007-MEISYO(1) TO  WRK-2007-MEISYO(IDX)
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      SYS-2007-KOHNUM (IDY)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM (IDY)
                                           TO WRK-2007-KOHNUM(IDX IDZ)
               END-PERFORM
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       20
                       OR      SYS-2007-KOHNUM1(IDY)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM1(IDY)
                                           TO  WRK-2007-KOHNUM(IDX IDZ)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX                     TO  WRK-2007-MAX
      *
      *    ¥ì¥»¥×¥È¡¦Áí³çÊÔ½¸¾ðÊó
      *    ¶èÊ¬¥³¡¼¥É£°£±¤Î¥Ç¡¼¥¿ÆÉ¤ß¹þ¤ß
           MOVE    SPACE               TO  SYS-200501-REC
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-200501-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-200501-STYUKYMD (7:2)
           MOVE    SYS-200501-STYUKYMD TO  SYS-200501-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200501-REC
           ELSE
               INITIALIZE                      SYS-200501-REC
           END-IF
           IF      SYS-200501-KOUIKIPRTKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-200501-KOUIKIPRTKBN
           END-IF
      *
           IF      SYS-200501-KOUIKIPRTKBN
                                       =   "1"
               CONTINUE
           ELSE
               MOVE    "00000000"          TO  WRK-PARA-HKNJANUM
           END-IF
      *
           DISPLAY "SYS-200501-KOUIKIPRTKBN=" SYS-200501-KOUIKIPRTKBN
           DISPLAY "WRK-PARA-HKNJANUM      =" WRK-PARA-HKNJANUM
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT  UPD-FILE
      *
      *    ½èÍý¶èÊ¬(0=Á´ÂÎ 1=Åö·î¡¦·îÃÙ¤ì 2=ÊÖÌáÊ¬)¤ÇÊ¬¤±¤ë
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "2"
                   MOVE    "2"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "1"
                   MOVE    "1"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
      *
           CLOSE   UPD-FILE
      *
      *    ÀÁµá£Ä£Â¹¹¿·
           IF      FLG-UPD        =   1
           AND     FLG-ERR        =   ZERO 
               IF      CNT-UPDFILE    >   ZERO
                   PERFORM 310-SEIKYU-UPD-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½èÍý¶èÊ¬½¸·×½èÍý
      *****************************************************************
       2001-SYOKBN-SYUKEI-SEC         SECTION.
      *
      *    Æþ±¡³°¡Ê¤½¤ÎÂ¾¤Î¥ì¥»¡Ë¡¦Æþ±¡½¸·×
      *    
           MOVE    LOW-VALUE           TO  KEY-AREA
           MOVE    1                   TO  WRK-INDEX
      *
      *    ½èÍý¶èÊ¬¤Ë¤è¤Ã¤Æ¥­¡¼¤òÊ¬¤±¤ë(SYOKBN1=2¢ªÆÃÊÌÎÅÍÜÈñ)
           EVALUATE    WRK-SYOKBN      ALSO    WRK-PARA-SYOKBN1
               WHEN    "1"             ALSO    "1"
                   MOVE    "key66"         TO  WRK-MCP-PATHNAME
               WHEN    "2"             ALSO    "1"
                   MOVE    "key65"         TO  WRK-MCP-PATHNAME
               WHEN    "1"             ALSO    "2"
                   MOVE    "key66"         TO  WRK-MCP-PATHNAME
               WHEN    "2"             ALSO    "2"
                   MOVE    "key65"         TO  WRK-MCP-PATHNAME
           END-EVALUATE  
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM 200-HENSYU-SEC  UNTIL   FLG-RECE10   =   1 
                                   OR      FLG-ERR      =   1
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2001-SYOKBN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¸·×½èÍý
      *****************************************************************
       200-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  SUM-AREA
      *
      *    ¥­¡¼Ëè¤Ë½¸·×
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM             UNTIL   FLG-END     =   1
                               OR      KEY-NEW NOT =   KEY-OLD
      *        ÊÝ¸±¼ÔÊÌ½¸·×½èÍý
               PERFORM 2001-KOUKI-SYUKEI-SEC
           END-PERFORM
      *
      *    °õºþ½èÍý
           IF      FLG-UPD        =   1
           AND     FLG-ERR        =   ZERO
               PERFORM 2002-PRINT-SEC
           END-IF
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¼ÔÊÌ½¸·×½èÍý
      *****************************************************************
       2001-KOUKI-SYUKEI-SEC                SECTION.
      *
           INITIALIZE                       IDX-AREA
                                            FLG-UPD
                                            WRK-HKNJANUM
                                            WRK-HKNJANAME
      *    ÊÝ¸±¼ÔÈÖ¹æ
           MOVE  RECE10-HKNJANUM    TO  WRK-HKNJANUM
      *    ¸©Ì¾¥»¥Ã¥È
           PERFORM  200101-HKNJANUM-SET-SEC
      *
      *    Æþ³°¶èÊ¬
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   MOVE    1                   TO  IDX3
               WHEN    "2"
                   MOVE    2                   TO  IDX3
           END-EVALUATE
      *
      *    µëÉÕ³ä¹ç
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    1                   TO  IDX1
               WHEN    7
                   MOVE    2                   TO  IDX1
           END-EVALUATE
      *
           PERFORM 20011-HKN-SYUKEI-SEC
      *
      *    ¸øÈñÉéÃ´Ê¬
           IF      RECE10-KOHNUM (1)   NOT =   SPACE  OR
                   RECE10-KOHNUM (2)   NOT =   SPACE  OR
                   RECE10-KOHNUM (3)   NOT =   SPACE  OR
                   RECE10-KOHNUM (4)   NOT =   SPACE
               PERFORM 20012-KOH-SYUKEI-SEC
           END-IF
      *
      *    ¸¶Çú¡¦Ê¡»ã
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  4
             EVALUATE  RECE10-KOHNUM(IDXX)
               WHEN "019"
                 PERFORM 20013-GENBK-SYUKEI-SEC
               WHEN "191"
               WHEN "291"
               WHEN "391"
                 MOVE  1                   TO  IDX4
                 PERFORM 20014-FUKUSI-SYUKEI-SEC
               WHEN "192"
               WHEN "292"
               WHEN "392"
                 MOVE  2                   TO  IDX4
                 PERFORM 20014-FUKUSI-SYUKEI-SEC
               WHEN "193"
                 MOVE  3                   TO  IDX4
                 PERFORM 20014-FUKUSI-SYUKEI-SEC
             END-EVALUATE
           END-PERFORM
      *
      *    ½¸·×¤ÎÂÐ¾Ý¤Ë¤Ê¤é¤Ê¤«¤Ã¤¿¤È¤­
           IF      FLG-UPD             =   1
               PERFORM 20015-UPD-FILE-HENSYU-SEC
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "½¸·×ÂÐ¾Ý³°¡¡´µ¼ÔÈÖ¹æ¡á"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " ¿ÇÎÅÇ¯·î¡á"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " ¥ì¥»¼ïÊÌ¡á"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF 
           .
       2001-KOUKI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸©Ì¾¥»¥Ã¥È
      *****************************************************************
       200101-HKNJANUM-SET-SEC                SECTION.
      *
           EVALUATE  WRK-HKNJANUM(3:2)
      *      ËÌ³¤Æ»
             WHEN  "01"
                 MOVE  "39010004"  TO  WRK-HKNJANUM
                 MOVE  "ËÌ³¤Æ»"    TO  WRK-HKNJANAME
      *      ÀÄ¿¹¸©
             WHEN  "02"
                 MOVE  "39020003"  TO  WRK-HKNJANUM
                 MOVE  "ÀÄ¿¹¸©"    TO  WRK-HKNJANAME
      *      ´ä¼ê¸©
             WHEN  "03"
                 MOVE  "39030002"  TO  WRK-HKNJANUM
                 MOVE  "´ä¼ê¸©"    TO  WRK-HKNJANAME
      *      µÜ¾ë¸©
             WHEN  "04"
                 MOVE  "39040001"  TO  WRK-HKNJANUM
                 MOVE  "µÜ¾ë¸©"    TO  WRK-HKNJANAME
      *      ½©ÅÄ¸©
             WHEN  "05"
                 MOVE  "39050000"  TO  WRK-HKNJANUM
                 MOVE  "½©ÅÄ¸©"    TO  WRK-HKNJANAME
      *      »³·Á¸©
             WHEN  "06"
                 MOVE  "39060009"  TO  WRK-HKNJANUM
                 MOVE  "»³·Á¸©"    TO  WRK-HKNJANAME
      *      Ê¡Åç¸©
             WHEN  "07"
                 MOVE  "39070008"  TO  WRK-HKNJANUM
                 MOVE  "Ê¡Åç¸©"    TO  WRK-HKNJANAME
      *      °ñ¾ë¸©
             WHEN  "08"
                 MOVE  "39080007"  TO  WRK-HKNJANUM
                 MOVE  "°ñ¾ë¸©"    TO  WRK-HKNJANAME
      *      ÆÊÌÚ¸©
             WHEN  "09"
                 MOVE  "39090006"  TO  WRK-HKNJANUM
                 MOVE  "ÆÊÌÚ¸©"    TO  WRK-HKNJANAME
      *      ·²ÇÏ¸©
             WHEN  "10"
                 MOVE  "39100003"  TO  WRK-HKNJANUM
                 MOVE  "·²ÇÏ¸©"    TO  WRK-HKNJANAME
      *      ºë¶Ì¸©
             WHEN  "11"
                 MOVE  "39110002"  TO  WRK-HKNJANUM
                 MOVE  "ºë¶Ì¸©"    TO  WRK-HKNJANAME
      *      ÀéÍÕ¸©
             WHEN  "12"
                 MOVE  "39120001"  TO  WRK-HKNJANUM
                 MOVE  "ÀéÍÕ¸©"    TO  WRK-HKNJANAME
      *      ÅìµþÅÔ
             WHEN  "13"
                 MOVE  "39130000"  TO  WRK-HKNJANUM
                 MOVE  "ÅìµþÅÔ"    TO  WRK-HKNJANAME
      *      ¿ÀÆàÀî¸©
             WHEN  "14"
                 MOVE  "39140009"  TO  WRK-HKNJANUM
                 MOVE  "¿ÀÆàÀî¸©"  TO  WRK-HKNJANAME
      *      ¿·³ã¸©
             WHEN  "15"
                 MOVE  "39150008"  TO  WRK-HKNJANUM
                 MOVE  "¿·³ã¸©"    TO  WRK-HKNJANAME
      *      ÉÙ»³¸©
             WHEN  "16"
                 MOVE  "39160007"  TO  WRK-HKNJANUM
                 MOVE  "ÉÙ»³¸©"    TO  WRK-HKNJANAME
      *      ÀÐÀî¸©
             WHEN  "17"
                 MOVE  "39170006"  TO  WRK-HKNJANUM
                 MOVE  "ÀÐÀî¸©"    TO  WRK-HKNJANAME
      *      Ê¡°æ¸©
             WHEN  "18"
                 MOVE  "39180005"  TO  WRK-HKNJANUM
                 MOVE  "Ê¡°æ¸©"    TO  WRK-HKNJANAME
      *      »³Íü¸©
             WHEN  "19"
                 MOVE  "39190004"  TO  WRK-HKNJANUM
                 MOVE  "»³Íü¸©"    TO  WRK-HKNJANAME
      *      Ä¹Ìî¸©
             WHEN  "20"
                 MOVE  "39200001"  TO  WRK-HKNJANUM
                 MOVE  "Ä¹Ìî¸©"    TO  WRK-HKNJANAME
      *      ´ôÉì¸©
             WHEN  "21"
                 MOVE  "39210000"  TO  WRK-HKNJANUM
                 MOVE  "´ôÉì¸©"    TO  WRK-HKNJANAME
      *      ÀÅ²¬¸©
             WHEN  "22"
                 MOVE  "39220009"  TO  WRK-HKNJANUM
                 MOVE  "ÀÅ²¬¸©"    TO  WRK-HKNJANAME
      *      °¦ÃÎ¸©
             WHEN  "23"
                 MOVE  "39230008"  TO  WRK-HKNJANUM
                 MOVE  "°¦ÃÎ¸©"    TO  WRK-HKNJANAME
      *      »°½Å¸©
             WHEN  "24"
                 MOVE  "39240007"  TO  WRK-HKNJANUM
                 MOVE  "»°½Å¸©"    TO  WRK-HKNJANAME
      *      ¼¢²ì¸©
             WHEN  "25"
                 MOVE  "39250006"  TO  WRK-HKNJANUM
                 MOVE  "¼¢²ì¸©"    TO  WRK-HKNJANAME
      *      µþÅÔÉÜ
             WHEN  "26"
                 MOVE  "39260005"  TO  WRK-HKNJANUM
                 MOVE  "µþÅÔÉÜ"    TO  WRK-HKNJANAME
      *      ÂçºåÉÜ
             WHEN  "27"
                 MOVE  "39270004"  TO  WRK-HKNJANUM
                 MOVE  "ÂçºåÉÜ"    TO  WRK-HKNJANAME
      *      Ê¼¸Ë¸©
             WHEN  "28"
                 MOVE  "39280003"  TO  WRK-HKNJANUM
                 MOVE  "Ê¼¸Ë¸©"    TO  WRK-HKNJANAME
      *      ÆàÎÉ¸©
             WHEN  "29"
                 MOVE  "39290002"  TO  WRK-HKNJANUM
                 MOVE  "ÆàÎÉ¸©"    TO  WRK-HKNJANAME
      *      ÏÂ²Î»³¸©
             WHEN  "30"
                 MOVE  "39300009"  TO  WRK-HKNJANUM
                 MOVE  "ÏÂ²Î»³¸©"  TO  WRK-HKNJANAME
      *      Ä»¼è¸©
             WHEN  "31"
                 MOVE  "39310008"  TO  WRK-HKNJANUM
                 MOVE  "Ä»¼è¸©"    TO  WRK-HKNJANAME
      *      Åçº¬¸©
             WHEN  "32"
                 MOVE  "39320007"  TO  WRK-HKNJANUM
                 MOVE  "Åçº¬¸©"    TO  WRK-HKNJANAME
      *      ²¬»³¸©
             WHEN  "33"
                 MOVE  "39330006"  TO  WRK-HKNJANUM
                 MOVE  "²¬»³¸©"    TO  WRK-HKNJANAME
      *      ¹­Åç¸©
             WHEN  "34"
                 MOVE  "39340005"  TO  WRK-HKNJANUM
                 MOVE  "¹­Åç¸©"    TO  WRK-HKNJANAME
      *      »³¸ý¸©
             WHEN  "35"
                 MOVE  "39350004"  TO  WRK-HKNJANUM
                 MOVE  "»³¸ý¸©"    TO  WRK-HKNJANAME
      *      ÆÁÅç¸©
             WHEN  "36"
                 MOVE  "39360003"  TO  WRK-HKNJANUM
                 MOVE  "ÆÁÅç¸©"    TO  WRK-HKNJANAME
      *      ¹áÀî¸©
             WHEN  "37"
                 MOVE  "39370002"  TO  WRK-HKNJANUM
                 MOVE  "¹áÀî¸©"    TO  WRK-HKNJANAME
      *      °¦É²¸©
             WHEN  "38"
                 MOVE  "39380001"  TO  WRK-HKNJANUM
                 MOVE  "°¦É²¸©"    TO  WRK-HKNJANAME
      *      ¹âÃÎ¸©
             WHEN  "39"
                 MOVE  "39390000"  TO  WRK-HKNJANUM
                 MOVE  "¹âÃÎ¸©"    TO  WRK-HKNJANAME
      *      Ê¡²¬¸©
             WHEN  "40"
                 MOVE  "39400007"  TO  WRK-HKNJANUM
                 MOVE  "Ê¡²¬¸©"    TO  WRK-HKNJANAME
      *      º´²ì¸©
             WHEN  "41"
                 MOVE  "39410006"  TO  WRK-HKNJANUM
                 MOVE  "º´²ì¸©"    TO  WRK-HKNJANAME
      *      Ä¹ºê¸©
             WHEN  "42"
                 MOVE  "39420005"  TO  WRK-HKNJANUM
                 MOVE  "Ä¹ºê¸©"    TO  WRK-HKNJANAME
      *      ·§ËÜ¸©
             WHEN  "43"
                 MOVE  "39430004"  TO  WRK-HKNJANUM
                 MOVE  "·§ËÜ¸©"    TO  WRK-HKNJANAME
      *      ÂçÊ¬¸©
             WHEN  "44"
                 MOVE  "39440003"  TO  WRK-HKNJANUM
                 MOVE  "ÂçÊ¬¸©"    TO  WRK-HKNJANAME
      *      µÜºê¸©
             WHEN  "45"
                 MOVE  "39450002"  TO  WRK-HKNJANUM
                 MOVE  "µÜºê¸©"    TO  WRK-HKNJANAME
      *      ¼¯»ùÅç¸©
             WHEN  "46"
                 MOVE  "39460001"  TO  WRK-HKNJANUM
                 MOVE  "¼¯»ùÅç¸©"    TO  WRK-HKNJANAME
      *      ²­Æì¸©
             WHEN  "47"
                 MOVE  "39470000"  TO  WRK-HKNJANUM
                 MOVE  "²­Æì¸©"    TO  WRK-HKNJANAME
           END-EVALUATE
      *
           .
       200101-HKNJANUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¸·×½èÍý
      *****************************************************************
       20011-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX1             =   ZERO
           OR      IDX3            =   ZERO 
               PERFORM  20011-HKN-SYUKEI-EXT
           END-IF
      *
      *    ÎÅÍÜ¤ÎµëÉÕ¡§·ï¿ô
           ADD  1                  TO  SUM-KENSU(IDX1 IDX3)
      *    ÎÅÍÜ¤ÎµëÉÕ¡§¿ÇÎÅ¼ÂÆü¿ô
           ADD  RECE10-JNISSU(1)   TO  SUM-NISSU(IDX1 IDX3)
      *    ÎÅÍÜ¤ÎµëÉÕ¡§ÅÀ¿ô
           ADD  RECE10-TOTALTEN(1) TO  SUM-TENSU(IDX1 IDX3)
      *    ÎÅÍÜ¤ÎµëÉÕ¡§°ìÉôÉéÃ´¶â
           ADD  RECE10-FTNMONEY(1) TO  SUM-ITBFTN(IDX1 IDX3)
      *
           IF      IDX3                   =   1 
             IF      RECE10-SHOKUJINISSU(1)   >   ZERO
      *    ¿©»öÎÅÍÜ¡§·ï¿ô
               ADD     1                   TO  SUM-S-KENSU(IDX1)
             END-IF
      *    ¿©»öÎÅÍÜ¡§Æü¿ô
             ADD  RECE10-SHOKUJINISSU(1)   TO  SUM-S-NISSU(IDX1)
      *    ¿©»öÎÅÍÜ¡§¶â³Û
             ADD  RECE10-SHOKUJIRYOYOHI(1) TO  SUM-S-KINGK(IDX1)
      *    ¿©»öÎÅÍÜ¡§É¸½àÉéÃ´³Û
             ADD  RECE10-SHOKUJIFTN(1)     TO  SUM-S-FTNGK(IDX1)
           END-IF    
      *
           MOVE    1               TO  FLG-UPD 
           .
       20011-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÉéÃ´Ê¬½¸·×½èÍý
      *****************************************************************
       20012-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
                   OR  RECE10-KOHNUM (IDZ) =   SPACE
      *
             MOVE      SPACE             TO  WRK-MEISYO
      *      £°£µ£±¤È£°£¹£±¤ÏÆ±¤¸¾ì½ê¤Ë½¸·×¤¹¤ë
             IF      RECE10-KOHNUM (IDZ) =   "091"
               MOVE    "051"               TO  RECE10-KOHNUM (IDZ)
             END-IF
      *
      *      ¸øÈñÈÖ¹æ¤Þ¤È¤á¾ðÊó¤¬¤¢¤ë¤È¤­
             IF        WRK-2007-MAX    >       ZERO
               PERFORM VARYING IDZ1    FROM    1   BY  1
                       UNTIL   IDZ1    >       WRK-2007-MAX
                 PERFORM VARYING IDZ2  FROM    1   BY  1
                         UNTIL   IDZ2  >       20
                         OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                   IF  RECE10-KOHNUM(IDZ)  
                                       =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                     MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                         TO  RECE10-KOHNUM (IDZ)
                     MOVE  WRK-2007-MEISYO (IDZ1)
                                         TO  WRK-MEISYO
                     MOVE  20            TO  IDZ2
                     MOVE  WRK-2007-MAX  TO  IDZ1
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-IF
      *
      *      ¸øÈñÈÖ¹æ³ÊÇ¼
             PERFORM VARYING   IDX2    FROM    1   BY  1
                     UNTIL     IDX2 >          100
      *        ¸¶Çú¡¦¸©ÃÏÊý¸øÈñ¤ÏÉ½¼¨°ÌÃÖ¸ÇÄê¤Î°Ù¡¢ÂÐ¾Ý³°
               IF  RECE10-KOHNUM(IDZ)  =  "019" OR "191" OR "291"
                                       OR "391" OR "192" OR "292"
                                       OR "392"
                                       OR "193"
                 CONTINUE
               ELSE
                 IF  SUM-KOHNUM (IDX2)   =   SPACE
                   MOVE  RECE10-KOHNUM(IDZ) TO  SUM-KOHNUM(IDX2)
                   MOVE  WRK-MEISYO        TO  SUM-KOH-MEISYO(IDX2)
                 END-IF
                 IF   RECE10-KOHNUM(IDZ) =  SUM-KOHNUM (IDX2)
                   PERFORM 200121-KOH-SYUKEI-01-SEC
                   MOVE    100             TO  IDX2
                 END-IF
               END-IF
             END-PERFORM
           END-PERFORM
           .
       20012-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ½¸·×½èÍý
      *****************************************************************
       200121-KOH-SYUKEI-01-SEC        SECTION.
      *
           COMPUTE IDY     =   IDZ     +   1
      *    
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§·ï¿ô
           ADD  1                    TO  SUM-KKENSU(IDX2 IDX3)
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§Æü¿ô
           ADD  RECE10-JNISSU(IDY)   TO  SUM-KNISSU(IDX2 IDX3)
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§ÅÀ¿ô
           ADD  RECE10-TOTALTEN(IDY) TO  SUM-KTENSU(IDX2 IDX3)
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§°ìÉôÉéÃ´¶â
           ADD  RECE10-FTNMONEY(IDY) TO  SUM-KITBFTN(IDX2 IDX3)
      *
           IF      IDX3              =   1 
             IF    RECE10-SHOKUJINISSU(IDY)   >   ZERO
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§·ï¿ô
               ADD     1                     TO SUM-S-KKENSU(IDX2)
             END-IF
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§Æü¿ô
             ADD  RECE10-SHOKUJINISSU(IDY)   TO SUM-S-KNISSU(IDX2)
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§¶â³Û
             ADD  RECE10-SHOKUJIRYOYOHI(IDY) TO SUM-S-KKINGK(IDX2)
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§É¸½àÉéÃ´³Û
             ADD  RECE10-SHOKUJIFTN(IDY)     TO SUM-S-KFTNGK(IDX2)
           END-IF    
      *
           MOVE    1               TO  FLG-UPD 
           .
       200121-KOH-SYUKEI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸¶Çú½¸·×½èÍý
      *****************************************************************
       20013-GENBK-SYUKEI-SEC        SECTION.
      *
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ¡§·ï¿ô
           ADD   1                              TO  SUM-GKENSU(IDX3)
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ:¿ÇÎÅ¼ÂÆü¿ô
           ADD  RECE10-JNISSU(IDXX + 1)         TO  SUM-GNISSU(IDX3)
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ¡§ÅÀ¿ô
           ADD  RECE10-TOTALTEN(IDXX + 1)       TO  SUM-GTENSU(IDX3)
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ¡§°ìÉôÉéÃ´¶â
           ADD  RECE10-FTNMONEY(IDXX + 1)       TO  SUM-GITBFTN(IDX3)
      *
           IF   RECE10-SHOKUJINISSU(IDXX + 1)  >  0
      *    ¸¶Çú¡§¿©»öÎÅÍÜ:·ï¿ô
             ADD  1                             TO  SUM-S-GKENSU
           END-IF
      *    ¸¶Çú¡§¿©»öÎÅÍÜ:Æü¿ô
           ADD  RECE10-SHOKUJINISSU(IDXX + 1)   TO  SUM-S-GNISSU
      *    ¸¶Çú¡§¿©»öÎÅÍÜ¡§¶â³Û
           ADD  RECE10-SHOKUJIRYOYOHI(IDXX + 1) TO  SUM-S-GKINGK
      *    ¸¶Çú¡§¿©»öÎÅÍÜ¡§É¸½àÉéÃ´³Û
           ADD  RECE10-SHOKUJIFTN(IDXX + 1)     TO  SUM-S-GFTNGK
      *
           MOVE    1               TO  FLG-UPD 
         .
       20013-GENBK-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê¡»ã½¸·×½èÍý
      *****************************************************************
       20014-FUKUSI-SYUKEI-SEC        SECTION.
      *
      *    Ê¡»ã¡§ÎÅÍÜ¤ÎµëÉÕ¡§·ï¿ô
           ADD   1                           TO  SUM-FKENSU(IDX4 IDX3)
      *    Ê¡»ã¡§ÎÅÍÜ¤ÎµëÉÕ:¿ÇÎÅ¼ÂÆü¿ô
           ADD  RECE10-JNISSU(IDXX + 1)      TO  SUM-FNISSU(IDX4 IDX3)
      *    Ê¡»ã¡§ÎÅÍÜ¤ÎµëÉÕ¡§ÅÀ¿ô
           ADD  RECE10-TOTALTEN(IDXX + 1)    TO  SUM-FTENSU(IDX4 IDX3)
      *    Ê¡»ã¡§ÎÅÍÜ¤ÎµëÉÕ¡§°ìÉôÉéÃ´¶â
           ADD  RECE10-FTNMONEY(IDXX + 1)    TO  SUM-FITBFTN(IDX4 IDX3)
      *
           IF   RECE10-SHOKUJINISSU(IDXX + 1)  >  0
      *    Ê¡»ã¡§¿©»öÎÅÍÜ:·ï¿ô
             ADD  1                          TO  SUM-S-FKENSU(IDX4)
           END-IF
      *    Ê¡»ã¡§¿©»öÎÅÍÜ:Æü¿ô
           ADD  RECE10-SHOKUJINISSU(IDXX + 1) TO SUM-S-FNISSU(IDX4)
      *    Ê¡»ã¡§¿©»öÎÅÍÜ¡§¶â³Û
           ADD  RECE10-SHOKUJIRYOYOHI(IDXX + 1) TO SUM-S-FKINGK(IDX4)
      *    Ê¡»ã¡§¿©»öÎÅÍÜ¡§É¸½àÉéÃ´³Û
           ADD  RECE10-SHOKUJIFTN(IDXX + 1)  TO  SUM-S-FFTNGK(IDX4)
      *
           MOVE    1                         TO  FLG-UPD 
         .
       20014-FUKUSI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       20015-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           INITIALIZE                      RECE10X-UPHKNJANUM
                                           RECE10X-XXPREFNUM
                                           RECE10X-XXSRYYM
                                           RECE10X-XXSTHKNJANUM
                                           RECE10X-XXEDHKNJANUM
                                           RECE10X-XXTEISYUTUSAKI
           WRITE   RECE10X-REC
      *
           IF      STS-RECEUPD         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë ½ñ¤­¹þ¤ß¥¨¥é¡¼ STS="
                                                   DELIMITED  BY  SIZE
                       STS-RECEUPD                 DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF 
           ADD     1                   TO  CNT-UPDFILE
           .
       20015-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *    Ä¢É¼ÊÔ½¸<¸«½Ð¤·>½èÍý
           PERFORM 20021-HEAD-HEN-SEC
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ>½èÍý
           PERFORM 20023-BODY-HEN-SEC
      *    Ä¢É¼°õºþ½èÍý
           PERFORM 20024-PRINT-OUT-SEC
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¸«½Ð¤·>½èÍý
      *****************************************************************
       20021-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      SKT3415
      *    ÀÁµáÇ¯·îÆü
           MOVE    WRK-SEIYMDWH        TO  SKT3415-SEIYMD
      *    ¿ÇÎÅÇ¯·î
           MOVE    WRK-SRYYMWH         TO  SKT3415-SRYYM
      *    °åÎÅµ¡´Ø½»½ê
           MOVE    WRK-HOSPADRS        TO  SKT3415-ADRS
           INSPECT SKT3415-ADRS        REPLACING ALL "  " BY "¡¡"
      *    °åÎÅµ¡´ØÌ¾
           MOVE    WRK-HOSPNAME        TO  SKT3415-HOSPNAME
           INSPECT SKT3415-HOSPNAME    REPLACING ALL "  " BY "¡¡"
      *    ³«Àß¼ÔÌ¾
           MOVE    WRK-KAISETUNAME     TO  SKT3415-KAISETUNAME
      *    °åÎÅµ¡´Ø¥³¡¼¥É
           MOVE    SYS-1001-HOSPCDN    TO  SKT3415-HOSPCD
      *    ÊÝ¸±¼ÔÈÖ¹æ
           MOVE     WRK-HKNJANUM       TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT      TO  SKT3415-HKNJANUM
      *    ÊÝ¸±¼ÔÌ¾(ÅÔÆ»ÉÜ¸©Ì¾)
           MOVE    WRK-HKNJANAME       TO  SKT3415-HKNJANAME
      *    ÆÃÊÌÎÅÍÜÈñ
           IF  WRK-PARA-SYOKBN1   =  "2"
             MOVE  "ÆÃÊÌÎÅÍÜÈñ"        TO  SKT3415-TOKUBETU
           END-IF
      *
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>½èÍý
      *****************************************************************
       20023-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO                  TO  IDZ
      *
      *    ÊÔ½¸½èÍý
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
             ADD     1                   TO  IDZ    
             PERFORM VARYING IDY FROM    1   BY  1
                     UNTIL   IDY >       2
      *    ÎÅÍÜ¤ÎµëÉÕ¡§·ï¿ô
               MOVE  SUM-KENSU(IDX IDY)  TO  WRK-Z5
               MOVE  WRK-Z5              TO  SKT3415-KENSU(IDZ IDY)
      *    ÎÅÍÜ¤ÎµëÉÕ¡§¿ÇÎÅ¼ÂÆü¿ô
               MOVE  SUM-NISSU(IDX IDY)  TO  WRK-Z5 
               MOVE  WRK-Z5              TO  SKT3415-NISSU(IDZ IDY)
      *    ÎÅÍÜ¤ÎµëÉÕ¡§ÅÀ¿ô
               MOVE  SUM-TENSU(IDX IDY)  TO  WRK-Z9
               MOVE  WRK-Z9              TO  SKT3415-TENSU(IDZ IDY)
      *    ÎÅÍÜ¤ÎµëÉÕ¡§°ìÉôÉéÃ´¶â
               MOVE  SUM-ITBFTN(IDX IDY) TO  WRK-Z9
               MOVE  WRK-Z9              TO  SKT3415-ITBFTN(IDZ IDY)
             END-PERFORM
      *
      *    ¿©»öÎÅÍÜ¡§·ï¿ô
             MOVE  SUM-S-KENSU(IDX)      TO  WRK-Z5
             MOVE  WRK-Z5                TO  SKT3415-S-KENSU(IDZ)
      *    ¿©»öÎÅÍÜ¡§Æü¿ô
             MOVE  SUM-S-NISSU(IDX)      TO  WRK-Z5
             MOVE  WRK-Z5                TO  SKT3415-S-NISSU(IDZ)
      *    ¿©»öÎÅÍÜ¡§¶â³Û
             MOVE  SUM-S-KINGK(IDX)      TO  WRK-Z9
             MOVE  WRK-Z9                TO  SKT3415-S-KINGK(IDZ)
      *    ¿©»öÎÅÍÜ¡§É¸½àÉéÃ´³Û
             MOVE  SUM-S-FTNGK(IDX)      TO  WRK-Z9
             MOVE  WRK-Z9                TO  SKT3415-S-FTNGK(IDZ)
           END-PERFORM
      *
      *    ¸¶ÇúÊ¬
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ¡§·ï¿ô
             MOVE  SUM-GKENSU(IDX)       TO  WRK-Z5
             MOVE  WRK-Z5                TO  SKT3415-GKENSU(IDX)
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ¡§Æü¿ô
             MOVE  SUM-GNISSU(IDX)       TO  WRK-Z5
             MOVE  WRK-Z5                TO  SKT3415-GNISSU(IDX)
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ¡§ÅÀ¿ô
             MOVE  SUM-GTENSU(IDX)       TO  WRK-Z9
             MOVE  WRK-Z9                TO  SKT3415-GTENSU(IDX)
      *    ¸¶Çú¡§ÎÅÍÜ¤ÎµëÉÕ¡§°ìÉôÉéÃ´¶â
             MOVE  SUM-GITBFTN(IDX)      TO  WRK-Z9
             MOVE  WRK-Z9                TO  SKT3415-GITBFTN(IDX)
           END-PERFORM
      *    ¸¶Çú¡§¿©»öÎÅÍÜ¡§·ï¿ô
           MOVE  SUM-S-GKENSU            TO  WRK-Z5
           MOVE  WRK-Z5                  TO  SKT3415-S-GKENSU
      *    ¸¶Çú¡§¿©»öÎÅÍÜ¡§Æü¿ô
           MOVE  SUM-S-GNISSU            TO  WRK-Z5
           MOVE  WRK-Z5                  TO  SKT3415-S-GNISSU
      *    ¸¶Çú¡§¿©»öÎÅÍÜ¡§¶â³Û
           MOVE  SUM-S-GKINGK            TO  WRK-Z9
           MOVE  WRK-Z9                  TO  SKT3415-S-GKINGK
      *    ¸¶Çú¡§¿©»öÎÅÍÜ¡§É¸½àÉéÃ´³Û
           MOVE  SUM-S-GFTNGK            TO  WRK-Z9
           MOVE  WRK-Z9                  TO  SKT3415-S-GFTNGK
      *
      *    Ê¡»ã
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
             PERFORM VARYING IDY FROM    1   BY  1
                     UNTIL   IDY >       2
      *    Ê¡»ã¡§·ï¿ô
               MOVE  SUM-FKENSU(IDX IDY) TO  WRK-Z5
               MOVE  WRK-Z5              TO  SKT3415-FKENSU(IDX IDY)
      *    Ê¡»ã¡§°ìÉôÉéÃ´¶â
               MOVE  SUM-FITBFTN(IDX IDY) TO  WRK-Z9
               MOVE  WRK-Z9               TO  SKT3415-FITBFTN(IDX IDY)
             END-PERFORM
           END-PERFORM
      *
      *    ¸øÈñÉéÃ´Ê¬ÊÔ½¸½èÍý
           MOVE    ZERO                  TO  FLG-KOH-END
           MOVE    1                     TO  IDX2
      * 
           PERFORM 200231-KOH-HENSYU-SEC
                                   UNTIL   FLG-KOH-END   =   1
      *
           .
       20023-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¸øÈñÉéÃ´Ê¬>½èÍý
      *****************************************************************
       200231-KOH-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       3
                   OR      FLG-KOH-END     =   1
             IF      SUM-KOHNUM (IDX2)   =   SPACE
               MOVE    1                     TO  FLG-KOH-END
             ELSE    
               PERFORM 2002311-HKNNUM-HENSYU-SEC     
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       2
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§·ï¿ô
                 MOVE  SUM-KKENSU(IDX2 IDZ)  TO WRK-Z5
                 MOVE  WRK-Z5                TO SKT3415-KKENSU(IDY IDZ)
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§Æü¿ô
                 MOVE  SUM-KNISSU(IDX2 IDZ)  TO WRK-Z5
                 MOVE  WRK-Z5                TO SKT3415-KNISSU(IDY IDZ)
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§ÅÀ¿ô
                 MOVE  SUM-KTENSU(IDX2 IDZ)  TO WRK-Z9
                 MOVE  WRK-Z9               TO SKT3415-KTENSU(IDY IDZ)
      *    ¸øÈñºÆ·ÇÍó¡§ÎÅÍÜ¤ÎµëÉÕ¡§°ìÉôÉéÃ´¶â
                 MOVE  SUM-KITBFTN(IDX2 IDZ) TO WRK-Z9
                 MOVE  WRK-Z9               TO SKT3415-KITBFTN(IDY IDZ)
               END-PERFORM
      *
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§·ï¿ô
               MOVE  SUM-S-KKENSU(IDX2)    TO  WRK-Z5
               MOVE  WRK-Z5                TO  SKT3415-S-KKENSU(IDY)
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§Æü¿ô
               MOVE  SUM-S-KNISSU(IDX2)    TO  WRK-Z5
               MOVE  WRK-Z5                TO  SKT3415-S-KNISSU(IDY)
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§¶â³Û
               MOVE  SUM-S-KKINGK(IDX2)    TO  WRK-Z9
               MOVE  WRK-Z9                TO  SKT3415-S-KKINGK(IDY)
      *    ¸øÈñºÆ·ÇÍó¡§¿©»öÎÅÍÜ¡§É¸½àÉéÃ´³Û
               MOVE  SUM-S-KFTNGK(IDX2)    TO  WRK-Z9
               MOVE  WRK-Z9                TO  SKT3415-S-KFTNGK(IDY)
      *
               ADD     1                   TO  IDX2   
             END-IF        
           END-PERFORM
      *
      *    £±ÊÇ½ÐÎÏ¤·¤Æ¤â¸øÈñ¤Ï¤Þ¤À»Ä¤Ã¤Æ¤¤¤ë¤«
           IF      SUM-KOHNUM (IDX2)  =   SPACE
               MOVE    1                   TO  FLG-KOH-END
           ELSE    
      *        Ä¢É¼°õºþ½èÍý¡Ê£²ÊÇ°Ê¹ß¡Ë
               PERFORM 20024-PRINT-OUT-SEC
      *        Ä¢É¼ÊÔ½¸<¸«½Ð¤·>½èÍý
               PERFORM 20021-HEAD-HEN-SEC
           END-IF     
      *
           .
       200231-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÊÝ¸±¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       2002311-HKNNUM-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHKBN
      *
           MOVE    SUM-KOHNUM (IDX2)   TO  WRK-KOHNUM9
           MOVE    WRK-KOHNUM9         TO  SKT3415-HBTNUM (IDY)
      *
      *    ÆÃÄê¼À´µ¤Î¤È¤­ 
           IF      SUM-KOHNUM (IDX2)  =   "051"
               MOVE    "51"            TO   SKT3415-HBTNUM (IDY)
               MOVE    "ÆÃÄê¼À´µ"      TO   WRK-HKNNAME
           ELSE
      *      ¸øÈñÈÖ¹æ¤Þ¤È¤á¾ðÊó¤«¤é¤Î½¸·×¤«
             IF      SUM-KOH-MEISYO(IDX2)  =   SPACE
      *        ÊÝ¸±ÈÖ¹æÆÉ¹þ¤ß  
               INITIALIZE                     HKNNUM-REC
               MOVE  SPA-HOSPNUM          TO  HKN-HOSPNUM
               MOVE  SUM-KOHNUM (IDX2)    TO  HKN-HKNNUM
               MOVE  HKNNUM-REC           TO  MCPDATA-REC
               MOVE  "tbl_hknnum"         TO  MCP-TABLE
               MOVE  "key2"               TO  MCP-PATHNAME
                 PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC              =   ZERO
                   MOVE  "tbl_hknnum"     TO  MCP-TABLE
                   MOVE  "key2"           TO  MCP-PATHNAME
                   PERFORM 900-HKNNUM-READ-SEC
                 ELSE
                   MOVE    1              TO  FLG-HKNNUM
                 END-IF
      *
                 PERFORM UNTIL       FLG-HKNNUM      =   1
                   IF      HKN-TEKSTYMD            <=  WRK-SRYYMD
                   AND     HKN-TEKEDYMD            >=  WRK-SRYYMD
                     MOVE  HKN-TANSEIDONAME   TO  WRK-HKNNAME
                     MOVE  1                  TO  FLG-HKNNUM   
                   END-IF
      *
                   IF      FLG-HKNNUM           =   ZERO
                     MOVE  "tbl_hknnum"       TO  MCP-TABLE
                     MOVE  "key2"             TO  MCP-PATHNAME
                     PERFORM 900-HKNNUM-READ-SEC
                   END-IF
                 END-PERFORM
                 MOVE  "tbl_hknnum"       TO  MCP-TABLE
                 MOVE  "key2"             TO  MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
      *
      *          Ë¡ÊÌÈÖ¹æ
                 MOVE  HKN-HBTNUM         TO  SKT3415-HBTNUM(IDY)
             ELSE
      *          ¸øÈñÈÖ¹æ¤Þ¤È¤á¾ðÊó¤«¤é¸øÈñÌ¾¤Ø
                 MOVE SUM-KOH-MEISYO(IDX2)    TO  WRK-HKNNAME
             END-IF
           END-IF
      *    ¸øÈñÌ¾
           MOVE  WRK-HKNNAME        TO  SKT3415-KOHNAME(IDY)
      *
           . 
       2002311-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼°õºþ½èÍý
      *****************************************************************
       20024-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-PAGE
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    WRK-PAGE            TO  SKT3415-NO
           MOVE    "SKT3415.red"         TO  SPRT-PRTID
           MOVE    SKT3415               TO  SPRT-PRTDATA
           EVALUATE    WRK-INDEX    ALSO    WRK-SYOKBN
                                            ALSO    WRK-PARA-SYOKBN1
               WHEN    1            ALSO    "1"     
                                            ALSO   "1"
                   MOVE    "¹­°èÏ¢¹çÀÁµá½ñ"        TO  SPRT-TITLE
               WHEN    2            ALSO    "1"
                                            ALSO   "1"
                   MOVE    "¹­°èÏ¢¹çÀÁµá½ñ¡Êºß°åÁíÅù¡Ë"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "¹­°èÏ¢¹çÀÁµá½ñ¡ÎÊÖÌáÊ¬¡Ï" 
                                               TO  SPRT-TITLE
               WHEN    2            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "¹­°èÏ¢¹çÀÁµá½ñ¡Êºß°åÁíÅù¡Ë¡ÎÊÖÌáÊ¬¡Ï"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "1"     
                                            ALSO   "2"
                   MOVE    "¹­°èÏ¢¹çÀÁµá½ñ¡ÎÆÃÊÌÎÅÍÜÈñ¡Ï"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "2"
                   MOVE    "¹­°èÏ¢¹çÀÁµá½ñ¡ÎÊÖÌáÊ¬¡Ï¡ÎÆÃÊÌÎÅÍÜÈñ¡Ï" 
                                               TO  SPRT-TITLE
           END-EVALUATE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
      *     
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       20024-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´³ÑÊÑ´¹½èÍý
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
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
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END     
                                           FLG-ERR
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
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "SOKATU3415 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING   
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** SOKATU3415 IN   "  CNT-RECE10 
           DISPLAY "***          UPD  "  CNT-UPDFILE 
           DISPLAY "***          PAGE "  CNT-PAGE 
           DISPLAY "*** SOKATU3415 END ***"
      *     
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *                              
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   ÀÁµá´ÉÍý£Ä£Â¹¹¿·½èÍý
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
                   MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD TO  RECE10-UPDYMD  
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       STRING "ÀÁµá´ÉÍý£Ä£Â¡¡¹¹¿·¥¨¥é¡¼¡¡"
                                               DELIMITED  BY  SIZE
                               RECE10-KEY      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
               ELSE
                   STRING "ÀÁµá´ÉÍý£Ä£Â¡¡ÆÉ¹þ¤ß¹¹¿·¥¨¥é¡¼¡¡"
                                               DELIMITED  BY  SIZE
                           RECE10X-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING                                
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    À¾ÎñÆüËÜ¸ìÊÑ´¹½èÍý
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "¡¡"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá´ÉÍýÆÉ¹þ
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE10
      *     
      *    ÀÁµá´ÉÍý£Ä£Â¸¡º÷
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           MOVE    WRK-CONS-SRYYM-200804
                                       TO  RECE10-SRYYM
           MOVE    999999              TO  RECE10-XXSRYYM
           IF      WRK-PARA-HKNJANUM   NOT =   ZERO
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXSTHKNJANUM
                                               RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           IF      WRK-PARA-SYOKBN1    =   "1"
               MOVE    "039"               TO  RECE10-HKNNUM-S
                                               RECE10-HKNNUM
           ELSE 
               MOVE    "040"               TO  RECE10-HKNNUM-S
                                               RECE10-HKNNUM
           END-IF
      *
           DISPLAY "SYOKBN=" WRK-SYOKBN " SYOKBN1=" WRK-PARA-SYOKBN1
                   " SRYYM=" RECE10-SRYYM "-" RECE10-XXSRYYM 
                   " HKNNUM= " RECE10-HKNNUM
                   " PATHNAME=" WRK-MCP-PATHNAME
                   " HKNJANUM=" RECE10-XXSTHKNJANUM "-" 
                                RECE10-XXEDHKNJANUM
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECE10
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        ÊÖÌáÊ¬¤Ç¥ª¥ó¥é¥¤¥óÀÁµá¤ÏÆÉ¤ßÈô¤Ð¤¹
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
      *        ¥Æ¥¹¥È´µ¼Ô¤Î¤È¤­¤ÏÆÉ¤ßÈô¤Ð¤¹
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *        ¥­¡¼¤Ë¸©ÈÖ¹æ¤ò³ÊÇ¼
               MOVE    RECE10-HKNJANUM(1:4)  TO  KEY-N-HKNJANUM
               ADD     1                     TO  CNT-RECE10   
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM 
      *         
           ELSE
               MOVE    HIGH-VALUE      TO  KEY-NEW
               MOVE    1               TO  FLG-RECE10
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá´ÉÍý£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ëÆÉ¹þ
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅµ¡´ØÊÔ½¸¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    °åÎÅµ¡´ØÊÔ½¸¾ðÊóÆÉ¤ß¹þ¤ß
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(17)  NOT =   SPACE
      *            °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸¾ðÊó
                   PERFORM 900-1901-READ-SEC
      *            °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊó
                   PERFORM 900-1902-READ-SEC
      *            °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊó¡Ê£²¡Ë
                   PERFORM 900-1903-READ-SEC
                END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸¾ðÊóÆÉ¤ß¹þ¤ß
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(17)
                                   TO  SYS-1901-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊóÆÉ¤ß¹þ¤ß
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(17)
                                   TO  SYS-1902-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(17) TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                  SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF          MCP-RC      =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-KEY10-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ¡Ê£Î£Å£Ø£Ô¡Ë
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¥­¡¼¡Ë
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-INV-EXT.
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
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
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
           .
       900-DBDISCONNECT-EXT.
           EXIT.
