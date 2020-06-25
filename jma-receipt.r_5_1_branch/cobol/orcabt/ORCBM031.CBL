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
       PROGRAM-ID.             ORCBM031.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ·î¼¡Ä¢É¼
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¹ñÊÝ¡¦¥ì¥»ÅÅÁí³çÉ½¡Ê°å²Ê¡¦»õ²Ê¡Ë
      *                      £È£Ã£Í£²£Ñ¡¢£È£Ã£Í£²£Ò
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/09/01    NACL-¿¹ÏÆ     ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.01.01    NACL-¿¹ÏÆ    04/10/07  ÌôºÞÉéÃ´¶âÍó¤òºï½ü
      *                           04/10/12  ¸©ÊÌ¤Ë½¸·×
      *                           04/10/13  ¸©Æâ¡¢¸©³°ÊÌ¤Ë½ÐÎÏ
      *  01.01.02    NACL-¾®Æ¦Âô  04/10/20  ÀÁµá£Ä£Â¹¹¿·½èÍýÄÉ²Ã
      *  01.01.03    NACL-Æ£¸¶    05/02/16  ÊÖÌáÊ¬¤ò½èÍýÂÐ¾Ý¤È¤·¤Ê¤¤
      *
      *  03.02.01    NACL-Æ£¸¶    06/10/04  ¥·¥¹¥Æ¥à´ÉÍý¤Î¾ò·ï¤ò½¤Àµ
      *
      *  03.03.01    NACL-Æ£¸¶    06/10/10  £²£°£°£¶Ç¯£±£°·î²þÀµÂÐ±þ
      *                                     HCM2Q.red ¢ª HCM2QV01.red
      *                                     HCM2R.red ¢ª HCM2RV01.red
      * 
      *                                     MONFUNCÂÐ±þ
      *  03.03.02    NACL-Æ£¸¶    06/11/08  À¸³èÎÅÍÜ¤Î¤ß¤Î¾ì¹ç¤Î½¸·×
      *  03.04.01    NACL-¿¹ÏÆ    07/01/16  °åÎÅµ¡Ì¾¾Î¡¢½»½êÊÔ½¸
      *
      *  03.05.01    NACL-Æ£¸¶    07/04/19  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.01    NACL-¾®Æ¦Âô  07/09/12  ¿·³ã¸©Ãæ±Û²­ÃÏ¿ÌÂÐ±þ¤ÇºÒ£±¡¢
      *                                     ÉÔ¾Ü¡¢ÊÝ¸±¼ÔÌ¤ÆÃÄê¤ÏÆÉ¤ßÈô¤Ð¤¹
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/18  Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤Î¿ÇÎÅÊ¬¤Ï
      *                                     ÆÉ¤ßÈô¤Ð¤¹
      *
      *  04.07.01    NACL-Æ£¸¶    12/01/24  monpe-0.6.20ÂÐ±þ
      *  04.07.02    NACL-Æ£¸¶    15/08/18  °åÎÅµ¡´ØÊÔ½¸¾ðÊó¤Ø¤Î³«Àß¼ÔÌ¾
      *                                     ÄÉ²ÃÂÐ±þ
      *
      *  04.08.01    NACL-Æ£¸¶    14/07/07  °ì»þ¥Ç¥£¥ì¥¯¥È¥êÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
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
       DATA                    DIVISION.
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
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è
           COPY    "CPRECEERR.INC".
      *
      *****COPY    "HCM2Q.INC".
      *****COPY    "HCM2QV01.INC"  REPLACING  //HCM2QV01//
           COPY    "HCM2QV02.INC"  REPLACING  //HCM2QV02//
                                   BY         //HCM2Q//.
      *****COPY    "HCM2R.INC".
           COPY    "HCM2RV01.INC"  REPLACING  //HCM2RV01//
                                   BY         //HCM2R//.
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-RECE10          PIC X(02). 
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-UPD             PIC 9(01).
      *
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRTKBN          PIC 9(01).
      *----(01.01.01)--ADD-START---
           03  FLG-KBN             PIC 9(01).
      *----(01.01.01)--ADD-START---
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01). 
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    ¥Ñ¥é¥á¥¿ÎÎ°è
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
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
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-PRTFLG          PIC 9(01).
      *
      *    ¿ô»ú¹àÌÜÊÔ½¸
           03  WRK-KENSUZ          PIC ZZZZ.
           03  WRK-TENSUZ          PIC ZZZZZZZZ.
           03  WRK-ITBFTNZ         PIC ZZZZZZZZ.
           03  WRK-PAGE            PIC ZZZ9.
      *----(03.04.01)--ADD-START---
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
      *----(03.04.01)--ADD-END-----
           03  WRK-KAISETUNAME     PIC X(80).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(04).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-LINE            PIC 9(02).
           03  CNT-UPDFILE         PIC 9(06).
      *
      *    ½¸·×ÎÎ°è
       01  SUM-AREA.
      *    ¹ñÌ±·ò¹¯ÊÝ¸±(1¡Á9)Ï·¿ÍÊÝ·ò(10¡Á11)¸øÈñÉéÃ´(12)
           03  SUM-TBL             OCCURS  12.
               05  SUM-KENSU           PIC 9(06).
               05  SUM-TOTALTEN        PIC 9(10).
               05  SUM-FTNMONEY        PIC 9(10).
               05  SUM-SHOKUJIKENSU    PIC 9(06).
               05  SUM-SHOKUJIRYOYOHI  PIC 9(10).
               05  SUM-SHOKUJIFTN      PIC 9(10).
      *    ¿©»ö½¸·×
           03  SUM-SHO-TBL         OCCURS  2.
               05  SUM-SHO-KENSU       PIC 9(06).
               05  SUM-SHO-RYOYOHI     PIC 9(10).
               05  SUM-SHO-FTN         PIC 9(10).
      *
      *    Áí·×ÎÎ°è
       01  GSUM-AREA.
      *    ¹ñÌ±·ò¹¯ÊÝ¸±(1¡Á9)Ï·¿ÍÊÝ·ò(10¡Á11)¸øÈñÉéÃ´(12)
           03  GSUM-G-TBL          OCCURS  12.
               05  GSUM-KENSU          PIC 9(06).
               05  GSUM-TOTALTEN       PIC 9(10).
               05  GSUM-FTNMONEY       PIC 9(10).
      *    ¿©»öÁí·×
           03  GSUM-SHO-TBL        OCCURS  2.
               05  GSUM-SHO-KENSU      PIC 9(06).
               05  GSUM-SHO-RYOYOHI    PIC 9(10).
               05  GSUM-SHO-FTN        PIC 9(10).
      *
      *    ¥­¡¼ÎÎ°è
       01  KEY-AREA            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM.
                   07  KEY-N-HKNJANUM1 PIC X(02).
                   07  KEY-N-HKNJANUM2 PIC X(06).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM.
                   07  KEY-O-HKNJANUM1 PIC X(02).
                   07  KEY-O-HKNJANUM2 PIC X(06).
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CONS-AREA.
      *    £²£°£°£¶Ç¯£´·î²þÀµ
           03  WRK-CONS-SRYYM-200604   PIC X(06)   VALUE   "200604".
      *    £²£°£°£¶Ç¯£±£°·î²þÀµ
           03  WRK-CONS-SRYYM-200610   PIC X(06)   VALUE   "200610".
      *    £²£°£°£¸Ç¯£´·î²þÀµ
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    °åÎÅµ¡´Ø¾ðÊó¾ðÊó
           COPY    "CPSK1001.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ¡¢Ï¢ÍíÀè
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
           COPY    "CPSK1903.INC".
      *
      *    ÀÁµá´ÉÍý
           COPY    "CPRCF010.INC".
      *
      *    ´µ¼Ô
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    °õºþ´ÉÍý
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    °õºþ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      ******************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    °õºþ£Ä£Â¹¹¿·¥µ¥Ö
           COPY    "CPORCSPRT.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
      *----(01.01.01)--UPD-START---
               PERFORM 200-MAIN-SEC
      *                 UNTIL   FLG-END     =   1
      *----(01.01.01)--UPD-START---
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
           INITIALIZE                  GSUM-AREA
                                       SPA-AREA
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
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBM031"      TO  JOB-PGID
           MOVE    "¹ñÊÝ¥ì¥»ÅÅ¡¦¿ÇÎÅÊó½·ÀÁµá½ñ¡Ê°å²Ê¡¦»õ²Ê¡Ë"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "RECEUPD"       TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEUPD-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  RECEUPD-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECEUPD-BASENAME
                                   TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEUPD
      *
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  WRK-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SRYYMD (7:2)
      *
           MOVE    WRK-SRYYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)
                                   TO  WRK-SRYYMWH
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF          STS-DAY-RC1     =   ZERO
               MOVE    LNK-DAY2-EDTYMD3
                                       TO  WRK-SEIYMDWH
           END-IF
      *
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD
                                   TO  SYS-1001-EDYUKYMD
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               IF          SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1001-HOSPNAME
               END-IF
               IF          SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1001-KAISETUNAME
               END-IF
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý½êºßÃÏ
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD
                                   TO  SYS-1002-EDYUKYMD
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
               IF          SYS-1002-ADRS   =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1002-ADRS
               END-IF
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¡Ê½êºßÃÏ¡Ë¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *----(03.04.01)--ADD-START---
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
      *    °åÎÅµ¡´ØÊÔ½¸¾ðÊó
           PERFORM 900-1900-READ-SEC
      *----(03.04.01)--ADD-END-----
      *
           OPEN    OUTPUT  UPD-FILE
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    1:-200609 2:200610-   
           PERFORM VARYING WRK-PRTFLG  FROM    1   BY  1
                   UNTIL   WRK-PRTFLG  >       2
      *
               IF    ( WRK-PRTFLG          =   2                     )
               AND   ( LNK-PRTKANRI-SRYYM  <   WRK-CONS-SRYYM-200610 )
                   CONTINUE
               ELSE
                   PERFORM VARYING FLG-KBN FROM    1   BY  1
                           UNTIL ( FLG-END     =   1 ) OR
                                 ( FLG-KBN     >   2 )
      *
                       MOVE    LOW-VALUE       TO  KEY-AREA
      *
                       PERFORM 900-SEIKYU-SELECT-SEC
      *
                       PERFORM 200-HENSYU-SEC
                               UNTIL ( FLG-RECE10   =   1 )
                               OR    ( FLG-ERR      =   1 )
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                  END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¸·×½èÍý
      *****************************************************************
       200-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  SUM-AREA
      *
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM UNTIL ( FLG-END     =   1 )
                   OR    ( KEY-NEW NOT =   KEY-OLD )
               PERFORM 2001-SYUKEI-SEC
           END-PERFORM
      *
           IF        ( FLG-UPD     =   1 )
               AND   ( FLG-ERR     =   ZERO )
               PERFORM 2002-PRINT-HENSYU-SEC
           END-IF
      *
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¼ÔÊÌ½¸·×½èÍý
      *****************************************************************
       2001-SYUKEI-SEC             SECTION.
      *
           MOVE    ZERO            TO  IDX-AREA
                                       FLG-UPD
      *
      *    Ï·¿ÍÊÝ·ò
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    10              TO  IDX
               WHEN    8
               WHEN    7
                   MOVE    11              TO  IDX
               END-EVALUATE
           ELSE
      *        ¹ñÌ±·ò¹¯ÊÝ¸±
               EVALUATE    RECE10-HKNNUM-S
               WHEN    "060"
                   IF          RECE10-AGE      >=  70
                       EVALUATE    RECE10-KYURATE
                       WHEN    9
                           MOVE    1           TO  IDX
                       WHEN    8
                       WHEN    7
                           MOVE    2           TO  IDX
                       END-EVALUATE
                   ELSE
                       IF          RECE10-AGE      <   3
                           MOVE    4           TO  IDX
                       ELSE
                           MOVE    3           TO  IDX
                       END-IF
                   END-IF
               WHEN    "067"
                   IF          RECE10-AGE      >=  70
                       EVALUATE    RECE10-KYURATE
                       WHEN    9
                           MOVE    6           TO  IDX
                       WHEN    8
                       WHEN    7
                           MOVE    7           TO  IDX
                       END-EVALUATE
                   ELSE
                       IF          RECE10-AGE      <   3
                           MOVE    9               TO  IDX
                       ELSE
                           IF          RECE10-HONKZKKBN    =   "1"
                               MOVE    5               TO  IDX
                           ELSE
                               MOVE    8               TO  IDX
                           END-IF
                       END-IF
                   END-IF
               END-EVALUATE
           END-IF
           PERFORM 20011-HKN-SYUKEI-SEC
      *
      *    ¸øÈñÉéÃ´Ê¬
           IF          RECE10-KOHNUM(1)   NOT =   SPACE
               PERFORM 20012-KOH-SYUKEI-SEC
           END-IF
      *
      *    ½¸·×¤ÎÂÐ¾Ý¤Ë¤Ê¤Ã¤¿¤È¤­
           IF          FLG-UPD     =   1
               PERFORM 20013-UPD-FILE-HENSYU-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE          TO  WRK-RECEERR
               STRING "½¸·×ÂÐ¾Ý³°¡¡´µ¼ÔÈÖ¹æ¡á"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " ¿ÇÎÅÇ¯·î¡á"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " ¥ì¥»¼ïÊÌ¡á"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅÊÝ¸±¡¦Ï·¿ÍÊÝ·òÊ¬½¸·×½èÍý
      *****************************************************************
       20011-HKN-SYUKEI-SEC        SECTION.
      *
           IF        ( IDX         =   ZERO )
               GO  TO  20011-HKN-SYUKEI-EXT
           END-IF
      *
      *    ·ï¿ô
           ADD     1               TO  SUM-KENSU(IDX)
      *
      *    ÎÅÍÜ¤ÎµëÉÕ 
           ADD     RECE10-TOTALTEN(1)
                                   TO  SUM-TOTALTEN(IDX)
           ADD     RECE10-FTNMONEY(1)
                                   TO  SUM-FTNMONEY(IDX)
      *    ¿©»öÎÅÍÜ
           IF          RECE10-NYUGAIKBN    =   "1"
               IF          RECE10-SHOKUJINISSU(1)   >   ZERO
                   ADD     1       TO   SUM-SHOKUJIKENSU(IDX)
               END-IF
               ADD     RECE10-SHOKUJIRYOYOHI(1)
                                   TO   SUM-SHOKUJIRYOYOHI(IDX)
               ADD     RECE10-SHOKUJIFTN(1)
                                   TO   SUM-SHOKUJIFTN(IDX)
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
                   UNTIL ( IDZ     >       4 )
                   OR    ( RECE10-KOHNUM(IDZ)  =   SPACE )
               COMPUTE IDY     =   IDZ     +   1
      *
      *        ·ï¿ô
               ADD     1               TO  SUM-KENSU(12)
      *
      *        ÎÅÍÜ¤ÎµëÉÕ
               ADD     RECE10-TOTALTEN(IDY)
                                   TO  SUM-TOTALTEN(12)
               ADD     RECE10-FTNMONEY(IDY)
                                   TO  SUM-FTNMONEY(12)
      *
               EVALUATE    RECE10-NYUGAIKBN
      *        ¿©»öÎÅÍÜ
               WHEN    "1"
                   IF      RECE10-SHOKUJINISSU(IDY)   >   ZERO
                       ADD     1   TO  SUM-SHOKUJIKENSU(12)
                   END-IF
                   ADD     RECE10-SHOKUJIRYOYOHI(IDY)
                                   TO  SUM-SHOKUJIRYOYOHI(12)
                   ADD     RECE10-SHOKUJIFTN(IDY)
                                   TO  SUM-SHOKUJIFTN(12)
               END-EVALUATE
      *
               MOVE    1               TO  FLG-UPD
           END-PERFORM
           .
       20012-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá£Ä£Â¹¹¿·ÍÑ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       20013-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           ADD     1                   TO  CNT-UPDFILE
           .
       20013-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       2002-PRINT-HENSYU-SEC       SECTION.
      *
      *    Ä¢É¼ÊÔ½¸<¸«½Ð¤·>½èÍý
           IF      CNT-LINE    =   ZERO
               PERFORM 20021-HEAD-HEN-SEC
           END-IF
      *
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ>½èÍý
           PERFORM 20022-BODY-HEN-SEC
      *
      *    Ä¢É¼°õºþ½èÍý
           IF      CNT-LINE            =   23
               PERFORM 20024-PRINT-OUT-SEC
               MOVE    ZERO                TO  CNT-LINE
           END-IF
      *
           IF          FLG-RECE10     =   1
               IF          CNT-LINE   =   ZERO
                   PERFORM 20021-HEAD-HEN-SEC
               END-IF
               PERFORM 20023-GOKEI-HEN-SEC
               PERFORM 20024-PRINT-OUT-SEC
               MOVE    ZERO                TO  GSUM-AREA
               MOVE    ZERO                TO  CNT-LINE
      *----(01.01.01)--ADD-START---
           ELSE
               IF   (( FLG-KBN         =   2)  AND
                     ( KEY-O-HKNJANUM1 NOT =   KEY-N-HKNJANUM1))
                   IF          CNT-LINE   =   ZERO
                       PERFORM 20021-HEAD-HEN-SEC
                   END-IF
                   PERFORM 20023-GOKEI-HEN-SEC
                   PERFORM 20024-PRINT-OUT-SEC
                   MOVE    ZERO                TO  GSUM-AREA
                   MOVE    ZERO                TO  CNT-LINE
               END-IF
      *----(01.01.01)--ADD-END-----
           END-IF
      *
           .
       2002-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¥Ø¥Ã¥À¡¼Éô>½èÍý
      *****************************************************************
       20021-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                  HCM2Q
           INITIALIZE                  HCM2R
      *
           ADD     1               TO  CNT-PAGE
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCM2Q-PAGE
      *
           MOVE    WRK-SEIYMDWH    TO  HCM2Q-SEIYMD
      *
           MOVE    WRK-SRYYMWH     TO  HCM2Q-PRTYM
      *
      *    °åÎÅµ¡´Ø½»½ê
           MOVE    WRK-HOSPADRS        TO  HCM2Q-ADRS
           INSPECT HCM2Q-ADRS          REPLACING   ALL "  "    BY  "¡¡"
      *    °åÎÅµ¡´ØÌ¾
           MOVE    WRK-HOSPNAME        TO  HCM2Q-HOSPNAME
           INSPECT HCM2Q-HOSPNAME      REPLACING   ALL "  "    BY  "¡¡"
      *
           MOVE    WRK-KAISETUNAME TO  HCM2Q-KAISETUNAME
      *
           MOVE    SYS-1001-HOSPCDN
                                   TO  HCM2Q-HOSPCD
      *
           EVALUATE    WRK-PRTFLG
               WHEN    1
                   MOVE    "¡¡¿©¡¡»ö¡¡ÎÅ¡¡ÍÜ"
                                         TO  HCM2Q-TITLE1
                   MOVE    "£¸"          TO  HCM2Q-TITLE2
                                             HCM2Q-TITLE3
               WHEN    2
                   MOVE    "¿©»öÎÅÍÜ¡¦À¸³èÎÅÍÜ"
                                         TO  HCM2Q-TITLE1
                   MOVE    "£·"          TO  HCM2Q-TITLE2
                                             HCM2Q-TITLE3
           END-EVALUATE
      *
           ADD     1               TO  CNT-PAGE
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCM2R-PAGE
      *
           MOVE    SYS-1001-HOSPCDN
                                   TO  HCM2R-HOSPCD
      *
           EVALUATE    WRK-PRTFLG
               WHEN    1
                   MOVE    "¡¡¿©¡¡»ö¡¡ÎÅ¡¡ÍÜ"
                                         TO  HCM2R-TITLE1
                   MOVE    "£¸"          TO  HCM2R-TITLE2
               WHEN    2
                   MOVE    "¿©»öÎÅÍÜ¡¦À¸³èÎÅÍÜ"
                                         TO  HCM2R-TITLE1
                   MOVE    "£·"          TO  HCM2R-TITLE2
           END-EVALUATE
      *
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>½èÍý
      *****************************************************************
       20022-BODY-HEN-SEC          SECTION.
      *
           ADD     1               TO  CNT-LINE
      *
           MOVE    KEY-O-HKNJANUM  TO  HCM2Q-HKNJANUM(CNT-LINE)
           MOVE    KEY-O-HKNJANUM  TO  HCM2R-HKNJANUM(CNT-LINE)
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >   12
               EVALUATE    IDX
      *        ¹ñÌ±·ò¹¯ÊÝ¸±
               WHEN    1   THRU    4
                   MOVE    SUM-KENSU(IDX)  TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ  TO  HCM2Q-IPN-KENSU(CNT-LINE IDX)
                   MOVE    SUM-TOTALTEN(IDX)
                                       TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ  TO  HCM2Q-IPN-TENSU(CNT-LINE IDX)
                   EVALUATE    IDX
                   WHEN    1   THRU    2
                       MOVE    SUM-FTNMONEY(IDX)
                                       TO  WRK-ITBFTNZ
                       MOVE    WRK-ITBFTNZ
                               TO  HCM2Q-IPN-ITBFTN(CNT-LINE IDX)
                   END-EVALUATE
      *
                   ADD     SUM-SHOKUJIKENSU(IDX)
                                           TO  SUM-SHO-KENSU(1)
                   ADD     SUM-SHOKUJIRYOYOHI(IDX)
                                           TO  SUM-SHO-RYOYOHI(1)
                   ADD     SUM-SHOKUJIFTN(IDX)
                                           TO  SUM-SHO-FTN(1)
      *        ¹ñÌ±·ò¹¯ÊÝ¸±¡ÊÂà¿¦¼Ô¡Ë
               WHEN    5   THRU    9
                   COMPUTE IDZ             =   IDX     -   4
                   MOVE    SUM-KENSU(IDX)  TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ  TO  HCM2R-TAI-KENSU(CNT-LINE IDZ)
                   MOVE    SUM-TOTALTEN(IDX)
                                       TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ  TO  HCM2R-TAI-TENSU(CNT-LINE IDZ)
                   EVALUATE    IDX
                       WHEN    6
                           MOVE    SUM-FTNMONEY(IDX)
                                           TO  WRK-ITBFTNZ
                           MOVE    WRK-ITBFTNZ
                                   TO  HCM2R-TAI-ITBFTN(CNT-LINE 1)
                       WHEN    7
                           MOVE    SUM-FTNMONEY(IDX)
                                           TO  WRK-ITBFTNZ
                           MOVE    WRK-ITBFTNZ
                                   TO  HCM2R-TAI-ITBFTN(CNT-LINE 2)
                   END-EVALUATE
      *
                   ADD     SUM-SHOKUJIKENSU(IDX)
                                           TO  SUM-SHO-KENSU(2)
                   ADD     SUM-SHOKUJIRYOYOHI(IDX)
                                           TO  SUM-SHO-RYOYOHI(2)
                   ADD     SUM-SHOKUJIFTN(IDX)
                                           TO  SUM-SHO-FTN(2)
      *        Ï·¿ÍÊÝ·ò
               WHEN    10  THRU    11
                   COMPUTE IDZ             =   IDX     -   9
                   MOVE    SUM-KENSU(IDX)      TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ
                           TO  HCM2Q-ROU-KENSU(CNT-LINE IDZ)
                   MOVE    SUM-TOTALTEN(IDX)   TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ
                           TO  HCM2Q-ROU-TENSU(CNT-LINE IDZ)
                   MOVE    SUM-FTNMONEY(IDX)   TO  WRK-ITBFTNZ
                   MOVE    WRK-ITBFTNZ
                           TO  HCM2Q-ROU-ITBFTN(CNT-LINE IDZ)
      *
                   ADD     SUM-SHOKUJIKENSU(IDX)
                                           TO  SUM-SHO-KENSU(1)
                   ADD     SUM-SHOKUJIRYOYOHI(IDX)
                                           TO  SUM-SHO-RYOYOHI(1)
                   ADD     SUM-SHOKUJIFTN(IDX) TO  SUM-SHO-FTN(1)
      *        ¸øÈñ
               WHEN    12
                   MOVE    SUM-KENSU(IDX)  TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM2R-KOU-KENSU(CNT-LINE)
                   MOVE    SUM-TOTALTEN(IDX)
                                           TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ      TO  HCM2R-KOU-TENSU(CNT-LINE)
                   MOVE    SUM-FTNMONEY(IDX)
                                           TO  WRK-ITBFTNZ
                   MOVE    WRK-ITBFTNZ TO  HCM2R-KOU-ITBFTN(CNT-LINE)
      *
                   ADD     SUM-SHOKUJIKENSU(IDX)
                                   TO  SUM-SHO-KENSU(2)
                   ADD     SUM-SHOKUJIRYOYOHI(IDX)
                                   TO  SUM-SHO-RYOYOHI(2)
                   ADD     SUM-SHOKUJIFTN(IDX)
                                   TO  SUM-SHO-FTN(2)
               END-EVALUATE
      *
               ADD     SUM-KENSU(IDX)      TO  GSUM-KENSU(IDX)
               ADD     SUM-TOTALTEN(IDX)   TO  GSUM-TOTALTEN(IDX)
               ADD     SUM-FTNMONEY(IDX)   TO  GSUM-FTNMONEY(IDX)
      *
           END-PERFORM
      *
           MOVE    SUM-SHO-KENSU(1)    TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ          TO  HCM2Q-SYO-KENSU(CNT-LINE)
           MOVE    SUM-SHO-RYOYOHI(1)  TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  HCM2Q-SYO-TENSU(CNT-LINE)
           MOVE    SUM-SHO-FTN(1)      TO  WRK-ITBFTNZ
           MOVE    WRK-ITBFTNZ         TO  HCM2Q-SYO-ITBFTN(CNT-LINE)
      *
           MOVE    SUM-SHO-KENSU(2)    TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ          TO  HCM2R-SYO-KENSU(CNT-LINE)
           MOVE    SUM-SHO-RYOYOHI(2)  TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  HCM2R-SYO-TENSU(CNT-LINE)
           MOVE    SUM-SHO-FTN(2)      TO  WRK-ITBFTNZ
           MOVE    WRK-ITBFTNZ         TO  HCM2R-SYO-ITBFTN(CNT-LINE)
      *
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL   IDX      >  2
               ADD     SUM-SHO-KENSU(IDX)  TO  GSUM-SHO-KENSU(IDX)
               ADD     SUM-SHO-RYOYOHI(IDX)    TO  GSUM-SHO-RYOYOHI(IDX)
               ADD     SUM-SHO-FTN(IDX)    TO  GSUM-SHO-FTN(IDX)
           END-PERFORM
      *
           INITIALIZE                  SUM-AREA
      *
           .
       20022-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¹ç·×¹Ô>½èÍý
      *****************************************************************
       20023-GOKEI-HEN-SEC         SECTION.
      *
           MOVE    23              TO  CNT-LINE
      *
           MOVE    "¡¡¹ç¡¡·×"      TO  HCM2Q-HKNJANUM(CNT-LINE)
                                       HCM2R-HKNJANUM(CNT-LINE)
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >   12
               EVALUATE    IDX
      *        ¹ñÌ±·ò¹¯ÊÝ¸±
               WHEN    1   THRU    4
                   MOVE    GSUM-KENSU(IDX) TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ  TO  HCM2Q-IPN-KENSU(CNT-LINE IDX)
                   MOVE    GSUM-TOTALTEN(IDX)
                                       TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ  TO  HCM2Q-IPN-TENSU(CNT-LINE IDX)
                   EVALUATE    IDX
                   WHEN    1   THRU    2
                       MOVE    GSUM-FTNMONEY(IDX)
                                       TO  WRK-ITBFTNZ
                       MOVE    WRK-ITBFTNZ
                               TO  HCM2Q-IPN-ITBFTN(CNT-LINE IDX)
                   END-EVALUATE
      *        ¹ñÌ±·ò¹¯ÊÝ¸±¡ÊÂà¿¦¼Ô¡Ë
               WHEN    5   THRU    9
                   COMPUTE IDZ             =   IDX     -   4
                   MOVE    GSUM-KENSU(IDX) TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ  TO  HCM2R-TAI-KENSU(CNT-LINE IDZ)
                   MOVE    GSUM-TOTALTEN(IDX)
                                       TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ  TO  HCM2R-TAI-TENSU(CNT-LINE IDZ)
                   EVALUATE    IDX
                   WHEN    6
                       MOVE    GSUM-FTNMONEY(IDX)
                                       TO  WRK-ITBFTNZ
                       MOVE    WRK-ITBFTNZ
                               TO  HCM2R-TAI-ITBFTN(CNT-LINE 1)
                   WHEN    7
                       MOVE    GSUM-FTNMONEY(IDX)
                                       TO  WRK-ITBFTNZ
                       MOVE    WRK-ITBFTNZ
                               TO  HCM2R-TAI-ITBFTN(CNT-LINE 2)
                   END-EVALUATE
      *
      *        Ï·¿ÍÊÝ·ò
               WHEN    10  THRU    11
                   COMPUTE IDZ             =   IDX     -   9
                   MOVE    GSUM-KENSU(IDX)     TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ
                           TO  HCM2Q-ROU-KENSU(CNT-LINE IDZ)
                   MOVE    GSUM-TOTALTEN(IDX)  TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ
                           TO  HCM2Q-ROU-TENSU(CNT-LINE IDZ)
                   MOVE    GSUM-FTNMONEY(IDX)  TO  WRK-ITBFTNZ
                   MOVE    WRK-ITBFTNZ
                           TO  HCM2Q-ROU-ITBFTN(CNT-LINE IDZ)
      *        ¸øÈñ
               WHEN    12
                   MOVE    GSUM-KENSU(IDX) TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM2R-KOU-KENSU(CNT-LINE)
                   MOVE    GSUM-TOTALTEN(IDX)
                                           TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ      TO  HCM2R-KOU-TENSU(CNT-LINE)
                   MOVE    GSUM-FTNMONEY(IDX)
                                           TO  WRK-ITBFTNZ
                   MOVE    WRK-ITBFTNZ     TO
                                           HCM2R-KOU-ITBFTN(CNT-LINE)
               END-EVALUATE
      *
           END-PERFORM
      *
           MOVE    GSUM-SHO-KENSU(1)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ          TO  HCM2Q-SYO-KENSU(CNT-LINE)
           MOVE    GSUM-SHO-RYOYOHI(1) TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  HCM2Q-SYO-TENSU(CNT-LINE)
           MOVE    GSUM-SHO-FTN(1)     TO  WRK-ITBFTNZ
           MOVE    WRK-ITBFTNZ         TO  HCM2Q-SYO-ITBFTN(CNT-LINE)
      *
           MOVE    GSUM-SHO-KENSU(2)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ          TO  HCM2R-SYO-KENSU(CNT-LINE)
           MOVE    GSUM-SHO-RYOYOHI(2) TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  HCM2R-SYO-TENSU(CNT-LINE)
           MOVE    GSUM-SHO-FTN(2)     TO  WRK-ITBFTNZ
           MOVE    WRK-ITBFTNZ         TO  HCM2R-SYO-ITBFTN(CNT-LINE)
      *
           .
       20023-GOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼°õºþ½èÍý
      *****************************************************************
       20024-PRINT-OUT-SEC         SECTION.
      *
           IF          CNT-LINE    =   ZERO
               GO  TO  20024-PRINT-OUT-EXT
           END-IF
      *
           PERFORM VARYING FLG-PRTKBN  FROM  1  BY  1
                   UNTIL   FLG-PRTKBN  >   2
      *
               INITIALIZE                  ORCSPRTAREA
               MOVE    "INS"           TO  SPRT-MODE
               MOVE    LNK-PRTKANRI-RENNUM
                                       TO  SPRT-RENNUM
               MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
               MOVE    LNK-PRTKANRI-SRYYM
                                       TO  SPRT-SRYYM
               MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SPRT-SKYYMD
               MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
               MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
               EVALUATE    FLG-PRTKBN
               WHEN    1
      *************MOVE    "HCM2Q.red"     TO  SPRT-PRTID
      *************MOVE    "HCM2QV01.red"  TO  SPRT-PRTID
                   MOVE    "HCM2QV02.red"  TO  SPRT-PRTID
                   MOVE    HCM2Q           TO  SPRT-PRTDATA
               WHEN    2
      *************MOVE    "HCM2R.red"     TO  SPRT-PRTID
                   MOVE    "HCM2RV01.red"  TO  SPRT-PRTID
                   MOVE    HCM2R           TO  SPRT-PRTDATA
               END-EVALUATE
      *
               MOVE    "¹ñÊÝ¥ì¥»ÅÅ¡¦¿ÇÎÅÊó½·ÀÁµá½ñ"
                                           TO  SPRT-TITLE
               MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
               MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
               MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
               MOVE    "1"                 TO  SPRT-SITEKBN
      *
               CALL    "ORCSPRT"           USING
                                           ORCSPRTAREA
grpsys                                     SPA-AREA
               IF          SPRT-RETURN     =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-PERFORM
           .
       20024-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF          STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
                                       FLG-ERR
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
           CLOSE   UPD-FILE
      *
      *
           IF      CNT-RECE10            =   ZERO 
               CONTINUE
           ELSE
              IF      FLG-UPD        =   1
              AND     FLG-ERR        =   ZERO 
                   IF      CNT-UPDFILE    >   ZERO
                       PERFORM 310-SEIKYU-UPD-SEC
                   END-IF
               END-IF     
           END-IF
      *
           DISPLAY "*** ORCBM031 IN  "  CNT-RECE10
           DISPLAY "*** ORCBM031 END ***"
      *     
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
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
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       STRING "ÀÁµá´ÉÍý£Ä£Â¡¡¹¹¿·¥¨¥é¡¼¡¡"
                                               DELIMITED  BY  SIZE
                               RECE10-KEY      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                   STRING "ÀÁµá´ÉÍý£Ä£Â¡¡ÆÉ¹þ¤ß¹¹¿·¥¨¥é¡¼¡¡"
                                               DELIMITED  BY  SIZE
                           RECE10X-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING                                
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
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
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "¡¡"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá´ÉÍýÆÉ¹þ
      *****************************************************************
       900-SEIKYU-SELECT-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-RECE10
      *
      *    ÀÁµá´ÉÍý£Ä£Â¸¡º÷
           MOVE    "tbl_seikyu"    TO  WRK-MCP-TABLE
           MOVE    "key58"         TO  WRK-MCP-PATHNAME
      *
           MOVE    SPACE           TO  RECE10-REC
           INITIALIZE                  RECE10-REC
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  RECE10-SKYYM
           EVALUATE    FLG-KBN
               WHEN    1
                   MOVE    "1"             TO  RECE10-PREFKBN     
               WHEN    2
                   MOVE    "2"             TO  RECE10-PREFKBN     
           END-EVALUATE
           EVALUATE    WRK-PRTFLG
               WHEN    "1"
                   MOVE    000000          TO  RECE10-SRYYM
                   MOVE    WRK-CONS-SRYYM-200610
                                           TO  RECE10-XXSRYYM
               WHEN    "2"
                   MOVE    WRK-CONS-SRYYM-200610
                                           TO  RECE10-SRYYM
                   MOVE    999999          TO  RECE10-XXSRYYM
           END-EVALUATE
      *###
           DISPLAY " SRYYM=" RECE10-SRYYM "-" RECE10-XXSRYYM
                   " PREFKBN=" RECE10-PREFKBN
      *###
      *     
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF          MCP-RC      =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECE10
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÀÁµá¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF          MCP-RC      =   ZERO
               MOVE    ZERO            TO  FLG-RECE10
               MOVE    MCPDATA-REC     TO  RECE10-REC
      * 
      *        ÊÖÌáÊ¬¤Î¤È¤­ÆÉ¤ßÈô¤Ð¤¹
               IF      RECE10-HENREIYM NOT =   ZERO
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      * 
      *        ¿·³ã¸©Ãæ±Û²­ÃÏ¿Ì¤ÇºÒ£±¡¢ÉÔ¾Ü¡¢ÊÝ¸±¼ÔÌ¤ÆÃÄê¤ÏÆÉ¤ßÈô¤Ð¤¹
               IF    ((RECE10-SRYYM  >=    "200709")   AND
                      (RECE10-SRYYM  <=    "200712"))  AND
                      (RECE10-ETC-YKZFTN(1)   =  1  OR  2  OR  3)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      * 
      *        ¥Æ¥¹¥È´µ¼Ô¤Î¤È¤­¤ÏÆÉ¤ßÈô¤Ð¤¹
               MOVE    SPACE           TO  PTINF-REC
               INITIALIZE                  PTINF-REC
grpsys         MOVE    RECE10-HOSPNUM  TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID     TO  PTINF-PTID
               MOVE    PTINF-REC       TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF          FLG-PTINF       =   ZERO
                   AND PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      * 
      *        Ê¿À®£²£°Ç¯£´·î°Ê¹ß¿ÇÎÅÊ¬¤ÏÆÉ¤ßÈô¤Ð¤¹
               IF      RECE10-SRYYM  >=    WRK-CONS-SRYYM-200804
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
               MOVE    RECE10-HKNJANUM-S
                                       TO  KEY-N-HKNJANUM
               ADD     1               TO  CNT-RECE10
      * 
               DISPLAY "PTNUM=" RECE10-PTNUM 
                       " HKNJANUM-S=" RECE10-HKNJANUM-S
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
       800-PTINF-READ-SEC          SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
      *----(03.04.01)--ADD-START---
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
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1900-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(7)  NOT =   SPACE
      *            °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸¾ðÊó
                   PERFORM 900-1901-READ-SEC
      *            °åÎÅµ¡´Ø½»½êÊÔ½¸¾ðÊó
                   PERFORM 900-1902-READ-SEC
      *            °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸¾ðÊó¡Ê£²¡Ë
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
           MOVE    SYS-1900-PRTKBN(7)
                                   TO  SYS-1901-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
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
           MOVE    SYS-1900-PRTKBN(7)
                                   TO  SYS-1902-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸¾ðÊó¡Ê£²¡ËÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1903-REC
           INITIALIZE                  SYS-1903-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1903-HOSPNUM
           MOVE    "1903"          TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(7)
                                   TO  SYS-1903-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1903-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD
                                   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                           TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                      SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
       910-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    ÀÁµá´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC               SECTION.
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
