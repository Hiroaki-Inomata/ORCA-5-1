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
       PROGRAM-ID.             ORCR0435.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ·î¼¡¶ÈÌ³¡¡Ï«ºÒ¼«ÇåÀÕÌÀºÙ½ñ¥Õ¥¡¥¤¥ëºîÀ®
      *                      ¥³¥á¥ó¥ÈÊÔ½¸£²¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/07/01    MCC-ÌçÏÆ¡¡    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    02.09.18  ¼«ÇåÀÕÂÐ±þ
      *  01.00.02    NACL-ÌçÏÆ    04.06.07  ¥³¥Ô¡¼¶ç£°£´£µÊÑ¹¹¤ËÈ¼¤¦½¤Àµ
      *  Ãæ»ß½èÍýÄÉ²Ã
      *  01.01.01    NACL-Æ£¸¶    04/10/13  Ãæ»ß½èÍý¤ÎÄÉ²Ã
      *  01.01.02    NACL-ÌçÏÆ    05/07/15  ¼«ÇåÀÕ½¾ÍèÍÍ¼°ÂÐ±þ
      *  02.09.01    NACL-ÌçÏÆ    06/04/13  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *  02.09.02    NACL-ÌçÏÆ    06/04/26  µßµÞ°åÎÅ´ÉÍý²Ã»»¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  03.04.01    NACL-ÌçÏÆ    06/12/01  ¼«ÇåÀÕ·òÊÝ½àµòÂÐ±þ
      *
      *  03.05.01    NACL-ÌçÏÆ    07/05/25  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.02    NACL-Æ£¸¶    07/06/01  realtime preview ½èÍýÄÉ²Ã
      *  04.01.01    NACL-ÌçÏÆ    07/10/31  ¸øÌ³ºÒ³²ÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    07/11/26  ÆÃÄê´ïºà¾¦ÉÊÌ¾µ­ºÜÂÐ±þ
      *  04.03.01    NACL-ÌçÏÆ    08/09/25  ¼«ÇåÀÕ£°£¹£µ£¹£³¤Î½¸·×ÀèÂÐ±þ
      *  04.05.01    NACL-ÌçÏÆ    12/04/11  Ê¿À®£²£´Ç¯£´·î²þÀµ
      *                                     ¥³¥á¥ó¥Èº¹¹þ
      *
      *  04.06.01    NACL-ÌçÏÆ    13/02/18  Ï«ºÒ¥ì¥»ÅÅÂÐ±þ
      *                                     ¡Ê¿·¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÂÐ±þ¡Ë
      *
      *  04.07.01    NACL-ÌçÏÆ    13/01/21  Ï«ºÒ¥ì¥»ÅÅÂÐ±þ
      *  04.07.02    NACL-ÌçÏÆ    13/10/02  ¼«ÇåÀÕ¡ÊÂè»°¼Ô¹Ô°Ù¡ËÂÐ±þ
      *  04.07.03    NACL-ÌçÏÆ    14/05/08  µßµÞ°åÎÅ´ÉÍý²Ã»»¤Î»»ÄêÆüµ­ºÜ
      *                                     ¤ä¤á¤ë
      *  04.07.04    NACL-ÌçÏÆ    14/07/08  ÌÀºÙ¤¬£µ£°¤òÄ¶¤¨¤ë¾ì¹ç¤ÎÂÐ±þ
      *
      *  04.08.01    NACL-Æ£¸¶    14/07/07  °ì»þ¥Ç¥£¥ì¥¯¥È¥êÂÐ±þ
      *  05.00.01    NACL-ÌçÏÆ    20/03/30  ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
      *
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
      *
           SELECT  RECE441-FILE    ASSIGN  RECE441PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE441-KEY
                                   FILE    STATUS  IS  STS-RECE441.
      *
           SELECT  RECE451-FILE    ASSIGN  RECE451PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE451-KEY
                                   FILE    STATUS  IS  STS-RECE451.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
      *
       FD  RECE44-FILE.
       01  RECE44-REC.
           COPY    "CPRCF044.INC".
      *
      *
       FD  RECE45-FILE.
       01  RECE45-REC.
           COPY    "CPRCF045.INC".
      *
       FD  RECE441-FILE.
       01  RECE441-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE441//.
      *
       FD  RECE451-FILE.
       01  RECE451-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE451//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE441//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE451//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE41          PIC X(02).
           03  STS-RECE44          PIC X(02).
           03  STS-RECE441         PIC X(02).
           03  STS-RECE45          PIC X(02).
           03  STS-RECE451         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE44          PIC 9(01).
           03  FLG-RECE45          PIC 9(01).
           03  FLG-WRKRECE45       PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
      *    
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE44          PIC 9(06).
           03  CNT-RECE45          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  DAY-IDX             PIC 9(02).
           03  IDX-RECE45          PIC 9(04).
      *     
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN            PIC  X(01).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-RECEERR         PIC X(200).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
      *
           03  WRK-TAIHI-SRYYMD.
               05  WRK-TAIHI-SRYYM             PIC  9(06).
               05  WRK-TAIHI-SRYD              PIC  9(02).
      *
       01  WRK-RECE45-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45//.
      *
       01  WRK-RECE45XX-REC.
         02 WRK-RECE45XX-TBL       OCCURS 100.
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45XX//.
      *
       01  MAX-MEISAISU            PIC 9(07).
      *
       01  WRK-FREE-AREA2.
           03  WRK-FREEINF2                  OCCURS 50.
               05  WK-FREE-YUKOKETA2         PIC  9(03).
               05  WK-FREE-NAME2             PIC  X(300).
               05  WK-FREE-MEISKYFLG2        PIC  X(01).
               05  WK-FREE-SETCOMATFLG2      PIC  9(03).
      *
       01  WRK-JIDO-SETCOM-AREA.
           03  WRK-JIDO-SETCOMATFLG          PIC  9(03).
           03  WRK-JIDO-SETCOMFLG            PIC  X(01).
      *
      *    ÆüÉÕÁ´³ÑÊÑ´¹ÎÎ°è
       01  TBL-TUKI-AREA.
           03  FILLER              PIC X(04)  VALUE "¡¡£±".
           03  FILLER              PIC X(04)  VALUE "¡¡£²".
           03  FILLER              PIC X(04)  VALUE "¡¡£³".
           03  FILLER              PIC X(04)  VALUE "¡¡£´".
           03  FILLER              PIC X(04)  VALUE "¡¡£µ".
           03  FILLER              PIC X(04)  VALUE "¡¡£¶".
           03  FILLER              PIC X(04)  VALUE "¡¡£·".
           03  FILLER              PIC X(04)  VALUE "¡¡£¸".
           03  FILLER              PIC X(04)  VALUE "¡¡£¹".
           03  FILLER              PIC X(04)  VALUE "£±£°".
           03  FILLER              PIC X(04)  VALUE "£±£±".
           03  FILLER              PIC X(04)  VALUE "£±£²".
       01  TBL-TUKI-AREA-R         REDEFINES   TBL-TUKI-AREA.
           03  TBL-TUKI            PIC X(04)   OCCURS   12.
      *
      *    ÆüÉÕÁ´³ÑÊÑ´¹ÎÎ°è
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "¡¡£±".
           03  FILLER              PIC X(04)  VALUE "¡¡£²".
           03  FILLER              PIC X(04)  VALUE "¡¡£³".
           03  FILLER              PIC X(04)  VALUE "¡¡£´".
           03  FILLER              PIC X(04)  VALUE "¡¡£µ".
           03  FILLER              PIC X(04)  VALUE "¡¡£¶".
           03  FILLER              PIC X(04)  VALUE "¡¡£·".
           03  FILLER              PIC X(04)  VALUE "¡¡£¸".
           03  FILLER              PIC X(04)  VALUE "¡¡£¹".
           03  FILLER              PIC X(04)  VALUE "£±£°".
           03  FILLER              PIC X(04)  VALUE "£±£±".
           03  FILLER              PIC X(04)  VALUE "£±£²".
           03  FILLER              PIC X(04)  VALUE "£±£³".
           03  FILLER              PIC X(04)  VALUE "£±£´".
           03  FILLER              PIC X(04)  VALUE "£±£µ".
           03  FILLER              PIC X(04)  VALUE "£±£¶".
           03  FILLER              PIC X(04)  VALUE "£±£·".
           03  FILLER              PIC X(04)  VALUE "£±£¸".
           03  FILLER              PIC X(04)  VALUE "£±£¹".
           03  FILLER              PIC X(04)  VALUE "£²£°".
           03  FILLER              PIC X(04)  VALUE "£²£±".
           03  FILLER              PIC X(04)  VALUE "£²£²".
           03  FILLER              PIC X(04)  VALUE "£²£³".
           03  FILLER              PIC X(04)  VALUE "£²£´".
           03  FILLER              PIC X(04)  VALUE "£²£µ".
           03  FILLER              PIC X(04)  VALUE "£²£¶".
           03  FILLER              PIC X(04)  VALUE "£²£·".
           03  FILLER              PIC X(04)  VALUE "£²£¸".
           03  FILLER              PIC X(04)  VALUE "£²£¹".
           03  FILLER              PIC X(04)  VALUE "£³£°".
           03  FILLER              PIC X(04)  VALUE "£³£±".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
      *
      *    ¸ÇÄêÃÍ
       01  CONST-AREA.
      *    µßµÞ°åÎÅ´ÉÍý²Ã»»
           03  CONS-KYUKYU     PIC X(09)   VALUE   "101110030".
           03  CONS-KYUKYU2    PIC X(09)   VALUE   "101800890".
      *
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    ¿ÇÎÅ²ÊÌÜ¾ðÊó
           COPY    "CPSK1005.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    ¿ôÃÍÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
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
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END  =  1
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0435"      TO  JOB-PGID
               MOVE    "¥³¥á¥ó¥ÈÊÔ½¸£²¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
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
           MOVE    "RECE44X"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE45PARA
           INITIALIZE                      RECE45PARA
           MOVE    "RECE451"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE441PARA
           INITIALIZE                      RECE441PARA
           MOVE    "RECE44B"           TO  RECE441PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE441PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE441PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE451PARA
           INITIALIZE                      RECE451PARA
           MOVE    "RECE45B"           TO  RECE451PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE451PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE451PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE44PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE45PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE441PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE451PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
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
                                       TO  RECE441PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE451PARA
      *
           OPEN    INPUT               RECE41-FILE
      *
           OPEN    INPUT               RECE44-FILE
      *
           OPEN    INPUT               RECE45-FILE
      *
           OPEN    OUTPUT              RECE441-FILE
           CLOSE                       RECE441-FILE
      *
           OPEN    OUTPUT              RECE451-FILE
           CLOSE                       RECE451-FILE
      *
           OPEN    I-O                 RECE441-FILE
      *
           OPEN    I-O                 RECE451-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      WRK-JIDO-SETCOM-AREA
      *
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPNUM      TO  RECE44-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                   )
               OR     ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               MOVE          SPACE       TO      RECE441-REC
               INITIALIZE                        RECE441-REC
      *
               EVALUATE   RECE44-SRYKBN
                 WHEN      "11"
                   IF  ((RECE41-HKNKBN   =  "4") AND
                        (RECE41-JIBAIKBN =  "2"))
                   OR  ((RECE41-HKNKBN   =  "5") AND
                        (RECE41-KOMUKBN  =  "2"))
                   OR  ( RECE41-HKNKBN   =  "6" )
                       MOVE       RECE44-REC    TO   RECE441-REC
                       WRITE      RECE441-REC
                       ADD     1          TO      CNT-RECE44
                       PERFORM    2002-RECE45-TENSOU-SEC
                   ELSE
                     IF    RECE44-FLG-SYODOU-COMMENT   =    "1"
                       PERFORM    2001-SYODOU-COMMENT-SEC
                     ELSE
                       MOVE       RECE44-REC    TO   RECE441-REC
                       WRITE      RECE441-REC
                       ADD     1          TO      CNT-RECE44
                       PERFORM    2002-RECE45-TENSOU-SEC
                     END-IF
                   END-IF
                 WHEN      "12"
                   IF  ((RECE41-HKNKBN   =  "4") AND
                        (RECE41-JIBAIKBN =  "2"))
                   OR  ((RECE41-HKNKBN   =  "5") AND
                        (RECE41-KOMUKBN  =  "2"))
                   OR  ( RECE41-HKNKBN   =  "6" )
                       MOVE       RECE44-REC    TO   RECE441-REC
                       WRITE      RECE441-REC
                       ADD     1          TO      CNT-RECE44
                       PERFORM    2002-RECE45-TENSOU-SEC
                   ELSE
                     IF    RECE44-FLG-SAIDOU-COMMENT   =    "1"
                       PERFORM    2001-SAIDOU-COMMENT-SEC
                     ELSE
                       MOVE       RECE44-REC    TO   RECE441-REC
                       WRITE      RECE441-REC
                       ADD     1          TO      CNT-RECE44
                       PERFORM    2002-RECE45-TENSOU-SEC
                     END-IF
                   END-IF
                 WHEN      "80"
                   IF (( RECE41-KYUKYU-PRTKBN    =      1         ) AND
                       ( RECE44-SRYCDTOTAL(6:9)  =   CONS-KYUKYU  ) AND
                       ( RECE41-SRYYM           <=   "201306"     ))
      ***          OR (( RECE41-KYUKYU-PRTKBN    =      1         ) AND
      ***              ( RECE44-SRYCDTOTAL(6:9)  =   CONS-KYUKYU2 ) AND
      ***              ( RECE41-SRYYM           >=   "201307"     ))
                   OR (( RECE44-JIBAISYUKEIKBN   =      4         ) AND
                       ( RECE44-SRYCDTOTAL(6:5)  =   "09593"      ) AND
                       ( RECE41-SRYYM           <=   "201306"     ))
                       MOVE    SPACE   TO   WRK-TAIHI-SRYYMD
                       IF  (RECE44-JIBAISYUKEIKBN   =    4   )
                       AND (RECE44-SRYCDTOTAL(6:5)  = "09593")
                         PERFORM  VARYING  IDX2  FROM  1  BY  1
                                   UNTIL   IDX2   >   31
                           IF  RECE44-KAISU(1 IDX2)   NOT =    ZERO
                             MOVE  RECE44-SRYYM  TO  WRK-TAIHI-SRYYM
                             MOVE  IDX2          TO  WRK-TAIHI-SRYD
                             MOVE  31            TO  IDX2
                           END-IF
                         END-PERFORM
                       ELSE
                         MOVE  RECE41-KYUKYUYMD  TO  WRK-TAIHI-SRYYMD
                       END-IF
                       PERFORM    2001-KYUKYU-COMMENT-SEC
                   ELSE
                       MOVE       RECE44-REC    TO   RECE441-REC
                       WRITE      RECE441-REC
                       ADD     1          TO      CNT-RECE44
                       PERFORM    2002-RECE45-TENSOU-SEC
                   END-IF
                 WHEN   OTHER
                       MOVE       RECE44-REC    TO   RECE441-REC
                       WRITE      RECE441-REC
                       ADD     1          TO      CNT-RECE44
                       PERFORM    2002-RECE45-TENSOU-SEC
               END-EVALUATE
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
      *
           END-PERFORM
      *
           IF      FLG-END         =   ZERO
               PERFORM 990-RECE41-READ-SEC
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    µßµÞ°åÎÅ´ÉÍý²Ã»»¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KYUKYU-COMMENT-SEC          SECTION.
      *
           INITIALIZE            WRK-JIDO-SETCOM-AREA
           MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
           MOVE      301       TO     WRK-JIDO-SETCOMATFLG
      *
           MOVE       RECE44-REC    TO   RECE441-REC
           ADD        1             TO   RECE441-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG
                                    TO   RECE441-SETCOMFLG
           END-IF
           WRITE      RECE441-REC
           ADD        1             TO   CNT-RECE44
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
                     PERFORM 990-RECE45-NEXT-SEC
           END-START
      *
           MOVE    RECE45-REC           TO  RECE451-REC
           MOVE    RECE45-SRYACT-TBL(1) TO  RECE451-SRYACT-TBL(1)
           MOVE    RECE45-SRYACT-TBL(2) TO  RECE451-SRYACT-TBL(3)
           MOVE    RECE45-SRYACT-TBL(3) TO  RECE451-SRYACT-TBL(4)
           MOVE    RECE45-SRYACT-TBL(4) TO  RECE451-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE451-SRYACT-TBL(2)
           INITIALIZE                       RECE451-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE451-SRYCD(2)
           MOVE    ZERO                 TO  RECE451-SRYSURYO(2)
           MOVE    9                    TO  RECE451-YUKOKETA(2)
      * 
           MOVE    "»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü" TO  RECE451-NAME(2)
      **   MOVE     RECE41-KYUKYUYMD(5:2)
           MOVE     WRK-TAIHI-SRYYMD(5:2)
                                        TO  DAY-IDX
           MOVE     TBL-TUKI(DAY-IDX)   TO  RECE451-NAME(2)(7:4)
      **   MOVE     RECE41-KYUKYUYMD(7:2)
           MOVE     WRK-TAIHI-SRYYMD(7:2)
                                        TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE451-NAME(2)(13:4)
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                  TO  RECE451-MEISKYFLG(2)
             MOVE  WRK-JIDO-SETCOMATFLG TO  RECE451-SETCOMATFLG(2)
           END-IF
      *
           MOVE     "2"                 TO  RECE451-SIKFLG(2)
      *
           WRITE    RECE451-REC
           ADD      1                   TO  CNT-RECE45
      *    
           .
       2001-KYUKYU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-SYODOU-COMMENT-SEC          SECTION.
      *
           INITIALIZE            WRK-JIDO-SETCOM-AREA
           MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
           MOVE      204       TO     WRK-JIDO-SETCOMATFLG
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE44-SYODOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "Ê£½é»»Äê¿ÇÎÅ²Ê¡§"  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE44-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE41-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key10"           TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM   VARYING   IDW   FROM   300   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
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
                     PERFORM 990-RECE45-NEXT-SEC
                     MOVE  RECE45-REC  TO  WRK-RECE45-REC
           END-START
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU     TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE45XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
                MOVE    "2"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM
             UNTIL  ( FLG-RECE45           =   1                   )
             OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
             OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
             OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
             OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
             OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
             OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
             OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
             OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE45-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
             END-PERFORM 
      *
             PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE451-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE451-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE451-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE451-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE451-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE451-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE451-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE451-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                     OR (IDX-RECE45                       >  100 )
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE451-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE45XX-SIKFLG(IDX-RECE45 1)  =  "2"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF
      *
                     END-IF
                  END-PERFORM
                  IF   RECE451-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE451-REC
                     ADD     1            TO   CNT-RECE45
                  END-IF
           END-PERFORM
      *
           MOVE    RECE44-REC        TO      RECE441-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE441-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG
                                     TO      RECE441-SETCOMFLG
           END-IF
           WRITE   RECE441-REC
           ADD     1                 TO      CNT-RECE44
      *
           .
       2001-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-SAIDOU-COMMENT-SEC          SECTION.
      *
           INITIALIZE            WRK-JIDO-SETCOM-AREA
           MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
           MOVE      225       TO     WRK-JIDO-SETCOMATFLG
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE44-SAIDOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "Ê£ºÆ»»Äê¿ÇÎÅ²Ê¡§"  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE44-SAIDOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE41-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key10"           TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM   VARYING   IDW   FROM   300   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
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
                     PERFORM 990-RECE45-NEXT-SEC
                     MOVE  RECE45-REC  TO  WRK-RECE45-REC
           END-START
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU     TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE45XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
                MOVE    "2"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM
             UNTIL  ( FLG-RECE45           =   1                   )
             OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
             OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
             OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
             OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
             OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
             OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
             OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
             OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE45-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
             END-PERFORM 
      *
             PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE451-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE451-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE451-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE451-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE451-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE451-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE451-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE451-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                     OR (IDX-RECE45                       >  100 )
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE451-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE45XX-SIKFLG(IDX-RECE45 1)  =  "2"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF
      *
                     END-IF
                  END-PERFORM
                  IF   RECE451-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE451-REC
                     ADD     1            TO   CNT-RECE45
                  END-IF
           END-PERFORM
      *
           MOVE    RECE44-REC        TO      RECE441-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE441-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG
                                     TO      RECE441-SETCOMFLG
           END-IF
           WRITE   RECE441-REC
           ADD     1                 TO      CNT-RECE44
      *
           .
       2001-SAIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µÅ¾Á÷½èÍý
      *****************************************************************
       2002-RECE45-TENSOU-SEC     SECTION.
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
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE45           =   1                   )
               OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
               OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
               OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
               OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
               OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
               OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
               OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
               OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
               MOVE          RECE45-REC       TO      RECE451-REC
               WRITE         RECE451-REC
               ADD           1                TO      CNT-RECE45
      *
               PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2002-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ¥Õ¥¡¥¤¥ë£Ã£Ì£Ï£Ó£Å½èÍý
           PERFORM 310-FILE-CLOSE-SEC
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
           DISPLAY "RECE41  IN  " CNT-RECE41  
           DISPLAY "RECE44  CNT " CNT-RECE44  
           DISPLAY "RECE45  CNT " CNT-RECE45  
           DISPLAY "*** ORCR0435 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ã£Ì£Ï£Ó£Å½èÍý
      *****************************************************************
       310-FILE-CLOSE-SEC              SECTION.
      *
           CLOSE                       RECE41-FILE
      *
           CLOSE                       RECE44-FILE
      *
           CLOSE                       RECE45-FILE
      *
           CLOSE                       RECE441-FILE
           CLOSE                       RECE451-FILE
      *
           .
       310-FILE-CLOSE-EXT.
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
      *
           .
       500-ERR-HENSYU-EXT.
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
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE41
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE44-NEXT-SEC           SECTION.
      *
           READ    RECE44-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE44
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE44
                   ADD     1         TO  CNT-IN-CANCEL
           END-READ
      *
           .
       990-RECE44-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE45-NEXT-SEC           SECTION.
      *
           READ    RECE45-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE45
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE45
           END-READ
      *
           .
       990-RECE45-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
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
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
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
