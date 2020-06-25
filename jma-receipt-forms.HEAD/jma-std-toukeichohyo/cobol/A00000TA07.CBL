      ******************************************************************
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
      *****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000TA07.
      ******************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æü¼¡Ä¢É¼
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Ï«ºÒ¼«ÇåÀÕ¸øÌ³ºÒ³²½¸·×É½
      *  ´ÉÍý¼Ô            : 
      *  Maj/Min/Rev  ½¤Àµ¼Ô  ÆüÉÕ       ÆâÍÆ
      *   01.00.00    Ìç´Ö    11/03/29   ¿·µ¬ºîÀ®
      *   01.00.01    Ìç´Ö    14/03/12   ¥Æ¥¹¥È´µ¼Ô¤òÂÐ¾Ý³°¤È¤¹¤ë
      *   02.00.01    Ìç´Ö    14/10/31   ver4.8.0ÂÐ±þ¡Ê°ì»þ¥Ç¥£¥ì¥¯¥È¥êÊÑ¹¹¡Ë
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    ¼«ÇåÀÕÏ«ºÒ  °ì»þ¥Õ¥¡¥¤¥ë
           SELECT  TA07-FILE       ASSIGN          TA07P
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TA07TMP-KEY
                                   FILE    STATUS  IS  STS-TA07TMP.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ¼«ÇåÀÕÏ«ºÒ  °ì»þ¥Õ¥¡¥¤¥ë
       FD  TA07-FILE.
           COPY    "A00000TA07TMP.INC".
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥·¥§¥ëÍÑÎÎ°è
           COPY    "CPCOMMONSHELL.INC".
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è
           COPY    "CPRECEERR.INC".
      *
      *    °ì»þ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //TA07P//.
      *
      *    ¼«ÇåÀÕ
           COPY    "A00000TA07J.INC".
      *
      *    Ï«ºÒ
           COPY    "A00000TA07R.INC".
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
           03  STS-TA07TMP             PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PRINT               PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SEIKYU              PIC 9(01).
           03  FLG-INVALID             PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-HEADER              PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-RENNUM              PIC 9(03).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX-NG                  PIC 9(01).
           03  IDX-NG2                 PIC 9(01).
           03  IDX-NG3                 PIC 9(01).
           03  IDX-MAX                 PIC 9(03).
           03  IDX-DAY                 PIC 9(02).
      *
           03  IDX-ST                  PIC 9(03).
           03  IDX-ED                  PIC 9(03).
           03  IDXP                    PIC 9(05).
      *
      *    ¥Ñ¥é¥á¥¿ÎÎ°è
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-PRTKBN         PIC X(01).
           03  WRK-PARA-PRTYM          PIC X(06).
           03  WRK-PARA-HKNKBN         PIC X(01).
           03  WRK-PARA-NYUGAIKBN      PIC 9(01).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-Z3                  PIC ZZ9.
           03  WRK-M9                  PIC -,---,--9.
           03  WRK-Z9                  PIC Z,ZZZ,ZZZ.
           03  WRK-Z92                 PIC ZZZZZZZZZ.
           03  WRK-M11                 PIC ---,---,--9.
           03  WRK-Z11                 PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z15                 PIC ZZZZZZZZZZZZZZZ.
           03  WRK-SYMD                PIC 9(08).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-DATA                PIC 9(01).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-NYUGAIKBN           PIC 9(01).
           03  WRK-SKYMONEY            PIC S9(08).
      *
           03  WRK-4001-TENTANKA       PIC 9(02)V9(01).
           03  WRK-4001-KOMUFLG        PIC X(01).
           03  WRK-4001-KOMUTENTANKA   PIC 9(02).
           03  WRK-TENTNK              PIC 9(02)V9(01).
      *
           03  WRK-HKNKBN-NUM          PIC X(01).
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-RENNUM      PIC X(50) VALUE "Ï¢ÈÖ".
               07  CSV-HEAD-PTNUM       PIC X(50) VALUE "´µ¼ÔÈÖ¹æ".
               07  CSV-HEAD-PTNAME      PIC X(50) VALUE "´µ¼Ô»áÌ¾".
               07  CSV-HEAD-HKNKBN      PIC X(50) VALUE "ÊÝ¸±¶èÊ¬".
               07  CSV-HEAD-RHNUM       PIC X(50) VALUE "Ï«Æ¯ÊÝ¸±ÈÖ¹æ".
               07  CSV-HEAD-SGIKBN      PIC X(50) VALUE "ºÒ³²¶èÊ¬".
               07  CSV-HEAD-SKSBETU     PIC X(50) VALUE "¿··ÑºÆÊÌ".
               07  CSV-HEAD-SKYKBN      PIC X(50) VALUE "ÀÁµá¶èÊ¬".
               07  CSV-HEAD-TENKI       PIC X(50) VALUE "Å¾µ¢»öÍ³".
               07  CSV-HEAD-SYMD        PIC X(50) VALUE "½ýÉÂÇ¯·îÆü".
               07  CSV-HEAD-RYMDST      PIC X(50) VALUE "ÎÅÍÜ³«»ÏÆü".
               07  CSV-HEAD-RYMDED      PIC X(50) VALUE "ÎÅÍÜ½ªÎ»Æü".
               07  CSV-HEAD-SKYTENSU    PIC X(50) VALUE "ÀÁµáÅÀ¿ô".
               07  CSV-HEAD-SKYMONEY    PIC X(50) VALUE "ÀÁµá¶â³Û".
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 14.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE 14.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó
           COPY    "CPSK1001.INC".
      *
      *    ¿ÇÎÅ²ÊÌÜ¾ðÊó
           COPY    "CPSK1005.INC".
      *
      *    °å»Õ¾ðÊó
           COPY    "CPSK1010.INC".
      *
      *    ½ÐÎÏÀè¥×¥ê¥ó¥¿Ì¾³ä¤êÅö¤Æ¾ðÊó
           COPY    "CPSK1031.INC".
      *
      *    Ï«ºÒ°åÎÅµ¡´Ø¾ðÊó
           COPY    "CPSK4001.INC".
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ÀÁµá´ÉÍý¥Þ¥¹¥¿
           COPY    "CPRCF010.INC".
      *
      *    ´µ¼ÔÈÖ¹æ¥Þ¥¹¥¿
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ´µ¼ÔÏ«ºÒ¥Þ¥¹¥¿
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *
           COPY    "CPORCSLNK.INC".
      *
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *
      *    Åý·×£Ã£Ó£ÖÀ©¸æ¥µ¥Ö
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    °ì»þ¥Õ¥¡¥¤¥ëÌ¾ÊÔ½¸
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE            SECTION.
       01  COMMAND-PARAM.
           02  FILLER            PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           IF  FLG-END  NOT =  1
             PERFORM  300-MAIN-SEC
           END-IF
      *
           PERFORM  600-END-SEC
      *
           STOP  RUN
           .
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
           INITIALIZE                    FLG-AREA
                                         STS-AREA
                                         WRK-AREA
                                         CNT-AREA
                                         SPA-AREA
                                         IDX-AREA
           INITIALIZE                    RECEERR
      *
           PERFORM  100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM      DELIMITED  BY  ","
                                         INTO  LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-PRTYM
                                               WRK-PARA-HKNKBN
                                               WRK-PARA-NYUGAIKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM            TO  SPA-HOSPNUM
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"                       TO  SJOBKANRI-MODE
           INITIALIZE                          JOBKANRI-REC
           MOVE    SPA-HOSPNUM                 TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID              TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID            TO  JOB-SHELLID
           MOVE    "A00000TA07"                TO  JOB-PGID
           MOVE    "Ï«ºÒ¼«ÇåÀÕ¸øÌ³ºÒ³²½¸·×É½"  TO  JOB-SHELLMSG
           CALL    "ORCSJOB"                   USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
      *
      *    ¥µ¥Ö¥×¥í¥°¥é¥à¤è¤ê°ì»þ¥Õ¥¡¥¤¥ëÌ¾ÊÔ½¸
           MOVE    "ATA07TP"                TO  TA07P-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  TA07P-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  TA07P-HOSPNUM
      *
           MOVE    TA07P-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  TA07P-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    ¥Ñ¥é¥á¥¿ÊÔ½¸½èÍý
           PERFORM  200-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¥¿ÊÔ½¸½èÍý
      *****************************************************************
       200-PARA-HENSYU-SEC            SECTION.
      *
           MOVE    ZERO                  TO  FLG-END
      *
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           INITIALIZE                        SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTYM        TO  SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                  TO  SYS-1001-EDYUKYMD(7:2)
           MOVE    WRK-PARA-PRTYM        TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "31"                  TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM   900-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE
             MOVE    "°åÎÅµ¡´Ø£É£Ä¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                         TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    Ï«ºÒ°åÎÅµ¡´Ø¾ðÊó
           MOVE    SPACE                 TO  SYS-4001-REC
           INITIALIZE                    SYS-4001-REC
           MOVE    "4001"                TO  SYS-4001-KANRICD
           MOVE    "*"                   TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-4001-HOSPNUM
           MOVE    WRK-PARA-PRTYM        TO  SYS-4001-EDYUKYMD(1:6)
           MOVE    "01"                  TO  SYS-4001-EDYUKYMD(7:2)
           MOVE    WRK-PARA-PRTYM        TO  SYS-4001-STYUKYMD(1:6)
           MOVE    "31"                  TO  SYS-4001-STYUKYMD(7:2)
           MOVE    SYS-4001-REC          TO  MCPDATA-REC
           PERFORM   900-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC         TO   SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5            TO   WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12              TO   WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10              TO   WRK-4001-TENTANKA
             END-EVALUATE
      *
             IF  SYS-4001-KOMUKBN  =  "2"
      *    ¸øÌ³ºÒ³²(·òÊÝ´ð½à)»þ¤ÎÅÀ¿ôÃ±²ÁÀßÄê
               MOVE    SYS-4001-KOMUTENTANKA-X TO WRK-4001-KOMUTENTANKA
      *    ¸øÌ³ºÒ³²¶èÊ¬¤¬·òÊÝ´ð½à¤Î¾ì¹ç¡¢£±¤ò³ÊÇ¼
               MOVE    "1"              TO  WRK-4001-KOMUFLG
             END-IF
           ELSE
             MOVE    10               TO  WRK-4001-TENTANKA
           END-IF
      *
      *    Æþ³°¶èÊ¬¥Á¥§¥Ã¥¯
           IF     WRK-PARA-NYUGAIKBN  NOT =  "1"
             AND  WRK-PARA-NYUGAIKBN  NOT =  "2"
             AND  WRK-PARA-NYUGAIKBN  NOT =  ZERO
             MOVE    "Æþ³°¶èÊ¬¤¬´Ö°ã¤Ã¤Æ¤¤¤Þ¤¹"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           .
       200-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       300-MAIN-SEC            SECTION.
      *
           OPEN   OUTPUT  TA07-FILE
           CLOSE  TA07-FILE
           OPEN   I-O     TA07-FILE
      *
           EVALUATE  WRK-PARA-PRTKBN
           WHEN  "1"
             PERFORM  400-BODY-SYUNOU-SEC
           WHEN  OTHER
             PERFORM  400-BODY-SEIKYU-SEC
           END-EVALUATE
      *
           CLOSE  TA07-FILE
      *
      *    °õºþ½èÍý
           IF  WRK-DATA  NOT =  ZERO
      *      °õ»ú¥Ç¡¼¥¿ÊÔ½¸½èÍý
             EVALUATE  WRK-PARA-HKNKBN
               WHEN  "1"
                 PERFORM  400-PRINT-HEN-ROSAI-SEC
               WHEN  "2"
                 PERFORM  400-PRINT-HEN-JIBAI-SEC
               WHEN  "3"
                 PERFORM  400-PRINT-HEN-JIBAI-SEC
               WHEN  OTHER
                 PERFORM  400-PRINT-HEN-ROSAI-SEC
                 PERFORM  400-PRINT-HEN-JIBAI-SEC
               END-EVALUATE
           ELSE
             MOVE    "¥Ç¡¼¥¿¤¬¤¢¤ê¤Þ¤»¤ó"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           .
       300-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>
      *****************************************************************
       400-BODY-SYUNOU-SEC            SECTION.
      *
           EVALUATE  WRK-PARA-NYUGAIKBN
           WHEN  "1"
             MOVE    1             TO  IDX-NG2
             MOVE    1             TO  IDX-NG3
           WHEN  "2"
             MOVE    2             TO  IDX-NG2
             MOVE    2             TO  IDX-NG3
           WHEN  OTHER
             MOVE    1             TO  IDX-NG2
             MOVE    2             TO  IDX-NG3
           END-EVALUATE
      *
      *    Ä¢É¼ÊÔ½¸
           PERFORM  VARYING  IDX-NG  FROM  IDX-NG2  BY  1
                    UNTIL    IDX-NG  >     IDX-NG3
      *
             PERFORM  900-DBSELECT-SEC
             PERFORM  UNTIL  FLG-SYUNOU  = 1
      *
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SYU-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SYU-PTID            TO  COMB-PTID
               MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
               PERFORM  910-HKNCOMBI-READ-SEC
      *
               INITIALIZE                       PTRSIINF-REC
               MOVE    SYU-HOSPNUM          TO  PTRSI-HOSPNUM
               MOVE    SYU-PTID             TO  PTRSI-PTID
               MOVE    COMB-HKNID           TO  PTRSI-HKNID
               PERFORM  920-PTRSIINF-READ-SEC
      *
               INITIALIZE  TA07TMP
               EVALUATE  WRK-PARA-HKNKBN
      *        Ï«ºÒ
               WHEN  "1"
                 IF  SYU-SYUHKNNUM  =  "971"
                     AND  (  PTRSI-HKNKBN  =  "1"  OR  "2"  OR  "3" )
                   PERFORM  410-SYUNOU-ROSAI-SEC
                 END-IF
      *        ¼«ÇåÀÕ
               WHEN  "2"
                 IF  SYU-SYUHKNNUM  =  "973"
                     AND  ( PTRSI-HKNKBN  =  "4" )
                   PERFORM  410-SYUNOU-JIBAI-SEC
                 END-IF
      *        ¸øÌ³ºÒ³²
               WHEN  "3"
                 IF  SYU-SYUHKNNUM  =  "971"
                     AND  ( PTRSI-HKNKBN  =  "5" )
                   PERFORM  410-SYUNOU-JIBAI-SEC
                 END-IF
               WHEN  OTHER
      *          Ï«ºÒ
                 IF  SYU-SYUHKNNUM  =  "971"
                     AND  ( PTRSI-HKNKBN  =  "1"  OR  "2"  OR  "3" )
                   PERFORM  410-SYUNOU-ROSAI-SEC
                 END-IF
      *          ¼«ÇåÀÕ
                 IF  SYU-SYUHKNNUM  =  "973"
                     AND  ( PTRSI-HKNKBN  =  "4" )
                   PERFORM  410-SYUNOU-JIBAI-SEC
                 END-IF
      *          ¸øÌ³ºÒ³²
                 IF  SYU-SYUHKNNUM  =  "971"
                     AND  ( PTRSI-HKNKBN  =  "5" )
                   PERFORM  410-SYUNOU-JIBAI-SEC
                 END-IF
               END-EVALUATE
      *
               PERFORM  900-DBFETCH-SEC
             END-PERFORM
             PERFORM  900-DBCLOSE-SEC
           END-PERFORM
      *
           .
       400-BODY-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>
      *****************************************************************
       400-BODY-SEIKYU-SEC            SECTION.
      *
           EVALUATE  WRK-PARA-NYUGAIKBN
           WHEN  "1"
             MOVE    1             TO  IDX-NG2
             MOVE    1             TO  IDX-NG3
           WHEN  "2"
             MOVE    2             TO  IDX-NG2
             MOVE    2             TO  IDX-NG3
           WHEN  OTHER
             MOVE    1             TO  IDX-NG2
             MOVE    2             TO  IDX-NG3
           END-EVALUATE
      *
      *    Ä¢É¼ÊÔ½¸
           PERFORM  VARYING  IDX-NG  FROM  IDX-NG2  BY  1
                    UNTIL    IDX-NG  >     IDX-NG3
      *
             PERFORM  910-DBSELECT-SEC
             PERFORM  UNTIL  FLG-SEIKYU  = 1
      *
               INITIALIZE                  HKNCOMBI-REC
               MOVE    RECE10-HOSPNUM      TO  COMB-HOSPNUM
               MOVE    RECE10-PTID         TO  COMB-PTID
               MOVE    RECE10-HKNCOMBI(1)  TO  COMB-HKNCOMBINUM
               PERFORM  910-HKNCOMBI-READ-SEC
      *
               INITIALIZE                       PTRSIINF-REC
               MOVE    RECE10-HOSPNUM      TO  PTRSI-HOSPNUM
               MOVE    RECE10-PTID         TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               PERFORM  920-PTRSIINF-READ-SEC
      *
               INITIALIZE  TA07TMP
               EVALUATE  WRK-PARA-HKNKBN
      *        Ï«ºÒ
               WHEN  "1"
                 IF      RECE10-TEISYUTUSAKI  =  "3"
                   AND ( RECE10-HKNKBN        =  "1"  OR  "2"  OR  "3" )
      *            ÊÝ¸±¶èÊ¬
                   MOVE    "1"  TO  TA07TMP-HKNKBN-NUM
                   PERFORM  410-SEIKYU-ROSAI-SEC
                 END-IF
      *        ¼«ÇåÀÕ
               WHEN  "2"
                 IF  RECE10-TEISYUTUSAKI   =  "4"
      *            ÊÝ¸±¶èÊ¬
                   MOVE    "2"  TO  TA07TMP-HKNKBN-NUM
                   PERFORM  410-SEIKYU-JIBAI-SEC
                 END-IF
      *        ¸øÌ³ºÒ³²
               WHEN  "3"
                 IF      RECE10-TEISYUTUSAKI  =  "3"
                   AND ( RECE10-HKNKBN        =  "4" )
      *            ÊÝ¸±¶èÊ¬
                   MOVE    "3"  TO  TA07TMP-HKNKBN-NUM
                   PERFORM  410-SEIKYU-JIBAI-SEC
                 END-IF
               WHEN  OTHER
      *          Ï«ºÒ
                 IF      RECE10-TEISYUTUSAKI  =  "3"
                   AND ( RECE10-HKNKBN        =  "1"  OR  "2"  OR  "3" )
      *            ÊÝ¸±¶èÊ¬
                   MOVE    "1"  TO  TA07TMP-HKNKBN-NUM
                   PERFORM  410-SEIKYU-ROSAI-SEC
                 END-IF
      *          ¼«ÇåÀÕ
                 IF  RECE10-TEISYUTUSAKI   =  "4"
      *            ÊÝ¸±¶èÊ¬
                   MOVE    "2"  TO  TA07TMP-HKNKBN-NUM
                   PERFORM  410-SEIKYU-JIBAI-SEC
                 END-IF
      *          ¸øÌ³ºÒ³²
                 IF      RECE10-TEISYUTUSAKI  =  "3"
                   AND ( RECE10-HKNKBN        =  "4" )
      *            ÊÝ¸±¶èÊ¬
                   MOVE    "3"  TO  TA07TMP-HKNKBN-NUM
                   PERFORM  410-SEIKYU-JIBAI-SEC
                 END-IF
               END-EVALUATE
      *
               PERFORM  910-DBFETCH-SEC
             END-PERFORM
             PERFORM  910-DBCLOSE-SEC
           END-PERFORM
      *
           .
       400-BODY-SEIKYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>
      *****************************************************************
       410-SYUNOU-ROSAI-SEC            SECTION.
      *
      *    ÊÝ¸±¶èÊ¬
           EVALUATE  PTRSI-HKNKBN
           WHEN  "1"
           WHEN  "2"
           WHEN  "3"
             MOVE    "1"              TO  TA07TMP-HKNKBN-NUM
           WHEN  "4"
             MOVE    "2"              TO  TA07TMP-HKNKBN-NUM
           WHEN  "5"
             MOVE    "3"              TO  TA07TMP-HKNKBN-NUM
           END-EVALUATE
      *
      *    Æþ³°¶èÊ¬
           MOVE    SYU-NYUGAIKBN    TO  TA07TMP-NYUGAI
      *
      *    ´µ¼ÔÈÖ¹æ(KEY)
           PERFORM  920-PTNUM-READ-SEC
           MOVE    PTNUM-PTNUM  TO  TA07TMP-PTNUM
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    SYU-HKNCOMBINUM    TO  TA07TMP-HKNCOMBI
      *
      *    ¾ðÊóÆÉ¹þ
           PERFORM  700-TMP-READ-SEC
      *
      *    ÊÝ¸±ÈÖ¹æ
           MOVE    SYU-SYUHKNNUM    TO  TA07TMP-HKNNUM
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  PTNUM-PTNUM(11:1)  =  SPACE
             MOVE    PTNUM-PTNUM  TO  TA07TMP-PTNUM10
           ELSE
             MOVE    PTNUM-PTNUM  TO  TA07TMP-PTNUM20
           END-IF
      *
      *    »áÌ¾
           INITIALIZE                    PTINF-REC
           MOVE    SYU-HOSPNUM           TO   PTINF-HOSPNUM
           MOVE    SYU-PTID              TO   PTINF-PTID
           PERFORM  920-PTINF-READ-SEC
           IF  PTINF-NAME(25:2)  =  SPACE
             MOVE    PTINF-NAME   TO  TA07TMP-NAME24
           ELSE
             MOVE    PTINF-NAME   TO  TA07TMP-NAME48
           END-IF
      *
      *    ÊÝ¸±¶èÊ¬
           EVALUATE  PTRSI-HKNKBN
           WHEN  "1"
             MOVE    "Ã»´üµëÉÕ"      TO  TA07TMP-HKNKBN
           WHEN  "2"
             MOVE    "½ýÉÂÇ¯¶â"      TO  TA07TMP-HKNKBN
           WHEN  "3"
             MOVE    "¥¢¥Õ¥¿¡¼¥±¥¢"  TO  TA07TMP-HKNKBN
           WHEN  "4"
             MOVE    "¼«ÇåÀÕÊÝ¸±"    TO  TA07TMP-HKNKBN
           WHEN  "5"
             MOVE    "¸øÌ³ºÒ³²"      TO  TA07TMP-HKNKBN
           END-EVALUATE
      *
      *    Ï«Æ¯ÊÝ¸±ÈÖ¹æ
           MOVE    PTRSI-KOFUNUM     TO  TA07TMP-RHNUM
      *
      *    ºÒ³²¶èÊ¬
           EVALUATE  PTRSI-SAIGAIKBN
           WHEN  "1"
             MOVE    "¶ÈÌ³Ãæ¤ÎºÒ³²"    TO  TA07TMP-SGIKBN
           WHEN  "2"
             MOVE    "ÄÌ¶ÐÅÓÃæ¤ÎºÒ³²"  TO  TA07TMP-SGIKBN
           END-EVALUATE
      *
      *    ¿··ÑºÆÊÌ
           EVALUATE  PTRSI-SINKEI
           WHEN  "1"
             MOVE    "½é¿Ç"          TO  TA07TMP-SKSBETU
           WHEN  "3"
             MOVE    "Å¾°å»Ï¿Ç"      TO  TA07TMP-SKSBETU
           WHEN  "5"
             MOVE    "·ÑÂ³"          TO  TA07TMP-SKSBETU
           WHEN  "7"
             MOVE    "ºÆÈ¯"          TO  TA07TMP-SKSBETU
           END-EVALUATE
      *
      *    Å¾µ¢»öÍ³
           EVALUATE  PTRSI-TENKI
           WHEN  "1"
             MOVE    "¼£Ìþ"       TO  TA07TMP-TENKI
           WHEN  "3"
             MOVE    "·ÑÂ³"       TO  TA07TMP-TENKI
           WHEN  "5"
             MOVE    "Å¾°å"       TO  TA07TMP-TENKI
           WHEN  "7"
             MOVE    "Ãæ»ß"       TO  TA07TMP-TENKI
           WHEN  "9"
             MOVE    "»àË´"       TO  TA07TMP-TENKI
           END-EVALUATE
      *
           IF  PTRSI-HKNKBN  NOT =  "3"
      *
      *      ½ýÉÂÇ¯·îÆü
             MOVE    PTRSI-SHOBYOYMD   TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-SYMD
      *
      *      ÎÅÍÜ³«»ÏÆü
             MOVE    PTRSI-RYOSTYMD    TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDST
      *
      *      ÎÅÍÜ½ªÎ»Æü
             IF  PTRSI-RYOEDYMD  NOT =  "99999999"
               MOVE    PTRSI-RYOEDYMD    TO  WRK-SYMD
               PERFORM  700-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDED
             END-IF
      *
           ELSE
      *
      *      ½ýÉÂÇ¯·îÆü
             MOVE    SPACE               TO  TA07TMP-SYMD
      *
      *      ÎÅÍÜ³«»ÏÆü
             MOVE    PTRSI-SINSATUYMD    TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1    TO  TA07TMP-RYMDST
      *
      *      ÎÅÍÜ½ªÎ»Æü
             IF  PTRSI-SINSATUEND  NOT =  "99999999"
               MOVE    PTRSI-SINSATUEND  TO  WRK-SYMD
               PERFORM  700-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDED
             END-IF
      *
           END-IF
      *
      *    ÀÁµáÅÀ¿ô
           ADD     SYU-TOTAL-HKNTEN       TO  TA07TMP-SKYTENSU
           ADD     SYU-TOTAL-TGMONEY      TO  TA07TMP-SKYTENSU
           ADD     SYU-TOTAL-TGMONEY-TAX  TO  TA07TMP-SKYTENSU
      *
      *    ÀÁµá¶â³Û
           COMPUTE  TA07TMP-SKYMONEY  =  TA07TMP-SKYMONEY
                                      +  SYU-RSIJIBAI-SKYMONEY
                                      -  SYU-JIHI-TOTAL
                                      -  SYU-JIHI-TOTAL-TAX
                                      -  SYU-RMSAGAKU
                                      -  SYU-RMSAGAKU-TAX-SAI
      *
      *    ¥Ç¡¼¥¿½ñ¹þ¤ß
           IF  FLG-INVALID  =  1
             WRITE  TA07TMP
             ADD    1  TO  WRK-DATA
             DISPLAY  "WRITE"
           ELSE
             REWRITE  TA07TMP
             DISPLAY  "REWRITE"
           END-IF
      *
           .
       410-SYUNOU-ROSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>
      *****************************************************************
       410-SYUNOU-JIBAI-SEC            SECTION.
      *
      *    ÊÝ¸±¶èÊ¬
           EVALUATE  PTRSI-HKNKBN
           WHEN  "1"
           WHEN  "2"
           WHEN  "3"
             MOVE    "1"              TO  TA07TMP-HKNKBN-NUM
           WHEN  "4"
             MOVE    "2"              TO  TA07TMP-HKNKBN-NUM
           WHEN  "5"
             MOVE    "3"              TO  TA07TMP-HKNKBN-NUM
           END-EVALUATE
      *
      *    Æþ³°¶èÊ¬
           MOVE    SYU-NYUGAIKBN    TO  TA07TMP-NYUGAI
      *
      *    ´µ¼ÔÈÖ¹æ(KEY)
           PERFORM  920-PTNUM-READ-SEC
           MOVE    PTNUM-PTNUM      TO  TA07TMP-PTNUM
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    SYU-HKNCOMBINUM    TO  TA07TMP-HKNCOMBI
      *
      *    ¾ðÊóÆÉ¹þ
           PERFORM  700-TMP-READ-SEC
      *
      *    ÊÝ¸±ÈÖ¹æ
           MOVE    SYU-SYUHKNNUM    TO  TA07TMP-HKNNUM
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  PTNUM-PTNUM(11:1)  =  SPACE
             MOVE    PTNUM-PTNUM      TO  TA07TMP-PTNUM10
           ELSE
             MOVE    PTNUM-PTNUM      TO  TA07TMP-PTNUM20
           END-IF
      *
      *    »áÌ¾
           INITIALIZE                    PTINF-REC
           MOVE    SYU-HOSPNUM           TO   PTINF-HOSPNUM
           MOVE    SYU-PTID              TO   PTINF-PTID
           PERFORM  920-PTINF-READ-SEC
           IF  PTINF-NAME(25:2)  =  SPACE
             MOVE    PTINF-NAME       TO  TA07TMP-NAME24
           ELSE
             MOVE    PTINF-NAME       TO  TA07TMP-NAME48
           END-IF
      *
      *    ÊÝ¸±¶èÊ¬
           EVALUATE  PTRSI-HKNKBN
           WHEN  "1"
             MOVE    "Ã»´üµëÉÕ"       TO  TA07TMP-HKNKBN
           WHEN  "2"
             MOVE    "½ýÉÂÇ¯¶â"       TO  TA07TMP-HKNKBN
           WHEN  "3"
             MOVE    "¥¢¥Õ¥¿¡¼¥±¥¢"   TO  TA07TMP-HKNKBN
           WHEN  "4"
             MOVE    "¼«ÇåÀÕÊÝ¸±"     TO  TA07TMP-HKNKBN
           WHEN  "5"
             MOVE    "¸øÌ³ºÒ³²"       TO  TA07TMP-HKNKBN
           END-EVALUATE
      *
      *    ÀÁµá¶èÊ¬
           EVALUATE  PTRSI-JIBAISEIKBN
           WHEN  "1"
             MOVE    "´µ¼ÔÀÁµá¤¹¤ë¡Ê£±£°£°¡ó¡Ë"  TO  TA07TMP-SKYKBN
           WHEN  "2"
             MOVE    "´µ¼ÔÀÁµá¤·¤Ê¤¤¡Ê£°¡ó¡Ë"    TO  TA07TMP-SKYKBN
           END-EVALUATE
      *
      *    Å¾µ¢»öÍ³
           EVALUATE  PTRSI-TENKI
           WHEN  "1"
             MOVE    "¼£Ìþ"       TO  TA07TMP-TENKI
           WHEN  "3"
             MOVE    "·ÑÂ³"       TO  TA07TMP-TENKI
           WHEN  "5"
             MOVE    "Å¾°å"       TO  TA07TMP-TENKI
           WHEN  "7"
             MOVE    "Ãæ»ß"       TO  TA07TMP-TENKI
           WHEN  "9"
             MOVE    "»àË´"       TO  TA07TMP-TENKI
           END-EVALUATE
      *
      *    ½ýÉÂÇ¯·îÆü
           MOVE    PTRSI-SHOBYOYMD   TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-SYMD
      *
      *    ÎÅÍÜ³«»ÏÆü
           MOVE    PTRSI-RYOSTYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDST
      *
      *    ÎÅÍÜ½ªÎ»Æü
           IF  PTRSI-RYOEDYMD  NOT =  "99999999"
             MOVE    PTRSI-RYOEDYMD    TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDED
           END-IF
      *
      *    ÀÁµáÅÀ¿ô
           ADD     SYU-TOTAL-HKNTEN       TO  TA07TMP-SKYTENSU
           ADD     SYU-TOTAL-TGMONEY      TO  TA07TMP-SKYTENSU
           ADD     SYU-TOTAL-TGMONEY-TAX  TO  TA07TMP-SKYTENSU
      *
      *    ÀÁµá¶â³Û
           COMPUTE  TA07TMP-SKYMONEY  =  TA07TMP-SKYMONEY
                                      +  SYU-RSIJIBAI-SKYMONEY
                                      -  SYU-JIHI-TOTAL
                                      -  SYU-JIHI-TOTAL-TAX
                                      -  SYU-RMSAGAKU
                                      -  SYU-RMSAGAKU-TAX-SAI
      *
      *    ¥Ç¡¼¥¿½ñ¹þ¤ß
           IF  FLG-INVALID  =  1
             WRITE  TA07TMP
             ADD    1  TO  WRK-DATA
             DISPLAY  "WRITE"
           ELSE
             REWRITE  TA07TMP
             DISPLAY  "REWRITE"
           END-IF
      *
           .
       410-SYUNOU-JIBAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>
      *****************************************************************
       410-SEIKYU-ROSAI-SEC            SECTION.
      *
      *    Æþ³°¶èÊ¬
           MOVE    RECE10-NYUGAIKBN    TO  TA07TMP-NYUGAI
      *
      *    ´µ¼ÔÈÖ¹æ
           MOVE    RECE10-PTNUM        TO  TA07TMP-PTNUM
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    RECE10-HKNCOMBI(1)  TO  TA07TMP-HKNCOMBI
      *
      *    ¾ðÊóÆÉ¹þ
           PERFORM  700-TMP-READ-SEC
      *
      *    ÊÝ¸±ÈÖ¹æ
           MOVE    RECE10-HKNNUM       TO  TA07TMP-HKNNUM
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  RECE10-PTNUM(11:1)  =  SPACE
             MOVE    RECE10-PTNUM        TO  TA07TMP-PTNUM10
           ELSE
             MOVE    RECE10-PTNUM        TO  TA07TMP-PTNUM20
           END-IF
      *
      *    »áÌ¾
           INITIALIZE                    PTINF-REC
           MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID         TO  PTINF-PTID
           PERFORM  920-PTINF-READ-SEC
           IF  PTINF-NAME(25:2)  =  SPACE
             MOVE    PTINF-NAME          TO  TA07TMP-NAME24
           ELSE
             MOVE    PTINF-NAME          TO  TA07TMP-NAME48
           END-IF
      *
      *    ÊÝ¸±¶èÊ¬
           EVALUATE  RECE10-HKNKBN
           WHEN  "1"
             MOVE    "Ã»´üµëÉÕ"      TO  TA07TMP-HKNKBN
           WHEN  "2"
             MOVE    "½ýÉÂÇ¯¶â"      TO  TA07TMP-HKNKBN
           WHEN  "3"
             MOVE    "¥¢¥Õ¥¿¡¼¥±¥¢"  TO  TA07TMP-HKNKBN
           WHEN  "4"
             MOVE    "¸øÌ³ºÒ³²"      TO  TA07TMP-HKNKBN
           WHEN  OTHER
             MOVE    "¼«ÇåÀÕÊÝ¸±"    TO  TA07TMP-HKNKBN
           END-EVALUATE
      *
      *    Ï«Æ¯ÊÝ¸±ÈÖ¹æ
           MOVE    RECE10-KOFUNUM    TO  TA07TMP-RHNUM
      *
      *    ºÒ³²¶èÊ¬
           EVALUATE  RECE10-SAIGAIKBN
           WHEN  "1"
             MOVE    "¶ÈÌ³Ãæ¤ÎºÒ³²"    TO  TA07TMP-SGIKBN
           WHEN  "2"
             MOVE    "ÄÌ¶ÐÅÓÃæ¤ÎºÒ³²"  TO  TA07TMP-SGIKBN
           END-EVALUATE
      *
      *    ¿··ÑºÆÊÌ
           EVALUATE  PTRSI-SINKEI
           WHEN  "1"
             MOVE    "½é¿Ç"          TO  TA07TMP-SKSBETU
           WHEN  "3"
             MOVE    "Å¾°å»Ï¿Ç"      TO  TA07TMP-SKSBETU
           WHEN  "5"
             MOVE    "·ÑÂ³"          TO  TA07TMP-SKSBETU
           WHEN  "7"
             MOVE    "ºÆÈ¯"          TO  TA07TMP-SKSBETU
           END-EVALUATE
      *
      *    Å¾µ¢»öÍ³
           EVALUATE  PTRSI-TENKI
           WHEN  "1"
             MOVE    "¼£Ìþ"       TO  TA07TMP-TENKI
           WHEN  "3"
             MOVE    "·ÑÂ³"       TO  TA07TMP-TENKI
           WHEN  "5"
             MOVE    "Å¾°å"       TO  TA07TMP-TENKI
           WHEN  "7"
             MOVE    "Ãæ»ß"       TO  TA07TMP-TENKI
           WHEN  "9"
             MOVE    "»àË´"       TO  TA07TMP-TENKI
           END-EVALUATE
      *
           IF  PTRSI-HKNKBN  NOT =  "3"
      *
      *      ½ýÉÂÇ¯·îÆü
             MOVE    PTRSI-SHOBYOYMD   TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-SYMD
      *
      *      ÎÅÍÜ³«»ÏÆü
             MOVE    PTRSI-RYOSTYMD    TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDST
      *
      *      ÎÅÍÜ½ªÎ»Æü
             IF  PTRSI-RYOEDYMD  NOT =  "99999999"
               MOVE    PTRSI-RYOEDYMD    TO  WRK-SYMD
               PERFORM  700-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDED
             END-IF
      *
           ELSE
      *
      *      ½ýÉÂÇ¯·îÆü
             MOVE    SPACE               TO  TA07TMP-SYMD
      *
      *      ÎÅÍÜ³«»ÏÆü
             MOVE    PTRSI-SINSATUYMD    TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1    TO  TA07TMP-RYMDST
      *
      *      ÎÅÍÜ½ªÎ»Æü
             IF  PTRSI-SINSATUEND  NOT =  "99999999"
               MOVE    PTRSI-SINSATUEND  TO  WRK-SYMD
               PERFORM  700-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDED
             END-IF
      *
           END-IF
      *
      *    ÀÁµáÅÀ¿ô
           ADD     RECE10-TOTALTEN(1)  TO  TA07TMP-SKYTENSU
      *
      *    ÀÁµá¶â³Û
           ADD     RECE10-FTNMONEY(1)  TO  TA07TMP-SKYMONEY
      *
      *    ¥Ç¡¼¥¿½ñ¹þ¤ß
           IF  FLG-INVALID  =  1
             WRITE  TA07TMP
             ADD    1  TO  WRK-DATA
             DISPLAY  "WRITE"
           ELSE
             REWRITE  TA07TMP
             DISPLAY  "REWRITE"
           END-IF
      *
           .
       410-SEIKYU-ROSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>
      *****************************************************************
       410-SEIKYU-JIBAI-SEC            SECTION.
      *
      *    Æþ³°¶èÊ¬
           MOVE    RECE10-NYUGAIKBN    TO  TA07TMP-NYUGAI
      *
      *    ´µ¼ÔÈÖ¹æ(KEY)
           PERFORM  920-PTNUM-READ-SEC
           MOVE    RECE10-PTNUM        TO  TA07TMP-PTNUM
      *
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    RECE10-HKNCOMBI(1)  TO  TA07TMP-HKNCOMBI
      *
      *    ¾ðÊóÆÉ¹þ
           PERFORM  700-TMP-READ-SEC
      *
      *    ÊÝ¸±ÈÖ¹æ
           MOVE    RECE10-HKNNUM       TO  TA07TMP-HKNNUM
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  RECE10-PTNUM(11:1)  =  SPACE
             MOVE    RECE10-PTNUM        TO  TA07TMP-PTNUM10
           ELSE
             MOVE    RECE10-PTNUM        TO  TA07TMP-PTNUM20
           END-IF
      *
      *    »áÌ¾
           INITIALIZE                 PTINF-REC
           MOVE    RECE10-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID        TO  PTINF-PTID
           PERFORM  920-PTINF-READ-SEC
           IF  PTINF-NAME(25:2)  =  SPACE
             MOVE    PTINF-NAME       TO  TA07TMP-NAME24
           ELSE
             MOVE    PTINF-NAME       TO  TA07TMP-NAME48
           END-IF
      *
      *    ÊÝ¸±¶èÊ¬
           EVALUATE  RECE10-HKNKBN
           WHEN  "1"
             MOVE    "Ã»´üµëÉÕ"       TO  TA07TMP-HKNKBN
           WHEN  "2"
             MOVE    "½ýÉÂÇ¯¶â"       TO  TA07TMP-HKNKBN
           WHEN  "3"
             MOVE    "¥¢¥Õ¥¿¡¼¥±¥¢"   TO  TA07TMP-HKNKBN
           WHEN  "4"
             MOVE    "¸øÌ³ºÒ³²"       TO  TA07TMP-HKNKBN
           WHEN  OTHER
             MOVE    "¼«ÇåÀÕÊÝ¸±"     TO  TA07TMP-HKNKBN
           END-EVALUATE
      *
      *    ÀÁµá¶èÊ¬
           EVALUATE  PTRSI-JIBAISEIKBN
           WHEN  "1"
             MOVE    "´µ¼ÔÀÁµá¤¹¤ë¡Ê£±£°£°¡ó¡Ë"  TO  TA07TMP-SKYKBN
           WHEN  "2"
             MOVE    "´µ¼ÔÀÁµá¤·¤Ê¤¤¡Ê£°¡ó¡Ë"    TO  TA07TMP-SKYKBN
           END-EVALUATE
      *
      *    Å¾µ¢»öÍ³
           EVALUATE  PTRSI-TENKI
           WHEN  "1"
             MOVE    "¼£Ìþ"       TO  TA07TMP-TENKI
           WHEN  "3"
             MOVE    "·ÑÂ³"       TO  TA07TMP-TENKI
           WHEN  "5"
             MOVE    "Å¾°å"       TO  TA07TMP-TENKI
           WHEN  "7"
             MOVE    "Ãæ»ß"       TO  TA07TMP-TENKI
           WHEN  "9"
             MOVE    "»àË´"       TO  TA07TMP-TENKI
           END-EVALUATE
      *
      *    ½ýÉÂÇ¯·îÆü
           MOVE    PTRSI-SHOBYOYMD   TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-SYMD
      *
      *    ÎÅÍÜ³«»ÏÆü
           MOVE    PTRSI-RYOSTYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDST
      *
      *    ÎÅÍÜ½ªÎ»Æü
           IF  PTRSI-RYOEDYMD  NOT =  "99999999"
             MOVE    PTRSI-RYOEDYMD    TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    LNK-DAY2-EDTYMD1  TO  TA07TMP-RYMDED
           END-IF
      *
      *    ÀÁµáÅÀ¿ô
           ADD     RECE10-TOTALTEN(1)  TO  TA07TMP-SKYTENSU
      *
      *    ÀÁµá¶â³Û
           ADD     RECE10-FTNMONEY(1)  TO  TA07TMP-SKYMONEY
      *
      *    ¥Ç¡¼¥¿½ñ¹þ¤ß
           IF  FLG-INVALID  =  1
             WRITE  TA07TMP
             ADD    1  TO  WRK-DATA
             DISPLAY  "WRITE"
           ELSE
             REWRITE  TA07TMP
             DISPLAY  "REWRITE"
           END-IF
      *
           .
       410-SEIKYU-JIBAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °õºþÊÔ½¸½èÍý
      *****************************************************************
       400-PRINT-HEN-ROSAI-SEC            SECTION.
      *
           OPEN    INPUT       TA07-FILE
           MOVE    ZERO        TO  FLG-PRINT
      *
           INITIALIZE          TA07TMP
           INITIALIZE          TA07R
                               FLG-HEADER
      *
           MOVE    ZERO        TO  CNT-RENNUM
           MOVE    ZERO        TO  IDX
           MOVE    1           TO  IDXP
      *
      *    °ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ
           PERFORM  710-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
             IF  TA07TMP-HKNKBN-NUM  =  "1"
      *
               MOVE    TA07TMP-HKNKBN-NUM  TO  WRK-HKNKBN-NUM
               MOVE    TA07TMP-NYUGAI      TO  WRK-NYUGAIKBN
      *
               ADD    1  TO  IDX
               IF  FLG-HEADER  =  ZERO
                 PERFORM  800-CSV-HEAD-SEC
                 MOVE    1  TO  FLG-HEADER
               END-IF
               PERFORM  410-PRINT-ROSAI-SEC
      *
      *        °ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ
               PERFORM  710-TMP-READ-SEC
      *
               IF    ( IDX                     =  30 )
                 OR  ( TA07TMP-NYUGAI      NOT =  WRK-NYUGAIKBN )
                 OR  ( TA07TMP-HKNKBN-NUM  NOT =  WRK-HKNKBN-NUM )
                 OR  ( FLG-PRINT               =  1 )
      *
                 PERFORM  400-HEAD-ROSAI-SEC
                 PERFORM  500-PRINT-ROSAI-SEC
                 PERFORM  230-TOUKEICSV-SEC
                 INITIALIZE  IDX  CNT-RENNUM
      *
                 IF  TA07TMP-NYUGAI      NOT =  WRK-NYUGAIKBN
                   INITIALIZE  FLG-HEADER
                 END-IF
      *
                 IF    ( TA07TMP-HKNKBN-NUM  NOT =  WRK-HKNKBN-NUM )
                   OR  ( FLG-PRINT               =  1 )
                   MOVE    1  TO  FLG-PRINT
                 END-IF
      *
               END-IF
      *
             ELSE
      *
      *        °ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ
               PERFORM  710-TMP-READ-SEC
      *
             END-IF
      *
           END-PERFORM
      *
           CLOSE  TA07-FILE
      *
           .
       400-PRINT-HEN-ROSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °õºþÊÔ½¸½èÍý
      *****************************************************************
       400-PRINT-HEN-JIBAI-SEC            SECTION.
      *
           OPEN    INPUT       TA07-FILE
           MOVE    ZERO        TO  FLG-PRINT
      *
           INITIALIZE          TA07TMP
           INITIALIZE          TA07J
                               FLG-HEADER
      *
           MOVE    ZERO        TO  CNT-RENNUM
           MOVE    ZERO        TO  IDX
           MOVE    1           TO  IDXP
      *
      *    °ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ
           PERFORM  710-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
             IF  TA07TMP-HKNKBN-NUM  =  "2"  OR  "3"
      *
               MOVE    TA07TMP-HKNKBN-NUM  TO  WRK-HKNKBN-NUM
               MOVE    TA07TMP-NYUGAI      TO  WRK-NYUGAIKBN
      *
               ADD    1  TO  IDX
      *
               IF  FLG-HEADER  =  ZERO
                 PERFORM  800-CSV-HEAD-SEC
                 MOVE    1  TO  FLG-HEADER
               END-IF
      *
               PERFORM  410-PRINT-JIBAI-SEC
      *
      *        °ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ
               PERFORM  710-TMP-READ-SEC
      *
               IF    ( IDX                     =  30 )
                 OR  ( TA07TMP-NYUGAI      NOT = WRK-NYUGAIKBN )
                 OR  ( TA07TMP-HKNKBN-NUM  NOT = WRK-HKNKBN-NUM )
                 OR  ( FLG-PRINT               =  1 )
      *
                 PERFORM  400-HEAD-JIBAI-SEC
                 PERFORM  500-PRINT-JIBAI-SEC
                 PERFORM  230-TOUKEICSV-SEC
                 INITIALIZE  IDX  CNT-RENNUM
      *
                 IF    TA07TMP-HKNKBN-NUM  NOT = WRK-HKNKBN-NUM
                   OR  TA07TMP-NYUGAI      NOT =  WRK-NYUGAIKBN
                   INITIALIZE  FLG-HEADER
                 END-IF
      *
               END-IF
      *
             ELSE
      *
      *        °ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ
               PERFORM  710-TMP-READ-SEC
      *
             END-IF
      *
           END-PERFORM
      *
           CLOSE  TA07-FILE
      *
           .
       400-PRINT-HEN-JIBAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °õºþÊÔ½¸½èÍý
      *****************************************************************
       410-PRINT-ROSAI-SEC            SECTION.
      *
      *    Ï¢ÈÖ
           ADD     1                      TO  CNT-RENNUM
           MOVE    CNT-RENNUM             TO  WRK-Z3
           MOVE    WRK-Z3                 TO  TA07R-RENNUM(IDX)
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  TA07TMP-PTNUM10  NOT =  ZERO
             MOVE    TA07TMP-PTNUM10        TO  TA07R-PTNUM10(IDX)
           ELSE
             MOVE    TA07TMP-PTNUM20        TO  TA07R-PTNUM20(IDX)
           END-IF
      *
      *    »áÌ¾
           MOVE    TA07TMP-NAME24         TO  TA07R-NAME24(IDX)
           MOVE    TA07TMP-NAME48         TO  TA07R-NAME48(IDX)
      *
      *    ÊÝ¸±¶èÊ¬
           MOVE    TA07TMP-HKNKBN         TO  TA07R-HKNKBN(IDX)
      *
      *    Ï«Æ¯ÊÝ¸±ÈÖ¹æ
           MOVE    TA07TMP-RHNUM          TO  WRK-Z15
           MOVE    WRK-Z15                TO  TA07R-RHNUM(IDX)
      *
      *    ºÒ³²¶èÊ¬
           MOVE    TA07TMP-SGIKBN         TO  TA07R-SGIKBN(IDX)
      *
      *    ¿··ÑºÆÊÌ
           MOVE    TA07TMP-SKSBETU        TO  TA07R-SKSBETU(IDX)
      *
      *    Å¾µ¢»öÍ³
           MOVE    TA07TMP-TENKI          TO  TA07R-TENKI(IDX)
      *
      *    ½ýÉÂÇ¯·îÆü
           MOVE    TA07TMP-SYMD           TO  TA07R-SYMD(IDX)
      *
      *    ÎÅÍÜ³«»ÏÆü
           MOVE    TA07TMP-RYMDST         TO  TA07R-RYMDST(IDX)
      *
      *    ÎÅÍÜ½ªÎ»Æü
           MOVE    TA07TMP-RYMDED         TO  TA07R-RYMDED(IDX)
      *
      *    ÀÁµáÅÀ¿ô
           MOVE    TA07TMP-SKYTENSU       TO  WRK-Z9
           MOVE    WRK-Z9                 TO  TA07R-SKYTENSU(IDX)
      *
      *    ÀÁµá¶â³Û
           MOVE    TA07TMP-SKYMONEY       TO  WRK-Z11
           MOVE    WRK-Z11                TO  TA07R-SKYMONEY(IDX)
      *
           PERFORM  400-CSV-DATA-ROSAI-SEC
      *
           .
       410-PRINT-ROSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °õºþÊÔ½¸½èÍý
      *****************************************************************
       410-PRINT-JIBAI-SEC            SECTION.
      *
      *    Ï¢ÈÖ
           ADD     1                      TO  CNT-RENNUM
           MOVE    CNT-RENNUM             TO  WRK-Z3
           MOVE    WRK-Z3                 TO  TA07J-RENNUM(IDX)
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  TA07TMP-PTNUM10  NOT =  ZERO
           MOVE    TA07TMP-PTNUM10        TO  TA07J-PTNUM10(IDX)
           ELSE
           MOVE    TA07TMP-PTNUM20        TO  TA07J-PTNUM20(IDX)
           END-IF
      *
      *    »áÌ¾
           MOVE    TA07TMP-NAME24         TO  TA07J-NAME24(IDX)
           MOVE    TA07TMP-NAME48         TO  TA07J-NAME48(IDX)
      *
      *    ÊÝ¸±¶èÊ¬
           MOVE    TA07TMP-HKNKBN         TO  TA07J-HKNKBN(IDX)
      *
      *    ÀÁµá¶èÊ¬
           MOVE    TA07TMP-SKYKBN         TO  TA07J-SKYKBN(IDX)
      *
      *    Å¾µ¢»öÍ³
           MOVE    TA07TMP-TENKI          TO  TA07J-TENKI(IDX)
      *
      *    ½ýÉÂÇ¯·îÆü
           MOVE    TA07TMP-SYMD           TO  TA07J-SYMD(IDX)
      *
      *    ÎÅÍÜ³«»ÏÆü
           MOVE    TA07TMP-RYMDST         TO  TA07J-RYMDST(IDX)
      *
      *    ÎÅÍÜ½ªÎ»Æü
           MOVE    TA07TMP-RYMDED         TO  TA07J-RYMDED(IDX)
      *
      *    ÀÁµáÅÀ¿ô
           MOVE    TA07TMP-SKYTENSU       TO  WRK-Z9
           MOVE    WRK-Z9                 TO  TA07J-SKYTENSU(IDX)
      *
      *    ÀÁµá¶â³Û
           MOVE    TA07TMP-SKYMONEY       TO  WRK-Z11
           MOVE    WRK-Z11                TO  TA07J-SKYMONEY(IDX)
      *
           PERFORM  400-CSV-DATA-JIBAI-SEC
      *
           .
       410-PRINT-JIBAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¥Ø¥Ã¥À¡¼Éô>½èÍý
      *****************************************************************
       400-HEAD-ROSAI-SEC            SECTION.
      *
      *    ¥¿¥¤¥È¥ë
           MOVE    "Ï«ºÒ½¸·×É½"           TO  TA07R-TITLE
      *
      *    ÂÐ¾Ý³«»ÏÇ¯·îÊÔ½¸
           MOVE    WRK-PARA-PRTYM         TO  WRK-SYMD(1:6)
           MOVE    "01"                   TO  WRK-SYMD(7:2)
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)      TO  TA07R-SRYYM
      *
      *    ºîÀ®Ç¯·îÆüÊÔ½¸
           MOVE    LNK-PRTKANRI-SKYYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  TA07R-CREATEYMD
      *
      *    ÊÇ¿ôÊÔ½¸
           ADD     1                      TO  CNT-PAGE
           MOVE    CNT-PAGE               TO  WRK-Z3
           MOVE    WRK-Z3                 TO  TA07R-PAGE
      *
      *    Æþ±¡¡¿³°Íè
           IF  WRK-NYUGAIKBN  =  "1"
             MOVE    "Æþ±¡"               TO  TA07R-NYUGAI
           ELSE
             MOVE    "³°Íè"               TO  TA07R-NYUGAI
           END-IF
      *
           .
       400-HEAD-ROSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¥Ø¥Ã¥À¡¼Éô>½èÍý
      *****************************************************************
       400-HEAD-JIBAI-SEC            SECTION.
      *
      *    ¥¿¥¤¥È¥ë
           EVALUATE  WRK-HKNKBN-NUM
           WHEN  "1"
             MOVE    "Ï«ºÒ½¸·×É½"         TO  TA07J-TITLE
           WHEN  "2"
             MOVE    "¼«ÇåÀÕ½¸·×É½"       TO  TA07J-TITLE
           WHEN  "3"
             MOVE    "¸øÌ³ºÒ³²½¸·×É½"     TO  TA07J-TITLE
           END-EVALUATE
      *
      *    ÂÐ¾Ý³«»ÏÇ¯·îÊÔ½¸
           MOVE    WRK-PARA-PRTYM         TO  WRK-SYMD(1:6)
           MOVE    "01"                   TO  WRK-SYMD(7:2)
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)      TO  TA07J-SRYYM
      *
      *    ºîÀ®Ç¯·îÆüÊÔ½¸
           MOVE    LNK-PRTKANRI-SKYYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  TA07J-CREATEYMD
      *
      *    ÊÇ¿ôÊÔ½¸
           ADD     1                      TO  CNT-PAGE
           MOVE    CNT-PAGE               TO  WRK-Z3
           MOVE    WRK-Z3                 TO  TA07J-PAGE
      *
      *    Æþ±¡¡¿³°Íè
           IF  WRK-NYUGAIKBN  =  "1"
             MOVE    "Æþ±¡"               TO  TA07J-NYUGAI
           ELSE
             MOVE    "³°Íè"               TO  TA07J-NYUGAI
           END-IF
      *
           .
       400-HEAD-JIBAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼°õºþ½èÍý
      *****************************************************************
       500-PRINT-ROSAI-SEC            SECTION.
      *
      *    °õºþ½èÍý
           INITIALIZE                         ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           MOVE    "A00000TA07R.red"          TO  SPRT-PRTID
           MOVE    "Ï«ºÒ½¸·×É½"               TO  SPRT-TITLE
           MOVE    TA07R                      TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
                                              SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"  TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE  TA07R
      *
           .
       500-PRINT-ROSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼°õºþ½èÍý
      *****************************************************************
       500-PRINT-JIBAI-SEC            SECTION.
      *
      *    °õºþ½èÍý
           INITIALIZE                         ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           MOVE    "A00000TA07J.red"          TO  SPRT-PRTID
           MOVE    "¼«ÇåÀÕ¸øÌ³ºÒ³²½¸·×É½"     TO  SPRT-TITLE
           MOVE    TA07J                      TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
                                              SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"  TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE  TA07J
      *
           .
       500-PRINT-JIBAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV¥Ø¥Ã¥ÀÊÔ½¸½èÍý
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
      *    Ï«ºÒ¡¦¼«ÇåÀÕ
           EVALUATE  WRK-HKNKBN-NUM
           WHEN  "1"
             MOVE    "ÊÝ¸±¶èÊ¬¡§Ï«ºÒ"       TO  WRK-ITEM
           WHEN  "2"
             MOVE    "ÊÝ¸±¶èÊ¬¡§¼«ÇåÀÕ"     TO  WRK-ITEM
           WHEN  "3"
             MOVE    "ÊÝ¸±¶èÊ¬¡§¸øÌ³ºÒ³²"   TO  WRK-ITEM
           END-EVALUATE
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Æþ±¡¡¿³°Íè
           IF  WRK-NYUGAIKBN  =  "1"
             MOVE    "Æþ³°¶èÊ¬¡§Æþ±¡"       TO  WRK-ITEM
           ELSE
             MOVE    "Æþ³°¶èÊ¬¡§³°Íè"       TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÂÐ¾Ý³«»ÏÇ¯·îÊÔ½¸
           MOVE    "³«»ÏÇ¯·î¡§"             TO  WRK-ITEM(1:10)
           MOVE    WRK-PARA-PRTYM           TO  WRK-SYMD(1:6)
           MOVE    "01"                     TO  WRK-SYMD(7:2)
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)        TO  WRK-ITEM(11:16)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ºîÀ®Ç¯·îÆüÊÔ½¸
           MOVE    "ºîÀ®Ç¯·îÆü¡§"           TO  WRK-ITEM(1:12)
           MOVE    LNK-PRTKANRI-SKYYMD      TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG              TO  WRK-ITEM(13:22)
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    ¹àÌÜÌ¾¾Î¥»¥Ã¥È
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >  CSV-HEAD-MAX
      *
             MOVE    CSV-HEAD-ITEM(IDX2)      TO  WRK-ITEM
      *
             IF  WRK-HKNKBN-NUM  =  "1"
               EVALUATE  IDX2
               WHEN  5
               WHEN  6
               WHEN  7
                 PERFORM  800-CSVDATA-EDIT-SEC
               WHEN  8
                 CONTINUE
               WHEN  CSV-HEAD-MAX
                 PERFORM  810-CSVDATA-EDIT-SEC
               WHEN  OTHER
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-EVALUATE
             ELSE
               EVALUATE  IDX2
               WHEN  5
               WHEN  6
               WHEN  7
                 CONTINUE
               WHEN  8
                 PERFORM  800-CSVDATA-EDIT-SEC
               WHEN  CSV-HEAD-MAX
                 PERFORM  810-CSVDATA-EDIT-SEC
               WHEN  OTHER
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-EVALUATE
             END-IF
      *
           END-PERFORM
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVÊÔ½¸½èÍý<Ï«ºÒ>
      *****************************************************************
       400-CSV-DATA-ROSAI-SEC     SECTION.
      *
      *    Ï¢ÈÖ
           MOVE    TA07R-RENNUM(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  TA07R-PTNUM10(IDX)  NOT =  ZERO
             MOVE    TA07R-PTNUM10(IDX)     TO  WRK-ITEM
           ELSE
             MOVE    TA07R-PTNUM20(IDX)     TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    »áÌ¾
           IF  TA07R-NAME24(IDX) NOT =  SPACE
             MOVE    TA07R-NAME24(IDX)      TO  WRK-ITEM
           ELSE
             MOVE    TA07R-NAME48(IDX)      TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÊÝ¸±¶èÊ¬
           MOVE    TA07R-HKNKBN(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Ï«Æ¯ÊÝ¸±ÈÖ¹æ
           MOVE    TA07R-RHNUM(IDX)       TO  WRK-Z15
           MOVE    WRK-Z15                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ºÒ³²¶èÊ¬
           MOVE    TA07R-SGIKBN(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ¿··ÑºÆÊÌ
           MOVE    TA07R-SKSBETU(IDX)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Å¾µ¢»öÍ³
           MOVE    TA07R-TENKI(IDX)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ½ýÉÂÇ¯·îÆü
           MOVE    TA07R-SYMD(IDX)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÎÅÍÜ³«»ÏÆü
           MOVE    TA07R-RYMDST(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÎÅÍÜ½ªÎ»Æü
           MOVE    TA07R-RYMDED(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÀÁµáÅÀ¿ô
           MOVE    TA07TMP-SKYTENSU       TO  WRK-Z92
           MOVE    WRK-Z92                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÀÁµá¶â³Û
           MOVE    TA07TMP-SKYMONEY       TO  WRK-Z92
           MOVE    WRK-Z92                TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
      *
       400-CSV-DATA-ROSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVÊÔ½¸½èÍý<¼«ÇåÀÕ>
      *****************************************************************
       400-CSV-DATA-JIBAI-SEC     SECTION.
      *
      *    Ï¢ÈÖ
           MOVE    TA07J-RENNUM(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ´µ¼ÔÈÖ¹æ
           IF  TA07J-PTNUM10(IDX)  NOT =  ZERO
             MOVE    TA07J-PTNUM10(IDX)     TO  WRK-ITEM
           ELSE
             MOVE    TA07J-PTNUM20(IDX)     TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    »áÌ¾
           IF  TA07J-NAME24(IDX) NOT =  SPACE
             MOVE    TA07J-NAME24(IDX)      TO  WRK-ITEM
           ELSE
             MOVE    TA07J-NAME48(IDX)      TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÊÝ¸±¶èÊ¬
           MOVE    TA07J-HKNKBN(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÀÁµá¶èÊ¬
           MOVE    TA07J-SKYKBN(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    Å¾µ¢»öÍ³
           MOVE    TA07J-TENKI(IDX)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ½ýÉÂÇ¯·îÆü
           MOVE    TA07J-SYMD(IDX)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÎÅÍÜ³«»ÏÆü
           MOVE    TA07J-RYMDST(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÎÅÍÜ½ªÎ»Æü
           MOVE    TA07J-RYMDED(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÀÁµáÅÀ¿ô
           MOVE    TA07TMP-SKYTENSU       TO  WRK-Z92
           MOVE    WRK-Z92                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ÀÁµá¶â³Û
           MOVE    TA07TMP-SKYMONEY       TO  WRK-Z92
           MOVE    WRK-Z92                TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
      *
       400-CSV-DATA-JIBAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV¥Ç¡¼¥¿ÊÔ½¸½èÍý
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       800-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSV¥Ç¡¼¥¿ÊÔ½¸½èÍý
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
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
           STRING    ","       DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
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
           STRING    X"0D"     DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
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
           INITIALIZE                         ORCSTOUKEICSVAREA
           MOVE    "INS"                      TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM        TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID       TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM         TO  STOUKEICSV-SRYYM
           MOVE    ZERO                       TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"                TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM                DELIMITED  BY  SIZE
                   "rosaijibai_syukeihyo"     DELIMITED  BY  SIZE
                   WRK-PARA-PRTYM             DELIMITED  BY  SIZE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "Ï«ºÒ¼«ÇåÀÕ¸øÌ³ºÒ³²½¸·×É½" 
                                              TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"            USING
                                              ORCSTOUKEICSVAREA
                                              SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "Åý·×£Ã£Ó£Ö£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                         WRK-CSVDATA
           MOVE    1                          TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       800-ERR-HENSYU-SEC            SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT             RECEERR-FILE
      *
             MOVE    WRK-RECEERR        TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
             MOVE    "JBE"              TO  SJOBKANRI-MODE
             INITIALIZE                 JOBKANRI-REC
             MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR        TO  JOB-YOBI
             MOVE    "9999"             TO  JOB-ERRCD
             CALL    "ORCSJOB"          USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           MOVE    1           TO  FLG-END
      *
           .
       800-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    À¾ÎñÆüËÜ¸ìÊÑ´¹½èÍý
      *****************************************************************
       700-SEIWA-HEN-SEC            SECTION.
      *
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY2-AREA
           MOVE    "21"                 TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"            USING
                                        STS-AREA-DAY
                                        LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3     TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG  REPLACING  ALL "  "  BY  "¡¡"
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       600-END-SEC            SECTION.
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"                TO  SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE             TO  JOB-UPDCNT
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
      *
           .
       600-END-EXT.
           EXIT.
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"        TO  MCP-FUNC
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    ZERO           TO  FLG-SYSKANRI
             ELSE
               MOVE    1              TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-SYSKANRI-SELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM  910-SYSKANRI-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-SYSKANRI-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-SYSKANRI-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷(SELECT)
      *****************************************************************
       900-DBSELECT-SEC             SECTION.
      *
           INITIALIZE                  SYUNOU-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    IDX-NG              TO  SYU-NYUGAIKBN
           MOVE    WRK-PARA-PRTYM      TO  SYU-SRYYMD(1:6)
           MOVE    "01"                TO  SYU-SRYYMD(7:2)
           MOVE    WRK-PARA-PRTYM      TO  SYU-UPSRYYMD(1:6)
           MOVE    "31"                TO  SYU-UPSRYYMD(7:2)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "st_key1"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             PERFORM  900-DBFETCH-SEC
           ELSE
             MOVE    1                   TO  FLG-SYUNOU
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷(FETCH)
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "st_key1"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    ZERO                TO  FLG-SYUNOU
             MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
             MOVE    1                   TO  FLG-SYUNOU
           END-IF
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷(CLOSE)
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "st_key1"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷(SELECT)
      *****************************************************************
       910-DBSELECT-SEC             SECTION.
      *
           INITIALIZE                  RECE10-REC
      *
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    IDX-NG              TO  RECE10-NYUGAIKBN
           MOVE    WRK-PARA-PRTYM      TO  RECE10-SRYYM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             PERFORM  910-DBFETCH-SEC
           ELSE
             MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷(FETCH)
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    ZERO                TO  FLG-SEIKYU
             MOVE    MCPDATA-REC         TO  RECE10-REC
      *
             INITIALIZE                PTINF-REC
             MOVE    RECE10-HOSPNUM    TO  PTINF-HOSPNUM
             MOVE    RECE10-PTID       TO  PTINF-PTID
             PERFORM  910-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  910-DBFETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¾ðÊó¸¡º÷(CLOSE)
      *****************************************************************
       910-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-PTNUM-READ-SEC               SECTION.
      *
           INITIALIZE                   PTNUM-REC
           MOVE    SYU-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    SYU-PTID             TO  PTNUM-PTID
           MOVE    PTNUM-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnum"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_ptnum"        TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO  PTNUM-REC
               MOVE    ZERO             TO  FLG-PTNUM
             ELSE
               MOVE    1                TO  FLG-PTNUM
             END-IF
           ELSE
             MOVE    1                  TO  FLG-PTNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnum"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¾ðÊó¸¡º÷
      *****************************************************************
       910-PTINF-READ-SEC            SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
             ELSE
               MOVE    1               TO  FLG-PTINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       920-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptinf"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTINF-REC
               MOVE    ZERO              TO   FLG-PTINF
             ELSE
               MOVE    1                 TO   FLG-PTINF
               INITIALIZE                PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_hkncombi"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
             ELSE
               MOVE    1               TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¾ðÊóÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-PTRSIINF-READ-SEC               SECTION.
      *
           MOVE    PTRSIINF-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO  PTRSIINF-REC
               MOVE    ZERO             TO  FLG-PTRSIINF
             ELSE
               MOVE    1                TO  FLG-PTRSIINF
             END-IF
           ELSE
             MOVE    1                  TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¾ðÊó¥»¥Ã¥È
      *****************************************************************
       920-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE    WRK-PARA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM           TO  HKN-HKNNUM
           MOVE    "00"                 TO  HKN-PAYKBN
           MOVE    ALL "9"              TO  HKN-TEKSTYMD
           MOVE    ZERO                 TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_hknnum"       TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  HKNNUM-REC
               MOVE    ZERO             TO  FLG-HKNNUM
             ELSE
               INITIALIZE               HKNNUM-REC
               MOVE    1                TO  FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1                  TO  FLG-HKNNUM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ½èÍý
      *****************************************************************
       920-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¼õ¿ÇÂÐ¾Ý´µ¼Ô°ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ(KEY READ)
      *****************************************************************
       700-TMP-READ-SEC              SECTION.
      *
           INITIALIZE                FLG-INVALID
           READ   TA07-FILE
               INVALID    KEY
                 MOVE    1           TO  FLG-INVALID
               NOT  INVALID  KEY
                 MOVE    ZERO        TO  FLG-INVALID
           END-READ
           .
       700-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¼õ¿ÇÂÐ¾Ý´µ¼Ô°ì»þ¥Õ¥¡¥¤¥ëÆÉ¹þ(NEXT READ)
      *****************************************************************
       710-TMP-READ-SEC              SECTION.
      *
           READ    TA07-FILE  NEXT
               AT  END
                 MOVE    1           TO  FLG-PRINT
               NOT  AT END
                 MOVE    ZERO        TO  FLG-PRINT
           END-READ
           .
       710-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC            SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
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
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
