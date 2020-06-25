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
       PROGRAM-ID.             ORHCML12.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ÅÀ¿ô¥Þ¥¹¥¿
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÅÀ¿ô¥Þ¥¹¥¿°ìÍ÷É½¡¦ÌôºÞÊ¬Îà¶èÊ¬¤¢¤ê¡ÊHCML01¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/08/22    NACL-¿¹ÏÆ     ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.01.01     NACL-¿¹ÏÆ    04/03/16  ÅÀ¿ô¤Î·å½¤Àµ
      *  01.01.02    NACL-¿¹ÏÆ    06/07/27  ¥Õ¥é¥°ÄÉ²Ã¡¢MONFUNCÂÐ±þ
      *  03.05.01    NACL-¿¹ÏÆ    07/05/29  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.03.01    NACL-¿¹ÏÆ    08/06/13  £Ã£Ó£Ö¥Õ¥¡¥¤¥ë½ÐÎÏÂÐ±þ
      *  04.08.01    NACL-¿¹ÏÆ    14/07/31  °ì»þ¥Õ¥¡¥¤¥ë¥Ç¥£¥ì¥¯¥È¥êÀßÄê
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  TENSUERR-FILE   ASSIGN  TENSUERR
                                   FILE    STATUS  IS  STS-TENSUERR.
      *    £Ã£Ó£Ö¥Õ¥¡¥¤¥ë
           SELECT  TENSUCSV-FILE   ASSIGN  TENSUCSV
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TENSUCSV.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  TENSUERR-FILE.
       01  TENSUERR-REC            PIC X(200).
      *    CSV¥Õ¥¡¥¤¥ë
       FD  TENSUCSV-FILE.
       01  TENSUCSV-REC.
           03  CSV-HOSPNUM         PIC 9(2).
           03  CSV-FILLER1         PIC X(1).
           03  CSV-SRYCD           PIC X(9).
           03  CSV-FILLER2         PIC X(1).
           03  CSV-YUKOSTYMD       PIC X(8).
           03  CSV-FILLER3         PIC X(1).
           03  CSV-YUKOEDYMD       PIC X(8).
           03  CSV-FILLER4         PIC X(1).
           03  CSV-NAME            PIC X(100).
           03  CSV-FILLER5         PIC X(1).
           03  CSV-TEN             PIC 9(09).99.
           03  CSV-FILLER6         PIC X(1).
           03  CSV-TANINAME        PIC X(24).
           03  CSV-FILLER7         PIC X(1).
           03  CSV-CSYYOURYO       PIC X(5).
           03  CSV-FILLER8         PIC X(1).
           03  CSV-MADOKUKBN       PIC X(8).
           03  CSV-FILLER9         PIC X(1).
           03  CSV-SINKEIHAKAIKBN  PIC X(8).
           03  CSV-FILLER10        PIC X(1).
           03  CSV-SEIBUTUGAKUKBN  PIC X(8).
           03  CSV-FILLER11        PIC X(1).
           03  CSV-ZOEIZAIKBN      PIC X(8).
           03  CSV-FILLER12        PIC X(1).
           03  CSV-YKZKBN          PIC X(8).
           03  CSV-FILLER13        PIC X(1).
           03  CSV-ZAIGATAKBN      PIC X(8).
           03  CSV-FILLER14        PIC X(1).
           03  CSV-KEIKASOCHIYMD   PIC X(8).
           03  CSV-FILLER15        PIC X(1).
           03  CSV-YAKKAKJNCD      PIC X(12).
      *
       WORKING-STORAGE             SECTION.
      *    ¥·¥§¥ëÍÑÎÎ°è
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *----(04.08.01)--ADD-START---
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TENSUERR//.
      *----(04.08.01)--ADD-END----- 
      *     
           COPY    "HCML01.INC".
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-TENSUERR        PIC X(02).
           03  STS-TENSUCSV        PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    ½¸·×ÎÎ°è
       01  SUM-AREA.
           03  SUM-KENSU           PIC 9(07).
      *
      *    ¥Ñ¥é¥á¥¿ÎÎ°è
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           COPY    "CPHCMLSHELL.INC"
                                   REPLACING  //SHELL-//
                                   BY         //WRK-PARA-//.
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-CSVKBN     PIC X(1).
           03  WRK-PARA-FILE       PIC X(1).
           03  WRK-PARA-CSVDIR     PIC X(100).
           03  WRK-PARA-CSVFILE    PIC X(100).
      *----(04.08.01)--UPD-START---
      *     03  TENSUERR            PIC X(100).
           03  TENSUCSV            PIC X(1024).
      *----(04.08.01)--UPD-END-----
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PATH            PIC X(64).
           03  WRK-TENSUERR        PIC X(200).
      *
           03  WRK-LNKYMD          PIC X(10).
           03  WRK-MSTYMDH         PIC X(10).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-NAME            PIC X(100).
      *
           03  WRK-PAGE            PIC ZZ,ZZ9.
           03  WRK-TEN             PIC ZZZ,ZZZ,ZZ9.99.
           03  WRK-CSYYOURYO       PIC ZZ,ZZ9.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    °åÎÅµ¡´ØÌ¾
           COPY  "CPSK1001.INC".
      *
      *    ½ÐÎÏÀè¥×¥ê¥ó¥¿Ì¾³ä¤êÅö¤Æ¾ðÊó
           COPY  "CPSK1031.INC".
      *
      *    ¹â³ÛÀÁµá»ñÎÁÍÑÌôºÞÊ¬Îà¾ðÊó
           COPY  "CPSK1036.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿
           COPY    "CPTENSU.INC".
      *
      *    ÅÀ¿ôÉÕ²Ã¾ðÊó¥Þ¥¹¥¿
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
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
           COPY    "CPORCSLNK.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *    LIKE»ÈÍÑKEYÂÐ±þ
           COPY    "CPORCSADDSIGN.INC".
      *
      *    ¥¯¥é¥¤¥¢¥ó¥ÈÊÝÂ¸£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSFILESV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    °ì»þ¥Õ¥¡¥¤¥ëÌ¾ÊÔ½¸
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(1000).
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
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
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
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  TENSUERR
           INITIALIZE                  TENSUCSV
      *----(04.08.01)--ADD-END-----
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
                                               WRK-PARA-AREA
                                               WRK-PARA-HOSPNUM
                                               TENSUERR
                                               WRK-PARA-CSVKBN
                                               WRK-PARA-FILE
                                               WRK-PARA-CSVDIR
                                               WRK-PARA-CSVFILE
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           MOVE    "ORHCML12"      TO  JOB-PGID
           MOVE    "ÅÀ¿ô¥Þ¥¹¥¿°ìÍ÷É½¡¦ÌôºÞÊ¬Îà¶èÊ¬"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           MOVE    WRK-LNKYMD      TO  WRK-MSTYMDH
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TENSUERR-BASENAME   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  TENSUERR
      *----(04.08.01)--ADD-END-----
      *
      *    ¥Ñ¥é¥á¥¿ÊÔ½¸½èÍý
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¥¿ÊÔ½¸½èÍý
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    °åÎÅµ¡´ØÌ¾¾ÎÊÔ½¸
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-MSTYMD TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               IF          SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1001-HOSPNAME
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    "°åÎÅµ¡´ØÌ¾¾Î¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                       TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      WRK-PARA-CSVKBN =   "1"
      *        £Ã£Ó£ÖÊÔ½¸½èÍý
               PERFORM 300-CSV-SEC 
           ELSE
      *        Ä¢É¼ÊÔ½¸½èÍý
               PERFORM 300-PRINT-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý¡Ê£Ã£Ó£Ö¡Ë
      *****************************************************************
       300-CSV-SEC                 SECTION.
      *
      *----(04.08.01)--UPD-START---
      *     STRING  "/tmp/"             DELIMITED SIZE
           STRING  SGETTEMP-TEMPDIR    DELIMITED SPACE
                   WRK-PARA-CSVFILE    DELIMITED SPACE
                   ".wk"               DELIMITED SIZE
                               INTO    TENSUCSV
           END-STRING
      *----(04.08.01)--UPD-END-----
      *
      *    £Ã£Ó£Ö¥Õ¥¡¥¤¥ë¤ò³«¤¯
           OPEN    OUTPUT              TENSUCSV-FILE
      *
      *    Ä¢É¼°õºþ³«»Ï½èÍý
           PERFORM 900-DBSELECT-SEC
           PERFORM UNTIL   FLG-TENSU   =   1
      *        Ä¢É¼ÊÔ½¸<CSV>½èÍý
               PERFORM 330-CSV-HEN-SEC
           END-PERFORM
      *    Ä¢É¼°õºþ½ªÎ»½èÍý
           PERFORM 900-DBCLOSE-SEC
      *
      *    £Ã£Ó£Ö¥Õ¥¡¥¤¥ë¤òÊÄ¤¸¤ë
           CLOSE                       TENSUCSV-FILE
      *
      *    ¥¯¥é¥¤¥¢¥ó¥ÈÊÝÂ¸¤Î¾ì¹ç
           IF      WRK-PARA-FILE       =   "1"
               PERFORM 380-CSV-OUT-SEC
           END-IF
      *
           .
       300-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       300-PRINT-SEC                SECTION.
      *
      *    Ä¢É¼°õºþ³«»Ï½èÍý
           PERFORM 900-DBSELECT-SEC
           PERFORM UNTIL   FLG-TENSU   =   1
      *        Ä¢É¼ÊÔ½¸<¸«½Ð¤·>½èÍý
               PERFORM 310-HEAD-HEN-SEC
      *        Ä¢É¼ÊÔ½¸<ÌÀºÙ>½èÍý
               PERFORM 320-BODY-HEN-SEC
                       UNTIL   FLG-TENSU   =   1
                       OR      CNT-LINE    =   15
      *        Ä¢É¼°õºþ½èÍý
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *    Ä¢É¼°õºþ½ªÎ»½èÍý
           PERFORM 900-DBCLOSE-SEC
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<¥Ø¥Ã¥À¡¼Éô>½èÍý
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
                                       SUM-KENSU
      *
           MOVE    SPACE           TO  HCML01
      *
           MOVE    SYS-1001-HOSPNAME
                                   TO  HCML01-HOSPNAME
           MOVE    WRK-MSTYMDH     TO  HCML01-MSTYMD
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCML01-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>½èÍý
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
      *    ÅÀ¿ôÉÕ²Ã¾ðÊó¸¡º÷
           PERFORM 900-TENSUPLUS-READ-SEC
      *    ¹â³ÛÀÁµá»ñÎÁÍÑÌôºÞÊ¬Îà¾ðÊó
           PERFORM 900-SYS1036-READ-SEC
      *
           IF          FLG-SYSKANRI    =   1
               MOVE    TNS-NAME        TO  WRK-NAME
           ELSE
               MOVE    SPACE           TO  WRK-NAME
               STRING  TNS-NAME            DELIMITED  BY  SPACE
                       "¡Ê"                DELIMITED  BY  SIZE
                       SYS-1036-BUNRUIMEI  DELIMITED  BY  SPACE
                       "¡Ë"                DELIMITED  BY  SIZE
                                   INTO    WRK-NAME
               END-STRING
           END-IF
      *
           ADD     1               TO  CNT-LINE
      *
           MOVE    TNS-TEN         TO  WRK-TEN
           MOVE    WRK-TEN         TO  HCML01-TEN(CNT-LINE)
           MOVE    TNS-CSYYOURYO   TO  WRK-CSYYOURYO
           MOVE    WRK-CSYYOURYO   TO  HCML01-CSYYOURYO(CNT-LINE)
      *
           MOVE    TNS-SRYCD       TO  HCML01-SRYCD(CNT-LINE)
           MOVE    WRK-NAME        TO  HCML01-NAME(CNT-LINE)
           MOVE    TNS-TANINAME    TO  HCML01-TANINAME(CNT-LINE)
           MOVE    TNS-YUKOSTYMD   TO  HCML01-YUKOSTYMD(CNT-LINE)
           MOVE    TNS-YUKOEDYMD   TO  HCML01-YUKOEDYMD(CNT-LINE)
           MOVE    TNS-YAKKAKJNCD  TO  HCML01-YAKKAKJNCD(CNT-LINE)
           MOVE    "-"             TO  HCML01-NYORO(CNT-LINE)
           IF          TNS-KEIKASOCHIYMD   NOT =  "00000000"
               MOVE    TNS-KEIKASOCHIYMD
                                   TO  HCML01-KEIKASOCHIYMD(CNT-LINE)
           END-IF
      *    ËãÆÇ¶èÊ¬
           EVALUATE  TNS-MADOKUKBN
               WHEN  "0"
                   MOVE  SPACE         TO  HCML01-MADOKUKBN(CNT-LINE)
               WHEN  "1"
                   MOVE  "ËãÆÇ"        TO  HCML01-MADOKUKBN(CNT-LINE)
               WHEN  "2"
                   MOVE  "ÆÇÌô"        TO  HCML01-MADOKUKBN(CNT-LINE)
               WHEN  "3"
                   MOVE  "³Ð¤»¤¤ºÞ"    TO  HCML01-MADOKUKBN(CNT-LINE)
               WHEN  "5"
                   MOVE  "¸þÀº¿ÀÌô"    TO  HCML01-MADOKUKBN(CNT-LINE)
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"    TO  HCML01-MADOKUKBN(CNT-LINE)
           END-EVALUATE
      *    ¿À·ÐÇË²õºÞ
           EVALUATE  TNS-SINKEIHAKAIKBN
               WHEN  "0"
                   MOVE  SPACE    TO  HCML01-SINKEIHAKAIKBN(CNT-LINE)
               WHEN  "1"
                   MOVE  "¿À·ÐÇË²õ"
                                  TO  HCML01-SINKEIHAKAIKBN(CNT-LINE)
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"
                                  TO  HCML01-SINKEIHAKAIKBN(CNT-LINE)
           END-EVALUATE
      *    À¸Êª³ØÅªÀ½ºÞ
           EVALUATE  TNS-SEIBUTUGAKUKBN
               WHEN  "0"
                   MOVE  SPACE    TO  HCML01-SEIBUTUGAKUKBN(CNT-LINE)
               WHEN  "1"
                   MOVE  "À¸ÊªÀ½ºÞ"
                                  TO  HCML01-SEIBUTUGAKUKBN(CNT-LINE)
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"
                                  TO  HCML01-SEIBUTUGAKUKBN(CNT-LINE)
           END-EVALUATE
      *    Â¤±ÆºÞ(Â¤±ÆÊä½õºÞ) 
           EVALUATE  TNS-ZOEIZAIKBN
               WHEN  "0"
                   MOVE  SPACE        TO  HCML01-ZOEIZAIKBN(CNT-LINE)
               WHEN  "1"
                   MOVE  "Â¤±ÆºÞ"     TO  HCML01-ZOEIZAIKBN(CNT-LINE)
               WHEN  "2"
                   MOVE  "Â¤±ÆÊä½õ"   TO  HCML01-ZOEIZAIKBN(CNT-LINE)
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"   TO  HCML01-ZOEIZAIKBN(CNT-LINE)
           END-EVALUATE
      *    ÌôºÞ 
           EVALUATE  TNS-YKZKBN
               WHEN  "1"
                   MOVE  "ÆâÍÑ"       TO  HCML01-YKZKBN(CNT-LINE)
               WHEN  "3"
                   MOVE  "¤½¤ÎÂ¾"     TO  HCML01-YKZKBN(CNT-LINE)
               WHEN  "4"
                   MOVE  "Ãí¼Í"       TO  HCML01-YKZKBN(CNT-LINE)
               WHEN  "6"
                   MOVE  "³°ÍÑ"       TO  HCML01-YKZKBN(CNT-LINE)
               WHEN  "8"
                   MOVE  "»õ²ÊÌôºÞ"   TO  HCML01-YKZKBN(CNT-LINE)
               WHEN  "9"
                   MOVE  "»õ²ÊÆÃÄê"   TO  HCML01-YKZKBN(CNT-LINE)
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"   TO  HCML01-YKZKBN(CNT-LINE)
           END-EVALUATE
      *    ºÞ·¿
           EVALUATE  TNS-ZAIGATAKBN
               WHEN  "0"
                   MOVE  SPACE        TO  HCML01-ZAIGATAKBN(CNT-LINE)
               WHEN  "1"
                   MOVE  "»¶ºÞ"       TO  HCML01-ZAIGATAKBN(CNT-LINE)
               WHEN  "2"
                   MOVE  "ðùÎ³ºÞ"     TO  HCML01-ZAIGATAKBN(CNT-LINE)
               WHEN  "3"
                   MOVE  "±ÕºÞ"       TO  HCML01-ZAIGATAKBN(CNT-LINE)
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"   TO  HCML01-ZAIGATAKBN(CNT-LINE)
           END-EVALUATE
      *
           PERFORM 900-DBFETCH-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸<ÌÀºÙ¹Ô>½èÍý
      *****************************************************************
       330-CSV-HEN-SEC            SECTION.
      *
      *    ÅÀ¿ôÉÕ²Ã¾ðÊó¸¡º÷
           PERFORM 900-TENSUPLUS-READ-SEC
      *    ¹â³ÛÀÁµá»ñÎÁÍÑÌôºÞÊ¬Îà¾ðÊó
           PERFORM 900-SYS1036-READ-SEC
      *
           ADD     1               TO  SUM-KENSU
      *
           INITIALIZE                  TENSUCSV-REC
      *
           IF          FLG-SYSKANRI    =   1
               MOVE    TNS-NAME        TO  WRK-NAME
           ELSE
               MOVE    SPACE           TO  WRK-NAME
               STRING  TNS-NAME            DELIMITED  BY  SPACE
                       "¡Ê"                DELIMITED  BY  SIZE
                       SYS-1036-BUNRUIMEI  DELIMITED  BY  SPACE
                       "¡Ë"                DELIMITED  BY  SIZE
                                           INTO    WRK-NAME
               END-STRING
           END-IF
      *
           MOVE    TNS-HOSPNUM     TO  CSV-HOSPNUM
           MOVE    TNS-TEN         TO  CSV-TEN
           MOVE    TNS-CSYYOURYO   TO  CSV-CSYYOURYO
      *
           MOVE    TNS-SRYCD       TO  CSV-SRYCD
           MOVE    WRK-NAME        TO  CSV-NAME
           MOVE    TNS-TANINAME    TO  CSV-TANINAME
           MOVE    TNS-YUKOSTYMD   TO  CSV-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD   TO  CSV-YUKOEDYMD
           MOVE    TNS-YAKKAKJNCD  TO  CSV-YAKKAKJNCD
           MOVE    TNS-KEIKASOCHIYMD   TO CSV-KEIKASOCHIYMD
      *    ËãÆÇ¶èÊ¬
           EVALUATE  TNS-MADOKUKBN
               WHEN  "0"
                   MOVE  SPACE         TO  CSV-MADOKUKBN
               WHEN  "1"
                   MOVE  "ËãÆÇ"        TO  CSV-MADOKUKBN
               WHEN  "2"
                   MOVE  "ÆÇÌô"        TO  CSV-MADOKUKBN
               WHEN  "3"
                   MOVE  "³Ð¤»¤¤ºÞ"    TO  CSV-MADOKUKBN
               WHEN  "5"
                   MOVE  "¸þÀº¿ÀÌô"    TO  CSV-MADOKUKBN
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"    TO  CSV-MADOKUKBN
           END-EVALUATE
      *    ¿À·ÐÇË²õºÞ
           EVALUATE  TNS-SINKEIHAKAIKBN
               WHEN  "0"
                   MOVE  SPACE         TO  CSV-SINKEIHAKAIKBN
               WHEN  "1"
                   MOVE  "¿À·ÐÇË²õ"    TO  CSV-SINKEIHAKAIKBN
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"    TO  CSV-SINKEIHAKAIKBN
           END-EVALUATE
      *    À¸Êª³ØÅªÀ½ºÞ
           EVALUATE  TNS-SEIBUTUGAKUKBN
               WHEN  "0"
                   MOVE  SPACE         TO  CSV-SEIBUTUGAKUKBN
               WHEN  "1"
                   MOVE  "À¸ÊªÀ½ºÞ"    TO  CSV-SEIBUTUGAKUKBN
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"    TO  CSV-SEIBUTUGAKUKBN
           END-EVALUATE
      *    Â¤±ÆºÞ(Â¤±ÆÊä½õºÞ) 
           EVALUATE  TNS-ZOEIZAIKBN
               WHEN  "0"
                   MOVE  SPACE        TO  CSV-ZOEIZAIKBN
               WHEN  "1"
                   MOVE  "Â¤±ÆºÞ"     TO  CSV-ZOEIZAIKBN
               WHEN  "2"
                   MOVE  "Â¤±ÆÊä½õ"   TO  CSV-ZOEIZAIKBN
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"   TO  CSV-ZOEIZAIKBN
           END-EVALUATE
      *    ÌôºÞ 
           EVALUATE  TNS-YKZKBN
               WHEN  "1"
                   MOVE  "ÆâÍÑ"       TO  CSV-YKZKBN
               WHEN  "3"
                   MOVE  "¤½¤ÎÂ¾"     TO  CSV-YKZKBN
               WHEN  "4"
                   MOVE  "Ãí¼Í"       TO  CSV-YKZKBN
               WHEN  "6"
                   MOVE  "³°ÍÑ"       TO  CSV-YKZKBN
               WHEN  "8"
                   MOVE  "»õ²ÊÌôºÞ"   TO  CSV-YKZKBN
               WHEN  "9"
                   MOVE  "»õ²ÊÆÃÄê"   TO  CSV-YKZKBN
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"   TO  CSV-YKZKBN
           END-EVALUATE
      *    ºÞ·¿
           EVALUATE  TNS-ZAIGATAKBN
               WHEN  "0"
                   MOVE  SPACE        TO  CSV-ZAIGATAKBN
               WHEN  "1"
                   MOVE  "»¶ºÞ"       TO  CSV-ZAIGATAKBN
               WHEN  "2"
                   MOVE  "ðùÎ³ºÞ"     TO  CSV-ZAIGATAKBN
               WHEN  "3"
                   MOVE  "±ÕºÞ"       TO  CSV-ZAIGATAKBN
               WHEN  OTHER
                   MOVE  "¡ö¡ö¡ö¡ö"   TO  CSV-ZAIGATAKBN
           END-EVALUATE
      *
           MOVE    ","                TO  CSV-FILLER1
                                          CSV-FILLER2
                                          CSV-FILLER3
                                          CSV-FILLER4
                                          CSV-FILLER5
                                          CSV-FILLER6
                                          CSV-FILLER7
                                          CSV-FILLER8
                                          CSV-FILLER9
                                          CSV-FILLER10
                                          CSV-FILLER11
                                          CSV-FILLER12
                                          CSV-FILLER13
                                          CSV-FILLER14
                                          CSV-FILLER15
           WRITE   TENSUCSV-REC
      *
           PERFORM 900-DBFETCH-SEC
      *
           .
       330-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼£Ã£Ó£Ö½èÍý
      *****************************************************************
       380-CSV-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSFILESVAREA
           MOVE    "I"                 TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID    TO  SFILESV-TBL-KEY
           MOVE    "tensu.sh"          TO  SFILESV-SHELLID(1)
           MOVE    1                   TO  SFILESV-SHORI-RENNUM(1)
           MOVE    LNK-PRTKANRI-RENNUM TO  SFILESV-RENNUM(1)
      *----(04.08.01)--UPD-START---
      *     MOVE    "/tmp/"             TO  SFILESV-FOR-FOLDER(1)
           MOVE    SGETTEMP-TEMPDIR    TO  SFILESV-FOR-FOLDER(1)
           STRING  WRK-PARA-CSVFILE        DELIMITED  BY  SPACE
                   ".wk"                   DELIMITED  BY  SIZE
                                   INTO    SFILESV-FOR-DATA (1)
           END-STRING
      *----(04.08.01)--UPD-END-----
           MOVE    WRK-PARA-CSVDIR     TO  SFILESV-TO-FOLDER(1)
           MOVE    WRK-PARA-CSVFILE    TO  SFILESV-TO-DATA(1)
           MOVE    SUM-KENSU           TO  SFILESV-CNT-ALL(1)
           MOVE    JOB-SHELLMSG        TO  SFILESV-TITLE(1)
           CALL    "ORCSFILESV"            USING
                                           ORCSFILESVAREA
                                           SPA-AREA
      *
           .
       380-CSV-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼°õºþ½èÍý
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
           IF      CNT-LINE        =   ZERO
               GO  TO  390-PRINT-OUT-EXT
           END-IF
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
           MOVE    "HCML01.red"    TO  SPRT-PRTID
           MOVE    "ÅÀ¿ô¥Þ¥¹¥¿°ìÍ÷É½¡¦ÌôºÞÊ¬Îà¶èÊ¬"
                                   TO  SPRT-TITLE
           MOVE    HCML01          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "°õºþ£Ä£Â¤Ë¹¹¿·¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                       TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   TENSUERR-FILE
      *
           IF          STS-TENSUERR    =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              TENSUERR-FILE
      *
               MOVE    WRK-TENSUERR    TO  TENSUERR-REC
               WRITE   TENSUERR-REC
      *
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-PARA-HOSPNUM
                                       TO  JOB-HOSPNUM
               MOVE    WRK-TENSUERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           CLOSE   TENSUERR-FILE
           MOVE    1               TO  FLG-END
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
           DISPLAY "*** ORHCML12 END ***"
      *     
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   ÏÂÎñÇ¯·îÆü¢ªÀ¾ÎñÇ¯·îÆü¡Êµ¡Ç½£±£²¡Ë
      *****************************************************************
       831-YMDCONV12-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-LNKYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-LNKYMD
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¡¸¡º÷ 
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *  
           INITIALIZE                  TNS-TENSU-REC
           MOVE    "600000000"     TO  TNS-SRYCD
           MOVE    "699999999"     TO  TNSUP-SRYCD
      *
           MOVE    WRK-PARA-MSTYMD     TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           EVALUATE    WRK-PARA-SYUKBN
           WHEN    1
           WHEN    4
               MOVE    "key23s"  TO  WRK-PATH
           WHEN    2
               MOVE    "key23m"  TO  WRK-PATH
           WHEN    3
               MOVE    "key23k"  TO  WRK-PATH
           END-EVALUATE
      *
           MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF          MCP-RC      =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     ÅÀ¿ô¥Þ¥¹¥¿¤ò¸¡º÷½èÍý
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF          MCP-RC      =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÅÀ¿ôÉÕ²Ã¾ðÊó¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-TENSUPLUS-READ-SEC      SECTION.
      *
           INITIALIZE                  TENSUPLUS-REC
           MOVE    TNS-SRYCD       TO  TNSP-SRYCD
           MOVE    TNS-YUKOSTYMD   TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD   TO  TNSP-YUKOEDYMD
      *
           MOVE    WRK-PARA-HOSPNUM    TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF          MCP-RC      =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF          MCP-RC      =   ZERO
                   MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
                   MOVE    ZERO            TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1               TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-TENSUPLUS-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¹â³ÛÀÁµá»ñÎÁÍÑÌôºÞÊ¬Îà¾ðÊó
      ****************************************************************
       900-SYS1036-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1036-REC
           INITIALIZE                  SYS-1036-REC
           MOVE    "1036"          TO  SYS-1036-KANRICD
           MOVE    TNSP-TENPUYAKKBN    TO  SYS-1036-KBNCD
           MOVE    WRK-PARA-MSTYMD     TO  SYS-1036-STYUKYMD
                                           SYS-1036-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1036-HOSPNUM
           MOVE    SYS-1036-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1036-REC
           END-IF
      *
           .
       900-SYS1036-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF          MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF          MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
