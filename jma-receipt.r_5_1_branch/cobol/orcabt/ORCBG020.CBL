      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
       PROGRAM-ID.             ORCBG020.
      *****************************************************************
      *  �����ƥ�̾        :   �ϣңã�
      *  ���֥����ƥ�̾    :   �Ģɼ
      *  ����ݡ��ͥ��̾  :   ���Ź԰�̤���ϴ��԰���ɽ
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/08/20    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  04.08.01    NACL-����     14/07/22  ����ե�����ǥ��쥯�ȥ�����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    �����ѥե�����
           SELECT  TOKEI-FILE      ASSIGN  TOKEIPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOKEI-JYUNBAN
                                   FILE    STATUS  IS  STS-TOKEI.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    �����ѥե�����
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-JYUNBAN           PIC X(120).
           03  TOKEI-PTNUM             PIC X(20).
           03  TOKEI-NAME              PIC X(100).
           03  TOKEI-LASTYMD           PIC X(08).
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ���ץե�����
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER                  PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCM63.INC".
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(05).
           03  IDY                     PIC 9(05).
           03  IDZ                     PIC 9(05).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR                 PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-STRYM          PIC X(06).
           03  WRK-PARA-ENDYM          PIC X(06).
           03  WRK-PARA-JYUNBAN        PIC X(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-NUM                 PIC 9(05).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-LINE                PIC 9(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *    �����ֹ��
           03  WRK-KEY1.
               05  WRK-KEY1-PTNUM      PIC X(20).
               05  WRK-KEY1-KANANAME   PIC X(100).
      *    ���ԥ��ʻ�̾��
           03  WRK-KEY2.
               05  WRK-KEY2-KANANAME   PIC X(100).
               05  WRK-KEY2-PTNUM      PIC X(20).
      *
      *    ���顼����
           03  WRK-RECEERR             PIC X(200).
      *
      *    �����Խ���
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-STRYMD              PIC X(8).
           03  WRK-ENDYMD              PIC X(8).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-KIKAN.
               05  WRK-STRYM           PIC X(16).
               05  WRK-LABEL           PIC X(02).
               05  WRK-ENDYM           PIC X(16).
           03  WRK-PRTYMD              PIC X(22).
           03  WRK-LASTYMD             PIC X(09).
      *
      *    �����Խ�
           03  WRK-Z5                  PIC ZZZZ9.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �����ֹ�
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ��������
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    �����ѥǡ���
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �����ģ¹�������
           COPY    "CPORCSPRT.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ���������Ѵ�����
           COPY    "CPORCSGDAY.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *----(04.08.01)--ADD-START---
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR 
      *----(04.08.01)--ADD-END-----
      *
      *    �ģ¥����ץ����
           PERFORM 100-DBOPEN-SEC
      *
      *    �ѥ�᥿���å�
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
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-STRYM
                                               WRK-PARA-ENDYM
                                               WRK-PARA-JYUNBAN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG020"          TO  JOB-PGID
           MOVE    "���Ź԰�̤���ϴ��԰���ɽ"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    �ե���������
           MOVE    "BG02001"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1) 
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    �ѥ�᥿�Խ�����
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ѥ�᥿�Խ�����
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    ���������å�
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
           MOVE    WRK-SYSYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PRTYMD
      *
           MOVE    SPACE               TO  WRK-KIKAN
      *    ����ɽ��
           IF    ( WRK-PARA-STRYM  =   SPACE ) AND
                 ( WRK-PARA-ENDYM  =   SPACE )
               MOVE    "���ֻ���ʤ�"      TO  WRK-STRYM
           ELSE
               MOVE    "��"                TO  WRK-LABEL
           END-IF
      *
      *    ����ǯ��å�
           IF      WRK-PARA-STRYM  NOT =   SPACE
                   MOVE    WRK-PARA-STRYM      TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               MOVE    WRK-SYMD            TO  WRK-STRYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:16)   TO  WRK-STRYM
           ELSE
               MOVE    "00000000"          TO  WRK-STRYMD
           END-IF
      *
      *    ��λǯ��å�
           IF      WRK-PARA-ENDYM  NOT =   SPACE
               MOVE    WRK-PARA-ENDYM      TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               PERFORM 20021-GETSUMASTU-GET-SEC
               MOVE    WRK-SYMD            TO  WRK-ENDYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:16)   TO  WRK-ENDYM
           ELSE
               MOVE    "99999999"          TO  WRK-ENDYMD
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    �����ѥե�����򥯥ꥢ����
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *    �����ѥե�����򳫤�
           OPEN    I-O                 TOKEI-FILE
      *
      *    �Խ�����
           PERFORM 210-TOKEI-HENSYU-SEC
      *
      *    �����ѥե�������Ĥ���
           CLOSE                       TOKEI-FILE
      *    �����ѥե�����򳫤�
           OPEN    INPUT               TOKEI-FILE
      *
      *    ��������
           PERFORM 300-PRINT-HENSYU-SEC
      *
      *    �����ѥե�������Ĥ���
           CLOSE                       TOKEI-FILE
      *    �����ѥե�����򥯥ꥢ����
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
      *    �ե�������
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���׽���
      *****************************************************************
       210-TOKEI-HENSYU-SEC        SECTION.
      *
      *    �оݴ����ɤ߹���
           PERFORM 900-PTINF-SELECT-SEC
      *
      *    �оݴ��Ԥ��ʤ��ʤ�ޤ�
           PERFORM UNTIL   FLG-PTINF   =   1
      *        �оݴ��Խ���
               PERFORM 220-TOKEI-SET-SEC
      *        �оݴ����ɤ߹���
               PERFORM 900-PTINF-FETCH-SEC
           END-PERFORM
      *
      *    �оݴ����ɤ߹���
           PERFORM 900-PTINF-CLOSE-SEC
      *
           .
       210-TOKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ץ��åȽ���
      *****************************************************************
       220-TOKEI-SET-SEC           SECTION.
      *
      *    �����ֹ楻�å�
           PERFORM 900-PTNUM-READ-SEC
      *
           INITIALIZE                  TOKEI-REC
      *
      *    ���֥������å�
           IF      WRK-PARA-JYUNBAN    =   "1"
               MOVE    SPACE           TO  WRK-KEY2
               MOVE    PTNUM-PTNUM     TO  WRK-KEY2-PTNUM
               MOVE    PTINF-KANANAME  TO  WRK-KEY2-KANANAME
               MOVE    WRK-KEY2        TO  TOKEI-JYUNBAN
           ELSE
               MOVE    SPACE           TO  WRK-KEY1
               MOVE    PTNUM-PTNUM     TO  WRK-KEY1-PTNUM
               MOVE    PTINF-KANANAME  TO  WRK-KEY1-KANANAME
               MOVE    WRK-KEY1        TO  TOKEI-JYUNBAN
           END-IF
      *
           MOVE    PTNUM-PTNUM     TO  TOKEI-PTNUM
           MOVE    PTINF-NAME      TO  TOKEI-NAME
           IF      PTINF-CREYMD        >=  PTINF-UPYMD
               MOVE    PTINF-CREYMD    TO  TOKEI-LASTYMD
           ELSE
               MOVE    PTINF-UPYMD     TO  TOKEI-LASTYMD
           END-IF
      *
           WRITE   TOKEI-REC
      *
           .
       220-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       300-PRINT-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
           MOVE    ZERO            TO  CNT-NUM
      *
      *    ���ץե������ɤ߹���
           INITIALIZE                  TOKEI-REC
           PERFORM 900-TOKEI-NEXT-SEC
      *
      *    ���ץǡ����ʤ��ʤ�ޤ�
           PERFORM UNTIL ( FLG-TOKEI   =   1 )
      *        Ģɼ�Խ�<���Ф�>����
               PERFORM 310-HEAD-HEN-SEC
      *        Ģɼ�Խ�<����>����
               PERFORM 320-BODY-HEN-SEC
                       UNTIL ( FLG-TOKEI   =   1 )
                       OR    ( CNT-LINE    =   70 )
      *        Ģɼ��������
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           .
       300-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�Խ�<���Ф�>����
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  HCM63
           MOVE    WRK-KIKAN       TO  HCM63-KIKAN
           MOVE    WRK-PRTYMD      TO  HCM63-PRTYMD
           MOVE    CNT-PAGE        TO  WRK-Z5
           MOVE    WRK-Z5          TO  HCM63-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�Խ�<���ٹ�>����
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           COMPUTE CNT-NUM     =   CNT-NUM    +   1
           COMPUTE CNT-LINE    =   CNT-LINE   +   1
      *
           MOVE    CNT-NUM         TO  WRK-Z5
           MOVE    WRK-Z5          TO  HCM63-NUM(CNT-LINE)
      *
           IF      TOKEI-PTNUM(11:)    =   SPACE
               MOVE    TOKEI-PTNUM(1:10)   TO  HCM63-PTNUM2(CNT-LINE)
           ELSE
               MOVE    TOKEI-PTNUM         TO  HCM63-PTNUM(CNT-LINE)
           END-IF
      *
           MOVE    TOKEI-NAME          TO  HCM63-NAME(CNT-LINE)
      *
           IF      TOKEI-LASTYMD       =   "99999999"
               MOVE    "������"            TO  HCM63-LASTYMD(CNT-LINE)
           ELSE
               MOVE    TOKEI-LASTYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-LASTYMD         TO  HCM63-LASTYMD(CNT-LINE)
           END-IF
      *
           PERFORM 900-TOKEI-NEXT-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ��������
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *    ��������
           INITIALIZE                              ORCSPRTAREA
           MOVE    "INS"                       TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM         TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY        TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-STRYM              TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD         TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID        TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY       TO  SPRT-PRIORITY
           MOVE    "HCM63.red"                 TO  SPRT-PRTID
           MOVE    "���Ź԰�̤���ϴ��԰���ɽ"  TO  SPRT-TITLE
           MOVE    HCM63                       TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID         TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID           TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM          TO  SPRT-PRTNM
           MOVE    "1"                         TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "�����ģ¤˹����Ǥ��ޤ���Ǥ���"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������
      *****************************************************************
       20021-GETSUMASTU-GET-SEC    SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD(1:6)   TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-SYMD
      *
           .
       20021-GETSUMASTU-GET-EXT.
            EXIT.
      *
      *****************************************************************
      *    �������ܸ��Ѵ�����
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
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "��"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ե����å����Խ�����
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    ���ղ��̥����å�����
           INITIALIZE                  ORCSGDAYAREA
           MOVE    WRK-SYMD        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           MOVE    SGDAY-OTDAY     TO  WRK-LASTYMD
      *
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT                  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ����ִ������Ͻ���
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"               USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
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
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-PAGE        =   ZERO 
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG020 END ***"
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ�ޥ����ɹ�
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    PTINF-PTID      TO  PTNUM-PTID
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTNUM
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
               ELSE
                   MOVE    1               TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ���ޥ����ɹ�
      *****************************************************************
       900-PTINF-SELECT-SEC        SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-STRYMD      TO  PTINF-ST-SRYYMD
           MOVE    WRK-ENDYMD      TO  PTINF-ED-SRYYMD
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key22"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-PTINF-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ���ޥ����ɹ�
      *****************************************************************
       900-PTINF-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key22"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ���ޥ����ɹ�
      *****************************************************************
       900-PTINF-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key22"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PTINF-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ץե�������ɹ�
      *****************************************************************
       900-TOKEI-NEXT-SEC         SECTION.
      *
           READ    TOKEI-FILE     NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡������ץ����
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
      *
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
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
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.