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
       PROGRAM-ID.             ORHCML07.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ޥ���
      *  ����ݡ��ͥ��̾  : ���ϥ����ɰ���ɽ��������ʣȣạ̃ͣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/04/08    MCC-����      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     02/04/24  ���������������ģ¤��ɲ�
      *  01.00.02    MCC-ƣ��     02/05/10  �ǿ��Υ���ִ����ģ¤ؤι���
      *
      *  01.01.01    NACL-ƣ��    02/10/10  �����ȶ�ʬ�ɲ�
      *  01.01.02    NACL-����    03/03/10  TERMID���ΰ����ʤ�
      *  02.00.01    NACL-����    03/12/25  ����¾�����ɲ�
      *  02.00.02    NACL-����    06/07/27  �ե饰�ɲá�MONFUNC�б�
      *  03.05.01    NACL-����    07/05/29  ���롼�׿����б�
      *  04.01.01    NACL-����    08/01/17  ��������ɲ�
      *  04.03.01    NACL-����    08/06/13  �ãӣ֥ե���������б�
      *  04.08.01    NACL-����    14/07/31  ����ե�����ǥ��쥯�ȥ�����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  TENSUERR-FILE   ASSIGN  TENSUERR
                                   FILE    STATUS  IS  STS-TENSUERR.
      *    �ãӣ֥ե�����
           SELECT  TENSUCSV-FILE   ASSIGN  TENSUCSV
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TENSUCSV.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  TENSUERR-FILE.
       01  TENSUERR-REC             PIC X(200). 
      *    �ãӣ֥ե�����
       FD  TENSUCSV-FILE.
       01  TENSUCSV-REC.
           03  CSV-HOSPNUM         PIC 9(2).
           03  CSV-FILLER1         PIC X(1).
           03  CSV-SRYCD           PIC X(20).
           03  CSV-FILLER2         PIC X(1).
           03  CSV-NAME            PIC X(60).
           03  CSV-FILLER3         PIC X(1).
           03  CSV-TEN             PIC 9(09).99.
      *
       WORKING-STORAGE             SECTION.
      *    ���������ΰ�
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *----(04.08.01)--ADD-START---
      *    ���顼�ե����� ̾���ΰ�
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TENSUERR//.
      *----(04.08.01)--ADD-END-----
      *     
           COPY    "HCML06.INC".
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-TENSUERR        PIC X(02).
           03  STS-TENSUCSV        PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-ICD             PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
      *
      *    �����ΰ�
       01  SUM-AREA.
           03  SUM-KENSU           PIC 9(07).
      *
      *    �ѥ�᥿�ΰ�
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
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
           03  WRK-TENSUERR          PIC X(200).        
      *
           03  WRK-LNKYMD            PIC X(10).
           03  WRK-MSTYMDH           PIC X(10).
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-TEN               PIC ZZZ,ZZZ,ZZ9.99.    
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ŵ���̾      
           COPY  "CPSK1001.INC".
      *
      *    ������ץ��̾������ƾ���
           COPY  "CPSK1031.INC".
      *  
      *    �����ޥ���
           COPY    "CPTENSU.INC".
      *    ���ϥ�����
       01  ICD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    ����ִ����ޥ���
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
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �����ģ¹�������
           COPY    "CPORCSPRT.INC".          
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *    ���饤�������¸�ģ����極��
           COPY    "CPORCSFILESV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    ����ե�����̾�Խ�
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
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END         =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  SUM-AREA
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
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           MOVE    "ORHCML07"      TO  JOB-PGID
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "���ϥ����ɰ���ɽ��������" 
                                   TO  JOB-SHELLMSG
               WHEN    2
                   MOVE    "���ϥ����ɰ���ɽ������¾����" 
                                   TO  JOB-SHELLMSG
               WHEN    3
                   MOVE    "���ϥ����ɰ���ɽ����ྦ��"
                                   TO  JOB-SHELLMSG
           END-EVALUATE
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *                             
           MOVE    LNK-PRTKANRI-SKYYMD
                                    TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           MOVE    WRK-LNKYMD       TO  WRK-MSTYMDH
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TENSUERR-BASENAME   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  TENSUERR
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
       110-PARA-HENSYU-SEC                   SECTION.              
      *      
      *    ���ŵ���̾���Խ�
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-MSTYMD     TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE    
               INITIALIZE                  SYS-1001-REC
               MOVE    "���ŵ���̾�Τ������Ǥ��ޤ���Ǥ�����"
                                           TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
           .
      *
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      WRK-PARA-CSVKBN =   "1"
      *        �ãӣ��Խ�����
               PERFORM 300-CSV-SEC 
           ELSE
      *        Ģɼ�Խ�����
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
      *    �硡������
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
      *    �ãӣ֥ե�����򳫤�
           OPEN    OUTPUT              TENSUCSV-FILE
      *
      *    Ģɼ�������Ͻ���
           PERFORM 900-DBSELECT-SEC
      *    Ģɼ�Խ�<����>����
           PERFORM 330-CSV-HEN-SEC
      *    Ģɼ������λ����
           PERFORM 900-DBCLOSE-SEC
      *
      *    �ãӣ֥ե�������Ĥ���
           CLOSE                       TENSUCSV-FILE
      *
      *    ���饤�������¸�ξ��
           IF      WRK-PARA-FILE       =   "1"
               PERFORM 380-CSV-OUT-SEC
           END-IF
      *
           .
       300-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       300-PRINT-SEC                SECTION.
      *
      *    Ģɼ�������Ͻ���
           PERFORM 900-DBSELECT-SEC
           PERFORM UNTIL   FLG-TENSU   =   1
      *        Ģɼ�Խ�<���Ф�>����
               PERFORM 310-HEAD-HEN-SEC 
      *        Ģɼ�Խ�<����>����
               PERFORM 320-BODY-HEN-SEC
                       UNTIL   FLG-TENSU     =   1
                       OR      CNT-LINE      =   35
      *        Ģɼ��������
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *    Ģɼ������λ����
           PERFORM 900-DBCLOSE-SEC
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�Խ�<�إå�����>����
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
                                           SUM-KENSU
      *
           INITIALIZE                      HCML06
           MOVE    SPACE               TO  HCML06
      *
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HCML06-PAGE
      *
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "���������"      TO  HCML06-DAI
               WHEN    2
                   MOVE    "�ڤ���¾������"    TO  HCML06-DAI
               WHEN    3
                   MOVE    "�ڴ�ྦ�ʡ�"      TO  HCML06-DAI
           END-EVALUATE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�Խ�<���ٹ�>����
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           PERFORM 900-ICDSELECT-SEC 
           PERFORM UNTIL   FLG-ICD   =   1
               IF      CNT-LINE    =   35  
      *            Ģɼ��������
                   PERFORM 390-PRINT-OUT-SEC
      *            Ģɼ�Խ�<���Ф�>����
                   PERFORM 310-HEAD-HEN-SEC
               END-IF
               ADD     1                 TO  CNT-LINE
               MOVE    TNS-TEN           TO  WRK-TEN
               MOVE    WRK-TEN           TO  HCML06-TEN(CNT-LINE)
               MOVE    ICD-INPUTCD       TO  HCML06-SRYCD(CNT-LINE)
               MOVE    TNS-NAME          TO  HCML06-NAME(CNT-LINE)
               PERFORM 900-ICDFETCH-SEC
           END-PERFORM 
           PERFORM 900-ICDCLOSE-SEC 
      *
           PERFORM 900-DBFETCH-SEC 
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�Խ�<���ٹ�>����
      *****************************************************************
       330-CSV-HEN-SEC      SECTION.
      *
           PERFORM UNTIL   FLG-TENSU   =   1
               PERFORM 900-ICDSELECT-SEC 
               PERFORM UNTIL   FLG-ICD     =   1
                   ADD     1               TO  SUM-KENSU
                   INITIALIZE                  TENSUCSV-REC
                   MOVE    TNS-HOSPNUM     TO  CSV-HOSPNUM
                   MOVE    TNS-TEN         TO  CSV-TEN
                   MOVE    ICD-INPUTCD     TO  CSV-SRYCD
                   MOVE    TNS-NAME        TO  CSV-NAME
                   MOVE    ","             TO  CSV-FILLER1
                                               CSV-FILLER2
                                               CSV-FILLER3
                   WRITE   TENSUCSV-REC
                   PERFORM 900-ICDFETCH-SEC
               END-PERFORM 
               PERFORM 900-ICDCLOSE-SEC 
               PERFORM 900-DBFETCH-SEC 
           END-PERFORM    
      *
           .
       330-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ��������
      *****************************************************************
       380-CSV-OUT-SEC                SECTION.
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
      *    Ģɼ��������
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           IF      CNT-LINE             =   ZERO
               GO  TO  390-PRINT-OUT-EXT
           END-IF
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
           MOVE    "HCML06.red"        TO  SPRT-PRTID
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "���ϥ����ɰ���ɽ��������"
                                       TO  SPRT-TITLE
               WHEN    2
                   MOVE    "���ϥ����ɰ���ɽ������¾����"
                                       TO  SPRT-TITLE
               WHEN    3
                   MOVE    "���ϥ����ɰ���ɽ����ྦ��"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    HCML06              TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "�����ģ¤˹����Ǥ��ޤ���Ǥ���"
                                          TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC                           
           END-IF                                                              
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TENSUERR-FILE
      *     
           IF      STS-TENSUERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              TENSUERR-FILE
      *
               MOVE    WRK-TENSUERR         TO  TENSUERR-REC
               WRITE   TENSUERR-REC
      *         
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-PARA-HOSPNUM
                                       TO  JOB-HOSPNUM
               MOVE    WRK-TENSUERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
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
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORHCML07 END ***"
      *     
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SUM-KENSU       TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
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
      *   ����ǯ����������ǯ�����ʵ�ǽ������
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-LNKYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-LNKYMD
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����ɡ����� 
      *****************************************************************
       900-ICDSELECT-SEC           SECTION.
      *  
           INITIALIZE                  ICD-REC
           MOVE    TNS-SRYCD       TO  ICD-SRYCD
           MOVE    WRK-PARA-HOSPNUM    TO  ICD-HOSPNUM
           MOVE    ICD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
              PERFORM 900-ICDFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-ICD
           END-IF
           .
       900-ICDSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     ���ϥ����ɤ򸡺�����
      *****************************************************************
       900-ICDFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ICD-REC
               MOVE    ZERO            TO  FLG-ICD
               IF      WRK-PARA-CYUKBN     =   3
                   AND ICD-DSPSEQ          >   1
                   MOVE    1           TO  FLG-ICD
               END-IF
           ELSE
               MOVE    1               TO  FLG-ICD
           END-IF  
           .
       900-ICDFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����ɡ�������������
      *****************************************************************
       900-ICDCLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ICDCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ��������� 
      *****************************************************************
       900-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "700000000"         TO  TNS-SRYCD
                   MOVE    "799999999"         TO  TNSUP-SRYCD 
               WHEN    2
                   MOVE    "059000001"         TO  TNS-SRYCD
                   MOVE    "059999999"         TO  TNSUP-SRYCD 
               WHEN    3
                   MOVE    "058000001"         TO  TNS-SRYCD
                   MOVE    "058999999"         TO  TNSUP-SRYCD 
           END-EVALUATE
      *
           MOVE    WRK-PARA-MSTYMD     TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
            EVALUATE    WRK-PARA-SYUKBN        
                WHEN    1
                    MOVE    "key12s1"   TO  WRK-PATH 
                WHEN    2
                    MOVE    "key12m1"   TO  WRK-PATH 
                WHEN    3
                    MOVE    "key12k1"   TO  WRK-PATH
                WHEN    4
                    MOVE    "key12i1"   TO  WRK-PATH
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
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF        
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     �����ޥ����򸡺�����
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
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
      *    �����ޥ�����������������
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
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
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ߹���
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC              =   ZERO
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
           .
       900-DBCLOSE-EXT.
           EXIT.