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
       PROGRAM-ID.             ORHCML10.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ޥ���
      *  ����ݡ��ͥ��̾  : �����ޥ�������ɽ������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/07/11    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    03/10/15  ���������ʬ�ι�θϳ�����
      *  01.00.02    NACL-����    06/07/27  �ե饰�ɲá�MONFUNC�б�
      *  03.05.01    NACL-����    07/05/29  ���롼�׿����б�
      *  04.03.01    NACL-����    08/06/13  �ãӣ֥ե���������б�
      *  04.08.01    NACL-����    14/08/01  ����ե�����ǥ��쥯�ȥ�����
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
           03  CSV-HOSPNUM          PIC 9(02).
           03  CSV-FILLER1          PIC X(1).
           03  CSV-SRYCD            PIC X(9).
           03  CSV-FILLER2          PIC X(1).
           03  CSV-YUKOSTYMD        PIC X(8).
           03  CSV-FILLER3          PIC X(1).
           03  CSV-YUKOEDYMD        PIC X(8).
           03  CSV-FILLER4          PIC X(1).
           03  CSV-NAME             PIC X(100).
           03  CSV-FILLER5          PIC X(1).
           03  CSV-TEN              PIC 9(09).99.
           03  CSV-FILLER6          PIC X(1).
           03  CSV-HKNTEK           PIC X(12).
           03  CSV-FILLER7          PIC X(1).
           03  CSV-BUI              PIC X(10).
           03  CSV-FILLER8          PIC X(1).
           03  CSV-GAITEN           PIC X(3).
           03  CSV-FILLER9          PIC X(1).
           03  CSV-NYUTEN           PIC X(3).
           03  CSV-FILLER10         PIC X(1).
           03  CSV-KRM1             PIC X(3).
           03  CSV-FILLER11         PIC X(1).
           03  CSV-KET1             PIC X(3).
           03  CSV-FILLER12         PIC X(1).
           03  CSV-KRM2             PIC X(3).
           03  CSV-FILLER13         PIC X(1).
           03  CSV-KET2             PIC X(3).
           03  CSV-FILLER14         PIC X(1).
           03  CSV-KRM3             PIC X(3).
           03  CSV-FILLER15         PIC X(1).
           03  CSV-KET3             PIC X(3).
           03  CSV-FILLER16         PIC X(1).
           03  CSV-KRM4             PIC X(3).
           03  CSV-FILLER17         PIC X(1).
           03  CSV-KET4             PIC X(3).
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
           COPY    "HCML07.INC".
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
       01  INEX-AREA.
           03  IDX                   PIC 9(02).      
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
           03  WRK-TENSUERR          PIC X(200).        
      *
           03  WRK-LNKYMD            PIC X(10).
           03  WRK-MSTYMDH           PIC X(22).
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
           03  WRK-CDKBN.
               05  WRK-CDKBN-KBN         PIC X(1).
               05  WRK-CDKBN-KBNNUM      PIC X(3).
               05  WRK-CDKBN-HI1         PIC X(1).
               05  WRK-CDKBN-KBNNUM-EDA  PIC X(2).
               05  WRK-CDKBN-HI2         PIC X(1).
               05  WRK-CDKBN-KOUBAN      PIC X(2).
      *
           03  WRK-PAGE              PIC ZZ,ZZ9.
           03  WRK-TEN               PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ9               PIC ZZ9.
      *
           03  WRK-SRYCDSTR          PIC X(09).
           03  WRK-SRYCDEND          PIC X(09).
      *
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
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
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
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
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE           DIVISION
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
                   UNTIL       FLG-END     =   1
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
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           MOVE    "ORHCML10"      TO  JOB-PGID
           MOVE    "�����ޥ�������ɽ������" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *                             
           MOVE    LNK-PRTKANRI-SKYYMD
                                    TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           MOVE    WRK-LNKYMD       TO  WRK-MAE-INPUT
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-OUT-INPUT    TO  WRK-MSTYMDH
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
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF  
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
                   UNTIL   FLG-TENSU   =   1
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
                       UNTIL   FLG-TENSU   =   1
                       OR      CNT-LINE    =  15 
      *        Ģɼ��������
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
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
           INITIALIZE                      HCML07
           MOVE    SPACE               TO  HCML07
      *
           MOVE    SYS-1001-HOSPNAME   TO  HCML07-HOSPNAME
           MOVE    WRK-MSTYMDH         TO  HCML07-MSTYMD
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HCML07-PAGE
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
           ADD     1                  TO  CNT-LINE
      *
      *    ���Ź԰٥�����
           MOVE    TNS-SRYCD          TO  HCML07-SRYCD (CNT-LINE)
      *    ��̾
           MOVE    TNS-NAME           TO  HCML07-NAME (CNT-LINE)
      *    ���
           MOVE    TNS-TEN            TO  WRK-TEN
           MOVE    WRK-TEN            TO  HCML07-TEN (CNT-LINE)
      *    �ݸ�Ŭ��
           EVALUATE    TNS-HKNTEKKBN
               WHEN    "0"
                   MOVE    SPACE          TO  HCML07-HKNTEK (CNT-LINE)
               WHEN    "1"
                   MOVE    "�ݸ�Ŭ��"     TO  HCML07-HKNTEK (CNT-LINE)
               WHEN    "2"
                   MOVE    "�ݸ�Ŭ�ѳ�"   TO  HCML07-HKNTEK (CNT-LINE)
           END-EVALUATE
      *    ���̶�ʬ
      *     EVALUATE    TNS-BUIKBN
      *         WHEN    "1"
      *             MOVE    "Ƭ��"         TO  HCML07-BUI (CNT-LINE)
      *         WHEN    "2"
      *             MOVE    "����"         TO  HCML07-BUI (CNT-LINE)
      *         WHEN    "3"
      *             MOVE    "�ͻ�"         TO  HCML07-BUI (CNT-LINE)
      *         WHEN    "5"
      *             MOVE    "����"         TO  HCML07-BUI (CNT-LINE)
      *         WHEN    "6"
      *             MOVE    "ʢ��"         TO  HCML07-BUI (CNT-LINE)
      *         WHEN    "7"
      *             MOVE    "����"         TO  HCML07-BUI (CNT-LINE)
      *         WHEN    "8"
      *             MOVE    "�ò���"       TO  HCML07-BUI (CNT-LINE)
      *         WHEN    "0"
      *             MOVE    "����¾"       TO  HCML07-BUI (CNT-LINE)
      *     END-EVALUATE
           EVALUATE    TNS-TENSIKIBETU
               WHEN    0
                   MOVE    "��ȴ��"       TO  HCML07-BUI (CNT-LINE)
               WHEN    4
                   MOVE    "�ǹ���"       TO  HCML07-BUI (CNT-LINE)
           END-EVALUATE
      *
      *    �����󽸷��輱�̡ʳ����
           MOVE    TNS-GAITENTTLKBN   TO  WRK-ZZ9
           MOVE    WRK-ZZ9            TO  HCML07-GAITEN (CNT-LINE)
      *    �����󽸷��輱�̡�������
           MOVE    TNS-NYUTENTTLKBN   TO  WRK-ZZ9
           MOVE    WRK-ZZ9            TO  HCML07-NYUTEN (CNT-LINE)
      *
      *    ���֣�
           MOVE  TNS-SSTKIJUNCD(1)   TO HCML07-KRM1 (CNT-LINE)
      *    �����
           MOVE  TNS-SSTKIJUNCD(2)   TO HCML07-KET1 (CNT-LINE)
      *    ���֣�
           MOVE  TNS-SSTKIJUNCD(3)   TO HCML07-KRM2 (CNT-LINE)
      *    �����
           MOVE  TNS-SSTKIJUNCD(4)   TO HCML07-KET2 (CNT-LINE)
      *    ���֣�
           MOVE  TNS-SSTKIJUNCD(5)   TO HCML07-KRM3 (CNT-LINE)
      *    �����
           MOVE  TNS-SSTKIJUNCD(6)   TO HCML07-KET3 (CNT-LINE)
      *    ���֣�
           MOVE  TNS-SSTKIJUNCD(7)   TO HCML07-KRM4 (CNT-LINE)
      *    �����
           MOVE  TNS-SSTKIJUNCD(8)   TO HCML07-KET4 (CNT-LINE)
      *    ͭ�����ֳ�����
           MOVE    TNS-YUKOSTYMD      TO  HCML07-YUKOSTYMD(CNT-LINE)
      *    "-"
           MOVE    "-"                TO  HCML07-NYORO(CNT-LINE)
      *    ͭ�����ֽ�λ��
           MOVE    TNS-YUKOEDYMD      TO  HCML07-YUKOEDYMD(CNT-LINE)
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
       330-CSV-HEN-SEC             SECTION.
      *
           ADD     1               TO  SUM-KENSU
      *
           INITIALIZE                  TENSUCSV-REC
           MOVE    TNS-HOSPNUM     TO  CSV-HOSPNUM
           MOVE    TNS-SRYCD       TO  CSV-SRYCD
           MOVE    TNS-YUKOSTYMD   TO  CSV-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD   TO  CSV-YUKOEDYMD
           MOVE    TNS-NAME        TO  CSV-NAME
           MOVE    TNS-TEN         TO  CSV-TEN
      *    �ݸ�Ŭ��
           EVALUATE    TNS-HKNTEKKBN
               WHEN    "0"
                   MOVE    SPACE          TO  CSV-HKNTEK
               WHEN    "1"
                   MOVE    "�ݸ�Ŭ��"     TO  CSV-HKNTEK
               WHEN    "2"
                   MOVE    "�ݸ�Ŭ�ѳ�"   TO  CSV-HKNTEK
           END-EVALUATE
      *    ���̶�ʬ
      *     EVALUATE    TNS-BUIKBN
      *         WHEN    "1"
      *             MOVE    "Ƭ��"         TO  CSV-BUI
      *         WHEN    "2"
      *             MOVE    "����"         TO  CSV-BUI
      *         WHEN    "3"
      *             MOVE    "�ͻ�"         TO  CSV-BUI
      *         WHEN    "5"
      *             MOVE    "����"         TO  CSV-BUI
      *         WHEN    "6"
      *             MOVE    "ʢ��"         TO  CSV-BUI
      *         WHEN    "7"
      *             MOVE    "����"         TO  CSV-BUI
      *         WHEN    "8"
      *             MOVE    "�ò���"       TO  CSV-BUI
      *         WHEN    "0"
      *             MOVE    "����¾"       TO  CSV-BUI
      *     END-EVALUATE
           EVALUATE    TNS-TENSIKIBETU
               WHEN    0
                   MOVE    "��ȴ��"       TO  CSV-BUI
               WHEN    4
                   MOVE    "�ǹ���"       TO  CSV-BUI
           END-EVALUATE
      *
           MOVE    TNS-GAITENTTLKBN   TO  CSV-GAITEN 
           MOVE    TNS-NYUTENTTLKBN   TO  CSV-NYUTEN
           MOVE    TNS-SSTKIJUNCD(1)  TO  CSV-KRM1
           MOVE    TNS-SSTKIJUNCD(2)  TO  CSV-KET1
           MOVE    TNS-SSTKIJUNCD(3)  TO  CSV-KRM2
           MOVE    TNS-SSTKIJUNCD(4)  TO  CSV-KET2
           MOVE    TNS-SSTKIJUNCD(5)  TO  CSV-KRM3
           MOVE    TNS-SSTKIJUNCD(6)  TO  CSV-KET3
           MOVE    TNS-SSTKIJUNCD(7)  TO  CSV-KRM4
           MOVE    TNS-SSTKIJUNCD(8)  TO  CSV-KET4
      *
           MOVE    ","                 TO  CSV-FILLER1
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
                                           CSV-FILLER16
                                           CSV-FILLER17
           WRITE   TENSUCSV-REC
      *
           PERFORM 900-DBFETCH-SEC
      *
           .
       330-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�ãӣֽ���
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
           IF      CNT-LINE        =   ZERO
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
           MOVE    "HCML07.red"        TO  SPRT-PRTID
           MOVE    "�����ޥ�������ɽ������"
                                       TO  SPRT-TITLE
           MOVE    HCML07              TO  SPRT-PRTDATA
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
           DISPLAY "*** ORHCML10 END ***"
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
      *    �����Ѵ�����
      *****************************************************************
       800-HENKAN-SEC                 SECTION.
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
       800-HENKAN-EXT.
           EXIT.
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
      *    �����ޥ��������� 
      *****************************************************************
       900-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    "095000000"         TO  TNS-SRYCD
           MOVE    "096999999"         TO  TNSUP-SRYCD 
      *
           MOVE    WRK-PARA-MSTYMD     TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
           EVALUATE    WRK-PARA-CYUKBN
               WHEN    1  
                   EVALUATE    WRK-PARA-SYUKBN        
                       WHEN    1
                           MOVE    "key12s"    TO  WRK-PATH 
                       WHEN    2
                           MOVE    "key12m"    TO  WRK-PATH 
                       WHEN    3
                           MOVE    "key12k"    TO  WRK-PATH
                   END-EVALUATE 
               WHEN    2 
               WHEN    3
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
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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