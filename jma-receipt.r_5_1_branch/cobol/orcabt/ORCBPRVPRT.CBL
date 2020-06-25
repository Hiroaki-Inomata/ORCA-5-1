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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBPRVPRT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �ץ�ӥ塼DB����ΰ���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/08/01    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.00.01    NACL-ƣ��    07/10/05  �����ģ¹������Υ��顼�б�
      *  04.05.00    NACL-����    10/01/29  ��ĥ�����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *    ����
           COPY    "HCMSL80.INC".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-OUT                 PIC X(02).
           03  STS-PTHKN               PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-RECLST              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR3                PIC 9(01).
           03  FLG-HOJOKBN             PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
      *
           03  CNT-LCNT1               PIC 9(02).
           03  CNT-LCNT2               PIC 9(02).
           03  CNT-PAGE                PIC 9(02).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
      *    03  WRK-PARA-HOSPID         PIC X(24).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-LSTYMD         PIC X(08).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-MAE-TEKSTYMD        PIC X(08).
           03  WRK-MAE-TEKEDYMD        PIC X(08).
           03  WRK-MAE-HKNNUM          PIC X(03).
           03  WRK-MAE-HKNID           PIC 9(10).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNNUM              PIC X(03).
      *
           03  WRK-Z3                  PIC ZZ9.
      *
           03  WRK-LSTYMD              PIC X(08).
      *
           03  WRK-BYMD-70.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
      *
      *    Ģɼ�����ΰ�
       01  ERRMES-AREA.
           03  ERRMES-TITLE1.
               05  FILLER              PIC X(02)   VALUE  SPACE.
               05  FILLER              PIC X(60)   VALUE
               "��㡡�쥻�ץȥ����å��ꥹ�Ȱ��������".
               05  ERRMES-TITLE1-YY    PIC X(04).
               05  FILLER              PIC X(01) VALUE ".".
               05  ERRMES-TITLE1-MM    PIC X(02).
               05  FILLER              PIC X(01) VALUE ".".
               05  ERRMES-TITLE1-DD    PIC X(02).
           03  ERRMES-TITLE3.
               05  FILLER              PIC X(04) VALUE
                                                 "NO".
               05  FILLER              PIC X(38) VALUE
                                                 "ERR-MESSAGE".
               05  FILLER              PIC X(20) VALUE
                   "�����ֹ�".
               05  FILLER              PIC X(01) VALUE SPACE.
               05  FILLER              PIC X(10) VALUE SPACE.
               05  FILLER              PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(10) VALUE
                   "���ԣɣ�".
      *
      *    ���ϥ��顼�ե�����
       01  ERR-REC.
               05  ERR-NO              PIC X(05).
               05  FILLER              PIC X(01).
               05  ERR-MES             PIC X(40).
               05  FILLER              PIC X(01).
               05  ERR-PTNUM           PIC X(20).
               05  FILLER              PIC X(01).
               05  ERR-LSTYMD          PIC X(10).
               05  FILLER              PIC X(03).
               05  ERR-PTID            PIC X(10).
               05  FILLER              PIC X(03).
               05  ERR-ERRMSG          PIC X(50).
      *
      *    �Ѵ�����
       01  MEISAI-HEAD.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(60)   VALUE
               "��㡡�쥻�ץȥ����å��ꥹ�Ȱ��������".
           03  FILLER                  PIC X(10)   VALUE   SPACE.
           03  MEISAI-HEAD-YY          PIC X(04).
           03  FILLER                  PIC X(01) VALUE ".".
           03  MEISAI-HEAD-MM          PIC X(02).
           03  FILLER                  PIC X(01) VALUE ".".
           03  MEISAI-HEAD-DD          PIC X(02).
      *
       01  MEISAI-HEAD2.
           03  FILLER                  PIC X(22)   VALUE
               "�����ֹ�".
           03  FILLER                  PIC X(20)   VALUE
               "�ᡡ��̾".
           03  FILLER                  PIC X(08)   VALUE
               "����ǯ��".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(06)   VALUE   SPACE.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(12)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE
               "��������".
      *
       01  MEISAI-BODY1.
           03  MEISAI-PTNUM            PIC X(20).
           03  MEISAI-NAME             PIC X(20).
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  MEISAI-SRYYM            PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  MEISAI-MSG              PIC X(58).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ץ�ӥ塼�ģ�
           COPY    "CPPRVKANRI.INC".          
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    �����ƥ�����ʰ��ŵ��ؾ�����ܾ����
           COPY    "CPSK1001.INC".
      *
      *    �����ֹ湽����������
           COPY  "CPSK1009.INC".
      *
      *    �����ֹ��Ѵ��ޥ���
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
           COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
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
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK2.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
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
                   UNTIL       FLG-END     =   1
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-LSTYMD
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBPRVPRT"    TO  JOB-PGID
           MOVE    "�ץ�ӥ塼�����å��ꥹ�Ƚ���"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
           INITIALIZE                  SPA-AREA
      *
      *    �����ƥ����դ򣲣��������������Ȥ���
           MOVE    "20030401"      TO  SPA-SYSYMD
           IF      WRK-PARA-LSTYMD NOT NUMERIC
             MOVE    LNK-PRTKANRI-SKYYMD   TO  SPA-SYSYMD
           END-IF
      *    ������
           ACCEPT  WRK-SYMD2       FROM    DATE
           COMPUTE WRK-SYY         =   WRK-SYY2    +   2000
           MOVE    WRK-SMM2        TO  WRK-SMM
           MOVE    WRK-SDD2        TO  WRK-SDD
      *
           MOVE    WRK-SYY         TO  ERRMES-TITLE1-YY
           MOVE    WRK-SMM         TO  ERRMES-TITLE1-MM
           MOVE    WRK-SDD         TO  ERRMES-TITLE1-DD
      *
           MOVE    WRK-SYY         TO  MEISAI-HEAD-YY
           MOVE    WRK-SMM         TO  MEISAI-HEAD-MM
           MOVE    WRK-SDD         TO  MEISAI-HEAD-DD
      *
      *    ü���ɣĸ���
           MOVE    "ORCATERM"          TO  SPA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    �����ֹ湽�����������Խ�
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "�����ֹ湽���������󤬼����Ǥ��ޤ���Ǥ���"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
           PERFORM 900-PRVKANRI-INV-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           ADD     1                   TO  CNT-IN
           MOVE    1                   TO  CNT-LCNT1
      *
           INITIALIZE                  HCMSL80
           MOVE    MEISAI-HEAD         TO  HCMSL80-MOJI (CNT-LCNT1)
           ADD     2                   TO  CNT-LCNT1
           MOVE    MEISAI-HEAD2        TO  HCMSL80-MOJI (CNT-LCNT1)
      *
           PERFORM             UNTIL   FLG-END     =   1
                               OR      CNT-LCNT1   >=  80
      *
               ADD     1                   TO  CNT-LCNT1
      *
               MOVE    SPACE               TO  MEISAI-BODY1
      *
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    PRVKANRI-PTID       TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               MOVE    SPTID-PTNUM         TO  MEISAI-PTNUM
      * 
               INITIALIZE                          PTINF-REC
               MOVE    SPA-HOSPNUM         TO      PTINF-HOSPNUM
               MOVE    PRVKANRI-PTID       TO      PTINF-PTID
grpsys*        MOVE SPA-HOSPNUM TO -HOSPNUM
               MOVE    PTINF-REC           TO      MCPDATA-REC
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
                   INSPECT PTINF-NAME      REPLACING   ALL "  "
                                                       BY  "��"
                   MOVE    PTINF-NAME      TO  MEISAI-NAME
                   IF    ( SPA-ENCODING    =   2 )
                       INITIALIZE              ORCSKANACHK2AREA
                       MOVE    "1"         TO  KANACHK2-SYORI
                       MOVE    MEISAI-NAME TO  KANACHK2-MAE-INPUT
                       CALL    "ORCSKANACHK2"  USING
                                               ORCSKANACHK2AREA
                       MOVE    KANACHK2-MAE-INPUT
                                           TO  MEISAI-NAME
                   END-IF
               END-IF 
      *
               MOVE    PRVKANRI-SRYYM      TO  MEISAI-SRYYM
               MOVE    PRVKANRI-MEMO       TO  MEISAI-MSG  
      *
               MOVE    MEISAI-BODY1        TO  HCMSL80-MOJI (CNT-LCNT1)
      *
               MOVE    "tbl_prvkanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
             IF  MCP-RC                  NOT =   ZERO
               MOVE    1                   TO  FLG-END
             ELSE
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
             END-IF
           END-PERFORM 
      *
      *    Ģɼ��������
           PERFORM 400-PRINT-OUT-SEC
           IF  MCP-RC                  NOT =   ZERO
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ��������
      *****************************************************************
       400-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
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
           MOVE   "HCMSL80.red"        TO  SPRT-PRTID
           MOVE    "�ץ�ӥ塼�����å��ꥹ�Ƚ���"
                                       TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBPRVPRT PRVKANRI:"       CNT-IN
           DISPLAY "*** ORCBPRVPRT END ***"
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ����ɤ߹���
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
      *    �ģ£ӣţ̣ţãԽ���
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
      *    �ģ£ƣţԣãȽ���
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
      *    �ơ��֥륢����������
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
      *    �ơ��֥륯����������
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
      *     �ץ�ӥ塼�ģ��ɤ߹���
      *****************************************************************
       900-PRVKANRI-INV-SEC            SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_prvkanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               MOVE    MCPDATA-REC     TO   PRVKANRI-REC    
           ELSE 
                   MOVE    1               TO  FLG-END
           END-IF
           .
       900-PRVKANRI-INV-EXT.
           EXIT.
      *      
      *****************************************************************
      *     �ץ�ӥ塼�ģ��ɤ߹���
      *****************************************************************
       900-PRVKANRI-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PRVKANRI-REC
                   MOVE    ZERO            TO  FLG-END
           ELSE  
                   MOVE    SPACE           TO  PRVKANRI-REC
                   MOVE    1               TO  FLG-END
           END-IF 
           .
       900-PRVKANRI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    ���Ծ����ɹ�
      *****************************************************************
       900-PTINF-READ-SEC           SECTION.
      *       
           MOVE    "DBSELECT"          TO      MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =       ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM 1001-DBSTART-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣԣ��ңԽ���
      *****************************************************************
       1001-DBSTART-SEC        SECTION.
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       1001-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣԣ��ңԽ���
      *****************************************************************
       9001-DBDBCOMMIT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       9001-DBDBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           IF      FLG-ERR         =   ZERO
               MOVE    "DBCOMMIT"      TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF.
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.