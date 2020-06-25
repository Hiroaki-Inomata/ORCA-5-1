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
       PROGRAM-ID.             ORCBTPRCVR.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �������
      *  ����ݡ��ͥ��̾  : �����ղþ�������Ѥ�ϳ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  14/04/05    NACL�ݰ�ƣ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    ��ǧ�����ʽġ�
      **** COPY    "HCMSL80.INC".
      *    ���顼�����ʲ���
      **** COPY    "HCMSL55.INC".
      *    Ģɼ
           COPY    "HCPOM2.INC".
      *
           COPY    "CPRECEERR.INC".
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-PAGE1               PIC 9(05).
           03  CNT-OK                  PIC 9(05).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-P                   PIC 9(04).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH             PIC 9(02).
               05  SYS-TMM             PIC 9(02).
               05  SYS-TSS             PIC 9(02).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-SRYYMD         PIC X(08).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYSYMDG             PIC X(22).
           03  WRK-MAE-SRYYMD          PIC X(08).
           03  WRK-STR-SRYYMD          PIC X(08).
           03  WRK-HEN-MAX             PIC 9(04).
      *
      *    �ꥹ���ΰ�
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1N.
               05  FILLER              PIC X(05)   VALUE   SPACE.
               05  FILLER              PIC X(24)   VALUE   
                   "���������ղð��Ѥ�������".
               05  PRT-TAITOL          PIC X(20).
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE
                   "��������".
               05  PRT-SRYYMD          PIC X(22).
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(82)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "P.".
               05  PRT-PAGE            PIC ZZ9.
           03  LST-PRT-HEAD3.
               05  FILLER              PIC X(12)   VALUE
                   "������".
               05  FILLER              PIC X(12)   VALUE
                   "ͭ��������".
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  FILLER              PIC X(46)   VALUE
                   "̾��".
      *
           03  LST-PRT-TAIL1.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "���������".
               05  PRT-UPDCNT          PIC ZZ,ZZ9.
               05  FILLER              PIC X(04)   VALUE
                   "����".
      *
       01  LST-PRT-BODY.
           03  PRT-SRYCD               PIC X(09).
           03  FILLER                  PIC X(02).
           03  PRT-EDYMD               PIC X(09).
      *
       01  LST-PRT-BODYN.
           03  PRT-F1                  PIC X(26).
           03  PRT-NAME                PIC X(70).
      *
       01  LINE-MAX                    PIC 9(03)   VALUE   69.
      *
      *    ����
       01  WRK-PRTHEN-AREA.
      *    Ⱦ��
           03  WRK-TBL-REC             PIC X(90)   OCCURS   800.
      *    ����
           03  WRK-TBL-REC2            PIC X(108)  OCCURS   800.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  FILLER          PIC X(01)   VALUE   "(".
               05  WRK-THH         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-TMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ")".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ����ղå�����
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    �����ޥ����ղå����ɥ��
       01  HZN-TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC"    REPLACING
                                       //TNSP-// BY //HZN-TNSP-//.
      *    �����ޥ���
           COPY    "CPTENSU.INC".
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
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
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
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾�����ѥ�
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
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
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  MCPAREA
           MOVE    SPACE           TO  WRK-PRTHEN-AREA
           INITIALIZE                  LST-PRT-BODY
           INITIALIZE                  LST-PRT-BODYN
           MOVE    ZERO            TO  CNT-AREA
           INITIALIZE                  RECEERR
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
                                       WRK-PARA-SRYYMD
           END-UNSTRING
      *
      *    �ӣУ�����
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE  SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBTPRCVR"        TO  JOB-PGID
           MOVE    "�����ղð����Ѥ�"  TO  JOB-SHELLMSG
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    ����ͥ��å�
           PERFORM 110-SYOKI-SET-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ͥ��å�
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    �����ƥ����ե��å�
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    ������
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    ��������
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SYS-THH             TO  WRK-THH
           MOVE    SYS-TMM             TO  WRK-TMM
      ***  MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *�ӣУ�����
      *    ������
           IF      WRK-PARA-SRYYMD     =   SPACE
              MOVE    "20140401"           TO  WRK-PARA-SRYYMD
           END-IF
           IF      WRK-PARA-SRYYMD     <   "20140401"
              MOVE    "20140401"           TO  WRK-PARA-SRYYMD
           END-IF
           MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           MOVE    WRK-PARA-SRYYMD     TO  WRK-STR-SRYYMD
      *
      *    ������
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-STR-SRYYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  PRT-SRYYMD
      *
      *    ������������
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-STR-SRYYMD      TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-MAE-SRYYMD
           ELSE
               MOVE    "�����������������顼�Ǥ��������Ǥ��ޤ���"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO      TO      110-SYOKI-SET-EXT
           END-IF
      *
           MOVE    ALL "*"             TO  SPA-TERMID
      *    ���ڥ졼���ɣ�
           MOVE    LNK-PRTKANRI-OPID   TO  SPA-OPID
           INSPECT SPA-OPID            REPLACING  ALL " "  BY  "*"
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ��������
      *    �����������������¤������ղä�����ʽ�λ����������
      *    ���ο��ť����ɤ����������ͭ���������ղä�̵ͭ���ǧ
      *    ̵�����
      *    ���ο��ť����ɤ����������ͭ����������̵ͭ���ǧ
      *    �ʳ��������������Ǥ��뤳�ȡ�
      *    �����ޥ�����ͭ�����֤������ղä����
      *
           INITIALIZE  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
      *
           PERFORM UNTIL    (FLG-TENSUPLUS =   1)
      *        ����������������λ���Υ쥳����
               IF      TNSP-YUKOEDYMD      =   WRK-MAE-SRYYMD
                   MOVE    TENSUPLUS-REC       TO  HZN-TENSUPLUS-REC
                   PERFORM 2001-TENSUPLUS-CHECK-SEC
               END-IF
               MOVE    ZERO                TO  FLG-TENSUPLUS
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-TENSUPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM 2903-END-HENSYU-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ղåޥ�����������
      *****************************************************************
       2001-TENSUPLUS-CHECK-SEC    SECTION.
      *
      *    ��������ͭ���ʥ쥳���ɤ�¸�ߤ��ǧ
           MOVE    WRK-STR-SRYYMD      TO  TNSP-YUKOSTYMD
           MOVE    WRK-STR-SRYYMD      TO  TNSP-YUKOEDYMD
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSUPLUS       =   1
               PERFORM 20011-TENSU-CHECK-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 20012-TENSUPLUS-COPY-SEC
               END-IF
           END-IF
           .
       2001-TENSUPLUS-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ�����������
      *****************************************************************
       20011-TENSU-CHECK-SEC       SECTION.
      *
      *    ��������ͭ���ʥ쥳���ɤ�¸�ߤ��ǧ
           INITIALIZE  TNS-TENSU-REC
           MOVE    HZN-TNSP-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    HZN-TNSP-SRYCD      TO  TNS-SRYCD
           MOVE    WRK-STR-SRYYMD      TO  TNS-YUKOSTYMD
           MOVE    WRK-STR-SRYYMD      TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           ELSE
               INITIALIZE  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               IF      TNS-YUKOSTYMD   NOT =   WRK-STR-SRYYMD
                   INITIALIZE  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           END-IF
           .
       20011-TENSU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ղð����Ѥ���������
      *****************************************************************
       20012-TENSUPLUS-COPY-SEC    SECTION.
      *
           MOVE    HZN-TENSUPLUS-REC   TO  TENSUPLUS-REC
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    SMCNDATE-YMD        TO  TNSP-CREYMD
                                           TNSP-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           MOVE    SPACE               TO  WRK-RECEERR
           IF      MCP-RC              NOT =   ZERO
               DISPLAY "ORCBTPRCVR TENSUPLUS INS ERR:" MCP-RC
                           ",KEY:" TNSP-KEY
               MOVE    SPACE               TO  WRK-RECEERR
               STRING  "�����ղ��ɲäǥ��顼�Ȥʤ�ޤ�����"
                       "������λ���ޤ���"
                                   DELIMITED  BY  SIZE
                                   INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-RECEERR         =   SPACE
               ADD     1               TO  CNT-OK
               PERFORM 700-PTNUM-PRINT-SEC
           END-IF
           .
       20012-TENSUPLUS-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������Խ���������
      *****************************************************************
       2903-END-HENSYU-SEC         SECTION.
      *
           IF      IDX-P               =   ZERO
               PERFORM 7001-PRT-INIT-SEC
           END-IF
      *    ��ǧ���
           ADD     3                   TO  IDX-P
           IF      IDX-P               >   LINE-MAX
      *        ��������
               PERFORM 400-HCPOM2-PRINT-SEC
               MOVE    1               TO  IDX-P
           END-IF
      *
           MOVE    CNT-OK              TO  PRT-UPDCNT
           MOVE    LST-PRT-TAIL1       TO   HCPOM2-MOJI (IDX-P)
      *    ��������
           PERFORM 400-HCPOM2-PRINT-SEC
           .
       2903-END-HENSYU-EXIT.
           EXIT.
      *
      *****************************************************************
      *    ��Ͽ��ǧ��å���������
      *****************************************************************
       700-PTNUM-PRINT-SEC         SECTION.
      *
           IF      CNT-OK              =   1
               PERFORM 7001-PRT-INIT-SEC
           END-IF
      *
           INITIALIZE  WRK-PRTHEN-AREA
           MOVE    ZERO                TO  WRK-HEN-MAX
           INITIALIZE  LST-PRT-BODY
           INITIALIZE  LST-PRT-BODYN
      *
           MOVE    HZN-TNSP-SRYCD      TO  PRT-SRYCD
      *    ������
           MOVE    HZN-TNSP-YUKOSTYMD  TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-EDYMD
      *
           MOVE    TNS-NAME            TO  PRT-NAME
           ADD     1                   TO  WRK-HEN-MAX
           MOVE    LST-PRT-BODY        TO  WRK-TBL-REC  (WRK-HEN-MAX)
           MOVE    LST-PRT-BODYN       TO  WRK-TBL-REC2 (WRK-HEN-MAX)
      *    �Խ�����
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-HEN-MAX
               IF      IDX-P               >=  LINE-MAX
      *            ��������
                   PERFORM 400-HCPOM2-PRINT-SEC
      *            ���Ф��Խ�
                   PERFORM 7001-PRT-INIT-SEC
               END-IF
               ADD     1                   TO  IDX-P
               MOVE    WRK-TBL-REC (IDX)   TO  HCPOM2-MOJI  (IDX-P)
               MOVE    WRK-TBL-REC2(IDX)   TO  HCPOM2-MOJI2 (IDX-P)
           END-PERFORM
      *
           INITIALIZE  WRK-PRTHEN-AREA
           MOVE    ZERO                TO  WRK-HEN-MAX
           .
       700-PTNUM-PRINT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    �����������
      *****************************************************************
       7001-PRT-INIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-P
           MOVE    SPACE               TO  HCPOM2
      *    ���Ф��Խ�
           ADD     1                   TO  CNT-PAGE1
           MOVE    CNT-PAGE1           TO  PRT-PAGE
           MOVE    LST-PRT-HEAD1N      TO  HCPOM2-MOJI (1)
           MOVE    LST-PRT-HEAD1       TO  HCPOM2-MOJI2 (1)
           MOVE    LST-PRT-HEAD3       TO  HCPOM2-MOJI (3)
           MOVE    4                   TO  IDX-P
           .
       7001-PRT-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       400-HCPOM2-PRINT-SEC        SECTION.
      *
           INITIALIZE  ORCSPRTAREA
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
           MOVE    "HCPOM2.red"        TO  SPRT-PRTID
           MOVE    "���������ղð����Ѥ�������ǧ�ꥹ��"
                                       TO  SPRT-TITLE
           MOVE    HCPOM2              TO  SPRT-PRTDATA
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
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-HCPOM2-PRINT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      ***  PERFORM 3001-PARA-DEL-SEC
      *
           DISPLAY "*** ORCBTPRCVR OK :"  CNT-OK   " ***"
           DISPLAY "*** ORCBTPRCVR END ***"
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    CNT-OK              TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
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
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ����ִ������Ͻ���
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               CALL    "ORCSJOB"           USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
                                       OR  ZERO
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ����ղþ����ɹ�
      *****************************************************************
       910-TENSUPLUS-READ-SEC      SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               MOVE    ZERO                TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           .
       910-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ�
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡���������
      *****************************************************************
       990-DBCOMMIT-SEC                SECTION.
      *
      *    �ģ¡�������������
           PERFORM 900-DBDISCONNECT-SEC
      *    �ģ¡������ץ����
           PERFORM 100-DBOPEN-SEC
           .
       990-DBCOMMIT-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.