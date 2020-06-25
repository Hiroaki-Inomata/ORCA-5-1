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
       PROGRAM-ID.             ORCBZ002.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ģɼ
      *  ����ݡ��ͥ��̾  : ����ƣ���������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/07/04    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  04/12/15  ���������ɲ�
      * 03.05.00     NACL-¿��Ǽ  07/05/24  ���롼�׿����б�
      * 03.05.00     NACL-¿��Ǽ  07/06/26  ���襫��ư������������ɲ�
      * 03.05.00     NACL-¿��Ǽ  08/08/04  ���Ի��굡ǽ�ɲ�
      * 04.08.00     NACL-¿��Ǽ  14/07/17  ����ե������б�
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
      *D   ���顼�ե����� ̾���ΰ� 
      *    COPY    "CPCOMMONDAT2.INC"
      *                            REPLACING  //RECE01PARA//
      *                            BY         //RECEERR//.
      *D   03  FILLER              PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCM33.INC".
      *
      *    ����ե������б�
           COPY    "CPRECEERR.INC".
      *
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-BTPARA              PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(03).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-BZ001               PIC 9(05).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
     ***** 03  RECEERR                 PIC X(100).
           03  WRK-PARA-SRYYM          PIC X(06).
           03  WRK-PARA-SRYYM-END      PIC X(06).
           03  WRK-PARA-SRYKA          PIC X(02).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-PTNUM          PIC X(20).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-PTNUM               PIC X(20).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    ���ŵ��ؾ���
           COPY    "CPSK1009.INC".
      *    Ģɼ�Խ���ʬ����
           COPY    "CPSK1030.INC".
      *
      *    ���ԥޥ���
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
      *01  PRTKANRI-REC.
      *    COPY    "CPPRTKANRI.INC".
      *
      *    �����ѥǡ���
      *01  PRTDATA-REC.
      *    COPY    "CPPRTDATA.INC".
      *
      *    �Хå��ѥ�᥿
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTOANY.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ����ƣ���極��
           COPY    "CPORCHC33.INC".
      *
      *    �����Ѵ�
      *    COPY    "CPORCSKANACHK.INC".
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
      *    �ե��������ѥ�᥿
      *    COPY    "CPORCSFDEL.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾�����ѥ�
      *    COPY    "CPORCSPRTNM.INC".
      *
      *    ����ե�����̾����
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *    COPY    "ORCA-DBPATH".
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           MOVE    ZERO            TO  CNT-AREA
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
                                       WRK-PARA-SRYYM
                                       WRK-PARA-SRYYM-END
                                       WRK-PARA-SRYKA
                                       WRK-PARA-NYUGAIKBN
                                       WRK-PARA-PTNUM
           END-UNSTRING
      *
      *    �ӣУ�����
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ����ե�����̾����
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBZ002"          TO  JOB-PGID
           MOVE    "����ƣ�����ɸ��˰�����"
                                       TO  JOB-SHELLMSG
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *DDD MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *    MOVE    LNK-PRTKANRI-TERMID
      *                            TO  RECEERR-TERMID
      *DDD MOVE    SPA-HOSPNUM     TO  RECEERR-HOSPNUM
      *
      *    ����ͥ��å�
           PERFORM 110-SYOKI-SET-SEC
      *
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
      *    ȯ����
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
      *
      *    �ӣУ�����
      **** MOVE    WRK-PARA-HOSPNUM     TO  SPA-HOSPNUM
      *    ���Ų�
           IF      WRK-PARA-SRYKA      =   SPACE
               MOVE    ZERO            TO  WRK-PARA-SRYKA
           END-IF
      *
      *    �����ֹ湽�����������Խ�
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           END-IF
      *
      *    ������ʬ
           IF      WRK-PARA-NYUGAIKBN  NOT =   "1"
               MOVE    "2"             TO  WRK-PARA-NYUGAIKBN
           END-IF
      *
      *    Ģɼ�Խ���ʬ
           MOVE    SPACE               TO  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1030-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1030-REC
           ELSE
               INITIALIZE                  SYS-1030-REC
           END-IF
           IF      SYS-1030-KARUTE3KBN     =   SPACE
               MOVE    ZERO                TO  SYS-1030-KARUTE3KBN
           END-IF
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    �����ѥ�᥿�Խ�
           INITIALIZE                      ORCSPRTAREA
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
           IF      WRK-PARA-NYUGAIKBN  =   "2"
               MOVE    "HCM33.red"         TO  SPRT-PRTID
               MOVE    "����ƣ����ʳ���˰��"
                                           TO  SPRT-TITLE
           ELSE
               MOVE    "HCM33N.red"        TO  SPRT-PRTID
               MOVE    "����ƣ����������˰��"
                                           TO  SPRT-TITLE
           END-IF
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           IF    ( WRK-PARA-PTNUM  NOT =   "*" )
               MOVE    WRK-PARA-PTNUM  TO  WRK-PTNUM
               IF      WRK-PARA-NYUGAIKBN  =   "1"
      *            ����
                   PERFORM 2001-NYUIN-SYORI-SEC
               ELSE
      *            ����
                   PERFORM 2002-GAIRAI-SYORI-SEC
               END-IF
           ELSE
               PERFORM 2003-SHOKAI-RENKEI-SEC
           END-IF
      *
             .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       2002-GAIRAI-SYORI-SEC                 SECTION.
      *
      *    �����Ԥ��оݤȤ���
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  PTINF-UPYMD(1:6)
           MOVE    "01"                TO  PTINF-UPYMD(7:2)
           MOVE    WRK-PARA-SRYYM-END  TO  PTINF-CREYMD(1:6)
           MOVE    "31"                TO  PTINF-CREYMD(7:2)
      *
           IF    ( WRK-PTNUM      =   SPACE )
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    WRK-PTNUM  TO  PTINF-VPTNUM
               MOVE    "key20"         TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
           PERFORM
                   UNTIL     (FLG-PTINF    =   1  )  OR
                             (FLG-END      =   1  )
      *
               MOVE    PTINF-PTID          TO  SPRT-PTID
      *
               INITIALIZE                  ORCHC33AREA
               MOVE    "2"                 TO  ORCHC33-KBN
               MOVE    PTINF-PTID          TO  ORCHC33-PTID
               MOVE    PTINF-NAME          TO  ORCHC33-NAME
               MOVE    PTINF-BIRTHDAY      TO  ORCHC33-BIRTHDAY
               MOVE    PTINF-SEX           TO  ORCHC33-SEX
               MOVE    WRK-PARA-SRYKA      TO  ORCHC33-SRYKA
               MOVE    WRK-PARA-SRYYM      TO  ORCHC33-SRYYM
               MOVE    WRK-PARA-SRYYM-END  TO  ORCHC33-SRYYM-END
               MOVE    WRK-PARA-NYUGAIKBN  TO  ORCHC33-NYUGAIKBN
      *        ���ѥ�᥿
               MOVE    ORCSPRTAREA         TO  ORCHC33-PRTAREA
      *        ����ƣ����ʳ��������
               MOVE    SYS-1030-KARUTE3KBN
                                           TO  ORCHC33-KARUTE3KBN
      *        ����
               CALL    "ORCHC33"           USING
                                           SPA-AREA
                                           ORCHC33AREA
               IF      ORCHC33-RC          =   ZERO
                   ADD     ORCHC33-PAGE        TO  CNT-PAGE
      ******   ELSE
      ******       PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               IF      ORCHC33-RC          =   9
      *            �����۾ｪλ
                   MOVE    1               TO  FLG-END
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-END             =   1
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
             .
       2002-GAIRAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       2001-NYUIN-SYORI-SEC                 SECTION.
      *
      *    �����Ԥ��оݤȤ���
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  PTINF-UPYMD(1:6)
           MOVE    "01"                TO  PTINF-UPYMD(7:2)
           MOVE    WRK-PARA-SRYYM-END  TO  PTINF-CREYMD(1:6)
           MOVE    "31"                TO  PTINF-CREYMD(7:2)
      *
           IF    ( WRK-PTNUM       =   SPACE )
               MOVE    "key15"         TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    WRK-PTNUM       TO  PTINF-VPTNUM
               MOVE    "key21"         TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
           DISPLAY "FLG-PTINF =" FLG-PTINF
           PERFORM
                   UNTIL     (FLG-PTINF    =   1  )  OR
                             (FLG-END      =   1  )
      *
               MOVE    PTINF-PTID          TO  SPRT-PTID
      *
               INITIALIZE                  ORCHC33AREA
               MOVE    "2"                 TO  ORCHC33-KBN
               MOVE    PTINF-PTID          TO  ORCHC33-PTID
               MOVE    PTINF-NAME          TO  ORCHC33-NAME
               MOVE    PTINF-BIRTHDAY      TO  ORCHC33-BIRTHDAY
               MOVE    PTINF-SEX           TO  ORCHC33-SEX
               MOVE    WRK-PARA-SRYKA      TO  ORCHC33-SRYKA
               MOVE    WRK-PARA-SRYYM      TO  ORCHC33-SRYYM
               MOVE    WRK-PARA-SRYYM-END  TO  ORCHC33-SRYYM-END
               MOVE    WRK-PARA-NYUGAIKBN  TO  ORCHC33-NYUGAIKBN
      *        ���ѥ�᥿
               MOVE    ORCSPRTAREA         TO  ORCHC33-PRTAREA
      *        ����
               CALL    "ORCHCN33"          USING
                                           SPA-AREA
                                           ORCHC33AREA
      *
               IF      ORCHC33-RC          =   ZERO
                   ADD     ORCHC33-PAGE        TO  CNT-PAGE
      **           DISPLAY "ORCHC33-PAGE =" ORCHC33-PAGE
      ******   ELSE
      ******       PERFORM 500-ERR-HENSYU-SEC
               END-IF
               IF      ORCHC33-RC          =   9
      *            �����۾ｪλ
                   MOVE    1               TO  FLG-END
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-END             =   1
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
             .
       2001-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Ȳ�Ϣ�Ƚ���
      *****************************************************************
       2003-SHOKAI-RENKEI-SEC             SECTION.
      *
           PERFORM 900-BTPARA-KEY8-SEL-SEC
           MOVE    BTPARA-INFO-PARA    TO  QTOANY-AREA
      *
           PERFORM UNTIL ( FLG-BTPARA  NOT =   ZERO )
      *
               PERFORM 900-PTNUM-KEY-SEL-SEC
               IF    ( FLG-PTNUM       =   ZERO )
                   MOVE    PTNUM-PTNUM TO  WRK-PTNUM
                   IF      WRK-PARA-NYUGAIKBN  =   "1"
      *                ����
                       PERFORM 2001-NYUIN-SYORI-SEC
                   ELSE
      *                ����
                       PERFORM 2002-GAIRAI-SYORI-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-BTPARA-KEY8-FET-SEC
               MOVE    BTPARA-INFO-PARA
                                       TO  QTOANY-AREA
      *
           END-PERFORM
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
             .
       2003-SHOKAI-RENKEI-EXT.
           EXIT.
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-PAGE            =   ZERO
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBZ002 PAGE:" CNT-PAGE  " ***"
           DISPLAY "*** ORCBZ002 END ***"
      *     
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
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
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
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
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
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
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ���ԥޥ������ɤ߹���
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ��ɤ߹���
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    QTOANY-PTID     TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptnum" TO  MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC      =   ZERO
                   MOVE    MCPDATA-REC
                                   TO  PTNUM-REC
               END-IF
           ELSE
               MOVE    1           TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �Хå��ѥ�᥿����
      *****************************************************************
       900-BTPARA-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BTPARA
      *
           INITIALIZE                      BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    "ORCBQL01"          TO  BTPARA-SHELLID
           MOVE    2                   TO  BTPARA-JOBID
           MOVE    2                   TO  BTPARA-DSP-RENNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-BTPARA-KEY8-FET-SEC
           ELSE
               INITIALIZE                  BTPARA-REC
               MOVE    1               TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �Хå��ѥ�᥿�ɹ�
      *****************************************************************
       900-BTPARA-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BTPARA
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE                  BTPARA-REC
               MOVE    1               TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-FET-EXT.
           EXIT.
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
      *
           .
       910-DBSELECT-EXT.
           EXIT.
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
      *
           .
      *
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
      *
           .
       990-DBCLOSE-EXT.
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