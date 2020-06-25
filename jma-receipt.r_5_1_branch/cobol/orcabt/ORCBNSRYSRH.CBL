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
       PROGRAM-ID.             ORCBNSRYSRH.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����Ģɼ
      *  ����ݡ��ͥ��̾  : �������Ź԰پȲ�ơ��֥��������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/01/23    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ������ץե�����
           SELECT  TMP01-FILE      ASSIGN  TMP01PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TMP01.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
      *
       FILE                        SECTION.
      *
      *    ������ץե�����
       FD  TMP01-FILE.
       01  TMP01-REC.
           COPY    "CPNYUINACCT.INC"
                                   REPLACING   //NACCT-//
                                   BY          //TMP01-//.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01//
                                   BY          //TMP01//.
      *
           COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-TMP01                    PIC X(02).
           03  STS-RECEERR                 PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-TMP01                   PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-UPD                     PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-STSRYYM                 PIC X(06).
           03  WRK-EDSRYYM                 PIC X(06).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-TIME.
               05  WRK-TIME-HH             PIC 9(02).
               05  WRK-TIME-MM             PIC 9(02).
               05  WRK-TIME-SS             PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
      *
       01  KEY-AREA.
           03  KEY-SRYYM                   PIC X(06).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-SRYYM          PIC X(06).
           03  WRK-PARA-PTNUM          PIC X(20).
      *
      *
      *01  CONST-AREA.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    �������
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    �����ֹ�
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �������Ź԰پȲ�ơ��֥�
           COPY  "CPORCSS010.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    �ޥ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ****************************************************************
       PROCEDURE                   DIVISION
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
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
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
           INITIALIZE                  STS-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
           INTO                        LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-PTNUM
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "NSRYSRH"       TO  TMP01PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  TMP01PARA-TERMID
           MOVE    SPA-HOSPNUM     TO  TMP01PARA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    TMP01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  TMP01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBNSRYSRH"   TO  JOB-PGID
           MOVE    "�������Ź԰پȲ�������"
                                   TO  JOB-SHELLMSG
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    3               TO  JOB-STOPFLG
           PERFORM 800-ORCSJOB-SEC
      *
      *    �ѥ�᥿�Խ�����
           PERFORM 110-PARA-HENSYU-SEC
      *
           PERFORM 800-MCNDATE-SEC
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
           IF    ( WRK-PARA-SRYYM  =   SPACE )
               MOVE    "000000"    TO  WRK-STSRYYM
           ELSE
               MOVE    WRK-PARA-SRYYM
                                   TO  WRK-STSRYYM
           END-IF
      *
           IF    ( WRK-PARA-PTNUM  =   SPACE )
               CONTINUE
           ELSE
               PERFORM 900-PTNUM-KEY4-SEL-SEC
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( WRK-PARA-PTNUM  =   SPACE )
               PERFORM 2001-IKKATU-SEC
           ELSE
               PERFORM 2001-KOBETU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������
      *****************************************************************
       2001-IKKATU-SEC                 SECTION.
      *
           OPEN    OUTPUT  TMP01-FILE
      *
           PERFORM 900-NYUINACCT-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    NYUINACCT-REC   TO  TMP01-REC
               WRITE   TMP01-REC
      *
               PERFORM 900-NYUINACCT-KEY41-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   TMP01-FILE
      *
           OPEN    INPUT   TMP01-FILE
      *
           PERFORM 900-TMP01-READ-SEC
      *
           PERFORM UNTIL ( FLG-TMP01   NOT =   ZERO )
      *
               PERFORM 20011-SYORI-KEIKA-SEC
      *
               MOVE    NACCT-SRYYM     TO  KEY-SRYYM
               PERFORM UNTIL ( FLG-TMP01       NOT =   ZERO )
                        OR   ( NACCT-SRYYM     NOT =   KEY-SRYYM )
      *
                   PERFORM 20011-ORCSS010-SEC
      *
                   PERFORM 900-TMP01-READ-SEC
               END-PERFORM
           END-PERFORM
      *
           CLOSE   TMP01-FILE
      *
           .
       2001-IKKATU-EXT.
           EXIT.
      *****************************************************************
      *    ���̽���
      *****************************************************************
       2001-KOBETU-SEC                 SECTION.
      *
           PERFORM 900-NYUINACCT-KEY40-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               PERFORM 20011-ORCSS010-SEC
      *
               PERFORM 900-NYUINACCT-KEY40-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-KOBETU-EXT.
           EXIT.
      *****************************************************************
      *    �����в��Խ�����
      *****************************************************************
       20011-SYORI-KEIKA-SEC           SECTION.
      *
           ACCEPT  WRK-TIME        FROM    TIME
      *
           MOVE    NACCT-SRYYM         TO  WRK-YMD
           MOVE    "01"                TO  WRK-YMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
      *
      *    ���ƥå��������
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  WRK-EDTYMD3 (1:16)      DELIMITED BY SIZE
                   "ʬ��"                  DELIMITED BY SPACE
                   WRK-TIME-HH             DELIMITED BY SIZE
                   "."                     DELIMITED BY SIZE
                   WRK-TIME-MM             DELIMITED BY SIZE
                   "."                     DELIMITED BY SIZE
                   WRK-TIME-SS             DELIMITED BY SIZE
           INTO    JOB-YOBI
           END-STRING
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBNSRYSRH"       TO  JOB-PGID
           MOVE    "�������Ź԰پȲ�������"
                                       TO  JOB-SHELLMSG
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           .
       20011-SYORI-KEIKA-EXT.
           EXIT.
      *****************************************************************
      *    �������Ź԰پȲ�ơ��֥��Խ����ָƽ�
      *****************************************************************
       20011-ORCSS010-SEC              SECTION.
      *
           INITIALIZE                      SS010-AREA
           MOVE    NACCT-PTID          TO  SS010-PTID
           MOVE    NACCT-SRYYM         TO  SS010-SRYYM
           CALL    "ORCSS010"      USING   SS010-AREA
                                           SPA-AREA
           IF    ( SS010-RC            =   ZERO )
               COMPUTE CNT-UPD =   CNT-UPD +   1
           ELSE
               DISPLAY "ORCSS010 ERR [" SS010-RC "] "
                       NACCT-PTID " " NACCT-SRYYM
           END-IF
      *
           .
       20011-ORCSS010-EXT.
           EXIT.
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    �ե�������
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP01PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *    MOVE    CNT-UPD         TO  JOB-UPDCNT
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܸ��Ѵ�����
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-YMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-YMD             TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "��"
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ޥ������ռ�������
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ����ִ�������
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ������׸���
      *****************************************************************
       900-NYUINACCT-KEY40-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    PTNUM-PTID          TO  NACCT-PTID
           MOVE    WRK-STSRYYM         TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY40-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������FETCH
      *****************************************************************
       900-NYUINACCT-KEY40-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY40-FET-EXT.
           EXIT.
      *****************************************************************
      *    ������׸���
      *****************************************************************
       900-NYUINACCT-KEY41-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    WRK-STSRYYM         TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY41-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������FETCH
      *****************************************************************
       900-NYUINACCT-KEY41-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ渡������(KEY4)
      *****************************************************************
       900-PTNUM-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    WRK-PARA-PTNUM      TO  PTNUM-PTNUM
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTNUM
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ������ץե�����
      *****************************************************************
       900-TMP01-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP01
      *
           READ    TMP01-FILE
           AT  END
               MOVE    1           TO  FLG-TMP01
           NOT AT  END
               MOVE    TMP01-REC   TO  NYUINACCT-REC
           END-READ
      *
           .
       900-TMP01-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH�ϹԤ�ʤ�)
      *****************************************************************
       911-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ɣΣӣţңԽ���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
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
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.