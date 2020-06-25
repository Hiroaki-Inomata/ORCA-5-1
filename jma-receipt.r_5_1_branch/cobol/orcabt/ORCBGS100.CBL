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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBGS100.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ���ݸ�����������å�ɽ
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  11/01/13    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  BGS100ERR-FILE  ASSIGN  BGS100P-ERRFNAME
                                   FILE    STATUS  IS  STS-BGS100ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  BGS100ERR-FILE.
       01  BGS100ERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT3.INC".
      *
       01  STS-AREA.
           03  STS-BGS100ERR       PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-ERRMSG          PIC X(200).
           03  WRK-PARA-NAME       PIC X(25).
           03  WRK-PARA            PIC X(100).
      *
       01  TPARA-AREA.
           03  TPARA-DAISAN-SAKI   PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-H2204         PIC X(06)   VALUE "201004".
      *
      *    �ѥ�᥿�ΰ�
       01  BGS100P-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBGS100PARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    �ץ�����४�ץ�����Խ��ѥ�᥿
           COPY    "CPORCSPRGOPTION.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
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
           IF    ( FLG-ERR         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP   RUN
      *
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              BGS100P-AREA
                                   SPA-AREA
                                   STS-AREA
                                   WRK-AREA
                                   FLG-AREA
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
                                               BGS100P-JOBID
                                               BGS100P-SHELLID
                                               BGS100P-HOSPNUM
                                               BGS100P-ERRFNAME
                                               BGS100P-FORMKBN
                                               BGS100P-SRYYM
                                               BGS100P-NYUGAIKBN
                                               BGS100P-EDITKBN
                                               BGS100P-ETCGETKBN
           END-UNSTRING
      *
           MOVE    BGS100P-HOSPNUM     TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
      *
           MOVE    SPA-HOSPNUM         TO  TMPFLPARA-HOSPNUM
           MOVE    SPA-TERMID          TO  TMPFLPARA-TERMID
           MOVE    "BGS100F"           TO  TMPFLPARA-FILE-ID
      *
           MOVE    1                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-SKYFNAME
      *
           MOVE    2                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-KOHFNAME
      *
           MOVE    3                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-HKNJAFNAME
      *
           MOVE    4                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-ETCFNAME
      *
           MOVE    5                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-MEIFNAME
      *
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE   BGS100P-SKYFNAME     TO  SGETTEMP-BASENAMES (1)
           MOVE   BGS100P-KOHFNAME     TO  SGETTEMP-BASENAMES (2)
           MOVE   BGS100P-HKNJAFNAME   TO  SGETTEMP-BASENAMES (3)
           MOVE   BGS100P-ETCFNAME     TO  SGETTEMP-BASENAMES (4)
           MOVE   BGS100P-MEIFNAME     TO  SGETTEMP-BASENAMES (5)
           MOVE   BGS100P-ERRFNAME     TO  SGETTEMP-BASENAMES (6)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BGS100P-SKYFNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  BGS100P-KOHFNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                   TO  BGS100P-HKNJAFNAME
           MOVE    SGETTEMP-FULLNAMES (4)
                                   TO  BGS100P-ETCFNAME
           MOVE    SGETTEMP-FULLNAMES (5)
                                   TO  BGS100P-MEIFNAME
           MOVE    SGETTEMP-FULLNAMES (6)
                                   TO  BGS100P-ERRFNAME
      *
           EVALUATE    BGS100P-FORMKBN
           WHEN    "1"
               MOVE    "�ݸ�����������å�ɽ�ʼ��ݡˡ�ɸ��Ģɼ��"
                                       TO  BGS100P-SHELLMSG
           WHEN    "2"
               MOVE    "�ݸ�����������å�ɽ�ʹ��ݡˡ�ɸ��Ģɼ��"
                                       TO  BGS100P-SHELLMSG
           WHEN    "3"
               MOVE    "�ݸ�����������å�ɽ�ʸ������ԡˡ�ɸ��Ģɼ��"
                                       TO  BGS100P-SHELLMSG
           WHEN    "9"
               MOVE    "�ݸ�����������å�ɽ������ɽ�ˡ�ɸ��Ģɼ��"
                                       TO  BGS100P-SHELLMSG
           WHEN    OTHER
               MOVE    "�ݸ�����������å�ɽ��ɸ��Ģɼ��"
                                       TO  BGS100P-SHELLMSG
           END-EVALUATE
      *
           MOVE    "/var/tmp/"         TO  BGS100P-CSVDIR
      *
           MOVE    SPACE               TO  BGS100P-CSVFNAME
           STRING  SPA-HOSPNUM     DELIMITED   BY  SIZE
                   "hknseikyuchk_" DELIMITED   BY  SIZE
                   BGS100P-SRYYM   DELIMITED   BY  SIZE
                   ".csv"          DELIMITED   BY  SIZE
           INTO    BGS100P-CSVFNAME
           END-STRING
      *
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 1001-PARA-CHK-SEC
      *
           IF    ( FLG-ERR             =   ZERO )
               PERFORM 1001-PRGOPTION-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ѥ�᥿������
      *****************************************************************
       1001-PARA-CHK-SEC               SECTION.
      *
           IF    ( BGS100P-FORMKBN = "0" OR "1" OR "2" OR "3" OR "9" )
               CONTINUE
           ELSE
               MOVE    "0"         TO  BGS100P-FORMKBN
           END-IF
      *
           IF    ( BGS100P-NYUGAIKBN   = " " OR "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    " "         TO  BGS100P-NYUGAIKBN
           END-IF
      *
           IF    ( BGS100P-EDITKBN =   "1" OR "0" )
               CONTINUE
           ELSE
               MOVE    "0"         TO  BGS100P-EDITKBN
           END-IF
      *
           IF    ( BGS100P-ETCGETKBN   =   "1" OR "0" )
               CONTINUE
           ELSE
               MOVE    "0"         TO  BGS100P-ETCGETKBN
           END-IF
      *
           MOVE    BGS100P-SRYYM   TO  BGS100P-SRYYMD
           MOVE    "01"            TO  BGS100P-SRYYMD(7:2)
      *
           EVALUATE    BGS100P-FORMKBN
           WHEN    "0"
               MOVE    1           TO  BGS100P-SYAHO
                                       BGS100P-KOKUHO
                                       BGS100P-KOUKI
                                       BGS100P-SOUKATU
           WHEN    "1"
               MOVE    1           TO  BGS100P-SYAHO
           WHEN    "2"
               MOVE    1           TO  BGS100P-KOKUHO
           WHEN    "3"
               MOVE    1           TO  BGS100P-KOUKI
           WHEN    "9"
               MOVE    1           TO  BGS100P-SOUKATU
           END-EVALUATE
      *
           IF    ( BGS100P-SRYYM   >=  CONST-H2204 )
               CONTINUE
           ELSE
               MOVE    SPACE       TO  WRK-ERRMSG
               STRING  "�о�ǯ���ʿ������ǯ����ʹ�"
                                       DELIMITED BY SIZE
                       "�Ȥ��Ƥ�������"
                                       DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       1001-PARA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����४�ץ�������
      *****************************************************************
       1001-PRGOPTION-SEC              SECTION.
      *
           MOVE    ZERO            TO  BGS100P-SKYGET-PARA
      *
      *    ���軰�Թ԰١˰����������ô�ۡ�����ɸ����ô�ۤν�����
           MOVE    "DAISAN_SAKI"   TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  BGS100P-SKYGET-PARA
           END-IF
      *
           .
       1001-PRGOPTION-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����४�ץ����ѥ�᥿��������
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-PARA
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCBGS100"         TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    ORCSPRGOPTION-OT-PARA
                                       TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ��ɽ�ѥǡ�������
           IF    ( BGS100P-RC      =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBGS110"
                   CALL    "ORCBGS110"  USING
                                       BGS100P-AREA
                                       SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBGS110"
           END-IF
      *
      *    ����
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-SYAHO   =   1 )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBGS137"
                   CALL    "ORCBGS137" USING
                                       BGS100P-AREA
                                       SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBGS137"
               END-IF
           END-IF
      *
      *    ����
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-KOKUHO  =   1 )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBGS138"
                   CALL    "ORCBGS138" USING
                                       BGS100P-AREA
                                       SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBGS138"
               END-IF
           END-IF
      *
      *    ��������
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-KOUKI   =   1 )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBGS140"
                   CALL    "ORCBGS140" USING
                                       BGS100P-AREA
                                       SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBGS140"
               END-IF
           END-IF
      *
      *    ����ɽ
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-SOUKATU =   1 )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBGS139"
                   CALL    "ORCBGS139" USING
                                       BGS100P-AREA
                                       SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBGS139"
               END-IF
           END-IF
      *
           EVALUATE    BGS100P-RC
           WHEN    2
               MOVE    SPACE   TO  WRK-ERRMSG
               STRING    "�������񵭺ܤ��оݥ쥻�ץȤ����뤿��"
                                           DELIMITED BY SIZE
                         "�����Ǥ��ޤ���"  DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-SKYFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-KOHFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-HKNJAFNAME  TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-ETCFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-MEIFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           IF    ( BGS100P-PAGE        =   ZERO )
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                   TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( FLG-ERR             =   ZERO )
               MOVE    "STM"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    BGS100P-PAGE    TO  JOB-UPDCNT
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BGS100ERR-FILE
           IF      STS-BGS100ERR     =   ZERO
               CLOSE   BGS100ERR-FILE
           ELSE
               OPEN    OUTPUT              BGS100ERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BGS100ERR-REC
               WRITE   BGS100ERR-REC
               CLOSE   BGS100ERR-FILE
      *
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    �ޥ������ռ�������
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ����ִ�������
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BGS100P-JOBID       TO  JOB-JOBID
           MOVE    BGS100P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCBGS100"         TO  JOB-PGID
           MOVE    BGS100P-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
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
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.