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
       PROGRAM-ID.             ORCBUMCP.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : �ޥ��������å�
      *  ������            : ����
      *  ��������    ��ȼ�        ����
      *  19/03/06    NACL-����     ��������
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
           SELECT  BUMCPERR-FILE   ASSIGN  BUMCP-ERRFNAME
                                   FILE    STATUS  IS  STS-BUMCPERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  BUMCPERR-FILE.
       01  BUMCPERR-REC            PIC X(200).
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT3.INC".
      *
       01  STS-AREA.
           03  STS-BUMCPERR            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-ERR                 PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-ERRMSG              PIC X(200).
      *
      *    �ѥ�᥿�ΰ�
       01  BUMCP-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBUMCPPARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
           IF    ( FLG-ERR         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
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
           INITIALIZE              BUMCP-AREA
                                   SPA-AREA
                                   STS-AREA
                                   WRK-AREA
                                   FLG-AREA
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
                                       BUMCP-JOBID
                                       BUMCP-SHELLID
                                       BUMCP-HOSPNUM
                                       BUMCP-ERRFNAME
                                       BUMCP-MSTKBN
                                       BUMCP-CSVKBN
                                       BUMCP-MSTYMD
           END-UNSTRING
      *
           MOVE    BUMCP-HOSPNUM       TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BUMCP-ERRFNAME      TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BUMCP-ERRFNAME

      *
           EVALUATE    BUMCP-MSTKBN
               WHEN    "1"
                   MOVE    "�����桼�����ޥ��������å��ꥹ��"
                                           TO  BUMCP-SHELLMSG
               WHEN    "2"
                   MOVE    "�����å��桼�����ޥ��������å��ꥹ��"
                                           TO  BUMCP-SHELLMSG
               WHEN    "3"
                   MOVE    "����Ϸ���ִ��桼�����ޥ��������å��ꥹ��"
                                           TO  BUMCP-SHELLMSG
               WHEN    "4"
                   MOVE    "�ݸ��ԥ桼�����ޥ��������å��ꥹ��"
                                           TO  BUMCP-SHELLMSG
               WHEN    "5"
                   MOVE    "����桼�����ޥ��������å��ꥹ��"
                                           TO  BUMCP-SHELLMSG
           END-EVALUATE
      *
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    �����ޥ���
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "1" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCTENSU"
                   CALL    "ORCBUMCTENSU"      USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCTENSU"
               END-IF
           END-IF
      *
      *    �������ޥ���
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "2" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCCHK"
                   CALL    "ORCBUMCCHK"        USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCCHK"
               END-IF
           END-IF
      *
      *    ���̡�Ϸ�ͥ����ɥޥ���
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "3" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCSRYCDCHG"
                   CALL    "ORCBUMCSRYCDCHG"   USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCSRYCDCHG"
               END-IF
           END-IF
      *
      *    �ݸ��ԥޥ���
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "4" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCHKNJAINF"
                   CALL    "ORCBUMCHKNJAINF"   USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCHKNJAINF"
               END-IF
           END-IF
      *
      *    ����ޥ���
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "5" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCADRS"
                   CALL    "ORCBUMCADRS"       USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCADRS"
               END-IF
           END-IF
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
           IF    ( BUMCP-PAGE          =   ZERO )
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                   TO  WRK-ERRMSG
 ��              PERFORM 500-ERR-HENSYU-SEC
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
           MOVE    BUMCP-PAGE      TO  JOB-UPDCNT
           PERFORM 800-ORCSJOB-SEC
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
           OPEN    INPUT   BUMCPERR-FILE
           IF      STS-BUMCPERR        =   ZERO
               CLOSE   BUMCPERR-FILE
           ELSE
               OPEN    OUTPUT              BUMCPERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BUMCPERR-REC
               WRITE   BUMCPERR-REC
               CLOSE   BUMCPERR-FILE
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
           MOVE    BUMCP-JOBID         TO  JOB-JOBID
           MOVE    BUMCP-SHELLID       TO  JOB-SHELLID
           MOVE    "ORCBUMCP"          TO  JOB-PGID
           MOVE    BUMCP-SHELLMSG      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA


 
           .
       800-ORCSJOB-EXT.
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
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.