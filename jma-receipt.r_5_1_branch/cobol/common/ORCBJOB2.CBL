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
       PROGRAM-ID.             ORCBJOB2.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����ִ���
      *  ����ݡ��ͥ��̾  : ��λ����񤭹���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/05/30    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  MSTERR-FILE     ASSIGN  RECEERR
                                   FILE    STATUS IS  STS-MSTERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *    ���顼�ե�����
       FD  MSTERR-FILE.
       01  MSTERR-REC              PIC X(200).
      *
       WORKING-STORAGE         SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
grpsys*
       COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-MSTERR              PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-JOBKANRI            PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                    PIC 9(03). 
      *
       01  WRK-AREA.
           03  WRK-MSTERR              PIC X(200).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA-AREA.
           03  WRK-PARA.
               05  WRK-PARA-MODE       PIC X(03).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-ERRCD      PIC X(04).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-ERRFILE        PIC X(100).
           03  WRK-PARA-SHELLMSG       PIC X(100).
           03  WRK-PARA-YOBI           PIC X(100).
           03  WRK-PARA-UPDCNT-X.
               05  WRK-PARA-UPDCNT     PIC 9(10).
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
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
      **************************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
                                       WRK-AREA
                                       WRK-PARA-AREA
      *
           MOVE    SPACE           TO  RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SHELLMSG
                                               WRK-PARA-YOBI
                                               WRK-PARA-UPDCNT
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      D    DISPLAY "ORCBJOB=" WRK-PARA-MODE "##"
      *
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    ZERO                TO  FLG-END
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
           IF    ( WRK-PARA-MODE       =   "STS" )
      *
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN    =   ZERO )
                   IF  ( JOB-STOPFLG       =   1    )
                       PERFORM 500-CAN-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
               MOVE    WRK-PARA-MODE       TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-PARA-ERRCD      TO  JOB-ERRCD
               MOVE    WRK-PARA-SHELLMSG   TO  JOB-SHELLMSG
               MOVE    WRK-PARA-YOBI       TO  JOB-YOBI
      *
               IF    ( WRK-PARA-MODE       =   "STE" )
                AND  ( WRK-PARA-UPDCNT-X   IS  NUMERIC )
                   MOVE    WRK-PARA-UPDCNT TO  JOB-UPDCNT
               END-IF
      *
               PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( WRK-PARA-MODE       =   "CHK" )
                AND  ( SJOBKANRI-RETURN    =   ZERO )
                AND  ( JOB-ENDYMD          =   SPACE )
                   MOVE    "�������¹���Ǥ�"
                                       TO  WRK-MSTERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ����֥���󥻥����
      *****************************************************************
       500-CAN-HENSYU-SEC              SECTION.
      *
           MOVE    "��������ߤ���ޤ���"
                                       TO  WRK-MSTERR
      *
           OPEN    INPUT   MSTERR-FILE
           IF    ( STS-MSTERR          =   ZERO )
               CLOSE   MSTERR-FILE
           ELSE
               OPEN    OUTPUT              MSTERR-FILE
      *
               MOVE    WRK-MSTERR      TO  MSTERR-REC
               WRITE   MSTERR-REC
               CLOSE   MSTERR-FILE
      *
      *        ����ִ������Ͻ���
               MOVE    "CAN"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "8888"              TO  JOB-ERRCD
               MOVE    WRK-MSTERR          TO  JOB-YOBI
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    2                   TO  JOB-STOPFLG
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-CAN-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   MSTERR-FILE
           IF    ( STS-MSTERR          =   ZERO )
               CLOSE   MSTERR-FILE
           ELSE
               OPEN    OUTPUT              MSTERR-FILE
      *
               MOVE    WRK-MSTERR      TO  MSTERR-REC
               WRITE   MSTERR-REC
               CLOSE   MSTERR-FILE
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
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
      *    �ģ¥����ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.