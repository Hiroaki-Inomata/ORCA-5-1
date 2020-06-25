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
       PROGRAM-ID.             ORCSINFCHK.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �����ɥ����٥���󥹽��������å�����
      *  �ֵѥ��顼������  : 00:����
      *                      10:�ǡ��������ʤ�(�����ƥ����)
      *                      20:���������
      *                      30:�ǡ�������������
      *                      40:�ǡ������������Ѥ�
      *                      50:���ðջפʤ�
      *                      90:�ǡ�������������(�������������Ǥʤ�)
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09.12.09    ��ƣ          ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  04.06.01    NACL-��ƣ    11/01/28  �ꥢ�륿��������
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-DASCONF         PIC 9(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *    ����ִ����ģ¤Υ����͡ʴ����ɥǡ�����
           03  WRK-CONS-JOB-SHELLID-INFECT
                                   PIC X(08)   VALUE   "INFECTIO".
           03  WRK-CONS-JOB-JOBID-INFECT
                                   PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��������
       01  DASCF-REC.
           COPY    "CPDASCONF.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSINFCHK.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSINFCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO                TO  INFCHK-RC
      *
      *    ����ִ��������å��ʥǡ���������
           IF      SPA-DATAINFECTIONKBN    NOT =   2
               MOVE    10                  TO  INFCHK-RC
           END-IF
      *
      *    �ޥ������ռ���
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      INFCHK-RC           =   ZERO
      *        �����ʹߡ�
               IF      SMCNDATE-HMS      <=    "060000"
                   MOVE    20                  TO  INFCHK-RC
               END-IF
           END-IF
      *
           IF      INFCHK-RC           =   ZERO
               PERFORM 100-JOBEXEC-CHK-SEC
           END-IF
      *
      **   IF      INFCHK-RC           =   ZERO
      **       PERFORM 200-AGREEMENT-CHK-SEC
      **   END-IF
           .
      *
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �����ɥǡ����������������å�����
      *****************************************************************
       100-JOBEXEC-CHK-SEC         SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID-INFECT
                                       TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-INFECT
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    30              TO  INFCHK-RC
                   IF      JOB-STARTYMD    NOT =   SMCNDATE-YMD
                       MOVE    90          TO  INFCHK-RC
                   END-IF
               ELSE
                   IF      JOB-ENDYMD      =   SMCNDATE-YMD
                       MOVE    40          TO  INFCHK-RC
                   END-IF
               END-IF
           END-IF
           .
       100-JOBEXEC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ǡ����ե�����̵ͭ�����å�����
      *****************************************************************
       200-AGREEMENT-CHK-SEC       SECTION.
      *
           PERFORM 900-DASCONF-READ-SEC
           IF      FLG-DASCONF         =   ZERO
               IF    ( DASCF-AGREEMENT      =   "1"   )  AND
                     ( DASCF-SENDRESULT     =   "000" )
                   CONTINUE
               ELSE
                   MOVE    50              TO  INFCHK-RC
               END-IF
           ELSE
               MOVE    50              TO  INFCHK-RC
           END-IF
           .
       200-AGREEMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ɹ�
      *****************************************************************
       900-DASCONF-READ-SEC            SECTION.
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-DASCONF
                   MOVE    MCPDATA-REC         TO  DASCF-REC
               ELSE
                   MOVE    1                   TO  FLG-DASCONF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-DASCONF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DASCONF-READ-EXT.
           EXIT.
      *