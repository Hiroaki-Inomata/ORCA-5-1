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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSTEIKIJOB.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ������᥸��֥����å�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/02/16    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SHELLID             PIC X(08).
           03  WRK-JOBID               PIC 9(07).
      *
       01  CONST-AREA.
           03  CONST-SHELLID-TEIKI1    PIC X(08)   VALUE   "TEIKI1".
           03  CONST-SHELLID-TEIKI3    PIC X(08)   VALUE   "TEIKI3".
           03  CONST-3003-KBNCD        PIC X(08)   VALUE   "033".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK3003.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCSTEIKIJOB.INC".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   STEIKIJOB-AREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           INITIALIZE                  STEIKIJOB-OT
      *
           IF        ( STEIKIJOB-RECEKBN       =   "1" )
               IF    ( STEIKIJOB-NYUGAIKBN NOT =   "1" )
                   MOVE    1       TO  FLG-END
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-TEIKI1-CHK-SEC
      *
           IF    ( STEIKIJOB-ERRCD     =   SPACE )
               PERFORM 2001-TEIKI3-CHK-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������᥸��֥����å�����
      *****************************************************************
       2001-TEIKI1-CHK-SEC             SECTION.
      *
      *    ����ִ��������å�
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3003-REC
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SYS-3003-JOBID )
                        OR   ( STEIKIJOB-ERRCD NOT =   SPACE )
                   MOVE    CONST-SHELLID-TEIKI1
                                       TO  WRK-SHELLID
                   MOVE    IDX0        TO  WRK-JOBID
                   PERFORM 2001-ORCSJOB-SEC
               END-PERFORM
           END-IF
      *
           .
       2001-TEIKI1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���������֥����å�����
      *****************************************************************
       2001-TEIKI3-CHK-SEC             SECTION.
      *
           MOVE    CONST-SHELLID-TEIKI3    TO  WRK-SHELLID
           MOVE    1                       TO  WRK-JOBID
           PERFORM 2001-ORCSJOB-SEC
      *
           .
       2001-TEIKI3-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ������᥸��ּ¹Ծ��������å�����
      *****************************************************************
       2001-ORCSJOB-SEC                SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-JOBID           TO  JOB-JOBID
           MOVE    WRK-SHELLID         TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
                   IF    ( STEIKIJOB-RECEKBN   =   SPACE )
                       PERFORM 20011-ERRMES-HEN-SEC
                   ELSE
                       PERFORM 20011-RECETEIKI-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    �쥻�Ȥ���¾�����å�����
      *****************************************************************
       20011-RECETEIKI-CHK-SEC         SECTION.
      *
           IF    ( STEIKIJOB-SYOKBN    =   "1" )
               IF    ( JOB-SYOKBN      =   "1"           )
                AND  ( JOB-SRYYM   NOT =   STEIKIJOB-SRYYM )
                   CONTINUE
               ELSE
                   PERFORM 20011-ERRMES-HEN-SEC
               END-IF
           ELSE
                   PERFORM 20011-ERRMES-HEN-SEC
           END-IF
      *
           .
       20011-RECETEIKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       20011-ERRMES-HEN-SEC            SECTION.
      *
           MOVE    "7777"      TO  STEIKIJOB-ERRCD
           STRING  "������������Ǥ���"
                                           DELIMITED  BY  SIZE
                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                   "��"            DELIMITED  BY  SIZE
           INTO  STEIKIJOB-ERRMSG
           END-STRING
      *
           .
       20011-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
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
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
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
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
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
       910-DBCLOSECURSOR-SEC                 SECTION.
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