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
       PROGRAM-ID.             ORCSAUDIT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ƻ����
      *  ����ݡ��ͥ��̾  : �ƻ��������
      *  �ֵѥ��顼������  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  10/02/27    NACL-����     ��������
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
       
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYSYMD          PIC X(08).
           03  WRK-MCP-RC          PIC ---9.
           03  WRK-STR             PIC X(255).
           03  WRK-LOGMSG          PIC X(255).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-FUNC            PIC X(64).
           03  WRK-TABLE           PIC X(64).
           03  WRK-PTID            PIC 9(10).
      *
       01  ORCXKANACONVAREA.
      *   IN/OUT��Ĺ����MCPDARA-REC�˹礻�뤿���ȼ��˰����ΰ���Խ�
      *   ���ϥХ��ȿ�
           03 KANACONV-LEN         PIC S9(9)   BINARY.
      *   �֤���( 0: ���� 1: �ϰϳ�ʸ�� 2: �������Ѵ���ǽ )
           03 KANACONV-RETURN      PIC S9(9) BINARY.
      *   �Ѵ�ʸ�����(�Х���)
           03 KANACONV-MAXLEN      PIC S9(9)   BINARY.
      *   �Ѵ��ե饰 (0: �Ѵ����ʤ�,     1: �Ѵ�����
      *               2: 1+���������Ѵ�, 3: 1+�Ҥ餬���Ѵ�)
      *   �� Ⱦ�ѥ��ʤ���ӳ��������ƤΥե饰���Ѵ������
      *      ���ΤƤ��롣
      *     (�����Ϣ�)
           03 KANACONV-CONV-FLAG   PIC S9(9)   BINARY.
      *   ����ʸ�������
      *       0: Ⱦ�ѥ��ʡ������Τ��Ѵ�
      *      14: ����   2: �Ҥ餬��  4: ��������
      *      112: ASCII 16: ����     32: ����ե��٥å�
      *      15: ����+���ڡ�������   113: ASCII+���ڡ������� 
      *   ����ʸ����� OR ��Ȥ뤳�Ȥ�ʣ������Ǥ���
      *   ��: (6: �Ҥ餬�ʡ��������ʤΤ�)
      *       (48: �ѿ����Τ�)
           03 KANACONV-CHAR-TYPE   PIC S9(9)   BINARY.
      *   ʸ���ΰ��Ĺ��(INDATA, OUTDATA��Ĺ����Ʊ���Ǥ��뤳��)
           03 KANACONV-STLEN       PIC S9(9) BINARY.
      *   ����ʸ����
           03 KANACONV-INDATA      PIC X(50000).
      *   ����ʸ����
           03 KANACONV-OUTDATA     PIC X(50000).
      *
       01  CONST-AREA.
           03 CONST-MAX-LEN        PIC 9(08)   VALUE   50000.
      *
           COPY    "MCPDATA.INC"   REPLACING   //MCPDATA-//
                                   BY          //WKMCPDATA-//.
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *    �������̶�ʬ
       01  SHIKIBETU-KBN           PIC X(01).
      *    �����ΰ�
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
           COPY    "CPORCSLOCK.INC".
      *
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
       PROCEDURE               DIVISION    USING
                               SHIKIBETU-KBN
                               MCPAREA
                               MCPDATA-REC
                               SPA-AREA
                               ORCSLOCKAREA
                               JOBKANRI-REC
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
           MOVE    MCP-FUNC            TO  WRK-FUNC
           MOVE    1                   TO  MCP-LOGFLAG
           MOVE   "AUDITLOG"           TO  MCP-FUNC
      *
           PERFORM 100-INIT-SEC
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           MOVE    WRK-FUNC            TO  MCP-FUNC
           MOVE    ZERO                TO  MCP-LOGFLAG
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SHIKIBETU-KBN       =   "1"
      *        �ƻ�����Խ����� (���å��ơ��֥�)
               PERFORM 1001-LOCKLOG-SEC
           ELSE
      *        �ƻ�����Խ����� (����ִ����ơ��֥�)
               PERFORM 1001-JOBLOG-SEC
           END-IF
      *
           MOVE    WRK-LOGMSG          TO  WKMCPDATA-REC
      *    ���顼��å������Խ�����
           PERFORM 800-KANACONV-SEC
           IF      KANACONV-RETURN     =   ZERO 
               MOVE    WRK-LOGMSG          TO  MCP-LOGCOMMENT
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ƻ�����Խ����� (���å��ơ��֥�)
      *****************************************************************
       1001-LOCKLOG-SEC            SECTION.
      *
           IF      SLOCK-MODE          =   1
               MOVE   "Lock_start"         TO  WRK-STR
               MOVE SLOCK-PTID             TO  WRK-PTID
           ELSE
               MOVE   "Lock_release"       TO  WRK-STR
               MOVE   SPA-LAST-PTID        TO  WRK-PTID
           END-IF
      *
           STRING WRK-STR              DELIMITED   BY  SPACE
                  " HOSPNUM["          DELIMITED   BY  SIZE
                  SPA-HOSPNUM          DELIMITED   BY  SIZE
                  "] PTID["            DELIMITED   BY  SIZE
                  WRK-PTID             DELIMITED   BY  SIZE
                  "] SRYYMD["          DELIMITED   BY  SIZE
                  SPA-SRYYMD           DELIMITED   BY  SIZE
                  "]"                  DELIMITED   BY  SIZE
                                       INTO    WRK-LOGMSG
           END-STRING
           .
      *
       1001-LOCKLOG-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ƻ�����Խ����� (����ִ����ơ��֥�)
      *****************************************************************
       1001-JOBLOG-SEC            SECTION.
      *
           MOVE   "Batch_start"        TO  WRK-STR
      *
           STRING WRK-STR              DELIMITED   BY  SPACE
                  " HOSPNUM["          DELIMITED   BY  SIZE
                  SPA-HOSPNUM          DELIMITED   BY  SIZE
                  "] SHELLID["         DELIMITED   BY  SIZE
                  JOB-SHELLID          DELIMITED   BY  SIZE
                  "] SRYYM["           DELIMITED   BY  SIZE
                  JOB-SRYYM            DELIMITED   BY  SIZE
                  "] NYUGAI["          DELIMITED   BY  SIZE
                  JOB-NYUGAIKBN        DELIMITED   BY  SIZE
                  "]"                  DELIMITED   BY  SIZE
                                       INTO    WRK-LOGMSG
           END-STRING
           .
      *
       1001-JOBLOG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       800-KANACONV-SEC             SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    CONST-MAX-LEN   TO  KANACONV-LEN
                                       KANACONV-STLEN
           MOVE    0               TO  KANACONV-CONV-FLAG
           MOVE    113             TO  KANACONV-CHAR-TYPE
           MOVE    WKMCPDATA-REC   TO  KANACONV-INDATA
           CALL    "kanaconv"      USING   ORCXKANACONVAREA
      *
           .
       800-KANACONV-EXT.
           EXIT.
      *