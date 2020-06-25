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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW20SUB01.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : �������������Х���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/07/30    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-����    03.12.04  �Ǹ�գ��Ͱʾ�Ǥʤ���硢
      *                                     ͭ�����Ž꣱��������������
      *                                     �ϴǸ�����Ŭ����֤�������
      * 03.05.00     NACL-����    07.05/18  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-NYUKHN                          PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-TNS-NAME                        PIC X(200).
           03  WRK-TNS-TEN                         PIC 9(05).
           03  WRK-PATHNAME                        PIC X(64).
           03  WRK-CHG-SRYCD                       PIC X(09).
      *
       01  CONST-AREA.
           03  CONST-H260401           PIC X(08)   VALUE "20140401".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ���
           COPY  "CPTENSU.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPW20SUB01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               W20SUB01-LNK
                                               SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-EDIT-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       100-EDIT-SEC                    SECTION.
      *
           INITIALIZE  W20SUB01-OT
      *
           EVALUATE    W20SUB01-I-KBN
      *
      *    ���Ź԰٥����ɤǸ���
           WHEN    "1"
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU   =   ZERO )
                   PERFORM 1001-W20SUB01-0-HEN-SEC
               ELSE
                   MOVE    1               TO  W20SUB01-O-RC
               END-IF
           WHEN    OTHER
               MOVE    1                   TO  W20SUB01-O-RC
           END-EVALUATE 
      *
           .
       100-EDIT-EXT.
          EXIT.
      *****************************************************************
      *    �������������Х����ֵ����Խ�����
      *****************************************************************
       1001-W20SUB01-0-HEN-SEC         SECTION.
      *
           MOVE    ZERO        TO  W20SUB01-O-RC
           MOVE    TNS-NAME    TO  W20SUB01-O-NAME
           IF    ( W20SUB01-I-KHNSRYCD (1:1)   =   "0" )
               MOVE    W20SUB01-I-KHNSRYCD
                               TO  W20SUB01-O-KHNSRYCD
           ELSE
               MOVE    TNS-SRYCD
                               TO  W20SUB01-O-KHNSRYCD
           END-IF
           MOVE    TNS-TEN     TO  W20SUB01-O-KHNTEN
      *
           .
       1001-W20SUB01-0-HEN-EXT.
          EXIT.
      *****************************************************************
      *    �����ޥ�����������
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE  TNS-TENSU-REC
      *
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
      *
           MOVE    W20SUB01-I-KHNSRYCD
                                   TO  TNS-SRYCD
      *
           EVALUATE    W20SUB01-I-KHNSRYCD
           WHEN    "099999950"
               MOVE    "190097010" TO  TNS-SRYCD
           WHEN    "099999951"
               MOVE    "190097210" TO  TNS-SRYCD
           WHEN    "099999952"
               MOVE    "190135110" TO  TNS-SRYCD
           WHEN    "099999955"
               MOVE    "190169610" TO  TNS-SRYCD
           WHEN    "099999956"
               MOVE    "190169910" TO  TNS-SRYCD
           WHEN    "099999957"
               MOVE    "190170210" TO  TNS-SRYCD
           END-EVALUATE
      *
           EVALUATE    TRUE
           WHEN  ( W20SUB01-I-KJNYMD   >=  CONST-H260401 )
             AND ( W20SUB01-I-KHNSRYCD  =  "190131310" )
               MOVE    "190117710" TO  TNS-SRYCD
           WHEN  ( W20SUB01-I-KJNYMD   >=  CONST-H260401 )
             AND ( W20SUB01-I-KHNSRYCD  =  "190131410" )
               MOVE    "190077410" TO  TNS-SRYCD
           END-EVALUATE
      *
           MOVE    W20SUB01-I-KJNYMD   TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
      *    �����ޥ�������
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    (  MCP-RC     NOT =   ZERO )
               MOVE    1           TO  FLG-TENSU
           ELSE
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           END-IF
      *
      *    �����ޥ�����������
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.