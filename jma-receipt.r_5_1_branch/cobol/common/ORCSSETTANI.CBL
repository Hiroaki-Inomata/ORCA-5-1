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
       PROGRAM-ID.                     ORCSSETTANI.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ñ�̥������ֵ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  20/02/13    NMED������    ��������
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
      *
       WORKING-STORAGE                 SECTION.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  TNSP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPTENSU.INC"       REPLACING
                                       //TNS-//   BY //LNK-//
                                       //TNSUP-// BY //LNKUP-//
                                       //TNSXX-// BY //LNKXX-//.
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           LNK-TENSU-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( LNK-SRYCD (1:1)     =   "6" OR "7" )
               PERFORM 900-TENSUPLUS-KEY5-SEL-SEC
               IF    ( TNSP-USERTANICD NOT =   SPACE )
                   MOVE    TNSP-USERTANICD
                                       TO  LNK-TANICD
                   MOVE    TNSP-USERTANINAME
                                       TO  LNK-TANINAME
                   IF    ( LNK-TANINAME    =   SPACE )
                       MOVE    ZERO    TO  LNK-TANIMOJISU
                   ELSE
                       MOVE    FUNCTION LENGTH (
                                   FUNCTION TRIM(LNK-TANINAME TRAILING)
                               )       TO  LNK-TANIMOJISU
                       COMPUTE LNK-TANIMOJISU
                           =   LNK-TANIMOJISU  /  2
                   END-IF
      D            DISPLAY "LNK-SRYCD:" LNK-SRYCD
      D                             " " LNK-TANICD
      D                             " " LNK-TANIMOJISU
      D                             " " LNK-TANINAME
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ������
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����ղø�������
      *****************************************************************
       900-TENSUPLUS-KEY5-SEL-SEC          SECTION.
      *
           INITIALIZE                      TNSP-REC
           MOVE    LNK-HOSPNUM        TO   TNSP-HOSPNUM
           MOVE    LNK-SRYCD          TO   TNSP-SRYCD
           IF    ( LNKUP-YUKOSTYMD    =    SPACE )
               MOVE    LNK-YUKOSTYMD  TO   TNSP-YUKOEDYMD
               MOVE    LNK-YUKOEDYMD  TO   TNSP-YUKOSTYMD
           ELSE
               MOVE    LNKUP-YUKOSTYMD TO  TNSP-YUKOEDYMD
               MOVE    LNKUP-YUKOEDYMD TO  TNSP-YUKOSTYMD
           END-IF
           MOVE    TNSP-REC            TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNSP-REC
           ELSE
               INITIALIZE                  TNSP-REC
           END-IF
      *
           MOVE    "tbl_tensuplus"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSUPLUS-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.