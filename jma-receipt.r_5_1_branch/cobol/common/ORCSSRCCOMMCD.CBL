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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSSRCCOMMCD.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �����ȥ����ɸ�������
      *  �ֵѥ��顼������  : 0:���������ɤ��ꡢ1:���������ɤʤ���
      *                      2:�����ȼ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  13.07.23    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����        ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-TENSU           PIC 9(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-STSRYCD         PIC X(09).
           03  WRK-EDSRYCD         PIC X(09).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ���
           COPY    "CPTENSU.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    �����ȥ����ɸ���
           COPY    "CPORCSSRCCOMMCD.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION
                                   USING
                                       ORCSSRCCOMMCD-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    0                   TO  SRCCOM-RC
           MOVE    SPACE               TO  SRCCOM-COMMENT-CODE
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  WRK-AREA
      *
           PERFORM 110-COMMENT-KENSAKU-SEC
           .
       000-PROC-EXT.
           EXIT PROGRAM.
      *****************************************************************
      *    �����ȥ����ɸ�������
      *****************************************************************
       110-COMMENT-KENSAKU-SEC     SECTION.
      *
      *    �����ȼ���
           EVALUATE    SRCCOM-COMMENT-KBN
      *        ������ˡ
               WHEN    1
                   MOVE    "001000000"         TO  WRK-STSRYCD
                   MOVE    "001999999"         TO  WRK-EDSRYCD
      *        ������������
               WHEN    2
                   MOVE    "002000000"         TO  WRK-STSRYCD
                   MOVE    "002999999"         TO  WRK-EDSRYCD
      *        ������
               WHEN    8
                   MOVE    "820000000"         TO  WRK-STSRYCD
                   MOVE    "829999999"         TO  WRK-EDSRYCD
      *        �����ȼ�������
               WHEN   OTHER
                   MOVE    2                   TO  SRCCOM-RC
                   GO  TO  110-COMMENT-KENSAKU-EXT
           END-EVALUATE
      *
      *    �����ޥ����ɤ߹��߽���
           PERFORM 900-TENSU-READ-SEC
      *
      *    �����ǡ�������
           IF      FLG-TENSU           =   ZERO
               MOVE    0                   TO  SRCCOM-RC
               MOVE    TNS-SRYCD           TO  SRCCOM-COMMENT-CODE
      *    �����ǡ����ʤ�
           ELSE
               MOVE    1                   TO  SRCCOM-RC
           END-IF
           .
       110-COMMENT-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ����ɤ߹��߽���
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SRCCOM-MSTYMD       TO  TNS-YUKOSTYMD
           MOVE    SRCCOM-MSTYMD       TO  TNS-YUKOEDYMD
           MOVE    WRK-STSRYCD         TO  TNS-SRYCD
           MOVE    WRK-EDSRYCD         TO  TNSUP-SRYCD
           MOVE    SRCCOM-COMMENT      TO  TNS-NAME
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSU
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               ELSE
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.