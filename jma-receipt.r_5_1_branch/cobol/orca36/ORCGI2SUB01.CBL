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
       PROGRAM-ID.                     ORCGI2SUB01.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �������ԾȲ�
      *  ����ݡ��ͥ��̾  : �������ԾȲ񥵥�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/03/11    NACL�����ġ�  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07.05.29  ���롼�׿����б�
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-FILEMEI         PIC X(20).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ӣУ�����ơ��֥�
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPI2SUB01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           I2SUB01-LNK
           SPA-AREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   WRK-AREA
      *
      *
           IF  ( I2SUB01-KBN       =   "I" )
               PERFORM 100-SPA-INPUT-SEC
           ELSE
               PERFORM 200-SPA-OUTPUT-SEC
           END-IF
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �ӣãңӣУ��ɹ�����
      *****************************************************************
       100-SPA-INPUT-SEC               SECTION.
      *
           MOVE    "SCREEN"        TO  WRK-FILEMEI
           PERFORM 1001-SPA-INPUT-SEC
      *
           MOVE    "COMMON"        TO  WRK-FILEMEI
           PERFORM 1001-SPA-INPUT-SEC
      *
           .
       100-SPA-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    �ӣãңӣУ��ɹ�����
      *****************************************************************
       1001-SPA-INPUT-SEC               SECTION.
      *
           PERFORM 900-SPATMP-KEY-SEL-SEC
           IF    ( WRK-FILEMEI     =   "SCREEN" )
               MOVE    SPATMP-DATA-REC
                                   TO  I2SUB01-SCRSPA-AREA
           ELSE
               MOVE    SPATMP-DATA-REC
                                   TO  I2SUB01-COMMON
           END-IF
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBDELETE-SEC
      *
           .
       1001-SPA-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    �ӣãңӣУ����Ͻ���
      *****************************************************************
       200-SPA-OUTPUT-SEC              SECTION.
      *
           MOVE    "SCREEN"        TO  WRK-FILEMEI
           PERFORM 2001-SPA-OUTPUT-SEC
      *
           MOVE    "COMMON"        TO  WRK-FILEMEI
           PERFORM 2001-SPA-OUTPUT-SEC
      *
           .
       200-SPA-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    �ӣãңӣУ����Ͻ���
      *****************************************************************
       2001-SPA-OUTPUT-SEC             SECTION.
      *
           INITIALIZE                  SPATMP-REC
      *
           PERFORM 9001-SPATMP-KEY-EDIT-SEC
      *
           IF    ( WRK-FILEMEI     =   "SCREEN" )
               MOVE    I2SUB01-SCRSPA-AREA
                                   TO  SPATMP-DATA-REC
           ELSE
               MOVE    I2SUB01-COMMON
                                   TO  SPATMP-DATA-REC
           END-IF
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBINSERT-SEC
      *
           .
       2001-SPA-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    �����¸�ơ��֥븡������
      *****************************************************************
       900-SPATMP-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SPATMP
      *
           INITIALIZE                  SPATMP-REC
      *
           PERFORM 9001-SPATMP-KEY-EDIT-SEC
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBSELECT-SEC
           IF    ( FLG-SPATMP      =   ZERO )
               MOVE    MCPDATA2-REC TO SPATMP-REC
           ELSE
               MOVE    1           TO  FLG-SPATMP
               INITIALIZE              SPATMP-REC
           END-IF
      *
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBCLOSECURSOR-SEC
      *
           .
       900-SPATMP-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����¸�ơ��֥븡������Խ�����
      *****************************************************************
       9001-SPATMP-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  SPATMP-HOSPNUM
           MOVE    "I20"           TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID      TO  SPATMP-TERMID
           MOVE    WRK-FILEMEI     TO  SPATMP-FILEMEI
           MOVE    1               TO  SPATMP-EDANUM
      *
           .
       9001-SPATMP-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       920-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           .
      *
       920-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       920-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ɣΣӣţңԽ���
      *****************************************************************
       920-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       920-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    �ϣңãӣУ��ԣͣиƽн���
      *****************************************************************
       920-ORCDBSPATMP-SEC                SECTION.
      *
           CALL    "ORCDBSPATMP"       USING
                                       MCPAREA
                                       MCPDATA2-REC
                                       SPA-AREA
      *
           .
      *
       920-ORCDBSPATMP-EXT.
           EXIT.
