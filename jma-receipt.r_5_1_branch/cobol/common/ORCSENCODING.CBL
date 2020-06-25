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
       PROGRAM-ID.             ORCSENCODING.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��̳����
      *  ����ݡ��ͥ��̾  : ���󥳡��ɼ�������
      *  ������            : 
      *  09/12/14    NACL��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 04.08.00     NACL-¿��Ǽ  16/09/01  �����ƥ����Ƚ���ɲ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *01  IDX-AREA.
      *    03  IDX                   PIC 9(03).
      *    03  IDY                   PIC 9(03).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-ERRMSG          PIC X(500).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ५������
           COPY    "CPDBCTG.INC".
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
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
           COPY    MCPAREA.
           COPY    COMMON-SPA.
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                     FLG-AREA
           INITIALIZE                     WRK-AREA
      *
      *    ���󥳡��ɼ�������
           PERFORM 100-DBCATLG-GET-SEC
      *H28.9
      *    "UTF8"�λ��������ƥ��������
           IF      SPA-ENCODING        =   2
               PERFORM 1001-SYSKANRI-9901-SEC
           END-IF
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    DB���󥳡��ǥ��󥰼�������
      *****************************************************************
       100-DBCATLG-GET-SEC        SECTION.
      *
           MOVE    ZERO                TO  SPA-ENCODING
      *    ���󥳡��ǥ��󥰼�������
           INITIALIZE                      DBCATALOG-REC
           MOVE    DBCATALOG-REC       TO  MCPDATA-REC
           MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
           MOVE    "enc"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
               MOVE    "enc"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  DBCATALOG-REC
      *            DISPLAY "���󥳡��ǥ��󥰾���  = " CPDBCTG-INFO-1
      *
                   EVALUATE    CPDBCTG-INFO-1
                       WHEN    "EUC_JP"
                           MOVE    1               TO  SPA-ENCODING
                       WHEN    "UTF8"
                           MOVE    2               TO  SPA-ENCODING
                       WHEN    OTHER
                           MOVE    3               TO  SPA-ENCODING
                   END-EVALUATE
      *            DISPLAY "SPA-ENCODING  = " SPA-ENCODING
      *        ELSE
      *            DISPLAY "ENCODING GET ERR = " MCP-RC
               END-IF
           END-IF
      *    CPDBCTG-INFO-1 = EUC_JP  or UTF8 or OTHER
      *
           MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
           MOVE    "enc"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-ENCODING        =   1
                                       OR  2
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG
               STRING  "tbl_dbcatalog CPDBCTG-INFO-1:"
                        CPDBCTG-INFO-1     DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
      *  ���������������
           INITIALIZE                      DBCATALOG-REC
           MOVE    DBCATALOG-REC       TO  MCPDATA-REC
           MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
           MOVE    "lcl"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
               MOVE    "lcl"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                  MOVE    MCPDATA-REC         TO  DBCATALOG-REC
      *           DISPLAY "�����������  = " CPDBCTG-INFO-1
      *        ELSE
      *           DISPLAY "ENCODING GET ERR = " MCP-RC
               END-IF
           END-IF
      *    CPDBCTG-INFO-1 = EUC_JP  or UTF8 or OTHER
      *
           MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
           MOVE    "lcl"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-DBCATLG-GET-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������
      *****************************************************************
       1001-SYSKANRI-9901-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "9901"              TO  SYS-KANRICD
           MOVE    "0001"              TO  SYS-KBNCD
           MOVE    "00000000"          TO  SYS-STYUKYMD
           MOVE    "99999999"          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
      *        ��ĥ�����Բġ���"EUC_JP" �Ȥ���
               IF      SYS-KANRITBL(1:1)   =   "0"
                   MOVE    1               TO  SPA-ENCODING
               END-IF
           END-IF
      *
           .
       1001-SYSKANRI-9901-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥�FETCH����
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *