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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPIV3SUB01.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : ���̥���󥻥륵�֡ʰ��β���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  15/07/XX    NACL��¿��Ǽ����������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    ��������ΰ�
      *01  CNT-AREA.
      *    03  CNT-SRYCNT              PIC 9(04).
      *    ����ΰ�
      *
       01  WRK-AREA.
      *
           03  WRK-PTNUM               PIC X(20).
           03  WRK-TERMID              PIC X(64).
           03  WRK-RENNUM              PIC 9(01).
           03  WRK-PARA-GYOUMUID       PIC X(04).
      *
           03  WRK-MCP-TERM            PIC X(62).
      *
      *    ����̳���
       01  CONS-ORCA-UID       PIC X(64)   VALUE
                   "267c5110-71b6-431e-baac-0989ab98a505".
      *
       01  CONS-ORCA-UID2       PIC X(64)   VALUE
                   "API+267c5110-71b6-431e-baac-0989ab98a505".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    ��¾���極�֥ѥ�᥿
           COPY    "CPORCSLOCK.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *     �ѥ�᥿�ơ��֥�
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    �ӣУ�����ơ��֥�
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "MCPAREA".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    �ѥ�᥿�ΰ�
           COPY    "CPORAPIV3SUB01.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           ORAPIV3SUB01AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPACE               TO  APIV3SUB01-ERRCD
      *
           PERFORM 100-INIT-SEC
      *
           IF      APIV3SUB01-TERMID   =    CONS-ORCA-UID
                                       OR   CONS-ORCA-UID2
      *        ����̳������������λ
               PERFORM 400-ALL-CANCEL-SEC
           ELSE
               IF      APIV3SUB01-ERRCD     =   SPACE
      *            ����������λ
                   PERFORM 200-CANCEL-SEC
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    ɬ�ܹ��ܥ����å�
           IF     (APIV3SUB01-TERMID    =   SPACE )
              AND (APIV3SUB01-KARTE-UID =   SPACE )
               MOVE    "01"            TO  APIV3SUB01-ERRCD
           END-IF
      *
           IF      APIV3SUB01-API-GYOUMUID  =   SPACE
               MOVE    "02"            TO  APIV3SUB01-ERRCD
           END-IF
      *
           IF      APIV3SUB01-SYORI NOT =   "9"
               IF     (APIV3SUB01-TERMID    =   SPACE )
                  OR  (APIV3SUB01-PARA-GYOUMUID =   SPACE )
                   MOVE    "01"            TO  APIV3SUB01-ERRCD
               END-IF
           END-IF
           .
       100-INIT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    ����������λ
      *****************************************************************
       200-CANCEL-SEC   SECTION.
      *
           MOVE    APIV3SUB01-PTNUM     TO  WRK-PTNUM
           IF      APIV3SUB01-TERMID    =   SPACE
      *        ORUCA-UID ����
               PERFORM 2001-TERMID-GET-SEC
           ELSE
               MOVE    APIV3SUB01-TERMID    TO  WRK-TERMID
           END-IF
      *
           IF     (WRK-TERMID          =   SPACE )
                MOVE    "03"                TO  APIV3SUB01-ERRCD
           ELSE
      *        �ѥ�᥿�������
               PERFORM 2002-FILE-DELETE-SEC
      *        ��¾������
               IF      APIV3SUB01-PARA-GYOUMUID  NOT =   SPACE
                   PERFORM 990-LOCK-OUT-SEC
                   IF      SLOCK-RETURN    NOT =   ZERO
                       MOVE    "04"            TO  APIV3SUB01-ERRCD
                   END-IF
               END-IF
      *
               MOVE    WRK-TERMID          TO  APIV3SUB01-TERMID
           END-IF
           .
       200-CANCEL-EXIT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ����������
      *****************************************************************
       2001-TERMID-GET-SEC          SECTION.
      *
      *    �ѥ�᥿����
           MOVE    SPACE               TO  WRK-TERMID
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM             TO  APIPARA-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID  TO  APIPARA-GYOUMUID
           MOVE    APIV3SUB01-KARTE-UID     TO  APIPARA-KARTE-UID
           MOVE    APIV3SUB01-API-FILEMEI   TO  APIPARA-FILEMEI
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-APIPARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM UNTIL       FLG-APIPARA     =   1
      *        �����ֹ椬����
               IF     (WRK-PTNUM           =   SPACE )
               OR    ((WRK-PTNUM       NOT =   SPACE        ) AND
                      (WRK-PTNUM           =   APIPARA-PTNUM))
                   MOVE    APIPARA-TERMID      TO  WRK-TERMID
                   MOVE    1                   TO  FLG-PARA
               END-IF
               IF      FLG-APIPARA         =   ZERO
                   MOVE    "tbl_api_para"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-APIPARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_api_para"          TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-TERMID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ����������
      *****************************************************************
       2002-FILE-DELETE-SEC          SECTION.
      *
      *    ��̳�����¸�ե�������
           IF      APIV3SUB01-PARA-GYOUMUID  NOT =   SPACE
               MOVE    APIV3SUB01-PARA-GYOUMUID  TO  WRK-PARA-GYOUMUID
               PERFORM 20021-PARA-DELETE-SEC
           END-IF
      *
           IF      APIV3SUB01-PARA-GYOUMUID2  NOT =   SPACE
               MOVE    APIV3SUB01-PARA-GYOUMUID2  TO  WRK-PARA-GYOUMUID
               PERFORM 20021-PARA-DELETE-SEC
           END-IF
      *
      *    API�����¸�ե�������
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID
                                       TO  PARA-GYOUMUID
           MOVE    WRK-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_para"          TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID
                                       TO  SPATMP-GYOUMUID
           MOVE    WRK-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID
                                       TO  APIPARA-GYOUMUID
           MOVE    WRK-TERMID          TO  APIPARA-TERMID
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       2002-FILE-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ѥ�᥿�ե�����������
      *****************************************************************
       20021-PARA-DELETE-SEC         SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    WRK-PARA-GYOUMUID   TO  PARA-GYOUMUID
           MOVE    WRK-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_para"          TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    WRK-PARA-GYOUMUID   TO  SPATMP-GYOUMUID
           MOVE    WRK-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           .
       20021-PARA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����̳������������λ
      *****************************************************************
       400-ALL-CANCEL-SEC         SECTION.
      *
           MOVE    SPACE               TO  APIV3SUB01-PARA-GYOUMUID
           MOVE    SPACE               TO  APIV3SUB01-ERRCD
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-APIPARA-READ-SEC
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           PERFORM
               UNTIL  (FLG-APIPARA         =   1     )
      *
               MOVE    APIPARA-TERMID      TO  WRK-TERMID
      *        API�����¸�ե�������
               INITIALIZE                  PARA-REC
               MOVE    APIPARA-HOSPNUM     TO  PARA-HOSPNUM
               MOVE    APIPARA-TERMID      TO  PARA-TERMID
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "del4"              TO  MCP-PATHNAME
               MOVE    "tbl_para"          TO  MCP-TABLE
               PERFORM 900-ORCDBMAIN-SEC
      *        SPATMP
               INITIALIZE                      SPATMP-REC
               MOVE    APIPARA-HOSPNUM     TO  SPATMP-HOSPNUM
               MOVE    APIPARA-TERMID      TO  SPATMP-TERMID
      *
               MOVE    SPATMP-REC          TO  MCPDATA2-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "del4"              TO  MCP-PATHNAME
               PERFORM 910-SPATMP-CALL-SEC
      *
      *        ��¾������
               PERFORM 990-LOCK-OUT-SEC
      *        APIPARA ���
               MOVE    APIPARA-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               PERFORM 900-ORCDBMAIN-SEC
      *
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 990-APIPARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       400-ALL-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��¾����������(���)
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
           MOVE    MCP-TERM            TO  WRK-MCP-TERM
      *    ��¾����������
           MOVE    APIV3SUB01-PARA-GYOUMUID
                                       TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
      *    API+���륫�գɣ� ��TERMID
           MOVE    WRK-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       300-ERRMSG-HEN-SEC          SECTION.
      *
           EVALUATE    APIV3SUB01-ERRCD
               WHEN    "01"
                   MOVE    "�գɣĤ����꤬����ޤ���"
                                       TO  APIV3SUB01-ERRMSG
               WHEN    "02"
                   MOVE    "��̳�����꤬����ޤ���"
                                       TO  APIV3SUB01-ERRMSG
               WHEN    "03"
                   MOVE    "���륫�գɣĤ������Ǥ��ޤ���"
                                       TO  APIV3SUB01-ERRMSG
               WHEN    "04"
                   MOVE    "��¾����˼��Ԥ��ޤ���"
                                       TO  APIV3SUB01-ERRMSG
           END-EVALUATE
           .
       300-ERRMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ѥ�᥿�����ɤ߹���
      *****************************************************************
       990-APIPARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  APIPARA-REC
               MOVE    ZERO            TO  FLG-APIPARA
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
      *
           .
       990-APIPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����¸�ơ��֥븡������
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥�CALL����
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *