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
       PROGRAM-ID.                 ORCSMEIHEN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ɽ��̾���Խ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/01/23    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDM2                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-H         PIC X(18).
      *
           03  WRK-KANJI           PIC X(100).
      *    ��縡����
           03  WRK-SURYO-2X            PIC X(02).
           03  WRK-SURYO-2             PIC Z9.
      *
           03  WRK-HOSPNUM             PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *    �����ޥ�����
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ɽ��̾���Խ��ѥ�᥿�ΰ�
           COPY    "CPORCSMEIHEN.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSMEIHENAREA
           .
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
           INITIALIZE                  ORCSMEIHEN-OUT-AREA
           IF      ORCSMEIHEN-GRPHOSPNUM   =   ZERO
               MOVE    SPA-HOSPNUM         TO  WRK-HOSPNUM
           ELSE
               MOVE    ORCSMEIHEN-GRPHOSPNUM   TO  WRK-HOSPNUM
           END-IF
      *
           IF      ORCSMEIHEN-TNS-NAME  =   SPACE
               PERFORM 100-TNS-NAME-HEN-SEC
           END-IF
      *
           IF      ORCSMEIHEN-TNS-NAME  NOT =   SPACE
               EVALUATE    ORCSMEIHEN-KBN
      *        ���̳��
               WHEN    "1"
                   PERFORM 100-YOURYOU-HEN-SEC
      *        ��縡����
               WHEN    "2"
                   PERFORM 200-HOUKATU-HEN-SEC
               END-EVALUATE
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �����ޥ�����̾���Խ�����
      *****************************************************************
       100-TNS-NAME-HEN-SEC         SECTION.
      *
           MOVE    ORCSMEIHEN-SRYCD    TO  TNS-SRYCD
           IF      SPA-SRYYMD          =   SPACE
               MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
               MOVE    SPA-SYSYMD          TO  TNS-YUKOEDYMD
           ELSE
               MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
               MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
           END-IF
      *
           MOVE    WRK-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ��������̾��
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60")  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
           MOVE    TNS-NAME            TO  ORCSMEIHEN-TNS-NAME
           .
       100-TNS-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̳�����
      *****************************************************************
       100-YOURYOU-HEN-SEC               SECTION.
      *
           MOVE    ORCSMEIHEN-SURYO    TO  WRK-SURYO
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
           MOVE    SPACE               TO  WRK-SURYO-H
           PERFORM VARYING     IDM     FROM    1  BY  1
                   UNTIL       IDM     >   11
               IF      WRK-SURYO-X(IDM:1)  NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDM:)  TO  WRK-SURYO-H
                   MOVE    11                  TO  IDM
               END-IF
           END-PERFORM
      *    �����Ѵ�
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SURYO-H         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KANJI
           ELSE
               MOVE    SPACE               TO  WRK-KANJI
           END-IF
      *
           MOVE    SPACE               TO  ORCSMEIHEN-MEISYO
           STRING  ORCSMEIHEN-TNS-NAME DELIMITED  BY  SPACE
                   WRK-KANJI           DELIMITED  BY  SIZE
                                       INTO    ORCSMEIHEN-MEISYO
           END-STRING
           .
       100-YOURYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��縡��������
      *****************************************************************
       200-HOUKATU-HEN-SEC               SECTION.
      *
      *    ���ܿ��ϣ���Ȥ���
           MOVE    ORCSMEIHEN-SURYO    TO  WRK-SURYO-2
           MOVE    SPACE               TO  WRK-SURYO-2X
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDM     FROM    1  BY  1
                   UNTIL       IDM     >   2
               IF      WRK-SURYO-2(IDM:1)  NOT =   SPACE
                   ADD     1                   TO  IDM2
                   MOVE    WRK-SURYO-2(IDM:1)  TO  WRK-SURYO-2X(IDM2:1)
               END-IF
           END-PERFORM
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SURYO-2X        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KANJI
           ELSE
               MOVE    SPACE               TO  WRK-KANJI
           END-IF
      *
           MOVE    SPACE               TO  ORCSMEIHEN-MEISYO
           MOVE    SPACE               TO  ORCSMEIHEN-MEISYO2
           STRING  ORCSMEIHEN-TNS-NAME DELIMITED  BY  SPACE
                   "�������ܿ���"      DELIMITED  BY  SIZE
                   WRK-KANJI           DELIMITED  BY  SIZE
                                       INTO    ORCSMEIHEN-MEISYO
           END-STRING
      *    ���ܿ��Τ�
           STRING  "�������ܿ���"      DELIMITED  BY  SIZE
                   WRK-KANJI           DELIMITED  BY  SPACE
                                       INTO    ORCSMEIHEN-MEISYO2
           END-STRING
           .
       200-HOUKATU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *