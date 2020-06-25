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
       PROGRAM-ID.             ORCHC02GELOG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�
      *  ����ݡ��ͥ��̾  : ����䵡�����̾���ܥ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  13/03/18    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
           03  FLG-GENERICLOG      PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-YAKKAKJNCD          PIC X(12).
           03  WRK-GENERIC-STYMD       PIC X(08).
           03  WRK-GENERIC-EDYMD       PIC X(08).
      *
      *    ����̾�㳰�����ɥơ��֥�
           COPY    "CMGENERICTBL.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����̾���ܥ����ǡ���
       01  GENERICLOG-REC.
           COPY    "CPGENERICLOG.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ����̾���ܥ��������ѥ�᥿
           COPY    "CPORCHC02GELOG.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC02GELOGAREA
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
           MOVE    ZERO                TO  ORCHC02GELOG-RCD
      *
      *    �������
           PERFORM 100-INIT-SYORI-SEC
      *
      *    ����̾���ܥ�������
           PERFORM 200-MAIN-SYORI-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *    ���ֽ�ʣ�����å�
           PERFORM UNTIL    FLG-CHK    =   1
      *        ����������
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                           ORCSMCNDATEAREA
      *        ����̾���ܥ���
               PERFORM 1001-GENERICLOG-KEN-SEC
               IF      FLG-GENERICLOG      =   1
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ����̾���ܥ������Ͻ���
      *****************************************************************
       1001-GENERICLOG-KEN-SEC             SECTION.
      *
           INITIALIZE                      GENERICLOG-REC
           MOVE    SPA-HOSPNUM         TO  GENERICLOG-HOSPNUM
           MOVE    SPA-PTID            TO  GENERICLOG-PTID
           MOVE    SPA-SRYYMD          TO  GENERICLOG-SRYYMD
      *    ����ǯ����
           MOVE    SMCNDATE-YMD        TO  GENERICLOG-PRINTYMD
      *    ��������
           MOVE    SMCNDATE-HMS        TO  GENERICLOG-PRINTHMS
      *    Ϣ��
           MOVE    1                   TO  GENERICLOG-RECIPENUM
      *
           MOVE    GENERICLOG-REC      TO  MCPDATA-REC
           MOVE    "tbl_generic_log"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_generic_log"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICLOG-REC
                   MOVE    ZERO                TO  FLG-GENERICLOG
               ELSE
                   MOVE    1                   TO  FLG-GENERICLOG
               END-IF
           ELSE
               MOVE    1                   TO  FLG-GENERICLOG
           END-IF
           MOVE    "tbl_generic_log"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-GENERICLOG-KEN-EXT.
           EXIT.
      *****************************************************************
      *    ����̾���ܥ������Ͻ���
      *****************************************************************
       200-MAIN-SYORI-SEC             SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   100 )
                         OR  (ORCHC02GELOG-SRYCD (IDX) =   SPACE)
                         OR  (ORCHC02GELOG-RCD     NOT =   ZERO )
      *        ����̾���ܥ�������
               PERFORM 2001-GENERICLOG-SYORI-SEC
           END-PERFORM
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����̾���ܥ������Ͻ���
      *****************************************************************
       2001-GENERICLOG-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  GENERICLOG-REC
           INITIALIZE                      GENERICLOG-REC
           MOVE    SPA-HOSPNUM         TO  GENERICLOG-HOSPNUM
           MOVE    SPA-PTID            TO  GENERICLOG-PTID
           MOVE    SPA-SRYYMD          TO  GENERICLOG-SRYYMD
      *    ����ǯ����
           MOVE    SMCNDATE-YMD        TO  GENERICLOG-PRINTYMD
      *    ��������
           MOVE    SMCNDATE-HMS        TO  GENERICLOG-PRINTHMS
      *    Ϣ��
           MOVE    IDX                 TO  GENERICLOG-RECIPENUM
      *
      *    ������ʬ
           MOVE    ORCHC02GELOG-SYORIKBN
                                       TO  GENERICLOG-SYORIKBN
      *    ��ɼ�ֹ�
           MOVE    ORCHC02GELOG-DENPNUM (IDX)
                                       TO  GENERICLOG-DENPNUM
      *    ���ֹ�
           MOVE    ORCHC02GELOG-ZAINUM (IDX)
                                       TO  GENERICLOG-ZAINUM
      *    ���ť�����
           MOVE    ORCHC02GELOG-SRYCD (IDX)
                                       TO  GENERICLOG-SRYCD
      *    ������ॳ���ɡʰ���̾�����ɡ�
      *    ����̾�㳰�������Ѵ�
           PERFORM 20039-GENERICCD-HEN-SEC
           MOVE    WRK-YAKKAKJNCD      TO  GENERICLOG-YAKKAKJNCD
      *    ����̾��������
           MOVE    ORCHC02GELOG-GENERICNAME (IDX)
                                       TO  GENERICLOG-GENERICNAME
      *
           MOVE    GENERICLOG-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_generic_log"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    9                   TO  ORCHC02GELOG-RCD
               DISPLAY "GENERICLOG INS ERR:" MCP-RC 
           END-IF
           .
       2001-GENERICLOG-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����̾�㳰�������Ѵ�����
      *****************************************************************
       20039-GENERICCD-HEN-SEC         SECTION.
      *
           SET     TBL-GEN             TO  1
           SEARCH      TBL-GENERIC-OCC     VARYING   TBL-GEN
                   AT  END
                       MOVE    SPACE            TO  WRK-YAKKAKJNCD
                       MOVE    SPACE            TO  WRK-GENERIC-STYMD
                       MOVE    SPACE            TO  WRK-GENERIC-EDYMD
                   WHEN    ORCHC02GELOG-YAKKAKJNCD(IDX)
                                               =   TBL-YAKKAKJNCD
                                                           (TBL-GEN)
                       MOVE    TBL-GENERIC-CODE (TBL-GEN)
                                                TO  WRK-YAKKAKJNCD
                       MOVE    TBL-GENERIC-STYMD (TBL-GEN)
                                                TO  WRK-GENERIC-STYMD
                       MOVE    TBL-GENERIC-EDYMD (TBL-GEN)
                                                TO  WRK-GENERIC-EDYMD
           END-SEARCH
      *
           IF      WRK-YAKKAKJNCD  NOT =   SPACE
      *        ͭ������Ƚ��
               IF    (SPA-SRYYMD           >=  WRK-GENERIC-STYMD)
                 AND (SPA-SRYYMD           <=  WRK-GENERIC-EDYMD)
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-YAKKAKJNCD
               END-IF
           END-IF
      *
           IF      WRK-YAKKAKJNCD      =   SPACE
               MOVE    ORCHC02GELOG-YAKKAKJNCD(IDX)(1:9)
                                           TO  WRK-YAKKAKJNCD
           END-IF
           .
       20039-GENERICCD-HEN-EXT.
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
      *    �ơ��֥��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
      *