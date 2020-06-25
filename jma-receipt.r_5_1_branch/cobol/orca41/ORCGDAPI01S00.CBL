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
       PROGRAM-ID.         ORCGDAPI01S00.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : �ǡ��������å�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  16/10/31    NACL�����ġ���������
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
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY             PIC 9(04).
               05  WRK-DATE-FL1            PIC X(01).
               05  WRK-DATE-MM             PIC 9(02).
               05  WRK-DATE-FL2            PIC X(01).
               05  WRK-DATE-DD             PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH              PIC 9(02).
               05  WRK-HMS-MM              PIC 9(02).
               05  WRK-HMS-SS              PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH             PIC 9(02).
               05  WRK-TIME-FL1            PIC X(01).
               05  WRK-TIME-MM             PIC 9(02).
               05  WRK-TIME-FL2            PIC X(01).
               05  WRK-TIME-SS             PIC 9(02).
      *
      *    �ǡ��������å���ǧ����
           COPY    "CPDTCHKKOMOKU.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1101.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    API XML�������
           COPY    "CPAPIV3REQ.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA41SCRAREA.INC".
           COPY    "CPDCV3REQ.INC".
           COPY    "CPDCV3RES.INC".
           COPY    "COMMON-SPA".
           COPY    "D01COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               DCREQ-AREA
                                               DCRES-AREA
                                               SPA-AREA
                                               SPA-D01KYOUTU.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1101"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1101-REC
           IF    ( SYS-1101-INGAIPROCKBN   =   SPACE )
               MOVE    "1"         TO  SYS-1101-INGAIPROCKBN
           END-IF
           IF    ( SYS-1101-PRTSRTKBN      =   SPACE )
               MOVE    "1"         TO  SYS-1101-PRTSRTKBN
           END-IF
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKHOKEN-MAX )
      *
               MOVE    DTCHKHOKEN-CHKHOKENID (IDX0)
                                   TO  DCRES-HOKENID (IDX0)
               MOVE    DTCHKHOKEN-CHKHOKEN (IDX0)
                                   TO  DCRES-HOKENNAME (IDX0)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               MOVE    DTCHKKOMOKU-CHKKOMOKUID (IDX0)
                                   TO  DCRES-ITEMID (IDX0)
               MOVE    DTCHKKOMOKU-CHKKOMOKU (IDX0)
                                   TO  DCRES-ITEMNAME (IDX0)
      *
           END-PERFORM
      *
           MOVE    SYS-1101-INGAIPROCKBN
                                       TO  DCRES-INGAIPROCKBN
      *
           EVALUATE    SYS-1101-INGAIPROCKBN
           WHEN    "0"
               MOVE    "�ޤޤʤ�"      TO  DCRES-INGAIPROCNAME
           WHEN    "1"
               MOVE    "����������ޤ�"
                                       TO  DCRES-INGAIPROCNAME
           WHEN    "2"
               MOVE    "���������Τ�"  TO  DCRES-INGAIPROCNAME
           END-EVALUATE
      *
           MOVE    SYS-1101-PRTSRTKBN  TO  DCRES-PRTSRTKBN
      *
           EVALUATE    SYS-1101-PRTSRTKBN
           WHEN    "1"
               MOVE    "�����ֹ��"        TO  DCRES-PRTSRTNAME
           WHEN    "2"
               MOVE    "���ʻ�̾��"        TO  DCRES-PRTSRTNAME
           WHEN    "3"
               MOVE    "�ݸ��������ֹ��"  TO  DCRES-PRTSRTNAME
           WHEN    "4"
               MOVE    "�ݸ������ʻ�̾��"  TO  DCRES-PRTSRTNAME
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                         SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API01S02"      TO  SPA-MOTOPG
           CALL    "ORCGDERRMSG"   USING
                                   SPA-AREA
                                   SPA-D01KYOUTU
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    XML�����񤭽Ф�
      *****************************************************************
       900-XML-WRITE-SEC               SECTION.
      *
           MOVE    DCREQ-REQNUM        TO  DCRES-REQNUM
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  DCRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  DCRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO            TO  DCRES-API-RESULT
           ELSE
               MOVE    "E"             TO  DCRES-API-RESULT
               MOVE    SPA-ERRCD       TO  DCRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-D01-ERRMSG      TO  DCRES-API-RESULT-MSG
      *
           MOVE    "xml_data_checkv3res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    DCRES-AREA          TO  APIREQ-AREA
           MOVE    1                   TO  APIREQ-MODE
           MOVE    "data_checkv3res"    TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  DAPI01-BODY
               MOVE    "application/xml"
                                       TO  DAPI01-CONTENT-TYPE
            ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    �ģ¸���������FETCH�Ϥ��ʤ���
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *****************************************************************
      *    XMLWRITE����
      *****************************************************************
       910-XMLWRITE-SEC                 SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       APIREQ-AREA
      *
           .
       910-XMLWRITE-EXT.
           EXIT.