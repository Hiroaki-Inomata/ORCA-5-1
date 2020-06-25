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
       PROGRAM-ID.         ORCGQAPI03S02.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : �����
      *  ����ݡ��ͥ��̾  : �ޥ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/10/31    NACL�����ġ���������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       WORKING-STORAGE                     SECTION.
      *
       01  FLG-AREA.
           03  FLG-APIPARA                 PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-TERM                    PIC X(64).
           03  WRK-WINDOW                  PIC X(64).
           03  WRK-ERRCD                   PIC X(04).
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
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
       01  CONST-AREA.
           03  CONST-FOUNDID           PIC X(08)   VALUE   "api01301".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
           COPY    "Q01SCR-SPA".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    API XML�񤭹����������
           COPY    "CPFOUNDV3RES.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    ���Уɥѥ�᥿
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA013.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
           COPY    "CPFOUNDV3REQ.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               XML-FOUNDV3REQ
                                               SPA-AREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *    INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPACE           TO  SPA-Q01FREE
           INITIALIZE                  SPA-Q01FREE
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
           INITIALIZE                  XML-FOUNDV3RES
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ���Ϲ��ܥ����å�����
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STATE-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���ֳ�ǧ����
      *****************************************************************
       2001-STATE-CHK-SEC              SECTION.
      *
           IF    ( FOUNDREQ-ORCA-UID   =   SPACE )
               MOVE    "0205"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( FOUNDREQ-KARTE-UID   =  SPACE )
               MOVE    "0203"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-APIPARA-KEY5-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA013-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   EVALUATE    PARA013-STATE
                   WHEN    "01"
                       MOVE    "1052"      TO  SPA-ERRCD
                       MOVE    "02"        TO  FOUNDRES-RESNUM
                   WHEN    "02"
                       MOVE    PARA013-NUMBER-OF-RECORDS
                                           TO  FOUNDRES-DATA-COUNT
                       MOVE    PARA013-NUMBER-OF-SHEETS
                                           TO  FOUNDRES-PRINT-COUNT
                   WHEN    OTHER
                       MOVE    "1054"      TO  SPA-ERRCD
                   END-EVALUATE
               ELSE
                       MOVE    "1053"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-STATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��Ϣ�����å�����
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �쥹�ݥ��Խ�����
      *****************************************************************
       2003-RESPONSE-EDIT-SEC              SECTION.
      *
      *
           .
       2003-RESPONSE-EDIT-EXT.
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
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
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
      *    ��������������
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC               SECTION.
      *
           MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCGQERRMSG"   USING
                                   SPA-AREA
                                   SPA-Q01FREE
           MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML�����񤭽Ф�
      *****************************************************************
       900-XML-WRITE-SEC               SECTION.
      *
           MOVE    FOUNDREQ-REQNUM     TO  FOUNDRES-REQNUM
           IF    ( SPA-ERRCD       =   ZERO  OR  SPACE )
               MOVE    "02"            TO  FOUNDRES-RESNUM
           ELSE
               IF    ( FOUNDRES-RESNUM =   SPACE )
                   MOVE    "01"        TO  FOUNDRES-RESNUM
               END-IF
           END-IF
      *
           MOVE    FOUNDREQ-KARTE-UID  TO  FOUNDRES-KARTE-UID
           MOVE    FOUNDREQ-ORCA-UID   TO  FOUNDRES-ORCA-UID
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  FOUNDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  FOUNDRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD           =   ZERO )
               MOVE    ZERO            TO  FOUNDRES-API-RESULT
           ELSE
               MOVE    "E"             TO  FOUNDRES-API-RESULT
               MOVE    SPA-ERRCD       TO  FOUNDRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-Q01-ERRMSG      TO  FOUNDRES-API-RESULT-MSG
      *
           MOVE    WRK-TIME            TO  FOUNDRES-INFORMATION-TIME
      *
           MOVE    "xml_foundv3res"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  FOUNDRES-MODE
           MOVE    "foundv3res"        TO  FOUNDRES-RECORDNAME
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-FOUNDV3RES
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    FOUNDRES-OBJECT TO  QAPI03-BODY
               MOVE    "application/xml"
                                       TO  QAPI03-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    API�ѥ�᥿��������
      *****************************************************************
       900-APIPARA-KEY5-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    CONST-FOUNDID       TO  APIPARA-GYOUMUID
           MOVE    FOUNDREQ-ORCA-UID   TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    FOUNDREQ-KARTE-UID   TO  APIPARA-KARTE-UID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY5-SEL-EXT.
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