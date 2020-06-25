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
       PROGRAM-ID.         ORAPI000R3V2.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : Push���μ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  18/01/25    NACL�����ġ���������
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
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-MPE                 PIC 9(01).
           03  FLG-TEXT-ERR            PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-RESPONSE            PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-TEXT                PIC X(512).
           03  WRK-DATETIME.
               05  WRK-DATETIME-DATE   PIC X(10).
               05  WRK-DATETIME-FL     PIC X(01).
               05  WRK-DATETIME-TIME   PIC X(05).
      *
       01  API-AREA.
           03  API-START-TIMESTAMP.
               05  API-START-TS-DATE   PIC X(10).
               05  API-START-TS-FL     PIC X(01).
               05  API-START-TS-TIME   PIC X(08).
           03  API-END-TIMESTAMP.
               05  API-END-TS-DATE     PIC X(10).
               05  API-END-TS-FL       PIC X(01).
               05  API-END-TS-TIME     PIC X(08).
      *
           COPY    "CPPEGV2REQ.INC".
           COPY    "CPPEGV2ERR.INC".
           COPY    "CPPEGV2RES.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    ʸ��������å�
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML�������
           COPY    "CPAPIV2REQ.INC".
           COPY    "CPAPIMSG.INC".
           COPY    "CPIMGGETV2REQ.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      * MonPushEvent
        01 MPE-REC.
           COPY    "CPMPE.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*********************"
           DISPLAY   "* pusheventgetv2 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  XML-PEGREQ
           INITIALIZE                  XML-PEGERR
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           IF    ( SPA-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           DISPLAY   "*********************"
           DISPLAY   "* pusheventgetv2 end  *"
           DISPLAY   "*********************"
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
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    ���ŵ��ؼ����ֹ楻�å�
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    �ӣУ���������
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"           TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  SPA-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  SPA-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  SPA-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  SPA-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  SPA-ERRCD
               END-EVALUATE
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               CONTINUE
           ELSE
               MOVE    "0099"          TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML������Ф�
           PERFORM 900-XML-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ��ʸ���ܥ����å�����
      *****************************************************************
       1001-INIT-CHECK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-EVENT-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-USER-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-START-TIME-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-END-TIME-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-KANREN-CHECK-SEC
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    ���٥��̾�����å�����
      *****************************************************************
       10011-EVENT-CHECK-SEC           SECTION.
      *
           MOVE    PEGREQ-EVENT    TO  WRK-TEXT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-TEXT-ERR    NOT =   ZERO )
               MOVE    "0001"      TO  SPA-ERRCD
           END-IF
      *
           .
       10011-EVENT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    �桼��̾�����å�����
      *****************************************************************
       10011-USER-CHECK-SEC            SECTION.
      *
           MOVE    PEGREQ-USER     TO  WRK-TEXT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-TEXT-ERR    NOT =   ZERO )
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
       10011-USER-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    �����������������å�����
      *****************************************************************
       10011-START-TIME-CHECK-SEC      SECTION.
      *
           MOVE    "1900-01-01 00:00:00"   TO  API-START-TIMESTAMP
      *
           IF    ( PEGREQ-START-TIME (1:10)    NOT =   SPACE )
               MOVE    PEGREQ-START-TIME   TO  WRK-DATETIME
               PERFORM 100111-TIME-CHECK-SEC
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( WRK-DATETIME-DATE   NOT =   SPACE )
                       MOVE    WRK-DATETIME-DATE
                                           TO  API-START-TS-DATE
                       IF    ( WRK-DATETIME-TIME
                                               NOT =   SPACE )
                           MOVE    WRK-DATETIME-TIME
                                           TO  API-START-TS-TIME
                           MOVE    ":00"   TO  API-START-TS-TIME(6:)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       10011-START-TIME-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    ������λ���������å�����
      *****************************************************************
       10011-END-TIME-CHECK-SEC        SECTION.
      *
           MOVE    "9999-12-31 23:59:59"   TO  API-END-TIMESTAMP
      *
           IF    ( PEGREQ-END-TIME (1:10)    NOT =   SPACE )
               MOVE    PEGREQ-END-TIME     TO  WRK-DATETIME
               PERFORM 100111-TIME-CHECK-SEC
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( WRK-DATETIME-DATE   NOT =   SPACE )
                       MOVE    WRK-DATETIME-DATE
                                           TO  API-END-TS-DATE
                       IF    ( WRK-DATETIME-TIME
                                               NOT =   SPACE )
                           MOVE    WRK-DATETIME-TIME
                                           TO  API-END-TS-TIME
                           MOVE    ":59"   TO  API-END-TS-TIME(6:)
                       END-IF
                   END-IF
               ELSE
                   EVALUATE    SPA-ERRCD
                   WHEN    "0003"
                       MOVE    "0005"      TO  SPA-ERRCD
                   WHEN    "0004"
                       MOVE    "0006"      TO  SPA-ERRCD
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       10011-END-TIME-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    ���������å�����
      *****************************************************************
       100111-TIME-CHECK-SEC           SECTION.
      *
           MOVE    WRK-DATETIME-DATE
                                   TO  WRK-DATE
           PERFORM 800-SYMD-SEC
           PERFORM 800-HIZUKE-SEC
           IF    ( WRK-EDTYMD1 =   SPACE )
               MOVE    "0003"  TO  SPA-ERRCD
           ELSE
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  WRK-DATETIME-DATE
      *
               IF    ( WRK-DATETIME-TIME
                                   NOT =   SPACE )
                   IF    ( WRK-DATETIME-TIME (1:2)    IS NUMERIC )
                    AND  ( WRK-DATETIME-TIME (4:2)    IS NUMERIC )
                    AND  ( WRK-DATETIME-TIME (1:2)    >=  "00" )
                    AND  ( WRK-DATETIME-TIME (1:2)    <=  "23" )
                    AND  ( WRK-DATETIME-TIME (4:2)    >=  "00" )
                    AND  ( WRK-DATETIME-TIME (4:2)    <=  "59" )
                       MOVE    ":"     TO  WRK-DATETIME-TIME(3:1)
                   ELSE
                       MOVE    "0004"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       100111-TIME-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    ��Ϣ�����å�����
      *****************************************************************
       10011-KANREN-CHECK-SEC          SECTION.
      *
           IF    ( API-START-TIMESTAMP >   API-END-TIMESTAMP )
               MOVE    "0008"          TO  SPA-ERRCD
           END-IF
      *
           .
       10011-KANREN-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-RESPONSE-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥹�ݥ��Խ�����
      *****************************************************************
       2001-RESPONSE-EDIT-SEC                  SECTION.
      *
           MOVE    ZERO                    TO  CNT-RESPONSE
      *
           INITIALIZE                          XML-APIMSG
           MOVE    3                       TO  APIMSG-MODE
           MOVE    "xml_pusheventgetv2res" TO  MCP-TABLE
           MOVE    LOW-VALUE               TO  MCP-PATHNAME
           PERFORM 910-MSGOPEN-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               DISPLAY "MSGOPEN failure = " MCP-RC
           END-IF
      *
           PERFORM 900-MONPUSHEVENT-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MPE     NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               INITIALIZE                      XML-PEGRES
      *        MOVE    MPE-ID              TO  PEGRES-ID
      *        MOVE    MPE-EVENT           TO  PEGRES-EVENT
      *        MOVE    MPE-PUSHED-AT-DATE  TO  WRK-SYMD
      *        PERFORM 800-DATE-SEC
      *        MOVE    MPE-PUSHED-AT-TIME  TO  WRK-HMS
      *        PERFORM 800-TIME-SEC
      *        STRING  MPE-PUSHED-AT-DATE      DELIMITED BY SIZE
      *                " "                     DELIMITED BY SIZE
      *                MPE-PUSHED-AT-TIME(1:8) DELIMITED BY SIZE
      *        INTO    PEGRES-TIME
      *        END-STRING
      *
               MOVE    MPE-DATA            TO  PEGRES-DATA
               MOVE    XML-PEGRES          TO  XML-APIMSG
               MOVE    "xml_pusheventgetv2res"
                                           TO  MCP-TABLE
               MOVE    LOW-VALUE           TO  MCP-PATHNAME
               PERFORM 910-MSGWRITE-SEC
               IF    ( MCP-RC              =   ZERO )
                   COMPUTE  CNT-RESPONSE   =   CNT-RESPONSE + 1
               ELSE
                   MOVE    "4001"          TO  SPA-ERRCD
                   DISPLAY "MSGWRITE failure = " MCP-RC
               END-IF
      *
               PERFORM 900-MONPUSHEVENT-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "monpushevent"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                          XML-APIMSG
           MOVE    "xml_pusheventgetv2res" TO  MCP-TABLE
           MOVE    LOW-VALUE               TO  MCP-PATHNAME
           PERFORM 910-MSGUNESCAPE-SEC
           PERFORM 910-MSGCLOSE-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    APIMSG-OBJECT       TO  APILST37-BODY
               MOVE    "application/json"  TO  APILST37-CONTENT-TYPE
           ELSE
               DISPLAY "MSGCLOSE failure = " MCP-RC
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    "4002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( CNT-RESPONSE        =   ZERO )
                   MOVE    "0007"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 9901-ERR-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *
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
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
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
      *    �����Խ�����
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
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
      *
      *****************************************************************
      *    ������������
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
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
      *    Ⱦ��ʸ�������å�
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-TEXT-ERR
      *
           IF  ( WRK-TEXT          NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-TEXT        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-TEXT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1       TO  FLG-TEXT-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-TEXT-ERR
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC               SECTION.
      *
           MOVE        SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "���٥��̾��Ⱦ�Ѱʳ���ʸ�������Ϥ���Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "�桼��̾��Ⱦ�Ѱʳ���ʸ�������Ϥ���Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "���ֳ������������դ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "���ֳ��������λ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "���ֽ�λ���������դ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "���ֽ�λ�����λ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "�оݤȤʤ�Уգӣ����Τ�¸�ߤ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "���ֳ�����������ֽ�λ�����Ǥ���"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4002"
               MOVE    "���������˼��Ԥ��ޤ�����"
                                       TO  SPA-ERRMSG
      *���̥��顼
           WHEN    "8900"
               MOVE    "�����ƥ���ܤ�����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "�������󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "���롼�װ��ŵ��ؤ�������Ǥ���" DELIMITED BY SIZE
                      "������λ���Ʋ�������"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0097"
               MOVE    "�������Ƥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE    "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "�桼���ɣĤ�̤��Ͽ�Ǥ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONPUSHEVENT��������
      *****************************************************************
       900-MONPUSHEVENT-KEY2-SEL-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-MPE
      *
           INITIALIZE                  MPE-REC
           MOVE    PEGREQ-EVENT    TO  MPE-EVENT
           MOVE    PEGREQ-USER     TO  MPE-USER
           IF    ( API-START-TIMESTAMP NOT =   SPACE )
               STRING API-START-TIMESTAMP  DELIMITED BY SIZE
                      " JST"               DELIMITED BY SIZE
               INTO   MPE-PUSHED-FROM
               END-STRING
           END-IF
           IF    ( API-END-TIMESTAMP   NOT =   SPACE )
               STRING API-END-TIMESTAMP    DELIMITED BY SIZE
                      " JST"               DELIMITED BY SIZE
               INTO   MPE-PUSHED-TO
               END-STRING
           END-IF
           MOVE    MPE-REC         TO  MCPDATA-REC
           MOVE    "monpushevent"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  MPE-REC
           ELSE
               MOVE    1           TO  FLG-MPE
               INITIALIZE              MPE-REC
           END-IF
      *
           .
       900-MONPUSHEVENT-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    MONPUSHEVENT��������
      *****************************************************************
       900-MONPUSHEVENT-KEY2-FET-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-MPE
      *
           MOVE    "monpushevent"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  MPE-REC
           ELSE
               MOVE    1           TO  FLG-MPE
               INITIALIZE              MPE-REC
           END-IF
      *
           .
       900-MONPUSHEVENT-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST37-BODY    NOT =   LOW-VALUE )
               DISPLAY "pusheventgetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    "xml_pusheventgetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST37-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "pusheventgetv2req" TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"           TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST37-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML�ե��륿��󥰽���
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  PEGREQ-AREA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML�����񤭽Ф��ʥ��顼����
      *****************************************************************
       900-XML-WRITE-SEC           SECTION.
      *
           IF    ( APILST37-BODY    NOT =   LOW-VALUE )
               DISPLAY "pusheventgetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    PEGERR-AREA             TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_pusheventgetv2err" 
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    3                       TO  APIREQ-MODE
           MOVE    "error"                 TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO )
               MOVE    APIREQ-OBJECT       TO  APILST37-BODY
               MOVE    "application/xml"   TO  APILST37-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
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
      *    �أͣ��ɹ�����
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    �أͣ̽������
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    �ͣӣǥ����ץ����
      *****************************************************************
       910-MSGOPEN-SEC                 SECTION.
      *
           MOVE    "MSGOPEN"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGOPEN-EXT.
           EXIT.
      *****************************************************************
      *    �ͣӣǽ������
      *****************************************************************
       910-MSGWRITE-SEC                SECTION.
      *
           MOVE    "MSGWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGWRITE-EXT.
           EXIT.
      *****************************************************************
      *    �ͣӣǥ��󥨥������׽���
      *****************************************************************
       910-MSGUNESCAPE-SEC             SECTION.
      *
           MOVE    "MSGUNESCAPE"   TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGUNESCAPE-EXT.
           EXIT.
      *****************************************************************
      *    �ͣӣǥ�����������
      *****************************************************************
       910-MSGCLOSE-SEC                SECTION.
      *
           MOVE    "MSGCLOSE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����ཪλ�����ʥ��顼����
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "0099" )
               MOVE    404             TO  APILST37-HTTPSTATUS
           ELSE
               PERFORM 9901-ERR-RESPONSE-EDIT-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    ���顼�����Ͻ���
      *****************************************************************
       9901-ERR-RESPONSE-EDIT-SEC      SECTION.
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    SPA-ERRCD           TO  PEGERR-CODE
           MOVE    SPA-ERRMSG          TO  PEGERR-MESSAGE
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-ERR-RESPONSE-EDIT-EXT.
           EXIT.