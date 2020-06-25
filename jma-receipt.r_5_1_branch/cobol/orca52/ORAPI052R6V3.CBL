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
       PROGRAM-ID.         ORAPI052R6V3.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : �ޥ����ǽ��������ֵ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  17/11/12    NACL�����ġ���������
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
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-TENSU               PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-REQUESTED           PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXL                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
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
           03  WRK-HISTORY-G.
               05  WRK-HLASTYMD        PIC X(8).
               05  WRK-HCOUNT          PIC 9(8).
           03  WRK-COUNT-Z9            PIC Z(7)9.
      *
       01  CONST-AREA.
           03  CONST-TENSU             PIC X(32) VALUE
                                       "medication_master".
           03  CONST-BYOMEI            PIC X(32) VALUE
                                       "disease_master".
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *
      *    API XML�ɤ߹����������
           COPY    "CPAPIV2REQ.INC".
           COPY    "CPMLUV3REQ.INC".
      *
      *    API XML�񤭹����������
           COPY    "CPMLUV3RES.INC".
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ���
           COPY    "CPTENSU.INC".
      *
      *    ��̾�ޥ���
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
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
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
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
           DISPLAY   "**************************"
           DISPLAY   "* masterlastupdate start *"
           DISPLAY   "**************************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
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
           DISPLAY   "**************************"
           DISPLAY   "* masterlastupdate end   *"
           DISPLAY   "**************************"
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
           INITIALIZE                  XML-MLURES
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    ���ŵ��ؼ����ֹ楻�å� 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"           TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    �ӣУ���������
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
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
      *    XML������Ф�
           PERFORM 900-XML-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
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
      *
      *****************************************************************
      *    ���Ϲ��ܥ����å�����
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           .
       2001-INPUT-CHK-EXT.
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
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-REQUESTED
           MOVE    ZERO                TO  IDXL
           MOVE    ZERO                TO  IDXL
      *
           IF    ( MLUREQ-MSTID        =   SPACE OR CONST-TENSU )
               MOVE    1               TO  FLG-REQUESTED
               PERFORM 20031-TENSU-SEC
           END-IF
      *
           IF    ( MLUREQ-MSTID        =   SPACE OR CONST-BYOMEI )
               MOVE    1               TO  FLG-REQUESTED
               PERFORM 20031-BYOMEI-SEC
           END-IF
      *
           IF    ( FLG-REQUESTED       =   ZERO )
               MOVE    "E01"           TO  SPA-ERRCD
           END-IF
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ����ǽ���������������
      *****************************************************************
       20031-TENSU-SEC                 SECTION.
      *
           IF    ( MLUREQ-MSTID        =   SPACE )
               PERFORM 900-TENSU-LAST-UPDATE1-SEL-SEC
      *
               COMPUTE IDXL = IDXL + 1
      *
               MOVE    TNS-TERMID(1:8) TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    TNS-TERMID(9:6) TO  WRK-HMS
               PERFORM 800-TIME-SEC
      *
               MOVE    CONST-TENSU     TO  MLURES-MSTID (IDXL)
               MOVE    WRK-DATE        TO  MLURES-LASTYMD (IDXL)
           ELSE
               MOVE    CONST-TENSU     TO  MLURES-SOLO-MSTID
               PERFORM 900-TENSU-LAST-UPDATE2-SEL-SEC
               PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
      *
                   COMPUTE IDXL = IDXL + 1
      *
                   MOVE    TNS-TERMID      TO  WRK-HISTORY-G
                   MOVE    WRK-HLASTYMD    TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE        TO  MLURES-HLASTYMD (IDXL)
                   IF    ( IDXL        =   1 )
                       MOVE    WRK-DATE    TO  MLURES-SOLO-LASTYMD
                   END-IF
                   MOVE    WRK-HCOUNT      TO  WRK-COUNT-Z9
                   MOVE    FUNCTION TRIM(WRK-COUNT-Z9)
                                           TO  MLURES-HCOUNT (IDXL)
                   PERFORM 900-TENSU-LAST-UPDATE2-FET-SEC
               END-PERFORM
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "last_update2"      TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       20031-TENSU-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�ޥ����ǽ���������������
      *****************************************************************
       20031-BYOMEI-SEC                SECTION.
      *
           IF    ( MLUREQ-MSTID        =   SPACE )
               PERFORM 900-BYOMEI-LAST-UPDATE1-SEL-SEC
      *
               COMPUTE IDXL = IDXL + 1
      *
               MOVE    BYO-TERMID(1:8) TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    BYO-TERMID(9:6) TO  WRK-HMS
               PERFORM 800-TIME-SEC
      *
               MOVE    CONST-BYOMEI    TO  MLURES-MSTID (IDXL)
               MOVE    WRK-DATE        TO  MLURES-LASTYMD (IDXL)
           ELSE
               MOVE    CONST-BYOMEI    TO  MLURES-SOLO-MSTID
               PERFORM 900-BYOMEI-LAST-UPDATE2-SEL-SEC
               PERFORM UNTIL ( FLG-BYOMEI  NOT =   ZERO )
      *
                   COMPUTE IDXL = IDXL + 1
      *
                   MOVE    BYO-TERMID      TO  WRK-HISTORY-G
                   MOVE    WRK-HLASTYMD    TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE        TO  MLURES-HLASTYMD (IDXL)
                   IF    ( IDXL        =   1 )
                       MOVE    WRK-DATE    TO  MLURES-SOLO-LASTYMD
                   END-IF
                   MOVE    WRK-HCOUNT      TO  WRK-COUNT-Z9
                   MOVE    FUNCTION TRIM(WRK-COUNT-Z9)
                                           TO  MLURES-HCOUNT (IDXL)
                   PERFORM 900-BYOMEI-LAST-UPDATE2-FET-SEC
               END-PERFORM
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "last_update2"      TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       20031-BYOMEI-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�ޥ����ǽ��������Խ�����
      *****************************************************************
       200311-BYOMEI-SEC               SECTION.
      *
           MOVE    BYO-TERMID(1:8) TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    BYO-TERMID(9:6) TO  WRK-HMS
           PERFORM 800-TIME-SEC
      *
           .
       200311-BYOMEI-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           PERFORM 900-XML-WRITE-SEC
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
           IF    ( WRK-SYMD            =   SPACE )
               MOVE    "00000000"      TO  WRK-SYMD
           END-IF
      *
           INITIALIZE                      WRK-DATE
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
      *    ������������
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
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
           IF    ( WRK-HMS             =   SPACE )
               MOVE    "000000"        TO  WRK-HMS
           END-IF
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
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0000"
               MOVE    "���ｪλ"
                                       TO  SPA-ERRMSG
               MOVE    "000"           TO  SPA-ERRCD
           WHEN    "E01"
               MOVE    "��������ޥ�����¸�ߤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "8900"
               MOVE    "�����ƥ���ܤ�����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "�������󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "���롼�װ��ŵ��ؤ�������Ǥ���" DELIMITED BY SIZE
                      "������λ���Ʋ�������"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "0097"
               MOVE    "�������Ƥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "E97"           TO  SPA-ERRCD
           WHEN    "0098"
               MOVE    "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
               MOVE    "E98"           TO  SPA-ERRCD
           WHEN    "0099"
               MOVE    "�桼���ɣĤ�̤��Ͽ�Ǥ���"
                                       TO  SPA-ERRMSG
               MOVE    "E99"           TO  SPA-ERRCD
           END-EVALUATE
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST06-BODY     NOT =   LOW-VALUE )
               DISPLAY "masterlastupdatev3 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_masterlastupdatev3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST06-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "masterlastupdatev3req"
                                       TO  APIREQ-RECORDNAME
           MOVE    "XMLREAD"           TO  MCP-FUNC
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               MOVE     XML-APIREQ     TO  XML-MLUREQ
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"           TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST06-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML�����񤭽Ф�
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  MLURES-API-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  MLURES-API-INFORMATION-TIME
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO        TO  SPA-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    SPA-ERRCD       TO  MLURES-API-RESULT
           MOVE    SPA-ERRMSG      TO  MLURES-API-RESULT-MSG
      *
           MOVE    "xml_masterlastupdatev3res"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    XML-MLURES      TO  XML-APIREQ
           MOVE    1               TO  APIREQ-MODE
           MOVE    "masterlastupdatev3res"   TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  APILST06-BODY
               MOVE    "application/xml"
                                       TO  APILST06-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ�����������
      *****************************************************************
       900-TENSU-LAST-UPDATE1-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-LAST-UPDATE1-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ�����������
      *****************************************************************
       900-TENSU-LAST-UPDATE2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-LAST-UPDATE2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ�����������
      *****************************************************************
       900-TENSU-LAST-UPDATE2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-LAST-UPDATE1-FET-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�ޥ�����������
      *****************************************************************
       900-BYOMEI-LAST-UPDATE1-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BYOMEI-LAST-UPDATE2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�ޥ�����������
      *****************************************************************
       900-BYOMEI-LAST-UPDATE2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           .
      *
       900-BYOMEI-LAST-UPDATE2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�ޥ�����������
      *****************************************************************
       900-BYOMEI-LAST-UPDATE2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           .
      *
       900-BYOMEI-LAST-UPDATE2-FET-EXT.
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
      *    �ץ�����ཪλ�����ʥ��顼����
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "0099" )
               MOVE    404             TO  APILST06-HTTPSTATUS
           ELSE
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.