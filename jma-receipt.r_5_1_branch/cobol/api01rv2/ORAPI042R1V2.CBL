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
       PROGRAM-ID.         ORAPI042R1V2.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : �쥻�ץȾ���ʥ쥻�ץȤ�����������˼���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  16/01/08    NACL��ƣ��������������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SEIKYU              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDH                     PIC 9(04).
           03  IDH2                    PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    �����Խ�
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
           03  WRK-YMD-01.
               05  WRK-YY-01           PIC 9(04).
               05  WRK-MM-01           PIC 9(02).
               05  WRK-DD-01           PIC 9(02).
      *
           03  WRK-Z9-X.
               05  WRK-Z9              PIC ZZZZZZZZZ9.
      *    ���顼������
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-CNT                 PIC 9(03).
           03  WRK-ERR-CNT             PIC 9(03).
      *
           03  WRK-MONTH               PIC X(06).
           03  WRK-POINT               PIC 9(10).
           03  WRK-COUNT               PIC 9(06).
      *
       01  WRK-XML-AREA.
           03  WRK-BASE-DATE           PIC X(10).
           03  WRK-BASE-YMD            PIC X(08).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    ���ե��֥롼�����ΰ�
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    ���������Ѵ�����
           COPY    "CPORCSGDAY.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    API XML�ɤ߹����������
           COPY    "CPRECEIPT-INF1V2REQ.INC".
      *    API XML�ɤ߹����������
           COPY    "CPRECEIPT-INF1V2RES.INC".
      *
      *    API XML�ɤ߹��߶������
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �������
           COPY    "CPRCF010.INC".
      *
      *    ��������ʿ���
           COPY    "CPSKYMAIN.INC".
      *
      *    ��������ʸ�������
           COPY    "CPSKYKOH.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
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
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "********************"
           DISPLAY   "* patient res start*"
           DISPLAY   "********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
      *
      *    �������
           PERFORM 100-INIT-SEC
      *    �����
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        �桼���ɣĥ��顼
               MOVE   404                  TO  APILST22-HTTPSTATUS
           ELSE
      *        �ֵ��ΰ��Խ�
               PERFORM 300-PTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* patient res end *"
           DISPLAY   "*******************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-RECEIPT-INFRES
           INITIALIZE                      XML-APIREQ
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  RECEIPT-INFS-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    ���ա����ֽ����Խ�
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  RECEIPT-INFS-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  RECEIPT-INFS-INFORMATION-TIME
      *
      *    ���ŵ��ؼ����ֹ楻�å� 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML������Ф�
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  RECEIPT-INFS-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
      *    ���Ϲ��ܥ����å�����
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    �ֵѽ���
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-RECEIPT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST22-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
                ",APILST22-BODY:"  APILST22-BODY
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_receipt_inf1v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST22-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "receipt_infreq"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-RECEIPT-INFREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɤ߹����XML��LOW-VALUE ��  SPACE ���ִ�
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *****************************************************************
      *    �������ƥ����å�
      *****************************************************************
       2001-INPUT-CHK-SEC          SECTION.
      *
      *    ����ǯ��
           MOVE    RECEIPT-INFQ-PERFORM-MONTH
                                       TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD (1:6)      TO  WRK-MONTH
           IF      WRK-MONTH           =   SPACE
               MOVE    RECEIPT-INFS-INFORMATION-DATE (1:7)
                                           TO
                                           RECEIPT-INFQ-PERFORM-MONTH
               MOVE    SMCNDATE-YMD (1:6)  TO  WRK-MONTH
           END-IF
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    "1"             TO  SGDAY-INTYPE
           MOVE    WRK-MONTH       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"      USING   ORCSGDAYAREA
           IF      SGDAY-RC        =   ZERO
               CONTINUE
           ELSE
               MOVE    "10"            TO  WRK-ERRCD
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȾ���ʥ쥻�ץȤ���������������˽���
      *****************************************************************
       2002-RECEIPT-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-POINT
                                           WRK-COUNT
      * 
      *    ���ݸ�����Υ쥻�ץ�
           INITIALIZE                      SKYMAIN-REC
           MOVE    SPA-HOSPNUM         TO  SKYMAIN-HOSPNUM
           MOVE    WRK-MONTH           TO  SKYMAIN-SRYYM
           MOVE    "tbl_seikyu_main"   TO  WRK-MCP-TABLE
           MOVE    "key85"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-READ-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  SKYMAIN-REC
               ADD     SKYMAIN-TOTALTEN    TO  WRK-POINT
               ADD     SKYMAIN-FTNMONEY    TO  WRK-COUNT
           END-IF
      *
      *    ����ñ�ȥ쥻�ץ�
           INITIALIZE                      SKYMAIN-REC
           MOVE    SPA-HOSPNUM         TO  SKYMAIN-HOSPNUM
           MOVE    WRK-MONTH           TO  SKYMAIN-SRYYM
           MOVE    "tbl_seikyu_main"   TO  WRK-MCP-TABLE
           MOVE    "key86"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-READ-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  SKYMAIN-REC
               ADD     SKYMAIN-TOTALTEN    TO  WRK-POINT
               ADD     SKYMAIN-FTNMONEY    TO  WRK-COUNT
           END-IF
      *
           MOVE    RECEIPT-INFQ-PERFORM-MONTH
                                       TO  RECEIPT-INFS-PERFORM-MONTH
           MOVE    WRK-POINT           TO  WRK-Z9
           MOVE    WRK-Z9-X            TO  RECEIPT-INFS-RECEIPT-POINT
           MOVE    WRK-COUNT           TO  WRK-Z9
           MOVE    WRK-Z9-X (5:6)      TO  RECEIPT-INFS-RECEIPT-COUNT
      *
           IF      WRK-COUNT           =   ZERO
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
           .
       2002-RECEIPT-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML����񤭹��߽���
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
      *    ���顼��å������Խ�
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        ���ｪλ
               MOVE    "������λ"      TO  RECEIPT-INFS-API-RESULT-MSG
           ELSE
      *        ���顼���ٹ��å�����
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  RECEIPT-INFS-API-RESULT
               MOVE    WRK-ERRMSG      TO  RECEIPT-INFS-API-RESULT-MSG
           END-IF
      *
           IF      APILST22-BODY     NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_receipt_inf1v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  RECEIPT-INFS-MODE
           MOVE    "receipt_infres"    TO  RECEIPT-INFS-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-RECEIPT-INFRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    RECEIPT-INFS-OBJECT     TO  APILST22-BODY 
               MOVE    "application/xml"   TO  APILST22-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ѵ��Խ�����
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "10"
               MOVE    "����ǯ������ǤϤ���ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "�ؼ����줿����ǯ��ˤϡ��оݤ�����ޤ���"
                                               TO  WRK-ERRMSG
      *���̥��顼
           WHEN    "89"
      *        ORCSCOMMON �Υ��顼
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "�������󤬼����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "���롼�װ��ŵ��ؤ�������Ǥ���"
                           "������λ���Ʋ�������"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "�����ƥ���ܤ�����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "¾ü��������"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "������ʬ̤����"        TO  WRK-ERRMSG
           WHEN    "97"
               MOVE    "�������Ƥ˸��꤬����ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ���"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "�桼���ɣ�̤��Ͽ��"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ��������ɹ�����
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    SKYMAIN-REC         TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING   MCPAREA
                                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               CALL    "MONFUNC"           USING   MCPAREA
                                                   MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                       TO  FLG-SEIKYU
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING   MCPAREA
                                               MCPDATA-REC
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.