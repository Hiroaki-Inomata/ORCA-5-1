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
       PROGRAM-ID.         ORAPI105R1V2.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : �ݸ��ԥޥ�������  (xml2)
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  13/10/XX    NACL��¿��Ǽ����������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TEND                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNJAINF            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  TBL-MAX             PIC 9(04).
      *
      *    ��������ΰ�
      *01  CNT-AREA.
      *    03  CNT-SRYCNT              PIC 9(04).
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
      *    ���顼������
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYM           PIC X(06).
               05  WRK-SYSDD           PIC X(02).
      *
           03  WRK-RATE                PIC ZZZ.
      *
       01  WRK-XML-AREA.
           03  WRK-HKNJANUM               PIC X(08).
           03  WRK-HKNNUM                 PIC X(03).
      *
      *    Ⱦ�������Ѵ�
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
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
           COPY    "CPORCSKANACHK.INC".
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    API XML�ɤ߹����������
           COPY    "CPINSPROGETV2REQ.INC".
      *    API XML�񤭹����������
           COPY    "CPINSPROGETV2RES.INC".
      *ver.4.7
      *    API XML�ɤ߹��߶������
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    ��������
           COPY  "CPSK1010.INC".
      *
      *    �ݸ��ֹ�ޥ�����
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    �ݸ��ֹ�ޥ���
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
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
           DISPLAY   "********************"
           DISPLAY   "* insproget  start*"
           DISPLAY   "********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      **** INITIALIZE                      CNT-AREA
      *
      *    �������
           PERFORM 100-INIT-SEC
      *    �����
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           IF      WRK-ERRCD           =   "99"
      *        �桼���ɣĥ��顼
               MOVE   404                  TO  APILST15-HTTPSTATUS
           ELSE
      *        �ֵ��ΰ��Խ�
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* insproget  end *"
           DISPLAY   "*******************"
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
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-INSPROGETRES
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  INSPRORES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    ���ա����ֽ����Խ�
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  INSPRORES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  INSPRORES-INFORMATION-TIME
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
      *    �ӣУ���������
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML������Ф�
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    ���Ϲ��ܥ����å�����
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    �ݸ��ԥޥ����������
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-SYORI-01-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST15-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML������Ф�
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_insprogetv2req"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST15-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "insprogetreq"      TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO   1000-APTXMLREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_insprogetv2req"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST15-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "insprogetreq"      TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-INSPROGETREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML����񤭹��߽���
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      *    ���顼��å������Խ�
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  INSPRORES-API-RESULT
               MOVE    "������λ"          TO  INSPRORES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        ���顼���ٹ��å�����
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  INSPRORES-API-RESULT
               MOVE    WRK-ERRMSG          TO  INSPRORES-API-RESULT-MSG
           END-IF
      *
           PERFORM 3001-TMDCXMLMAKE-SEC
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML����񤭹��߽���
      *****************************************************************
       3001-TMDCXMLMAKE-SEC        SECTION.
      *
           IF      APILST15-BODY   NOT =   LOW-VALUE
               DISPLAY "INSPRORES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_insprogetv2res"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  INSPRORES-MODE
           MOVE    "insprogetres"      TO  INSPRORES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-INSPROGETRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_insprogetv2res"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  INSPRORES-MODE
           MOVE    "insprogetres"      TO  INSPRORES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-INSPROGETRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    INSPRORES-OBJECT    TO  APILST15-BODY 
               MOVE    "application/xml"   TO  APILST15-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-TMDCXMLMAKE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �ɤ߹����XML��LOW-VALUE ��  SPACE ���ִ�
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      INSPROREQ-INSPRO-NUMBER   =   LOW-VALUE
               MOVE  SPACE             TO  INSPROREQ-INSPRO-NUMBER
           END-IF
           IF     INSPROREQ-INS-NUMBER =   LOW-VALUE
               MOVE  SPACE             TO  INSPROREQ-INS-NUMBER
           END-IF
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Դ��ܾ����������ƥ����å�
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    �Խ�����
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    ����ʤ�
           IF     (WRK-ERRCD           =   SPACE )
           AND    (WRK-HKNJANUM        =   SPACE )
           AND    (WRK-HKNNUM          =   SPACE )
               MOVE    "10"            TO  WRK-ERRCD
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ϲ����Ѵ��Խ�����
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    �ݸ����ֹ�
           MOVE    INSPROREQ-INSPRO-NUMBER   TO  WRK-HKNJANUM
      *    �ݸ��ֹ�
           MOVE    INSPROREQ-INS-NUMBER      TO  WRK-HKNNUM
      *
           MOVE    INSPROREQ-INSPRO-NUMBER   TO  INSPRORES-INSPRO-NUMBER
                                                                   (1)
           MOVE    INSPROREQ-INS-NUMBER      TO  INSPRORES-INS-NUMBER
                                                                   (1)
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ԥޥ����Խ�����
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    ���������Խ�
           PERFORM 20021-INSPRORES-HEN-SEC
      *
           INITIALIZE                  HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM        TO  HKNJA-HKNJANUM
           MOVE    WRK-HKNNUM          TO  HKNJA-HKNNUM
           IF     (WRK-HKNJANUM    NOT =   SPACE)
               MOVE    "key"               TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    ZERO                TO  TBL-MAX
           PERFORM
               UNTIL   (FLG-HKNJAINF       =   1 )  OR
                       (WRK-ERRCD      NOT =   SPACE)
      *
               ADD     1               TO  TBL-MAX
               IF      TBL-MAX         >   2500
                   MOVE    "14"            TO  WRK-ERRCD
               ELSE
      *            ���ٹ����Խ�
                   PERFORM 20021-HKNJAINF-HENSYU-SEC
               END-IF
      *
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-HKNJAINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      TBL-MAX             =   ZERO
      *        �оݤʤ�
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       20021-INSPRORES-HEN-SEC     SECTION.
      *
           .
       20021-INSPRORES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ԥޥ����ǡ��������Խ�����
      *****************************************************************
       20021-HKNJAINF-HENSYU-SEC     SECTION.
      *
           ADD    1                    TO  IDX
      *
      *    �ݸ����ֹ�
           MOVE    HKNJA-HKNJANUM      TO  INSPRORES-INSPRO-NUMBER (IDX)
      *    �ݸ���̾
           MOVE    HKNJA-HKNJANAME     TO  INSPRORES-INSPRO-NAME (IDX)
      *    �ݸ���̾ά�Σ�
           MOVE    HKNJA-HKNJANAME-TAN1
                                       TO  INSPRORES-INSPRO-NAME1 (IDX)
      *    �ݸ���̾ά�Σ�
           MOVE    HKNJA-HKNJANAME-TAN2
                                       TO  INSPRORES-INSPRO-NAME2 (IDX)
      *    �ݸ���̾ά�Σ�
           MOVE    HKNJA-HKNJANAME-TAN3
                                       TO  INSPRORES-INSPRO-NAME3 (IDX)
      *    �ݸ��ֹ�
           MOVE    HKNJA-HKNNUM        TO  INSPRORES-INS-NUMBER (IDX)
           PERFORM 200211-HKNNUM-HEN-SEC
           MOVE    HKN-TANSEIDONAME    TO  INSPRORES-INS-NUMBER-NAME
                                                               (IDX)
      *    ͹���ֹ�
           MOVE    HKNJA-POST          TO  INSPRORES-INSE-ZIP-CODE (IDX)
      *    ����
           MOVE    HKNJA-ADRS          TO  INSPRORES-INS-ADDRESSES1
                                                               (IDX)
      *    ��������
           MOVE    HKNJA-BANTI         TO  INSPRORES-INS-ADDRESSES2
                                                               (IDX)
      *    �����ֹ�
           MOVE    HKNJA-TEL           TO  INSPRORES-INS-HPONE (IDX)
      *    ����
           MOVE    HKNJA-KIGO          TO  INSPRORES-INS-SYMBOL (IDX)
      *
      *    ���ճ����ܿ͡������
           MOVE    HKNJA-HON-GAIKYURATE    TO  WRK-RATE
           MOVE    WRK-RATE            TO  INSPRORES-HON-GAIKYURATE(IDX)
      *    ���ճ����ܿ͡�������
           MOVE    HKNJA-HON-NYUKYURATE    TO  WRK-RATE
           MOVE    WRK-RATE            TO  INSPRORES-HON-NYUKYURATE(IDX)
      *    ���ճ��ʲ�²�������
           MOVE    HKNJA-KZK-GAIKYURATE    TO  WRK-RATE
           MOVE    WRK-RATE            TO  INSPRORES-KZK-GAIKYURATE(IDX)
      *    ���ճ��ʲ�²��������
           MOVE    HKNJA-KZK-NYUKYURATE    TO  WRK-RATE
           MOVE    WRK-RATE            TO  INSPRORES-KZK-NYUKYURATE(IDX)
      *    ��ư��ʬ
           EVALUATE    HKNJA-IDOKBN
               WHEN    "1"
                   MOVE    "�ѻ�"
                                       TO  INSPRORES-CHANGE-MEMO (IDX)
               WHEN    "2"
                   MOVE    "�����ѻ�"
                                       TO  INSPRORES-CHANGE-MEMO (IDX)
               WHEN    "3"
                   MOVE    "��"
                                       TO  INSPRORES-CHANGE-MEMO (IDX)
               WHEN    "4"
                   MOVE    "��ʻ�����"
                                       TO  INSPRORES-CHANGE-MEMO (IDX)
           END-EVALUATE
      *    ��ưǯ����
           IF      HKNJA-IDOYMD    NOT =   SPACE
               MOVE    HKNJA-IDOYMD        TO  WRK-SYMD
               MOVE    SPACE               TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  INSPRORES-CHANGE-DATE
                                                              (IDX)
           END-IF
      *
           .
       20021-WKSRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ�����å�
      *****************************************************************
       200211-HKNNUM-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  HKNNUM-REC
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNJA-HKNNUM        TO  HKN-HKNNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
                   MOVE    ZERO                TO  FLG-HKNNUM
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
                   INITIALIZE                  HKNNUM-REC
               END-IF
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200211-HKNNUM-HEN-EXT.
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
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "10"
               MOVE    "�ݸ����ֹ桦�ݸ��ֹ�λ��꤬����ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "�оݤ�����������ʾ�¸�ߤ��ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "�оݤ�����ޤ���"
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
               MOVE   "�������Ƥ˸��꤬����ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ���"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "�桼���ɣ�̤��Ͽ��"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ԥޥ����ɹ�
      *****************************************************************
       910-HKNJAINF-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
               MOVE    ZERO            TO  FLG-HKNJAINF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           .
       910-HKNJAINF-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����ƥ�����ޥ�����������
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    �������륯������
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ������ңţ���
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
      *