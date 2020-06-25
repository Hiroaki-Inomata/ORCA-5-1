       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBMIGADRS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���åȥ��å׽���
      *  ����ݡ��ͥ��̾  : ����ޥ�������
      *                    : ����ޥ���ʬΥ��ȼ���桼����������
      *                    : �������ƤˤĤ��Ƥϥ桼�����ѤΥơ��֥�Ȥ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  17/10/27    NACL-��ƣ     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���ϡ��ץ롼�եե�����
           SELECT  PRF-FILE        ASSIGN
                                   ASS-PRF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���ϡ��ץ롼�եե�����
       FD  PRF-FILE.
       01  PRF-REC                     PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *    �ե���������ΰ�
       01  ASS-AREA.
           03  ASS-PRF                 PIC X(256).
       01  STS-AREA.
           03  STS-PRF                 PIC X(02).
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ADRSM               PIC 9(01).
           03  FLG-ADRSU               PIC 9(01).
           03  FLG-ADRSO               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ADRSO               PIC 9(06).
           03  CNT-ADRSM               PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-LIMIT-PRF  PIC 9(05).
               05  WRK-PARA-FILE       PIC X(256).
           03  WRK-MSG                 PIC X(256).
           03  WRK-MSG-CNT             PIC ZZZ,ZZ9.
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-LIMIT-PRF           PIC 9(05).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-PRFCD               PIC 9(02).
      *    ��ӥơ��֥�
       01  TBL-COMP-AREA.
           03  TBL-COMP                PIC 9(01) OCCURS    11.
      *    �ץ롼���Խ��ΰ�
       01  PRF-AREA.
           03  PRF-HEADER1.
               05  FILLER              PIC X(35) VALUE
                   "����ޥ����ܹԽ�����ORCBMIGADRS��".
               05  PRF-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE "/".
               05  PRF-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE "/".
               05  PRF-TITLE1-DD       PIC X(02) VALUE SPACE.
           03  PRF-HEADER2.
               05  FILLER              PIC X(01) VALUE "[".
               05  PRF-HOSPNUM         PIC 9(02).
               05  FILLER              PIC X(01) VALUE "]".
               05  PRF-HOSPNAME        PIC X(120) VALUE SPACE.
           03  PRF-HEADER3.
               05  FILLER              PIC X(04) VALUE "����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(10) VALUE "����������".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(08) VALUE "͹���ֹ�".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(14) VALUE
                               "�桼�����꽻��".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(12) VALUE
                               "ɸ���󶡽���".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "����".
           03  PRF-DETAIL.
               05  PRF-COMMAND         PIC X(20).
               05  PRF-LPUBCD          PIC X(05).
               05  PRF-POST.
                   07  PRF-POST1       PIC X(03).
                   07  PRF-POSTH       PIC X(01).
                   07  PRF-POST2       PIC X(04).
               05  PRF-EDANUM          PIC 9(02).
               05  PRF-ADRS-NAMEU      PIC X(320).
               05  PRF-ADRS-NAMEM      PIC X(320).
               05  PRF-COMMENT         PIC X(200).
       01  CONST-AREA.
           03  CONST-COMM1             PIC X(38) VALUE
           "�桼�����꽻������Ƥ��ѹ�����ޤ�����".
           03  CONST-COMM2.
               05  CONST-COMM2-1       PIC X(36) VALUE
               "�桼�����꽻������Ƥ��ɲä���ޤ���".
               05  CONST-COMM2-2       PIC X(28) VALUE
               "��ɸ���󶡤ˤϤ���ޤ���ˡ�".
      *****************************************************************
      *    ���֥ץ������
      *****************************************************************
      *    �ޥ������ռ���
           COPY    "CPORCSMCNDATE.INC".
      *    ����ե�����̾�Խ�
      *    COPY    "CPORCSGETTEMP.INC".
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *    ����ܹ����ơ��֥�
       01  ADRSO-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSO-//.
      *    ����ޥ������ơ��֥�
       01  ADRSM-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSM-//.
      *    ����桼�����ơ��֥�
       01  ADRSU-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSU-//.
      *    ������
       01  ADRSW-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSW-//.
      *    ���ŵ��ؾ���ݴ��ܾ��� 
           COPY    "CPSK1001.INC".
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      ****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
           PERFORM 300-END-SEC
           STOP    RUN
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
           DISPLAY "*(ORCBMIGADRS)* ����ܹԽ�������"
           MOVE    ZERO                TO  FLG-AREA
           MOVE    ZERO                TO  CNT-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  SPA-AREA
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                   INTO    WRK-PARA-HOSPNUM
                                           WRK-PARA-LIMIT-PRF
                                           WRK-PARA-FILE
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-LIMIT-PRF  TO  WRK-LIMIT-PRF
           MOVE    WRK-PARA-FILE       TO  ASS-PRF
      *
           PERFORM 100-DBOPEN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "*(ORCBMIGADRS)* DATABASE OPEN ERROR"
               MOVE    1                   TO  FLG-END
           END-IF
      *
           IF      FLG-END             =   ZERO
               OPEN    OUTPUT  PRF-FILE
               IF      STS-PRF         NOT =   ZERO
                   DISPLAY "*(ORCBMIGADRS)* PROOF FILE OPEN ERROR "
                           "STATUS [ " STS-PRF " ]"
                   MOVE    1                   TO  FLG-END
               ELSE
                   MOVE    WRK-SYY             TO  PRF-TITLE1-YY
                   MOVE    WRK-SMM             TO  PRF-TITLE1-MM
                   MOVE    WRK-SDD             TO  PRF-TITLE1-DD
                   MOVE    SPA-HOSPNUM         TO  PRF-HOSPNUM
                   PERFORM 900-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    SYS-1001-HOSPNAME
                                               TO  PRF-HOSPNAME
                       DISPLAY "*(ORCBMIGADRS)* [ "
                               FUNCTION TRIM(SYS-1001-HOSPNAME) " ]"
                       DISPLAY " "
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-ADRSO-SELECT-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                SECTION.
           INITIALIZE  PRF-DETAIL
      *    �ޥ������ơ��֥�¸�߳�ǧ
           PERFORM 900-ADRSM-READ-SEC
           IF      FLG-ADRSM           =   ZERO
      *        ɸ��ȹ�������ӡ����ξ��桼�����ơ��֥���ɲ�
               PERFORM 220-COMPARE-SEC
               IF      TBL-COMP-AREA       =   ZERO
                   MOVE    ZERO                TO  WRK-PRFCD
               ELSE
                   MOVE    ADRSO-REC           TO  ADRSW-REC
                   PERFORM 210-ADRSU-INSERT-SEC
                   IF      WRK-PRFCD           =   1
                       MOVE    2                   TO  WRK-PRFCD
                   END-IF
               END-IF
           ELSE
      *        �ޥ������ˤʤ����桼�����ơ��֥���ɲ�
               MOVE    ADRSO-REC           TO  ADRSW-REC
               PERFORM 210-ADRSU-INSERT-SEC
           END-IF
           PERFORM 290-PRF-SEC
           PERFORM 900-ADRSO-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ����桼�����ơ��֥��ɲ�
      *****************************************************************
       210-ADRSU-INSERT-SEC        SECTION.
           PERFORM 900-ADRSU-READ-SEC
           IF      FLG-ADRSU           =   ZERO
      *        �桼�����ơ��֥��ͭ��
               MOVE    ZERO                TO  WRK-PRFCD
           ELSE
      *        �������ϸ��ΤޤޤȤ���
      *        MOVE    LNK-MNTYMD          TO  CHGW-CREYMD
      *                                        CHGW-UPYMD
      *        MOVE    LNK-MNTHMS          TO  CHGW-UPHMS
               MOVE    ADRSW-REC           TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_adrs_user"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    1                   TO  WRK-PRFCD
               ELSE
                   MOVE    91                  TO  WRK-PRFCD
               END-IF
           END-IF
           .
       210-ADRSU-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       220-COMPARE-SEC             SECTION.
           MOVE    ZERO                TO  TBL-COMP-AREA
      *
      *�����������Υ�����
      *͹���ֹ�
      *����
      *��ƻ�ܸ�̾����
           IF      ADRSO-PREFKANA  NOT =   ADRSM-PREFKANA
               MOVE    1                   TO  TBL-COMP (1)
           END-IF
      *��Į¼̾����
           IF      ADRSO-CITYKANA  NOT =   ADRSM-CITYKANA
               MOVE    1                   TO  TBL-COMP (2)
           END-IF
      *Į��̾����
           IF      ADRSO-TOWNKANA  NOT =   ADRSM-TOWNKANA
               MOVE    1                   TO  TBL-COMP (3)
           END-IF
      *��ƻ�ܸ�̾
           IF      ADRSO-PREFNAME  NOT =   ADRSM-PREFNAME
               MOVE    1                   TO  TBL-COMP (4)
           END-IF
      *��Į¼̾
           IF      ADRSO-CITYNAME  NOT =   ADRSM-CITYNAME
               MOVE    1                   TO  TBL-COMP (5)
           END-IF
      *Į��̾
           IF      ADRSO-TOWNNAME  NOT =   ADRSM-TOWNNAME
               MOVE    1                   TO  TBL-COMP (6)
           END-IF
      *�Խ��彻�ꥫ��
           IF      ADRSO-EDITADRS-KANA
                                   NOT =   ADRSM-EDITADRS-KANA
               MOVE    1                   TO  TBL-COMP (7)
           END-IF
      *�Խ��彻��
           IF      ADRSO-EDITADRS-NAME
                                   NOT =   ADRSM-EDITADRS-NAME
               MOVE    1                   TO  TBL-COMP (8)
           END-IF
      *Į��ʬ��ե饰
           IF      ADRSO-TOWNDIVFLG
                                   NOT =   ADRSM-TOWNDIVFLG
               MOVE    1                   TO  TBL-COMP (9)
           END-IF
      *ʣ��Į��ե饰
           IF      ADRSO-MANYTOWNFLG
                                   NOT =   ADRSM-MANYTOWNFLG
               MOVE    1                   TO  TBL-COMP (10)
           END-IF
      *��­���̤�̾��
           IF      ADRSO-TOWNNAME2 NOT =   ADRSM-TOWNNAME2
               MOVE    1                   TO  TBL-COMP (11)
           END-IF
           .
       220-COMPARE-EXT.
           EXIT.
      *****************************************************************
      *    �ץ롼�ս��Ͻ���
      *****************************************************************
       290-PRF-SEC                 SECTION.
           EVALUATE    WRK-PRFCD
               WHEN    1
                   MOVE    "�ɲ�"              TO  PRF-COMMAND
                   MOVE    CONST-COMM2         TO  PRF-COMMENT
                   MOVE    ADRSW-LPUBCD        TO  PRF-LPUBCD
                   MOVE    ADRSW-POST (1:3)    TO  PRF-POST1
                   MOVE    "-"                 TO  PRF-POSTH
                   MOVE    ADRSW-POST (4:4)    TO  PRF-POST2
                   MOVE    ADRSW-EDANUM        TO  PRF-EDANUM
                   MOVE    ADRSW-EDITADRS-NAME TO  PRF-ADRS-NAMEU
               WHEN    2
                   MOVE    "�ѹ�"              TO  PRF-COMMAND
                   MOVE    CONST-COMM1         TO  PRF-COMMENT
                   MOVE    ADRSW-LPUBCD        TO  PRF-LPUBCD
                   MOVE    ADRSW-POST (1:3)    TO  PRF-POST1
                   MOVE    "-"                 TO  PRF-POSTH
                   MOVE    ADRSW-POST (4:4)    TO  PRF-POST2
                   MOVE    ADRSW-EDANUM        TO  PRF-EDANUM
                   MOVE    ADRSW-EDITADRS-NAME TO  PRF-ADRS-NAMEU
                   MOVE    ADRSM-EDITADRS-NAME TO  PRF-ADRS-NAMEM
               WHEN    91
                   MOVE    "�ɲå��顼"        TO  PRF-COMMAND
                   MOVE    ADRSW-LPUBCD        TO  PRF-LPUBCD
                   MOVE    ADRSW-POST (1:3)    TO  PRF-POST1
                   MOVE    "-"                 TO  PRF-POSTH
                   MOVE    ADRSW-POST (4:4)    TO  PRF-POST2
                   MOVE    ADRSW-EDANUM        TO  PRF-EDANUM
                   MOVE    ADRSW-EDITADRS-NAME TO  PRF-ADRS-NAMEU
           END-EVALUATE
           IF      WRK-PRFCD       NOT =   ZERO
               PERFORM 2901-PRF-SEC
           END-IF
           .
       290-PRF-EXT.
           EXIT.
      *****************************************************************
      *    �ץ롼�ս��Ͻ���
      *****************************************************************
       2901-PRF-SEC                SECTION.
           IF      CNT-PRF             =   ZERO
               MOVE    PRF-HEADER1         TO  PRF-REC
               WRITE   PRF-REC
               MOVE    PRF-HEADER2         TO  PRF-REC
               WRITE   PRF-REC
               MOVE    SPACE               TO  PRF-REC
               WRITE   PRF-REC
               MOVE    PRF-HEADER3         TO  PRF-REC
               WRITE   PRF-REC
           END-IF
           MOVE    SPACE               TO  PRF-REC
           STRING  PRF-COMMAND         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-LPUBCD          DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-POST            DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-EDANUM          DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-ADRS-NAMEU      DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-ADRS-NAMEM      DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-COMMENT         DELIMITED BY SPACE
               INTO    PRF-REC
           END-STRING
           WRITE   PRF-REC
           ADD     1                   TO  CNT-PRF
           .
       2901-PRF-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
           MOVE    "tbl_adrs_org"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           CLOSE   PRF-FILE
           DISPLAY " "
           MOVE    CNT-PRF             TO  WRK-MSG-CNT
           DISPLAY "*(ORCBMIGADRS)* �ץ롼�� [ " WRK-MSG-CNT " ]"
           DISPLAY " "
           DISPLAY "*(ORCBMIGADRS)* ����ܹԽ�����λ"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ����ܹ����ơ��֥��ɹ�
      *****************************************************************
       900-ADRSO-SELECT-SEC        SECTION.
           INITIALIZE  ADRSO-REC
           MOVE    SPA-HOSPNUM         TO  ADRSO-HOSPNUM
           MOVE    ADRSO-REC           TO  MCPDATA-REC
           MOVE    "tbl_adrs_org"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ADRSO-FETCH-SEC
           END-IF
           .
       900-ADRSO-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ����ܹ����ơ��֥��ɹ�
      *****************************************************************
       900-ADRSO-FETCH-SEC         SECTION.
           MOVE    "tbl_adrs_org"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ADRSO-REC
               ADD     1                   TO  CNT-ADRSO
               MOVE    ZERO                TO  FLG-END
               IF      CNT-ADRSO (3:4)     =   "0000"
                   MOVE    CNT-ADRSO           TO  WRK-MSG-CNT
                   DISPLAY "*(ORCBMIGADRS)* org    CNT [ "
                           WRK-MSG-CNT " ]"
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
               MOVE    CNT-ADRSO           TO  WRK-MSG-CNT
               DISPLAY "*(ORCBMIGADRS)* org    CNT [ "
                       WRK-MSG-CNT " ]"
           END-IF
           .
       900-ADRSO-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    ����ޥ����ơ��֥��ɹ�
      *****************************************************************
       900-ADRSM-READ-SEC          SECTION.
           INITIALIZE  ADRSM-REC
           MOVE    ADRSO-KEY           TO  ADRSM-KEY
           MOVE    SPA-HOSPNUM         TO  ADRSM-HOSPNUM
           MOVE    ADRSM-REC           TO  MCPDATA-REC
           MOVE    "tbl_adrs_master"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-ADRSM
               MOVE    "tbl_adrs_master"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRSM-REC
                   MOVE    ZERO                TO  FLG-ADRSM
               ELSE
                   MOVE    1                   TO  FLG-ADRSM
               END-IF
           END-IF
           MOVE    "tbl_adrs_master"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ADRSM-READ-EXT.
           EXIT.
      *****************************************************************
      *    ����桼�����ơ��֥��ɹ�
      *****************************************************************
       900-ADRSU-READ-SEC          SECTION.
           INITIALIZE  ADRSU-REC
           MOVE    ADRSW-KEY           TO  ADRSU-KEY
           MOVE    SPA-HOSPNUM         TO  ADRSU-HOSPNUM
           MOVE    ADRSU-REC           TO  MCPDATA-REC
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-ADRSU
               MOVE    "tbl_adrs_user"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRSU-REC
                   MOVE    ZERO                TO  FLG-ADRSU
               ELSE
                   MOVE    1                   TO  FLG-ADRSU
               END-IF
           END-IF
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ADRSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ɹ�
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
           INITIALIZE  SYS-1001-REC
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
           MOVE    WRK-SYMD            TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
                   INITIALIZE  SYS-1001-REC
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               PERFORM 900-DBABORT-SEC
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �쥳������н���
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �������륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥����ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥빹�����顼������λ����
      *****************************************************************
       900-DBABORT-SEC             SECTION.
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "*(ORCBMIGADRS)* "      DELIMITED BY SIZE
                   MCP-FUNC (3:)           DELIMITED BY SPACE
                   " ERROR: table [ "      DELIMITED BY SIZE
                   MCP-TABLE               DELIMITED BY SPACE
                   " ] pathname [ "        DELIMITED BY SIZE
                   MCP-PATHNAME            DELIMITED BY SPACE
                   " ]"                    DELIMITED BY SIZE
                   LOW-VALUE               DELIMITED BY SIZE
               INTO  WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       900-DBABORT-EXT.
           EXIT.