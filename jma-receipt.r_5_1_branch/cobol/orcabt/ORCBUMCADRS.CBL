       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBUMCADRS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ޥ����ꥹ�Ƚ���
      *  ����ݡ��ͥ��̾  : ����ޥ��������å�
      *                    : ����ޥ����Υ桼������ɸ���󶡤�
      *                    : ���Ƥ�����å����ꥹ�Ȥ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  19/02/25    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  BUMCPERR-FILE   ASSIGN  BUMCP-ERRFNAME
                                   FILE    STATUS  IS  STS-BUMCPERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    ���顼�ե�����
       FD  BUMCPERR-FILE.
       01  BUMCPERR-REC                PIC X(200).
      *
       WORKING-STORAGE             SECTION.
       01  STS-AREA.
           03  STS-BUMCPERR            PIC X(02).
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ADRSM               PIC 9(01).
           03  FLG-ADRSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-DETAIL              PIC 9(01).
       01  CNT-AREA.
           03  CNT-ADRSU               PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-LINE                PIC 9(03).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-CSV                 PIC 9(06).
       01  IDX-AREA.
           03  IDX                     PIC 9(02).
           03  IDW                     PIC 9(05).
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-BUMCPERR            PIC X(200).
           03  WRK-MSG                 PIC X(256).
           03  WRK-MSG-CNT             PIC ZZZ,ZZ9.
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-CSV-ITEM            PIC X(22).
           03  WRK-CSV-VAL-USR         PIC X(200).
           03  WRK-CSV-VAL-STD         PIC X(200).
      *    �ãӣ֥ǡ����Խ���
       01  WRK-CSV-AREA.
           03  WRK-IN-DATA             PIC X(2000).
           03  WRK-OUT-REC             PIC X(2000).
           03  WRK-CSV-REC             PIC X(2000).
           03  WRK-REC-LENGTH          PIC 9(05).
           03  WRK-IN-MAX              PIC 9(04).
           03  WRK-END                 PIC 9(01).
           03  WRK-CSVDIR              PIC X(09)   VALUE
                                       "/var/tmp/".
           03  WRK-CSVFNAME            PIC X(19)   VALUE
                                       "adrs_user_check.csv".
      *    ��ӥơ��֥�
       01  TBL-COMP-AREA.
           03  TBL-COMP                PIC 9(01) OCCURS    11.
      *    �ץ롼���Խ��ΰ�
       01  PRF-AREA.
           03  PRF-HEADER1.
               05  FILLER              PIC X(32) VALUE
                   "����桼�����ޥ��������å��ꥹ��".
               05  FILLER              PIC X(15) VALUE
                   "��ORCBUMCADRS��".
               05  PRF-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE "/".
               05  PRF-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE "/".
               05  PRF-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(10) VALUE SPACE.
               05  PRF-PAGE            PIC ZZ,ZZ9.
               05  FILLER              PIC X(03) VALUE " ��".
           03  PRF-HEADER2.
               05  FILLER              PIC X(01) VALUE "[".
               05  PRF-HOSPNUM         PIC 9(02).
               05  FILLER              PIC X(01) VALUE "]".
               05  PRF-HOSPNAME        PIC X(120) VALUE SPACE.
           03  PRF-HEADER3.
               05  FILLER              PIC X(02) VALUE "��".
               05  FILLER              PIC X(01) VALUE SPACE.
               05  FILLER              PIC X(08) VALUE "͹���ֹ�".
               05  FILLER              PIC X(01) VALUE SPACE.
               05  FILLER              PIC X(04) VALUE "����".
               05  FILLER              PIC X(56) VALUE SPACE.
               05  FILLER              PIC X(26) VALUE
                                       "�ơ������桼���������ޥ���".
           03  PRF-HEADER4.
               05  FILLER              PIC X(16) VALUE SPACE.
               05  FILLER              PIC X(06) VALUE "����".
               05  FILLER              PIC X(46) VALUE
                   "�ʾ��ʡ��桼���������͡����ʡ�ɸ���������͡�".
               05  FILLER              PIC X(08) VALUE SPACE.
               05  FILLER              PIC X(28) VALUE
                                       "����ɸ���󶡥ޥ�����Ʊ������".
           03  PRF-DETAIL-1.
               05  PRF-FLG             PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE SPACE.
               05  PRF-POST            PIC X(08).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  PRF-EDANUM          PIC 9(02).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  PRF-ADRS            PIC X(200).
           03  PRF-DETAIL-3.
               05  FILLER              PIC X(19) VALUE SPACE.
               05  PRF-ITEM            PIC X(22).
               05  PRF-LITE1           PIC X(02) VALUE "��".
               05  PRF-VAL-USR         PIC X(200).
           03  PRF-DETAIL-4.
               05  FILLER              PIC X(43) VALUE SPACE.
               05  PRF-VAL-STD         PIC X(200).
       01  CSV-HEAD-01.
           03  FILLER                  PIC X(02) VALUE "��".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(08) VALUE "͹���ֹ�".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(04) VALUE "����".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(04) VALUE "����".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(14) VALUE "����̾�����ơ�".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(12) VALUE "�桼��������".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(08) VALUE "ɸ������".
       01  CONST-AREA.
           03  CONST-COMM3             PIC X(08) VALUE "�ʶ����".
           03  CONST-KUGIRI            PIC X(01) VALUE ",".
           03  CONST-KAIGYO            PIC X(01) VALUE X"0D".
      *
           COPY    "HCMSL55.INC".
       01  MAX-LINE                    PIC 9(03) VALUE 55.
      *
           COPY    "CPRECEERR.INC".
      *****************************************************************
      *    ���֥ץ������
      *****************************************************************
      *    �ޥ������ռ���
           COPY    "CPORCSMCNDATE.INC".
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *    ���ףãӣ����極��
           COPY    "CPORCSTOUKEICSV.INC".
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *    ����ޥ������ơ��֥�
       01  ADRSM-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSM-//.
      *    ����桼�����ơ��֥�
       01  ADRSU-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSU-//.
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    ���ŵ��ؾ���ݴ��ܾ���
           COPY    "CPSK1001.INC".
      *    �����ģ¹�������
           COPY    "CPORCSPRT.INC".
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
       01  BUMCP-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBUMCPPARA.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION
                                       USING
                                       BUMCP-AREA
                                       SPA-AREA.
      ****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC    UNTIL FLG-END   =   1
           PERFORM 300-END-SEC
           PERFORM 990-EXIT-PROGRAM-SEC
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
           INITIALIZE  FLG-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-AREA
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "����桼�����ޥ��������å��ꥹ��"
                                       TO  JOB-YOBI
           PERFORM 800-ORCSJOB-SEC
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    WRK-SYY             TO  PRF-TITLE1-YY
           MOVE    WRK-SMM             TO  PRF-TITLE1-MM
           MOVE    WRK-SDD             TO  PRF-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  PRF-HOSPNUM
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-1001-HOSPNAME   TO  PRF-HOSPNAME
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-ADRSU-SELECT-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                SECTION.
           PERFORM 900-ADRSM-READ-SEC
           IF      FLG-ADRSM       =   ZERO
               PERFORM 220-COMPARE-SEC
               IF      TBL-COMP-AREA       =   ZERO
                   MOVE    3                   TO  FLG-SYORI
               ELSE
                   MOVE    2                   TO  FLG-SYORI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYORI
           END-IF
      *    FLG-SYORI
      *      1:�桼�������ꡢɸ��ʤ�
      *      2:�桼�������ꡢɸ�ढ�ꡢ�԰���
      *      3:�桼�������ꡢɸ�ढ�ꡢ����
           ADD     1                   TO  CNT-OUT
           PERFORM 290-PRF-SEC
           PERFORM 900-ADRSU-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       220-COMPARE-SEC             SECTION.
           MOVE    ZERO                TO  TBL-COMP-AREA
      *    ��ƻ�ܸ�̾����
           IF      ADRSU-PREFKANA  NOT =   ADRSM-PREFKANA
               MOVE    1                   TO  TBL-COMP (1)
           END-IF
      *    ��Į¼̾����
           IF      ADRSU-CITYKANA  NOT =   ADRSM-CITYKANA
               MOVE    1                   TO  TBL-COMP (2)
           END-IF
      *    Į��̾����
           IF      ADRSU-TOWNKANA  NOT =   ADRSM-TOWNKANA
               MOVE    1                   TO  TBL-COMP (3)
           END-IF
      *    ��ƻ�ܸ�̾
           IF      ADRSU-PREFNAME  NOT =   ADRSM-PREFNAME
               MOVE    1                   TO  TBL-COMP (4)
           END-IF
      *    ��Į¼̾
           IF      ADRSU-CITYNAME  NOT =   ADRSM-CITYNAME
               MOVE    1                   TO  TBL-COMP (5)
           END-IF
      *    Į��̾
           IF      ADRSU-TOWNNAME  NOT =   ADRSM-TOWNNAME
               MOVE    1                   TO  TBL-COMP (6)
           END-IF
      *    �Խ��彻�ꥫ��
           IF      ADRSU-EDITADRS-KANA
                                   NOT =   ADRSM-EDITADRS-KANA
               MOVE    1                   TO  TBL-COMP (7)
           END-IF
      *    �Խ��彻��
           IF       ADRSU-EDITADRS-NAME
                                   NOT =   ADRSM-EDITADRS-NAME
               MOVE    1                   TO  TBL-COMP (8)
           END-IF
      *    Į��ʬ��ե饰
           IF      ADRSU-TOWNDIVFLG
                                   NOT =   ADRSM-TOWNDIVFLG
               MOVE    1                   TO  TBL-COMP (9)
           END-IF
      *    ʣ��Į��ե饰
           IF      ADRSU-MANYTOWNFLG
                                   NOT =   ADRSM-MANYTOWNFLG
               MOVE    1                   TO  TBL-COMP(10)
           END-IF
      *    ��­���̤�̾��
           IF      ADRSU-TOWNNAME2 NOT =   ADRSM-TOWNNAME2
               MOVE    1                   TO  TBL-COMP(11)
           END-IF
           .
       220-COMPARE-EXT.
           EXIT.
      *****************************************************************
      *    �ץ롼�ս��Ͻ���
      *****************************************************************
       290-PRF-SEC                 SECTION.
           IF      CNT-LINE            =   ZERO
               PERFORM 2903-PRF-HEADER-SEC
           END-IF
           MOVE    1                   TO  FLG-DETAIL
           MOVE    ADRSU-POST(1:3)     TO  PRF-POST(1:3)
           MOVE    "-"                 TO  PRF-POST(4:1)
           MOVE    ADRSU-POST(4:4)     TO  PRF-POST(5:4)
           MOVE    ADRSU-EDANUM        TO  PRF-EDANUM
           STRING  ADRSU-PREFNAME          DELIMITED BY SPACE
                   ADRSU-CITYNAME          DELIMITED BY SPACE
                   ADRSU-TOWNNAME          DELIMITED BY SPACE
                                     INTO  PRF-ADRS
           END-STRING
           EVALUATE    FLG-SYORI
               WHEN    1
                   MOVE    "��"            TO  PRF-FLG
               WHEN    2
                   MOVE    SPACE           TO  PRF-FLG
               WHEN    3
                   MOVE    "��"            TO  PRF-FLG
           END-EVALUATE
           PERFORM 2901-PRF-SEC
           IF      FLG-SYORI           =   2
               PERFORM VARYING IDX
                       FROM    1   BY  1
                       UNTIL   IDX >   11
                   IF      TBL-COMP (IDX)       =   1
                       PERFORM 2902-PRF-SET-VALUE-SEC
                       MOVE    3                   TO  FLG-DETAIL
                       PERFORM 2901-PRF-SEC
                       MOVE    4                   TO  FLG-DETAIL
                       PERFORM 2901-PRF-SEC
                   END-IF
               END-PERFORM
           END-IF
           IF      BUMCP-CSVKBN     =   "1"
               PERFORM 600-CSV-SYORI-SEC
           END-IF
           .
       290-PRF-EXT.
           EXIT.
      *****************************************************************
      *    �ץ롼�ս��Ͻ���
      *****************************************************************
       2901-PRF-SEC                SECTION.
           IF      CNT-LINE            >=  MAX-LINE
               PERFORM 2909-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 2903-PRF-HEADER-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           EVALUATE    FLG-DETAIL
               WHEN    1
                   MOVE    PRF-DETAIL-1
                                       TO  HCMSL55-MOJI (CNT-LINE)
               WHEN    3
                   IF      PRF-VAL-USR         =   SPACE
                       MOVE    CONST-COMM3         TO  PRF-VAL-USR
                   END-IF
                   MOVE    PRF-DETAIL-3
                                       TO  HCMSL55-MOJI (CNT-LINE)
               WHEN    4
                   IF      PRF-VAL-STD         =   SPACE
                       MOVE    CONST-COMM3         TO  PRF-VAL-STD
                   END-IF
                   MOVE    PRF-DETAIL-4
                                       TO  HCMSL55-MOJI (CNT-LINE)
           END-EVALUATE
           MOVE    ZERO                TO  FLG-DETAIL
           .
       2901-PRF-EXT.
           EXIT.
      *****************************************************************
      *    �ѹ��Ľ�ɽ������
      *****************************************************************
       2902-PRF-SET-VALUE-SEC      SECTION.
           EVALUATE    IDX
               WHEN    1
                   MOVE    "��ƻ�ܸ�̾����"    TO  PRF-ITEM
                   MOVE    ADRSU-PREFKANA      TO  PRF-VAL-USR
                   MOVE    ADRSM-PREFKANA      TO  PRF-VAL-STD
               WHEN    2
                   MOVE    "��Į¼̾����"      TO  PRF-ITEM
                   MOVE    ADRSU-CITYKANA      TO  PRF-VAL-USR
                   MOVE    ADRSM-CITYKANA      TO  PRF-VAL-STD
               WHEN    3
                   MOVE    "Į��̾����"        TO  PRF-ITEM
                   MOVE    ADRSU-TOWNKANA      TO  PRF-VAL-USR
                   MOVE    ADRSM-TOWNKANA      TO  PRF-VAL-STD
               WHEN    4
                   MOVE    "��ƻ�ܸ�̾"        TO  PRF-ITEM
                   MOVE    ADRSU-PREFNAME      TO  PRF-VAL-USR
                   MOVE    ADRSM-PREFNAME      TO  PRF-VAL-STD
               WHEN    5
                   MOVE    "��Į¼̾"          TO  PRF-ITEM
                   MOVE    ADRSU-CITYNAME      TO  PRF-VAL-USR
                   MOVE    ADRSM-CITYNAME      TO  PRF-VAL-STD
               WHEN    6
                   MOVE    "Į��̾"            TO  PRF-ITEM
                   MOVE    ADRSU-TOWNNAME      TO  PRF-VAL-USR
                   MOVE    ADRSM-TOWNNAME      TO  PRF-VAL-STD
               WHEN    7
                   MOVE    "�Խ��彻�ꥫ��"    TO  PRF-ITEM
                   MOVE    ADRSU-EDITADRS-KANA TO  PRF-VAL-USR
                   MOVE    ADRSM-EDITADRS-KANA TO  PRF-VAL-STD
              WHEN    8
                   MOVE    "�Խ��彻��"        TO  PRF-ITEM
                   MOVE    ADRSU-EDITADRS-NAME TO  PRF-VAL-USR
                   MOVE    ADRSM-EDITADRS-NAME TO  PRF-VAL-STD
              WHEN    9
                   MOVE    "Į��ʬ��ե饰"    TO  PRF-ITEM
                   MOVE    ADRSU-TOWNDIVFLG    TO  PRF-VAL-USR
                   MOVE    ADRSM-TOWNDIVFLG    TO  PRF-VAL-STD
              WHEN    10
                   MOVE    "ʣ��Į��ե饰"    TO  PRF-ITEM
                   MOVE    ADRSU-MANYTOWNFLG   TO  PRF-VAL-USR
                   MOVE    ADRSM-MANYTOWNFLG   TO  PRF-VAL-STD
              WHEN    11
                   MOVE    "��­���̤�̾��"    TO  PRF-ITEM
                   MOVE    ADRSU-TOWNNAME2     TO  PRF-VAL-USR
                   MOVE    ADRSM-TOWNNAME2     TO  PRF-VAL-STD
           END-EVALUATE
      *
           MOVE    PRF-ITEM            TO  WRK-CSV-ITEM
           MOVE    PRF-VAL-USR         TO  WRK-CSV-VAL-USR
           MOVE    PRF-VAL-STD         TO  WRK-CSV-VAL-STD
           .
       2902-PRF-SET-VALUE-EXT.
           EXIT.
      *****************************************************************
      *    ���Ф��Խ�����
      *****************************************************************
       2903-PRF-HEADER-SEC         SECTION.
           MOVE    SPACE               TO  HCMSL55
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  PRF-PAGE
           MOVE    PRF-HEADER1         TO  HCMSL55-MOJI (1)
           MOVE    PRF-HEADER2         TO  HCMSL55-MOJI (2)
      ****                                 HCMSL55-MOJI (3)
           MOVE    PRF-HEADER3         TO  HCMSL55-MOJI (4)
           MOVE    PRF-HEADER4         TO  HCMSL55-MOJI (5)
           MOVE    ALL "-"             TO  HCMSL55-MOJI (6)
           MOVE    6                   TO  CNT-LINE
           .
       2903-PRF-HEADER-EXT.
           EXIT.
      *****************************************************************
      *    �оݤʤ���������
      *****************************************************************
       2904-PRF-NOTHING-SEC        SECTION.
           PERFORM 2903-PRF-HEADER-SEC
           ADD     3                   TO  CNT-LINE
           MOVE    "�������ᡡ����桼�����ޥ�����Ͽ�ʤ�������"
                                       TO  HCMSL55-MOJI (CNT-LINE)
           PERFORM 2909-PRINT-SEC
           .
       2904-PRF-NOTHING-EXT.
           EXIT.
      *****************************************************************
      *    ��������
      *****************************************************************
       2909-PRINT-SEC              SECTION.
           INITIALIZE  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"       TO  SPRT-PRTID
           MOVE    "����桼�����ޥ��������å��ꥹ��"
                                       TO  SPRT-TITLE
           MOVE    HCMSL55             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"       USING   ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "�����ģ¤������Ǥ��ޤ���Ǥ���"
                                       TO  WRK-BUMCPERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2909-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
           IF      CNT-LINE            >   6
               PERFORM 2909-PRINT-SEC
           END-IF
      *
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      CNT-ADRSU           =   ZERO
               PERFORM 2904-PRF-NOTHING-SEC
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                       TO  WRK-BUMCPERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           COMPUTE BUMCP-PAGE
               =   BUMCP-PAGE    +   CNT-PAGE
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
           OPEN    INPUT   BUMCPERR-FILE
           IF      STS-BUMCPERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT  BUMCPERR-FILE
               MOVE    WRK-BUMCPERR        TO  BUMCPERR-REC
               WRITE   BUMCPERR-REC
               CLOSE   BUMCPERR-FILE
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  BUMCP-RC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    �ãӣ֥ǡ�������
      *****************************************************************
       600-CSV-SYORI-SEC           SECTION.
           IF      CNT-CSV             =   ZERO
               PERFORM 6001-CSV-HEAD-SEC
           END-IF
           INITIALIZE  WRK-CSV-AREA
           IF      FLG-SYORI           =   2
      *        �ե饰
               MOVE    PRF-FLG             TO  WRK-IN-DATA
               MOVE    2                   TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        ͹���ֹ�
               MOVE    PRF-POST            TO  WRK-IN-DATA
               MOVE    8                   TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        ����
               MOVE    PRF-EDANUM          TO  WRK-IN-DATA
               MOVE    2                   TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC     
      *        ����
               MOVE    PRF-ADRS            TO  WRK-IN-DATA
               MOVE    200                 TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        ����̾
               MOVE    WRK-CSV-ITEM        TO  WRK-IN-DATA
               MOVE    200                 TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        �桼��������
               MOVE    WRK-CSV-VAL-USR     TO  WRK-IN-DATA
               MOVE    200                 TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        ɸ������
               MOVE    WRK-CSV-VAL-STD     TO  WRK-IN-DATA
               MOVE    200                 TO  WRK-IN-MAX
               MOVE    1                   TO  WRK-END
           ELSE
      *        �ե饰
               MOVE    PRF-FLG             TO  WRK-IN-DATA
               MOVE    2                   TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        ͹���ֹ�
               MOVE    PRF-POST            TO  WRK-IN-DATA
               MOVE    8                   TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        ����
               MOVE    PRF-EDANUM          TO  WRK-IN-DATA
               MOVE    2                   TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC     
      *        ����
               MOVE    PRF-ADRS            TO  WRK-IN-DATA
               MOVE    200                 TO  WRK-IN-MAX
               MOVE    1                   TO  WRK-END
               PERFORM 6002-MOJI-HENSYU-SEC
           END-IF
           MOVE    SPACE               TO  WRK-OUT-REC
           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                   CONST-KAIGYO            DELIMITED  BY  SIZE
                   INTO        WRK-OUT-REC
           END-STRING
           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
           PERFORM 6009-TOUKEICSV-SEC
           .
       600-CSV-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ���ףãӣֽ��Ͻ���
      *****************************************************************
       6001-CSV-HEAD-SEC           SECTION.
           MOVE    CSV-HEAD-01         TO  WRK-OUT-REC
           MOVE    SPACE               TO  WRK-CSV-REC
           STRING  WRK-OUT-REC         DELIMITED  BY  SPACE
                   CONST-KAIGYO        DELIMITED  BY  SIZE
                   INTO    WRK-CSV-REC
           END-STRING
           PERFORM 6009-TOUKEICSV-SEC
           .
       6001-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    ���ϥ쥳���ɽ�����ʸ����
      *****************************************************************
       6002-MOJI-HENSYU-SEC        SECTION.
           IF      WRK-IN-DATA         =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW     FROM    WRK-IN-MAX  BY  -1
                       UNTIL   IDW     <       1
                   IF      WRK-IN-DATA (IDW:1) NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-IN-DATA (1:IDW)
                                                   TO  WRK-CSV-REC
                       ELSE
                           MOVE    SPACE           TO  WRK-OUT-REC
                           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-IN-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-OUT-REC
                           END-STRING
                           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               MOVE    SPACE               TO  WRK-OUT-REC
               STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                       CONST-KUGIRI        DELIMITED BY  SIZE
                       INTO    WRK-OUT-REC
               END-STRING
               MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       6002-MOJI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ���ףãӣֽ��Ͻ���
      *****************************************************************
       6009-TOUKEICSV-SEC          SECTION.
           INITIALIZE  ORCSTOUKEICSVAREA
           MOVE    "INS"               TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  STOUKEICSV-SHELLID
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    WRK-CSVDIR          TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-CSVFNAME        TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSV-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "����桼�����ޥ��������å��ꥹ�ȣãӣֺ���"
                                       TO  STOUKEICSV-TITLE
           CALL    "ORCSTOUKEICSV"         USING
                                           ORCSTOUKEICSVAREA
                                           SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               ADD     1                   TO  CNT-CSV
           ELSE
               MOVE    "���ףãӣ֣ģ¤˹����Ǥ��ޤ���Ǥ�����"
                                       TO  WRK-BUMCPERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       6009-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    ����ִ�������
      *****************************************************************
       800-ORCSJOB-SEC             SECTION.
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BUMCP-JOBID         TO  JOB-JOBID
           MOVE    BUMCP-SHELLID       TO  JOB-SHELLID
           MOVE    "ORCBUMCP"          TO  JOB-PGID
           MOVE    BUMCP-SHELLMSG      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
           .
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ����桼�����ơ��֥��ɹ�
      *****************************************************************
       900-ADRSU-SELECT-SEC        SECTION.
           INITIALIZE  ADRSU-REC
           MOVE    SPA-HOSPNUM         TO  ADRSU-HOSPNUM
           MOVE    ADRSU-REC           TO  MCPDATA-REC
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ADRSU-FETCH-SEC
           END-IF
           .
       900-ADRSU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ����桼�����ơ��֥��ɹ�
      *****************************************************************
       900-ADRSU-FETCH-SEC         SECTION.
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ADRSU-REC
               ADD     1                   TO  CNT-ADRSU
               MOVE    ZERO                TO  FLG-END
               IF      CNT-ADRSU (4:3)     =   "000"
                   MOVE    CNT-ADRSU           TO  WRK-MSG-CNT
                   DISPLAY "*(ORCBUMCADRS)* org    CNT [ "
                           WRK-MSG-CNT " ]"
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-ADRSU-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    ����ޥ����ơ��֥��ɹ�
      *****************************************************************
       900-ADRSM-READ-SEC          SECTION.
           INITIALIZE  ADRSM-REC
           MOVE    ADRSU-HOSPNUM       TO  ADRSM-HOSPNUM
           MOVE    ADRSU-LPUBCD        TO  ADRSM-LPUBCD
           MOVE    ADRSU-POST          TO  ADRSM-POST
           MOVE    ADRSU-EDANUM        TO  ADRSM-EDANUM
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
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �쥳������н���
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �������륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����ཪλ����
      *****************************************************************
       990-EXIT-PROGRAM-SEC        SECTION.
           DISPLAY "ORCBUMCADRS RC [" BUMCP-RC "]"
           EXIT PROGRAM
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.