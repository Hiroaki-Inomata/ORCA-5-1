       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBMIGCHK.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���åȥ��å׽���
      *  ����ݡ��ͥ��̾  : �����å��ޥ�������
      *                    : �����å��ޥ���ʬΥ��ȼ���桼����������
      *                    : �������ƤˤĤ��Ƥϥ桼�����ѤΥơ��֥�Ȥ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  17/10/31    NACL-��ƣ     ��������
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
           03  FLG-CHKM                PIC 9(01).
           03  FLG-CHKU                PIC 9(01).
           03  FLG-CHKO                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-CHKO                PIC 9(06).
           03  CNT-CHKM                PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-LIMIT-PRF  PIC 9(05).
               05  WRK-PARA-FILE       PIC X(256).
           03  WRK-SRYCD               PIC X(09).
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
           03  TBL-COMP                PIC 9(01) OCCURS    4.
      *    �ץ롼���Խ��ΰ�
       01  PRF-AREA.
           03  PRF-HEADER1.
               05  FILLER              PIC X(38) VALUE
                   "�����å��ޥ����ܹԽ�����ORCBMIGCHK��".
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
               05  FILLER              PIC X(12) VALUE "�����å���ʬ".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(10) VALUE "���ť�����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(10) VALUE "����ǯ����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(10) VALUE "��λǯ����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(10) VALUE "�����ɶ�ʬ".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(10) VALUE "Ŭ�ѥ�����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(20) VALUE
                               "�桼�����ꥨ�顼����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(18) VALUE
                               "ɸ���󶡥��顼����".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "����".
           03  PRF-DETAIL.
               05  PRF-COMMAND         PIC X(20).
               05  PRF-CHKKBN          PIC X(01).
               05  PRF-SRYCD           PIC X(09).
               05  PRF-YUKOSTYMD       PIC X(08).
               05  PRF-YUKOEDYMD       PIC X(08).
               05  PRF-CDKBN           PIC X(01).
               05  PRF-CD              PIC X(09).
               05  PRF-CHKERRU         PIC 9(01).
               05  PRF-CHKERRM         PIC 9(01).
               05  PRF-COMMENT         PIC X(200).
       01  CONST-AREA.
           03  CONST-COMM1             PIC X(44) VALUE
           "�桼�����ꥨ�顼���������Ƥ��ѹ�����ޤ�����".
           03  CONST-COMM2.
               05  CONST-COMM2-1       PIC X(42) VALUE
               "�桼�����ꥨ�顼���������Ƥ��ɲä���ޤ���".
               05  CONST-COMM2-2       PIC X(28) VALUE
               "��ɸ���󶡤ˤϤ���ޤ���ˡ�".
      *****************************************************************
      *    ���֥ץ������
      *****************************************************************
      *    �ޥ������ռ���
           COPY    "CPORCSMCNDATE.INC".
      *    ����ե�����̾�Խ�
      *     COPY    "CPORCSGETTEMP.INC".
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *    �����å��ܹ����ơ��֥�
       01  CHKO-REC.
           COPY    "CPCHK.INC"         REPLACING
                                       //CHK-// BY //CHKO-//.
      *    �����å��ޥ������ơ��֥�
       01  CHKM-REC.
           COPY    "CPCHK.INC"         REPLACING
                                       //CHK-// BY //CHKM-//.
      *    �����å��桼�����ơ��֥�
       01  CHKU-REC.
           COPY    "CPCHK.INC"         REPLACING
                                       //CHK-// BY //CHKU-//.
      *    �����å����
       01  CHKW-REC.
           COPY    "CPCHK.INC"         REPLACING
                                       //CHK-// BY //CHKW-//.
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
           DISPLAY "*(ORCBMIGCHK)* �����å��ܹԽ�������"
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
               DISPLAY "*(ORCBMIGCHK)* DATABASE OPEN ERROR"
               MOVE    1                   TO  FLG-END
           END-IF
      *
           IF      FLG-END             =   ZERO
               OPEN    OUTPUT  PRF-FILE
               IF      STS-PRF         NOT =   ZERO
                   DISPLAY "*(ORCBMIGCHK)* PROOF FILE OPEN ERROR "
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
                       DISPLAY "*(ORCBMIGCHK)* [ "
                               FUNCTION TRIM(SYS-1001-HOSPNAME) " ]"
                       DISPLAY " "
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-CHKO-SELECT-SEC
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
           PERFORM 900-CHKM-READ-SEC
           IF      FLG-CHKM            =   ZERO
      *        ɸ��ȹ�������ӡ����ξ��桼�����ơ��֥���ɲ�
               PERFORM 220-COMPARE-SEC
               IF      TBL-COMP-AREA       =   ZERO
                   MOVE    ZERO                TO  WRK-PRFCD
               ELSE
                   MOVE    CHKO-REC            TO  CHKW-REC
                   MOVE    "2"                 TO  CHKW-DATAKBN
                   PERFORM 210-CHKU-INSERT-SEC
                   IF      WRK-PRFCD           =   1
                       MOVE    2                   TO  WRK-PRFCD
                   END-IF
               END-IF
           ELSE
      *        �ޥ������ˤʤ����桼�����ơ��֥���ɲ�
               MOVE    CHKO-REC            TO  CHKW-REC
               MOVE    "2"                 TO  CHKW-DATAKBN
               PERFORM 210-CHKU-INSERT-SEC
           END-IF
           PERFORM 290-PRF-SEC
           PERFORM 900-CHKO-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �����å��桼�����ơ��֥��ɲ�
      *****************************************************************
       210-CHKU-INSERT-SEC         SECTION.
           PERFORM 900-CHKU-READ-SEC
           IF      FLG-CHKU            =   ZERO
      *        �桼�����ơ��֥��ͭ��
               MOVE    ZERO                TO  WRK-PRFCD
           ELSE
      *        �������ϸ��ΤޤޤȤ���
      *        MOVE    LNK-MNTYMD          TO  CHGW-CREYMD
      *                                        CHGW-UPYMD
      *        MOVE    LNK-MNTHMS          TO  CHGW-UPHMS
               MOVE    CHKW-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_chk_user"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    1               TO  WRK-PRFCD
               ELSE
                   MOVE    91              TO  WRK-PRFCD
               END-IF
           END-IF
           .
       210-CHKU-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       220-COMPARE-SEC             SECTION.
           MOVE    ZERO                TO  TBL-COMP-AREA
      *�����å���ʬ
      *���ť�����
      *���롼���ֹ��̤���ѡ�
      *ͭ������ǯ����
      *ͭ����λǯ����
      *�����ɶ�ʬ
      *Ŭ�ѥ�����
      *
      *������ʬ
           IF      CHKO-NYUGAIKBN  NOT =   CHKM-NYUGAIKBN
               MOVE    1                   TO  TBL-COMP (1)
           END-IF
      *Ʊ�����������ʬ��1:�����������Ƚ���Ԥ���
           IF      CHKO-NYUGAIBNDKBN
                                   NOT =   CHKM-NYUGAIBNDKBN
               MOVE    1                   TO  TBL-COMP (2)
           END-IF
      *������ʬ��1:�������˴����������å���2:����������å���
           IF      CHKO-SYORIKBN   NOT =   CHKM-SYORIKBN
               MOVE    1                   TO  TBL-COMP (3)
           END-IF
      *���顼������0:���顼��9:���顼��8:�����ʤ���
           IF      CHKO-CHKERR     NOT =   CHKM-CHKERR
               MOVE    1                   TO  TBL-COMP (4)
           END-IF
      *�ǡ�����ʬ��1:�ޥ����󶡡�2:�桼����Ͽ��
      *Ϣ�ֹ�ʲ���ɽ����
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
                   MOVE    CHKW-CHKKBN         TO  PRF-CHKKBN
                   MOVE    CHKW-SRYCD          TO  PRF-SRYCD
                   MOVE    CHKW-YUKOSTYMD      TO  PRF-YUKOSTYMD
                   MOVE    CHKW-YUKOEDYMD      TO  PRF-YUKOEDYMD
                   MOVE    CHKW-CDKBN          TO  PRF-CDKBN
                   MOVE    CHKW-CD             TO  PRF-CD
                   MOVE    CHKW-CHKERR         TO  PRF-CHKERRU
               WHEN    2
                   MOVE    "�ѹ�"              TO  PRF-COMMAND
                   MOVE    CONST-COMM1         TO  PRF-COMMENT
                   MOVE    CHKW-CHKKBN         TO  PRF-CHKKBN
                   MOVE    CHKW-SRYCD          TO  PRF-SRYCD
                   MOVE    CHKW-YUKOSTYMD      TO  PRF-YUKOSTYMD
                   MOVE    CHKW-YUKOEDYMD      TO  PRF-YUKOEDYMD
                   MOVE    CHKW-CDKBN          TO  PRF-CDKBN
                   MOVE    CHKW-CD             TO  PRF-CD
                   MOVE    CHKW-CHKERR         TO  PRF-CHKERRU
                   MOVE    CHKM-CHKERR         TO  PRF-CHKERRM
               WHEN    91
                   MOVE    "�ɲå��顼"        TO  PRF-COMMAND
                   MOVE    CHKW-CHKKBN         TO  PRF-CHKKBN
                   MOVE    CHKW-SRYCD          TO  PRF-SRYCD
                   MOVE    CHKW-YUKOSTYMD      TO  PRF-YUKOSTYMD
                   MOVE    CHKW-YUKOEDYMD      TO  PRF-YUKOEDYMD
                   MOVE    CHKW-CDKBN          TO  PRF-CDKBN
                   MOVE    CHKW-CD             TO  PRF-CD
                   MOVE    CHKW-CHKERR         TO  PRF-CHKERRU
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
           INITIALIZE  PRF-REC
           STRING  PRF-COMMAND         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CHKKBN          DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-SRYCD           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-YUKOSTYMD       DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-YUKOEDYMD       DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CDKBN           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CD              DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CHKERRU         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CHKERRM         DELIMITED BY SPACE
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
           MOVE    "tbl_chk_org"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           CLOSE   PRF-FILE
           DISPLAY " "
           MOVE    CNT-PRF             TO  WRK-MSG-CNT
           DISPLAY "*(ORCBMIGCHK)* Proof Data [ " WRK-MSG-CNT " ]"
           DISPLAY " "
           DISPLAY "*(ORCBMIGCHK)* �����å��ܹԽ�����λ"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����å��ܹ����ơ��֥��ɹ�
      *****************************************************************
       900-CHKO-SELECT-SEC         SECTION.
           INITIALIZE  CHKO-REC
           MOVE    SPA-HOSPNUM         TO  CHKO-HOSPNUM
           MOVE    CHKO-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk_org"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-CHKO-FETCH-SEC
           END-IF
           .
       900-CHKO-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �����å��ܹ����ơ��֥��ɹ�
      *****************************************************************
       900-CHKO-FETCH-SEC          SECTION.
           MOVE    "tbl_chk_org"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CHKO-REC
               ADD     1                   TO  CNT-CHKO
               MOVE    ZERO                TO  FLG-END
               IF      CNT-CHKO (3:4)      =   "0000"
                   MOVE    CNT-CHKO            TO  WRK-MSG-CNT
                   DISPLAY "*(ORCBMIGCHK)* org    CNT [ "
                           WRK-MSG-CNT " ]"
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
               MOVE    CNT-CHKO            TO  WRK-MSG-CNT
               DISPLAY "*(ORCBMIGCHK)* org    CNT [ "
                       WRK-MSG-CNT " ]"
           END-IF
           .
       900-CHKO-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    �����å��ޥ����ơ��֥��ɹ�
      *****************************************************************
       900-CHKM-READ-SEC           SECTION.
           INITIALIZE  CHKM-REC
           MOVE    CHKO-KEY            TO  CHKM-KEY
           MOVE    SPA-HOSPNUM         TO  CHKM-HOSPNUM
           MOVE    CHKM-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-CHKM
               MOVE    "tbl_chk_master"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  CHKM-REC
                   MOVE    ZERO                TO  FLG-CHKM
               ELSE
                   MOVE    1                   TO  FLG-CHKM
               END-IF
           END-IF
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-CHKM-READ-EXT.
           EXIT.
      *****************************************************************
      *    �����å��桼�����ơ��֥��ɹ�
      *****************************************************************
       900-CHKU-READ-SEC           SECTION.
           INITIALIZE  CHKU-REC
           MOVE    CHKW-KEY            TO  CHKU-KEY
           MOVE    SPA-HOSPNUM         TO  CHKU-HOSPNUM
           MOVE    CHKU-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-CHKU
               MOVE    "tbl_chk_user"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  CHKU-REC
                   MOVE    ZERO                TO  FLG-CHKU
               ELSE
                   MOVE    1                   TO  FLG-CHKU
               END-IF
           END-IF
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-CHKU-READ-EXT.
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
           STRING  "*(ORCBMIGCHK)* "       DELIMITED BY SIZE
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