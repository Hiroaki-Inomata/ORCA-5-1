       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBUMCTENSU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタリスト処理
      *  コンポーネント名  : 点数マスタチェック
      *                    : 点数マスタのユーザーと標準提供の
      *                    : 内容をチェックしリストする
      *  管理者            :
      *  作成日付    作業者        記述
      *  19/03/07    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  BUMCPERR-FILE   ASSIGN  BUMCP-ERRFNAME
                                   FILE    STATUS  IS  STS-BUMCPERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    エラーファイル
       FD  BUMCPERR-FILE.
       01  BUMCPERR-REC                PIC X(200).
      *
       WORKING-STORAGE             SECTION.
       01  STS-AREA.
           03  STS-BUMCPERR            PIC X(02).
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSUM              PIC 9(01).
           03  FLG-TENSUU              PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-DETAIL              PIC 9(01).
           03  FLG-DECIMAL             PIC 9(01).
       01  CNT-AREA.
           03  CNT-TENSUU              PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-LINE                PIC 9(03).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-CSV                 PIC 9(06).
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(05).
      *    一時領域
       01  WRK-AREA.
           03  WRK-BUMCPERR            PIC X(200).
           03  WRK-MSG                 PIC X(256).
           03  WRK-MSG-CNT             PIC ZZZ,ZZ9.
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-LNKYMD              PIC X(10).
           03  WRK-HENYMDG             PIC X(09).
           03  WRK-CSV-ITEM            PIC X(22).
           03  WRK-CSV-VAL-USR         PIC X(200).
           03  WRK-CSV-VAL-STD         PIC X(200).
           03  WRK-PRF-STD-R           PIC 9(08)V9(05).
           03  WRK-PRF-USR-R           PIC 9(08)V9(05).
           03  WRK-DECIMAL             PIC 9(08)V9(05).
           03  WRK-DECIMAL-R           REDEFINES   WRK-DECIMAL.
               05  WRK-DECIMAL-INT     PIC 9(08).
               05  WRK-DECIMAL-DEC     PIC 9(05).
           03  WRK-ZERO-SUP.
               05  WRK-ZERO-SUP-N      PIC ZZZZZZZ9.
           03  WRK-PRF-DECIMAL.
               05  WRK-PRF-INT         PIC X(08).
               05  WRK-PRF-POINT       PIC X(01).
               05  WRK-PRF-DEC         PIC X(05).
      *    ＣＳＶデータ編集用
       01  WRK-CSV-AREA.
           03  WRK-IN-DATA             PIC X(2000).
           03  WRK-OUT-REC             PIC X(2000).
           03  WRK-CSV-REC             PIC X(2000).
           03  WRK-REC-LENGTH          PIC 9(05).
           03  WRK-IN-MAX              PIC 9(04).
           03  WRK-END                 PIC 9(01).
           03  WRK-CSVDIR              PIC X(09)   VALUE
                                       "/var/tmp/".
           03  WRK-CSVFNAME            PIC X(23)   VALUE
                                       "tensu_user_check.csv".
      *    比較テーブル
       01  TBL-COMP-AREA.
           03  TBL-COMP                PIC 9(01) OCCURS    160.
      *    プルーフ編集領域
       01  PRF-AREA.
           03  PRF-HEADER1.
               05  FILLER              PIC X(32) VALUE
                   "点数ユーザーマスタチェックリスト".
               05  FILLER              PIC X(16) VALUE
                   "【ORCBUMCTENSU】".
               05  PRF-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE "/".
               05  PRF-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE "/".
               05  PRF-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(10) VALUE SPACE.
               05  PRF-PAGE            PIC ZZ,ZZ9.
               05  FILLER              PIC X(03) VALUE " 頁".
           03  PRF-HEADER2.
               05  FILLER              PIC X(01) VALUE "[".
               05  PRF-HOSPNUM         PIC 9(02).
               05  FILLER              PIC X(01) VALUE "]".
               05  PRF-HOSPNAME        PIC X(120) VALUE SPACE.
           03  PRF-HEADER3.
               05  FILLER              PIC X(02) VALUE "Ｆ".
               05  FILLER              PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(06) VALUE "コード".
               05  FILLER              PIC X(09) VALUE SPACE.
               05  FILLER              PIC X(06) VALUE "期　間".
               05  FILLER              PIC X(12) VALUE SPACE.
               05  FILLER              PIC X(12) VALUE "標準提供期間".
               05  FILLER              PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(06) VALUE "名　称".
               05  FILLER              PIC X(50) VALUE SPACE.
               05  FILLER              PIC X(26) VALUE
                                       "Ｆ　◇：ユーザー作成マスタ".
           03  PRF-HEADER4.
               05  FILLER              PIC X(53) VALUE SPACE.
               05  FILLER              PIC X(06) VALUE "内容".
               05  FILLER              PIC X(46) VALUE
                   "（上段　ユーザー設定値、下段　標準提供設定値）".
               05  FILLER              PIC X(08) VALUE SPACE.
               05  FILLER              PIC X(28) VALUE
                                       "★：標準提供マスタと同一内容".
           03  PRF-DETAIL-1.
               05  PRF-FLG             PIC X(02).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  PRF-SRYCD           PIC X(09).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  PRF-YUKOSTYMD       PIC X(09).
               05  FILLER              PIC X(01) VALUE "-".
               05  PRF-YUKOEDYMD       PIC X(09).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  PRF-YUKOSTYMD-M     PIC X(09).
               05  FILLER              PIC X(01) VALUE "-".
               05  PRF-YUKOEDYMD-M     PIC X(09).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  PRF-NAME            PIC X(200).
           03  PRF-DETAIL-3.
               05  FILLER              PIC X(53) VALUE SPACE.
               05  PRF-ITEM            PIC X(18).
               05  PRF-LITE1           PIC X(02) VALUE "：".
               05  PRF-VAL-USR         PIC X(200).
           03  PRF-DETAIL-4.
               05  FILLER              PIC X(73) VALUE SPACE.
               05  PRF-VAL-STD         PIC X(200).
       01  CSV-HEAD-01.
           03  FILLER                  PIC X(02) VALUE "Ｆ".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(06) VALUE "コード".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(06) VALUE "開始日".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(06) VALUE "終了日".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(10) VALUE "標準開始日".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(10) VALUE "標準終了日".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(04) VALUE "名称".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(14) VALUE "項目名（内容）".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(12) VALUE "ユーザー設定".
           03  FILLER                  PIC X(01) VALUE ",".
           03  FILLER                  PIC X(08) VALUE "標準設定".
       01  CONST-AREA.
           03  CONST-COMM3             PIC X(08) VALUE "（空白）".
           03  CONST-KUGIRI            PIC X(01) VALUE ",".
           03  CONST-KAIGYO            PIC X(01) VALUE X"0D".
      *
      *    項目名称テーブル
           COPY    "CPTENSUNAMETBL.INC".
      *
           COPY    "HCMSL55.INC".
       01  MAX-LINE                    PIC 9(03) VALUE 55.
      *
           COPY    "CPRECEERR.INC".
      *****************************************************************
      *    サブプロ用定義
      *****************************************************************
      *    マシン日付取得
           COPY    "CPORCSMCNDATE.INC".
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *    点数マスターテーブル
           COPY    "CPTENSU.INC"       REPLACING
                                       //TNS-// BY //TNSM-//.
      *    点数ユーザーテーブル
           COPY    "CPTENSU.INC"       REPLACING
                                       //TNS-// BY //TNSU-//.
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    連絡　領域
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
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
           DISPLAY "MSTYMD [ " BUMCP-MSTYMD " ]"
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC    UNTIL FLG-END   =   1
           PERFORM 300-END-SEC
           PERFORM 990-EXIT-PROGRAM-SEC
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           INITIALIZE  FLG-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-AREA
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "点数ユーザーマスタチェックリスト"
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
               PERFORM 900-TENSUU-SELECT-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
           IF      BUMCP-MSTYMD    NOT =   SPACE
               IF      TNSU-YUKOEDYMD      <   BUMCP-MSTYMD
                   GO  TO  200-MAIN-90
               END-IF
           END-IF
      *    FLG-SYORI
      *      1:ユーザーあり、標準なし
      *      2:ユーザーあり、標準あり、不一致
      *      3:ユーザーあり、標準あり、一致
           PERFORM 900-TENSUM-SELECT-SEC
           IF      FLG-TENSUM          =   1
               MOVE    1                   TO  FLG-SYORI
               ADD     1                   TO  CNT-OUT
               PERFORM 290-PRF-SEC
           ELSE
               PERFORM UNTIL   FLG-TENSUM  =   1
                   IF     (TNSU-YUKOSTYMD      >   TNSM-YUKOEDYMD)   OR
                          (TNSU-YUKOEDYMD      <   TNSM-YUKOSTYMD)
                       IF     (TNSU-YUKOEDYMD      <   TNSM-YUKOSTYMD)
                           MOVE    1                   TO  FLG-TENSUM
                       END-IF
                   ELSE
                       PERFORM 220-COMPARE-SEC
                       IF      TBL-COMP-AREA       =   ZERO
                           MOVE    3                   TO  FLG-SYORI
                       ELSE
                           MOVE    2                   TO  FLG-SYORI
                       END-IF
                       ADD     1                   TO  CNT-OUT
                       PERFORM 290-PRF-SEC
                   END-IF
                   PERFORM 900-TENSUM-FETCH-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       200-MAIN-90.
           PERFORM 900-TENSUU-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    項目別比較
      *****************************************************************
       220-COMPARE-SEC             SECTION.
           MOVE    ZERO                TO  TBL-COMP-AREA
           IF      TNSU-YUKOSTYMD  NOT =   TNSM-YUKOSTYMD
               MOVE    1                   TO  TBL-COMP (2)
           END-IF
           IF      TNSU-YUKOEDYMD  NOT =   TNSM-YUKOEDYMD
               MOVE    1                   TO  TBL-COMP (3)
           END-IF
           IF      TNSU-SRYKBN     NOT =   TNSM-SRYKBN
               MOVE    1                   TO  TBL-COMP (4)
           END-IF
           IF      TNSU-SRYSYUKBN  NOT =   TNSM-SRYSYUKBN
               MOVE    1                   TO  TBL-COMP (5)
           END-IF
           IF      TNSU-YUKOKETA   NOT =   TNSM-YUKOKETA
               MOVE    1                   TO  TBL-COMP (6)
           END-IF
           IF      TNSU-NAME       NOT =   TNSM-NAME
               MOVE    1                   TO  TBL-COMP (7)
           END-IF
           IF      TNSU-KANAYUKOKETA
                                   NOT =   TNSM-KANAYUKOKETA
               MOVE    1                   TO  TBL-COMP (8)
           END-IF
           IF      TNSU-KANANAME   NOT =   TNSM-KANANAME
               MOVE    1                   TO  TBL-COMP (9)
           END-IF
           IF      TNSU-FORMALYUKOKETA
                                   NOT =   TNSM-FORMALYUKOKETA
               MOVE    1                   TO  TBL-COMP (10)
           END-IF
           IF      TNSU-FORMALNAME NOT =   TNSM-FORMALNAME
               MOVE    1                   TO  TBL-COMP (11)
           END-IF
           IF      TNSU-TENSIKIBETU
                                   NOT =   TNSM-TENSIKIBETU
               MOVE    1                   TO  TBL-COMP (12)
           END-IF
           IF      TNSU-TEN        NOT =   TNSM-TEN
               MOVE    1                   TO  TBL-COMP (13)
           END-IF
           IF      TNSU-TANICD     NOT =   TNSM-TANICD
               MOVE    1                   TO  TBL-COMP (14)
           END-IF
           IF      TNSU-TANIMOJISU NOT =   TNSM-TANIMOJISU
               MOVE    1                   TO  TBL-COMP (15)
           END-IF
           IF      TNSU-TANINAME   NOT =   TNSM-TANINAME
               MOVE    1                   TO  TBL-COMP (16)
           END-IF
           IF      TNSU-DATAKBN    NOT =   TNSM-DATAKBN
               MOVE    1                   TO  TBL-COMP (17)
           END-IF
           IF      TNSU-HKNTEKKBN  NOT =   TNSM-HKNTEKKBN
               MOVE    1                   TO  TBL-COMP (18)
           END-IF
           IF      TNSU-NYUGAITEKKBN
                                   NOT =   TNSM-NYUGAITEKKBN
               MOVE    1                   TO  TBL-COMP (19)
           END-IF
           IF      TNSU-ROUTEKKBN  NOT =   TNSM-ROUTEKKBN
               MOVE    1                   TO  TBL-COMP (20)
           END-IF
           IF      TNSU-HOSPSRYKBN NOT =   TNSM-HOSPSRYKBN
               MOVE    1                   TO  TBL-COMP (21)
           END-IF
           IF      TNSU-OSINKBN    NOT =   TNSM-OSINKBN
               MOVE    1                   TO  TBL-COMP (22)
           END-IF
           IF      TNSU-HOUKNSKBN  NOT =   TNSM-HOUKNSKBN
               MOVE    1                   TO  TBL-COMP (23)
           END-IF
           IF      TNSU-BYOKANRENKBN
                                   NOT =   TNSM-BYOKANRENKBN
               MOVE    1                   TO  TBL-COMP (24)
           END-IF
           IF      TNSU-SDOKNRYO   NOT =   TNSM-SDOKNRYO
               MOVE    1                   TO  TBL-COMP (25)
           END-IF
           IF      TNSU-JITUDAY    NOT =   TNSM-JITUDAY
               MOVE    1                   TO  TBL-COMP (26)
           END-IF
           IF      TNSU-DAYCNT     NOT =   TNSM-DAYCNT
               MOVE    1                   TO  TBL-COMP (27)
           END-IF
           IF      TNSU-YKZKNRKBN  NOT =   TNSM-YKZKNRKBN
               MOVE    1                   TO  TBL-COMP (28)
           END-IF
           IF      TNSU-KZMCOMPSIKIBETU
                                   NOT =   TNSM-KZMCOMPSIKIBETU
               MOVE    1                   TO  TBL-COMP (29)
           END-IF
           IF      TNSU-KZMKGN     NOT =   TNSM-KZMKGN
               MOVE    1                   TO  TBL-COMP (30)
           END-IF
           IF      TNSU-KZMJGN     NOT =   TNSM-KZMJGN
               MOVE    1                   TO  TBL-COMP (31)
           END-IF
           IF      TNSU-KZM        NOT =   TNSM-KZM
               MOVE    1                   TO  TBL-COMP (32)
           END-IF
           IF      TNSU-KZMTEN     NOT =   TNSM-KZMTEN
               MOVE    1                   TO  TBL-COMP (33)
           END-IF
           IF      TNSU-KZMERR     NOT =   TNSM-KZMERR
               MOVE    1                   TO  TBL-COMP (34)
           END-IF
           IF      TNSU-JGNCNT     NOT =   TNSM-JGNCNT
               MOVE    1                   TO  TBL-COMP (35)
           END-IF
           IF      TNSU-JGNCNT1D   NOT =   TNSM-JGNCNT1D
               MOVE    1                   TO  TBL-COMP (36)
           END-IF
           IF      TNSU-JGNCNT1W   NOT =   TNSM-JGNCNT1W
               MOVE    1                   TO  TBL-COMP (37)
           END-IF
           IF      TNSU-JGNCNTETCM NOT =   TNSM-JGNCNTETCM
               MOVE    1                   TO  TBL-COMP (38)
           END-IF
           IF      TNSU-JGNCNTETC  NOT =   TNSM-JGNCNTETC
               MOVE    1                   TO  TBL-COMP (39)
           END-IF
           IF      TNSU-JGNCNTERR  NOT =   TNSM-JGNCNTERR
               MOVE    1                   TO  TBL-COMP (40)
           END-IF
           IF      TNSU-CHUKSNCD   NOT =   TNSM-CHUKSNCD
               MOVE    1                   TO  TBL-COMP (41)
           END-IF
           IF      TNSU-CHUKSNTUBAN
                                   NOT =   TNSM-CHUKSNTUBAN
               MOVE    1                   TO  TBL-COMP (42)
           END-IF
           IF      TNSU-TSUSOKUAGE NOT =   TNSM-TSUSOKUAGE
               MOVE    1                   TO  TBL-COMP (43)
           END-IF
           IF      TNSU-KGNAGE     NOT =   TNSM-KGNAGE
               MOVE    1                   TO  TBL-COMP (44)
           END-IF
           IF      TNSU-JGNAGE     NOT =   TNSM-JGNAGE
               MOVE    1                   TO  TBL-COMP (45)
           END-IF
           IF      TNSU-TIMEKSNKBN NOT =   TNSM-TIMEKSNKBN
               MOVE    1                   TO  TBL-COMP (46)
           END-IF
           IF      TNSU-KIJUNTEIGENKBN
                                   NOT =   TNSM-KIJUNTEIGENKBN
               MOVE    1                   TO  TBL-COMP (47)
           END-IF
           IF      TNSU-KIJUNTEIGENCD
                                   NOT =   TNSM-KIJUNTEIGENCD
               MOVE    1                   TO  TBL-COMP (48)
           END-IF
           IF      TNSU-LASERKBN   NOT =   TNSM-LASERKBN
               MOVE    1                   TO  TBL-COMP (49)
           END-IF
           IF      TNSU-CHPMESUKSN NOT =   TNSM-CHPMESUKSN
               MOVE    1                   TO  TBL-COMP (50)
           END-IF
           IF      TNSU-MICMESUKSN NOT =   TNSM-MICMESUKSN
               MOVE    1                   TO  TBL-COMP (51)
           END-IF
           IF      TNSU-DONORKBN   NOT =   TNSM-DONORKBN
               MOVE    1                   TO  TBL-COMP (52)
           END-IF
           IF      TNSU-KNSJISKBN  NOT =   TNSM-KNSJISKBN
               MOVE    1                   TO  TBL-COMP (53)
           END-IF
           IF      TNSU-KNSJISGRPKBN
                                   NOT =   TNSM-KNSJISGRPKBN
               MOVE    1                   TO  TBL-COMP (54)
           END-IF
           IF      TNSU-TEIGENKBN  NOT =   TNSM-TEIGENKBN
               MOVE    1                   TO  TBL-COMP (55)
           END-IF
           IF      TNSU-SEKIZUISOKUTEI
                                   NOT =   TNSM-SEKIZUISOKUTEI
               MOVE    1                   TO  TBL-COMP (56)
           END-IF
           IF      TNSU-KEIBUJYUTU NOT =   TNSM-KEIBUJYUTU
               MOVE    1                   TO  TBL-COMP (57)
           END-IF
           IF      TNSU-AUTOHOUGO  NOT =   TNSM-AUTOHOUGO
               MOVE    1                   TO  TBL-COMP (58)
           END-IF
           IF      TNSU-GAIKANRIKBN
                                   NOT =   TNSM-GAIKANRIKBN
               MOVE    1                   TO  TBL-COMP (59)
           END-IF
           IF      TNSU-TUSOKUGAIKBN
                                   NOT =   TNSM-TUSOKUGAIKBN
               MOVE    1                   TO  TBL-COMP (60)
           END-IF
           IF      TNSU-HOUKATUTEIGENKBN
                                   NOT =   TNSM-HOUKATUTEIGENKBN
               MOVE    1                   TO  TBL-COMP (61)
           END-IF
           IF      TNSU-CHPNAISIKSN
                                   NOT =   TNSM-CHPNAISIKSN
               MOVE    1                   TO  TBL-COMP (62)
           END-IF
           IF      TNSU-AUTOFUNGO  NOT =   TNSM-AUTOFUNGO
               MOVE    1                   TO  TBL-COMP (63)
           END-IF
           IF      TNSU-CHIIKIKSN  NOT =   TNSM-CHIIKIKSN
               MOVE    1                   TO  TBL-COMP (64)
           END-IF
           IF      TNSU-BYOSYOKBN  NOT =   TNSM-BYOSYOKBN
               MOVE    1                   TO  TBL-COMP (65)
           END-IF
           IF      TNSU-CHPGYOKOSOTIKSN
                                   NOT =   TNSM-CHPGYOKOSOTIKSN
               MOVE    1                   TO  TBL-COMP (66)
           END-IF
           IF      TNSU-SHORTSTAYOPE
                                   NOT =   TNSM-SHORTSTAYOPE
               MOVE    1                   TO  TBL-COMP (67)
           END-IF
           IF      TNSU-BUIKBN     NOT =   TNSM-BUIKBN
               MOVE    1                   TO  TBL-COMP (68)
           END-IF
           IF      TNSU-SANTEIRRKKBN
                                   NOT =   TNSM-SANTEIRRKKBN
               MOVE    1                   TO  TBL-COMP (69)
           END-IF
           IF      TNSU-SSTKIJUNCD(1)
                                   NOT =   TNSM-SSTKIJUNCD(1)
               MOVE    1                   TO  TBL-COMP (70)
           END-IF
           IF      TNSU-SSTKIJUNCD(2)
                                   NOT =   TNSM-SSTKIJUNCD(2)
               MOVE    1                   TO  TBL-COMP (71)
           END-IF
           IF      TNSU-SSTKIJUNCD(3)
                                   NOT =   TNSM-SSTKIJUNCD(3)
               MOVE    1                   TO  TBL-COMP (72)
           END-IF
           IF      TNSU-SSTKIJUNCD(4)
                                   NOT =   TNSM-SSTKIJUNCD(4)
               MOVE    1                   TO  TBL-COMP (73)
           END-IF
           IF      TNSU-SSTKIJUNCD(5)
                                   NOT =   TNSM-SSTKIJUNCD(5)
               MOVE    1                   TO  TBL-COMP (74)
           END-IF
           IF      TNSU-SSTKIJUNCD(6)
                                   NOT =   TNSM-SSTKIJUNCD(6)
               MOVE    1                   TO  TBL-COMP (75)
           END-IF
           IF      TNSU-SSTKIJUNCD(7)
                                   NOT =   TNSM-SSTKIJUNCD(7)
               MOVE    1                   TO  TBL-COMP (76)
           END-IF
           IF      TNSU-SSTKIJUNCD(8)
                                   NOT =   TNSM-SSTKIJUNCD(8)
               MOVE    1                   TO  TBL-COMP (77)
           END-IF
           IF      TNSU-SSTKIJUNCD(9)
                                   NOT =   TNSM-SSTKIJUNCD(9)
               MOVE    1                   TO  TBL-COMP (78)
           END-IF
           IF      TNSU-SSTKIJUNCD(10)
                                   NOT =   TNSM-SSTKIJUNCD(10)
               MOVE    1                   TO  TBL-COMP (79)
           END-IF
           IF      TNSU-AGEKSNKGN(1)
                                   NOT =   TNSM-AGEKSNKGN(1)
               MOVE    1                   TO  TBL-COMP (80)
           END-IF
           IF      TNSU-AGEKSNJGN(1)
                                   NOT =   TNSM-AGEKSNJGN(1)
               MOVE    1                   TO  TBL-COMP (81)
           END-IF
           IF      TNSU-AGEKSNSRYCD(1)
                                   NOT =   TNSM-AGEKSNSRYCD(1)
               MOVE    1                   TO  TBL-COMP (82)
           END-IF
           IF      TNSU-AGEKSNKGN(2)
                                   NOT =   TNSM-AGEKSNKGN(2)
               MOVE    1                   TO  TBL-COMP (83)
           END-IF
           IF      TNSU-AGEKSNJGN(2)
                                   NOT =   TNSM-AGEKSNJGN(2)
               MOVE    1                   TO  TBL-COMP (84)
           END-IF
           IF      TNSU-AGEKSNSRYCD(2)
                                   NOT =   TNSM-AGEKSNSRYCD(2)
               MOVE    1                   TO  TBL-COMP (85)
           END-IF
           IF      TNSU-AGEKSNKGN(3)
                                   NOT =   TNSM-AGEKSNKGN(3)
               MOVE    1                   TO  TBL-COMP (86)
           END-IF
           IF      TNSU-AGEKSNJGN(3)
                                   NOT =   TNSM-AGEKSNJGN(3)
               MOVE    1                   TO  TBL-COMP (87)
           END-IF
           IF      TNSU-AGEKSNSRYCD(3)
                                   NOT =   TNSM-AGEKSNSRYCD(3)
               MOVE    1                   TO  TBL-COMP (88)
           END-IF
           IF      TNSU-AGEKSNKGN(4)
                                   NOT =   TNSM-AGEKSNKGN(4)
               MOVE    1                   TO  TBL-COMP (89)
           END-IF
           IF      TNSU-AGEKSNJGN(4)
                                   NOT =   TNSM-AGEKSNJGN(4)
               MOVE    1                   TO  TBL-COMP (90)
           END-IF
           IF      TNSU-AGEKSNSRYCD(4)
                                   NOT =   TNSM-AGEKSNSRYCD(4)
               MOVE    1                   TO  TBL-COMP (91)
           END-IF
           IF      TNSU-KENTAICOMMENT
                                   NOT =   TNSM-KENTAICOMMENT
               MOVE    1                   TO  TBL-COMP (92)
           END-IF
           IF      TNSU-NYUKHNKBN  NOT =   TNSM-NYUKHNKBN
               MOVE    1                   TO  TBL-COMP (93)
           END-IF
           IF      TNSU-NYUKHNKSNKBN
                                   NOT =   TNSM-NYUKHNKSNKBN
               MOVE    1                   TO  TBL-COMP (94)
           END-IF
           IF      TNSU-KANGOKSN   NOT =   TNSM-KANGOKSN
               MOVE    1                   TO  TBL-COMP (95)
           END-IF
           IF      TNSU-OLDTENSKB  NOT =   TNSM-OLDTENSKB
               MOVE    1                   TO  TBL-COMP (96)
           END-IF
           IF      TNSU-OLDTEN     NOT =   TNSM-OLDTEN
               MOVE    1                   TO  TBL-COMP (97)
           END-IF
           IF      TNSU-MADOKUKBN  NOT =   TNSM-MADOKUKBN
               MOVE    1                   TO  TBL-COMP (98)
           END-IF
           IF      TNSU-SINKEIHAKAIKBN
                                   NOT =   TNSM-SINKEIHAKAIKBN
               MOVE    1                   TO  TBL-COMP (99)
           END-IF
           IF      TNSU-SEIBUTUGAKUKBN
                                   NOT =   TNSM-SEIBUTUGAKUKBN
               MOVE    1                   TO  TBL-COMP (100)
           END-IF
           IF      TNSU-ZOEIZAIKBN NOT =   TNSM-ZOEIZAIKBN
               MOVE    1                   TO  TBL-COMP (101)
           END-IF
           IF      TNSU-CSYYOURYO  NOT =   TNSM-CSYYOURYO
               MOVE    1                   TO  TBL-COMP (102)
           END-IF
           IF      TNSU-YKZKBN     NOT =   TNSM-YKZKBN
               MOVE    1                   TO  TBL-COMP (103)
           END-IF
           IF      TNSU-ZAIGATAKBN NOT =   TNSM-ZAIGATAKBN
               MOVE    1                   TO  TBL-COMP (104)
           END-IF
           IF      TNSU-KOUHATUKBN NOT =   TNSM-KOUHATUKBN
               MOVE    1                   TO  TBL-COMP (105)
           END-IF
           IF      TNSU-LONGTOYOKBN
                                   NOT =   TNSM-LONGTOYOKBN
               MOVE    1                   TO  TBL-COMP (106)
           END-IF
           IF      TNSU-MEIUSESKB  NOT =   TNSM-MEIUSESKB
               MOVE    1                   TO  TBL-COMP (107)
           END-IF
           IF      TNSU-TOKUKIZAIAGEKSNKBN
                                   NOT =   TNSM-TOKUKIZAIAGEKSNKBN
               MOVE    1                   TO  TBL-COMP (108)
           END-IF
           IF      TNSU-SANSOKBN   NOT =   TNSM-SANSOKBN
               MOVE    1                   TO  TBL-COMP (109)
           END-IF
           IF      TNSU-TOKUKIZAISBT1
                                   NOT =   TNSM-TOKUKIZAISBT1
               MOVE    1                   TO  TBL-COMP (110)
           END-IF
           IF      TNSU-TOKUKIZAISBT2
                                   NOT =   TNSM-TOKUKIZAISBT2
               MOVE    1                   TO  TBL-COMP (111)
           END-IF
           IF      TNSU-JGNTEN     NOT =   TNSM-JGNTEN
               MOVE    1                   TO  TBL-COMP (112)
           END-IF
           IF      TNSU-GAITENTTLKBN
                                   NOT =   TNSM-GAITENTTLKBN
               MOVE    1                   TO  TBL-COMP (113)
           END-IF
           IF      TNSU-NYUTENTTLKBN
                                   NOT =   TNSM-NYUTENTTLKBN
               MOVE    1                   TO  TBL-COMP (114)
           END-IF
           IF      TNSU-CDKBN-KBN  NOT =   TNSM-CDKBN-KBN
               MOVE    1                   TO  TBL-COMP (115)
           END-IF
           IF      TNSU-CDKBN-SYO  NOT =   TNSM-CDKBN-SYO
               MOVE    1                   TO  TBL-COMP (116)
           END-IF
           IF      TNSU-CDKBN-BU   NOT =   TNSM-CDKBN-BU
               MOVE    1                   TO  TBL-COMP (117)
           END-IF
           IF      TNSU-CDKBN-KBNNUM
                                   NOT =   TNSM-CDKBN-KBNNUM
               MOVE    1                   TO  TBL-COMP (118)
           END-IF
           IF      TNSU-CDKBN-KBNNUM-EDA
                                   NOT =   TNSM-CDKBN-KBNNUM-EDA
               MOVE    1                   TO  TBL-COMP (119)
           END-IF
           IF      TNSU-CDKBN-KOUBAN
                                   NOT =   TNSM-CDKBN-KOUBAN
               MOVE    1                   TO  TBL-COMP (120)
           END-IF
           IF      TNSU-KOKUCHI-KBN
                                   NOT =   TNSM-KOKUCHI-KBN
               MOVE    1                   TO  TBL-COMP (121)
           END-IF
           IF      TNSU-KOKUCHI-SYO
                                   NOT =   TNSM-KOKUCHI-SYO
               MOVE    1                   TO  TBL-COMP (122)
           END-IF
           IF      TNSU-KOKUCHI-BU NOT =   TNSM-KOKUCHI-BU
               MOVE    1                   TO  TBL-COMP (123)
           END-IF
           IF      TNSU-KOKUCHI-KBNNUM
                                   NOT =   TNSM-KOKUCHI-KBNNUM
               MOVE    1                   TO  TBL-COMP (124)
           END-IF
           IF      TNSU-KOKUCHI-KBNNUM-EDA
                                   NOT =   TNSM-KOKUCHI-KBNNUM-EDA
               MOVE    1                   TO  TBL-COMP (125)
           END-IF
           IF      TNSU-KOKUCHI-KOUBAN
                                   NOT =   TNSM-KOKUCHI-KOUBAN
               MOVE    1                   TO  TBL-COMP (126)
           END-IF
           IF      TNSU-KOKUJISKBKBN1
                                   NOT =   TNSM-KOKUJISKBKBN1
               MOVE    1                   TO  TBL-COMP (127)
           END-IF
           IF      TNSU-KOKUJISKBKBN2
                                   NOT =   TNSM-KOKUJISKBKBN2
               MOVE    1                   TO  TBL-COMP (128)
           END-IF
           IF      TNSU-NAMECHGKBN NOT =   TNSM-NAMECHGKBN
               MOVE    1                   TO  TBL-COMP (129)
           END-IF
           IF      TNSU-KANANAMECHGKBN
                                   NOT =   TNSM-KANANAMECHGKBN
               MOVE    1                   TO  TBL-COMP (130)
           END-IF
           IF      TNSU-IDOKANREN  NOT =   TNSM-IDOKANREN
               MOVE    1                   TO  TBL-COMP (131)
           END-IF
           IF      TNSU-KOUHYOJYUNNUM
                                   NOT =   TNSM-KOUHYOJYUNNUM
               MOVE    1                   TO  TBL-COMP (132)
           END-IF
           IF      TNSU-YAKKAKJNCD NOT =   TNSM-YAKKAKJNCD
               MOVE    1                   TO  TBL-COMP (133)
           END-IF
           IF      TNSU-SYUSAISKB  NOT =   TNSM-SYUSAISKB
               MOVE    1                   TO  TBL-COMP (134)
           END-IF
           IF      TNSU-SYOMEIKANREN
                                   NOT =   TNSM-SYOMEIKANREN
               MOVE    1                   TO  TBL-COMP (135)
           END-IF
           IF      TNSU-CHGYMD     NOT =   TNSM-CHGYMD
               MOVE    1                   TO  TBL-COMP (136)
           END-IF
           IF      TNSU-HAISIYMD   NOT =   TNSM-HAISIYMD
               MOVE    1                   TO  TBL-COMP (137)
           END-IF
           IF      TNSU-KEIKASOCHIYMD
                                   NOT =   TNSM-KEIKASOCHIYMD
               MOVE    1                   TO  TBL-COMP (138)
           END-IF
      *    139 端末ＩＤ
      *    140 オペレーターＩＤ
      *    141 作成年月日
      *    142 更新年月日
      *    143 更新時間
      *    144 医療機関識別番号
           IF      TNSU-GAZOOPESUP NOT =   TNSM-GAZOOPESUP
               MOVE    1                   TO  TBL-COMP (145)
           END-IF
           IF      TNSU-IRYOKANSATUKBN
                                   NOT =   TNSM-IRYOKANSATUKBN
               MOVE    1                   TO  TBL-COMP (146)
           END-IF
           IF      TNSU-MASUISKBKBN
                                   NOT =   TNSM-MASUISKBKBN
               MOVE    1                   TO  TBL-COMP (147)
           END-IF
      *H24.4追加４つ
           IF      TNSU-FUKUBIKUNAIKBN
                                   NOT =   TNSM-FUKUBIKUNAIKBN 
               MOVE    1                   TO  TBL-COMP (148)
           END-IF
           IF      TNSU-FUKUBIKUKOTUKBN
                                   NOT =   TNSM-FUKUBIKUKOTUKBN 
               MOVE    1                   TO  TBL-COMP (149)
           END-IF
           IF      TNSU-TIMEMASUIKBN
                                   NOT =   TNSM-TIMEMASUIKBN 
               MOVE    1                   TO  TBL-COMP (150)
           END-IF
           IF      TNSU-DPCKBN     NOT =   TNSM-DPCKBN 
               MOVE    1                   TO  TBL-COMP (151)
           END-IF
           IF      TNSU-HISINSYUMONITORKSN
                                   NOT =   TNSM-HISINSYUMONITORKSN
               MOVE    1                   TO  TBL-COMP (152)
           END-IF
           IF      TNSU-TOUKETUHOZONKSN
                                   NOT =   TNSM-TOUKETUHOZONKSN
               MOVE    1                   TO  TBL-COMP (153)
           END-IF
           IF      TNSU-KUBUNBANGOU
                                   NOT =   TNSM-KUBUNBANGOU
               MOVE    1                   TO  TBL-COMP (154)
           END-IF
           IF      TNSU-ROSAIKBN   NOT =   TNSM-ROSAIKBN
               MOVE    1                   TO  TBL-COMP (155)
           END-IF
           IF      TNSU-SISIKSN    NOT =   TNSM-SISIKSN
               MOVE    1                   TO  TBL-COMP (156)
           END-IF
      *H30.4追加１つ
           IF      TNSU-AKUSEIBYORIKSN
                                   NOT =   TNSM-AKUSEIBYORIKSN
               MOVE    1                   TO  TBL-COMP (157)
           END-IF
      *R02.4追加３つ
           IF      TNSU-SOGAIKOTEIKSN
                                   NOT =   TNSM-SOGAIKOTEIKSN
               MOVE    1                   TO  TBL-COMP (158)
           END-IF
           IF      TNSU-CHPSESSAKUKSN
                                   NOT =   TNSM-CHPSESSAKUKSN
               MOVE    1                   TO  TBL-COMP (159)
           END-IF
           IF      TNSU-SASINJIHEISIKBN
                                   NOT =   TNSM-SASINJIHEISIKBN
               MOVE    1                   TO  TBL-COMP (160)
           END-IF
      *
      * 特定器材　金額を点数金額へ登録するマスタであるか判定
           IF     (TBL-COMP (13)       =   1)  OR
                  (TBL-COMP (97)       =   1)
               IF     (TNSM-SRYCD(1:1)     =   "7")  AND
                      (TNSM-TENSIKIBETU    =   2)
      **             ((TNSM-TENSIKIBETU    =   2)    OR
      **              (TNSM-SANSOKBN       =   2 OR 3 OR 4
      **                                    OR 5 OR 9))
                   MOVE    ZERO            TO  TBL-COMP (13)
                   MOVE    ZERO            TO  TBL-COMP (97)
               END-IF
           END-IF
           .
       220-COMPARE-EXT.
           EXIT.
      *****************************************************************
      *    プルーフ出力処理
      *****************************************************************
       290-PRF-SEC                 SECTION.
           IF      CNT-LINE            =   ZERO
               PERFORM 2903-PRF-HEADER-SEC
           END-IF
           MOVE    1                   TO  FLG-DETAIL
           MOVE    TNSU-SRYCD          TO  PRF-SRYCD
      *    MOVE    TNSU-YUKOSTYMD      TO  PRF-YUKOSTYMD
           MOVE    TNSU-YUKOSTYMD      TO  WRK-LNKYMD
           PERFORM 800-YMDCONV21-SEC
           MOVE    WRK-HENYMDG         TO  PRF-YUKOSTYMD
      *    MOVE    TNSU-YUKOEDYMD      TO  PRF-YUKOEDYMD
           MOVE    TNSU-YUKOEDYMD      TO  WRK-LNKYMD
           PERFORM 800-YMDCONV21-SEC
           MOVE    WRK-HENYMDG         TO  PRF-YUKOEDYMD
           MOVE    SPACE               TO  PRF-YUKOSTYMD-M
           MOVE    SPACE               TO  PRF-YUKOEDYMD-M
           IF     (TNSU-YUKOSTYMD  NOT =   TNSM-YUKOSTYMD)   OR
                  (TNSU-YUKOEDYMD  NOT =   TNSM-YUKOEDYMD)
               MOVE    TNSM-YUKOSTYMD      TO  WRK-LNKYMD
               PERFORM 800-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  PRF-YUKOSTYMD-M
               MOVE    TNSM-YUKOEDYMD      TO  WRK-LNKYMD
               PERFORM 800-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  PRF-YUKOEDYMD-M
           END-IF
           MOVE    TNSU-NAME           TO  PRF-NAME
           EVALUATE    FLG-SYORI
               WHEN    1
                   MOVE    "◇"            TO  PRF-FLG
               WHEN    2
                   MOVE    SPACE           TO  PRF-FLG
               WHEN    3
                   MOVE    "★"            TO  PRF-FLG
           END-EVALUATE
           PERFORM 2901-PRF-SEC
           IF      FLG-SYORI           =   2
               PERFORM VARYING IDX
                       FROM    2   BY  1
                       UNTIL   IDX >   TBL-TENSU-NAME-MAX
                   MOVE    ZERO                TO  FLG-DECIMAL
                   IF      TBL-COMP (IDX)      =   1
                       SET     TBL-TNM             TO  IDX
                       MOVE    TBL-TNM-NAME(TBL-TNM)
                                                   TO  PRF-ITEM
                       PERFORM 2902-PRF-SET-VALUE-SEC
                       MOVE    3                   TO  FLG-DETAIL
                       PERFORM 2901-PRF-SEC
                       MOVE    4                   TO  FLG-DETAIL
                       PERFORM 2901-PRF-SEC
                       IF      BUMCP-CSVKBN     =   "1"
                           PERFORM 600-CSV-SYORI-SEC
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               IF      BUMCP-CSVKBN     =   "1"
                   PERFORM 600-CSV-SYORI-SEC
               END-IF
           END-IF
           .
       290-PRF-EXT.
           EXIT.
      *****************************************************************
      *    プルーフ出力処理
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
                   IF      FLG-DECIMAL         =   1
                       PERFORM 800-DECIMAL-SEC
                   END-IF
                   MOVE    PRF-DETAIL-3
                                       TO  HCMSL55-MOJI (CNT-LINE)
               WHEN    4
                   IF      PRF-VAL-STD         =   SPACE
                       MOVE    CONST-COMM3         TO  PRF-VAL-STD
                   END-IF
                   IF      FLG-DECIMAL         =   1
                       PERFORM 800-DECIMAL-SEC
                   END-IF
                   MOVE    PRF-DETAIL-4
                                       TO  HCMSL55-MOJI (CNT-LINE)
           END-EVALUATE
           MOVE    ZERO                TO  FLG-DETAIL
           .
       2901-PRF-EXT.
           EXIT.
      *****************************************************************
      *    変更個所表示処理
      *****************************************************************
       2902-PRF-SET-VALUE-SEC      SECTION.
           EVALUATE    IDX
               WHEN    2
                   MOVE    TNSU-YUKOSTYMD      TO  PRF-VAL-USR
                   MOVE    TNSM-YUKOSTYMD      TO  PRF-VAL-STD
               WHEN    3
                   MOVE    TNSU-YUKOEDYMD      TO  PRF-VAL-USR
                   MOVE    TNSM-YUKOEDYMD      TO  PRF-VAL-STD
               WHEN    4
                   MOVE    TNSU-SRYKBN         TO  PRF-VAL-USR
                   MOVE    TNSM-SRYKBN         TO  PRF-VAL-STD
               WHEN    5
                   MOVE    TNSU-SRYSYUKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-SRYSYUKBN      TO  PRF-VAL-STD
               WHEN    6
                   MOVE    TNSU-YUKOKETA       TO  PRF-VAL-USR
                   MOVE    TNSM-YUKOKETA       TO  PRF-VAL-STD
               WHEN    7
                   MOVE    TNSU-NAME           TO  PRF-VAL-USR
                   MOVE    TNSM-NAME           TO  PRF-VAL-STD
               WHEN    8
                   MOVE    TNSU-KANAYUKOKETA   TO  PRF-VAL-USR
                   MOVE    TNSM-KANAYUKOKETA   TO  PRF-VAL-STD
               WHEN    9
                   MOVE    TNSU-KANANAME       TO  PRF-VAL-USR
                   MOVE    TNSM-KANANAME       TO  PRF-VAL-STD
               WHEN    10
                   MOVE    TNSU-FORMALYUKOKETA TO  PRF-VAL-USR
                   MOVE    TNSM-FORMALYUKOKETA TO  PRF-VAL-STD
               WHEN    11
                   MOVE    TNSU-FORMALNAME     TO  PRF-VAL-USR
                   MOVE    TNSM-FORMALNAME     TO  PRF-VAL-STD
               WHEN    12
                   MOVE    TNSU-TENSIKIBETU    TO  PRF-VAL-USR
                   MOVE    TNSM-TENSIKIBETU    TO  PRF-VAL-STD
               WHEN    13
                   MOVE    1                   TO  FLG-DECIMAL
                   MOVE    TNSU-TEN            TO  WRK-PRF-USR-R
                   MOVE    TNSM-TEN            TO  WRK-PRF-STD-R
               WHEN    14
                   MOVE    TNSU-TANICD         TO  PRF-VAL-USR
                   MOVE    TNSM-TANICD         TO  PRF-VAL-STD
               WHEN    15
                   MOVE    TNSU-TANIMOJISU     TO  PRF-VAL-USR
                   MOVE    TNSM-TANIMOJISU     TO  PRF-VAL-STD
               WHEN    16
                   MOVE    TNSU-TANINAME       TO  PRF-VAL-USR
                   MOVE    TNSM-TANINAME       TO  PRF-VAL-STD
               WHEN    17
                   MOVE    TNSU-DATAKBN        TO  PRF-VAL-USR
                   MOVE    TNSM-DATAKBN        TO  PRF-VAL-STD
               WHEN    18
                   MOVE    TNSU-HKNTEKKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-HKNTEKKBN      TO  PRF-VAL-STD
               WHEN    19
                   MOVE    TNSU-NYUGAITEKKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-NYUGAITEKKBN   TO  PRF-VAL-STD
               WHEN    20
                   MOVE    TNSU-ROUTEKKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-ROUTEKKBN      TO  PRF-VAL-STD
               WHEN    21
                   MOVE    TNSU-HOSPSRYKBN     TO  PRF-VAL-USR
                   MOVE    TNSM-HOSPSRYKBN     TO  PRF-VAL-STD
               WHEN    22
                   MOVE    TNSU-OSINKBN        TO  PRF-VAL-USR
                   MOVE    TNSM-OSINKBN        TO  PRF-VAL-STD
               WHEN    23
                   MOVE    TNSU-HOUKNSKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-HOUKNSKBN      TO  PRF-VAL-STD
               WHEN    24
                   MOVE    TNSU-BYOKANRENKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-BYOKANRENKBN   TO  PRF-VAL-STD
               WHEN    25
                   MOVE    TNSU-SDOKNRYO       TO  PRF-VAL-USR
                   MOVE    TNSM-SDOKNRYO       TO  PRF-VAL-STD
               WHEN    26
                   MOVE    TNSU-JITUDAY        TO  PRF-VAL-USR
                   MOVE    TNSM-JITUDAY        TO  PRF-VAL-STD
               WHEN    27
                   MOVE    TNSU-DAYCNT         TO  PRF-VAL-USR
                   MOVE    TNSM-DAYCNT         TO  PRF-VAL-STD
               WHEN    28
                   MOVE    TNSU-YKZKNRKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-YKZKNRKBN      TO  PRF-VAL-STD
               WHEN    29
                   MOVE    TNSU-KZMCOMPSIKIBETU
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-KZMCOMPSIKIBETU
                                               TO  PRF-VAL-STD
               WHEN    30
                   MOVE    TNSU-KZMKGN         TO  PRF-VAL-USR
                   MOVE    TNSM-KZMKGN         TO  PRF-VAL-STD
               WHEN    31
                   MOVE    TNSU-KZMJGN         TO  PRF-VAL-USR
                   MOVE    TNSM-KZMJGN         TO  PRF-VAL-STD
               WHEN    32
                   MOVE    TNSU-KZM            TO  PRF-VAL-USR
                   MOVE    TNSM-KZM            TO  PRF-VAL-STD
               WHEN    33
                   MOVE    1                   TO  FLG-DECIMAL
                   MOVE    TNSU-KZMTEN         TO  WRK-PRF-USR-R
                   MOVE    TNSM-KZMTEN         TO  WRK-PRF-STD-R
               WHEN    34
                   MOVE    TNSU-KZMERR         TO  PRF-VAL-USR
                   MOVE    TNSM-KZMERR         TO  PRF-VAL-STD
               WHEN    35
                   MOVE    TNSU-JGNCNT         TO  PRF-VAL-USR
                   MOVE    TNSM-JGNCNT         TO  PRF-VAL-STD
               WHEN    36
                   MOVE    TNSU-JGNCNT1D       TO  PRF-VAL-USR
                   MOVE    TNSM-JGNCNT1D       TO  PRF-VAL-STD
               WHEN    37
                   MOVE    TNSU-JGNCNT1W       TO  PRF-VAL-USR
                   MOVE    TNSM-JGNCNT1W       TO  PRF-VAL-STD
               WHEN    38
                   MOVE    TNSU-JGNCNTETCM     TO  PRF-VAL-USR
                   MOVE    TNSM-JGNCNTETCM     TO  PRF-VAL-STD
               WHEN    39
                   MOVE    TNSU-JGNCNTETC      TO  PRF-VAL-USR
                   MOVE    TNSM-JGNCNTETC      TO  PRF-VAL-STD
               WHEN    40
                   MOVE    TNSU-JGNCNTERR      TO  PRF-VAL-USR
                   MOVE    TNSM-JGNCNTERR      TO  PRF-VAL-STD
               WHEN    41
                   MOVE    TNSU-CHUKSNCD       TO  PRF-VAL-USR
                   MOVE    TNSM-CHUKSNCD       TO  PRF-VAL-STD
               WHEN    42
                   MOVE    TNSU-CHUKSNTUBAN    TO  PRF-VAL-USR
                   MOVE    TNSM-CHUKSNTUBAN    TO  PRF-VAL-STD
               WHEN    43
                   MOVE    TNSU-TSUSOKUAGE     TO  PRF-VAL-USR
                   MOVE    TNSM-TSUSOKUAGE     TO  PRF-VAL-STD
               WHEN    44
                   MOVE    TNSU-KGNAGE         TO  PRF-VAL-USR
                   MOVE    TNSM-KGNAGE         TO  PRF-VAL-STD
               WHEN    45
                   MOVE    TNSU-JGNAGE         TO  PRF-VAL-USR
                   MOVE    TNSM-JGNAGE         TO  PRF-VAL-STD
               WHEN    46
                   MOVE    TNSU-TIMEKSNKBN     TO  PRF-VAL-USR
                   MOVE    TNSM-TIMEKSNKBN     TO  PRF-VAL-STD
               WHEN    47
                   MOVE    TNSU-KIJUNTEIGENKBN TO  PRF-VAL-USR
                   MOVE    TNSM-KIJUNTEIGENKBN TO  PRF-VAL-STD
               WHEN    48
                   MOVE    TNSU-KIJUNTEIGENCD  TO  PRF-VAL-USR
                   MOVE    TNSM-KIJUNTEIGENCD  TO  PRF-VAL-STD
               WHEN    49
                   MOVE    TNSU-LASERKBN       TO  PRF-VAL-USR
                   MOVE    TNSM-LASERKBN       TO  PRF-VAL-STD
               WHEN    50
                   MOVE    TNSU-CHPMESUKSN     TO  PRF-VAL-USR
                   MOVE    TNSM-CHPMESUKSN     TO  PRF-VAL-STD
               WHEN    51
                   MOVE    TNSU-MICMESUKSN     TO  PRF-VAL-USR
                   MOVE    TNSM-MICMESUKSN     TO  PRF-VAL-STD
               WHEN    52
                   MOVE    TNSU-DONORKBN       TO  PRF-VAL-USR
                   MOVE    TNSM-DONORKBN       TO  PRF-VAL-STD
               WHEN    53
                   MOVE    TNSU-KNSJISKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-KNSJISKBN      TO  PRF-VAL-STD
               WHEN    54
                   MOVE    TNSU-KNSJISGRPKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-KNSJISGRPKBN   TO  PRF-VAL-STD
               WHEN    55
                   MOVE    TNSU-TEIGENKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-TEIGENKBN      TO  PRF-VAL-STD
               WHEN    56
                   MOVE    TNSU-SEKIZUISOKUTEI TO  PRF-VAL-USR
                   MOVE    TNSM-SEKIZUISOKUTEI TO  PRF-VAL-STD
               WHEN    57
                   MOVE    TNSU-KEIBUJYUTU     TO  PRF-VAL-USR
                   MOVE    TNSM-KEIBUJYUTU     TO  PRF-VAL-STD
               WHEN    58
                   MOVE    TNSU-AUTOHOUGO      TO  PRF-VAL-USR
                   MOVE    TNSM-AUTOHOUGO      TO  PRF-VAL-STD
               WHEN    59
                   MOVE    TNSU-GAIKANRIKBN    TO  PRF-VAL-USR
                   MOVE    TNSM-GAIKANRIKBN    TO  PRF-VAL-STD
               WHEN    60
                   MOVE    TNSU-TUSOKUGAIKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-TUSOKUGAIKBN   TO  PRF-VAL-STD
               WHEN    61
                   MOVE    TNSU-HOUKATUTEIGENKBN
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-HOUKATUTEIGENKBN
                                               TO  PRF-VAL-STD
               WHEN    62
                   MOVE    TNSU-CHPNAISIKSN    TO  PRF-VAL-USR
                   MOVE    TNSM-CHPNAISIKSN    TO  PRF-VAL-STD
               WHEN    63
                   MOVE    TNSU-AUTOFUNGO      TO  PRF-VAL-USR
                   MOVE    TNSM-AUTOFUNGO      TO  PRF-VAL-STD
               WHEN    64
                   MOVE    TNSU-CHIIKIKSN      TO  PRF-VAL-USR
                   MOVE    TNSM-CHIIKIKSN      TO  PRF-VAL-STD
               WHEN    65
                   MOVE    TNSU-BYOSYOKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-BYOSYOKBN      TO  PRF-VAL-STD
               WHEN    66
                   MOVE    TNSU-CHPGYOKOSOTIKSN
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-CHPGYOKOSOTIKSN
                                               TO  PRF-VAL-STD
               WHEN    67
                   MOVE    TNSU-SHORTSTAYOPE   TO  PRF-VAL-USR
                   MOVE    TNSM-SHORTSTAYOPE   TO  PRF-VAL-STD
               WHEN    68
                   MOVE    TNSU-BUIKBN         TO  PRF-VAL-USR
                   MOVE    TNSM-BUIKBN         TO  PRF-VAL-STD
               WHEN    69
                   MOVE    TNSU-SANTEIRRKKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-SANTEIRRKKBN   TO  PRF-VAL-STD
               WHEN    70 THRU 79
                   COMPUTE IDY                 =   IDX - 69
                   MOVE    TNSU-SSTKIJUNCD(IDY)
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-SSTKIJUNCD(IDY)
                                               TO  PRF-VAL-STD
               WHEN    80
               WHEN    83
               WHEN    86
               WHEN    89
                   COMPUTE IDY                 =   (IDX - 80)
                                               /   3 + 1
                   MOVE    TNSU-AGEKSNKGN(IDY) TO  PRF-VAL-USR
                   MOVE    TNSM-AGEKSNKGN(IDY) TO  PRF-VAL-STD
               WHEN    81
               WHEN    84
               WHEN    87
               WHEN    90
                   COMPUTE IDY                 =   (IDX - 81)
                                               /   3 + 1
                   MOVE    TNSU-AGEKSNJGN(IDY) TO  PRF-VAL-USR
                   MOVE    TNSM-AGEKSNJGN(IDY) TO  PRF-VAL-STD
               WHEN    82
               WHEN    85
               WHEN    88
               WHEN    91
                   COMPUTE IDY                 =   (IDX - 82)
                                               /   3 + 1
                   MOVE    TNSU-AGEKSNSRYCD(IDY)
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-AGEKSNSRYCD(IDY)
                                               TO  PRF-VAL-STD
               WHEN    92
                   MOVE    TNSU-KENTAICOMMENT  TO  PRF-VAL-USR
                   MOVE    TNSM-KENTAICOMMENT  TO  PRF-VAL-STD
               WHEN    93
                   MOVE    TNSU-NYUKHNKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-NYUKHNKBN      TO  PRF-VAL-STD
               WHEN    94
                   MOVE    TNSU-NYUKHNKSNKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-NYUKHNKSNKBN   TO  PRF-VAL-STD
               WHEN    95
                   MOVE    TNSU-KANGOKSN       TO  PRF-VAL-USR
                   MOVE    TNSM-KANGOKSN       TO  PRF-VAL-STD
               WHEN    96
                   MOVE    TNSU-OLDTENSKB      TO  PRF-VAL-USR
                   MOVE    TNSM-OLDTENSKB      TO  PRF-VAL-STD
               WHEN    97
                   MOVE    1                   TO  FLG-DECIMAL
                   MOVE    TNSU-OLDTEN         TO  WRK-PRF-USR-R
                   MOVE    TNSM-OLDTEN         TO  WRK-PRF-STD-R
               WHEN    98
                   MOVE    TNSU-MADOKUKBN      TO  PRF-VAL-USR
                   MOVE    TNSM-MADOKUKBN      TO  PRF-VAL-STD
               WHEN    99
                   MOVE    TNSU-SINKEIHAKAIKBN TO  PRF-VAL-USR
                   MOVE    TNSM-SINKEIHAKAIKBN TO  PRF-VAL-STD
               WHEN    100
                   MOVE    TNSU-SEIBUTUGAKUKBN TO  PRF-VAL-USR
                   MOVE    TNSM-SEIBUTUGAKUKBN TO  PRF-VAL-STD
               WHEN    101
                   MOVE    TNSU-ZOEIZAIKBN     TO  PRF-VAL-USR
                   MOVE    TNSM-ZOEIZAIKBN     TO  PRF-VAL-STD
               WHEN    102
                   MOVE    TNSU-CSYYOURYO      TO  PRF-VAL-USR
                   MOVE    TNSM-CSYYOURYO      TO  PRF-VAL-STD
               WHEN    103
                   MOVE    TNSU-YKZKBN         TO  PRF-VAL-USR
                   MOVE    TNSM-YKZKBN         TO  PRF-VAL-STD
               WHEN    104
                   MOVE    TNSU-ZAIGATAKBN     TO  PRF-VAL-USR
                   MOVE    TNSM-ZAIGATAKBN     TO  PRF-VAL-STD
               WHEN    105
                   MOVE    TNSU-KOUHATUKBN     TO  PRF-VAL-USR
                   MOVE    TNSM-KOUHATUKBN     TO  PRF-VAL-STD
               WHEN    106
                   MOVE    TNSU-LONGTOYOKBN    TO  PRF-VAL-USR
                   MOVE    TNSM-LONGTOYOKBN    TO  PRF-VAL-STD
               WHEN    107
                   MOVE    TNSU-MEIUSESKB      TO  PRF-VAL-USR
                   MOVE    TNSM-MEIUSESKB      TO  PRF-VAL-STD
               WHEN    108
                   MOVE    TNSU-TOKUKIZAIAGEKSNKBN
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-TOKUKIZAIAGEKSNKBN
                                               TO  PRF-VAL-STD
               WHEN    109
                   MOVE    TNSU-SANSOKBN       TO  PRF-VAL-USR
                   MOVE    TNSM-SANSOKBN       TO  PRF-VAL-STD
               WHEN    110
                   MOVE    TNSU-TOKUKIZAISBT1  TO  PRF-VAL-USR
                   MOVE    TNSM-TOKUKIZAISBT1  TO  PRF-VAL-STD
               WHEN    111
                   MOVE    TNSU-TOKUKIZAISBT2  TO  PRF-VAL-USR
                   MOVE    TNSM-TOKUKIZAISBT2  TO  PRF-VAL-STD
               WHEN    112
                   MOVE    TNSU-JGNTEN         TO  PRF-VAL-USR
                   MOVE    TNSM-JGNTEN         TO  PRF-VAL-STD
               WHEN    113
                   MOVE    TNSU-GAITENTTLKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-GAITENTTLKBN   TO  PRF-VAL-STD
               WHEN    114
                   MOVE    TNSU-NYUTENTTLKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-NYUTENTTLKBN   TO  PRF-VAL-STD
               WHEN    115
                   MOVE    TNSU-CDKBN-KBN      TO  PRF-VAL-USR
                   MOVE    TNSM-CDKBN-KBN      TO  PRF-VAL-STD
               WHEN    116
                   MOVE    TNSU-CDKBN-SYO      TO  PRF-VAL-USR
                   MOVE    TNSM-CDKBN-SYO      TO  PRF-VAL-STD
               WHEN    117
                   MOVE    TNSU-CDKBN-BU       TO  PRF-VAL-USR
                   MOVE    TNSM-CDKBN-BU       TO  PRF-VAL-STD
               WHEN    118
                   MOVE    TNSU-CDKBN-KBNNUM   TO  PRF-VAL-USR
                   MOVE    TNSM-CDKBN-KBNNUM   TO  PRF-VAL-STD
               WHEN    119
                   MOVE    TNSU-CDKBN-KBNNUM-EDA
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-CDKBN-KBNNUM-EDA
                                               TO  PRF-VAL-STD
               WHEN    120
                   MOVE    TNSU-CDKBN-KOUBAN   TO  PRF-VAL-USR
                   MOVE    TNSM-CDKBN-KOUBAN   TO  PRF-VAL-STD
               WHEN    121
                   MOVE    TNSU-KOKUCHI-KBN    TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUCHI-KBN    TO  PRF-VAL-STD
               WHEN    122
                   MOVE    TNSU-KOKUCHI-SYO    TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUCHI-SYO    TO  PRF-VAL-STD
               WHEN    123
                   MOVE    TNSU-KOKUCHI-BU     TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUCHI-BU     TO  PRF-VAL-STD
               WHEN    124
                   MOVE    TNSU-KOKUCHI-KBNNUM TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUCHI-KBNNUM TO  PRF-VAL-STD
               WHEN    125
                   MOVE    TNSU-KOKUCHI-KBNNUM-EDA
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUCHI-KBNNUM-EDA
                                               TO  PRF-VAL-STD
               WHEN    126
                   MOVE    TNSU-KOKUCHI-KOUBAN TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUCHI-KOUBAN TO  PRF-VAL-STD
               WHEN    127
                   MOVE    TNSU-KOKUJISKBKBN1  TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUJISKBKBN1  TO  PRF-VAL-STD
               WHEN    128
                   MOVE    TNSU-KOKUJISKBKBN2  TO  PRF-VAL-USR
                   MOVE    TNSM-KOKUJISKBKBN2  TO  PRF-VAL-STD
               WHEN    129
                   MOVE    TNSU-NAMECHGKBN     TO  PRF-VAL-USR
                   MOVE    TNSM-NAMECHGKBN     TO  PRF-VAL-STD
               WHEN    130
                   MOVE    TNSU-KANANAMECHGKBN TO  PRF-VAL-USR
                   MOVE    TNSM-KANANAMECHGKBN TO  PRF-VAL-STD
               WHEN    131
                   MOVE    TNSU-IDOKANREN      TO  PRF-VAL-USR
                   MOVE    TNSM-IDOKANREN      TO  PRF-VAL-STD
               WHEN    132
                   MOVE    TNSU-KOUHYOJYUNNUM  TO  PRF-VAL-USR
                   MOVE    TNSM-KOUHYOJYUNNUM  TO  PRF-VAL-STD
               WHEN    133
                   MOVE    TNSU-YAKKAKJNCD     TO  PRF-VAL-USR
                   MOVE    TNSM-YAKKAKJNCD     TO  PRF-VAL-STD
               WHEN    134
                   MOVE    TNSU-SYUSAISKB      TO  PRF-VAL-USR
                   MOVE    TNSM-SYUSAISKB      TO  PRF-VAL-STD
               WHEN    135
                   MOVE    TNSU-SYOMEIKANREN   TO  PRF-VAL-USR
                   MOVE    TNSM-SYOMEIKANREN   TO  PRF-VAL-STD
               WHEN    136
                   MOVE    TNSU-CHGYMD         TO  PRF-VAL-USR
                   MOVE    TNSM-CHGYMD         TO  PRF-VAL-STD
               WHEN    137
                   MOVE    TNSU-HAISIYMD       TO  PRF-VAL-USR
                   MOVE    TNSM-HAISIYMD       TO  PRF-VAL-STD
               WHEN    138
                   MOVE    TNSU-KEIKASOCHIYMD  TO  PRF-VAL-USR
                   MOVE    TNSM-KEIKASOCHIYMD  TO  PRF-VAL-STD
      *    139 端末ＩＤ
      *    140 オペレーターＩＤ
      *    141 作成年月日
      *    142 更新年月日
      *    143 更新時間
      *    144 医療機関識別番号
               WHEN    145
                   MOVE    TNSU-GAZOOPESUP     TO  PRF-VAL-USR
                   MOVE    TNSM-GAZOOPESUP     TO  PRF-VAL-STD
               WHEN    146
                   MOVE    TNSU-IRYOKANSATUKBN TO  PRF-VAL-USR
                   MOVE    TNSM-IRYOKANSATUKBN TO  PRF-VAL-STD
               WHEN    147
                   MOVE    TNSU-MASUISKBKBN    TO  PRF-VAL-USR
                   MOVE    TNSM-MASUISKBKBN    TO  PRF-VAL-STD
      *H24.4追加４つ
               WHEN    148
                   MOVE    TNSU-FUKUBIKUNAIKBN TO  PRF-VAL-USR
                   MOVE    TNSM-FUKUBIKUNAIKBN TO  PRF-VAL-STD
               WHEN    149
                   MOVE    TNSU-FUKUBIKUKOTUKBN
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-FUKUBIKUKOTUKBN
                                               TO  PRF-VAL-STD
               WHEN    150
                   MOVE    TNSU-TIMEMASUIKBN   TO  PRF-VAL-USR
                   MOVE    TNSM-TIMEMASUIKBN   TO  PRF-VAL-STD
               WHEN    151
                   MOVE    TNSU-DPCKBN         TO  PRF-VAL-USR
                   MOVE    TNSM-DPCKBN         TO  PRF-VAL-STD
      *H28.4追加５つ
               WHEN    152
                   MOVE    TNSU-HISINSYUMONITORKSN
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-HISINSYUMONITORKSN
                                               TO  PRF-VAL-STD
               WHEN    153
                   MOVE    TNSU-TOUKETUHOZONKSN
                                               TO  PRF-VAL-USR
                   MOVE    TNSM-TOUKETUHOZONKSN
                                               TO  PRF-VAL-STD
               WHEN    154
                   MOVE    TNSU-KUBUNBANGOU    TO  PRF-VAL-USR
                   MOVE    TNSM-KUBUNBANGOU    TO  PRF-VAL-STD
               WHEN    155
                   MOVE    TNSU-ROSAIKBN       TO  PRF-VAL-USR
                   MOVE    TNSM-ROSAIKBN       TO  PRF-VAL-STD
               WHEN    156
                   MOVE    TNSU-SISIKSN        TO  PRF-VAL-USR
                   MOVE    TNSM-SISIKSN        TO  PRF-VAL-STD
      *H30.4追加１つ
               WHEN    157
                   MOVE    TNSU-AKUSEIBYORIKSN TO  PRF-VAL-USR
                   MOVE    TNSM-AKUSEIBYORIKSN TO  PRF-VAL-STD
      *R02.4追加３つ
               WHEN    158
                   MOVE    TNSU-SOGAIKOTEIKSN  TO  PRF-VAL-USR
                   MOVE    TNSM-SOGAIKOTEIKSN  TO  PRF-VAL-STD
               WHEN    159
                   MOVE    TNSU-CHPSESSAKUKSN  TO  PRF-VAL-USR
                   MOVE    TNSM-CHPSESSAKUKSN  TO  PRF-VAL-STD
               WHEN    160
                   MOVE    TNSU-SASINJIHEISIKBN TO  PRF-VAL-USR
                   MOVE    TNSM-SASINJIHEISIKBN TO  PRF-VAL-STD
           END-EVALUATE
      *
           MOVE    PRF-ITEM            TO  WRK-CSV-ITEM
           MOVE    PRF-VAL-USR         TO  WRK-CSV-VAL-USR
           MOVE    PRF-VAL-STD         TO  WRK-CSV-VAL-STD
           .
       2902-PRF-SET-VALUE-EXT.
           EXIT.
      *****************************************************************
      *    見出し編集処理
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
      *    対象なし印刷処理
      *****************************************************************
       2904-PRF-NOTHING-SEC        SECTION.
           PERFORM 2903-PRF-HEADER-SEC
           ADD     3                   TO  CNT-LINE
           MOVE    "　　＝＝＝　点数ユーザーマスタ登録なし　＝＝＝"
                                       TO  HCMSL55-MOJI (CNT-LINE)
           PERFORM 2909-PRINT-SEC
           .
       2904-PRF-NOTHING-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
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
           MOVE    "点数ユーザーマスタチェックリスト"
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
               MOVE    "印刷ＤＢが更新できませんでした"
                                       TO  WRK-BUMCPERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2909-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
           IF      CNT-LINE            >   6
               PERFORM 2909-PRINT-SEC
           END-IF
      *
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "chklist"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      CNT-TENSUU          =   ZERO
               PERFORM 2904-PRF-NOTHING-SEC
               MOVE    "処理対象のデータがありませんでした"
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
      *    エラー出力処理
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
      *        ジョブ管理開始処理
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
      *    ＣＳＶデータ処理
      *****************************************************************
       600-CSV-SYORI-SEC           SECTION.
           IF      CNT-CSV             =   ZERO
               PERFORM 6001-CSV-HEAD-SEC
           END-IF
           INITIALIZE  WRK-CSV-AREA
      *    フラグ
           MOVE    PRF-FLG             TO  WRK-IN-DATA
           MOVE    2                   TO  WRK-IN-MAX
           PERFORM 6002-MOJI-HENSYU-SEC
      *    コード
           MOVE    PRF-SRYCD           TO  WRK-IN-DATA
           MOVE    9                   TO  WRK-IN-MAX
           PERFORM 6002-MOJI-HENSYU-SEC
      *    開始日
      *    MOVE    PRF-YUKOSTYMD       TO  WRK-IN-DATA
           MOVE    TNSU-YUKOSTYMD      TO  WRK-IN-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 6002-MOJI-HENSYU-SEC
      *    終了日
      *    MOVE    PRF-YUKOEDYMD       TO  WRK-IN-DATA
           MOVE    TNSU-YUKOEDYMD      TO  WRK-IN-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 6002-MOJI-HENSYU-SEC
      *    標準開始日
           MOVE    TNSM-YUKOSTYMD      TO  WRK-IN-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 6002-MOJI-HENSYU-SEC
      *    標準終了日
           MOVE    TNSM-YUKOEDYMD      TO  WRK-IN-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 6002-MOJI-HENSYU-SEC
      *    名称
           MOVE    PRF-NAME            TO  WRK-IN-DATA
           MOVE    200                 TO  WRK-IN-MAX
           PERFORM 6002-MOJI-HENSYU-SEC
           IF      FLG-SYORI           =   2
      *        項目名
               MOVE    WRK-CSV-ITEM        TO  WRK-IN-DATA
               MOVE    200                 TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        ユーザー設定
               MOVE    WRK-CSV-VAL-USR     TO  WRK-IN-DATA
               MOVE    200                 TO  WRK-IN-MAX
               PERFORM 6002-MOJI-HENSYU-SEC
      *        標準設定
               MOVE    WRK-CSV-VAL-STD     TO  WRK-IN-DATA
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
      *    統計ＣＳＶ出力処理
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
      *    出力レコード処理（文字）
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
      *    統計ＣＳＶ出力処理
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
           MOVE    "点数ユーザーマスタチェックリストＣＳＶ作成"
                                       TO  STOUKEICSV-TITLE
           CALL    "ORCSTOUKEICSV" USING   ORCSTOUKEICSVAREA
                                           SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               ADD     1                   TO  CNT-CSV
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした。"
                                       TO  WRK-BUMCPERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       6009-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    小数点処理
      *****************************************************************
       800-DECIMAL-SEC             SECTION.
           MOVE    WRK-PRF-STD-R       TO  WRK-DECIMAL
           IF      WRK-DECIMAL-DEC     =   ZERO
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    FUNCTION TRIM (WRK-ZERO-SUP)
                                           TO  PRF-VAL-STD
           ELSE
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    WRK-ZERO-SUP        TO  WRK-PRF-INT
               MOVE    WRK-DECIMAL-DEC     TO  WRK-ZERO-SUP-N
               MOVE    WRK-ZERO-SUP (4:2)  TO  WRK-PRF-DEC
               MOVE    "."                 TO  WRK-PRF-POINT
               MOVE    FUNCTION TRIM (WRK-PRF-DECIMAL)
                                           TO  PRF-VAL-STD
           END-IF
           MOVE    WRK-PRF-USR-R       TO  WRK-DECIMAL
           IF      WRK-DECIMAL-DEC     =   ZERO
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    FUNCTION TRIM (WRK-ZERO-SUP)
                                           TO  PRF-VAL-USR
           ELSE
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    WRK-ZERO-SUP        TO  WRK-PRF-INT
               MOVE    WRK-DECIMAL-DEC     TO  WRK-ZERO-SUP-N
               MOVE    WRK-ZERO-SUP (4:2)  TO  WRK-PRF-DEC
               MOVE    "."                 TO  WRK-PRF-POINT
               MOVE    FUNCTION TRIM (WRK-PRF-DECIMAL)
                                           TO  PRF-VAL-USR
           END-IF
           MOVE    PRF-VAL-USR         TO  WRK-CSV-VAL-USR
           MOVE    PRF-VAL-STD         TO  WRK-CSV-VAL-STD
           .
       800-DECIMAL-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦変換処理
      *****************************************************************
       800-YMDCONV21-SEC           SECTION.
           INITIALIZE  STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  ZERO 
               WHEN  "99999999"
                   MOVE    WRK-LNKYMD          TO  WRK-HENYMDG
               WHEN  OTHER
                   INITIALIZE  STS-AREA-DAY
                   INITIALIZE  LNK-DAY2-AREA
                   MOVE  "21"                  TO  LNK-DAY2-IRAI
                   MOVE  WRK-LNKYMD(1:8)       TO  LNK-DAY2-YMD
                   CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
           .
       800-YMDCONV21-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
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
      *    点数ユーザーテーブル読込
      *****************************************************************
       900-TENSUU-SELECT-SEC       SECTION.
           INITIALIZE  TNSU-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNSU-HOSPNUM
           MOVE    TNSU-TENSU-REC      TO  MCPDATA-REC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "chklist"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TENSUU-FETCH-SEC
           END-IF
           .
       900-TENSUU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    点数ユーザーテーブル読込
      *****************************************************************
       900-TENSUU-FETCH-SEC        SECTION.
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "chklist"               TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNSU-TENSU-REC
               ADD     1                   TO  CNT-TENSUU
               MOVE    ZERO                TO  FLG-END
               IF      CNT-TENSUU (4:3)    =   "000"
                   MOVE    CNT-TENSUU          TO  WRK-MSG-CNT
                   DISPLAY "*(ORCBUMCTENSU)* org    CNT [ "
                           WRK-MSG-CNT " ]"
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-TENSUU-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    点数テーブル読込
      *****************************************************************
       900-TENSUM-SELECT-SEC       SECTION.
           INITIALIZE  TNSM-TENSU-REC
           MOVE    TNSU-HOSPNUM        TO  TNSM-HOSPNUM
           MOVE    TNSU-SRYCD          TO  TNSM-SRYCD
           MOVE    TNSM-TENSU-REC      TO  MCPDATA-REC
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TENSUM-FETCH-SEC
           END-IF
           .
       900-TENSUM-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    点数テーブル読込
      *****************************************************************
       900-TENSUM-FETCH-SEC        SECTION.
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNSM-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSUM
           ELSE
               MOVE    1                   TO  FLG-TENSUM
           END-IF
           .
       900-TENSUM-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読込
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
      *    テーブル検索処理
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
      *    レコード抽出処理
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
      *    カーソルクローズ処理
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
      *    プログラム終了処理
      *****************************************************************
       990-EXIT-PROGRAM-SEC        SECTION.
           DISPLAY "ORCBUMCTENSU RC [" BUMCP-RC "]"
           EXIT PROGRAM
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
