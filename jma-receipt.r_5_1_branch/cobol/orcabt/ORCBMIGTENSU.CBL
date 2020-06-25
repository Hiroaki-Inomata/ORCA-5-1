       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBMIGTENSU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : セットアップ処理
      *  コンポーネント名  : 点数マスタ更新
      *                    : 点数マスタ分離に伴いユーザーが訂正した内容
      *                    : についてはユーザー点数とする
      *                    : 特定器材のユーザーは設定する金額は点数金額
      *                    : テーブルへ追い出す
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/ 2/22    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    出力：プルーフファイル
           SELECT  PRF-FILE        ASSIGN
                                   ASS-PRF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    出力：プルーフファイル
       FD  PRF-FILE.
       01  PRF-REC                     PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-PRF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PRF                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PRF                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSU-USER          PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-GUID                PIC 9(01).
           03  FLG-TEN2PRICE           PIC 9(01).
           03  FLG-PRICE               PIC 9(01).
           03  FLG-TOUSER              PIC 9(01).
      *        (0:標準 1:点数→ユーザー登録 2:標準→点数)
           03  FLG-DECIMAL             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCDLIST           PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDI                     PIC 9(04).
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDM                     PIC 9(04).
           03  IDO                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-LIMIT-PRF  PIC 9(05).
               05  WRK-PARA-FILE       PIC X(256).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-MSG                 PIC X(256).
           03  WRK-MSG-CNT             PIC ZZZ,ZZ9.
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-ARRAYNAME           PIC X(20).
           03  SYS-DATE.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-TABLE               PIC X(64).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-LIMIT-PRF           PIC 9(05).
           03  WRK-MCNT                PIC 9(04).
           03  WRK-OCNT                PIC 9(04).
           03  WRK-PRF-MST-R           PIC 9(08)V9(05).
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
           03  WRK-YUKOSTYMD-COMP      PIC X(08).
      *
       01  MST-TENSU-AREA.
           03  MST-TENSU-OCC           OCCURS  30.
               05  MST-TENSU-BODY      PIC X(1500).
      *
       01  ORG-TENSU-AREA.
           03  ORG-TENSU-OCC           OCCURS  30.
               05  ORG-TENSU-BODY      PIC X(1500).
      *
       01  WRK-TENSU-PRICE-AREA.
           03  WRK-TENSU-PRICE-OCC     OCCURS  30.
               05  WRK-TENSU-PRICE-BODY    PIC X(150).
      *
       01  TBL-ERR-AREA.
           03  TBL-ERR                 PIC 9(01)   OCCURS  160.
       01  CONST-AREA.
           03  COL-START               PIC 9(03) VALUE  4.
           03  LIMIT-MSG.
               05  FILLER              PIC X(42) VALUE
                    "★　プルーフ件数がリミッターに到達しました".
               05  FILLER              PIC X(16) VALUE
                    "ので終了します。".
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(25) VALUE
                                       "Ver5.1 点数マスタ移行処理".
               05  FILLER              PIC X(14) VALUE
                                       "(ORCBMIGTENSU)".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(06) VALUE
                                       "処理日".
               05  FILLER              PIC X(01) VALUE   ",".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ",".
               05  MES-TITLE1-HOSPNUM  PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ",".
               05  MES-TITLE1-HOSPNAME PIC X(120) VALUE SPACE.
      *
           03  MES-TITLE2.
               05  FILLER              PIC X(04) VALUE   "結果".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(10) VALUE   "診療コード".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(06) VALUE   "開始日".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(06) VALUE   "終了日".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(04) VALUE   "名称".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(04) VALUE   "内容".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(04) VALUE   "項目".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(10) VALUE   "オリジナル".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(06) VALUE   "ユーザ".
               05  FILLER              PIC X(01) VALUE   ",".
               05  FILLER              PIC X(06) VALUE   "更新日".
      *
       01  WRK-PRF-AREA.
           03  WRK-PRF-ANS             PIC X(100).
           03  WRK-PRF-SRYCD           PIC X(09).
           03  WRK-PRF-STYMD           PIC X(08).
           03  WRK-PRF-EDYMD           PIC X(08).
           03  WRK-PRF-NAME            PIC X(500).
           03  WRK-PRF-MSG.
               05  WRK-PRF-GUID        PIC X(100).
               05  WRK-PRF-ITM         PIC X(100).
               05  WRK-PRF-MST         PIC X(500).
               05  WRK-PRF-USR         PIC X(500).
      *
      *    項目名称テーブル
           COPY    "CPTENSUNAMETBL.INC".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
           COPY    "CPTENSU.INC"       REPLACING
                                       //TNS-// BY //MST-//
                                       //TNSUP-// BY //MSTUP-//
                                       //TNSXX-// BY //MSTXX-//.
           COPY    "CPTENSU.INC"       REPLACING
                                       //TNS-// BY //ORG-//
                                       //TNSUP-// BY //ORGUP-//
                                       //TNSXX-// BY //ORGXX-//.
      *
       01  TENSU-PRICE-REC.
           COPY    "CPTENSU-PRICE.INC".
      *
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      ****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
           PERFORM 300-END-SEC
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           DISPLAY "ORCBMIGTENSU: START"
           MOVE    ZERO                TO  FLG-AREA
           MOVE    ZERO                TO  CNT-AREA
           MOVE    ZERO                TO  IDX-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  SPA-AREA
           ACCEPT  SYS-DATE        FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY  +   2000
           MOVE    SYS-MM              TO  WRK-SMM
           MOVE    SYS-DD              TO  WRK-SDD
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
               DISPLAY "ORCBMIGTENSU: DATABASE OPEN ERROR"
               MOVE    1                   TO  FLG-END
           END-IF
      *
           IF      FLG-END             =   ZERO
               OPEN    OUTPUT  PRF-FILE
               IF      STS-PRF         NOT =   ZERO
                   DISPLAY "ORCBMIGTENSU: PROOF FILE OPEN ERROR "
                           "STATUS [ " STS-PRF " ]"
                   MOVE    1                   TO  FLG-END
               ELSE
                   MOVE    WRK-SYY             TO  MES-TITLE1-YY
                   MOVE    SYS-MM              TO  MES-TITLE1-MM
                   MOVE    SYS-DD              TO  MES-TITLE1-DD
                   MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
                   PERFORM 900-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    SYS-1001-HOSPNAME
                                               TO  MES-TITLE1-HOSPNAME
                       DISPLAY "ORCBMIGTENSU: [ "
                               FUNCTION TRIM(SYS-1001-HOSPNAME) " ]"
                       DISPLAY " "
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-SRYCDLIST-SELECT-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *D   DISPLAY "[ " WRK-SRYCD " ]"
      *    tbl_tensu_userに該当診療コードのレコードがある場合はスキップ
           PERFORM 900-TENSUUSER-READ-SEC
           IF      FLG-TENSU-USER      =   ZERO
               MOVE    1                   TO  FLG-GUID
               PERFORM 20102-PRF-OUT-SEC
               GO  TO  200-MAIN-90
           END-IF
      *
           MOVE    ZERO                TO  FLG-TOUSER
           MOVE    ZERO                TO  FLG-TEN2PRICE
           MOVE    ZERO                TO  FLG-PRICE
           PERFORM 9001-TENSU-SAVE-SEC
           PERFORM 9002-TENSU-SAVE-SEC
      *            9002-TENSU-SAVE-SEC で特定器材　金額を点数金額へ
      *            登録するマスタであるかのフラグを設定 FLG-TEN2PRICE
      *D   DISPLAY "org      [ " WRK-OCNT " ]"
      *D   DISPLAY "master   [ " WRK-MCNT " ]"
      *
           IF      WRK-SRYCD(1:1)  NOT =   "7"
      *        特定器材以外の処理
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL  (IDX         >   WRK-OCNT)   OR
                              (FLG-TOUSER  NOT =   ZERO)
                   MOVE    ORG-TENSU-BODY (IDX)
                                               TO  ORG-TENSU-REC
                   IF      IDX                 >   WRK-MCNT
                       MOVE    1                   TO  FLG-TOUSER
                       EXIT    PERFORM
                   END-IF
                   MOVE    MST-TENSU-BODY (IDX)
                                               TO  MST-TENSU-REC
                   PERFORM 2001-COMP-KIKAN-SEC
                   IF      FLG-TOUSER          =   1
                       EXIT    PERFORM
                   END-IF
                   PERFORM 2001-COMP-ITEM-SEC
review*            エラーはどの項目が不一致の場合とするかよく考えて見直す
      *            変更されていても問題ない項目は比較チェックから除く
                   IF      TBL-ERR-AREA        =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-TOUSER
                       MOVE    3                   TO  FLG-GUID
                       PERFORM 20102-PRF-SET-SEC
                   END-IF
               END-PERFORM
           ELSE
      *        特定器材処理
      *     IF     (WRK-SRYCD(1:1)      =   "7")  AND
      *            (FLG-TOUSER          =   1)
               PERFORM 2001-TOKUTEIKIZAI-SEC
      *     END-IF
           END-IF
           IF      FLG-TOUSER          =   1  OR  2
      *D       IF      FLG-TOUSER          =   1
      *            点数ユーザーテーブルに登録（ユーザー管理マスタ）
      *D           DISPLAY "[ " WRK-SRYCD " ] ユーザー管理マスタ決定！"
      *D                   " FLG-GUID [ " FLG-GUID " ]"
      *D       ELSE
      *D           DISPLAY "[ " WRK-SRYCD " ] 標準マスタ決定！"
      *D       END-IF
               PERFORM 2100-TENSU-USER-SEC
           END-IF
           .
       200-MAIN-90.
           PERFORM 900-SRYCDLIST-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    有効期間比較処理
      *****************************************************************
       2001-COMP-KIKAN-SEC         SECTION.
           IF     (ORG-YUKOSTYMD       =   MST-YUKOSTYMD)  AND
                  (ORG-YUKOEDYMD       =   MST-YUKOEDYMD)
      *        期間一致
               CONTINUE
           ELSE
      *        期間不一致
      *D       DISPLAY "有効期間 [ " ORG-YUKOSTYMD " / "
      *D                             MST-YUKOSTYMD " - "
      *D                             ORG-YUKOEDYMD " / "
      *D                             MST-YUKOEDYMD " ]"
               MOVE    1                   TO  FLG-TOUSER
               MOVE    2                   TO  FLG-GUID
               PERFORM 20102-PRF-OUT-SEC
           END-IF
           .
       2001-COMP-KIKAN-EXT.
           EXIT.
      *****************************************************************
      *    項目別比較処理
      *****************************************************************
       2001-COMP-ITEM-SEC          SECTION.
           MOVE    ZERO                TO  TBL-ERR-AREA
           IF      ORG-SRYKBN      NOT =   MST-SRYKBN
               MOVE    1                   TO  TBL-ERR (4)
           END-IF
           IF      ORG-SRYSYUKBN   NOT =   MST-SRYSYUKBN
               MOVE    1                   TO  TBL-ERR (5)
           END-IF
           IF      ORG-YUKOKETA    NOT =   MST-YUKOKETA
               MOVE    1                   TO  TBL-ERR (6)
           END-IF
           IF      ORG-NAME        NOT =   MST-NAME
               MOVE    1                   TO  TBL-ERR (7)
           END-IF
           IF      ORG-KANAYUKOKETA
                                   NOT =   MST-KANAYUKOKETA
               MOVE    1                   TO  TBL-ERR (8)
           END-IF
           IF      ORG-KANANAME    NOT =   MST-KANANAME
               MOVE    1                   TO  TBL-ERR (9)
           END-IF
           IF      ORG-FORMALYUKOKETA
                                   NOT =   MST-FORMALYUKOKETA
               MOVE    1                   TO  TBL-ERR (10)
           END-IF
           IF      ORG-FORMALNAME  NOT =   MST-FORMALNAME
               MOVE    1                   TO  TBL-ERR (11)
           END-IF
           IF      ORG-TENSIKIBETU NOT =   MST-TENSIKIBETU
               MOVE    1                   TO  TBL-ERR (12)
           END-IF
review*     IF     (ORG-TENSIKIBETU     =   2  OR  4) AND
      *            (ORG-TEN             =   0)
      *         CONTINUE
      *     ELSE
               IF      ORG-TEN         NOT =   MST-TEN
      *            クリーンインストールのダンプデータの誤りを救済
      *            眼底カメラ検査用インスタントフィルム 50円
                   IF     (ORG-SRYCD           =   "729780000")  AND
                          (ORG-TEN             =   50) AND
                          (MST-TEN             =   0)
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  TBL-ERR (13)
                   END-IF
               END-IF
      *     END-IF
           IF      ORG-TANICD      NOT =   MST-TANICD
               MOVE    1                   TO  TBL-ERR (14)
           END-IF
           IF      ORG-TANIMOJISU  NOT =   MST-TANIMOJISU
               MOVE    1                   TO  TBL-ERR (15)
           END-IF
           IF      ORG-TANINAME    NOT =   MST-TANINAME
               MOVE    1                   TO  TBL-ERR (16)
           END-IF
           IF      ORG-DATAKBN     NOT =   MST-DATAKBN
               MOVE    1                   TO  TBL-ERR (17)
           END-IF
           IF      ORG-HKNTEKKBN   NOT =   MST-HKNTEKKBN
               MOVE    1                   TO  TBL-ERR (18)
           END-IF
           IF      ORG-NYUGAITEKKBN
                                   NOT =   MST-NYUGAITEKKBN
               MOVE    1                   TO  TBL-ERR (19)
           END-IF
           IF      ORG-ROUTEKKBN   NOT =   MST-ROUTEKKBN
               MOVE    1                   TO  TBL-ERR (20)
           END-IF
           IF      ORG-HOSPSRYKBN  NOT =   MST-HOSPSRYKBN
               MOVE    1                   TO  TBL-ERR (21)
           END-IF
           IF      ORG-OSINKBN     NOT =   MST-OSINKBN
               MOVE    1                   TO  TBL-ERR (22)
           END-IF
           IF      ORG-HOUKNSKBN   NOT =   MST-HOUKNSKBN
               MOVE    1                   TO  TBL-ERR (23)
           END-IF
           IF      ORG-BYOKANRENKBN
                                   NOT =   MST-BYOKANRENKBN
               MOVE    1                   TO  TBL-ERR (24)
           END-IF
           IF      ORG-SDOKNRYO    NOT =   MST-SDOKNRYO
               MOVE    1                   TO  TBL-ERR (25)
           END-IF
           IF      ORG-JITUDAY     NOT =   MST-JITUDAY
               MOVE    1                   TO  TBL-ERR (26)
           END-IF
           IF      ORG-DAYCNT      NOT =   MST-DAYCNT
               MOVE    1                   TO  TBL-ERR (27)
           END-IF
           IF      ORG-YKZKNRKBN   NOT =   MST-YKZKNRKBN
               MOVE    1                   TO  TBL-ERR (28)
           END-IF
           IF      ORG-KZMCOMPSIKIBETU
                                   NOT =   MST-KZMCOMPSIKIBETU
               MOVE    1                   TO  TBL-ERR (29)
           END-IF
           IF      ORG-KZMKGN      NOT =   MST-KZMKGN
               MOVE    1                   TO  TBL-ERR (30)
           END-IF
           IF      ORG-KZMJGN      NOT =   MST-KZMJGN
               MOVE    1                   TO  TBL-ERR (31)
           END-IF
           IF      ORG-KZM         NOT =   MST-KZM
               MOVE    1                   TO  TBL-ERR (32)
           END-IF
           IF      ORG-KZMTEN      NOT =   MST-KZMTEN
               MOVE    1                   TO  TBL-ERR (33)
           END-IF
           IF      ORG-KZMERR      NOT =   MST-KZMERR
               MOVE    1                   TO  TBL-ERR (34)
           END-IF
           IF      ORG-JGNCNT      NOT =   MST-JGNCNT
               MOVE    1                   TO  TBL-ERR (35)
           END-IF
           IF      ORG-JGNCNT1D    NOT =   MST-JGNCNT1D
               MOVE    1                   TO  TBL-ERR (36)
           END-IF
           IF      ORG-JGNCNT1W    NOT =   MST-JGNCNT1W
               MOVE    1                   TO  TBL-ERR (37)
           END-IF
           IF      ORG-JGNCNTETCM  NOT =   MST-JGNCNTETCM
               MOVE    1                   TO  TBL-ERR (38)
           END-IF
           IF      ORG-JGNCNTETC   NOT =   MST-JGNCNTETC
               MOVE    1                   TO  TBL-ERR (39)
           END-IF
           IF      ORG-JGNCNTERR   NOT =   MST-JGNCNTERR
               MOVE    1                   TO  TBL-ERR (40)
           END-IF
           IF      ORG-CHUKSNCD    NOT =   MST-CHUKSNCD
               MOVE    1                   TO  TBL-ERR (41)
           END-IF
           IF      ORG-CHUKSNTUBAN NOT =   MST-CHUKSNTUBAN
               MOVE    1                   TO  TBL-ERR (42)
           END-IF
           IF      ORG-TSUSOKUAGE  NOT =   MST-TSUSOKUAGE
               MOVE    1                   TO  TBL-ERR (43)
           END-IF
           IF      ORG-KGNAGE      NOT =   MST-KGNAGE
               MOVE    1                   TO  TBL-ERR (44)
           END-IF
           IF      ORG-JGNAGE      NOT =   MST-JGNAGE
               MOVE    1                   TO  TBL-ERR (45)
           END-IF
           IF      ORG-TIMEKSNKBN  NOT =   MST-TIMEKSNKBN
               MOVE    1                   TO  TBL-ERR (46)
           END-IF
           IF      ORG-KIJUNTEIGENKBN
                                   NOT =   MST-KIJUNTEIGENKBN
               MOVE    1                   TO  TBL-ERR (47)
           END-IF
           IF      ORG-KIJUNTEIGENCD
                                   NOT =   MST-KIJUNTEIGENCD
               MOVE    1                   TO  TBL-ERR (48)
           END-IF
           IF      ORG-LASERKBN    NOT =   MST-LASERKBN
               MOVE    1                   TO  TBL-ERR (49)
           END-IF
           IF      ORG-CHPMESUKSN  NOT =   MST-CHPMESUKSN
               MOVE    1                   TO  TBL-ERR (50)
           END-IF
           IF      ORG-MICMESUKSN  NOT =   MST-MICMESUKSN
               MOVE    1                   TO  TBL-ERR (51)
           END-IF
           IF      ORG-DONORKBN    NOT =   MST-DONORKBN
               MOVE    1                   TO  TBL-ERR (52)
           END-IF
           IF      ORG-KNSJISKBN   NOT =   MST-KNSJISKBN
               MOVE    1                   TO  TBL-ERR (53)
           END-IF
           IF      ORG-KNSJISGRPKBN
                                   NOT =   MST-KNSJISGRPKBN
               MOVE    1                   TO  TBL-ERR (54)
           END-IF
           IF      ORG-TEIGENKBN   NOT =   MST-TEIGENKBN
               MOVE    1                   TO  TBL-ERR (55)
           END-IF
           IF      ORG-SEKIZUISOKUTEI
                                   NOT =   MST-SEKIZUISOKUTEI
               MOVE    1                   TO  TBL-ERR (56)
           END-IF
           IF      ORG-KEIBUJYUTU  NOT =   MST-KEIBUJYUTU
               MOVE    1                   TO  TBL-ERR (57)
           END-IF
           IF      ORG-AUTOHOUGO   NOT =   MST-AUTOHOUGO
               MOVE    1                   TO  TBL-ERR (58)
           END-IF
           IF      ORG-GAIKANRIKBN NOT =   MST-GAIKANRIKBN
               MOVE    1                   TO  TBL-ERR (59)
           END-IF
           IF      ORG-TUSOKUGAIKBN
                                   NOT =   MST-TUSOKUGAIKBN
               MOVE    1                   TO  TBL-ERR (60)
           END-IF
           IF      ORG-HOUKATUTEIGENKBN
                                   NOT =   MST-HOUKATUTEIGENKBN
               MOVE    1                   TO  TBL-ERR (61)
           END-IF
           IF      ORG-CHPNAISIKSN NOT =   MST-CHPNAISIKSN
               MOVE    1                   TO  TBL-ERR (62)
           END-IF
           IF      ORG-AUTOFUNGO   NOT =   MST-AUTOFUNGO
               MOVE    1                   TO  TBL-ERR (63)
           END-IF
           IF      ORG-CHIIKIKSN   NOT =   MST-CHIIKIKSN
               MOVE    1                   TO  TBL-ERR (64)
           END-IF
           IF      ORG-BYOSYOKBN   NOT =   MST-BYOSYOKBN
               MOVE    1                   TO  TBL-ERR (65)
           END-IF
           IF      ORG-CHPGYOKOSOTIKSN
                                   NOT =   MST-CHPGYOKOSOTIKSN
               MOVE    1                   TO  TBL-ERR (66)
           END-IF
           IF      ORG-SHORTSTAYOPE
                                   NOT =   MST-SHORTSTAYOPE
               MOVE    1                   TO  TBL-ERR (67)
           END-IF
           IF      ORG-BUIKBN      NOT =   MST-BUIKBN
               MOVE    1                   TO  TBL-ERR (68)
           END-IF
           IF      ORG-SANTEIRRKKBN
                                   NOT =   MST-SANTEIRRKKBN
               MOVE    1                   TO  TBL-ERR (69)
           END-IF
           IF      ORG-SSTKIJUNCD(1)
                                   NOT =   MST-SSTKIJUNCD(1)
               MOVE    1                   TO  TBL-ERR (70)
           END-IF
           IF      ORG-SSTKIJUNCD(2)
                                   NOT =   MST-SSTKIJUNCD(2)
               MOVE    1                   TO  TBL-ERR (71)
           END-IF
           IF      ORG-SSTKIJUNCD(3)
                                   NOT =   MST-SSTKIJUNCD(3)
               MOVE    1                   TO  TBL-ERR (72)
           END-IF
           IF      ORG-SSTKIJUNCD(4)
                                   NOT =   MST-SSTKIJUNCD(4)
               MOVE    1                   TO  TBL-ERR (73)
           END-IF
           IF      ORG-SSTKIJUNCD(5)
                                   NOT =   MST-SSTKIJUNCD(5)
               MOVE    1                   TO  TBL-ERR (74)
           END-IF
           IF      ORG-SSTKIJUNCD(6)
                                   NOT =   MST-SSTKIJUNCD(6)
               MOVE    1                   TO  TBL-ERR (75)
           END-IF
           IF      ORG-SSTKIJUNCD(7)
                                   NOT =   MST-SSTKIJUNCD(7)
               MOVE    1                   TO  TBL-ERR (76)
           END-IF
           IF      ORG-SSTKIJUNCD(8)
                                   NOT =   MST-SSTKIJUNCD(8)
               MOVE    1                   TO  TBL-ERR (77)
           END-IF
           IF      ORG-SSTKIJUNCD(9)
                                   NOT =   MST-SSTKIJUNCD(9)
               MOVE    1                   TO  TBL-ERR (78)
           END-IF
           IF      ORG-SSTKIJUNCD(10)
                                   NOT =   MST-SSTKIJUNCD(10)
               MOVE    1                   TO  TBL-ERR (79)
           END-IF
           IF      ORG-AGEKSNKGN(1)
                                   NOT =   MST-AGEKSNKGN(1)
               MOVE    1                   TO  TBL-ERR (80)
           END-IF
           IF      ORG-AGEKSNJGN(1)
                                   NOT =   MST-AGEKSNJGN(1)
               MOVE    1                   TO  TBL-ERR (81)
           END-IF
           IF      ORG-AGEKSNSRYCD(1)
                                   NOT =   MST-AGEKSNSRYCD(1)
               MOVE    1                   TO  TBL-ERR (82)
           END-IF
           IF      ORG-AGEKSNKGN(2)
                                   NOT =   MST-AGEKSNKGN(2)
               MOVE    1                   TO  TBL-ERR (83)
           END-IF
           IF      ORG-AGEKSNJGN(2)
                                   NOT =   MST-AGEKSNJGN(2)
               MOVE    1                   TO  TBL-ERR (84)
           END-IF
           IF      ORG-AGEKSNSRYCD(2)
                                   NOT =   MST-AGEKSNSRYCD(2)
               MOVE    1                   TO  TBL-ERR (85)
           END-IF
           IF      ORG-AGEKSNKGN(3)
                                   NOT =   MST-AGEKSNKGN(3)
               MOVE    1                   TO  TBL-ERR (86)
           END-IF
           IF      ORG-AGEKSNJGN(3)
                                   NOT =   MST-AGEKSNJGN(3)
               MOVE    1                   TO  TBL-ERR (87)
           END-IF
           IF      ORG-AGEKSNSRYCD(3)
                                   NOT =   MST-AGEKSNSRYCD(3)
               MOVE    1                   TO  TBL-ERR (88)
           END-IF
           IF      ORG-AGEKSNKGN(4)
                                   NOT =   MST-AGEKSNKGN(4)
               MOVE    1                   TO  TBL-ERR (89)
           END-IF
           IF      ORG-AGEKSNJGN(4)
                                   NOT =   MST-AGEKSNJGN(4)
               MOVE    1                   TO  TBL-ERR (90)
           END-IF
           IF      ORG-AGEKSNSRYCD(4)
                                   NOT =   MST-AGEKSNSRYCD(4)
               MOVE    1                   TO  TBL-ERR (91)
           END-IF
           IF      ORG-KENTAICOMMENT
                                   NOT =   MST-KENTAICOMMENT
               MOVE    1                   TO  TBL-ERR (92)
           END-IF
           IF      ORG-NYUKHNKBN   NOT =   MST-NYUKHNKBN
               MOVE    1                   TO  TBL-ERR (93)
           END-IF
           IF      ORG-NYUKHNKSNKBN
                                   NOT =   MST-NYUKHNKSNKBN
               MOVE    1                   TO  TBL-ERR (94)
           END-IF
           IF      ORG-KANGOKSN    NOT =   MST-KANGOKSN
               MOVE    1                   TO  TBL-ERR (95)
           END-IF
           IF      ORG-OLDTENSKB   NOT =   MST-OLDTENSKB
               MOVE    1                   TO  TBL-ERR (96)
           END-IF
review*    IF     (ORG-OLDTENSKB       =   2  OR  4) AND
      *           (ORG-OLDTEN          =   0)
      *        CONTINUE
      *    ELSE
               IF      ORG-OLDTEN      NOT =   MST-OLDTEN
                   MOVE    1                   TO  TBL-ERR (97)
               END-IF
      *    END-IF
           IF      ORG-MADOKUKBN   NOT =   MST-MADOKUKBN
               MOVE    1                   TO  TBL-ERR (98)
           END-IF
           IF      ORG-SINKEIHAKAIKBN
                                   NOT =   MST-SINKEIHAKAIKBN
               MOVE    1                   TO  TBL-ERR (99)
           END-IF
           IF      ORG-SEIBUTUGAKUKBN
                                   NOT =   MST-SEIBUTUGAKUKBN
               MOVE    1                   TO  TBL-ERR (100)
           END-IF
           IF      ORG-ZOEIZAIKBN  NOT =   MST-ZOEIZAIKBN
               MOVE    1                   TO  TBL-ERR (101)
           END-IF
           IF      ORG-CSYYOURYO   NOT =   MST-CSYYOURYO
               MOVE    1                   TO  TBL-ERR (102)
           END-IF
           IF      ORG-YKZKBN      NOT =   MST-YKZKBN
               MOVE    1                   TO  TBL-ERR (103)
           END-IF
           IF      ORG-ZAIGATAKBN  NOT =   MST-ZAIGATAKBN
               MOVE    1                   TO  TBL-ERR (104)
           END-IF
           IF      ORG-KOUHATUKBN  NOT =   MST-KOUHATUKBN
               MOVE    1                   TO  TBL-ERR (105)
           END-IF
           IF      ORG-LONGTOYOKBN NOT =   MST-LONGTOYOKBN
               MOVE    1                   TO  TBL-ERR (106)
           END-IF
           IF      ORG-MEIUSESKB   NOT =   MST-MEIUSESKB
               MOVE    1                   TO  TBL-ERR (107)
           END-IF
           IF      ORG-TOKUKIZAIAGEKSNKBN
                                   NOT =   MST-TOKUKIZAIAGEKSNKBN
               MOVE    1                   TO  TBL-ERR (108)
           END-IF
           IF      ORG-SANSOKBN    NOT =   MST-SANSOKBN
               MOVE    1                   TO  TBL-ERR (109)
           END-IF
           IF      ORG-TOKUKIZAISBT1
                                   NOT =   MST-TOKUKIZAISBT1
               MOVE    1                   TO  TBL-ERR (110)
           END-IF
           IF      ORG-TOKUKIZAISBT2
                                   NOT =   MST-TOKUKIZAISBT2
               MOVE    1                   TO  TBL-ERR (111)
           END-IF
           IF      ORG-JGNTEN      NOT =   MST-JGNTEN
               MOVE    1                   TO  TBL-ERR (112)
           END-IF
           IF      ORG-GAITENTTLKBN
                                   NOT =   MST-GAITENTTLKBN
               MOVE    1                   TO  TBL-ERR (113)
           END-IF
           IF      ORG-NYUTENTTLKBN
                                   NOT =   MST-NYUTENTTLKBN
               MOVE    1                   TO  TBL-ERR (114)
           END-IF
           IF      ORG-CDKBN-KBN   NOT =   MST-CDKBN-KBN
               MOVE    1                   TO  TBL-ERR (115)
           END-IF
           IF      ORG-CDKBN-SYO   NOT =   MST-CDKBN-SYO
               MOVE    1                   TO  TBL-ERR (116)
           END-IF
           IF      ORG-CDKBN-BU    NOT =   MST-CDKBN-BU
               MOVE    1                   TO  TBL-ERR (117)
           END-IF
           IF      ORG-CDKBN-KBNNUM
                                   NOT =   MST-CDKBN-KBNNUM
               MOVE    1                   TO  TBL-ERR (118)
           END-IF
           IF      ORG-CDKBN-KBNNUM-EDA
                                   NOT =   MST-CDKBN-KBNNUM-EDA
               MOVE    1                   TO  TBL-ERR (119)
           END-IF
           IF      ORG-CDKBN-KOUBAN
                                   NOT =   MST-CDKBN-KOUBAN
               MOVE    1                   TO  TBL-ERR (120)
           END-IF
           IF      ORG-KOKUCHI-KBN NOT =   MST-KOKUCHI-KBN
               MOVE    1                   TO  TBL-ERR (121)
           END-IF
           IF      ORG-KOKUCHI-SYO NOT =   MST-KOKUCHI-SYO
               MOVE    1                   TO  TBL-ERR (122)
           END-IF
           IF      ORG-KOKUCHI-BU  NOT =   MST-KOKUCHI-BU
               MOVE    1                   TO  TBL-ERR (123)
           END-IF
           IF      ORG-KOKUCHI-KBNNUM
                                   NOT =   MST-KOKUCHI-KBNNUM
               MOVE    1                   TO  TBL-ERR (124)
           END-IF
           IF      ORG-KOKUCHI-KBNNUM-EDA
                                   NOT =   MST-KOKUCHI-KBNNUM-EDA
               MOVE    1                   TO  TBL-ERR (125)
           END-IF
           IF      ORG-KOKUCHI-KOUBAN
                                   NOT =   MST-KOKUCHI-KOUBAN
               MOVE    1                   TO  TBL-ERR (126)
           END-IF
           IF      ORG-KOKUJISKBKBN1
                                   NOT =   MST-KOKUJISKBKBN1
               MOVE    1                   TO  TBL-ERR (127)
           END-IF
           IF      ORG-KOKUJISKBKBN2
                                   NOT =   MST-KOKUJISKBKBN2
               MOVE    1                   TO  TBL-ERR (128)
           END-IF
           IF      ORG-NAMECHGKBN  NOT =   MST-NAMECHGKBN
               MOVE    1                   TO  TBL-ERR (129)
           END-IF
           IF      ORG-KANANAMECHGKBN
                                   NOT =   MST-KANANAMECHGKBN
               MOVE    1                   TO  TBL-ERR (130)
           END-IF
           IF      ORG-IDOKANREN   NOT =   MST-IDOKANREN
               MOVE    1                   TO  TBL-ERR (131)
           END-IF
           IF      ORG-KOUHYOJYUNNUM
                                   NOT =   MST-KOUHYOJYUNNUM
               MOVE    1                   TO  TBL-ERR (132)
           END-IF
           IF      ORG-YAKKAKJNCD  NOT =   MST-YAKKAKJNCD
               MOVE    1                   TO  TBL-ERR (133)
           END-IF
           IF      ORG-SYUSAISKB   NOT =   MST-SYUSAISKB
               MOVE    1                   TO  TBL-ERR (134)
           END-IF
           IF      ORG-SYOMEIKANREN
                                   NOT =   MST-SYOMEIKANREN
               MOVE    1                   TO  TBL-ERR (135)
           END-IF
           IF      ORG-CHGYMD      NOT =   MST-CHGYMD
               MOVE    1                   TO  TBL-ERR (136)
           END-IF
           IF      ORG-HAISIYMD    NOT =   MST-HAISIYMD
               MOVE    1                   TO  TBL-ERR (137)
           END-IF
           IF      ORG-KEIKASOCHIYMD
                                   NOT =   MST-KEIKASOCHIYMD
               IF    ((MST-KEIKASOCHIYMD   =   SPACE) AND
                      (ORG-KEIKASOCHIYMD   =   "00000000"))  OR
                     ((MST-KEIKASOCHIYMD   =   "00000000") AND
                      (ORG-KEIKASOCHIYMD   =   SPACE))
                   CONTINUE
               ELSE
                   MOVE    1                   TO  TBL-ERR (138)
               END-IF
           END-IF
      *    139 端末ＩＤ
      *    140 オペレーターＩＤ
      *    141 作成年月日
      *    142 更新年月日
      *    143 更新時間
      *    144 医療機関識別番号
           IF      ORG-GAZOOPESUP  NOT =   MST-GAZOOPESUP
               MOVE    1                   TO  TBL-ERR (145)
           END-IF
           IF      ORG-IRYOKANSATUKBN
                                   NOT =   MST-IRYOKANSATUKBN
               MOVE    1                   TO  TBL-ERR (146)
           END-IF
           IF      ORG-MASUISKBKBN NOT =   MST-MASUISKBKBN
               MOVE    1                   TO  TBL-ERR (147)
           END-IF
      *H24.4追加４つ
           IF      ORG-FUKUBIKUNAIKBN
                                   NOT =   MST-FUKUBIKUNAIKBN 
               MOVE    1                   TO  TBL-ERR (148)
           END-IF
           IF      ORG-FUKUBIKUKOTUKBN
                                   NOT =   MST-FUKUBIKUKOTUKBN 
               MOVE    1                   TO  TBL-ERR (149)
           END-IF
           IF      ORG-TIMEMASUIKBN
                                   NOT =   MST-TIMEMASUIKBN 
               MOVE    1                   TO  TBL-ERR (150)
           END-IF
           IF      ORG-DPCKBN      NOT =   MST-DPCKBN 
               MOVE    1                   TO  TBL-ERR (151)
           END-IF
           IF      ORG-HISINSYUMONITORKSN
                                   NOT =   MST-HISINSYUMONITORKSN
               MOVE    1                   TO  TBL-ERR (152)
           END-IF
           IF      ORG-TOUKETUHOZONKSN
                                   NOT =   MST-TOUKETUHOZONKSN
               MOVE    1                   TO  TBL-ERR (153)
           END-IF
           IF      ORG-KUBUNBANGOU NOT =   MST-KUBUNBANGOU
               MOVE    1                   TO  TBL-ERR (154)
           END-IF
           IF      ORG-ROSAIKBN    NOT =   MST-ROSAIKBN
               MOVE    1                   TO  TBL-ERR (155)
           END-IF
           IF      ORG-SISIKSN     NOT =   MST-SISIKSN
               MOVE    1                   TO  TBL-ERR (156)
           END-IF
      *H30.4追加１つ
           IF      ORG-AKUSEIBYORIKSN
                                   NOT =   MST-AKUSEIBYORIKSN
               MOVE    1                   TO  TBL-ERR (157)
           END-IF
      *R02.4追加３つ
           IF      ORG-SOGAIKOTEIKSN
                                   NOT =   MST-SOGAIKOTEIKSN
               MOVE    1                   TO  TBL-ERR (158)
           END-IF
           IF      ORG-CHPSESSAKUKSN
                                   NOT =   MST-CHPSESSAKUKSN
               MOVE    1                   TO  TBL-ERR (159)
           END-IF
           IF      ORG-SASINJIHEISIKBN
                                   NOT =   MST-SASINJIHEISIKBN
               MOVE    1                   TO  TBL-ERR (160)
           END-IF
           .
       2001-COMP-ITEM-EXT.
           EXIT.
      *****************************************************************
      *    特定器材の特別処理
      *****************************************************************
       2001-TOKUTEIKIZAI-SEC       SECTION.
      *    特定器材は期間が違っていてもユーザー管理とはしない
      *    すべての期間で項目内容が標準と一致している場合は標準とする
           PERFORM 2201-KIZAI-ITEM-CHECK-SEC
      *D   DISPLAY "2201-KIZAI-ITEM-CHECK END ! [ " FLG-TOUSER " ]"
           .
       2001-TOKUTEIKIZAI-EXT.
           EXIT.
      *****************************************************************
      *    テーブル登録処理
      *      点数テーブル → 点数ユーザーテーブルへ複写
      *        特定器材の金額設定マスタでは複写しない場合あり
      *      点数テーブルからレコード削除
      *      標準テーブル → 点数テーブルへ複写
      *****************************************************************
       2100-TENSU-USER-SEC         SECTION.
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >   WRK-OCNT
               INITIALIZE  TNS-TENSU-REC
               MOVE    ORG-TENSU-BODY (IDX)
                                           TO  TNS-TENSU-REC
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *        FLG-TOUSER=2の時は点数ユーザーテーブルには登録しない
               IF      FLG-TOUSER          =   1
                   MOVE    "tbl_tensu_user"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-INSERT-SEC
                   IF      MCP-RC              =   ZERO
                       CONTINUE
                   ELSE
                       PERFORM 900-DBABORT-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       2100-TENSU-USER-EXT.
           EXIT.
      *****************************************************************
      *    点数金額テーブル登録処理
      *****************************************************************
       2200-TENSU-PRICE-SEC        SECTION.
           INITIALIZE  WRK-TENSU-PRICE-AREA
           INITIALIZE  TENSU-PRICE-REC
           MOVE    ZERO                TO  IDI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >   WRK-OCNT
               INITIALIZE  TNS-TENSU-REC
               MOVE    ORG-TENSU-BODY (IDX)
                                           TO  TNS-TENSU-REC
               IF     (IDI                 >   ZERO)   AND
                      (TNS-TEN             =   TNSPR-PRICE)
                   MOVE    TNS-YUKOEDYMD       TO
                                               TNSPR-YUKOEDYMD
               ELSE
                   MOVE    TNS-KEY             TO  TNSPR-KEY
                   MOVE    TNS-TEN             TO  TNSPR-PRICE
                   MOVE    TNS-TERMID          TO  TNSPR-TERMID
                   MOVE    TNS-OPID            TO  TNSPR-OPID
                   MOVE    TNS-CREYMD          TO  TNSPR-CREYMD
                   MOVE    TNS-UPYMD           TO  TNSPR-UPYMD
                   MOVE    TNS-UPHMS           TO  TNSPR-UPHMS
                   ADD     1                   TO  IDI
                   IF      IDI                 >   30
                       MOVE    "WRK-TENSU-PRICE-BODY"
                                                   TO  WRK-ARRAYNAME
                       PERFORM 900-ARRAYABORT-SEC
                   END-IF
               END-IF
               MOVE    TENSU-PRICE-REC     TO
                                           WRK-TENSU-PRICE-BODY (IDI)
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >   IDI
               INITIALIZE  TENSU-PRICE-REC
               MOVE    WRK-TENSU-PRICE-BODY (IDX)
                                           TO  TENSU-PRICE-REC
               MOVE    TENSU-PRICE-REC     TO  MCPDATA-REC
               MOVE    "tbl_tensu_price"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-INSERT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    4                   TO  FLG-GUID
                   PERFORM 20102-PRF-OUT-SEC
               ELSE
                   PERFORM 900-DBABORT-SEC
               END-IF
           END-PERFORM
           .
       2200-TENSU-PRICE-EXT.
           EXIT.
      *****************************************************************
      *    特定器材項目チェック処理
      *****************************************************************
       2201-KIZAI-ITEM-CHECK-SEC   SECTION.
           MOVE    1                   TO  IDO
           MOVE    1                   TO  IDM
           INITIALIZE ORG-TENSU-REC
           MOVE    ORG-TENSU-BODY (IDO)
                                       TO  ORG-TENSU-REC
           INITIALIZE MST-TENSU-REC
           MOVE    MST-TENSU-BODY (IDM)
                                       TO  MST-TENSU-REC
           .
       2201-KIZAI-ITEM-CHECK-10.
      *D   DISPLAY "ORG [ " IDO " ] [ " ORG-YUKOSTYMD " - "
      *D           ORG-YUKOEDYMD " ]"
      *D   DISPLAY "MST [ " IDM " ] [ " MST-YUKOSTYMD " - "
      *D           MST-YUKOEDYMD " ]"
           IF     (ORG-YUKOSTYMD       >=  MST-YUKOSTYMD)  AND
                  (ORG-YUKOSTYMD       <=  MST-YUKOEDYMD)
               PERFORM 2001-COMP-ITEM-SEC
               IF     (FLG-TEN2PRICE       =   1)  AND
                     ((TBL-ERR (12)        =   1)  OR
                      (TBL-ERR (13)        =   1))
      *            点数金額テーブルへ登録
                   IF      FLG-PRICE           =   ZERO
                       PERFORM 2200-TENSU-PRICE-SEC
                       MOVE    1                   TO  FLG-PRICE
                   END-IF
      *            比較除外項目リセット(特定器材／点数識別、点数)
                   MOVE    ZERO                TO  TBL-ERR (12)
                   MOVE    ZERO                TO  TBL-ERR (13)
               END-IF
review*        公表順序番号、変更年月日、廃止年月日（テスト）
               MOVE    ZERO                TO  TBL-ERR (132)
               MOVE    ZERO                TO  TBL-ERR (136)
               MOVE    ZERO                TO  TBL-ERR (137)
               IF      TBL-ERR-AREA        =   ZERO
      *            変更なし
                   CONTINUE
      *D           DISPLAY "☆特定器材　相違なし"
               ELSE
      *            変更分メッセージ出力
      *D           DISPLAY "★特定器材　相違あり"
                   MOVE    1                   TO  FLG-TOUSER
                   MOVE    3                   TO  FLG-GUID
                   PERFORM 20102-PRF-SET-SEC
                   GO  TO  2201-KIZAI-ITEM-CHECK-EXT
               END-IF
               IF      ORG-YUKOEDYMD       <=  MST-YUKOEDYMD
                   IF      IDO                 >=  WRK-OCNT
      *D               DISPLAY "★ＯＫ"
                       MOVE    2                   TO  FLG-TOUSER
                   ELSE
                       ADD     1                   TO  IDO
                       MOVE    ORG-TENSU-BODY (IDO)
                                                   TO  ORG-TENSU-REC
                       GO  TO  2201-KIZAI-ITEM-CHECK-10
                   END-IF
               ELSE
               IF      ORG-YUKOEDYMD       >   MST-YUKOEDYMD
      *            ORG-YUKOSTYMDを標準の終了日の翌日にする
                   INITIALIZE  STS-AREA-DAY
                   INITIALIZE  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    MST-YUKOEDYMD       TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO  ORG-YUKOSTYMD
      *D           DISPLAY "★翌日 [ " ORG-YUKOSTYMD " ]"
                   IF      IDM                 >=  WRK-MCNT
      *D               DISPLAY "★アウト"
                       MOVE    1                   TO  FLG-TOUSER
                   ELSE
                       ADD     1                   TO  IDM
                       MOVE    MST-TENSU-BODY (IDM)
                                                   TO  MST-TENSU-REC
                       GO  TO  2201-KIZAI-ITEM-CHECK-10
                   END-IF
               ELSE
review*            本当にアウトかロジックを見直す
review*            ELSEを再考する
      *D           DISPLAY "★アウト"
                   MOVE    1                   TO  FLG-TOUSER
               END-IF
               END-IF
           ELSE
           IF      ORG-YUKOSTYMD       <   MST-YUKOSTYMD
      *D       DISPLAY "★アウト"
               MOVE    1                   TO  FLG-TOUSER
           ELSE
           IF      ORG-YUKOSTYMD       >   MST-YUKOEDYMD
               ADD     1                   TO  IDM
               IF      IDM             >   WRK-MCNT
      *D           DISPLAY "★アウト"
                   MOVE    1                   TO  FLG-TOUSER
               ELSE
                   MOVE    MST-TENSU-BODY (IDM)
                                               TO  MST-TENSU-REC
                   GO  TO  2201-KIZAI-ITEM-CHECK-10
               END-IF
           ELSE
      *D       DISPLAY "★アウト"
               MOVE    1                   TO  FLG-TOUSER
           END-IF
           END-IF
           END-IF
           .
       2201-KIZAI-ITEM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    変更個所表示処理
      *****************************************************************
       20102-PRF-SET-SEC           SECTION.
           PERFORM VARYING IDX     FROM    COL-START  BY  1
                   UNTIL   IDX     >   TBL-TENSU-NAME-MAX
               MOVE    SPACE               TO  WRK-PRF-MSG
               MOVE    ZERO                TO  FLG-DECIMAL
               IF      TBL-ERR (IDX)       =   1
                   SET     TBL-TNM             TO  IDX
                   MOVE    TBL-TNM-NAME(TBL-TNM)
                                               TO  WRK-PRF-ITM
                   EVALUATE    IDX
                       WHEN    4
                           MOVE    ORG-SRYKBN          TO  WRK-PRF-USR
                           MOVE    MST-SRYKBN          TO  WRK-PRF-MST
                       WHEN    5
                           MOVE    ORG-SRYSYUKBN       TO  WRK-PRF-USR
                           MOVE    MST-SRYSYUKBN       TO  WRK-PRF-MST
                       WHEN    6
                           MOVE    ORG-YUKOKETA        TO  WRK-PRF-USR
                           MOVE    MST-YUKOKETA        TO  WRK-PRF-MST
                       WHEN    7
                           MOVE    ORG-NAME            TO  WRK-PRF-USR
                           MOVE    MST-NAME            TO  WRK-PRF-MST
                       WHEN    8
                           MOVE    ORG-KANAYUKOKETA    TO  WRK-PRF-USR
                           MOVE    MST-KANAYUKOKETA    TO  WRK-PRF-MST
                       WHEN    9
                           MOVE    ORG-KANANAME        TO  WRK-PRF-USR
                           MOVE    MST-KANANAME        TO  WRK-PRF-MST
                       WHEN    10
                           MOVE    ORG-FORMALYUKOKETA  TO  WRK-PRF-USR
                           MOVE    MST-FORMALYUKOKETA  TO  WRK-PRF-MST
                       WHEN    11
                           MOVE    ORG-FORMALNAME      TO  WRK-PRF-USR
                           MOVE    MST-FORMALNAME      TO  WRK-PRF-MST
                       WHEN    12
                           MOVE    ORG-TENSIKIBETU     TO  WRK-PRF-USR
                           MOVE    MST-TENSIKIBETU     TO  WRK-PRF-MST
                       WHEN    13
                           MOVE    1                   TO  FLG-DECIMAL
                           MOVE    ORG-TEN             TO  WRK-PRF-USR-R
                           MOVE    MST-TEN             TO  WRK-PRF-MST-R
                       WHEN    14
                           MOVE    ORG-TANICD          TO  WRK-PRF-USR
                           MOVE    MST-TANICD          TO  WRK-PRF-MST
                       WHEN    15
                           MOVE    ORG-TANIMOJISU      TO  WRK-PRF-USR
                           MOVE    MST-TANIMOJISU      TO  WRK-PRF-MST
                       WHEN    16
                           MOVE    ORG-TANINAME        TO  WRK-PRF-USR
                           MOVE    MST-TANINAME        TO  WRK-PRF-MST
                       WHEN    17
                           MOVE    ORG-DATAKBN         TO  WRK-PRF-USR
                           MOVE    MST-DATAKBN         TO  WRK-PRF-MST
                       WHEN    18
                           MOVE    ORG-HKNTEKKBN       TO  WRK-PRF-USR
                           MOVE    MST-HKNTEKKBN       TO  WRK-PRF-MST
                       WHEN    19
                           MOVE    ORG-NYUGAITEKKBN    TO  WRK-PRF-USR
                           MOVE    MST-NYUGAITEKKBN    TO  WRK-PRF-MST
                       WHEN    20
                           MOVE    ORG-ROUTEKKBN       TO  WRK-PRF-USR
                           MOVE    MST-ROUTEKKBN       TO  WRK-PRF-MST
                       WHEN    21
                           MOVE    ORG-HOSPSRYKBN      TO  WRK-PRF-USR
                           MOVE    MST-HOSPSRYKBN      TO  WRK-PRF-MST
                       WHEN    22
                           MOVE    ORG-OSINKBN         TO  WRK-PRF-USR
                           MOVE    MST-OSINKBN         TO  WRK-PRF-MST
                       WHEN    23
                           MOVE    ORG-HOUKNSKBN       TO  WRK-PRF-USR
                           MOVE    MST-HOUKNSKBN       TO  WRK-PRF-MST
                       WHEN    24
                           MOVE    ORG-BYOKANRENKBN    TO  WRK-PRF-USR
                           MOVE    MST-BYOKANRENKBN    TO  WRK-PRF-MST
                       WHEN    25
                           MOVE    ORG-SDOKNRYO        TO  WRK-PRF-USR
                           MOVE    MST-SDOKNRYO        TO  WRK-PRF-MST
                       WHEN    26
                           MOVE    ORG-JITUDAY         TO  WRK-PRF-USR
                           MOVE    MST-JITUDAY         TO  WRK-PRF-MST
                       WHEN    27
                           MOVE    ORG-DAYCNT          TO  WRK-PRF-USR
                           MOVE    MST-DAYCNT          TO  WRK-PRF-MST
                       WHEN    28
                           MOVE    ORG-YKZKNRKBN       TO  WRK-PRF-USR
                           MOVE    MST-YKZKNRKBN       TO  WRK-PRF-MST
                       WHEN    29
                           MOVE    ORG-KZMCOMPSIKIBETU TO  WRK-PRF-USR
                           MOVE    MST-KZMCOMPSIKIBETU TO  WRK-PRF-MST
                       WHEN    30
                           MOVE    ORG-KZMKGN          TO  WRK-PRF-USR
                           MOVE    MST-KZMKGN          TO  WRK-PRF-MST
                       WHEN    31
                           MOVE    ORG-KZMJGN          TO  WRK-PRF-USR
                           MOVE    MST-KZMJGN          TO  WRK-PRF-MST
                       WHEN    32
                           MOVE    ORG-KZM             TO  WRK-PRF-USR
                           MOVE    MST-KZM             TO  WRK-PRF-MST
                       WHEN    33
                           MOVE    1                   TO  FLG-DECIMAL
                           MOVE    ORG-KZMTEN          TO  WRK-PRF-USR-R
                           MOVE    MST-KZMTEN          TO  WRK-PRF-MST-R
                       WHEN    34
                           MOVE    ORG-KZMERR          TO  WRK-PRF-USR
                           MOVE    MST-KZMERR          TO  WRK-PRF-MST
                       WHEN    35
                           MOVE    ORG-JGNCNT          TO  WRK-PRF-USR
                           MOVE    MST-JGNCNT          TO  WRK-PRF-MST
                       WHEN    36
                           MOVE    ORG-JGNCNT1D        TO  WRK-PRF-USR
                           MOVE    MST-JGNCNT1D        TO  WRK-PRF-MST
                       WHEN    37
                           MOVE    ORG-JGNCNT1W        TO  WRK-PRF-USR
                           MOVE    MST-JGNCNT1W        TO  WRK-PRF-MST
                       WHEN    38
                           MOVE    ORG-JGNCNTETCM      TO  WRK-PRF-USR
                           MOVE    MST-JGNCNTETCM      TO  WRK-PRF-MST
                       WHEN    39
                           MOVE    ORG-JGNCNTETC       TO  WRK-PRF-USR
                           MOVE    MST-JGNCNTETC       TO  WRK-PRF-MST
                       WHEN    40
                           MOVE    ORG-JGNCNTERR       TO  WRK-PRF-USR
                           MOVE    MST-JGNCNTERR       TO  WRK-PRF-MST
                       WHEN    41
                           MOVE    ORG-CHUKSNCD        TO  WRK-PRF-USR
                           MOVE    MST-CHUKSNCD        TO  WRK-PRF-MST
                       WHEN    42
                           MOVE    ORG-CHUKSNTUBAN     TO  WRK-PRF-USR
                           MOVE    MST-CHUKSNTUBAN     TO  WRK-PRF-MST
                       WHEN    43
                           MOVE    ORG-TSUSOKUAGE      TO  WRK-PRF-USR
                           MOVE    MST-TSUSOKUAGE      TO  WRK-PRF-MST
                       WHEN    44
                           MOVE    ORG-KGNAGE          TO  WRK-PRF-USR
                           MOVE    MST-KGNAGE          TO  WRK-PRF-MST
                       WHEN    45
                           MOVE    ORG-JGNAGE          TO  WRK-PRF-USR
                           MOVE    MST-JGNAGE          TO  WRK-PRF-MST
                       WHEN    46
                           MOVE    ORG-TIMEKSNKBN      TO  WRK-PRF-USR
                           MOVE    MST-TIMEKSNKBN      TO  WRK-PRF-MST
                       WHEN    47
                           MOVE    ORG-KIJUNTEIGENKBN  TO  WRK-PRF-USR
                           MOVE    MST-KIJUNTEIGENKBN  TO  WRK-PRF-MST
                       WHEN    48
                           MOVE    ORG-KIJUNTEIGENCD   TO  WRK-PRF-USR
                           MOVE    MST-KIJUNTEIGENCD   TO  WRK-PRF-MST
                       WHEN    49
                           MOVE    ORG-LASERKBN        TO  WRK-PRF-USR
                           MOVE    MST-LASERKBN        TO  WRK-PRF-MST
                       WHEN    50
                           MOVE    ORG-CHPMESUKSN      TO  WRK-PRF-USR
                           MOVE    MST-CHPMESUKSN      TO  WRK-PRF-MST
                       WHEN    51
                           MOVE    ORG-MICMESUKSN      TO  WRK-PRF-USR
                           MOVE    MST-MICMESUKSN      TO  WRK-PRF-MST
                       WHEN    52
                           MOVE    ORG-DONORKBN        TO  WRK-PRF-USR
                           MOVE    MST-DONORKBN        TO  WRK-PRF-MST
                       WHEN    53
                           MOVE    ORG-KNSJISKBN       TO  WRK-PRF-USR
                           MOVE    MST-KNSJISKBN       TO  WRK-PRF-MST
                       WHEN    54
                           MOVE    ORG-KNSJISGRPKBN    TO  WRK-PRF-USR
                           MOVE    MST-KNSJISGRPKBN    TO  WRK-PRF-MST
                       WHEN    55
                           MOVE    ORG-TEIGENKBN       TO  WRK-PRF-USR
                           MOVE    MST-TEIGENKBN       TO  WRK-PRF-MST
                       WHEN    56
                           MOVE    ORG-SEKIZUISOKUTEI  TO  WRK-PRF-USR
                           MOVE    MST-SEKIZUISOKUTEI  TO  WRK-PRF-MST
                       WHEN    57
                           MOVE    ORG-KEIBUJYUTU      TO  WRK-PRF-USR
                           MOVE    MST-KEIBUJYUTU      TO  WRK-PRF-MST
                       WHEN    58
                           MOVE    ORG-AUTOHOUGO       TO  WRK-PRF-USR
                           MOVE    MST-AUTOHOUGO       TO  WRK-PRF-MST
                       WHEN    59
                           MOVE    ORG-GAIKANRIKBN     TO  WRK-PRF-USR
                           MOVE    MST-GAIKANRIKBN     TO  WRK-PRF-MST
                       WHEN    60
                           MOVE    ORG-TUSOKUGAIKBN    TO  WRK-PRF-USR
                           MOVE    MST-TUSOKUGAIKBN    TO  WRK-PRF-MST
                       WHEN    61
                           MOVE    ORG-HOUKATUTEIGENKBN
                                                       TO  WRK-PRF-USR
                           MOVE    MST-HOUKATUTEIGENKBN
                                                       TO  WRK-PRF-MST
                       WHEN    62
                           MOVE    ORG-CHPNAISIKSN     TO  WRK-PRF-USR
                           MOVE    MST-CHPNAISIKSN     TO  WRK-PRF-MST
                       WHEN    63
                           MOVE    ORG-AUTOFUNGO       TO  WRK-PRF-USR
                           MOVE    MST-AUTOFUNGO       TO  WRK-PRF-MST
                       WHEN    64
                           MOVE    ORG-CHIIKIKSN       TO  WRK-PRF-USR
                           MOVE    MST-CHIIKIKSN       TO  WRK-PRF-MST
                       WHEN    65
                           MOVE    ORG-BYOSYOKBN       TO  WRK-PRF-USR
                           MOVE    MST-BYOSYOKBN       TO  WRK-PRF-MST
                       WHEN    66
                           MOVE    ORG-CHPGYOKOSOTIKSN TO  WRK-PRF-USR
                           MOVE    MST-CHPGYOKOSOTIKSN TO  WRK-PRF-MST
                       WHEN    67
                           MOVE    ORG-SHORTSTAYOPE    TO  WRK-PRF-USR
                           MOVE    MST-SHORTSTAYOPE    TO  WRK-PRF-MST
                       WHEN    68
                           MOVE    ORG-BUIKBN          TO  WRK-PRF-USR
                           MOVE    MST-BUIKBN          TO  WRK-PRF-MST
                       WHEN    69
                           MOVE    ORG-SANTEIRRKKBN    TO  WRK-PRF-USR
                           MOVE    MST-SANTEIRRKKBN    TO  WRK-PRF-MST
                       WHEN    70 THRU 79
                           COMPUTE IDY                 =   IDX - 69
                           MOVE    ORG-SSTKIJUNCD(IDY) TO  WRK-PRF-USR
                           MOVE    MST-SSTKIJUNCD(IDY) TO  WRK-PRF-MST
                       WHEN    80
                       WHEN    83
                       WHEN    86
                       WHEN    89
                           COMPUTE IDY                 =   (IDX - 80)
                                                       /   3 + 1
                           MOVE    ORG-AGEKSNKGN(IDY)  TO  WRK-PRF-USR
                           MOVE    MST-AGEKSNKGN(IDY)  TO  WRK-PRF-MST
                       WHEN    81
                       WHEN    84
                       WHEN    87
                       WHEN    90
                           COMPUTE IDY                 =   (IDX - 81)
                                                       /   3 + 1
                           MOVE    ORG-AGEKSNJGN(IDY)  TO  WRK-PRF-USR
                           MOVE    MST-AGEKSNJGN(IDY)  TO  WRK-PRF-MST
                       WHEN    82
                       WHEN    85
                       WHEN    88
                       WHEN    91
                           COMPUTE IDY                 =   (IDX - 82)
                                                       /   3 + 1
                           MOVE    ORG-AGEKSNSRYCD(IDY)
                                                       TO  WRK-PRF-USR
                           MOVE    MST-AGEKSNSRYCD(IDY)
                                                       TO  WRK-PRF-MST
                       WHEN    92
                           MOVE    ORG-KENTAICOMMENT   TO  WRK-PRF-USR
                           MOVE    MST-KENTAICOMMENT   TO  WRK-PRF-MST
                       WHEN    93
                           MOVE    ORG-NYUKHNKBN       TO  WRK-PRF-USR
                           MOVE    MST-NYUKHNKBN       TO  WRK-PRF-MST
                       WHEN    94
                           MOVE    ORG-NYUKHNKSNKBN    TO  WRK-PRF-USR
                           MOVE    MST-NYUKHNKSNKBN    TO  WRK-PRF-MST
                       WHEN    95
                           MOVE    ORG-KANGOKSN        TO  WRK-PRF-USR
                           MOVE    MST-KANGOKSN        TO  WRK-PRF-MST
                       WHEN    96
                           MOVE    1                   TO  FLG-DECIMAL
                           MOVE    ORG-OLDTENSKB       TO  WRK-PRF-USR-R
                           MOVE    MST-OLDTENSKB       TO  WRK-PRF-MST-R
                       WHEN    97
                           MOVE    1                   TO  FLG-DECIMAL
                           MOVE    ORG-OLDTEN          TO  WRK-PRF-USR-R
                           MOVE    MST-OLDTEN          TO  WRK-PRF-MST-R
                       WHEN    98
                           MOVE    ORG-MADOKUKBN       TO  WRK-PRF-USR
                           MOVE    MST-MADOKUKBN       TO  WRK-PRF-MST
                       WHEN    99
                           MOVE    ORG-SINKEIHAKAIKBN  TO  WRK-PRF-USR
                           MOVE    MST-SINKEIHAKAIKBN  TO  WRK-PRF-MST
                       WHEN    100
                           MOVE    ORG-SEIBUTUGAKUKBN  TO  WRK-PRF-USR
                           MOVE    MST-SEIBUTUGAKUKBN  TO  WRK-PRF-MST
                       WHEN    101
                           MOVE    ORG-ZOEIZAIKBN      TO  WRK-PRF-USR
                           MOVE    MST-ZOEIZAIKBN      TO  WRK-PRF-MST
                       WHEN    102
                           MOVE    ORG-CSYYOURYO       TO  WRK-PRF-USR
                           MOVE    MST-CSYYOURYO       TO  WRK-PRF-MST
                       WHEN    103
                           MOVE    ORG-YKZKBN          TO  WRK-PRF-USR
                           MOVE    MST-YKZKBN          TO  WRK-PRF-MST
                       WHEN    104
                           MOVE    ORG-ZAIGATAKBN      TO  WRK-PRF-USR
                           MOVE    MST-ZAIGATAKBN      TO  WRK-PRF-MST
                       WHEN    105
                           MOVE    ORG-KOUHATUKBN      TO  WRK-PRF-USR
                           MOVE    MST-KOUHATUKBN      TO  WRK-PRF-MST
                       WHEN    106
                           MOVE    ORG-LONGTOYOKBN     TO  WRK-PRF-USR
                           MOVE    MST-LONGTOYOKBN     TO  WRK-PRF-MST
                       WHEN    107
                           MOVE    ORG-MEIUSESKB       TO  WRK-PRF-USR
                           MOVE    MST-MEIUSESKB       TO  WRK-PRF-MST
                       WHEN    108
                           MOVE    ORG-TOKUKIZAIAGEKSNKBN
                                                       TO  WRK-PRF-USR
                           MOVE    MST-TOKUKIZAIAGEKSNKBN
                                                       TO  WRK-PRF-MST
                       WHEN    109
                           MOVE    ORG-SANSOKBN        TO  WRK-PRF-USR
                           MOVE    MST-SANSOKBN        TO  WRK-PRF-MST
                       WHEN    110
                           MOVE    ORG-TOKUKIZAISBT1   TO  WRK-PRF-USR
                           MOVE    MST-TOKUKIZAISBT1   TO  WRK-PRF-MST
                       WHEN    111
                           MOVE    ORG-TOKUKIZAISBT2   TO  WRK-PRF-USR
                           MOVE    MST-TOKUKIZAISBT2   TO  WRK-PRF-MST
                       WHEN    112
                           MOVE    ORG-JGNTEN          TO  WRK-PRF-USR
                           MOVE    MST-JGNTEN          TO  WRK-PRF-MST
                       WHEN    113
                           MOVE    ORG-GAITENTTLKBN    TO  WRK-PRF-USR
                           MOVE    MST-GAITENTTLKBN    TO  WRK-PRF-MST
                       WHEN    114
                           MOVE    ORG-NYUTENTTLKBN    TO  WRK-PRF-USR
                           MOVE    MST-NYUTENTTLKBN    TO  WRK-PRF-MST
                       WHEN    115
                           MOVE    ORG-CDKBN-KBN       TO  WRK-PRF-USR
                           MOVE    MST-CDKBN-KBN       TO  WRK-PRF-MST
                       WHEN    116
                           MOVE    ORG-CDKBN-SYO       TO  WRK-PRF-USR
                           MOVE    MST-CDKBN-SYO       TO  WRK-PRF-MST
                       WHEN    117
                           MOVE    ORG-CDKBN-BU        TO  WRK-PRF-USR
                           MOVE    MST-CDKBN-BU        TO  WRK-PRF-MST
                       WHEN    118
                           MOVE    ORG-CDKBN-KBNNUM    TO  WRK-PRF-USR
                           MOVE    MST-CDKBN-KBNNUM    TO  WRK-PRF-MST
                       WHEN    119
                           MOVE    ORG-CDKBN-KBNNUM-EDA
                                                       TO  WRK-PRF-USR
                           MOVE    MST-CDKBN-KBNNUM-EDA
                                                       TO  WRK-PRF-MST
                       WHEN    120
                           MOVE    ORG-CDKBN-KOUBAN    TO  WRK-PRF-USR
                           MOVE    MST-CDKBN-KOUBAN    TO  WRK-PRF-MST
                       WHEN    121
                           MOVE    ORG-KOKUCHI-KBN     TO  WRK-PRF-USR
                           MOVE    MST-KOKUCHI-KBN     TO  WRK-PRF-MST
                       WHEN    122
                           MOVE    ORG-KOKUCHI-SYO     TO  WRK-PRF-USR
                           MOVE    MST-KOKUCHI-SYO     TO  WRK-PRF-MST
                       WHEN    123
                           MOVE    ORG-KOKUCHI-BU      TO  WRK-PRF-USR
                           MOVE    MST-KOKUCHI-BU      TO  WRK-PRF-MST
                       WHEN    124
                           MOVE    ORG-KOKUCHI-KBNNUM  TO  WRK-PRF-USR
                           MOVE    MST-KOKUCHI-KBNNUM  TO  WRK-PRF-MST
                       WHEN    125
                           MOVE    ORG-KOKUCHI-KBNNUM-EDA
                                                       TO  WRK-PRF-USR
                           MOVE    MST-KOKUCHI-KBNNUM-EDA
                                                       TO  WRK-PRF-MST
                       WHEN    126
                           MOVE    ORG-KOKUCHI-KOUBAN  TO  WRK-PRF-USR
                           MOVE    MST-KOKUCHI-KOUBAN  TO  WRK-PRF-MST
                       WHEN    127
                           MOVE    ORG-KOKUJISKBKBN1   TO  WRK-PRF-USR
                           MOVE    MST-KOKUJISKBKBN1   TO  WRK-PRF-MST
                       WHEN    128
                           MOVE    ORG-KOKUJISKBKBN2   TO  WRK-PRF-USR
                           MOVE    MST-KOKUJISKBKBN2   TO  WRK-PRF-MST
                       WHEN    129
                           MOVE    ORG-NAMECHGKBN      TO  WRK-PRF-USR
                           MOVE    MST-NAMECHGKBN      TO  WRK-PRF-MST
                       WHEN    130
                           MOVE    ORG-KANANAMECHGKBN  TO  WRK-PRF-USR
                           MOVE    MST-KANANAMECHGKBN  TO  WRK-PRF-MST
                       WHEN    131
                           MOVE    ORG-IDOKANREN       TO  WRK-PRF-USR
                           MOVE    MST-IDOKANREN       TO  WRK-PRF-MST
                       WHEN    132
                           MOVE    ORG-KOUHYOJYUNNUM   TO  WRK-PRF-USR
                           MOVE    MST-KOUHYOJYUNNUM   TO  WRK-PRF-MST
                       WHEN    133
                           MOVE    ORG-YAKKAKJNCD      TO  WRK-PRF-USR
                           MOVE    MST-YAKKAKJNCD      TO  WRK-PRF-MST
                       WHEN    134
                           MOVE    ORG-SYUSAISKB       TO  WRK-PRF-USR
                           MOVE    MST-SYUSAISKB       TO  WRK-PRF-MST
                       WHEN    135
                           MOVE    ORG-SYOMEIKANREN    TO  WRK-PRF-USR
                           MOVE    MST-SYOMEIKANREN    TO  WRK-PRF-MST
                       WHEN    136
                           MOVE    ORG-CHGYMD          TO  WRK-PRF-USR
                           MOVE    MST-CHGYMD          TO  WRK-PRF-MST
                       WHEN    137
                           MOVE    ORG-HAISIYMD        TO  WRK-PRF-USR
                           MOVE    MST-HAISIYMD        TO  WRK-PRF-MST
                       WHEN    138
                           MOVE    ORG-KEIKASOCHIYMD   TO  WRK-PRF-USR
                           MOVE    MST-KEIKASOCHIYMD   TO  WRK-PRF-MST
      *    139 端末ＩＤ
      *    140 オペレーターＩＤ
      *    141 作成年月日
      *    142 更新年月日
      *    143 更新時間
      *    144 医療機関識別番号
                       WHEN    145
                           MOVE    ORG-GAZOOPESUP      TO  WRK-PRF-USR
                           MOVE    MST-GAZOOPESUP      TO  WRK-PRF-MST
                       WHEN    146
                           MOVE    ORG-IRYOKANSATUKBN  TO  WRK-PRF-USR
                           MOVE    MST-IRYOKANSATUKBN  TO  WRK-PRF-MST
                       WHEN    147
                           MOVE    ORG-MASUISKBKBN     TO  WRK-PRF-USR
                           MOVE    MST-MASUISKBKBN     TO  WRK-PRF-MST
      *H24.4追加４つ
                       WHEN    148
                           MOVE    ORG-FUKUBIKUNAIKBN  TO  WRK-PRF-USR
                           MOVE    MST-FUKUBIKUNAIKBN  TO  WRK-PRF-MST
                       WHEN    149
                           MOVE    ORG-FUKUBIKUKOTUKBN TO  WRK-PRF-USR
                           MOVE    MST-FUKUBIKUKOTUKBN TO  WRK-PRF-MST
                       WHEN    150
                           MOVE    ORG-TIMEMASUIKBN    TO  WRK-PRF-USR
                           MOVE    MST-TIMEMASUIKBN    TO  WRK-PRF-MST
                       WHEN    151
                           MOVE    ORG-DPCKBN          TO  WRK-PRF-USR
                           MOVE    MST-DPCKBN          TO  WRK-PRF-MST
      *H28.4追加５つ
                       WHEN    152
                           MOVE    ORG-HISINSYUMONITORKSN
                                                       TO  WRK-PRF-USR
                           MOVE    MST-HISINSYUMONITORKSN
                                                       TO  WRK-PRF-MST
                       WHEN    153
                           MOVE    ORG-TOUKETUHOZONKSN TO  WRK-PRF-USR
                           MOVE    MST-TOUKETUHOZONKSN TO  WRK-PRF-MST
                       WHEN    154
                           MOVE    ORG-KUBUNBANGOU     TO  WRK-PRF-USR
                           MOVE    MST-KUBUNBANGOU     TO  WRK-PRF-MST
                       WHEN    155
                           MOVE    ORG-ROSAIKBN        TO  WRK-PRF-USR
                           MOVE    MST-ROSAIKBN        TO  WRK-PRF-MST
                       WHEN    156
                           MOVE    ORG-SISIKSN         TO  WRK-PRF-USR
                           MOVE    MST-SISIKSN         TO  WRK-PRF-MST
      *H30.4追加１つ
                       WHEN    157
                           MOVE    ORG-AKUSEIBYORIKSN  TO  WRK-PRF-USR
                           MOVE    MST-AKUSEIBYORIKSN  TO  WRK-PRF-MST
      *R02.4追加３つ
                       WHEN    158
                           MOVE    ORG-SOGAIKOTEIKSN   TO  WRK-PRF-USR
                           MOVE    MST-SOGAIKOTEIKSN   TO  WRK-PRF-MST
                       WHEN    159
                           MOVE    ORG-CHPSESSAKUKSN   TO  WRK-PRF-USR
                           MOVE    MST-CHPSESSAKUKSN   TO  WRK-PRF-MST
                       WHEN    160
                           MOVE    ORG-SASINJIHEISIKBN TO  WRK-PRF-USR
                           MOVE    MST-SASINJIHEISIKBN TO  WRK-PRF-MST
                   END-EVALUATE
                   PERFORM 20102-PRF-OUT-SEC
               END-IF
           END-PERFORM
           .
       20102-PRF-SET-EXT.
           EXIT.
      *****************************************************************
      *    プルーフ出力
      *****************************************************************
       20102-PRF-OUT-SEC           SECTION.
           IF      CNT-PRF             =   ZERO
               MOVE    MES-TITLE1          TO  PRF-REC
               WRITE   PRF-REC
               MOVE    MES-TITLE2          TO  PRF-REC
               WRITE   PRF-REC
           END-IF
      *
           EVALUATE    FLG-GUID
               WHEN    1
                   MOVE    SPACE               TO  WRK-PRF-AREA
                   MOVE    "ユーザー点数既存　スキップ"
                                               TO  WRK-PRF-ANS
                   MOVE    WRK-SRYCD           TO  WRK-PRF-SRYCD
                   MOVE    TNS-YUKOSTYMD       TO  WRK-PRF-STYMD
                   MOVE    TNS-YUKOEDYMD       TO  WRK-PRF-EDYMD
                   MOVE    TNS-NAME            TO  WRK-PRF-NAME
                   MOVE    SPACE               TO  TNS-UPYMD
               WHEN    2
                   MOVE    SPACE               TO  WRK-PRF-AREA
                   MOVE    "期間不一致"        TO  WRK-PRF-ANS
                   MOVE    WRK-SRYCD           TO  WRK-PRF-SRYCD
                   MOVE    MST-NAME            TO  WRK-PRF-NAME
                   MOVE    "有効期間"          TO  WRK-PRF-ITM
                   STRING  MST-YUKOSTYMD       DELIMITED BY SIZE
                           " - "               DELIMITED BY SIZE
                           MST-YUKOEDYMD       DELIMITED BY SIZE
                       INTO    WRK-PRF-MST
                   END-STRING
                   STRING  ORG-YUKOSTYMD       DELIMITED BY SIZE
                           " - "               DELIMITED BY SIZE
                           ORG-YUKOEDYMD       DELIMITED BY SIZE
                       INTO    WRK-PRF-USR
                   END-STRING
               WHEN    3
                   IF      FLG-TOUSER          =   1
                       MOVE    "ユーザー管理"      TO  WRK-PRF-ANS
                   END-IF
                   MOVE    "項目値変更"        TO  WRK-PRF-GUID
                   MOVE    WRK-SRYCD           TO  WRK-PRF-SRYCD
                   MOVE    MST-YUKOSTYMD       TO  WRK-PRF-STYMD
                   MOVE    MST-YUKOEDYMD       TO  WRK-PRF-EDYMD
                   MOVE    MST-NAME            TO  WRK-PRF-NAME
                   IF      FLG-DECIMAL         =   1
                       PERFORM 800-DECIMAL-SEC
                   END-IF
               WHEN    4
                   MOVE    SPACE               TO  WRK-PRF-AREA
                   MOVE    "特定器材"          TO  WRK-PRF-ANS
                   MOVE    "金額"              TO  WRK-PRF-GUID
                   MOVE    WRK-SRYCD           TO  WRK-PRF-SRYCD
                   MOVE    TNSPR-YUKOSTYMD     TO  WRK-PRF-STYMD
                   MOVE    TNSPR-YUKOEDYMD     TO  WRK-PRF-EDYMD
                   MOVE    MST-NAME            TO  WRK-PRF-NAME
                   MOVE    TNSPR-PRICE         TO  WRK-PRF-USR-R
                   PERFORM 800-DECIMAL-SEC
               WHEN    OTHER
                   DISPLAY "20102-PRF-OUT-SEC FLG-GUID ERROR"
           END-EVALUATE
      *
           MOVE    SPACE               TO  PRF-REC
           STRING  WRK-PRF-ANS         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-SRYCD       DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-STYMD       DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-EDYMD       DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-NAME        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-GUID        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-ITM         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-MST         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   WRK-PRF-USR         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   TNS-UPYMD           DELIMITED BY SPACE
                                       INTO  PRF-REC
           END-STRING
      *
           WRITE   PRF-REC
           ADD     1                   TO  CNT-PRF
           IF     (WRK-LIMIT-PRF   NOT =   ZERO   )  AND
                  (WRK-LIMIT-PRF       =   CNT-PRF)
               MOVE    1                   TO  FLG-END
               DISPLAY " "
               DISPLAY LIMIT-MSG
               DISPLAY " "
           END-IF
           .
       201021-PRF-OUT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
           PERFORM 900-DBDISCONNECT-SEC
           CLOSE   PRF-FILE
      *
           DISPLAY " "
      *D   MOVE    CNT-IN              TO  WRK-MSG-CNT
      *D   DISPLAY "*(ORCBMIGTENSU)* Original Data [ " WRK-MSG-CNT " ]"
           MOVE    CNT-PRF             TO  WRK-MSG-CNT
           DISPLAY "*(ORCBMIGTENSU)* Proof Data    [ " WRK-MSG-CNT " ]"
           DISPLAY " "
           DISPLAY "ORCBMIGTENSU: END"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    小数点処理
      *****************************************************************
       800-DECIMAL-SEC             SECTION.
           MOVE    WRK-PRF-MST-R       TO  WRK-DECIMAL
           IF      WRK-DECIMAL-DEC     =   ZERO
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    FUNCTION TRIM (WRK-ZERO-SUP)
                                           TO  WRK-PRF-MST
           ELSE
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    WRK-ZERO-SUP        TO  WRK-PRF-INT
               MOVE    WRK-DECIMAL-DEC     TO  WRK-ZERO-SUP-N
               MOVE    WRK-ZERO-SUP (4:)   TO  WRK-PRF-DEC
               MOVE    "."                 TO  WRK-PRF-POINT
               MOVE    FUNCTION TRIM (WRK-PRF-DECIMAL)
                                           TO  WRK-PRF-MST
           END-IF
           MOVE    WRK-PRF-USR-R       TO  WRK-DECIMAL
           IF      WRK-DECIMAL-DEC     =   ZERO
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    FUNCTION TRIM (WRK-ZERO-SUP)
                                           TO  WRK-PRF-USR
           ELSE
               MOVE    WRK-DECIMAL-INT     TO  WRK-ZERO-SUP-N
               MOVE    WRK-ZERO-SUP        TO  WRK-PRF-INT
               MOVE    WRK-DECIMAL-DEC     TO  WRK-ZERO-SUP-N
               IF      FLG-GUID            =   4
                   MOVE    WRK-ZERO-SUP (4:2)  TO  WRK-PRF-DEC
               ELSE
                   MOVE    WRK-ZERO-SUP (4:)   TO  WRK-PRF-DEC
               END-IF
               MOVE    "."                 TO  WRK-PRF-POINT
               MOVE    FUNCTION TRIM (WRK-PRF-DECIMAL)
                                           TO  WRK-PRF-USR
           END-IF
           IF      FLG-GUID            =   4
               MOVE    SPACE               TO  WRK-PRF-MST
           END-IF
           .
       800-DECIMAL-EXT.
           EXIT.
      *****************************************************************
      *    マスタ点数テーブル読込
      *****************************************************************
       900-SRYCDLIST-SELECT-SEC    SECTION.
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu_org"     TO  MCP-TABLE
           MOVE    "srycdlist"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SRYCDLIST-FETCH-SEC
           END-IF
           .
       900-SRYCDLIST-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    マスタ点数テーブル読込
      *****************************************************************
       900-SRYCDLIST-FETCH-SEC     SECTION.
           INITIALIZE  MCPDATA-REC
           MOVE    "tbl_tensu_org"     TO  MCP-TABLE
           MOVE    "srycdlist"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    TNS-SRYCD           TO  WRK-SRYCD
               ADD     1                   TO  CNT-SRYCDLIST
               MOVE    ZERO                TO  FLG-END
               IF      CNT-SRYCDLIST (4:3) =   "000"
                   MOVE    CNT-SRYCDLIST       TO  WRK-MSG-CNT
                   DISPLAY "*(ORCBMIGTENSU)* org    CNT [ "
                           WRK-MSG-CNT " ]"
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SRYCDLIST-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー点数テーブルレコードチェック
      *****************************************************************
       900-TENSUUSER-READ-SEC      SECTION.
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-TENSU
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU-USER
               ELSE
                   MOVE    1                   TO  FLG-TENSU-USER
               END-IF
           END-IF
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-TENSUUSER-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込（tbl_tensu_org:移行前の点数テーブル）
      *****************************************************************
       9001-TENSU-SAVE-SEC         SECTION.
           INITIALIZE  ORG-TENSU-AREA
           MOVE    ZERO                TO  WRK-OCNT
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu_org"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-TENSU
               MOVE    ZERO                TO  IDM
               PERFORM UNTIL   FLG-TENSU   =   1
                   MOVE    "tbl_tensu_org"     TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                       ADD     1                   TO  IDM
                       IF      IDM                 >   30
                           MOVE    "ORG-TENSU-BODY"
                                                   TO  WRK-ARRAYNAME
                           PERFORM 900-ARRAYABORT-SEC
                       ELSE
                           MOVE    TNS-TENSU-REC       TO
                                                   ORG-TENSU-BODY (IDM)
                           IF      IDM                 =   1
                               MOVE    TNS-YUKOSTYMD       TO
                                                   WRK-YUKOSTYMD-COMP
                           END-IF
                       END-IF
                   ELSE
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               END-PERFORM
               MOVE    IDM                 TO  WRK-OCNT
           END-IF
           MOVE    "tbl_tensu_org"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       9001-TENSU-SAVE-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込（tbl_tensu_master:標準提供の点数テーブル）
      *****************************************************************
       9002-TENSU-SAVE-SEC         SECTION.
           INITIALIZE  MST-TENSU-AREA
           MOVE    ZERO                TO  WRK-MCNT
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-TENSU
               MOVE    ZERO                TO  IDM
               PERFORM UNTIL   FLG-TENSU   =   1
                   MOVE    "tbl_tensu_master"  TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
      *                移行前に過日期間のレコードを削除されていた対応
                       IF      WRK-YUKOSTYMD-COMP  >   TNS-YUKOEDYMD
                           CONTINUE
                       ELSE
                           ADD     1                   TO  IDM
                           IF      IDM                 >   30
                               MOVE    "MST-TENSU-BODY"
                                                       TO  WRK-ARRAYNAME
                               PERFORM 900-ARRAYABORT-SEC
                           ELSE
                               MOVE    TNS-TENSU-REC       TO
                                                   MST-TENSU-BODY (IDM)
                           END-IF
      * 特定器材　金額を点数金額へ登録するマスタであるか判定
                           IF     (TNS-SRYCD(1:1)      =   "7")  AND
                                 ((TNS-TENSIKIBETU     =   2)    OR
                                  (TNS-SANSOKBN        =   2 OR 3 OR 4
                                                        OR 5 OR 9))
                               MOVE    1               TO  FLG-TEN2PRICE
                           END-IF
                       END-IF
                   ELSE
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               END-PERFORM
               MOVE    IDM                 TO  WRK-MCNT
           END-IF
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       9002-TENSU-SAVE-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"       USING   MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       900-TENSU-READ-EXT.
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
      *    レコード抽出処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    レコード挿入処理
      *****************************************************************
       900-INSERT-SEC              SECTION.
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    レコード削除処理
      *****************************************************************
       900-DELETE-SEC              SECTION.
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢオープン処理
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
      *    ＤＢクローズ処理
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
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *****************************************************************
      *    テーブル更新エラー強制終了処理
      *****************************************************************
       900-DBABORT-SEC             SECTION.
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBMIGTENSU: "        DELIMITED BY SIZE
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
      *****************************************************************
      *    配列オーバー強制終了処理
      *****************************************************************
       900-ARRAYABORT-SEC          SECTION.
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBMIGTENSU: "        DELIMITED BY SIZE
                   "index over ERROR [ "   DELIMITED BY SIZE
                   WRK-ARRAYNAME           DELIMITED BY SPACE
                   " ]"                    DELIMITED BY SIZE
                   LOW-VALUE               DELIMITED BY SIZE
               INTO  WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       900-ARRAYABORT-EXT.
           EXIT.
