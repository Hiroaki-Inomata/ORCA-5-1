       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBMIGSRYCDCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : セットアップ処理
      *  コンポーネント名  : 一般老人置換マスタ更新
      *                    : 一般老人置換マスタ分離に伴いユーザーが訂正
      *                    : した内容についてはユーザー用のテーブルとする
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/ 7/13    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
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
       01  STS-AREA.
           03  STS-PRF                 PIC X(02).
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SRYCDCHGM           PIC 9(01).
           03  FLG-SRYCDCHGU           PIC 9(01).
           03  FLG-SRYCDCHGO           PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCDCHGO           PIC 9(06).
           03  CNT-SRYCDCHGM           PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-LIMIT-PRF  PIC 9(05).
               05  WRK-PARA-FILE       PIC X(256).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-MSG                 PIC X(256).
           03  WRK-MSG-CNT             PIC ZZZ,ZZ9.
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-LIMIT-PRF           PIC 9(05).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-PRFCD               PIC 9(02).
      *    プルーフ編集領域
       01  PRF-AREA.
           03  PRF-HEADER1.
               05  FILLER              PIC X(47) VALUE
                   "一般老人置換マスタ移行処理【ORCBMIGSRYCDCHG】".
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
               05  FILLER              PIC X(04) VALUE "処理".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(10) VALUE "対象コード".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "名称".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(20) VALUE
                               "ユーザ設定置換コード".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "名称".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(18) VALUE
                               "標準提供置換コード".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "名称".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "内容".
           03  PRF-DETAIL.
               05  PRF-COMMAND         PIC X(20).
               05  PRF-CODE1           PIC X(09).
               05  PRF-NAME1           PIC X(200).
               05  PRF-CODE2           PIC X(09).
               05  PRF-NAME2           PIC X(200).
               05  PRF-CODE3           PIC X(09).
               05  PRF-NAME3           PIC X(200).
               05  PRF-COMMENT         PIC X(200).
       01  CONST-AREA.
           03  CONST-COMM1             PIC X(44) VALUE
           "ユーザ設定置換コードの内容に変更されました。".
           03  CONST-COMM2.
               05  CONST-COMM2-1       PIC X(42) VALUE
               "ユーザ設定置換コードの内容を追加されました".
               05  CONST-COMM2-2       PIC X(28) VALUE
               "（標準提供にはありません）。".
           03  CONST-COMM3             PIC X(42) VALUE
           "標準提供置換コードの内容を削除されました。".
      *****************************************************************
      *    サブプロ用定義
      *****************************************************************
      *    マシン日付取得
           COPY    "CPORCSMCNDATE.INC".
      *    一時ファイル名編集
      *     COPY    "CPORCSGETTEMP.INC".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *    老人一般置換移行前テーブル
       01  SRYCDCHGO-REC.
           COPY    "CPSRYCDCHG.INC"    REPLACING
                                       //CHG-// BY //CHGO-//.
      *    老人一般置換マスターテーブル
       01  SRYCDCHGM-REC.
           COPY    "CPSRYCDCHG.INC"    REPLACING
                                       //CHG-// BY //CHGM-//.
      *    老人一般置換ユーザーテーブル
       01  SRYCDCHGU-REC.
           COPY    "CPSRYCDCHG.INC"    REPLACING
                                       //CHG-// BY //CHGU-//.
      *    老人一般置換ワーク
       01  SRYCDCHGW-REC.
           COPY    "CPSRYCDCHG.INC"    REPLACING
                                       //CHG-// BY //CHGW-//.
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
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
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
           PERFORM 300-END-SEC
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           DISPLAY "*(ORCBMIGSRYCDCHG)* 一般老人置換移行処理開始"
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
               DISPLAY "*(ORCBMIGSRYCDCHG)* DATABASE OPEN ERROR"
               MOVE    1                   TO  FLG-END
           END-IF
      *
           IF      FLG-END             =   ZERO
               OPEN    OUTPUT  PRF-FILE
               IF      STS-PRF         NOT =   ZERO
                   DISPLAY "*(ORCBMIGSRYCDCHG)* PROOF FILE OPEN ERROR "
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
                       DISPLAY "*(ORCBMIGSRYCDCHG)* [ "
                               FUNCTION TRIM(SYS-1001-HOSPNAME) " ]"
                       DISPLAY " "
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-SRYCDCHGO-SELECT-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
           INITIALIZE  PRF-DETAIL
      *    マスターテーブル存在確認
           MOVE    "key"               TO  WRK-PATHNAME
           PERFORM 900-SRYCDCHGM-READ-SEC
           IF      FLG-SRYCDCHGM       =   ZERO
      *        標準に有る
               MOVE    ZERO                TO  WRK-PRFCD
           ELSE
      *        マスターテーブル存在確認(IPNSRYCDのみ)
               MOVE    "key2"              TO  WRK-PATHNAME
               PERFORM 900-SRYCDCHGM-READ-SEC
               IF      FLG-SRYCDCHGM       =   ZERO
                   MOVE    CHGM-RJNSRYCD       TO  PRF-CODE3
               END-IF
      *        ユーザーテーブルに追加する
               MOVE    SRYCDCHGO-REC       TO  SRYCDCHGW-REC
               PERFORM 210-SRYCDCHGU-INSERT-SEC
           END-IF
           PERFORM 290-PRF-SEC
           PERFORM 900-SRYCDCHGO-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    一般老人置換ユーザーテーブル追加
      *****************************************************************
       210-SRYCDCHGU-INSERT-SEC    SECTION.
           PERFORM 900-SRYCDCHGU-READ-SEC
           IF      FLG-SRYCDCHGU       =   ZERO
      *        ユーザーテーブルに有る
               MOVE    ZERO                TO  WRK-PRFCD
           ELSE
      *        更新日は元のままとする
      *        MOVE    LNK-MNTYMD          TO  CHGW-CREYMD
      *                                        CHGW-UPYMD
      *        MOVE    LNK-MNTHMS          TO  CHGW-UPHMS
               MOVE    SRYCDCHGW-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_srycdchg_user" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    1               TO  WRK-PRFCD
               ELSE
                   MOVE    91              TO  WRK-PRFCD
               END-IF
           END-IF
           .
       210-SRYCDCHGU-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    プルーフ出力処理
      *****************************************************************
       290-PRF-SEC                 SECTION.
           EVALUATE    WRK-PRFCD
               WHEN    1
                   IF      PRF-CODE3       NOT =   SPACE
                       IF      PRF-CODE2           =   "999999999"
                           MOVE    "削除"              TO  PRF-COMMAND
                           MOVE    CONST-COMM3         TO  PRF-COMMENT
                       ELSE
                           MOVE    "変更"              TO  PRF-COMMAND
                           MOVE    CONST-COMM1         TO  PRF-COMMENT
                       END-IF
                   ELSE
                       MOVE    "追加"              TO  PRF-COMMAND
                       MOVE    CONST-COMM2         TO  PRF-COMMENT
                   END-IF
                   MOVE    CHGW-IPNSRYCD       TO  PRF-CODE1
                   MOVE    CHGW-IPNSRYCD       TO  WRK-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
                       MOVE    TNS-NAME            TO  PRF-NAME1
                   END-IF
                   IF      CHGW-RJNSRYCD   NOT =   "999999999"
                       MOVE    CHGW-RJNSRYCD       TO  PRF-CODE2
                       MOVE    CHGW-RJNSRYCD       TO  WRK-SRYCD
                       PERFORM 900-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           MOVE    TNS-NAME            TO  PRF-NAME2
                       END-IF
                   END-IF
                   IF      PRF-CODE3       NOT =   SPACE
                       MOVE    PRF-CODE3           TO  WRK-SRYCD
                       PERFORM 900-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           MOVE    TNS-NAME            TO  PRF-NAME3
                       END-IF
                   END-IF
               WHEN    91
                   MOVE    "追加エラー"        TO  PRF-COMMAND
                   MOVE    CHGW-IPNSRYCD       TO  PRF-CODE1
                   MOVE    CHGW-IPNSRYCD       TO  WRK-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
                       MOVE    TNS-NAME            TO  PRF-NAME1
                   END-IF
                   IF      CHGW-RJNSRYCD   NOT =   "999999999"
                       MOVE    CHGW-RJNSRYCD       TO  PRF-CODE2
                       MOVE    CHGW-RJNSRYCD       TO  WRK-SRYCD
                       PERFORM 900-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           MOVE    TNS-NAME            TO  PRF-NAME2
                       END-IF
                   END-IF
           END-EVALUATE
           IF      WRK-PRFCD       NOT =   ZERO
               PERFORM 2901-PRF-SEC
           END-IF
           .
       290-PRF-EXT.
           EXIT.
      *****************************************************************
      *    プルーフ出力処理
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
           IF      PRF-CODE2           =   "999999999"
               MOVE    SPACE               TO  PRF-CODE2
           END-IF
           INITIALIZE  PRF-REC
           STRING  PRF-COMMAND         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CODE1           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-NAME1           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CODE2           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-NAME2           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-CODE3           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-NAME3           DELIMITED BY SPACE
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
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           CLOSE   PRF-FILE
           DISPLAY " "
           MOVE    CNT-PRF             TO  WRK-MSG-CNT
           DISPLAY "*(ORCBMIGSRYCDCHG)* Proof Data [ " WRK-MSG-CNT " ]"
           DISPLAY " "
           DISPLAY "*(ORCBMIGSRYCDCHG)* 一般老人置換移行処理終了"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    一般老人置換移行前テーブル読込
      *****************************************************************
       900-SRYCDCHGO-SELECT-SEC    SECTION.
           INITIALIZE  SRYCDCHGO-REC
           MOVE    SPA-HOSPNUM         TO  CHGO-HOSPNUM
           MOVE    SRYCDCHGO-REC       TO  MCPDATA-REC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SRYCDCHGO-FETCH-SEC
           END-IF
           .
       900-SRYCDCHGO-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    一般老人置換移行前テーブル読込
      *****************************************************************
       900-SRYCDCHGO-FETCH-SEC     SECTION.
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHGO-REC
               ADD     1                   TO  CNT-SRYCDCHGO
               MOVE    ZERO                TO  FLG-END
               IF      CNT-SRYCDCHGO (4:3) =   "000"
                   MOVE    CNT-SRYCDCHGO       TO  WRK-MSG-CNT
                   DISPLAY "*(ORCBMIGSRYCDCHG)* org    CNT [ "
                           WRK-MSG-CNT " ]"
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SRYCDCHGO-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    一般老人置換マスタテーブル読込
      *****************************************************************
       900-SRYCDCHGM-READ-SEC      SECTION.
           INITIALIZE  SRYCDCHGM-REC
           MOVE    SPA-HOSPNUM         TO  CHGM-HOSPNUM
           MOVE    CHGO-IPNSRYCD       TO  CHGM-IPNSRYCD
           MOVE    CHGO-RJNSRYCD       TO  CHGM-RJNSRYCD
           MOVE    SRYCDCHGM-REC       TO  MCPDATA-REC
           MOVE    "tbl_srycdchg_master"
                                       TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYCDCHGM
               MOVE    "tbl_srycdchg_master"
                                           TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYCDCHGM-REC
                   MOVE    ZERO                TO  FLG-SRYCDCHGM
               ELSE
                   MOVE    1                   TO  FLG-SRYCDCHGM
               END-IF
           END-IF
           MOVE    "tbl_srycdchg_master"
                                       TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SRYCDCHGM-READ-EXT.
           EXIT.
      *****************************************************************
      *    一般老人置換ユーザーテーブル読込
      *****************************************************************
       900-SRYCDCHGU-READ-SEC      SECTION.
           INITIALIZE  SRYCDCHGU-REC
           MOVE    SPA-HOSPNUM         TO  CHGU-HOSPNUM
           MOVE    CHGW-IPNSRYCD       TO  CHGU-IPNSRYCD
           MOVE    CHGW-RJNSRYCD       TO  CHGU-RJNSRYCD
           MOVE    SRYCDCHGU-REC       TO  MCPDATA-REC
           MOVE    "tbl_srycdchg_user" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYCDCHGU
               MOVE    "tbl_srycdchg_user" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYCDCHGU-REC
                   MOVE    ZERO                TO  FLG-SRYCDCHGU
               ELSE
                   MOVE    1                   TO  FLG-SRYCDCHGU
               END-IF
           END-IF
           MOVE    "tbl_srycdchg_user" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SRYCDCHGU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
           STRING  "*(ORCBMIGSRYCDCHG)* "  DELIMITED BY SIZE
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
