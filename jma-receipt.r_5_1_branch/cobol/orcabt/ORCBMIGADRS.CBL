       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBMIGADRS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : セットアップ処理
      *  コンポーネント名  : 住所マスタ更新
      *                    : 住所マスタ分離に伴いユーザーが訂正
      *                    : した内容についてはユーザー用のテーブルとする
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/10/27    NACL-伊藤     新規作成
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
           03  FLG-ADRSM               PIC 9(01).
           03  FLG-ADRSU               PIC 9(01).
           03  FLG-ADRSO               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ADRSO               PIC 9(06).
           03  CNT-ADRSM               PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
      *    一時領域
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
      *    比較テーブル
       01  TBL-COMP-AREA.
           03  TBL-COMP                PIC 9(01) OCCURS    11.
      *    プルーフ編集領域
       01  PRF-AREA.
           03  PRF-HEADER1.
               05  FILLER              PIC X(35) VALUE
                   "住所マスタ移行処理【ORCBMIGADRS】".
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
               05  FILLER              PIC X(10) VALUE "公共コード".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(08) VALUE "郵便番号".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "枝番".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(14) VALUE
                               "ユーザ設定住所".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(12) VALUE
                               "標準提供住所".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "内容".
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
           "ユーザ設定住所の内容に変更されました。".
           03  CONST-COMM2.
               05  CONST-COMM2-1       PIC X(36) VALUE
               "ユーザ設定住所の内容を追加されました".
               05  CONST-COMM2-2       PIC X(28) VALUE
               "（標準提供にはありません）。".
      *****************************************************************
      *    サブプロ用定義
      *****************************************************************
      *    マシン日付取得
           COPY    "CPORCSMCNDATE.INC".
      *    一時ファイル名編集
      *    COPY    "CPORCSGETTEMP.INC".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *    住所移行前テーブル
       01  ADRSO-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSO-//.
      *    住所マスターテーブル
       01  ADRSM-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSM-//.
      *    住所ユーザーテーブル
       01  ADRSU-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSU-//.
      *    住所ワーク
       01  ADRSW-REC.
           COPY    "CPADRS.INC"        REPLACING
                                       //ADRS-// BY //ADRSW-//.
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
           DISPLAY "*(ORCBMIGADRS)* 住所移行処理開始"
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
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
           INITIALIZE  PRF-DETAIL
      *    マスターテーブル存在確認
           PERFORM 900-ADRSM-READ-SEC
           IF      FLG-ADRSM           =   ZERO
      *        標準と項目別比較、相違の場合ユーザーテーブルに追加
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
      *        マスターになし、ユーザーテーブルに追加
               MOVE    ADRSO-REC           TO  ADRSW-REC
               PERFORM 210-ADRSU-INSERT-SEC
           END-IF
           PERFORM 290-PRF-SEC
           PERFORM 900-ADRSO-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    住所ユーザーテーブル追加
      *****************************************************************
       210-ADRSU-INSERT-SEC        SECTION.
           PERFORM 900-ADRSU-READ-SEC
           IF      FLG-ADRSU           =   ZERO
      *        ユーザーテーブルに有る
               MOVE    ZERO                TO  WRK-PRFCD
           ELSE
      *        更新日は元のままとする
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
      *    項目別比較
      *****************************************************************
       220-COMPARE-SEC             SECTION.
           MOVE    ZERO                TO  TBL-COMP-AREA
      *
      *地方公共団体コード
      *郵便番号
      *枝番
      *都道府県名カナ
           IF      ADRSO-PREFKANA  NOT =   ADRSM-PREFKANA
               MOVE    1                   TO  TBL-COMP (1)
           END-IF
      *市町村名カナ
           IF      ADRSO-CITYKANA  NOT =   ADRSM-CITYKANA
               MOVE    1                   TO  TBL-COMP (2)
           END-IF
      *町域名カナ
           IF      ADRSO-TOWNKANA  NOT =   ADRSM-TOWNKANA
               MOVE    1                   TO  TBL-COMP (3)
           END-IF
      *都道府県名
           IF      ADRSO-PREFNAME  NOT =   ADRSM-PREFNAME
               MOVE    1                   TO  TBL-COMP (4)
           END-IF
      *市町村名
           IF      ADRSO-CITYNAME  NOT =   ADRSM-CITYNAME
               MOVE    1                   TO  TBL-COMP (5)
           END-IF
      *町域名
           IF      ADRSO-TOWNNAME  NOT =   ADRSM-TOWNNAME
               MOVE    1                   TO  TBL-COMP (6)
           END-IF
      *編集後住所カナ
           IF      ADRSO-EDITADRS-KANA
                                   NOT =   ADRSM-EDITADRS-KANA
               MOVE    1                   TO  TBL-COMP (7)
           END-IF
      *編集後住所
           IF      ADRSO-EDITADRS-NAME
                                   NOT =   ADRSM-EDITADRS-NAME
               MOVE    1                   TO  TBL-COMP (8)
           END-IF
      *町域分割フラグ
           IF      ADRSO-TOWNDIVFLG
                                   NOT =   ADRSM-TOWNDIVFLG
               MOVE    1                   TO  TBL-COMP (9)
           END-IF
      *複数町域フラグ
           IF      ADRSO-MANYTOWNFLG
                                   NOT =   ADRSM-MANYTOWNFLG
               MOVE    1                   TO  TBL-COMP (10)
           END-IF
      *補足（通り名）
           IF      ADRSO-TOWNNAME2 NOT =   ADRSM-TOWNNAME2
               MOVE    1                   TO  TBL-COMP (11)
           END-IF
           .
       220-COMPARE-EXT.
           EXIT.
      *****************************************************************
      *    プルーフ出力処理
      *****************************************************************
       290-PRF-SEC                 SECTION.
           EVALUATE    WRK-PRFCD
               WHEN    1
                   MOVE    "追加"              TO  PRF-COMMAND
                   MOVE    CONST-COMM2         TO  PRF-COMMENT
                   MOVE    ADRSW-LPUBCD        TO  PRF-LPUBCD
                   MOVE    ADRSW-POST (1:3)    TO  PRF-POST1
                   MOVE    "-"                 TO  PRF-POSTH
                   MOVE    ADRSW-POST (4:4)    TO  PRF-POST2
                   MOVE    ADRSW-EDANUM        TO  PRF-EDANUM
                   MOVE    ADRSW-EDITADRS-NAME TO  PRF-ADRS-NAMEU
               WHEN    2
                   MOVE    "変更"              TO  PRF-COMMAND
                   MOVE    CONST-COMM1         TO  PRF-COMMENT
                   MOVE    ADRSW-LPUBCD        TO  PRF-LPUBCD
                   MOVE    ADRSW-POST (1:3)    TO  PRF-POST1
                   MOVE    "-"                 TO  PRF-POSTH
                   MOVE    ADRSW-POST (4:4)    TO  PRF-POST2
                   MOVE    ADRSW-EDANUM        TO  PRF-EDANUM
                   MOVE    ADRSW-EDITADRS-NAME TO  PRF-ADRS-NAMEU
                   MOVE    ADRSM-EDITADRS-NAME TO  PRF-ADRS-NAMEM
               WHEN    91
                   MOVE    "追加エラー"        TO  PRF-COMMAND
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
      *    終了処理
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
           DISPLAY "*(ORCBMIGADRS)* プルーフ [ " WRK-MSG-CNT " ]"
           DISPLAY " "
           DISPLAY "*(ORCBMIGADRS)* 住所移行処理終了"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    住所移行前テーブル読込
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
      *    住所移行前テーブル読込
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
      *    住所マスタテーブル読込
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
      *    住所ユーザーテーブル読込
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
