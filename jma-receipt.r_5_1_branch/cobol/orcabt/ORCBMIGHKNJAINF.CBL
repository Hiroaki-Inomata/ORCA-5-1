       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBMIGHKNJAINF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : セットアップ処理
      *  コンポーネント名  : 保険者マスタ更新
      *                    : 保険者マスタ分離に伴いユーザーが訂正
      *                    : した内容についてはユーザー用のテーブルとする
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/10/31    NACL-伊藤     新規作成
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
           03  FLG-HKNJAINFM           PIC 9(01).
           03  FLG-HKNJAINFU           PIC 9(01).
           03  FLG-HKNJAINFO           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-HKNJAINFO           PIC 9(06).
           03  CNT-HKNJAINFM           PIC 9(06).
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
           03  TBL-COMP                PIC 9(01) OCCURS    16.
      *    プルーフ編集領域
       01  PRF-AREA.
           03  PRF-HEADER1.
               05  FILLER              PIC X(41) VALUE
                   "保険者マスタ移行処理【ORCBMIGHKNJAINF】".
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
               05  FILLER              PIC X(10) VALUE "保険者番号".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(16) VALUE
                               "ユーザ設定保険者".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(14) VALUE
                               "標準提供保険者".
               05  FILLER              PIC X(01) VALUE ",".
               05  FILLER              PIC X(04) VALUE "内容".
           03  PRF-DETAIL.
               05  PRF-COMMAND         PIC X(20).
               05  PRF-HKNJANUM        PIC X(08).
               05  PRF-HKNJANAMEU      PIC X(200).
               05  PRF-HKNJANAMEM      PIC X(200).
               05  PRF-COMMENT         PIC X(200).
       01  CONST-AREA.
           03  CONST-COMM1             PIC X(40) VALUE
           "ユーザ設定保険者の内容に変更されました。".
           03  CONST-COMM2.
               05  CONST-COMM2-1       PIC X(38) VALUE
               "ユーザ設定保険者の内容を追加されました".
               05  CONST-COMM2-2       PIC X(28) VALUE
               "（標準提供にはありません）。".
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
      *    保険者移行前テーブル
       01  HKNJAINFO-REC.
           COPY    "CPHKNJAINF.INC"    REPLACING
                                       //HKNJA-// BY //HKNJAO-//.
      *    保険者マスターテーブル
       01  HKNJAINFM-REC.
           COPY    "CPHKNJAINF.INC"    REPLACING
                                       //HKNJA-// BY //HKNJAM-//.
      *    保険者ユーザーテーブル
       01  HKNJAINFU-REC.
           COPY    "CPHKNJAINF.INC"    REPLACING
                                       //HKNJA-// BY //HKNJAU-//.
      *    保険者ワーク
       01  HKNJAINFW-REC.
           COPY    "CPHKNJAINF.INC"    REPLACING
                                       //HKNJA-// BY //HKNJAW-//.
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
           DISPLAY "*(ORCBMIGHKNJAINF)* 保険者移行処理開始"
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
               DISPLAY "*(ORCBMIGHKNJAINF)* DATABASE OPEN ERROR"
               MOVE    1                   TO  FLG-END
           END-IF
      *
           IF      FLG-END             =   ZERO
               OPEN    OUTPUT  PRF-FILE
               IF      STS-PRF         NOT =   ZERO
                   DISPLAY "*(ORCBMIGHKNJAINF)* PROOF FILE OPEN ERROR "
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
                       DISPLAY "*(ORCBMIGHKNJAINF)* [ "
                               FUNCTION TRIM(SYS-1001-HOSPNAME) " ]"
                       DISPLAY " "
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-HKNJAINFO-SELECT-SEC
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
           PERFORM 900-HKNJAINFM-READ-SEC
           IF      FLG-HKNJAINFM       =   ZERO
      *        標準と項目別比較、相違の場合ユーザーテーブルに追加
               PERFORM 220-COMPARE-SEC
               IF      TBL-COMP-AREA       =   ZERO
                   MOVE    ZERO                TO  WRK-PRFCD
               ELSE
                   MOVE    HKNJAINFO-REC       TO  HKNJAINFW-REC
                   PERFORM 210-HKNJAINFU-INSERT-SEC
                   IF      WRK-PRFCD           =   1
                       MOVE    2                   TO  WRK-PRFCD
                   END-IF
               END-IF
           ELSE
      *        マスターになし、ユーザーテーブルに追加
               MOVE    HKNJAINFO-REC       TO  HKNJAINFW-REC
               PERFORM 210-HKNJAINFU-INSERT-SEC
           END-IF
           PERFORM 290-PRF-SEC
           PERFORM 900-HKNJAINFO-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    保険者ユーザーテーブル追加
      *****************************************************************
       210-HKNJAINFU-INSERT-SEC    SECTION.
           PERFORM 900-HKNJAINFU-READ-SEC
           IF      FLG-HKNJAINFU       =   ZERO
      *        ユーザーテーブルに有る
               MOVE    ZERO                TO  WRK-PRFCD
           ELSE
      *        更新日は元のままとする
      *        MOVE    LNK-MNTYMD          TO  CHGW-CREYMD
      *                                        CHGW-UPYMD
      *        MOVE    LNK-MNTHMS          TO  CHGW-UPHMS
               MOVE    HKNJAINFW-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_hknjainf_user" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    1               TO  WRK-PRFCD
               ELSE
                   MOVE    91              TO  WRK-PRFCD
               END-IF
           END-IF
           .
       210-HKNJAINFU-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    項目別比較
      *****************************************************************
       220-COMPARE-SEC             SECTION.
           MOVE    ZERO                TO  TBL-COMP-AREA
      *
      *保険者番号
      *保険者名
           IF      HKNJAO-HKNJANAME
                                   NOT =   HKNJAM-HKNJANAME
               MOVE    1                   TO  TBL-COMP (1)
           END-IF
      *保険者名略称１
           IF      HKNJAO-HKNJANAME-TAN1
                                   NOT =   HKNJAM-HKNJANAME-TAN1
               MOVE    1                   TO  TBL-COMP (2)
           END-IF
      *保険者名略称２
           IF      HKNJAO-HKNJANAME-TAN2
                                   NOT =   HKNJAM-HKNJANAME-TAN2
               MOVE    1                   TO  TBL-COMP (3)
           END-IF
      *保険者名略称３
           IF      HKNJAO-HKNJANAME-TAN3
                                   NOT =   HKNJAM-HKNJANAME-TAN3
               MOVE    1                   TO  TBL-COMP (4)
           END-IF
      *保険番号
           IF      HKNJAO-HKNNUM   NOT =   HKNJAM-HKNNUM
               MOVE    1                   TO  TBL-COMP (5)
           END-IF
      *給付割合（本人・外来）
           IF      HKNJAO-HON-GAIKYURATE
                                   NOT =   HKNJAM-HON-GAIKYURATE
               MOVE    1                   TO  TBL-COMP (6)
           END-IF
      *給付割合（本人・入院）
           IF      HKNJAO-HON-NYUKYURATE
                                   NOT =   HKNJAM-HON-NYUKYURATE
               MOVE    1                   TO  TBL-COMP (7)
           END-IF
      *給付割合（家族・外来）
          IF      HKNJAO-KZK-GAIKYURATE
                                   NOT =   HKNJAM-KZK-GAIKYURATE
               MOVE    1                   TO  TBL-COMP (8)
           END-IF
      *給付割合（家族・入院）
           IF      HKNJAO-KZK-NYUKYURATE
                                   NOT =   HKNJAM-KZK-NYUKYURATE
               MOVE    1                   TO  TBL-COMP (9)
           END-IF
      *郵便番号
           IF      HKNJAO-POST     NOT =   HKNJAM-POST
               MOVE    1                   TO  TBL-COMP(10)
           END-IF
      *住所
           IF      HKNJAO-ADRS     NOT =   HKNJAM-ADRS
               MOVE    1                   TO  TBL-COMP(11)
           END-IF
      *番地方書
           IF      HKNJAO-BANTI    NOT =   HKNJAM-BANTI
               MOVE    1                   TO  TBL-COMP(12)
           END-IF
      *電話番号
           IF      HKNJAO-TEL      NOT =   HKNJAM-TEL
               MOVE    1                   TO  TBL-COMP(13)
           END-IF
      *記号
           IF      HKNJAO-KIGO     NOT =   HKNJAM-KIGO
               MOVE    1                   TO  TBL-COMP(14)
           END-IF
      *異動区分
           IF      HKNJAO-IDOKBN   NOT =   HKNJAM-IDOKBN
               MOVE    1                   TO  TBL-COMP(15)
           END-IF
      *異動年月日
           IF      HKNJAO-IDOYMD   NOT =   HKNJAM-IDOYMD
               MOVE    1                   TO  TBL-COMP(16)
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
                   MOVE    HKNJAW-HKNJANUM     TO  PRF-HKNJANUM
                   MOVE    HKNJAW-HKNJANAME    TO  PRF-HKNJANAMEU
               WHEN    2
                   MOVE    "変更"              TO  PRF-COMMAND
                   MOVE    CONST-COMM1         TO  PRF-COMMENT
                   MOVE    HKNJAW-HKNJANUM     TO  PRF-HKNJANUM
                   MOVE    HKNJAW-HKNJANAME    TO  PRF-HKNJANAMEU
                   MOVE    HKNJAM-HKNJANAME    TO  PRF-HKNJANAMEM
               WHEN    91
                   MOVE    "追加エラー"        TO  PRF-COMMAND
                   MOVE    HKNJAW-HKNJANUM     TO  PRF-HKNJANUM
                   MOVE    HKNJAW-HKNJANAME    TO  PRF-HKNJANAMEU
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
                   PRF-HKNJANUM        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-HKNJANAMEU      DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   PRF-HKNJANAMEM      DELIMITED BY SPACE
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
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           CLOSE   PRF-FILE
           DISPLAY " "
           MOVE    CNT-PRF             TO  WRK-MSG-CNT
           DISPLAY "*(ORCBMIGHKNJAINF)* Proof Data [ " WRK-MSG-CNT " ]"
           DISPLAY " "
           DISPLAY "*(ORCBMIGHKNJAINF)* 保険者移行処理終了"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    保険者移行前テーブル読込
      *****************************************************************
       900-HKNJAINFO-SELECT-SEC    SECTION.
           INITIALIZE  HKNJAINFO-REC
           MOVE    SPA-HOSPNUM         TO  HKNJAO-HOSPNUM
           MOVE    HKNJAINFO-REC       TO  MCPDATA-REC
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-HKNJAINFO-FETCH-SEC
           END-IF
           .
       900-HKNJAINFO-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    保険者移行前テーブル読込
      *****************************************************************
       900-HKNJAINFO-FETCH-SEC     SECTION.
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINFO-REC
               ADD     1                   TO  CNT-HKNJAINFO
               MOVE    ZERO                TO  FLG-END
               IF      CNT-HKNJAINFO (4:3) =   "000"
                   MOVE    CNT-HKNJAINFO       TO  WRK-MSG-CNT
                   DISPLAY "*(ORCBMIGHKNJAINF)* org    CNT [ "
                           WRK-MSG-CNT " ]"
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-HKNJAINFO-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスタテーブル読込
      *****************************************************************
       900-HKNJAINFM-READ-SEC      SECTION.
           INITIALIZE  HKNJAINFM-REC
           MOVE    HKNJAO-HKNJANUM     TO  HKNJAM-HKNJANUM
           MOVE    SPA-HOSPNUM         TO  HKNJAM-HOSPNUM
           MOVE    HKNJAINFM-REC       TO  MCPDATA-REC
           MOVE    "tbl_hknjainf_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-HKNJAINFM
               MOVE    "tbl_hknjainf_master"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNJAINFM-REC
                   MOVE    ZERO                TO  FLG-HKNJAINFM
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINFM
               END-IF
           END-IF
           MOVE    "tbl_hknjainf_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNJAINFM-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険者ユーザーテーブル読込
      *****************************************************************
       900-HKNJAINFU-READ-SEC      SECTION.
           INITIALIZE  HKNJAINFU-REC
           MOVE    HKNJAW-HKNJANUM     TO  HKNJAU-HKNJANUM
           MOVE    SPA-HOSPNUM         TO  HKNJAU-HOSPNUM
           MOVE    HKNJAINFU-REC       TO  MCPDATA-REC
           MOVE    "tbl_hknjainf_user" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-HKNJAINFU
               MOVE    "tbl_hknjainf_user" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNJAINFU-REC
                   MOVE    ZERO                TO  FLG-HKNJAINFU
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINFU
               END-IF
           END-IF
           MOVE    "tbl_hknjainf_user" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNJAINFU-READ-EXT.
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
           STRING  "*(ORCBMIGHKNJAINF)* "  DELIMITED BY SIZE
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
