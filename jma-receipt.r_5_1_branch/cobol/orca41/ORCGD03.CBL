      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGD03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 確認項目保存画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.07.20    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    データチェック共通パラメタ
           COPY    "D01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-D03.
           03  SPA-D03-AREA.
               05  SPA-GMN-D03-CUR         PIC 9(03).
               05  SPA-GMN-D03-AREA.
                   07  SPA-GMN-D03-NAME    PIC X(30).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-CHK004                  PIC 9(01).
           03  FLG-KANACHK-ERR             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-PRTKANRI-RENNUM         PIC 9(04).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD-NUM           PIC 9(02).
               05  WRK-KBNCD-FL            PIC X(06).
      *
      *    データチェック確認項目
           COPY    "CPDTCHKKOMOKU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
           COPY    "CPCK2002.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA41SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D03
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D03             TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "DERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "D03"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-D03
      *
           MOVE    1               TO  SPA-GMN-D03-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 220-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-D03-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    D03-NAME        TO  SPA-GMN-D03-NAME
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    登録名チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NAME-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録名チェック処理
      *****************************************************************
       4101-NAME-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-D03-NAME    TO  WRK-KANACHK-STR
           PERFORM 800-ZENKAKU-CHK-SEC
           MOVE    WRK-KANACHK-STR     TO  SPA-GMN-D03-NAME
           IF    ( FLG-KANACHK-ERR     NOT =   ZERO )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-D03-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  CHK004-REC
               MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
               MOVE    "2002"          TO  CHK004-KANRICD
               MOVE    "%"             TO  CHK004-KBNCD
               MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                           CHK004-EDYUKYMD
               PERFORM 900-CHK004-KEY9-SEL-SEC
               MOVE    CHK004-REC      TO  CK4-2002-REC
      *
               PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPA-GMN-D03-NAME    =   CK4-2002-NAME )
                       MOVE    "0002"  TO  SPA-ERRCD
                       MOVE    1       TO  SPA-GMN-D03-CUR
                   END-IF
      *
                   PERFORM 900-CHK004-KEY9-FET-SEC
                   MOVE    CHK004-REC  TO  CK4-2002-REC
      *
               END-PERFORM
      *
               MOVE    "tbl_chk004"    TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       4101-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4900-KANREN-CHK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD              =    SPACE )
               IF    ( SPA-GMN-D03-NAME   =    SPACE )
                   MOVE    "1001" TO   SPA-ERRCD
                   MOVE    1      TO   SPA-GMN-D03-CUR
               END-IF
           END-IF
      *
           .
       4900-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "D01"               TO  SPA-SAKIPG
           MOVE    "D03"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       220-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    (  SPA-ERRCD      NOT =   SPACE )
               GO  TO  220-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANREN-CHK-SEC
           IF    (  SPA-ERRCD      NOT =   SPACE )
               GO  TO  220-TOUROKU-EXT
           END-IF
      *
           INITIALIZE                  WRK-KBNCD-G
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2002"          TO  CHK004-KANRICD
           MOVE    "%"             TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY9-SEL-SEC
           IF    ( FLG-CHK004      =   ZERO )
               MOVE    CHK004-REC  TO  CK4-2002-REC
               MOVE    CK4-2002-KBNCD
                                   TO  WRK-KBNCD-G
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE WRK-KBNCD-NUM
               =   WRK-KBNCD-NUM   +   1
      *
           INITIALIZE                      CK4-2002-REC
      *
           MOVE    SPA-HOSPNUM         TO  CK4-2002-HOSPNUM
           MOVE    "2002"              TO  CK4-2002-KANRICD
           MOVE    WRK-KBNCD-G         TO  CK4-2002-KBNCD
           MOVE    "00000000"          TO  CK4-2002-STYUKYMD
           MOVE    "99999999"          TO  CK4-2002-EDYUKYMD
           MOVE    SPA-GMN-D03-NAME    TO  CK4-2002-NAME
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
               MOVE    SPA-D01-CHKKBN (IDX0)
                                       TO  CK4-2002-CHKKBN (IDX0)
           END-PERFORM
      *
           MOVE    SPA-D01-INGAIPROCKBN
                                       TO  CK4-2002-INGAIPROCKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  CK4-2002-CREYMD
                                           CK4-2002-UPYMD
           MOVE    SMCNDATE-HMS        TO  CK4-2002-UPHMS
      *
           MOVE    CK4-2002-REC        TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
               GO  TO  220-TOUROKU-EXT
           END-IF
      *
           MOVE    WRK-KBNCD-NUM       TO  SPA-D01-USRCHKKBN
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
      *
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "D03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  D03
      *
      *    登録名
           MOVE    SPA-GMN-D03-NAME   TO  D03-NAME
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-D03-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-D03-CUR
           WHEN    001
               MOVE    "NAME"          TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "NAME"
               MOVE    212             TO  SPA-GMN-D03-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "登録名は全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "同じ登録名が既に存在します"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "登録名を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  DERR
           MOVE    SPA-ERRCD            TO  DERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  DERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "D03"                TO  SPA-MOTOPG
           MOVE    "DERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "DERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    全角文字チェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK-ERR
      *
           IF  ( WRK-KANACHK-STR       =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       MOVE    1       TO  FLG-KANACHK-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-KANACHK-ERR
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY9-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４FETCH処理
      *****************************************************************
       900-CHK004-KEY9-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

