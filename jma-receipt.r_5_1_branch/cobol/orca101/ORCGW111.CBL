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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGW111.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 帳票オフセット登録
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/03/08    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *****************************************************************
      *
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W11SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SELNUM          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-WIDMSG              PIC X(70).
      *
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-KBNCD               PIC 9(08).
           03  WRK-ZZ9                 PIC ZZ9.
      *
      *    コラムリスト位置
           03  WRK-W111-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    帳票オフセット情報
           COPY  "CPSK1044.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W11
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W11         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "W111"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *            更新
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2201-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W111-CUR
                   END-IF
               WHEN    "1003"
      *            削除
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W111-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W111-AREA
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-W111-LIST-G
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                      SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1044-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1044-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  (FLG-SYSKANRI    =   1 )  OR
                          (IDX             >=  100)
               MOVE    MCPDATA-REC         TO  SYS-1044-REC
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-W111-LIST-MAX
               MOVE    IDX                 TO  SPA-W111-TRENNUM (IDX)
               MOVE    SYS-1044-KBNCD      TO  SPA-W111-NAI-KBNCD (IDX)
               MOVE    SYS-1044-FORMID     TO  SPA-W111-TFORMID (IDX)
               MOVE    SYS-1044-PRTNM      TO  SPA-W111-TPRTNM (IDX)
               MOVE    SYS-1044-OFFSET-X   TO  SPA-W111-TOFFSET-X (IDX)
               MOVE    SYS-1044-OFFSET-Y   TO  SPA-W111-TOFFSET-Y (IDX)
      *        最大区分番号
               IF     (SPA-W111-KBNCD-MAX  =   ZERO )  OR
                      (SPA-W111-KBNCD-MAX  <   SYS-1044-KBNCD)
                   MOVE    SYS-1044-KBNCD      TO  SPA-W111-KBNCD-MAX
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-W111-LIST-MAX   =   ZERO
               MOVE    2                   TO  SPA-W111-CUR
           ELSE
               MOVE    1                   TO  SPA-W111-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-W111-ROW
           ELSE
               MOVE    W111-MEILIST-ROW    TO  WRK-W111-ROW
           END-IF
      *
           MOVE    SPACE               TO  W111
      *    コラムリストの位置指定
           INITIALIZE                      W111
           MOVE    ZERO                TO  W111-MEILIST-ROW
           IF      WRK-W111-ROW    NOT =   ZERO
               MOVE    WRK-W111-ROW    TO  W111-MEILIST-ROW 
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W111-LIST-MAX
               MOVE    SPA-W111-TRENNUM (IDX)  TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  W111-TRENNUM (IDX)
               MOVE    SPA-W111-TFORMID(IDX)
                                           TO  W111-TFORMID (IDX)
               MOVE    SPA-W111-TPRTNM (IDX)
                                           TO  W111-TPRTNM (IDX)
               MOVE    SPA-W111-TOFFSET-X (IDX)
                                           TO  W111-TOFFSET-X (IDX)
               MOVE    SPA-W111-TOFFSET-Y (IDX)
                                           TO  W111-TOFFSET-Y (IDX)
               IF      SPA-W111-SELNUM     =   SPA-W111-TRENNUM (IDX)
                   MOVE    "T"             TO  W111-MEILIST-SELECT
                                                               (IDX)
                   MOVE    IDX             TO  W111-MEILIST-ROW
                   MOVE    2               TO  W111-MEILIST-ROWATTR
               ELSE
                   MOVE    "F"             TO  W111-MEILIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-W111-LIST-MAX    TO  W111-MEILIST-COUNT
      *
           MOVE    SPA-W111-SELNUM      TO  W111-SELNUM
           MOVE    SPA-W111-FORMID      TO  W111-FORMID
           MOVE    SPA-W111-PRTNM       TO  W111-PRTNM
           MOVE    SPA-W111-OFFSET-X    TO  W111-OFFSET-X
           MOVE    SPA-W111-OFFSET-Y    TO  W111-OFFSET-Y
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-W111-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "SELNUM"
                       MOVE    2                   TO  SPA-W111-CUR
                   WHEN    "FORMID"
                       MOVE    3                   TO  SPA-W111-CUR
                   WHEN    "PRTNM"
                       MOVE    4                   TO  SPA-W111-CUR
                   WHEN    "OFFSET_X"
                       MOVE    5                   TO  SPA-W111-CUR
                   WHEN    "OFFSET_Y"
                       MOVE    99                  TO  SPA-W111-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W111-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "FORMID"            TO  MCP-WIDGET
               WHEN    3
                   MOVE    "PRTNM"            TO  MCP-WIDGET
               WHEN    4
                   MOVE    "OFFSET_X"          TO  MCP-WIDGET
               WHEN    5
                   MOVE    "OFFSET_Y"          TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLERA-SYORI-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-DELETE-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
           EVALUATE    WRK-WIDGET
               WHEN    "MEILIST"
                   PERFORM 2001-MEILIST-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 220-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-W111-CUR
      *    入力項目の決定
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2201-GMNSPA-SET-SEC
      *
           PERFORM 2002-SELNUM-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2202-FORMID-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2203-PRTNM-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2204-OFFSETX-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2205-OFFSETY-CHK-SEC
           END-IF
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト選択処理
      *****************************************************************
       2001-MEILIST-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-W111-LIST-MAX
              IF      W111-MEILIST-SELECT (IDX)    =  "T"
                  IF       IDX                 =   SPA-W111-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX              TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-W111-SELNUM
               MOVE    1                   TO  FLG-SELNUM
               PERFORM 2002-SELNUM-SEC
           END-IF
      *
           .
       2001-HKNCOMBI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       2002-SELNUM-SEC               SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-W111-SELNUM     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-W111-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-W111-SELNUM
           END-IF
      *
           IF      (SPA-W111-SELNUM    >   ZERO)  AND
                   (SPA-ERRCD          =   SPACE) 
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-W111-LIST-MAX ) OR
                                  (FLG-CHK =   1    )
                   IF      SPA-W111-TRENNUM (IDX)   =   SPA-W111-SELNUM
                       MOVE    1                   TO  FLG-CHK
                       IF      FLG-SELNUM          =   1
      *                    明細編集
                           PERFORM 20021-MEISAI-HENSYU-SEC
                       END-IF
                       MOVE    SPA-W111-SELNUM     TO
                                                   SPA-W111-MAE-SELNUM
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W111-CUR
               END-IF
           END-IF
      *
           .
       2002-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20021-MEISAI-HENSYU-SEC               SECTION.
      *
           MOVE    SPA-W111-TFORMID (IDX)   TO  SPA-W111-FORMID
           MOVE    SPA-W111-TPRTNM  (IDX)   TO  SPA-W111-PRTNM
           MOVE    SPA-W111-TOFFSET-X (IDX) TO  SPA-W111-OFFSET-X
           MOVE    SPA-W111-TOFFSET-Y (IDX) TO  SPA-W111-OFFSET-Y
      *
           .
       20021-MEISAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           IF      W111-SELNUM         =   SPA-W111-MAE-SELNUM
               MOVE    ZERO                TO  FLG-SELNUM
           ELSE
               MOVE    1                   TO  FLG-SELNUM
           END-IF
           MOVE    W111-SELNUM          TO  SPA-W111-SELNUM
           MOVE    W111-FORMID          TO  SPA-W111-FORMID
           MOVE    W111-PRTNM           TO  SPA-W111-PRTNM
           MOVE    W111-OFFSET-X        TO  SPA-W111-OFFSET-X
           MOVE    W111-OFFSET-Y        TO  SPA-W111-OFFSET-Y
      *
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票ＩＤチェック処理
      *****************************************************************
       2202-FORMID-CHK-SEC            SECTION.
      *
      *
           .
       2202-FORMID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ名チェック処理
      *****************************************************************
       2203-PRTNM-CHK-SEC            SECTION.
      *
      *
           .
       2203-PRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｘ方法チェック処理
      *****************************************************************
       2204-OFFSETX-CHK-SEC            SECTION.
      *
           IF      SPA-W111-OFFSET-X   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-W111-OFFSET-X   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   2     )   OR
                      (SNUM-SYOSUU         >   1     )   OR
                      (SNUM-KETSUU         =   ZERO  )
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-W111-CUR
               END-IF
           END-IF
      *
           .
       2204-OFFSETX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ｙ方法チェック処理
      *****************************************************************
       2205-OFFSETY-CHK-SEC            SECTION.
      *
           IF      SPA-W111-OFFSET-Y   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-W111-OFFSET-Y   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   2     )   OR
                      (SNUM-SYOSUU         >   1     )   OR
                      (SNUM-KETSUU         =   ZERO  )
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-W111-CUR
               END-IF
           END-IF
      *
           .
       2205-OFFSETY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLERA-SYORI-SEC                   SECTION.
      *
           INITIALIZE                  SPA-W111-GMN-G
           MOVE    ZERO                TO  SPA-W111-MAE-SELNUM
      *
           IF      SPA-W111-LIST-MAX   =   ZERO
               MOVE    2                   TO  SPA-W111-CUR
           ELSE
               MOVE    1                   TO  SPA-W111-CUR
           END-IF
      *
           .
       220-CLERA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       230-DELETE-SEC                   SECTION.
      *
           PERFORM 2002-SELNUM-SEC
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-W111-SELNUM     >   ZERO
                   MOVE    "1003"          TO  SPA-WIDCD
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W111-CUR
               END-IF
           END-IF
      *
           .
       230-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2301-DELETE-SYORI-SEC                   SECTION.
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                      SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    SPA-W111-NAI-KBNCD(SPA-W111-SELNUM)
                                       TO  SYS-1044-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1044-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1044-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1044-REC
               MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
               MOVE    SYS-1044-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "9001"              TO  SPA-ERRCD
                   DISPLAY "W111 DEL SYSKANRI-KEY =>" SYS-1044-KEY 
                           ",MCP-RC:" MCP-RC 
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       2301-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       220-TOUROKU-SEC                   SECTION.
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *    更新前
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2201-ALL-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1001"          TO  SPA-WIDCD
           END-IF
      *
           .
       220-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       2201-ALL-CHK-SEC                SECTION.
      *
           IF      SPA-W111-FORMID     =   SPACE
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-W111-CUR
           END-IF
           IF      SPA-W111-PRTNM      =   SPACE
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-W111-CUR
           END-IF
           IF     (SPA-W111-OFFSET-X   =   SPACE )  AND
                  (SPA-W111-OFFSET-Y   =   SPACE )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-W111-CUR
           END-IF
           .
       2201-ALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       2201-TOUROKU-SYORI-SEC                   SECTION.
      *
           PERFORM 800-MCNDATE-SEC
           IF      SPA-W111-SELNUM     =   ZERO
      *        連番号決定
               COMPUTE WRK-KBNCD           =   SPA-W111-KBNCD-MAX
                                           +   1
           ELSE
               MOVE    SPA-W111-NAI-KBNCD(SPA-W111-SELNUM)
                                           TO  WRK-KBNCD
           END-IF
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                      SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-1044-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1044-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1044-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
      *        更新
               MOVE    MCPDATA-REC         TO  SYS-1044-REC
               PERFORM 22011-DBHENSYU-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-1044-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1044-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
               MOVE    SYS-1044-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "9001"              TO  SPA-ERRCD
                   DISPLAY "W111 UPD SYSKANRI-KEY =>" SYS-1044-KEY 
                           ",MCP-RC:" MCP-RC 
               END-IF
           ELSE
      *        新規
               MOVE    SPACE               TO  SYS-1044-REC
               INITIALIZE                      SYS-1044-REC
               MOVE    "1044"              TO  SYS-1044-KANRICD
               MOVE    WRK-KBNCD           TO  SYS-1044-KBNCD
               MOVE    "00000000"          TO  SYS-1044-STYUKYMD
               MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
               PERFORM 22011-DBHENSYU-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-1044-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1044-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1044-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
               MOVE    SYS-1044-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "9001"              TO  SPA-ERRCD
                   DISPLAY "W111 INS SYSKANRI-KEY =>" SYS-1044-KEY 
                           ",MCP-RC:" MCP-RC 
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       2201-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ編集処理
      *****************************************************************
       22011-DBHENSYU-SEC                   SECTION.
      *
           MOVE    SPA-W111-FORMID     TO  SYS-1044-FORMID
           MOVE    SPA-W111-PRTNM      TO  SYS-1044-PRTNM
           MOVE    SPA-W111-OFFSET-X   TO  SYS-1044-OFFSET-X
           MOVE    SPA-W111-OFFSET-Y   TO  SYS-1044-OFFSET-Y
           .
       22011-DBHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W11"               TO  SPA-SAKIPG
           MOVE    "W111"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "W11"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W111"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "Ｘ方向は整数２桁、小数１桁での入力です。"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "Ｙ方向は整数２桁、小数１桁での入力です。"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "選択番号を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "帳票ＩＤを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "プリンタ名を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "Ｘ方向、Ｙ方向のどちらかを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "更新エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W111"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   STRING  "帳票オフセットを登録します。"
                                                   DELIMITED  BY  SIZE
                           "よろしいですか？"      DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "帳票オフセットを削除します。"
                                                   DELIMITED  BY  SIZE
                           "よろしいですか？"      DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
          END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           INITIALIZE                      WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W111"              TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
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
      *    管理マスター読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
