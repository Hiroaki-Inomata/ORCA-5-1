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
       PROGRAM-ID.                 ORCGW321.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 包括診療行為オフ設定情報（Ｗ３２１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11/01/21    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  16/06/XX  包括ＣＤ対象修正
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
           COPY    "W32SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKTSRYCD        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-ETENSU1         PIC 9(01).
           03  FLG-ETENSU2         PIC 9(01).
           03  FLG-ETENSU2-JMA     PIC 9(01).
           03  FLG-ETENSU2-OFF     PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SELNUM          PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-CHKEND          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDC                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-WIDMSG              PIC X(80).
           03  WRK-SDATE               PIC X(06).
           03  WRK-STIME.
               05  WRK-SHH              PIC 9(02).
               05  WRK-SMM              PIC 9(02).
               05  WRK-SSS              PIC 9(02).
      *
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-ZZ9                 PIC ZZZ9.
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-TNS-NAME            PIC X(80).
           03  WRK-TNS-STYMD           PIC X(08).
           03  WRK-TNS-EDYMD           PIC X(08).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *    コラムリスト位置
           03  WRK-W321-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-H-STYMD             PIC X(09).
           03  WRK-H-EDYMD             PIC X(09).
           03  WRK-YUKOSTYMD           PIC X(08).
           03  WRK-YUKOEDYMD           PIC X(08).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    包括診療コードマスタ
       01  HKTSRYCD-REC.
           COPY    "CPHKTSRYCD.INC".
      *
      *    電子点数表　補助マスター
       01  ETENSU1-REC.
           COPY    "CPETENSU1.INC".
      *
      *    包括・被包括マスター
       01  ETENSU2-REC.
           COPY    "CPETENSU2.INC".
      *
      *    包括・被包括マスター(ORCA)
       01  ETENSU2-JMA-REC.
           COPY    "CPETENSU2-JMA.INC".
      *
      *    包括・被包括マスター(ユーザーオフ）
       01  ETENSU2-OFF-REC.
           COPY    "CPETENSU2-OFF.INC".
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
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
           MOVE    SPA-FREE        TO  SPA-W32
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
           MOVE    SPA-W32         TO  SPA-FREE
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
               IF      SPA-WIDCD       NOT =   SPACE
                   PERFORM 520-WIDSET-SEC
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "W321"               TO  MCP-WINDOW
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
      *            登録
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2401-TOUROKU-SYORI-SEC
                       IF      SPA-ERRCD           =   SPACE
                           PERFORM 210-BACK
                       END-IF
                   ELSE
                       MOVE    1           TO  SPA-W321-CUR
                   END-IF
               WHEN    "1002"
      *            前頁
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2401-TOUROKU-SYORI-SEC
                       IF      SPA-ERRCD       =   SPACE
                           PERFORM 2601-MAE-PAGE-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1           TO  SPA-W321-CUR
                   END-IF
               WHEN    "1003"
      *            次頁
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2401-TOUROKU-SYORI-SEC
                       IF      SPA-ERRCD       =   SPACE
                           PERFORM 2701-NEXT-PAGE-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1           TO  SPA-W321-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE           TO  SPA-WID1-FLG
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           INITIALIZE                  SPA-W321-AREA
           MOVE    SPA-GMN-SELNUM      TO  SPA-W321-IDX
      *
           MOVE    SPACE               TO  HKTSRYCD-REC
           INITIALIZE                      HKTSRYCD-REC
     **    MOVE    SPA-NAI-STYUKYMD    TO  HKTSRYCD-YUKOSTYMD
     ***   MOVE    SPA-NAI-EDYUKYMD    TO  HKTSRYCD-YUKOEDYMD
      *H30.4
      *    履歴開始日で有効
           MOVE    SPA-NAI-STYUKYMD    TO  HKTSRYCD-YUKOSTYMD
           MOVE    SPA-NAI-STYUKYMD    TO  HKTSRYCD-YUKOEDYMD
           MOVE    SPA-GMN-KBNCD       TO  HKTSRYCD-KANRICD
      *
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKTSRYCD
           END-IF
      *
           PERFORM UNTIL   (FLG-HKTSRYCD   =   1  )
                      OR   (SPA-W321-H-GROUP   NOT =   SPACE)
      *        補助レコード算定チェック
               PERFORM 3101-ETENSU1-CHEK-SEC
      *
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-HKTSRYCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H28.6
           IF      SPA-W321-H-GROUP    NOT =   SPACE
      *        対象の期間を編集
               PERFORM 3101-ETENSU1-KIKAN-SEC
      *        対象の期間決定
               PERFORM 3201-KIKAN-HEN-SEC
           END-IF
      *
           IF      SPA-W321-H-GROUP    NOT =   SPACE
               MOVE    1               TO  SPA-W321-PAGE
               MOVE    ZERO            TO  SPA-W321-NEXT
      *        包括コードリスト
               PERFORM 320-HKTLIST-SYORI-SEC
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助マスターチェック処理
      *****************************************************************
       3101-ETENSU1-CHEK-SEC             SECTION.
      *
           MOVE    SPACE               TO  ETENSU1-REC
           INITIALIZE                      ETENSU1-REC
           MOVE    HKTSRYCD-SRYCD      TO  ETENSU1-SRYCD
     **    MOVE    SPA-NAI-EDYUKYMD    TO  ETENSU1-YUKOSTYMD
     **    MOVE    SPA-NAI-EDYUKYMD    TO  ETENSU1-YUKOEDYMD
           MOVE    ETENSU1-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_1"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-ETENSU1-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU1
               INITIALIZE                  ETENSU1-REC
           END-IF
           PERFORM UNTIL       FLG-ETENSU1 =   1
               IF     (ETENSU1-YUKOSTYMD   <=  SPA-NAI-EDYUKYMD)
                  OR  (ETENSU1-YUKOEDYMD   >=  SPA-NAI-STYUKYMD)
      *            グループ番号（１）を対象とする
                   MOVE    ETENSU1-H-GROUP(1)  TO  SPA-W321-H-GROUP
                   MOVE    1                   TO  FLG-ETENSU1
               END-IF
               IF      FLG-ETENSU1         =   ZERO
                   MOVE    "tbl_etensu_1"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-ETENSU1-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-ETENSU1-CHEK-EXT.
           EXIT.
      *
      *H28.6
      *****************************************************************
      *    包括コードリスト編集処理
      *****************************************************************
       3101-ETENSU1-KIKAN-SEC                  SECTION.
      *
           MOVE    SPA-NAI-STYUKYMD    TO  SPA-W321-YUKOSTYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  SPA-W321-YUKOEDYMD
      *
           .
       3101-ETENSU1-KIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象期間決定処理
      *****************************************************************
       3201-KIKAN-HEN-SEC                  SECTION.
      *
           MOVE    SPA-W321-YUKOSTYMD  TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-H-STYMD
           MOVE    SPA-W321-YUKOEDYMD  TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-H-EDYMD
           STRING  "【"
                   WRK-H-STYMD
                   "〜"
                   WRK-H-EDYMD
                   "】"
                                       DELIMITED  BY  SIZE
                                   INTO      SPA-W321-HEN-KIKAN
           END-STRING
           .
       3201-KIKAN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括コードリスト編集処理
      *****************************************************************
       320-HKTLIST-SYORI-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           INITIALIZE                  SPA-W321-LIST-AREA
           INITIALIZE                  SPA-W321-MAX
           MOVE    ZERO                TO  SPA-W321-LINE
      *
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  IDX
           COMPUTE IDX-STR             =  (SPA-W321-PAGE  -  1 )
                                       *   500
                                       +   1
           MOVE    ZERO                TO  SPA-W321-NEXT
           MOVE    ZERO                TO  SPA-W321-UPKBN
      *
      *    包括マスター処理
           PERFORM 3201-ETENSU2-SYORI-SEC
      *    包括マスター（ＯＲＣＡ）処理
           IF      SPA-W321-NEXT       =   ZERO
               PERFORM 3202-ETENSU2-JMA-SYORI-SEC
           END-IF
      *    包括解除処理
           IF     IDX-T                >   ZERO
               PERFORM 3203-ETENSU2-OFF-SYORI-SEC
           END-IF
           .
       320-HKTLIST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括マスター編集処理
      *****************************************************************
       3201-ETENSU2-SYORI-SEC                  SECTION.
      *
      *    キー編集
           PERFORM 3201-ETENSU2-HEN-SEC
      *
           MOVE    ETENSU2-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_2"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_2"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME   TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU2
               INITIALIZE                  ETENSU2-REC
           END-IF
           PERFORM UNTIL  (FLG-ETENSU2     =   1 )
                    OR    (SPA-W321-NEXT   =   1 )
      *H28.6
               IF     (ETENSU2-YUKOSTYMD   >   SPA-W321-YUKOEDYMD)
                  OR  (ETENSU2-YUKOEDYMD   <   SPA-W321-YUKOSTYMD)
                   CONTINUE
               ELSE
      *
               MOVE    ETENSU2-SRYCD      TO  WRK-SRYCD
               PERFORM 3201-TENSU-HENSYU-SEC
      *
               END-IF
      *
               MOVE    "tbl_etensu_2"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME   TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-READ-SEC
           END-PERFORM
           MOVE    "tbl_etensu_2"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3201-ETENSU2-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括マスター編集処理
      *****************************************************************
       3201-ETENSU2-HEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  ETENSU2-REC
           INITIALIZE                  ETENSU2-REC
           MOVE    SPA-W321-H-GROUP    TO  ETENSU2-H-GROUP
      *H24.4 開始日時点で有効
           MOVE    SPA-W321-YUKOSTYMD  TO  ETENSU2-YUKOSTYMD
           MOVE    SPA-W321-YUKOSTYMD  TO  ETENSU2-YUKOEDYMD
      *    検索用
           MOVE    SPA-HOSPNUM         TO  ETENSU2-TK-HOSPNUM
      *****MOVE    "key2"              TO  WRK-MCP-PATHNAME
      *
           MOVE    "key22"             TO  WRK-MCP-PATHNAME
      *
           EVALUATE    SPA-W321-SYORIKKBN
               WHEN    "01"
      *            投薬・処方せん料
                   MOVE    "key42"             TO  WRK-MCP-PATHNAME
                   MOVE    "20"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "29"                TO  ETENSU2-TK-SRYKBN-2
                   MOVE    "820"               TO  ETENSU2-TK-SRYSYUKBN
                   MOVE    "820"             TO  ETENSU2-TK-SRYSYUKBN-2
               WHEN    "02"
      *            注射
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "30"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "39"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "03"
      *            処置
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "40"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "49"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "04"
      *            手術・麻酔
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "50"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "59"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "05"
      *            検査
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "60"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "60"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "06"
      *            画像診断
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "70"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "79"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "07"
      *            リハビリ・その他
                   MOVE    "key52"             TO  WRK-MCP-PATHNAME
                   MOVE    "800"               TO  ETENSU2-TK-SRYSYUKBN
                   MOVE    "850"             TO  ETENSU2-TK-SRYSYUKBN-2
               WHEN    "08"
      *            精神科専門
                   MOVE    "key52"             TO  WRK-MCP-PATHNAME
                   MOVE    "830"               TO  ETENSU2-TK-SRYSYUKBN
                   MOVE    "830"             TO  ETENSU2-TK-SRYSYUKBN-2
               WHEN    "09"
      *            放射線治療
                   MOVE    "key52"             TO  WRK-MCP-PATHNAME
                   MOVE    "840"               TO  ETENSU2-TK-SRYSYUKBN
                   MOVE    "840"             TO  ETENSU2-TK-SRYSYUKBN-2
               WHEN    "10"
      *            病理診断
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "64"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "64"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "11"
      *            指導料
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "13"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "13"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "12"
      *            指導料
                   MOVE    "key32"             TO  WRK-MCP-PATHNAME
                   MOVE    "14"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "14"                TO  ETENSU2-TK-SRYKBN-2
               WHEN    "13"
      *            基本料
                   MOVE    "key42"             TO  WRK-MCP-PATHNAME
                   MOVE    "11"                TO  ETENSU2-TK-SRYKBN
                   MOVE    "12"                TO  ETENSU2-TK-SRYKBN-2
                   MOVE    "890"               TO  ETENSU2-TK-SRYSYUKBN
                   MOVE    "971"             TO  ETENSU2-TK-SRYSYUKBN-2
           END-EVALUATE
           .
       3201-ETENSU2-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コード対象チェック処理
      *****************************************************************
       3201-TENSU-HENSYU-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKEND
           PERFORM VARYING     IDC FROM    1   BY  1
                   UNTIL      (IDC     >   500 )
                         OR   (SPA-W321-TSRYCD (IDC) =   SPACE)
                         OR   (FLG-CHKEND      =   1    )
               IF      WRK-SRYCD       =   SPA-W321-TSRYCD (IDC)
                   MOVE    1               TO  FLG-CHKEND
                   IF      ETENSU2-YUKOSTYMD   >   SPA-W321-TSTYMD(IDC)
                       MOVE    IDC                 TO  IDX
      *                有効期間編集
                       MOVE    ETENSU2-YUKOSTYMD   TO  SPA-W321-TSTYMD
                                                           (IDX)
                       MOVE    ETENSU2-YUKOEDYMD   TO  SPA-W321-TEDYMD
                                                           (IDX)
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-CHKEND          =   1
               GO      TO      3201-TENSU-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-TNS-NAME
           MOVE    SPACE               TO  WRK-TNS-STYMD
           MOVE    SPACE               TO  WRK-TNS-EDYMD
      *
           PERFORM 3209-TENSU-CHEK-SEC
      *    点数マスタなしは対象外
           IF      WRK-TNS-NAME        =   SPACE
               GO      TO      3201-TENSU-HENSYU-EXT
           END-IF
      *
           ADD     1                   TO  IDX-T
           IF      IDX-T               >=  IDX-STR
               ADD     1               TO  IDX
               IF      IDX             >   500
                   MOVE    1               TO  SPA-W321-NEXT
               ELSE
      *            テーブル編集
                   PERFORM 3202-TBL-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       3201-TENSU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       3202-TBL-HENSYU-SEC                  SECTION.
      *
           MOVE    IDX             TO  SPA-W321-MAX
           MOVE    IDX-T           TO  SPA-W321-TNUM   (IDX)
           MOVE    WRK-SRYCD       TO  SPA-W321-TSRYCD (IDX)
           MOVE    WRK-TNS-NAME    TO  SPA-W321-TNAME  (IDX)
           MOVE    ETENSU2-YUKOSTYMD   TO  SPA-W321-TSTYMD(IDX)
           MOVE    ETENSU2-YUKOEDYMD   TO  SPA-W321-TEDYMD(IDX)
      *
           MOVE    ETENSU2-YUKOSTYMD   TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-H-STYMD
      *
           IF     (WRK-TNS-EDYMD   NOT =   SPACE)
              AND (WRK-TNS-EDYMD       <   ETENSU2-YUKOEDYMD)
               MOVE    WRK-TNS-EDYMD       TO  WRK-SYMD
           ELSE
               MOVE    ETENSU2-YUKOEDYMD   TO  WRK-SYMD
           END-IF
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-H-EDYMD
           STRING      WRK-H-STYMD
                       "〜"
                       WRK-H-EDYMD
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-W321-TKIKAN (IDX)
           END-STRING
           .
       3202-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ期間外編集処理
      *****************************************************************
       3209-TENSU-CHEK-SEC                  SECTION.
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           PERFORM UNTIL      FLG-TENSU    =   1
      *
      *
      *        MOVE    SPACE               TO  WRK-TNS-NAME
      *        STRING  "【外】"
      *                TNS-NAME            DELIMITED   BY  SPACE
      *                                    INTO    WRK-TNS-NAME
      *        END-STRING
      *        MOVE    TNS-YUKOSTYMD       TO  WRK-TNS-STYMD
      *        MOVE    TNS-YUKOEDYMD       TO  WRK-TNS-EDYMD
      *TEST
               IF      TNS-YUKOEDYMD       >   WRK-TNS-EDYMD
                   MOVE    TNS-YUKOEDYMD       TO  WRK-TNS-EDYMD
               END-IF
      ***      IF     (SPA-W321-YUKOSTYMD   <= TNS-YUKOSTYMD)
      ***        AND  (SPA-W321-YUKOEDYMD   >= TNS-YUKOEDYMD)
      *H30.4
               IF     (SPA-W321-YUKOSTYMD   >  TNS-YUKOEDYMD)
                 OR   (SPA-W321-YUKOEDYMD   <  TNS-YUKOSTYMD)
                   CONTINUE
               ELSE
                   MOVE    TNS-NAME            TO  WRK-TNS-NAME
      *            電子点数表期間内
                   IF     (TNS-YUKOSTYMD       <=  ETENSU2-YUKOSTYMD)
                     OR   (TNS-YUKOEDYMD       >=  ETENSU2-YUKOEDYMD)
                       MOVE    1                  TO  FLG-TENSU
                   END-IF
      *
               END-IF
      *
               IF      FLG-TENSU           =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 900-TENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *?
      *    IF      WRK-TNS-NAME        =   SPACE
      *        MOVE    ALL "＊"            TO  WRK-TNS-NAME
      *
      *?   END-IF
           .
       3209-TENSU-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    包括マスター（ＯＲＣＡ）編集処理
      *****************************************************************
       3202-ETENSU2-JMA-SYORI-SEC                  SECTION.
      *
      *    キー編集
           PERFORM 3201-ETENSU2-HEN-SEC
      *
           MOVE    ETENSU2-REC         TO  ETENSU2-JMA-REC
           MOVE    ETENSU2-JMA-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_2_jma"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_2_jma"  TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-JMA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU2-JMA
               INITIALIZE                  ETENSU2-JMA-REC
           END-IF
           PERFORM UNTIL   FLG-ETENSU2-JMA    =   1
      *
               MOVE    ETNSJMA-SRYCD       TO  WRK-SRYCD
               MOVE    ETENSU2-JMA-REC     TO  ETENSU2-REC
               IF     (ETENSU2-YUKOSTYMD   >   SPA-W321-YUKOEDYMD)
                  OR  (ETENSU2-YUKOEDYMD   <   SPA-W321-YUKOSTYMD)
                   CONTINUE
               ELSE
      *
               PERFORM 3201-TENSU-HENSYU-SEC
      *
               END-IF
      *
               MOVE    "tbl_etensu_2_jma"  TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-JMA-READ-SEC
           END-PERFORM
           MOVE    "tbl_etensu_2_jma"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3202-ETENSU2-JMA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括マスター（オフ）編集処理
      *****************************************************************
       3203-ETENSU2-OFF-SYORI-SEC      SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-W321-MAX  )
                          OR  (SPA-W321-TSRYCD (IDX)   =   SPACE )
      *        包括オフ検索
               PERFORM 3203-ETENSU2-OFFR-KEYREAD-SEC
               IF      FLG-ETENSU2-OFF     =   ZERO
                   MOVE    "1"             TO  SPA-W321-TDATAKBN(IDX)
                   MOVE    "1"             TO  SPA-W321-TOFFKBN (IDX)
               END-IF
           END-PERFORM
           .
       3203-ETENSU2-OFF-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括マスター（オフ）検索処理
      *****************************************************************
       3203-ETENSU2-OFFR-KEYREAD-SEC   SECTION.
      *
           MOVE    SPACE               TO  ETENSU2-OFF-REC
           INITIALIZE                  ETENSU2-OFF-REC
           MOVE    SPA-HOSPNUM         TO  ETNSOFF-HOSPNUM
           MOVE    SPA-W321-H-GROUP    TO  ETNSOFF-H-GROUP
           MOVE    SPA-W321-TSTYMD (IDX)   TO  ETNSOFF-YUKOSTYMD
           MOVE    SPA-W321-TEDYMD (IDX)   TO  ETNSOFF-YUKOEDYMD
           MOVE    SPA-W321-TSRYCD (IDX)   TO  ETNSOFF-SRYCD
           MOVE    ETENSU2-OFF-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-OFF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU2-OFF
               INITIALIZE                  ETENSU2-OFF-REC
           END-IF
           MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3203-ETENSU2-OFFR-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-W321-ROW
           ELSE
               MOVE    W321-LIST-ROW       TO  WRK-W321-ROW
           END-IF
      *
           MOVE    SPACE               TO  W321
           INITIALIZE                      W321
      *    コラムリストの位置指定
           MOVE    ZERO                TO  W321-LIST-ROW
           IF      WRK-W321-ROW    NOT =   ZERO
               MOVE    WRK-W321-ROW    TO  W321-LIST-ROW 
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W321-MAX
               MOVE    SPA-W321-TNUM(IDX)  TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  W321-TNUM (IDX)
               MOVE    SPA-W321-TSRYCD(IDX)
                                           TO  W321-TSRYCD (IDX)
               MOVE    SPA-W321-TNAME (IDX)
                                           TO  W321-TNAME  (IDX)
               IF      SPA-W321-TOFFKBN(IDX)   =   "1"
                   MOVE    "●"            TO  W321-TOFFKBN (IDX)
               END-IF
               MOVE    SPA-W321-TKIKAN (IDX)
                                           TO  W321-TKIKAN (IDX)
               IF     (SPA-W321-LINE       >   ZERO )
                  AND (SPA-W321-LINE       =   IDX  )
                   MOVE    IDX             TO  W321-LIST-ROW
                   MOVE    2               TO  W321-LIST-ROWATTR
               END-IF
           END-PERFORM
           MOVE    SPA-W321-MAX        TO  W321-LIST-COUNT
      *
      *    次頁あり
           MOVE    SPACE               TO  W321-NEXT-VALUE
           MOVE    "black"             TO  W321-NEXT-STYLE
           IF     (SPA-W321-NEXT       =   1   )  AND
                  (SPA-W321-PAGE       =   1   )
               MOVE    "［次頁有り］"      TO  W321-NEXT-VALUE
               MOVE    "red"               TO  W321-NEXT-STYLE
           END-IF
           IF     (SPA-W321-NEXT       =   0   )  AND
                  (SPA-W321-PAGE       >   1   )
               MOVE    "［前頁有り］"      TO  W321-NEXT-VALUE
               MOVE    "red"               TO  W321-NEXT-STYLE
           END-IF
           IF     (SPA-W321-NEXT       =   1   )  AND
                  (SPA-W321-PAGE       >   1   )
               MOVE    "［前次頁有り］"    TO  W321-NEXT-VALUE
               MOVE    "red"               TO  W321-NEXT-STYLE
           END-IF
      *
           MOVE    SPA-W321-SELNUM     TO  W321-SELNUM
      *
           MOVE    SPA-W321-HEN-KIKAN  TO  W321-MIDASI
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
           IF      SPA-W321-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "SELNUM"
                       MOVE    2                   TO  SPA-W321-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W321-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
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
                   PERFORM 220-CLREA-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 260-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-NEXT-PAGE-SEC
      *        オフ遷移
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 290-NEXT-OFF-SEC
      *        前履歴
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 290-MAE-RIREKI-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 2120-TOUROKU-SEC
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
               WHEN    "LIST"
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
           MOVE    ZERO                TO  SPA-W321-CUR
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
                   UNTIL       IDX     >   SPA-W321-MAX
              IF      W321-LIST-SELECT (IDX)    =  "T"
                  IF       SPA-W321-TNUM (IDX)  =   SPA-W321-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-W321-TNUM (IDX)  TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-W321-SELNUM
               MOVE    1                   TO  FLG-SELNUM
               PERFORM 2002-SELNUM-SEC
           END-IF
      *
           .
       2001-MEILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           MOVE    W321-SELNUM          TO  SPA-W321-SELNUM
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       2002-SELNUM-SEC               SECTION.
      *
           IF      (SPA-W321-SELNUM    >   ZERO)  AND
                   (SPA-ERRCD          =   SPACE) 
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  IDX-T
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-W321-MAX ) OR
                                  (FLG-CHK =   1    )
                   IF      SPA-W321-TNUM (IDX) =   SPA-W321-SELNUM
                       MOVE    IDX                 TO  IDX-T
                       MOVE    1                   TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W321-CUR
               END-IF
      *
               IF      IDX-T               >   ZERO
                   IF      SPA-W321-TOFFKBN (IDX-T)    =   "1"
                       MOVE    SPACE       TO  SPA-W321-TOFFKBN (IDX-T)
                   ELSE
                       MOVE    "1"         TO  SPA-W321-TOFFKBN (IDX-T)
                   END-IF
                   MOVE    1               TO  SPA-W321-UPKBN
                   MOVE    ZERO            TO  SPA-W321-SELNUM
                   MOVE    1               TO  SPA-W321-CUR
                   MOVE    IDX-T           TO  SPA-W321-LINE
               END-IF
           END-IF
      *
           .
       2002-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLREA-SEC                  SECTION.
      *
      *   　初期画面表示
           PERFORM 310-SPASET-SEC
           .
       220-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-MAE-PAGE-SEC                  SECTION.
      *
           IF      SPA-W321-PAGE       =   1
               MOVE    "0006"              TO  SPA-ERRCD
           ELSE
               IF      SPA-W321-UPKBN      =   1
                   MOVE    "1002"          TO  SPA-WIDCD
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-WIDCD           =   SPACE )
               PERFORM 2601-MAE-PAGE-SYORI-SEC
           END-IF
           .
       260-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *     前頁処理
      *****************************************************************
       2601-MAE-PAGE-SYORI-SEC                   SECTION.
      *
           COMPUTE SPA-W321-PAGE       =   SPA-W321-PAGE   -  1
      *    包括コードリスト
           PERFORM 320-HKTLIST-SYORI-SEC
           .
       2601-MAE-PAGE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXT-PAGE-SEC                  SECTION.
      *
           IF      SPA-W321-NEXT       =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
           ELSE
               IF      SPA-W321-UPKBN      =   1
                   MOVE    "1003"          TO  SPA-WIDCD
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-WIDCD           =   SPACE )
               PERFORM 2701-NEXT-PAGE-SYORI-SEC
           END-IF
           .
       270-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       2701-NEXT-PAGE-SYORI-SEC                   SECTION.
      *
           COMPUTE SPA-W321-PAGE       =   SPA-W321-PAGE   +  1
      *    包括コードリスト
           PERFORM 320-HKTLIST-SYORI-SEC
           .
       2701-NEXT-PAGE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       290-NEXT-OFF-SEC                   SECTION.
      *
           MOVE    SPA-W321-LINE       TO  IDX-STR
           ADD     1                   TO  IDX-STR
           MOVE    ZERO                TO  SPA-W321-LINE
           PERFORM VARYING     IDX     FROM    IDX-STR   BY  1
                   UNTIL       IDX     >   SPA-W321-MAX
               IF     (SPA-W321-TOFFKBN (IDX)  =   "1"  )
                   MOVE    IDX                 TO  SPA-W321-LINE
                   MOVE    SPA-W321-MAX        TO  IDX
               END-IF
           END-PERFORM
           IF     (SPA-W321-LINE        =   ZERO )
             AND  (IDX-STR              >   1    )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-W321-MAX
                   IF     (SPA-W321-TOFFKBN (IDX)  =   "1"  )
                       MOVE    IDX                 TO  SPA-W321-LINE
                       MOVE    SPA-W321-MAX        TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-W321-LINE        =   ZERO
               MOVE    "0020"           TO  SPA-ERRCD
               MOVE    1                TO  SPA-W321-CUR
           END-IF
           .
       290-NEXT-OFF-EXT.
           EXIT.
      *****************************************************************
      *    更新処理
      *****************************************************************
       2120-TOUROKU-SEC                   SECTION.
      *
           MOVE    "1001"          TO  SPA-WIDCD
      *
           .
       2120-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       2401-TOUROKU-SYORI-SEC          SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W321-MAX
               IF     (SPA-W321-TOFFKBN (IDX)  =   "1"  )  AND
                      (SPA-W321-TDATAKBN(IDX)  =   SPACE)
      *            新規追加
                   PERFORM 24011-INSERT-SYORI-SEC
               END-IF
               IF     (SPA-W321-TOFFKBN (IDX)  =   SPACE)  AND
                      (SPA-W321-TDATAKBN(IDX)  =   "1"  )
      *            削除処理
                   PERFORM 24012-DELETE-SYORI-SEC
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  SPA-W321-UPKBN
      *
           .
       2401-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       24011-INSERT-SYORI-SEC          SECTION.
      *
      *    包括オフ検索
           PERFORM 3203-ETENSU2-OFFR-KEYREAD-SEC
           IF      FLG-ETENSU2-OFF     =   ZERO
      *        登録済み
               GO      TO      24011-INSERT-SYORI-EXT
           END-IF
      *    包括オフ登録
           MOVE    SPACE               TO  ETENSU2-OFF-REC
           INITIALIZE                  ETENSU2-OFF-REC
           MOVE    SPA-HOSPNUM         TO  ETNSOFF-HOSPNUM
           MOVE    SPA-W321-H-GROUP    TO  ETNSOFF-H-GROUP
           MOVE    SPA-W321-TSTYMD (IDX)   TO  ETNSOFF-YUKOSTYMD
           MOVE    SPA-W321-TEDYMD (IDX)   TO  ETNSOFF-YUKOEDYMD
           MOVE    SPA-W321-TSRYCD (IDX)   TO  ETNSOFF-SRYCD
      *
           MOVE    SPA-OPID            TO  ETNSOFF-OPID
           MOVE    SMCNDATE-YMD        TO  ETNSOFF-CREYMD
           MOVE    SMCNDATE-YMD        TO  ETNSOFF-UPYMD
           MOVE    SMCNDATE-HMS        TO  ETNSOFF-UPHMS
      *
           MOVE    ETENSU2-OFF-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "W321 DB INS ERR KEY:"  ETNSOFF-KEY 
           END-IF
           .
       24011-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       24012-DELETE-SYORI-SEC          SECTION.
      *
      *    包括オフ検索
           PERFORM 3203-ETENSU2-OFFR-KEYREAD-SEC
           IF      FLG-ETENSU2-OFF     =   ZERO
      *        削除処理
               MOVE    ETENSU2-OFF-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "W321 DB DEL ERR:" ETNSOFF-KEY 
               END-IF
           END-IF
           .
       24012-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    前履歴処理
      *****************************************************************
       290-MAE-RIREKI-SEC             SECTION.
      *
      *!!!
      *    履歴毎の表示とする
           IF     (SPA-W321-IDX        =   ZERO
                                       OR  1   )
               MOVE    "0021"              TO  SPA-ERRCD
               GO      TO      290-MAE-RIREKI-EXT
           END-IF
           COMPUTE SPA-W321-IDX        =   SPA-W321-IDX
                                       -   1
           MOVE    SPA-NAI-TSTYUKYMD (SPA-W321-IDX)
                                       TO  SPA-W321-YUKOSTYMD
           MOVE    SPA-NAI-TEDYUKYMD (SPA-W321-IDX)
                                       TO  SPA-W321-YUKOEDYMD
      *    対象の期間決定
           PERFORM 3201-KIKAN-HEN-SEC
           MOVE    1               TO  SPA-W321-PAGE
           MOVE    ZERO            TO  SPA-W321-NEXT
      *    包括コードリスト
           PERFORM 320-HKTLIST-SYORI-SEC
           IF      SPA-W321-MAX        =   ZERO
               MOVE    "0022"              TO  SPA-ERRCD
           END-IF
      *
      *
      *
      *
               GO      TO      290-MAE-RIREKI-EXT
      *
           MOVE    SPACE               TO  ETENSU1-REC
           INITIALIZE                      ETENSU1-REC
           MOVE    SPA-W321-H-GROUP    TO  ETENSU1-H-GROUP(1)
      *
           MOVE    ETENSU1-REC         TO  MCPDATA-REC
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_1"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-ETENSU1-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU1
               INITIALIZE                  ETENSU1-REC
           END-IF
           MOVE    SPACE               TO  WRK-YUKOSTYMD
           MOVE    SPACE               TO  WRK-YUKOEDYMD
           PERFORM UNTIL  (FLG-ETENSU1     =   1 )
                     OR   (WRK-YUKOSTYMD   NOT =   SPACE)
               IF      (ETENSU1-YUKOSTYMD  >=  SPA-NAI-STYUKYMD)
                   AND (ETENSU1-YUKOEDYMD  <=  SPA-NAI-EDYUKYMD)
                   IF      ETENSU1-YUKOEDYMD   <   SPA-W321-YUKOSTYMD
                       MOVE    ETENSU1-YUKOSTYMD   TO  WRK-YUKOSTYMD
                       MOVE    ETENSU1-YUKOEDYMD   TO  WRK-YUKOEDYMD
                    END-IF
               END-IF
               MOVE    "tbl_etensu_1"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-ETENSU1-READ-SEC
           END-PERFORM
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-YUKOSTYMD       =   SPACE
               MOVE    "0021"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-YUKOSTYMD       TO  SPA-W321-YUKOSTYMD
               MOVE    WRK-YUKOEDYMD       TO  SPA-W321-YUKOEDYMD
               MOVE    SPA-W321-YUKOSTYMD  TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-H-STYMD
               MOVE    SPA-W321-YUKOEDYMD  TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-H-EDYMD
               STRING  "【"
                       WRK-H-STYMD
                       "〜"
                       WRK-H-EDYMD
                       "】"
                                           DELIMITED  BY  SIZE
                                       INTO      SPA-W321-HEN-KIKAN
               END-STRING
      *
               MOVE    1               TO  SPA-W321-PAGE
               MOVE    ZERO            TO  SPA-W321-NEXT
      *        包括コードリスト
               PERFORM 320-HKTLIST-SYORI-SEC
               IF      SPA-W321-MAX        =   ZERO
                   MOVE    "0022"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       290-MAE-RIREKI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W32"               TO  SPA-SAKIPG
           MOVE    "W321"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "W32"               TO  MCP-WINDOW
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
           MOVE    "W321"              TO  MCP-WINDOW.
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
               WHEN    "0006"
                   MOVE    "前頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "次頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "頁内に包括解除設定はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "前履歴データはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "対象診療区分の前履歴データはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "登録エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W321"               TO  SPA-MOTOPG
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
                   STRING  "包括オフ情報を登録します。"
                           "よろしいですか？"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1002"
                   STRING  "包括オフ情報が変更されています。"
                           "更新後、前頁を表示します。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "包括オフ情報が変更されています。"
                           "更新後、次頁を表示します。"
                                                   DELIMITED  BY  SIZE
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
           MOVE    "W321"              TO  SPA-MOTOPG
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
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    ZERO
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    包括診療コード読み込み
      *****************************************************************
       900-HKTSRYCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSRYCD-REC
               MOVE    ZERO            TO  FLG-HKTSRYCD
           ELSE
               INITIALIZE                  HKTSRYCD-REC
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
      *
           .
       900-HKTSRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    補助マスタ読み込み
      *****************************************************************
       910-ETENSU1-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU1-REC
               MOVE    ZERO            TO  FLG-ETENSU1
           ELSE
               INITIALIZE                  ETENSU1-REC
               MOVE    1               TO  FLG-ETENSU1
           END-IF
      *
           .
       910-ETENSU1-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括・被包括マスター読み込み
      *****************************************************************
       910-ETENSU2-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU2-REC
               MOVE    ZERO            TO  FLG-ETENSU2
           ELSE
               INITIALIZE                  ETENSU2-REC
               MOVE    1               TO  FLG-ETENSU2
           END-IF
      *
           .
       910-ETENSU2-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括・被包括マスター（ＪＭＡ）読み込み
      *****************************************************************
       910-ETENSU2-JMA-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU2-JMA-REC
               MOVE    ZERO            TO  FLG-ETENSU2-JMA
           ELSE
               INITIALIZE                  ETENSU2-JMA-REC
               MOVE    1               TO  FLG-ETENSU2-JMA
           END-IF
      *
           .
       910-ETENSU2-JMA-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括・被包括マスター（ユーザオフ）読み込み
      *****************************************************************
       910-ETENSU2-OFF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU2-OFF-REC
               MOVE    ZERO            TO  FLG-ETENSU2-OFF
           ELSE
               INITIALIZE                  ETENSU2-OFF-REC
               MOVE    1               TO  FLG-ETENSU2-OFF
           END-IF
      *
           .
       910-ETENSU2-OFF-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
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
