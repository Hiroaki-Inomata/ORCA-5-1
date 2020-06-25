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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGZ010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : ユーザー指定項目更新画面（Ｚ０１０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/01/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.00    NACL-多々納  20/02/05  ユーザー単位追加
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    ＳＰＡエリア
           COPY    "Z010SCR-SPA".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-TENSUPRICE                    PIC 9(01).
           03  FLG-SYSKANRI                      PIC 9(01).
      *
           03  FLG-CHK                           PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ERR                           PIC 9(01).
      *
           03  FLG-TOUROKU                       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDX-1                             PIC 9(04).
           03  IDXX                              PIC 9(04).
           03  IDX1                              PIC 9(04).
           03  IDX-T                             PIC 9(04).
           03  IDH                               PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-STRING                        PIC 9(03).
      *
      *    ワーク領域
       01  WRK-AREA.
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
      *
           03  WRK-LNKYMD                        PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO                       PIC X(01).
               05  WRK-WYY                       PIC 9(02).
               05  WRK-WMM                       PIC 9(02).
               05  WRK-WDD                       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-Z9                  PIC ZZ9.
           03  WRK-ZZZ                 PIC ZZZ.
           03  WRK-SELNUM              PIC 9(03).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-OUT-INPUT       PIC X(400).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-ZIDMSG          PIC X(80).
           03  WRK-YUKOEDYMD       PIC X(08).
           03  WRK-SRYLST-DD       PIC X(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-STRINGTBL.
               05  WRK-STRING          PIC X(01)  OCCURS 200.
      *R02.2
           03  WRK-CHK-TANICD      PIC X(03).
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
      *    単位の項目順テーブル
           COPY    "CMTANICD.INC".
      *
      *    年令単位テーブル
           COPY    "CMNENREICD.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    高額請求資料用薬剤分類情報
           COPY  "CPSK1036.INC".
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    点数マスタ器材金額
      * 01 TENSU-PRICE-REC.
      *    COPY    "CPTENSU-PRICE.INC".
      *    老人一般コード変換
      * 01  SRYCDCHG-REC.
      *     COPY    "CPSRYCDCHG.INC".
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
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA102SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 LINKAREA
                                                 SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            IDX-AREA
           INITIALIZE                            WRK-AREA
           INITIALIZE                            CNT-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z010FREE                  TO  SPA-Z010
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *    
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z010                      TO  SPA-Z010FREE
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            IDX-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "Z010"               TO  MCP-WINDOW
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
       300-SCREEN-SEC             SECTION.
      *
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "ZID1"
      *        更新処理
                   PERFORM 330-ZID1-SET-SEC
               WHEN    OTHER
      *        画面用スパ編集処理
                   PERFORM 310-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
      *            削除
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4031-DEL-SEC
                   END-IF
               WHEN    "0002"
      *            登録
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4121-TOUROKU-SEC
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD      
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通ＳＰＡ情報をＳＰＡにセット
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-Z010
      *
           MOVE    SPA-MOTOPG          TO  SPA-Z010-MOTOPG
      *
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "【ユーザー管理】"
                                       TO  SPA-Z010-MIDASHI
           END-IF
           IF      SPA-Z01-SRYCD(1:1)  =   "6"
               MOVE    "処方名称"      TO  SPA-Z010-MEISHO
           ELSE
               MOVE    "出力名称"      TO  SPA-Z010-MEISHO
           END-IF
      *
      *    初期表示
           PERFORM 3101-INIT-GMN-SEC
           IF      SPA-Z010-RRKMAX      =   ZERO
               MOVE    2                   TO  SPA-Z010-CUR
      *        新規
               MOVE    1                   TO  SPA-Z010-SYORI
           ELSE
               MOVE    1                   TO  SPA-Z010-CUR
      *        新規
               MOVE    1                   TO  SPA-Z010-SYORI
      *        最終履歴を表示
      ***      MOVE    SPA-Z010-RRKMAX     TO  SPA-Z010-NAI-SELNUM
      *        MOVE    SPA-Z010-RRKMAX     TO  WRK-ZZZ
      *        MOVE    WRK-ZZZ             TO  SPA-Z010-SELNUM
      *        PERFORM 3104-SELNUM-SYORI-SEC
      ***      MOVE    2                   TO  SPA-Z010-SYORI
           END-IF
           .
        310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期表示編集処理
      *****************************************************************
       3101-INIT-GMN-SEC            SECTION.
      *
      *    付加情報検索
           PERFORM 3101-RRKLIST-HEN01-SEC
      *    リスト編集
           MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           PERFORM 3102-LIST-HENSYU-SEC
      *
           .
        3101-INIT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    付加情報履歴編集処理
      *****************************************************************
       3101-RRKLIST-HEN01-SEC            SECTION.
      *
           INITIALIZE                  SPA-Z010-RRKLIST-G
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-TENSUPLUS       =   1 )
                       OR  (IDX                 >=  50 )
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-Z010-RRKMAX
               MOVE    IDX                 TO  SPA-Z010-TRENNUM
                                                          (IDX)
               MOVE    TNSP-YUKOSTYMD      TO  SPA-Z010-NAI-TYUKOSTYMD
                                                          (IDX)
               MOVE    TNSP-YUKOEDYMD      TO  SPA-Z010-NAI-TYUKOEDYMD
                                                          (IDX)
      *
               MOVE    TNSP-YUKOSTYMD      TO  WRK-SYMD
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-Z010-TYUKOSTYMD
                                                          (IDX)
               MOVE    TNSP-YUKOEDYMD      TO  WRK-SYMD
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-Z010-TYUKOEDYMD
                                                          (IDX)
      *
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3101-RRKLIST-HEN01-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    リスト編集処理
      *****************************************************************
       3102-LIST-HENSYU-SEC            SECTION.
      *
      *    採血料区分一覧
           MOVE    "0:　　　　"        TO  SPA-Z010-SAIKETUCOMB (01)
           MOVE    "1:末梢採血"        TO  SPA-Z010-SAIKETUCOMB (02)
           MOVE    "2:静脈採血"        TO  SPA-Z010-SAIKETUCOMB (03)
           MOVE    "3:動脈採血"        TO  SPA-Z010-SAIKETUCOMB (04)
           MOVE    4                   TO  SPA-Z010-SAIKETUKBN-MAX
      *
      *    前回検査日区分
           MOVE    "0 表示しない"      TO  SPA-Z010-ZENKNSKBN-COMB(1)
           MOVE    "1 表示する（初回コメント）"
                                       TO  SPA-Z010-ZENKNSKBN-COMB(2)
           MOVE    "2 表示する（初回実施日）"
                                       TO  SPA-Z010-ZENKNSKBN-COMB(3)
           MOVE    3                   TO  SPA-Z010-ZENKNSKBN-MAX
      *    上限回数
           MOVE    "0 無効"            TO  SPA-Z010-PLUS-SANTEICOMB(1)
           MOVE    "1 有効（算定履歴作成）"
                                       TO  SPA-Z010-PLUS-SANTEICOMB(2)
           MOVE    2                   TO  SPA-Z010-PLUS-SANTEI-MAX
      *
      *    データ規格
           MOVE    ZERO                TO  SPA-Z010-TANICD-MAX
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       MTANI-MAX
               MOVE    MTANI-TANICD  (IDXX)
                                       TO  SPA-Z010-TANICDL   (IDXX)
               MOVE    MTANI-TANINAME(IDXX)
                                       TO  SPA-Z010-TANICDMEIL(IDXX)
               MOVE    IDXX            TO  SPA-Z010-TANICD-MAX
           END-PERFORM
      *
      *    総量編集
           MOVE    "0"                 TO  SPA-Z010-ALLSURYOKBNL   (1)
           MOVE    "記載しない"        TO  SPA-Z010-ALLSURYOKBNMEIL(1)
           MOVE    "1"                 TO  SPA-Z010-ALLSURYOKBNL   (2)
           MOVE    "記載する"          TO  SPA-Z010-ALLSURYOKBNMEIL(2)
           MOVE    2                   TO  SPA-Z010-ALLSURYOKBN-MAX
      *
      *    一般名記載区分
           MOVE    "0 記載しない"      TO  SPA-Z010-IPN-KISAIKBN-LST(1)
           MOVE    "1 一般名を記載する"
                                       TO  SPA-Z010-IPN-KISAIKBN-LST(2)
           MOVE    "2 処方名称を記載する"
                                       TO  SPA-Z010-IPN-KISAIKBN-LST(3)
           MOVE    "3 処方名称（一般名）を記載"
                                       TO  SPA-Z010-IPN-KISAIKBN-LST(4)
           MOVE    4                   TO  SPA-Z010-IPN-KISAI-MAX
      *    総量編集
           MOVE    "0 なし"            TO  SPA-Z010-SOURYOKBN-LST(1)
           MOVE    "1 内服滴剤"
                                       TO  SPA-Z010-SOURYOKBN-LST(2)
           MOVE    2                   TO  SPA-Z010-SOURYO-MAX
      *
      *    高額請求資料用薬剤分類情報
           PERFORM 31021-SYS1036-HEN-SEC
      *
           PERFORM 5021-INIT-HEN-SEC
           .
       3102-LIST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    付加情報取得処理
      *****************************************************************
       31021-SYS1036-HEN-SEC           SECTION.
      *
           INITIALIZE                  SPA-Z010-BUNRUIKBNLST-G
           MOVE    ZERO                TO  SPA-Z010-BUNRUIKBN-MAX
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPACE               TO  SYS-1036-REC
           INITIALIZE                      SYS-1036-REC
           MOVE    "1036"              TO  SYS-1036-KANRICD
           MOVE    WRK-SYSYMD          TO  SYS-1036-STYUKYMD
           MOVE    WRK-SYSYMD          TO  SYS-1036-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1036-HOSPNUM
           MOVE    SYS-1036-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX1
           PERFORM UNTIL      (FLG-SYSKANRI   =   1 )
                         OR   (IDX1           >=  20)
      *
               COMPUTE SPA-Z010-BUNRUIKBN-MAX
                   =   SPA-Z010-BUNRUIKBN-MAX   +   1
               MOVE    SPA-Z010-BUNRUIKBN-MAX
                                           TO  IDX1
      *
               MOVE    MCPDATA-REC         TO  SYS-1036-REC
               MOVE    SYS-1036-KBNCD(1:2) TO  SPA-Z010-BUNRUIKBNL
                                                               (IDX1)
               MOVE    SYS-1036-BUNRUIMEI  TO  SPA-Z010-BUNRUIKBNMEIL
                                                               (IDX1)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31021-SYS1036-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    付加情報取得処理
      *****************************************************************
       3104-SELNUM-SYORI-SEC           SECTION.
      *
           MOVE    SPA-Z010-NAI-SELNUM      TO  IDH
      *
           INITIALIZE                  SPA-Z010-KIKAN
           MOVE    SPA-Z010-TYUKOSTYMD (IDH)
                                       TO  SPA-Z010-YUKOSTYMD
           MOVE    SPA-Z010-TYUKOEDYMD (IDH)
                                       TO  SPA-Z010-YUKOEDYMD
           MOVE    SPA-Z010-NAI-TYUKOSTYMD (IDH)
                                       TO  SPA-Z010-NAI-YUKOSTYMD
           MOVE    SPA-Z010-NAI-TYUKOEDYMD (IDH)
                                       TO  SPA-Z010-NAI-YUKOEDYMD
      *
           INITIALIZE                  SPA-Z010-INPUT-G
      *    付加情報
           PERFORM 31041-TENSU-PLUS-HEN-SEC
           .
       3104-SELNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    付加情報処理
      *****************************************************************
       31041-TENSU-PLUS-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                  TENSUPLUS-REC
           MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
           MOVE    SPA-Z010-NAI-TYUKOSTYMD (IDH)
                                       TO  TNSP-YUKOSTYMD
           MOVE    SPA-Z010-NAI-TYUKOEDYMD (IDH)
                                       TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSUPLUS       =   1
               INITIALIZE                  TENSUPLUS-REC
           END-IF
      *
      *    高額請求資料用薬剤分類情報
           MOVE    SPA-Z010-NAI-TYUKOSTYMD (IDH) TO  WRK-SYSYMD
           PERFORM 31021-SYS1036-HEN-SEC
      *
      *    処方名
           MOVE    TNSP-SHONAME        TO  SPA-Z010-SHONAME
      *
           IF      ( SPA-Z01-SRYCD (1:1)   =   "1"  )
               PERFORM 310411-SRYCD01-HEN-SEC
           END-IF
      *
           IF      ( SPA-Z01-SRYCD (1:1)   =   "6"  )
               PERFORM 310411-SRYCD02-HEN-SEC
           END-IF
      *
           IF      ( SPA-Z01-SRYCD (1:1)   =   "7"  )
               PERFORM 310411-SRYCD03-HEN-SEC
           END-IF
           .
       31041-TENSU-PLUS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       310411-SRYCD01-HEN-SEC           SECTION.
      *
      *    採血区分
           MOVE    TNSP-SAIKETUKBN     TO  SPA-Z010-SAIKETUKBN
      *    採血料区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-SAIKETUKBN-MAX
                         OR  ( SPA-Z01-SRYCD (1:1) NOT =   "1"  )
               IF      SPA-Z010-SAIKETUKBN  =   SPA-Z010-SAIKETUCOMB
                                                            (IDX)(1:1)
                   MOVE    SPA-Z010-SAIKETUCOMB (IDX)
                                           TO  SPA-Z010-SAIKETUKBN-G
                   MOVE    SPA-Z010-SAIKETUKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *    前回検査日区分
           MOVE    TNSP-ZENKNSKBN      TO  SPA-Z010-ZENKNSKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-ZENKNSKBN-MAX
                     OR  ( SPA-Z01-SRYCD (1:1) NOT =   "1"  )
               IF      SPA-Z010-ZENKNSKBN   =   SPA-Z010-ZENKNSKBN-COMB
                                                            (IDX)(1:1)
                   MOVE    SPA-Z010-ZENKNSKBN-COMB (IDX)
                                           TO  SPA-Z010-ZENKNSKBN-G
                   MOVE    SPA-Z010-ZENKNSKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    上限回数
           MOVE    TNSP-SANTEIRRKKBN   TO  SPA-Z010-PLUS-SANTEIRRK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-PLUS-SANTEI-MAX
                         OR  ( SPA-Z01-SRYCD (1:1) NOT =   "1"  )
               IF      SPA-Z010-PLUS-SANTEIRRK
                                       =   SPA-Z010-PLUS-SANTEICOMB
                                                            (IDX)(1:1)
                   MOVE    SPA-Z010-PLUS-SANTEICOMB (IDX)
                                           TO  SPA-Z010-PLUS-SANTEIRRK-G
                   MOVE    SPA-Z010-PLUS-SANTEI-MAX  TO  IDX
               END-IF
           END-PERFORM
      *    月上限回数
           MOVE    TNSP-JGNCNT         TO  SPA-Z010-PLUS-JGNCNT
      *    日上限回数
           MOVE    TNSP-JGNCNT1D       TO  SPA-Z010-PLUS-JGNCNT1D
      *    週上限回数
           MOVE    TNSP-JGNCNT1W       TO  SPA-Z010-PLUS-JGNCNT1W
      *    他月数
           MOVE    TNSP-JGNCNTETCM     TO  SPA-Z010-PLUS-JGNCNTETCM
      *    他回数
           MOVE    TNSP-JGNCNTETC      TO  SPA-Z010-PLUS-JGNCNTETC
      *    エラー処理
           MOVE    TNSP-JGNCNTERR      TO  SPA-Z010-PLUS-JGNCNTERR
      *
           .
       310411-SRYCD01-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤編集処理
      *****************************************************************
       310411-SRYCD02-HEN-SEC           SECTION.
      *
      *    分類区分
           MOVE    TNSP-TENPUYAKKBN    TO  SPA-Z010-BUNRUIKBN
           MOVE    TNSP-ALLSURYOKBN    TO  SPA-Z010-ALLSURYOKBN
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-Z010-BUNRUIKBN-MAX )
      *
               IF    ( TNSP-TENPUYAKKBN
                                  =    SPA-Z010-BUNRUIKBNL (IDX1))
                   MOVE    SPA-Z010-BUNRUIKBNLST (IDX1)
                                       TO   SPA-Z010-BUNRUIKBN-G
                   MOVE    SPA-Z010-BUNRUIKBN-MAX
                                       TO   IDX1
               END-IF
           END-PERFORM
      *
      *    全数量記載区分
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-Z010-ALLSURYOKBN-MAX
               IF      TNSP-ALLSURYOKBN 
                                       =   SPA-Z010-ALLSURYOKBNL(IDXX)
                   MOVE    SPA-Z010-ALLSURYOKBNLST (IDXX)
                                           TO  SPA-Z010-ALLSURYOKBN-G
                   MOVE    SPA-Z010-ALLSURYOKBN-MAX
                                           TO  IDXX
               END-IF
           END-PERFORM
      *
      *    数量換算単位コード
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-Z010-TANICD-MAX
               IF      TNSP-KANZANTANICD 
                                       =   SPA-Z010-TANICDL(IDXX)
                   MOVE    SPA-Z010-TANICDLST (IDXX)
                                           TO  SPA-Z010-KANZANTANICD-G
                   MOVE    SPA-Z010-TANICD-MAX
                                           TO  IDXX
               END-IF
           END-PERFORM
      *    数量換算値
           MOVE    TNSP-KANZANCHI      TO  SPA-Z010-KANZANCHI
      *
      *    総量編集
           MOVE    TNSP-SOURYOHENKBN   TO  SPA-Z010-SOURYOKBN
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-Z010-SOURYO-MAX
               IF      TNSP-SOURYOHENKBN
                                       =   SPA-Z010-SOURYOKBN-LST
                                                       (IDX-T)(1:1)
                   MOVE    SPA-Z010-SOURYOKBN-LST (IDX-T)
                                       TO  SPA-Z010-SOURYOKBN-G
                   MOVE    SPA-Z010-SOURYO-MAX  TO  IDX-T
               END-IF
           END-PERFORM
      *    一般名記載区分
           MOVE    TNSP-IPN-KISAIKBN    TO  SPA-Z010-IPN-KISAIKBN
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL  (IDX1    >   SPA-Z010-IPN-KISAI-MAX)
               IF      TNSP-IPN-KISAIKBN
                                       =    SPA-Z010-IPN-KISAIKBN-LST
                                                         (IDX1)(1:1)
                   MOVE    SPA-Z010-IPN-KISAIKBN-LST (IDX1)
                                       TO   SPA-Z010-IPN-KISAIKBN-G
                   MOVE    SPA-Z010-IPN-KISAI-MAX
                                       TO   IDX1
               END-IF
           END-PERFORM
      *    一般名記載単位
           MOVE    TNSP-IPN-KANZANTANICD   TO  SPA-Z010-IPN-TANICD
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   SPA-Z010-TANICD-MAX
               IF      TNSP-IPN-KANZANTANICD
                                           =    SPA-Z010-TANICDL (IDX1)
                   MOVE    SPA-Z010-TANICDLST (IDX1)
                                           TO   SPA-Z010-IPN-TANICD-G
                   MOVE    SPA-Z010-TANICD-MAX
                                           TO   IDX1
               END-IF
           END-PERFORM
      *    一般名記載換算値
           MOVE    TNSP-IPN-KANZANCHI  TO  SPA-Z010-IPN-KANZANCHI
      *    湿布薬関係
           MOVE    TNSP-SIPPURYO1      TO  SPA-Z010-SIPRYOU1
           MOVE    TNSP-SIPPUIRISU1    TO  SPA-Z010-SIPIRISU1
           MOVE    TNSP-SIPPURYO2      TO  SPA-Z010-SIPRYOU2
           MOVE    TNSP-SIPPUIRISU2    TO  SPA-Z010-SIPIRISU2
      *R02.2
      *    ユーザー単位
           MOVE    TNSP-USERTANICD     TO  SPA-Z010-USER-TANICD
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   SPA-Z010-TANICD-MAX
               IF      TNSP-USERTANICD
                                           =    SPA-Z010-TANICDL (IDX1)
                   MOVE    SPA-Z010-TANICDLST (IDX1)
                                           TO   SPA-Z010-USER-TANICD-G
                   MOVE    SPA-Z010-TANICD-MAX
                                           TO   IDX1
               END-IF
           END-PERFORM
           .
       310411-SRYCD02-HEN-EXT.
           EXIT.
      *
      *R02.2
      *****************************************************************
      *    器材編集処理
      *****************************************************************
       310411-SRYCD03-HEN-SEC           SECTION.
      *
      *    ユーザー単位
           MOVE    TNSP-USERTANICD     TO  SPA-Z010-USER-TANICD
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   SPA-Z010-TANICD-MAX
               IF      TNSP-USERTANICD
                                           =    SPA-Z010-TANICDL (IDX1)
                   MOVE    SPA-Z010-TANICDLST (IDX1)
                                           TO   SPA-Z010-USER-TANICD-G
                   MOVE    SPA-Z010-TANICD-MAX
                                           TO   IDX1
               END-IF
           END-PERFORM
           .
       310411-SRYCD03-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  402-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  502-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 41015-LIST1SEL-SEC
      *        一括入力分
               WHEN    OTHER
                   PERFORM 5000-ENTRY-SYORI-SEC
           END-EVALUATE.
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *  　選択番号チェック
      *****************************************************************
       502-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-Z010-CUR
           INITIALIZE                      SPA-Z010-INPUT-G
           INITIALIZE                      SPA-Z010-KIKAN
           MOVE    ZERO                TO  SPA-Z010-NAI-SELNUM
      *
           MOVE    Z010-SELNUM         TO  SPA-Z010-SELNUM
      *
           IF      SPA-Z010-SELNUM      =   SPACE
      *R01.11
      *        最終履歴を表示
               IF     (SPA-Z010-RRKMAX     >   ZERO  )
                 AND  (SPA-Z010-NAI-TYUKOEDYMD (SPA-Z010-RRKMAX)
                                           =   "99999999"  )
                   MOVE    SPA-Z010-RRKMAX     TO  SPA-Z010-SELNUM
                   MOVE    ZERO                TO  SPA-Z010-SYORI
               ELSE
      *        新規
                  MOVE    1                   TO  SPA-Z010-SYORI
                  MOVE    2                   TO  SPA-Z010-CUR
                  PERFORM 5021-INIT-HEN-SEC
                  GO      TO      502-SELNUM-CHK-EXT
               END-IF
           ELSE
               MOVE    ZERO                TO  SPA-Z010-SYORI
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-Z010-SELNUM      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT           =  ZERO  )   OR
                  (SNUM-MINKBN     NOT           =  SPACE )   OR
                  (SNUM-SYOKBN     NOT           =  SPACE )
               MOVE    "0002"                    TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT         TO  SPA-Z010-NAI-SELNUM
               MOVE    SPA-Z010-NAI-SELNUM      TO  WRK-ZZZ
               MOVE    WRK-ZZZ             TO  SPA-Z010-SELNUM
           END-IF
      *
           IF      (SPA-Z010-NAI-SELNUM     <   1   )  OR
                   (SPA-Z010-NAI-SELNUM     >   SPA-Z010-RRKMAX)
               IF      SPA-ERRCD           =   SPACE
  	           MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3104-SELNUM-SYORI-SEC
               MOVE    2                   TO  SPA-Z010-CUR
           END-IF
           .
       502-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    新規初期表示処理
      *****************************************************************
       5021-INIT-HEN-SEC       SECTION.
      *
           IF    (SPA-Z01-SRYCD (1:1) =   "1"   )
      *        採血料区分
               MOVE    SPA-Z010-SAIKETUCOMB (1)
                                       TO  SPA-Z010-SAIKETUKBN-G
               MOVE    SPA-Z010-ZENKNSKBN-COMB (1)
                                       TO  SPA-Z010-ZENKNSKBN-G
               MOVE    SPA-Z010-PLUS-SANTEICOMB (1)
                                       TO  SPA-Z010-PLUS-SANTEIRRK-G
           END-IF
      *
           IF    (SPA-Z01-SRYCD (1:1) =   "6"   )
      *       総量編集
               MOVE    SPA-Z010-SOURYOKBN-LST(1)
                                       TO  SPA-Z010-SOURYOKBN-G
               MOVE    SPA-Z010-IPN-KISAIKBN-LST(1)
                                       TO  SPA-Z010-IPN-KISAIKBN-G
           END-IF
           .
       5021-INIT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41015-LIST1SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-RRKMAX
               IF      Z010-RRKLIST-SELECT (IDX)   =   "T"
                   IF      IDX             =   SPA-Z010-NAI-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-Z010-RRKMAX      TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-Z010-NAI-SELNUM
           END-IF
      *
           IF      SPA-Z010-NAI-SELNUM      >   ZERO
               MOVE    SPA-Z010-NAI-SELNUM      TO  Z010-SELNUM
               PERFORM 502-SELNUM-CHK-SEC
           END-IF
           .
       41015-LIST1SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       5000-ENTRY-SYORI-SEC         SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-Z010-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       5000-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
               PERFORM 4109-TOUROKU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      410-INPUT-CHK-EXT
               END-IF
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
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    Z010-YUKOSTYMD       TO  SPA-Z010-YUKOSTYMD
           MOVE    Z010-YUKOEDYMD       TO  SPA-Z010-YUKOEDYMD
      *
           MOVE    Z010-SHONAME         TO  SPA-Z010-SHONAME
      *
           IF      SPA-Z01-SRYCD (1:1)  =   "1"
               MOVE    Z010-SAIKETUKBN      TO  SPA-Z010-SAIKETUKBN-G
               MOVE    Z010-ZENKNSKBN       TO  SPA-Z010-ZENKNSKBN-G
               MOVE    Z010-PLUS-SANTEIRRK  TO
                                            SPA-Z010-PLUS-SANTEIRRK-G
               MOVE    Z010-PLUS-JGNCNT     TO  SPA-Z010-PLUS-JGNCNT
               MOVE    Z010-PLUS-JGNCNT1D   TO  SPA-Z010-PLUS-JGNCNT1D
               MOVE    Z010-PLUS-JGNCNT1W   TO  SPA-Z010-PLUS-JGNCNT1W
               MOVE    Z010-PLUS-JGNCNTETCM TO  SPA-Z010-PLUS-JGNCNTETCM
               MOVE    Z010-PLUS-JGNCNTETC  TO  SPA-Z010-PLUS-JGNCNTETC
               MOVE    Z010-PLUS-JGNCNTERR  TO  SPA-Z010-PLUS-JGNCNTERR
           END-IF
      *
           IF      SPA-Z01-SRYCD (1:1)  =   "6"
               MOVE    Z010-BUNRUIKBN       TO  SPA-Z010-BUNRUIKBN-G
               MOVE    Z010-KANZANCHI       TO  SPA-Z010-KANZANCHI
               MOVE    Z010-ALLSURYOKBN     TO  SPA-Z010-ALLSURYOKBN-G
               MOVE    Z010-KANZANTANICD    TO  SPA-Z010-KANZANTANICD-G
               MOVE    Z010-SOURYOKBN       TO  SPA-Z010-SOURYOKBN-G
               MOVE    Z010-IPNKISAIKBN     TO  SPA-Z010-IPN-KISAIKBN-G
               MOVE    Z010-IPNTANICD       TO  SPA-Z010-IPN-TANICD-G
               MOVE    Z010-IPNKANZANCHI    TO  SPA-Z010-IPN-KANZANCHI
      *        湿布薬
               MOVE    Z010-SIPRYOU1        TO  SPA-Z010-SIPRYOU1
               MOVE    Z010-SIPIRISU1       TO  SPA-Z010-SIPIRISU1
               MOVE    Z010-SIPRYOU2        TO  SPA-Z010-SIPRYOU2
               MOVE    Z010-SIPIRISU2       TO  SPA-Z010-SIPIRISU2
      *R02.2   ユーザー単位
               MOVE    Z010-USER-TANICD     TO  SPA-Z010-USER-TANICD-G
           END-IF
      *
      *R02.2   ユーザー単位
           IF      SPA-Z01-SRYCD (1:1)  =   "7"
               MOVE    Z010-USER-TANICD     TO  SPA-Z010-USER-TANICD-G
           END-IF
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    有効開始日
           PERFORM 501-YUKOSTYMDCHK-SEC
      *    有効終了日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 502-YUKOEDYMDCHK-SEC
           END-IF
      *    処方名称
           IF      SPA-ERRCD           =   SPACE
               PERFORM 503-SHONAME-CHK-SEC
           END-IF
      *R02.2
      *    ユーザー単位
           IF     (SPA-ERRCD           =   SPACE)
             AND  (SPA-Z01-SRYCD (1:1) =   "6"
                                       OR  "7"  )
               PERFORM  503-USER-TANICD-SEC
           END-IF
      *
      *    採血料区分
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "1"   )
               PERFORM 5030-SAIKETUKBN-CHK-SEC
           END-IF
      *    前回検査日区分
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "1"   )
               PERFORM 5030-ZENKNSKBN-CHK-SEC
           END-IF
      *    ユーザ設定算定履歴
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "1"   )
               PERFORM 505-PLUS-SANTEIRRK-SEC
           END-IF
      *    ユーザ設定算定履歴
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "1"   )
               PERFORM 5030-JGNCNTERR-CHK-SEC
           END-IF
      *    総量編集
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM  523-SOURYOHENKBN-SEC
           END-IF
      *    一般名記載
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM  523-IPN-KISAIKBN-SEC
           END-IF
      *    一般名単位
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM  523-IPN-TANICD-SEC
           END-IF
      *    分類区分
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM 524-BUNRUIKBNCHK-SEC
           END-IF
      *    全数量記載区分
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM 524-ALLSURYOKBNCHK-SEC
           END-IF
      *    数量換算単位コード
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM 524-KANZANTANICDCHK-SEC
           END-IF
      *    換算値
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM 524-KANZANCHICHK-SEC
           END-IF
      *    湿布薬関係
           IF      SPA-ERRCD           =   SPACE
             AND  (SPA-Z01-SRYCD (1:1) =   "6"   )
               PERFORM  523-SIPPKBN-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       501-YUKOSTYMDCHK-SEC            SECTION.
      *
           IF      WRK-MCP-WIDGET      =   "YUKOSTYMD"
               IF      SPA-Z010-YUKOSTYMD  =   SPACE
      *            点数マスタの開始日
                   MOVE    SPA-Z01-YUKOSTYMD
                                           TO  SPA-Z010-YUKOSTYMD
               END-IF
           END-IF
           IF      SPA-Z010-YUKOSTYMD   =   SPACE
               MOVE    SPACE               TO  SPA-Z010-NAI-YUKOSTYMD
               GO      TO      501-YUKOSTYMDCHK-EXT
           END-IF
      *
           MOVE    SPA-Z010-YUKOSTYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-Z010-NAI-YUKOSTYMD
               MOVE    WRK-LNKYMD          TO  SPA-Z010-YUKOSTYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-Z010-CUR
           END-IF
      *
           IF     (SPA-ERRCD           =  SPACE )
             AND  (SPA-Z01-YUKOSTYMD   NOT =   SPA-Z010-NAI-YUKOSTYMD)
      *        １日であること
               IF     SPA-Z010-NAI-YUKOSTYMD(7:2)  =   "01"
                                                   OR  "00"
                   CONTINUE
               ELSE
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-Z010-CUR
               END-IF
           END-IF
      *
           .
       501-YUKOSTYMDCHK-EXT.
           EXIT.
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       502-YUKOEDYMDCHK-SEC                      SECTION.
      *
           IF      WRK-MCP-WIDGET      =   "YUKOEDYMD"
               IF      SPA-Z010-YUKOEDYMD   =   SPACE
                   MOVE    "9999999"           TO  SPA-Z010-YUKOEDYMD
                   MOVE    SPA-Z01-YUKOEDYMD   TO  SPA-Z010-YUKOEDYMD
               END-IF
           END-IF
           IF      SPA-Z010-YUKOEDYMD   =   SPACE
               GO      TO      502-YUKOEDYMDCHK-EXT
           END-IF
      *
           MOVE    SPA-Z010-YUKOEDYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-Z010-NAI-YUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-Z010-YUKOEDYMD
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-Z010-CUR
               GO      TO      502-YUKOEDYMDCHK-EXT
           END-IF
      *
           IF      SPA-Z010-NAI-YUKOEDYMD  =   "99999999"
               CONTINUE
           ELSE
      *        月末日を求める
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SPA-Z010-NAI-YUKOEDYMD(1:6)
                                           TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-SRYLST-DD
               ELSE
                   MOVE    "31"                TO  WRK-SRYLST-DD
               END-IF
               IF     (SPA-Z010-NAI-YUKOEDYMD(7:2)
                                       NOT =   WRK-SRYLST-DD)
                 AND  ( SPA-Z010-ENDKEIFLG =   ZERO  )
                   MOVE    "K013"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-Z010-CUR
                   MOVE    1                   TO  SPA-Z010-ENDKEIFLG
               END-IF
           END-IF
      *
           .
       502-YUKOEDYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方名称チェック処理
      *****************************************************************
       503-SHONAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-Z010-SHONAME     TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 810-ZENCHK-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-Z010-SHONAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-OUT-INPUT       TO  SPA-Z010-SHONAME
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-Z010-CUR
           END-IF
           .
      *
       503-SHONAME-CHK-EXT.
           EXIT.
      *R02.2
      *****************************************************************
      *    ユーザー単位チェック
      *****************************************************************
       503-USER-TANICD-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-Z010-TANICD-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-Z010-USER-TANICD
                                       =   SPA-Z010-TANICDL(IDX1)
                   MOVE    SPA-Z010-TANICDLST (IDX1)
                                       TO  SPA-Z010-USER-TANICD-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      SPA-Z010-USER-TANICD    =   SPACE
                                           OR  ZERO
      *        ゼロは選択なしとする
               MOVE    SPACE               TO  SPA-Z010-USER-TANICD-G
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "0042"              TO  SPA-ERRCD
                   MOVE    42                  TO  SPA-Z010-CUR
               END-IF
           END-IF
           .
       503-USER-TANICD-EXT.
           EXIT.
      *****************************************************************
      *  採血料区分  チェック
      *****************************************************************
       5030-SAIKETUKBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-SAIKETUKBN-MAX
               IF      SPA-Z010-SAIKETUKBN  =   SPA-Z010-SAIKETUCOMB
                                                            (IDX)(1:1)
                   MOVE    SPA-Z010-SAIKETUCOMB (IDX)
                                           TO  SPA-Z010-SAIKETUKBN-G
                   MOVE    SPA-Z010-SAIKETUKBN-MAX  TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-Z010-CUR
           END-IF
      *    採血料区分は、検査の時のみ入力可能
           IF      SPA-Z010-SAIKETUKBN  NOT =   "0"
               IF      SPA-Z010-SRYKBN     =   "60"
                   CONTINUE
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0007"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-Z010-CUR
                   END-IF
               END-IF
           END-IF
           .    
       5030-SAIKETUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前回検査日チェック
      *****************************************************************
       5030-ZENKNSKBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-ZENKNSKBN-MAX
               IF      SPA-Z010-ZENKNSKBN   =   SPA-Z010-ZENKNSKBN-COMB
                                                            (IDX)(1:1)
                   MOVE    SPA-Z010-ZENKNSKBN-COMB (IDX)
                                           TO  SPA-Z010-ZENKNSKBN-G
                   MOVE    SPA-Z010-ZENKNSKBN-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-Z010-CUR
           END-IF
      *    採血料区分は、検査の時のみ入力可能
           IF      SPA-Z010-ZENKNSKBN   NOT =   "0"
               IF      SPA-Z010-SRYKBN     =   "60"
                   CONTINUE
               ELSE
                   MOVE    SPA-Z010-ZENKNSKBN-COMB (1)
                                               TO  SPA-Z010-ZENKNSKBN-G
               END-IF
           END-IF
           .
       5030-ZENKNSKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  ユーザ設定　算定履歴チェック
      *****************************************************************
       505-PLUS-SANTEIRRK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-PLUS-SANTEI-MAX
               IF      SPA-Z010-PLUS-SANTEIRRK
                                       =   SPA-Z010-PLUS-SANTEICOMB
                                                         (IDX)(1:1)
                   MOVE    SPA-Z010-PLUS-SANTEICOMB (IDX)
                                           TO  SPA-Z010-PLUS-SANTEIRRK-G
                   MOVE    SPA-Z010-PLUS-SANTEI-MAX TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-Z010-CUR
           ELSE
      *        システム予約は指定附加
               IF      SPA-Z010-PLUS-SANTEIRRK  =   1
                   IF      SPA-Z01-SRYCD(1:1)  NOT =   "1"
                       MOVE    "0009"              TO  SPA-ERRCD
                       MOVE    7                   TO  SPA-Z010-CUR
                   END-IF
               END-IF
           END-IF
           .
      *
       505-PLUS-SANTEIRRK-EXT.
           EXIT.
      *****************************************************************
      *  ユーザ設定　エラー処理チェック
      *****************************************************************
       5030-JGNCNTERR-CHK-SEC         SECTION.
      *
           EVALUATE    SPA-Z010-PLUS-JGNCNTERR
               WHEN    ZERO
                   CONTINUE
               WHEN    9
                   IF       SPA-Z010-PLUS-SANTEIRRK =   ZERO
                       MOVE    "0016"              TO  SPA-ERRCD
                       MOVE    7                   TO  SPA-Z010-CUR
                   ELSE
                       IF   (SPA-Z010-PLUS-JGNCNT   =   ZERO )  AND
                            (SPA-Z010-PLUS-JGNCNT1D =   ZERO )  AND
                            (SPA-Z010-PLUS-JGNCNT1W =   ZERO )  AND
                            (SPA-Z010-PLUS-JGNCNTETCM   =   ZERO ) AND
                            (SPA-Z010-PLUS-JGNCNTETC    =   ZERO )
                           MOVE    "0017"              TO  SPA-ERRCD
                           MOVE    8                   TO  SPA-Z010-CUR
                       END-IF
                   END-IF
               WHEN    OTHER
                   MOVE    "0015"              TO  SPA-ERRCD
                   MOVE    13                  TO  SPA-Z010-CUR
           END-EVALUATE
           .
      *
       5030-JGNCNTERR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    総量編集チェック
      *****************************************************************
       523-SOURYOHENKBN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-Z010-SOURYO-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-Z010-SOURYOKBN
                                       =   SPA-Z010-SOURYOKBN-LST
                                                        (IDX1)(1:1)
                   MOVE    SPA-Z010-SOURYOKBN-LST(IDX1)
                                       TO  SPA-Z010-SOURYOKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
      ***  IF      SPA-Z010-SOURYOKBN  =   SPACE
      *        GO    TO      523-SOURYOHENKBN-EXT
      ***  END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE    30                  TO  SPA-Z010-CUR
           ELSE
               IF     (SPA-Z010-SOURYOKBN    NOT =   ZERO )
                   IF     (SPA-Z010-YKZKBN      =   "1"
                                                OR  "3"      )
                       CONTINUE
                   ELSE
                       MOVE    "0026"              TO  SPA-ERRCD
                       MOVE    30                  TO  SPA-Z010-CUR
                   END-IF
               END-IF
           END-IF
           .
       523-SOURYOHENKBN-EXT.
           EXIT.
      *****************************************************************
      *    一般名記載チェック
      *****************************************************************
       523-IPN-KISAIKBN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-Z010-IPN-KISAI-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-Z010-IPN-KISAIKBN
                                       =   SPA-Z010-IPN-KISAIKBN-LST
                                                        (IDX1)(1:1)
                   MOVE    SPA-Z010-IPN-KISAIKBN-LST(IDX1)
                                       TO  SPA-Z010-IPN-KISAIKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
      ***  IF      SPA-Z010-IPN-KISAIKBN   =   SPACE
      *        GO    TO      523-IPN-KISAIKBN-EXT
      ***  END-IF
           IF      FLG-OK              =   ZERO
               MOVE    "0029"              TO  SPA-ERRCD
               MOVE    31                  TO  SPA-Z010-CUR
           END-IF
      *    一般名記載
           IF      SPA-Z010-IPN-KISAIKBN    =   "1"
               IF      SPA-Z010-GENERICNAME    =   SPACE
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    31                  TO  SPA-Z010-CUR
               END-IF
           END-IF
           IF      SPA-Z010-IPN-KISAIKBN    =   "2"
               IF      SPA-Z010-SHONAME         =   SPACE
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    31                  TO  SPA-Z010-CUR
               END-IF
           END-IF
           IF      SPA-Z010-IPN-KISAIKBN    =   "3"
               IF      SPA-Z010-SHONAME         =   SPACE
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    31                  TO  SPA-Z010-CUR
               END-IF
           END-IF
           .
       523-IPN-KISAIKBN-EXT.
           EXIT.
      *****************************************************************
      *    一般名記載単位チェック
      *****************************************************************
       523-IPN-TANICD-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-Z010-TANICD-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-Z010-IPN-TANICD
                                       =   SPA-Z010-TANICDL(IDX1)
                   MOVE    SPA-Z010-TANICDLST (IDX1)
                                       TO  SPA-Z010-IPN-TANICD-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-Z010-IPN-TANICD  =   SPACE
               CONTINUE
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    32                  TO  SPA-Z010-CUR
               END-IF
           END-IF
           .
       523-IPN-TANICD-EXT.
           EXIT.
      *****************************************************************
      *    分類区分チェック
      *****************************************************************
       524-BUNRUIKBNCHK-SEC                    SECTION.
      *
           IF      SPA-Z010-BUNRUIKBN   =   SPACE
               GO  TO  524-BUNRUIKBNCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-Z010-BUNRUIKBN-MAX )
      *
               IF    ( SPA-Z010-BUNRUIKBN
                                  =    SPA-Z010-BUNRUIKBNL (IDX1))
                   MOVE    SPA-Z010-BUNRUIKBNLST (IDX1)
                                       TO   SPA-Z010-BUNRUIKBN-G
                   MOVE    1           TO   FLG-OK
                   MOVE    SPA-Z010-BUNRUIKBN-MAX
                                       TO   IDX1
               END-IF
      *
           END-PERFORM
      *
           IF     (SPA-Z010-BUNRUIKBN-MAX   >   ZERO  )  AND
                  (FLG-OK                  =   ZERO )
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    34                  TO  SPA-Z010-CUR
           END-IF
      *
           .
       524-BUNRUIKBNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全数量記載区分チェック
      *****************************************************************
       524-ALLSURYOKBNCHK-SEC      SECTION.
      *
           IF      SPA-Z010-ALLSURYOKBN   =   SPACE
               GO  TO  524-ALLSURYOKBNCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
      *
               IF    ( SPA-Z010-ALLSURYOKBN
                                  =    SPA-Z010-ALLSURYOKBNL (IDX1))
                   MOVE    SPA-Z010-ALLSURYOKBNLST (IDX1)
                                       TO   SPA-Z010-ALLSURYOKBN-G
                   MOVE    1           TO   FLG-OK
                   MOVE    2           TO   IDX1
               END-IF
      *
           END-PERFORM
      *
           IF     (FLG-OK                  =   ZERO )
               MOVE    "0036"              TO  SPA-ERRCD
               MOVE    35                  TO  SPA-Z010-CUR
           END-IF
      *
           .
       524-ALLSURYOKBNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量換算単位コードチェック
      *****************************************************************
       524-KANZANTANICDCHK-SEC     SECTION.
      *
           IF      SPA-Z010-KANZANTANICD   =   SPACE
               GO  TO  524-KANZANTANICDCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-Z010-TANICD-MAX )
      *
               IF    ( SPA-Z010-KANZANTANICD
                                  =    SPA-Z010-TANICDL (IDX1))
                   MOVE    SPA-Z010-TANICDLST (IDX1)
                                       TO   SPA-Z010-KANZANTANICD-G
                   MOVE    1           TO   FLG-OK
                   MOVE    SPA-Z010-TANICD-MAX
                                       TO   IDX1
               END-IF
      *
           END-PERFORM
      *
           IF     (SPA-Z010-TANICD-MAX      >   ZERO  )  AND
                  (FLG-OK                  =   ZERO )
               MOVE    "0037"              TO  SPA-ERRCD
               MOVE    36                  TO  SPA-Z010-CUR
           END-IF
      *
      *    単位コードと一致しないこと
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z010-KANZANTANICD    =   SPA-Z010-TANICD
                   MOVE    "0038"              TO  SPA-ERRCD
                   MOVE    36                  TO  SPA-Z010-CUR
               END-IF
           END-IF
      *    単位コード 000 はエラー
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z010-KANZANTANICD    =   "000"
                   MOVE    "0039"              TO  SPA-ERRCD
                   MOVE    36                  TO  SPA-Z010-CUR
               END-IF
           END-IF
           .
       524-KANZANTANICDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    換算値チェック
      *****************************************************************
       524-KANZANCHICHK-SEC        SECTION.
      *
           .
       524-KANZANCHICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    湿布薬チェック
      *****************************************************************
       523-SIPPKBN-CHK-SEC               SECTION.
      *
           IF      SPA-Z010-SIPPKBN    NOT =    "1"
      *        湿布薬以外はZERO
               IF      (SPA-Z010-SIPRYOU1    =   ZERO )
                   AND (SPA-Z010-SIPIRISU1   =   ZERO )
                   AND (SPA-Z010-SIPRYOU2    =   ZERO )
                   AND (SPA-Z010-SIPIRISU2   =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  SPA-Z010-SIPRYOU1
                   MOVE    ZERO            TO  SPA-Z010-SIPIRISU1
                   MOVE    ZERO            TO  SPA-Z010-SIPRYOU2
                   MOVE    ZERO            TO  SPA-Z010-SIPIRISU2
               END-IF
           END-IF
           .
       523-SIPPKBN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           IF     (SPA-Z010-SYORI       =   1 )
              OR  (SPA-Z010-NAI-SELNUM      =   ZERO )
               MOVE    "1003"          TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0001"              TO  SPA-ZIDCD
           END-IF
      *
           .
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           PERFORM 40311-TENSUPLUS-DEL-SEC
      *
           IF     SPA-ERRCD            =   SPACE
      *        初期表示
               PERFORM 3101-INIT-GMN-SEC
               INITIALIZE                  SPA-Z010-INPUT-G
               INITIALIZE                  SPA-Z010-KIKAN
               MOVE    SPACE               TO  SPA-Z010-SELNUM
               MOVE    ZERO                TO  SPA-Z010-NAI-SELNUM
               PERFORM 5021-INIT-HEN-SEC
      *        新規
               MOVE    1                   TO  SPA-Z010-SYORI
               MOVE    1                   TO  SPA-Z010-CUR
           END-IF
      *
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報削除処理
      *****************************************************************
       40311-TENSUPLUS-DEL-SEC         SECTION.
      *
      *    点数付加情報削除
           MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
           MOVE    SPA-Z010-NAI-TYUKOSTYMD(SPA-Z010-NAI-SELNUM)
                                       TO  TNSP-YUKOSTYMD
           MOVE    SPA-Z010-NAI-TYUKOEDYMD(SPA-Z010-NAI-SELNUM)
                                       TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z010 TENSUPLUS DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
           .
       40311-TENSUPLUS-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                           SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-Z010-CUR
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0002"              TO  SPA-ZIDCD
           END-IF
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-Z010-YUKOSTYMD   =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-Z010-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-Z010-YUKOEDYMD   =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-Z010-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-Z010-NAI-YUKOSTYMD   >=  SPA-Z010-NAI-YUKOEDYMD
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-Z010-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    有効終了日の月末日再チェック
           IF      SPA-Z010-NAI-YUKOEDYMD  =   "99999999"
               CONTINUE
           ELSE
      *        月末日を求める
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SPA-Z010-NAI-YUKOEDYMD(1:6)
                                           TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-SRYLST-DD
               ELSE
                   MOVE    "31"                TO  WRK-SRYLST-DD
               END-IF
               IF     (SPA-Z010-NAI-YUKOEDYMD(7:2)
                                       NOT =   WRK-SRYLST-DD)
                 AND  ( SPA-Z010-ENDKEIFLG NOT =   2   )
                   MOVE    "K013"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-Z010-CUR
                   MOVE    2                   TO  SPA-Z010-ENDKEIFLG
               END-IF
           END-IF
      *
      *
      *    有効期限範囲チェック
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING    IDX  FROM    1   BY  1
                   UNTIL      IDX      >   SPA-Z010-RRKMAX
               IF     IDX              =   SPA-Z010-NAI-SELNUM
                   CONTINUE
               ELSE
                   IF     (SPA-Z010-NAI-YUKOEDYMD
                                       <   SPA-Z010-NAI-TYUKOSTYMD
                                                             (IDX))
                     OR   (SPA-Z010-NAI-YUKOSTYMD
                                       >   SPA-Z010-NAI-TYUKOEDYMD
                                                             (IDX))
                       CONTINUE
                   ELSE
                       MOVE    "1011"        TO  SPA-ERRCD
                       MOVE    2             TO  SPA-Z010-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
      *    診療行為情報
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SRYCD(1:1)  =   "1"
                   PERFORM 41091-TOUROKU-CHK01-SEC
               END-IF
           END-IF
      *    薬剤情報
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SRYCD(1:1)  =   "6"
                   PERFORM 41091-TOUROKU-CHK02-SEC
               END-IF
           END-IF
      *    器材情報
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SRYCD(1:1)  =   "7"
                   PERFORM 41091-TOUROKU-CHK03-SEC
               END-IF
           END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報チェック処理
      *****************************************************************
       41091-TOUROKU-CHK01-SEC         SECTION.
      *
      *    前回検査日と算定履歴作成有無判定
           IF      SPA-Z010-ZENKNSKBN   NOT =   ZERO
               IF      (SPA-ERRCD          =   SPACE )
                   AND (SPA-Z010-NAI-KEIFLG3    =   ZERO  )
                   IF     (SPA-Z010-SANTEIRRKKBN   =   ZERO )
                     AND  (SPA-Z010-PLUS-SANTEIRRK  =   ZERO )
                       MOVE    "K100"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-Z010-NAI-KEIFLG3
                       MOVE    6               TO  SPA-Z010-CUR
                   END-IF
               END-IF
           END-IF
      *    ユーザ設定の上限回数
           IF       SPA-Z010-PLUS-SANTEIRRK NOT =   ZERO
               IF     (SPA-Z010-JGNCNT   =   ZERO )  AND
                      (SPA-Z010-JGNCNT1D =   ZERO )
                   CONTINUE
               ELSE
                   IF    (SPA-Z010-JGNCNT   NOT =   ZERO )
                     AND (SPA-Z010-PLUS-JGNCNT  =   ZERO )
                     AND (SPA-Z010-PLUS-JGNCNTETC    =   ZERO )
                     AND (SPA-Z010-PLUS-JGNCNT1W     =   ZERO )
                     AND (SPA-ERRCD                 =   SPACE )
                       MOVE    "0018"              TO  SPA-ERRCD
                       MOVE    7                   TO  SPA-Z010-CUR
                   END-IF
                   IF    (SPA-Z010-JGNCNT1D    NOT =   ZERO )
                     AND (SPA-Z010-PLUS-JGNCNT1D    =   ZERO )
                     AND (SPA-ERRCD                =   SPACE )
                       MOVE    "0018"              TO  SPA-ERRCD
                       MOVE    9                   TO  SPA-Z010-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF      (SPA-Z010-SHONAME       =   SPACE )
               AND (SPA-Z010-SAIKETUKBN    =   ZERO  )
               AND (SPA-Z010-ZENKNSKBN     =   ZERO  )
               AND (SPA-Z010-PLUS-SANTEIRRK    =   ZERO )
               AND (SPA-Z010-PLUS-JGNCNT   =   ZERO )
               AND (SPA-Z010-PLUS-JGNCNT1D     =   ZERO )
               AND (SPA-Z010-PLUS-JGNCNT1W     =   ZERO )
               AND (SPA-Z010-PLUS-JGNCNTETCM   =   ZERO )
               AND (SPA-Z010-PLUS-JGNCNTETC    =   ZERO )
               AND (SPA-Z010-PLUS-JGNCNTERR    =   ZERO )
      *
               IF     (SPA-Z010-SYORI       =   1 )
                  OR  (SPA-Z010-NAI-SELNUM      =   ZERO )
      *           新規
                   MOVE    "0019"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-Z010-CUR
               END-IF
           END-IF
           .
      *
       41091-TOUROKU-CHK01-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報チェック処理
      *****************************************************************
       41091-TOUROKU-CHK02-SEC          SECTION.
      *
      *R02.2
      *    ユーザー単位とマスタ単位チェック
           IF      (SPA-Z010-USER-TANICD   NOT =   SPACE )
             AND   (SPA-Z010-TANIKEIFLG    =   ZERO )
               PERFORM 41091-MASTERTANICD-CHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  41091-TOUROKU-CHK02-EXT
           END-IF
      *
      *    単位と換算値
           IF     ((SPA-Z010-IPN-TANICD         =   SPACE) AND
                   (SPA-Z010-IPN-KANZANCHI  NOT =   ZERO ))  OR
                  ((SPA-Z010-IPN-TANICD     NOT =   SPACE) AND
                   (SPA-Z010-IPN-KANZANCHI      =   ZERO ))
               MOVE    "0034"              TO  SPA-ERRCD
               MOVE    32                  TO  SPA-Z010-CUR
           END-IF
      *
      *    換算値単位の判定
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z010-KANZANTANICD        =   SPACE
                                                   OR  "0000"
                   IF      SPA-Z010-KANZANCHI   NOT =   ZERO
                       MOVE    "0040"              TO  SPA-ERRCD
                       MOVE    37                  TO  SPA-Z010-CUR
                   END-IF
               ELSE
                   IF      SPA-Z010-KANZANCHI       =   ZERO
                       MOVE    "0041"              TO  SPA-ERRCD
                       MOVE    37                  TO  SPA-Z010-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF      (SPA-Z010-SHONAME       =   SPACE )
               AND (SPA-Z010-BUNRUIKBN     =   SPACE )
               AND (SPA-Z010-ALLSURYOKBN   =   SPACE )
               AND (SPA-Z010-KANZANTANICD  =   SPACE )
               AND (SPA-Z010-KANZANCHI     =   ZERO )
      *****    AND (SPA-Z010-SOURYOKBN     =   SPACE)
      *****    AND (SPA-Z010-IPN-KISAIKBN  =   SPACE)
               AND (SPA-Z010-SOURYOKBN     =   ZERO )
               AND (SPA-Z010-IPN-KISAIKBN  =   ZERO )
               AND (SPA-Z010-IPN-TANICD    =   SPACE)
               AND (SPA-Z010-IPN-KANZANCHI =   ZERO )
               AND (SPA-Z010-SIPRYOU1      =   ZERO )
               AND (SPA-Z010-SIPIRISU1     =   ZERO )
               AND (SPA-Z010-SIPRYOU2      =   ZERO )
               AND (SPA-Z010-SIPIRISU2     =   ZERO )
               AND (SPA-Z010-USER-TANICD   =   SPACE)
      *
               IF     (SPA-Z010-SYORI       =   1 )
                  OR  (SPA-Z010-NAI-SELNUM      =   ZERO )
      *           新規
                   MOVE    "0019"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-Z010-CUR
               END-IF
           END-IF
           .
       41091-TOUROKU-CHK02-EXT.
           EXIT.
      *****************************************************************
      *    器材情報チェック処理
      *****************************************************************
       41091-TOUROKU-CHK03-SEC          SECTION.
      *R02.2
      *    ユーザー単位とマスタ単位チェック
           IF      (SPA-Z010-USER-TANICD   NOT =   SPACE )
             AND   (SPA-Z010-TANIKEIFLG    =   ZERO )
               PERFORM 41091-MASTERTANICD-CHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  41091-TOUROKU-CHK03-EXT
           END-IF
      *
           IF      (SPA-Z010-SHONAME       =   SPACE)
               AND (SPA-Z010-USER-TANICD   =   SPACE)
               IF     (SPA-Z010-SYORI       =   1 )
                  OR  (SPA-Z010-NAI-SELNUM      =   ZERO )
      *           新規
                   MOVE    "0019"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-Z010-CUR
              END-IF
           END-IF
           .
       41091-TOUROKU-CHK03-EXT.
           EXIT.
      *R02.2
      *****************************************************************
      *    ユーザー単位とマスタ単位チェック処理
      *****************************************************************
       41091-MASTERTANICD-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
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
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    SPACE               TO  WRK-CHK-TANICD
           PERFORM UNTIL      (FLG-TENSU   =   1  )
               IF     (TNS-YUKOSTYMD       >   SPA-Z010-NAI-YUKOEDYMD)
                   OR (TNS-YUKOEDYMD       <   SPA-Z010-NAI-YUKOSTYMD)
                   CONTINUE
               ELSE
                   MOVE    TNS-TANICD      TO  WRK-CHK-TANICD
                   MOVE    1               TO  FLG-TENSU
               END-IF
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-CHK-TANICD      =   ZERO
                                       OR  SPACE
               CONTINUE
           ELSE
               MOVE    "K042"          TO  SPA-ERRCD
               MOVE    42              TO  SPA-Z010-CUR
               MOVE    1               TO  SPA-Z010-TANIKEIFLG
           END-IF
           .
      *
       41091-MASTERTANICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-TOUROKU-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    点数マスタ付加情報登録処理
           PERFORM 41211-TENSUPLUS-UPD-SEC
      *
           IF     SPA-ERRCD            =   SPACE
      *        初期表示
               PERFORM 3101-INIT-GMN-SEC
               INITIALIZE                  SPA-Z010-INPUT-G
               INITIALIZE                  SPA-Z010-KIKAN
               MOVE    SPACE               TO  SPA-Z010-SELNUM
               MOVE    ZERO                TO  SPA-Z010-NAI-SELNUM
               PERFORM 5021-INIT-HEN-SEC
      *        新規
               MOVE    1                   TO  SPA-Z010-SYORI
               MOVE    1                   TO  SPA-Z010-CUR
           END-IF
           .
      *
       4121-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報登録処理
      *****************************************************************
       41211-TENSUPLUS-UPD-SEC                SECTION.
      *
      *    点数マスタ付加情報検索
           IF      SPA-Z010-NAI-SELNUM      >   ZERO
               MOVE    SPACE               TO  TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-Z010-NAI-TYUKOSTYMD(SPA-Z010-NAI-SELNUM)
                                           TO  TNSP-YUKOSTYMD
               MOVE    SPA-Z010-NAI-TYUKOEDYMD(SPA-Z010-NAI-SELNUM)
                                           TO  TNSP-YUKOEDYMD
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSUPLUS
               END-IF
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           ELSE
      *        新規
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
      *
           EVALUATE    SPA-Z010-SYORI
               WHEN    1
      *        新規
                   IF      FLG-TENSUPLUS       =   ZERO
                       MOVE    "9003"          TO  SPA-ERRCD
                   END-IF
               WHEN    0
      *        更新
                   IF      FLG-TENSUPLUS       =   1
                       MOVE    "9002"          TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41211-TENSUPLUS-UPD-EXT
           END-IF
      *
           EVALUATE    SPA-Z010-SYORI
      *    新規
           WHEN    1
               INITIALIZE                  TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-Z010-NAI-YUKOSTYMD   TO  TNSP-YUKOSTYMD
               MOVE    SPA-Z010-NAI-YUKOEDYMD   TO  TNSP-YUKOEDYMD
               PERFORM 4001-TENSUPLUS-HEN-SEC
      *
               MOVE    SPA-OPID            TO  TNSP-OPID
               MOVE    SMCNDATE-YMD        TO  TNSP-CREYMD
               MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   MOVE    "9003"          TO  SPA-ERRCD
                   DISPLAY "Z010 TENSUPLUS INS ERR:" MCP-RC
                          ",KEY:" TNSP-KEY
               END-IF
      *    更新
           WHEN    0
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-Z010-NAI-YUKOSTYMD   TO  TNSP-YUKOSTYMD
               MOVE    SPA-Z010-NAI-YUKOEDYMD   TO  TNSP-YUKOEDYMD
               PERFORM 4001-TENSUPLUS-HEN-SEC
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSP-UP-SRYCD
               MOVE    SPA-Z010-NAI-TYUKOSTYMD(SPA-Z010-NAI-SELNUM)
                                           TO  TNSP-UP-YUKOSTYMD
               MOVE    SPA-Z010-NAI-TYUKOEDYMD(SPA-Z010-NAI-SELNUM)
                                           TO  TNSP-UP-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
      *
               MOVE    SPA-OPID            TO  TNSP-OPID
               MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "9002"          TO  SPA-ERRCD
                   DISPLAY "Z010 TENSUPLUS UPD ERR:" MCP-RC
                                  ",KEY:" TNSP-KEY
               END-IF
           END-EVALUATE
           .
      *
       41211-TENSUPLUS-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタ付加情報更新処理
      *****************************************************************
       4001-TENSUPLUS-HEN-SEC          SECTION.
      *
           MOVE    SPA-Z010-SHONAME     TO  TNSP-SHONAME
                                           WRK-STRINGTBL
           MOVE    200                 TO  CNT-STRING
           PERFORM 891-STRCOUNT-SEC
           MOVE    CNT-STRING          TO  TNSP-SHOYUKOKETA
      *
      *    採血料区分
           IF      SPA-Z010-SAIKETUKBN  =   SPACE
               MOVE    ZERO                 TO  TNSP-SAIKETUKBN
           ELSE
               MOVE    SPA-Z010-SAIKETUKBN  TO  TNSP-SAIKETUKBN
           END-IF
      *    前回検査　
           IF      SPA-Z010-ZENKNSKBN   =   SPACE
               MOVE    ZERO                 TO  TNSP-ZENKNSKBN
           ELSE
               MOVE    SPA-Z010-ZENKNSKBN   TO  TNSP-ZENKNSKBN
           END-IF
           MOVE    SPA-Z010-PLUS-SANTEIRRK
                                       TO  TNSP-SANTEIRRKKBN
           MOVE    SPA-Z010-PLUS-JGNCNT
                                       TO  TNSP-JGNCNT
           MOVE    SPA-Z010-PLUS-JGNCNT1D
                                       TO  TNSP-JGNCNT1D
           MOVE    SPA-Z010-PLUS-JGNCNT1W
                                       TO  TNSP-JGNCNT1W
           MOVE    SPA-Z010-PLUS-JGNCNTETCM
                                       TO  TNSP-JGNCNTETCM
           MOVE    SPA-Z010-PLUS-JGNCNTETC
                                       TO  TNSP-JGNCNTETC
           MOVE    SPA-Z010-PLUS-JGNCNTERR
                                       TO  TNSP-JGNCNTERR
      *    他月数
           MOVE    SPA-Z010-PLUS-JGNCNTETCM
                                       TO  TNSP-JGNCNTETCM
      *    他回数
           MOVE    SPA-Z010-PLUS-JGNCNTETC
                                        TO  TNSP-JGNCNTETC
      *
           MOVE    SPA-Z010-BUNRUIKBN   TO  TNSP-TENPUYAKKBN
           MOVE    SPA-Z010-ALLSURYOKBN TO  TNSP-ALLSURYOKBN
           MOVE    SPA-Z010-KANZANTANICD    TO  TNSP-KANZANTANICD
           MOVE    SPA-Z010-KANZANTANICDMEI TO  TNSP-KANZANTANINAME
           MOVE    SPA-Z010-KANZANCHI       TO  TNSP-KANZANCHI
      *    総量編集
           IF      SPA-Z010-SOURYOKBN   =   SPACE
               MOVE    ZERO                     TO  TNSP-SOURYOHENKBN
           ELSE
               MOVE    SPA-Z010-SOURYOKBN       TO  TNSP-SOURYOHENKBN
           END-IF
      *    一般名記載区分
           IF      SPA-Z010-IPN-KISAIKBN    =   SPACE
               MOVE    ZERO                     TO  TNSP-IPN-KISAIKBN 
           ELSE
               MOVE    SPA-Z010-IPN-KISAIKBN    TO  TNSP-IPN-KISAIKBN 
           END-IF
           MOVE    SPA-Z010-IPN-TANICD      TO  TNSP-IPN-KANZANTANICD 
           MOVE    SPA-Z010-IPN-TANICDMEI   TO  TNSP-IPN-KANZANTANINAME 
           MOVE    SPA-Z010-IPN-KANZANCHI   TO  TNSP-IPN-KANZANCHI
      *    湿布薬区分
           MOVE    SPA-Z010-SIPRYOU1        TO  TNSP-SIPPURYO1
           MOVE    SPA-Z010-SIPIRISU1       TO  TNSP-SIPPUIRISU1
           MOVE    SPA-Z010-SIPRYOU2        TO  TNSP-SIPPURYO2
           MOVE    SPA-Z010-SIPIRISU2       TO  TNSP-SIPPUIRISU2
      *R02.2
      *    ユーザー単位
           MOVE    SPA-Z010-USER-TANICD     TO  TNSP-USERTANICD
           MOVE    SPA-Z010-USER-TANICDMEI  TO  TNSP-USERTANINAME
      *
           .
       4001-TENSUPLUS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-Z010-MOTOPG       TO  SPA-SAKIPG
      *
           INITIALIZE                    SPA-Z010
      *
           MOVE    "Z010"                TO  SPA-MOTOPG
      *
           MOVE    "JOIN"                TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC                     SECTION.
      *
           INITIALIZE                  SPA-Z010-INPUT-G
           INITIALIZE                  SPA-Z010-KIKAN
           MOVE    SPACE               TO  SPA-Z010-SELNUM
           MOVE    ZERO                TO  SPA-Z010-NAI-SELNUM
      *    リスト編集
           MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           PERFORM 3102-LIST-HENSYU-SEC
      *
           MOVE    1                   TO  SPA-Z010-CUR
      *    新規
           MOVE    1                   TO  SPA-Z010-SYORI
           .
       402-CLEAR-EXT.
           EXIT.
      *
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
           IF      SPA-ZIDCD       NOT =   SPACE
               PERFORM 520-ZIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z010"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  Z010
      *
           INITIALIZE                      Z010
           MOVE    SPA-Z010-MIDASHI    TO  Z010-MIDASHI-DATA
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "red"               TO  Z010-MIDASHI-STYLE
           END-IF
           MOVE    SPA-Z010-MEISHO     TO  Z010-MEISHO-DATA
      *
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  SPA-Z010-RRKMAX
               MOVE    SPA-Z010-TRENNUM (IDX)    TO  WRK-Z9
               MOVE    WRK-Z9              TO  Z010-TRENNUM (IDX)
               MOVE    SPA-Z010-TYUKOSTYMD(IDX)
                                           TO  Z010-TYUKOSTYMD(IDX)
               MOVE    SPA-Z010-TYUKOEDYMD(IDX)
                                           TO  Z010-TYUKOEDYMD(IDX)
               IF      SPA-Z010-NAI-SELNUM      =   IDX
                   MOVE    "T"                 TO  Z010-RRKLIST-SELECT
                                                               (IDX)
               ELSE
                   MOVE    "F"                 TO  Z010-RRKLIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-Z010-RRKMAX      TO  Z010-RRKLIST-COUNT
      *
           MOVE    SPA-Z010-SELNUM      TO  Z010-SELNUM
           MOVE    SPA-Z010-YUKOSTYMD   TO  Z010-YUKOSTYMD
           MOVE    SPA-Z010-YUKOEDYMD   TO  Z010-YUKOEDYMD
      *
           MOVE    SPA-Z010-SHONAME     TO  Z010-SHONAME
      *    採血料区分
           MOVE    SPA-Z010-SAIKETUKBN-G    TO  Z010-SAIKETUKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-SAIKETUKBN-MAX
               MOVE    SPA-Z010-SAIKETUCOMB (IDX)
                                           TO  Z010-SAIKETU-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-Z010-SAIKETUKBN-MAX  TO  Z010-SAIKETU-COUNT
      *    前回検査日区分
           MOVE  SPA-Z010-ZENKNSKBN-G   TO  Z010-ZENKNSKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-ZENKNSKBN-MAX
               MOVE    SPA-Z010-ZENKNSKBN-COMB (IDX)
                                          TO  Z010-ZENKNSKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-Z010-ZENKNSKBN-MAX   TO  Z010-ZENKNSKBN-COUNT
      *    ユーザ設定上限回数
           MOVE  SPA-Z010-PLUS-SANTEIRRK-G  TO  Z010-PLUS-SANTEIRRK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-PLUS-SANTEI-MAX
               MOVE    SPA-Z010-PLUS-SANTEICOMB (IDX)
                                       TO  Z010-PLUS-SANTEI-ITEM (IDX)
           END-PERFORM
           MOVE  SPA-Z010-PLUS-SANTEI-MAX   TO  Z010-PLUS-SANTEI-COUNT
           MOVE  SPA-Z010-PLUS-JGNCNT       TO  Z010-PLUS-JGNCNT
           MOVE  SPA-Z010-PLUS-JGNCNT1D     TO  Z010-PLUS-JGNCNT1D
           MOVE  SPA-Z010-PLUS-JGNCNT1W     TO  Z010-PLUS-JGNCNT1W
           MOVE  SPA-Z010-PLUS-JGNCNTETCM   TO  Z010-PLUS-JGNCNTETCM
           MOVE  SPA-Z010-PLUS-JGNCNTETC    TO  Z010-PLUS-JGNCNTETC
           MOVE  SPA-Z010-PLUS-JGNCNTERR    TO  Z010-PLUS-JGNCNTERR 
      *
      *    分類区分
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-Z010-BUNRUIKBN-MAX
               MOVE    SPA-Z010-BUNRUIKBNLST(IDX)
                                       TO  Z010-BUNRUIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-Z010-BUNRUIKBN-MAX
                                       TO  Z010-BUNRUIKBN-COUNT
           MOVE    SPA-Z010-BUNRUIKBN-G TO  Z010-BUNRUIKBN
      *
      *    全数量記載区分
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-Z010-ALLSURYOKBN-MAX
               MOVE    SPA-Z010-ALLSURYOKBNLST(IDX)
                                       TO  Z010-ALLSURYOKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-Z010-ALLSURYOKBN-MAX TO  Z010-ALLSURYOKBN-COUNT
           MOVE    SPA-Z010-ALLSURYOKBN-G   TO  Z010-ALLSURYOKBN
      *
      *    数量換算単位コード
           MOVE    ZERO                TO  IDX
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-Z010-TANICD-MAX
               IF      SPA-Z010-TANICDLST(IDXX)  NOT =   SPACE
                   ADD     1               TO  IDX
                   MOVE    SPA-Z010-TANICDLST(IDXX)
                                           TO  Z010-KANZANTANICD-ITEM
                                                               (IDX)
                   MOVE    IDX             TO  Z010-KANZANTANICD-COUNT
               END-IF
           END-PERFORM
           MOVE    SPA-Z010-KANZANTANICD-G  TO  Z010-KANZANTANICD
      *
      *    数量換算値
           MOVE  SPA-Z010-KANZANCHI     TO  Z010-KANZANCHI
      *    総量編集
           MOVE    SPA-Z010-SOURYOKBN-G TO  Z010-SOURYOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-SOURYO-MAX
               MOVE    SPA-Z010-SOURYOKBN-LST (IDX)
                                       TO  Z010-SOURYOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-Z010-SOURYO-MAX  TO  Z010-SOURYOKBN-COUNT
      *
           MOVE    SPA-Z010-IPN-KISAIKBN-G  TO  Z010-IPNKISAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-IPN-KISAI-MAX
               MOVE    SPA-Z010-IPN-KISAIKBN-LST (IDX)
                                       TO  Z010-IPNKISAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-Z010-IPN-KISAI-MAX   TO  Z010-IPNKISAIKBN-COUNT
      *
           MOVE    SPA-Z010-IPN-TANICD-G    TO  Z010-IPNTANICD 
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-TANICD-MAX
               MOVE    SPA-Z010-TANICDLST(IDX)
                                       TO  Z010-IPNTANICD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-Z010-TANICD-MAX  TO  Z010-IPNTANICD-COUNT
      *
           MOVE    SPA-Z010-IPN-KANZANCHI   TO  Z010-IPNKANZANCHI
      *    湿布薬
           MOVE    SPA-Z010-SIPRYOU1    TO  Z010-SIPRYOU1
           MOVE    SPA-Z010-SIPIRISU1   TO  Z010-SIPIRISU1
           MOVE    SPA-Z010-SIPRYOU2    TO  Z010-SIPRYOU2
           MOVE    SPA-Z010-SIPIRISU2   TO  Z010-SIPIRISU2
      *R02.2
      *    ユーザー単位
           MOVE    SPA-Z010-USER-TANICD-G    TO  Z010-USER-TANICD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z010-TANICD-MAX
               MOVE    SPA-Z010-TANICDLST(IDX)
                                       TO  Z010-USER-TANICD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-Z010-TANICD-MAX TO  Z010-USER-TANICD-COUNT
      *
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
           IF      SPA-Z010-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-Z010-CUR
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "YUKOSTYMD"     TO  MCP-WIDGET
               WHEN    03
                   MOVE    "YUKOEDYMD"     TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SHONAME"       TO  MCP-WIDGET
      *R02.2
               WHEN    42
                   MOVE    "USER_TANICD"   TO  MCP-WIDGET
      *
               WHEN    05
                   MOVE    "SAIKETUKBN"    TO  MCP-WIDGET
               WHEN    06
                   MOVE    "ZENKNSKBN"     TO  MCP-WIDGET
               WHEN    07
                   MOVE    "PLUS_SANTEIRRK"   TO  MCP-WIDGET
               WHEN    08
                   MOVE    "PLUS_JGNCNT1"      TO  MCP-WIDGET
               WHEN    09
                   MOVE    "PLUS_JGNCNT1D"     TO  MCP-WIDGET
               WHEN    10
                   MOVE    "PLUS_JGNCNT1W"     TO  MCP-WIDGET
               WHEN    11
                   MOVE    "PLUS_JGNCNTETCM"   TO  MCP-WIDGET
               WHEN    12
                   MOVE    "PLUS_JGNCNTETC"    TO  MCP-WIDGET
               WHEN    13
                   MOVE    "PLUS_JGNCNTERR"    TO  MCP-WIDGET
               WHEN    30
                   MOVE    "SOURYOKBN"         TO  MCP-WIDGET
               WHEN    31
                   MOVE    "IPNKISAIKBN"       TO  MCP-WIDGET
               WHEN    32
                   MOVE    "IPNTANICD"         TO  MCP-WIDGET
               WHEN    33
                   MOVE    "IPNKANZANCHI"      TO  MCP-WIDGET
               WHEN    34
                   MOVE    "BUNRUIKBN"         TO  MCP-WIDGET
               WHEN    35
                   MOVE    "ALLSURYOKBN"       TO  MCP-WIDGET
               WHEN    36
                   MOVE    "KANZANTANICD"      TO  MCP-WIDGET
               WHEN    37
                   MOVE    "KANZANCHI"         TO  MCP-WIDGET
               WHEN    38
                   MOVE    "SIPRYOU1"          TO  MCP-WIDGET
               WHEN    39
                   MOVE    "SIPIRISU1"         TO  MCP-WIDGET
               WHEN    40
                   MOVE    "SIPRYOU2"          TO  MCP-WIDGET
               WHEN    41
                   MOVE    "SIPIRISU2"         TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルセット処理
      *****************************************************************
       50011-CUR-SET-SEC                            SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2           TO  SPA-Z010-CUR
               WHEN    "YUKOSTYMD"
                   MOVE    3           TO  SPA-Z010-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    4           TO  SPA-Z010-CUR
               WHEN    "SHONAME"
                   MOVE    5            TO  SPA-Z010-CUR
                   EVALUATE    SPA-Z01-SRYCD (1:1)
                   WHEN    "6"
                   WHEN    "7"
      *R02.2           MOVE    30           TO  SPA-Z010-CUR
                       MOVE    42           TO  SPA-Z010-CUR
                   WHEN    "1"
                       MOVE    5            TO  SPA-Z010-CUR
                   WHEN    OTHER
                       MOVE    90           TO  SPA-Z010-CUR
                   END-EVALUATE
      *R02.2
               WHEN    "USER_TANICD"
                   EVALUATE    SPA-Z01-SRYCD (1:1)
                   WHEN    "6"
                       MOVE    30           TO  SPA-Z010-CUR
                   WHEN    OTHER
                       MOVE    90           TO  SPA-Z010-CUR
                   END-EVALUATE
      *
               WHEN    "SAIKETUKBN"
                   MOVE    6           TO  SPA-Z010-CUR
               WHEN    "ZENKNSKBN"
                   MOVE    7           TO  SPA-Z010-CUR
               WHEN    "PLUS_SANTEIRRK"
                   MOVE    8           TO  SPA-Z010-CUR
               WHEN    "PLUS_JGNCNT1"
                   MOVE    9           TO  SPA-Z010-CUR
               WHEN    "PLUS_JGNCNT1D"
                   MOVE    10          TO  SPA-Z010-CUR
               WHEN    "PLUS_JGNCNT1W"
                   MOVE    11          TO  SPA-Z010-CUR
               WHEN    "PLUS_JGNCNTETCM"
                   MOVE    12          TO  SPA-Z010-CUR
               WHEN    "PLUS_JGNCNTETC"
                   MOVE    13          TO  SPA-Z010-CUR
               WHEN    "PLUS_JGNCNTERR"
                   MOVE    90          TO  SPA-Z010-CUR
      *
               WHEN    "SOURYOKBN"
                   MOVE    31          TO  SPA-Z010-CUR
               WHEN    "IPNKISAIKBN"
                   MOVE    32          TO  SPA-Z010-CUR
               WHEN    "IPNTANICD"
                   MOVE    33          TO  SPA-Z010-CUR
               WHEN    "IPNKANZANCHI"
                   MOVE    34          TO  SPA-Z010-CUR
               WHEN    "BUNRUIKBN"
                   MOVE    35          TO  SPA-Z010-CUR
               WHEN    "ALLSURYOKBN"
                   MOVE    36          TO  SPA-Z010-CUR
               WHEN    "KANZANTANICD"
                   MOVE    37          TO  SPA-Z010-CUR
               WHEN    "KANZANCHI"
                   IF      SPA-Z010-SIPPKBN    =   "1"
                       MOVE    38          TO  SPA-Z010-CUR
                   ELSE
                       MOVE    90          TO  SPA-Z010-CUR
                   END-IF
      *        湿布薬
               WHEN    "SIPRYOU1"
                   MOVE    39          TO  SPA-Z010-CUR
               WHEN    "SIPIRISU1"
                   MOVE    40          TO  SPA-Z010-CUR
               WHEN    "SIPRYOU2"
                   MOVE    41          TO  SPA-Z010-CUR
               WHEN    "SIPIRISU2"
                   MOVE    90          TO  SPA-Z010-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効開始年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "有効開始年月日は１日にしてください"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "有効終了年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "K013"
                   STRING  "警告！"
                           "有効終了年月日が月末日ではありません"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   MOVE    "出力名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "採血料区分エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "前回検査日エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "採血料区分は検査のみの入力です。"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "算定履歴区分エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "算定履歴区分は入力できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "算定履歴区分は入力できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "エラー処理は、０、９のみ入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
                   STRING  "ユーザー設定が有効になっていません。"
                           "有効にして下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0017"
                   STRING  "ユーザー設定の上限回数が設定されていません"
                           "。設定して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0018"
                   STRING  "上限回数がゼロではありません。"
                           "ユーザ設定の上限回数はゼロにできません。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0019"
                   STRING  "ユーザ設定の入力がありません。"
                           "登録できません。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K100"
                   STRING   "警告！算定履歴作成の設定がありません。"
                            "前回の判定ができません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0031"
                   MOVE    "総量編集がありません"
                                                   TO    SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "総量編集は内用薬剤のみ設定できます。"
                                                   TO    SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "一般名記載区分がありません"
                                                   TO    SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "一般名記載単位コードがありません"
                                                   TO    SPA-ERRMSG
               WHEN    "0032"
                   STRING  "一般名がありません。"
                           "一般名記載はできません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0033"
                   STRING  "処方名称がありません。"
                           "処方名称記載はできません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0034"
                   STRING  "一般名記載単位コードと換算値は一緒に"
                           "設定して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0034"
                   STRING  "一般名記載単位コードと換算値は一緒に"
                           "設定して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0035"
                   MOVE    "分類区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "全数量記載区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "数量換算単位がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0038"
                   STRING  "単位コードと数量換算単位が同じです。"
                           "数量換算単位を変更して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0039"
                   MOVE    "数量換算単位は 000 の選択はできません"
                                                 TO  SPA-ERRMSG
               WHEN    "0040"
                   MOVE    "数量換算単位を設定して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "数量換算値を入力して下さい"
                                                 TO  SPA-ERRMSG
      *R02.2
               WHEN    "0042"
                   MOVE    "ユーザー単位がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "K042"
                   STRING  "警告！点数マスタに単位がありますが、"
                           "ユーザー単位が必要か確認して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1010"
                   MOVE
                   "有効開始年月日≧有効終了年月日で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "1011"
                   STRING  "有効期間が重複しています。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "選択がありません。"
                           "削除できません。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9001"
                   MOVE    "削除処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "追加処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "更新処理が失敗しました"
                                                 TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
      *
           MOVE    "Z010"              TO  SPA-MOTOPG
           MOVE    "ZERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "ZERR"              TO  MCP-WINDOW
           MOVE    "B01"               TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
        510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0002"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z010"               TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角変換（チェック）処理
      *****************************************************************
       810-ZENCHK-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-OUT-INPUT 
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT 
           END-IF
           .
       810-ZENCHK-HEN-EXT.
           EXIT.
      *****************************************************************
      *   西暦→和暦（機能２１）
      *****************************************************************
       832-YMDCONV21-SEC                          SECTION.
      *
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE    WRK-SYMD
               WHEN    SPACE
               WHEN    ZERO 
               WHEN    "99999999"
                   MOVE    WRK-SYMD             TO  WRK-HENYMDG
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY2-AREA
                  MOVE  "21"                     TO  LNK-DAY2-IRAI
                  MOVE  WRK-SYMD                 TO  LNK-DAY2-YMD
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  MOVE    LNK-DAY2-EDTYMD1       TO  WRK-HENYMDG
           END-EVALUATE
           .
      *
       832-YMDCONV21-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           INITIALIZE                      STS-AREA-DAY
           EVALUATE    WRK-LNKYMD(1:7)
               WHEN    ZERO
                   MOVE    ZERO                TO  WRK-SYMD
               WHEN    "9999999"
                   MOVE    ALL "9"             TO  WRK-SYMD
               WHEN  OTHER
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE
           .    
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   文字数カウント処理（全角のみ）
      *****************************************************************
       891-STRCOUNT-SEC                         SECTION.
      *
           PERFORM  UNTIL  WRK-STRING(CNT-STRING)  NOT  =  SPACE
                    OR  CNT-STRING  =  1
               COMPUTE  CNT-STRING  =  CNT-STRING  -  1
           END-PERFORM
      *
           IF  CNT-STRING                        =  1
               MOVE  ZERO                        TO  CNT-STRING
           ELSE
               COMPUTE  CNT-STRING               =  CNT-STRING  /  2
           END-IF
           .
       891-STRCOUNT-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .    
      *
       990-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
      *
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
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
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                    USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
