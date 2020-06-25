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
       PROGRAM-ID.                 ORCSCZAICHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療会計同一剤チェック処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/05/09    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/22  MONFUNC 対応
      *  03.04.00    NACL-多々納  07/02/XX  包括診療対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.05.00    NACL-多々納  09/10/XX  多剤投与・換算値等対応
      *  04.07.00    NACL-多々納  12/04/12  後発品変更不可等対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年０４月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-CHK-OK          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *H24.4
           03  FLG-HENCHK          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-H-ZAINUM            PIC 9(08).
      *    診療種別
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-MAE-SRYSYUKBN       PIC X(03).
      *
      *    診療行為内容比較用テーブル
       01  TBL-AREA.
           03  TBL-MAE-AREA.
               05  TBL-MAE-REC     OCCURS   50.
                   07  TBL-MAE-SRYCD       PIC  X(09).
                   07  TBL-MAE-SRYSURYO    PIC  9(05)V9(05).
                   07  TBL-MAE-SRYKAISU    PIC  9(03).
                   07  TBL-MAE-INPUTCOMENT PIC  X(140).
                   07  TBL-MAE-ATAI-G.
                       09  TBL-MAE-ATAI    PIC  X(10)  OCCURS  5.
                   07  TBL-MAE-KANSURYO    PIC  9(05)V9(05).
                   07  TBL-MAE-INPUTKBN    PIC  X(01).
                   07  TBL-MAE-JIHIMONEY   PIC  9(07).
      *H24.4
      *            投薬予約コード
                   07  TBL-MAE-TOUYKK-G.
                       09  TBL-MAE-TOUYYK       PIC  X(01)
                                                OCCURS   5.
               05  TBL-MAE-CHKFLG          PIC  X(01)
                                           OCCURS   50.
           03  TBL-ATO-AREA.
               05  TBL-ATO-REC     OCCURS   50.
                   07  TBL-ATO-SRYCD       PIC  X(09).
                   07  TBL-ATO-SRYSURYO    PIC  9(05)V9(05).
                   07  TBL-ATO-SRYKAISU    PIC  9(03).
                   07  TBL-ATO-INPUTCOMENT PIC  X(140).
                   07  TBL-ATO-ATAI-G.
                       09  TBL-ATO-ATAI    PIC  X(10)  OCCURS  5.
                   07  TBL-ATO-KANSURYO    PIC  9(05)V9(05).
                   07  TBL-ATO-INPUTKBN    PIC  X(01).
                   07  TBL-ATO-JIHIMONEY   PIC  9(07).
      *H24.4
      *            投薬予約コード
                   07  TBL-ATO-TOUYKK-G.
                       09  TBL-ATO-TOUYYK       PIC  X(01)
                                                OCCURS   5.
               05  TBL-ATO-CHKFLG          PIC  X(01)
                                           OCCURS   50.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCZAICHK.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCZAICHKAREA
           .
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
           INITIALIZE                  ORCSCZAICHK-OUT-AREA
      *
           EVALUATE    ORCSCZAICHK-KBN
      *        診療会計チェック
               WHEN    "1"
                   PERFORM 100-CHK-SYORI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       100-CHK-SYORI-SEC               SECTION.
      *
      *    診療種別により判定を追加する
           IF     (ORCSCZAICHK-SRYSYUKBN   =  SPACE )  OR
                  (ORCSCZAICHK-SRYSYUKBN   =   "210"  OR
                                               "230" )
               MOVE    SPACE                   TO  WRK-SRYSYUKBN
      *        画像診断で部位からの場合
               IF     (ORCSCZAICHK-SRYSYUKBN   =   SPACE  )  AND
                      (ORCSCZAICHK-SRYKBN      =   "70"   )
                   MOVE    "700"               TO  WRK-SRYSYUKBN
               END-IF
           ELSE
               MOVE    ORCSCZAICHK-SRYSYUKBN   TO  WRK-SRYSYUKBN
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-OK
      *    診療会計マスターを特定する
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCSCZAICHK-SRYKA   TO  ACCT-SRYKA
           MOVE    ORCSCZAICHK-SRYYM   TO  ACCT-SRYYM
           MOVE    ORCSCZAICHK-SRYKBN  TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL   (FLG-SRYACCT        =   1    ) OR
                       (FLG-OK             =   1    )
      *        数量計をゼロで作成していたものがあったので、
      *        ゼロを１で判定する
               IF      ACCT-SURYOUTOTAL    =   ZERO
                   MOVE    1                   TO  ACCT-SURYOUTOTAL
               END-IF
      *        自保区分（包括剤）ゼロを空白とする（ゼロがあった）
               IF      ACCT-JIHOKBN        =   "0"
                   MOVE    SPACE               TO  ACCT-JIHOKBN
               END-IF
      *
      *        診療コード計を比較する
               MOVE     ZERO               TO  FLG-OK
               IF      (ACCT-HKNCOMBI      =   ORCSCZAICHK-HKNCOMBI  )
                 AND   (ACCT-SRYCDTOTAL    =   ORCSCZAICHK-SRYCDTOTAL)
                 AND   (ACCT-ZAITEN        =   ORCSCZAICHK-ZAITEN    )
                 AND   (ACCT-MEISAISU      =   ORCSCZAICHK-MEISAISU  )
                 AND   (ACCT-SURYOUTOTAL   =   ORCSCZAICHK-SURYOUTOTAL)
      *          自保区分（包括剤）
                 AND   (ACCT-JIHOKBN       =   ORCSCZAICHK-JIHOKBN   )
                   MOVE    ACCT-ZAINUM         TO  WRK-H-ZAINUM
      *            診療行為内容の比較処理
                   PERFORM 1002-SRYACCT-CHK-SEC
                   IF      FLG-OK              =   1
                       MOVE    WRK-H-ZAINUM        TO WRK-ZAINUM
                   END-IF
               END-IF
               IF      FLG-OK              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    同一剤あり
           IF      WRK-ZAINUM      NOT =   ZERO
               MOVE    WRK-ZAINUM          TO  ORCSCZAICHK-ZAINUM
           ELSE
               MOVE    ZERO                TO  ORCSCZAICHK-ZAINUM
           END-IF
	   .
      *
      *
       100-CHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為内容の比較処理
      *****************************************************************
       1002-SRYACCT-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX-SIN
           INITIALIZE                      TBL-MAE-AREA 
      *    診療会計マスターから診療行為マスターを検索する
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT       =   1  )
      *        診療種別により判定を追加する
               IF     (SRY-SRYSYUKBN       =  SPACE )  OR
                      (SRY-SRYSYUKBN       =  "210"  OR
                                              "230" )
                   MOVE    SPACE           TO  WRK-MAE-SRYSYUKBN
      *            画像診断で部位からの場合
                   IF     (SRY-SRYSYUKBN       =   SPACE  )  AND
                          (SRY-SRYKBN          =   "70"   )
                       MOVE    "700"               TO  WRK-MAE-SRYSYUKBN
                   END-IF
               ELSE
                   MOVE    SRY-SRYSYUKBN   TO  WRK-MAE-SRYSYUKBN
               END-IF
      *        診療種別区分判定
               IF       WRK-SRYSYUKBN       =   WRK-MAE-SRYSYUKBN
                   MOVE     1               TO  FLG-CHK-OK
               ELSE
                   MOVE     ZERO            TO  FLG-CHK-OK
               END-IF
               IF      FLG-CHK-OK          =   1
      *
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL  (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE ) OR
                                  (IDX-SIN     >=  50 )
      *H24.4
                       MOVE    ZERO                TO  FLG-HENCHK
                       IF      IDX-SIN         >   ZERO
                           IF     (TBL-MAE-SRYCD(IDX-SIN)(1:1) =  "6")
                             AND  (SRY-SRYCD (IDX2)(1:7)   =  "0992099")
                               PERFORM 10022-SRYACCT-HEN-SEC
                           END-IF
                       END-IF
                       IF      FLG-HENCHK          =   ZERO
      *H24.4
                       ADD     1                   TO  IDX-SIN
                       MOVE    SRY-SRYCD   (IDX2)  TO  TBL-MAE-SRYCD
                                                             (IDX-SIN)
                       MOVE    SRY-SRYSURYO(IDX2)  TO  TBL-MAE-SRYSURYO
                                                             (IDX-SIN)
                       MOVE    SRY-SRYKAISU(IDX2)  TO  TBL-MAE-SRYKAISU
                                                             (IDX-SIN)
      *                自費金額
                       MOVE    SRY-JIHIMONEY(IDX2) TO  TBL-MAE-JIHIMONEY
                                                             (IDX-SIN)
      *                換算値数量
                       MOVE    SRY-KANSURYO(IDX2)  TO  TBL-MAE-KANSURYO
                                                             (IDX-SIN)
      *                多剤投与・関連コメント
                       EVALUATE    SRY-MEISKYFLG (IDX2)
                           WHEN    "2"
                               MOVE    "1"         TO  TBL-MAE-INPUTKBN
                                                             (IDX-SIN)
                           WHEN    "3"
                               MOVE    "2"         TO  TBL-MAE-INPUTKBN
                                                             (IDX-SIN)
      *H25.3
                           WHEN    "R"
                               MOVE    "R"         TO  TBL-MAE-INPUTKBN
                                                             (IDX-SIN)
                       END-EVALUATE
      *
                       IF     (SRY-SRYCD (IDX2)(1:1)   =   "8" ) OR
                              (SRY-SRYCD (IDX2)(1:3)   =   "008")
                       OR     (SRY-SRYCD (IDX2)(1:3)   =   "001")
      *H30.4
                       OR     (SRY-SRYCD (IDX2)(1:7)   =   "0992081")
                           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                           MOVE    SRY-PTID            TO  PTCOM-PTID
                           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                           MOVE    SRY-SRYCD (IDX2)    TO  PTCOM-SRYCD
                           MOVE    SRY-INPUTNUM(IDX2)  TO  PTCOM-RENNUM
                           PERFORM 950-PTCOM-READ-SEC
                           IF      FLG-PTCOM           =   ZERO
                               MOVE    PTCOM-INPUTCOMENT   TO
                                               TBL-MAE-INPUTCOMENT
                                                            (IDX-SIN)
                               MOVE    PTCOM-INPUTCHI-AREA TO
                                               TBL-MAE-ATAI-G
                                                            (IDX-SIN)
                           END-IF
                       END-IF
      *H24.4
                       END-IF
      *H24.4
                   END-PERFORM
      *
               END-IF
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療行為内容のみテーブルに保存する
           MOVE    ZERO                TO  IDX-ATO
           INITIALIZE                      TBL-ATO-AREA 
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   50   )  OR
                              (ORCSCZAICHK-SRYCD (IDX1)  =   SPACE )
      *H24.4
               MOVE    ZERO                TO  FLG-HENCHK
               IF      IDX-ATO         >   ZERO
                   IF     (TBL-ATO-SRYCD(IDX-ATO)(1:1) =  "6")
                      AND (ORCSCZAICHK-SRYCD(IDX1)(1:7)  =  "0992099")
                       PERFORM 10023-ZAICHK-HEN-SEC
                   END-IF
               END-IF
               IF      FLG-HENCHK          =   ZERO
      *H24.4
               ADD     1                   TO  IDX-ATO
               MOVE    ORCSCZAICHK-SRYCD (IDX1)
                                           TO  TBL-ATO-SRYCD  (IDX-ATO)
               MOVE    ORCSCZAICHK-SRYSURYO (IDX1)
                                           TO  TBL-ATO-SRYSURYO(IDX-ATO)
               MOVE    ORCSCZAICHK-SRYKAISU (IDX1)
                                           TO  TBL-ATO-SRYKAISU
                                                              (IDX-ATO)
               MOVE    ORCSCZAICHK-INPUTCOMENT (IDX1)
                                           TO  TBL-ATO-INPUTCOMENT
                                                              (IDX-ATO)
               MOVE    ORCSCZAICHK-INPUTCHI-G (IDX1)
                                           TO  TBL-ATO-ATAI-G
                                                              (IDX-ATO)
               MOVE    ORCSCZAICHK-KANSURYO (IDX1)
                                           TO  TBL-ATO-KANSURYO(IDX-ATO)
               MOVE    ORCSCZAICHK-INPUTKBN (IDX1)
                                           TO  TBL-ATO-INPUTKBN(IDX-ATO)
               MOVE    ORCSCZAICHK-JIHIMONEY (IDX1)
                                           TO  TBL-ATO-JIHIMONEY
                                                              (IDX-ATO)
      *H24.4
               END-IF
      *H24.4
           END-PERFORM
      *
      *    診療行為数が違う
           IF      IDX-SIN         NOT =   IDX-ATO
               GO      TO      1002-SRYACCT-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   IDX-ATO  )  OR
                              (FLG-ERR     =   1        )
               MOVE    ZERO                TO  FLG-ARI
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   IDX-SIN  )  OR
                                  (FLG-ARI     =   1        )
                   IF     (TBL-ATO-REC (IDX1)  =   TBL-MAE-REC(IDX2) )
                     AND  (TBL-MAE-CHKFLG (IDX2)   =   SPACE         )
                        MOVE    1                  TO  FLG-ARI
                        MOVE    "1"                TO  TBL-MAE-CHKFLG
                                                               (IDX2)
                   END-IF
               END-PERFORM
               IF      FLG-ARI             =   ZERO
                    MOVE    1                  TO  FLG-ERR
               END-IF
           END-PERFORM
           IF      FLG-ERR             =   ZERO
               MOVE    1               TO  FLG-OK
           END-IF
           .
      *
       1002-SRYACCT-CHK-EXT.
           EXIT.
      *
      *H24.4
      *****************************************************************
      *    診療行為データ　予約コード編集
      *****************************************************************
       10022-SRYACCT-HEN-SEC         SECTION.
      *
           EVALUATE    SRY-SRYCD (IDX2)
      *        後発変更不可
               WHEN    "099209903"
                   MOVE    "1"         TO  TBL-MAE-TOUYYK (IDX-SIN 1)
                   MOVE    1           TO  FLG-HENCHK
      *        含量変更不可
               WHEN    "099209905"
                   MOVE    "1"         TO  TBL-MAE-TOUYYK (IDX-SIN 2)
                   MOVE    1           TO  FLG-HENCHK
      *        剤形変更不可
               WHEN    "099209906"
                   MOVE    "1"         TO  TBL-MAE-TOUYYK (IDX-SIN 3)
                   MOVE    1           TO  FLG-HENCHK
      *        銘柄名記載
               WHEN    "099209907"
                   MOVE    "1"         TO  TBL-MAE-TOUYYK (IDX-SIN 4)
                   MOVE    1           TO  FLG-HENCHK
      *        一般名記載
               WHEN    "099209908"
                   MOVE    "1"         TO  TBL-MAE-TOUYYK (IDX-SIN 5)
                   MOVE    1           TO  FLG-HENCHK
           END-EVALUATE
           .
      *
       10022-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約コード編集処理
      *****************************************************************
       10023-ZAICHK-HEN-SEC         SECTION.
      *
           EVALUATE    ORCSCZAICHK-SRYCD (IDX1)
      *        後発変更不可
               WHEN    "099209903"
                   MOVE    "1"         TO  TBL-ATO-TOUYYK (IDX-ATO 1)
                   MOVE    1           TO  FLG-HENCHK
      *        含量変更不可
               WHEN    "099209905"
                   MOVE    "1"         TO  TBL-ATO-TOUYYK (IDX-ATO 2)
                   MOVE    1           TO  FLG-HENCHK
      *        剤形変更不可
               WHEN    "099209906"
                   MOVE    "1"         TO  TBL-ATO-TOUYYK (IDX-ATO 3)
                   MOVE    1           TO  FLG-HENCHK
      *        銘柄名記載
               WHEN    "099209907"
                   MOVE    "1"         TO  TBL-ATO-TOUYYK (IDX-ATO 4)
                   MOVE    1           TO  FLG-HENCHK
      *        一般名記載
               WHEN    "099209908"
                   MOVE    "1"         TO  TBL-ATO-TOUYYK (IDX-ATO 5)
                   MOVE    1           TO  FLG-HENCHK
           END-EVALUATE
           .
      *
       10023-ZAICHK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスター読込
      *****************************************************************
       950-PTCOM-READ-SEC         SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTCOM-REC
                   MOVE    ZERO               TO  FLG-PTCOM
               ELSE
                   MOVE    1                  TO  FLG-PTCOM
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTCOM
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       950-PTCOM-READ-EXT.
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
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
