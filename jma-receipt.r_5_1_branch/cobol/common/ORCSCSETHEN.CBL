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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCSETHEN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : セットマスタ展開サブ
      *                     （Ｋ０２，Ｋ０２Ｎ，Ｋ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/01/16    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−多々納 05/02/01  残量廃棄判定追加
      *  01.00.02    NACL-多々納  05/12/22  MONFUNC 対応
      *  01.00.03    NACL-多々納  06/03/02  セット期間対応
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *  04.05.00    NACL-多々納  09/09/XX  '0086'コメントコード対応
      *  04.05.00    NACL-多々納  09/10/XX  換算値入力・関係コメント指示等対応
      *  04.08.00    NACL-多々納  16/06/XX  全体展開処理追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-YAKUSOKU        PIC 9(01).
           03  FLG-SETSYORI        PIC 9(01).
      *
           03  FLG-AUTOKBN2        PIC 9(01).
           03  FLG-ZEROOK          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
           03  IDX-IP                  PIC 9(04).
           03  IDX-YAK                 PIC 9(04).
           03  IDX-YAKUSOKU            PIC 9(04).
           03  IDX-H                   PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
           03  WRK-INPUTCD         PIC X(10).
      *
           03  WRK-GMN-IDX         PIC 9(04).
           03  WRK-GMN-STR         PIC 9(04).
      *    内部
           03  SPA-GMN-IDX         PIC 9(04).
      *
           03  WRK-PATH            PIC X(64).
      *    エラーコード
           03  WRK-ERRCD           PIC X(04).
           03  WRK-ERRCD1          PIC X(04).
           03  WRK-ERRCD2          PIC X(04).
           03  WRK-ERRMSG1         PIC X(80).
      *    約束セット位置
           03  WRK-IDX-YAKUSOKU.
               05  WRK-IDX-YAK         PIC 9(04)    OCCURS  20.
      *画面最大行
      *01  WRK-GYO-MAX                 PIC 9(04)   VALUE   400.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1038.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    点数編集チェックパラメタ
           COPY    "CPORCSC92.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
      *
           COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *
           COPY    "CPORCSCSETHEN.INC".
      *    画面ＳＰＡ領域
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
      *****COPY    "K02SCR-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSCSETHENAREA
           SPA-AREA
      *****SPA-K02
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           IF      ORCSCSETHEN-PG      =   "K02"
                                       OR  "KT01"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                  SPA-K01KYOUTU
      *H28.6
      *        セット展開区分設定
               PERFORM 1001-SETALLKBN-SEC
           END-IF
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           MOVE    ORCSCSETHEN-SRYCD   TO  WRK-SRYCD
           MOVE    ORCSCSETHEN-IDX     TO  SPA-GMN-IDX
      *
           EVALUATE    ORCSCSETHEN-SRYCD(1:1)
      *        セット展開処理
               WHEN    "P"
                   PERFORM 100-PSET-TENKAI-SEC
      *        約束処方セット展開処理
               WHEN    "S"
                   PERFORM 200-SSET-TENKAI-SEC
           END-EVALUATE
      * 
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    セット展開処理
      *****************************************************************
       100-PSET-TENKAI-SEC          SECTION.
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-STR
      *
           INITIALIZE                  WRK-IDX-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
           MOVE    ZERO                TO  IDX-IP
           MOVE    WRK-SRYCD(1:6)      TO  WRK-SETCD
      *
      *    入力セットマスタ検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-SETCD           TO  ISET-SETCD
           MOVE    SPA-SRYYMD          TO  ISET-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           IF      FLG-INPUTSET        =   1
               MOVE    "0150"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRCD1
           MOVE    SPACE               TO  WRK-ERRCD2
           MOVE    SPACE               TO  WRK-ERRMSG1
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE ) OR
                             (SPA-GMN-IDX      >   SPA-MAX-LINE )
      *        セット展開
               MOVE    1               TO  FLG-SETSYORI
               MOVE    ZERO            TO  FLG-ERR
      *
               ADD     1               TO  IDX-IP
      *        編集
               MOVE    ZERO            TO  FLG-SYORI
      *!!
      *        器材商品コード対応
      *        1行前が器材コードの自動発生の時、置換え
               MOVE    ZERO            TO  FLG-AUTOKBN2
               IF     (SPA-GMN-IDX         >   1    )    AND
                      (ISET-INPUTCD (1:1)  =   "7"  )    AND
                      (SPA-COM-AUTOKBN2(SPA-GMN-IDX - 1)
                                           =   "1"   )   AND
                      (ISET-INPUTCD        =   SPA-COM-INPUTCD
                                                  (SPA-GMN-IDX - 1 ))
                   MOVE    1               TO  FLG-AUTOKBN2
                   COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX   -  1
               END-IF
      *!!
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *        保険組合せは前の行を複写する
               IF     (SPA-GMN-IDX     >   1   )  AND
                      (SPA-COM-HKNCOMBI (SPA-GMN-IDX - 1 ) =   "9999")
                  MOVE    "9999"           TO  SPA-COM-HKNCOMBI
                                                      (SPA-GMN-IDX)
               END-IF
      *
               EVALUATE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                   WHEN    "S"
      *                セット内に約束セットがある時
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                       PERFORM 1001-INSET-HEN-SEC
                       PERFORM 1003-YAKUSOKU-SYORI-SEC
                   WHEN    "."
      *                回数入力で剤終了とする
                       IF       SPA-GMN-IDX        >   1
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                      (SPA-GMN-IDX - 1)
                       END-IF
      *                診療種別区分名称チェック
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
                       PERFORM 510-SRYKBN-MEI-SEC
                   WHEN    OTHER
      *                診療コード編集
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                       PERFORM 1001-INSET-HEN-SEC
                       PERFORM 1001-SRYCD-SYORI-SEC
               END-EVALUATE
      *
      *!!
               IF      FLG-AUTOKBN2        =   1
                   MOVE    "1"             TO  SPA-COM-AUTOKBN2 
                                                      (SPA-GMN-IDX)
               END-IF
      *!!
      *
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
      *            回数入力で剤終了とする
                   MOVE    "1"             TO  SPA-COM-ZAIEND
                                                      (SPA-GMN-IDX)
               END-IF
      *        エラーがない時、チェック済みとする
               IF     (FLG-ERR             =   ZERO )  AND
                      (SPA-ERRCD           =   SPACE)
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                           TO  SPA-MAE-INPUTCD
                                                          (SPA-GMN-IDX)
               END-IF
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-INPUTSET-READ-SEC
      *
               IF      (FLG-INPUTSET     =   ZERO  )  AND
                       (SPA-ERRCD        =   SPACE )
      *
      *            行を挿入して追加する
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    セットなし
           IF       SPA-ERRCD           =   "0150"
               GO      TO    100-PSET-TENKAI-EXT
           END-IF
      *   エラーの為展開できない
          IF      (SPA-ERRCD       NOT =   SPACE )  AND
                  (FLG-INPUTSET        =   ZERO  )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    100-PSET-TENKAI-EXT
           END-IF
           IF      SPA-GMN-IDX         >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  SPA-GMN-IDX
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    対象なし
           IF      IDX-IP              =   ZERO
               MOVE    "0050"              TO  SPA-ERRCD
               GO      TO    100-PSET-TENKAI-EXT
           END-IF
      *    最終行を回数入力とする
           IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
               MOVE    "1"             TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *        セットの時前に編集
               IF      WRK-ERRCD       =   SPACE
                   MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
               ELSE
                   MOVE    WRK-ERRCD   TO  SPA-ERRCD
                   MOVE    "1"         TO  SPA-COM-CUR (WRK-GMN-IDX)
               END-IF
           END-IF
      *
      *    約束セットがあった場合、編集する
           MOVE    ZERO                TO  IDX-IP
           PERFORM VARYING     IDX-YAK FROM    1   BY  1
                   UNTIL      (IDX-YAK     >   IDX-YAKUSOKU) OR
                              (SPA-ERRCD   NOT =   SPACE )
               COMPUTE SPA-GMN-IDX         =   WRK-IDX-YAK (IDX-YAK)
                                           +   IDX-IP
               MOVE    ZERO                TO  IDX-IP
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                               TO  WRK-SRYCD
      *            約束処方セット展開処理
                   PERFORM 200-SSET-TENKAI-SEC
               END-IF
           END-PERFORM
      *
      *    残量廃棄の有無判定
           PERFORM VARYING     IDX-H   FROM    WRK-GMN-STR BY  1
                   UNTIL       IDX-H   >   SPA-GMN-IDX
               IF    ((SPA-COM-SRYKBN (IDX-H)(1:1) =   "3" )  AND
                      (SPA-COM-INPUTCD(IDX-H)(1:1) =   "6" ))
      *H27.11   全診区　追加
               OR    ((SPA-HAIKIKBN-FLG            =   2  )  AND
                      (SPA-COM-SRYKBN (IDX-H)(1:1) =   "4"
                                                   OR  "5"
                                                   OR  "6"
                                                   OR  "7"
                                                   OR  "8" )  AND
                      (SPA-COM-INPUTCD(IDX-H)(1:1) =   "6" ))
                   MOVE    SPA-COM-SURYO(IDX-H)    TO  WRK-SURYO
      *        注射薬でアンプル、少数以下がある時、次に残量廃棄がある
      *H27.11  外用薬追加
                   IF      (SPA-COM-YKZKBN (IDX-H)  =   "4"
                                                    OR  "6"  )  AND
                           (SPA-COM-TANICD (IDX-H)  =   "014"
                                                    OR  "022"
                                                    OR  "046" )  AND
                           (WRK-SURYO-S2           >   ZERO)
                       IF     (IDX-H               <   SPA-MAX-LINE) AND
                              (SPA-COM-INPUTCD(IDX-H + 1)
                                                   =   "099309901")
                           CONTINUE
                       ELSE
                           MOVE    "1"         TO  SPA-COM-AUTOFLG2
                                                             (IDX-H)
      *                    入力コードの画面用再編集
                           PERFORM 3101-INPUTCD-HEN-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    数量ゼロエラー
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD2      NOT =   SPACE
                   MOVE    WRK-ERRCD2          TO  SPA-ERRCD
               END-IF
           END-IF
      *    マスタなしエラー
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD1      NOT =   SPACE
                   MOVE    WRK-ERRCD1          TO  SPA-ERRCD
                   MOVE    WRK-ERRMSG1         TO  ORCSCSETHEN-ERRMSG2
               END-IF
           END-IF
           .
       100-PSET-TENKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード編集処理
      *****************************************************************
       1001-INSET-HEN-SEC          SECTION.
      *
      *    残量廃棄の為、診療区分設定
           MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (SPA-GMN-IDX)
      *
           IF      ISET-SURYO1     =   ZERO
      *        数量ゼロフラグ
               MOVE    ZERO                TO  FLG-ZEROOK
               IF     (ISET-INPUTCD(1:1)   =   "6"
                                           OR  "7"
                                           OR  "1"  )
                  OR  (ISET-INPUTCD(1:3)   =   "059" )
                   IF     (SPA-SURYOZERO-FLG   =   ZERO ) OR
                          (FLG-SYORI           =   1    )  OR
                          (ISET-INPUTCD        =   "799990070"  OR
                                                   "770020070" )
                       MOVE    ZERO                TO  FLG-ZEROOK
                   ELSE
                       MOVE    1                   TO  FLG-ZEROOK
                   END-IF
               END-IF
               IF      FLG-ZEROOK          =   ZERO
                   MOVE    1               TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
               ELSE
                   MOVE    ZERO            TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                   MOVE    "A001"          TO  WRK-ERRCD2
                   MOVE    1               TO  FLG-ERR
               END-IF
           ELSE
               MOVE    ISET-SURYO1     TO  SPA-COM-SURYO (SPA-GMN-IDX)
      *        １以上の時、数量入力とする
               IF     (ISET-INPUTCD(1:1)   =   "1"
                                           OR  "6"
                                           OR  "7" )   AND
                      (ISET-SURYO1         >   1   )
                   MOVE    "1"         TO  SPA-COM-SURFLG(SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
           IF      ISET-SURYO2     =   ZERO
               MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           ELSE
               MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           END-IF
           IF      ISET-KAISU      =   ZERO
               MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX)
           ELSE
               MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX)
               MOVE    "1"         TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
           END-IF
           MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                   TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
           IF      ISET-INPUTCD        =   "810000001"
                                       OR  "008500000"
                                       OR  "008600000"
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO-G
                                                      (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                      (SPA-GMN-IDX)
           ELSE
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
               IF     (ISET-INPUTCD(1:2)   =   "83" )  OR
                      (ISET-INPUTCD(1:4)   =   "0083")
                  OR  (ISET-INPUTCD(1:4)   =   "0085")
                  OR  (ISET-INPUTCD(1:4)   =   "0086")
                   MOVE    "1"         TO  SPA-COM-MEIFLG
                                                          (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (05)  TO  SPA-COM-ATAI5 (SPA-GMN-IDX)
      *    換算値
           MOVE    ISET-KANSURYO       TO  SPA-COM-KANSURYO
                                                           (SPA-GMN-IDX)
      *    関係コメント指示
           IF      ISET-INPUTKBN       =   "1"
                MOVE    "1"            TO  SPA-COM-INPUTCONT
                                                           (SPA-GMN-IDX)
           END-IF
      *    内服１種類区分
           IF      SPA-NYUGAIKBN       =   "2"
               IF      ISET-INPUTKBN       =   "2"
                   MOVE    "1"             TO  SPA-COM-INPUTNAIF
                                                           (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-TANDKKBN
                                                           (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       1001-INSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード展開処理
      *****************************************************************
       1001-SRYCD-SYORI-SEC          SECTION.
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    ORCSCSETHEN-PG      TO  LNK-SC92-PG
           MOVE    ORCSCSETHEN-SYORIFLG    TO  LNK-SC92-SYORIFLG
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    "8"                 TO  LNK-SC92-KBN
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    ORCSCSETHEN-NENREI      TO  LNK-SC92-NENREI
           MOVE    ORCSCSETHEN-15MIMAN-ARI TO  LNK-SC92-15MIMAN-ARI
           MOVE    ORCSCSETHEN-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    ORCSCSETHEN-LSTSRYKA    TO  LNK-SC92-LSTSRYKA
           MOVE    ORCSCSETHEN-LSTSRYKAMEI TO  LNK-SC92-LSTSRYKAMEI
           MOVE    ORCSCSETHEN-TEISEI-AREA
                                           TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           MOVE    LNK-SC92-GMN-IDX    TO  SPA-GMN-IDX
      *
      *ver.4.5
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                       =   "6"   )  AND
                  (ISET-KANSURYO       >   ZERO  )
               IF      ISET-SURYO1     =  (ISET-KANSURYO
                                       *   SPA-COM-KANZANCHI
                                                  (SPA-GMN-IDX))
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  SPA-COM-KANSURYO
                                                      (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1     TO  SPA-COM-SURYO
                                                      (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1     TO  SPA-COM-SURYO2
                                                      (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    点数マスタなしのエラーは表示する
           IF      SPA-ERRCD           =   "0001"
                IF      SPA-COM-SET (SPA-GMN-IDX)   =  "1"
                    MOVE    "S001"          TO  WRK-ERRCD
                END-IF
                IF      WRK-ERRCD1         =   SPACE
                    MOVE    LNK-SC92-ERRMSG    TO  WRK-ERRMSG1
                END-IF
                MOVE    1               TO  FLG-ERR
                MOVE    SPA-ERRCD       TO  WRK-ERRCD1
                MOVE    SPACE           TO  SPA-ERRCD
                MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *    警告の時、表示する
           IF     (SPA-ERRCD   (1:1)   =   "K"  )
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-COM-KZMFLG (SPA-GMN-IDX)
           END-IF
      *    警告の時、表示する
           IF     (LNK-SC92-ERRCD3  (1:1)   =   "K"  )  OR
                  (LNK-SC92-ERRCD6  (1:1)   =   "K"  )
               MOVE    SPACE           TO  LNK-SC92-ERRAREA
               MOVE    SPACE           TO  SPA-COM-KZMFLG (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-HKEIFLG (SPA-GMN-IDX)
           END-IF
      *    画像診断のＣＴ．ＭＲＩは２回目のコードが違うので
      *    算定回数のチェックなし
           IF     (SPA-COM-TNSSRYSYUKBN (SPA-GMN-IDX)
                                               =   "723"
                                               OR  "724"  )  AND
                  (SPA-COM-HOUKATUTEIGENKBN(SPA-GMN-IDX)
                                               =   "201"
                                               OR  "202"
                                               OR  "203"  )  AND
                   (SPA-COM-KAISU (SPA-GMN-IDX)
                                               =   1      )
               MOVE    SPACE           TO  LNK-SC92-ERRCD(3)
           END-IF
      *
      *H28.6
      *    エラーでも展開する(約束セットは展開なし）
           IF      SPA-SETALLKBN       =   "1"
               IF     (SPA-ERRCD           NOT =   SPACE )  OR
                      (LNK-SC92-ERRAREA    NOT =   SPACE )
                   IF      SPA-COM-SET (SPA-GMN-IDX)   =  "1"
                       MOVE    "S001"          TO  WRK-ERRCD
                   END-IF
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  LNK-SC92-ERRAREA
                   MOVE    SPACE           TO  SPA-COM-KZMFLG
                                                        (SPA-GMN-IDX)
                   MOVE    SPACE           TO  SPA-COM-HKEIFLG
                                                        (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-CUR
                                                        (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               IF      WRK-GMN-IDX         =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                           -   1
               END-IF
           ELSE
      *        保険外金額判定
               PERFORM 4101191-JIHI-HEN-SEC
      *        入力コードの画面用再編集
               MOVE    SPA-GMN-IDX         TO  IDX-H
               PERFORM 3101-INPUTCD-HEN-SEC
           END-IF
      *
           .
       1001-SRYCD-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険外金額判定処理
      *****************************************************************
       4101191-JIHI-HEN-SEC           SECTION.
      *
      *    残量廃棄の為、診療区分設定
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1) =   "1" ) AND
                  (SPA-COM-DATAKBN(SPA-GMN-IDX) =   "1" ) AND
                  (WRK-SRYKBN (1:1)    NOT =   "9"         )
               MOVE    SPA-COM-TNSSRYKBN (SPA-GMN-IDX) TO  WRK-SRYKBN
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN
                                                          (SPA-GMN-IDX)
           END-IF
      *
      *    自賠責の時、対象外
           IF     (SPA-HKNKBN          =   1    )  AND
                  (SPA-RSI-HKNKBN      =   "4"  )  AND
                  (SPA-COM-SRYKBN (SPA-GMN-IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                       =   "09591"  OR
                                           "09592"  OR
                                           "09593"  )
      *        自賠責入力
               IF    (SPA-COM-SRYSYUKBN (SPA-GMN-IDX)  =   "809")
                   MOVE    "1"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
                   MOVE    1                   TO  SPA-COM-SURYO
                                                        (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-JIHIMONEY
                                                         (SPA-GMN-IDX)
               END-IF
               GO     TO      4101191-JIHI-HEN-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)  =   "095"  OR
                                                          "096"
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-KEI
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-JIHIMONEY
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
                                                         (SPA-GMN-IDX)
                   MOVE    1                   TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
               END-IF
           ELSE
               IF      WRK-SRYKBN          =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       4101191-JIHI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       3101-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    ORCSCSETHEN-PG      TO  LNK-SC94-PG
           MOVE    IDX-H               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    セット内の約束セット展開処理
      *****************************************************************
       1003-YAKUSOKU-SYORI-SEC          SECTION.
      *
      *    入力コードの画面用再編集
           MOVE    SPA-GMN-IDX         TO  IDX-H
           PERFORM 3101-INPUTCD-HEN-SEC
      *    約束名称編集
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                       TO  WRK-INPUTCD
           PERFORM 41011921-INPSET-MEI-SEC
           MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                     (SPA-GMN-IDX)
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      1003-YAKUSOKU-SYORI-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-YAKUSOKU
           ADD     1                   TO  IDX-YAKUSOKU
           IF      IDX-YAKUSOKU        >   20
                MOVE    "0007"         TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-IDX     TO  WRK-IDX-YAK (IDX-YAKUSOKU)
           END-IF
           .
       1003-YAKUSOKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束セット展開処理
      *****************************************************************
       200-SSET-TENKAI-SEC     SECTION.
      *
      *    約束の時、１行前に診療種別があること
           IF     (SPA-GMN-IDX         =   1   )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1) NOT =   "." )
               MOVE    "0048"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      200-SSET-TENKAI-EXT
           END-IF
      *
      *    約束名称編集
           MOVE    WRK-SRYCD(1:6)      TO  WRK-INPUTCD
           PERFORM 41011921-INPSET-MEI-SEC
           MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                      (SPA-GMN-IDX)
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      200-SSET-TENKAI-EXT
           END-IF
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *
           MOVE    ZERO                TO  IDX-IP
           MOVE    WRK-SRYCD(1:6)      TO  WRK-SETCD
      *    入力セットマスタ検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-SETCD           TO  ISET-SETCD
           MOVE    SPA-SRYYMD          TO  ISET-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           IF      FLG-INPUTSET        =   1
               MOVE    "0150"              TO  SPA-ERRCD
           END-IF
      *
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE )
      *
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
      *
                   ADD     1               TO  IDX-IP
      *
      *            編集
                   MOVE    1               TO  FLG-SYORI
                   MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                           TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                   PERFORM 1001-INSET-HEN-SEC
      *
      *            画面表示なし
                   MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX)
      *
      *            約束処方の、数量を計算する
                   COMPUTE SPA-COM-SURYO (SPA-GMN-IDX)
                                           =   SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                                           *   SPA-COM-SURYO
                                                         (WRK-GMN-IDX)
                   MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                               TO  SPA-COM-SURYO2
                                                         (SPA-GMN-IDX)
      *            回数変更
                   MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                         (SPA-GMN-IDX)
                   MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU2
                                                         (SPA-GMN-IDX)
      *
      *            点数マスタ編集・基本チェック
                   PERFORM 1001-SRYCD-SYORI-SEC
      *
      *            セットの時前に編集
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                               TO  SPA-MAE-INPUTCD
                                                          (SPA-GMN-IDX)
                   END-IF
      *
                   MOVE    "tbl_inputset"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-INPUTSET-READ-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    セットなし
           IF       SPA-ERRCD           =   "0150"
               GO      TO    200-SSET-TENKAI-EXT
           END-IF
      *    行オーバー
           IF     ((SPA-ERRCD       NOT =   SPACE )  AND
                   (FLG-INPUTSET        =   ZERO  ))  OR
                  ( SPA-ERRCD           =   "0150" )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    200-SSET-TENKAI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    最終行を回数入力とする
           MOVE    "1"                 TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *    セットの時前に編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
           ELSE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (WRK-GMN-IDX)
           END-IF
      *
      *    数量ゼロエラー
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD2      NOT =   SPACE
                   MOVE    WRK-ERRCD2          TO  SPA-ERRCD
               END-IF
           END-IF
      *    マスタなしエラー
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD1      NOT =   SPACE
                   MOVE    WRK-ERRCD1          TO  SPA-ERRCD
                   MOVE    WRK-ERRMSG1         TO  ORCSCSETHEN-ERRMSG2
               END-IF
           END-IF
      *
           .
       200-SSET-TENKAI-EXT.
           EXIT.
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       41011921-INPSET-MEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
      *
           STRING    WRK-INPUTCD       DELIMITED   BY  SPACE
                     "%"               DELIMITED   BY  SIZE
                                       INTO    ICD-INPUTCD
           END-STRING
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "0050"          TO  SPA-ERRCD
               GO  TO                  41011921-INPSET-MEI-EXT
           END-IF
           MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *    回数入力としない
           MOVE    SPACE               TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
           .
       41011921-INPSET-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           IF      SPA-GMN-IDX         >=  SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      IDX             =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-GMN-TBL (IDY) 
                                           TO  SPA-GMN-TBL(IDX)
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           .
       41014-GYOINSN-EXT.
           EXIT.
      *****************************************************************
      *    セット展開区分検索処理
      *****************************************************************
       1001-SETALLKBN-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  SYSKANRI-REC
      *    システム管理キー検索
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1038-REC
      *        セット展開区分
               MOVE    SYS-1038-SETALLKBN  TO  SPA-SETALLKBN
           END-IF
           IF      SPA-SETALLKBN       =   SPACE
               MOVE    "0"             TO  SPA-SETALLKBN
           END-IF
      *
           .
       1001-SETALLKBN-EXT.
           EXIT.
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       910-INPUTSET-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       910-INPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 9301-INPUTCD-NEXT-SEC
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       9301-INPUTCD-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       9301-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               MOVE    SPACE               TO  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
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
