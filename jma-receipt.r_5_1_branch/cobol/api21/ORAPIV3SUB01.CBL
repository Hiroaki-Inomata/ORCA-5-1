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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPIV3SUB01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 共通キャンセルサブ（一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/07/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
      *    03  CNT-SRYCNT              PIC 9(04).
      *    一時領域
      *
       01  WRK-AREA.
      *
           03  WRK-PTNUM               PIC X(20).
           03  WRK-TERMID              PIC X(64).
           03  WRK-RENNUM              PIC 9(01).
           03  WRK-PARA-GYOUMUID       PIC X(04).
      *
           03  WRK-MCP-TERM            PIC X(62).
      *
      *    全業務解除
       01  CONS-ORCA-UID       PIC X(64)   VALUE
                   "267c5110-71b6-431e-baac-0989ab98a505".
      *
       01  CONS-ORCA-UID2       PIC X(64)   VALUE
                   "API+267c5110-71b6-431e-baac-0989ab98a505".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "MCPAREA".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ領域
           COPY    "CPORAPIV3SUB01.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           ORAPIV3SUB01AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPACE               TO  APIV3SUB01-ERRCD
      *
           PERFORM 100-INIT-SEC
      *
           IF      APIV3SUB01-TERMID   =    CONS-ORCA-UID
                                       OR   CONS-ORCA-UID2
      *        全業務　強制処理終了
               PERFORM 400-ALL-CANCEL-SEC
           ELSE
               IF      APIV3SUB01-ERRCD     =   SPACE
      *            強制処理終了
                   PERFORM 200-CANCEL-SEC
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    必須項目チェック
           IF     (APIV3SUB01-TERMID    =   SPACE )
              AND (APIV3SUB01-KARTE-UID =   SPACE )
               MOVE    "01"            TO  APIV3SUB01-ERRCD
           END-IF
      *
           IF      APIV3SUB01-API-GYOUMUID  =   SPACE
               MOVE    "02"            TO  APIV3SUB01-ERRCD
           END-IF
      *
           IF      APIV3SUB01-SYORI NOT =   "9"
               IF     (APIV3SUB01-TERMID    =   SPACE )
                  OR  (APIV3SUB01-PARA-GYOUMUID =   SPACE )
                   MOVE    "01"            TO  APIV3SUB01-ERRCD
               END-IF
           END-IF
           .
       100-INIT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    強制処理終了
      *****************************************************************
       200-CANCEL-SEC   SECTION.
      *
           MOVE    APIV3SUB01-PTNUM     TO  WRK-PTNUM
           IF      APIV3SUB01-TERMID    =   SPACE
      *        ORUCA-UID 取得
               PERFORM 2001-TERMID-GET-SEC
           ELSE
               MOVE    APIV3SUB01-TERMID    TO  WRK-TERMID
           END-IF
      *
           IF     (WRK-TERMID          =   SPACE )
                MOVE    "03"                TO  APIV3SUB01-ERRCD
           ELSE
      *        パラメタ削除処理
               PERFORM 2002-FILE-DELETE-SEC
      *        排他制御解除
               IF      APIV3SUB01-PARA-GYOUMUID  NOT =   SPACE
                   PERFORM 990-LOCK-OUT-SEC
                   IF      SLOCK-RETURN    NOT =   ZERO
                       MOVE    "04"            TO  APIV3SUB01-ERRCD
                   END-IF
               END-IF
      *
               MOVE    WRK-TERMID          TO  APIV3SUB01-TERMID
           END-IF
           .
       200-CANCEL-EXIT.
           EXIT.
      *
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       2001-TERMID-GET-SEC          SECTION.
      *
      *    パラメタ検索
           MOVE    SPACE               TO  WRK-TERMID
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM             TO  APIPARA-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID  TO  APIPARA-GYOUMUID
           MOVE    APIV3SUB01-KARTE-UID     TO  APIPARA-KARTE-UID
           MOVE    APIV3SUB01-API-FILEMEI   TO  APIPARA-FILEMEI
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-APIPARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM UNTIL       FLG-APIPARA     =   1
      *        患者番号が一致
               IF     (WRK-PTNUM           =   SPACE )
               OR    ((WRK-PTNUM       NOT =   SPACE        ) AND
                      (WRK-PTNUM           =   APIPARA-PTNUM))
                   MOVE    APIPARA-TERMID      TO  WRK-TERMID
                   MOVE    1                   TO  FLG-PARA
               END-IF
               IF      FLG-APIPARA         =   ZERO
                   MOVE    "tbl_api_para"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-APIPARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_api_para"          TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-TERMID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       2002-FILE-DELETE-SEC          SECTION.
      *
      *    業務一時保存ファイル削除
           IF      APIV3SUB01-PARA-GYOUMUID  NOT =   SPACE
               MOVE    APIV3SUB01-PARA-GYOUMUID  TO  WRK-PARA-GYOUMUID
               PERFORM 20021-PARA-DELETE-SEC
           END-IF
      *
           IF      APIV3SUB01-PARA-GYOUMUID2  NOT =   SPACE
               MOVE    APIV3SUB01-PARA-GYOUMUID2  TO  WRK-PARA-GYOUMUID
               PERFORM 20021-PARA-DELETE-SEC
           END-IF
      *
      *    API一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID
                                       TO  PARA-GYOUMUID
           MOVE    WRK-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_para"          TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID
                                       TO  SPATMP-GYOUMUID
           MOVE    WRK-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    APIV3SUB01-API-GYOUMUID
                                       TO  APIPARA-GYOUMUID
           MOVE    WRK-TERMID          TO  APIPARA-TERMID
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       2002-FILE-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル削除処理
      *****************************************************************
       20021-PARA-DELETE-SEC         SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    WRK-PARA-GYOUMUID   TO  PARA-GYOUMUID
           MOVE    WRK-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_para"          TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    WRK-PARA-GYOUMUID   TO  SPATMP-GYOUMUID
           MOVE    WRK-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           .
       20021-PARA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    全業務　強制処理終了
      *****************************************************************
       400-ALL-CANCEL-SEC         SECTION.
      *
           MOVE    SPACE               TO  APIV3SUB01-PARA-GYOUMUID
           MOVE    SPACE               TO  APIV3SUB01-ERRCD
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-APIPARA-READ-SEC
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           PERFORM
               UNTIL  (FLG-APIPARA         =   1     )
      *
               MOVE    APIPARA-TERMID      TO  WRK-TERMID
      *        API一時保存ファイル削除
               INITIALIZE                  PARA-REC
               MOVE    APIPARA-HOSPNUM     TO  PARA-HOSPNUM
               MOVE    APIPARA-TERMID      TO  PARA-TERMID
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "del4"              TO  MCP-PATHNAME
               MOVE    "tbl_para"          TO  MCP-TABLE
               PERFORM 900-ORCDBMAIN-SEC
      *        SPATMP
               INITIALIZE                      SPATMP-REC
               MOVE    APIPARA-HOSPNUM     TO  SPATMP-HOSPNUM
               MOVE    APIPARA-TERMID      TO  SPATMP-TERMID
      *
               MOVE    SPATMP-REC          TO  MCPDATA2-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "del4"              TO  MCP-PATHNAME
               PERFORM 910-SPATMP-CALL-SEC
      *
      *        排他制御解除
               PERFORM 990-LOCK-OUT-SEC
      *        APIPARA 削除
               MOVE    APIPARA-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               PERFORM 900-ORCDBMAIN-SEC
      *
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 990-APIPARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       400-ALL-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理(削除)
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
           MOVE    MCP-TERM            TO  WRK-MCP-TERM
      *    排他制御解除処理
           MOVE    APIV3SUB01-PARA-GYOUMUID
                                       TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
      *    API+オルカＵＩＤ をTERMID
           MOVE    WRK-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       300-ERRMSG-HEN-SEC          SECTION.
      *
           EVALUATE    APIV3SUB01-ERRCD
               WHEN    "01"
                   MOVE    "ＵＩＤの設定がありません"
                                       TO  APIV3SUB01-ERRMSG
               WHEN    "02"
                   MOVE    "業務の設定がありません"
                                       TO  APIV3SUB01-ERRMSG
               WHEN    "03"
                   MOVE    "オルカＵＩＤが取得できません"
                                       TO  APIV3SUB01-ERRMSG
               WHEN    "04"
                   MOVE    "排他解除に失敗しました"
                                       TO  APIV3SUB01-ERRMSG
           END-EVALUATE
           .
       300-ERRMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-APIPARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  APIPARA-REC
               MOVE    ZERO            TO  FLG-APIPARA
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
      *
           .
       990-APIPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *****************************************************************
      *    テーブルCALL処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
