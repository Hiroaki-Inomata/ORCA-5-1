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
       PROGRAM-ID.             ORCGP015.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者番号再設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/10/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  04.07.00    NACL-多々納  11/09/XX  公費請求テーブル等対応追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    Ｐ０２画面スパ領域
           COPY    "P02SCR-SPA".
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PIDMSG          PIC X(100).
      *
           03  WRK-WAYMD.
               05  WRK-WAGO         PIC 9(01).
               05  WRK-WAYY         PIC 9(02).
               05  WRK-WAMM         PIC 9(02).
               05  WRK-WADD         PIC 9(02).
      *
           03  WRK-SEYMD.
               05  WRK-SEYY         PIC 9(04).
               05  WRK-SEMM         PIC 9(02).
               05  WRK-SEDD         PIC 9(02).
      *
           03  WRK-KETA             PIC 9(04).
           03  FLG-KETA             PIC 9(01).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換（管理）
       01  PTNUM-REC.
           COPY  "CPPTNUM.INC".
      *
      *    地方公費請求書
           COPY    "CPKOHSKY.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    入院オーダ
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    定期請求印刷
       01  TEIKIPRT-REC.
           COPY    "CPTEIKIPRT.INC".
      *
      *    分娩管理テーブル
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *   患者番号発生サブ
           COPY    "CPORCSP01.INC".
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
      *
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "P015"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "PID1"
                   IF      SPA-PID1-FLG        =   SPACE
                       CONTINUE
                   ELSE
                       PERFORM 4901-KOUSIN-SYORI-SEC
                   END-IF
                   MOVE    SPACE           TO  SPA-PIDCD
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
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
           INITIALIZE                      SPA-P015-AREA
           MOVE    1                   TO  SPA-GMN015-CUR
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      P015
      *
           MOVE    SPA-GMN-PTNUM       TO  P015-PTNUM1
           MOVE    SPA-GMN015-PTNUM    TO  P015-PTNUM2
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
         EVALUATE    SPA-GMN015-CUR
              WHEN    01
                  MOVE    "PTNUM2"       TO  MCP-WIDGET
              WHEN    02
                  MOVE    "B12"          TO  MCP-WIDGET
          END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE       TO  SPA-GMN015-PTNUM
                   PERFORM 210-BACK
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PTNUM2"
                   PERFORM 4001-PTNUM-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    患者番号処理
      *****************************************************************
       4001-PTNUM-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN015-CUR
           MOVE    P015-PTNUM2         TO  SPA-GMN015-PTNUM
      *
           IF     (SPA-GMN015-PTNUM        =   SPACE )  OR
                  (SPA-GMN015-PTNUM(1:1)   =   "*" AND
                   SPA-GMN015-PTNUM(2:)    =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               INITIALIZE                      ORCSPTNUMAREA
               MOVE    SPA-GMN015-PTNUM    TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                                   SPA-AREA
               EVALUATE    SPTNUM-RC
                   WHEN    00
      *            患者あり
                       MOVE    SPTNUM-PTNUM        TO  SPA-GMN015-PTNUM
                       MOVE    "0003"              TO  SPA-ERRCD
                   WHEN    20
      *            漢字
                       MOVE    "0001"              TO  SPA-ERRCD
                   WHEN    10
      *            患者構成チェック
                       MOVE    SPTNUM-PTNUM        TO  SPA-GMN015-PTNUM
                       PERFORM 40011-PTNUM-CHK-SEC
                   WHEN    OTHER
                       MOVE    "0001"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-GMN015-CUR
           END-IF
      *
           .
       4001-PTNUM-EXT.
           EXIT.
      *****************************************************************
      *    患者構成チェック処理
      *****************************************************************
       40011-PTNUM-CHK-SEC             SECTION.
      *
      *H30.10
      *    桁数チェック
           MOVE    ZERO                TO  WRK-KETA
           PERFORM   VARYING   IDX     FROM    20  BY  -1
                     UNTIL    (IDX         <   1 )
                           OR (WRK-KETA     >   ZERO )
               IF      SPA-GMN015-PTNUM(IDX:1)   NOT =   SPACE
                   MOVE    IDX               TO  WRK-KETA
               END-IF
           END-PERFORM
      *    間に空白がある時エラー
           MOVE    ZERO                TO  FLG-KETA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-KETA  )
                         OR   (SPA-ERRCD   NOT =   SPACE)
               IF      SPA-GMN015-PTNUM(IDX:1) =   SPACE
                   IF      FLG-KETA            =   1
                       MOVE    "1005"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-KETA
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  40011-PTNUM-CHK-EXT
           END-IF
      *
      *    患者構成チェック
           INITIALIZE                  ORCSP01AREA
           MOVE    "2"                 TO  ORCSP01-SYORI
           MOVE    SPA-GMN015-PTNUM    TO  ORCSP01-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ORCSP01-PTNUM       TO  SPA-GMN015-PTNUM
           END-IF
      *
           .
       40011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *    更新前チェック
           PERFORM 4001-PTNUM-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"          TO  SPA-PIDCD
           END-IF
      *
          .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4901-KOUSIN-SYORI-SEC           SECTION.
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN015-PTNUM    TO  PTNUM-PTNUM
           IF      SPA-GMN015-PTNUM(20:1) =   SPACE
               STRING  SPA-GMN015-PTNUM    DELIMITED   BY  SPACE
                       "%"                 DELIMITED   BY  SIZE
                       INTO                    PTNUM-PTNUM
               END-STRING
           END-IF
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTNUM           =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               PERFORM 49011-PTNUM-UPDATE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 210-BACK
           ELSE
               MOVE    1                   TO  SPA-GMN015-CUR
           END-IF
          .
       4901-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       49011-PTNUM-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 950-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF     (FLG-PTNUM           =   1  )  OR
                  (PTNUM-PTNUM     NOT =   SPA-GMN-PTNUM)
               MOVE    "0004"              TO  SPA-ERRCD
           ELSE
      *        患者番号更新
               MOVE    SPA-GMN015-PTNUM    TO  PTNUM-PTNUM
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P015 PTNUM UPDATE:" MCP-RC
                           ",KEY:" PTNUM-KEY
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    公費請求テーブル
           INITIALIZE                      KOHSKY-REC
           MOVE    SPA-HOSPNUM         TO  KOHSKY-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  KOHSKY-PTID
           MOVE    SPA-GMN015-PTNUM    TO  KOHSKY-PTNUM
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 KOHSKY UPDATE:" MCP-RC
                           ",KEY:" KOHSKY-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
      *    請求管理
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECE10-PTID
           MOVE    SPA-GMN015-PTNUM    TO  RECE10-PTNUM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 RECE10 UPDATE:" MCP-RC
                           ",KEY:" RECE10-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *    患者定期請求履歴
           INITIALIZE                      TEIKIPRT-REC
           MOVE    SPA-HOSPNUM         TO  TEIKIPRT-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  TEIKIPRT-PTID
           MOVE    SPA-GMN015-PTNUM    TO  TEIKIPRT-PTNUM
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 TEIKIPRT UPDATE:" MCP-RC
                           ",KEY:" TEIKIPRT-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
      *    分娩管理テーブル
           INITIALIZE                      BUNBEN-REC
           MOVE    SPA-HOSPNUM         TO  BUNBEN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  BUNBEN-PTID
           MOVE    SPA-GMN015-PTNUM    TO  BUNBEN-PTNUM
           MOVE    BUNBEN-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_bunben"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 BUNBEN UPDATE:" MCP-RC
                           ",KEY:" BUNBEN-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
      *    入院オーダテーブル更新
           INITIALIZE                      ORDER-REC
           MOVE    SPA-HOSPNUM         TO  ORDER-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  ORDER-PTID
           MOVE    SPA-GMN015-PTNUM    TO  ORDER-PTNUM
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 ORDER UPDATE:" MCP-RC
                           ",KEY:" ORDER-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-PTNUM           =   SPA-GMN-PTNUM
                   MOVE    SPA-GMN015-PTNUM    TO  SPA-PTNUM
               END-IF
               IF      SPA-GMN-PTNUM       =   SPA-LAST-PTNUM
                   MOVE    SPA-GMN015-PTNUM    TO  SPA-LAST-PTNUM
               END-IF
               MOVE    SPA-GMN015-PTNUM    TO  SPA-GMN-PTNUM
           END-IF
      *
           .
       49011-PTNUM-UPDATE-EXT.
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
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P015"              TO  MCP-WINDOW.
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "患者番号が違います。"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "既に登録済の患者番号です"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "患者ＩＤと患者番号が違います。"
                                           TO  SPA-ERRMSG
                   MOVE    "更新できません。"
                                           TO  SPA-ERRMSG(31:)
               WHEN    "0009"
                   MOVE    "マスタが更新できませんでした。"
                                           TO  SPA-ERRMSG
      *H30.10
               WHEN    "1013"
                   MOVE    "患者番号に使用できない文字があります。"
                                               TO  SPA-ERRMSG
      *
      *        サブエラー
               WHEN    "1005"
                   MOVE    "患者番号の桁数が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "患者番号の構成が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "患者番号の検証番号が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "患者番号の連番は管理連番以下にして下さい"
                                               TO  SPA-ERRMSG
      *        患者番号取得エラー
               WHEN    "8000"
                   MOVE    "自動採番の患者番号が最大値です。"
                                               TO  SPA-ERRMSG
                   MOVE    "初期番号から採番しました。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "8001"
                   MOVE    "患者ＩＤが最大値です。"
                                               TO  SPA-ERRMSG
                   MOVE    "初期番号から採番しました。"
                                               TO  SPA-ERRMSG(23:)
               WHEN    "8002"
                   MOVE    "自動採番の患者番号が最大値になりました。"
                                               TO  SPA-ERRMSG
                   MOVE    "連番号桁数を変更して下さい。"
                                               TO  SPA-ERRMSG(41:)
               WHEN    "8003"
                   MOVE    "患者ＩＤが登録済です。登録できません。"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P015"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   MOVE    "患者番号を変更します。"
                                               TO  WRK-PIDMSG
                   MOVE    "現在の患者番号は欠番となります。"
                                               TO  WRK-PIDMSG(23:)
                   MOVE    "よろしいですか？"
                                               TO  WRK-PIDMSG(55:)
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P015"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *
           MOVE    "P015"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "P02"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号変換読み込み
      *****************************************************************
       950-PTNUM-READ-SEC           SECTION.
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       950-PTNUM-READ-EXT.
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
      *
