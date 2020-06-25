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
       PROGRAM-ID.             ORCGN11.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 人名辞書情報（Ｎ１１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/06/18    MCC−森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-森脇    03/05/13  カナ入力のチェック中止
      *  00.00.02    NACL-森脇    04/10/05  半角全角チェックの追加
      *  04.06.00    NACL-多々納  11/03/22  LD104 ,LD105,LD106,LD108,LD109　統合
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N11COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-N11.
           03  SPA-N11-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(05).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-JYOKEN         PIC X(50).
                   07  SPA-GMN-NAIYO          PIC X(50).
                   07  SPA-GMN-SELNUM         PIC 9(03).
      *
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-G          OCCURS  200.
                           11  SPA-GMN-NO         PIC X(05).
                           11  SPA-GMN-NAME       PIC X(100).
      *
               05  SPA-NAI-AREA.
                   07  SPA-JYOKEN-KBN         PIC X(01).
                   07  SPA-NAIYO-KBN          PIC X(01).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-JINMEI          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH-TABLE      PIC X(64).
           03  WRK-PATH-PATHNAME   PIC X(64).
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-N11IDMSG        PIC X(70).
           03  WRK-ZZZZ9           PIC ZZZZ9.
           03  WRK-STR1            PIC X(100).
           03  WRK-STR2            PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    人名マスタ
       01  JINMEI-REC.
           COPY    "CPJINMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    カナ変換
           COPY    "CPORCSKANACNV.INC".
      *----(00.00.02)--ADD-START---
      *    全角半角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *    全角半角変換サブ
           COPY    "CPORCSKANACHK2.INC".
      *----(00.00.02)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
      *orca104
           COPY    "V01.INC".
           COPY    "V02.INC".
           COPY    "V03.INC".
           COPY    "VERR.INC".
           COPY    "VID1.INC".
      *orca105
           COPY    "N01.INC".
           COPY    "NERR.INC".
           COPY    "NID1.INC".
      *orca106
           COPY    "N11.INC".
           COPY    "N11ERR.INC".
           COPY    "N11ID1.INC".
      *orca108
           COPY    "N31.INC".
           COPY    "N31ERR.INC".
           COPY    "N31ID1.INC".
      *orca109
           COPY    "N41.INC".
           COPY    "N41ERR.INC".
           COPY    "N41ID1.INC".
      *
       PROCEDURE               DIVISION    USING
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-N11KYOUTU
           MOVE    SPA-FREE        TO  SPA-N11
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-N11         TO  SPA-FREE
           MOVE    SPA-N11KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           .
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "N11ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "N11"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "N11ID1"
                   PERFORM 330-N11ID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-N11ID1-FLG
           MOVE    SPACE               TO  SPA-N11IDCD
      *
           PERFORM 310-SPASET-SEC
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-N11-AREA
      *
           MOVE    2               TO  SPA-GMN-CUR
           . 
      *
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｎ１１ＩＤ１）ＯＫ処理
      *****************************************************************
       330-N11ID1-SET-SEC            SECTION.
      *
           IF      SPA-N11ID1-FLG    =   "OK"
               EVALUATE      SPA-N11IDCD     
                   WHEN  "1012"
                       PERFORM 4031-DEL-SEC
      *             WHEN  "1011"
      *                 PERFORM 490-KOUSHIN-SEC
               END-EVALUATE
           ELSE
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    SPACE           TO  SPA-N11ID1-FLG
           MOVE    SPACE           TO  SPA-N11IDCD
           .
      *
       330-N11ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"   ALSO    "B01"
                   PERFORM  210-BACK
      *    クリア
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM  403-CLEAR-SEC
      *    削除
               WHEN    "CLICKED"   ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM  490-TOUROKU-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "JYOKEN"
                   PERFORM 4111-JYOKEN-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4112-SELNUM-SEC
               WHEN    "ACTIVATE"      ALSO    "NAIYO"
                   PERFORM 4113-NAIYO-SEC
               WHEN    ANY             ALSO    "KEKKALST"
                   PERFORM 4114-KEKKALST-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件処理
      *****************************************************************
       4111-JYOKEN-SEC         SECTION.
      *
           PERFORM 310-SPASET-SEC
      *     MOVE    2               TO  SPA-GMN-CUR
      *
           MOVE    N11-JYOKEN      TO  SPA-GMN-JYOKEN
      *
      *     MOVE    ZERO            TO  SPA-GMN-SELNUM
      *     MOVE    SPACE           TO  SPA-GMN-NAIYO
      *
           IF      SPA-GMN-JYOKEN      NOT  =   SPACE
      *    半角文字混在チェック
               PERFORM  4121-JYOKEN-CHK-SEC
      *
               IF  SPA-ERRCD  NOT  =  SPACE
                   GO  TO  4111-JYOKEN-EXT
               END-IF
      *    結果リストセット
               EVALUATE  KANACNV-TYPE
                   WHEN  2
                       MOVE    "2"         TO  SPA-JYOKEN-KBN
                       PERFORM 500-KANJILIST-SET-SEC
                   WHEN  OTHER
                       MOVE    "1"         TO  SPA-JYOKEN-KBN
                       PERFORM 500-KANALIST-SET-SEC
               END-EVALUATE
      *
           END-IF
      *
           MOVE    3                TO  SPA-GMN-CUR
           .
      *
       4111-JYOKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件チェック
      *****************************************************************
       4121-JYOKEN-CHK-SEC         SECTION.
      *
      *----(01.00.02)--ADD-START---
           INITIALIZE              ORCSKANACHK2AREA
           MOVE    "1"             TO  KANACHK2-SYORI
           MOVE    SPA-GMN-JYOKEN  TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"       USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-OUT-INPUT  TO  SPA-GMN-JYOKEN
      *    全角半角混在エラー
           IF    ( KANACHK2-SYUBETU     NOT =   2    )  OR
                 ( KANACHK2-RC          NOT =   ZERO )  OR
                 ( KANACHK2-RC2             =   7    )
               MOVE    "0002"      TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4121-JYOKEN-CHK-EXT
           END-IF
      *----(01.00.02)--ADD-END-----
      *
      *    半角文字混在チェック
           INITIALIZE              KANACNV-LNK
           MOVE    SPA-GMN-JYOKEN  TO  KANACNV-IN-STR
           CALL    "ORCSKANACNV"   USING
                                   KANACNV-LNK
      *
           EVALUATE  KANACNV-RC
               WHEN  0
                   CONTINUE
               WHEN  OTHER
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       4121-JYOKEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     選択番号処理
      *****************************************************************
       4112-SELNUM-SEC         SECTION.
      *
           MOVE    3               TO  SPA-GMN-CUR
      *
           MOVE    N11-SELNUM      TO  SPA-GMN-SELNUM
      *
           IF  SPA-GMN-SELNUM          >   ZERO
      *    番号チェック
               PERFORM  4122-SELNUM-CHK-SEC
      *
               IF    SPA-ERRCD  NOT  =  SPACE
                   GO  TO  4112-SELNUM-EXT
               END-IF
      *
               MOVE    SPA-GMN-NAME (SPA-GMN-SELNUM)
                                   TO  SPA-GMN-NAIYO
               EVALUATE  SPA-JYOKEN-KBN
                   WHEN  "1"
                       MOVE  "2"       TO  SPA-NAIYO-KBN
                   WHEN  "2"
                       MOVE  "1"       TO  SPA-NAIYO-KBN
               END-EVALUATE
           END-IF
      *
           MOVE    4               TO  SPA-GMN-CUR
           .
      *
       4112-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *     選択番号チェック
      *****************************************************************
       4122-SELNUM-CHK-SEC         SECTION.
      *
      *    番号チェック
           IF      SPA-GMN-SELNUM        >   SPA-GMN-MAX
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    "1013"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
           .
      *
       4122-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     登録内容処理
      *****************************************************************
       4113-NAIYO-SEC          SECTION.
      *
           MOVE    4               TO  SPA-GMN-CUR
      *
           MOVE    N11-NAIYO       TO  SPA-GMN-NAIYO
      *
           MOVE    SPACE           TO  SPA-NAIYO-KBN
      *
           IF    SPA-GMN-NAIYO     NOT  =   SPACE
      *    半角文字混在チェック
               PERFORM 4123-NAIYO-CHK-SEC
      *
               IF  SPA-ERRCD  NOT  =  SPACE
                   GO  TO  4113-NAIYO-EXT
               END-IF
      *    結果リストセット
               EVALUATE  KANACNV-TYPE
                   WHEN  2
                       MOVE    "2"         TO  SPA-NAIYO-KBN
                   WHEN  OTHER
                       MOVE    "1"         TO  SPA-NAIYO-KBN
               END-EVALUATE
      *
            END-IF
      *
            MOVE    99             TO  SPA-GMN-CUR
           .
      *
       4113-NAIYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録内容チェック
      *****************************************************************
       4123-NAIYO-CHK-SEC         SECTION.
      *
      *----(01.00.02)--ADD-START---
           INITIALIZE              ORCSKANACHK2AREA
           MOVE    "1"             TO  KANACHK2-SYORI
           MOVE    SPA-GMN-NAIYO   TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-OUT-INPUT  TO  SPA-GMN-NAIYO
      *    全角半角混在エラー
           IF    ( KANACHK2-SYUBETU    NOT =   2    )  OR
                 ( KANACHK2-RC         NOT =   ZERO )  OR
                 ( KANACHK2-RC2            =   7    )
               MOVE    "0002"      TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  4123-NAIYO-CHK-EXT
           END-IF
      *----(01.00.02)--ADD-END-----
      *
      *    半角文字混在チェック
           INITIALIZE              KANACNV-LNK
           MOVE    SPA-GMN-NAIYO  TO  KANACNV-IN-STR
           CALL    "ORCSKANACNV"   USING
                                   KANACNV-LNK
      *
           EVALUATE  KANACNV-RC
               WHEN  0
                   CONTINUE
               WHEN  OTHER
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       4123-NAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    結果リスト チェック処理
      *****************************************************************
       4114-KEKKALST-SEC        SECTION.
      *
      *    行選択
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       200
               IF    N11-KEKKALST-SELECT(IDX)  =   "T"
                   MOVE    IDX             TO  SPA-GMN-SELNUM
                   MOVE    200     TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-NAME (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-NAIYO
           EVALUATE  SPA-JYOKEN-KBN
               WHEN  "1"
                   MOVE  "2"           TO  SPA-NAIYO-KBN
               WHEN  "2"
                   MOVE  "1"           TO  SPA-NAIYO-KBN
           END-EVALUATE 
           .
      *
       4114-KEKKALST-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字リストセット処理
      *****************************************************************
       500-KANJILIST-SET-SEC       SECTION.
      *
           INITIALIZE                  JINMEI-REC
           MOVE    SPA-GMN-JYOKEN  TO  JINMEI-KANA
           MOVE    SPA-HOSPNUM     TO  JINMEI-HOSPNUM
           MOVE    JINMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_jinmei"    TO  WRK-PATH-TABLE
           MOVE    "key2"          TO  WRK-PATH-PATHNAME
           PERFORM 900-JINMEI-SELECT-SEC
           IF      FLG-JINMEI        =   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >    200
                   MOVE    IDX           TO  WRK-ZZZZ9
                   MOVE    WRK-ZZZZ9     TO  SPA-GMN-NO(IDX)
                   MOVE    JINMEI-KANJI  TO  SPA-GMN-NAME(IDX)
                   PERFORM 900-JINMEI-FETCH-SEC
                   IF   FLG-JINMEI    NOT  =  ZERO
                        MOVE    IDX       TO  SPA-GMN-MAX
                        PERFORM 900-JINMEI-CLOSE-SEC
                        GO TO 500-KANJILIST-CHK-EXT
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 900-JINMEI-CLOSE-SEC
          .
      *
       500-KANJILIST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナリストセット処理
      *****************************************************************
       500-KANALIST-SET-SEC        SECTION.
      *
           INITIALIZE                  JINMEI-REC
           MOVE    SPA-GMN-JYOKEN  TO  JINMEI-KANJI
           MOVE    SPA-HOSPNUM     TO  JINMEI-HOSPNUM
           MOVE    JINMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_jinmei"    TO  WRK-PATH-TABLE
           MOVE    "key3"          TO  WRK-PATH-PATHNAME
           PERFORM 900-JINMEI-SELECT-SEC
           IF      FLG-JINMEI      =   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >    200
                   MOVE    IDX           TO  WRK-ZZZZ9
                   MOVE    WRK-ZZZZ9     TO  SPA-GMN-NO(IDX)
                   MOVE    JINMEI-KANA   TO  SPA-GMN-NAME(IDX)
                   PERFORM 900-JINMEI-FETCH-SEC
                   IF  FLG-JINMEI       NOT  =  ZERO
                       MOVE    IDX       TO  SPA-GMN-MAX
                       PERFORM 900-JINMEI-CLOSE-SEC
                       GO TO 500-KANALIST-CHK-EXT
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 900-JINMEI-CLOSE-SEC
           .
      *
       500-KANALIST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "M02"               TO  SPA-SAKIPG
           MOVE    "N11"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       403-CLEAR-SEC                   SECTION.
      *
      *    画面初期化
           PERFORM 310-SPASET-SEC
      *
           .
      *
       403-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
      *    チェック
           IF  SPA-GMN-JYOKEN   =   SPACE
               MOVE    "1001"   TO  SPA-ERRCD
               MOVE    2        TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD        =   SPACE
               PERFORM  4121-JYOKEN-CHK-SEC
               IF      SPA-ERRCD        =   SPACE
                   PERFORM  4122-SELNUM-CHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD        =   SPACE
              MOVE    "1012"           TO  SPA-N11IDCD
           END-IF
           .
      *
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                SECTION.
      *
           MOVE    SPACE           TO  JINMEI-REC
           INITIALIZE                  JINMEI-REC
      *
           EVALUATE  SPA-JYOKEN-KBN
               WHEN  "1"
                   MOVE    SPA-GMN-JYOKEN          TO  JINMEI-KANJI
                   MOVE    SPA-GMN-NAME(SPA-GMN-SELNUM)
                                                   TO  JINMEI-KANA
               WHEN  "2"
                   MOVE    SPA-GMN-NAME(SPA-GMN-SELNUM)
                                                   TO  JINMEI-KANJI
                   MOVE    SPA-GMN-JYOKEN          TO  JINMEI-KANA
           END-EVALUATE
      *
           MOVE    SPA-HOSPNUM     TO  JINMEI-HOSPNUM
           MOVE    JINMEI-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_jinmei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO
               MOVE    "1012"                  TO  SPA-ERRCD
               DISPLAY "JINMEI-KEY =>" SPA-GMN-JYOKEN
               GO  TO  4031-DEL-EXT
           END-IF
      *
           PERFORM  4111-JYOKEN-SEC
           .
      *
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      *    チェック
           IF  SPA-GMN-JYOKEN   =   SPACE
               MOVE    "1001"   TO  SPA-ERRCD
               MOVE    2        TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD        =   SPACE
               PERFORM  4121-JYOKEN-CHK-SEC
               IF      SPA-ERRCD        =   SPACE
                   PERFORM  4123-NAIYO-CHK-SEC
               END-IF
           END-IF
      *
      *    カナ（漢字）チェック
           IF      SPA-ERRCD        =   SPACE
               IF  SPA-GMN-NAIYO   =   SPACE
                   EVALUATE  SPA-JYOKEN-KBN
                       WHEN  "1"
                           MOVE    "1002"   TO  SPA-ERRCD
                       WHEN  "2"
                           MOVE    "1003"   TO  SPA-ERRCD
                   END-EVALUATE
               ELSE
                   EVALUATE  SPA-JYOKEN-KBN
                       WHEN  "1"
                           IF  SPA-NAIYO-KBN  NOT  =  "2"
                               MOVE    "1002"   TO  SPA-ERRCD
                           END-IF
                       WHEN  "2"
      *----(00.00.01)--UPD-START---
      *                     IF  SPA-NAIYO-KBN  NOT  =  "1"
      *                         MOVE    "1003"   TO  SPA-ERRCD
      *                     END-IF
                           CONTINUE
      *----(00.00.01)--UPD-END-----
                   END-EVALUATE
               END-IF
           ELSE
               GO  TO  490-TOUROKU-EXT
           END-IF
      *
      *    同一人名チェック
           IF      SPA-ERRCD        =   SPACE
               MOVE    SPACE           TO  JINMEI-REC
               INITIALIZE                  JINMEI-REC
      *
               EVALUATE  SPA-JYOKEN-KBN
                   WHEN  "1"
                       MOVE    SPA-GMN-JYOKEN          TO  JINMEI-KANJI
                       MOVE    SPA-GMN-NAIYO           TO  JINMEI-KANA
                   WHEN  "2"
                       MOVE    SPA-GMN-NAIYO           TO  JINMEI-KANJI
                       MOVE    SPA-GMN-JYOKEN          TO  JINMEI-KANA
               END-EVALUATE
      *
               MOVE    SPA-HOSPNUM     TO  JINMEI-HOSPNUM
               MOVE    JINMEI-REC      TO  MCPDATA-REC
               MOVE    "tbl_jinmei"    TO  WRK-PATH-TABLE
               MOVE    "key"           TO  WRK-PATH-PATHNAME
               PERFORM 900-JINMEI-SELECT-SEC
               PERFORM 900-JINMEI-CLOSE-SEC
               IF  FLG-JINMEI  =  ZERO
                   MOVE    "1010"   TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               PERFORM 490-KOUSHIN-SEC
           END-IF
      *
           .
      *
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC             SECTION.
      *
           MOVE    SPACE           TO  JINMEI-REC
           INITIALIZE                  JINMEI-REC
      *
           EVALUATE  SPA-JYOKEN-KBN
               WHEN  "1"
                   MOVE    SPA-GMN-JYOKEN          TO  JINMEI-KANJI
                   MOVE    SPA-GMN-NAIYO           TO  JINMEI-KANA
               WHEN  "2"
                   MOVE    SPA-GMN-NAIYO           TO  JINMEI-KANJI
                   MOVE    SPA-GMN-JYOKEN          TO  JINMEI-KANA
           END-EVALUATE
      *
           ACCEPT  SYS-TIME        FROM    TIME
           MOVE    SYS-TIME        TO  JINMEI-UPHMS
           MOVE    SPA-SYSYMD      TO  JINMEI-UPYMD
      *
           MOVE    SPA-HOSPNUM     TO  JINMEI-HOSPNUM
           MOVE    JINMEI-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_jinmei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1011"          TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
           PERFORM  4111-JYOKEN-SEC
           .
      *
       490-KOUSHIN-EXT.
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-N11IDCD       NOT =   SPACE
               PERFORM 520-N11IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "N11    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-JYOKEN          TO  N11-JYOKEN
           MOVE    SPA-GMN-NAIYO           TO  N11-NAIYO
           MOVE    SPA-GMN-SELNUM          TO  N11-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               MOVE    SPA-GMN-NO(IDX)
                                           TO  N11-KEKKALST-NO(IDX)
               MOVE    SPA-GMN-NAME(IDX)
                                           TO  N11-KEKKALST-NAME(IDX)
               IF      IDX             =   SPA-GMN-SELNUM
                   MOVE    "T"         TO  N11-KEKKALST-SELECT(IDX)
               ELSE
                   MOVE    "F"         TO  N11-KEKKALST-SELECT(IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  N11-KEKKALST-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
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
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "全て全角で入力して下さい"  
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "範囲外の番号が入力されています"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "検索条件を入力して下さい" 
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "カナを入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "漢字を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE    SPA-GMN-NAIYO       TO  WRK-STR1
                   MOVE    "】はすでに登録されています"
                                               TO  WRK-STR2
                   PERFORM  530-MSGSET-SEC
                   MOVE    WRK-N11IDMSG          TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    SPA-GMN-NAIYO       TO  WRK-STR1
                   MOVE    "】は登録できませんでした"
                                               TO  WRK-STR2
                   PERFORM  530-MSGSET-SEC
                   MOVE    WRK-N11IDMSG          TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE  SPA-GMN-NAME(SPA-GMN-SELNUM) 
                                               TO  WRK-STR1
                   MOVE    "】は削除できませんでした"
                                               TO  WRK-STR2
                   PERFORM  530-MSGSET-SEC
                   MOVE    WRK-N11IDMSG          TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "番号を入力してください"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  N11ERR
           MOVE    SPA-ERRCD            TO  N11ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  N11ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "N11"                TO  SPA-MOTOPG
           MOVE    "N11ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "N11ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-N11IDSET-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-N11IDMSG
      *
           EVALUATE    SPA-N11IDCD
      *         WHEN    "1011"
      *             MOVE    "人名辞書情報を登録します"
      *                                          TO  WRK-N11IDMSG
               WHEN    "1012"
                   MOVE  SPA-GMN-NAME(SPA-GMN-SELNUM)
                                                  TO  WRK-STR1
                   MOVE    "】を削除します"       TO  WRK-STR2
                   PERFORM  530-MSGSET-SEC
           END-EVALUATE
      *
           MOVE    SPACE           TO  N11ID1
           INITIALIZE                  N11ID1
           MOVE    SPA-N11IDCD     TO  N11ID1-ID1CODE
           MOVE    WRK-N11IDMSG    TO  N11ID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N11"           TO  SPA-MOTOPG
           MOVE    "N11ID1"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N11ID1"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
      *
       520-N11IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    メッセージ作成
      *****************************************************************
       530-MSGSET-SEC              SECTION.
      *
           STRING "【"     DELIMITED BY SPACE
                  WRK-STR1 DELIMITED BY SPACE
                  WRK-STR2 DELIMITED BY SPACE
                  INTO WRK-N11IDMSG
           END-STRING
           .
      *
       530-MSGSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    2
                   MOVE  "JYOKEN"      TO  MCP-WIDGET
               WHEN    3
                   MOVE  "SELNUM"      TO  MCP-WIDGET
               WHEN    4
                   MOVE  "NAIYO"       TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"         TO  MCP-WIDGET
           END-EVALUATE
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    人名マスター読込（キー）
      *****************************************************************
       900-JINMEI-SELECT-SEC       SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-JINMEI
               PERFORM  900-JINMEI-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-JINMEI
           END-IF
      *
            .
       900-JINMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    人名マスター読込（キー）
      *****************************************************************
       900-JINMEI-FETCH-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-JINMEI
               MOVE    MCPDATA-REC     TO  JINMEI-REC
           ELSE
               MOVE    1               TO  FLG-JINMEI
           END-IF
           .
      *
       900-JINMEI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    人名マスター読込（キー）
      *****************************************************************
       900-JINMEI-CLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-JINMEI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL   "ORCDBMAIN"      USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
