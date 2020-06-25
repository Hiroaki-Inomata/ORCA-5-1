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
       PROGRAM-ID.             ORCGP02S.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 主科設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/02/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  04.08.00    NACL-多々納  15/06/16  診療科・保険組合せ件数対応
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
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
      *
           03  FLG-HEN             PIC 9(01).
           03  FLG-OK              PIC 9(01).
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
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *    主科テーブル
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
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
      *    主情報アクセス用パラメタ
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
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
               MOVE   "P02S"               TO  MCP-WINDOW
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
                       PERFORM 4902-KOUSIN-SYORI-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                       END-IF
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
           INITIALIZE                      SPA-P02S-AREA
      *    システム年月を初期表示とする
           MOVE    SPA-SYSYMD (1:6)    TO  SPA-P02S-NAI-SRYYM
           MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-P02S-GMN-SRYYM
      *    新規で設定済み
           IF      SPA-GMN-PTID        =   ZERO
               IF      SPA-NAI-SYUKAYM     NOT =   SPACE
                   MOVE    SPA-NAI-SYUKAYM     TO  SPA-P02S-NAI-SRYYM
                   MOVE    SPA-NAI-SYUKAYMG    TO  SPA-P02S-GMN-SRYYM
               END-IF
           END-IF
      *
      *    主科一覧編集
           PERFORM 3101-SYUKA-HEN-SEC
      *
           IF      SPA-GMN-PTID        =   ZERO
      *        新規登録時、空白設定
               MOVE    SPACE               TO  SPA-P02S-SYUKA-G
               MOVE    SPA-NAI-SYUKA       TO  SPA-P02S-SYUKA
           ELSE
               PERFORM 3102-SYUKASUB-CALL-SEC
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02S-SYUKA-MAX
               IF      SPA-P02S-SYUKA  =   SPA-P02S-SYUKA-LST(IDX)(1:2)
                   MOVE    SPA-P02S-SYUKA-LST(IDX)
                                       TO  SPA-P02S-SYUKA-G
                   MOVE    SPA-P02S-SYUKA-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-P02S-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科一覧編集処理
      *****************************************************************
       3101-SYUKA-HEN-SEC              SECTION.
      *
           INITIALIZE                      SPA-P02S-SYUKA-COB
           MOVE    ZERO                TO  SPA-P02S-SYUKA-MAX
      *    診療科一覧
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-P02S-NAI-SRYYM  TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
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
           PERFORM VARYING IDX
                   FROM    1       BY  1
      *********    UNTIL      (IDX             >   40    )   OR
                   UNTIL      (IDX             >   99    )   OR
                              (FLG-SYSKANRI    =   1     )
               MOVE    MCPDATA-REC             TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)    TO  SPA-P02S-SYUKA-LST
                                                        (IDX)
               MOVE    SYS-1005-SRYKANAME1     TO  SPA-P02S-SYUKA-LST
                                                        (IDX)(4:)
               MOVE    IDX                     TO  SPA-P02S-SYUKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-SYUKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科検索サブ処理
      *****************************************************************
       3102-SYUKASUB-CALL-SEC              SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
      *    処理区分
           MOVE    "01"                TO  LNK-SYUACC-I-KBN
      *    診療年月
           MOVE    SPA-P02S-NAI-SRYYM  TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
      *    診療科(手入力情報)
           MOVE    SPACE               TO  SPA-P02S-SYUKA-G
           IF       LNK-SYUACC-RC      =   ZERO
               MOVE    LNK-SYUACC-IO-H-SRYKA   TO  SPA-P02S-SYUKA
           END-IF
           .
       3102-SYUKASUB-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      P02S
      *
           MOVE    SPA-P02S-GMN-SRYYM  TO  P02S-SRYYM
      *
           MOVE    SPA-P02S-SYUKA-G    TO  P02S-SYUKA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02S-SYUKA-MAX
               MOVE    SPA-P02S-SYUKA-LST(IDX)
                                       TO  P02S-SYUKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-P02S-SYUKA-MAX  TO  P02S-SYUKA-COUNT
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
         EVALUATE    SPA-P02S-CUR
              WHEN    01
                  MOVE    "SRYYM"       TO  MCP-WIDGET
              WHEN    02
                  MOVE    "SYUKA"       TO  MCP-WIDGET
              WHEN    03
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
                   MOVE    SPACE               TO  MCP-PUTTYPE
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
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 4001-SRYYM-SEC
               WHEN    "ACTIVATE"      ALSO    "SYUKA"
                   PERFORM 4002-SYUKA-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    診療年月処理
      *****************************************************************
       4001-SRYYM-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-P02S-CUR
           MOVE    P02S-SRYYM          TO  SPA-P02S-GMN-SRYYM
      *
           MOVE    ZERO                TO  FLG-HEN
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P02S-GMN-SRYYM  TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               IF      SGDAY-SDAY (1:6)    =   SPA-P02S-NAI-SRYYM
                   MOVE    ZERO            TO  FLG-HEN
               ELSE
                   MOVE    1               TO  FLG-HEN
               END-IF
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-P02S-GMN-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-P02S-NAI-SRYYM
      *        誕生月以降であること
               IF      SPA-P02S-NAI-SRYYM  <   SPA-NAI-BIRTHDAY(1:6)
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-P02S-CUR
               END-IF
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P02S-CUR
               MOVE    SPACE               TO  SPA-P02S-NAI-SRYYM
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-HEN         =   1
                   PERFORM 40011-SRYYM-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-P02S-CUR
           END-IF
      *
           .
       4001-SRYYM-EXT.
           EXIT.
      *****************************************************************
      *    診療年月変更処理
      *****************************************************************
       40011-SRYYM-HEN-SEC             SECTION.
      *
      *    主科一覧編集
           PERFORM 3101-SYUKA-HEN-SEC
      *    主科編集
           PERFORM 3102-SYUKASUB-CALL-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02S-SYUKA-MAX
               IF      SPA-P02S-SYUKA  =   SPA-P02S-SYUKA-LST(IDX)(1:2)
                   MOVE    SPA-P02S-SYUKA-LST(IDX)
                                       TO  SPA-P02S-SYUKA-G
                   MOVE    SPA-P02S-SYUKA-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           .
       40011-SRYYM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主科処理
      *****************************************************************
       4002-SYUKA-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-P02S-CUR
           MOVE    P02S-SYUKA          TO  SPA-P02S-SYUKA-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-P02S-SYUKA-MAX)
                          OR  (FLG-OK  =   1   )
               IF      SPA-P02S-SYUKA  =   SPA-P02S-SYUKA-LST(IDX)(1:2)
                   MOVE    SPA-P02S-SYUKA-LST(IDX)
                                               TO  SPA-P02S-SYUKA-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-P02S-SYUKA      =   SPACE
               MOVE    SPACE               TO  SPA-P02S-SYUKA-G
               MOVE    1                   TO  FLG-OK
           END-IF
           IF      FLG-OK              =   1
               MOVE    3                   TO  SPA-P02S-CUR
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
      *
           .
       4002-SYUKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *    更新前チェック
           IF      SPA-P02S-NAI-SRYYM  =   SPACE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P02S-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4002-SYUKA-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-P02-SYORI       =   1  ) AND
                      (SPA-GMN-PTID        =   ZERO)
      *            新規登録時、ＳＰＡへ
                   PERFORM 4901-SINKI-SYORI-SEC
               ELSE
                   MOVE    "0101"          TO  SPA-PIDCD
               END-IF
           END-IF
      *
          .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    新規登録処理
      *****************************************************************
       4901-SINKI-SYORI-SEC           SECTION.
      *
           MOVE    SPA-P02S-SYUKA       TO  SPA-NAI-SYUKA
           MOVE    SPA-P02S-NAI-SRYYM   TO  SPA-NAI-SYUKAYM
           MOVE    SPA-P02S-GMN-SRYYM   TO  SPA-NAI-SYUKAYMG
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4901-SINKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4902-KOUSIN-SYORI-SEC           SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
      *    処理区分
           MOVE    "02"                TO  LNK-SYUACC-I-KBN
      *    診療年月
           MOVE    SPA-P02S-NAI-SRYYM  TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-P02S-SYUKA      TO  LNK-SYUACC-IO-H-SRYKA
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
           IF       LNK-SYUACC-RC  NOT =   ZERO
               MOVE    "1000"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-NAI-SYUKA
               MOVE    SPACE               TO  SPA-NAI-SYUKAYM
               PERFORM 210-BACK
           END-IF
           .
       4902-KOUSIN-SYORI-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02S"              TO  MCP-WINDOW
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
                   MOVE    "診療年月入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "主科がありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "診療年月が生年月以前です。"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "診療年月を入力して下さい。"
                                           TO  SPA-ERRMSG
               WHEN    "1000"
                   MOVE    "マスタが更新できませんでした。"
                                           TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02S"              TO  SPA-MOTOPG
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
                   MOVE    "主科を登録します。よろしいですか？"
                                               TO  WRK-PIDMSG
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
           MOVE    "P02S"              TO  SPA-MOTOPG
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
           MOVE    "P02S"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
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
      *
