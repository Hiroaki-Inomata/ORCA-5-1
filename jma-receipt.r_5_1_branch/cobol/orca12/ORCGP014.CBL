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
       PROGRAM-ID.             ORCGP014.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者番号発行（拡張（P014）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/12/05    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  05.00.00    NACL-多々納  18/10/XX  患者番号空白等チェック追加
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
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTGETNUM        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-KANRIMST        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
           03  IDX-LINE            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-KNJID           PIC 9(10).
           03  WRK-END             PIC 9(04).
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
           INITIALIZE                  CNT-AREA
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
           INITIALIZE                  CNT-AREA
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
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "P014"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-1009-KKCKSIKBN
      *        拡張構成 <フリー + 連番号 + フリー>
               WHEN    "1"
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        拡張構成 <フリー + 連番号>
               WHEN    "2"
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        拡張構成 <連番号 + フリー>
               WHEN    "3"
                   MOVE    2                   TO  SPA-GMN014-CUR
           END-EVALUATE
      *
           
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
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
           INITIALIZE                      P014
      *
           MOVE    SPA-GMN014-PTNUM1   TO  P014-PTNUM1
           MOVE    SPA-GMN014-PTNUM2   TO  P014-PTNUM2
           MOVE    SPA-GMN014-PTNUM3   TO  P014-PTNUM3
      *
      *    拡張構成区分
           MOVE    WIDGET-INSENSITIVE  TO  P014-PTNUM1-STATE
           MOVE    WIDGET-INSENSITIVE  TO  P014-PTNUM2-STATE
           MOVE    WIDGET-INSENSITIVE  TO  P014-PTNUM3-STATE
      *
           EVALUATE    SPA-1009-KKCKSIKBN
      *        拡張構成 <フリー + 連番号 + フリー>
               WHEN    "1"
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM1-STATE
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM3-STATE
      *        拡張構成 <フリー + 連番号>
               WHEN    "2"
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM1-STATE
      *        拡張構成 <連番号 + フリー>
               WHEN    "3"
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM2-STATE
           END-EVALUATE
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
         EVALUATE    SPA-GMN014-CUR
              WHEN    01
                  MOVE    "PTNUM1"       TO  MCP-WIDGET
              WHEN    02
                  MOVE    "PTNUM2"       TO  MCP-WIDGET
              WHEN    03
                  MOVE    "PTNUM3"       TO  MCP-WIDGET
              WHEN    04
                  MOVE    "B12"          TO  MCP-WIDGET
      *----(01.00.01) LINE ADD START -------------------------------
              WHEN    05
                  MOVE    "B04"          TO  MCP-WIDGET
      *----(01.00.01) LINE ADD END   -------------------------------
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
                   PERFORM 210-BACK
      *        再発行
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 420-SAIHAKKOU-SEC
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
               WHEN    "ACTIVATE"      ALSO    "PTNUM1"
                   PERFORM 4001-PTNUM1-SEC
               WHEN    "ACTIVATE"      ALSO    "PTNUM2"
                   PERFORM 4002-PTNUM2-SEC
               WHEN    "ACTIVATE"      ALSO    "PTNUM3"
                   PERFORM 4003-PTNUM3-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    番号１処理
      *****************************************************************
       4001-PTNUM1-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN014-CUR
           MOVE    P014-PTNUM1         TO  SPA-GMN014-PTNUM1
      *
      *    桁数チェック
           MOVE    ZERO              TO  WRK-END
      *H30.10
           PERFORM   VARYING   IDX   FROM    20  BY  -1
                     UNTIL    (IDX           <   1    )
                         OR   (WRK-END       >   ZERO )
               IF      (SPA-GMN014-PTNUM1(IDX:1) NOT =   SPACE)
                   MOVE    IDX                 TO  WRK-END
               END-IF
           END-PERFORM
      *
      *    拡張構成前桁数チェック
           IF      WRK-END         NOT =   SPA-1009-KKCKSIMAEKETA
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     SPA-GMN014-PTNUM1(1:1)   =   "*"
                   MOVE    "0004"              TO  SPA-ERRCD
               END-IF
           END-IF
      *H30.10
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-END )
                         OR   (SPA-ERRCD   NOT =   SPACE)
      *        \ はエラー
               IF      SPA-GMN014-PTNUM1(IDX:1)    =   "\"
                  MOVE    "1013"           TO  SPA-ERRCD
               END-IF
      *        空白エラー
               IF      SPA-GMN014-PTNUM1(IDX:1)    =   SPACE
                  MOVE    "0001"           TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4001-PTNUM1-EXT
           END-IF
      *
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-1009-KKCKSIKBN
      *        拡張構成 <フリー + 連番号 + フリー>
                   WHEN    "1"
                       MOVE    3               TO  SPA-GMN014-CUR
                   WHEN    OTHER
                       MOVE    4               TO  SPA-GMN014-CUR
               END-EVALUATE
           END-IF
           .
       4001-PTNUM1-EXT.
           EXIT.
      *****************************************************************
      *    番号２処理
      *****************************************************************
       4002-PTNUM2-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN014-CUR
           MOVE    P014-PTNUM2         TO  SPA-GMN014-PTNUM2
      *
      *    桁数チェック
           MOVE    ZERO              TO  WRK-END
           PERFORM   VARYING   IDX   FROM    20  BY  -1
                     UNTIL    (IDX           <   1    )
                         OR   (WRK-END       >   ZERO )
               IF      (SPA-GMN014-PTNUM2(IDX:1) NOT =   SPACE)
                   MOVE    IDX                 TO  WRK-END
               END-IF
           END-PERFORM
      *    拡張構成後桁数チェック
           IF      WRK-END         NOT =   SPA-1009-KKCKSIATOKETA
               MOVE    "0001"          TO  SPA-ERRCD
      ***  ELSE
      ***      MOVE    4               TO  SPA-GMN014-CUR
           END-IF
      *H30.10
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-END )
                         OR   (SPA-ERRCD   NOT =   SPACE)
      *        \ はエラー
               IF      SPA-GMN014-PTNUM2(IDX:1)    =   "\"
                  MOVE    "1013"           TO  SPA-ERRCD
               END-IF
      *        空白エラー
               IF      SPA-GMN014-PTNUM2(IDX:1)    =   SPACE
                  MOVE    "0001"           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    4               TO  SPA-GMN014-CUR
           END-IF
      *
           .
       4002-PTNUM2-EXT.
           EXIT.
      *****************************************************************
      *    番号３処理
      *****************************************************************
       4003-PTNUM3-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-GMN014-CUR
           MOVE    P014-PTNUM3         TO  SPA-GMN014-PTNUM3
      *
      *    桁数チェック
           MOVE    ZERO              TO  WRK-END
           PERFORM   VARYING   IDX   FROM    20  BY  -1
                     UNTIL    (IDX           <   1 )
                           OR (WRK-END       >   ZERO )
               IF      (SPA-GMN014-PTNUM3(IDX:1) NOT =   SPACE)
                   MOVE    IDX                 TO  WRK-END
               END-IF
           END-PERFORM
      *
      *    拡張構成後桁数チェック
           IF      WRK-END         NOT =   SPA-1009-KKCKSIATOKETA
               MOVE    "0001"          TO  SPA-ERRCD
      ***  ELSE
      ***      MOVE    4               TO  SPA-GMN014-CUR
           END-IF
      *
      *H30.10
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-END )
                         OR   (SPA-ERRCD   NOT =   SPACE)
      *        \ はエラー
               IF      SPA-GMN014-PTNUM3(IDX:1)    =   "\"
                  MOVE    "1013"           TO  SPA-ERRCD
               END-IF
      *        空白エラー
               IF      SPA-GMN014-PTNUM3(IDX:1)    =   SPACE
                  MOVE    "0001"           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    4               TO  SPA-GMN014-CUR
           END-IF
           .
       4003-PTNUM3-EXT.
           EXIT.
      *****************************************************************
      *    再発行処理
      *****************************************************************
       420-SAIHAKKOU-SEC             SECTION.
      *
           MOVE    5                   TO  SPA-GMN014-CUR
      *
           INITIALIZE                  ORCSP01AREA
           MOVE    "3"                 TO  ORCSP01-SYORI
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
           MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      420-SAIHAKKOU-EXT
           END-IF
      *
           EVALUATE    SPA-1009-KKCKSIKBN
      *        拡張構成 <フリー + 連番号 + フリー>
               WHEN    "1"
                   MOVE    ORCSP01-PTNUM2      TO  SPA-GMN014-PTNUM2
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        拡張構成 <フリー + 連番号>
               WHEN    "2"
                   MOVE    ORCSP01-PTNUM2      TO  SPA-GMN014-PTNUM2
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        拡張構成 <連番号 + フリー>
               WHEN    "3"
                   MOVE    ORCSP01-PTNUM1      TO  SPA-GMN014-PTNUM1
                   MOVE    2                   TO  SPA-GMN014-CUR
           END-EVALUATE
      *
           MOVE    4                   TO  SPA-GMN014-CUR
           .
       420-SAIHAKKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *
      *    更新前チェック
           EVALUATE    SPA-1009-KKCKSIKBN
      *        拡張構成 <フリー + 連番号 + フリー>
               WHEN    "1"
                   PERFORM 4001-PTNUM1-SEC
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4003-PTNUM3-SEC
                   END-IF
      *        拡張構成 <フリー + 連番号>
               WHEN    "2"
                   PERFORM 4001-PTNUM1-SEC
      *        拡張構成 <連番号 + フリー>
               WHEN    "3"
                   PERFORM 4002-PTNUM2-SEC
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4901-KAKUTEI-HEN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        戻る
               PERFORM 210-BACK
           END-IF
      *
          .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号編集処理
      *****************************************************************
       4901-KAKUTEI-HEN-SEC           SECTION.
      *
      *    患者番号編集
           MOVE    SPACE               TO  SPA-GMN014-PTNUM
           STRING  SPA-GMN014-PTNUM1   DELIMITED  BY  " "
                   SPA-GMN014-PTNUM2   DELIMITED  BY  " "
                   SPA-GMN014-PTNUM3   DELIMITED  BY  " "
                   INTO                SPA-GMN014-PTNUM
           END-STRING
      *
      *    患者番号変換(管理）を検索
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    SPA-GMN014-PTNUM    TO  SPTID-PTNUM
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
      *        既に患者番号が登録されている場合
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    05                  TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-GMN014-PTNUM
           END-IF
      *
          .
       4901-KAKUTEI-HEN-EXT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P014"              TO  MCP-WINDOW.
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
                   MOVE    "桁数と違います"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "検証番号が取得できませんでした。"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "１桁目に ＊ は設定できません。"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "既に登録済の患者番号です"  TO  SPA-ERRMSG
      *H30.10
               WHEN    "1013"
                   MOVE    "患者番号に使用できない文字があります。"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P014"              TO  SPA-MOTOPG
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
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *
           MOVE    "P014"              TO  SPA-MOTOPG
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
