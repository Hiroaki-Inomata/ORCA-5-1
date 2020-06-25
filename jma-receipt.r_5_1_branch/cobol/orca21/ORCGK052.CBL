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
       PROGRAM-ID.                 ORCGK052.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為セット一覧表入力（Ｋ０５より）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/04/23    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/17  MONFUNC 対応
      *  03.04.00    NACL-多々納  07/03/13  経過措置費追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.07.00    NACL-多々納  12/01/06  クライアント印刷対応
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
      *    会計照会共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
      *    クライアント印刷有
      **** 03  FLG-CLIENT-OK       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-STR-SETCD       PIC X(06).
           03  WRK-END-SETCD       PIC X(06).
      *
      *
           03  WRK-KIDMSG              PIC X(70).
      *
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
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
      *    一覧印刷変換領域
           COPY    "CPORCHCSET.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *
       01  SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
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
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
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
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
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
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "K052"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
       300-SCREEN-SEC              SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
                   PERFORM 3001-KID1-SYORI-SEC
                   MOVE    SPACE               TO  SPA-KIDCD
               WHEN    OTHER
                   PERFORM 310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-K052-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面よりＯＫ処理
      *****************************************************************
       3001-KID1-SYORI-SEC          SECTION.
      *
            EVALUATE    SPA-KIDCD
      *         印刷確認ＯＫ
                WHEN    "0001"
                    IF      SPA-KID1-FLG        =   "OK"
      *                一覧表印刷へ
                        PERFORM 3001-PRINT-SYORI-SEC
                    ELSE
                        MOVE    1               TO  SPA-K052-CUR
                    END-IF
           END-EVALUATE
      *
           .
       3001-KID1-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-K052-AREA
      *
           MOVE    SPACE               TO  SPA-K052-INPUTCD1
           MOVE    ALL "9"             TO  SPA-K052-INPUTCD2
           MOVE    SPACE               TO  SPA-K052-KEIKAYMD
           MOVE    SPACE               TO  SPA-K052-NAI-KEIKAYMD
      *
           MOVE    1                   TO  SPA-K052-CUR
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  K052
           INITIALIZE                  K052
      *
      *    画面編集
           MOVE    SPA-K052-INPUTCD1    TO  K052-INPUTCD1
           MOVE    SPA-K052-INPUTCD2    TO  K052-INPUTCD2
           MOVE    SPA-K052-KEIKAYMD    TO  K052-KEIKAYMD
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
           IF      SPA-K052-CUR        =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "INPUTCD1"
                       MOVE    2               TO  SPA-K052-CUR
                   WHEN    "INPUTCD2"
                       MOVE    3               TO  SPA-K052-CUR
                   WHEN    "KEIKAYMD"
                       MOVE    9               TO  SPA-K052-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-K052-CUR
               WHEN    1
                   MOVE    "INPUTCD1"          TO  MCP-WIDGET
               WHEN    2
                   MOVE    "INPUTCD2"          TO  MCP-WIDGET
               WHEN    3
                   MOVE    "KEIKAYMD"          TO  MCP-WIDGET
               WHEN    9
                   MOVE    "B09"               TO  MCP-WIDGET
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
                   PERFORM 310-SPA-SET-SEC
      *        印刷
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 300-PRINT-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-K052-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
           MOVE    K052-INPUTCD1       TO  SPA-K052-INPUTCD1
           MOVE    K052-INPUTCD2       TO  SPA-K052-INPUTCD2
           MOVE    K052-KEIKAYMD       TO  SPA-K052-KEIKAYMD
      *
      *    セットコード入力１
           PERFORM 2001-INPUTCD1-SEC
      *
      *    セットコード入力２
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2002-INPUTCD2-SEC
           END-IF
      *    経過措置日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2002-KEIKAYMD-SEC
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットコード入力処理
      *****************************************************************
       2001-INPUTCD1-SEC                SECTION.
      *
           IF     (SPA-K052-INPUTCD1       =   SPACE  )  OR
                  (SPA-K052-INPUTCD1(1:1)  =   "S"    OR
                                               "P"    )
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K052-CUR
           END-IF
      *
           .
       2001-INPUTCD1-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットコード入力処理
      *****************************************************************
       2002-INPUTCD2-SEC                SECTION.
      *
           IF      SPA-K052-INPUTCD2   =   SPACE
               MOVE    ALL "9"             TO  SPA-K052-INPUTCD2
           END-IF
           IF      SPA-K052-INPUTCD2(1:1)  =   "9"    OR
                                               "S"    OR
                                               "P"
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K052-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K052-INPUTCD2   NOT =   ALL "9"
                   IF      SPA-K052-INPUTCD1   >  SPA-K052-INPUTCD2
                       MOVE    "0002"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-K052-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       2002-INPUTCD2-EXT.
           EXIT.
      *
      *****************************************************************
      *    経過措置処理
      *****************************************************************
       2002-KEIKAYMD-SEC                   SECTION.
      *
           IF      SPA-K052-KEIKAYMD   =   SPACE
               MOVE    SPACE               TO  SPA-K052-NAI-KEIKAYMD
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-K052-KEIKAYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-K052-KEIKAYMD
                   MOVE    SGDAY-SDAY          TO  SPA-K052-NAI-KEIKAYMD
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-K052-CUR
               END-IF
           END-IF
      *
           .
       2002-KEIKAYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷　処理
      *****************************************************************
       300-PRINT-SEC                   SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      300-PRINT-EXT
           END-IF
      *
      *    対象件数チェック
           IF      SPA-K052-INPUTCD1   =   SPACE
               MOVE    "P     "            TO  WRK-STR-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD1   TO  WRK-STR-SETCD
           END-IF
           IF      SPA-K052-INPUTCD2   =   ALL "9"
               MOVE    "SXXXXX"            TO  WRK-END-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD2   TO  WRK-END-SETCD
           END-IF
      *
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-STR-SETCD       TO  ISET-SETCD
           MOVE    WRK-END-SETCD       TO  ISET-UPHMS
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-INPUTSET        =   1
      *        対象なし
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
      *        一覧表印刷へ
               MOVE    "0001"              TO  SPA-KIDCD
           END-IF
      *
           MOVE    1                   TO  SPA-K052-CUR
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表印刷処理
      *****************************************************************
       3001-PRINT-SYORI-SEC            SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        印刷制御情報取得サブ
      ***      CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *        クライアント印刷制御情報取得処理
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    対象件数チェック
           IF      SPA-K052-INPUTCD1   =   SPACE
               MOVE    "P     "            TO  WRK-STR-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD1   TO  WRK-STR-SETCD
           END-IF
           IF      SPA-K052-INPUTCD2   =   ALL "9"
               MOVE    "SXXXXX"            TO  WRK-END-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD2   TO  WRK-END-SETCD
           END-IF
      *
           INITIALIZE                  ORCHCSETAREA
           MOVE    WRK-STR-SETCD       TO  HCSET-STR-SETCD
           MOVE    WRK-END-SETCD       TO  HCSET-END-SETCD
           MOVE    SPA-K052-NAI-KEIKAYMD   TO  HCSET-KEIKAYMD
      *
           MOVE    "01"                TO  SPA-PRT-FLG
      *    再印刷なし
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
           CALL    "ORCHCSET"          USING   SPA-AREA
                                               ORCHCSETAREA
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    SPACE               TO  SPA-PRT-FLG
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           .
       3001-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      ***  INITIALIZE                  KDMY01
      **** MOVE    SPACE               TO  KDMY01-PANDAPRINT1
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      *****MOVE    "セット一覧表"      TO  ORCSPRTCTRL-TITLE-I(01)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *****MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
      **** MOVE    1                   TO  FLG-CLIENT-OK
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷画面処理
      *****************************************************************
      *600-CLIENT-DMY-SEC              SECTION.
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *
      *    MOVE    "_KDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *    .
      *600-CLIENT-DMY-EXT.
      *    EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "K05"               TO  SPA-SAKIPG
           MOVE    "K052"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "K05"               TO  MCP-WINDOW
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
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K052"              TO  MCP-WINDOW.
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
                   MOVE    "セットコードを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "開始コード＞終了コードです。"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "セットコードの対象データがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "経過措置日暦日エラー"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K052"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0001"
                   MOVE    "印刷を開始します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K052"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       900-INPUTSET-READ-SEC         SECTION.
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
       900-INPUTSET-READ-EXT.
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
