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
       PROGRAM-ID.                               ORCGZ90.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 老人一般コード変換設定画面（Ｚ９０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-藤原     01/09/26  JOINの修正      
      *  02.00.00    NACL-多々納  03/06/16  一括入力へ変更
      *  02.00.01    NACL-多々納  05/12/12  MONFUNC対応 他
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
           COPY    "Z03SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
      *
           03  FLG-SRYCDCHG                      PIC 9(01).
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-KANJI                         PIC 9(01).
           03  FLG-KANA                          PIC 9(01).
           03  FLG-SUJI                          PIC 9(01).
      *
           03  FLG-TOUROKU                       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-Z90-AREA.
               05  WRK-Z90GMN-TBL.
                   07  WRK-Z90GMN-TBL-G           OCCURS   5.
                       09  WRK-Z90GMN-SRYCDCHG        PIC X(09).
                       09  WRK-Z90GMN-SRYNAME         PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z03
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
           MOVE    SPA-Z03                       TO  SPA-Z01SONOTA 
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
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
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
           MOVE   "Z90"                TO  MCP-WINDOW.
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
       300-SCREEN-SEC             SECTION.
      *
      *
           EVALUATE    SPA-MOTOPG(1:3)
               WHEN    "Z98"
                   PERFORM 320-Z98SPASET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-Z90GMN-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通ＳＰＡ情報をＳＰＡにセット
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
        310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98SPASET-SEC          SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
               MOVE    SPA-Z98-SRYCD       TO  Z90-SRYCDCHG
                                                        (SPA-Z90-IDX)
               MOVE    SPA-Z98-SRYCD       TO  SPA-Z90GMN-SRYCDCHG
                                                        (SPA-Z90-IDX)
               PERFORM 2001-SRYCD-CHK-SEC
           END-IF
           .
       320-Z98SPASET-EXT.
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
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-KAKUTEI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-Z90GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
      *
       200-ENTRY-EXT.
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
      *    IF      FLG-TOUROKU         =   1
      *        PERFORM 4109-TOUROKU-CHK-SEC
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      410-INPUT-CHK-EXT
      *        END-IF
      *    END-IF
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    Z90-SRYCDCHG (IDX)  TO
                                     SPA-Z90GMN-SRYCDCHG (IDX)
           END-PERFORM
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
      *    入力コードチェック
           PERFORM 2001-SRYCD-CHK-SEC
           IF     (FLG-END             =   1 )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-SRYCD-CHK-SEC               SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END         =   1     )
               MOVE    IDX                 TO  SPA-Z90-IDX
      *        一覧表へ
               IF      SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)(1:2)   =   "//"
                   PERFORM 20011-Z98-SYORI-SEC
               ELSE
                   PERFORM 20012-SRYCD-TNSCHK-SEC
               END-IF
           END-PERFORM
      *
           IF     (FLG-END             =   1 )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      2001-SRYCD-CHK-EXT
           END-IF
      *
      *    空白行削除
           PERFORM 2002-SPACE-DEL-SEC
      *
           .
       2001-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ存在チェック処理
      *****************************************************************
       20012-SRYCD-TNSCHK-SEC           SECTION.
      *
      *    点数マスタ存在チェック
           IF      SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)     =   SPACE
               MOVE    SPACE       TO  SPA-Z90GMN-SRYNAME (SPA-Z90-IDX)
               GO      TO      20012-SRYCD-TNSCHK-EXT
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)
                                           TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  SPA-Z90GMN-SRYNAME
                                                       (SPA-Z90-IDX)
      *        病院診療所区分が同じであること
               IF      TNS-HOSPSRYKBN      =   SPA-GMN-HOSPSRYKBN
                   CONTINUE
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
               END-IF
      *        老人区分が同じ時、エラー
               IF      TNS-ROUTEKKBN      =   SPA-GMN-ROUTEKKBN
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-Z90GMN-SRYNAME
                                                       (SPA-Z90-IDX)
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPA-Z90-IDX         TO  SPA-Z90GMN-CUR
               GO      TO      20012-SRYCD-TNSCHK-EXT
           END-IF
      *
      *    ダブりチェック
           IF      SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)
                                           =   SPA-Z01-SRYCD
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY 1
                   UNTIL      (IDY             >   5   )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (IDY             NOT =   SPA-Z90-IDX )  AND
                      (SPA-Z90GMN-SRYCDCHG(IDY)
                                           =   SPA-Z90GMN-SRYCDCHG
                                                         (SPA-Z90-IDX))
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPA-Z90-IDX         TO  SPA-Z90GMN-CUR
               GO      TO      20012-SRYCD-TNSCHK-EXT
           END-IF
      *
           .
       20012-SRYCD-TNSCHK-EXT.
           EXIT.
      *****************************************************************
      *    空白行削除処理
      *****************************************************************
       2002-SPACE-DEL-SEC           SECTION.
      *
           MOVE    SPA-Z90GMN-TBL      TO  WRK-Z90GMN-TBL
           INITIALIZE                      SPA-Z90GMN-TBL
           MOVE    ZERO                TO  SPA-Z90-IDX
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   5
               IF      WRK-Z90GMN-TBL-G (IDX)  NOT =   SPACE
                   ADD     1               TO  SPA-Z90-IDX
                   MOVE    WRK-Z90GMN-TBL-G (IDX)
                                   TO  SPA-Z90GMN-TBL-G (SPA-Z90-IDX)
               END-IF
           END-PERFORM
      *
           .
       2002-SPACE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    入力コードチェック処理
      *****************************************************************
       20011-Z98-SYORI-SEC            SECTION.
      *
      *
           MOVE    "Z90"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z90"               TO  SPA-Z98-MOTOPG
      *
           MOVE    "INPUT"             TO  MCP-WIDGET 
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-Z98-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           INITIALIZE                  SPA-Z98GMN-AREA
           MOVE    SPACE               TO  Z98
      *
           INITIALIZE                      Z98
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "Z98    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       20011-Z98-SYORI-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z90"               TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  Z90
      *
           INITIALIZE                      Z90
           MOVE    SPA-Z01-SRYCD       TO  Z90-SRYCD
           MOVE    SPA-Z01-NAMEH       TO  Z90-SRYNAME
      *
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  5
               MOVE    SPA-Z90GMN-SRYCDCHG (IDX)
                                       TO  Z90-SRYCDCHG (IDX)
               MOVE    SPA-Z90GMN-SRYNAME  (IDX)
                                       TO  Z90-SRYCDNAME(IDX)
           END-PERFORM
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
           IF      SPA-Z90GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-Z90GMN-CUR
               WHEN    01
                   MOVE    "SRYCDCHG1"     TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SRYCDCHG2"     TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYCDCHG3"     TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SRYCDCHG4"     TO  MCP-WIDGET
               WHEN    05
                   MOVE    "SRYCDCHG5"     TO  MCP-WIDGET
               WHEN    06
                   MOVE    "B12"           TO  MCP-WIDGET
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
      *    最後の行へ
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  5
               IF      SPA-Z90GMN-SRYCDCHG (IDX)   =   SPACE
                   MOVE    IDX             TO  SPA-Z90GMN-CUR
                   MOVE    5               TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-Z90GMN-CUR      =   ZERO
               MOVE    6                   TO  SPA-Z90GMN-CUR
           END-IF
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
                   MOVE    "点数マスタに存在しません"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "診療コードが重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "処理中のコードです"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "病院診療所区分が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "老人適用区分が同じです"
                                                 TO  SPA-ERRMSG
      *R01.10
               WHEN    "0006"
                   MOVE    "更新はできません。"
                                                 TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
      *
           MOVE    "Z90"               TO  SPA-MOTOPG
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
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Z03"                 TO  SPA-SAKIPG.
      *
           MOVE    "Z90"                 TO  SPA-MOTOPG.
      *
      ***  MOVE    "BACK"                TO  MCP-PUTTYPE.
           MOVE    "JOIN"                TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認処理
      *****************************************************************
       412-KAKUTEI-SEC                           SECTION.
      *
      *R1.10
      *    更新不可とする（現在対象なし）
           MOVE    "0006"              TO  SPA-ERRCD
           GO      TO      412-KAKUTEI-EXT
      *
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-KAKUTEI-EXT
           END-IF
      *
      *    入力内容をＰＲＧ共通ＳＰＡにセット
           PERFORM  VARYING  IDX       FROM  1  BY  1
                    UNTIL    IDX       >  5
               MOVE    SPA-Z90GMN-SRYCDCHG (IDX)
                                 TO  SPA-Z90-SRYCDCHG (IDX)
               MOVE    SPA-Z90GMN-SRYNAME  (IDX)
                                 TO  SPA-Z90-SRYNAME (IDX)
           END-PERFORM
      *
      *    前画面に戻る
           PERFORM  210-BACK
      *
           .
       412-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-Z01-YUKOSTYMD   TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-YUKOSTYMD   TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *
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
