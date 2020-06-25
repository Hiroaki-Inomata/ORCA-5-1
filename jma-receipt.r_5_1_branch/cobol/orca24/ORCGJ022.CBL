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
       PROGRAM-ID.                 ORCGJ022.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計カード
      *  コンポーネント名  : 回数入力（Ｊ０２２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/30  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  04.05.00    NACL-多々納  09/06/29  一括入力対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.08.00    NACL-多々納  13/01/15  診療回数テーブル１０対応
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
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J02SCR-SPA".
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZ2                PIC 9(04).
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
           03  WRK-ENDDD           PIC 9(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
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
       01  SCRAREA.
           COPY    "ORCA24SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
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
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J02         TO  SPA-FREE
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
      *    エラー画面より
           IF      SPA-MOTOPG          =   "JERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "J022"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
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
      *
           PERFORM                     310-SPA-SET-SEC
      *
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    SPA-J022-MAX        TO  IDY
           INITIALIZE                  SPA-J022-AREA
      *
           MOVE    IDY                 TO  SPA-J022-MAX
      *
           MOVE    SPA-DAY-IDX         TO  SPA-J022-IDX
           MOVE    1                   TO  SPA-J022NAI-DAYCHK
                                                        (SPA-DAY-IDX)
     **    MOVE    SPA-GNAI-DAY (2 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY1 (SPA-DAY-IDX)
      *    MOVE    SPA-GNAI-DAY (3 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY2 (SPA-DAY-IDX)
      *    MOVE    SPA-GNAI-DAY (4 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY3 (SPA-DAY-IDX)
      *
      *    MOVE    SPA-GNAI-DAY (2 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY1
      *    MOVE    SPA-GNAI-DAY (3 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY2
      *    MOVE    SPA-GNAI-DAY (4 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY3
      *    IF     (SPA-J022-DAY1   >   ZERO )  AND
      *           (SPA-J022-MAX    <   1    )
      *        MOVE    1               TO  SPA-J022-MAX
      *    END-IF
      *    IF     (SPA-J022-DAY2   >   ZERO )  AND
      *           (SPA-J022-MAX    <   2    )
      *        MOVE    2               TO  SPA-J022-MAX
      *    END-IF
      *    IF     (SPA-J022-DAY3   >   ZERO )  AND
      *           (SPA-J022-MAX    <   3    )
      *        MOVE    3               TO  SPA-J022-MAX
      **** END-IF
      *ver.4.8
      *    テーブル９回対応
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING    IDX      FROM    2   BY  1
                   UNTIL      IDX      >   10
               ADD     1               TO  IDZ
               MOVE    SPA-GNAI-DAY (IDX SPA-DAY-IDX)
                                       TO  SPA-J022NAI-DAY
                                                   (SPA-DAY-IDX IDZ)
               MOVE    SPA-GNAI-DAY (IDX SPA-J022-IDX) 
                                       TO  SPA-J022-DAY (IDZ)
               IF      SPA-J022-DAY (IDZ)
                                       >   ZERO
                   MOVE    IDZ             TO  SPA-J022-MAX
               END-IF
           END-PERFORM
      *
      *    日付変換
           MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    SPA-J022-IDX        TO  WRK-SDD
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE  LNK-DAY2-EDTYMD3      TO  SPA-HENNYMD
           END-IF
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
           MOVE    SPACE               TO  J022
           INITIALIZE                      J022
      *
      *    画面編集
           MOVE    SPA-HENNYMD(11:)    TO  J022-SRYYMD
      *
           PERFORM VARYING    IDZ      FROM    1   BY  1
                   UNTIL      IDZ      >   9
               MOVE    SPA-J022-DAY (IDZ)  TO  J022-DAY (IDZ)
               MOVE    WIDGET-NORMAL       TO  J022-DAY-STATE (IDZ)
           END-PERFORM
      *    MOVE    SPA-J022-DAY2       TO  J022-DAY2
      *    MOVE    SPA-J022-DAY3       TO  J022-DAY3
      *
      *    MOVE    WIDGET-NORMAL       TO  J022-DAY1-STATE
      *    MOVE    WIDGET-NORMAL       TO  J022-DAY2-STATE
      *    MOVE    WIDGET-NORMAL       TO  J022-DAY3-STATE
           IF      SPA-NYUGAIKBN       =   "1"
      ******   MOVE    WIDGET-INSENSITIVE  TO  J022-DAY3-STATE
               MOVE    "（退院分）"        TO  J022-MSG1
               MOVE    "（入院分）"        TO  J022-MSG2
               PERFORM VARYING    IDZ      FROM    3   BY  1
                       UNTIL      IDZ      >   9
                   MOVE    WIDGET-INSENSITIVE  TO  J022-DAY-STATE (IDZ)
               END-PERFORM
           ELSE
      **       IF      SPA-J022-MAX        =   2
      *            MOVE    WIDGET-INSENSITIVE  TO  J022-DAY3-STATE
      ***      END-IF
               COMPUTE IDZ2                =   SPA-J022-MAX  +  1
               PERFORM VARYING    IDZ      FROM    IDZ2   BY  1
                       UNTIL      IDZ      >   9
                   MOVE    WIDGET-INSENSITIVE  TO  J022-DAY-STATE (IDZ)
               END-PERFORM
           END-IF
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
           IF      SPA-J022-CUR        =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "DAY1"
                       MOVE    2                   TO  SPA-J022-CUR
                   WHEN    "DAY2"
                       MOVE    3                   TO  SPA-J022-CUR
                       IF     (SPA-NYUGAIKBN       =   "1"  )
                       OR     (SPA-J022-MAX        =   2    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY3"
                       MOVE    4                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   3    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY4"
                       MOVE    5                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   4    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY5"
                       MOVE    6                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   5    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY6"
                       MOVE    7                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   6    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY7"
                       MOVE    8                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   7    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY8"
                       MOVE    9                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   8    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY9"
                       MOVE    10                  TO  SPA-J022-CUR
                   WHEN    OTHER
                       MOVE    1                   TO  SPA-J022-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-J022-CUR
               WHEN    1
                   MOVE    "DAY1"              TO  MCP-WIDGET
               WHEN    2
                   MOVE    "DAY2"              TO  MCP-WIDGET
               WHEN    3
                   MOVE    "DAY3"              TO  MCP-WIDGET
               WHEN    4
                   MOVE    "DAY4"              TO  MCP-WIDGET
               WHEN    5
                   MOVE    "DAY5"              TO  MCP-WIDGET
               WHEN    6
                   MOVE    "DAY6"              TO  MCP-WIDGET
               WHEN    7
                   MOVE    "DAY7"              TO  MCP-WIDGET
               WHEN    8
                   MOVE    "DAY8"              TO  MCP-WIDGET
               WHEN    9
                   MOVE    "DAY9"              TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B12"               TO  MCP-WIDGET
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
                   PERFORM 230-MODORU
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-KAKUTEI-SEC
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
           MOVE    ZERO                TO  SPA-J022-CUR
      *
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
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      ***  MOVE    J022-DAY1           TO  SPA-J022-DAY1
      *    MOVE    J022-DAY2           TO  SPA-J022-DAY2
      ***  MOVE    J022-DAY3           TO  SPA-J022-DAY3
           PERFORM VARYING    IDZ      FROM    1   BY  1
                   UNTIL      IDZ      >   9
               MOVE    J022-DAY (IDZ)      TO  SPA-J022-DAY (IDZ)
           END-PERFORM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC                   SECTION.
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       220-KAKUTEI-SEC                   SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
      ***  MOVE    SPA-J022-DAY1       TO  SPA-NAI-DAY1 (SPA-J022-IDX)
      *    MOVE    SPA-J022-DAY2       TO  SPA-NAI-DAY2 (SPA-J022-IDX)
      **** MOVE    SPA-J022-DAY3       TO  SPA-NAI-DAY3 (SPA-J022-IDX)
           PERFORM VARYING    IDZ      FROM    1   BY  1
                   UNTIL      IDZ      >   9
               MOVE    SPA-J022-DAY (IDZ)  TO  SPA-J022NAI-DAY
                                                     (SPA-J022-IDX IDZ)
           END-PERFORM
      *
           COMPUTE IDY                 =   SPA-J022-IDX  +  1
           MOVE    ZERO                TO  SPA-J022-IDX
           PERFORM VARYING     IDX     FROM    IDY BY 1
                   UNTIL       IDX     >   31
               IF      SPA-J022NAI-DAYCHK (IDX)    =   1
                   MOVE    IDX             TO  SPA-J022-IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-J022-IDX        =   ZERO
               PERFORM 210-BACK
           ELSE
      ***      MOVE    SPA-GNAI-DAY (2 SPA-J022-IDX)
      *                                TO  SPA-J022-DAY1
      *        MOVE    SPA-GNAI-DAY (3 SPA-J022-IDX)
      *                                TO  SPA-J022-DAY2
      *        MOVE    SPA-GNAI-DAY (4 SPA-J022-IDX)
      ****                             TO  SPA-J022-DAY3
               PERFORM VARYING    IDZ      FROM    1   BY  1
                       UNTIL      IDZ      >   9
                   COMPUTE IDX             =   IDZ     +   1
                   MOVE    SPA-GNAI-DAY (IDX SPA-J022-IDX)
                                           TO  SPA-J022-DAY (IDZ)
               END-PERFORM
      *
      *        日付変換
               MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
               MOVE    SPA-J022-IDX        TO  WRK-SDD
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                                  LNK-DAY2-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE  LNK-DAY2-EDTYMD3      TO  SPA-HENNYMD
               END-IF
           END-IF
           .
       220-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       230-MODORU                  SECTION.
      *
      *    更新を取消して戻る
      *    PERFORM VARYING     IDX     FROM    IDY BY 1
      *            UNTIL       IDX     >   31
      *        MOVE    ZERO            TO  SPA-NAI-DAYCHK (IDX)
      *    END-PERFORM
      *
           PERFORM 210-BACK
           .
       230-MODORU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J022"              TO  SPA-MOTOPG
      *
      *****MOVE   "BACK"               TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   "J02"                TO  MCP-WINDOW
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J022"              TO  MCP-WINDOW.
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
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J022"              TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
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
      *
