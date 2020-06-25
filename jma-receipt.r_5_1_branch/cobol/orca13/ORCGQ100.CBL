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
       PROGRAM-ID.                 ORCGQ100.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 出力指示画面（Ｑ１００）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/06/11    MCC−太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03/06/10  一括入力対応
      *  03.05.00    NACL-太田    07.05.23  グループ診療対応
      *  04.01.01    NACL-藤原    07/10/01  クライアント保存対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "Q01SCR-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM                             PIC 9(09).
           03  WRK-MCP-WIDGET                      PIC X(64).
      *
      *    固定値領域
      *01  CONST-AREA.
      *
           03  CONST-JOBID         PIC 9(07)   VALUE   1.
           03  CONST-JOBID2        PIC 9(07)   VALUE   2.
           03  CONST-JOB-SHELLID   PIC X(08)   VALUE   "ORCBQ01".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
           COPY    "ORCA13SCRAREA.INC".
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
      *----(01.00.02) LINE ADD START ----------------------------------
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
      *----(01.00.02) LINE ADD END   ----------------------------------
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
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
           IF  ( FLG-END           =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
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
                                       IDX-AREA
                                       WRK-AREA
      *
      *    エラー画面より
           IF  ( SPA-MOTOPG            =   "QERR" )
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
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q100"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-Q100
           INITIALIZE                      SPA-Q100
      *
           MOVE  4                     TO  SPA-GMN-Q100-CUR
      *
           IF  ( SPA-Q02-Q100-PROC-FLG     =   ZERO )
      *        ＣＳＶ出力処理
      *
      *        クライアント保存可否情報取得サブ
               IF      SPA-Q01-CLIENT-SAVE
                                       =   "1"
                   MOVE    "1"             TO  SPA-GMN-Q100-FILEL   (1)
                   MOVE    "クライアント保存"
                                           TO  SPA-GMN-Q100-FILEMEIL(1)
                   MOVE    1               TO  SPA-GMN-Q100-FILE-MAX
                   MOVE    WIDGET-NORMAL   TO  SPA-GMN-Q100-FILE-STATE
      *
                   MOVE    "1"             TO  SPA-GMN-Q100-CDKBNL   (1)
                   MOVE    "ＥＵＣ"        TO  SPA-GMN-Q100-CDKBNMEIL(1)
                   MOVE    "2"             TO  SPA-GMN-Q100-CDKBNL   (2)
                   MOVE    "Ｓ−ｊｉｓ"    TO  SPA-GMN-Q100-CDKBNMEIL(2)
                   MOVE    "3"             TO  SPA-GMN-Q100-CDKBNL   (3)
                   MOVE    "ＵＴＦ−８"    TO  SPA-GMN-Q100-CDKBNMEIL(3)
                   MOVE    3               TO  SPA-GMN-Q100-CDKBN-MAX
                   MOVE    WIDGET-NORMAL   TO  SPA-GMN-Q100-CDKBN-STATE
      *
                   IF    ( SPA-MW          =   SPA-GINBEE )
                       MOVE    SPA-GMN-Q100-FILELST (1)
                                           TO  SPA-GMN-Q100-FILE-G
                       MOVE    SPA-GMN-Q100-CDKBNLST (1)
                                           TO  SPA-GMN-Q100-CDKBN-G
                   END-IF
      *
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  SPA-GMN-Q100-FILE-STATE
                                               SPA-GMN-Q100-CDKBN-STATE
               END-IF
      *
               MOVE    "3001"          TO  SPA-GMN-Q100-CD
               MOVE    "ＣＳＶ出力を開始します"
                                       TO  SPA-GMN-Q100-MSG
               MOVE    "出力ファイル名"
                                       TO  SPA-GMN-Q100-LBLFILENAME
               IF    ( SPA-MW          =   SPA-GINBEE )
                   MOVE    "OUTPUT.csv"
                                       TO  SPA-GMN-Q100-FILENAME
               ELSE
                   MOVE    "/tmp/OUTPUT.csv"
                                       TO  SPA-GMN-Q100-FILENAME
               END-IF
               MOVE    WIDGET-NORMAL   TO  SPA-GMN-Q100-FILENAME-STATE
               MOVE    "件数指定"      TO  SPA-GMN-Q100-LBLNUMSEL
           ELSE
               MOVE    "3002"          TO  SPA-GMN-Q100-CD
               MOVE    "印刷処理を開始します"
                                       TO  SPA-GMN-Q100-MSG
               MOVE    SPACE           TO  SPA-GMN-Q100-LBLFILENAME
                                           SPA-GMN-Q100-FILENAME
               MOVE    WIDGET-INSENSITIVE
                                       TO  SPA-GMN-Q100-FILENAME-STATE
               MOVE    "ページ指定"    TO  SPA-GMN-Q100-LBLNUMSEL
               MOVE    WIDGET-INSENSITIVE
                                       TO  SPA-GMN-Q100-FILE-STATE
                                           SPA-GMN-Q100-CDKBN-STATE
           END-IF
      *
           MOVE    SPA-Q02-Q100-STNUM      TO  SPA-GMN-Q100-STNUM
           MOVE    SPA-Q02-Q100-EDNUM      TO  SPA-GMN-Q100-EDNUM
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET          TO        WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *     確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO        WRK-MCP-WIDGET
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-Q100-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ファイル名チェック
           PERFORM 4100-FILENAME-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    開始件数（ページ数）チェック
           PERFORM 4100-STNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    終了件数（ページ数）チェック
           PERFORM 4100-EDNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    出力先チェック
           PERFORM 4100-FILE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    文字コードチェック
           PERFORM 4100-CDKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    ファイル名
           MOVE    Q100-FILENAME   TO  SPA-GMN-Q100-FILENAME
      *
      *    開始件数（ページ数）
           MOVE    Q100-STNUM      TO  SPA-GMN-Q100-STNUM
      *
      *    終了件数（ページ数）
           MOVE    Q100-EDNUM      TO  SPA-GMN-Q100-EDNUM
      *
      *    出力先
           MOVE    Q100-FILE       TO  SPA-GMN-Q100-FILE-G
      *
      *    文字コード
           MOVE    Q100-CDKBN      TO  SPA-GMN-Q100-CDKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ファイル名チェック
      *****************************************************************
       4100-FILENAME-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-Q100-FILENAME   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       100
                       OR      SPA-GMN-Q100-FILENAME (IDX2:1)
                                           =   SPACE
      *
                   IF    ( SPA-MW      =   SPA-GINBEE )
                       IF        SPA-GMN-Q100-FILENAME (IDX2:1)
                                               =   "\" OR  ":"  OR "/"
                           MOVE    "1006"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-GMN-Q100-CUR
                           MOVE    100             TO  IDX2
                       END-IF
                   ELSE
                       IF        SPA-GMN-Q100-FILENAME (IDX2:1)
                                               =   "\" OR  ":" 
                           MOVE    "1004"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-GMN-Q100-CUR
                           MOVE    100             TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       4100-FILENAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始件数（ページ数）チェック
      *****************************************************************
       4100-STNUM-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-Q100-STNUM
                                   TO  WRK-NUM
      *
      *    件数（ページ数）範囲チェック
           PERFORM 4100-NUM-HANI-CHK-SEC
      *
           MOVE    WRK-NUM         TO  SPA-GMN-Q100-STNUM
      *
           .
       4100-STNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了件数（ページ数）チェック
      *****************************************************************
       4100-EDNUM-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-Q100-EDNUM
                                   TO  WRK-NUM
      *
      *    件数（ページ数）範囲チェック
           PERFORM 4100-NUM-HANI-CHK-SEC
      *
           MOVE    WRK-NUM         TO  SPA-GMN-Q100-EDNUM
      *
           .
       4100-STNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    件数（ページ数）範囲チェック
      *****************************************************************
       4100-NUM-HANI-CHK-SEC           SECTION.
      *
           IF    ( WRK-NUM         <   1 )
               MOVE    1           TO  WRK-NUM
           END-IF
      *
           IF    ( WRK-NUM         >   SPA-Q02-Q100-EDNUM )
               MOVE    SPA-Q02-Q100-EDNUM
                                   TO  WRK-NUM
           END-IF
      *
           .
       4100-NUM-HANI-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先チェック処理
      *****************************************************************
       4100-FILE-CHK-SEC           SECTION.
      *
           IF      SPA-Q01-CLIENT-SAVE    =   "1"
      *
               IF    ( SPA-MW             =   SPA-GINBEE )
                   MOVE    SPA-GMN-Q100-FILELST (1)
                                          TO  SPA-GMN-Q100-FILE-G
               END-IF
      *
               IF      SPA-GMN-Q100-FILE       =   SPACE
                   MOVE    SPACE               TO  SPA-GMN-Q100-CDKBN-G
               ELSE
                   PERFORM VARYING IDX2 FROM    1   BY  1
                           UNTIL   IDX2 >       SPA-GMN-Q100-FILE-MAX
                       IF        SPA-GMN-Q100-FILE
                                       =   SPA-GMN-Q100-FILEL (IDX2)
                           MOVE    SPA-GMN-Q100-FILEMEIL (IDX2)
                                               TO  SPA-GMN-Q100-FILEMEI
                           MOVE    SPA-GMN-Q100-FILE-MAX
                                               TO  IDX2
                       END-IF
                   END-PERFORM        
                   IF      SPA-GMN-Q100-FILEMEI  =   SPACE  
                       MOVE    SPA-GMN-Q100-FILELST (1)
                                               TO  SPA-GMN-Q100-FILE-G
                   END-IF
               END-IF
           END-IF
           .
       4100-FILE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字コードチェック処理
      *****************************************************************
       4100-CDKBN-CHK-SEC           SECTION.
      *
           IF      SPA-Q01-CLIENT-SAVE    =   "1"
               IF      SPA-GMN-Q100-CDKBN      =   SPACE
                   IF      SPA-GMN-Q100-FILE       =   "1"
                       MOVE    SPA-GMN-Q100-CDKBNLST (1)
                                               TO  SPA-GMN-Q100-CDKBN-G
                   END-IF
               ELSE
                   PERFORM VARYING IDX2    FROM   1   BY  1
                           UNTIL   IDX2    >      SPA-GMN-Q100-CDKBN-MAX
                       IF        SPA-GMN-Q100-CDKBN
                                       =   SPA-GMN-Q100-CDKBNL (IDX2)
                           MOVE    SPA-GMN-Q100-CDKBNMEIL (IDX2)
                                               TO  SPA-GMN-Q100-CDKBNMEI
                           MOVE    SPA-GMN-Q100-CDKBN-MAX
                                               TO  IDX2
                       END-IF
                   END-PERFORM        
                   IF      SPA-GMN-Q100-CDKBNMEI   =   SPACE  
                       MOVE    SPA-GMN-Q100-CDKBNLST (1)
                                               TO  SPA-GMN-Q100-CDKBN-G
                   END-IF
               END-IF
           END-IF
           .
       4100-CDKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    ZERO                TO  SPA-Q02-Q100-RTOK-FLG
      *
           MOVE    SPA-Q01-SAKIPG      TO  SPA-SAKIPG
           MOVE    "Q100"              TO  SPA-MOTOPG
      *
      *****MOVE    "BACK"              TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
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
      *    確定　処理
      *****************************************************************
       490-KAKUTEI                 SECTION.
      *
      *    入力時チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    出力ファイル名未入力チェック（ＣＳＶ出力時）
           IF  ( SPA-Q02-Q100-PROC-FLG     =   ZERO )
               IF  ( SPA-GMN-Q100-FILENAME
                                       =   SPACE )
                   MOVE    "1001"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-Q100-CUR
                   GO  TO  490-KAKUTEI-EXT
               END-IF
      *
               PERFORM VARYING IDX0 FROM 100 BY -1 
                       UNTIL ( IDX0 <    1 )
                        OR   ( SPA-ERRCD NOT =   SPACE )
      *
                   IF    ( SPA-GMN-Q100-FILENAME (IDX0 : 1) =  "/" )
                       IF    ( SPA-ERRCD =  SPACE )
                           IF    ( IDX0      =  100 )
                            OR   ( SPA-GMN-Q100-FILENAME ( IDX0 + 1 : )
                                         = SPACE )
                               MOVE    "1005"      TO  SPA-ERRCD
                               MOVE    1           TO  SPA-GMN-Q100-CUR
                               GO  TO  490-KAKUTEI-EXT
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
      *    件数(ぺージ数）指定チェック
           IF  ( SPA-GMN-Q100-STNUM    >   SPA-GMN-Q100-EDNUM )
      *
               IF  ( SPA-Q02-Q100-PROC-FLG =   ZERO )
                   MOVE    "1002"      TO  SPA-ERRCD
               ELSE
                   MOVE    "1003"      TO  SPA-ERRCD
               END-IF
      *
               MOVE    2           TO  SPA-GMN-Q100-CUR
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    Ｑ０２に戻る
           MOVE    SPA-GMN-Q100-FILENAME
                                       TO  SPA-Q02-Q100-FILENAME
           IF      SPA-GMN-Q100-FILE   =   SPACE
               MOVE    "0"                 TO  SPA-Q02-Q100-FILE
           ELSE
               MOVE    SPA-GMN-Q100-FILE   TO  SPA-Q02-Q100-FILE
           END-IF
      *
           IF  ( SPA-Q02-Q100-PROC-FLG     =   1 )
      *        開始件数
               COMPUTE SPA-Q02-Q100-STKNS
                   = ( SPA-GMN-Q100-STNUM  -   1 )
                   *   SPA-Q02-Q100-LINE   +   1
      *        終了件数
               COMPUTE SPA-Q02-Q100-EDKNS
                   =   SPA-GMN-Q100-EDNUM
                   *   SPA-Q02-Q100-LINE
      *        開始ページ
               MOVE    SPA-GMN-Q100-STNUM
                                       TO  SPA-Q02-Q100-STPAGE
      *        終了ページ
               MOVE    SPA-GMN-Q100-EDNUM
                                       TO  SPA-Q02-Q100-EDPAGE
           ELSE
      *        開始件数
               MOVE    SPA-GMN-Q100-STNUM
                                       TO  SPA-Q02-Q100-STKNS
      *        終了件数
               MOVE    SPA-GMN-Q100-EDNUM
                                       TO  SPA-Q02-Q100-EDKNS
      *        文字コード区分
               IF      SPA-GMN-Q100-CDKBN  =   SPACE
                   MOVE    "1"                 TO  SPA-Q02-Q100-CDKBN
               ELSE
                   MOVE    SPA-GMN-Q100-CDKBN  TO  SPA-Q02-Q100-CDKBN
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-Q02-Q100-RTOK-FLG
      *
           MOVE    SPA-Q01-SAKIPG      TO  SPA-SAKIPG
           MOVE    "Q100"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q100"              TO  MCP-WINDOW.
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
      *    メッセージコード
           MOVE    SPA-GMN-Q100-CD     TO  Q100-ID1CODE
      *
      *    メッセージ
           MOVE    SPA-GMN-Q100-MSG    TO  Q100-ID1MSG
      *
      *    ファイル名（ラベル）
           MOVE    SPA-GMN-Q100-LBLFILENAME
                                       TO  Q100-LBLFILENAME
      *
      *    ファイル名
           MOVE    SPA-GMN-Q100-FILENAME
                                       TO  Q100-FILENAME
           MOVE    SPA-GMN-Q100-FILENAME-STATE
                                       TO  Q100-FILENAME-STATE
      *
      *    ファイル名（ラベル）
           MOVE    SPA-GMN-Q100-LBLNUMSEL
                                       TO  Q100-LBLNUMSEL
      *
      *    開始件数（ページ）数
           MOVE    SPA-GMN-Q100-STNUM
                                       TO  Q100-STNUM
      *
      *    終了件数（ページ）数
           MOVE    SPA-GMN-Q100-EDNUM
                                       TO  Q100-EDNUM
      *
      *    出力先
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-Q100-FILE-MAX
               MOVE    SPA-GMN-Q100-FILELST (IDX2)
                                           TO  Q100-FILE-ITEM (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-Q100-FILE-MAX   TO  Q100-FILE-COUNT
           MOVE    SPA-GMN-Q100-FILE-G     TO  Q100-FILE
           MOVE    SPA-GMN-Q100-FILE-STATE TO  Q100-FILE-STATE
      *
      *    文字コード
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-Q100-CDKBN-MAX
               MOVE    SPA-GMN-Q100-CDKBNLST (IDX2)
                                           TO  Q100-CDKBN-ITEM (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-Q100-CDKBN-MAX  TO  Q100-CDKBN-COUNT
           MOVE    SPA-GMN-Q100-CDKBN-G    TO  Q100-CDKBN
           MOVE    SPA-GMN-Q100-CDKBN-STATE
                                           TO  Q100-CDKBN-STATE
           IF    ( SPA-GMN-Q100-FILE       =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  Q100-CDKBN-STATE
           END-IF
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
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-Q100-CUR    =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-Q100-CUR
      *
      *    ファイル名
           WHEN   001
               MOVE  "FILENAME"    TO  MCP-WIDGET
      *
      *    開始件数（ページ）数
           WHEN   002
               MOVE  "STNUM"       TO  MCP-WIDGET
      *
      *    終了件数（ページ）数
           WHEN   003
               MOVE  "EDNUM"       TO  MCP-WIDGET
      *
      *    確定ボタン
           WHEN   004
               MOVE  "B12"         TO  MCP-WIDGET
      *
      *    出力先
           WHEN   005
               MOVE  "FILE"        TO  MCP-WIDGET
      *
      *    文字コード
           WHEN   006
               MOVE  "CDKBN"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
      *
      *    表題
           WHEN    "FILENAME"
               MOVE    001         TO  SPA-GMN-Q100-CUR
      *
      *    条件上段
           WHEN    "STNUM"
               MOVE    002         TO  SPA-GMN-Q100-CUR
      *
      *    条件下段
           WHEN    "EDNUM"
               MOVE    003         TO  SPA-GMN-Q100-CUR
      *
      *    出力先
           WHEN   "FILE"
               MOVE    005         TO  SPA-GMN-Q100-CUR
      *
      *    文字コード
           WHEN   "CDKBN"
               MOVE    006         TO  SPA-GMN-Q100-CUR
      *
           END-EVALUATE
      *
      *    次カーソル位置を設定
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "EDNUM"
               IF      SPA-Q01-CLIENT-SAVE    =   "1"
                   MOVE    005         TO  SPA-GMN-Q100-CUR
               ELSE
                   MOVE    004         TO  SPA-GMN-Q100-CUR
               END-IF
           WHEN    "CDKBN"
               MOVE    004         TO  SPA-GMN-Q100-CUR
           WHEN   "FILE"
               IF      SPA-GMN-Q100-FILE       =   "1"
                   MOVE    006         TO  SPA-GMN-Q100-CUR
               ELSE
                   MOVE    004         TO  SPA-GMN-Q100-CUR
               END-IF
           WHEN    OTHER
               COMPUTE SPA-GMN-Q100-CUR    =   SPA-GMN-Q100-CUR +   1
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "1001"
           WHEN    "1005"
               MOVE    "出力ファイル名が入力されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "開始件数＞終了件数となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "開始ページ＞終了ページとなっています"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               STRING   "出力ファイル名に" DELIMITED  BY  SIZE
                        "：￥が入力されています"
                                             DELIMITED  BY  SIZE
                                             INTO  SPA-ERRMSG
               END-STRING
           WHEN    "1006"
               STRING   "出力ファイル名に" DELIMITED  BY  SIZE
                        "：￥／が入力されています"
               DELIMITED  BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q100"               TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
