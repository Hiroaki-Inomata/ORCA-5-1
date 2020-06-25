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
       PROGRAM-ID.                 ORCGMDAS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : メニュー
      *  コンポーネント名  : 情報（ＭＤＡＳ）
      *  管理者            : 
      *  09/11/04    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
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
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    診療行為用共通パラメタ
           COPY    "M00COMMON-SPA".
      *    スパ領域
           COPY    "M01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-USER            PIC 9(01).
           03  FLG-JOBLOG          PIC 9(01).
           03  FLG-DASCONFIG       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PROFILE".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
       01  SHELL-AREA.
           05  SHELL-NAME          PIC X(14)   VALUE   "das-profile.sh".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブログ
       01  JOBLOG-REC.
           COPY    "CPJOBLOG.INC".
      *
      *    シェル起動
           COPY    "CPSHELLTBL.INC".
      *
      *    定点設定
       01  DASCF-REC.
           COPY    "CPDASCONF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      * 
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
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
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPACE           TO  SPA-ERRCD
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    "NEW"           TO  MCP-PUTTYPE
               MOVE    "MDAS"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
                   PERFORM 320-MID1-SET-SEC
               WHEN    "MID2"
                   PERFORM 320-MID2-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       320-MID1-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-MIDCD
           IF      SPA-MID1-FLG        =   "OK"
      *        申請データを出力する
               PERFORM 2120-DASCONFIG-WRITE-SEC
      *        シェルを起動する
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 212-SHELL-OUT-SEC
               END-IF
           END-IF
      *
           .
       320-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ２）ＯＫ処理
      *****************************************************************
       320-MID2-SET-SEC            SECTION.
      *
           IF      SPA-MIDCD       NOT =   "0000"
               GO  TO  320-MID2-SET-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
      *
           MOVE    "M98"           TO  SPA-SAKIPG
           MOVE    "MDAS"          TO  SPA-MOTOPG
      *
           MOVE    "M98"           TO  MCP-WINDOW
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       320-MID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2120-DASCONFIG-WRITE-SEC        SECTION.
      *
           IF      FLG-DASCONFIG       =   ZERO
               INITIALIZE                      DASCF-REC
               MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
               MOVE    DASCF-REC           TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    SPA-MUID-NAI-SINSEIYMD
                                       TO  DASCF-SINSEIYMD
           MOVE    SPA-MUID-KANANAME   TO  DASCF-KANANAME
           MOVE    SPA-MUID-NAME       TO  DASCF-NAME
           MOVE    SPA-MUID-EMAIL      TO  DASCF-EMAIL
           MOVE    SPA-MUID-HOSPSBT    TO  DASCF-HOSPSBT1
           MOVE    SPA-MUID-HOSPSBT1   TO  DASCF-HOSPSBT2
           MOVE    SPA-MUID-BEDSUIPPAN TO  DASCF-BEDSUIPPAN
           MOVE    SPA-MUID-BEDSURYOYO TO  DASCF-BEDSURYOYO
           MOVE    SPA-MUID-BEDSUSEISIN
                                       TO  DASCF-BEDSUSEISIN
           MOVE    SPA-MUID-BEDSUKEKKAKU
                                       TO  DASCF-BEDSUKEKKAKU
           MOVE    SPA-MUID-BEDSU      TO  DASCF-BEDSU
           MOVE    SPA-MUID-SETTI      TO  DASCF-SETTI
           MOVE    SPA-MUID-SYUKA      TO  DASCF-SYUKA
           MOVE    SPA-MUID-CIIKI      TO  DASCF-CHIIKI
           MOVE    SPA-MUID-DPCKBN     TO  DASCF-DPCKBN
           MOVE    SPA-MUID-ZAITAKU    TO  DASCF-ZAITAKU
           MOVE    SPA-MUID-NAI-KAIINYM
                                       TO  DASCF-KAIINYM
           MOVE    SPA-MUID-POST       TO  DASCF-POST
           MOVE    SPA-MUID-USER       TO  DASCF-USERID
           MOVE    SPA-MUID-PASSWORD   TO  DASCF-PASSWORD
           MOVE    "0"                 TO  DASCF-AGREEMENT
           MOVE    SPA-SYSYMD          TO  DASCF-CREYMD
      *
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       2120-DASCONFIG-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       212-SHELL-OUT-SEC           SECTION.
      *
      *    シェル起動処理
           MOVE    SPACE           TO  SHELLTBL
           MOVE    SHELL-NAME      TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID      TO  SHELLTBL-ARG2
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  MDAS-DURATION
               MOVE    "1000"          TO  SPA-MIDCD
               PERFORM 220-MID2SET-SEC
           END-IF
      *
           .
       212-SHELL-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    状況
      *****************************************************************
       213-JYOKYO-SEC              SECTION.
      *
           PERFORM 900-JOBLOG-READ-SEC
      *
           IF      FLG-JOBLOG          =   1
               MOVE    5               TO  MDAS-DURATION
               MOVE    SPACE           TO  SPA-MIDCD
           ELSE
               MOVE    SPACE           TO  SPA-MIDCD
               MOVE    ZERO            TO  MDAS-DURATION
               IF      JOBLOG-ERRCD        =   "000"
                                           OR  "401"
                                           OR  "403"
                                           OR  "404"
                                           OR  "999"
                   STRING  "0"                 DELIMITED BY SIZE
                           JOBLOG-ERRCD        DELIMITED BY SIZE
                           INTO                SPA-MIDCD
                   END-STRING
               END-IF
           END-IF
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 220-MID2SET-SEC
           END-IF
      *
           .
       213-JYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  LINKAREA
               MOVE    "CURRENT"       TO  MCP-PUTTYPE
               MOVE    "MDAS"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "9000"
                    MOVE    "設定ファイルのＯＰＥＮに失敗しました。"
                                           TO  SPA-ERRMSG
                WHEN    "9001"
                    MOVE    "設定ファイルの出力に失敗しました。"
                                           TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE          TO  SPA-ERRMSG
            END-EVALUATE.
      *
           MOVE    SPACE           TO  MERR
           MOVE    SPA-ERRCD       TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MERR-ERRMSG
      *
           MOVE    "B01"           TO  MCP-WIDGET.
           MOVE    "MDAS"          TO  SPA-MOTOPG.
           MOVE    "MERR"          TO  SPA-SAKIPG.
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "MERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   PERFORM 210-BACK
      *        同意しない
               WHEN    "CLICKED"   ALSO    "B05"
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   PERFORM 210-FILECHECK-SEC
      *        同意する
               WHEN    "CLICKED"   ALSO    "B08"
      *2012.10 サーベイ簡易設定改修
      *        メッセージダイアログからの復帰と同様になりすます
                   IF      SPA-MOTOPG      =   "M00"
                        MOVE    "9102"          TO  SPA-MIDCD
                   END-IF
      *
                   MOVE    "OK"            TO  SPA-MID1-FLG
                   PERFORM 210-BACK
      *        タイマーイベント
               WHEN    "CLICKED"   ALSO    "pandatimer1"
                   PERFORM 213-JYOKYO-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *2012.10 サーベイ簡易設定改修
           IF      SPA-MOTOPG      =   "M00"
               MOVE    "M00"           TO  SPA-SAKIPG
               MOVE    "M00"           TO  MCP-WINDOW
           ELSE
               MOVE    "M98"           TO  SPA-SAKIPG
               MOVE    "M98"           TO  MCP-WINDOW
           END-IF
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請ファイルチェック処理
      *****************************************************************
       210-FILECHECK-SEC           SECTION.
      *
           INITIALIZE                  SPA-MUID-AREA
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM 900-DASCONFIG-READ-SEC
           IF      FLG-DASCONFIG       =   ZERO
               PERFORM 3120-CONFIG-SPA-SET-SEC
           END-IF
      *
           MOVE    SPA-HOSPID      TO  SPA-MUID-HOSPID
           MOVE    SPA-SYSYMDWH    TO  SPA-MUID-SINSEIYMD
           MOVE    SPA-SYSYMD      TO  SPA-MUID-NAI-SINSEIYMD
      *
      *    申請してなければ戻る
           IF      FLG-OK              =   ZERO
               MOVE    SPACE           TO  SPA-MID1-FLG
               PERFORM 210-BACK
           ELSE
               PERFORM 220-MID1SET-SEC
           END-IF
      *
           .
       210-FILECHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請データＳＰＡセット処理
      *****************************************************************
       3120-CONFIG-SPA-SET-SEC         SECTION.
      *
           MOVE    DASCF-KANANAME      TO  SPA-MUID-KANANAME
           MOVE    DASCF-NAME          TO  SPA-MUID-NAME
           MOVE    DASCF-EMAIL         TO  SPA-MUID-EMAIL
           MOVE    DASCF-HOSPSBT1      TO  SPA-MUID-HOSPSBT
           MOVE    DASCF-HOSPSBT2      TO  SPA-MUID-HOSPSBT1
           MOVE    DASCF-BEDSUIPPAN    TO  SPA-MUID-BEDSUIPPAN
           MOVE    DASCF-BEDSURYOYO    TO  SPA-MUID-BEDSURYOYO
           MOVE    DASCF-BEDSUSEISIN   TO  SPA-MUID-BEDSUSEISIN
           MOVE    DASCF-BEDSUKEKKAKU  TO  SPA-MUID-BEDSUKEKKAKU
           MOVE    DASCF-BEDSU         TO  SPA-MUID-BEDSU
           MOVE    DASCF-SETTI         TO  SPA-MUID-SETTI
           MOVE    DASCF-SYUKA         TO  SPA-MUID-SYUKA
           MOVE    DASCF-CHIIKI        TO  SPA-MUID-CIIKI
           MOVE    DASCF-DPCKBN        TO  SPA-MUID-DPCKBN
           MOVE    DASCF-ZAITAKU       TO  SPA-MUID-ZAITAKU
           MOVE    DASCF-KAIINYM       TO  SPA-MUID-KAIINYM
           MOVE    DASCF-POST          TO  SPA-MUID-POST
           MOVE    DASCF-USERID        TO  SPA-MUID-USER
           MOVE    DASCF-PASSWORD      TO  SPA-MUID-PASSWORD
           IF      DASCF-SENDRESULT    =   "000"
               MOVE    1                   TO  FLG-OK
           END-IF
           .
       3120-CONFIG-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       220-MID1SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID1
      *
           INITIALIZE                  MID1
           MOVE    "1001"          TO  MID1-ID1CODE
           MOVE
           "申請情報（参加しない）を登録（送信）しますか？"
                                   TO  MID1-ID1MSG
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "MID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       220-MID1SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       220-MID2SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID2
      *
           INITIALIZE                  MID2
           MOVE    SPA-MIDCD       TO  MID2-ID1CODE
      *
           EVALUATE    SPA-MIDCD
               WHEN    "1000"
                   MOVE
       "登録作業中です。２〜３秒待ってからボタンをクリックして下さい。"
                                       TO  MID2-ID1MSG
               WHEN    "0000"
                   MOVE    "申請情報を登録（送信）しました。"
                                       TO  MID2-ID1MSG
               WHEN    "0401"
                   MOVE    "定点参加番号またはパスワードが不正です。"
                                       TO  MID2-ID1MSG
               WHEN    "0403"
                   MOVE    "医療機関ＩＤが不正です。"
                                       TO  MID2-ID1MSG
               WHEN    "0404"
                   MOVE    "ファイルが見つかりません。"
                                       TO  MID2-ID1MSG
               WHEN    "0999"
                   MOVE    "収集サーバが見つかりません。"
                                       TO  MID2-ID1MSG
           END-EVALUATE
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "MID2"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID2"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       220-MID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    定点設定読込
      *****************************************************************
       900-DASCONFIG-READ-SEC          SECTION.
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-DASCONFIG
                   MOVE    MCPDATA-REC         TO  DASCF-REC
               ELSE
                   MOVE    1                   TO  FLG-DASCONFIG
               END-IF
           ELSE
               MOVE    1                   TO  FLG-DASCONFIG
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DASCONFIG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブログ読み込み
      *****************************************************************
       900-JOBLOG-READ-SEC         SECTION.
      *
           INITIALIZE                  JOBLOG-REC
           MOVE    SPA-HOSPNUM     TO  JOBLOG-HOSPNUM
           MOVE    "das-profile"   TO  JOBLOG-PROGNAME
           MOVE    JOBLOG-REC      TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_joblog"    TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-JOBLOG
                   MOVE    MCPDATA-REC     TO  JOBLOG-REC
               ELSE
                   MOVE    1               TO  FLG-JOBLOG
               END-IF
           ELSE
               MOVE    1               TO  FLG-JOBLOG
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
            .
       900-JOBLOG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
