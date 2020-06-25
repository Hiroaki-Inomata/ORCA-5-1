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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGG02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 月次統計データ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/11/12    NACL−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04.02.17  ファイルの出力先指定
      *  01.00.02    NACL-伊藤    04.02.17  統計データの転送エラー
      *                                     メッセージの追加
      *  01.00.03    NACL-藤原    04.03.01  確認画面出力の条件修正
      *  01.00.04    NACL-藤原    04.08.10  処理開始前にジョブ管理の
      *                                     チェックを再度行なう
      *  01.00.05    NACL-藤原    05/01/30  入院対応
      *  02.07.00    NACL-太田    05/10/26  クリアボタン追加
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *  03.05.01    NACL-藤原    07/08/01  クライアント保存対応
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *
      *  04.05.01    NACL-藤原    10/03/23  ＨＯＳＴ名取得方法の変更
      *
      *  04.07.01    NACL-藤原    12/10/31  ファイル出力先のデフォルト変更
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    14/08/26  fdd 廃止に伴う対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    月次統計共通パラメタ
           COPY    "G01COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "G01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TOUKEIERR         PIC X(200).
           03  WRK-SRYYM           PIC X(06).
           03  WRK-CREYMD          PIC X(08).
           03  WRK-GIDMSG          PIC X(70).
           03  WRK-YM              PIC X(07). 
           03  WRK-PATH            PIC X(64).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENFLG          PIC 9(01).    
      *     
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-FILENM-AREA.
               07  WRK-FILENM1     PIC X(150).
               07  WRK-FILENM2     PIC X(150).
      *********07  WRK-FILENM1-FOLDER 
      *********                    PIC X(150).
      *********07  WRK-FILENM2-FOLDER
      *********                    PIC X(150).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID        PIC X(08)   VALUE "MONTHLY1".
           03  WRK-CONS-JOB-JOBID          PIC 9(07)   VALUE 1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSK1001.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    クライアント保存可否情報取得サブ
           COPY    "CPORCSCLIENTSV.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
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
           COPY    "ORCA51SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
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
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-G01FREE     TO  SPA-FREE
      *
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
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "GERR"
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
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "G02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XA01"  OR  "XB01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-G01FREE
                   INITIALIZE              SPA-G01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *
               INITIALIZE              SPA-G01KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "G99"
                   PERFORM 340-G99-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *    作表処理
               WHEN    "GID1"
                   PERFORM 330-GID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
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
           INITIALIZE              SPA-G02-AREA
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-SYMD    (1:6)  TO  SPA-NAI-G02-SRYYM
           MOVE    WRK-HENYMDG (1:6)  TO  SPA-GMN-G02-SRYYM
           MOVE    WRK-HENYMDG3(1:16) TO  SPA-NAI-G02-SRYYMH
      *
           IF      SPA-BEDSU       >   ZERO
               MOVE    "1"         TO  SPA-GMN-G02-NYUGAIKBNL       (1)
               MOVE    "入院"      TO  SPA-GMN-G02-NYUGAIKBNMEIL    (1)
               MOVE    "2"         TO  SPA-GMN-G02-NYUGAIKBNL       (2)
               MOVE    "入院外"    TO  SPA-GMN-G02-NYUGAIKBNMEIL    (2)
               MOVE    2           TO  SPA-GMN-G02-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"         TO  SPA-GMN-G02-NYUGAIKBNL       (1)
               MOVE    "入院外"    TO  SPA-GMN-G02-NYUGAIKBNMEIL    (1)
               MOVE    1           TO  SPA-GMN-G02-NYUGAIKBN-MAX
           END-IF
      *
           MOVE    SPA-GMN-G02-NYUGAIKBNLST (1)
                                   TO  SPA-GMN-G02-NYUGAIKBN-G
      *
      *    クライアント保存可否情報取得サブ
           INITIALIZE                      ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"      USING   ORCSCLIENTSVAREA
                                               SPA-AREA
      *
           IF      SCLIENTSV-CLIENT-SAVE
                                   =   "1"
               MOVE    "6"             TO  SPA-GMN-G02-FILE1L    (1)
               MOVE    "クライアント保存"
                                       TO  SPA-GMN-G02-FILE1MEIL (1)
               MOVE    1               TO  SPA-GMN-G02-FILE1-MAX
           END-IF
      *
           MOVE    SPA-GMN-G02-FILE1LST (1)
                                   TO  SPA-GMN-G02-FILE1-G
      *
           IF      SCLIENTSV-CLIENT-SAVE
                                   =   "1"
               MOVE    "6"             TO  SPA-GMN-G02-FILE2L    (1)
               MOVE    "クライアント保存"
                                       TO  SPA-GMN-G02-FILE2MEIL (1)
               MOVE    1               TO  SPA-GMN-G02-FILE2-MAX
           END-IF
      *
           MOVE    SPA-GMN-G02-FILE2LST (1)
                                   TO  SPA-GMN-G02-FILE2-G
      *
           MOVE    "T"             TO  SPA-GMN-G02-DATAKBN1-VALUE
           MOVE    "F"             TO  SPA-GMN-G02-DATAKBN2-VALUE
           MOVE    "F"             TO  SPA-GMN-G02-DATAKBN3-VALUE
           MOVE    "T"             TO  SPA-GMN-G02-HOUKATUKBN1-VALUE
           MOVE    "F"             TO  SPA-GMN-G02-HOUKATUKBN2-VALUE
           MOVE    "T"             TO  SPA-GMN-G02-KOJINKBN1-VALUE
           MOVE    "F"             TO  SPA-GMN-G02-KOJINKBN2-VALUE
           MOVE    "T"             TO  SPA-GMN-G02-CDKBN-VALUE (1)
           MOVE    "F"             TO  SPA-GMN-G02-CDKBN-VALUE (2)
           MOVE    "F"             TO  SPA-GMN-G02-CDKBN-VALUE (3)
      *
           MOVE    1               TO  SPA-GMN-G02-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＧＩＤ１）ＯＫ処理
      *****************************************************************
       330-GID1-SET-SEC            SECTION.
      *
           IF      SPA-GID1-FLG    =   "OK"
               EVALUATE    SPA-GIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"             TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1004"
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD      =   SPACE
                           PERFORM 500-KAKUTEI-SEC
                           GO  TO  330-GID1-SET-EXT
                       END-IF
                END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-G99-FLG
           MOVE    SPACE           TO  SPA-GID1-FLG
           MOVE    SPACE           TO  SPA-GIDCD
           .
       330-GID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-G99-SET-SEC             SECTION.
      *
           IF    ( SPA-GID1-FLG    =   "SV" )
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "D"             TO  SFILESV-MODE
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SFILESV-TBL-KEY
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
      *
           MOVE    SPACE           TO  SPA-G99-FLG
           MOVE    SPACE           TO  SPA-GID1-FLG
           MOVE    SPACE           TO  SPA-GIDCD
           MOVE    SPACE           TO  SPA-G99-MOTOPG
      *
           .
       340-G99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *     クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 310-SPASET-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-MAE-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
      *    処理区分
               WHEN    "CLICKED"       ALSO    "DATAKBN1"
               WHEN    "CLICKED"       ALSO    "DATAKBN2"
               WHEN    "CLICKED"       ALSO    "DATAKBN3"
                   PERFORM 400-DATAKBN-SEC
      *    作成内容区分
               WHEN    "CLICKED"       ALSO    "HOUKATUKBN1"
               WHEN    "CLICKED"       ALSO    "HOUKATUKBN2"
                   PERFORM 400-HOUKATUKBN-SEC
      *    個人情報
               WHEN    "CLICKED"       ALSO    "KOJINKBN1"
               WHEN    "CLICKED"       ALSO    "KOJINKBN2"
                   PERFORM 400-KOJINKBN-SEC
      *    文字コード
               WHEN    "CLICKED"       ALSO    "CDKBN1"
               WHEN    "CLICKED"       ALSO    "CDKBN2"
               WHEN    "CLICKED"       ALSO    "CDKBN3"
                   PERFORM 400-CDKBN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-G02-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4101-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    G02-SRYYM            TO  SPA-GMN-G02-SRYYM
           MOVE    G02-NYUGAIKBN        TO  SPA-GMN-G02-NYUGAIKBN-G
           MOVE    G02-FILENM1          TO  SPA-GMN-G02-FILENM1
           MOVE    G02-FILENM2          TO  SPA-GMN-G02-FILENM2
           MOVE    G02-FILE1            TO  SPA-GMN-G02-FILE1-G
           MOVE    G02-FILE2            TO  SPA-GMN-G02-FILE2-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    診療年月
           PERFORM 41021-SRYYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    入外区分
           PERFORM 41022-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    （診療）出力先
           PERFORM 41023-FILE1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    （病名）出力先
           PERFORM 41024-FILE2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    （診療）出力ファイル名
           PERFORM 41025-FILENM1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    （病名）出力ファイル名
           PERFORM 41026-FILENM2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       41021-SRYYM-CHK-SEC           SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-G02-SRYYM
           MOVE    SPACE              TO  SPA-NAI-G02-SRYYMH
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    SPA-GMN-G02-SRYYM   TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-G02-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-G02-SRYYM
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-G02-SRYYMH
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       41021-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分チェック処理
      *****************************************************************
       41022-NYUGAIKBN-CHK-SEC                SECTION.
      *
           MOVE    ZERO               TO  WRK-HENFLG
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-G02-NYUGAIKBN-MAX
                   OR      WRK-HENFLG =   1
               IF      SPA-GMN-G02-NYUGAIKBN 
                               =   SPA-GMN-G02-NYUGAIKBNL (IDY)
                   MOVE    SPA-GMN-G02-NYUGAIKBNLST (IDY)
                                      TO  SPA-GMN-G02-NYUGAIKBN-G
                   MOVE    1          TO  WRK-HENFLG
               END-IF
           END-PERFORM
           IF      WRK-HENFLG         =   ZERO
               MOVE    "0002"             TO  SPA-ERRCD
           END-IF
           .
       41022-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    （診療）出力先チェック処理
      *****************************************************************
       41023-FILE1-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-G02-FILE1-MAX
                                       =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-G02-CUR
               GO  TO  41023-FILE1-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-G02-FILE1     =   SPACE
               MOVE    SPA-GMN-G02-FILE1LST (1)
                                           TO  SPA-GMN-G02-FILE1-G
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-G02-FILE1-MAX
                   IF        SPA-GMN-G02-FILE1
                                   =   SPA-GMN-G02-FILE1L (IDX)
                       MOVE    SPA-GMN-G02-FILE1MEIL (IDX)
                                           TO  SPA-GMN-G02-FILE1MEI
                       MOVE    SPA-GMN-G02-FILE1-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM        
               IF      SPA-GMN-G02-FILE1MEI  =   SPACE  
                   MOVE    SPA-GMN-G02-FILE1LST (1)
                                             TO  SPA-GMN-G02-FILE1-G
               END-IF
           END-IF
           .
       41023-FILE1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    （病名）出力先チェック処理
      *****************************************************************
       41024-FILE2-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-G02-FILE1-MAX
                                       =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-G02-CUR
               GO  TO  41024-FILE2-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-G02-FILE2     =   SPACE
               MOVE    SPA-GMN-G02-FILE2LST (1)
                                           TO  SPA-GMN-G02-FILE2-G
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-G02-FILE2-MAX
                   IF        SPA-GMN-G02-FILE2
                                   =   SPA-GMN-G02-FILE2L (IDX)
                       MOVE    SPA-GMN-G02-FILE2MEIL (IDX)
                                           TO  SPA-GMN-G02-FILE2MEI
                       MOVE    SPA-GMN-G02-FILE2-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM        
               IF      SPA-GMN-G02-FILE2MEI  =   SPACE  
                   MOVE    SPA-GMN-G02-FILE2LST (1)
                                               TO  SPA-GMN-G02-FILE2-G
               END-IF
           END-IF
           .
       41024-FILE2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    （病名）出力先ファイル名チェック処理
      *****************************************************************
       41025-FILENM1-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-G02-FILENM1     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       150
                       OR      SPA-GMN-G02-FILENM1 (IDX:1)
                                           =   SPACE
                   IF        SPA-GMN-G02-FILENM1 (IDX:1)
                                               =   "/" OR  "\" OR  ":" 
                       MOVE    "0005"              TO  SPA-ERRCD
                       MOVE    4                   TO  SPA-GMN-G02-CUR
                       MOVE    150                 TO  IDX
                   END-IF
               END-PERFORM        
           END-IF
           .
       41025-FILENM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    （病名）出力先ファイル名チェック処理
      *****************************************************************
       41026-FILENM2-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-G02-FILENM2     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       150
                       OR      SPA-GMN-G02-FILENM2 (IDX:1)
                                           =   SPACE
                   IF        SPA-GMN-G02-FILENM2 (IDX:1)
                                               =   "/" OR  "\" OR  ":" 
                       MOVE    "0005"              TO  SPA-ERRCD
                       MOVE    6                   TO  SPA-GMN-G02-CUR
                       MOVE    150                 TO  IDX
                   END-IF
               END-PERFORM        
           END-IF
           .
       41026-FILENM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           IF    ( SPA-GMN-G02-DATAKBN2-VALUE  =   "T" )
           OR    ( SPA-GMN-G02-DATAKBN1-VALUE  =   "T" )
               IF      SPA-GMN-G02-FILENM1     =   SPACE
                   MOVE    4                   TO  SPA-GMN-G02-CUR
                   MOVE    "0003"              TO  SPA-ERRCD
                   GO  TO  4101-KAKUTEI-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-G02-DATAKBN3-VALUE  =   "T" )
           OR    ( SPA-GMN-G02-DATAKBN1-VALUE  =   "T" )
               IF      SPA-GMN-G02-FILENM2     =   SPACE
                   MOVE    6                   TO  SPA-GMN-G02-CUR
                   MOVE    "0004"              TO  SPA-ERRCD
                   GO  TO  4101-KAKUTEI-CHK-EXT
               END-IF
           END-IF  
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻るチェック処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "6666"          TO  SPA-GIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "G01"               TO  SPA-SAKIPG
           MOVE    "G02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分選択処理
      *****************************************************************
       400-DATAKBN-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    G02-DATAKBN1  TO  SPA-GMN-G02-DATAKBN1-VALUE
           MOVE    G02-DATAKBN2  TO  SPA-GMN-G02-DATAKBN2-VALUE
           MOVE    G02-DATAKBN3  TO  SPA-GMN-G02-DATAKBN3-VALUE
      *
           MOVE    1             TO  SPA-GMN-G02-CUR     
           .
       400-DATAKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成内容区分選択処理
      *****************************************************************
       400-HOUKATUKBN-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    G02-HOUKATUKBN1 TO  SPA-GMN-G02-HOUKATUKBN1-VALUE
           MOVE    G02-HOUKATUKBN2 TO  SPA-GMN-G02-HOUKATUKBN2-VALUE
      *
           MOVE    2               TO  SPA-GMN-G02-CUR     
           .
       400-HOUKATUKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    個人情報有無選択処理
      *****************************************************************
       400-KOJINKBN-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    G02-KOJINKBN1   TO  SPA-GMN-G02-KOJINKBN1-VALUE
           MOVE    G02-KOJINKBN2   TO  SPA-GMN-G02-KOJINKBN2-VALUE
      *
           MOVE    1               TO  SPA-GMN-G02-CUR     
           .
       400-KOJINKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字コード選択処理
      *****************************************************************
       400-CDKBN-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    G02-CDKBN(1)    TO  SPA-GMN-G02-CDKBN-VALUE(1)
           MOVE    G02-CDKBN(2)    TO  SPA-GMN-G02-CDKBN-VALUE(2)
           MOVE    G02-CDKBN(3)    TO  SPA-GMN-G02-CDKBN-VALUE(3)
      *
           MOVE    1               TO  SPA-GMN-G02-CUR     
           .
       400-CDKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計データ作成前チェック処理
      *****************************************************************
       450-KAKUTEI-MAE-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       =   SPACE
      *        ジョブ管理チェック処理
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "1004"          TO  SPA-GIDCD
               END-IF
           END-IF        
      *
           .
       450-KAKUTEI-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
      *    ジョブ管理チェック処理
           MOVE    "CH2"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE             TO  WRK-TOUKEIERR
                   STRING  "月次統計処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-TOUKEIERR
                   END-STRING                          
               END-IF
           END-IF    
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       500-KAKUTEI-SEC                 SECTION.
      *
           INITIALIZE                  SPA-G01-TBL-GROUP
                                       SPA-G01-JOB-JOBID
           MOVE    ZERO            TO  SPA-G99-FILESV-CHK-FLG
      *
           MOVE    SPACE           TO  WRK-FILENM-AREA
           IF    ( SPA-GMN-G02-FILE1   =   "6" )
               PERFORM VARYING IDX FROM    150 BY  -1
                       UNTIL   IDX <       1
                   IF      ( SPA-GMN-G02-FILENM1 (IDX:1)
                                               =   "/" )
      *********        MOVE    SPA-GMN-G02-FILENM1 (1:IDX)
      *********                                TO  WRK-FILENM1-FOLDER
                       COMPUTE IDY =   IDX     +   1
                       MOVE    SPA-GMN-G02-FILENM1 (IDY:)
                                               TO  WRK-FILENM1
                       MOVE    1               TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           IF      WRK-FILENM1         =   SPACE
      *********MOVE    "/home/orca/"        TO  WRK-FILENM1-FOLDER
               MOVE    SPA-GMN-G02-FILENM1     TO  WRK-FILENM1
           END-IF             
           IF    ( SPA-GMN-G02-FILE2       =   "6" )
               PERFORM VARYING IDX FROM    150 BY  -1
                       UNTIL   IDX <       1
                   IF      ( SPA-GMN-G02-FILENM2 (IDX:1)
                                               =   "/" )
      ********         MOVE    SPA-GMN-G02-FILENM2 (1:IDX)
      ********                                 TO  WRK-FILENM2-FOLDER
                       COMPUTE IDY =   IDX     +   1
                       MOVE    SPA-GMN-G02-FILENM2 (IDY:)
                                               TO  WRK-FILENM2
                       MOVE    1               TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           IF      WRK-FILENM2         =   SPACE
      ******** MOVE    "/home/orca/"        TO  WRK-FILENM2-FOLDER
               MOVE    SPA-GMN-G02-FILENM2     TO  WRK-FILENM2
           END-IF             
      *
      *    ジョブ開始セット
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID                        
           PERFORM 800-ORCSJOB-SEC
      *
      *    ファイル保存情報削除
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "D"             TO  SFILESV-MODE
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA  
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "OR052ERR"          TO  ERRFLPARA-FILE-ID
           MOVE    2                   TO  ERRFLPARA-SYOKBN1
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
      *    医療機関情報の取得
           INITIALIZE                  SYS-1001-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-1001-REC
           ELSE
               MOVE   SPACE        TO  SYS-1001-REC
           END-IF
      *     
           MOVE    "toukei1.sh"    TO  SHELLTBL-NAME
      *
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-SHELLID
      *
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID      TO  SHELLTBL-ARG2
           MOVE     SYS-1001-HOSPCD
                                   TO  SHELLTBL-ARG3
           MOVE    SYS-1001-PREFNUM
                                   TO  SHELLTBL-ARG4
           MOVE    SPA-NAI-G02-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    SPA-GMN-G02-NYUGAIKBN 
                                   TO  SHELLTBL-ARG6
           MOVE    WRK-FILENM1     TO  SHELLTBL-ARG7
           MOVE    WRK-FILENM2     TO  SHELLTBL-ARG8
           EVALUATE    TRUE
               WHEN    SPA-GMN-G02-DATAKBN1-VALUE  =   "T"
                   MOVE    "0"         TO  SHELLTBL-ARG9
               WHEN    SPA-GMN-G02-DATAKBN2-VALUE  =   "T"
                   MOVE    "1"         TO  SHELLTBL-ARG9
               WHEN    SPA-GMN-G02-DATAKBN3-VALUE  =   "T"
                   MOVE    "2"         TO  SHELLTBL-ARG9
           END-EVALUATE
           EVALUATE    TRUE
               WHEN    SPA-GMN-G02-HOUKATUKBN1-VALUE   =   "T"
                   MOVE    "1"         TO  SHELLTBL-ARG10
               WHEN    SPA-GMN-G02-HOUKATUKBN2-VALUE   =   "T"
                   MOVE    "2"         TO  SHELLTBL-ARG10
           END-EVALUATE
           EVALUATE    TRUE
               WHEN    SPA-GMN-G02-KOJINKBN1-VALUE =   "T"
                   MOVE    "1"         TO  SHELLTBL-ARG11
               WHEN    SPA-GMN-G02-KOJINKBN2-VALUE =   "T"
                   MOVE    "2"         TO  SHELLTBL-ARG11
           END-EVALUATE
           EVALUATE    TRUE
               WHEN    SPA-GMN-G02-CDKBN-VALUE(1) =   "T"
                   MOVE    "1"         TO  SHELLTBL-ARG12
               WHEN    SPA-GMN-G02-CDKBN-VALUE(2) =   "T"
                   MOVE    "2"         TO  SHELLTBL-ARG12
               WHEN    SPA-GMN-G02-CDKBN-VALUE(3) =   "T"
                   MOVE    "3"         TO  SHELLTBL-ARG12
           END-EVALUATE
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG15
           MOVE    SPA-GMN-G02-FILE1   TO  SHELLTBL-ARG16
           MOVE    SPA-GMN-G02-FILE2   TO  SHELLTBL-ARG17
      *    ipアドレス取得
      *****PERFORM 900-SYSTEM-GET-SEC
      *****MOVE    SLDUSER-HOST(1) TO  SHELLTBL-ARG18
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG19
      *****MOVE    WRK-FILENM1-FOLDER
      *****                        TO  SHELLTBL-ARG20
      *****MOVE    WRK-FILENM2-FOLDER
      *****                        TO  SHELLTBL-ARG21
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF    ( SPA-GMN-G02-FILE1   =   "6" )
           OR    ( SPA-GMN-G02-FILE2   =   "6" )
               MOVE    "SV"            TO  SPA-GID1-FLG
           ELSE
               MOVE    SPACE           TO  SPA-GID1-FLG
           END-IF
           MOVE    SPACE           TO  SPA-G99-FLG
           MOVE    SPACE           TO  SPA-GIDCD
      *
      *    自動的に情報確認画面を表示
           PERFORM 490-KAKUNIN-SEC      
      *
           .
       500-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      G99
           MOVE    ZERO                TO  G99-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  G99-B12-STATE
      *
           MOVE    5                   TO  G99-DURATION
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                     
               MOVE    JOB-JOBID       TO  G99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  G99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  G99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  G99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  G99-ERRMSG   (IDX)
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  G99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SPACE       TO  G99-MSG
                       PERFORM 4901-JOB-ERRSET-SEC
                       IF      G99-MSG     =   SPACE
                           STRING  JOB-YOBI        DELIMITED BY SPACE
                                   "【"            DELIMITED BY SIZE
                                   JOB-SHELLMSG    DELIMITED BY SPACE
                                   "】"            DELIMITED BY SIZE
                                                   INTO  G99-MSG
                           END-STRING   
                       END-IF
                   END-IF                          
               END-IF    
               MOVE    IDX                     TO  G99-COUNT    
           END-IF
      *
           IF      G99-MSG              =   SPACE
               IF      G99-COUNT            >   ZERO    
                   MOVE    "処理は正常に終了しました"
                                                TO  G99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  G99-MSG
               END-IF                                 
               MOVE    "B01"                TO  MCP-WIDGET
           ELSE
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "G02"                TO  SPA-MOTOPG
                                            SPA-G99-MOTOPG
           MOVE    "G99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "G99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージセット処理
      *****************************************************************
       4901-JOB-ERRSET-SEC             SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "【ファイルがありません】" 
                                               TO  G99-MSG
               WHEN    "0002"
                   MOVE    "【ｆｄｄが起動されていません】" 
                                               TO  G99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "【ｆｄｄで処理が失敗しました】" 
                                               TO  G99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】" 
                                               TO  G99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】" 
                                               TO  G99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】" 
                                               TO  G99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】" 
                                               TO  G99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-G01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "G02"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           IF      SPA-GIDCD       NOT =   SPACE
               PERFORM 520-GIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "G02    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  G02
      *
           INITIALIZE                      G02
           MOVE    SPA-GMN-G02-SRYYM       TO  G02-SRYYM
           MOVE    SPA-GMN-G02-FILENM1     TO  G02-FILENM1
           MOVE    SPA-GMN-G02-FILENM2     TO  G02-FILENM2
      *
           MOVE    SPA-GMN-G02-DATAKBN1-VALUE  TO  G02-DATAKBN1
           MOVE    SPA-GMN-G02-DATAKBN2-VALUE  TO  G02-DATAKBN2
           MOVE    SPA-GMN-G02-DATAKBN3-VALUE  TO  G02-DATAKBN3
           MOVE    SPA-GMN-G02-KOJINKBN1-VALUE TO  G02-KOJINKBN1
           MOVE    SPA-GMN-G02-KOJINKBN2-VALUE TO  G02-KOJINKBN2
           MOVE    SPA-GMN-G02-HOUKATUKBN1-VALUE
                                               TO  G02-HOUKATUKBN1
           MOVE    SPA-GMN-G02-HOUKATUKBN2-VALUE
                                               TO  G02-HOUKATUKBN2
           MOVE    SPA-GMN-G02-CDKBN-VALUE (1) TO  G02-CDKBN(1)
           MOVE    SPA-GMN-G02-CDKBN-VALUE (2) TO  G02-CDKBN(2)
           MOVE    SPA-GMN-G02-CDKBN-VALUE (3) TO  G02-CDKBN(3)
      *
           MOVE    SPA-GMN-G02-NYUGAIKBN-G     TO  G02-NYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-G02-NYUGAIKBN-MAX
               MOVE    SPA-GMN-G02-NYUGAIKBNLST (IDX)
                                           TO  G02-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-G02-NYUGAIKBN-MAX
                                           TO  G02-NYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-G02-FILE1-G     TO  G02-FILE1
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-G02-FILE1-MAX
               MOVE    SPA-GMN-G02-FILE1LST (IDX)
                                           TO  G02-FILE1-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-G02-FILE1-MAX   TO  G02-FILE1-COUNT
      *
           MOVE    SPA-GMN-G02-FILE2-G     TO  G02-FILE2
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-G02-FILE2-MAX
               MOVE    SPA-GMN-G02-FILE2LST (IDX)
                                           TO  G02-FILE2-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-G02-FILE2-MAX   TO  G02-FILE2-COUNT
      *
           IF      SPA-GMN-G02-FILE1-MAX
                                       =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  G02-B12-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  G02-B12-STATE
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
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-G02-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-G02-CUR
             WHEN    1
                    MOVE  "SRYYM"      TO  MCP-WIDGET
             WHEN    2
                    MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    3
                    MOVE  "FILE1"      TO  MCP-WIDGET
             WHEN    4
                    MOVE  "FILENM1"    TO  MCP-WIDGET
             WHEN    5
                    MOVE  "FILE2"      TO  MCP-WIDGET
             WHEN    6
                    MOVE  "FILENM2"    TO  MCP-WIDGET
             WHEN    99
                    MOVE  "B12"        TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SRYYM"
               MOVE    1                   TO  SPA-GMN-G02-CUR
           WHEN    "NYUGAIKBN"
               MOVE    2                   TO  SPA-GMN-G02-CUR
           WHEN    "FILE1"
               MOVE    3                   TO  SPA-GMN-G02-CUR
           WHEN    "FILENM1"
               MOVE    4                   TO  SPA-GMN-G02-CUR
           WHEN    "FILE2"
               MOVE    5                   TO  SPA-GMN-G02-CUR
           WHEN    "FILENM2"
               MOVE    99                  TO  SPA-GMN-G02-CUR
           WHEN    "B12"
               MOVE    99                  TO  SPA-GMN-G02-CUR
           END-EVALUATE
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "FILENM2"
           WHEN    "B12"
               CONTINUE
           WHEN    OTHER
               COMPUTE SPA-GMN-G02-CUR =   SPA-GMN-G02-CUR +   1
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
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
                   MOVE    "診療年月入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "入外区分入力エラー"       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "（診療）出力ファイル名入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "（病名）出力ファイル名入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   STRING   "出力ファイル名に”／”" DELIMITED  BY  SIZE
                            "、”：”、”￥”が入力してあります"
                                                     DELIMITED  BY  SIZE
                                                     INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING   "システム管理のクライアント保存"
                                                   DELIMITED  BY  SIZE
                            "の指定を有効にして下さい"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "7777"
                   MOVE    WRK-TOUKEIERR               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  GERR
           MOVE    SPA-ERRCD            TO  GERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  GERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "G02"                TO  SPA-MOTOPG
           MOVE    "GERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "GERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示理
      *****************************************************************
       520-GIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-GIDMSG
      *
           EVALUATE    SPA-GIDCD
               WHEN    "1004"
                   STRING  SPA-NAI-G02-SRYYMH
                                             DELIMITED  BY  SIZE
                           "分統計データ作成処理を行います"
                                             DELIMITED  BY  SIZE 
                                             INTO  WRK-GIDMSG
                   END-STRING
               WHEN    "6666"
                   MOVE    "統計データ作成中です"
                                       TO  WRK-GIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  GID1
           INITIALIZE                       GID1
           MOVE    SPA-GIDCD            TO  GID1-ID1CODE
           MOVE    WRK-GIDMSG           TO  GID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "G02"                TO  SPA-MOTOPG
           MOVE    "GID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "GID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-GIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ipアドレス取得
      *****************************************************************
       900-SYSTEM-GET-SEC          SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    2               TO  SLDUSER-KBN
           MOVE    MCP-TERM        TO  SLDUSER-ID
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       900-SYSTEM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
