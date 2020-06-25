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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGN22.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬剤情報設定
      *  コンポーネント名  : 薬剤情報（Ｎ２２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/12/13    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  01.01.01    NACL-森脇     06/09/09  ジョブ管理削除
      *  01.01.02    NACL-森脇     06/10/11  タイマーイベント追加
      *  04.02.01    NACL-森脇     08/02/20  参照情報表示追加
      *                                      リスト行数５０→４０に変更
      *                                      タイマーイベントクリア追加
      *  04.06.00    NACL-森脇     11/02/07  情報削除追加
      *  04.07.00    NACL-伊藤     11/12/01  参照スクリプト変更
      *  04.08.01    NACL-森脇     14/07/07  一時ファイルパスセット
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                DIVISION.
      *
       WORKING-STORAGE         SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N21COMMON-SPA".
      *
           COPY    "N21SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    COPY    "ORCA-DBPATH".
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TEMP            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
           03  IDY                 PIC 9(03).
      *
      *    処理区分領域
       01  SKBN-AREA.
           03  SKBN-RUN            PIC X(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IN              PIC X(01).
           03  WRK-ZZZZ9           PIC ZZZZ9.
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-GAZO            PIC X(256).
           03  WRK-GAZOFILE        PIC X(256).
           03  WRK-OBJECT          PIC X(36).
           03  WRK-N21IDMSG        PIC X(70).
      *
       01  SHELL-AREA.
           03  SHELL-NAME1         PIC X(13)  VALUE "siori_pull.sh".
      *----(04.08.01)--UPD-START---
      * 01  WRK-FILE-AREA.
      *     03  WRK-FILE-FOLDER     PIC X(14)  VALUE "/tmp/medphoto/".
      *----(04.08.01)--UPD-END-----
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "YKJMST".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
           03  WRK-CONS-MEDIMGDIR  PIC X(08)   VALUE   "medimage".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    薬剤情報参照用ＤＢ
           COPY    "CPYAKUJYOTEMP.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
           COPY    "ORCA-BLOB".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイルパスセット
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "N21.INC".
           COPY    "N22.INC".
           COPY    "N23.INC".
           COPY    "N29.INC".
           COPY    "N21ERR.INC".
           COPY    "N21ID1.INC".
           COPY    "N21ID2.INC".
           COPY    "N21FIL.INC".
      *
       PROCEDURE           DIVISION    USING
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-N21KYOUTU
           MOVE    SPA-FREE        TO  SPA-N21SCRAREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-N21SCRAREA  TO  SPA-FREE
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面セット
           PERFORM 300-SCREEN-SEC
      *    画面編集
           PERFORM 500-GMNHEN-SEC
      *
      *----(01.01.01)--UPD-START---
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "N22"           TO  MCP-WINDOW.
      *----(01.01.01)--UPD-END-----
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1               TO  FLG-END
           .
      *
       100-INIT-EXT.
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
                   MOVE    SPACE           TO  SPA-N22-FLG
                   PERFORM 450-BACK
      *    情報削除
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 201-DELETE-SEC
      *    状況
      *----(01.01.02)---ADD-START---
               WHEN    "CLICKED"   ALSO    "pandatimer1"
      *----(01.01.02)---ADD-END-----
               WHEN    "CLICKED"   ALSO    "B10"
                   PERFORM 400-JYOKYO-SEC
      *    問合せ
               WHEN    "CLICKED"   ALSO    "B11"
                   PERFORM 400-JYOUHOU-SEC
      *    登録
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 480-TOUROKU-SEC
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
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"  ALSO    "SELNUM"
                   PERFORM 4111-SELNUM-SEC
               WHEN    "ACTIVATE"  ALSO    "GAZOKBN"
                   PERFORM 4112-GAZOKBN-SEC
               WHEN    "ACTIVATE"  ALSO    "COPYKBN"
                   PERFORM 4112-COPYKBN-SEC
               WHEN    ANY         ALSO    "KEKKALST"
                   PERFORM 4113-KEKKALST-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *        確認画面ＯＫ処理
               WHEN    "N21ID1"
                   PERFORM 490-JYOUHOU-DEL-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-N21ID1-FLG
           MOVE    SPACE           TO  SPA-N21IDCD
      *
           INITIALIZE                  SPA-N22-AREA
           MOVE    SPA-NAI-SRYCD   TO  SPA-N22-KENCD
           MOVE    SPA-GMN-KENNAME TO  SPA-N22-KENNAME
           MOVE    LOW-VALUE       TO  SPA-N22-OBJECT(1)
                                       SPA-N22-OBJECT(2)
                                       SPA-N22-OBJECT(3)
      *
      *    取込区分コンボ
           MOVE    "1 マージ"      TO  SPA-N22-COPYKBNLST(1)
           MOVE    "2 置き換え"    TO  SPA-N22-COPYKBNLST(2)
           MOVE    "3 写真のみ"    TO  SPA-N22-COPYKBNLST(3)
           EVALUATE    SPA-N22-COPY
               WHEN    "1"
                 MOVE    SPA-N22-COPYKBNLST(1)
                                       TO  SPA-N22-COPYKBN-G
               WHEN    "2"
                 MOVE    SPA-N22-COPYKBNLST(2)
                                       TO  SPA-N22-COPYKBN-G
               WHEN    "3"
                 MOVE    SPA-N22-COPYKBNLST(3)
                                       TO  SPA-N22-COPYKBN-G
               WHEN    OTHER
                 MOVE    SPA-N22-COPYKBNLST(1)
                                       TO  SPA-N22-COPYKBN-G
           END-EVALUATE
      *
           MOVE    3               TO  SPA-N22-COPYKBN-MAX
      *
           MOVE    1               TO  SPA-N22-CUR
      *
           PERFORM 400-JYOKYO-SEC
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報入手
      *****************************************************************
       400-JYOUHOU-SEC             SECTION.
      *
           MOVE    3               TO  WRK-CONS-JOB-JOBID
           PERFORM 820-JOBKANRI-READ-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "処理中です"    TO  SPA-N22-MESSAGE
               MOVE    "blue"          TO  SPA-N22-MESSAGE-STYLE
               GO  TO  400-JYOUHOU-SEC-EXT
           END-IF
      *
      *    ジョブ管理削除
           PERFORM 820-JOBKANRI-DEL-SEC
      *
      *    ジョブ管理作成
           PERFORM 820-JOBKANRI-JBS-SEC
      *
      *    処理
           MOVE    SPACE           TO  SHELLTBL
           MOVE    SHELL-NAME1     TO  SHELLTBL-NAME
           MOVE    SPA-N22-KENCD   TO  SHELLTBL-ARG1
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG2
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG3
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG4
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG5
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *----(01.01.02)--ADD-START---
           MOVE    1                TO  SPA-N22-JYOKYOFLG
      *----(01.01.02)--ADD-END-----
      *
           PERFORM 400-JYOKYO-SEC
      *
           .
       400-JYOUHOU-SEC-EXT.
           EXIT.
      *
     *****************************************************************
      *    状況
      *****************************************************************
       400-JYOKYO-SEC              SECTION.
      *
           MOVE    3               TO  WRK-CONS-JOB-JOBID
           PERFORM 820-JOBKANRI-READ-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "処理中です"    TO  SPA-N22-MESSAGE
               MOVE    "blue"          TO  SPA-N22-MESSAGE-STYLE
      *----(04.02.01)--ADD-START---
               MOVE    1               TO  SPA-N22-DURATION
      *----(04.02.01)--ADD-END-----
               GO  TO  400-JYOKYO-EXT
           ELSE
               MOVE    ZERO                TO  SPA-N22-DURATION
               IF      JOB-ERRCD           =   "0000"
                   MOVE    SPACE           TO  SPA-N22-MESSAGE
                   MOVE    SPACE           TO  SPA-N22-MESSAGE-STYLE
               ELSE
                   MOVE    JOB-YOBI        TO  SPA-N22-MESSAGE
                   IF      JOB-YOBI(9:16)  =   "No route to host"
                       MOVE    "[ERROR] サーバが見つかりません"
                                           TO  SPA-N22-MESSAGE
                   END-IF
                   IF      JOB-YOBI(9:3)   =   "404"
                       MOVE    "[ERROR] ファイルが見つかりません"
                                           TO  SPA-N22-MESSAGE
                   END-IF
                   IF      MCP-STATUS      =   "LINK"
                       MOVE    "blue"      TO  SPA-N22-MESSAGE-STYLE
                   ELSE
                       MOVE    "red"       TO  SPA-N22-MESSAGE-STYLE
                   END-IF
                   GO  TO  400-JYOKYO-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM 900-TEMP-SELECT-SEC
           PERFORM UNTIL  (FLG-TEMP    =   1)
      *----(04.02.01)--UPD-START---
      *             OR     (IDX         >=  50)
                   OR     (IDX         >=  40)
      *----(04.02.01)--UPD-END-----
               COMPUTE IDX =   IDX     +   1
               MOVE    IDX             TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  SPA-N22-TNO(IDX)
               MOVE    TEMP-YJCD       TO  SPA-N22-TYJCD(IDX)
      *         MOVE    TEMP-MAKE-COMPANY   TO  SPA-N22-TMAKER(IDX)
               MOVE    TEMP-NAME       TO  SPA-N22-TMAKER(IDX)
               MOVE    TEMP-SALE-COMPANY   TO  SPA-N22-TSELLER(IDX)
               MOVE    TEMP-EFFECT     TO  SPA-N22-TEFFECT(IDX)
               MOVE    TEMP-COLOR      TO  SPA-N22-TCOLOR(IDX)
               MOVE    TEMP-SHAPE      TO  SPA-N22-TSHAPE(IDX)
               MOVE    TEMP-MARK       TO  SPA-N22-TMARK(IDX)
               MOVE    TEMP-CAUTION    TO  SPA-N22-TCAUTION(IDX)
               MOVE    TEMP-PHOTO1-FILENAME    TO  SPA-N22-PHOTO1(IDX)
               MOVE    TEMP-PHOTO2-FILENAME    TO  SPA-N22-PHOTO2(IDX)
               MOVE    TEMP-PHOTO3-FILENAME    TO  SPA-N22-PHOTO3(IDX)
               IF     (TEMP-PHOTO1-FILENAME    NOT =   SPACE)
                   OR (TEMP-PHOTO2-FILENAME    NOT =   SPACE)
                   OR (TEMP-PHOTO3-FILENAME    NOT =   SPACE)
                   MOVE    "○"            TO  SPA-N22-TGAZO(IDX)
               END-IF
               PERFORM 900-TEMP-FETCH-SEC
           END-PERFORM
           PERFORM 900-TEMP-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-N22-MAX
      *
      *----(01.01.02)--ADD-START---
           IF      SPA-N22-JYOKYOFLG   NOT =   1
               GO  TO  400-JYOKYO-EXT
           END-IF
      *----(01.01.02)--ADD-END-----
      *
           IF      SPA-N22-MAX     =  ZERO
               MOVE    SPACE         TO  SKBN-RUN
               MOVE    "更新データはありません。"
                                     TO  SPA-N22-MESSAGE
               MOVE    "blue"        TO  SPA-N22-MESSAGE-STYLE
               GO  TO  400-JYOKYO-EXT
           END-IF
      *
           .
       400-JYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像コピー
      *****************************************************************
       400-GAZOCOPY-SEC            SECTION.
      *
           EVALUATE    SPA-N22-GAZOKBN
               WHEN    "1"
                   MOVE    SPA-N22-PHOTO1(SPA-N22-SELNUM)
                                   TO  WRK-GAZO
                   MOVE    "1"     TO  SPA-N22-GAZO
               WHEN    "2"
                   MOVE    SPA-N22-PHOTO2(SPA-N22-SELNUM)
                                   TO  WRK-GAZO
                   MOVE    "2"     TO  SPA-N22-GAZO
               WHEN    "3"
                   MOVE    SPA-N22-PHOTO3(SPA-N22-SELNUM)
                                   TO  WRK-GAZO
                   MOVE    "3"     TO  SPA-N22-GAZO
               WHEN    OTHER
                   MOVE    "0"     TO  SPA-N22-GAZO
                   GO  TO  400-GAZOCOPY-SEC-EXT
           END-EVALUATE
      *
           MOVE    WRK-GAZO        TO  SPA-GMN-PHOTO
           .
       400-GAZOCOPY-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *     選択番号処理
      *****************************************************************
       4111-SELNUM-SEC         SECTION.
      *
           MOVE    1           TO  SPA-N22-CUR
           INITIALIZE              SPA-N22-GAZOCOMBO
           INITIALIZE              SPA-N22-OBJECTAREA
           MOVE    LOW-VALUE   TO  SPA-N22-OBJECT(1)
                                   SPA-N22-OBJECT(2)
                                   SPA-N22-OBJECT(3)
      *----(04.02.01)--ADD-START---
           INITIALIZE              SPA-N22-ENTRY
      *----(04.02.01)--ADD-END-----
      *
           MOVE    N22-SELNUM  TO  SPA-N22-SELNUM
      *
           PERFORM 4111-SELNUM-CHK-SEC
           IF      SPA-N22-MESSAGE     NOT =   SPACE
               GO  TO  4111-SELNUM-EXT
           END-IF
      *
           PERFORM 4114-COMBOSET-SEC
      *
           MOVE    2           TO  SPA-N22-CUR
           .
      *
       4111-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *     選択番号チェック処理
      *****************************************************************
       4111-SELNUM-CHK-SEC     SECTION.
      *
           IF     (SPA-N22-SELNUM  <=  ZERO)
               OR (SPA-N22-SELNUM  =   SPACE)
               OR (SPA-N22-SELNUM  >   SPA-N22-MAX)
               MOVE    "範囲外の番号が入力されています"
                                   TO  SPA-N22-MESSAGE
               MOVE    "red"       TO  SPA-N22-MESSAGE-STYLE
           ELSE
               MOVE    SPACE       TO  SPA-N22-MESSAGE
               MOVE    SPACE       TO  SPA-N22-MESSAGE-STYLE
           END-IF
           .
      *
       4111-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     画像形態処理
      *****************************************************************
       4112-GAZOKBN-SEC        SECTION.
      *
           MOVE    2           TO  SPA-N22-CUR
      *
           MOVE    N22-GAZOKBN     TO  SPA-N22-GAZOKBN-G
      *
           PERFORM 4112-GAZOKBN-CHK-SEC
           IF      SPA-N22-MESSAGE     NOT =   SPACE
               GO  TO  4112-GAZOKBN-EXT
           END-IF
      *
           MOVE    3           TO  SPA-N22-CUR
           .
      *
       4112-GAZOKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *     画像形態処理
      *****************************************************************
       4112-GAZOKBN-CHK-SEC        SECTION.
      *
           IF      SPA-N22-GAZOKBN     =   SPACE
               GO  TO  4112-GAZOKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-HENFLG
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >       SPA-N22-GAZOKBN-MAX)
                   OR     (WRK-HENFLG      =   1)
               IF      SPA-N22-GAZOKBN =   SPA-N22-GAZOKBNL (IDX)
                   MOVE    SPA-N22-GAZOKBNLST (IDX)
                                       TO  SPA-N22-GAZOKBN-G
                   MOVE    1           TO  WRK-HENFLG
               END-IF
           END-PERFORM
      *
           IF      WRK-HENFLG          =   ZERO
               MOVE    "範囲外の番号が入力されています"
                                       TO  SPA-N22-MESSAGE
               MOVE    "red"           TO  SPA-N22-MESSAGE-STYLE
           ELSE
               MOVE    SPACE           TO  SPA-N22-MESSAGE
               MOVE    SPACE           TO  SPA-N22-MESSAGE-STYLE
           END-IF
           .
      *
       4112-GAZOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     取込形態処理
      *****************************************************************
       4112-COPYKBN-SEC            SECTION.
      *
           MOVE    3               TO  SPA-N22-CUR
      *
           MOVE    N22-COPYKBN     TO  SPA-N22-COPYKBN-G
      *
           PERFORM 4112-COPYKBN-CHK-SEC
           IF      SPA-N22-MESSAGE     NOT =   SPACE
               GO  TO  4112-COPYKBN-EXT
           END-IF
      *
           MOVE    4           TO  SPA-N22-CUR
           .
      *
       4112-COPYKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *     取込形態処理
      *****************************************************************
       4112-COPYKBN-CHK-SEC        SECTION.
      *
           IF      SPA-N22-COPYKBN     =   SPACE
               GO  TO  4112-COPYKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-HENFLG
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >       SPA-N22-COPYKBN-MAX)
                   OR     (WRK-HENFLG      =   1)
               IF      SPA-N22-COPYKBN =   SPA-N22-COPYKBNL (IDX)
                   MOVE    SPA-N22-COPYKBNLST (IDX)
                                       TO  SPA-N22-COPYKBN-G
                   MOVE    1           TO  WRK-HENFLG
               END-IF
           END-PERFORM
      *
           IF      WRK-HENFLG          =   ZERO
               MOVE    "範囲外の番号が入力されています"
                                       TO  SPA-N22-MESSAGE
               MOVE    "red"           TO  SPA-N22-MESSAGE-STYLE
           ELSE
               MOVE    SPACE           TO  SPA-N22-MESSAGE
               MOVE    SPACE           TO  SPA-N22-MESSAGE-STYLE
           END-IF
           .
      *
       4112-COPYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    結果リスト チェック処理
      *****************************************************************
       4113-KEKKALST-SEC           SECTION.
      *
           MOVE    ZERO            TO  WRK-HENFLG
           PERFORM VARYING IDX     FROM    1   BY  1
      *----(04.02.01)--UPD-START---
      *             UNTIL  (IDX     >       50)
                   UNTIL  (IDX     >       40)
      *----(04.02.01)--UPD-END-----
                   OR     (WRK-HENFLG  =   1)
               IF      N22-SELECT(IDX)     =   "T"
                   MOVE    IDX             TO  SPA-N22-SELNUM
                   MOVE    1               TO  WRK-HENFLG
               END-IF
           END-PERFORM
      *
           IF      WRK-HENFLG      NOT =   ZERO
               MOVE    SPACE           TO  SPA-N22-MESSAGE
               MOVE    SPACE           TO  SPA-N22-MESSAGE-STYLE
               PERFORM 4114-COMBOSET-SEC
           END-IF
      *
           MOVE    2               TO  SPA-N22-CUR
           .
      *
       4113-KEKKALST-EXT.
           EXIT.
      *
      *****************************************************************
      *     コンボボックスセット処理
      *****************************************************************
       4114-COMBOSET-SEC           SECTION.
      *
           INITIALIZE              SPA-N22-GAZOCOMBO
           INITIALIZE              SPA-N22-OBJECTAREA
           MOVE    LOW-VALUE   TO  SPA-N22-OBJECT(1)
                                   SPA-N22-OBJECT(2)
                                   SPA-N22-OBJECT(3)
           MOVE    ZERO            TO  IDX
      *
           IF  SPA-N22-TGAZO(SPA-N22-SELNUM)   =   "○"
               COMPUTE IDX =   IDX     +   1
               MOVE    "0 取り込まない"    TO  SPA-N22-GAZOKBNLST(IDX)
               IF      SPA-N22-GAZO        =   "0"
                   MOVE    SPA-N22-GAZOKBNLST(IDX)
                                           TO  SPA-N22-GAZOKBN-G
               END-IF
           END-IF
      *
           IF  SPA-N22-PHOTO1(SPA-N22-SELNUM) NOT = SPACE
               MOVE    SPA-N22-PHOTO1(SPA-N22-SELNUM)  TO  WRK-GAZO
               PERFORM 4115-GAZOSET-SEC
               MOVE    WRK-OBJECT          TO  SPA-N22-OBJECT(IDX)
               MOVE    "【　裸　体　】"    TO  SPA-N22-LABEL(IDX)
               COMPUTE IDX =   IDX     +   1
               MOVE    "1 裸体"            TO  SPA-N22-GAZOKBNLST(IDX)
               IF     (SPA-N22-GAZOKBN-G   =   SPACE)
                   OR (SPA-N22-GAZO        =   "1")
                   MOVE    SPA-N22-GAZOKBNLST(IDX)
                                           TO  SPA-N22-GAZOKBN-G
               END-IF
           END-IF
      *
           IF  SPA-N22-PHOTO2(SPA-N22-SELNUM) NOT = SPACE
               MOVE    SPA-N22-PHOTO2(SPA-N22-SELNUM)  TO  WRK-GAZO
               PERFORM 4115-GAZOSET-SEC
               MOVE    WRK-OBJECT          TO  SPA-N22-OBJECT(IDX)
               MOVE    "【ＰＴＰ包装】"    TO  SPA-N22-LABEL(IDX)
               COMPUTE IDX =   IDX     +   1
               MOVE    "2 ＰＴＰ包装"      TO  SPA-N22-GAZOKBNLST(IDX)
               IF     (SPA-N22-GAZOKBN-G   =   SPACE)
                   OR (SPA-N22-GAZO        =   "2")
                   MOVE    SPA-N22-GAZOKBNLST(IDX)
                                           TO  SPA-N22-GAZOKBN-G
               END-IF
           END-IF
      *
           IF  SPA-N22-PHOTO3(SPA-N22-SELNUM) NOT = SPACE
               MOVE    SPA-N22-PHOTO3(SPA-N22-SELNUM)  TO  WRK-GAZO
               PERFORM 4115-GAZOSET-SEC
               MOVE    WRK-OBJECT          TO  SPA-N22-OBJECT(IDX)
               MOVE    "【　その他　】"    TO  SPA-N22-LABEL(IDX)
               COMPUTE IDX =   IDX     +   1
               MOVE    "3 その他"          TO  SPA-N22-GAZOKBNLST(IDX)
               IF     (SPA-N22-GAZOKBN-G   =   SPACE)
                   OR (SPA-N22-GAZO        =   "3")
                   MOVE    SPA-N22-GAZOKBNLST(IDX)
                                           TO  SPA-N22-GAZOKBN-G
               END-IF
           END-IF
      *
           MOVE    IDX         TO  SPA-N22-GAZOKBN-MAX
      *
      *----(04.02.01)--ADD-START---
           INITIALIZE              SPA-N22-ENTRY
           MOVE    SPA-N22-TEFFECT(SPA-N22-SELNUM)
                                   TO  SPA-N22-EFFECT
           MOVE    SPA-N22-TCOLOR(SPA-N22-SELNUM)
                                   TO  SPA-N22-COLOR
           MOVE    SPA-N22-TSHAPE(SPA-N22-SELNUM)
                                   TO  SPA-N22-SHAPE
           MOVE    SPA-N22-TMARK(SPA-N22-SELNUM)
                                   TO  SPA-N22-MARK
           MOVE    SPA-N22-TCAUTION(SPA-N22-SELNUM)
                                   TO  SPA-N22-CAUTION
      *----(04.02.01)--ADD-END-----
           .
      *
       4114-COMBOSET-EXT.
           EXIT.
      *
      *****************************************************************
      *     画像セット処理
      *****************************************************************
       4115-GAZOSET-SEC            SECTION.
      *
      *----(04.08.01)-ADD-START---
           INITIALIZE                  SGETTEMP-AREA
           MOVE    WRK-GAZO        TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
      *----(04.08.01)-ADD-END-----
      *----(04.08.01)-UPD-START---
           INITIALIZE                  WRK-GAZOFILE
           STRING  SGETTEMP-TEMPDIR    DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   WRK-CONS-MEDIMGDIR  DELIMITED BY SIZE
                   "/"                 DELIMITED BY SIZE
                   WRK-GAZO            DELIMITED BY SPACE
                   INTO    WRK-GAZOFILE
           END-STRING
      *     STRING  WRK-FILE-FOLDER     DELIMITED BY SIZE
      *             SPA-HOSPNUM         DELIMITED BY SIZE
      *             "/"                 DELIMITED BY SIZE
      *                         INTO    WRK-GAZOFILE
      *     END-STRING
      *     PERFORM VARYING IDY     FROM    128 BY  -1
      *             UNTIL   IDY     <       1
      *         IF  WRK-GAZOFILE(IDY:1)     NOT =   SPACE
      *             COMPUTE IDY     =   IDY +   1
      *             MOVE  WRK-GAZO          TO  WRK-GAZOFILE(IDY:)
      *             MOVE    1               TO  IDY
      *         END-IF
      *     END-PERFORM
      *----(04.08.01)-UPD-END-----
      *
      *    ファイル存在チェック
           INITIALIZE                  ORCSFILECHKAREA
           MOVE    WRK-GAZOFILE    TO  FILECHK-NAME
           CALL    "ORCSFILECHK"       USING
                                       ORCSFILECHKAREA
           IF      FILECHK-RC      NOT =   0
               MOVE    "画像ファイルのパスが正しくありません"
                                       TO  SPA-N22-MESSAGE
               MOVE    "red"           TO  SPA-N22-MESSAGE-STYLE
               MOVE    SPACE           TO  WRK-OBJECT
           ELSE
               MOVE    "BLOBIMPORT"    TO  MCP-FUNC
               MOVE    "blob"          TO  MCP-TABLE
grpsys         MOVE    "key"       TO  MCP-PATHNAME
               MOVE    WRK-GAZOFILE    TO  ORCA-BLOB-FILE
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   ORCA-BLOB  
                                                   SPA-AREA
               MOVE    ORCA-BLOB-OBJECT
                                       TO  WRK-OBJECT
           END-IF
           .
      *
       4115-GAZOSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       450-BACK                    SECTION.
      *
           MOVE    "N21"           TO  SPA-SAKIPG
           MOVE    "N22"           TO  SPA-MOTOPG
      *
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
      *
       450-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
           IF      SPA-N22-MAX     =   ZERO
               MOVE    "該当するデータがありません"
                                   TO  SPA-N22-MESSAGE
               MOVE    "red"       TO  SPA-N22-MESSAGE-STYLE
               GO  TO  480-TOUROKU-EXT
           END-IF
           PERFORM 4111-SELNUM-CHK-SEC
           IF      SPA-N22-MESSAGE NOT =   SPACE
               GO  TO  480-TOUROKU-EXT
           END-IF
           PERFORM 4112-GAZOKBN-CHK-SEC
           IF      SPA-N22-MESSAGE NOT =   SPACE
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 400-GAZOCOPY-SEC
      *
           MOVE    "OK"            TO  SPA-N22-FLG
      *
      *----(04.02.01)--UPD-START---
           EVALUATE    SPA-N22-COPYKBN
               WHEN    "1"
                   IF      SPA-GMN-EFFECT =  SPACE
                       MOVE    SPA-N22-EFFECT
                                       TO  SPA-GMN-EFFECT
                   END-IF
                   IF      SPA-GMN-COLOR  = SPACE
                       MOVE    SPA-N22-COLOR
                                       TO  SPA-GMN-COLOR
                   END-IF
                   IF      SPA-GMN-SHAPE  = SPACE
                       MOVE    SPA-N22-SHAPE
                                       TO  SPA-GMN-SHAPE
                   END-IF
                   IF      SPA-GMN-MARK   = SPACE
                       MOVE    SPA-N22-MARK
                                       TO  SPA-GMN-MARK
                   END-IF
                   IF      SPA-GMN-CAUTION  = SPACE
                       MOVE    SPA-N22-CAUTION
                                       TO  SPA-GMN-CAUTION
                   END-IF
               WHEN     "2"
                   MOVE    SPA-N22-EFFECT
                                       TO  SPA-GMN-EFFECT
                   MOVE    SPA-N22-COLOR
                                       TO  SPA-GMN-COLOR
                   MOVE    SPA-N22-SHAPE
                                       TO  SPA-GMN-SHAPE
                   MOVE    SPA-N22-MARK
                                       TO  SPA-GMN-MARK
                   MOVE    SPA-N22-CAUTION
                                       TO  SPA-GMN-CAUTION
           END-EVALUATE
      *----(04.02.01)--UPD-END-----
           MOVE    SPA-N22-COPYKBN     TO  SPA-N22-COPY
           MOVE    LOW-VALUE           TO  SPA-N21FIL-OBJECT
      *
           PERFORM 450-BACK
           .
      *
       480-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除　処理
      *****************************************************************
       201-DELETE-SEC                  SECTION.
      *
      *    ＪＯＢ管理検索
           MOVE    3               TO  WRK-CONS-JOB-JOBID
           PERFORM 820-JOBKANRI-READ-SEC
      *
           IF    (  SJOBKANRI-RETURN   =   ZERO )
               MOVE    "0001"          TO  SPA-N21IDCD
               MOVE    SPACE           TO  SPA-N22-MESSAGE
               MOVE    SPACE           TO  SPA-N22-MESSAGE-STYLE
           ELSE
               MOVE    "ＪＯＢ管理情報は、ありません"
                                       TO  SPA-N22-MESSAGE
               MOVE    "blue"          TO  SPA-N22-MESSAGE-STYLE
           END-IF
           .
       201-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           IF      SPA-N21ID1-FLG    =   "OK"
      *        ジョブ管理削除
               MOVE    3               TO  WRK-CONS-JOB-JOBID
               PERFORM 820-JOBKANRI-DEL-SEC
               MOVE    4               TO  WRK-CONS-JOB-JOBID
               PERFORM 820-JOBKANRI-DEL-SEC
               MOVE    ZERO            TO  SPA-N22-DURATION
           END-IF
           MOVE    SPACE           TO  SPA-N21ID1-FLG
           MOVE    SPACE           TO  SPA-N21IDCD
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
      *    画面編集処理
           PERFORM 500-GMNHEN-SEC
      *
      *    確認メッセージ表示処理
           IF      SPA-N21IDCD     NOT =   SPACE
               PERFORM 520-N21IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "N22    "       TO  MCP-WINDOW.
      *
      *    ＰＵＴ処理
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
           INITIALIZE                  N22
           MOVE    SPA-N22-KENCD   TO  N22-KENCD
           MOVE    SPA-N22-KENNAME TO  N22-KENNAME
           MOVE    SPA-N22-SELNUM  TO  N22-SELNUM
           MOVE    SPA-N22-MESSAGE TO  N22-MESSAGE
           MOVE    SPA-N22-MESSAGE-STYLE
                                   TO  N22-MESSAGE-STYLE
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       3
               MOVE    SPA-N22-OBJECT(IDX) TO  N22-GAZO(IDX)
               MOVE    SPA-N22-LABEL(IDX)  TO  N22-GAZOLABEL(IDX)
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-N22-MAX
               MOVE    SPA-N22-TNO(IDX)    TO  N22-TNO(IDX)
               MOVE    SPA-N22-TYJCD(IDX)  TO  N22-TYJCD(IDX)
               MOVE    SPA-N22-TGAZO(IDX)  TO  N22-TGAZO(IDX)
               MOVE    SPA-N22-TMAKER(IDX) TO  N22-TMAKER(IDX)
               MOVE    SPA-N22-TSELLER(IDX)
                                           TO  N22-TSELLER(IDX)
               IF      IDX             =   SPA-N22-SELNUM
                   MOVE    "T"         TO  N22-SELECT(IDX)
               ELSE
                   MOVE    "F"         TO  N22-SELECT(IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-N22-MAX     TO  N22-COUNT
      *
           MOVE    SPA-N22-GAZOKBN-G   TO  N22-GAZOKBN
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL   IDX     >       SPA-N22-GAZOKBN-MAX
               MOVE    SPA-N22-GAZOKBNLST(IDX)
                                       TO  N22-GAZOKBNLST(IDX)
           END-PERFORM
           MOVE    SPA-N22-GAZOKBN-MAX TO  N22-GAZOKBN-COUNT
      *
           MOVE    SPA-N22-COPYKBN-G   TO  N22-COPYKBN
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL   IDX     >       SPA-N22-COPYKBN-MAX
               MOVE    SPA-N22-COPYKBNLST(IDX)
                                       TO  N22-COPYKBNLST(IDX)
           END-PERFORM
           MOVE    SPA-N22-COPYKBN-MAX TO  N22-COPYKBN-COUNT
      *
      *----(04.02.01)--ADD-START---
           MOVE    SPA-N22-EFFECT      TO  N22-EFFECT
           MOVE    SPA-N22-COLOR       TO  N22-COLOR
           MOVE    SPA-N22-SHAPE       TO  N22-SHAPE
           MOVE    SPA-N22-MARK        TO  N22-MARK
           MOVE    SPA-N22-CAUTION     TO  N22-CAUTION
           MOVE    SPA-N22-DURATION    TO  N22-DURATION
      *----(04.02.01)--ADD-END-----
      *
           PERFORM 530-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-N21IDSET-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-N21IDMSG
      *
           EVALUATE    SPA-N21IDCD
               WHEN    "0001"
                   MOVE
                   "ＪＯＢ管理情報を削除します。よろしいですか？"
                                           TO  WRK-N21IDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N21ID1
           INITIALIZE                  N21ID1
           MOVE    SPA-N21IDCD     TO  N21ID1-ID1CODE
           MOVE    WRK-N21IDMSG    TO  N21ID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N22"           TO  SPA-MOTOPG
           MOVE    "N21ID1"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N21ID1"        TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-N21IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       530-MAPCUR-SEC              SECTION.
      *
           EVALUATE    SPA-N22-CUR
               WHEN    1
                   MOVE  "SELNUM"      TO  MCP-WIDGET
               WHEN    2
                   MOVE  "GAZOKBN"     TO  MCP-WIDGET
               WHEN    3
                   MOVE  "COPYKBN"     TO  MCP-WIDGET
               WHEN    96
                   MOVE  "B10"         TO  MCP-WIDGET
               WHEN    97
                   MOVE  "B11"         TO  MCP-WIDGET
               WHEN    98
                   MOVE  "B12"         TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B01"         TO  MCP-WIDGET
           END-EVALUATE
           .
      *
       530-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ-SEC       SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ERRCD(1:2)      =   "99"
                   MOVE    SPACE           TO  SKBN-RUN
               ELSE
                   IF      JOB-ERRCD(1:2)      =   "01"
                       MOVE    SPACE           TO  SKBN-RUN
                   ELSE
                       IF     (JOB-ENDTIME     NOT =   SPACE)  AND
                              (JOB-ENDYMD      NOT =   SPACE)
                           MOVE    SPACE           TO  SKBN-RUN
                       ELSE
                           MOVE    "RUN"           TO  SKBN-RUN
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE            TO  SKBN-RUN
           END-IF
           .
      *
       820-JOBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理削除
      *****************************************************************
       820-JOBKANRI-DEL-SEC        SECTION.
      *
      *    ジョブ管理削除
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
      *
       820-JOBKANRI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理作成
      *****************************************************************
       820-JOBKANRI-JBS-SEC        SECTION.
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           EVALUATE    WRK-CONS-JOB-JOBID
               WHEN    3
                   MOVE    "薬剤情報問合せ"
                                   TO  JOB-SHELLMSG
               WHEN    4
                   MOVE    "薬剤情報取込み"
                                   TO  JOB-SHELLMSG
           END-EVALUATE
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
      *
       820-JOBKANRI-JBS-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報マスタ読み込み（キー）
      *****************************************************************
       900-TEMP-SELECT-SEC         SECTION.
      *
           INITIALIZE                  TEMP-REC
grpsys     MOVE    SPA-HOSPNUM     TO  TEMP-HOSPNUM
           MOVE    SPA-TERMID      TO  TEMP-TERMID
           MOVE    SPA-N22-KENCD   TO  TEMP-SRYCD
           MOVE    TEMP-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_yakujyo_temp"  TO  MCP-TABLE
           MOVE    "srycd"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-TEMP
               PERFORM 900-TEMP-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-TEMP
           END-IF
      *
            .
       900-TEMP-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報マスタ読み込み（キー）
      *****************************************************************
       900-TEMP-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_yakujyo_temp"  TO  MCP-TABLE
           MOVE    "srycd"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-TEMP
               MOVE    MCPDATA-REC     TO  TEMP-REC
           ELSE
               MOVE    1               TO  FLG-TEMP
           END-IF
           .
      *
       900-TEMP-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報マスタ読み込み（キー）
      *****************************************************************
       900-TEMP-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_yakujyo_temp"  TO  MCP-TABLE
           MOVE    "srycd"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-TEMP-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
