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
       PROGRAM-ID.                 ORCGXC01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 印刷プレビュー
      *  コンポーネント名  : 印刷プレビュー指定（ＸＣ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/20    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03/09/01    NACL−藤原            メモ欄の全角チェック追加
      *  03/10/27    NACL−藤原            前頁追加
      *  04/02/13    NACL−太田            コラムリスト位置修正
      *  02/05/00    NACL−竹田            データ格納方法変更
      *  02/06/00    NACL−竹田            リアルタイムプレビュー対応
      *  02/06/01    NACL−竹田            最大行の修正
      *  02/06/02    NACL−竹田            タイマーの停止処理
      *  07/06/02    NACL−竹田            タイマーの停止処理
      *  03.05.01    NACL−竹田    07/05/09  グループ診療対応およびオンライン
      *                                      帳票対応
      *  03.05.02    NACL−竹田    07/06/22  労災自賠対応
      *
      *  04.06.01    NACL-藤原     10/10/12  設定による初期表示の変更
      *
      *  04.07.01    NACL-太田     11/11/08  コラムリスト選択行表示制御変更
      *  04.07.02    NACL-藤原     12/03/30  タイトル表示の修正
      *  04.07.03    NACL-藤原    15/07/17  労災診療費請求書及びアフター
      *                                     ケア委託費請求書のＯＣＲ様式
      *                                     （枠なし）対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  05.00.00    NACL-太田     17/11/29  クライアント印刷対応
      *  05.00.03    NACL-藤原     19/03/25  元号改正対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
       01  PREVIEW              PIC X(512).
       01  OTPREVIEW            PIC X(512).
       01  WORK-KEY-AREA.
           03 WRK-TITLE         PIC X(60).
           03 WRK-PTNUM         PIC X(20).
           03 WRK-SRYYM         PIC X(10).
           03 WRK-PRTID         PIC X(20).
      *
      *     レセプト明細ファイル シェル領域 
           COPY    "CPCOMMONSHELL1.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    再印刷処理共通パラメタ
           COPY    "XC01COMMON-SPA".
      *
       01  SPA-XC01.
           03  SPA-XC01-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(05).
               05  SPA-GMN-CUR         PIC 9(02).
               05  SPA-GMN-NEXT        PIC 9(01).
      *
               05  SPA-XC01-HYOUJI     PIC 9(01).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-TITLE               PIC X(40).
                   07  SPA-GMN-TITLE1              PIC X(08).
                   07  SPA-GMN-TITLE2              PIC X(10).
                   07  SPA-GMN-ERRMSG              PIC X(60).
                   07  SPA-GMN-FILENAME            PIC X(200).
      *            07  SPA-GMN-OBJECT              PIC X(36).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-OCC             OCCURS  100.
                           11  SPA-GMN-TNO         PIC 9(05).
                           11  SPA-GMN-TTBL-GROUP  PIC X(14).
                           11  SPA-GMN-TRENNUM     PIC 9(04).
                           11  SPA-GMN-TSHORI-RENNUM
                                                   PIC 9(04).
                           11  SPA-GMN-TPAGE       PIC 9(05).
                           11  SPA-GMN-TSRYYM      PIC X(06).
                           11  SPA-GMN-TSKYYMD     PIC X(09).
                           11  SPA-GMN-TSHORI-TITLE
                                                   PIC X(20).
                           11  SPA-GMN-TPRT-TITLE  PIC X(60).
                           11  SPA-GMN-PTNUM       PIC X(20).
                           11  SPA-GMN-TPRFLG      PIC X(01).
                           11  SPA-GMN-TUPDFLG     PIC X(02).
                           11  SPA-GMN-TPRTID      PIC X(20).
                   07  SPA-GMN-BODY.        
                       09  SPA-GMN-SELNUM          PIC 9(05).
                       09  SPA-GMN-MEMO            PIC X(100).      
                       09  SPA-GMN-BODY1.
                           11  SPA-GMN-TBL-GROUP   PIC X(14).
      *                    11  SPA-GMN-RENNUM      PIC 9(04).
      *                    11  SPA-GMN-SHORI-RENNUM
      *                                            PIC 9(04).
                           11  SPA-GMN-STPAGE      PIC 9(05).
                           11  SPA-GMN-EDPAGE      PIC 9(05).
      *                 
               05  SPA-NAI-AREA.
                   07  SPA-NAI-ENV-FLG             PIC 9(01).      
                   07  SPA-NAI-ENV                 PIC X(200).
                   07  SPA-NAI-BODY.      
                       09  SPA-NAI-SELNUM          PIC 9(05).      
                       09  SPA-NAI-MEMO-FLG        PIC 9(01).      
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-OCC             OCCURS  100.
                           11  SPA-NAI-TBL-KEY     PIC X(08).
                           11  SPA-NAI-RENNUM      PIC 9(04).
                           11  SPA-NAI-MEMO       PIC X(100).      
                           11  SPA-NAI-TBL-GROUP   PIC X(14).
                           11  SPA-NAI-TBL-UUID    PIC X(36).
                           11  SPA-NAI-SHORI-RENNUM
                                                   PIC 9(04).
                           11  SPA-NAI-PAGE        PIC 9(05).
                           11  SPA-NAI-FILENAME    PIC X(200).
                           11  SPA-NAI-PRTNM       PIC X(16).
                           11  SPA-NAI-PTID        PIC 9(10).
                           11  SPA-NAI-MAKEOK-FLG  PIC 9(01).
                           11  SPA-NAI-UPDFLG      PIC X(01).
      *                    11  SPA-NAI-OBJECT      PIC X(08).
      *                     
               05  SPA-WRK-TBL-KEY                 PIC X(08).            
      *                 
      *   
           COPY    "ENUM-VALUE".
      *
           COPY    "CMJOBNAME.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
       01  PS-FILE-ID              PIC X(200).
      *    フラグ領域
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PREVIEW         PIC 9(01).
           03  FLG-PRVKANRI        PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-IN              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-GMN-INIT        PIC 9(01).
           03  FLG-RECEPRT         PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PRVKANRI-TBL-GROUP
                                   PIC X(14).
           03  WRK-PRVKANRI-TBL-UUID
                                   PIC X(36).
           03  WRK-PREVIEW-RENNUM PIC 9(04).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-XCIDMSG         PIC X(70).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.   
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNUM          PIC 9(05). 
           03  WRK-BANGO           PIC 9(05).
           03  WRK-STR             PIC 9(05).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-ZZ              PIC ZZZZ9.
      *
       01  WRK-ROW-AREA.
           03  WRK-ROW             PIC S9(9)   BINARY.
           03  WRK-ROWATTR         PIC S9(9)   BINARY.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
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
       01  WRK-SHELL-NAME.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/allways/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *
       01  WRK-PSFILE-AREA.
             05  WRK-PSFILE-FOLDER     PIC X(165).
             05  WRK-PSFILE-TBL-KEY    PIC X(08).
             05  WRK-PSFILE-RENNUM     PIC X(04).
             05  WRK-PSFILE-GROUP      PIC X(14).
             05  WRK-PSFILE-SRENNUM    PIC X(04).
             05  WRK-PSFILE-PAGE       PIC X(05).
             05  WRK-PSFILE-UUID       PIC X(36).

           COPY    "CPSHELLTBL.INC".
      *     
      *    COPY    "ORCA-DBPATH".
           COPY    "ORCA-BLOB".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".   
      *              
      *
       01  WRK-CONS-AREA.
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PREVIEW-TBL-KEY
                                   PIC X(08)   VALUE   "PREPRINT".
           03  WRK-CONS-PREVIEW-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PREPRINT".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.
      *     
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
      *   ファイル取得関数
       01 ORCFILEGET-AREA.
           03  ORCF-FILE-MODE   PIC X(1).
           03  ORCF-FILE-NAME   PIC X(200).
           03  ORCF-DATA        PIC X(100000).
           03  ORCF-RET         PIC X(2).
       01  ST                PIC 9(2).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *01  PREVIEW-REC.
           COPY    "CPPREVIEW.INC".
      *
      *01  PRVKANRI-REC.
           COPY    "CPPRVKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPRCF008.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSACCESS.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
           COPY    "XC01.INC".
           COPY    "XC02.INC".
           COPY    "XC99.INC".
           COPY    "XCERR.INC".
      *
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
           MOVE    SPA-FREE        TO  SPA-XC01
           MOVE    SPA-WORK        TO  SPA-XC01KYOUTU 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
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
           MOVE    SPA-XC01KYOUTU  TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XC01        TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "XCERR"  OR  "XC02"
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
           MOVE    SPACE               TO  MCP-PUTTYPE
           EVALUATE    SPA-XC01-HYOUJI
               WHEN    1
                   MOVE    "XC01"              TO  MCP-WINDOW
               WHEN    2
                   MOVE    "XC02"              TO  MCP-WINDOW
           END-EVALUATE
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               INITIALIZE              SPA-XC01-AREA
                                       SPA-XC01KYOUTU
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-WORK            TO  LNK-XC01-AREA
               MOVE    LNK-XC01-AREA       TO  SPA-XC01-LN-AREA
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               PERFORM 310-SPASET-SEC
               GO  TO  300-SCREEN-EXT
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "XC99" 
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           INITIALIZE              SPA-XC01-AREA
                                   SPA-XC01KYOUTU
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
           INITIALIZE                  SPA-XC01-AREA
           STRING  MCP-TEMPDIR         DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                                       INTO    SPA-NAI-ENV
           END-STRING
      *
           EVALUATE    SPA-PRTKANRI-TBL-KEY
               WHEN    "SOKATU"
                   MOVE    "診療年月"     TO  SPA-GMN-TITLE1
                   MOVE    "請求年月日"   TO  SPA-GMN-TITLE2
               WHEN    "RECEPT2"
                   MOVE    "診療年月"     TO  SPA-GMN-TITLE1
                   MOVE    "請求年月日"   TO  SPA-GMN-TITLE2
               WHEN    "DAILY"
               WHEN    "MONTHLY"
                   MOVE    SPACE          TO  SPA-GMN-TITLE1
                   MOVE    "作成年月日"   TO  SPA-GMN-TITLE2
               WHEN    OTHER
                   MOVE    "年月"         TO  SPA-GMN-TITLE1
                   MOVE    "年月日"       TO  SPA-GMN-TITLE2
           END-EVALUATE
      *
      *    プレビューDB検索処理
           MOVE    1                      TO  SPA-GMN-PAGE
           PERFORM 2011-PRVKANRI-HEN-SEC
           MOVE    1                      TO  SPA-GMN-CUR
      *
           IF      LNK-XC01-MODE          =   "2"
               MOVE    2                      TO  SPA-XC01-HYOUJI
           ELSE
               MOVE    ZERO                   TO  SPA-XC01-HYOUJI
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    SPA-XC01-HYOUJI
               WHEN    2
                   MOVE    XC02        TO  XC01
           END-EVALUATE
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     終了
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *     状態
               WHEN    "CLICKED"       ALSO    "B02"
               WHEN    "CLICKED"       ALSO    "pandatimer1"
                 IF     SPA-PRTKANRI-TBL-KEY        =  "RECEPTX"
                        INITIALIZE                  SPA-NAI-TBL
      *                 PERFORM VARYING IDZ FROM    1   BY  1
      *                 UNTIL   IDZ >       SPA-GMN-MAX
      *                 MOVE SPACE      TO  SPA-NAI-TBL-KEY      (IDZ)
      *                 MOVE ZERO       TO  SPA-NAI-RENNUM       (IDZ)
      *                 MOVE SPACE      TO  SPA-NAI-TBL-GROUP    (IDZ)
      *                 MOVE ZERO       TO  SPA-NAI-SHORI-RENNUM (IDZ)
      *                 MOVE ZERO       TO  SPA-NAI-PAGE         (IDZ)
      *                 END-PERFORM
                        MOVE    ZERO            TO  SPA-GMN-MAX
                 END-IF
                   PERFORM 2011-PRVKANRI-HEN-SEC
      *     表示切替
               WHEN    "CLICKED"       ALSO    "B03"
                   EVALUATE    SPA-XC01-HYOUJI
                       WHEN    0
                       WHEN    1
                           MOVE    2           TO  SPA-XC01-HYOUJI
                           MOVE    "XC02"      TO  MCP-WINDOW
                       WHEN    2
                           MOVE    1           TO  SPA-XC01-HYOUJI
                           MOVE    "XC01"      TO  MCP-WINDOW
                   END-EVALUATE
      *
                   PERFORM 500-GMNHEN-SEC
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "XC02"              TO  SPA-MOTOPG
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
      *     訂正
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-MAKE-UPD-SEC
      *
      *     前行
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 480-BEFORELINE-SEC
      *     前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-MAEGMN-SEC
      *     次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *     次行
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-NEXTLINE-SEC
      *     印刷
               WHEN    "CLICKED"       ALSO    "B12"
      *            PERFORM 450-PRINT-GID-SEC
                   PERFORM 450-PRINT-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    SPA-XC01-HYOUJI
               WHEN    2
                   MOVE    XC02        TO  XC01
           END-EVALUATE
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 201-SELNUM-SEC
      *        メモ入力
               WHEN    "ACTIVATE"      ALSO    "MEMO"
                   PERFORM 202-MEMO-SEC
      *        行選択
               WHEN    ANY             ALSO    "CLIST"
                   PERFORM 203-SELECT-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       201-SELNUM-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-BODY
      *      
           MOVE    XC01-SELNUM         TO  SPA-GMN-SELNUM
      *     
           IF      SPA-GMN-SELNUM      =   ZERO
               CONTINUE
           ELSE
               COMPUTE WRK-BANGO       =   SPA-GMN-SELNUM
                                       - ( SPA-GMN-PAGE    -   1 )
                                       *   WRK-CONS-LINE-MAX
      *
               IF      WRK-BANGO       >   SPA-GMN-MAX
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-BANGO       TO  SPA-NAI-SELNUM     
                   PERFORM 2011-SELECT-HENSYU-SEC
               END-IF    
           END-IF
           .
       201-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ入力処理
      *****************************************************************
       202-MEMO-SEC                   SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE   SPA-NAI-SELNUM       TO  IDX
      *
           MOVE    XC01-MEMO           TO  SPA-GMN-MEMO
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    SPA-GMN-MEMO        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   "2"
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEMO
           ELSE
               MOVE    "0007"              TO  SPA-ERRCD
               GO  TO  202-MEMO-EXT
           END-IF
      *
           MOVE    SPA-GMN-MEMO        TO  SPA-NAI-MEMO(IDX)
      *
           INITIALIZE                  PRVKANRI-REC
           MOVE    SPA-NAI-TBL-KEY (IDX)
                                       TO  PRVKANRI-TBL-KEY
           MOVE    SPA-NAI-RENNUM (IDX)
                                       TO  PRVKANRI-RENNUM
           MOVE    SPA-NAI-TBL-GROUP (IDX)
                                       TO  PRVKANRI-TBL-GROUP
           MOVE    SPA-NAI-TBL-UUID  (IDX)
                                       TO  PRVKANRI-TBL-UUID
           MOVE    SPA-NAI-SHORI-RENNUM (IDX)
                                       TO  PRVKANRI-SHORI-RENNUM
           MOVE    SPA-NAI-PAGE(IDX)   TO  PRVKANRI-PAGE
           MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-PRVKANRI-INV-SEC
           MOVE    MCPDATA-REC         TO  PRVKANRI-REC
           MOVE    SPA-NAI-UPDFLG(IDX) TO  PRVKANRI-MOD-FLG
           MOVE    SPA-NAI-MEMO(IDX)   TO  PRVKANRI-MEMO
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           PERFORM 4401-PRVKANRI-UPD-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       202-MEMO-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者検索処理
      *****************************************************************
       2011-SELECT-HENSYU-SEC        SECTION.
      *
      *    IF      WRK-BANGO           =   ZERO
      *            MOVE  1             TO  WRK-BANGO
      *    END-IF
           MOVE    WRK-BANGO                TO  IDY
      *     
           MOVE    SPA-NAI-FILENAME (IDY)   TO  SPA-GMN-FILENAME 
      *    MOVE    SPA-NAI-OBJECT   (IDY)   TO  SPA-GMN-OBJECT
      **** MOVE    SPA-GMN-TSHORI-RENNUM (IDY) TO
      ****                                     SPA-NAI-SHORI-RENNUM
      *                                 
           MOVE    SPACE               TO  SPA-GMN-ERRMSG
           MOVE    SPACE               TO  PREVIEW
           MOVE    SPA-NAI-FILENAME (IDY)
                                       TO  WRK-PSFILE-AREA
           STRING  SPA-NAI-ENV         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SPACE
                   WRK-PSFILE-TBL-KEY  DELIMITED  BY  SPACE
                   WRK-PSFILE-RENNUM   DELIMITED  BY  SPACE
                   WRK-PSFILE-GROUP    DELIMITED  BY  SPACE
                   WRK-PSFILE-SRENNUM  DELIMITED  BY  SPACE
                   WRK-PSFILE-PAGE     DELIMITED  BY  SPACE
                   WRK-PSFILE-UUID     DELIMITED  BY  SPACE
                   ".pdf"              DELIMITED  BY  SIZE
                                       INTO   PREVIEW
           END-STRING
      *                                 
           PERFORM 460-FILE-HENSYU-SEC
           MOVE    SPA-NAI-SELNUM      TO  IDX
           PERFORM 2013-PREVIEW-CHECK-SEC
           MOVE    SPA-NAI-FILENAME (IDY)
                                       TO  SPA-GMN-FILENAME
      *    MOVE    SPA-NAI-OBJECT (IDY)
      *                                TO  SPA-GMN-OBJECT
      *
           IF      SPA-NAI-UPDFLG(IDY)      =   "1"
               MOVE    SPA-NAI-MEMO (IDY)
                                        TO  SPA-GMN-MEMO
               MOVE    1                TO  SPA-NAI-MEMO-FLG
               MOVE    2                TO  SPA-GMN-CUR 
           ELSE
               MOVE    SPACE            TO  SPA-GMN-MEMO         
               MOVE    ZERO             TO  SPA-NAI-MEMO-FLG
               MOVE    1                TO  SPA-GMN-CUR 
           END-IF
      *
           .
       2011-SELECT-HENSYU-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       203-SELECT-SEC               SECTION.
      *          
           MOVE    99                  TO  SPA-GMN-CUR         
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      XC01-SELECT (IDX)   =   "T"
                   IF      IDX             =   SPA-NAI-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI-SELNUM
           END-IF
      *
           INITIALIZE                  SPA-GMN-BODY
      *
           MOVE    SPA-GMN-TNO (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-SELNUM
           MOVE    SPA-NAI-SELNUM      TO  WRK-BANGO
      *                                 
           PERFORM 2011-SELECT-HENSYU-SEC
      *
           .
       203-SELECT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    訂正処理
      *****************************************************************
       440-MAKE-UPD-SEC            SECTION.
      *
           IF      SPA-NAI-SELNUM      =  ZERO
               GO  TO  440-MAKE-UPD-EXT
           END-IF
      *
           MOVE    SPA-NAI-SELNUM      TO  WRK-BANGO
           PERFORM 2011-SELECT-HENSYU-SEC
           IF      SPA-ERRCD       NOT =  SPACE
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  440-MAKE-UPD-EXT
           END-IF
      *
           MOVE    SPA-NAI-SELNUM      TO  IDX
           PERFORM 2013-PREVIEW-CHECK-SEC
           IF      SPA-GMN-ERRMSG  NOT =   SPACE                
               GO  TO  440-MAKE-UPD-EXT
           END-IF
      *
           IF      XC01-SELECT (SPA-NAI-SELNUM)   =   "T"
              IF   SPA-GMN-TUPDFLG (SPA-NAI-SELNUM)   =   "○"
                   MOVE SPACE          TO
                                       SPA-GMN-TUPDFLG(SPA-NAI-SELNUM)
                   MOVE SPACE          TO
                                       SPA-NAI-UPDFLG(SPA-NAI-SELNUM)
                   MOVE ZERO           TO
                                       SPA-NAI-MEMO-FLG
                   MOVE SPACE          TO
                                       SPA-NAI-MEMO(SPA-NAI-SELNUM)
                                       SPA-GMN-MEMO
                   MOVE    1           TO  SPA-GMN-CUR
              ELSE
                   MOVE "○"           TO
                                       SPA-GMN-TUPDFLG(SPA-NAI-SELNUM)
                   MOVE "1"            TO
                                       SPA-NAI-UPDFLG(SPA-NAI-SELNUM)
                   MOVE 1              TO
                                       SPA-NAI-MEMO-FLG
                   MOVE 2              TO  SPA-GMN-CUR
              END-IF
           END-IF
           MOVE   SPA-NAI-SELNUM       TO  IDX
           INITIALIZE                  PRVKANRI-REC
           MOVE    SPA-NAI-TBL-KEY (IDX)
                                       TO  PRVKANRI-TBL-KEY
           MOVE    SPA-NAI-RENNUM (IDX)
                                       TO  PRVKANRI-RENNUM
           MOVE    SPA-NAI-TBL-GROUP (IDX)
                                       TO  PRVKANRI-TBL-GROUP
           MOVE    SPA-NAI-TBL-UUID  (IDX)
                                       TO  PRVKANRI-TBL-UUID
           MOVE    SPA-NAI-SHORI-RENNUM (IDX)
                                       TO  PRVKANRI-SHORI-RENNUM
           MOVE    SPA-NAI-PAGE(IDX)   TO  PRVKANRI-PAGE
           MOVE    SPA-NAI-MEMO(IDX)   TO  PRVKANRI-MEMO
           MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-PRVKANRI-INV-SEC
           MOVE    MCPDATA-REC         TO  PRVKANRI-REC
           MOVE    SPA-NAI-UPDFLG(IDX) TO  PRVKANRI-MOD-FLG
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           PERFORM 4401-PRVKANRI-UPD-SEC
           .
       440-MAKE-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       2014-PRV-HENSYU-SEC         SECTION.
           IF      SPA-NAI-MAKEOK-FLG (IDX)    =   ZERO
               PERFORM   20121-PREVIEW-EXIST-SEC
             IF    FLG-PREVIEW           =   1  
      *             再印刷以外からの遷移
                   PERFORM   20122-PSFILE-EXIST-SEC
             END-IF
           END-IF
           .
       2014-PRV-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       460-FILE-HENSYU-SEC         SECTION.
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
      ******** PERFORM 2013-PREVIEW-CHECK-SEC
               PERFORM 2014-PRV-HENSYU-SEC
      *        
           END-PERFORM
           .
       460-FILE-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    リアルタイムプレビュー時は、中間ファイル削除
           IF      SPA-PRTKANRI-TBL-KEY    =  "RECEPTX" OR "ONLINE"
               PERFORM 2101-FILE-DEL-SEC         
           END-IF    
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル削除　処理
      *****************************************************************
       2101-FILE-DEL-SEC               SECTION.
      *
      *    psファイルクリア
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF    SPA-NAI-FILENAME (IDX)
                                       NOT =   SPACE
                   MOVE    ZERO            TO  IF-RESULT
                   MOVE    SPA-NAI-FILENAME (IDX)
                                           TO  IF-FILENAME
                   CALL    "orcfiledel"    USING
                                           ORCSFDELAREA
               END-IF    
           END-PERFORM
           IF      SPA-PRTKANRI-TBL-KEY    =  "ONLINE"
                   GO      TO  2101-FILE-DEL-EXT
           END-IF
           MOVE    SPACE               TO  WRK-PTNUM
           MOVE    SPA-GMN-PTNUM(1)    TO  WRK-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      WRK-PTNUM (IDX:1)   NOT =   SPACE
                   MOVE    1               TO  IDX
               ELSE
                   MOVE    "Z"             TO  WRK-PTNUM (IDX:1)
               END-IF
           END-PERFORM
      *
           EVALUATE    LNK-XC01-HKNKBN
               WHEN    "0"
                   MOVE    "HC06"          TO  RECE08-PRTID
               WHEN    "1"
                   MOVE    "HC21"          TO  RECE08-PRTID
               WHEN    "2"
                   MOVE    "HC23"          TO  RECE08-PRTID
               WHEN    "3"
                   MOVE    "HC25"          TO  RECE08-PRTID
           END-EVALUATE
           MOVE    "999999"            TO  RECE08-RECEYM
           MOVE    WRK-PTNUM           TO  RECE08-PTNUM
           MOVE    SPA-HOSPNUM         TO  RECE08-HOSPNUM
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM 900-RECEPRT-INV-SEC
           IF      FLG-RECEPRT         =   1
                   GO        TO        2101-FILE-DEL-EXT
           END-IF
           MOVE    "999999"            TO  RECE08-RECEYM
           MOVE    WRK-PTNUM           TO  RECE08-PTNUM
           MOVE    SPA-HOSPNUM         TO  RECE08-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    RECE08-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 RECEPRT DELETE ERR [" MCP-RC "]" 
           END-IF
      *
           MOVE    "RECEPTX"           TO  PRTDATA-TBL-KEY
           MOVE    LNK-XC01-TBL-GROUP  TO  PRTDATA-TBL-GROUP
           MOVE    LNK-XC01-TBL-UUID   TO  PRTDATA-TBL-UUID
           MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 PRTDATA DELETE ERR [" MCP-RC "]" 
           END-IF
      *
           MOVE    "RECEPTX"           TO  PRTKANRI-TBL-KEY
           MOVE    LNK-XC01-TBL-GROUP  TO  PRTKANRI-TBL-GROUP
           MOVE    LNK-XC01-TBL-UUID   TO  PRTKANRI-TBL-UUID
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 PRTKANRI DELETE ERR [" MCP-RC "]" 
           END-IF
      *
           MOVE    "RECEPTX"           TO  PRVKANRI-TBL-KEY
           MOVE    LNK-XC01-TBL-GROUP  TO  PRVKANRI-TBL-GROUP
           MOVE    LNK-XC01-TBL-UUID   TO  PRVKANRI-TBL-UUID
           MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "del13"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 PRVKANRI DELETE ERR [" MCP-RC "]" 
           END-IF
      *
           .
       2101-FILE-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    SPA-MOTOPG2         TO  SPA-SAKIPG
           MOVE    "XC01"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  SPA-WORK
           MOVE    SPA-XC01-LN-AREA    TO  SPA-WORK 
      **** INITIALIZE                  SPA-KYOTUKEY
      ****                             SPA-KYOTU-SET
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
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
      *    前行　処理
      *****************************************************************
       480-BEFORELINE-SEC                SECTION.
      *
      *    行選択
           IF     (SPA-NAI-SELNUM      =   1)
               IF    ( SPA-GMN-PAGE    =   1 )
                   GO                  TO  480-BEFORELINE-EXT
               END-IF
               PERFORM 440-MAEGMN-SEC
               MOVE    WRK-CONS-LINE-MAX   TO  SPA-NAI-SELNUM
               MOVE    SPA-GMN-TNO (SPA-NAI-SELNUM)
                                           TO  SPA-GMN-SELNUM
               MOVE    SPA-NAI-SELNUM      TO  WRK-BANGO
      *                                 
               PERFORM 2011-SELECT-HENSYU-SEC
               MOVE    2               TO  FLG-GMN-INIT
                   GO                  TO  480-BEFORELINE-EXT
           END-IF
      *     
           IF      SPA-NAI-SELNUM      =   ZERO
                   MOVE  "T"           TO  XC01-SELECT (1)
                   MOVE   1            TO  SPA-NAI-SELNUM
           ELSE
                   MOVE  "F"           TO  XC01-SELECT (SPA-NAI-SELNUM)
                   COMPUTE SPA-NAI-SELNUM  =  SPA-NAI-SELNUM  -  1
                   MOVE  "T"           TO  XC01-SELECT (SPA-NAI-SELNUM)
           END-IF
      *    検索処理
           PERFORM 203-SELECT-SEC
      *
           .
       480-BEFORELINE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次行　処理
      *****************************************************************
       470-NEXTLINE-SEC                SECTION.
      *
      *    行選択
           IF     (SPA-GMN-MAX         <=  SPA-NAI-SELNUM) 
               IF    ( SPA-GMN-NEXT     =   ZERO )
                   GO                  TO  470-NEXTLINE-EXT
               END-IF
               PERFORM 440-ATOGMN-SEC
                   GO                  TO  470-NEXTLINE-EXT
           END-IF
      *     
           IF      SPA-NAI-SELNUM      =   ZERO
                   MOVE  "T"           TO  XC01-SELECT (1)
                   MOVE   1            TO  SPA-NAI-SELNUM
           ELSE
                   MOVE  "F"           TO  XC01-SELECT (SPA-NAI-SELNUM)
                   COMPUTE SPA-NAI-SELNUM  =  SPA-NAI-SELNUM  +  1
                   MOVE  "T"           TO  XC01-SELECT (SPA-NAI-SELNUM)
           END-IF
      *    検索処理
           PERFORM 203-SELECT-SEC
      *
           .
       470-NEXTLINE-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       440-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE        =   1
               GO      TO      440-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    -   1
      *
           INITIALIZE                  SPA-GMN-TBL
                                       SPA-GMN-BODY
                                       SPA-GMN-FILENAME
      *                                SPA-GMN-OBJECT
           INITIALIZE                  SPA-NAI-BODY
                                       SPA-NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX
      *    検索処理
           PERFORM 2011-PRVKANRI-HEN-SEC
      *
           .
       440-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-NEXT        =   ZERO
               GO      TO      440-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
      *
           INITIALIZE                  SPA-GMN-TBL
                                       SPA-GMN-BODY
                                       SPA-GMN-FILENAME
      *                                SPA-GMN-OBJECT
           INITIALIZE                  SPA-NAI-BODY
                                       SPA-NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX
      *    検索処理
           PERFORM 2011-PRVKANRI-HEN-SEC
      *
           .
       440-ATOGMN-EXT.
           EXIT.
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-PRINT-GID-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD                 
      *     
      *    IF      SPA-GMN-TBL-GROUP   =   SPACE
      *        MOVE    "0001"              TO  SPA-ERRCD
      *    ELSE
      *        IF      SPA-GMN-STPAGE      >
      *                            SPA-GMN-TPAGE (SPA-GMN-SELNUM) 
      *        OR      SPA-GMN-EDPAGE      >
      *                            SPA-GMN-TPAGE (SPA-GMN-SELNUM) 
      *            MOVE    "0002"              TO  SPA-ERRCD
      *        ELSE    
      *            IF      SPA-GMN-EDPAGE      <   SPA-GMN-STPAGE 
      *                MOVE    "0002"              TO  SPA-ERRCD
      *            END-IF
      *        END-IF
      *    END-IF                                            
      *
           IF      SPA-ERRCD       NOT =   SPACE                 
               GO  TO  450-PRINT-GID-EXT               
           END-IF
      *         
      *    プレビューＤＢのチェック
           INITIALIZE                      PREVIEW-REC
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PREVIEW-TBL-KEY
               MOVE    1                   TO  FLG-PREVIEW
      *    指定のグループが存在しないとき
           IF      FLG-PREVIEW        =   1
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO  450-PRINT-GID-EXT   
           ELSE
               MOVE    PREVIEW-TBL-KEY    TO  SPA-WRK-TBL-KEY    
           END-IF
      *     
      *    ガイドメッセージセット
           MOVE    "1001"                 TO  SPA-XCIDCD
      *
      *    未処理の帳票があるとき
           PERFORM                     UNTIL   FLG-PREVIEW    =   1
               PERFORM 900-PREVIEW-FET2-SEC
           END-PERFORM                   
      *                  
           .
       450-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       450-PRINT-SEC                   SECTION.
      * 
           IF (    SPA-PRTKANRI-TBL-KEY (1:6)  =   "RECEPT"      )
           OR (  ( SPA-PRTKANRI-TBL-KEY (1:6)  =   "SOKATU"    )
               AND
                 ( SPA-GMN-TPRTID(SPA-NAI-SELNUM)
                                               =   "HCM79.red"
                                               OR  "HCM81.red"
                                               OR  "HCM85.red"
                                               OR  "HCM86.red"
                                               OR  "HCM89.red" ) )
                   PERFORM 4501-PRINT-SEC
                   GO      TO          450-PRINT-EXT
           END-IF
      * 
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
                                           WRK-PREVIEW-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PREVIEW-TBL-GROUP                                
      *
      *    印刷処理
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "PVERRORR"          TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "preprint.sh"       TO  SHELLTBL-NAME
      *     
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    SPA-WRK-TBL-KEY     TO  LNK-PRTKANRI-TBL-KEY  
           MOVE    SPA-NAI-TBL-GROUP (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-TBL-UUID  (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-TBL-UUID
           MOVE    SPA-NAI-RENNUM    (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-RENNUM
           MOVE    SPA-NAI-SHORI-RENNUM (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "preprint.sh"       TO  LNK-PRTKANRI-SHELLID
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11      
           MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
      *    MOVE    SPA-GMN-STPAGE      TO  SHELLTBL-ARG15
      *    MOVE    SPA-GMN-EDPAGE      TO  SHELLTBL-ARG16                      
           IF      SPA-XCID1-FLG    =   "OK"
      *      MOVE    "/tmp/"
             MOVE  SPA-NAI-ENV 
                                       TO  OTPREVIEW
             MOVE    WRK-CONS-JOB-SHELLID
                                       TO  OTPREVIEW(6:)
             MOVE  SPA-NAI-ENV 
                                       TO  SHELLTBL-ARG15
             MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG15(6:)
             MOVE    SPA-NAI-PRTNM    (1)
                                       TO  SHELLTBL-ARG16                                 
           ELSE
             MOVE    SPA-NAI-FILENAME (SPA-NAI-SELNUM)
                                       TO  SHELLTBL-ARG15
             MOVE    SPA-NAI-PRTNM    (SPA-NAI-SELNUM)
                                       TO  SHELLTBL-ARG16                                 
           END-IF
           IF    SPA-CLIENT-PRT-FLG  =  2
               MOVE    "1"             TO  SHELLTBL-ARG17
           END-IF
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
      *    情報確認画面を表示                                                   
           .
       450-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4501-PRINT-SEC                  SECTION.
      * 
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    印刷処理
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "PVERRORR"          TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      *     
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    SPA-PRTKANRI-TBL-KEY    TO  LNK-PRTKANRI-TBL-KEY  
           MOVE    SPA-NAI-TBL-GROUP (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-TBL-UUID  (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-TBL-UUID
           MOVE    SPA-NAI-RENNUM    (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-RENNUM
           MOVE    SPA-NAI-SHORI-RENNUM (SPA-NAI-SELNUM)
                                       TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11      
      *                 
      *                 
           MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
           MOVE    SPA-NAI-PAGE (SPA-NAI-SELNUM)
                                       TO  SHELLTBL-ARG15
           MOVE    SPA-NAI-PAGE (SPA-NAI-SELNUM)
                                       TO  SHELLTBL-ARG16
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
               MOVE    "5"             TO  SHELLTBL-ARG17
           ELSE
               MOVE    "1"             TO  SHELLTBL-ARG17
           END-IF
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
           MOVE    "1"                 TO  SHELLTBL-ARG23
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"
                                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       4501-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューDB検索処理
      *****************************************************************
       2011-PREVIEW-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-AREA
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX
      *
      *    検索
           INITIALIZE                  PREVIEW-REC
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PREVIEW-TBL-KEY
               MOVE    1                   TO  FLG-PREVIEW
      *
           IF     (SPA-GMN-NEXT        =   1  )  AND
                  (SPA-GMN-PAGE        >   1  )
               COMPUTE WRK-STR         =   (SPA-GMN-PAGE  -  1)
                                       *   WRK-CONS-LINE-MAX
                                       +   1
           ELSE
               MOVE    1               TO  WRK-STR
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
           PERFORM UNTIL   (IDX        >   WRK-CONS-LINE-MAX ) OR
                           (FLG-PREVIEW  =   1         )
      *
               ADD     1               TO  IDX2
               IF      IDX2            >=  WRK-STR
                       PERFORM 2012-PRVKANRI-HENSYU-SEC
               END-IF
      *
               PERFORM 900-PREVIEW-FET-SEC
           END-PERFORM
      * 
           IF     (IDX             >   WRK-CONS-LINE-MAX  )  AND
                  (FLG-PREVIEW       =   ZERO )
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           .
       2011-PREVIEW-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューDB検索処理
      *****************************************************************
       2011-PRVKANRI-HEN-SEC        SECTION.
      *
      *    検索
           INITIALIZE                  PRVKANRI-REC
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
           MOVE    SPA-XC01-LN-TBL-GROUP
                                       TO  PRVKANRI-TBL-GROUP
           MOVE    SPA-XC01-LN-TBL-UUID
                                       TO  PRVKANRI-TBL-UUID
           MOVE    SPA-XC01-LN-SHORI-RENNUM
                                       TO  PRVKANRI-SHORI-RENNUM
           MOVE    SPA-XC01-LN-RENNUM  TO  PRVKANRI-RENNUM
           MOVE    SPA-XC01-LN-ST-PAGE TO  PRVKANRIXX-STPAGE
           MOVE    SPA-XC01-LN-ED-PAGE TO  PRVKANRIXX-EDPAGE
           MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           IF      SPA-XC01-LN-SHORI-RENNUM
                                       NOT =   ZERO
               MOVE    "key3"
                                           TO  WRK-MCP-PATHNAME
           ELSE
               IF     SPA-XC01-LN-RENNUM   NOT =   ZERO
                   MOVE   "key7" 
                                               TO  WRK-MCP-PATHNAME
               ELSE
                   MOVE   "key6" 
                                               TO  WRK-MCP-PATHNAME
               END-IF
           END-IF
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRVKANRI-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
      *
      *
           COMPUTE WRK-STR         = ( SPA-GMN-PAGE  -  1 )
                                   *   WRK-CONS-LINE-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
           PERFORM UNTIL ( IDX             >=  WRK-CONS-LINE-MAX ) OR
                         ( FLG-PRVKANRI    =   1                 )
      *
               ADD     1               TO  IDX2
               IF      IDX2            >=  WRK-STR
                       PERFORM 2012-PRVKANRI-HENSYU-SEC
               END-IF
      *
               PERFORM 900-PRVKANRI-FET-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF     (IDX             >=  WRK-CONS-LINE-MAX  )  AND
                  (FLG-PRVKANRI     =   ZERO              )
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           IF      SPA-NAI-SELNUM  =   1
                   MOVE  ZERO          TO  SPA-NAI-SELNUM
           END-IF
      *
           IF      SPA-NAI-SELNUM  =   ZERO    
               MOVE    1                   TO  SPA-NAI-SELNUM
               MOVE    SPA-GMN-TNO (SPA-NAI-SELNUM)
                                           TO  SPA-GMN-SELNUM
               MOVE    SPA-NAI-SELNUM      TO  WRK-BANGO
      *                                 
               PERFORM 2011-SELECT-HENSYU-SEC
           END-IF
      *
           .
       2011-PRVKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューDB編集処理
      *****************************************************************
       2012-PRVKANRI-HENSYU-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
      *     SPA-NAI-MAKEOK-FLG (IDX)
      *    既にＳＰＡに存在するかチェック
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       SPA-GMN-MAX
               IF    ( PRVKANRI-TBL-KEY        =
                                           SPA-NAI-TBL-KEY      (IDZ) )
               AND   ( PRVKANRI-RENNUM         =
                                           SPA-NAI-RENNUM       (IDZ) )
               AND   ( PRVKANRI-TBL-GROUP      =
                                           SPA-NAI-TBL-GROUP    (IDZ) )
               AND   ( PRVKANRI-TBL-UUID       =
                                           SPA-NAI-TBL-UUID     (IDZ) )
               AND   ( PRVKANRI-SHORI-RENNUM   =
                                           SPA-NAI-SHORI-RENNUM (IDZ) )
               AND   ( PRVKANRI-PAGE           =  
                                           SPA-NAI-PAGE         (IDZ) )
                   MOVE    1               TO  FLG-CHK
                   MOVE    IDZ             TO  IDX
                   MOVE    SPA-GMN-MAX     TO  IDZ
               END-IF
           END-PERFORM
      *
      *    ＳＰＡに追加
           IF      FLG-CHK             =   ZERO
               COMPUTE IDX     =   SPA-GMN-MAX     +   1
               IF      IDX                 <=  WRK-CONS-LINE-MAX
                   MOVE    IDX2                TO  SPA-GMN-TNO(IDX)
      *
                   MOVE    PRVKANRI-TBL-KEY    TO
                                           SPA-NAI-TBL-KEY   (IDX)
                   
                   MOVE    PRVKANRI-RENNUM     TO  
                                           SPA-NAI-RENNUM   (IDX)
                   MOVE    PRVKANRI-TBL-GROUP  TO  
                                           SPA-NAI-TBL-GROUP (IDX)
                   MOVE    PRVKANRI-TBL-UUID   TO  
                                           SPA-NAI-TBL-UUID  (IDX)
                   MOVE    PRVKANRI-SHORI-RENNUM TO  
                                           SPA-NAI-SHORI-RENNUM (IDX)
                   MOVE    PRVKANRI-PAGE       TO  
                                           SPA-NAI-PAGE (IDX)
                   MOVE    PRVKANRI-TITLE      TO
                                           SPA-GMN-TPRT-TITLE (IDX)
                   MOVE    PRVKANRI-MEMO       TO
                                           SPA-NAI-MEMO (IDX)
                   MOVE    PRVKANRI-FILENAME   TO  WRK-PSFILE-AREA
                   STRING  SPA-NAI-ENV         DELIMITED  BY  SPACE
                          SPA-HOSPNUM          DELIMITED  BY  SPACE
                          WRK-PSFILE-TBL-KEY   DELIMITED  BY  SPACE
                          WRK-PSFILE-RENNUM    DELIMITED  BY  SPACE
                          WRK-PSFILE-GROUP     DELIMITED  BY  SPACE
                          WRK-PSFILE-SRENNUM   DELIMITED  BY  SPACE
                          WRK-PSFILE-PAGE      DELIMITED  BY  SPACE
                          PRVKANRI-TBL-UUID    DELIMITED  BY  SPACE
                          ".pdf"               DELIMITED  BY  SIZE
                                       INTO    SPA-NAI-FILENAME (IDX)
                   END-STRING
                   MOVE    PRVKANRI-PRTNM      TO  SPA-NAI-PRTNM (IDX)
                   MOVE    PRVKANRI-PTID       TO  SPA-NAI-PTID  (IDX)
                   MOVE    PRVKANRI-MOD-FLG    TO  SPA-NAI-UPDFLG (IDX)
                   IF      SPA-NAI-UPDFLG (IDX) =  "1"
                     MOVE  "○"                TO  SPA-GMN-TUPDFLG (IDX)
                   END-IF
                   MOVE    PRVKANRI-PRTID      TO
                                           SPA-GMN-TPRTID (IDX)
      *
                   INITIALIZE                      ORCSPTIDAREA
      *            MOVE    SPA-HOSPID          TO  SPTID-HOSPID
                   MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                   MOVE    PRVKANRI-PTID       TO  SPTID-PTID
                   CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                   SPA-AREA
                   IF      SPTID-RC        NOT =   ZERO
                       MOVE    SPACE           TO  SPA-GMN-PTNUM  (IDX)
                   ELSE
                       MOVE    SPTID-PTNUM     TO  SPA-GMN-PTNUM  (IDX)
                   END-IF
      *
      *            IF      PRVKANRI-TBL-GROUP  NOT =
      *                                        WRK-PRVKANRI-TBL-GROUP
                       MOVE    PRVKANRI-TBL-GROUP   TO
      *                                        SPA-NAI-TBL-GROUP (IDX)
                                               WRK-PRVKANRI-TBL-GROUP
                       MOVE    PRVKANRI-TBL-UUID    TO
      *                                        SPA-NAI-TBL-UUID  (IDX)
                                               WRK-PRVKANRI-TBL-UUID
                       IF      PRVKANRI-SRYYM  NOT =   SPACE   AND ZERO
                           MOVE    PRVKANRI-SRYYM  TO  WRK-SYMD (1:6)
                           MOVE    "01"            TO  WRK-SYMD (7:2)
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    WRK-HENYMDG(1:6)
                                               TO  SPA-GMN-TSRYYM (IDX)
                       END-IF
                       IF      PRVKANRI-SKYYMD NOT =   SPACE   AND ZERO
                           MOVE    PRVKANRI-SKYYMD TO  WRK-SYMD 
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    WRK-HENYMDG     TO
                                                   SPA-GMN-TSKYYMD(IDX)
                       END-IF
                       SET     TBL-IDX             TO  1
                       SEARCH      MNAME-TBL-OCC   VARYING   TBL-IDX
                           AT   END
                               MOVE    SPACE       TO
                                           SPA-GMN-TSHORI-TITLE (IDX)
                           WHEN    PRVKANRI-TBL-KEY    =   MNAME-SHELLID
                                                          (TBL-IDX)
                               MOVE    MNAME-JOBNAME (TBL-IDX)
                                                TO  SPA-GMN-TSHORI-TITLE
                                                           (IDX)
                       END-SEARCH
      *            END-IF
      *
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-IF
      *
      *    未処理のときプレビューＤＢより編集
           IF      IDX                 <=  WRK-CONS-LINE-MAX
             IF      SPA-NAI-MAKEOK-FLG (IDX)    =   ZERO
               PERFORM   20121-PREVIEW-EXIST-SEC
               IF    FLG-PREVIEW           =   1  
      *    再印刷以外からの遷移
                   PERFORM   20122-PSFILE-EXIST-SEC
               END-IF
             END-IF
           END-IF
      *           
           .
       2012-PRVKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューpsファイル編集処理
      *****************************************************************
       20122-PSFILE-EXIST-SEC      SECTION.
      *
      *    MOVE   PRVKANRI-FILENAME    TO  WRK-PSFILE-AREA
      *
           MOVE   SPA-NAI-TBL-KEY (IDX)    TO  WRK-PSFILE-TBL-KEY
           MOVE   SPA-NAI-RENNUM    (IDX)  TO  WRK-PSFILE-RENNUM
           MOVE   SPA-NAI-TBL-GROUP (IDX)  TO  WRK-PSFILE-GROUP
           MOVE   SPA-NAI-TBL-UUID  (IDX)  TO  WRK-PSFILE-UUID
           MOVE   SPA-NAI-SHORI-RENNUM (IDX)
                                           TO  WRK-PSFILE-SRENNUM
           MOVE   SPA-NAI-PAGE (IDX)       TO  WRK-PSFILE-PAGE
      *
           MOVE    SPACE               TO  ORCF-FILE-NAME
           STRING  SPA-NAI-ENV         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SPACE
                   WRK-PSFILE-TBL-KEY  DELIMITED  BY  SPACE
                   WRK-PSFILE-RENNUM   DELIMITED  BY  SPACE
                   WRK-PSFILE-GROUP    DELIMITED  BY  SPACE
                   WRK-PSFILE-SRENNUM  DELIMITED  BY  SPACE
                   WRK-PSFILE-PAGE     DELIMITED  BY  SPACE
                   WRK-PSFILE-UUID     DELIMITED  BY  SPACE
                   ".pdf"              DELIMITED  BY  SIZE
                                       INTO   ORCF-FILE-NAME
           END-STRING
           MOVE    "r"                 TO  ORCF-FILE-MODE
           CALL    "orcfileio"         USING ST  ORCFILEGET-AREA
           IF      ORCF-RET            =   ZERO
                   MOVE  1             TO
                                       SPA-NAI-MAKEOK-FLG (IDX)
                   MOVE   "*"          TO
                                        SPA-GMN-TPRFLG     (IDX)
      *            MOVE    ORCF-FILE-NAME      TO  ORCA-BLOB-FILE
      *            MOVE    "BLOBIMPORT"      TO  MCP-FUNC
      *            MOVE    "blob"            TO  MCP-TABLE
      *            MOVE    "key"             TO  MCP-PATHNAME
grpsys*            CALL    "MONFUNC"         USING MCPAREA
grpsys*            CALL    "ORCDBMAIN"       USING MCPAREA
      *                                      ORCA-BLOB
grpsys*                                      SPA-AREA
      *            MOVE    ORCA-BLOB-OBJECT    TO  SPA-NAI-OBJECT(IDX)
                   MOVE    ORCF-FILE-NAME      TO  SPA-NAI-FILENAME(IDX)
                   GO        TO  20122-PSFILE-EXIST-EXT
           END-IF
           .
       20122-PSFILE-EXIST-EXT.
           EXIT.
      *           
      *****************************************************************
      *    プレビューDB編集処理
      *****************************************************************
       20121-PREVIEW-EXIST-SEC      SECTION.
      *           
      *           
            MOVE    SPA-NAI-TBL-KEY (IDX)    TO  PREVIEW-TBL-KEY
            MOVE    SPA-NAI-RENNUM    (IDX)  TO  PREVIEW-RENNUM
            MOVE    SPA-NAI-TBL-GROUP (IDX)  TO  PREVIEW-TBL-GROUP
            MOVE    SPA-NAI-TBL-UUID  (IDX)  TO  PREVIEW-TBL-UUID
            MOVE    SPA-NAI-SHORI-RENNUM (IDX)
                                             TO  PREVIEW-SHORI-RENNUM
            MOVE    SPA-NAI-PAGE (IDX)       TO  PREVIEW-PAGE
      *           
            MOVE    PREVIEW-REC              TO  MCPDATA-REC
            MOVE    "DBSELECT"               TO  MCP-FUNC
                    MOVE    1                   TO  FLG-PREVIEW
            IF     FLG-PREVIEW               =   ZERO
                   MOVE   SPACE                TO  ORCF-FILE-NAME
                   MOVE   SPA-NAI-FILENAME (IDX)
                                               TO  ORCF-FILE-NAME
                   MOVE    "r"                 TO  ORCF-FILE-MODE
                   MOVE    PREVIEW-PRTDATA     TO  ORCF-DATA 
                   CALL    "orcfileio"     USING ST  ORCFILEGET-AREA
                   IF      ORCF-RET        NOT =   ZERO
                       DISPLAY "FGET FILE OPEN ERROR " ORCF-FILE-NAME 
                   ELSE
                     MOVE  1                   TO
                                               SPA-NAI-MAKEOK-FLG (IDX)
                     MOVE   "*"                TO
                                               SPA-GMN-TPRFLG     (IDX)
      *              MOVE   "BLOBIMPORT"       TO  MCP-FUNC
      *              MOVE   SPA-NAI-FILENAME (IDX)
      *                                        TO  ORCA-BLOB-FILE
      *              MOVE    "key"               TO  MCP-PATHNAME
      *              MOVE    "blob"              TO  MCP-TABLE
grpsys*              CALL    "ORCDBMAIN"       USING
      *                                        MCPAREA
      *                                        ORCA-BLOB
      *                                        SPA-AREA
      *              MOVE    ORCA-BLOB-OBJECT    TO  SPA-NAI-OBJECT(IDX)
      ***************DISPLAY "take file = "  ORCA-BLOB-FILE
                   END-IF
            END-IF
           .
       20121-PREVIEW-EXIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューDB編集処理
      *****************************************************************
       2013-PREVIEW-CHECK-SEC        SECTION.
      *
            IF      SPA-NAI-MAKEOK-FLG (IDX) =   1
                    MOVE    SPACE
                                           TO  SPA-GMN-ERRMSG
                    MOVE    SPA-NAI-MEMO(IDX)
                                           TO  SPA-GMN-MEMO
                    MOVE    1              TO  SPA-NAI-MEMO-FLG
                    GO      TO               2013-PREVIEW-CHECK-EXT
            END-IF
      *
            MOVE    SPA-NAI-TBL-KEY (IDX)    TO  PREVIEW-TBL-KEY
            MOVE    SPA-NAI-RENNUM    (IDX)  TO  PREVIEW-RENNUM
            MOVE    SPA-NAI-TBL-GROUP (IDX)  TO  PREVIEW-TBL-GROUP
            MOVE    SPA-NAI-TBL-UUID  (IDX)  TO  PREVIEW-TBL-UUID
            MOVE    SPA-NAI-SHORI-RENNUM (IDX)
                                             TO  PREVIEW-SHORI-RENNUM
            MOVE    SPA-NAI-PAGE (IDX)       TO  PREVIEW-PAGE
            MOVE    SPA-HOSPNUM              TO  PREVIEW-HOSPNUM
grpsys*     MOVE SPA-HOSPNUM TO -HOSPNUM
            MOVE    PREVIEW-REC              TO  MCPDATA-REC
            MOVE    "DBSELECT"               TO  MCP-FUNC
                MOVE    1                   TO  FLG-PREVIEW
            IF     FLG-PREVIEW         =   ZERO
                   MOVE   SPA-NAI-FILENAME (IDX)   TO  ORCF-FILE-NAME
                   MOVE    "w"                 TO  ORCF-FILE-MODE
                   MOVE    PREVIEW-PRTDATA     TO  ORCF-DATA 
                   CALL    "orcfileio"     USING ST  ORCFILEGET-AREA
                   IF      ORCF-RET        NOT =   ZERO
                           DISPLAY "file write err = ["
                                  ORCF-FILE-NAME "]"
                   ELSE 
                          MOVE  1              TO 
                                               SPA-NAI-MAKEOK-FLG (IDX)
                          MOVE   "*"           TO
                                               SPA-GMN-TPRFLG (IDX)
                          MOVE   SPACE
                                               TO  SPA-GMN-ERRMSG
                   END-IF
            ELSE
                 MOVE    "帳票データ作成中です。"
                                               TO  SPA-GMN-ERRMSG
            END-IF
      *
           .
       2013-PREVIEW-CHECK-EXT.
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
      *****MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           EVALUATE    SPA-XC01-HYOUJI
               WHEN    0
               WHEN    1
                   MOVE    "XC01"              TO  MCP-WINDOW
               WHEN    2
                   MOVE    "XC02"              TO  MCP-WINDOW
           END-EVALUATE
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF    ( MCP-WIDGET          =   "B05" OR "B08" OR "SELNUM" )
            OR   ( FLG-GMN-INIT        =   2 )
            OR   ( SPA-XC01-HYOUJI     =   2 )
               MOVE    ZERO            TO  WRK-ROWATTR
               COMPUTE WRK-ROW
                   =   SPA-NAI-SELNUM  -   2
               IF    ( WRK-ROW         <=  0 )
                   MOVE    1           TO  WRK-ROW
                   MOVE    1           TO  WRK-ROWATTR
               END-IF
           ELSE
               MOVE    XC01-ROW        TO  WRK-ROW
               MOVE    ZERO            TO  WRK-ROWATTR
           END-IF
      *
           MOVE    SPACE               TO  XC01
      *
           EVALUATE    FLG-GMN-INIT
           WHEN    1
               MOVE    ZERO            TO  XC01-ROW 
               MOVE    ZERO            TO  XC01-ROWATTR
      *    WHEN    2
      *        MOVE    SPA-GMN-MAX     TO  XC01-ROW 
      *        MOVE    ZERO            TO  XC01-ROWATTR
           WHEN    OTHER
                   MOVE    WRK-ROW     TO  XC01-ROW
                   MOVE    WRK-ROWATTR TO  XC01-ROWATTR
           END-EVALUATE
      *
           EVALUATE    SPA-PRTKANRI-TBL-KEY
               WHEN    "RECEPT2"
                   MOVE    WIDGET-NORMAL
                                       TO  XC01-B05-STATE
                   MOVE    WIDGET-INSENSITIVE
                                       TO  XC01-MEMO-STATE
               WHEN    OTHER
                   MOVE    WIDGET-INSENSITIVE
                                       TO  XC01-B05-STATE
                                           XC01-MEMO-STATE
           END-EVALUATE
      *
           MOVE    SPA-GMN-TITLE       TO  XC01-TITLE
           MOVE    "blue"              TO  XC01-TITLE-STYLE
           MOVE    SPA-GMN-ERRMSG      TO  XC01-ERRMSG
           MOVE    "red"               TO  XC01-ERRMSG-STYLE
      *    MOVE    SPA-GMN-TITLE2      TO  XC01-HEAD2
      *
           MOVE    SPA-GMN-SELNUM      TO  XC01-SELNUM
           MOVE    LOW-VALUE           TO  XC01-PSAREA
      *
           IF    ( SPA-GMN-FILENAME    NOT =   SPACE )
               INITIALIZE                  SACCESS-AREA
               MOVE    SPA-GMN-FILENAME TO SACCESS-IN-FILENAME
               CALL    "ORCSACCESS"        USING
                                           SACCESS-AREA
               IF    ( SACCESS-RC  =   ZERO )
                   MOVE   "BLOBIMPORT"     TO  MCP-FUNC
                   MOVE   SPA-GMN-FILENAME TO  ORCA-BLOB-FILE
                   MOVE    "key"           TO  MCP-PATHNAME
                   MOVE    "blob"          TO  MCP-TABLE
                   CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           ORCA-BLOB
                                           SPA-AREA
                   MOVE    ORCA-BLOB-OBJECT
                                       TO  XC01-PSAREA
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-TITLE
           MOVE    SPACE               TO  WRK-PTNUM
           MOVE    SPACE               TO  WRK-SRYYM
           MOVE    SPACE               TO  WRK-PRTID
           IF     (SPA-NAI-MEMO-FLG    =   1)
           AND    (SPA-PRTKANRI-TBL-KEY
                                       =   "RECEPT2")
               MOVE  WIDGET-NORMAL     TO  XC01-MEMO-STATE
               MOVE  SPA-GMN-MEMO      TO  XC01-MEMO
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  XC01-MEMO-STATE
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
      *
               MOVE    SPA-GMN-TNO       (IDX) TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  XC01-TNO       (IDX)
               MOVE    SPA-GMN-TPAGE     (IDX) TO  WRK-PAGE
               IF      SPA-GMN-TSRYYM    (IDX) NOT =   WRK-SRYYM
                       MOVE    SPA-GMN-TSRYYM (IDX)
                                               TO  XC01-TSRYYM    (IDX)
                                                   WRK-SRYYM
               END-IF
      *        MOVE    SPA-GMN-TSRYYM    (IDX) TO  XC01-TSRYYM    (IDX)
               IF      SPA-GMN-TPRT-TITLE(IDX) NOT =   WRK-TITLE 
                 MOVE    SPA-GMN-TPRT-TITLE(IDX)
                                               TO  WRK-TITLE
                 MOVE  SPA-GMN-TPRT-TITLE(IDX) TO  XC01-TPRT-TITLE(IDX)
               END-IF
               IF      SPA-GMN-PTNUM(IDX)      NOT = WRK-PTNUM
                 MOVE  SPA-GMN-PTNUM(IDX)      TO  WRK-PTNUM
                 MOVE  SPA-GMN-PTNUM(IDX)      TO  XC01-PTNUM(IDX)
               END-IF
               IF      SPA-GMN-TPRTID(IDX)     NOT = WRK-PRTID
                 MOVE  SPA-GMN-TPRTID(IDX)     TO  WRK-PRTID
                 MOVE  SPA-GMN-TPRTID(IDX)     TO  XC01-PRTID(IDX)
               END-IF
               MOVE    SPA-GMN-TPRFLG    (IDX) TO  XC01-OKFLG(IDX)
               MOVE    SPA-GMN-TUPDFLG   (IDX) TO  XC01-UPDFLG(IDX)
      *        
               IF      IDX             =   SPA-NAI-SELNUM
                   MOVE    "T"             TO  XC01-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  XC01-SELECT (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  XC01-COUNT
      *
           IF      SPA-GMN-TPRFLG (1)  =   "*"
               MOVE    ZERO                TO  XC01-DURATION
           ELSE
               MOVE    2                   TO  XC01-DURATION
           END-IF
      *
           MOVE    XC01                TO  XC02
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
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SELNUM"       TO  MCP-WIDGET
               WHEN    2
                   MOVE    "MEMO"         TO  MCP-WIDGET
               WHEN    3
                   MOVE    "B08"          TO  MCP-WIDGET
               WHEN    5
                   MOVE    "STPAGE"       TO  MCP-WIDGET
               WHEN    6
                   MOVE    "EDPAGE"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"          TO  MCP-WIDGET
           END-EVALUATE
          .
       5001-MAPCUR-EXT.
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
                   MOVE    "該当の選択番号はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "頁の入力が違います" 
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "未入力の項目があります" 
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "頁の入力が違います" 
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "該当の帳票はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "該当の帳票はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "全角入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "帳票管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "帳票ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    ZERO                 TO  XC01-DURATION
      *
           MOVE    SPACE                TO  XCERR
           MOVE    SPA-ERRCD            TO  XCERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  XCERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           IF    ( SPA-XC01-HYOUJI      =   2 )
               MOVE    "XC02"           TO  SPA-MOTOPG
           ELSE
               MOVE    "XC01"            TO  SPA-MOTOPG
           END-IF
           MOVE    "XCERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "XCERR"               TO  MCP-WINDOW
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
      *     印刷管理ＤＢ読み込み（ｋｅｙ２）
      *****************************************************************
       900-PREVIEW-FET2-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    1                   TO  FLG-PREVIEW
           .
       900-PREVIEW-FET2-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PREVIEW-FET-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    1                   TO  FLG-PREVIEW
           .
       900-PREVIEW-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *     プレビュー管理ＤＢ更新
      *****************************************************************
       4401-PRVKANRI-UPD-SEC            SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
               MOVE    ZERO                TO  FLG-PRVKANRI
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
           .
       4401-PRVKANRI-UPD--EXT.
           EXIT.
      *
      *****************************************************************
      *     プレビュー管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-FET-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
               MOVE    ZERO                TO  FLG-PRVKANRI
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
           .
       900-PRVKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー管理マスター読込（キー）
      *****************************************************************
       900-PRVKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_prvkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PRVKANRI
               ELSE
                   MOVE    1                   TO  FLG-PRVKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PRVKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    receprt読込（キー）
      *****************************************************************
       900-RECEPRT-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key43"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECEPRT 
               ELSE
                   MOVE    1                   TO  FLG-RECEPRT 
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECEPRT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-RECEPRT-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
