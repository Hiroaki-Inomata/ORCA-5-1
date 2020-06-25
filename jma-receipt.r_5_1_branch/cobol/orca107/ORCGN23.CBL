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
       PROGRAM-ID.                 ORCGN23.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬剤情報設定
      *  コンポーネント名  : 薬剤情報マスタ出力設定画面（Ｎ２３）
      *  管理者            : 
      *  06/05/11    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  03.05.01    NACL-森脇     07/05/30  グループ診療対応
      *  03.05.02    NACL-森脇     07/07/03  確認リスト追加
      *  04.08.01    NACL-森脇     14/08/21  一時ファイルディレクトリ設定
      *  05.00.00    NACL-森脇     17/11/29  クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  YKJMERR-FILE    ASSIGN  YKJMERR
                                   FILE    STATUS  IS  STS-YKJMERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  YKJMERR-FILE.
       01  YKJMERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //YKJMERR//.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    薬剤情報マスタ設定用共通パラメタ
           COPY    "N21COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "N21SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-YKJMERR         PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX                 PIC 9(05).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
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
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-SHORIERR        PIC X(200).
           03  WRK-N21IDMSG        PIC X(70).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
           03  WRK-HENFLG          PIC 9(01).
      *
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
           03  WRK-PRT-PAGE        PIC 9(10).
           03  WRK-YKJMERR         PIC X(200).
       01  WRK-JYOKEN-AREA.
           03  WRK-CHK             PIC X(01).
           03  WRK-JYO             PIC X(01).
           03  WRK-KBN             PIC X(01).
           03  WRK-NAME            PIC X(30).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-INPUTCD         PIC X(10).
           03  WRK-IYK             PIC X(01).
           03  WRK-KIKAN           PIC X(01).
           03  WRK-PRT             PIC X(01).
           03  WRK-CYU             PIC X(01).
           03  WRK-SYU             PIC X(01).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
      *    SHELL用
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "107".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "YKJMST".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "YKJMST".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK3003.INC".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *----(05.00.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(05.00.00)--ADD-END-----
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
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
       PROCEDURE               DIVISION    USING
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-N21KYOUTU
           MOVE    SPA-FREE        TO  SPA-N21SCRAREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-N21SCRAREA  TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "N21ERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "N23"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF    (  SPA-MOTOPG     =   "XA01" OR "XB01" OR "XC01" )
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-N21SCRAREA
                   INITIALIZE              SPA-N21KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "N29"
               PERFORM 340-N29-SET-SEC
               GO  TO  300-SCREEN-EXT
      *    作表処理
           WHEN    "N21ID1"
               PERFORM 330-N21ID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "N21ID2"
               PERFORM 350-N21ID2-SET-SEC
               GO  TO  300-SCREEN-EXT
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
           INITIALIZE                  SPA-N23-AREA
      *
           MOVE    "F"             TO  SPA-N23-JYO
      *
           MOVE    "T"             TO  SPA-N23-KBN(1)
           MOVE    "F"             TO  SPA-N23-KBN(2)
           MOVE    "F"             TO  SPA-N23-KBN(3)
           MOVE    "F"             TO  SPA-N23-KBN(4)
      *
           MOVE    "F"             TO  SPA-N23-IYK(1)
           MOVE    "F"             TO  SPA-N23-IYK(2)
           MOVE    "F"             TO  SPA-N23-IYK(3)
      *
           MOVE    "T"             TO  SPA-N23-SYU(1)
           MOVE    "F"             TO  SPA-N23-SYU(2)
      *----(03.05.02)--ADD-START---
           MOVE    "F"             TO  SPA-N23-CHK(1)
           MOVE    "F"             TO  SPA-N23-CHK(2)
           MOVE    "F"             TO  SPA-N23-CHK(3)
           MOVE    "F"             TO  SPA-N23-PRT
           MOVE    "F"             TO  SPA-N23-CYU
      *
      *    期間コンボ
           MOVE    "1 なし"        TO  SPA-N23-KIKANLST(1)
           MOVE    "2 ２ヶ月分"    TO  SPA-N23-KIKANLST(2)
           MOVE    "3 ６ヶ月分"    TO  SPA-N23-KIKANLST(3)
           MOVE    3               TO  SPA-N23-KIKAN-MAX
      *
      *----(03.05.02)--ADD-END-----
      *
           MOVE    1               TO  SPA-N23-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面ＯＫ処理
      *****************************************************************
       330-N21ID1-SET-SEC          SECTION.
      *
           IF    ( SPA-N21ID1-FLG  =   "OK" )
               EVALUATE    SPA-N21IDCD
               WHEN    "6666"
                   MOVE    "JOIN"  TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               WHEN    OTHER
      *            ジョブ管理チェック
                   PERFORM 4501-JOBKANRI-CHK-SEC
                   IF      SPA-ERRCD       =   SPACE
                       PERFORM 490-PRINT-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-N29-FLG
           MOVE    SPACE           TO  SPA-N21ID1-FLG
           MOVE    SPACE           TO  SPA-N21IDCD
      *
           .
       330-N21ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｎ２１ＩＤ２）ＯＫ処理
      *****************************************************************
       350-N21ID2-SET-SEC          SECTION.
      *
           IF    ( SPA-N21ID1-FLG  NOT =   SPACE )
      *        ジョブ管理チェック
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 490-PRINT-SEC
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-N21ID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-N29-FLG
           MOVE    SPACE           TO  SPA-N21IDCD
      *
           .
       350-N21ID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-N29-SET-SEC             SECTION.
      *
           IF    ( SPA-N29-FLG     =   "OK" )
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
      *
           MOVE    1               TO  SPA-N23-CUR
      *
           MOVE    SPACE           TO  SPA-N29-FLG
           MOVE    SPACE           TO  SPA-N21ID1-FLG
           MOVE    SPACE           TO  SPA-N21IDCD
      *
           .
       340-N29-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC           SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "N23"           TO  SPA-MOTOPG
           MOVE    "XC01"          TO  SPA-SAKIPG
      *
           MOVE    0               TO  WRK-STPAGE
           MOVE    0               TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO            TO  LNK-XC01-RENNUM
           MOVE    SPA-N23-TBL-GROUP
                                   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO            TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE      TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE      TO  LNK-XC01-ED-PAGE
           MOVE    "N23"           TO  LNK-XC01-MOTOPG
           MOVE    SPACE           TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA   TO  SPA-WORK
      *
           MOVE    SPACE           TO  LINKAREA
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU      TO  LNK-COMMON
           MOVE    SPA-N21SCRAREA  TO  LNK-SCRSPA
      *
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE   "XC01   "        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"     TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 310-SPASET-SEC
      *    再印刷
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 450-REPRINT-SEC
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 490-JYOUHOU-DEL-SEC
      *    印刷
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-PRINT-MAE-SEC
      *    処理確認
           WHEN    "CLICKED"       ALSO    "B13"
               PERFORM 490-KAKUNIN-SEC
      *
      *----(03.05.02)--UPD-START---
      *    リスト選択
           WHEN    "CLICKED"       ALSO    "CHK1"
               IF      SPA-N23-CHK(1)  =   "F"
                   MOVE    "T"         TO  SPA-N23-CHK(1)
                   MOVE    2           TO  SPA-N23-CUR
               ELSE
                   MOVE    "F"         TO  SPA-N23-CHK(1)
                   MOVE    13          TO  SPA-N23-CUR
               END-IF
      *    条件指定ありなしチェックボタン
           WHEN    "CLICKED"       ALSO    "JYO"
               IF      SPA-N23-JYO      =   "F"
                   MOVE    "T"         TO  SPA-N23-JYO
                   MOVE    3           TO  SPA-N23-CUR
               ELSE
                   MOVE    "F"         TO  SPA-N23-JYO
                   MOVE    13          TO  SPA-N23-CUR
               END-IF
      *    条件種類チェックボタン
           WHEN    "CLICKED"       ALSO    "KBN1"
               IF      SPA-N23-KBN(1)  =   "F"
                   MOVE    "T"         TO  SPA-N23-KBN(1)
                   MOVE    "F"         TO  SPA-N23-KBN(2)
                   MOVE    "F"         TO  SPA-N23-KBN(3)
                   MOVE    "F"         TO  SPA-N23-KBN(4)
                   MOVE    7           TO  SPA-N23-CUR
               ELSE
                   MOVE    4           TO  SPA-N23-CUR
               END-IF
           WHEN    "CLICKED"       ALSO    "KBN2"
               IF      SPA-N23-KBN(2)  =   "F"
                   MOVE    "F"         TO  SPA-N23-KBN(1)
                   MOVE    "T"         TO  SPA-N23-KBN(2)
                   MOVE    "F"         TO  SPA-N23-KBN(3)
                   MOVE    "F"         TO  SPA-N23-KBN(4)
                   MOVE    8           TO  SPA-N23-CUR
               ELSE
                   MOVE    5           TO  SPA-N23-CUR
               END-IF
           WHEN    "CLICKED"       ALSO    "KBN3"
               IF      SPA-N23-KBN(3)  =   "F"
                   MOVE    "F"         TO  SPA-N23-KBN(1)
                   MOVE    "F"         TO  SPA-N23-KBN(2)
                   MOVE    "T"         TO  SPA-N23-KBN(3)
                   MOVE    "F"         TO  SPA-N23-KBN(4)
                   MOVE    9           TO  SPA-N23-CUR
               ELSE
                   MOVE    6           TO  SPA-N23-CUR
               END-IF
           WHEN    "CLICKED"       ALSO    "KBN4"
               IF      SPA-N23-KBN(4)  =   "F"
                   MOVE    "F"         TO  SPA-N23-KBN(1)
                   MOVE    "F"         TO  SPA-N23-KBN(2)
                   MOVE    "F"         TO  SPA-N23-KBN(3)
                   MOVE    "T"         TO  SPA-N23-KBN(4)
                   MOVE    10          TO  SPA-N23-CUR
               ELSE
                   MOVE    13          TO  SPA-N23-CUR
               END-IF
      *    薬剤区分チェックボタン
           WHEN    "CLICKED"       ALSO    "IYK1"
               IF      SPA-N23-IYK(1)  =   "F"
                   MOVE    "T"         TO  SPA-N23-IYK(1)
               ELSE
                   MOVE    "F"         TO  SPA-N23-IYK(1)
               END-IF
               MOVE    10          TO  SPA-N23-CUR
           WHEN    "CLICKED"       ALSO    "IYK2"
               IF      SPA-N23-IYK(2)  =   "F"
                   MOVE    "T"         TO  SPA-N23-IYK(2)
               ELSE
                   MOVE    "F"         TO  SPA-N23-IYK(2)
               END-IF
               MOVE    11           TO  SPA-N23-CUR
           WHEN    "CLICKED"       ALSO    "IYK3"
               IF      SPA-N23-IYK(3)  =   "F"
                   MOVE    "T"         TO  SPA-N23-IYK(3)
               ELSE
                   MOVE    "F"         TO  SPA-N23-IYK(3)
               END-IF
               MOVE    12          TO  SPA-N23-CUR
      *    確認リスト選択
           WHEN    "CLICKED"       ALSO    "CHK2"
               IF      SPA-N23-CHK(2)  =   "F"
                   MOVE    "T"         TO  SPA-N23-CHK(2)
                   MOVE    14          TO  SPA-N23-CUR
                   MOVE    SPA-N23-KIKANLST(1)
                                       TO  SPA-N23-KIKAN-G
               ELSE
                   MOVE    "F"         TO  SPA-N23-CHK(2)
                   MOVE    17          TO  SPA-N23-CUR
               END-IF
      *    注射薬含めるか
           WHEN    "CLICKED"       ALSO    "CYU"
               IF      SPA-N23-CYU     =   "F"
                   MOVE    "T"         TO  SPA-N23-CYU
               ELSE
                   MOVE    "F"         TO  SPA-N23-CYU
               END-IF
               MOVE    16          TO  SPA-N23-CUR
      *    出力形式指定
           WHEN    "CLICKED"       ALSO    "PRT"
               IF      SPA-N23-PRT     =   "F"
                   MOVE    "T"         TO  SPA-N23-PRT
               ELSE
                   MOVE    "F"         TO  SPA-N23-PRT
               END-IF
               MOVE    17          TO  SPA-N23-CUR
      *    確認リスト選択
           WHEN    "CLICKED"       ALSO    "CHK3"
               IF      SPA-N23-CHK(3)  =   "F"
                   MOVE    "T"         TO  SPA-N23-CHK(3)
               ELSE
                   MOVE    "F"         TO  SPA-N23-CHK(3)
               END-IF
               MOVE    18          TO  SPA-N23-CUR
      *    出力順指定
           WHEN    "CLICKED"       ALSO    "SYU1"
               IF      SPA-N23-SYU(1)  =   "F"
                   MOVE    "T"         TO  SPA-N23-SYU(1)
                   MOVE    "F"         TO  SPA-N23-SYU(2)
               END-IF
               MOVE    19          TO  SPA-N23-CUR
           WHEN    "CLICKED"       ALSO    "SYU2"
               IF      SPA-N23-SYU(2)  =   "F"
                   MOVE    "F"         TO  SPA-N23-SYU(1)
                   MOVE    "T"         TO  SPA-N23-SYU(2)
               END-IF
               MOVE    99          TO  SPA-N23-CUR
      *----(03.05.02)--UPD-END-----
           END-EVALUATE
      *
           MOVE    N23-NAME         TO  SPA-N23-NAME
           MOVE    N23-SRYCD        TO  SPA-N23-SRYCD
           MOVE    N23-INPUTCD      TO  SPA-N23-INPUTCD
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
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "NAME"
               MOVE    N23-NAME        TO  SPA-N23-NAME
               MOVE    13              TO  SPA-N23-CUR
           WHEN    "ACTIVATE"      ALSO    "SRYCD"
               MOVE    N23-SRYCD       TO  SPA-N23-SRYCD
               MOVE    13              TO  SPA-N23-CUR
           WHEN    "ACTIVATE"      ALSO    "INPUTCD"
               MOVE    N23-INPUTCD     TO  SPA-N23-INPUTCD
               MOVE    13              TO  SPA-N23-CUR
      *----(03.05.02)--ADD-START---
           WHEN    "ACTIVATE"      ALSO    "KIKAN"
               MOVE    N23-KIKAN       TO  SPA-N23-KIKAN-G
               MOVE    ZERO            TO  WRK-HENFLG
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL  (IDX     >   SPA-N23-KIKAN-MAX)
                       OR     (WRK-HENFLG      =   1)
                   IF       SPA-N23-KIKANL(IDX)   =  SPA-N23-KIKAN
                       MOVE    SPA-N23-KIKANLST(IDX)
                                           TO  SPA-N23-KIKAN-G
                       MOVE    1           TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO
                   MOVE    "0005"      TO  SPA-ERRCD
                   MOVE    14          TO  SPA-N23-CUR
               ELSE
                   MOVE    15              TO  SPA-N23-CUR
               END-IF
      *----(03.05.02)--ADD-END-----
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-3003-STYUKYMD
                                       SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC 
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0 >      SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"       TO  SJOBKANRI-MODE
                   INITIALIZE              JOBKANRI-REC
                   MOVE    IDX0        TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF    ( SJOBKANRI-RETURN
                                           =   ZERO )
                       IF    ( JOB-ENDYMD      =   SPACE )
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"  TO  SPA-N21IDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      * 
           MOVE    "N21"               TO  SPA-SAKIPG
           MOVE    "N23"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
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
      *    再印刷画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-N21SCRAREA      TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "N23"               TO  SPA-MOTOPG
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
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-N21SCRAREA      TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "N23"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
               PERFORM VARYING IDX0 FROM    1   BY  1
                       UNTIL ( IDX0 >       SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"       TO  SJOBKANRI-MODE
                   INITIALIZE              JOBKANRI-REC
                   MOVE    IDX0        TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF    ( SJOBKANRI-RETURN
                                           =   ZERO )
                       IF    ( JOB-ENDYMD  =   SPACE )
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-SHORIERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-SHORIERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX0
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-JYOKEN-AREA
      *
      *    処理が選択されているかチェック
           IF    ( SPA-N23-CHK(1)  NOT =   "T" ) AND
                 ( SPA-N23-CHK(2)  NOT =   "T" ) AND
                 ( SPA-N23-CHK(3)  NOT =   "T" )
               MOVE    "0006"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-N23-CUR
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    条件セット
           PERFORM 420-JYOKEN-SET-SEC
      *
      *
      *    確認リスト指定
           IF      SPA-N23-CHK(1)      =   "T"
               IF      WRK-JYO     =   "2"
                   EVALUATE    WRK-KBN
                   WHEN    "1"
                       IF      WRK-NAME     =   SPACE
                           MOVE    "0001"       TO  SPA-ERRCD
                           MOVE    7            TO  SPA-N23-CUR
                       END-IF
                   WHEN    "2"
                       IF      WRK-SRYCD    =   SPACE
                           MOVE    "0002"       TO  SPA-ERRCD
                           MOVE    8            TO  SPA-N23-CUR
                       END-IF
                   WHEN    "3"
                       IF      WRK-INPUTCD  =   SPACE
                           MOVE    "0003"       TO  SPA-ERRCD
                           MOVE    9            TO  SPA-N23-CUR
                       END-IF
                   WHEN    "4"
                       IF      WRK-IYK          =   SPACE
                           MOVE    "0004"       TO  SPA-ERRCD
                           MOVE    10           TO  SPA-N23-CUR
                       END-IF
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF      SPA-N23-CHK(2)   =   "T"
      *        期間指定
               IF      WRK-KIKAN    =   SPACE
                   MOVE    "0005"      TO  SPA-ERRCD
                   MOVE    14          TO  SPA-N23-CUR
               END-IF
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件セット処理
      *****************************************************************
       420-JYOKEN-SET-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-JYOKEN-AREA
      *
      *    確認リスト指定
           IF      SPA-N23-CHK(1)      =   "T"
      *        条件有無指定
               IF      SPA-N23-JYO     =   "F"
                   MOVE    "1"         TO  WRK-JYO
               ELSE
                   MOVE    "2"         TO  WRK-JYO
      *
      *            条件区分指定
                   IF      SPA-N23-KBN(1)  =   "T"
                       MOVE    "1"         TO  WRK-KBN
                   ELSE
                       IF      SPA-N23-KBN(2)  =   "T"
                           MOVE    "2"         TO  WRK-KBN
                       ELSE
                           IF      SPA-N23-KBN(3)  =   "T"
                               MOVE    "3"         TO  WRK-KBN
                           ELSE
                               IF      SPA-N23-KBN(4)  =   "T"
                                   MOVE    "4"         TO  WRK-KBN
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
                   EVALUATE    WRK-KBN
      *            薬剤名称指定
                   WHEN    "1"
                       IF      SPA-N23-NAME     NOT =   SPACE
                           MOVE    SPA-N23-NAME     TO  WRK-NAME
                       END-IF
      *            診療コード指定
                   WHEN    "2"
                       IF      SPA-N23-SRYCD    NOT =   SPACE
                           MOVE    SPA-N23-SRYCD    TO  WRK-SRYCD
                       END-IF
      *            入力コード指定
                   WHEN    "3"
                       IF      SPA-N23-INPUTCD  NOT =   SPACE
                           MOVE    SPA-N23-INPUTCD  TO  WRK-INPUTCD
                       END-IF
      *            薬剤区分指定
                   WHEN    "4"
                       IF      SPA-N23-IYK(1)      =   "T"
                           IF      SPA-N23-IYK(2)      =   "T"
                               IF      SPA-N23-IYK(3)      =   "T"
                                   MOVE    "0"         TO  WRK-IYK
                               ELSE
                                   MOVE    "1"         TO  WRK-IYK
                               END-IF
                           ELSE
                               IF      SPA-N23-IYK(3)      =   "T"
                                   MOVE    "2"         TO  WRK-IYK
                               ELSE
                                   MOVE    "3"         TO  WRK-IYK
                               END-IF
                           END-IF
                       ELSE
                           IF      SPA-N23-IYK(2)      =   "T"
                               IF      SPA-N23-IYK(3)      =   "T"
                                   MOVE    "4"         TO  WRK-IYK
                               ELSE
                                   MOVE    "5"         TO  WRK-IYK
                               END-IF
                           ELSE
                               IF      SPA-N23-IYK(3)      =   "T"
                                   MOVE    "6"         TO  WRK-IYK
                               END-IF
                           END-IF
                       END-IF
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF      SPA-N23-CHK(2)   =   "T"
      *        期間指定
               EVALUATE    SPA-N23-KIKAN
               WHEN    "1"
                   MOVE    "0"         TO  WRK-KIKAN
               WHEN    "2"
                   MOVE    "2"         TO  WRK-KIKAN
               WHEN    "3"
                   MOVE    "6"         TO  WRK-KIKAN
               END-EVALUATE
      *        注射を条件に含めるか指定
               IF      SPA-N23-CYU         =   "T"
                   MOVE    "1"         TO  WRK-CYU
               END-IF
      *        出力形式指定
               IF      SPA-N23-PRT         =   "T"
                   MOVE    "1"         TO  WRK-PRT
               END-IF
           END-IF
      *
      *    出力順指定
           IF      SPA-N23-SYU(1)      =   "T"
               MOVE    "1"         TO  WRK-SYU
           ELSE
               IF      SPA-N23-SYU(2)      =   "T"
                   MOVE    "2"         TO  WRK-SYU
               END-IF
           END-IF
      *
           .
       420-JYOKEN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷前確認処理
      *****************************************************************
       490-PRINT-MAE-SEC           SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
      *    ジョブ管理チェック
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF   (  SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
           MOVE    "1001"          TO  SPA-N21IDCD
      *
           .
       490-PRINT-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       490-PRINT-SEC               SECTION.
      * 
           INITIALIZE                  SPA-N23-TBL-GROUP
                                       SPA-N23-JOB-JOBID
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-3003-STYUKYMD
                                       SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
               MOVE    SYS-3003-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "1005"      TO  SPA-ERRCD
                   GO  TO  490-PRINT-EXT
               END-IF
           END-IF
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1004"          TO  SPA-ERRCD
               GO  TO  490-PRINT-EXT
           END-IF
      *
      *    印刷処理
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
           MOVE    WRK-SYSYMD          TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    "OR107ERR"          TO  YKJMERR-FILE-ID
           MOVE    SPA-TERMID          TO  YKJMERR-TERMID
           MOVE    SPA-HOSPNUM         TO  YKJMERR-HOSPNUM
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC    NOT =   ZERO )
               INITIALIZE                  SYS-1031-REC
           END-IF 
      *
      *    条件
           PERFORM 420-JYOKEN-SET-SEC
      *
           IF      SPA-N23-CHK(1)   =  "T"
               PERFORM 4901-PRINT-SYORI-SEC
           END-IF
      *
           IF      SPA-N23-CHK(2)   =  "T"
               MOVE    "1"          TO  WRK-CHK
               PERFORM 4901-PRINT-SYORI2-SEC
           END-IF
      *
           IF      SPA-N23-CHK(3)   =  "T"
               MOVE    "2"          TO  WRK-CHK
               MOVE    "0"          TO  WRK-KIKAN
               MOVE    "0"          TO  WRK-CYU
               MOVE    "0"          TO  WRK-PRT
               PERFORM 4901-PRINT-SYORI2-SEC
           END-IF
      *
      *    印刷処理
           IF      SPA-N21ID1-FLG        =   "OK"  OR "PV"
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  SPA-N23-TBL-GROUP
               MOVE    WRK-CONS-JOB-JOBID
                                           TO  SPA-N23-JOB-JOBID
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *
               MOVE    SPACE               TO  SHELLTBL
      *
      *        ファイルデイレクトリ位置指定
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    "scripts/allways/print_prv.sh"
                                           TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    SPA-SYSYMD(1:6)     TO  LNK-PRTKANRI-SRYYM
               MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
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
      *----(04.08.01)--UPD-START---
      *         MOVE    YKJMERR             TO  SHELLTBL-ARG12
               MOVE    YKJMERR-BASENAME    TO  SHELLTBL-ARG12
      *----(04.08.01)--UPD-END-----
               MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-N21ID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
      *         WHEN    "NO"
      *             MOVE    "3"             TO  SHELLTBL-ARG17
      *----(05.00.00)--ADD-START---
               WHEN    "OK"
                   IF      SPA-CLIENT-PRT-FLG  =  2
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-IF
               END-EVALUATE
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *----(05.00.00)--ADD-END-----
      *----(03.05.01)--ADD-START---
grpsys         MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SPACE               TO  SHELLTBL-ARG19
               MOVE    SPACE               TO  SHELLTBL-ARG20
      *----(03.05.01)--ADD-END-----
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "shell"             TO  MCP-PATHNAME
               MOVE    "SHELL"             TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 45012-JOBID-UPD-SEC
      *
           PERFORM 490-KAKUNIN-SEC
      *
           .
       490-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4901-PRINT-SYORI-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *    ファイルデイレクトリ位置指定
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "ykjmst.sh"     TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE           TO  SHELLTBL 
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "scripts/allways/ykjmst.sh"
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11
      *
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           MOVE    WRK-JYO         TO  SHELLTBL-ARG15
           MOVE    WRK-KBN         TO  SHELLTBL-ARG16
           MOVE    WRK-IYK         TO  SHELLTBL-ARG17
           MOVE    WRK-SYU         TO  SHELLTBL-ARG18
           MOVE    WRK-NAME        TO  SHELLTBL-ARG19
           MOVE    WRK-SRYCD       TO  SHELLTBL-ARG20
           MOVE    WRK-INPUTCD     TO  SHELLTBL-ARG21
           MOVE    YKJMERR         TO  SHELLTBL-ARG22
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
           .
      *
       4501-PRINT-EXT.
           EXIT.
      *
      *----(03.05.02)--ADD-START---
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4901-PRINT-SYORI2-SEC           SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *    ファイルデイレクトリ位置指定
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "ykjchk.sh"     TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE           TO  SHELLTBL 
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "scripts/allways/ykjchk.sh"
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11
      *
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           MOVE    WRK-CHK         TO  SHELLTBL-ARG15
           MOVE    WRK-KIKAN       TO  SHELLTBL-ARG16
           MOVE    WRK-CYU         TO  SHELLTBL-ARG17
           MOVE    WRK-PRT         TO  SHELLTBL-ARG18
           MOVE    WRK-SYU         TO  SHELLTBL-ARG19
           MOVE    YKJMERR         TO  SHELLTBL-ARG20
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
           .
      *
       4501-PRINT2-EXT.
           EXIT.
      *----(03.05.02)--ADD-END-----
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *       
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID
      *
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    SPACE               TO  N29
           INITIALIZE                      N29
           MOVE    ZERO                TO  N29-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  N29-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC 
               PERFORM VARYING IDX0 FROM   1   BY  1
                       UNTIL ( IDX0 >      SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX0            TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
      *
                       MOVE    JOB-JOBID       TO  N29-RENNUM   (IDX0)
                       MOVE    JOB-SHELLMSG    TO  N29-SHORINM  (IDX0)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  N29-STARTTIME(IDX0)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  N29-ENDTIME  (IDX0)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  N29-PAGE     (IDX0)
                       MOVE    JOB-YOBI        TO  N29-ERRMSG   (IDX0)
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  N29-MSG
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE )
                               CONTINUE
                           ELSE
                               MOVE    SPACE   TO  N29-MSG
                               STRING
                                   JOB-YOBI        DELIMITED  BY  SPACE
                                   "【"            DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                               INTO    N29-MSG
                               END-STRING
                           END-IF
                       END-IF
                       MOVE    IDX0        TO  N29-COUNT
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( N29-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( N29-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                       TO  N29-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                       TO  N29-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-N21ID1-FLG    =   "PV" )
               MOVE    "F12:プレビュー"    TO  N29-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                   MOVE    WIDGET-NORMAL   TO  N29-B12-STATE
                   MOVE    "B12"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "N23"               TO  SPA-MOTOPG
           MOVE    "N29"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "N29"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-N21IDCD     NOT =   SPACE )
               EVALUATE    SPA-N21IDCD
               WHEN    "1001"
                   PERFORM 540-N21ID2SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-N21IDSET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "N23    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  N23
      *
      *----(03.05.02)--ADD-START---
           IF      SPA-N23-CHK(1)  =   "F"
               MOVE    "F"             TO  SPA-N23-JYO
               MOVE    WIDGET-INSENSITIVE
                                       TO  N23-JYO-STATE
           ELSE
               MOVE     WIDGET-NORMAL  TO  N23-JYO-STATE
           END-IF
           IF      SPA-N23-CHK(2)  =   "F"
               MOVE    SPACE           TO  SPA-N23-KIKAN-G
               MOVE    "F"             TO  SPA-N23-CYU
               MOVE    "F"             TO  SPA-N23-PRT
               MOVE    WIDGET-INSENSITIVE
                                       TO  N23-KIKAN-STATE
                                           N23-CYU-STATE
                                           N23-PRT-STATE
           ELSE
               MOVE     WIDGET-NORMAL  TO  N23-KIKAN-STATE
                                           N23-CYU-STATE
                                           N23-PRT-STATE
           END-IF
      *----(03.05.02)--ADD-END-----
      *----(03.05.02)--UPD-START---
           IF      SPA-N23-JYO     =   "F"
               MOVE    WIDGET-INSENSITIVE
                                       TO  N23-KBN-STATE(1)
                                           N23-KBN-STATE(2)
                                           N23-KBN-STATE(3)
                                           N23-KBN-STATE(4)
                                           N23-NAME-STATE
                                           N23-SRYCD-STATE
                                           N23-INPUTCD-STATE
                                           N23-IYK-STATE(1)
                                           N23-IYK-STATE(2)
                                           N23-IYK-STATE(3)
               MOVE    SPACE           TO  SPA-N23-NAME
                                           SPA-N23-SRYCD
                                           SPA-N23-INPUTCD
               MOVE    "F"             TO  SPA-N23-IYK(1)
                                           SPA-N23-IYK(2)
                                           SPA-N23-IYK(3)
           ELSE
               MOVE     WIDGET-NORMAL  TO  N23-KBN-STATE(1)
                                           N23-KBN-STATE(2)
                                           N23-KBN-STATE(3)
                                           N23-KBN-STATE(4)
               IF      SPA-N23-KBN(1)  =   "T"
                   MOVE    WIDGET-NORMAL   TO  N23-NAME-STATE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  N23-SRYCD-STATE
                                               N23-INPUTCD-STATE
                                               N23-IYK-STATE(1)
                                               N23-IYK-STATE(2)
                                               N23-IYK-STATE(3)
                   MOVE    SPACE           TO  SPA-N23-SRYCD
                                               SPA-N23-INPUTCD
                   MOVE    "F"             TO  SPA-N23-IYK(1)
                                               SPA-N23-IYK(2)
                                               SPA-N23-IYK(3)
               END-IF
               IF      SPA-N23-KBN(2)  =   "T"
                   MOVE    WIDGET-NORMAL   TO  N23-SRYCD-STATE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  N23-NAME-STATE
                                               N23-INPUTCD-STATE
                                               N23-IYK-STATE(1)
                                               N23-IYK-STATE(2)
                                               N23-IYK-STATE(3)
                   MOVE    SPACE           TO  SPA-N23-NAME
                                               SPA-N23-INPUTCD
                   MOVE    "F"             TO  SPA-N23-IYK(1)
                                               SPA-N23-IYK(2)
                                               SPA-N23-IYK(3)
               END-IF
               IF      SPA-N23-KBN(3)  =   "T"
                   MOVE    WIDGET-NORMAL   TO  N23-INPUTCD-STATE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  N23-NAME-STATE
                                               N23-SRYCD-STATE
                                               N23-IYK-STATE(1)
                                               N23-IYK-STATE(2)
                                               N23-IYK-STATE(3)
                   MOVE    SPACE           TO  SPA-N23-NAME
                                               SPA-N23-SRYCD
                   MOVE    "F"             TO  SPA-N23-IYK(1)
                                               SPA-N23-IYK(2)
                                               SPA-N23-IYK(3)
               END-IF
               IF      SPA-N23-KBN(4)  =   "T"
                   MOVE    WIDGET-NORMAL   TO  N23-IYK-STATE(1)
                                               N23-IYK-STATE(2)
                                               N23-IYK-STATE(3)
                   MOVE    WIDGET-INSENSITIVE
                                           TO  N23-NAME-STATE
                                               N23-SRYCD-STATE
                                               N23-INPUTCD-STATE
                   MOVE    SPACE           TO  SPA-N23-NAME
                                               SPA-N23-SRYCD
                                               SPA-N23-INPUTCD
               END-IF
           END-IF
      *
           MOVE    SPA-N23-CHK(1)  TO  N23-CHK(1)
           MOVE    SPA-N23-CHK(2)  TO  N23-CHK(2)
           MOVE    SPA-N23-CHK(3)  TO  N23-CHK(3)
           MOVE    SPA-N23-JYO     TO  N23-JYO 
           MOVE    SPA-N23-KBN(1)  TO  N23-KBN(1)
           MOVE    SPA-N23-KBN(2)  TO  N23-KBN(2)
           MOVE    SPA-N23-KBN(3)  TO  N23-KBN(3)
           MOVE    SPA-N23-KBN(4)  TO  N23-KBN(4)
           MOVE    SPA-N23-NAME    TO  N23-NAME
           MOVE    SPA-N23-SRYCD   TO  N23-SRYCD
           MOVE    SPA-N23-INPUTCD TO  N23-INPUTCD
           MOVE    SPA-N23-IYK(1)  TO  N23-IYK(1)
           MOVE    SPA-N23-IYK(2)  TO  N23-IYK(2)
           MOVE    SPA-N23-IYK(3)  TO  N23-IYK(3)
           MOVE    SPA-N23-CYU     TO  N23-CYU
           MOVE    SPA-N23-PRT     TO  N23-PRT
           MOVE    SPA-N23-SYU(1)  TO  N23-SYU(1)
           MOVE    SPA-N23-SYU(2)  TO  N23-SYU(2)
      *
           MOVE    SPA-N23-KIKAN-G     TO  N23-KIKAN
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL   IDX     >   SPA-N23-KIKAN-MAX
               MOVE    SPA-N23-KIKANLST(IDX)
                                       TO  N23-KIKANLST(IDX)
           END-PERFORM
           MOVE    SPA-N23-KIKAN-MAX   TO  N23-KIKAN-COUNT
      *----(03.05.02)--UPD-END-----
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-N23-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
      *----(03.05.02)--UPD-START---
           EVALUATE    SPA-N23-CUR
           WHEN    1
               MOVE    "CHK1"          TO  MCP-WIDGET
           WHEN    2
               MOVE    "JYO"           TO  MCP-WIDGET
           WHEN    3
               MOVE    "KBN1"          TO  MCP-WIDGET
           WHEN    4
               MOVE    "KBN2"          TO  MCP-WIDGET
           WHEN    5
               MOVE    "KBN3"          TO  MCP-WIDGET
           WHEN    6
               MOVE    "KBN4"          TO  MCP-WIDGET
           WHEN    7
               MOVE    "NAME"          TO  MCP-WIDGET
           WHEN    8
               MOVE    "SRYCD"         TO  MCP-WIDGET
           WHEN    9
               MOVE    "INPUTCD"       TO  MCP-WIDGET
           WHEN    10
               MOVE    "IYK1"          TO  MCP-WIDGET
           WHEN    11
               MOVE    "IYK2"          TO  MCP-WIDGET
           WHEN    12
               MOVE    "IYK3"          TO  MCP-WIDGET
           WHEN    13
               MOVE    "CHK2"          TO  MCP-WIDGET
           WHEN    14
               MOVE    "KIKAN"         TO  MCP-WIDGET
           WHEN    15
               MOVE    "CYU"           TO  MCP-WIDGET
           WHEN    16
               MOVE    "PRT"           TO  MCP-WIDGET
           WHEN    17
               MOVE    "CHK3"          TO  MCP-WIDGET
           WHEN    18
               MOVE    "SYU1"          TO  MCP-WIDGET
           WHEN    19
               MOVE    "SYU2"          TO  MCP-WIDGET
           WHEN    99
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *----(03.05.02)--UPD-END-----
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
      *----(03.05.02)--UPD-START---
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHK1"
               MOVE    2               TO  SPA-N23-CUR
           WHEN    "JYO"
               MOVE    3               TO  SPA-N23-CUR
           WHEN    "KBN1"
               MOVE    4               TO  SPA-N23-CUR
           WHEN    "KBN2"
               MOVE    5               TO  SPA-N23-CUR
           WHEN    "KBN3"
               MOVE    6               TO  SPA-N23-CUR
           WHEN    "KBN4"
               MOVE    7               TO  SPA-N23-CUR
           WHEN    "NAME"
               MOVE    8               TO  SPA-N23-CUR
           WHEN    "SRYCD"
               MOVE    9               TO  SPA-N23-CUR
           WHEN    "INPUTCD"
               MOVE    10              TO  SPA-N23-CUR
           WHEN    "IYK1"
               MOVE    11              TO  SPA-N23-CUR
           WHEN    "IYK2"
               MOVE    12              TO  SPA-N23-CUR
           WHEN    "IYK3"
               MOVE    13              TO  SPA-N23-CUR
           WHEN    "CHK2"
               MOVE    14              TO  SPA-N23-CUR
           WHEN    "KIKAN"
               MOVE    15              TO  SPA-N23-CUR
           WHEN    "CYU"
               MOVE    16              TO  SPA-N23-CUR
           WHEN    "PRT"
               MOVE    17              TO  SPA-N23-CUR
           WHEN    "CHK3"
               MOVE    18              TO  SPA-N23-CUR
           WHEN    "SYU1"
               MOVE    19              TO  SPA-N23-CUR
           WHEN    "SYU2"
               MOVE    99              TO  SPA-N23-CUR
           END-EVALUATE
      *----(03.05.02)--UPD-END-----
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "薬剤名称が未入力です"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "診療行為コードが未入力です"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "入力コードが未入力です"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "薬剤区分を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "対象期間を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "パラメタが入力されていない項目があります"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE
                   "システム管理ＤＢ（３００３）削除できませんでした"
                                       TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    WRK-SHORIERR    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N21ERR
           MOVE    SPA-ERRCD       TO  N21ERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  N21ERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "N23"           TO  SPA-MOTOPG
           MOVE    "N21ERR"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N21ERR"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-N21IDSET-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-N21IDMSG
      *
           EVALUATE    SPA-N21IDCD
           WHEN    "6666"
               MOVE    "印刷処理中です"
                                       TO  WRK-N21IDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N21ID1
           INITIALIZE                  N21ID1
           MOVE    SPA-N21IDCD     TO  N21ID1-ID1CODE
           MOVE    WRK-N21IDMSG    TO  N21ID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N23"           TO  SPA-MOTOPG
           MOVE    "N21ID1"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N21ID1"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-N21IDSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       540-N21ID2SET-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-N21IDMSG
      *
           EVALUATE    SPA-N21IDCD
           WHEN    "1001"
               MOVE    "設定内容に従い帳票を印刷します"
                                       TO  WRK-N21IDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N21ID2
           INITIALIZE                  N21ID2
           MOVE    SPA-N21IDCD     TO  N21ID2-ID1CODE
           MOVE    WRK-N21IDMSG    TO  N21ID2-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N23"           TO  SPA-MOTOPG
           MOVE    "N21ID2"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N21ID2"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-N21ID2SET-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF    ( MCP-RC              =   ZERO )
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
                   MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
