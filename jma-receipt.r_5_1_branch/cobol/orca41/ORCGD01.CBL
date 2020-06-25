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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGD01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : データチェック条件入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01.07.13    ひさなが 　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    ひさなが     01/11/08  実日数の確認追加
      *  01.00.01    ひさなが     02/01/25  診療年が和暦の場合西暦変換
      *  01.00.01    ひさなが     02/03/18  薬剤、適応病名のチェク追加
      *  01.00.02    ひさなが     02/05/10  ジョブ管理検索追加
      *  01.00.02    ひさなが     02/05/10  半角チェックの追加
      *  01.00.02    ひさなが     02/06/26  印刷管理・印刷ＤＢ追加
      *  01.00.03    ひさなが     02/09/13  ディレクトリをサブプロへ
      *  01.00.04    ひさなが     03/04/17  入院時保険組合せチェック
      *  01.00.05    ひさなが     03/05/13  ボタンが押せない場合修正
      *  01.00.06    ひさなが     03/06/06  病名コード抜けのチェク追加
      *  01.00.07    ひさなが     03/07/29  チェック内容全件印刷追加
      *  01.00.08    ひさなが     03/08/26  院外処方、自費の指示追加
      *  01.00.09    ひさなが     03/09/11  処理メッセージ修正
      *                                     指示に含まないを追加
      *  01.00.10    NACL-太田    03/11/19  シェルパラメータの変更他
      *  01.00.11    NACL-森脇    04/07/29  直前ジョブ管理チェック
      *  03.05.00    NACL-太田    07/06/01  グループ診療対応
      *  05.00.00    NACL-森脇    17/11/30  クライアント印刷対応
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
      *
      *    エラー用ファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //DTCHKERR//.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    データチェック共通パラメタ
           COPY    "D01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-D01.
           03  SPA-D01-AREA.
               05  SPA-GMN-D01-CUR                         PIC 9(03).
               05  SPA-GMN-D01-AREA.
                   07  SPA-GMN-D01-INPUT-AREA.
                       09  SPA-GMN-D01-SRYYM               PIC X(07).
                       09  SPA-GMN-D01-CHK-SYAHO           PIC X(01).
                       09  SPA-GMN-D01-CHK-KOKUHO          PIC X(01).
                       09  SPA-GMN-D01-CHK-KOUKI           PIC X(01).
                       09  SPA-GMN-D01-CHK-RSIJBI          PIC X(01).
                       09  SPA-GMN-D01-CHK-KOUGAI          PIC X(01).
                       09  SPA-GMN-D01-CHK-JIHI            PIC X(01).
                       09  SPA-GMN-D01-CHK-SRYYMD          PIC X(01).
                       09  SPA-GMN-D01-STSRYYMD            PIC 9(02).
                       09  SPA-GMN-D01-EDSRYYMD            PIC 9(02).
                       09  SPA-GMN-D01-PRTSRTKBN-G.
                           11  SPA-GMN-D01-PRTSRTKBN       PIC X(01).
                           11  SPA-GMN-D01-FL              PIC X(01).
                           11  SPA-GMN-D01-PRTSRTKBNMEI    PIC X(20).
                       09  SPA-GMN-D01-NYUGAIKBN-G.
                           11  SPA-GMN-D01-NYUGAIKBN       PIC X(01).
                           11  SPA-GMN-D01-FL              PIC X(01).
                           11  SPA-GMN-D01-NYUGAIKBNMEI    PIC X(18).
                       09  SPA-GMN-D01-USRCHKKBN-G.
                           11  SPA-GMN-D01-USRCHKKBN       PIC X(02).
                           11  SPA-GMN-D01-FL              PIC X(01).
                           11  SPA-GMN-D01-USRCHKKBNMEI    PIC X(30).
                       09  SPA-GMN-D01-CHKKBN-OCC          OCCURS  21.
                           11  SPA-GMN-D01-CHKKBN-G.
                               13  SPA-GMN-D01-CHKKBN      PIC X(01).
                               13  SPA-GMN-D01-FL1         PIC X(01).
                               13  SPA-GMN-D01-CHKKBNMEI   PIC X(18).
                       09  SPA-GMN-D01-INGAIPROCKBN-G.
                           11  SPA-GMN-D01-INGAIPROCKBN    PIC X(01).
                           11  SPA-GMN-D01-FL              PIC X(01).
                           11  SPA-GMN-D01-INGAIPROCKBNMEI PIC X(18).
                   07  SPA-GMN-D01-CHKMSG-OCC              OCCURS  21.
                       09  SPA-GMN-D01-CHKMSG              PIC X(40).
                   07  SPA-GMN-D01-INGAIPROCMSG            PIC X(40).
                   07  SPA-GMN-D01-NYUGAIKBNLST-G.
                       09  SPA-GMN-D01-NYUGAIKBN-MAX       PIC 9(03).
                       09  SPA-GMN-D01-NYUGAIKBNLST        OCCURS  5.
                           11  SPA-GMN-D01-NYUGAIKBNL      PIC X(01).
                           11  SPA-GMN-D01-FL1             PIC X(01).
                           11  SPA-GMN-D01-NYUGAIKBNMEIL   PIC X(18).
                   07  SPA-GMN-D01-PRTSRTKBNLST-G.
                       09  SPA-GMN-D01-PRTSRTKBN-MAX       PIC 9(03).
                       09  SPA-GMN-D01-PRTSRTKBNLST        OCCURS   4.
                           11  SPA-GMN-D01-PRTSRTKBNL      PIC X(01).
                           11  SPA-GMN-D01-FL1             PIC X(01).
                           11  SPA-GMN-D01-PRTSRTKBNMEIL   PIC X(20).
                   07  SPA-GMN-D01-USRCHKKBNLST-G.
                       09  SPA-GMN-D01-USRCHKKBN-MAX       PIC 9(03).
                       09  SPA-GMN-D01-USRCHKKBNLST        OCCURS  10.
                           11  SPA-GMN-D01-USRCHKKBNL      PIC X(02).
                           11  SPA-GMN-D01-FL1             PIC X(01).
                           11  SPA-GMN-D01-USRCHKKBNMEIL
                                                           PIC X(30).
                   07  SPA-GMN-D01-CHKKBNLST-G.
                       09  SPA-GMN-D01-CHKKBN-MAX          PIC 9(03).
                       09  SPA-GMN-D01-CHKKBNLST           OCCURS  5.
                           11  SPA-GMN-D01-CHKKBNL         PIC X(01).
                           11  SPA-GMN-D01-FL1             PIC X(01).
                           11  SPA-GMN-D01-CHKKBNMEIL      PIC X(18).
                   07  SPA-GMN-D01-INGAIPROCKBNLST-G.
                       09  SPA-GMN-D01-INGAIPROCKBN-MAX    PIC 9(03).
                       09  SPA-GMN-D01-INGAIPROCKBNLST     OCCURS  5.
                           11  SPA-GMN-D01-INGAIPROCKBNL   PIC X(01).
                           11  SPA-GMN-D01-FL1             PIC X(01).
                           11  SPA-GMN-D01-INGAIPROCKBNMEIL
                                                           PIC X(18).
               05  SPA-NAIB-D01-AREA.
                   07  SPA-NAI-D01-KBTUSER                 PIC 9(01).
                   07  SPA-NAI-D01-INPUT-AREA.
                       09  SPA-NAI-D01-SRYYM               PIC X(06).
                       09  SPA-NAI-D01-STSRYYMD-BF         PIC 9(02).
                       09  SPA-NAI-D01-IKTKBT              PIC X(01).
                   07  SPA-NAI-D01-USRCHKKBN-SEL           PIC 9(03).
               05  SPA-NAIB-CMB-AREA.
                       09  SPA-NAI-D01-USRCHKKBNLST-G.
                           11  SPA-NAI-D01-USRCHKKBNLST    OCCURS  10.
                               13  SPA-NAI-D01-USRCHKKBN-G.
                                   15  SPA-NAI-D01-USRCHKKBN
                                                           PIC X(01)
                                                           OCCURS  50.
                               13  SPA-NAI-D01-USRINGAIPROCKBN
                                                           PIC X(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-CHK004                  PIC 9(01).
      *
      *    エンドステータス領域
       01  STS-AREA.
           03  STS-DID1                    PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-CHK                     PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDXA                        PIC 9(05).
           03  IDXB                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-PRTKANRI-RENNUM         PIC 9(04).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-IDX             PIC 9(03).
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-ERR-CUR                 PIC 9(03).
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO                 PIC X(01).
               05  WRK-HYY                 PIC 9(02).
               05  FILLER                  PIC X(01)   VALUE   ".".
               05  WRK-HMM                 PIC 9(02).
               05  FILLER                  PIC X(01)   VALUE   ".".
               05  WRK-HDD                 PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH                  PIC X(02).
               05  FILLER                  PIC X(01)   VALUE   ".".
               05  WRK-MM                  PIC X(02).
               05  FILLER                  PIC X(01)   VALUE   ".".
               05  WRK-SS                  PIC X(02).
           03  WRK-ERRMSG                  PIC X(200).
           03  WRK-DIDMSG                  PIC X(200).
           03  WRK-STPAGE                  PIC 9(05).
           03  WRK-EDPAGE                  PIC 9(05).
           03  WRK-PAGE                    PIC ZZZZZZZZZ9.
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-Z9                      PIC Z9.
           03  WRK-PRT-PAGE                PIC 9(10).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YYYY         PIC 9(04).
               05  WRK-SRYYMD-MM           PIC 9(02).
               05  WRK-SRYYMD-DD           PIC 9(02).
           03  WRK-DELKANRICD              PIC X(04).
           03  WRK-DELKBNCD                PIC X(08).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD-NUM           PIC 9(02).
               05  WRK-KBNCD-FL            PIC X(06).
           03  WRK-MOTOPG                  PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-H2004                 PIC X(06)   VALUE   "200804".
           03  CONST-USRCHKKBN-MAX         PIC 9(03)   VALUE   10.
           03  CONST-LINE                  PIC X(40)   VALUE
               "＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿".
           03  CONST-INGAIPROCMSG          PIC X(40)   VALUE
               "院外処方の処理".
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  CONST-3003-KBNCD            PIC X(08) VALUE "041".
      *    印刷管理ＤＢのキー値
           03  CONST-PRTKANRI-TBL-KEY      PIC X(08) VALUE "ORCBSD1".
           03  CONST-PRTKANRI-TBL-GROUP    PIC X(14) VALUE SPACE.
      *    ジョブ管理ＤＢのキー値
           03  CONST-JOB-SHELLID           PIC X(08) VALUE "ORCBSD1".
           03  CONST-JOB-JOBID             PIC 9(07) VALUE ZERO.
      *
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
      *    データチェック確認項目
           COPY    "CPDTCHKKOMOKU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
           COPY    "CPCK2000.INC".
           COPY    "CPCK2001.INC".
           COPY    "CPCK2002.INC".
      *
      *    システム管理
           COPY    "CPSK1101.INC".
           COPY    "CPSK1031.INC".
           COPY    "CPSK3003.INC".
      *  
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *----(05.00.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(05.00.00)--ADD-END-----
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA41SCRAREA.INC".
      *
      *
       PROCEDURE                       DIVISION    USING
                                                   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
                                           IDX-AREA
                                           CNT-AREA
                                           WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D01
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D01             TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "DERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "D01    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF    ( SPA-MOTOPG      =   "XA01" OR "XB01" OR "XC01" )
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-D01
                   INITIALIZE                  SPA-D01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    確認画面１
           WHEN    "DID1"
               PERFORM 330-DID1-SET-SEC
               GO  TO  300-SCREEN-EXT
      *    確認画面２
           WHEN    "DID2"
               PERFORM 340-DID2-SET-SEC
               GO      TO      300-SCREEN-EXT
      *    処理結果画面
           WHEN    "D99"
               PERFORM 340-D99-SET-SEC
               GO  TO  300-SCREEN-EXT
      *    患者選択画面
           WHEN    "D02"
               PERFORM 340-D02-SET-SEC
               GO  TO  300-SCREEN-EXT
      *    確認項目保存画面
           WHEN    "D03"
               PERFORM 340-D03-SET-SEC
               GO  TO  300-SCREEN-EXT
      *    チェック内容確認画面
           WHEN    "D04"
               PERFORM 340-D04-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                      SPA-D01KYOUTU
      *
           PERFORM 311-SPASET-SEC
      *
           MOVE    1                   TO  SPA-GMN-D01-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC                 SECTION.
      *
           INITIALIZE                      SPA-D01
      *
      *    入外区分コンボ編集
           PERFORM 3101-NYUGAIKBNLST-HEN-SEC
      *
      *    印刷順コンボ編集
           PERFORM 3101-PRTSRTKBNLST-HEN-SEC
      *
      *    ユーザー確認項目コンボ編集処理
           PERFORM 3101-USRCHKKBNLST-HEN-SEC
      *
      *    チェック項目名編集
           PERFORM 3101-CHKMSG-HEN-SEC
      *
      *    チェック項目コンボ編集
           PERFORM 3101-CHKKBNLST-HEN-SEC
      *
      *    院外処方の処理項目コンボ編集処理
           PERFORM 3101-INGAIPROCKBNLST-HEN-SEC
      *
           MOVE    CONST-INGAIPROCMSG  TO  SPA-GMN-D01-INGAIPROCMSG
      *
           PERFORM 420-CLEAR-SEC
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボ編集処理
      *****************************************************************
       3101-NYUGAIKBNLST-HEN-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-D01-NYUGAIKBNLST-G
      *
           IF    ( SPA-BEDSU           >   ZERO )
               MOVE    2               TO  SPA-GMN-D01-NYUGAIKBN-MAX
               MOVE    "1"             TO  SPA-GMN-D01-NYUGAIKBNL    (1)
               MOVE    "入院"          TO  SPA-GMN-D01-NYUGAIKBNMEIL (1)
               MOVE    "2"             TO  SPA-GMN-D01-NYUGAIKBNL    (2)
               MOVE    "外来"          TO  SPA-GMN-D01-NYUGAIKBNMEIL (2)
           ELSE
               MOVE    1               TO  SPA-GMN-D01-NYUGAIKBN-MAX
               MOVE    "2"             TO  SPA-GMN-D01-NYUGAIKBNL    (1)
               MOVE    "外来"          TO  SPA-GMN-D01-NYUGAIKBNMEIL (1)
           END-IF
      *
           .
       3101-NYUGAIKBNLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    印刷順コンボ編集処理
      *****************************************************************
       3101-PRTSRTKBNLST-HEN-SEC       SECTION.
      *
      *
           INITIALIZE                  SPA-GMN-D01-PRTSRTKBNLST-G
      *
           MOVE    4               TO  SPA-GMN-D01-PRTSRTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-D01-PRTSRTKBNL    (1)
           MOVE    "患者番号順"    TO  SPA-GMN-D01-PRTSRTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-D01-PRTSRTKBNL    (2)
           MOVE    "カナ氏名順"    TO  SPA-GMN-D01-PRTSRTKBNMEIL (2)
      *
           MOVE    "3"                 TO  SPA-GMN-D01-PRTSRTKBNL    (3)
           MOVE    "保険、患者番号順"  TO  SPA-GMN-D01-PRTSRTKBNMEIL (3)
      *
           MOVE    "4"                 TO  SPA-GMN-D01-PRTSRTKBNL    (4)
           MOVE    "保険、カナ氏名順"  TO  SPA-GMN-D01-PRTSRTKBNMEIL (4)
      *
           .
       3101-PRTSRTKBNLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目名編集処理
      *****************************************************************
       3101-CHKMSG-HEN-SEC             SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               MOVE    IDX0            TO  WRK-Z9
               MOVE    CONST-LINE      TO  SPA-GMN-D01-CHKMSG (IDX0)
               STRING  WRK-Z9              DELIMITED   BY  SIZE
                       ". "                DELIMITED   BY  SIZE
                       DTCHKKOMOKU-CHKKOMOKU (IDX0)
                                           DELIMITED   BY  SPACE
               INTO    SPA-GMN-D01-CHKMSG (IDX0)
               END-STRING
      *
           END-PERFORM
      *
           .
       3101-CHKMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目コンボ編集処理
      *****************************************************************
       3101-CHKKBNLST-HEN-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-D01-CHKKBNLST-G
           MOVE    1                   TO  SPA-GMN-D01-CHKKBN-MAX
      *
           MOVE    "2"                 TO  SPA-GMN-D01-CHKKBNL    (1)
           MOVE    "確認　全印刷"      TO  SPA-GMN-D01-CHKKBNMEIL (1)
      *
           .
       3101-CHKKBNLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ユーザーチェック区分コンボ編集処理
      *****************************************************************
       3101-USRCHKKBNLST-HEN-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-D01-USRCHKKBNLST-G
                                       SPA-NAI-D01-USRCHKKBNLST-G
      *
           MOVE    ZERO            TO  CNT-CHK
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2002"          TO  CHK004-KANRICD
           MOVE    "%"             TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-2002-REC
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                    OR   ( SPA-GMN-D01-USRCHKKBN-MAX
                                          >=   CONST-USRCHKKBN-MAX )
      *
               COMPUTE SPA-GMN-D01-USRCHKKBN-MAX
                   =   SPA-GMN-D01-USRCHKKBN-MAX   +   1
      *
               MOVE    SPA-GMN-D01-USRCHKKBN-MAX
                                   TO  IDX-CMB
      *
               MOVE    CK4-2002-KBNCD
                                   TO  SPA-GMN-D01-USRCHKKBNL (IDX-CMB)
      *
               MOVE    CK4-2002-NAME
                                   TO  SPA-GMN-D01-USRCHKKBNMEIL
                                                              (IDX-CMB)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
                   EVALUATE    CK4-2002-CHKKBN (IDX0)
                   WHEN    ZERO
                       MOVE    SPACE   TO  SPA-NAI-D01-USRCHKKBN
                                                         (IDX-CMB IDX0)
                   WHEN    SPACE
                       IF    ( IDX0 - 1    =   CNT-CHK )
                           MOVE    "2"
                                       TO  SPA-NAI-D01-USRCHKKBN
                                                         (IDX-CMB IDX0)
                           COMPUTE CNT-CHK =   CNT-CHK     +   1
                       END-IF
                   WHEN    OTHER
                       MOVE    CK4-2002-CHKKBN (IDX0)
                                       TO  SPA-NAI-D01-USRCHKKBN
                                                         (IDX-CMB IDX0)
                       COMPUTE CNT-CHK =   CNT-CHK     +   1
                   END-EVALUATE
               END-PERFORM
      *
               IF    ( CK4-2002-INGAIPROCKBN   =   SPACE )
                   MOVE    "0"         TO  SPA-NAI-D01-USRINGAIPROCKBN
                                                              (IDX-CMB)
               ELSE
                   MOVE    CK4-2002-INGAIPROCKBN
                                   TO  SPA-NAI-D01-USRINGAIPROCKBN
                                                              (IDX-CMB)
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-2002-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-USRCHKKBNLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    院外処方の処理項目コンボ編集処理
      *****************************************************************
       3101-INGAIPROCKBNLST-HEN-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-D01-INGAIPROCKBNLST-G
           MOVE    3               TO  SPA-GMN-D01-INGAIPROCKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-D01-INGAIPROCKBNL    (1)
           MOVE    "含まない"      TO  SPA-GMN-D01-INGAIPROCKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-D01-INGAIPROCKBNL    (2)
           MOVE    "院外処方を含む"
                                   TO  SPA-GMN-D01-INGAIPROCKBNMEIL (2)
           MOVE    "2"             TO  SPA-GMN-D01-INGAIPROCKBNL    (3)
           MOVE    "院外処方のみ"  TO  SPA-GMN-D01-INGAIPROCKBNMEIL (3)
      *
           .
       3101-INGAIPROCKBNLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理（子画面より戻り時）
      *****************************************************************
       312-SPASET-SEC                 SECTION.
      *
           MOVE    SPA-D01-IKTKBT  TO  SPA-NAI-D01-IKTKBT
      *
           MOVE    SPA-D01-SRYYM   TO  SPA-GMN-D01-SRYYM
           IF    ( SPA-D01-SYAHOPROCKBN    =   "1" )
               MOVE    "T"         TO  SPA-GMN-D01-CHK-SYAHO
           END-IF
           IF    ( SPA-D01-KOKUHOPROCKBN   =   "1" )
               MOVE    "T"         TO  SPA-GMN-D01-CHK-KOKUHO
           END-IF
           IF    ( SPA-D01-KOUKIPROCKBN    =   "1" )
               MOVE    "T"         TO  SPA-GMN-D01-CHK-KOUKI
           END-IF
           IF    ( SPA-D01-RSIPROCKBN      =   "1" )
             OR  ( SPA-D01-JBIPROCKBN      =   "1" )
             OR  ( SPA-D01-DAISANPROCKBN   =   "1" )
               MOVE    "T"         TO  SPA-GMN-D01-CHK-RSIJBI
           END-IF
           IF    ( SPA-D01-KGIPROCKBN      =   "1" )
               MOVE    "T"         TO  SPA-GMN-D01-CHK-KOUGAI
           END-IF
           IF    ( SPA-D01-JIHIPROCKBN     =   "1" )
            OR   ( SPA-D01-CHIKENPROCKBN   =   "1" )
               MOVE    "T"         TO  SPA-GMN-D01-CHK-JIHI
           END-IF
           IF    ( SPA-D01-SRYYMDKBN       =   "1" )
               MOVE    "T"         TO  SPA-GMN-D01-CHK-SRYYMD
           END-IF
           MOVE    SPA-D01-STSRYYMD    TO  SPA-GMN-D01-STSRYYMD
                                           SPA-NAI-D01-STSRYYMD-BF
           MOVE    SPA-D01-EDSRYYMD    TO  SPA-GMN-D01-EDSRYYMD
           MOVE    SPA-D01-PRTSRTKBN   TO  SPA-GMN-D01-PRTSRTKBN
           MOVE    SPA-D01-NYUGAIKBN   TO  SPA-GMN-D01-NYUGAIKBN
      *
           MOVE    SPA-D01-USRCHKKBN   TO  SPA-GMN-D01-USRCHKKBN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               IF    ( SPA-D01-CHKKBN (IDX0)   =   ZERO )
                   MOVE    SPACE       TO  SPA-GMN-D01-CHKKBN (IDX0)
               ELSE
                   MOVE    SPA-D01-CHKKBN (IDX0)
                                       TO  SPA-GMN-D01-CHKKBN (IDX0)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-D01-KBTUSER     TO  SPA-NAI-D01-KBTUSER
      *
           MOVE    SPA-D01-INGAIPROCKBN
                               TO  SPA-GMN-D01-INGAIPROCKBN
      *
           .
       312-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＤＩＤ１）ＯＫ処理
      *****************************************************************
       330-DID1-SET-SEC            SECTION.
      *
           IF    ( SPA-DID1-FLG    =   "OK" )
               EVALUATE    SPA-DIDCD
               WHEN    "6666"
                   MOVE    "JOIN"      TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               WHEN    "3002"
                   PERFORM 4501-USRCHKKBN-DEL-SEC
               WHEN    "3001"
                   PERFORM 4601-USRCHKKBN-UPD-SEC
               WHEN    OTHER
      *----(01.00.11)--UPD-START---
      *            ジョブ管理チェック
                   PERFORM 4900-JOBKANRICHK-SEC
                   IF      SPA-ERRCD       =   SPACE
                       PERFORM 490-PRINT-SEC
                   END-IF
      *----(01.00.11)--UPD-END-----
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  SPA-D99-FLG
           MOVE    SPACE               TO  SPA-DID1-FLG
           MOVE    SPACE               TO  SPA-DIDCD
      *
           .
       330-DID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＤＩＤ２）ＯＫ処理
      *****************************************************************
       340-DID2-SET-SEC            SECTION.
      *
           IF    ( SPA-DID1-FLG    NOT =   SPACE  )
      *        ジョブ管理チェック
               PERFORM 4900-JOBKANRICHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 490-PRINT-SEC
               END-IF
           ELSE
               MOVE    SPACE       TO  SPA-DID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-D99-FLG
           MOVE    SPACE           TO  SPA-DIDCD
           .
       340-DID2-SET-EXT.
           EXIT.
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-D99-SET-SEC             SECTION.
      *
           IF    ( SPA-D99-FLG     =   "OK" )
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-D01-CUR
      *     MOVE    ZERO            TO  IDX
      *     MOVE    3               TO  IDY
      *
           MOVE    SPACE           TO  SPA-D99-FLG
           MOVE    SPACE           TO  SPA-DID1-FLG
           MOVE    SPACE           TO  SPA-DIDCD
      *
           .
       340-D99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者選択画面戻り処理
      *****************************************************************
       340-D02-SET-SEC             SECTION.
      *
           IF    ( SPA-DID1-FLG    =   "OK" )
               MOVE    212         TO  SPA-GMN-D01-CUR
           END-IF
      *
           MOVE    SPACE           TO  SPA-DID1-FLG
      *
           PERFORM 311-SPASET-SEC
      *
           PERFORM 312-SPASET-SEC
      *
           IF    ( SPA-NAI-D01-KBTUSER =   ZERO )
      *
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       340-D02-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認項目内容保存画面戻り処理
      *****************************************************************
       340-D03-SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  SPA-DID1-FLG
      *
           PERFORM 311-SPASET-SEC
      *
           PERFORM 312-SPASET-SEC
      *
           MOVE    SPA-D01-CUR     TO  SPA-GMN-D01-CUR
      *
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       340-D03-SET-EXT.
           EXIT.
      *****************************************************************
      *    チェック内容確認画面戻り処理
      *****************************************************************
       340-D04-SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  SPA-DID1-FLG
      *
           PERFORM 311-SPASET-SEC
      *
           PERFORM 312-SPASET-SEC
      *
           MOVE    SPA-D01-CUR     TO  SPA-GMN-D01-CUR
      *
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       340-D04-SET-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    0                   TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-D01-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE          TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE          TO  LNK-XC01-ED-PAGE
           MOVE    "D01"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-D01             TO  LNK-SCRSPA
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       490-PRINT-SEC                       SECTION.
      *
           PERFORM 4901-SET-ORCBSD01PARA-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SPA-D01-UPYMD
           MOVE    SMCNDATE-HMS        TO  SPA-D01-UPHMS
      *
           IF    ( SPA-NAI-D01-IKTKBT  =   "2" )
               PERFORM 4901-SET-CK4-2000-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  490-PRINT-EXT
               END-IF
      *
               PERFORM 4901-SET-CK4-2001-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  490-PRINT-EXT
               END-IF
           END-IF
      *
           CALL    "ORCGDSUB02"    USING
                                   ORCBSD01PARA
                                   SPA-AREA
                                   SPA-D01KYOUTU
      *
           IF    ( SPA-NAI-D01-KBTUSER =   1 )
      *
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *
           END-IF
      *
           MOVE    "2001"             TO  WRK-DELKANRICD
           MOVE    "2%"               TO  WRK-DELKBNCD
           PERFORM 4901-DEL-CHK004-SEC
      *
           MOVE    ZERO        TO  SPA-NAI-D01-KBTUSER
           MOVE    "1"         TO  SPA-NAI-D01-IKTKBT
      *
           PERFORM 490-SYORIKAKUNIN-SEC
      *
           .
       490-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データチェックパラメタ設定処理
      *****************************************************************
       4901-SET-ORCBSD01PARA-SEC       SECTION.
      *
           PERFORM 900-SYS1101-SEL-SEC
      *
           INITIALIZE                  ORCBSD01PARA
           MOVE    SPA-NAI-D01-IKTKBT
                                   TO  ORCBSD01-IKTKBT
           MOVE    SPA-NAI-D01-SRYYM
                                   TO  ORCBSD01-SRYYM
           MOVE    SPA-GMN-D01-NYUGAIKBN
                                   TO  ORCBSD01-NYUGAIKBN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               IF    ( SPA-GMN-D01-CHKKBN (IDX0)   =   SPACE )
                   MOVE    ZERO    TO  ORCBSD01-CHKKBN (IDX0)
               ELSE
                   MOVE    SPA-GMN-D01-CHKKBN (IDX0)
                                   TO  ORCBSD01-CHKKBN (IDX0)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-D01-CHK-SYAHO       =   "T" )
               MOVE    "1"         TO  ORCBSD01-SYAHOPROCKBN
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-KOKUHO      =   "T" )
               MOVE    "1"         TO  ORCBSD01-KOKUHOPROCKBN
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-KOUKI       =   "T" )
               MOVE    "1"         TO  ORCBSD01-KOUKIPROCKBN
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-RSIJBI      =   "T" )
               MOVE    "1"         TO  ORCBSD01-RSIPROCKBN
               MOVE    "1"         TO  ORCBSD01-JBIPROCKBN
               MOVE    "1"         TO  ORCBSD01-DAISANPROCKBN
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-KOUGAI      =   "T" )
               MOVE    "1"         TO  ORCBSD01-KGIPROCKBN
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-JIHI        =   "T" )
               MOVE    "1"         TO  ORCBSD01-JIHIPROCKBN
               MOVE    "1"         TO  ORCBSD01-CHIKENPROCKBN
           END-IF
      *
           MOVE    SPA-GMN-D01-INGAIPROCKBN
                                   TO  ORCBSD01-INGAIPROCKBN
      *
           MOVE    SPA-GMN-D01-PRTSRTKBN
                                   TO  ORCBSD01-PRTSRTKBN
      *
           IF    ( SYS-1101-PRTPTLSTKBN        =   SPACE )
               MOVE    "1"         TO  ORCBSD01-PRTPTLSTKBN
           ELSE
               MOVE    SYS-1101-PRTPTLSTKBN
                                   TO  ORCBSD01-PRTPTLSTKBN
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-SRYYMD      =   "T" )
               MOVE    "1"         TO  ORCBSD01-SRYYMDKBN
               IF    ( SPA-GMN-D01-STSRYYMD    =   ZERO )
                   MOVE    1       TO  ORCBSD01-STSRYYMD
               ELSE
                   MOVE    SPA-GMN-D01-STSRYYMD
                                   TO  ORCBSD01-STSRYYMD
               END-IF
               IF    ( SPA-GMN-D01-EDSRYYMD    =   ZERO )
                   MOVE    SPA-NAI-D01-SRYYM
                                   TO  WRK-SYMD
                   PERFORM 800-GETUMATU-SEC
                   MOVE    WRK-SYMD TO ORCBSD01-EDSRYYMD
               ELSE
                   MOVE    SPA-GMN-D01-EDSRYYMD
                                   TO  ORCBSD01-EDSRYYMD
               END-IF
           END-IF
      *
           .
       4901-SET-ORCBSD01PARA-EXT.
           EXIT.
      *****************************************************************
      *    子画面遷移時基本情報設定処理
      *****************************************************************
       4901-SET-D01-INF-SEC            SECTION.
      *
           INITIALIZE                          SPA-D01-INF-AREA
           MOVE    SPA-NAI-D01-KBTUSER     TO  SPA-D01-KBTUSER
           MOVE    ORCBSD01-IKTKBT         TO  SPA-D01-IKTKBT
           MOVE    ORCBSD01-SRYYM          TO  SPA-D01-SRYYM
           MOVE    ORCBSD01-NYUGAIKBN      TO  SPA-D01-NYUGAIKBN
           MOVE    SPA-GMN-D01-USRCHKKBN   TO  SPA-D01-USRCHKKBN
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
               MOVE    ORCBSD01-CHKKBN (IDX0)
                                           TO  SPA-D01-CHKKBN (IDX0)
      *
           END-PERFORM
      *
           MOVE    ORCBSD01-SYAHOPROCKBN   TO  SPA-D01-SYAHOPROCKBN
           MOVE    ORCBSD01-KOKUHOPROCKBN  TO  SPA-D01-KOKUHOPROCKBN
           MOVE    ORCBSD01-KOUKIPROCKBN   TO  SPA-D01-KOUKIPROCKBN
           MOVE    ORCBSD01-RSIPROCKBN     TO  SPA-D01-RSIPROCKBN
           MOVE    ORCBSD01-KGIPROCKBN     TO  SPA-D01-KGIPROCKBN
           MOVE    ORCBSD01-JBIPROCKBN     TO  SPA-D01-JBIPROCKBN
           MOVE    ORCBSD01-DAISANPROCKBN  TO  SPA-D01-DAISANPROCKBN
           MOVE    ORCBSD01-JIHIPROCKBN    TO  SPA-D01-JIHIPROCKBN
           MOVE    ORCBSD01-CHIKENPROCKBN  TO  SPA-D01-CHIKENPROCKBN
           MOVE    ORCBSD01-INGAIPROCKBN   TO  SPA-D01-INGAIPROCKBN
           MOVE    ORCBSD01-PRTSRTKBN      TO  SPA-D01-PRTSRTKBN
           MOVE    ORCBSD01-SRYYMDKBN      TO  SPA-D01-SRYYMDKBN
           MOVE    ORCBSD01-STSRYYMD       TO  SPA-D01-STSRYYMD
           MOVE    ORCBSD01-EDSRYYMD       TO  SPA-D01-EDSRYYMD
      *
           MOVE    SPA-GMN-D01-CUR         TO  SPA-D01-CUR
      *
           .
       4901-SET-D01-INF-EXT.
           EXIT.
      *****************************************************************
      *    個別患者基本情報設定処理
      *****************************************************************
       4901-SET-CK4-2000-SEC           SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    "2000"          TO  WRK-DELKANRICD
           MOVE    "1%"            TO  WRK-DELKBNCD
           PERFORM 4901-DEL-CHK004-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4901-SET-CK4-2000-EXT
           END-IF
      *
           INITIALIZE                  CK4-2000-REC
           MOVE    SPA-HOSPNUM     TO  CK4-2000-HOSPNUM
           MOVE    "2000"          TO  CK4-2000-KANRICD
           MOVE    "1"             TO  CK4-2000-KBNCD
           MOVE    "00000000"      TO  CK4-2000-STYUKYMD
           MOVE    "99999999"      TO  CK4-2000-EDYUKYMD
           MOVE    ORCBSD01PARA    TO  CK4-2000-TBL
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  CK4-2000-CREYMD
                                       CK4-2000-UPYMD
           MOVE    SMCNDATE-YMD    TO  CK4-2000-UPHMS
           MOVE    CK4-2000-REC    TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4006"      TO  SPA-ERRCD
           END-IF
      *
           .
       4901-SET-CK4-2000-EXT.
           EXIT.
      *****************************************************************
      *    個別指示患者情報設定処理
      *****************************************************************
       4901-SET-CK4-2001-SEC           SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    "2001"          TO  WRK-DELKANRICD
           MOVE    "1%"            TO  WRK-DELKBNCD
           PERFORM 4901-DEL-CHK004-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4901-SET-CK4-2001-EXT
           END-IF
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    "2%"            TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-2001-REC
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                     OR  ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    "1"          TO CK4-2001-KBNCD(1:1)
               MOVE    SMCNDATE-YMD TO CK4-2001-CREYMD
                                       CK4-2001-UPYMD
               MOVE    SMCNDATE-HMS TO CK4-2001-UPHMS
      *
               MOVE    CK4-2001-REC TO MCPDATA-REC
               MOVE    "tbl_chk004" TO MCP-TABLE
               MOVE    "key"        TO MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4005"  TO  SPA-ERRCD
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-2001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4901-SET-CK4-2001-EXT.
           EXIT.
      *****************************************************************
      *    個別処理患者情報削除処理
      *****************************************************************
       4901-DEL-CHK004-SEC             SECTION.
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    WRK-DELKANRICD  TO  CHK004-KANRICD
           MOVE    WRK-DELKBNCD    TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4004"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4901-DEL-CHK004-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "CHK_SYAHO"
           WHEN    "CLICKED"       ALSO    "CHK_KOKUHO"
           WHEN    "CLICKED"       ALSO    "CHK_KOUKI"
           WHEN    "CLICKED"       ALSO    "CHK_RSIJBI"
           WHEN    "CLICKED"       ALSO    "CHK_KOUGAI"
           WHEN    "CLICKED"       ALSO    "CHK_JIHI"
               PERFORM 4101-TARGET-HOKEN-CHK-SEC
           WHEN    "CLICKED"       ALSO    "B01"
      *        戻る
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
      *        クリア
               PERFORM 420-CLEAR-SEC
      *    WHEN    "CLICKED"       ALSO    "B03"
      *        確認
      *        PERFORM 430-KAKUNIN-SEC
           WHEN    "CLICKED"       ALSO    "B04"
      *        確認全印刷
               PERFORM 440-KAKUNIN-ALL-SEC
           WHEN    "CLICKED"       ALSO    "B05S"
      *        確認項目保存内容削除
               PERFORM 450-USRCHKKBN-DEL-SEC
           WHEN    "CLICKED"       ALSO    "B06S"
      *        確認項目保存内容変更
               PERFORM 460-USRCHKKBN-UPD-SEC
           WHEN    "CLICKED"       ALSO    "B07S"
      *        確認項目保存画面遷移
               PERFORM 440-D03-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B05"
      *        再印刷
               PERFORM 450-REPRINT-SEC
           WHEN    "CLICKED"       ALSO    "B06"
      *        患者選択
               PERFORM 460-D02-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B09"
      *        チェック内容確認画面
               PERFORM 490-D04-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B08"
      *        一括・個別切替
               IF    ( SPA-NAI-D01-KBTUSER =   1 )
                   PERFORM 480-IKTKBT-SEC
               END-IF
           WHEN    "CLICKED"       ALSO    "B11"
      *        情報削除
               PERFORM 490-JYOUHOU-DEL-SEC
           WHEN    "CLICKED"       ALSO    "B12"
      *        確定
               PERFORM 490-KAKUTEI-SEC
           WHEN    "CLICKED"       ALSO    "B12S"
      *        処理確認
               PERFORM 490-SYORIKAKUNIN-SEC
           WHEN    "CLICKED"       ALSO    "B12S"
      *        処理確認
               PERFORM 490-SYORIKAKUNIN-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD"
      *        診療日指定チェックボタン押下時の処理
               PERFORM 4100-CHK-SRYYMD-CHK-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    WHEN    "XXXX"
      *        入力個所のセット
      *        PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
      *        PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
           MOVE    ZERO            TO  SPA-GMN-D01-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    処理年月
           MOVE    D01-SRYYM           TO  SPA-GMN-D01-SRYYM
      *
      *    入外区分
           MOVE    D01-NYUGAIKBN       TO  SPA-GMN-D01-NYUGAIKBN-G
      *
      *    印刷順
           MOVE    D01-PRTSRTKBN       TO  SPA-GMN-D01-PRTSRTKBN-G
      *
      *    診療日指定期間
           MOVE    D01-STSRYYMD        TO  SPA-GMN-D01-STSRYYMD
           MOVE    D01-EDSRYYMD        TO  SPA-GMN-D01-EDSRYYMD
      *
           MOVE    D01-USRCHKKBN       TO  SPA-GMN-D01-USRCHKKBN-G
      *
      *    チェック項目
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               MOVE    D01-CHKKBN (IDX0)
                                       TO  SPA-GMN-D01-CHKKBN-G (IDX0)
      *
           END-PERFORM
      *
      *    院外処方の処理項目
           MOVE    D01-INGAIPROCKBN    TO  SPA-GMN-D01-INGAIPROCKBN-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    診療年月チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYYM-CHK-SEC
           END-IF
      *
      *    入外区分処理チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NYUGAIKBN-CHK-SEC
           END-IF
      *
      *    印刷順チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-PRTSRTKBN-CHK-SEC
           END-IF
      *
      *    診療日指定開始日チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-STSRYYMD-CHK-SEC
           END-IF
      *
      *    診療日指定終了日チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-EDSRYYMD-CHK-SEC
           END-IF
      *
      *    ユーザー確認項目チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-USRCHKKBN-CHK-SEC
           END-IF
      *
      *    チェック項目チェック
           PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   DTCHKKOMOKU-MAX )
      *
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 4101-CHKKBN-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
      *    院外処方の処理項目チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-INGAIPROCKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象保険チェック処理
      *****************************************************************
       4101-TARGET-HOKEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-D01-CUR
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    D01-CHK-SYAHO       TO  SPA-GMN-D01-CHK-SYAHO
           MOVE    D01-CHK-KOKUHO      TO  SPA-GMN-D01-CHK-KOKUHO
           MOVE    D01-CHK-KOUKI       TO  SPA-GMN-D01-CHK-KOUKI
           MOVE    D01-CHK-RSIJBI      TO  SPA-GMN-D01-CHK-RSIJBI
           MOVE    D01-CHK-KOUGAI      TO  SPA-GMN-D01-CHK-KOUGAI
           MOVE    D01-CHK-JIHI        TO  SPA-GMN-D01-CHK-JIHI
      *
           .
       4101-TARGET-HOKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック
      *****************************************************************
       4101-SRYYM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-D01-SRYYM
      *
           IF    ( SPA-GMN-D01-SRYYM   =   SPACE )
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-D01-SRYYM
               MOVE    SPA-SYSYMD(1:6)     TO  SPA-NAI-D01-SRYYM
               GO  TO  4101-SRYYM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-D01-SRYYM   TO  WRK-YMD
           PERFORM 5003-NENGETSU-CHK-SEC
           IF    ( FLG-HIZUKE-ERR  NOT =   ZERO )
               MOVE    1               TO  SPA-GMN-D01-CUR
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4101-SRYYM-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-D01-SRYYM
           MOVE    WRK-SYMD    (1:6)   TO  SPA-NAI-D01-SRYYM
      *
           IF    ( CONST-H2004         >   SPA-NAI-D01-SRYYM )
               MOVE    "F"             TO  SPA-GMN-D01-CHK-KOUKI
           END-IF
      *
           .
       4101-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       4101-NYUGAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-D01-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "2"                 TO  WRK-CMB-CD
               PERFORM 4200-NYUGAIKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-D01-NYUGAIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-D01-NYUGAIKBN-G
           END-IF
      *
           IF    ( SPA-GMN-D01-NYUGAIKBN   =   "1" )
               MOVE    "F"                 TO  SPA-GMN-D01-CHK-SRYYMD
               MOVE    ZERO                TO  SPA-GMN-D01-STSRYYMD
                                               SPA-GMN-D01-EDSRYYMD
           END-IF
      *
           .
       4101-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷順区分チェック処理
      *****************************************************************
       4101-PRTSRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-D01-PRTSRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-PRTSRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-D01-PRTSRTKBNLST (1)
                                   TO  SPA-GMN-D01-PRTSRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-D01-PRTSRTKBN-G
           END-IF
      *
           .
      *
       4101-PRTSRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日指定開始日チェック処理
      *****************************************************************
       4101-STSRYYMD-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-D01-STSRYYMD        =   "99" )
               MOVE    SPA-SYSYMD              TO  WRK-SRYYMD
               MOVE    WRK-SRYYMD-DD           TO  SPA-GMN-D01-STSRYYMD
                                                   SPA-GMN-D01-EDSRYYMD
           END-IF
      *
           IF    ( SPA-GMN-D01-STSRYYMD    NOT =   ZERO )
               MOVE    SPA-NAI-D01-SRYYM       TO  WRK-YMD
               MOVE    SPA-GMN-D01-STSRYYMD    TO  WRK-YMD (7:2)
               PERFORM 800-YMD-CHK-SEC
               IF    ( FLG-HIZUKE-ERR  =   ZERO )
                   IF    ( SPA-GMN-D01-EDSRYYMD    =   ZERO )
                    AND  ( SPA-NAI-D01-STSRYYMD-BF =   ZERO )
                       MOVE    SPA-GMN-D01-STSRYYMD
                                               TO  SPA-GMN-D01-EDSRYYMD
                   END-IF
               ELSE
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    107         TO  SPA-GMN-D01-CUR
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-D01-STSRYYMD    TO  SPA-NAI-D01-STSRYYMD-BF
      *
           .
      *
       4101-STSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日指定終了日チェック処理
      *****************************************************************
       4101-EDSRYYMD-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-D01-EDSRYYMD    NOT =   ZERO )
               MOVE    SPA-NAI-D01-SRYYM       TO  WRK-YMD
               MOVE    SPA-GMN-D01-EDSRYYMD    TO  WRK-YMD (7:2)
               PERFORM 800-YMD-CHK-SEC
               IF    ( FLG-HIZUKE-ERR  =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    108         TO  SPA-GMN-D01-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-EDSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー確認項目チェック処理
      *****************************************************************
       4101-USRCHKKBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-D01-USRCHKKBN-SEL
      *
           MOVE    SPA-GMN-D01-USRCHKKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-USRCHKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-D01-USRCHKKBN-G
           MOVE    WRK-CMB-IDX         TO  SPA-NAI-D01-USRCHKKBN-SEL
                                           IDX-CMB
      *
           IF    ( WRK-MCP-WIDGET      =   "USRCHKKBN" )
             AND ( IDX-CMB            >    ZERO )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
                   MOVE    SPA-NAI-D01-USRCHKKBN (IDX-CMB IDX0)
                                       TO  SPA-GMN-D01-CHKKBN (IDX0)
               END-PERFORM
      *
               MOVE    SPA-NAI-D01-USRINGAIPROCKBN(IDX-CMB)
                                       TO  SPA-GMN-D01-INGAIPROCKBN
      *
           END-IF
      *
           .
       4101-USRCHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目チェック
      *****************************************************************
       4101-CHKKBN-CHK-SEC             SECTION.
      *
           COMPUTE WRK-ERR-CUR     =   IDXA    +   2
      *
           IF    ( SPA-GMN-D01-CHKKBN-G (IDXA) =   SPACE )
               GO  TO  4101-CHKKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-D01-CHKKBN (IDXA)
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-D01-CHKKBN-G (IDXA)
      *
           .
       4101-CHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方の処理項目チェック処理
      *****************************************************************
       4101-INGAIPROCKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-D01-INGAIPROCKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-INGAIPROCKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPACE           TO  WRK-CMB-CD
               PERFORM 4200-INGAIPROCKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-D01-INGAIPROCKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-D01-INGAIPROCKBN-G
           END-IF
      *
           .
       4101-INGAIPROCKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日指定ボタン押下時の処理
      *****************************************************************
       4100-CHK-SRYYMD-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-D01-CUR
      *
           MOVE    D01-CHK-SRYYMD      TO  SPA-GMN-D01-CHK-SRYYMD
      *
           PERFORM 410-INPUT-CHK-SEC
      *
      *    IF    ( SPA-GMN-D01-CHK-SRYYMD  =   "T" )
      *     AND  ( SPA-SYSYMD (1:6)    =   SPA-NAI-D01-SRYYM )
      *        MOVE    SPA-SYSYMD (7:2)
      *                                TO  SPA-GMN-D01-STSRYYMD
      *                                    SPA-GMN-D01-EDSRYYMD
      *    ELSE
               MOVE    ZERO            TO  SPA-GMN-D01-STSRYYMD
                                           SPA-GMN-D01-EDSRYYMD
      *    END-IF
      *
           .
       4100-CHK-SRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 900-SYS3003-SEL-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               PERFORM VARYING IDX0 FROM   1   BY  1
                       UNTIL ( IDX0 >      SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"       TO  SJOBKANRI-MODE
                   INITIALIZE              JOBKANRI-REC
                   MOVE    IDX0        TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                       TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       IF    ( JOB-ENDYMD      =   SPACE )
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"      TO  SPA-DIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "D01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-KYOTUKEY
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
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                      SPA-GMN-D01-INPUT-AREA
                                           SPA-NAI-D01-INPUT-AREA
      *
           MOVE    "1"                 TO  SPA-NAI-D01-IKTKBT
      *
           MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-D01-SRYYM
           MOVE    SPA-SYSYMD(1:6)     TO  SPA-NAI-D01-SRYYM
      *
           MOVE    "T"                 TO  SPA-GMN-D01-CHK-SYAHO
           MOVE    "T"                 TO  SPA-GMN-D01-CHK-KOKUHO
           MOVE    "T"                 TO  SPA-GMN-D01-CHK-KOUKI
           IF    ( CONST-H2004         >   SPA-NAI-D01-SRYYM )
               MOVE    "F"             TO  SPA-GMN-D01-CHK-KOUKI
           END-IF
           MOVE    "T"                 TO  SPA-GMN-D01-CHK-RSIJBI
           MOVE    "T"                 TO  SPA-GMN-D01-CHK-KOUGAI
           MOVE    "F"                 TO  SPA-GMN-D01-CHK-JIHI
           MOVE    "F"                 TO  SPA-GMN-D01-CHK-SRYYMD
      *
      *    入外区分のデフォルトを外来として設定
           MOVE    "2"                 TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-D01-NYUGAIKBN-G
      *
           MOVE    1                   TO  SPA-GMN-D01-CUR
      *
           PERFORM 900-SYS1101-SEL-SEC
      *
           MOVE    SYS-1101-INGAIPROCKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-INGAIPROCKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-D01-INGAIPROCKBNLST (2)
                                   TO  SPA-GMN-D01-INGAIPROCKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-D01-INGAIPROCKBN-G
           END-IF
      *
           MOVE    SYS-1101-PRTSRTKBN  TO  WRK-CMB-CD
           PERFORM 4200-PRTSRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-D01-PRTSRTKBNLST (1)
                                   TO  SPA-GMN-D01-PRTSRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-D01-PRTSRTKBN-G
           END-IF
      *
           .
      *
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    確認全印刷
      *****************************************************************
       440-KAKUNIN-ALL-SEC             SECTION.
      *
           MOVE    "2"                 TO  WRK-CMB-CD
           PERFORM 4200-CHKKBNLST-CHK-SEC
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-D01-CHKKBN-G (IDX0)
      *
           END-PERFORM
      *
           .
      *
       440-KAKUNIN-ALL-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー確認項目削除処理
      *****************************************************************
       450-USRCHKKBN-DEL-SEC           SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-USRCHKKBN-DEL-EXT
           END-IF
      *
           MOVE    "3002"          TO  SPA-DIDCD
      *
           .
       450-USRCHKKBN-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー確認項目削除処理
      *****************************************************************
       4501-USRCHKKBN-DEL-SEC          SECTION.
      *
           INITIALIZE                      CHK004-REC
           MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
           MOVE    "2002"              TO  CHK004-KANRICD
           MOVE    SPA-GMN-D01-USRCHKKBN
                                       TO  CHK004-KBNCD
           MOVE    "00000000"          TO  CHK004-STYUKYMD
           MOVE    "99999999"          TO  CHK004-EDYUKYMD
           MOVE    CHK004-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4009"      TO  SPA-ERRCD
           END-IF
      *
           INITIALIZE                  WRK-KBNCD-G
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2002"          TO  CHK004-KANRICD
           MOVE    "%"             TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               COMPUTE WRK-KBNCD-NUM   =   WRK-KBNCD-NUM  +   1
      *
               IF    ( CHK004-KBNCD    NOT =   WRK-KBNCD-G )
                   MOVE    CHK004-KEY      TO  CHK004-UPDKEY
                   MOVE    WRK-KBNCD-G     TO  CHK004-KBNCD
                   MOVE    SMCNDATE-YMD    TO  CK4-2002-UPYMD
                   MOVE    SMCNDATE-HMS    TO  CK4-2002-UPHMS
      *
                   MOVE    CHK004-REC      TO  MCPDATA-REC
                   MOVE    "tbl_chk004"    TO  MCP-TABLE
                   MOVE    "upd1"          TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "4010"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 3101-USRCHKKBNLST-HEN-SEC
               MOVE    SPACE       TO  SPA-GMN-D01-USRCHKKBN-G
               MOVE    ZERO        TO  SPA-NAI-D01-USRCHKKBN-SEL
           END-IF
      *
           .
       4501-USRCHKKBN-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー確認項目変更処理
      *****************************************************************
       460-USRCHKKBN-UPD-SEC           SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  460-USRCHKKBN-UPD-EXT
           END-IF
      *
           MOVE    "3001"          TO  SPA-DIDCD
      *
           .
       460-USRCHKKBN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー確認項目変更処理
      *****************************************************************
       4601-USRCHKKBN-UPD-SEC          SECTION.
      *
           PERFORM 4901-SET-ORCBSD01PARA-SEC
      *
           INITIALIZE                      CHK004-REC
           MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
           MOVE    "2002"              TO  CHK004-KANRICD
           MOVE    SPA-GMN-D01-USRCHKKBN
                                       TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                           CHK004-EDYUKYMD
      *
           PERFORM 900-CHK004-KEY-SEL-SEC
           IF    ( FLG-CHK004          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4007"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               MOVE    CHK004-REC          TO  CK4-2002-REC
      *
               MOVE    ORCBSD01-CHKKBN-G   TO  CK4-2002-CHKKBN-G
               MOVE    ORCBSD01-INGAIPROCKBN
                                           TO  CK4-2002-INGAIPROCKBN
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  CK4-2002-UPYMD
               MOVE    SMCNDATE-HMS        TO  CK4-2002-UPHMS
      *
               MOVE    CK4-2002-REC        TO  MCPDATA-REC
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4008"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 3101-USRCHKKBNLST-HEN-SEC
           END-IF
      *
           .
       4601-USRCHKKBN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    再印刷処理
      *****************************************************************
       450-REPRINT-SEC                 SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-D01             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
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
      *
       450-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    確認項目保存画面遷移処理
      *****************************************************************
       440-D03-SENI-SEC                SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  460-D02-SENI-EXT
           END-IF
      *
           PERFORM 4901-SET-ORCBSD01PARA-SEC
      *
           PERFORM 4901-SET-D01-INF-SEC
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D01             TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGD03"       USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D01
      *
           MOVE    "NAME"              TO  MCP-WIDGET
      *
           MOVE    "D01"                   TO  SPA-MOTOPG
           MOVE    "D03"                   TO  SPA-SAKIPG
      *
           MOVE    "NEW"                   TO  MCP-PUTTYPE.
           MOVE    "D03    "               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       440-D03-SENI-EXT.
           EXIT.
      *****************************************************************
      *    患者選択処理
      *****************************************************************
       460-D02-SENI-SEC                SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  460-D02-SENI-EXT
           END-IF
      *
           PERFORM 990-LOCK-IN-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  460-D02-SENI-EXT
           END-IF
      *
           IF    ( SLOCK-INSERT        =   "1" )
               MOVE    "2001"             TO  WRK-DELKANRICD
               MOVE    "2%"               TO  WRK-DELKBNCD
               PERFORM 4901-DEL-CHK004-SEC
           END-IF
      *
           PERFORM 4901-SET-ORCBSD01PARA-SEC
      *
           PERFORM 4901-SET-D01-INF-SEC
      *
           MOVE    "D01"                   TO  SPA-MOTOPG
           MOVE    "D02"                   TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"                 TO  MCP-PUTTYPE.
           MOVE   "D02    "                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       460-D02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェック内容確認画面遷移処理
      *****************************************************************
       490-D04-SENI-SEC                SECTION.
      *
           PERFORM 4901-SET-ORCBSD01PARA-SEC
      *
           PERFORM 4901-SET-D01-INF-SEC
      *
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "D04"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "D04"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       490-D04-SENI-EXT.
           EXIT.
      *****************************************************************
      *    一括・個別切替処理
      *****************************************************************
       480-IKTKBT-SEC                  SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-NAI-D01-IKTKBT  =   "1" )
               MOVE    "2"             TO  SPA-NAI-D01-IKTKBT
           ELSE
               MOVE    "1"             TO  SPA-NAI-D01-IKTKBT
           END-IF
      *
           .
      *
       480-IKTKBT-EXT.
           EXIT.
      *****************************************************************
      *    情報削除
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-D01             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-JOB-SHELLID   TO  SPA-JOB-SHELLID
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
      *
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    確定
      *****************************************************************
       490-KAKUTEI-SEC                 SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    ジョブ管理チェック
           PERFORM 4900-JOBKANRICHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    関連チェック
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
           MOVE    "1001"              TO  SPA-DIDCD
      *
           .
      *
       490-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    処理確認
      *****************************************************************
       490-SYORIKAKUNIN-SEC            SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPACE               TO  D99
           INITIALIZE                      D99
           MOVE    ZERO                TO  D99-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  D99-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           PERFORM 900-SYS3003-SEL-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >       SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX0            TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       MOVE    JOB-JOBID       TO  D99-RENNUM   (IDX0)
                       MOVE    JOB-SHELLMSG    TO  D99-SHORINM  (IDX0)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  D99-STARTTIME(IDX0)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  D99-ENDTIME  (IDX0)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  D99-PAGE     (IDX0)
                       MOVE    JOB-YOBI        TO  D99-ERRMSG   (IDX0)
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  D99-MSG
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE )
                               CONTINUE
                           ELSE
                               MOVE   SPACE        TO  D99-MSG
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE
                                             INTO  D99-MSG
                               END-STRING
                           END-IF
                       END-IF
                       MOVE    IDX0            TO  D99-COUNT
      *
                       IF    ( JOB-JOBID   =   SPA-D01-JOB-JOBID )
                           MOVE    JOB-UPDCNT  TO  WRK-PRT-PAGE
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           IF    ( D99-MSG             =   SPACE )
               IF    ( D99-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                               TO  D99-MSG
                   IF    ( SPA-DID1-FLG    =   "PV" )
                    AND  ( WRK-PRT-PAGE    >   ZERO )
                       MOVE    WIDGET-NORMAL    TO  D99-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
                   ELSE
                       MOVE    "B01"            TO  MCP-WIDGET
                   END-IF
               ELSE
                   MOVE    "該当する処理はありません"
                                               TO  D99-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-DID1-FLG    =   "PV" )
               MOVE    "F12:プレビュー"    TO  D99-B12-LABEL
           END-IF
      *
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "D99"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "D99"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
      *
       490-SYORIKAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4900-JOBKANRICHK-SEC            SECTION.
      *
      *    ジョブ管理チェック
           PERFORM 900-SYS3003-SEL-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
               PERFORM VARYING IDX0 FROM   1   BY  1
                       UNTIL ( IDX0 >      SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX0            TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN
                                           =   ZERO )
                       IF    ( JOB-ENDYMD  =   SPACE )
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-ERRMSG
                           STRING  "指定された条件でチェック実行中"
                                               DELIMITED  BY  SIZE
                                   "のためしばらくお待ちください"
                                               DELIMITED  BY  SIZE
                           INTO  WRK-ERRMSG
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX0
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4900-JOBKANRICHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *    診療年月が入力されていること
           IF    ( SPA-GMN-D01-SRYYM   =   SPACE )
               MOVE    1               TO  SPA-GMN-D01-CUR
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    保険がいずれか一つは入力されていること
           IF    ( SPA-GMN-D01-CHK-SYAHO   =   "F" )
            AND  ( SPA-GMN-D01-CHK-KOKUHO  =   "F" )
            AND  ( SPA-GMN-D01-CHK-KOUKI   =   "F" )
            AND  ( SPA-GMN-D01-CHK-RSIJBI  =   "F" )
            AND  ( SPA-GMN-D01-CHK-KOUGAI  =   "F" )
            AND  ( SPA-GMN-D01-CHK-JIHI    =   "F" )
               MOVE    "1003"          TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-SRYYMD  =   "T" )
               IF    ( SPA-GMN-D01-STSRYYMD    =   ZERO )
                AND  ( SPA-GMN-D01-EDSRYYMD    =   ZERO )
                   MOVE    "1004"      TO  SPA-ERRCD
                   MOVE    107         TO  SPA-GMN-D01-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
      *
               IF    ( SPA-GMN-D01-STSRYYMD    NOT =   ZERO )
                AND  ( SPA-GMN-D01-EDSRYYMD    NOT =   ZERO )
                   IF    ( SPA-GMN-D01-STSRYYMD
                                               >   SPA-GMN-D01-EDSRYYMD)
                       MOVE    "1005"      TO  SPA-ERRCD
                       MOVE    107         TO  SPA-GMN-D01-CUR
                       GO  TO  4900-KANRENCHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    確認項目がいずれか一つは入力されていること
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   DTCHKKOMOKU-MAX )
                    OR   ( SPA-GMN-D01-CHKKBN (IDX0)
                                   NOT =   SPACE )
                CONTINUE
           END-PERFORM
      *
           IF    ( IDX0    >   DTCHKKOMOKU-MAX )
               MOVE    "1002"          TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボチェック処理
      *****************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-D01-NYUGAIKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-D01-NYUGAIKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-D01-NYUGAIKBN-MAX )
               MOVE    SPA-GMN-D01-NYUGAIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               MOVE    IDX-CMB         TO  WRK-CMB-IDX
           END-IF
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力順区分コンボチェック処理
      *****************************************************************
       4200-PRTSRTKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-D01-PRTSRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-D01-PRTSRTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-D01-PRTSRTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-PRTSRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目コンボチェック処理
      *****************************************************************
       4200-CHKKBNLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           IF    ( WRK-CMB-CD      =   "1" )
               MOVE    "2"             TO  WRK-CMB-CD
           END-IF
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-D01-CHKKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-D01-CHKKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-D01-CHKKBN-MAX )
               MOVE    SPA-GMN-D01-CHKKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               MOVE    IDX-CMB         TO  WRK-CMB-IDX
           END-IF
      *
           .
       4200-CHKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー確認項目コンボチェック処理
      *****************************************************************
       4200-USRCHKKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-D01-USRCHKKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-D01-USRCHKKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-D01-USRCHKKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-USRCHKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方の処理項目コンボチェック処理
      *****************************************************************
       4200-INGAIPROCKBNLST-CHK-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-D01-INGAIPROCKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-D01-INGAIPROCKBNL
                                                           (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-D01-INGAIPROCKBN-MAX )
               MOVE    SPA-GMN-D01-INGAIPROCKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               MOVE    IDX-CMB         TO  WRK-CMB-IDX
           END-IF
      *
           .
       4200-INGAIPROCKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-DIDCD       NOT =   SPACE )
               EVALUATE    SPA-DIDCD
               WHEN    "1001"
                   PERFORM 520-DID2SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-DID1SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "D01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           INITIALIZE                      D01
      *
      *    診療年月
           MOVE    SPA-GMN-D01-SRYYM   TO  D01-SRYYM
      *
      *    入外区分
           MOVE    SPA-GMN-D01-NYUGAIKBN-G
                                       TO  D01-NYUGAIKBN
      *
      *    入外区分コンボ
           MOVE    SPA-GMN-D01-NYUGAIKBN-MAX
                                       TO  D01-NYUGAIKBN-COUNT
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-D01-NYUGAIKBN-MAX )
      *
               MOVE    SPA-GMN-D01-NYUGAIKBNLST (IDXA)
                                       TO  D01-NYUGAIKBN-ITEM (IDXA)
      *
           END-PERFORM
      *
      *    印刷順
           MOVE    SPA-GMN-D01-PRTSRTKBN-G
                                       TO  D01-PRTSRTKBN
      *
      *    印刷順コンボ
           MOVE    SPA-GMN-D01-PRTSRTKBN-MAX
                                       TO  D01-PRTSRTKBN-COUNT
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-D01-PRTSRTKBN-MAX )
      *
               MOVE    SPA-GMN-D01-PRTSRTKBNLST (IDXA)
                                       TO  D01-PRTSRTKBN-ITEM (IDXA)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-D01-CHK-SRYYMD  TO  D01-CHK-SRYYMD
           MOVE    SPA-GMN-D01-STSRYYMD    TO  D01-STSRYYMD
           MOVE    SPA-GMN-D01-EDSRYYMD    TO  D01-EDSRYYMD
      *
      *    ユーザー確認項目
           MOVE    SPA-GMN-D01-USRCHKKBN-G
                                           TO  D01-USRCHKKBN
      *
      *    ユーザー確認項目コンボ
           MOVE    SPA-GMN-D01-USRCHKKBN-MAX
                                           TO  D01-USRCHKKBN-COUNT
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-D01-USRCHKKBN-MAX )
      *
               MOVE    SPA-GMN-D01-USRCHKKBNLST (IDXA)
                                           TO  D01-USRCHKKBN-ITEM (IDXA)
      *
           END-PERFORM
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   DTCHKKOMOKU-MAX )
      *
      *        チェック項目名
               MOVE    SPA-GMN-D01-CHKMSG (IDXA)
                                       TO  D01-CHKMSG (IDXA)
      *        チェック項目
               MOVE    SPA-GMN-D01-CHKKBN-G (IDXA)
                                       TO  D01-CHKKBN (IDXA)
      *
      *        チェック項目コンボ
               MOVE    SPA-GMN-D01-CHKKBN-MAX
                                       TO  D01-CHKKBN-COUNT (IDXA)
      *
               PERFORM VARYING IDXB    FROM    1   BY  1
                       UNTIL ( IDXB    >   SPA-GMN-D01-CHKKBN-MAX )
      *
                   MOVE    SPA-GMN-D01-CHKKBNLST (IDXB)
                                       TO  D01-CHKKBN-ITEM (IDXA IDXB)
      *
               END-PERFORM
      *
           END-PERFORM
      *
      *    チェック対象保険
           MOVE    SPA-GMN-D01-CHK-SYAHO   TO  D01-CHK-SYAHO
           MOVE    SPA-GMN-D01-CHK-KOKUHO  TO  D01-CHK-KOKUHO
           MOVE    SPA-GMN-D01-CHK-KOUKI   TO  D01-CHK-KOUKI
           MOVE    SPA-GMN-D01-CHK-RSIJBI  TO  D01-CHK-RSIJBI
           MOVE    SPA-GMN-D01-CHK-KOUGAI  TO  D01-CHK-KOUGAI
           MOVE    SPA-GMN-D01-CHK-JIHI    TO  D01-CHK-JIHI
      *
      *    院外処方の処理項目名
           MOVE    SPA-GMN-D01-INGAIPROCMSG
                                       TO  D01-INGAIPROCMSG
      *    院外処方の処理項目
           MOVE    SPA-GMN-D01-INGAIPROCKBN-G
                                       TO  D01-INGAIPROCKBN
      *
      *    院外処方の処理項目コンボ
           MOVE    SPA-GMN-D01-INGAIPROCKBN-MAX
                                       TO  D01-INGAIPROCKBN-COUNT
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-D01-INGAIPROCKBN-MAX )
      *
               MOVE    SPA-GMN-D01-INGAIPROCKBNLST (IDXA)
                                       TO  D01-INGAIPROCKBN-ITEM (IDXA)
      *
           END-PERFORM
      *
           MOVE    "black"             TO  D01-LBLIKTKBT-STYLE
      *
           IF    ( SPA-NAI-D01-KBTUSER     =   1 )
               IF    ( SPA-NAI-D01-IKTKBT  =   "1" )
                   MOVE    "　一括チェック　"  TO  D01-LBLIKTKBT
               ELSE
                   MOVE    "blue"              TO  D01-LBLIKTKBT-STYLE
                   MOVE    "＊個別チェック＊"  TO  D01-LBLIKTKBT
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-D01-KBTUSER     =   1 )
               MOVE    "一括／個別"        TO  D01-B08-LABEL
           END-IF
      *
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  D01-B05S-STATE
                                           D01-B06S-STATE
                                           D01-B07S-STATE
                                           D01-CHK-KOUKI-STATE
                                           D01-CHK-SRYYMD-STATE
                                           D01-STSRYYMD-STATE
                                           D01-EDSRYYMD-STATE
      *
           IF    ( SPA-GMN-D01-NYUGAIKBN   =   "1" )
               MOVE    WIDGET-INSENSITIVE  TO  D01-CHK-SRYYMD-STATE
                                               D01-STSRYYMD-STATE
                                               D01-EDSRYYMD-STATE
           END-IF
      *
           IF    ( CONST-H2004             >   SPA-NAI-D01-SRYYM )
               MOVE    WIDGET-INSENSITIVE  TO  D01-CHK-KOUKI-STATE
           END-IF
      *
           IF    ( SPA-GMN-D01-CHK-SRYYMD  =   "F" )
               MOVE    WIDGET-INSENSITIVE  TO  D01-STSRYYMD-STATE
                                               D01-EDSRYYMD-STATE
           END-IF
      *
           IF    ( SPA-NAI-D01-USRCHKKBN-SEL   =    ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  D01-B05S-STATE
                                               D01-B06S-STATE
           END-IF
      *
           IF    ( SPA-GMN-D01-USRCHKKBN-MAX
                                          >=   CONST-USRCHKKBN-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  D01-B07S-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-D01-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-D01-CUR
           WHEN    01
               MOVE    "SRYYM"         TO  MCP-WIDGET
           WHEN    02
               MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
           WHEN    03
               MOVE    "CHKKBN01"      TO  MCP-WIDGET
           WHEN    04
               MOVE    "CHKKBN02"      TO  MCP-WIDGET
           WHEN    05
               MOVE    "CHKKBN03"      TO  MCP-WIDGET
           WHEN    06
               MOVE    "CHKKBN04"      TO  MCP-WIDGET
           WHEN    07
               MOVE    "CHKKBN05"      TO  MCP-WIDGET
           WHEN    08
               MOVE    "CHKKBN06"      TO  MCP-WIDGET
           WHEN    09
               MOVE    "CHKKBN07"      TO  MCP-WIDGET
           WHEN    10
               MOVE    "CHKKBN08"      TO  MCP-WIDGET
           WHEN    11
               MOVE    "CHKKBN09"      TO  MCP-WIDGET
           WHEN    12
               MOVE    "CHKKBN10"      TO  MCP-WIDGET
           WHEN    13
               MOVE    "CHKKBN11"      TO  MCP-WIDGET
           WHEN    14
               MOVE    "CHKKBN12"      TO  MCP-WIDGET
           WHEN    15
               MOVE    "CHKKBN13"      TO  MCP-WIDGET
           WHEN    16
               MOVE    "CHKKBN14"      TO  MCP-WIDGET
           WHEN    17
               MOVE    "CHKKBN15"      TO  MCP-WIDGET
           WHEN    18
               MOVE    "CHKKBN16"      TO  MCP-WIDGET
           WHEN    19
               MOVE    "CHKKBN17"      TO  MCP-WIDGET
           WHEN    20
               MOVE    "CHKKBN18"      TO  MCP-WIDGET
           WHEN    21
               MOVE    "CHKKBN19"      TO  MCP-WIDGET
           WHEN    22
               MOVE    "CHKKBN20"      TO  MCP-WIDGET
           WHEN    23
               MOVE    "CHKKBN21"      TO  MCP-WIDGET
           WHEN    51
               MOVE    "INGAIPROCKBN"  TO  MCP-WIDGET
           WHEN    101
               MOVE    "CHK_SYAHO"     TO  MCP-WIDGET
           WHEN    102
               MOVE    "CHK_KOKUHO"    TO  MCP-WIDGET
           WHEN    103
               MOVE    "CHK_RSIJBI"    TO  MCP-WIDGET
           WHEN    104
               MOVE    "CHK_JIHI"      TO  MCP-WIDGET
           WHEN    105
               MOVE    "PRTSRTKBN"     TO  MCP-WIDGET
           WHEN    106
               MOVE    "CHK_SRYYMD"    TO  MCP-WIDGET
           WHEN    107
               MOVE    "STSRYYMD"      TO  MCP-WIDGET
           WHEN    108
               MOVE    "EDSRYYMD"      TO  MCP-WIDGET
           WHEN    109
               MOVE    "USRCHKKBN"     TO  MCP-WIDGET
           WHEN    110
               MOVE    "CHK_KOUGAI"    TO  MCP-WIDGET
           WHEN    111
               MOVE    "CHK_KOUKI"     TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
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
               MOVE    02              TO  SPA-GMN-D01-CUR
           WHEN    "NYUGAIKBN"
               MOVE    109             TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN01"
               MOVE    04              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN02"
               MOVE    05              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN03"
               MOVE    06              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN04"
               MOVE    07              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN05"
               MOVE    08              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN06"
               MOVE    09              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN07"
               MOVE    10              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN08"
               MOVE    11              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN09"
               MOVE    12              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN10"
               MOVE    13              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN11"
               MOVE    14              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN12"
               MOVE    15              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN13"
               MOVE    16              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN14"
               MOVE    17              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN15"
               MOVE    18              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN16"
               MOVE    19              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN17"
               MOVE    20              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN18"
               MOVE    21              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN19"
               MOVE    22              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN20"
               MOVE    23              TO  SPA-GMN-D01-CUR
           WHEN    "CHKKBN21"
               MOVE    51              TO  SPA-GMN-D01-CUR
           WHEN    "CHK_SYAHO"
               MOVE    102             TO  SPA-GMN-D01-CUR
           WHEN    "CHK_KOKUHO"
               MOVE    111             TO  SPA-GMN-D01-CUR
           WHEN    "CHK_KOUKI"
               MOVE    103             TO  SPA-GMN-D01-CUR
           WHEN    "CHK_RSIJBI"
               MOVE    110             TO  SPA-GMN-D01-CUR
           WHEN    "CHK_KOUGAI"
               MOVE    104             TO  SPA-GMN-D01-CUR
           WHEN    "CHK_JIHI"
               MOVE    106             TO  SPA-GMN-D01-CUR
           WHEN    "INGAIPROCKBN"
               MOVE    212             TO  SPA-GMN-D01-CUR
           WHEN    "PRTSRTKBN"
               MOVE    003             TO  SPA-GMN-D01-CUR
           WHEN    "CHK_SRYYMD"
               MOVE    107             TO  SPA-GMN-D01-CUR
           WHEN    "STSRYYMD"
               MOVE    108             TO  SPA-GMN-D01-CUR
           WHEN    "EDSRYYMD"
               MOVE    105             TO  SPA-GMN-D01-CUR
           WHEN    "USRCHKKBN"
               MOVE    101             TO  SPA-GMN-D01-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
           IF    ( SPA-GMN-D01-CUR     =   111 )
               IF    ( D01-CHK-KOUKI-STATE     =   WIDGET-INSENSITIVE )
                   MOVE    103         TO  SPA-GMN-D01-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-D01-CUR     =   106 )
               IF    ( D01-CHK-SRYYMD-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    107         TO  SPA-GMN-D01-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-D01-CUR     =   107 )
               IF    ( D01-STSRYYMD-STATE      =   WIDGET-INSENSITIVE )
                   MOVE    108         TO  SPA-GMN-D01-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-D01-CUR     =   108 )
               IF    ( D01-EDSRYYMD-STATE      =   WIDGET-INSENSITIVE )
                   MOVE    105         TO  SPA-GMN-D01-CUR
               END-IF
           END-IF
      *
           .
      *
       5011-INPUT-CUR-SKIP-EXT.
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
               MOVE    "診療年月入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "確認項目入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "診療日指定開始日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "診療日指定終了日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "診療年月を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "確認項目を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "診療日を指定してください"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "診療日の指定に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "4001"              THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "保険を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    WRK-ERRMSG      TO  SPA-ERRMSG
           WHEN    "9999"
               MOVE    "他端末で使用中です。"
                                       TO  SPA-ERRMSG
               MOVE    SLOCK-MSG       TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  DERR
           MOVE    SPA-ERRCD           TO  DERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  DERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "DERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DERR"              TO  MCP-WINDOW
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
       520-DID1SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-DIDMSG
      *
           EVALUATE    SPA-DIDCD
           WHEN    "3001"
               MOVE    "選択中の確認項目設定を変更します"
                                       TO  WRK-DIDMSG
           WHEN    "3002"
               MOVE    "選択中の確認項目設定を削除します"
                                       TO  WRK-DIDMSG
           WHEN    "6666"
               MOVE    "処理中です"    TO  WRK-DIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  DID1
           INITIALIZE                      DID1
           MOVE    SPA-DIDCD           TO  DID1-ID1CODE
           MOVE    WRK-DIDMSG          TO  DID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "DID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-DID1SET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-DID2SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-DIDMSG
      *
           EVALUATE    SPA-DIDCD
           WHEN    "1001"
               MOVE    SPA-NAI-D01-SRYYM
                                       TO  WRK-YMD
               MOVE    "01"            TO  WRK-YMD(7:2)
               PERFORM 800-SEIWA-HEN-SEC
               STRING  WRK-EDTYMD3 (1:16)  DELIMITED   BY  SIZE
                       "分チェック処理を行います"
                                           DELIMITED   BY  SIZE
               INTO    WRK-DIDMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  DID2
           INITIALIZE                      DID2
           MOVE    SPA-DIDCD           TO  DID2-ID1CODE
           MOVE    WRK-DIDMSG          TO  DID2-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "D01"               TO  SPA-MOTOPG
           MOVE    "DID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-DID2SET-EXT.
           EXIT.
      *****************************************************************
      *    年月チェック・編集処理
      *****************************************************************
       5003-NENGETSU-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       5003-NENGETSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年月チェック・編集処理
      *****************************************************************
       800-YMD-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
      *
           MOVE    ZERO                TO  WRK-SYMD
           MOVE    SPACE               TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-YMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-YMD             =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
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
      *****************************************************************
      *    ジョブ管理サブ
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
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    データチェック機能情報２検索処理
      *****************************************************************
       900-SYS1101-SEL-SEC             SECTION.
      *
           INITIALIZE                      SYS-1101-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1101-HOSPNUM
           MOVE    "1101"              TO  SYS-1101-KANRICD
           MOVE    "*"                 TO  SYS-1101-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1101-STYUKYMD
           MOVE    SYS-1101-STYUKYMD   TO  SYS-1101-EDYUKYMD
           MOVE    SYS-1101-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1101-REC
           ELSE
               INITIALIZE                  SYS-1101-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYS1101-SEL-EXT.
           EXIT.
      *****************************************************************
      *    データチェックジョブ制御情報検索処理
      *****************************************************************
       900-SYS3003-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYS3003-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４FETCH処理
      *****************************************************************
       900-CHK004-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    ZERO                TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
