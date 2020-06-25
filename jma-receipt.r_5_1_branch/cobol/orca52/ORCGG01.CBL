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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGG01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 日次統計
      *  管理者            :
      *  01/06/19    MCC-森脇　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/09/06  パラメタ項目追加
      *  01.01.02    MCC-藤原     01/09/21  JOINの修正
      *  01.00.03    MCC-藤原     02.03.18  端末ＩＤ
      *                                     X(16)からX(32)に変更
      *  01.00.04    MCC-藤原     02.03/22  パラメタにシステム日付を追加
      *  01.00.05    MCC-森脇     02/04/23  パラメタ空白時日付セット
      *  01.00.06    MCC-藤原     02/04/15  印刷管理・印刷ＤＢ・
      *                                     ジョブ管理ＤＢの追加
      *  01.00.07    MCC-藤原     02/05/15  処理前にジョブ管理を削除する
      *  01.00.08    MCC-藤原     02/06/04  再印刷・情報削除ボタン追加
      *  01.00.09    NACL-藤原    02/07/25  確定のとき自動的に情報確認画面を表示
      *  01.00.10    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.00.11    NACL-藤原    03/05/12  印刷開始・終了頁の編集の修正
      *  01.00.12    NACL-太田    03/07/02  出力先プリンタサブ追加
      *  01.00.13    NACL-太田    03/07/30  編集パラメータにPTNUM追加
      *  01.00.14    NACL-太田    03/08/05  パラメータを８つまで設定可能
      *                                     に修正
      *  01.00.15    NACL-藤原    03/11/12  統計データ作成画面の追加
      *  01.00.16    NACL-森脇    04/07/29  直前ジョブ管理チェック追加
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *
      *  04.05.01    NACL-藤原    09/10/01  統計ＣＳＶ出力対応
      *
      *  04.08.01    NACL-藤原    14/08/28  ４．８ミドル対応
      *
      *  05.00.00    NACL-森脇    17/11/29  クライアント印刷対応
      *  05.00.01    NACL-藤原    17/12/28  ＨＡＯＲＩ対応
      *  05.00.02    NACL-太田    18/07/11  クラウド対応
      *  05.00.03    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *  05.00.04    NACL-藤原    19/06/05  システム管理（３００３）の
      *                                     キー値を変更
      *                                     エラーファイルの設定を修正
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    日次統計共通パラメタ
           COPY    "G01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "G01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
      *
           03  FLG-CHK                         PIC 9(01).
           03  FLG-ERR                         PIC 9(01).
           03  FLG-TOUKEIMEMO                  PIC 9(01).
           03  FLG-FIXED                       PIC 9(01).
           03  FLG-UPDATE                      PIC 9(01).
           03  FLG-ENABLED                     PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR                     PIC X(02).
      *
       01  IDX-AREA.
           03  IDX                             PIC 9(04).
           03  IDY                             PIC 9(04).
           03  IDZ                             PIC 9(04).
           03  IDZ1                            PIC 9(04).
           03  IDZ2                            PIC 9(04).
           03  IDXA                            PIC 9(04).
           03  IDXB                            PIC 9(04).
           03  IDXC                            PIC 9(04).
           03  IDXI                            PIC 9(04).
           03  IDXJ                            PIC 9(04).
           03  IDXK                            PIC 9(04).
           03  IDX0                            PIC 9(04).
           03  IDX1                            PIC 9(04).
           03  IDX2                            PIC 9(04).
           03  IDXL                            PIC 9(04).
           03  IDXT                            PIC 9(04).
           03  IDXU                            PIC 9(04).
           03  IDX-PARA                        PIC 9(04).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
       01  WRK-AREA.
           03  WRK-ERR-CUR                     PIC 9(03).
           03  WRK-PRTKANRI-RENNUM             PIC 9(04).
           03  WRK-GIDMSG                      PIC X(70).
           03  WRK-YMD                         PIC X(10).
           03  WRK-SYMD.
               05  WRK-SYY                     PIC 9(04).
               05  WRK-SMM                     PIC 9(02).
               05  WRK-SDD                     PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO                     PIC X(01).
               05  WRK-WYY                     PIC 9(02).
               05  WRK-WMM                     PIC 9(02).
               05  WRK-WDD                     PIC 9(02).
           03  WRK-PAGE                        PIC ZZZZZZZZZ9.
      *
           03  WRK-NUM                         PIC 9(03).
      *
           03  WRK-RECEERR                     PIC X(200).
      *
           03  WRK-WIDGET1                     PIC 9(01).
           03  WRK-WIDGET2                     PIC 9(02).
      *
           03  WRK-STPAGE                      PIC 9(05).
           03  WRK-EDPAGE                      PIC 9(05).
           03  WRK-CHK-CUR                     PIC 9(05).
           03  WRK-MCP-WIDGET                  PIC X(64).
           03  WRK-PARA-NUM1                   PIC 9(02).
           03  WRK-PARA-NUM2                   PIC 9(02).
           03  WRK-MAE-INPUT                   PIC X(100).
           03  WRK-ATO-INPUT                   PIC X(100).
           03  WRK-PRT-PAGE                    PIC 9(10).
           03  WRK-PGID                        PIC X(20).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD                   PIC 9(03).
           03  WRK-ERRMSG                      PIC X(200).
           03  WRK-ZERO                        PIC X(10).
           03  WRK-NUM99-G.
               05  WRK-NUM99                   PIC 99.
           03  WRK-NUMZ9                       PIC Z9.
           03  WRK-NUMMSG                      PIC X(04).
           03  WRK-MAX                         PIC 9(03).
           03  WRK-FMTNUM-G.
               05  WRK-FMTNUM-X10              PIC X(10).
               05  WRK-FMTNUM-R1   REDEFINES   WRK-FMTNUM-X10.
                   07  WRK-FMTNUM-Z10          PIC Z(9)9.
               05  WRK-FMTNUM-R2   REDEFINES   WRK-FMTNUM-X10.
                   07  WRK-FMTNUM-910          PIC 9(10).
           03  WRK-MOTOPG                      PIC X(08).
      *
           03  WRK-SHELLTBL-ARG-X.
               05  WRK-SHELLTBL-ARG-9          PIC 9(10).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                      PIC 9(02).
               05  SYS-MM                      PIC 9(02).
               05  SYS-DD                      PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY                   PIC 9(04).
               05  WRK-SYSMM                   PIC 9(02).
               05  WRK-SYSDD                   PIC 9(02).
           03  SYS-TIME                        PIC X(06).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG                     PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO                     PIC X(01).
               05  WRK-HYY                     PIC 9(02).
               05  FILLER                      PIC X(01)   VALUE   ".".
               05  WRK-HMM                     PIC 9(02).
               05  FILLER                      PIC X(01)   VALUE   ".".
               05  WRK-HDD                     PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH                      PIC X(02).
               05  FILLER                      PIC X(01)   VALUE   ".".
               05  WRK-MM                      PIC X(02).
               05  FILLER                      PIC X(01)   VALUE   ".".
               05  WRK-SS                      PIC X(02).
      *     
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPSK0043.INC"              REPLACING    //0043//
                                               BY           //TOUKEI//.
      *
      *
       01  WRK-CONS-AREA.
      *    パラメータ最大個数
           03  WRK-CONS-PARA-MAX               PIC 9(05) VALUE 10.
      *    最大明細数
           03  WRK-CONS-LINE-MAX               PIC 9(05) VALUE 100.
      *    １画面あたりの最大明細数
           03  WRK-CONS-SCR-MAX                PIC 9(05) VALUE 10.
      *    １パラメタのカーソル予約数
           03  WRK-CONS-PARA-CUR               PIC 9(05) VALUE 50.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "052G01".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY   PIC X(08)   VALUE "MONTHLY".
           03  WRK-CONS-PRTKANRI-TBL-GROUP PIC X(14)   VALUE SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID        PIC X(08)   VALUE "MONTHLY".
           03  WRK-CONS-JOB-JOBID          PIC 9(07)   VALUE ZERO.
           03  WRK-CONS-NUMMSG-G.
               05  WRK-CONS-NUMMSG-VAL.
                   07  WRK-CONS-NUMMSG-01      PIC X(04) VALUE "１".
                   07  WRK-CONS-NUMMSG-02      PIC X(04) VALUE "２".
                   07  WRK-CONS-NUMMSG-03      PIC X(04) VALUE "３".
                   07  WRK-CONS-NUMMSG-04      PIC X(04) VALUE "４".
                   07  WRK-CONS-NUMMSG-05      PIC X(04) VALUE "５".
                   07  WRK-CONS-NUMMSG-06      PIC X(04) VALUE "６".
                   07  WRK-CONS-NUMMSG-07      PIC X(04) VALUE "７".
                   07  WRK-CONS-NUMMSG-08      PIC X(04) VALUE "８".
                   07  WRK-CONS-NUMMSG-09      PIC X(04) VALUE "９".
                   07  WRK-CONS-NUMMSG-10      PIC X(04) VALUE "１０".
               05  WRK-CONS-NUMMSG-R  REDEFINES   WRK-CONS-NUMMSG-VAL.
                   07  WRK-CONS-NUMMSG         PIC X(04) OCCURS 10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1031.INC".
           COPY    "CPSK3002.INC".
           COPY    "CPSK3003.INC".
           COPY    "CPSK3102.INC".
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
      *    統計メモ
       01  TOUKEIMEMO-REC.
           COPY  "CPTOUKEIMEMO.INC".
      *
           COPY    "MONBLOB".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
      *
      *    クライアント保存可否情報取得サブ
           COPY    "CPORCSCLIENTSV.INC".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
      *
      *    日レセ選択式プログラム一覧
           COPY    "CPORCSORCAPG.INC".
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
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO   "CLICKED"
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
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-G01FREE     TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
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
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "G01"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
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
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF    ( SPA-MOTOPG      =   "XA01" OR "XB01" OR "XC01"
                                                  OR "XG01"           )
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    SPA-WORK        TO  SPA-G01KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-G01FREE
                   PERFORM 3001-UPDATE-CHK-SEC
                   IF    ( FLG-UPDATE      =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM 310-SPASET-SEC
                   END-IF
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "G05"
               GO  TO  300-SCREEN-EXT
           WHEN    "G99"
               PERFORM 340-G99-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "G07"
               PERFORM 360-G07-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "G03"
               PERFORM 350-G03-SET-SEC
               GO  TO  300-SCREEN-EXT
      *    作表処理
           WHEN    "GID1"
               PERFORM 330-GID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "GID2"
               PERFORM 340-GID2-SET-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "G100"
               PERFORM 310-SPASET-SEC
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
      *    更新チェック処理
      *****************************************************************
       3001-UPDATE-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-UPDATE
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX         >   WRK-CONS-LINE-MAX )
                    OR   ( FLG-UPDATE  =   1 )
      *
               IF    ( SPA-GMN-G01-HCNAME (IDX) NOT =   SPACE )
                   INITIALIZE          SYSKANRI-REC
                   MOVE    "3002"      TO  SYS-KANRICD
                   MOVE    IDX         TO  WRK-KBNCD
                   MOVE    WRK-KBNCD-G TO  SYS-KBNCD
                   MOVE    SPA-SYSYMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
                   MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYSKANRI-REC
                                       TO  SYS-3002-REC
                       IF    ( SYS-3002-HCNAME
                                       =   SPA-GMN-G01-HCNAME (IDX))
                        AND  ( SYS-3002-PRGNAME
                                       =   SPA-GMN-G01-PRGNAME(IDX))
                           CONTINUE
                       ELSE
                           MOVE    1       TO  FLG-UPDATE
                       END-IF
                   ELSE
                           MOVE    1       TO  FLG-UPDATE
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       3001-UPDATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-G01FREE
                                       SPA-G01KYOUTU
      *
           CALL    "ORCSUPDTOUKEIPRM"  USING   SPA-AREA
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               CALL    "ORCSBILLINGCHECK"  USING   SBC-AREA
                                                   SPA-AREA
               IF    ( SBC-BILLINGUSER =   "1" )
                   MOVE    "1"         TO  SPA-G01-CUSTOM-ENABLED
               END-IF
           ELSE
               MOVE    "1"             TO  SPA-G01-CUSTOM-ENABLED
           END-IF
      *
           INITIALIZE                      SORCAPG-AREA
           MOVE    SPA-SYSYMD          TO  SORCAPG-SYSYMD
           MOVE    "MONTHLY"           TO  SORCAPG-TYPE
           CALL    "ORCSORCAPG"        USING
                                       SORCAPG-AREA
                                       SPA-AREA
      *
           MOVE    SORCAPG-PGLST-MAX   TO  SPA-G01-PGLST-MAX
           PERFORM VARYING IDXT    FROM    1   BY  1
                   UNTIL ( IDXT    >   SORCAPG-PGLST-MAX )
      *
               MOVE    SORCAPG-TPGID (IDXT)
                                       TO  SPA-G01-TPGID (IDXT)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >       WRK-CONS-LINE-MAX )
               MOVE    "F"             TO  SPA-GMN-G01-CHK-VALUE (IDX)
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >       WRK-CONS-LINE-MAX )
               MOVE    IDX             TO  WRK-NUM
               MOVE    WRK-NUM         TO  SPA-GMN-G01-NUM (IDX)
           END-PERFORM
      *
      *    パラメータ初期設定
           PERFORM 3101-PARA-SYOKI-SEC
      *
      *    クライアント保存可否情報取得サブ
           INITIALIZE                      ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"      USING   ORCSCLIENTSVAREA
                                               SPA-AREA
           MOVE    SCLIENTSV-CLIENT-SAVE
                                       TO  SPA-NAI-G01-CLIENT-SAVE
      *
           IF    ( SPA-GMN-G01-MAX         =   ZERO )
               MOVE    1                   TO  SPA-GMN-G01-MAXPAGE
           ELSE
               COMPUTE SPA-GMN-G01-MAXPAGE = ( SPA-GMN-G01-MAX - 1 )
                                       / WRK-CONS-SCR-MAX
               COMPUTE SPA-GMN-G01-MAXPAGE =   SPA-GMN-G01-MAXPAGE +   1
           END-IF
      *************
               COMPUTE SPA-GMN-G01-MAXPAGE = ( WRK-CONS-LINE-MAX - 1 )
                                           /   WRK-CONS-SCR-MAX
               COMPUTE SPA-GMN-G01-MAXPAGE =   SPA-GMN-G01-MAXPAGE +   1
      *************
           MOVE    1                   TO  SPA-GMN-G01-PAGE
      *
           MOVE    ZERO                TO  SPA-GMN-G01-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ初期設定処理
      *****************************************************************
       3101-PARA-SYOKI-SEC              SECTION.
      *
      *    システム管理マスタ読込
           INITIALIZE                      SYS-3002-REC
           MOVE  SPA-HOSPNUM           TO  SYS-3002-HOSPNUM
           MOVE  "3002"                TO  SYS-3002-KANRICD
           MOVE  SPA-SYSYMD            TO  SYS-3002-STYUKYMD
                                           SYS-3002-EDYUKYMD
           MOVE  SYS-3002-REC          TO  MCPDATA-REC
           MOVE  "DBSELECT"            TO  MCP-FUNC
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-SYSKANRI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               IF    ( SYS-3002-KBNCD(1:3) IS  NUMERIC )
                   MOVE    SYS-3002-KBNCD(1:3) TO  WRK-NUM
                   IF    ( WRK-NUM         >   WRK-CONS-LINE-MAX )
                    OR   ( WRK-NUM         <   1 )
                       CONTINUE
                   ELSE
                       MOVE    SYS-3002-KBNCD(1:3)
                                           TO  WRK-NUM
                       MOVE    WRK-NUM     TO  IDX
                       MOVE    SYS-3002-HCNAME
                                           TO  SPA-GMN-G01-HCNAME (IDX)
                       MOVE    SYS-3002-PRGNAME
                                           TO  SPA-GMN-G01-PRGNAME(IDX)
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL ( IDY >   WRK-CONS-PARA-MAX )
                           MOVE    SYS-3002-PARAHYOJI(IDY)
                                           TO  SPA-GMN-G01-PARAHYOJI
                                                               (IDX IDY)
                           MOVE    SYS-3002-PARA     (IDY)
                                           TO  SPA-NAI-G01-PARA(IDX IDY)
                           MOVE    SYS-3002-HISSU-KBN(IDY)
                                           TO  SPA-NAI-G01-HISSU-KBN
                                                               (IDX IDY)
                           IF    ( SPA-NAI-G01-HISSU-KBN(IDX IDY)
                                           =   SPACE   )
                               MOVE    "1"
                                           TO  SPA-NAI-G01-HISSU-KBN
                                                               (IDX IDY)
                           END-IF
                       END-PERFORM
      *
                       MOVE    SYS-3002-KBNCD
                                           TO  WRK-KBNCD-G
                       PERFORM 31011-SYS-3102-SEL-SEC
                       IF    ( SPA-MW      =   SPA-GINBEE )
                           MOVE    SYS-3102-REPOS-NAME
                                       TO  SPA-GMN-G01-REPOS-NAME (IDX)
                       END-IF
      *
                       MOVE    IDX     TO  IDXI
                       PERFORM 800-IS-ENABLED-SEC
                       IF    ( FLG-ENABLED   =   1 )
                           MOVE    IDX TO  IDXJ
                           PERFORM 31011-PARA-SYOKI-SEC
                       END-IF
      *
                       IF    ( IDX         >   SPA-GMN-G01-MAX )
                           MOVE    IDX         TO  SPA-GMN-G01-MAX
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-NEXT-SEC
      *
           END-PERFORM
      *
           MOVE  "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE  "tbl_syskanri"            TO  MCP-TABLE
           MOVE  "key2"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       3101-PARA-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ初期設定処理
      *****************************************************************
       31011-PARA-SYOKI-SEC              SECTION.
      *
           MOVE    SPA-GMN-G01-PRGNAME (IDXJ)
                                           TO  WRK-PGID
           MOVE    IDXJ         TO  WRK-KBNCD
           PERFORM 900-TOUKEIMEMO-KEY2-SEL-SEC
           IF    ( FLG-TOUKEIMEMO  =   ZERO )
               MOVE    TKIMEMO-SAVEPARA
                                TO  SPA-NAI-G01-SAVEPARA (IDXJ)
               PERFORM VARYING IDX-PARA   FROM    1   BY  1
                       UNTIL ( IDX-PARA   >   WRK-CONS-PARA-MAX )
      *
                   IF (   SPA-GMN-G01-PARAHYOJI (IDXJ IDX-PARA)
                                       NOT =   SPACE )
                       EVALUATE    IDX-PARA
                       WHEN    9
                           MOVE    TKIMEMO-PARA09
                                       TO  SPA-GMN-G01-PARA
                                                 (IDXJ  IDX-PARA)
                       WHEN    10
                           MOVE    TKIMEMO-PARA10
                                       TO  SPA-GMN-G01-PARA
                                                 (IDXJ  IDX-PARA)
                       WHEN    OTHER
                           MOVE    TKIMEMO-PARA (IDX-PARA)
                                       TO  SPA-GMN-G01-PARA
                                                 (IDXJ  IDX-PARA)
      *
                       END-EVALUATE
                   END-IF
               END-PERFORM
           ELSE
               MOVE    "1"     TO  SPA-NAI-G01-SAVEPARA (IDXJ)
           END-IF
      *
           .
       31011-PARA-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    月次統計付加情報検索処理
      *****************************************************************
       31011-SYS-3102-SEL-SEC          SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "3102"          TO  SYS-KANRICD
           MOVE    WRK-KBNCD-G     TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-3102-REC
      *
           .
       31011-SYS-3102-SEL-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＧＩＤ１）ＯＫ処理
      *****************************************************************
       330-GID1-SET-SEC            SECTION.
      *
           IF    ( SPA-GID1-FLG    =   "OK" )
               EVALUATE    SPA-GIDCD
               WHEN    "6666"
                   MOVE    "JOIN"      TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               WHEN    OTHER
      *----(01.00.16)--UPD-START---
      *           ジョブ管理チェック
                   PERFORM 4900-JOBKANRICHK-SEC
                   IF      SPA-ERRCD       =   SPACE
                       PERFORM 490-PRINT-SEC
                   END-IF
      *----(01.00.16)--UPD-END-----
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  SPA-G99-FLG
           MOVE    SPACE               TO  SPA-GID1-FLG
           MOVE    SPACE               TO  SPA-GIDCD
      *
           .
       330-GID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＧＩＤ２）ＯＫ処理
      *****************************************************************
       340-GID2-SET-SEC            SECTION.
      *
           IF    ( SPA-GID1-FLG    NOT =   SPACE )
      *        ジョブ管理チェック
               PERFORM 4900-JOBKANRICHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 490-PRINT-SEC
               END-IF
           ELSE
               MOVE    SPACE       TO  SPA-GID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-G99-FLG
           MOVE    SPACE           TO  SPA-GIDCD
           .
       340-GID2-SET-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ説明画面より戻り時の処理
      *****************************************************************
       350-G03-SET-SEC              SECTION.
      *
           EVALUATE    SPA-NAI-G03-STATUS
           WHEN    1
      *
               COMPUTE SPA-GMN-G01-CUR
                   =   SPA-G01-IDX
                   *   WRK-CONS-PARA-CUR
                   +   1
      *
               IF    ( SPA-G01-IDX    >=   WRK-CONS-SCR-MAX )
                   MOVE    999     TO  SPA-GMN-G01-CUR
               END-IF
      *
               MOVE    SPA-G01-SEL TO  IDX0
      *
               MOVE    SPA-GMN-G03-CHK-VALUE
                                   TO  SPA-GMN-G01-CHK-VALUE (IDX0)
      *
               MOVE    SPA-GMN-G03-NUM
                                   TO  SPA-GMN-G01-NUM     (IDX0)
               MOVE    SPA-GMN-G03-HCNAME
                                   TO  SPA-GMN-G01-HCNAME  (IDX0)
               MOVE    SPA-GMN-G03-PRGNAME
                                   TO  SPA-GMN-G01-PRGNAME (IDX0)
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   WRK-CONS-PARA-MAX )
      *
                   MOVE    SPA-GMN-G03-PARAHYOJI (IDX1)
                               TO  SPA-GMN-G01-PARAHYOJI (IDX0 IDX1)
      *
                   MOVE    SPA-GMN-G03-PARA (IDX1)
                               TO  SPA-GMN-G01-PARA  (IDX0 IDX1)
      *
                   MOVE    SPA-NAI-G03-PARA (IDX1)
                               TO  SPA-NAI-G01-PARA  (IDX0 IDX1)
      *
                   MOVE    SPA-NAI-G03-HENPARA (IDX1)
                               TO  SPA-NAI-G01-HENPARA (IDX0 IDX1)
      *
                   MOVE    SPA-NAI-G03-HISSU-KBN (IDX1)
                               TO  SPA-NAI-G01-HISSU-KBN  (IDX0 IDX1)
      *
               END-PERFORM
      *
               IF    ( SPA-MW      =   SPA-GINBEE )
                   MOVE    SPA-GMN-G03-REPOS-NAME
                                   TO  SPA-GMN-G01-REPOS-NAME (IDX0)
      *
                   MOVE    IDX0    TO  IDXI
                   PERFORM 800-IS-ENABLED-SEC
                   IF    ( FLG-ENABLED   =   ZERO )
                       MOVE    "F" TO  SPA-GMN-G01-CHK-VALUE (IDX0)
                   END-IF
      *
               END-IF
           WHEN    3
               MOVE    SPA-G01-SEL     TO  IDX
               INITIALIZE                  SPA-GMN-G01-TBL (IDX)
                                           SPA-NAI-G01-TBL (IDX)
               MOVE    "F"             TO  SPA-GMN-G01-CHK-VALUE (IDX)
               MOVE    IDX             TO  WRK-NUM
               MOVE    WRK-NUM         TO  SPA-GMN-G01-NUM (IDX)
               MOVE    SPA-G01-CUR     TO  SPA-GMN-G01-CUR
           WHEN    OTHER
      *
               MOVE    SPA-G01-CUR     TO  SPA-GMN-G01-CUR
      *
           END-EVALUATE
      *
           .
       350-G03-SET-EXT.
           EXIT.
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-G99-SET-SEC             SECTION.
      *
           IF    ( SPA-G99-FLG     =   "OK" )
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-G01-CUR
           MOVE    ZERO            TO  IDX
           MOVE    3               TO  IDY
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
      *    統計帳票一覧画面より戻り時の処理
      *****************************************************************
       360-G07-SET-SEC              SECTION.
      *
           IF    ( SPA-NAI-G07-RET  =   ZERO )
      *
               MOVE    SPA-G01-CUR     TO  SPA-GMN-G01-CUR
      *
           ELSE
      *
               COMPUTE SPA-GMN-G01-CUR
                   =   SPA-G01-CUR +   WRK-CONS-PARA-MAX   +   1
      *
               IF    ( SPA-G01-IDX    >=   WRK-CONS-SCR-MAX )
                   MOVE    99      TO  SPA-GMN-G01-CUR
               END-IF
      *
               PERFORM 3601-G07-SET-SEC
           END-IF
      *
           .
       360-G07-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票一覧画面より戻り時の処理
      *****************************************************************
       3601-G07-SET-SEC              SECTION.
      *
           INITIALIZE              SYS-TOUKEI-REC
      *
           MOVE    SPA-NAI-G07-KANRICD TO  SYS-TOUKEI-KANRICD
           MOVE    SPA-NAI-G07-KBNCD   TO  SYS-TOUKEI-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-TOUKEI-STYUKYMD
                                           SYS-TOUKEI-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-TOUKEI-HOSPNUM
           MOVE    SYS-TOUKEI-REC      TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-TOUKEI-REC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
      *
               MOVE    SPA-G01-SEL     TO  IDX
      *
               INITIALIZE              SPA-GMN-G01-TBL (IDX)
                                       SPA-NAI-G01-TBL (IDX)
               MOVE    "F"         TO  SPA-GMN-G01-CHK-VALUE (IDX)
      *
               MOVE    IDX             TO  WRK-NUM
               MOVE    WRK-NUM         TO  SPA-GMN-G01-NUM (IDX)
               MOVE    SYS-TOUKEI-HCNAME
                                       TO  SPA-GMN-G01-HCNAME (IDX)
               MOVE    SYS-TOUKEI-PRGNAME
                                       TO  SPA-GMN-G01-PRGNAME(IDX)
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL ( IDY >   WRK-CONS-PARA-MAX )
      *
                   MOVE    SYS-TOUKEI-PARAHYOJI(IDY)
                                   TO  SPA-GMN-G01-PARAHYOJI(IDX IDY)
                   MOVE    SYS-TOUKEI-PARA     (IDY)
                                   TO  SPA-NAI-G01-PARA     (IDX IDY)
                   MOVE    SYS-TOUKEI-HISSU-KBN(IDY)
                                   TO  SPA-NAI-G01-HISSU-KBN(IDX IDY)
                   IF    ( SPA-NAI-G01-HISSU-KBN(IDX IDY)
                                   =   SPACE   )
                       MOVE    "1"
                                   TO  SPA-NAI-G01-HISSU-KBN(IDX IDY)
                   END-IF
               END-PERFORM
      *
               IF    ( SPA-NAI-G07-KANRICD =   "3002" )
                   MOVE    IDX         TO  WRK-KBNCD
                   PERFORM 31011-SYS-3102-SEL-SEC
                   IF    ( SPA-MW         =   SPA-GINBEE )
                       MOVE    SYS-3102-REPOS-NAME
                                       TO  SPA-GMN-G01-REPOS-NAME (IDX)
                   END-IF
               END-IF
      *
               MOVE    IDX     TO  IDXI
               PERFORM 800-IS-ENABLED-SEC
               IF    ( FLG-ENABLED   =   1 )
                   MOVE    IDX TO  IDXJ
                   PERFORM 31011-PARA-SYOKI-SEC
               END-IF
      *
           END-IF
      *
           .
       3601-G07-SET-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "G01"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    0                   TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-G01-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE          TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE          TO  LNK-XC01-ED-PAGE
           MOVE    "G01"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-G01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-G01FREE         TO  LNK-SCRSPA
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
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    並び順変更
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 480-G100-SENI-SEC
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 230-CLEAR-SEC
      *    統計データ作成
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 240-TOUKEI-SEC
      *    再印刷
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 450-REPRINT-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 460-MAEGMN-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 470-ATOGMN-SEC
      *    個別指示
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 480-G05-SENI-SEC
      *    ＣＳＶ出力
           WHEN    "CLICKED"       ALSO    "B10"
               PERFORM 490-PUTCSV-SEC
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 490-JYOUHOU-DEL-SEC
      *    OK
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-PRINT-MAE-SEC
      *    個別出力
           WHEN    "CLICKED"       ALSO    "CHK01"
           WHEN    "CLICKED"       ALSO    "CHK02"
           WHEN    "CLICKED"       ALSO    "CHK03"
           WHEN    "CLICKED"       ALSO    "CHK04"
           WHEN    "CLICKED"       ALSO    "CHK05"
           WHEN    "CLICKED"       ALSO    "CHK06"
           WHEN    "CLICKED"       ALSO    "CHK07"
           WHEN    "CLICKED"       ALSO    "CHK08"
           WHEN    "CLICKED"       ALSO    "CHK09"
           WHEN    "CLICKED"       ALSO    "CHK10"
               MOVE    MCP-WIDGET (4:2)    TO  WRK-WIDGET2
               MOVE    WRK-WIDGET2         TO  IDX
               PERFORM 420-CHK-CHK-SEC
           WHEN    "CLICKED"       ALSO    "NUM01"
           WHEN    "CLICKED"       ALSO    "NUM02"
           WHEN    "CLICKED"       ALSO    "NUM03"
           WHEN    "CLICKED"       ALSO    "NUM04"
           WHEN    "CLICKED"       ALSO    "NUM05"
           WHEN    "CLICKED"       ALSO    "NUM06"
           WHEN    "CLICKED"       ALSO    "NUM07"
           WHEN    "CLICKED"       ALSO    "NUM08"
           WHEN    "CLICKED"       ALSO    "NUM09"
           WHEN    "CLICKED"       ALSO    "NUM10"
               MOVE    MCP-WIDGET (4:2)    TO  WRK-WIDGET2
               MOVE    WRK-WIDGET2         TO  IDX
               IF    ( G01-CHK-LABEL (IDX)
                                   NOT =   SPACE  )
                   PERFORM 480-G03-SENI-SEC
               ELSE
                   PERFORM 480-G07-SENI-SEC
               END-IF
      *    処理確認
           WHEN    "CLICKED"       ALSO    "B13"
               PERFORM 490-KAKUNIN-SEC
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
      *
           .
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-G01-CUR
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
      *    入力パラメータ
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   WRK-CONS-SCR-MAX )
      *
               COMPUTE IDXA    =   (   SPA-GMN-G01-PAGE    -   1 )
                               *   WRK-CONS-SCR-MAX
                               +   IDXC
      *
               PERFORM VARYING IDXB    FROM    1   BY  1
                       UNTIL ( IDXB    >   WRK-CONS-PARA-MAX )
                   MOVE    G01-PARA (IDXC IDXB)
                                   TO  SPA-GMN-G01-PARA(IDXA IDXB)
               END-PERFORM
           END-PERFORM
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
           MOVE    ZERO                        TO  WRK-PARA-NUM1
                                                   WRK-PARA-NUM2
           IF    ( WRK-MCP-WIDGET (1:4)    =   "PARA" )
            AND  ( WRK-MCP-WIDGET (5:3)    IS  NUMERIC )
               MOVE    WRK-MCP-WIDGET (5:2)    TO  WRK-PARA-NUM1
               MOVE    WRK-MCP-WIDGET (7:2)    TO  WRK-PARA-NUM2
           END-IF
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   WRK-CONS-SCR-MAX )
      *
               COMPUTE IDXA    =   (   SPA-GMN-G01-PAGE    -   1 )
                               *   WRK-CONS-SCR-MAX
                               +   IDXC
      *
               PERFORM VARYING IDXB    FROM    1   BY  1
                       UNTIL ( IDXB    >   WRK-CONS-PARA-MAX )
                   IF    ( SPA-GMN-G01-PARAHYOJI (IDXA IDXB)
                                   NOT =   SPACE )
      *                入力パラメータ
                       PERFORM 4101-PARA-CHK-SEC
                       IF    ( SPA-ERRCD   NOT =   SPACE )
                           GO  TO  4102-KIHON-CHK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パラメタチェック
      *****************************************************************
       4101-PARA-CHK-SEC                         SECTION.
      *
      *    エラー時のカーソル位置を決定
           COMPUTE WRK-ERR-CUR =  ( IDXC   -   1 )
                               *  WRK-CONS-PARA-CUR
                               +  IDXB
                               +  1
      *
           IF    ( SPA-GMN-G01-PARA (IDXA IDXB)    =   SPACE )
               IF    ( SPA-NAI-G01-HISSU-KBN (IDXA IDXB)
                                               =   "1" )
                   IF    ( WRK-MCP-WIDGET (1:4)    =   "PARA" )
                    AND  ( IDXC            =   WRK-PARA-NUM1 )
                    AND  ( IDXB            =   WRK-PARA-NUM2 )
                       PERFORM 41011-PARA-KIHON-SET-SEC
                       IF    ( SPA-ERRCD       NOT =   SPACE )
                           MOVE    WRK-ERR-CUR     TO  SPA-GMN-G01-CUR
                           GO  TO  4101-PARA-CHK-EXT
                       END-IF
                   END-IF
               ELSE
                   MOVE    SPACE           TO  SPA-NAI-G01-HENPARA
                                                       (IDXA IDXB)
               END-IF
           ELSE
               IF    ( SPA-GMN-G01-CHK-VALUE (IDXA)
                                           =   "T" )
                   PERFORM 41012-PARA-NAIYO-CHK-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       MOVE    WRK-ERR-CUR TO  SPA-GMN-G01-CUR
                       GO  TO  4101-PARA-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-PARA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ基本値設定処理
      *****************************************************************
       41011-PARA-KIHON-SET-SEC        SECTION.
      *
           IF    ( SPA-GMN-G01-CHK-VALUE (IDXA)    =   "F" )
               GO  TO 41011-PARA-KIHON-SET-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-G01-PARA (IDXA IDXB)
           WHEN    "YM"
               MOVE    SPA-SYSYMD  (1:6)
                                   TO  SPA-NAI-G01-HENPARA(IDXA IDXB)
               MOVE    SPA-SYSYMDWH(1:6)
                                   TO  SPA-GMN-G01-PARA   (IDXA IDXB)
           WHEN    "YMD"
               MOVE    SPA-SYSYMD  TO  SPA-NAI-G01-HENPARA(IDXA IDXB)
               MOVE    SPA-SYSYMDWH
                                   TO  SPA-GMN-G01-PARA   (IDXA IDXB)
           WHEN    "N1"
           WHEN    "N2"
           WHEN    "N3"
           WHEN    "N4"
           WHEN    "N5"
           WHEN    "N6"
           WHEN    "N7"
           WHEN    "N8"
           WHEN    "N9"
           WHEN    "N10"
           WHEN    "PSN1"
               MOVE    "0"         TO  SPA-NAI-G01-HENPARA(IDXA IDXB)
               MOVE    "0"
                                   TO  SPA-GMN-G01-PARA   (IDXA IDXB)
           WHEN    OTHER
               MOVE    SPACE       TO  SPA-NAI-G01-HENPARA(IDXA IDXB)
               MOVE    SPACE       TO  SPA-GMN-G01-PARA   (IDXA IDXB)
           END-EVALUATE
      *
           .
       41011-PARA-KIHON-SET-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ入力内容チェック処理
      *****************************************************************
       41012-PARA-NAIYO-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-G01-PARA (IDXA IDXB)
                                   TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-ATO-INPUT   TO  SPA-GMN-G01-PARA (IDXA IDXB)
      *
           EVALUATE    SPA-NAI-G01-PARA (IDXA IDXB)
           WHEN    "YM"
               MOVE    SPA-GMN-G01-PARA (IDXA IDXB)    TO  WRK-YMD
               PERFORM 5003-NENGETSU-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    WRK-SYMD(1:6)
                                   TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
                   MOVE    WRK-HENYMDG(1:6)
                                   TO  SPA-GMN-G01-PARA    (IDXA IDXB)
               END-IF
           WHEN    "KKNYM"
               MOVE    SPA-GMN-G01-PARA (IDXA IDXB)    TO  WRK-YMD
               PERFORM 5003-NENGETSU-KKN-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    WRK-SYMD(1:6)
                                   TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
                   MOVE    WRK-HENYMDG(1:6)
                                   TO  SPA-GMN-G01-PARA    (IDXA IDXB)
               END-IF
           WHEN    "YMD"
               MOVE    SPA-GMN-G01-PARA (IDXA IDXB)    TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    WRK-SYMD
                                   TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
                   MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-G01-PARA    (IDXA IDXB)
               END-IF
           WHEN    "PSN1"
                   IF    ( SPA-GMN-G01-PARA (IDXA IDXB)(2:)
                                               =   SPACE )
                       MOVE    SPA-GMN-G01-PARA(IDXA IDXB)(1:1)
                           TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
                   ELSE
                       MOVE    "0002"  TO  SPA-ERRCD
                   END-IF
           WHEN    "N1"
           WHEN    "N2"
           WHEN    "N3"
           WHEN    "N4"
           WHEN    "N5"
           WHEN    "N6"
           WHEN    "N7"
           WHEN    "N8"
           WHEN    "N9"
           WHEN    "N10"
               MOVE    ZERO            TO  FLG-FIXED
               PERFORM 410121-PARA-NUM-CHK-SEC
           WHEN    "N01"
           WHEN    "N02"
           WHEN    "N03"
           WHEN    "N04"
           WHEN    "N05"
           WHEN    "N06"
           WHEN    "N07"
           WHEN    "N08"
           WHEN    "N09"
           WHEN    "N010"
               MOVE    1               TO  FLG-FIXED
               PERFORM 410121-PARA-NUM-CHK-SEC
           WHEN    "A"
           WHEN    "A1"
           WHEN    "A2"
           WHEN    "A3"
           WHEN    "A4"
           WHEN    "A5"
           WHEN    "A6"
           WHEN    "A7"
           WHEN    "A8"
           WHEN    "A9"
           WHEN    "A10"
               MOVE    SPA-GMN-G01-PARA (IDXA IDXB)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-HANKAKU-CHK-SEC
               MOVE    WRK-ATO-INPUT   TO  SPA-GMN-G01-PARA (IDXA IDXB)
               IF    ( FLG-ERR     NOT =   ZERO )
                   MOVE    "0002"  TO  SPA-ERRCD
               END-IF
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( SPA-NAI-G01-PARA (IDXA IDXB)
                               NOT =   "A" )
                   INITIALIZE                  ORCSNUMAREA
                   MOVE    SPA-NAI-G01-PARA (IDXA IDXB)(2:)
                                           TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   MOVE    SNUM-OUTNUM     TO  WRK-MAX
                   IF    ( SPA-GMN-G01-PARA (IDXA IDXB)(WRK-MAX + 1 :)
                                        NOT =   SPACE )
                       MOVE    "0103"      TO  SPA-ERRCD
                       MOVE   WRK-CONS-NUMMSG (WRK-MAX)
                                           TO  WRK-NUMMSG
                   END-IF
               END-IF
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    SPA-GMN-G01-PARA (IDXA IDXB)
                                   TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
               END-IF
           WHEN    "PTNUM"
               IF    ( SPA-GMN-G01-PARA (IDXA IDXB)    =   "*" or "＊" )
                   MOVE    "*"     TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
                                       SPA-GMN-G01-PARA    (IDXA IDXB)
               ELSE
                   INITIALIZE                  ORCSPTNUMAREA
                   MOVE    SPA-GMN-G01-PARA (IDXA IDXB)
                                   TO  SPTNUM-PTNUM
                   CALL    "ORCSPTNUM" USING   ORCSPTNUMAREA
                                                       SPA-AREA
                   IF    ( SPTNUM-RC       NOT =   20 )
                       MOVE    SPTNUM-PTNUM
                                   TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
                                       SPA-GMN-G01-PARA    (IDXA IDXB)
                   ELSE
                       MOVE    "0002"      TO  SPA-ERRCD
                   END-IF
               END-IF
           WHEN    OTHER
               MOVE    SPA-GMN-G01-PARA (IDXA IDXB)
                                   TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
              MOVE    SPACE                TO  WRK-ERRMSG
              STRING  SPA-GMN-G01-NUM    (IDXA)    DELIMITED BY SIZE
                      ":"                          DELIMITED BY SIZE
                      SPA-GMN-G01-HCNAME (IDXA)    DELIMITED BY SPACE
                      "　"                         DELIMITED BY SIZE
                      SPA-GMN-G01-PARAHYOJI (IDXA IDXB)
                                                   DELIMITED BY SPACE
              INTO    WRK-ERRMSG
              END-STRING
      *       ＨＡＯＲＩのとき位置情報設定
              IF      SPA-G01-JOB-PARAM
                                       NOT =   SPACE
                  MOVE    IDXA             TO  SPA-G01-PARAHYOJI-1
                  MOVE    IDXB             TO  SPA-G01-PARAHYOJI-2
              END-IF
           END-IF
      *
           .
       41012-PARA-NAIYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ数値項目入力内容チェック処理
      *****************************************************************
       410121-PARA-NUM-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE   SPA-NAI-G01-PARA (IDXA IDXB)(2:)
                                   TO  SNUM-INX
           CALL   "ORCSNUM"            USING
                                       ORCSNUMAREA
           MOVE   SNUM-OUTNUM      TO  WRK-MAX
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE   SPA-GMN-G01-PARA (IDXA IDXB)
                                   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           EVALUATE    TRUE
           WHEN  ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-SYOKBN         =   "1"   )
            OR   ( SNUM-MINKBN         =   "1"   )
               MOVE    "0002"      TO  SPA-ERRCD
               MOVE   WRK-CONS-NUMMSG (WRK-MAX)
                                   TO  WRK-NUMMSG
           WHEN  ( SNUM-SEISUU         >   WRK-MAX )
           WHEN  ( SPA-GMN-G01-PARA (IDXA IDXB)(WRK-MAX + 1 :)
                               NOT =   SPACE )
               MOVE    "0103"      TO  SPA-ERRCD
               MOVE   WRK-CONS-NUMMSG (WRK-MAX)
                                   TO  WRK-NUMMSG
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( FLG-FIXED       =   ZERO )
                   MOVE    SPA-GMN-G01-PARA (IDXA IDXB)
                                   TO  WRK-FMTNUM-X10
                   PERFORM VARYING IDXK    FROM    1   BY  1
                           UNTIL ( IDXK    >   10 )
                       IF    ( WRK-FMTNUM-X10(IDXK :1)
                                       NOT =   SPACE )
                           MOVE    WRK-FMTNUM-X10 (IDXK :)
                               TO   SPA-NAI-G01-HENPARA (IDXA IDXB)
                                    SPA-GMN-G01-PARA    (IDXA IDXB)
                           MOVE    10      TO  IDXK
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    SNUM-OUTNUM TO   WRK-FMTNUM-910
                   MOVE    WRK-FMTNUM-X10(11 - WRK-MAX : WRK-MAX)
                               TO   SPA-NAI-G01-HENPARA (IDXA IDXB)
                                     SPA-GMN-G01-PARA    (IDXA IDXB)
               END-IF
           END-IF
      *
           .
       410121-PARA-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック
      *****************************************************************
       4900-JOBKANRICHK-SEC            SECTION.
      *
      *    ジョブ管理チェック
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN
                                           =   ZERO )
                       IF    ( JOB-ENDYMD  =   SPACE )
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-RECEERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-RECEERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
      *
       4900-JOBKANRICHK-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *    未入力チェック
           PERFORM 49001-MINYURYOKU-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    いずれかの処理にチェックがはいっていること
           IF    ( FLG-CHK             =   ZERO )
               MOVE    "0005"          TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    未入力チェック
      *****************************************************************
       49001-MINYURYOKU-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   WRK-CONS-LINE-MAX )
      *
               IF    ( SPA-GMN-G01-CHK-VALUE (IDXA)    =   "T" )
                   MOVE    1       TO  FLG-CHK
                   PERFORM VARYING IDXB    FROM    1   BY  1
                           UNTIL ( IDXB    >   WRK-CONS-PARA-MAX )
      *
                       IF    ( SPA-GMN-G01-PARAHYOJI(IDXA IDXB)
                                           NOT =   SPACE )
                        AND  ( SPA-GMN-G01-PARA (IDXA IDXB)
                                               =   SPACE )
                        AND  ( SPA-NAI-G01-HISSU-KBN (IDXA IDXB)
                                               =   "1" )
                           MOVE    "0004"  TO  SPA-ERRCD
                           MOVE    SPACE   TO  WRK-ERRMSG
                           STRING  SPA-GMN-G01-NUM    (IDXA)
                                                   DELIMITED BY SIZE
                                   ":"             DELIMITED BY SIZE
                                   SPA-GMN-G01-HCNAME (IDXA)
                                                   DELIMITED BY SPACE
                                   "　"            DELIMITED BY SIZE
                                   SPA-GMN-G01-PARAHYOJI (IDXA IDXB)
                                                   DELIMITED BY SPACE
                           INTO    WRK-ERRMSG
                           END-STRING
      *                    ＨＡＯＲＩのとき位置情報設定
                           IF      SPA-G01-JOB-PARAM
                                           NOT =   SPACE
                               MOVE    IDXA    TO  SPA-G01-PARAHYOJI-1
                               MOVE    IDXB    TO  SPA-G01-PARAHYOJI-2
                           END-IF
                           GO  TO  49001-MINYURYOKU-CHK-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
      *
       49001-MINYURYOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       IF    ( JOB-ENDYMD      =   SPACE )
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"          TO  SPA-GIDCD
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
      *    クリア　処理
      *****************************************************************
       230-CLEAR-SEC                   SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   WRK-CONS-LINE-MAX )
      *
               MOVE    "F"         TO  SPA-GMN-G01-CHK-VALUE (IDXA)
      *
               PERFORM VARYING IDXB    FROM    1   BY  1
                       UNTIL ( IDXB    >   WRK-CONS-PARA-MAX )
                   MOVE    SPACE   TO  SPA-NAI-G01-HENPARA (IDXA IDXB)
                                       SPA-GMN-G01-PARA    (IDXA IDXB)
               END-PERFORM
           END-PERFORM
      *
           .
       230-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計データ作成画面へ
      *****************************************************************
       240-TOUKEI-SEC                  SECTION.
      * 
           MOVE    "G01"               TO  SPA-MOTOPG
           MOVE    "G02"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       240-TOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "G01"               TO  SPA-MOTOPG
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
      *    個別出力処理
      *****************************************************************
       420-CHK-CHK-SEC                 SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           COMPUTE IDZ             =   IDX
                                   + ( SPA-GMN-G01-PAGE    -   1 )
                                   * WRK-CONS-SCR-MAX
      *
           MOVE    G01-CHK-VALUE (IDX) TO  SPA-GMN-G01-CHK-VALUE (IDZ)
      *
           MOVE    IDZ             TO  IDXI
           PERFORM 800-IS-ENABLED-SEC
           IF    ( FLG-ENABLED   =   ZERO )
               MOVE    "F"         TO  SPA-GMN-G01-CHK-VALUE (IDZ)
           END-IF
      *
           COMPUTE SPA-GMN-G01-CUR   = ( IDX -   1 )
                                 *     WRK-CONS-PARA-CUR
                                 +     1
                                 +     1
      *
           MOVE    IDZ                 TO  IDXA
           PERFORM VARYING IDXB    FROM    1   BY  1
                       UNTIL ( IDXB    >   WRK-CONS-PARA-MAX )
               IF    ( SPA-GMN-G01-PARAHYOJI (IDXA IDXB)
                                   NOT =   SPACE )
                AND  ( SPA-GMN-G01-PARA (IDXA IDXB)
                                       =   SPACE )
                AND  ( SPA-NAI-G01-HISSU-KBN (IDXA IDXB)  =   "1" )
      *            パラメータ基本値設定
                   PERFORM 41011-PARA-KIHON-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       420-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           IF    ( SPA-GMN-G01-PAGE   =   1 )
               MOVE    "0006"          TO  SPA-ERRCD
               GO  TO  460-MAEGMN-EXT
           END-IF
           COMPUTE SPA-GMN-G01-PAGE    =   SPA-GMN-G01-PAGE    -   1
      *
           MOVE    1                   TO  SPA-GMN-G01-CUR
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC                    SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF      SPA-GMN-G01-PAGE       =   SPA-GMN-G01-MAXPAGE
               MOVE    "0007"             TO  SPA-ERRCD
               GO  TO  470-ATOGMN-EXT
           END-IF
           COMPUTE SPA-GMN-G01-PAGE   =    SPA-GMN-G01-PAGE    +   1
      *
           MOVE    1                   TO  SPA-GMN-G01-CUR
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷前確認処理
      *****************************************************************
       490-PRINT-MAE-SEC                   SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
      *    ジョブ管理チェック
           PERFORM 4900-JOBKANRICHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
      *    関連チェック
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
           MOVE    "1001"              TO  SPA-GIDCD
      *
           .
       490-PRINT-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       490-PRINT-SEC                       SECTION.
      *
           PERFORM 4901-TOUKEIMEMO-UPD-SEC
      *
           INITIALIZE                      SPA-G01-TBL-GROUP
                                           SPA-G01-JOB-JOBID
      *
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  490-PRINT-EXT
               END-IF
           END-IF
      *
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  490-PRINT-EXT
           END-IF
      *
      *    monblobapi処理
           IF      SPA-G01-JOB-PARAM
                               NOT =   SPACE
      *        Ｄａｔａ＿Ｉｄ取得
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  490-PRINT-EXT
               END-IF
               MOVE    MONBLOB-ID          TO  SPA-G01-MONBLOB-PRINT-ID
           END-IF
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "OR052ERR"          TO  ERRFLPARA-FILE-ID
           MOVE    1                   TO  ERRFLPARA-SYOKBN1
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
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
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
      *    MOVE    SPACE               TO  SYS-1031-REC
      *    INITIALIZE                  SYS-1031-REC
      *    MOVE    SPA-HOSPNUM         TO  SYS-1031-HOSPNUM
      *    MOVE    "1031"              TO  SYS-1031-KANRICD
      *    MOVE    "*"                 TO  SYS-1031-KBNCD
      *    MOVE    SYS-1031-REC        TO  MCPDATA-REC
      *    PERFORM 900-SYSKANRI-INV-SEC
      *    IF      FLG-SYSKANRI        =   ZERO
      *        MOVE    MCPDATA-REC         TO  SYS-1031-REC
      *    ELSE
      *        INITIALIZE                  SYS-1031-REC
      *    END-IF
      *
           MOVE    WRK-SYSYMD          TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           PERFORM VARYING IDY FROM    1       BY  1
                   UNTIL ( IDY             >   WRK-CONS-LINE-MAX )
                   OR    ( SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-G01-CHK-VALUE   (IDY)   =   "T"
      *
                   PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *            印刷用スクリプト処理
      *
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
                   MOVE    "execoprg.sh"   TO  LNK-PRTKANRI-SHELLID
                   MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
                   MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
                   MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
                   MOVE    SYS-1031-PRTNM (6)
                                           TO  LNK-PRTKANRI-PRTNM
      *
                   MOVE    SPACE           TO  SHELLTBL
                   MOVE    "execoprg.sh"   TO  SHELLTBL-NAME
      *
                   MOVE     SPA-HOSPNUM    TO   SHELLTBL-HOSPNUM
                   MOVE     WRK-CONS-JOB-JOBID
                                           TO   SHELLTBL-JOBID
                   MOVE     WRK-CONS-JOB-SHELLID
                                           TO   SHELLTBL-SHELLID
                   IF    ( SPA-MW          =   SPA-GINBEE )
                    AND  ( SPA-G01-CUSTOM-ENABLED
                                           =   "1" )
                       MOVE    SPA-GMN-G01-REPOS-NAME (IDY)
                                           TO  SHELLTBL-REPOS-NAME
                   END-IF
      *
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
                   MOVE    ERRFLPARA-BASENAME
                                           TO  SHELLTBL-ARG15
                   MOVE    SPA-GMN-G01-PRGNAME (IDY)
                                           TO  SHELLTBL-ARG16
                   MOVE    SPA-NAI-G01-HENPARA (IDY 1)
                                           TO  SHELLTBL-ARG17
                   MOVE    SPA-NAI-G01-HENPARA (IDY 2)
                                           TO  SHELLTBL-ARG18
                   MOVE    SPA-NAI-G01-HENPARA (IDY 3)
                                           TO  SHELLTBL-ARG19
                   MOVE    SPA-NAI-G01-HENPARA (IDY 4)
                                           TO  SHELLTBL-ARG20
                   MOVE    SPA-NAI-G01-HENPARA (IDY 5)
                                           TO  SHELLTBL-ARG21
                   MOVE    SPA-NAI-G01-HENPARA (IDY 6)
                                           TO  SHELLTBL-ARG22
                   MOVE    SPA-NAI-G01-HENPARA (IDY 7)
                                           TO  SHELLTBL-ARG23
                   MOVE    SPA-NAI-G01-HENPARA (IDY 8)
                                           TO  SHELLTBL-ARG24
                   MOVE    SPA-NAI-G01-HENPARA (IDY 9)
                                           TO  SHELLTBL-ARG25
                   MOVE    SPA-NAI-G01-HENPARA (IDY 10)
                                           TO  SHELLTBL-ARG26
                   MOVE    SHELLTBL        TO  MCPDATA-REC
                   MOVE    "SHELL"         TO  MCP-FUNC
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    "daily"         TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               END-IF
           END-PERFORM
      *
      *    印刷処理
           IF     ( SPA-GID1-FLG        =   "OK"  OR "PV")
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  SPA-G01-TBL-GROUP
               MOVE    WRK-CONS-JOB-JOBID
                                           TO  SPA-G01-JOB-JOBID
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *
               MOVE    SPACE               TO  SHELLTBL
               MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      *
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID
                                           TO  SHELLTBL-JOBID
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-SHELLID
      *
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    "000000"            TO  LNK-PRTKANRI-SRYYM
               MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"          TO  LNK-PRTKANRI-SHELLID
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
               MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
               MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-GID1-FLG
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
               IF      SPA-G01-JOB-PARAM
                                       NOT =   SPACE
                   MOVE    "7"                 TO  SHELLTBL-ARG17
               END-IF
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *----(05.00.00)--ADD-END-----
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
      *    monblobapi処理
           IF      SPA-G01-JOB-PARAM
                               NOT =   SPACE
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 45013-MONBLOBAPI-SEC
               END-IF
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 45012-JOBID-UPD-SEC
      *
           PERFORM 490-KAKUNIN-SEC
      *
      *    PERFORM VARYING IDZ FROM    1   BY  1
      *            UNTIL ( IDZ >       WRK-CONS-LINE-MAX )
      *        MOVE    "F"             TO  SPA-GMN-G01-CHK-VALUE(IDZ)
      *        INITIALIZE              SPA-GMN-G01-PARA-TBL     (IDZ)
      *                                SPA-NAI-G01-HENPARA-TBL  (IDZ)
      *    END-PERFORM
      *
           .
       490-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計メモ更新処理
      *****************************************************************
       4901-TOUKEIMEMO-UPD-SEC         SECTION.
      *
           PERFORM VARYING IDY FROM    1       BY  1
                   UNTIL ( IDY             >   WRK-CONS-LINE-MAX )
                   OR    ( SPA-ERRCD   NOT =   SPACE )
               IF    ( SPA-GMN-G01-CHK-VALUE   (IDY)   =   "T" )
      *
                   IF    ( SPA-NAI-G01-SAVEPARA (IDY)    =   "1" )
                       MOVE    SPA-GMN-G01-PRGNAME (IDY)
                                           TO  WRK-PGID
                       MOVE    IDY         TO  WRK-KBNCD
                       PERFORM 900-TOUKEIMEMO-KEY2-SEL-SEC
                       IF    ( FLG-TOUKEIMEMO  =   ZERO )
                           PERFORM VARYING IDX-PARA    FROM    1   BY  1
                                   UNTIL ( IDX-PARA
                                               >   WRK-CONS-PARA-MAX )
                                   OR    ( SPA-ERRCD   NOT =   SPACE )
      *
                               IF    ( SPA-NAI-G01-PARA (IDY IDX-PARA)
                                           =   "YM" OR "KKNYM" OR "YMD")
                                   CONTINUE
                               ELSE
                                   EVALUATE    IDX-PARA
                                   WHEN    9
                                       MOVE    SPA-GMN-G01-PARA
                                                          (IDY IDX-PARA)
                                               TO  TKIMEMO-PARA09
                                   WHEN    10
                                       MOVE    SPA-GMN-G01-PARA
                                                          (IDY IDX-PARA)
                                               TO  TKIMEMO-PARA10
                                   WHEN    OTHER
                                       MOVE    SPA-GMN-G01-PARA
                                                          (IDY IDX-PARA)
                                           TO  TKIMEMO-PARA (IDX-PARA)
                                   END-EVALUATE
                               END-IF
      *
                           END-PERFORM
      *
                           MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
                           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
                           MOVE    "key2"              TO  MCP-PATHNAME
                           PERFORM 910-DBUPDATE-SEC
                           IF    ( MCP-RC          NOT =   ZERO )
                               MOVE    "4001"          TO  SPA-ERRCD
                           END-IF
      *
                       END-IF
                   END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       4901-TOUKEIMEMO-UPD-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-G01-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-G01-JOB-ORCA-UID
                                   TO  JOB-ORCA-UID
           PERFORM 800-ORCSJOB-SEC
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
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
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
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONBLOBAPI処理
      *****************************************************************
       45013-MONBLOBAPI-SEC        SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "haori_monblobapi.sh"
                                       TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                       TO  SHELLTBL-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-SHELLID
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    SPA-G01-TBL-GROUP   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "000000"            TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
           MOVE    "haori_monblobapi.sh"
                                       TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
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
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG15
      *
           MOVE    SPA-G01-MONBLOB-PRINT-ID
                                       TO  SHELLTBL-ARG16
           MOVE    SPA-G01-PUSHBA01-EVENT
                                       TO  SHELLTBL-ARG17
           MOVE    SPA-G01-JOB-JOBID   TO  WRK-SHELLTBL-ARG-9
           MOVE    WRK-SHELLTBL-ARG-X  TO  SHELLTBL-ARG18
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       45013-MONBLOBAPI-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-G01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-G01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "G01"               TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-G01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-G01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "G01"               TO  SPA-MOTOPG
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
      *****************************************************************
      *    個別指示画面へ
      *****************************************************************
       480-G05-SENI-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    画面移行用のパラメタ変更
           MOVE    "G01"               TO  SPA-MOTOPG
           MOVE    "G05"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "G05   "             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-G05-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ説明画面遷移処理
      *****************************************************************
       480-G03-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           COMPUTE SPA-G01-SEL     =   IDX
                                   + ( SPA-GMN-G01-PAGE    -   1 )
                                   * WRK-CONS-SCR-MAX
      *
           MOVE    IDX             TO  SPA-G01-IDX
      *
           COMPUTE SPA-G01-CUR     = ( IDX    -   1 )
                                   *   WRK-CONS-PARA-CUR
                                   +   1
      *
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-G01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGG03"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
      *
           MOVE    "G01"               TO  SPA-MOTOPG
           MOVE    "G03"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "G03"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-G03-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計帳票一覧画面画面遷移処理
      *****************************************************************
       480-G07-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 800-LDUSER-CHK-SEC
           IF    ( SLDUSER-OTHERS  =   ZERO )
               PERFORM 4801-G07-SENI-SEC
           ELSE
               MOVE    "0104"      TO  SPA-ERRCD
           END-IF
      *
           .
       480-G07-SENI-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票一覧画面画面遷移処理
      *****************************************************************
       4801-G07-SENI-SEC                SECTION.
      *
           MOVE    "G01"           TO  SPA-G01-MOTOPG
      *
           COMPUTE SPA-G01-SEL     =   IDX
                                   + ( SPA-GMN-G01-PAGE    -   1 )
                                   * WRK-CONS-SCR-MAX
      *
           MOVE    IDX             TO  SPA-G01-IDX
      *
           COMPUTE SPA-G01-CUR     = ( IDX    -   1 )
                                   * ( WRK-CONS-PARA-MAX + 1 )
                                   +   1
      *
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-G01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGG07"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
      *
           MOVE    "G01"               TO  SPA-MOTOPG
           MOVE    "G07"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "G07"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4801-G07-SENI-EXT.
           EXIT.
      *****************************************************************
      *    並び順変更画面遷移処理
      *****************************************************************
       480-G100-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 800-LDUSER-CHK-SEC
           IF    ( SLDUSER-OTHERS  =   ZERO )
               PERFORM 4801-G100-SENI-SEC
           ELSE
               MOVE    "0104"      TO  SPA-ERRCD
           END-IF
      *
           .
       480-G100-SENI-EXT.
           EXIT.
      *****************************************************************
      *    並び順変更画面遷移処理
      *****************************************************************
       4801-G100-SENI-SEC              SECTION.
      *
           MOVE    "G01"           TO  SPA-MOTOPG
      *
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-G01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGG100"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
      *
           MOVE    "G01"           TO  SPA-MOTOPG
           MOVE    "G100"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "G100"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       4801-G100-SENI-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力画面へ
      *****************************************************************
       490-PUTCSV-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-G01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-G01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "G01"               TO  SPA-MOTOPG
           MOVE    "XG01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XG01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-PUTCSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE                      G99
           MOVE    ZERO                TO  G99-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  G99-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           MOVE    5                   TO  G99-DURATION
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
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
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  G99-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  G99-ERRMSG   (IDX)
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  G99-MSG
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE )
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE
                                                   INTO  G99-MSG
                               END-STRING
                           END-IF
                       END-IF
                       MOVE    IDX             TO  G99-COUNT
      *
                       IF    ( JOB-JOBID   =   SPA-G01-JOB-JOBID )
                           MOVE    JOB-UPDCNT  TO  WRK-PRT-PAGE
                       END-IF
      *
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( G99-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( G99-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                       TO  G99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                       TO  G99-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-GID1-FLG    =   "PV" )
               MOVE    "F12:プレビュー"    TO  G99-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                   MOVE    WIDGET-NORMAL   TO  G99-B12-STATE
                   MOVE    "B12"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "G01"                TO  SPA-MOTOPG
                                            SPA-G99-MOTOPG
           MOVE    "G99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "G99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
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
           IF    ( SPA-GIDCD       NOT =   SPACE )
               EVALUATE    SPA-GIDCD
               WHEN    "1001"
                   PERFORM 540-GID2SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-GIDSET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "G01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    "red"               TO  G01-MARKHISSU-STYLE
           MOVE    "*"                 TO  G01-MARKHISSU
      *
           MOVE    "blue"              TO  G01-MARKNINI-STYLE
           MOVE    "*"                 TO  G01-MARKNINI
      *
           COMPUTE IDY     =   SPA-GMN-G01-PAGE    *   WRK-CONS-SCR-MAX
           COMPUTE IDZ     =   IDY         - ( WRK-CONS-SCR-MAX - 1 )
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDZ1     FROM    IDZ   BY  1
                   UNTIL     ( IDZ1     >       IDY )
               ADD     1                        TO  IDX
      *
               MOVE    SPA-GMN-G01-NUM      (IDZ1)
                                       TO  G01-NUM   (IDX)
               MOVE    SPA-GMN-G01-CHK-VALUE(IDZ1)
                                       TO  G01-CHK-VALUE (IDX)
               MOVE    SPA-GMN-G01-HCNAME   (IDZ1)
                                       TO  G01-CHK-LABEL (IDX)
      *
               MOVE    IDZ1        TO  IDXI
               PERFORM 800-IS-ENABLED-SEC
      *
               PERFORM VARYING IDZ2 FROM    1   BY  1
                       UNTIL ( IDZ2 >       WRK-CONS-PARA-MAX )
      *
                   MOVE    SPA-GMN-G01-PARAHYOJI (IDZ1 IDZ2)
                                       TO  G01-PARAHYOJI (IDX IDZ2)
                   MOVE    SPA-GMN-G01-PARA      (IDZ1 IDZ2)
                                       TO  G01-PARA      (IDX IDZ2)
      *
                   MOVE    WIDGET-INSENSITIVE
                                       TO  G01-PARA-STATE(IDX IDZ2)
      *
                   MOVE    SPACE       TO  G01-PARAMARK-STYLE(IDX IDZ2)
                   MOVE    SPACE       TO  G01-PARAMARK      (IDX IDZ2)
                   IF    ( SPA-GMN-G01-PARAHYOJI (IDZ1 IDZ2)
                                           NOT =   SPACE )
                       IF    ( FLG-ENABLED   =   1 )
                           MOVE    WIDGET-NORMAL
                                        TO  G01-PARA-STATE(IDX IDZ2)
                       END-IF
                       IF  ( SPA-NAI-G01-HISSU-KBN(IDZ1 IDZ2)
                                               =   "1" )
                           MOVE    "red"
                                       TO  G01-PARAMARK-STYLE(IDX IDZ2)
                           MOVE    "*" TO  G01-PARAMARK      (IDX IDZ2)
                       ELSE
                           MOVE    "blue"
                                       TO  G01-PARAMARK-STYLE(IDX IDZ2)
                           MOVE    "*" TO  G01-PARAMARK      (IDX IDZ2)
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    WIDGET-NORMAL   TO  G01-NUM-STATE(IDX)
      *
               MOVE    WIDGET-INSENSITIVE
                                       TO  G01-CHK-STATE(IDX)
      *
               IF    ( SPA-GMN-G01-HCNAME   (IDZ1)  NOT =   SPACE )
                   IF    ( FLG-ENABLED   =   ZERO )
                       MOVE    WIDGET-INSENSITIVE
                                       TO  G01-CHK-STATE(IDX)
                       MOVE    "F"     TO  G01-CHK-VALUE(IDX)
                   ELSE
                       MOVE    WIDGET-NORMAL
                                       TO  G01-CHK-STATE(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-G01-PAGE        =   1 )
               MOVE    WIDGET-INSENSITIVE  TO  G01-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  G01-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-G01-PAGE        =   SPA-GMN-G01-MAXPAGE )
               MOVE    WIDGET-INSENSITIVE  TO  G01-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  G01-B07-STATE
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  G01-B04-STATE
                                           G01-B10-STATE
           IF    ( SPA-MW              =   SPA-GINBEE )
           AND   ( SPA-NAI-G01-CLIENT-SAVE
                                       =   "0"        )
               MOVE    WIDGET-INSENSITIVE  TO  G01-B04-STATE
                                               G01-B10-STATE
           END-IF
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-G01-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-G01-CUR
           WHEN    001
               MOVE    "CHK01"              TO  MCP-WIDGET
           WHEN    002
               MOVE    "PARA0101"           TO  MCP-WIDGET
           WHEN    003
               MOVE    "PARA0102"           TO  MCP-WIDGET
           WHEN    004
               MOVE    "PARA0103"           TO  MCP-WIDGET
           WHEN    005
               MOVE    "PARA0104"           TO  MCP-WIDGET
           WHEN    006
               MOVE    "PARA0105"           TO  MCP-WIDGET
           WHEN    007
               MOVE    "PARA0106"           TO  MCP-WIDGET
           WHEN    008
               MOVE    "PARA0107"           TO  MCP-WIDGET
           WHEN    009
               MOVE    "PARA0108"           TO  MCP-WIDGET
           WHEN    010
               MOVE    "PARA0109"           TO  MCP-WIDGET
           WHEN    011
               MOVE    "PARA0110"           TO  MCP-WIDGET
           WHEN    051
               MOVE    "CHK02"             TO  MCP-WIDGET
           WHEN    052
               MOVE    "PARA0201"           TO  MCP-WIDGET
           WHEN    053
               MOVE    "PARA0202"           TO  MCP-WIDGET
           WHEN    054
               MOVE    "PARA0203"           TO  MCP-WIDGET
           WHEN    055
               MOVE    "PARA0204"           TO  MCP-WIDGET
           WHEN    056
               MOVE    "PARA0205"           TO  MCP-WIDGET
           WHEN    057
               MOVE    "PARA0206"           TO  MCP-WIDGET
           WHEN    058
               MOVE    "PARA0207"           TO  MCP-WIDGET
           WHEN    059
               MOVE    "PARA0208"           TO  MCP-WIDGET
           WHEN    060
               MOVE    "PARA0209"           TO  MCP-WIDGET
           WHEN    061
               MOVE    "PARA0210"           TO  MCP-WIDGET
           WHEN    101
               MOVE    "CHK03"             TO  MCP-WIDGET
           WHEN    102
               MOVE    "PARA0301"           TO  MCP-WIDGET
           WHEN    103
               MOVE    "PARA0302"           TO  MCP-WIDGET
           WHEN    104
               MOVE    "PARA0303"           TO  MCP-WIDGET
           WHEN    105
               MOVE    "PARA0304"           TO  MCP-WIDGET
           WHEN    106
               MOVE    "PARA0305"           TO  MCP-WIDGET
           WHEN    107
               MOVE    "PARA0306"           TO  MCP-WIDGET
           WHEN    108
               MOVE    "PARA0307"           TO  MCP-WIDGET
           WHEN    109
               MOVE    "PARA0308"           TO  MCP-WIDGET
           WHEN    110
               MOVE    "PARA0309"           TO  MCP-WIDGET
           WHEN    111
               MOVE    "PARA0310"           TO  MCP-WIDGET
           WHEN    151
               MOVE    "CHK04"             TO  MCP-WIDGET
           WHEN    152
               MOVE    "PARA0401"           TO  MCP-WIDGET
           WHEN    153
               MOVE    "PARA0402"           TO  MCP-WIDGET
           WHEN    154
               MOVE    "PARA0403"           TO  MCP-WIDGET
           WHEN    155
               MOVE    "PARA0404"           TO  MCP-WIDGET
           WHEN    156
               MOVE    "PARA0405"           TO  MCP-WIDGET
           WHEN    157
               MOVE    "PARA0406"           TO  MCP-WIDGET
           WHEN    158
               MOVE    "PARA0407"           TO  MCP-WIDGET
           WHEN    159
               MOVE    "PARA0408"           TO  MCP-WIDGET
           WHEN    160
               MOVE    "PARA0409"           TO  MCP-WIDGET
           WHEN    161
               MOVE    "PARA0410"           TO  MCP-WIDGET
           WHEN    201
               MOVE    "CHK05"             TO  MCP-WIDGET
           WHEN    202
               MOVE    "PARA0501"           TO  MCP-WIDGET
           WHEN    203
               MOVE    "PARA0502"           TO  MCP-WIDGET
           WHEN    204
               MOVE    "PARA0503"           TO  MCP-WIDGET
           WHEN    205
               MOVE    "PARA0504"           TO  MCP-WIDGET
           WHEN    206
               MOVE    "PARA0505"           TO  MCP-WIDGET
           WHEN    207
               MOVE    "PARA0506"           TO  MCP-WIDGET
           WHEN    208
               MOVE    "PARA0507"           TO  MCP-WIDGET
           WHEN    209
               MOVE    "PARA0508"           TO  MCP-WIDGET
           WHEN    210
               MOVE    "PARA0509"           TO  MCP-WIDGET
           WHEN    211
               MOVE    "PARA0510"           TO  MCP-WIDGET
           WHEN    251
               MOVE    "CHK06"             TO  MCP-WIDGET
           WHEN    252
               MOVE    "PARA0601"           TO  MCP-WIDGET
           WHEN    253
               MOVE    "PARA0602"           TO  MCP-WIDGET
           WHEN    254
               MOVE    "PARA0603"           TO  MCP-WIDGET
           WHEN    255
               MOVE    "PARA0604"           TO  MCP-WIDGET
           WHEN    256
               MOVE    "PARA0605"           TO  MCP-WIDGET
           WHEN    257
               MOVE    "PARA0606"           TO  MCP-WIDGET
           WHEN    258
               MOVE    "PARA0607"           TO  MCP-WIDGET
           WHEN    259
               MOVE    "PARA0608"           TO  MCP-WIDGET
           WHEN    260
               MOVE    "PARA0609"           TO  MCP-WIDGET
           WHEN    261
               MOVE    "PARA0610"           TO  MCP-WIDGET
           WHEN    301
               MOVE    "CHK07"             TO  MCP-WIDGET
           WHEN    302
               MOVE    "PARA0701"           TO  MCP-WIDGET
           WHEN    303
               MOVE    "PARA0702"           TO  MCP-WIDGET
           WHEN    304
               MOVE    "PARA0703"           TO  MCP-WIDGET
           WHEN    305
               MOVE    "PARA0704"           TO  MCP-WIDGET
           WHEN    306
               MOVE    "PARA0705"           TO  MCP-WIDGET
           WHEN    307
               MOVE    "PARA0706"           TO  MCP-WIDGET
           WHEN    308
               MOVE    "PARA0707"           TO  MCP-WIDGET
           WHEN    309
               MOVE    "PARA0708"           TO  MCP-WIDGET
           WHEN    310
               MOVE    "PARA0709"           TO  MCP-WIDGET
           WHEN    311
               MOVE    "PARA0710"           TO  MCP-WIDGET
           WHEN    351
               MOVE    "CHK08"             TO  MCP-WIDGET
           WHEN    352
               MOVE    "PARA0801"           TO  MCP-WIDGET
           WHEN    353
               MOVE    "PARA0802"           TO  MCP-WIDGET
           WHEN    354
               MOVE    "PARA0803"           TO  MCP-WIDGET
           WHEN    355
               MOVE    "PARA0804"           TO  MCP-WIDGET
           WHEN    356
               MOVE    "PARA0805"           TO  MCP-WIDGET
           WHEN    357
               MOVE    "PARA0806"           TO  MCP-WIDGET
           WHEN    358
               MOVE    "PARA0807"           TO  MCP-WIDGET
           WHEN    359
               MOVE    "PARA0808"           TO  MCP-WIDGET
           WHEN    360
               MOVE    "PARA0809"           TO  MCP-WIDGET
           WHEN    361
               MOVE    "PARA0810"           TO  MCP-WIDGET
           WHEN    401
               MOVE    "CHK09"             TO  MCP-WIDGET
           WHEN    402
               MOVE    "PARA0901"           TO  MCP-WIDGET
           WHEN    403
               MOVE    "PARA0902"           TO  MCP-WIDGET
           WHEN    404
               MOVE    "PARA0903"           TO  MCP-WIDGET
           WHEN    405
               MOVE    "PARA0904"           TO  MCP-WIDGET
           WHEN    406
               MOVE    "PARA0905"           TO  MCP-WIDGET
           WHEN    407
               MOVE    "PARA0906"           TO  MCP-WIDGET
           WHEN    408
               MOVE    "PARA0907"           TO  MCP-WIDGET
           WHEN    409
               MOVE    "PARA0908"           TO  MCP-WIDGET
           WHEN    410
               MOVE    "PARA0909"           TO  MCP-WIDGET
           WHEN    411
               MOVE    "PARA0910"           TO  MCP-WIDGET
           WHEN    451
               MOVE    "CHK10"             TO  MCP-WIDGET
           WHEN    452
               MOVE    "PARA1001"           TO  MCP-WIDGET
           WHEN    453
               MOVE    "PARA1002"           TO  MCP-WIDGET
           WHEN    454
               MOVE    "PARA1003"           TO  MCP-WIDGET
           WHEN    455
               MOVE    "PARA1004"           TO  MCP-WIDGET
           WHEN    456
               MOVE    "PARA1005"           TO  MCP-WIDGET
           WHEN    457
               MOVE    "PARA1006"           TO  MCP-WIDGET
           WHEN    458
               MOVE    "PARA1007"           TO  MCP-WIDGET
           WHEN    459
               MOVE    "PARA1008"           TO  MCP-WIDGET
           WHEN    460
               MOVE    "PARA1009"           TO  MCP-WIDGET
           WHEN    461
               MOVE    "PARA1010"           TO  MCP-WIDGET
           WHEN    999
               MOVE    "B12"               TO  MCP-WIDGET
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
           WHEN    "CHK01"
               MOVE    002                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0101"
               MOVE    003                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0102"
               MOVE    004                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0103"
               MOVE    005                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0104"
               MOVE    006                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0105"
               MOVE    007                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0106"
               MOVE    008                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0107"
               MOVE    009                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0108"
               MOVE    010                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0109"
               MOVE    011                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0110"
               MOVE    051                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK02"
               MOVE    052                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0201"
               MOVE    053                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0202"
               MOVE    054                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0203"
               MOVE    055                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0204"
               MOVE    056                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0205"
               MOVE    057                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0206"
               MOVE    058                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0207"
               MOVE    059                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0208"
               MOVE    060                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0209"
               MOVE    061                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0210"
               MOVE    101                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK03"
               MOVE    102                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0301"
               MOVE    103                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0302"
               MOVE    104                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0303"
               MOVE    105                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0304"
               MOVE    106                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0305"
               MOVE    107                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0306"
               MOVE    108                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0307"
               MOVE    109                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0308"
               MOVE    110                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0309"
               MOVE    111                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0310"
               MOVE    151                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK04"
               MOVE    152                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0401"
               MOVE    153                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0402"
               MOVE    154                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0403"
               MOVE    155                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0404"
               MOVE    156                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0405"
               MOVE    157                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0406"
               MOVE    158                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0407"
               MOVE    159                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0408"
               MOVE    160                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0409"
               MOVE    161                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0410"
               MOVE    201                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK05"
               MOVE    202                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0501"
               MOVE    203                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0502"
               MOVE    204                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0503"
               MOVE    205                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0504"
               MOVE    206                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0505"
               MOVE    207                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0506"
               MOVE    208                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0507"
               MOVE    209                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0508"
               MOVE    210                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0509"
               MOVE    211                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0510"
               MOVE    251                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK06"
               MOVE    252                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0601"
               MOVE    253                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0602"
               MOVE    254                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0603"
               MOVE    255                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0604"
               MOVE    256                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0605"
               MOVE    257                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0606"
               MOVE    258                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0607"
               MOVE    259                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0608"
               MOVE    260                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0609"
               MOVE    261                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0610"
               MOVE    301                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK07"
               MOVE    302                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0701"
               MOVE    303                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0702"
               MOVE    304                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0703"
               MOVE    305                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0704"
               MOVE    306                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0705"
               MOVE    307                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0706"
               MOVE    308                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0707"
               MOVE    309                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0708"
               MOVE    310                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0709"
               MOVE    311                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0710"
               MOVE    351                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK08"
               MOVE    352                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0801"
               MOVE    353                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0802"
               MOVE    354                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0803"
               MOVE    355                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0804"
               MOVE    356                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0805"
               MOVE    357                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0806"
               MOVE    358                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0807"
               MOVE    359                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0808"
               MOVE    360                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0809"
               MOVE    361                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0810"
               MOVE    401                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK09"
               MOVE    402                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0901"
               MOVE    403                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0902"
               MOVE    404                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0903"
               MOVE    405                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0904"
               MOVE    406                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0905"
               MOVE    407                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0906"
               MOVE    408                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0907"
               MOVE    409                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0908"
               MOVE    410                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0909"
               MOVE    411                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA0910"
               MOVE    451                 TO  SPA-GMN-G01-CUR
           WHEN    "CHK10"
               MOVE    452                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1001"
               MOVE    453                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1002"
               MOVE    454                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1003"
               MOVE    455                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1004"
               MOVE    456                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1005"
               MOVE    457                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1006"
               MOVE    458                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1007"
               MOVE    459                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1008"
               MOVE    460                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1009"
               MOVE    461                 TO  SPA-GMN-G01-CUR
           WHEN    "PARA1010"
               MOVE    462                 TO  SPA-GMN-G01-CUR
           WHEN    "B12"
               MOVE    999                 TO  SPA-GMN-G01-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-CUR-SKIP-SEC               SECTION.
      *
      *    非活性項目にカーソルが移動した場合はスキップする
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WRK-CONS-SCR-MAX )
               COMPUTE WRK-CHK-CUR = ( IDX1    -   1 )
                                   * WRK-CONS-PARA-CUR
                                   + 1
               IF    ( SPA-GMN-G01-CUR     =   WRK-CHK-CUR  )
                AND  ( G01-CHK-STATE(IDX1) =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-G01-CUR =   SPA-GMN-G01-CUR   + 1
               END-IF
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   WRK-CONS-PARA-MAX )
                   COMPUTE WRK-CHK-CUR = WRK-CHK-CUR
                                       + 1
                   IF    ( SPA-GMN-G01-CUR     = WRK-CHK-CUR  )
                    AND  ( G01-PARA-STATE(IDX1 IDX2)
                                               = WIDGET-INSENSITIVE )
                       IF    ( IDX2    >=  WRK-CONS-PARA-MAX )
                           COMPUTE SPA-GMN-G01-CUR
                                 = IDX1    * WRK-CONS-PARA-CUR
                                 + 1
                       ELSE
                           COMPUTE SPA-GMN-G01-CUR
                               =   SPA-GMN-G01-CUR +   1
                       END-IF
                   END-IF
               END-PERFORM
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-G01-CUR >   462 )
               MOVE    999         TO  SPA-GMN-G01-CUR
           END-IF
      *
           .
      *
       5012-CUR-SKIP-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0002"
               STRING  "入力エラー"            DELIMITED BY  SIZE
                       "【"                    DELIMITED BY  SIZE
                       WRK-ERRMSG              DELIMITED BY  SPACE
                       "】"                    DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0003"
               MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
           WHEN    "0004"
               STRING  "必須項目が未入力です"  DELIMITED BY  SIZE
                       "【"                    DELIMITED BY  SIZE
                       WRK-ERRMSG              DELIMITED BY  SPACE
                       "】"                    DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0005"
               MOVE    "対象の処理が選択されていません"
                                                   TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "前頁はありません"          TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "次頁はありません"          TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "半角文字を規定文字数以内で入力してください"
                                                   TO  SPA-ERRMSG
           WHEN    "0102"
               STRING  "数字（０以上）を規定桁数以内で入力してください"
                                               DELIMITED BY  SPACE
                       "【"                    DELIMITED BY  SIZE
                       WRK-ERRMSG              DELIMITED BY  SPACE
                       "】"                    DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0103"
               STRING  "項目が規定の長さ（"    DELIMITED BY  SIZE
                       WRK-NUMMSG              DELIMITED BY  SPACE
                       "）を超えて入力されています"
                                               DELIMITED BY  SPACE
                       "【"                    DELIMITED BY  SIZE
                       WRK-ERRMSG              DELIMITED BY  SPACE
                       "】"                    DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0104"
               STRING  "他端末で月次統計業務使用中の為、"
                                               DELIMITED BY  SIZE
                       "実行できません"
                                               DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1001"
               MOVE    "更新ができませんでした"
                                                   TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                                   TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                                   TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR             TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR             TO  SPA-ERRMSG
               WHEN    "4001"
                   MOVE    "ＤＢの更新に失敗しました"
                                                   TO  SPA-ERRMSG
      *
           WHEN    "E88"
               MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  GERR
           MOVE    SPA-ERRCD           TO  GERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  GERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "G01"               TO  SPA-MOTOPG
           MOVE    "GERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "GERR"              TO  MCP-WINDOW
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
       520-GIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-GIDMSG
      *
           EVALUATE    SPA-GIDCD
           WHEN    "1001"
               MOVE    "選択された帳票の印刷を開始します"
                                        TO  WRK-GIDMSG
           WHEN    "6666"
               MOVE    "処理中です"     TO  WRK-GIDMSG
           WHEN    "8888"
               MOVE    "処理が正常終了しました"
                                        TO  WRK-GIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  GID1
           INITIALIZE                       GID1
           MOVE    SPA-GIDCD            TO  GID1-ID1CODE
           MOVE    WRK-GIDMSG           TO  GID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "G01"                TO  SPA-MOTOPG
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
      *    ガイドメッセージ表示
      *****************************************************************
       540-GID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-GIDMSG
      *
           EVALUATE    SPA-GIDCD
               WHEN    "1001"
                   MOVE    "選択された帳票の印刷を開始します"
                                        TO  WRK-GIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  GID2
           INITIALIZE                       GID2
           MOVE    SPA-GIDCD            TO  GID2-ID1CODE
           MOVE    WRK-GIDMSG           TO  GID2-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "G01"                TO  SPA-MOTOPG
           MOVE    "GID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "GID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       540-GID2SET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年月チェック・編集処理
      *****************************************************************
       5003-NENGETSU-CHK-SEC         SECTION.
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
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
           .
       5003-NENGETSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年月（期間）チェック・編集処理
      *****************************************************************
       5003-NENGETSU-KKN-CHK-SEC       SECTION.
      *
           IF    ( WRK-YMD (1:6)     =     "000000" )
               MOVE    "000000"       TO   WRK-HENYMDG
                                           WRK-SYMD
               GO  TO  5003-NENGETSU-KKN-CHK-EXT
           END-IF
      *
           IF    ( WRK-YMD (1:6)     =     "999999" )
               MOVE    "999999"       TO   WRK-HENYMDG
                                           WRK-SYMD
               GO  TO  5003-NENGETSU-KKN-CHK-EXT
           END-IF
      *
           PERFORM 5003-NENGETSU-CHK-SEC
      *
           .
       5003-NENGETSU-KKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カナチェック処理
      *****************************************************************
       800-KANA-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-ATO-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-ATO-INPUT
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    使用可否チェック処理
      *****************************************************************
       800-IS-ENABLED-SEC        SECTION.
      *
           MOVE    1               TO  FLG-ENABLED
      *
           IF    ( SPA-MW                       =   SPA-GINBEE )
             AND ( SPA-GMN-G01-REPOS-NAME (IDXI)
                                            NOT =   SPACE )
             AND ( SPA-G01-CUSTOM-ENABLED   NOT =   "1" )
               MOVE    ZERO        TO  FLG-ENABLED
           END-IF
      *
           IF    ( SPA-MW                       =   SPA-GINBEE )
             AND ( SPA-GMN-G01-REPOS-NAME (IDXI)
                                               =   SPACE )
               MOVE    ZERO        TO  IDXU
               PERFORM VARYING IDXT    FROM    1   BY  1
                       UNTIL ( IDXT    >   SPA-G01-PGLST-MAX )
                        OR   ( IDXU    >   ZERO )
      *
                   IF    ( SPA-GMN-G01-PRGNAME (IDXI)
                                       =   SPA-G01-TPGID (IDXT))
                       MOVE    IDXT    TO  IDXU
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXU            =   ZERO )
                   MOVE    ZERO        TO  FLG-ENABLED
               END-IF
      *
           END-IF
      *
           .
       800-IS-ENABLED-EXT.
           EXIT.
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-ERR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU NOT =   1     )
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
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
      *****************************************************************
      *    ＬＤユーザーチェック処理
      *****************************************************************
       800-LDUSER-CHK-SEC              SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    52              TO  SLDUSER-MY-LD
           MOVE    1               TO  SLDUSER-KBN
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       800-LDUSER-CHK-EXT.
           EXIT.
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
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF    ( MCP-RC          =   ZERO )
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
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
      *    システム管理マスタ読込処理
      *****************************************************************
       900-SYSKANRI-NEXT-SEC            SECTION.
      *
           MOVE  "DBFETCH"             TO  MCP-FUNC
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   ( MCP-RC               =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3002-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
      *
       120-FECH-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    統計メモテーブル検索処理
      *****************************************************************
       900-TOUKEIMEMO-KEY2-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUKEIMEMO
      *
           INITIALIZE                      TOUKEIMEMO-REC
           MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
           MOVE    WRK-PGID            TO  TKIMEMO-PGID
           MOVE    "3002"              TO  TKIMEMO-KANRICD
           MOVE    WRK-KBNCD-G         TO  TKIMEMO-KBNCD
           MOVE    SPA-SYSYMD          TO  TKIMEMO-STYUKYMD
                                           TKIMEMO-EDYUKYMD
           MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TOUKEIMEMO-REC
           ELSE
               INITIALIZE                  TOUKEIMEMO-REC
               MOVE    1               TO  FLG-TOUKEIMEMO
           END-IF
      *
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TOUKEIMEMO-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
      *    DBUPDATE処理
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
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
