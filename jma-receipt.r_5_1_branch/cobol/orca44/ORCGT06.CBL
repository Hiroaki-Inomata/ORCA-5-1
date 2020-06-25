      ********************************************************************
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
       PROGRAM-ID.             ORCGT06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : レセ電データ個別指示
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/29     NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/11/19  レセ電ＣＤ−Ｒ出力対応
      *  04.04.02    NACL-藤原    09/11/25  症状詳記作成区分追加
      *  04.04.03    NACL-藤原    09/12/10  返戻対応
      *  04.04.04    NACL-藤原    09/03/31  主科設定時の主科未設定データ
      *                                     の有無チェック
      *
      *  04.05.01    NACL-藤原    09/08/21  レセ電データ格納フォルダ設定
      *                                     対応
      *                                     ファイル出力先のデフォルト値
      *                                     の設定も可能にする 
      *  04.05.02    NACL-藤原    10/03/23  ＨＯＳＴ名取得方法の変更
      *  04.05.03    NACL-藤原    11/03/28  児童保護(53)の公費単独のレセ
      *                                     プトの負担者番号等の表示
      *  04.05.04    NACL-藤原    11/04/20  災害対応
      *                                     システム管理の設定に従い、災
      *                                     害該当レセプトのレセ電データ
      *                                     の作成のチェックを追加
      *  04.05.05    NACL-藤原    12/03/27  算定日情報に係る記録条件仕様
      *                                     の変更に伴う月後れ分のチェック
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *
      *  04.07.01    NACL-藤原    12/01/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.02    NACL-藤原    12/08/01  レセ電データチェック機能対応
      *  04.07.03    NACL-藤原    12/08/28  スクリプト起動パスサーチ対応
      *  04.07.04    NACL-藤原    12/11/19  主科情報の取得方法変更
      *  04.07.05    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.06    NACL-藤原    13/10/07  労災レセ電対応
      *
      *  04.08.01    NACL-森脇    13/04/12  システムＤＢ対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    14/08/26  fdd 廃止に伴う対応
      *  04.08.04    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.05    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *  04.08.06    NACL-藤原    17/02/13  医療機関コード変更時のレセ電
      *                                     データ等の作成時のパラメタ修正
      *  04.08.07    NACL-藤原    17/03/08  初画面の診療年月が月途中の医療
      *                                     期間コード変更月のときの対応
      *  04.08.08    NACL-藤原    17/03/09  期間表示の設定の追加
      *  04.08.09    NACL-藤原    18/01/11  労災のとき期間表示のチェック
      *                                     は行わない
      *  04.08.10    NACL-藤原    18/03/28  平成３０年４月改正対応
      *
      *  05.00.00    NACL-森脇    17/12/21  クライアント印刷対応
      *  05.00.01    NACL-藤原    17/12/19  ＨＡＯＲＩ対応
      *  05.00.02    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *  05.00.03    NACL-藤原    19/01/17  Warning 対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING   //TEMPFLPARA//
                                   BY          //RECEERR//.
      *
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "T01COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "T06SCR-SPA".
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-LNK-//.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ONRECE-K            PIC 9(01).
           03  FLG-ONRECE-B            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SEIKYU              PIC 9(01).
           03  FLG-BTPARA              PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-HENREI-B            PIC 9(01).
           03  FLG-RENKEI-TMP          PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-RECE-ROW            PIC 9(01).
           03  FLG-BODY-ROW            PIC 9(01).
           03  FLG-TOUROKU             PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-HENREI-B-SEIKYU-CHK PIC 9(01).
           03  FLG-SYUKA-CHK           PIC 9(01).
           03  FLG-SET                 PIC 9(01).
      *
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
       01  CNT-AREA.
           03  CNT-RECE-HIT        PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
           03  IDWW                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(05).
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-TIDMSG          PIC X(200).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZZ9           PIC ZZZZ9.
           03  WRK-ZZZ9             PIC ZZZ9.
           03  WRK-WRNO-X.
               05  WRK-WRNO        PIC 9(02).
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-RENNUM          PIC 9(03).
      *
           03  WRK-IDX             PIC 9(02).
           03  WRK-ST              PIC 9(02).
           03  WRK-LEN             PIC 9(02).
      *     
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC  X(06).
             05  WRK-SHELL-TERMID  PIC  X(16).
             05  WRK-SHELL-SYSYMD  PIC  X(08).  
      *
           03  WRK-SHELL-RECEKBN   PIC X(01).
           03  WRK-SHELL-SYOKBN    PIC X(01).
      *  
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
      *
           03  WRK-T06-BODY-ROW    PIC S9(09).
           03  WRK-T06-RECE-ROW    PIC S9(09).
      *
      *    パラメタＤＢ編集用
           03  WRK-BTPARA-INFO-PARA.
               05  WRK-BTPARA-DATA PIC X(10).
               05  WRK-BTPARA-FILENM
                                   PIC X(20).
               05  WRK-BTPARA-DATAKBN
                                   PIC X(01).
               05  WRK-BTPARA-STNO PIC 9(02).
               05  WRK-BTPARA-EDNO PIC 9(02).
               05  WRK-BTPARA-WRNO PIC 9(02).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-PRT-PAGE        PIC 9(10).
           03  WRK-PRT-PAGE1       PIC 9(10).
           03  WRK-PRT-PAGE2       PIC 9(06).
      *
           03  WRK-SYS-SRYYMD      PIC X(08).
      *
           03  WRK-SRYYMKBN.
               05  WRK-SRYYMKBN-ST PIC X(06).
               05  WRK-SRYYMKBN-FL PIC X(01).
               05  WRK-SRYYMKBN-ED PIC X(06).
               05  WRK-SRYYMKBN-ERR
                                   PIC X(02).
      *
           COPY    "CPORCSPRTLNK.INC".
      *
       01  WRK-MSG.
           03  WRK-MSG1.
               05  WRK-MSG1-WRNO   PIC Z9.
               05  FILLER          PIC X(01)   VALUE   "/".
               05  WRK-MSG1-EDNO   PIC Z9.     
      *
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"    REPLACING  //SPA-//
                                   BY         //WRK-SPA-//.
      *
       01  WRK-HEN-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG.
               05  WRK-HENYM       PIC X(06).
               05  WRK-HENYM-FIL   PIC X(03).
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
           03  WRK-T99-MSG.
               05  WRK-T99-MSG1    PIC X(24)   VALUE
                   "処理は正常に終了しました".
               05  FILLER          PIC X(16)   VALUE
                   "　レセプト総件数".
               05  WRK-T99-MSG-KENSU
                                   PIC ZZZZZ9.
               05  FILLER          PIC X(02)   VALUE   "件".
      *
      *    ワーク請求管理
           COPY    "CPRCF010.INC"  REPLACING  //RECE10-//
                                   BY         //WRK-RECE10-//.
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
      *    退避用
       01  WRK-ONRECE-K-REC.
           COPY    "CPONRECE-KANRI.INC"
                                   REPLACING  //ONRECE-K-//
                                   BY         //WRK-ONRECE-K-//.
      *
       01  WRK-CONS-AREA.
      *    個別指示一覧最大行
           03  WRK-CONS-LINE-MAX   PIC 9(04)   VALUE   500.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *     
      *    ジョブ管理ＤＢのキー値（レセプト）  
           03  WRK-CONS-JOB-SHELLID-RECEPT1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID-RECEPT1
                                   PIC 9(07)   VALUE   1.
      *     
      *    ジョブ管理ＤＢのキー値（データ収集）  
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                  PIC X(08)    VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
      *     
      *    パラメタＤＢのキー値（レセ電作成）  
           03  WRK-CONS-BTPARA-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-BTPARA-SCRIPTID
                                   PIC X(10)   VALUE   "recept4.sh".
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR044ERR".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1031.INC".
      *700-EXPORT-SEC
           COPY    "CPSK3003.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
           COPY    "CPSK2009.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
       01  ONRECE-K-REC.
           COPY    "CPONRECE-KANRI.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC". 
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
           COPY    "MONBLOB".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA44SCRAREA.INC".
      *
       PROCEDURE                       DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T06
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
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
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T06         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "TERR"
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
               IF      SPA-TIDCD       NOT =   SPACE
                   EVALUATE    SPA-TIDCD
                       WHEN    "1002"
                           PERFORM 530-TID2SET-SEC
                           GO  TO  100-INIT-EXT
                       WHEN    "1006"
                       WHEN    "1011"
                       WHEN    "1012"
                           PERFORM 520-TIDSET-SEC
                           GO  TO  100-INIT-EXT
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "T06    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
      *    他のＬＤより遷移
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XA01"  OR  "XB01"  OR
                                           "XC01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-T06
      *************MOVE    SPA-WORK        TO  SPA-T01KYOUTU
                   MOVE    SPA-WORK        TO  SPA-T01KYOUTU
                   INITIALIZE              SPA-T01KYOUTU-H
                   GO  TO  300-SCREEN-EXT
               END-IF
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
               IF      WRK-MOTOPG(1:3)     =  "P97"  
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-T06
      *************MOVE    SPA-WORK        TO  SPA-T01KYOUTU
                   MOVE    SPA-WORK        TO  SPA-T01KYOUTU
                   INITIALIZE              SPA-T01KYOUTU-H
                   PERFORM 3101-U01SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "T99"
                   PERFORM 320-T99-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "TID1"
                   PERFORM 330-TID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "TID2"
                   PERFORM 340-TID2-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "T08"
                   INITIALIZE              SPA-T01KYOUTU-H
                   GO  TO  300-SCREEN-EXT
               WHEN    "T07"
                   IF      SPA-T07-FLG     =   "1"
                       MOVE    "1002"          TO  SPA-TIDCD
                   ELSE
                       INITIALIZE              SPA-T01KYOUTU-H
    ***                INITIALIZE              SPA-GMN-T06-RECE-KOUMOKU
    ***                                        SPA-NAI-T06-RECE-KOUMOKU
                       MOVE    4               TO  SPA-GMN-T06-CUR
                   END-IF
                   GO  TO  300-SCREEN-EXT
               WHEN    "T98"
                   PERFORM 350-T98-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "T09"
                   PERFORM 340-T09-SET-SEC
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
      *    他のＬＤより遷移処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    患者検索より
           IF      WRK-SPA-PTNUM   NOT =   SPACE
                   MOVE    WRK-SPA-PTNUM   TO  SPA-GMN-T06-PTNUM
               PERFORM 4103-PTNUM-CHK-SEC
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-T06-CUR
           END-IF
           .
       3101-U01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-T06-AREA
      *
      *    直接請求する保険者番号                                             
           MOVE    SPA-T01-DIRECT-HKNJANUM-AREA
                                       TO
                                       SPA-NAI-T06-DIRECT-HKNJA-AREA
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                  SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    "*"                 TO  SYS-1031-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1031-STYUKYMD
                                           SYS-1031-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1031-HOSPNUM
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
           ELSE    
               INITIALIZE                  SYS-1031-REC
           END-IF
           MOVE    SYS-1031-PRTNM (5)
                                   TO  SPA-NAI-T06-1031-PRTNM
      *
      *    診療年月
           IF      SPA-SYSYMD(7:2)     >   "12"
               MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-T06-SRYYM-H
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-T06-SRYYM-H
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE  "61"                  TO  LNK-DAY6-IRAI
               MOVE  SPA-SYSYMD            TO  LNK-DAY6-KIJUN
               MOVE  "2"                   TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                    TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  SPA-NAI-T06-SRYYM-H
               MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD  
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYM           TO  SPA-GMN-T06-SRYYM-H
           END-IF
      *
      *    システム日付−２ヵ月
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE  "61"                  TO  LNK-DAY6-IRAI
           MOVE  SPA-SYSYMD            TO  LNK-DAY6-KIJUN
           MOVE  "2"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -2                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  SPA-NAI-T06-SYSYM-2
      *
      *    請求年月日
           MOVE    SPA-SYSYMD          TO  SPA-NAI-T06-SKYYMD-H
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-T06-SKYYMD-H
      *
      *    支払先
           MOVE    "T"                 TO  SPA-GMN-T06-TEISYUTUSAKI1
           MOVE    "F"                 TO  SPA-GMN-T06-TEISYUTUSAKI2
           MOVE    "F"                 TO  SPA-GMN-T06-TEISYUTUSAKI3
           MOVE    "F"                 TO  SPA-GMN-T06-TEISYUTUSAKI4
           MOVE    WIDGET-NORMAL       TO
                                       SPA-GMN-T06-TEISYUTUSAKI1-STATE
                                       SPA-GMN-T06-TEISYUTUSAKI2-STATE
                                       SPA-GMN-T06-TEISYUTUSAKI4-STATE
           MOVE    WIDGET-INSENSITIVE  TO
                                       SPA-GMN-T06-TEISYUTUSAKI3-STATE
           IF    ( SPA-NAI-T06-SRYYM-H <   201307      )
           OR    ( SPA-T01-PRTKBN      =   "6" OR  "7" )
               MOVE    WIDGET-INSENSITIVE  TO
                                       SPA-GMN-T06-TEISYUTUSAKI4-STATE
           END-IF
      *
      *    症状詳記作成区分
           INITIALIZE                      SYS-25-REC
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    SPA-NAI-T06-SRYYM-H TO  SYS-25-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
           MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE   MCPDATA-REC          TO  SYS-25-REC
           ELSE
               INITIALIZE                  SYS-25-REC
           END-IF
           IF      SYS-25-RECEDEN-LST1KBN
                                       =   SPACE
               MOVE    "1"                 TO
                                           SYS-25-RECEDEN-LST1KBN
           END-IF 
           IF      SYS-25-RECEDEN-RIREKIKBN
                                       =   SPACE
               MOVE    "0"                 TO
                                           SYS-25-RECEDEN-RIREKIKBN
           END-IF 
           IF     SYS-25-RECEDEN-DISASTER
                                       =   SPACE
               MOVE    "1"             TO  SYS-25-RECEDEN-DISASTER
           END-IF
           MOVE    SYS-25-RECEDEN-LST1KBN
                                       TO
                                   SPA-T06-2005-RECEDEN-LST1KBN
           MOVE    SYS-25-RECEDEN-RIREKIKBN
                                       TO
                                   SPA-T06-2005-RECEDEN-RIREKIKBN
           MOVE    SYS-25-RECEDEN-DISASTER
                                       TO
                                   SPA-T06-2005-RECEDEN-DISASTER
      *
      *    レセプト編集区分情報（主科情報）
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    SPA-NAI-T06-SRYYM-H TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2009-REC
           ELSE
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   "0"
               MOVE    "0"             TO  SYS-2009-SYUKA-SYAHO
                                           SYS-2009-SYUKA-KOKUHO
                                           SYS-2009-SYUKA-KOUIKI
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-SYAHO
                                   =   SPACE )
               MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
           END-IF      
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOKUHO
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOUIKI
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOUIKI
           END-IF
           MOVE    SYS-2009-SYUKA-SYAHO
                                       TO  SPA-NAI-T06-2009-SYUKA-SYAHO
           MOVE    SYS-2009-SYUKA-KOKUHO
                                       TO  SPA-NAI-T06-2009-SYUKA-KOKUHO
           MOVE    SYS-2009-SYUKA-KOUIKI
                                       TO  SPA-NAI-T06-2009-SYUKA-KOUIKI
      *
      *    労災医療機関情報取得
           MOVE     SPACE              TO  SPA-NAI-T06-4001-T
           INITIALIZE                      SPA-NAI-T06-4001-T
           MOVE    "20130701"          TO  WRK-SYS-SRYYMD
      *
           INITIALIZE                      SYS-4001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM UNTIL ( FLG-SYSKANRI    =   1  )
                       OR    ( SPA-NAI-T06-4001-MAX
                                               >=  10 )
                   MOVE    MCPDATA-REC             TO  SYS-4001-REC
                   IF      SYS-4001-EDYUKYMD       <   WRK-SYS-SRYYMD
                       CONTINUE
                   ELSE
                       ADD     1               TO  SPA-NAI-T06-4001-MAX
                       MOVE    SYS-4001-STYUKYMD
                                               TO
                                           SPA-NAI-T06-4001-STYUKYMD
                                                  (SPA-NAI-T06-4001-MAX)
                       MOVE    SYS-4001-EDYUKYMD
                                               TO
                                           SPA-NAI-T06-4001-EDYUKYMD
                                                  (SPA-NAI-T06-4001-MAX)
                       MOVE    SYS-4001-RSIIRYOKBN
                                               TO
                                           SPA-NAI-T06-4001-RSIIRYOKBN
                                                  (SPA-NAI-T06-4001-MAX)
                   END-IF
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key8"            TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    レセ電データチェック 
           MOVE    SPA-T01-RECEDEN-CHK TO
                                   SPA-T06-2005-RECEDEN-CHK-KBN
      *
      *    診療年月期間設定処理
           PERFORM 3105-SRYYMKBN-HENSYU-SEC
      *
      *    月途中変更の診療年月かチェック
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T06-SRYYMKBN-MAX
               IF      ( SPA-NAI-T06-SRYYMKBN-ST (IDX)
                                   <=   SPA-NAI-T06-SRYYM-H )
               AND     ( SPA-NAI-T06-SRYYMKBN-ED (IDX)
                                   >=   SPA-NAI-T06-SRYYM-H )
                   IF      SPA-NAI-T06-SRYYMKBN-ERR (IDX)
                                       =   "1"
                       MOVE    ZERO    TO  SPA-GMN-T06-SRYYMKBN-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       10
                           INITIALIZE  SPA-GMN-T06-SRYYMKBNLST (IDX)
                       END-PERFORM
      *****************MOVE    "0051"          TO  SPA-ERRCD
                       MOVE
                   "月途中に医療機関コードが変更されている診療年月です"
                                               TO  SPA-GMN-T06-TITLE3
                   END-IF
                   MOVE    SPA-GMN-T06-SRYYMKBN-MAX
                                           TO  IDX
               END-IF
           END-PERFORM
      *
      *    管理情報編集処理
           PERFORM 3101-ONRECE-KANRI-HENSYU-SEC
      *
      *    点検レセ０点公費記載の請求管理のチェック処理
           PERFORM 3104-SEIKYU-CHECK-SEC
      *
           MOVE    1               TO  SPA-GMN-T06-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理情報編集処理
      *****************************************************************
       3101-ONRECE-KANRI-HENSYU-SEC    SECTION.
      *
      *??
        DISPLAY " 3101 SPA-NAI-T06-HOSPCD=" SPA-NAI-T06-HOSPCD "#"
      *??
      *
           INITIALIZE                  SPA-GMN-T06-KANRI-TBL
                                       SPA-NAI-T06-KANRI-TBL
                                       SPA-GMN-T06-BODY-TBL
                                       SPA-NAI-T06-BODY-TBL
                                       SPA-GMN-T06-RECE-TBL
                                       SPA-NAI-T06-RECE-TBL
      *
           INITIALIZE                  SPA-NAI-T06-KANRI-SELNUM
                                       SPA-NAI-T06-BODY-SELNUM
                                       SPA-NAI-T06-SYORIFLG
      *
           INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
      *
      *    労災は対象外
           IF      SPA-GMN-T06-TEISYUTUSAKI4
                                   =   "T"
               GO  TO  3101-ONRECE-KANRI-HENSYU-EXT
           END-IF 
      *
           IF      SPA-T01-PRTKBN  =   "6" OR  "7"
               GO  TO  3101-ONRECE-KANRI-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO            TO  IDX
      *
           INITIALIZE                  ONRECE-K-REC
           MOVE   SPA-HOSPNUM      TO  ONRECE-K-HOSPNUM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                   =   "T"
               MOVE   "1"              TO  ONRECE-K-TEISYUTUSAKI
           ELSE
               MOVE   "2"              TO  ONRECE-K-TEISYUTUSAKI
           END-IF
           MOVE   SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-K-SRYYM
      *
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-KANRI-SELECT-SEC
      *
           PERFORM         UNTIL   FLG-ONRECE-K    =   1
               MOVE    ZERO            TO  FLG-HIT
      *
               IF      SPA-GMN-T06-SRYYMKBN-MAX
                                       =   ZERO
                   MOVE    1               TO  FLG-HIT
               ELSE
      *??
        DISPLAY "ONRECE-K-HOSPCD=" ONRECE-K-HOSPCD "#"
                             ONRECE-K-UKEYMD "#"
      *??
                   IF      ONRECE-K-HOSPCD =   SPACE
                       MOVE    SPA-HOSPNUM     TO  ONRECE-B-HOSPNUM
                       MOVE    ONRECE-K-TEISYUTUSAKI
                                               TO  ONRECE-B-TEISYUTUSAKI
                       MOVE    ONRECE-K-SRYYM  TO  ONRECE-B-SRYYM
                       MOVE    ONRECE-K-HOSPCD TO  ONRECE-B-HOSPCD
                       MOVE    ONRECE-K-UCNT   TO  ONRECE-B-UCNT
                       MOVE    ONRECE-K-SCNT   TO  ONRECE-B-SCNT
                       MOVE    ZERO            TO  ONRECE-B-RENNUM
                       PERFORM 900-ONRECE-BODY-READ-SEC
      *??
        DISPLAY "ONRECE-B-SRYYM=" ONRECE-B-SRYYM-B "#"
      *??
                       PERFORM VARYING IDYY    FROM    1   BY  1
                               UNTIL   IDYY    >       100
                               OR      SPA-T01-1001-TSTYUKYMD (IDYY)
                                               =       SPACE
                           IF    ( ONRECE-B-SRYYM-B
                                       >=  SPA-T01-1001-TSTYUKYMD (IDYY)
                                                            (1:6) )
                           AND   ( ONRECE-B-SRYYM-B
                                       <=  SPA-T01-1001-TEDYUKYMD (IDYY)
                                                            (1:6) )
      *??
        DISPLAY "1001=" SPA-T01-1001-THOSPCD (IDYY) "#"
      *??
                               IF      SPA-T01-1001-THOSPCD (IDYY)
                                               =   SPA-NAI-T06-HOSPCD
                                   MOVE    1               TO  FLG-HIT
                               END-IF
                               MOVE    100             TO  IDYY
                           END-IF
                       END-PERFORM
                   ELSE
                       IF      ONRECE-K-HOSPCD =   SPA-NAI-T06-HOSPCD
                                   MOVE    1               TO  FLG-HIT
                       END-IF
                   END-IF
               END-IF
      *
      *??
        DISPLAY "FLG-HIT=" FLG-HIT "#" ONRECE-K-UKEYMD "#"
      *??
               IF      FLG-HIT         =   1
                   ADD     1           TO  IDX
                   MOVE    IDX         TO  SPA-GMN-T06-K-TRENNUM (IDX)
                   MOVE    ONRECE-K-UKEYMD
                                       TO  SPA-NAI-T06-K-TUKEYMD (IDX)
                                           WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-T06-K-TUKEYMD (IDX)
                   MOVE    ONRECE-K-UCNT
                                       TO  SPA-GMN-T06-K-TUCNT   (IDX)
                   MOVE    ONRECE-K-SKYYMD
                                       TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-T06-K-TSKYYMD (IDX)
                   MOVE    ONRECE-K-SCNT
                                       TO  SPA-GMN-T06-K-TSCNT   (IDX)
                   MOVE    ONRECE-K-NOSEND-PATIENT-CNT
                                       TO
                                   SPA-GMN-T06-K-TNOSEND-PAT-CNT (IDX)
                   MOVE    ONRECE-K-SEND-TOTAL-CNT
                                       TO
                                   SPA-GMN-T06-K-TSEND-TOTAL-CNT (IDX)
                   MOVE    ONRECE-K-HOSPCD
                                       TO  SPA-NAI-T06-K-THOSPCD (IDX)
               END-IF
      * 
               MOVE    "tbl_onrece_kanri"
                                       TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-KANRI-FETCH-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-T06-KANRI-MAX
      *
           IF      IDX             >   ZERO
               MOVE    1               TO  SPA-NAI-T06-KANRI-SELNUM
                                           T06-KANRI-LIST-ROW
      *        明細情報編集処理
               PERFORM 3102-ONRECE-BODY-HENSYU-SEC
           END-IF
      *
           .
       3101-ONRECE-KANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細情報編集処理
      *****************************************************************
       3102-ONRECE-BODY-HENSYU-SEC     SECTION.
      *
           INITIALIZE                  SPA-GMN-T06-BODY-TBL
                                       SPA-NAI-T06-BODY-TBL
      *
           MOVE    ZERO            TO  IDXX
      *
           INITIALIZE                  ONRECE-B-REC
           MOVE   SPA-HOSPNUM      TO  ONRECE-B-HOSPNUM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                   =   "T"
               MOVE   "1"              TO  ONRECE-B-TEISYUTUSAKI
           ELSE
               MOVE   "2"              TO  ONRECE-B-TEISYUTUSAKI
           END-IF
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-B-SRYYM
           MOVE    SPA-GMN-T06-K-TUCNT (SPA-NAI-T06-KANRI-SELNUM)
                                   TO  ONRECE-B-UCNT
           MOVE    SPA-GMN-T06-K-TSCNT (SPA-NAI-T06-KANRI-SELNUM)
                                   TO  ONRECE-B-SCNT
           MOVE    ZERO            TO  ONRECE-B-RENNUM
           MOVE    SPA-NAI-T06-K-THOSPCD   (SPA-NAI-T06-KANRI-SELNUM)
                                   TO  ONRECE-B-HOSPCD
      *
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
      *
           PERFORM         UNTIL   FLG-ONRECE-B    =   1
                           OR      IDXX            >=  1000
               ADD     1               TO  IDXX
               MOVE    IDXX            TO  SPA-GMN-T06-B-TRENNUM (IDXX)
               MOVE    ONRECE-B-NYUGAIKBN
                                       TO  SPA-NAI-T06-B-TNYUGAIKBN
                                                                 (IDXX)
               EVALUATE    SPA-NAI-T06-B-TNYUGAIKBN (IDXX)
                   WHEN    "1"
                       MOVE    "入"    TO
                                       SPA-GMN-T06-B-TNYUGAIKBN(IDXX)
                   WHEN    "2"
                       MOVE    "外"    TO
                                       SPA-GMN-T06-B-TNYUGAIKBN(IDXX)
               END-EVALUATE
               MOVE    ONRECE-B-PTNUM  TO  SPA-GMN-T06-B-TPTNUM  (IDXX)
               MOVE    ONRECE-B-PTID   TO  SPA-NAI-T06-B-TPTID   (IDXX)
               MOVE    ONRECE-B-U-HENREI-FLG
                                       TO  SPA-NAI-T06-B-THENREI-FLG
                                                                 (IDXX)
               MOVE    ONRECE-B-NAME   TO  SPA-GMN-T06-B-TNAME   (IDXX)
               MOVE    ONRECE-B-SRYYM-B
                                       TO  SPA-NAI-T06-B-TSRYYM  (IDXX)
                                           WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYM       TO  SPA-GMN-T06-B-TSRYYM   (IDXX)
               MOVE    ONRECE-B-BIRTHDAY
                                       TO  SPA-NAI-T06-B-TBIRTHDAY(IDXX)
                                           WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-T06-B-TBIRTHDAY(IDXX)
               MOVE    ONRECE-B-RECESYUBETU
                                       TO  SPA-GMN-T06-B-TRECESYUBETU
                                                                  (IDXX)
               MOVE    ONRECE-B-HKNJANUM
                                       TO  SPA-GMN-T06-B-THKNJANUM(IDXX)
               STRING  ONRECE-B-KIGO   DELIMITED   BY  SPACE
                       "　"            DELIMITED   BY  SIZE
                       ONRECE-B-NUM    DELIMITED   BY  SPACE
                                       INTO    SPA-GMN-T06-B-TKIGO(IDXX)
               END-STRING
               MOVE    ONRECE-B-U-ERR-CODE
                                       TO  SPA-GMN-T06-B-TERRCD   (IDXX)
               MOVE    ONRECE-B-U-ERR-MSG-1
                                       TO  SPA-GMN-T06-B-TERR     (IDXX)
      *
               MOVE    ONRECE-B-RECEIPT-NO
                                       TO  SPA-NAI-T06-B-TRECEIPT-NO
                                                                  (IDXX)
      ***      MOVE    ONRECE-B-SEX    TO  SPA-NAI-T06-B-TSEX     (IDXX)
      * 
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDXX            TO  SPA-GMN-T06-BODY-MAX
      *
           .
       3102-ONRECE-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点検レセ０点公費記載の請求管理のチェック処理
      *****************************************************************
       3104-SEIKYU-CHECK-SEC            SECTION.
      *
           INITIALIZE                      SPA-NAI-T06-RECEDEN-CHK
      *
      *****社保分チェック
      *     MOVE    SPACE               TO  RECE10-REC
      *    INITIALIZE                       RECE10-REC
      *     MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
      *     MOVE    SPA-NAI-T06-SRYYM-H TO  RECE10-SKYYM
      *     MOVE    "%"                 TO  RECE10-NYUGAIKBN
      *     MOVE    1                   TO  RECE10-TEISYUTUSAKI
      *     MOVE    "key78"             TO  WRK-MCP-PATHNAME
      *     PERFORM 900-SEIKYU-INV-SEC
      *
      *     IF      FLG-SEIKYU          =   ZERO
      *         MOVE    "1"                 TO  SPA-NAI-T06-RECEDEN-CHK1
      *     END-IF
      *
      *    国保分チェック
      *     MOVE    SPACE               TO  RECE10-REC
      *     INITIALIZE                      RECE10-REC
      *     MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
      *     MOVE    SPA-NAI-T06-SRYYM-H TO  RECE10-SKYYM
      *     MOVE    "%"                 TO  RECE10-NYUGAIKBN
      *     MOVE    2                   TO  RECE10-TEISYUTUSAKI
      *     MOVE    "key78"             TO  WRK-MCP-PATHNAME
      *     PERFORM 900-SEIKYU-INV-SEC
      *
      *     IF      FLG-SEIKYU          =   ZERO
      *         MOVE    "1"                 TO  SPA-NAI-T06-RECEDEN-CHK2
      *     END-IF
      *
      *    広域分チェック
      *     MOVE    SPACE               TO  RECE10-REC
      *     INITIALIZE                      RECE10-REC
      *     MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
      *     MOVE    SPA-NAI-T06-SRYYM-H TO  RECE10-SKYYM
      *     MOVE    "%"                 TO  RECE10-NYUGAIKBN
      *     MOVE    6                   TO  RECE10-TEISYUTUSAKI
      *     MOVE    "key78"             TO  WRK-MCP-PATHNAME
      *     PERFORM 900-SEIKYU-INV-SEC
      *
      *     IF      FLG-SEIKYU          =   ZERO
      *         MOVE    "1"                 TO  SPA-NAI-T06-RECEDEN-CHK3
      ******END-IF
      *
      *    ０点公費の有無チェック
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-NAI-T06-SRYYM-H TO  RECE10-SKYYM
           MOVE    "%"                 TO  RECE10-NYUGAIKBN
           MOVE    "key79"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
      *
           IF      FLG-SEIKYU          =   ZERO
               MOVE    "111"               TO  SPA-NAI-T06-RECEDEN-CHK
           END-IF
      *
           .
       3104-SEIKYU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月期間設定処理
      *****************************************************************
       3105-SRYYMKBN-HENSYU-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-T06-SRYYMKBN-G
           MOVE    ZERO                TO  SPA-GMN-T06-SRYYMKBN-MAX
                                           SPA-NAI-T06-SRYYMKBN-SELNUM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               INITIALIZE      SPA-GMN-T06-SRYYMKBNLST (IDX)
                               SPA-NAI-T06-SRYYMKBNLST (IDX)
           END-PERFORM
      *
           IF      SPA-T01-PRTKBN      =   "6" OR  "7"
               GO  TO  3105-SRYYMKBN-HENSYU-EXT
           END-IF
      *
      *??  IF      SPA-NAI-T06-SRYYM-H <   201604
      *??      GO  TO  3105-SRYYMKBN-HENSYU-EXT
      *??  END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-T01-1001-MAX
               IF      SPA-T01-1001-TSTYUKYMD (IDX) (1:6)
                                       >   SPA-NAI-T06-SRYYM-H
                 CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-T06-SRYYMKBN-MAX
                   INITIALIZE                  SPA-GMN-T06-SRYYMKBNLST
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
                                               SPA-NAI-T06-SRYYMKBNLST
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
                   MOVE    SPA-GMN-T06-SRYYMKBN-MAX
                                           TO  WRK-WRNO
                   MOVE    WRK-WRNO-X      TO  SPA-GMN-T06-SRYYMKBNL
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
      *
                   MOVE    SPACE           TO  WRK-SRYYMKBN
                   MOVE    SPA-T01-1001-TSTYUKYMD (IDX) (1:6)
                                           TO  SPA-NAI-T06-SRYYMKBN-ST
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
                   MOVE    SPA-T01-1001-TSTYUKYM-G (IDX) 
                                           TO
                                           SPA-NAI-T06-SRYYMKBN-ST-PRT
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
                   MOVE    SPA-T01-1001-TSTYUKYM (IDX) 
                                           TO  WRK-SRYYMKBN-ST
      *
                   MOVE    SPA-T01-1001-TEDYUKYMD (IDX) (1:6)
                                           TO  SPA-NAI-T06-SRYYMKBN-ED
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
                   MOVE    SPA-T01-1001-TEDYUKYM-G (IDX)
                                           TO
                                           SPA-NAI-T06-SRYYMKBN-ED-PRT
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
                   MOVE    SPA-T01-1001-TEDYUKYM (IDX)
                                           TO  WRK-SRYYMKBN-ED
                   MOVE    "-"             TO  WRK-SRYYMKBN-FL
                   IF      SPA-T01-1001-THOSPCD  (IDX)
                                           =   9999999
                       MOVE    "*"             TO  WRK-SRYYMKBN-ERR
                       MOVE    "1"             TO
                                           SPA-NAI-T06-SRYYMKBN-ERR
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
                   END-IF
                   MOVE    WRK-SRYYMKBN    TO  SPA-GMN-T06-SRYYMKBNNML
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
      *
                   MOVE    SPA-T01-1001-THOSPCD (IDX)
                                           TO
                                           SPA-NAI-T06-SRYYMKBN-HOSPCD
                                              (SPA-GMN-T06-SRYYMKBN-MAX)
               END-IF
           END-PERFORM
           IF      SPA-GMN-T06-SRYYMKBN-MAX    >   1
               MOVE    SPA-GMN-T06-SRYYMKBNLST (1)
                                       TO  SPA-GMN-T06-SRYYMKBN-G
               MOVE    1               TO  SPA-NAI-T06-SRYYMKBN-SELNUM
               MOVE    SPA-NAI-T06-SRYYMKBN-HOSPCD
                                           (SPA-NAI-T06-SRYYMKBN-SELNUM)
                                       TO  SPA-NAI-T06-HOSPCD
           ELSE
               MOVE    ZERO            TO  SPA-GMN-T06-SRYYMKBN-MAX
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                   INITIALIZE      SPA-GMN-T06-SRYYMKBNLST (IDX)
               END-PERFORM
           END-IF
      *??
           DISPLAY "SPA-NAI-T06-HOSPCD=" SPA-NAI-T06-HOSPCD "#"
      *
           .
       3105-SRYYMKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理結果画面（Ｔ９９）戻り処理
      *****************************************************************
       320-T99-SET-SEC            SECTION.
      *
           IF      SPA-T01-SYORIFLG    =   ZERO    OR  9
               MOVE    1                   TO  SPA-NAI-T06-SYORIFLG
           ELSE
               MOVE    ZERO                TO  SPA-NAI-T06-SYORIFLG
           END-IF
      *
           IF      SPA-T99-FLG     =   "OK"
               PERFORM 3203-NEXT-GMN-SEC
               GO  TO  320-T99-SET-EXT
           END-IF
      *
           MOVE    1               TO  SPA-GMN-T06-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU-H
           .
       320-T99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電処理後の印刷処理
      *****************************************************************
       3202-PRINT-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *      
           MOVE    SPA-LNK-PRTKANRI-AREA
                                   TO  LNK-PRTKANRI-AREA
      *
           PERFORM 45013-PRINT-SYORI-SEC
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
           MOVE    9               TO  SPA-T01-SYORIFLG
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
      *
           .
       3202-PRINT-EXT.
           EXIT. 
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       3203-NEXT-GMN-SEC            SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T06             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T06"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
                                           LNK-XC01-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-T01-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    0                   TO  LNK-XC01-ST-PAGE
           MOVE    0                   TO  LNK-XC01-ED-PAGE
           MOVE    "T06"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       3203-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面戻り処理
      *****************************************************************
       330-TID1-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "1005"
                   WHEN    "1008"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1003"
                       PERFORM 460-CLEAR-SEC
                   WHEN    "1006"
      *                返戻年月未入力のときの返戻データ存在チェック
                       IF      SPA-NAI-T09-THENREIYM (1)
                                           =   "1" OR  "2"
                           EVALUATE    SPA-NAI-T09-THENREIYM (1)
                               WHEN    "1"
                                   MOVE    "1011"      TO  SPA-TIDCD
                               WHEN    "2"
                                   MOVE    "1012"      TO  SPA-TIDCD
                           END-EVALUATE
                           MOVE    SPACE           TO  SPA-TID1-FLG
                           GO  TO  330-TID1-SET-EXT
                       ELSE 
      *                    追加処理
                           PERFORM 410411-TUIKA-HENSYU-SEC
      *
                           IF      SPA-GMN-T06-SELNUM  =   ZERO
                               MOVE    SPA-GMN-T06-RECE-MAX
                                               TO  T06-RECE-LIST-ROW
                           ELSE
                               MOVE    SPA-GMN-T06-SELNUM
                                               TO  T06-RECE-LIST-ROW
                           END-IF
                           INITIALIZE          SPA-GMN-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-BODY-SELNUM
                           INITIALIZE          SPA-T09-AREA
                           MOVE    4           TO  SPA-GMN-T06-CUR
                       END-IF
                   WHEN    "1011"
                   WHEN    "1012"
      *                追加処理
                       PERFORM 410411-TUIKA-HENSYU-SEC
      *
                       IF      SPA-GMN-T06-SELNUM  =   ZERO
                           MOVE    SPA-GMN-T06-RECE-MAX
                                                   TO  T06-RECE-LIST-ROW
                       ELSE
                           MOVE    SPA-GMN-T06-SELNUM
                                                   TO  T06-RECE-LIST-ROW
                       END-IF
                       INITIALIZE              SPA-GMN-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-BODY-SELNUM
                       INITIALIZE              SPA-T09-AREA
                       MOVE    4               TO  SPA-GMN-T06-CUR  
                   WHEN    "1007"
      *                削除処理
                       PERFORM 440-SAKUJYO-SEC
                   WHEN    "1009"
                       EVALUATE    SPA-T01-WIDGET
                           WHEN    "TEISYUTUSAKI1"
                               MOVE    "T" TO  SPA-GMN-T06-TEISYUTUSAKI1
                               MOVE    "F" TO  SPA-GMN-T06-TEISYUTUSAKI2
                                               SPA-GMN-T06-TEISYUTUSAKI4
                           WHEN    "TEISYUTUSAKI2"
                               MOVE    "F" TO  SPA-GMN-T06-TEISYUTUSAKI1
                                               SPA-GMN-T06-TEISYUTUSAKI4
                               MOVE    "T" TO  SPA-GMN-T06-TEISYUTUSAKI2
                           WHEN    "TEISYUTUSAKI4"
                               MOVE    "F" TO  SPA-GMN-T06-TEISYUTUSAKI1
                                               SPA-GMN-T06-TEISYUTUSAKI2
                               MOVE    "T" TO  SPA-GMN-T06-TEISYUTUSAKI4
                           WHEN    "SRYYM_H"
                               IF      SPA-NAI-T06-SYSYM-2
                                               >=  SPA-NAI-T06-SRYYM-H
                                   MOVE    "2001"      TO  SPA-ERRCD
                               END-IF
                       END-EVALUATE
      *
                       IF      SPA-NAI-T06-SRYYM-H <   201307
                           IF      SPA-GMN-T06-TEISYUTUSAKI4
                                                       =   "T"
                               MOVE    "T"                 TO
                                           SPA-GMN-T06-TEISYUTUSAKI1
                           END-IF 
                           MOVE    WIDGET-INSENSITIVE  TO
                                       SPA-GMN-T06-TEISYUTUSAKI4-STATE
                           MOVE    "F"                 TO
                                       SPA-GMN-T06-TEISYUTUSAKI4
                       ELSE
                           MOVE    WIDGET-NORMAL       TO
                                       SPA-GMN-T06-TEISYUTUSAKI4-STATE
                       END-IF
      *
      *                診療年月期間設定処理
                       PERFORM 3105-SRYYMKBN-HENSYU-SEC
      *
      *                管理情報編集処理
                       PERFORM 3101-ONRECE-KANRI-HENSYU-SEC
      *
      *                点検レセ０点公費記載の請求管理のチェック処理
                       PERFORM 3104-SEIKYU-CHECK-SEC
      *
                       MOVE    2               TO  SPA-GMN-T06-CUR
                   WHEN    "1019"
      *                管理情報編集処理
                       PERFORM 3101-ONRECE-KANRI-HENSYU-SEC
                       MOVE    2               TO  SPA-GMN-T06-CUR
                   WHEN    "1004"
                   WHEN    "1010"
                       PERFORM 480-ZENKAI-HYOUJI-SEC
               END-EVALUATE
           ELSE        
               EVALUATE    SPA-TIDCD
                   WHEN    "1009"
                       EVALUATE    SPA-T01-WIDGET
                           WHEN    "SRYYM_H"
                               MOVE    SPA-NAI-T06-SRYYM-OLD
                                               TO  SPA-NAI-T06-SRYYM-H
                               MOVE    SPA-GMN-T06-SRYYM-OLD
                                               TO  SPA-GMN-T06-SRYYM-H
                               MOVE    2       TO  SPA-GMN-T06-CUR
                           WHEN    "SRYYMKBN"
                               MOVE    SPA-GMN-T06-SRYYMKBN-OLD
                                               TO  SPA-GMN-T06-SRYYMKBN
                       END-EVALUATE
                   WHEN    "1019"
                       MOVE    SPA-GMN-T06-SRYYMKBN-G-OLD
                                               TO
                                           SPA-GMN-T06-SRYYMKBN-G
                       MOVE    SPA-NAI-T06-SRYYMKBN-SELNUM-OLD
                                               TO
                                           SPA-NAI-T06-SRYYMKBN-SELNUM
                       MOVE    SPA-NAI-T06-SRYYMKBN-HOSPCD
                                           (SPA-NAI-T06-SRYYMKBN-SELNUM)
                                           TO  SPA-NAI-T06-HOSPCD
                               MOVE    2       TO  SPA-GMN-T06-CUR
                   WHEN    "1006"
                   WHEN    "1011"
                   WHEN    "1012"
                       IF      SPA-NAI-T06-BODY-SELNUM  >   ZERO
                           INITIALIZE          SPA-GMN-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-BODY-SELNUM
                                               SPA-T09-AREA
                       END-IF
                       MOVE    4                TO  SPA-GMN-T06-CUR  
               END-EVALUATE
           END-IF
      *
           INITIALIZE                  SPA-T01KYOUTU-H
      *
           INITIALIZE                  SPA-NAI-T06-SRYYM-OLD
                                       SPA-GMN-T06-SRYYM-OLD
           INITIALIZE                  SPA-GMN-T06-SRYYMKBN-G-OLD
                                       SPA-NAI-T06-SRYYMKBN-SELNUM-OLD
           INITIALIZE                  SPA-T09-AREA
      *
           .
       330-TID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＴＩＤ２）ＯＫ処理
      *****************************************************************
       340-TID2-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "1002"
      *                作成日のチェック
                       IF      SPA-NAI-T09-TCREYMD (1)
                                           <   SPA-NAI-T09-TUKEYMD (1)      
                           MOVE    "1006"              TO  SPA-TIDCD
                           MOVE    SPACE               TO  SPA-TID1-FLG
                           GO  TO  340-TID2-SET-EXT
                       ELSE
      *                   返戻年月未入力のときの返戻データ存在チェック
                          IF      SPA-NAI-T09-THENREIYM (1)
                                               =   "1" OR  "2"
                               EVALUATE    SPA-NAI-T09-THENREIYM (1)
                                   WHEN    "1"
                                       MOVE    "1011"      TO  SPA-TIDCD
                                   WHEN    "2"
                                       MOVE    "1012"      TO  SPA-TIDCD
                               END-EVALUATE
                               MOVE    SPACE           TO  SPA-TID1-FLG
                               GO  TO  340-TID2-SET-EXT
                           ELSE 
      *                        追加処理
                               PERFORM 410411-TUIKA-HENSYU-SEC
      *
                               IF      SPA-GMN-T06-SELNUM  =   ZERO
                                   MOVE    SPA-GMN-T06-RECE-MAX
                                                   TO  T06-RECE-LIST-ROW
                               ELSE
                                   MOVE    SPA-GMN-T06-SELNUM
                                                   TO  T06-RECE-LIST-ROW
                               END-IF
                               INITIALIZE      SPA-GMN-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-BODY-SELNUM
                               INITIALIZE              SPA-T09-AREA
                               MOVE    4            TO  SPA-GMN-T06-CUR
                           END-IF
                       END-IF            
               END-EVALUATE 
           ELSE
               IF      SPA-TID1-FLG    =   "NO"
                   EVALUATE    SPA-TIDCD
                       WHEN    "1002"
      *                    個人履歴表示
                           MOVE    "1"                TO  SPA-T07-FLG
                           PERFORM 400-T07-SYORI-SEC
                           MOVE    SPACE              TO  SPA-TID1-FLG
                           GO  TO  340-TID2-SET-EXT
                   END-EVALUATE 
               END-IF
           END-IF
      *
           IF      SPA-NAI-T06-BODY-SELNUM  >   ZERO
               INITIALIZE              SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-BODY-SELNUM
                                       SPA-T09-AREA
           END-IF
      *
           INITIALIZE                  SPA-T01KYOUTU-H
      *
           MOVE    4               TO  SPA-GMN-T06-CUR
           .
       340-TID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電対象患者一覧戻り処理
      *****************************************************************
       340-T09-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
      *        追加処理
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       5
                   IF      SPA-GMN-T09-SELNUM (IDZ)    >   ZERO
                       MOVE    SPA-GMN-T09-SELNUM (IDZ)
                                               TO  IDY
      *
      *
                       INITIALIZE                  RECE10-REC
                       MOVE    SPA-NAI-T09-RECE10-KEY   (IDY)
                                               TO  RECE10-KEY
                       IF      RECE10-TEISYUTUSAKI =   "3"
                           MOVE    SPA-NAI-T09-RECE10-TKIGO (IDY)
                                                   TO  RECE10-KOFUNUM
                       ELSE
                           MOVE    SPA-NAI-T09-RECE10-TKIGO (IDY)
                                                   TO  RECE10-KIGO
                           MOVE    SPA-NAI-T09-RECE10-TNUM  (IDY)
                                                   TO  RECE10-NUM
                       END-IF
                       MOVE    SPA-NAI-T09-RECE10-THKNNUM  (IDY)
                                               TO  RECE10-HKNNUM
                       MOVE    SPA-NAI-T09-RECE10-THENREI-SIJI(IDY)
                                               TO
                                               RECE10-HENREI-SIJI-FLG
                       IF      RECE10-TEISYUTUSAKI =   "3"
                           MOVE    SPA-NAI-T09-RECE10-TSYOKAIFLG  (IDY)
                                               TO  RECE10-SYOKAIFLG
                           MOVE    SPA-NAI-T09-RECE10-THKNJANUM-S (IDY)
                                               TO  RECE10-HKNJANUM-S
                       END-IF
                       MOVE    SPA-NAI-T09-TCREYMD  (IDY)
                                               TO  RECE10-CREYMD
                       MOVE    SPA-GMN-T09-TPTNUM  (IDY)
                                               TO  RECE10-PTNUM
                       MOVE    SPA-NAI-T09-TDISASTER  (IDY)
                                               TO  RECE10-ETC-YKZFTN (1)
                       PERFORM 410411-TUIKA-HENSYU-SEC
      *
                       MOVE    SPA-GMN-T06-RECE-MAX
                                               TO  T06-RECE-LIST-ROW
                   END-IF
               END-PERFORM
      *
               INITIALIZE              SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-BODY-SELNUM
                                       SPA-T09-AREA
           END-IF
      *
           IF      SPA-NAI-T06-BODY-SELNUM
                                       >   ZERO
               INITIALIZE              SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-BODY-SELNUM
                                       SPA-T09-AREA
           END-IF
           MOVE    SPACE           TO  SPA-TID1-FLG
           MOVE    SPACE           TO  SPA-TIDCD
           MOVE    4               TO  SPA-GMN-T06-CUR      
      *
           .
       340-T09-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電作成指示画面戻り処理
      *****************************************************************
       350-T98-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"    OR  "NO"    OR  "PV"
               MOVE    "T06"                  TO  SPA-T99-MOTOPG
      *        ジョブ管理チェック処理
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    SPA-TID1-FLG    TO  SPA-RECEDEN-FLG 
                   PERFORM 460-RECEDEN-SEC
                   GO  TO  350-T98-SET-EXT
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-T01KYOUTU-H
           MOVE    4               TO  SPA-GMN-T06-CUR      
      *
           .
       350-T98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 460-CLEAR-TID-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-SAKUJYO-GID-SEC
      *     再印刷
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-REPRINT-SEC
      *    前回作成分表示
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-ZENKAI-HYOUJI-TID-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *    レセ電作成
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 400-RECEDEN-SYORI-SEC
      *    個人履歴表示
               WHEN    "CLICKED"       ALSO    "B17"
                   MOVE    SPACE               TO  SPA-T07-FLG
                   PERFORM 400-T07-SYORI-SEC
      *    作成履歴表示
               WHEN    "CLICKED"       ALSO    "B18"
                   PERFORM 400-T08-SYORI-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B24"
                   MOVE    "T06"               TO  SPA-T99-MOTOPG
                   PERFORM 490-KAKUNIN-SEC
      *    提出先設定
               WHEN    "CLICKED"       ALSO    "TEISYUTUSAKI1"
               WHEN    "CLICKED"       ALSO    "TEISYUTUSAKI2"
               WHEN    "CLICKED"       ALSO    "TEISYUTUSAKI3"
               WHEN    "CLICKED"       ALSO    "TEISYUTUSAKI4"
                   PERFORM 400-TEISYUTUSAKI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    提出先設定処理
      *****************************************************************
       400-TEISYUTUSAKI-SEC               SECTION.
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO  TO  400-TEISYUTUSAKI-EXT
           END-IF
           EVALUATE    MCP-WIDGET
               WHEN    "TEISYUTUSAKI1"
                   IF      T06-TEISYUTUSAKI1   =   "F"
                       IF      SPA-GMN-T06-TEISYUTUSAKI1
                                                   =   "T"
                           MOVE    "F"     TO  SPA-GMN-T06-TEISYUTUSAKI1
                           GO  TO  400-TEISYUTUSAKI-EXT
                       END-IF
                   ELSE
                       IF      SPA-GMN-T06-TEISYUTUSAKI-G
                                                   =   "FFFF"
                           CONTINUE
                       ELSE
                           IF      SPA-GMN-T06-TEISYUTUSAKI1   =   "T"
                               GO  TO  400-TEISYUTUSAKI-EXT
                           END-IF
                       END-IF
                   END-IF
               WHEN    "TEISYUTUSAKI2"
                   IF      T06-TEISYUTUSAKI2   =   "F"
                       IF      SPA-GMN-T06-TEISYUTUSAKI2
                                                   =   "T"
                           MOVE    "F"     TO  SPA-GMN-T06-TEISYUTUSAKI2
                           GO  TO  400-TEISYUTUSAKI-EXT
                       END-IF
                   ELSE
                       IF      SPA-GMN-T06-TEISYUTUSAKI-G
                                                   =   "FFFF"
                           CONTINUE
                       ELSE
                           IF      SPA-GMN-T06-TEISYUTUSAKI2   =   "T"
                               GO  TO  400-TEISYUTUSAKI-EXT
                           END-IF
                       END-IF
                   END-IF
               WHEN    "TEISYUTUSAKI3"
                   IF      T06-TEISYUTUSAKI3   =   "F"
                       IF      SPA-GMN-T06-TEISYUTUSAKI3
                                                   =   "T"
                           MOVE    "F"     TO  SPA-GMN-T06-TEISYUTUSAKI3
                           GO  TO  400-TEISYUTUSAKI-EXT
                       END-IF
                   ELSE
                       IF      SPA-GMN-T06-TEISYUTUSAKI-G
                                                   =   "FFFF"
                           CONTINUE
                       ELSE
                           IF      SPA-GMN-T06-TEISYUTUSAKI3   =   "T"
                               GO  TO  400-TEISYUTUSAKI-EXT
                           END-IF
                       END-IF
                   END-IF
               WHEN    "TEISYUTUSAKI4"
                   IF      T06-TEISYUTUSAKI4   =   "F"
                       IF      SPA-GMN-T06-TEISYUTUSAKI4
                                                   =   "T"
                           MOVE    "F"     TO  SPA-GMN-T06-TEISYUTUSAKI4
                           GO  TO  400-TEISYUTUSAKI-EXT
                       END-IF
                   ELSE
                       IF      SPA-GMN-T06-TEISYUTUSAKI-G
                                                   =   "FFFF"
                           CONTINUE
                       ELSE
                           IF      SPA-GMN-T06-TEISYUTUSAKI4   =   "T"
                               GO  TO  400-TEISYUTUSAKI-EXT
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    未処理分があるときのチェック 
           IF    ( SPA-NAI-T06-SYORIFLG
                                       =   ZERO )
           AND   ( SPA-GMN-T06-RECE-MAX
                                       >   ZERO )
               MOVE    "1009"          TO  SPA-TIDCD
               MOVE    MCP-WIDGET      TO  SPA-T01-WIDGET
               GO  TO  400-TEISYUTUSAKI-EXT
           END-IF 
      *
           EVALUATE    MCP-WIDGET
               WHEN    "TEISYUTUSAKI1"
                   MOVE    T06-TEISYUTUSAKI1
                                       TO  SPA-GMN-T06-TEISYUTUSAKI1
               WHEN    "TEISYUTUSAKI2"
                    MOVE    T06-TEISYUTUSAKI2
                                       TO  SPA-GMN-T06-TEISYUTUSAKI2
               WHEN    "TEISYUTUSAKI4"
                    MOVE    T06-TEISYUTUSAKI4
                                       TO  SPA-GMN-T06-TEISYUTUSAKI4
           END-EVALUATE
      *    管理情報編集処理
           PERFORM 3101-ONRECE-KANRI-HENSYU-SEC
      *
           .
       400-TEISYUTUSAKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SRYYM_H"
                   PERFORM 410-SRYYM-H-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYYMKBN"
                   PERFORM 4106-SRYYMKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 430-SELNUM-CHK-SEC
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
               WHEN    ANY             ALSO    "KANRI_LIST"
                   PERFORM 450-KANRILST-SELECT-SEC
               WHEN    ANY             ALSO    "BODY_LIST"
                   PERFORM 460-BODYLST-SELECT-SEC
               WHEN    ANY             ALSO    "RECE_LIST"
                   PERFORM 470-RECELST-SELECT-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       410-SRYYM-H-CHK-SEC           SECTION.
      *
           MOVE    1               TO  SPA-GMN-T06-CUR
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO  TO  410-SRYYM-H-CHK-EXT
           END-IF
      *  
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  SPA-NAI-T06-SRYYM-OLD
           MOVE    SPA-GMN-T06-SRYYM-H
                                   TO  SPA-GMN-T06-SRYYM-OLD
           MOVE    ZERO            TO  SPA-NAI-T06-SRYYM-H
      *
           MOVE    T06-SRYYM-H     TO  SPA-GMN-T06-SRYYM-H
      *
           IF      SPA-GMN-T06-SRYYM-H
                                   =   SPACE
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE 
               INITIALIZE                  ORCSGDAYAREA
               MOVE    "1"             TO  SGDAY-INTYPE
               MOVE    SPA-GMN-T06-SRYYM-H
                                       TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC        =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-T06-SRYYM-H
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-T06-SRYYM-H
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
      *        医療機関ＩＤ編集
               INITIALIZE                  SYS-1001-REC
               MOVE    SPA-HOSPNUM        TO  SYS-1001-HOSPNUM
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    SPA-NAI-T06-SRYYM-H
                                           TO  SYS-1001-STYUKYMD (1:6)
               MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
               MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
               ELSE
                   INITIALIZE                      SYS-1001-REC
               END-IF
               MOVE    SYS-1001-HOSPCD     TO  SPA-NAI-T06-HOSPCD
      *
               IF      SPA-NAI-T06-SRYYM-H
                                   NOT =   SPA-NAI-T06-SRYYM-OLD
      *            未処理分があるときのチェック 
                   IF    ( SPA-NAI-T06-SYORIFLG
                                           =   ZERO )
                   AND   ( SPA-GMN-T06-RECE-MAX
                                           >   ZERO )
                       MOVE    "1009"          TO  SPA-TIDCD
                       MOVE    "SRYYM_H"       TO  SPA-T01-WIDGET
                       GO  TO  410-SRYYM-H-CHK-EXT
                   END-IF
      *
                   IF      SPA-NAI-T06-SRYYM-H <   201307
                       IF      SPA-GMN-T06-TEISYUTUSAKI4
                                                   =   "T"
                           MOVE    "T"                 TO
                                           SPA-GMN-T06-TEISYUTUSAKI1
                       END-IF 
                       MOVE    WIDGET-INSENSITIVE  TO
                                       SPA-GMN-T06-TEISYUTUSAKI4-STATE
                       MOVE    "F"                 TO
                                       SPA-GMN-T06-TEISYUTUSAKI4
                   ELSE
                       MOVE    WIDGET-NORMAL       TO
                                       SPA-GMN-T06-TEISYUTUSAKI4-STATE
                   END-IF
      *
      *            管理情報編集処理
                   PERFORM 3101-ONRECE-KANRI-HENSYU-SEC
      *
      *            点検レセ０点公費記載の請求管理のチェック処理
                   PERFORM 3104-SEIKYU-CHECK-SEC
               END-IF
           ELSE
               MOVE    SPA-NAI-T06-SRYYM-OLD   TO  SPA-NAI-T06-SRYYM-H
               MOVE    SPA-GMN-T06-SRYYM-OLD   TO  SPA-GMN-T06-SRYYM-H
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
                IF      SPA-NAI-T06-SRYYM-H
                                   NOT =   SPA-NAI-T06-SRYYM-OLD
      *            診療年月期間設定処理
                   PERFORM 3105-SRYYMKBN-HENSYU-SEC
      *
                   MOVE    SPACE               TO  SPA-GMN-T06-TITLE3
      *            月途中変更の診療年月かチェック
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-T06-SRYYMKBN-MAX
                       IF      ( SPA-NAI-T06-SRYYMKBN-ST (IDX)
                                           <=   SPA-NAI-T06-SRYYM-H )
                       AND     ( SPA-NAI-T06-SRYYMKBN-ED (IDX)
                                           >=   SPA-NAI-T06-SRYYM-H )
                           IF      SPA-NAI-T06-SRYYMKBN-ERR (IDX)
                                               =   "1"
                               MOVE    ZERO        TO
                                               SPA-GMN-T06-SRYYMKBN-MAX
                               PERFORM VARYING IDX FROM    1   BY  1
                                       UNTIL   IDX >       10
                                   INITIALIZE
                                       SPA-GMN-T06-SRYYMKBNLST (IDX)
                               END-PERFORM
                               MOVE    "0051"      TO  SPA-ERRCD
                               MOVE
                   "月途中に医療機関コードが変更されている診療年月です"
                                               TO  SPA-GMN-T06-TITLE3
                           END-IF
                           MOVE    SPA-GMN-T06-SRYYMKBN-MAX
                                                   TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE   OR  "0051"
               INITIALIZE                  SPA-NAI-T06-SRYYM-OLD
                                           SPA-GMN-T06-SRYYM-OLD
               IF      SPA-GMN-T06-SRYYMKBN-MAX    >   ZERO
                   MOVE    7               TO  SPA-GMN-T06-CUR
               ELSE
                   MOVE    2               TO  SPA-GMN-T06-CUR
               END-IF
               IF      SPA-T01-PRTKBN  =   "6" OR  "7"
                   CONTINUE
               ELSE
                   IF      SPA-NAI-T06-SYSYM-2 >=  SPA-NAI-T06-SRYYM-H
                       MOVE    "2001"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       410-SRYYM-H-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       430-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    3                   TO  SPA-GMN-T06-CUR
      *
           INITIALIZE                      SPA-GMN-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-RECE-KOUMOKU
           MOVE    ZERO                TO  SPA-NAI-T06-SELNUM
      *
           MOVE    T06-SELNUM          TO  SPA-GMN-T06-SELNUM
      *
           IF      SPA-GMN-T06-SELNUM  =   ZERO
               IF      SPA-GMN-T06-RECE-MAX =   WRK-CONS-LINE-MAX
                   MOVE    "0002"              TO  SPA-ERRCD
               ELSE
                   MOVE    4                   TO  SPA-GMN-T06-CUR 
                   GO  TO  430-SELNUM-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN-T06-SELNUM  >   SPA-GMN-T06-RECE-MAX
               MOVE    "0023"          TO  SPA-ERRCD
           END-IF
               
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-T06-SELNUM  TO  SPA-NAI-T06-SELNUM
                                               T06-RECE-LIST-ROW
      *        選択項目編集処理
               PERFORM 4301-RENNUM-HENSYU-SEC
           END-IF
      *
           .
       430-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択項目編集処理
      *****************************************************************
       4301-RENNUM-HENSYU-SEC            SECTION.
      *         
           MOVE    SPA-GMN-T06-R-TPTNUM   (SPA-GMN-T06-SELNUM)
                                       TO  SPA-GMN-T06-PTNUM
           MOVE    SPA-NAI-T06-RECE10-PTID (SPA-GMN-T06-SELNUM)
                                       TO  SPA-NAI-T06-PTID
           MOVE    SPA-GMN-T06-R-TNAME      (SPA-GMN-T06-SELNUM)
                                       TO  SPA-GMN-T06-NAME
           MOVE    SPA-GMN-T06-R-TSRYYM    (SPA-GMN-T06-SELNUM)
                                       TO  SPA-GMN-T06-SRYYM
           MOVE    SPA-NAI-T06-RECE10-SRYYM   (SPA-GMN-T06-SELNUM)
                                       TO  SPA-NAI-T06-SRYYM
           MOVE    SPA-NAI-T06-RECE10-NYUGAIKBN (SPA-GMN-T06-SELNUM)
                                       TO  SPA-GMN-T06-NYUGAIKBN
      *
           MOVE    4                   TO  SPA-GMN-T06-CUR 
      *
           .
       4301-RENNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理情報一覧選択処理
      *****************************************************************
       450-KANRILST-SELECT-SEC       SECTION.
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO  TO  450-KANRILST-SELECT-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T06-KANRI-MAX
               IF      T06-KANRI-LIST-SELECT (IDX)
                                       =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    SPA-GMN-T06-KANRI-MAX
                                           TO  IDX
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-T06-KANRI-SELNUM
                                               T06-KANRI-LIST-ROW
      *        明細情報編集処理
               PERFORM 3102-ONRECE-BODY-HENSYU-SEC
           END-IF
           .
       450-KANRILST-SELECT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    明細情報選択処理
      *****************************************************************
       460-BODYLST-SELECT-SEC       SECTION.
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO  TO  460-BODYLST-SELECT-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T06-BODY-MAX
               IF      T06-BODY-LIST-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-T06-BODY-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNO
                       MOVE    SPA-GMN-T06-BODY-MAX
                                               TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-T06-BODY-SELNUM
                                               T06-BODY-LIST-ROW
      *
      *        選択項目編集処理
               PERFORM 4601-BODYLST-HENSYU-SEC
           END-IF
           .
       460-BODYLST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択項目編集処理
      *****************************************************************
       4601-BODYLST-HENSYU-SEC            SECTION.
      *         
           MOVE    SPA-GMN-T06-B-TPTNUM   (SPA-NAI-T06-BODY-SELNUM)
                                       TO  SPA-GMN-T06-PTNUM
           MOVE    SPA-NAI-T06-B-TPTID (SPA-NAI-T06-BODY-SELNUM)
                                       TO  SPA-NAI-T06-PTID
           MOVE    SPA-GMN-T06-B-TNAME      (SPA-NAI-T06-BODY-SELNUM)
                                       TO  SPA-GMN-T06-NAME
           MOVE    SPA-GMN-T06-B-TSRYYM    (SPA-NAI-T06-BODY-SELNUM)
                                       TO  SPA-GMN-T06-SRYYM
           MOVE    SPA-NAI-T06-B-TSRYYM   (SPA-NAI-T06-BODY-SELNUM)
                                       TO  SPA-NAI-T06-SRYYM
           MOVE    SPA-NAI-T06-B-TNYUGAIKBN (SPA-NAI-T06-BODY-SELNUM)
                                       TO  SPA-GMN-T06-NYUGAIKBN
           MOVE    SPA-NAI-T06-B-THENREI-FLG (SPA-NAI-T06-BODY-SELNUM)
                                       TO  SPA-NAI-T06-HENREI-FLG
      *
      *    患者番号が存在するとき追加処理
           IF      SPA-GMN-T06-B-TPTNUM   (SPA-NAI-T06-BODY-SELNUM)
                                   NOT =   SPACE
      *        患者マスター存在チェック
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-T06-B-TPTID (SPA-NAI-T06-BODY-SELNUM)
                                           TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   PERFORM 41041-TUIKA-SEC
               END-IF
               IF      SPA-TIDCD        =   SPACE
                   INITIALIZE              SPA-GMN-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-RECE-KOUMOKU
                   MOVE    ZERO        TO  SPA-NAI-T06-BODY-SELNUM
               END-IF
               MOVE    4               TO  SPA-GMN-T06-CUR
               GO  TO  4601-BODYLST-HENSYU-EXT
           END-IF
      *
      *    患者番号不明のとき氏名から検索
           MOVE    ZERO                TO  IDX
                                           IDY
                                           IDZ
                                           IDW
           INITIALIZE                      WRK-RECE10-REC
                                           SPA-T09-AREA
      *
      *    追加できるか
           IF      SPA-GMN-T06-RECE-MAX =   WRK-CONS-LINE-MAX
               MOVE    "0002"              TO  SPA-ERRCD
               INITIALIZE                      SPA-GMN-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-RECE-KOUMOKU
               MOVE    ZERO                TO  SPA-NAI-T06-BODY-SELNUM
               GO  TO  4601-BODYLST-HENSYU-EXT
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-T06-B-TNAME     (SPA-NAI-T06-BODY-SELNUM)
                                       TO  PTINF-NAME
           MOVE    SPA-NAI-T06-B-TBIRTHDAY (SPA-NAI-T06-BODY-SELNUM)
                                       TO  PTINF-BIRTHDAY
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key23"             TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           PERFORM         UNTIL       FLG-PTINF     =   1
      *        請求管理とのチェック処理
               INITIALIZE                      RECE10-REC
               MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
               MOVE    PTINF-PTID          TO  RECE10-PTID
               MOVE    SPA-NAI-T06-SRYYM   TO  RECE10-SRYYM
               MOVE    SPA-NAI-T06-SRYYM-H TO  RECE10-SKYYM
               IF      SPA-GMN-T06-TEISYUTUSAKI1
                                           =   "T"
                   MOVE    1                   TO  RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI
               ELSE
                   IF      SPA-GMN-T06-TEISYUTUSAKI2
                                               =   "T"
                       MOVE    2               TO  RECE10-TEISYUTUSAKI
                       MOVE    6               TO  RECE10-XXTEISYUTUSAKI
                   ELSE
                       MOVE    3               TO  RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI
                   END-IF
               END-IF
               EVALUATE    SPA-GMN-T06-NYUGAIKBN
                   WHEN    "1"
                   WHEN    "2"
                       MOVE    SPA-GMN-T06-NYUGAIKBN
                                               TO  RECE10-NYUGAIKBN
                   WHEN    OTHER
                      MOVE    "%"              TO  RECE10-NYUGAIKBN
               END-EVALUATE
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               IF      SPA-NAI-T06-SRYYM   =   SPA-NAI-T06-SRYYM-H
                   MOVE    "key70"             TO  WRK-MCP-PATHNAME
               ELSE
                   MOVE    "key71"             TO  WRK-MCP-PATHNAME
               END-IF
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-SEIKYU-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
      *
               PERFORM         UNTIL       FLG-SEIKYU     =   1
                               OR          IDX            >=  20
                   MOVE    MCPDATA-REC         TO  RECE10-REC
      *            災害対応（未確定分をレセ電データに記録するか）
                   IF    (  SPA-T06-2005-RECEDEN-DISASTER
                                               =   "0"              )
                   AND   ( RECE10-ETC-YKZFTN (1)
                                               =   6   OR  7  OR  8 )
                       ADD     1                   TO  IDW
                   ELSE
      *              月遅れ分、請求しないは対象外
                     IF    ( RECE10-SKYKBN   NOT =   "0"             )
                     AND   ( RECE10-SKYYM        =   "000000"
                                           OR  SPA-NAI-T06-SRYYM-H )
                       EVALUATE    SPA-NAI-T06-B-THENREI-FLG
                                           (SPA-NAI-T06-BODY-SELNUM)
                           WHEN    SPACE
      *                        返戻分は対象外
                               IF      RECE10-HENREIYM =   "000000"
                                   ADD     1               TO  IDX
                                   PERFORM 42031-T09-HENSYU-SEC
                                   IF      IDX             =   1    
                                       MOVE    RECE10-REC  TO
                                                       WRK-RECE10-REC
                                   END-IF
                               ELSE
                                   ADD     1               TO  IDY
                               END-IF
                           WHEN    "1"
      *                        返戻分設定済のみ対象
                               IF    ( RECE10-HENREIYM NOT =  "000000" )
                               AND   ( RECE10-HENREI-SIJI-FLG
                                                           =  1        )
                                   ADD     1               TO  IDX
                                   PERFORM 42031-T09-HENSYU-SEC
                                   IF      IDX             =   1    
                                       MOVE    RECE10-REC  TO
                                                       WRK-RECE10-REC
                                   END-IF
                               ELSE
                                   ADD     1               TO  IDY
                               END-IF
                       END-EVALUATE
                     ELSE
                       ADD     1               TO  IDZ
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-SEIKYU-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key23"             TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           EVALUATE    IDX
               WHEN    ZERO
                   IF    ( IDY             =   0 )
                   AND   ( IDZ             =   0 )
                   AND   ( IDW             =   0 )
                       MOVE    "0007"          TO  SPA-ERRCD
                   ELSE
                       IF      SPA-NAI-T06-B-THENREI-FLG
                                           (SPA-NAI-T06-BODY-SELNUM)
                                       =   "1"
                           MOVE    "0017"          TO  SPA-ERRCD
                       ELSE
                           IF    ( IDY             >   0 )
                           AND   ( IDZ             >   0 )
                           AND   ( IDW             >   0 )
                               MOVE    "0015"          TO  SPA-ERRCD
                           ELSE
                               IF    ( IDY             >   0 )
                                   MOVE    "0014"          TO  SPA-ERRCD
                               ELSE
                                 IF    ( IDZ             >   0 )
                                   MOVE    "0016"          TO  SPA-ERRCD
                                 ELSE
                                   MOVE    "0019"          TO  SPA-ERRCD
                                 END-IF
                               END-IF 
                           END-IF 
                       END-IF 
                   END-IF 
               WHEN    1
                   MOVE    WRK-RECE10-REC  TO  RECE10-REC
                   PERFORM 42031-SEIKYU-CHK-SEC
                   IF      SPA-TIDCD   NOT =   SPACE
                       GO  TO  4601-BODYLST-HENSYU-EXT
                   END-IF
                   IF      SPA-ERRCD   NOT =   SPACE
                       INITIALIZE              SPA-GMN-T06-RECE-KOUMOKU
                                               SPA-NAI-T06-RECE-KOUMOKU
                       MOVE    ZERO        TO  SPA-NAI-T06-BODY-SELNUM
                       GO  TO  4601-BODYLST-HENSYU-EXT
                    END-IF
               WHEN    OTHER
                   PERFORM 42032-T09-HENSYU-SEC
                   GO  TO  4601-BODYLST-HENSYU-EXT
           END-EVALUATE
      *
           IF      SPA-TIDCD        =   SPACE
               INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-RECE-KOUMOKU
               MOVE    ZERO            TO  SPA-NAI-T06-BODY-SELNUM
           END-IF             
           MOVE    4               TO  SPA-GMN-T06-CUR
      *
           .
       4601-BODYLST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別指示一覧選択処理
      *****************************************************************
       470-RECELST-SELECT-SEC       SECTION.
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO  TO  470-RECELST-SELECT-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T06-RECE-MAX
               IF      T06-RECE-LIST-SELECT (IDX)    =   "T"
                   IF      IDX                 =   SPA-NAI-T06-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNO
                       MOVE    SPA-GMN-T06-RECE-MAX
                                               TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
               INITIALIZE                  SPA-NAI-T06-RECE-KOUMOKU
      *
               MOVE    WRK-SELNO           TO  SPA-NAI-T06-SELNUM
               MOVE    SPA-GMN-T06-R-TRENNUM (SPA-NAI-T06-SELNUM)
                                       TO  SPA-GMN-T06-SELNUM
      *
      *        選択項目編集処理
               PERFORM 4301-RENNUM-HENSYU-SEC
           END-IF
           .
       470-RECELST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-T06-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
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
           MOVE    T06-SKYYMD-H    TO  SPA-GMN-T06-SKYYMD-H
      *
           IF      SPA-NAI-T06-BODY-SELNUM =   ZERO
               MOVE    T06-PTNUM       TO  SPA-GMN-T06-PTNUM
               MOVE    T06-SRYYM       TO  SPA-GMN-T06-SRYYM
               MOVE    T06-NYUGAIKBN   TO  SPA-GMN-T06-NYUGAIKBN
           END-IF
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 420-SKYYMD-H-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4103-PTNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4104-SRYYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4105-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    追加処理
           IF    ( SPA-NAI-T06-SRYYM   >   ZERO  )
           AND   ( SPA-NAI-T06-PTID    >   ZERO  )
      *****AND   ( SPA-GMN-T06-NYUGAIKBN
      *****                        NOT =   SPACE )
      *****    IF      SPA-GMN-T06-SELNUM  =   ZERO
                   PERFORM 41041-TUIKA-SEC
      *****    END-IF
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月日チェック処理
      *****************************************************************
       420-SKYYMD-H-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-NAI-T06-SKYYMD-H
      *
           IF      SPA-GMN-T06-SKYYMD-H
                                   =   SPACE
               MOVE    SPA-SYSYMD      TO  SPA-NAI-T06-SKYYMD-H
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-T06-SKYYMD-H
           ELSE 
               INITIALIZE                  ORCSGDAYAREA
               MOVE    SPA-GMN-T06-SKYYMD-H
                                       TO  SGDAY-INDAY
               CALL    "ORCSGDAY"      USING
                                       ORCSGDAYAREA
               IF      SGDAY-RC        =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN-T06-SKYYMD-H
                   MOVE    SGDAY-SDAY      TO  SPA-NAI-T06-SKYYMD-H
               ELSE
                   MOVE    "0013"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD   NOT =   SPACE
               MOVE    2               TO  SPA-GMN-T06-CUR
           END-IF
      *
           .
       420-SKYYMD-H-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4103-PTNUM-CHK-SEC            SECTION.
      *
      *    請求年月日入力時に患者番号が未入力のとき
           IF    ( SPA-GMN-T06-PTNUM  =   SPACE      )
           AND   ( WRK-MCP-WIDGET     =   "SKYYMD_H"   OR
                                          "SRYYMKBN" )
               GO  TO  4103-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-T06-NAME
           INITIALIZE                      SPA-NAI-T06-PTID
                                           SPA-NAI-T06-BIRTHDAY
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    SPA-GMN-T06-PTNUM   TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
           ELSE
               MOVE    SPTID-PTNUM         TO  SPA-GMN-T06-PTNUM
               MOVE    SPTID-PTID          TO  SPA-NAI-T06-PTID
      *
      *        患者マスター存在チェック
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-T06-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    PTINF-NAME          TO  SPA-GMN-T06-NAME
                   MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-T06-BIRTHDAY
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4               TO  SPA-GMN-T06-CUR
           END-IF
           .
       4103-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4105-NYUGAIKBN-CHK-SEC                SECTION.
      *
      *    無床のときは２をセット
           IF    ( SPA-GMN-T06-NYUGAIKBN
                                   =   SPACE )
           AND   ( SPA-BEDSU       =   ZERO  )
               MOVE    "2"             TO  SPA-GMN-T06-NYUGAIKBN
               GO  TO  4105-NYUGAIKBN-CHK-EXT
           END-IF
      *
      *    連携用の時Ｔ０１で設定した入外区分とする
           IF      SPA-T01-PRTKBN  =   "6" OR  "7"
               MOVE    SPA-T01-NYUGAIKBN
                                       TO  SPA-GMN-T06-NYUGAIKBN
               GO  TO  4105-NYUGAIKBN-CHK-EXT
           END-IF
      *
      *    患者番号入力時に入外区分が未入力のとき
           IF    ( WRK-MCP-WIDGET     =   "PTNUM"  OR  "SKYYMD_H"  OR
                                          "SRYYMKBN" )
           AND   ( SPA-GMN-T06-NYUGAIKBN
                                      =   SPACE      )
               GO  TO  4105-NYUGAIKBN-CHK-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-T06-NYUGAIKBN
               WHEN    "1"
               WHEN    "2"
                   CONTINUE
               WHEN    SPACE
                   IF      SPA-BEDSU       =   ZERO
                       MOVE    "2"             TO  SPA-GMN-T06-NYUGAIKBN
                   END-IF
               WHEN    OTHER
                   MOVE    "0001"          TO  SPA-ERRCD
           END-EVALUATE 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5                   TO  SPA-GMN-T06-CUR
           END-IF
           .
       4105-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       4104-SRYYM-CHK-SEC          SECTION.
      *
      *    診療年月が未入力のとき
           IF    ( WRK-MCP-WIDGET NOT =   "SRYYM" )
           AND   ( SPA-GMN-T06-SRYYM  =   SPACE   )
               GO  TO  4104-SRYYM-CHK-EXT
           END-IF
      *  
           MOVE    ZERO               TO  SPA-NAI-T06-SRYYM
      *
           IF      SPA-GMN-T06-SRYYM      =   SPACE
               MOVE    SPA-NAI-T06-SRYYM-H    TO  SPA-NAI-T06-SRYYM
               MOVE    SPA-GMN-T06-SRYYM-H    TO  SPA-GMN-T06-SRYYM
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-T06-SRYYM       TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-T06-SRYYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-T06-SRYYM
               ELSE
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-T01-PRTKBN      =   "6" OR  "7"         )
               AND   ( SPA-NAI-T06-SRYYM
                                       NOT =   SPA-NAI-T06-SRYYM-H )
                   MOVE    "0028"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-T06-SRYYMKBN-MAX
                                       >   ZERO            )
               AND   ( SPA-T01-PRTKBN  NOT =   "6" AND "7" )
                   IF      SPA-GMN-T06-TEISYUTUSAKI4
                                               =   "T"
                       CONTINUE
                   ELSE
                       IF    ( SPA-NAI-T06-SRYYM
                                       <   SPA-NAI-T06-SRYYMKBN-ST
                                       (SPA-NAI-T06-SRYYMKBN-SELNUM) )
                       OR    ( SPA-NAI-T06-SRYYM
                                       >   SPA-NAI-T06-SRYYMKBN-ED
                                       (SPA-NAI-T06-SRYYMKBN-SELNUM) )
                           MOVE    "0030"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-T06-SRYYM   >   SPA-NAI-T06-SRYYM-H
                   MOVE    "0012"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6                   TO  SPA-GMN-T06-CUR
           END-IF
      *
           .
       4104-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月期間入力処理
      *****************************************************************
       4106-SRYYMKBN-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-T06-SRYYMKBN-MAX
                                       =   ZERO
               MOVE    2                   TO  SPA-GMN-T06-CUR
               GO  TO  4106-SRYYMKBN-CHK-EXT
           END-IF
      *
           MOVE    7                   TO  SPA-GMN-T06-CUR
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"              TO  SPA-ERRCD
               GO  TO  4106-SRYYMKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-T06-SRYYMKBN-G
                                       TO  SPA-GMN-T06-SRYYMKBN-G-OLD
           MOVE    SPA-NAI-T06-SRYYMKBN-SELNUM
                                       TO
                                       SPA-NAI-T06-SRYYMKBN-SELNUM-OLD
      *
           MOVE    T06-SRYYMKBN        TO  SPA-GMN-T06-SRYYMKBN-G
           MOVE    SPACE               TO  SPA-GMN-T06-SRYYMKBNNM
           MOVE    ZERO                TO  SPA-NAI-T06-SRYYMKBN-SELNUM
           IF      SPA-GMN-T06-SRYYMKBN
                                       =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-T06-SRYYMKBN-MAX
                   IF        SPA-GMN-T06-SRYYMKBN
                                       =   SPA-GMN-T06-SRYYMKBNL (IDX)
                       IF      SPA-NAI-T06-SRYYMKBN-ERR (IDX)
                                           =   SPACE
                           MOVE    SPA-GMN-T06-SRYYMKBNNML (IDX)
                                           TO  SPA-GMN-T06-SRYYMKBNNM
                           MOVE    IDX         TO
                                           SPA-NAI-T06-SRYYMKBN-SELNUM
                       ELSE
                           MOVE    "0050"      TO  SPA-ERRCD
                       END-IF
                       MOVE    SPA-GMN-T06-SRYYMKBN-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
           IF      SPA-GMN-T06-SRYYMKBNNM  =   SPACE
               MOVE    SPA-GMN-T06-SRYYMKBNLST (1)
                                           TO  SPA-GMN-T06-SRYYMKBN-G
               MOVE    1                   TO
                                           SPA-NAI-T06-SRYYMKBN-SELNUM
           END-IF
      *
           MOVE    SPA-NAI-T06-SRYYMKBN-HOSPCD
                                           (SPA-NAI-T06-SRYYMKBN-SELNUM)
                                           TO  SPA-NAI-T06-HOSPCD
      *
           IF      SPA-GMN-T06-SRYYMKBN
                                   NOT =   SPA-GMN-T06-SRYYMKBN-OLD
      *        未処理分があるときのチェック 
               IF    ( SPA-NAI-T06-SYORIFLG
                                           =   ZERO )
               AND   ( SPA-GMN-T06-RECE-MAX
                                           >   ZERO )
                   MOVE    "1019"              TO  SPA-TIDCD
                   MOVE    "SRYYMKBN"          TO  SPA-T01-WIDGET
                   GO  TO  4106-SRYYMKBN-CHK-EXT
               END-IF
           END-IF
      *
      *    管理情報編集処理
           PERFORM 3101-ONRECE-KANRI-HENSYU-SEC
      *
           INITIALIZE                  SPA-GMN-T06-SRYYMKBN-G-OLD
                                       SPA-NAI-T06-SRYYMKBN-SELNUM-OLD
           MOVE    2                   TO  SPA-GMN-T06-CUR
           .
       4106-SRYYMKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加前チェック処理
      *****************************************************************
       41041-TUIKA-SEC                SECTION.
      *
      *    追加できるか
           IF      SPA-GMN-T06-SELNUM  =   ZERO
               IF      SPA-GMN-T06-RECE-MAX =   WRK-CONS-LINE-MAX
                   MOVE    "0002"              TO  SPA-ERRCD
                   GO  TO  41041-TUIKA-EXT
               END-IF
           END-IF
      *
           IF      SPA-T01-PRTKBN      =   "6" OR  "7"
      *        連携テンポラリとのチェック処理
               PERFORM 41041-RENKEI-TMP-TUIKA-SEC
           ELSE
      *       請求管理とのチェック処理
               PERFORM 41041-SEIKYU-TUIKA-SEC
           END-IF
           .
      *
       41041-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携テンポラリから編集処理
      *****************************************************************
       41041-RENKEI-TMP-TUIKA-SEC              SECTION.
      *
           INITIALIZE                      RECEDEN-REC
           MOVE    SPA-HOSPNUM         TO  RECEDEN-HOSPNUM
           MOVE    SPA-NAI-T06-PTID    TO  RECEDEN-PTID
           MOVE    SPA-NAI-T06-SRYYM   TO  RECEDEN-SRYYM
           MOVE    SPA-GMN-T06-NYUGAIKBN
                                       TO  RECEDEN-NYUGAIKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
               MOVE    "key4"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           END-IF
           PERFORM 900-RENKEI-TMP-READ-SEC
      *
           IF      FLG-RENKEI-TMP      =   ZERO
      *        レセ電請求一覧とのチェック
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-T06-RECE-MAX
                   IF      RECEDEN-PTID    =   SPA-NAI-T06-RECE10-PTID
                                                                  (IDY)
                       MOVE    "0008"          TO  SPA-ERRCD
                       MOVE    SPA-GMN-T06-RECE-MAX
                                               TO  IDY  
                   END-IF
               END-PERFORM
           ELSE
               MOVE    "0021"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD            =   SPACE
               IF      SPA-GMN-T06-SELNUM  =   ZERO
                   ADD     1                   TO  SPA-GMN-T06-RECE-MAX
                   MOVE    SPA-GMN-T06-RECE-MAX
                                               TO  SPA-GMN-T06-R-TRENNUM
                                                (SPA-GMN-T06-RECE-MAX)
                   MOVE    SPA-GMN-T06-RECE-MAX
                                               TO  IDX
               ELSE
                   MOVE    SPA-GMN-T06-SELNUM  TO  IDX
               END-IF    
      *
               MOVE    RECEDEN-KEY      TO  SPA-NAI-T06-RECE10-KEY (IDX)
      *   
               EVALUATE    RECEDEN-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"    TO  SPA-GMN-T06-R-TNYUGAIKBN(IDX)
                   WHEN    "2"    
                       MOVE    "外"    TO  SPA-GMN-T06-R-TNYUGAIKBN(IDX)
               END-EVALUATE
               MOVE    SPA-GMN-T06-PTNUM   TO  SPA-GMN-T06-R-TPTNUM(IDX)
               MOVE    SPA-GMN-T06-NAME    TO  SPA-GMN-T06-R-TNAME (IDX)
               MOVE    SPA-GMN-T06-SRYYM   TO  SPA-GMN-T06-R-TSRYYM(IDX)
      *
               MOVE    RECEDEN-CREYMD  TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-T06-R-TCREYMD (IDX)
      *
               IF      SPA-GMN-T06-SELNUM  =   ZERO
                   MOVE    SPA-GMN-T06-RECE-MAX
                                            TO  T06-RECE-LIST-ROW
               ELSE
                   MOVE    SPA-GMN-T06-SELNUM
                                            TO  T06-RECE-LIST-ROW
               END-IF
      * 
               INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-BODY-SELNUM
           END-IF
      *
           MOVE    4                    TO  SPA-GMN-T06-CUR
           .
      *
       41041-RENKEI-TMP-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理とのチェック処理
      *****************************************************************
       41041-SEIKYU-TUIKA-SEC              SECTION.
      *
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-NAI-T06-PTID    TO  RECE10-PTID
           MOVE    SPA-NAI-T06-SRYYM   TO  RECE10-SRYYM
           MOVE    SPA-NAI-T06-SRYYM-H TO  RECE10-SKYYM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
               MOVE    1                   TO  RECE10-TEISYUTUSAKI
                                               RECE10-XXTEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI2
                                           =   "T"
                   MOVE    6                   TO  RECE10-TEISYUTUSAKI
                   MOVE    2                   TO  RECE10-XXTEISYUTUSAKI
               ELSE
                   MOVE    3                   TO  RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI
               END-IF
           END-IF
           EVALUATE    SPA-GMN-T06-NYUGAIKBN
               WHEN    "1"
               WHEN    "2"
                   MOVE    SPA-GMN-T06-NYUGAIKBN
                                           TO  RECE10-NYUGAIKBN
               WHEN    OTHER
                   MOVE    "%"             TO  RECE10-NYUGAIKBN
           END-EVALUATE
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           IF      SPA-NAI-T06-SRYYM   =   SPA-NAI-T06-SRYYM-H
               MOVE    "key70"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key71"             TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    ZERO                TO  IDX
                                           IDY
                                           IDZ
                                           IDW
           INITIALIZE                      WRK-RECE10-REC
                                           SPA-T09-AREA
      *
           PERFORM         UNTIL       FLG-SEIKYU     =   1
                           OR          IDX            >=  20
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *        災害対応（未確定分をレセ電データに記録するか）
               IF    (  SPA-T06-2005-RECEDEN-DISASTER
                                           =   "0"              )
               AND   ( RECE10-ETC-YKZFTN (1)
                                           =   6   OR  7  OR  8 )
                   ADD     1                   TO  IDW
               ELSE
      *            月遅れ分、請求しないは対象外
                   IF    ( RECE10-SKYKBN   NOT =   "0"             )
                   AND   ( RECE10-SKYYM        =   "000000"
                                           OR  SPA-NAI-T06-SRYYM-H )
                       IF      SPA-NAI-T06-BODY-SELNUM
                                           >   ZERO
                           EVALUATE    SPA-NAI-T06-B-THENREI-FLG
                                           (SPA-NAI-T06-BODY-SELNUM)
                               WHEN    SPACE
      *                            返戻分は対象外
                                   IF      RECE10-HENREIYM =   "000000"
                                       ADD     1               TO  IDX
                                       PERFORM 42031-T09-HENSYU-SEC
                                       IF      IDX             =   1    
                                           MOVE    RECE10-REC  TO
                                                       WRK-RECE10-REC
                                       END-IF
                                   ELSE
                                       ADD     1               TO  IDY
                                   END-IF
                               WHEN    "1"
      *                            返戻分設定済のみ対象
                                   IF    ( RECE10-HENREIYM
                                                       NOT =  "000000" )
                                   AND   ( RECE10-HENREI-SIJI-FLG
                                                           =  1        )
                                       ADD     1               TO  IDX
                                       PERFORM 42031-T09-HENSYU-SEC
                                       IF      IDX             =   1    
                                           MOVE    RECE10-REC  TO
                                                       WRK-RECE10-REC
                                       END-IF
                                   ELSE
                                       ADD     1               TO  IDY
                                   END-IF
                           END-EVALUATE
                       ELSE
      *                    返戻分は対象外
                           IF      RECE10-HENREIYM =   "000000"
                               ADD     1               TO  IDX
                               PERFORM 42031-T09-HENSYU-SEC
                               IF      IDX             =   1
                                   MOVE    RECE10-REC
                                                   TO  WRK-RECE10-REC
                               END-IF
                           ELSE
      *                        返戻分設定済のみ対象
                               IF    ( RECE10-HENREIYM NOT =  "000000" )
                               AND   ( RECE10-HENREI-SIJI-FLG
                                                           =  1        )
                                   ADD     1               TO  IDX
                                   PERFORM 42031-T09-HENSYU-SEC
                                   IF      IDX             =   1
                                       MOVE    RECE10-REC  TO
                                                       WRK-RECE10-REC
                                   END-IF
                               ELSE
                                   ADD     1               TO  IDY
                               END-IF
                           END-IF
                       END-IF
                   ELSE
                       ADD     1               TO  IDZ
                   END-IF
               END-IF
      *
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           EVALUATE    IDX
               WHEN    ZERO
                   IF    ( IDY             =   0 )
                   AND   ( IDZ             =   0 )
                   AND   ( IDW             =   0 )
                       MOVE    "0007"          TO  SPA-ERRCD
                   ELSE
                       IF    ( SPA-NAI-T06-BODY-SELNUM
                                       >   ZERO                      )
                       AND   ( SPA-NAI-T06-B-THENREI-FLG
                                           (SPA-NAI-T06-BODY-SELNUM) )
                                       =   "1"
                           MOVE    "0017"          TO  SPA-ERRCD
                       ELSE
                           IF    ( IDY             >   0 )
                           AND   ( IDZ             >   0 )
                           AND   ( IDW             >   0 )
                               MOVE    "0015"          TO  SPA-ERRCD
                           ELSE
                               IF    ( IDY             >   0 )
                                   MOVE    "0014"          TO  SPA-ERRCD
                               ELSE
                                 IF    ( IDZ             >   0 )
                                   MOVE    "0016"          TO  SPA-ERRCD
                                 ELSE
                                   MOVE    "0019"          TO  SPA-ERRCD
                                 END-IF
                               END-IF 
                           END-IF 
                       END-IF 
                   END-IF 
               WHEN    1
                   MOVE    WRK-RECE10-REC   TO  RECE10-REC
                   PERFORM 42031-SEIKYU-CHK-SEC
                   IF      SPA-TIDCD       =   "1002"
                       MOVE    RECE10-NYUGAIKBN
                                           TO  SPA-GMN-T06-NYUGAIKBN
                   END-IF  
      *************INITIALIZE              SPA-T09-AREA
               WHEN    OTHER
                   PERFORM 42032-T09-HENSYU-SEC
                   GO  TO  41041-SEIKYU-TUIKA-EXT
           END-EVALUATE
      *
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-GMN-CUR          TO  SPA-GMN-T06-CUR
           ELSE
               MOVE    4                    TO  SPA-GMN-T06-CUR
           END-IF             
           .
      *
       41041-SEIKYU-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象患者一覧編集処理
      *****************************************************************
       42031-T09-HENSYU-SEC              SECTION.
      *
           MOVE    "F"             TO  SPA-NAI-T09-TSELECT        (IDX)
      *
           MOVE    RECE10-KEY      TO  SPA-NAI-T09-RECE10-KEY     (IDX)
           MOVE    RECE10-HKNNUM   TO  SPA-NAI-T09-RECE10-THKNNUM (IDX)
           MOVE    RECE10-HENREI-SIJI-FLG
                                   TO
                                   SPA-NAI-T09-RECE10-THENREI-SIJI
                                                                  (IDX)
           IF      RECE10-TEISYUTUSAKI =   "3"
               MOVE    RECE10-SYOKAIFLG    TO
                                       SPA-NAI-T09-RECE10-TSYOKAIFLG
                                                                  (IDX)
               MOVE    RECE10-HKNJANUM-S   TO
                                       SPA-NAI-T09-RECE10-THKNJANUM-S
                                                                 (IDX)
           END-IF
           IF      RECE10-TEISYUTUSAKI =   "1"
               MOVE    RECE10-HKNJANUM-S
                                   TO  SPA-NAI-T09-THKNJANUM-S    (IDX)
           END-IF
      *
           MOVE    IDX             TO  SPA-GMN-T09-TRENNUM        (IDX)
           MOVE    RECE10-PTNUM    TO  SPA-GMN-T09-TPTNUM         (IDX)
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-NAME      TO  SPA-GMN-T09-TNAME      (IDX)
           MOVE    PTINF-KANANAME  TO  SPA-NAI-T09-TKANANAME  (IDX)
      *
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   MOVE    "入"    TO  SPA-GMN-T09-TNYUGAIKBN (IDX)
               WHEN    "2"    
                   MOVE    "外"    TO  SPA-GMN-T09-TNYUGAIKBN (IDX)
           END-EVALUATE
      *
      *    ＨＡＯＲＩのとき
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    RECE10-HKNID    TO  SPA-NAI-T09-RECE10-THKNID
                                                                 (IDX)
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    "3"
                       MOVE    RECE10-HKNKBN   TO
                                       SPA-NAI-T09-RECE10-THKNKBN  (IDX)
                       MOVE    RECE10-SAIGAIKBN    TO
                                       SPA-NAI-T09-RECE10-TSAIGAIKBN
                                                                   (IDX)
                   WHEN    OTHER
                       MOVE    RECE10-HOJOKBN  TO
                                       SPA-NAI-T09-THOJOKBN  (IDX)
                       IF      RECE10-KOHNUM (1)
                                           NOT =   SPACE
      *                    対象患者一覧編集処理（公費情報の設定）
                           PERFORM 420311-T09-HENSYU-KOH-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    "3"
                   EVALUATE    RECE10-RECESYUBETU
                       WHEN    "7101"
                       WHEN    "7091"
                           MOVE    "傷病・業務"        TO
                                           SPA-GMN-T09-TRECESYUBETU(IDX)
                       WHEN    "7102"
                       WHEN    "7092"
                           MOVE    "傷病・通勤"        TO
                                           SPA-GMN-T09-TRECESYUBETU(IDX)
                       WHEN    "7071"
                       WHEN    "7081"
                           MOVE    "短期・業務"        TO
                                           SPA-GMN-T09-TRECESYUBETU(IDX)
                       WHEN    "7072"
                       WHEN    "7082"
                           MOVE    "短期・通勤"        TO
                                           SPA-GMN-T09-TRECESYUBETU(IDX)
                       WHEN    OTHER
                           MOVE    RECE10-RECESYUBETU  TO
                                           SPA-GMN-T09-TRECESYUBETU(IDX)
                   END-EVALUATE
               WHEN    OTHER
                   MOVE    RECE10-RECESYUBETU  TO
                                           SPA-GMN-T09-TRECESYUBETU(IDX)
           END-EVALUATE
      *
           IF     RECE10-HKNJANUM  NOT =   "00000000"
               MOVE    RECE10-HKNJANUM     TO  SPA-GMN-T09-THKNJANUM
                                                                  (IDX)
           END-IF
      *
           EVALUATE    RECE10-HKNNUM
               WHEN    "002"
               WHEN    "031"  THRU  "034"
                   EVALUATE    RECE10-HOJOKBN-KEY
                       WHEN    "1"
                       WHEN    "A"
                       WHEN    "D"
                       WHEN    "G"
                           MOVE    "職務上"    TO
                                               SPA-GMN-T09-THOJOKBN(IDX)
                       WHEN    "2"
                       WHEN    "B"
                       WHEN    "E"
                       WHEN    "H"
                           MOVE    "下船３月"  TO
                                               SPA-GMN-T09-THOJOKBN(IDX)
                       WHEN    "3"
                       WHEN    "C"
                       WHEN    "F"
                       WHEN    "I"
                           MOVE    "通勤災害"  TO
                                               SPA-GMN-T09-THOJOKBN(IDX)
                   END-EVALUATE
           END-EVALUATE
      *
      *    公費単独で生保又は中国残留邦人等のとき負担者番号、
      *    受給者番号をセット
           IF    ( RECE10-HKNNUM       =   SPACE               )
             IF    ( RECE10-KOHNUM (1)   =   "012"   OR  "025"
                                                     OR  "020"
                                                     OR  "053" )
             AND   ( RECE10-KOHNUM (2)   =   SPACE             )
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
               MOVE    RECE10-PTID     TO  PTKOH-PTID
               MOVE    RECE10-KOHID(1) TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               MOVE    PTKOH-FTNJANUM  TO  SPA-NAI-T09-RECE10-TKIGO(IDX)
               MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
               MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
               MOVE    RECE10-KOHID (1)
                                       TO  MONTHLYNUM-KOHID
               MOVE    RECE10-SRYYM    TO  MONTHLYNUM-SRYYM
               MOVE    RECE10-NYUGAIKBN
                                       TO  MONTHLYNUM-NYUGAIKBN
               MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
               PERFORM 900-MONTHLYNUM-READ-SEC
               IF      FLG-MONTHLYNUM  =   ZERO
                   MOVE    MONTHLYNUM-JKYSNUM  TO
                                           SPA-NAI-T09-RECE10-TNUM (IDX)
               ELSE
                   MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
                   MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
                   MOVE    ZERO            TO  MONTHLYNUM-KOHID
                   MOVE    RECE10-SRYYM    TO  MONTHLYNUM-SRYYM
                   MOVE    RECE10-NYUGAIKBN
                                           TO  MONTHLYNUM-NYUGAIKBN
                   MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                   PERFORM 900-MONTHLYNUM-READ-SEC
                   IF      FLG-MONTHLYNUM          =   ZERO
                       MOVE    MONTHLYNUM-JKYSNUM
                                       TO  SPA-NAI-T09-RECE10-TNUM (IDX)
                   ELSE
                       MOVE   PTKOH-JKYSNUM
                                       TO  SPA-NAI-T09-RECE10-TNUM (IDX)
                   END-IF
               END-IF             
               STRING  SPA-NAI-T09-RECE10-TKIGO (IDX)
                                               DELIMITED   BY  SPACE
                       "　"                    DELIMITED   BY  SIZE
                       SPA-NAI-T09-RECE10-TNUM  (IDX)
                                               DELIMITED   BY  SPACE
                                       INTO    SPA-GMN-T09-TKIGO  (IDX)
               END-STRING
      **       MOVE    SPACE           TO  SPA-NAI-T09-RECE10-TKIGO(IDX)
      **                                   SPA-NAI-T09-RECE10-TNUM (IDX)
             ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
               MOVE    RECE10-PTID     TO  PTKOH-PTID
               MOVE    RECE10-KOHID(1) TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               MOVE    PTKOH-FTNJANUM  TO  SPA-NAI-T09-RECE10-TKIGO(IDX)
               MOVE    PTKOH-JKYSNUM   TO  SPA-NAI-T09-RECE10-TNUM (IDX)
             END-IF
           ELSE
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    "3"
                       MOVE    RECE10-KOFUNUM  TO
                                           SPA-GMN-T09-TKIGO       (IDX)
                                           SPA-NAI-T09-RECE10-TKIGO(IDX)
                   WHEN    OTHER
                       MOVE    RECE10-KIGO     TO
                                           SPA-NAI-T09-RECE10-TKIGO(IDX)
                       MOVE    RECE10-NUM      TO
                                           SPA-NAI-T09-RECE10-TNUM (IDX)
                       STRING  SPA-NAI-T09-RECE10-TKIGO (IDX)
                                               DELIMITED   BY  SPACE
                               "　"            DELIMITED   BY  SIZE
                               SPA-NAI-T09-RECE10-TNUM  (IDX)
                                               DELIMITED   BY  SPACE
                                       INTO    SPA-GMN-T09-TKIGO  (IDX)
                       END-STRING
               END-EVALUATE
           END-IF
      *
           EVALUATE    RECE10-RECEIPT-KBN (1:1)
               WHEN    "1"
                   MOVE    "老併"          TO
                                       SPA-GMN-T09-TRECEIPT-KBN (IDX)
               WHEN    "2"
                   MOVE    "老健"          TO
                                       SPA-GMN-T09-TRECEIPT-KBN (IDX)
               WHEN    "3"
                   MOVE    "医併"          TO
                                       SPA-GMN-T09-TRECEIPT-KBN (IDX)
               WHEN    "4"
                   MOVE    "医療"          TO
                                       SPA-GMN-T09-TRECEIPT-KBN (IDX)
           END-EVALUATE
      *
           MOVE    RECE10-ETC-YKZFTN (1)
                                   TO  SPA-NAI-T09-TDISASTER   (IDX)
           EVALUATE    RECE10-ETC-YKZFTN (1)
               WHEN    0
                   CONTINUE
               WHEN    OTHER
                   MOVE    "災"        TO  SPA-GMN-T09-TDISASTER   (IDX)
           END-EVALUATE
      *
           MOVE    RECE10-CREYMD   TO  SPA-NAI-T09-TCREYMD (IDX)
                                       WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-T09-TCREYMD (IDX)
      *
      *    返戻データのとき該当の請求管理の返戻データへの設定チェック
           IF      SPA-NAI-T06-HENREI-FLG
                                   =   "1"
               MOVE    ZERO            TO  FLG-HENREI-B-SEIKYU-CHK
               IF      RECE10-HENREI-SIJI-FLG
                                       =   1
      *
                   INITIALIZE                  HENREI-B-REC
                   MOVE    SPA-HOSPNUM     TO  HENREI-B-HOSPNUM
                   MOVE    RECE10-NYUGAIKBN
                                           TO  HENREI-B-NYUGAIKBN
                   MOVE    RECE10-SRYYM    TO  HENREI-B-SRYYM
                   MOVE    RECE10-PTID     TO  HENREI-B-PTID
                   MOVE    HENREI-B-REC    TO  MCPDATA-REC
                   MOVE    "tbl_henrei_body"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_henrei_body"
                                               TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 900-HENREI-BODY-READ-SEC
                   ELSE
                       MOVE    1               TO  FLG-HENREI-B
                   END-IF
      *
                   PERFORM         UNTIL   FLG-HENREI-B            =   1
                                   OR      FLG-HENREI-B-SEIKYU-CHK =   1
                       PERFORM VARYING IDZZ    FROM    1   BY  1
                               UNTIL   IDZZ    >       10
                               OR      HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                               =       SPACE
                               OR      FLG-HENREI-B-SEIKYU-CHK
                                               =       1
                           IF    ( HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                               =   RECE10-TEISYUTUSAKI )
                           AND   ( HENREI-B-S-NYUGAIKBN    (IDZZ)
                                               =   RECE10-NYUGAIKBN    )
                           AND   ( HENREI-B-S-RECEKA       (IDZZ)
                                               =   RECE10-RECEKA       )
                           AND   ( HENREI-B-S-RECESYUBETU  (IDZZ)
                                               =   RECE10-RECESYUBETU  )
                           AND   ( HENREI-B-S-HKNJANUM     (IDZZ)
                                               =   RECE10-HKNJANUM     )
                           AND   ( HENREI-B-S-HOJOKBN-KEY  (IDZZ)
                                               =   RECE10-HOJOKBN-KEY  )
                           AND   ( HENREI-B-S-KOHID-KEY    (IDZZ)
                                               =   RECE10-KOHID-KEY    )
                           AND   ( HENREI-B-S-TEKSTYMD     (IDZZ)
                                               =   RECE10-TEKSTYMD     )
                           AND   ( HENREI-B-S-RECEIPT-KBN  (IDZZ)
                                               =   RECE10-RECEIPT-KBN  )
                               MOVE    1   TO  FLG-HENREI-B-SEIKYU-CHK
                           END-IF
                       END-PERFORM
      *
                       MOVE    "tbl_henrei_body"       TO  MCP-TABLE
                       MOVE    "key"                   TO  MCP-PATHNAME
                       PERFORM 900-HENREI-BODY-READ-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_henrei_body"       TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
      *
               IF      FLG-HENREI-B-SEIKYU-CHK =   ZERO
                   MOVE    "1"             TO  SPA-NAI-T09-THENREI (IDX)
               END-IF
           END-IF
      *
      *    返戻年月入力なしのとき返戻データの存在チェック
           IF    ( SPA-NAI-T06-BODY-SELNUM
                                   =   ZERO                )
           AND   ( RECE10-SKYYM    =   SPA-NAI-T06-SRYYM-H )
           AND   ( RECE10-SRYYM    <=  SPA-NAI-T06-SRYYM-H )
           AND   ( RECE10-HENREIYM =   "000000"            )
               INITIALIZE                  HENREI-B-REC
               MOVE    SPA-HOSPNUM     TO  HENREI-B-HOSPNUM
               MOVE    RECE10-NYUGAIKBN
                                       TO  HENREI-B-NYUGAIKBN
               MOVE    RECE10-SRYYM    TO  HENREI-B-SRYYM
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    1
                       MOVE    "1"     TO  HENREI-B-TEISYUTUSAKI
                                           HENREI-B-XXTEISYUTUSAKI
                   WHEN    2
                   WHEN    6
                       MOVE   "2"      TO  HENREI-B-TEISYUTUSAKI
                                           HENREI-B-XXTEISYUTUSAKI
                   WHEN    3
                       MOVE    "3"     TO  HENREI-B-TEISYUTUSAKI
                                           HENREI-B-XXTEISYUTUSAKI
               END-EVALUATE
               MOVE    RECE10-PTID     TO  HENREI-B-PTID
               MOVE    HENREI-B-REC    TO  MCPDATA-REC
               MOVE    "tbl_henrei_body"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_henrei_body"
                                           TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 900-HENREI-BODY-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-HENREI-B
               END-IF
      *           
               PERFORM         UNTIL   FLG-HENREI-B    =   1
                               OR      SPA-NAI-T09-THENREIYM (IDX)
                                                   NOT =   SPACE
                   MOVE    "1"     TO  SPA-NAI-T09-THENREIYM (IDX)
                   IF    ( HENREI-B-RECESYUBETU
                                       =   RECE10-RECESYUBETU )
                   AND   ( HENREI-B-HKNJANUM
                                       =   RECE10-HKNJANUM    )
                       IF      RECE10-HKNNUM   NOT =   SPACE
                           IF    ( RECE10-KIGO =   HENREI-B-KIGO )
                           AND   ( RECE10-NUM  =   HENREI-B-NUM  )
                               MOVE    "2" TO  SPA-NAI-T09-THENREIYM
                                                                   (IDX)
                           END-IF
                       ELSE
                           IF    ( SPA-NAI-T09-RECE10-TKIGO(IDX)
                                               =   HENREI-B-FTNNUM (1) )
                           AND   ( SPA-NAI-T09-RECE10-TNUM (IDX)
                                               =   HENREI-B-JKYNUM (1) )
                               MOVE    "2" TO  SPA-NAI-T09-THENREIYM
                                                                   (IDX)
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_henrei_body"   TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-HENREI-BODY-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    レセ電請求一覧とのチェック
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-T06-RECE-MAX
               IF      RECE10-KEY  =   SPA-NAI-T06-RECE10-KEY (IDY)
                   MOVE    "1"         TO  SPA-GMN-T09-TCHK (IDX)
                   MOVE    SPA-GMN-T06-RECE-MAX
                                           TO  IDY  
               END-IF
           END-PERFORM
      *
      *    レセ電データ存在チェック
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                      RECEDEN-REC
           MOVE    RECE10-KEY          TO  RECEDEN-KEY
           MOVE    "1"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           PERFORM 900-RECEDEN-READ-SEC
           IF      FLG-RECEDEN         =   1
               MOVE    "1"                 TO  SPA-NAI-T09-TRECEDEN(IDX)
           ELSE
      *        平成２４年３月以前の月後れ分が改正パッチ前の作成か
               IF    ( SPA-NAI-T06-SRYYM-H >   201203     )
               AND   ( RECE10-SRYYM        <=  201203     )
               AND   ( RECE10-CREYMD       <   "20120423" )
                   MOVE    "9"             TO  SPA-NAI-T09-TRECEDEN(IDX)
               END-IF
           END-IF
      *
      *    請求済かどうか
           INITIALIZE                  ONRECE-B-REC
           MOVE    SPA-HOSPNUM     TO  ONRECE-B-HOSPNUM
           IF      RECE10-TEISYUTUSAKI =   "1"
               MOVE    RECE10-TEISYUTUSAKI
                                       TO  ONRECE-B-TEISYUTUSAKI
           ELSE
               IF      RECE10-TEISYUTUSAKI =   "3"
                   MOVE    "3"             TO  ONRECE-B-TEISYUTUSAKI
               ELSE
                   MOVE    "2"             TO  ONRECE-B-TEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-B-SRYYM
           MOVE    RECE10-SRYYM    TO  ONRECE-B-SRYYM-B
           MOVE    RECE10-PTID     TO  ONRECE-B-PTID
           MOVE    RECE10-NYUGAIKBN
                                   TO  ONRECE-B-NYUGAIKBN
           MOVE    RECE10-RECESYUBETU
                                   TO  ONRECE-B-RECESYUBETU
           MOVE    SPA-GMN-T09-THKNJANUM (IDX)
                                   TO  ONRECE-B-HKNJANUM
           MOVE    "1"             TO  ONRECE-B-KAKUTEI-KBN
      *
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
           PERFORM             UNTIL   FLG-ONRECE-B    =   1
                               OR       SPA-NAI-T09-TSEIKYU (IDX)
                                                       =   "1"
               IF      RECE10-HKNNUM   NOT =   SPACE
                   IF    ( RECE10-KIGO     =   ONRECE-B-KIGO )
                   AND   ( RECE10-NUM      =   ONRECE-B-NUM  )
                       MOVE    "1"     TO  SPA-NAI-T09-TSEIKYU (IDX)
                   END-IF
               ELSE
                   IF    ( SPA-NAI-T09-RECE10-TKIGO(IDX)
                                           =   ONRECE-B-FTNNUM (1) )
                   AND   ( SPA-NAI-T09-RECE10-TNUM (IDX)
                                           =   ONRECE-B-JKYNUM (1) )
                       MOVE    "1"     TO  SPA-NAI-T09-TSEIKYU (IDX)
                   END-IF
               END-IF
      * 
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    最新の受付日
      *    請求管理と同じ条件のデータの受付日を取得
           INITIALIZE                  ONRECE-B-REC
           MOVE    SPA-HOSPNUM     TO  ONRECE-B-HOSPNUM
           IF      RECE10-TEISYUTUSAKI =   "1"
               MOVE    RECE10-TEISYUTUSAKI
                                       TO  ONRECE-B-TEISYUTUSAKI
           ELSE
               IF      RECE10-TEISYUTUSAKI =   "3"
                   MOVE    "3"             TO  ONRECE-B-TEISYUTUSAKI
               ELSE
                   MOVE    "2"             TO  ONRECE-B-TEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-B-SRYYM
           MOVE    RECE10-SRYYM    TO  ONRECE-B-SRYYM-B
           MOVE    RECE10-PTID     TO  ONRECE-B-PTID
           MOVE    RECE10-NYUGAIKBN
                                   TO  ONRECE-B-NYUGAIKBN
           MOVE    RECE10-RECESYUBETU
                                   TO  ONRECE-B-RECESYUBETU
           MOVE    SPA-GMN-T09-THKNJANUM (IDX)
                                   TO  ONRECE-B-HKNJANUM
           MOVE    "%"             TO  ONRECE-B-KAKUTEI-KBN
      *
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
           PERFORM             UNTIL   FLG-ONRECE-B    =   1
                               OR      SPA-NAI-T09-TUKEYMD (IDX)
                                                       >   ZERO
               IF      RECE10-HKNNUM   NOT =   SPACE
                   IF    ( RECE10-KIGO     =   ONRECE-B-KIGO )
                   AND   ( RECE10-NUM      =   ONRECE-B-NUM  )
                       MOVE    ONRECE-B-SYORIYMD
                                           TO  SPA-NAI-T09-TUKEYMD (IDX)
                   END-IF
               ELSE
                   IF    ( SPA-NAI-T09-RECE10-TKIGO(IDX)
                                           =   ONRECE-B-FTNNUM (1) )
                   AND   ( SPA-NAI-T09-RECE10-TNUM (IDX)
                                           =   ONRECE-B-JKYNUM (1) )
                       MOVE    ONRECE-B-SYORIYMD
                                           TO  SPA-NAI-T09-TUKEYMD (IDX)
                   END-IF
               END-IF
      * 
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    請求管理と同じ条件の請求済データがない場合最新の受付日を取得
           IF      SPA-NAI-T09-TUKEYMD (IDX)
                                   =   ZERO
               INITIALIZE                  ONRECE-B-REC
               MOVE    SPA-HOSPNUM     TO  ONRECE-B-HOSPNUM
               IF      RECE10-TEISYUTUSAKI =   "1"
                   MOVE    RECE10-TEISYUTUSAKI
                                           TO  ONRECE-B-TEISYUTUSAKI
               ELSE
                   IF      RECE10-TEISYUTUSAKI =   "3"
                       MOVE    "3"             TO  ONRECE-B-TEISYUTUSAKI
                   ELSE
                       MOVE    "2"             TO  ONRECE-B-TEISYUTUSAKI
                   END-IF
               END-IF
               MOVE    SPA-NAI-T06-SRYYM-H
                                       TO  ONRECE-B-SRYYM
               MOVE    RECE10-SRYYM    TO  ONRECE-B-SRYYM-B
               MOVE    RECE10-PTID     TO  ONRECE-B-PTID
               MOVE    RECE10-NYUGAIKBN
                                       TO  ONRECE-B-NYUGAIKBN
               MOVE    "%"             TO  ONRECE-B-KAKUTEI-KBN
      *
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-SELECT-SEC
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
               IF      FLG-ONRECE-B    =   ZERO
                   MOVE    ONRECE-B-SYORIYMD
                                           TO  SPA-NAI-T09-TUKEYMD (IDX)
               END-IF
           END-IF
      *    
      *    主科設定時の主科未設定データの存在チェック
           IF      RECE10-RECEKA            =   "00"
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    "1"
                       IF     SPA-NAI-T06-2009-SYUKA-SYAHO  =   "1"
                           MOVE    "1"     TO  SPA-NAI-T09-TSYUKA (IDX)
                       END-IF
                   WHEN    "2"
                       IF      SPA-NAI-T06-2009-SYUKA-KOKUHO =   "1"
                           MOVE    "1"     TO  SPA-NAI-T09-TSYUKA (IDX)
                       END-IF
                   WHEN    "6"
                       IF      SPA-NAI-T06-2009-SYUKA-KOUIKI =   "1"
                           MOVE    "1"     TO  SPA-NAI-T09-TSYUKA (IDX)
                       END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    IDX             TO  SPA-GMN-T09-MAX
           .
      *
       42031-T09-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象患者一覧編集処理（公費情報の設定）
      *****************************************************************
       420311-T09-HENSYU-KOH-SEC   SECTION.
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       4
                   OR      RECE10-KOHNUM (IDZZ)
                                   =       SPACE
               MOVE    RECE10-KOHNUM  (IDZZ)  TO
                                   SPA-NAI-RECE10-T09-KOHNUM (IDX IDZZ)
               MOVE    RECE10-KOHID   (IDZZ)  TO
                                   SPA-NAI-RECE10-T09-KOHID  (IDX IDZZ)
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-RECEDEN
                                           IDZZ
      *
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                      RECEDEN-REC
           MOVE    RECE10-KEY          TO  RECEDEN-KEY
           MOVE    "1"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-RECEDEN-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
       *
           PERFORM             UNTIL   FLG-RECEDEN =   1
                               OR      IDZZ        =   4
               IF      RECEDEN-RECEDATA (1:2)  =   "KO"
                   ADD     1           TO  IDZZ
                   MOVE    4           TO  WRK-ST
                   MOVE    ZERO        TO  WRK-IDX
                   PERFORM VARYING IDY FROM    4   BY  1
                           UNTIL   IDY     >   20
                           OR      WRK-IDX >=  2
                       IF      RECEDEN-RECEDATA (IDY:1)    =   ","
                           ADD     1           TO  WRK-IDX
                           IF      WRK-ST      <   IDY
                               COMPUTE WRK-LEN =   IDY -   WRK-ST
                               EVALUATE   WRK-IDX
                                   WHEN    1
                                       MOVE    RECEDEN-RECEDATA
                                                      (WRK-ST:WRK-LEN)
                                               TO
                                       SPA-NAI-T09-FTNJANUM  (IDX IDZZ)
                                   WHEN    2
                                       MOVE    RECEDEN-RECEDATA
                                                      (WRK-ST:WRK-LEN)
                                               TO
                                       SPA-NAI-T09-JKYSNUM   (IDX IDZZ)
                               END-EVALUATE
                           END-IF
                           COMPUTE WRK-ST  =   IDY +   1
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        ＫＯレコード以外のデータになった場合終わる
               IF      RECEDEN-RECEDATA (1:2)  =   "SY"    OR
                                                   "SI"
                   MOVE    1               TO  FLG-RECEDEN
               ELSE
                   PERFORM 900-RECEDEN-READ-N-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       420311-T09-HENSYU-KOH-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理チェック処理
      *****************************************************************
       42031-SEIKYU-CHK-SEC         SECTION.
      *
      *    直接請求する保険者とのチェック（社保）
           IF    ( SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"   )
           AND   ( SPA-NAI-T06-DIRECT-HKNJANUM
                                   NOT =   SPACE )
               EVALUATE    SPA-NAI-T06-DIRECT-HKNJANUM
                   WHEN    "00000000"
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       10
                               OR      SPA-NAI-T06-DIRECT-HKNJANUML(IDX)
                                           =   SPACE
                           IF      SPA-NAI-T09-THKNJANUM-S (1)
                                           =
                                       SPA-NAI-T06-DIRECT-HKNJANUML(IDX)
                               MOVE    "0027"              TO  SPA-ERRCD
                               GO  TO  42031-SEIKYU-CHK-EXT
                           END-IF
                       END-PERFORM
                   WHEN    OTHER
                       IF      SPA-NAI-T09-THKNJANUM-S (1)
                                           =
                                           SPA-NAI-T06-DIRECT-HKNJANUM
                           CONTINUE
                       ELSE
                           MOVE    "0027"              TO  SPA-ERRCD
                           GO  TO  42031-SEIKYU-CHK-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    レセ電請求一覧とのチェック
           IF      SPA-GMN-T09-TCHK(1) =   "1"
               MOVE    "0008"              TO  SPA-ERRCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
      *
      *    レセ電データ存在チェック
           IF      SPA-NAI-T09-TRECEDEN (1)
                                       =   "1"
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
      *
      *    平成２４年３月以前の月後れ分が改正パッチ後に再作成されているか
           IF      SPA-NAI-T09-TRECEDEN (1)
                                       =   "9"
               MOVE    "0020"              TO  SPA-ERRCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
      *
      *    返戻指示設定時に返戻データに設定されているか
           IF      SPA-NAI-T09-THENREI (1)
                                       =   "1"
               MOVE    "0011"              TO  SPA-ERRCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
      *
      *    主科未設定データ存在チェック
           IF      SPA-NAI-T09-TSYUKA (1)
                                       =   "1"
               MOVE    "0018"              TO  SPA-ERRCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
      *
      *    既に請求済かどうか
           IF      SPA-NAI-T09-TSEIKYU (1)
                                       =   "1"
               MOVE    "1002"              TO  SPA-TIDCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
      *
      *    作成日のチェック
           IF      SPA-NAI-T09-TCREYMD (1)
                                       <   SPA-NAI-T09-TUKEYMD (1)      
                MOVE    "1006"             TO  SPA-TIDCD
               GO  TO  42031-SEIKYU-CHK-EXT
         END-IF 
      *
      *    返戻年月未入力のときの返戻データ存在チェック
           IF      SPA-NAI-T09-THENREIYM (1)
                                       =   "1"
               MOVE    "1011"              TO  SPA-TIDCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
           IF      SPA-NAI-T09-THENREIYM (1)
                                       =   "2"
               MOVE    "1012"              TO  SPA-TIDCD
               GO  TO  42031-SEIKYU-CHK-EXT
           END-IF
      *
      *    追加処理
           PERFORM 410411-TUIKA-HENSYU-SEC
      *
           IF      SPA-GMN-T06-SELNUM  =   ZERO
               MOVE    SPA-GMN-T06-RECE-MAX
                                        TO  T06-RECE-LIST-ROW
           ELSE
               MOVE    SPA-GMN-T06-SELNUM
                                        TO  T06-RECE-LIST-ROW
           END-IF
      * 
      *
      *    ＨＡＯＲＩのとき
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-BODY-SELNUM
                                           SPA-T09-AREA
           END-IF
      *
           MOVE    4                TO  SPA-GMN-T06-CUR
           .
       42031-SEIKYU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       410411-TUIKA-HENSYU-SEC           SECTION.
      *
           IF      SPA-GMN-T06-SELNUM  =   ZERO
               ADD     1                   TO  SPA-GMN-T06-RECE-MAX
               MOVE    SPA-GMN-T06-RECE-MAX
                                           TO  SPA-GMN-T06-R-TRENNUM
                                                (SPA-GMN-T06-RECE-MAX)
               MOVE    SPA-GMN-T06-RECE-MAX
                                           TO  IDX
           ELSE
               MOVE    SPA-GMN-T06-SELNUM  TO  IDX
           END-IF    
      *
           MOVE    RECE10-KEY      TO  SPA-NAI-T06-RECE10-KEY (IDX)
      *   
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   MOVE    "入"        TO  SPA-GMN-T06-R-TNYUGAIKBN(IDX)
               WHEN    "2"    
                   MOVE    "外"        TO  SPA-GMN-T06-R-TNYUGAIKBN(IDX)
           END-EVALUATE
           MOVE    RECE10-PTNUM        TO  SPA-GMN-T06-R-TPTNUM    (IDX)
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID         TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-NAME          TO  SPA-GMN-T06-R-TNAME     (IDX)
           MOVE    SPA-GMN-T06-SRYYM   TO  SPA-GMN-T06-R-TSRYYM    (IDX)
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    "3"
                   MOVE    RECE10-KOFUNUM  TO  SPA-GMN-T06-R-TKIGO (IDX)
               WHEN    OTHER
                   STRING  RECE10-KIGO         DELIMITED   BY  SPACE
                           "　"                DELIMITED   BY  SIZE
                           RECE10-NUM          DELIMITED   BY  SPACE
                                       INTO    SPA-GMN-T06-R-TKIGO (IDX)
                   END-STRING
           END-EVALUATE
      *
           EVALUATE    RECE10-HKNNUM
               WHEN    "002"
               WHEN    "031"  THRU  "034"
                   EVALUATE    RECE10-HOJOKBN-KEY
                       WHEN    "1"
                       WHEN    "A"
                       WHEN    "D"
                       WHEN    "G"
                           MOVE    "職"    TO
                                           SPA-GMN-T06-R-THOJOKBN(IDX)
                       WHEN    "2"
                       WHEN    "B"
                       WHEN    "E"
                       WHEN    "H"
                           MOVE    "下"    TO
                                           SPA-GMN-T06-R-THOJOKBN(IDX)
                       WHEN    "3"
                       WHEN    "C"
                       WHEN    "F"
                       WHEN    "I"
                           MOVE    "通"    TO
                                            SPA-GMN-T06-R-THOJOKBN(IDX)
                   END-EVALUATE
           END-EVALUATE
      *
           EVALUATE    RECE10-RECEIPT-KBN (1:1)
               WHEN    "1"
                   MOVE    "老併"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
               WHEN    "2"
                   MOVE    "老健"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
               WHEN    "3"
                   MOVE    "医併"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
               WHEN    "4"
                   MOVE    "医療"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
           END-EVALUATE
      *
           EVALUATE    RECE10-ETC-YKZFTN (1)
               WHEN    0
                   CONTINUE
               WHEN    OTHER
                   MOVE    "災"          TO
                                       SPA-GMN-T06-R-TDISASTER   (IDX)
           END-EVALUATE
      *
           MOVE    RECE10-CREYMD   TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-T06-R-TCREYMD (IDX)
      *
           MOVE    RECE10-HKNNUM   TO  SPA-NAI-T06-R-THKNNUM (IDX)
      *
           MOVE    RECE10-HENREI-SIJI-FLG
                                   TO  SPA-NAI-T06-R-THENREI-SIJI-FLG
                                                             (IDX)
           IF      RECE10-TEISYUTUSAKI =   "3"
               MOVE    RECE10-SYOKAIFLG    TO
                                       SPA-NAI-T06-R-TSYOKAIFLG  (IDX)
               MOVE    RECE10-HKNJANUM-S   TO
                                       SPA-NAI-T06-R-THKNJANUM-S (IDX)
           END-IF
      *
           .
       410411-TUIKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト検索画面編集処理
      *****************************************************************
       42032-T09-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-T06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGT09"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-T06
           MOVE    SPA-WORK            TO  SPA-T01KYOUTU
      *
           MOVE    "SELNUM01"           TO  MCP-WIDGET
      *
           MOVE    "T06"                TO  SPA-MOTOPG
           MOVE    "T09"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T09"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       42032-T09-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           IF    ( SPA-GMN-T06-SELNUM      >   ZERO  )
           OR    ( SPA-GMN-T06-PTNUM   NOT =   SPACE )
               MOVE    "0022"             TO  SPA-ERRCD
               MOVE    3                  TO  SPA-GMN-T06-CUR
               GO  TO  4101-KAKUTEI-CHK-EXT
           END-IF
      * 
           IF      SPA-GMN-T06-RECE-MAX   =   ZERO
               MOVE    "0026"             TO  SPA-ERRCD
               MOVE    4                  TO  SPA-GMN-T06-CUR
           END-IF
      *
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T06-RECE-MAX
                   OR      WRK-RENNUM  >   ZERO
               IF      SPA-GMN-T06-R-TERR  (IDX)
                                           =   SPACE
                   ADD     1                   TO  WRK-RENNUM
               END-IF
           END-PERFORM
           IF      WRK-RENNUM         =   ZERO
               MOVE    "0026"             TO  SPA-ERRCD
               MOVE    4                  TO  SPA-GMN-T06-CUR
           END-IF
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る前チェック処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    未処理分があるときのチェック 
           IF    ( SPA-NAI-T06-SYORIFLG
                                       =   ZERO )
           AND   ( SPA-GMN-T06-RECE-MAX
                                       >   ZERO )
               IF      SPA-NAI-T06-SYSYM-2 >=  SPA-NAI-T06-SRYYM-H
                   CONTINUE
               ELSE
                   MOVE    "1008"          TO  SPA-TIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "1005"          TO  SPA-TIDCD
               GO  TO  210-BACK-EXT
           END-IF
      *    
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "6666"          TO  SPA-TIDCD
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
           MOVE    SPACE           TO  SPA-TIDCD
           MOVE    SPACE           TO  SPA-TID1-FLG
      *
           MOVE    "T01"           TO  SPA-SAKIPG
           MOVE    "T06"           TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除ガイドメッセージ処理
      *****************************************************************
       440-SAKUJYO-GID-SEC             SECTION.
      *
           IF      SPA-GMN-T06-SELNUM  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
           MOVE    "1007"             TO  SPA-TIDCD
          .
       440-SAKUJYO-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       440-SAKUJYO-SEC             SECTION.
      *
           MOVE    SPA-GMN-T06-SELNUM  TO  IDX
           INITIALIZE              SPA-GMN-T06-RECE-G (IDX)
           INITIALIZE              SPA-NAI-T06-RECE-G (IDX)
           IF      SPA-GMN-T06-RECE-MAX
                                       =   1
               MOVE    ZERO                TO  SPA-GMN-T06-RECE-MAX
           ELSE
               PERFORM VARYING IDXX    FROM    IDX     BY  1
                       UNTIL   IDXX    >       SPA-GMN-T06-RECE-MAX
                   IF      IDXX            =   SPA-GMN-T06-RECE-MAX
                       INITIALIZE          SPA-GMN-T06-RECE-G (IDXX)
                       INITIALIZE          SPA-NAI-T06-RECE-G (IDXX)
                   ELSE 
                       COMPUTE IDXY    =   IDXX    +   1
                       MOVE    SPA-GMN-T06-RECE-G (IDXY)
                                           TO  SPA-GMN-T06-RECE-G(IDXX)
                       MOVE    SPA-NAI-T06-RECE-G (IDXY)
                                           TO  SPA-NAI-T06-RECE-G(IDXX)
                       MOVE    IDXX        TO  SPA-GMN-T06-R-TRENNUM
                                                                 (IDXX)
                   END-IF
               END-PERFORM
               COMPUTE SPA-GMN-T06-RECE-MAX
                           =   SPA-GMN-T06-RECE-MAX    -   1
           END-IF    
      *
           IF      SPA-GMN-T06-RECE-MAX
                                       =   ZERO
               MOVE    ZERO                TO  FLG-RECE-ROW
           ELSE
               COMPUTE IDX     =   SPA-GMN-T06-SELNUM  -   1
               MOVE    IDX                 TO  T06-RECE-LIST-ROW
           END-IF
      *
           INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
      *
           MOVE    3                   TO  SPA-GMN-T06-CUR
      *
          .
       440-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T06             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T06"               TO  SPA-MOTOPG
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
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア確認処理
      *****************************************************************
       460-CLEAR-TID-SEC             SECTION.
      *
           MOVE    "1003"             TO  SPA-TIDCD
          .
       460-CLEAR-TID-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       460-CLEAR-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-BODY-SELNUM
      *
           MOVE    3               TO  SPA-GMN-T06-CUR
      *
          .
       460-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回作成分表示確認処理
      *****************************************************************
       480-ZENKAI-HYOUJI-TID-SEC      SECTION.
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO  TO  480-ZENKAI-HYOUJI-TID-EXT
           END-IF
      *
           INITIALIZE                  ONRECE-B-REC
           MOVE   SPA-HOSPNUM      TO  ONRECE-B-HOSPNUM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                   =   "T"
               MOVE   "1"              TO  ONRECE-B-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
                   MOVE   "3"              TO  ONRECE-B-TEISYUTUSAKI
               ELSE
                   MOVE   "2"              TO  ONRECE-B-TEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-B-SRYYM
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
      * 
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-ONRECE-B    =   1
               MOVE    "0025"          TO  SPA-ERRCD
               GO  TO  480-ZENKAI-HYOUJI-TID-EXT
           ELSE
      *        前回が連携用のとき
               IF      ONRECE-B-U-HENREI-FLG
                                       =   "9"
                   MOVE    "0029"          TO  SPA-ERRCD
                   GO  TO  480-ZENKAI-HYOUJI-TID-EXT
               END-IF
      *        期間指定時に前回の期間が違うとき
               IF    ( SPA-GMN-T06-SRYYMKBN-MAX
                                       >   ZERO            )
               AND   ( SPA-T01-PRTKBN  NOT =   "6" AND "7" )
                   IF    ( ONRECE-B-SRYYM-B
                                   <   SPA-NAI-T06-SRYYMKBN-ST
                                       (SPA-NAI-T06-SRYYMKBN-SELNUM) )
                   OR    ( ONRECE-B-SRYYM-B
                                   >   SPA-NAI-T06-SRYYMKBN-ED
                                       (SPA-NAI-T06-SRYYMKBN-SELNUM) )
                       MOVE    "0031"              TO  SPA-ERRCD
                       GO  TO  480-ZENKAI-HYOUJI-TID-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    直接請求する保険者とのチェック（社保）
           IF    ( SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"   )
           AND   ( SPA-NAI-T06-DIRECT-HKNJANUM
                                   NOT =   SPACE )
               EVALUATE    SPA-NAI-T06-DIRECT-HKNJANUM
                   WHEN    "00000000"
                       PERFORM VARYING IDYY    FROM    1   BY  1
                               UNTIL   IDYY    >       10
                               OR      SPA-NAI-T06-DIRECT-HKNJANUML
                                                               (IDYY)
                                               =   SPACE
                           IF      ONRECE-B-HKNJANUM   =
                                       SPA-NAI-T06-DIRECT-HKNJANUML
                                                               (IDYY)
                                   OR  SPA-NAI-T06-DIRECT-HKNJANUM-63L
                                                               (IDYY)
                               MOVE    "0027"              TO  SPA-ERRCD
                               GO  TO  480-ZENKAI-HYOUJI-TID-EXT
                           END-IF
                       END-PERFORM
                   WHEN    OTHER
                       IF      ONRECE-B-HKNJANUM   =
                                       SPA-NAI-T06-DIRECT-HKNJANUM
                                   OR  SPA-NAI-T06-DIRECT-HKNJANUM-63
                           CONTINUE
                       ELSE
                           MOVE    "0027"              TO  SPA-ERRCD
                           GO  TO  480-ZENKAI-HYOUJI-TID-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    未処理分があるときのチェック 
           IF    ( SPA-NAI-T06-SYORIFLG
                                   =   ZERO )
           AND   ( SPA-GMN-T06-RECE-MAX
                                   >   ZERO )
               MOVE    "1010"          TO  SPA-TIDCD
           ELSE
               MOVE    "1004"          TO  SPA-TIDCD
           END-IF
           .
       480-ZENKAI-HYOUJI-TID-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回作成分表示処理
      *****************************************************************
       480-ZENKAI-HYOUJI-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-T06-SELNUM
      *
           INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
                                       SPA-GMN-T06-RECE-TBL
                                       SPA-NAI-T06-RECE-TBL
      *
           INITIALIZE                  ONRECE-B-REC
           MOVE   SPA-HOSPNUM      TO  ONRECE-B-HOSPNUM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                   =   "T"
               MOVE   "1"              TO  ONRECE-B-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
                   MOVE   "3"              TO  ONRECE-B-TEISYUTUSAKI
               ELSE
                   MOVE   "2"              TO  ONRECE-B-TEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-B-SRYYM
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
           MOVE    ONRECE-B-RENNUM TO  WRK-RENNUM
      *
           PERFORM         UNTIL   FLG-ONRECE-B    =   1
                           OR      ONRECE-B-RENNUM 
                                               NOT =   WRK-RENNUM    
               ADD     1                TO  SPA-GMN-T06-RECE-MAX
               MOVE    SPA-GMN-T06-RECE-MAX
                                        TO  SPA-GMN-T06-R-TRENNUM
                                                (SPA-GMN-T06-RECE-MAX)
               MOVE    SPA-GMN-T06-RECE-MAX
                                        TO  IDX
      *
               MOVE    ONRECE-B-HOSPNUM TO  SPA-NAI-T06-RECE10-HOSPNUM
                                                               (IDX)
               MOVE    ONRECE-B-SRYYM-B TO  SPA-NAI-T06-RECE10-SRYYM
                                                               (IDX)
               MOVE    ONRECE-B-TEISYUTUSAKI-B
                                       TO
                                       SPA-NAI-T06-RECE10-TEISYUTUSAKI
                                                               (IDX)
               MOVE    ONRECE-B-NYUGAIKBN
                                       TO  SPA-NAI-T06-RECE10-NYUGAIKBN
                                                               (IDX)
               MOVE    ONRECE-B-PTID   TO  SPA-NAI-T06-RECE10-PTID
                                                               (IDX)
               MOVE    ONRECE-B-RECESYUBETU
                                       TO
                                       SPA-NAI-T06-RECE10-RECESYUBETU
                                                               (IDX)
               MOVE    ONRECE-B-HKNJANUM
                                       TO  SPA-NAI-T06-RECE10-HKNJANUM
                                                               (IDX)
               MOVE    ONRECE-B-RECEKA TO  SPA-NAI-T06-RECE10-RECEKA 
                                                               (IDX)
               MOVE    ONRECE-B-HOJOKBN
                                       TO  SPA-NAI-T06-RECE10-HOJOKBN
                                                               (IDX)
               MOVE    ONRECE-B-KOHID  TO  SPA-NAI-T06-RECE10-KOHID-KEY
                                                               (IDX)
               MOVE    ONRECE-B-TEKSTYMD
                                       TO  SPA-NAI-T06-RECE10-TEKSTYMD
                                                               (IDX)
               MOVE    ONRECE-B-RECEIPT-KBN
                                       TO
                                       SPA-NAI-T06-RECE10-RECEIPT-KBN
                                                               (IDX)
      *
      *        請求管理チェック
               MOVE    SPA-NAI-T06-RECE10-KEY (IDX)
                                       TO  RECE10-KEY      
               MOVE    RECE10-REC      TO  MCPDATA-REC
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-SEIKYU-READ-SEC
               END-IF
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
               IF      FLG-SEIKYU      =   1
                   MOVE    "1"             TO  SPA-GMN-T06-R-TERR  (IDX)
               ELSE
      *          災害対応（未確定分をレセ電データに記録するか）
                 IF    (  SPA-T06-2005-RECEDEN-DISASTER
                                               =   "0"              )
                 AND   ( RECE10-ETC-YKZFTN (1)
                                               =   6   OR  7  OR  8 )
                     MOVE    "5"           TO  SPA-GMN-T06-R-TERR  (IDX)
                 ELSE
                   IF    ( RECE10-SKYKBN   NOT =   "0"                 )
                   AND   ( RECE10-SKYYM    =   "000000"
                                       OR  SPA-NAI-T06-SRYYM-H         )
                   AND   (   ( RECE10-HENREIYM     =   "000000"      )
                          OR ( RECE10-HENREIYM NOT =   "000000"  AND
                               RECE10-HENREI-SIJI-FLG
                                                   =   1             ) )
      *                レセ電データ存在チェック
                       MOVE    SPACE               TO  RECEDEN-REC
                       INITIALIZE                      RECEDEN-REC
                       MOVE    RECE10-KEY          TO  RECEDEN-KEY
                       MOVE    "1"                 TO  RECEDEN-RECKBN
                       MOVE    RECEDEN-REC         TO  MCPDATA-REC
                       PERFORM 900-RECEDEN-READ-SEC
                       IF      FLG-RECEDEN         =   1
                           MOVE    "2"     TO  SPA-GMN-T06-R-TERR  (IDX)
                       ELSE
      *                    平成２４年３月以前の月後れ分が改正パッチ前の作成か
                           IF    ( SPA-NAI-T06-SRYYM-H
                                                       >   201203     )
                           AND   ( RECE10-SRYYM        <=  201203     )
                           AND   ( RECE10-CREYMD       <   "20120423" )
                               MOVE    "6"                 TO
                                               SPA-GMN-T06-R-TERR  (IDX)
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "3"     TO  SPA-GMN-T06-R-TERR  (IDX)
                   END-IF
                 END-IF  
               END-IF
      *   
               EVALUATE    ONRECE-B-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"    TO  SPA-GMN-T06-R-TNYUGAIKBN(IDX)
                   WHEN    "2"    
                       MOVE    "外"    TO  SPA-GMN-T06-R-TNYUGAIKBN(IDX)
               END-EVALUATE
      *
               MOVE    ONRECE-B-PTNUM  TO  SPA-GMN-T06-R-TPTNUM    (IDX)
      *
               INITIALIZE                  PTINF-REC
               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    ONRECE-B-PTID   TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       =   ZERO
                   MOVE    PTINF-NAME      TO  SPA-GMN-T06-R-TNAME (IDX)
               ELSE
                   MOVE    "4"             TO  SPA-GMN-T06-R-TERR  (IDX)
               END-IF
      *
               MOVE    ONRECE-B-SRYYM-B
                                       TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2) 
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-T06-R-TSRYYM (IDX)
      *
      ***      IF    ( SPA-GMN-T06-SRYYMKBN-MAX
      ***                              >   ZERO            )
      ***      AND   ( SPA-T01-PRTKBN  NOT =   "6" AND "7" )
      ***          IF    ( ONRECE-B-SRYYM-B
      ***                              <   SPA-NAI-T06-SRYYMKBN-ST
      ***                              (SPA-NAI-T06-SRYYMKBN-SELNUM) )
      ***          OR    ( ONRECE-B-SRYYM-B
      ***                              >   SPA-NAI-T06-SRYYMKBN-ED
      ***                              (SPA-NAI-T06-SRYYMKBN-SELNUM) )
      ***              MOVE    "7"         TO  SPA-GMN-T06-R-TERR  (IDX)
      ***          END-IF
      ***      END-IF
      *
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    "3"
                       MOVE    RECE10-KOFUNUM  TO
                                               SPA-GMN-T06-R-TKIGO (IDX)
                   WHEN    OTHER
                       IF      RECE10-HKNNUM   =   SPACE
                           MOVE    ONRECE-B-KIGO   TO
                                               SPA-GMN-T06-R-TKIGO (IDX)
                       ELSE
                           STRING  RECE10-KIGO     DELIMITED   BY  SPACE
                                   "　"            DELIMITED   BY  SIZE
                                   RECE10-NUM      DELIMITED   BY  SPACE
                                       INTO    SPA-GMN-T06-R-TKIGO (IDX)
                           END-STRING
                      END-IF
               END-EVALUATE
      *
               EVALUATE    RECE10-HKNNUM
                   WHEN    "002"
                   WHEN    "031"  THRU  "034"
                       EVALUATE    RECE10-HOJOKBN-KEY
                           WHEN    "1"
                           WHEN    "A"
                           WHEN    "D"
                           WHEN    "G"
                               MOVE    "職"    TO
                                           SPA-GMN-T06-R-THOJOKBN(IDX)
                           WHEN    "2"
                           WHEN    "B"
                           WHEN    "E"
                           WHEN    "H"
                              MOVE    "下"    TO
                                           SPA-GMN-T06-R-THOJOKBN(IDX)
                           WHEN    "3"
                           WHEN    "C"
                           WHEN    "F"
                           WHEN    "I"
                               MOVE    "通"    TO
                                            SPA-GMN-T06-R-THOJOKBN(IDX)
                       END-EVALUATE
               END-EVALUATE
      *
               EVALUATE    RECE10-RECEIPT-KBN (1:1)
                    WHEN    "1"
                       MOVE    "老併"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
                   WHEN    "2"
                       MOVE    "老健"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
                   WHEN    "3"
                       MOVE    "医併"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
                   WHEN    "4"
                       MOVE    "医療"          TO
                                       SPA-GMN-T06-R-TRECEIPT-KBN(IDX)
               END-EVALUATE
      *
               EVALUATE    RECE10-ETC-YKZFTN (1)
                   WHEN    0
                       CONTINUE
                  WHEN    OTHER
                       MOVE    "災"          TO
                                       SPA-GMN-T06-R-TDISASTER   (IDX)
           END-EVALUATE
      *
               MOVE    RECE10-CREYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-T06-R-TCREYMD (IDX)
      *
               MOVE    RECE10-HKNNUM   TO  SPA-NAI-T06-R-THKNNUM (IDX)
      *
               MOVE    RECE10-HENREI-SIJI-FLG
                                       TO
                                       SPA-NAI-T06-R-THENREI-SIJI-FLG
                                                                 (IDX)
               IF      RECE10-TEISYUTUSAKI =   "3"
                   MOVE    RECE10-SYOKAIFLG    TO
                                       SPA-NAI-T06-R-TSYOKAIFLG  (IDX)
                   MOVE    RECE10-HKNJANUM-S   TO
                                       SPA-NAI-T06-R-THKNJANUM-S (IDX)
               END-IF
      *
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM     
      * 
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    4               TO  SPA-GMN-T06-CUR
           .
       480-ZENKAI-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電作成処理
      *****************************************************************
       400-RECEDEN-SYORI-SEC                SECTION.
      *
      *    確定ボタン押下時チェック処理
           PERFORM 4101-KAKUTEI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  400-RECEDEN-SYORI-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  400-RECEDEN-SYORI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-TID1-FLG
      *    
      *    レセ電作成指示画面へ
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-T06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGT98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-T06
           MOVE    SPA-WORK            TO  SPA-T01KYOUTU
      *
           MOVE    "PRTKBN"            TO  MCP-WIDGET
      *
           MOVE    "T06"               TO  SPA-MOTOPG
           MOVE    "T98"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "T98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       400-RECEDEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
      *    ジョブ管理チェック        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-T06-SRYYM-H TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
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
      *
      *    ジョブ管理チェック（レセプト）
           IF      SPA-ERRCD           =   SPACE         
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CONS-JOB-JOBID-RECEPT1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID-RECEPT1
                                       TO  JOB-SHELLID
               PERFORM 900-CALL-ORCSJOB-SEC
              IF      SJOBKANRI-RETURN
                                       =   ZERO
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "7777"          TO  SPA-ERRCD
                       MOVE    SPACE             TO  WRK-RECEERR
                       STRING  "レセプト作成処理中です【"
                                                 DELIMITED  BY  SIZE
                               JOB-SHELLMSG      DELIMITED  BY  SPACE
                               "】"              DELIMITED  BY  SIZE      
                                                 INTO  WRK-RECEERR
                       END-STRING
                   END-IF
               END-IF    
           END-IF    
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電出力処理
      *****************************************************************
       460-RECEDEN-SEC                 SECTION.
      *
           INITIALIZE                  SPA-T01-TBL-GROUP
           MOVE    ZERO            TO  SPA-T01-TIMER-FLG
      *
      *    ＨＡＯＲＩのときＭＯＮＢＬＯＢ−ＩＤを設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  460-RECEDEN-EXT
               END-IF
               MOVE    MONBLOB-ID          TO  SPA-T01-MONBLOB-DATA-ID
      *
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  460-RECEDEN-EXT
               END-IF
               MOVE    MONBLOB-ID          TO  SPA-T01-MONBLOB-PRINT-ID
           END-IF
      *
      *    クライアント保存削除処理             
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "D"             TO  SFILESV-MODE
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  SFILESV-SRYYM   (1)
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                   TO  SFILESV-SHELLID (1)
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF             
      *
      *    パラメタＤＢ削除処理
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 900-DBDELETE-SEC
           IF      MCP-RC          >   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    レセ電用パラメタＤＢ作成処理
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    1                   TO  BTPARA-JOBID
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                       TO  BTPARA-SCRIPTID
           INITIALIZE                      WRK-BTPARA-INFO-PARA
           MOVE    "/tmp/"             TO  WRK-BTPARA-DATA
           MOVE    "RECEDEN"           TO  WRK-BTPARA-FILENM
           MOVE    SPA-GMN-T98-DATAKBN TO  WRK-BTPARA-DATAKBN
           MOVE    WRK-BTPARA-INFO-PARA
                                       TO  BTPARA-INFO-PARA
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBINSERT-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    "1002"          TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF
      *
      *    個別作成レセ電データ更新処理
      *
           INITIALIZE                  ONRECE-K-REC
           MOVE   SPA-HOSPNUM      TO  ONRECE-K-HOSPNUM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                   =   "T"
               MOVE   "1"              TO  ONRECE-K-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
                   MOVE   "3"              TO  ONRECE-K-TEISYUTUSAKI
               ELSE
                   MOVE   "2"              TO  ONRECE-K-TEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-K-SRYYM
           MOVE    ZERO            TO  ONRECE-K-UCNT
                                       ONRECE-K-SCNT
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-KANRI-SELECT-SEC
      * 
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-ONRECE-K    =   1
               INITIALIZE                  ONRECE-K-REC
               MOVE   SPA-HOSPNUM      TO  ONRECE-K-HOSPNUM
               IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
                   MOVE   "1"              TO  ONRECE-K-TEISYUTUSAKI
               ELSE
                   IF      SPA-GMN-T06-TEISYUTUSAKI4
                                           =   "T"
                       MOVE   "3"              TO  ONRECE-K-TEISYUTUSAKI
                   ELSE
                       MOVE   "2"              TO  ONRECE-K-TEISYUTUSAKI
                   END-IF
               END-IF
               MOVE    SPA-NAI-T06-SRYYM-H
                                       TO  ONRECE-K-SRYYM
               MOVE    ZERO            TO  ONRECE-K-UCNT
                                           ONRECE-K-SCNT
               MOVE    ZERO            TO  ONRECE-K-RENNUM
           END-IF
      *
           IF      ONRECE-K-RENNUM     =   999
               MOVE    ZERO                TO  ONRECE-K-RENNUM
           END-IF
      *
           ADD     1               TO  ONRECE-K-RENNUM
           MOVE    ONRECE-K-REC    TO  WRK-ONRECE-K-REC
      *
      *    次の連番のデータが存在するとき
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
           PERFORM 900-ONRECE-KANRI-SELECT-SEC
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-ONRECE-K    =   ZERO
               MOVE    "1006"          TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF
      *
           MOVE    WRK-ONRECE-K-REC    TO  ONRECE-K-REC
           MOVE    SPA-NAI-T06-SKYYMD-H
                                       TO  ONRECE-K-SKYYMD
           MOVE    SPA-GMN-T06-RECE-MAX
                                       TO  ONRECE-K-SEND-TOTAL-CNT
           MOVE    SMCNDATE-YMD        TO  ONRECE-K-CREYMD
           MOVE    SPA-TERMID          TO  ONRECE-K-TERMID
           MOVE    SPA-OPID            TO  ONRECE-K-OPID
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
           MOVE    "tbl_onrece_kanri"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBINSERT-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    "1009"          TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T06-RECE-MAX
             IF      SPA-GMN-T06-R-TERR  (IDX)
                                           =   SPACE
               ADD     1                   TO  WRK-RENNUM
               INITIALIZE                      ONRECE-B-REC
               MOVE    SPA-HOSPNUM         TO  ONRECE-B-HOSPNUM
               MOVE    ONRECE-K-TEISYUTUSAKI
                                           TO  ONRECE-B-TEISYUTUSAKI
               MOVE    ONRECE-K-SRYYM      TO  ONRECE-B-SRYYM
               MOVE    ONRECE-K-UCNT       TO  ONRECE-B-UCNT
               MOVE    ONRECE-K-SCNT       TO  ONRECE-B-SCNT
               MOVE    ONRECE-K-RENNUM     TO  ONRECE-B-RENNUM               
               MOVE    WRK-RENNUM          TO  ONRECE-B-RECEIPT-NO
               MOVE    SPA-NAI-T06-RECE10-SRYYM       (IDX)
                                           TO  ONRECE-B-SRYYM-B
               MOVE    SPA-NAI-T06-RECE10-TEISYUTUSAKI(IDX)
                                           TO  ONRECE-B-TEISYUTUSAKI-B
               MOVE    SPA-NAI-T06-RECE10-NYUGAIKBN   (IDX)
                                           TO  ONRECE-B-NYUGAIKBN
               MOVE    SPA-NAI-T06-RECE10-PTID        (IDX)
                                           TO  ONRECE-B-PTID
               MOVE    SPA-NAI-T06-RECE10-RECESYUBETU (IDX)
                                           TO  ONRECE-B-RECESYUBETU
               MOVE    SPA-NAI-T06-RECE10-HKNJANUM    (IDX)
                                           TO  ONRECE-B-HKNJANUM
               MOVE    SPA-NAI-T06-RECE10-RECEKA      (IDX)
                                           TO  ONRECE-B-RECEKA
               MOVE    SPA-NAI-T06-RECE10-HOJOKBN     (IDX)
                                           TO  ONRECE-B-HOJOKBN
               MOVE    SPA-NAI-T06-RECE10-KOHID-KEY   (IDX)
                                           TO  ONRECE-B-KOHID
               MOVE    SPA-NAI-T06-RECE10-TEKSTYMD    (IDX)
                                           TO  ONRECE-B-TEKSTYMD
               MOVE    SPA-NAI-T06-RECE10-RECEIPT-KBN (IDX)
                                           TO  ONRECE-B-RECEIPT-KBN
               MOVE    SPA-GMN-T06-R-TPTNUM           (IDX)
                                           TO  ONRECE-B-PTNUM
               MOVE    SPA-GMN-T06-R-TNAME            (IDX)
                                           TO  ONRECE-B-NAME
               MOVE    SPA-GMN-T06-R-TKIGO            (IDX)
                                           TO  ONRECE-B-KIGO
               MOVE    SPA-NAI-T06-R-THKNNUM      (IDX)
                                           TO  ONRECE-B-HKNNUM
               IF      SPA-T01-PRTKBN      =   "6" OR  "7"
      *            連携用のとき識別のためにセット
                   MOVE    "9"             TO  ONRECE-B-U-HENREI-FLG
               ELSE
                   IF      SPA-NAI-T06-R-THENREI-SIJI-FLG (IDX)
                                               =   1
                       MOVE    "1"             TO  ONRECE-B-U-HENREI-FLG
                  END-IF
               END-IF
               IF      ONRECE-B-TEISYUTUSAKI-B
                                           =   "3"
                   MOVE    SPA-NAI-T06-R-TSYOKAIFLG   (IDX)
                                               TO  ONRECE-B-U-YOBI1
                   MOVE    SPA-NAI-T06-R-THKNJANUM-S  (IDX)
                                               TO  ONRECE-B-U-YOBI2
               END-IF
               MOVE    SMCNDATE-YMD        TO  ONRECE-B-CREYMD
               MOVE    SPA-TERMID          TO  ONRECE-B-TERMID
               MOVE    SPA-OPID            TO  ONRECE-B-OPID
               MOVE    ONRECE-B-REC        TO  MCPDATA-REC
               MOVE    "tbl_onrece_body"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBINSERT-SEC
               IF      MCP-RC      NOT =   ZERO
                   CALL    "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
                   MOVE    "1010"          TO  SPA-ERRCD
                   GO  TO  460-RECEDEN-EXT
               END-IF
             END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
           MOVE    SPACE               TO  SYS-2005-REC 
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SPA-NAI-T06-SRYYM-H TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                   SYS-2005-REC
           END-IF
           IF      SYS-2005-RECEDENOUT =   SPACE
               MOVE    "/var/tmp/"     TO  SYS-2005-RECEDENOUT
           END-IF    
      *
      *    印刷ＤＢのＫＥＹ用     
           MOVE    SMCNDATE-YMD    TO  WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS    TO  WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  SPA-T01-TBL-GROUP
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           EVALUATE    ONRECE-K-TEISYUTUSAKI
               WHEN    "1"
               WHEN    "2"
                   MOVE    "recept4_kobetu.sh"
                                           TO  SHELLTBL-NAME
               WHEN    "3"
                   MOVE    "recept41_kobetu.sh"
                                           TO  SHELLTBL-NAME
           END-EVALUATE   
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           MOVE    1               TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-T06-SKYYMD-H
                                   TO  LNK-PRTKANRI-SKYYMD
           EVALUATE    ONRECE-K-TEISYUTUSAKI
               WHEN    "1"
               WHEN    "2"
                   MOVE    "recept4_kobetu.sh"
                                           TO  LNK-PRTKANRI-SHELLID
               WHEN    "3"
                   MOVE    "recept41_kobetu.sh"
                                           TO  LNK-PRTKANRI-SHELLID
           END-EVALUATE
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SPA-NAI-T06-1031-PRTNM
                                   TO  LNK-PRTKANRI-PRTNM
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
           MOVE    ONRECE-K-TEISYUTUSAKI
                                   TO  SHELLTBL-ARG12
           MOVE    ONRECE-K-RENNUM TO  SHELLTBL-ARG13
      *               
           IF      SPA-GMN-T98-DATAKBN  =  "5" OR  "6"
               MOVE    "/tmp/"     TO  SYS-2005-RECEDENOUT
           END-IF
           STRING  SYS-2005-RECEDENOUT DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                               INTO    SHELLTBL-ARG17
           END-STRING
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG18
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG19         
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG20
      *    ＨＡＯＲＩのときチェック用は「８」それ以外は「９」を設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    SPA-T01-JOB-DATAKBN
                                       TO  SHELLTBL-ARG21
           ELSE
               MOVE    SPA-GMN-T98-DATAKBN
                                       TO  SHELLTBL-ARG21
           END-IF
      *ipアドレス取得
      *****PERFORM 900-SYSTEM-GET-SEC
      *****MOVE    SLDUSER-HOST(1) TO  SHELLTBL-ARG22
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG23
           MOVE    SPA-GMN-T98-PRTKBN
                                   TO  SHELLTBL-ARG24
           MOVE    SPA-T06-2005-RECEDEN-LST1KBN
                                   TO  SHELLTBL-ARG25
           MOVE    SPA-T06-2005-RECEDEN-RIREKIKBN
                                   TO  SHELLTBL-ARG26
           MOVE    "99999999"      TO  SHELLTBL-ARG27
           IF      SPA-NAI-T06-DIRECT-HKNJANUM
                                   NOT =   SPACE
               MOVE    SPA-NAI-T06-DIRECT-HKNJANUM
                                       TO  SHELLTBL-ARG27
           END-IF
           IF      SPA-NAI-T06-SRYYM-H
                                   <   201204
               MOVE    "0"             TO  SHELLTBL-ARG28
           ELSE
               MOVE    SPA-T06-2005-RECEDEN-CHK-KBN
                                       TO  SHELLTBL-ARG28
           END-IF
           MOVE    SPA-T01-NYUGAIKBN
                                   TO  SHELLTBL-ARG29
      *
      *    連携基盤用の時再設定
           IF      SPA-T01-PRTKBN  =   "6" OR  "7"
               MOVE    SPACE           TO  SHELLTBL-ARG17 
               STRING  SPA-T01-9101-FILE-DIR   DELIMITED  BY  SPACE
                       "/"                     DELIMITED  BY  SIZE
                                       INTO    SHELLTBL-ARG17
               END-STRING
               MOVE    "0"             TO  SHELLTBL-ARG25
               MOVE    "0"             TO  SHELLTBL-ARG26
               MOVE    "0"             TO  SHELLTBL-ARG28
           END-IF
      *
           IF      SPA-GMN-T06-SRYYMKBN-MAX
                                   =   ZERO
               MOVE    "999999"        TO  SHELLTBL-ARG30
               MOVE    "999999"        TO  SHELLTBL-ARG31
           ELSE
               MOVE    SPA-NAI-T06-SRYYMKBN-ST
                                           (SPA-NAI-T06-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG30
               MOVE    SPA-NAI-T06-SRYYMKBN-ED
                                           (SPA-NAI-T06-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG31
           END-IF
      *
      *    ＨＡＯＲＩのときＭＯＮＢＬＯＢ−ＩＤを設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    SPA-T01-MONBLOB-DATA-ID
                                           TO  SHELLTBL-ARG32
           END-IF
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
           IF    ( SPA-GMN-T98-DATAKBN =   "5" OR  "6" )
               MOVE    "SV"            TO  SPA-TID1-FLG
           END-IF
      *
           MOVE    1                   TO  SPA-T01-WRNO
           MOVE    1                   TO  SPA-T01-SYORIFLG
           MOVE    LNK-PRTKANRI-AREA   TO  SPA-LNK-PRTKANRI-AREA
      * 
      *****MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    WRK-CONS-JOB-JOBID  TO  SPA-T01-JOB-JOBID
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
      *
           .
       460-RECEDEN-EXT.
           EXIT.
      *
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
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-T01-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-T01-JOB-ORCA-UID
                                   TO  JOB-ORCA-UID
           PERFORM 900-CALL-ORCSJOB-SEC
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
           MOVE    SPA-NAI-T06-SRYYM-H TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
           ELSE       
               MOVE    SPACE               TO  SYS-3003-REC 
               INITIALIZE                      SYS-3003-REC
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               MOVE    ZERO                TO  SYS-3003-STYUKYMD
               MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           END-IF    
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
               PERFORM 900-DBUPDATE-SEC
           ELSE
               PERFORM 900-DBINSERT-SEC
           END-IF         
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
      *    印刷処理
      *****************************************************************
       45013-PRINT-SYORI-SEC           SECTION.
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    99999               TO  WRK-EDPAGE
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
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
               EVALUATE    SPA-TID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
      **       WHEN    "NO"
      **           MOVE    "3"             TO  SHELLTBL-ARG17
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
grpsys     MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           .
       45013-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-PRT-PAGE
                                           WRK-PRT-PAGE1
                                           WRK-PRT-PAGE2
      *
           INITIALIZE                      T99
           MOVE    ZERO                TO  T99-COUNT
           MOVE    "black"             TO  T99-MSG-STYLE
      *
           MOVE    5                   TO  T99-DURATION
           MOVE    WIDGET-INSENSITIVE  TO  T99-B12-STATE
           IF    ( SPA-TID1-FLG    =   "PV" ) 
           OR    ( SPA-RECEDEN-FLG =   "PV" ) 
               IF      SPA-T99-MOTOPG  NOT =   "T04"
                   MOVE    "F12:プレビュー"    TO  T99-B12-LABEL
               END-IF
           END-IF 
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-T06-SRYYM-H TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-JOBID       TO  T99-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  T99-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-ENDTIME  (IDX)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  T99-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  T99-ERRMSG   (IDX)
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  T99-MSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               IF      JOB-PGID        =   "ORCBPRT1"
                                   CONTINUE
                               ELSE
      *                            レセ電データチェックリストの有無
                                   IF      JOB-UPDCNT1 >   ZERO
                                       MOVE
                                   "【チェックリストが作成されました】"
                                                 TO  T99-ERRMSG   (IDX)
                                   END-IF  
                               END-IF
                           ELSE
                               MOVE    SPACE       TO  T99-MSG
                               PERFORM 4901-JOB-ERRSET-SEC
                               IF      T99-MSG     =   SPACE
                                 STRING  JOB-YOBI    DELIMITED BY SPACE
                                         "【"        DELIMITED BY SIZE
                                        JOB-SHELLMSG DELIMITED BY SPACE
                                        "】"         DELIMITED BY SIZE
                                                     INTO  T99-MSG
                                 END-STRING   
                               END-IF
                           END-IF                          
                       END-IF    
                       MOVE    IDX                     TO  T99-COUNT    
                       IF    ( JOB-PGID        =   "ORCBPRT1" )
                           MOVE    JOB-UPDCNT  TO  WRK-PRT-PAGE
                       ELSE
      *                    レセ電データチェックリストの枚数
                           MOVE    JOB-UPDCNT1 TO  WRK-PRT-PAGE1
      *                    レセ電データのレセプト件数
                           MOVE    JOB-UPDCNT2 TO  WRK-PRT-PAGE2
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           IF      T99-MSG              =   SPACE
               MOVE    "B01"                TO  MCP-WIDGET
               IF      T99-COUNT            >   ZERO    
      *            メッセージ編集処理
                   PERFORM 4900-MSG-HENSYU-SEC
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  T99-MSG
               END-IF                                 
               MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
           ELSE                                        
               MOVE    "B11"               TO  MCP-WIDGET
               MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
           END-IF                                 
      *
      *
           MOVE    "T06"                TO  SPA-MOTOPG
           MOVE    "T99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
           EXIT.
       *
      *****************************************************************
      *    メッセージ編集処理
      *****************************************************************
       4900-MSG-HENSYU-SEC         SECTION.
      *
           IF    ( WRK-PRT-PAGE1   =   ZERO )
           AND   ( WRK-PRT-PAGE2   =   ZERO )
               MOVE    WRK-T99-MSG1    TO  T99-MSG
           ELSE
      *        レセ電データ作成時のレセプト件数
               IF      WRK-PRT-PAGE2   >   ZERO
                   MOVE    WRK-PRT-PAGE2   TO  WRK-T99-MSG-KENSU
                   MOVE    WRK-T99-MSG     TO  T99-MSG
               END-IF
      *
      *        レセ電データチェック時のエラーリストの有無
               IF      WRK-PRT-PAGE1   >   ZERO
                   MOVE    "red"           TO  T99-MSG-STYLE
               END-IF
           END-IF
      *
           .
       4900-MSG-HENSYU-EXT.
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
                                               TO  T99-MSG
               WHEN    "0002"
                   MOVE    "【ｆｄｄが起動されていません】" 
                                               TO  T99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "【ｆｄｄで処理が失敗しました】" 
                                               TO  T99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】" 
                                               TO  T99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】" 
                                               TO  T99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】" 
                                               TO  T99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】" 
                                               TO  T99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    個人履歴表示
      *****************************************************************
       400-T07-SYORI-SEC           SECTION.
      *
           INITIALIZE                  T07
           MOVE    SPA-GMN-T06-PTNUM   TO  T07-PTNUM
           MOVE    SPA-GMN-T06-NAME    TO  T07-NAME
           MOVE    SPA-GMN-T06-SRYYM   TO  T07-SRYYM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
               MOVE    "社保"              TO  T07-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                           =   "T"
                   MOVE    "労災"              TO  T07-TEISYUTUSAKI
               ELSE
                   MOVE    "国保"              TO  T07-TEISYUTUSAKI
               END-IF
           END-IF
      *
           IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
               MOVE    "年金証書番号等"    TO  T07-LIST-HYODAI1
           ELSE
               MOVE    "記号・番号"        TO  T07-LIST-HYODAI1
           END-IF
      *
           MOVE    ZERO                TO  IDX
           INITIALIZE                      ONRECE-B-REC
           MOVE    SPA-HOSPNUM         TO  ONRECE-B-HOSPNUM
           MOVE    SPA-NAI-T06-SRYYM-H TO  ONRECE-B-SRYYM
           MOVE    SPA-NAI-T06-SRYYM   TO  ONRECE-B-SRYYM-B
           MOVE    SPA-NAI-T06-PTID    TO  ONRECE-B-PTID
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
               MOVE    "1"                 TO  ONRECE-B-TEISYUTUSAKI
                                               ONRECE-B-XXTEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                           =   "T"
                   MOVE    "3"             TO  ONRECE-B-TEISYUTUSAKI
                                               ONRECE-B-XXTEISYUTUSAKI
               ELSE
                   MOVE    "6"             TO  ONRECE-B-TEISYUTUSAKI
                   MOVE    "2"             TO  ONRECE-B-XXTEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-GMN-T06-NYUGAIKBN
                                       TO  ONRECE-B-NYUGAIKBN
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
      *
           PERFORM         UNTIL   FLG-ONRECE-B    =   1
               ADD     1               TO  IDX
               MOVE    IDX             TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  T07-TRENNUM (IDX)
               IF      ONRECE-B-KAKUTEI-KBN
                                       =   "1"
                   MOVE    "済"            TO  T07-TSEIKYU (IDX)
               END-IF
               MOVE    ONRECE-B-SKYYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  T07-TSKYYMD (IDX)
               MOVE    ONRECE-B-SCNT   TO  WRK-Z9
               MOVE    WRK-Z9          TO  T07-TSCNT   (IDX)
               MOVE    ONRECE-B-UKEYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  T07-TUKEYMD (IDX)
               MOVE    ONRECE-B-UCNT   TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  T07-TUCNT   (IDX)
               EVALUATE    ONRECE-B-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  T07-TNYUGAIKBN (IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  T07-TNYUGAIKBN (IDX)
               END-EVALUATE
      *
               EVALUATE    ONRECE-B-TEISYUTUSAKI
                   WHEN    "3"
                       EVALUATE    ONRECE-B-RECESYUBETU
                           WHEN    "7101"
                           WHEN    "7091"
                               MOVE    "傷病・業務"        TO
                                               T07-TRECESYUBETU   (IDX)
                           WHEN    "7102"
                           WHEN    "7092"
                               MOVE    "傷病・通勤"        TO
                                               T07-TRECESYUBETU   (IDX)
                           WHEN    "7071"
                           WHEN    "7081"
                               MOVE    "短期・業務"        TO
                                               T07-TRECESYUBETU   (IDX)
                           WHEN    "7072"
                           WHEN    "7082"
                               MOVE    "短期・通勤"        TO
                                               T07-TRECESYUBETU   (IDX)
                           WHEN    OTHER
                               MOVE    RECE10-RECESYUBETU  TO
                                               T07-TRECESYUBETU   (IDX)
                       END-EVALUATE
                   WHEN    OTHER
                       MOVE    ONRECE-B-RECESYUBETU
                                           TO  T07-TRECESYUBETU   (IDX)
               END-EVALUATE
      *
               MOVE    ONRECE-B-HKNJANUM
                                       TO  T07-THKNJANUM      (IDX)
               IF      ONRECE-B-RECESYUBETU    =   "1212"  OR  "1211"
                   STRING  ONRECE-B-FTNNUM (1) DELIMITED   BY  SPACE
                           "　"                DELIMITED   BY  SIZE
                           ONRECE-B-JKYNUM (1) DELIMITED   BY  SPACE
                                               INTO    T07-TKIGO (IDX)
                   END-STRING
               ELSE
                   STRING  ONRECE-B-KIGO   DELIMITED   BY  SPACE
                           "　"            DELIMITED   BY  SIZE
                           ONRECE-B-NUM    DELIMITED   BY  SPACE
                                           INTO    T07-TKIGO (IDX)
                   END-STRING
               END-IF
      * 
               MOVE    IDX             TO  T07-LIST-COUNT
      * 
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      IDX             =   ZERO
               GO  TO  400-T07-SYORI-EXT
           END-IF
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T06"                TO  SPA-MOTOPG
           MOVE    "T07"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       400-T07-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成履歴表示
      *****************************************************************
       400-T08-SYORI-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX
           INITIALIZE                  ONRECE-K-REC
           MOVE    SPA-HOSPNUM     TO  ONRECE-K-HOSPNUM
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-K-SRYYM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
               MOVE   "1"              TO  ONRECE-K-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                           =   "T"
                   MOVE   "3"              TO  ONRECE-K-TEISYUTUSAKI
               ELSE
                   MOVE   "2"              TO  ONRECE-K-TEISYUTUSAKI
               END-IF
           END-IF
      *
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-KANRI-SELECT-SEC
      * 
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-ONRECE-K    =   1
               GO  TO  400-T08-SYORI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-T06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGT08"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-T06
           MOVE    SPA-WORK            TO  SPA-T01KYOUTU
      *
           MOVE    "T06"                TO  SPA-MOTOPG
           MOVE    "T08"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T08"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       400-T08-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
      *    入力中のチェック 
           IF      SPA-NAI-T06-PTID    >   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO  TO  480-KENSAKU-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T06             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T06"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T06             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T06"               TO  SPA-MOTOPG
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
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO   WRK-HENYMDG
           ELSE
               IF      WRK-SYMD            =   ALL "9"
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
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               END-IF
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
           IF      SPA-TIDCD       NOT =   SPACE
               EVALUATE    SPA-TIDCD
                   WHEN    "1002"
                       PERFORM 530-TID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    OTHER
                       PERFORM 520-TIDSET-SEC
                       GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T06    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-BODY-ROW             =   1
               MOVE    ZERO                TO  WRK-T06-BODY-ROW
           ELSE
               MOVE    T06-BODY-LIST-ROW   TO  WRK-T06-BODY-ROW
           END-IF
      *
           IF      FLG-RECE-ROW             =   1
               MOVE    ZERO                TO  WRK-T06-RECE-ROW
           ELSE
               MOVE    T06-RECE-LIST-ROW   TO  WRK-T06-RECE-ROW
           END-IF
      * 
           INITIALIZE                  T06
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T06-KANRI-MAX
               MOVE    SPA-GMN-T06-K-TRENNUM    (IDX)
                                           TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9            TO  T06-KANRI-TRENNUM  (IDX)
               MOVE    SPA-GMN-T06-K-TUKEYMD     (IDX)
                                           TO  T06-KANRI-TUKEYMD  (IDX)
               MOVE    SPA-GMN-T06-K-TUCNT      (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  T06-KANRI-TUCNT    (IDX)
               MOVE    SPA-GMN-T06-K-TSKYYMD (IDX)
                                           TO  T06-KANRI-TSKYYMD  (IDX)
               MOVE    SPA-GMN-T06-K-TSCNT   (IDX)
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  T06-KANRI-TSCNT    (IDX)
               MOVE    SPA-GMN-T06-K-TNOSEND-PAT-CNT   (IDX)
                                           TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9             TO  
                                   T06-KANRI-TNOSEND-PATIENT-CNT  (IDX)
               MOVE    SPA-GMN-T06-K-TSEND-TOTAL-CNT    (IDX)
                                           TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9             TO  
                                   T06-KANRI-TSEND-TOTAL-CNT      (IDX)
      *         
               IF      IDX             =   SPA-NAI-T06-KANRI-SELNUM
                   MOVE    "T"         TO  T06-KANRI-LIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  T06-KANRI-LIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-T06-KANRI-MAX
                                       TO  T06-KANRI-LIST-COUNT
           MOVE    ZERO                TO  T06-KANRI-LIST-ROW
      **     IF      WRK-T06-RECE-ROW         >   ZERO
      **         MOVE    WRK-T06-RECE-ROW         TO  T06-ROW
      **     END-IF
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T06-BODY-MAX
               MOVE    SPA-GMN-T06-B-TRENNUM    (IDX)
                                           TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9            TO  T06-BODY-TRENNUM   (IDX)
               MOVE    SPA-GMN-T06-B-TNYUGAIKBN (IDX)
                                           TO  T06-BODY-TNYUGAIKBN(IDX)
               IF      SPA-NAI-T06-B-THENREI-FLG(IDX)
                                           =   "1"
                   MOVE    "○"            TO  T06-BODY-THENREI-FLG(IDX)
               END-IF     
               MOVE    SPA-GMN-T06-B-TPTNUM     (IDX)
                                           TO  T06-BODY-TPTNUM    (IDX)
               MOVE    SPA-GMN-T06-B-TNAME      (IDX)
                                           TO  T06-BODY-TNAME     (IDX)
               MOVE    SPA-GMN-T06-B-TSRYYM     (IDX)
                                           TO  T06-BODY-TSRYYM    (IDX)
               MOVE    SPA-GMN-T06-B-TBIRTHDAY  (IDX)
                                           TO  T06-BODY-TBIRTHDAY (IDX)
               MOVE    SPA-GMN-T06-B-TRECESYUBETU (IDX)
                                           TO  T06-BODY-TRECESYUBETU
                                                                  (IDX)
               MOVE    SPA-GMN-T06-B-THKNJANUM  (IDX)
                                           TO  T06-BODY-THKNJANUM (IDX)
               MOVE    SPA-GMN-T06-B-TKIGO      (IDX)
                                           TO  T06-BODY-TKIGO     (IDX)
               MOVE    SPA-GMN-T06-B-TERRCD     (IDX)
                                           TO  T06-BODY-TERRCD    (IDX)
               MOVE    SPA-GMN-T06-B-TERR       (IDX)
                                           TO  T06-BODY-TERR      (IDX)
      *         
               IF      IDX             =   SPA-NAI-T06-BODY-SELNUM
                   MOVE    "T"         TO  T06-BODY-LIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  T06-BODY-LIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-T06-BODY-MAX
                                       TO  T06-BODY-LIST-COUNT
           MOVE    ZERO                TO  T06-BODY-LIST-ROW
           IF      WRK-T06-BODY-ROW         >   ZERO
               MOVE    WRK-T06-BODY-ROW         TO  T06-BODY-LIST-ROW
           END-IF
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T06-RECE-MAX
               MOVE    SPA-GMN-T06-R-TRENNUM    (IDX)
                                           TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9            TO  T06-RECE-TRENNUM   (IDX)
               MOVE    SPA-GMN-T06-R-TNYUGAIKBN (IDX)
                                           TO  T06-RECE-TNYUGAIKBN(IDX)
               EVALUATE    SPA-GMN-T06-R-TERR  (IDX)
                   WHEN    "1"
                       MOVE    "○"        TO  T06-RECE-TERR      (IDX)
                   WHEN    "2"
                       MOVE    "△"        TO  T06-RECE-TERR      (IDX)
                   WHEN    "3"
                       MOVE    "●"        TO  T06-RECE-TERR      (IDX)
                   WHEN    "4"
                       MOVE    "◎"        TO  T06-RECE-TERR      (IDX)
                   WHEN    "5"
                       MOVE    "□"        TO  T06-RECE-TERR      (IDX)
                   WHEN    "6"
                       MOVE    "＊"        TO  T06-RECE-TERR      (IDX)
                   WHEN    "7"
                       MOVE    "▲"        TO  T06-RECE-TERR      (IDX)
               END-EVALUATE
               MOVE    SPA-GMN-T06-R-TPTNUM     (IDX)
                                           TO  T06-RECE-TPTNUM    (IDX)
               MOVE    SPA-GMN-T06-R-TNAME      (IDX)
                                           TO  T06-RECE-TNAME     (IDX)
               MOVE    SPA-GMN-T06-R-TSRYYM     (IDX)
                                           TO  T06-RECE-TSRYYM    (IDX)
               IF      SPA-T01-PRTKBN      =   "6" OR  "7"
                   CONTINUE
               ELSE
                   EVALUATE    SPA-NAI-T06-RECE10-RECESYUBETU (IDX)
                       WHEN    "7101"
                       WHEN    "7091"
                           MOVE    "傷病・業務"        TO
                                           T06-RECE-TRECESYUBETU (IDX)
                       WHEN    "7102"
                       WHEN    "7092"
                           MOVE    "傷病・通勤"        TO
                                           T06-RECE-TRECESYUBETU (IDX)
                       WHEN    "7071"
                       WHEN    "7081"
                           MOVE    "短期・業務"        TO
                                           T06-RECE-TRECESYUBETU (IDX)
                       WHEN    "7072"
                       WHEN    "7082"
                           MOVE    "短期・通勤"        TO
                                           T06-RECE-TRECESYUBETU (IDX)
                       WHEN    OTHER
                           MOVE    SPA-NAI-T06-RECE10-RECESYUBETU (IDX)
                                                       TO
                                           T06-RECE-TRECESYUBETU (IDX)
                   END-EVALUATE
                   IF     SPA-NAI-T06-RECE10-HKNJANUM  (IDX)
                                       NOT =   "00000000"
                       MOVE    SPA-NAI-T06-RECE10-HKNJANUM  (IDX)
                                           TO  T06-RECE-THKNJANUM (IDX)
                   END-IF
      *
                   MOVE    SPA-GMN-T06-R-TKIGO      (IDX)
                                           TO  T06-RECE-TKIGO     (IDX)
                   MOVE    SPA-GMN-T06-R-THOJOKBN   (IDX)
                                           TO  T06-RECE-THOJOKBN  (IDX)
                   MOVE    SPA-GMN-T06-R-TRECEIPT-KBN (IDX)
                                           TO
                                           T06-RECE-TRECEIPT-KBN  (IDX)
                   MOVE    SPA-GMN-T06-R-TDISASTER (IDX)
                                           TO
                                           T06-RECE-TDISASTER (IDX)
               END-IF
               MOVE    SPA-GMN-T06-R-TCREYMD    (IDX)
                                           TO  T06-RECE-TCREYMD   (IDX)
      *         
               IF      IDX             =   SPA-NAI-T06-SELNUM
                   MOVE    "T"         TO  T06-RECE-LIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  T06-RECE-LIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-T06-RECE-MAX
                                       TO  T06-RECE-LIST-COUNT
           MOVE    ZERO                TO  T06-RECE-LIST-ROW
           IF      WRK-T06-RECE-ROW        >   ZERO
               MOVE    WRK-T06-RECE-ROW        TO  T06-RECE-LIST-ROW
           END-IF
      *
           IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
               MOVE    "年金証書番号等"    TO  T06-RECE-LIST-HYODAI1
           ELSE
               MOVE    "記号・番号"        TO  T06-RECE-LIST-HYODAI1
           END-IF
      *
           MOVE    SPA-GMN-T06-SRYYM-H     TO  T06-SRYYM-H
           MOVE    SPA-GMN-T06-SKYYMD-H    TO  T06-SKYYMD-H
           MOVE    SPA-GMN-T06-SELNUM      TO  T06-SELNUM
           MOVE    SPA-GMN-T06-PTNUM       TO  T06-PTNUM
           MOVE    SPA-GMN-T06-NAME        TO  T06-NAME
           MOVE    SPA-GMN-T06-SRYYM       TO  T06-SRYYM
           MOVE    SPA-GMN-T06-NYUGAIKBN   TO  T06-NYUGAIKBN
           MOVE    "red"                   TO  T06-TITLE3-STYLE
           MOVE    SPA-GMN-T06-TITLE3      TO  T06-TITLE3
      *
           MOVE    "blue"                  TO  T06-HKNJANAME-STYLE
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                           =   "T"
               IF      SPA-NAI-T06-DIRECT-HKNJANUM =   SPACE
                   CONTINUE   
               ELSE
                   MOVE    SPA-NAI-T06-DIRECT-HKNJANAME
                                                   TO  T06-HKNJANAME
               END-IF
           END-IF
           MOVE    SPA-GMN-T06-TEISYUTUSAKI1
                                           TO  T06-TEISYUTUSAKI1
           MOVE    SPA-GMN-T06-TEISYUTUSAKI2
                                           TO  T06-TEISYUTUSAKI2
           MOVE    SPA-GMN-T06-TEISYUTUSAKI3
                                           TO  T06-TEISYUTUSAKI3
           MOVE    SPA-GMN-T06-TEISYUTUSAKI4
                                           TO  T06-TEISYUTUSAKI4
           MOVE    SPA-GMN-T06-TEISYUTUSAKI1-STATE
                                           TO  T06-TEISYUTUSAKI1-STATE
           MOVE    SPA-GMN-T06-TEISYUTUSAKI2-STATE
                                           TO  T06-TEISYUTUSAKI2-STATE
           MOVE    SPA-GMN-T06-TEISYUTUSAKI3-STATE
                                           TO  T06-TEISYUTUSAKI3-STATE
           MOVE    SPA-GMN-T06-TEISYUTUSAKI4-STATE
                                           TO  T06-TEISYUTUSAKI4-STATE
      *
           IF    ( SPA-GMN-T06-KANRI-MAX   =   ZERO )
           OR    ( SPA-GMN-T06-SELNUM      =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  T06-B17-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  T06-B17-STATE
           END-IF
           MOVE    WIDGET-NORMAL       TO  T06-B08-STATE
                                           T06-B18-STATE
                                           T06-B12-STATE
           IF      SPA-NAI-T06-SYSYM-2 >=  SPA-NAI-T06-SRYYM-H
               MOVE    WIDGET-INSENSITIVE  TO  T06-B12-STATE
           END-IF
      *
      *    連携用のとき
           MOVE    "blue"                  TO  T06-TITLE1-STYLE
                                               T06-TITLE2-STYLE
           IF      SPA-T01-PRTKBN      =   "6" OR  "7"
               EVALUATE    SPA-T01-PRTKBN
                   WHEN    "6"
                       MOVE    "連携用"            TO  T06-TITLE1
                   WHEN    "7"
                       MOVE    "連携用（当日）"    TO  T06-TITLE1
               END-EVALUATE
               EVALUATE    SPA-T01-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "【入院】"          TO  T06-TITLE2
                   WHEN    "2"
                       MOVE    "【入院外】"        TO  T06-TITLE2
               END-EVALUATE
               MOVE    WIDGET-NORMAL       TO  T06-B12-STATE
               MOVE    WIDGET-INSENSITIVE  TO  T06-B18-STATE
                                               T06-B08-STATE
           END-IF
      *
           IF      SPA-GMN-T06-SRYYMKBN-MAX
                                       =   ZERO
               MOVE    "F"                 TO  T06-SRYYMKBN-COM-VISIBLE
               MOVE    WIDGET-INSENSITIVE  TO  T06-SRYYMKBN-COM-STATE
           ELSE
               MOVE    "T"                 TO  T06-SRYYMKBN-COM-VISIBLE
               MOVE    WIDGET-NORMAL       TO  T06-SRYYMKBN-COM-STATE
               MOVE    SPA-GMN-T06-SRYYMKBN-G
                                   TO  T06-SRYYMKBN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-T06-SRYYMKBN-MAX
                   MOVE    SPA-GMN-T06-SRYYMKBNLST (IDX)
                                       TO  T06-SRYYMKBN-ITEM(IDX)
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-T06-SRYYMKBN-MAX
                                   TO  T06-SRYYMKBN-COUNT
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
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-GMN-T06-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-T06-CUR
               WHEN    1
                   MOVE  "SRYYM_H"         TO  MCP-WIDGET
               WHEN    2
                   MOVE  "SKYYMD_H"        TO  MCP-WIDGET
               WHEN    3
                   MOVE  "SELNUM"          TO  MCP-WIDGET
               WHEN    4
                   MOVE  "PTNUM"           TO  MCP-WIDGET
               WHEN    5
                   MOVE  "NYUGAIKBN"       TO  MCP-WIDGET
               WHEN    6
                   MOVE  "SRYYM"           TO  MCP-WIDGET
               WHEN    7
                   MOVE  "SRYYMKBN"        TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"             TO  MCP-WIDGET   
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SRYYM_H"
                   IF      SPA-GMN-T06-SRYYMKBN-MAX    >   ZERO
                       MOVE    7               TO  SPA-GMN-T06-CUR
                   ELSE
                       MOVE    2               TO  SPA-GMN-T06-CUR
                   END-IF
               WHEN    "SRYYMKBN"
                   MOVE    2               TO  SPA-GMN-T06-CUR
               WHEN    "SKYYMD_H"
                   MOVE    4               TO  SPA-GMN-T06-CUR
               WHEN    "SELNUM"
                   MOVE    4               TO  SPA-GMN-T06-CUR
               WHEN    "PTNUM"
                   MOVE    5               TO  SPA-GMN-T06-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    6               TO  SPA-GMN-T06-CUR
               WHEN    "SRYYM"
                   MOVE    4               TO  SPA-GMN-T06-CUR
               WHEN    "TEISYUTUSAKI1"
               WHEN    "TEISYUTUSAKI2"
               WHEN    "TEISYUTUSAKI3"
               WHEN    "TEISYUTUSAKI4"
                   MOVE    4               TO  SPA-GMN-T06-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入外区分入力エラー"        TO  TERR-ERRMSG
               WHEN    "0002"
                   MOVE    "これ以上データの追加はできません"
                                                       TO  TERR-ERRMSG
               WHEN    "0003"
                   MOVE    "該当の患者が存在しません"  TO  TERR-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0006"
                   MOVE    "診療年月入力エラー"        TO  TERR-ERRMSG
               WHEN    "0007"
                   MOVE    "対象の請求データがありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0008"
                   MOVE    "個別指示一覧に選択済です"  TO  TERR-ERRMSG
               WHEN    "0009"
                   MOVE    "診療年月入力エラー"        TO  TERR-ERRMSG
               WHEN    "0010"
                   MOVE    "対象のレセ電データがありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0011"
                   MOVE    "返戻データが設定されていません"
                                                       TO  TERR-ERRMSG
               WHEN    "0012"
                   MOVE
                   "請求する診療年月より未来の診療年月が入力されました"
                                                       TO  TERR-ERRMSG
               WHEN    "0013"
                   MOVE    "請求年月日入力エラー"      TO  TERR-ERRMSG
               WHEN    "0014"
                   MOVE
                   "対象の請求データは返戻年月の設定が行なわれています"
                                                       TO  TERR-ERRMSG
               WHEN    "0015"
                   STRING  "対象の請求データは返戻年月、"
                                                   DELIMITED  BY  SIZE
                           "月遅れ等の設定が行なわれています"
                                                   DELIMITED  BY  SIZE
                                                   INTO  TERR-ERRMSG
                   END-STRING
               WHEN    "0016"
                   MOVE
                   "対象の請求データは月遅れ等の設定が行なわれています"
                                                       TO  TERR-ERRMSG
               WHEN    "0017"
                   MOVE
                   "対象の請求データに返戻の設定が行なわれていません"
                                                       TO  TERR-ERRMSG
               WHEN    "0018"
                   MOVE    "主科未設定データです"      TO  TERR-ERRMSG
               WHEN    "0019"
                   MOVE
                   "対象の請求データは災害の未確定分です"
                                                       TO  TERR-ERRMSG
               WHEN    "0020"
                   STRING  "改正パッチ提供以降に平成２４年３月以前の"
                                                   DELIMITED  BY  SIZE
                     "月遅れ分についてレセプト再処理が行われていません"
                                                   DELIMITED  BY  SIZE
                                                   INTO    TERR-ERRMSG
                   END-STRING
               WHEN    "0021"
                   MOVE    "対象の連携データがありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0022"
                   MOVE    "患者情報を入力中です"      TO  TERR-ERRMSG
               WHEN    "0023"
                   MOVE    "選択番号入力エラー"        TO  TERR-ERRMSG
               WHEN    "0024"
                   MOVE    "対象診療年月入力エラー"    TO  TERR-ERRMSG
               WHEN    "0025"
                   MOVE    "作成済のデータはありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0026"
                   MOVE    "作成対象となるデータがありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0027"
                   MOVE    "選択した保険者と違います"  TO  TERR-ERRMSG
               WHEN    "0028"
                   MOVE
                       "連携用のときは違う診療年月は入力できません"
                                                       TO  TERR-ERRMSG
               WHEN    "0029"
                   MOVE    "前回作成分は連携用ですので表示できません"
                                                       TO  TERR-ERRMSG
               WHEN    "0030"
                   MOVE    "選択した期間以外の診療年月です"
                                                       TO  TERR-ERRMSG
               WHEN    "0031"
                   MOVE    "前回作成分は選択した期間以外の診療年月です"
                                                       TO  TERR-ERRMSG
               WHEN    "0041"
                   MOVE    "データが入力してあるので遷移できません"
                                                       TO  TERR-ERRMSG
      *
               WHEN    "0050"
                   STRING
                       "月途中に医療機関コードが変更されているので、"
                                                   DELIMITED  BY  SIZE
                       "この期間の処理はできません"
                                                   DELIMITED  BY  SIZE
                                                   INTO    TERR-ERRMSG
                   END-STRING
               WHEN    "0051"
                   STRING
                       "月途中に医療機関コードが変更されている診療年月"
                                                   DELIMITED  BY  SIZE
                       "のため、期間指定による処理はできません"
                                                   DELIMITED  BY  SIZE
                                                   INTO    TERR-ERRMSG
                   END-STRING
      *
               WHEN    "1001"
                   MOVE    "レセプト指示コード（個別）が削除できません"
                                                       TO  TERR-ERRMSG
               WHEN    "1002"
                   MOVE    "レセプト指示コード（個別）が更新できません"
                                                       TO  TERR-ERRMSG
               WHEN    "1003"
                   MOVE    "レセプト指示コード（管理）が更新できません"
                                                       TO  TERR-ERRMSG
               WHEN    "1004"
                   MOVE    "パラメタＤＢが更新できません"
                                                       TO  TERR-ERRMSG
               WHEN    "1005"
                   MOVE    "パラメタＤＢが存在しません" 
                                                       TO  TERR-ERRMSG
               WHEN    "1006"
                   MOVE    "処理上限数を超えています" 
                                                       TO  TERR-ERRMSG
               WHEN    "1009"
                   MOVE    "オンライン管理ＤＢが更新できません"
                                                       TO  TERR-ERRMSG
               WHEN    "1010"
                   MOVE    "オンライン明細ＤＢが更新できません"
                                                       TO  TERR-ERRMSG
               WHEN    "2001"
                   MOVE    "入力された診療月のデータは閲覧のみ可能です"
                                                       TO  TERR-ERRMSG
               WHEN    "7777"
               WHEN    "9999"
                   MOVE    WRK-RECEERR                 TO  TERR-ERRMSG
      *
               WHEN    "E88"
                   MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
                                                       TO  TERR-ERRMSG
           END-EVALUATE
      *
           MOVE    TERR-ERRMSG          TO  SPA-ERRMSG
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T06"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
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
      *    確認画面１表示処理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1003"
                   MOVE
                   "入力された内容をすべてクリアします"  TO  WRK-TIDMSG
               WHEN    "1004"
                   MOVE    "前回個別作成分を表示します"  TO  WRK-TIDMSG
               WHEN    "1005"
                   MOVE
                       "患者情報を入力中ですが終了しますか"
                                                         TO  WRK-TIDMSG
               WHEN    "1006"
                   STRING   "請求データの作成日がレセ送信日以前ですが"
                                       DELIMITED  BY  SIZE
                            "個別指示の対象としますか（"
                                       DELIMITED  BY  SIZE
                            SPA-GMN-T09-TCREYMD (1)
                                       DELIMITED  BY  SIZE
                            "）"       DELIMITED  BY  SIZE
                                       INTO    WRK-TIDMSG
                   END-STRING
               WHEN    "1007"
                   MOVE    "選択された患者を処理対象から削除しますか？"
                                       TO  WRK-TIDMSG
               WHEN    "1008"
                   MOVE    "未処理データがありますが終了しますか"
                                       TO  WRK-TIDMSG
               WHEN    "1009"
               WHEN    "1019"
                   MOVE    "未処理データがありますが変更しますか"
                                       TO  WRK-TIDMSG
               WHEN    "1010"
                   MOVE
                 "未処理データがありますが前回個別作成分を表示しますか"
                                       TO  WRK-TIDMSG
               WHEN    "1011"
                   STRING   "返戻データが存在します（返戻設定無し）。"
                                       DELIMITED  BY  SIZE
                            "対象としますか"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-TIDMSG
                   END-STRING
               WHEN    "1012"
                   STRING   "保険情報が一致する返戻データが存在します"
                                       DELIMITED  BY  SIZE
                            "（返戻設定無し）。対象としますか"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-TIDMSG
                   END-STRING
               WHEN    "6666"
                   MOVE    "レセ電データ作成処理中です"  TO  WRK-TIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  TID1
           INITIALIZE                      TID1
           MOVE    SPA-TIDCD           TO  TID1-ID1CODE
           MOVE    WRK-TIDMSG          TO  TID1-ID1MSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1006"
               WHEN    "1011"
               WHEN    "1012"
                   MOVE    "いいえ"         TO  TID1-B01-LABEL
                   MOVE    "は  い"         TO  TID1-B12-LABEL
                   MOVE    "B12"            TO  MCP-WIDGET
               WHEN    "1007"
               WHEN    "2002"
               WHEN    "2007"
                   MOVE    "いいえ"         TO  TID1-B01-LABEL
                   MOVE    "は  い"         TO  TID1-B12-LABEL
                   MOVE    "B01"            TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "戻る"           TO  TID1-B01-LABEL
                   MOVE    "ＯＫ"           TO  TID1-B12-LABEL
                   MOVE    "B12"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "T06"                TO  SPA-MOTOPG
           MOVE    "TID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       530-TID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           MOVE    SPACE                TO  TID2
           INITIALIZE                       TID2
           MOVE    SPA-TIDCD            TO  TID2-ID1CODE
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1002"
                   MOVE    "既に請求済ですが個別指示の対象としますか"
                                            TO  TID2-ID1MSG
           END-EVALUATE
      *
           MOVE    "F"                  TO  TID2-B10-VISIBLE
           MOVE    "T"                  TO  TID2-B11-VISIBLE
           MOVE    "F11:個人履歴"       TO  TID2-B11-LABEL
           MOVE    "F12:作成する"       TO  TID2-B12-LABEL
           MOVE    "blue"               TO  TID2-LABEL-STYLE
           MOVE    "black"              TO  TID2-ID1MSG-STYLE
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T06"                TO  SPA-MOTOPG
           MOVE    "TID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-TID2SET-EXT.
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
      *    管理ファイルＳＥＬＥＣＴ処理
      *****************************************************************
       900-ONRECE-KANRI-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-KANRI-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-ONRECE-K
               INITIALIZE                      ONRECE-K-REC
           END-IF
           .
       900-ONRECE-KANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイルFETCH処理
      *****************************************************************
       900-ONRECE-KANRI-FETCH-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE   ZERO                 TO  FLG-ONRECE-K
               MOVE   MCPDATA-REC          TO  ONRECE-K-REC
           ELSE
               INITIALIZE                      ONRECE-K-REC
               MOVE   1                    TO  FLG-ONRECE-K
           END-IF
           .
       900-ONRECE-KANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルSELECT処理
      *****************************************************************
       900-ONRECE-BODY-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-BODY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルFETCH処理
      *****************************************************************
       900-ONRECE-BODY-FETCH-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  ONRECE-B-REC
               MOVE  ZERO                  TO  FLG-ONRECE-B
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-BODY-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイル読込処理
      *****************************************************************
       900-ONRECE-BODY-READ-SEC        SECTION.
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ONRECE-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               MOVE    1                   TO  FLG-PTINF
                INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   INITIALIZE                  PTKOHINF-REC
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       900-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC         TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-INV-SEC             SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  RECE10-REC
      *
      *        労災のとき
               IF      RECE10-TEISYUTUSAKI =   "3"
      *            アフター、公務災害は対象外
      *            ０９５９３で始まるコードがあるとき対象外
                   IF    ( RECE10-RECESYUBETU  =   "7021"
                                               OR  "7051"
                                               OR  "7061" )
                   OR    ( RECE10-FLG (1)      =   1      )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
      *            労災指定医療機関でないときは対象外
                   MOVE    RECE10-SRYYM    TO  WRK-SYS-SRYYMD (1:6)
                   MOVE    "01"            TO  WRK-SYS-SRYYMD (7:2)
      *
                   MOVE    ZERO            TO  FLG-SET
                   PERFORM VARYING IDWW    FROM    1   BY  1
                           UNTIL   IDWW    >   SPA-NAI-T06-4001-MAX
                       IF    ( SPA-NAI-T06-4001-STYUKYMD (IDWW)
                                               <=  WRK-SYS-SRYYMD   )
                       AND   ( WRK-SYS-SRYYMD  <=
                                   SPA-NAI-T06-4001-EDYUKYMD (IDWW) )
                           IF      SPA-NAI-T06-4001-RSIIRYOKBN (IDWW)
                                                   =   "1"
                               MOVE     1              TO  FLG-SET
                           END-IF
                           MOVE    SPA-NAI-T06-4001-MAX
                                                       TO  IDWW
                       END-IF
                   END-PERFORM
                   IF      FLG-SET             =   ZERO
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF    
       *
               MOVE    ZERO            TO  FLG-SEIKYU
           ELSE
               MOVE    1               TO  FLG-SEIKYU
               INITIALIZE                  RECE10-REC
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-SEC             SECTION.
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receden"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECEDEN
               ELSE
                   MOVE    1                   TO  FLG-RECEDEN
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECEDEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-N-SEC          SECTION.
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  RECEDEN-REC
               MOVE    ZERO                TO  FLG-RECEDEN
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           .
       900-RECEDEN-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携テンポラリ読込
      *****************************************************************
       900-RENKEI-TMP-READ-SEC             SECTION.
      *
           MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_renkei_tmp"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RENKEI-TMP
                   MOVE    MCPDATA-REC         TO  RECEDEN-REC
               ELSE
                   MOVE    1                   TO  FLG-RENKEI-TMP
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RENKEI-TMP
           END-IF
      *
           MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RENKEI-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       900-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "INSERT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "INSERT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
                       " mcp-rc="          MCP-RC
           END-IF
           .
       900-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       900-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "UPDATE ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "UPDATE ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
                       " mcp-rc="          MCP-RC
           END-IF
           .
       900-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       900-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "DELETE ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "DELETE ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
                       " mcp-rc="          MCP-RC
           END-IF
           .
       900-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
