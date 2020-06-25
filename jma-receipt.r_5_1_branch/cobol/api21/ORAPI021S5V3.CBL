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
       PROGRAM-ID.         ORAPI021S5V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　リハビリ開始日・コメント登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/10/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  17/06/23  API　情報取得追加
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K06SCR-SPA".
      *
      *    オンライン画面
       01  WRK-SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-SANTEIPLUS          PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-DAYERR              PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SYORIEND            PIC 9(01).
           03  FLG-CONTKBN             PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-COMMTARI            PIC 9(01).
           03  FLG-ENDDATE             PIC 9(01).
           03  FLG-COMEND              PIC 9(01).
           03  FLG-COMM-OK             PIC 9(01).
      *
           03  FLG-ERRCHK              PIC 9(01).
           03  FLG-CANSEL              PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-DELETE-OK           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-R                   PIC 9(04).
      *
           03  IDX-ERR                 PIC 9(04).
           03  IDX-WAR                 PIC 9(04).
      *
           03  IDD                      PIC 9(04).
           03  IDD2                     PIC 9(04).
           03  IDC                      PIC 9(04).
           03  IDE                      PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(200).
           03  WRK-ERRCD-2             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(200).
      *
      *    ワーニングコード
           03  WRK-KEICD               PIC X(03).
           03  WRK-OKMSG               PIC X(100).
      *    項目毎のエラー
           03  WRK-ERRCD-TBLG.
               05  WRK-ERRCD-TBL       OCCURS   100.
                   07  WRK-ERRCD-T         PIC X(04).
      *
      *    オルカUID
           03  WRK-ORCA-UID            PIC X(36).
           03  WRK-OPID                PIC X(16).
      *
           03  HZN-SPA-OPID            PIC X(16).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(64).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
           03  WRK-ENDKBN              PIC X(01).
      *
           03  WRK-HKNCOMBIMEI        PIC X(100).
           03  WRK-HKNKBN-MEI         PIC X(02).
           03  WRK-COMB-TEKSTYMD      PIC X(08).
           03  WRK-COMB-TEKEDYMD      PIC X(08).
      *
           03  WRK-SANTEI-YMD.
               05  WRK-SANTEI-YM           PIC X(06).
               05  WRK-SANTEI-DD           PIC X(02).
           03  WRK-LAST-DAY            PIC 9(02).
           03  WRK-SRYLST-DD           PIC 9(02).
      *
           03  WRK-DD                  PIC 9(02).
      *
           03  WRK-DAYKEY              PIC 9(02).
      *    保険組合せ
           03  WRK-CHK-HKNCOMBI        PIC X(04).
      *
      *    履歴内容
           03  WRK-SANTEI-SRYYM        PIC X(06).
           03  WRK-SANTEI-SRYCD        PIC X(09).
           03  WRK-SANTEI-HKNCOMBI     PIC X(04).
           03  WRK-FSANTEIYMD          PIC X(08).
      *
           03  WRK-SANTEI-ENDYMD       PIC X(10).
           03  WRK-SANTEI-RENNUM       PIC 9(02).
           03  WRK-SANTEI-COMMNT       PIC X(80).
      *
           03  WRK-ENDDATE-GMN         PIC X(08).
           03  WRK-LST-ENDDATE         PIC X(08).
      *
           03  WRK-CHK-YMD.
               05  WRK-CHK-YM              PIC X(06).
               05  WRK-CHK-DD              PIC X(02).
      *R01.8
           03  WRK-END-SRYCD           PIC X(09).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
      *    算定年月
           03  WRK-SRYYM               PIC X(06).
      *    コード
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-DAY-G.
               05  WRK-DAY             PIC 9(03)
                                       OCCURS    31.
      *
           03  WRK-IN-COMMNT-AREA.
               05  WRK-IN-COMMNT-TBL       OCCURS   31.
                   07  WRK-COMMNT-DAY      PIC 9(02).
                   07  WRK-END-DATE        PIC X(10).
                   07  WRK-COMMNT-TBL      OCCURS   50.
                       09  WRK-COMMNT          PIC X(80).
                       09  WRK-COM-END-DATE    PIC X(10).
      *    基準日
           03  WRK-BASE-YMD            PIC X(08).
           03  WRK-BASE-YMD-R      REDEFINES  WRK-BASE-YMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
      *
      *    共通パラメタ内容
       01  WRK-APIV3DATA-AREA.
      *    電子カルテUID
           03  WRK-KARTE-UID           PIC X(36).
      *    患者番号
           03  WRK-PARA-PTNUM          PIC X(20).
           03  WRK-APIV3DATA-REC.
      *        レスポンス番号
               05  WRK-RESPONSE-NUMBER     PIC X(02).
      *        処理ＰＧ
               05  WRK-PARA-SYORIPG        PIC X(02).
      *        登録終了
               05  WRK-PARA-ENDKBN         PIC X(01).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *    全角
           03  WRK-HENYMD-Z        PIC X(22).
      *
      *!!!
      *K061 と同じ内容に変更があれば同時に変更
      *
      *算定履歴作成
       01  TBL-RRK-SRYCD-G.
      *    心大血管疾患リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800111".
      *    心大血管疾患リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800112".
      *    脳血管疾患リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800121".
      *    脳血管疾患リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800122".
      *    運動器リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800131".
      *    運動器リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800132".
      *    呼吸器リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800141".
      *    呼吸器リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800142".
      *    摂食機能リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800151".
      *    摂食機能リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800152".
      *    難病患者リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800161".
      *    難病患者リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800162".
      *    障害児リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800171".
      *    障害児リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800172".
      *    がん患者リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800181".
      *    がん患者リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800182".
      *    廃用症候群リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800191".
      *    廃用症候群リハビリ終了日
           03  FILLER          PIC X(09)   VALUE  "099800192".
      *    心大血管疾患　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800211".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    脳血管疾患　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800221".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    運動器　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800231".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    呼吸器　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800241".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    廃用症候群　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800291".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *
      *    他医院退院日
           03  FILLER          PIC X(09)   VALUE  "099999906".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    精神科ケア開始日
           03  FILLER          PIC X(09)   VALUE  "099830101".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    在宅患者共同診療料開始日
           03  FILLER          PIC X(09)   VALUE  "099140004".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    通院・在宅精神療法開始日
           03  FILLER          PIC X(09)   VALUE  "099830102".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    依存症集団療法開始日
           03  FILLER          PIC X(09)   VALUE  "099830103".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    磁気による刺激法開始日
           03  FILLER          PIC X(09)   VALUE  "099400001".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *H30.4
      *    精神科ＳＣ疾患別等専門プログラム加算開始日
           03  FILLER          PIC X(09)   VALUE  "099830104".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    高気圧酸素治療開始日
           03  FILLER          PIC X(09)   VALUE  "099400002".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *R02.4
      *    心不全に対する遠赤外線温熱療法開始日
           03  FILLER          PIC X(09)   VALUE  "099400003".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    依存症集団療法開始日（ギャンブル依存症）
           03  FILLER          PIC X(09)   VALUE  "099830105".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *    経頭蓋磁気刺激療法開始日
           03  FILLER          PIC X(09)   VALUE  "099830106".
           03  FILLER          PIC X(09)   VALUE  SPACE.
      *
       01  TBL-RRK-SRYCD-GR    REDEFINES   TBL-RRK-SRYCD-G.
      **** 03  TBL-RRK-SRYCD-GT        OCCURS    22.
           03  TBL-RRK-SRYCD-GT        OCCURS    25.
               05  TBL-RRK-SRYCD           PIC X(09).
               05  TBL-END-SRYCD           PIC X(09).
       01  TBL-RRK-MAX                 PIC 9(02)   VALUE   25.
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
      **   COPY    "CPMKPASS.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
           COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV3REQ5.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV3RES5.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
      *01  SYSBASE-REC.
      *    COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
      *01  SYSUSER-REC.
      *    COPY    "CPSYSUSER.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
      *    COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1009.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    算定履歴テーブル
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    算定履歴付加テーブル
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "CPAPILSTV2.INC".
      *V3  一体化
           COPY    "API21V3SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "**********************"
           DISPLAY   "* medicalreq5v3 start*"
           DISPLAY   "**********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
           MOVE    MCP-TERM            TO  WRK-MCP-TERM
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST08-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "**********************"
           DISPLAY   "* medicalreq5v3 end  *"
           DISPLAY   "**********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-MEDICALV3RES5
           INITIALIZE                      WRK-APIV3DATA-AREA
           INITIALIZE                      SPA-K01KYOUTU
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  MDCRES-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MDCRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO         100-INIT-EXT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  IDX-WAR
           MOVE    ZERO                TO  IDX-ERR
      *
           MOVE    MDCREQ-REQUEST-NUMBER
                                       TO  MDCRES-REQUEST-NUMBER
           MOVE    MDCREQ-REQUEST-NUMBER
                                       TO  MDCRES-RESPONSE-NUMBER
      *    オルカＵＩＤ
           IF     (MDCREQ-ORCA-UID   NOT =   SPACE  )
             AND  (MDCREQ-REQUEST-NUMBER NOT =   "99" )
               PERFORM 2009-PARA-INIT-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
               MOVE    3                   TO  FLG-SYORI
     ***   WHEN    "05"
     ***       MOVE    5                   TO  FLG-SYORI
      *H29.6
      *API
           WHEN    "00"
               MOVE    ZERO                TO  FLG-SYORI
               PERFORM 2010-API-SYORI-SEC
               MOVE    "00"                TO  MDCRES-RESPONSE-NUMBER
               GO      TO      200-MAIN-EXT
      *API
           WHEN    "99"
      *        強制処理終了
               PERFORM 2099-CANCEL-SEC
               MOVE    "00"                TO  MDCRES-RESPONSE-NUMBER
               GO      TO      200-MAIN-EXT
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
               MOVE    "01"                TO  MDCRES-RESPONSE-NUMBER
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               PERFORM 890-ERRCD-MSG-SEC
               IF      WRK-ERRCD           =   "08"
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
      *        編集処理
               PERFORM 20010-INPUT-HEN-SEC
      *        パラメタ削除・排他解除処理
               IF     (MDCREQ-ORCA-UID   NOT =   SPACE  )
                 AND  (FLG-SYORIEND            =   1      )
                   PERFORM 2900-SYORI-END-SEC
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
      ****         MOVE    SPACE           TO  MDCRES-CONT-MODE
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "01"
      *        初期処理
               PERFORM 2001-PTINF-SYORI-SEC
           WHEN    "02"
      *        確認・更新処理
               PERFORM 2002-SYORI-02-SEC
           WHEN    "03"
      *        更新処理
               PERFORM 2003-SYORI-03-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    強制処理終了
      *****************************************************************
       2099-CANCEL-SEC                 SECTION.
      *
           IF      MDCREQ-PATIENTID    =   SPACE
               MOVE    SPACE               TO  WRK-PTNUM
           ELSE
               MOVE    MDCREQ-PATIENTID    TO  WRK-PTNUM
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    WRK-PTNUM           TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM         TO  WRK-PTNUM
               END-IF
           END-IF
      *
           INITIALIZE                  ORAPIV3SUB01AREA
           MOVE    "9"                 TO  APIV3SUB01-SYORI
           IF      MDCREQ-ORCA-UID     NOT =   SPACE
      *        API+UIDコードをTERMID
               STRING  "API+"
                       MDCREQ-ORCA-UID     DELIMITED  BY  SPACE
                                           INTO    APIV3SUB01-TERMID
               END-STRING
           END-IF
           MOVE    MDCREQ-KARTE-UID    TO  APIV3SUB01-KARTE-UID
           MOVE    "AP21"              TO  APIV3SUB01-API-GYOUMUID
           MOVE    "API21V3"           TO  APIV3SUB01-API-FILEMEI
           MOVE    WRK-PTNUM           TO  APIV3SUB01-PTNUM
           MOVE    "K02"               TO  APIV3SUB01-PARA-GYOUMUID
           CALL    "ORAPIV3SUB01"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPIV3SUB01AREA
           IF      APIV3SUB01-ERRCD    NOT =   SPACE
               MOVE    "81"                TO  WRK-ERRCD
               MOVE    APIV3SUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
           MOVE    9                   TO  FLG-SYORI
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
           .
       2099-CANCEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST08-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALV3REQ5
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST08-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req5"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "!!!XMLOPEN failure = " MCP-RC
               MOVE    "97"             TO  WRK-ERRCD
               GO      TO      1000-APTXMLREAD-EXT
           END-IF
      *
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST08-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req5"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALV3REQ5
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST08-BODY
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           MOVE    SPACE               TO  WRK-OKMSG
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
                   WHEN    2
                       IF      MDCRES-RESPONSE-NUMBER    =  "00"
                           IF      FLG-DELETE-OK           =   ZERO
                               MOVE    "登録処理終了"
                                               TO  WRK-OKMSG
                           ELSE
                               MOVE    "削除処理終了"
                                               TO  WRK-OKMSG
                           END-IF
                       ELSE
                           MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
                       END-IF
                   WHEN    3
                       IF      MDCRES-RESPONSE-NUMBER    =  "00"
                           IF      FLG-DELETE-OK           =   ZERO
                               MOVE    "登録処理終了"
                                               TO  WRK-OKMSG
                           ELSE
                               MOVE    "削除処理終了"
                                               TO  WRK-OKMSG
                           END-IF
                       ELSE
                           MOVE    "処理終了"
                                           TO  WRK-OKMSG
                       END-IF
      *API
                   WHEN    0
                       MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
      *API
                   WHEN    9
                       MOVE    "解除処理終了"
                                           TO  WRK-OKMSG
               END-EVALUATE
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    ZERO            TO  WRK-ERRCD
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    ZERO            TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               MOVE    WRK-OKMSG           TO  MDCRES-API-RESULT-MSG
           ELSE
      *        エラーメッセージ
               IF      WRK-ERRMSG          =   SPACE
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDCRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST08-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res5"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES5
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res5"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES5
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    MDCRES-OBJECT       TO  APILST08-BODY
               MOVE    "patientmod/xml"    TO  APILST08-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *****************************************************************
      *    ORCA-UID 初期処理
      *****************************************************************
       2009-PARA-INIT-SEC            SECTION.
      *
      *    UIDコードをTERMID
           MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA          =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
               MOVE    APIPARA-PTNUM       TO  WRK-PARA-PTNUM
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
             AND  (MDCREQ-REQUEST-NUMBER   =   "01"  )
      *        リクエスト０１　は排他解除
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
             AND  (MDCREQ-REQUEST-NUMBER   NOT =   "01"  )
      *        リクエスト０１以外は、同じ処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "05"
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2009-PARA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *    必須入力チェック
           PERFORM 20010-INIT-CHECK-SEC
      *
      *    電子カルテＵＩＤチェック
           IF     (MDCREQ-ORCA-UID   NOT =   SPACE  )
             AND  (WRK-ERRCD           =   SPACE  )
               IF      MDCREQ-KARTE-UID  NOT =   WRK-KARTE-UID
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    1               TO  FLG-SYORIEND
           END-IF
      *    継続区分
      *    IF      MDCREQ-CONT-MODE  =   "True"
      *        MOVE    1               TO  FLG-CONTKBN
      *    ELSE
      *        MOVE    ZERO            TO  FLG-CONTKBN
      *    END-IF
      *
      *    患者基本チェック処理
      *    IF      WRK-ERRCD           =   SPACE
      *        PERFORM 200211-KIHON-CHK-SEC
      *    END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オルカＵＩＤ変換編集処理
      *****************************************************************
       2001-TERMID-HEN-SEC     SECTION.
      *
           MOVE    MDCREQ-ORCA-UID   TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
      *
           .
       2001-TERMID-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    患者番号
           MOVE    MDCREQ-PATIENTID      TO  MDCRES-PATIENTID
           MOVE    MDCREQ-PATIENTID      TO  WRK-PTNUM
      *    基準日
      *    MOVE    MDCREQ-BASE-DATE      TO  MDCRES-BASE-DATE
      *    MOVE    MDCREQ-BASE-DATE      TO  WRK-HEN-DATE
      *    MOVE    SPACE                   TO  WRK-HEN-TIME
      *    PERFORM 802-DAYHEN02-SEC
      *    MOVE    WRK-SYMD                TO  WRK-BASE-YMD
      *    未設定時はシステム情報設定
      *    IF      WRK-BASE-YMD         =   SPACE
      *        MOVE    SMCNDATE-YMD        TO  WRK-BASE-YMD
      *    END-IF
      *    基準日をシステム日付へ
      *    MOVE    WRK-BASE-YMD        TO  SPA-SYSYMD
      *
           MOVE    MDCREQ-KARTE-UID      TO  MDCRES-KARTE-UID
           MOVE    MDCREQ-ORCA-UID       TO  MDCRES-ORCA-UID
      *
      ***  MOVE    MDCREQ-CONT-MODE      TO  MDCRES-CONT-MODE
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    必須入力チェック処理
      *****************************************************************
       20010-INIT-CHECK-SEC    SECTION.
      *
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *
      *    UID（電子カルテ）
           IF      MDCREQ-KARTE-UID  =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "06"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    オルカＵＩＤ
           IF      MDCREQ-ORCA-UID   =   SPACE
               IF      MDCREQ-REQUEST-NUMBER NOT =   "01"
                   MOVE    "07"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20010-INIT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-PTID
           MOVE    SPACE               TO  SPA-PTNUM
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM         TO  WRK-PTNUM
               MOVE    SPTID-PTNUM         TO  SPA-PTNUM
               MOVE    SPTID-PTID          TO  SPA-PTID
           ELSE
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
      *
           MOVE    WRK-PTNUM           TO  MDCRES-PATIENTID
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＵＩＤコード取得処理
      *****************************************************************
       20020-ORCAUID-SYORI-SEC        SECTION.
      *
      *    UIDコード処理
           IF      MDCREQ-ORCA-UID   =   SPACE
      *        UIDコード取得処理
               PERFORM 1001-UID-GET-SEC
               MOVE    C-UID               TO  WRK-ORCA-UID
           ELSE
               MOVE    MDCREQ-ORCA-UID   TO  WRK-ORCA-UID
           END-IF
      *    UIDコードをTERMID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
      *
           MOVE    WRK-ORCA-UID        TO  MDCRES-ORCA-UID
      *
           .
       20020-ORCAUID-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    UIDコード取得処理
      *****************************************************************
       1001-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           .
       1001-UID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者・算定履歴情報取得処理
      *****************************************************************
       2001-PTINF-SYORI-SEC        SECTION.
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
           IF      MDCREQ-ORCA-UID   NOT =   SPACE
               PERFORM 2902-PARA-ALLDEL-SEC
               PERFORM 2009-SPATMP-DEL-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        リハビリ等開始日編集処理
               PERFORM 20013-RIHA-SANTEI-HENSYU-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
           ELSE
               MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
               IF      FLG-SYORIEND    =   1
                   MOVE    MDCREQ-ORCA-UID   TO  MDCRES-ORCA-UID
               END-IF
           END-IF
      *    パラメタ登録
           IF      FLG-SYORIEND    =   1
               IF      MDCRES-ORCA-UID   NOT =   SPACE
                   PERFORM 2902-APIPARA-INS-SEC
               END-IF
           ELSE
               PERFORM 2901-PARA-INS-SEC
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 2008-SPATMP-INS-SEC
               END-IF
           END-IF
      *
           .
       2001-PTINF-SYORI-EXT.
           EXIT.
      ****************************************************************
      *    患者番号判定共通処理
      ****************************************************************
       20011-PTINF-INITCHK-SEC      SECTION.
      *
      *    患者番号判定
           IF     (FLG-SYORI       NOT =   1              )
             AND  (SPA-PTNUM       NOT =   WRK-PARA-PTNUM )
               MOVE    "22"                TO  WRK-ERRCD
               MOVE    "01"                TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        患者情報検索
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
                   MOVE    "10"            TO  WRK-ERRCD
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
      *            排他制御なし
                   MOVE    1               TO  FLG-SYORIEND
               END-IF
           END-IF
      *    排他チェック & ロック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "90"            TO  WRK-ERRCD
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
      *            排他制御なし
                   MOVE    1               TO  FLG-SYORIEND
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        患者情報出力内容編集処理
               PERFORM 20021-MDCRES-HEN-SEC
           END-IF
      *
           .
       20011-PTINF-INITCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報出力内容編集処理
      *****************************************************************
       20021-MDCRES-HEN-SEC        SECTION.
      *
      *    患者番号
           MOVE    SPA-PTNUM           TO  MDCRES-PATIENTID
      *    患者情報
           MOVE    PTINF-NAME          TO  MDCRES-NAME
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME
           MOVE    PTINF-SEX           TO  MDCRES-SEX
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY
      *
      *    自宅情報
      *    MOVE    PTINF-HOME-ADRS     TO  MDCRES-HOME-ADDRESSES1
      *    MOVE    PTINF-HOME-BANTI    TO  MDCRES-HOME-ADDRESSES2
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ登録処理
      *****************************************************************
       2901-PARA-INS-SEC        SECTION.
      *
      *    APIパラメタ登録
           PERFORM 2902-APIPARA-INS-SEC
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    SPA保存
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "K01KYOUTU"         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           MOVE    SPA-K01KYOUTU       TO  PARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  PARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  PARA-UPHMS
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
           .
       2901-PARA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    APIパラメタ登録処理
      *****************************************************************
       2902-APIPARA-INS-SEC        SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      APIPARA-REC
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    登録
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    レスポンス番号・電子カルテUID保存
           MOVE    MDCREQ-KARTE-UID    TO  APIPARA-KARTE-UID
           MOVE    WRK-PTNUM           TO  APIPARA-PTNUM
      *
           MOVE    SPACE               TO  WRK-APIV3DATA-REC
           MOVE    MDCRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
           MOVE    WRK-ENDKBN          TO  WRK-PARA-ENDKBN
      *    処理ＰＧ
           MOVE    "05"                TO  WRK-PARA-SYORIPG
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  APIPARA-UPHMS
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
           .
       2902-APIPARA-INS-EXT.
           EXIT.
      *
      ****************************************************************
      *    リハビリ等開始日編集処理
      ****************************************************************
       20013-RIHA-SANTEI-HENSYU-SEC     SECTION.
      *
           INITIALIZE                  SPA-K06
           INITIALIZE                  WRK-SCRAREA
      *
           MOVE    2                   TO  SPA-K061-GMN
           MOVE    "APIV3"             TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK061"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K06
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           INITIALIZE                   MDCRES-SANTEIRRK-TBLINF
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY      FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-K061-MAX )
      *       算定履歴情報編集
              IF      SPA-K061-TSRYYM (IDY)    NOT =   SPACE
                  PERFORM 200131-SANTEI-HENSYU-SEC
              END-IF
           END-PERFORM
      *
      *    算定なし一覧
           PERFORM 200132-NASILIST-SEC
           .
       20013-RIHA-SANTEI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    算定履歴情報編集処理
      **************************************************************
       200131-SANTEI-HENSYU-SEC           SECTION.
      *
      *    算定履歴検索
           MOVE    SPA-K061-TSRYYM (IDY)   TO  WRK-SANTEI-SRYYM
           MOVE    SPA-K061-TSRYCD (IDY)   TO  WRK-SANTEI-SRYCD
           MOVE    SPA-K061-THKNCOMBI(IDY) TO  WRK-SANTEI-HKNCOMBI
           PERFORM 910-SANTEI-KEY-READ-SEC
           IF     (FLG-SANTEI          =   1    )
              OR  (SANTEI-MSANTEICNT   =   ZERO  )
      *        算定履歴なし
               GO      TO      200131-SANTEI-HENSYU-EXT
           END-IF
      *
           ADD     1                       TO  IDX
           MOVE    SPA-K061-TSRYCD (IDY)
                                       TO  MDCRES-TPRSCRPT-CODE (IDX)
           MOVE    SPA-K061-TMEISYO (IDY)
                                       TO  MDCRES-TPRSCRPT-NAME (IDX)
      *    名称なしの時、算定日の１日
           IF      SPA-K061-TMEISYO (IDY)  =   SPACE
               MOVE    SPA-K061-TSRYYM (IDY)   TO  WRK-CHK-YM
               MOVE    "01"                TO  WRK-CHK-DD
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME    TO  MDCRES-TPRSCRPT-NAME (IDX)
               END-IF
           END-IF
      *
      *    算定年月
           MOVE    SPA-K061-TSRYYM (IDY)   TO  WRK-SYMD(1:6)
           MOVE    "01"                    TO  WRK-SYMD(7:2)
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE(1:7)   TO  MDCRES-TPERFORM-YM (IDX)
      *    保険組合せ番号
           IF      SPA-K061-THKNCOMBI(IDY) =   ZERO
                                           OR  SPACE
               MOVE    SPACE           TO  MDCRES-TCOMBINATION-NUMBER
                                                            (IDX)
           ELSE
               MOVE    SPA-K061-THKNCOMBI(IDY)
                                       TO  MDCRES-TCOMBINATION-NUMBER
                                                            (IDX)
           END-IF
      *    保険組合せ番号名称
           MOVE    SPA-K061-THKNCOMBIMEI(IDY)
                                   TO  MDCRES-TCOMBINATION-NAME
                                                            (IDX)
      *    算定日
           MOVE    ZERO                TO  IDD2
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               IF      SANTEI-DAY (IDD)    NOT =   ZERO
                   MOVE    IDD                 TO  WRK-DD
                   ADD     1                   TO  IDD2
                   MOVE    WRK-DD              TO  MDCRES-TPERFORM-DAY
                                                         (IDX IDD2)
               END-IF
           END-PERFORM
      *    登録区分(1：診療行為）
           IF      SPA-K061-TKBNMEI(IDY)(1:2)  =   "診"
               MOVE    "1"                 TO  MDCRES-TOROKU-MODE1
                                                            (IDX)
           END-IF
      *    コメント区分(1：コメントあり）
           IF      SPA-K061-TPLUSKBN(IDY)  =   "1"
               MOVE    "1"                 TO  MDCRES-TOROKU-MODE2
                                                            (IDX)
           END-IF
      *    終了日
           IF      SPA-K061-TENDYMD (IDY)  NOT =   SPACE
               MOVE    SPA-K061-TENDYMD (IDY)  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-TEND-DATE
                                                             (IDX)
           END-IF
      *    終了日区分(1：診療行為）
           IF      SPA-K061-TENDFLG (IDY)  =   "2"
               MOVE    "1"                 TO  MDCRES-TOROKU-MODE3
                                                             (IDX)
           END-IF
           .
       200131-SANTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴算定なし一覧処理
      *****************************************************************
       200132-NASILIST-SEC     SECTION.
      *
           INITIALIZE                  MDCRES-STARTSRYCD-INF
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-RRK-MAX)
                          OR  (IDX     >=  100 )
              MOVE    ZERO             TO  FLG-CHK
              PERFORM VARYING      IDZ     FROM    1   BY  1
                      UNTIL       (IDZ     >   100  )
                             OR   (FLG-CHK     =   1  )
                             OR   (MDCRES-TPRSCRPT-CODE (IDZ)
                                               =   SPACE)
                   IF      TBL-RRK-SRYCD (IDY) =   MDCRES-TPRSCRPT-CODE
                                                                 (IDZ)
                       MOVE    1                   TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   PERFORM 2001321-SRYCD-HENSYU-SEC
               END-IF
           END-PERFORM
           .
       200132-NASILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日コード編集処理
      *****************************************************************
        2001321-SRYCD-HENSYU-SEC     SECTION.
      *
           MOVE    TBL-RRK-SRYCD (IDY) TO  WRK-SRYCD
           MOVE    SPA-SYSYMD          TO  WRK-CHK-YMD
           PERFORM 9001-TENSU-LASTREAD-SEC
           IF      FLG-TENSU           =   1
               GO      TO    2001321-SRYCD-HENSYU-EXT
           END-IF
           ADD     1                   TO  IDX
           MOVE    TBL-RRK-SRYCD (IDY) TO  MDCRES-STARTSRYCD-CODE
                                                            (IDX)
           MOVE    TNS-NAME            TO  MDCRES-STARTSRYCD-NAME
                                                            (IDX)
           IF      TNS-YUKOSTYMD   NOT =   ZERO
               MOVE    TNS-YUKOSTYMD       TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-STARTSRYCD-DATE
                                                             (IDX)
           END-IF
           .
        2001321-SRYCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新確認処理
      *****************************************************************
       2002-SYORI-02-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX-ERR
      *
      *    レスポンス番号チェック
           IF      WRK-RESPONSE-NUMBER =   "02"
               CONTINUE
           ELSE
               MOVE    "09"                TO  WRK-ERRCD
               GO      TO      2002-SYORI-02-EXT
           END-IF
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
      *    ＰＡＲＡのＳＰＡ編集
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2031-SPA-PARA-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    SPACE               TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
      *    項目チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-KOUSIN-CHK-SEC
           END-IF
      *    更新処理
           IF     (WRK-ERRCD           =   SPACE )
              AND (IDX-ERR             =   ZERO  )
               PERFORM 20022-KOUSIN-SYORI-SEC
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE   )
      *        更新処理終了
             AND  (MDCRES-RESPONSE-NUMBER  =   SPACE )
      *        継続
               IF      FLG-CONTKBN         =   1
      *            パラメタ削除
                   PERFORM 2902-PARA-ALLDEL-SEC
                   PERFORM 2009-SPATMP-DEL-SEC
      *            保険登録へ
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
      *            継続区分終了
                   MOVE    "1"             TO  WRK-ENDKBN
      *            APIパラメタ登録
                   PERFORM 2902-APIPARA-INS-SEC
               ELSE
                   MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
      *            パラメタ削除・排他解除処理
                   PERFORM 2900-SYORI-END-SEC
               END-IF
           ELSE
               IF     (MDCRES-RESPONSE-NUMBER  =   SPACE )
                   MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
               IF      FLG-CANSEL      =   ZERO
      *            SPA再編集
                   PERFORM 2901-PARA-INS-SEC
                   PERFORM 2008-SPATMP-INS-SEC
               END-IF
           END-IF
      *
           .
       2002-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       20021-KOUSIN-CHK-SEC         SECTION.
      *
      *    送信内容編集
           PERFORM 200211-MDCREQ-HEN02-SEC
      *
           MOVE    ZERO                TO  IDX-ERR
           INITIALIZE                  WRK-ERRCD-TBLG
      *    基本チェック処理
           PERFORM 200212-INPUT-CHK01-SEC
      *
      *    関連チェック
           IF     IDX-ERR              =   ZERO
              PERFORM 200213-INPUT-CHK02-SEC
           END-IF
           IF      IDX-ERR             >   ZERO
              MOVE    "30"             TO  WRK-ERRCD
           END-IF
      *
           .
       20021-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200212-INPUT-CHK01-SEC         SECTION.
      *
           EVALUATE    MDCREQ-SANTEI-MODE
               WHEN    "New"
               WHEN    "Modify"
               WHEN    "Delete"
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "0001"          TO  WRK-ERRCD-T (IDX-ERR)
           END-EVALUATE
      *
      *    開始日診療コードチェック
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                  UNTIL       (IDX     >   TBL-RRK-MAX )
                          OR  (FLG-CHK =   1           )
               IF      TBL-RRK-SRYCD (IDX) =   MDCREQ-PRSCRPT-CODE
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               ADD     1               TO  IDX-ERR
               MOVE    "0002"          TO  WRK-ERRCD-T (IDX-ERR)
           ELSE
               MOVE    MDCREQ-PRSCRPT-CODE  TO  WRK-SRYCD
           END-IF
      *    算定年月
           MOVE    MDCREQ-PERFORM-YM   TO  WRK-HEN-DATE(1:7)
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD(1:6)       TO  WRK-SRYYM
      *    暦日チェック
           MOVE    WRK-SRYYM           TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 803-ORCSDAY-CHEK-SEC
           IF     (FLG-DAYERR          =   1    )
               ADD     1               TO  IDX-ERR
               MOVE    "0003"          TO  WRK-ERRCD-T (IDX-ERR)
           END-IF
      *    算定日の１日で点数マスタ検索
           IF      IDX-ERR             =   ZERO
               MOVE    WRK-SRYYM           TO  WRK-CHK-YM
               MOVE    "01"                TO  WRK-CHK-DD
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME        TO  MDCRES-PRSCRPT-NAME
               ELSE
                   ADD     1               TO  IDX-ERR
                   MOVE    "0011"          TO  WRK-ERRCD-T (IDX-ERR)
               END-IF
           END-IF
      *
      *    保険組合せ番号数値
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               ADD     1               TO  IDX-ERR
               MOVE    "0004"          TO  WRK-ERRCD-T (IDX-ERR)
               MOVE    MDCREQ-COMBINATION-NUMBER   TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *    算定日
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  WRK-DAY-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                  UNTIL       (IDX     >   31   )
                          OR  (SPA-ERRCD   NOT =  SPACE )
               MOVE    ZERO                TO  WRK-DD
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCRES-PERFORM-DAY(IDX) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0005"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-DD
               END-IF
               IF      WRK-DD          NOT =   ZERO
                   IF      WRK-DD              <=  31
                       MOVE    1                   TO  WRK-DAY (WRK-DD)
                   ELSE
                       MOVE    "0005"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               ADD     1               TO  IDX-ERR
               MOVE    SPA-ERRCD       TO  WRK-ERRCD-T (IDX-ERR)
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
      *    エラーコード編集処理
           PERFORM 200212-SAN-ERRCD-SEC
           .
       200212-INPUT-CHK01-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーコード編集処理
      *****************************************************************
       200212-SAN-ERRCD-SEC         SECTION.
      *
           PERFORM VARYING     IDE     FROM    1   BY  1
                  UNTIL       (IDE     >   50   )
                          OR  (WRK-ERRCD-T(IDE)    =   SPACE)
               MOVE    WRK-ERRCD-T (IDE)   TO  MDCRES-SAN-ERRCD (IDE)
               MOVE    WRK-ERRCD-T (IDE)   TO  SPA-ERRCD
               PERFORM 8901-PTERRCD-SETAPI-SEC
               MOVE    WRK-ERRMSG          TO  MDCRES-SAN-ERRCD-MSG
                                                                (IDE)
               MOVE    SPACE               TO  WRK-ERRMSG
               MOVE    SPACE               TO  SPA-ERRCD
           END-PERFORM
           .
       200212-SAN-ERRCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       200213-INPUT-CHK02-SEC         SECTION.
      *
      *    算定履歴登録チェック
           MOVE    WRK-SRYYM           TO  WRK-SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  WRK-SANTEI-SRYCD
           MOVE    WRK-HKNCOMBI        TO  WRK-SANTEI-HKNCOMBI
           PERFORM 910-SANTEI-KEY-READ-SEC
           IF     (FLG-SANTEI          =   1    )
      *        登録なし
               IF      MDCREQ-SANTEI-MODE  =   "New"
                   CONTINUE
               ELSE
      *            新規以外はエラー
                   ADD     1               TO  IDX-ERR
                   MOVE    "0006"          TO  WRK-ERRCD-T (IDX-ERR)
               END-IF
           ELSE
      *        新規はエラー
               IF      MDCREQ-SANTEI-MODE  =   "New"
                   ADD     1               TO  IDX-ERR
                   MOVE    "0007"          TO  WRK-ERRCD-T (IDX-ERR)
               ELSE
      *            選択あり
                   PERFORM 2002131-SANTEI-HENSYU-SEC
               END-IF
           END-IF
      *
           IF      IDX-ERR             =   ZERO
               EVALUATE    MDCREQ-SANTEI-MODE
                   WHEN    "New"
                   WHEN    "Modify"
      *                算定日チェック
                       PERFORM 2002133-SANTEIDAY-CHECK-SEC
      *                保険組合せ番号
                       IF      WRK-HKNCOMBI    NOT =   ZERO
                           PERFORM 2002132-HKNCOMBI-CHECK-SEC
                       END-IF
      *R01.8
      *                リハビリ開始日のチェック
                       PERFORM 2002133-KANREN-END-CHK-SEC
      *
                   WHEN    SPACE
      *                算定履歴の算定日を編集
                       PERFORM 2002132-SANTEI-DAYHEN-SEC
      *                保険組合せ番号
                       IF      WRK-HKNCOMBI    NOT =   ZERO
                           PERFORM 2002132-HKNCOMBI-CHECK-SEC
                       END-IF
                   WHEN    "Delete"
      *                選択内容編集
                       MOVE    SPA-K061-THKNCOMBIMEI(IDY)
                                       TO  MDCRES-COMBINATION-NAME
      *                算定履歴の算定日を編集
                       PERFORM 2002132-SANTEI-DAYHEN-SEC
               END-EVALUATE
           END-IF
      *    エラーコード編集処理
           PERFORM 200212-SAN-ERRCD-SEC
      *
           .
       200213-INPUT-CHK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信内容編集処理
      *****************************************************************
       200211-MDCREQ-HEN02-SEC         SECTION.
      *
           MOVE    MDCREQ-SANTEI-MODE  TO  MDCRES-SANTEI-MODE
      *
           MOVE    MDCREQ-PRSCRPT-CODE TO  MDCRES-PRSCRPT-CODE
           MOVE    MDCREQ-PRSCRPT-NAME TO  MDCRES-PRSCRPT-NAME
           MOVE    MDCREQ-PERFORM-YM   TO  MDCRES-PERFORM-YM
           MOVE    MDCREQ-COMBINATION-NUMBER
                                       TO  MDCRES-COMBINATION-NUMBER
           MOVE    MDCREQ-PERFORM-DATE-G
                                       TO  MDCRES-PERFORM-DATE-G
      *
           .
       200211-MDCREQ-HEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴算定日編集処理
      *****************************************************************
       2002132-SANTEI-DAYHEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  MDCRES-PERFORM-DATE-G
           MOVE    ZERO                TO  IDD2
           MOVE    ZERO                TO  WRK-DAY-G
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               IF      SANTEI-DAY (IDD)    NOT =   ZERO
                   ADD     1                   TO  IDD2
                   MOVE    IDD                 TO  WRK-DD
                   MOVE    WRK-DD              TO  MDCRES-PERFORM-DAY
                                                             (IDD2)
      *
                   MOVE    1                   TO  WRK-DAY (IDD)
               END-IF
           END-PERFORM
           .
       2002132-SANTEI-DAYHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴内容編集処理
      *****************************************************************
       2002131-SANTEI-HENSYU-SEC         SECTION.
      *
           IF       WRK-HKNCOMBI       =   ZERO
                                       OR  SPACE
               MOVE    SPACE           TO  WRK-CHK-HKNCOMBI
           ELSE
               MOVE    WRK-HKNCOMBI    TO  WRK-CHK-HKNCOMBI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY      FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-K061-MAX )
      *       算定履歴情報履歴
              IF      (SPA-K061-TSRYCD (IDY)   =   WRK-SRYCD )
                AND   (SPA-K061-TSRYYM (IDY)   =   WRK-SRYYM )
                AND   (SPA-K061-THKNCOMBI(IDY) =   WRK-CHK-HKNCOMBI)
                   MOVE    IDY             TO  IDX
                   MOVE    SPA-K061-MAX    TO  IDY
               END-IF
           END-PERFORM
      *
           IF      IDX                 =   ZERO
               ADD     1               TO  IDX-ERR
               MOVE    "0008"          TO  WRK-ERRCD-T (IDX-ERR)
           ELSE
      *        診療行為からの登録は変更不可
               IF      SPA-K061-TKBNMEI(IDX)(1:2)  =   "診"
                   EVALUATE    MDCREQ-SANTEI-MODE
                   WHEN    "Modify"
                   WHEN    "Delete"
                       ADD     1               TO  IDX-ERR
                       MOVE    "0009"          TO  WRK-ERRCD-T(IDX-ERR)
                   END-EVALUATE
               END-IF
      *        コメントあり
               IF      SPA-K061-TPLUSKBN(IDX)  =   "1"
                   MOVE    1                   TO  FLG-COMMTARI
               END-IF
      *        終了日診療行為登録
               IF      SPA-K061-TENDFLG (IDX)  =   "2"
                   MOVE    1                   TO  FLG-ENDDATE
                   MOVE    SPA-K061-TENDYMD (IDX)
                                               TO  WRK-ENDDATE-GMN
               END-IF
      *        終了日コメントあり
               IF     (SPA-K061-TENDYMD (IDX)  NOT =   SPACE )
                 AND  (SPA-K061-TENDFLG (IDX)      =   "1"   )
                   MOVE    1                   TO  FLG-COMMTARI
               END-IF
           END-IF
           .
        2002131-SANTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日チェック処理
      *****************************************************************
       2002133-SANTEIDAY-CHECK-SEC         SECTION.
      *
      *    算定年月の月末
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SRYYM           TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-SRYLST-DD
           ELSE
               MOVE    31                  TO  WRK-SRYLST-DD
           END-IF
      *
           MOVE    ZERO                TO  WRK-LAST-DAY
           PERFORM VARYING     IDD     FROM    1   BY  1
                  UNTIL       (IDD     >   31   )
               IF      WRK-DAY (IDD)   NOT =   ZERO
                   MOVE    IDD             TO  WRK-LAST-DAY
               END-IF
           END-PERFORM
      *
           IF      WRK-LAST-DAY        >   WRK-SRYLST-DD
               ADD     1               TO  IDX-ERR
               MOVE    "0010"          TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       2002133-SANTEIDAY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       2002132-HKNCOMBI-CHECK-SEC         SECTION.
      *
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *    保険組合せ設定チェック
           IF      WRK-SRYCD(1:7)      =   "0998001"
                                       OR  "0998002"
      *        リハビリ開始日のみ
               CONTINUE
           ELSE
               MOVE    "0021"              TO  SPA-ERRCD
           END-IF
      *    保険組合せがキーにない時、エラー
           IF      SPA-ERRCD           =   SPACE
      *        保険組合せ検索編集
               PERFORM 20021321-HKNCOMBI-SET-SEC
               IF      FLG-HKNCOMBI        =   ZERO
      *            労災の時、コメントの追加
                   IF     (COMB-HKNNUM         =   "971"
                                               OR  "973"
                                               OR  "975" )
      *              第三者行為
                      OR  (COMB-KOH1HKNNUM     =   "970" )
      *                算定日との期間チェック
                       PERFORM 20021322-HKNCOMBI-KIKAN-SEC
                   ELSE
                       MOVE    "0023"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0022"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               ADD     1               TO  IDX-ERR
               MOVE    SPA-ERRCD       TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
      *    保険組合せ名称編集
           MOVE    WRK-HKNCOMBIMEI     TO  MDCRES-COMBINATION-NAME
           MOVE    SPACE               TO  SPA-ERRCD
      *
           .
       2002132-HKNCOMBI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ検索編集処理
      *****************************************************************
       20021321-HKNCOMBI-SET-SEC         SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    COMB-TEKSTYMD       TO  WRK-COMB-TEKSTYMD
           MOVE    COMB-TEKEDYMD       TO  WRK-COMB-TEKEDYMD
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
      *        傷病年月日と療養開始日判定
               IF     (PTRSI-SHOBYOYMD     NOT =   SPACE  )  AND
                      (PTRSI-SHOBYOYMD         <   COMB-TEKSTYMD)
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-COMB-TEKSTYMD
               END-IF
           END-IF
      *ver.4.7
      *    第三者行為
           IF      COMB-KOH1HKNNUM     =   "970"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    ZERO                TO  PTRSI-HKNID
               MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        名称編集
               IF      PTRSI-COMMENT   NOT =   SPACE
      *            保険＋公費１のみとなる
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
      *        傷病年月日と療養開始日判定
               IF     (PTRSI-SHOBYOYMD     NOT =   SPACE  )  AND
                      (PTRSI-SHOBYOYMD         <   COMB-TEKSTYMD)
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-COMB-TEKSTYMD
               END-IF
           END-IF
           .
       20021321-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日との期間チェック処理
      *****************************************************************
       20021322-HKNCOMBI-KIKAN-SEC         SECTION.
      *
      *    算定日とする
           MOVE    WRK-SRYYM           TO  WRK-SANTEI-YM
           PERFORM VARYING     IDD     FROM    1   BY  1
                  UNTIL       (IDD     >   31   )
                           OR (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-DAY (IDD)   NOT =   ZERO
                   MOVE    IDD             TO  WRK-SANTEI-DD
      *            期間外
                   IF     (WRK-COMB-TEKSTYMD   <=  WRK-SANTEI-YMD )
                     AND  (WRK-COMB-TEKEDYMD   >=  WRK-SANTEI-YMD )
                       CONTINUE
                   ELSE
                       MOVE    "0024"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           .
       20021322-HKNCOMBI-KIKAN-EXT.
           EXIT.
      *
      *R01.8
      *****************************************************************
      *    リハビリ開始日のチェック処理
      *****************************************************************
       2002133-KANREN-END-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-END-SRYCD
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-RRK-MAX )
                         OR   (FLG-CHK     =   1       )
               IF      TBL-RRK-SRYCD (IDY)
                                           =   WRK-SRYCD
                   MOVE    TBL-END-SRYCD (IDY)
                                           TO  WRK-END-SRYCD
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      WRK-END-SRYCD       =   SPACE
               GO  TO  2002133-KANREN-END-CHK-EXT
           END-IF
      *
           MOVE    WRK-SRYYM           TO  WRK-SANTEI-SRYYM
           MOVE    WRK-END-SRYCD       TO  WRK-SANTEI-SRYCD
           MOVE    WRK-HKNCOMBI        TO  WRK-SANTEI-HKNCOMBI
           PERFORM 910-SANTEI-KEY-READ-SEC
           IF      (FLG-SANTEI         =   ZERO  )
               PERFORM VARYING     IDD     FROM    1   BY  1
                      UNTIL       (IDD     >   31   )
                               OR (SPA-ERRCD   NOT =   SPACE )
                   IF     (WRK-DAY    (IDD)    NOT =   ZERO)
                     AND  (SANTEI-DAY (IDD)        >   ZERO)
                       MOVE    "0051"          TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               ADD     1               TO  IDX-ERR
               MOVE    SPA-ERRCD       TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       2002133-KANREN-END-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       20022-KOUSIN-SYORI-SEC         SECTION.
      *
      *    コメント・終了日あり
           IF      FLG-COMMTARI        =   1
               PERFORM 20022-RIHACOMMNT-HEN-SEC
               MOVE    "03"            TO  MDCRES-RESPONSE-NUMBER
               GO      TO      20022-KOUSIN-SYORI-EXT
           END-IF
      *
      *    コメント・終了日チェック
      *    送信内容編集
           PERFORM 200311-MDCREQ-HEN03-SEC
           MOVE    ZERO                TO  FLG-ERRCHK
           PERFORM 200312-INPUT-CHK03-SEC
           IF     (FLG-ERRCHK          =   1    )
              AND (WRK-ERRCD           =   SPACE)
              MOVE    "30"             TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *
      *        算定履歴情報
               PERFORM 200221-SANTEI-KOUSIN-SEC
           END-IF
      *
           .
       20022-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴情報処理
      *****************************************************************
       200221-SANTEI-KOUSIN-SEC         SECTION.
      *
      *    算定履歴情報
           EVALUATE    MDCREQ-SANTEI-MODE
               WHEN    "Delete"
                   PERFORM 200221-SANTEI-DELETE-SEC
               WHEN    "Modify"
                   PERFORM 200222-SANTEI-UPDATE-SEC
                   IF     (WRK-ERRCD           =   SPACE    )
      *                算定履歴付加情報更新
                       PERFORM 200231-SANTEI-UPDATE03-SEC
                   END-IF
               WHEN    "New"
                   PERFORM 200223-SANTEI-ADD-SEC
                   IF     (WRK-ERRCD           =   SPACE    )
      *                算定履歴付加情報更新
                       PERFORM 200231-SANTEI-UPDATE03-SEC
                   END-IF
               WHEN    SPACE
                   IF      MDCREQ-COMMNT-MODE  =   "Modify"
      *                算定履歴付加情報更新
                       PERFORM 200231-SANTEI-UPDATE03-SEC
                   ELSE
      *                次へ
                       MOVE    "03"        TO  MDCRES-RESPONSE-NUMBER
                   END-IF
           END-EVALUATE
      *
           .
       200221-SANTEI-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴削除処理
      *****************************************************************
       200221-SANTEI-DELETE-SEC         SECTION.
      *
           MOVE    WRK-SRYYM           TO  WRK-SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  WRK-SANTEI-SRYCD
           MOVE    WRK-HKNCOMBI        TO  WRK-SANTEI-HKNCOMBI
           PERFORM 910-SANTEI-KEY-READ-SEC
           IF      FLG-SANTEI          =   ZERO
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "61"                    TO  WRK-ERRCD
               END-IF
               MOVE    1                   TO  FLG-DELETE-OK
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE )
      *        リハビリのコメント削除
               PERFORM 2002211-SANTEIPLUS-DEL-SEC
            END-IF
      *
           .
       200221-SANTEI-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    コメント削除　処理
      *****************************************************************
       2002211-SANTEIPLUS-DEL-SEC      SECTION.
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    WRK-SANTEI-SRYCD    TO  SANTEIPLUS-SRYCD
           MOVE    WRK-SANTEI-SRYYM    TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           IF      WRK-SANTEI-HKNCOMBI =   SPACE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    WRK-SANTEI-HKNCOMBI
                                           TO  SANTEIPLUS-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "62"                    TO  WRK-ERRCD
           END-IF
      *
           .
       2002211-SANTEIPLUS-DEL-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       200222-SANTEI-UPDATE-SEC         SECTION.
      *
           MOVE    WRK-SRYYM           TO  WRK-SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  WRK-SANTEI-SRYCD
           MOVE    WRK-HKNCOMBI        TO  WRK-SANTEI-HKNCOMBI
           PERFORM 910-SANTEI-KEY-READ-SEC
           IF      FLG-SANTEI          =   1
               MOVE    "32"                    TO  WRK-ERRCD
               GO      TO    200222-SANTEI-UPDATE-EXT
           END-IF
      *
           MOVE    ZERO                TO  SANTEI-MSANTEICNT
           MOVE    SPACE               TO  SANTEI-MSANTEID
      *    算定回数編集処理
           PERFORM 200223-SANTEI-DAYHEN-SEC
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
           MOVE    WRK-OPID            TO  SANTEI-OPID
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "63"                    TO  WRK-ERRCD
           END-IF
      *
           .
       200222-SANTEI-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       200223-SANTEI-ADD-SEC            SECTION.
      *
           MOVE    WRK-SRYYM           TO  WRK-SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  WRK-SANTEI-SRYCD
           MOVE    WRK-HKNCOMBI        TO  WRK-SANTEI-HKNCOMBI
           PERFORM 910-SANTEI-KEY-READ-SEC
           IF      FLG-SANTEI          =   ZERO
               MOVE    "33"                    TO  WRK-ERRCD
               GO      TO    200223-SANTEI-ADD-EXT
           END-IF
      *
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SANTEI-SRYYM    TO  SANTEI-SRYYM
           MOVE    WRK-SANTEI-SRYCD    TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      WRK-SANTEI-HKNCOMBI =   ZERO
                                       OR  SPACE
               MOVE    ZERO            TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    WRK-SANTEI-HKNCOMBI
                                       TO  SANTEI-HKNCOMBINUM
           END-IF
      *    算定回数編集処理
           PERFORM 200223-SANTEI-DAYHEN-SEC
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
           MOVE    WRK-OPID            TO  SANTEI-OPID
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "65"                    TO  WRK-ERRCD
           END-IF
      *
           .
       200223-SANTEI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数編集処理
      *****************************************************************
       200223-SANTEI-DAYHEN-SEC          SECTION.
      *
      *    初回算定日編集
           IF       WRK-SANTEI-HKNCOMBI    =   ZERO
                                           OR  SPACE
               MOVE    SPACE               TO  WRK-CHK-HKNCOMBI
           ELSE
               MOVE    WRK-SANTEI-HKNCOMBI TO  WRK-CHK-HKNCOMBI
           END-IF
           MOVE    SPACE               TO  WRK-FSANTEIYMD
           PERFORM VARYING     IDY      FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-K061-MAX )
      *       同じコードの初回
              IF      (SPA-K061-TSRYCD (IDY)   =   WRK-SRYCD )
                 AND  (SPA-K061-TSYOYMD(IDY)   <   WRK-SANTEI-SRYYM)
                 AND  (SPA-K061-THKNCOMBI(IDY) =   WRK-CHK-HKNCOMBI)
                   IF     (WRK-FSANTEIYMD  =   SPACE    )
                     OR   (SPA-K061-TSYOYMD (IDY)
                                           <   WRK-FSANTEIYMD)
                       MOVE    SPA-K061-TSYOYMD (IDY)
                                           TO  WRK-FSANTEIYMD
                   END-IF
               END-IF 
           END-PERFORM
      *
      *    回数設定
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       (IDD     >   31   )
               IF      WRK-DAY (IDD)       =   ZERO
                   MOVE    ZERO                TO  SANTEI-DAY (IDD)
               ELSE
                   MOVE    1                   TO  SANTEI-DAY (IDD)
                   ADD     SANTEI-DAY (IDD)    TO  SANTEI-MSANTEICNT
                   IF      SANTEI-MSANTEID     =   SPACE
                                               OR  ZERO
                       MOVE    IDD             TO  WRK-DD
                       MOVE    WRK-DD          TO  SANTEI-MSANTEID
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-FSANTEIYMD      =   SPACE
               MOVE    WRK-SANTEI-SRYYM    TO  WRK-FSANTEIYMD(1:6)
               MOVE    SANTEI-MSANTEID     TO  WRK-FSANTEIYMD(7:2)
           END-IF
           MOVE    WRK-FSANTEIYMD      TO  SANTEI-FSANTEIYMD
      *
           .
       200223-SANTEI-DAYHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント・終了日編集処理
      *****************************************************************
       20022-RIHACOMMNT-HEN-SEC         SECTION.
      *
           MOVE    WRK-SRYYM           TO  WRK-SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  WRK-SANTEI-SRYCD
           MOVE    WRK-HKNCOMBI        TO  WRK-SANTEI-HKNCOMBI
           PERFORM 910-SANTEI-KEY-READ-SEC
           IF      FLG-SANTEI          =   1
               MOVE    "32"                TO  WRK-ERRCD
               GO      TO    20022-RIHACOMMNT-HEN-EXT
           END-IF
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SANTEI-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    SANTEI-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYCD        TO  SANTEIPLUS-SRYCD
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    SANTEI-SRYKA        TO  SANTEIPLUS-SRYKA
           MOVE    SANTEI-NYUGAIKBN    TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SANTEI-HKNCOMBINUM  TO  SANTEIPLUS-HKNCOMBINUM
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
           MOVE    ZERO                TO  IDD
           MOVE    ZERO                TO  IDD2
           MOVE    ZERO                TO  WRK-DAYKEY
           PERFORM UNTIL   FLG-SANTEIPLUS  =   1
               IF      SANTEIPLUS-DAYKEY   NOT =   WRK-DAYKEY
                   MOVE    ZERO            TO  IDD2
                   ADD     1               TO  IDD
                   MOVE    SANTEIPLUS-DAYKEY   TO  MDCRES-COMMNT-DAY
                                                           (IDD)
               END-IF
               IF      SANTEIPLUS-RENNUM   =   ZERO
      *            終了日
                   MOVE    SANTEIPLUS-ENDYMD   TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO  MDCRES-END-DATE
                                                           (IDD)
               ELSE
                   ADD     1               TO  IDD2
                   MOVE    SANTEIPLUS-COMMENT
                                           TO  MDCRES-COMMNT(IDD IDD2)
      *            終了日
                   IF      SANTEIPLUS-ENDYMD   NOT =   SPACE
                       MOVE    SANTEIPLUS-ENDYMD   TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE    TO  MDCRES-COM-ENDDATE
                                                           (IDD IDD2)
                   END-IF
               END-IF
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20022-RIHACOMMNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新確認処理
      *****************************************************************
       2003-SYORI-03-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX-ERR
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
      *    ＰＡＲＡのＳＰＡ編集
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2031-SPA-PARA-SEC
           END-IF
      *    項目チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-KOUSIN-CHK-SEC
           END-IF
      *    更新処理
           IF     (WRK-ERRCD           =   SPACE )
              AND (IDX-ERR             =   ZERO  )
               PERFORM 20032-KOUSIN-SYORI-SEC
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE   )
      *        継続
               IF      FLG-CONTKBN         =   1
      *            パラメタ削除
                   PERFORM 2902-PARA-ALLDEL-SEC
                   PERFORM 2009-SPATMP-DEL-SEC
      *            保険登録へ
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
      *            継続区分終了
                   MOVE    "1"             TO  WRK-ENDKBN
      *            APIパラメタ登録
                   PERFORM 2902-APIPARA-INS-SEC
               ELSE
                   MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
      *            パラメタ削除・排他解除処理
                   PERFORM 2900-SYORI-END-SEC
               END-IF
           ELSE
               IF     (MDCRES-RESPONSE-NUMBER  =   SPACE )
                   MOVE    "03"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
               IF      FLG-CANSEL      =   ZERO
      *            SPA再編集
                   PERFORM 2901-PARA-INS-SEC
                   PERFORM 2008-SPATMP-INS-SEC
               END-IF
           END-IF
      *
           .
       2003-SYORI-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       20031-KOUSIN-CHK-SEC         SECTION.
      *
      *    送信内容編集
           PERFORM 200211-MDCREQ-HEN02-SEC
           PERFORM 200311-MDCREQ-HEN03-SEC
      *
           MOVE    ZERO                TO  IDX-ERR
           INITIALIZE                  WRK-ERRCD-TBLG
           MOVE    ZERO                TO  FLG-ERRCHK
      *    基本チェック処理
           PERFORM 200212-INPUT-CHK01-SEC
      *    関連チェック
           IF     IDX-ERR              =   ZERO
              PERFORM 200213-INPUT-CHK02-SEC
           END-IF
      *    コメント基本チェック処理
           PERFORM 200312-INPUT-CHK03-SEC
      *
           IF     (IDX-ERR             >   ZERO )
              OR  (FLG-ERRCHK          =   1    )
              MOVE    "30"             TO  WRK-ERRCD
           END-IF
      *
           .
       20023-KOUSIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    送信内容編集処理
      *****************************************************************
       200311-MDCREQ-HEN03-SEC         SECTION.
      *
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       (IDD     >   31   )
               IF      MDCREQ-COMMNT-DAY (IDD) NOT =   SPACE
                   MOVE    MDCREQ-COMMNT-DAY (IDD)
                                       TO  MDCRES-COMMNT-DAY(IDD)
                   MOVE    MDCREQ-END-DATE (IDD)
                                       TO  MDCRES-END-DATE (IDD)
                   MOVE    MDCREQ-COMMNT-TBL-G (IDD)
                                       TO  MDCRES-COMMNT-TBL-G(IDD)
               END-IF
           END-PERFORM
      *
           .
       200311-MDCREQ-HEN03-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200312-INPUT-CHK03-SEC         SECTION.
      *
           EVALUATE    MDCREQ-COMMNT-MODE
               WHEN    "Modify"
                   PERFORM 200312-COMMNT-INCHK-SEC
               WHEN    SPACE
                   EVALUATE    MDCREQ-SANTEI-MODE
                   WHEN    "New"
                   WHEN    "Modify"
                       PERFORM 200312-COMMNT-INCHK-SEC
                   END-EVALUATE
               WHEN    OTHER
                   MOVE    "34"            TO  WRK-ERRCD
           END-EVALUATE
           .
       200312-INPUT-CHK03-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200312-COMMNT-INCHK-SEC         SECTION.
      *
      *    コメント登録可
           IF      WRK-SRYCD(1:7)      =   "0998001"
                                       OR  "0998002"
               MOVE    1                   TO  FLG-COMM-OK
           ELSE
               MOVE    ZERO                TO  FLG-COMM-OK
           END-IF
      *
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL      (IDD     >   31   )
                          OR  (WRK-ERRCD   NOT =   SPACE )
               IF      MDCREQ-COMMNT-DAY (IDD)
                                           =   SPACE
                                           OR  ZERO
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   PERFORM 2003121-COMMNT-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    SPA-ERRCD       TO  MDCRES-COM-ERRCD
                                                                (IDD)
                       PERFORM 8901-PTERRCD-SETAPI-SEC
                       MOVE    WRK-ERRMSG      TO  MDCRES-COM-ERRCD-MSG
                                                                (IDD)
                       MOVE    SPACE           TO  WRK-ERRMSG
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    1               TO  FLG-ERRCHK
                   END-IF
               END-IF
           END-PERFORM
           .
       200312-COMMNT-INCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       2003121-COMMNT-CHK-SEC         SECTION.
      *
      *    算定日チェック
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-COMMNT-DAY (IDD) TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      SNUM-RC         NOT =   ZERO
               MOVE    "0201"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-COMMNT-DAY (IDD)
           END-IF
      *    算定履歴に設定があること
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-COMMNT-DAY (IDD)    TO  IDC
               IF      WRK-DAY (IDC)       =   ZERO
                   MOVE    "0202"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    算定日設定
           MOVE    WRK-SANTEI-SRYYM        TO  WRK-CHK-YM
           MOVE    WRK-COMMNT-DAY (IDD)    TO  WRK-CHK-DD
      *    終了日チェック
           IF      SPA-ERRCD           =   SPACE
               IF      MDCREQ-END-DATE (IDD)   NOT =   SPACE
                   MOVE    MDCREQ-END-DATE (IDD)   TO  WRK-HEN-DATE
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD                TO  WRK-END-DATE
                                                             (IDD)
      *            暦日チェック
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1    )
                       MOVE    "0203"          TO  SPA-ERRCD
                   END-IF
      *            算定開始日以降であること
                   IF      WRK-CHK-YMD         >=  WRK-END-DATE
                                                             (IDD)
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "0204"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            診療行為画面から登録
                   IF      FLG-ENDDATE         =   1
                       IF      WRK-END-DATE(IDD)   >=  WRK-ENDDATE-GMN
                           MOVE    "0208"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    コメントチェック
           PERFORM VARYING     IDD2    FROM    1   BY  1
                   UNTIL      (IDD2    >   50          )
                           OR (SPA-ERRCD   NOT =   SPACE)
               IF      MDCREQ-COMMNT (IDD IDD2)    NOT =   SPACE
      *            コメント登録不可
                   IF      FLG-COMM-OK         =   ZERO
                       MOVE    "0207"              TO  SPA-ERRCD
                   ELSE
      *                全角チェック
                       MOVE    MDCREQ-COMMNT(IDD IDD2) TO  WRK-MAE-INPUT
                       PERFORM 890-HALF-ZEN-SEC
                       IF      FLG-ERR             =   ZERO
                           MOVE    WRK-ATO-INPUT   TO  WRK-COMMNT
                                                              (IDD IDD2)
                       ELSE
                           MOVE    "0205"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *        終了日
               IF      MDCREQ-COM-END-DATE (IDD IDD2)    NOT =   SPACE
                   MOVE    MDCREQ-COM-END-DATE(IDD IDD2)
                                                   TO  WRK-HEN-DATE
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD                TO  WRK-COM-END-DATE
                                                            (IDD IDD2)
      *            暦日チェック
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1    )
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "0203"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            算定開始日以降であること
                   IF      WRK-CHK-YMD         >=  WRK-COM-END-DATE
                                                         (IDD IDD2)
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "0204"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            コメント存在すること
                   IF      MDCREQ-COMMNT (IDD IDD2)    =   SPACE
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "0206"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    コメントチェック
           MOVE    SPACE               TO  WRK-LST-ENDDATE
           MOVE    ZERO                TO  FLG-COMEND
           PERFORM VARYING     IDD2    FROM    1   BY  1
                   UNTIL      (IDD2    >   50          )
                           OR (SPA-ERRCD   NOT =   SPACE)
               IF      MDCREQ-COMMNT (IDD IDD2)    NOT =   SPACE
                   IF      MDCREQ-COM-END-DATE (IDD IDD2)
                                                       =   SPACE
                       MOVE    1                   TO  FLG-COMEND
                   ELSE
                       IF      MDCREQ-COM-END-DATE(IDD IDD2)
                                               >   WRK-LST-ENDDATE
                           MOVE    MDCREQ-COM-END-DATE(IDD IDD2)
                                                   TO  WRK-LST-ENDDATE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD           =   SPACE
               IF      (FLG-COMEND         =   ZERO)
                   AND (WRK-LST-ENDDATE    NOT =   SPACE )
                   AND (WRK-END-DATE (IDD)     =   SPACE )
      *            履歴終了日なし
                   MOVE    "0209"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2003121-COMMNT-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    コメント更新処理
      ****************************************************************
       20032-KOUSIN-SYORI-SEC           SECTION.
      *
      *    算定履歴情報
           EVALUATE    MDCREQ-SANTEI-MODE
               WHEN    "Delete"
                   PERFORM 200221-SANTEI-DELETE-SEC
               WHEN    "Modify"
                   PERFORM 200222-SANTEI-UPDATE-SEC
                   IF      WRK-ERRCD           =   SPACE
      *                算定履歴付加情報更新
                       PERFORM 200231-SANTEI-UPDATE03-SEC
                   END-IF
               WHEN    "New"
                   PERFORM 200223-SANTEI-ADD-SEC
                   IF      WRK-ERRCD           =   SPACE
      *                算定履歴付加情報更新
                       PERFORM 200231-SANTEI-UPDATE03-SEC
                   END-IF
               WHEN    SPACE
                   IF      MDCREQ-COMMNT-MODE  =   "Modify"
      *                算定履歴付加情報更新
                       PERFORM 200231-SANTEI-UPDATE03-SEC
                   END-IF
           END-EVALUATE
      *
           .
       20032-KOUSIN-SYORI-EXT.
           EXIT.
      *
      ****************************************************************
      *    コメント登録処理
      ****************************************************************
       200231-SANTEI-UPDATE03-SEC           SECTION.
      *
      *    コメント削除
           PERFORM 2002211-SANTEIPLUS-DEL-SEC
      *
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL      (IDD     >   31   )
                          OR  (WRK-ERRCD   NOT =   SPACE )
                          OR  (WRK-COMMNT-DAY (IDD)    =   ZERO)
               MOVE    WRK-COMMNT-DAY(IDD)     TO  WRK-DAYKEY
               MOVE    ZERO                    TO  WRK-SANTEI-RENNUM
               IF      WRK-END-DATE (IDD)  NOT =   SPACE
      *            履歴終了日登録
                   MOVE    WRK-END-DATE (IDD)      TO  WRK-SANTEI-ENDYMD
                   MOVE    ZERO                    TO  WRK-SANTEI-RENNUM
                   MOVE    SPACE                   TO  WRK-SANTEI-COMMNT
                   PERFORM 200231-SANTEIPLUS-ADD-SEC
               END-IF
               PERFORM VARYING     IDD2    FROM    1   BY  1
                       UNTIL      (IDD2    >   50          )
                               OR (WRK-ERRCD   NOT =   SPACE)
                   IF      WRK-COMMNT (IDD IDD2)   NOT =   SPACE
                       ADD     1               TO  WRK-SANTEI-RENNUM
                       MOVE    WRK-COMMNT (IDD IDD2)
                                               TO  WRK-SANTEI-COMMNT
                       MOVE    WRK-COM-END-DATE (IDD IDD2)
                                               TO  WRK-SANTEI-ENDYMD
                       PERFORM 200231-SANTEIPLUS-ADD-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       200231-SANTEI-UPDATE03-EXT.
           EXIT.
      *
      ****************************************************************
      *    算定履歴付加情報登録処理
      ****************************************************************
       200231-SANTEIPLUS-ADD-SEC           SECTION.
      *
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SANTEI-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    SANTEI-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYCD        TO  SANTEIPLUS-SRYCD
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    SANTEI-SRYKA        TO  SANTEIPLUS-SRYKA
           MOVE    SANTEI-NYUGAIKBN    TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SANTEI-HKNCOMBINUM  TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    WRK-DAYKEY          TO  SANTEIPLUS-DAYKEY
           MOVE    WRK-SANTEI-RENNUM   TO  SANTEIPLUS-RENNUM
           MOVE    WRK-SANTEI-ENDYMD   TO  SANTEIPLUS-ENDYMD
           MOVE    WRK-SANTEI-COMMNT   TO  SANTEIPLUS-COMMENT
      *
           MOVE    WRK-OPID            TO  SANTEIPLUS-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SANTEIPLUS-CREYMD
           MOVE    SMCNDATE-YMD        TO  SANTEIPLUS-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEIPLUS-UPHMS
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "61"                TO  SPA-ERRCD
           END-IF
           .
       200231-SANTEIPLUS-ADD-EXT.
           EXIT.
      *
      ****************************************************************
      *    ＰＡＲＡのＳＰＡ編集処理
      ****************************************************************
       2031-SPA-PARA-SEC           SECTION.
      *
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "K01KYOUTU"         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           PERFORM 9010-PARA-KEYREAD-SEC
           IF      FLG-PARA            =   ZERO
               MOVE    PARA-DATA-REC       TO  SPA-K01KYOUTU
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K06"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-K06
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
           .
       2031-SPA-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ編集処理
      *****************************************************************
      *292-SELECT-HEN-SEC            SECTION.
      *
      *    MOVE    SPA-PIDCD           TO  MDCRES-SELECT-CODE
      *    PERFORM 8902-PTPIDCD-SET-SEC
      *    MOVE    WRK-ERRMSG          TO  MDCRES-SELECT-MSG
      *    MOVE    SPACE               TO  WRK-ERRMSG
      *    .
      *292-SELECT-HEN-EXT.
      *    EXIT.
      *H29.6
      *API
      *****************************************************************
      *    API 処理
      *****************************************************************
       2010-API-SYORI-SEC            SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *    患者番号チェック
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           ELSE
      *        患者番号チェック
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        患者情報検索
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
                   MOVE    "10"            TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        患者情報出力内容編集処理
               PERFORM 20021-MDCRES-HEN-SEC
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2010-API-SYORI-EXT
           END-IF
      *
      *    リハビリ等開始日編集処理
           PERFORM 20013-RIHA-SANTEI-HENSYU-SEC
      *    履歴算定なし一覧処理はクリア
           INITIALIZE                  MDCRES-STARTSRYCD-INF
      *
           IF     (MDCREQ-PRSCRPT-CODE NOT =   SPACE )
              AND (MDCREQ-PERFORM-YM   NOT =   SPACE )
      *        リハビリコメント編集
               PERFORM 20101-API-COMMNT-SEC
           END-IF
           .
       2010-API-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    API リハビリコメント編集処理
      *****************************************************************
       20101-API-COMMNT-SEC            SECTION.
      *
      *    送信内容編集
           MOVE    SPACE               TO  MDCREQ-SANTEI-MODE
           MOVE    SPACE               TO  MDCRES-SANTEI-MODE
           MOVE    MDCREQ-PRSCRPT-CODE TO  MDCRES-PRSCRPT-CODE
           MOVE    MDCREQ-PRSCRPT-NAME TO  MDCRES-PRSCRPT-NAME
           MOVE    MDCREQ-PERFORM-YM   TO  MDCRES-PERFORM-YM
           MOVE    MDCREQ-COMBINATION-NUMBER
                                       TO  MDCRES-COMBINATION-NUMBER
      *
           MOVE    ZERO                TO  IDX-ERR
           INITIALIZE                  WRK-ERRCD-TBLG
      *    基本チェック処理
           PERFORM 200212-INPUT-CHK01-SEC
      *
           IF     IDX-ERR              =   ZERO
      *        算定履歴登録チェック
               MOVE    WRK-SRYYM           TO  WRK-SANTEI-SRYYM
               MOVE    WRK-SRYCD           TO  WRK-SANTEI-SRYCD
               MOVE    WRK-HKNCOMBI        TO  WRK-SANTEI-HKNCOMBI
               PERFORM 910-SANTEI-KEY-READ-SEC
               IF     (FLG-SANTEI          =   1    )
                   ADD     1               TO  IDX-ERR
                   MOVE    "0006"          TO  WRK-ERRCD-T (IDX-ERR)
               END-IF
      *        エラーコード編集処理
               PERFORM 200212-SAN-ERRCD-SEC
           END-IF
           IF     IDX-ERR              =   ZERO
               PERFORM 20022-RIHACOMMNT-HEN-SEC
           END-IF
           IF      IDX-ERR             >   ZERO
              MOVE    "30"             TO  WRK-ERRCD
           END-IF
           .
       20101-API-COMMNT-EXT.
           EXIT.
      *API
      *
      *****************************************************************
      *    終了処理（PARA削除・排他解除）
      *****************************************************************
       2900-SYORI-END-SEC            SECTION.
      *
           PERFORM 2902-PARA-ALLDEL-SEC
           PERFORM 2009-SPATMP-DEL-SEC
      *    排他制御解除
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
               MOVE    "81"            TO  WRK-ERRCD
           END-IF
           .
       2900-SYORI-END-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       2902-PARA-ALLDEL-SEC          SECTION.
      *
      *    APIパラメタ削除
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "AP21"              TO  PARA-GYOUMUID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "AP21"              TO  PARA-GYOUMUID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       2902-PARA-ALLDEL-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの登録処理
      *****************************************************************
       2008-SPATMP-INS-SEC            SECTION.
      *
      *    削除
           PERFORM 2009-SPATMP-DEL-SEC
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K06"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K06             TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
           .
       2008-SPATMP-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ一時保存テーブル追加
      *****************************************************************
       20081-SPATMP-INSERT-SEC                SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  SPATMP-UPYMD
           MOVE    SMCNDATE-HMS        TO  SPATMP-UPHMS
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "80"                TO  WRK-ERRCD
           END-IF
           .
       20081-SPATMP-INSERT-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの削除処理
      *****************************************************************
       2009-SPATMP-DEL-SEC            SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SPATMP DELETE ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
           END-IF
      *
           .
       2009-SPATMP-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       4904-PARA-DEL-SEC          SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       4904-PARA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    日付暦日チェック
      *****************************************************************
       803-ORCSDAY-CHEK-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-DAYERR
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE               TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "00000000"          TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "99999999"          TO  WRK-HENYMD
               WHEN    OTHER
      *            日付チェックサブ
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    1                   TO  FLG-DAYERR
                   ELSE
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
      *
           .
       803-ORCSDAY-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       890-HALF-ZEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *    拡張漢字有無
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    WRK-MAE-INPUT       TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM2    NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *H29.9   改行エラー
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "01"
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
      *    WHEN    "02"
      *        MOVE    "患者氏名未設定"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "03"
      *        MOVE    "患者カナ氏名未設定"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "04"
      *        MOVE    "生年月日未設定"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "05"
      *        MOVE    "性別未設定"
      *                                        TO  WRK-ERRMSG
           WHEN    "06"
               MOVE    "電子カルテＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "07"
               MOVE    "オルカＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "08"
               STRING  "初回接続が完了していません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
      **   WHEN    "11"
      *        MOVE    "基準日の暦日エラー"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "12"
      *        MOVE    "基準日は平成２７年４月１日からです。"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "13"
      *        MOVE    "患者番号登録済み"
      *                                        TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "漢字氏名は全角で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "カナ氏名は全角で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               MOVE    "性別は１：男２：女で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "17"
               MOVE    "生年月日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "18"
               STRING  "患者氏名・性別・生年月日が送信内容と違います。"
                       "患者を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "22"
               MOVE    "前回と患者番号が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "30"
               MOVE    "エラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "31"
               MOVE    "コメント・終了日の登録があります。"
                                               TO  WRK-ERRMSG
           WHEN    "32"
               MOVE    "更新対象の算定履歴がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "33"
               MOVE    "既に算定履歴が登録されています。"
                                               TO  WRK-ERRMSG
           WHEN    "34"
               MOVE    "コメント・終了日の更新区分エラー"
                                               TO  WRK-ERRMSG
      *
           WHEN    "61"
               STRING  "算定履歴削除エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "62"
               MOVE    "算定履歴付加情報削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "63"
               MOVE    "算定履歴更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "64"
               MOVE    "算定履歴付加情報更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "65"
               MOVE    "算定履歴追加エラー"
                                               TO  WRK-ERRMSG
      *
           WHEN    "80"
               STRING  "一時データ出力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "81"
               STRING  "解除対象がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
      ******   MOVE    "処理区分未設定"        TO  WRK-ERRMSG
               MOVE    "リクエスト番号不正"    TO  WRK-ERRMSG
           WHEN    "97"
               MOVE   "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別エラーメッセージ
      *****************************************************************
       8901-PTERRCD-SETAPI-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "リハビリ等開始日更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "0002"
               MOVE    "リハビリ等開始日のコードではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "0003"
               MOVE    "算定年月暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "0004"
               MOVE    "保険組合せ番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "0005"
               MOVE    "算定日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "0006"
               MOVE    "対象の算定履歴の登録がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "0007"
               MOVE    "既に算定履歴が登録されています。"
                                               TO  WRK-ERRMSG
           WHEN    "0008"
               MOVE    "算定履歴が対象ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "0009"
               STRING    "診療行為から登録した算定履歴です。"
                         "更新できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           WHEN    "0010"
               STRING    "算定回数の算定日は暦日ではありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           WHEN    "0011"
               STRING    "算定年月は点数マスタの登録がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           WHEN    "0021"
               MOVE    "保険組合せは設定できません。"
                                               TO  WRK-ERRMSG
           WHEN    "0022"
               MOVE    "保険組合せは登録されていません。"
                                               TO  WRK-ERRMSG
           WHEN    "0023"
               STRING  "保険組合せが労災・自賠責ではありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           WHEN    "0024"
               STRING  "保険組合せの適用期間外です。"
                      "算定日を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
      *R01.8
           WHEN    "0051"
               STRING  "算定日でリハビリ終了日が登録されています。"
                       "同日に開始日は登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
      *
           WHEN    "0201"
               MOVE    "コメント算定日数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "0202"
               STRING  "コメント算定日は算定履歴の算定日で"
                       "はありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           WHEN    "0203"
               MOVE    "終了日付暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "0204"
               MOVE    "算定開始日≧終了日付です。"
                                               TO  WRK-ERRMSG
           WHEN    "0205"
               MOVE    "コメント全角エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "0206"
               MOVE    "コメントの入力がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "0207"
               MOVE    "コメント入力ができないコードです。"
                                               TO  WRK-ERRMSG
           WHEN    "0208"
               STRING  "診療行為で算定終了日の登録があります。"
                       "算定終了日より前の日付のみ可能です。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           WHEN    "0209"
               STRING  "すべてのコメントに終了日があります。"
                       "履歴終了日を設定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           END-EVALUATE
            .
       8901-PTERRCD-SETAPI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-PTINF-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込処理
      *****************************************************************
       910-PTINF-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       910-PTINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込処理
      *****************************************************************
       910-PTNUM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTNUM
               MOVE    MCPDATA-REC         TO  PTNUM-REC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定履歴検索主キー処理
      *****************************************************************
       910-SANTEI-KEY-READ-SEC         SECTION.
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SANTEI-SRYYM    TO  SANTEI-SRYYM
           MOVE    WRK-SANTEI-SRYCD    TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      WRK-SANTEI-HKNCOMBI =   ZERO
                                       OR  SPACE
               MOVE    ZERO            TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    WRK-SANTEI-HKNCOMBI
                                       TO  SANTEI-HKNCOMBINUM
           END-IF
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-SANTEI-KEY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込処理
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SANTEI
               MOVE    MCPDATA-REC         TO  SANTEI-REC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
      *    電子カルテキー
           MOVE    MDCREQ-KARTE-UID    TO  SLOCK-KARTE-UID
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理(削除)
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEYREAD-SEC      SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-CHK-YMD         TO  TNS-YUKOSTYMD
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（最終履歴）
      *****************************************************************
       9001-TENSU-LASTREAD-SEC         SECTION.
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9001-TENSU-LASTREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       940-HKNCOMBI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    算定歴付加マスタ読み込み
      *****************************************************************
       920-SANTEIPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEIPLUS-REC
               MOVE    ZERO                TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                      SANTEIPLUS-REC
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       920-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-PARA-KEYREAD-SEC         SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-PARA-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-APIPARA-KEYREAD-SEC         SECTION.
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  APIPARA-REC
                   MOVE    ZERO            TO  FLG-APIPARA
               ELSE
                   INITIALIZE                  APIPARA-REC
                   MOVE    1               TO  FLG-APIPARA
               END-IF
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-APIPARA-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 910-SPATMP-CALL-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA2-REC    TO  SPATMP-REC
                   MOVE    ZERO            TO  FLG-SPATMP
               ELSE
                   INITIALIZE                  SPATMP-REC
                   MOVE    1               TO  FLG-SPATMP
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルCALL処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
