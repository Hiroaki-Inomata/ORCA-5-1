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
       PROGRAM-ID.         ORAPI021NS2V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院診療行為　診療内容チェック
      *                                          　（入力一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/12/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-WKSRYACT            PIC 9(01).
      **** 03  FLG-PTNYUINRRK          PIC 9(01).
      *****03  FLG-HKNNUM              PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
           03  FLG-GENERICNAME         PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
      *    03  FLG-SRYACCT             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HENCHK              PIC 9(01).
           03  FLG-END-SYORI           PIC 9(01).
           03  FLG-END-SYORI2          PIC 9(01).
      *
           03  FLG-DELCHK              PIC 9(01).
           03  FLG-KENSADEL            PIC 9(01).
           03  FLG-KEIMSG              PIC 9(01).
           03  FLG-SYOHOU              PIC 9(01).
           03  FLG-NYUIN               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-INGAI               PIC 9(01).
      *
           03  FLG-DOUNYUIN            PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-NEXTTUKI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDD                 PIC 9(02).
      *
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
           03  IDM1                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-ERR             PIC 9(04).
           03  IDX-A               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDN1                PIC 9(04).
           03  IDN3                PIC 9(04).
           03  IDN4                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
           03  CNT-WKSRY               PIC 9(04).
      *
      *    一時領域
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
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
      *    エラーコード
           03  WRK-ERRCD-G.
      *        エラー種類
               05  WRK-ERRKBN              PIC X(01).
      *        エラーコード
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(200).
           03  WRK-ERRMSG-02           PIC X(200).
           03  WRK-MEISYO              PIC X(200).
           03  WRK-ERRMSG1             PIC X(200).
           03  WRK-ERRMSG2             PIC X(200).
      *    内容エラー
           03  WRK-MD-ERRCD            PIC X(04).
           03  WRK-MD-ERRMSG.
               05  WRK-MD-ERRMSG1          PIC X(100).
               05  WRK-MD-ERRMSG2          PIC X(100).
           03  WRK-MD-ERR-LINE         PIC X(03).
           03  WRK-MD-ERR-IDX          PIC X(02).
           03  WRK-MD-SRYCD            PIC X(09).
      *    警告メッセージ
           03  WRK-KEIMSG.
               05  WRK-KEIMSG1             PIC X(100).
               05  WRK-KEIMSG2             PIC X(100).
      *
           03  WRK-MCP-WINDOW          PIC X(64).
      *
           03  WRK-OPID                PIC X(16).
           03  WRK-ORCA-UID            PIC X(64).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
      *    保険組合せ内容
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *    算定履歴用
           03  WRK-YMD.
               05  WRK-YM              PIC X(06).
               05  WRK-DD              PIC X(02).
           03  WRK-DD-1                PIC 9(02).
      *
           03  WRK-SINSATU             PIC X(01).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-ERR-SRYCD           PIC X(09).
           03  WRK-ERR-INPUTCD         PIC X(09).
      *    剤内容
           03  WRK-KINGAK              PIC 9(08).
      *
           03  WRK-TOROKU              PIC X(01).
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
      *
           03  WRK-H-LINE              PIC X(03).
           03  WRK-H-IDX               PIC X(02).
      *
           03  WRK-KIDMSG              PIC X(100).
           03  WRK-MAE-KIDCD           PIC X(04).
      *    訂正
           03  WRK-MOD-SRYKA           PIC X(02).
           03  WRK-MOD-HKNCOMBI        PIC X(04).
           03  WRK-SYORI-MODE          PIC X(03).
      *
           03  WRK-TDD                 PIC 9(02).
      *
           03  WRK-DELRENNUM           PIC 9(02).
           03  WRK-ZAINUM              PIC 9(08).
      *    一般名
           03  WRK-GENECHG-KBN         PIC X(01).
           03  WRK-IPN-KISAIKBN        PIC X(01).
      *    包括検査数
           03  WRK-HOUKNS-CNT          PIC 9(02).
      *
           03  WRK-KAISU               PIC 9(03).
      *
      *    入院日
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SRYDD               PIC 9(02).
      *   入院回数・日数
           03  WRK-NYUIN-AREA.
               05  WRK-NYU-KAI        PIC 9(03).
               05  WRK-NYUINDAY       PIC 9(01)    OCCURS  31.
           03  WRK-NYUIN-CHK-AREA.
               05  WRK-NYUIN-KAI       PIC 9(03).
           03  IDX-N2                  PIC 9(02).
           03  WRK-NYU-KAISU           PIC 9(03).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-TIME8       PIC X(08).
      *
           03  WRK-PERFORM-YMD         PIC X(08).
           03  WRK-PERFORM-TIME.
               05  WRK-PERFORM-THH     PIC X(02).
               05  WRK-PERFORM-TMM     PIC X(02).
               05  WRK-PERFORM-TSS     PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-RENNUM              PIC X(01).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
      *
      *    警告メッセージ
       01  TBL-KEIMSG-AREA.
           03  TBL-KEIMSG-G            OCCURS  400.
               05  TBL-KEIERRCD            PIC X(04).
               05  TBL-KEIMSG.
                   07  TBL-KEIMSG1             PIC X(100).
                   07  TBL-KEIMSG2             PIC X(100).
               05  TBL-KEI-SRYCD           PIC X(09).
               05  TBL-KEI-LINE            PIC X(03).
               05  TBL-KEI-IDX             PIC X(02).
           03  IDX-KEI                     PIC 9(03).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
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
      *        処理区分
               05  WRK-PARA-MODE           PIC X(03).
      *        選択コード
               05  WRK-SELECT-CODE         PIC X(04).
      *        訂正対象
               05  WRK-PARA-SRYKA          PIC X(02).
               05  WRK-PARA-HKNCOMBI       PIC X(04).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *受取剤数
       01  COB-ZAI-INMAX           PIC 9(04)   VALUE   110.
      *返却剤数
       01  COB-ZAI-OUTMAX          PIC 9(04)   VALUE   120.
      *
      *    入力コード項目桁数
       01  CONS-INPUT              PIC 9(02)   VALUE   54.
      *
      *    ワーク診療行為マスタ−
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *    API用剤削除連番
           03  LNK-WKSRYAPI-RENNUM         PIC 9(02).
      *    剤削除区分
           03  LNK-WKSRYAPI-DELKBN         PIC 9(01).
      *
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC92.INC".
      *    パラメタ
           COPY    "CPORCSC10S.INC".
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPI021SUB1V3.INC".
      *
      *    診療内容編集サブ領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    一括日変換パラメタ
           COPY    "CPORCSNDAYCHG.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
      *
      *    ＳＰＡ画面パラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    画面スパ領域保存
           COPY    "K02SCR-SPA"  REPLACING
                                     //SPA-// BY //HZN-SPA-//.
      *
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLAV3REQ2.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLAV3RES2.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    入院履歴編集サブパラメタ
           COPY    "CPORCSCNYUHEN.INC".
      *
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
      **** COPY    "CPAPIV2REQ.INC".
           COPY    "CPAPI21V3REQ.INC".
      **01  XML-APIREQ.
      *     03  APIREQ-CONTEXT          PIC S9(9)   BINARY.
      *     03  APIREQ-OBJECT           PIC X(8).
      *     03  APIREQ-MODE             PIC S9(9)   BINARY.
      *     03  APIREQ-RECORDNAME       PIC X(128).
      *     03  APIREQ-PATIENTINFOREQ   PIC X(1000000).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *    入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為マスタ−
      *01  SRYACT-REC.
      *    COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
      *01  SRYACCT-REC.
      *    COPY    "CPSRYACCT.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
      *
           COPY    "MCPDATA3.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
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
           DISPLAY   "*********************"
           DISPLAY   "* medicalreqv32 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      SPA-K01KYOUTU
           INITIALIZE                      CNT-AREA
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                      SPA-SCR-REC
           INITIALIZE                      SPA-K02
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
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST13-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "*********************"
           DISPLAY   "* medicalreqv32 end *"
           DISPLAY   "*********************"
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
           INITIALIZE                      XML-MEDICALAV3RES2
           INITIALIZE                      TBL-KEIMSG-AREA
           INITIALIZE                      WRK-APIV3DATA-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  MDCRES-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
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
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
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
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD-G
      *
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           MOVE    MDCREQ-REQUEST-NUMBER   TO  MDCRES-REQUEST-NUMBER
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "02"
      *        診療内容チェック
               MOVE    1                   TO  FLG-SYORI
               MOVE    "02"                TO  MDCRES-RESPONSE-NUMBER
           WHEN    "03"
      *        確認チェック
               MOVE    2                   TO  FLG-SYORI
               MOVE    "03"                TO  MDCRES-RESPONSE-NUMBER
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
               MOVE    "02"                TO  MDCRES-RESPONSE-NUMBER
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        患者情報内容編集
               PERFORM 20021-MDCRES-HEN-SEC
      *
      *        排他チェック & ロック処理
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "90"            TO  WRK-ERRCD
                   MOVE    9               TO  FLG-SYORI
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *        診療行為内容編集
               PERFORM 200101-MDCHENSYU-SEC
               MOVE    "02"                TO  MDCRES-RESPONSE-NUMBER
               IF      FLG-SYORI       =   9
      *            終了処理
                   PERFORM 300-SYORI-END-SEC
      ***          一時データ削除処理
      *            PERFORM 700-FILE-DELETE-SEC
      *            排他制御解除
      ***          PERFORM 990-LOCK-OUT-SEC
                   IF      WRK-ERRCD       NOT =   "08"
                       MOVE    "初回接続からおこなってください。"
                                           TO  WRK-ERRMSG-02
                   END-IF
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    FLG-SYORI
           WHEN    1
      *        診療内容チェック処理
               PERFORM 2002-SYORI-01-SEC
           WHEN    2
      *        確認処理
               PERFORM 2003-SYORI-02-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST13-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALAV3REQ2
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalav3req2" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST13-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalav3req2"    TO  APIREQ-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO   1000-APTXMLREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicalav3req2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST13-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalav3req2"    TO  APIREQ-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALAV3REQ2
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST13-BODY
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
               IF      FLG-KEIMSG          =   1
      *            警告メッセージあり
                   MOVE    "W"                 TO  WRK-ERRKBN
                   MOVE    ZERO                TO  WRK-ERRCD
                   MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               END-IF
          ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"             TO  WRK-ERRKBN
               END-IF
      *
               MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDCRES-API-RESULT-MSG
      *
               IF      WRK-ERRMSG-02   NOT =   SPACE
                   MOVE    SPACE           TO  MDCRES-API-RESULT-MSG
                   STRING  WRK-ERRMSG
                           WRK-ERRMSG-02   DELIMITED  BY  SPACE
                                           INTO  MDCRES-API-RESULT-MSG
                   END-STRING
               END-IF
           END-IF
      *
      *    診療内容エラー・警告編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *
           IF      APILST13-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalav3res2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalav3res2"    TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALAV3RES2
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalav3res2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalav3res2"    TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALAV3RES2
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
               MOVE  MDCRES-OBJECT     TO  APILST13-BODY 
               MOVE  "application/xml" TO  APILST13-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容エラー・警告編集処理
      *****************************************************************
       3001-WAINGMSG-HEN-SEC        SECTION.
      *
           IF      WRK-MD-ERRCD    NOT =   SPACE
               MOVE    WRK-MD-ERRCD        TO  MDCRES-MEDI-RESULT
               MOVE    WRK-MD-ERRMSG       TO  MDCRES-MEDI-RESULT-MSG
               MOVE    WRK-MD-ERR-LINE     TO  MDCRES-MEDI-POSITION
               MOVE    WRK-MD-ERR-IDX      TO  MDCRES-MEDI-ITEM-POSITION
               MOVE    WRK-MD-SRYCD        TO  MDCRES-MEDI-CODE
      *
               IF      WRK-MD-SRYCD(1:1)   =   "*"
                   MOVE    SPACE           TO  MDCRES-MEDI-CODE
               END-IF
           END-IF
           MOVE    ZERO                TO  IDM1
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   100     )   OR
                              (TBL-KEIERRCD (IDK)  =   SPACE)
      *        ワーニングコード
               MOVE    TBL-KEIERRCD (IDK)  TO  MDCRES-MEDI-WARNING (IDK)
               MOVE    TBL-KEIMSG   (IDK)  TO  MDCRES-WARNING-MSG  (IDK)
               MOVE    TBL-KEI-LINE (IDK)  TO  MDCRES-WRG-POSITION (IDK)
               MOVE    TBL-KEI-IDX  (IDK)  TO  MDCRES-WRG-RITEM-POSITION
                                                                 (IDK)
               MOVE    TBL-KEI-SRYCD(IDK)  TO  MDCRES-WRG-MEDI-CODE(IDK)
      *
               IF      TBL-KEI-SRYCD(IDK)(1:1) =   "*"
                   MOVE    SPACE           TO  MDCRES-WRG-MEDI-CODE(IDK)
               END-IF
           END-PERFORM
      *
          .
       3001-WAINGMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
      *    IF      MDCREQ-OUTPATIENT-CLASS    =   LOW-VALUE
      *        MOVE  SPACE             TO  MDCREQ-OUTPATIENT-CLASS
      *    END-IF
           IF      MDCREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PATIENTID
           END-IF
           IF     MDCREQ-PERFORM-DATE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-DATE
           END-IF
           IF     MDCREQ-PERFORM-TIME  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-TIME
           END-IF
           IF      MDCREQ-APPOINT-DEP-CODE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-APPOINT-DEP-CODE
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-CLASS  =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-CLASS
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NUMBER =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NUMBER
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NAME   =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NAME
           END-IF
           IF  MDCREQ-MAIN-MARK      =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-MARK
           END-IF
           IF  MDCREQ-MAIN-NUMBER    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-NUMBER
           END-IF
           IF  MDCREQ-MAIN-CONTINUATION     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-CONTINUATION
           END-IF
           IF  MDCREQ-MAIN-ASSISTANCE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-ASSISTANCE
           END-IF
           IF  MDCREQ-MAIN-FAMILY     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-FAMILY
           END-IF
      **   IF  MDCREQ-MAIN-POLICYHOLDER     =   LOW-VALUE
      *        MOVE  SPACE             TO  MDCREQ-MAIN-POLICYHOLDER
      **   END-IF
           IF  MDCREQ-MAIN-START-DATE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-START-DATE
           END-IF
           IF  MDCREQ-MAIN-END-DATE   =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-END-DATE
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      MDCREQ-INSURANCE-CLASS(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  MDCREQ-INSURANCE-CLASS
                                                                  (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-NAME(IDX)    =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-INSURANCE-NAME(IDX)
               END-IF
               IF      MDCREQ-PROVIDER-NUMBER(IDX)   =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-PROVIDER-NUMBER(IDX)
               END-IF
               IF      MDCREQ-RECIPIENT-NUMBER(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-RECIPIENT-NUMBER(IDX)
               END-IF
               IF      MDCREQ-INSURANCE-START-DATE(IDX) =   LOW-VALUE
                   MOVE    SPACE      TO   MDCREQ-INSURANCE-START-DATE
                                                                 (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-END-DATE(IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  MDCREQ-INSURANCE-END-DATE
                                                                (IDX)
               END-IF
           END-PERFORM 
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    SPATMPからSPAの取得
           IF     (MDCREQ-ORCA-UID NOT =   SPACE  )
             AND  (WRK-ERRCD           =   SPACE  )
               PERFORM 2008-SPATMP-SEL-SEC
           END-IF
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    電子カルテＵＩＤチェック
           IF      WRK-ERRCD           =   SPACE
               IF      MDCREQ-KARTE-UID    NOT =   WRK-KARTE-UID
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    同日再入院区分チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-DOUJITSUKBN-CHK-SEC
           END-IF
      *    訂正判定
           IF      WRK-ERRCD           =   SPACE
               IF     (MDCREQ-PATIENT-MODE =   "Modify")
                  OR  (WRK-PARA-MODE       =   "UPD" )
                   PERFORM 20011-MODIFY-CHK-SEC
               ELSE
      *            送信内容クリア
                   MOVE    SPACE           TO  MDCRES-MOD-COMBINATION
                   MOVE    SPACE           TO  MDCRES-MOD-DEP-CODE
               END-IF
           END-IF
      *
      *    上記のエラー時は初回から
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    9                   TO  FLG-SYORI
           END-IF
      *
      *    診療科コードチェック
           IF     (WRK-ERRCD           =   SPACE  )
               IF      WRK-SRYKA           =   SPACE
                   IF     (FLG-SYORI           =   1      )
                       MOVE    "02"                TO  WRK-ERRCD
                   END-IF
               ELSE
                   PERFORM 20011-SRYKA-CHK-SEC
               END-IF
           END-IF
      *    ドクターコードチェック
      *    IF     (WRK-ERRCD           =   SPACE  )
      *        IF     (WRK-DRCD            =   SPACE  )
      *            IF     (FLG-SYORI           =   1      )
      *                MOVE    "03"                TO  WRK-ERRCD
      *            END-IF
      *        ELSE
      *            PERFORM 20011-DRCD-CHK-SEC
      *        END-IF
      **** END-IF
      *
      *    時間外区分チェック
           IF     (MDCREQ-OFFTIME  NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-OFFTIME-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    患者番号
           MOVE    MDCREQ-PATIENTID        TO  MDCRES-PATIENTID
           MOVE    MDCREQ-PATIENTID        TO  WRK-PTNUM
      *    診療科
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  MDCRES-APPOINT-DEP-CODE
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  WRK-SRYKA
      *    ドクター
      *    MOVE    MDCREQ-APPOINT-DR-CODE
      *                                    TO  MDCRES-APPOINT-DR-CODE
      *    MOVE    MDCREQ-APPOINT-DR-CODE
      ***                                  TO  WRK-DRCD
      *    診療日・時間
           MOVE    MDCREQ-PERFORM-DATE     TO  MDCRES-PERFORM-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  MDCRES-PERFORM-TIME
      *    数字のみに編集
           MOVE    MDCREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    MDCRES-INFORMATION-DATE
                                           TO  MDCRES-PERFORM-DATE
      ******   ADD     1                   TO  IDK
      ******   MOVE    "W01"               TO  WRK-KEICD(IDK)
           END-IF
      *    同日再入院
           MOVE    MDCREQ-DOUJITSUKBN  TO  MDCRES-DOUJITSUKBN
      *    訂正前情報
           MOVE    MDCREQ-MOD-COMBINATION  TO  MDCRES-MOD-COMBINATION
           MOVE    MDCREQ-MOD-DEP-CODE     TO  MDCRES-MOD-DEP-CODE
      *
      *    入力UID
           MOVE    MDCREQ-ORCA-UID     TO  MDCRES-ORCA-UID
           MOVE    MDCREQ-KARTE-UID    TO  MDCRES-KARTE-UID
      *
      *    入外区分（入院）
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *    HAORI処理
           MOVE    1                   TO  SPA-API-FLG
      *
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *
      *    UID（電子カルテ）
           IF      MDCREQ-KARTE-UID    =   SPACE
               MOVE    "04"                TO  WRK-ERRCD
           END-IF
      *    SPATMPからSPAの取得
           IF      MDCREQ-ORCA-UID     =   SPACE
               MOVE    "05"                TO  WRK-ERRCD
           END-IF
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-PERFORM-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-PERFORM-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
      *    ＳＰＡと一致すること
           IF      WRK-PERFORM-YMD NOT =   SPA-SRYYMD
               MOVE    "06"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-PERFORM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC     SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  MDCRES-PATIENTID
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  WRK-PTID
      *    ＳＰＡと一致すること
           IF     (WRK-PTID            =   SPA-PTID )
              AND (WRK-PTNUM           =   SPA-PTNUM)
               CONTINUE
           ELSE
               MOVE    "07"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *    MOVE    SPTID-PTID          TO  SPA-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正チェック
      *****************************************************************
       20011-MODIFY-CHK-SEC         SECTION.
      *
           IF     (MDCREQ-PATIENT-MODE =   "Modify")
              AND (WRK-PARA-MODE       =   "UPD" )
               MOVE    "UPD"               TO  WRK-SYORI-MODE
           ELSE
               MOVE    "29"                TO  WRK-ERRCD
               GO      TO      20011-MODIFY-CHK-EXT
           END-IF
      *
      *    訂正前保険組合せ
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-MOD-COMBINATION  TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    MDCREQ-MOD-COMBINATION  TO  WRK-MOD-HKNCOMBI
               MOVE    "28"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-MOD-HKNCOMBI
               MOVE    WRK-MOD-HKNCOMBI    TO  MDCRES-MOD-COMBINATION
           END-IF
      *    訂正前診療科
           MOVE    MDCREQ-MOD-DEP-CODE     TO  WRK-MOD-SRYKA
           IF      WRK-MOD-SRYKA       =   SPACE
               MOVE    "28"                TO  WRK-ERRCD
           END-IF
      *
      *    前回と一致
           IF     (WRK-MOD-HKNCOMBI    =   WRK-PARA-HKNCOMBI)
              AND (WRK-MOD-SRYKA       =   WRK-PARA-SRYKA   )
               CONTINUE
           ELSE
               MOVE    "28"                TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                  HZN-SPA-K02
      *    TBL-SPATMP
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K02"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  HZN-SPA-K02
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-MODIFY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院区分チェック
      *****************************************************************
       20011-DOUJITSUKBN-CHK-SEC         SECTION.
      *
      *    ＳＰＡと一致すること
           IF     (MDCREQ-DOUJITSUKBN  =   "2"   )
              AND (SPA-K02N-DOUKBN     =   "1"   )
               IF     (SPA-K02N-DOUKBN     =   "1" )
                   MOVE    1               TO  FLG-DOUNYUIN
               ELSE
                   MOVE    ZERO            TO  FLG-DOUNYUIN
               END-IF
           ELSE
               MOVE    ZERO            TO  FLG-DOUNYUIN
               IF     (MDCREQ-DOUJITSUKBN  NOT =   SPA-K02N-DOUKBN  )
                  AND (SPA-K02N-DOUKBN     =   "1"   )
                   MOVE    "12"            TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20011-DOUJITSUKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクタコードチェック
      *****************************************************************
       20011-DRCD-CHK-SEC          SECTION.
      *
      *    ドクタの存在チェック
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-DRCD (1:1)  NOT =   "1"
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外区分チェック処理
      *****************************************************************
       20011-OFFTIME-CHK-SEC        SECTION.
      *
           IF      MDCREQ-OFFTIME      =   SPACE
               MOVE    "0"             TO  MDCREQ-OFFTIME
           END-IF
           IF      MDCREQ-OFFTIME      >=  "0"  AND
                                       <=  "4"
               CONTINUE
           ELSE
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
           .
       20011-OFFTIME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リクエスト内容をレスポンス編集処理
      *****************************************************************
       200101-MDCHENSYU-SEC        SECTION.
      *
      *    時間外加算
           MOVE    MDCREQ-OFFTIME      TO  MDCRES-REQ-OFFTIME
      *
           PERFORM VARYING     IDX     FROM    1   BY    1
                   UNTIL      (IDX     >   COB-ZAI-INMAX )
                       OR     (MDCREQ-MDC-CLASS (IDX) =   SPACE )
               MOVE    MDCREQ-MDC-CLASS (IDX)
                                       TO  MDCRES-REQ-CLASS     (IDX)
               MOVE    MDCREQ-MDC-CLASS-NAME (IDX)
                                       TO  MDCRES-REQ-CLASS-NAME
                                                                (IDX)
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDX)
                                       TO  MDCRES-REQ-CLASS-NUMBER
                                                                (IDX)
      *入院用
      *        算定日
               MOVE    MDCREQ-MDC-DAY-G(IDX)
                                       TO  MDCRES-REQ-DAY-G   (IDX)
      *        診療内容
               PERFORM VARYING     IDY     FROM    1   BY    1
                       UNTIL      (IDY     >   50   )
                           OR     (MDCREQ-PRSCRPT-CODE (IDX IDY)
                                           =   SPACE )
                   MOVE    MDCREQ-PRSCRPT-CODE (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-CODE
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-NAME
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-NUMBER
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-MONEY (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-MONEY
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-ATAI-G
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-CONTKBN
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-NAIFKBN
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-AUTOKBN (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-AUTOKBN
                                                           (IDX IDY)
      *            フィルム分画数
                   MOVE    MDCREQ-PRSCRPT-FILMNUM (IDX IDY)
                                       TO  MDCRES-REQ-PRSCRPT-FILMNUM
                                                           (IDX IDY)
      *            自動算定なし区分（在宅療養実績加算）
                   MOVE    MDCREQ-PRSCRPT-NOAUTO (IDX IDY)
                                       TO MDCRES-REQ-PRSCRPT-NOAUTO
                                                           (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           .
       200101-MDCHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容チェック処理
      *****************************************************************
       2002-SYORI-01-SEC        SECTION.
      *
      *    診療行為内容編集
           PERFORM 200101-MDCHENSYU-SEC
      *
      *    診察料登録処理
           PERFORM 20021-HENSYU-01-SEC
      *
      *    返却レスポンス番号
           IF     (WRK-ERRCD           =   SPACE )
      *        エラーなしは次へ
               MOVE    "03"            TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
           IF      WRK-ERRCD           =   "80"
      *        ＤＢエラー
               GO      TO      2002-SYORI-01-EXT
           END-IF
      *    SPATMPの削除処理
           PERFORM 2009-SPATMP-DEL-SEC
      *    SPAの登録
           PERFORM 2008-SPATMP-INS-SEC
      *
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-MDCRES-HEN-SEC     SECTION.
      *
           MOVE    PTINF-NAME          TO  MDCRES-NAME
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDCRES-SEX
      *
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-APPOINT-DEP-NAME
      **** MOVE    WRK-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
      *
      *    保険情報
           PERFORM 200211-HKNINF-HEN-SEC
      *
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険入力情報編集処理
      *****************************************************************
       200211-HKNINF-HEN-SEC        SECTION.
      *
      *    保険組合せ番号
           MOVE    MDCREQ-COMBINATION-NUMBER
                                       TO  MDCRES-COMBINATION-NUMBER
      *    保険種類
           MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                       TO  MDCRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                       TO  MDCRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    MDCREQ-MAIN-INSURANCE-NAME
                                       TO  MDCRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    MDCREQ-MAIN-MARK    TO  MDCRES-MAIN-MARK
      *    番号
           MOVE    MDCREQ-MAIN-NUMBER  TO  MDCRES-MAIN-NUMBER
      *    継続区分
           MOVE    MDCREQ-MAIN-CONTINUATION
                                       TO  MDCRES-MAIN-CONTINUATION
      *    補助区分
           MOVE    MDCREQ-MAIN-ASSISTANCE
                                       TO  MDCRES-MAIN-ASSISTANCE
           MOVE    MDCREQ-MAIN-FAMILY  TO  MDCRES-MAIN-FAMILY
      *    MOVE    MDCREQ-MAIN-POLICYHOLDER
      *                                TO  MDCRES-MAIN-POLICYHOLDER
      *    MOVE    MDCREQ-MAIN-START-DATE
      *                                TO  MDCRES-MAIN-START-DATE
      *    MOVE    MDCREQ-MAIN-END-DATE
      *                                TO  MDCRES-MAIN-END-DATE
      *    公費情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        公費種類
               MOVE    MDCREQ-INSURANCE-CLASS (IDX)
                                       TO  MDCRES-INSURANCE-CLASS(IDX)
      *        公費名称
               MOVE    MDCREQ-INSURANCE-NAME  (IDX)
                                       TO  MDCRES-INSURANCE-NAME (IDX)
      *        負担者番号
               MOVE    MDCREQ-PROVIDER-NUMBER(IDX)
                                       TO  MDCRES-PROVIDER-NUMBER(IDX)
      *        受給者番号
               MOVE    MDCREQ-RECIPIENT-NUMBER (IDX)
                                       TO  MDCRES-RECIPIENT-NUMBER(IDX)
      *        有効開始日
      *        MOVE    MDCREQ-INSURANCE-START-DATE (IDX)
      *                                TO  MDCRES-INSURANCE-START-DATE
      *                                                         (IDX)
      *        有効終了日
      *        MOVE    MDCREQ-INSURANCE-END-DATE (IDX)
      *                                TO  MDCRES-INSURANCE-END-DATE
      *                                                         (IDX)
           END-PERFORM
           .
       200211-HKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-HENSYU-01-SEC        SECTION.
      *
      *    保険組合せの決定
           PERFORM 200215-HKNCOMBI-SET-SEC
      *    診察料なし
           IF     (WRK-HKNCOMBI        =   ZERO   )
            AND   (WRK-ERRCD           =   SPACE  )
               MOVE    "17"            TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20021-HENSYU-01-EXT
           END-IF
      *
           INITIALIZE                  SPA-K02
      *    ＳＰＡ初期設定
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *    SPA患者情報
           MOVE    WRK-PTID            TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  SPA-PTNUM
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
           MOVE    WRK-HKNCOMBI        TO  SPA-HKNCOMBINUM
      *
           MOVE    WRK-SRYKA-NAME      TO  SPA-SRYKAMEI
      **** MOVE    WRK-DRCD-NAME       TO  SPA-DRCD-NAME
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "@"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    "K10"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *    保険組合せ負担割合編集
           IF      SPA-FTNRATE     NOT =   ZERO
               COMPUTE WRK-FTNRATE         =   SPA-FTNRATE
                                           /   100
               MOVE    WRK-FTNRATE         TO  WRK-Z9
               MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
               MOVE    WRK-FTNRATEMEI      TO  MDCRES-COMBINATION-RATE
           ELSE
               MOVE    "0"                 TO  MDCRES-COMBINATION-RATE
           END-IF
      *
      *    入院判定
           PERFORM 200211-NYUGAIKBN-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  20021-HENSYU-01-EXT
           END-IF
      *
      *    訂正判定
           IF     (WRK-ERRCD          =   SPACE )
            AND   (WRK-PARA-MODE      =   "UPD" )
               PERFORM 200213-UPDATESYORI-SEC
               IF      WRK-ERRCD       NOT=   SPACE
                   GO  TO  20021-HENSYU-01-EXT
               END-IF
           END-IF
      *
      *    保険組合せ判定
           IF     (SPA-HKN-ERR     NOT =   ZERO )  OR
                  (SPA-RJN-ERR     NOT =   ZERO )
               PERFORM 20021-HKN-CHEK-SEC
           END-IF
      *    アフターケアはエラー
           IF     (WRK-ERRCD           =   SPACE)
             AND ((WRK-HKNNUM          =   "971")  AND
                  (WRK-RSI-HKNKBN      =   "3"  ))
               MOVE    "9029"              TO  SPA-ERRCD
               PERFORM 200232-ERR-HEN-SEC
               MOVE    "22"                TO  WRK-ERRCD
           END-IF
      *
           IF     (LNK-SC95-LAST-RENNUM    =   9     )
             AND  (WRK-ERRCD               =   SPACE )
             AND  (SPA-SYORIFLG            =   ZERO  )
               MOVE    "0220"              TO  SPA-ERRCD
               PERFORM 200211-ERRCD-HEN-SEC
               MOVE    "23"                TO  WRK-ERRCD
           END-IF
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
      **** MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
      *    診療日の末日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYLST-DD
           ELSE
               MOVE    31                  TO  SPA-SRYLST-DD
           END-IF
      *
      *    入院料包括の判定（Ｈ２０．４から）
           INITIALIZE                  SPA-NYUHKT-AREA
           IF     (SPA-HKTNYUIN-FLG    =   1     )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")  AND
                  (SPA-CHIKENKBN   NOT =   "1"   )  AND
                  (SPA-SRYYMD          >=  "20080401" )
               PERFORM 200233-HKTCHK-INIT-SEC
               IF      SPA-NYU-ERR         =   1
                   MOVE    "0217"              TO  SPA-ERRCD
                   PERFORM 200232-ERR-HEN-SEC
                   MOVE    "25"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD       NOT=   SPACE
               GO  TO  20021-HENSYU-01-EXT
           END-IF
      *
           INITIALIZE                      SPA-NAIACCT2-AREA
           INITIALIZE                      SPA-RIGAKU-NCNT-AREA
      *    当月診療行為チェック用編集
      **** PERFORM 3201-TOUCHKSET-SEC
      *    相互作用チェックなし
           MOVE    ZERO                TO  SPA-INTERACT-KIKAN
      *
      *    時間外加算
           MOVE    MDCREQ-OFFTIME      TO  SPA-K02N-JIKANKBN
           IF      SPA-K02N-JIKANKBN   =   SPACE
               MOVE    "0"             TO  SPA-K02N-JIKANKBN
           END-IF
      *
      *    院内とする
           MOVE    "0"                 TO  SPA-SYOHOKBN
      *
      *    ワーク診療行為データレイアウト作成
           PERFORM 20021-WKSRYACT-SYORI-SEC
      *    画面展開・計算
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-INPUT-SYORI-SEC
           END-IF
      *
           .
       20021-HENSYU-01-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入外区分判定処理
      *****************************************************************
       200211-NYUGAIKBN-CHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSCNYUHENAREA
           MOVE    "1"                 TO  ORCSCNYUHEN-KBN
           MOVE    SPA-SRYYMD          TO  ORCSCNYUHEN-SRYYMD
      *    同日再入院分
           IF      FLG-DOUNYUIN        =   1
               MOVE   "1"                  TO  ORCSCNYUHEN-KBN2
           END-IF
           CALL    "ORCSCNYUHEN"       USING
                                       SPA-AREA
                                       ORCSCNYUHENAREA
      *    入院履歴なし
           IF      ORCSCNYUHEN-RRK-NYUIN   =  ZERO
               MOVE    "24"                TO  WRK-ERRCD
           ELSE
      *        入院中以外はエラー
               IF      ORCSCNYUHEN-NYUIN-KBN   =   ZERO
                   MOVE    "24"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        入院日
               MOVE    ORCSCNYUHEN-NYUINYMD
                                           TO  SPA-K02N-NYUINYMD
      *        退院日
               MOVE    ORCSCNYUHEN-TAIINYMD
                                           TO  SPA-K02N-TAIINYMD
      *        入院期間
               MOVE    ORCSCNYUHEN-NYUIN-DAYG
                                           TO  SPA-K02-NYUIN-AREA
      *        継続開始日入院日
               MOVE    ORCSCNYUHEN-STR-NYUINYMD
                                           TO  SPA-STR-NYUINYMD
      *        同日再入院有無
               MOVE    ORCSCNYUHEN-DOUNYUIN   TO  SPA-K02N-DOUDAY
      *        診療日付＝同日再入院日
               IF     (SPA-SRYYMD             =   SPA-K02N-NYUINYMD)
                 AND  (ORCSCNYUHEN-DOUNYUIN   =   SPA-SRYYMD(7:2)  )
                   MOVE    "1"                TO  SPA-K02N-DOUKBN
               ELSE
                   MOVE    SPACE              TO  SPA-K02N-DOUKBN
               END-IF
      *        同日再入院日へ変更不可
      *****    MOVE    ORCSCNYUHEN-DOUNYUKBN  TO  SPA-K02N-DOUNYUKBN
               MOVE    ZERO                   TO  SPA-K02N-DOUNYUKBN
               MOVE    ORCSCNYUHEN-DOUTAIKBN  TO  SPA-K02N-DOUTAIKBN
      *        担当医１（主治医）
               MOVE    ORCSCNYUHEN-DRCD       TO  SPA-DRCD
               MOVE    SPA-DRCD               TO  WRK-DRCD
               PERFORM 20011-DRCD-CHK-SEC
               IF      WRK-ERRCD              =   SPACE
                   MOVE    WRK-DRCD-NAME      TO  SPA-DRCD-NAME
               ELSE
                   MOVE    SPACE              TO  SPA-DRCD-NAME
                   MOVE    SPACE              TO  WRK-ERRCD
               END-IF
      ****     診療科
      *        MOVE    ORCSCNYUHEN-NYUINKA TO  SPA-SRYKA
      *        保険組合せ番号
               IF      WRK-HKNCOMBI        =   ZERO
                   MOVE    ORCSCNYUHEN-HKNCOMBI    TO  SPA-HKNCOMBINUM
                   MOVE    ORCSCNYUHEN-HKNCOMBI    TO  WRK-HKNCOMBI
               END-IF
      *        入院情報返却
               PERFORM 200211-NYUIN-HENSYU-SEC
           END-IF
           .
       200211-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院情報返却処理
      *****************************************************************
       200211-NYUIN-HENSYU-SEC        SECTION.
      *
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-NYUIN-DATE
      *
           MOVE    SPA-K02N-TAIINYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-TAIIN-DATE
      *    同日再入院
           IF      SPA-K02N-DOUKBN     =   "1"
               MOVE    "2"             TO  MDCRES-DOUJITSUKBN
           ELSE
               MOVE    SPACE           TO  MDCRES-DOUJITSUKBN
           END-IF
      *
           .
       200211-NYUIN-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    入院料包括の判定チェック処理
      *****************************************************************
       200233-HKTCHK-INIT-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    入院料包括の期間チェック
           INITIALIZE                  ORCSCHKTCHKAREA
           INITIALIZE                  ORCSC00AREA
           MOVE    "5"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           MOVE    SPA-K02-TEISEI-AREA TO   LNK-ORCSC-TEISEI-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    ORCSCHKTCHK-NYUHKTCHK   TO  SPA-NYUHKTCHK
           MOVE    ORCSCHKTCHK-NYUHKTCD    TO  SPA-NYUHKTCD
           MOVE    ORCSCHKTCHK-NYUSRYCD    TO  SPA-NYUHKTSRYCD
           MOVE    ORCSCHKTCHK-NYUHKT-G    TO  SPA-NYUHKT-G
      *    入院料算定なし
           MOVE    ORCSCHKTCHK-NYUERR  TO  SPA-NYU-ERR
      *    翌月入院料算定なし
           MOVE    ORCSCHKTCHK-NYUERR-2    TO  SPA-NYU-ERR-2
           .
       200233-HKTCHK-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正編集処理
      *****************************************************************
       200213-UPDATESYORI-SEC     SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    WRK-MOD-SRYKA       TO  JYURRK-SRYKA
           MOVE    WRK-MOD-HKNCOMBI    TO  JYURRK-HKNCOMBINUM
      *    同日再入院
           IF      SPA-K02N-DOUKBN     =   "1"
               MOVE    2               TO  JYURRK-RENNUM
           ELSE
               MOVE    1               TO  JYURRK-RENNUM
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key19"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key19"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key19"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   1
               MOVE    "27"                TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200213-UPDATESYORI-EXT
           END-IF
      *    訂正編集
           MOVE    JYURRK-SRYYMD       TO  SPA-SRYYMD
           MOVE    JYURRK-RENNUM       TO  SPA-RENNUM
           MOVE    JYURRK-DOUJI-RENNUM TO  SPA-DOUJI-RENNUM
           MOVE    JYURRK-SRYKA        TO  SPA-OLD-SRYKA
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-OLD-HKNCOMBI
           MOVE    JYURRK-DENPNUM      TO  SPA-DENPNUM
      *    訂正処理
           MOVE    1                   TO  SPA-SYORIFLG
      *    訂正の情報
           MOVE    HZN-SPA-K02-TEISEI-AREA
                                       TO  SPA-K02-TEISEI-AREA
           MOVE    HZN-SPA-YAKUZAI-AREA
                                       TO  SPA-YAKUZAI-AREA
      *
      *    今回診療科、保険組合せ番号チェック
           IF     (SPA-OLD-SRYKA       =   SPA-SRYKA )
              AND (SPA-OLD-HKNCOMBI    =   SPA-HKNCOMBINUM)
               CONTINUE
           ELSE
               PERFORM 200213-JYURRK-CHK-SEC
           END-IF
           .
       200213-UPDATESYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回受診履歴チェック処理
      *****************************************************************
       200213-JYURRK-CHK-SEC     SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
      *    同日再入院
           IF      SPA-K02N-DOUKBN     =   "1"
               MOVE    2               TO  JYURRK-RENNUM
           ELSE
               MOVE    1               TO  JYURRK-RENNUM
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
      *        診療科・保険組合せ変更後の受診履歴あり
               ADD     1                   TO  IDX-KEI
               IF      IDX-KEI             <=  400
                   MOVE    "KA01"          TO  TBL-KEIERRCD (IDX-KEI)
                   MOVE    SPACE           TO  WRK-KIDMSG
                   STRING  "診療科・保険組合せを変更します。"
                           "同日の同じ受診履歴に追加されます。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
                   MOVE    WRK-KIDMSG      TO  TBL-KEIMSG   (IDX-KEI)
               END-IF
          END-IF
           .
       200213-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200211-ERRCD-HEN-SEC     SECTION.
      *
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    "K02"               TO  LNK-SC96-PG
               CALL    "ORCSC96"           USING    SPA-AREA
                                                    ORCSC96AREA
               MOVE    SPA-ERRMSG          TO  SPA-ERRMSG2
           END-IF
      *    全角変換
           MOVE    SPA-ERRMSG2         TO  WRK-MEISYO
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MEISYO          TO  WRK-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  SPA-ERRMSG
           .
       200211-ERRCD-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ワーク診療行為データ作成処理
      *****************************************************************
       20021-WKSRYACT-SYORI-SEC        SECTION.
      *
           INITIALIZE                  ORAPI021SUB1V3AREA
           MOVE    WRK-HKNCOMBI        TO  ORAPISUB01-HKNCOMBI
      *    時間外加算
           MOVE    MDCREQ-OFFTIME      TO  ORAPISUB01-TIMEFLG
      *
           PERFORM VARYING     IDX     FROM    1   BY    1
                   UNTIL       IDX     >   COB-ZAI-INMAX
      ***      MOVE    MDCREQ-MDC-CLASS-KBN (IDX)
      *                                TO  ORAPISUB01-MDC-CLASS-KBN
      ***                                                       (IDX)
               MOVE    MDCREQ-MDC-CLASS (IDX)
                                       TO  ORAPISUB01-MDC-CLASS
                                                                (IDX)
               MOVE    MDCREQ-MDC-CLASS-NAME (IDX)
                                       TO  ORAPISUB01-MDC-CLASS-NAME
                                                                (IDX)
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDX)
                                       TO  ORAPISUB01-MDC-CLASS-NUMBER
                                                                (IDX)
      *入院用
      *        算定日
               MOVE    MDCREQ-MDC-DAY-G(IDX)
                                       TO  ORAPISUB01-MDC-DAY-G
                                                               (IDX)
      *        診療内容
               PERFORM VARYING     IDY     FROM    1   BY    1
                       UNTIL       IDY     >   50
                   MOVE    MDCREQ-PRSCRPT-CODE (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-CODE
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAME (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-NAME
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-NUMBER
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-MONEY (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-MONEY
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  ORAPISUB01-ATAI-G
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  ORAPISUB01-CONTKBN
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  ORAPISUB01-NAIFKBN
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-AUTOKBN (IDX IDY)
                                       TO  ORAPISUB01-AUTOKBN
                                                           (IDX IDY)
      *H29.5
      *            フィルム分画数
                   MOVE    MDCREQ-PRSCRPT-FILMNUM (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-FILMNUM
                                                           (IDX IDY)
      *            自動算定なし区分（在宅療養実績加算）
                   MOVE    MDCREQ-PRSCRPT-NOAUTO (IDX IDY)
                                       TO  ORAPISUB01-NOAUTO
                                                           (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    SPACE               TO  ORAPISUB01-SYORIKBN
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORAPI021SUB1V3"   USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB1V3AREA
      *
      *    エラー処理
           IF      ORAPISUB01-ERRCD    NOT =   SPACE
               MOVE    ORAPISUB01-ERRCD    TO  WRK-ERRCD
               MOVE    ORAPISUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
           PERFORM VARYING     IDX    FROM    1   BY  1
                   UNTIL       (IDX        >   100 )
                           OR  (ORAPISUB01-IN-ERR (IDX)
                                           =   SPACE )
               MOVE    ORAPISUB01-IN-ERR (IDX)
                                       TO  MDCRES-PRSC-RESULT (IDX)
               MOVE    ORAPISUB01-IN-ERRMSG (IDX)
                                       TO  MDCRES-PRSC-RESULT-MSG (IDX)
               MOVE    ORAPISUB01-IN-POS (IDX)
                                       TO  MDCRES-PRSC-POSITION (IDX)
               MOVE    ORAPISUB01-IN-LINE (IDX)
                                       TO  MDCRES-PRSC-ITEM-POSITION
                                                                (IDX)
               IF      ORAPISUB01-IN-ERR (IDX)(1:1)
                                           =   "W"
      *            警告メッセージ
                   MOVE    1                   TO  FLG-KEIMSG
               ELSE
                   IF      WRK-ERRCD       =   SPACE
                       MOVE    "21"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           .
       20021-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    中途データ展開・計算編集処理
      *****************************************************************
       20022-INPUT-SYORI-SEC           SECTION.
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *    診療行為処理
           INITIALIZE                  SPA-SCR-REC
      *
           MOVE    WRK-HKNCOMBI        TO  SPA-HKNCOMBINUM
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
           MOVE    WRK-PTID            TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  SPA-PTNUM
      *
      *    中途データ展開処理
           PERFORM 200221-SAIHEN-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-GENECHG-KBN
      *    一般名記載とする
           MOVE    "1"                 TO  SPA-GENERICFLG
      *    時間外区分
           MOVE    SPA-K02N-JIKANKBN   TO  SPA-NAI-TIMEFLG
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                    OR     (SPA-ERRCD          NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "."
                   WHEN    "#"
                   WHEN    "$"
                       CONTINUE
                   WHEN    "*"
      *                算定日指定チェック
                       PERFORM 200224-KAISUU-CHK-SEC
                   WHEN    OTHER
      *                点数マスタチェック（再表示編集時にエラー）
                       IF      SPA-COM-CUR (IDX)   NOT =   SPACE
                           PERFORM 200222-KIHONCHK-SEC
                       END-IF
      *!!
      *                後発品変更不可
                       IF      SPA-COM-INPUTCD (IDX)   =   "099209910"
                           MOVE    "1"             TO  WRK-GENECHG-KBN
                       END-IF
      *                後発品変更可
                       IF      SPA-COM-INPUTCD (IDX)   =   "099209911"
                           MOVE    "2"             TO  WRK-GENECHG-KBN
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    診療行為、計計算処理、編集
           IF      SPA-ERRCD           =   SPACE
               PERFORM 510-TBLHEN-SYORI-SEC
           END-IF
      *
      *    包括の判定
           IF     (WRK-ERRCD           =   SPACE )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
      *            治験以外
             AND  (SPA-CHIKENKBN   NOT =   "1"   )
               IF     (SPA-HKTSANTEI-FLG   =   1     )  OR
                      (SPA-HKTSANTEI-08    =   1     )  OR
                      (SPA-NYUHKTCHK       =   1     )
                   PERFORM 5109-HKTSANTEI-CHK-SEC
      *R01.9
                   IF      SPA-ERRCD(1:1)      =   "K"
                       PERFORM 3001-KEIMSGSYORI-SEC
                   END-IF
      *
      *            投薬調剤料の翌月チェック
                   IF     (SPA-KIDCD           =   SPACE )
                      AND (SPA-K02N-TAIINYMD   =   "99999999")
                      AND (SPA-NYU-ERR-2       =   1     )
                       PERFORM 51092-NEXTCHOZAI-CHK-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
      *    エラーメッセージ編集処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
           END-IF
      *
      *    一般名記載区分編集
      ***  PERFORM 520-GENE-HEN-SYORI-SEC
      *
      *    明細編集
           PERFORM 200222-MEDICAL-HEN-SEC
      *
           IF      WRK-ERRCD           =   SPACE
      *        パラメタ登録
               PERFORM 200231-WKSRYPARA-SEC
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "03"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
           END-IF
      *
      *    一時データ削除処理
      *****PERFORM 700-FILE-DELETE-SEC
           .
       20022-INPUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面ＳＰＡ編集処理
      *****************************************************************
       200221-SAIHEN-HEN-SEC           SECTION.
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
     ******MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *R02.4
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
      *    診療算定回数編集
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
      *    処理区分（１：中途終了展開）
           MOVE    1                   TO  ORCSCWKSRY-SYORIFLG
      *    追加開始位置
           MOVE    ZERO                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  SPA-ERRCD
           END-IF
      *
           .
       200221-SAIHEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ判定処理
      *****************************************************************
       20021-HKN-CHEK-SEC        SECTION.
      *
           EVALUATE    SPA-RJN-ERR
      *        老人保険編集エラーがある時、エラーとする
               WHEN    1
                   MOVE    "1027"              TO  SPA-ERRCD
      *        老人保険編集エラーがある時、エラーとする
               WHEN    2
                   MOVE    "1070"              TO  SPA-ERRCD
      *        後期高齢者の保険がない時
               WHEN    3
                   MOVE    "1076"              TO  SPA-ERRCD
      *        退職国保で６５歳以上
               WHEN    9
                   MOVE    "1077"              TO  SPA-ERRCD
           END-EVALUATE
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKN-ERR
      *            保険エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1028"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "1032"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200211-ERRCD-HEN-SEC
               MOVE    "23"            TO  WRK-ERRCD
           END-IF
           .
       20021-HKN-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *   点数マスタチェック処理
      *****************************************************************
       200222-KIHONCHK-SEC            SECTION.
      *
      *    点数マスタ編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    IDX                 TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD(IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                   TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF     (SPA-ERRMSG2         =   SPACE )
              AND (LNK-SC92-ERRMSG NOT =   SPACE )
               MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
           END-IF
      *
      *    警告は登録対象
           IF      SPA-ERRCD(1:1)      =   "K"
               MOVE    "1"                 TO  SPA-COM-KZMFLG (IDX)
               MOVE    "2"                 TO  SPA-COM-CUR  (IDX)
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 3001-KEIMSGSYORI-SEC
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           NOT =   SPACE )
               MOVE    "1"             TO  SPA-COM-CUR (IDX)
           END-IF
           .
       200222-KIHONCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日指定チェック処理
      *****************************************************************
       200224-KAISUU-CHK-SEC            SECTION.
      *
      *    入院の回数の変換処理
           INITIALIZE                      ORCSNDAYCHGAREA
           MOVE    SPA-SRYYMD          TO  ORCSNDAYCHG-SRYYMD
           MOVE    SPA-GMN-INPUTCD (IDX)
                                       TO  ORCSNDAYCHG-INPUTCD
           MOVE    CONS-INPUT          TO  ORCSNDAYCHG-KETA
           CALL    "ORCSNDAYCHG"       USING   SPA-AREA
                                               ORCSNDAYCHGAREA
           MOVE    ORCSNDAYCHG-ERRCD   TO  SPA-ERRCD
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  SPA-COM-CUR (IDX)
               GO      TO      200224-KAISUU-CHK-EXT
           END-IF
      *
      *    入院回数・日数
           INITIALIZE                  SPA-COM-NYUIN-AREA (IDX)
      *    日付チェック
           IF      SPA-K02N-NYUINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-NYUINYMD(7:2)  TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-K02N-TAIINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-TAIINYMD(7:2)  TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           MOVE    SPA-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
              IF      ORCSNDAYCHG-NYU-DAY (IDD)    NOT =   ZERO
                   IF     (IDD             >=  WRK-STDD )  AND
                          (IDD             <=  WRK-EDDD )
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                       (IDX IDD)
                       ADD     1               TO  SPA-COM-NYU-KAI
                                                       (IDX)
                   ELSE
      *                返却用
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                       (IDX IDD)
                       MOVE    "9007"          TO  SPA-ERRCD
                   END-IF
      *            訂正の時、訂正日のみ
                   IF     (SPA-SYORIFLG        =   1   )  AND
                          (IDD             NOT =   WRK-SRYDD)
                       MOVE    "9008"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    保険組合せ期間との日付チェック
           IF      SPA-COB-TEKSTYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKSTYMD(7:2)   TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-COB-TEKEDYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKEDYMD(7:2)   TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           MOVE    SPA-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL      (IDD     >   31  )
                          OR  (SPA-ERRCD   NOT =   SPACE )
              IF      ORCSNDAYCHG-NYU-DAY (IDD)    NOT =   ZERO
                   IF     (IDD             >=  WRK-STDD )  AND
                          (IDD             <=  WRK-EDDD )
                       CONTINUE
                   ELSE
                       MOVE    "9038"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-ERRMSG2
                       STRING  WRK-STDD
                               "日〜"
                               WRK-EDDD
                              "日が当月の有効期間です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG2
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  SPA-COM-CUR (IDX)
               MOVE    IDX             TO  SPA-COM-GYOU(IDX)
           END-IF
      *
           .
       200224-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SYORI-SEC            SECTION.
      *
           MOVE     SPA-SYOHOKBN     TO  SPA-INGAIKBN
      *
      *    腫瘍マーカー一覧展開しないとする
           MOVE    ZERO                TO  SPA-AKUSEIHYOFLG
      *H30.9
      *    選択式コメント一覧展開しないとする
           MOVE    ZERO                TO  SPA-RECEKISAIFLG
      *    診療行為、計計算処理、編集
           MOVE    ZERO                TO  FLG-END-SYORI
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-KENSADEL
      *    入力チェック
           MOVE    SPACE               TO  WRK-TOROKU
           PERFORM
               UNTIL   (FLG-END-SYORI  =   1  )
               OR      (SPA-ERRCD      NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-KIDCD
               MOVE    SPACE               TO  SPA-ERRMSG2
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "1"                 TO  ORCSC100-KBN
      *******  MOVE    SPACE               TO  ORCSC100-TOROKU
               MOVE    WRK-TOROKU          TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    ZERO                TO  FLG-KENSADEL
      *        確認メッセージはすべてエラーとする
               IF      SPA-KIDCD        NOT =   SPACE
                   PERFORM 5101-KIDCD-CHK-SEC
                   MOVE    1                   TO  FLG-END-SYORI
               ELSE
      *            警告は登録対象
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    2                   TO  FLG-ERR
                       PERFORM 3001-KEIMSGSYORI-SEC
                       MOVE    ZERO            TO  FLG-END-SYORI
                   ELSE
                       IF      WRK-TOROKU      =   SPACE
      *                    登録前チェックへ
                           MOVE    "1"             TO  WRK-TOROKU
                       ELSE
                           MOVE    1               TO  FLG-END-SYORI
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       510-TBLHEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ（入力）処理
      *****************************************************************
       5101-KIDCD-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    SPA-KIDCD
      *        同一検査削除へ
               WHEN    "0160"
                   MOVE    "0064"              TO  SPA-ERRCD
      *
               WHEN    OTHER
                   MOVE    SPA-KIDCD       TO  WRK-MD-ERRCD
                   IF      WRK-ERRCD           =   SPACE
                       MOVE    "30"                TO  WRK-ERRCD
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       5101-KIDCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    警告メッセージ処理
      *****************************************************************
       3001-KEIMSGSYORI-SEC          SECTION.
      *
           MOVE    "K02"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
      *    全角変換
           MOVE    SPA-ERRMSG          TO  WRK-MEISYO
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MEISYO          TO  WRK-ERRMSG1
      *
           MOVE    SPACE               TO  WRK-KEIMSG
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    SPACE               TO  WRK-ERRMSG2
               MOVE    WRK-ERRMSG1         TO  WRK-KEIMSG1
           ELSE
               MOVE    SPA-ERRMSG2         TO  WRK-MEISYO
               PERFORM 800-ZENKAKUHEN-SEC
               MOVE    WRK-MEISYO          TO  WRK-ERRMSG2
      *
               MOVE    WRK-ERRMSG2         TO  WRK-KEIMSG1
               MOVE    WRK-ERRMSG1         TO  WRK-KEIMSG2
           END-IF
      *
      *
      **   STRING  WRK-ERRMSG1         DELIMITED  BY  SPACE
      *            WRK-ERRMSG2         DELIMITED  BY  SPACE
      *                INTO                    WRK-ERRMSG
      **   END-STRING
      *
      *    警告行判定
           MOVE    SPACE               TO  WRK-ERR-SRYCD
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   OR     ( WRK-ERR-SRYCD      NOT =   SPACE  )
               IF     (SPA-COM-CUR    (IDX)    =   "2"
                                               OR  "1" )
                   PERFORM 30011-KEIMSGHEN-SEC
                   MOVE    SPA-COM-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   IF      SPA-COM-INPUTCD (IDX)   =   SPACE
                       MOVE    SPA-GMN-INPUTCD(IDX)(1:9)
                                                   TO  WRK-ERR-SRYCD
                   END-IF
                   MOVE    SPACE                   TO  SPA-COM-CUR(IDX)
               END-IF
           END-PERFORM
      *
           IF      WRK-ERR-SRYCD       =   SPACE
               ADD     1                   TO  IDX-KEI
               IF      IDX-KEI             <=  400
                   MOVE    SPA-ERRCD       TO  TBL-KEIERRCD (IDX-KEI)
                   MOVE    WRK-KEIMSG      TO  TBL-KEIMSG   (IDX-KEI)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  WRK-ERR-SRYCD
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           MOVE    1                   TO  FLG-KEIMSG
           .
       3001-KEIMSGSYORI-EXIT.
           EXIT.
      *
      *****************************************************************
      *    警告メッセージ編集処理
      *****************************************************************
       30011-KEIMSGHEN-SEC     SECTION.
      *
           ADD     1                   TO  IDX-KEI
           IF      IDX-KEI             <=  400
               MOVE    SPA-ERRCD           TO  TBL-KEIERRCD (IDX-KEI)
               MOVE    WRK-KEIMSG          TO  TBL-KEIMSG   (IDX-KEI)
               MOVE    SPA-COM-INPUTCD (IDX)
                                           TO  TBL-KEI-SRYCD(IDX-KEI)
               IF      SPA-COM-INPUTCD (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD (IDX)(1:9)
                                           TO  TBL-KEI-SRYCD(IDX-KEI)
               END-IF
      *        警告位置
               MOVE    IDX-KEI             TO  SPA-COM-GYOU (IDX)
           END-IF
           .
       30011-KEIMSGHEN-EXIT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200232-ERR-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    "K02"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
      *
      *    全角変換
           MOVE    SPA-ERRMSG          TO  WRK-MEISYO
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MEISYO          TO  WRK-ERRMSG1
      *
           MOVE    SPACE               TO  WRK-MD-ERRMSG
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    WRK-ERRMSG1         TO  WRK-MD-ERRMSG1
           ELSE
      *        全角変換
               MOVE    SPA-ERRMSG2         TO  WRK-MEISYO
               PERFORM 800-ZENKAKUHEN-SEC
               MOVE    WRK-MEISYO          TO  WRK-ERRMSG2
      *
               MOVE    WRK-ERRMSG2         TO  WRK-MD-ERRMSG1
               MOVE    WRK-ERRMSG1         TO  WRK-MD-ERRMSG2
           END-IF
           MOVE    SPA-ERRCD           TO  WRK-MD-ERRCD
      *
      *    行判定
           MOVE    SPACE           TO  WRK-ERR-SRYCD
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   OR     ( WRK-ERR-SRYCD      NOT =   SPACE  )
               IF     (SPA-COM-CUR    (IDX)    =   "1" )
                  AND (SPA-COM-KZMFLG (IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG2(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG3(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG4(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG5(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG6(IDX)    =   SPACE )
                  AND (SPA-COM-KINKIFLG(IDX)   =   SPACE )
                  AND (SPA-COM-TUKIJGNFLG(IDX) =   SPACE )
      *
                   MOVE    SPA-COM-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   IF      SPA-COM-INPUTCD (IDX)   =   SPACE
                       MOVE    SPA-GMN-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-ERR-SRYCD       TO  WRK-MD-SRYCD
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    "30"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-ERRMSG
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  SPA-ERRMSG
           .
       200232-ERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200222-MEDICAL-HEN-SEC          SECTION.
      *
      *
           INITIALIZE                  MDCRES-MEDICAL-INF-G
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  FLG-HENCHK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   SPA-MAX-LINE )
                       OR ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                       OR  (FLG-HENCHK NOT =   ZERO    )
      *
               EVALUATE    SPA-GMN-INPUTCD(IDX)(1:1)
               WHEN    "."
                   MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                   MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
                   MOVE    ZERO                    TO  WRK-KINGAK
                   MOVE    ZERO                    TO  WRK-HOUKNS-CNT
      *            エラー・警告
                   IF     (SPA-COM-CUR (IDX)   =   "1"  )
                     OR   (SPA-COM-GYOU(IDX)   >   ZERO )
                       PERFORM 2002222-LINE-HEN-SEC
                   END-IF
               WHEN    "*"
      *            剤毎の編集
                   MOVE    ZERO                    TO  IDZ
                   PERFORM 2002221-ZAIHENSYU-SEC
                   MOVE    ZERO                    TO  IDZ
                   IF     (SPA-COM-CUR (IDX)   =   "1"  )
                     OR   (SPA-COM-GYOU(IDX)   >   ZERO )
                       PERFORM 2002222-LINE-HEN-SEC
                       IF     (SPA-COM-CUR (IDX)   =   "1"  )
                           MOVE    WRK-H-LINE      TO  WRK-MD-ERR-LINE
                       END-IF
                   END-IF
      *
                   IF      SPA-GMN-INPUTCD(IDX + 1)(1:1)
                                               =   "*"
                       ADD     1               TO  IDX
                   END-IF
      *
               WHEN    OTHER
                   IF      IDZ                 =   ZERO
                       ADD     1                   TO  IDY
                       IF      IDY                 >   COB-ZAI-OUTMAX
                           MOVE    1               TO  FLG-HENCHK
                       END-IF
                       MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                       MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
                       IF      WRK-SRYSYUKBN        =   SPACE
                            MOVE    WRK-SRYKBN      TO  WRK-SRYSYUKBN
                                                                  (1:2)
                            MOVE    "0"             TO  WRK-SRYSYUKBN
                                                                  (3:1)
                            MOVE    WRK-SRYSYUKBN   TO SPA-COM-SRYSYUKBN
                                                                  (IDX) 
                       END-IF
                       MOVE    ZERO                TO  WRK-KINGAK
                       MOVE    ZERO                TO  WRK-HOUKNS-CNT
                   END-IF
                   ADD     1                   TO  IDZ
                   IF      IDZ                 >   50
                       MOVE    1               TO  FLG-HENCHK
                   END-IF
      *
                   IF      FLG-HENCHK          =   ZERO
                       PERFORM 2002221-MEISAI-HEN-SEC
                   END-IF
               END-EVALUATE
           END-PERFORM
      *
           IF     (FLG-HENCHK          =   1     )
      ****** AND  (WRK-MD-ERRCD        =   SPACE )
              AND (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
      *****    MOVE    "AP03"            TO  WRK-MD-ERRCD
               MOVE    "43"              TO  WRK-ERRCD
           END-IF
      *!!!
      *    明細なし
           IF     (IDY                 =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "20"            TO  WRK-ERRCD
           END-IF
           .
       200222-MEDICAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       2002221-MEISAI-HEN-SEC          SECTION.
      *
      *    コード
           MOVE    SPA-COM-INPUTCD(IDX)    TO MDCRES-PRSCRPT-CODE
                                                             (IDY IDZ)
      *    名称
           IF     (SPA-COM-INPUTCD(IDX)(1:1) =   "8")
             OR   (SPA-COM-INPUTCD(IDX)(1:3) =   "008")
             OR   (SPA-COM-INPUTCD(IDX)(1:3) =   "001")
               IF     (SPA-COM-INPUTCD(IDX)  =   "810000001"
                                             OR  "008500000"
                                             OR  "008600000")
      *            フリーコード
                   MOVE    SPA-GMN-MEISYO-G (IDX)
                                               TO  MDCRES-PRSCRPT-NAME
                                                             (IDY IDZ)
               ELSE
                   MOVE    SPA-GMN-MEISYO (IDX)
                                               TO  MDCRES-PRSCRPT-NAME
                                                             (IDY IDZ)
               END-IF
           ELSE
      *        点数マスタの名称
      ***      MOVE    SPA-COM-NAME (IDX)  TO  MDCRES-PRSCRPT-NAME
      ***                                                    (IDY IDZ)
               MOVE    SPACE               TO  TNS-TENSU-REC
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-COM-INPUTCD (IDX)
                                           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               MOVE    TNS-NAME            TO  MDCRES-PRSCRPT-NAME
                                                             (IDY IDZ)
           END-IF
      *    数量
           IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "058"
                                               OR  "001"
                                               OR  "002"
                                               OR  "099")
      *        数量入力のできないコード
               IF      SPA-COM-INPUTCD(IDX)(1:7)   =   "0992000"
                   CONTINUE
               ELSE
                   MOVE    1               TO  WRK-SURYO
                   MOVE    SPACE           TO  SPA-COM-SURFLG (IDX)
               END-IF
           END-IF
      *
      *    一般名記載
           IF      (SPA-COM-INPUTCD (IDX)(1:1) =   "6" )
               IF     (SPA-COM-MEI-KBN (IDX)    =   "1"   )
                   PERFORM 9102-GENERICNAME-READ-SEC
                   IF      FLG-GENERICNAME     =   ZERO
                       MOVE    GENERIC-GENERICNAME
                                           TO  MDCRES-PRSCRPT-GENNAME
                                                             (IDY IDZ)
                       MOVE    "1"         TO  MDCRES-PRSCRPT-GENERIC
                                                             (IDY IDZ)
                   END-IF
               END-IF
               IF     (SPA-COM-MEI-KBN (IDX)    =   "3"   )
      *            処方名称（一般）
                   MOVE    "3"            TO  MDCRES-PRSCRPT-GENERIC
                                                             (IDY IDZ)
      *            点数附加情報検索
                   PERFORM 9101-TENSUPLUS-READ-SEC
                   MOVE    TNSP-SHONAME
                                           TO  MDCRES-PRSCRPT-GENNAME
                                                             (IDY IDZ)
               END-IF
           END-IF
      *
      *    数量
           MOVE    SPA-COM-SURYO(IDX)      TO  WRK-SURYO
           IF      WRK-SURYO               =   ZERO
               MOVE    1                   TO  WRK-SURYO
           END-IF
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)   TO  MDCRES-PRSCRPT-NUMBER
                                                             (IDY IDZ)
      *H29.5
      *    単位コード
           IF     (SPA-COM-INPUTCD (IDX)(1:1) =   "6"
                                              OR  "7"
                                              OR  "1" )
             OR   (SPA-COM-INPUTCD (IDX)(1:3) =   "058"
                                              OR  "059"
                                              OR  "095"
                                              OR  "096" )
               PERFORM 20022211-TANICD-MEI-SEC
           END-IF
      *    フィルム分画数
           IF     (SPA-COM-SRYKBN (IDX)    =   "70" )
              AND (SPA-COM-DATAKBN(IDX)    =   "3"  )
              AND (SPA-COM-BUNGSU (IDX)    >   ZERO )
               MOVE    SPA-COM-BUNGSU(IDX)     TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-PRSCRPT-FILMNUM
                                                             (IDY IDZ)
           END-IF
      *H29.5
      *    加算自動算定なし（在宅加算、精神通院）
           IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )
              AND (SPA-COM-KSNAUTO(IDX)    =   "1"  )
      *        精神通院の２０歳未満加算の自動発生なし
               IF      (SPA-COM-CDKBN-KBN (IDX)    =   "I"  )
                  AND  (SPA-COM-CDKBN-KBNNUM(IDX)  =   "002")
                  AND  (SPA-COM-AGEKSNSRYCD(IDX 1) NOT =   ZERO
                                                        AND SPACE )
                   MOVE    "Yes"           TO  MDCRES-PRSCRPT-NOAUTO
                                                             (IDY IDZ)
               END-IF
      *        在宅加算等の自動算定なし設定
               IF     (SPA-COM-CDKBN-KBN (IDX)     =   "C"  )
                  AND (SPA-COM-CDKBN-KBNNUM(IDX)   =   "002")
                   MOVE    "Yes"           TO  MDCRES-PRSCRPT-NOAUTO
                                                             (IDY IDZ)
               END-IF
      *H29.7
      *        心身療法の２０歳未満加算の自動発生なし
               IF      (SPA-COM-CDKBN-KBN (IDX)    =   "I"  )
                  AND  (SPA-COM-CDKBN-KBNNUM(IDX)  =   "004")
                  AND  (SPA-COM-AGEKSNSRYCD(IDX 1) NOT =   ZERO
                                                        AND SPACE )
                  AND  (SPA-SRYYMD             >=  "20170701")
                   MOVE    "Yes"           TO  MDCRES-PRSCRPT-NOAUTO
                                                             (IDY IDZ)
               END-IF
           END-IF
      *
      *    自費金額
           IF     (SPA-COM-SRYKBN (IDX)    =   "96"
                                           OR  "95"   )
      *        自賠責金額
           OR     (SPA-COM-SRYSYUKBN (IDX)  =   "809" )
           OR     (SPA-COM-JIHIMONEY (IDX)  >   ZERO  )
               MOVE    SPA-COM-JIHIMONEY (IDX) TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                               TO  MDCRES-PRSCRPT-MONEY
                                                             (IDY IDZ)
           END-IF
      *    器材の商品名入力がある時
           IF     (SPA-COM-INPUTCD (IDX)(1:1)   =   "7"   )
             AND  (SPA-COM-AUTOKBN2(IDX)    NOT =   SPACE )
               MOVE    "2"        TO  MDCRES-PRSCRPT-CONTKBN
                                                             (IDY IDZ)
           END-IF
      *    自動算定区分
           MOVE    SPA-COM-AUTOKBN (IDX) TO  MDCRES-PRSCRPT-AUTOKBN
                                                             (IDY IDZ)
      *    継続コメント
           IF      SPA-COM-INPUTCONT (IDX)     =   "1"
               MOVE    "1"        TO  MDCRES-PRSCRPT-CONTKBN
                                                             (IDY IDZ)
           END-IF
      *    内服１種類区分
           IF      SPA-COM-INPUTNAIF (IDX)     =   "1"
               MOVE    "1"        TO  MDCRES-PRSCRPT-NAIFKBN
                                                             (IDY IDZ)
           END-IF
      *    埋め込み入力値
           IF     (SPA-COM-INPUTCD(IDX)(1:2)   =   "84" )
              OR  (SPA-COM-INPUTCD(IDX)(1:4)   =   "0084")
      *       用法コメント
              OR ((SPA-COM-INPUTCD(IDX)(1:3)   =   "001") AND
                  (SPA-COM-YCOMKBN (IDX)       =   "1" ))
      *       予約コード
              OR  (SPA-COM-INPUTCD(IDX)(1:7)   =   "0992081")
      *R02.4
               OR (SPA-COM-INPUTCD(IDX)(1:2)   =   "85"   )
               OR (SPA-COM-INPUTCD(IDX)(1:3)   =   "831"  )
               PERFORM VARYING     IDX-A   FROM    1   BY  1
                       UNTIL      (IDX-A       >   5   )
                   IF      SPA-COM-ATAI (IDX IDX-A)    NOT =   SPACE
                       MOVE    SPA-COM-ATAI (IDX IDX-A)
                                       TO  MDCRES-PRSCRPT-ATAI
                                                     (IDY IDZ IDX-A)
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
               ADD      SPA-COM-JIHIMONEY (IDX)    TO  WRK-KINGAK
           END-IF
      *H29.5
      *    包括検査の判定
           IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                  (SPA-COM-SRYKBN (IDX)        =   "60" ) AND
                  (SPA-COM-DATAKBN (IDX)       =   1    ) AND
                  (SPA-COM-HOUKNSKBN (IDX) NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
           END-IF
      *!
      *
           IF     (IDX                     <   SPA-GMN-MAX) AND
                  (SPA-GMN-INPUTCD (IDX + 1)(1:1)
                                            =   "."    )
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
           END-IF
      *
      *    エラー・警告
           IF     (SPA-COM-CUR (IDX)   =   "1"  )
             OR   (SPA-COM-GYOU(IDX)   >   ZERO )
               PERFORM 2002222-LINE-HEN-SEC
           END-IF
      *
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
      *        剤毎の編集
               PERFORM 2002221-ZAIHENSYU-SEC
               MOVE    ZERO                    TO  IDZ
           END-IF
      *
           .
       2002221-MEISAI-HEN-EXT.
           EXIT.
      *
      *H29.5
      *****************************************************************
      *    単位コード編集処理
      *****************************************************************
       20022211-TANICD-MEI-SEC          SECTION.
      *
           IF     (SPA-COM-INPUTCD (IDX)(1:1) =   "6"
                                              OR  "7")
      *R02.2
      *        薬剤・器材はユーザー単位反映
      *        MOVE    SPA-COM-TANICD (IDX)    TO  MDCRES-PRSCRPT-TANICD
      *                                                      (IDY IDZ)
      *        点数附加情報検索
               IF     (FLG-TENSU           =   ZERO)
                   PERFORM 9101-TENSUPLUS-READ-SEC
                   IF      (FLG-TENSUPLUS          =   ZERO )
                     AND   (TNSP-USERTANICD    NOT =   SPACE)
                        MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                        MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
                   END-IF
               END-IF
               MOVE    TNS-TANICD      TO  MDCRES-PRSCRPT-TANICD
                                                             (IDY IDZ)
               MOVE    TNS-TANINAME
                                       TO  MDCRES-PRSCRPT-TANICD-NAME
                                                             (IDY IDZ)
               GO  TO  20022211-TANICD-MEI-EXT
           END-IF
      *
           IF      SPA-COM-TANICD (IDX)    =   SPACE
                                           OR  ZERO
               CONTINUE
           ELSE
               MOVE    TNS-TANINAME
                                       TO  MDCRES-PRSCRPT-TANICD-NAME
                                                             (IDY IDZ)
               MOVE    SPA-COM-TANICD (IDX)    TO  MDCRES-PRSCRPT-TANICD
                                                             (IDY IDZ)
           END-IF
      *
           .
       20022211-TANICD-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤編集処理
      *****************************************************************
       2002221-ZAIHENSYU-SEC          SECTION.
      *
      *    回数
           IF      SPA-COM-KAISU (IDX) >   ZERO
               MOVE    SPA-COM-KAISU (IDX) TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MDC-CLASS-NUMBER(IDY)
           END-IF
      *    剤点数
           IF      SPA-COM-TENSU (IDX) >   ZERO
               IF      SPA-COM-PLUSKBN (IDX)   =   1
      *            マイナス点数
                   COMPUTE WRK-SURYO           =   SPA-COM-TENSU (IDX)
                                               *   -1
               ELSE
                   MOVE    SPA-COM-TENSU (IDX) TO  WRK-SURYO
               END-IF
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-MDC-POINT(IDY)
           END-IF
      *    剤金額
           IF      WRK-KINGAK          >   ZERO
               MOVE    WRK-KINGAK          TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-MDC-MONEY(IDY)
           END-IF
      *    剤区分（包括分）
           IF      SPA-COM-HOUKATU-ZAI (IDX)   =   1
                MOVE    "True"         TO  MDCRES-INCLUSION-CLASS(IDY)
           END-IF
      *    診療区分
           MOVE    WRK-SRYKBN          TO  MDCRES-MDC-CLASS-KBN (IDY)
           MOVE    WRK-SRYSYUKBN       TO  MDCRES-MDC-CLASS     (IDY)
           PERFORM 510-SRYKBN-MEI-SEC
           MOVE    WRK-SRYSYUKBNMEI    TO  MDCRES-MDC-CLASS-NAME(IDY)
      *
      *    包括検査の検査数
           IF      WRK-HOUKNS-CNT      >   ZERO
               MOVE    WRK-HOUKNS-CNT      TO  MDCRES-HKTKNSA-CNT(IDY)
           END-IF
      *
      *    算定日
           MOVE    SPA-SRYYMD (7:2)    TO  WRK-SRYDD
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD         >   31
               IF      SPA-COM-NYUINDAY (IDX IDD)  =   ZERO
                   MOVE    "0"             TO  MDCRES-MDC-DAY
                                                 (IDY IDD)
               ELSE
                   MOVE    "1"             TO  MDCRES-MDC-DAY
                                                 (IDY IDD)
                   IF      IDD         NOT =   WRK-SRYDD
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-IF
           END-PERFORM
      *    算定日のみの指定は、設定なし
      *    IF      FLG-ARI             =   ZERO
      *        MOVE    SPACE           TO  MDCRES-MDC-DAY-G
      *                                                 (IDY)
      *    END-IF
      *
           .
       2002221-ZAIHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    エラー・警告行位置編集処理
      *****************************************************************
       2002222-LINE-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-H-LINE
           MOVE    SPACE               TO  WRK-H-IDX
           IF      IDY                 >   ZERO
               MOVE    IDY             TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  WRK-H-LINE
           END-IF
           IF      IDZ                 >   ZERO
               MOVE    IDZ             TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  WRK-H-IDX
           END-IF
      *
           IF      SPA-COM-GYOU(IDX)   >   ZERO
               MOVE    SPA-COM-GYOU(IDX)   TO  IDK
               MOVE    WRK-H-LINE          TO  TBL-KEI-LINE (IDK)
               MOVE    WRK-H-IDX           TO  TBL-KEI-IDX  (IDK)
           END-IF
           IF      SPA-COM-CUR (IDX)   =   "1"
               IF     (SPA-GMN-INPUTCD (IDX)   =   WRK-ERR-INPUTCD)
                  OR  (SPA-COM-INPUTCD (IDX)   =   WRK-ERR-SRYCD  )
                   MOVE    WRK-H-LINE          TO  WRK-MD-ERR-LINE
                   MOVE    WRK-H-IDX           TO  WRK-MD-ERR-IDX
               END-IF
           END-IF
           .
       2002222-LINE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為パラメタ登録処理
      *****************************************************************
       200231-WKSRYPARA-SEC          SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "4"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    中途データ登録とする
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    ZERO                TO  CNT-WKSRY
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
               MOVE    PARA-DATA-REC       TO  WKSRYACT-REC
      *        中途終了分とする
               MOVE    "WKSRYACT"          TO  WKSRY-TERMID
               MOVE    "APIV3"             TO  WKSRY-OPID
               MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
               PERFORM 2002341-DBPARA-UPD-SEC
      *
               ADD     1                   TO  CNT-WKSRY
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    明細なし
           IF     (CNT-WKSRY           =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "20"            TO  WRK-ERRCD
           END-IF
      *
           .
       200231-WKSRYPARA-EXT.
           EXIT.
      *****************************************************************
      *    包括診療チェック処理
      *****************************************************************
       5109-HKTSANTEI-CHK-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "6"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-ORCSC-TEISEI-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      ORCSCHKTCHK-HKTFLG  =   1
               MOVE    1               TO  SPA-HKTSANTEI-CHK
           ELSE
               MOVE    ZERO            TO  SPA-HKTSANTEI-CHK
           END-IF
           IF      ORCSCHKTCHK-HKTERR  =   1
      *        包括をエラーとする削除処理
               PERFORM 51091-HKTSANTEI-DEL-SEC
           END-IF
           .
       5109-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療エラー削除処理
      *****************************************************************
       51091-HKTSANTEI-DEL-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "3"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    診療行為、計計算処理、編集
           PERFORM 510-TBLHEN-SYORI-SEC
           .
       51091-HKTSANTEI-DEL-EXT.
           EXIT.
      *H29.1
      *****************************************************************
      *    投薬調剤料の翌月チェック処理
      *****************************************************************
       51092-NEXTCHOZAI-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )
                          OR ((SPA-GMN-INPUTCD(IDX) =   SPACE) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE))
                          OR  (SPA-ERRCD        NOT =   SPACE )
      *
      *        薬評・減点・持参薬対象外
               IF     (SPA-COM-SRYKBN   (IDX)  =   "21"  )
                AND   (SPA-COM-YAKUHYO  (IDX)  =   SPACE  )
                AND   (SPA-COM-GENTEN   (IDX)  =   SPACE  )
                AND   (SPA-COM-JISANYAKU(IDX)  =   SPACE  )
                AND   (SPA-COM-HOUKATU-ZAI(IDX)    =   1  )
                AND   (SPA-COM-SRYSYUKBN (IDX) NOT =   "214" )
                   IF      SPA-COM-ZAIEND (IDX)    =   "1"
                       PERFORM 51921-DAYCHEK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD(1:1)      =   "K"
               PERFORM 3001-KEIMSGSYORI-SEC
           END-IF
           .
       51092-NEXTCHOZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51921-DAYCHEK-SEC             SECTION.
      *
      *
           MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
           MOVE    ZERO                TO  FLG-NEXTTUKI
      *    投薬で入院回数の時、回数集計
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )
                           OR (FLG-NEXTTUKI    =   1 )
               IF      SPA-COM-NYUINDAY (IDX IDN1) >   ZERO
                   MOVE    IDN1                TO  IDN4
                   PERFORM VARYING     IDN3    FROM    1   BY  1
                           UNTIL      (IDN3    >   WRK-KAISU )
                                   OR (FLG-NEXTTUKI    =   1 )
      *                翌月あり
                       IF      IDN4            >   SPA-SRYLST-DD
                           MOVE    1               TO  FLG-NEXTTUKI
                       END-IF
                       ADD     1                   TO  IDN4
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF      FLG-NEXTTUKI        =   1
               IF      SPA-COM-KZMFLGN2 (IDX)  =   SPACE
                   MOVE    "K190"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR  (IDX)
                   MOVE    "1"             TO  SPA-COM-KZMFLGN2(IDX)
               END-IF
           END-IF
           .
       51921-DAYCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容確認処理
      *****************************************************************
       2003-SYORI-02-SEC        SECTION.
      *
      *    レスポンス番号チェック
           IF     (WRK-ERRCD           =   SPACE  )
           AND    (WRK-RESPONSE-NUMBER =   "04"
                                       OR  "05"   )
           AND    (MDCREQ-SELECT-MODE  NOT =   "1" )
               MOVE    "09"                TO  WRK-ERRCD
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
               GO      TO      2003-SYORI-02-EXT
           END-IF
           IF     (WRK-RESPONSE-NUMBER =   "03"
                                       OR  "04"
                                       OR  "05"   )
               CONTINUE
           ELSE
               MOVE    "19"                TO  WRK-ERRCD
               MOVE    "02"                TO  MDCRES-RESPONSE-NUMBER
               GO      TO      2003-SYORI-02-EXT
           END-IF
      *
      *    診察料確認処理
           PERFORM 20031-HENSYU-02-SEC
      *
      *    返却レスポンス番号
           IF     (WRK-ERRCD       =   SPACE )
      *    エラーなしは次へ
               MOVE    "04"            TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
           IF      WRK-ERRCD           =   "80"
      *        ＤＢエラー
               GO      TO      2003-SYORI-02-EXT
           END-IF
      *    SPATMPの削除処理
           PERFORM 2009-SPATMP-DEL-SEC
      *    SPAの登録
           PERFORM 2008-SPATMP-INS-SEC
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療確認処理
      *****************************************************************
       20031-HENSYU-02-SEC        SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "K02SPA"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-K02
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
               GO      TO      20031-HENSYU-02-EXT
           END-IF
      *
      *    前回の確認コード
           MOVE    SPA-KIDCD           TO  WRK-MAE-KIDCD
      *    診療行為内容編集
           PERFORM 200311-MDCRES-HEN03-SEC
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *    中途データ展開
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
      *    処理区分（１：中途終了展開）
           MOVE    1                   TO  ORCSCWKSRY-SYORIFLG
      *    追加開始位置
           MOVE    ZERO                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  SPA-ERRCD
           END-IF
      *
           MOVE    SPACE               TO  WRK-GENECHG-KBN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                    OR     (SPA-ERRCD          NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "."
                       CONTINUE
                   WHEN    "*"
      *                算定日指定チェック
                       PERFORM 200224-KAISUU-CHK-SEC
                   WHEN    OTHER
      *                点数マスタチェック（再表示編集時にエラー）
                       IF      SPA-COM-CUR (IDX)   NOT =   SPACE
                           PERFORM 200222-KIHONCHK-SEC
                       END-IF
      *!!
      *                後発品変更不可
                       IF      SPA-COM-INPUTCD (IDX)   =   "099209910"
                           MOVE    "1"             TO  WRK-GENECHG-KBN
                       END-IF
      *                後発品変更可
                       IF      SPA-COM-INPUTCD (IDX)   =   "099209911"
                           MOVE    "2"             TO  WRK-GENECHG-KBN
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    確認メッセージの最初から
           IF      MDCREQ-SELECT-MODE  =   "1"
               MOVE    ZERO                TO  SPA-TOKUTEIFLG
               MOVE    ZERO                TO  SPA-CHOKITOFLG
               MOVE    ZERO                TO  SPA-CHOKITOFLG-CHK
               MOVE    ZERO                TO  SPA-TECHOKBN-CHK
               MOVE    ZERO                TO  SPA-NAIFUKU-CHK
               MOVE    ZERO                TO  SPA-NAIFUKU-OK
               MOVE    ZERO                TO  SPA-PSYCHO-OK
               MOVE    ZERO                TO  SPA-PSYCHO-CHK
               MOVE    ZERO                TO  SPA-TEIGEN30-CHK
               MOVE    ZERO                TO  SPA-TEIGEN30-OK
               MOVE    SPACE               TO  WRK-MAE-KIDCD
               MOVE    SPACE               TO  SPA-KIDCD
           END-IF
      *
      *    診療行為、計計算処理、編集
           PERFORM 510-TBLHEN-SYORI-SEC
      *
      *    包括の判定
           IF     (WRK-ERRCD           =   SPACE )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
      *            治験以外
             AND  (SPA-CHIKENKBN   NOT =   "1"   )
               IF     (SPA-HKTSANTEI-FLG   =   1     )  OR
                      (SPA-HKTSANTEI-08    =   1     )  OR
                      (SPA-NYUHKTCHK       =   1     )
                   PERFORM 5109-HKTSANTEI-CHK-SEC
      *R01.9
                   IF      SPA-ERRCD(1:1)      =   "K"
                       PERFORM 3001-KEIMSGSYORI-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    一般名記載区分編集
      *****PERFORM 520-GENE-HEN-SYORI-SEC
      *
           IF     (WRK-ERRCD           =   SPACE )  AND
                  (SPA-ERRCD           =   SPACE )
               PERFORM 20033-TBLCHI-SYORI-SEC
           END-IF
      *
      *    エラーメッセージ編集処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        明細編集（登録）
               PERFORM 200234-MEDICAL-OUTHEN-SEC
           ELSE
      *        明細編集（画面）
               PERFORM 200222-MEDICAL-HEN-SEC
      *        リクエスト＝０２へ
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
               IF      WRK-ERRKBN      =   "S"
                   MOVE    "03"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
           END-IF
      *
           .
       20031-HENSYU-02-EXT.
           EXIT.
      *****************************************************************
      *    返却内容編集処理
      *****************************************************************
       200311-MDCRES-HEN03-SEC          SECTION.
      *
           IF      WRK-SRYKA       NOT =   SPACE
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
           END-IF
     **    IF      WRK-DRCD        NOT =   SPACE
      *        MOVE    WRK-DRCD            TO  SPA-DRCD
      **   END-IF
      *    前回保険組合せ
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *    保険組合せ編集処理
           IF      WRK-HKNCOMBI        =   "9999"
               MOVE    SPA-GMN-HKNCOMBIMEI
                                       TO  MDCRES-MAIN-INSURANCE-NAME
           ELSE
               INITIALIZE                      ORCSAPIHKNAREA
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
               CALL    "ORCSAPIHKN"        USING
                                           SPA-AREA
                                           ORCSAPIHKNAREA
               MOVE    1                   TO  IDX
               PERFORM 2002101-HKNHEN-TBL-SEC
      *        保険組合せ負担割合編集
               IF      SPA-FTNRATE     NOT =   ZERO
                   COMPUTE WRK-FTNRATE         =   SPA-FTNRATE
                                               /   100
                   MOVE    WRK-FTNRATE         TO  WRK-Z9
                   MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
                   MOVE    WRK-FTNRATEMEI  TO  MDCRES-COMBINATION-RATE
               ELSE
                   MOVE    "0"             TO  MDCRES-COMBINATION-RATE
               END-IF
           END-IF
           MOVE    WRK-HKNCOMBI        TO  MDCRES-COMBINATION-NUMBER
           .
       200311-MDCRES-HEN03-EXT.
           EXIT.
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       20033-TBLCHI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-END-SYORI2
      *    確認メッセージ返却
           IF      WRK-MAE-KIDCD       NOT =   SPACE
               PERFORM 200331-KIDCD-CHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    1               TO  FLG-END-SYORI2
               END-IF
           END-IF
      *    緊急時間外警告フラグリア
           MOVE    ZERO                TO  SPA-KENJIKAN-CHK
           MOVE    ZERO                TO  SPA-GAZJIKAN-CHK
      *
           PERFORM
               UNTIL    FLG-END-SYORI2 =   1
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "2"                 TO  ORCSC100-KBN
               MOVE    "1"                 TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    ZERO                TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *        確認メッセージ編集
               IF      SPA-KIDCD        NOT =   SPACE
                   PERFORM 200332-KIDCD-HEN-SEC
                   MOVE    1                   TO  FLG-END-SYORI2
               ELSE
      *            警告は登録対象
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    2                   TO  FLG-ERR
                       PERFORM 3001-KEIMSGSYORI-SEC
                       MOVE    ZERO            TO  FLG-END-SYORI2
                   ELSE
                       MOVE    1               TO  FLG-END-SYORI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20033-TBLCHI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージチェック処理 (K02と同じ処理）
      *****************************************************************
       200331-KIDCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
           EVALUATE    MDCREQ-SELECT-ANSWER
               WHEN    "OK"
               WHEN    "Ok"
                   MOVE    "OK"             TO  SPA-KID1-FLG
               WHEN    "NO"
               WHEN    "No"
                   MOVE    "NO"             TO  SPA-KID1-FLG
               WHEN    OTHER
                   MOVE    WRK-MAE-KIDCD    TO  SPA-KIDCD
                   PERFORM 200332-KIDCD-HEN-SEC
                   MOVE    "31"             TO  WRK-ERRCD
                   GO      TO      200331-KIDCD-CHK-EXT
           END-EVALUATE
      *
      **** EVALUATE    WRK-MAE-KIDCD
      *        特定疾患処方管理加算の算定確認
      *        WHEN    "0113"
      *        WHEN    "0213"
      *            IF      SPA-KID1-FLG        =   "OK"
      *                MOVE    2                   TO  SPA-TOKUTEIFLG
      *            ELSE
      *                MOVE    1                   TO  SPA-TOKUTEIFLG
      *            END-IF
      **** END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       200331-KIDCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ編集処理
      *****************************************************************
       200332-KIDCD-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
      **   EVALUATE    SPA-KIDCD
      *        WHEN    OTHER
      *            MOVE    SPA-KIDCD       TO  WRK-KIDMSG
      **   END-EVALUATE
      *
           MOVE    SPA-KIDCD           TO  MDCRES-SELECT-CODE
           MOVE    WRK-KIDMSG          TO  MDCRES-SELECT-MSG
      *
      *    確認あり
           MOVE    "True"              TO  MDCRES-MEDICAL-SELECT-FLAG
      *
           MOVE    "40"                TO  WRK-ERRCD
           MOVE    "S"                 TO  WRK-ERRKBN
      *
           .
       200332-KIDCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    明細編集（登録）処理
      *****************************************************************
       200234-MEDICAL-OUTHEN-SEC      SECTION.
      *
      *    ワーク診療行為検索
           PERFORM 200234-WKSRYACT-INIT-SEC
      *
           IF      LNK-WKSRYACT-MAX    =   ZERO
               GO      TO      200234-MEDICAL-OUTHEN-EXT
           END-IF
      *
      *    剤削除の連番編集
           MOVE    ZERO                TO  WRK-DELRENNUM
           MOVE    ZERO                TO  WRK-ZAINUM
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   LNK-WKSRYACT-MAX)
               MOVE    ZERO            TO  LNK-WKSRYAPI-RENNUM (IDX)
               MOVE    ZERO            TO  LNK-WKSRYAPI-DELKBN (IDX)
      *        自動発生分、削除対象
               IF      LNK-WKSRY-AUTOKBN (IDX 1)   =   "3"
                                                   OR  "4"
                                                   OR  "6"
                                                   OR  "7"
                   MOVE    1                   TO  FLG-DELCHK
                   IF      (LNK-WKSRY-INPUTCD (IDX 1)
                                               =   "820000047"
                                               OR  "820000166")
                      OR   (LNK-WKSRYP-PLUSKBN (IDX)  =   1   )
      *                削除できるかの判定（減）・マイナス対象外
                       MOVE    ZERO                TO  FLG-DELCHK
                   END-IF
      *            入院調剤料は削除できない
                   IF      LNK-WKSRY-SRYSYUKBN(IDX) =   "240"
                                                   OR  "260"
                       MOVE    ZERO                TO  FLG-DELCHK
                   END-IF
      *
                   IF      FLG-DELCHK          =   1
                       IF      WRK-ZAINUM  NOT =   LNK-WKSRY-ZAINUM
                                                              (IDX)
                           ADD     1               TO  WRK-DELRENNUM
                       END-IF
                       MOVE    WRK-DELRENNUM   TO  LNK-WKSRYAPI-RENNUM
                                                             (IDX)
                       MOVE    LNK-WKSRY-ZAINUM(IDX)
                                               TO  WRK-ZAINUM
                   END-IF
               END-IF
           END-PERFORM
      *    ワーク診療行為更新
           PERFORM 2002341-WKSRYACT-DELUPD-SEC
      *
      *    診療内容編集サブ
           INITIALIZE                  ORAPI021SUB2V3AREA
           MOVE    "1"                 TO  ORAPISUB2-SYORIKBN
           MOVE    LNK-WKSRYACT-MAX    TO  ORAPISUB2-WKSRYACT-MAX
           CALL    "ORAPI021SUB2V3"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB2V3AREA
                                       LNK-WKSRYACT-AREA
      *    算定日
           MOVE    SPA-SRYYMD (7:2)    TO  WRK-SRYDD
      *
           INITIALIZE                  MDCRES-MEDICAL-INF-G
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   COB-ZAI-OUTMAX )
                       OR  (ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           =   SPACE )
               MOVE    ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           TO  MDCRES-MDC-CLASS-KBN(IDX)
               MOVE    ORAPISUB2-MDC-CLASS (IDX)
                                           TO  MDCRES-MDC-CLASS (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NAME (IDX)
                                           TO  MDCRES-MDC-CLASS-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NUMBER (IDX)
                                           TO  MDCRES-MDC-CLASS-NUMBER
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-POINT (IDX)
                                           TO  MDCRES-MDC-POINT (IDX)
               MOVE    ORAPISUB2-MDC-MONEY (IDX)
                                           TO  MDCRES-MDC-MONEY (IDX)
      *        剤区分（包括分）
               IF       ORAPISUB2-MDC-CODE (IDX)  =   "1"
                    MOVE    "True"         TO  MDCRES-INCLUSION-CLASS
                                                                (IDX)
               END-IF
               MOVE    ORAPISUB2-DEL-NUMBER (IDX)
                                           TO  MDCRES-DEL-NUMBER(IDX)
      *H29.5
      *        包括検査の検査数
               IF      ORAPISUB2-HKTKNSA-CNT (IDX) >   ZERO
                   MOVE    ORAPISUB2-HKTKNSA-CNT (IDX)
                                               TO  MDCRES-HKTKNSA-CNT
                                                                (IDX)
               END-IF
      *        算定日
               MOVE    ORAPISUB02-MDC-DAY-G (IDX)
                                               TO  MDCRES-MDC-DAY-G
                                                                   (IDX)
      *
      *        剤内容
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   (IDY            >   50    )
                           OR  (ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                               =   SPACE )
                   MOVE    ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CODE
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAME
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NUMBER (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NUMBER
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-MONEY (IDX IDY)
                                       TO  MDCRES-PRSCRPT-MONEY
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  MDCRES-PRSCRPT-ATAI-G
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-GENERIC (IDX IDY)
                                       TO  MDCRES-PRSCRPT-GENERIC
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-GENNAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-GENNAME
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CONTKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAIFKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-AUTOKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-AUTOKBN
                                                            (IDX IDY)
      *H29.5
      *            単位コード
                   MOVE    ORAPISUB2-PRSCRPT-TANICD (IDX IDY)
                                       TO  MDCRES-PRSCRPT-TANICD
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-TANICD-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-TANICD-NAME
                                                            (IDX IDY)
      *            フィルム分画数
                   MOVE    ORAPISUB2-PRSCRPT-FILMNUM (IDX IDY)
                                       TO  MDCRES-PRSCRPT-FILMNUM
                                                            (IDX IDY)
      *            加算自動算定なし
                   MOVE    ORAPISUB2-PRSCRPT-NOAUTO (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NOAUTO
                                                            (IDX IDY)
      *
               END-PERFORM
           END-PERFORM
           IF     (ORAPISUB2-ERRCD NOT =   SPACE )
      ****** AND  (WRK-MD-ERRCD        =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
      *****    MOVE    ORAPISUB2-ERRCD     TO  WRK-MD-ERRCD
      *****    MOVE    ORAPISUB2-ERRMSG    TO  WRK-MD-ERRMSG
               MOVE    "43"                TO  WRK-ERRCD
           END-IF
      *
           .
       200234-MEDICAL-OUTHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       200234-WKSRYACT-INIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "K02 LNK-WKSRYACT-MAX 400 OVR"
                   MOVE    "42"                TO  WRK-ERRCD
               ELSE
                   MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200234-WKSRYACT-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイル更新処理
      *****************************************************************
       2002341-WKSRYACT-DELUPD-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           PERFORM 30012-PARA-DBDELETE-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   LNK-WKSRYACT-MAX)
      *        TBL_PARAに保存
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "WKSRYACT"          TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
      *        中途終了分とする
               MOVE    "WKSRYACT"          TO  LNK-WKSRY-TERMID (IDX)
               MOVE    "APIV3"             TO  LNK-WKSRY-OPID (IDX)
      *
               MOVE    LNK-WKSRYACT-REC(IDX)
                                           TO  PARA-DATA-REC
      *
                PERFORM 30010-PARA-DBINSERT-SEC
      *
           END-PERFORM
           .
       2002341-WKSRYACT-DELUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ更新処理
      *****************************************************************
       2002341-DBPARA-UPD-SEC     SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA UPDATE ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
      *
           .
       2002341-DBPARA-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ追加処理
      *****************************************************************
       30010-PARA-DBINSERT-SEC     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  PARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  PARA-UPHMS
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
      *
           .
       30010-PARA-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ追加処理
      *****************************************************************
       30012-PARA-DBDELETE-SEC     SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       30012-PARA-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せの決定処理
      *****************************************************************
       200215-HKNCOMBI-SET-SEC     SECTION.
      *
      *    包括分入力
           IF     (MDCREQ-MAIN-INSURANCE-CLASS =   "999" )
               OR (MDCREQ-COMBINATION-NUMBER   =   "9999")
               INITIALIZE              MDCRES-INSURANCE-G
               MOVE    "9999"          TO  WRK-HKNCOMBI
               MOVE    "包括分入力"    TO  MDCRES-MAIN-INSURANCE-NAME
           ELSE
               PERFORM 2002151-HKNCOMBI-CALL-SEC
           END-IF
      *    保険組合せ番号
           MOVE    WRK-HKNCOMBI        TO  MDCRES-COMBINATION-NUMBER
           .
       200215-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       2002151-HKNCOMBI-CALL-SEC     SECTION.
      *
           MOVE    ZERO                TO  WRK-HKNCOMBI
      *
           IF      MDCREQ-COMBINATION-NUMBER   =   SPACE
      *        保険組合せ編集処理
               INITIALIZE                      ORCSAPIHKNAREA
      *        対象なし有
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
      *        対象なし有
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
      *        診療科
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                           TO  ORCSAPIHKN-IN-HKNNUM
      *        保険者番号
               MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                           TO  ORCSAPIHKN-IN-HKNJANUM
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-NAME 
                                           TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *        記号
               MOVE    MDCREQ-MAIN-MARK    TO  ORCSAPIHKN-IN-MARK
      *        番号
               MOVE    MDCREQ-MAIN-NUMBER  TO  ORCSAPIHKN-IN-NUMBER
      *        公費情報
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
      *            公費種類
                   MOVE    MDCREQ-INSURANCE-CLASS (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM (IDX)
      *            公費名称
                   MOVE    MDCREQ-INSURANCE-NAME  (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX)
      *            負担者番号
                   MOVE    MDCREQ-PROVIDER-NUMBER(IDX)
                                       TO  ORCSAPIHKN-IN-FTNJANUM (IDX)
      *            受給者番号
                   MOVE    MDCREQ-RECIPIENT-NUMBER (IDX)
                                       TO  ORCSAPIHKN-IN-JKYSNUM  (IDX)
               END-PERFORM
           ELSE
      *        保険組合せチェック
               PERFORM 200311-HKNCOMBI-NUM-SEC
      *        保険組合せ編集処理
               INITIALIZE                      ORCSAPIHKNAREA
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
      *        診療科
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
           END-IF
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
               IF      MDCREQ-COMBINATION-NUMBER   =   SPACE
                   MOVE    "17"                    TO  WRK-ERRCD
               ELSE
                   MOVE    "16"                    TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
      *
      *        出力領域保険情報編集
               MOVE    1                   TO  IDX
               PERFORM 2002101-HKNHEN-TBL-SEC
           END-IF
           .
       2002151-HKNCOMBI-CALL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       200311-HKNCOMBI-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "16"                TO  WRK-ERRCD
               MOVE    MDCREQ-COMBINATION-NUMBER   TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *
           .
       200311-HKNCOMBI-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
           IF      ORCSAPIHKN-THKNCOMBI (IDX)
                                   NOT =   ORCSAPIHKN-OT-HKNCOMBI
      *        保険組合せ番号なし
               MOVE    "16"                TO  WRK-ERRCD
               GO      TO      2002101-HKNHEN-TBL-EXT
           END-IF
      *
           INITIALIZE              MDCRES-INSURANCE-G
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  MDCRES-MAIN-MARK
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  MDCRES-MAIN-NUMBER
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  MDCRES-MAIN-CONTINUATION
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  MDCRES-MAIN-ASSISTANCE
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                   TO  MDCRES-MAIN-ASSI-NAME
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  MDCRES-MAIN-FAMILY
      *    被保険者名
      *    MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
      *                            TO  MDCRES-MAIN-POLICYHOLDER
      *    有効開始日
      *    MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
      *                            TO  MDCRES-MAIN-START-DATE
      *    有効終了日
      *    MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
      ***                          TO  MDCRES-MAIN-END-DATE
      *    非表示区分判定
           IF      ORCSAPIHKN-HYOJIKBN (IDX)   =   "1"
                                               OR  "2"
      *        非表示分
               MOVE    "18"                    TO  WRK-ERRCD
           END-IF
      *
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  MDCRES-INSURANCE-CLASS
                                                             (IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  MDCRES-INSURANCE-NAME
                                                             (IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  MDCRES-PROVIDER-NUMBER
                                                             (IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  MDCRES-RECIPIENT-NUMBER
                                                             (IDY2)
      *        外来−負担率（割）
      *        MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
      *                                TO  MDCRES-RATE-OUTPATIENT
      *                                                      (IDY2)
      *        外来−固定額
      *        MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
      *                                TO  MDCRES-MONEY-OUTPATIENT
      *                                                      (IDY2)
      *        有効開始日
      *        MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
      *                                TO  MDCRES-INSURANCE-START-DATE
      *                                                      (IDY2)
      *        有効終了日
      *        MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
      *                                TO  MDCRES-INSURANCE-END-DATE
      *                                                      (IDY2)
      *        入院−負担率（割）
      *        MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
      *                                TO  MDCRES-RATE-ADMISSION
      *                                                      (IDY2)
      *        入院−固定額
      *        MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
      *                                TO  MDCRES-MONEY-ADMISSION
      *                                                      (IDY2)
           END-PERFORM
      *
      *    労災区分
           MOVE    ORCSAPIHKN-RSI-HKNKBN (IDX) TO  WRK-RSI-HKNKBN
           MOVE    ORCSAPIHKN-HKNNUM (IDX)     TO  WRK-HKNNUM
      *
           .
       2002101-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *     SPATMPのＳＰＡの取得処理
      *****************************************************************
       2008-SPATMP-SEL-SEC            SECTION.
      *
      *    UIDコードをTERMID
           MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
      *
      *    TBL-SPATMP
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-AREA
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF      SPA-TERMID      NOT =   SPATMP-TERMID
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
      *    レスポンス番号取得
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21NV3"          TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA         =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
               MOVE    APIPARA-PTNUM       TO  WRK-PARA-PTNUM
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
      *        診療行為登録の処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "10"
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2008-SPATMP-SEL-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの登録処理
      *****************************************************************
       2008-SPATMP-INS-SEC            SECTION.
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K02"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K02             TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
      *    API処理登録(レスポンス番号保存)
           PERFORM 2008-APIPARAINS-SEC
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
      *****************************************************************
      *     API処理登録処理
      *****************************************************************
       2008-APIPARAINS-SEC            SECTION.
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21NV3"          TO  APIPARA-FILEMEI
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
           MOVE    "API21NV3"          TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    レスポンス番号・電子カルテUID保存
           MOVE    SPACE               TO  WRK-APIV3DATA-AREA
           MOVE    MDCRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
      *    処理ＰＧ
           MOVE    "10"                TO  WRK-PARA-SYORIPG
      *
      **** MOVE    WRK-DENPNUM         TO  WRK-PARA-DENPNUM
           MOVE    WRK-SYORI-MODE      TO  WRK-PARA-MODE
      *    訂正分
           MOVE    WRK-MOD-SRYKA       TO  WRK-PARA-SRYKA
           MOVE    WRK-MOD-HKNCOMBI    TO  WRK-PARA-HKNCOMBI
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    MDCRES-KARTE-UID    TO  APIPARA-KARTE-UID
           MOVE    WRK-PTNUM           TO  APIPARA-PTNUM
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
      *
           .
       2008-APIPARAINS-EXT.
           EXIT.
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
               AT   END
                   MOVE    SPACE           TO  WRK-SRYKBN
                   MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
               WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                   MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                   MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKUHEN-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MEISYO          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-MEISYO
           END-IF
           .
       800-ZENKAKUHEN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-SYORI-END-SEC          SECTION.
      *
      *    一時データ削除・排他制御解除
           INITIALIZE                  ORAPIV3SUB01AREA
           MOVE    "1"                 TO  APIV3SUB01-SYORI
           MOVE    SPA-TERMID          TO  APIV3SUB01-TERMID
           MOVE    MDCREQ-KARTE-UID    TO  APIV3SUB01-KARTE-UID
           MOVE    "AP21"              TO  APIV3SUB01-API-GYOUMUID
           MOVE    "API21NV3"          TO  APIV3SUB01-API-FILEMEI
           MOVE    WRK-PTNUM           TO  APIV3SUB01-PTNUM
           MOVE    "K02"               TO  APIV3SUB01-PARA-GYOUMUID
           CALL    "ORAPIV3SUB01"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPIV3SUB01AREA
           IF      APIV3SUB01-ERRCD    NOT =   SPACE
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "60"                TO  WRK-ERRCD
                   MOVE    APIV3SUB01-ERRMSG   TO  WRK-ERRMSG
               END-IF
           END-IF
           .
       300-SYORI-END-EXT.
           EXIT.
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       700-FILE-DELETE-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *    SPATMPの削除処理
           PERFORM 2009-SPATMP-DEL-SEC
      *
           .
       700-FILE-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
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
           WHEN    "02"
               MOVE    "診療科が未設定です"
                                               TO  WRK-ERRMSG
      **   WHEN    "03"
      *        MOVE    "ドクターが未設定です"
      **                                       TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "電子カルテＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "オルカＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "06"
               MOVE    "前回と診療日付が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "07"
               MOVE    "前回と患者番号が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "08"
               STRING  "初回接続が完了していません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "09"
               STRING  "確認処理は終了しています。"
                       "次の処理を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "19"
               STRING  "処理の順番が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "同日再入院区分が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません。"
                                               TO  WRK-ERRMSG
      *??  WHEN    "14"
      *        MOVE    "ドクターが存在しません"
      *??                                      TO  WRK-ERRMSG
           WHEN    "15"
               MOVE     "時間外区分は、０から４で設定して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               STRING  "保険組合せ番号が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "17"
               STRING  "保険組合せが存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "18"
               STRING  "保険組合せは入院非表示となっています。"
                       "入院では使用できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "20"
               STRING  "診療内容が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "21"
               MOVE     "診療内容に誤りがあります"
                                               TO  WRK-ERRMSG
           WHEN    "22"
           WHEN    "23"
               CONTINUE
           WHEN    "24"
               MOVE     "入院中ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "25"
               CONTINUE
      *
           WHEN    "27"
               MOVE     "該当の受診履歴が存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "28"
               STRING  "訂正対象診療科・保険組合せが違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "29"
               STRING  "訂正処理の順番が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "30"
               STRING  "診療行為にエラーがあります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "31"
               STRING  "選択結果エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "40"
               STRING  "選択項目があります。"
                       "選択結果を返却してください。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "42"
               STRING  "診療行為が４００行をオーバーします。"
                       "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "43"
               STRING  "剤数・明細数が最大件数以上となります。"
                       "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "80"
               STRING  "一時データ出力エラーです。"
                       "強制終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
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
               MOVE    "他端末使用中。"        TO  WRK-ERRMSG
           WHEN    "91"
      *********MOVE    "処理区分未設定"        TO  WRK-ERRMSG
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
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
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
               MOVE    "90"            TO  WRK-ERRCD
           END-IF
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
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
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
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
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
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9102-GENERICNAME-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  GENERICNAME-REC
           INITIALIZE                      GENERICNAME-REC
      *****MOVE    TNS-YAKKAKJNCD      TO  GENERIC-YAKKAKJNCD
           MOVE    SPA-COM-YAKKAKJNCD(IDX)
                                       TO  GENERIC-YAKKAKJNCD
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICNAME-REC
                   MOVE    ZERO                TO  FLG-GENERICNAME
               ELSE
                   INITIALIZE                  GENERICNAME-REC
                   MOVE    1                   TO  FLG-GENERICNAME
               END-IF
           ELSE
               INITIALIZE                      GENERICNAME-REC
               MOVE    1                   TO  FLG-GENERICNAME
           END-IF
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-GENERICNAME-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
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
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 910-DBFETCH-SEC
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
               PERFORM 910-DBFETCH-SEC
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
      *
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      **** PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
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
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
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
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
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
      *
