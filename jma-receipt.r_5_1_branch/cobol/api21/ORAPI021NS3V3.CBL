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
       PROGRAM-ID.         ORAPI021NS3V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院診療行為　診療行為登録　（入力一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/12/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.01    NMED-太田    19/09/15  API-PDF印刷対応
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
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-TENSU              PIC 9(01).
      *    03  FLG-HKNNUM              PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-NO                  PIC 9(01).
      *
           03  FLG-DELCHK              PIC 9(01).
           03  FLG-DELOK               PIC 9(01).
           03  FLG-DEL                 PIC 9(01).
           03  FLG-UPDATE              PIC 9(01).
           03  FLG-UPDEND              PIC 9(01).
      *
           03  FLG-SYOHOU-PRT          PIC 9(01).
           03  FLG-CHUUSHA-PRT         PIC 9(01).
           03  FLG-SIJISEN-PRT         PIC 9(01).
           03  FLG-NO2                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDW2                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
           03  IDD                 PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
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
     **    03  WRK-HKNCOMBI            PIC 9(04).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRMSG-02           PIC X(100).
      *
      *    内容エラー
           03  WRK-MD-ERRCD            PIC X(04).
           03  WRK-MD-ERRMSG           PIC X(100).
      *
           03  WRK-MCP-WINDOW          PIC X(64).
      *
           03  WRK-OPID                PIC X(16).
           03  WRK-ORCA-UID            PIC X(64).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
      *    保険組合せ内容
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
      *    訂正
           03  WRK-MOD-SRYKA           PIC X(02).
           03  WRK-MOD-HKNCOMBI        PIC X(04).
           03  WRK-SYORI-MODE          PIC X(03).
      *
           03  WRK-ERR-DAY             PIC 9(02).
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
           03  WRK-JYURRK-RENNUM       PIC 9(01).
           03  WRK-ZENMISYU            PIC S9(09).
           03  WRK-CHOKAKIN            PIC S9(09).
      *
      *    03  WRK-H-LINE              PIC X(03).
      *    03  WRK-H-IDX               PIC X(02).
      *
      *    03  WRK-KIDMSG              PIC X(100).
      *    03  WRK-MAE-KIDCD           PIC X(04).
      *
           03  WRK-DELNUM              PIC 9(02).
      *
           03  WRK-Z92             PIC --,---,--9.
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z97             PIC ZZZZ,ZZ9.
      *    処方箋見出し名称
       01  WRK-SIJISEN-AREA.
           03  WRK-SIJISEN-HEAD1-G     OCCURS   4.
               05  WRK-SIJISEN-HEAD1       PIC X(30).
               05  WRK-SIJISEN-PRT1        PIC 9(02).
               05  WRK-SIJISEN-OK1         PIC 9(01).
           03  WRK-SIJISEN-HEAD3-G     OCCURS   4.
               05  WRK-SIJISEN-HEAD3       PIC X(30).
               05  WRK-SIJISEN-PRT3        PIC 9(02).
               05  WRK-SIJISEN-OK3         PIC 9(01).
           03  WRK-SIJISEN-HEAD5-G     OCCURS   4.
               05  WRK-SIJISEN-HEAD5       PIC X(30).
               05  WRK-SIJISEN-PRT5        PIC 9(02).
               05  WRK-SIJISEN-OK5         PIC 9(01).
      *
      *受診履歴連番チェック用
       01  TBL-JYURRKCHK-AREA.
           03  TBL-JYURRK-OCC          OCCURS   31.
               05  TBL-JYU-REN             PIC 9(03).
      *
           03  WRK-KEY-SRYYMD.
               05  WRK-KEY-SRYYM           PIC X(06).
               05  WRK-KEY-SRYDD           PIC 9(02).
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
           03  WRK-BASE-YMD            PIC X(08).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
      *    03  WRK-SRYKA               PIC X(02).
      *    03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-PRINT-DRCD          PIC X(05).
           03  WRK-PRINT-DRCD-NAME     PIC X(80).
      *
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
      *
               05  WRK-SELECT-CODE         PIC X(04).
      *        訂正対象
               05  WRK-PARA-SRYKA          PIC X(02).
               05  WRK-PARA-HKNCOMBI       PIC X(04).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
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
      *    診療行為マスタワーク(パラメタ）
       01  PARA-WKSRYACT-AREA.
           02  PARA-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //PARA-WKSRY-//.
      *    附加情報を追加
           03  PARA-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //PARA-WKSRYP-//.
      *
      *
      *    請求点数名称
       01  TBL-HKNTEN-MEI-AREA.
           03  FILLER              PIC X(30)   VALUE   "初・再診料".
           03  FILLER              PIC X(30)   VALUE   "医学管理等".
           03  FILLER              PIC X(30)   VALUE   "在宅療養".
           03  FILLER              PIC X(30)   VALUE   "投薬".
           03  FILLER              PIC X(30)   VALUE   "注射".
           03  FILLER              PIC X(30)   VALUE   "処置".
           03  FILLER              PIC X(30)   VALUE   "手術".
           03  FILLER              PIC X(30)   VALUE   "麻酔".
           03  FILLER              PIC X(30)   VALUE   "検査".
           03  FILLER              PIC X(30)   VALUE   "画像診断".
           03  FILLER              PIC X(30)   VALUE   "リハビリ".
           03  FILLER              PIC X(30)   VALUE   "精神科専門".
           03  FILLER              PIC X(30)   VALUE   "放射線治療".
           03  FILLER              PIC X(30)   VALUE   "病理診断".
           03  FILLER              PIC X(30)   VALUE   "入院料".
           03  FILLER              PIC X(30)   VALUE   "療養担当手当".
       01  TBL-HKNTEN-MEI-RES      REDEFINES   TBL-HKNTEN-MEI-AREA.
           03  TBL-HKNTEN-MEI      PIC X(30)   OCCURS   16.
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    2019/09/19 Added
      *    オンライン帳票ＩＤ連絡
           COPY    "CPRPKEY.INC".
           COPY    "CPORCSRPLNK.INC".
      *    2019/09/19 Added
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCNYUIN.INC".
      *
      *    ＳＰＡ画面パラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
      *    COPY    "K02SCR-SPA"
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLAV3REQ3.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLAV3RES3.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    保険年齢・負担割合算定サブ
      ***  COPY    "CPORCSAGECHK.INC".
      *
      *    診療種別名テーブル
      *    COPY    "CPORCSSRYSYU.INC".
      *
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPI21V3REQ.INC".
      **01  XML-APIREQ.
      *     03  APIREQ-CONTEXT          PIC S9(9)   BINARY.
      *     03  APIREQ-OBJECT           PIC X(8).
      *     03  APIREQ-MODE             PIC S9(9)   BINARY.
      *     03  APIREQ-RECORDNAME       PIC X(128).
      ****  03  APIREQ-PATIENTINFOREQ   PIC X(1000000).
      *
      *H29.6
      *    一時ディレクトリ取得・削除サブ
           COPY    "CPORCSTEMPDIR.INC".
      *    クライアント印刷情報取得
           COPY    "CPORCSPRTCTRL.INC".
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *
      *    入院指示せんオーダ作成サブ
           COPY    "CPORCSORDERPRT.INC".
      *    入院処方箋発行サブ
           COPY    "CPORCHC501.INC".
      *    注射処方箋発行サブ
           COPY    "CPORCHC502.INC".
      *    入院指示箋発行サブ
           COPY    "CPORCHC503.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    発行方法
           COPY    "CPSK5007.INC".
      *    診療科情報
      *    COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    医療機関情報
      *    COPY    "CPSK1001.INC".
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
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    収納
      *01  SYUNOU-REC.
      *    COPY    "CPSYUNOU.INC".
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
           DISPLAY   "* medicalreqv33 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
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
               MOVE   404                  TO  APILST14-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "*********************"
           DISPLAY   "* medicalreqv33 end *"
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
           MOVE    SPACE               TO  XML-MEDICALAV3RES3
           INITIALIZE                      XML-MEDICALAV3RES3
           INITIALIZE                      WRK-APIV3DATA-AREA
           INITIALIZE                      WRK-SIJISEN-AREA
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
      *    CALL    "ORCSSRYSYU"        USING
      *                                MSYU-DATA-REC
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
           MOVE    MDCREQ-REQUEST-NUMBER   TO  MDCRES-REQUEST-NUMBER
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "04"
      *        確認処理
               MOVE    1                   TO  FLG-SYORI
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
           WHEN    "05"
      *        登録処理
               MOVE    2                   TO  FLG-SYORI
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      FLG-SYORI       NOT =   9
      *        患者情報内容編集
               PERFORM 20021-MDCRES-HEN-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
               IF      FLG-SYORI           =   9
      *            終了処理
                   PERFORM 300-SYORI-END-SEC
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
      *        確認処理
               PERFORM 2002-SYORI-01-SEC
           WHEN    2
      *        登録処理
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
           IF      APILST14-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALAV3REQ3
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalav3req3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST14-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalav3req3"    TO  APIREQ-RECORDNAME
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
           MOVE    "xml_medicalav3req3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST14-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalav3req3"    TO  APIREQ-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALAV3REQ3
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST14-BODY
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
                       MOVE    "登録正常終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRKBN
               END-IF
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
           IF      APILST14-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalav3res3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalav3res3"    TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALAV3RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalav3res3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalav3res3"    TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALAV3RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST14-BODY 
               MOVE  "application/xml" TO  APILST14-CONTENT-TYPE
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
           END-IF
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
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        排他チェック & ロック処理
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "90"            TO  WRK-ERRCD
                END-IF
           END-IF
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *
      *    訂正判定
           IF      WRK-ERRCD           =   SPACE
               IF     (MDCREQ-PATIENT-MODE =   "Modify")
                  OR  (WRK-PARA-MODE       =   "UPD" )
                   PERFORM 20011-MODIFY-CHK-SEC
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        ワーク診療行為検索
               PERFORM 200234-WKSRYACT-INIT-SEC
               IF      LNK-WKSRYACT-MAX    =   ZERO
                   MOVE    "08"            TO  WRK-ERRCD
               END-IF
           END-IF
      *    電子カルテＵＩＤチェック
           IF      WRK-ERRCD           =   SPACE
               IF      MDCREQ-KARTE-UID    NOT =   WRK-KARTE-UID
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    上記のエラー時は初回から
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    9                   TO  FLG-SYORI
           END-IF
      *
           IF      FLG-SYORI           =   2
      *        リクエスト＝０５のチェックチェック
               PERFORM 20011-INPUT-CHK05-SEC
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       20011-INPUT-CHK05-SEC      SECTION.
      *
           PERFORM 20010-INPUT-HEN05-SEC
      *
      *    システム管理検索
           PERFORM 20011-SYSKANRI5007-SEC
      *    印刷モード 2019/09/19 Added
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-PRINT-MODE-CHK-SEC
           END-IF
      *    処方せん印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-SYOHOU-CHK-SEC
           END-IF
      *    注射処方せん印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-CHUUSHA-CHK-SEC
           END-IF
      *    指示せん印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-SIJISEN-CHK-SEC
           END-IF
      *    発行医師
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-PRINT-DRCHK-SEC
           END-IF
           .
       20011-INPUT-CHK05-EXT.
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
           END-IF
      *
      *    入力UID
           MOVE    MDCREQ-ORCA-UID     TO  MDCRES-ORCA-UID
           MOVE    MDCREQ-KARTE-UID    TO  MDCRES-KARTE-UID
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
               DISPLAY "Accept Ymd =  " WRK-PERFORM-YMD
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
      *H28.1
      *****************************************************************
      *    更新対象チェック処理
      *****************************************************************
       20011-MODIFY-CHK-SEC         SECTION.
      *
           MOVE    WRK-PARA-SRYKA      TO  WRK-MOD-SRYKA
           MOVE    WRK-PARA-HKNCOMBI   TO  WRK-MOD-HKNCOMBI
      *
           IF     (MDCREQ-PATIENT-MODE =   "Modify")
              AND (WRK-PARA-MODE       =   "UPD"   )
              AND (SPA-SYORIFLG        =   1       )
               MOVE    "UPD"               TO  WRK-SYORI-MODE
               IF     (SPA-OLD-SRYKA       =   WRK-PARA-SRYKA)
                 AND  (SPA-OLD-HKNCOMBI    =   WRK-PARA-HKNCOMBI)
                   CONTINUE
               ELSE
                   MOVE    "21"                TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "28"                TO  WRK-ERRCD
           END-IF
           .
       20011-MODIFY-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    指示箋発行システム管理検索処理
      *****************************************************************
       20010-INPUT-HEN05-SEC         SECTION.
      *
      *    処方せん印刷
           MOVE    MDCREQ-PRINT-SYOHOU
                                       TO  MDCRES-PRINT-SYOHOU
      *    注射処方せん印刷
           MOVE    MDCREQ-PRINT-CHUUSHA
                                       TO  MDCRES-PRINT-CHUUSHA
      *    指示せん印刷
           MOVE    MDCREQ-PRINT-SIJISEN
                                       TO  MDCRES-PRINT-SIJISEN
      *    発行医師
           MOVE    MDCREQ-PRINT-DRCD   TO  MDCRES-PRINT-DRCD
           .
       20010-INPUT-HEN05-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    指示箋発行システム管理検索処理
      *****************************************************************
       20011-SYSKANRI5007-SEC         SECTION.
      *
           INITIALIZE                      WRK-SIJISEN-AREA
      *
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-5007-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-5007-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYS-5007-REC
           END-IF
           PERFORM
                   UNTIL       FLG-SYSKANRI    =   1
               MOVE    SYSKANRI-REC         TO  SYS-5007-REC
               EVALUATE    SYS-5007-KBNCD
                   WHEN    "01"
      *                入院処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO   WRK-SIJISEN-HEAD1(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO   WRK-SIJISEN-PRT1(IDX)
                       END-PERFORM
                   WHEN    "03"
      *                注射処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO   WRK-SIJISEN-HEAD3(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO   WRK-SIJISEN-PRT3(IDX)
                       END-PERFORM
                   WHEN    "05"
      *                指示箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO   WRK-SIJISEN-HEAD5(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO   WRK-SIJISEN-PRT5(IDX)
                       END-PERFORM
               END-EVALUATE
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20011-SYSKANRI5007-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷モードチェック 2019/09/19 Added
      *****************************************************************
       20013-PRINT-MODE-CHK-SEC         SECTION.
      *
      D    DISPLAY "MDCREQ-PRINT-MODE=" MDCREQ-PRINT-MODE
      *
           MOVE    FUNCTION UPPER-CASE(MDCREQ-PRINT-MODE)
                                       TO  MDCREQ-PRINT-MODE
           EVALUATE    MDCREQ-PRINT-MODE
           WHEN    SPACE
               CONTINUE
           WHEN    "PDF"
               INITIALIZE                  SRPLNK-AREA
               MOVE    "CHK_MONBLOB"   TO  SRPLNK-MODE
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
               IF    ( SRPLNK-RC       NOT =   ZERO )
                   MOVE    "54"        TO  WRK-ERRCD
               END-IF
           WHEN    OTHER
               MOVE    "55"            TO  WRK-ERRCD
           END-EVALUATE
           .
       20013-PRINT-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院処方せん印刷チェック処理
      *****************************************************************
       20013-SYOHOU-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYOHOU-PRT
           EVALUATE    MDCRES-PRINT-SYOHOU
           WHEN    "1"
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   4
                   IF      WRK-SIJISEN-PRT1(IDX)   =   ZERO
      *                印刷部数なし
                       MOVE    ZERO        TO  WRK-SIJISEN-OK1(IDX)
                   ELSE
                       MOVE    1           TO  WRK-SIJISEN-OK1(IDX)
                       MOVE    1           TO  FLG-SYOHOU-PRT
                   END-IF
               END-PERFORM
               IF      FLG-SYOHOU-PRT     =   1
                   MOVE    "発行する"      TO  MDCRES-PRINT-SYOHOU-MEI
               ELSE
                   MOVE    "0"             TO  MDCRES-PRINT-SYOHOU
                   MOVE    "発行しない"    TO  MDCRES-PRINT-SYOHOU-MEI
               END-IF
           WHEN    SPACE
               MOVE    ZERO            TO  FLG-SYOHOU-PRT
           WHEN    "0"
      *        発行なし
               MOVE    ZERO            TO  FLG-SYOHOU-PRT
               MOVE    "発行しない"    TO  MDCRES-PRINT-SYOHOU-MEI
           WHEN    OTHER
               MOVE    "51"            TO  WRK-ERRCD
           END-EVALUATE
           .
       20013-SYOHOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射処方せん印刷印刷チェック処理
      *****************************************************************
       20013-CHUUSHA-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHUUSHA-PRT
           EVALUATE    MDCRES-PRINT-CHUUSHA
           WHEN    "1"
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   4
                   IF      WRK-SIJISEN-PRT3(IDX)   =   ZERO
      *                印刷部数なし
                       MOVE    ZERO        TO  WRK-SIJISEN-OK3(IDX)
                   ELSE
                       MOVE    1           TO  WRK-SIJISEN-OK3(IDX)
                       MOVE    1           TO  FLG-CHUUSHA-PRT
                   END-IF
               END-PERFORM
               IF      FLG-CHUUSHA-PRT     =   1
                   MOVE    "発行する"      TO  MDCRES-PRINT-CHUUSHA-MEI
               ELSE
                   MOVE    "0"             TO  MDCRES-PRINT-CHUUSHA
                   MOVE    "発行しない"    TO  MDCRES-PRINT-CHUUSHA-MEI
               END-IF
           WHEN    SPACE
               MOVE    ZERO            TO  FLG-CHUUSHA-PRT
           WHEN    "0"
      *        発行なし
               MOVE    ZERO            TO  FLG-CHUUSHA-PRT
               MOVE    "発行しない"    TO  MDCRES-PRINT-CHUUSHA-MEI
           WHEN    OTHER
               MOVE    "52"            TO  WRK-ERRCD
           END-EVALUATE
           .
       20013-CHUUSHA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    指示せん印刷印刷チェック処理
      *****************************************************************
       20013-SIJISEN-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SIJISEN-PRT
           EVALUATE    MDCRES-PRINT-SIJISEN
           WHEN    "1"
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   4
                   IF      WRK-SIJISEN-PRT5(IDX)   =   ZERO
      *                印刷部数なし
                       MOVE    ZERO        TO  WRK-SIJISEN-OK5(IDX)
                   ELSE
                       MOVE    1           TO  WRK-SIJISEN-OK5(IDX)
                       MOVE    1           TO  FLG-SIJISEN-PRT
                   END-IF
               END-PERFORM
               IF      FLG-SIJISEN-PRT     =   1
                   MOVE    "発行する"      TO  MDCRES-PRINT-SIJISEN-MEI
               ELSE
                   MOVE    "0"             TO  MDCRES-PRINT-SIJISEN
                   MOVE    "発行しない"    TO  MDCRES-PRINT-SIJISEN-MEI
               END-IF
           WHEN    SPACE
               MOVE    ZERO            TO  FLG-SIJISEN-PRT
           WHEN    "0"
      *        発行なし
               MOVE    ZERO            TO  FLG-SIJISEN-PRT
               MOVE    "発行しない"    TO  MDCRES-PRINT-SIJISEN-MEI
           WHEN    OTHER
               MOVE    "53"            TO  WRK-ERRCD
           END-EVALUATE
           .
       20013-SIJISEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    発行医師チェック処理
      *****************************************************************
       20013-PRINT-DRCHK-SEC         SECTION.
      *
           IF      MDCRES-PRINT-DRCD   =   SPACE
               GO      TO      20013-PRINT-DRCHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PRINT-DRCD-NAME
      *    ドクタの存在チェック
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    MDCRES-PRINT-DRCD   TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-PRINT-DRCD-NAME
               MOVE    SYS-1010-NAME       TO  MDCRES-PRINT-DR-NAME
           ELSE
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           IF      MDCRES-PRINT-DRCD(1:1)  NOT =   "1"
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
           .
       20013-PRINT-DRCHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療内容チェック処理
      *****************************************************************
       2002-SYORI-01-SEC        SECTION.
      *
      *    レスポンス番号チェック
           IF      WRK-RESPONSE-NUMBER =   "04"
                                       OR  "05"
               CONTINUE
           ELSE
               MOVE    "09"            TO  WRK-ERRCD
               GO      TO      2002-SYORI-01-EXT
           END-IF
      *
      *    収納レコード編集
      ***  PERFORM 20020-SYUNOU-PARA-SEC
      *    剤削除処理
           IF      MDCREQ-MDC-MODE     =   "1"
               PERFORM 20021-ZAIDELETE-SEC
      *        ワーク診療行為更新
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 200291-WKSRYACT-UPD-SEC
               END-IF
           END-IF
      *
      *    診療行為内容編集
           PERFORM 20021-HENSYU-01-SEC
      *
      *    返却レスポンス番号
           IF     (WRK-ERRCD           =   SPACE )
      *        エラーなしは次へ
               MOVE    "05"            TO  MDCRES-RESPONSE-NUMBER
      *        データ保存
               PERFORM 20029-PARA-INS-SEC
           END-IF
      *
      *    API処理登録
           PERFORM 2008-APIPARAINS-SEC
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
      *    診療科
           MOVE    SPA-SRYKA           TO  MDCRES-APPOINT-DEP-CODE
           MOVE    SPA-SRYKAMEI        TO  MDCRES-APPOINT-DEP-NAME
      *   ドクター（主治医）
      *    MOVE    SPA-DRCD            TO  MDCRES-APPOINT-DR-CODE
      *    MOVE    SPA-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
      *    保険組合せ編集処理
           MOVE    SPA-HKNCOMBINUM     TO  MDCRES-COMBINATION-NUMBER
           IF      SPA-HKNCOMBINUM     =   "9999"
               MOVE    SPA-HKNCOMBIMEI
                                       TO  MDCRES-MAIN-INSURANCE-NAME
           ELSE
               INITIALIZE                      ORCSAPIHKNAREA
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
               MOVE    SPA-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCSAPIHKN-IN-HKNCOMBI
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
      *
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       20021-ZAIDELETE-SEC        SECTION.
      *
      *    削除区分解除
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL      (IDW     >   LNK-WKSRYACT-MAX)
               MOVE    ZERO            TO  LNK-WKSRYAPI-DELKBN(IDW)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30 )
                         OR   (MDCREQ-DEL-NUMBER (IDX) =   SPACE)
                         OR   (WRK-ERRCD       NOT =   SPACE )
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-DEL-NUMBER (IDX) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   2     )   OR
                      (SNUM-SYOSUU         >   0     )
                   MOVE    "20"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-DELNUM
               END-IF
      *
               MOVE    ZERO                TO  FLG-DELOK
               PERFORM VARYING     IDW     FROM    1   BY  1
                       UNTIL      (IDW     >   LNK-WKSRYACT-MAX)
                              OR  (FLG-DELOK   =   1  )
                              OR  (WRK-ERRCD   NOT =   SPACE )
                   IF      LNK-WKSRYAPI-RENNUM (IDW)    =   WRK-DELNUM
                       MOVE    1           TO  LNK-WKSRYAPI-DELKBN(IDW)
                       MOVE    1           TO  FLG-DELOK
                   END-IF
               END-PERFORM
               IF      FLG-DELOK       =   ZERO
                   MOVE    "20"            TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *
           .
       20021-ZAIDELETE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       20021-HENSYU-01-SEC        SECTION.
      *
      *    診療明細編集
           PERFORM 200211-MEDICAL-OUTHEN-SEC
      *
           .
       20021-HENSYU-01-EXT.
           EXIT.
      *****************************************************************
      *    明細編集（登録）処理
      *****************************************************************
       200211-MEDICAL-OUTHEN-SEC      SECTION.
      *
      *    対象の受診履歴連番
           PERFORM 200321-JURRK-INIT-SEC
           MOVE    ZERO                TO  WRK-ERR-DAY
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
      *
           INITIALIZE                  MDCRES-MEDICAL-INF-G
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   120   )
                       OR  (ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           =   SPACE )
      *
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
      *        算定日
               MOVE    ORAPISUB02-MDC-DAY-G (IDX)
                                           TO  MDCRES-MDC-DAY-G (IDX)
      *        剤区分（包括分）
               IF       ORAPISUB2-MDC-CODE (IDX)  =   "1"
                    MOVE    "True"         TO  MDCRES-INCLUSION-CLASS
                                                                (IDX)
               END-IF
               MOVE    ORAPISUB2-DEL-NUMBER (IDX)
                                           TO  MDCRES-DEL-NUMBER(IDX)
      *
      *        包括検査の検査数
               IF      ORAPISUB2-HKTKNSA-CNT (IDX) >   ZERO
                   MOVE    ORAPISUB2-HKTKNSA-CNT (IDX)
                                               TO  MDCRES-HKTKNSA-CNT
                                                                (IDX)
               END-IF
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
      *
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
      *        剤削除区分
               IF      ORAPISUB2-DEL-INF (IDX) =   "1"
                   MOVE    "Delete"            TO  MDCRES-DEL-INF (IDX)
               END-IF
      *
               IF     (ORAPISUB2-DEL-INF (IDX) =   SPACE )
      *            剤数チェック
                   PERFORM 2002112-ZAICONUNT-SEC
               END-IF
      *
           END-PERFORM
           IF     (ORAPISUB2-ERRCD NOT =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
      *******  MOVE    ORAPISUB2-ERRCD     TO  WRK-ERRCD
               MOVE    "13"                TO  WRK-ERRCD
               MOVE    ORAPISUB2-ERRMSG    TO  WRK-ERRMSG
           END-IF
      *
      *    剤上限数エラー
           IF     (WRK-ERRCD           =   SPACE )
             AND  (WRK-ERR-DAY     NOT =   ZERO  )
               MOVE    "31"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-ERRMSG
               STRING  WRK-ERR-DAY
                       "日の受診履歴が１３５剤以上になります。"
                       "剤を減らして入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
               END-STRING
           END-IF
           .
       200211-MEDICAL-OUTHEN-EXT.
           EXIT.
      *****************************************************************
      *    対象の受診履歴連番編集処理
      *****************************************************************
       200321-JURRK-INIT-SEC          SECTION.
      *
           INITIALIZE                      TBL-JYURRKCHK-AREA
      *    １月分の受診履歴の剤数
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-SRYYMD          TO  JYURRK-UPSRYYMD
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           IF      WRK-PARA-MODE       =   "UPD"
      *        訂正前の情報
               MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
               MOVE    SPA-OLD-HKNCOMBI    TO  JYURRK-HKNCOMBINUM
               MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
               MOVE    SPA-SRYYMD          TO  JYURRK-UPSRYYMD
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL      (FLG-JYURRK      =   1 )
               MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
               MOVE    WRK-KEY-SRYDD       TO  IDD 
               MOVE    ZERO                TO  FLG-NO
      *        同日再入院日の判定
               IF     (JYURRK-SRYYMD       =   SPA-K02N-NYUINYMD)
                 AND  (JYURRK-SRYYMD       =   SPA-SRYYMD   )
                 AND  (SPA-K02N-DOUKBN     =   "1"          )
      *            同日再入院分
                   IF      JYURRK-RENNUM       =   1
      *                連番１は対象外
                       MOVE    1               TO  FLG-NO
                   END-IF
               ELSE
                   IF      JYURRK-RENNUM   NOT =   1
      *                連番１以外は対象外
                       MOVE    1              TO  FLG-NO
                   END-IF
               END-IF
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   15       )
                              OR  (FLG-NO  =   1        )
                   IF      JYURRK-ZAINUM (IDX) NOT =   ZERO
                       ADD     1               TO  TBL-JYU-REN(IDD)
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200321-JURRK-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤数チェック処理
      *****************************************************************
       2002112-ZAICONUNT-SEC     SECTION.
      *
      *    訂正で変更なしは対象外
           IF     (WRK-PARA-MODE       =   "UPD" )
             AND  (SPA-SRYKA           =   SPA-OLD-SRYKA)
             AND  (SPA-HKNCOMBINUM     =   SPA-OLD-HKNCOMBI)
               GO      TO      2002112-ZAICONUNT-EXT
           END-IF
      *
      *    入院調剤料は対象外
           IF     (ORAPISUB2-MDC-CLASS-KBN (IDX)
                                       =   "24"
                                       OR  "26" )
               IF      ORAPISUB2-PRSCRPT-AUTOKBN(IDX 1 )  =   "3"
                   GO      TO      2002112-ZAICONUNT-EXT
               END-IF
           END-IF
      *    算定日集計
           PERFORM VARYING    IDD      FROM    1   BY  1
                   UNTIL     (IDD          >   31  )
                         OR  (WRK-ERR-DAY  NOT =   ZERO  )
               IF      ORAPISUB02-MDC-DAY (IDX IDD)    =   ZERO
                   CONTINUE
               ELSE
                   ADD     1               TO  TBL-JYU-REN (IDD)
                   IF      TBL-JYU-REN (IDD)  >   135
                       MOVE    IDD            TO  WRK-ERR-DAY
                   END-IF
               END-IF
           END-PERFORM
           .
       2002112-ZAICONUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ保存処理
      *****************************************************************
       20029-PARA-INS-SEC     SECTION.
      *
      *    収納データ更新
      *    PERFORM 200292-SYUNOU-UPD-SEC
      *    SPA保存
      **   PERFORM 2008-SPATMP-INS-SEC
           .
       20029-PARA-INS-EXT.
           EXIT.
      *****************************************************************
      *     パラメタファイル更新処理
      *****************************************************************
       200291-WKSRYACT-UPD-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           PERFORM 30012-PARA-DBDELETE-SEC
      *
           MOVE    ZERO                TO  IDW2
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   LNK-WKSRYACT-MAX)
      *        削除区分の登録
               ADD     1                   TO  IDW2
      *        TBL_PARAに保存
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "WKSRYACT"          TO  PARA-FILEMEI
               MOVE    IDW2                TO  PARA-EDANUM
               MOVE    LNK-WKSRYACT-REC(IDX)
                                               TO  PARA-DATA-REC
      *
                PERFORM 30010-PARA-DBINSERT-SEC
           END-PERFORM
           .
       200291-WKSRYACT-UPD-EXT.
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
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           .
       30012-PARA-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       2003-SYORI-02-SEC        SECTION.
      *
      *    レスポンス番号チェック
           IF      WRK-RESPONSE-NUMBER =   "05"
               CONTINUE
           ELSE
               MOVE    "09"                TO  WRK-ERRCD
               GO      TO      2003-SYORI-02-EXT
           END-IF
      *
      *    診療行為内容編集
           PERFORM 20021-HENSYU-01-SEC
      *
      *
           IF     (WRK-ERRCD       =   SPACE )
             AND  (SPA-ERRCD       =   SPACE )
      *        登録処理
               PERFORM 20032-TOUROKU-SYORI-SEC
      *
               MOVE    1            TO  FLG-UPDEND
           END-IF
      *
           IF     (SPA-ERRCD       =   SPACE )
             AND  (FLG-UPDEND      =   1     )
      *        当月入院調剤料登録内容
               PERFORM 20034-CHOZAI-HEN-SEC
           END-IF
      *
      *    帳票印刷指示
           IF     (SPA-ERRCD       =   SPACE )
               PERFORM 20034-PRINT-SYORI-SEC
           END-IF
      *
           IF     (SPA-ERRCD   NOT =   SPACE )
      *        エラー
               PERFORM 200329-ERRCD-MSG-SEC
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
      *    返却レスポンス番号
           IF     (SPA-ERRCD       =   SPACE )
      *        終了処理
               PERFORM 300-SYORI-END-SEC
      *
               MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     登録　処理
      *****************************************************************
       20032-TOUROKU-SYORI-SEC          SECTION.
      *
      *    ワーク診療行為更新
           PERFORM 20033-WKSRYACT-UPD-SEC
      *
      *    入院更新処理
           INITIALIZE                  ORCSCNYUINAREA
           MOVE    "1"                 TO  ORCSCNYUIN-KBN
      *    退院日
           MOVE    SPA-K02N-TAIINYMD   TO  ORCSCNYUIN-TAIINYMD
           CALL    "ORCSCNYUIN"        USING
                                       SPA-AREA
                                       ORCSCNYUINAREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)
                   MOVE    "W"                 TO  WRK-ERRKBN
                   MOVE    "30"                TO  WRK-ERRCD
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG-02
      *
                   IF      SPA-ERRMSG2(1:28)   =
                                      "警告！！保険組合せ有効期間外"
                       MOVE    SPACE           TO  WRK-ERRMSG-02
                       STRING  "警告！！"
                               "保険組合せ有効期間外の入院調剤料が"
                               "あります。"
                               "保険変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG-02
                       END-STRING
                   END-IF
               END-IF
           ELSE
      *        エラー
               PERFORM 200329-ERRCD-MSG-SEC
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
               IF      WRK-ERRMSG-02   NOT =   SPACE
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG-02
               END-IF
               IF      SPA-ERRCD(1:1)      =   "K"
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20032-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *     パラメタファイル更新処理
      *****************************************************************
       20033-WKSRYACT-UPD-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           PERFORM 30012-PARA-DBDELETE-SEC
      *
           MOVE    ZERO                TO  IDW2
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   LNK-WKSRYACT-MAX)
      *        削除分以外を保存
               IF      LNK-WKSRYAPI-DELKBN (IDX)   =   ZERO
      *
                   ADD     1                   TO  IDW2
      *            TBL_PARAに保存
                   INITIALIZE                      PARA-REC
                   MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
                   MOVE    "K02"               TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   MOVE    IDW2                TO  PARA-EDANUM
      *API         一般名記載用コメントのクリア
                   PERFORM VARYING    IDY      FROM    1   BY  1
                           UNTIL      IDY      >   5
                       IF     (LNK-WKSRY-SRYCD (IDX IDY)(1:1) =   "6")
                           MOVE    SPACE       TO  LNK-WKSRY-INPUTCOMENT
                                                              (IDX IDY)
                       END-IF
                   END-PERFORM
      *
                   MOVE    LNK-WKSRYACT-REC(IDX)
                                               TO  PARA-DATA-REC
      *
                    PERFORM 30010-PARA-DBINSERT-SEC
               END-IF
           END-PERFORM
           .
       200291-WKSRYACT-UPD-EXT.
           EXIT.
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
                   MOVE    "50"            TO  WRK-ERRCD
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
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
           IF      ORCSAPIHKN-THKNCOMBI (IDX)
                                   NOT =   ORCSAPIHKN-OT-HKNCOMBI
      *        保険組合せ番号なし
               MOVE    "19"                TO  WRK-ERRCD
               GO      TO      2002101-HKNHEN-TBL-EXT
           END-IF
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
           MOVE    ORCSAPIHKN-HOJOKBN-NAME (IDX)
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
           .
       2002101-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *     帳票印刷指示処理
      *****************************************************************
       20034-PRINT-SYORI-SEC       SECTION.
      *
           IF     (FLG-SYOHOU-PRT      =   ZERO )
             AND  (FLG-CHUUSHA-PRT     =   ZERO )
             AND  (FLG-SIJISEN-PRT     =   ZERO )
               GO      TO      20034-PRINT-SYORI-EXT
           END-IF
      *
      *
      *    入院指示せんオーダーデータ作成
           INITIALIZE                  ORCSORDERPRTAREA
           MOVE    "K08"               TO  LNK-ORDERPRT-PG
      *    パラメタより
           MOVE    "2"                 TO  LNK-ORDERPRT-KBN
      *
           IF      FLG-SYOHOU-PRT      =   1
               MOVE    1                   TO  LNK-ORDERPRT-SYOHOU
           END-IF
           IF      FLG-CHUUSHA-PRT     =   1
               MOVE    1                   TO  LNK-ORDERPRT-CHUSYA
           END-IF
           IF      FLG-SIJISEN-PRT     =   1
               MOVE    1                   TO  LNK-ORDERPRT-SIJISEN
           END-IF
           IF      MDCRES-PRINT-DRCD   =   SPACE
               MOVE    SPACE               TO  LNK-ORDERPRT-DRCD
           ELSE
               MOVE    MDCRES-PRINT-DRCD  TO  LNK-ORDERPRT-DRCD
           END-IF
      *    同日再入院区分
           MOVE    SPA-K02N-DOUKBN     TO  LNK-ORDERPRT-DOUJITSUKBN
           CALL    "ORCSORDERPRT"      USING
                                       ORCSORDERPRTAREA
                                       SPA-AREA
           IF      LNK-ORDERPRT-RCD    NOT =   ZERO
               MOVE    "1200"              TO  SPA-ERRCD
           END-IF
      *
      *
      *    一時ディレクトリ作成
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
      *    SPA-API-FLGに1を設定
           MOVE     1                  TO  SPA-API-FLG
      *    2019/09/19 Added
           EVALUATE    MDCREQ-PRINT-MODE
           WHEN    "PDF"
               MOVE    "2"             TO  SPA-PRTCONF
           END-EVALUATE
      *
      *    ORCSONPRTSETをCALL
           CALL    "ORCSONPRTSET"      USING
                                       SPA-AREA
      *
      *    ORCSPRTCTRLをCALL
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING
                                       SPA-AREA
                                       ORCSPRTCTRLAREA
                                       MCPAREA
      *
      *    入院処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (FLG-SYOHOU-PRT      =   1      )
               INITIALIZE              ORCHC501AREA
               MOVE    SPA-PTNUM           TO  ORCHC501-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC501-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC501-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC501-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC501-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC501-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC501-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      WRK-SIJISEN-OK1 (IDX)   =   1
                       MOVE    1               TO  ORCHC501-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "01"                TO  SPA-PRT-FLG
      *        2019/09/19 Added
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-SHOHOSEN-N    TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
               MOVE    SPA-NYUGAIKBN       TO  ORCSPRTNM-NYUGAIKBN
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
               CALL    "ORCSPRTNM"             USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYSKANRI-REC
               CALL    ORCSPRTNM-PRTPG         USING
                                               SPA-AREA
                                               ORCHC501AREA
           END-IF
      *    注射処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (FLG-CHUUSHA-PRT     =   1      )
               INITIALIZE                  ORCHC502AREA
               MOVE    SPA-PTNUM           TO  ORCHC502-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC502-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC502-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC502-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC502-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC502-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC502-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      WRK-SIJISEN-OK3 (IDX)   =   1
                       MOVE    1               TO  ORCHC502-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "02"                TO  SPA-PRT-FLG
      *        2019/09/19 Added
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-CHUSHASEN-N   TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "00000006"          TO  ORCSPRTNM-KBNCD
               MOVE    SPA-NYUGAIKBN       TO  ORCSPRTNM-NYUGAIKBN
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
               CALL    "ORCSPRTNM"             USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYSKANRI-REC
               CALL    ORCSPRTNM-PRTPG         USING
                                               SPA-AREA
                                               ORCHC502AREA
           END-IF
      *    指示せん印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (FLG-SIJISEN-PRT     =   1      )
      *        2019/09/19 Added
               INITIALIZE                  ORCHC503AREA
               MOVE    SPA-PTNUM           TO  ORCHC503-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC503-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC503-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC503-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC503-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC503-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC503-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      WRK-SIJISEN-OK5 (IDX)   =   1
                       MOVE    1               TO  ORCHC503-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "03"                TO  SPA-PRT-FLG
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-SHIJISEN-N    TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "00000007"          TO  ORCSPRTNM-KBNCD
               MOVE    SPA-NYUGAIKBN       TO  ORCSPRTNM-NYUGAIKBN
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
               CALL    "ORCSPRTNM"             USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYSKANRI-REC
               CALL    ORCSPRTNM-PRTPG         USING
                                               SPA-AREA
                                               ORCHC503AREA
           END-IF
      *    2019/09/19 Added
           IF    ( MDCREQ-PRINT-MODE   NOT =   SPACE )
            AND  ( SPA-PUTTYPE             =   "PRINTED" )
               MOVE    SPA-PRT-UUID    TO  MDCRES-DATA-ID
           END-IF
      *
      *    印刷ダミー処理
           MOVE    "99"                TO  SPA-PRT-FLG
           CALL    "ORCHCDUMMY"        USING   SPA-AREA
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
      *    一時ディレクトリ削除
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       20034-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     当月入院調剤料登録内容処理
      *****************************************************************
       20034-CHOZAI-HEN-SEC            SECTION.
      *
           INITIALIZE                  MDCRES-MEDICAL-CHOZAIINF-G
           MOVE    ZERO                TO  IDX
      *
      *    入院調剤料
           MOVE    "24"                TO  WRK-SRYKBN
           PERFORM 20034-CHOZAI-SYORI-SEC
      *    麻毒加算
           MOVE    "26"                TO  WRK-SRYKBN
           PERFORM 20034-CHOZAI-SYORI-SEC
           .
       20034-CHOZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料編集処理
      *****************************************************************
       20034-CHOZAI-SYORI-SEC           SECTION.
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM
               UNTIL       (FLG-SRYACCT        =   1 )  OR
                           (IDX                >=  10 )
               MOVE    ZERO                TO  FLG-NO2
      *        回数ゼロは対象外
               IF      ACCT-ZAIKAISU       =   ZERO
                   MOVE    1                   TO  FLG-NO2
               END-IF
               IF      (ACCT-SYUTEN1       =   ZERO )
      *           包括まとめ入力は対象外とする
                  OR   (ACCT-HKNCOMBI     =   "9999" )
      *           薬評は対象外とする
                  OR   (ACCT-ZAIKBN       =   1      )
                   MOVE    1                   TO  FLG-NO2
               END-IF
      *
               IF      FLG-NO2             =   ZERO
                   PERFORM 200341-CHOZAI-HENSYU-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20034-CHOZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料編集処理
      *****************************************************************
       200341-CHOZAI-HENSYU-SEC           SECTION.
      *
           ADD     1                   TO  IDX
           IF      IDX                 >   10
               GO      TO      200341-CHOZAI-HENSYU-EXT
           END-IF
           MOVE    ACCT-SRYKBN         TO  MDCRES-CHZ-CLASS-KBN
                                                                (IDX)
           MOVE    ACCT-SRYKA          TO  MDCRES-CHZ-DEP-CODE
                                                                (IDX)
           MOVE    ACCT-HKNCOMBI       TO  MDCRES-CHZ-COMBINATION
                                                                 (IDX)
           IF      ACCT-JIHOKBN        =   "1"
      *        包括分
               MOVE    "True"          TO  MDCRES-CHZ-INCLUSION-CLASS
                                                                 (IDX)
           END-IF
      *    算定日
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL      (IDD     >   31   )
               IF      ACCT-DAY (1 IDD)    =   ZERO
                   MOVE    "0"             TO  MDCRES-CHZ-DAY(IDX IDD)
               ELSE
                   MOVE    "1"             TO  MDCRES-CHZ-DAY(IDX IDD)
               END-IF
           END-PERFORM
      *    剤点数
           MOVE    ACCT-ZAITEN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-CHZ-POINT (IDX)
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           MOVE    SRY-SRYSYUKBN       TO  MDCRES-CHZ-CLASS
                                                      (IDX)
           EVALUATE    SRY-SRYSYUKBN
           WHEN    "240"
               MOVE    "入院調剤料"    TO  MDCRES-CHZ-CLASS-NAME
                                                      (IDX)
           WHEN    "260"
               MOVE    "麻毒加算"      TO  MDCRES-CHZ-CLASS-NAME
                                                      (IDX)
           END-EVALUATE
      *
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2            >   5   )
                              OR  (SRY-SRYCD(IDX2) =   SPACE )
                              OR  (IDY             >=  5   )
                   ADD     1               TO  IDY
                   MOVE    SRY-SRYCD (IDX2)
                                           TO  MDCRES-CHZ-CODE
                                                          (IDX IDY)
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   MOVE    TNS-NAME        TO  MDCRES-CHZ-NAME
                                                          (IDX IDY)
      *            自動算定区分
                   MOVE    SRY-AUTOKBN (IDX2)
                                           TO  MDCRES-CHZ-AUTOKBN
                                                          (IDX IDY)
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200341-CHOZAI-HENSYU-EXT.
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
      *
           IF     (WRK-ERRCD               =   SPACE )
      *        診療行為登録の処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "10"
                   MOVE    "10"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    SPA-API-FLGに1を設定
           MOVE    1              TO  SPA-API-FLG
      *
           .
       2008-SPATMP-SEL-EXT.
           EXIT.
      *****************************************************************
      *     API処理登録処理
      *****************************************************************
       2008-APIPARAINS-SEC            SECTION.
      *
      *    削除
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
      *    訂正分
           MOVE    WRK-MOD-SRYKA       TO  WRK-PARA-SRYKA
           MOVE    WRK-MOD-HKNCOMBI    TO  WRK-PARA-HKNCOMBI
      *
           MOVE    WRK-SYORI-MODE      TO  WRK-PARA-MODE
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
      *     SPATMPの登録処理
      *****************************************************************
       2008-SPATMP-INS-SEC            SECTION.
      *
      *    削除
           PERFORM 2009-SPATMP-DEL-SEC
      *
      *    TBL-SPATMP保存
      *    INITIALIZE                      SPATMP-REC
      *    MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *    MOVE    "AP21"              TO  SPATMP-GYOUMUID
      *    MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *    MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
      *    MOVE    1                   TO  SPATMP-EDANUM
      *
      *    MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    MOVE    SPA-AREA            TO  SPATMP-DATA-REC
      *    PERFORM 20081-SPATMP-INSERT-SEC
      *
      *
      *    TBL-SPATMP保存
      *    INITIALIZE                      SPATMP-REC
      *    MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *    MOVE    "AP21"              TO  SPATMP-GYOUMUID
      *    MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *    MOVE    "SPA-K03"           TO  SPATMP-FILEMEI
      *    MOVE    1                   TO  SPATMP-EDANUM
      *
      *    MOVE    SPA-K03             TO  SPATMP-DATA-REC
      **   PERFORM 20081-SPATMP-INSERT-SEC
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
           MOVE    "SPA-K03"           TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del1"              TO  MCP-PATHNAME
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
           IF      APIV3SUB01-ERRCD     NOT =   SPACE
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "81"                 TO  WRK-ERRCD
                   MOVE    APIV3SUB01-ERRMSG    TO  WRK-ERRMSG
               END-IF
           END-IF
           .
       300-SYORI-END-EXT.
           EXIT.
      *
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
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "API1"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "API1"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
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
       200329-ERRCD-MSG-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0002"
                   STRING  "患者情報が検索できませんでした。"
                           "患者情報テーブルを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "40"            TO  WRK-ERRCD
               WHEN    "0003"
                   STRING  "伝票番号が取得できませんでした。"
                           "システム管理テーブルを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "41"            TO  WRK-ERRCD
               WHEN    "0005"
                   STRING  "ＤＢ更新がエラーになりました。"
                           "ＳＥに連絡して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "42"            TO  WRK-ERRCD
               WHEN    "0105"
                   STRING  "この診療科の当日の受診が９回登録済です。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "43"            TO  WRK-ERRCD
               WHEN    "9000"
                   MOVE    "算定履歴の更新ができませんでした"
                                           TO  WRK-ERRMSG
                   MOVE    "44"            TO  WRK-ERRCD
               WHEN    "0051"
                   STRING  "中間データに不整合が発生しました。"
                           "強制終了後、"
                           "初回接続からおこなってください。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "45"            TO  WRK-ERRCD
               WHEN    "0052"
                   STRING  "外用薬剤の回数が１回以上になります。"
                           "同じ剤は回数をまとめて入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
                   MOVE    "46"            TO  WRK-ERRCD
      *
               WHEN    "0006"
                   STRING  "１日の受診履歴が１３５剤以上になります。"
                           "剤を減らして入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1200"
                   STRING  "印刷処理エラー。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
                   MOVE    "36"            TO  WRK-ERRCD
      *
               WHEN    OTHER
                   MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
                   MOVE    "37"            TO  WRK-ERRCD
           END-EVALUATE
           .
       200329-ERRCD-MSG-EXT.
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
      *        MOVE    "診療科が未設定です"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "03"
      *        MOVE    "ドクターが未設定です"
      *                                        TO  WRK-ERRMSG
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
                       "初回接続からおこなってください。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "09"
               STRING  "処理の順番が違います。"
                       "正しい順で処理を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
      **   WHEN    "12"
      *        MOVE     "基準日付が暦日ではありません。"
      **                                       TO  WRK-ERRMSG
           WHEN    "13"
               MOVE     SPACE                  TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "ドクターが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "19"
               STRING  "保険組合せ番号が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "20"
               MOVE     "剤削除番号が存在しません。"
                                               TO  WRK-ERRMSG
      **   WHEN    "26"
      *        STRING  "訂正処理の順番が違います。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      **       END-STRING
      *
           WHEN    "30"
      *        ORCSCNYUIN 警告
               CONTINUE
      *
           WHEN    "31"
               CONTINUE
           WHEN    "40"  THRU  "49"
      *        ORCSCNYUIN エラー
               CONTINUE
           WHEN    "50"
               STRING  "診療行為が４００行をオーバーします。"
                       "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "51"
               STRING  "入院処方せん印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "52"
               STRING  "注射せん印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "53"
               STRING  "指示せん印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "54"
               STRING  "印刷モードを有効にする場合、"
                                                       DELIMITED BY SIZE
                       "ミドルウェアを更新してください。"
                                                       DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "55"
               STRING  "無効な印刷モードです。"
                                                       DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
      *
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
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
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
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
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
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
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
      *
      *
      *****************************************************************
      *    パラメータテーブル検索処理
      *****************************************************************
       900-PARA-CALL-SEC                SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-PARA-CALL-EXT.
           EXIT.
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
