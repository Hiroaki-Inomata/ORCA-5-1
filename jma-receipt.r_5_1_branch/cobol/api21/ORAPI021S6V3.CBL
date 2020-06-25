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
       PROGRAM-ID.         ORAPI021S6V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　保険一括変更処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/02/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  18/12/XX  入院対応追加
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
      *    会計照会共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYCT               PIC 9(01).
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
           03  FLG-ARI                 PIC 9(01).
           03  FLG-HKNNO               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-J                   PIC 9(04).
           03  IDX-H                   PIC 9(04).
           03  IDX-R                   PIC 9(04).
      *
           03  IDX-ERR                 PIC 9(04).
           03  IDX-WAR                 PIC 9(04).
      *
           03  IDH                      PIC 9(04).
           03  IDY2                     PIC 9(04).
           03  IDX3                     PIC 9(04).
           03  IDK                      PIC 9(04).
      *
           03  IDX-TBL-MAX              PIC 9(04).
      *H30.6
           03  IDD                      PIC 9(02).
           03  IDXC                     PIC 9(04).
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
           03  WRK-OKMSG               PIC X(100).
      *    ワーニングテーブル
           03  WRK-KEICD-TBLG.
               05  WRK-KEICD-TBL       OCCURS   20.
                   07  WRK-KEICD           PIC X(03).
                   07  WRK-KEIMSG          PIC X(200).
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
      *    保険組合せ
           03  WRK-CHK-HKNCOMBI        PIC X(04).
           03  WRK-FTNRATE             PIC 9(03).
           03  WRK-DENPNUM             PIC 9(07).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-Z3                  PIC ZZ9.
      *
           03  WRK-HZN-ERRCD           PIC X(04).
      *H30.6
      *    入院調剤料対応
           03  WRK-CHK-YMD.
               05  WRK-CHK-YM          PIC X(06).
               05  WRK-CHK-DD          PIC 9(02).
           03  WRK-SRYKBN              PIC X(02).
           03  FLG-HKNARI              PIC 9(01).
      *
      *    保険組合せ
       01  TBL-HKNCOMBI-AREA.
           03  TBL-HKNCOBM-OCC         OCCURS   200.
               07  TBL-COMB-HKNCOMBI        PIC X(04).
      *        保険組合せ負担割合
               07  TBL-COMB-GAI-KFTNRATE    PIC  9(03).
               07  TBL-COMB-NYU-KFTNRATE    PIC  9(03).
      *        保険組合せ有効期間
               07  TBL-COMB-TEKSTYMD        PIC  X(10).
               07  TBL-COMB-TEKEDYMD        PIC  X(10).
               07  TBL-COMB-NAI-TEKSTYMD    PIC  X(08).
               07  TBL-COMB-NAI-TEKEDYMD    PIC  X(08).
      *        削除区分
               07  TBL-COMB-DELKBN          PIC  X(01).
      *        保険種別
               07  TBL-COMB-HKNNUM           PIC  X(03).
      *        保険種別名称
               07  TBL-COMB-HKNNUM-NAME      PIC  X(20).
      *        保険者番号
               07  TBL-COMB-HKNJANUM         PIC  X(08).
      *        補助区分
               07  TBL-COMB-HOJOKBN          PIC  X(01).
      *        補助区分名称
               07  TBL-COMB-HOJOKBN-NAME     PIC  X(30).
      *        本人家族区分
               07  TBL-COMB-HONKZKKBN        PIC  X(01).
      *        保険ID
               07  TBL-COMB-HKNID            PIC  9(10).
      *        非表示区分
               07  TBL-COMB-HYOJIKBN         PIC  X(01).
      *        公費種類・公費ＩＤ
               07  TBL-COMB-KOH-G.
                   09  TBL-COMB-KOH-OCC      OCCURS   4.
                       11  TBL-COMB-KOHNUM           PIC  X(03).
                       11  TBL-COMB-KOHNUM-NAME      PIC  X(20).
                       11  TBL-COMB-FTNJANUM         PIC  X(08).
                       11  TBL-COMB-JKYSNUM          PIC  X(20).
                       11  TBL-COMB-KOHID            PIC  9(10).
      *
      *H30.6
      *    入院調剤料対応
       01  TBL-JYURRK-CHOZAI-AREA.
           03  TBL-JYURRK-OCC          OCCURS    150.
               05  TBL-SRYYMD          PIC X(08).
               05  TBL-HKNCOMBI        PIC X(04).
      *H30.6
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
      *    診療年月
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
      *    診療科
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(20).
      *
      *    前保険組合せ
           03  WRK-MAE-HKNCOMBI        PIC X(04).
      *    後保険組合せ
           03  WRK-ATO-HKNCOMBI        PIC X(04).
      *
           03  WRK-START-DAY           PIC 9(02).
           03  WRK-END-DAY             PIC 9(02).
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
      *        診療年月
               05  WRK-PARA-SRYYM          PIC X(06).
      *        診療科
               05  WRK-PARA-SRYKA          PIC X(02).
      *H30.12
      *        入外区分
               05  WRK-PARA-NYUGAIKBN      PIC X(01).
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
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *    MCPAREA 保存
       01  WRK-MCPAREA             PIC X(5000).
      *
      *    オンライン画面
       01  WRK-SCRAREA.
           COPY    "ORCA24SCRAREA.INC".
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
      *    保険算定チェックサブ
           COPY    "CPORCSAGECHK.INC".
      *
      ***  全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
      *    COPY    "CPORCSKANACHK2.INC".
      *
      *    COPY    "CPORCXKANACONV.INC".
      *
      *    拡張文字対応
      ***  COPY    "CPORCSSTRING.INC".
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
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV3REQ6.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV3RES6.INC".
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
           COPY    "CPSK1005.INC".
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
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
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
           COPY    "MCPDATA3.INC".
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
           DISPLAY   "* medicalreq6v3 start*"
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
               MOVE   404                  TO  APILST09-HTTPSTATUS
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
           DISPLAY   "* medicalreq6v3 end  *"
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
           INITIALIZE                      XML-MEDICALV3RES6
           INITIALIZE                      WRK-APIV3DATA-AREA
           INITIALIZE                      SPA-J01KYOUTU
      *
           INITIALIZE                      WRK-SCRAREA
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
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
      *
      *    ＳＰＡ共通設定
           IF      WRK-ERRCD           =   SPACE
               PERFORM 1001-ORCSCOMMON-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ共通設定処理
      *****************************************************************
       1001-ORCSCOMMON-SEC           SECTION.
      *
      *    診療年月をシステム日付へ
           IF      MDCREQ-PERFORM-YM   NOT =   SPACE
               MOVE    MDCREQ-PERFORM-YM   TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD(1:6)       TO  WRK-PERFORM-YM
               MOVE    "01"                TO  WRK-PERFORM-DD
               PERFORM 20011-PERFORM-CHK-SEC
               IF      WRK-ERRCD           =   SPACE
      *            システム日付とする
                   MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
               END-IF
               MOVE    SPACE               TO  WRK-ERRCD
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
           MOVE    SPACE               TO  SPA-MOTOPG
           .
       1001-ORCSCOMMON-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
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
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "01"
      *        初期処理
               PERFORM 2001-SYORI-01-SEC
           WHEN    "02"
      *        確認・更新処理
               PERFORM 2002-SYORI-02-SEC
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
           MOVE    "API216V3"          TO  APIV3SUB01-API-FILEMEI
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
           IF      APILST09-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALV3REQ6
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req6" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST09-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req6"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_medicalv3req6" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST09-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req6"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALV3REQ6
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST09-BODY
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
                       MOVE    "更新処理終了"
                                               TO  WRK-OKMSG
                   WHEN    9
                       MOVE    "解除処理終了"
                                           TO  WRK-OKMSG
               END-EVALUATE
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    ZERO            TO  WRK-ERRCD
               IF      IDX-WAR         >   ZERO
                   MOVE    "W"             TO  WRK-ERRKBN
               END-IF
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
      *    ワーニングメッセージ
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   IDX-WAR  )
               MOVE    WRK-KEICD (IDK)     TO  MDCRES-WARNING
                                                              (IDK)
               MOVE    WRK-KEIMSG(IDK)     TO  MDCRES-WARNIG-MESG
                                                              (IDK)
           END-PERFORM
      *
           IF      APILST09-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res6" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res6"     TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALV3RES6
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res6" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res6"     TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALV3RES6
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    MDCRES-OBJECT       TO  APILST09-BODY
               MOVE    "patientmod/xml"    TO  APILST09-CONTENT-TYPE
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
           MOVE    "API216V3"          TO  APIPARA-FILEMEI
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
               IF      WRK-PARA-SYORIPG    NOT =   "06"
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
      *    診療年月チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    診療科チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-SRYKA-CHK-SEC
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
      *    診療年月
           MOVE    MDCREQ-PERFORM-YM     TO  MDCRES-PERFORM-YM
           MOVE    MDCREQ-PERFORM-YM     TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD(1:6)         TO  WRK-PERFORM-YM
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YM        =   SPACE
               MOVE    SMCNDATE-YMD(1:6)   TO  WRK-PERFORM-YM
           END-IF
      *    １日とする
           MOVE    "01"                TO  WRK-PERFORM-DD
      *    診療日付
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
      *    診療科
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                         TO  MDCRES-APPOINT-DEP-CODE
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                         TO  WRK-SRYKA
      *
           MOVE    MDCREQ-KARTE-UID      TO  MDCRES-KARTE-UID
           MOVE    MDCREQ-ORCA-UID       TO  MDCRES-ORCA-UID
      *    外来とする
      **** MOVE    "2"                   TO  SPA-NYUGAIKBN
      *H30.12
      *    入外区分
           IF      MDCREQ-OUTPATIENT-CLASS =   "I"
      *        入院
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           ELSE
      *        外来
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
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
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               MOVE    "13"                TO  WRK-ERRCD
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
      *    H28.4.1からとする
           IF      WRK-PERFORM-YMD     <   "20160401"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-PERFORM-CHK-EXT.
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
           PERFORM 900-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-SRYKA-NAME
           END-IF
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-APPOINT-DEP-NAME
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者・算定履歴情報取得処理
      *****************************************************************
       2001-SYORI-01-SEC        SECTION.
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
      *        保険組合せ情報取得など
               PERFORM 20014-HKNCOMBI-HEN-SEC
      *        受診履歴編集処理
               PERFORM 20013-JYURRK-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        会計照会初期設定
               PERFORM 20015-J02INIT-SEC
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
               IF      FLG-SYORIEND    =   1
                   MOVE    MDCREQ-ORCA-UID   TO  MDCRES-ORCA-UID
               END-IF
           ELSE
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
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
       2001-SYORI-01-EXT.
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
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
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
           MOVE    "J01KYOUTU"         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           MOVE    SPA-J01KYOUTU       TO  PARA-DATA-REC
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
           MOVE    "API216V3"          TO  APIPARA-FILEMEI
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
           MOVE    "06"                TO  WRK-PARA-SYORIPG
      *
           MOVE    WRK-PERFORM-YM      TO  WRK-PARA-SRYYM
           MOVE    WRK-SRYKA           TO  WRK-PARA-SRYKA
           MOVE    SPA-NYUGAIKBN       TO  WRK-PARA-NYUGAIKBN
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
      *    受診履歴編集処理
      ****************************************************************
       20013-JYURRK-SYORI-SEC           SECTION.
      *
           INITIALIZE                  TBL-JYURRK-CHOZAI-AREA
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-PERFORM-YMD     TO  JYURRK-SRYYMD
           MOVE    WRK-PERFORM-YM      TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key67"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key67"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-J
           PERFORM
               UNTIL      (FLG-JYURRK      =   1 )
                       OR (IDX-J           >=  150)
      *        保険組合せチェック
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
               PERFORM 200131-HKNCOMBI-CHK-SEC
      *
               IF      FLG-OK              =   1
                   PERFORM 200132-JYURRK-HEN-SEC
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key67"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key67"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *H30.12
      *    入院調剤料のみの保険組合せ判定
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 200132-NYUCHOZAI-SEC
           END-IF
      *
      *    対象なし
           IF      IDX-J               =   ZERO
               MOVE    "23"                TO  WRK-ERRCD
           ELSE
      *        変換対象保険組合わせあり
               IF      IDH                 =   ZERO
                   MOVE    "23"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20013-JYURRK-SYORI-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険組合わせ編集処理
      **************************************************************
       200131-HKNCOMBI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-TBL-MAX
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200  )
                         OR   (TBL-COMB-HKNCOMBI (IDX)
                                           =   SPACE  )
                         OR   (FLG-ARI     =   1      )
               IF      TBL-COMB-HKNCOMBI (IDX) =   WRK-HKNCOMBI
                   MOVE    1               TO  FLG-ARI
      *            労災・自賠責対象外
                   IF     (TBL-COMB-HKNNUM(IDX) =   "971"
                                                OR  "973")
                    OR    (TBL-COMB-KOHNUM (IDX 1)
                                                =   "970")
                       MOVE    ZERO                TO  FLG-OK
                   ELSE
                       MOVE    1                   TO  FLG-OK
                       MOVE    IDX                 TO  IDX-H
                   END-IF
               END-IF
               MOVE    IDX             TO  IDX-TBL-MAX
           END-PERFORM
           IF      FLG-ARI         =   ZERO
      *        当月分に存在しないとき、直接チェック
               PERFORM 200132-HKNCOMBI-CHK-SEC
      *        労災・自賠責対象外
               IF     (TBL-COMB-HKNNUM(IDX) =   "971"
                                            OR  "973")
                OR    (TBL-COMB-KOHNUM (IDX 1)
                                            =   "970")
                   MOVE    ZERO                TO  FLG-OK
               ELSE
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-IF
      *
           .
       200131-HKNCOMBI-CHK-EXT.
           EXIT.
      ****************************************************************
      *    保険組合わせ編集処理
      **************************************************************
       200132-HKNCOMBI-CHK-SEC           SECTION.
      *
      *    保険組合マスター編集
           INITIALIZE                  TBL-HKNCOMBI-AREA
           MOVE    SPACE               TO  HKNCOMBI-REC
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
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           ADD    1                    TO  IDX-TBL-MAX
           IF     IDX-TBL-MAX          >   200
               MOVE    200                 TO  IDX-TBL-MAX
           END-IF
           MOVE   IDX-TBL-MAX          TO  IDX
           IF     FLG-HKNCOMBI         =   1
      *        保険組合せなし
               MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
               MOVE    "？？？"            TO  COMB-SYU-TANSEIDONAME
           END-IF
      *
           MOVE    IDX                 TO  IDX-H
      *
           MOVE    COMB-HKNCOMBINUM   TO  TBL-COMB-HKNCOMBI (IDX)
      *    適用開始日
           MOVE    COMB-TEKSTYMD      TO  TBL-COMB-TEKSTYMD (IDX)
           MOVE    COMB-TEKEDYMD      TO  TBL-COMB-TEKEDYMD (IDX)
      *    負担割合
           MOVE    COMB-HKNCOMBINUM   TO  WRK-CHK-HKNCOMBI
           PERFORM 2004111-AGECHK-SEC
           IF      SPA-NYUGAIKBN      =   "2"
               MOVE    WRK-FTNRATE    TO  TBL-COMB-GAI-KFTNRATE (IDX)
           ELSE
               MOVE    WRK-FTNRATE    TO  TBL-COMB-NYU-KFTNRATE (IDX)
           END-IF
      *    削除
           IF      COMB-DELKBN         =   "1"
               MOVE    "1"                 TO  TBL-COMB-DELKBN (IDX)
           END-IF
      *    主保険
           MOVE    COMB-HKNID          TO  TBL-COMB-HKNID (IDX)
           MOVE    COMB-HKNNUM         TO  TBL-COMB-HKNNUM (IDX)
           MOVE    COMB-HONKZKKBN      TO  TBL-COMB-HONKZKKBN (IDX)
      *    補助区分
           MOVE    COMB-HOJOKBN        TO  TBL-COMB-HOJOKBN  (IDX)
      *    非表示区分
           MOVE    COMB-HYOJIKBN       TO  TBL-COMB-HYOJIKBN (IDX)
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  TBL-COMB-HKNNUM-NAME (IDX)
      *    保険者番号
           IF      COMB-HKNID      NOT =   ZERO
               MOVE    ZERO                TO  FLG-ARI
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL      (IDX3        >=  IDX  )
                             OR   (FLG-ARI     =   1    )
                   IF      COMB-HKNID      =   TBL-COMB-HKNID (IDX3)
                       MOVE    TBL-COMB-HKNJANUM(IDX3)
                                       TO  TBL-COMB-HKNJANUM(IDX)
                       MOVE    1       TO  FLG-ARI
                   END-IF
               END-PERFORM
               IF      FLG-ARI         =   ZERO
                   PERFORM 901-PTHKNINF-READ-SEC
                   MOVE    PTHKN-HKNJANUM  TO  TBL-COMB-HKNJANUM(IDX)
               END-IF
           END-IF
      *
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL      (IDY2    >   4   )
                           OR (COMB-KOHHKNNUM (IDY2)   =   SPACE)
      *        公費の種類
               MOVE    COMB-KOHHKNNUM (IDY2)
                                       TO  TBL-COMB-KOHNUM  (IDX IDY2)
      *        公費の種類名称
               MOVE    COMB-KOH-TANSEIDONAME(IDY2)
                                       TO  TBL-COMB-KOHNUM-NAME
                                                             (IDX IDY2)
               MOVE    COMB-KOHID (IDY2)
                                       TO  TBL-COMB-KOHID   (IDX IDY2)
      *        負担者番号、受給者番号
               PERFORM 901-PTKOHINF-READ-SEC
               MOVE    PTKOH-FTNJANUM  TO  TBL-COMB-FTNJANUM(IDX IDY2)
               MOVE    PTKOH-JKYSNUM   TO  TBL-COMB-JKYSNUM (IDX IDY2)
           END-PERFORM
      *
           .
       200132-HKNCOMBI-CHK-EXT.
           EXIT.
      ****************************************************************
      *    対象受診履歴一覧編集処理
      **************************************************************
       200132-JYURRK-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDX-J
      *
      *H30.12
           MOVE    JYURRK-SRYYMD       TO  TBL-SRYYMD (IDX-J)
           MOVE    JYURRK-HKNCOMBINUM  TO  TBL-HKNCOMBI (IDX-J)
      *
      *    診療日付
           MOVE    JYURRK-SRYYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-PERFORM-DATE (IDX-J)
      *    保険組合せ番号
           MOVE    JYURRK-HKNCOMBINUM
                                   TO  MDCRES-MED-COMBINATION-NUMBER
                                                             (IDX-J)
      *    連番
           MOVE    JYURRK-RENNUM   TO  MDCRES-MED-RENNUM  (IDX-J)
      *
      *    伝票番号
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    JYURRK-DENPNUM      TO  MDCRES-INVOICD-NUMBER
                                                             (IDX-J)
           END-IF
      *
      *H30.12
      *    保険組合せ編集
           MOVE    JYURRK-SRYYMD   TO  WRK-CHK-YMD
           PERFORM 2001321-HKNCOMBI-MEDHEN-SEC
           .
       200132-JYURRK-HEN-EXT.
           EXIT.
      ****************************************************************
      *    保険組合わせ編集処理
      **************************************************************
       2001321-HKNCOMBI-MEDHEN-SEC           SECTION.
      *
      *    保険組合せ負担割合
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    TBL-COMB-GAI-KFTNRATE (IDX-H)
                                   TO  MDCRES-MED-COMBINATION-RATE
                                                             (IDX-J)
           ELSE
               MOVE    TBL-COMB-NYU-KFTNRATE (IDX-H)
                                   TO  MDCRES-MED-COMBINATION-RATE
                                                             (IDX-J)
           END-IF
      *    保険組合せ適用開始日
           MOVE    TBL-COMB-TEKSTYMD (IDX-H)   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  MDCRES-MED-START-DATE
                                                             (IDX-J)
           MOVE    TBL-COMB-TEKEDYMD (IDX-H)   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  MDCRES-MED-END-DATE  (IDX-J)
      *    保険の種類
           MOVE    TBL-COMB-HKNNUM (IDX-H)
                                   TO  MDCRES-MED-INSURANCE-CLASS
                                                             (IDX-J)
           MOVE    TBL-COMB-HKNNUM-NAME (IDX-H)
                                   TO  MDCRES-MED-INSURANCE-NAME
                                                             (IDX-J)
      *    保険者番号
           MOVE    TBL-COMB-HKNJANUM (IDX-H)
                                   TO  MDCRES-MED-INSURANCE-NUMBER
                                                             (IDX-J)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    TBL-COMB-KOHNUM  (IDX-H IDY2)
                                       TO  MDCRES-MED-INSU-CLASS
                                                         (IDX-J IDY2)
      *        公費の種類名称
               MOVE    TBL-COMB-KOHNUM-NAME(IDX-H IDY2)
                                       TO  MDCRES-MED-INSU-NAME
                                                         (IDX-J IDY2)
      *        負担者番号
               MOVE    TBL-COMB-FTNJANUM(IDX-H IDY2)
                                       TO  MDCRES-MED-PROVIDER-NUMBER
                                                         (IDX-J IDY2)
      *        受給者番号
               MOVE    TBL-COMB-JKYSNUM (IDX-H IDY2)
                                       TO  MDCRES-MED-RECIPIENT-NUMBER
                                                         (IDX-J IDY2)
           END-PERFORM
      *    保険組合せ削除区分（１：削除、２：期間外）
           IF      TBL-COMB-DELKBN (IDX-H) =   "1"
               MOVE    "1"             TO  MDCRES-MED-COMBINATION-MODE
                                                             (IDX-J)
           ELSE
      *        期間外
               IF      (WRK-CHK-YMD        >=  TBL-COMB-TEKSTYMD(IDX-H))
                  AND  (WRK-CHK-YMD        <=  TBL-COMB-TEKEDYMD(IDX-H))
                   CONTINUE
               ELSE
                   MOVE    "2"         TO  MDCRES-MED-COMBINATION-MODE
                                                             (IDX-J)
               END-IF
           END-IF
      *    非表示区分
           IF      MDCRES-MED-COMBINATION-MODE (IDX-J) =   SPACE
               IF      SPA-NYUGAIKBN       =   "2"
      *            外来非表示
                   IF      TBL-COMB-HYOJIKBN (IDX-H)   =   "3"
                       MOVE    "3"     TO  MDCRES-MED-COMBINATION-MODE
                                                             (IDX-J)
                   END-IF
               ELSE
      *            入院非表示
                   IF      TBL-COMB-HYOJIKBN (IDX-H)   =   "2"
                       MOVE    "3"     TO  MDCRES-MED-COMBINATION-MODE
                                                             (IDX-J)
                   END-IF
               END-IF
           END-IF
           .
       2001321-HKNCOMBI-MEDHEN-EXT.
           EXIT.
      ****************************************************************
      *    保険組合わせ編集処理
      **************************************************************
       20014-HKNCOMBI-HEN-SEC           SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    年月、全部
           MOVE    "2"                 TO  ORCSAPIHKN-KBN2
           MOVE    "1"                 TO  ORCSAPIHKN-KBN3
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
      *
      *    出力領域保険情報編集
           MOVE    ZERO                TO  IDH
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
               MOVE    ZERO                TO  FLG-HKNNO
      *        労災・自賠責対象外
               IF     (ORCSAPIHKN-HKNNUM (IDX) =   "971"
                                               OR  "973")
                OR    (ORCSAPIHKN-KOHNUM (IDX 1)   =   "970")
                   MOVE    1               TO  FLG-HKNNO
               END-IF
      *        外来非表示は対象外
               IF     (SPA-NYUGAIKBN       =   "2"  )
                 AND  (ORCSAPIHKN-HYOJIKBN (IDX)   =   "3"  )
                   MOVE    1               TO  FLG-HKNNO
               END-IF
      *        入院非表示は対象外
               IF     (SPA-NYUGAIKBN       =   "1"  )
                 AND  (ORCSAPIHKN-HYOJIKBN (IDX)   =   "2"  )
                   MOVE    1               TO  FLG-HKNNO
               END-IF
               IF      FLG-HKNNO           =   ZERO
      *            変更可保険組合せ編集
                   PERFORM 200141-HKNHEN-TBL-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-SYORI           =   2
               GO   TO  20014-HKNCOMBI-HEN-EXT
           END-IF
      *
           INITIALIZE                  TBL-HKNCOMBI-AREA
      *    テーブル編集処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
               PERFORM 200142-TBLCOMB-HEN-SEC
           END-PERFORM
      *
      *
           .
       20014-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    チェック用テーブル編集処理
      **************************************************************
       200142-TBLCOMB-HEN-SEC           SECTION.
      *
           MOVE    ORCSAPIHKN-THKNCOMBI (IDX)
                                       TO  TBL-COMB-HKNCOMBI (IDX)
      *    保険組合せ有効期間
           MOVE    ORCSAPIHKN-COMBN-TEKSTYMD(IDX)
                                       TO  TBL-COMB-TEKSTYMD  (IDX)
           MOVE    ORCSAPIHKN-COMBN-TEKEDYMD(IDX)
                                       TO  TBL-COMB-TEKEDYMD  (IDX)
      *    負担割合
           MOVE    ORCSAPIHKN-COMB-RATEG(IDX)
                                       TO  TBL-COMB-GAI-KFTNRATE
                                                              (IDX)
           MOVE    ORCSAPIHKN-COMB-RATEN(IDX)
                                       TO  TBL-COMB-NYU-KFTNRATE
                                                              (IDX)
      *    保険種別
           MOVE    ORCSAPIHKN-HKNNUM  (IDX)
                                       TO  TBL-COMB-HKNNUM    (IDX)
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                       TO  TBL-COMB-HKNNUM-NAME(IDX)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                       TO  TBL-COMB-HKNJANUM  (IDX)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                       TO  TBL-COMB-HOJOKBN(IDX)
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                       TO  TBL-COMB-HOJOKBN-NAME
                                                               (IDX)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN (IDX)
                                       TO  TBL-COMB-HONKZKKBN(IDX)
      *    保険ID
           MOVE    ORCSAPIHKN-NAI-HKNID (IDX)
                                       TO  TBL-COMB-HKNID(IDX)
      *    非表示区分
           MOVE    ORCSAPIHKN-HYOJIKBN (IDX)
                                       TO  TBL-COMB-HYOJIKBN(IDX)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  TBL-COMB-KOHNUM
                                                             (IDX IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  TBL-COMB-KOHNUM-NAME
                                                             (IDX IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  TBL-COMB-FTNJANUM
                                                             (IDX IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  TBL-COMB-JKYSNUM
                                                             (IDX IDY2)
               MOVE    ORCSAPIHKN-NAI-KOHID(IDX IDY2)
                                       TO  TBL-COMB-KOHID
                                                             (IDX IDY2)
           END-PERFORM
      *
           .
       200142-TBLCOMB-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    変更可保険組合せ一覧編集処理
      **************************************************************
       200141-HKNHEN-TBL-SEC           SECTION.
      *
           ADD     1                   TO  IDH
           MOVE    ORCSAPIHKN-THKNCOMBI (IDX)
                                       TO  MDCRES-COMBINATION-NUMBER
                                                               (IDH)
      *    保険組合せ負担割合
           MOVE    ORCSAPIHKN-THKNCOMBI (IDX)  TO  WRK-CHK-HKNCOMBI
           PERFORM 2004111-AGECHK-SEC
           MOVE    WRK-FTNRATE         TO  MDCRES-COMBINATION-RATE
                                                               (IDH)
      *    パラメタへ再編集
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    WRK-FTNRATE     TO  ORCSAPIHKN-COMB-RATEG
                                                               (IDH)
           ELSE
               MOVE    WRK-FTNRATE     TO  ORCSAPIHKN-COMB-RATEN
                                                               (IDH)
           END-IF
      *    保険組合せ有効期間
           MOVE    ORCSAPIHKN-COMB-TEKSTYMD(IDX)
                                       TO  MDCRES-COMB-START-DATE
                                                               (IDH)
           MOVE    ORCSAPIHKN-COMB-TEKEDYMD(IDX)
                                       TO  MDCRES-COMB-END-DATE
                                                               (IDH)
      *    保険種別
           MOVE    ORCSAPIHKN-HKNNUM  (IDX)
                                       TO  MDCRES-MAIN-INSURANCE-CLASS
                                                               (IDH)
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                       TO  MDCRES-MAIN-INSURANCE-NAME
                                                               (IDH)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                       TO  MDCRES-MAIN-INSURANCE-NUMBER
                                                               (IDH)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                       TO  MDCRES-MAIN-MARK (IDH)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                       TO  MDCRES-MAIN-NUMBER(IDH)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                       TO  MDCRES-MAIN-CONTINUATION
                                                               (IDH)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                       TO  MDCRES-MAIN-ASSISTANCE(IDH)
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                       TO  MDCRES-MAIN-ASSISTANCE-NAME
                                                               (IDH)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                       TO  MDCRES-MAIN-FAMILY(IDH)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                       TO  MDCRES-MAIN-START-DATE(IDH)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                       TO  MDCRES-MAIN-END-DATE(IDH)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  MDCRES-INSURANCE-CLASS
                                                             (IDH IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  MDCRES-INSURANCE-NAME
                                                             (IDH IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  MDCRES-PROVIDER-NUMBER
                                                             (IDH IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  MDCRES-RECIPIENT-NUMBER
                                                             (IDH IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-RATE-OUTPATIENT
                                                             (IDH IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-MONEY-OUTPATIENT
                                                             (IDH IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-START-DATE
                                                             (IDH IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-END-DATE
                                                             (IDH IDY2)
      *        入院−負担率（割）
      *        MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
      *                                TO  MDCRES-RATE-ADMISSION
      *                                                      (IDY2)
      *        入院−固定額
      *        MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
      *                                TO  MDCRES-MONEY-ADMISSION
      *                                                      (IDY2)
           END-PERFORM
           .
       200141-HKNHEN-TBL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せ負担割合編集処理
      *****************************************************************
       2004111-AGECHK-SEC              SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    WRK-CHK-HKNCOMBI    TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC           =   ZERO
               MOVE    AGECHK-FTNRATE      TO  WRK-FTNRATE
           END-IF
           .
       2004111-AGECHK-EXT.
           EXIT.
      *
      *H30.12
      *****************************************************************
      *    入院調剤料のみの保険組合せ判定処理
      *****************************************************************
       200132-NYUCHOZAI-SEC     SECTION.
      *
      *    入院調剤料
           MOVE    "24"                TO  WRK-SRYKBN
           IF      IDX-J               <   150
               PERFORM 200132-CHOZAI-SYORI-SEC
           END-IF
      *    麻毒加算
           MOVE    "26"                TO  WRK-SRYKBN
           IF      IDX-J               <   150
               PERFORM 200132-CHOZAI-SYORI-SEC
           END-IF
           .
       200132-NYUCHOZAI-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料編集処理
      *****************************************************************
       200132-CHOZAI-SYORI-SEC           SECTION.
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    WRK-SRYKA           TO  ACCT-SRYKA
           MOVE    WRK-PERFORM-YM      TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT    =   1  )
                      OR   (IDX-J          >=  150 )
      *        包括保険は対象外
               IF     (ACCT-HKNCOMBI       =   "9999" )
                   CONTINUE
               ELSE
                   MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI
                   PERFORM 200131-HKNCOMBI-CHK-SEC
                   IF      FLG-OK          =   1
                       PERFORM 2001321-CHOZAI-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200132-CHOZAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象入院調剤料編集処理
      *****************************************************************
       2001321-CHOZAI-HENSYU-SEC     SECTION.
      *
      *    算定日チェック
           MOVE    ACCT-SRYYM          TO  WRK-CHK-YM
           MOVE    ZERO                TO  WRK-CHK-DD
      *    入院調剤料算定日
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               IF      ACCT-DAY (1 IDD)    NOT =   ZERO
                   MOVE    IDD             TO  WRK-CHK-DD
                   PERFORM 2001321-CHOZAI-SYORI-SEC
               END-IF
           END-PERFORM
           .
       2001321-CHOZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象入院調剤料編集処理
      *****************************************************************
       2001321-CHOZAI-SYORI-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNARI
           PERFORM VARYING     IDXC    FROM    1   BY  1
                   UNTIL      (IDXC    >   150 )
                         OR   (FLG-HKNARI  =   1   )
                         OR   (TBL-SRYYMD (IDXC)   =   SPACE )
              IF     (WRK-CHK-YMD      =   TBL-SRYYMD (IDXC))
               AND   (WRK-HKNCOMBI     =   TBL-HKNCOMBI(IDXC))
                   MOVE    1               TO  FLG-HKNARI
              END-IF
           END-PERFORM
           IF      FLG-HKNARI          =   1
               GO  TO  2001321-CHOZAI-SYORI-EXT
           END-IF
      *
           ADD     1                   TO  IDX-J
           IF      IDX-J               >   150
               GO  TO  2001321-CHOZAI-SYORI-EXT
           END-IF
      *    テーブル編集
           MOVE    WRK-CHK-YMD         TO  TBL-SRYYMD (IDX-J)
           MOVE    WRK-HKNCOMBI        TO  TBL-HKNCOMBI(IDX-J)
      *
      *    入院調剤料のみ対応
           MOVE    "Ad_Only"           TO  MDCRES-CHOZAI-ONLY(IDX-J)
      *    算定日付
           MOVE    WRK-CHK-YMD         TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-PERFORM-DATE (IDX-J)
      *    保険組合せ番号
           MOVE    WRK-HKNCOMBI
                                   TO  MDCRES-MED-COMBINATION-NUMBER
                                                             (IDX-J)
      *    保険組合せ編集
           PERFORM 2001321-HKNCOMBI-MEDHEN-SEC
      *
           .
       2001321-CHOZAI-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    会計照会初期設定処理
      *****************************************************************
       20015-J02INIT-SEC     SECTION.
      *
           INITIALIZE                  SPA-J01KYOUTU
           INITIALIZE                  SPA-J02
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    WRK-SRYKA           TO  SPA-J01-SRYKA
           MOVE    WRK-PERFORM-YM      TO  SPA-NAI-SRYYM
           MOVE    PTINF-BIRTHDAY      TO  SPA-GMN-BIRTHDAY
      *    患者の入金方法
           IF      PTINF-NYUKIN-HOHO   =   SPACE
               MOVE    "01"                TO  SPA-NYUKIN-HOHO
           ELSE
               MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NYUKIN-HOHO
           END-IF
      *
           MOVE    MCPAREA             TO  WRK-MCPAREA
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "INIT"              TO  MCP-EVENT
      *
           CALL    "ORCGJ02"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *
           MOVE    WRK-MCPAREA         TO  MCPAREA
      *
           INITIALIZE                  SPA-J021-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "INIT"              TO  MCP-EVENT
      *
           CALL    "ORCGJ021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           MOVE    WRK-MCPAREA         TO  MCPAREA
           .
       20015-J02INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新確認処理
      *****************************************************************
       2002-SYORI-02-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX-ERR
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
      *    ＡＰＩＰＡＲＡの内容チェック
           IF      WRK-ERRCD           =   SPACE
               IF     (WRK-PARA-SRYYM      =   WRK-PERFORM-YM)
                 AND  (WRK-PARA-SRYKA      =   WRK-SRYKA    )
      *H30.12
                 AND  (WRK-PARA-NYUGAIKBN  =   SPA-NYUGAIKBN)
                   CONTINUE
               ELSE
                   MOVE    "24"            TO  WRK-ERRCD
               END-IF
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
      *            検索へ
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
           MOVE    ZERO                TO  IDX-WAR
           INITIALIZE                  WRK-KEICD-TBLG
      *    基本チェック処理
           PERFORM 200212-INPUT-CHK01-SEC
      *
      *    関連チェック
           IF      WRK-ERRCD           =   SPACE
              PERFORM 200213-INPUT-CHK02-SEC
           END-IF
      *
           .
       20021-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信内容編集処理
      *****************************************************************
       200211-MDCREQ-HEN02-SEC         SECTION.
      *
           MOVE    MDCREQ-IN-COMBINATION-NUMBER
                                       TO  MDCRES-IN-COMBINATION-NUMBER
           MOVE    MDCREQ-OT-COMBINATION-NUMBER
                                       TO  MDCRES-OT-COMBINATION-NUMBER
      *
           MOVE    MDCREQ-START-DAY    TO  MDCRES-START-DAY
           MOVE    MDCREQ-END-DAY      TO  MDCRES-END-DAY
      *    収納一括更新あり
           IF     (SPA-NYUGAIKBN       =   "2"     )
               MOVE    MDCREQ-INCOME-CHANGE    TO  MDCRES-INCOME-CHANGE
           END-IF
      *
      *    保険組合せ情報取得など
      *??? PERFORM 20014-HKNCOMBI-HEN-SEC
      *
           .
       200211-MDCREQ-HEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200212-INPUT-CHK01-SEC         SECTION.
      *
      *
      *    保険組合せ番号数値
           INITIALIZE                      ORCSNUMAREA
           MOVE     MDCREQ-IN-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "25"            TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-MAE-HKNCOMBI
           END-IF
           IF      WRK-MAE-HKNCOMBI    =   SPACE
                                       OR  ZERO
               MOVE    "25"            TO  WRK-ERRCD
           END-IF
      *
      *    保険組合せ番号数値
           INITIALIZE                      ORCSNUMAREA
           MOVE     MDCREQ-OT-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "26"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-ATO-HKNCOMBI
           END-IF
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-ATO-HKNCOMBI    =   SPACE
                                           OR  ZERO
                   MOVE    "26"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    変更開始日チェック
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-START-DAY    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "27"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  WRK-START-DAY
           END-IF
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-START-DAY       =   ZERO
                   MOVE    "27"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    変更開始日チェック
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-END-DAY      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "28"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  WRK-END-DAY
           END-IF
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-END-DAY         =   ZERO
                   MOVE    "28"            TO  WRK-ERRCD
               END-IF
           END-IF
      *    収納更新区分
           IF     (SPA-NYUGAIKBN       =   "2"     )
             IF      MDCREQ-INCOME-CHANGE    =   "Yes"
                                             OR  "No"
               CONTINUE
             ELSE
               MOVE    "29"                TO  WRK-ERRCD
             END-IF
           END-IF
           .
       200212-INPUT-CHK01-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       200213-INPUT-CHK02-SEC         SECTION.
      *
           MOVE    WRK-MAE-HKNCOMBI
                                       TO  MDCRES-IN-COMBINATION-NUMBER
           MOVE    WRK-ATO-HKNCOMBI
                                       TO  MDCRES-OT-COMBINATION-NUMBER
           MOVE    WRK-MAE-HKNCOMBI    TO  SPA-J021-MAE-HKNCOMBI
           MOVE    WRK-ATO-HKNCOMBI    TO  SPA-J021-ATO-HKNCOMBI
           MOVE    WRK-START-DAY       TO  SPA-J021-DAY1
           MOVE    WRK-END-DAY         TO  SPA-J021-DAY2
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    MCPAREA             TO  WRK-MCPAREA
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "CHK"               TO  MCP-EVENT
      *
           CALL    "ORCGJ021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           MOVE    WRK-MCPAREA         TO  MCPAREA
      *
           IF      SPA-ERRCD       NOT =   SPACE
      *        エラーコード編集
               PERFORM 8903-ONLERRCD-SEC
           END-IF
      *
           .
       200213-INPUT-CHK02-EXT.
           EXIT.
      *****************************************************************
      *    更新処理
      *****************************************************************
       20022-KOUSIN-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    MCPAREA             TO  WRK-MCPAREA
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "UPD"               TO  MCP-EVENT
      *
           CALL    "ORCGJ021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           MOVE    WRK-MCPAREA     TO  MCPAREA
      *
      *    正常終了
           IF      SPA-ERRCD       NOT =   SPACE
      *        エラーコード編集
               PERFORM 8903-ONLERRCD-SEC
           END-IF
      *
      *    収納一括更新
           IF     (SPA-NYUGAIKBN       =   "2" )
              AND (WRK-ERRCD           =   SPACE )
               IF      MDCREQ-INCOME-CHANGE    =   "Yes"
      *            収納一括更新
                   PERFORM 200221-SYUNOU-SAIKEISAN-SEC
                   IF      WRK-ERRCD           =   SPACE
      *                更新一覧表示
                       PERFORM 20223-KOUSIN-HENSYU-SEC
                   END-IF
               ELSE
      *            収納一括変更なし
                   MOVE    "0010"              TO  SPA-ERRCD
      *            エラーコード編集
                   PERFORM 8903-ONLERRCD-SEC
               END-IF
           END-IF
           .
       20022-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    収納一括更新処理
      *****************************************************************
       200221-SYUNOU-SAIKEISAN-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    MCPAREA             TO  WRK-MCPAREA
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "UPD"               TO  MCP-EVENT
      *
           CALL    "ORCGJ02"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    WRK-MCPAREA     TO  MCPAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           IF      SPA-J025-ERRMSG     NOT =   SPACE
      *        一括変更不可あり
               ADD     1                   TO  IDX-WAR
               MOVE    SPA-J025-ERRMSG     TO  WRK-KEIMSG(IDX-WAR)
               MOVE    "W01"               TO  WRK-KEICD(IDX-WAR)
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
      *        エラーコード編集
               PERFORM 8903-ONLJ02ERRCD-SEC
           END-IF
           .
       200221-SYUNOU-SAIKEISAN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    収納更新一覧編集処理
      *****************************************************************
       20223-KOUSIN-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-J025-MAX
               MOVE    SPA-J025-DENPNUM (IDX)
                                       TO  MDCRES-CD-INVOICD-NUMBER
                                                               (IDX)
      *        収納データ検索
               MOVE    SPA-J025-DENPNUM (IDX)  TO  WRK-DENPNUM
               PERFORM 910-SYUNOU-KEY-READ-SEC
      *        診療日付
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  MDCRES-CD-PERFORM-DATE (IDX)
      *
               MOVE    SYU-HKNCOMBINUM
                                       TO  MDCRES-CD-COMBINATION-NUMBER
                                                               (IDX)
      *        収納負担割合
               MOVE    SYU-PTFTNRATE   TO  WRK-Z3
               MOVE    WRK-Z3          TO  MDCRES-CD-RATE
                                                               (IDX)
      *        伝票発行日
               MOVE    SYU-DENPPRTYMD  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  MDCRES-CD-DENPPRTYMD (IDX)
               IF      MDCREQ-INCOME-CHANGE    =   "Yes"
      *            変更前請求額
                   MOVE    SPA-J025-MAE-SKYMONEY (IDX)
                                           TO  MDCRES-MAE-AC-MONEY
                                                             (IDX)
      *            変更後請求額
                   MOVE    SPA-J025-ATO-SKYMONEY (IDX)
                                           TO  MDCRES-ATO-AC-MONEY
                                                             (IDX)
               ELSE
                   MOVE    SPACE           TO  MDCRES-MAE-AC-MONEY
                                                             (IDX)
                   MOVE    SPACE           TO  MDCRES-ATO-AC-MONEY
                                                             (IDX)
               END-IF
      *        一括変更不可
               IF      SPA-J025-KBNMEI (IDX)   NOT =   SPACE
                   MOVE    SPA-J025-KBNMEI (IDX)
                                           TO  MDCRES-DC-ERR (IDX)
               END-IF
           END-PERFORM
           .
       200221-SYUNOU-SAIKEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーコード編集処理
      *****************************************************************
       8903-ONLERRCD-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           MOVE    SPACE               TO  WRK-ERRCD
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "変更前の保険組合せがありません。"
                                           TO  WRK-ERRMSG
                   MOVE    "30"            TO  WRK-ERRCD
               WHEN    "0002"
                   MOVE    "この保険組合せでは受診はありません"
                                           TO  WRK-ERRMSG
                   MOVE    "31"            TO  WRK-ERRCD
               WHEN    "0003"
                   MOVE    "変更後の保険組合せがありません"
                                           TO  WRK-ERRMSG
                   MOVE    "32"            TO  WRK-ERRCD
      **       WHEN    "0005"
      *            MOVE    "変更前保険組合せを入力して下さい。"
      *                                    TO  WRK-ERRMSG
      *            MOVE    "34"            TO  WRK-ERRCD
      *        WHEN    "0006"
      **           MOVE    "変更後保険組合せを入力して下さい。"
      *                                    TO  WRK-ERRMSG
      **           MOVE    "35"            TO  WRK-ERRCD
               WHEN    "0007"
                   MOVE    "変更前と変更後が同じです。変更して下さい"
                                           TO  WRK-ERRMSG
                   MOVE    "33"            TO  WRK-ERRCD
               WHEN    "0008"
                   STRING  "変更後の保険組合せは対象外です。"
                           "変更して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "34"            TO  WRK-ERRCD
               WHEN    "0009"
                   STRING  "変更後の保険組合せは受診日に対象では"
                           "ありません。変更して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "35"            TO  WRK-ERRCD
               WHEN    "0012"
                   STRING  "変更後の保険組合せは削除分です。"
                           "変更して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "36"            TO  WRK-ERRCD
               WHEN    "0022"
                   STRING  "変更後の保険組合せは非表示です。"
                           "選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "37"            TO  WRK-ERRCD
               WHEN    "0011"
                   MOVE    "更新エラーです。"
                                           TO  WRK-ERRMSG
                   MOVE    "50"            TO  WRK-ERRCD
               WHEN    "0013"
                   MOVE    "開始日での受診がありません。"
                                           TO  WRK-ERRMSG
                   MOVE    "38"            TO  WRK-ERRCD
      ******** WHEN    "0014"
      *            MOVE    "終了日が違います。"
      ********                         TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "開始日＜終了日で入力して下さい。"
                                           TO  WRK-ERRMSG
                   MOVE    "39"            TO  WRK-ERRCD
               WHEN    "0016"
                   MOVE    "期間内に受診はありません。変更できません。"
                                           TO  WRK-ERRMSG
                   MOVE    "40"            TO  WRK-ERRCD
               WHEN    "0017"
                   STRING  "変更後の保険組合せが期間内にありません。"
                           "変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "41"            TO  WRK-ERRCD
               WHEN    "0018"
                   STRING  "変更後の保険組合せで既に受診があります。"
                           "診療行為で変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "42"            TO  WRK-ERRCD
               WHEN    "0019"
                   STRING  "剤番号が取得できませんでした。"
                           "患者情報を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "43"            TO  WRK-ERRCD
               WHEN    "0023"
                   STRING  "入院期間中です。"
                           "外来は自費保険以外の変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "44"            TO  WRK-ERRCD
               WHEN    "0021"
                   STRING  "受診履歴の伝票番号がゼロのものがあります。"
                           "変更できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "45"            TO  WRK-ERRCD
      *
               WHEN    "0010"
      *警告
                   ADD     1                   TO  IDX-WAR
                   STRING  "保険組合せの変更が終了しました。"
                           "請求額の変更をして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-KEIMSG(IDX-WAR)
                   END-STRING
                   MOVE    "W04"           TO  WRK-KEICD(IDX-WAR)
               WHEN    "0020"
      *警告
                   ADD     1                   TO  IDX-WAR
                   STRING  "レセプトコメントが複数あります。"
                           "レセプトコメントは更新しませんでした。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-KEIMSG(IDX-WAR)
                   END-STRING
                   MOVE    "W02"           TO  WRK-KEICD(IDX-WAR)
               WHEN    "K999"
                   ADD     1                   TO  IDX-WAR
                   STRING  "警告！！"
                           "変更前と後の一般・老人判定が違います。"
                           "変更後、診療内容を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    WRK-KEIMSG(IDX-WAR)
                   END-STRING
                   MOVE    "W03"           TO  WRK-KEICD(IDX-WAR)
           END-EVALUATE
           .
       8903-ONLERRCD-EXT.
           EXIT.
      *
      ****************************************************************
      *    ＰＡＲＡのＳＰＡ編集処理
      ****************************************************************
       8903-ONLJ02ERRCD-SEC           SECTION.
      *
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0064"
                   ADD     1               TO  IDX-WAR
                   MOVE    "更新すべき収納はありません。"
                                           TO  WRK-KEIMSG(IDX-WAR)
                   MOVE    "W04"           TO  WRK-KEICD(IDX-WAR)
               WHEN    "2009"
                   STRING  "収納明細の更新エラーです。"
                           "再計算できませんでした。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
                   MOVE    "52"            TO  WRK-ERRCD
               WHEN    "2001"
                   MOVE    "更新エラーです。"
                                       TO  WRK-ERRMSG
                   MOVE    "50"            TO  WRK-ERRCD
               WHEN    "2000"
                   MOVE    "更新エラーです。"
                                           TO  WRK-ERRMSG
                   MOVE    "50"            TO  WRK-ERRCD
           END-EVALUATE
           .
       8903-ONLJ02ERRCD-EXT.
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
           MOVE    "J01KYOUTU"         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           PERFORM 9010-PARA-KEYREAD-SEC
           IF      FLG-PARA            =   ZERO
               MOVE    PARA-DATA-REC       TO  SPA-J01KYOUTU
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAJ02"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-J02
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "J02HLIST"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  TBL-HKNCOMBI-AREA
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
      *****************************************************************
      *    終了処理（PARA削除・排他解除）
      *****************************************************************
       2900-SYORI-END-SEC            SECTION.
      *
           PERFORM 2009-SPATMP-DEL-SEC
           PERFORM 2902-PARA-ALLDEL-SEC
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
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "APIPARA DELETE ERR:"  MCP-RC
                           ",KEY:" APIPARA-KEY
           END-IF
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
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PARA DELETE ERR:"  MCP-RC
                           ",KEY:" PARA-KEY
           END-IF
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
           MOVE    "J02HLIST"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    TBL-HKNCOMBI-AREA   TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAJ02"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-J02             TO  SPATMP-DATA-REC
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
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PARA DELETE ERR:"  MCP-RC
                           ",KEY:" PARA-KEY
           END-IF
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
               MOVE    "診療年月の暦日エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療年月は平成２８年４月１日からです。"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません。"
                                               TO  WRK-ERRMSG
      **   WHEN    "14"
      *        MOVE    "漢字氏名は全角で入力して下さい"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "15"
      *        MOVE    "カナ氏名は全角で入力して下さい"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "16"
      *        MOVE    "性別は１：男２：女で入力して下さい"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "17"
      *        MOVE    "生年月日暦日エラー"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "18"
      *        STRING  "患者氏名・性別・生年月日が送信内容と違います。"
      *                "患者を確認して下さい。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      ***      END-STRING
      *
           WHEN    "22"
               MOVE    "前回と患者番号が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "対象の受診履歴がありません"
                                               TO  WRK-ERRMSG
           WHEN    "24"
               STRING  "前回と診療年月・診療科・入外区分が違います"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "25"
               MOVE    "変更前の保険組合せ番号が違います"
                                               TO  WRK-ERRMSG
           WHEN    "26"
               MOVE    "変更後の保険組合せ番号が違います"
                                               TO  WRK-ERRMSG
           WHEN    "27"
               MOVE    "変更開始日が違います"
                                               TO  WRK-ERRMSG
           WHEN    "28"
               MOVE    "変更終了日が違います"
                                               TO  WRK-ERRMSG
           WHEN    "29"
               MOVE    "収納更新区分が違います"
                                               TO  WRK-ERRMSG
           WHEN    "30"  THRU  "53"
               CONTINUE
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
      *    収納検索主キー処理
      *****************************************************************
       910-SYUNOU-KEY-READ-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-SYUNOU-KEY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込処理
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUNOU
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報検索
      *****************************************************************
       901-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       901-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索
      *****************************************************************
       901-PTKOHINF-READ-SEC         SECTION.
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    COMB-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE    COMB-PTID           TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDY2)   TO  PTKOH-KOHID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       901-PTHKNINF-READ-EXT.
           EXIT.
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
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
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
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   受診履歴情報読み込み
      *****************************************************************
       970-JYURRK-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計読み込み
      *****************************************************************
       930-SRYACCT-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
