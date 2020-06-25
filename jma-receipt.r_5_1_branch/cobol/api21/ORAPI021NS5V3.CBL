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
       PROGRAM-ID.         ORAPI021NS5V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院診療行為　入院調剤料更新（入力一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/12/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-TAHOKEN             PIC 9(01).
           03  FLG-HKN-ARI             PIC 9(01).
           03  FLG-UIDOK               PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-CONTMODE            PIC 9(01).
           03  FLG-NYUINCHK            PIC 9(01).
           03  FLG-SYORIEND            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDD                 PIC 9(02).
           03  IDD2                PIC 9(02).
           03  IDX-D               PIC 9(02).
      *
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
           03  IDX-Z               PIC 9(04).
      *
           03  IDH                 PIC 9(04).
           03  IDM1                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDX-ZAI             PIC 9(04).
           03  IDX-ZAI2            PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-CHK             PIC 9(04).
           03  IDX-ERR             PIC 9(04).
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
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRMSG-02           PIC X(100).
           03  WRK-RESULT-MSG          PIC X(100).
      *
           03  WRK-WERRCD-1            PIC X(04).
           03  WRK-WERRCD-2            PIC X(04).
           03  WRK-WERRCD-3            PIC X(04).
           03  WRK-WERRCD              PIC X(04).
           03  WRK-KEICD-G.
               05  WRK-KEICD-OCC       OCCURS   40.
                   07  WRK-KEICD           PIC X(04).
                   07  WRK-KEIMSG          PIC X(100).
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
      *
           03  WRK-SYORI-MODE          PIC X(03).
      *
           03  WRK-SINSATU             PIC X(01).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
           03  WRK-SRYCD               PIC X(09).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(20).
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-DAY-G.
               05  WRK-DAY-OCC         OCCURS   3.
                   07  WRK-DAY              PIC 9(03)
                                            OCCURS    31.
      *    同日再入院日
           03  WRK-SAIDAY              PIC 9(02).
      *    診療日付
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM           PIC X(06).
               05  WRK-SRYDD             PIC 9(02).
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
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-TIME8       PIC X(08).
      *
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
           03  WRK-PERFORM-TIME.
               05  WRK-PERFORM-THH     PIC X(02).
               05  WRK-PERFORM-TMM     PIC X(02).
               05  WRK-PERFORM-TSS     PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
      *
      *    入院日
      *    03  WRK-NYUIN-DATE          PIC X(10).
      *    03  WRK-NYUIN-YMD           PIC X(08).
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
               05  WRK-PARA-SRYYM          PIC X(06).
      *        剤番号
               05  WRK-PARA-ZAINUM         PIC X(08).
      *
      *    MCPAREA 保存
       01  WRK-MCPAREA             PIC X(5000).
      *
      *J02 CALL用
       01  WRK-SCRAREA.
           COPY    "ORCA24SCRAREA.INC".
      *
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
      *****COPY    "CPORCSPTNUM.INC".
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
      *    ＳＰＡ画面パラメタ
       01  SPA-SCR-REC.
           COPY    "CPJ02SPASCR.INC".
      *
      *    会計照会用共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J02SCR-SPA".
      *
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLAV3REQ5.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLAV3RES5.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    保険年齢・負担割合算定サブ
      *    COPY    "CPORCSAGECHK.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    入院履歴編集サブパラメタ
      *    COPY    "CPORCSCNYUHEN.INC".
      *
      *    剤対象データ編集領域
           COPY    "CPORAPI021SUB3V3.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
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
           DISPLAY   "* medicalreqv3 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
      *
           INITIALIZE                      SPA-J01KYOUTU
           INITIALIZE                      WRK-SCRAREA
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
               MOVE   404                  TO  APILST17-HTTPSTATUS
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
           DISPLAY   "* medicalreqv3 end  *"
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
           INITIALIZE                      XML-MEDICALAV3RES5
           MOVE    SPACE               TO  WRK-APIV3DATA-AREA
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
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
      *
      *    ＳＰＡ共通設定
           IF      WRK-ERRCD           =   SPACE
               PERFORM 1001-ORCSCOMMON-SEC
           END-IF
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
      *    ＳＰＡ共通設定処理
      *****************************************************************
       1001-ORCSCOMMON-SEC           SECTION.
      *
      *    システム日付で検索
      ***  診療日をシステム日付へ
      *    IF      MDCREQ-PERFORM-DATE NOT =   SPACE
      *        MOVE    MDCREQ-PERFORM-DATE TO  WRK-HEN-DATE
      *        MOVE    SPACE               TO  WRK-HEN-TIME
      *        PERFORM 802-DAYHEN02-SEC
      *        MOVE    WRK-SYMD            TO  WRK-PERFORM-YMD
      *        PERFORM 20011-PERFORM-CHK-SEC
      *        診療日をシステム日付へ
      *        IF      WRK-ERRCD           =   SPACE
      *            MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *        END-IF
      *        MOVE    SPACE               TO  WRK-ERRCD
      *****END-IF
      *
      *
           MOVE    "API"               TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           .
       1001-ORCSCOMMON-EXT.
           EXIT.
      *
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
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD-G
           MOVE    SPACE               TO  WRK-WERRCD
           MOVE    SPACE               TO  WRK-KEICD-G
           MOVE    ZERO                TO  FLG-UIDOK
      *
           MOVE    MDCREQ-REQUEST-NUMBER
                                       TO  MDCRES-REQUEST-NUMBER
           MOVE    MDCREQ-REQUEST-NUMBER
                                       TO  MDCRES-RESPONSE-NUMBER
      *    オルカＵＩＤ
           IF     (MDCREQ-ORCA-UID         NOT =   SPACE )
             AND  (MDCREQ-REQUEST-NUMBER   NOT =   "99"  )
               PERFORM 2009-PARA-INIT-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "01"
      *        検索処理
               MOVE    1                   TO  FLG-SYORI
               IF      MDCREQ-ORCA-UID NOT =   SPACE
                   PERFORM 20019-PARA-ALLDELDEL-SEC
               END-IF
           WHEN    "02"
      *        確認処理
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        更新処理
               MOVE    3                   TO  FLG-SYORI
      *
      *ロック解除
           WHEN    "99"
      *        強制処理終了
               PERFORM 2099-CANCEL-SEC
               GO      TO      200-MAIN-EXT
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           ELSE
      *        編集処理
               PERFORM 20010-INPUT-HEN-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *        API処理登録
               IF     (WRK-ORCA-UID    NOT =   SPACE )
                 AND  (FLG-UIDOK           =   1     )
                   PERFORM 2008-APIPARAINS-SEC
               ELSE
                   MOVE    SPACE           TO  MDCRES-ORCA-UID
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    FLG-SYORI
           WHEN    1
      *        検索内容返却処理
               PERFORM 2002-SYORI-01-SEC
           WHEN    2
      *        確認処理
               PERFORM 2003-SYORI-02-SEC
           WHEN    3
      *        更新処理
               PERFORM 2004-SYORI-03-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    強制処理終了
      *****************************************************************
       2099-CANCEL-SEC   SECTION.
      *
           MOVE    9                   TO  FLG-SYORI
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
               MOVE    SPA-TERMID          TO  APIV3SUB01-TERMID
           END-IF
           MOVE    MDCREQ-KARTE-UID    TO  APIV3SUB01-KARTE-UID
           MOVE    "AP21"              TO  APIV3SUB01-API-GYOUMUID
           MOVE    "API21NV3"          TO  APIV3SUB01-API-FILEMEI
           MOVE    WRK-PTNUM           TO  APIV3SUB01-PTNUM
           MOVE    "J02"               TO  APIV3SUB01-PARA-GYOUMUID
           CALL    "ORAPIV3SUB01"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPIV3SUB01AREA
           IF      APIV3SUB01-ERRCD    NOT =   SPACE
               MOVE    "81"                TO  WRK-ERRCD
               MOVE    APIV3SUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
           .
       2099-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST17-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALAV3REQ5
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalav3req5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST17-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalav3req5"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "xml_medicalav3req5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST17-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalav3req5"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALAV3REQ5
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST17-BODY
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
                       MOVE    "検索処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "確認処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    3
                       MOVE    "更新処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    9
                       MOVE    "解除処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
      *        警告あり
               IF      WRK-KEICD (1)   NOT =   SPACE
                AND    WRK-ERRKBN          =   SPACE
                   MOVE    "W00"           TO  MDCRES-API-RESULT
                   MOVE    MDCRES-API-RESULT-MSG
                                           TO  WRK-RESULT-MSG
                   MOVE    SPACE           TO  MDCRES-API-RESULT-MSG
                   STRING  WRK-RESULT-MSG
                           "　警告メッセージがあります。"
                                           DELIMITED  BY  SPACE
                                           INTO  MDCRES-API-RESULT-MSG
                   END-STRING
               END-IF
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
      *    ワーニング編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *
      *
           IF      APILST17-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalav3res5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalav3res5"    TO  MDCRES-RECORDNAME
      **** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALAV3RES5
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalav3res5" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalav3res5"    TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALAV3RES5
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST17-BODY 
               MOVE  "application/xml" TO  APILST17-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    各エラー、ワーニング編集
      *****************************************************************
       3001-WAINGMSG-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDM1
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   40   )   OR
                              (WRK-KEICD (IDK) =   SPACE)
               MOVE    WRK-KEICD (IDK)     TO  WRK-WERRCD
               IF      WRK-KEIMSG(IDK)     =   SPACE
      *            エラー・警告メッセージ
                   PERFORM 891-WARNING-MSG-SEC
                   MOVE    WRK-ERRMSG          TO  WRK-KEIMSG(IDK)
               END-IF
               ADD     1                   TO  IDM1
               MOVE    WRK-KEIMSG(IDK)     TO  MDCRES-WARNING-MSG
                                                               (IDM1)
               MOVE    WRK-WERRCD          TO  MDCRES-WARNING
                                                               (IDM1)
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
      *    必須チェック
           PERFORM 20010-INIT-CHECK-SEC
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
      *        排他制御処理
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 20019-LOCK-SYORI-SEC
                   IF      WRK-ERRCD           =   SPACE
      *                パラメタ登録
                       MOVE    1               TO  FLG-UIDOK
                   END-IF
               END-IF
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
           MOVE    MDCREQ-PATIENTID    TO  MDCRES-PATIENTID
           MOVE    MDCREQ-PATIENTID    TO  WRK-PTNUM
      *    診療年月
           MOVE    MDCREQ-PERFORM-YM   TO  MDCRES-PERFORM-YM
           MOVE    MDCREQ-PERFORM-YM   TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD(1:6)       TO  WRK-PERFORM-YM
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YM      =   SPACE
               MOVE    SMCNDATE-YMD(1:6)   TO  WRK-PERFORM-YM
           END-IF
      *    １日とする
           MOVE    "01"                TO  WRK-PERFORM-DD
      *
      *    診療日付
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
      *
      *    診療日をシステム日付・診療日付へ
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *
      *    入外区分（入院）
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    入力UID
           MOVE    MDCREQ-ORCA-UID     TO  MDCRES-ORCA-UID
           MOVE    MDCREQ-KARTE-UID    TO  MDCRES-KARTE-UID
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
           IF      MDCREQ-KARTE-UID    =   SPACE
               MOVE    "04"                TO  WRK-ERRCD
           END-IF
      *    電子カルテＵＩＤチェック
           IF      WRK-ERRCD           =   SPACE
               IF     (WRK-KARTE-UID       NOT =   SPACE  )
                 AND  (MDCREQ-KARTE-UID    NOT =   WRK-KARTE-UID )
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
                   MOVE    ZERO            TO  FLG-UIDOK
               END-IF
           END-IF
      *
      *    オルカＵＩＤ
           IF      MDCREQ-ORCA-UID   =   SPACE
               IF      MDCREQ-REQUEST-NUMBER NOT =   "01"
                   MOVE    "05"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20010-INIT-CHECK-EXT.
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
           MOVE    SPTID-PTID          TO  SPA-PTID
           MOVE    SPTID-PTNUM         TO  SPA-PTNUM
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
      *    排他制御処理処理
      *****************************************************************
       20019-LOCK-SYORI-SEC        SECTION.
      *
      *    ＵＩＤコード取得処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-KEICD-G
           ELSE
               MOVE    WRK-ORCA-UID        TO  MDCRES-ORCA-UID
           END-IF
      *
           .
       20019-LOCK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＵＩＤコード取得処理
      *****************************************************************
       20020-ORCAUID-SYORI-SEC        SECTION.
      *
      *    UIDコード処理
           IF      MDCREQ-ORCA-UID     =   SPACE
      *        UIDコード取得処理
               PERFORM 1001-UID-GET-SEC
               MOVE    C-UID               TO  WRK-ORCA-UID
           ELSE
               MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           END-IF
      *    UIDコードをTERMID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
           .
       20020-ORCAUID-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料返却処理
      *****************************************************************
       2002-SYORI-01-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *    会計照会初期設定
           PERFORM 20022-J02INIT-SEC
      *
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-HENSYU-01-SEC
           END-IF
      *
      *    返却レスポンス番号
           IF     (WRK-ERRCD           =   SPACE )
      *        エラーなしは次へ
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
      *
      *        SPAの登録
               PERFORM 2008-SPATMP-INS-SEC
           END-IF
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
           MOVE    WRK-ORCA-UID        TO  MDCRES-ORCA-UID
      *
     ***   MOVE    WRK-HKNCOMBI        TO  MDCRES-COMBINATION-NUMBER
      *
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会初期設定処理
      *****************************************************************
       20022-J02INIT-SEC     SECTION.
      *
           INITIALIZE                  SPA-J01KYOUTU
           INITIALIZE                  SPA-J02
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    ZERO                TO  SPA-J01-SRYKA
           MOVE    WRK-PERFORM-YM      TO  SPA-NAI-SRYYM
           MOVE    PTINF-BIRTHDAY      TO  SPA-GMN-BIRTHDAY
           MOVE    "20"                TO  SPA-GMN-SRYKBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    "1"                 TO  SPA-GMN-NYUGAIKBN
           MOVE    ZERO                TO  SPA-GMN-HHKNCOMBI
      *
           MOVE    MCPAREA             TO  WRK-MCPAREA
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "INIT5"             TO  MCP-EVENT
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
           MOVE    WRK-MCPAREA         TO  MCPAREA
      *
      *    入院中の判定
           MOVE    ZERO                TO  FLG-NYUINCHK
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL      (IDD     >   31  )
                           OR (FLG-NYUINCHK   =   1   )
               IF      SPA-NAI-NYUIN (IDD)    NOT =   ZERO
                   MOVE    1               TO  FLG-NYUINCHK
               END-IF
           END-PERFORM
           IF      FLG-NYUINCHK        =   ZERO
               MOVE    "14"             TO  WRK-ERRCD
           END-IF
      *
           IF      (SPA-NAI-HKTERR     =   1   )  AND
                   (SPA-NAI-HKTKEI     =   ZERO)
      *        当月入院料なし
      ****     MOVE    "K077"              TO  SPA-ERRCD
               ADD     1                   TO  IDK
               MOVE    "W004"              TO  WRK-KEICD(IDK)
           END-IF
           .
       20022-J02INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為内容編集処理
      *****************************************************************
       20021-HENSYU-01-SEC        SECTION.
      *
      *    J02PARA の編集
           INITIALIZE                      ORAPI021SUB3V3AREA
      *    J02CSR
           MOVE    "1"                 TO  ORAPISUB3-SYORIKBN
           CALL    "ORAPI021SUB3V3"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB3V3AREA
      *
      *    テーブルオーバー
      *    IF      ORAPISUB3-ERRCD     NOT =   SPACE
      *         ADD     1                  TO  IDK
      *         MOVE    "W001"             TO  WRK-KEICD(IDK)
      *         MOVE    ORAPISUB3-ERRMSG   TO  WRK-KEIMSG(IDK)
      *    END-IF
      *
           INITIALIZE                      MDCRES-MEDICAL-INF-G
           INITIALIZE                      MDCRES-MEDICAL-CHOZAIINF-G
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  IDX2
           MOVE    SPACE               TO  WRK-WERRCD-1
           MOVE    SPACE               TO  WRK-WERRCD-2
           MOVE    SPACE               TO  WRK-WERRCD-3
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX          >   400 )
                         OR  (IDX          >   ORAPISUB3-ZAI-MAX)
      *        保険組合せ削除状態
               IF      ORAPISUB3-COMBINATION-MODE (IDX)
                                           =   "1"
                                           OR  "2"
                   MOVE    "W001"              TO  WRK-WERRCD-1
               END-IF
      *
               IF     (ORAPISUB3-SRYSYUKBN (IDX)
                                            =   "240"
                                            OR  "260" )
                  AND (ORAPISUB3-CHOUZAIAUTO(IDX)
                                            =   "1"   )
      *            入院調剤料（自動算定分）
                   PERFORM 200211-CHOZAI-HEN-SEC
               ELSE
      *            投薬内容
                   PERFORM 200212-MEDICAL-HEN-SEC
               END-IF
           END-PERFORM
      *
      *    テーブルオーバー
           IF      WRK-WERRCD-1    NOT =   SPACE
                ADD     1                   TO  IDK
                MOVE    WRK-WERRCD-1        TO  WRK-KEICD(IDK)
           END-IF
           IF      WRK-WERRCD-2    NOT =   SPACE
                ADD     1                   TO  IDK
                MOVE    WRK-WERRCD-2        TO  WRK-KEICD(IDK)
           END-IF
           IF      WRK-WERRCD-3    NOT =   SPACE
                ADD     1                   TO  IDK
                MOVE    WRK-WERRCD-3        TO  WRK-KEICD(IDK)
           END-IF
      *
      *    入院調剤料（自動算定分）なし
           IF      IDX2                =   ZERO
                MOVE    "15"                TO  WRK-ERRCD
           END-IF
           .
       20021-HENSYU-01-EXT.
           EXIT.
      *
      ****************************************************************
      *    投薬内容編集処理
      **************************************************************
       200212-MEDICAL-HEN-SEC           SECTION.
      *
           ADD     1                    TO  IDX1
           IF      IDX1                 >   100
               MOVE    "W002"               TO  WRK-WERRCD-2
               GO      TO      200212-MEDICAL-HEN-EXT
           END-IF
      *
           MOVE    ORAPISUB3-SRYKA(IDX)
                                       TO  MDCRES-MDC-DEP-CODE(IDX1)
           MOVE    ORAPISUB3-SRYKA-NAME (IDX)
                                       TO  MDCRES-MDC-DEP-CODE-NAME
                                                             (IDX1)
      *    保険組合せ番号
           MOVE    ORAPISUB3-HKNCOMBI(IDX)
                                       TO  MDCRES-MDC-COMBINATION-NUMBER
                                                             (IDX1)
           MOVE    ORAPISUB3-MAIN-INS-NAME(IDX)
                                       TO  MDCRES-MDC-MAIN-INS-NAME
                                                             (IDX1)
           MOVE    ORAPISUB3-PUBLIC-INS-INFO(IDX)
                                       TO  MDCRES-MDC-PUBLIC-INS-INFO
                                                             (IDX1)
      *    保険組合せ状態
           MOVE    ORAPISUB3-COMBINATION-MODE (IDX)
                                       TO  MDCRES-MDC-COMBINATION-MODE
                                                             (IDX1)
      *
      *    診療区分
           MOVE    ORAPISUB3-SRYKBN (IDX)
                                       TO  MDCRES-MDC-CLASS-KBN
                                                             (IDX1)
           MOVE    ORAPISUB3-SRYSYUKBN (IDX)
                                       TO  MDCRES-MDC-CLASS  (IDX1)
           MOVE    ORAPISUB3-MDC-CLASS-NAME (IDX)
                                       TO  MDCRES-MDC-CLASS-NAME
                                                             (IDX1)
      *    剤点数
           MOVE    ORAPISUB3-MDC-POINT (IDX)
                                       TO  MDCRES-MDC-POINT
                                                             (IDX1)
      *    剤区分（包括分）
           IF       ORAPISUB3-MDC-CODE (IDX)  =   "1"
                MOVE    "True"         TO  MDCRES-INCLUSION-CLASS
                                                                (IDX)
           END-IF
      *    包括検査の検査数
      *    IF      ORAPISUB3-HKTKNSA-CNT (IDX) >   ZERO
      *         MOVE    ORAPISUB3-HKTKNSA-CNT (IDX)
      *                                   TO  MDCRES-HKTKNSA-CNT
      *                                                         (IDX)
      *    END-IF
      *    算定日毎の回数
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               MOVE    ORAPISUB3-NAI-DAY (IDX 1 IDD)
                                       TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MDC-CLASS-NUMBER
                                                          (IDX1 IDD)
           END-PERFORM
      *    同時再入院日
           MOVE    ZERO                TO  IDD2
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               IF      ORAPISUB3-NAI-DAY (IDX 3 IDD)  >  ZERO
                   ADD     1               TO  IDD2
                   MOVE    IDD             TO  MDCRES-MDC-REDAY
                                                          (IDX1 IDD2)
                   MOVE    ORAPISUB3-NAI-DAY (IDX 3 IDD)
                                       TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MDC-RECLASS-NUMBER
                                                          (IDX1 IDD2)
               END-IF
           END-PERFORM
      *
      *    剤内容編集
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   (IDY            >   50    )
                       OR  (ORAPISUB3-SRYCD (IDX IDY)
                                           =   SPACE )
               MOVE    ORAPISUB3-SRYCD (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CODE
                                                           (IDX1 IDY)
               MOVE    ORAPISUB3-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAME
                                                           (IDX1 IDY)
               MOVE    ORAPISUB3-PRSCRPT-NUMBER (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NUMBER
                                                           (IDX1 IDY)
      *        単位コード
               MOVE    ORAPISUB3-PRSCRPT-TANICD (IDX IDY)
                                       TO  MDCRES-PRSCRPT-TANICD
                                                           (IDX1 IDY)
               MOVE    ORAPISUB3-PRSCRPT-TANICD-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-TANICD-NAME
                                                           (IDX1 IDY)
           END-PERFORM
           .
       200212-MEDICAL-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    入院調剤料（自動算定分）内容編集処理
      **************************************************************
       200211-CHOZAI-HEN-SEC           SECTION.
      *
           ADD     1                    TO  IDX2
           IF      IDX2                 >   10
               MOVE    "W003"               TO  WRK-WERRCD-3
               GO      TO      200211-CHOZAI-HEN-EXT
           END-IF
      *
           MOVE    ORAPISUB3-ZAINUM(IDX)
                                       TO  MDCRES-CHZ-ZAINUM (IDX2)
      *
           MOVE    ORAPISUB3-SRYKA(IDX)
                                       TO  MDCRES-CHZ-DEP-CODE(IDX2)
           MOVE    ORAPISUB3-SRYKA-NAME (IDX)
                                       TO  MDCRES-CHZ-DEP-CODE-NAME
                                                             (IDX2)
      *    保険組合せ番号
           MOVE    ORAPISUB3-HKNCOMBI(IDX)
                                       TO  MDCRES-CHZ-COMBINATION
                                                             (IDX2)
           MOVE    ORAPISUB3-MAIN-INS-NAME(IDX)
                                       TO  MDCRES-CHZ-MAIN-INS-NAME
                                                             (IDX2)
           MOVE    ORAPISUB3-PUBLIC-INS-INFO(IDX)
                                       TO  MDCRES-CHZ-PUBLIC-INS-INFO
                                                             (IDX2)
      *    保険組合せ状態
           MOVE    ORAPISUB3-COMBINATION-MODE (IDX)
                                       TO  MDCRES-CHZ-COMBINATION-MODE
                                                             (IDX2)
      *    診療区分
           MOVE    ORAPISUB3-SRYKBN (IDX)
                                       TO  MDCRES-CHZ-CLASS-KBN
                                                             (IDX2)
           MOVE    ORAPISUB3-SRYSYUKBN (IDX)
                                       TO  MDCRES-CHZ-CLASS  (IDX2)
           MOVE    ORAPISUB3-MDC-CLASS-NAME (IDX)
                                       TO  MDCRES-CHZ-CLASS-NAME
                                                             (IDX2)
      *    剤点数
           MOVE    ORAPISUB3-MDC-POINT (IDX)
                                       TO  MDCRES-CHZ-POINT (IDX2)
      *    剤区分（包括分）
           IF       ORAPISUB3-MDC-CODE (IDX)  =   "1"
                MOVE    "True"         TO  MDCRES-CHZ-INCLUSION-CLASS
                                                            (IDX2)
           END-IF
      *    算定日毎の回数
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               MOVE    ORAPISUB3-NAI-DAY (IDX 1 IDD)
                                       TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-CHZ-CLASS-NUMBER
                                                          (IDX2 IDD)
           END-PERFORM
      *    同時再入院日
           MOVE    ZERO                TO  IDD2
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               IF      ORAPISUB3-NAI-DAY (IDX 3 IDD)  >  ZERO
                   ADD     1               TO  IDD2
                   MOVE    IDD             TO  MDCRES-CHZ-REDAY
                                                          (IDX2 IDD2)
                   MOVE    ORAPISUB3-NAI-DAY (IDX 3 IDD)
                                       TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-CHZ-RECLASS-NUMBER
                                                          (IDX2 IDD2)
               END-IF
           END-PERFORM
      *
      *    剤内容編集
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   (IDY            >   50    )
                       OR  (ORAPISUB3-SRYCD (IDX IDY)
                                           =   SPACE )
               MOVE    ORAPISUB3-SRYCD (IDX IDY)
                                       TO  MDCRES-CHZ-CODE
                                                          (IDX2 IDY)
               MOVE    ORAPISUB3-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-CHZ-NAME
                                                           (IDX2 IDY)
               MOVE    ORAPISUB3-PRSCRPT-AUTOKBN (IDX IDY)
                                       TO  MDCRES-CHZ-AUTOKBN
                                                           (IDX2 IDY)
           END-PERFORM
           .
       200211-CHOZAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認処理
      *****************************************************************
       2003-SYORI-02-SEC        SECTION.
      *
      *    PARAからSPA取得
           PERFORM 20031-PARA-SPAHEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2003-SYORI-02-EXT
           END-IF
      *
      *    前回の続きであること
           IF     (WRK-PARA-PTNUM      =   WRK-PTNUM  )
              AND (WRK-PARA-PTNUM      =   SPA-PTNUM  )
              AND (WRK-PARA-SRYYM      =   WRK-PERFORM-YM )
               CONTINUE
           ELSE
               MOVE    "09"                TO  WRK-ERRCD
           END-IF
           IF      WRK-RESPONSE-NUMBER     =   "02"
                                           OR  "03"
               CONTINUE
           ELSE
               MOVE    "09"                TO  WRK-ERRCD
           END-IF
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-INPUTCHK-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        SPAの登録
               PERFORM 2008-SPATMP-INS-SEC
               MOVE    "03"            TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *    API処理登録
           PERFORM 2008-APIPARAINS-SEC
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       2004-SYORI-03-SEC        SECTION.
      *
      *    PARAからSPA取得
           PERFORM 20031-PARA-SPAHEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2004-SYORI-03-EXT
           END-IF
      *
      *    前回の続きであること
           IF     (WRK-PARA-PTNUM      =   WRK-PTNUM  )
              AND (WRK-PARA-PTNUM      =   SPA-PTNUM  )
              AND (WRK-PARA-SRYYM      =   WRK-PERFORM-YM )
              AND (WRK-RESPONSE-NUMBER =   "03"      )
               CONTINUE
           ELSE
               MOVE    "09"                TO  WRK-ERRCD
           END-IF
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
           MOVE    WRK-PARA-ZAINUM     TO  WRK-ZAINUM
      *
           IF      WRK-ERRCD           =   SPACE
      *        SPAと一致すること
               MOVE    SPA-NAI-BANGO       TO  IDX
               IF     (IDX                 =   ZERO)
                OR    (SPA-NAI-ZAINUM(IDX)
                                       NOT =   WRK-ZAINUM)
                   MOVE    "29"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20041-UPDATE-SYORI-SEC
           END-IF
      *
      *    返却レスポンス番号
           IF      WRK-ERRCD           =   SPACE
      *        終了
               IF      FLG-CONTMODE        =   ZERO
                   PERFORM 2900-SYORI-END-SEC
                   MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
               ELSE
      *        継続
                   PERFORM 20019-PARA-ALLDELDEL-SEC
      *
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
      *            API処理登録
                   PERFORM 2008-APIPARAINS-SEC
               END-IF
           ELSE
               MOVE    "02"                TO  MDCRES-RESPONSE-NUMBER
      *        API処理登録
               PERFORM 2008-APIPARAINS-SEC
           END-IF
           .
       2004-SYORI-03-EXT.
           EXIT.
      *****************************************************************
      *    算定日変更チェック処理
      *****************************************************************
       20031-INPUTCHK-SEC        SECTION.
      *
      *    送信内容編集
           PERFORM 20031-CHO-MDCRES-HEN-SEC
      *
      *    剤存在チェック
           PERFORM 20032-ZAINUM-CHKHEN-SEC
      *    算定日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200312-INPUT-DAY-CHK-SEC
           END-IF
      *
           .
       20031-INPUTCHK-EXT.
           EXIT.
      *****************************************************************
      *    送信内容編集処理
      *****************************************************************
       20031-CHO-MDCRES-HEN-SEC        SECTION.
      *
           MOVE    1                   TO  IDX2
      *
           MOVE    MDCREQ-CHZ-ZAINUM
                                       TO  MDCRES-CHZ-ZAINUM (IDX2)
           MOVE    MDCREQ-CHZ-DEP-CODE
                                       TO  MDCRES-CHZ-DEP-CODE(IDX2)
      *    保険組合せ番号
           MOVE    MDCREQ-CHZ-COMBINATION
                                       TO  MDCRES-CHZ-COMBINATION
                                                             (IDX2)
      *    診療区分
           MOVE    MDCREQ-CHZ-CLASS
                                       TO  MDCRES-CHZ-CLASS  (IDX2)
           MOVE    MDCREQ-CHZ-CLASS-NAME
                                       TO  MDCRES-CHZ-CLASS-NAME
                                                             (IDX2)
      *    算定日毎の回数
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               MOVE    MDCREQ-CHZ-CLASS-NUMBER (IDD)
                                       TO  MDCRES-CHZ-CLASS-NUMBER
                                                          (IDX2 IDD)
      *        空白はゼロにする
               IF      MDCREQ-CHZ-CLASS-NUMBER (IDD)  =   SPACE
                   MOVE    "0"         TO  MDCRES-CHZ-CLASS-NUMBER
                                                          (IDX2 IDD)
               END-IF
           END-PERFORM
      *    同時再入院日
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               MOVE    MDCREQ-CHZ-REDAY  (IDD)
                                       TO  MDCRES-CHZ-REDAY
                                                          (IDX2 IDD)
               MOVE    MDCREQ-CHZ-RECLASS-NUMBER(IDD)
                                       TO  MDCRES-CHZ-RECLASS-NUMBER
                                                          (IDX2 IDD)
           END-PERFORM
      *    剤内容編集
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   (IDY            >   5     )
                       OR  (MDCREQ-CHZ-CODE (IDY)
                                           =   SPACE )
               MOVE    MDCREQ-CHZ-CODE (IDY)
                                       TO  MDCRES-CHZ-CODE
                                                          (IDX2 IDY)
               MOVE    MDCREQ-CHZ-NAME (IDY)
                                       TO  MDCRES-CHZ-NAME
                                                           (IDX2 IDY)
               MOVE    MDCREQ-CHZ-AUTOKBN (IDY)
                                       TO  MDCRES-CHZ-AUTOKBN
                                                           (IDX2 IDY)
           END-PERFORM
      *
           .
       20031-CHO-MDCRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定日変更選択処理
      *****************************************************************
       20032-ZAINUM-CHKHEN-SEC        SECTION.
      *
      *    保険組合せ番号
           PERFORM 200311-HKNCOMBI-CHK-SEC
      *    剤番号数値チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200311-ZAINUM-CHK-SEC
           END-IF
      *    算定日数値チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200311-DAY-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  20032-ZAINUM-CHKHEN-EXT
           END-IF
      *    診療科
           MOVE    MDCREQ-CHZ-DEP-CODE     TO WRK-SRYKA
      *
      *    対象剤チェック
           MOVE    ZERO                TO  IDX-Z
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   ORAPISUB3-ZAI-MAX)
                        OR   (IDX-Z    >   ZERO   )
               IF     (ORAPISUB3-ZAINUM (IDX)
                                           =   WRK-ZAINUM )
                 AND  (ORAPISUB3-SRYKA  (IDX)
                                           =   WRK-SRYKA  )
                 AND  (ORAPISUB3-HKNCOMBI(IDX)
                                           =   WRK-HKNCOMBI)
                 AND  (ORAPISUB3-SRYSYUKBN(IDX)
                                           =   MDCREQ-CHZ-CLASS)
      *
                   IF     (ORAPISUB3-SRYSYUKBN (IDX)
                                            =   "240"
                                            OR  "260" )
                      AND (ORAPISUB3-CHOUZAIAUTO(IDX)
                                            =   "1"   )
                       MOVE    IDX              TO  IDX-Z
                   END-IF
               END-IF
      *        剤内容の送信があればチェックする
               MOVE    ZERO            TO  FLG-CHK
               IF     (IDX-Z               >   ZERO )
                  AND ( MDCREQ-CHZ-CODE (1)    NOT =   SPACE )
                   PERFORM VARYING     IDY      FROM    1   BY  1
                           UNTIL     (IDY      >   5    )
                       IF     MDCREQ-CHZ-CODE (IDY)
                                           NOT =   ORAPISUB3-SRYCD
                                                          (IDX IDY)
                           MOVE    1           TO  FLG-CHK
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF    IDX-Z             =   ZERO
      *        対象剤なし
               MOVE    "21"            TO  WRK-ERRCD
           ELSE
      *        診療内容不一致
               IF      FLG-CHK         =   1
                   MOVE    "22"            TO  WRK-ERRCD
               END-IF
               PERFORM 200311-MDCCHOZAI-HEN-SEC
           END-IF
      *    SPA剤チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200311-J02ZAINUM-CHK-SEC
           END-IF
           .
       20032-ZAINUM-CHKHEN-EXT.
           EXIT.
      *****************************************************************
      *    更新剤内容編集処理
      *****************************************************************
       200311-MDCCHOZAI-HEN-SEC          SECTION.
      *
           MOVE    1                    TO  IDX2
      *
           MOVE    ORAPISUB3-SRYKA(IDX-Z)
                                       TO  MDCRES-CHZ-DEP-CODE(IDX2)
           MOVE    ORAPISUB3-SRYKA-NAME (IDX-Z)
                                       TO  MDCRES-CHZ-DEP-CODE-NAME
                                                             (IDX2)
      *    保険組合せ番号
           MOVE    ORAPISUB3-HKNCOMBI(IDX-Z)
                                       TO  MDCRES-CHZ-COMBINATION
                                                             (IDX2)
           MOVE    ORAPISUB3-MAIN-INS-NAME(IDX-Z)
                                       TO  MDCRES-CHZ-MAIN-INS-NAME
                                                             (IDX2)
           MOVE    ORAPISUB3-PUBLIC-INS-INFO(IDX-Z)
                                       TO  MDCRES-CHZ-PUBLIC-INS-INFO
                                                             (IDX2)
      *    保険組合せ状態
           MOVE    ORAPISUB3-COMBINATION-MODE (IDX-Z)
                                       TO  MDCRES-CHZ-COMBINATION-MODE
                                                             (IDX2)
      *    診療区分
           MOVE    ORAPISUB3-SRYKBN (IDX-Z)
                                       TO  MDCRES-CHZ-CLASS-KBN
                                                             (IDX2)
           MOVE    ORAPISUB3-SRYSYUKBN (IDX-Z)
                                       TO  MDCRES-CHZ-CLASS  (IDX2)
           MOVE    ORAPISUB3-MDC-CLASS-NAME (IDX-Z)
                                       TO  MDCRES-CHZ-CLASS-NAME
                                                             (IDX2)
      *    剤点数
           MOVE    ORAPISUB3-MDC-POINT (IDX-Z)
                                       TO  MDCRES-CHZ-POINT (IDX2)
      *    剤区分（包括分）
           IF       ORAPISUB3-MDC-CODE (IDX-Z)  =   "1"
                MOVE    "True"         TO  MDCRES-CHZ-INCLUSION-CLASS
                                                            (IDX2)
           END-IF
      *    剤内容編集
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   (IDY            >   50    )
                       OR  (ORAPISUB3-SRYCD (IDX-Z IDY)
                                           =   SPACE )
               MOVE    ORAPISUB3-SRYCD (IDX-Z IDY)
                                       TO  MDCRES-CHZ-CODE
                                                          (IDX2 IDY)
               MOVE    ORAPISUB3-PRSCRPT-NAME (IDX-Z IDY)
                                       TO  MDCRES-CHZ-NAME
                                                           (IDX2 IDY)
               MOVE    ORAPISUB3-PRSCRPT-AUTOKBN (IDX-Z IDY)
                                       TO  MDCRES-CHZ-AUTOKBN
                                                           (IDX2 IDY)
           END-PERFORM
      *
      *    算定日毎の回数
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               MOVE    WRK-DAY  (1 IDD)
                                       TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-CHZ-CLASS-NUMBER
                                                          (IDX2 IDD)
           END-PERFORM
      *    同時再入院日
           MOVE    ZERO                TO  IDD2
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               IF      WRK-DAY (3 IDD)  >  ZERO
                   ADD     1               TO  IDD2
                   MOVE    IDD             TO  MDCRES-CHZ-REDAY
                                                          (IDX2 IDD2)
                   MOVE    WRK-DAY (3 IDD)
                                       TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-CHZ-RECLASS-NUMBER
                                                          (IDX2 IDD2)
               END-IF
           END-PERFORM
           .
       200311-MDCCHOZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       200311-HKNCOMBI-CHK-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-CHZ-COMBINATION  TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "17"                TO  WRK-ERRCD
               MOVE     MDCREQ-CHZ-COMBINATION TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
           MOVE    WRK-HKNCOMBI        TO  MDCRES-CHZ-COMBINATION
                                                             (IDX2)
      *
           .
       200311-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    剤番号数値チェック
      *****************************************************************
       200311-ZAINUM-CHK-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-CHZ-ZAINUM   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "18"                TO  WRK-ERRCD
               MOVE    ZERO                TO  WRK-ZAINUM
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-ZAINUM
           END-IF
      *
           .
       200311-ZAINUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    剤番号数値チェック
      *****************************************************************
       200311-DAY-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-DAY-G
      *    算定日毎の回数
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-CHZ-CLASS-NUMBER (IDD)
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "23"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-DAY (1 IDD)
               END-IF
           END-PERFORM
      *
      *    同日再入院日の回数
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               MOVE    ZERO                TO  IDD2
               IF      MDCREQ-CHZ-REDAY (IDD)    NOT =   SPACE
                   INITIALIZE                  ORCSNUMAREA
                   MOVE    MDCREQ-CHZ-REDAY (IDD)
                                           TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )
                       MOVE    "24"                TO  WRK-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  IDD2
                   END-IF
      *            算定日チェック
                   IF      IDD2                <   1   OR
                                               >   31
                       MOVE    "24"                TO  WRK-ERRCD
                   END-IF
               END-IF
      *        算定回数
               MOVE    ZERO                TO  WRK-SAIDAY
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-CHZ-RECLASS-NUMBER (IDD)
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "25"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-SAIDAY
               END-IF
      *
               IF     (WRK-ERRCD           =   SPACE)
                 AND  (IDD2            NOT =   ZERO )
                   MOVE    WRK-SAIDAY      TO  WRK-DAY (3 IDD2)
               END-IF
      *        同日再入院日の指定があること
               IF     (WRK-ERRCD           =   SPACE)
                 AND  (IDD2                =   ZERO )
                 AND  (WRK-SAIDAY          >   ZERO )
                   MOVE    "24"            TO  WRK-ERRCD
               END-IF
      *        同日再入院の日であること
               IF     (WRK-ERRCD           =   SPACE)
                 AND  (IDD2            NOT =   ZERO )
                   IF     (SPA-NAI-NYUIN (IDD2)    NOT =   3 )
                       MOVE    "26"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    算定テーブル編集
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200311-DAY-CHK-EXT
           END-IF
      *
           PERFORM VARYING     IDD     FROM   1   BY  1
                   UNTIL       IDD     >   31
               IF      WRK-DAY (3 IDD)     >   ZERO
      *            合計がゼロは、同日再入院日の算定回数
                   IF      WRK-DAY (1 IDD)     =   ZERO
                       MOVE    WRK-DAY (3 IDD)
                                           TO  WRK-DAY (1 IDD)
                   END-IF
               END-IF
      *        合計回数-同日再入院日回数
               IF      WRK-DAY (1 IDD)     <   WRK-DAY (3 IDD)
                   MOVE    "27"            TO  WRK-ERRCD
               ELSE
                   COMPUTE WRK-DAY (2 IDD) =   WRK-DAY (1 IDD)
                                           -   WRK-DAY (3 IDD)
               END-IF
           END-PERFORM
           .
       200311-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定日変更チェック処理
      *****************************************************************
       200311-J02ZAINUM-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-BANGO
           MOVE    ZERO                TO  IDX-CHK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX              >   400  )
                        OR   (SPA-GMN-BANGO    >   ZERO )
               IF      SPA-NAI-ZAINUM (IDX)    =   WRK-ZAINUM
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   MOVE    SPA-GMN-TBANGO (IDG)
                                               TO  SPA-GMN-BANGO
                   MOVE    IDX                 TO  SPA-NAI-BANGO
                   MOVE    IDX                 TO  IDX-CHK
               END-IF
           END-PERFORM
           IF     (IDX-CHK             >   ZERO )
              AND (SPA-NAI-HKNCOMBI (IDX-CHK)
                                       =   WRK-HKNCOMBI)
              AND (SPA-NAI-SRYKA (IDX-CHK)
                                       =   WRK-SRYKA   )
      *       SPAと一致すること
               CONTINUE
           ELSE
               MOVE    "22"            TO  WRK-ERRCD
               GO  TO  200311-J02ZAINUM-CHK-EXT
           END-IF
      *
           MOVE    MCPAREA             TO  WRK-MCPAREA
           INITIALIZE                  WRK-SCRAREA
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "CHKZ5"             TO  MCP-EVENT
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
           MOVE    WRK-MCPAREA         TO  MCPAREA
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "28"            TO  WRK-ERRCD
               MOVE    SPA-ERRCD       TO  MDCRES-MEDI-RESULT
               MOVE    SPA-ERRMSG      TO  MDCRES-MEDI-RESULT-MSG
           END-IF
           .
       200311-J02ZAINUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日変更チェック処理
      *****************************************************************
       200312-INPUT-DAY-CHK-SEC        SECTION.
      *
           MOVE    MCPAREA             TO  WRK-MCPAREA
      *
           INITIALIZE                  SPA-GMN-DAY-G
           INITIALIZE                  SPA-GNAI-DAY-G
      *
           PERFORM   VARYING   IDD     FROM    1   BY  1
                     UNTIL    (IDD         >   31  )
               MOVE    WRK-DAY (1 IDD)     TO  SPA-GMN-DAY (IDD)
      *
               MOVE    WRK-DAY (1 IDD)     TO  SPA-GNAI-DAY(1 IDD)
               MOVE    WRK-DAY (2 IDD)     TO  SPA-GNAI-DAY(2 IDD)
               MOVE    WRK-DAY (3 IDD)     TO  SPA-GNAI-DAY(3 IDD)
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-NAI-KEIFLG
           MOVE    ZERO                TO  SPA-NAI-KEIFLG2
           MOVE    SPACE               TO  SPA-ERRCD
      *
           PERFORM UNTIL    FLG-SYORIEND   =   1
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    "CHKD5"             TO  MCP-EVENT
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
           IF      SPA-ERRCD(1:1)  =   "K"
               ADD     1               TO  IDK
               MOVE    SPA-ERRCD       TO  WRK-KEICD (IDK)
               MOVE    SPA-ERRMSG      TO  WRK-KEIMSG(IDK)
               MOVE    ZERO            TO  FLG-SYORIEND
               MOVE    SPACE           TO  SPA-ERRCD
           ELSE
               MOVE    1               TO  FLG-SYORIEND
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "28"            TO  WRK-ERRCD
               MOVE    SPA-ERRCD       TO  MDCRES-MEDI-RESULT
               MOVE    SPA-ERRMSG      TO  MDCRES-MEDI-RESULT-MSG1
           END-IF
      *
           END-PERFORM
      *
      *    包括算定日のチェック
           IF      SPA-GMN-HKTMSG  NOT =   SPACE
               ADD     1               TO  IDK
               MOVE    "W005"          TO  WRK-KEICD (IDK)
               IF      SPA-JIHOKBN         =   "1"
                   STRING  "出来高対象の日があります。"
                           SPA-GMN-HKTMSG
                                       DELIMITED BY  SIZE
                                       INTO    WRK-KEIMSG(IDK)
                   END-STRING
               ELSE
                   STRING  "包括対象の日があります。"
                           SPA-GMN-HKTMSG
                                       DELIMITED BY  SIZE
                                       INTO    WRK-KEIMSG(IDK)
                   END-STRING
               END-IF
               MOVE    SPACE           TO  SPA-GMN-HKTMSG
           END-IF
      *
           MOVE    WRK-MCPAREA         TO  MCPAREA
      *
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        MOVE    "28"            TO  WRK-ERRCD
      *        MOVE    SPA-ERRCD       TO  MDCRES-MEDI-RESULT
      *        MOVE    SPA-ERRMSG      TO  MDCRES-MEDI-RESULT-MSG1
      **       IF      SPA-ERRCD        =   "0025"
      *            STRING  "有効期間："
      *                    SPA-GMN-HKNCOMBMSG
      *                                DELIMITED  BY  SIZE 
      *                                INTO    MDCRES-MEDI-RESULT-MSG2
      *            END-STRING
      **       END-IF
      *     END-IF
           .
       200312-INPUT-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    SPA内容　取得処理
      *****************************************************************
       20031-PARA-SPAHEN-SEC        SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-J01AREA"       TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-AREA
               MOVE    SPA-WORK            TO  SPA-J01KYOUTU
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-J02"           TO  SPATMP-FILEMEI
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
           MOVE    "SUB3V3"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  ORAPI021SUB3V3AREA
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
      *    画面ＳＰＡ読み込み
      *
           INITIALIZE                      SPA-SCR-REC
      *    tbl_spa_tmp を使用する
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "J02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO   SPA-SCR-REC
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           .
       20031-PARA-SPAHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日変更処理
      *****************************************************************
       20041-UPDATE-SYORI-SEC        SECTION.
      *
           MOVE    SPA-NAI-BANGO       TO  IDX
      *
      *    診療会計テーブル検索
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-NAI-SRYKA(IDX)  TO  ACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           MOVE    SPA-NAI-SRYKBN(IDX) TO  ACCT-SRYKBN
           MOVE    SPA-NAI-ZAINUM(IDX) TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYACCT          =   ZERO
      *
      *        診療会計マスター変更・診療内容確認
               PERFORM 200411-SRYACCT-KOUSIN-SEC
      *
               IF      WRK-ERRCD        =   SPACE
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
                   MOVE    SPA-OPID            TO  ACCT-OPID
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "J02 SRYACCT UPD ERR:" MCP-RC
                        MOVE    "40"            TO  WRK-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    "29"            TO  WRK-ERRCD
           END-IF
      *
           .
       20041-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日更新処理
      *****************************************************************
       200411-SRYACCT-KOUSIN-SEC        SECTION.
      *
      *    回数変更時、算定履歴を変更する
           IF      SPA-NAI-DAY-AREA (IDX)  NOT =   ACCT-DAY-AREA
      *        診療内容を確認・算定履歴更新
               PERFORM 200411-SRYACT-CHK-SEC
           END-IF
      *
      *    剤回数
           MOVE    SPA-NAI-ZAIKAISU(IDX)   TO  ACCT-ZAIKAISU
      *    回数テーブル（１日から３１日）
           MOVE    SPA-NAI-DAY-AREA (IDX)  TO  ACCT-DAY-AREA
           .
        200411-SRYACCT-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為　検索処理
      *****************************************************************
       200411-SRYACT-CHK-SEC        SECTION.
      *
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYYMM
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
      *    診療行為マスター読み込み
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
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL      (FLG-SRYACT      =   1 )
               PERFORM VARYING    IDZ      FROM    1   BY  1
                       UNTIL     (IDZ      >   5    )
                             OR  (SRY-SRYCD  (IDZ) =   SPACE )
                   IF     (SRY-SRYKBN          =   "24"
                                               OR  "26" )
                     AND  (SRY-SRYCD  (IDZ)    =   "120001110"
                                               OR  "120000410" )
                     AND  (SRY-AUTOKBN (IDZ)   =   "3"         )
                       MOVE    1                   TO  FLG-OK
      *
      *                算定履歴チェック
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 200411-SANTEI-UPD-SEC
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   ZERO
      *        入院調剤料の剤でない
               MOVE    "29"            TO  WRK-ERRCD
           END-IF
           .
       200411-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       200411-SANTEI-UPD-SEC           SECTION.
      *
      *    包括入力の時対象外
           IF      ACCT-HKNCOMBI       =   "9999"
               GO      TO      200411-SANTEI-UPD-EXT
           END-IF
      *    今回分の登録
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "J02"               TO  SSANTEI-PG
           MOVE    SRY-SRYCD (IDZ)     TO  SSANTEI-SRYCD
           MOVE    ACCT-SRYYM          TO  SSANTEI-SRYYMD(1:6)
           MOVE    "01"                TO  SSANTEI-SRYYMD(7:2)
           MOVE    ACCT-PTID           TO  SSANTEI-PTID
           MOVE    ACCT-NYUGAIKBN      TO  SSANTEI-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  SSANTEI-SRYKA
           MOVE    ZERO                TO  SSANTEI-MSANTEID
           MOVE    ZERO                TO  SSANTEI-MSANTEICNT
      *    保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  SPA-HKNCOMBINUM
      *    数量
      *    １日上限で単位が分以外の時、数量編集
           IF     (SRY-SRYSURYO(IDZ)   >   ZERO  )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
               MOVE    SRY-SRYSURYO(IDZ)   TO  SSANTEI-SRYSURYO
           END-IF
      *    保険内容編集
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKN-MAX
               IF      SPA-GMN-THKNCOMBINUM (IDH)
                                           =   SPA-HKNCOMBINUM
      *            労災判定
                   MOVE    SPA-NAI-THKNKBN(IDH)    TO  SPA-HKNKBN
                   MOVE    SPA-NAI-TRSI-HKNKBN (IDH)
                                                   TO  SPA-RSI-HKNKBN
                   MOVE    SPA-NAI-TRSI-JUNKYO (IDH)
                                                   TO  SPA-RSI-JUNKYO
               END-IF
           END-PERFORM
      *
      *    今回分
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL   IDX-D       >   31
               IF      SPA-NAI-DAY (IDX 1 IDX-D)  >   ZERO
                   IF      SSANTEI-SRYYMD(7:2)    =  "01"
                       MOVE    IDX-D       TO  SSANTEI-SRYYMD(7:2)
                   END-IF
                   MOVE    SPA-NAI-DAY (IDX 1 IDX-D)
                                           TO  SSANTEI-DAY (IDX-D)
               END-IF
           END-PERFORM
      *    前回分
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL   IDX-D       >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   MOVE    ACCT-DAY (1 IDX-D)
                                           TO  SSANTEI-MAE-DAY (IDX-D)
               END-IF
           END-PERFORM
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
           .
       200411-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *     SPATMPの登録処理
      *****************************************************************
       2008-SPATMP-INS-SEC            SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SPATMP DELETE ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
           END-IF
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-J01AREA"       TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-J02"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-J02             TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SUB3V3"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    ORAPI021SUB3V3AREA  TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
           .
       2008-SPATMP-INS-EXT.
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
           MOVE    SPACE               TO  WRK-APIV3DATA-REC
           MOVE    MDCRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
      *    処理ＰＧ
           MOVE    "11"                TO  WRK-PARA-SYORIPG
      *
           MOVE    WRK-PERFORM-YM      TO  WRK-PARA-SRYYM
           MOVE    WRK-ZAINUM          TO  WRK-PARA-ZAINUM
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
           MOVE    "API21NV3"          TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA          =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
               MOVE    APIPARA-PTNUM       TO  WRK-PARA-PTNUM
               MOVE    1                   TO  FLG-UIDOK
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
      *
           IF     (WRK-ERRCD               =   SPACE )
             AND  (MDCREQ-REQUEST-NUMBER   NOT =   "01"  )
      *        リクエスト０１以外は、同じ処理の続きであること
      *        診療行為登録の処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "11"
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2009-PARA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       20019-PARA-ALLDELDEL-SEC        SECTION.
      *
      *    一時保存ファイル削除
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
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "J02"               TO  SPATMP-GYOUMUID
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
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       20019-PARA-ALLDELDEL-EXT.
           EXIT.
      *****************************************************************
      *    終了処理（PARA削除・排他解除）
      *****************************************************************
       2900-SYORI-END-SEC            SECTION.
      *
      *    PARA削除
           PERFORM 20019-PARA-ALLDELDEL-SEC
      *    前回の排他制御解除
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
               MOVE    "81"            TO  WRK-ERRCD
           END-IF
      *
           .
       2900-SYORI-END-EXT.
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
               MOVE    "処理区分エラー"
                                               TO  WRK-ERRMSG
      *    WHEN    "03"
      *        MOVE    "ドクターが未設定です"
      *                                        TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "電子カルテＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "オルカＵＩＤが未設定です"
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
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療日が平成２８年４月１日以降ではありません"
                                               TO  WRK-ERRMSG
      **   WHEN    "13"
      *        MOVE    "診療科エラーです"
      *                                        TO  WRK-ERRMSG
           WHEN    "14"
               STRING  "入院中ではありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *    WHEN    "15"
      *        STRING  "保険組合せ番号が期間外の算定があります。"
      *                "保険組合せ番号変更処理を行って下さい。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      *        END-STRING
           WHEN    "15"
               STRING  "自動算定した入院調剤料は存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "17"
               MOVE    "保険組合せ番号が数値ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "18"
               MOVE    "剤番号が数値ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               STRING  "処理順が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "21"
               STRING  "対象の剤が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "22"
               MOVE    "剤の内容が一致しません。"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "算定回数が数値エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "24"
               MOVE    "同日再入院日がエラーです"
                                               TO  WRK-ERRMSG
           WHEN    "25"
               MOVE    "同日再入院日の算定回数が数値エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "26"
               MOVE    "同日再入院日ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "27"
               STRING  "算定回数が同日再入院日の算定回数以下です。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "28"
               MOVE    "エラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "29"
               MOVE    "更新対象の剤ではありません。"
                                               TO  WRK-ERRMSG
      *
      *    WHEN    "30"
      *        STRING  "選択項目があります。"
      *                "選択結果を返却してください。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      *        END-STRING
      *
           WHEN    "30"
               MOVE    "更新処理エラーです。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "80"
               STRING  "一時データ出力エラーです。"
                       "強制終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "81"
               STRING  "解除対象がありません。"
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
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
      ******** MOVE    "処理区分未設定"        TO  WRK-ERRMSG
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
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       891-WARNING-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-WERRCD
      *警告
           WHEN    "W001"
               STRING  "保険組合せが期間外の算定があります。"
                       "保険組合せ変更処理を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W002"
               MOVE    "投薬の剤数が最大件数以上となります"
                                               TO  WRK-ERRMSG
           WHEN    "W003"
               STRING  "入院調剤料の剤数が最大件数以上となります"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W004"
               STRING  "当月の入院料が算定されていません。"
                       "入院料包括チェックはできません。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
               END-STRING
           WHEN    "W005"
               CONTINUE
           END-EVALUATE
           .
       891-WARNING-MSG-EXT.
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
      **   MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
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
               MOVE    "9999"              TO  SPA-ERRCD
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
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-RAED-SEC          SECTION.
      *
           PERFORM  910-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    診療会計テーブル読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
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
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
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
      *    附加情報の上限回数設定
           IF     (FLG-TENSU           =   ZERO )  AND
                  (TNS-SRYCD(1:1)      =   "1"  )
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOEDYMD
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
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNT1W       TO  TNS-JGNCNT1W
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
      *
               MOVE    TNSP-JGNCNTETCM     TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC      TO  TNS-JGNCNTETC
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
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
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
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
      *    パラメータテーブル検索処理
      *****************************************************************
       900-PARA-CALL-SEC                SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    PARA-REC            TO  MCPDATA-REC
      *
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PARA-CALL-EXT.
           EXIT.
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
