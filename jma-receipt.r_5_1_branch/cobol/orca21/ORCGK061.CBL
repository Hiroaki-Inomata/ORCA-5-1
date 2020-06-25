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
       PROGRAM-ID.             ORCGK061.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 算定履歴移行履歴登録（Ｋ０６１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/11/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  11/08/XX  リハビリコメント終了日対応
      *  04.07.00    NACL-多々納  11/09/XX  精神科ケア開始日対応
      *  04.07.00    NACL-多々納  13/09/11  リハビリコメント削除対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  04.08.00    NACL-多々納  16/10/31  HAORI対応
      *  05.00.00    NACL-多々納  19/08/XX  リハビリ開始日と終了日チェック
      *  05.00.00    NACL-多々納  19/10/XX  初診料（移行）削除対応対応
      *  05.00.00    NACL-多々納  20/04/13  Ｒ０２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    算定履歴更新パラメタ
           COPY    "K06SCR-SPA".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TBLOK           PIC 9(01).
           03  FLG-SANTEI-OK       PIC 9(01).
           03  FLG-SRY-OK          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
      *
      *H25.9
           03  FLG-PLUS-DEL        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDXS                PIC 9(02).
      *
           03  IDX-C               PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-YMD             PIC X(09).
           03  WRK-YMDMAX          PIC 9(01). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    保険適用期間
           03  WRK-ST-SRYYMD.
               05  WRK-STYY            PIC 9(04).
               05  WRK-STMM            PIC 9(02).
               05  WRK-STDD            PIC 9(02).
           03  WRK-ED-SRYYMD.
               05  WRK-EDYY            PIC 9(04).
               05  WRK-EDMM            PIC 9(02).
               05  WRK-EDDD            PIC 9(02).
      *
           03  WRK-KIDMSG              PIC X(80).
           03  WRK-DAY                 PIC 9(02).
      *
           03  WRK-ZZ                  PIC ZZ9.
           03  WRK-Z9                  PIC ZZ.
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
           03  WRK-DD                  PIC 9(02).
           03  WRK-LSTYMD              PIC X(08).
      *
           03  WRK-YUKOSTYMD           PIC X(08).
      *
           03  WRK-CNT                 PIC 9(03).
           03  WRK-CNT-SYOSIN          PIC 9(03).
      *
           03  WRK-CD-MCNT            PIC 9(03).
           03  WRK-ICD-CDSYU          PIC X(01).
           03  FLG-KANJI              PIC 9(01).
           03  FLG-K98-SYORI          PIC 9(01).
      *
           03  WRK-RENNUM             PIC 9(03).
           03  WRK-RENNUM2            PIC 9(03).
      *    診療コード
           03  WRK-SRYCD              PIC X(09).
      *    保険組合せ名称
           03  WRK-HKNCOMBI           PIC X(04).
           03  WRK-HKNCOMBIMEI        PIC X(100).
           03  WRK-HKNKBN-MEI         PIC X(02).
           03  WRK-COMB-TEKSTYMD      PIC X(08).
           03  WRK-COMB-TEKEDYMD      PIC X(08).
      *入力タイプ
           03  WRK-INTYPE             PIC X(01).
      *
           03  WRK-MIDASI             PIC X(160).
      *
           03  WRK-END-YMD.
               05  WRK-END-YM          PIC X(06).
               05  WRK-END-DD          PIC X(02).
           03  IDD                     PIC 9(02).
      *
           03  WRK-SYOYMD              PIC X(08).
           03  WRK-SYOYMDG             PIC X(09).
      *
           03  WRK-END-SRYCD           PIC X(09).
      *R01.10
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-IN            PIC X(02).
           03  FLG-KARRK-ARI           PIC 9(01).
           03  WRK-SYO-YMD.
               05  WRK-SYO-YM          PIC X(06).
               05  WRK-SYO-DD          PIC X(02).
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
      *
      *    算定履歴テーブル
       01  TBL-DAT-G.
           03  TBL-DAT-OCC          OCCURS   100.
               05  TBL-DAT-SRYCD           PIC X(09).
               05  TBL-DAT-HKNCOMBI        PIC X(04).
               05  TBL-DAT-SRYYM           PIC X(06).
               05  TBL-DAT-FSANTEIYMD      PIC X(08).
               05  TBL-DAT-MSANTEID        PIC X(02).
               05  TBL-DAT-DAY-G.
                   07  TBL-DAT-DAY         PIC 9(03)
                                              OCCURS   31.
      *        1:複数あり、２：受診履歴あり
               05  TBL-DAT-SYORIKBN        PIC X(01).
               05  TBL-DAT-CHKKBN          PIC X(01).
               05  TBL-DAT-SANTEIPLUS      PIC X(01).
      *
               05  TBL-DAT-ENDYMD          PIC X(08).
               05  TBL-DAT-ENDFLG          PIC X(01).
      *R01.9
               05  TBL-DAT-ENDCHK          PIC X(01).
               05  TBL-DAT-DAYKEY-G.
                   07  TBL-DAT-DAYKEY      PIC X(02)  OCCURS  31.
      *
      *移行算定履歴作成
       01  TBL-IKOU-SRYCD-G.
      *    初診ダミー
           03  FILLER          PIC X(09)   VALUE  "099110001".
      *    特定薬剤治療管理料
           03  FILLER          PIC X(09)   VALUE  "113000410".
      *    慢性疼痛疾患管理料
           03  FILLER          PIC X(09)   VALUE  "113006510".
      *    ニコチン依存管理料（初回）
           03  FILLER          PIC X(09)   VALUE  "113008310".
      *    ニコチン依存管理料（２回から４回まで）
           03  FILLER          PIC X(09)   VALUE  "113008410".
      *
       01  TBL-IKOU-SRYCD-GR   REDEFINES   TBL-IKOU-SRYCD-G.
           03  TBL-IKOU-SRYCD          PIC X(09)
                                       OCCURS    5.
       01  TBL-IKOU-MAX                PIC 9(02)   VALUE   5.
      *
      *算定履歴作成
      *!!!! (変更時は　ORAPI021R5V3 も変更すること）
      *
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
     **    03  TBL-RRK-SRYCD-GT        OCCURS    20.
     **    03  TBL-RRK-SRYCD-GT        OCCURS    22.
           03  TBL-RRK-SRYCD-GT        OCCURS    25.
               05  TBL-RRK-SRYCD           PIC X(09).
               05  TBL-END-SRYCD           PIC X(09).
       01  TBL-RRK-MAX                 PIC 9(02)   VALUE   25.
      *
      *
       01  WRK-MIDAIS-HEN.
           03  WRK-H-SYOSIN        PIC X(9).
           03  WRK-H-FSYOSIN       PIC X(9).
           03  WRK-H-LSTYMD        PIC X(9).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    算定履歴
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"       REPLACING
                                       //SANTEI-// BY //HZN-SANTEI-//.
      *
      *    算定履歴付加情報
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    算定履歴付加情報
       01  TBL-SANTEIPLUS-AREA.
           02  TBL-SANTEIPLUS-REC      OCCURS   10.
           COPY    "CPSANTEIPLUS.INC"   REPLACING
                               //SANTEIPLUS-// BY //TBL-SANTEIPLUS-//.
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    診療科履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *   診療科履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
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
           COPY    "ORCA21SCRAREA.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-MIDAIS-HEN
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K06
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
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
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K06         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
      ****     MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  LNK-KYOUTU
      *
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "K061   "           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
      *
      *API
              WHEN    "APIV3"
      *           算定履歴一覧のみ編集 
      *           初回判定
                   PERFORM 3101-RRK-HENSYU-SEC
      *            リハビリ開始登録
                   PERFORM 3103-RRKLIST-INIT2-SEC
                   MOVE    1             TO  FLG-END
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        削除確認
               WHEN    "0101"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                削除処理
                       PERFORM 4401-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-K061-CUR
                   END-IF
      *        登録確認
               WHEN    "0102"
               WHEN    "0103"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                登録処理
                       PERFORM 4501-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-K061-CUR
                   END-IF
      *H25.9
      *        登録処理　リハコメント対応
               WHEN    "0104"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    1               TO  FLG-PLUS-DEL
                       PERFORM 4501-TOUROKU-SYORI-SEC
                   END-IF
      *H25.9
           END-EVALUATE
           .
       30010-KID1-SET-EXT.
           EXIT. 
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-K061-AREA
      *
           IF     SPA-K061-GMN         =   1
      *        移行履歴登録
      *        初回判定
               PERFORM 3101-RRK-HENSYU-SEC
      *        一覧初期表示
               PERFORM 3102-RRKLIST-INIT-SEC
           ELSE
      *        初回判定
               PERFORM 3101-RRK-HENSYU-SEC
      *        リハビリ開始登録
               PERFORM 3103-RRKLIST-INIT2-SEC
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面編集処理
      *****************************************************************
       3101-RRK-HENSYU-SEC         SECTION.
      *
      *    受診履歴の１件目
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key46"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYYMD       TO  SPA-K061-RRKYMD
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    初診算定チェック
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    "999999"            TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    ZERO                TO  WRK-CNT-SYOSIN
           MOVE    SPACE               TO  WRK-SRYYMD
           INITIALIZE                  HZN-SANTEI-REC
           PERFORM
                   UNTIL       (FLG-SANTEI     =   1    )
               ADD     1                   TO  WRK-CNT-SYOSIN
               IF      WRK-CNT-SYOSIN      =   1
      *            最終初診日
                   MOVE    SANTEI-SRYYM        TO  WRK-SRYYM
                   MOVE    ZERO                TO  WRK-SRYDD
                   PERFORM VARYING     IDX2    FROM    31  BY  -1
                           UNTIL      (IDX2    <   1    )
                       IF      SANTEI-DAY (IDX2)   >   ZERO
                           MOVE    IDX2            TO  WRK-SRYDD
                           MOVE    1               TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
               IF      SANTEI-SRYCD        =   "099110001"
                   MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    算定履歴の初診料算定日
           MOVE    WRK-SRYYMD          TO  SPA-K061-FSYOYMD
      *    最終来院日
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYKARRK        =   ZERO
               MOVE    KARRK-LASTYMD       TO  WRK-LSTYMD
           END-IF
      *
           IF      SPA-K061-RRKYMD    =   SPACE
               MOVE    SPACE               TO  WRK-H-FSYOSIN
           ELSE
               MOVE    SPA-K061-RRKYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-H-FSYOSIN
           END-IF
           IF      SPA-K061-FSYOYMD     =   SPACE
               MOVE    SPACE               TO  WRK-H-SYOSIN
           ELSE
               MOVE    SPA-K061-FSYOYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-H-SYOSIN
           END-IF
           IF      SPA-K061-RRKYMD    =   SPACE
               IF      WRK-LSTYMD          =   SPACE
                   MOVE    SPACE               TO  WRK-H-LSTYMD
               ELSE
                   MOVE    WRK-LSTYMD          TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  WRK-H-LSTYMD
               END-IF
           ELSE
               MOVE    SPACE               TO  WRK-H-LSTYMD
           END-IF
      *
           MOVE    SPACE               TO  SPA-K061-MIDASI
           MOVE    SPACE               TO  WRK-MIDASI
           IF      WRK-H-SYOSIN    NOT =   SPACE
               STRING  "【初診算定日："
                       WRK-H-SYOSIN
                       "】"            DELIMITED BY  SIZE
                                       INTO      SPA-K061-MIDASI
               END-STRING
           END-IF
           IF      WRK-H-FSYOSIN    NOT =   SPACE
               MOVE    SPA-K061-MIDASI     TO  WRK-MIDASI
               MOVE    SPACE               TO  SPA-K061-MIDASI
               STRING  WRK-MIDASI      DELIMITED  BY  "  "
                       "【受診歴初日："
                       WRK-H-FSYOSIN
                       "】"            DELIMITED  BY  SIZE
                                       INTO      SPA-K061-MIDASI
               END-STRING
           END-IF
           IF      WRK-H-LSTYMD    NOT =   SPACE
               MOVE    SPA-K061-MIDASI     TO  WRK-MIDASI
               MOVE    SPACE               TO  SPA-K061-MIDASI
               STRING  WRK-MIDASI      DELIMITED  BY  "  "
                       "【最終受診日："
                       WRK-H-LSTYMD
                       "】"            DELIMITED  BY  SIZE
                                       INTO      SPA-K061-MIDASI
               END-STRING
           END-IF
      *    初回受診日＜初診算定日は移行登録できない
           IF     (SPA-K061-RRKYMD NOT =   SPACE )
            AND   (SPA-K061-FSYOYMD    >=  SPA-K061-RRKYMD )
               MOVE    1                   TO  SPA-K061-SYORI
               MOVE    SPA-K061-MIDASI     TO  WRK-MIDASI
               MOVE    SPACE               TO  SPA-K061-MIDASI
               STRING  WRK-MIDASI      DELIMITED  BY  "  "
                       "≪移行履歴の登録はできません≫"
                                       DELIMITED  BY  SIZE
                                       INTO      SPA-K061-MIDASI
               END-STRING
           END-IF
      *    登録基準日
           IF      SPA-K061-RRKYMD     =   SPACE
               MOVE    WRK-LSTYMD          TO  SPA-K061-CHKYMD
               MOVE    WRK-LSTYMD          TO  SPA-K061-NAI-LSTYMD
               MOVE    SPACE               TO  SPA-K061-NAI-LSTYMDG
               IF      WRK-LSTYMD      NOT =   SPACE
                   MOVE    WRK-LSTYMD          TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-K061-NAI-LSTYMDG
               END-IF
           ELSE
               MOVE    SPA-K061-RRKYMD     TO  SPA-K061-CHKYMD
               MOVE    SPACE               TO  SPA-K061-NAI-LSTYMD
               MOVE    SPACE               TO  SPA-K061-NAI-LSTYMDG
           END-IF
           .
       3101-RRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面編集処理
      *****************************************************************
       3102-RRKLIST-INIT-SEC         SECTION.
      *
           INITIALIZE                      SPA-K061-INPUT-G
           INITIALIZE                      SPA-K061-SELNUM
      *
           INITIALIZE                      SPA-K061-LIST-G
           INITIALIZE                      TBL-DAT-G
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           IF      SPA-K061-RRKYMD     =   SPACE
               MOVE    SPA-SRYYMD(1:6)         TO  SANTEI-SRYYM
           ELSE
               MOVE    SPA-K061-RRKYMD(1:6)    TO  SANTEI-SRYYM
           END-IF
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
                   UNTIL       (FLG-SANTEI     =   1    )
      *        テーブル編集
               PERFORM 3101-SANTEI-HEN-SEC
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM 3102-INIT-SANTEI-SEC
      *
           IF      SPA-K061-SYORI      =   1
               MOVE    91                  TO  SPA-K061-CUR 
           ELSE
               MOVE    1                   TO  SPA-K061-CUR 
           END-IF
           .
       3102-RRKLIST-INIT-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴編集処理
      *****************************************************************
       3101-SANTEI-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   100  )
                           OR (IDX     >   ZERO )
               IF      TBL-DAT-SRYCD (IDY)  =   SPACE
                   MOVE    IDY                 TO  IDX
               ELSE
                   IF     (TBL-DAT-SRYCD (IDY)
                                               =   SANTEI-SRYCD)
                      AND (TBL-DAT-HKNCOMBI(IDY)
                                               =   SANTEI-HKNCOMBINUM)
                       MOVE    IDY                 TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *    初期コードチェック処理
           MOVE    SANTEI-SRYCD        TO  WRK-SRYCD
           PERFORM 3102-IKOUTBL-CHK-SEC
      *    算定日が受診歴初日以降は対象外
           MOVE    SANTEI-SRYYM        TO  WRK-SRYYM
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31   )
               IF      SANTEI-DAY (IDX2)   >   ZERO
                   MOVE    IDX2                TO  WRK-SRYDD
                   IF     (SPA-K061-RRKYMD    NOT =   SPACE )
                     AND  (WRK-SRYYMD          >=  SPA-K061-RRKYMD )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDX                 =   ZERO
               GO      TO      3101-SANTEI-HEN-EXT
           END-IF
           IF     (FLG-TBLOK           =   ZERO )  AND
                  (FLG-OK              =   ZERO )
               GO      TO      3101-SANTEI-HEN-EXT
           END-IF
      *
           IF      TBL-DAT-SRYCD (IDX)  NOT =   SPACE
      *        複数履歴がある場合、最後の履歴を対象
               MOVE    "1"                 TO  TBL-DAT-SYORIKBN (IDX)
           END-IF
           MOVE    SANTEI-SRYCD        TO  TBL-DAT-SRYCD (IDX)
           MOVE    SANTEI-HKNCOMBINUM  TO  TBL-DAT-HKNCOMBI (IDX)
           MOVE    SANTEI-SRYYM        TO  TBL-DAT-SRYYM (IDX)
           MOVE    SANTEI-FSANTEIYMD   TO  TBL-DAT-FSANTEIYMD (IDX)
           MOVE    SANTEI-MSANTEID     TO  TBL-DAT-MSANTEID  (IDX)
           MOVE    SANTEI-DAY-AREA     TO  TBL-DAT-DAY-G (IDX)
      *    最終日
           MOVE    TBL-DAT-SRYYM (IDX)
                                       TO  WRK-SRYYM
           MOVE    ZERO                TO  WRK-CNT
           MOVE    ZERO                TO  WRK-DD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31   )
               IF      TBL-DAT-DAY (IDX IDX2)   >   ZERO
                   MOVE    IDX2            TO  WRK-DD
                   IF     (SPA-K061-RRKYMD    NOT =   SPACE )
                     AND  (WRK-SRYYMD          >=  SPA-K061-RRKYMD )
      *                受診履歴登録日以降は、診療行為有とする
                       MOVE    "2"             TO  TBL-DAT-SYORIKBN
                                                                (IDX)
                   END-IF
      *
                   ADD     1               TO  WRK-CNT
                   IF      WRK-CNT         >   1
      *                受診履歴登録２日以上
                       MOVE    "3"             TO  TBL-DAT-SYORIKBN
                                                                (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3101-SANTEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初期コードチェック処理
      *****************************************************************
       3102-IKOUTBL-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TBLOK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-IKOU-MAX )
                         OR   (FLG-TBLOK   =   1    )
               IF      TBL-IKOU-SRYCD (IDY)
                                           =   WRK-SRYCD
                   MOVE    1               TO  FLG-TBLOK
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-RRK-MAX )
                         OR   (FLG-TBLOK   =   1    )
               IF      TBL-RRK-SRYCD (IDY)
                                           =   WRK-SRYCD
                   MOVE    1               TO  FLG-TBLOK
               END-IF
           END-PERFORM
           .
       3102-IKOUTBL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡ編集処理
      *****************************************************************
       3102-INIT-SANTEI-SEC          SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-IKOU-MAX )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   100  )
                   IF      TBL-DAT-SRYCD (IDX)
                                           =   TBL-IKOU-SRYCD (IDY)
                       MOVE    100             TO  IDX
                   ELSE
                       IF      TBL-DAT-SRYCD (IDX)  =   SPACE
      *                    算定履歴登録判定
                           MOVE    TBL-IKOU-SRYCD (IDY)    TO  WRK-SRYCD
                           PERFORM 31021-SANTEI-RRK-SEC
                           IF      FLG-SANTEI      =   1
                               MOVE    TBL-IKOU-SRYCD (IDY)
                                               TO  TBL-DAT-SRYCD(IDX)
                           END-IF
                           MOVE    100         TO  IDX
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-RRK-MAX )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   100  )
                   IF     (TBL-DAT-SRYCD (IDX)
                                           =   TBL-RRK-SRYCD (IDY))
                       MOVE    100             TO  IDX
                   ELSE
                       IF      TBL-DAT-SRYCD (IDX)  =   SPACE
      *                    算定履歴登録判定
                           MOVE    TBL-RRK-SRYCD (IDY)    TO  WRK-SRYCD
                           PERFORM 31021-SANTEI-RRK-SEC
                           IF      FLG-SANTEI      =   1
                               MOVE    TBL-RRK-SRYCD (IDY)
                                               TO  TBL-DAT-SRYCD(IDX)
                           END-IF
                           MOVE    100         TO  IDX
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    ＳＰＡ編集
           PERFORM 31021-SPATBL-HENSYU-SEC
           .
        3102-INIT-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ編集処理
      *****************************************************************
       31021-SPATBL-HENSYU-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   100  )
                         OR   (TBL-DAT-SRYCD (IDX)  =   SPACE)
      *
               ADD     1                   TO  SPA-K061-MAX
               MOVE    SPA-K061-MAX        TO  SPA-K061-TRENNUM (IDX)
               MOVE    TBL-DAT-SRYCD (IDX)
                                           TO  SPA-K061-TSRYCD (IDX)
               MOVE    TBL-DAT-SRYYM (IDX)
                                           TO  SPA-K061-TSRYYM (IDX)
               MOVE    TBL-DAT-SRYYM (IDX)
                                           TO  WRK-SRYYM
               MOVE    ZERO                TO  WRK-DD
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   31   )
                   IF      TBL-DAT-DAY (IDX IDX2)   >   ZERO
                       ADD     TBL-DAT-DAY (IDX IDX2)
                                               TO  SPA-K061-TKAISU (IDX)
                       MOVE    IDX2            TO  WRK-DD
                   END-IF
               END-PERFORM
               IF      WRK-DD              =   ZERO
                   MOVE    SPACE           TO  WRK-SRYYMD
               ELSE
                   MOVE    WRK-DD          TO  WRK-SRYDD
               END-IF
               MOVE    WRK-SRYYMD          TO  SPA-K061-TSANYMD (IDX)
               MOVE    TBL-DAT-FSANTEIYMD(IDX)
                                           TO  SPA-K061-TSYOYMD (IDX)
               IF      TBL-DAT-HKNCOMBI (IDX)   =   ZERO
                                                   OR  SPACE
                   MOVE    SPACE           TO  SPA-K061-THKNCOMBI (IDX)
               ELSE
                   MOVE    TBL-DAT-HKNCOMBI  (IDX)
                                           TO  SPA-K061-THKNCOMBI (IDX)
               END-IF
      *        初診料ダミー判定
               IF      TBL-DAT-SRYCD (IDX)  =   "099110001"
                   IF     (SPA-K061-FSYOYMD    NOT =   SPACE )  AND
                          (SPA-K061-TSANYMD (IDX)
                                               NOT =   SPA-K061-FSYOYMD)
                       MOVE    "9"         TO  TBL-DAT-SYORIKBN (IDX)
                   END-IF
               END-IF
               MOVE    TBL-DAT-SYORIKBN (IDX)
                                           TO  SPA-K061-TSYORIKBN (IDX)
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-K061-TSRYCD (IDX)   TO  TNS-SRYCD
               MOVE    SPA-K061-TSRYYM (IDX)   TO  WRK-SRYYM
      *        点数マスタ編集
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME            TO  SPA-K061-TMEISYO(IDX)
               END-IF
      *        日付編集
               IF      SPA-K061-TSANYMD (IDX)  =   SPACE
                   MOVE    SPACE               TO  SPA-K061-TSANYMDG
                                                                  (IDX)
               ELSE
                   MOVE    SPA-K061-TSANYMD (IDX)  TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-K061-TSANYMDG
                                                                  (IDX)
               END-IF
               IF      SPA-K061-TSYOYMD (IDX)  =   SPACE
                   MOVE    SPACE               TO  SPA-K061-TSYOYMDG
                                                                  (IDX)
               ELSE
                   MOVE    SPA-K061-TSYOYMD (IDX)  TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-K061-TSYOYMDG
                                                                  (IDX)
               END-IF
      *        保険組合せ
               IF      SPA-K061-THKNCOMBI (IDX)    NOT =   SPACE
                   MOVE    SPA-K061-THKNCOMBI (IDX)    TO  WRK-HKNCOMBI
                   PERFORM 31011-HKNCOMBI-SET-SEC
                   MOVE    WRK-HKNCOMBIMEI     TO  SPA-K061-THKNCOMBIMEI
                                                                  (IDX)
               END-IF
               EVALUATE    TBL-DAT-SYORIKBN (IDX)
                   WHEN    "1"
                       MOVE    "他日"          TO  SPA-K061-TKBNMEI(IDX)
                   WHEN    "2"
                       MOVE    "前歴"          TO  SPA-K061-TKBNMEI(IDX)
                   WHEN    "3"
                       MOVE    "複日"          TO  SPA-K061-TKBNMEI(IDX)
                   WHEN    "9"
                       MOVE    "不可"          TO  SPA-K061-TKBNMEI(IDX)
                   WHEN    "P"
                       MOVE    "診　"          TO  SPA-K061-TKBNMEI(IDX)
               END-EVALUATE
               MOVE    TBL-DAT-SANTEIPLUS (IDX)
                                           TO  SPA-K061-TPLUSKBN(IDX)
               IF      TBL-DAT-SANTEIPLUS (IDX)    =   "1"
                   MOVE    "コ"            TO  SPA-K061-TKBNMEI2(IDX)
               END-IF
      *        リハビリコメント終了日
               MOVE    TBL-DAT-ENDYMD(IDX)
                                           TO  SPA-K061-TENDYMD (IDX)
               IF      SPA-K061-TENDYMD (IDX)  =   SPACE
                   MOVE    SPACE               TO  SPA-K061-TENDYMDG
                                                                  (IDX)
               ELSE
                   MOVE    SPA-K061-TENDYMD (IDX)  TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-K061-TENDYMDG
                                                                  (IDX)
               END-IF
               MOVE    TBL-DAT-ENDFLG(IDX)
                                           TO  SPA-K061-TENDFLG (IDX)
      *R01.9
               IF      TBL-DAT-ENDCHK (IDX)    =   "1"
                   MOVE    "1"             TO  SPA-K061-TENDCHK(IDX)
               END-IF
           END-PERFORM
           .
       31021-SPATBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       31021-SANTEI-RRK-SEC          SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31021-SANTEI-RRK-EXT.
           EXIT.
      *****************************************************************
      *    リハビリ開始日算定履歴画面編集処理
      *****************************************************************
       3103-RRKLIST-INIT2-SEC         SECTION.
      *
           INITIALIZE                      SPA-K061-INPUT-G
           INITIALIZE                      SPA-K061-SELNUM
      *
           INITIALIZE                      SPA-K061-LIST-G
           INITIALIZE                      TBL-DAT-G
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-RRK-MAX )
      *       算定履歴登録判定
              MOVE    TBL-RRK-SRYCD (IDY)  TO  WRK-SRYCD
              PERFORM 31031-SANTEI-INIHEN-SEC
              IF      FLG-SANTEI-OK        =   ZERO
                  ADD     1                    TO  IDX
                  MOVE    TBL-RRK-SRYCD (IDY)  TO  TBL-DAT-SRYCD(IDX)
              END-IF
           END-PERFORM
      *    ＳＰＡ編集
           PERFORM 31021-SPATBL-HENSYU-SEC
      *
           MOVE    "【リハビリテーション開始日・他院退院日等履歴】"
                                       TO  SPA-K061-MIDASI
      *
           MOVE    1                   TO  SPA-K061-CUR 
           .
       3103-RRKLIST-INIT2-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴編集処理
      *****************************************************************
       31031-SANTEI-INIHEN-SEC          SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    ZERO                TO  FLG-SANTEI-OK
           PERFORM
                   UNTIL       (FLG-SANTEI     =   1    )
                          OR   (IDX            >=  100  )
      *        テーブル編集
               ADD     1                   TO  IDX
               MOVE    SANTEI-SRYCD        TO  TBL-DAT-SRYCD  (IDX)
               MOVE    SANTEI-HKNCOMBINUM  TO  TBL-DAT-HKNCOMBI
                                                               (IDX)
               MOVE    SANTEI-SRYYM        TO  TBL-DAT-SRYYM   (IDX)
               MOVE    SANTEI-FSANTEIYMD   TO  TBL-DAT-FSANTEIYMD
                                                                (IDX)
               MOVE    SANTEI-MSANTEID     TO  TBL-DAT-MSANTEID (IDX)
               MOVE    SANTEI-DAY-AREA     TO  TBL-DAT-DAY-G   (IDX)
               MOVE    1                   TO  FLG-SANTEI-OK
      *R01.9
               MOVE    ZERO                TO  IDZ
      *        テーブル編集
               IF      SANTEI-MSANTEICNT   >   ZERO
      *            診療行為有無判定
                   PERFORM 31032-SRYACT-CHK-SEC
      *            コメント登録判定
                   PERFORM 31032-SANTEIPLUS-CHK-SEC
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *!!!!
      *    リハビリ終了日算定履歴検索
           IF      TBL-END-SRYCD (IDY) =   SPACE
               GO      TO      31031-SANTEI-INIHEN-EXT
           END-IF
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    TBL-END-SRYCD (IDY) TO  SANTEI-SRYCD
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM
                   UNTIL       (FLG-SANTEI     =   1    )
      *        対象開始日判定
               MOVE    SPACE               TO  WRK-END-YMD
               MOVE    SANTEI-SRYYM        TO  WRK-END-YM
               PERFORM VARYING     IDD     FROM    31  BY  -1
                       UNTIL       IDD     <   1
      *            最終日で判定
                   IF      SANTEI-DAY (IDD)    >   ZERO
                       MOVE    IDD             TO  WRK-END-DD
                       PERFORM 31033-ENDYMD-HEN-SEC
                       MOVE    1               TO  IDD
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       31031-SANTEI-INIHEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為有無判定処理
      *****************************************************************
       31032-SRYACT-CHK-SEC          SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SANTEI-SRYYM        TO  SRY-SRYYM
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (1)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (2)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (3)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (4)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  FLG-SRY-OK
           PERFORM UNTIL      (FLG-SRYACT  =   1 )
                         OR   (FLG-SRY-OK  =   1 )
               IF      SRY-RENNUM      =   1
                   PERFORM 310321-SRYACCT-CHK-SEC
               END-IF
               IF      FLG-SRY-OK          =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key12"             TO  MCP-PATHNAME
                   PERFORM 920-SRYACT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRY-OK          =   1
               MOVE    "P"             TO  TBL-DAT-SYORIKBN (IDX)
           END-IF
           .
       31032-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       310321-SRYACCT-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYACCT         =   ZERO
               IF     (SANTEI-HKNCOMBINUM  NOT =   ZERO )  AND
                      (ACCT-HKNCOMBI           =   SANTEI-HKNCOMBINUM)
                   MOVE    1               TO  FLG-SRY-OK
               END-IF
               IF     (SANTEI-HKNCOMBINUM  =   ZERO )
      *            労災・自賠責保険以外の保険判定
                   MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI
                   PERFORM 31011-HKNCOMBI-SET-SEC
                   IF      COMB-HKNNUM         =   "971"  OR  "973"
      *             第三者行為
                    OR    (COMB-KOH1HKNNUM     =   "970"  )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-SRY-OK
                   END-IF
               END-IF
           END-IF
           .
       310321-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       31011-HKNCOMBI-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
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
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
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
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
      *            保険組合せ名は３０バイト
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HKNCOMBIMEI     TO  KANACHK-MAE-INPUT
                   MOVE    30                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNCOMBIMEI
               END-IF
      *        傷病年月日と療養開始日判定
               IF     (PTRSI-SHOBYOYMD     NOT =   SPACE  )  AND
                      (PTRSI-SHOBYOYMD         <   COMB-TEKSTYMD)
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-COMB-TEKSTYMD
               END-IF
           END-IF
           .
       31011-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント登録判定
      *****************************************************************
       31032-SANTEIPLUS-CHK-SEC           SECTION.
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
           PERFORM UNTIL   FLG-SANTEIPLUS  =   1
               IF      SANTEIPLUS-RENNUM   =   ZERO
                   MOVE    SANTEIPLUS-ENDYMD   TO  TBL-DAT-ENDYMD (IDX)
                   MOVE    "1"                 TO  TBL-DAT-ENDFLG (IDX)
                   ADD     1                   TO  IDZ
                   MOVE    SANTEIPLUS-DAYKEY   TO  TBL-DAT-DAYKEY
                                                             (IDX IDZ)
               ELSE
                   MOVE    "1"                 TO  TBL-DAT-SANTEIPLUS
                                                                (IDX)
               END-IF
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *R01.9
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL      (IDD     >   31  )
               IF      TBL-DAT-DAY (IDX IDD)   >   ZERO
                   MOVE    ZERO            TO  FLG-CHK
                   PERFORM VARYING     IDZ     FROM    1   BY  1
                           UNTIL      (IDZ     >   31  )
                                   OR (FLG-CHK     =   1   )
                       IF      TBL-DAT-DAYKEY(IDX IDZ) =   IDD
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK             =   ZERO
                       MOVE    "1"             TO  TBL-DAT-ENDCHK(IDX)
                       MOVE    31              TO  IDD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    IF      FLG-SANTEIPLUS      =   ZERO
      *        MOVE    "1"                 TO  TBL-DAT-SANTEIPLUS (IDX)
      *        IF      SANTEIPLUS-RENNUM   =   ZERO
      *            MOVE    SANTEIPLUS-ENDYMD   TO  TBL-DAT-ENDYMD (IDX)
      *            MOVE    "1"                 TO  TBL-DAT-ENDFLG (IDX)
      *        END-IF
      *    END-IF
           .
       31032-SANTEIPLUS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ終了日編集処理
      *****************************************************************
       31033-ENDYMD-HEN-SEC          SECTION.
      *
           PERFORM VARYING    IDX-C    FROM    1  BY  1
                   UNTIL     (IDX-C    >   IDX  )
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  FLG-CHK2
               IF     (TBL-RRK-SRYCD (IDY) =   TBL-DAT-SRYCD (IDX-C))
               AND    (TBL-DAT-HKNCOMBI(IDX-C) =   SANTEI-HKNCOMBINUM)
               AND    (TBL-DAT-SRYYM (IDX-C)   <=  SANTEI-SRYYM     )
                   IF     (TBL-DAT-ENDYMD(IDX-C)   =   SPACE )  OR
                         ((TBL-DAT-ENDFLG(IDX-C)   =   "1"   ) AND
                          (TBL-DAT-ENDYMD(IDX-C)   >   WRK-END-YMD ))
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-CHK2
      *R01.8
      *                複数算定日があり、終了日がないもの
                       IF    ((TBL-DAT-ENDFLG(IDX-C)   =   "1"   ) AND
                              (TBL-DAT-ENDCHK(IDX-C)   =   "1"   ))
                           MOVE    0                   TO  FLG-CHK2
                       END-IF
      *
                   END-IF
                   IF      TBL-DAT-SRYYM(IDX-C)    =   SANTEI-SRYYM
      *                同月算定の時、初回日より後であること
                       IF      TBL-DAT-MSANTEID (IDX-C) <  WRK-END-DD
                            MOVE    1              TO  FLG-CHK
                       END-IF
                   ELSE
                       MOVE    1                   TO  FLG-CHK
                   END-IF
                   IF     (FLG-CHK             =   1   )
                      AND (FLG-CHK2            =   ZERO)
                        MOVE    WRK-END-YMD    TO  TBL-DAT-ENDYMD(IDX-C)
                        MOVE    "2"            TO  TBL-DAT-ENDFLG(IDX-C)
                   END-IF
               END-IF
           END-PERFORM
           .
       31033-ENDYMD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    ZERO                TO  FLG-SYORI
                   PERFORM 210-BACK
      *     クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLERA-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 440-SAKUJYO-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
      *    リハビリコメント履歴登録
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-RIHACOMMNT-SEC
      *    算定履歴へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4100-K06-SYORI-SEC
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
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-K061-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        行選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-RENNUM-HEN-SEC
      *        行選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 41012-RRKLIST-SEC
      *        診療コード
               WHEN    "ACTIVATE"      ALSO    "SRYCD"
                   PERFORM 4102-SRYCD-CHK-SEC
               WHEN    OTHER
      *            入力チェック処理
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       41011-RENNUM-HEN-SEC         SECTION.
      *
           INITIALIZE                 SPA-K061-INPUT-G
      *
           MOVE    K061-SELNUM        TO  SPA-K061-SELNUM
      *
           IF      SPA-K061-SELNUM     =   ZERO
               MOVE    2                   TO  SPA-K061-CUR
           ELSE
               IF      SPA-K061-SELNUM     >   SPA-K061-MAX
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K061-CUR
               ELSE
                   PERFORM 41011-SELNUM-HEN-SEC
               END-IF
           END-IF
           .
       41011-RENNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       41011-SELNUM-HEN-SEC         SECTION.
      *
           MOVE    SPA-K061-TSRYCD (SPA-K061-SELNUM)
                                       TO  SPA-K061-SRYCD
           MOVE    SPA-K061-TMEISYO (SPA-K061-SELNUM)
                                       TO  SPA-K061-MEISYO
           MOVE    SPA-K061-TSANYMDG(SPA-K061-SELNUM)
                                       TO  SPA-K061-SANYMDG
           MOVE    SPA-K061-TSANYMD (SPA-K061-SELNUM)
                                       TO  SPA-K061-SANYMD
           MOVE    SPA-K061-TSYOYMDG(SPA-K061-SELNUM)
                                       TO  SPA-K061-SYOYMDG
           MOVE    SPA-K061-TSYOYMD (SPA-K061-SELNUM)
                                       TO  SPA-K061-SYOYMD
           MOVE    SPA-K061-TKAISU  (SPA-K061-SELNUM)
                                       TO  SPA-K061-KAISU
      *
           MOVE    SPA-K061-THKNCOMBI (SPA-K061-SELNUM)
                                       TO  SPA-K061-HKNCOMBI
           MOVE    SPA-K061-THKNCOMBIMEI (SPA-K061-SELNUM)
                                       TO  SPA-K061-HKNCOMBIMEI
           MOVE    SPA-K061-TSYORIKBN (SPA-K061-SELNUM)
                                       TO  SPA-K061-SYORIKBN
      *
           IF      SPA-K061-SANYMD NOT =   SPACE
               MOVE    "1"                 TO  SPA-K061-KBN
           END-IF
           MOVE    SPA-K061-SRYCD      TO  WRK-SRYCD
           MOVE    SPA-K061-TSRYYM (SPA-K061-SELNUM)
                                       TO  WRK-SRYYM
      *    点数マスタ編集
           PERFORM 40121-TENSU-HEN-SEC
           IF     (SPA-ERRCD           =   SPACE)   AND
                  (SPA-K061-KBN2       =   "2"  )
               MOVE    SPA-K061-NAI-LSTYMD     TO  SPA-K061-LSTYMD
               MOVE    SPA-K061-NAI-LSTYMDG    TO  SPA-K061-LSTYMDG
           END-IF
      *!
      *    終了日付
           IF     (SPA-K061-KBN        =   "1" )  AND
                  (SPA-K061-KBN2       =   "1" )
               MOVE    SPA-K061-TENDFLG (SPA-K061-SELNUM)
                                           TO  SPA-K061-NAI-ENDFLG
               IF      SPA-K061-TENDFLG (SPA-K061-SELNUM)  NOT =   "2"
                   MOVE    SPA-K061-TENDYMD (SPA-K061-SELNUM)
                                               TO  SPA-K061-ENDYMD
                   MOVE    SPA-K061-TENDYMDG(SPA-K061-SELNUM)
                                               TO  SPA-K061-LSTYMDG
               END-IF
           END-IF
      *
           IF      SPA-K061-KBN        =   "1"
               IF      SPA-K061-KBN2       =   "1"
                   MOVE    97                  TO  SPA-K061-CUR
               ELSE
                   MOVE    3                   TO  SPA-K061-CUR
               END-IF
           ELSE
               MOVE    3                   TO  SPA-K061-CUR
           END-IF
      *
           .
       41011-SELNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-RRKLIST-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-K061-MAX
              IF      K061-RRKLIST-SELECT (IDX)    =  "T"
                  MOVE    SPA-K061-TRENNUM (IDX)   TO  WRK-RENNUM
              END-IF
           END-PERFORM
      *
           MOVE    WRK-RENNUM          TO  K061-SELNUM
           PERFORM 41011-RENNUM-HEN-SEC
      *
           .
       41012-RRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード  入力処理
      *****************************************************************
       4102-SRYCD-CHK-SEC               SECTION.
      *
           MOVE    2                    TO  SPA-K061-CUR
      *
           MOVE    K061-SRYCD           TO  SPA-K061-SRYCD
           MOVE    SPACE                TO  SPA-K061-MEISYO
      *
           IF      SPA-K061-SRYCD       =   SPACE
               MOVE    "0002"               TO  SPA-ERRCD
           ELSE
               IF      SPA-K061-SRYCD   NOT NUMERIC
      *            数値入力以外は、検索へ
                   PERFORM 41029-KENSAKU-SYORI-SEC
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE)
               MOVE    SPA-K061-SRYCD       TO  WRK-SRYCD
               MOVE    SPA-K061-SANYMD(1:6) TO  WRK-SRYYM
      *        点数マスタ編集
               PERFORM 40121-TENSU-HEN-SEC
               IF      SPA-ERRCD           =   SPACE
      *            初診料コードはエラー
                   IF     (TNS-SRYKBN          =   "11"   )  AND
                          (TNS-SRYCD       NOT =   "099110001")
                       MOVE    "0034"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE)
               GO      TO      4102-SRYCD-CHK-EXT
           END-IF
      *    リストチェック処理
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   SPA-K061-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-K061-TSRYCD (IDX)   =   SPA-K061-SRYCD
                   MOVE    SPA-K061-TRENNUM (IDX)  TO  WRK-RENNUM
               END-IF
           END-PERFORM
           IF      SPA-K061-GMN        =   1
      *    移行履歴
               IF      WRK-RENNUM          =   ZERO
      *        初期コードチェック処理
                   MOVE    SPA-K061-SRYCD      TO  WRK-SRYCD
                   PERFORM 3102-IKOUTBL-CHK-SEC
                   IF      FLG-TBLOK           =   1
                      MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    WRK-RENNUM          TO  K061-SELNUM
                   PERFORM 41011-RENNUM-HEN-SEC
               END-IF
               IF     (SPA-ERRCD           =   SPACE)   AND
                      (SPA-K061-KBN2       =   "2"  )
                   MOVE    SPA-K061-NAI-LSTYMD     TO  SPA-K061-LSTYMD
                   MOVE    SPA-K061-NAI-LSTYMDG    TO  SPA-K061-LSTYMDG
               END-IF
           ELSE
      *        履歴
               IF      WRK-RENNUM          =   ZERO
                   MOVE    "0035"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                   TO  SPA-K061-CUR
           END-IF
           .
       4102-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       40121-TENSU-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-K061-KBN2
           INITIALIZE                      SPA-K061-SANTEIRRKKBN
           INITIALIZE                      SPA-K061-JGNCNT
           INITIALIZE                      SPA-K061-JGNCNT1D
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           IF      FLG-TENSU            =   ZERO
               MOVE    TNS-NAME         TO  SPA-K061-MEISYO
               IF      TNS-SANTEIRRKKBN     =   ZERO
                   MOVE    "0003"           TO  SPA-ERRCD
               ELSE
                   MOVE    TNS-NAME         TO  SPA-K061-MEISYO
                   MOVE    TNS-SANTEIRRKKBN TO  SPA-K061-SANTEIRRKKBN
                   MOVE    TNS-JGNCNT       TO  SPA-K061-JGNCNT
                   MOVE    TNS-JGNCNT1D     TO  SPA-K061-JGNCNT1D
                   IF      TNS-JGNCNT       =   ZERO
                       MOVE    TNS-JGNCNTETC    TO  SPA-K061-JGNCNT
                   END-IF
               END-IF
           ELSE
               MOVE    "0004"               TO  SPA-ERRCD
           END-IF
      *    初診ダミー
           IF     (WRK-SRYCD           =   "099110001"
                                       OR  "111000110"
                                       OR  "113003510"
                                       OR  "113003710"
                                       OR  "101110010"
                                       OR  "111700210"
                                       OR  "111700110"
                                       OR  "111003610"
      *                紹介状なし追加
                                       OR  "111012510"
      *                妥結率５割以下
                                       OR  "111012710"
      *H28.4           小児かかりつけ
                                       OR  "113019710"
                                       OR  "113019910"
      *R02.4 から
      *          初診料（新型コロナウイルス感染症・臨時）
                                       OR  "111013850"
                                                       )
               MOVE    "2"                 TO  SPA-K061-KBN2
           END-IF
      *    リハビリ発症日
           IF     (WRK-SRYCD           =   "099800111"
                                       OR  "099800121"
                                       OR  "099800131"
                                       OR  "099800141"
                                       OR  "099800151"
                                       OR  "099800161"
                                       OR  "099800171"
                                       OR  "099800191" )
      *      早期・初期リハビリ開始日
             OR   (WRK-SRYCD           =   "099800211"
                                       OR  "099800221"
                                       OR  "099800231"
                                       OR  "099800241"
                                       OR  "099800291")
               MOVE    "1"                 TO  SPA-K061-KBN2
           END-IF
           .
       40121-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードチェック処理
      *****************************************************************
       41029-KENSAKU-SYORI-SEC          SECTION.
      *
           IF      SPA-K061-SRYCD(1:1) =   "P"  OR  "S"
               MOVE    "0003"          TO  SPA-ERRCD
           ELSE
               PERFORM 410292-INPUTCD-CHK-SEC
           END-IF
           .
       41029-KENSAKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410292-INPUTCD-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-K061-SRYCD      TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-K061-SRYCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
               PERFORM 2030-INPUTCD-KENSAKU-SEC
           END-IF
           .
       410292-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       2030-INPUTCD-KENSAKU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-ICD-CDSYU
           IF      FLG-KANJI           =   1
      *        全角のコードチェック
               MOVE    "J"             TO  WRK-ICD-CDSYU
           ELSE
               IF     (WRK-CD-MCNT     >   1   AND  <=  9 )  AND
                      (SPA-K061-SRYCD(1:WRK-CD-MCNT)   NUMERIC    )
                    EVALUATE    WRK-CD-MCNT
      *                    短縮３桁
                           WHEN    3
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *                    ４桁
                           WHEN    4
                               MOVE    "4"         TO  WRK-ICD-CDSYU
      *                    ５桁
                           WHEN    5
                               MOVE    "5"         TO  WRK-ICD-CDSYU
      *                    ６桁
                           WHEN    6
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *
                           WHEN    OTHER
                               MOVE    "A"         TO  WRK-ICD-CDSYU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  WRK-ICD-CDSYU
                   END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-K98-SYORI
      *    入力コード存在チェック
           IF     WRK-ICD-CDSYU    NOT =   SPACE
      *
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU
               MOVE    SPA-K061-SRYCD      TO  ICD-INPUTCD
      *
               PERFORM 930-INPUTCD-READ-SEC
      *        入力コードなし
               IF      FLG-INPUTCD         =   ZERO
                   IF     (SPA-K061-SRYCD      =   ICD-INPUTCD )
                       MOVE    ICD-SRYCD           TO  SPA-K061-SRYCD
                       MOVE    1                   TO  FLG-K98-SYORI
                   END-IF
               END-IF
           END-IF
           .
       2030-INPUTCD-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    K061-SANYMD         TO  SPA-K061-SANYMDG
           MOVE    K061-SYOYMD         TO  SPA-K061-SYOYMDG
           MOVE    K061-KAISU          TO  SPA-K061-KAISU
           MOVE    K061-HKNCOMBI       TO  SPA-K061-HKNCOMBI
           MOVE    K061-LSTYMD         TO  SPA-K061-LSTYMDG
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    算定日
           PERFORM 41012-SANYMD-CHK-SEC
      *    初回算定日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-SYOYMD-CHK-SEC
           END-IF
      *    回数チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-KAISU-CHK-SEC
           END-IF
      *    保険組合せチェック
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 41012-HKNCOMBI-CHK-SEC
           END-IF
      *    最終来院日
           IF      SPA-ERRCD           =   SPACE
               IF      (SPA-K061-KBN2      =   "1" )
      *            リハビリ開始日は終了日
                   PERFORM 41012-ENDYMD-CHK-SEC
               ELSE
                   PERFORM 41012-LSTYMD-CHK-SEC
               END-IF
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日チェック処理
      *****************************************************************
       41012-SANYMD-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOYMD
           MOVE    SPACE               TO  WRK-SYOYMDG
           IF     SPA-K061-SANYMDG     =   SPACE
               MOVE    SPACE               TO  SPA-K061-SANYMD
           ELSE
               MOVE    SPA-K061-SANYMDG    TO  WRK-YMD
               MOVE    SPACE               TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-K061-SANYMD
                   MOVE    WRK-HENYMDG         TO  SPA-K061-SANYMDG
                   MOVE    SPA-K061-SANYMD     TO  WRK-SYOYMD
                   MOVE    SPA-K061-SANYMDG    TO  WRK-SYOYMDG
                   IF      SPA-K061-GMN        =   1
      *            移行履歴
                       PERFORM 41012-SANYMD-IKO-CHK-SEC
                   ELSE
      *            履歴登録
                       PERFORM 41012-SANYMD-RRK-CHK-SEC
                   END-IF
               ELSE
                   MOVE    "0008"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K061-SYOYMD     =   SPACE
      ***          MOVE    SPA-K061-SANYMD     TO  SPA-K061-SYOYMD
      ***          MOVE    SPA-K061-SANYMDG    TO  SPA-K061-SYOYMDG
                   MOVE    WRK-SYOYMD          TO  SPA-K061-SYOYMD
                   MOVE    WRK-SYOYMDG         TO  SPA-K061-SYOYMDG
               END-IF
           ELSE
               MOVE    3                   TO  SPA-K061-CUR
           END-IF
      *
           .
       41012-SANYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    移行算定日チェック処理
      *****************************************************************
       41012-SANYMD-IKO-CHK-SEC          SECTION.
      *
      *    受診日チェック
           IF     (SPA-K061-RRKYMD    NOT =   SPACE )
             AND  (SPA-K061-RRKYMD    <=   SPA-K061-SANYMD)
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
      *    初診算定は受診履歴日と同日はＯＫ
           IF     (SPA-K061-RRKYMD     NOT =   SPACE )
             AND  (SPA-K061-FSYOYMD        =   SPACE )
             AND  (SPA-K061-RRKYMD         =   SPA-K061-SANYMD)
             AND  (SPA-K061-SRYCD          =    "099110001"  )
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *    初診日チェック
           IF     (SPA-ERRCD            =   SPACE )
              AND (SPA-K061-FSYOYMD NOT =   SPACE )
              AND (SPA-K061-FSYOYMD     >   SPA-K061-SANYMD)
               MOVE    "0007"               TO  SPA-ERRCD
           END-IF
      *    初診算定は受診履歴日と同日はＯＫ
           IF     (SPA-K061-RRKYMD     =   SPACE )
             AND  (SPA-K061-KBN        =   "1"   )
             AND  (SPA-K061-FSYOYMD    =   SPA-K061-TSANYMD
                                                (SPA-K061-SELNUM) )
      ****   AND  (SPA-K061-SRYCD      =    "099110001"  )
             AND  (SPA-K061-KBN2       =   "2"     )
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *    システム日チェック
           IF     (SPA-ERRCD            =   SPACE )
              AND (SPA-K061-SANYMD      >   SPA-SYSYMD )
                MOVE    "0013"               TO  SPA-ERRCD
           END-IF
           .
       41012-SANYMD-IKO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日チェック処理
      *****************************************************************
       41012-SANYMD-RRK-CHK-SEC          SECTION.
      *
      *    システム日チェック
           IF     (SPA-ERRCD            =   SPACE )
              AND (SPA-K061-SANYMD      >   SPA-SYSYMD )
                MOVE    "0013"               TO  SPA-ERRCD
           END-IF
      *    同じコードの判定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-K061-MAX  )
               IF     (SPA-K061-SELNUM         =   ZERO  )  AND
                      (SPA-K061-TSRYCD (IDX)   =   SPA-K061-SRYCD )
                   MOVE    SPA-K061-TSYOYMD (IDX)  TO  WRK-SYOYMD
                   MOVE    SPA-K061-TSYOYMDG(IDX)  TO  WRK-SYOYMDG
               END-IF
           END-PERFORM
      *
           .
       41012-SANYMD-RRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回算定日チェック処理
      *****************************************************************
       41012-SYOYMD-CHK-SEC          SECTION.
      *
           IF     SPA-K061-SYOYMDG     =   SPACE
               MOVE    SPACE               TO  SPA-K061-SYOYMD
           ELSE
               MOVE    SPA-K061-SYOYMDG    TO  WRK-YMD
               MOVE    SPACE               TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-K061-SYOYMD
                   MOVE    WRK-HENYMDG         TO  SPA-K061-SYOYMDG
                   IF      SPA-K061-GMN        =   1
      *            移行履歴
                       PERFORM 41012-SYOYMD-IKO-CHK-SEC
                   ELSE
      *            履歴登録
                       PERFORM 41012-SYOYMD-RRK-CHK-SEC
                   END-IF
               ELSE
                   MOVE    "0011"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-K061-CUR
           END-IF
      *
           .
       41012-SYOYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    移行算定日チェック処理
      *****************************************************************
       41012-SYOYMD-IKO-CHK-SEC          SECTION.
      *
      *    受診日チェック
           IF     (SPA-K061-RRKYMD    NOT =   SPACE )
             AND  (SPA-K061-RRKYMD    <=  SPA-K061-SYOYMD)
               MOVE    "0009"               TO  SPA-ERRCD
           END-IF
      *    初診算定は受診履歴日と同日はＯＫ
           IF      (SPA-K061-RRKYMD    NOT =   SPACE )
              AND  (SPA-K061-FSYOYMD       =   SPACE )
              AND  (SPA-K061-RRKYMD        =   SPA-K061-SYOYMD)
              AND  (SPA-K061-SRYCD         =    "099110001"  )
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *    初診日チェック
      *    IF     (SPA-ERRCD           =   SPACE )
      *       AND (SPA-K061-FSYOYMD NOT =   SPACE )
      *       AND (SPA-K061-FSYOYMD     >   SPA-K061-SYOYMD)
      *        MOVE    "0010"               TO  SPA-ERRCD
      *    END-IF
      *    受診日チェック
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-K061-SYOYMD     >   SPA-K061-SANYMD)
               MOVE    "0012"               TO  SPA-ERRCD
           END-IF
      *
           .
       41012-SYOYMD-IKO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定日チェック処理
      *****************************************************************
       41012-SYOYMD-RRK-CHK-SEC          SECTION.
      *
      *    システム日チェック
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-K061-SYOYMD     >   SPA-K061-SANYMD)
               MOVE    "0012"               TO  SPA-ERRCD
           END-IF
           .
       41012-SYOYMD-RRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数処理
      *****************************************************************
       41012-KAISU-CHK-SEC                SECTION.
      *
           IF      SPA-K061-KAISU      =   ZERO
               IF      (SPA-K061-SRYCD     NOT =   SPACE )
                  AND  (SPA-K061-SANYMD    NOT =   SPACE )
                   MOVE    1               TO  SPA-K061-KAISU
               END-IF
           END-IF
           IF     (SPA-K061-JGNCNT     >   ZERO            )  AND
                  (SPA-K061-KAISU      >   SPA-K061-JGNCNT )
               IF      SPA-K061-KAISU-KEI  =   ZERO
                   MOVE    "K005"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-K061-CUR
                   MOVE    1               TO  SPA-K061-KAISU-KEI
               END-IF
           END-IF
           .
       41012-KAISU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ  入力処理
      *****************************************************************
       41012-HKNCOMBI-CHK-SEC              SECTION.
      *
           IF     (SPA-K061-HKNCOMBI   =   SPACE )
               MOVE    SPACE               TO  SPA-K061-HKNCOMBIMEI
               GO      TO      4109-HKNCOMBI-CHK-EXT
           END-IF
      *
      *    保険毎のコードの判定
           MOVE    SPA-K061-SRYCD      TO  WRK-SRYCD
           PERFORM 1000-SANTEI-CHK-SEC
      *    保険組合せがキーにない時、エラー
           IF      FLG-HKNSAN          =   ZERO
               MOVE    "0021"              TO  SPA-ERRCD
           ELSE
      *        保険組合せチェック
               MOVE    SPA-K061-HKNCOMBI   TO  WRK-HKNCOMBI
               PERFORM 31011-HKNCOMBI-SET-SEC
               IF      FLG-HKNCOMBI        =   ZERO
      *            労災の時、コメントの追加
                   IF     (COMB-HKNNUM         =   "971"
                                               OR  "973"
                                               OR  "975" )
      *              第三者行為
                      OR  (COMB-KOH1HKNNUM     =   "970" )
                       MOVE    WRK-HKNCOMBIMEI
                                               TO  SPA-K061-HKNCOMBIMEI
      *                算定日との期間チェック
                       IF     (WRK-COMB-TEKSTYMD   <=  SPA-K061-SANYMD )
                         AND  (WRK-COMB-TEKEDYMD   >=  SPA-K061-SANYMD )
                           CONTINUE
                       ELSE
                           MOVE    "0024"              TO  SPA-ERRCD
                       END-IF
                   ELSE
                       MOVE    "0023"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0022"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6                   TO  SPA-K061-CUR
           END-IF
           .
       4109-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO (TBL-SAN)    =   "H"
                       IF      SPA-K061-SANYMD(1:6)    <   "200604"
                           MOVE    ZERO            TO  FLG-HKNSAN
                       END-IF
                   END-IF
           END-SEARCH
      *    Ｈ２２年４月から追加のコードは固定で判定
      *     (COPY に追加するのは、パッケージ対応で）
           IF      WRK-SRYCD           =   "099800181"
                                       OR  "099800182"
              OR  (WRK-SRYCD(1:7)      =   "0998002"  )
      *        がん患者リハ開始日・終了日
               MOVE    1               TO  FLG-HKNSAN
           END-IF
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    最終来院日チェック処理
      *****************************************************************
       41012-LSTYMD-CHK-SEC          SECTION.
      *
           IF     SPA-K061-LSTYMDG     =   SPACE
               MOVE    SPACE               TO  SPA-K061-LSTYMD
           ELSE
               MOVE    SPA-K061-LSTYMDG    TO  WRK-YMD
               MOVE    SPACE               TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-K061-LSTYMD
                   MOVE    WRK-HENYMDG         TO  SPA-K061-LSTYMDG
                   IF      SPA-K061-SANYMD     >   SPA-K061-LSTYMD
      *            算定日以降
                       MOVE    "0015"              TO  SPA-ERRCD
                   END-IF
                   IF     (SPA-ERRCD           =   SPACE   )  AND
                          (SPA-K061-LSTYMD     >=  SPA-SYSYMD  )
      *                システム日付以降
                       MOVE    "0016"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0014"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    7                   TO  SPA-K061-CUR
           END-IF
      *
           .
       41012-LSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日付チェック処理
      *****************************************************************
       41012-ENDYMD-CHK-SEC          SECTION.
      *
           IF    (SPA-K061-LSTYMDG     =   SPACE )
               MOVE    SPACE               TO  SPA-K061-ENDYMD
           ELSE
               MOVE    SPA-K061-LSTYMDG    TO  WRK-YMD
               MOVE    SPACE               TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-K061-ENDYMD
                   MOVE    WRK-HENYMDG         TO  SPA-K061-LSTYMDG
                   IF      SPA-K061-SANYMD     >   SPA-K061-ENDYMD
      *            算定日以降
                       MOVE    "0018"              TO  SPA-ERRCD
                   END-IF
      *
                   IF     (SPA-ERRCD           =   SPACE )
                   AND    (SPA-K061-TENDFLG (SPA-K061-SELNUM)  =   "2")
                       IF     SPA-K061-TENDYMD (SPA-K061-SELNUM)
                                               <   SPA-K061-ENDYMD
      *                    リハビリ終了日以降
                           MOVE    "0019"          TO  SPA-ERRCD
                       END-IF
                   END-IF
                   IF     (SPA-ERRCD           =   SPACE )
                   AND    (SPA-K061-ENDYMD     <   "20100401")
                       MOVE    "0020"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0017"               TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    7                   TO  SPA-K061-CUR
           END-IF
      *
           .
       41012-ENDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "K06"               TO  SPA-SAKIPG
      ***  MOVE    SPA-K061-MOTOPG     TO  SPA-SAKIPG
           MOVE    "K061"              TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       420-CLERA-SEC                SECTION.
      *
           IF      SPA-K061-SELNUM     =   ZERO
               INITIALIZE              SPA-K061-INPUT-G
           ELSE
               INITIALIZE              SPA-K061-INPUT-G
               MOVE    ZERO            TO  SPA-K061-SELNUM
           END-IF
           MOVE    1                   TO  SPA-K061-CUR
           .
       420-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       440-SAKUJYO-SEC                SECTION.
      *
           IF      SPA-K061-SYORIKBN   =   "P"
                                       OR  "2"
               MOVE    "0040"              TO  SPA-ERRCD
           END-IF
           IF     (SPA-K061-SELNUM     >   ZERO )
              AND (SPA-ERRCD           =   SPACE )
              AND (SPA-K061-KEIFLG     =   ZERO  )
      *        初診料削除で、最終来院日も削除される
               IF     (SPA-K061-KBN2       =   "2"         )
                  AND (SPA-K061-NAI-LSTYMD NOT =   SPACE   )
                   MOVE    "K041"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K061-KEIFLG
               END-IF
           END-IF
           IF     (SPA-K061-SELNUM     >   ZERO )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "0101"          TO  SPA-KIDCD
           END-IF
           .
       440-SAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *    削除　処理
      *****************************************************************
       4401-DELETE-SYORI-SEC                SECTION.
      *
      *    削除処理
           PERFORM 4501-SANTEI-DEL-SEC
      *    初診ダミーは診療科履歴更新
      ***  IF      SPA-K061-SRYCD      =   "099110001"
           IF     (SPA-K061-KBN2       =   "2"         )
               MOVE    SPA-K061-SANYMD(1:6)    TO  WRK-SRYYM
               PERFORM 4510-KARRK-UPD-SEC
           END-IF
      *    リハビリのコメント削除
           IF      SPA-ERRCD           =   SPACE
               PERFORM 45011-SANTEIPLUS-DEL-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        一覧初期表示
               PERFORM 310-SPASET-SEC
           END-IF
           .
       4401-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
           IF      SPA-K061-SRYCD      =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K061-CUR
           END-IF
           IF      SPA-K061-SYORIKBN   =   "P"
                                       OR  "2"
      *!!
               IF      (SPA-K061-KBN       =   "1" )
                  AND  (SPA-K061-KBN2      =   "1" )
                  AND  (SPA-K061-TKAISU  (SPA-K061-SELNUM) =   1   )
                   CONTINUE
               ELSE
                   MOVE    "0041"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K061-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-K061-SRYCD       TO  WRK-SRYCD
               MOVE    SPA-K061-SANYMD(1:6) TO  WRK-SRYYM
      *        点数マスタ編集
               PERFORM 40121-TENSU-HEN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  FLG-TOUROKU
               PERFORM 410-INPUT-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4501-KANREN-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               MOVE    "0102"          TO  SPA-KIDCD
               IF      SPA-K061-SYORIKBN   =   "P"
                   MOVE    "0103"          TO  SPA-KIDCD
               END-IF
           END-IF
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴登録処理
      *****************************************************************
       4501-TOUROKU-SYORI-SEC                SECTION.
      *
           IF      SPA-K061-SYORIKBN   =   "P"
      *        診療行為登録分は、リハビリ終了日のみ更新
               PERFORM 4502-SANTEIPLUS-ADD-SEC
               IF      SPA-ERRCD           =   SPACE
      *            一覧初期表示
                   PERFORM 310-SPASET-SEC
               END-IF
               GO      TO      450-TOUROKU-EXT
           END-IF
      *
      *    登録処理
           IF      SPA-K061-SELNUM     >   ZERO
      *        削除処理
               PERFORM 4501-SANTEI-DEL-SEC
      *        リハビリのコメント削除
               IF     (SPA-ERRCD           =   SPACE )
      ***      AND    (SANTEI-SRYYM    NOT =   SPA-K061-SANYMD(1:6))
                   IF      FLG-PLUS-DEL        =   1
      *                リハビリのコメント削除
                       PERFORM 45011-SANTEIPLUS-DEL-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-ERRCD           =   SPACE
      *        登録処理
               PERFORM 4502-SANTEI-ADD-SEC
           END-IF
      *ver.4.7
           IF     (SPA-ERRCD           =   SPACE )
      *        終了日付登録処理
           AND    (SPA-K061-KBN         =   "1"  )
           AND    (SPA-K061-KBN2        =   "1"  )
               PERFORM 4502-SANTEIPLUS-ADD-SEC
           END-IF
      *
      *    初診ダミーは診療科履歴更新
           IF      SPA-K061-KBN2       =   "2"
               MOVE    SPA-K061-SANYMD(1:6)    TO  WRK-SRYYM
               PERFORM 4510-KARRK-UPD-SEC
               IF     (SPA-K061-LSTYMD     NOT =   SPACE )
      *            最終来院日更新
                   PERFORM 4520-KARRK-UPD2-SEC
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    SPA-K061-LSTYMD TO  SPA-K061-NAI-LSTYMD
                       MOVE    SPA-K061-LSTYMDG
                                               TO  SPA-K061-NAI-LSTYMDG
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        一覧初期表示
               PERFORM 310-SPASET-SEC
           END-IF
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前関連チェック処理
      *****************************************************************
       4501-KANREN-CHK-SEC                SECTION.
      *
           IF      (SPA-K061-SRYCD     NOT =   SPACE )
              AND  (SPA-K061-SANYMD    NOT =   SPACE )
              AND  (SPA-K061-SYOYMD    NOT =   SPACE )
              AND  (SPA-K061-KAISU     NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K061-CUR
           END-IF
      *
           IF      SPA-K061-GMN        =   1
      *        移行履歴
               PERFORM 4501-KANREN-IKO-CHK-SEC
           ELSE
               PERFORM 4501-KANREN-RRK-CHK-SEC
           END-IF
      *
           .
       4501-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    移行履歴関連チェック処理
      *****************************************************************
       4501-KANREN-IKO-CHK-SEC             SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K061-SRYCD      TO  SANTEI-SRYCD
           IF      SPA-K061-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           IF      (SPA-K061-KBN       =   SPACE )  AND
                   (FLG-SANTEI         =   ZERO  )
      *        新規登録で算定履歴登録済みは、エラー
               MOVE    "0032"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K061-CUR
           END-IF
      *
           MOVE    ZERO                TO  WRK-CNT
           PERFORM
                   UNTIL       (FLG-SANTEI     =   1    )
                           OR  (SPA-ERRCD  NOT =   SPACE)
      *        更新の時、１件のみ登録であること
               IF     (SPA-K061-CHKYMD     =   SPACE )  OR
                      (SANTEI-SRYYM        <=  SPA-K061-CHKYMD(1:6))
                   ADD     1               TO  WRK-CNT
                   IF      WRK-CNT         >   1
                       MOVE    "0033"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-K061-CUR
                   END-IF
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4501-KANREN-IKO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴登録関連チェック処理
      *****************************************************************
       4501-KANREN-RRK-CHK-SEC             SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K061-SRYCD      TO  SANTEI-SRYCD
           MOVE    SPA-K061-SANYMD(1:6)
                                       TO  SANTEI-SRYYM
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      SPA-K061-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           END-IF
      *
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
           IF      (FLG-SANTEI         =   ZERO  )
               IF     (SPA-K061-KBN       =   SPACE )
      *        新規登録で算定履歴登録済みは、エラー
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-K061-CUR
               END-IF
           END-IF
      *
      *R01.8
      *    終了日が開始日で登録されていないこと
           IF      SPA-ERRCD           =   SPACE
               PERFORM 45012-KANREN-END-CHK-SEC
           END-IF
      *
      *H25.9
           IF    (SPA-ERRCD            =   SPACE   )
            AND  (SPA-K061-SYORIKBN    NOT =   "P" )
            AND  (SPA-K061-SELNUM     >   ZERO )
      *        更新チェック
               PERFORM 45011-KANREN-UPDCHK-SEC
           END-IF
      *
           .
       4501-KANREN-RRK-CHK-EXT.
           EXIT.
      *H25.9
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       45011-KANREN-UPDCHK-SEC             SECTION.
      *
           MOVE    SPA-K061-SELNUM     TO  IDX
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K061-TSRYCD(IDX)
                                       TO  SANTEI-SRYCD
           MOVE    SPA-K061-TSRYYM(IDX)
                                       TO  SANTEI-SRYYM
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      SPA-K061-THKNCOMBI(IDX) =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-THKNCOMBI (IDX)
                                           TO  SANTEI-HKNCOMBINUM
           END-IF
      *
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
      *    複数日算定は更新不可とする
           MOVE    ZERO                TO  WRK-DAY
           PERFORM VARYING    IDD      FROM    1   BY  1
                   UNTIL     (IDD      >   31  )
                        OR   (SPA-ERRCD    NOT =   SPACE)
              IF      SANTEI-DAY (IDD)     >   ZERO
                   IF      WRK-DAY         >   ZERO
                       MOVE    "0050"          TO  SPA-ERRCD
                       MOVE    2               TO  SPA-K061-CUR
                   END-IF
                   MOVE    IDD             TO  WRK-DAY
               END-IF
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE     )
           AND   ( SPA-K061-TPLUSKBN (SPA-K061-SELNUM)
                                       =   "1"       )
      *        リハビリコメント有
               IF    ((SANTEI-SRYYM    =   SPA-K061-SANYMD(1:6))
                  AND (WRK-DAY         =   SPA-K061-SANYMD(7:2)))
               AND   ( SPA-K061-THKNCOMBIMEI (SPA-K061-SELNUM)
                                       =   SPA-K061-HKNCOMBI   )
                   CONTINUE
               ELSE
      *            算定日・保険組合せ変更時、確認メッセージ表示
                   MOVE    "0104"          TO  SPA-KIDCD
               END-IF
           END-IF
           .
       45011-KANREN-UPDCHK-EXT.
           EXIT.
      *H25.9
      *
      *R01.8
      *****************************************************************
      *    終了日チェック処理
      *****************************************************************
       45012-KANREN-END-CHK-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-END-SRYCD
           MOVE    ZERO                TO  FLG-TBLOK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-RRK-MAX )
                         OR   (FLG-TBLOK   =   1       )
               IF      TBL-RRK-SRYCD (IDY)
                                           =   SPA-K061-SRYCD
                   MOVE    TBL-END-SRYCD (IDY)
                                           TO  WRK-END-SRYCD
                   MOVE    1               TO  FLG-TBLOK
               END-IF
           END-PERFORM
      *
           IF      WRK-END-SRYCD       =   SPACE
               GO  TO  45012-KANREN-END-CHK-EXT
           END-IF
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-END-SRYCD       TO  SANTEI-SRYCD
           MOVE    SPA-K061-SANYMD(1:6)
                                       TO  SANTEI-SRYYM
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      SPA-K061-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           END-IF
      *
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
           IF      (FLG-SANTEI         =   ZERO  )
               MOVE    SPA-K061-SANYMD(7:2)    TO  IDD
               IF      SANTEI-DAY (IDD)        >   ZERO
                   MOVE    "0051"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-K061-CUR
               END-IF
           END-IF
           .
       45012-KANREN-END-CHK-EXT.
           EXIT.
      *!
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       4501-SANTEI-DEL-SEC             SECTION.
      *
           MOVE    SPA-K061-SELNUM     TO  IDX
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K061-TSRYCD(IDX)
                                       TO  SANTEI-SRYCD
           MOVE    SPA-K061-TSRYYM(IDX)
                                       TO  SANTEI-SRYYM
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      SPA-K061-THKNCOMBI(IDX) =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-THKNCOMBI (IDX)
                                           TO  SANTEI-HKNCOMBINUM
           END-IF
      *
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
           MOVE    SPACE               TO  HZN-SANTEI-REC
           IF      FLG-SANTEI          =   ZERO
      *!!!
               MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"                  TO  SPA-ERRCD
                   DISPLAY "K061 DEL SANTEI-KEY =>" SANTEI-KEY 
               END-IF
           END-IF
      *
           .
       4501-SANTEI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除　処理
      *****************************************************************
       45011-SANTEIPLUS-DEL-SEC                SECTION.
      *
           MOVE    SPA-K061-SELNUM     TO  IDX
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    SPA-K061-TSRYCD(IDX)
                                       TO  SANTEIPLUS-SRYCD
           MOVE    SPA-K061-TSRYYM(IDX)
                                       TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           IF      SPA-K061-THKNCOMBI(IDX) =   SPACE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-THKNCOMBI (IDX)
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
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "K061 DEL SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
           END-IF
      *
          .
       45011-SANTEIPLUS-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       4502-SANTEI-ADD-SEC                SECTION.
      *
      *    更新日取得
      *    INITIALIZE                  ORCSMCNDATEAREA
      *    CALL    "ORCSMCNDATE"       USING
      *                                ORCSMCNDATEAREA
      *
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K061-SANYMD(1:6)
                                       TO  SANTEI-SRYYM
           MOVE    SPA-K061-SRYCD      TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      SPA-K061-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           END-IF
           MOVE    SPA-K061-SYOYMD     TO  SANTEI-FSANTEIYMD
           MOVE    SPA-K061-SANYMD (7:2)
                                       TO  SANTEI-MSANTEID
           MOVE    SPA-K061-KAISU      TO  SANTEI-MSANTEICNT
           MOVE    SANTEI-MSANTEID     TO  IDX2
           MOVE    SPA-K061-KAISU      TO  SANTEI-DAY (IDX2)
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
           MOVE    SPA-OPID            TO  SANTEI-OPID
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "K061 INS SANTEI-KEY =>" SANTEI-KEY 
           END-IF
      *
          .
       4502-SANTEI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       4502-SANTEIPLUS-ADD-SEC                SECTION.
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K061-SANYMD(1:6)
                                       TO  SANTEI-SRYYM
           MOVE    SPA-K061-SRYCD      TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      SPA-K061-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K061-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           END-IF
      *
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
           IF      FLG-SANTEI          =   1
               MOVE    "XXXX"              TO  SPA-ERRCD
               GO      TO      4502-SANTEIPLUS-ADD-EXT
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
           MOVE    SANTEI-MSANTEID     TO  SANTEIPLUS-DAYKEY
           MOVE    ZERO                TO  SANTEIPLUS-RENNUM
      *    算定履歴付加情報読込（キー）
           PERFORM 9010-SANTEIPLUS-KEY-SEC
           IF      FLG-SANTEIPLUS      =   ZERO
               IF      SPA-K061-ENDYMD     =   SPACE
      *            削除
                   PERFORM 45023-SANTEIPLUS-END-DEL-SEC
               ELSE
      *            更新
                   IF      SANTEIPLUS-ENDYMD   NOT =   SPA-K061-ENDYMD
                       PERFORM 45022-SANTEIPLUS-END-UPD-SEC
                   END-IF
               END-IF
           ELSE
      *        新規
               IF      SPA-K061-ENDYMD NOT =   SPACE
                   PERFORM 45021-SANTEIPLUS-END-INS-SEC
               END-IF
           END-IF
      *
          .
       4502-SANTEIPLUS-ADD-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴付加情報登録処理
      *****************************************************************
       45021-SANTEIPLUS-END-INS-SEC         SECTION.
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SANTEI-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    SANTEI-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYCD        TO  SANTEIPLUS-SRYCD
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    SANTEI-SRYKA        TO  SANTEIPLUS-SRYKA
           MOVE    SANTEI-NYUGAIKBN    TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SANTEI-HKNCOMBINUM  TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    SANTEI-MSANTEID     TO  SANTEIPLUS-DAYKEY
           MOVE    ZERO                TO  SANTEIPLUS-RENNUM
           MOVE    SPA-K061-ENDYMD     TO  SANTEIPLUS-ENDYMD
      *
           MOVE    SPA-OPID            TO  SANTEIPLUS-OPID
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
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "K061 INS SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
           END-IF
           .
       45021-SANTEIPLUS-END-INS-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴付加情報更新処理
      *****************************************************************
       45022-SANTEIPLUS-END-UPD-SEC         SECTION.
      *
           MOVE    SPA-K061-ENDYMD     TO  SANTEIPLUS-ENDYMD
      *
           MOVE    SPA-OPID            TO  SANTEIPLUS-OPID
           MOVE    SMCNDATE-YMD        TO  SANTEIPLUS-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEIPLUS-UPHMS
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "K061 UPD SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
           END-IF
          .
       45022-SANTEIPLUS-END-UPD-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴付加情報削除処理
      *****************************************************************
       45023-SANTEIPLUS-END-DEL-SEC         SECTION.
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "K061 DEL SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
           END-IF
          .
       45023-SANTEIPLUS-END-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴の初診算定日修正処理
      *****************************************************************
       4510-KARRK-UPD-SEC             SECTION.
      *
      *    更新前の算定日
           MOVE    SPACE               TO  WRK-SYO-YMD
           IF      HZN-SANTEI-SRYYM    NOT =   SPACE
               MOVE    HZN-SANTEI-SRYYM    TO  WRK-SYO-YM
               MOVE    ZERO                TO  WRK-SYO-DD
               PERFORM VARYING     IDX2    FROM    31  BY  -1
                       UNTIL      (IDX2    <   1    )
                   IF      HZN-SANTEI-DAY(IDX2)    >   ZERO
                       MOVE    IDX2            TO  WRK-SYO-DD
                       MOVE    1               TO  IDX2
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    SPACE               TO  WRK-SRYKA
           MOVE    SPACE               TO  WRK-SRYKA-IN
           MOVE    ZERO                TO  FLG-KARRK-ARI
           PERFORM
                   UNTIL   FLG-SRYKARRK    =   1
               IF      KARRK-SRYKA         =   SPA-SRYKA
                   MOVE    1                   TO  FLG-KARRK-ARI
               ELSE
                   MOVE    KARRK-SRYKA         TO  WRK-SRYKA
               END-IF
               IF     (WRK-SYO-YMD     NOT =   SPACE       )
               AND    (KARRK-SYOSINYMD2    =   WRK-SYO-YMD )
      *            初診算定
                   MOVE    KARRK-SRYKA         TO  WRK-SRYKA-IN
               END-IF
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  ORCSKARRKAREA
           MOVE    WRK-SRYYM           TO  SKARRK-SRYYM
      *
           MOVE    SPA-SRYKA           TO  SKARRK-SRYKA
           IF      WRK-SRYKA-IN    NOT =   SPACE
      *        初診ダミーの算定日の診療科
               MOVE    WRK-SRYKA-IN    TO  SKARRK-SRYKA
           ELSE
      *        SPA-SRYKA の診療科履歴が存在しない時
               IF     (WRK-SRYKA       NOT =   SPACE )
                 AND  (FLG-KARRK-ARI       =   ZERO  )
                   MOVE    WRK-SRYKA       TO  SKARRK-SRYKA
               END-IF
           END-IF
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA.
      *
           IF      SKARRK-ERRMSG   NOT =   SPACE
               MOVE    "1004"              TO  SPA-ERRCD
           END-IF
      *
           .
       4510-KARRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    診療科履歴の最終来院日修正処理
      *****************************************************************
       4520-KARRK-UPD2-SEC            SECTION.
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           PERFORM
                   UNTIL   FLG-SRYKARRK    =   1
               IF     (KARRK-SYOSINYMD1    =   SPA-K061-SANYMD )
      *
                   MOVE    SPA-K061-LSTYMD     TO  KARRK-LASTYMD
                   MOVE    SRYKARRK-REC        TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_srykarrk"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4520-KARRK-UPD2-EXT.
           EXIT.
      *****************************************************************
      *    リハビリコメント履歴登録　処理
      *****************************************************************
       480-RIHACOMMNT-SEC             SECTION.
      *
           IF     (SPA-K061-KBN         =   "1"  )
           AND    (SPA-K061-KBN2        =   "1"  )
      *        リハビリ開始日で登録済みの時
               PERFORM 4901-K062-SYORI-SEC
           END-IF
           .
       480-RIHACOMMNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリコメント履歴登録　処理
      *****************************************************************
       4901-K062-SYORI-SEC             SECTION.
      *
           INITIALIZE                  SPA-K062-GMN-G
           INITIALIZE                  SPA-K062-AREA
      *
           MOVE    SPA-K061-SRYCD      TO  SPA-K062-SRYCD
           MOVE    SPA-K061-TSRYYM(SPA-K061-SELNUM)
                                       TO  SPA-K062-SRYYM
           MOVE    SPA-K061-THKNCOMBI(SPA-K061-SELNUM)
                                       TO  SPA-K062-HKNCOMBI
      *    リハビリ終了日算定日
           IF     (SPA-K061-TENDFLG (SPA-K061-SELNUM)  =   "2")
               MOVE    SPA-K061-TENDYMD (SPA-K061-SELNUM)
                                           TO  SPA-K062-RRKENDYMD
               MOVE    "リハビリ終了日"    TO  SPA-K062-MIDASI2
               MOVE    SPA-K061-TENDYMDG(SPA-K061-SELNUM)
                                           TO  SPA-K062-MSGENDYMD
           END-IF
           MOVE    "K061"              TO  SPA-K062-MOTOPG
      *
      *    初期画面セット
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK062"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K06
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    "K062"              TO  SPA-SAKIPG
           MOVE    "K061"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      **** MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K062"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4901-K062-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月の算定履歴月へ処理
      *****************************************************************
       4100-K06-SYORI-SEC               SECTION.
      *
      *    算定履歴画面へ
           IF     (SPA-K061-SELNUM     >   ZERO      AND
                                       <=  SPA-K061-MAX )   AND
                  (SPA-K061-TSRYYM (SPA-K061-SELNUM)
                                   NOT =   SPACE        )
      *        選択中の診療年月の履歴を表示
               MOVE    SPA-K061-TSRYYM (SPA-K061-SELNUM)
                                           TO  SPA-NAI-SRYYM
               MOVE    SPA-K061-TSANYMDG (SPA-K061-SELNUM)(1:6)
                                           TO  SPA-GMN-SRYYM
      *
               PERFORM 210-BACK
           ELSE
               MOVE    "0036"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K061-CUR
           END-IF
           .
       4100-K06-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-INTYPE          TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
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
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
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
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      **** MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K061"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  K061
           INITIALIZE                  K061
      *
           MOVE    SPA-K061-MIDASI     TO  K061-MIDASI-VALUE
           MOVE    "black"             TO  K061-MIDASI-STYLE
           IF      SPA-K061-SYORI      =   1
               MOVE    "red"               TO  K061-MIDASI-STYLE
           END-IF
           IF      SPA-K061-GMN        =   2
               MOVE    "blue"              TO  K061-MIDASI-STYLE
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K061-MAX
               MOVE    SPA-K061-TRENNUM (IDX)  TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K061-TNUM      (IDX)
               MOVE    SPA-K061-TSRYCD (IDX)   TO  K061-TSRYCD    (IDX)
               MOVE    SPA-K061-TMEISYO (IDX)  TO  K061-TMEISYO   (IDX)
               MOVE    SPA-K061-THKNCOMBIMEI(IDX)
                                               TO  K061-THKNCOMBI (IDX)
      *H25.7
      *        保険組合せ名は20バイト（第三者対応）
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                     TO  KANACHK-SYORI
               MOVE    SPA-K061-THKNCOMBIMEI(IDX)
                                               TO  KANACHK-MAE-INPUT
               MOVE    20                      TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"           USING
                                               ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT       TO  K061-THKNCOMBI (IDX)
      *
               MOVE    SPA-K061-TSANYMDG (IDX) TO  K061-TSANYMD   (IDX)
               MOVE    SPA-K061-TSYOYMDG (IDX) TO  K061-TSYOYMD   (IDX)
               MOVE    SPA-K061-TKAISU   (IDX) TO  WRK-Z9
               MOVE    WRK-Z9                  TO  K061-TKAISU(IDX)
               MOVE    SPA-K061-TKBNMEI  (IDX) TO  K061-TKUBUN    (IDX)
      *ver.4.7
               MOVE    SPA-K061-TENDYMDG (IDX) TO  K061-TENDYMD   (IDX)
               IF      SPA-K061-TENDFLG (IDX)  =   "2"
                   MOVE    "（診）"            TO  K061-TENDYMD (IDX)
                                                                  (10:)
               END-IF
      *R01.8   終了日なしあり
               IF     (SPA-K061-TENDFLG (IDX)  =   "1" )
                  AND (SPA-K061-TENDCHK (IDX)  =   "1" )
                   MOVE    "（※）"            TO  K061-TENDYMD (IDX)
                                                                  (10:)
               END-IF
               IF      IDX               =   SPA-K061-SELNUM
                   MOVE    "T"           TO  K061-RRKLIST-SELECT(IDX)
               ELSE
                   MOVE    "F"           TO  K061-RRKLIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K061-MAX          TO  K061-RRKLIST-COUNT
      *
           MOVE    SPA-K061-SELNUM        TO  K061-SELNUM
           MOVE    SPA-K061-SRYCD         TO  K061-SRYCD
           MOVE    SPA-K061-MEISYO        TO  K061-SRYMEI
           MOVE    SPA-K061-SANYMDG       TO  K061-SANYMD
           MOVE    SPA-K061-SYOYMDG       TO  K061-SYOYMD
           MOVE    SPA-K061-KAISU         TO  K061-KAISU
           MOVE    SPA-K061-HKNCOMBI      TO  K061-HKNCOMBI
           MOVE    SPA-K061-HKNCOMBIMEI   TO  K061-HKNCOMBIMEI
           MOVE    SPA-K061-LSTYMDG       TO  K061-LSTYMD
      *
           IF      SPA-K061-KBN        =   SPACE
               MOVE    WIDGET-NORMAL       TO  K061-SRYCD-STATE
               MOVE    WIDGET-NORMAL       TO  K061-SANYMD-STATE
               MOVE    WIDGET-NORMAL       TO  K061-SYOYMD-STATE
               MOVE    WIDGET-NORMAL       TO  K061-KAISU-STATE
               MOVE    WIDGET-NORMAL       TO  K061-HKNCOMBI-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K061-SRYCD-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K061-SANYMD-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K061-SYOYMD-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K061-KAISU-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K061-HKNCOMBI-STATE
           END-IF
           MOVE    WIDGET-NORMAL       TO  K061-SANYMD-STATE
           MOVE    WIDGET-NORMAL       TO  K061-SYOYMD-STATE
           MOVE    WIDGET-NORMAL       TO  K061-KAISU-STATE
           MOVE    WIDGET-NORMAL       TO  K061-HKNCOMBI-STATE
      *
           IF     (SPA-K061-KBN        =   "1" )
             AND  (SPA-K061-KBN2       =   "1" )
               MOVE    WIDGET-NORMAL       TO  K061-B08-STATE
               MOVE    "F8 コメント"       TO  K061-B08-LABEL
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K061-B08-STATE
               MOVE    SPACE               TO  K061-B08-LABEL
           END-IF
      *
           IF     (SPA-K061-KBN2       =   "2" )
             AND  (SPA-K061-RRKYMD     =   SPACE )
               MOVE    WIDGET-NORMAL       TO  K061-LSTYMD-STATE
               MOVE    "最終来院日"        TO  K061-MIDASI2
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K061-LSTYMD-STATE
               MOVE    SPACE               TO  K061-MIDASI2
           END-IF
      *
           IF     (SPA-K061-KBN         =   "1"  )
           AND    (SPA-K061-KBN2        =   "1"  )
      *        リハビリ開始日で登録済みの時
               IF      SPA-K061-TKAISU  (SPA-K061-SELNUM)  =   1
                   MOVE    WIDGET-NORMAL       TO  K061-LSTYMD-STATE
               END-IF
               MOVE    "　終了日付"        TO  K061-MIDASI2
           END-IF
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
           IF      SPA-K061-CUR        =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-K061-CUR
               WHEN    01
                   MOVE  "SELNUM"            TO  MCP-WIDGET
               WHEN    02
                   MOVE  "SRYCD"             TO  MCP-WIDGET
               WHEN    03
                   MOVE  "SANYMD"            TO  MCP-WIDGET
               WHEN    04
                   MOVE  "SYOYMD"            TO  MCP-WIDGET
               WHEN    05
                   MOVE  "KAISU"             TO  MCP-WIDGET
               WHEN    06
                   MOVE  "HKNCOMBI"          TO  MCP-WIDGET
               WHEN    07
                   MOVE  "LSTYMD"            TO  MCP-WIDGET
               WHEN    91
                   MOVE  "B01"               TO  MCP-WIDGET
               WHEN    97
                   MOVE  "B08"               TO  MCP-WIDGET
               WHEN    98
                   MOVE  "B03"               TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SRYCD"
                   MOVE    03          TO  SPA-K061-CUR
               WHEN    "SANYMD"
                   MOVE    04          TO  SPA-K061-CUR
               WHEN    "SYOYMD"
                   MOVE    05          TO  SPA-K061-CUR
               WHEN    "KAISU"
                   MOVE    06          TO  SPA-K061-CUR
                   IF      SPA-K061-KBN     =   "1"
                       MOVE    99          TO  SPA-K061-CUR
                   END-IF
               WHEN    "HKNCOMBI"
                   IF     (SPA-K061-KBN2       =   "2"   )
                     AND  (SPA-K061-RRKYMD     =   SPACE )
                       MOVE    07          TO  SPA-K061-CUR
                   ELSE
                       MOVE    99          TO  SPA-K061-CUR
                   END-IF
               WHEN    "LSTYMD"
                   MOVE    99          TO  SPA-K061-CUR
           END-EVALUATE
           .
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
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号がありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "診療コードを入力して下さい"
                                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "算定履歴の算定できない診療です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "点数マスタに存在しません。"
                                                   TO  SPA-ERRMSG
               WHEN    "K005"
                   MOVE    "警告！！上限回数以上です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0006"
                   STRING  "算定日は受診履歴初日より前日で入力して"
                           "下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0007"
                   STRING  "算定日が初診算定日より前です。"
                           "初診算定日以降で入力して下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0008"
                   MOVE    "算定日暦日エラーです。"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   STRING  "初回算定日は受診履歴初日より前日で入力して"
                           "下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "初回算定日が初診算定日より前です。"
                           "初診算定日以降で入力して下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   MOVE    "初回算定日暦日エラーです。"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   STRING  "初回算定日≦算定日で入力して下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "算定日＜システム日付で入力して下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0014"
                   MOVE    "最終来院日暦日エラーです。"
                                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "算定日≦最終来院日で入力して下さい。"
                                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "最終来院日＜システム日付で入力して下さい。"
                                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "終了日付暦日エラーです。"
                                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "算定日≦終了日付で入力して下さい。"
                                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   STRING  "リハビリテーション終了日が算定済みです。"
                           "終了日付は算定日より前の日付のみ有効です。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0020"
                   STRING  "終了日付はＨ２２年４月１日以降として下さい"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0021"
                   MOVE    "保険組合せは入力できません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "保険組合せがありません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "保険組合せが健保です。入力できません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0024"
                   STRING  "保険組合せの適用期間外です。"
                           "算定日を変更して下さい。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0031"
                   MOVE    "未入力項目があります。"
                                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   STRING  "既に算定履歴に登録済みのコードです。"
                           "登録はできません。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0033"
                   STRING  "算定履歴が複数登録されています。"
                           "算定履歴画面で変更して下さい。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0034"
                   STRING  "初診料は初診料ダミーコードで登録して下さい"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0035"
                   STRING  "リハビリ開始日・他院退院日・"
                           "精神科ケア開始日のみ"
                           "登録できます。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0036"
                   STRING  "算定日が登録されていません。"
                           "算定履歴画面へ遷移できません。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0040"
                   STRING  "診療行為から登録した算定履歴です。"
                           "削除はできません。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0041"
                   STRING  "診療行為から登録した算定履歴です。"
                           "登録はできません。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K041"
                   STRING  "警告！！"
                           "初診料の削除で、最終来院日も削除されます。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *H25.9
               WHEN    "0050"
                   STRING  "算定日が複数日登録されています。"
                           "この画面で更新はできません。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *R01.8
               WHEN    "0051"
                   STRING  "算定日でリハビリ終了日が登録されています。"
                           "同日に開始日は登録できません。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *T
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "マスタが存在しません" 
                                                   TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "診療科履歴更新エラー" 
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  KERR
           MOVE    SPA-ERRCD            TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  KERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "K061"               TO  SPA-MOTOPG
           MOVE    "KERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "KERR"               TO  MCP-WINDOW
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
      *    確認メッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   STRING    "選択した算定履歴を削除します。"
                             "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING    "算定履歴を登録します。"
                             "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING    "診療行為から登録した算定履歴です。"
                             "リハビリ終了日付のみ更新します。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
      *H25.9
               WHEN    "0104"
                   STRING  "算定履歴が変更となります。"
                           "リハビリコメントは削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
      *
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K061"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴付加情報読込（キー）
      *****************************************************************
       9010-SANTEIPLUS-KEY-SEC         SECTION.
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-SANTEIPLUS-KEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           IF      WRK-SRYYM           =   SPACE
               MOVE    SPA-NAI-SRYYM       TO  TNS-YUKOSTYMD (1:6)
           ELSE
               MOVE    WRK-SRYYM           TO  TNS-YUKOSTYMD (1:6)
           END-IF
           MOVE    "01"                TO  TNS-YUKOSTYMD (7:2)
      *
           MOVE    TNS-YUKOSTYMD       TO  WRK-YUKOSTYMD
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
      *!
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60")  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *    附加情報の上限回数設定
           IF      FLG-TENSU           =   ZERO
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      **** MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
      *    算定履歴マスター読込
      *****************************************************************
       900-SANTEI-INV-SEC              SECTION.
      *
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
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SANTEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
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
      *
      *****************************************************************
      *    受診履歴読み込み処理
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               INITIALIZE                      JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読み込み処理
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読み込み処理
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
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
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
