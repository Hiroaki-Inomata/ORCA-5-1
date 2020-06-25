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
       PROGRAM-ID.         ORAPI021S3V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　診療行為登録　（入力一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/04/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  17/08/XX  前回未収、印刷対応
      *  05.00.00    NACL-多々納  19/01/XX  複数科・保険対応
      *  05.01.01    NMED-太田    19/09/19  API-PDF印刷対応
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
           03  FLG-SYUNOU              PIC 9(01).
      *    03  FLG-PTNYUINRRK          PIC 9(01).
      *    03  FLG-HKNNUM              PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
      *
           03  FLG-DELCHK              PIC 9(01).
           03  FLG-DELOK               PIC 9(01).
           03  FLG-DEL                 PIC 9(01).
           03  FLG-UPDATE              PIC 9(01).
           03  FLG-UPDEND              PIC 9(01).
      *2019.1
           03  FLG-TBLOK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDW2                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-S               PIC 9(04).
      *2019.1
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDC                 PIC 9(04).
           03  TBL-IDC             PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX-A               PIC 9(04).
      *
           03  IDS2                PIC 9(04).
           03  IDS3                PIC 9(04).
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
      *    警告
           03  WRK-KEICD               PIC X(02).
           03  WRK-KEIMSG              PIC X(100).
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
      *    伝票番号
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-SYORI-MODE          PIC X(03).
      *
      *    金額
           03  WRK-KINGAKU             PIC S9(08).
      *    請求金額
           03  WRK-SKYMONEY-GOK        PIC S9(08).
      *    訂正用　入金上限額
           03  WRK-NYUKIN-GOK          PIC S9(08).
           03  WRK-HENKIN-GOK          PIC S9(08).
           03  WRK-TEIKEI-SKYMONEY     PIC S9(08).
           03  WRK-OLD-SKYMONEY        PIC S9(08).
           03  WRK-SEIKYU              PIC S9(07).
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
      *2019.1
      *    複数科保険対応
           03  WRK-ERRCD2              PIC X(02).
           03  WRK-ERR-SRYKA           PIC X(02).
           03  WRK-ERR-HKNCOMBI        PIC X(04).
           03  WRK-ERR2-SRYKA          PIC X(02).
           03  WRK-ERR2-HKNCOMBI       PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *請求金額
           03  WRK-SKYMONEY            PIC S9(07).
      *差額の絶対値
           03  WRK-GRP-SGKMONEY        PIC  9(07).
      *
           03  WRK-HZN-ERRMSG          PIC X(100).
      *    調整金
           03  WRK-GOK-CHOSEI1         PIC S9(07).
           03  WRK-GOK-CHOSEI2         PIC S9(07).
           03  WRK-IN-CHOSEI1          PIC S9(07).
           03  WRK-IN-CHOSEI2          PIC S9(07).
      *
           03  WRK-SYUNOU-IDX          PIC  9(04).
           03  WRK-NAI-HENFLG          PIC  9(01).
      *
           03  WRK-SYU-TEI-ADD.
               05  WRK-NAI-SEIKYU              PIC S9(07).
               05  WRK-NAI-SKYMONEY-TAX-SAI    PIC S9(07).
               05  WRK-TMAE-CHOSEI             PIC S9(07).
               05  WRK-TMAE-CHOSEI1            PIC S9(07).
               05  WRK-TMAE-CHOSEI2            PIC S9(07).
               05  WRK-OLD-SEIKYU              PIC S9(07).
               05  WRK-NAI-NYUKIN              PIC S9(07).
               05  WRK-NAI-HENKIN              PIC S9(07).
               05  WRK-TEI-NYUKIN-TOTAL        PIC S9(07).
           03  WRK-SYU-HENKIN              PIC S9(07).
      *
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
      *    03  WRK-DRCD                PIC X(05).
      *    03  WRK-DRCD-NAME           PIC X(80).
      *
           03  WRK-NYUKIN-HOHO         PIC X(02).
           03  WRK-NYUKIN-HOHO-NAME    PIC X(20).
           03  WRK-NYUKIN-JYOTAI       PIC X(01).
      *    入金額
           03  WRK-NYUKIN              PIC  9(07).
           03  WRK-NYUKIN-OK           PIC  9(01).
      *    返金額
           03  WRK-HENKIN              PIC S9(07).
           03  WRK-HENKIN-OK           PIC  9(01).
      *
      *    調整金１
           03  WRK-CHOSEI1-G           PIC S9(07).
      *    調整金２
           03  WRK-CHOSEI2-G           PIC S9(07).
           03  WRK-CHOSEI-OK-G         PIC 9(01).
      *2019.1
           03  WRK-IN-DATA             OCCURS    15.
      *        保険組合せ
               05  WRK-IN-HKNCOMBI         PIC X(04).
               05  WRK-IN-SRYKA            PIC X(02).
      *        調整金１
               05  WRK-CHOSEI1             PIC S9(07).
      *        調整金２
               05  WRK-CHOSEI2             PIC S9(07).
               05  WRK-CHOSEI-OK           PIC 9(01).
      *        個別入金額
               05  WRK-IN-NYUKIN           PIC 9(07).
               05  WRK-IN-NYUKIN-OK        PIC 9(01).
      *        収納の位置
               05  WRK-IN-SYU-IDX          PIC 9(03).
      *
      *    請求書発行区分
           03  WRK-HAKHOUFLG           PIC X(01).
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
      *
               05  WRK-SELECT-CODE         PIC X(04).
      *        処理区分
               05  WRK-PARA-MODE           PIC X(03).
      *        伝票番号
               05  WRK-PARA-DENPNUM        PIC 9(07).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *返却剤数
       01  COB-ZAI-OUTMAX          PIC 9(04)   VALUE   120.
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
      *    収納マスタワーク
       01  LNK-SYUNOU-AREA.
           02  LNK-SYUNOU-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUNOU-MAX              PIC 9(02).
      *
      *    収納マスタワーク（計算用）
       01  LNK-SYUNOU2-AREA.
           02  LNK-SYUNOU2-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU2-//.
       01  LNK-SYUNOU2-MAX              PIC 9(02).
      *
      *    収納マスタワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //WRK-SYU-//.
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
      *
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
      *    外来登録サブ
           COPY    "CPORCSCGAIRAI.INC".
      *
      *    外来収納負担金計算サブ
           COPY    "CPORCSCSYUNOUG.INC".
      *
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    ＳＰＡ画面パラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
      *    COPY    "K02SCR-SPA"     REPLACING
      *                                //SPA-// BY //K02-SPA-//.
      *
      *
      *    画面スパ領域
           COPY    "K03SCR-SPA".
      *    保存画面
           COPY    "K03SCR-SPA"     REPLACING
                                       //SPA-// BY //HZN-SPA-//.
      *
      *    オンライン画面
       01  WRK-SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV3REQ3.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV3RES3.INC".
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
      * 01  XML-APIREQ.
      *     03  APIREQ-CONTEXT          PIC S9(9)   BINARY.
      *     03  APIREQ-OBJECT           PIC X(8).
      *     03  APIREQ-MODE             PIC S9(9)   BINARY.
      *     03  APIREQ-RECORDNAME       PIC X(128).
      **    03  APIREQ-PATIENTINFOREQ   PIC X(1000000).
      *
      *H29.6
      *    一時ディレクトリ取得・削除サブ
           COPY    "CPORCSTEMPDIR.INC".
      *    クライアント印刷情報取得
           COPY    "CPORCSPRTCTRL.INC".
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
      *
      *    診療費明細書印刷パラメタ
           COPY    "CPORCHC04.INC".
      *
      *    薬情報印刷パラメタ
           COPY    "CPORCSCH30.INC".
      *
      *    お薬手帳発行サブパラメタ
           COPY    "CPORCSCH62.INC".
      *
      *    予約票パラメタ
           COPY    "CPORCHC67.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    入金方法
           COPY    "CPSK1041.INC".
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
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
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
           INITIALIZE                      SPA-K03
           INITIALIZE                      SPA-K01KYOUTU
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
               MOVE   404                  TO  APILST05-HTTPSTATUS
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
           DISPLAY   "* medicalreqv33 end  *"
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
           INITIALIZE                      XML-MEDICALV3RES3
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
           MOVE    SPACE               TO  WRK-KEICD
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
      *****    MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
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
           IF      APILST05-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALV3REQ3
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST05-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req3"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_medicalv3req3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST05-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req3"     TO  APIREQ-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALV3REQ3
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST05-BODY
      *
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      *    警告
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-KEICD       NOT =   SPACE
                   MOVE    WRK-KEICD       TO  WRK-ERRCD
                   MOVE    "W"             TO  WRK-ERRKBN
               END-IF
           END-IF
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
                       IF      FLG-UPDATE      =   ZERO
                           MOVE    "確認処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                       END-IF
               END-EVALUATE
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
      *
                   IF      (WRK-ERR-SRYKA  NOT =   SPACE)
      *                エラー診療科・保険組合せ編集
                       PERFORM 891-ERRSRYKA-HEN-SEC
                   END-IF
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
           IF      APILST05-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res3"     TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALV3RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalv3res3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res3"     TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALV3RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST05-BODY 
               MOVE  "application/xml" TO  APILST05-CONTENT-TYPE
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
      *!!!!
      *    訂正判定
           IF      WRK-ERRCD           =   SPACE
      *        伝票番号
               MOVE    SPACE               TO  MDCRES-INVOICD-NUMBER
               IF     (MDCREQ-PATIENT-MODE =   "Modify")
                  OR  (WRK-PARA-MODE       =   "UPD" )
                   PERFORM 20011-DENPNUM-CHK-SEC
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
      *    基準日付（伝票発行日）チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-BASEDATE-CHK-SEC
           END-IF
      *
           IF     (FLG-SYORI           =   2     )
            AND   (WRK-ERRCD           =   SPACE )
      *        リクエスト＝０５のチェックチェック
               PERFORM 20011-INPUT-CHK05-SEC
      *
               MOVE    "05"                TO  MDCRES-RESPONSE-NUMBER
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *H29.6
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       20011-INPUT-CHK05-SEC      SECTION.
      *
      *    更新区分
           IF     WRK-ERRCD            =   SPACE
               PERFORM 20013-REQUEST-MODE-CHK-SEC
           END-IF
      *
      *    入金方法
           IF     WRK-ERRCD            =   SPACE
               PERFORM 20013-NYUKIN-HOHO-CHK-SEC
           END-IF
      *    入金取り扱い区分
           IF     WRK-ERRCD            =   SPACE
               PERFORM 20013-IC-REQUEST-CHK-SEC
           END-IF
      *!!
      *    請求書兼領収書明細書発行区分
           IF     WRK-ERRCD            =   SPACE
               PERFORM 20013-HAKKOU-KBN-CHK-SEC
           END-IF
      *
      *    一括入返金区分
           IF     WRK-ERRCD            =   SPACE
               PERFORM 20013-IC-ALLKBN-CHK-SEC
           END-IF
      *
      *    入金額
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-NYUKIN-CHK-SEC
           END-IF
      *
      *    調整金
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-CHOSEIKIN-CHK-SEC
           END-IF
      *
      *    返金
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-HENKIN-CHK-SEC
           END-IF
      *
      *2019.1
      *複数保険・科対応
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )
                          OR  (WRK-ERRCD   NOT =   SPACE )
      *        保険組合せ番号・診療科
               PERFORM 20013-HKNCOMBI-CHK-SEC
      *
      *        調整金
               IF    (WRK-ERRCD            =   SPACE )
                   PERFORM 20013-CHOSEIKIN-SUB-CHK-SEC
               END-IF
      *
      *        入金
               IF    (WRK-ERRCD            =   SPACE )
                   PERFORM 20014-NYUKIN-SUB-CHK-SEC
               END-IF
      *        関連チェック
               IF    (WRK-ERRCD            =   SPACE )
                   PERFORM 20013-TBL-CHECK-SEC
               END-IF
           END-PERFORM
      *2019.1
      *    単独の調整金編集
           IF    (WRK-ERRCD            =   SPACE )
             AND (WRK-CHOSEI-OK-G      =   1     )
               PERFORM 20014-CHOSEI-TBL-SEC
           END-IF
      *    2019/09/19 Added
      *    印刷モード
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-PRINT-MODE-CHK-SEC
           END-IF
      *    処方せん印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-SHOHOU-CHK-SEC
           END-IF
      *    請求書兼領収書印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-SEIKYU-CHK-SEC
           END-IF
      *    診療費明細書印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-MEISAI-CHK-SEC
           END-IF
      *    薬剤情報印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-YAKUJYO-CHK-SEC
           END-IF
      *    お薬手帳印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-TECHOU-CHK-SEC
           END-IF
      *    予約票印刷
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-YOYAKU-CHK-SEC
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
      *    基準日付（伝票発行日）
           MOVE    MDCREQ-BASE-DATE        TO  MDCRES-BASE-DATE
      *    数字のみに編集
           MOVE    MDCREQ-BASE-DATE        TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-BASE-YMD
      *    未設定時はシステム情報設定
           IF      WRK-BASE-YMD            =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-BASE-YMD
               MOVE    MDCRES-INFORMATION-DATE
                                           TO  MDCRES-BASE-DATE
           END-IF
      *
      *    入力UID
           MOVE    MDCREQ-ORCA-UID     TO  MDCRES-ORCA-UID
           MOVE    MDCREQ-KARTE-UID    TO  MDCRES-KARTE-UID
      *    入金方法
           MOVE    MDCREQ-IC-CODE      TO  MDCREQ-IC-CODE
      *
           MOVE    MDCREQ-IC-REQUEST   TO  MDCRES-IC-REQUEST
      *    入金金額
      **** MOVE    MDCREQ-IC-MONEY     TO  MDCRES-IC-MONEY
           MOVE    MDCREQ-IC-MONEY     TO  MDCRES-IN-IC-MONEY
      *    返金額
      **** MOVE    MDCREQ-RE-MONEY     TO  MDCRES-RE-MONEY
           MOVE    MDCREQ-RE-MONEY     TO  MDCRES-IN-RE-MONEY
      *    調整金１
           MOVE    MDCREQ-AD-MONEY1    TO  MDCRES-IN-AD-MONEY1
           MOVE    MDCREQ-AD-MONEY2    TO  MDCRES-IN-AD-MONEY2
      *!!!!
      *2019.1
      *複数保険・科対応
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   15
      *        保険組合せ
               MOVE    MDCREQ-CD-COMBINATION-NUMBER(IDX)
                                       TO  MDCRES-IN-CD-COMB-NUMBER
                                                             (IDX)
      *        診療科
               MOVE    MDCREQ-CD-DEP-CODE(IDX)
                                       TO   MDCRES-IN-CD-DEP-CODE
                                                             (IDX)
      *        入金金額
               MOVE    MDCREQ-CD-IC-MONEY(IDX)
                                      TO  MDCRES-IN-CD-IC-MONEY(IDX)
      *        調整金１
               MOVE    MDCREQ-CD-AD-MONEY1(IDX)
                                      TO  MDCRES-IN-CD-AD-MONEY1(IDX)
               MOVE    MDCREQ-CD-AD-MONEY2(IDX)
                                      TO  MDCRES-IN-CD-AD-MONEY2(IDX)
           END-PERFORM
      *
      *    請求書兼領収書発行区分
           MOVE    MDCREQ-HAKKOU-KBN   TO  MDCRES-HAKKOU-KBN
      *
      *H29.6
      *    請求書印刷
           MOVE    MDCREQ-PRINT-SEIKYU TO  MDCRES-PRINT-SEIKYU
      *    診療費明細書印刷
           MOVE    MDCREQ-PRINT-MEISAI TO  MDCRES-PRINT-MEISAI
      *    処方せん印刷
           MOVE    MDCREQ-PRINT-SHOHOU TO  MDCRES-PRINT-SHOHOU
      *    お薬手帳印刷
           MOVE    MDCREQ-PRINT-TECHOU TO  MDCRES-PRINT-TECHOU
      *    薬剤情報印刷
           MOVE    MDCREQ-PRINT-YAKUJYO TO  MDCRES-PRINT-YAKUJYO
      *    予約票印刷
           MOVE    MDCREQ-PRINT-YOYAKU TO  MDCRES-PRINT-YOYAKU
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
      *****************************************************************
      *    基準日付（伝票発行日）チェック
      *****************************************************************
       20011-BASEDATE-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-BASE-YMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-BASEDATE-CHK-EXT.
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
      *H28.1
      *****************************************************************
      *    伝票番号チェック処理
      *****************************************************************
       20011-DENPNUM-CHK-SEC         SECTION.
      *
           MOVE    WRK-PARA-DENPNUM    TO  WRK-DENPNUM
      *
           IF     (MDCREQ-PATIENT-MODE =   "Modify")
              AND (WRK-PARA-MODE       =   "UPD"   )
              AND (SPA-SYORIFLG        =   1       )
               MOVE    "UPD"               TO  WRK-SYORI-MODE
               IF     (SPA-DENPNUM     NOT =   WRK-PARA-DENPNUM)
                   MOVE    "21"                TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "26"                TO  WRK-ERRCD
           END-IF
           .
       20011-DENPNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法区分チェック処理
      *****************************************************************
       20013-NYUKIN-HOHO-CHK-SEC         SECTION.
      *
           IF      MDCREQ-IC-CODE      =   SPACE
               MOVE    SPA-NYUKIN-HOHO     TO  WRK-NYUKIN-HOHO
               IF      SPA-HKNCOMBINUM     =   "9999"
                   GO      TO      20013-NYUKIN-HOHO-CHK-EXT
               END-IF
           ELSE
               MOVE    MDCREQ-IC-CODE      TO  WRK-NYUKIN-HOHO
           END-IF
      *
      *    入金方法区分
           MOVE    SPACE               TO  SYS-1041-REC
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1041-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1041-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
           IF      WRK-NYUKIN-HOHO     =   SPACE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    WRK-NYUKIN-HOHO     TO  SYS-1041-KBNCD
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
           END-IF
      *    システム管理検索
           MOVE    SYS-1041-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYS-1041-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1041-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-1041-NYKN-TANNAME
                                           TO  WRK-NYUKIN-HOHO-NAME
               MOVE    SYS-1041-NYKN-JYOTAI
                                           TO  WRK-NYUKIN-JYOTAI
               MOVE    SYS-1041-KBNCD(1:2) TO  WRK-NYUKIN-HOHO
           ELSE
               MOVE    SPACE               TO  WRK-NYUKIN-HOHO-NAME
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-NYUKIN-HOHO         TO  MDCRES-IC-CODE
           MOVE    WRK-NYUKIN-HOHO-NAME    TO  MDCRES-IC-CODE-NAME
           .
       20013-NYUKIN-HOHO-CHK-EXT.
           EXIT.
      *
      *H29.6
      *****************************************************************
      *    更新区分チェック処理
      *****************************************************************
       20013-REQUEST-MODE-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-REQUEST-MODE
      ******** WHEN    SPACE
               WHEN    "Check"
                   MOVE    ZERO            TO  FLG-UPDATE
               WHEN    "Finish"
               WHEN    SPACE
                   MOVE    1               TO  FLG-UPDATE
               WHEN    OTHER
                   MOVE    "61"            TO  WRK-ERRCD
           END-EVALUATE
           .
       20013-REQUEST-MODE-CHK-EXT.
           EXIT.
      *
      *H29.6
      *****************************************************************
      *    入金取り扱い区分チェック処理
      *****************************************************************
       20013-IC-REQUEST-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-IC-REQUEST
               WHEN    SPACE
                   CONTINUE
               WHEN    "1"
                   MOVE    "今回請求分のみ入金"
                                       TO  MDCRES-IC-REQUEST-NAME
               WHEN    "2"
                   MOVE    "今回分・伝票の古い未収順に入金"
                                       TO  MDCRES-IC-REQUEST-NAME
               WHEN    "3"
                   MOVE    "今回分・伝票の新しい未収順に入金"
                                       TO  MDCRES-IC-REQUEST-NAME
               WHEN    "4"
                   MOVE    "伝票の古い未収順に入金"
                                       TO  MDCRES-IC-REQUEST-NAME
               WHEN    "5"
                   MOVE    "伝票の新しい未収順に入金"
                                       TO  MDCRES-IC-REQUEST-NAME
               WHEN    OTHER
                   MOVE    "57"        TO  WRK-ERRCD
           END-EVALUATE
      *    訂正は今回分のみ
           IF      SPA-SYORIFLG        =   1
               IF      MDCRES-IC-REQUEST   =   "1"
                                           OR  SPACE
                   CONTINUE
               ELSE
                   MOVE    "58"        TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20013-IC-REQUEST-CHK-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    請求書兼領収書明細書発行区分チェック処理
      *****************************************************************
       20013-HAKKOU-KBN-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-HAKKOU-KBN
               WHEN    SPACE
                   CONTINUE
               WHEN    "1"
                   MOVE    "診療科・保険組合せ別に発行"
                                       TO  MDCRES-HAKKOU-KBN-MEI
               WHEN    "2"
                   MOVE    "保険組合せ別に発行"
                                       TO  MDCRES-HAKKOU-KBN-MEI
               WHEN    "3"
                  MOVE    "診療科別に発行"
                                       TO  MDCRES-HAKKOU-KBN-MEI
               WHEN    "4"
                  MOVE    "全体をまとめて発行"
                                       TO  MDCRES-HAKKOU-KBN-MEI
               WHEN    OTHER
                   MOVE    "62"        TO  WRK-ERRCD
           END-EVALUATE
           .
       20013-HAKKOU-KBN-CHK-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    一括入返金区分チェック処理
      *****************************************************************
       20013-IC-ALLKBN-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-IC-ALLKBN
               WHEN    SPACE
                   CONTINUE
               WHEN    "1"
                   IF     (MDCREQ-IC-REQUEST   =   "2"
                                               OR  "3"
                                               OR  "4"
                                               OR  "5" )
                     OR   (SPA-SYORIFLG        =   1   )
                       CONTINUE
                   ELSE
                       MOVE    "60"            TO  WRK-ERRCD
                   END-IF
               WHEN    OTHER
                   MOVE    "59"        TO  WRK-ERRCD
           END-EVALUATE
           .
       20013-IC-ALLKBN-CHK-EXT.
           EXIT.
      *
      *!!!!!
      *2019.1
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20013-HKNCOMBI-CHK-SEC         SECTION.
      *
           IF      MDCREQ-CD-COMBINATION-NUMBER (IDX)
                                       =   SPACE
               MOVE    SPACE           TO  WRK-IN-HKNCOMBI (IDX)
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-CD-COMBINATION-NUMBER (IDX)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "70"                TO  WRK-ERRCD
                   MOVE    MDCREQ-CD-COMBINATION-NUMBER (IDX)
                                               TO  WRK-IN-HKNCOMBI(IDX)
               ELSE
                   MOVE    SNUM-OUTEDT(12:4)   TO  WRK-IN-HKNCOMBI(IDX)
               END-IF
           END-IF
      *    9999　はエラー（収納なし）
           IF      WRK-IN-HKNCOMBI(IDX)    =   "9999"
                                           OR  ZERO
               MOVE    "70"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        診療科
               MOVE    MDCREQ-CD-DEP-CODE(IDX)
                                           TO  WRK-IN-SRYKA (IDX)
               IF      WRK-IN-SRYKA (IDX)  =   ZERO
                   MOVE    "71"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20013-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       20013-TBL-CHECK-SEC         SECTION.
      *
      *
           IF      (WRK-CHOSEI-OK(IDX)     =   ZERO )
               AND (WRK-IN-NYUKIN-OK(IDX)  =   ZERO )
      *        送信なし
               MOVE    SPACE               TO  WRK-IN-HKNCOMBI(IDX)
               MOVE    SPACE               TO  WRK-IN-SRYKA (IDX)
           ELSE
      *        送信あり
               IF     (WRK-IN-HKNCOMBI(IDX)    =   SPACE )
      *            １件目は主たる保険
                   IF      IDX                 =   1
                       MOVE    SPA-HKNCOMBINUM
                                               TO  WRK-IN-HKNCOMBI(IDX)
                   END-IF
               END-IF
               IF     (WRK-IN-SRYKA (IDX)      =   SPACE )
                   IF      IDX                 =   1
                       MOVE    SPA-SRYKA    TO  WRK-IN-SRYKA(IDX)
                   END-IF
               END-IF
      *
      *        保険・診療科テーブルに存在すること
               MOVE    ZERO                TO  FLG-TBLOK
               PERFORM VARYING   IDH1      FROM    1   BY  1
                       UNTIL    (IDH1      >   15  )
                        OR      (SPA-TBL-HKNCOMBI(IDH1)    =   SPACE)
                        OR      (FLG-TBLOK         =    1    )
                   IF      SPA-TBL-HKNCOMBI(IDH1)
                                               =   WRK-IN-HKNCOMBI(IDX)
                       PERFORM VARYING   IDH2      FROM   1   BY  1
                               UNTIL    (IDH2      >   10  )
                                    OR  (SPA-TBL-SRYKA (IDH1 IDH2)
                                                   =   SPACE )
                                    OR  (FLG-TBLOK =    1    )
                           IF      SPA-TBL-SRYKA (IDH1 IDH2)
                                               =   WRK-IN-SRYKA (IDX)
                               MOVE    1           TO  FLG-TBLOK
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
               IF      FLG-TBLOK               =   ZERO
                   MOVE    "72"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20013-TBL-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金金額数値チェック処理
      *****************************************************************
       20013-NYUKIN-CHK-SEC         SECTION.
      *
           IF      MDCREQ-IC-MONEY     =   SPACE
               MOVE    ZERO            TO  WRK-NYUKIN-OK
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-IC-MONEY         TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   0     )
      *            整数７桁
                   MOVE    "16"                TO  WRK-ERRCD
               ELSE
      *            入金額設定あり
                   MOVE    SNUM-OUTNUM         TO  WRK-NYUKIN
                   MOVE    1                   TO  WRK-NYUKIN-OK
               END-IF
               IF     (SPA-HKNCOMBINUM         =   "9999" )
                   IF      WRK-NYUKIN          NOT =   ZERO
                       MOVE    "23"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       20013-NYUKIN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    調整金数値チェック処理
      *****************************************************************
       20013-CHOSEIKIN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-CHOSEI1-G
           MOVE    ZERO                TO  WRK-CHOSEI2-G
      *    調整金１
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-AD-MONEY1    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "17"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-CHOSEI1-G
           END-IF
      *
      *    調整金２
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-AD-MONEY2    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "18"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-CHOSEI2-G
           END-IF
      *
           IF      SPA-HKNCOMBINUM     =   "9999"
               IF     (WRK-CHOSEI1-G     NOT =   ZERO )
                 OR   (WRK-CHOSEI2-G     NOT =   ZERO )
                   MOVE    "24"            TO  WRK-ERRCD
               END-IF
           END-IF
           IF     (MDCREQ-AD-MONEY1    =   SPACE  )
             AND  (MDCREQ-AD-MONEY2    =   SPACE  )
               MOVE    ZERO                TO  WRK-CHOSEI-OK-G
           ELSE
               MOVE    1                   TO  WRK-CHOSEI-OK-G
           END-IF
      *
      *    複数科・保険は送信不可
           IF     (SPA-TBL-HKNCOMBI(2) NOT =   SPACE )
              OR  (SPA-TBL-SRYKA (1 2) NOT =   SPACE )
               IF     (WRK-CHOSEI-OK-G     =   1     )
                   MOVE    "69"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20013-CHOSEIKIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独の調整金編集処理
      *****************************************************************
       20014-CHOSEI-TBL-SEC         SECTION.
      *
           IF    ((WRK-IN-HKNCOMBI(1)  =   SPA-HKNCOMBINUM )
              AND (WRK-IN-SRYKA   (1)  =   SPA-SRYKA       ))
           OR    ((WRK-IN-HKNCOMBI(1)  =   SPACE           )
              AND (WRK-IN-SRYKA   (1)  =   SPACE           ))
      *      １件目は主たる保険
               MOVE    1                   TO  IDX
               MOVE    SPA-HKNCOMBINUM     TO  WRK-IN-HKNCOMBI(IDX)
               MOVE    SPA-SRYKA           TO  WRK-IN-SRYKA   (IDX)
               MOVE    1                   TO  WRK-CHOSEI-OK(IDX)
      *
               MOVE    WRK-CHOSEI1-G       TO  WRK-CHOSEI1 (IDX)
               MOVE    WRK-CHOSEI2-G       TO  WRK-CHOSEI2 (IDX)
           END-IF
           .
       20014-CHOSEI-TBL-EXT.
           EXIT.
      *
      *H30.XX
      *****************************************************************
      *    入金金額数値チェック処理
      *****************************************************************
       20014-NYUKIN-SUB-CHK-SEC         SECTION.
      *
           IF      MDCREQ-CD-IC-MONEY(IDX) =   SPACE
               MOVE    ZERO            TO  WRK-IN-NYUKIN-OK(IDX)
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-CD-IC-MONEY(IDX) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   0     )
      *            整数７桁
                   MOVE    "16"                TO  WRK-ERRCD
               ELSE
      *            入金額設定あり
                   MOVE    SNUM-OUTNUM         TO  WRK-IN-NYUKIN(IDX)
                   MOVE    1                   TO  WRK-IN-NYUKIN-OK(IDX)
               END-IF
               IF     (SPA-HKNCOMBINUM         =   "9999" )
                   IF      WRK-IN-NYUKIN(IDX)  NOT =   ZERO
                       MOVE    "23"            TO  WRK-ERRCD
                   END-IF
               END-IF
      *
      *        合計入金額と同時送信はできない
               IF      WRK-IN-NYUKIN-OK  (IDX) =   1
                   IF     (SPA-SYORIFLG        =   0   )
                       MOVE    "73"            TO  WRK-ERRCD
                   END-IF
                   IF     (WRK-NYUKIN-OK       =   1   )
                       IF      WRK-ERRCD       =   SPACE
                           MOVE    "74"            TO  WRK-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       20014-NYUKIN-SUB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調整金数値チェック処理
      *****************************************************************
       20013-CHOSEIKIN-SUB-CHK-SEC         SECTION.
      *
      *    調整金１
           IF     (MDCREQ-CD-AD-MONEY1(IDX)   =   SPACE  )
             AND  (MDCREQ-CD-AD-MONEY2(IDX)   =   SPACE  )
               MOVE    ZERO                TO  WRK-CHOSEI-OK(IDX)
           ELSE
               MOVE    1                   TO  WRK-CHOSEI-OK(IDX)
           END-IF
      *
      *    調整金１
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-CD-AD-MONEY1(IDX)    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "17"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-CHOSEI1 (IDX)
           END-IF
      *
      *    調整金２
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-CD-AD-MONEY2(IDX)    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "18"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-CHOSEI2(IDX)
           END-IF
      *
           IF     (SPA-HKNCOMBINUM     =   "9999"   )
               IF     (WRK-CHOSEI1(IDX)    NOT =   ZERO )
                 OR   (WRK-CHOSEI2(IDX)    NOT =   ZERO )
                   MOVE    "24"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    同時送信はできない
           IF      WRK-CHOSEI-OK(IDX)   =   1
               IF     (WRK-CHOSEI-OK-G     =   1   )
                   IF      WRK-ERRCD       =   SPACE
                       MOVE    "79"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       20013-CHOSEIKIN-SUB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返金金額数値チェック処理
      *****************************************************************
       20013-HENKIN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-HENKIN
           MOVE    ZERO                TO  WRK-HENKIN-OK
           IF      MDCREQ-RE-MONEY     =   SPACE
               MOVE    ZERO            TO  WRK-HENKIN-OK
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-RE-MONEY         TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   0     )
      *            整数７桁
                   MOVE    "14"                TO  WRK-ERRCD
               ELSE
      *            設定あり
                   MOVE    SNUM-OUTNUM         TO  WRK-HENKIN
                   MOVE    1                   TO  WRK-HENKIN-OK
               END-IF
               IF      WRK-HENKIN      NOT =   ZERO
                   IF      SPA-HKNCOMBINUM     =   "9999"
                       MOVE    "25"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       20013-HENKIN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    請求書兼領収書印刷チェック処理
      *****************************************************************
       20013-SEIKYU-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-PRINT-SEIKYU
               WHEN    SPACE
                   CONTINUE
               WHEN    "0"
                   MOVE    "0"             TO  MDCRES-PRINT-SEIKYU
                   MOVE    "発行なし"      TO  MDCRES-PRINT-SEIKYU-MEI
               WHEN    "1"
                   MOVE    "発行あり"      TO  MDCRES-PRINT-SEIKYU-MEI
                   IF      SPA-SYORIFLG        =   1
                       MOVE    "発行あり（訂正分）"
                                           TO  MDCRES-PRINT-SEIKYU-MEI
                   END-IF
               WHEN    "2"
                   MOVE    "発行あり"      TO  MDCRES-PRINT-SEIKYU-MEI
                   IF      SPA-SYORIFLG        =   1
                       MOVE    "発行あり（合計）"
                                           TO  MDCRES-PRINT-SEIKYU-MEI
                   END-IF
               WHEN    OTHER
                   MOVE    "51"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20013-SEIKYU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療費明細書印刷チェック処理
      *****************************************************************
       20013-MEISAI-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-PRINT-MEISAI
               WHEN    SPACE
                   CONTINUE
               WHEN    "0"
                   MOVE    "0"             TO  MDCRES-PRINT-MEISAI
                   MOVE    "発行なし"      TO  MDCRES-PRINT-MEISAI-MEI
               WHEN    "1"
                   MOVE    "発行あり"      TO  MDCRES-PRINT-MEISAI-MEI
               WHEN    OTHER
                   MOVE    "52"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20013-MEISAI-CHK-EXT.
           EXIT.
      *
      * 2019/09/19 Added
      *****************************************************************
      *    印刷モードチェック処理
      *****************************************************************
       20013-PRINT-MODE-CHK-SEC    SECTION.
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
                   MOVE    "63"        TO  WRK-ERRCD
               END-IF
           WHEN    OTHER
               MOVE    "64"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20013-PRINT-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処方せん印刷チェック処理
      *****************************************************************
       20013-SHOHOU-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-PRINT-SHOHOU
               WHEN    SPACE
                   CONTINUE
               WHEN    "0"
                   MOVE    "0"             TO  MDCRES-PRINT-SHOHOU
                   MOVE    "発行なし"      TO  MDCRES-PRINT-SHOHOU-MEI
               WHEN    "1"
                   MOVE    "発行あり"      TO  MDCRES-PRINT-SHOHOU-MEI
               WHEN    "2"
                   MOVE    "院内処方発行"  TO  MDCRES-PRINT-SHOHOU-MEI
               WHEN    OTHER
                   MOVE    "53"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20013-SHOHOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報印刷チェック処理
      *****************************************************************
       20013-YAKUJYO-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-PRINT-YAKUJYO
               WHEN    SPACE
                   CONTINUE
               WHEN    "0"
                   MOVE    "0"             TO  MDCRES-PRINT-YAKUJYO
                   MOVE    "発行なし"      TO  MDCRES-PRINT-YAKUJYO-MEI
               WHEN    "1"
                   MOVE    "発行あり"      TO  MDCRES-PRINT-YAKUJYO-MEI
               WHEN    "2"
                   MOVE    "院外分発行"    TO  MDCRES-PRINT-YAKUJYO-MEI
               WHEN    OTHER
                   MOVE    "54"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20013-YAKUJYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳印刷チェック処理
      *****************************************************************
       20013-TECHOU-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-PRINT-TECHOU
               WHEN    SPACE
                   CONTINUE
               WHEN    "0"
                   MOVE    "0"             TO  MDCRES-PRINT-TECHOU
                   MOVE    "発行なし"      TO  MDCRES-PRINT-TECHOU-MEI
               WHEN    "1"
                   MOVE    "発行あり"      TO  MDCRES-PRINT-TECHOU-MEI
               WHEN    "2"
                   MOVE    "院外分発行"    TO  MDCRES-PRINT-TECHOU-MEI
               WHEN    OTHER
                   MOVE    "55"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20013-TECHOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約票印刷チェック処理
      *****************************************************************
       20013-YOYAKU-CHK-SEC         SECTION.
      *
           EVALUATE    MDCREQ-PRINT-YOYAKU
               WHEN    SPACE
                   CONTINUE
               WHEN    "0"
                   MOVE    "0"             TO  MDCRES-PRINT-YOYAKU
                   MOVE    "発行なし"      TO  MDCRES-PRINT-YOYAKU-MEI
               WHEN    "1"
                   MOVE    "発行あり"      TO  MDCRES-PRINT-YOYAKU-MEI
               WHEN    OTHER
                   MOVE    "56"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20013-YOYAKU-CHK-EXT.
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
           PERFORM 20020-SYUNOU-PARA-SEC
      *    剤削除処理
           IF      MDCREQ-MDC-MODE     =   "1"
               PERFORM 20021-ZAIDELETE-SEC
      *        ワーク診療行為更新
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 200291-WKSRYACT-UPD-SEC
               END-IF
           END-IF
      *    請求金額取得
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-SYUNOU-HEN-SEC
           END-IF
      *
      *    入金・請求額
           IF     (WRK-ERRCD           =   SPACE)
               INITIALIZE                  SPA-K03
               IF     (SPA-HKNCOMBINUM     =   "9999" )
      *            収納データをSPAへ
                   PERFORM 20025-SYUNOU-9999-SEC
               ELSE
      *            請求確認画面編集処理
                   PERFORM 20023-SEIKYU-SYORI-SEC
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
      *   ドクター
           MOVE    SPA-DRCD            TO  MDCRES-APPOINT-DR-CODE
           MOVE    SPA-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
      *    保険組合せ編集処理
           MOVE    SPA-HKNCOMBINUM    TO  MDCRES-COMBINATION-NUMBER
           IF      SPA-HKNCOMBINUM     =   "9999"
               MOVE    SPA-HKNCOMBIMEI
                                       TO  MDCRES-MAIN-INSURANCE-NAME
           ELSE
               INITIALIZE                      ORCSAPIHKNAREA
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
               MOVE    SPA-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCSAPIHKN-IN-HKNCOMBI
               MOVE    SPA-SRYYMD          TO  ORCSAPIHKN-SRYYMD
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
      *    収納データ編集処理
      *****************************************************************
       20020-SYUNOU-PARA-SEC        SECTION.
      *
           MOVE    SPACE               TO  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SYUNOU-AREA
           MOVE    ZERO                TO  LNK-SYUNOU-MAX
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
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
      *        収納
               ADD     1                   TO  LNK-SYUNOU-MAX
               IF      LNK-SYUNOU-MAX      <=  15
                   MOVE    PARA-DATA-REC       TO  LNK-SYUNOU-REC
                                                       (LNK-SYUNOU-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20020-SYUNOU-PARA-EXT.
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
      *****************************************************************
      *    収納内容処理
      *****************************************************************
       20022-SYUNOU-HEN-SEC        SECTION.
      *
      *
           INITIALIZE                  PARA-WKSRYACT-AREA
           MOVE    ZERO                TO  IDW2
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   LNK-WKSRYACT-MAX
               MOVE    ZERO                TO  FLG-DEL
      *
               IF     (LNK-WKSRY-INPUTCD(IDW 1)(1:1)     =   "#"
                                                         OR  "$")
                   MOVE    1               TO  FLG-DEL
               END-IF
      *        包括分は対象外
               IF     (LNK-WKSRY-HKNCOMBI (IDW)    =   "9999")
                   OR (LNK-WKSRY-CONTKBN  (IDW)    =   "H"   )
                   MOVE    1               TO  FLG-DEL
               END-IF
               IF      LNK-WKSRYAPI-DELKBN (IDW)   NOT =   ZERO
                   MOVE    1               TO  FLG-DEL
               END-IF
               IF      FLG-DEL             =   ZERO
                   ADD     1               TO  IDW2
                   MOVE    LNK-WKSRYACT-REC (IDW)
                                           TO  PARA-WKSRYACT-REC(IDW2)
               END-IF
           END-PERFORM
      *
      *    収納　負担金計算
           INITIALIZE                  ORCSCSYUNOUGAREA
           MOVE    "K08"               TO  ORCSCSYU-SYORIPG
      *    初期設定編集
           MOVE    "1"                 TO  ORCSCSYU-SYORIKBN
      *    複数保険・科編集
           PERFORM VARYING   IDH1      FROM    1   BY  1
                   UNTIL    (IDH1      >   15  )   OR
                            (SPA-TBL-HKNCOMBI(IDH1)    =   SPACE)
               MOVE    SPA-TBL-HKNCOMBI(IDH1)     TO  ORCSCSYU-HKNCOMBI
                                                                (IDH1)
               PERFORM VARYING   IDH2      FROM   1   BY  1
                       UNTIL    (IDH2      >   10  )  OR
                                (SPA-TBL-SRYKA (IDH1 IDH2) =   SPACE )
                   MOVE    SPA-TBL-SRYKA (IDH1 IDH2)
                                               TO  ORCSCSYU-SRYKA
                                                           (IDH1 IDH2)
               END-PERFORM
           END-PERFORM
      *
           INITIALIZE                  LNK-ORCS01-REC
      *
           MOVE    IDW2                TO  ORCSCSYU-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-SYUNOU2-AREA
           MOVE    LNK-SYUNOU-AREA     TO  LNK-SYUNOU2-AREA
           CALL    "ORCSCSYUNOUG"      USING
                                       ORCSCSYUNOUGAREA
                                       LNK-ORCS01-REC
                                       SPA-AREA
                                       LNK-SYUNOU2-AREA
                                       PARA-WKSRYACT-AREA
           MOVE    ORCSCSYU-SYUNOU-MAX TO  LNK-SYUNOU2-MAX
      *
      *H30.6
      *    収納オーバー
           IF      ORCSCSYU-ERRCD      =   9
               MOVE    "78"            TO  WRK-ERRCD
           END-IF
      *    金額桁あふれ
           IF      ORCSCSYU-ERRCD      =   11
      *        負担額
               MOVE    "77"            TO  WRK-ERRCD
           END-IF
           IF      ORCSCSYU-ERRCD      =   12
      *        請求額
               MOVE    "29"            TO  WRK-ERRCD
           END-IF
      *    収納請求金額チェック処理
           PERFORM 45017-SYUNOU-GOKCHK-SEC
      *
      *    現物給付メッセージフラグ
           MOVE    LNK-S01-GENBUTU-MSG-FLG     TO  SPA-FTNHEN-KBN
      *
           IF      SPA-KAGRPFLG        =   "0"
      *        複数科まとめ集計の差額計算
               PERFORM 45016-SYUNOU-SAGAKU-SEC
           ELSE
      *        複数科まとめ集計をしない場合、科合計を計算
               PERFORM 45015-SYUNOU-GOUKEI-SEC
           END-IF
           .
       20022-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納請求金額チェック処理
      *****************************************************************
       45017-SYUNOU-GOKCHK-SEC        SECTION.
      *
          MOVE    SPACE               TO  SYUNOU-REC
          INITIALIZE                  SYUNOU-REC
          PERFORM VARYING     IDS1    FROM    1   BY  1
                  UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   CONTINUE
               ELSE
      *            請求金額の桁あふれチェック
                   IF     (LNK-SYU2-SKYMONEY(IDS1)
                                       +   SYU-SKYMONEY)
                                       >   9999999
                       MOVE    "29"           TO  WRK-ERRCD
                   END-IF
                   ADD    LNK-SYU2-SKYMONEY(IDS1)
                                               TO  SYU-SKYMONEY
               END-IF
           END-PERFORM
      *
           .
       45017-SYUNOU-GOKCHK-EXT.
           EXIT.
      *****************************************************************
      *    複数科まとめ集計の差額計算処理
      *****************************************************************
       45016-SYUNOU-SAGAKU-SEC             SECTION.
      *
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   MOVE    LNK-SYU2-HKNCOMBINUM(IDS1)
                                               TO  WRK-HKNCOMBI
                   MOVE    ZERO                TO  WRK-SKYMONEY
                   MOVE    LNK-SYUNOU2-REC (IDS1)  TO  SYUNOU-REC
                   PERFORM VARYING     IDS2    FROM    1   BY  1
                           UNTIL       IDS2    >   LNK-SYUNOU2-MAX
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS2)
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS2)
                                           NOT =   ZERO  )
                           ADD     LNK-SYU2-SKYMONEY(IDS2)
                                               TO  WRK-SKYMONEY
                       END-IF
                   END-PERFORM
                   COMPUTE SYU-GRP-SGKMONEY    =   SYU-SKYMONEY
                                               -   WRK-SKYMONEY
                   MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC(IDS1)
      *            収納に差額編集、請求額変更
                   MOVE    ZERO                TO  IDS3
                   MOVE    SYU-GRP-SGKMONEY    TO  WRK-GRP-SGKMONEY
                   PERFORM VARYING     IDS2    FROM    LNK-SYUNOU2-MAX
                                                       BY  -1
                           UNTIL      (IDS2    <   1   )  OR
                                      (IDS3    >   ZERO )
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS2) 
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS2)
                                           NOT =   ZERO  )
                           IF     (SYU-GRP-SGKMONEY    >   ZERO ) OR
                                  (LNK-SYU2-SKYMONEY(IDS2)
                                               >=  WRK-GRP-SGKMONEY)
                               MOVE    IDS2        TO  IDS3
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      IDS3                >   ZERO
                       MOVE    SYU-GRP-SGKMONEY
                                              TO  LNK-SYU2-GRP-SGKMONEY
                                                                (IDS3)
                       COMPUTE LNK-SYU2-SKYMONEY(IDS3)
                                               =   LNK-SYU2-SKYMONEY
                                                                (IDS3)
                                               +   SYU-GRP-SGKMONEY
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       45016-SYUNOU-SAGAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ集計なしの収納合計作成処理
      *****************************************************************
       45015-SYUNOU-GOUKEI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   MOVE    LNK-SYU2-HKNCOMBINUM(IDS1)
                                               TO  WRK-HKNCOMBI
                   MOVE    SPACE               TO  SYUNOU-REC
                   INITIALIZE                  SYUNOU-REC
                   PERFORM VARYING     IDS3    FROM    1   BY  1
                           UNTIL       IDS3    >   LNK-SYUNOU2-MAX
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS3)
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS3)
                                           NOT =   ZERO  )
                           IF      SYU-PTID        =   ZERO
                               MOVE    LNK-SYUNOU2-REC (IDS3)
                                                   TO  SYUNOU-REC
                               MOVE    ZERO        TO  SYU-SRYKA
                           ELSE
                               MOVE    LNK-SYUNOU2-REC (IDS3)
                                                   TO  WRK-SYUNOU-REC
                               PERFORM 450141-SYUNOU-ADD-HEN-SEC
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE    SYUNOU-REC      TO  LNK-SYUNOU2-REC (IDS1)
               END-IF
           END-PERFORM
      *
           .
       45015-SYUNOU-GOUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新・科合計追加処理
      *****************************************************************
       450141-SYUNOU-ADD-HEN-SEC       SECTION.
      *
           ADD     WRK-SYU-TAX-TAISHOU TO  SYU-TAX-TAISHOU
           ADD     WRK-SYU-TAX-MONEY   TO  SYU-TAX-MONEY
           ADD     WRK-SYU-SKYGK       TO  SYU-SKYGK
           ADD     WRK-SYU-HKNTEKMONEY TO  SYU-HKNTEKMONEY
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   17
               ADD    WRK-SYU-HKNTEN (IDS2)    TO  SYU-HKNTEN(IDS2)
               ADD    WRK-SYU-MONEY (IDS2)     TO  SYU-MONEY (IDS2)
               ADD    WRK-SYU-TGMONEY (IDS2)   TO  SYU-TGMONEY(IDS2)
               ADD    WRK-SYU-TGMONEY-TAX (IDS2)
                                               TO  SYU-TGMONEY-TAX(IDS2)
           END-PERFORM
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   10
               ADD    WRK-SYU-JIHI-TAXNASI (IDS2)
                                           TO  SYU-JIHI-TAXNASI(IDS2)
               ADD    WRK-SYU-JIHI-TAXARI (IDS2)
                                           TO  SYU-JIHI-TAXARI(IDS2)
           END-PERFORM
      *    処方せん料再掲
           ADD     WRK-SYU-SHOHOU-SAI      TO  SYU-SHOHOU-SAI
           ADD     WRK-SYU-JIHI-TOTAL      TO  SYU-JIHI-TOTAL
           ADD     WRK-SYU-JIHI-TOTAL-TAX  TO  SYU-JIHI-TOTAL-TAX
           ADD     WRK-SYU-JIHI-TAX        TO  SYU-JIHI-TAX
           ADD     WRK-SYU-RJN-FTN         TO  SYU-RJN-FTN
           ADD     WRK-SYU-KOH-FTN         TO  SYU-KOH-FTN
           ADD     WRK-SYU-KOH-FTN-ENTANI  TO  SYU-KOH-FTN-ENTANI
           ADD     WRK-SYU-YKZ-FTN         TO  SYU-YKZ-FTN
      *調整金
           ADD     WRK-SYU-CHOSEI          TO  SYU-CHOSEI
           ADD     WRK-SYU-CHOSEI1         TO  SYU-CHOSEI1
           ADD     WRK-SYU-CHOSEI2         TO  SYU-CHOSEI2
      *労災保険
           ADD     WRK-SYU-RSISHOSHIN-MONEY
                                           TO  SYU-RSISHOSHIN-MONEY
           ADD     WRK-SYU-RSISAISHIN-MONEY
                                           TO  SYU-RSISAISHIN-MONEY
           ADD     WRK-SYU-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           ADD     WRK-SYU-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           ADD     WRK-SYU-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           ADD     WRK-SYU-RSI-TOTAL       TO  SYU-RSI-TOTAL
           ADD     WRK-SYU-RSIJIBAI-SKYMONEY
                                           TO  SYU-RSIJIBAI-SKYMONEY
      *請求金額−消費税（再掲）
           ADD     WRK-SYU-SKYMONEY-TAX-SAI TO  SYU-SKYMONEY-TAX-SAI
      *請求金額
           ADD     WRK-SYU-SKYMONEY        TO  SYU-SKYMONEY
      *入金合計額
           ADD     WRK-SYU-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL 
      *診療区分別給付外点数
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   6
               ADD    WRK-SYU-KYUFUGAI-TEN (IDS2)
                                       TO  SYU-KYUFUGAI-TEN(IDS2)
           END-PERFORM
      *
           .
       450141-SYUNOU-ADD-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    収納データをSPAへ編集処理
      *****************************************************************
       20025-SYUNOU-9999-SEC           SECTION.
      *
           INITIALIZE                  SPA-K03
           MOVE    ZERO                TO  SPA-IDX-KOUI
           INITIALIZE                  SPA-SYUNOU-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU-MAX
               ADD     1                   TO  SPA-SYUNOU-MAX
               MOVE    LNK-SYUNOU-REC(IDX)
                                           TO  SPA-SYUNOU-REC
                                                       (SPA-SYUNOU-MAX)
           END-PERFORM
           .
       20025-SYUNOU-9999-EXT.
           EXIT.
      *****************************************************************
      *    請求確認画面編集処理
      *****************************************************************
       20023-SEIKYU-SYORI-SEC         SECTION.
      *
           INITIALIZE                  SPA-K03
           MOVE    ZERO                TO  SPA-IDX-KOUI
           INITIALIZE                  SPA-SYUNOU-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU2-MAX
               ADD     1                   TO  SPA-SYUNOU-MAX
               MOVE    LNK-SYUNOU2-REC(IDX)
                                           TO  SPA-SYUNOU-REC
                                                       (SPA-SYUNOU-MAX)
           END-PERFORM
      *    診療行為カウント
           MOVE    ORCSCSYU-WKSRYACT-MAX   TO  SPA-IDX-KOUI
      *    入金状態
      *    MOVE    WRK-NYUKIN-JYOTAI   TO  SPA-NAI-NYUKIN-JYOTAI
      *    MOVE    WRK-NYUKIN-HOHO     TO  SPA-GMN-NYUKINHOU
      *    入金取り扱い区分
      *    IF      MDCRES-IC-REQUEST   NOT =   SPACE
      *        MOVE    MDCRES-IC-REQUEST   TO  SPA-GMN-NYUKINKBN
      *    END-IF
      *
           MOVE    "APIV3"             TO  SPA-MOTOPG
      *    初期処理
           MOVE    1                   TO  SPA-GMN-PAGE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K03             TO  SPA-FREE
      *
           MOVE    "HAOR"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           MOVE    SPACE               TO  MCP-WIDGET
      *
           INITIALIZE                  WRK-SCRAREA
           CALL    "ORCGK03"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K03
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    前回未収額等
           PERFORM 200231-GMNINIT-HEN-SEC
      *    収納編集
           PERFORM 200234-SYUNOU-HEN-SEC
      *
           .
       20023-SEIKYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回未収など編集処理
      *****************************************************************
       200231-GMNINIT-HEN-SEC        SECTION.
      *
      *    前回未収額
           MOVE    SPA-GMN-ZENMISYU    TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MAE-MISYU
      *    前回超過額
           MOVE    SPA-GMN-CHOKAKIN    TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MAE-CHOUKA
      *
      *    入金方法
           MOVE    SPA-GMN-NYUKINHOU
                                       TO  MDCRES-IC-CODE
           MOVE    SPA-GMN-NYUKINHOUMEI
                                       TO  MDCRES-IC-CODE-NAME
      *    入金取り扱い区分
           IF     (MDCRES-IC-REQUEST   =   SPACE )
              OR  (FLG-SYORI           =   1     )
               MOVE    SPA-GMN-NYUKINKBN   TO  MDCRES-IC-REQUEST
               MOVE    SPA-GMN-NYUKINKBN-MEI
                                           TO  MDCRES-IC-REQUEST-NAME
           END-IF
      *
      *2019.1
      *    請求書兼領収書発行区分
           IF     (MDCRES-HAKKOU-KBN   =   SPACE )
              OR  (FLG-SYORI           =   1     )
               MOVE    SPA-GMN-HAKHOUFLG   TO  MDCRES-HAKKOU-KBN
               MOVE    SPA-GMN-HAKHOUFLGMSG
                                           TO  MDCRES-HAKKOU-KBN-MEI
           END-IF
      *
      *    今回請求額
           MOVE    SPA-GMN-SEIKYU      TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-TOTAL-AC-MONEY
      *    今回入金額（合計）
           MOVE    SPA-GOK-NYUKIN      TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-TOTAL-IC-MONEY
      *
      *    返金額
           IF      SPA-GMN-HENKIN  NOT =   ZERO
               MOVE    SPA-GMN-HENKIN      TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-TOTAL-RE-MONEY
           END-IF
      *    入金上限額
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPA-NAI-JYOGENKIN      TO  WRK-KINGAKU
               MOVE    WRK-KINGAKU            TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MAX-IC-MONEY
           END-IF
           .
       200231-GMNINIT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       200234-SYUNOU-HEN-SEC        SECTION.
      *
      *!!!!
      *    収納編集合計
           IF     (SPA-SYUNOU-MAX      >   ZERO  )
              AND (SPA-HKNCOMBINUM NOT =   "9999")
               PERFORM 200213-SYUNOU-GOKHEN-SEC
           END-IF
      *
      *    収納情報明細編集（複数科・保険のみ）
           MOVE    ZERO                TO  IDXS
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
               IF      SYU-HKNCOMBINUM NOT =   "9999"
      *            収納明細編集
                   PERFORM 200212-SYUNOU-OUTHEN-SEC
               END-IF
           END-PERFORM
           .
       200234-SYUNOU-HEN-EXT.
           EXIT.
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
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   COB-ZAI-OUTMAX )
      *???             OR  (ORAPISUB2-MDC-CLASS-KBN (IDX)
      *??                                  =   SPACE )
                       OR ((ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           =   SPACE )   AND
                           (ORAPISUB2-MDC-DATA-KBN (IDX)
                                           =   SPACE ))
      *!!!
      *        複数科・保険項目
               MOVE    ORAPISUB2-MDC-DATA-KBN (IDX)
                                           TO  MDCRES-MDC-DATA-KBN(IDX)
               MOVE    ORAPISUB2-MDC-COMBINATION (IDX)
                                       TO  MDCRES-MDC-COMBINATION-NUMBER
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-MAIN-INS-NAME (IDX)
                                       TO  MDCRES-MDC-MAIN-INS-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-PUBLIC-INSURANCE-INFO (IDX)
                                       TO  MDCRES-MDC-PUBLIC-INS-INFO
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DEP-CODE (IDX)
                                       TO  MDCRES-MDC-DEP-CODE
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DEP-CODE-NAME (IDX)
                                       TO  MDCRES-MDC-DEP-CODE-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DR-CODE (IDX)
                                       TO  MDCRES-MDC-DR-CODE
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DR-CODE-NAME (IDX)
                                       TO  MDCRES-MDC-DR-CODE-NAME
                                                               (IDX)
      *!!!
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
      *H29.5 削除  一般名称があれば、名称は削除
      *            IF      ORAPISUB2-PRSCRPT-GENNAME (IDX IDY)
      *                                    NOT =   SPACE
      *                MOVE    SPACE   TO  MDCRES-PRSCRPT-NAME
      *                                                     (IDX IDY)
      *****        END-IF
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
      *        剤削除区分
               IF      ORAPISUB2-DEL-INF (IDX) =   "1"
                   MOVE    "Delete"            TO  MDCRES-DEL-INF (IDX)
               END-IF
           END-PERFORM
           IF     (ORAPISUB2-ERRCD NOT =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
      *******  MOVE    ORAPISUB2-ERRCD     TO  WRK-ERRCD
               MOVE    "13"                TO  WRK-ERRCD
               MOVE    ORAPISUB2-ERRMSG    TO  WRK-ERRMSG
           END-IF
      *
           .
       200211-MEDICAL-OUTHEN-EXT.
           EXIT.
      *
      *!!!!
      *!!!!
      *
      *****************************************************************
      *    収納情報合計処理（合計）
      *****************************************************************
       2002131-SYUNOU-ADD-SEC        SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    ZERO                TO  WRK-SYUNOU-IDX
      ***  MOVE    ZERO                TO  FLG-FUKU
      *
           INITIALIZE                  WRK-SYU-TEI-ADD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
      *        マル長の公費負担を保険適用金額へ
               IF      WRK-SYU-HKNTEKKBN   =   "1"
                   MOVE    WRK-SYU-KOH-FTN     TO  WRK-SYU-HKNTEKMONEY
                   MOVE    ZERO                TO  WRK-SYU-KOH-FTN
               END-IF
      *
               IF      SPA-GMN-HKNCOMBI    =   "0000"
                   IF       SPA-GMN-SRYKA      =   WRK-SYU-SRYKA
      *                集計
                       IF      WRK-SYUNOU-IDX      =   ZERO
                           MOVE    WRK-SYUNOU-REC  TO  SYUNOU-REC
                           MOVE    IDX             TO  WRK-SYUNOU-IDX
                           IF     (SPA-SYORIFLG         =   1    )
      *                        訂正内容集計
                               MOVE    IDX             TO  IDX-A
                               PERFORM 20021311-SYUNOU-TEIADD-SEC
                           END-IF
                       ELSE
                           PERFORM 31000-SYUNOU-ADD-SEC
      ***                  MOVE    1               TO  FLG-FUKU
                       END-IF
                   END-IF
               ELSE
                   IF     (SPA-GMN-HKNCOMBI    =   WRK-SYU-HKNCOMBINUM)
                     AND  (SPA-GMN-SRYKA       =   WRK-SYU-SRYKA )
                       MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
                       IF     (SPA-SYORIFLG         =   1    )
      *                    訂正内容集計
                           MOVE    IDX             TO  IDX-A
                           PERFORM 20021311-SYUNOU-TEIADD-SEC
                       END-IF
                       MOVE    IDX                 TO  WRK-SYUNOU-IDX
                       MOVE    SPA-SYUNOU-MAX      TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *    全科の時、単独科の保険を集計
           IF      SPA-GMN-SRYKA       =   ZERO
               PERFORM VARYING     IDX4    FROM    1   BY  1
                       UNTIL      (IDX4    >   15  )  OR
                                  (SPA-TBL-HKNCOMBI (IDX4)  =   SPACE )
                   IF     (SPA-GMN-HKNCOMBI    =   "0000" ) OR
                          (SPA-GMN-HKNCOMBI    =   SPA-TBL-HKNCOMBI
                                                            (IDX4))
                       IF      SPA-TBL-SRYKA (IDX4 2)   =   SPACE
                           MOVE    SPA-TBL-HKNCOMBI(IDX4)
                                                   TO  WRK-HKNCOMBI
                           MOVE    SPA-TBL-SRYKA (IDX4 1)
                                                   TO  WRK-SRYKA
                           PERFORM 31001-ZENNKA-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2002131-SYUNOU-ADD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    訂正内容集計処理
      *****************************************************************
       20021311-SYUNOU-TEIADD-SEC      SECTION.
      *
      *    訂正新規就農
           IF     (WRK-SYU-DENPNUM     =   ZERO )
      *        請求金額
               ADD     WRK-SYU-SKYMONEY
                                           TO  WRK-NAI-SEIKYU
      *        請求金額−消費税（再掲）
               ADD     WRK-SYU-SKYMONEY-TAX-SAI
                                           TO  WRK-NAI-SKYMONEY-TAX-SAI
               GO      TO      20021311-SYUNOU-TEIADD-EXT
           END-IF
      *
      *    収納訂正
           ADD     SPA-TEIKEI-SKYMONEY(IDX-A)
                                           TO  WRK-NAI-SEIKYU
           ADD     SPA-TEI-SKYMONEY-TAX-SAI(IDX-A)
                                           TO  WRK-NAI-SKYMONEY-TAX-SAI
           ADD     SPA-TMAE-CHOSEI (IDX-A)
                                           TO  WRK-TMAE-CHOSEI
           ADD     SPA-TMAE-CHOSEI1(IDX-A)
                                           TO  WRK-TMAE-CHOSEI1
           ADD     SPA-TMAE-CHOSEI2(IDX-A)
                                           TO  WRK-TMAE-CHOSEI2
           ADD     SPA-MAE-SKYMONEY(IDX-A)
                                           TO  WRK-OLD-SEIKYU
      *    入金差額
           ADD     SPA-MAE-NYUKIN-TOTAL(IDX-A)
                                           TO  WRK-NAI-NYUKIN
      *    返金額
           ADD     SPA-SYU-HENKIN  (IDX-A)
                                           TO  WRK-NAI-HENKIN
      *
           .
       20021311-SYUNOU-TEIADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    全科収納編集処理
      *****************************************************************
       31001-ZENNKA-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX)     TO  WRK-SYUNOU-REC
               IF     (WRK-HKNCOMBI        =   WRK-SYU-HKNCOMBINUM) AND
                      (WRK-SRYKA           =   WRK-SYU-SRYKA      )
                   IF      SYU-PTID        =   ZERO
                       MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
                       IF     (SPA-SYORIFLG         =   1    )
      *                    訂正内容集計
                           MOVE    IDX                 TO  IDX-A
                           PERFORM 20021311-SYUNOU-TEIADD-SEC
                       END-IF
                   ELSE
                       PERFORM 31000-SYUNOU-ADD-SEC
                   END-IF
               END-IF
           END-PERFORM
            .
       31001-ZENNKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    収納集計処理
      *****************************************************************
       31000-SYUNOU-ADD-SEC             SECTION.
      *
      *    収納更新・科合計追加処理
           PERFORM 450141-SYUNOU-ADD-HEN-SEC
      *
      **** IF     (WRK-SYU-DENPNUM     >   ZERO )
           IF     (SPA-SYORIFLG        =   1    )
      *        訂正前集計
               MOVE    IDX                 TO  IDX-A
               PERFORM 20021311-SYUNOU-TEIADD-SEC
           END-IF
      *
      *    減免区分
           IF     (WRK-SYU-DISCOUNT-TEIRITU    NOT =   SPACE )
               MOVE    WRK-SYU-DISCOUNT-TEIRITU
                                               TO  SYU-DISCOUNT-TEIRITU
               MOVE    WRK-SYU-DISCOUNT-RATE
                                               TO  SYU-DISCOUNT-RATE
           END-IF
            .
       31000-SYUNOU-ADD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    収納情報編集処理（合計）
      *****************************************************************
       200213-SYUNOU-GOKHEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-DENPNUM
      *
      *    画面初期表示内容
           IF      SPA-SYUNOU-IDX      =   ZERO
      *        主たる伝票番号
               IF      SPA-SYUNOU-MAX      >=  1
                   MOVE    SPA-SYUNOU-REC(1)   TO  SYUNOU-REC
                   MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
               END-IF
      *
      *        保険合計を集計する
               PERFORM 2002131-SYUNOU-ADD-SEC
      *        画面を収納へ
               MOVE    SPA-GMN-SRYKA       TO  SYU-SRYKA
               MOVE    SPA-GMN-HKNCOMBI    TO  SYU-HKNCOMBINUM
               MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
               MOVE    ZERO                TO  IDX
           ELSE
               MOVE    SPA-SYUNOU-REC (SPA-SYUNOU-IDX)
                                           TO  SYUNOU-REC
               MOVE    SPA-SYUNOU-IDX      TO  IDX
               MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
           END-IF
      *
      *    保険組合せ番号
           MOVE    SPA-GMN-HKNCOMBI    TO  MDCRES-CD-COMBINATION-NUMBER
           IF      SPA-GMN-HKNCOMBI    =   ZERO
               MOVE    SPA-GMN-HKNCOMBIMEI
                                       TO  MDCRES-CD-COMBINATION-NAME
           END-IF
      *    診療科
           MOVE    SPA-GMN-SRYKA       TO  MDCRES-CD-DEP-CODE
           IF      SPA-GMN-SRYKA       =   ZERO
               MOVE    SPA-GMN-SRYKAMEI    TO  MDCRES-CD-DEP-CODE-NAME
           END-IF
      *
      *    伝票番号
           IF     SYU-DENPNUM      NOT =   ZERO
               MOVE    SYU-DENPNUM         TO  MDCRES-INVOICD-NUMBER
           END-IF
      *
      *    減免区分
           IF      SYU-DISCOUNT-TEIRITU    NOT =   SPACE
               EVALUATE    SYU-DISCOUNT-TEIRITU
               WHEN    "1"
                   MOVE    SYU-DISCOUNT-RATE   TO  WRK-ZZ9
                   STRING  WRK-ZZ9
                           "％"            DELIMITED  BY  SIZE
                                           INTO    MDCRES-DISCOUNT-NAME
                   END-STRING
               WHEN    "2"
                   MOVE    SYU-DISCOUNT-RATE   TO  WRK-Z97
                   STRING  WRK-Z97
                           "円"            DELIMITED  BY  SIZE
                                           INTO    MDCRES-DISCOUNT-NAME
                   END-STRING
               END-EVALUATE
           END-IF
      *
      *    保険適用金額
           MOVE    SYU-HKNTEKMONEY     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AI-MONEY
      *
      *    自費金額（自費合計金額）
           COMPUTE WRK-KINGAKU         =   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TOTAL
      *                                保険適応外
                                       +   SYU-TGMONEY (17)
                                       +   SYU-TGMONEY-TAX (17)
           MOVE    WRK-KINGAKU         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-OE-MONEY
      *    薬剤一部負担金
           MOVE    SYU-YKZ-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-DG-SMONEY  
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-OM-SMONEY
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-PI-SMONEY
      *    労災金額合計
      *    労災・自賠責金額
           COMPUTE WRK-KINGAKU         =   SYU-RSISHOSHIN-MONEY
                                       +   SYU-RSISAISHIN-MONEY
                                       +   SYU-RSISDO-MONEY
                                       +   SYU-RSIETC-MONEY
           MOVE    WRK-KINGAKU         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-LSI-TOTALMONEY
      *
      *    請求点数情報
      *    保険点数明細
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL      IDY      >   16
      *        手術料・療養担当手当は、点数ありのみ
               IF     (IDY             =   15   OR  16  )
                  AND (SYU-HKNTEN (IDY)    =   ZERO     )
                  AND (SYU-TGMONEY (IDY)    =   ZERO     )
                  AND (SYU-TGMONEY-TAX(IDY) =   ZERO     )
                   CONTINUE
               ELSE
      *
                   MOVE    TBL-HKNTEN-MEI(IDY)
                                           TO  MDCRES-AC-POINT-NAME(IDY)
                   MOVE    SYU-HKNTEN(IDY) TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-AC-POINT (IDY)
      *            保険適用外
                   COMPUTE WRK-SURYO       =   SYU-TGMONEY (IDY)
                                           +   SYU-TGMONEY-TAX(IDY)
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-AC-TGMONEY (IDY)
               END-IF
           END-PERFORM
      *    保険合計点数
           MOVE    SYU-HKNTEN(17)      TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AC-TOTAL-POINT
      *    自費
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   10
               MOVE    SPA-GMN-JIHIMSG (IDY)   TO  MDCRES-JIHI-NAME
                                                          (IDY)
      *        自費
               MOVE    SYU-JIHI-TAXNASI(IDY)   TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                               TO  MDCRES-JIHI-MONEY
                                                          (IDY)
      *        自費（消費税あり）
               MOVE    SYU-JIHI-TAXARI(IDY)    TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                               TO  MDCRES-JIHI-MONEY-TAX
                                                          (IDY)
           END-PERFORM
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-JIHI-TAX
      *
      *    請求金額（調整金を含まない）
           IF     SYU-DENPNUM          =   ZERO
               MOVE    SYU-SKYMONEY        TO  WRK-SURYO
           ELSE
               IF     (SPA-SYORIFLG        =   1   )
                   IF      IDX                 =   ZERO
                       MOVE    WRK-NAI-SEIKYU      TO  WRK-SURYO
                   ELSE
                       MOVE    SPA-TEIKEI-SKYMONEY(IDX)  TO  WRK-SURYO
                   END-IF
               ELSE
      *            新規で登録後
                   COMPUTE WRK-SURYO       =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
               END-IF
           END-IF
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AC-MONEY
      *
      *    エラー時は画面情報
      *    IF     (FLG-UPDEND          =   ZERO )
      *       AND ((SPA-ERRCD      NOT =   SPACE) OR
      *            (WRK-ERRCD      NOT =   SPACE))
      *       AND (SYU-SRYKA           =   SPA-GMN-SRYKA)
      *       AND (SYU-HKNCOMBINUM     =   SPA-GMN-HKNCOMBI)
      *        入金額
      *        MOVE    SPA-GMN-NYUKIN      TO  WRK-SURYO
      *        PERFORM 810-NUMHENKAN-SEC
      *        MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
      *                                    TO  MDCRES-IC-MONEY
      *        返金額
      *        IF      SPA-GMN-HENKIN      NOT =   ZERO
      *            MOVE    SPA-GMN-HENKIN  TO  WRK-SURYO
      *            PERFORM 810-NUMHENKAN-SEC
      *            MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
      *                                    TO  MDCRES-RE-MONEY
      *         END-IF
      *         調整金額１
      *****     MOVE    WRK-GOK-CHOSEI1    TO  WRK-SURYO
      *         MOVE    SYU-CHOSEI1         TO  WRK-SURYO
      *         PERFORM 810-NUMHENKAN-SEC
      *         MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
      *                                    TO  MDCRES-AD-MONEY1
      *         調整金額２
      *****     MOVE    WRK-GOK-CHOSEI2    TO  WRK-SURYO
      *         MOVE    SYU-CHOSEI2         TO  WRK-SURYO
      *         PERFORM 810-NUMHENKAN-SEC
      *         MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
      *                                    TO  MDCRES-AD-MONEY2
      *    ELSE
      *
      *    入金金額
           IF     SYU-DENPNUM          =   ZERO
               MOVE    SYU-NYUKIN-TOTAL        TO  WRK-SURYO
           ELSE
               IF      IDX             =   ZERO
                   MOVE    SYU-NYUKIN-TOTAL        TO  WRK-SURYO
               ELSE
                   MOVE    SPA-TEI-NYUKIN-TOTAL(IDX)
                                                   TO  WRK-SURYO
               END-IF
           END-IF
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-IC-MONEY
      *
      *    返金額
           IF      IDX             =   ZERO
               MOVE    SPA-NAI-HENKIN      TO  WRK-SYU-HENKIN
           ELSE
               MOVE    SPA-SYU-HENKIN(IDX) TO  WRK-SYU-HENKIN
           END-IF
           IF      WRK-SYU-HENKIN      NOT =   ZERO
               MOVE    WRK-SYU-HENKIN      TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-RE-MONEY
           END-IF
      *
      *    調整金額１
           MOVE    SYU-CHOSEI1         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AD-MONEY1
      *    調整金額２
           MOVE    SYU-CHOSEI2         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AD-MONEY2
      *
           IF     (SPA-SYORIFLG        =   1   )
      *        前回までの入金額
               IF      IDX                 =   ZERO
                   MOVE    WRK-NAI-NYUKIN          TO  WRK-SURYO
               ELSE
                   MOVE    SPA-MAE-NYUKIN-TOTAL(IDX)   TO  WRK-SURYO
               END-IF
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-IC-MONEY
      *        前回請求額（調整金を含まない）
               IF      IDX                 =   ZERO
                   COMPUTE WRK-SURYO       =   WRK-OLD-SEIKYU
                                           -   WRK-TMAE-CHOSEI
               ELSE
                   COMPUTE WRK-SURYO       =   SPA-MAE-SKYMONEY (IDX)
                                           -   SPA-TMAE-CHOSEI (IDX)
               END-IF
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-AC-MONEY
      *        前回調整金額１
               IF      IDX                 =   ZERO
                   MOVE    WRK-TMAE-CHOSEI1        TO  WRK-SURYO
               ELSE
                   MOVE    SPA-TMAE-CHOSEI1(IDX)   TO  WRK-SURYO
               END-IF
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-AD-MONEY1
      *        前回調整金額２
               IF      IDX                 =   ZERO
                   MOVE    WRK-TMAE-CHOSEI2        TO  WRK-SURYO
               ELSE
                   MOVE    SPA-TMAE-CHOSEI2(IDX)   TO  WRK-SURYO
               END-IF
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-AD-MONEY2
           END-IF
           .
       200213-SYUNOU-GOKHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       200212-SYUNOU-OUTHEN-SEC        SECTION.
      *
           ADD     1                   TO  IDXS
      *
      *    診療科
           MOVE    SYU-SRYKA           TO  MDCRES-SUB-DEP-CODE
                                                              (IDXS)
           IF      SYU-SRYKA           =   ZERO
      *        診療科合計
               MOVE    "全科"          TO  MDCRES-SUB-DEP-CODE-NAME
                                                              (IDXS)
           END-IF
      *
           MOVE    SYU-HKNCOMBINUM     TO  MDCRES-SUB-COMBINATION-NUMBER
                                                              (IDXS)
           IF      SYU-SRYKA       NOT =   ZERO
      *        ドクター
               MOVE    SYU-DRCD-G          TO  MDCRES-SUB-DR-CODE
                                                              (IDXS)
           END-IF
      *    伝票番号
           IF    (SYU-DENPNUM      NOT =   ZERO )
             AND (SYU-SRYKA        NOT =   ZERO )
               MOVE    SYU-DENPNUM     TO  MDCRES-SUB-INVOICD-NUMBER
                                                              (IDXS)
           END-IF
      *
      *    保険適用金額
           MOVE    SYU-HKNTEKMONEY     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-AI-MONEY
                                                              (IDXS)
      *
      *    自費金額（自費合計金額）
           COMPUTE WRK-KINGAKU         =   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TOTAL
      *                                保険適応外
                                       +   SYU-TGMONEY (17)
                                       +   SYU-TGMONEY-TAX (17)
           MOVE    WRK-KINGAKU         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-OE-MONEY
                                                              (IDXS)
      *    薬剤一部負担金
           MOVE    SYU-YKZ-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-DG-SMONEY  
                                                              (IDXS)
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-OM-SMONEY
                                                              (IDXS)
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-PI-SMONEY
                                                              (IDXS)
      *    労災金額合計
      *    労災・自賠責金額
           COMPUTE WRK-KINGAKU         =   SYU-RSISHOSHIN-MONEY
                                       +   SYU-RSISAISHIN-MONEY
                                       +   SYU-RSISDO-MONEY
                                       +   SYU-RSIETC-MONEY
           MOVE    WRK-KINGAKU         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-LSI-TOTALMONEY
                                                              (IDXS)
      *
      *    請求点数情報
      *    保険点数明細
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL      IDY      >   16
      *        手術料・療養担当手当は、点数ありのみ
               IF     (IDY             =   15   OR  16  )
                  AND (SYU-HKNTEN (IDY)    =   ZERO     )
                  AND (SYU-TGMONEY (IDY)    =   ZERO     )
                  AND (SYU-TGMONEY-TAX(IDY) =   ZERO     )
                   CONTINUE
               ELSE
      *
                   MOVE    TBL-HKNTEN-MEI(IDY)
                                           TO  MDCRES-SUB-AC-POINT-NAME
                                                          (IDXS IDY)
                   MOVE    SYU-HKNTEN(IDY) TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-AC-POINT
                                                          (IDXS IDY)
      *            保険適用外
                   COMPUTE WRK-SURYO       =   SYU-TGMONEY (IDY)
                                           +   SYU-TGMONEY-TAX(IDY)
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-AC-TGMONEY
                                                          (IDXS IDY)
               END-IF
           END-PERFORM
      *    保険合計点数
           MOVE    SYU-HKNTEN(17)      TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-AC-TOTAL-POINT
                                                              (IDXS)
      *    自費
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   10
               MOVE    SPA-GMN-JIHIMSG (IDY)   TO  MDCRES-SUB-JIHI-NAME
                                                          (IDXS IDY)
      *        自費
               MOVE    SYU-JIHI-TAXNASI(IDY)   TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                               TO  MDCRES-SUB-JIHI-MONEY
                                                          (IDXS IDY)
      *        自費（消費税あり）
               MOVE    SYU-JIHI-TAXARI(IDY)    TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-JIHI-MONEY-TAX
                                                          (IDXS IDY)
           END-PERFORM
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-JIHI-TAX
                                                              (IDXS)
      *
      *    請求金額（調整金を含まない）
           IF     SYU-DENPNUM          =   ZERO
               MOVE    SYU-SKYMONEY        TO  WRK-SURYO
           ELSE
               IF     (SPA-SYORIFLG        =   1   )
                   MOVE    SPA-TEIKEI-SKYMONEY (IDX)  TO  WRK-SURYO
               ELSE
      *            新規で登録後
                   COMPUTE WRK-SURYO       =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
               END-IF
           END-IF
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-AC-MONEY
                                                              (IDXS)
      *
      *    入金金額
           IF     SYU-DENPNUM          =   ZERO
               MOVE    SYU-NYUKIN-TOTAL        TO  WRK-SURYO
           ELSE
               MOVE    SPA-TEI-NYUKIN-TOTAL(IDX)
                                               TO  WRK-SURYO
           END-IF
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-IC-MONEY
                                                              (IDXS)
      *    返金額
           IF      SPA-SYU-HENKIN (IDX)    NOT =   ZERO
               MOVE    SPA-SYU-HENKIN(IDX) TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-RE-MONEY
                                                              (IDXS)
           END-IF
      *
      *    調整金額１
           MOVE    SYU-CHOSEI1         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-AD-MONEY1
                                                              (IDXS)
      *    調整金額２
           MOVE    SYU-CHOSEI2         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-SUB-AD-MONEY2
                                                              (IDXS)
      *
           IF     (SPA-SYORIFLG        =   1   )
      *        前回までの入金額
               MOVE    SPA-MAE-NYUKIN-TOTAL(IDX)   TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-OLD-IC-MONEY
                                                              (IDXS)
      *        前回請求額（調整金を含まない）
               COMPUTE WRK-SURYO       =   SPA-MAE-SKYMONEY (IDX)
                                       -   SPA-TMAE-CHOSEI (IDX)
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-OLD-AC-MONEY
                                                              (IDXS)
      *        前回調整金額１
               MOVE    SPA-TMAE-CHOSEI1(IDX)   TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-OLD-AD-MONEY1
                                                              (IDXS)
      *        前回調整金額２
               MOVE    SPA-TMAE-CHOSEI2(IDX)   TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-OLD-AD-MONEY2
                                                              (IDXS)
           END-IF
      *
      *    複数科まとめ差額
           IF     (SYU-GRP-SGKMONEY    NOT =   ZERO )
             AND  (SYU-SRYKA           NOT =   ZERO )
               MOVE    SYU-GRP-SGKMONEY    TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-SUB-GRP-SGKMONEY
                                                              (IDXS)
           END-IF
           .
       200212-SYUNOU-OUTHEN-EXT.
           EXIT.
      *****************************************************************
      *    データ保存処理
      *****************************************************************
       20029-PARA-INS-SEC     SECTION.
      *
      *    収納データ更新
           PERFORM 200292-SYUNOU-UPD-SEC
      *    SPA保存
           PERFORM 2008-SPATMP-INS-SEC
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
      *    収納データ更新処理
      *****************************************************************
       200292-SYUNOU-UPD-SEC           SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
           PERFORM 30012-PARA-DBDELETE-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU2-MAX
      *        TBL_PARAに保存
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "SYUNOU"            TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    LNK-SYUNOU2-REC(IDX)
                                           TO  PARA-DATA-REC
      *
               PERFORM 30010-PARA-DBINSERT-SEC
      *
           END-PERFORM
           .
       200292-SYUNOU-UPD-EXT.
           EXIT.
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
      *    入金額等編集
           PERFORM 20031-HENSYU-02-SEC
      *
      *    診療行為内容編集
           PERFORM 20021-HENSYU-01-SEC
      *
      *
           IF     (WRK-ERRCD       =   SPACE )
             AND  (FLG-UPDATE      =   1     )
      *        登録処理
               PERFORM 20032-TOUROKU-SYORI-SEC
               MOVE    1            TO  FLG-UPDEND
           END-IF
      *
      *    更新後返却内容編集
      *??  IF     (SPA-HKNCOMBINUM NOT =   "9999" )
           IF     (FLG-UPDEND          =   1      )
      *        収納編集
               PERFORM 200234-SYUNOU-HEN-SEC
           END-IF
      *
           IF      FLG-UPDATE      =   ZERO
      *        更新なし
               IF     (SPA-ERRCD       =   SPACE )
                  AND (WRK-ERRCD       =   SPACE )
                   MOVE    "05"        TO  MDCRES-RESPONSE-NUMBER
               END-IF
               GO      TO  2003-SYORI-02-EXT
           END-IF
      *
      *H29.6
      *    帳票印刷指示
           IF     (SPA-ERRCD       =   SPACE )
              AND (WRK-ERRCD       =   SPACE )
               PERFORM 20034-PRINT-SYORI-SEC
           END-IF
      *
      *    返却レスポンス番号
           IF     (SPA-ERRCD       =   SPACE )
              AND (WRK-ERRCD       =   SPACE )
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
      *****************************************************************
      *    診療確認処理
      *****************************************************************
       20031-HENSYU-02-SEC        SECTION.
      *
      *    収納レコード編集
      ***  PERFORM 20020-SYUNOU-PARA-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K03"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-K03
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
      *    一括入返金区分チェック処理
           IF    ( MDCREQ-IC-ALLKBN    =   "1" )
               IF     (MDCREQ-IC-REQUEST   =   "2"
                                           OR  "3"
                                           OR  "4"
                                           OR  "5" )
                AND  ((SPA-GMN-ZENMISYU    NOT =   ZERO )  OR
                      (SPA-GMN-CHOKAKIN    NOT =   ZERO ))
                   CONTINUE
               ELSE
                   IF     (SPA-SYORIFLG        =   ZERO   )
                       MOVE    "60"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    入金の取り扱い設定
           IF      MDCRES-IC-REQUEST   NOT =   SPACE
               MOVE    MDCRES-IC-REQUEST   TO  SPA-GMN-NYUKINKBN
           END-IF
      *
      *
      *    返金額のチェック
           IF      WRK-HENKIN-OK       =   1
               IF     (SPA-SYORIFLG        =   ZERO   )
      *            前回超過額なし
                   IF     (SPA-GMN-CHOKAKIN    =   ZERO   )
                      OR  (SPA-GMN-NYUKINKBN   =   "1"    )
                       IF      WRK-ERRCD           =   SPACE
                           MOVE    "27"                TO  WRK-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    一括入返金の時、入金額・返金額の送信はなしとする
           IF    ( MDCREQ-IC-ALLKBN    =   "1" )
             AND ( WRK-ERRCD           =   SPACE )
               MOVE    ZERO                TO  WRK-HENKIN-OK
               MOVE    ZERO                TO  WRK-NYUKIN-OK
               PERFORM VARYING     IDC     FROM    1   BY  1
                        UNTIL      (IDC     >   15  )
                               OR  (WRK-IN-SRYKA (IDC)  =   SPACE)
                   IF      WRK-IN-NYUKIN-OK (IDC)  =   1
                       MOVE    ZERO            TO  WRK-IN-NYUKIN-OK
                                                               (IDC)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    訂正で複数収納の時、入金額のチェック
           IF    ( SPA-SYORIFLG        =   1      )
             AND ( WRK-ERRCD           =   SPACE  )
             AND ( WRK-NYUKIN-OK       =   1      )
             AND ( SPA-SYUNOU-MAX      >   1      )
      *        今回入金額と違う時、エラー
               IF      SPA-GOK-NYUKIN  NOT =   WRK-NYUKIN
                   MOVE    "75"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    入金・請求額
           IF     (WRK-ERRCD           =   SPACE)
               IF     (SPA-HKNCOMBINUM     NOT =   "9999" )
      *            請求確認画面編集処理
                   PERFORM 200311-SEIKYU-SYORI-02-SEC
               END-IF
           END-IF
      *
           .
       20031-HENSYU-02-EXT.
           EXIT.
      *****************************************************************
      *    請求確認画面編集処理
      *****************************************************************
       200311-SEIKYU-SYORI-02-SEC      SECTION.
      *
      *    入金の取り扱い設定
           IF      MDCRES-IC-REQUEST   NOT =   SPACE
               MOVE    MDCRES-IC-REQUEST   TO  SPA-GMN-NYUKINKBN
           END-IF
      *
           MOVE    ZERO                TO  WRK-GOK-CHOSEI1
           MOVE    ZERO                TO  WRK-GOK-CHOSEI2
           MOVE    SPACE               TO  WRK-ERRCD2
      *    保存
           MOVE    SPA-K03             TO  HZN-SPA-K03
      *
           MOVE    ZERO                TO  WRK-NAI-HENFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SYUNOU-MAX )
                          OR  (WRK-ERRCD   NOT =   SPACE  )
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
      *
               IF      SYU-SRYKA       NOT =   ZERO
      *
                   MOVE    ZERO                TO  TBL-IDC
                   PERFORM VARYING     IDC     FROM    1   BY  1
                           UNTIL      (IDC     >   15  )
                                  OR  (WRK-IN-SRYKA (IDC)  =   SPACE)
                                  OR  (TBL-IDC     NOT =   ZERO  )
                       IF      (WRK-IN-HKNCOMBI(IDC)
                                               =   SYU-HKNCOMBINUM )
                           AND (WRK-IN-SRYKA (IDC)
                                               =   SYU-SRYKA)
      *                    調整金送信あり
                           MOVE    IDC             TO  TBL-IDC
                       END-IF
                   END-PERFORM
      *            調整金チェック
                   PERFORM 200311-INPUT-CHOSEI-CHK-SEC
      *            訂正時の返金チェック
                   IF     (WRK-ERRCD           =   SPACE )
                     AND  (WRK-HENKIN-OK       =   1     )
                     AND  (SPA-SYORIFLG        =   1     )
                    AND   (WRK-HENKIN      NOT =   ZERO  )
                       PERFORM 200311-INPUT-HENKIN-CHK-SEC
                   END-IF
                   IF      WRK-ERRCD           =   SPACE
      *                調整金等送信あり
                       IF     (TBL-IDC             >   ZERO  )
                         AND ((WRK-IN-NYUKIN-OK(TBL-IDC)   =   1 )
                           OR (WRK-CHOSEI-OK   (TBL-IDC)   =   1 ))
                           PERFORM 200313-K03CALL4-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    SYU-SRYKA           TO  WRK-ERR-SRYKA
                       MOVE    SYU-HKNCOMBINUM     TO  WRK-ERR-HKNCOMBI
                   END-IF
      *
               END-IF
           END-PERFORM
      *
      *    訂正時の返金エラーがあり返金対象なしの時
           IF      (WRK-NAI-HENFLG     =   1   )
      *        調整金エラーあり
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    ZERO            TO  WRK-NAI-HENFLG
               ELSE
      *            未収の収納あり
                   IF      WRK-ERRCD2      =   "28"
                       MOVE    "76"            TO  WRK-KEICD
                   END-IF
                   MOVE    SPACE           TO  WRK-ERRCD2
               END-IF
           END-IF
           IF     (WRK-NAI-HENFLG      =   ZERO  )
              AND (WRK-ERRCD2      NOT =   SPACE )
               IF      WRK-ERRCD       =   SPACE
                   MOVE    WRK-ERRCD2      TO  WRK-ERRCD
                   MOVE    WRK-ERR2-SRYKA  TO  WRK-ERR-SRYKA
                   MOVE    WRK-ERR2-HKNCOMBI
                                               TO  WRK-ERR-HKNCOMBI
               END-IF
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200311-SEIKYU-SYORI-02-EXT
           END-IF
      *
      *    入金・調整金チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200311-INPUT-CHK-SEC
           END-IF
      *
      *    前回未収額等
           PERFORM 200231-GMNINIT-HEN-SEC
      *    収納編集
           PERFORM 200234-SYUNOU-HEN-SEC
      *
           .
       200311-SEIKYU-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力内容チェック処理
      *****************************************************************
       2003112-K03CALL-SYORI-SEC       SECTION.
      *
      *    入金状態
           MOVE    WRK-NYUKIN-JYOTAI   TO  SPA-NAI-NYUKIN-JYOTAI
           MOVE    WRK-NYUKIN-HOHO     TO  SPA-GMN-NYUKINHOU
      *
           IF      MDCRES-IC-REQUEST   NOT =   SPACE
               MOVE    MDCRES-IC-REQUEST   TO  SPA-GMN-NYUKINKBN
           END-IF
      *    請求書作成区分
           MOVE    MDCRES-PRINT-SEIKYU TO  SPA-GMN-HAKFLG
      *    伝票発行日
           MOVE    WRK-BASE-YMD        TO  SPA-GMN-HAKYMD
           MOVE    WRK-BASE-YMD        TO  SPA-NAI-HAKYMD
      *
           MOVE    "APIV3"             TO  SPA-MOTOPG
      *    チェック処理
           MOVE    2                   TO  SPA-GMN-PAGE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K03             TO  SPA-FREE
      *
           MOVE    "HAOR"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           MOVE    SPACE               TO  MCP-WIDGET
      *
           INITIALIZE                  WRK-SCRAREA
           CALL    "ORCGK03"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K03
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200329-ERRCD-MSG-SEC
               MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
           END-IF
           .
       2003112-K03CALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     調整金入力内容チェック処理
      *****************************************************************
       200311-INPUT-CHOSEI-CHK-SEC  SECTION.
      *
      *    請求額（請求額- 調整金）
           IF      SYU-SKYMONEY    <=  SYU-CHOSEI
               MOVE    ZERO            TO  WRK-TEIKEI-SKYMONEY
           ELSE
               COMPUTE WRK-TEIKEI-SKYMONEY
                                       =   SYU-SKYMONEY
                                       -   SYU-CHOSEI
           END-IF
      *
           IF     (TBL-IDC         >   ZERO )
             AND  (WRK-CHOSEI-OK (TBL-IDC)  =   1   )
               MOVE    WRK-CHOSEI1 (TBL-IDC)   TO  WRK-IN-CHOSEI1
               MOVE    WRK-CHOSEI2 (TBL-IDC)   TO  WRK-IN-CHOSEI2
           ELSE
               MOVE    SYU-CHOSEI1             TO  WRK-IN-CHOSEI1
               MOVE    SYU-CHOSEI2             TO  WRK-IN-CHOSEI2
           END-IF
      *    今回合計請求額（請求額＋今回調整金）
           COMPUTE WRK-SKYMONEY-GOK    =   WRK-TEIKEI-SKYMONEY
                                       +   WRK-IN-CHOSEI1
                                       +   WRK-IN-CHOSEI2
           IF      WRK-SKYMONEY-GOK    <   ZERO
               MOVE    "22"            TO  WRK-ERRCD
           END-IF
      *
      *    ７桁数チェック
           IF      WRK-SKYMONEY-GOK    >   9999999
               MOVE    "29"            TO  WRK-ERRCD
           END-IF
      *
           .
       200311-INPUT-CHOSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     返金額のチェック処理（訂正）
      *****************************************************************
       200311-INPUT-HENKIN-CHK-SEC     SECTION.
      *
      *    訂正入金額（今回請求額-前回請求額）
           COMPUTE WRK-SEIKYU      =   WRK-SKYMONEY-GOK
                                   -   SPA-MAE-SKYMONEY (IDX)
           IF      WRK-SEIKYU      >=  ZERO
               IF      WRK-ERRCD2      =   SPACE
                   MOVE    "27"                TO  WRK-ERRCD2
               END-IF
           END-IF
           IF     (WRK-SEIKYU      <   ZERO  )
      *        前回入金が未収の時
               IF     (SPA-MAE-SKYMONEY(IDX)
                                       +   SPA-TMAE-CHOSEI (IDX))
                                       <=  SPA-MAE-NYUKIN-TOTAL
                                                             (IDX)
                   MOVE    1               TO  WRK-NAI-HENFLG
               ELSE
                   MOVE    "28"            TO  WRK-ERRCD2
               END-IF
           END-IF
      *
           IF     (WRK-ERRCD2      NOT =   SPACE )
             AND  (WRK-ERR2-SRYKA      =   SPACE )
               MOVE    SYU-SRYKA           TO  WRK-ERR2-SRYKA
               MOVE    SYU-HKNCOMBINUM     TO  WRK-ERR2-HKNCOMBI
           END-IF
           .
       200311-INPUT-HENKIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     調整金力内容チェック処理
      *****************************************************************
       200313-K03CALL4-SYORI-SEC       SECTION.
      *
           MOVE    SYU-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBI
           MOVE    SYU-SRYKA           TO  SPA-GMN-SRYKA
      *
      *    入金状態
           MOVE    WRK-NYUKIN-JYOTAI   TO  SPA-NAI-NYUKIN-JYOTAI
           MOVE    WRK-NYUKIN-HOHO     TO  SPA-GMN-NYUKINHOU
      *
           MOVE    WRK-CHOSEI1 (TBL-IDC)   TO  SPA-GMN-CHOSEI1
           MOVE    WRK-CHOSEI2 (TBL-IDC)   TO  SPA-GMN-CHOSEI2
           MOVE    WRK-CHOSEI1 (TBL-IDC)   TO  WRK-Z92
           MOVE    WRK-Z92                 TO  SPA-GMN-CHOSEI1-X
           MOVE    WRK-CHOSEI2 (TBL-IDC)   TO  WRK-Z92
           MOVE    WRK-Z92                 TO  SPA-GMN-CHOSEI2-X
      *    個別入金（訂正のみ）
           IF      WRK-IN-NYUKIN-OK(TBL-IDC)   =   1
               MOVE    WRK-IN-NYUKIN(TBL-IDC)  TO  SPA-GMN-NYUKIN
               MOVE    "HAORI"             TO  SPA-GMN-NYUKIN-X
           END-IF
      *
           MOVE    ZERO                TO  SPA-MAE-CHOSEI
      *
           MOVE    "APIV3"             TO  SPA-MOTOPG
      *    チェック処理
           MOVE    4                   TO  SPA-GMN-PAGE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K03             TO  SPA-FREE
      *
           MOVE    "HAOR"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           MOVE    SPACE               TO  MCP-WIDGET
      *
           INITIALIZE                  WRK-SCRAREA
           CALL    "ORCGK03"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K03
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200329-ERRCD-MSG-SEC
               MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
           END-IF
           .
       200313-K03CALL4-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     入力内容チェック処理
      *****************************************************************
       200311-INPUT-CHK-SEC          SECTION.
      *
      *    入金額
           IF      WRK-NYUKIN-OK       =   1
               MOVE    WRK-NYUKIN          TO  SPA-GMN-NYUKIN
               MOVE    "HAORI"             TO  SPA-GMN-NYUKIN-X
           END-IF
           IF      WRK-HENKIN-OK       =   1
               MOVE    WRK-HENKIN          TO  SPA-GMN-HENKIN
               MOVE    "HAORI"             TO  SPA-GMN-HENKIN-X
           END-IF
      *
      *    一括処理は入金額・返金額ゼロへ
           IF     (MDCREQ-IC-ALLKBN    =   "1"  )
               MOVE    SPACE               TO  SPA-GMN-NYUKIN-X
               MOVE    ZERO                TO  SPA-GMN-NYUKIN
               MOVE    SPACE               TO  SPA-GMN-HENKIN-X
               MOVE    ZERO                TO  SPA-GMN-HENKIN
               MOVE    "*"                 TO  SPA-GMN-NYUKIN-X(1:1)
               MOVE    "*"                 TO  SPA-GMN-HENKIN-X(1:1)
           END-IF
      *
      *    請求額・入金額反映
           PERFORM 2003112-K03CALL-SYORI-SEC
           .
       200311-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     登録　処理
      *****************************************************************
       20032-TOUROKU-SYORI-SEC          SECTION.
      *
      *    ワーク診療行為更新
           PERFORM 20033-WKSRYACT-UPD-SEC
      *
      *    入金状態
           MOVE    WRK-NYUKIN-JYOTAI   TO  SPA-NAI-NYUKIN-JYOTAI
           MOVE    WRK-NYUKIN-HOHO     TO  SPA-GMN-NYUKINHOU
      *
           IF      MDCRES-IC-REQUEST   NOT =   SPACE
               MOVE    MDCRES-IC-REQUEST   TO  SPA-GMN-NYUKINKBN
           END-IF
      *    請求書作成区分
           MOVE    MDCRES-PRINT-SEIKYU TO  SPA-GMN-HAKFLG
      *    伝票発行日
           MOVE    WRK-BASE-YMD        TO  SPA-NAI-HAKYMD
      *
           MOVE    "APIV3"             TO  SPA-MOTOPG
      *    更新処理
           MOVE    3                   TO  SPA-GMN-PAGE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K03             TO  SPA-FREE
      *
           MOVE    "HAOR"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           MOVE    SPACE               TO  MCP-WIDGET
      *
           INITIALIZE                  WRK-SCRAREA
           CALL    "ORCGK03"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       WRK-SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K03
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    伝票番号を編集する
           IF     (SPA-ERRCD           =   SPACE )
             OR   (SPA-ERRCD(1:1)      =   "K"   )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-SYUNOU-MAX )
                   MOVE    SPA-SYUNOU-REC (IDX)
                                       TO  LNK-SYUNOU-REC(IDX)
               END-PERFORM
           END-IF
      *
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)
                   MOVE    "W"                 TO  WRK-ERRKBN
                   MOVE    "39"                TO  WRK-ERRCD
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG-02
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
      *
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
                   OR (WRK-ERRCD       NOT =   SPACE )
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
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-RATE-OUTPATIENT
                                                             (IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-MONEY-OUTPATIENT
                                                             (IDY2)
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
      *H29.6
      *****************************************************************
      *     帳票印刷指示処理
      *****************************************************************
       20034-PRINT-SYORI-SEC       SECTION.
      *
           IF     (MDCRES-PRINT-SEIKYU     =   "0"
                                           OR  SPACE )
             AND  (MDCRES-PRINT-MEISAI     =   "0"
                                           OR  SPACE )
             AND  (MDCRES-PRINT-SHOHOU     =   "0"
                                           OR  SPACE )
             AND  (MDCRES-PRINT-YAKUJYO    =   "0"
                                           OR  SPACE )
             AND  (MDCRES-PRINT-TECHOU     =   "0"
                                           OR  SPACE )
             AND  (MDCRES-PRINT-YOYAKU     =   "0"
                                           OR  SPACE )
               GO      TO      20034-PRINT-SYORI-EXT
           END-IF
      *
      *    一時ディレクトリ作成
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
      *    SPA-API-FLG,SPA-PRTCONFを設定
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
      *    2019/09/19 Added
           MOVE    "00"            TO  SPA-PRT-FLG
           INITIALIZE                  SRPLNK-AREA
           MOVE    "INS"           TO  SRPLNK-MODE
           MOVE    RPKEY-COMMON    TO  SRPLNK-VAL
           MOVE    MDCREQ-KARTE-UID
                                   TO  SRPLNK-KARTE-UID
           MOVE    MDCREQ-ORCA-UID
                                   TO  SRPLNK-ORCA-UID
           CALL    "ORCSRPLNK"         USING
                                       SRPLNK-AREA
                                       SPA-AREA
      *
      *    主たる伝票番号
           IF      SPA-SYUNOU-MAX      >=  1
               MOVE    SPA-SYUNOU-REC(1)   TO  SYUNOU-REC
               MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
      *
               MOVE    ORCSCGAIRAI-JYURRK-RENNUM (1)
                                           TO  WRK-JYURRK-RENNUM
           ELSE
               MOVE    ZERO                TO  WRK-DENPNUM
               MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           END-IF
      *
      *    処方箋印刷（Ａ５）
           IF      MDCRES-PRINT-SHOHOU =   "1"  OR  "2"
               INITIALIZE                      ORCHC19AREA
               MOVE    WRK-JYURRK-RENNUM   TO  ORCHC19-RENNUM
               MOVE    SPACE               TO  ORCHC19-KBN
               IF      MDCRES-PRINT-SHOHOU =   "2"
                   MOVE    "3"                 TO  ORCHC19-KBN
               END-IF
               MOVE    WRK-DENPNUM         TO  ORCHC19-DENPNUM
               MOVE    "01"                TO  SPA-PRT-FLG
      *
      *        2019/09/19 Added
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-SHOHOSEN      TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
               MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
               CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
               CALL    ORCSPRTNM-PRTPG     USING   SPA-AREA
                                                   ORCHC19AREA
           END-IF
      *
      *    請求書印刷
           IF      MDCRES-PRINT-SEIKYU =   "1"  OR  "2" 
               PERFORM 200341-SEIKYU-PRINT-SEC
           END-IF
      *
      *    薬情報出力
           IF      MDCRES-PRINT-YAKUJYO    =   "1"  OR  "2"
               INITIALIZE                      ORCSCH30AREA
               MOVE    WRK-DENPNUM         TO  ORCSCH30-DENPNUM
               MOVE    MDCRES-PRINT-YAKUJYO    TO  ORCSCH30-KBN
               MOVE    "03"                TO  SPA-PRT-FLG
      *
      *        2019/09/19 Added
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-OKUSURI-JOHO  TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
               CALL    "ORCSCH30"          USING   SPA-AREA
                                                   ORCSCH30AREA
           END-IF
      *
      *    診療費明細書印刷
           IF      MDCRES-PRINT-MEISAI     =   "1"
               INITIALIZE                      ORCHC04AREA
               MOVE    WRK-DENPNUM         TO  ORCHC04-DENPNUM
      *        発行フラグ
               IF      SPA-SYORIFLG        =   1
      *            訂正分
                   MOVE    2                   TO  ORCHC04-HAKKOFLG
               ELSE
                   MOVE    ZERO                TO  ORCHC04-HAKKOFLG
               END-IF
      *        発行方法（診療科・保険組合せ別のみ）
      *****    MOVE    1                   TO  ORCHC04-HAKHOUFLG
      *        発行方法
               MOVE    MDCRES-HAKKOU-KBN   TO  ORCHC04-HAKHOUFLG
               IF      SPA-SYUNOU-MAX      =   1
      *            単独
                   MOVE    ZERO                TO  ORCHC04-HAKHOUFLG
               END-IF
               MOVE    "04"                TO  SPA-PRT-FLG
      *
      *        2019/09/19 Added
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-MEISAISHO     TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
               MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
               CALL    "ORCSPRTNM"             USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYSKANRI-REC
               CALL    ORCSPRTNM-PRTPG         USING
                                               SPA-AREA
                                               ORCHC04AREA
           END-IF
      *    お薬手帳出力
           IF      MDCRES-PRINT-TECHOU     =   "1"  OR  "2"
               INITIALIZE                      ORCSCH62AREA
               MOVE    WRK-DENPNUM         TO  ORCSCH62-DENPNUM
               MOVE    MDCRES-PRINT-TECHOU TO  ORCSCH62-KBN
               MOVE    "05"                TO  SPA-PRT-FLG
      *
      *        2019/09/19 Added
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-OKUSURI-TECHO TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
               MOVE    "1"                 TO  ORCSCH62-PGKBN
               CALL    "ORCSCH62"          USING   SPA-AREA
                                                   ORCSCH62AREA
           END-IF
      *
      *    予約票発行
           IF      MDCRES-PRINT-YOYAKU     =   "1"
               INITIALIZE                  ORCHC67AREA
               MOVE    SPA-SRYYMD          TO  ORCHC67-SRYYMD
               MOVE    "2"                 TO  ORCHC67-KBN
               MOVE    "06"                TO  SPA-PRT-FLG
      *
      *        2019/09/19 Added
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-YOYAKUHYO     TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
      *ver.4.7 プレビューの為、直接印刷へ変更
               MOVE    ORCHC67AREA         TO  ONPRTDATA-DATA
      *
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "00000010"          TO  ORCSPRTNM-KBNCD
               MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
               CALL    "ORCSPRTNM"             USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYSKANRI-REC
               CALL    ORCSPRTNM-PRTPG         USING
                                               SPA-AREA
                                               ONPRTDATA
           END-IF
      *
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
      *     請求書兼領収書印刷処理
      *****************************************************************
       200341-SEIKYU-PRINT-SEC            SECTION.
      *
      *    請求書印刷
           INITIALIZE                      ORCHC03AREA
           MOVE    WRK-DENPNUM         TO  ORCHC03-DENPNUM
           MOVE    SPA-GMN-SEIKYU      TO  ORCHC03-SEIKYU
           MOVE    SPA-GOK-NYUKIN      TO  ORCHC03-NYUKIN
      *    消費税（再掲）
           MOVE    SPA-NAI-SKYMONEY-TAX-SAI
                                       TO  ORCHC03-SEIKYU-TAX-SAI
      *    前回未収額−前回返金額
           IF      SPA-GMN-ZENMISYU    >   SPA-GMN-CHOKAKIN
               COMPUTE ORCHC03-ZENMISYU    =   SPA-GMN-ZENMISYU
                                           -   SPA-GMN-CHOKAKIN
               MOVE    ZERO                TO  ORCHC03-HENKIN
           ELSE
               COMPUTE ORCHC03-HENKIN      =   SPA-GMN-ZENMISYU
                                           -   SPA-GMN-CHOKAKIN
               MOVE    ZERO                TO  ORCHC03-ZENMISYU
           END-IF
      *    前回未収額２
           MOVE    SPA-GMN-ZENMISYU    TO  ORCHC03-ZENMISYU-2
      *    前回過入金
           MOVE    SPA-GMN-CHOKAKIN    TO  ORCHC03-KANYUKIN
           IF      SPA-SYORIFLG        =   1
               MOVE    ZERO                TO  ORCHC03-ZENMISYU-2
               MOVE    ZERO                TO  ORCHC03-KANYUKIN
      *        入金額は返金との差額とする
               COMPUTE ORCHC03-NYUKIN  =   ORCHC03-NYUKIN
                                       -   SPA-NAI-HENKIN
           END-IF
      *
      *    発行フラグ
           IF      SPA-SYORIFLG        =   1
      *        訂正分
               MOVE    2                   TO  ORCHC03-HAKKOFLG
           ELSE
               MOVE    ZERO                TO  ORCHC03-HAKKOFLG
           END-IF
      *    訂正で合計金額編集
           IF      SPA-SYORIFLG        =   1
               EVALUATE    SPA-GMN-HAKFLG
                   WHEN    "0"
                   WHEN    "1"
                       MOVE    ZERO            TO  ORCHC03-TEIKIN-KBN
                   WHEN    "2"
                       MOVE    1               TO  ORCHC03-TEIKIN-KBN
               END-EVALUATE
           END-IF
      *
           MOVE    ZERO                TO  WRK-HENKIN
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-SYU
           PERFORM VARYING     IDX-S   FROM    1   BY  1
                   UNTIL       IDX-S   >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX-S)   TO  SYUNOU-REC
               IF      SYU-HKNCOMBINUM     NOT =   "9999"
      *
               ADD     1               TO  IDX
               ADD     1               TO  IDX-SYU
               MOVE    SYU-DENPNUM     TO  ORCHC03-OLD-DENPNUM (IDX)
               MOVE    SYU-SRYKA       TO  ORCHC03-OLD-SRYKA   (IDX)
               MOVE    SYU-HKNCOMBINUM TO  ORCHC03-OLD-HKNCOMBI (IDX)
               MOVE    SPA-MAE-SKYMONEY(IDX)
                                       TO  ORCHC03-OLD-SKYMONEY(IDX)
               MOVE    SPA-MAE-SKYMONEY-TAX-SAI(IDX)
                                       TO  ORCHC03-OLD-SKYMONEY-SAI
                                                               (IDX)
               MOVE    SPA-TEI-NYUKIN-TOTAL (IDX)
                                       TO  ORCHC03-OLD-NYUKIN-TOTAL
                                                               (IDX)
      *        返金対応
               IF      SPA-SYORIFLG        =   1
                   IF      SPA-SYU-HENKIN(IDX) NOT =   ZERO
      *                マイナス入金
                       COMPUTE ORCHC03-OLD-NYUKIN-TOTAL (IDX)
                                       =   SPA-TEI-NYUKIN-TOTAL (IDX)
                                       -   SPA-SYU-HENKIN (IDX)
                   END-IF
               ELSE
      *            充当
                   MOVE    SPA-SYU-HENKIN (IDX)
                                       TO  ORCHC03-KON-HENKIN (IDX)
                   IF      SYU-SRYKA       NOT =   ZERO
                       ADD     SPA-SYU-HENKIN (IDX)    TO  WRK-HENKIN
                   END-IF
               END-IF
      *
               END-IF
           END-PERFORM
      *
      *    発行方法
           MOVE    MDCRES-HAKKOU-KBN   TO  ORCHC03-HAKHOUFLG
      *    収納単独
           IF      IDX-SYU             =   1
               MOVE    ZERO                TO  ORCHC03-HAKHOUFLG
           END-IF
      *
      *    今回返金充当額
           IF      SPA-SYORIFLG        =   1
               MOVE    ZERO                TO  ORCHC03-ZEN-HENKIN
           ELSE
               COMPUTE ORCHC03-ZEN-HENKIN  =   SPA-GOK-HENKIN
                                           -   WRK-HENKIN
           END-IF
      *    充当処理あり
           MOVE    SPA-MATOME-NYUHENKIN-KBN
                                       TO  ORCHC03-MATOME-NYUHENKIN-KBN
           MOVE    "02"                TO  SPA-PRT-FLG
      *
      *    2019/09/19 Added
           INITIALIZE                      SRPLNK-AREA
           MOVE    "INS"               TO  SRPLNK-MODE
           MOVE    RPKEY-SEIKYUSHO     TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"             USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"             USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
      *
           CALL    ORCSPRTNM-PRTPG     USING
                                       SPA-AREA
                                       ORCHC03AREA
           .
       200341-SEIKYU-PRINT-EXT.
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
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
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
      *    INITIALIZE                      SPATMP-REC
      *    MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *    MOVE    "AP21"              TO  SPATMP-GYOUMUID
      *    MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *    MOVE    "SPA-K03"           TO  SPATMP-FILEMEI
      *    MOVE    1                   TO  SPATMP-EDANUM
      *    PERFORM 9100-SPATMP-KEYREAD-SEC
      *    IF      FLG-SPATMP          =   ZERO
      *        MOVE    SPATMP-DATA-REC     TO  SPA-K03
      *    ELSE
      *        MOVE    "08"                TO  WRK-ERRCD
      *    END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
      *        診療行為登録の処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "01"
                   MOVE    "08"                TO  WRK-ERRCD
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
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
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
           MOVE    SPACE               TO  WRK-APIV3DATA-AREA
           MOVE    MDCRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
      *    処理ＰＧ
           MOVE    "01"                TO  WRK-PARA-SYORIPG
           MOVE    WRK-DENPNUM         TO  WRK-PARA-DENPNUM
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
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K03"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K03             TO  SPATMP-DATA-REC
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
           MOVE    "API21V3"           TO  APIV3SUB01-API-FILEMEI
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
      *    入金額・返金額等チェック
               WHEN    "0004"
                   STRING  "今回請求額がゼロ以下になります。"
                           "調整金を再入力して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
                   MOVE    "30"            TO  WRK-ERRCD
               WHEN    "0008"
                   MOVE    "入金額は今回請求額以下にして下さい。"
                                       TO  SPA-ERRMSG
                   MOVE    "31"            TO  WRK-ERRCD
      *        返金追加
               WHEN    "0106"
                   MOVE    "返金額は整数で入力して下さい。"
                                       TO  SPA-ERRMSG
                   MOVE    "32"            TO  WRK-ERRCD
               WHEN    "0107"
                   MOVE    "返金額は前回までの超過額にして下さい。"
                                       TO  SPA-ERRMSG
                   MOVE    "33"            TO  WRK-ERRCD
               WHEN    "0108"
                   MOVE    "返金額が今回請求額ではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "34"            TO  WRK-ERRCD
               WHEN    "0109"
                   MOVE    "返金額は入力できません。"
                                       TO  SPA-ERRMSG
                   MOVE    "27"            TO  WRK-ERRCD
               WHEN    "0110"
                   STRING  "入金額は今回請求額＋返金額以下に"
                           "して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
                   MOVE    "35"            TO  WRK-ERRCD
               WHEN    "0113"
                   STRING  "今回請求分のみの入金です。"
                           "過入金の返金はできません。"
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
           WHEN    "12"
               MOVE     "基準日付が暦日ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE     SPACE                  TO  WRK-ERRMSG
           WHEN    "15"
               STRING  "入金方法区分が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "14"
               STRING  "返金額の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "16"
               STRING  "入金額の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "17"
               STRING  "調整金１の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "18"
               STRING  "調整金２の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "19"
               STRING  "保険組合せ番号が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "20"
               MOVE     "剤削除番号が存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               STRING  "伝票番号が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *    WHEN    "21"
      *        STRING  "今回請求額＜入金額となります。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      *        END-STRING
           WHEN    "22"
               STRING  "今回請求額がマイナスとなります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "23"
               STRING  "包括分入力は収納を作成しません。"
                       "入金額の設定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "24"
               STRING  "包括分入力は収納を作成しません。"
                       "調整金の設定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "25"
               STRING  "包括分入力は収納を作成しません。"
                       "返金額の設定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "26"
               STRING  "訂正処理の順番が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "27"
               STRING  "返金額は入力できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "28"
               STRING  "収納が未収です。返金額は入力できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *H30.6
          WHEN    "29"
              STRING  "今回請求額が７桁を超えます。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
      *2019.1
          WHEN    "70"
              STRING  "個別設定できる保険組合せ番号ではありません。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
          WHEN    "71"
              STRING  "個別設定できる診療科ではありません。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
          WHEN    "72"
              STRING  "個別設定できる保険組合せ番号と"
                      "診療科ではありません。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
          WHEN    "73"
              STRING  "個別入金額は訂正時のみの設定です。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
          WHEN    "74"
              STRING  "入金額と個別入金額はどちらかの設定です。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
          WHEN    "75"
              STRING  "入金額は収納が１件の時に設定して下さい。"
                      "個別入金額に設定して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
           WHEN    "76"
               STRING  "警告！未収の収納に返金はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "77"
               STRING  "負担割合計算額が７桁を超えます。"
                                       DELIMITED   BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "78"
               STRING  "作成する収納が上限件数を超えます。"
                                       DELIMITED   BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
          WHEN    "79"
              STRING  "調整金と個別調整金はどちらかの設定です。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
          WHEN    "69"
              STRING  "収納が複数あります。"
                      "個別調整金に設定して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
              END-STRING
      **   WHEN    "26"
      *        STRING  "訂正処理ではありません。"
      *                "返金額の設定はできません。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      **       END-STRING
      *    WHEN    "27"
      *        STRING  "伝票番号が違います。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      *        END-STRING
      *    WHEN    "26"
      *        STRING  "訂正処理の順番が違います。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      ***      END-STRING
      *    WHEN    "29"
      *        STRING  "今回返金額以上です。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      *        END-STRING
      *
           WHEN    "30"
      *        ORCSCGAIRAI 警告
               CONTINUE
           WHEN    "40"  THRU  "49"
      *        ORCSCGAIRAI エラー
               CONTINUE
           WHEN    "50"
               STRING  "診療行為が４００行をオーバーします。"
                       "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *H29.6
           WHEN    "51"
               STRING  "請求書兼領収書印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "52"
               STRING  "診療費明細書印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "53"
               STRING  "処方せん印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "54"
               STRING  "薬剤情報印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "55"
               STRING  "お薬手帳印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "56"
               STRING  "予約票の印刷指示区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "57"
               STRING  "入金取り扱い区分エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "58"
               STRING  "訂正の入金取り扱い区分は「今回請求分のみ入金」"
                       "のみ可能です。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "59"
               STRING  "一括入返金区分エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "60"
               STRING  "一括入返金区分は設定できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "61"
               STRING  "更新処理区分エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *!!
           WHEN    "62"
               STRING  "請求書発行方法区分エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
      *    2019/09/19 Added
           WHEN    "63"
               STRING  "印刷モードを有効にする場合、"
                                                       DELIMITED BY SIZE
                       "ミドルウェアを更新してください。"
                                                       DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "64"
               STRING  "無効な印刷モードです。"
                                                       DELIMITED BY SIZE
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
     ********  MOVE    "処理区分未設定"        TO  WRK-ERRMSG
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
      *    エラー診療科・保険組合せ編集
      *****************************************************************
       891-ERRSRYKA-HEN-SEC              SECTION.
      *
           MOVE    WRK-ERRMSG          TO  WRK-HZN-ERRMSG
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  WRK-HZN-ERRMSG
                   "　診療科："
                   WRK-ERR-SRYKA
                   "　保険組合せ番号："
                   WRK-ERR-HKNCOMBI
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           .
       891-ERRSRYKA-HEN-EXT.
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
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "tbl_syunou"       TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
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
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
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
