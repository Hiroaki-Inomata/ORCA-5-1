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
       PROGRAM-ID.             ORCSCWKSRY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : ワーク診療行為展開、登録処理サブ
      *                     （Ｋ０２，Ｋ０２Ｎ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/16    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/22  MONFUNC 対応
      *  01.00.02    NACL-多々納  06/07/06  収納変更対応
      *  03.04.00    NACL-多々納  07/02/XX  包括診療対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/06/01  セット対応
      *  03.05.01    NACL-多々納  07/07/19  "0085XXXXX"コメント対応
      *  04.01.00    NACL-多々納  07/11/21  器材商品名表示対応
      *  04.05.00    NACL-多々納  09/09/16  "0086XXXXX"コメント対応
      *  04.05.00    NACL-多々納  09/10/02  換算値数値他対応
      *  04.05.00    NACL-多々納  10/01/XX  治験対応
      *  04.06.00    NACL-多々納  10/11/29  自賠責金額入力対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.07.00    NACL-多々納  14/02/13  労災金額％加算対応
      *  04.07.00    NACL-多々納  14/04/03  Ｈ２６年４月改正対応
      *  04.08.00    NACL-多々納  14/05/27  一時ファイル変更
      *  04.07.00    NACL-多々納  14/09/XX  Ｈ２６年１０月改正対応
      *  04.07.00    NACL-多々納  15/03/XX  Ｈ２７年４月改正対応
      *  04.08.00    NACL-多々納  15/07/28  労災金額・点数編集変更
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  04.08.00    NACL-多々納  16/08/XX  持参薬対応
      *  04.08.00    NACL-多々納  17/02/XX  在宅実績加算２削除対応
      *  04.08.00    NACL-多々納  17/03/XX  Ｈ２９年４月改正対応他
      *  04.08.00    NACL-多々納  17/05/XX  HAORI対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応他
      *  04.08.00    NACL-多々納  18/06/XX  自費金額桁数チェック対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
      *    パラメタデータ
      *    SELECT  PARAK02-FILE    ASSIGN  FILE-NAME22
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA2.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARAF-REC.
      *    03  PARAF-KEY.
      *        05  PARAF-FILEMEI         PIC X(08).
      *        05  PARAF-EDANUM          PIC 9(03).
      *    03  PARAF-DATA-REC            PIC X(60000).
      *
      *    画面スパ領域パラメタデータ
      *FD  PARAK02-FILE.
      *    画面スパ領域
      *    COPY    "K02SCR-SPA"      REPLACING
      *                              //SPA-// BY //PARA-SPA-//.
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      **   パラメタファイル名
      *01  FILE-NAME22.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(08)   VALUE   "K01PARA2".
      *    03  FILE-HOSPNUM22      PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID22       PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *    03  STS-PARA2           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-SYORIFLG        PIC 9(01).
           03  FLG-ZENKAI          PIC 9(01).
           03  FLG-HEN-CHK         PIC 9(01).
           03  FLG-AUTO-OK         PIC 9(01).
           03  FLG-INGAIKBN        PIC 9(01).
           03  FLG-INGAIKBN-ARI    PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
           03  FLG-SYUBETU         PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-FUKU            PIC 9(01).
           03  FLG-FIRUMU          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-AUTO-ARI        PIC 9(01).
           03  FLG-AUTO-NASI       PIC 9(01).
      *
           03  FLG-WKSYORIFLG      PIC 9(01).
      *
           03  FLG-TIME            PIC 9(01).
           03  FLG-HKN999          PIC 9(01).
           03  FLG-HEN-CHK2        PIC 9(01).
      *
           03  FLG-RSITEN          PIC 9(01).
      *H27.4
           03  FLG-TEIGEN30-ZAI    PIC 9(01).
      *API(HAORI)
           03  FLG-AUTONO          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDXS                    PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDW2                    PIC 9(04).
           03  IDW-END                 PIC 9(04).
           03  IDW-STR                 PIC 9(04).
      *
           03  IDX-TEI                  PIC 9(04).
           03  IDX-1                    PIC 9(04).
           03  IDX-K                    PIC 9(04).
           03  IDX-K2                   PIC 9(04).
      *
           03  IDX-F                    PIC 9(04).
           03  IDH                      PIC 9(04).
           03  IDH1                     PIC 9(04).
           03  IDH2                     PIC 9(04).
           03  IDH3                     PIC 9(04).
      *
           03  IDX-HZN                 PIC 9(04).
           03  IDX-STR                 PIC 9(04).
      *
           03  IDT                     PIC 9(04).
      *H27.7
           03  IDW2-H                  PIC 9(04).
      *
           03  IDHC                    PIC 9(04).
      *
           03  IDG                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRCD1              PIC X(04).
           03  WRK-ERRCD2              PIC X(04).
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SRYDD               PIC 9(02).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-DRCD                PIC X(05).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
               05  WRK-SYUTEN2-R           PIC  9(07).
               05  WRK-TENSUKEI            PIC 9(08).
               05  WRK-KAISU               PIC 9(04).
               05  WRK-KINGAK              PIC 9(08).
               05  WRK-KAISUKEI            PIC 9(08).
      *
               05  WRK-MEINUM              PIC 9(04).
               05  WRK-RENNUM              PIC 9(04).
      *
               05  WRK-SRYKBN              PIC X(02).
               05  WRK-SRYSYUKBN           PIC X(03).
      *
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-PARAEDABAN          PIC 9(04).
      *
      *    小児科外来診察料加算判定
           03  WRK-SYONIKA-KBN         PIC X(01).
      *    自費用
           03  WRK-TENTTLKBN       PIC 9(03).
           03  WRK-HOZ-KEI         PIC 9(08).
      *    複数保険
           03  WRK-FTNRATE         PIC 9(03).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-KOH1HKNNUM      PIC X(03).
           03  WRK-KOH2HKNNUM      PIC X(03).
           03  WRK-KOH3HKNNUM      PIC X(03).
           03  WRK-KOH4HKNNUM      PIC X(03).
      *    労災保険用
           03  WRK-TENSU           PIC S9(08).
           03  WRK-EN              PIC 9(08).
           03  WRK-TANKA           PIC 9(08).
      *
           03  WRK-SYU-DENPNUM     PIC 9(07).
           03  WRK-TNSSRYSYUKBN        PIC X(03).
      *
           03  WRK-HOUKATU-ZAI         PIC 9(01).
      *
           03  WRK-KANSURYO            PIC 9(05)V9(5).
      *
           03  WRK-YAKUHYO             PIC X(01).
           03  WRK-GENTEN              PIC X(01).
      *
      *H28.9   持参薬
           03  WRK-JISANYAKU           PIC X(01).
      *
      *    労災施術加算等用
           03  WRK-ENTANKA             PIC S9(08).
           03  IDR-1                   PIC 9(04).
           03  IDR-2                   PIC 9(04).
           03  WRK-TENKEISAN           PIC 9(08)V99.
           03  WRK-TENSUKEI-9          PIC S9(08).
           03  WRK-TENSUKEI-G          PIC S9(08).
           03  WRK-TENSUKEI-K          PIC S9(08).
      *
           03  WRK-TIME-KBN2           PIC X(01).
      *H29.2
      *    在医総管判定
           03  WRK-ZS-GRP                  PIC X(02).
           03  WRK-ZS-KBN                  PIC X(01).
           03  WRK-ZS-KBN2                 PIC X(01).
      *
           03  FLG-CHKAUTO                 PIC 9(01).
           03  FLG-CHKEND                  PIC 9(01).
           03  IDXZ                        PIC 9(04).
           03  WRK-ZKS-GRP             PIC X(02).
           03  WRK-ZKS-KBN             PIC X(01).
      *H29.8
           03  WRK-GRPCNT              PIC 9(03).
           03  FLG-CHK01               PIC 9(01).
      *
      *H30.6
      *    保険外の合計金額７桁オーバーチェック
           03  WRK-SUM-HKNTEN             PIC 9(08).
           03  WRK-SUM-TGMONEY            PIC 9(08).
           03  WRK-SUM-TGMONEY-TAX        PIC 9(08).
           03  WRK-SUM-JIHI-TOTAL         PIC 9(08).
           03  WRK-SUM-JIHI-TOTAL-TAX     PIC 9(08).
      *
      *    収納集計点数内容
       01  TBL-K02SCR-AREA.
           03  TBL-K02SCR-OCC          OCCURS   400.
               05  TBL-SYU-TENSU       PIC 9(07).
      *
      *    集計収納請求内容
       01  WRK-SUM-AREA.
           03  SUM-SYU-SKY-NAIYOU.
               05  SUM-SYU-SKY-TBL             OCCURS   17.
      *    保険点数
                   09  SUM-SYU-HKNTEN          PIC  S9(08).
      *
      *画面最大行
      *01  WRK-GYO-MAX                 PIC 9(04)   VALUE   400.
      *
      *    小児科外来診察料テーブル
           COPY   "CMSYONIKATBL.INC".
      *
      *H24.5
       01  CONS-AREA.
      *    時間外対応加算１
           03  TBL-CHISRYCD1       PIC X(09)   VALUE   "112016070".
      *    時間外対応加算２
           03  TBL-CHISRYCD2       PIC X(09)   VALUE   "112015670".
      *    時間外対応加算３
           03  TBL-CHISRYCD3       PIC X(09)   VALUE   "112016170".
      *
      *
      *    時間外加算テーブル
       01  TBL2-TIMEKSN-AREA.
      *小児科外来診療科（初診）
      *    時間外
           03  FILLER              PIC X(12)   VALUE   "11 113009670".
      *    休日
           03  FILLER              PIC X(12)   VALUE   "12 113007170".
      *    深夜
           03  FILLER              PIC X(12)   VALUE   "13 113007270".
      *    時間外特例
           03  FILLER              PIC X(12)   VALUE   "14 111010770".
      *    夜間（時間外）加算
           03  FILLER              PIC X(12)   VALUE   "15 113007070".
      *小児科外来診療科（再診）
      *    時間外
           03  FILLER              PIC X(12)   VALUE   "21 113009770".
      *    休日
           03  FILLER              PIC X(12)   VALUE   "22 113007470".
      *    深夜
           03  FILLER              PIC X(12)   VALUE   "23 113007570".
      *    時間外特例
           03  FILLER              PIC X(12)   VALUE   "24 112006070".
      *    夜間（時間外）加算
           03  FILLER              PIC X(12)   VALUE   "25 113007370".
      *外来診察料外来診療科（再診）
      *    時間外
           03  FILLER              PIC X(12)   VALUE   "21 113009870".
      *    休日
           03  FILLER              PIC X(12)   VALUE   "22 113007770".
      *    深夜
           03  FILLER              PIC X(12)   VALUE   "23 113007870".
      *    時間外特例
           03  FILLER              PIC X(12)   VALUE   "24 113005570".
      *    夜間（時間外）加算
           03  FILLER              PIC X(12)   VALUE   "25 113007670".
      *
      *地域包括診療料
      *    時間外
           03  FILLER              PIC X(12)   VALUE   "31 113016270".
      *    休日
           03  FILLER              PIC X(12)   VALUE   "32 113016370".
      *    深夜
           03  FILLER              PIC X(12)   VALUE   "33 113016470".
      *    時間外特例
           03  FILLER              PIC X(12)   VALUE   "34 113016870".
      *    乳幼児時間外加算
           03  FILLER              PIC X(12)   VALUE   "31 113016570".
      *    乳幼児休日加算
           03  FILLER              PIC X(12)   VALUE   "32 113016670".
      *    乳幼児深夜加算
           03  FILLER              PIC X(12)   VALUE   "33 113016770".
      *    乳幼児時間外特例加算
           03  FILLER              PIC X(12)   VALUE   "34 113016970".
      *    小児科 乳幼児夜間
           03  FILLER              PIC X(12)   VALUE   "35 113017070".
      *    小児科 乳幼児休日
           03  FILLER              PIC X(12)   VALUE   "32 113017170".
      *    小児科 乳幼児深夜
           03  FILLER              PIC X(12)   VALUE   "33 113017270".
      *    早朝加算
           03  FILLER              PIC X(12)   VALUE   "38 113017370".
      *
       01  TBL2-TIMEKSN-AREA-R      REDEFINES   TBL2-TIMEKSN-AREA.
           03  TBL2-TIMEKSN-OCC     OCCURS   27
                                   INDEXED  TBL2-TIDX.
      *        1:初診、2：再診、3:地域包括診療料
               05  TBL2-TM-KBN1         PIC X(01).
      *        1:時間外、2：休日、3:深夜、4:特定時間外、5:夜間、8:夜間・早朝
               05  TBL2-TM-KBN2         PIC X(01).
               05  TBL2-TM-YOBI         PIC X(01).
      *
               05  TBL2-TM-SRYCD        PIC X(09).
      *H28.4
      *    時間外加算テーブル(H28.4分）
           COPY     "CMTIMEKSN2016.INC".
      *H30.4
      *    時間外加算テーブル(H30.4分）
           COPY     "CMTIMEKSN2018.INC".
      *
      *H26.10
      * 向精神薬多剤投与減算コード
       01  CONS-TAZAI-SRYCD        PIC X(09)   VALUE    "630010005".
      * （精減）
       01  CONS-SGEN-SRYCD         PIC X(09)   VALUE    "820000166".
      *
      *H27.4
      * 低紹介率病院　減算コード
       01  CONS-TEIHP-SRYCD        PIC X(09)   VALUE    "630010006".
      * （減）（内服逓減と同じ）
       01  CONS-TGEN-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *H28.4
      * 湿布薬枚数減算
       01  CONS-SPGEN-SRYCD        PIC X(09)   VALUE    "630010007".
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *
      *H29.2
      *    在宅時医療総合管理料コードテーブル（H28.4分)
           COPY     "CMZISOTBL2016.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *!!!
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *!!!
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    点数編集チェックパラメタ
           COPY    "CPORCSC92.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    診察料自動発生パラメタ
           COPY    "CPORCSC10S.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    セット展開サブ
           COPY    "CPORCSCSETHEN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
      *    附加情報
       01  WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC".
      *    ワーク診療行為マスタ−（パラメタ）
       01  PARA-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //PARA-WKSRY-//.
      *    附加情報を追加
           03  PARA-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //PARA-WKSRYP-//.
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
      **** COPY    "ORCA-DBPATH".
      *
           COPY    "ORCA-SPA".
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *
           COPY    "CPORCSCWKSRY.INC".
      *    画面ＳＰＡ領域
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSCWKSRYAREA
           SPA-AREA
           SPA-K02
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  STS-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           EVALUATE    ORCSCWKSRY-KBN
      *        展開処理
               WHEN    "1"
                   PERFORM 100-WKSRYACT-TENKAI-SEC
      *        ワーク診療行為マスタ登録（中途終了登録）
               WHEN    "2"
                   PERFORM 200-WKSRYACT-INS-SEC
      *        パラメタ登録（登録時）
               WHEN    "3"
                   PERFORM 300-TOUROKU-PARA-SEC
      *        パラメタ登録（他画面遷移）
               WHEN    "4"
                   PERFORM 400-SENI-PARA-SEC
           END-EVALUATE
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ワーク診療行為展開処理
      *****************************************************************
       100-WKSRYACT-TENKAI-SEC          SECTION.
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *    ワーク診療行為検索
           PERFORM 1001-WKSRYACT-INIT-SEC
      *
           IF      LNK-WKSRYACT-MAX    >   ZERO
               PERFORM 1002-SAIHEN-SYORI-SEC
           END-IF
      *
           .
       100-WKSRYACT-TENKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       1001-WKSRYACT-INIT-SEC          SECTION.
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
               MOVE    MCPDATA-REC         TO  PARA-REC
               MOVE    ZERO                TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "K02 LNK-WKSRYACT-MAX 400 OVR"
                   MOVE    "9000"          TO  SPA-ERRCD
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
       1001-WKSRYACT-INIT-EXT.
           EXIT.
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       1002-SAIHEN-SYORI-SEC          SECTION.
      *
           MOVE    ZERO               TO  FLG-SET
           MOVE    SPACE              TO  WRK-ERRCD1
           MOVE    SPACE              TO  WRK-ERRCD2
           MOVE    ORCSCWKSRY-STRIDX  TO  IDX2
      *
           MOVE    ZERO               TO  FLG-SYORIFLG
           MOVE    ZERO               TO  FLG-WKSYORIFLG
           MOVE    ZERO               TO  FLG-ZENKAI
      *
           MOVE    ZERO               TO  FLG-AUTO-ARI
           MOVE    ZERO               TO  FLG-AUTO-NASI
      *    ワーク診療行為からの時は、訂正と同様に編集する
           IF      LNK-WKSRY-TERMID (1)    =   "WKSRYACT"
               MOVE    1               TO  FLG-SYORIFLG
               MOVE    1               TO  FLG-WKSYORIFLG
           ELSE
      *        訂正展開
               IF      ORCSCWKSRY-SYORIFLG =   ZERO
                   MOVE    1               TO  FLG-SYORIFLG
               END-IF
      *        前回処方展開
               IF     (ORCSCWKSRY-SYORIFLG =   1 ) AND
                      (SPA-NYUGAIKBN       =   "2")
                   MOVE    1               TO  FLG-ZENKAI
               END-IF
           END-IF
      *    ワーク診療行為からの時、剤毎に剤点数を編集し直す
           IF      LNK-WKSRY-TERMID (1)    =   "WKSRYACT"
               PERFORM 10021-WKSRY-ZAITENHEN-SEC
           END-IF
      *    前回処方展開時の、療養担当手当（北海道）判定
           IF      FLG-ZENKAI          =   1
               IF     (IDX2            >   ZERO ) AND
                      (SPA-GMN-INPUTCD(IDX2)   =   "199000610" )
                   MOVE    "1"             TO  SPA-COM-KAIFLG
                                                           (IDX2)
                   MOVE    "1"             TO  SPA-COM-ZAIEND
                                                           (IDX2)
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-INGAIKBN
           MOVE    ZERO                TO  FLG-INGAIKBN-ARI
           MOVE    ZERO                TO  IDX-HZN
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX   >   LNK-WKSRYACT-MAX )  OR
                                (IDX2  >   SPA-MAX-LINE     )
      *
               MOVE    1               TO  IDX1
      *        前回再編集の時、小児科外来診療科算定の時、薬剤のみとする
               IF      (FLG-ZENKAI             =   1     )  AND
                       (SPA-SYONIKA-ARI    NOT =   ZERO  )
                   IF      (LNK-WKSRY-SRYKBN (IDX) =   "21" AND
                                                       "22" AND
                                                       "23")  AND
                           (LNK-WKSRY-ZAITENKEI(IDX)   =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    6               TO  IDX1
                   END-IF
               END-IF
      *        前回再編集の時、保険外の項目は対象外とする
               IF      (FLG-ZENKAI             =   1     )  AND
                       (LNK-WKSRY-SRYKBN (IDX) =   "95" OR  "96" )
                   MOVE    6               TO  IDX1
               END-IF
      *        約束セットの内容は対象外
               IF       FLG-SET            =   1
                   MOVE    6               TO  IDX1
               END-IF
      *
               MOVE    ZERO                TO  IDXS
               MOVE    ZERO                TO  FLG-HEN-CHK2
               PERFORM     VARYING   IDY   FROM    IDX1   BY  1
                           UNTIL     IDY   >   5
                   MOVE    ZERO                TO  FLG-AUTO-OK
                   MOVE    ZERO                TO  FLG-HEN-CHK
      *
      *            診療コードのないものは対象外
                   IF      LNK-WKSRY-SRYCD  (IDX IDY) =   SPACE
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
      *            前回編集時、自動作成分は編集しない
                   IF      FLG-ZENKAI          =   1
                       IF      LNK-WKSRY-AUTOKBN(IDX IDY)    =   "2"
                                                             OR  "3"
                                                             OR  "4"
                                                             OR  "6"
                           MOVE    1               TO  FLG-HEN-CHK
      *                    残量廃棄は対象とする
                           IF      LNK-WKSRY-SRYCD(IDX IDY)
                                                   =   "099309901"
                               MOVE    ZERO            TO  FLG-HEN-CHK
                           END-IF
                       ELSE
                           IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1)
                                                   =   "1"
                               PERFORM 10022-ZENKAICHK-SEC
                           END-IF
                       END-IF
                   END-IF
      *            登録時自動発生分は編集しない
                   IF      LNK-WKSRY-AUTOKBN(IDX IDY)    =   "3"
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
      *            前回処方展開
                   IF     (ORCSCWKSRY-SYORIFLG =   1 ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)  =   "3"
                                                       OR  "4"
                                                       OR  "5"
                                                       OR  "6" )
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
                   IF     (FLG-SYORIFLG                  =   1  ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)    =  "3"
                                                         OR "6" )
      *                訂正自動作成で算定回数を超えている時対象とする
      *                （薬情）
                       PERFORM 10023-AUTOCHK-SEC
                       IF      FLG-AUTO-OK         =   1
                           MOVE    ZERO            TO  FLG-HEN-CHK
                       END-IF
                   END-IF
      *            登録時自動発生分は編集しない
                   IF     (ORCSCWKSRY-SYORIFLG         =   ZERO ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "4"
                                                       OR "6" )
      *                訂正時の自動発生しない
                       IF     (SPA-TEISEI-FLG            =  "1"  )
                         AND  (FLG-AUTO-OK               =   ZERO)
                           MOVE    1               TO  FLG-HEN-CHK
                       ELSE
                           MOVE    ZERO            TO  FLG-HEN-CHK
                           MOVE    1               TO  FLG-AUTO-OK
                       END-IF
                   END-IF
      *            特定疾患処方管理加算編集判定
                   IF     (ORCSCWKSRY-SYORIFLG         =   ZERO ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)    =  "5")
      *                訂正時の自動発生しない
                       IF     (SPA-TEISEITOKU-FLG        =  "1")
                         AND  (FLG-AUTO-OK               =   ZERO)
                           MOVE    1               TO  FLG-HEN-CHK
                       ELSE
                           MOVE    ZERO            TO  FLG-HEN-CHK
                           MOVE    1               TO  FLG-AUTO-OK
                       END-IF
                   END-IF
      *H24.6
      *            処方せん料を訂正時展開する
                   IF     (SPA-SRYYMD              >=  "20120401")  AND
                          (ORCSCWKSRY-SYORIFLG         =   ZERO ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "3"   ) AND
                          (LNK-WKSRY-SRYSYUKBN(IDX)    =  "820" )
                       MOVE    ZERO               TO  FLG-HEN-CHK
                       MOVE    SPACE              TO  LNK-WKSRY-AUTOKBN
                                                            (IDX IDY)
                   END-IF
      *!!!!
      *            自動発生の有無判定
                   IF      ORCSCWKSRY-SYORIFLG     =   ZERO
                       IF     (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "4"
                                                           OR "5"
                                                           OR "6" )
                           MOVE    1               TO  FLG-AUTO-ARI
                       END-IF
                       IF     (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "3" ) AND
                              (LNK-WKSRY-SRYKBN (IDX)      =  "13"
                                                           OR "27"
                                                           OR "60"
                                                           OR "70")
                           MOVE    1               TO  FLG-AUTO-NASI
                       END-IF
                   END-IF
      *!!!!
      *H29.3
      *            処方料を訂正時展開する
                   IF     (SPA-SRYYMD              >=  "20160401")
                     AND  (SPA-TEISHOHOU-FLG       =   "1"      )
                     AND  (ORCSCWKSRY-SYORIFLG     =   ZERO     )
                     AND  (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "3"   )
                     AND  (LNK-WKSRY-SRYSYUKBN(IDX)    =  "250"
                                                       OR "260" )
      *
                       IF      LNK-WKSRY-SRYCD (IDX IDY)
                                                  =   "120000110"
                           CONTINUE
                       ELSE
                           MOVE    ZERO           TO  FLG-HEN-CHK
                           MOVE    SPACE          TO  LNK-WKSRY-AUTOKBN
                                                            (IDX IDY)
                       END-IF
                   END-IF
      *
                   IF      FLG-HEN-CHK         =   ZERO
                       PERFORM 10020-SPA-HEN-SEC
                   END-IF
      *
      *            入力回数（入院のみ）
                   IF      LNK-WKSRY-INPUTCD(IDX IDY)(1:1) =   "*"
                       ADD     1               TO  IDX2
                       IF      IDX2            >   SPA-MAX-LINE
                           MOVE    "9100"          TO  WRK-ERRCD1
                       ELSE
                           MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-COM-SRYSYUKBN
                                                              (IDX2)
                           MOVE    LNK-WKSRY-SRYKBN (IDX)
                                               TO  SPA-COM-SRYKBN(IDX2)
                           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                               TO  SPA-COM-KAISU(IDX2)
                           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                               TO  SPA-COM-KAISU2(IDX2)
                           MOVE    LNK-WKSRY-INPUTCD(IDX IDY)
                                               TO  SPA-GMN-INPUTCD(IDX2)
                           MOVE    "1"             TO  SPA-COM-KAIFLG
                                                               (IDX2)
                       END-IF
                   END-IF
      *            複数科・保険
                   IF      LNK-WKSRY-INPUTCD(IDX IDY)(1:1) =   "#"
                                                           OR  "$"
                       PERFORM 10024-FUKU-HEN-SEC
                   END-IF
      *
      *            約束セットの時、以下を編集しない
                   IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
                       MOVE    5               TO  IDY
                       MOVE    1               TO  FLG-SET
                   END-IF
               END-PERFORM
      *
      *        剤の最後に回数入力をセット
               IF     (IDXS                >   ZERO  )  OR
                      (FLG-SET             =   1     ) 
                   IF     (IDX                 =   LNK-WKSRYACT-MAX ) OR
                          ((IDX                <   SPA-MAX-LINE  ) AND
                           (LNK-WKSRY-ZAINUM (IDX + 1)
                                           NOT =   LNK-WKSRY-ZAINUM
                                                                (IDX)))
                       IF      IDX2            <=  SPA-MAX-LINE
                           MOVE    "1"             TO  SPA-COM-KAIFLG
                                                               (IDX2)
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                               (IDX2)
                       END-IF
                       MOVE    ZERO                TO  FLG-SET
      *************    院内・院外処方を点数より変更
      *                IF      FLG-INGAIKBN-ARI    =   1
      *                院外
      *                    IF      FLG-INGAIKBN        =   1
      *                        MOVE    "1"             TO  SPA-INGAIKBN
      *                    END-IF
      *                    院内（入院は院内）
      *                    IF      FLG-INGAIKBN        =   2
      *                        MOVE    "0"             TO  SPA-INGAIKBN
      *                    END-IF
      *                END-IF
      *
      *                MOVE    ZERO                TO  FLG-INGAIKBN
      ***********      MOVE    ZERO                TO  FLG-INGAIKBN-ARI
                   END-IF
               END-IF
           END-PERFORM
      *H24.10
      *    院内・院外処方を投薬点数より変更
           IF      FLG-INGAIKBN-ARI    =   1
      *        院外
               IF      FLG-INGAIKBN        =   1
                   MOVE    "1"             TO  SPA-INGAIKBN
               END-IF
      *        院内（入院は院内）
               IF      FLG-INGAIKBN        =   2
                   MOVE    "0"             TO  SPA-INGAIKBN
               END-IF
           END-IF
      *!!
      *    訂正時の算定有無登録の判定
           IF      FLG-AUTO-ARI        =   ZERO
               IF      FLG-AUTO-NASI       =   1
                   MOVE    "1"             TO  SPA-TEISEI-FLG
               END-IF
           END-IF
      *
      *    中途終了追加場合
           IF      FLG-FUKU        NOT =   ZERO
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               IF      ORCSCWKSRY-K09TUIKA =   1
      *            コメント追加
                   MOVE    "A"                 TO  LNK-SC10S-SYORI
      *            中途終了展開時の自動発生
                   IF     (SPA-CHUTOTENKAI-FLG     =   "0" ) AND
                          (SPA-WKSRY-KARTE-FLG     =   1   )
                       MOVE    "D"                 TO  LNK-SC10S-SYORI
                   END-IF
                   MOVE    IDX-HZN             TO  LNK-SC10S-STRIDX
               ELSE
      *            コメント変更
                   MOVE    "A"                 TO  LNK-SC10S-SYORI
      *            訂正の展開時
                   IF      SPA-SYORIFLG        =   1
                       MOVE    "B"             TO  LNK-SC10S-SYORI
                   END-IF
      *H31.3
      *HAORI
                   IF     (LNK-SC10S-SYORI     =   "A" )
                      AND (SPA-API-FLG         =   1   )
      *                HAORI 中途データ展開の時、SPA科・保険
                       PERFORM 10029-SRYKA-HEN-SEC
                    END-IF
      *!
               END-IF
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    SPACE               TO  SPA-K97-HKNCOMBI
               IF      SPA-ERRCD           =   "0007"
                   MOVE    "9100"          TO  WRK-ERRCD1
                   MOVE    SPACE           TO  SPA-ERRCD
               ELSE
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    SPA-ERRCD       TO  WRK-ERRCD2
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
                   IF      IDX2            <   SPA-GMN-MAX
                       MOVE    SPA-GMN-MAX         TO  IDX2
                   END-IF
               END-IF
           END-IF
      *
           IF      IDX2                >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  IDX2
           END-IF
           MOVE    ZERO                TO  IDX-TEI
           COMPUTE IDX-STR             =   ORCSCWKSRY-STRIDX  +  1
      *
      *    約束コード以外の画面再編集
           PERFORM VARYING     IDX-1 FROM    IDX-STR BY  1
                   UNTIL      (IDX-1     >   SPA-MAX-LINE )  OR
                             ((SPA-GMN-INPUTCD(IDX-1) =   SPACE )
                          AND (SPA-COM-INPUTCD(IDX-1) =   SPACE ))
      *
      *H29.5
      *API(HAORI)
      *        HAORI　加算自動算定
               IF      SPA-COM-KSNAUTO (IDX-1) =   "Y"
      *            自動算定なし
                   MOVE    1                   TO  FLG-AUTONO
                   MOVE    SPACE               TO  SPA-COM-KSNAUTO
                                                           (IDX-1)
               ELSE
                   MOVE    ZERO                TO  FLG-AUTONO
               END-IF
      *
      *
               IF     (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "S" OR ".") OR
                      (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "S"       ) OR
                      (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "*"       ) OR
                      (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "#"
                                                     OR  "$"       )
                   CONTINUE
               ELSE
      *            点数マスタ編集
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG
                   MOVE    IDX-1               TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(IDX-1)
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
                   IF     (SPA-ERRCD           NOT =   SPACE )  OR
                          (LNK-SC92-ERRAREA    NOT =   SPACE )
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX-1)
                   END-IF
      *            残量廃棄の判定
                   IF     (SPA-COM-SRYKBN (IDX-1)(1:1)  =   "3" )  AND
                          (SPA-COM-INPUTCD(IDX-1)(1:1)  =   "6" )
                       PERFORM 10025-HAIKI-CHKI-SEC
                   END-IF
      *            注射以下
                   IF     (SPA-HAIKIKBN-FLG    =   "2"   )   AND
                          (SPA-COM-INPUTCD(IDX-1)(1:1)  =   "6" ) AND
                          (SPA-COM-SRYKBN (IDX-1)(1:1)  =   "4"
                                                        OR  "5"
                                                        OR  "6"
                                                        OR  "7"
                                                        OR  "8" )
                       PERFORM 10025-HAIKI-CHKI-SEC
                   END-IF
      *!!!!
      *H26.4
      *            精神通院の２０歳加算の自動発生なし
                   IF     (SPA-SRYYMD              >=  "20140401")
      ******          AND (ORCSCWKSRY-SYORIFLG     =   ZERO      )
      *H29.5
      *API(HAORI)
                      AND ((ORCSCWKSRY-SYORIFLG    =   ZERO    )
                        OR (FLG-AUTONO             =   1       ))
      *H31.4   中途終了追加以外
                     AND  (ORCSCWKSRY-K09TUIKA  NOT  =   1   )
      *
                      AND (SPA-COM-CDKBN-KBN (IDX-1)   =   "I"  )
                      AND (SPA-COM-CDKBN-KBNNUM(IDX-1) =   "002")
                       IF      (SPA-COM-AUTOKBN(IDX-1) =  "2"   )
                           MOVE    SPACE          TO  SPA-COM-AUTOKBN
                                                             (IDX-1)
                       END-IF
                       IF      (SPA-COM-DATAKBN(IDX-1) =  "1"   )
                           MOVE    "1"            TO  SPA-COM-KSNAUTO
                                                             (IDX-1)
                       END-IF
                   END-IF
      *H29.7
      *            心身療法の２０歳加算の自動発生なし(H29/7から）
                   IF     (SPA-SRYYMD              >=  "20170701")
                      AND ((ORCSCWKSRY-SYORIFLG    =   ZERO    )
      *API(HAORI)
                        OR (FLG-AUTONO             =   1       ))
      *H31.4   中途終了追加以外
                     AND  (ORCSCWKSRY-K09TUIKA  NOT  =   1   )
      *
                      AND (SPA-COM-CDKBN-KBN (IDX-1)   =   "I"  )
                      AND (SPA-COM-CDKBN-KBNNUM(IDX-1) =   "004")
                       IF      (SPA-COM-AUTOKBN(IDX-1) =  "2"   )
                           AND (SPA-COM-DATAKBN(IDX-1) =  "2"   )
                           MOVE    SPACE          TO  SPA-COM-AUTOKBN
                                                             (IDX-1)
                       END-IF
                       IF      (SPA-COM-DATAKBN(IDX-1) =  "1"   )
                           MOVE    "1"            TO  SPA-COM-KSNAUTO
                                                             (IDX-1)
                       END-IF
                   END-IF
      *H29.2
      *            在宅加算２の訂正時自動発生対応
                   IF     (SPA-SRYYMD              >=  "20160401")
                      AND (SPA-COM-CDKBN-KBN (IDX-1)   =   "C"  )
                      AND (SPA-COM-CDKBN-KBNNUM(IDX-1) =   "002")
      ****            AND (ORCSCWKSRY-SYORIFLG     =   ZERO      )
      *H29.5
      *API(HAORI)
                      AND ((ORCSCWKSRY-SYORIFLG    =   ZERO    )
                        OR (FLG-AUTONO             =   1       ))
      *!!!!
      *H31.4   中途終了追加以外
                     AND  (ORCSCWKSRY-K09TUIKA  NOT  =   1   )
      *
                       PERFORM 10026-ZAITAKU-AUTO-SEC
                   END-IF
      *
      *            回数の入力（外来のみ）
                   IF     (SPA-NYUGAIKBN       =   "2" )  AND
                          (SPA-COM-KAIFLG (IDX-1)  =   "1" )
                       IF     (SPA-COM-KAISU(IDX-1)    >   1  )  OR
                              (SPA-COM-SRYKBN(IDX-1)(1:1)
                                                   =   "2"    )  OR
                              (IDX-1               =   SPA-MAX-LINE)
                           CONTINUE
                       ELSE
                           IF     (SPA-COM-INPUTCD(IDX-1 + 1)
                                                   =   SPACE   )  OR
                                  (SPA-GMN-INPUTCD(IDX-1 + 1)(1:1)
                                                   =   "."     )
                               MOVE    SPACE       TO  SPA-COM-KAIFLG
                                                         (IDX-1)
                           ELSE
                               IF    ((SPA-COM-SRYKBN (IDX-1 + 1)
                                               NOT =  SPA-COM-SRYKBN
                                                       (IDX-1) )  AND
                                      (SPA-COM-SRYKBN (IDX-1)
                                                   >   "13"    )  AND
                                      (SPA-COM-INPUTCD(IDX-1 + 1)(1:1)
                                                   =   "6"     ))
      *                         コメントの時
                                OR   ((SPA-COM-SRYKBN (IDX-1 + 1)
                                                   =  SPA-COM-SRYKBN
                                                       (IDX-1) )  AND
                                      (SPA-COM-SRYKBN (IDX-1 + 1)
                                                   =   "99"
                                                   OR  "98"   ) AND
                                      (SPA-COM-SRYSYUKBN (IDX-1 + 1)
                                                   =   SPACE   ))
      *                         コメントの時
                                OR   ((SPA-COM-SRYKBN (IDX-1)
                                                   =   "95"
                                                   OR  "96"    )  AND
                                      (SPA-COM-SRYKBN (IDX-1 + 1)
                                                   =   "99"
                                                   OR  "98"   ) AND
                                      (SPA-COM-SRYSYUKBN (IDX-1 + 1)
                                                   =   SPACE   ))
                                   CONTINUE
                               ELSE
                                   MOVE    SPACE   TO  SPA-COM-KAIFLG
                                                         (IDX-1)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
      *            入力コードの画面用再編集
                   MOVE    IDX-1               TO  IDX
                   PERFORM 10026-INPUTCD-HEN-SEC
                   IF     (SPA-COM-CUR (IDX-1) =   SPACE )  AND
                          (SPA-COM-CHKFLG (IDX-1)  =   "1"   )
      *                コメントの全角チェックをする
                     AND  (SPA-COM-MEIFLG (IDX-1)  =   SPACE )
                       MOVE    SPA-GMN-INPUTCD(IDX-1)
                                          TO  SPA-MAE-INPUTCD (IDX-1)
                   ELSE
                       MOVE    SPACE      TO  SPA-MAE-INPUTCD (IDX-1)
                       MOVE    SPACE      TO  SPA-COM-CHKFLG (IDX-1)
                   END-IF
               END-IF
      *        入院回数の画面用再編集
               IF      SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "*"
                   PERFORM 10028-NYUIN-KAISU-SEC
      *!           MOVE    SPACE           TO  SPA-ERRCD
               END-IF
      *        訂正の展開時はすべて自動発生済みとする
               IF      ORCSCWKSRY-SYORIFLG =   ZERO
                   MOVE    IDX-1           TO  IDX
                   MOVE    "1"             TO  SPA-COM-AUTOFLG (IDX)
      *            寝たきり老人在宅総合診療科判定
                   IF     (SPA-COM-INPUTCD(IDX)    =   "114701210" OR
                                                       "114701710" )
      *                次行が自動発生の２４時間加算の時、自動としない
                       IF      (IDX                <   SPA-MAX-LINE) AND
                               (SPA-COM-AUTOKBN (IDX + 1)  =   "2")
                           MOVE    SPACE           TO  SPA-COM-AUTOKBN
                                                           (IDX + 1)
                       END-IF
                   END-IF
      *
      *            検査の逓減がある場合、フラグを変更する
                   IF     (SPA-COM-SRYKBN (IDX)    =   "60" )  AND
                          (SPA-COM-INPUTCD(IDX)(1:1)   =   "1")
                       IF      (SPA-COM-DATAKBN (IDX)  =   "1")  AND
                               (SPA-COM-TEIGENKBN(IDX) =   1  )
                           MOVE    IDX                 TO  IDX-TEI
                       END-IF
                       IF      (SPA-COM-DATAKBN (IDX)  =   "2")  AND
                               (IDX-TEI                >   ZERO) AND
                               (SPA-COM-AUTOKBN (IDX)  =   "2")  AND
                               (SPA-COM-TENSIKIBETU(IDX)   =  6 ) AND
                               (SPA-COM-KIJUNTEIGENKBN(IDX)   =  ZERO)
      *                    自動発生で％減算、施設基準逓減区分がないもの
                           MOVE    1           TO  SPA-COM-TEIGEN-FLG
                                                          (IDX-TEI)
                       END-IF
                   END-IF
      *H22.4改正
      *            ＣＴの逓減がある場合、フラグを変更する
                   IF     (SPA-SRYYMD              >=  "20100401" ) AND
                          (SPA-COM-TNSSRYSYUKBN(IDX)   =   "723"
                                                       OR  "724"  ) AND
                          (SPA-COM-INPUTCD(IDX)(1:1)   =   "1"    )
                       IF      (SPA-COM-DATAKBN (IDX)  =   "1") 
                           MOVE    IDX                 TO  IDX-TEI
                       END-IF
                       IF      (SPA-COM-DATAKBN (IDX)  =   "2")  AND
                               (IDX-TEI                >   ZERO) AND
                               (SPA-COM-AUTOKBN (IDX)  =   "2")  AND
                               (SPA-COM-TENSIKIBETU(IDX)   =  6 )
      *                    自動発生で％減算
                           MOVE    1           TO  SPA-COM-TEIGEN-FLG
                                                          (IDX-TEI)
                       END-IF
                   END-IF
      *H22.4改正
               END-IF
      *
               IF      SPA-COM-ZAIEND (IDX-1)  =   "1"
                   MOVE    ZERO                TO  IDX-TEI
               END-IF
      *        最終行
               MOVE    IDX-1               TO  IDX2
           END-PERFORM
      *
           MOVE    ZERO              TO  FLG-FIRUMU
           PERFORM VARYING     IDX-1 FROM    IDX2   BY  -1
                   UNTIL       IDX-1     <   1
               IF      SPA-COM-ZAIEND (IDX-1)  =   "1"
                   MOVE    ZERO            TO  FLG-FIRUMU
               END-IF
      *        画像診断のフィルム判定
               IF      SPA-COM-SRYKBN (IDX-1)  =   "70"
                   IF    ((SPA-COM-INPUTCD(IDX-1)(1:1)   =   "7" )  OR
                          (SPA-COM-INPUTCD(IDX-1)(1:3)   =   "059")) AND
                          (SPA-COM-DATAKBN (IDX-1)       =   "3" )
                       MOVE    1               TO  FLG-FIRUMU
                   END-IF
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" )
                     AND  (SPA-COM-DATAKBN (IDX-1)       =   "1" )
                     AND  (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)
                     AND  (SPA-COM-SURYO (IDX-1)         >   1   )
                       IF      FLG-FIRUMU                =   ZERO
                           MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                       ELSE
                           IF      SPA-COM-SURFLG (IDX-1)
                                                       NOT =   "1"
                               MOVE    1           TO  SPA-COM-SURYO
                                                               (IDX-1)
      *                        入力コードの画面用再編集
                               MOVE    IDX-1               TO  IDX
                               PERFORM 10026-INPUTCD-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
      *H31.3
      *            加算はフィルムに関係なく手入力（脳脊髄造影剤使用撮影加算）
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" )
                     AND  (SPA-COM-DATAKBN (IDX-1)       =   "2" )
                     AND  (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)
                     AND  (SPA-COM-SURYO (IDX-1)         >   1   )
                       MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                   END-IF
               END-IF
      *        画像診断以外の数量入力
               IF      SPA-COM-SRYKBN (IDX-1)  NOT =   "70"
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" ) 
      *H31.3         AND  (SPA-COM-DATAKBN (IDX-1)       =   "1" )
                     AND  (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)
                     AND  (SPA-COM-SURYO (IDX-1)         >   1   )
                       MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ワーク診療行為の時、セットの展開
           IF     (FLG-WKSYORIFLG      =   1     )
               PERFORM VARYING     IDX-1 FROM    IDX-STR BY  1
                       UNTIL      (IDX-1     >   SPA-MAX-LINE )
                   IF      (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "P" )  OR
                           (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "P" )
                       MOVE    SPACE               TO  SPA-COM-KAIFLG
                                                            (IDX-1)
                       MOVE    IDX-1               TO  SPA-GMN-IDX
                       PERFORM 10028-INPUTSET-SYORI-SEC
                   END-IF
               END-PERFORM
      *        空白行削除
               PERFORM 100282-SYOKYO-SEC
           END-IF
      *
      *    約束コードの画面用再編集
           MOVE    SPACE               TO  WRK-ERRCD
           PERFORM VARYING     IDX-1   FROM    IDX-STR   BY  1
                   UNTIL      (IDX-1   >   SPA-MAX-LINE )  OR
                             ((SPA-COM-INPUTCD(IDX-1)    =   SPACE) AND
                              (SPA-GMN-INPUTCD(IDX-1)    =   SPACE))
               IF      SPA-COM-INPUTCD(IDX-1)(1:1)   =   "S"
                   MOVE    IDX-1               TO  IDX
                   MOVE    SPA-COM-INPUTCD (IDX-1)
                                               TO  SPA-GMN-INPUTCD
                                                              (IDX-1)
      *            入力コードの画面用再編集
                   PERFORM 10026-INPUTCD-HEN-SEC
      *?           MOVE    SPACE               TO  SPA-MAE-INPUTCD (IDX)
                   MOVE    IDX-1               TO  SPA-GMN-IDX
      *
                   MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                             (IDX-1)
                   MOVE    SPACE               TO  SPA-COM-KAIFLG
                                                             (IDX-1)
      *
                   INITIALIZE                  ORCSC97AREA
                   MOVE    "K02"               TO  LNK-SC97-PG
                   MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               TO  LNK-SC97-INPUTCD
                   CALL    "ORCSC97"           USING
                                               SPA-AREA
                                               ORCSC97AREA
                                               SPA-SCR-REC
      *
                   PERFORM 10028-INPUTSET-SYORI-SEC
               END-IF
           END-PERFORM
      *    最大行数
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1   >   SPA-MAX-LINE )
                          OR ((SPA-COM-INPUTCD(IDX-1)    =   SPACE)
                          AND (SPA-GMN-INPUTCD(IDX-1)    =   SPACE))
               MOVE    IDX-1               TO  SPA-GMN-MAX
           END-PERFORM
      *
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  ORCSCWKSRY-ERRCD
               IF      ORCSCWKSRY-SYORIFLG =   1
                   MOVE    "7004"          TO  ORCSCWKSRY-ERRCD
               END-IF
               IF      ORCSCWKSRY-K09TUIKA =   1
                   MOVE    "7004"          TO  ORCSCWKSRY-ERRCD
               END-IF
           ELSE
               MOVE    WRK-ERRCD           TO  ORCSCWKSRY-ERRCD
           END-IF
           MOVE    WRK-ERRCD2          TO  ORCSCWKSRY-ERRCD2
      *
           .
       1002-SAIHEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    残量廃棄の判定処理
      *****************************************************************
       10025-HAIKI-CHKI-SEC              SECTION.
      *
           IF      (SPA-COM-YKZKBN (IDX-1)  =   "4"  )  AND
                   (SPA-COM-TANICD (IDX-1)  =   "014"
                                            OR  "022"
                                            OR  "046" )  AND
                   (SPA-COM-SURYO(IDX-1)(6:3)  >   ZERO)
           AND     (SPA-COM-YAKUHYO(IDX-1)  =   SPACE )
               IF     (IDX-1               <   SPA-MAX-LINE  )  AND
                      (SPA-COM-INPUTCD(IDX-1 + 1)  =   "099309901")
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  SPA-COM-AUTOFLG2(IDX-1)
               END-IF
           END-IF
      *
           .
       10025-HAIKI-CHKI-EXT.
           EXIT.
      *
      *H29.2
      *****************************************************************
      *    （在支診等以外）の加算処理
      *****************************************************************
       10026-ZAITAKU-AUTO-SEC              SECTION.
      *
      *    在宅時医学総合管理料等判定（在医総管・特医総管）
           SET     TBL-ZSIDX           TO  1
           SEARCH  TBL-ZISOSIN-OCC     VARYING   TBL-ZSIDX
              AT   END
                   MOVE    SPACE                   TO  WRK-ZS-KBN
                   MOVE    SPACE                   TO  WRK-ZS-KBN2
                   MOVE    SPACE                   TO  WRK-ZS-GRP
               WHEN   (TBL-ZS-SRYCD (TBL-ZSIDX)
                                           =   SPA-COM-INPUTCD (IDX-1)) 
                   MOVE    TBL-ZS-GRP (TBL-ZSIDX)  TO  WRK-ZS-GRP
                   MOVE    TBL-ZS-KBN (TBL-ZSIDX)  TO  WRK-ZS-KBN
                   MOVE    TBL-ZS-KBN2(TBL-ZSIDX)  TO  WRK-ZS-KBN2
           END-SEARCH
      *
           IF     (WRK-ZS-GRP(1:1)        =   "0"
                                          OR  "1" )
      *
               MOVE    ZERO              TO  FLG-CHKEND
               MOVE    ZERO              TO  FLG-CHKAUTO
               PERFORM VARYING     IDX   FROM    IDX-1   BY  1
                       UNTIL      (IDX   >   SPA-MAX-LINE )
                           OR   ((SPA-COM-INPUTCD(IDX) =   SPACE) AND
                                 (SPA-GMN-INPUTCD(IDX) =   SPACE))
                           OR   ( FLG-CHKEND           =    1   )
                   IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1"  )
                      AND (SPA-COM-AUTOKBN(IDX)        =   "2"  )
      *                在宅実績加算のチェック
                       PERFORM 330811-ZISOKSN-CHK-SEC
      *                在宅実績加算が自動発生分
                       IF     (WRK-ZKS-GRP(1:1)    =   "0"
                                                   OR  "1"    )
                           MOVE    1               TO  FLG-CHKAUTO
                       END-IF
                   END-IF
                   IF      SPA-COM-ZAIEND (IDX)     =   "1"
                       MOVE    1                    TO  FLG-CHKEND
                   END-IF
               END-PERFORM
               IF      FLG-CHKAUTO        =   1
                   CONTINUE
               ELSE
      *            在宅実績加算が自動発生でない時自動発生なしとする
                   MOVE    "1"             TO  SPA-COM-KSNAUTO(IDX-1)
               END-IF
           END-IF
           .
       10026-ZAITAKU-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅実績加算処理
      *****************************************************************
       330811-ZISOKSN-CHK-SEC         SECTION.
      *
           SET     TBL-ZKSIDX          TO  1
           SEARCH  TBL-ZISOKSN-OCC     VARYING   TBL-ZKSIDX
              AT   END
                   MOVE    SPACE                   TO  WRK-ZKS-KBN
                   MOVE    SPACE                   TO  WRK-ZKS-GRP
              WHEN   (TBL-ZKS-SRYCD (TBL-ZKSIDX)
                                       =   SPA-COM-INPUTCD
                                                        (IDX))
                   MOVE    TBL-ZKS-GRP (TBL-ZKSIDX) TO  WRK-ZKS-GRP
                   MOVE    TBL-ZKS-KBN (TBL-ZKSIDX) TO  WRK-ZKS-KBN
           END-SEARCH
           .
       330811-ZISOKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎に剤点数を編集処理
      *****************************************************************
       10021-WKSRY-ZAITENHEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI
           PERFORM VARYING   IDX       FROM    LNK-WKSRYACT-MAX  BY  -1
                   UNTIL     IDX       <       1
               IF    ( IDX             =   LNK-WKSRYACT-MAX  )  OR
                     ((IDX             <   SPA-MAX-LINE  )    AND
                      (LNK-WKSRY-ZAINUM (IDX + 1)
                                       NOT =   LNK-WKSRY-ZAINUM(IDX)))
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                               TO  WRK-TENSUKEI
               END-IF
               MOVE    WRK-TENSUKEI        TO  LNK-WKSRY-ZAITENKEI(IDX)
           END-PERFORM
           .
       10021-WKSRY-ZAITENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   複数科・保険編集処理
      *****************************************************************
       10024-FUKU-HEN-SEC              SECTION.
      *
      *    保険組合せ
           IF    ((LNK-WKSRY-INPUTCD (IDX IDY)(1:1) =   "#" ) AND
                  (LNK-WKSRY-INPUTCD (IDX IDY)(2:4) NUMERIC ) ) OR
                 ((LNK-WKSRY-INPUTCD (IDX IDY)(1:1) =   "$" ) AND
                  (LNK-WKSRY-INPUTCD (IDX IDY)(2:2) NUMERIC ) )
      *        展開に複数科あり
               MOVE    1               TO  FLG-FUKU
               ADD     1                   TO  IDX2
               IF      IDX2                >   SPA-MAX-LINE
                    MOVE    "9100"         TO  WRK-ERRCD1
               ELSE
                   MOVE    LNK-WKSRY-INPUTCD (IDX IDY)
                                               TO  SPA-GMN-INPUTCD
                                                            (IDX2)
                   IF      IDX-HZN             =   ZERO
                       MOVE    IDX2                TO  IDX-HZN
                   END-IF
               END-IF
           END-IF
      *
           .
       10024-FUKU-HEN-EXT.
           EXIT.
      *
      *HAORI
      *****************************************************************
      *   中途データ展開の時、主科・保険設定編集処理
      *****************************************************************
       10029-SRYKA-HEN-SEC             SECTION.
      *
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
      *
           PERFORM VARYING     IDX-1 FROM    1     BY  1
                   UNTIL      (IDX-1     >   SPA-MAX-LINE )  OR
                             ((SPA-GMN-INPUTCD(IDX-1) =   SPACE )
                          AND (SPA-COM-INPUTCD(IDX-1) =   SPACE ))
               IF      SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "#"
                   MOVE    SPACE                     TO  WRK-HKNCOMBI
               END-IF
               IF      SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "$"
                   MOVE    SPACE                     TO  WRK-SRYKA
               END-IF
      *
               MOVE    WRK-HKNCOMBI        TO  SPA-COM-HKNCOMBI(IDX-1)
               MOVE    WRK-SRYKA           TO  SPA-COM-SRYKA   (IDX-1)
      *
               IF      SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "1"
      *            点数マスタ編集
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG
                   MOVE    IDX-1               TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(IDX-1)
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
               END-IF
           END-PERFORM
           .
       10029-SRYKA-HEN-EXT.
           EXIT.
      *HAORI
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       10020-SPA-HEN-SEC              SECTION.
      *
      *    診療種別セット
           IF     (IDXS                    =   ZERO)  AND
                  (LNK-WKSRY-RENNUM (IDX)  =   1   )
               IF      IDX2                <   SPA-MAX-LINE
                   IF      SPA-NYUGAIKBN       =   "2"
                       PERFORM 310112-SRYSYUKBN-GAI-SEC
                   ELSE
                       PERFORM 310111-SRYSYUKBN-NYU-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    投薬の判定
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 310113-INGAI-CHK-SEC
           END-IF
      *
      *    明細編集
           ADD     1                   TO  IDX2
           IF      IDX2                >   SPA-MAX-LINE
                MOVE    "9100"         TO  WRK-ERRCD1
                GO      TO      10020-SPA-HEN-EXT
           END-IF
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-COM-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-GMN-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                       TO  SPA-COM-SRYSYUKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                       TO  SPA-COM-INPUTCD (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO   (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO2  (IDX2)
           MOVE    LNK-WKSRY-AUTOKBN (IDX IDY)
                                       TO  SPA-COM-AUTOKBN  (IDX2)
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "A" OR "B" OR "C"  OR  "D"
                                       OR  "E" OR "F" OR "G"  OR  "H"
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *!!
      *    器材商品名対応
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "T"
               MOVE    "1"             TO  SPA-COM-AUTOKBN2(IDX2)
               MOVE    SPACE           TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    数量入力時
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "S"
               MOVE    "1"                 TO  SPA-COM-SURFLG  (IDX2)
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    訂正時登録時の区分
           IF      ORCSCWKSRY-SYORIFLG =   ZERO
               MOVE    SPA-COM-AUTOKBN (IDX2)
                                       TO  SPA-COM-MAE-AUTOKBN  (IDX2)
           END-IF
      *
           IF      FLG-AUTO-OK         =   1
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *@
      *    包括剤の設定
           IF      LNK-WKSRY-CONTKBN (IDX) =   "H"
               MOVE    1               TO  SPA-COM-HOUKATU-FLG(IDX2)
               MOVE    1               TO  SPA-COM-HOUKATU-ZAI(IDX2)
      *        包括診療行為チェックをオンにする
               IF      SPA-HKTSANTEI-FLG   =   ZERO
                   MOVE   1                TO  SPA-HKTSANTEI-FLG
               END-IF
           END-IF
      *@
      *
           MOVE    LNK-WKSRY-INPUTCHI-G (IDX IDY)
                                       TO  SPA-COM-INPUTCHI-G  (IDX2)
      *
      *    回数を編集
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU(IDX2)
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU2(IDX2)
           MOVE    IDX2                TO  IDXS
      *    分画数を編集
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-BUNGSU (IDX2)
           END-IF
      *!!
      *    内服１種類区分
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "6"  )  AND
                  (LNK-WKSRY-INPUTKBN(IDX IDY) =   "2"  )
                MOVE    "1"                TO  SPA-COM-INPUTNAIF (IDX2)
                MOVE    "1"                TO  SPA-COM-TANDKKBN  (IDX2)
           END-IF
      *    コメントの継続
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "8"  )  OR
                  (SPA-COM-INPUTCD (IDX2)(1:3) =   "008")
               IF     (LNK-WKSRY-INPUTKBN (IDX IDY)    =   "1" )
                   MOVE    "1"             TO  SPA-COM-INPUTCONT (IDX2)
               END-IF
           END-IF
      *!!
      *!!
      *    外用の回数を編集
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-KAISU2(IDX2)
               IF      SPA-COM-KAISU2(IDX2)    =   ZERO
                   MOVE    1               TO  SPA-COM-KAISU2(IDX2)
               END-IF
      *        数量の再計算
               IF     (SPA-COM-KAISU2  (IDX2)  >   1    )  AND
                     ((SPA-COM-INPUTCD (IDX2)(1:1) =   "6"
                                                   OR  "7" ) OR
                      (SPA-COM-INPUTCD (IDX2)(1:3) =   "059") OR
                      (SPA-COM-INPUTCD (IDX2)(1:1) =   "S"  ))
                   COMPUTE SPA-COM-SURYO2  (IDX2)
                                       =   SPA-COM-SURYO2  (IDX2)
                                       /   SPA-COM-KAISU2(IDX2)
               END-IF
               IF      LNK-WKSRY-ZAIKAIKEI(IDX) >   1
                   COMPUTE SPA-COM-KAISU2  (IDX2)
                                       =   SPA-COM-KAISU2  (IDX2)
                                       *   LNK-WKSRY-ZAIKAIKEI
                                                           (IDX)
               END-IF
               MOVE    SPA-COM-KAISU2  (IDX2)
                                       TO  SPA-COM-KAISU (IDX2)
               MOVE    SPA-COM-SURYO2  (IDX2)
                                       TO  SPA-COM-SURYO (IDX2)
           END-IF
      *ver.4.5.0
      *    換算値入力
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "6"  )  AND
                  (LNK-WKSRY-KANSURYO (IDX IDY) >   ZERO )
      *        点数マスタの換算値×換算値数量＝数量の時のみ設定
               MOVE    SPA-COM-INPUTCD (IDX2)  TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 9101-TENSUPLUS-SYORI-SEC
                   COMPUTE WRK-KANSURYO    =  (TNSP-KANZANCHI
                                           *   LNK-WKSRY-KANSURYO
                                                          (IDX IDY))
                   IF      WRK-KANSURYO    =   LNK-WKSRY-SRYSURYO
                                                          (IDX IDY)
                       MOVE    LNK-WKSRY-KANSURYO (IDX IDY)
                                           TO  SPA-COM-KANSURYO (IDX2)
      *                外用薬は÷回数
                       IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
                           COMPUTE WRK-KANSURYO   =  LNK-WKSRY-KANSURYO
                                                          (IDX IDY)
                                                   /   SPA-COM-KAISU2
                                                            (IDX2)
                           MOVE    WRK-KANSURYO
                                           TO  SPA-COM-KANSURYO (IDX2)
                       END-IF
                   END-IF
      *           外来投薬で、１種類設定可能かの判定
                   IF      (SPA-COM-SRYKBN (IDX2)  =   "21"  )
                   AND     (SPA-NYUGAIKBN          =   "2"   )
                   AND     (SPA-COM-KANSURYO(IDX2) NOT =   ZERO )
      *                ｇ、�咫，髻（顱　■蹌譟，髻）�
                       IF    ((TNS-TANICD          =   "032"
                                                   OR  "033")  AND 
                              (TNSP-KANZANTANICD   =  "018"))
                       OR    ((TNS-TANICD          =   "036") AND
                              (TNSP-KANZANTANICD   =   "007"))
                           MOVE    "1"             TO  SPA-COM-TANDKKBN
                                                                (IDX2)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    薬評・減点
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   1
               MOVE    "1"                 TO  SPA-COM-YAKUHYO (IDX2)
           END-IF
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   2
               MOVE    "1"                 TO  SPA-COM-GENTEN  (IDX2)
           END-IF
      *
      *H28.9
      *    持参薬
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   3
               MOVE    "1"                 TO  SPA-COM-JISANYAKU(IDX2)
           END-IF
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   4
               MOVE    "2"                 TO  SPA-COM-JISANYAKU(IDX2)
           END-IF
      *!!
           IF      LNK-WKSRY-INPUTCD (IDX IDY) =   SPACE
               MOVE    SPA-COM-INPUTCD (IDX2)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           ELSE
               MOVE    LNK-WKSRY-INPUTCD (IDX IDY)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           END-IF
           MOVE    SPA-GMN-INPUTCD (IDX2)  TO  SPA-MAE-INPUTCD (IDX2)
      *
      *    剤点数の設定（訂正前）
           MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                       TO  SPA-COM-MAE-TENSU (IDX2)
      *
      *    約束セットの時、以下処理なし
           IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
               GO      TO      10020-SPA-HEN-EXT
           END-IF
      *
      *    時間加算
           IF      (SPA-GMN-INPUTCD (IDX2)(1:1)  IS  NUMERIC) AND
                   (SPA-GMN-INPUTCD (IDX2)(2:1)  =   SPACE  ) AND
                   (SPA-GMN-INPUTCD (IDX2)(3:1)  NOT =   SPACE)
               MOVE    SPA-GMN-INPUTCD (IDX2)(1:1)
                                           TO  SPA-COM-TIMEFLG(IDX2)
      *        入院で訂正の時、設定なし
               IF     (SPA-NYUGAIKBN       =   "1"   )  AND
                      (ORCSCWKSRY-SYORIFLG =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    SPA-COM-TIMEFLG (IDX2)
                                           TO  SPA-NAI-TIMEFLG
               END-IF
           END-IF
      *
      *    コメントの時、コメント内容編集
           IF      SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8"
               IF      LNK-WKSRY-INPUTCOMENT (IDX IDY)  NOT =   SPACE
                   IF      SPA-COM-INPUTCD (IDX2)       =   "810000001"
                                                        OR  "008500000"
                                                        OR  "008600000"
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO-G (IDX2)
                       MOVE    "1"         TO  SPA-COM-MEIFLG   (IDX2)
                   ELSE
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO   (IDX2)
                       IF     (SPA-COM-INPUTCD (IDX2)(1:2)  =   "83" )
                         OR   (SPA-COM-INPUTCD (IDX2)(1:4)  =   "0083")
                         OR   (SPA-COM-INPUTCD (IDX2)(1:4)  =   "0085")
                         OR   (SPA-COM-INPUTCD (IDX2)(1:4)  =   "0086")
                           MOVE    "1"     TO  SPA-COM-MEIFLG   (IDX2)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *H29.5
      *API(HAORI)
      *    加算自動算定なし
           IF     (SPA-API-FLG             =   1   )
             AND  (SPA-COM-INPUTCD(IDX2)(1:1)
                                           =   "1"  )
               IF     (LNK-WKSRY-INPUTCOMENT(IDX IDY)   =   "Yes" )
                   MOVE    "Y"             TO  SPA-COM-KSNAUTO(IDX2)
               END-IF
               MOVE    SPACE           TO  LNK-WKSRY-INPUTCOMENT
                                                        (IDX IDY)
           END-IF
      *
      *
      *    金額入力時、金額へ
           IF      SPA-COM-INPUTCD(IDX2)(1:3)  =   "095"  OR  "096"
      *        自賠責の時、対象外
               IF     (SPA-HKNKBN          =   1    )  AND
      *************** (SPA-RSI-HKNKBN      =   "4"  )  AND
                      (SPA-COM-SRYKBN (IDX2)
                                           =   "80" )  AND
                      (SPA-COM-INPUTCD(IDX2)(1:5)
                                           =   "09591"  OR
                                               "09592"  OR
                                               "09593"  OR
                                               "09594"  )
                   MOVE    ZERO                TO  FLG-OK2
               ELSE
                   MOVE    1                   TO  FLG-OK2
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-OK2
           END-IF
           IF      FLG-OK2             =   1
               MOVE    SPA-COM-INPUTCD (IDX2)  TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      TNS-TEN                 =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
      *            CLAIMは自費金額は数量に
                   IF     (LNK-WKSRY-KARTE-FLG (IDX)
                                                   =   ZERO )
                     AND  (LNK-WKSRY-TERMID (1)    =   "WKSRYACT" )
                     AND  (LNK-WKSRY-JIHIMONEY(IDX IDY)
                                                   =   ZERO )
                     AND  (LNK-WKSRY-SRYSURYO (IDX IDY)
                                                   >   1    )
                       MOVE    LNK-WKSRY-SRYSURYO (IDX IDY) 
                                               TO  LNK-WKSRY-JIHIMONEY
                                                             (IDX IDY)
                       MOVE    1               TO  SPA-COM-SURYO
                                                             (IDX2)
                       MOVE    1               TO  SPA-COM-SURYO2
                                                             (IDX2)
                   END-IF
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
               END-IF
               MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
               MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
               MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
      *        剤点数の設定（訂正前）
               MOVE    SPA-COM-JIHIMONEY(IDX2)
                                           TO  SPA-COM-MAE-TENSU (IDX2)
      *        自費の時、金額編集
           ELSE
               IF      SPA-COM-SRYKBN (IDX2)   =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
      *            剤点数の設定（訂正前）
                   MOVE    SPA-COM-JIHIMONEY(IDX2)
                                           TO  SPA-COM-MAE-TENSU (IDX2)
               ELSE
                   MOVE    SPACE               TO  SPA-COM-KEIFLG(IDX2)
      *            金額入力
                   IF      SPA-COM-SRYSYUKBN(IDX2) =   "809"
      *H31.3                   CLAIM,API対応（800で送信）
                                                   OR  "800"
                       MOVE    SPA-COM-INPUTCD (IDX2)  TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-TEN                 =   ZERO
      *                    CLAIMは自費金額は数量に
                           IF     (LNK-WKSRY-KARTE-FLG (IDX)
                                                           =   ZERO )
                             AND  (LNK-WKSRY-TERMID (1)    =
                                                           "WKSRYACT" )
                             AND  (LNK-WKSRY-JIHIMONEY(IDX IDY)
                                                           =   ZERO )
                             AND  (LNK-WKSRY-SRYSURYO (IDX IDY)
                                                           >   1    )
                               MOVE    LNK-WKSRY-SRYSURYO (IDX IDY) 
                                               TO  LNK-WKSRY-JIHIMONEY
                                                             (IDX IDY)
                               MOVE    1       TO  SPA-COM-SURYO
                                                             (IDX2)
                               MOVE    1       TO  SPA-COM-SURYO2
                                                             (IDX2)
                           END-IF
      *
                           MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
                           MOVE    "1"         TO  SPA-COM-KEIFLG(IDX2)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       10020-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別設定（外来）処理
      *****************************************************************
       310112-SRYSYUKBN-GAI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUBETU
      *    診察料以外
           IF    ((LNK-WKSRY-SRYSYUKBN (IDX)   NOT =   SPACE )  AND
                  (LNK-WKSRY-SRYSYUKBN (IDX)(1:2)
                                           NOT =   "11" AND
                                                   "12" AND
                                                   "13"  ))
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *    診察料で前の剤が自費の時
           IF     (LNK-WKSRY-SRYSYUKBN(IDX)(1:2)
                                       =   "11" OR
                                           "12" OR
                                           "13"  )   AND
                 ((IDX2                >   ZERO  ) AND
                  (SPA-COM-SRYKBN (IDX2)
                                       =   "96"  OR  "95"))
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *
      *    診察料で前の剤が731の時
           IF     (LNK-WKSRY-SRYSYUKBN(IDX)(1:2)
                                       =   "11" OR
                                           "12" OR
                                           "13"  )   AND
                 ((IDX2                >   ZERO  ) AND
                  (SPA-COM-SRYSYUKBN (IDX2)
                                       =   "731" ))
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *H28.1
      *    初再診加算
           IF     LNK-WKSRY-SRYSYUKBN(IDX)     =   "114"
                                               OR  "124"
                                               OR  "133"
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *
      *    訂正の時、再診料算定科を再診とする
           IF     (LNK-WKSRY-SRYKBN    (IDX)       =   "12" AND
                   LNK-WKSRY-SRYCD (IDX IDY)       =   "830000021")
               OR (LNK-WKSRY-SRYKBN    (IDX)       =   "11" AND
                   LNK-WKSRY-SRYCD (IDX IDY)       =   "830000020")
      *             訂正の時、ダミー診察の時
              OR ((LNK-WKSRY-SRYKBN    (IDX)       =   "11" OR
                                                       "12"   ) AND
                  (LNK-WKSRY-SRYCD (IDX IDY)       =   "820000003"
                                                   OR  "820000004"
                                                   OR  "820000005"
                                                   OR  "820000006"))
               MOVE    1                   TO  FLG-SYUBETU
           END-IF
           IF      LNK-WKSRY-SRYKBN    (IDX)       =   "13"
      *        小児科外来診察料の加算のみ
               PERFORM 310119-SYONIKA-CHK-SEC
               IF     (WRK-SYONIKA-KBN     =   "4"   )
                  OR  (WRK-TIME-KBN2   NOT =   SPACE )
                   MOVE    1                   TO  FLG-SYUBETU
               END-IF
           END-IF
      *    最初が手技料以外の時
           IF     (LNK-WKSRY-SRYKBN(IDX)   =   "11"
                                           OR  "12"
                                           OR  "13"  ) AND
                  (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                       NOT =   "1" )
               MOVE    1                   TO  FLG-SYUBETU
           END-IF
      *    薬評・減点分の時
           IF     (LNK-WKSRY-ZAIKBN(IDX)   NOT =   ZERO  )
               MOVE    1                   TO  FLG-SYUBETU
           END-IF
      *    CLAIMデータは包括減点コードを判定する
           IF     (FLG-SYUBETU         =   ZERO )  AND
                  (LNK-WKSRY-SRYKBN(IDX)   =   "12"
                                           OR  "13"
                                           OR  "14"  ) AND
                  (LNK-WKSRY-SRYCD (IDX IDY)
                                           =   "199000210"
                                           OR  "199000410" )
               MOVE    1                   TO  FLG-SYUBETU
           END-IF
      *
      *    自動発生の展開分
           IF      FLG-SYUBETU         =   1
               IF     (FLG-AUTO-OK         =   1 ) AND
                      (LNK-WKSRY-SRYKBN (IDX)  NOT =   "70" )
      *            前の剤が自費以外
                   IF      (IDX2                >   ZERO  ) AND
                          ((SPA-COM-SRYKBN (IDX2)
                                               =   "96"  OR  "95")
                      OR   (SPA-COM-SRYSYUKBN (IDX2)
                                               =   "731"   ))
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  FLG-SYUBETU
                   END-IF
               END-IF
           END-IF
      *
      *    画像診断で部位から始まる場合
           IF     (LNK-WKSRY-SRYKBN (IDX)      =   "70" )  AND
                  (LNK-WKSRY-SRYKBN (IDX - 1 ) =   "70"
                                               OR  "95"
                                               OR  "96" )  AND
                  (LNK-WKSRY-SRYCD (IDX IDY)(1:3)  =   "002")  AND
                  (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE )
                MOVE    1               TO  FLG-SYUBETU
                MOVE    "700"           TO  LNK-WKSRY-SRYSYUKBN (IDX)
            END-IF
      *
      *    診療種別のない投薬で前が検査の時
           IF     (FLG-SYUBETU         =   ZERO ) AND
                  (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE ) AND
                  (LNK-WKSRY-SRYKBN (IDX)(1:1) =   "2"   ) AND
                  (LNK-WKSRY-SRYKBN (IDX - 1 ) =   "60"  ) 
                MOVE    1               TO  FLG-SYUBETU
                MOVE    LNK-WKSRY-SRYKBN (IDX)
                                        TO  LNK-WKSRY-SRYSYUKBN (IDX)
                                                           (1:2)
                MOVE    "0"             TO  LNK-WKSRY-SRYSYUKBN (IDX)
                                                           (3:1)
            END-IF
      *
            IF      FLG-SYUBETU         =   1
               ADD     1                   TO  IDX2
               MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                           TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                           TO  SPA-COM-SRYSYUKBN
                                                              (IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)
                                           TO  SPA-COM-SRYKBN(IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)
                                           TO  SPA-GMN-SRYKBN(IDX2)
           END-IF
      *
           .
       310112-SRYSYUKBN-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *   投薬の判定処理
      *****************************************************************
       310113-INGAI-CHK-SEC                  SECTION.
      *
      *    訂正の時、診療種別が未入力で院外の薬剤があった時
           IF     (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE OR
                                                   "210" OR
                                                   "220" OR
                                                   "230" OR
      *           頓服・外用（臨時投薬）追加
                                                   "293" OR
                                                   "296" OR
                                                   "290" )
            AND   (LNK-WKSRY-SRYKBN    (IDX)   =   "21" OR "22"
                                               OR  "23" )
      *        薬評以外
             AND  (LNK-WKSRY-ZAIKBN (IDX)  NOT =   1     )
      *
               IF     (FLG-SYORIFLG            =   1    )  OR
                      (LNK-WKSRY-TERMID (IDX)  =   "WKSRYACT")
                   MOVE    1                   TO  FLG-INGAIKBN-ARI
                   IF      LNK-WKSRY-ZAITENKEI (IDX)   =   ZERO
      *                剤点数＝ゼロ　院外
      *                剤内に薬剤がある時のみ
                       IF     ( LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                               =   "6" )
                           IF      FLG-INGAIKBN    =   ZERO
                               MOVE    1               TO  FLG-INGAIKBN
                           END-IF
                       END-IF
      *H24.10
      *                ユーザ器材で、金額＞ゼロの時、院外とする
                       IF      FLG-INGAIKBN    =   ZERO
                           IF     ( LNK-WKSRY-SRYCD (IDX IDY)(1:3)
                                                   =   "059" )
                               MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                                       TO  TNS-SRYCD
                               PERFORM 910-TENSU-READ-SEC
                               IF      TNS-TEN         >   ZERO
                                   MOVE    1           TO  FLG-INGAIKBN
                               END-IF
                           END-IF
                       END-IF
                   ELSE
      *                剤点数≠ゼロ　院内
                       MOVE    2               TO  FLG-INGAIKBN
                   END-IF
               END-IF
           END-IF
           .
       310113-INGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別設定（入院）処理
      *****************************************************************
       310111-SRYSYUKBN-NYU-SEC                  SECTION.
      *
      *    診療種別セット
           IF     (LNK-WKSRY-SRYSYUKBN (IDX)   NOT =   SPACE )  AND
                 ((LNK-WKSRY-SRYCD (IDX 01)(1:1)   NOT =   "1") OR
                  (LNK-WKSRY-SRYSYUKBN (IDX)(1:1)  =   "9"  OR
                                                       "3"  OR
                                                       "2"   )  OR
                  (LNK-WKSRY-SRYSYUKBN (IDX)(3:1)  NOT =   "0") OR
                  (LNK-WKSRY-SRYSYUKBN (IDX)       =   "610"  )
      *           減点分の時
             OR   (LNK-WKSRY-ZAIKBN  (IDX)     NOT =   ZERO   ))
               ADD     1                       TO  IDX2
               MOVE    "."                     TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-COM-SRYSYUKBN
                                                             (IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  SPA-COM-SRYKBN(IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  SPA-GMN-SRYKBN(IDX2)
           END-IF
      *
           .
       310111-SRYSYUKBN-NYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   小児科外来診察料科チェックク処理
      *****************************************************************
       310119-SYONIKA-CHK-SEC                  SECTION.
      *
      *H28.4 から
           IF      SPA-SRYYMD          >=  "20160401"
                                 AND   <   "20180401"
      *        時間外加算検索
               SET     TBL-TIDX            TO  1
               SEARCH  TBL-TIMEKSN-OCC     VARYING   TBL-TIDX
               AT  END
                   MOVE    SPACE           TO  WRK-TIME-KBN2
                WHEN   LNK-WKSRY-SRYCD (IDX IDY)
                                           =   TBL-TM-SRYCD
                                                      (TBL-TIDX)
                   MOVE    TBL-TM-TIMKBN (TBL-TIDX)
                                           TO  WRK-TIME-KBN2
               END-SEARCH
               GO      TO      310119-SYONIKA-CHK-EXT
           END-IF
      *H30.4 から
           IF      SPA-SRYYMD          >=  "20180401"
      *        時間外加算検索
               SET     TBL-TIDX18            TO  1
               SEARCH  TBL-TIMEKSN18-OCC     VARYING   TBL-TIDX18
               AT  END
                   MOVE    SPACE           TO  WRK-TIME-KBN2
                WHEN   LNK-WKSRY-SRYCD (IDX IDY)
                                           =   TBL-TM18-SRYCD
                                                      (TBL-TIDX18)
                   MOVE    TBL-TM18-TIMKBN (TBL-TIDX18)
                                           TO  WRK-TIME-KBN2
               END-SEARCH
               IF      WRK-TIME-KBN2       =   SPACE
      *            妊婦加算
                   SET     TBL-NPIDX18         TO  1
                   SEARCH  TBL-NINPUKSN18-OCC  VARYING   TBL-NPIDX18
                   AT   END
                       CONTINUE
                    WHEN   LNK-WKSRY-SRYCD (IDX IDY)
                                           =   TBL-NP18-SRYCD
                                                      (TBL-NPIDX18)
                       MOVE    TBL-NP18-TIMKBN (TBL-NPIDX18)
                                           TO  WRK-TIME-KBN2
                   END-SEARCH
               END-IF
               GO      TO      310119-SYONIKA-CHK-EXT
           END-IF
      *
      *    診察料検索
           SET     TBL-SYO             TO  1
           SEARCH  TBL-SYONIKA-OCC     VARYING   TBL-SYO
               AT  END
                   MOVE    SPACE           TO  WRK-SYONIKA-KBN
                WHEN   LNK-WKSRY-SRYCD (IDX IDY)
                                           =   TBL-SYONIKA-SRYCD
                                                      (TBL-SYO)
                   MOVE    TBL-SYONIKA-KBN (TBL-SYO)
                                           TO  WRK-SYONIKA-KBN
           END-SEARCH
      *
      *H26.4 から　地域包括診療料の時間外追加
           IF      SPA-SRYYMD          <   "20140401"
               MOVE    SPACE           TO  WRK-TIME-KBN2
               GO      TO      310119-SYONIKA-CHK-EXT
           END-IF
      *    診察料検索
           SET     TBL2-TIDX            TO  1
           SEARCH  TBL2-TIMEKSN-OCC     VARYING   TBL2-TIDX
               AT  END
                   MOVE    SPACE           TO  WRK-TIME-KBN2
                WHEN   LNK-WKSRY-SRYCD (IDX IDY)
                                           =   TBL2-TM-SRYCD
                                                      (TBL2-TIDX)
                   MOVE    TBL2-TM-KBN2 (TBL2-TIDX)
                                           TO  WRK-TIME-KBN2
           END-SEARCH
           .
       310119-SYONIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   自動算定分編集処理
      *****************************************************************
       10023-AUTOCHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-AUTO-OK
      *
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           MOVE    SPA-SYORIFLG        TO  LNK-SC91-SYORIFLG
           MOVE    LNK-WKSRY-SRYCD  (IDX IDY)
                                       TO  LNK-SC91-SRYCD
           MOVE    "4"                 TO  LNK-SC91-KBN
           MOVE    ZERO                TO  LNK-SC91-GMN-IDX
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC91-TEISEI-AREA
      *
           CALL    "ORCSC91"           USING    MCPAREA
                                                ORCSC91AREA
                                                SPA-SCR-REC
                                                SPA-AREA
                                                TNS-TENSU-REC
           IF      LNK-SC91-ERRCD      =   "K004"
              OR  (LNK-SC91-ERRCD2     =   "K004"  )
               MOVE    1               TO  FLG-AUTO-OK
               MOVE    SPACE           TO  LNK-SC91-ERRCD
           END-IF
      *
      *    薬剤情報で毎回回の算定
           IF     (SPA-YAKJYO-1        =   2 )  AND
                  (FLG-AUTO-OK         =   1 )  AND
                  (LNK-WKSRY-SRYCD  (IDX IDY)   =   "120002370")
             AND  (SPA-NAI-ROUJIN      =   0 )
               MOVE    ZERO            TO  FLG-AUTO-OK
           END-IF
           IF    ((SPA-YAKJYO-2        =   2 )  OR
                  (SPA-YAKJYO-3        =   2 )) AND
                  (FLG-AUTO-OK         =   1 )  AND
                  (LNK-WKSRY-SRYCD  (IDX IDY)   =   "113701410"
                                                OR  "113701310"
                                                OR  "120002370")
             AND  (SPA-NAI-ROUJIN      =   1 )
               MOVE    ZERO            TO  FLG-AUTO-OK
           END-IF
      *
           IF     (SPA-SRYYMD          >=  "20100401" )
               IF     (LNK-WKSRY-SRYCD  (IDX IDY)   =   "113701410"
                                                    OR  "113701310" )
                   MOVE    FLG-HEN-CHK2    TO  FLG-AUTO-OK
               END-IF
               IF     (LNK-WKSRY-SRYCD  (IDX IDY)   =   "120002370" )
                   MOVE    FLG-AUTO-OK     TO  FLG-HEN-CHK2 
               END-IF
           END-IF
      *
           .
       10023-AUTOCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   自動算定分編集処理
      *****************************************************************
       10022-ZENKAICHK-SEC                  SECTION.
      *
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           MOVE    SPA-SYORIFLG        TO  LNK-SC91-SYORIFLG
           MOVE    LNK-WKSRY-SRYCD  (IDX IDY)
                                       TO  LNK-SC91-SRYCD
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    ZERO                TO  LNK-SC91-GMN-IDX
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC91-TEISEI-AREA
      *
           CALL    "ORCSC91"           USING    MCPAREA
                                                ORCSC91AREA
                                                SPA-SCR-REC
                                                SPA-AREA
                                                TNS-TENSU-REC
           IF      LNK-SC91-ERRCD      =   "K004" 
               MOVE    SPACE           TO  LNK-SC91-ERRCD
           END-IF
           IF      LNK-SC91-ERRCD  NOT =   SPACE
               MOVE    1               TO  FLG-HEN-CHK
           END-IF
      *    併用算定チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "4"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           COMPUTE LNK-SC92-GMN-IDX    =   IDX2    +   1
           MOVE    LNK-WKSRY-SRYCD(IDX IDY)
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
           IF      LNK-SC92-ERRCD6 NOT =   SPACE
               MOVE    1               TO  FLG-HEN-CHK
           END-IF
      *
      *    処方料・処方せん料は対象外とする
           IF      LNK-WKSRY-SRYSYUKBN (IDX)  =   "250"
                                              OR  "820"
               EVALUATE    LNK-WKSRY-SRYCD(IDX IDY)
      *            長期投与加算（処方せん）
                   WHEN    "120003270"
      *            特定疾患処方箋
                   WHEN    "120002570"
      *            特定疾患処方料
                   WHEN    "120002270"
      *            長期投与加算（処方）
                   WHEN    "120003170"
                       MOVE    1               TO  FLG-HEN-CHK
               END-EVALUATE
           END-IF
           .
       10022-ZENKAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       10026-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "K02"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           MOVE    SPA-SYORIFLG        TO  LNK-SC94-SYORIFLG
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    前へ編集
           MOVE    SPA-GMN-INPUTCD (IDX)   TO  SPA-MAE-INPUTCD (IDX)
      *
           .
       10026-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *     入院回数の画面用再編集
      *****************************************************************
       10028-NYUIN-KAISU-SEC            SECTION.
      *    入力コードの画面用再編集
           IF     (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "*" )  AND
                  (SPA-GMN-INPUTCD(IDX-1)(2:)    =   SPACE)
               MOVE    IDX-1               TO  IDX
               PERFORM 10026-INPUTCD-HEN-SEC
               MOVE    SPACE               TO  SPA-MAE-INPUTCD (IDX)
           END-IF
      *    回数入力の画面用再編集
           IF     (SPA-GMN-INPUTCD(IDX-1)(1:1)     =   "*"  ) AND
                  (SPA-GMN-INPUTCD(IDX-1)(2:)  NOT =   SPACE)
               MOVE    IDX-1               TO  SPA-GMN-IDX
               INITIALIZE                  ORCSC97AREA
               MOVE    "K02N"              TO  LNK-SC97-PG
               MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                           TO  LNK-SC97-INPUTCD
               CALL    "ORCSC97"           USING
                                           SPA-AREA
                                           ORCSC97AREA
                                           SPA-SCR-REC
      *
      *        変換文字列編集
               PERFORM 100281-KAISUU-CHK-SEC
      *
               IF     (SPA-COM-CUR (IDX-1) =   SPACE )  AND
                      (SPA-COM-CHKFLG (IDX-1)  =   "1"   )
      *            コメントの全角チェックをする
                 AND  (SPA-COM-MEIFLG (IDX-1)  =   SPACE )
                   MOVE    SPA-GMN-INPUTCD(IDX-1)
                                          TO  SPA-MAE-INPUTCD (IDX-1)
               ELSE
                   MOVE    SPACE      TO  SPA-MAE-INPUTCD (IDX-1)
                   MOVE    SPACE      TO  SPA-COM-CHKFLG (IDX-1)
               END-IF
           END-IF
      *
           .
       10028-NYUIN-KAISU-EXT.
           EXIT.
      *****************************************************************
      *    回数入力チェック処理
      *****************************************************************
       100281-KAISUU-CHK-SEC           SECTION.
      *
      *    回数に変更があった時、剤内の警告はクリアする
           IF     SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                   NOT =   SPA-MAE-INPUTCD(SPA-GMN-IDX)
               COMPUTE IDX-K           =   SPA-GMN-IDX   -   1
               PERFORM VARYING     IDX-K2  FROM    IDX-K  BY  -1
                       UNTIL       IDX-K2  <   1
                  IF      SPA-COM-ZAIEND (IDX-K2)  =   "1"
                      MOVE    1                TO  IDX-K2
                  ELSE
                      IF      SPA-COM-SRYKBN (IDX-K2)  =  "80"
                           MOVE    SPACE           TO  SPA-COM-KZMFLG
                                                            (IDX-K2)
                           MOVE    SPACE           TO  SPA-COM-KZMFLG2
                                                            (IDX-K2)
                      END-IF
                  END-IF
               END-PERFORM
           END-IF
      *
      *    クリア
           MOVE    SPACE           TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
      *
      *    回数（未入力時、１回）
           IF      LNK-SC97-KAISUCNT   =   ZERO
               MOVE    1               TO  SPA-COM-KAISU (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               MOVE    LNK-SC97-KAISU  TO  SPA-COM-KAISU (SPA-GMN-IDX)
               IF      SPA-COM-KAISU(SPA-GMN-IDX)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *    入院回数・日数
           INITIALIZE                  SPA-COM-NYUIN-AREA (SPA-GMN-IDX)
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
           MOVE    SPA-NAI-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
              IF      LNK-SC97-NYU-DAY (IDX)   NOT =   ZERO
                   IF     (IDX             >=  WRK-STDD )  AND
                          (IDX             <=  WRK-EDDD )
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                       (SPA-GMN-IDX IDX)
                       ADD     1               TO  SPA-COM-NYU-KAI
                                                       (SPA-GMN-IDX)
                   ELSE
                       MOVE    "9007"          TO  WRK-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                   END-IF
      *            訂正の時、訂正日のみ
                   IF     (SPA-SYORIFLG        =   1   )  AND
                          (IDX             NOT =   WRK-SRYDD)
                       MOVE    "9008"          TO  WRK-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100281-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       10028-INPUTSET-SYORI-SEC      SECTION.
      *
           INITIALIZE                  ORCSCSETHENAREA
           MOVE    "K02"               TO  ORCSCSETHEN-PG
      *    セット名
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  ORCSCSETHEN-SRYCD
      *    対象行
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
      *    ORCSC92検索用
           MOVE    SPA-SYORIFLG        TO  ORCSCSETHEN-SYORIFLG
           MOVE    SPA-NENREI          TO  ORCSCSETHEN-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  ORCSCSETHEN-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  ORCSCSETHEN-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  ORCSCSETHEN-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  ORCSCSETHEN-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  ORCSCSETHEN-TEISEI-AREA
      *
           CALL    "ORCSCSETHEN"       USING
                                       ORCSCSETHENAREA
                                       SPA-AREA
                                       SPA-SCR-REC
      *    展開後の位置
           MOVE    ORCSCSETHEN-IDX     TO  SPA-GMN-IDX
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    ORCSCSETHEN-ERRMSG2     TO  SPA-ERRMSG2
               MOVE    SPA-ERRCD               TO  WRK-ERRCD
               MOVE    SPACE                   TO  SPA-ERRCD
           END-IF
      *
           .
       10028-INPUTSET-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    セット展開の空白行削除処理
      *****************************************************************
       100282-SYOKYO-SEC         SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-HZN-SCR-AREA
           MOVE    ZERO                TO  SPA-GMN-IDX
           INITIALIZE                      SPA-SCR-REC
      *
           PERFORM VARYING     IDX-1     FROM    1   BY  1
     ***********   UNTIL       IDX-1     >=  SPA-MAX-LINE
                   UNTIL       IDX-1     >   SPA-MAX-LINE
               IF     (WRK-HZN-GMN-INPUTCD(IDX-1)    =   SPACE ) AND
                      (WRK-HZN-COM-INPUTCD(IDX-1)    =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   MOVE    WRK-HZN-COM-TBLREC (IDX-1) 
                                           TO  SPA-COM-TBLREC
                                                    (SPA-GMN-IDX)
                   MOVE    WRK-HZN-GMN-TBLREC (IDX-1) 
                                           TO  SPA-GMN-TBLREC
                                                    (SPA-GMN-IDX)
               END-IF
           END-PERFORM
           .
       100282-SYOKYO-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為マスタ登録（中途終了登録）処理
      *****************************************************************
       200-WKSRYACT-INS-SEC         SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    ワーク診療行為の削除
           PERFORM 2001-WKSRYACT-DEL-SEC
      *
           MOVE    1                   TO  FLG-SYORI
           PERFORM 2002-WKSRYACT-SYORI-SEC
      *
           .
       200-WKSRYACT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為展開中の削除処理
      *****************************************************************
       2001-WKSRYACT-DEL-SEC             SECTION.
      *
      *    現在の内容をクリア
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                      WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
           MOVE    SPA-K02N-DOUKBN     TO  WKSRY-DOUJITSUKBN
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    中途追加分の削除
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  WKSRYACT-REC
               MOVE    ZERO                TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1                   TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               IF      WKSRY-MOD-FLG       NOT =   ZERO
      *
                   MOVE    WKSRYACT-REC        TO  MCPDATA-REC
                   MOVE    "tbl_wksryact"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  WKSRYACT-REC
                   MOVE    ZERO                TO  FLG-WKSRYACT
               ELSE
                   MOVE    1                   TO  FLG-WKSRYACT
               END-IF
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-WKSRYACT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       2002-WKSRYACT-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDY
           INITIALIZE                  WRK-WKSRY-AREA
           MOVE    1                   TO  WRK-ZAINUM
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPACE               TO  WKSRYACTPLUS-REC
           INITIALIZE                  WKSRYACTPLUS-REC
           MOVE    ZERO                TO  IDW-STR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                   MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
               ELSE
      *
                   ADD     1                       TO  IDY
                   IF      IDY                     =   1
                       MOVE    IDX                     TO  IDW-STR
                   END-IF
      *            入力コード
                   MOVE    SPA-GMN-INPUTCD(IDX)    TO  WKSRY-INPUTCD
                                                                 (IDY)
      *            行為コード
                   MOVE    SPA-COM-INPUTCD(IDX)    TO  WKSRY-SRYCD
                                                                 (IDY)
      *            セットの時、セットコードを行為コードへ
                   IF     (SPA-COM-INPUTCD(IDX)    =   SPACE )  AND
                          (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" )
                       MOVE    SPA-GMN-INPUTCD(IDX)(1:6)
                                                    TO  WKSRY-SRYCD
                                                                 (IDY)
                   END-IF
      *
      *            数量
      *!!          数量入力のできないコードは数量編集なし
      ****         IF     (SPA-COM-SURYO  (IDX)    >   1   )  AND
                   IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "058"
                                                       OR  "001"
                                                       OR  "002"
                                                       OR  "099")
                       IF      SPA-COM-INPUTCD(IDX)(1:7)   =   "0992000"
      *H30.4
                                                           OR  "0992081"
                           CONTINUE
                       ELSE
                           MOVE    ZERO          TO  SPA-COM-SURYO(IDX)
                           MOVE    SPACE         TO  SPA-COM-SURFLG(IDX)
                       END-IF
                   END-IF
      *!!
                   IF      SPA-COM-SURYO  (IDX)    =   ZERO
                       MOVE    1                   TO  SPA-COM-SURYO
                                                                 (IDX)
                   END-IF
                   MOVE    SPA-COM-SURYO  (IDX)    TO  WKSRY-SRYSURYO
                                                                 (IDY)
      *            回数
                   IF      SPA-COM-DATAKBN (IDX)   =   "3"
      *                画像診断のみ
                    AND   (SPA-COM-SRYKBN  (IDX)   =   "70" )
                       MOVE    SPA-COM-BUNGSU(IDX) TO  WKSRY-SRYKAISU
                                                                 (IDY)
                   END-IF
      *!!
      *            外用の個別回数を設定
                   IF      SPA-COM-SRYKBN  (IDX)   =   "23"
                       MOVE    SPA-COM-KAISU2 (IDX) TO  WKSRY-SRYKAISU
                                                                 (IDY)
                   END-IF
      *!!
      *            自動算定区分
                   MOVE    SPA-COM-AUTOKBN (IDX)    TO  WKSRY-AUTOKBN
                                                                 (IDY)
      *            訂正で前回がある時
                   IF     (SPA-COM-AUTOKBN (IDX)    =   SPACE )  AND
                          (SPA-COM-MAE-AUTOKBN(IDX) =   "4"
                                                    OR  "5"
                                                    OR  "6"   )
                      MOVE    SPA-COM-MAE-AUTOKBN(IDX)
                                                   TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *            数量の入力がある時
                   IF     (SPA-COM-AUTOKBN (IDX)    =   SPACE )  AND
                          (SPA-COM-SURFLG  (IDX)    =   "1"   )
                       MOVE    "S"                  TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *!!
      *            器材の商品名入力がある時
                   IF     (SPA-COM-INPUTCD (IDX)(1:1)   =   "7"   )
                     AND  (SPA-COM-AUTOKBN2(IDX)    NOT =   SPACE )
                       MOVE    "T"                  TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *!!
      *            時間外区分入力がある時、英字変換後セット
                   IF      SPA-COM-AUTOKBN (IDX)    =   SPACE
                       PERFORM 20021-JIKANGAI-SET-SEC
                   END-IF
      *
      *            自費金額
                   IF      SPA-COM-SRYKBN (IDX)     =   "96"  OR  "95"
                       MOVE    SPA-COM-JIHIMONEY (IDX)
                                                    TO  WKSRY-JIHIMONEY
                                                                 (IDY)
                   END-IF
      *            自賠責金額
                   IF     (SPA-COM-SRYSYUKBN (IDX)  =   "809" )
                    OR    (SPA-COM-JIHIMONEY (IDX)  >   ZERO  )
                       MOVE    SPA-COM-JIHIMONEY (IDX)
                                                    TO  WKSRY-JIHIMONEY
                                                                 (IDY)
                   END-IF
      *            入力コメント番号
                   IF     (SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                     OR  "84" )
      *R02.3
                      OR  (SPA-COM-INPUTCD(IDX)(1:2) =   "85")
                      OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0083"
                                                     OR  "0084")
                      OR  (SPA-COM-INPUTCD(IDX)      =   "810000001"
                                                     OR  "008500000"
                                                     OR  "008600000" )
                      OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0085" AND
                           SPA-COM-MEIFLG (IDX)      =   "1"   )
                      OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0086" AND
                           SPA-COM-MEIFLG (IDX)      =   "1"   )
      *               用法コメント
                      OR  (SPA-COM-INPUTCD(IDX)(1:3) =   "001"  AND
                           SPA-COM-YCOMKBN (IDX)      =   "1"   )
      *H30.4
      *               分割調剤など
                      OR  (SPA-COM-INPUTCD(IDX)(1:7)  =   "0992081" )
      *
                       ADD     1                   TO  WRK-MEINUM
                       MOVE    WRK-MEINUM          TO  WKSRY-INPUTNUM
                                                                 (IDY)
      *
                       MOVE    SPA-COM-ATAI1(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 1)
                       MOVE    SPA-COM-ATAI2(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 2)
      *                入力値
                       MOVE    SPA-COM-ATAI3(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 3)
      *                入力値
                       MOVE    SPA-COM-ATAI4(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 4)
      *                入力値
                       MOVE    SPA-COM-ATAI5(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 5)
      *                入力コメント
                       IF      SPA-COM-MEIFLG (IDX)    =   "1"
                           IF   (SPA-COM-INPUTCD(IDX)  =   "810000001"
                                                       OR  "008500000"
                                                       OR  "008600000" )
                               MOVE    SPA-GMN-MEISYO-G (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           ELSE
                               IF  (SPA-COM-INPUTCD(IDX)(1:4) =  "0085"
                                                              OR "0086")
                                AND (SPA-GMN-MEISYO (IDX)      =
                                                    SPA-COM-NAME (IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    SPA-GMN-MEISYO (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                               END-IF
                           END-IF
                       ELSE
                           IF     (SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                             OR  "84")
      *R02.3
                              OR  (SPA-COM-INPUTCD(IDX)(1:2) =   "85")
                              OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0083"
                                                             OR  "0084")
      *H30.4
      *                     分割調剤など
                              OR  (SPA-COM-INPUTCD(IDX)(1:7)
                                                       =   "0992081")
                               MOVE    SPA-GMN-MEISYO(IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
      *                    用法コメントは【】なしを編集
                           IF     (SPA-COM-INPUTCD(IDX)(1:3) =   "001" )
                               MOVE    SPA-COM-NAME  (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
                       END-IF
                   END-IF
      *ver.4.5
      *            継続コメント
                   IF      SPA-COM-INPUTCONT (IDX)     =   "1"
                       MOVE    "1"             TO  WKSRY-INPUTKBN(IDY)
                   END-IF
      *            換算値数量入力
                   IF      SPA-COM-KANSURYO (IDX)  NOT =   ZERO
                       MOVE    SPA-COM-KANSURYO (IDX)
                                               TO  WKSRY-KANSURYO(IDY)
      *                外用薬は＊回数を登録
                       IF      SPA-COM-SRYKBN  (IDX)   =   "23"
                           COMPUTE  WKSRY-KANSURYO(IDY)
                                               =   SPA-COM-KANSURYO(IDX)
                                               *   WKSRY-SRYKAISU (IDY) 
                       END-IF
                   END-IF
      *            内服１種類区分
                   IF      SPA-COM-INPUTNAIF (IDX)     =   "1"
                       MOVE    "2"             TO  WKSRY-INPUTKBN(IDY)
                   END-IF
      *!!!!
      *APIV3
      *            APIの時、一般名記載分編集
                   IF     (FLG-SYORI           =   2  )
      ********       AND  (SPA-TERMID(1:4)     =   "API+")
                     AND  (SPA-API-FLG         =   1  )
                     AND  (SPA-COM-INPUTCD (IDX)(1:1) =   "6" )
                     AND  (SPA-COM-MEI-KBN(IDX)    =   "1"
                                                   OR  "3"   )
                       PERFORM 2002-API-GENEHENSYU-SEC
                   END-IF
      *H29.5
      *            加算自動算定なし
                   IF     (SPA-API-FLG         =   1  )
                     AND  (SPA-COM-INPUTCD (IDX)(1:1)
                                                   =   "1"     )
                     AND  (SPA-COM-KSNAUTO(IDX)    =   "1"     )
                       MOVE    "Yes"       TO  WKSRY-INPUTCOMENT (IDY)
                   END-IF
      *APIV3
      *!!!!
      *
                   MOVE    SPA-COM-KAISU  (IDX)   TO  WRK-KAISUKEI
      *            画面の計を集計する（１剤で１件）
                   IF      SPA-COM-ZAIEND (IDX)       =   "1"
                       MOVE    SPA-COM-TENSU  (IDX)   TO  WRK-TENSUKEI
      *                各計を編集する
                       MOVE    SPA-COM-SYUTEN1(IDX)   TO  WRK-SYUTEN1
      *
                       IF     (SPA-COM-SYUTEN2 (IDX)  >   ZERO )
                        AND   (WRK-SYUTEN2            =   ZERO )
      *H25.11           画像診断のみ
                        AND   (SPA-COM-SRYKBN  (IDX)  =   "70"  )
                           MOVE    SPA-COM-SYUTEN2 (IDX)
                                                      TO  WRK-SYUTEN2
                       END-IF
                       MOVE    SPA-COM-YKZTEN  (IDX)  TO  WRK-YKZTEN
                       MOVE    SPA-COM-KIZAITEN(IDX)  TO  WRK-KIZAITEN
                   END-IF
      *            労災読み替え点数編集
                   IF     (SPA-HKNKBN          =   1   )
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *              診療種別
                     AND  (SPA-COM-SRYSYUKBN (IDX)
                                        NOT =   "409" )
                       IF     (SPA-COM-SYUTEN2 (IDX)   >   ZERO  )
                       AND    (SPA-COM-RSIGIRI (IDX)   =   1     )
                           MOVE    SPA-COM-SYUTEN2 (IDX)
                                                   TO  WRK-SYUTEN2
      *H25.3               労災読み替え手技
                           IF      SPA-SRYYMD      >=  "20130701"
                               MOVE    "R"         TO  WKSRY-INPUTKBN
                                                               (IDY)
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
                       ADD      SPA-COM-JIHIMONEY (IDX)
                                                  TO  WRK-KINGAK
                   END-IF
      *
                   IF     (IDX                     <   SPA-GMN-MAX) AND
                          (SPA-GMN-INPUTCD (IDX + 1)(1:1)
                                                   =   "."    )
                       MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
                   END-IF
                   IF     (SPA-COM-ZAIEND (IDX)    =   "1"   )  OR
                          (IDY                     =   5     )
                       IF      FLG-SYORI           =   1
                           PERFORM 20021-WKSRYACT-OUT-SEC
                       ELSE
                           PERFORM 30021-WKSRYPARA-OUT-SEC
                       END-IF
      *                労災読み替え点数クリア
                       IF     (SPA-COM-ZAIEND (IDX)    =   "1"   )
                           MOVE    ZERO                TO  WRK-SYUTEN2
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDY                 >   ZERO
               MOVE    SPA-GMN-MAX             TO  IDX
               IF      FLG-SYORI           =   1
                   PERFORM 20021-WKSRYACT-OUT-SEC
               ELSE
                   PERFORM 30021-WKSRYPARA-OUT-SEC
               END-IF
           END-IF
      *
           .
       2002-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為ＤＢ出力処理
      *****************************************************************
       20021-WKSRYACT-OUT-SEC              SECTION.
      *
           MOVE    SPA-COM-SRYKBN  (IDX)   TO  WRK-SRYKBN
           MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
           MOVE    SPA-COM-HKNCOMBI(IDX)   TO  WRK-HKNCOMBI
      *
           ADD     1                   TO  WRK-RENNUM
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    WRK-ZAINUM          TO  WKSRY-ZAINUM 
           MOVE    WRK-RENNUM          TO  WKSRY-RENNUM 
           MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEYTOTAL
      *
           MOVE    WRK-TENSUKEI        TO  WKSRY-ZAITENKEI
           MOVE    WRK-KAISUKEI        TO  WKSRY-ZAIKAIKEI
      *    各計
           MOVE    WRK-SYUTEN1         TO  WKSRY-SYUTEN1
           MOVE    WRK-SYUTEN2         TO  WKSRY-SYUTEN2
           MOVE    WRK-YKZTEN          TO  WKSRY-YKZTEN
           MOVE    WRK-KIZAITEN        TO  WKSRY-KIZAITEN
      *
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *    同時診療伝票番号
           MOVE    SPA-DOUJI-DENPNUM   TO  WKSRY-DOUJI-DENPNUM
      *    同時診療連番
           MOVE    SPA-DOUJI-RENNUM    TO  WKSRY-DOUJI-RENNUM
      *    継続区分
           MOVE    SPA-CONTKBN         TO  WKSRY-CONTKBN
      *ver.4.7
      *    同日再入院分
           MOVE    SPACE               TO  WKSRY-DOUJITSUKBN
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-K02N-DOUKBN     TO  WKSRY-DOUJITSUKBN
           END-IF
      *@
      *    包括剤は継続区分に'H'を編集
           IF      SPA-COM-HOUKATU-ZAI(IDX) =   1
               MOVE    "H"              TO  WKSRY-CONTKBN
           END-IF
      *    薬評・器評
           IF      SPA-COM-YAKUHYO (IDX)   =   "1"
               MOVE    1               TO  WKSRY-ZAIKBN
           END-IF
      *    減点
           IF      SPA-COM-GENTEN  (IDX)   =   "1"
               MOVE    2               TO  WKSRY-ZAIKBN
           END-IF
      *
      *H28.9
      *    持参薬
           IF      SPA-COM-JISANYAKU(IDX)  =   "1"
               MOVE    3               TO  WKSRY-ZAIKBN
           END-IF
           IF      SPA-COM-JISANYAKU(IDX)  =   "2"
               MOVE    4               TO  WKSRY-ZAIKBN
           END-IF
      *
      *@
      *    ドクター
           MOVE    SPA-DRCD            TO  WKSRY-DRCD
      *    電子カルテフラグ
           MOVE    1                   TO  WKSRY-KARTE-FLG
      *
           MOVE    SMCNDATE-YMD        TO  WKSRY-CREYMD
           MOVE    SMCNDATE-YMD        TO  WKSRY-UPYMD
           MOVE    SMCNDATE-HMS        TO  WKSRY-UPHMS
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    "1093"              TO  SPA-ERRCD
               DISPLAY "K02 WKSRYACT INS ERR:"  MCP-RC
           END-IF
      *
           MOVE    ZERO                TO  IDY
           INITIALIZE                  WKSRYACT-REC
      *
           IF      SPA-COM-ZAIEND(IDX) =   "1"
               ADD     1                   TO  WRK-ZAINUM
               INITIALIZE                  WRK-WKSRY-AREA
           END-IF
           .
       20021-WKSRYACT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    時間外区分 編集処理
      *****************************************************************
       20021-JIKANGAI-SET-SEC            SECTION.
      *
           IF     (SPA-GMN-INPUTCD(IDX)(1:1)       NUMERIC   ) AND
                  (SPA-GMN-INPUTCD(IDX)(2:1)       =   SPACE ) AND
                  (SPA-GMN-INPUTCD(IDX)(3:1)   NOT =   SPACE )
               PERFORM 20021-JIKANGAI-CHK-SEC
      ***      EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
      *            WHEN    "1"
      *                MOVE    "A"      TO  WKSRY-AUTOKBN  (IDY)
      *            WHEN    "2"
      *                MOVE    "B"      TO  WKSRY-AUTOKBN  (IDY)
      *            WHEN    "3"
      *                MOVE    "C"      TO  WKSRY-AUTOKBN  (IDY)
      *            WHEN    "4"
      *                MOVE    "D"      TO  WKSRY-AUTOKBN  (IDY)
      *            WHEN    "5"
      *                MOVE    "E"      TO  WKSRY-AUTOKBN  (IDY)
      *            WHEN    "6"
      *                MOVE    "F"      TO  WKSRY-AUTOKBN  (IDY)
      *            WHEN    "7"
      *                MOVE    "G"      TO  WKSRY-AUTOKBN  (IDY)
      ****     END-EVALUATE
      *
           ELSE
               IF     (SPA-NYUGAIKBN           =   "1" ) AND
                      (SPA-COM-DATAKBN (IDX)   =   "1" ) AND
      *               時間加算区分
                      (SPA-COM-TIMEKSNKBN(IDX) NOT =   ZERO )
                   EVALUATE    SPA-COM-TIMEFLG (IDX)
                       WHEN    1
                           MOVE    "A"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    2
                           MOVE    "B"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    3
                           MOVE    "C"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    4
                           MOVE    "D"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    5
                           MOVE    "E"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    6
                           MOVE    "F"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    7
                           MOVE    "G"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    8
                           MOVE    "H"      TO  WKSRY-AUTOKBN  (IDY)
                   END-EVALUATE
               END-IF
           END-IF
           .
       20021-JIKANGAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     時間外区分チェック処理
      *****************************************************************
       20021-JIKANGAI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TIME
           PERFORM VARYING     IDT     FROM    IDX   BY  1
                   UNTIL      (IDT     >   SPA-GMN-MAX )  OR
                              (FLG-TIME    =   1       )
      *        自動発生の時間外加算
               IF     (SPA-COM-DATAKBN(IDT)    =   "2" )  AND
                      (SPA-COM-AUTOKBN(IDT)    =   "2" )
      *********  AND  (SPA-COM-TIMEKSNKBN(IDT) NOT =   ZERO )
                   MOVE    1               TO  FLG-TIME
               END-IF
               IF      SPA-COM-ZAIEND (IDT)    =   "1"
                   MOVE    SPA-GMN-MAX         TO  IDT
               END-IF
           END-PERFORM
      *
           IF      FLG-TIME             =   1
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "A"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "2"
                       MOVE    "B"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "3"
                       MOVE    "C"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "4"
                       MOVE    "D"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "5"
                       MOVE    "E"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "6"
                       MOVE    "F"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "7"
                       MOVE    "G"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "8"
                       MOVE    "H"      TO  WKSRY-AUTOKBN  (IDY)
               END-EVALUATE
           END-IF
           .
       20021-JIKANGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタ登録（登録時）処理
      *****************************************************************
       300-TOUROKU-PARA-SEC         SECTION.
      *
      *    一時データ削除
           PERFORM 3009-PARADELETE-SEC
      *
      *    ＳＰＡ保存パラメタを作成
           PERFORM 3001-PARA-SYORI-SEC
      *
           INITIALIZE                  TBL-K02SCR-AREA
      *
      *    負担金算定・収納マスタ処理 
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 30021-SYUNOU-NYU-SYORI-SEC
           ELSE
               PERFORM 30022-SYUNOU-GAI-SYORI-SEC
           END-IF
      *    診療行為マスタのパラメタを作成する
           MOVE    ZERO                TO  WRK-PARAEDABAN
           MOVE    2                   TO  FLG-SYORI
           PERFORM 2002-WKSRYACT-SYORI-SEC
      *
      *    自動作成の診療行為作成処理
           PERFORM 3004-JIDOU-SAKU-SEC
      *
      *    ＳＰＡ保存パラメタを作成
           PERFORM 3001-PARAK02-SYORI-SEC
      *
           .
       300-TOUROKU-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタ登録（他画面遷移）処理
      *****************************************************************
       400-SENI-PARA-SEC         SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    OPEN    OUTPUT              PARA-FILE
      *    IF      STS-PARA        NOT =  ZERO
      *        DISPLAY "ORCSCWKSRY STS-PARA OPEN:"  STS-PARA  "]"
      **   END-IF
      *!!
      *    一時データ削除
           PERFORM 3009-PARADELETE-SEC
      *
      *    ＳＰＡ保存パラメタを作成
           PERFORM 3001-PARA-SYORI-SEC
      *
      *    診療行為マスタのパラメタを作成する
           MOVE    ZERO                TO  WRK-PARAEDABAN
           MOVE    2                   TO  FLG-SYORI
           PERFORM 2002-WKSRYACT-SYORI-SEC
      *
      **** CLOSE                       PARA-FILE
      *    ＳＰＡ保存パラメタを作成
           PERFORM 3001-PARAK02-SYORI-SEC
      *
           .
       400-SENI-PARA-EXT.
           EXIT.
      *
      *!!!!
      *APIV3
      *****************************************************************
      *    API 般名記載分編集処理
      *****************************************************************
       2002-API-GENEHENSYU-SEC              SECTION.
      *
      *    入力コメントに一般名区分編集
           IF      SPA-COM-MEI-KBN(IDX)    =   "1"
               MOVE    "API+1"         TO  WKSRY-INPUTCOMENT (IDY)
           END-IF
           IF      SPA-COM-MEI-KBN(IDX)    =   "3"
               MOVE    "API+3"         TO  WKSRY-INPUTCOMENT (IDY)
           END-IF
      *
           .
       2002-API-GENEHENSYU-EXT.
           EXIT.
      *APIV3
      *!!!!
      *
      *****************************************************************
      *    診療行為パラメタ出力処理
      *****************************************************************
       30021-WKSRYPARA-OUT-SEC              SECTION.
      *
           MOVE    SPA-COM-SRYSYUKBN (IDX)
                                           TO  WRK-SRYSYUKBN
           MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
           MOVE    SPA-COM-HKNCOMBI(IDX)   TO  WRK-HKNCOMBI
           IF      SPA-COM-HKNCOMBI(IDX)   =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           END-IF
           MOVE    SPA-COM-SRYKA   (IDX)   TO  WRK-SRYKA
           IF      SPA-COM-SRYKA   (IDX)   =   SPACE
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           END-IF
      *    各附加情報（１件目に附加する）
           IF      WRK-RENNUM          =   ZERO
               PERFORM 30022-WKSRYACTPLUS-SEC
           END-IF
      *
           MOVE    SPA-COM-HOUKATU-ZAI (IDX)   TO  WRK-HOUKATU-ZAI
           MOVE    SPA-COM-YAKUHYO     (IDX)   TO  WRK-YAKUHYO
           MOVE    SPA-COM-GENTEN      (IDX)   TO  WRK-GENTEN
      *H28.9   持参薬
           MOVE    SPA-COM-JISANYAKU  (IDX)   TO  WRK-JISANYAKU
           PERFORM 300211-WKSRYPARA-WRITE-SEC
      *
           IF      SPA-COM-ZAIEND(IDX) =   "1"
               ADD     1                   TO  WRK-ZAINUM
               MOVE    ZERO                TO  IDY
               INITIALIZE                  WRK-WKSRY-AREA
           END-IF
           .
       30021-WKSRYPARA-OUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡをパラメタファイルへ書込処理
      *****************************************************************
       300211-WKSRYPARA-WRITE-SEC          SECTION.
      *
           ADD     1                   TO  WRK-RENNUM
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM 
           MOVE    SPA-PTID            TO  WKSRY-PTID 
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN 
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA 
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD 
           MOVE    WRK-ZAINUM          TO  WKSRY-ZAINUM 
           MOVE    WRK-RENNUM          TO  WKSRY-RENNUM 
           MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEYTOTAL
      *    診療会計マスタ作成の為、剤点数計、回数を保存する
           MOVE    WRK-TENSUKEI        TO  WKSRY-ZAITENKEI
           MOVE    WRK-KAISUKEI        TO  WKSRY-ZAIKAIKEI
      *    各計
           MOVE    WRK-SYUTEN1         TO  WKSRY-SYUTEN1
           MOVE    WRK-SYUTEN2         TO  WKSRY-SYUTEN2
           MOVE    WRK-YKZTEN          TO  WKSRY-YKZTEN
           MOVE    WRK-KIZAITEN        TO  WKSRY-KIZAITEN
      *
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
      *    同時診療伝票番号
           MOVE    SPA-DOUJI-DENPNUM   TO  WKSRY-DOUJI-DENPNUM
      *    同時診療連番
           MOVE    SPA-DOUJI-RENNUM    TO  WKSRY-DOUJI-RENNUM
      *    継続区分
           MOVE    SPA-CONTKBN         TO  WKSRY-CONTKBN
      *ver.4.7
      *    同日再入院分
           MOVE    SPACE               TO  WKSRY-DOUJITSUKBN
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-K02N-DOUKBN     TO  WKSRY-DOUJITSUKBN
           END-IF
      *@
      *    包括剤は継続区分に'H'を編集
           IF      WRK-HOUKATU-ZAI      =   1
               MOVE    "H"              TO  WKSRY-CONTKBN
           END-IF
      *    薬評・器評
           IF      WRK-YAKUHYO         =   "1"
               MOVE    1               TO  WKSRY-ZAIKBN
           END-IF
      *    減点
           IF      WRK-GENTEN          =   "1"
               MOVE    2               TO  WKSRY-ZAIKBN
           END-IF
      *H28.9
      *    持参薬
           IF      WRK-JISANYAKU       =   "1"
               MOVE    3               TO  WKSRY-ZAIKBN
           END-IF
           IF      WRK-JISANYAKU       =   "2"
               MOVE    4               TO  WKSRY-ZAIKBN
           END-IF
      *@
      *    ドクター
           MOVE    SPA-DRCD            TO  WKSRY-DRCD
      *
      ***  MOVE    SPACE               TO  PARAF-REC
      *    MOVE    "WKSRYACT"          TO  PARAF-FILEMEI
      **** MOVE    WRK-PARAEDABAN      TO  PARAF-EDANUM
      *    ＤＢからの作成でない
           MOVE    ALL "1"             TO  WKSRY-TERMID
      *
           MOVE    WKSRYACT-REC        TO  PARA-WKSRYACT-REC
           MOVE    WKSRYACTPLUS-REC    TO  PARA-WKSRYACTPLUS-REC
      *    MOVE    SPACE               TO  PARAF-DATA-REC
      **   MOVE    PARA-WKSRYACT-REC   TO  PARAF-DATA-REC
      *
      *    WRITE   PARAF-REC
      *    IF      STS-PARA        NOT =   ZERO
      *        DISPLAY "ORCSCWKSRY PARA WRITE ERR:"   STS-PARA    "."
      *    END-IF
      *
           ADD     1                   TO  WRK-PARAEDABAN
      *
      *    TBL_PARAに保存
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    WRK-PARAEDABAN      TO  PARA-EDANUM
           MOVE    PARA-WKSRYACT-REC   TO  PARA-DATA-REC
      *
           PERFORM 30010-PARA-DBINSERT-SEC
      *
           MOVE    ZERO                TO  IDY
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPACE               TO  WKSRYACTPLUS-REC
           INITIALIZE                  WKSRYACTPLUS-REC
      *
           .
       300211-WKSRYPARA-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ出力処理
      *****************************************************************
       30010-PARA-DBINSERT-SEC          SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
               DISPLAY "K02 PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
           .
       30010-PARA-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       30022-WKSRYACTPLUS-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDW
           MOVE    ZERO                TO  IDW-END
           MOVE    ZERO                TO  IDW2-H
           MOVE    SPACE               TO  WKSRYACTPLUS-REC
           INITIALIZE                  WKSRYACTPLUS-REC
      *
           MOVE    ZERO                TO  FLG-TEIGEN30-ZAI
      *
           PERFORM VARYING     IDW2    FROM    IDW-STR   BY  1
                   UNTIL       IDW2    >   SPA-GMN-MAX
      *H27.4
      *        ３０日減対象判定
               IF     (SPA-COM-SRYKBN(IDW2)    =   "21"
                                               OR  "22" )
                 AND  (SPA-COM-INPUTCD(IDW2)(1:1)  =   "6" )
                 AND  (SPA-COM-YOBI2 (IDW2)(1:1)   =   SPACE)
                   MOVE    1                   TO  FLG-TEIGEN30-ZAI
               END-IF
      *
               IF      SPA-COM-ZAIEND(IDW2)    =   "1"
                   MOVE    IDW2                TO  IDW-END
                   MOVE    SPA-GMN-MAX         TO  IDW2
               END-IF
           END-PERFORM
           IF      IDW-END             =   ZERO
               MOVE    SPA-GMN-MAX     TO  IDW-END
           END-IF
           PERFORM VARYING     IDW2    FROM    IDW-STR   BY  1
                   UNTIL       IDW2    >   IDW-END
               PERFORM 30022-WKSRYACTPLUS-HEN-SEC
           END-PERFORM
      *!!
      *    労災円・点数の混在
           IF     (WKSRYP-RSIKINGAKU   NOT =   ZERO )
             AND  (WKSRYP-RSI-TENSU    NOT =   ZERO )
               IF      IDW                 =   ZERO
      *            明細がない時、１件目に点数のみ記載
                   MOVE    1               TO  IDW
                   MOVE    SPA-COM-INPUTCD(IDW2-H)
                                               TO  WKSRYP-SRYCD (IDW)
                   MOVE    WKSRYP-RSI-TENSU    TO  WKSRYP-SYUTEN (IDW)
               END-IF
           END-IF
           .
       30022-WKSRYACTPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       30022-WKSRYACTPLUS-HEN-SEC          SECTION.
      *
      *    労災の円
           IF      SPA-HKNKBN          =   1
               IF      SPA-COM-RSIKIN(IDW2) >   ZERO
                   MOVE    SPA-COM-RSIKIN(IDW2)    TO  WKSRYP-RSIKINGAKU
                   MOVE    "1"              TO  WKSRYP-ADDKBN
               END-IF
      *!!!!!!
      *H27.7
      *        剤の収納集計用点数
               IF      TBL-SYU-TENSU (IDW2)    >   ZERO
                   ADD     TBL-SYU-TENSU (IDW2)    TO  WKSRYP-RSI-TENSU
                   IF      IDW2-H              =   ZERO
                       MOVE    IDW2                TO  IDW2-H
                   END-IF
               END-IF
      *!!!!!!
           ELSE
               MOVE    ZERO                 TO  WKSRYP-RSIKINGAKU
           END-IF
      *    点数区分
           IF     (SPA-COM-TENSIKIBETU(IDW2)   =   7
                                               OR  8  )  AND
                  (SPA-COM-KISOTEN (IDW2)  =   SPA-COM-TENSU(IDW-END))
               MOVE    1                   TO  WKSRYP-PLUSKBN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    手技毎の内容（診察・手術・画像診断・処置）
           IF     (SPA-COM-SYUFLG(IDW2)    =   1  )  AND
                  (SPA-COM-SYUTEN-M(IDW2)  NOT =  SPA-COM-SYUTEN1
                                                       (IDW-END))
               IF     (SPA-HKNKBN              =   1     )
                 AND  (SPA-COM-RSIKIN(IDW2)    >   ZERO  )
      *            労災の円は対象外
                   CONTINUE
               ELSE
      *
               MOVE    "1"                 TO  WKSRYP-ADDKBN
               ADD     1                   TO  IDW
               IF      IDW                 >   5
                   GO      TO      30022-WKSRYACTPLUS-HEN-EXT
               END-IF
      *
      *        開始手技コード
               MOVE    SPA-COM-INPUTCD(IDW2)   TO  WKSRYP-SRYCD (IDW)
      *        手技点数
               MOVE    SPA-COM-SYUTEN-M(IDW2)  TO  WKSRYP-SYUTEN (IDW)
      *        薬剤点数
               MOVE    SPA-COM-YKZTEN-M(IDW2)  TO  WKSRYP-YKZTEN (IDW)
      *        その他器材点数（フィルム・酸素・窒素以外）
               MOVE    SPA-COM-KIZTEN-M(IDW2)  TO  WKSRYP-KIZAITEN (IDW)
      *        フィルム点数
               MOVE    SPA-COM-FIRTEN-M(IDW2)  TO  WKSRYP-FIRTEN (IDW)
      *
               END-IF
           ELSE
      *        フィルム点数のみ
               IF      SPA-COM-FIRTEN-M(IDW2)  NOT =  ZERO
                   ADD     1                   TO  IDW
                   IF      IDW                 >   5
                       GO      TO      30022-WKSRYACTPLUS-HEN-EXT
                   END-IF
      *
                   MOVE    "1"                 TO  WKSRYP-ADDKBN
      *            その他器材点数（フィルム・酸素・窒素以外）
                   MOVE    SPA-COM-KIZTEN-M(IDW2)
                                               TO  WKSRYP-KIZAITEN (IDW)
                   MOVE    SPA-COM-FIRTEN-M(IDW2)
                                               TO  WKSRYP-FIRTEN (IDW)
               END-IF
           END-IF
      *    酸素点数
           IF      SPA-COM-SANSOTEN(IDW2)  NOT =   ZERO
               MOVE    SPA-COM-SANSOTEN(IDW2)   TO  WKSRYP-SANSOTEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    窒素点数
           IF      SPA-COM-CHISOTEN(IDW2)  NOT =   ZERO
               MOVE    SPA-COM-CHISOTEN(IDW2)  TO  WKSRYP-CHISOTEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *ver.4.7
      *    院外処方点数
           IF      SPA-COM-INGAITEN(IDW2)  NOT =   ZERO
               MOVE    SPA-COM-INGAITEN(IDW2)  TO  WKSRYP-INGAITEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    内分泌負荷試験用
           IF     (SPA-COM-SRYKBN(IDW2)    =   "60" )  AND
                  (SPA-COM-DATAKBN(IDW2)   =   "1"   )  AND
                  (SPA-COM-HOUKNSKBN(IDW2) =   08    )
      *        当月算定フラグ
               MOVE    1                   TO  WKSRYP-MSANTEITFLG
      *        当月算定点数区分
               IF      SPA-COM-TEN (IDW2)  =   SPA-COM-KISOTEN(IDW2)
                   MOVE    1               TO  WKSRYP-MSANTEITENKBN
               ELSE
                   MOVE    2               TO  WKSRYP-MSANTEITENKBN
               END-IF
      *        当月算定点数
               MOVE    SPA-COM-KISOTEN(IDW2)   TO WKSRYP-MSANTEITEN
           END-IF
      *
      *    内服７種類逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "21" )  AND
                  (SPA-COM-INPUTCD(IDW2)   =   "820000047")
      *H27.4
            AND   (SPA-NAIFUKU-OK          =   1     )
               MOVE    1                   TO  WKSRYP-TEI-NKBN
           END-IF
      *H26.10
      *    向精神薬多剤投与逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "21"
                                           OR  "22"
                                           OR  "23" )  AND
                  (SPA-COM-INPUTCD(IDW2)   =   CONS-SGEN-SRYCD )
               MOVE    1                   TO  WKSRYP-TEI-NKBN
           END-IF
      *H27.4
      *    ３０日投薬逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "21"
                                           OR  "22" )
             AND  (SPA-COM-INPUTCD(IDW2)   =   CONS-TGEN-SRYCD )
             AND  (SPA-TEIGEN30-OK         =   1    )
             AND  (WRK-KAISUKEI            >=  30   )
      *        対象の剤判定
             AND  (FLG-TEIGEN30-ZAI        =   1    )
               MOVE    1                   TO  WKSRYP-TEI-NKBN30
           END-IF
      *H28.4 
      *    湿布薬７０枚以上逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "23" )  AND
                  (SPA-COM-INPUTCD(IDW2)   =   CONS-SPCM-SRYCD )
               MOVE    1                   TO  WKSRYP-TEI-NKBN30
           END-IF
      *
           .
       30022-WKSRYACTPLUS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡをパラメタファイルへ書込処理
      *****************************************************************
       3001-PARA-SYORI-SEC          SECTION.
      *
      *    共通ＳＰＡをパラメタへ出力
      *    MOVE    SPACE               TO  PARAF-REC
      *    MOVE    "K01PARA"           TO  PARAF-FILEMEI
      *    MOVE    1                   TO  PARAF-EDANUM
      *    MOVE    SPA-K01KYOUTU       TO  PARAF-DATA-REC
      **   WRITE                       PARAF-REC
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "K01KYOUTU"         TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           MOVE    SPA-K01KYOUTU       TO  SPATMP-DATA-REC
           PERFORM 30011-SPATMP-INSERT-SEC
      *
      *
           .
       3001-PARA-SYORI-EXT.
      *
      *****************************************************************
      *    ＳＰＡをパラメタファイルへ書込処理
      *****************************************************************
       3001-PARAK02-SYORI-SEC          SECTION.
      *
      **** MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *
      *    OPEN    OUTPUT              PARAK02-FILE
      *    IF      STS-PARA2        NOT =  ZERO
      *        DISPLAY "ORCSCWKSRY STS-PARA2 OPEN:"  STS-PARA2  "]"
      *    END-IF
      *    Ｋ０２ＳＰＡをパラメタへ出力
      *    MOVE    SPA-K02             TO  PARA-SPA-K02
      *    WRITE                       PARA-SPA-K02
      *
      *****CLOSE                       PARAK02-FILE
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "K02SPA"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           MOVE    SPA-K02             TO  SPATMP-DATA-REC
           PERFORM 30011-SPATMP-INSERT-SEC
      *
           .
       3001-PARAK02-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時テーブル削除処理
      *****************************************************************
       3009-PARADELETE-SEC          SECTION.
      *
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
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       3009-PARADELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ一時保存テーブル追加
      *****************************************************************
       30011-SPATMP-INSERT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K02 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       30011-SPATMP-INSERT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   入院負担金算定・収納マスタ処理
      *****************************************************************
       30021-SYUNOU-NYU-SYORI-SEC          SECTION.
      *
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-DRCD            TO  WRK-DRCD
      *    保険情報
           MOVE    SPA-FTNRATE         TO  WRK-FTNRATE
           MOVE    SPA-HKNNUM          TO  WRK-HKNNUM
           MOVE    SPA-KOH1HKNNUM      TO  WRK-KOH1HKNNUM
           MOVE    SPA-KOH2HKNNUM      TO  WRK-KOH2HKNNUM
           MOVE    SPA-KOH3HKNNUM      TO  WRK-KOH3HKNNUM
           MOVE    SPA-KOH4HKNNUM      TO  WRK-KOH4HKNNUM
      *
           MOVE    ZERO                TO  IDX-F
      *
           MOVE    ZERO                TO  WRK-PARAEDABAN
      *
      *    診療行為より、収納マスタのパラメタを作成する 
      *    MOVE    SPACE               TO  PARAF-REC
      *    MOVE    "SYUNOU"            TO  PARAF-FILEMEI
      *    MOVE    01                  TO  PARAF-EDANUM
      *
           PERFORM 4520101-SYUNOU-HENSYU-SEC
      *
           .
       30021-SYUNOU-NYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   外来負担金算定・収納マスタ処理
      *****************************************************************
       30022-SYUNOU-GAI-SYORI-SEC          SECTION.
      *
      *    保険毎の科をテーブルへ
           PERFORM 45021-HKNCOMBI-SRYKA-SEC
      *
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-DRCD            TO  WRK-DRCD
      *
           MOVE    ZERO                TO  IDX-F
      *
           MOVE    ZERO                TO  WRK-PARAEDABAN
      *
      *    診療行為より、収納マスタのパラメタを作成する 
      *    MOVE    SPACE               TO  PARAF-REC
      *    MOVE    "SYUNOU"            TO  PARAF-FILEMEI
      *    MOVE    01                  TO  PARAF-EDANUM
      *
           PERFORM VARYING     IDH     FROM    1  BY  1
                   UNTIL      (IDH     >   15  )  OR
                              (SPA-TBL-HKNCOMBI(IDH)  =   SPACE )
      *        複数保険収納作成
               PERFORM 45209-HKNCOMBI-SYUNOU-SEC
           END-PERFORM
      *
           .
       30022-SYUNOU-GAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数保険毎の複数科をテーブル編集処理
      *****************************************************************
       45021-HKNCOMBI-SRYKA-SEC             SECTION.
      *
           INITIALIZE                  SPA-TBL-HKNCOMBI-AREA
           MOVE    1                   TO  IDH
      *    主保険・主科
           MOVE    SPA-HKNCOMBINUM     TO  SPA-TBL-HKNCOMBI (IDH)
           MOVE    SPA-GMN-HKNCOMBIMEI TO  SPA-TBL-HKNCOMBIMEI(IDH)
           MOVE    SPA-GMN-FTNRATE     TO  SPA-TBL-FTNRATE   (IDH)
           MOVE    SPA-GMN-FTNRATEMEI  TO  SPA-TBL-FTNRATEMEI(IDH)
           MOVE    SPA-SRYKA           TO  SPA-TBL-SRYKA (IDH 1)
           MOVE    SPA-DRCD            TO  SPA-TBL-DRCD  (IDH 1)
           MOVE    ZERO                TO  FLG-HKN999
           PERFORM VARYING     IDH1    FROM    1   BY  1
                   UNTIL      (IDH1    >   SPA-MAX-LINE )  OR
                             ((SPA-GMN-INPUTCD (IDH1)  =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDH1)  =   SPACE ))
               IF      SPA-GMN-INPUTCD (IDH1)(1:1) =   "#"
                                                   OR  "$"
                   CONTINUE
                   IF      SPA-GMN-INPUTCD (IDH1)(1:1) =   "#"
                       MOVE    IDH1              TO  IDHC
                   END-IF
               ELSE
                 IF      SPA-COM-HKNCOMBI(IDH1)  =   "9999"
                     MOVE    1                   TO  FLG-HKN999
                 ELSE
                   MOVE    SPA-COM-HKNCOMBI(IDH1)  TO  WRK-HKNCOMBI
      *H29.8
                   MOVE    ZERO                TO  FLG-CHK01
                   PERFORM VARYING     IDH     FROM    1   BY  1
                           UNTIL       IDH     >   15
                       IF      SPA-TBL-HKNCOMBI (IDH)  =   SPACE
                           MOVE    WRK-HKNCOMBI    TO  SPA-TBL-HKNCOMBI
                                                                 (IDH)
                       END-IF
                       IF      SPA-TBL-HKNCOMBI (IDH)   =   WRK-HKNCOMBI
                           MOVE    15              TO  IDH
      *H29.8
                           MOVE    1               TO  FLG-CHK01
                       END-IF
                   END-PERFORM
      *H29.8
      *            テーブルオーバーチェック
                   IF      FLG-CHK01            =   ZERO
                       MOVE    "1310"           TO  SPA-ERRCD
                       MOVE    "1"              TO  SPA-COM-CUR (IDHC)
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
      *    包括保険は最後
           IF     (SPA-HKNCOMBINUM NOT =   "9999")  AND
                  (FLG-HKN999          =   1     )
               MOVE    ZERO                TO  FLG-CHK01
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   15
                   IF      SPA-TBL-HKNCOMBI (IDH)  =   SPACE
                       MOVE    "9999"          TO  SPA-TBL-HKNCOMBI
                                                                 (IDH)
                       MOVE    15              TO  IDH
                       MOVE    1               TO  FLG-CHK01
                   END-IF
               END-PERFORM
      *H29.8
      *        テーブルオーバーチェック
               IF      FLG-CHK01            =   ZERO
                   MOVE    "1310"           TO  SPA-ERRCD
                   MOVE    "1"              TO  SPA-COM-CUR (IDHC)
               END-IF
           END-IF
      *
      *    H29.8   SPA-SRYKA,HKNCOMBIは設定済み
           MOVE    1                   TO  WRK-GRPCNT
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   15   )  OR
                              (SPA-TBL-HKNCOMBI(IDH)   =   SPACE )
      *H29.8
                          OR  (SPA-ERRCD   NOT =   SPACE )
               PERFORM VARYING     IDH1    FROM    1   BY  1
                       UNTIL      (IDH1    >   SPA-MAX-LINE  )  OR
                                 ((SPA-GMN-INPUTCD (IDH1)  =   SPACE )
                             AND  (SPA-COM-INPUTCD (IDH1)  =   SPACE ))
      *H29.8
                          OR  (SPA-ERRCD   NOT =   SPACE )
                 IF      SPA-GMN-INPUTCD (IDH1)(1:1) =   "#"
                                                     OR  "$"
                     CONTINUE
                     IF      SPA-GMN-INPUTCD (IDH1)(1:1) =   "$"
                         MOVE    IDH1              TO  IDHC
                     END-IF
                 ELSE
                   IF     (SPA-COM-HKNCOMBI(IDH1)  =
                                               SPA-TBL-HKNCOMBI(IDH))
                       MOVE    SPA-COM-SRYKA(IDH1)
                                               TO  WRK-SRYKA
                       MOVE    SPA-COM-DRCD (IDH1)
                                                   TO  WRK-DRCD
      *H29.8
                       MOVE    ZERO                TO  FLG-CHK01
                       PERFORM VARYING     IDH2    FROM    1   BY  1
                               UNTIL       IDH2    >   10
                           IF      SPA-TBL-SRYKA (IDH  IDH2)   =   SPACE
                               MOVE    WRK-SRYKA  TO  SPA-TBL-SRYKA
                                                             (IDH IDH2)
      *H29.8
                               ADD     1               TO  WRK-GRPCNT
                               IF      WRK-DRCD   NOT =   SPACE
                                   MOVE    "1"        TO  SPA-TBL-DRCD1
                                                             (IDH IDH2)
                                   MOVE    WRK-DRCD   TO  SPA-TBL-DRCD2
                                                             (IDH IDH2)
                               END-IF
                           END-IF
                           IF      SPA-TBL-SRYKA (IDH  IDH2)
                                                       =   WRK-SRYKA
                               MOVE    10              TO  IDH2
                               MOVE    1               TO  FLG-CHK01
                           END-IF
                       END-PERFORM
      *H29.8
                       IF      FLG-CHK01            =   ZERO
                           MOVE    "1311"           TO  SPA-ERRCD
                           MOVE    "1"              TO  SPA-COM-CUR
                                                         (IDHC)
                       END-IF
                   END-IF
                 END-IF
               END-PERFORM
      *
               IF      SPA-TBL-SRYKA(IDH 1)    =   SPACE
                   MOVE    SPA-SRYKA           TO  SPA-TBL-SRYKA(IDH 1)
                   MOVE    SPA-DRCD            TO  SPA-TBL-DRCD (IDH 1)
      *H29.8
                   ADD     1               TO  WRK-GRPCNT
               END-IF
           END-PERFORM
      *
      *H29.8
           IF      SPA-ERRCD           =   SPACE
      *        組合せ最大１３件（合計を除く）
               IF      WRK-GRPCNT      >   13
                   MOVE    "1312"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR
                                                      (IDHC)
               END-IF
           END-IF
           .
       45021-HKNCOMBI-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数保険収納マスタの作成処理
      *****************************************************************
       45209-HKNCOMBI-SYUNOU-SEC             SECTION.
      *
           MOVE    SPA-TBL-HKNCOMBI(IDH)   TO  WRK-HKNCOMBI
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDH2    FROM    1   BY  1
                   UNTIL      (IDH2    >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      WRK-HKNCOMBI        =   SPA-GMN-THKNCOMBINUM
                                                             (IDH2)
                   MOVE    SPA-NAI-TFTNRATE(IDH2)  TO  WRK-FTNRATE
                   MOVE    SPA-NAI-HKNNUM  (IDH2)  TO  WRK-HKNNUM
                   MOVE    SPA-NAI-KOH1HKNNUM(IDH2)
                                                   TO  WRK-KOH1HKNNUM
                   MOVE    SPA-NAI-KOH2HKNNUM(IDH2)
                                                   TO  WRK-KOH2HKNNUM
                   MOVE    SPA-NAI-KOH3HKNNUM(IDH2)
                                                   TO  WRK-KOH3HKNNUM
                   MOVE    SPA-NAI-KOH4HKNNUM(IDH2)
                                                   TO  WRK-KOH4HKNNUM
      *
                   MOVE    SPA-GMN-THKNCOMBIMEI(IDH2)
                                             TO  SPA-TBL-HKNCOMBIMEI
                                                               (IDH)
                   MOVE    SPA-NAI-TFTNRATE (IDH2)
                                             TO  SPA-TBL-FTNRATE
                                                               (IDH)
                   MOVE    SPA-GMN-TFTNRATEMEI(IDH2)
                                             TO  SPA-TBL-FTNRATEMEI
                                                               (IDH)
                   MOVE    1                 TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               CONTINUE
           ELSE
      *        複数科収納作成
               PERFORM 452010-SRYKA-SYUNOU-SEC
           END-IF
      *
           .
        45209-HKNCOMBI-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科収納作成処理
      *****************************************************************
       452010-SRYKA-SYUNOU-SEC             SECTION.
      *
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL      (IDH3    >   10   )  OR
                              (SPA-TBL-SRYKA(IDH IDH3)   =   SPACE )
               MOVE    SPA-TBL-SRYKA (IDH IDH3)    TO  WRK-SRYKA
               IF      SPA-TBL-DRCD2(IDH IDH3)     =   SPACE
                   MOVE    SPA-DRCD        TO  SPA-TBL-DRCD (IDH IDH3)
               END-IF
               MOVE    SPA-TBL-DRCD  (IDH IDH3)    TO  WRK-DRCD
               PERFORM 4520101-SYUNOU-HENSYU-SEC
           END-PERFORM
      *
           .
       452010-SRYKA-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為より、収納マスタの請求額の計算処理
      *****************************************************************
       4520101-SYUNOU-HENSYU-SEC             SECTION.
      *
      *    修正の時、収納テーブルを検索
           IF     (SPA-NYUGAIKBN       =   "2")  AND
                  (SPA-SYORIFLG        =   1 )  AND
                  (WRK-SRYKA       NOT =   ZERO)
               ADD     1               TO  IDX-F
               IF    ((IDX-F           >   1   )  AND
                      (SPA-TEI-DENPNUM(IDX-F)  =   ZERO)) OR
                      (IDX-F           >   15  )
      *            新規
                   MOVE    ZERO                TO  WRK-SYU-DENPNUM
               ELSE
                   IF     (SPA-TEI-DENPNUM(1)  =   ZERO )  OR
                          (IDX-F               =   1    )
                       MOVE    SPA-DENPNUM     TO  WRK-SYU-DENPNUM
                   ELSE
                       MOVE    SPA-TEI-DENPNUM(IDX-F)
                                               TO  WRK-SYU-DENPNUM
                   END-IF
               END-IF
           ELSE
               MOVE    ZERO            TO  WRK-SYU-DENPNUM
           END-IF
           IF      WRK-SYU-DENPNUM NOT =   ZERO
               INITIALIZE                  SYUNOU-REC
               MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
               MOVE    SPA-PTID            TO  SYU-PTID
               MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
               MOVE    WRK-SYU-DENPNUM     TO  SYU-DENPNUM
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYUNOU-REC
                   MOVE    ZERO                TO  FLG-SYUNOU
               ELSE
                   INITIALIZE                  SYUNOU-REC
                   MOVE    1                   TO  FLG-SYUNOU
               END-IF
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           IF      SYU-PTID                =   ZERO
               MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
               MOVE    SPA-PTID            TO  SYU-PTID
               MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
               MOVE    ZERO                TO  SYU-DENPNUM
               MOVE    WRK-SRYKA           TO  SYU-SRYKA
               MOVE    SPA-SRYYMD          TO  SYU-SRYYMD
               MOVE    SPA-SRYYMD          TO  SYU-SKYSTYMD
               MOVE    ALL "9"             TO  SYU-SKYEDYMD
               MOVE    "1"                 TO  SYU-DENPJTIKBN
           ELSE
      *        請求内容クリア
               INITIALIZE                  SYU-SKY-NAIYOU
               INITIALIZE                  SYU-JIHI-NAIYOU
               INITIALIZE                  SYU-JIHI-TOTAL
               INITIALIZE                  SYU-JIHI-TAX
      *        労災保険クリア
               INITIALIZE                  SYU-RSISHOSHIN-MONEY
               INITIALIZE                  SYU-RSISAISHIN-MONEY
               INITIALIZE                  SYU-RSISDO-MONEY
               INITIALIZE                  SYU-RSIETC-MONEY
      *        診療日
               MOVE    SPA-SRYYMD          TO  SYU-SRYYMD
      *        訂正時の科変更
               MOVE    WRK-SRYKA           TO  SYU-SRYKA
               MOVE    ZERO                TO  SYU-GRP-SGKMONEY
           END-IF
      *
           MOVE    WRK-HKNCOMBI        TO  SYU-HKNCOMBINUM
           MOVE    WRK-FTNRATE         TO  SYU-PTFTNRATE
      *    保険情報
           MOVE    WRK-HKNNUM          TO  SYU-SYUHKNNUM
           MOVE    WRK-KOH1HKNNUM      TO  SYU-KOH1HKNNUM
           MOVE    WRK-KOH2HKNNUM      TO  SYU-KOH2HKNNUM
           MOVE    WRK-KOH3HKNNUM      TO  SYU-KOH3HKNNUM
           MOVE    WRK-KOH4HKNNUM      TO  SYU-KOH4HKNNUM
      *    同時診療伝票番号
           MOVE    ZERO                TO  SYU-DOUJI-DENPNUM
      *    院外処方区分
           MOVE    SPA-INGAIKBN        TO  SYU-INGAISHOHOKBN
      *    ドクター
           MOVE    WRK-DRCD            TO  SYU-DRCD-G
      *    訂正の時
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-DENPNUM     TO  SYU-GRP-DENPNUM
           END-IF
      *    収納点数集計
           INITIALIZE                  WRK-SUM-AREA
           MOVE    ZERO                TO  FLG-RSITEN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "." 
                                                   OR  "#" 
                                                   OR  "$" 
                   CONTINUE
               ELSE
                   IF   ( SPA-NYUGAIKBN            =   "1"    )   OR
                        ((SPA-COM-HKNCOMBI (IDX)   =   WRK-HKNCOMBI) AND
                         ((WRK-SRYKA               =   ZERO   )  OR
                          (SPA-COM-SRYKA    (IDX)  =   WRK-SRYKA  )))
                       PERFORM 4501-SEIKYU-KEI-SEC
      *                剤終了時クリア
                       IF      SPA-COM-ZAIEND (IDX)    =   "1"
                           MOVE    ZERO                TO  FLG-RSITEN
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    自動計算処理
           PERFORM 4504-JIDOUKEI-SEC
      *    各合計計算処理
           PERFORM 4506-TOTALKEI-SEC
      *
      *R02.1
           IF     (SPA-SYORIFLG        =   1 )
              AND (SPA-GMN-MAX         =   ZERO)
      *        削除の時、調整金のクリア
               MOVE    ZERO            TO  SYU-CHOSEI
               MOVE    ZERO            TO  SYU-CHOSEI1
               MOVE    ZERO            TO  SYU-CHOSEI2
           END-IF
      *
      *
           ADD     1                   TO  WRK-PARAEDABAN
      *    TBL_PARAに保存
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
           MOVE    WRK-PARAEDABAN      TO  PARA-EDANUM
           MOVE    SYUNOU-REC          TO  PARA-DATA-REC
      *
           PERFORM 30010-PARA-DBINSERT-SEC
      *
           .
       4520101-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為より、収納マスタの請求額の計算処理
      *****************************************************************
       4501-SEIKYU-KEI-SEC             SECTION.
      *
      *    包括剤は対象外
           IF     (SPA-COM-HOUKATU-ZAI (IDX)   =   1 )  OR
                  (SPA-COM-HKNCOMBI    (IDX)   =   "9999")
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *   自費の集計（診療コードが'09500'で始まるもの）
          IF     (SPA-COM-INPUTCD(IDX)(1:5)   =   "09500"  OR
                                                  "09600" )  OR
                ((SPA-COM-INPUTCD(IDX)(1:5)   =   "09591"  OR
                                                  "09592"  OR
                                                  "09593"  OR
                                                  "09594" )  AND
                 (SPA-COM-SRYKBN (IDX)    NOT =   "80"   ))
      *          '09691'の追加
            OR   (SPA-COM-INPUTCD(IDX)(1:5)   =   "09691"  OR
                                                  "09692"  OR
                                                  "09693"  OR
                                                  "09694" )
               PERFORM 45010-JIHI-SET-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *   保険適用外の集計（診療コードが'095XX'で始まるもの）または、
      *                     自費の診療種別のもの
          IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "095"  OR  "096")  OR
                 (SPA-COM-SRYSYUKBN (IDX)     =   "950"  OR  "960")
      *H26.10
            OR   (SPA-COM-SRYSYUKBN (IDX)     =   "961"           )
      *
               PERFORM 45011-TGMONEY-SET-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *    労災保険は円と点数に分ける
           IF      SPA-HKNKBN          =   1
               PERFORM 45011-RSI-SYUNOU-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *    保険適用分は剤の最後の計を集計
           IF      SPA-COM-ZAIEND (IDX)    NOT =   "1"
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
           EVALUATE    SPA-COM-SRYKBN  (IDX)
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (14)
      *        その他
               WHEN    "80"
                   PERFORM 45011-HKNTEN-SONOTA-SEC
           END-EVALUATE
      *
           .
       4501-SEIKYU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他個別集計処理
      *****************************************************************
       45011-HKNTEN-SONOTA-SEC               SECTION.
      *
           EVALUATE    SPA-COM-SRYSYUKBN  (IDX)
      *        リハビリ
               WHEN    "800"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (11)
      *        処方せん料
               WHEN    "820"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (04)
      *        処方せん料再掲
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-SHOHOU-SAI
      *        精神科
               WHEN    "830"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (12)
      *        放射線治療
               WHEN    "840"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (13)
      *        療養担当手当て
               WHEN    "850"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (16)
      *        入院料
               WHEN    "890"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (15)
      *        その他
               WHEN    OTHER
                  ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (11)
           END-EVALUATE
           .
       45011-HKNTEN-SONOTA-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費セット集計処理
      *****************************************************************
       45010-JIHI-SET-SEC               SECTION.
      *
           IF      SPA-NYUGAIKBN          =   "1"
               MOVE    SPA-COM-NYUTENTTLKBN(IDX)  TO  WRK-TENTTLKBN
           ELSE
               MOVE    SPA-COM-GAITENTTLKBN(IDX)  TO  WRK-TENTTLKBN
           END-IF
      *
      *    添え字エラー
           IF      WRK-TENTTLKBN       =   ZERO
               GO  TO  45010-JIHI-SET-EXT
           END-IF
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
      *H30.5   桁数チェック
               IF     (SPA-COM-KEI (IDX) + SYU-JIHI-TAXNASI
                                             (WRK-TENTTLKBN))
                                         >   9999999
                   MOVE    "1089"        TO  SPA-ERRCD
                   MOVE    "1"           TO  SPA-COM-CUR (IDX)
               END-IF
      *
               EVALUATE    WRK-TENTTLKBN
                   WHEN    1
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-1
                   WHEN    2
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-2
                   WHEN    3
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-3
                   WHEN    4
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-4
                   WHEN    5
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-5
                   WHEN    6
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-6
                   WHEN    7
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-7
                   WHEN    8
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-8
                   WHEN    9
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-9
                   WHEN    10
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-10
               END-EVALUATE
      *        消費税あり
               WHEN    "960"
      *H26.10      追加
               WHEN    "961"
      *
      *H30.5   桁数チェック
               IF     (SPA-COM-KEI (IDX) + SYU-JIHI-TAXARI
                                             (WRK-TENTTLKBN))
                                         >   9999999
                   MOVE    "1089"        TO  SPA-ERRCD
                   MOVE    "1"           TO  SPA-COM-CUR(IDX)
               END-IF
      *
               EVALUATE    WRK-TENTTLKBN
                   WHEN    1
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-1-TAX
                   WHEN    2
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-2-TAX
                   WHEN    3
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-3-TAX
                   WHEN    4
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-4-TAX
                   WHEN    5
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-5-TAX
                   WHEN    6
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-6-TAX
                   WHEN    7
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-7-TAX
                   WHEN    8
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-8-TAX
                   WHEN    9
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-9-TAX
                   WHEN    10
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-10-TAX
               END-EVALUATE
           END-EVALUATE
      *
           .
       45010-JIHI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外の集計処理
      *****************************************************************
       45011-TGMONEY-SET-SEC               SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
           IF     (SPA-HKNKBN          =   1   )  AND
                  (SPA-COM-SRYKBN (IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(IDX)(1:5)   =   "09591"
                                               OR  "09592"
                                               OR  "09593"
                                               OR  "09594")
               PERFORM 450112-JIBAISEKI-SET-SEC
               GO      TO      45011-TGMONEY-SET-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(IDX)(1:3)   =   "095"  OR  "096"
               MOVE    SPA-COM-INPUTCD(IDX)(4:2)   TO  WRK-SRYKBN
               MOVE    SPACE                       TO  WRK-TNSSRYSYUKBN
           ELSE
               MOVE    SPA-COM-TNSSRYKBN(IDX)      TO  WRK-SRYKBN
               MOVE    SPA-COM-TNSSRYSYUKBN(IDX)   TO  WRK-TNSSRYSYUKBN
           END-IF
      *
      *    薬剤の時、投薬へ
           IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
      *        注射薬の時、注射へ
               IF      SPA-COM-YKZKBN (IDX)    =   "4"
                   MOVE    "31"                        TO  WRK-SRYKBN
               ELSE
                   MOVE    "21"                        TO  WRK-SRYKBN
               END-IF
           END-IF
      *    器材の時、その他へ
           IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "7" )  OR
                  (SPA-COM-INPUTCD(IDX)(1:3)   =   "059")
               MOVE    "80"                        TO  WRK-SRYKBN
           END-IF
      *!!!
      *H30.6
      *    テーブル位置決定
           PERFORM 450110-SRYKBN-TBL-SEC
      *
      *    桁数チェック
           PERFORM 450113-TGMONE-CHK-SEC
      *
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
      *H30.6
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (IDG)
      ************ PERFORM 450111-TGMONEY-SEC
      *        消費税あり
               WHEN    "960"
      *H26.10      追加
               WHEN    "961"
      *H30.6
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                               (IDG)
      *************PERFORM 450111-TGMONEY-TAX-SEC
           END-EVALUATE
           .
       45011-TGMONEY-SET-EXT.
           EXIT.
      *
      *H30.6
      *****************************************************************
      *    保険適用外の桁数チェック処理
      *****************************************************************
       450113-TGMONE-CHK-SEC               SECTION.
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
                   IF     (SPA-GMN-KEI (IDX)   +   SYU-TGMONEY (IDG))
                                               >   9999999
                       MOVE    "1089"        TO  SPA-ERRCD
                       MOVE    "1"           TO  SPA-COM-CUR(IDX)
                   END-IF
      *        消費税あり
               WHEN    "960"
               WHEN    "961"
                   IF     (SPA-GMN-KEI (IDX)   +   SYU-TGMONEY-TAX
                                                                (IDG))
                                               >   9999999
                       MOVE    "1089"        TO  SPA-ERRCD
                       MOVE    "1"           TO  SPA-COM-CUR(IDX)
                   END-IF
           END-EVALUATE
           .
       450113-TGMONE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分から集計位置決定処理
      *****************************************************************
       450110-SRYKBN-TBL-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDG
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   MOVE    1                   TO  IDG
      *        指導
               WHEN    "13"
                   MOVE    2                   TO  IDG
      *        在宅
               WHEN    "14"
                   MOVE    3                   TO  IDG
      *        投薬
               WHEN    "21"   THRU  "27"
                   MOVE    4                   TO  IDG
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   MOVE    5                   TO  IDG
      *        処置
               WHEN    "40"
                   MOVE    6                   TO  IDG
      *        手術
               WHEN    "50"
                   MOVE    7                   TO  IDG
      *        麻酔
               WHEN    "54"
                   MOVE    8                   TO  IDG
      *        検査
               WHEN    "60"
                   MOVE    9                   TO  IDG
      *        Ｘ線
               WHEN    "70"
                   MOVE    10                  TO  IDG
      *        病理診断
               WHEN    "64"
                   MOVE    14                  TO  IDG
      *        その他
               WHEN    "80"
                   EVALUATE    WRK-TNSSRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       MOVE    11                  TO  IDG
      *            処方せん料
                   WHEN    "820"
                       MOVE    04                  TO  IDG
      *            精神科
                   WHEN    "830"
                       MOVE    12                  TO  IDG
      *            放射線治療
                   WHEN    "840"
                       MOVE    13                  TO  IDG
      *            入院料
                   WHEN    "890"
                       MOVE    15                  TO  IDG
      *            療養担当手当て
                   WHEN    "850"
                       MOVE    16                  TO  IDG
      *            その他
                   WHEN    OTHER
                       MOVE    11                  TO  IDG
                   END-EVALUATE
      *        精神科
               WHEN    "83"
                   MOVE    12                  TO  IDG
      *        放射線治療
               WHEN    "84"
                   MOVE    13                  TO  IDG
      *        入院料
               WHEN    "89"
                   MOVE    15                  TO  IDG
      *        療養担当手当て
               WHEN    "85"
                   MOVE    16                  TO  IDG
      *        その他
               WHEN    OTHER
                   MOVE    11                  TO  IDG
           END-EVALUATE
      *
           .
       450110-SRYKBN-TBL-EXT.
           EXIT.
      *
      *H30.6から　不使用
      *****************************************************************
      *    保険適用外の集計処理(消費税なし)
      *****************************************************************
       450111-TGMONEY-SEC               SECTION.
      *
      *
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (14)
      *        その他
               WHEN    "80"
                   EVALUATE    WRK-TNSSRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (11)
      *            処方せん料
                   WHEN    "820"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (04)
      *            精神科
                   WHEN    "830"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (12)
      *            放射線治療
                   WHEN    "840"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (13)
      *            入院料
                   WHEN    "890"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (15)
      *            療養担当手当て
                   WHEN    "850"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (16)
      *            その他
                   WHEN    OTHER
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (11)
                   END-EVALUATE
      *        精神科
               WHEN    "83"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (12)
      *        放射線治療
               WHEN    "84"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (13)
      *        入院料
               WHEN    "89"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (15)
      *        療養担当手当て
               WHEN    "85"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (16)
      *        その他
               WHEN    OTHER
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (11)
           END-EVALUATE
      *
           .
       450111-TGMONEY-EXT.
           EXIT.
      *
      *H30.6から　不使用
      *****************************************************************
      *    保険適用外の集計処理(消費税あり)
      *****************************************************************
       450111-TGMONEY-TAX-SEC               SECTION.
      *
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (14)
      *        その他
               WHEN    "80"
                   EVALUATE    WRK-TNSSRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (11)
      *            処方せん料
                   WHEN    "820"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (04)
      *            精神科
                   WHEN    "830"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (12)
      *            放射線治療
                   WHEN    "840"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (13)
      *            療養担当手当て
                   WHEN    "850"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (16)
      *            入院料
                   WHEN    "890"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (15)
      *            その他
                   WHEN    OTHER
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (11)
                   END-EVALUATE
      *        精神科
               WHEN    "83"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (12)
      *        放射線治療
               WHEN    "84"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (13)
      *        療養担当手当て
               WHEN    "85"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (16)
      *        入院料
               WHEN    "89"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (15)
      *        その他
               WHEN    OTHER
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (11)
           END-EVALUATE
      *
           .
       450111-TGMONEY-TAX-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自賠責診断料・明細料その他の編集処理
      *****************************************************************
       450112-JIBAISEKI-SET-SEC        SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
           IF     (SPA-COM-SRYSYUKBN (IDX)
                                       =   "809"  )
      *        金額入力
               COMPUTE WRK-EN              =   SPA-COM-JIHIMONEY (IDX)
                                           *   SPA-COM-KAISU (IDX)
               MOVE    SPA-COM-JIHIMONEY (IDX) TO  WRK-TANKA
           ELSE
      *    労災保険−その他
               COMPUTE WRK-EN              =   SPA-COM-TEN (IDX)
                                           *   SPA-COM-KAISU (IDX)
               MOVE    SPA-COM-TEN (IDX)       TO  WRK-TANKA
           END-IF
           ADD     WRK-EN              TO  SYU-RSIETC-MONEY
      *    労災の円
           MOVE    WRK-TANKA           TO  SPA-COM-RSIKIN(IDX)
      *
           MOVE    1                   TO  FLG-RSITEN
      *
           .
       450112-JIBAISEKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険計算処理
      *****************************************************************
       45011-RSI-SYUNOU-SEC               SECTION.
      *
      *    労災保険は円と点数に分ける
           IF     (SPA-COM-TENSIKIBETU (IDX)   =   1  )  AND
                  (SPA-COM-RSIKBN      (IDX)   =   1  )  AND
                  (SPA-COM-INPUTCD (IDX)(1:1)  =   "1")
      *        円
      *H26.2
      *        労災金額％加算再計算対応
               PERFORM 450112-RSI-SKASAN-SEC
      *******  COMPUTE WRK-EN          =   SPA-COM-TEN (IDX)
               COMPUTE WRK-EN          =   WRK-ENTANKA
                                       *   SPA-COM-KAISU (IDX)
               MOVE    ZERO            TO  WRK-TENSU
      *        労災の円
      ******   MOVE    WRK-EN          TO  SPA-COM-RSIKIN(IDX)
               MOVE    WRK-ENTANKA     TO  SPA-COM-RSIKIN(IDX)
               MOVE    1               TO  FLG-RSITEN
           ELSE
               IF     (SPA-COM-TENSIKIBETU (IDX)   =   3  )
      *            点数
                   COMPUTE WRK-TENSU       =   SPA-COM-KISOTEN (IDX)
                                           *   SPA-COM-KAISU (IDX)
                   IF      SPA-COM-PLUSKBN(IDX)    =   1
                       COMPUTE WRK-TENSU       =  (SPA-COM-KISOTEN (IDX)
                                               *   SPA-COM-KAISU (IDX) )
                                               *   -1
                   END-IF
                   MOVE    ZERO            TO  WRK-EN
      *            労災の円
                   MOVE    ZERO            TO  SPA-COM-RSIKIN(IDX)
      *
               ELSE
                   MOVE    ZERO            TO  WRK-EN
                   MOVE    ZERO            TO  WRK-TENSU
               END-IF
           END-IF
      *
           IF     (SPA-COM-SRYKBN  (IDX)   =   "11"
                                           OR  "12"
                                           OR  "13" ) OR
                 ((SPA-COM-SRYKBN (IDX)    =   "80" ) AND
                  (FLG-RSITEN              =   1    ))
               CONTINUE
           ELSE
               IF      SPA-COM-ZAIEND (IDX)    NOT =   "1"
                   GO      TO      45011-RSI-SYUNOU-EXT
               END-IF
           END-IF
      *H27.7
      *    収納　円・点数
           IF     (WRK-TENSU           >   ZERO)
              AND (FLG-RSITEN          =   1  )
               MOVE    WRK-TENSU           TO  TBL-SYU-TENSU (IDX)
           END-IF
      *H27.7
      *
           EVALUATE    SPA-COM-SRYKBN  (IDX)
      *        初診
               WHEN    "11"
      *
      *        労災保険−初診料
                   ADD     WRK-EN              TO  SYU-RSISHOSHIN-MONEY
                   ADD     WRK-TENSU           TO  SUM-SYU-HKNTEN (01)
      *        再診
               WHEN    "12"
      *            *労災保険−再診料
                   ADD     WRK-EN              TO  SYU-RSISAISHIN-MONEY
                   ADD     WRK-TENSU           TO  SUM-SYU-HKNTEN (01)
      *        指導
               WHEN    "13"
      *            労災保険−指導料
                   ADD     WRK-EN              TO  SYU-RSISDO-MONEY
                   ADD     WRK-TENSU           TO  SUM-SYU-HKNTEN (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (14)
      *        その他
               WHEN    "80"
      *            労災保険−その他
                   ADD     WRK-EN              TO  SYU-RSIETC-MONEY
      *            その他の円と点数は剤内で混在しないこととする(H14.8)
                   IF     (WRK-EN              =   ZERO )
                     AND  (FLG-RSITEN          =   ZERO )
                       PERFORM 45011-HKNTEN-SONOTA-SEC
                   END-IF
           END-EVALUATE
           .
       45011-RSI-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災金額％加算再計算対応処理
      *****************************************************************
       450112-RSI-SKASAN-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-ENTANKA
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
               COMPUTE IDR-1           =   SPA-GMN-MAX +   1
           ELSE
               COMPUTE IDR-1           =   IDX     +   1
           END-IF
           PERFORM VARYING     IDR-2   FROM    IDR-1   BY  1
                   UNTIL      (IDR-2   >   SPA-GMN-MAX  )
      *
               IF     (SPA-COM-TENSIKIBETU (IDR-2) =   "5"  )  AND
                      (SPA-COM-INPUTCD(IDR-2)(1:3) =   "101")
      *************％加算　円未満切り捨て
      *            ％加算　円未満切り上げ（労災加算と合わせる）
                   MOVE    ZERO                TO  WRK-TENKEISAN
                   COMPUTE WRK-TENKEISAN   =   SPA-COM-TEN (IDX)
                                           *   SPA-COM-KISOTEN (IDR-2)
                                           /   100
                                           +   0.9
                   MOVE    WRK-TENKEISAN       TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *対象なし
      *        IF     (SPA-COM-TENSIKIBETU (IDR-2) =   "6"  )  AND
      *               (SPA-COM-INPUTCD(IDR-2)(1:3) =   "101")
      *            ％減算　円未満切り捨て
      *            MOVE    ZERO                TO  WRK-TENKEISAN
      *            COMPUTE WRK-TENKEISAN   =   SPA-COM-TEN (IDX)
      *                                    *   SPA-COM-KISOTEN (IDR-2)
      *                                    /   100
      *            MOVE    WRK-TENKEISAN       TO  WRK-TENSUKEI-9
      *            ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
      ***      END-IF
               IF      (SPA-COM-ZAIEND (IDR-2)  =   "1" )
                   MOVE    SPA-GMN-MAX     TO  IDR-2
               END-IF
           END-PERFORM
           COMPUTE WRK-ENTANKA         =   SPA-COM-TEN (IDX)
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
           .
       450112-RSI-SKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動計算処理
      *****************************************************************
       4504-JIDOUKEI-SEC               SECTION.
      *
      *    自動加算分集計
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   ORCSCWKSRY-KSN-IDX
      *    包括剤は対象外
           IF     (ORCSCWKSRY-KSN-HKTKBN  (IDX)   =   1 )  OR
                  (ORCSCWKSRY-KSN-HKNCOMBI(IDX)   =   "9999")
               CONTINUE
           ELSE
      *
            IF   ( ORCSCWKSRY-KSN-HKNCOMBI (IDX)  =   WRK-HKNCOMBI) AND
                 ((WRK-SRYKA               =   ZERO   )   OR
                  (ORCSCWKSRY-KSN-SRYKA    (IDX)  =   WRK-SRYKA  ))
               EVALUATE    ORCSCWKSRY-KSN-SRYKBN  (IDX)
      *            診療
                   WHEN    "11"
                   WHEN    "12"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (01)
      *            指導
                   WHEN    "13"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (02)
      *            在宅
                   WHEN    "14"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (03)
      *            投薬
                   WHEN    "21"   THRU  "27"
      *                薬剤料逓減　を減算
                       IF      ORCSCWKSRY-KSN-SRYCD (IDX)
                                                     =   "630010002"
      *H26.10                  向精神薬多剤投与
                                                 OR  CONS-TAZAI-SRYCD
      *H27.4                   低紹介率病院　減算コード
                                                 OR  CONS-TEIHP-SRYCD
      *H28.4                   湿布薬枚数　減点
                                                 OR  CONS-SPGEN-SRYCD
                           COMPUTE SUM-SYU-HKNTEN (04)
                                               =   SUM-SYU-HKNTEN(04)
                                               -   ORCSCWKSRY-KSN-TENSU
                                                                 (IDX)
                       ELSE
                           ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                                    TO  SUM-SYU-HKNTEN
                                                                  (04)
                       END-IF
      *            注射
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(05)
      *            処置
                   WHEN    "40"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(06)
      *            手術
                   WHEN    "50"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(07)
      *            麻酔
                   WHEN    "54"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(08)
      *            検査
                   WHEN    "60"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(09)
      *            Ｘ線
                   WHEN    "70"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(10)
      *            病理診断
                   WHEN    "64"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(14)
      *            その他
                   WHEN    "80"
                       PERFORM 4504-JIDOUKEI-SONOTA-SEC
               END-EVALUATE
             END-IF
      *
           END-IF
      *
           END-PERFORM
      *
           .
       4504-JIDOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    その他個別集計処理
      *****************************************************************
       4504-JIDOUKEI-SONOTA-SEC               SECTION.
      *
           EVALUATE    ORCSCWKSRY-KSN-SRYSYUKBN (IDX)
      *        リハビリ（その他）
               WHEN    "800"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (11)
      *        処方せん料
               WHEN    "820"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (04)
      *        処方せん料再掲
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SYU-SHOHOU-SAI
      *        精神科
               WHEN    "830"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (12)
      *        放射線治療
               WHEN    "840"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (13)
      *        療養担当手当て
               WHEN    "850"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (16)
      *        入院料
               WHEN    "890"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (15)
      *        その他
               WHEN    OTHER
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                              TO  SUM-SYU-HKNTEN (11)
           END-EVALUATE
           .
       4504-JIDOUKEI-SONOTA-EXT.
           EXIT.
      *
      *****************************************************************
      *    各合計計算処理
      *****************************************************************
       4506-TOTALKEI-SEC             SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   16
      *        保険点数
               IF      SUM-SYU-HKNTEN(IDX)  <   ZERO
                   IF      SPA-NYUGAIKBN        =   "2"
                       MOVE    "0224"           TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    SUM-SYU-HKNTEN (IDX) TO  SYU-HKNTEN(IDX)
              END-IF
      *!!!
      *H30.6
      *       桁オーバーチェック
              IF     (SUM-SYU-HKNTEN(IDX)  +   SUM-SYU-HKNTEN(17))
                                       >   9999999
                  IF      SPA-ERRCD        =   SPACE
                      MOVE    "1090"           TO  SPA-ERRCD
                  END-IF
              END-IF
              ADD     SUM-SYU-HKNTEN(IDX)  TO  SUM-SYU-HKNTEN(17)
              IF     (SYU-TGMONEY(IDX) +   SYU-TGMONEY(17))
                                       >   9999999
                  IF      SPA-ERRCD        =   SPACE
                      MOVE    "1089"           TO  SPA-ERRCD
                  END-IF
              END-IF
              IF     (SYU-TGMONEY-TAX(IDX) +   SYU-TGMONEY-TAX(17))
                                       >   9999999
                  IF      SPA-ERRCD        =   SPACE
                      MOVE    "1089"           TO  SPA-ERRCD
                  END-IF
              END-IF
      *H30.6
      *
      *       保険点数
              ADD     SYU-HKNTEN(IDX)      TO  SYU-HKNTEN(17)
      *       金額
              ADD     SYU-MONEY (IDX)      TO  SYU-MONEY (17)
      *       適用外金額
              ADD     SYU-TGMONEY(IDX)     TO  SYU-TGMONEY(17)
      *       適用外金額（消費税あり）
              ADD     SYU-TGMONEY-TAX(IDX) TO  SYU-TGMONEY-TAX(17)
      *
           END-PERFORM
      *
      *H30.6
      *    自費合計桁数７桁オーバーチェック
           PERFORM 45069-TOTALOVER-CHK-SEC
      *
           .
       4506-TOTALKEI-EXT.
           EXIT.
      *
      *H30.6
      *****************************************************************
      *    自費合計桁数７桁オーバーチェック処理
      *****************************************************************
       45069-TOTALOVER-CHK-SEC         SECTION.
      *
      *    合計桁数７桁オーバーチェック
           IF     (SYU-HKNTEN(17)      +   WRK-SUM-HKNTEN )
                                       >   9999999
               MOVE    "1089"              TO  SPA-ERRCD
           END-IF
           IF     (SYU-TGMONEY(17)     +   WRK-SUM-TGMONEY )
                                       >   9999999
               MOVE    "1089"              TO  SPA-ERRCD
           END-IF
           IF     (SYU-TGMONEY-TAX(17) +   WRK-SUM-TGMONEY-TAX )
                                       >   9999999
               MOVE    "1089"              TO  SPA-ERRCD
           END-IF
      *
           ADD     SYU-HKNTEN(17)      TO  WRK-SUM-HKNTEN
           ADD     SYU-TGMONEY(17)     TO  WRK-SUM-TGMONEY
           ADD     SYU-TGMONEY-TAX(17) TO  WRK-SUM-TGMONEY-TAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )
      *        消費税なし
               IF     (SYU-JIHI-TAXNASI (IDX)  +  WRK-SUM-JIHI-TOTAL)
                                           >   9999999
                   MOVE    "1089"          TO  SPA-ERRCD
               END-IF
      *        消費税あり
               IF     (SYU-JIHI-TAXARI  (IDX)
                                           +  WRK-SUM-JIHI-TOTAL-TAX)
                                           >   9999999
                   MOVE    "1089"          TO  SPA-ERRCD
               END-IF
               ADD     SYU-JIHI-TAXNASI(IDX)  TO  WRK-SUM-JIHI-TOTAL
               ADD     SYU-JIHI-TAXARI (IDX)  TO  WRK-SUM-JIHI-TOTAL-TAX
           END-PERFORM
           .
       45069-TOTALOVER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動作成の診療行為作成処理
      *****************************************************************
       3004-JIDOU-SAKU-SEC         SECTION.
      *
      *    自動加算料
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  WRK-HOUKATU-ZAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCSCWKSRY-KSN-IDX
               IF      IDY             =   ZERO
                   MOVE    SPACE               TO  WKSRYACT-REC
                   INITIALIZE                  WKSRYACT-REC
                   MOVE    SPACE               TO  WKSRYACTPLUS-REC
                   INITIALIZE                  WKSRYACTPLUS-REC
                   ADD     1                   TO  WRK-ZAINUM
                   MOVE    ZERO                TO  WRK-RENNUM
                   MOVE    ORCSCWKSRY-KSN-HKNCOMBI (IDX)
                                               TO  WRK-HKNCOMBI
                   MOVE    ORCSCWKSRY-KSN-SRYKA (IDX) TO  WRK-SRYKA
                   MOVE    ORCSCWKSRY-KSN-SRYSYUKBN(IDX)
                                               TO  WRK-SRYSYUKBN
                   MOVE    ORCSCWKSRY-KSN-SRYKBN(IDX) TO  WRK-SRYKBN
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   MOVE    ZERO                TO  WRK-KAISUKEI
                   MOVE    ZERO                TO  WRK-KINGAK
                   MOVE    ZERO                TO  WRK-MEINUM
               END-IF
               ADD     1                       TO  IDY
               MOVE    ORCSCWKSRY-KSN-SRYCD  (IDX)
                                               TO  WKSRY-SRYCD (IDY)
               MOVE    ORCSCWKSRY-KSN-INPUTCD(IDX)
                                               TO  WKSRY-INPUTCD (IDY)
               MOVE    1                       TO  WKSRY-SRYSURYO(IDY)
               IF      ORCSCWKSRY-KSN-SURYO(IDX)   >   1
                   MOVE    ORCSCWKSRY-KSN-SURYO (IDX)
                                               TO  WKSRY-SRYSURYO(IDY)
               END-IF
               MOVE    ZERO                    TO  WKSRY-ZAIKAISU(IDY)
               MOVE    ORCSCWKSRY-KSN-AUTOKBN (IDX)
                                               TO  WKSRY-AUTOKBN (IDY)
               IF      ORCSCWKSRY-KSN-AUTOKBN(IDX) =   SPACE
                    MOVE    "3"                TO  WKSRY-AUTOKBN (IDY)
               END-IF
               IF      ORCSCWKSRY-KSN-AUTOKBN(IDX) =   "1"
                    MOVE    SPACE              TO  WKSRY-AUTOKBN (IDY)
               END-IF
               IF     (ORCSCWKSRY-KSN-SRYCD  (IDX)(1:1)
                                                       =   "8"  )  AND
                      (ORCSCWKSRY-KSN-ATAI-G (IDX) NOT =   SPACE )
                   PERFORM 45022-COMMNT-SET-SEC
               END-IF
               ADD     ORCSCWKSRY-KSN-TENSU(IDX)      TO  WRK-TENSUKEI
               ADD     ORCSCWKSRY-KSN-TENSU(IDX)      TO  WRK-SYUTEN1
               IF      ORCSCWKSRY-KSN-HKTKBN(IDX)  =  1
                   MOVE    ORCSCWKSRY-KSN-HKTKBN(IDX)
                                               TO  WRK-HOUKATU-ZAI
               END-IF
               MOVE    1                       TO  WRK-KAISUKEI
      *        各附加情報編集処理
               PERFORM 30041-WKSRYACTPLUS-SEC
      *
               IF      ORCSCWKSRY-KSN-ZAIEND(IDX)     =   "1"
      *            診療行為パラメタ出力
      ***          MOVE    ORCSCWKSRY-KSN-HKTKBN(IDX)
      ******                                   TO  WRK-HOUKATU-ZAI
                   MOVE    SPACE               TO  WRK-YAKUHYO
                   MOVE    SPACE               TO  WRK-GENTEN
                   MOVE    SPACE               TO  WRK-JISANYAKU
                   PERFORM 300211-WKSRYPARA-WRITE-SEC
                   ADD     1                   TO  WRK-ZAINUM
                   MOVE    ZERO                TO  IDY
                   INITIALIZE                  WRK-WKSRY-AREA
                   MOVE    ZERO                TO  WRK-HOUKATU-ZAI
               END-IF
           END-PERFORM
           .
       3004-JIDOU-SAKU-EXT.
           EXIT.
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       30041-WKSRYACTPLUS-SEC          SECTION.
      *
      *    薬剤料逓減　を減算
           IF      ORCSCWKSRY-KSN-SRYCD (IDX)  =   "630010002"
      *H26.10      向精神薬多剤投与
                                               OR  CONS-TAZAI-SRYCD
               MOVE    1                   TO  WKSRYP-PLUSKBN
      *        内服７種類逓減対象
               MOVE    2                   TO  WKSRYP-TEI-NKBN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *
      *H27.4
      *    低紹介率病院　減算
      *    薬剤料逓減　を減算
           IF      ORCSCWKSRY-KSN-SRYCD (IDX)  =   CONS-TEIHP-SRYCD
               MOVE    1                   TO  WKSRYP-PLUSKBN
      *        内服７種類逓減対象
               MOVE    2                   TO  WKSRYP-TEI-NKBN30
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *
      *H28.4
      *    湿布薬減点
           IF      ORCSCWKSRY-KSN-SRYCD (IDX)  =   CONS-SPGEN-SRYCD
               MOVE    1                   TO  WKSRYP-PLUSKBN
      *        内服７種類逓減対象
               MOVE    2                   TO  WKSRYP-TEI-NKBN30
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *
           .
       30041-WKSRYACTPLUS-EXT.
           EXIT.
      *****************************************************************
      *    追加のコメントの日数変換編集処理
      *****************************************************************
       45022-COMMNT-SET-SEC              SECTION.
      *
           INITIALIZE                  WRK-SCR-REC
           MOVE    WKSRY-SRYCD (IDY)   TO  WRK-COM-INPUTCD (01)
           MOVE    ORCSCWKSRY-KSN-ATAI-G(IDX) TO  WRK-COM-INPUTCHI-G(01)
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    1                   TO  LNK-SC92-GMN-IDX
           MOVE    WKSRY-SRYCD (IDY)   TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    入力コメント番号
           MOVE    1                   TO  WKSRY-INPUTNUM  (IDY)
      *
           MOVE    WRK-COM-ATAI1(01)   TO  WKSRY-INPUTCHI  (IDY 1)
           MOVE    WRK-COM-ATAI2(01)   TO  WKSRY-INPUTCHI  (IDY 2)
           MOVE    WRK-COM-ATAI3(01)   TO  WKSRY-INPUTCHI  (IDY 3)
           MOVE    WRK-COM-ATAI4(01)   TO  WKSRY-INPUTCHI  (IDY 4)
           MOVE    WRK-COM-ATAI5(01)   TO  WKSRY-INPUTCHI  (IDY 5)
           MOVE    WRK-GMN-MEISYO(01)  TO  WKSRY-INPUTCOMENT (IDY)
      *
           .
       45022-COMMNT-SET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
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
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
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
               MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               MOVE    ZERO                TO  FLG-TENSUPLUS
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO            TO  MCP-RC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      **** EXIT.
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
      *
