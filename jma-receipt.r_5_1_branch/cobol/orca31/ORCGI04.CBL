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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 請求確認画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/10/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02.12.10  収納明細作成処理追加
      *  01.00.02    NACL-太田    03.01.23  食事の計算方法修正
      *  01.00.03    NACL-太田    03.01.24  保険組合せリストの表示を修正
      *  01.00.04    NACL-太田    03.02.05  自院歴作成データを考慮する
      *  01.00.05    NACL-太田    03.03.07  帳票発行コンボの初期表示修正
      *  01.00.06    NACL-太田    03.03.13  入院請求書複数枚一括印刷対応
      *  01.00.07    NACL-太田    03.03.18  入院カスタマイズ帳票対応
      *  01.00.08    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.09    NACL-太田    03.11.14  更新日付編集対応
      *  01.00.10    NACL-太田    04.08.02  定期請求中は退院登録は不可
      *                                     とする
      *  01.00.11    NACL-太田    04.10.14  退院事由追加
      *  01.00.12    NACL-小豆沢  05.07.19  自費１０項目対応
      *  01.00.13    NACL-小豆沢  05/10/17  入金額ゼロでも入金方法を
      *                                     セットする
      *  03.05.00    NACL-太田    07.05.15  グループ診療対応
      *  04.03.00    NACL-多々納  08.08.27  退院時院外処方せん印刷対応
      *  04.05.00    NACL-太田    09.07.13  請求書発行方法対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    パラメタファイル名
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I0SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUDAY          PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-BUNBEN          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
      *
           03  FLG-SYOSIN-DUMMY    PIC 9(01).
      *
           03  FLG-TOKTEI          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-ENTRYCHK        PIC 9(01).
           03  FLG-ZI-SAISIN       PIC 9(01).
           03  FLG-NYUINKA         PIC 9(01).
           03  FLG-SKYPRT          PIC 9(01).
           03  FLG-SRYPRT          PIC 9(01).
           03  FLG-TEISEIINF       PIC 9(01).
           03  FLG-TARGET          PIC 9(01).
           03  FLG-HKNCOMBILST-ROW PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX6                PIC 9(05).
           03  IDX7                PIC 9(05).
           03  IDX8                PIC 9(05).
           03  IDX-M               PIC 9(05).
           03  IDX-MAX             PIC 9(05).
           03  IDX-G               PIC 9(05).
           03  IDX-SYU             PIC 9(05).
           03  IDXC                PIC 9(05).
           03  IDXY                PIC 9(05).
           03  IDXZ                PIC 9(05).
           03  IDX-LST             PIC 9(05).
           03  IDX-CMB             PIC 9(05).
           03  IDX-SUM             PIC 9(05).
           03  IDX-STR             PIC 9(05).
           03  IDX-NYUKIN          PIC 9(05).
           03  IDX-TUKI            PIC 9(05).
           03  IDX-HKNCOMBI        PIC 9(05).
           03  IDX-CHOSEI          PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TUKI            PIC 9(05).
           03  CNT-HKNCOMBI        PIC 9(05).
           03  CNT-SRYPRT-PAGE     PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SKYMONEY               PIC  S9(08).
           03  WRK-YMD                    PIC X(10). 
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-HENYMDG                PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
      *
      *****03  WRK-Z9                      PIC Z,ZZZ,ZZZ.
           03  WRK-Z99                     PIC --,---,--9.
           03  WRK-Z9                      PIC --,---,---.
           03  WRK-Z92                     PIC --,---,---.
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-99                      PIC 9(02).
           03  WRK-TEISEIBF                PIC X(20).
           03  WRK-TEISEIAF                PIC X(20).
           03  WRK-STR                     PIC X(500).
           03  WRK-STR-LEN                 PIC 9(5).
           03  WRK-TMP                     PIC X(500).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
      *    パス名
           03  WRK-FTNRATE                 PIC X(05).
           03  WRK-I0IDMSG                 PIC X(80).
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-KBNCD                   PIC X(08).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-SEIKYU                  PIC S9(10).
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-G.
               05  WRK-CMB-IDX             PIC 9(03).
               05  WRK-CMB-ITM             PIC X(100).
               05  WRK-CMB-ITM2            PIC X(100).
           03  WRK-TAIINUPDCNT             PIC 9(05).
           03  WRK-HKNCOMBI                PIC X(60).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-TAIHI-SRYKA             PIC X(02).
           03  WRK-ZI-SAISIN-RRKNUM        PIC 9(03).
           03  WRK-ZI-SAISIN-RRKEDANUM     PIC 9(03).
           03  WRK-LST-IDX                 PIC 9(05).
           03  WRK-IDX                     PIC 9(05).
           03  WRK-DENPNUM                 PIC 9(07).
           03  WRK-DENPNUM-X               PIC X(08).
           03  WRK-DENPEDANUM              PIC 9(02).
           03  WRK-NYUKINRENNUM            PIC 9(03).
           03  WRK-NYUINKA                 PIC X(02).
           03  WRK-BTUNUM                  PIC X(02).
           03  WRK-BRMNUM                  PIC X(08).
           03  WRK-DRCD                    PIC X(04).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-EDIDX                   PIC 9(05).
           03  WRK-SKYMONEY-BF             PIC S9(08).
           03  WRK-SKY-CNT                 PIC 9(10).
           03  WRK-PRINT-PG                PIC X(20).
           03  WRK-SEIKYU-PG               PIC X(20).
           03  WRK-MEISAI-PG               PIC X(20).
           03  WRK-ORG-PRINT-PG            PIC X(20).
           03  WRK-LASTTEIKIEDYMD          PIC X(08).
           03  WRK-SEIKYU-G.
               05  WRK-SKY-SKYMONEY-TAX-SAI PIC S9(08).
               05  WRK-SKY-SKYMONEY        PIC  S9(08).
               05  WRK-SKY-NYUKIN-TOTAL    PIC  S9(08).
               05  WRK-SKY-KONMISYU        PIC  S9(08).
           03  WRK-NYUKIN                  PIC  S9(08).
           03  WRK-NYUKINTTL               PIC  S9(08).
           03  WRK-KONNYUKINTTL            PIC  S9(08).
           03  WRK-PATHNAME                PIC X(64).
           03  WRK-NYUKINLIMIT             PIC  S9(08).
           03  WRK-LASTSRYYM               PIC X(06).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-CHOSEITTL               PIC  S9(08).
           03  WRK-CHOSEI                  PIC  S9(08).
           03  WRK-BASE-MONEY              PIC  S9(08).
           03  WRK-GMN-CUR                 PIC  9(03).
      *!!
      *I041用
           03  IDX-D                    PIC 9(04).
      *
       01  TSYUGRP-AREA.
           03  TSYUGRP-GRP-MAX             PIC 9(05).
           03  TSYUGRP-GRP-OCC             OCCURS   10.
               05  TSYUGRP-SKYPRT          PIC X(01).
               05  TSYUGRP-SRYPRT          PIC X(01).
               05  TSYUGRP-SKYSTYMD        PIC X(08).
               05  TSYUGRP-SKYEDYMD        PIC X(08).
               05  TSYUGRP-GRP-MAX2        PIC 9(05).
               05  TSYUGRP-GRP-OCC2        OCCURS   10.
                   07  TSYUGRP-GRP-DENPNUM PIC 9(07).
                   07  TSYUGRP-GRP-SRYYMD  PIC X(08).
      *
       01  BFSYU-AREA.
           03  BFSYU-MAX                           PIC 9(05).
           03  BFSYU-OCC                           OCCURS  400.
               05  BFSYU-DENPNUM                   PIC S9(9).
               05  BFSYU-SKYSTYMD                  PIC X(08).
               05  BFSYU-SEIKYU                    PIC S9(9).
               05  BFSYU-NYUKIN                    PIC S9(9).
      *
       01  CONST-AREA.
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
           03  CONST-H270101               PIC X(08)   VALUE "20150101".
           03  CONST-JOB-SHELLID           PIC X(08)   VALUE   "TEIKI1".
           03  CONST-JOB-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-EDIT-RSIJBI           PIC 9(01)   VALUE   ZERO.
           03  CONST-EDIT-TEIKI            PIC 9(01)   VALUE   1.
           03  CONST-BFSYU-MAX             PIC 9(05)   VALUE   400.
           03  CONST-PRTDEF-G.
               05  CONST-PRTDEF0       PIC X(09)   VALUE   "000000000".
               05  CONST-PRTDEF1       PIC X(09)   VALUE   "111111111".
               05  CONST-PRTDEF2       PIC X(09)   VALUE   "222222222".
               05  CONST-PRTDEF3       PIC X(09)   VALUE   "311110000".
               05  CONST-PRTDEF4       PIC X(09)   VALUE   "410101010".
               05  CONST-PRTDEF5       PIC X(09)   VALUE   "520202020".
               05  CONST-PRTDEF6       PIC X(09)   VALUE   "610100000".
               05  CONST-PRTDEF7       PIC X(09)   VALUE   "721212121".
               05  CONST-PRTDEF8       PIC X(09)   VALUE   "801010101".
               05  CONST-PRTDEF9       PIC X(09)   VALUE   "902020202".
               05  CONST-PRTDEFA       PIC X(09)   VALUE   "A01010000".
           03  CONST-PRTDEF-R  REDEFINES   CONST-PRTDEF-G.
      *        0〜7 患者登録業務の請求書発行区分
               05  CONST-PRTDEF-OCC            OCCURS  11.
                   07  CONST-PRTDEF-PRTFLG     PIC  X(01).
      *            (1):新規 (2)：訂正時
                   07  CONST-PRTDEF-OCC2       OCCURS  2.
      *            (1):請求額>0 (2)：請求額=0
                       09  CONST-PRTDEF-OCC3   OCCURS  2.
                           11  CONST-SKYPRTOUT PIC X(01).
                           11  CONST-SRYPRTOUT PIC X(01).
           03  CONST-PRTDEF-MAX                PIC 9(05)   VALUE   11.
           03  CONST-KINGAKU-MAX               PIC 9(10) VALUE 9999999.
      *
       01  WRK-ROW-AREA.
           03  WRK-HKNCOMBILST-ROW         PIC S9(9)   BINARY.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    患者入院履歴ワーク
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"
                                     REPLACING
                               //PTNYUINRRK-// BY //WKPTNYUINRRK-//.
      *
       01  NRRK-REC.
           COPY    "CPPTNYUINRRK.INC"
                                     REPLACING
                               //PTNYUINRRK-// BY //NRRK-//.
      *    収納ワーク
       01  WKSYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-//
                                           BY //WKSYU-//.
      *
      *    日別収納ワーク
       01  WKSYUDAY-REC.
           COPY  "CPSYUDAY.INC"    REPLACING  //SDAY-//
                                           BY //WKSDAY-//.
      *
      *    収納明細ワーク
       01  WKSYUMEI-REC.
           COPY  "CPSYUMEI.INC"    REPLACING  //SMEI-//
                                           BY //WKSMEI-//.
      *
           COPY  "CPORCHCN03.INC"  REPLACING  //ORCHCN03//
                                           BY //WORCHCN03//.
      *
           COPY  "CPLNKS08.INC".
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
           COPY    "CPSK0042.INC".
      *
      *    診療科コード
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1013.INC".
      *    ドクター
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK9700.INC".
      *
      *    帳票プログラム名
           COPY    "CPSK1035.INC".
      *    医療機関入院基本情報
           COPY    "CPSK5000.INC".
      *    出力先プリンタ名割り当て情報\
           COPY  "CPSK1031.INC".
      *    帳票プログラム選択情報
           COPY  "CPSK1032.INC".
      *
           COPY  "CPSK1041.INC".
      *
           COPY  "CPSK5010.INC".
           COPY  "CPSK5013.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    日別収納マスター
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    収納明細マスタ−
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   30.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  WRK-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                    //WKSRY-// BY //WRK-WKSRY-//.
      *
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC        OCCURS   10.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *    患者定期請求履歴
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY  "CPBUNBEN.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入院登録サブ
           COPY    "CPI0SUB01.INC".
      *
      *    分娩負担金サブ
       01  LNKBUNBEN-REC.
           COPY  "CPBUNBEN.INC"    REPLACING  //BUNBEN-//
                                           BY //LNKBUNBEN-//.
      *
      *    定期請求差額計算サブ
           COPY  "CPORCSTEIKISGK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    帳票サブ < 請求書兼領収書 >
           COPY    "CPORCHCN03.INC".
      *
      *    帳票サブ < 診療費明細書 >
           COPY    "CPORCHCN04.INC".
      *
      *    帳票サブ < 退院証明書 >
       01  LNK-PTNYUINRRK.
           COPY    "CPPTNYUINRRK.INC"
               REPLACING   //PTNYUINRRK-//
               BY          //LNK-PTNYUINRRK-//.
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    定期請求ジョブチェック
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
      *    入院診療行為照会用テーブル編集サブ
           COPY  "CPORCSS010.INC".
      *
      *    まとめ入金編集サブ
           COPY    "CPORCSNYUKINS.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    入院院外処方せんパラメタ領域
           COPY    "CPORCSNYUINSHO.INC".
      *
      *    分娩入院チェックサブ
           COPY  "CPORCSBUNBENCHK.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    請求書兼領収書まとめサブ
           COPY    "CPORCHCN03S02.INC".
      *
      *    入退院登録PUSH通知
           COPY    "CPORCSI03.INC".
      *
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA31SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I0ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "I04"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
           WHEN    "I0ID1"
      *        確認画面より
               PERFORM 3001-I0ID1-SET-SEC
               MOVE   SPACE        TO  SPA-I0IDCD
               GO  TO      300-SCREEN-EXT
           WHEN    "I041"
      *        院外処方せんドクター設定画面より
      *        院外処方せん発行ありとする
               IF      SPA-GMN-I041-FLG    =   1
                   MOVE    SPA-GMN-I04-SHOHOU-ITM (2)
                                       TO  SPA-GMN-I04-SHOHOU
               END-IF
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-I0IDCD
      *
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-GMN-I04-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｉ５ＩＤ１）ＯＫ処理
      *****************************************************************
       3001-I0ID1-SET-SEC         SECTION.
      *
           IF    ( SPA-I0ID1-FLG   =   "OK" )
               EVALUATE    SPA-I0IDCD
               WHEN    "3001"
                   PERFORM 452-TOUROKU-SEC
               WHEN    "3002"
                   PERFORM 452-TOUROKU-SEC
               END-EVALUATE
           END-IF
           .
       3001-I0ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    "1"             TO  SPA-NYUGAIKBN
      *
           MOVE    SPACE           TO  SPA-I04
           INITIALIZE                  SPA-I04
      *!!
           MOVE    SPACE           TO  SPA-I041
           INITIALIZE                  SPA-I041
      *
      *    引継ぎ項目設定処理
           PERFORM 3100-SYOKI-SET-SEC
      *
      *    コンボボックス初期化
           PERFORM 3100-CMB-SYOKI-SEC
      *
      *    患者マスタ検索
           PERFORM 900-PTINF-KEY-SEL-SEC
           MOVE    PTINF-SKYPRTFLG TO  SPA-NAI-I04-PTSKYPRT
      *
      *    システム管理マスタ検索処理
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5000"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-NAI-I04-TAIINYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5000-REC
      *
           MOVE    SYS-5000-SKYSUMKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKYSUMKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SKYSUMKBN-ITM (1)
                                   TO  SPA-GMN-I04-SKYSUMKBN
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SKYSUMKBN
           END-IF
      *
           IF    ( SPA-I01-I04-PROCKBN      =   "1" )
               IF    ( SYS-5000-TAIPRTKBN   =   "0" )
                   MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (01)
                                       TO  SPA-GMN-I04-TAIPRTOUT
               ELSE
                   MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (02)
                                       TO  SPA-GMN-I04-TAIPRTOUT
               END-IF
           ELSE
                   MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (01)
                                       TO  SPA-GMN-I04-TAIPRTOUT
           END-IF
      *
      *    入金方法初期値
           MOVE    PTINF-NYUKIN-HOHO   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYKNHOHO-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "01"        TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYKNHOHO-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I04-NYKNHOHO
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I04-NYKNJYOTAI
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I04-NYKNHOHO
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I04-NYKNJYOTAI
           END-IF
      *
      *    入金の取扱い初期値
           MOVE    SYS-5000-NYKNKNRKBN TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYKNKNRKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "1"        TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYKNKNRKBN-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I04-NYKNKNRKBN
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I04-NYKNKNRKBN
           END-IF
      *
           MOVE    SPA-GMN-I04-NYKNKNRKBN-CD
                                       TO  SPA-GMN-I04-NYKNKNRKBN-BF
      *
      *    主治医氏名初期値
           MOVE    SPA-GMN-I01-DR-CD (1)
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DR-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-DR-ITM (1)
                                   TO  SPA-GMN-I04-DR
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-DR
           END-IF
      *    退院時院外処方せん発行
           EVALUATE    SYS-5000-SHOHOUKBN
               WHEN    "1"
                   MOVE    SPA-GMN-I04-SHOHOU-ITM (2)
                                           TO  SPA-GMN-I04-SHOHOU
               WHEN    OTHER
                   MOVE    SPA-GMN-I04-SHOHOU-ITM (1)
                                           TO  SPA-GMN-I04-SHOHOU
           END-EVALUATE
      *
      *    発効日
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-I04-HAKYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-I04-HAKYMD
      *
           IF    ( SPA-I01-I04-PROCKBN     =  "2" )
               IF    ( SYS-5000-SAIKEISANKBN   =   "1" )
                   MOVE    1           TO  SPA-NAI-I04-SAIKEISANFLG
               ELSE
                   MOVE    ZERO        TO  SPA-NAI-I04-SAIKEISANFLG
               END-IF
           END-IF
      *
           PERFORM 3100-SEIKYU-DAT-INIT-SEC
      *
      *    未収データ取得処理
           PERFORM 3100-MISYU-DAT-EDIT-SEC
      *
      *    入金額編集
           IF    ( SPA-NAI-I04-NYKNJYOTAI  =   "1" )
               PERFORM 3100-NYUKIN-DAT-EDIT-SEC
           END-IF
      *
      *    合計行編集処理
           PERFORM 4101-TTL-LINE-EDIT-SEC
      *
      *    保険組み合わせリスト編集処理
           PERFORM 3100-HKNCOMBILST-EDIT-SEC
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX(SPA-GMN-I04-SELNUM)
                                   TO  WRK-LST-IDX
      *    請求データ編集処理
           PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
      **   定期請求差額初期表示
           IF    ( SYS-5000-TEIKISAGAKUACTKBN  =   "1" )
               MOVE    CONST-EDIT-TEIKI    TO  SPA-NAI-I04-RSITEIKIFLG
               PERFORM 31001-TEIKISAGAKU-HEN-SEC
           ELSE
               MOVE    CONST-EDIT-RSIJBI   TO  SPA-NAI-I04-RSITEIKIFLG
           END-IF
      *
           MOVE    SYS-5000-SKYPRTKBN
                                   TO  SPA-GMN-I04-SKYPRTOUT
      *
           MOVE    SYS-5000-SRYPRTKBN
                                   TO  SPA-GMN-I04-SRYPRTOUT
      *
           MOVE    ZERO            TO  IDX1
           PERFORM VARYING IDX8    FROM    1   BY  1
                   UNTIL ( IDX8    >   CONST-PRTDEF-MAX )
                    OR   ( IDX1    >   ZERO )
      *
               IF    ( CONST-PRTDEF-PRTFLG (IDX8)
                                   =   SPA-NAI-I04-PTSKYPRT )
                   MOVE    IDX8    TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX1        NOT =  ZERO )
      *
               IF    ( SPA-I01-I04-PROCKBN         =   "2" )
                AND  ( SPA-NAI-I04-SAIKEISANFLG    =   1 )
                   MOVE    2       TO  IDX2
               ELSE
                   MOVE    1       TO  IDX2
               END-IF
      *
               IF    ( SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                                   >   ZERO )
                   MOVE    1       TO  IDX3
               ELSE
                   MOVE    2       TO  IDX3
               END-IF
      *
               MOVE    CONST-SKYPRTOUT(IDX1 IDX2 IDX3)
                                   TO  SPA-GMN-I04-SKYPRTOUT
               MOVE    CONST-SRYPRTOUT(IDX1 IDX2 IDX3)
                                   TO  SPA-GMN-I04-SRYPRTOUT
      *
           END-IF
      *
           MOVE    SPA-GMN-I04-SKYPRTOUT
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKYPRTOUT-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SKYPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SKYPRTOUT
           END-IF
      *
           MOVE    SPA-GMN-I04-SRYPRTOUT
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SRYPRTOUT-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SRYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SRYPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SRYPRTOUT
           END-IF
      *
            .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    引継ぎ項目設定処理
      *****************************************************************
       3100-SYOKI-SET-SEC              SECTION.
      *
      *    患者ＩＤ
           MOVE    SPA-NAI-I01-PTID    TO  SPA-NAI-I04-PTID
      *
      *    入院履歴番号
           MOVE    SPA-I01-I04-RRKNUM
                                       TO  SPA-NAI-I04-PTRRKNUM
      *    入院履歴枝番号
           MOVE    SPA-I01-I04-RRKEDANUM
                                       TO  SPA-NAI-I04-PTRRKEDANUM
      *    患者番号
           MOVE    SPA-GMN-I01-PTNUM   TO  SPA-GMN-I04-PTNUM
      *    患者カナ氏名
           MOVE    SPA-NAI-I01-KANANAME
                                       TO  SPA-GMN-I04-KANANAME
      *    患者氏名
           MOVE    SPA-GMN-I01-NAME    TO  SPA-GMN-I04-NAME
      *    患者性別
           MOVE    SPA-GMN-I01-SEX     TO  SPA-GMN-I04-SEX
      *    患者誕生日
           MOVE    SPA-GMN-I01-BIRTHDAY
                                       TO  SPA-GMN-I04-BIRTHDAY
      *
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  SPA-NAI-I04-BIRTHDAY
      *
      *    患者年齢
           MOVE    SPA-GMN-I01-AGE     TO  SPA-GMN-I04-AGE
      *
      *    保険組み合わせ
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           INITIALIZE                      WRK-SPA-KYOTUKEY
                                           WRK-SPA-KYOTU-SET
                                           ORCSHKNSETAREA
      *
           MOVE    SPA-I01-I04-KJNYMD  TO  WRK-SPA-SRYYMD
           MOVE    SPA-I01-I04-NYUINKA TO  WRK-SPA-SRYKA
           MOVE    "1"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  WRK-SPA-PTID
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  WRK-SPA-BIRTHDAY
           MOVE    SPA-I01-I04-HKNCOMBI
                                       TO  WRK-SPA-HKNCOMBINUM
           MOVE    "1"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           IF    ( ORCSHKN-IDX         >   ZERO )
            AND  ( ORCSHKN-IDX         <=  ORCSHKN-HKN-MAX )
               IF    ( ORCSHKN-NAI-HKNJANUM (ORCSHKN-IDX) =   SPACE )
                   MOVE    ORCSHKN-GMN-HKNCOMBIMEI (ORCSHKN-IDX)
                                       TO  WRK-HKNCOMBI
               ELSE
                   MOVE    SPACE       TO  WRK-HKNCOMBI
                   STRING  ORCSHKN-GMN-HKNCOMBIMEI (ORCSHKN-IDX)
                                               DELIMITED BY  "  "
                       " ("                    DELIMITED  BY  SIZE
                       ORCSHKN-NAI-HKNJANUM (ORCSHKN-IDX)
                                               DELIMITED  BY  SPACE
                       ")"                     DELIMITED  BY  SIZE
                   INTO    WRK-HKNCOMBI
                   END-STRING
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HKNCOMBI        TO  KANACHK-MAE-INPUT
                   MOVE    60                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT
                                       TO  WRK-HKNCOMBI
               END-IF
               MOVE    WRK-HKNCOMBI    TO  SPA-GMN-I04-HKNCOMBI
               MOVE    ORCSHKN-GMN-FTNRATEMEI (ORCSHKN-IDX)
                                       TO  SPA-GMN-I04-FTNRATE
           END-IF
      *
      *    入院科
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-I01-I04-NYUINKA TO  SYS-1005-KBNCD
           MOVE    SPA-I01-I04-KJNYMD  TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           MOVE    SYS-1005-SRYKANAME  TO  SPA-GMN-I04-NYUINKA
      *
      *    入院日
           MOVE    SPA-I01-I04-NYUINYMD
                                       TO  SPA-NAI-I04-NYUINYMD
           MOVE    SPA-I01-I04-NYUINYMD
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-I04-NYUINYMD
      *
      *    退院日
           MOVE    SPA-I01-I04-TAIINYMD
                                       TO  SPA-NAI-I04-TAIINYMD
           MOVE    SPA-I01-I04-TAIINYMD
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-I04-TAIINYMD
      *
      *    退院事由
           INITIALIZE                      SYS-5013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5013-HOSPNUM
           MOVE    "5013"              TO  SYS-5013-KANRICD
           MOVE    SPA-I01-I04-TAIINCD TO  SYS-5013-KBNCD
           MOVE    SPA-I01-I04-TAIINYMD
                                       TO  SYS-5013-STYUKYMD
                                           SYS-5013-EDYUKYMD
           MOVE    SYS-5013-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5013-REC
      *
           MOVE    SYS-5013-TANNAME    TO  SPA-GMN-I04-TAIINCD
      *
           MOVE    SPA-I01-I04-TAIINCD TO  SPA-NAI-I04-TAIINCD
      *
           MOVE    1                   TO  SPA-NAI-I04-SAIKEISANFLG
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5010"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-I01-I04-SKYSTYMD TO SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYSKANRI-REC        TO  SYS-5010-REC
      *
               EVALUATE    TRUE
               WHEN    SYS-5010-SEIKYU-DAY (1) =   99
                   MOVE    "1" TO  SPA-NAI-I04-SAKKBN-MAX
               WHEN    SYS-5010-SEIKYU-DAY (2) =   99
                   MOVE    "2" TO  SPA-NAI-I04-SAKKBN-MAX
               WHEN    OTHER
                   MOVE    "3" TO  SPA-NAI-I04-SAKKBN-MAX
               END-EVALUATE
           END-IF
      *
            .
       3100-SYOKI-SET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求差額編集処理
      *****************************************************************
       31001-TEIKISAGAKU-HEN-SEC       SECTION.
      *
           IF    ( SPA-NAI-I04-TEIKIKEISANFLG
                                       =   ZERO )
               PERFORM 3101-KBT-HEN-SEC
               PERFORM 3101-KBT-SAGAKU-HEN-SEC
      *
               MOVE    1           TO  SPA-NAI-I04-TEIKIKEISANFLG
      *
           END-IF
      *
      *
            .
       31001-TEIKISAGAKU-HEN-EXT.
           EXIT.
      *************************************************************
      *    期別編集処理
      *************************************************************
       3101-KBT-HEN-SEC         SECTION.
      *
           INITIALIZE                      SPA-GMN-I04-KBT-AREA
      *
           MOVE    SPA-I01-I04-SKYSTYMD    TO  WRK-YMD
           MOVE    "1"                     TO  WRK-ZOGENPTN
           MOVE    -1                      TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD           TO  WRK-LASTTEIKIEDYMD
      *
           MOVE    WRK-LASTTEIKIEDYMD      TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           STRING  WRK-EDTYMD3 (11:6)      DELIMITED   BY  SIZE
                   "定期請求差額"          DELIMITED   BY  SIZE
           INTO    SPA-GMN-I04-KBT-TITLE
           END-STRING
      *
           INITIALIZE                          STEIKISGK-AREA
           MOVE    SPA-NAI-I04-PTID        TO  STEIKISGK-PTID
           MOVE    WRK-LASTTEIKIEDYMD      TO  STEIKISGK-SRYYMD
           MOVE    SPA-NAI-I04-PTRRKNUM    TO  STEIKISGK-RRKNUM
           IF    ( WRK-LASTTEIKIEDYMD (1:6)
                                      =    SPA-I01-I04-SKYSTYMD (1:6))
               MOVE    SPA-I01-I04-SKYSTYMD
                                           TO  STEIKISGK-TAIIN-SKYSTYMD
               MOVE    SPA-I01-I04-SKYEDYMD
                                           TO  STEIKISGK-TAIIN-SKYEDYMD
           END-IF
           CALL    "ORCSTEIKISGK"          USING
                                           STEIKISGK-AREA
                                           SPA-AREA
           IF    ( STEIKISGK-RC        =   ZERO )
      *
               MOVE    STEIKISGK-SAGAKU(1)
                                       TO  SPA-GMN-I04-SKY1ST
               MOVE    STEIKISGK-SAGAKU(2)
                                       TO  SPA-GMN-I04-SKY2ND
               MOVE    STEIKISGK-SAGAKU(3)
                                       TO  SPA-GMN-I04-SKY3RD
               MOVE    STEIKISGK-TAIIN-SKYMONEY
                                       TO  SPA-GMN-I04-FTNTAIIN
           END-IF
      *
           .
       3101-KBT-HEN-EXT.
           EXIT.
      *************************************************************
      *    期別差額編集処理
      *************************************************************
       3101-KBT-SAGAKU-HEN-SEC         SECTION.
      *
           IF    ( WRK-LASTTEIKIEDYMD (1:6)
                                      =    SPA-I01-I04-SKYSTYMD (1:6))
      *
             PERFORM VARYING IDX-SYU FROM    1   BY  1
                   UNTIL ( IDX-SYU >   SPA-I0-SYUNOU-CNT )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-SYU)
                                   =   1 )
                AND  ( SPA-I0-SYUNOU-YM-S (IDX-SYU)
                                   =   SPA-I01-I04-SKYSTYMD (1:6))
                   MOVE    SPA-I0-SYUNOU-DAT (IDX-SYU)
                                   TO  WKSYUNOU-REC
                   IF    ( SPA-I01-I04-PROCKBN         =   "2" )
                    AND  ( SPA-NAI-I04-SAIKEISANFLG    =   1 )
                       COMPUTE SPA-GMN-I04-SKYTAIIN
                            =  STEIKISGK-TAIIN-SKYMONEY
                            -  WKSYU-SKYMONEY
                   ELSE
                       COMPUTE SPA-GMN-I04-SKYTAIIN
                            =  SPA-GMN-I04-FTNTAIIN
                            -( WKSYU-SKYMONEY
                            -  WKSYU-CHOSEI )
                   END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           COMPUTE SPA-GMN-I04-SKYSGK
               =   SPA-GMN-I04-SKY1ST
               +   SPA-GMN-I04-SKY2ND
               +   SPA-GMN-I04-SKY3RD
               +   SPA-GMN-I04-SKYTAIIN
      *
           .
       3101-KBT-SAGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期化
      *****************************************************************
       3100-CMB-SYOKI-SEC              SECTION.
      *
      *
           INITIALIZE                  SPA-GMN-I04-CMB-DAT-AREA
      *
      *    入金方法コンボ
           PERFORM 3100-CMB-NYKNHOHO-SET-SEC
      *
      *    入金の取扱い
           PERFORM 3100-CMB-NYKNKNRKBN-SET-SEC
      *
      *    請求書兼領収書発行コンボ
           PERFORM 3100-CMB-SKYPRTOUT-SET-SEC
      *
      *    請求書兼領収書発行方法コンボ
           PERFORM 3100-CMB-SKYSUMKBN-SET-SEC
      *
      *    診療費明細書発行コンボ
           PERFORM 3100-CMB-SRYPRTOUT-SET-SEC
      *
      *    退院証明書発行コンボ
           PERFORM 3100-CMB-TAIPRTOUT-SET-SEC
      *
      *    主治医氏名コンボ
           PERFORM 3100-CMB-DR-SET-SEC
      *
      *    ユーザープログラムコンボ
           PERFORM 3100-CMB-USERPG-SET-SEC
      *!!
      *    院外処方せんコンボ
           PERFORM 3100-CMB-SHOHOU-SET-SEC
      *
            .
       3100-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法コンボ
      *****************************************************************
       3100-CMB-NYKNHOHO-SET-SEC       SECTION.
      *
      *    入金方法
           INITIALIZE                      SYS-1041-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1041-STYUKYMD
                                           SYS-1041-EDYUKYMD
           MOVE    SYS-1041-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1041-REC
      *
           MOVE    ZERO                TO  IDX0
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX0           >=   99 )
      *
               COMPUTE IDX0    =   IDX0    +   1
      *
               MOVE    SYS-1041-KBNCD
                                   TO  SPA-GMN-I04-NYKNHOHO-ITM (IDX0)
                                                                (1:2)
               MOVE    SYS-1041-NYKN-NAME
                                   TO  SPA-GMN-I04-NYKNHOHO-ITM (IDX0)
                                                                (4:)
               IF    ( SYS-1041-NYKN-JYOTAI-N  NOT =   SPACE )
                   MOVE    SYS-1041-NYKN-JYOTAI-N
                                   TO  SPA-GMN-I04-NYKNJYOTAI-ITM (IDX0)
               ELSE
                   MOVE    "2"     TO  SPA-GMN-I04-NYKNJYOTAI-ITM (IDX0)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC        TO  SYS-1041-REC
      *
           END-PERFORM
      *
           MOVE    IDX0                TO  SPA-GMN-I04-NYKNHOHO-CNT
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
            .
       3100-CMB-NYKNHOHO-SET-EXT.
           EXIT.
      *****************************************************************
      *    入金の取扱い
      *****************************************************************
       3100-CMB-NYKNKNRKBN-SET-SEC     SECTION.
      *
           IF    ( SPA-I01-I04-PROCKBN =   "2" )
      *
               MOVE    1           TO  SPA-GMN-I04-NYKNKNRKBN-CNT
               MOVE    "1 今回請求分のみ入金"
                                   TO  SPA-GMN-I04-NYKNKNRKBN-ITM (1)
      *
           ELSE
      *
               MOVE    5           TO  SPA-GMN-I04-NYKNKNRKBN-CNT
               MOVE    "1 今回請求分のみ入金"
                                   TO  SPA-GMN-I04-NYKNKNRKBN-ITM (1)
               MOVE    "2 今回分・伝票の古い未収順に入金"
                                   TO  SPA-GMN-I04-NYKNKNRKBN-ITM (2)
               MOVE    "3 今回分・伝票の新しい未収順に入金"
                                   TO  SPA-GMN-I04-NYKNKNRKBN-ITM (3)
               MOVE    "4 伝票の古い未収順に入金"
                                   TO  SPA-GMN-I04-NYKNKNRKBN-ITM (4)
               MOVE    "5 伝票の新しい未収順に入金"
                                   TO  SPA-GMN-I04-NYKNKNRKBN-ITM (5)
           END-IF
      *
            .
       3100-CMB-NYKNKNRKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行コンボ
      *****************************************************************
       3100-CMB-SKYPRTOUT-SET-SEC      SECTION.
      *
           MOVE    3               TO  SPA-GMN-I04-SKYPRTOUT-CNT
           MOVE    "0:発行しない"  TO  SPA-GMN-I04-SKYPRTOUT-ITM (01)
           MOVE    "1:発行する"    TO  SPA-GMN-I04-SKYPRTOUT-ITM (02)
           MOVE    "2:発行する（請求あり）"  
                                   TO  SPA-GMN-I04-SKYPRTOUT-ITM (03)
      *
            .
       3100-CMB-SKYPRTOUT-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行方法コンボ
      *****************************************************************
       3100-CMB-SKYSUMKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-I01-I04-SKYSTYMD    <   CONST-H200401 )
            AND  ( SPA-I01-I04-SKYEDYMD    >=  CONST-H200401 )
               MOVE    2           TO  SPA-GMN-I04-SKYSUMKBN-CNT
               MOVE    "0:個別に発行する"
                                   TO  SPA-GMN-I04-SKYSUMKBN-ITM (01)
               MOVE    "1:月別に発行する"
                                   TO  SPA-GMN-I04-SKYSUMKBN-ITM (02)
           ELSE
               MOVE    4           TO  SPA-GMN-I04-SKYSUMKBN-CNT
               MOVE    "0:個別に発行する"
                                   TO  SPA-GMN-I04-SKYSUMKBN-ITM (01)
               MOVE    "1:月別に発行する"
                                   TO  SPA-GMN-I04-SKYSUMKBN-ITM (02)
               MOVE    "2:保険組合せ別に発行する"  
                                   TO  SPA-GMN-I04-SKYSUMKBN-ITM (03)
               MOVE    "3:全体をまとめて発行する"  
                                   TO  SPA-GMN-I04-SKYSUMKBN-ITM (04)
           END-IF
      *
            .
       3100-CMB-SKYSUMKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行コンボ
      *****************************************************************
       3100-CMB-SRYPRTOUT-SET-SEC      SECTION.
      *
           MOVE    3               TO  SPA-GMN-I04-SRYPRTOUT-CNT
           MOVE    "0:発行しない"  TO  SPA-GMN-I04-SRYPRTOUT-ITM (01)
           MOVE    "1:発行する"    TO  SPA-GMN-I04-SRYPRTOUT-ITM (02)
           MOVE    "2:発行する（請求あり）"  
                                   TO  SPA-GMN-I04-SRYPRTOUT-ITM (03)
      *
            .
       3100-CMB-SRYPRTOUT-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行コンボ
      *****************************************************************
       3100-CMB-TAIPRTOUT-SET-SEC      SECTION.
      *
           MOVE    2               TO  SPA-GMN-I04-TAIPRTOUT-CNT
           MOVE    "0:発行しない"  TO  SPA-GMN-I04-TAIPRTOUT-ITM (01)
           MOVE    "1:発行する"    TO  SPA-GMN-I04-TAIPRTOUT-ITM (02)
      *
            .
       3100-CMB-TAIPRTOUT-SET-EXT.
           EXIT.
      *****************************************************************
      *    主治医氏名コンボ
      *****************************************************************
       3100-CMB-DR-SET-SEC             SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    "1%"            TO  SYS-KBNCD
           MOVE    SPA-NAI-I04-TAIINYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY9-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   99 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX1    TO  SPA-GMN-I04-DR-CNT
               MOVE    SYS-1010-KBNCD (2 : 4)
                           TO  SPA-GMN-I04-DR-ITM(IDX1)(1 : 4)
               MOVE    SYS-1010-NAME
                           TO  SPA-GMN-I04-DR-ITM(IDX1)(6 : )
      *
               PERFORM 900-SYSKANRI-KEY9-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
            .
       3100-CMB-DR-SET-EXT.
           EXIT.
      *****************************************************************
      *    ユーザープログラムコンボ初期処理
      *****************************************************************
       3100-CMB-USERPG-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I04-CMB-USERPG
      *
           MOVE    1           TO  SPA-GMN-I04-USERPG-CNT
      *
           MOVE    "0"         TO  SPA-GMN-I04-USERPGL    (1)
           MOVE    "Ｕ・Ｐ指示なし" 
                               TO  SPA-GMN-I04-USERPGMEIL (1)
      *
           INITIALIZE              ORCSUSERCHKAREA
           MOVE    "I04"       TO  ORCSUSERCHK-GMNPG
           MOVE    "1"         TO  ORCSUSERCHK-SYORI
           CALL    "ORCSUSERCHK"   USING
                                   ORCSUSERCHKAREA
                                   SPA-AREA
           MOVE    ORCSUSERCHK-GYOUMU-FLG
                               TO  SPA-NAI-I04-GYOUMU-FLG
           MOVE    ORCSUSERCHK-GYOUMU-ARI
                               TO  SPA-NAI-I04-GYOUMU-ARI
      *
           IF    ( SPA-NAI-I04-GYOUMU-ARI  =   "1" )
      *
      *        実行するプログラムがある時
               MOVE    2       TO  SPA-GMN-I04-USERPG-CNT
      *
               MOVE    "1"     TO  SPA-GMN-I04-USERPGL  (2)
               MOVE    "Ｕ・Ｐ指示あり" 
                               TO  SPA-GMN-I04-USERPGMEIL (2)
      *
               IF    ( SPA-NAI-I04-GYOUMU-FLG  =   "1" )
                   MOVE    SPA-GMN-I04-USERPG-ITM (2)
                               TO  SPA-GMN-I04-USERPG
               ELSE
                   MOVE    SPA-GMN-I04-USERPG-ITM (1)
                               TO  SPA-GMN-I04-USERPG
               END-IF
           ELSE
               MOVE    SPA-GMN-I04-USERPG-ITM (1)
                               TO  SPA-GMN-I04-USERPG
           END-IF
      *
           .
       3100-CMB-USERPG-SET-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    院外処方せんコンボ得処理
      *****************************************************************
       3100-CMB-SHOHOU-SET-SEC         SECTION.
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-I04-SHOHOU-ITM (1)
           MOVE    "1 発行する"        TO  SPA-GMN-I04-SHOHOU-ITM (2)
           MOVE    2                   TO  SPA-GMN-I04-SHOHOU-CNT
      *
           MOVE    SPA-GMN-I04-SHOHOU-ITM (1)
                                       TO  SPA-GMN-I04-SHOHOU
           .
       3100-CMB-SHOHOU-SET-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    未収データ取得処理
      *****************************************************************
       3100-MISYU-DAT-EDIT-SEC         SECTION.
      *
           MOVE    ZERO                TO   SPA-GMN-I04-ZENMISYU-NYU
                                            SPA-GMN-I04-ZENKANYU-NYU
                                            SPA-GMN-I04-ZENMISYU-GAI
                                            SPA-GMN-I04-LBLSKYINF
      *
      *    入院分未収集計
           INITIALIZE                       SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO   SYU-HOSPNUM
           MOVE    "1"                 TO   SYU-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTID    TO   SYU-PTID
           MOVE    SYUNOU-REC          TO   MCPDATA-REC
           MOVE    "tbl_syunou"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM  910-DBSELECT-SEC
      *
           PERFORM   UNTIL  MCP-RC  NOT =  ZERO
      *
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               IF    ( SPA-I01-I04-SKYSTYMD   <= SYU-SKYSTYMD )
                AND  ( SPA-I01-I04-SKYEDYMD   >= SYU-SKYEDYMD )
                AND  ( SPA-I01-I04-RRKNUM
                                        = SYU-NYUIN-RRKNUM )
                   CONTINUE
               ELSE
                   IF  SYU-SKYEDYMD  <=  SPA-NAI-I04-TAIINYMD
                       IF SYU-SKYMONEY  >   SYU-NYUKIN-TOTAL
                          COMPUTE   SPA-GMN-I04-ZENMISYU-NYU   =
                                    SPA-GMN-I04-ZENMISYU-NYU   +
                                   (SYU-SKYMONEY   -
                                    SYU-NYUKIN-TOTAL)
                       ELSE
                          COMPUTE   SPA-GMN-I04-ZENKANYU-NYU   =
                                    SPA-GMN-I04-ZENKANYU-NYU   +
                                   (SYU-NYUKIN-TOTAL  -
                                    SYU-SKYMONEY  )
                       END-IF
                   END-IF
               END-IF
               MOVE    "tbl_syunou"        TO   MCP-TABLE
               MOVE    "key13"             TO   MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SPA-NAI-I04-SAIKEISANFLG    =   1 )
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU     >    SPA-I0-SYUNOU-CNT )
      *
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-SYU)
                                           =    ZERO )
                     AND ( SPA-I0-SYUNOU-UPDFLG     (IDX-SYU)
                                           =    ZERO )
      *
                       MOVE    SPA-I0-SYUNOU-DAT (IDX-SYU)
                                       TO  SYUNOU-REC
      *
                       COMPUTE WRK-SKYMONEY-BF
                           =   SYU-SKYMONEY
                           -   SPA-I0-UPD-SKYMONEY (IDX-SYU)
                       IF    ( WRK-SKYMONEY-BF
                                       >   SYU-NYUKIN-TOTAL )
                           COMPUTE SPA-GMN-I04-ZENMISYU-NYU
                               =   SPA-GMN-I04-ZENMISYU-NYU
                               + ( WRK-SKYMONEY-BF
                               -   SYU-NYUKIN-TOTAL )
                       ELSE
                           COMPUTE SPA-GMN-I04-ZENKANYU-NYU
                               =   SPA-GMN-I04-ZENKANYU-NYU
                               + ( SYU-NYUKIN-TOTAL
                               -   WRK-SKYMONEY-BF )
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    外来分未収集計
           INITIALIZE                       SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO   SYU-HOSPNUM
           MOVE    "2"                 TO   SYU-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTID    TO   SYU-PTID
           MOVE    SYUNOU-REC          TO   MCPDATA-REC
           MOVE    "tbl_syunou"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM  910-DBSELECT-SEC
      *
           PERFORM   UNTIL  MCP-RC  NOT =  ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               IF      SYU-DENPPRTYMD  <=  SPA-NAI-I04-TAIINYMD
                   IF      SYU-SKYMONEY  >   SYU-NYUKIN-TOTAL
                       COMPUTE   SPA-GMN-I04-ZENMISYU-GAI   =
                                SPA-GMN-I04-ZENMISYU-GAI   +
                                (SYU-SKYMONEY   -
                                 SYU-NYUKIN-TOTAL)
                   END-IF
               END-IF
               MOVE    "tbl_syunou"        TO   MCP-TABLE
               MOVE    "key13"             TO   MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
            .
       3100-MISYU-DAT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入金額編集処理
      *****************************************************************
       3100-NYUKIN-DAT-EDIT-SEC        SECTION.
      *
           MOVE    ZERO            TO  SPA-NAI-I04-NYUKIN
                                       (SPA-I0-SYUNOU-CNT)
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1)
                                       =   ZERO )
      *
                   COMPUTE SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                       =   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                       +   SPA-NAI-I04-SEIKYU-PLUS
                                              (IDX1)
      *
               END-IF
           END-PERFORM
      *
           MOVE    SPA-I0-SYUNOU-CNT   TO  IDX-LST
           MOVE    SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                                       TO  WRK-NYUKIN
      *
           PERFORM 4101-NYUKIN-SORT-SEC
      *
            .
       3100-NYUKIN-DAT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ内部領域初期化処理
      *****************************************************************
       3100-SEIKYU-DAT-INIT-SEC        SECTION.
      *
           INITIALIZE                  SPA-NAI-I04-SEIKYU-AREA
      *
      *    請求データ内部領域を初期化
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
               MOVE    SPA-I0-SYUNOU-DAT (IDX1)
                                       TO  WKSYUNOU-REC
      *
               IF    ( SPA-I01-I04-PROCKBN         =   "2" )
                AND  ( SPA-NAI-I04-SAIKEISANFLG    =   1 )
      *
                   MOVE    WKSYU-SKYMONEY
                                       TO  SPA-NAI-I04-SEIKYU-MOT
                                                   (IDX1)
      *
                   COMPUTE WRK-SKYMONEY
                       =   SPA-I0-UPD-SKYMONEY (IDX1)
               ELSE
                   COMPUTE WRK-SKYMONEY
                       =   WKSYU-SKYMONEY
                       -   WKSYU-CHOSEI
      *
                   MOVE    ZERO        TO  WKSYU-CHOSEI
                                           WKSYU-CHOSEI1
                                           WKSYU-CHOSEI2
               END-IF
      *
      *
               MOVE    WRK-SKYMONEY    TO  SPA-NAI-I04-SEIKYU
                                                   (IDX1)
                                           SPA-NAI-I04-SEIKYU-KON
                                                   (IDX1)
      *
               IF    ( WRK-SKYMONEY    >   ZERO )
                   MOVE    WRK-SKYMONEY TO SPA-NAI-I04-SEIKYU-PLUS
                                                   (IDX1)
               END-IF
      *
               MOVE    WKSYU-CHOSEI1    TO  SPA-NAI-I04-CHOSEI
                                                   (IDX1 1)
                                           SPA-NAI-I04-CHOSEI-MOT
                                                   (IDX1 1)
      *
               MOVE    WKSYU-CHOSEI2    TO  SPA-NAI-I04-CHOSEI
                                                   (IDX1 2)
                                           SPA-NAI-I04-CHOSEI-MOT
                                                   (IDX1 2)
      *        MOVE    WKSYU-SKYMONEY      TO  SPA-NAI-I04-NYUKIN
      *                                            (IDX1)
               IF    ( SPA-NAI-I04-NYKNJYOTAI  =   "1" )
                   IF    ( WRK-SKYMONEY
                                       >=  ZERO )
                       MOVE    WRK-SKYMONEY
                                       TO  SPA-NAI-I04-NYUKIN
                                                   (IDX1)
                   END-IF
               ELSE
                   MOVE    ZERO        TO  SPA-NAI-I04-NYUKIN
                                                   (IDX1)
               END-IF
               MOVE    SPA-NAI-I04-NYUKIN (IDX1)
                                           TO  WRK-Z92
               MOVE    WRK-Z92             TO  SPA-NAI-I04-NYUKIN-X
                                                   (IDX1)
           END-PERFORM
      *
           .
      *
       3100-SEIKYU-DAT-INIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ編集処理
      *****************************************************************
       3100-SEIKYU-DAT-EDIT-SEC        SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-I04-SEIKYU-AREA
           INITIALIZE              SPA-GMN-I04-SEIKYU-AREA
      *
           MOVE    SPA-I0-SYUNOU-TTLDAT-FLG  (WRK-LST-IDX)
                               TO  SPA-GMN-I04-TTLDAT-FLG
      *
      *
           MOVE    SPA-I0-SYUNOU-DAT (WRK-LST-IDX)
                                       TO  WKSYUNOU-REC
      *
           IF    ( WKSYU-HKNTEKKBN     =   "1" )
               MOVE    WKSYU-KOH-FTN   TO  WKSYU-HKNTEKMONEY
               MOVE    ZERO            TO  WKSYU-KOH-FTN
           END-IF
      *
      *    保険料
           PERFORM VARYING     IDX1     FROM    1   BY  1
                   UNTIL     ( IDX1     >   16 )
               MOVE    WKSYU-HKNTEN (IDX1)
                                      TO  SPA-GMN-I04-HKNRYO (IDX1)
      *    保険適用外
               COMPUTE SPA-GMN-I04-TGMONEY (IDX1)
                   =   WKSYU-TGMONEY (IDX1)
                   +   WKSYU-TGMONEY-TAX(IDX1)
           END-PERFORM
      *
           MOVE    WKSYU-HKNTEN (17)   TO  SPA-GMN-I04-HKNRYO (17)
      *
      *    保険適用外
           COMPUTE SPA-GMN-I04-HKNGAI
                   =   WKSYU-TGMONEY (17)
                   +   WKSYU-TGMONEY-TAX (17)
      *
      *    保険適用金額
           MOVE    WKSYU-HKNTEKMONEY       TO  SPA-GMN-I04-HKNTEKI
      *    老人一部負担金
           MOVE    WKSYU-RJN-FTN           TO  SPA-GMN-I04-ROUFTN
           MOVE    WKSYU-KOH-FTN           TO  SPA-GMN-I04-KOHFTN
      *    自費１
           MOVE    WKSYU-JIHI-1            TO  SPA-GMN-I04-JIHI1
           MOVE    WKSYU-JIHI-2            TO  SPA-GMN-I04-JIHI2
           MOVE    WKSYU-JIHI-3            TO  SPA-GMN-I04-JIHI3
           MOVE    WKSYU-JIHI-4            TO  SPA-GMN-I04-JIHI4
           MOVE    WKSYU-JIHI-5            TO  SPA-GMN-I04-JIHI5
           MOVE    WKSYU-JIHI-6            TO  SPA-GMN-I04-JIHI6
           MOVE    WKSYU-JIHI-7            TO  SPA-GMN-I04-JIHI7
           MOVE    WKSYU-JIHI-8            TO  SPA-GMN-I04-JIHI8
           MOVE    WKSYU-JIHI-9            TO  SPA-GMN-I04-JIHI9
           MOVE    WKSYU-JIHI-10           TO  SPA-GMN-I04-JIHI10
      *    自費計
           MOVE    WKSYU-JIHI-TOTAL        TO  SPA-GMN-I04-JIHIKEI
      *    自費（消費税あり）
           MOVE    WKSYU-JIHI-1-TAX        TO  SPA-GMN-I04-JIHITAX1
           MOVE    WKSYU-JIHI-2-TAX        TO  SPA-GMN-I04-JIHITAX2
           MOVE    WKSYU-JIHI-3-TAX        TO  SPA-GMN-I04-JIHITAX3
           MOVE    WKSYU-JIHI-4-TAX        TO  SPA-GMN-I04-JIHITAX4
           MOVE    WKSYU-JIHI-5-TAX        TO  SPA-GMN-I04-JIHITAX5
           MOVE    WKSYU-JIHI-6-TAX        TO  SPA-GMN-I04-JIHITAX6
           MOVE    WKSYU-JIHI-7-TAX        TO  SPA-GMN-I04-JIHITAX7
           MOVE    WKSYU-JIHI-8-TAX        TO  SPA-GMN-I04-JIHITAX8
           MOVE    WKSYU-JIHI-9-TAX        TO  SPA-GMN-I04-JIHITAX9
           MOVE    WKSYU-JIHI-10-TAX       TO  SPA-GMN-I04-JIHITAX10
      *    自費計（消費税あり）
           MOVE    WKSYU-JIHI-TOTAL-TAX    TO  SPA-GMN-I04-JIHITAXKEI
      *    自費消費税
           MOVE    WKSYU-JIHI-TAX          TO  SPA-GMN-I04-JIHIZEI
      *    食事負担額
           COMPUTE SPA-GMN-I04-SKYMONEY-SKJ
               =   WKSYU-SKYMONEY-SKJ-KEI
               +   WKSYU-SKYMONEY-SKJ-JIHI-KEI
      *    生活負担額
           COMPUTE SPA-GMN-I04-SKYMONEY-LIFE
               =   WKSYU-SKYMONEY-LIFE-KEI
               +   WKSYU-SKYMONEY-LIFE-JIHI-KEI
      *    食事療養費
           COMPUTE SPA-GMN-I04-RYOYOHI-SKJ
               =   WKSYU-RYOYOHI-SKJ
               +   WKSYU-RYOYOHI-SKJ-JIHI
      *    生活療養費
           COMPUTE SPA-GMN-I04-RYOYOHI-LIFE
               =   WKSYU-RYOYOHI-LIFE
               +   WKSYU-RYOYOHI-LIFE-JIHI
      *
      *    一部負担金
           COMPUTE SPA-GMN-I04-ICHIFTN =   SPA-GMN-I04-ROUFTN
                                       +   SPA-GMN-I04-KOHFTN
                                       +   SPA-GMN-I04-SKYMONEY-SKJ
                                       +   SPA-GMN-I04-SKYMONEY-LIFE
      *
      *    労災初診
           MOVE    WKSYU-RSISHOSHIN-MONEY  TO  SPA-GMN-I04-ROUSYOSIN
      *    労災再診
           MOVE    WKSYU-RSISAISHIN-MONEY  TO  SPA-GMN-I04-ROUSAISIN
      *    労災指導
           MOVE    WKSYU-RSISDO-MONEY      TO  SPA-GMN-I04-ROUSIDOU
      *    労災その他
           MOVE    WKSYU-RSIETC-MONEY      TO  SPA-GMN-I04-ROUETC
      *
      *    室料差額
           MOVE    WKSYU-RMSAGAKU          TO  SPA-GMN-I04-RMSAGAKU
      *
      *    調整金
           MOVE    SPA-NAI-I04-CHOSEI (WRK-LST-IDX 1)
                                           TO  SPA-GMN-I04-CHOSEI (1)
                                               WRK-Z9
           MOVE    WRK-Z9                  TO  SPA-GMN-I04-CHOSEI-X (1)
           MOVE    SPA-NAI-I04-CHOSEI (WRK-LST-IDX 2)
                                           TO  SPA-GMN-I04-CHOSEI (2)
                                               WRK-Z9
           MOVE    WRK-Z9                  TO  SPA-GMN-I04-CHOSEI-X (2)
      *    今回請求額
           MOVE    SPA-NAI-I04-SEIKYU (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-SEIKYU
      *
      *    入金額
           MOVE    SPA-NAI-I04-NYUKIN (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-NYUKIN
           MOVE    SPA-NAI-I04-NYUKIN-X (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-NYUKIN-X
      *
      *
      *    合計請求額
           COMPUTE SPA-GMN-I04-SEIGOK  =   SPA-NAI-I04-SEIKYU-PLUS
                                                (SPA-I0-SYUNOU-CNT)
                                       +   SPA-GMN-I04-ZENMISYU-NYU
      *
      *    合計未収額
           COMPUTE SPA-GMN-I04-MISYUGOK  =   SPA-NAI-I04-SEIKYU
                                                (SPA-I0-SYUNOU-CNT)
                                       +   SPA-GMN-I04-ZENMISYU-NYU
                                       -   SPA-GMN-I04-ZENKANYU-NYU
                                       -   SPA-NAI-I04-NYUKIN
                                                (SPA-I0-SYUNOU-CNT)
      *
      *    自費名称編集
           PERFORM 3100-JIHIMSG-HEN-SEC
      *
           MOVE    ZERO                TO  FLG-TEISEIINF
      *
           IF    ( SPA-I01-I04-PROCKBN         =   "2" )
            AND  ( SPA-NAI-I04-SAIKEISANFLG    =   1 )
            AND  ( SPA-I0-UPD-SKYMONEY  (WRK-LST-IDX)
                                           NOT =   ZERO )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG  (WRK-LST-IDX)
                                               =   ZERO )
                    IF    ( SPA-I0-SYUNOU-UPDFLG (WRK-LST-IDX)
                                               = ZERO )
                        MOVE    1      TO  FLG-TEISEIINF
                    END-IF
      *        ELSE
      *                 MOVE    1      TO  FLG-TEISEIINF
               END-IF
      *
               IF    ( SPA-GMN-I04-HKNCOMBILST-CNT     =   1 )
                   PERFORM VARYING IDX0 FROM   1   BY  1
                           UNTIL ( IDX0    >   SPA-I0-SYUNOU-CNT )
                       IF    ( SPA-I0-SYUNOU-TTLDAT-FLG  (IDX0)
                                               =   ZERO )
                            IF    ( SPA-I0-SYUNOU-UPDFLG (IDX0)
                                               = ZERO )
                                MOVE    1      TO  FLG-TEISEIINF
                            END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-IF
      *
           IF    ( FLG-TEISEIINF       =   1 )
      *
               COMPUTE WRK-SKYMONEY
                   =   WKSYU-SKYMONEY
                   -   SPA-I0-UPD-SKYMONEY (WRK-LST-IDX)
               MOVE    WRK-SKYMONEY    TO  WRK-Z99
               MOVE    WRK-Z99         TO  WRK-STR
               PERFORM 800-LTRIM-SEC
               MOVE    WRK-STR         TO  WRK-TEISEIBF
      *
               COMPUTE WRK-SKYMONEY
                   =   WKSYU-SKYMONEY
               MOVE    WRK-SKYMONEY    TO  WRK-Z99
               MOVE    WRK-Z99         TO  WRK-STR
               PERFORM 800-LTRIM-SEC
               MOVE    WRK-STR         TO  WRK-TEISEIAF
               STRING  "訂正前請求額："    DELIMITED   BY  SIZE
                       WRK-TEISEIBF        DELIMITED   BY  SPACE
                       "　今回請求額："    DELIMITED   BY  SIZE
                       WRK-TEISEIAF        DELIMITED   BY  SPACE
               INTO    SPA-GMN-I04-LBLSKYINF
               END-STRING
           END-IF
      *
            .
       3100-SEIKYU-DAT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3100-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    SPA-NAI-I04-TAIINYMD
                                       TO  SYS-1013-STYUKYMD
                                           SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1013-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1) )
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   
                                           TO  SPA-GMN-I04-JIHIMSG (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG(10)
                   END-EVALUATE
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC        TO  SYS-1013-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3100-JIHIMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせリスト編集処理
      *****************************************************************
       3100-HKNCOMBILST-EDIT-SEC       SECTION.
      *
           MOVE    1               TO  FLG-HKNCOMBILST-ROW
      *
           MOVE    ZERO            TO  IDX-HKNCOMBI
                                       IDX-TUKI
                                       CNT-HKNCOMBI
                                       CNT-TUKI
      *
           MOVE    SPACE           TO  SPA-GMN-I04-HKNCOMBILST
                                       SPA-NAI-I04-HKNCOMBILST
      *
           INITIALIZE                  SPA-GMN-I04-HKNCOMBILST
                                       SPA-NAI-I04-HKNCOMBILST
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1) =   1 )
                   IF    ( SPA-I0-SYUNOU-HKNCOMBI-SU (IDX1)    =   1 )
                       COMPUTE IDX2    =   IDX1    +   1
                       COMPUTE IDX-LST =   IDX-LST     +   1
                       MOVE  IDX2  TO  SPA-NAI-I04-TSYUNOUIDX  (IDX-LST)
                       STRING  SPA-I0-SYUNOU-YM-W (IDX2)
                               "　"
                               SPA-I0-SYUNOU-HKNCOMBI (IDX2) (1 : 44 )
                               DELIMITED BY  SIZE
                       INTO    SPA-GMN-I04-THKNCOMBI (IDX-LST)
                       END-STRING
      *
                       COMPUTE CNT-TUKI        =   CNT-TUKI        +   1
                       IF    ( CNT-TUKI        =   1 )
                           MOVE    IDX-LST     TO  IDX-TUKI
                       END-IF
                       COMPUTE CNT-HKNCOMBI    =   CNT-HKNCOMBI    +   1
                       IF    ( CNT-HKNCOMBI    =   1 )
                           MOVE    IDX-LST     TO  IDX-HKNCOMBI
                       END-IF
      *
                   ELSE
                       COMPUTE IDX-LST =   IDX-LST     +   1
                       MOVE  IDX1  TO  SPA-NAI-I04-TSYUNOUIDX  (IDX-LST)
      *
                       MOVE  SPA-I0-SYUNOU-YM-W (IDX1)
                                   TO  SPA-GMN-I04-THKNCOMBI (IDX-LST)
      *
                       COMPUTE CNT-TUKI        =   CNT-TUKI        +   1
                       IF    ( CNT-TUKI        =   1 )
                           MOVE    IDX-LST     TO  IDX-TUKI
                       END-IF
      *
                       COMPUTE WRK-STIDX   =   IDX1    +   1
                       COMPUTE WRK-EDIDX
                           =   IDX1
                           +   SPA-I0-SYUNOU-HKNCOMBI-SU (IDX1)
                       PERFORM VARYING IDX2    FROM WRK-STIDX  BY  1
                               UNTIL ( IDX2    >    WRK-EDIDX )
                           COMPUTE IDX-LST =   IDX-LST     +   1
                           MOVE    IDX2
                                   TO  SPA-NAI-I04-TSYUNOUIDX (IDX-LST)
                           STRING  "　"
                                   SPA-I0-SYUNOU-HKNCOMBI (IDX2)
                                                          (1 : 44 )
                                   DELIMITED BY  SIZE
                           INTO    SPA-GMN-I04-THKNCOMBI (IDX-LST)
                           END-STRING
      *
                           COMPUTE CNT-HKNCOMBI
                               =   CNT-HKNCOMBI    +   1
                           IF    ( CNT-HKNCOMBI    =   1 )
                               MOVE    IDX-LST TO  IDX-HKNCOMBI
                           END-IF
      *
                       END-PERFORM
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    TRUE
           WHEN    ( CNT-HKNCOMBI    =   1 )
               MOVE    SPA-I0-SYUNOU-CNT TO  SPA-NAI-I04-TSYUNOUIDX
                                                       (IDX-HKNCOMBI)
           WHEN    ( CNT-TUKI        =   1 )
               MOVE    SPA-I0-SYUNOU-CNT TO  SPA-NAI-I04-TSYUNOUIDX
                                                       (IDX-TUKI)
           WHEN    OTHER
               COMPUTE IDX-LST =   IDX-LST     +   1
               MOVE    "合計"          TO  SPA-GMN-I04-THKNCOMBI
                                                               (IDX-LST)
               MOVE    SPA-I0-SYUNOU-CNT
                                       TO  SPA-NAI-I04-TSYUNOUIDX
                                                               (IDX-LST)
           END-EVALUATE
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >    IDX-LST )
               MOVE    IDX1        TO  SPA-GMN-I04-HKNCOMBILST-CNT
                                       SPA-GMN-I04-TSELNUM (IDX1)
                                       SPA-NAI-I04-TSELNUM (IDX1)
           END-PERFORM
      *
           IF    ( SPA-I0-SYUNOU-CNT   =   SPA-NAI-I04-TSYUNOUIDX
                                          (SPA-GMN-I04-HKNCOMBILST-CNT))
               MOVE    SPA-GMN-I04-HKNCOMBILST-CNT
                                       TO  SPA-GMN-I04-HKNCOMBILST-SEL
                                           SPA-GMN-I04-SELNUM
           ELSE
               MOVE    1               TO  SPA-GMN-I04-HKNCOMBILST-SEL
                                           SPA-GMN-I04-SELNUM
           END-IF
           
      *
            .
       3100-HKNCOMBILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    I04-HKNCOMBILST-ROW
                                       TO  WRK-HKNCOMBILST-ROW
      *
           MOVE    SPACE               TO  I04
      *
           INITIALIZE                      I04
           EVALUATE    FLG-HKNCOMBILST-ROW
           WHEN    1
               MOVE    ZERO            TO  I04-HKNCOMBILST-ROW
               MOVE    ZERO            TO  I04-HKNCOMBILST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-HKNCOMBILST-ROW
                                       TO  I04-HKNCOMBILST-ROW
               MOVE    ZERO            TO  I04-HKNCOMBILST-ROWATTR
           END-EVALUATE
      *
      *    患者番号
           MOVE    SPA-GMN-I04-PTNUM   TO  I04-PTNUM
      *    カナ氏名
           MOVE    SPA-GMN-I04-KANANAME
                                       TO  I04-KANANAME
      *    氏名
           MOVE    SPA-GMN-I04-NAME    TO  I04-NAME
      *    性別
           MOVE    SPA-GMN-I04-SEX     TO  I04-SEX
      *    保険組み合わせ
           MOVE    SPA-GMN-I04-HKNCOMBI
                                       TO  I04-HKNCOMBI
      *    負担率
           MOVE    SPA-GMN-I04-FTNRATE TO  I04-FTNRATE
      *    生年月日
           MOVE    SPA-GMN-I04-BIRTHDAY
                                       TO  I04-BIRTHDAY
      *    入院科
           MOVE    SPA-GMN-I04-NYUINKA TO  I04-SRYKA
      *    年齢
           MOVE    SPA-GMN-I04-AGE     TO  I04-NENREI
      *
      *    発効日
           MOVE    SPA-GMN-I04-HAKYMD  TO  I04-HAKYMD
      *    入院日
           MOVE    SPA-GMN-I04-NYUINYMD
                                       TO  I04-NYUINYMD
      *    退院日
           MOVE    SPA-GMN-I04-TAIINYMD
                                       TO  I04-TAIINYMD
      *    退院事由
           MOVE    SPA-GMN-I04-TAIINCD
                                       TO  I04-TAIINCD
      *    診療料
           MOVE    SPA-GMN-I04-HKNRYO (01)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SSUHKNTEN
      *    指導料
           MOVE    SPA-GMN-I04-HKNRYO (02)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SDOHKNTEN
      *    在宅料
           MOVE    SPA-GMN-I04-HKNRYO (03)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ZTKHKNTEN
      *    投薬料
           MOVE    SPA-GMN-I04-HKNRYO (04)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-TYKHKNTEN
      *    注射料
           MOVE    SPA-GMN-I04-HKNRYO (05)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-CSYHKNTEN
      *    処置料
           MOVE    SPA-GMN-I04-HKNRYO (06)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SYCHKNTEN
      *    手術料
           MOVE    SPA-GMN-I04-HKNRYO (07)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SJTHKNTEN
      *    麻酔料
           MOVE    SPA-GMN-I04-HKNRYO (08)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-MSIHKNTEN
      *    検査料
           MOVE    SPA-GMN-I04-HKNRYO (09)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-KNSHKNTEN
      *    Ｘ線料
           MOVE    SPA-GMN-I04-HKNRYO (10)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-GZUHKNTEN
      *    リハビリ
           MOVE    SPA-GMN-I04-HKNRYO (11)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ETCHKNTEN
      *    精神科専門
           MOVE    SPA-GMN-I04-HKNRYO (12)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SSNHKNTEN
      *    放射線治療
           MOVE    SPA-GMN-I04-HKNRYO (13)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-HOUHKNTEN
      *    病理診断
           MOVE    SPA-GMN-I04-HKNRYO (14)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-BYRHKNTEN
      *    入院料
           MOVE    SPA-GMN-I04-HKNRYO (15)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-NYUHKNTEN
      *    療養担当手当
           MOVE    SPA-GMN-I04-HKNRYO (16)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RYOHKNTEN
      *    合計点数
           MOVE    SPA-GMN-I04-HKNRYO (17)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-TOTALHKNTEN
      *    保険適用外
      *    診療料
           MOVE    SPA-GMN-I04-TGMONEY (01)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SUUTGMONEY
      *    指導料
           MOVE    SPA-GMN-I04-TGMONEY (02)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SDOTGMONEY
      *    在宅料
           MOVE    SPA-GMN-I04-TGMONEY (03)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ZTKTGMONEY
      *    投薬料
           MOVE    SPA-GMN-I04-TGMONEY (04)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-TYKTGMONEY
      *    注射料
           MOVE    SPA-GMN-I04-TGMONEY (05)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-CSYTGMONEY
      *    処置料
           MOVE    SPA-GMN-I04-TGMONEY (06)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SYCTGMONEY
      *    手術料
           MOVE    SPA-GMN-I04-TGMONEY (07)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SJTGMONEY
      *    麻酔料
           MOVE    SPA-GMN-I04-TGMONEY (08)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-MSITGMONEY
      *    検査料
           MOVE    SPA-GMN-I04-TGMONEY (09)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-KNSTGMONEY
      *    Ｘ線料
           MOVE    SPA-GMN-I04-TGMONEY (10)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-GZUTGMONEY
      *    リハビリ
           MOVE    SPA-GMN-I04-TGMONEY (11)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ETCTGMONEY
      *    精神科専門
           MOVE    SPA-GMN-I04-TGMONEY (12)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SSNTGMONEY
      *    放射線治療
           MOVE    SPA-GMN-I04-TGMONEY (13)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-HOUTGMONEY
      *
      *    病理診断
           MOVE    SPA-GMN-I04-TGMONEY (14)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-BYRTGMONEY
      *
      *    入院料
           MOVE    SPA-GMN-I04-TGMONEY (15)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-NYUTGMONEY
      *    療養担当手当
           MOVE    SPA-GMN-I04-TGMONEY (16)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RYOTGMONEY
      *
      *    自費
           MOVE    SPA-GMN-I04-JIHI1   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI1
           MOVE    SPA-GMN-I04-JIHI2   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI2
           MOVE    SPA-GMN-I04-JIHI3   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI3
           MOVE    SPA-GMN-I04-JIHI4   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI4
           MOVE    SPA-GMN-I04-JIHI5   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI5
           MOVE    SPA-GMN-I04-JIHI6   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI6
           MOVE    SPA-GMN-I04-JIHI7   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI7
           MOVE    SPA-GMN-I04-JIHI8   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI8
           MOVE    SPA-GMN-I04-JIHI9   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI9
           MOVE    SPA-GMN-I04-JIHI10  TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI10
      *    自費計
           MOVE    SPA-GMN-I04-JIHIKEI TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHIGOK
      *    自費（消費税あり）
           MOVE    SPA-GMN-I04-JIHITAX1
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX1
           MOVE    SPA-GMN-I04-JIHITAX2
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX2
           MOVE    SPA-GMN-I04-JIHITAX3
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX3
           MOVE    SPA-GMN-I04-JIHITAX4
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX4
           MOVE    SPA-GMN-I04-JIHITAX5
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX5
           MOVE    SPA-GMN-I04-JIHITAX6
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX6
           MOVE    SPA-GMN-I04-JIHITAX7
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX7
           MOVE    SPA-GMN-I04-JIHITAX8
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX8
           MOVE    SPA-GMN-I04-JIHITAX9
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX9
           MOVE    SPA-GMN-I04-JIHITAX10
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX10
      *    自費計
           MOVE    SPA-GMN-I04-JIHITAXKEI  TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAXGOK
      *    自費税
           MOVE    SPA-GMN-I04-JIHIZEI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX
      *    食事療養費
           MOVE    SPA-GMN-I04-RYOYOHI-SKJ TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RYOYOHI-SKJ
      *    生活療養費
           MOVE    SPA-GMN-I04-RYOYOHI-LIFE TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RYOYOHI-LIFE
      *    食事負担額
           MOVE    SPA-GMN-I04-SKYMONEY-SKJ
                                           TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SKYMONEY-SKJ
      *    生活負担額
           MOVE    SPA-GMN-I04-SKYMONEY-LIFE
                                           TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SKYMONEY-LIFE
      *    老人一部負担金
           MOVE    SPA-GMN-I04-ROUFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RJNFTN
      *    公費一部負担金
           MOVE    SPA-GMN-I04-KOHFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-KOHFTN
      *    一部負担金計
           MOVE    SPA-GMN-I04-ICHIFTN     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-FTNTOTAL
      *    保険適用分
           MOVE    SPA-GMN-I04-HKNTEKI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-HKNTEKMONEY
      *    保険適用外
           MOVE    SPA-GMN-I04-HKNGAI      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-HKNTEKGAI
      *    今回請求額
           MOVE    SPA-GMN-I04-SEIKYU  TO  WRK-Z99
           MOVE    WRK-Z99             TO  I04-SKYMONEY
      *
      *    前回未収（外来）
           MOVE    "black"             TO  I04-MISYUZAN-GAI-STYLE
           IF  (  SPA-GMN-I04-ZENMISYU-GAI NOT =   ZERO )
               MOVE    "red"           TO  I04-MISYUZAN-GAI-STYLE
               MOVE    SPA-GMN-I04-ZENMISYU-GAI
                                       TO  WRK-Z99
               MOVE    WRK-Z99         TO  WRK-STR
               PERFORM 800-LTRIM-SEC
               STRING  "前回までの未収額（外来）：" 
                                           DELIMITED   BY  SIZE
                       WRK-STR             DELIMITED   BY  SPACE
               INTO    I04-MISYUZAN-GAI
               END-STRING
           END-IF
      *
      *    前回未収（入院）
           MOVE    "blue"              TO  I04-MISYUZAN-NYU-STYLE
           MOVE    SPA-GMN-I04-ZENMISYU-NYU
                                       TO  WRK-Z92
           MOVE    WRK-Z92             TO  I04-MISYUZAN-NYU
      *
      *    前回過入金（入院）
           MOVE    "red"               TO  I04-KANYUZAN-NYU-STYLE
           MOVE    SPA-GMN-I04-ZENKANYU-NYU
                                       TO  WRK-Z92
           MOVE    WRK-Z92             TO  I04-KANYUZAN-NYU
      *    合計未収
           MOVE    SPA-GMN-I04-MISYUGOK TO WRK-Z99
           MOVE    WRK-Z99             TO  I04-GOKSKY
      *
      *
      *    自費名称
           MOVE    SPA-GMN-I04-JIHIMSG(1)
                                       TO  I04-JIHIMSG1
           MOVE    SPA-GMN-I04-JIHIMSG(2)
                                       TO  I04-JIHIMSG2
           MOVE    SPA-GMN-I04-JIHIMSG(3)
                                       TO  I04-JIHIMSG3
           MOVE    SPA-GMN-I04-JIHIMSG(4)
                                       TO  I04-JIHIMSG4
           MOVE    SPA-GMN-I04-JIHIMSG(5)
                                       TO  I04-JIHIMSG5
           MOVE    SPA-GMN-I04-JIHIMSG(6)
                                       TO  I04-JIHIMSG6
           MOVE    SPA-GMN-I04-JIHIMSG(7)
                                       TO  I04-JIHIMSG7
           MOVE    SPA-GMN-I04-JIHIMSG(8)
                                       TO  I04-JIHIMSG8
           MOVE    SPA-GMN-I04-JIHIMSG(9)
                                       TO  I04-JIHIMSG9
           MOVE    SPA-GMN-I04-JIHIMSG(10)
                                       TO  I04-JIHIMSG10
      *    室料差額
           MOVE    SPA-GMN-I04-RMSAGAKU
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RMSAGAKU
      *    選択番号
           MOVE    SPA-GMN-I04-SELNUM  TO  I04-SELNUM
      *
      *    調整金
           MOVE    SPA-GMN-I04-CHOSEI-X (1)
                                       TO  I04-CHOSEI (1)
           MOVE    SPA-GMN-I04-CHOSEI-X (2)
                                       TO  I04-CHOSEI (2)
      *
      *    入金上限額
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   =   "1" )
               MOVE    SPA-NAI-I04-SEIKYU-PLUS (SPA-I0-SYUNOU-CNT)
                                       TO  WRK-NYUKINLIMIT
           ELSE
               MOVE    SPA-GMN-I04-SEIGOK
                                       TO  WRK-NYUKINLIMIT
           END-IF
      *
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
               MOVE    SPA-NAI-I04-TSYUNOUIDX
                                       (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDX1
               IF    ( SPA-NAI-I04-SEIKYU-PLUS (IDX1)  >   ZERO )
                AND  ( WRK-NYUKINLIMIT     >   ZERO )
                   MOVE    "入金上限額："  TO  I04-LBLNYUKINMAX
                   MOVE    WRK-NYUKINLIMIT TO  WRK-Z99
                   MOVE    WRK-Z99         TO  WRK-STR
                   PERFORM 800-LTRIM-SEC
                   STRING  WRK-STR         DELIMITED   BY  SPACE
                           "円"            DELIMITED   BY  SIZE
                   INTO  I04-NYUKINMAX
                   END-STRING
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   =   "1" )
            AND  ( SPA-GMN-I04-HKNCOMBILST-CNT =   1   )
               CONTINUE
           ELSE
      *        合計入金額
               MOVE    "合計入金額："      TO  I04-LBLNYUKINTTL
               MOVE    SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                                           TO  WRK-Z99
               MOVE    WRK-Z99             TO  I04-NYUKINTTL
           END-IF
      *
      *    入金額
           MOVE    SPA-GMN-I04-NYUKIN-X
                                       TO  I04-NYUKIN
      *
      *    入金方法
           MOVE    SPA-GMN-I04-NYKNHOHO
                                       TO  I04-NYKNHOHO
           MOVE    SPA-GMN-I04-NYKNHOHO-CNT
                                       TO  I04-NYKNHOHO-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-NYKNHOHO-CNT )
               MOVE    SPA-GMN-I04-NYKNHOHO-ITM (IDX1)
                                       TO  I04-NYKNHOHO-ITM (IDX1)
           END-PERFORM
      *
      *    入金の取扱い
           MOVE    SPA-GMN-I04-NYKNKNRKBN
                                       TO  I04-NYKNKNRKBN
           MOVE    SPA-GMN-I04-NYKNKNRKBN-CNT
                                       TO  I04-NYKNKNRKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-NYKNKNRKBN-CNT )
               MOVE    SPA-GMN-I04-NYKNKNRKBN-ITM (IDX1)
                                       TO  I04-NYKNKNRKBN-ITM (IDX1)
           END-PERFORM
      *
      *    請求書兼領収書発行
           MOVE    SPA-GMN-I04-SKYPRTOUT
                                       TO  I04-SKYPRTOUT
           MOVE    SPA-GMN-I04-SKYPRTOUT-CNT
                                       TO  I04-SKYPRTOUT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-SKYPRTOUT-CNT )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (IDX1)
                                       TO  I04-SKYPRTOUT-ITM (IDX1)
           END-PERFORM
      *
      *    請求書兼領収書発行方法
           MOVE    SPA-GMN-I04-SKYSUMKBN
                                       TO  I04-SKYSUMKBN
           MOVE    SPA-GMN-I04-SKYSUMKBN-CNT
                                       TO  I04-SKYSUMKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-SKYSUMKBN-CNT )
               MOVE    SPA-GMN-I04-SKYSUMKBN-ITM (IDX1)
                                       TO  I04-SKYSUMKBN-ITM (IDX1)
           END-PERFORM
      *
      *    診療費明細書書発行
           MOVE    SPA-GMN-I04-SRYPRTOUT
                                       TO  I04-SRYPRTOUT
           MOVE    SPA-GMN-I04-SRYPRTOUT-CNT
                                       TO  I04-SRYPRTOUT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-SRYPRTOUT-CNT )
               MOVE    SPA-GMN-I04-SRYPRTOUT-ITM (IDX1)
                                       TO  I04-SRYPRTOUT-ITM (IDX1)
           END-PERFORM
      *
      *    退院証明書発行
           MOVE    SPA-GMN-I04-TAIPRTOUT
                                       TO  I04-TAIPRTOUT
           MOVE    SPA-GMN-I04-TAIPRTOUT-CNT
                                       TO  I04-TAIPRTOUT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-TAIPRTOUT-CNT )
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (IDX1)
                                       TO  I04-TAIPRTOUT-ITM (IDX1)
           END-PERFORM
      *
      *    主治医氏名
           MOVE    SPA-GMN-I04-DR      TO  I04-DR
           MOVE    SPA-GMN-I04-DR-CNT  TO  I04-DR-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-DR-CNT )
               MOVE    SPA-GMN-I04-DR-ITM (IDX1)
                                       TO  I04-DR-ITM (IDX1)
           END-PERFORM
      *
      *    ユーザープログラム
           MOVE    SPA-GMN-I04-USERPG      TO  I04-USERPG
           MOVE    SPA-GMN-I04-USERPG-CNT  TO  I04-USERPG-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-USERPG-CNT   )
               MOVE    SPA-GMN-I04-USERPG-ITM (IDX1)
                                           TO  I04-USERPG-ITM (IDX1)
           END-PERFORM
      *!!
      *    院外処方せん印刷
           MOVE    SPA-GMN-I04-SHOHOU      TO  I04-SHOHOSEN
           MOVE    SPA-GMN-I04-SHOHOU-CNT  TO  I04-SHOHOSEN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I04-SHOHOU-CNT   )
               MOVE    SPA-GMN-I04-SHOHOU-ITM (IDX1)
                                           TO  I04-SHOHOSEN-ITEM(IDX1)
           END-PERFORM
      *!!
      *
      *
      *    保険組み合わせリスト
           MOVE    SPA-GMN-I04-HKNCOMBILST-CNT
                                       TO  I04-HKNCOMBILST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-HKNCOMBILST-CNT )
      *
               MOVE    SPA-NAI-I04-TSELNUM (IDX1)
                                       TO  I04-TSELNUM (IDX1)
      *
               MOVE    SPA-GMN-I04-THKNCOMBI (IDX1)
                                       TO  I04-THKNCOMBI (IDX1)
               IF    ( SPA-GMN-I04-HKNCOMBILST-SEL =   IDX1 )
                   MOVE    "T"         TO  I04-HKNCOMBILST-SELECT (IDX1)
               ELSE
                   MOVE    "F"         TO  I04-HKNCOMBILST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-I01-I04-PROCKBN =   "1" )
               MOVE    "black"             TO  I04-LBLTAIINKEISAN-STYLE
               MOVE    SPACE               TO  I04-LBLTAIINKEISAN
           ELSE
               MOVE    "red"               TO  I04-LBLTAIINKEISAN-STYLE
               IF    ( SPA-NAI-I04-SAIKEISANFLG    =   ZERO )
                   MOVE    "［退院再計算］再作成"
                                           TO  I04-LBLTAIINKEISAN
                   MOVE    "収納訂正"      TO  I04-B05-LABEL
               ELSE
                   MOVE    "［退院再計算］訂正"
                                           TO  I04-LBLTAIINKEISAN
                   MOVE    "収納再作成"    TO  I04-B05-LABEL
               END-IF
           END-IF
      *
           MOVE    "black"             TO  I04-LBLRSIETC-STYLE
                                           I04-ROUETC-STYLE
      *
           IF    ( SPA-NAI-I04-RSITEIKIFLG =   CONST-EDIT-RSIJBI )
      *
               MOVE    "定期差額"          TO  I04-B07-LABEL
      *
               MOVE    "労災自賠保険適用分（円）"
                                           TO  I04-LBLRSITTL
               MOVE    "初診"
                                           TO  I04-LBLRSISYO
               MOVE    "再診"
                                           TO  I04-LBLRSISAI
               MOVE    "指導"
                                           TO  I04-LBLRSISDO
               MOVE    "その他"
                                           TO  I04-LBLRSIETC
      *
      *        労災初診
               MOVE    SPA-GMN-I04-ROUSYOSIN
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  I04-ROUSYOSIN
      *        労災再診
               MOVE    SPA-GMN-I04-ROUSAISIN
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  I04-ROUSAISIN
      *        労災指導
               MOVE    SPA-GMN-I04-ROUSIDOU
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  I04-ROUSIDOU
      *        労災その他
               MOVE    SPA-GMN-I04-ROUETC  TO  WRK-Z9
               MOVE    WRK-Z9              TO  I04-ROUETC
           ELSE
      *
               MOVE    "blue"              TO  I04-LBLRSIETC-STYLE
                                               I04-ROUETC-STYLE
      *
               MOVE    "労災自賠責"        TO  I04-B07-LABEL
      *
               MOVE    SPA-GMN-I04-KBT-TITLE
                                           TO  I04-LBLRSITTL
      *
               IF    ( SPA-NAI-I04-SAKKBN-MAX  >=  "1" )
                   MOVE    "１期"          TO  I04-LBLRSISYO
               END-IF
      *
               IF    ( SPA-NAI-I04-SAKKBN-MAX  >=  "2" )
                   MOVE    "２期"          TO  I04-LBLRSISAI
               END-IF
      *
               IF    ( SPA-NAI-I04-SAKKBN-MAX  >=  "3" )
                   MOVE    "３期"          TO  I04-LBLRSISDO
               END-IF
      *
               MOVE    "差額"              TO  I04-LBLRSIETC
      *
      *        １期
               MOVE    SPA-GMN-I04-SKY1ST  TO  WRK-Z9
               MOVE    WRK-Z9              TO  I04-ROUSYOSIN
      *
      *        ２期
               MOVE    SPA-GMN-I04-SKY2ND  TO  WRK-Z9
               MOVE    WRK-Z9              TO  I04-ROUSAISIN
      *
      *        ３期
               MOVE    SPA-GMN-I04-SKY3RD  TO  WRK-Z9
               MOVE    WRK-Z9              TO  I04-ROUSIDOU
      *
               IF    ( SPA-GMN-I04-SKYTAIIN    NOT =   ZERO )
                   MOVE    "退院"          TO  I04-LBLRSISDO
                   MOVE    SPA-GMN-I04-SKYTAIIN
                                           TO  WRK-Z9
                   MOVE    WRK-Z9          TO  I04-ROUSIDOU
               END-IF
      *
      *        差額
               MOVE    SPA-GMN-I04-SKYSGK  TO  WRK-Z99
               MOVE    WRK-Z99             TO  I04-ROUETC
      *
           END-IF
      *
           MOVE    SPA-GMN-I04-LBLSKYINF   TO  I04-LBLSKYINF
      *
           MOVE    "black"                 TO  I04-LBLINFO-STYLE
           IF    ( SPA-I01-I04-PROCKBN =   "2" )
               IF    ( SPA-NAI-I04-SAIKEISANFLG    =   ZERO )
                   MOVE    "red"           TO  I04-LBLINFO-STYLE
                   MOVE    "※元の請求データは取り消されます"
                                           TO  I04-LBLINFO
               ELSE
                   PERFORM VARYING  IDX-SYU    FROM    1   BY  1
                           UNTIL  ( IDX-SYU    >   SPA-I0-SYUNOU-CNT )
                            OR    ( SPA-I0IDCD NOT =   SPACE )
                       IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-SYU)
                                                   =   3 )
                           MOVE    "red"   TO  I04-LBLINFO-STYLE
                           MOVE    SPACE   TO  I04-LBLINFO
                           STRING  "※保険組合せを変更した請求データは"
                                               DELIMITED   BY  SIZE
                                   "取り消されます"
                                               DELIMITED   BY  SIZE
                           INTO    I04-LBLINFO
                           END-STRING
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
               EVALUATE SPA-NAI-I04-PTSKYPRT
               WHEN    "0"
                   MOVE    "【請求書・明細書不要】"
                                   TO  I04-LBLINFO2
               WHEN    "1"
                   MOVE    "【請求書・明細書必要】"
                                   TO  I04-LBLINFO2
               WHEN    "2"
                   MOVE    "【請求書・明細書必要（請求あり）】"
                                   TO  I04-LBLINFO2
               WHEN    "3"
                   MOVE    "【請求書・明細書必要（訂正時なし）】"
                                   TO  I04-LBLINFO2
               WHEN    "4"
                   MOVE    "【請求書必要（明細書不要）】"
                                   TO  I04-LBLINFO2
               WHEN    "5"
                   MOVE    "【請求書必要（請求あり）（明細書不要）】"
                                   TO  I04-LBLINFO2
               WHEN    "6"
                   MOVE    "【請求書必要（訂正時なし）（明細書不要）】"
                                   TO  I04-LBLINFO2
               WHEN    "7"
                   MOVE    "【請求書必要（請求あり）（明細書必要）】"
                                   TO  I04-LBLINFO2
               WHEN    "8"
                   MOVE    "【請求書不要・明細書必要】"
                                   TO  I04-LBLINFO2
               WHEN    "9"
                   MOVE    "【請求書不要・明細書必要（請求あり）】"
                                   TO  I04-LBLINFO2
               WHEN    "A"
                   MOVE    "【請求書不要・明細書必要（訂正時なし）】"
                                   TO  I04-LBLINFO2
               END-EVALUATE
      *
           MOVE    "black"         TO  I04-LBLGENMEN-STYLE
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
               MOVE    SPA-NAI-I04-TSYUNOUIDX
                                       (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDX1
               MOVE    SPA-I0-SYUNOU-DAT (IDX1)
                                       TO  WKSYUNOU-REC
               IF    ( WKSYU-DISCOUNT-TEIRITU  =   "1" OR "2" )
                   MOVE    "blue"      TO  I04-LBLGENMEN-STYLE
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1)
                                       =   ZERO )
                    OR   ( SPA-GMN-I04-HKNCOMBILST-CNT =   1 )
                       EVALUATE    WKSYU-DISCOUNT-TEIRITU
                       WHEN    "1"
                           MOVE    WKSYU-DISCOUNT-RATE     TO  WRK-ZZ9
                           STRING  "減免有（"  DELIMITED   BY  SIZE
                                   WRK-ZZ9     DELIMITED   BY  SIZE
                                   "％）"      DELIMITED   BY  SIZE
                           INTO    I04-LBLGENMEN
                           END-STRING
                       WHEN    "2"
                           MOVE    WKSYU-DISCOUNT-RATE     TO  WRK-Z99
                           STRING  "減免有（"  DELIMITED   BY  SIZE
                                   WRK-Z99     DELIMITED   BY  SIZE
                                   "円）"      DELIMITED   BY  SIZE
                           INTO    I04-LBLGENMEN
                           END-STRING
                       END-EVALUATE
                   ELSE
                       MOVE    "減免有"            TO  I04-LBLGENMEN
                   END-IF
               END-IF
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  I04-CHOSEI-STT (1)
                                       I04-CHOSEI-STT (2)
                                       I04-NYUKIN-STT
                                       I04-USERPG-STT
                                       I04-USERPG-ITM-STT
      *
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL =   ZERO )
                   MOVE    WIDGET-INSENSITIVE
                                   TO  I04-NYUKIN-STT
           ELSE
               MOVE    SPA-NAI-I04-TSYUNOUIDX
                                       (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDX1
               IF    ( SPA-NAI-I04-SEIKYU-PLUS (IDX1)
                                   <=  ZERO )
                AND  ( SPA-GMN-I04-NYUKIN  =   ZERO )
                   MOVE    WIDGET-INSENSITIVE
                                   TO  I04-NYUKIN-STT
               END-IF
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I04-CUR     =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-I04-CUR
           WHEN    001
               IF    ( I04-NYUKIN-STT
                                   =   WIDGET-NORMAL )
                   MOVE    "NYUKIN"        TO  MCP-WIDGET
               ELSE
                   MOVE    "SKYPRTOUT"     TO  MCP-WIDGET
               END-IF
           WHEN    002
               MOVE    "SKYPRTOUT"     TO  MCP-WIDGET
           WHEN    003
               MOVE    "TAIPRTOUT"     TO  MCP-WIDGET
           WHEN    004
               MOVE    "DR"            TO  MCP-WIDGET
           WHEN    005
               MOVE    "NYKNHOHO"      TO  MCP-WIDGET
           WHEN    007
               MOVE    "NYKNKNRKBN"    TO  MCP-WIDGET
           WHEN    006
               MOVE    "SRYPRTOUT"     TO  MCP-WIDGET
           WHEN    008
               IF    ( I04-USERPG-STT
                                   =   WIDGET-NORMAL )
                   MOVE    "USERPG"    TO  MCP-WIDGET
               ELSE
                   MOVE    "B12"       TO  MCP-WIDGET
               END-IF
           WHEN    009
               MOVE    "SHOHOSEN"        TO  MCP-WIDGET
           WHEN    010
               MOVE    "SKYSUMKBN"     TO  MCP-WIDGET
           WHEN    101
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    102
               IF    ( I04-CHOSEI-STT (1)
                                   =   WIDGET-NORMAL )
                   MOVE    "CHOSEI1"   TO  MCP-WIDGET
               END-IF
           WHEN    103
               IF    ( I04-CHOSEI-STT (2)
                                   =   WIDGET-NORMAL )
                   MOVE    "CHOSEI2"   TO  MCP-WIDGET
               END-IF
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
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
           WHEN    "NYUKIN"
               MOVE    002             TO  SPA-GMN-I04-CUR
           WHEN    "SKYPRTOUT"
               MOVE    010             TO  SPA-GMN-I04-CUR
           WHEN    "SKYSUMKBN"
               MOVE    006             TO  SPA-GMN-I04-CUR
           WHEN    "SRYPRTOUT"
               MOVE    003             TO  SPA-GMN-I04-CUR
           WHEN    "TAIPRTOUT"
     ********* MOVE    004             TO  SPA-GMN-I04-CUR
               MOVE    009             TO  SPA-GMN-I04-CUR
           WHEN    "SHOHOSEN"
               MOVE    004             TO  SPA-GMN-I04-CUR
           WHEN    "DR"
               MOVE    212             TO  SPA-GMN-I04-CUR
           WHEN    "SELNUM"
               MOVE    001             TO  SPA-GMN-I04-CUR
           WHEN    "CHOSEI1"
               MOVE    103             TO  SPA-GMN-I04-CUR
           WHEN    "CHOSEI2"
               MOVE    212             TO  SPA-GMN-I04-CUR
           WHEN    "NYKNHOHO"
               MOVE    007             TO  SPA-GMN-I04-CUR
           WHEN    "NYKNKNRKBN"
               MOVE    002             TO  SPA-GMN-I04-CUR
           WHEN    "USERPG"
               MOVE    212             TO  SPA-GMN-I04-CUR
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT     ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "B01"         TO  SPA-I01-I04-BTN
               MOVE    "CHANGE"      TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    調整
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE    102           TO    SPA-GMN-I04-CUR
      *    選択
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE    101           TO    SPA-GMN-I04-CUR
      *    選択
           WHEN    "CLICKED"       ALSO    "B05"
               IF    ( SPA-I01-I04-PROCKBN =   "2" )
                   PERFORM 4100-SAIKEISAN-CHG-SEC
               END-IF
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 4100-RSITEIKI-CHG-SEC
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 4100-IKKATU-NYUKIN-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 450-TOUROKU-SEC
      *!!
      *    院外処方せん
           WHEN    "CLICKED"       ALSO    "B10"
               PERFORM 409-I041-SET-SEC
      *!!
           END-EVALUATE
          .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-I04-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    選択番号チェック
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN        ANY         ALSO    "HKNCOMBILST"
               PERFORM 4101-HKNCOMBILST-SEL-SEC
           WHEN        OTHER
               MOVE    ZERO        TO      SPA-GMN-I04-CUR
           END-EVALUATE
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴リスト選択処理
      *****************************************************************
       4101-HKNCOMBILST-SEL-SEC        SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO TO   4101-HKNCOMBILST-SEL-EXT
           END-IF
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I04-HKNCOMBILST-CNT )
      *
               IF    ( I04-HKNCOMBILST-SELECT (IDX1)   =   "T"  )
                   MOVE    IDX1        TO  SPA-GMN-I04-HKNCOMBILST-SEL
                   MOVE    SPA-GMN-I04-TSELNUM (IDX1)
                                       TO  SPA-GMN-I04-SELNUM
      *
                   MOVE    SPA-NAI-I04-TSYUNOUIDX (SPA-GMN-I04-SELNUM)
                                       TO  WRK-LST-IDX
                   PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
      *            調整金
                   MOVE    SPA-NAI-I04-CHOSEI (WRK-LST-IDX 1)
                                           TO  WRK-Z9
                   MOVE    WRK-Z9          TO  I04-CHOSEI (1)
                   MOVE    SPA-NAI-I04-CHOSEI (WRK-LST-IDX 2)
                                           TO  WRK-Z9
                   MOVE    WRK-Z9          TO  I04-CHOSEI (2)
      *            入金額
                   MOVE    SPA-NAI-I04-NYUKIN-X (WRK-LST-IDX)
                                           TO  I04-NYUKIN
      *
                   MOVE    SPA-GMN-I04-HKNCOMBILST-CNT
                                           TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-I04-CUR
           .
      *
       4101-HKNCOMBILST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
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
      *    調整金
           MOVE    I04-CHOSEI(1)   TO  SPA-GMN-I04-CHOSEI-X (1)
           MOVE    I04-CHOSEI(2)   TO  SPA-GMN-I04-CHOSEI-X (2)
      *
      *    入金額
           MOVE    I04-NYUKIN      TO  SPA-GMN-I04-NYUKIN-X
      *
      *    入金方法
           MOVE    I04-NYKNHOHO    TO  SPA-GMN-I04-NYKNHOHO
      *
      *    入金の取扱い
           MOVE    I04-NYKNKNRKBN  TO  SPA-GMN-I04-NYKNKNRKBN
      *
      *    請求書兼領収書発行
           MOVE    I04-SKYPRTOUT   TO  SPA-GMN-I04-SKYPRTOUT
      *
      *    請求書兼領収書発行方法
           MOVE    I04-SKYSUMKBN   TO  SPA-GMN-I04-SKYSUMKBN
      *
      *    診療費明細書発行
           MOVE    I04-SRYPRTOUT   TO  SPA-GMN-I04-SRYPRTOUT
      *
      *    退院証明書発行
           MOVE    I04-TAIPRTOUT   TO  SPA-GMN-I04-TAIPRTOUT
      *
      *    主治医氏名
           MOVE    I04-DR          TO  SPA-GMN-I04-DR
      *
      *    ユーザープログラム
           MOVE    I04-USERPG      TO  SPA-GMN-I04-USERPG
      *
      *    ユーザープログラム
           MOVE    I04-SHOHOSEN        TO  SPA-GMN-I04-SHOHOU
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
      *    入金の取扱いチェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-NYKNKNRKBN-CHK-SEC
           END-IF
      *
      *    調整金チェック処理
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 4100-CHOSEI-CHK-SEC
      *
           END-PERFORM
      *
      *    入金額チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-NYUKIN-CHK-SEC
           END-IF
      *
      *    入金方法チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-NYKNHOHO-CHK-SEC
           END-IF
      *
      *    請求書兼領収書発行チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SKYPRTOUT-CHK-SEC
           END-IF
      *
      *    請求書兼領収書発行方法チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SKYSUMKBN-CHK-SEC
           END-IF
      *
      *    診療費明細書発行チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SRYPRTOUT-CHK-SEC
           END-IF
      *
      *    退院証明書発行チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-TAIPRTOUT-CHK-SEC
           END-IF
      *!!
      *    院外処方せん発行チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SHOHOU-CHK-SEC
           END-IF
      *
      *    主治医氏名チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-DR-CHK-SEC
           END-IF
      *
      *    ユーザープログラムチェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-USERPG-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調整金チェック処理
      *****************************************************************
       4100-CHOSEI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
            AND  ( SPA-GMN-I04-HKNCOMBILST-SEL
                           <=  SPA-GMN-I04-HKNCOMBILST-CNT )
               CONTINUE
           ELSE
               GO  TO  4100-CHOSEI-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX
                                   (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDXZ
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-I04-CHOSEI-X (IDXC)   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               COMPUTE SPA-GMN-I04-CUR
                   =   101 +   IDXC
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-CHOSEI
      *
               MOVE    IDXZ                TO  IDX-G
      *
               PERFORM 4101-CHOSEI-SORT-SEC
      *
      *        合計再計算
               PERFORM 4101-SEIKYU-KEI-SEC
      *
               MOVE    SPA-NAI-I04-CHOSEI (IDXZ IDXC)
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  SPA-GMN-I04-CHOSEI-X (IDXC)
      *
      *        合計額がゼロ以下の時、発行なしとする
      *        EVALUATE    WRK-MCP-WIDGET
      *        調整金
      *        WHEN    "CHOSEI1"
      *        WHEN    "CHOSEI2"
      *            IF    ( SPA-GMN-I04-SEIGOK  <=  ZERO )
      *                MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
      *                                        TO  SPA-GMN-I04-SKYPRTOUT
      *                MOVE    SPA-GMN-I04-SRYPRTOUT-ITM (1)
      *                                        TO  SPA-GMN-I04-SRYPRTOUT
      *            END-IF
      *        END-EVALUATE
           END-IF
      *
      *    今回請求額がゼロ以上であること
           IF    ( IDXC                =   2 )
               MOVE    IDXZ        TO  IDX7
               PERFORM 41001-CHOSEI-SEIKYU-CHK-SEC
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-I04-HKNCOMBILST-CNT )
                         OR  ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    SPA-NAI-I04-TSYUNOUIDX (IDX0)
                                           TO  IDX7
                   PERFORM 41001-CHOSEI-SEIKYU-CHK-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       MOVE    IDX0    TO  SPA-GMN-I04-HKNCOMBILST-SEL
                       MOVE  SPA-GMN-I04-TSELNUM (IDX0)
                                           TO  SPA-GMN-I04-SELNUM
      *
                       MOVE  SPA-NAI-I04-TSYUNOUIDX (SPA-GMN-I04-SELNUM)
                                           TO  WRK-LST-IDX
                       PERFORM 3100-SEIKYU-DAT-EDIT-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4100-CHOSEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金振り分け処理
      *****************************************************************
       4101-CHOSEI-SORT-SEC            SECTION.
      *
           COMPUTE WRK-CHOSEITTL
               =   SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT IDXC)
               +   WRK-CHOSEI
               -   SPA-NAI-I04-CHOSEI (IDX-G IDXC)
      *
           PERFORM 41001-CHOSEI-SORT-SEC
      *
           MOVE    WRK-CHOSEITTL   TO  SPA-NAI-I04-CHOSEI
                                       (SPA-I0-SYUNOU-CNT IDXC)
      *
           .
       4101-CHOSEI-SORT-EXT.
           EXIT.
      *****************************************************************
      *    調整金振り分け処理
      *****************************************************************
       41001-CHOSEI-SORT-SEC           SECTION.
      *
           IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-G)
                                       =   ZERO )
               MOVE    WRK-CHOSEI  TO  SPA-NAI-I04-CHOSEI (IDX-G IDXC)
           ELSE
               COMPUTE WRK-CHOSEI
                   =   WRK-CHOSEI  -   SPA-NAI-I04-CHOSEI (IDX-G IDXC)
      *
               IF    ( WRK-CHOSEI  >=  ZERO )
                   PERFORM 410011-CHOSEI-SORT-SEC
               ELSE
                   PERFORM 410012-CHOSEI-SORT-SEC
               END-IF
           END-IF
      *
           .
       41001-CHOSEI-SORT-EXT.
           EXIT.
      *****************************************************************
      *    調整金振り分け処理（調整金増）
      *****************************************************************
       410011-CHOSEI-SORT-SEC          SECTION.
      *
           COMPUTE WRK-IDX =   SPA-I0-SYUNOU-CNT   -   1
      *
           PERFORM VARYING IDX7    FROM    WRK-IDX     BY  -1
                   UNTIL ( IDX7        <   1 )
                    OR   ( WRK-CHOSEI  =   ZERO )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                      =   ZERO )
      *
                   MOVE    ZERO        TO  FLG-TARGET
      *
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-G)
                                      =    1 )
                       IF    ( SPA-I0-SYUNOU-YM-S  (IDX7)
                                      =    SPA-I0-SYUNOU-YM-S(IDX-G))
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
      *
                   IF    ( FLG-TARGET  =   1 )
      *
                       IF    ( SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                                       <   ZERO )
                           COMPUTE SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                               =   SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                               +   WRK-CHOSEI
      *
                           IF    ( SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                                       >   ZERO )
                               MOVE    SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                                       TO  WRK-CHOSEI
                               MOVE    ZERO
                                       TO  SPA-NAI-I04-CHOSEI(IDX7 IDXC)
                           ELSE
                               MOVE    ZERO
                                       TO  WRK-CHOSEI
                           END-IF
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX7    FROM    1   BY  1
                   UNTIL ( IDX7        >   WRK-IDX )
                    OR   ( WRK-CHOSEI  =   ZERO )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                      =   ZERO )
      *
                   MOVE    ZERO        TO  FLG-TARGET
      *
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-G)
                                      =    1 )
                       IF    ( SPA-I0-SYUNOU-YM-S  (IDX7)
                                      =    SPA-I0-SYUNOU-YM-S(IDX-G))
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
      *
                   IF    ( FLG-TARGET  =   1 )
      *
                       COMPUTE SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                           =   SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                           +   WRK-CHOSEI
      *
                       MOVE    ZERO    TO  WRK-CHOSEI
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       410011-CHOSEI-SORT-EXT.
           EXIT.
      *****************************************************************
      *    調整金振り分け処理（調整金減）
      *****************************************************************
       410012-CHOSEI-SORT-SEC          SECTION.
      *
           COMPUTE WRK-IDX =   SPA-I0-SYUNOU-CNT   -   1
           MOVE    ZERO            TO  IDX-CHOSEI
      *
           PERFORM VARYING IDX7    FROM    WRK-IDX     BY  -1
                   UNTIL ( IDX7        <   1 )
                    OR   ( WRK-CHOSEI  =   ZERO )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                      =   ZERO )
      *
                   MOVE    ZERO        TO  FLG-TARGET
      *
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-G)
                                      =    1 )
                       IF    ( SPA-I0-SYUNOU-YM-S  (IDX7)
                                      =    SPA-I0-SYUNOU-YM-S(IDX-G))
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
      *
                   IF    ( FLG-TARGET  =   1 )
      *
                       IF    ( SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                                       >   ZERO )
                           COMPUTE SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                               =   SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                               +   WRK-CHOSEI
      *
                           IF    ( SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                                       <   ZERO )
                               MOVE    SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                                       TO  WRK-CHOSEI
                               MOVE    ZERO
                                       TO  SPA-NAI-I04-CHOSEI(IDX7 IDXC)
                           ELSE
                               MOVE    ZERO
                                       TO  WRK-CHOSEI
                           END-IF
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX7    FROM    1   BY  1
                   UNTIL ( IDX7        >   WRK-IDX )
                    OR   ( WRK-CHOSEI  =   ZERO )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                       =   ZERO )
      *
                   MOVE    ZERO        TO  FLG-TARGET
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-G)
                                       =   1 )
                       IF    ( SPA-I0-SYUNOU-YM-S  (IDX7)
                                       =   SPA-I0-SYUNOU-YM-S(IDX-G))
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
      *
                   IF    ( FLG-TARGET  =   1 )
      *
                       MOVE    IDX7    TO  IDX-CHOSEI
      *
                       COMPUTE SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                           =   SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                           +   WRK-CHOSEI
      *
                       COMPUTE WRK-SEIKYU
                           =   SPA-NAI-I04-SEIKYU-KON (IDX7)
                           +   SPA-NAI-I04-SEIKYU-MOT (IDX7)
                           +   SPA-NAI-I04-CHOSEI (IDX7 1)
                           +   SPA-NAI-I04-CHOSEI (IDX7 2)
      *
                       IF    ( WRK-SEIKYU         >=  ZERO )
                           MOVE    ZERO    TO  WRK-CHOSEI
                       ELSE
                           COMPUTE SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                               =   SPA-NAI-I04-CHOSEI (IDX7 IDXC)
                               -   WRK-SEIKYU
                           COMPUTE WRK-CHOSEI
                               =   WRK-SEIKYU
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CHOSEI      NOT     =   ZERO )
               IF    ( IDX-CHOSEI  NOT     =   ZERO )
                   COMPUTE SPA-NAI-I04-CHOSEI (IDX-CHOSEI IDXC)
                       =   SPA-NAI-I04-CHOSEI (IDX-CHOSEI IDXC)
                       +   WRK-CHOSEI
               END-IF
           END-IF
      *
           .
       410012-CHOSEI-SORT-EXT.
           EXIT.
      *****************************************************************
      *    調整後請求額チェック処理
      *****************************************************************
       41001-CHOSEI-SEIKYU-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  WRK-GMN-CUR
           IF    ( SPA-NAI-I04-CHOSEI (IDX7 2)
                               NOT =   ZERO )
               MOVE    103     TO  WRK-GMN-CUR
           ELSE
               MOVE    102     TO  WRK-GMN-CUR
           END-IF
      *
           MOVE    ZERO                        TO  WRK-SEIKYU
           IF    ( SPA-I01-I04-PROCKBN =   "2" )
            AND  ( SPA-I0-SYUNOU-UPDFLG (IDX7)  =   ZERO )
            AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
      *
               COMPUTE WRK-SEIKYU
                   =   SPA-NAI-I04-SEIKYU (IDX7)
                   +   SPA-NAI-I04-SEIKYU-MOT (IDX7)
                   -   SPA-I0-UPD-SKYMONEY(IDX7)
      *
               MOVE    SYU-DENPNUM TO  WRK-DENPNUM
           ELSE
               COMPUTE WRK-SEIKYU
                   =   SPA-NAI-I04-SEIKYU (IDX7)
           END-IF
      *
           IF     (WRK-SEIKYU          <   ZERO )
               MOVE    "0004"      TO  SPA-ERRCD
               MOVE    WRK-GMN-CUR TO  SPA-GMN-I04-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF     (WRK-SEIKYU          >   CONST-KINGAKU-MAX )
                   MOVE    "0014"      TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR TO  SPA-GMN-I04-CUR
               END-IF
           END-IF
      *
           .
       41001-CHOSEI-SEIKYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金チェック処理
      *****************************************************************
       4100-NYUKIN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
            AND  ( SPA-GMN-I04-HKNCOMBILST-SEL
                           <=  SPA-GMN-I04-HKNCOMBILST-CNT )
               CONTINUE
           ELSE
               GO  TO  4100-NYUKIN-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX
                                  (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDXZ
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-I04-NYUKIN-X    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-I04-CUR
      *
           ELSE
      *
               MOVE    SNUM-OUTNUM    TO   SPA-GMN-I04-NYUKIN
               MOVE    SNUM-OUTNUM    TO   WRK-Z92
               MOVE    WRK-Z92        TO   SPA-GMN-I04-NYUKIN-X
      *
      *        今回入金がゼロ以上であること
               IF    ( SPA-GMN-I04-NYUKIN  <   ZERO )
                   MOVE    "0007"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN-I04-CUR
                   GO  TO  4100-NYUKIN-CHK-EXT
               END-IF
      *
               IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   =   "1" )
      *
      *            今回入金が請求額以下であること
                   IF   (SPA-GMN-I04-NYUKIN
                                   >   SPA-NAI-I04-SEIKYU-PLUS(IDXZ))
                       MOVE    "0008"  TO  SPA-ERRCD
                       MOVE    1       TO  SPA-GMN-I04-CUR
                       GO  TO  4100-NYUKIN-CHK-EXT
                   END-IF
               ELSE
      *
      *            今回入金が入金上限額以下であること
                   IF    ( SPA-GMN-I04-SEIGOK
                                       >=  ZERO )
                    AND  ( SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                       +   SPA-GMN-I04-NYUKIN
                       -   SPA-NAI-I04-NYUKIN (IDXZ)
                                       >   SPA-GMN-I04-SEIGOK )
                       MOVE    "0008"  TO  SPA-ERRCD
                       MOVE    1       TO  SPA-GMN-I04-CUR
                       GO  TO  4100-NYUKIN-CHK-EXT
                   END-IF
               END-IF
      *
               MOVE    IDXZ                TO  IDX-LST
               MOVE    SNUM-OUTNUM         TO  WRK-NYUKIN
      *
      *        入金額振り分け
               PERFORM 4101-NYUKIN-SORT-SEC
      *
      *        合計行編集処理
               PERFORM 4101-TTL-LINE-EDIT-SEC
      *
               MOVE    SPA-NAI-I04-NYUKIN (IDXZ)
                                           TO  SPA-GMN-I04-NYUKIN
               MOVE    SPA-NAI-I04-NYUKIN-X (IDXZ)
                                           TO  SPA-GMN-I04-NYUKIN-X
      *
      *        合計再計算
               COMPUTE SPA-GMN-I04-MISYUGOK
                   =   SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                   +   SPA-GMN-I04-ZENMISYU-NYU
                   -   SPA-GMN-I04-ZENKANYU-NYU
                   -   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
      *
           END-IF
      *
           .
       4100-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金振り分け処理
      *****************************************************************
       4101-NYUKIN-SORT-SEC            SECTION.
      *
           COMPUTE WRK-NYUKINTTL
               =   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
               +   WRK-NYUKIN
               -   SPA-NAI-I04-NYUKIN (IDX-LST)
      *
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   =   "1" )
      *
      *
               PERFORM 41001-NYUKIN-SORT-SEC
      *
           ELSE
      *
               PERFORM 41002-NYUKIN-SORT-SEC
           END-IF
      *
           MOVE    WRK-NYUKINTTL   TO  SPA-NAI-I04-NYUKIN
                                       (SPA-I0-SYUNOU-CNT)
      *
           .
       4101-NYUKIN-SORT-EXT.
           EXIT.
      *****************************************************************
      *    入金振り分け処理 入金の取扱い＝今回請求分のみ
      *****************************************************************
       41001-NYUKIN-SORT-SEC           SECTION.
      *
           IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-LST)
                                       =   ZERO )
               MOVE    WRK-NYUKIN  TO  SPA-NAI-I04-NYUKIN (IDX-LST)
           ELSE
               COMPUTE WRK-NYUKIN
                   =   WRK-NYUKIN  -   SPA-NAI-I04-NYUKIN (IDX-LST)
      *
               IF    ( WRK-NYUKIN  >=  ZERO )
                   PERFORM 410011-NYUKIN-SORT-SEC
               ELSE
                   PERFORM 410012-NYUKIN-SORT-SEC
               END-IF
           END-IF
      *
           .
       41001-NYUKIN-SORT-EXT.
           EXIT.
      *****************************************************************
      *    入金振り分け処理 入金の取扱い＝今回請求分のみ（合計行入金増）
      *****************************************************************
       410011-NYUKIN-SORT-SEC          SECTION.
      *
           COMPUTE WRK-IDX =   SPA-I0-SYUNOU-CNT   -   1
           MOVE    ZERO            TO  IDX-NYUKIN
      *
           PERFORM VARYING IDX7    FROM    1   BY  1
                   UNTIL ( IDX7        >   WRK-IDX )
                    OR   ( WRK-NYUKIN  =   ZERO )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                      =   ZERO )
      *
                   MOVE    ZERO        TO  FLG-TARGET
      *
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-LST)
                                      =    1 )
                       IF    ( SPA-I0-SYUNOU-YM-S  (IDX7)
                                      =    SPA-I0-SYUNOU-YM-S(IDX-LST))
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
      *
                   IF    ( FLG-TARGET  =   1 )
                       MOVE    IDX7    TO  IDX-NYUKIN
      *
                       COMPUTE SPA-NAI-I04-NYUKIN (IDX7)
                           =   SPA-NAI-I04-NYUKIN (IDX7)
                           +   WRK-NYUKIN
      *
                       IF    ( SPA-NAI-I04-SEIKYU-PLUS (IDX7)
                                       >   SPA-NAI-I04-NYUKIN (IDX7) )
                           MOVE    ZERO    TO  WRK-NYUKIN
                       ELSE
                           COMPUTE WRK-NYUKIN
                               =   SPA-NAI-I04-NYUKIN (IDX7)
                               -   SPA-NAI-I04-SEIKYU-PLUS (IDX7)
      *
                           MOVE    SPA-NAI-I04-SEIKYU-PLUS (IDX7)
                                           TO  SPA-NAI-I04-NYUKIN (IDX7)
      *
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-NYUKIN       NOT     =   ZERO )
               IF    ( IDX-NYUKIN   NOT     =   ZERO )
                   COMPUTE SPA-NAI-I04-NYUKIN (IDX-NYUKIN)
                       =   SPA-NAI-I04-NYUKIN (IDX-NYUKIN)
                       +   WRK-NYUKIN
               END-IF
           END-IF
      *
           .
       410011-NYUKIN-SORT-EXT.
           EXIT.
      *****************************************************************
      *    入金振り分け処理 入金の取扱い＝今回請求分のみ（合計行入金減）
      *****************************************************************
       410012-NYUKIN-SORT-SEC          SECTION.
      *
           COMPUTE WRK-IDX =   SPA-I0-SYUNOU-CNT   -   1
           MOVE    ZERO            TO  IDX-NYUKIN
      *
           PERFORM VARYING IDX7    FROM    WRK-IDX     BY  -1
                   UNTIL ( IDX7        <   1 )
                    OR   ( WRK-NYUKIN  =   ZERO )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                       =   ZERO )
      *
                   MOVE    ZERO        TO  FLG-TARGET
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-LST)
                                       =   1 )
                       IF    ( SPA-I0-SYUNOU-YM-S  (IDX7)
                                       =   SPA-I0-SYUNOU-YM-S(IDX-LST))
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
      *
                   IF    ( FLG-TARGET  =   1 )
      *
                       COMPUTE SPA-NAI-I04-NYUKIN (IDX7)
                           =   SPA-NAI-I04-NYUKIN (IDX7)
                           +   WRK-NYUKIN
      *
                       IF    ( SPA-NAI-I04-NYUKIN (IDX7)   >=  ZERO )
                           MOVE    ZERO    TO  WRK-NYUKIN
                       ELSE
                           MOVE    SPA-NAI-I04-NYUKIN (IDX7)
                                           TO  WRK-NYUKIN
                           MOVE    ZERO    TO  SPA-NAI-I04-NYUKIN (IDX7)
      *
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-NYUKIN
           PERFORM VARYING IDX7    FROM    1   BY  1
                   UNTIL ( IDX7        >   WRK-IDX )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                       =   ZERO )
      *
                   MOVE    ZERO        TO  FLG-TARGET
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-LST)
                                       =   1 )
                       IF    ( SPA-I0-SYUNOU-YM-S  (IDX7)
                                       =   SPA-I0-SYUNOU-YM-S(IDX-LST))
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TARGET
                   END-IF
      *
                   IF    ( FLG-TARGET  =   1 )
      *
                       IF    ( SPA-NAI-I04-NYUKIN (IDX7)
                                   >   SPA-NAI-I04-SEIKYU-PLUS (IDX7))
      *
                           COMPUTE WRK-NYUKIN
                               =   WRK-NYUKIN
                               +   SPA-NAI-I04-NYUKIN (IDX7)
                               -   SPA-NAI-I04-SEIKYU-PLUS (IDX7)
                           COMPUTE SPA-NAI-I04-NYUKIN (IDX7)
                               =   SPA-NAI-I04-SEIKYU-PLUS (IDX7)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( WRK-NYUKIN  >   ZERO )
               PERFORM 410011-NYUKIN-SORT-SEC
           END-IF
      *
           .
       410012-NYUKIN-SORT-EXT.
           EXIT.
      *****************************************************************
      *    入金振り分け処理 入金の取扱い≠（否）今回請求分のみ
      *****************************************************************
       41002-NYUKIN-SORT-SEC           SECTION.
      *
           COMPUTE WRK-NYUKIN
               =   WRK-NYUKIN
               -   SPA-NAI-I04-NYUKIN (IDX-LST)
               +   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
      *
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   =   "4" OR  "5" )
               COMPUTE WRK-NYUKIN
                   =   WRK-NYUKIN
                   -   SPA-GMN-I04-ZENMISYU-NYU
               IF    ( WRK-NYUKIN  <   ZERO )
                   MOVE    ZERO    TO  WRK-NYUKIN
               END-IF
           END-IF
      *
           COMPUTE WRK-IDX =   SPA-I0-SYUNOU-CNT   -   1
      *
           PERFORM VARYING IDX7    FROM    1   BY  1
                   UNTIL ( IDX7         >   WRK-IDX )
      *
               IF    ( WRK-NYUKIN   =   ZERO )
                   MOVE    ZERO     TO  SPA-NAI-I04-NYUKIN (IDX7)
               ELSE
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                    =   ZERO )
      *
                       MOVE    WRK-NYUKIN  TO  SPA-NAI-I04-NYUKIN (IDX7)
      *
                       IF    ( SPA-NAI-I04-SEIKYU-PLUS (IDX7)
                                       >   SPA-NAI-I04-NYUKIN (IDX7) )
                           MOVE    ZERO    TO  WRK-NYUKIN
                       ELSE
                           COMPUTE WRK-NYUKIN
                               =   SPA-NAI-I04-NYUKIN (IDX7)
                               -   SPA-NAI-I04-SEIKYU-PLUS (IDX7)
      *
                           MOVE    SPA-NAI-I04-SEIKYU-PLUS (IDX7)
                                       TO  SPA-NAI-I04-NYUKIN (IDX7)
      *
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       41002-NYUKIN-SORT-EXT.
           EXIT.
      *****************************************************************
      *    合計請求額計算処理
      *****************************************************************
       4101-SEIKYU-KEI-SEC                  SECTION.
      *
      *
      *    今回入金
      *    IF    ( SPA-GMN-I04-SEIKYU  <   ZERO )
      *        MOVE    ZERO            TO  SPA-NAI-I04-NYUKIN (IDX-G)
      *    ELSE
      *        MOVE    SPA-GMN-I04-SEIKYU
      *                                TO  SPA-NAI-I04-NYUKIN (IDX-G)
      *    END-IF
      *
      *    MOVE    SPA-NAI-I04-NYUKIN (IDX-G)
      *                                TO  WRK-Z92
      *    MOVE    WRK-Z92             TO  SPA-NAI-I04-NYUKIN-X (IDX-G)
      *
      *    合計行編集処理
           PERFORM 4101-TTL-LINE-EDIT-SEC
      *
      *    合計請求額
           COMPUTE SPA-GMN-I04-SEIGOK  =   SPA-NAI-I04-SEIKYU-PLUS
                                                (SPA-I0-SYUNOU-CNT)
                                       +   SPA-GMN-I04-ZENMISYU-NYU
      *
           MOVE    SPA-NAI-I04-SEIKYU (IDX-G)
                                       TO  SPA-GMN-I04-SEIKYU
      *
           .
       4101-SEIKYU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計行編集処理
      *****************************************************************
       4101-TTL-LINE-EDIT-SEC          SECTION.
      *
           MOVE    1               TO  IDX-SUM
      *
           COMPUTE WRK-IDX
               =   SPA-I0-SYUNOU-CNT   -   1
      *
           MOVE    ZERO
                       TO  SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                           SPA-NAI-I04-SEIKYU-PLUS (SPA-I0-SYUNOU-CNT)
                           SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT 1)
                           SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT 2)
      *                        SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL ( IDXY            >   WRK-IDX )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDXY)
                                           =   1 )
      *
                   MOVE    IDXY    TO  IDX-SUM
                   MOVE    ZERO
                           TO  SPA-NAI-I04-SEIKYU (IDX-SUM)
                               SPA-NAI-I04-SEIKYU-PLUS (IDX-SUM)
                               SPA-NAI-I04-CHOSEI (IDX-SUM 1)
                               SPA-NAI-I04-CHOSEI (IDX-SUM 2)
                               SPA-NAI-I04-NYUKIN (IDX-SUM)
               ELSE
      *
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDXY)
                                           =   ZERO )
      *                今回請求額
                       COMPUTE SPA-NAI-I04-SEIKYU     (IDXY)
                           =   SPA-NAI-I04-SEIKYU-KON (IDXY)
                           +   SPA-NAI-I04-CHOSEI     (IDXY 1)
                           -   SPA-NAI-I04-CHOSEI-MOT (IDXY 1)
                           +   SPA-NAI-I04-CHOSEI     (IDXY 2)
                           -   SPA-NAI-I04-CHOSEI-MOT (IDXY 2)
      *
                       MOVE    SPA-NAI-I04-SEIKYU (IDXY)
                                       TO  SPA-NAI-I04-SEIKYU-PLUS
                                                              (IDXY)
      *
                       IF    ( SPA-NAI-I04-SEIKYU-PLUS (IDXY)
                                       <   ZERO )
                               MOVE    ZERO
                                       TO  SPA-NAI-I04-SEIKYU-PLUS
                                                              (IDXY)
                       END-IF
      *
      *                請求金額
                       COMPUTE SPA-NAI-I04-SEIKYU (IDX-SUM)
                           =   SPA-NAI-I04-SEIKYU (IDX-SUM)
                           +   SPA-NAI-I04-SEIKYU (IDXY)
                       COMPUTE SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                           =   SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                           +   SPA-NAI-I04-SEIKYU (IDXY)
      *
      *                請求金額（プラス分）
                       COMPUTE SPA-NAI-I04-SEIKYU-PLUS
                                                  (IDX-SUM)
                           =   SPA-NAI-I04-SEIKYU-PLUS
                                                  (IDX-SUM)
                           +   SPA-NAI-I04-SEIKYU-PLUS
                                                  (IDXY)
                       COMPUTE SPA-NAI-I04-SEIKYU-PLUS
                                                 (SPA-I0-SYUNOU-CNT)
                           =   SPA-NAI-I04-SEIKYU-PLUS
                                                 (SPA-I0-SYUNOU-CNT)
                           +   SPA-NAI-I04-SEIKYU-PLUS
                                                 (IDXY)
      *
      *                調整金額
                       COMPUTE SPA-NAI-I04-CHOSEI (IDX-SUM 1)
                           =   SPA-NAI-I04-CHOSEI (IDX-SUM 1)
                           +   SPA-NAI-I04-CHOSEI (IDXY 1)
                       COMPUTE SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT 1)
                           =   SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT 1)
                           +   SPA-NAI-I04-CHOSEI (IDXY 1)
      *
                       COMPUTE SPA-NAI-I04-CHOSEI (IDX-SUM 2)
                           =   SPA-NAI-I04-CHOSEI (IDX-SUM 2)
                           +   SPA-NAI-I04-CHOSEI (IDXY 2)
                       COMPUTE SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT 2)
                           =   SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT 2)
                           +   SPA-NAI-I04-CHOSEI (IDXY 2)
      *
      *                入金額
                       COMPUTE SPA-NAI-I04-NYUKIN (IDX-SUM)
                           =   SPA-NAI-I04-NYUKIN (IDX-SUM)
                           +   SPA-NAI-I04-NYUKIN (IDXY)
      *                COMPUTE SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
      *                    =   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
      *                    +   SPA-NAI-I04-NYUKIN (IDXY)
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL ( IDXY            >   SPA-I0-SYUNOU-CNT )
      *
               MOVE    SPA-NAI-I04-NYUKIN (IDXY)
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  SPA-NAI-I04-NYUKIN-X
                                                               (IDXY)
           END-PERFORM
      *
           .
       4101-TTL-LINE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入金方法チェック処理
      *****************************************************************
       4100-NYKNHOHO-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-I04-NYKNHOHO   =   SPACE )
               MOVE    "0009"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I04-CUR
               GO  TO  4100-NYKNHOHO-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-NYKNHOHO-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYKNHOHO-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0010"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I04-CUR
               GO  TO  4100-NYKNHOHO-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-NYKNHOHO
               IF    ( WRK-CMB-ITM2    NOT = SPA-NAI-I04-NYKNJYOTAI )
                   MOVE    1       TO  SPA-GMN-I04-CUR
               END-IF
               MOVE   WRK-CMB-ITM2 TO  SPA-NAI-I04-NYKNJYOTAI
           END-IF
      *
           .
       4100-NYKNHOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金の取扱いチェック処理
      *****************************************************************
       4100-NYKNKNRKBN-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-I04-NYKNKNRKBN   =   SPACE )
               MOVE    "0011"      TO  SPA-ERRCD
               MOVE    7           TO  SPA-GMN-I04-CUR
               GO  TO  4100-NYKNKNRKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-NYKNKNRKBN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYKNKNRKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0012"      TO  SPA-ERRCD
               MOVE    7           TO  SPA-GMN-I04-CUR
               GO  TO  4100-NYKNKNRKBN-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-NYKNKNRKBN
           END-IF
      *
      *
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD
                                   NOT =   SPA-GMN-I04-NYKNKNRKBN-BF )
               IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   =   "1" )
      *
                   MOVE    ZERO        TO  WRK-KONNYUKINTTL
      *
                   PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >=  SPA-I0-SYUNOU-CNT )
      *
                       IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX7)
                                       =   ZERO )
      *
                           COMPUTE WRK-KONNYUKINTTL
                               =   WRK-KONNYUKINTTL
                               +   SPA-NAI-I04-NYUKIN (IDX7)
      *
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( WRK-KONNYUKINTTL
                                   NOT =   SPA-NAI-I04-NYUKIN 
                                                   (SPA-I0-SYUNOU-CNT) )
      *
                       MOVE    SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                                               TO  WRK-NYUKINTTL
      *
                       MOVE    SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                                               TO  WRK-NYUKIN
                       MOVE    WRK-KONNYUKINTTL 
                                               TO  SPA-NAI-I04-NYUKIN
                                                    (SPA-I0-SYUNOU-CNT)
                       MOVE    SPA-I0-SYUNOU-CNT   TO  IDX-LST
      *
      *
                       PERFORM 41001-NYUKIN-SORT-SEC
      *
                       MOVE    WRK-NYUKINTTL   TO  SPA-NAI-I04-NYUKIN
                                                    (SPA-I0-SYUNOU-CNT)
      *
                       IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
                        AND  ( SPA-GMN-I04-HKNCOMBILST-SEL
                                   <=  SPA-GMN-I04-HKNCOMBILST-CNT )
                           MOVE    SPA-NAI-I04-TSYUNOUIDX
                                         (SPA-GMN-I04-HKNCOMBILST-SEL)
                                               TO  IDXZ
                           MOVE    SPA-NAI-I04-NYUKIN (IDXZ)
                                               TO  WRK-Z99
                           MOVE    WRK-Z99     TO  SPA-GMN-I04-NYUKIN-X
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-I04-NYKNKNRKBN-CD
                                       TO  SPA-GMN-I04-NYKNKNRKBN-BF
      *
           .
       4100-NYKNKNRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行チェック処理
      *****************************************************************
       4100-SKYPRTOUT-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-I04-SKYPRTOUT   =   SPACE )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SKYPRTOUT
               GO  TO  4100-SKYPRTOUT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-SKYPRTOUT-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKYPRTOUT-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SKYPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SKYPRTOUT
           END-IF
      *
           .
       4100-SKYPRTOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行方法チェック処理
      *****************************************************************
       4100-SKYSUMKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-I04-SKYSUMKBN   =   SPACE )
               MOVE    SPA-GMN-I04-SKYSUMKBN-ITM (1)
                                   TO  SPA-GMN-I04-SKYSUMKBN
               GO  TO  4100-SKYSUMKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-SKYSUMKBN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKYSUMKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SKYSUMKBN-ITM (1)
                                   TO  SPA-GMN-I04-SKYSUMKBN
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SKYSUMKBN
           END-IF
      *
           .
       4100-SKYSUMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行チェック処理
      *****************************************************************
       4100-SRYPRTOUT-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-I04-SRYPRTOUT   =   SPACE )
               MOVE    SPA-GMN-I04-SRYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SRYPRTOUT
               GO  TO  4100-SRYPRTOUT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-SRYPRTOUT-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SRYPRTOUT-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SRYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SRYPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SRYPRTOUT
           END-IF
      *
           .
       4100-SRYPRTOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行チェック処理
      *****************************************************************
       4100-TAIPRTOUT-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-I04-TAIPRTOUT   =   SPACE )
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-TAIPRTOUT
               GO  TO  4100-TAIPRTOUT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-TAIPRTOUT-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-TAIPRTOUT-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (2)
                                   TO  SPA-GMN-I04-TAIPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-TAIPRTOUT
           END-IF
      *
           .
       4100-TAIPRTOUT-CHK-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    院外処方せん発行チェック処理
      *****************************************************************
       4100-SHOHOU-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-I04-SHOHOU   =   SPACE )
               MOVE    SPA-GMN-I04-SHOHOU-ITM (1)
                                   TO  SPA-GMN-I04-SHOHOU
           END-IF
      *
           MOVE    ZERO            TO  WRK-CMB-IDX
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-SHOHOU-CNT )
      *
               IF    ( SPA-GMN-I04-SHOHOU-ITM (IDX-CMB)(1:1)
                                   =   SPA-GMN-I04-SHOHOU-CD )
                   MOVE    SPA-GMN-I04-SHOHOU-ITM (IDX-CMB)
                                           TO  SPA-GMN-I04-SHOHOU
                   MOVE    IDX-CMB         TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I04-SHOHOU-CNT
                                           TO  IDX-CMB
               END-IF
           END-PERFORM
           IF    ( WRK-CMB-IDX     =   ZERO )
               MOVE    SPA-GMN-I04-SHOHOU-ITM (1)
                                   TO  SPA-GMN-I04-SHOHOU
           END-IF
      *
           .
       4100-SHOHOU-CHK-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    主治医氏名チェック処理
      *****************************************************************
       4100-DR-CHK-SEC                 SECTION.
      *
           IF    ( SPA-GMN-I04-DR      =   SPACE )
               GO  TO  4100-DR-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-DR-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DR-CHK-SEC
           MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-DR
      *
           .
       4100-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザープログラム
      *****************************************************************
       4100-USERPG-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-I04-USERPG-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-USERPG-CHK-SEC
           IF    ( WRK-CMB-ITM =   SPACE )
               IF    ( SPA-NAI-I04-GYOUMU-ARI  =   "1" )
                   IF    ( SPA-NAI-I04-GYOUMU-FLG  =   "1" )
                       MOVE    SPA-GMN-I04-USERPG-ITM (2)
                                   TO  SPA-GMN-I04-USERPG
                   ELSE
                       MOVE    SPA-GMN-I04-USERPG-ITM (1)
                                   TO  SPA-GMN-I04-USERPG
                   END-IF
               ELSE
                   MOVE        SPA-GMN-I04-USERPG-ITM (1)
                                   TO  SPA-GMN-I04-USERPG
               END-IF
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-USERPG
           END-IF
      *
           .
       4100-USERPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    101                 TO  SPA-GMN-I04-CUR
           MOVE    I04-SELNUM          TO  SPA-GMN-I04-SELNUM
      *
           IF    ( SPA-GMN-I04-SELNUM  =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I04-SELNUM  >   SPA-GMN-I04-HKNCOMBILST-CNT )
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-SELNUM  TO  SPA-GMN-I04-HKNCOMBILST-SEL
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX (SPA-GMN-I04-SELNUM)
                                       TO  WRK-LST-IDX
           PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
           MOVE    1                   TO  SPA-GMN-I04-CUR
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    再計算方法変更処理
      *****************************************************************
       4100-SAIKEISAN-CHG-SEC          SECTION.
      *
           IF    ( SPA-NAI-I04-SAIKEISANFLG    =   ZERO )
               MOVE    1               TO  SPA-NAI-I04-SAIKEISANFLG
           ELSE
               MOVE    ZERO            TO  SPA-NAI-I04-SAIKEISANFLG
           END-IF
      *
           PERFORM 3100-SEIKYU-DAT-INIT-SEC
      *
           PERFORM 3100-MISYU-DAT-EDIT-SEC
      *
           IF    ( SPA-NAI-I04-NYKNJYOTAI  =   "1" )
               PERFORM 3100-NYUKIN-DAT-EDIT-SEC
           END-IF
      *
           PERFORM 4101-TTL-LINE-EDIT-SEC
      *
           PERFORM 3100-HKNCOMBILST-EDIT-SEC
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX(SPA-GMN-I04-SELNUM)
                                   TO  WRK-LST-IDX
           PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
           PERFORM 31001-TEIKISAGAKU-HEN-SEC
      *
           .
       4100-SAIKEISAN-CHG-EXT.
           EXIT.
      *****************************************************************
      *    労災・定期請求差額表示切替処理
      *****************************************************************
       4100-RSITEIKI-CHG-SEC           SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-NAI-I04-RSITEIKIFLG =   CONST-EDIT-RSIJBI )
               MOVE    CONST-EDIT-TEIKI
                                   TO  SPA-NAI-I04-RSITEIKIFLG
               PERFORM 31001-TEIKISAGAKU-HEN-SEC
           ELSE
               MOVE    CONST-EDIT-RSIJBI
                                   TO  SPA-NAI-I04-RSITEIKIFLG
           END-IF
      *
           .
       4100-RSITEIKI-CHG-EXT.
           EXIT.
      *****************************************************************
      *    一括入金処理
      *****************************************************************
       4100-IKKATU-NYUKIN-SEC          SECTION.
      *
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL ( IDX-SYU     >   SPA-I0-SYUNOU-CNT )
               MOVE    ZERO        TO  SPA-NAI-I04-NYUKIN (IDX-SYU)
           END-PERFORM
      *
           MOVE    SPA-I0-SYUNOU-CNT   TO  IDX-LST
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   =   "1" )
               MOVE    SPA-NAI-I04-SEIKYU-PLUS (SPA-I0-SYUNOU-CNT)
                                           TO  WRK-NYUKIN
           ELSE
               MOVE   SPA-GMN-I04-SEIGOK  TO  WRK-NYUKIN
           END-IF
      *
           IF    ( WRK-NYUKIN      <   ZERO )
               MOVE    ZERO        TO  WRK-NYUKIN
           END-IF
      *
           PERFORM 4101-NYUKIN-SORT-SEC
      *
      *    合計行編集処理
           PERFORM 4101-TTL-LINE-EDIT-SEC
      *
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
            AND  ( SPA-GMN-I04-HKNCOMBILST-SEL
                           <=  SPA-GMN-I04-HKNCOMBILST-CNT )
               MOVE    SPA-NAI-I04-TSYUNOUIDX
                                  (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDXZ
               MOVE    SPA-NAI-I04-NYUKIN (IDXZ)
                                       TO  SPA-GMN-I04-NYUKIN
               MOVE    SPA-NAI-I04-NYUKIN-X (IDXZ)
                                       TO  SPA-GMN-I04-NYUKIN-X
           END-IF
      *
      *    合計再計算
           COMPUTE SPA-GMN-I04-MISYUGOK
               =   SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
               +   SPA-GMN-I04-ZENMISYU-NYU
               -   SPA-GMN-I04-ZENKANYU-NYU
               -   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
      *
           .
       4100-IKKATU-NYUKIN-EXT.
           EXIT.
      *****************************************************************
      *    入金方法サーチ処理
      *****************************************************************
       4103-CMB-NYKNHOHO-CHK-SEC   SECTION.
      *
           INITIALIZE              WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-IDX
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-NYKNHOHO-CNT )
                     OR  ( SPA-GMN-I04-NYKNHOHO-ITM (IDX-CMB)(1:2)
                                   =   WRK-CMB-CD )
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-I04-NYKNHOHO-CNT
                                   >=  IDX-CMB    )
               MOVE    SPA-GMN-I04-NYKNHOHO-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               MOVE    SPA-GMN-I04-NYKNJYOTAI-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM2
               MOVE    IDX-CMB     TO  WRK-CMB-IDX
           END-IF
      *
           .
      *
       4103-CMB-NYKNHOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金の取扱いサーチ処理
      *****************************************************************
       4103-CMB-NYKNKNRKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-NYKNKNRKBN-CNT )
                     OR  ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I04-NYKNKNRKBN-ITM (IDX-CMB)(1:1)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I04-NYKNKNRKBN-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-NYKNKNRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行サーチ処理
      *****************************************************************
       4103-CMB-SKYPRTOUT-CHK-SEC  SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-SKYPRTOUT-CNT )
                     OR  ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I04-SKYPRTOUT-ITM (IDX-CMB)(1:1)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-SKYPRTOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行方法サーチ処理
      *****************************************************************
       4103-CMB-SKYSUMKBN-CHK-SEC  SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-SKYSUMKBN-CNT )
                     OR  ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I04-SKYSUMKBN-ITM (IDX-CMB)(1:1)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I04-SKYSUMKBN-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-SKYSUMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書リストチェック処理
      *****************************************************************
       4103-CMB-SRYPRTOUT-CHK-SEC  SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-SRYPRTOUT-CNT )
                     OR  ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I04-SRYPRTOUT-ITM (IDX-CMB)(1:1)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I04-SRYPRTOUT-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-SRYPRTOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行サーチ処理
      *****************************************************************
       4103-CMB-TAIPRTOUT-CHK-SEC  SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-TAIPRTOUT-CNT )
                     OR  ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I04-TAIPRTOUT-ITM (IDX-CMB)(1:1)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
      *
           END-PERFORM

      *
           .
      *
       4103-CMB-TAIPRTOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主治医サーチ処理
      *****************************************************************
       4103-CMB-DR-CHK-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-DR-CNT )
                     OR  ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I04-DR-ITM (IDX-CMB)(1:1)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I04-DR-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
      *
           END-PERFORM
      *
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1010"          TO  SYS-KANRICD
               MOVE    "1"             TO  SYS-KBNCD (1:1)
               MOVE    WRK-CMB-CD      TO  SYS-KBNCD (2:)
               MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-KBNCD (2:4)
                                           TO  WRK-CMB-ITM (1:4)
                   MOVE    SYS-1010-NAME   TO  WRK-CMB-ITM (6:)
               END-IF
      *
           END-IF
      *
           .
      *
       4103-CMB-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザープログラムコンボチェック処理
      *****************************************************************
       4103-CMB-USERPG-CHK-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-USERPG-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I04-USERPGL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I04-USERPG-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-USERPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "I01"               TO  SPA-SAKIPG
           MOVE    "I04"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    出産育児一時金画面遷移処理
      *****************************************************************
       220-S08-SENI-SEC                    SECTION.
      *
           MOVE    SPA-NAI-I04-PTID    TO  SPA-PTID
           MOVE    SPA-GMN-I04-PTNUM   TO  SPA-PTNUM
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
           INITIALIZE                      LNKS08-AREA
           MOVE    "I01"               TO  LNKS08-SAKIPG
           MOVE    SPA-NAI-I04-PTRRKNUM
                                       TO  LNKS08-RRKNUM
           MOVE    LNKS08-AREA         TO  SPA-WORK
      *
           MOVE    "S08"               TO  SPA-SAKIPG
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "I04"               TO  SPA-I0-MOTOPG2
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I0SUB01-LNK
           MOVE    SPA-TERMID          TO  I0SUB01-TERMID
           MOVE    "O"                 TO  I0SUB01-KBN
           MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
           MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
           CALL    "ORCGI0SUB01"       USING   I0SUB01-LNK
                                               SPA-AREA
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    "S02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       220-S08-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       450-TOUROKU-SEC             SECTION.
      *
      *    定期請求処理中チェック
           PERFORM 4801-TEIKI-JOB-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    入力時チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    関連チェック処理
           PERFORM 4801-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
           IF    ( SPA-I01-I04-PROCKBN     =   "2" )
               PERFORM 4502-TOUROKU-SEC
           ELSE
               PERFORM 4501-TOUROKU-SEC
           END-IF
      *
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理 （退院登録）
      *****************************************************************
       4501-TOUROKU-SEC            SECTION.
      *
           INITIALIZE              BFSYU-AREA
      *
      *    ＤＢ更新処理
           PERFORM 451-TOUROKU-SYORI-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
      *        帳票出力処理
               PERFORM 451-PRTOUT-SEC
      *
      *        前回患者情報を設定
               MOVE SPA-NAI-I04-PTID   TO  SPA-LAST-PTID
               MOVE SPA-GMN-I04-PTNUM  TO  SPA-LAST-PTNUM
      *
      *        ユーザープログラム
               PERFORM 4801-USERPG-SEC
      *
      *        PUSH通知
               PERFORM 4801-PUSH-HOSPITAL-STAY-SEC
      *
               IF    ( FLG-END         =   ZERO )
                   IF    ( SPA-I01-I04-BUNBENFTN-FLG   =   1 )
                       PERFORM 220-S08-SENI-SEC
                   ELSE
                       MOVE    "B12"       TO  SPA-I01-I04-BTN
                       MOVE    "JOIN"      TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                   END-IF
               END-IF
           END-IF
      *
           .
       4501-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理 （退院再計算）
      *****************************************************************
       4502-TOUROKU-SEC            SECTION.
      *
           IF    ( SPA-NAI-I04-SAIKEISANFLG    =   ZERO )
               MOVE    "3001"      TO  SPA-I0IDCD
           ELSE
               PERFORM VARYING  IDX-SYU    FROM    1   BY  1
                       UNTIL  ( IDX-SYU    >   SPA-I0-SYUNOU-CNT )
                        OR    ( SPA-I0IDCD NOT =   SPACE )
                   IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-SYU)  =   3 )
                       MOVE    "3002"  TO  SPA-I0IDCD
                   END-IF
               END-PERFORM
      *
           END-IF
      *
           IF    ( SPA-I0IDCD  =   SPACE )
               PERFORM 452-TOUROKU-SEC
           END-IF
      *
           .
       4502-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録　処理
      *****************************************************************
       451-TOUROKU-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  WKPTNYUINRRK-REC
           INITIALIZE                      WKPTNYUINRRK-REC
      *
      *    退院日更新処理
           PERFORM 4801-TAIINYMD-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  451-TOUROKU-SYORI-EXT
           END-IF
      *
           MOVE    WKPTNYUINRRK-REC    TO  PTNYUINRRK-REC
      *
           IF    ( SPA-I01-I04-TORIKESI-FLG    =   1 )
      *        請求取消更新処理
               PERFORM 4701-TORIKESI-KOUSHIN-SEC
           END-IF
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           MOVE    NACCT-SRYYM TO  WRK-LASTSRYYM
      *
      *    入院会計テーブルを編集する（退院時）
           PERFORM 4801-TAIINKAIKEI-EDIT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1)
                                   =   ZERO )
                   MOVE    IDX1    TO  IDX-SYU
      *            収納マスタ更新処理
                   PERFORM 4801-SYUNOU-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  451-TOUROKU-SYORI-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   NOT =   "1" )
      *
      *        前回未収更新処理
               PERFORM 4801-ZENMISYU-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  451-TOUROKU-SYORI-EXT
               END-IF
           END-IF
     *
           IF    ( SPA-I01-I04-BUNBENFTN-FLG   =   1 )
               PERFORM 4801-BUNBEN-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  451-TOUROKU-SYORI-EXT
               END-IF
           END-IF
     *
           PERFORM 4801-ORCSS010-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  451-TOUROKU-SYORI-EXT
           END-IF
     *
      *    患者情報の入院歴番号を更新する
           PERFORM 4801-PTINF-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  451-TOUROKU-SYORI-EXT
           END-IF
      *
      *    PERFORM 800-ORCSSYUKAMAIN-SEC
      *
           .
       451-TOUROKU-SYORI-EXT.
           EXIT.
      *
      **************************************************************
      *    請求取消更新処理
      **************************************************************
       4701-TORIKESI-KOUSHIN-SEC       SECTION.
      *
      *    患者定期請求履歴を更新する
           PERFORM 4701-PTTEIKIRRK-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4701-TORIKESI-KOUSHIN-EXT
           END-IF
      *
      *    収納データを更新する
           PERFORM 4701-SYUNOU-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4701-TORIKESI-KOUSHIN-EXT
           END-IF
      *
           .
       4701-TORIKESI-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       4701-SYUNOU-UPD-SEC             SECTION.
      *
           PERFORM 900-SYUNOU-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SYU-NYUIN-RRKNUM    =   SPA-NAI-I04-PTRRKNUM )
                AND  ( SYU-DENPJTIKBN  NOT =   "3" )
      *
                   PERFORM 47011-SYUNOU-UPD-SEC
      *
               END-IF
      *
               PERFORM 900-SYUNOU-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4701-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       47011-SYUNOU-UPD-SEC             SECTION.
      *
           COMPUTE WRK-SKYMONEY    =   SYU-SKYMONEY        *   -1
      *
      *    請求取消として収納データを更新
           MOVE    ZERO        TO  SYU-SKYMONEY
           MOVE    "3"         TO  SYU-DENPJTIKBN
      *
      *    収納明作成処理
           PERFORM 4701-SYUMEI-SAKUSEI-SEC
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
               MOVE    SYUNOU-REC      TO  MCPDATA-REC
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       47011-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       4701-SYUMEI-SAKUSEI-SEC         SECTION.
      *
           PERFORM 800-SS002-SEC
      *
      *    伝票番号枝番を求める
           PERFORM 900-SYUMEI-KEY14-SEL-SEC
           COMPUTE WRK-DENPEDANUM
               =   SMEI-DENPEDANUM     +   1
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    WRK-SKYMONEY        TO  SMEI-SKYMONEY
      *
      *入返金区分
           MOVE    SPACE               TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "5"                 TO  SMEI-JOUTAIKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
      *    収納明細追加
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4003"          TO  SPA-ERRCD
               GO  TO  4701-SYUMEI-SAKUSEI-EXT
           END-IF
      *
           .
       4701-SYUMEI-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴更新処理
      *****************************************************************
       4701-PTTEIKIRRK-UPD-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
      *    患者請求履歴更新
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  PTTEIKIRRK-PTID
           MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  PTTEIKIRRK-SKYSTYMD
           MOVE    SPA-I01-I04-SKYEDYMD
                                       TO  PTTEIKIRRK-SKYEDYMD
           MOVE    SPA-I01-I04-RRKNUM
                                       TO  PTTEIKIRRK-RRKNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTTEIKIRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTTEIKIRRK-UPHMS
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC         NOT =   ZERO )
               MOVE    "4004"          TO  SPA-ERRCD
               GO  TO  4701-PTTEIKIRRK-UPD-EXT
           END-IF
      *
           .
       4701-PTTEIKIRRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理（退院再計算）
      *****************************************************************
       452-TOUROKU-SEC             SECTION.
      *
           INITIALIZE              BFSYU-AREA
      *
      *    ＤＢ更新処理
           PERFORM 452-TOUROKU-SYORI-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  452-TOUROKU-EXT
           END-IF
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
           MOVE    PTNYUINRRK-REC  TO  WKPTNYUINRRK-REC
      *
      *    帳票出力処理
           PERFORM 451-PRTOUT-SEC
      *
      *    ユーザープログラム
           PERFORM 4801-USERPG-SEC
      *
      *    PUSH通知
           PERFORM 4801-PUSH-HOSPITAL-STAY-SEC
      *
           IF    ( FLG-END         =   ZERO )
               IF    ( SPA-I01-I04-BUNBENFTN-FLG   =   1 )
                   PERFORM 220-S08-SENI-SEC
               ELSE
                   MOVE    "B12"       TO  SPA-I01-I04-BTN
                   MOVE    "JOIN"      TO  MCP-PUTTYPE
                   PERFORM 210-BACK
               END-IF
           END-IF
      *
           .
      *
       452-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       452-TOUROKU-SYORI-SEC           SECTION.
      *
           PERFORM VARYING IDX-SYU    FROM    1   BY  1
                   UNTIL ( IDX-SYU        >   SPA-I0-SYUNOU-CNT )
                     OR  ( SPA-ERRCD  NOT =   SPACE )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-SYU)  =   ZERO 
                                                           OR  3 )
                   EVALUATE    SPA-I0-SYUNOU-UPDFLG (IDX-SYU)
      *            収納訂正
                   WHEN    ZERO
                       IF    ( SPA-NAI-I04-SAIKEISANFLG    =   1 )
                           PERFORM 4801-SYUNOU-UPD-SEC
                       ELSE
                           MOVE    SPA-I0-SYUNOU-DAT (IDX-SYU)
                                           TO  SYUNOU-REC
                           PERFORM 900-SYUNOU-KEY-SEL-SEC
                           IF    ( FLG-SYUNOU =   ZERO )
                               PERFORM 47011-SYUNOU-UPD-SEC
                           ELSE
                               MOVE   "4035"  TO  SPA-ERRCD
                               PERFORM 910-DBABORT-SEC
                           END-IF
      *
                           IF    ( SPA-ERRCD  =   SPACE )
                               PERFORM 4801-SYUNOU-UPD-SEC
                           END-IF
                       END-IF
      *
      *            収納作成
                   WHEN    1
                       PERFORM 4801-SYUNOU-UPD-SEC
      *            収納取消
                   WHEN    2
      *
                       MOVE    SPA-I0-SYUNOU-DAT (IDX-SYU)
                                           TO  SYUNOU-REC
                       PERFORM 900-SYUNOU-KEY-SEL-SEC
                       IF    ( FLG-SYUNOU =   ZERO )
                           PERFORM 47011-SYUNOU-UPD-SEC
                       ELSE
                           MOVE   "4034"  TO  SPA-ERRCD
                           PERFORM 910-DBABORT-SEC
                       END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
     *
           IF    ( SPA-I01-I04-BUNBENFTN-FLG   =   1 )
               PERFORM 4802-BUNBEN-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  452-TOUROKU-SYORI-EXT
               END-IF
           END-IF
      *
      *    PERFORM 800-ORCSSYUKAMAIN-SEC
      *
           .
       452-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    定期請求処理中チェック
      *****************************************************************
       4801-TEIKI-JOB-CHK-SEC          SECTION.
      *
      *    ジョブ管理チェック処理
           INITIALIZE                  STEIKIJOB-AREA
           CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
                                               SPA-AREA
           IF    ( STEIKIJOB-ERRCD NOT =   SPACE )
                   MOVE    "1001"      TO  SPA-ERRCD
           END-IF
      *
           .
       4801-TEIKI-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4801-KANREN-CHK-SEC         SECTION.
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-I04-HKNCOMBILST-CNT )
                     OR  ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    SPA-NAI-I04-TSYUNOUIDX (IDX-LST)
                                               TO  IDX-SYU
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX-SYU)  =   ZERO )
                   IF    ( SPA-NAI-I04-SEIKYU (IDX-SYU)   >=   ZERO )
                    AND  ( SPA-NAI-I04-SEIKYU (IDX-SYU)
                                       <   SPA-NAI-I04-NYUKIN (IDX-SYU))
                       MOVE    "0008"  TO  SPA-ERRCD
                       MOVE    IDX-LST TO  SPA-GMN-I04-SELNUM
                                           SPA-GMN-I04-HKNCOMBILST-SEL
                       MOVE    IDX-SYU TO  WRK-LST-IDX
                       PERFORM 3100-SEIKYU-DAT-EDIT-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       4801-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院日更新処理
      *****************************************************************
       4801-TAIINYMD-UPD-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-TAIINUPDCNT
      *
           PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
      *
      *    入院歴の退院日を転科歴も含めて更新する。
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               COMPUTE WRK-TAIINUPDCNT
                   =   WRK-TAIINUPDCNT +   1
      *
               IF    ( WRK-TAIINUPDCNT =   1 )
                   IF    ( SPA-NAI-I04-PTRRKNUM
                                       =   PTNYUINRRK-RRKNUM )
                    AND  ( SPA-NAI-I04-PTRRKEDANUM
                                       =   PTNYUINRRK-RRKEDANUM )
      *
                       MOVE    SPA-NAI-I04-TAIINYMD
                                       TO  PTNYUINRRK-TAIINYMD
                                           PTNYUINRRK-TENSTUYMD
      *
                       MOVE    PTNYUINRRK-REC
                                       TO  WKPTNYUINRRK-REC
      *
                   ELSE
                       MOVE    "4007"  TO  SPA-ERRCD
                       GO  TO  4801-TAIINYMD-UPD-EXT
                   END-IF
               ELSE
                   MOVE    SPA-NAI-I04-TAIINYMD
                                       TO  PTNYUINRRK-TAIINYMD
               END-IF
      *
               MOVE    SPA-NAI-I04-TAIINCD
                                       TO  PTNYUINRRK-TAIINCD
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
      *
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key"           TO MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4008"  TO  SPA-ERRCD
                   GO  TO  4801-TAIINYMD-UPD-EXT
               END-IF
      *
      *
               PERFORM 900-PTNYUINRRK-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-TAIINUPDCNT     =   ZERO )
               MOVE    "4009"  TO  SPA-ERRCD
               GO  TO  4801-TAIINYMD-UPD-EXT
           END-IF
      *
      *
           .
       4801-TAIINYMD-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院　会計テーブル編集処理
      *****************************************************************
       4801-TAIINKAIKEI-EDIT-SEC         SECTION.
      *
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "2"     TO   LNK-SCNYUACCT-SYORI-KBN
           IF     SPA-GMN-I01-SYORIKBN-CD   =  "11"
               MOVE     "A"   TO   LNK-SCNYUACCT-SYORI-KBN
           END-IF
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE     PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
      *    保険組合せ
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA            TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-TAIINKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ更新処理
      *****************************************************************
       4801-SYUNOU-UPD-SEC         SECTION.
      *
           MOVE    SPA-I0-SYUNOU-DAT (IDX-SYU)
                                   TO  SYUNOU-REC
      *
           IF    ( SPA-I0-SYUNOU-UPDFLG (IDX-SYU)  =   ZERO )
            AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
      *
               COMPUTE WRK-SKYMONEY
                   =   SPA-NAI-I04-SEIKYU (IDX-SYU)
                   +   SPA-NAI-I04-SEIKYU-MOT (IDX-SYU)
                   -   SPA-I0-UPD-SKYMONEY(IDX-SYU)
      *
               MOVE    SYU-DENPNUM TO  WRK-DENPNUM
           ELSE
               COMPUTE WRK-SKYMONEY
                   =   SPA-NAI-I04-SEIKYU (IDX-SYU)
               MOVE    ZERO        TO  SYU-NYUKIN-TOTAL
                                       SYU-NYUKIN-KAISU
      *
      *        伝票番号取得処理
               PERFORM 4801-DENPNUM-SET-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4801-SYUNOU-UPD-EXT
               END-IF
           END-IF
      *
      *
           IF    ( SPA-I01-I04-SKYSTYMD (1:6)
                                   NOT =   SPA-I01-I04-SKYEDYMD (1:6) )
               IF    ( SPA-I01-I04-SKYSTYMD (1:6)
                                       =   SYU-SKYSTYMD (1:6))
                   MOVE    SPA-I01-I04-SKYSTYMD    TO  SYU-SKYSTYMD
                   MOVE    SPA-I01-I04-SKYSTYMD    TO  WRK-SYMD
                   PERFORM 5002-GETUMATU-SEC
                   MOVE    WRK-SYMD                TO  SYU-SKYEDYMD
               END-IF
               IF    ( SPA-I01-I04-SKYEDYMD (1:6)
                                       =   SYU-SKYEDYMD (1:6))
                   MOVE    SPA-I01-I04-SKYEDYMD (1:6)
                                                   TO  SYU-SKYSTYMD(1:6)
                   MOVE    "01"                    TO  SYU-SKYSTYMD(7:2)
                   MOVE    SPA-I01-I04-SKYEDYMD    TO  SYU-SKYEDYMD
               END-IF
           ELSE
               MOVE    SPA-I01-I04-SKYSTYMD    TO  SYU-SKYSTYMD
               MOVE    SPA-I01-I04-SKYEDYMD    TO  SYU-SKYEDYMD
           END-IF
      *
           MOVE    WRK-DENPNUM     TO  SYU-DENPNUM
      *
      *    請求時最終履歴取得処理
           PERFORM 4801-SEILASTRRK-SET-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE    WRK-NYUINKA     TO  SYU-SRYKA
           MOVE    WRK-BRMNUM      TO  SYU-ROOMNUM
           MOVE    WRK-BTUNUM      TO  SYU-BYOTONUM
           MOVE    WRK-DRCD        TO  SYU-DRCD
      *
           MOVE    SPA-NAI-I04-CHOSEI (IDX-SYU 1)
                                   TO  SYU-CHOSEI1
      *
           MOVE    SPA-NAI-I04-CHOSEI (IDX-SYU 2)
                                   TO  SYU-CHOSEI2
           COMPUTE SYU-CHOSEI
               =   SYU-CHOSEI1
               +   SYU-CHOSEI2
           MOVE    WRK-SKYMONEY    TO  SYU-SKYMONEY
      *
           COMPUTE SYU-NYUKIN-TOTAL
               =   SYU-NYUKIN-TOTAL
               +   SPA-NAI-I04-NYUKIN (IDX-SYU)
      *
           IF    ( SYU-NYUKIN-TOTAL    >   ZERO )
               COMPUTE SYU-NYUKIN-KAISU
                   =   SYU-NYUKIN-KAISU
                   +   1
           END-IF
      *
           IF    ( SYU-SKYMONEY        =   ZERO )
               MOVE    SPACE       TO  SYU-DENPJTIKBN
           ELSE
               IF    ( SYU-SKYMONEY   <=  SYU-NYUKIN-TOTAL )
                   MOVE    "1"     TO  SYU-DENPJTIKBN
               ELSE
                   MOVE    "2"     TO  SYU-DENPJTIKBN
               END-IF
           END-IF
      *
           MOVE    SPA-NAI-I04-PTRRKNUM
                                   TO  SYU-NYUIN-RRKNUM
           EVALUATE    SPA-GMN-I04-SKYSUMKBN-CD
           WHEN    "1"
               MOVE    1           TO  SYU-GRP-HAKHOUFLG
           WHEN    "2"
               MOVE    2           TO  SYU-GRP-HAKHOUFLG
           WHEN    "3"
               MOVE    3           TO  SYU-GRP-HAKHOUFLG
           WHEN    OTHER
               MOVE    0           TO  SYU-GRP-HAKHOUFLG
           END-EVALUATE
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF    ( SPA-I0-SYUNOU-UPDFLG (IDX-SYU)  =   ZERO )
            AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
               CONTINUE
           ELSE
               MOVE    SMCNDATE-YMD
                                   TO  SYU-CREYMD
           END-IF
      *
           MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
           PERFORM 800-SS002-SEC
      *
           IF    ( SPA-I0-SYUNOU-UPDFLG (IDX-SYU)  =   ZERO )
            AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
               MOVE    SYUNOU-REC      TO  MCPDATA-REC
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4032"      TO  SPA-ERRCD
                   GO  TO  4801-SYUNOU-UPD-EXT
               END-IF
      *
           END-IF
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4011"      TO  SPA-ERRCD
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE      SYUNOU-REC    TO  SPA-I0-SYUNOU-DAT (IDX-SYU)
      *
      *    日別収納マスタ更新処理
           PERFORM 4801-SYUDAY-UPD-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
      *    収納明細マスタ更新処理
           PERFORM 4801-SYUMEI-UPD-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           .
       4801-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    日別収納マスタ更新処理
      *****************************************************************
       4801-SYUDAY-UPD-SEC         SECTION.
      *
           INITIALIZE                  SYUDAY-REC
           MOVE    SYU-HOSPNUM      TO  SDAY-HOSPNUM
           MOVE    SYU-NYUGAIKBN   TO  SDAY-NYUGAIKBN
           MOVE    SYU-PTID        TO  SDAY-PTID
           MOVE    SYU-DENPNUM     TO  SDAY-DENPNUM
           MOVE    SYUDAY-REC      TO  MCPDATA-REC
           MOVE    "tbl_syuday"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4033"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               MOVE    SPA-I0-SYUDAY-DAT (IDX-SYU)
                                       TO  SYUDAY-REC
      *
               IF    ( SDAY-DAYINFFLG      =   "1" )
      *
                   MOVE    SYU-DENPNUM     TO  SDAY-DENPNUM
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  SDAY-CREYMD
                                               SDAY-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SDAY-UPHMS
      *
                   MOVE    SYUDAY-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syuday"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4020"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4801-SYUDAY-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納明細マスタ更新処理
      *****************************************************************
       4801-SYUMEI-UPD-SEC         SECTION.
      *
           IF    ( SPA-I0-SYUNOU-UPDFLG (IDX-SYU)  =   ZERO )
            AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
               PERFORM 900-SYUMEI-KEY14-SEL-SEC
               COMPUTE WRK-DENPEDANUM
                   =   SMEI-DENPEDANUM     +   1
               COMPUTE WRK-NYUKINRENNUM
                   =   SMEI-NYUKINRENNUM   +   1
           ELSE
               MOVE    1               TO  WRK-DENPEDANUM
                                           WRK-NYUKINRENNUM
           END-IF
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *    伝票番号枝番
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *    診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *    伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *    請求発行日（発行日の西暦）
           MOVE    SPA-SYSYMD
                                       TO  SMEI-SKYPRINTYMD
      *    請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *    領収書発行日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-RYOSYUPRINTYMD
      *    領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *    請求金額
           MOVE    SPA-NAI-I04-SEIKYU (IDX-SYU)
                                       TO  SMEI-SKYMONEY
      *    入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD
      *    入金連番
           MOVE    WRK-NYUKINRENNUM    TO  SMEI-NYUKINRENNUM
      *
      *    入金額がゼロの時、入金力なし
           IF    ( SYU-NYUKIN-TOTAL    =   ZERO )
               MOVE    SPA-GMN-I04-NYKNHOHO-CD
                                           TO  SMEI-NYUKIN-HOHO
           ELSE
      *        入返金区分
               MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *        入返金額
               MOVE    SPA-NAI-I04-NYUKIN (IDX-SYU)
                                           TO  SMEI-NYUHEN-MONEY
      *        入金方法
               MOVE    SPA-GMN-I04-NYKNHOHO-CD
                                           TO  SMEI-NYUKIN-HOHO
           END-IF
      *    状態区分
           IF    ( SPA-I0-SYUNOU-UPDFLG (IDX-SYU)  =   ZERO )
            AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
               MOVE    "8"                 TO  SMEI-JOUTAIKBN
           ELSE
               IF    ( SYU-SKYMONEY        =   ZERO )
                   MOVE    SPACE               TO  SMEI-JOUTAIKBN
               ELSE
                   IF   (  SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                       MOVE    "9"             TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "1"             TO  SMEI-JOUTAIKBN
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4013"      TO  SPA-ERRCD
               GO  TO  4801-SYUMEI-UPD-EXT
           END-IF
      *
      *
           .
       4801-SYUMEI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    伝票番号取得処理
      *****************************************************************
       4801-DENPNUM-SET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
                                           WRK-DENPNUM
      *
           INITIALIZE                  SYS-0042-REC
           MOVE    SPA-HOSPNUM         TO  SYS-0042-HOSPNUM
           MOVE    "0042"              TO  SYS-0042-KANRICD
           MOVE    "*"                 TO  SYS-0042-KBNCD
           MOVE    SPA-SYSYMD
                                       TO  SYS-0042-STYUKYMD
                                           SYS-0042-EDYUKYMD
           MOVE    SYS-0042-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-0042-REC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                       TO  WRK-DENPNUM-X
               IF    ( WRK-DENPNUM-X   =   SPACE )
                   MOVE    ZERO        TO  SYS-0042-DENPNUMMAX-NYU
               ELSE
                   MOVE    WRK-DENPNUM-X
                                       TO  SYS-0042-DENPNUMMAX-NYU
               END-IF
      *
               COMPUTE SYS-0042-DENPNUMMAX-NYU
                   =   SYS-0042-DENPNUMMAX-NYU +   1
      *
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                           TO  WRK-DENPNUM
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-0042-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-0042-UPHMS
      *
               MOVE    SYS-0042-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4014"              TO  SPA-ERRCD
                   GO  TO  4801-DENPNUM-SET-EXT
               END-IF
           ELSE
               MOVE    "4015"              TO  SPA-ERRCD
               GO  TO  4801-DENPNUM-SET-EXT
           END-IF
      *
           .
       4801-DENPNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求時最終履歴取得処理
      *****************************************************************
       4801-SEILASTRRK-SET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINKA
           MOVE    SPACE               TO  WRK-NYUINKA
                                           WRK-BTUNUM
                                           WRK-BRMNUM
                                           WRK-DRCD
      *
      *    入院履歴マスタ検索
           PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( FLG-NYUINKA =   1 )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               IF    ( PTNYUINRRK-TENNYUYMD
                                       <=  SYU-SKYEDYMD )
                AND  ( PTNYUINRRK-TENSTUYMD
                                       >=  SYU-SKYEDYMD )
                   MOVE    1           TO  FLG-NYUINKA
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-NYUINKA
                   MOVE    PTNYUINRRK-BTUNUM
                                       TO  WRK-BTUNUM
                   MOVE    PTNYUINRRK-BRMNUM
                                       TO  WRK-BRMNUM
                   MOVE    PTNYUINRRK-DRCD1 (2 : 4)
                                       TO  WRK-DRCD
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINKA         =   ZERO )
               MOVE    "4016"  TO  SPA-ERRCD
               GO  TO  4801-SEILASTRRK-SET-EXT
           END-IF
      *
      *
           .
       4801-SEILASTRRK-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理テーブル更新処理
      *****************************************************************
       4801-BUNBEN-UPD-SEC          SECTION.
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( BUNBEN-UPDKBN    =  ZERO )
               PERFORM 48011-BUNBEN-UPD-SEC
           ELSE
               PERFORM 4802-BUNBEN-UPD-SEC
           END-IF
      *
           .
       4801-BUNBEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理テーブル更新処理
      *****************************************************************
       48011-BUNBEN-UPD-SEC          SECTION.
      *
           PERFORM 800-ORCSBUNBENFTN-SEC
      *
           MOVE    BUNBEN-RIYOUKBN TO  LNKBUNBEN-RIYOUKBN
           MOVE    BUNBEN-BUNBENKBN TO LNKBUNBEN-BUNBENKBN
           MOVE    "1"             TO  LNKBUNBEN-UPDKBN
           MOVE    BUNBEN-CREYMD   TO  LNKBUNBEN-CREYMD
           MOVE    LNKBUNBEN-REC   TO  BUNBEN-REC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  BUNBEN-UPYMD
           MOVE    SMCNDATE-HMS    TO  BUNBEN-UPHMS
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4037"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4038"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       48011-BUNBEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理テーブル更新処理
      *****************************************************************
       4802-BUNBEN-UPD-SEC          SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-NAI-I04-TAIINYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           IF    ( SYS-1001-SANKANUM   IS  NOT NUMERIC )
            OR   ( SYS-1001-SANKANUM   =   ZERO    )
               MOVE    ZERO            TO  SYS-1001-SANKANUM
           END-IF
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
           PERFORM 800-ORCSBUNBENFTN-SEC
      *
           MOVE    BUNBEN-RIYOUKBN     TO  LNKBUNBEN-RIYOUKBN
           MOVE    BUNBEN-SKYYM        TO  LNKBUNBEN-SKYYM
           MOVE    BUNBEN-SKYKBN       TO  LNKBUNBEN-SKYKBN
           MOVE    BUNBEN-BUNBENKBN    TO  LNKBUNBEN-BUNBENKBN
           MOVE    BUNBEN-KANYUKBN     TO  LNKBUNBEN-KANYUKBN
           MOVE    BUNBEN-HONKZKKBN    TO  LNKBUNBEN-HONKZKKBN
           MOVE    BUNBEN-HKNJANUM     TO  LNKBUNBEN-HKNJANUM
           MOVE    BUNBEN-HKNJANUM-S   TO  LNKBUNBEN-HKNJANUM-S
           MOVE    BUNBEN-KIGO         TO  LNKBUNBEN-KIGO
           MOVE    BUNBEN-NUM          TO  LNKBUNBEN-NUM
           MOVE    BUNBEN-BIRTHDAY     TO  LNKBUNBEN-BIRTHDAY
           MOVE    BUNBEN-ZAIWEEK      TO  LNKBUNBEN-ZAIWEEK
           MOVE    BUNBEN-SIZANKBN     TO  LNKBUNBEN-SIZANKBN
           MOVE    BUNBEN-BIRTHCNT     TO  LNKBUNBEN-BIRTHCNT
           IF    ( BUNBEN-NYUIN-DAY    NOT =   ZERO )
               MOVE    BUNBEN-NYUIN-DAY TO LNKBUNBEN-NYUIN-DAY
           END-IF
           MOVE    BUNBEN-SANKAKBN     TO  LNKBUNBEN-SANKAKBN
           MOVE    BUNBEN-BIKOU        TO  LNKBUNBEN-BIKOU
           MOVE    "1"                 TO  LNKBUNBEN-UPDKBN
           MOVE    BUNBEN-CREYMD       TO  LNKBUNBEN-CREYMD
           MOVE    BUNBEN-TEISYUTUBIKBN TO LNKBUNBEN-TEISYUTUBIKBN
      *
           MOVE    LNKBUNBEN-REC       TO  BUNBEN-REC
      *
           MOVE    ZERO                TO  WRK-BASE-MONEY
           IF    ( BUNBEN-RIYOUKBN     =   "1" )
               EVALUATE    TRUE
               WHEN  ( SYS-1001-SANKANUM   =   ZERO )
               WHEN  ( BUNBEN-ZAIWEEK      <   22 )
               WHEN  ( BUNBEN-SANKAKBN     =   "2" OR "3" )
                   EVALUATE   TRUE
                   WHEN    ( BUNBEN-BIRTHDAY  >= CONST-H270101 )
                       MOVE    404000  TO  WRK-BASE-MONEY
                   WHEN    OTHER
                       MOVE    390000  TO  WRK-BASE-MONEY
                   END-EVALUATE
               WHEN    OTHER
                   MOVE    420000      TO  WRK-BASE-MONEY
               END-EVALUATE
      *
               IF    ( BUNBEN-SANKAKBN =   "3" )
                   COMPUTE BUNBEN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   BUNBEN-BIRTHCNT
                       +   LNKBUNBEN-SNK-MONEY
               ELSE
                   COMPUTE BUNBEN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   BUNBEN-BIRTHCNT
               END-IF
      *
               IF    ( BUNBEN-DAIRI-MONEY  >   BUNBEN-NINPUFTN-MONEY )
                   MOVE    BUNBEN-NINPUFTN-MONEY
                                       TO  BUNBEN-DAIRI-MONEY
               END-IF
           ELSE
                   MOVE    ZERO    TO  BUNBEN-DAIRI-MONEY
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  BUNBEN-UPYMD
           MOVE    SMCNDATE-HMS    TO  BUNBEN-UPHMS
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4039"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4040"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4802-BUNBEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会テーブル作成処理
      *****************************************************************
       4801-ORCSS010-SEC               SECTION.
      *
           MOVE    SPA-NAI-I04-TAIINYMD
                                   TO  WRK-STYMD
           MOVE    WRK-LASTSRYYM   TO  WRK-EDYMD
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( WRK-EDYMD       =   SPACE )
            OR   ( NACCT-SRYYM >=  WRK-EDYMD (1:6) )
               MOVE    NACCT-SRYYM TO  WRK-EDYMD
           END-IF
      *
           IF    ( WRK-EDYMD       =   SPACE )
               MOVE    "4021"          TO  SPA-ERRCD
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           PERFORM UNTIL ( WRK-STYMD (1:6) >   WRK-EDYMD (1:6))
                    OR   ( SPA-ERRCD  NOT  =   SPACE )
      *
               INITIALIZE                  SS010-AREA
               MOVE    SPA-NAI-I04-PTID    TO  SS010-PTID
               MOVE    WRK-STYMD (1:6)     TO  SS010-SRYYM
               CALL    "ORCSS010"      USING   SS010-AREA
                                               SPA-AREA
               IF    ( SS010-RC        NOT =   ZERO )
                   MOVE    "4022"      TO  SPA-ERRCD
               END-IF
      *
               MOVE    WRK-STYMD       TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE     1              TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-STYMD
      *
           END-PERFORM
      *
           .
       4801-ORCSS010-EXT.
           EXIT.
      *****************************************************************
      *    前回未収更新処理
      *****************************************************************
       4801-ZENMISYU-UPD-SEC           SECTION.
      *
           MOVE    ZERO            TO  WRK-NYUKIN
           IF    ( SPA-GMN-I04-NYKNKNRKBN-CD   = "2" OR "3" )
               COMPUTE WRK-NYUKIN
                   =   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                   -   SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
           ELSE
               MOVE    SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                                   TO  WRK-NYUKIN
           END-IF
      *
           MOVE    SPACE               TO  WRK-PATHNAME
           EVALUATE    SPA-GMN-I04-NYKNKNRKBN-CD
           WHEN    "2"
           WHEN    "4"
               MOVE   "key22"          TO  WRK-PATHNAME
           WHEN    "3"
           WHEN    "5"
               MOVE   "key23"          TO  WRK-PATHNAME
           END-EVALUATE
      *
      *    入院分未収集計
           INITIALIZE                       SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO   SYU-HOSPNUM
           MOVE    "1"                 TO   SYU-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTID    TO   SYU-PTID
           MOVE    SYUNOU-REC          TO   MCPDATA-REC
           PERFORM 900-SYUNOU-SEL-SEC
      *
           PERFORM UNTIL  ( FLG-SYUNOU NOT =  ZERO )
                    OR    ( WRK-NYUKIN    <=  ZERO )
                    OR    ( BFSYU-MAX     >= CONST-BFSYU-MAX )
      *
               IF    ( SPA-I01-I04-SKYSTYMD   <= SYU-SKYSTYMD )
                AND  ( SPA-I01-I04-SKYEDYMD   >= SYU-SKYEDYMD )
                AND  ( SPA-I01-I04-RRKNUM
                                        = SYU-NYUIN-RRKNUM )
                   CONTINUE
               ELSE
                   IF    ( SYU-SKYEDYMD <=   SPA-NAI-I04-TAIINYMD )
                       IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                           COMPUTE BFSYU-MAX   =   BFSYU-MAX   +   1
                           MOVE    SYU-DENPNUM TO  BFSYU-DENPNUM
                                                         (BFSYU-MAX)
                           MOVE    SYU-SKYSTYMD TO BFSYU-SKYSTYMD
                                                         (BFSYU-MAX)
                           COMPUTE BFSYU-SEIKYU (BFSYU-MAX)
                               =   SYU-SKYMONEY
                               -   SYU-NYUKIN-TOTAL
                           IF    ( BFSYU-SEIKYU (BFSYU-MAX)
                                               >=  WRK-NYUKIN )
                               COMPUTE BFSYU-NYUKIN (BFSYU-MAX)
                                   =   WRK-NYUKIN
                               COMPUTE WRK-NYUKIN
                                   =   0
                           ELSE
                               COMPUTE BFSYU-NYUKIN (BFSYU-MAX)
                                   =   BFSYU-SEIKYU (BFSYU-MAX)
                               COMPUTE WRK-NYUKIN
                                   =   WRK-NYUKIN
                                   -   BFSYU-SEIKYU (BFSYU-MAX)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-SYUNOU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           INITIALIZE                      ORCSNYUKINAREAS
           MOVE    SPA-HOSPNUM          TO  LNK-SCNYUKINS-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  LNK-SCNYUKINS-PTID
           MOVE    SPA-SYSYMD          TO  LNK-SCNYUKINS-NYUKIN-YMD
           MOVE    SPA-GMN-I04-NYKNHOHO-CD
                                       TO  LNK-SCNYUKINS-NYUKIN-HOHO
      *
           MOVE    BFSYU-MAX           TO  LNK-SCNYUKINS-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   LNK-SCNYUKINS-MAX )
      *
      *        入外区分
               MOVE    "1"         TO  LNK-SCNYUKINS-NYUGAIKBN    (IDX0)
      *
      *        伝票番号
               MOVE    BFSYU-DENPNUM (IDX0)
                                   TO  LNK-SCNYUKINS-DENPNUM      (IDX0)
      *
      *        入金額
               MOVE    BFSYU-NYUKIN  (IDX0)
                                   TO  LNK-SCNYUKINS-NYUKIN-MONEY (IDX0)
      *
           END-PERFORM
      *
           CALL    "ORCSNYUKINS"   USING   ORCSNYUKINAREAS
                                           SPA-AREA
           IF    ( LNK-SCNYUKINS-RC    NOT =   ZERO )
               MOVE    "4036"          TO  SPA-ERRCD
           END-IF
      *
           .
       4801-ZENMISYU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       4801-PTINF-UPD-SEC          SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    ZERO                TO  FLG-ZI-SAISIN
           PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( MCP-RC          NOT =   ZERO )
                   OR    ( FLG-ZI-SAISIN   NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-ZI-SAISIN
                   MOVE    PTNYUINRRK-RRKNUM
                                       TO  WRK-ZI-SAISIN-RRKNUM
                   MOVE    PTNYUINRRK-RRKEDANUM
                                       TO  WRK-ZI-SAISIN-RRKEDANUM
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY3-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    入院履歴の最大枝番号を更新
           MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-UPYMD
           MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "upd4"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4031"      TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
      *    患者マスタ検索
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF           NOT =   ZERO )
               MOVE    "4017"          TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
      *    患者情報の入院歴番号を更新する
           IF    ( FLG-ZI-SAISIN       =   1 )
               MOVE   WRK-ZI-SAISIN-RRKNUM
                                       TO  PTINF-RRKNUM
               MOVE   WRK-ZI-SAISIN-RRKEDANUM
                                       TO  PTINF-RRKEDANUM
           ELSE
               MOVE    ZERO            TO  PTINF-RRKNUM
                                           PTINF-RRKEDANUM
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
      *    患者情報更新
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF     ( MCP-RC         NOT =  ZERO )
               MOVE    "4018"          TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
           .
       4801-PTINF-UPD-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       451-PRTOUT-SEC              SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
             OR  ( SPA-PRTCONF         =   "1" )
               INITIALIZE                  ORCSPRTCTRLAREA
               CALL    "ORCSPRTCTRL"       USING
                                           SPA-AREA
                                           ORCSPRTCTRLAREA
                                           MCPAREA
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-AREA        TO  WRK-SPA-AREA
      *
           MOVE    "1"             TO  WRK-SPA-NYUGAIKBN
      *
           MOVE    PTINF-PTID      TO  WRK-SPA-PTID
           MOVE    SPA-GMN-I04-PTNUM
                                   TO  WRK-SPA-PTNUM
           MOVE    PTINF-KANANAME
                                   TO  WRK-SPA-KANANAME
           MOVE    PTINF-NAME      TO  WRK-SPA-NAME
           MOVE    PTINF-SEX       TO  WRK-SPA-SEX
           MOVE    PTINF-BIRTHDAY
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SPA-BIRTHDAYW
           MOVE    WRK-SYMD        TO  WRK-SPA-BIRTHDAY
      *
           MOVE    SPA-NAI-I04-TAIINYMD
                                   TO  WRK-SPA-SRYYMD
      *
           PERFORM 4511-SKYSRYPRT-SEC
      *
      *    退院証明書
           IF    ( SPA-GMN-I04-TAIPRTOUT-CD
                                       =   "1" )
               PERFORM 451-TAIPRT-OUT-SEC
           END-IF
      *
      *!!
      *     院外処方せん印刷
           IF      SPA-GMN-I04-SHOHOU-CD   =   "1"
               PERFORM 451-SHOHOSEN-PRINT-SEC
           END-IF
      *!!
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           .
       451-PRTOUT-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    院外処方せん印刷処理
      *****************************************************************
       409-I041-SET-SEC              SECTION.
      *
           IF      SPA-NAI-I041-KBN    =   ZERO
               INITIALIZE                  SPA-I041
      *
               INITIALIZE                  ORCSNYUINSHOAREA
               MOVE    "1"                 TO  ORCSNYUINSHO-KBN
               MOVE    SPA-NAI-I04-NYUINYMD    TO  ORCSNYUINSHO-NYUINYMD
               MOVE    SPA-NAI-I04-TAIINYMD    TO  ORCSNYUINSHO-TAIINYMD
               IF    ( WKPTNYUINRRK-DOUJITSUKBN    =   "1" )
                   MOVE    "1"             TO  ORCSNYUINSHO-DOUJITSUKBN
               END-IF
               CALL    "ORCSNYUINSHO"      USING
                                           WRK-SPA-AREA
                                           ORCSNYUINSHOAREA
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL      (IDX-D   >   ORCSNYUINSHO-SRYKA-MAX )
                   MOVE    ORCSNYUINSHO-SRYKA (IDX-D)
                                           TO  SPA-NAI-I041-SRYKA(IDX-D)
               END-PERFORM
               MOVE    ORCSNYUINSHO-SRYKA-MAX  TO  SPA-GMN-I041-MAX
           END-IF
           IF     (SPA-GMN-I041-MAX      >   ZERO )
               PERFORM 4091-I041-SEC
           ELSE
               MOVE    "0013"            TO  SPA-ERRCD
           END-IF
           .
       409-I041-SET-EXT.
           EXIT.
      *****************************************************************
      *    院外処方せん画面遷移処理
      *****************************************************************
       4091-I041-SEC              SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-I0KYOUTU        TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-I0FREE          TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGI041"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
      *    共通キー編集
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "I041"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I041"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4091-I041-EXT.
           EXIT.
      *****************************************************************
      *    院外処方せん印刷処理
      *****************************************************************
       451-SHOHOSEN-PRINT-SEC              SECTION.
      *
           INITIALIZE                  ORCSNYUINSHOAREA
           MOVE    "2"                 TO  ORCSNYUINSHO-KBN
           IF      SPA-GMN-I04-DR-CD   NOT =   SPACE
               MOVE    "1"             TO  ORCSNYUINSHO-SYUDRCD (1:1)
               MOVE    SPA-GMN-I04-DR-CD
                                       TO  ORCSNYUINSHO-SYUDRCD (2:4)
           END-IF
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   5   )
                         OR   (SPA-NAI-I041-SRYKA (IDX-D)  =   SPACE )
               MOVE    SPA-NAI-I041-SRYKA (IDX-D)
                                           TO  ORCSNYUINSHO-SRYKA(IDX-D)
               IF      SPA-NAI-I041-DRCD (IDX-D)   NOT =   SPACE
                   MOVE    "1"             TO  ORCSNYUINSHO-DRCD (IDX-D)
                                                                 (1:1)
                   MOVE    SPA-NAI-I041-DRCD (IDX-D) 
                                           TO  ORCSNYUINSHO-DRCD (IDX-D)
                                                                 (2:4)
               END-IF
           END-PERFORM
           MOVE    SPA-NAI-I04-NYUINYMD    TO  ORCSNYUINSHO-NYUINYMD
           MOVE    SPA-NAI-I04-TAIINYMD    TO  ORCSNYUINSHO-TAIINYMD
           IF    ( WKPTNYUINRRK-DOUJITSUKBN    =   "1" )
               MOVE    "1"                 TO  ORCSNYUINSHO-DOUJITSUKBN
           END-IF
           MOVE    "04"            TO      WRK-SPA-PRT-FLG
           CALL    "ORCSNYUINSHO"      USING
                                       WRK-SPA-AREA
                                       ORCSNYUINSHOAREA
           MOVE    SPACE           TO      WRK-SPA-PRT-FLG
           .
       451-SHOHOSEN-PRINT-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    請求書兼領収書・診療費明細書発行処理
      *****************************************************************
       4511-SKYSRYPRT-SEC              SECTION.
      *
           INITIALIZE                  WRK-SEIKYU-G
      *
           MOVE    "ORCHCN03"      TO  WRK-ORG-PRINT-PG
           MOVE    "00000003"      TO  WRK-KBNCD
           PERFORM 4511-PRINT-PG-EDIT-SEC
           MOVE    WRK-PRINT-PG    TO  WRK-SEIKYU-PG
      *
           MOVE    "ORCHCN04"      TO  WRK-ORG-PRINT-PG
           MOVE    "00000008"      TO  WRK-KBNCD
           PERFORM 4511-PRINT-PG-EDIT-SEC
           MOVE    WRK-PRINT-PG    TO  WRK-MEISAI-PG
      *
           INITIALIZE                  ORCHCN03AREA
           MOVE    WRK-SPA-HOSPNUM TO  ORCHCN03-HEAD-HOSPNUM
           MOVE    WRK-SPA-STAFFCD TO  ORCHCN03-STAFFCD
           MOVE    WRK-SPA-SYSYMD  TO  ORCHCN03-SYSYMD
           MOVE    WRK-SPA-TERMID  TO  ORCHCN03-TERMID
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   SPA-I0-SYUNOU-CNT )
      *
               MOVE    SPA-I0-SYUNOU-DAT (IDX1)
                                       TO  WKSYUNOU-REC
      *
               IF    ( SPA-I0-SYUNOU-UPDFLG (IDX1)     =   ZERO )
                AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
                   MOVE    SPA-I0-UPD-TAX-SAI  (IDX1)
                                   TO  WRK-SKY-SKYMONEY-TAX-SAI
                   MOVE    SPA-NAI-I04-SEIKYU  (IDX1)
                                   TO  WRK-SKY-SKYMONEY
                   MOVE    SPA-NAI-I04-NYUKIN  (IDX1)
                                   TO  WRK-SKY-NYUKIN-TOTAL
                   COMPUTE WRK-SKY-KONMISYU
                       = ( WKSYU-SKYMONEY      -   WKSYU-NYUKIN-TOTAL)
                       - ( WRK-SKY-SKYMONEY    -   WRK-SKY-NYUKIN-TOTAL)
               ELSE
                   MOVE    WKSYU-SKYMONEY-TAX-SAI
                                   TO  WRK-SKY-SKYMONEY-TAX-SAI
                   MOVE    WKSYU-SKYMONEY
                                   TO  WRK-SKY-SKYMONEY
                   MOVE    WKSYU-NYUKIN-TOTAL
                                   TO  WRK-SKY-NYUKIN-TOTAL
               END-IF
      *
               MOVE  1                 TO  FLG-SKYPRT
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1)
                                   NOT =   ZERO )
                       MOVE    ZERO    TO  FLG-SKYPRT
               END-IF
      *
               IF    ( FLG-SKYPRT      =   1 )
      *
                   COMPUTE ORCHCN03-CNT
                                       =   ORCHCN03-CNT +   1
                   MOVE    ORCHCN03-CNT
                                       TO  IDX2
      *
                   MOVE    WRK-SPA-HOSPNUM
                                       TO ORCHCN03-HOSPNUM (IDX2)
                   MOVE    WRK-SPA-PTID
                                       TO ORCHCN03-PTID   (IDX2)
                   MOVE    WRK-SPA-PTNUM
                                       TO ORCHCN03-PTNUM  (IDX2)
                   MOVE    "1"         TO ORCHCN03-NYUGAIKBN (IDX2)
      *
                   MOVE    WKSYU-SKYSTYMD
                                       TO  ORCHCN03-KJNYMD  (IDX2)
                   MOVE    WKSYU-DENPNUM
                                       TO  ORCHCN03-DENPNUM (IDX2)
                   MOVE    WRK-SKY-SKYMONEY-TAX-SAI
                                       TO  ORCHCN03-SEIKYU-TAX-SAI
                                                            (IDX2)
                   MOVE    WRK-SKY-SKYMONEY
                                       TO  ORCHCN03-SEIKYU  (IDX2)
                   MOVE    WRK-SKY-NYUKIN-TOTAL
                                       TO  ORCHCN03-NYUKIN  (IDX2)
                   IF    ( SPA-I0-SYUNOU-UPDFLG (IDX1)     =   ZERO )
                    AND  ( SPA-NAI-I04-SAIKEISANFLG        =   1 )
                       MOVE    2       TO  ORCHCN03-HAKKOFLG (IDX2)
                   ELSE
                       MOVE    ZERO    TO  ORCHCN03-HAKKOFLG (IDX2)
                   END-IF
                   MOVE    WRK-SKY-KONMISYU
                                       TO  ORCHCN03-KONMISYU (IDX2)
      *
               END-IF
           END-PERFORM
      *
           MOVE    ORCHCN03AREA        TO  WORCHCN03AREA
           PERFORM 4510-SYUGRP-SEC
           MOVE    WORCHCN03AREA       TO  ORCHCN03AREA
      *
           PERFORM 451-SKYPRT-OUT-SEC
      *
           PERFORM 451-SRYPRT-OUT-SEC
      *
           .
       4511-SKYSRYPRT-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書出力処理
      *****************************************************************
       451-SKYPRT-OUT-SEC              SECTION.
      *
           PERFORM VARYING IDX-M   FROM    1   BY  1
                   UNTIL ( IDX-M   >   ORCHCN03-CNT )
      *
               INITIALIZE          ORCHCN03-OCC (IDX-M)
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  ORCHCN03-CNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   TSYUGRP-GRP-MAX )
      *
               IF    ( TSYUGRP-SKYPRT (IDX1)  =   "1" )
      *
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   TSYUGRP-GRP-MAX2 (IDX1))
      *
                       PERFORM VARYING IDX-M   FROM    1   BY  1
                               UNTIL ( IDX-M   >   WORCHCN03-CNT )
      *
                           IF    ( TSYUGRP-GRP-DENPNUM (IDX1 IDX2)
                                       =   WORCHCN03-DENPNUM (IDX-M))
                               COMPUTE ORCHCN03-CNT
                                   =   ORCHCN03-CNT
                                   +   1
                               MOVE    WORCHCN03-OCC (IDX-M)
                                       TO  ORCHCN03-OCC (ORCHCN03-CNT)
                           END-IF
      *
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-I04-SKYPRTOUT-CD   =    "1" OR "2" )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1        >   BFSYU-MAX )
      *
                   COMPUTE ORCHCN03-CNT
                       =   ORCHCN03-CNT +   1
      *
                   MOVE    WRK-SPA-HOSPNUM
                                       TO ORCHCN03-HOSPNUM(ORCHCN03-CNT)
                   MOVE    WRK-SPA-PTID
                                       TO ORCHCN03-PTID   (ORCHCN03-CNT)
                   MOVE    WRK-SPA-PTNUM
                                       TO ORCHCN03-PTNUM  (ORCHCN03-CNT)
                   MOVE    "1"         TO ORCHCN03-NYUGAIKBN
                                                          (ORCHCN03-CNT)
      *
                   MOVE    BFSYU-SKYSTYMD (IDX1)
                                       TO  ORCHCN03-KJNYMD(ORCHCN03-CNT)
                   MOVE    BFSYU-DENPNUM  (IDX1)
                                       TO  ORCHCN03-DENPNUM
                                                          (ORCHCN03-CNT)
                   MOVE    ZERO        TO  ORCHCN03-SEIKYU-TAX-SAI
                                                          (ORCHCN03-CNT)
                   MOVE    BFSYU-SEIKYU   (IDX1)
                                       TO  ORCHCN03-SEIKYU(ORCHCN03-CNT)
                   MOVE    BFSYU-NYUKIN   (IDX1)
                                       TO  ORCHCN03-NYUKIN(ORCHCN03-CNT)
                   MOVE    ZERO        TO  ORCHCN03-HAKKOFLG
                                                          (ORCHCN03-CNT)
                   MOVE    ZERO        TO  ORCHCN03-KONMISYU
                                                          (ORCHCN03-CNT)
               END-PERFORM
           END-IF
      *
      *    入院請求書印刷
           IF    ( ORCHCN03-CNT    >    ZERO )
      *
               MOVE    "01"            TO      WRK-SPA-PRT-FLG
               CALL    WRK-SEIKYU-PG   USING   ORCHCN03AREA
                                               WRK-SPA-AREA
               MOVE    SPACE           TO      WRK-SPA-PRT-FLG
           END-IF
      *
           .
       451-SKYPRT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書出力処理
      *****************************************************************
       451-SRYPRT-OUT-SEC              SECTION.
      *
           MOVE    ZERO            TO  CNT-SRYPRT-PAGE
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   TSYUGRP-GRP-MAX )
      *
               IF    ( TSYUGRP-SRYPRT (IDX1)  =   "1" )
      *
                   INITIALIZE                  ORCHCN04AREA
                   MOVE    WRK-SPA-PTID    TO  ORCHCN04-PTID
                   MOVE    WRK-SPA-PTNUM   TO  ORCHCN04-PTNUM
                   MOVE    WRK-SPA-NAME    TO  ORCHCN04-NAME
                   MOVE    CNT-SRYPRT-PAGE TO  ORCHCN04-PAGE-CNT
                   MOVE    TSYUGRP-SKYSTYMD(IDX1)
                                           TO  ORCHCN04-SKYSTYMD
                   MOVE    TSYUGRP-SKYEDYMD(IDX1)
                                           TO  ORCHCN04-SKYEDYMD
      *
                   PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   TSYUGRP-GRP-MAX2 (IDX1) )
      *
                       MOVE    TSYUGRP-GRP-DENPNUM (IDX1 IDX2)
                                   TO  ORCHCN04-DENPNUM (IDX2)
      *
                   END-PERFORM
      *
                   MOVE    "02"            TO  WRK-SPA-PRT-FLG
                   CALL    WRK-MEISAI-PG   USING
                                           WRK-SPA-AREA
                                           ORCHCN04AREA
                   MOVE    SPACE           TO  WRK-SPA-PRT-FLG
      *
                   MOVE    ORCHCN04-PAGE-CNT
                                           TO  CNT-SRYPRT-PAGE
      *
               END-IF
           END-PERFORM
      *
           .
       451-SRYPRT-OUT-EXT.
           EXIT.
      *****************************************************************
      *   収納データ集計処理
      *****************************************************************
       4510-SYUGRP-SEC             SECTION.
      *
           INITIALIZE              TSYUGRP-AREA
           MOVE    ZERO        TO  IDX-MAX
      *
           PERFORM VARYING IDX-M   FROM    1   BY  1
                   UNTIL ( IDX-M   >   ORCHCN03-CNT )
      *
               PERFORM 901-SYUNOU-KEY-SEL-SEC
               INITIALIZE              ORCHCN03S02AREA
               MOVE    IDX-M       TO  ORCHCN03S02-IDX
               MOVE    1           TO  ORCHCN03S02-CHKPRT
               MOVE    "0"         TO  ORCHCN03S02-PTINFREF
               MOVE    SPA-GMN-I04-SKYPRTOUT-CD
                                   TO  ORCHCN03S02-SKYPRT
               MOVE    SPA-GMN-I04-SRYPRTOUT-CD
                                   TO  ORCHCN03S02-SRYPRT
               CALL    "ORCHCN03S02"   USING
                                       ORCHCN03AREA
                                       ORCHCN03S02AREA
                                       SYUNOU-REC
                                       SPA-AREA
               IF    ( ORCHCN03S02-PRTFLG  =   1 )
      *
                   COMPUTE IDX-MAX =   IDX-MAX +   1
      *
      D            DISPLAY "PTID[" ORCHCN03-PTID (IDX-M) "]"
      D                    "DENPNUM["  ORCHCN03-DENPNUM (IDX-M) "]"
      D                    "ORCHCN03S02-SKYPRT[" ORCHCN03S02-SKYPRT "]"
      D                    "ORCHCN03S02-SRYPRT[" ORCHCN03S02-SRYPRT "]"
                   MOVE    ORCHCN03S02-SKYPRT
                                   TO  TSYUGRP-SKYPRT
                                                  (IDX-MAX)
                   MOVE    ORCHCN03S02-SRYPRT
                                   TO  TSYUGRP-SRYPRT
                                                  (IDX-MAX)
                   MOVE    SYU-SKYSTYMD
                                   TO  TSYUGRP-SKYSTYMD (IDX-MAX)
                   MOVE    SYU-SKYEDYMD
                                   TO  TSYUGRP-SKYEDYMD (IDX-MAX)
                   MOVE    ORCHCN03S02-GRP-CNT
                                   TO  TSYUGRP-GRP-MAX2 (IDX-MAX)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   ORCHCN03S02-GRP-CNT )
                       MOVE    ORCHCN03S02-GRP-DENPNUM (IDX0)
                                   TO  TSYUGRP-GRP-DENPNUM
                                                       (IDX-MAX IDX0)
                       MOVE    ORCHCN03-KJNYMD (IDX0)
                                   TO  TSYUGRP-GRP-SRYYMD
                                                       (IDX-MAX IDX0)
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX-MAX TO  TSYUGRP-GRP-MAX
      *
           .
       4510-SYUGRP-EXT.
           EXIT. 
      *****************************************************************
      *    退院証明書出力処理
      *****************************************************************
       451-TAIPRT-OUT-SEC          SECTION.
      *
           MOVE    "ORCHCN02"      TO  WRK-ORG-PRINT-PG
           MOVE    "00000002"      TO  WRK-KBNCD
           PERFORM 4511-PRINT-PG-EDIT-SEC
      *
           INITIALIZE              LNK-PTNYUINRRK
      *
           IF    ( SPA-GMN-I04-DR-CD   NOT =   SPACE )
               MOVE    "1"         TO  WKPTNYUINRRK-DRCD1
               MOVE    SPA-GMN-I04-DR-CD
                                   TO  WKPTNYUINRRK-DRCD1 (2 : )
           ELSE
               MOVE    SPACE
                                   TO  WKPTNYUINRRK-DRCD1
           END-IF
           MOVE    WKPTNYUINRRK-REC
                                   TO  LNK-PTNYUINRRK
      *
           MOVE    "03"            TO      WRK-SPA-PRT-FLG
           CALL    WRK-PRINT-PG    USING   WRK-SPA-AREA
                                           LNK-PTNYUINRRK
           MOVE    SPACE           TO      WRK-SPA-PRT-FLG
      *
           .
       451-TAIPRT-OUT-EXT.
           EXIT.
      *****************************************************************
      *   帳票プログラム名編集処理
      *****************************************************************
       4511-PRINT-PG-EDIT-SEC         SECTION.
      *
           INITIALIZE                      WRK-PRINT-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    WRK-KBNCD           TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-PRINT-PG
           ELSE
              MOVE    WRK-ORG-PRINT-PG    TO  WRK-PRINT-PG
           END-IF
      *
           .
       4511-PRINT-PG-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4801-USERPG-SEC                 SECTION.
      *
           PERFORM 48011-USERPG-SEC
      *
           .
       4801-USERPG-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       48011-USERPG-SEC                SECTION.
      *
           MOVE    SPA-I01-I04-SYORIKBN
                                       TO  WRK-99
           MOVE    WRK-99              TO  SPA-SYORI-FLG
      *
           INITIALIZE                      SPA-KYOTUKEY
                                           SPA-KYOTU-SET
      *
           MOVE    SPA-I01-I04-NYUINKA TO  SPA-SRYKA
           MOVE    WKPTNYUINRRK-TAIINYMD
                                       TO  SPA-SRYYMD
           MOVE    WKPTNYUINRRK-TAIINYMD
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
      *
           MOVE    SPA-NAI-I04-PTID    TO  SPA-PTID
           MOVE    SPA-GMN-I04-PTNUM   TO  SPA-PTNUM
           MOVE    SPA-GMN-I04-KANANAME
                                       TO  SPA-KANANAME
           MOVE    SPA-GMN-I04-NAME    TO  SPA-NAME
           MOVE    SPA-GMN-I04-SEX     TO  SPA-SEX
           MOVE    SPA-GMN-I04-BIRTHDAY TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-I04-BIRTHDAY TO  SPA-BIRTHDAY
      *
           IF    ( SPA-GMN-I04-DR-CD   NOT =   SPACE )
               MOVE    "1"             TO  SPA-DRCD (1:1)
               MOVE    SPA-GMN-I04-DR-CD
                                       TO  SPA-DRCD (2:4)
           END-IF
           MOVE    WKPTNYUINRRK-HKNCOMBINUM
                                       TO  SPA-HKNCOMBINUM
      *
           IF    ( SPA-GMN-I04-USERPG-CD   =   "1" )
               PERFORM 48011-XD01-SENI-SEC
           ELSE
               IF    ( SPA-NAI-I04-GYOUMU-ARI  =   "1" )
      *
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "I04"               TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SRYYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPA-PTID            TO  ORCSUSERCHK-PTID
                   MOVE    SPA-PTNUM           TO  ORCSUSERCHK-PTNUM
                   MOVE    SPA-SRYKA           TO  ORCSUSERCHK-SRYKA
                   MOVE    SPA-HKNCOMBINUM     TO  ORCSUSERCHK-HKNCOMBI
                   MOVE    SPA-DRCD            TO  ORCSUSERCHK-DRCD
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
                   CALL    "ORCSUSERCHK"       USING
                                               ORCSUSERCHKAREA
                                               SPA-AREA
               END-IF
           END-IF
      *
           .
       48011-USERPG-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム実行画面表示処理
      *****************************************************************
       48011-XD01-SENI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
      *    共通キー編集
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
      *    共通のキーをクリアする
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "I01"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPACE               TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "XD01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       48011-XD01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    PUSH通知処理
      *****************************************************************
       4801-PUSH-HOSPITAL-STAY-SEC         SECTION.
      *
           INITIALIZE                      SI03-AREA
      *
           MOVE    SPA-I01-I04-SYORIKBN
                                       TO  SI03-REQNUM
           MOVE    SPA-GMN-I01-PTNUM   TO  SI03-PTNUM
           MOVE    WKPTNYUINRRK-NYUINYMD TO  SI03-NYUINYMD
           IF    ( WKPTNYUINRRK-TAIINYMD NOT =   "99999999" )
               MOVE    WKPTNYUINRRK-TAIINYMD
                                       TO  SI03-TAIINYMD
           END-IF
           CALL    "ORCSI03"           USING
                                       SI03-AREA
                                       SPA-AREA
      *
           .
       4801-PUSH-HOSPITAL-STAY-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-I0IDCD      NOT =   SPACE
               PERFORM 520-I0IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I04"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "主治医氏名を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE
                   "今回請求額が０円以下になります。"
                                       TO  SPA-ERRMSG(1:)
               MOVE
                   "調整金を再入力して下さい"
                                       TO  SPA-ERRMSG(33:)
           WHEN    "0007"
               MOVE    "入金額には０円以上を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "入金額が入金可能額以上です。"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "入金方法を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "入金方法入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "入金の取扱いを入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "入金の取扱い入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               STRING  "院外処方の投薬はありません。"
                       "院外処方せんは発行しません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0014"
               STRING  "今回入院請求額が上限を超えます。"
                                       DELIMITED  BY  SIZE
                       "調整金を再入力して下さい"
                                       DELIMITED  BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "1001"
               MOVE    "定期請求処理中のため登録はできません"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4040"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I0ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-I0IDSET-SEC                 SECTION.
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "3001"
               MOVE    "元の請求データは取り消されます。よろしいですか"
                                       TO  WRK-I0IDMSG
           WHEN    "3002"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "保険変更した請求データは取り消されます。"
                                       DELIMITED   BY  SIZE
                       "よろしいですか"
                                       DELIMITED   BY  SIZE
               INTO    WRK-I0IDMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ID1
           INITIALIZE                      I0ID1
           MOVE    SPA-I0IDCD          TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG         TO  I0ID1-ID1MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "I0ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I0ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       5002-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       5002-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-YMD             =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    分娩負担金サブ
      *****************************************************************
       800-ORCSBUNBENFTN-SEC           SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                  LNKBUNBEN-REC
           MOVE    SPA-HOSPNUM     TO  LNKBUNBEN-HOSPNUM
           MOVE    SPA-NAI-I04-PTID
                                   TO  LNKBUNBEN-PTID
           MOVE    "1"             TO  LNKBUNBEN-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTRRKNUM
                                   TO  LNKBUNBEN-RRKNUM
           CALL    "ORCSBUNBENFTN"     USING
                                       LNKBUNBEN-REC
                                       WRK-SPA-AREA
      *
           .
      *
       800-ORCSBUNBENFTN-EXT.
           EXIT.
      *****************************************************************
      *    主科決定サブ
      *****************************************************************
       800-ORCSSYUKAMAIN-SEC           SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           INITIALIZE                  ORCSSYUACCAREA
      *
           MOVE    PTINF-PTID          TO  SPA-PTID
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
      *
           INITIALIZE                      ORCSSYUACCAREA
           IF    ( SPA-I01-I04-PROCKBN =   "1" )
               MOVE    "05"            TO  LNK-SYUACC-I-KBN
           ELSE
               MOVE    "04"            TO  LNK-SYUACC-I-KBN
           END-IF
           MOVE    SPA-NAI-I04-TAIINYMD (1:6)
                                       TO  LNK-SYUACC-I-SRYYM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
      *
           .
      *
       800-ORCSSYUKAMAIN-EXT.
           EXIT.
      *****************************************************************
      *    収納更新差額取得サブ
      *****************************************************************
       800-SS002-SEC           SECTION.
      *
           INITIALIZE                      SS002-AREA
           CALL    "ORCSS002"      USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
      *
           .
      *
       800-SS002-EXT.
           EXIT.
      *****************************************************************
      *    収納明細更新差額設定処理
      *****************************************************************
       800-SET-SS002-SMEI-SEC      SECTION.
      *
           MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
           MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
           MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
      *
           .
      *
       800-SET-SS002-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    左詰処理
      *****************************************************************
       800-LTRIM-SEC                   SECTION.
      *
           MOVE    1               TO  WRK-STR-LEN
           MOVE    SPACE           TO  WRK-TMP
      *
           STRING  WRK-STR         DELIMITED   BY  SIZE
           INTO    WRK-TMP
           WITH    POINTER         WRK-STR-LEN
           END-STRING
      *
           COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
           MOVE    SPACE           TO  WRK-STR
           PERFORM VARYING IDX-STR FROM    1   BY  1
                   UNTIL ( IDX-STR >   WRK-STR-LEN )
                    OR   ( WRK-STR NOT =   SPACE )
               IF    ( WRK-TMP (IDX-STR : 1)   NOT =   SPACE )
                   MOVE    WRK-TMP (IDX-STR : )
                                   TO  WRK-STR
               END-IF
           END-PERFORM
      *
           .
      *
       800-LTRIM-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =  ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-I01-I04-RRKNUM  TO  PTNYUINRRK-RRKNUM
           MOVE    SPA-I01-I04-RRKEDANUM TO PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY3-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I04-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY4-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
      *
           .
       900-PTNYUINRRK-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY9-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  PTTEIKIRRK-PTID
           MOVE    WRK-LASTTEIKIEDYMD (1:6)
                                       TO  PTTEIKIRRK-SRYYM
           MOVE    SPA-NAI-I04-PTRRKNUM
                                       TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY9-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-NAI-I04-PTID
                                   TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTRRKNUM
                                   TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       900-SYUNOU-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-NAI-I04-PTID    TO  SYU-PTID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  SYU-SKYSTYMD
           MOVE    SPA-I01-I04-SKYEDYMD
                                       TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタFETCH処理
      *****************************************************************
       900-SYUNOU-KEY5-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY14-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYUMEI-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY9-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY9-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTID    TO  NACCT-PTID
           MOVE    "999999"            TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納ＦＥＴＣＨ処理
      *****************************************************************
       900-SYUNOU-FET-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       901-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    ORCHCN03-HOSPNUM (IDX-M)
                                       TO  SYU-HOSPNUM
           MOVE    ORCHCN03-NYUGAIKBN (IDX-M)
                                       TO  SYU-NYUGAIKBN
           MOVE    ORCHCN03-PTID (IDX-M)
                                       TO  SYU-PTID
           MOVE    ORCHCN03-DENPNUM (IDX-M)
                                       TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    トランザクションキャンセル
      *****************************************************************
       910-DBABORT-SEC                 SECTION.
      *
           MOVE    SPACE               TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
      *
       910-DBABORT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
