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
       PROGRAM-ID.             ORCGK01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 患者番号入力（Ｋ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01/04/11  前回表示の修正等
      *  01.00.02    MCC−多々納  01/05/24  患者番号変換サブの変更に伴う修正
      *  01.00.03    MCC−多々納  01/06/13  患者登録への遷移追加
      *  01.00.04    MCC−多々納  01/06/29  ＤＯ登録削除,累計点数追加
      *  02.00.00    MCC−多々納  01/08/22  Ｋ０２をメイン画面とする
      *  02.00.01    MCC-多々納   02/04/25  労災追加
      *  02.00.02    NACL-多々納  02/10/18  入院追加
      *  02.00.03    NACL-多々納  02/11/15  一括入力、保険選択修正
      *  02.00.04    NACL-多々納  03/03/05  排他制御処理追加
      *  02.00.05    NACL-多々納  03/09/09  受診履歴、入院履歴より診療科等決定
      *  02.00.06    NACL-多々納  04/07/23  継続入力区分の削除
      *  02.00.07    NACL-多々納  05/11/17  MONFUNC 対応
      *  02.00.08    NACL−多々納 06/08/17  当日担当医設定追加他
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/12  非表示保険組合せ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-K01.
           03  SPA-K01-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-PTNUM           PIC X(20).
                   07  SPA-GMN-PTID            PIC 9(10).
                   07  SPA-GMN-NAME            PIC X(40).
                   07  SPA-GMN-SRYKA-G.
                       09  SPA-GMN-SRYKA       PIC X(02).
                       09  SPA-GMN-F1          PIC X(01).
                       09  SPA-GMN-SRYKAMEI    PIC X(18).
      ***********  07  SPA-GMN-SRYKALST                    OCCURS  10.
      *                09  SPA-GMN-SRYKAL      PIC X(02).
      *                09  SPA-GMN-F1L         PIC X(01).
      ***********      09  SPA-GMN-SRYKAMEIL   PIC X(18).
                   07  SPA-GMN-DRCD-G.
                       09  SPA-GMN-DRCD        PIC X(04).
                       09  SPA-GMN-F1          PIC X(01).
                       09  SPA-GMN-DRCDMEI     PIC X(24).
      *
                   07  SPA-GMN-HKNCOMBINUM     PIC X(04).
                   07  SPA-GMN-RRKYMD          PIC X(09).
      *
      **********   07  SPA-GMN-CONTKBN-G.
      *                09  SPA-GMN-CONTKBN         PIC X(01).
      *                09  SPA-GMN-CONTKBNH        PIC X(01).
      *                09  SPA-GMN-CONTKBNMEI      PIC X(10).
      ***********  07  SPA-GMN-CONTKBNNAME     PIC X(60).
      *
               05  SPA-GMN-KYOUTU-G.
                   07  SPA-GMN-DRCDLST-G.
                       09  SPA-GMN-DRCDLST                OCCURS  99.
                           11  SPA-GMN-DRCDL       PIC X(04).
                           11  SPA-GMN-F1L         PIC X(01).
                           11  SPA-GMN-DRCDMEIL    PIC X(24).
                   07  SPA-DRCD-MAX            PIC 9(04).
      *
                   07  SPA-GMN-SRYYMD          PIC X(10).
                   07  SPA-GMN-ZENKAI-G.
                       09  SPA-GMN-ZENKAI          PIC X(01).
                       09  SPA-GMN-ZENKAIH         PIC X(01).
                       09  SPA-GMN-ZENKAIMEI       PIC X(10).
                   07  SPA-GMN-ZENKAI-COMB               OCCURS  5.
                       09  SPA-GMN-ZENKAIL       PIC X(01).
                       09  SPA-GMN-ZENL1         PIC X(01).
                       09  SPA-GMN-ZEIKAIMEIL    PIC X(10).
                   07  SPA-GMN-ZENKAI-MAX          PIC 9(04).
      *************07  SPA-GMN-DOTORO          PIC X(01).
      *
                   07  SPA-GMN-BUNSAN-G.
                       09  SPA-GMN-BUNSAN          PIC X(01).
                       09  SPA-GMN-BUNSANH         PIC X(01).
                       09  SPA-GMN-BUNSANMEI       PIC X(10).
                   07  SPA-GMN-BUNSAN-COMB         OCCURS   5.
                       09  SPA-GMN-BUNSANL         PIC X(01).
                       09  SPA-GMN-BUNSANHL        PIC X(01).
                       09  SPA-GMN-BUNSANMEIL      PIC X(10).
                   07  SPA-GMN-BUNSAN-MAX          PIC 9(04).
      *
                   07  SPA-GMN-INGAIKBN-G.
                       09  SPA-GMN-INGAIKBN        PIC X(01).
                       09  SPA-GMN-INGAIH          PIC X(01).
                       09  SPA-GMN-INGAIKBNMEI     PIC X(10).
                   07  SPA-GMN-INGAIKBN-COMB       OCCURS  5.
                       09  SPA-GMN-INGAIKBNL       PIC X(01).
                       09  SPA-GMN-INGAIHL         PIC X(01).
                       09  SPA-GMN-INGAIKBNMEIL    PIC X(10).
                   07  SPA-GMN-INGAI-MAX          PIC 9(04).
      *
      **************7  SPA-GMN-CONTKBN-COMB        OCCURS   5.
      *                09  SPA-GMN-CONTKBNL        PIC X(01).
      *                09  SPA-GMN-CONTKBNHL       PIC X(01).
      *                09  SPA-GMN-CONTKBNMEIL     PIC X(10).
      ************ 07  SPA-GMN-CONTKBN-MAX     PIC 9(04).
      *
      *            07  SPA-GMN-KA-TBL.
      *                09  SPA-GMN-KA-TBLO     OCCURS  20.
      *                    11  SPA-GMN-TKACD       PIC X(02).
      *                    11  SPA-GMN-TKAMEI      PIC X(12).
      *
                   07  SPA-GMN-HKN-TBL.
                       09  SPA-GMN-HKN-TBLO    OCCURS  100.
                           11  SPA-GMN-THKNCOMBINUM    PIC X(04).
                           11  SPA-GMN-THKNCOMBIMEI    PIC X(100).
                           11  SPA-GMN-TTEKSTYMD       PIC X(09).
                           11  SPA-GMN-TTEKEDYMD       PIC X(09).
                           11  SPA-GMN-TFTNRATE        PIC X(04).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-RRKYMD          PIC X(08).
                   07  SPA-NAI-RENNUM          PIC 9(01).
                   07  SPA-NAI-DRCD            PIC X(05).
      *
                   07  SPA-NAI-HKN-TBL.
                       09  SPA-NAI-HKN-TBLO    OCCURS   100.
                           11  SPA-NAI-TEKSTYMD    PIC X(08).
                           11  SPA-NAI-TEKEDYMD    PIC X(08).
                           11  SPA-NAI-TFTNRATE    PIC 9(03).
                           11  SPA-NAI-HKNNUM      PIC  X(03).
                           11  SPA-NAI-KOH1HKNNUM  PIC  X(03).
                           11  SPA-NAI-KOH2HKNNUM  PIC  X(03).
                           11  SPA-NAI-KOH3HKNNUM  PIC  X(03).
                           11  SPA-NAI-KOH4HKNNUM  PIC  X(03).
                           11  SPA-NAI-RSI-HKNKBN  PIC  X(01).
                           11  SPA-NAI-HYOJIKBN    PIC  X(01).
                       09  SPA-HKN-MAX             PIC  9(04).
      *
                   07  SPA-NAI-SYOYMD          PIC X(08).
                   07  SPA-NAI-LSTYMD          PIC X(08).
                   07  SPA-NAI-HKNIDX          PIC 9(04).
                   07  SPA-NAI-HKNCOMBIOK      PIC X(04).
      *
                   07  SPA-NAI-DOUJI.
                       09  SPA-NAI-DOUJI-RENNUM    PIC 9(01).
                       09  SPA-NAI-DOUJI-DENPNUM   PIC 9(07).
                       09  SPA-NAI-DOUJI-HKNNUM    PIC X(04)
                                                   OCCURS   3.
                   07  SPA-NAI-WSRYUMU         PIC X(01).
                   07  SPA-NAI-WSRYCONTKBN     PIC X(01).
      *
               05  SPA-NAI-KYOUTU.
                   07  SPA-NAI-SRYYMDS         PIC X(08).
                   07  SPA-NAI-SRYYMDS-R   REDEFINES  SPA-NAI-SRYYMDS.
                       09  SPA-NAI-SRYYY       PIC 9(04).
                       09  SPA-NAI-SRYMM       PIC 9(02).
                       09  SPA-NAI-SRYDD       PIC 9(02).
      *
               05  SPA-MAE-AREA.
                   07  SPA-MAE-SRYKA-G.
                       09  SPA-MAE-SRYKA       PIC X(02).
                       09  SPA-MAE-F1          PIC X(01).
                       09  SPA-MAE-SRYKAMEI    PIC X(18).
                   07  SPA-MAE-SRYYMD          PIC X(10).
                   07  SPA-MAE-HKNCOMBINUM     PIC X(04).
      *************07  SPA-MAE-CONTKBN-G.
      *                09  SPA-MAE-CONTKBN         PIC X(01).
      *                09  SPA-MAE-CONTKBNH        PIC X(01).
      **************   09  SPA-MAE-CONTKBNMEI      PIC X(10).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-PTNYUINRRK        PIC 9(01).
      *
           03  FLG-WKSYORI         PIC 9(01).
           03  FLG-HEN             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
      *
           03  FLG-SYOKI           PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
           03  FLG-UKETUKE-ARI     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-W               PIC 9(04).
           03  IDX-KAI             PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-R               PIC 9(04).
           03  IDXS                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
      *
           03  WRK-WSYSYMD.
               05  WRK-WSGO         PIC X(01).
               05  WRK-WSYY         PIC 9(02).
               05  WRK-WSMM         PIC 9(02).
               05  WRK-WSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-SRYYYMM         PIC X(06).
           03  WRK-HKNCOMBI        PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-ZAIKAISU        PIC 9(08).
           03  WRK-EDABAN          PIC 9(02).
      *剤番号
           03  WRK-ZAINUM-TBL.
               05  WRK-ZAINUM-G        OCCURS   3.
                   07  WRK-TZAINUM         PIC  9(08)
                                            OCCURS 15.
           03  WRK-RRK-SRYYMD          PIC  X(08).
           03  WRK-RRK-DENPNUM         PIC  9(07).
      *  診療履歴保存
           03  WRK-CONTKBN-AREA.
               05  WRK-CONTKBN-TBL      OCCURS    3.
                   07  WRK-CONTKBN-NUM      PIC X(04).
                   07  WRK-CONTKBN-TEN      PIC X(01).
           03  IDX-JYU                 PIC 9(04).
      *    初診料算定チェック
           03  FLG-TENKI               PIC 9(01).
           03  FLG-CHIYU               PIC 9(01).
           03  WRK-TENKIYMD            PIC X(08).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-ZZ9             PIC ZZZ9.
           03  WRK-SELNUM          PIC 9(04).
      *    未収金
           03  WRK-MISYU           PIC S9(08).
      *----(01.00.01) LINE ADD START ----------------------------------
           03  WRK-CHIYUYMD        PIC X(08).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    選択日付
           03  WRK-CAL-YMD.
               05  WRK-CAL-YEAR        PIC 9(04).
               05  WRK-CAL-MONTH       PIC 9(02).
               05  WRK-CAL-DAY         PIC 9(02).
      *
           03  WRK-DRCD-IDX            PIC 9(04).
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBINUM         PIC X(04).
           03  WRK-MAE-HKNCOMBINUM     PIC X(04).
      *
           03  WRK-MOTOPG              PIC X(08).
      *
           03  WRK-ORC-DBPATH          PIC X(64).
           03  WRK-MCP-WIDGET          PIC X(64).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
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
      *    COPY    "CPSK1006.INC".
      *
      *    COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1001.INC".
      *
      **** COPY    "CPSK1007.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
      *01  PTNUM-REC.
      *    COPY    "CPPTNUM.INC".
      *    診療行為マスタ−
      *01  SRYACT-REC.
      *    COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    保険組み合わせ
      *01  HKNCOMBI-REC.
      *    COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
      *01  PTCOM-REC.
      *    COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計マスタ−
      *01  SRYACCT-REC.
      *    COPY    "CPSRYACCT.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    診療科履歴
      *01  SRYKARRK-REC.
      *    COPY    "CPSRYKARRK.INC".
      *
      *    収納
      *01  SYUNOU-REC.
      *    COPY    "CPSYUNOU.INC".
      *
      *    患者病名
      *01  PTBYOMEI-REC.
      *    COPY    "CPPTBYOMEI.INC".
      *
      *    患者労災保険情報
      *01  PTRSIINF-REC.
      *    COPY    "CPPTRSIINF.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    初期ＳＰＡ編集処理パラメタ
      *    COPY    "CPORCSC95.INC".
      *
      *    ドクター編集処理パラメタ
           COPY    "CPORCSDRSET.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
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
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K01
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
           MOVE    ZERO            TO  FLG-SYOKI
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
           MOVE    SPA-K01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCGK01    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "K01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
           INITIALIZE              SPA-K01
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-K01
                   MOVE    SPA-WORK        TO  SPA-K01KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *----(01.00.03) LINE ADD START ----------------------------------
      *        患者登録より
      *        IF      SPA-MOTOPG(1:3)     =   "P02" 
      *            PERFORM 3103-P02SPASET-SEC
      *            GO      TO      300-SCREEN-EXT
      *        END-IF
      *----(01.00.03) LINE ADD END   ----------------------------------
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-U01SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               ELSE
                   MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               END-IF
           END-IF
      *
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *****PERFORM 310-SPASET-SEC
      *
           EVALUATE    SPA-MOTOPG
      ******** WHEN    "K07"
      *            履歴遷移より画面再編集
      ***********  PERFORM 320-K07SPASET-SEC
      *
               WHEN    "K02"
               WHEN    "K02N"
      *            患者番号より初期セット
                   IF      SPA-PTID            =   ZERO
                       PERFORM 310-K02SPASET-SEC
                   ELSE
                       PERFORM 310-K02SPASET-SEC
                       MOVE    ZERO                TO  FLG-SYORI
                       PERFORM 3101-PTINF-SPA-SEC
                   END-IF
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           IF      SPA-CONTFLG         =   ZERO
               MOVE    01              TO  SPA-GMN-CUR
           ELSE
               MOVE    09              TO  SPA-GMN-CUR
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  K01-PTNUM
                   MOVE    1               TO  FLG-SYORI
                   PERFORM 4101-PTNUM-CHK-SEC
                   MOVE    2               TO  SPA-GMN-CUR
      *
                   IF      SPA-SRYKA       NOT =   SPACE
                        PERFORM 4102-SRYKA-CHK-SEC
                        MOVE    SPA-SRYKA        TO  SPA-GMN-SRYKA
                        MOVE    SPA-SRYKAMEI     TO  SPA-GMN-SRYKAMEI
                        MOVE    3                TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       3101-U01SPASET-EXT.
           EXIT.
      *----(01.00.03) LINE ADD START ----------------------------------
      *
      *****************************************************************
      *    患者登録画面より編集処理
      *****************************************************************
       3103-P02SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *    ＳＰＡ初期セット
           PERFORM 310-SPASET-SEC
      *
      *    患者番号より初期セット
           MOVE    1                   TO  FLG-SYORI
           PERFORM 3101-PTINF-SPA-SEC
           .
       3103-P02SPASET-EXT.
           EXIT.
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    患者番号より初期セット編集処理
      *****************************************************************
       3101-PTINF-SPA-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-SYOKI
           MOVE    SPA-PTNUM           TO  K01-PTNUM
           PERFORM 4101-PTNUM-CHK-SEC
           MOVE    2                   TO  SPA-GMN-CUR
      *
           IF      SPA-HKNCOMBINUM     NOT =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
               MOVE    SPA-HKNCOMBINUM     TO  SPA-MAE-HKNCOMBINUM
               PERFORM 41041-HKNCOMBI-TBLCHK-SEC
          END-IF
      *
           IF      SPA-SRYKA       NOT =   SPACE
                MOVE    SPA-SRYKA          TO  SPA-GMN-SRYKA
                PERFORM VARYING    IDX     FROM    1  BY  1
                       UNTIL       IDX     >   SPA-SRYKA-MAX
                   IF      SPA-GMN-SRYKA       =   SPA-GMN-SRYKAL(IDX)
                       MOVE    SPA-GMN-SRYKALST(IDX)
                                                   TO  SPA-GMN-SRYKA-G
                       MOVE    SPA-SRYKA-MAX       TO  IDX
                   END-IF
                END-PERFORM
                MOVE    3                TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-DRCD        NOT =   SPACE
               MOVE    SPA-DRCD(2:4)       TO  SPA-GMN-DRCD
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL       IDX     >   SPA-DRCD-MAX
                   IF      SPA-GMN-DRCD (1:4)  =   SPA-GMN-DRCDL(IDX)
                       MOVE    SPA-GMN-DRCDLST(IDX)
                                               TO  SPA-GMN-DRCD-G
                       MOVE    "1"             TO  SPA-NAI-DRCD(1:1)
                       MOVE    SPA-GMN-DRCD    TO  SPA-NAI-DRCD(2:)
                       MOVE    SPA-DRCD-MAX    TO  IDX
                   END-IF
               END-PERFORM
      *H29.8
           ELSE
               MOVE    SPACE               TO  SPA-GMN-DRCD-G
               MOVE    SPACE               TO  SPA-NAI-DRCD
           END-IF
           .
       3101-PTINF-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-K01-AREA
      *
      *    システム管理検索（医療機関情報）
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *
      *        院外処方せん発行フラグ
               MOVE    SYS-1001-SYOHOPRTFLG    TO  SPA-SYOHOPRTFLG
      *        前回処方表示フラグ
               MOVE    SYS-1001-ZENSYOHOFLG    TO  SPA-ZENSYOHOFLG
               MOVE    SYS-1001-ZENSYOHOFLG    TO  SPA-GMN-ZENKAI
      *        処方区分
               MOVE    SYS-1001-SYOHOKBN       TO  SPA-SYOHOKBN
      *        病床数
      *********MOVE    SYS-1001-BEDSU          TO  SPA-BEDSU
      *        薬剤情報発行フラグ
               MOVE    SYS-1001-YAKUJYOPRTFLG  TO  SPA-YAKJYOPRTFLG
               IF      SYS-1001-YAKUJYOPRTFLG  =   SPACE
                   MOVE    ZERO                TO  SPA-YAKJYOPRTFLG
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入外区分
      *****MOVE    "2"                 TO  SPA-NYUGAIKBN
      *    診療日
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-SRYYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-SRYYMDS
      *    前回処方
      *****MOVE    "0"                 TO  SPA-GMN-ZENKAI
      *****MOVE    "1"                 TO  SPA-GMN-ZENKAI
           MOVE    "0 表示あり"        TO  SPA-GMN-ZENKAI-COMB (01)
           MOVE    "1 表示なし"        TO  SPA-GMN-ZENKAI-COMB (02)
           MOVE    2                   TO  SPA-GMN-ZENKAI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-ZENKAI-MAX
               IF      SPA-GMN-ZENKAI      =   SPA-GMN-ZENKAIL (IDX)
                   MOVE    SPA-GMN-ZENKAI-COMB (IDX)
                                               TO  SPA-GMN-ZENKAI-G
               END-IF
           END-PERFORM
      *    分散入力
           MOVE    "0 会計業務"        TO  SPA-GMN-BUNSAN-COMB (01)
           MOVE    "1 診察室入力"      TO  SPA-GMN-BUNSAN-COMB (02)
           MOVE    2                   TO  SPA-GMN-BUNSAN-MAX
           MOVE    SPA-GMN-BUNSAN-COMB (01)
                                       TO  SPA-GMN-BUNSAN-G
      *
      *    院外院内区分
           MOVE    SPA-SYOHOKBN        TO  SPA-GMN-INGAIKBN
      *
           MOVE    "0 院内"            TO  SPA-GMN-INGAIKBN-COMB(01)
           MOVE    "1 院外"            TO  SPA-GMN-INGAIKBN-COMB(02)
           MOVE    2                   TO  SPA-GMN-INGAI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-INGAI-MAX
               IF      SPA-GMN-INGAIKBN    =   SPA-GMN-INGAIKBNL(IDX)
                   MOVE    SPA-GMN-INGAIKBN-COMB (IDX)
                                           TO  SPA-GMN-INGAIKBN-G
               END-IF
           END-PERFORM
      *
      *****継続区分
      *    MOVE    "0:通常"            TO  SPA-GMN-CONTKBN-COMB (01)
      *    MOVE    "1:継続"            TO  SPA-GMN-CONTKBN-COMB (02)
      *    MOVE    "2:確定"            TO  SPA-GMN-CONTKBN-COMB (03)
      *    MOVE    3                   TO  SPA-GMN-CONTKBN-MAX
      **** MOVE    SPA-GMN-CONTKBN-COMB (01)
      *                                TO  SPA-GMN-CONTKBN-G
      *
      *    システム管理より編集
           PERFORM 3101-SYSKANRI-HEN-SEC
      *
      *    診療科
           IF      SPA-SRYKA           =   SPACE
               MOVE    SPA-GMN-SRYKALST(01)
                                       TO  SPA-GMN-SRYKA-G
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-SRYKA-MAX
                   IF      SPA-SRYKA       =   SPA-GMN-SRYKAL   (IDX)
                        MOVE    SPA-GMN-SRYKALST(IDX)
                                               TO  SPA-GMN-SRYKA-G
                        MOVE    SPA-SRYKA-MAX  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    職員情報（ドクター）
           IF      SPA-DRCD           =   SPACE
               IF      WRK-DRCD-IDX        =   ZERO
                   MOVE    1                   TO  IDX
               ELSE
                   MOVE    WRK-DRCD-IDX        TO  IDX
               END-IF
               MOVE    SPA-GMN-DRCDLST(IDX)    TO  SPA-GMN-DRCD-G
               MOVE    "1"                     TO  SPA-NAI-DRCD (1:1)
               MOVE    SPA-GMN-DRCDL  (IDX)    TO  SPA-NAI-DRCD (2:)
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-DRCD-MAX
                   IF      SPA-DRCD(2:4)   =   SPA-GMN-DRCDL   (IDX)
                       MOVE    SPA-GMN-DRCDLST(IDX) TO  SPA-GMN-DRCD-G
                       MOVE    "1"             TO  SPA-NAI-DRCD(1:1)
                       MOVE    SPA-GMN-DRCDL  (IDX)
                                               TO  SPA-NAI-DRCD(2:)
                       MOVE    SPA-DRCD-MAX    TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    入力チェック用
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    SPA-GMN-SRYYMD      TO  SPA-MAE-SRYYMD
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-MAE-HKNCOMBINUM
      *****MOVE    SPA-GMN-CONTKBN-G   TO  SPA-MAE-CONTKBN-G
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理（Ｋ０２より）
      *****************************************************************
       310-K02SPASET-SEC              SECTION.
      *
      *    前回処方表示フラグ
           MOVE    SPA-ZENSYOHOFLG    TO  SPA-GMN-ZENKAI
      *    入外区分
      *****MOVE    "2"                 TO  SPA-NYUGAIKBN
      *    診療日
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMDS
      *    前回処方
           MOVE    "0 表示あり"        TO  SPA-GMN-ZENKAI-COMB (01)
           MOVE    "1 表示なし"        TO  SPA-GMN-ZENKAI-COMB (02)
           MOVE    2                   TO  SPA-GMN-ZENKAI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-ZENKAI-MAX
               IF      SPA-GMN-ZENKAI      =   SPA-GMN-ZENKAIL (IDX)
                   MOVE    SPA-GMN-ZENKAI-COMB (IDX)
                                               TO  SPA-GMN-ZENKAI-G
               END-IF
           END-PERFORM
      *    分散入力
           MOVE    "0 会計業務"        TO  SPA-GMN-BUNSAN-COMB (01)
           MOVE    "1 診察室入力"      TO  SPA-GMN-BUNSAN-COMB (02)
           MOVE    2                   TO  SPA-GMN-BUNSAN-MAX
           MOVE    SPA-GMN-BUNSAN-COMB (01)
                                       TO  SPA-GMN-BUNSAN-G
      *
      *    院外院内区分
           MOVE    SPA-SYOHOKBN        TO  SPA-GMN-INGAIKBN
      *
           MOVE    "0 院内"            TO  SPA-GMN-INGAIKBN-COMB(01)
           MOVE    "1 院外"            TO  SPA-GMN-INGAIKBN-COMB(02)
           MOVE    2                   TO  SPA-GMN-INGAI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-INGAI-MAX
               IF      SPA-GMN-INGAIKBN    =   SPA-GMN-INGAIKBNL (IDX)
                   MOVE    SPA-GMN-INGAIKBN-COMB (IDX)
                                           TO  SPA-GMN-INGAIKBN-G
               END-IF
           END-PERFORM
      *
      **** 継続区分
      *    MOVE    "0:通常"            TO  SPA-GMN-CONTKBN-COMB (01)
      *    MOVE    "1:継続"            TO  SPA-GMN-CONTKBN-COMB (02)
      *    MOVE    "2:確定"            TO  SPA-GMN-CONTKBN-COMB (03)
      *    MOVE    3                   TO  SPA-GMN-CONTKBN-MAX
      *    MOVE    SPA-GMN-CONTKBN-COMB (01)
      *                                TO  SPA-GMN-CONTKBN-G
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   SPA-GMN-CONTKBN-MAX
      *        IF      SPA-CONTKBN         =   SPA-GMN-CONTKBN-COMB
      *                                                     (IDX)(1:1)
      *            MOVE    SPA-GMN-CONTKBN-COMB (IDX)
      *                                    TO  SPA-GMN-CONTKBN-G
      *        END-IF
      **** END-PERFORM
      *
      *    システム管理より編集
           PERFORM 3101-SYSKANRI-HEN-SEC
      *
      *    診療科
           IF      SPA-SRYKA           =   SPACE
               MOVE    SPA-GMN-SRYKALST(01)
                                       TO  SPA-GMN-SRYKA-G
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-SRYKA-MAX
                   IF      SPA-SRYKA       =   SPA-GMN-SRYKAL   (IDX)
                        MOVE    SPA-GMN-SRYKALST(IDX)
                                               TO  SPA-GMN-SRYKA-G
                        MOVE    SPA-SRYKA-MAX  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    職員情報（ドクター）
           IF      SPA-DRCD           =   SPACE
               IF      WRK-DRCD-IDX        =   ZERO
                   MOVE    1                   TO  IDX
               ELSE
                   MOVE    WRK-DRCD-IDX        TO  IDX
               END-IF
               MOVE    SPA-GMN-DRCDLST(IDX) TO  SPA-GMN-DRCD-G
               MOVE    "1"                  TO  SPA-NAI-DRCD (1:1)
               MOVE    SPA-GMN-DRCDL  (IDX) TO  SPA-NAI-DRCD (2:)
           ELSE
      *********PERFORM VARYING     IDX     FROM    1   BY  1
      *                UNTIL       IDX     >   SPA-DRCD-MAX
      *            IF      SPA-DRCD(1:4)   =   SPA-GMN-DRCDL   (IDX)
      *                MOVE    SPA-GMN-DRCDLST(IDX) TO  SPA-GMN-DRCD-G
      *                MOVE    "1"             TO  SPA-NAI-DRCD(1:1)
      *                MOVE    SPA-GMN-DRCDL  (IDX)
      *                                        TO  SPA-NAI-DRCD(2:)
      *                MOVE    SPA-DRCD-MAX    TO  IDX
      *            END-IF
      *********END-PERFORM
               MOVE    "1"                 TO  SPA-NAI-DRCD(1:1)
               MOVE    SPA-DRCD(2:4)       TO  SPA-NAI-DRCD(2:4)
           END-IF
      *    入力チェック用
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    SPA-GMN-SRYYMD      TO  SPA-MAE-SRYYMD
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-MAE-HKNCOMBINUM
      *****MOVE    SPA-GMN-CONTKBN-G   TO  SPA-MAE-CONTKBN-G
           .
       310-K02SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理より編集処理
      *****************************************************************
       3101-SYSKANRI-HEN-SEC              SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-KEY
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-SRYYMDS     TO  SYS-1005-STYUKYMD
           MOVE    SPA-NAI-SRYYMDS     TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
      *********MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-TKACD  (IDX)
      *********MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-TKAMEI (IDX)
      *
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKAL   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKAMEIL(IDX)
      *
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    職員情報（ドクター）
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-NAI-SRYYMDS     TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
           MOVE    SPA-DRCD(2:4)       TO  ORCSDR-IN-DRCD
           CALL   "ORCSDRSET"          USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           MOVE    ORCSDR-DRCD-IDX     TO  WRK-DRCD-IDX
      *
           .
       3101-SYSKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       320-K07SPASET-SEC              SECTION.
      *
           .
       32O-K07SPASET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        初期画面へ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 230-CLEAR-SEC
      *        前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *****患者登録へ
      *        WHEN    "CLICKED"       ALSO    "B04"
      *            PERFORM 440-KNJADD-SEC
      *    受診履歴へ
      *        WHEN    "CLICKED"       ALSO    "B05"
      *            PERFORM 491-RIREKI-SEC
      *    会計カード
      *        WHEN    "CLICKED"       ALSO    "B07"
      *            PERFORM 450-KAIKEI-SEC
      *    収納登録
      *        WHEN    "CLICKED"       ALSO    "B08"
      ***********  PERFORM 460-SYUNOU-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
      *        カレンダー選択
               WHEN    "CLICKED"       ALSO    "CALENDAR1"
                   PERFORM 4112-CALENDAR-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   MOVE    1               TO  FLG-SYORI
                   PERFORM 4101-PTNUM-CHK-SEC
      *        保険組合せ選択
               WHEN    ANY             ALSO    "HKNKUMILST"
                   PERFORM 4104-HKNCOMBI-LST-SEC
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
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
           MOVE    K01-SRYKA           TO  SPA-GMN-SRYKA-G
           MOVE    K01-SRYYMD          TO  SPA-GMN-SRYYMD
           MOVE    K01-ZENKAI          TO  SPA-GMN-ZENKAI-G
           MOVE    K01-DRNAME          TO  SPA-GMN-DRCD-G
           MOVE    K01-HKNKUMI         TO  SPA-GMN-HKNCOMBINUM
           MOVE    K01-BUNSAN          TO  SPA-GMN-BUNSAN-G
           MOVE    K01-INGAISHOHOKBN   TO  SPA-GMN-INGAIKBN-G
      *****MOVE    K01-CONTKBN         TO  SPA-GMN-CONTKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 4102-SRYKA-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-SRYYMD-CHK-SEC
           END-IF
      *    前回処方入力
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4104-ZENKAI-CHK-SEC
           END-IF
      *    ドクター入力
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4105-DRCD-CHK-SEC
           END-IF
      *    分散入力
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4107-BUNSAN-CHK-SEC
           END-IF
      *    院外処方区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4108-INGAISHOHOKBN-CHK-SEC
           END-IF
      *    保険組合わせ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4104-HKNCOMBI-CHK-SEC
           END-IF
      *    継続区分
      *****PERFORM 4109-CONTKBN-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    訂正の時、入力エラー
           IF     (SPA-SYORIFLG        =   1   )  AND
                  (SPA-GMN-PTNUM       =   SPA-PTNUM )
               IF      K01-PTNUM       NOT =   SPA-GMN-PTNUM
                   MOVE    "0009"          TO  SPA-ERRCD
               END-IF
               MOVE    2               TO  SPA-GMN-CUR
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    K01-PTNUM           TO  SPA-GMN-PTNUM
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    SPA-GMN-PTNUM       TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF     (SPTID-RC        NOT =   ZERO  )
           OR     (SPA-GMN-PTNUM       =   SPACE )
               MOVE    "0003"          TO  SPA-ERRCD
               GO  TO                  4101-PTNUM-CHK-EXT
           END-IF
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
      *    新規に患者番号を入力時、ＳＰＡをクリアする
           IF      FLG-SYORI           =   1
               INITIALIZE                      SPA-GMN-AREA
               INITIALIZE                      SPA-NAI-AREA
               MOVE    SPTID-PTID          TO  SPA-GMN-PTID
               MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
      *********MOVE    SPA-GMN-CONTKBN-COMB (01)
      *********                            TO  SPA-GMN-CONTKBN-G
           END-IF
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *
      *    新規の時、入院中かの判定
           IF      FLG-SYORI           =   1
               PERFORM 41015-PTNYUINRRK-CHK-SEC
           END-IF
      *
      *    患者番号入力時、受付読込
           IF     (FLG-SYORI           =   1 )  AND
                  (SPA-SYSYMD          =   SPA-NAI-SRYYMDS )
               PERFORM 4102-UKETUKE-HEN-SEC
           END-IF
      *
           IF      SPA-NYUGAIKBN       =   "2"
      *        受診履歴より、保険組合せ等を確定する
               PERFORM 41014-JYURRK-HKNCOMBI-SEC
           ELSE
      *        ドクター編集
               PERFORM 3101-DRCD-SET-SEC
           END-IF
      *
      *    診療科
           IF      SPA-GMN-SRYKA       =   SPACE
               MOVE    SPA-GMN-SRYKAL(01)  TO  SPA-GMN-SRYKA
           END-IF
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA       =   SPA-GMN-SRYKAL(IDX)
                   MOVE    SPA-GMN-SRYKALST(IDX)   TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX           TO  IDX
               END-IF
           END-PERFORM
      *
      *    保険組合マスター編集
           INITIALIZE                  SPA-GMN-HKN-TBL
           MOVE    ZERO                TO  SPA-HKN-MAX
           MOVE    ZERO                TO  IDX
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "2"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
      *    非表示区分込み
           MOVE    "1"                 TO  ORCSHKN-KBN3
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   ORCSHKN-HKN-MAX
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDX)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDX)
      *TEST
      *    受給者番号編集
               IF      ORCSHKN-NAI-HKNJANUM (IDX) NOT =   SPACE
                   MOVE    SPACE       TO  SPA-GMN-THKNCOMBIMEI (IDX)
                   MOVE    SPACE       TO  WRK-HKNCOMBI
                   STRING  ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                               DELIMITED BY  "  "
                       " ("                    DELIMITED  BY  SIZE
                       ORCSHKN-NAI-HKNJANUM (IDX)
                                               DELIMITED  BY  SPACE
                       ")"                     DELIMITED  BY  SIZE
                         INTO             WRK-HKNCOMBI
                   END-STRING
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HKNCOMBI        TO  KANACHK-MAE-INPUT
                   MOVE    60                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-THKNCOMBIMEI
                                                            (IDX)
               END-IF
      *    非表示区分
               EVALUATE    ORCSHKN-NAI-HYOJIKBN (IDX)
               WHEN    "1"
                   MOVE    SPACE       TO  WRK-HKNCOMBI
                   STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                 DELIMITED  BY  "  "
                           "【非表示】" 
                                                 DELIMITED  BY  SIZE
                         INTO             WRK-HKNCOMBI
                   END-STRING
                   MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI
                                                            (IDX)
               WHEN    "2"
                   MOVE    SPACE       TO  WRK-HKNCOMBI
                   STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                 DELIMITED  BY  "  "
                           "【入非表示】" 
                                                 DELIMITED  BY  SIZE
                         INTO             WRK-HKNCOMBI
                   END-STRING
                   MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI
                                                            (IDX)
               WHEN    "3"
                   MOVE    SPACE       TO  WRK-HKNCOMBI
                   STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                 DELIMITED  BY  "  "
                           "【外非表示】" 
                                                 DELIMITED  BY  SIZE
                         INTO             WRK-HKNCOMBI
                   END-STRING
                   MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI
                                                            (IDX)
               END-EVALUATE
      *TEST
      *
               MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-NAI-HKNNUM  (IDX)
               MOVE    ORCSHKN-NAI-KOH1HKNNUM (IDX)
                                       TO  SPA-NAI-KOH1HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH2HKNNUM (IDX)
                                       TO  SPA-NAI-KOH2HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH3HKNNUM (IDX)
                                       TO  SPA-NAI-KOH3HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH4HKNNUM (IDX)
                                       TO  SPA-NAI-KOH4HKNNUM (IDX)
      *
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                       TO  SPA-NAI-TFTNRATE (IDX)
               MOVE    ORCSHKN-GMN-FTNRATEMEI (IDX)
                                       TO  SPA-GMN-TFTNRATE (IDX)
      *
               MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-NAI-TEKSTYMD (IDX)
               MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-NAI-TEKEDYMD (IDX)
               MOVE    ORCSHKN-GMN-TEKSTYMD (IDX)
                                       TO  SPA-GMN-TTEKSTYMD (IDX)
               MOVE    ORCSHKN-GMN-TEKEDYMD (IDX)
                                       TO  SPA-GMN-TTEKEDYMD (IDX)
      *        労災区分
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX)
                                       TO  SPA-NAI-RSI-HKNKBN (IDX)
      *
               MOVE    ORCSHKN-NAI-HYOJIKBN (IDX)
                                       TO  SPA-NAI-HYOJIKBN (IDX)
      *
           END-PERFORM
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKN-MAX
           IF      SPA-HKN-MAX         =   ZERO
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-HKNCOMBINUM NOT =   SPACE
      *        対象の保険組合せチェック処理
               PERFORM 41041-HKNCOMBI-TBLCHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    2                   TO  SPA-GMN-CUR
                   GO      TO      4101-PTNUM-CHK-EXT
               END-IF
           END-IF
      *
      *    対象の保険組合がない時、省略分
           MOVE    SPACE               TO  SPA-ERRCD
      *
           IF      ORCSHKN-IDX         =   ZERO
               MOVE    1               TO  ORCSHKN-IDX
           END-IF
           MOVE    SPA-GMN-THKNCOMBINUM (ORCSHKN-IDX)
                                       TO  SPA-HKNCOMBINUM
           MOVE    SPA-HKNCOMBINUM     TO  SPA-NAI-HKNCOMBIOK
      *
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
      *    前回患者保存
           MOVE    SPA-PTID            TO  SPA-LAST-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       4101-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院中かの判定処理
      *****************************************************************
       41015-PTNYUINRRK-CHK-SEC             SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-NYUIN
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key34"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *    入院が１件もないとき、外来とする
           IF      FLG-PTNYUINRRK      =   1
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
      *
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-NYUIN           =   1 )
      *        転入日
               IF      PTNYUINRRK-TENNYUYMD    <=  SPA-NAI-SRYYMDS
      *            転出日
                   IF     (PTNYUINRRK-TENSTUYMD    =   SPACE )  OR
                          (PTNYUINRRK-TENSTUYMD    NOT =   SPACE AND
                           PTNYUINRRK-TENSTUYMD    >=  SPA-NAI-SRYYMDS)
      *                入院対象処理
                       IF     (PTNYUINRRK-TAIINYMD  =  SPA-NAI-SRYYMDS)
                         OR   (PTNYUINRRK-TENSTUYMD >  SPA-NAI-SRYYMDS)
      *                入院中
                           MOVE    1                   TO  FLG-NYUIN
                           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
                           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-GMN-SRYKA
      *                    保険組合せ番号
                           MOVE    PTNYUINRRK-HKNCOMBINUM
                                               TO  SPA-GMN-HKNCOMBINUM
      *                    担当医１（主治医）
                           MOVE    PTNYUINRRK-DRCD1    TO  SPA-NAI-DRCD
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key34"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    入院中の時、エラーとする
           IF      FLG-NYUIN           =   1
               MOVE    "1"             TO  SPA-NYUGAIKBN
      *        ドクター編集
               PERFORM 3101-DRCD-SET-SEC
           ELSE
               MOVE    "2"             TO  SPA-NYUGAIKBN
           END-IF
      *
           .
       41015-PTNYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスタ編集処理
      *****************************************************************
       4102-UKETUKE-HEN-SEC             SECTION.
      *
      *    受付マスタよりドクターコード設定
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-GMN-PTID        TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           PERFORM
               UNTIL   FLG-UKETUKE        =   1
               MOVE    ZERO               TO  FLG-UKETUKE-ARI
      *        会計のまだの最初の受付を対象とする
               IF     (UKE-KAIKEITIME      =   ZERO )  AND
                      (UKE-UKEID       NOT =   ZERO )
                   MOVE    1                  TO  FLG-UKETUKE-ARI
               END-IF
      *        診療科変更時
               IF     (FLG-HEN            =   1  )  AND
                      (UKE-SRYKA      NOT =   SPA-GMN-SRYKA)
                   MOVE    ZERO               TO  FLG-UKETUKE-ARI
               END-IF
               IF      FLG-UKETUKE-ARI     =   1
      *        診療科
                   MOVE    UKE-SRYKA           TO  SPA-GMN-SRYKA
                   MOVE    UKE-SRYKA           TO  SPA-SRYKA
      *            ドクター
                   IF      UKE-DRCD        NOT =   SPACE
                       MOVE    UKE-DRCD            TO  SPA-NAI-DRCD
                       PERFORM 3101-DRCD-SET-SEC
                   END-IF
      *            保険組合せ
                   IF      UKE-HKNCOMBI    NOT =   ZERO
                       MOVE    UKE-HKNCOMBI    TO  SPA-GMN-HKNCOMBINUM
                   END-IF
                   MOVE    1               TO  FLG-UKETUKE
               END-IF
      *
               IF      FLG-UKETUKE         =   ZERO
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-UKETUKE-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4102-UKETUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の保険組合せ決定処理
      *****************************************************************
       41015-HKNCOMBI-SEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-HKNCOMBIOK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-HKN-MAX
               IF      (SPA-NAI-SRYYMDS    >=  SPA-NAI-TEKSTYMD(IDX))
                  AND  (SPA-NAI-SRYYMDS    <=  SPA-NAI-TEKEDYMD(IDX))
                   MOVE    SPA-GMN-THKNCOMBINUM(IDX)
                                           TO  SPA-NAI-HKNCOMBIOK
                   MOVE    IDX             TO  SPA-NAI-HKNIDX
                   MOVE    SPA-HKN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-SYOKI           =   1
               IF      SPA-HKNCOMBINUM     NOT =   SPACE
                   MOVE    SPA-HKNCOMBINUM     TO  SPA-NAI-HKNCOMBIOK
               END-IF
           END-IF
           .
       41015-HKNCOMBI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       3101-DRCD-SET-SEC             SECTION.
      *
           MOVE    SPACE              TO  SPA-GMN-DRCD-G
           MOVE    SPA-NAI-DRCD(2:4)  TO  SPA-GMN-DRCD
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-NAI-SRYYMDS     TO  ORCSDR-SYSYMD
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    "3"                 TO  ORCSDR-KBN
           ELSE
               MOVE    "1"                 TO  ORCSDR-KBN
           END-IF
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCD        TO  ORCSDR-IN-DRCD
           CALL   "ORCSDRSET"          USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           IF      ORCSDR-IN-DRCD-G    NOT =   SPACE
               MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           ELSE
               IF      ORCSDR-DRCD-IDX     =   ZERO
                   MOVE    1                   TO  IDX
               ELSE
                   MOVE    ORCSDR-DRCD-IDX     TO  IDX
               END-IF
               MOVE    SPA-GMN-DRCDLST(IDX) TO  SPA-GMN-DRCD-G
               MOVE    "1"                  TO  SPA-NAI-DRCD (1:1)
               MOVE    SPA-GMN-DRCDL  (IDX) TO  SPA-NAI-DRCD (2:)
           END-IF
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           .
       3101-DRCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       41012-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       41012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診歴より、保険組合を確定する
      *****************************************************************
       41014-JYURRK-HKNCOMBI-SEC        SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ，診療科の最終レコードを対象
           INITIALIZE                      SPA-NAI-DOUJI
      *****MOVE    SPACE               TO  SPA-GMN-CONTKBNNAME
      *
           MOVE    ZERO                TO  IDX-JYU
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-NAI-SRYYMDS     TO  JYURRK-SRYYMD
      *
           IF      SPA-GMN-SRYKA       =   SPACE
               MOVE    "key10"             TO  WRK-ORC-DBPATH
           ELSE
               MOVE    "key11"             TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    SPACE               TO  WRK-SRYKA
           MOVE    SPACE               TO  WRK-DRCD
           MOVE    SPACE               TO  WRK-HKNCOMBINUM
           IF      FLG-JYURRK           =   ZERO
      *        診療科がない時
               IF      SPA-GMN-SRYKA       =   SPACE
                   MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
                   MOVE    JYURRK-DRCD         TO  WRK-DRCD
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
               END-IF
      *
               IF      SPA-GMN-SRYKA       =   JYURRK-SRYKA
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
                   MOVE    JYURRK-DRCD         TO  WRK-DRCD
               END-IF
      *
      ******** 継続のチェック
      *        IF      SPA-NAI-SRYYMDS     =   JYURRK-SRYYMD
      *            PERFORM 41015-CONTKBN-HANTEI-SEC
      *********END-IF
      *
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    診療科がない時
           IF      SPA-GMN-SRYKA       =   SPACE
               MOVE    WRK-SRYKA           TO  SPA-GMN-SRYKA
               MOVE    WRK-DRCD            TO  SPA-NAI-DRCD
               MOVE    WRK-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
      *        ドクター編集
               PERFORM 3101-DRCD-SET-SEC
           ELSE
               MOVE    WRK-DRCD            TO  SPA-NAI-DRCD
               PERFORM 3101-DRCD-SET-SEC
           END-IF
      *****ドクターがない時編集
      *    IF      SPA-NAI-DRCD        =   SPACE
      * *      MOVE    WRK-DRCD            TO  SPA-NAI-DRCD
      *        PERFORM 3101-DRCD-SET-SEC
      **** END-IF
      *    保険組合せがない時編集
           IF      SPA-GMN-HKNCOMBINUM =   SPACE
               MOVE    WRK-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
           END-IF
      *
      *****継続区分名称セット
      *    IF      SPA-NAI-DOUJI-DENPNUM NOT =   ZERO
      *        PERFORM 41016-CONTKBN-HEN-SEC
      *****END-IF
           .
       41014-JYURRK-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    継続区分判定処理
      *****************************************************************
      *41015-CONTKBN-HANTEI-SEC             SECTION.
      *
      *    収納テーブル読み込み
      *    MOVE    SPACE               TO  SYUNOU-REC
      *    INITIALIZE                  SYUNOU-REC
      *    MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
      *    MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
      *    MOVE    SPA-GMN-PTID        TO  SYU-PTID
      *    MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
      *    MOVE    SYUNOU-REC          TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SYUNOU-KEY"       TO  ORC-DBPATH
      *        PERFORM 980-SYUNOU-READ-SEC
      *    ELSE
      *        MOVE    SPACE               TO  SYUNOU-REC
      *        INITIALIZE                  SYUNOU-REC
      *        MOVE    1               TO  FLG-SYUNOU
      *    END-IF
      *    MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *    MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      FLG-SYUNOU          =   ZERO
      *        伝票状態区分
      *        IF      SYU-DENPJTIKBN      =   "7"
      *            同時診療伝票番号
      *            MOVE    SYU-DOUJI-DENPNUM  TO  SPA-NAI-DOUJI-DENPNUM
      *            受診連番
      *            MOVE    JYURRK-RENNUM      TO  SPA-NAI-DOUJI-RENNUM
      *
      *            ADD     1                  TO  IDX-JYU
      *            MOVE    JYURRK-HKNCOMBINUM TO  SPA-NAI-DOUJI-HKNNUM
      *                                                      (IDX-JYU)
      *        END-IF
      *    END-IF
      *
      *    .
      *41015-CONTKBN-HANTEI-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    継続区分名称編集処理
      *****************************************************************
      *41016-CONTKBN-HEN-SEC            SECTION.
      *
      *    MOVE    SPACE               TO  WRK-CONTKBN-AREA
      *    PERFORM VARYING     IDX-JYU FROM    1   BY  1
      *            UNTIL      (IDX-JYU     >   3   )   OR
      *                       (SPA-NAI-DOUJI-HKNNUM(IDX-JYU)
      *                                    =   SPACE )
      *        MOVE    SPA-NAI-DOUJI-HKNNUM(IDX-JYU)
      *                                    TO  WRK-CONTKBN-NUM(IDX-JYU)
      *        IF      IDX-JYU         >   1
      *            MOVE    ","             TO  WRK-CONTKBN-TEN(IDX-JYU)
      *        END-IF
      *    END-PERFORM
      *
      *    MOVE    SPACE               TO  SPA-GMN-CONTKBNNAME
      *    STRING
      *        "継続中のデータがあります　作成済の保険組合せ："
      *                                   DELIMITED  BY  SIZE
      *        WRK-CONTKBN-AREA           DELIMITED  BY  SPACE
      *            INTO                   SPA-GMN-CONTKBNNAME
      *    END-STRING
      *
      *    .
      *41016-CONTKBN-HEN-EXT.
      *****EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       4102-SRYKA-CHK-SEC             SECTION.
      *
      *    訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      SPA-GMN-SRYKA-G     NOT =   SPA-MAE-SRYKA-G
                   MOVE    "0009"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
                   MOVE    SPA-MAE-SRYKA-G     TO  SPA-GMN-SRYKA-G
               END-IF
               GO      TO      4102-SRYKA-CHK-EXT
           END-IF
      *
           IF      SPA-MAE-SRYKA-G     =   SPA-GMN-SRYKA-G
               MOVE    ZERO            TO  FLG-HEN
           ELSE
               MOVE    1               TO  FLG-HEN
           END-IF
           IF      SPA-GMN-SRYKA-G     NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-SRYKA       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-SRYKA
               END-IF
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   SPA-SRYKA-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SRYKA       =   SPA-GMN-SRYKAL(IDX)
                   MOVE    SPA-GMN-SRYKALST(IDX)   TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX           TO  IDX
                   MOVE    1                       TO  FLG-OK
               END-IF
           END-PERFORM
           IF     (FLG-OK              =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
      *
      *        受診歴より、保険組合せを確定する
               IF     (FLG-HEN             =   1   )  AND
                      (SPA-NYUGAIKBN       =   "2" )
                   MOVE    SPACE               TO  SPA-NAI-DRCD
                   MOVE    SPACE               TO  SPA-GMN-HKNCOMBINUM
      *            ドクター、保険組合せを受付より
                   IF      SPA-SYSYMD          =   SPA-NAI-SRYYMDS
                       PERFORM 4102-UKETUKE-HEN-SEC
                   END-IF
      *            ドクターを受診履歴より
                   PERFORM 41014-JYURRK-HKNCOMBI-SEC
               END-IF
           END-IF
      *
      **** 変更があった時、ドクターを変更する
      *    IF      FLG-HEN             =   1
      *        INITIALIZE                  ORCSDRSETAREA
      *        MOVE    SPA-NAI-SRYYMDS     TO  ORCSDR-SYSYMD
      *        MOVE    "1"                 TO  ORCSDR-KBN
      *        MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
      *        MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
      *        CALL   "ORCSDRSET"          USING
      *                                    SPA-AREA
      *                                    ORCSDRSETAREA
      *        MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
      *        MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *        MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
      *****END-IF
      *
           .
       4102-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月日入力処理
      *****************************************************************
       4103-SRYYMD-CHK-SEC             SECTION.
      *
      ***  IF      K01-SRYYMD          =   SPA-GMN-SRYYMD
      *        MOVE    4                   TO  SPA-GMN-CUR
      *        GO      TO      4103-SRYYMD-CHK-EXT
      ***  END-IF
      *
      *    訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      SPA-MAE-SRYYMD  NOT =   SPA-GMN-SRYYMD
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    SPA-MAE-SRYYMD      TO  SPA-GMN-SRYYMD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
               GO      TO      4103-SRYYMD-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-SRYYMD
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY          <   SPA-BIRTHDAY
                    MOVE    "0017"         TO  SPA-ERRCD
                    MOVE    3              TO  SPA-GMN-CUR
                    GO      TO      4103-SRYYMD-CHK-EXT
               END-IF
      *********変更のない時、以降の処理はそのまま
      *        IF      SGDAY-SDAY          =   SPA-NAI-SRYYMDS
      *             MOVE    4                   TO  SPA-GMN-CUR
      *             GO      TO      4103-SRYYMD-CHK-EXT
      ******** END-IF
               MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMDS
           ELSE
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4103-SRYYMD-CHK-EXT
           END-IF
      *    変更のない時、以降の処理はそのまま
           IF      SPA-GMN-SRYYMD      =   SPA-MAE-SRYYMD
               GO      TO      4103-SRYYMD-CHK-EXT
           END-IF
           MOVE    SPA-GMN-SRYYMD      TO  SPA-MAE-SRYYMD
      *
      *    システム管理より編集
           PERFORM 3101-SYSKANRI-HEN-SEC
      *    診療科編集
      *????PERFORM 4102-SRYKA-CHK-SEC
      *
      *    入院中かの判定
           PERFORM 41015-PTNYUINRRK-CHK-SEC
      *
      *    受診履歴より、保険組合・継続、ドクターを確定する
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    SPACE               TO  SPA-NAI-DRCD
               MOVE    SPACE               TO  SPA-GMN-SRYKA
               PERFORM 41014-JYURRK-HKNCOMBI-SEC
           END-IF
      *
      *    診療科
           IF      SPA-GMN-SRYKA       =   SPACE
               MOVE    SPA-GMN-SRYKAL(01)  TO  SPA-GMN-SRYKA
           END-IF
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA       =   SPA-GMN-SRYKAL(IDX)
                   MOVE    SPA-GMN-SRYKALST(IDX)   TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX           TO  IDX
               END-IF
           END-PERFORM
      *
      *    ドクター編集
      *****PERFORM 4105-DRCD-CHK-SEC
      *    システム日付より過去の時、受診歴よりドクターを確定
      *    IF      SPA-NAI-SRYYMDS     <   SPA-SYSYMD
      *        PERFORM 41031-JYURRK-DRCD-SEC
      *    END-IF
      *
           .
       4103-SRYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       4112-CALENDAR-SEC            SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
      *    画面入力の年月＝ＳＰＡの年月のチェック
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    K01-YEAR            TO  WRK-CAL-YEAR
           MOVE    K01-MONTH           TO  WRK-CAL-MONTH
           MOVE    K01-DAY             TO  WRK-CAL-DAY
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-CAL-YMD         TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
      *        MOVE    WRK-HENYMDG         TO  SPA-GMN-SRYYMD
      *        MOVE    WRK-CAL-YMD         TO  SPA-NAI-SRYYMDS
      *
               MOVE    WRK-HENYMDG         TO  K01-SRYYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN-SRYYMD
               MOVE    SPA-GMN-HKNCOMBINUM TO  WRK-MAE-HKNCOMBINUM
               PERFORM 4103-SRYYMD-CHK-SEC
      *
               IF      SPA-GMN-HKNCOMBINUM =   SPACE
                   MOVE    WRK-MAE-HKNCOMBINUM TO  SPA-GMN-HKNCOMBINUM
               END-IF
           ELSE
               MOVE    "0013"              TO  SPA-ERRCD
           END-IF
      *
           .
       4112-CALENDAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回処方入力処理
      *****************************************************************
       4104-ZENKAI-CHK-SEC             SECTION.
      *
      *
           IF      SPA-GMN-ZENKAI      =   SPACE
               MOVE    "0"             TO  SPA-GMN-ZENKAI
           END-IF
           IF      SPA-GMN-ZENKAI      =   "0"  OR  "1"
                CONTINUE
            ELSE
                MOVE    "0022"         TO  SPA-ERRCD
                MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
       4104-ZENKAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力処理
      *****************************************************************
       4105-DRCD-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE           TO  SPA-GMN-DRCD-G
               MOVE    SPACE           TO  SPA-NAI-DRCD
               GO      TO      4105-DRCD-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-NAI-SRYYMDS     TO  ORCSDR-SYSYMD
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           IF      ORCSDR-ERRCD    NOT =   ZERO
               MOVE    "0021"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
               GO      TO      4105-DRCD-CHK-EXT
           END-IF
      *
           MOVE    "1"                 TO  SPA-NAI-DRCD(1:1)
           MOVE    SPA-GMN-DRCD        TO  SPA-NAI-DRCD(2:)
      *
      *****IF     (SPA-GMN-DRCD    NOT =   SPACE )  AND
      *           (SPA-GMN-DRCD    NOT NUMERIC   )
      *        INITIALIZE                      ORCSNUMAREA
      *        MOVE    SPA-GMN-DRCD        TO  SNUM-INX
      *        CALL    "ORCSNUM"           USING
      *                                    ORCSNUMAREA
      *        IF     (SNUM-RC         NOT =   ZERO  )   OR
      *               (SNUM-MINKBN     NOT =   SPACE )   OR
      *               (SNUM-SYOKBN     NOT =   SPACE )
      *            MOVE    "0021"                  TO  SPA-ERRCD
      *            MOVE    5                   TO  SPA-GMN-CUR
      *            GO      TO      4105-DRCD-CHK-EXT
      *        ELSE
      *            MOVE    SNUM-OUTEDT(12:4)       TO  SPA-GMN-DRCD
      *        END-IF
      *    END-IF
      *
      *    MOVE    ZERO                TO  FLG-OK
      *    PERFORM VARYING     IDX     FROM   1   BY  1
      *            UNTIL       IDX     >   10
      *        IF      SPA-GMN-DRCD (1:4)  =   SPA-GMN-DRCDL(IDX)
      *            MOVE    SPA-GMN-DRCDLST(IDX)    TO  SPA-GMN-DRCD-G
      *            MOVE    "1"                 TO  SPA-NAI-DRCD(1:1)
      *            MOVE    SPA-GMN-DRCD        TO  SPA-NAI-DRCD(2:)
      *            MOVE    10                  TO  IDX
      *            MOVE    1                   TO  FLG-OK
      *        END-IF
      *    END-PERFORM
      *    IF      FLG-OK              =   ZERO
      *        MOVE    "0021"              TO  SPA-ERRCD
      *        MOVE    5                   TO  SPA-GMN-CUR
      *        GO      TO      4105-DRCD-CHK-EXT
      ******END-IF
      *
           .
       4105-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ番号入力処理
      *****************************************************************
       4104-HKNCOMBI-CHK-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               IF      SPA-GMN-HKNCOMBINUM NOT =   SPA-MAE-HKNCOMBINUM
                   MOVE    "0009"              TO  SPA-ERRCD
                   MOVE    9                   TO  SPA-GMN-CUR
                   MOVE    SPA-MAE-HKNCOMBINUM TO  SPA-GMN-HKNCOMBINUM
               END-IF
               GO      TO      4104-HKNCOMBI-CHK-EXT
           END-IF
           IF     (SPA-GMN-HKNCOMBINUM     NOT =   SPACE )  AND
                  (SPA-GMN-HKNCOMBINUM     NOT NUMERIC   )
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-HKNCOMBINUM TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    9                   TO  SPA-GMN-CUR
                   GO      TO      4104-HKNCOMBI-CHK-EXT
               ELSE
                   MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-HKNCOMBINUM
               END-IF
           END-IF
      *
           IF      SPA-GMN-HKNCOMBINUM   =   SPACE
               MOVE    SPA-NAI-HKNCOMBIOK    TO  SPA-GMN-HKNCOMBINUM
           END-IF
      *
      *    対象の保険組合せチェック処理
           PERFORM 41041-HKNCOMBI-TBLCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    9                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-MAE-HKNCOMBINUM
           END-IF
      *
           .
       4104-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ番号選択入力処理
      *****************************************************************
       4104-HKNCOMBI-LST-SEC             SECTION.
      *
           MOVE    10                  TO  SPA-GMN-CUR
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
              IF      K01-HKNKUM-SELECT(IDX)  =  "T"
                  MOVE    SPA-GMN-THKNCOMBINUM(IDX)
                                              TO  SPA-GMN-HKNCOMBINUM
                  MOVE    SPA-GMN-THKNCOMBINUM(IDX)
                                              TO  WRK-SELNUM
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    SPA-GMN-HKNCOMBINUM     TO  K01-HKNKUMI
           END-IF
      *
           .
       4104-HKNCOMBI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の保険組合せチェック処理
      *****************************************************************
       41041-HKNCOMBI-TBLCHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  SPA-ERRCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-HKN-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE   )  OR
                              (FLG-OK      NOT =   ZERO    )
               IF      SPA-GMN-HKNCOMBINUM =   SPA-GMN-THKNCOMBINUM
                                                                 (IDX)
                   IF      (SPA-NAI-SRYYMDS    >=  SPA-NAI-TEKSTYMD
                                                                 (IDX))
                      AND  (SPA-NAI-SRYYMDS    <=  SPA-NAI-TEKEDYMD
                                                                 (IDX))
                       IF      SPA-NAI-HYOJIKBN (IDX)  =   "1"
                           MOVE    "0024"              TO  SPA-ERRCD
                       ELSE
                           MOVE    IDX             TO  SPA-NAI-HKNIDX
                           MOVE    1               TO  FLG-OK
                       END-IF
                   ELSE
                       MOVE    "0006"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-OK              =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    "0014"              TO  SPA-ERRCD
           END-IF
      *
           .
       41041-HKNCOMBI-TBLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ登録入力処理
      *****************************************************************
      *4106-DOTOR-CHK-SEC             SECTION.
      *
      *    MOVE    6                   TO  SPA-GMN-CUR
      *
      *    MOVE    K01-DOTORO         TO  SPA-GMN-DOTORO
      *    IF      SPA-GMN-DOTORO      =   SPACE
      *        MOVE    "0"             TO  SPA-GMN-DOTORO
      *    END-IF
      *    IF      SPA-GMN-DOTORO      =   "0"  OR  "1"
      *        ADD     1               TO  SPA-GMN-CUR
      *    ELSE
      *        MOVE    "0001"          TO  SPA-ERRCD
      *    END-IF
      *
      *    .
      *4106-DOTOR-CHK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    分散入力処理
      *****************************************************************
       4107-BUNSAN-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-BUNSAN      =   SPACE
               MOVE    "0"             TO  SPA-GMN-BUNSAN
           END-IF
           IF      SPA-GMN-BUNSAN      =   "0"  OR  "1"
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4107-BUNSAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方区分入力処理
      *****************************************************************
       4108-INGAISHOHOKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-INGAIKBN    =   SPACE
               MOVE    "0"             TO  SPA-GMN-INGAIKBN
           END-IF
           IF      SPA-GMN-INGAIKBN    =   ZERO   OR  "1"
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4108-INGAISHOHOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    継続区分入力処理
      *****************************************************************
      *4109-CONTKBN-CHK-SEC             SECTION.
      *
      *
      *    訂正で継続があり既に確定済みの時、継続の変更不可
      *    IF    ( SPA-SYORIFLG        =   1       )  AND
      *          ( SPA-MAE-CONTKBN NOT =   SPA-GMN-CONTKBN )
      *        IF     (SPA-CONTKBN         =   "1"  OR  "2" )  AND
      *               (SPA-DENPJTIKBN  NOT =   "7"          )
      *            MOVE    "0018"              TO  SPA-ERRCD
      *            MOVE    10                  TO  SPA-GMN-CUR
      *            MOVE    SPA-MAE-CONTKBN-G   TO  SPA-GMN-CONTKBN-G
      *            GO      TO      4109-CONTKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
      *    MOVE    SPA-GMN-CONTKBN-G   TO  SPA-MAE-CONTKBN-G
      *
      *    訂正で継続中のものを確定にはできない。
      *    IF      SPA-SYORIFLG        =   1
      *        IF     (SPA-CONTKBN          =   "1"  )  AND
      *               (SPA-GMN-CONTKBN      =   "2"  )
      *            MOVE    "0019"          TO  SPA-ERRCD
      *            MOVE    10                  TO  SPA-GMN-CUR
      *            GO      TO      4109-CONTKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *    IF      SPA-GMN-CONTKBN      =   ZERO    OR  "1"  OR  "2"
      *        CONTINUE
      *    ELSE
      *        MOVE    "0015"              TO  SPA-ERRCD
      *        MOVE    10                  TO  SPA-GMN-CUR
      *        GO      TO      4109-CONTKBN-CHK-EXT
      *    END-IF
      *
      *    IF      SPA-GMN-CONTKBN      =   "2" 
      *        確定の時、継続分がない時、エラー
      *        IF      SPA-NAI-DOUJI-DENPNUM   =   ZERO
      *            MOVE    "0004"              TO  SPA-ERRCD
      **           MOVE    10                  TO  SPA-GMN-CUR
      *            GO      TO      4109-CONTKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
      *    IF      SPA-GMN-CONTKBN      =   "1" 
      *        継続の時、２回以下であること
      *        IF      SPA-NAI-DOUJI-HKNNUM(02)    NOT =   SPACE
      *            MOVE    "0005"          TO  SPA-ERRCD
      *            MOVE    10                  TO  SPA-GMN-CUR
      *            GO      TO      4109-CONTKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
      *    継続の時、同じ保険組合せでないこと
      *    IF      SPA-GMN-CONTKBN      =   "1"  OR  "2"
      *        PERFORM VARYING     IDX-JYU     FROM    1   BY  1
      *                UNTIL      (IDX-JYU     >   3   )   OR
      *                           (SPA-NAI-DOUJI-HKNNUM(IDX-JYU)
      *                                        =   SPACE )
      *            IF      SPA-NAI-DOUJI-HKNNUM(IDX-JYU)
      *                                    =   SPA-GMN-HKNCOMBINUM
      *                MOVE    "0010"          TO  SPA-ERRCD
      *                MOVE    3               TO  IDX-JYU
      *            END-IF
      *        END-PERFORM
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            MOVE    10                  TO  SPA-GMN-CUR
      *            GO      TO      4109-CONTKBN-CHK-EXT
      *        END-IF
      *        包括分入力はエラー
      *        IF      SPA-GMN-HKNCOMBINUM =   "9999"
      *            MOVE    "0023"              TO  SPA-ERRCD
      *            MOVE    10                  TO  SPA-GMN-CUR
      *            GO      TO      4109-CONTKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
      *    中途のデータがある時、継続区分が同じ時、メッセージをクリア
      *    IF      SPA-NAI-WSRYUMU     =   "1"
      *        IF      SPA-NAI-WSRYCONTKBN     =   SPA-GMN-CONTKBN
      *            MOVE    SPACE           TO  SPA-GMN-CONTKBNNAME
      *        END-IF
      *    END-IF
      *
      *
      *    .
      *4109-CONTKBN-CHK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-KAKUTEI-EXT
           END-IF
      *
           IF     (SPA-GMN-PTNUM       =   SPACE   )  OR
                  (SPA-GMN-SRYKA       =   SPACE   )  OR
                  (SPA-GMN-SRYYMD      =   SPACE   )  OR
                  (SPA-NAI-HKNIDX      =   ZERO    )
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      490-KAKUTEI-EXT
           ELSE
               PERFORM 4103-SRYYMD-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      490-KAKUTEI-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN-HKNCOMBINUM     =   ZERO  OR  SPACE
               PERFORM 4104-HKNCOMBI-CHK-SEC
               IF      SPA-GMN-HKNCOMBINUM     =   ZERO  OR  SPACE
                   MOVE    "0006"              TO  SPA-ERRCD
                   GO      TO      490-KAKUTEI-EXT
               END-IF
           END-IF
      *
      *    ワーク診療行為のチェック
      *    IF       SPA-NAI-WSRYUMU    =   SPACE
      *        PERFORM 4903-WSRYCHK-SEC
      *        IF      SPA-NAI-WSRYUMU     =   "1"
      *            MOVE    "0006"          TO  SPA-ERRCD
      *            MOVE    10              TO  SPA-GMN-CUR
      *            GO      TO      490-KAKUTEI-EXT
      *        END-IF
      *****END-IF
      *
      *    共通ＳＰＡ編集
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
      *****MOVE    "K02"               TO  SPA-SAKIPG
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    "K02N"              TO  SPA-SAKIPG
           ELSE
               MOVE    "K02"               TO  SPA-SAKIPG
           END-IF
      *****MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K01"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      **** MOVE    "K02"               TO  MCP-WINDOW.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       共通ＳＰＡ編集処理
      *****************************************************************
       4901-SPAKYOTUKEY-SET-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           INITIALIZE                  SPA-KYOTUKEY
      *
           MOVE    "K01"               TO  SPA-MOTOPG
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-NAI-SRYYMDS     TO  SPA-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
      *****MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    患者マスター読み込み
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-KANANAME      TO  SPA-KANANAME
           MOVE    PTINF-NAME          TO  SPA-NAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-BIRTHDAYW
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
           IF      SPA-NAI-HKNIDX  NOT =   ZERO
               MOVE    SPA-GMN-THKNCOMBINUM (SPA-NAI-HKNIDX)
                                           TO  SPA-HKNCOMBINUM
               MOVE    SPA-GMN-THKNCOMBIMEI (SPA-NAI-HKNIDX)
                                           TO  SPA-HKNCOMBIMEI
               MOVE    SPA-NAI-TFTNRATE    (SPA-NAI-HKNIDX)
                                           TO  SPA-FTNRATE 
               MOVE    SPA-GMN-TFTNRATE    (SPA-NAI-HKNIDX)
                                           TO  SPA-FTNRATEMEI
           END-IF
      *    ドクター
           MOVE    SPA-NAI-DRCD        TO  SPA-DRCD
      *
      *    診療行為固有編集
           IF      SPA-SYORIFLG        =   1
               CONTINUE
           ELSE
      *        診療行為固有編集
               INITIALIZE                  SPA-K01SET-AREA
           END-IF
      *    分散
           MOVE    SPA-GMN-BUNSAN      TO  SPA-BUNSAN
      *    院外処方
           MOVE    SPA-GMN-INGAIKBN    TO  SPA-INGAIKBN
      *    継続区分
      *****MOVE    SPA-GMN-CONTKBN     TO  SPA-CONTKBN
      *
      *****IF      SPA-GMN-CONTKBN     =   "1"  OR  "2"
      *    同時診療伝票番号
      *        MOVE    SPA-NAI-DOUJI-DENPNUM   TO  SPA-DOUJI-DENPNUM
      *    同時診療連番
      *        MOVE    SPA-NAI-DOUJI-RENNUM    TO  SPA-DOUJI-RENNUM
      *****END-IF
      *
      *    保険番号
           MOVE    SPA-NAI-HKNNUM (SPA-NAI-HKNIDX)
                                       TO  SPA-HKNNUM
           MOVE    SPA-NAI-KOH1HKNNUM (SPA-NAI-HKNIDX)
                                       TO  SPA-KOH1HKNNUM
           MOVE    SPA-NAI-KOH2HKNNUM (SPA-NAI-HKNIDX)
                                       TO  SPA-KOH2HKNNUM
           MOVE    SPA-NAI-KOH3HKNNUM (SPA-NAI-HKNIDX)
                                       TO  SPA-KOH3HKNNUM
           MOVE    SPA-NAI-KOH4HKNNUM (SPA-NAI-HKNIDX)
                                       TO  SPA-KOH4HKNNUM
           MOVE    SPA-NAI-RSI-HKNKBN (SPA-NAI-HKNIDX)
                                       TO  SPA-RSI-HKNKBN
      *    前回処方表示フラグ
           MOVE    SPA-GMN-ZENKAI      TO  SPA-ZENSYOHOFLG
      *
           .
       4901-SPAKYOTUKEY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為チェック処理
      *****************************************************************
       4903-WSRYCHK-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-WSRYUMU
           MOVE    SPACE               TO  SPA-NAI-WSRYCONTKBN
      *
           MOVE    SPACE               TO  WRK-SRYYYMM
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  WKSRY-PTID
           MOVE    SPA-GMN-SRYKA       TO  WKSRY-SRYKA
           MOVE    SPA-NAI-SRYYMDS     TO  WKSRY-SRYYMD
           MOVE    SPA-GMN-HKNCOMBINUM TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSINKOUI-READ-SEC
           ELSE
               MOVE    SPACE               TO  WKSRYACT-REC
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      **** IF      FLG-WKSRYACT        =   ZERO
      *        IF      WKSRY-CONTKBN   NOT =   SPA-GMN-CONTKBN
      *            MOVE    "1"             TO  SPA-NAI-WSRYUMU
      *        END-IF
      *        MOVE    WKSRY-CONTKBN       TO  SPA-NAI-WSRYCONTKBN
      *****END-IF
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    IF      SPA-NAI-WSRYUMU     =   "1"
      *D       IF      SPA-GMN-CONTKBNNAME    =   SPACE
      *            MOVE    "中途入力の継続分と継続区分が違います"
      *                                    TO  SPA-GMN-CONTKBNNAME
      *D       END-IF
      *    END-IF
      *
           .
       4903-WSRYCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
      *****MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG2
           MOVE    "K01"               TO  SPA-MOTOPG
      *    戻るのとき、エラーメッセージにＮＯをセット
           MOVE    "NO"                TO  SPA-ERRMSG
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *    INITIALIZE                  SPA-KYOTUKEY
      *    MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      *****MOVE   "JOIN"               TO  MCP-PUTTYPE.
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
      *    初期画面へ
      *****************************************************************
       230-CLEAR-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           INITIALIZE              SPA-K01
           MOVE    SPACE           TO  SPA-DRCD
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       230-CLEAR-EXT.
           EXIT.
      *    
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       230-MAEKPTNUM-SEC             SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      230-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  K01-PTNUM
           PERFORM 4101-PTNUM-CHK-SEC
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *    
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       440-KNJADD-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
      *    受付と患者検索以外の元をセット
           MOVE    "K01"               TO  SPA-MOTOPG
           MOVE    "P01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           INITIALIZE                  SPA-WORK
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      *****MOVE   "JOIN"               TO  MCP-PUTTYPE.
           MOVE   "P01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       440-KNJADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計カードへ
      *****************************************************************
       450-KAIKEI-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
      *    受付と患者検索以外の元をセット
           MOVE    "K01"               TO  SPA-MOTOPG
           MOVE    "J01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      *****MOVE   "JOIN"               TO  MCP-PUTTYPE.
           MOVE   "J01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       450-KAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納登録゛へ
      *****************************************************************
       460-SYUNOU-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
      *    受付と患者検索以外の元をセット
           MOVE    "K01"               TO  SPA-MOTOPG
           MOVE    "S01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      *****MOVE   "JOIN"               TO  MCP-PUTTYPE.
           MOVE   "S01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       460-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K01             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "K01"               TO  SPA-MOTOPG
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
      *
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K01             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K01"               TO  SPA-MOTOPG
           MOVE    "K01"               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE   "Y01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K01             TO  LNK-SCRSPA
      *
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    "K01"               TO  SPA-MOTOPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "U01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    エラーメッセージ
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO          500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K01    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  K01
           INITIALIZE                      K01
      *
           MOVE    SPA-GMN-PTNUM       TO  K01-PTNUM
           MOVE    SPA-GMN-NAME        TO  K01-NAME
           MOVE    SPA-GMN-SRYKA-G     TO  K01-SRYKA
           MOVE    SPA-GMN-SRYYMD      TO  K01-SRYYMD
           MOVE    SPA-GMN-ZENKAI-G    TO  K01-ZENKAI
      *****MOVE    SPA-GMN-DOTORO      TO  K01-DOTORO
           MOVE    SPA-GMN-BUNSAN-G    TO  K01-BUNSAN
           MOVE    SPA-GMN-HKNCOMBINUM TO  K01-HKNKUMI
      *****MOVE    SPA-GMN-RRKYMD      TO  K01-RRKYMD
           MOVE    SPA-GMN-INGAIKBN-G  TO  K01-INGAISHOHOKBN
      *****MOVE    SPA-GMN-CONTKBN-G   TO  K01-CONTKBN
      *****MOVE    SPA-GMN-CONTKBNNAME TO  K01-KZKKBNNAME
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)  TO  K01-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  K01-SRYKA-COUNT
      *    院外区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-INGAI-MAX
               MOVE    SPA-GMN-INGAIKBN-COMB (IDX)
                                           TO  K01-INGAI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-INGAI-MAX    TO  K01-INGAI-COUNT
      *
      **** PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   SPA-GMN-CONTKBN-MAX
      *        MOVE    SPA-GMN-CONTKBN-COMB (IDX)
      *                                    TO  K01-CONTKBN-ITEM(IDX)
      *    END-PERFORM
      *****MOVE    SPA-GMN-CONTKBN-MAX    TO  K01-CONTKBN-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-BUNSAN-MAX
               MOVE    SPA-GMN-BUNSAN-COMB (IDX)
                                           TO  K01-BUNSAN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-BUNSAN-MAX    TO  K01-BUNSAN-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-ZENKAI-MAX
               MOVE    SPA-GMN-ZENKAI-COMB (IDX)
                                           TO  K01-ZENKAI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-ZENKAI-MAX    TO  K01-ZENKAI-COUNT
      *
      *    カレンダー
           IF      SPA-NAI-SRYYMDS(1:6)     =   SPA-SYSYMD(1:6)
               MOVE    SPA-NAI-SRYYY        TO  K01-YEAR
               MOVE    SPA-NAI-SRYMM        TO  K01-MONTH
               MOVE    SPA-NAI-SRYDD        TO  K01-DAY
           END-IF
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K01-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K01-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K01-SYORIMEI-VALUE
           END-EVALUATE
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST (IDX)  TO  K01-DRNAME-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX       TO  K01-DRNAME-COUNT
           MOVE    SPA-GMN-DRCD-G     TO  K01-DRNAME
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
               IF      SPA-GMN-THKNCOMBINUM (IDX)  NOT =   ZERO
                   MOVE    SPA-GMN-THKNCOMBINUM (IDX)
                                           TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  K01-THKNCOMBINUM   (IDX)
               END-IF
               MOVE    SPA-GMN-THKNCOMBIMEI (IDX)
                                           TO  K01-THKNCOMBIMEI  (IDX)
               MOVE    SPA-GMN-TTEKSTYMD (IDX)
                                           TO  K01-TTEKSTYMD(IDX)
               MOVE    SPA-GMN-TTEKEDYMD (IDX)
                                           TO  K01-TTEKEDYMD(IDX)
               MOVE    SPA-GMN-TFTNRATE(IDX)
                                           TO  K01-TFTNRATE(IDX)
      *
               IF      SPA-GMN-THKNCOMBINUM(IDX)
                                               =   SPA-GMN-HKNCOMBINUM
                   MOVE    "T"             TO  K01-HKNKUM-SELECT(IDX)
                   MOVE    IDX             TO  K01-HKNKUM-ROW
                   MOVE    2               TO  K01-HKNKUM-ROWATTR
               ELSE
                   MOVE    "F"             TO  K01-HKNKUM-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  K01-HKNKUM-COUNT
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
          IF       SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYYMD"        TO  MCP-WIDGET
               WHEN    04
                   MOVE    "ZENKAI"        TO  MCP-WIDGET
               WHEN    05
                   MOVE    "DRNAME"        TO  MCP-WIDGET
      **       WHEN    06
      **           MOVE    "DOTORO"        TO  MCP-WIDGET
               WHEN    07
                   MOVE    "BUNSAN"        TO  MCP-WIDGET
               WHEN    08
                   MOVE    "INGAISHOHOKBN" TO  MCP-WIDGET
               WHEN    09
                   MOVE    "HKNKUMI"       TO  MCP-WIDGET
      ******** WHEN    10
      **********   MOVE    "CONTKBN"       TO  MCP-WIDGET
               WHEN    11
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力位置よりカーソルセット処理
      *****************************************************************
       50011-CUR-SET-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "PTNUM"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SRYYMD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "ZENKAI"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "DRNAME"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "BUNSAN"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "INGAISHOHOKBN"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "HKNKUMI"
                   MOVE    11              TO  SPA-GMN-CUR
      *****    WHEN    "CONTKBN"
      ****         MOVE    11              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "未入力があります" 
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "患者番号がありません。" 
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE
                   "継続中ではありません。確定は選択できません" 
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "継続は２回までです。確定に変更して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "保険組合は適用外です"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "保険組合せがありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
               MOVE    "訂正時は変更できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "既に継続中の保険組合せです。変更して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "患者マスタに存在しません。"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "診療科が存在しません。"
                                               TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "診療日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "保険組合せ選択エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "継続区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "患者番号未入力です。"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "誕生日前の診療日は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "訂正で継続確定の時、継続入力はできません"
                                               TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "訂正の時、確定へ変更できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "労災保険は継続はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "ドクターコードがありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "前回処方区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "包括分入力は選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "非表示の保険組合せは選択できません。"
                                               TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
      *
               WHEN    OTHER
                   MOVE    SPA-ERRCD           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K01"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスタ読み込み処理
      *****************************************************************
       900-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSINKOUI-READ-SEC          SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSINKOUI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"       TO  MCP-TABLE
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
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
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
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
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
