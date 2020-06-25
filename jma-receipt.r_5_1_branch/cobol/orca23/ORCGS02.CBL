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
       PROGRAM-ID.                 ORCGS02.
      **************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納登録
      *  コンポーネント名  : 請求一覧（S02）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      **************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−小豆沢  01/04/18  伝票発行日の修正
      *  01.00.02    MCC−小豆沢  01/04/18  再計算処理時の引き渡しパラメタ
      *                                     作成時に自動算定区分の判定を
      *                                     追加
      *  01.00.03    MCC−小豆沢  01/04/18  薬剤料逓減入力時の再計算修正
      *  01.00.04    MCC−小豆沢  01/04/18  診療行為テーブル読み込み条件
      *                                     を修正
      *  01.00.05    MCC−小豆沢  01/06/05  再計算処理時の引き渡しパラメタ
      *                                     作成時に診療種別区分の判定を
      *                                     追加
      *  01.00.06    MCC−小豆沢  01/06/05  再計算処理時の引き渡しパラメタ
      *                                     作成時に診療行為コードの判定を
      *                                     追加（薬剤であることの判定）
      *  01.00.07    MCC−小豆沢  01/07/12  保険適用外金額対応
      *                                     診療種別区分　.950　消費税なし
      *                                                   .960　消費税あり
      *  01.00.07    MCC−小豆沢  01/07/16  ＪＤ入金の削除 
      *                                     調整金のボタン中止      
      *  01.00.08    MCC−多々納  01/08/23  診療行為よりの遷移を追加
      *  01.00.09    MCC−小豆沢  01/09/19  コラムリストの選択追加
      *  01.01.01    MCC-藤原     01/11/07  患者番号、診療科、請求年月を
      *                                     入力項目に変更
      *                                     S01の削除
      *  01.01.01    MCC-多々納   01/12/12  画面遷移の修正
      *  01.01.03    MCC-小豆沢   01/12/13  再計算時の薬剤判定の修正
      *  01.01.04    MCC-多々納   02/01/31  選択番号の表示修正
      *  01.01.05    MCC-小豆沢   02/03/19  請求書再発行時に請求額と入金額
      *                                     のセット
      *  01.01.06    MCC-多々納   02/04/25  自費の判定に’０９６’を追加
      *  01.01.07    MCC-小豆沢   02/05/30  労災保険対応を追加
      *  01.01.08    MCC-多々納   02/06/05  患者氏名での検索を追加
      *  01.01.09    MCC-多々納   02/07/19  患者検索エラーメッセージ修正
      *  01.01.10    NACL-多々納  02/09/02  請求書をカスタマイズに修正
      *  01.01.11    NACL-小豆沢  02/09/12  自費の再計算時に薬剤、器材の
      *                                     集計を追加
      *  01.01.12    NACL-門脇    02/09/17  自賠責対応
      *  01.01.13    NACL-多々納  02/09/18  保険表示修正
      *  01.01.14    NACL-藤原    02/11/05  入外区分追加
      *  01.01.15    NACL-門脇    02.11.21  自賠責（ポリネック等の対応）
      *  01.01.16    NACL-太田    02.12.11  診療科・入外区分を判断して
      *                                     初期表示するよう修正
      *  01.01.17    NACL-太田    02.12.13  入院分の請求書再発行対応
      *  01.01.18    NACL-太田    02.12.16  帳票引数入外区分に選択された
      *                                     収納の入外区分を渡すよう修正
      *                                     コラムリストの金額欄を
      *                                     カンマ編集するよう修正
      *  01.01.19    NACL-森脇    02/12/19  支払証明書印刷追加
      *  01.01.20    NACL-太田    03/02/18  入院請求は取消可能とする
      *  01.01.21    NACL-太田    03/03/07  排他制御機能追加
      *  01.01.22    NACL-太田    03/03/13  入院請求書複数枚一括印刷対応
      *  01.01.23    NACL-太田    03.03.18  入院カスタマイズ帳票対応
      *  01.01.24    NACL-門脇    03.04.15  自賠責文書料消費税対応
      *  01.01.25    NACL-門脇    03.04.16  船員保険（被保険者）＋通勤災害
      *                                     ＋初診時　対応
      *  01.01.26    NACL-太田    03.05.01  収納一覧リスト表示修正
      *  01.01.27    NACL-太田    03.05.13  患者が特定された時点で
      *                                     支払証明書が出力できるよう修正
      *  01.01.28    NACL-門脇    03.05.14  自賠責：腰部固定帯対応
      *  01.01.29    NACL-門脇    03.05.20  自賠責：療養担当手当て対応
      *  01.00.30    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.31    NACL-太田    03.08.26  請求書の引数設定修正
      *                                     （労災対応）
      *  01.00.32    NACL-小豆沢  03.09.17  福岡県の対応追加（再計算時に給付
      *                                     対象外点数を取り込み）
      *  01.00.33    NACL-太田    03.10.01  未収金管理機能強化対応
      *  01.00.34    NACL-太田    03.11.12  更新日付編集対応
      *  01.00.35    NACL-門脇    04.01.14  診療行為の包括分は除く
      *                                     SRYACCT2→32
      *  01.00.36    NACL-門脇    04.01.21  労災の特定器材(09593)対応
      *  01.00.37    NACL-太田    04.02.09  コラムリスト編集修正
      *  01.00.38    NACL-門脇    04.07.16  再計算をサブへ修正
      *  01.00.39    NACL-門脇    04.12.01  収納外来一括再計算対応
      *  01.00.40    NACL-門脇    06.03.20  自立支援法対応
      *  01.00.41    NACL-太田    06.08.18  まとめ再発行対応
      *  03.04.01    NACL-森脇    06.12.19  支払証明書プレビュー表示
      *  03.05.00    NACL-太田    07/05/10  グループ診療対応
      *  04.04.00    NACL-太田    08/12/08  収納検索画面を吸収
      *
      *  04.05.01    NACL-門脇    09/06/16  一括再計算方法選択機能対応
      *  04.05.02    NACL-門脇    12/03/09  改正対応：外来現物給付対応
      *
      *  04.06.01    NACL-門脇    10/06/25  会計照会【収納更新】未処理分
      *                                     メッセージ対応
      *
      *  04.07.01    NACL-門脇    14/07/31  第三者行為の現物給付対応
      *  04.08.01    NACL-門脇    17/01/05  一括再計算該当者のリストアップ
      *                                     対応
      *  04.08.02    NACL-門脇    17/12/15  第三者行為現物給付に関わる
      *                                     負担金計算対応
      *  05.00.01    NACL-門脇    19/10/01  マル長対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求書用パラメタファイル
           SELECT  BS01-FILE   ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    パラメータファイル
       FD  BS01-FILE.
       01  BS01-REC.
           COPY    "CPPARA.INC"    REPLACING //PARA-//
                                   BY        //BS01-//.
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //PRFNAME//.
           COPY    "CPASGNPARA.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    収納共通パラメタ
           COPY    "S01COMMON-SPA".
      *
           COPY    ENUM-VALUE.
      *
       01  SPA-S02.
      *
           03  SPA-S02-AREA.
               05  SPA-GMN-PAGE                    PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYYM              PIC X(10).
                   07  SPA-GMN-MODE-G.
                       09  SPA-GMN-MODE        PIC X(01).
                       09  SPA-GMN-F1          PIC X(01).
                       09  SPA-GMN-MODEMEI     PIC X(20).
                   07  SPA-GMN-SORTKBN-G.
                       09  SPA-GMN-SORTKBN     PIC X(01).
                       09  SPA-GMN-F1          PIC X(01).
                       09  SPA-GMN-SORTKBNMEI  PIC X(20).
                   07  SPA-GMN-HEAD.
                       09  SPA-GMN-PTID            PIC 9(10).
                       09  SPA-GMN-PTNUM           PIC X(20).
                       09  SPA-GMN-KANANAME        PIC X(100).
                       09  SPA-GMN-SEX             PIC X(02).
                       09  SPA-GMN-NAME            PIC X(100).
                       09  SPA-GMN-BIRTHDAY        PIC X(09).
                       09  SPA-GMN-SRYKA-G.
                           11  SPA-GMN-SRYKA       PIC X(02).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-SRYKAMEI    PIC X(16).
                       09  SPA-GMN-NYUGAIKBN-G.
                           11  SPA-GMN-NYUGAIKBN   PIC X(01).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEI
                                                   PIC X(06).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-SELECTED        PIC 9(01).
                       09  SPA-GMN-MAX             PIC 9(05).
                       09  SPA-GMN-NEXT            PIC 9(01).
                       09  SPA-GMN-TBLREC  OCCURS  500.
                           11  SPA-GMN-TBANGO      PIC 9(03).
                           11  SPA-GMN-TDENPNUM    PIC 9(07).
                           11  SPA-GMN-DENPGRP     PIC X(01).
                           11  SPA-GMN-TNYUGAIKBN  PIC X(04).
                           11  SPA-GMN-TSRYKA      PIC X(06).
                           11  SPA-GMN-THKNCOMBI   PIC X(50).
                           11  SPA-GMN-TFTNRATE    PIC X(08).
                           11  SPA-GMN-TSRYYMD     PIC X(15).
                           11  SPA-GMN-TSKYEDYMD   PIC X(06).
                           11  SPA-GMN-THAKYMD     PIC X(09).
                           11  SPA-GMN-TSEIKYU     PIC 9(10).
                           11  SPA-GMN-TNYUKIN     PIC S9(10).
                           11  SPA-GMN-TMISYU      PIC S9(10).
                           11  SPA-GMN-TJYOTAI     PIC X(16).
                           11  SPA-GMN-SIKIBETUFLG PIC X(01).
                       09  SPA-GMN-TBL-SEL         PIC 9(05).
      *
                   07  SPA-GMN-INPUT.
                       09  SPA-GMN-SELBAN          PIC 9(04).
                   07  SPA-GMN-MISYU.
                       09  SPA-GMN-GAI-MISYU       PIC S9(08).
                       09  SPA-GMN-NYU-MISYU       PIC S9(08).
                   07  SPA-GMN-TUKISKY.
                       09  SPA-GMN-GAI-TUKISKY     PIC S9(10).
                       09  SPA-GMN-NYU-TUKISKY     PIC S9(10).
                   07  SPA-GMN-SYUMEI-AREA.
                       09  SPA-GMN-CHOSEI1IN       PIC S9(10).
                       09  SPA-GMN-CHOSEI1INX      PIC X(10).
                       09  SPA-GMN-CHOSEI2IN       PIC S9(10).
                       09  SPA-GMN-CHOSEI2INX      PIC X(10).
                       09  SPA-GMN-SKYMONEYIN      PIC S9(10).
                       09  SPA-GMN-NYUKIN          PIC S9(10).
                       09  SPA-GMN-NYUKINX         PIC X(10).
                       09  SPA-GMN-NYKN-HOHO-G.
                           11  SPA-GMN-NYKN-HOHO       PIC X(02).
                           11  SPA-GMN-NYKN-HOHOFL     PIC X(01).
                           11  SPA-GMN-NYKN-HOHOMEI    PIC X(30).
                       09  SPA-GMN-NYUKINYMD       PIC X(10).
                   07  SPA-GMN-SMEILIST-AREA.
                       09  SPA-GMN-SKYPRT-G.
                           11  SPA-GMN-SKYPRT      PIC X(02).
                           11  SPA-GMN-SKYPRTFL    PIC X(01).
                           11  SPA-GMN-SKYPRTMEI   PIC X(30).
                       09  SPA-GMN-SRYPRT-G.
                           11  SPA-GMN-SRYPRT      PIC X(02).
                           11  SPA-GMN-SRYPRTFL    PIC X(01).
                           11  SPA-GMN-SRYPRTMEI   PIC X(30).
                       09  SPA-GMN-EDANUM              PIC 9(02).
                       09  SPA-GMN-SMEILIST-MAX        PIC 9(02).
                       09  SPA-GMN-SMEILIST-SEL        PIC 9(02).
                       09  SPA-GMN-SMEITBL-OCC         OCCURS  99.
                           11  SPA-GMN-TMDENPEDANUM    PIC 9(02).
                           11  SPA-GMN-TMSKYMONEY      PIC S9(10).
                           11  SPA-GMN-TMNYUKIN        PIC S9(10).
                           11  SPA-GMN-TMSYORIYMD      PIC X(09).
                           11  SPA-GMN-TMJOUTAI        PIC X(12).
                           11  SPA-GMN-TMNYKN-HOHO     PIC X(10).
      *
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-SRYKALST-G.
                       09  SPA-GMN-SRYKA-MAX       PIC 9(03).
                       09  SPA-GMN-SRYKALST        OCCURS  99.
                          11  SPA-GMN-SRYKAL       PIC X(02).
                          11  SPA-GMN-SRYKAFL      PIC X(01).
                          11  SPA-GMN-SRYKAMEIL    PIC X(16).
                   07  SPA-GMN-NYUGAIKBNLST-G.
                       09  SPA-GMN-NYUGAIKBN-MAX   PIC 9(02).
                       09  SPA-GMN-NYUGAIKBNLST    OCCURS  3.
                           11  SPA-GMN-NYUGAIKBNL  PIC X(01).
                           11  SPA-GMN-NYUGAIKBNFL PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEIL
                                                   PIC X(06).
                   07  SPA-GMN-MODELST-G.
                       09  SPA-GMN-MODE-MAX        PIC 9(02).
                       09  SPA-GMN-MODELST         OCCURS  5.
                           11  SPA-GMN-MODEL       PIC X(01).
                           11  SPA-GMN-MODEFL      PIC X(01).
                           11  SPA-GMN-MODEMEIL    PIC X(20).
                   07  SPA-GMN-SORTKBNLST-G.
                       09  SPA-GMN-SORTKBN-MAX     PIC 9(02).
                       09  SPA-GMN-SORTKBNLST      OCCURS  5.
                           11  SPA-GMN-SORTKBNL    PIC X(01).
                           11  SPA-GMN-SORTKBNFL   PIC X(01).
                           11  SPA-GMN-SORTKBNMEIL PIC X(20).
                   07  SPA-GMN-NYKN-HOHOLST-G.
                       09  SPA-GMN-NYKN-HOHO-MAX       PIC 9(05).
                       09  SPA-GMN-NYKN-HOHOLST-OCC    OCCURS  50.
                           11  SPA-GMN-NYKN-HOHOLST.
                               13  SPA-GMN-NYKN-HOHOL      PIC X(02).
                               13  SPA-GMN-NYKN-HOHOFLL    PIC X(01).
                               13  SPA-GMN-NYKN-HOHOMEIL   PIC X(30).
                           11  SPA-GMN-NYKN-HOHOMEI2L      PIC X(10).
                   07  SPA-GMN-SKYPRTLST-G.
                       09  SPA-GMN-SKYPRT-MAX          PIC 9(05).
                       09  SPA-GMN-SKYPRTLST-OCC       OCCURS  5.
                           11  SPA-GMN-SKYPRTLST.
                               13  SPA-GMN-SKYPRTL     PIC X(02).
                               13  SPA-GMN-SKYPRTFLL   PIC X(01).
                               13  SPA-GMN-SKYPRTMEIL  PIC X(30).
                   07  SPA-GMN-SRYPRTLST-G.
                       09  SPA-GMN-SRYPRT-MAX          PIC 9(05).
                       09  SPA-GMN-SRYPRTLST-OCC       OCCURS  5.
                           11  SPA-GMN-SRYPRTLST.
                               13  SPA-GMN-SRYPRTL     PIC X(02).
                               13  SPA-GMN-SRYPRTFLL   PIC X(01).
                               13  SPA-GMN-SRYPRTMEIL  PIC X(30).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-SRYYM               PIC X(08).
                   07  SPA-NAI-SYS1039TBL          PIC X(500).
                   07  SPA-NAI-S10-CHK             PIC X(01).
                   07  SPA-NAI-HEAD.
                       09  SPA-NAI-LOCK            PIC 9(01).
                       09  SPA-NAI-SLOCK-MSG       PIC X(32).
                       09  SPA-NAI-SEX             PIC X(01).
                       09  SPA-NAI-BIRTHDAY        PIC X(08).
                       09  SPA-NAI-NYUKIN-HOHO     PIC X(02).
                       09  SPA-NAI-SKYPRT          PIC X(01).
                       09  SPA-NAI-GYOUMUID        PIC X(03).
                       09  SPA-NAI-FILEMEI.
                           11  SPA-NAI-FILEMEI-YMD PIC X(08).
                           11  SPA-NAI-FILEMEI-HMS PIC X(06).
                           11  SPA-NAI-FILEMEI-CD  PIC X(06).
                       09  SPA-NAI-UID             PIC X(36).
                       09  SPA-NAI-HKNCOMBI-G.
                           11  SPA-NAI-HKNCOMBI-MAX        PIC 9(05).
                           11  SPA-NAI-HKNCOMBILST         OCCURS  100.
                               13  SPA-NAI-HKNCOMBIL       PIC X(04).
                               13  SPA-NAI-HOJOKBNL        PIC X(01).
                               13  SPA-NAI-HKNCOMBIMEIL    PIC X(50).
                               13  SPA-NAI-HKNNUML         PIC X(03).
                               13  SPA-NAI-RSI-HKNKBNL     PIC X(01).
                   07  SPA-TUKI-PARA-G.
                       09  SPA-TUKI-DENPNUM        PIC  9(07).
                       09  SPA-TUKI-SRYYM          PIC  X(06).
                       09  SPA-TUKI-KIKANKBN       PIC  X(06).
                       09  SPA-TUKI-STYMD          PIC  X(08).
                       09  SPA-TUKI-EDYMD          PIC  X(08).
                       09  SPA-TUKI-GRPKBN         PIC  X(01).
                       09  SPA-TUKI-SKYPRT         PIC  X(01).
                       09  SPA-TUKI-SRYPRT         PIC  X(01).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-THSRYYM         PIC  X(08).
                       09  SPA-NAI-TBLREC  OCCURS  500.
                           11  SPA-NAI-TBUNBEN     PIC  X(01).
                           11  SPA-NAI-THOJOKBN    PIC  X(01).
                           11  SPA-NAI-TGRP-DENPNUM PIC  9(07).
                           11  SPA-NAI-TSRYKA      PIC  X(02).
                           11  SPA-NAI-TSRYKAMEI   PIC  X(16).
                           11  SPA-NAI-TRECESRYKA  PIC  X(02).
                           11  SPA-NAI-TSRYYMD     PIC  X(08).
                           11  SPA-NAI-THKNNUM     PIC  X(03).
                           11  SPA-NAI-TRSI-HKNKBN PIC  X(01).
                           11  SPA-NAI-THAKYMD     PIC  X(08).
                           11  SPA-NAI-TSKYEDYMD   PIC  X(08).
                           11  SPA-NAI-TNYUGAIKBN  PIC  X(01).
                           11  SPA-NAI-TJYOTAIKBN  PIC  X(01).
                   07  SPA-NAI-SYUMEI-AREA.
                       09  SPA-NAI-SYORINO         PIC X(02).
                       09  SPA-NAI-CHOSEI1IN-MOTO  PIC S9(10).
                       09  SPA-NAI-CHOSEI2IN-MOTO  PIC S9(10).
                       09  SPA-NAI-SKYMONEY-MOTO   PIC S9(10).
                       09  SPA-NAI-SKYMONEY        PIC S9(10).
                       09  SPA-NAI-NYUKINYMD       PIC X(08).
                       09  SPA-NAI-NYUKIN-MOTO     PIC S9(10).
                       09  SPA-NAI-DENPEDANUM      PIC 9(02).
                       09  SPA-NAI-SYURRKNUM       PIC 9(09).
                       09  SPA-NAI-SYUEDANUM       PIC 9(09).
                       09  SPA-NAI-SYURRKUPDKBN    PIC X(1).
                       09  SPA-NAI-DENPJTIKBN      PIC X(01).
                       09  SPA-NAI-MINUS           PIC X(01).
                       09  SPA-NAI-PROC-FLG        PIC 9(01).
                   07  SPA-NAI-SMEILIST-AREA.
                       09  SPA-NAI-SMEITBL-OCC         OCCURS  99.
                           11  SPA-NAI-TMNYKN-HOHO     PIC X(02).
                           11  SPA-NAI-TMJOUTAIKBN     PIC X(1).
                           11  SPA-NAI-TMMEIJOUTAIKBN  PIC X(1).
                           11  SPA-NAI-TMSYORIYMD      PIC X(08).
                           11  SPA-NAI-TMMINUS        PIC X(1).
                           11  SPA-NAI-TMSYURRKNUM     PIC 9(9).
                           11  SPA-NAI-TMSYUEDANUM     PIC 9(9).
                           11  SPA-NAI-TMSYURRKUPDKBN  PIC X(1).
                           11  SPA-NAI-TMCHOSEI1       PIC S9(10).
                           11  SPA-NAI-TMCHOSEI2       PIC S9(10).
                           11  SPA-NAI-TMPROC-FLG      PIC 9(01).
      *
               05  SPA-NAI-COMBO-AREA.
                   07  SPA-NAI-SRYKALST-G.
                       09  SPA-NAI-SRYKALST        OCCURS  99.
                          11  SPA-NAI-SRYKAMEIL    PIC X(06).
                          11  SPA-NAI-RECEKAL      PIC X(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUTTL          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
           03  FLG-TEIKIRRK        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYULIST-ROW     PIC 9(01).
           03  FLG-MEILIST-ROW     PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-HIT             PIC 9(01).
           03  FLG-HIZUKE-RC       PIC 9(01).
           03  FLG-SEIKYU-PRT      PIC 9(01).
           03  FLG-MEISAI-PRT      PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-BUNBENCHK       PIC 9(01).
           03  FLG-ORCSNUMERR      PIC 9(01).
           03  FLG-GENBUTU         PIC 9(01).
           03  FLG-GENBUTU2        PIC 9(01).
           03  FLG-GENBUTU3        PIC 9(01).
           03  FLG-GENBUTU4        PIC 9(01).
           03  FLG-S10-CHK         PIC 9(01).
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
           03  IDX9                PIC 9(05).
           03  IDX10               PIC 9(05).
           03  IDX-COMB            PIC 9(05).
           03  IDX12               PIC 9(05).
           03  IDX-CMB             PIC 9(05).
           03  IDXB                PIC 9(05).
           03  IDXD                PIC 9(05).
           03  IDXH                PIC 9(05).
           03  IDXL                PIC 9(05).
           03  IDXM                PIC 9(05).
           03  IDXST               PIC 9(05).
           03  IDX-DEF             PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-PRTKANRI-RENNUM PIC 9(05).
           03  CNT-JOBID           PIC 9(07).
           03  CNT-PAGE            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-STATE           PIC S9(9)   BINARY.
           03  WRK-SIDCD           PIC X(04).
           03  WRK-SIDMSG          PIC X(70).
           03  WRK-CMB-CD          PIC X(50).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM     PIC X(200).
               05  WRK-CMB-ITM2    PIC X(200).
           03  WRK-SELBAN          PIC 9(05).
           03  WRK-SYMD            PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-NAI-SRYYM       PIC X(08).
           03  WRK-SAVE-SRYYM      PIC X(08).
           03  WRK-PGNAME-G.
               05  WRK-PGNAME-OCC  OCCURS  3.
                   07  WRK-PGNAME  PIC X(20).
                   07  WRK-PGTYPE  PIC X(20).
           03  WRK-SKYPRT          PIC X(01).
           03  WRK-SRYPRT          PIC X(01).
      *
      *    保険組合せ名称
           03  WRK-KUMINO          PIC X(04).
           03  WRK-HKNKUMI         PIC X(40).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-HOJOKBN         PIC X(01).
           03  WRK-HKNKBN-MEI      PIC X(02).
           03  WRK-RSI-HKNKBN      PIC X(01).
           03  WRK-HKNID           PIC 9(10).
           03  WRK-NYUKIN          PIC S9(10).
           03  WRK-SKYMONEY        PIC S9(10).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
           03  WRK-SAGAKU          PIC S9(07).
      *
           03  WRK-EDANUMZ         PIC ZZ.
           03  WRK-TBANGO-W        PIC ZZZ.
           03  WRK-TSEIKYU-W       PIC ZZ,ZZZ,ZZZ.
           03  WRK-TNYUKIN-W       PIC --,---,---.
           03  WRK-TMISYU-W        PIC --,---,---.
           03  WRK-SELBAN-W        PIC ZZZZ.  
           03  WRK-KINGAKUZ        PIC ----------.
           03  WRK-KINGAKUX        PIC X(10).
           03  WRK-KINGAKU9        PIC S9(10).
           03  WRK-KINGAKUZ2       PIC --,---,---.
      *    請求書ＰＧ名
           03  WRK-SEIKYU-PG       PIC X(20).
           03  WRK-MEISAI-PG       PIC X(20).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-MISYU-G.
               05  WRK-MISYU       PIC S9(08).
               05  WRK-MISYU-P     PIC S9(08).
               05  WRK-MISYU-M     PIC S9(08).
           03  WRK-TBL-SEL-MOT     PIC 9(05).
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
      *----(03.04.01)--ADD-START---
           03  WRK-PRT-PAGE        PIC 9(10).
      *----(03.04.01)--ADD-END-----
      *
           03  WRK-GYOUMUID        PIC X(03).
           03  WRK-UID             PIC X(36).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD PIC X(08).
               05  WRK-FILEMEI-HMS PIC X(06).
               05  WRK-FILEMEI-CD  PIC X(06).
      *
           03  WRK-GAI-SRYYMD      PIC X(08).
           03  WRK-GAI-DENPNUM     PIC 9(07).
           03  WRK-GAI-SEIKYU      PIC S9(10).
           03  WRK-GAI-NYUKIN      PIC S9(10).
           03  WRK-GAI-MISYU-G.
               05  WRK-GAI-MISYU   PIC S9(10).
               05  WRK-GAI-MISYU-P PIC S9(10).
               05  WRK-GAI-MISYU-M PIC S9(10).
           03  WRK-NYU-MISYU       PIC S9(10).
           03  WRK-GAI-ZENMISYU    PIC S9(10).
           03  WRK-JYOTAI          PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-SRYYM           PIC X(06).
           03  WRK-SYULIST-PAGE-G.
               05  WRK-SYULIST-PAGE    PIC ZZZZZ.
               05  WRK-SYULIST-LBL     PIC X(12).
           03  WRK-SKYYM           PIC X(06).
           03  WRK-TABLE           PIC X(64).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-ORG-NYUGAIKBN   PIC X(01).
           03  WRK-ORG-DENPNUM     PIC 9(07).
           03  WRK-ORG-SYORINO     PIC X(02).
           03  WRK-PAGE-CNT        PIC 9(06).
           03  WRK-DENPEDANUM      PIC 9(02).
           03  WRK-OLD-NYUHEN-YMD  PIC X(08).
           03  WRK-S01-NYUGAIKBN   PIC X(01).
           03  WRK-S01-DENPNUM     PIC 9(07).
      *
           03  WRK-IKT-TABLE       PIC X(64).
           03  WRK-IKT-PATHNAME    PIC X(64).
           03  WRK-NAI-SYORINO     PIC X(02).
           03  WRK-NUMX.
               05  WRK-NUM9        PIC 9(01).
           03  WRK-MAX             PIC 9(10).
           03  WRK-GMN-MODE        PIC X(01).
           03  WRK-GMN-SORTKBN     PIC X(01).
           03  WRK-GMN-PTID        PIC 9(10).
           03  WRK-GMN-CUR         PIC 9(3).
      *
           03  WRK-CHECKFLG-TABLE.
             04  WRK-CHECKFLG-OCC  OCCURS   6.
               05  WRK-CHECKFLG    PIC 9(01).
      *
       01  WRK-ROW.
           05  WRK-SYULIST-ROW     PIC S9(9)   BINARY.
           05  WRK-MEILIST-ROW     PIC S9(9)   BINARY.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG2        PIC X(22).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
           03  WRK-SKYPRTMSG       PIC X(50).
      *
       01  WK-GMN-RITU.
           03  WK-GMN-FTNRITU      PIC ZZZ.
           03  WK-GMN-PA           PIC X(01).
           03  WK-GMN-FL           PIC X(01).
      *
      *    スパ領域 ワーク
           COPY "COMMON-SPA"       REPLACING //SPA-//
                                   BY        //WRK-SPA-//.
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
       01  TNYUKIN-AREA.
           03  TNYUKIN-MAX                     PIC 9(03).
           03  TNYUKIN-OCC                     OCCURS 99.
               05  TNYUKIN-NYUKIN              PIC S9(09).
               05  TNYUKIN-HOHO                PIC X(02).
      *
       01  THOHO-AREA.
           03  THOHO-MAX                     PIC 9(03).
           03  THOHO-OCC                     OCCURS 99.
               05  THOHO-NYUKIN              PIC S9(09).
               05  THOHO-HOHO                PIC X(02).
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  CONST-AREA.
           03  CONST-KINGAKU-MAX               PIC 9(10) VALUE 9999999.
           03  CONST-PROC-G.
               05  CONST-PROC-NYUKIN          PIC X(02) VALUE "05".
               05  CONST-PROC-RRKSYU          PIC X(02) VALUE "08".
               05  CONST-PROC-TORIKESHI       PIC X(02) VALUE "10".
           03  CONST-MODE-NORMON               PIC 9(01) VALUE 0.
           03  CONST-MODE-NORYEAR              PIC 9(01) VALUE 1.
           03  CONST-MODE-NORALL               PIC 9(01) VALUE 2.
           03  CONST-MODE-MISYU                PIC 9(01) VALUE 3.
           03  CONST-MODE-NYUHEN               PIC 9(01) VALUE 4.
           03  CONST-EDANUM-MAX                PIC 9(03) VALUE 99.
           03  CONST-SYULST-MAX                PIC 9(03) VALUE 500.
           03  CONST-SRYKA-MAX                 PIC 9(03) VALUE 99.
           03  CONST-ZENSRYKA-G.
               05  CONST-ZENSRYKA      PIC X(02)   VALUE "00".
               05                      PIC X(01)   VALUE SPACE.
               05  CONST-ZENSRYKAMEI   PIC X(16)   VALUE "全科".
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  CONST-3003-KBNCD            PIC X(08)   VALUE "023".
      *
      *    印刷管理ＤＢのキー値
           03  CONST-PRTKANRI-TBL-KEY      PIC X(08)   VALUE "ORCBS01".
           03  CONST-PRTKANRI-TBL-GROUP    PIC X(14)   VALUE SPACE.
      *
      *    ジョブ管理ＤＢのキー値
           03  CONST-JOB-SHELLID           PIC X(08)   VALUE "ORCBS01".
           03  CONST-REQMODE-G.
               05  CONST-REQMODE-NYUKIN       PIC X(02)   VALUE "01".
               05  CONST-REQMODE-RRKSYU       PIC X(02)   VALUE "02".
               05  CONST-REQMODE-TORIKESHI    PIC X(02)   VALUE "03".
               05  CONST-REQMODE-HENKIN       PIC X(02)   VALUE "04".
               05  CONST-REQMODE-SAIKEISAN    PIC X(02)   VALUE "05".
               05  CONST-REQMODE-M-SAIKEISAN  PIC X(02)   VALUE "06".
               05  CONST-REQMODE-M-NYUKIN     PIC X(02)   VALUE "07".
               05  CONST-REQMODE-SKY-TORIKESHI
                                              PIC X(02)   VALUE "08".
      *
      *    収納明細マスターＷＫ
       01  WKSYUMEI-REC.
           COPY    "CPSYUMEI.INC"  REPLACING //SMEI-//
                                   BY        //WKSMEI-//.
      *
       01  TSYU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING //SYU-//
                                   BY        //TSYU-//.
      *
       01  TSMEI-REC.
           COPY    "CPSYUMEI.INC"  REPLACING //SMEI-//
                                   BY        //TSMEI-//.
      *
           COPY    "CPLNKS08.INC".
      *
           COPY    "CPSK1039.INC"  REPLACING //SYS-//
                                   BY        //TMP-//.
      *
           COPY    "S01COMMON-SPA" REPLACING //SPA-//
                                   BY        //TSPA-//.
      *
      **************************************************************
      *    ファイルレイアウト
      **************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *    帳票ＰＧ
           COPY    "CPSK1031.INC".
           COPY    "CPSK1032.INC".
           COPY    "CPSK1035.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK1041.INC".
           COPY    "CPSK3003.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納合計
       01  SYUTOTAL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    保険組合せ
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
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA02.INC".
           COPY    "CPSYUPARA03.INC".
      *
      **************************************************************
      *    サブプロ用　領域
      **************************************************************
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
           COPY    "CPORCHCN03.INC".
      *    明細書印刷パラメタ
           COPY    "CPORCHC04.INC".
           COPY    "CPORCHCN04.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    保険組み合せ取得
           COPY    "CPORCSHKNSET.INC".
      *
      *    負担金再計算用パラメタ
           COPY    "CPORCSSAIKEISAN.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    保険組合せ名称編集サブ
           COPY    "CPORCSHKNMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    患者番号取得サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    収納更新
           COPY   "CPORCSS016.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角・半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCHC03S02.INC".
      *
      *    入院請求書兼領収書まとめサブ
           COPY    "CPORCHCN03S02.INC".
      *
      *    未収金編集サブ
       01  LNKSYU-AREA.
           COPY    "CPSYUNOU.INC"      REPLACING //SYU-//
                                       BY        //LNKSYU-//.
           COPY    "CPORCSMISYU.INC".
      *
      *    収納明細編集サブ
           COPY    "CPSSUB02.INC".
       01  LNKSMEI-AREA.
         02  LNKSMEI-OCC           OCCURS    99.
           COPY "CPSYUMEI.INC"     REPLACING //SMEI-//
                                   BY        //LNKSMEI-//.
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    請求書・明細書編集サブ
           COPY    "CPSSUB03.INC".
      *
      *    再計算サブ
           COPY    "CPSSUB05.INC".
      *
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      **************************************************************
      *    連絡　領域
      **************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      **************************************************************
      *    主　　処理
      **************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE1       TO  SPA-S02
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
           WHEN        "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"             ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
              PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S02         TO  SPA-FREE1
      *
           .
           EXIT    PROGRAM
           .
      **************************************************************
      *    初期　処理
      **************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG      =   "SERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
               EVALUATE    SPA-SIDCD
               WHEN     "2000"     THRU "2099"
                   PERFORM 520-SIDSET-SEC
                   GO  TO  100-INIT-EXT
               WHEN     "2101"
                   PERFORM 520-SID2SET-SEC
                   GO  TO  100-INIT-EXT
               WHEN     "2102"
                   PERFORM 520-SID3SET-SEC
                   GO  TO  100-INIT-EXT
               END-EVALUATE
      *
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "S02"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *
      **************************************************************
      *    初期画面処理
      **************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他画面より
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
      *        元画面表示
      *
               IF    ( WRK-MOTOPG      =   "Y01"  OR "U01"  OR  "P97"
                                       OR  "XA01" OR "XB01" OR  "XC01"
                                       )
                   MOVE    LNK-COMMON  TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA  TO  SPA-S02
                   MOVE    SPA-WORK    TO  SPA-S01KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF    ( WRK-MOTOPG      =   "Y01"
                                       OR  "XA01"
                                       OR  "XB01"
                                       OR  "XC01" )
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF    ( WRK-MOTOPG(1:3) =   "U01" 
                                       OR  "P97" )
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF    ( SPA-SAKIPG      =   "S08" )
                   MOVE    SPA-WORK        TO  LNKS08-AREA
                   INITIALIZE                  SPA-S01KYOUTU
                   MOVE    SPA-NYUGAIKBN   TO  SPA-S08-NYUGAIKBN
                   MOVE    LNKS08-RRKNUM   TO  SPA-S08-RRKNUM
                   MOVE    SPA-MOTOPG      TO  SPA-S08-MOTOPG
                   MOVE    LNKS08-SAKIPG   TO  SPA-MOTOPG2
                   PERFORM 480-S08-SENI-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
      *
           END-IF
      *
      *
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "S08"
               IF    ( SPA-S08-MOTOPG  =   "S02" )
                   PERFORM 320-S03SPASET-SEC
               ELSE
                   PERFORM 210-BACK
               END-IF
           WHEN    "K02"
           WHEN    "K02N"
           WHEN    "KT01"
           WHEN    "U02"
           WHEN    "I01"
               PERFORM 320-SENIJI-SYOGMN-EDIT-SEC
           WHEN    "S03"
           WHEN    "S06"
               PERFORM 320-S03SPASET-SEC
           WHEN    "S07"
               PERFORM 320-S07SPASET-SEC
               EVALUATE    TRUE
               WHEN  ( SPA-S01-TUKI-SKYPRT =   "2" )
               WHEN  ( SPA-S01-TUKI-SRYPRT =   "2" )
                   PERFORM 520-SID4SET-SEC
               WHEN  ( SPA-S01-TUKI-SKYPRT =   "1" )
               WHEN  ( SPA-S01-TUKI-SRYPRT =   "1" )
                   PERFORM 4901-GAI-TUKISKYPRT-SEC
               END-EVALUATE
           WHEN    "S04"
      *        PERFORM 320-S03SPASET-SEC
               IF    ( SPA-S01-SHR-ORDER  NOT =   SPACE )
                   PERFORM 340-SHIHARAI-PRT-SEC
               ELSE
                   MOVE    SPACE           TO  SPA-S01-SHR-ORDER
               END-IF
               MOVE    SPACE           TO  SPA-S99-FLG
               MOVE    SPACE           TO  SPA-SIDCD
           WHEN    "S10"
               PERFORM 320-S03SPASET-SEC
               IF    ( SPA-S10-PTNUM   NOT =   SPACE )
                   PERFORM 3101-S10SPASET-SEC
               END-IF           WHEN    "SID1"
               PERFORM 330-SID1-SET-SEC
           WHEN    "SID2"
               PERFORM 330-SID2-SET-SEC
           WHEN    "SID3"
               PERFORM 330-SID3-SET-SEC
           WHEN    "SID4"
      *        PERFORM 320-S03SPASET-SEC
               PERFORM 330-SID4-SET-SEC
           WHEN    "S99"
               PERFORM 340-S99-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＳＩＤ１）ＯＫ処理
      *****************************************************************
       330-SID1-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-SIDCD
      *
           IF    ( SPA-SID1-FLG    =   "OK" )
               EVALUATE    SPA-SIDCD
               WHEN    "2001"
                   PERFORM 430-TORIKESI-KOUSHIN-SEC
               WHEN    "2002"
                   PERFORM 440-IKATUSAIKEI-SEC
               WHEN    "2003"
                   PERFORM 4101-TOROKU-SEC
               END-EVALUATE
      *
           END-IF
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    WRK-SIDCD       TO  SPA-SIDCD
           .
       330-SID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＳＩＤ２）ＯＫ処理
      *****************************************************************
       330-SID2-SET-SEC            SECTION.
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2101"
               EVALUATE    SPA-SID1-FLG
               WHEN    "OK"
                   PERFORM 4701-NYUDEL-SEC
               WHEN    "NO"
                   PERFORM 4701-TORIKESHI-SEC
               END-EVALUATE
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＳＩＤ３）ＯＫ処理
      *****************************************************************
       330-SID3-SET-SEC            SECTION.
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2102"
               EVALUATE    SPA-SID1-FLG
               WHEN    "O1"
                   MOVE    1       TO  FLG-SEIKYU-PRT
                   PERFORM 420-SAIHAKKOU-SEC
               WHEN    "O2"
                   MOVE    1       TO  FLG-MEISAI-PRT
                   PERFORM 420-SAIHAKKOU-SEC
               WHEN    "OK"
                   MOVE    1       TO  FLG-SEIKYU-PRT
                   MOVE    1       TO  FLG-MEISAI-PRT
                   PERFORM 420-SAIHAKKOU-SEC
               WHEN    "SID4"
                   INITIALIZE          SPA-TUKI-PARA-G
                   PERFORM 520-SID4SET-SEC
               END-EVALUATE
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID3-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＳＩＤ４）ＯＫ処理
      *****************************************************************
       330-SID4-SET-SEC            SECTION.
      *
           EVALUATE    SPA-SID1-FLG
           WHEN    "O1"
               MOVE    1       TO  FLG-SEIKYU-PRT
           WHEN    "O2"
               MOVE    1       TO  FLG-MEISAI-PRT
           WHEN    "OK"
                MOVE    1      TO  FLG-SEIKYU-PRT
                MOVE    1      TO  FLG-MEISAI-PRT
           END-EVALUATE
      *
           IF    ( FLG-SEIKYU-PRT  =   1 )
            OR   ( FLG-MEISAI-PRT  =   1 )
      *
               IF    ( FLG-SEIKYU-PRT  =   1 )
                   PERFORM 3301-BS01-EDIT-SEC
               END-IF
      *
               IF    ( SPA-S01-TUKI-SRYYM  =   SPACE )
                   PERFORM 421-SAIHAKKOU-SEC
               ELSE
                   PERFORM 422-GAI-TUKISKYPRT-SEC
               END-IF
           END-IF
      *
           MOVE    SPA-NAI-GYOUMUID    TO  WRK-GYOUMUID
           MOVE    SPA-NAI-UID         TO  WRK-UID
           MOVE    SPA-NAI-FILEMEI     TO  WRK-FILEMEI
           MOVE    "001%"              TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
      *
           .
       330-SID4-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求書用パラメタファイル編集処理
      *****************************************************************
       3301-BS01-EDIT-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  PRFNAME-HOSPNUM
           MOVE    "ORC23001"      TO  PRFNAME-FILE-ID
           MOVE    1               TO  PRFNAME-SYOKBN1
           MOVE    SPA-NAI-UID     TO  PRFNAME-TERMID
           INSPECT PRFNAME-TERMID  REPLACING ALL " " BY "_"
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    PRFNAME-BASENAME TO SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME TO ASGNPARA
           MOVE    SGETTEMP-ST       TO ASGNPARA-ST
      *
           OPEN    OUTPUT          BS01-FILE
      *
           MOVE    SPA-NAI-GYOUMUID
                                   TO  WRK-GYOUMUID
           MOVE    SPA-NAI-UID     TO  WRK-UID
           MOVE    SPA-NAI-FILEMEI TO  WRK-FILEMEI
           MOVE    "00102"         TO  WRK-FILEMEI-CD
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-REC    TO  BS01-REC
               WRITE   BS01-REC
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   BS01-FILE
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "orcbs02.sh"        TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-PTNUM       TO  SHELLTBL-ARG2
           MOVE    SPA-OPID            TO  SHELLTBL-ARG3
           MOVE    ASGNPARA(ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG4
           MOVE    SPA-NAI-FILEMEI-YMD TO  SHELLTBL-ARG5
           MOVE    SPA-NAI-FILEMEI-HMS TO  SHELLTBL-ARG6
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    ASGNPARA            TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBIMPORT-SEC
           MOVE    ORCA-BLOB-OBJECT    TO  SHELLTBL-ARG7
     *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       3301-BS01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-S99-SET-SEC             SECTION.
      *
           EVALUATE    SPA-S99-FLG
           WHEN    "OK"
               PERFORM 4501-NEXT-GMN-SEC
           WHEN    "DEL"
               PERFORM 490-JYOUHOU-DEL-SEC
           END-EVALUATE
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
      *
           .
       340-S99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC           SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"           TO  SPA-MOTOPG
           MOVE    "XC01"          TO  SPA-SAKIPG
      *
           MOVE    0               TO  WRK-STPAGE
           MOVE    0               TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                   TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO            TO  LNK-XC01-RENNUM
           MOVE    SPA-S02-TBL-GROUP
                                   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO            TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE      TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE      TO  LNK-XC01-ED-PAGE
           MOVE    "N23"           TO  LNK-XC01-MOTOPG
           MOVE    SPACE           TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA   TO  SPA-WORK
      *
           MOVE    SPACE           TO  LINKAREA
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-KYOUTU      TO  LNK-COMMON
           MOVE    SPA-S02         TO  LNK-SCRSPA
      *
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE   "XC01   "        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    受付、患者検索より
           IF    ( WRK-SPA-PTNUM       =   SPA-GMN-PTNUM )
               CONTINUE
           ELSE
               IF    ( WRK-SPA-PTNUM   NOT =   SPACE )
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  S02-PTNUM
                   PERFORM 4102-PTNUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-PTID    =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
               ELSE
                   MOVE    3           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者一覧画面より戻り時の処理
      *****************************************************************
       3101-S10SPASET-SEC              SECTION.
      *
           MOVE    SPA-S10-PTNUM   TO  S02-PTNUM
           PERFORM 4102-PTNUM-CHK-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    3                   TO  SPA-GMN-CUR
      *
           .
       3101-S10SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボ初期値設定処理
      *****************************************************************
       3101-COMBO-SYOKI-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    SPA-GMN-MODE        TO  WRK-CMB-CD
           PERFORM 4200-MODELST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-MODE-G
      *
           MOVE    SPA-GMN-SORTKBN     TO  WRK-CMB-CD
           PERFORM 4200-SORTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SORTKBN-G
      *
      *
           .
       3101-COMBO-SYOKI-SET-EXT.
           EXIT.
      **************************************************************
      *    画面編集処理
      **************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                   SPA-S01KYOUTU
      *
           PERFORM 311-SPA-SET-SEC
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    画面編集処理
      **************************************************************
       311-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                   SPA-S02
                                        SPA-S01KYOUTU-AREA
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
           MOVE    SYS-1039-TBL        TO  SPA-NAI-SYS1039TBL
      *
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-SRYYM
           MOVE    SPA-SYSYMD          TO  SPA-NAI-SRYYM
      *
           PERFORM 3101-COMBO-EDIT-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           MOVE    SPA-GMN-MODE    TO  S02-MODE
           MOVE    SPACE           TO  SPA-GMN-MODE
           MOVE    SPA-NAI-SRYYM   TO  SPA-NAI-THSRYYM
           PERFORM 4106-MODE-CHK-SEC
      *
           PERFORM 3111-S10-CHK-SEC
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       311-SPA-SET-EXT.
           EXIT.
      **************************************************************
      *    再計算該当患者チェック処理
      **************************************************************
       3111-S10-CHK-SEC                SECTION.
      *
           MOVE    SPA-NAI-SYS1039TBL  TO TMP-1039-TBL
      *
           IF    ( TMP-1039-SAIKEISANSKYCHK
                                           =   SPACE OR "1" )
            AND  ( FLG-S10-CHK             =   ZERO )
      *
               MOVE    SPACE               TO  SPA-NAI-S10-CHK
               IF    ( SPA-GMN-MODE        =  CONST-MODE-NORMON )
                 AND ( SPA-NAI-SRYYM   NOT =   SPACE )
                   PERFORM 900-SYUNOU-KEY86-SEL-SEC
                   IF    ( FLG-SYUNOU      =   ZERO )
                       MOVE    "1"         TO  SPA-NAI-S10-CHK
                   END-IF
               END-IF
      *
               MOVE    1               TO  FLG-S10-CHK
      *
           END-IF
      *
           .
       3111-S10-CHK-EXT.
           EXIT.
      **************************************************************
      *    コンボボックス編集処理
      **************************************************************
       3101-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-SRYKALST-SEC
      *
           PERFORM 3101-NYUGAIKBNLST-SEC
      *
           PERFORM 3101-MODELST-SEC
      *
           PERFORM 3101-SORTKBNLST-SEC
      *
           PERFORM 3101-NYKN-HOHOLST-SEC
      *
           PERFORM 3101-SKYPRTLST-SEC
      *
           PERFORM 3101-SRYPRTLST-SEC
      *
           .
       3101-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科ＳＰＡ画面編集処理
      *****************************************************************
       3101-SRYKALST-SEC              SECTION.
      *
           INITIALIZE  SPA-GMN-SRYKALST-G
                       SPA-NAI-SRYKALST-G
      *
           MOVE    CONST-ZENSRYKA-G    TO  SPA-GMN-SRYKALST (1)
           MOVE    1                   TO  SPA-GMN-SRYKA-MAX
      *
           INITIALIZE  SYS-1005-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    "%"                 TO  SYS-KBNCD
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-SRYYM       =   SPACE )
               MOVE    "99999999"      TO  SYS-STYUKYMD
               MOVE    "00000000"      TO  SYS-EDYUKYMD
           WHEN  ( SPA-NAI-SRYYM(5:)   =   SPACE )
               MOVE    SPA-NAI-SRYYM   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               MOVE    "1231"          TO  SYS-STYUKYMD (5 : 4)
               MOVE    "0101"          TO  SYS-EDYUKYMD (5 : 4)
           WHEN  OTHER
               MOVE    SPA-NAI-SRYYM   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               MOVE    "31"            TO  SYS-STYUKYMD (7 : 2)
               MOVE    "01"            TO  SYS-EDYUKYMD (7 : 2)
           END-EVALUATE
      *
           PERFORM 900-SYSKANRI-KEY21-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO    )
                    OR   ( SPA-GMN-SRYKA-MAX  >=   CONST-SRYKA-MAX )
      *
               COMPUTE SPA-GMN-SRYKA-MAX   =   SPA-GMN-SRYKA-MAX   +   1
               MOVE    SPA-GMN-SRYKA-MAX   TO  IDX-CMB
      *
               MOVE    SYS-1005-KBNCD (1:2)
                                       TO  SPA-GMN-SRYKAL    (IDX-CMB)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  SPA-GMN-SRYKAMEIL (IDX-CMB)
               MOVE    SYS-1005-SRYKANAME2
                                       TO  SPA-NAI-SRYKAMEIL (IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY21-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key21"                   TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SRYKALST-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分ＳＰＡ画面編集処理
      *****************************************************************
       3101-NYUGAIKBNLST-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-NYUGAIKBNLST-G
      *
           IF    ( SPA-BEDSU           >   ZERO )
               MOVE    "0"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "全部"          TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL       (2)
               MOVE    "入院"          TO  SPA-GMN-NYUGAIKBNMEIL    (2)
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (3)
               MOVE    "外来"          TO  SPA-GMN-NYUGAIKBNMEIL    (3)
               MOVE    3               TO  SPA-GMN-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "外来"          TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    1               TO  SPA-GMN-NYUGAIKBN-MAX
           END-IF
      *
           .
       3101-NYUGAIKBNLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面モードＳＰＡ画面編集処理
      *****************************************************************
       3101-MODELST-SEC                SECTION.
      *
           INITIALIZE                  SPA-GMN-MODELST-G
      *
           MOVE    "0"                 TO  SPA-GMN-MODEL       (1)
           MOVE    "収納一覧（月）"    TO  SPA-GMN-MODEMEIL    (1)
           MOVE    "1"                 TO  SPA-GMN-MODEL       (2)
           MOVE    "収納一覧（年）"    TO  SPA-GMN-MODEMEIL    (2)
           MOVE    "2"                 TO  SPA-GMN-MODEL       (3)
           MOVE    "収納一覧（全部）"  TO  SPA-GMN-MODEMEIL    (3)
           MOVE    "3"                 TO  SPA-GMN-MODEL       (4)
           MOVE    "未収一覧"          TO  SPA-GMN-MODEMEIL    (4)
           MOVE    "4"                 TO  SPA-GMN-MODEL       (5)
           MOVE    "入返金日"          TO  SPA-GMN-MODEMEIL    (5)
           MOVE    5                   TO  SPA-GMN-MODE-MAX
      *
           .
       3101-MODELST-EXT.
           EXIT.
      *****************************************************************
      *    並び順ＳＰＡ画面編集処理
      *****************************************************************
       3101-SORTKBNLST-SEC                SECTION.
      *
           INITIALIZE                  SPA-GMN-SORTKBNLST-G
      *
           IF    ( SPA-GMN-MODE        =   CONST-MODE-MISYU
                                       OR  CONST-MODE-NYUHEN )
               MOVE    "0"             TO  SPA-GMN-SORTKBNL       (1)
               MOVE    "発行日の古い順"
                                       TO  SPA-GMN-SORTKBNMEIL    (1)
               MOVE    "1"             TO  SPA-GMN-SORTKBNL       (2)
               MOVE    "発行日の新しい順"
                                       TO  SPA-GMN-SORTKBNMEIL    (2)
           ELSE
               MOVE    "0"             TO  SPA-GMN-SORTKBNL       (1)
               MOVE    "伝票番号の小さい順"
                                       TO  SPA-GMN-SORTKBNMEIL    (1)
               MOVE    "1"             TO  SPA-GMN-SORTKBNL       (2)
               MOVE    "伝票番号の大きい順"
                                       TO  SPA-GMN-SORTKBNMEIL    (2)
           END-IF
           MOVE    "2"                 TO  SPA-GMN-SORTKBNL       (3)
           MOVE    "診療日の古い順"    TO  SPA-GMN-SORTKBNMEIL    (3)
           MOVE    "3"                 TO  SPA-GMN-SORTKBNL       (4)
           MOVE    "診療日の新しい順"  TO  SPA-GMN-SORTKBNMEIL    (4)
           MOVE    4                   TO  SPA-GMN-SORTKBN-MAX
      *
           .
       3101-SORTKBNLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法コンボボックス編集処理
      *****************************************************************
       3101-NYKN-HOHOLST-SEC                SECTION.
      *
           INITIALIZE                      SPA-GMN-NYKN-HOHOLST-G
      *
      *    入金方法
           MOVE   ZERO                 TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1041-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1041-STYUKYMD
                                           SYS-1041-EDYUKYMD
           MOVE    SYS-1041-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1041-REC
      *
           MOVE    ZERO                TO  IDXH
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDXH           >=  50 )
      *
               COMPUTE IDXH    =   IDXH    +   1
      *
               MOVE    SYS-1041-KBNCD  TO  SPA-GMN-NYKN-HOHOL    (IDXH)
               MOVE    SYS-1041-NYKN-NAME
                                       TO  SPA-GMN-NYKN-HOHOMEIL (IDXH)
               MOVE    SYS-1041-NYKN-TANNAME
                                       TO  SPA-GMN-NYKN-HOHOMEI2L(IDXH)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1041-REC
      *
           END-PERFORM
      *
           MOVE    IDXH                TO  SPA-GMN-NYKN-HOHO-MAX
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-NYKN-HOHOLST-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行コンボ編集処理
      *****************************************************************
       3101-SKYPRTLST-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SKYPRTLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-SKYPRTL (1)
           MOVE    "発行しない"        TO  SPA-GMN-SKYPRTMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-SKYPRTL (2)
           MOVE    "発行する"          TO  SPA-GMN-SKYPRTMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-SKYPRT-MAX
      *
           .
       3101-SKYPRTLST-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行コンボ編集処理
      *****************************************************************
       3101-SRYPRTLST-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SRYPRTLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-SRYPRTL (1)
           MOVE    "発行しない"        TO  SPA-GMN-SRYPRTMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-SRYPRTL (2)
           MOVE    "発行する"          TO  SPA-GMN-SRYPRTMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-SRYPRT-MAX
      *
           .
       3101-SRYPRTLST-EXT.
           EXIT.
      *****************************************************************
      *    他の業務より遷移時初期処理
      *****************************************************************
       320-SENIJI-SYOGMN-EDIT-SEC      SECTION.
      *
           PERFORM 311-SPA-SET-SEC
      *
           IF  (   SPA-PTID        =  ZERO )
      *
               MOVE    2           TO  SPA-GMN-CUR
      *
           ELSE
      *
               PERFORM 320-PTINF-EDIT-SEC
               MOVE    1           TO  SPA-GMN-CUR
      *
           END-IF
      *
           .
       320-SENIJI-SYOGMN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       320-PTINF-EDIT-SEC              SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE                  SPA-GMN-HEAD
                                       SPA-NAI-HEAD
                                       SPA-GMN-MODE-G
                                       SPA-GMN-SORTKBN-G
                                       SPA-GMN-PAGE
                                       SPA-GMN-TBL
                                       SPA-GMN-MISYU
                                       SPA-GMN-TUKISKY
                                       SPA-GMN-INPUT
                                       SPA-NAI-TBL
      *
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           MOVE    SPA-PTID             TO  SPA-GMN-PTID
           MOVE    PTNUM-PTNUM          TO  SPA-GMN-PTNUM
      *
           PERFORM 320-PTINF-SET-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           MOVE    SPA-GMN-MODE    TO  S02-MODE
           MOVE    SPACE           TO  SPA-GMN-MODE
           MOVE    SPA-NAI-SRYYM   TO  SPA-NAI-THSRYYM
           PERFORM 4106-MODE-CHK-SEC
           IF    ( SPA-GMN-MAX     = ZERO )
               MOVE    3           TO  SPA-GMN-CUR
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           .
       320-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       320-PTINF-SET-SEC               SECTION.
      *
      *    患者マスター存在チェック
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF           =   ZERO )
      *
               MOVE    PTINF-NAME      TO  SPA-GMN-NAME
               MOVE    PTINF-KANANAME  TO  SPA-GMN-KANANAME
               MOVE    PTINF-SEX       TO  SPA-NAI-SEX
               EVALUATE    PTINF-SEX
               WHEN    "1" 
                   MOVE    "男"        TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"        TO  SPA-GMN-SEX
               END-EVALUATE
               MOVE    PTINF-BIRTHDAY  TO  SPA-NAI-BIRTHDAY
               MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-BIRTHDAY
      *
              IF    ( PTINF-NYUKIN-HOHO    =   SPACE )
                   MOVE    "01"                TO  SPA-NAI-NYUKIN-HOHO
               ELSE
                   MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NAI-NYUKIN-HOHO
               END-IF
      *
               MOVE    PTINF-SKYPRTFLG TO  SPA-NAI-SKYPRT
      *
      *        排他制御処理
               PERFORM 990-LOCK-IN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    1               TO  SPA-NAI-LOCK
                   MOVE    SLOCK-MSG       TO  SPA-NAI-SLOCK-MSG
                   MOVE    SPA-GMN-PTID    TO  SPA-LAST-PTID
                   MOVE    SPA-GMN-PTNUM   TO  SPA-LAST-PTNUM
               END-IF
      *
               PERFORM 320-HKNCOMBI-EDIT-SEC
      *
               PERFORM 3101-MISYU-EDIT-SEC
      *
               PERFORM 3101-TUKISKY-EDIT-SEC
      *
           ELSE
               MOVE    1               TO  FLG-PTINF
               MOVE    "0031"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPA-GMN-PTID    TO  SPA-LAST-PTID
               MOVE    SPA-GMN-PTNUM   TO  SPA-LAST-PTNUM
           END-IF
      *
           .
       320-PTINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       320-HKNCOMBI-EDIT-SEC           SECTION.
      *
           INITIALIZE                  SPA-NAI-HKNCOMBI-G
      *
           INITIALIZE                  ORCSHKNSETAREA
                                       WRK-SPA-AREA
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-SPA-BIRTHDAY
           MOVE    "2"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING
                                       WRK-SPA-AREA
                                       ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-NAI-HKNCOMBI-MAX
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( IDX-COMB    >   ORCSHKN-HKN-MAX )
                   OR    ( IDX-COMB    >   100 )
      *
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX-COMB)
                                   TO  SPA-NAI-HKNCOMBIL    (IDX-COMB)
               MOVE    ORCSHKN-NAI-HOJOKBN (IDX-COMB)
                                   TO  SPA-NAI-HOJOKBNL     (IDX-COMB)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX-COMB)
                                   TO  SPA-NAI-HKNCOMBIMEIL (IDX-COMB)
               MOVE    ORCSHKN-NAI-HKNNUM  (IDX-COMB)
                                   TO  SPA-NAI-HKNNUML      (IDX-COMB)
               MOVE    ORCSHKN-NAI-RSI-HKNKBN  (IDX-COMB)
                                   TO  SPA-NAI-RSI-HKNKBNL  (IDX-COMB)
      *
           END-PERFORM
      *
           .
       320-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求確認戻り画面編集処理
      *****************************************************************
       320-S03SPASET-SEC          SECTION.
      *
           INITIALIZE                  SPA-S02
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
           MOVE    SYS-1039-TBL        TO  SPA-NAI-SYS1039TBL
      *
           MOVE    SPA-S01-PTNUM       TO  SPA-GMN-PTNUM
           MOVE    SPA-S01-PTID        TO  SPA-GMN-PTID
           MOVE    SPA-S01-SEX         TO  SPA-GMN-SEX
           IF    ( SPA-GMN-SEX         =   "男" )
               MOVE    "1"             TO  SPA-NAI-SEX
           ELSE
               MOVE    "2"             TO  SPA-NAI-SEX
           END-IF
           MOVE    SPA-S01-SRYYMW      TO  SPA-GMN-SRYYM
           MOVE    SPA-S01-SRYYM       TO  SPA-NAI-SRYYM
           MOVE    SPA-S01-KANANAME    TO  SPA-GMN-KANANAME
           MOVE    SPA-S01-NAME        TO  SPA-GMN-NAME
           MOVE    SPA-S01-LOCK        TO  SPA-NAI-LOCK
           MOVE    SPA-S01-SLOCK-MSG   TO  SPA-NAI-SLOCK-MSG
           MOVE    SPA-S01-BIRTHDAYW   TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-S01-BIRTHDAY    TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-S01-NYUKIN-HOHO TO  SPA-NAI-NYUKIN-HOHO
           MOVE    SPA-S01-SKYPRT      TO  SPA-NAI-SKYPRT
           MOVE    SPA-S01-GYOUMUID    TO  SPA-NAI-GYOUMUID
           MOVE    SPA-S01-FILEMEI     TO  SPA-NAI-FILEMEI
           MOVE    SPA-S01-UID         TO  SPA-NAI-UID
           MOVE    SPA-S01-HSRYKA      TO  SPA-GMN-SRYKA
           MOVE    SPA-S01-HNYUGAIKBN  TO  SPA-GMN-NYUGAIKBN
           MOVE    SPA-S01-PAGE        TO  SPA-GMN-PAGE
           MOVE    SPA-S01-MODE        TO  SPA-GMN-MODE
           MOVE    SPA-S01-SORTKBN     TO  SPA-GMN-SORTKBN
      *
           PERFORM 320-HKNCOMBI-EDIT-SEC
      *
           PERFORM 3101-COMBO-EDIT-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
      *
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL         >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-TBL-SEL >   ZERO )
      *
               IF    ( SPA-S01-NYUGAIKBN
                                   =   SPA-NAI-TNYUGAIKBN (IDXL))
                AND  ( SPA-S01-DENPNUM
                                   =   SPA-GMN-TDENPNUM   (IDXL))
      *
                   MOVE    IDXL     TO  SPA-GMN-TBL-SEL
                                           SPA-GMN-SELBAN
      *
                   PERFORM 300-SYUMEI-HENSYU-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-MOTOPG2         =   "K02" OR "K02N" OR "KT01" )
           AND   ( SPA-PTNUM       NOT =   SPACE )
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-MOTOPG          =   "S04" )
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       320-S03SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    月別請求書兼領収書印刷指示画面戻り時編集処理
      *****************************************************************
       320-S07SPASET-SEC          SECTION.
      *
           MOVE    SPA-S01-TUKI-DENPNUM    TO  SPA-TUKI-DENPNUM
           MOVE    SPA-S01-TUKI-SRYYM      TO  SPA-TUKI-SRYYM
           MOVE    SPA-S01-TUKI-KIKANKBN   TO  SPA-TUKI-KIKANKBN
           MOVE    SPA-S01-TUKI-STYMD      TO  SPA-TUKI-STYMD
           MOVE    SPA-S01-TUKI-EDYMD      TO  SPA-TUKI-EDYMD
           MOVE    SPA-S01-TUKI-GRPKBN     TO  SPA-TUKI-GRPKBN
           MOVE    SPA-S01-TUKI-SKYPRT     TO  SPA-TUKI-SKYPRT
           MOVE    SPA-S01-TUKI-SRYPRT     TO  SPA-TUKI-SRYPRT
      *
           .
       320-S07SPASET-EXT.
           EXIT.
      **************************************************************
      *    収納マスタ編集処理
      **************************************************************
       300-SYUNOU-HENSYU-SEC                  SECTION.
      *
           MOVE    SPA-GMN-TBL-SEL     TO  WRK-TBL-SEL-MOT
      *
           MOVE    SPA-NAI-THSRYYM     TO  WRK-NAI-SRYYM
      *
           INITIALIZE                      SPA-GMN-TBL
                                           SPA-NAI-TBL
                                           SPA-GMN-TUKISKY
      *
           IF    ( SPA-GMN-MODE         =  CONST-MODE-NORMON
                                        OR CONST-MODE-NORYEAR
                                        OR CONST-MODE-NORALL
                                        OR CONST-MODE-NYUHEN )
               MOVE    SPA-NAI-SRYYM    TO SPA-NAI-THSRYYM
           ELSE
               MOVE    WRK-NAI-SRYYM    TO SPA-NAI-THSRYYM
           END-IF
      *
           PERFORM 3101-SYULIST-EDIT-SEC
      *
           PERFORM 3101-TUKISKY-EDIT-SEC
      *
           MOVE    WRK-TBL-SEL-MOT      TO  SPA-GMN-TBL-SEL
      *
           IF    ( WRK-ORG-SYORINO      =   SPACE )
               PERFORM 300-SYUMEI-HENSYU-SEC
           END-IF
      *
           PERFORM 3111-S10-CHK-SEC
      *
           .
       300-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    収納明細編集処理
      **************************************************************
       300-SYUMEI-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SMEILIST-AREA
                                       SPA-NAI-SMEILIST-AREA
                                       SPA-GMN-SYUMEI-AREA
                                       SPA-NAI-SYUMEI-AREA
      *
           IF    ( FLG-MEILIST-ROW     =   ZERO )
               MOVE    1               TO  FLG-MEILIST-ROW
           END-IF
      *
           IF    ( SPA-GMN-TBL-SEL     >   ZERO )
               PERFORM 3001-SYUMEI-HENSYU-SEC
           END-IF
      *
           .
       300-SYUMEI-HENSYU-EXT.
           EXIT.
      **************************************************************
      *    収納明細編集処理
      **************************************************************
       3001-SYUMEI-HENSYU-SEC          SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-TBL-SEL)
                                       TO  WRK-SPA-NYUGAIKBN
           INITIALIZE                      SSUB02-AREA
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-TBL-SEL)
                                       TO  SSUB02-DENPNUM
           CALL    "ORCGSSUB02"            USING
                                           WRK-SPA-AREA
                                           SSUB02-AREA
                                           LNKSMEI-AREA
      *
           MOVE    SSUB02-MAX          TO  SPA-GMN-SMEILIST-MAX
           PERFORM VARYING IDXM    FROM    1   BY  1
                   UNTIL ( IDXM    >   SSUB02-MAX )
      *
               MOVE    LNKSMEI-OCC (IDXM)
                                       TO  SYUMEI-REC
      *
               MOVE    SMEI-DENPEDANUM TO  SPA-GMN-TMDENPEDANUM(IDXM)
               MOVE    SMEI-SYURRKNUM  TO  SPA-NAI-TMSYURRKNUM(IDXM)
               MOVE    SMEI-SYUEDANUM  TO  SPA-NAI-TMSYUEDANUM(IDXM)
               MOVE    SMEI-SYURRKUPDKBN
                                       TO  SPA-NAI-TMSYURRKUPDKBN(IDXM)
               MOVE    SMEI-SKYMONEY   TO  SPA-GMN-TMSKYMONEY (IDXM)
               MOVE    SMEI-NYUHEN-MONEY
                                       TO  SPA-GMN-TMNYUKIN (IDXM)
               MOVE    SMEI-JOUTAIKBN  TO  SPA-NAI-TMJOUTAIKBN (IDXM)
               MOVE    SMEI-MEISAIJOUTAIKBN
                                       TO  SPA-NAI-TMMEIJOUTAIKBN (IDXM)
               MOVE    SSUB02-MINUS (IDXM)
                                       TO  SPA-NAI-TMMINUS (IDXM)
               IF    ( SMEI-MEISAIJOUTAIKBN    =   "1" )
                   STRING  SSUB02-JOUTAI (IDXM)    DELIMITED BY SPACE
                           "*"                     DELIMITED BY SIZE
                   INTO    SPA-GMN-TMJOUTAI (IDXM)
                   END-STRING
               ELSE
                   MOVE    SSUB02-JOUTAI (IDXM)
                                       TO  SPA-GMN-TMJOUTAI (IDXM)
               END-IF
      *
               MOVE    SMEI-NYUHEN-YMD TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TMSYORIYMD (IDXM)
               MOVE    SMEI-NYUHEN-YMD TO  SPA-NAI-TMSYORIYMD (IDXM)
      *
               MOVE    SMEI-NYUKIN-HOHO
                                       TO  WRK-CMB-CD
               PERFORM 4200-NYKN-HOHOLST-CHK-SEC
               MOVE    WRK-CMB-ITM2    TO  SPA-GMN-TMNYKN-HOHO (IDXM)
      *
               MOVE    SMEI-NYUKIN-HOHO
                                       TO  SPA-NAI-TMNYKN-HOHO (IDXM)
      *
               MOVE    SSUB02-PROC-FLG (IDXM)
                                       TO  SPA-NAI-TMPROC-FLG (IDXM)
      *
               MOVE    SSUB02-CHOSEI1 (IDXM)
                                       TO  SPA-NAI-TMCHOSEI1(IDXM)
               MOVE    SSUB02-CHOSEI2 (IDXM)
                                       TO  SPA-NAI-TMCHOSEI2(IDXM)
      *
           END-PERFORM
      *
           PERFORM 410011-INPUT-AREA-HEN-SEC
      *
           IF    ( SPA-GMN-TSEIKYU(SPA-GMN-TBL-SEL)
                               >   SPA-GMN-TNYUKIN(SPA-GMN-TBL-SEL))
               PERFORM 4601-NYUKIN-SEC
           END-IF
      *
           .
       3001-SYUMEI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷処理
      *****************************************************************
       340-SHIHARAI-PRT-SEC            SECTION.
      *
      *----(03.04.01)--ADD-START---
           INITIALIZE                  SPA-S02-TBL-GROUP
                                       SPA-S02-JOB-JOBID
      *----(03.04.01)--ADD-START---
      *
      *----(03.04.01)--UPD-START---
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC        TO  SYS-3003-REC
               MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1006"          TO  SPA-ERRCD
                   GO  TO  340-SHIHARAI-PRT-EXT
               END-IF
           END-IF
      *
      *    ジョブ管理削除処理
           MOVE    "ALL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-JOB-SHELLID
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1007"          TO  SPA-ERRCD
               GO  TO  340-SHIHARAI-PRT-EXT
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    ZERO                TO  CNT-JOBID
                                           CNT-PRTKANRI-RENNUM
           MOVE    SPACE               TO  CONST-PRTKANRI-TBL-GROUP
      *
           MOVE    SMCNDATE-YMD    TO  CONST-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS    TO  CONST-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "OR023ERR"          TO  ERRFLPARA-FILE-ID
           MOVE    1                   TO  ERRFLPARA-SYOKBN1
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC    NOT =   ZERO )
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
      *    帳票発行プログラム名取得
           PERFORM 3401-PGNAME-SHIHARAI-SEC
      *
           PERFORM VARYING IDX2 FROM   1   BY  1
                   UNTIL ( IDX2 >      3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               IF      SPA-S01-SHR-PRTFLG(IDX2)  =   "1"
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
           END-PERFORM
      *
      *    印刷処理
           IF     ( SPA-S01-SHR-ORDER  =  "OK"  OR "PV" )
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    CONST-PRTKANRI-TBL-GROUP
                                           TO  SPA-S02-TBL-GROUP
               MOVE    CNT-JOBID           TO  SPA-S02-JOB-JOBID
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *
               MOVE    SPACE               TO  SHELLTBL
               MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    CONST-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY
               MOVE    CONST-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    "000000"            TO  LNK-PRTKANRI-SRYYM
               MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
               MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4
               MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
               MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
               MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7
               MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8
               MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
               MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
               MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
      *
               MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
               MOVE    CNT-JOBID           TO  SHELLTBL-ARG13
               MOVE    CONST-JOB-SHELLID   TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-S01-SHR-ORDER
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
               WHEN    "OK"
                   EVALUATE    TRUE
                   WHEN  ( SPA-CLIENT-PRT-FLG =   2  )
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   WHEN  ( SPA-PRTCONF        =   "1"  )
                       MOVE    "6"         TO  SHELLTBL-ARG17
                   WHEN    OTHER
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-EVALUATE
               END-EVALUATE
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *----(03.04.01)--UPD-END-----
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
           PERFORM 490-KAKUNIN-SEC
      *
           .
       340-SHIHARAI-PRT-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書プログラム名取得処理
      *****************************************************************
       3401-PGNAME-SHIHARAI-SEC        SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                      WRK-PGNAME-G
      *
      *    支払証明書（日別）
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000006"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-PGNAME (1)
           ELSE
              MOVE    "ORCHCM31"       TO  WRK-PGNAME (1)
           END-IF
      *
           MOVE       "HC31"           TO  WRK-PGTYPE (1)
      *
      *    支払証明書（月別）
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000007"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-PGNAME (2)
           ELSE
              MOVE    "ORCHCM32"       TO  WRK-PGNAME (2)
           END-IF
      *
           MOVE       "HC32"           TO  WRK-PGTYPE (2)
      *
      *----(03.04.01)--ADD-START---
           MOVE    "ORCHCM50"          TO  WRK-PGNAME (3)
      *
           MOVE       "HC50"           TO  WRK-PGTYPE (3)
      *----(03.04.01)--ADD-START---
           .
      *
       3401-PGNAME-SHIHARAI-EXT.
           EXIT.
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4901-PRINT-SYORI-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *    印刷用スクリプト処理
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  CNT-PRTKANRI-RENNUM
           MOVE    CNT-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM
           MOVE    CONST-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-SYSYMD(1:6) TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "orcbs01.sh"    TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           EVALUATE    IDX2
               WHEN    1
                   MOVE    SYS-1031-PRTNM (9)
                                       TO  LNK-PRTKANRI-PRTNM
               WHEN    2
                   MOVE    SYS-1031-PRTNM (9)
                                       TO  LNK-PRTKANRI-PRTNM
                   IF    ( SYS-1031-PRTNM (12) NOT =   SPACE )
                       MOVE    SYS-1031-PRTNM (12)
                                       TO  LNK-PRTKANRI-PRTNM
                   END-IF
               WHEN    3
                   MOVE    SYS-1031-PRTNM (15)
                                       TO  LNK-PRTKANRI-PRTNM
           END-EVALUATE
      *
           MOVE    SPACE           TO  SHELLTBL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "orcbs01.sh"    TO  SHELLTBL-NAME
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11
      *
           MOVE    CNT-JOBID       TO  SHELLTBL-ARG12
           MOVE    CONST-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG14
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG15
           MOVE    WRK-PGTYPE (IDX2)
                                   TO  SHELLTBL-ARG16
           MOVE    WRK-PGNAME (IDX2)
                                   TO  SHELLTBL-ARG17
           MOVE    SPA-STAFFCD
                                   TO  SHELLTBL-ARG18
           MOVE    SPA-S01-SHR-STYMD
                                   TO  SHELLTBL-ARG19
           MOVE    SPA-S01-SHR-EDYMD
                                   TO  SHELLTBL-ARG20
           MOVE    SPA-S01-SHR-KIKANKBN
                                   TO  SHELLTBL-ARG21
           MOVE    SPA-S01-SHR-SYORIKBN
                                   TO  SHELLTBL-ARG22
           MOVE    SPA-S01-SHR-DSPKBN
                                   TO  SHELLTBL-ARG23
           MOVE    SPA-GMN-PTID
                                   TO  SHELLTBL-ARG24
           MOVE    SPA-S01-SHR-NYUGAIKBN
                                   TO  SHELLTBL-ARG25
           MOVE    SPA-S01-SHR-JIHSUMKBN
                                   TO  SHELLTBL-ARG26
           MOVE    SPA-S01-SHR-SUMKBN
                                   TO  SHELLTBL-ARG27
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
           .
      *
       4501-PRINT-EXT.
           EXIT.
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           COMPUTE CNT-JOBID   =   CNT-JOBID   +   1
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-JOBID       TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-JOBID       TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    CNT-JOBID           TO  SYS-3003-JOBID
           MOVE    SPA-OPID            TO  SYS-3003-OPID
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF    ( FLG-SYSKANRI        =   ZERO )
               PERFORM 910-DBUPDATE-SEC
           ELSE
               PERFORM 910-DBINSERT-SEC
           END-IF
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      **************************************************************
      *   収納リスト編集処理
      **************************************************************
       3101-SYULIST-EDIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
      *
           EVALUATE    SPA-GMN-MODE
           WHEN    CONST-MODE-MISYU
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key93"         TO  WRK-PATHNAME
           WHEN    CONST-MODE-NYUHEN
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key94"         TO  WRK-PATHNAME
           WHEN    OTHER
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key95"         TO  WRK-PATHNAME
           END-EVALUATE
      *
           MOVE    CONST-SYULST-MAX    TO  WRK-MAX
           MOVE    SPA-NAI-SYS1039TBL  TO  SYS-1039-TBL
           IF    ( SYS-1039-S02LIMIT   IS  NUMERIC )
            AND  ( SYS-1039-S02LIMIT   >   ZERO )
            AND  ( SPA-GMN-MODE        =   CONST-MODE-NORALL )
               MOVE    SYS-1039-S02LIMIT
                                       TO  WRK-MAX
           END-IF
           PERFORM 900-SYUNOU-SEL-SEC
           IF    ( FLG-SYUNOU  =   ZERO )
               MOVE    1               TO  SPA-GMN-SELECTED
           END-IF
           IF    ( SPA-NAI-LOCK    NOT =   ZERO )
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( SPA-GMN-MAX    >=   WRK-MAX )
      *
               PERFORM 3101-SYULST-SEC
      *
               PERFORM 900-SYUNOU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          IF    ( FLG-SYUNOU           =   ZERO )
              MOVE     1               TO  SPA-GMN-NEXT
          END-IF
      *
           .
       3101-SYULIST-EDIT-EXT.
           EXIT.
      **************************************************************
      *   未収金額編集処理
      **************************************************************
       3101-MISYU-EDIT-SEC             SECTION.
      *
           INITIALIZE  SPA-GMN-MISYU
      *
      *    入院分未収金額集計
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  SPA-GMN-NYU-MISYU
      *
      *    外来分未収金額集計
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  SPA-GMN-GAI-MISYU
      *
           .
       3101-MISYU-EDIT-EXT.
           EXIT.
      **************************************************************
      *   未収金集計処理
      **************************************************************
       3101-MISYU-SUM-SEC              SECTION.
      *
           INITIALIZE                      WRK-MISYU-G
      *
           PERFORM 900-SYUNOU-KEY53-SEL-SEC
      *
           COMPUTE WRK-MISYU-P
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           PERFORM 900-SYUNOU-KEY54-SEL-SEC
      *
           COMPUTE WRK-MISYU-M
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           COMPUTE WRK-MISYU
               =   WRK-MISYU-P
               +   WRK-MISYU-M
      *
           .
       3101-MISYU-SUM-EXT.
           EXIT.
      **************************************************************
      *   診療年月請求額編集処理
      **************************************************************
       3101-TUKISKY-EDIT-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-TUKISKY
      *
           PERFORM 900-SYUNOU-KEY61-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU   NOT =   ZERO )
      *
               IF    ( SYU-NYUGAIKBN       =   "1" )
                   COMPUTE SPA-GMN-NYU-TUKISKY
                       =   SPA-GMN-NYU-TUKISKY +   SYU-SKYMONEY
               ELSE
                   COMPUTE SPA-GMN-GAI-TUKISKY
                       =   SPA-GMN-GAI-TUKISKY +   SYU-SKYMONEY
               END-IF
      *
               PERFORM 900-SYUNOU-KEY61-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key61"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-TUKISKY-SUM-EXT.
           EXIT.
      **************************************************************
      *   収納マスターより画面内容編集処理
      **************************************************************
       3101-SYULST-SEC                SECTION.
      *
      *    収納マスタ内容編集
           COMPUTE SPA-GMN-MAX =   SPA-GMN-MAX +   1
      *
           MOVE    SPA-GMN-MAX         TO  IDXL
      *
           MOVE    IDXL             TO  SPA-GMN-TBANGO   (IDXL)
      *
           MOVE    SYU-DENPNUM         TO  SPA-GMN-TDENPNUM (IDXL)
      *
           MOVE    SYU-GRP-DENPNUM     TO  SPA-NAI-TGRP-DENPNUM (IDXL)
           IF    ( SYU-GRP-DENPNUM     NOT =  ZERO )
               MOVE   "M"              TO  SPA-GMN-DENPGRP  (IDXL)
           END-IF
      *
      *    入外区分
           MOVE    SYU-NYUGAIKBN       TO  SPA-NAI-TNYUGAIKBN  (IDXL)
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    "入"            TO  SPA-GMN-TNYUGAIKBN  (IDXL)
           WHEN    "2"
               MOVE    "外"            TO  SPA-GMN-TNYUGAIKBN  (IDXL)
           END-EVALUATE
      *
           IF      SYU-DENPJTIKBN  NOT =   "3"
                IF  (SPA-NAI-SRYYM(1:6)  IS  NUMERIC     )
      **********OR  (SYU-SRYYMD(1:6)  =   SPA-SYSYMD(1:6))
                 IF  SYU-NYUGAIKBN  =  "2"
                  EVALUATE  SYU-SECMONEY
                    WHEN      4
                    WHEN      6
                       MOVE      "1"   TO  SPA-GMN-SIKIBETUFLG(IDXL)
                    WHEN      1
                    WHEN      3
                       MOVE      "2"   TO  SPA-GMN-SIKIBETUFLG(IDXL)
                    WHEN      2
                       MOVE      "3"   TO  SPA-GMN-SIKIBETUFLG(IDXL)
                    WHEN      5
                       MOVE      "4"   TO  SPA-GMN-SIKIBETUFLG(IDXL)
                    WHEN      7
                       MOVE      "6"   TO  SPA-GMN-SIKIBETUFLG(IDXL)
                  END-EVALUATE
                 END-IF
                END-IF
                IF  SYU-ACCT-UPDKBN NOT = SPACE
                   MOVE   "☆"    TO   SPA-GMN-TNYUGAIKBN(IDXL)(3:2)
                END-IF
           END-IF
      *
           MOVE    SYU-BUNBENINFFLG     TO SPA-NAI-TBUNBEN (IDXL)
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"               TO SYS-KANRICD
           MOVE    SYU-SRYKA            TO SYS-KBNCD
           MOVE    SYU-SRYYMD           TO SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *
           MOVE    SYSKANRI-REC         TO SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME2  TO SPA-GMN-TSRYKA (IDXL)
           MOVE    SYU-SRYKA            TO SPA-NAI-TSRYKA (IDXL)
           MOVE    SYS-1005-SRYKANAME1  TO SPA-NAI-TSRYKAMEI (IDXL)
           MOVE    SYS-1005-RECESRYKA   TO SPA-NAI-TRECESRYKA (IDXL)
      *
      *    診療日
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    SYU-SKYSTYMD    TO  SPA-NAI-TSRYYMD   (IDXL)
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD   (IDXL)
               MOVE    SYU-SKYEDYMD    TO  SPA-NAI-TSKYEDYMD (IDXL)
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    "-"         TO  SPA-GMN-TSRYYMD (IDXL)(10:1)
               MOVE    WRK-HENYMDG (5:5)
                                   TO  SPA-GMN-TSRYYMD (IDXL) (11:5)
           WHEN    "2" 
               MOVE    SYU-SRYYMD      TO  SPA-NAI-TSRYYMD (IDXL)
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD (IDXL)
           END-EVALUATE
      *
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  SPA-NAI-THAKYMD (IDXL)
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-THAKYMD (IDXL)
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SPA-GMN-TSEIKYU (IDXL)
      *    入金額
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-GMN-TNYUKIN (IDXL)
      *    未収金額
           COMPUTE SPA-GMN-TMISYU(IDXL)
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
      *    保険組合せより（保険と負担）
           MOVE    SYU-HKNCOMBINUM     TO  WRK-KUMINO
           PERFORM 510-HKNKUMI-MEI-SEC
           MOVE    WRK-HKNKUMI         TO  SPA-GMN-THKNCOMBI (IDXL)
           MOVE    WRK-HKNNUM          TO  SPA-NAI-THKNNUM   (IDXL)
           MOVE    WRK-HOJOKBN         TO  SPA-NAI-THOJOKBN  (IDXL)
           MOVE    WRK-RSI-HKNKBN      TO  SPA-NAI-TRSI-HKNKBN(IDXL)
      *
      *    患者負担率
           IF    ( SYU-PTFTNRATE       =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-TFTNRATE (IDXL)
           ELSE
               MOVE    SPACE           TO  WK-GMN-RITU
               MOVE    SYU-PTFTNRATE   TO  WK-GMN-FTNRITU
               MOVE    "%"             TO  WK-GMN-PA
               MOVE    WK-GMN-RITU     TO  SPA-GMN-TFTNRATE (IDXL)
           END-IF
      *
           MOVE    SYU-DENPJTIKBN      TO  SPA-NAI-TJYOTAIKBN (IDXL)
      *
      *    伝票状態区分
           MOVE    SPACE               TO  WRK-JYOTAI
           EVALUATE   SYU-DENPJTIKBN   ALSO   SYU-CREATEKBN
           WHEN   SPACE                ALSO   SPACE
               MOVE  "請求額なし"      TO  WRK-JYOTAI
           WHEN   "1"                  ALSO   SPACE
               MOVE  "入金済"          TO  WRK-JYOTAI
           WHEN   "2"                  ALSO   SPACE
               MOVE  "未入金"          TO  WRK-JYOTAI
           WHEN   "3"                  ALSO   SPACE
               MOVE  "請求取消し"      TO  WRK-JYOTAI
           WHEN   "7"                  ALSO   SPACE
               MOVE  "継続"            TO  WRK-JYOTAI
           WHEN   SPACE                ALSO   "1"
               MOVE  "請求額なし（退）"
                                       TO  WRK-JYOTAI
           WHEN   "1"                  ALSO   "1"
               MOVE  "入金済（退）"
                                       TO  WRK-JYOTAI
           WHEN   "2"                  ALSO   "1"
               MOVE  "未入金（退）"
                                       TO  WRK-JYOTAI
           WHEN   "3"                  ALSO   "1"
               MOVE  "請求取消し（退）"
                                       TO  WRK-JYOTAI
           WHEN   "7"                  ALSO   "1"
               MOVE  "継続（退）"      TO  WRK-JYOTAI
           WHEN   SPACE                ALSO   "2"
               MOVE  "請求額なし（定）"
                                       TO  WRK-JYOTAI
           WHEN   "1"                  ALSO   "2"
               MOVE  "入金済（定）"    TO  WRK-JYOTAI
           WHEN   "2"                  ALSO   "2"
               MOVE  "未入金（定）"    TO  WRK-JYOTAI
           WHEN   "3"                  ALSO   "2"
               MOVE  "請求取消し（定）"
                                       TO  WRK-JYOTAI
           WHEN   "7"                  ALSO   "2"
               MOVE  "継続（定）"      TO  WRK-JYOTAI
           END-EVALUATE
      *
           MOVE    WRK-JYOTAI      TO  SPA-GMN-TJYOTAI (IDXL)
      *
           .
       3101-SYULST-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-START---
      *************************************************************
      *    請求確認画面遷移　処理
      *************************************************************
       490-KAKUTEI-SEC                 SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      490-KAKUTEI-EXT
           END-IF
      *
      *****MOVE    SPA-GMN-THKNCOMBI(SPA-GMN-SELBAN) TO
      *****                            SPA-HKNCOMBIMEI
      *****MOVE    SPA-GMN-TSRYKAMEI(SPA-GMN-SELBAN) TO
      *****                            SPA-SRYKAMEI
           MOVE    SPA-GMN-TDENPNUM(SPA-GMN-SELBAN)  TO
                                       SPA-S01-DENPNUM
      *
           INITIALIZE                  SPA-S01KYOUTU-AREA
           MOVE    "09"                TO  SPA-S01-SYORINO
      *
           MOVE    "S03"           TO  SPA-NAI-GYOUMUID
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID           TO  SPA-NAI-UID
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    "S02"           TO  SPA-MOTOPG
      *
           MOVE    "S03"           TO  SPA-SAKIPG
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
           MOVE   "S03    "        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      S99
           MOVE    ZERO                TO  S99-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  S99-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3003-REC 
               PERFORM VARYING IDX2 FROM   1   BY  1
                       UNTIL ( IDX2 >      SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX2            TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                           TO  JOB-SHELLID
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       MOVE    JOB-JOBID       TO  S99-RENNUM   (IDX2)
                       MOVE    JOB-SHELLMSG    TO  S99-SHORINM  (IDX2)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  S99-STARTTIME(IDX2)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  S99-ENDTIME  (IDX2)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  S99-PAGE     (IDX2)
                       MOVE    JOB-YOBI        TO  S99-ERRMSG   (IDX2)
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  S99-MSG
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE )
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE
                                                   INTO  S99-MSG
                               END-STRING
                           END-IF
                       END-IF
                       MOVE    IDX2             TO  S99-COUNT
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( S99-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( S99-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                       TO  S99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                       TO  S99-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-S01-SHR-ORDER   =   "PV" )
               MOVE    "F12:プレビュー"    TO  S99-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                   MOVE    WIDGET-NORMAL   TO  S99-B12-STATE
                   MOVE    "B12"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "S99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "S99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書・診療費明細書発行処理
      *****************************************************************
       490-PRINT-SEC                   SECTION.
      *
           INITIALIZE                      TSMEI-REC
      *
           IF    ( SPA-GMN-SMEILIST-SEL    =   ZERO )
               IF    ( WRK-SKYPRT          =   "1" )
                   MOVE    13          TO  SPA-GMN-CUR
                   MOVE    "3006"      TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPA-GMN-TMDENPEDANUM (SPA-GMN-SMEILIST-SEL)
                                       TO  TSMEI-DENPEDANUM
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               INITIALIZE                  SPA-S01KYOUTU-AREA
               PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
      *        印刷制御情報取得サブ
               CALL    "ORCSONPRTSET"  USING   SPA-AREA
               IF    ( WRK-SKYPRT      =   "1" )
                 OR  ( WRK-SRYPRT      =   "1" )
                   IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                    OR   ( SPA-PRTCONF         =   "1" )
                       PERFORM 800-CLIENT-PRT-SEC
                   END-IF
               END-IF
      *
               MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
               INITIALIZE                      SSUB03-AREA
               MOVE    2                   TO  SSUB03-REQNO
               MOVE    TSMEI-DENPEDANUM    TO  SSUB03-DENPEDANUM
               MOVE    WRK-SKYPRT          TO  SSUB03-SKYPRT
               MOVE    WRK-SRYPRT          TO  SSUB03-SRYPRT
               CALL    "ORCGSSUB03"        USING
                                           WRK-SPA-AREA
                                           SPA-S01KYOUTU
                                           SSUB03-AREA
      *
               PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
           END-IF
      *
           .
       490-PRINT-EXT.
           EXIT.
      *----(03.04.01)--UPD-END-----
      *****************************************************************
      *    再印刷処理
      *****************************************************************
       450-REPRINT-SEC                 SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       450-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    情報削除
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-JOB-SHELLID   TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       490-JYOUHOU-DEL-EXT.
           EXIT.
      **************************************************************
      *    自画面編集処理
      **************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           EVALUATE    SPA-SIDCD
           WHEN     "2000"     THRU "2099"
               PERFORM 520-SIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           WHEN     "2101"
               PERFORM 520-SID2SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           WHEN     "2102"
               PERFORM 520-SID3SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "S02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      **************************************************************
      *    画面編集処理
      **************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    S02-SYULIST-ROW     TO  WRK-SYULIST-ROW
           MOVE    S02-MEILIST-ROW     TO  WRK-MEILIST-ROW
      *
           MOVE    SPACE               TO  S02
           INITIALIZE                      S02
      *
           EVALUATE    FLG-SYULIST-ROW
           WHEN    1
               MOVE    ZERO            TO  S02-SYULIST-ROW
               MOVE    ZERO            TO  S02-SYULIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-SYULIST-ROW TO  S02-SYULIST-ROW
               MOVE    ZERO            TO  S02-SYULIST-ROWATTR
           END-EVALUATE
      *
           EVALUATE    FLG-MEILIST-ROW
           WHEN    1
               MOVE    ZERO            TO  S02-MEILIST-ROW
               MOVE    ZERO            TO  S02-MEILIST-ROWATTR
           WHEN    2
               MOVE    SPA-GMN-SMEILIST-MAX
                                       TO  S02-MEILIST-ROW
               MOVE    1               TO  S02-MEILIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-MEILIST-ROW TO  S02-MEILIST-ROW
               MOVE    ZERO            TO  S02-MEILIST-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-PTNUM       TO  S02-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  S02-KANANAME
           MOVE    SPA-GMN-NAME        TO  S02-NAME
           MOVE    SPA-GMN-SEX         TO  S02-SEX
           MOVE    SPA-GMN-SRYYM      TO  S02-SRYYM
           MOVE    SPA-GMN-BIRTHDAY    TO  S02-BIRTHDAY
           MOVE    SPA-GMN-SRYKA-G     TO  S02-SRYKA
           MOVE    SPA-GMN-NYUGAIKBN-G TO  S02-NYUGAIKBN
           MOVE    SPA-GMN-MODE-G      TO  S02-MODE
           MOVE    SPA-GMN-SORTKBN-G   TO  S02-SORTKBN
      *
           MOVE    SPA-GMN-SRYKA-MAX   TO  S02-SRYKA-COUNT
           PERFORM VARYING IDX-CMB    FROM    1   BY  1
                   UNTIL ( IDX-CMB    >   SPA-GMN-SRYKA-MAX )
               MOVE    SPA-GMN-SRYKALST (IDX-CMB)
                                       TO  S02-SRYKALST(IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYUGAIKBN-MAX   TO  S02-NYUGAIKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM   1   BY  1
                   UNTIL ( IDX-CMB     >      SPA-GMN-NYUGAIKBN-MAX )
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX-CMB)
                                       TO  S02-NYUGAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-MODE-MAX   TO  S02-MODE-COUNT
           PERFORM VARYING IDX-CMB     FROM   1   BY  1
                   UNTIL ( IDX-CMB     >      SPA-GMN-MODE-MAX )
               MOVE    SPA-GMN-MODELST (IDX-CMB)
                                       TO  S02-MODE-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SORTKBN-MAX TO  S02-SORTKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM   1   BY  1
                   UNTIL ( IDX-CMB     >      SPA-GMN-SORTKBN-MAX )
               MOVE    SPA-GMN-SORTKBNLST (IDX-CMB)
                                       TO  S02-SORTKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    WIDGET-NORMAL       TO  S02-B04-STATE
           MOVE    WIDGET-INSENSITIVE  TO  S02-B03S-STATE
      *
           MOVE    "請求取消"          TO  S02-B03S-LABEL
      *
           INITIALIZE          WRK-CHECKFLG-TABLE
      *
           PERFORM VARYING     IDXL     FROM    1   BY  1
                   UNTIL     ( IDXL     >       SPA-GMN-MAX )
               MOVE    SPA-GMN-TBANGO (IDXL)
                                       TO  WRK-TBANGO-W
               MOVE    WRK-TBANGO-W    TO  S02-TNUM       (IDXL)
               MOVE    SPA-GMN-TDENPNUM(IDXL)
                                       TO  S02-TDENPNUM   (IDXL)
               MOVE    SPA-GMN-DENPGRP(IDXL)
                                       TO  S02-TDENPNUM   (IDXL)(9:1)
               IF    ( SPA-NAI-TBUNBEN(IDXL)    =   "1" )
                   MOVE    "S"     TO  S02-TDENPNUM   (IDXL)(10:1)
               END-IF
               MOVE    SPA-GMN-TNYUGAIKBN(IDXL)
                                       TO  S02-TNYUGAI    (IDXL)
               MOVE    SPA-GMN-TSRYKA(IDXL)
                                       TO  S02-TSRYKA    (IDXL)
               MOVE    SPA-GMN-THKNCOMBI(IDXL)
                                       TO  S02-THKN       (IDXL)
               MOVE    SPA-GMN-TFTNRATE(IDXL)
                                       TO  S02-TFTNRATE   (IDXL)
               MOVE    SPA-GMN-THAKYMD (IDXL)
                                       TO  S02-TSKYSTYMD  (IDXL)
               MOVE    SPA-GMN-TSRYYMD (IDXL)
                                       TO  S02-TSRYYMD    (IDXL)
               MOVE    SPA-GMN-TSEIKYU(IDXL)
                                       TO  WRK-TSEIKYU-W
               MOVE    WRK-TSEIKYU-W   TO  S02-TSKYMONEY  (IDXL)
               MOVE    SPA-GMN-TNYUKIN (IDXL)
                                       TO  WRK-TNYUKIN-W
               MOVE    WRK-TNYUKIN-W   TO  S02-TNYUKINMONEY(IDXL)
               MOVE    SPA-GMN-TMISYU (IDXL)
                                       TO  WRK-TMISYU-W
               MOVE    WRK-TMISYU-W    TO  S02-TMISYUMONEY(IDXL)
               MOVE    SPA-GMN-TJYOTAI (IDXL)
                                       TO  S02-TDENPJTI   (IDXL)
      *
               IF    ( IDXL         =   SPA-GMN-TBL-SEL )
                   MOVE    "T"         TO  S02-SYULIST-SELECT (IDXL)
                   IF    ( SPA-NAI-TNYUGAIKBN(IDXL) =   "1" )
                       MOVE    WIDGET-INSENSITIVE
                                       TO  S02-B04-STATE
                       MOVE    WIDGET-NORMAL
                                        TO  S02-B03S-STATE
                   ELSE
                       MOVE    IDXL  TO  IDXB
                       PERFORM 800-BUNBEN-CHK-SEC
                       IF    ( FLG-BUNBENCHK   =   1 )
                           MOVE    "出産一時金"
                                        TO  S02-B03S-LABEL
                           MOVE    WIDGET-NORMAL
                                        TO  S02-B03S-STATE
                       END-IF
                   END-IF
               ELSE
                   MOVE    "F"         TO  S02-SYULIST-SELECT (IDXL)
               END-IF
      *
               EVALUATE   SPA-GMN-SIKIBETUFLG(IDXL)
      *          第三者行為現物給付
                 WHEN       "1"
                   MOVE   1   TO   WRK-CHECKFLG(1)
      *          ７０歳未満現物給付
                 WHEN       "2"
                   MOVE   1   TO   WRK-CHECKFLG(2)
      *          高齢者現物給付
                 WHEN       "3"
                   MOVE   1   TO   WRK-CHECKFLG(3)
      *          マル長計算
                 WHEN       "4"
                   MOVE   1   TO   WRK-CHECKFLG(4)
      *          該当者チェック抽出分
                 WHEN       "6"
                   MOVE   1   TO   WRK-CHECKFLG(6)
               END-EVALUATE
      *        収納更新未処理
               IF    SPA-GMN-TNYUGAIKBN(IDXL)(3:2)  =  "☆"
                   MOVE   1   TO   WRK-CHECKFLG(5)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX                 TO  S02-SYULIST-COUNT
      *
           MOVE    SPA-GMN-SELBAN              TO  WRK-SELBAN-W
           MOVE    WRK-SELBAN-W                TO  S02-SELNUM
      *
           EVALUATE   TRUE
             WHEN   WRK-CHECKFLG(1) =  1
                 MOVE    "red"    TO    S02-LBLMSG-STYLE
       MOVE "★★　【第三者行為現物給付】一括再計算を行ってください　"
                                  TO    S02-LBLMSG(1:56)
       MOVE "★★"                TO    S02-LBLMSG(57:)
             WHEN   WRK-CHECKFLG(2) =  1
                 MOVE    "red"    TO    S02-LBLMSG-STYLE
       MOVE "★★　【７０歳未満現物給付】一括再計算を行ってください　"
                                  TO    S02-LBLMSG(1:56)
       MOVE "★★"                TO    S02-LBLMSG(57:)
             WHEN   WRK-CHECKFLG(3) =  1
                 MOVE    "red"    TO    S02-LBLMSG-STYLE
       MOVE "★★　【高齢者現物給付】一括再計算を行ってください　★★"
                                  TO    S02-LBLMSG
             WHEN   WRK-CHECKFLG(4) =  1
                 MOVE    "red"    TO    S02-LBLMSG-STYLE
       MOVE "★★　【マル長計算】一括再計算を行ってください　★★"
                                  TO    S02-LBLMSG
             WHEN   WRK-CHECKFLG(5) =  1
                 MOVE    "blue"   TO    S02-LBLMSG-STYLE
                 MOVE    SPACE    TO    S02-LBLMSG
                 STRING  "☆　再計算を行ってください"
                                        DELIMITED BY SIZE
                         "（会計照会【収納更新】未処理）"
                                        DELIMITED BY SIZE
                 INTO    S02-LBLMSG
                 END-STRING
             WHEN   WRK-CHECKFLG(6) =  1
                 MOVE    "red"    TO    S02-LBLMSG-STYLE
       MOVE "★★　【該当者チェック抽出分】一括再計算を行ってください"
                                  TO    S02-LBLMSG(1:56)
       MOVE "　★"                TO    S02-LBLMSG(57:)
           END-EVALUATE
      *
           MOVE    WIDGET-NORMAL       TO  S02-B06-STATE
                                           S02-B07-STATE
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-SRYYM       =   SPACE )
               MOVE    "前頁"          TO  S02-B06-LABEL
               MOVE    "次頁"          TO  S02-B07-LABEL
               STRING  "　　　　　　　　　　　"  DELIMITED   BY  SIZE
                       "請求額計：外来"          DELIMITED   BY  SIZE
               INTO    S02-LBLSRYYMG
               END-STRING
               IF    ( SPA-GMN-PAGE    =   ZERO )
                   MOVE    WIDGET-INSENSITIVE  TO  S02-B06-STATE
               END-IF
      *
               IF    ( SPA-GMN-NEXT    =   ZERO )
                   MOVE    WIDGET-INSENSITIVE  TO  S02-B07-STATE
               END-IF
           WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
               MOVE    "前年"          TO  S02-B06-LABEL
               MOVE    "次年"          TO  S02-B07-LABEL
               MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
               MOVE    "0101"          TO  WRK-SYMD(5:)
               PERFORM 800-HIZUKE-SEC
               STRING  "　　　　　　"      DELIMITED   BY  SIZE
                       WRK-HENYMDG2 (1:10) DELIMITED   BY  SIZE
                       "請求額計：外来"    DELIMITED   BY  SIZE
               INTO    S02-LBLSRYYMG
               END-STRING
           WHEN  ( SPA-NAI-SRYYM (7:)  =   SPACE )
               MOVE    "前月"          TO  S02-B06-LABEL
               MOVE    "次月"          TO  S02-B07-LABEL
               MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD(7:)
               PERFORM 800-HIZUKE-SEC
               STRING  "　　　"
                        WRK-HENYMDG2 (1:16) DELIMITED   BY  SIZE
                       "請求額計：外来"    DELIMITED   BY  SIZE
               INTO    S02-LBLSRYYMG
               END-STRING
           WHEN  OTHER
               MOVE    "前日"          TO  S02-B06-LABEL
               MOVE    "次日"          TO  S02-B07-LABEL
           END-EVALUATE
      *
           IF    ( SPA-NAI-SYORINO         =   SPACE )
            AND  ( SPA-GMN-SMEILIST-SEL    =   ZERO )
               MOVE    "再印刷"        TO  S02-B11S-LABEL
               MOVE    "処理結果"      TO  S02-B12S-LABEL
           ELSE
               MOVE    "明細書印刷"    TO  S02-B11S-LABEL
               MOVE    "請求書印刷"    TO  S02-B12S-LABEL
           END-IF
      *
           IF    ( SPA-GMN-MODE        =   CONST-MODE-NORMON
                                        OR CONST-MODE-NORYEAR
                                        OR CONST-MODE-NORALL
                                                          )
               MOVE    "入院"          TO  S02-LBLSRYYMN
           ELSE
               MOVE    SPACE           TO  S02-LBLSRYYMG
           END-IF
      *
           MOVE    "black"             TO  S02-LBLMSG4-STYLE
           PERFORM 4901-SKYYM-CHK-SEC
           IF    ( WRK-SKYYM       NOT =   SPACE )
               MOVE    WRK-SKYYM       TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD(7:2)
               PERFORM 800-HIZUKE-SEC
               STRING  "一括再計算は"
                                               DELIMITED   BY  SIZE
                       WRK-HENYMDG2 (1:16)     DELIMITED   BY  SIZE
                       "が対象となります。"    DELIMITED   BY  SIZE
               INTO    S02-LBLMSG4
               END-STRING
           END-IF
      *
           MOVE    "black"                     TO  S02-LBLMSG2-STYLE
      *
           IF    ( SPA-GMN-PTID    NOT =   ZERO )
      *
               MOVE    "blue"                  TO  S02-LBLMSG2-STYLE
      *
               IF    ( SPA-GMN-SELECTED    NOT =   ZERO )
                OR   ( SPA-GMN-PAGE            >   ZERO )
                   CONTINUE
               ELSE
                   EVALUATE    SPA-GMN-MODE
                   WHEN    CONST-MODE-NORMON
                   WHEN    CONST-MODE-NORYEAR
                   WHEN    CONST-MODE-NORALL
                       EVALUATE    TRUE
                       WHEN  ( SPA-NAI-SRYYM       =   SPACE )
                           MOVE    "該当患者に収納の登録はありません。"
                                                   TO  S02-LBLMSG2
                       WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
                           MOVE    "診療年に収納の登録はありません。"
                                                   TO  S02-LBLMSG2
                       WHEN  OTHER
                           MOVE    "診療年月に収納の登録はありません。"
                                                   TO  S02-LBLMSG2
                       END-EVALUATE
                   WHEN    CONST-MODE-MISYU
                           MOVE    "未収金（過入金）はありません"
                                                   TO  S02-LBLMSG2
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    "black"                     TO  S02-LBLMSG5-STYLE
                                                   S02-LBLMSG6-STYLE
                                                   S02-KANANAME-STYLE
                                                   S02-NAME-STYLE
      *
           IF    ( SPA-NAI-LOCK        =   ZERO )
               CONTINUE
           ELSE
               MOVE    "red"                   TO  S02-LBLMSG2-STYLE
                                                   S02-KANANAME-STYLE
                                                   S02-NAME-STYLE
      *
               MOVE    "他端末で使用中です。"  TO  S02-LBLMSG2
               MOVE    SPA-NAI-SLOCK-MSG       TO  S02-LBLMSG2(21:)
      *
           END-IF
      *
           IF    ( SPA-NAI-SRYYM   =   SPACE )
               IF    ( SPA-GMN-PAGE    >   ZERO )
                OR   ( SPA-GMN-NEXT    =   1    )
                   MOVE    "blue"      TO  S02-LBLMSG5-STYLE
                   INITIALIZE  WRK-SYULIST-PAGE-G
                   COMPUTE CNT-PAGE    =   SPA-GMN-PAGE    +   1
                   MOVE    CNT-PAGE    TO  WRK-SYULIST-PAGE
                   IF    ( SPA-GMN-NEXT    =   1 )
                       MOVE    "頁　次頁有り"
                                       TO  WRK-SYULIST-LBL
                   ELSE
                       MOVE    "頁"    TO  WRK-SYULIST-LBL
                   END-IF
                   MOVE    WRK-SYULIST-PAGE-G  TO  S02-LBLMSG5
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-MODE
           WHEN    CONST-MODE-NORMON
           WHEN    CONST-MODE-NORYEAR
           WHEN    CONST-MODE-NORALL
               MOVE    "未収一覧"              TO  S02-B08-LABEL
           WHEN    CONST-MODE-MISYU
               MOVE    "入返金日"              TO  S02-B08-LABEL
           WHEN    CONST-MODE-NYUHEN
               MOVE    "収納一覧"              TO  S02-B08-LABEL
           END-EVALUATE
      *
           MOVE    WIDGET-NORMAL               TO  S02-SRYYM-STATE
           IF    ( SPA-GMN-MODE                =   CONST-MODE-MISYU )
               MOVE    WIDGET-INSENSITIVE      TO  S02-SRYYM-STATE
           END-IF
      *
           MOVE    SPA-GMN-GAI-MISYU           TO  S02-GAI-MISYU
      *
           MOVE    SPA-GMN-NYU-MISYU           TO  S02-NYU-MISYU
      *
           IF    ( SPA-GMN-MODE    =   CONST-MODE-NORMON
                                    OR CONST-MODE-NORYEAR
                                    OR CONST-MODE-NORALL  )
               MOVE    SPA-GMN-GAI-TUKISKY     TO  S02-GAI-TUKISKY
      *
               MOVE    SPA-GMN-NYU-TUKISKY     TO  S02-NYU-TUKISKY
           END-IF
      *
      *
           MOVE    SPA-GMN-CHOSEI1INX  TO  S02-CHOSEI1IN
           MOVE    SPA-GMN-CHOSEI2INX  TO  S02-CHOSEI2IN
           MOVE    SPA-GMN-SKYMONEYIN  TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  S02-SKYMONEYIN
      *
      *    入金欄
           EVALUATE    SPA-NAI-SYORINO
           WHEN    CONST-PROC-NYUKIN
           WHEN    CONST-PROC-TORIKESHI
           WHEN    CONST-PROC-RRKSYU
               MOVE    SPA-GMN-NYUKINX     TO  S02-NYUKINGK2
               MOVE    SPA-GMN-NYUKINYMD   TO  S02-NYUKINYMD
      *
               IF    ( SPA-NAI-SYORINO     =   CONST-PROC-RRKSYU )
                AND  ( SPA-NAI-MINUS       =   "1" )
                AND  ( SPA-NAI-DENPJTIKBN  NOT =   "6" )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-NYKN-HOHO-G TO  S02-NYUKINHOUHOU
                   MOVE    SPA-GMN-NYKN-HOHO-MAX
                                           TO  S02-NYUKINHOUHOU-COUNT
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   SPA-GMN-NYKN-HOHO-MAX )
                       MOVE    SPA-GMN-NYKN-HOHOLST (IDX0)
                                       TO  S02-NYUKINHOUHOULST(IDX0)
                   END-PERFORM
               END-IF
      *
               MOVE    SPA-GMN-SKYPRT-G    TO  S02-SKYPRT
               MOVE    SPA-GMN-SKYPRT-MAX  TO  S02-SKYPRT-COUNT
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-SKYPRT-MAX )
                   MOVE    SPA-GMN-SKYPRTLST (IDX0)
                                   TO  S02-SKYPRTLST(IDX0)
               END-PERFORM
      *
               MOVE    SPA-GMN-SRYPRT-G    TO  S02-SRYPRT
               MOVE    SPA-GMN-SRYPRT-MAX  TO  S02-SRYPRT-COUNT
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-SRYPRT-MAX )
                   MOVE    SPA-GMN-SRYPRTLST (IDX0)
                                   TO  S02-SRYPRTLST(IDX0)
               END-PERFORM
      *
           END-EVALUATE
      *
           MOVE    SPA-GMN-EDANUM          TO  S02-RIREKIIN
      *
           MOVE    SPA-GMN-SMEILIST-MAX    TO  S02-MEILIST-COUNT
           PERFORM VARYING IDXM    FROM    1   BY  1
                   UNTIL ( IDXM    >   SPA-GMN-SMEILIST-MAX )
      *
               MOVE    IDXM                TO  WRK-EDANUMZ
               MOVE    WRK-EDANUMZ         TO  S02-TMEDANUM     (IDXM)
               MOVE    SPA-GMN-TMSKYMONEY (IDXM)
                                           TO  WRK-KINGAKUZ2
               MOVE    WRK-KINGAKUZ2       TO  S02-TMSKYMONY    (IDXM)
               MOVE    SPA-GMN-TMNYUKIN   (IDXM)
                                           TO  WRK-KINGAKUZ2
               MOVE    WRK-KINGAKUZ2       TO  S02-TMNYUKINMONEY(IDXM)
               MOVE    SPA-GMN-TMSYORIYMD (IDXM)
                                           TO  S02-TMSYORIYMD   (IDXM)
               MOVE    SPA-GMN-TMJOUTAI   (IDXM)
                                           TO  S02-TMJTI        (IDXM)
               MOVE    SPA-GMN-TMNYKN-HOHO(IDXM)
                                           TO  S02-TMNYKN-NAME  (IDXM)
               IF    ( SPA-GMN-SMEILIST-SEL    =   IDXM )
                   MOVE    "T"             TO  S02-MEILIST-SELECT(IDXM)
               ELSE
                   MOVE    "F"             TO  S02-MEILIST-SELECT(IDXM)
               END-IF
      *
           END-PERFORM
      *
           MOVE    WIDGET-NORMAL               TO  S02-PTNUM-STATE
      *
           MOVE    "black"                 TO  S02-SKYMONEYIN-STYLE
           MOVE    WIDGET-NORMAL           TO  S02-CHOSEI1IN-STATE
                                               S02-CHOSEI2IN-STATE
                                               S02-SKYMONEYIN-STATE
                                               S02-NYUKINGK2-STATE
                                               S02-NYUKINHOUHOU-STATE
                                               S02-NYUKINHOUHOULST-STATE
                                               S02-NYUKINYMD-STATE
                                               S02-SKYPRT-STATE
                                               S02-SKYPRTLST-STATE
                                               S02-SRYPRT-STATE
                                               S02-SRYPRTLST-STATE
                                               S02-B12-STATE
      *
           IF    ( SPA-NAI-SYORINO     =   CONST-PROC-RRKSYU )
      *
               MOVE    "blue"          TO  S02-LBLMSG7-STYLE
               MOVE    "【履歴修正】"  TO  S02-LBLMSG7
      *
               MOVE    "blue"          TO  S02-SKYMONEYIN-STYLE
      *
               IF    ( SPA-NAI-MINUS           =   "1" )
                AND  ( SPA-NAI-DENPJTIKBN  NOT =   "6" )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  S02-NYUKINHOUHOU-STATE
                                           S02-NYUKINHOUHOULST-STATE
               END-IF
      *
               IF    ( SPA-NAI-PROC-FLG    =   1  OR 2 )
                   IF    ( SPA-NAI-SYURRKNUM   =   ZERO )
                       MOVE    WIDGET-INSENSITIVE
                                       TO  S02-CHOSEI1IN-STATE
                                           S02-CHOSEI2IN-STATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-SYORINO     =   CONST-PROC-RRKSYU )
            AND  ( SPA-NAI-PROC-FLG    =   3  OR 4)
               MOVE    WIDGET-INSENSITIVE
                                       TO  S02-CHOSEI1IN-STATE
                                           S02-CHOSEI2IN-STATE
                                           S02-SKYMONEYIN-STATE
           END-IF
      *
           IF    ( SPA-NAI-SYORINO     =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S02-CHOSEI1IN-STATE
                                           S02-CHOSEI2IN-STATE
                                           S02-SKYMONEYIN-STATE
                                           S02-NYUKINGK2-STATE
                                           S02-NYUKINYMD-STATE
                                           S02-NYUKINHOUHOU-STATE
                                           S02-NYUKINHOUHOULST-STATE
                                           S02-SKYPRT-STATE
                                           S02-SKYPRTLST-STATE
                                           S02-SRYPRT-STATE
                                           S02-SRYPRTLST-STATE
                                           S02-B12-STATE
           END-IF
      *
           IF    ( SPA-NAI-SYORINO     =   CONST-PROC-NYUKIN
                                       OR  CONST-PROC-TORIKESHI )
      *
               MOVE    WIDGET-INSENSITIVE
                                       TO  S02-CHOSEI1IN-STATE
                                           S02-CHOSEI2IN-STATE
                                           S02-SKYMONEYIN-STATE
      *
               IF    ( SPA-NAI-SYORINO =   CONST-PROC-NYUKIN )
                   MOVE    "blue"          TO  S02-LBLMSG7-STYLE
                   MOVE    "【入金】"      TO  S02-LBLMSG7
               ELSE
                   MOVE    "blue"          TO  S02-LBLMSG7-STYLE
                   MOVE    "【入金取消】"  TO  S02-LBLMSG7
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-TBL-SEL     >   ZERO )
            AND  ( SPA-NAI-TJYOTAIKBN (SPA-GMN-TBL-SEL)
                                       =   "3" )
               MOVE    WIDGET-INSENSITIVE 
                                       TO  S02-CHOSEI1IN-STATE
                                           S02-CHOSEI2IN-STATE
                                           S02-SKYMONEYIN-STATE
           END-IF
      *
           MOVE    WIDGET-NORMAL               TO  S02-B04S-STATE
           IF    ( SPA-GMN-NYUGAIKBN           =   "1" )
               MOVE    WIDGET-INSENSITIVE      TO  S02-B04S-STATE
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE      TO  S02-B12C-STATE
           MOVE    "black"                 TO  S02-B12C-STYLE
           MOVE    "再計算該当者一覧"      TO  S02-B12C-LABEL
      *
           IF    ( SPA-GMN-MODE    =   CONST-MODE-NORMON )
               MOVE    WIDGET-NORMAL       TO  S02-B12C-STATE
               IF    ( SPA-NAI-S10-CHK     =   "1" )
                   MOVE    "blue"          TO  S02-B12C-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      **************************************************************
      *    画面カーソルセット処理
      **************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 50012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    1
               MOVE    "SELNUM"    TO  MCP-WIDGET
           WHEN    2
               MOVE    "PTNUM"     TO  MCP-WIDGET
           WHEN    3
               MOVE    "SRYYM"     TO  MCP-WIDGET
           WHEN    4
               MOVE    "SRYKA"     TO  MCP-WIDGET
           WHEN    5
               MOVE    "NYUGAIKBN" TO  MCP-WIDGET
           WHEN    6
               MOVE    "MODE"      TO  MCP-WIDGET
           WHEN    7
               MOVE    "SORTKBN"   TO  MCP-WIDGET
           WHEN    8
               MOVE    "CHOSEI1IN" TO  MCP-WIDGET
           WHEN    9
               MOVE    "CHOSEI2IN" TO  MCP-WIDGET
           WHEN    10
               MOVE    "NYUKINGK2" TO  MCP-WIDGET
           WHEN    11
               MOVE    "NYUKINHOUHOU"
                                   TO  MCP-WIDGET
           WHEN    12
               MOVE    "NYUKINYMD" TO  MCP-WIDGET
           WHEN    13
               MOVE    "EDANUM"    TO  MCP-WIDGET
           WHEN    14
               MOVE    "SKYPRT"    TO  MCP-WIDGET
           WHEN    15
               MOVE    "SRYPRT"    TO  MCP-WIDGET
           WHEN    112
               MOVE    "B12"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHOSEI1IN"
               MOVE    09              TO  SPA-GMN-CUR
           WHEN    "CHOSEI2IN"
               MOVE    10              TO  SPA-GMN-CUR
           WHEN    "NYUKINGK2"
               MOVE    11              TO  SPA-GMN-CUR
           WHEN    "NYUKINHOUHOU"
               MOVE    12              TO  SPA-GMN-CUR
           WHEN    "NYUKINYMD"
               MOVE    14              TO  SPA-GMN-CUR
           WHEN    "SKYPRT"
               MOVE    15              TO  SPA-GMN-CUR
           WHEN    "SRYPRT"
               MOVE    112             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       50012-CUR-SKIP-SEC              SECTION.
      *
           IF    ( SPA-GMN-CUR     =   3 )
               IF    ( S02-SRYYM-STATE =   WIDGET-INSENSITIVE )
                   IF    ( SPA-GMN-MAX >   ZERO )
                       MOVE    1   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    6   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   8 )
               IF   ( S02-CHOSEI1IN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    9           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   9 )
               IF   ( S02-CHOSEI2IN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    10          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   10 )
               IF   ( S02-NYUKINGK2-STATE =   WIDGET-INSENSITIVE )
                   MOVE    11          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   11)
               IF   ( S02-NYUKINHOUHOU-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    12          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   12 )
               IF   ( S02-NYUKINYMD-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    14              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   14 )
               IF   ( S02-SKYPRT-STATE     =   WIDGET-INSENSITIVE )
                   MOVE    15              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   15 )
               IF   ( S02-SRYPRT-STATE     =   WIDGET-INSENSITIVE )
                   MOVE    112             TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       50012-CUR-SKIP-EXT.
           EXIT.
      **************************************************************
      *    画面遷移処理
      **************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
      *    外来月別
           WHEN    "CLICKED"       ALSO    "B01S"
               PERFORM 490-GAI-TUKISKYPRT-SEC
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    前回患者処理
           WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-MAEKPTNUM-SEC
      *
      *    再発行
           WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 419-SAIHAKKOU-SEC
      *    請求取消（出産一時金）処理
           WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 430-TORIKESI-SEC
      *    再計算
           WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-SAIKEI-SEC
      *    返金
           WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 450-HENKIN-SEC
      *    入金
           WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 460-NYUKIN-SEC
      *    一括入金画面（Ｓ06）
           WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 460-IKKATU-NYUKIN-SEC
      *    入金取消
           WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 470-NYUDEL-SEC
      *    履歴修正
           WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 490-RIREKI-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 410-TOROKU-SEC
      *    ＪＤ入金
      ***  WHEN    "CLICKED"       ALSO    "B13"
      ***          PERFORM 490-JD-NYUKIN-SEC
      *    前月表示
      *****WHEN    "CLICKED"       ALSO    "B14"
           WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 490-ZENGETU-SEC
      *    次月表示
      *****WHEN    "CLICKED"       ALSO    "B15"
           WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 490-JIGETU-SEC
      *    画面モード変更
           WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-MODE-CHENGE-SEC
      *    氏名検索（Ｐ９７）
           WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ（Ｙ０２）
           WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ（Ｕ０１）
           WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *----(01.01.19)--ADD-START---
      *    支払証明書処理
           WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 500-SHIHARAI-SEC
      *----(01.01.19)--ADD-END-----
      *    処理結果画面
           WHEN    "CLICKED"       ALSO    "B12S"
               IF    ( SPA-NAI-SYORINO         =   SPACE )
                AND  ( SPA-GMN-SMEILIST-SEL    =  ZERO  )
                   PERFORM 490-KAKUNIN-SEC
               ELSE
                   MOVE    "1"     TO  WRK-SKYPRT
                   MOVE    "0"     TO  WRK-SRYPRT
                   PERFORM 490-PRINT-SEC
               END-IF
      *    登録
           WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 490-KAKUTEI-SEC
      *    再印刷
           WHEN    "CLICKED"       ALSO    "B11S"
               IF    ( SPA-NAI-SYORINO         =   SPACE )
                AND  ( SPA-GMN-SMEILIST-SEL    =  ZERO  )
                   PERFORM 450-REPRINT-SEC
               ELSE
                   MOVE    "0"     TO  WRK-SKYPRT
                   MOVE    "1"     TO  WRK-SRYPRT
                   PERFORM 490-PRINT-SEC
               END-IF
      *    外来分一括再計算
           WHEN    "CLICKED"       ALSO    "B04S"
               PERFORM 440-IKATUSAIKEI-CHK-SEC
      *    一括再計算該当
           WHEN    "CLICKED"       ALSO    "B12C"
           WHEN    "CLICKED"       ALSO    "B12C_2"
               PERFORM 412-S10-SENI-SEC
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      **************************************************************
      *    会話　処理
      **************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    選択番号
           WHEN    ANY                 ALSO    "SYULIST"
               PERFORM 4105-LSTSEL-SEC
           WHEN    "ACTIVATE"          ALSO    "SELNUM"
               PERFORM 4101-SELNUM-CHK-SEC
      *    患者番号
           WHEN    "ACTIVATE"          ALSO    "PTNUM"
               PERFORM 4102-PTNUM-CHK-SEC
      *    診療科
           WHEN    "ACTIVATE"          ALSO    "SRYKA"
               PERFORM 4103-SRYKA-CHK-SEC
      *    請求年月
           WHEN    "ACTIVATE"          ALSO    "SRYYM"
               PERFORM 4104-SRYYM-CHK-SEC
      *    入外区分
           WHEN    "ACTIVATE"          ALSO    "NYUGAIKBN"
               PERFORM 4106-NYUGAIKBN-CHK-SEC
      *    モード変更
           WHEN    "ACTIVATE"          ALSO    "MODE"
               PERFORM 4106-MODE-CHK-SEC
      *    並び順変更
           WHEN    "ACTIVATE"          ALSO    "SORTKBN"
               PERFORM 4106-SORTKBN-CHK-SEC
      *    明細番号
           WHEN    "ACTIVATE"          ALSO    "EDANUM"
               PERFORM 4100-EDANUM-CHK-SEC
      *    明細リスト
           WHEN    ANY                 ALSO    "MEILST"
               PERFORM 4100-SMEILIST-CHK-SEC
      *
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    S02-CHOSEI1IN       TO  SPA-GMN-CHOSEI1INX
           MOVE    S02-CHOSEI2IN       TO  SPA-GMN-CHOSEI2INX
           MOVE    S02-NYUKINGK2       TO  SPA-GMN-NYUKINX
           MOVE    S02-NYUKINYMD       TO  SPA-GMN-NYUKINYMD
           MOVE    S02-NYUKINHOUHOU    TO  SPA-GMN-NYKN-HOHO-G
           MOVE    S02-SKYPRT          TO  SPA-GMN-SKYPRT-G
           MOVE    S02-SRYPRT          TO  SPA-GMN-SRYPRT-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-TBL-SEL     >   ZERO )
               EVALUATE    SPA-NAI-SYORINO
               WHEN    SPACE
                   PERFORM 4102-SHOKAI-KIHON-CHK-SEC
               WHEN    CONST-PROC-NYUKIN
                   PERFORM 4102-NYUKIN-KIHON-CHK-SEC
      *
               WHEN    CONST-PROC-RRKSYU
                   PERFORM 4102-RRKSYU-KIHON-CHK-SEC
      *
               WHEN    CONST-PROC-TORIKESHI
                   PERFORM 4102-TORIKESHI-KIHON-CHK-SEC
               END-EVALUATE
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    照会基本チェック処理
      *****************************************************************
       4102-SHOKAI-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 4101-EDANUM-CHK-SEC
      *
           .
       4102-SHOKAI-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金処理基本チェック処理
      *****************************************************************
       4102-NYUKIN-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NYUKIN-CHK-SEC
           END-IF
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-NYUKIN-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴修正処理時基本チェック処理
      *****************************************************************
       4102-RRKSYU-KIHON-CHK-SEC       SECTION.
      *
           IF    ( WRK-MCP-WIDGET      =   "SKYPRT" OR "SRYPRT" )
               CONTINUE
           ELSE
               PERFORM 4101-EDANUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI1IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI2IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-MINUS  =   "1" )
                   PERFORM 4102-NYUKIN-CHK-SEC
               ELSE
                   PERFORM 4101-NYUKIN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-RRKSYU-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金取消処理基本チェック処理
      *****************************************************************
       4102-TORIKESHI-KIHON-CHK-SEC    SECTION.
      *
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4102-NYUKIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-TORIKESHI-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    金額欄妥当性チェック処理
      *****************************************************************
       4101-KINGAKU-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-CHOSEI1IN
               MOVE    SPA-GMN-CHOSEI1INX
                                       TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    8           TO  SPA-GMN-CUR
                   MOVE    "3017"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-CHOSEI1IN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-CHOSEI1INX
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-CHOSEI2IN
               MOVE    SPA-GMN-CHOSEI2INX
                                       TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    9           TO  SPA-GMN-CUR
                   MOVE    "3018"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-CHOSEI2IN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-CHOSEI2INX
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-NYUKIN
               MOVE    SPA-GMN-NYUKINX TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    10          TO  SPA-GMN-CUR
                   MOVE    "3019"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-NYUKIN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-NYUKINX
               END-IF
           END-IF
      *
           .
       4101-KINGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金１チェック処理
      *****************************************************************
       4101-CHOSEI1IN-CHK-SEC           SECTION.
      *
      *
           .
       4101-CHOSEI1IN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金２チェック処理
      *****************************************************************
       4101-CHOSEI2IN-CHK-SEC           SECTION.
      *
           COMPUTE SPA-GMN-SKYMONEYIN
               =   SPA-NAI-SKYMONEY
               +   SPA-GMN-CHOSEI1IN
               +   SPA-GMN-CHOSEI2IN
      *
           MOVE    ZERO                TO  WRK-GMN-CUR
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-CHOSEI1IN   NOT =   ZERO )
               MOVE    8               TO  WRK-GMN-CUR
           WHEN  ( SPA-GMN-CHOSEI2IN   NOT =   ZERO )
               MOVE    9               TO  WRK-GMN-CUR
           END-EVALUATE
      *
           IF    ( SPA-GMN-SKYMONEYIN  >   CONST-KINGAKU-MAX )
                   MOVE    "3023"      TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD        =  SPACE )
               MOVE    SPA-GMN-TBL-SEL TO  IDXL
               MOVE    ZERO            TO  WRK-SKYMONEY
               IF    ( SPA-NAI-SYORINO =   CONST-PROC-RRKSYU )
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-TSEIKYU(IDXL)
                       -   SPA-NAI-SKYMONEY-MOTO
                       +   SPA-GMN-SKYMONEYIN
               ELSE
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-TSEIKYU(IDXL)
                       +   SPA-GMN-SKYMONEYIN
               END-IF
      *
               IF    ( WRK-SKYMONEY  > CONST-KINGAKU-MAX )
                       MOVE    "3024"      TO  SPA-ERRCD
                       MOVE    WRK-GMN-CUR TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-CHOSEI2IN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理
      *****************************************************************
       4101-NYUKIN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-TBL-SEL TO  IDXL
      *
           MOVE    ZERO            TO  WRK-NYUKIN
      *
           EVALUATE    SPA-NAI-SYORINO
           WHEN    CONST-PROC-NYUKIN
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-TNYUKIN (IDXL)
                   +   SPA-GMN-NYUKIN
           WHEN    CONST-PROC-RRKSYU
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-TNYUKIN (IDXL)
                   -   SPA-NAI-NYUKIN-MOTO
                   +   SPA-GMN-NYUKIN
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-NYUKIN  <  ZERO )
                   MOVE    10      TO  SPA-GMN-CUR
                   MOVE    "3014"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  <  ZERO )
                   MOVE    10      TO  SPA-GMN-CUR
                   MOVE    "3008"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  >   CONST-KINGAKU-MAX )
                   MOVE    10      TO  SPA-GMN-CUR
                   MOVE    "3025"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "NYUKINGK2" )
                   IF    ( SPA-NAI-SYORINO     =   CONST-PROC-NYUKIN )
                    AND  ( SPA-GMN-NYUKIN      =   ZERO )
                    AND  ( SPA-GMN-TMISYU(IDXL) >  ZERO )
                       MOVE    SPA-GMN-TMISYU(IDXL)
                                   TO  SPA-GMN-NYUKIN
                       MOVE    SPA-GMN-NYUKIN  TO  WRK-KINGAKUZ
                       MOVE    WRK-KINGAKUZ    TO  SPA-GMN-NYUKINX
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理（入金取消、返金の履歴修正）
      *****************************************************************
       4102-NYUKIN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-TBL-SEL TO  IDXL
      *
           MOVE    ZERO            TO  WRK-NYUKIN
      *
           EVALUATE    SPA-NAI-SYORINO
           WHEN    CONST-PROC-TORIKESHI
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-TNYUKIN (IDXL)
                   +   SPA-GMN-NYUKIN
           WHEN    CONST-PROC-RRKSYU
      *
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-TNYUKIN (IDXL)
                   -   SPA-NAI-NYUKIN-MOTO
                   +   SPA-GMN-NYUKIN
           END-EVALUATE
      *
           IF    ( SPA-GMN-NYUKIN  >   ZERO )
               MOVE    10          TO  SPA-GMN-CUR
               MOVE    "3013"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  <  ZERO )
                   MOVE    10          TO  SPA-GMN-CUR
                   MOVE    "3008"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  >   CONST-KINGAKU-MAX )
                   MOVE    10      TO  SPA-GMN-CUR
                   MOVE    "3025"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-SYORINO     =   CONST-PROC-TORIKESHI )
               MOVE    ZERO            TO  WRK-NYUKIN
      *
               PERFORM VARYING IDXM FROM    1   BY  1
                       UNTIL ( IDXM >   SPA-GMN-SMEILIST-MAX )
      *
                   IF ( SPA-NAI-NYUKINYMD
                                       =   SPA-NAI-TMSYORIYMD (IDXM))
                       COMPUTE WRK-NYUKIN
                           =   WRK-NYUKIN
                           +   SPA-GMN-TMNYUKIN (IDXM)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "NYUKINGK2" )
                   IF    ( SPA-NAI-SYORINO
                                       =   CONST-PROC-TORIKESHI )
                    AND  ( SPA-GMN-NYUKIN      =   ZERO )
                       IF   ( WRK-NYUKIN          >   ZERO )
                           COMPUTE SPA-GMN-NYUKIN
                               =   WRK-NYUKIN      *   -1
      *
                           MOVE    SPA-GMN-NYUKIN
                                           TO  WRK-KINGAKUZ
                           MOVE    WRK-KINGAKUZ
                                           TO  SPA-GMN-NYUKINX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4102-NYUKIN-CHK-EXT.
           EXIT.
      **************************************************************
      *    入金方法チェック処理
      **************************************************************
       4101-NYKN-HOHO-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-NYKN-HOHO-G     =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-NYKN-HOHO   TO  WRK-CMB-CD
               PERFORM 4200-NYKN-HOHOLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "3011"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYKN-HOHO-G
               END-IF
           END-IF
      *
           .
       4101-NYKN-HOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理日チェック処理
      *****************************************************************
       4101-NYUKINYMD-CHK-SEC          SECTION.
      *
           MOVE    SPACE                   TO  SPA-NAI-NYUKINYMD
      *
           IF    ( SPA-GMN-NYUKINYMD   =   SPACE )
               MOVE    SPA-SYSYMD          TO  WRK-SYMD
               PERFORM 802-HIZUKE-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUKINYMD
               MOVE    WRK-SYMD            TO  SPA-NAI-NYUKINYMD
           ELSE
               MOVE    SPA-GMN-NYUKINYMD   TO  WRK-SYMD
               PERFORM 802-HIZUKE-SEC
               IF    ( SGDAY-RC            =   ZERO )
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUKINYMD
                   MOVE    WRK-SYMD        TO  SPA-NAI-NYUKINYMD
               ELSE
                   MOVE    12              TO  SPA-GMN-CUR
                   MOVE    "3010"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4101-NYUKINYMD-CHK-EXT.
           EXIT.
      **************************************************************
      *    番号選択入力処理
      **************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-TBL-SEL
      *
           MOVE    S02-SELNUM          TO  SPA-GMN-SELBAN
      *
           IF    ( SPA-GMN-SELBAN      =  ZERO )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SELBAN  >   SPA-GMN-MAX )
                   MOVE    "0001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPA-GMN-SELBAN  TO  SPA-GMN-TBL-SEL
           END-IF
      *
      *
           PERFORM 300-SYUMEI-HENSYU-SEC
      *
           IF    ( SPA-NAI-SYORINO     =   SPACE )
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    患者番号チェック処理
      **************************************************************
       4102-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE                  SPA-GMN-HEAD
                                       SPA-NAI-HEAD
                                       SPA-GMN-PAGE
                                       SPA-GMN-TBL
                                       SPA-GMN-MISYU
                                       SPA-GMN-TUKISKY
                                       SPA-GMN-INPUT
                                       SPA-NAI-TBL
      *
           MOVE    S02-PTNUM           TO  SPA-GMN-PTNUM
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *
           EVALUATE    SPTNUM-RC
           WHEN    00
               MOVE    SPTNUM-PTNUM    TO  SPA-GMN-PTNUM
               MOVE    SPTNUM-PTID     TO  SPA-GMN-PTID
      *
               PERFORM 320-PTINF-SET-SEC
      *
               PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
               MOVE    SPA-GMN-MODE    TO  S02-MODE
               MOVE    SPACE           TO  SPA-GMN-MODE
               MOVE    SPA-NAI-SRYYM   TO  SPA-NAI-THSRYYM
               PERFORM 4106-MODE-CHK-SEC
               IF    ( SPA-GMN-MAX     = ZERO )
                   MOVE    3           TO  SPA-GMN-CUR
               ELSE
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           WHEN   10
               MOVE    "0031"          TO  SPA-ERRCD
           WHEN    20
      *        氏名検索へ
               MOVE    SPACE           TO  LINKAREA
               MOVE    SPA-KYOUTU      TO  LNK-COMMON
               MOVE    SPA-S02         TO  LNK-SCRSPA
      *
               MOVE    "S02"           TO  SPA-MOTOPG
               MOVE    "P97"           TO  SPA-SAKIPG
      *        氏名セット
               MOVE    SPA-GMN-PTNUM   TO  SPA-NAME
               MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
               MOVE    "CHANGE"        TO  MCP-PUTTYPE
               MOVE    "P97"           TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1               TO  FLG-END
           WHEN    OTHER
               MOVE    "0031"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
       4102-PTNUM-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    診療科入力処理
      **************************************************************
       4103-SRYKA-CHK-SEC              SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           INITIALIZE                  SPA-GMN-PAGE
      *
           MOVE    S02-SRYKA (1:2)     TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
           MOVE    1               TO  FLG-SYULIST-ROW
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4103-SRYKA-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    診療年月入力処理
      **************************************************************
       4104-SRYYM-CHK-SEC              SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYMD
      *
           MOVE    S02-SRYYM           TO  WRK-SYMD
           PERFORM 801-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKE-RC       =   ZERO )
      *
               INITIALIZE                  SPA-GMN-PAGE
                                           SPA-GMN-SRYYM
                                           SPA-NAI-SRYYM
                                           SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
      *
               MOVE    WRK-SYMD    TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG TO  SPA-GMN-SRYYM
               IF    ( SPA-GMN-MODE    =   CONST-MODE-NORMON
                                        OR CONST-MODE-NORYEAR
                                        OR CONST-MODE-NORALL  )
                   EVALUATE    TRUE
                   WHEN  ( WRK-SYMD        =   SPACE )
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-MODELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-MODE-G
                   WHEN  ( WRK-SYMD (5:)   =   SPACE )
                       MOVE    "1"         TO  WRK-CMB-CD
                       PERFORM 4200-MODELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-MODE-G
                   WHEN  OTHER
                       MOVE    SPACE       TO  SPA-GMN-SRYYM (7:)
                                               SPA-NAI-SRYYM (7:)
                       MOVE    "0"         TO  WRK-CMB-CD
                       PERFORM 4200-MODELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-MODE-G
                   END-EVALUATE
               END-IF
      *
               PERFORM 3101-COMBO-EDIT-SEC
      *
               PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
               PERFORM 3101-MISYU-EDIT-SEC
      *
               MOVE    1               TO  FLG-SYULIST-ROW
               PERFORM 300-SYUNOU-HENSYU-SEC
               IF    ( SPA-GMN-MAX     >   ZERO )
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
      *
           ELSE
      *
               MOVE    "0033"          TO  SPA-ERRCD
      *
               MOVE    WRK-SRYYMD      TO  WRK-SYMD
               PERFORM 801-HIZUKE-CHK-SEC
               MOVE    WRK-SYMD    TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG TO  SPA-GMN-SRYYM
           END-IF
      *
           .
       4104-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4105-LSTSEL-SEC                 SECTION.
      *
      *    行選択
           MOVE    ZERO            TO  SPA-GMN-SELBAN
                                       SPA-GMN-TBL-SEL
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-TBL-SEL    >   ZERO )
      *
               IF    ( S02-SYULIST-SELECT (IDXL)    =   "T" )
                   MOVE    IDXL TO     S02-SELNUM
                   PERFORM 4101-SELNUM-CHK-SEC
                   MOVE    1    TO     SPA-GMN-CUR
               END-IF
      *
           END-PERFORM
      *
           .
       4105-LSTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4106-NYUGAIKBN-CHK-SEC          SECTION.
      *   
           MOVE    5                   TO  SPA-GMN-CUR
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           INITIALIZE                  SPA-GMN-PAGE
      *
           MOVE    S02-NYUGAIKBN (1:1) TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    1               TO  FLG-SYULIST-ROW
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4106-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    モード  入力処理
      *****************************************************************
       4106-MODE-CHK-SEC               SECTION.
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           IF    ( S02-MODE (1:1)      =   SPA-GMN-MODE )
               CONTINUE
           ELSE
      *
               MOVE    S02-MODE (1:1)  TO  WRK-CMB-CD
               PERFORM 4200-MODELST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-MODE-G
      *
               MOVE    SPA-NAI-THSRYYM
                                       TO  S02-SRYYM
               IF    ( SPA-GMN-TBL-SEL >   ZERO )
                   MOVE    SPA-NAI-TSRYYMD (SPA-GMN-TBL-SEL)
                                       TO  S02-SRYYM
               END-IF
      *
               EVALUATE    SPA-GMN-MODE
               WHEN    CONST-MODE-NORMON
                   IF    ( S02-SRYYM (1:6) IS  NUMERIC )
                       MOVE    SPACE       TO  S02-SRYYM (7:)
                   ELSE
                       MOVE    SPACE       TO  S02-SRYYM
                       MOVE    SPA-SYSYMD (1:6)
                                           TO  S02-SRYYM (1:6)
                   END-IF
               WHEN    CONST-MODE-NORYEAR
                   IF    ( S02-SRYYM (1:4) IS  NUMERIC )
                       MOVE    SPACE       TO  S02-SRYYM (5:)
                   ELSE
                       MOVE    SPACE       TO  S02-SRYYM
                       MOVE    SPA-SYSYMD (1:4)
                                           TO  S02-SRYYM (1:4)
                   END-IF
               WHEN    CONST-MODE-NORALL
                       MOVE    SPACE       TO  S02-SRYYM
               WHEN    CONST-MODE-MISYU
                       MOVE    SPACE       TO  S02-SRYYM
               WHEN    CONST-MODE-NYUHEN
                   IF    ( S02-SRYYM (1:8) IS  NUMERIC )
                       CONTINUE
                   ELSE
                       MOVE    SPA-SYSYMD  TO  S02-SRYYM
                   END-IF
               END-EVALUATE
      *
               PERFORM 4104-SRYYM-CHK-SEC
      *
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4106-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順  入力処理
      *****************************************************************
       4106-SORTKBN-CHK-SEC               SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           INITIALIZE                  SPA-GMN-PAGE
      *
           MOVE    S02-SORTKBN (1:1) TO  WRK-CMB-CD
           PERFORM 4200-SORTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM       TO  SPA-GMN-SORTKBN-G
      *
           MOVE    1               TO  FLG-SYULIST-ROW
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4106-SORTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細リストチェック
      *****************************************************************
       4100-SMEILIST-CHK-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SMEILIST-SEL
                                       SPA-GMN-EDANUM
      *
           IF    ( SPA-GMN-TBL-SEL     >   ZERO )
      *
               PERFORM VARYING IDXM    FROM    1   BY  1
                       UNTIL ( IDXM    >   SPA-GMN-SMEILIST-MAX )
      *
                   IF    ( S02-MEILIST-SELECT (IDXM)   =   "T" )
                       MOVE    IDXM    TO  SPA-GMN-SMEILIST-SEL
                                           S02-RIREKIIN
                       PERFORM 4100-EDANUM-CHK-SEC
      *
                       MOVE    SPA-GMN-SMEILIST-MAX
                                       TO  IDXM
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       4100-SMEILIST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細番号チェック処理
      *****************************************************************
       4100-EDANUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-SMEILIST-SEL
      *
           IF    ( SPA-NAI-SYORINO     =   CONST-PROC-RRKSYU )
      *
               INITIALIZE              SPA-GMN-SYUMEI-AREA
                                       SPA-NAI-SYUMEI-AREA
               MOVE    CONST-PROC-RRKSYU
                                       TO  SPA-NAI-SYORINO
      *
               MOVE    SPA-SYSYMD      TO  WRK-SYMD
               PERFORM 802-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUKINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-NYUKINYMD
      *
           END-IF
      *
           MOVE    S02-RIREKIIN        TO  SPA-GMN-EDANUM
      *
           IF    ( SPA-GMN-EDANUM  NOT =   ZERO )
               PERFORM 41001-EDANUM-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-SYORINO =   CONST-PROC-RRKSYU )
                       PERFORM 41001-INPUT-AREA-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    10              TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-EDANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細番号チェック処理
      *****************************************************************
       41001-EDANUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-EDANUM  >=  1 )
            AND  ( SPA-GMN-EDANUM  <=  SPA-GMN-SMEILIST-MAX )
               MOVE   SPA-GMN-EDANUM
                                   TO  SPA-GMN-SMEILIST-SEL
                                       IDXM
           ELSE
               MOVE    13          TO  SPA-GMN-CUR
               MOVE    "3005"      TO  SPA-ERRCD
           END-IF
      *
           .
       41001-EDANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41001-INPUT-AREA-HEN-SEC        SECTION.
      *
           MOVE    SPA-GMN-TMSKYMONEY (SPA-GMN-EDANUM)
                                       TO  SPA-GMN-SKYMONEYIN
                                           SPA-NAI-SKYMONEY-MOTO
      *
           MOVE    SPA-NAI-TMCHOSEI1 (SPA-GMN-EDANUM)
                                       TO  SPA-GMN-CHOSEI1IN
                                           SPA-NAI-CHOSEI1IN-MOTO
      *
           MOVE    SPA-GMN-CHOSEI1IN   TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-CHOSEI1INX
      *
           MOVE    SPA-NAI-TMCHOSEI2 (SPA-GMN-EDANUM)
                                       TO  SPA-GMN-CHOSEI2IN
                                           SPA-NAI-CHOSEI2IN-MOTO
      *
           MOVE    SPA-GMN-CHOSEI2IN   TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-CHOSEI2INX
      *
           COMPUTE SPA-NAI-SKYMONEY
               =   SPA-GMN-SKYMONEYIN
               -   SPA-NAI-CHOSEI1IN-MOTO
               -   SPA-NAI-CHOSEI2IN-MOTO
      *
      *
           MOVE    SPA-GMN-TMNYUKIN   (SPA-GMN-EDANUM)
                                       TO  SPA-GMN-NYUKIN
                                           SPA-NAI-NYUKIN-MOTO
           MOVE    SPA-GMN-NYUKIN      TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-NYUKINX
           MOVE    SPA-GMN-TMSYORIYMD (SPA-GMN-EDANUM)
                                       TO  SPA-GMN-NYUKINYMD
      *
           MOVE    SPA-NAI-TMNYKN-HOHO(SPA-GMN-EDANUM)
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYKN-HOHOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYKN-HOHO-G
      *
           MOVE    SPA-NAI-TMSYORIYMD (SPA-GMN-EDANUM)
                                       TO  SPA-NAI-NYUKINYMD
           MOVE    SPA-GMN-TMDENPEDANUM(SPA-GMN-EDANUM)
                                       TO  SPA-NAI-DENPEDANUM
           MOVE    SPA-NAI-TMSYURRKNUM (SPA-GMN-EDANUM)
                                       TO  SPA-NAI-SYURRKNUM
           MOVE    SPA-NAI-TMSYUEDANUM (SPA-GMN-EDANUM)
                                       TO  SPA-NAI-SYUEDANUM
           MOVE    SPA-NAI-TMSYURRKUPDKBN (SPA-GMN-EDANUM)
                                       TO  SPA-NAI-SYURRKUPDKBN
           MOVE    SPA-NAI-TMJOUTAIKBN(SPA-GMN-EDANUM)
                                       TO  SPA-NAI-DENPJTIKBN
           MOVE    SPA-NAI-TMMINUS(SPA-GMN-EDANUM)
                                       TO  SPA-NAI-MINUS
      *
           MOVE    SPA-NAI-TMPROC-FLG  (SPA-GMN-EDANUM)
                                       TO  SPA-NAI-PROC-FLG
      *
           .
       41001-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       410011-INPUT-AREA-HEN-SEC       SECTION.
      *
           MOVE    SPA-S01KYOUTU       TO  TSPA-S01KYOUTU
      *
           INITIALIZE                  SPA-S01KYOUTU-AREA
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           INITIALIZE                      SSUB03-AREA
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           MOVE    SPA-GMN-SKYPRT      TO  SSUB03-SKYPRT
           MOVE    SPA-GMN-SRYPRT      TO  SSUB03-SRYPRT
           MOVE    1                   TO  SSUB03-REQNO
           CALL    "ORCGSSUB03"        USING
                                       WRK-SPA-AREA
                                       SPA-S01KYOUTU
                                       SSUB03-AREA
      *
           MOVE    SSUB03-SKYPRT       TO  SPA-GMN-SKYPRT
           MOVE    SSUB03-SRYPRT       TO  SPA-GMN-SRYPRT
      *
           MOVE    TSPA-S01KYOUTU      TO  SPA-S01KYOUTU
      *
           PERFORM 4101-SKYPRT-CHK-SEC
           PERFORM 4101-SRYPRT-CHK-SEC
      *
           .
       410011-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    明細番号チェック処理
      *****************************************************************
       4101-EDANUM-CHK-SEC             SECTION.
      *
           PERFORM 41011-EDANUM-CHK-SEC
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
            AND  ( SPA-NAI-SYORINO =   CONST-PROC-RRKSYU )
      *
               INITIALIZE              SPA-GMN-SYUMEI-AREA
                                       SPA-NAI-SYUMEI-AREA
               MOVE    CONST-PROC-RRKSYU
                                   TO  SPA-NAI-SYORINO
      *
               MOVE    SPA-SYSYMD  TO  WRK-SYMD
               PERFORM 802-HIZUKE-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-NYUKINYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-NYUKINYMD
           END-IF
      *
           .
       4101-EDANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細番号チェック処理
      *****************************************************************
       41011-EDANUM-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-SMEILIST-SEL
      *
           IF    ( SPA-GMN-EDANUM      =   ZERO )
               MOVE    13          TO  SPA-GMN-CUR
               MOVE    "3006"      TO  SPA-ERRCD
           ELSE
               PERFORM 41001-EDANUM-CHK-SEC
           END-IF
      *
           .
       41011-EDANUM-CHK-EXT.
           EXIT.
      **************************************************************
      *    戻る　処理
      **************************************************************
       210-BACK                    SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    診療行為より遷移時、診療行為へ
           MOVE    SPACE               TO  LINKAREA
           IF    ( SPA-MOTOPG2         =   "K02" OR "K02N" OR "KT01"
                                       OR  "U02" OR "I01" )
               MOVE    SPA-MOTOPG2     TO  SPA-SAKIPG
               IF    ( SPA-MOTOPG      =   "S08" )
                   CONTINUE
               ELSE
                   MOVE    "S02"       TO  SPA-MOTOPG
               END-IF
               MOVE    SPACE           TO  SPA-MOTOPG2
               MOVE    SPACE           TO  LINKAREA
               MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
           ELSE
               MOVE    "M01"           TO  SPA-SAKIPG
               MOVE    "S02"           TO  SPA-MOTOPG
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
           END-IF
      *
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      **************************************************************
      *    再発行処理
      **************************************************************
       419-SAIHAKKOU-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELBAN      NOT =   ZERO )
               IF    ( SPA-NAI-TJYOTAIKBN (SPA-GMN-SELBAN)
                                       =   "3" )
                   MOVE    "0010"          TO  SPA-ERRCD
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    "2102"      TO  SPA-SIDCD
               END-IF
           END-IF
      *
           .
       419-SAIHAKKOU-EXT.
           EXIT.
      **************************************************************
      *    再発行処理
      **************************************************************
       420-SAIHAKKOU-SEC           SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
            OR   ( SPA-PRTCONF         =   "1" )
               PERFORM 800-CLIENT-PRT-SEC
           END-IF
      *
           IF    ( SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       =   "1" )
               PERFORM 4201-NYU-SAIHAKKOU-SEC
           ELSE
               PERFORM 4201-GAI-SAIHAKKOU-SEC
           END-IF
      *
           PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
           .
       420-SAIHAKKOU-EXT.
           EXIT.
      *
      **************************************************************
      *    再発行処理
      **************************************************************
       421-SAIHAKKOU-SEC           SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
            OR   ( SPA-PRTCONF         =   "1" )
               PERFORM 800-CLIENT-PRT-SEC
           END-IF
      *
           MOVE    SPA-NAI-GYOUMUID
                                   TO      WRK-GYOUMUID
           MOVE    SPA-NAI-UID     TO      WRK-UID
           MOVE    SPA-NAI-FILEMEI TO      WRK-FILEMEI
           PERFORM 42011-GAI-SAIHAKKOU-SEC
      *
           PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
           .
       421-SAIHAKKOU-EXT.
           EXIT.
      **************************************************************
      *    月別請求書発行処理
      **************************************************************
       422-GAI-TUKISKYPRT-SEC              SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
            OR   ( SPA-PRTCONF         =   "1" )
               PERFORM 800-CLIENT-PRT-SEC
           END-IF
      *
           MOVE    SPA-NAI-GYOUMUID
                                   TO      WRK-GYOUMUID
           MOVE    SPA-NAI-UID     TO      WRK-UID
           MOVE    SPA-NAI-FILEMEI TO      WRK-FILEMEI
           PERFORM 49012-GAI-TUKISKYPRT-SEC
      *
           PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
           .
       422-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    入院再発行処理
      **************************************************************
       4201-NYU-SAIHAKKOU-SEC          SECTION.
      *
      *    収納マスタを読み込みし、診療日の取込み
      *    選択された伝票番号から再発行を行う
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF   (  FLG-SYUNOU          =   ZERO )
               MOVE   SPA-AREA         TO  WRK-SPA-AREA
               MOVE   SYU-SRYYMD       TO  WRK-SPA-SRYYMD
               MOVE   SYU-NYUGAIKBN    TO  WRK-SPA-NYUGAIKBN
               MOVE   SPA-GMN-PTID     TO  WRK-SPA-PTID
               MOVE   SPA-GMN-PTNUM    TO  WRK-SPA-PTNUM
               MOVE   SPA-GMN-KANANAME TO  WRK-SPA-KANANAME
               MOVE   SPA-GMN-NAME     TO  WRK-SPA-NAME
      *
      *        労災判定
               IF   ( SPA-NAI-THKNNUM (SPA-GMN-SELBAN)
                                       =   "971" OR "973" )
                   MOVE    1           TO  WRK-SPA-HKNKBN
               ELSE
                   MOVE    ZERO        TO  WRK-SPA-HKNKBN
               END-IF
      *
               MOVE   SPA-NAI-TRSI-HKNKBN (SPA-GMN-SELBAN)
                                       TO  WRK-SPA-RSI-HKNKBN
      *
               PERFORM 4201-NYU-PRINT-PG-SEC
      *
               INITIALIZE      ORCHCN03AREA
      *
               MOVE    WRK-SPA-HOSPNUM  TO  ORCHCN03-HEAD-HOSPNUM
               MOVE    WRK-SPA-STAFFCD TO  ORCHCN03-STAFFCD
               MOVE    WRK-SPA-SYSYMD  TO  ORCHCN03-SYSYMD
               MOVE    WRK-SPA-TERMID  TO  ORCHCN03-TERMID
               MOVE    1               TO  ORCHCN03-CNT
      *
               MOVE    WRK-SPA-HOSPNUM  TO  ORCHCN03-HOSPNUM (1)
               MOVE    WRK-SPA-PTID    TO  ORCHCN03-PTID   (1)
               MOVE    WRK-SPA-PTNUM   TO  ORCHCN03-PTNUM  (1)
               MOVE    "1"             TO  ORCHCN03-NYUGAIKBN (1)
      *
               MOVE    SYU-SKYSTYMD    TO  ORCHCN03-KJNYMD  (1)
               MOVE    SYU-DENPNUM     TO  ORCHCN03-DENPNUM (1)
               MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  ORCHCN03-SEIKYU-TAX-SAI
                                                                (1)
               MOVE    SYU-SKYMONEY    TO  ORCHCN03-SEIKYU  (1)
               MOVE    SYU-NYUKIN-TOTAL
                                       TO  ORCHCN03-NYUKIN  (1)
               MOVE    1               TO  ORCHCN03-HAKKOFLG(1)
               MOVE    1               TO  ORCHCN03-MOTOID
      *
               IF    ( FLG-SEIKYU-PRT  =   1 )
                   MOVE    "03"            TO      WRK-SPA-PRT-FLG
                   CALL    WRK-SEIKYU-PG   USING   ORCHCN03AREA
                                                   WRK-SPA-AREA
                   MOVE    SPACE           TO  WRK-SPA-PRT-FLG
               END-IF
      *
               IF    ( FLG-MEISAI-PRT  =   1 )
                   PERFORM 42011-NYU-MEISAI-PRT-SEC
               END-IF
      *
           END-IF
      *
           .
       4201-NYU-SAIHAKKOU-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書出力処理
      *****************************************************************
       42011-NYU-MEISAI-PRT-SEC        SECTION.
      *
           INITIALIZE          ORCHCN03S02AREA
           MOVE    1       TO  ORCHCN03S02-IDX
           CALL    "ORCHCN03S02"   USING
                                   ORCHCN03AREA
                                   ORCHCN03S02AREA
                                   SYUNOU-REC
                                   WRK-SPA-AREA
           IF    ( ORCHCN03S02-PRTFLG  =   1 )
      *
               INITIALIZE                  ORCHCN04AREA
      *
               MOVE    WRK-SPA-PTID    TO  ORCHCN04-PTID
               MOVE    WRK-SPA-PTNUM   TO  ORCHCN04-PTNUM
               MOVE    WRK-SPA-NAME    TO  ORCHCN04-NAME
               MOVE    SYU-SKYSTYMD    TO  ORCHCN04-SKYSTYMD
               MOVE    SYU-SKYEDYMD    TO  ORCHCN04-SKYEDYMD
               MOVE    1               TO  ORCHCN04-HAKKOFLG
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   ORCHCN03S02-GRP-CNT )
      *
                   MOVE    ORCHCN03S02-GRP-DENPNUM (IDX0)
                                       TO  ORCHCN04-DENPNUM (IDX0)
      *
               END-PERFORM
      *
               MOVE    "04"            TO  WRK-SPA-PRT-FLG
               CALL    WRK-MEISAI-PG    USING
                                        WRK-SPA-AREA
                                        ORCHCN04AREA
               MOVE    SPACE           TO  WRK-SPA-PRT-FLG
           END-IF
      *
           .
       42011-NYU-MEISAI-PRT-EXT.
           EXIT.
      **************************************************************
      *    外来再発行処理
      **************************************************************
       4201-GAI-SAIHAKKOU-SEC          SECTION.
      *
           MOVE    "S02"           TO  WRK-GYOUMUID
                                       SPA-NAI-GYOUMUID
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID           TO  WRK-UID
                                       SPA-NAI-UID
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD    TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS    TO  WRK-FILEMEI-HMS
           MOVE    WRK-FILEMEI     TO  SPA-NAI-FILEMEI
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4201-GAI-SAIHAKKOU-EXT
           END-IF
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    "2"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  WRK-SPA-KANANAME
      *
           INITIALIZE                  SPA-TUKI-PARA-G
           INITIALIZE                  SPA-S01KYOUTU-AREA
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           CALL    "ORCGSSUB01"        USING
                                       WRK-SPA-AREA
                                       SPA-S01KYOUTU
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 42011-GAI-SAIHAKKOU-SEC
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
      *
           .
       4201-GAI-SAIHAKKOU-EXT.
           EXIT.
      **************************************************************
      *    外来再発行処理
      **************************************************************
       42011-GAI-SAIHAKKOU-SEC          SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    "2"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  WRK-SPA-KANANAME
           MOVE    SPA-S01-SRYYMD      TO  WRK-SPA-SRYYMD
      *
      *    帳票プログラム取得（外来）
           PERFORM 4201-PRINT-PG-SEC
      *
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
      *
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
           IF    ( FLG-SEIKYU-PRT      =   1 )
               PERFORM 42011-GAI-SEIKYU-PRT-SEC
           END-IF
      *
           IF    ( FLG-MEISAI-PRT      =   1 )
               PERFORM 42011-GAI-MEISAI-PRT-SEC
           END-IF
      *
           .
       42011-GAI-SAIHAKKOU-EXT.
           EXIT.
      **************************************************************
      *    外来請求書兼領収書再発行処理
      **************************************************************
       42011-GAI-SEIKYU-PRT-SEC    SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    SPA-S01-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    SYUPARA01-SEIKYU    TO  ORCHC03-SEIKYU
           MOVE    SYUPARA01-NYUKIN    TO  ORCHC03-NYUKIN
           MOVE    1                   TO  ORCHC03-HAKKOFLG
           MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC03-HAKHOUFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
           MOVE    SYUPARA01-ZENMISYU  TO  ORCHC03-ZENMISYU
           MOVE    SYUPARA01-ZENMISYU-2 TO ORCHC03-ZENMISYU-2
           MOVE    SYUPARA01-HENKIN    TO  ORCHC03-HENKIN
           MOVE    SYUPARA01-KANYUKIN  TO  ORCHC03-KANYUKIN
           MOVE    WRK-GYOUMUID        TO  ORCHC03-PARA-GYOUMUID
           MOVE    WRK-UID             TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    1                   TO  ORCHC03-MOTOID
           MOVE    "01"                TO  WRK-SPA-PRT-FLG
           CALL    WRK-SEIKYU-PG       USING
                                           WRK-SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  WRK-SPA-PRT-FLG
      *
           .
       42011-GAI-SEIKYU-PRT-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書サブ呼出処理
      *****************************************************************
       42011-GAI-MEISAI-PRT-SEC        SECTION.
      *
           MOVE    ZERO                    TO  WRK-PAGE-CNT
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC   TO  SYUPARA03-AREA
      *
               INITIALIZE                  ORCHC04AREA
               MOVE    SYUPARA03-DENPNUM
                                       TO  ORCHC04-DENPNUM
               MOVE    1               TO  ORCHC04-HAKKOFLG
               MOVE    SYUPARA01-SKYSUMFLG
                                       TO  ORCHC04-HAKHOUFLG
               MOVE    SYUPARA03-STSRYYMD  TO  ORCHC04-KIKAN-STYMD
               MOVE    SYUPARA03-EDSRYYMD  TO  ORCHC04-KIKAN-EDYMD
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                           TO  ORCHC04-KIKAN-DENPNUM
                                                               (IDXD)
               END-PERFORM
               MOVE    WRK-PAGE-CNT
                                       TO  ORCHC04-PAGE-CNT
      *
               MOVE    "02"        TO  WRK-SPA-PRT-FLG
               CALL    WRK-MEISAI-PG       USING
                                           WRK-SPA-AREA
                                           ORCHC04AREA
               MOVE    SPACE       TO  WRK-SPA-PRT-FLG
               MOVE    ORCHC04-PAGE-CNT
                                   TO  WRK-PAGE-CNT
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       42011-GAI-MEISAI-PRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       490-GAI-TUKISKYPRT-SEC          SECTION.
      *
           IF     ( SPA-GMN-PTID   =   ZERO )
               MOVE    "0071"      TO  SPA-ERRCD
           ELSE
      *
               MOVE    SPACE       TO  SPA-TUKI-SRYYM
      *
               IF    ( SPA-NAI-SRYYM (1:6) IS  NUMERIC )
                   MOVE    SPA-NAI-SRYYM
                                   TO  SPA-TUKI-SRYYM
               END-IF
               IF    ( SPA-GMN-TBL-SEL NOT =   ZERO )
                   MOVE    SPA-NAI-TSRYYMD (SPA-GMN-TBL-SEL)(1:6)
                                   TO  SPA-TUKI-SRYYM
               END-IF
      *
               INITIALIZE              SPA-S01KYOUTU-AREA
      *
               PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
               MOVE    SPA-S01KYOUTU   TO  SPA-WORK
               MOVE    SPA-AREA        TO  SPA-COMMON
               MOVE    SPA-S02         TO  SPA-FREE1
      *
               MOVE    "LINK"          TO  MCP-STATUS
               MOVE    SPACE           TO  MCP-EVENT
      *
               CALL    "ORCGS07"       USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-WORK        TO  SPA-S01KYOUTU
               MOVE    SPA-FREE1       TO  SPA-S02
      *
               MOVE    "S02"           TO  SPA-MOTOPG
               MOVE    "S07"           TO  SPA-SAKIPG
      *
               MOVE    "NEW"           TO  MCP-PUTTYPE
               MOVE    "S07    "       TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       490-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    収納患者一覧画面遷移処理
      **************************************************************
       412-S10-SENI-SEC            SECTION.
      *
           MOVE    SPACE       TO  SPA-TUKI-SRYYM
      *
           IF    ( SPA-NAI-SRYYM (1:6) IS  NUMERIC )
               MOVE    SPA-NAI-SRYYM
                               TO  SPA-TUKI-SRYYM
           END-IF
           IF    ( SPA-TUKI-SRYYM  =   SPACE )
               MOVE    SPA-SYSYMD(1:6)
                               TO  SPA-TUKI-SRYYM
           END-IF
      *
           INITIALIZE              SPA-S01KYOUTU-AREA
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
           MOVE    SPA-TUKI-SRYYM
                               TO  SPA-S01-TUKI-SRYYM
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S02         TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGS10"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE        TO  SPA-S02
      *
           MOVE    "S02"           TO  SPA-MOTOPG
           MOVE    "S10"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "S10"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       412-S10-SENI-EXT.
           EXIT.
      **************************************************************
      *    請求年月チェック処理
      **************************************************************
       4901-SKYYM-CHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-SKYYM
      *
           IF    ( SPA-GMN-PTID    NOT =   ZERO )
            AND  ( SPA-GMN-MAX     >   ZERO )
               MOVE    SPACE           TO  WRK-SYMD
               IF    ( SPA-NAI-SRYYM (1:6)     IS  NUMERIC )
                   MOVE    SPA-NAI-SRYYM
                                       TO  WRK-SKYYM
               END-IF
               IF    ( SPA-GMN-TBL-SEL NOT =   ZERO )
                   MOVE    SPA-NAI-TSRYYMD (SPA-GMN-TBL-SEL)(1:6)
                                       TO  WRK-SKYYM
               END-IF
           END-IF
      *
           .
       4901-SKYYM-CHK-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       4901-GAI-TUKISKYPRT-SEC         SECTION.
      *
           MOVE    "S02"           TO  WRK-GYOUMUID
                                       SPA-NAI-GYOUMUID
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID           TO  WRK-UID
                                       SPA-NAI-UID
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD    TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS    TO  WRK-FILEMEI-HMS
           MOVE    WRK-FILEMEI     TO  SPA-NAI-FILEMEI
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4901-GAI-TUKISKYPRT-EXT
           END-IF
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    "2"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  WRK-SPA-KANANAME
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
      *
           INITIALIZE                  SPA-S01KYOUTU-AREA
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           CALL    "ORCGSSUB01"        USING
                                       WRK-SPA-AREA
                                       SPA-S01KYOUTU
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPA-S01-TUKI-SKYPRT
                                       TO  FLG-SEIKYU-PRT
               MOVE    SPA-S01-TUKI-SRYPRT
                                       TO  FLG-MEISAI-PRT
               PERFORM 422-GAI-TUKISKYPRT-SEC
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
      *
           .
       4901-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       49012-GAI-TUKISKYPRT-SEC         SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    "2"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  WRK-SPA-KANANAME
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-S01-TUKI-DENPNUM
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           MOVE    SYU-SRYYMD          TO  WRK-SPA-SRYYMD
      *
      *    帳票プログラム取得（外来）
           PERFORM 4201-PRINT-PG-SEC
      *
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
      *
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
           IF    ( FLG-SEIKYU-PRT      =   "1" )
               PERFORM 49011-GAI-TUKISKYPRT-SEC
           END-IF
      *
           IF    ( FLG-MEISAI-PRT      =   "1" )
               PERFORM 49011-GAI-TUKIMEISAI-PRT-SEC
           END-IF
      *
           INITIALIZE                  SPA-TUKI-PARA-G
      *
           .
       49012-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       49011-GAI-TUKISKYPRT-SEC        SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    SPA-S01-TUKI-DENPNUM
                                       TO  ORCHC03-DENPNUM
           MOVE    SYUPARA01-SEIKYU    TO  ORCHC03-SEIKYU
           MOVE    SYUPARA01-NYUKIN    TO  ORCHC03-NYUKIN
           MOVE    4                   TO  ORCHC03-HAKKOFLG
           MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC03-HAKHOUFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
           MOVE    SYUPARA01-ZENMISYU  TO  ORCHC03-ZENMISYU
           MOVE    SYUPARA01-ZENMISYU-2 TO ORCHC03-ZENMISYU-2
           MOVE    SYUPARA01-HENKIN    TO  ORCHC03-HENKIN
           MOVE    SYUPARA01-KANYUKIN  TO  ORCHC03-KANYUKIN
           MOVE    WRK-GYOUMUID        TO  ORCHC03-PARA-GYOUMUID
           MOVE    WRK-UID             TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    2                   TO  ORCHC03-MOTOID
           MOVE    "01"                TO  WRK-SPA-PRT-FLG
      *
           CALL    WRK-SEIKYU-PG           USING
                                           WRK-SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  WRK-SPA-PRT-FLG
      *
           .
       49011-GAI-TUKISKYPRT-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書サブ呼出処理
      *****************************************************************
       49011-GAI-TUKIMEISAI-PRT-SEC    SECTION.
      *
           MOVE    ZERO                    TO  WRK-PAGE-CNT
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
      *
               INITIALIZE                  ORCHC04AREA
               MOVE    SYUPARA03-DENPNUM   TO  ORCHC04-DENPNUM
               MOVE    4                   TO  ORCHC04-HAKKOFLG
               MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC04-HAKHOUFLG
               MOVE    SPA-SYSYMD          TO  ORCHC04-DENPPRTYMD
               MOVE    SYUPARA03-STSRYYMD  TO  ORCHC04-KIKAN-STYMD
               MOVE    SYUPARA03-EDSRYYMD  TO  ORCHC04-KIKAN-EDYMD
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                           TO  ORCHC04-KIKAN-DENPNUM
                                                               (IDXD)
               END-PERFORM
      *
               MOVE    WRK-PAGE-CNT    TO  ORCHC04-PAGE-CNT
               MOVE    "02"            TO  WRK-SPA-PRT-FLG
               CALL    WRK-MEISAI-PG       USING
                                           WRK-SPA-AREA
                                           ORCHC04AREA
               MOVE    SPACE           TO  WRK-SPA-PRT-FLG
               MOVE    ORCHC04-PAGE-CNT
                                       TO  WRK-PAGE-CNT
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       49011-GAI-TUKIMEISAI-PRT-EXT.
           EXIT.
      *************************************************************
      *    パラメタ削除処理
      *************************************************************
       4801-DELETE-PARA-SEC            SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    WRK-GYOUMUID        TO  PARA-GYOUMUID
           MOVE    WRK-UID             TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4000"          TO  SPA-ERRCD
           END-IF
      *
           .
       4801-DELETE-PARA-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       4201-PRINT-PG-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-SEIKYU-PG
           MOVE    SPACE               TO  WRK-MEISAI-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHC03"           TO  WRK-SEIKYU-PG
           END-IF
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-MEISAI-PG
           ELSE
              MOVE    "ORCHC04"           TO  WRK-MEISAI-PG
           END-IF
      *
           .
      *
       4201-PRINT-PG-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票プログラム取得処理 （入院）
      *****************************************************************
       4201-NYU-PRINT-PG-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SEIKYU-PG
           MOVE    SPACE               TO  WRK-MEISAI-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHCN03"          TO  WRK-SEIKYU-PG
           END-IF
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-MEISAI-PG
           ELSE
              MOVE    "ORCHCN04"       TO  WRK-MEISAI-PG
           END-IF
      *
           .
      *
       4201-NYU-PRINT-PG-EXT.
           EXIT.
      *
      **************************************************************
      *    請求取消（出産一時金）処理
      **************************************************************
       430-TORIKESI-SEC                SECTION.
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               CONTINUE
           ELSE
      *
      *        外来分の請求取消は不可
               IF    ( SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       =   "2" )
                   MOVE    SPA-GMN-SELBAN
                                       TO  IDXB
                   PERFORM 800-BUNBEN-CHK-SEC
                   IF    ( FLG-BUNBENCHK   =   1 )
                       MOVE    SPA-GMN-PTID
                                       TO  SPA-PTID
                       MOVE    SPA-GMN-PTNUM
                                       TO  SPA-PTNUM
                       INITIALIZE              SPA-S01KYOUTU-AREA
                       PERFORM 4901-SPAKYOTUKEY-SET-SEC
                       MOVE    "2"     TO  SPA-S08-NYUGAIKBN
                       MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) 
                                       TO  SPA-S08-RRKNUM
                       MOVE    "S02"   TO  SPA-S08-MOTOPG
                       PERFORM 480-S08-SENI-SEC
                   ELSE
                       MOVE    "0119"  TO  SPA-ERRCD
                   END-IF
               ELSE
      *
      *            請求取消データの取消不可
                   IF    ( SPA-NAI-TJYOTAIKBN (SPA-GMN-SELBAN)
                                       =   "3" )
                       MOVE    "0119"  TO  SPA-ERRCD
                   END-IF
      *
                   IF    ( SPA-ERRCD   =   SPACE )
                       MOVE    "2001"  TO  SPA-SIDCD
                   END-IF
               END-IF
      *
           END-IF
      *
           .
      *
       430-TORIKESI-EXT.
           EXIT.
      **************************************************************
      *    請求取消更新処理
      **************************************************************
       430-TORIKESI-KOUSHIN-SEC        SECTION.
      *
           INITIALIZE                  TSYU-REC
           INITIALIZE                  TSMEI-REC
           INITIALIZE                  SS016-AREA
           MOVE    CONST-REQMODE-SKY-TORIKESHI
                                   TO  SS016-REQMODE
           MOVE    SPA-GMN-PTID    TO  SS016-PTID
           MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                   TO  SS016-DENPNUM
           CALL    "ORCSS016"          USING
                                       SS016-AREA
                                       TSYU-REC
                                       TSMEI-REC
                                       SPA-AREA
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 3101-MISYU-EDIT-SEC
      *
               MOVE    2               TO  FLG-MEILIST-ROW
               PERFORM 300-SYUNOU-HENSYU-SEC
      *
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       430-TORIKESI-KOUSHIN-EXT.
           EXIT.
      *
      **************************************************************
      *    再計算処理
      **************************************************************
       440-SAIKEI-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      440-SAIKEI-EXT
           END-IF
      *
           INITIALIZE                  SSUB05-AREA
           MOVE     "1"            TO  SSUB05-SYORIKBN
           MOVE     SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                   TO  SSUB05-NYUGAIKBN
           MOVE     SPA-GMN-PTID   TO  SSUB05-PTID
           MOVE     SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                   TO  SSUB05-DENPNUM
           MOVE     SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                   TO  SSUB05-SRYKA
           MOVE     SPA-NAI-TSRYYMD (SPA-GMN-SELBAN)(1:6)
                                   TO  SSUB05-SKYYM
           CALL    "ORCGSSUB05"    USING
                                   SSUB05-AREA
                                   SPA-AREA
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 3101-MISYU-EDIT-SEC
      *
               MOVE    2                   TO  FLG-MEILIST-ROW
               PERFORM 300-SYUNOU-HENSYU-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       440-SAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    外来分一括再計算チェック処理
      **************************************************************
       440-IKATUSAIKEI-CHK-SEC       SECTION.
      *
           PERFORM 4901-SKYYM-CHK-SEC
           IF    ( WRK-SKYYM           =   SPACE )
               MOVE    "0060"          TO  SPA-ERRCD
           ELSE
      *
               MOVE    ZERO                TO  FLG-HIT
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >   SPA-GMN-MAX )
                        OR   ( FLG-HIT =   1 )
      *
                   IF    ( SPA-NAI-TSRYYMD (IDX3)(1:6) =   WRK-SKYYM )
                    AND  ( SPA-NAI-TNYUGAIKBN (IDX3)   =   "2" )
                       MOVE    1           TO  FLG-HIT
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-HIT         =   ZERO )
                   MOVE    "0060"          TO  SPA-ERRCD
               ELSE
                   MOVE    "2002"          TO  SPA-SIDCD
               END-IF
      *
           END-IF
      *
           .
      *
       440-IKATUSAIKEI-CHK-EXT.
           EXIT.
      **************************************************************
      *    外来分一括再計算処理
      **************************************************************
       440-IKATUSAIKEI-SEC           SECTION.
      *
           PERFORM 4901-SKYYM-CHK-SEC
      *
           INITIALIZE                      SSUB05-AREA
           MOVE    "2"                 TO  SSUB05-SYORIKBN
           MOVE    SPA-GMN-PTID        TO  SSUB05-PTID
           MOVE    WRK-SKYYM           TO  SSUB05-SKYYM
           CALL    "ORCGSSUB05"    USING
                                   SSUB05-AREA
                                   SPA-AREA
      *
           IF    ( SPA-ERRCD      NOT =   "0070" )
               PERFORM 4401-IKATUSAIKEI-SEC
           ELSE
               MOVE    "4009"         TO  SPA-ERRCD
           END-IF
      *
      *
           .
       440-IKATUSAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    外来分一括再計算処理
      **************************************************************
       4401-IKATUSAIKEI-SEC           SECTION.
      *
      *****PERFORM    310-SPA-SET-SEC.
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           MOVE    2                   TO  FLG-MEILIST-ROW
           PERFORM 300-SYUNOU-HENSYU-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       4401-IKATUSAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    返金処理
      **************************************************************
       450-HENKIN-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      450-HENKIN-EXT
           END-IF
      *
      *    収納マスタの請求金額と入金合計額を比較して
      *    入金合計額が多ければ、収納マスタの入金合計額
      *    を請求金額と同じにして、収納明細マスタを作成する
      *
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC                            
      *
           IF      FLG-SYUNOU          =   ZERO
             IF    SYU-SKYMONEY        <  SYU-NYUKIN-TOTAL
                MOVE    ZERO                TO    WRK-SAGAKU
                COMPUTE WRK-SAGAKU = SYU-SKYMONEY - SYU-NYUKIN-TOTAL
                MOVE    SYU-SKYMONEY        TO    SYU-NYUKIN-TOTAL
      *
                MOVE    SPA-OPID            TO  SYU-OPID
                PERFORM 800-MCNDATE-SEC
                MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                MOVE    SYUNOU-REC          TO  MCPDATA-REC
                MOVE    "tbl_syunou"        TO  MCP-TABLE
                MOVE    "key"               TO  MCP-PATHNAME
                PERFORM 910-DBUPDATE-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1002"              TO  SPA-ERRCD
                   GO  TO  450-HENKIN-EXT
                END-IF     
      *
                PERFORM 900-SYUMEI-KEY7-SEL-SEC
                COMPUTE IDX1   =   SMEI-DENPEDANUM +  1
      *
                PERFORM    4501-SMEI-SAKUSEI-SEC
             ELSE
                MOVE    "0004"     TO    SPA-ERRCD
             END-IF
           END-IF
     *
           IF      SPA-ERRCD          =   SPACE 
     ******    PERFORM    310-SPA-SET-SEC
      *
               PERFORM 3101-MISYU-EDIT-SEC
      *
               MOVE    2                   TO  FLG-MEILIST-ROW
               PERFORM 300-SYUNOU-HENSYU-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       450-HENKIN-EXT.
           EXIT.
      *
      *************************************************************
      *    返金処理（収納明細レコード作成）
      *************************************************************
       4501-SMEI-SAKUSEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
      *****MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
      *伝票番号
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) (1:7)
                                       TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX1                 TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                       TO  SMEI-SRYKA
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
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *入返金区分
           MOVE    "2"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    WRK-SAGAKU          TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "4"                 TO  SMEI-JOUTAIKBN
      *
           MOVE    SPA-OPID            TO  SMEI-OPID
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
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "4001"              TO  SPA-ERRCD
           END-IF     
           .
       4501-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      *************************************************************
      *    入金処理
      *************************************************************
       460-NYUKIN-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               CONTINUE
           ELSE
               IF    ( SPA-NAI-TJYOTAIKBN (SPA-GMN-SELBAN) = "3" )
                   MOVE    "0006"      TO  SPA-ERRCD
               END-IF
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 4601-NYUKIN-SEC
               END-IF
           END-IF
      *
           .
       460-NYUKIN-EXT.
           EXIT.
      *************************************************************
      *    入金処理
      *************************************************************
       4601-NYUKIN-SEC                 SECTION.
      *
           INITIALIZE                  SPA-GMN-SYUMEI-AREA
                                       SPA-NAI-SYUMEI-AREA
                                       SPA-GMN-EDANUM
                                       SPA-GMN-SMEILIST-SEL
      *
           MOVE    CONST-PROC-NYUKIN
                               TO  SPA-NAI-SYORINO
      *
           MOVE    SPA-NAI-NYUKIN-HOHO
                               TO  WRK-CMB-CD
      *
           PERFORM 4200-NYKN-HOHOLST-CHK-SEC
           IF    ( WRK-CMB-ITM =   SPACE )
               MOVE    SPA-GMN-NYKN-HOHOLST (1)
                               TO  SPA-GMN-NYKN-HOHO-G
           ELSE
               MOVE    WRK-CMB-ITM
                               TO  SPA-GMN-NYKN-HOHO-G
           END-IF
      *
           MOVE    SPA-SYSYMD  TO  WRK-SYMD
           PERFORM 802-HIZUKE-SEC
           MOVE    WRK-HENYMDG TO  SPA-GMN-NYUKINYMD
           MOVE    WRK-SYMD    TO  SPA-NAI-NYUKINYMD
      *
           MOVE    8           TO  SPA-GMN-CUR
      *
           .
       4601-NYUKIN-EXT.
           EXIT.
      *************************************************************
      *    一括入金処理
      *************************************************************
       460-IKKATU-NYUKIN-SEC           SECTION.
      *
           IF    ( SPA-GMN-PTID        =   ZERO )
               MOVE    "0071"          TO  SPA-ERRCD
               GO  TO  460-IKKATU-NYUKIN-EXT
           END-IF
      *
           INITIALIZE                  SPA-S01KYOUTU-AREA
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
      *
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "S06"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "S06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       460-IKKATU-NYUKIN-EXT.
           EXIT.
      *
      *************************************************************
      *    入金取消
      *************************************************************
       470-NYUDEL-SEC                  SECTION.
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               CONTINUE
           ELSE
               MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
               MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
               MOVE    SPA-GMN-PTID        TO  SYU-PTID
               MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
               PERFORM 900-SYUNOU-KEY-SEL-SEC
               IF    ( FLG-SYUNOU      =   ZERO  )
                   IF ( SYU-NYUKIN-TOTAL   <=   ZERO  )
                       MOVE    "0002"  TO      SPA-ERRCD
                   END-IF
                   IF    ( SPA-ERRCD   =   SPACE )
                       MOVE    ZERO    TO  WRK-NYUKIN
                       PERFORM 900-SYUMEI-KEY2-SEL-SEC
      *
                       PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
      *
                           IF    ( SPA-SYSYMD  =   SMEI-NYUHEN-YMD )
                               CONTINUE
                           ELSE
                               COMPUTE WRK-NYUKIN
                                   =   WRK-NYUKIN
                                   +   SMEI-NYUHEN-MONEY
                           END-IF
      *
                           PERFORM 900-SYUMEI-KEY2-FET-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_syumei"    TO  MCP-TABLE
                       MOVE    "key2"          TO  MCP-PATHNAME
                       PERFORM 910-DBCLOSECURSOR-SEC
      *
                       IF    ( WRK-NYUKIN      =   ZERO )
                           PERFORM 4701-NYUDEL-SEC
                       ELSE
                           MOVE    "2101"      TO  SPA-SIDCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       470-NYUDEL-EXT.
           EXIT.
      *
      *************************************************************
      *    入金取消
      *************************************************************
       4701-NYUDEL-SEC           SECTION.
      *
      *    収納マスタと収納明細の入金額を０円に変更する
      *
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *     
           IF      FLG-SYUNOU          =   1
                   CONTINUE
           ELSE
             IF ( SYU-NYUKIN-TOTAL   <=    ZERO    )
                  MOVE    "0002"     TO    SPA-ERRCD
             ELSE
                 IF    (  SYU-DENPJTIKBN   =       "3" )
                      CONTINUE
                 ELSE
                   IF    ( SYU-SKYMONEY    =   ZERO )
                       MOVE    SPACE TO    SYU-DENPJTIKBN
                   ELSE
                       MOVE    "2"   TO    SYU-DENPJTIKBN
                   END-IF
                 END-IF
                  MOVE    SYU-NYUKIN-TOTAL
                                     TO    WRK-SAGAKU
                  MOVE    ZERO       TO    SYU-NYUKIN-TOTAL
      ***         MOVE    ZERO       TO    SYU-NYUKIN-KAISU
      *
                  MOVE    SPA-OPID            TO  SYU-OPID
                  PERFORM 800-MCNDATE-SEC
                  MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                  MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                  MOVE    SYUNOU-REC          TO  MCPDATA-REC
                  MOVE    "tbl_syunou"        TO  MCP-TABLE
                  MOVE    "key"               TO  MCP-PATHNAME
                  PERFORM 910-DBUPDATE-SEC
                  PERFORM 47011-SYUMEI-NYUDEL-SEC
             END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 3101-MISYU-EDIT-SEC
      *
               MOVE    2               TO  FLG-MEILIST-ROW
               PERFORM 300-SYUNOU-HENSYU-SEC
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4701-NYUDEL-EXT.
           EXIT.
      *************************************************************
      *    入金取消 （収納明細）
      *************************************************************
       47011-SYUMEI-NYUDEL-SEC         SECTION.
      *
           INITIALIZE                TNYUKIN-AREA
                                     THOHO-AREA
      *
           PERFORM 900-SYUMEI-KEY2-SEL-SEC
      *
           MOVE    ZERO          TO  TNYUKIN-MAX
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( IDXM           >=   CONST-EDANUM-MAX )
      *
               IF    ( SMEI-NYUHEN-MONEY   NOT = ZERO )
      *
                   COMPUTE TNYUKIN-MAX =   TNYUKIN-MAX     +   1
                   MOVE    SMEI-NYUHEN-MONEY
                                       TO  TNYUKIN-NYUKIN (TNYUKIN-MAX)
                   IF    ( SMEI-NYUKIN-HOHO    =   SPACE )
                       MOVE    ZERO    TO  TNYUKIN-HOHO   (TNYUKIN-MAX)
                   ELSE
                       MOVE    SMEI-NYUKIN-HOHO
                                       TO  TNYUKIN-HOHO   (TNYUKIN-MAX)
                   END-IF
               END-IF
      *
               PERFORM 900-SYUMEI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDXM    FROM    1   BY  1
                   UNTIL ( IDXM    >   TNYUKIN-MAX )
      *
               IF    ( TNYUKIN-NYUKIN (IDXM)   <   ZERO )
                   COMPUTE IDXST   =   IDXM    -   1
                   PERFORM VARYING IDX0    FROM    IDXST   BY  -1
                           UNTIL ( IDX0    <   1 )
                            OR   ( TNYUKIN-NYUKIN (IDXM)   =   ZERO )
                       IF    ( TNYUKIN-HOHO (IDX0)
                                           =   TNYUKIN-HOHO (IDXM))
                        AND  ( TNYUKIN-NYUKIN (IDX0)       >   ZERO )
                           COMPUTE WRK-NYUKIN
                               =   TNYUKIN-NYUKIN (IDX0)
                               +   TNYUKIN-NYUKIN (IDXM)
                           IF    ( WRK-NYUKIN      >   ZERO )
                               MOVE    WRK-NYUKIN
                                               TO  TNYUKIN-NYUKIN (IDX0)
                               MOVE    ZERO    TO  TNYUKIN-NYUKIN (IDXM)
                           ELSE
                               MOVE    ZERO    TO  TNYUKIN-NYUKIN (IDX0)
                               MOVE    WRK-NYUKIN
                                               TO  TNYUKIN-NYUKIN (IDXM)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXM    FROM    1   BY  1
                   UNTIL ( IDXM    >   TNYUKIN-MAX )
      *
               IF    ( TNYUKIN-NYUKIN (IDXM)   <   ZERO )
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   TNYUKIN-MAX )
                            OR   ( TNYUKIN-NYUKIN (IDXM)   =   ZERO )
      *
                       IF    ( IDX0                    NOT =   IDXM )
                         AND ( TNYUKIN-NYUKIN (IDX0)       >   ZERO )
                           COMPUTE WRK-NYUKIN
                               =   TNYUKIN-NYUKIN (IDX0)
                               +   TNYUKIN-NYUKIN (IDXM)
                           IF    ( WRK-NYUKIN      >   ZERO )
                               MOVE    WRK-NYUKIN
                                               TO  TNYUKIN-NYUKIN (IDX0)
                               MOVE    ZERO    TO  TNYUKIN-NYUKIN (IDXM)
                           ELSE
                               MOVE    ZERO    TO  TNYUKIN-NYUKIN (IDX0)
                               MOVE    WRK-NYUKIN
                                               TO  TNYUKIN-NYUKIN (IDXM)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXM    FROM    TNYUKIN-MAX BY  -1
                   UNTIL ( IDXM    <   1 )
                    OR   ( WRK-SAGAKU  =   ZERO )
      *
             IF    ( TNYUKIN-NYUKIN (IDXM) >   ZERO )
               PERFORM VARYING IDXH    FROM    1   BY  1
                       UNTIL ( IDXH    >   CONST-EDANUM-MAX )
                   IF    ( THOHO-HOHO (IDXH)   =   SPACE )
                    OR   ( THOHO-HOHO (IDXH)   =   TNYUKIN-HOHO(IDXM))
                       IF    ( THOHO-HOHO (IDXH)   =   SPACE )
                           MOVE    IDXH    TO  THOHO-MAX
                       END-IF
                       MOVE    TNYUKIN-HOHO (IDXM)
                                       TO  THOHO-HOHO(IDXH)
                       IF    ( WRK-SAGAKU  >   TNYUKIN-NYUKIN (IDXM))
                           COMPUTE THOHO-NYUKIN(IDXH)
                               =   THOHO-NYUKIN(IDXH)
                               +   TNYUKIN-NYUKIN (IDXM)
                           COMPUTE WRK-SAGAKU
                               =   WRK-SAGAKU
                               -   TNYUKIN-NYUKIN (IDXM)
                           COMPUTE TNYUKIN-NYUKIN (IDXM)
                               =   0
                       ELSE
                           COMPUTE THOHO-NYUKIN(IDXH)
                               =   THOHO-NYUKIN(IDXH)
                               +   WRK-SAGAKU
                           COMPUTE TNYUKIN-NYUKIN (IDXM)
                               =   TNYUKIN-NYUKIN (IDXM)
                               -   WRK-SAGAKU
                           COMPUTE WRK-SAGAKU
                               =   0
                       END-IF
                       MOVE    CONST-EDANUM-MAX
                                           TO  IDXH
                   END-IF
               END-PERFORM
             END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-SAGAKU      >   ZERO )
               PERFORM VARYING IDXH    FROM    1   BY  1
                       UNTIL ( IDXH    >   CONST-EDANUM-MAX )
                   IF    ( THOHO-HOHO (IDXH)   =   SPACE )
                    OR   ( THOHO-HOHO (IDXH)   =   "00" )
                       MOVE    "00"    TO  THOHO-HOHO(IDXH)
                       COMPUTE THOHO-NYUKIN(IDXH)
                           =   THOHO-NYUKIN(IDXH)
                           +   WRK-SAGAKU
                       MOVE    CONST-EDANUM-MAX    TO  IDXH
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 900-SYUMEI-KEY7-SEL-SEC
      *
           COMPUTE IDX1 =   SMEI-DENPEDANUM
      *
           PERFORM VARYING IDXH    FROM    1   BY  1
                   UNTIL ( IDXH    >   THOHO-MAX )
      *
               COMPUTE IDX1 = IDX1 +   1
      *
               PERFORM    4701-SMEI-SAKUSEI-SEC
      *
           END-PERFORM
      *
           .
       47011-SYUMEI-NYUDEL-EXT.
           EXIT.
      *************************************************************
      *    入金取消処理（収納明細レコード作成）
      *************************************************************
       4701-SMEI-SAKUSEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
      *伝票番号
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) (1:7)
                                       TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX1                 TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                       TO  SMEI-SRYKA
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
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *入返金区分
           MOVE    "2"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
           COMPUTE SMEI-NYUHEN-MONEY   =  THOHO-NYUKIN(IDXH)  *  -1
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    THOHO-HOHO (IDXH)   TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "6"                 TO  SMEI-JOUTAIKBN
      *
           MOVE    SPA-OPID            TO  SMEI-OPID
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
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "4002"              TO  SPA-ERRCD
           END-IF     
           .
       4701-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      *************************************************************
      *    入金取消
      *************************************************************
       4701-TORIKESHI-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SYUMEI-AREA
                                       SPA-NAI-SYUMEI-AREA
                                       SPA-GMN-EDANUM
                                       SPA-GMN-SMEILIST-SEL
      *
           MOVE    CONST-PROC-TORIKESHI
                               TO  SPA-NAI-SYORINO
      *
           MOVE    SPA-NAI-NYUKIN-HOHO
                               TO  WRK-CMB-CD
      *
           PERFORM 4200-NYKN-HOHOLST-CHK-SEC
           IF    ( WRK-CMB-ITM =   SPACE )
               MOVE    SPA-GMN-NYKN-HOHOLST (1)
                               TO  SPA-GMN-NYKN-HOHO-G
           ELSE
               MOVE    WRK-CMB-ITM
                               TO  SPA-GMN-NYKN-HOHO-G
           END-IF
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 802-HIZUKE-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUKINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-NYUKINYMD
      *
           MOVE    10              TO  SPA-GMN-CUR
      *
           .
       4701-TORIKESHI-EXT.
           EXIT.
      *************************************************************
      *    履歴修正
      *************************************************************
       490-RIREKI-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               CONTINUE
           ELSE
      *
               INITIALIZE              SPA-GMN-SYUMEI-AREA
                                       SPA-NAI-SYUMEI-AREA
      *
               MOVE    CONST-PROC-RRKSYU
                                   TO  SPA-NAI-SYORINO
      *
               MOVE    SPA-SYSYMD  TO  WRK-SYMD
               PERFORM 802-HIZUKE-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-NYUKINYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-NYUKINYMD
      *
               IF    ( SPA-GMN-SMEILIST-SEL
                                   >   ZERO )
                   MOVE    SPA-GMN-SMEILIST-SEL
                                   TO   S02-RIREKIIN
                   PERFORM 4100-EDANUM-CHK-SEC
               ELSE
                   MOVE    ZERO    TO  SPA-GMN-EDANUM
                   MOVE    13      TO  SPA-GMN-CUR
               END-IF
      *
           END-IF
      *
           .
       490-RIREKI-EXT.
           EXIT.
      *************************************************************
      *    登録処理
      *************************************************************
       410-TOROKU-SEC                  SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4901-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( SPA-SIDCD       =   SPACE )
      *
               MOVE    ZERO        TO  WRK-NYUKIN
                                       WRK-SKYMONEY
      *
               MOVE    SPA-GMN-SKYPRT  TO  WRK-SKYPRT
               MOVE    SPA-GMN-SRYPRT  TO  WRK-SRYPRT
      *
               MOVE    SPA-GMN-TBL-SEL TO  IDXL
      *
               IF    ( SPA-NAI-SYORINO
                                   =   CONST-PROC-RRKSYU )
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-TSEIKYU(IDXL)
                       -   SPA-NAI-SKYMONEY-MOTO
                       +   SPA-GMN-SKYMONEYIN
      *
                   COMPUTE WRK-NYUKIN
                       =   SPA-GMN-TNYUKIN(IDXL)
                       -   SPA-NAI-NYUKIN-MOTO
                       +   SPA-GMN-NYUKIN
               ELSE
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-TSEIKYU(IDXL)
                       +   SPA-GMN-SKYMONEYIN
      *
                   COMPUTE WRK-NYUKIN
                       =   SPA-GMN-TNYUKIN(IDXL)
                       +   SPA-GMN-NYUKIN
               END-IF
      *
               IF    ( SPA-NAI-PROC-FLG    =   4 )
                       PERFORM 4101-TOROKU-SEC
               ELSE
                   IF    ( WRK-SKYMONEY    <   WRK-NYUKIN )
                       MOVE    "2003"  TO  SPA-SIDCD
                   ELSE
                       PERFORM 4101-TOROKU-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( SPA-SIDCD       =   SPACE )
      *
               INITIALIZE                  SPA-GMN-SKYPRT-G
                                           SPA-GMN-SRYPRT-G
      *
               PERFORM 410011-INPUT-AREA-HEN-SEC
      *
           END-IF
      *
           .
       410-TOROKU-EXT.
           EXIT.
      *
      *************************************************************
      *    登録処理
      *************************************************************
       4101-TOROKU-SEC                 SECTION.
      *
           INITIALIZE                  SPA-S01KYOUTU-AREA
                                       TSYU-REC
                                       TSMEI-REC
      *
           MOVE    SPA-NAI-SYORINO TO  SPA-S01-SYORINO
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    SPA-S01-DENPNUM TO  WRK-S01-DENPNUM
           MOVE    SPA-S01-NYUGAIKBN
                                   TO  WRK-S01-NYUGAIKBN
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               INITIALIZE                      SS016-AREA
               MOVE SPA-NAI-PROC-FLG       TO  SS016-PROC-FLG
               MOVE SPA-S01-DENPNUM        TO  SS016-DENPNUM
               MOVE SPA-NAI-DENPEDANUM     TO  SS016-DENPEDANUM
               MOVE SPA-S01-DENPPRTYMD     TO  SS016-DENPPRTYMD
               MOVE SPA-S01-GRP-DENPNUM    TO  SS016-GRP-DENPNUM
               MOVE SPA-S01-PTID           TO  SS016-PTID
               MOVE SPA-S01-NYUGAIKBN      TO  SS016-NYUGAIKBN
               MOVE SPA-S01-SRYKA          TO  SS016-SRYKA
               EVALUATE    SPA-S01-SYORINO
               WHEN CONST-PROC-NYUKIN
                   MOVE    CONST-REQMODE-NYUKIN
                                           TO  SS016-REQMODE
               WHEN CONST-PROC-RRKSYU
                   MOVE    CONST-REQMODE-RRKSYU
                                           TO  SS016-REQMODE
               WHEN CONST-PROC-TORIKESHI
                   MOVE    CONST-REQMODE-TORIKESHI
                                           TO  SS016-REQMODE
               END-EVALUATE
      *
               MOVE SPA-GMN-CHOSEI1IN      TO  SS016-CHOSEI1
               MOVE SPA-GMN-CHOSEI2IN      TO  SS016-CHOSEI2
               MOVE SPA-GMN-SKYMONEYIN     TO  SS016-SKYMONEY
               MOVE SPA-GMN-NYUKIN         TO  SS016-NYUKIN
               MOVE SPA-GMN-NYKN-HOHO      TO  SS016-NYKN-HOHO
               MOVE SPA-NAI-CHOSEI1IN-MOTO TO  SS016-CHOSEI1-MOTO
               MOVE SPA-NAI-CHOSEI2IN-MOTO TO  SS016-CHOSEI2-MOTO
               MOVE SPA-NAI-SKYMONEY-MOTO  TO  SS016-SKYMONEY-MOTO
               MOVE SPA-NAI-NYUKIN-MOTO    TO  SS016-NYUKIN-MOTO
               MOVE SPA-NAI-NYUKINYMD      TO  SS016-NYUKINYMD
               MOVE SPA-NAI-SYURRKUPDKBN   TO  SS016-SYURRKUPDKBN
               MOVE SPA-NAI-SYURRKNUM      TO  SS016-SYURRKNUM
               MOVE SPA-NAI-SYUEDANUM      TO  SS016-SYUEDANUM
      *
               CALL    "ORCSS016"  USING
                                   SS016-AREA
                                   TSYU-REC
                                   TSMEI-REC
                                   SPA-AREA
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
      *        印刷制御情報取得サブ
               CALL    "ORCSONPRTSET"  USING   SPA-AREA
               IF    ( SPA-GMN-SKYPRT  =   "1" )
                 OR  ( SPA-GMN-SRYPRT  =   "1" )
                   IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                    OR   ( SPA-PRTCONF         =   "1" )
                       PERFORM 800-CLIENT-PRT-SEC
                   END-IF
               END-IF
      *
               MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
               INITIALIZE                      SSUB03-AREA
               MOVE    2                   TO  SSUB03-REQNO
               MOVE    TSMEI-DENPEDANUM    TO  SSUB03-DENPEDANUM
               MOVE    SPA-GMN-SKYPRT      TO  SSUB03-SKYPRT
               MOVE    SPA-GMN-SRYPRT      TO  SSUB03-SRYPRT
               CALL    "ORCGSSUB03"        USING
                                           WRK-SPA-AREA
                                           SPA-S01KYOUTU
                                           SSUB03-AREA
      *
               PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 3101-MISYU-EDIT-SEC
      *
               MOVE    2           TO  FLG-MEILIST-ROW
               PERFORM 300-SYUNOU-HENSYU-SEC
      *
               MOVE    SPA-GMN-TBL-SEL TO  S02-SELNUM
               PERFORM 41011-DENPNUM-CHK-SEC
      *
               IF    ( SPA-S01-SYORINO =   CONST-PROC-RRKSYU )
                   PERFORM 490-RIREKI-SEC
      *
               END-IF
           END-IF
      *
           .
       4101-TOROKU-EXT.
           EXIT.
      *************************************************************
      *    伝票番号チェック処理
      *************************************************************
       41011-DENPNUM-CHK-SEC           SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-TBL-SEL
                                           SPA-GMN-SELBAN
      *
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL         >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-TBL-SEL  >   ZERO )
      *
               IF    ( WRK-S01-NYUGAIKBN
                                   =   SPA-NAI-TNYUGAIKBN (IDXL))
                AND  ( WRK-S01-DENPNUM
                                   =   SPA-GMN-TDENPNUM   (IDXL))
      *
                   MOVE    IDXL     TO SPA-GMN-TBL-SEL
                                       SPA-GMN-SELBAN
      *
                   PERFORM 300-SYUMEI-HENSYU-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-SYORINO     =   SPACE )
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41011-DENPNUM-CHK-EXT.
           EXIT.
      *************************************************************
      *    関連チェック
      *************************************************************
       4901-KANRENCHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-TBL-SEL     =   ZERO )
               MOVE    "0065"          TO  SPA-ERRCD
           END-IF
           IF    ( SPA-ERRCD           =   SPACE )
               EVALUATE    SPA-NAI-SYORINO
               WHEN    CONST-PROC-NYUKIN
                   PERFORM 4901-NYUKIN-CHK-SEC
               WHEN    CONST-PROC-TORIKESHI
                   PERFORM 4901-TORIKESHI-CHK-SEC
               WHEN    CONST-PROC-RRKSYU
                   PERFORM 4901-RRKSYU-CHK-SEC
               END-EVALUATE
           END-IF
      *
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *************************************************************
      *    入金取消時関連チェック処理
      *************************************************************
       4901-TORIKESHI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-NYUKIN      =   ZERO )
               MOVE    10              TO  SPA-GMN-CUR
               MOVE    "3007"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-NYUKINYMD   =   SPACE )
                   MOVE    12          TO  SPA-GMN-CUR
                   MOVE    "3012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4901-TORIKESHI-CHK-EXT.
           EXIT.
      *************************************************************
      *    入金処理時関連チェック処理
      *************************************************************
       4901-NYUKIN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-NYUKIN      =   ZERO )
               MOVE    10              TO  SPA-GMN-CUR
               MOVE    "3007"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-NYUKINYMD   =   SPACE )
                   MOVE    12          TO  SPA-GMN-CUR
                   MOVE    "3012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NYKN-HOHO-G =   SPACE )
                   MOVE    11          TO  SPA-GMN-CUR
                   MOVE    "3004"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4901-NYUKIN-CHK-EXT.
           EXIT.
      *************************************************************
      *    履歴修正処理時関連チェック処理
      *************************************************************
       4901-RRKSYU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-EDANUM      =   ZERO )
               MOVE    13              TO  SPA-GMN-CUR
               MOVE    "3006"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-PROC-FLG    =   1  OR  2 )
      *
                   COMPUTE WRK-SKYMONEY
                      =   SPA-GMN-TSEIKYU(SPA-GMN-TBL-SEL)
                      -   SPA-NAI-SKYMONEY-MOTO
                      +   SPA-GMN-SKYMONEYIN
      *
                   IF    ( WRK-SKYMONEY    <   ZERO )
                       MOVE    "3016"      TO  SPA-ERRCD
                       IF    ( SPA-GMN-CHOSEI2IN   NOT =   ZERO )
                           MOVE    9       TO  SPA-GMN-CUR
                       ELSE
                           MOVE    8       TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-NYUKINYMD   =   SPACE )
                   MOVE    12          TO  SPA-GMN-CUR
                   MOVE    "3012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NYUKIN  >   ZERO )
                AND  ( SPA-GMN-NYKN-HOHO-G =   SPACE )
                       MOVE    11          TO  SPA-GMN-CUR
                       MOVE    "3004"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4901-RRKSYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *       収納共通ＳＰＡ編集処理
      *****************************************************************
       4901-SPAKYOTUKEY-SET-SEC               SECTION.
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-S01-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-S01-PTID
           MOVE    SPA-GMN-KANANAME    TO  SPA-S01-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-S01-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-S01-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-S01-BIRTHDAYW
           MOVE    SPA-NAI-NYUKIN-HOHO TO  SPA-S01-NYUKIN-HOHO
           MOVE    SPA-NAI-SKYPRT      TO  SPA-S01-SKYPRT
           MOVE    SPA-NAI-GYOUMUID    TO  SPA-S01-GYOUMUID
           MOVE    SPA-NAI-FILEMEI     TO  SPA-S01-FILEMEI
           MOVE    SPA-NAI-UID         TO  SPA-S01-UID
           MOVE    SPA-NAI-LOCK        TO  SPA-S01-LOCK
           MOVE    SPA-NAI-SLOCK-MSG   TO  SPA-S01-SLOCK-MSG
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-S01-BIRTHDAY
           IF    ( SPA-GMN-SELBAN      >   ZERO )
               MOVE    SPA-NAI-TSRYKA     (SPA-GMN-SELBAN)
                                       TO  SPA-S01-SRYKA
               MOVE    SPA-NAI-TSRYKAMEI  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-SRYKAMEI
               MOVE    SPA-GMN-THKNCOMBI  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-HKNCOMBIMEI
               MOVE    SPA-GMN-TFTNRATE   (SPA-GMN-SELBAN)
                                       TO  SPA-S01-FTNRATEMEI
               MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                       TO  SPA-S01-DENPNUM
               MOVE    SPA-NAI-TGRP-DENPNUM   (SPA-GMN-SELBAN)
                                       TO  SPA-S01-GRP-DENPNUM
               MOVE    SPA-NAI-THAKYMD    (SPA-GMN-SELBAN)
                                       TO  SPA-S01-DENPPRTYMD
               MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SPA-S01-NYUGAIKBN
               MOVE    SPA-NAI-TSRYYMD    (SPA-GMN-SELBAN)
                                       TO  SPA-S01-SRYYMD
               MOVE    SPA-NAI-THKNNUM  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-HKNNUM
               MOVE    SPA-NAI-TRSI-HKNKBN  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-RSI-HKNKBN
           END-IF
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-S01-SRYYMW
           MOVE    SPA-NAI-SRYYM       TO  SPA-S01-SRYYM
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-S01-HNYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  SPA-S01-HSRYKA
           MOVE    SPA-GMN-TBL-SEL     TO  SPA-S01-SYULISTSEL
           MOVE    SPA-GMN-GAI-MISYU   TO  SPA-S01-GAI-MISYU
           MOVE    SPA-GMN-NYU-MISYU   TO  SPA-S01-NYU-MISYU
           MOVE    SPA-GMN-PAGE        TO  SPA-S01-PAGE
           MOVE    SPA-GMN-MODE        TO  SPA-S01-MODE
           MOVE    SPA-GMN-SORTKBN     TO  SPA-S01-SORTKBN
      *
           MOVE    SPA-TUKI-DENPNUM    TO  SPA-S01-TUKI-DENPNUM
           MOVE    SPA-TUKI-SRYYM      TO  SPA-S01-TUKI-SRYYM
           MOVE    SPA-TUKI-KIKANKBN   TO  SPA-S01-TUKI-KIKANKBN
           MOVE    SPA-TUKI-STYMD      TO  SPA-S01-TUKI-STYMD
           MOVE    SPA-TUKI-EDYMD      TO  SPA-S01-TUKI-EDYMD
           MOVE    SPA-TUKI-GRPKBN     TO  SPA-S01-TUKI-GRPKBN
           MOVE    SPA-TUKI-SKYPRT     TO  SPA-S01-TUKI-SKYPRT
           MOVE    SPA-TUKI-SRYPRT     TO  SPA-S01-TUKI-SRYPRT
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
          .
       4901-SPAKYOTUKEY-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    前月表示処理
      **************************************************************
       490-ZENGETU-SEC                 SECTION.
      *
           IF    ( SPA-NAI-SRYYM       =   SPACE )
               IF    ( SPA-GMN-PAGE    >   ZERO )
                   COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    -   1
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
               END-IF
           ELSE
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
      *                前年の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:4)
                       MOVE    "0101"          TO  LNK-DAY6-KIJUN (5:)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     -12            TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:4)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:3)
                                           TO  SPA-GMN-SRYYM
               WHEN  ( SPA-GMN-MODE        =   CONST-MODE-NYUHEN )
                 AND ( SPA-NAI-SRYYM (1:8)     IS  NUMERIC )
      *                前日の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     -1             TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-SRYYM
               WHEN  OTHER
      *                前月の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:6)
                       MOVE    "01"            TO  LNK-DAY6-KIJUN (7:2)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     -1             TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:6)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:6)
                                               TO  SPA-GMN-SRYYM
               END-EVALUATE
      *
                   PERFORM 3101-COMBO-EDIT-SEC
                   MOVE    SPA-GMN-SRYKA       TO  WRK-CMB-CD
                   PERFORM 4200-SRYKALST-CHK-SEC
                   MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
           END-IF
      *
           .
       490-ZENGETU-EXT.
           EXIT.
      **************************************************************
      *    次月表示処理
      **************************************************************
       490-JIGETU-SEC                 SECTION.
      *
           IF    ( SPA-NAI-SRYYM      =   SPACE )
               IF    ( SPA-GMN-NEXT   =   1 )
                   COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    +   1
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
               END-IF
           ELSE
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
      *                次年の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:4)
                       MOVE    "0101"          TO  LNK-DAY6-KIJUN (5:)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     12             TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:4)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:3)
                                           TO  SPA-GMN-SRYYM
               WHEN  ( SPA-GMN-MODE        =   CONST-MODE-NYUHEN )
                 AND ( SPA-NAI-SRYYM (1:8)     IS  NUMERIC )
      *                次日の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     1              TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-SRYYM
               WHEN  OTHER
      *                次月の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:6)
                       MOVE    "01"            TO  LNK-DAY6-KIJUN (7:2)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     1              TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:6)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:6)
                                               TO  SPA-GMN-SRYYM
               END-EVALUATE
      *
                   PERFORM 3101-COMBO-EDIT-SEC
                   MOVE    SPA-GMN-SRYKA       TO  WRK-CMB-CD
                   PERFORM 4200-SRYKALST-CHK-SEC
                   MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
           END-IF
      *
           .
       490-JIGETU-EXT.
           EXIT.
      *****************************************************************
      *    出産一時金画面遷移処理
      *****************************************************************
       480-S08-SENI-SEC                 SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "S08"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "S08"                TO  MCP-WINDOW
      *
           MOVE    SPACE               TO  LINKAREA
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       480-S08-SENI-EXT.
           EXIT.
      **************************************************************
      *    共通パラメタ更新・画面遷移処理
      **************************************************************
       4901-PARA-HEN-SEC                 SECTION.
      *
      *
           .
       4901-PARA-HEN-EXT.
           EXIT.
      *
      **************************************************************
      *    診療科コンボボックスチェック処理
      **************************************************************
       4200-SRYKALST-CHK-SEC             SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SRYKA-MAX )
      *
               IF    ( SPA-GMN-SRYKAL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
          IF    (  WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-SRYKALST (1)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-SRYKALST (1)
                                   TO  WRK-CMB-ITM2
          END-IF
      *
           .
       4200-SRYKALST-CHK-EXT.
           EXIT.
      **************************************************************
      *    診療科コンボボックスチェック処理
      **************************************************************
       4201-SRYKALST-CHK-SEC             SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SRYKA-MAX )
      *
               IF    ( SPA-GMN-SRYKAL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       4201-SRYKALST-CHK-EXT.
           EXIT.
      **************************************************************
      *    入外区分コンボボックスチェック処理
      **************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NYUGAIKBN-MAX )
      *
               IF    ( SPA-GMN-NYUGAIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      **************************************************************
      *    モードコンボボックスチェック処理
      **************************************************************
       4200-MODELST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-MODE-MAX )
      *
               IF    ( SPA-GMN-MODEL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-MODELST (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-NAI-SYS1039TBL  TO  SYS-1039-TBL
               IF    ( SYS-1039-S02MODE    >=  "0" )
                AND  ( SYS-1039-S02MODE    <=  "4" )
                   MOVE    SYS-1039-S02MODE
                                   TO  WRK-NUMX
                   COMPUTE WRK-NUM9    =   WRK-NUM9    +   1
                   MOVE    SPA-GMN-MODELST (WRK-NUM9)
                                   TO  WRK-CMB-ITM
               ELSE
                   MOVE    SPA-GMN-MODELST (1)
                                   TO  WRK-CMB-ITM
               END-IF
           END-IF
      *
           .
       4200-MODELST-CHK-EXT.
           EXIT.
      **************************************************************
      *    並び順コンボボックスチェック処理
      **************************************************************
       4200-SORTKBNLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SORTKBN-MAX )
      *
               IF    ( SPA-GMN-SORTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SORTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-NAI-SYS1039TBL  TO  SYS-1039-TBL
               EVALUATE    SYS-1039-S02SORTKBN
               WHEN    "0"
                   MOVE    SPA-GMN-SORTKBNLST (1)
                                   TO  WRK-CMB-ITM
               WHEN    "2"
                   MOVE    SPA-GMN-SORTKBNLST (2)
                                   TO  WRK-CMB-ITM
               WHEN    "1"
                   MOVE    SPA-GMN-SORTKBNLST (3)
                                   TO  WRK-CMB-ITM
               WHEN    "3"
                   MOVE    SPA-GMN-SORTKBNLST (4)
                                   TO  WRK-CMB-ITM
               WHEN    OTHER
                   MOVE    SPA-GMN-SORTKBNLST (1)
                                   TO  WRK-CMB-ITM
               END-EVALUATE
           END-IF
      *
           .
       4200-SORTKBNLST-CHK-EXT.
           EXIT.
      **************************************************************
      *    入金方法コンボボックスチェック処理
      **************************************************************
       4200-NYKN-HOHOLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-NYKN-HOHO-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-NYKN-HOHOL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-NYKN-HOHO-MAX )
               MOVE    SPA-GMN-NYKN-HOHOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               MOVE    SPA-GMN-NYKN-HOHOMEI2L (IDX-CMB)
                                       TO  WRK-CMB-ITM2
           END-IF
      *
           .
       4200-NYKN-HOHOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行コンボチェック処理
      *****************************************************************
       4101-SKYPRT-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  IDX-DEF
      *
           IF    ( SPA-GMN-SKYPRT-G    =   SPACE )
               MOVE    SPA-GMN-SKYPRTLST (IDX-DEF)
                                       TO  SPA-GMN-SKYPRT-G
           ELSE
               MOVE    SPA-GMN-SKYPRT  TO  WRK-CMB-CD
               PERFORM 4200-SKYPRTLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-SKYPRTLST (IDX-DEF)
                                       TO  SPA-GMN-SKYPRT-G
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-SKYPRT-G
               END-IF
           END-IF
      *
           .
       4101-SKYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行コンボチェック処理
      *****************************************************************
       4101-SRYPRT-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  IDX-DEF
      *
           IF    ( SPA-GMN-SRYPRT-G    =   SPACE )
               MOVE    SPA-GMN-SRYPRTLST (IDX-DEF)
                                       TO  SPA-GMN-SRYPRT-G
           ELSE
               MOVE    SPA-GMN-SRYPRT  TO  WRK-CMB-CD
               PERFORM 4200-SRYPRTLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-SRYPRTLST (IDX-DEF)
                                       TO  SPA-GMN-SRYPRT-G
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYPRT-G
               END-IF
           END-IF
      *
           .
       4101-SRYPRT-CHK-EXT.
           EXIT.
      **************************************************************
      *    収納リスト選択状態保持
      **************************************************************
       4200-SAVE-SYULST-SEL-SEC        SECTION.
      *
           INITIALIZE                  WRK-ORG-NYUGAIKBN
                                       WRK-ORG-DENPNUM
                                       WRK-ORG-SYORINO
      *
           IF    ( SPA-GMN-TBL-SEL >   ZERO )
               MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-TBL-SEL)
                                   TO  WRK-ORG-NYUGAIKBN
               MOVE    SPA-GMN-TDENPNUM (SPA-GMN-TBL-SEL)
                                   TO  WRK-ORG-DENPNUM
               MOVE    SPA-NAI-SYORINO
                                   TO  WRK-ORG-SYORINO
           END-IF
      *
           .
       4200-SAVE-SYULST-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納リスト選択状態回復
      **************************************************************
       4200-LOAD-SYULST-SEL-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-INPUT
                                       SPA-GMN-TBL-SEL
      *
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL         >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-TBL-SEL >   ZERO )
      *
               IF    ( WRK-ORG-NYUGAIKBN
                                   =   SPA-NAI-TNYUGAIKBN (IDXL))
                AND  ( WRK-ORG-DENPNUM
                                   =   SPA-GMN-TDENPNUM   (IDXL))
      *
                   MOVE    IDXL    TO  SPA-GMN-TBL-SEL
                                       SPA-GMN-SELBAN
                   IF    ( WRK-ORG-SYORINO =   SPACE )
                       PERFORM 300-SYUMEI-HENSYU-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-TBL-SEL =   ZERO )
               PERFORM 300-SYUMEI-HENSYU-SEC
           END-IF
      *
           .
       4200-LOAD-SYULST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分コンボチェック処理
      *****************************************************************
       4200-SKYPRTLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-SKYPRT-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-SKYPRTL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-SKYPRT-MAX )
               MOVE    SPA-GMN-SKYPRTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKYPRTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行区分コンボチェック処理
      *****************************************************************
       4200-SRYPRTLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-SRYPRT-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-SRYPRTL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-SRYPRT-MAX )
               MOVE    SPA-GMN-SRYPRTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SRYPRTLST-CHK-EXT.
           EXIT.
      **************************************************************
      *    保険組合せ名称編集処理
      **************************************************************
       510-HKNKUMI-MEI-SEC             SECTION.
      *
      *    対象の保険組み合わせが存在すればその名称を、
      *    なければ保険組み合わせ番号を表示する
      *
           MOVE    SPACE           TO  WRK-HKNKUMI
                                       WRK-HKNNUM
                                       WRK-HOJOKBN
                                       WRK-RSI-HKNKBN
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( FLG-CHK =   1   )
                   OR    ( IDX-COMB    >   100 )
               IF    ( WRK-KUMINO  =   SPA-NAI-HKNCOMBIL (IDX-COMB))
                   MOVE    SPA-NAI-HKNCOMBIMEIL (IDX-COMB)
                                       TO  WRK-HKNKUMI
                   MOVE    SPA-NAI-HKNNUML (IDX-COMB)
                                       TO  WRK-HKNNUM
                   MOVE    SPA-NAI-HOJOKBNL (IDX-COMB)
                                       TO  WRK-HOJOKBN
                   MOVE    SPA-NAI-RSI-HKNKBNL (IDX-COMB)
                                       TO  WRK-RSI-HKNKBN
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK         =   ZERO )
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI    =   ZERO )
                   PERFORM 5101-HKNKUMI-MEI-SEC
               ELSE
                   MOVE    WRK-KUMINO  TO  WRK-HKNKUMI
               END-IF
           END-IF
      *
           .
       510-HKNKUMI-MEI-EXT.
           EXIT.
      *
      **************************************************************
      *    保険組合せ名称編集処理
      **************************************************************
       5101-HKNKUMI-MEI-SEC            SECTION.
      *
           MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
           MOVE    COMB-HOJOKBN    TO  WRK-HOJOKBN
      *
           EVALUATE    TRUE
           WHEN    ( COMB-HKNNUM =   "971" OR "973" )
               MOVE    COMB-HKNID  TO  WRK-HKNID
               PERFORM 900-PTRSIINF-KEY-SEL-SEC
               IF    ( FLG-PTRSIINF    =   ZERO )
                   MOVE    PTRSI-HKNKBN    TO  WRK-RSI-HKNKBN
      *
                   MOVE    SPACE   TO  WRK-HKNKBN-MEI
                   EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"    TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"    TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"    TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"    TO  WRK-HKNKBN-MEI
                   END-EVALUATE
                   IF    ( PTRSI-COMMENT   =   SPACE )
                       MOVE    SPACE   TO  WRK-HKNKUMI
                       STRING  COMB-SYU-TANSEIDONAME
                                               DELIMITED  BY  SPACE
                               " "             DELIMITED  BY  SIZE
                               WRK-HKNKBN-MEI  DELIMITED  BY  SPACE
                       INTO    WRK-HKNKUMI
                       END-STRING
                   ELSE
                       MOVE    SPACE   TO  WRK-HKNKUMI
                       STRING  COMB-SYU-TANSEIDONAME
                                               DELIMITED  BY  SPACE
                               " "             DELIMITED  BY  SIZE
                               WRK-HKNKBN-MEI
                                               DELIMITED  BY  SPACE
                               "（"            DELIMITED  BY  SIZE
                               PTRSI-COMMENT(1:24)
                                               DELIMITED  BY  SPACE
                               "）"            DELIMITED  BY  SIZE
                       INTO    WRK-HKNKUMI
                       END-STRING
                   END-IF
               END-IF
           WHEN    ( COMB-SYU-TANSEIDONAME   =   SPACE )
               MOVE    SPACE               TO  WRK-HKNKUMI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
               INTO    WRK-HKNKUMI
               END-STRING
           WHEN    OTHER
               MOVE    SPACE   TO  WRK-HKNKUMI
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
               INTO    WRK-HKNKUMI
               END-STRING
           END-EVALUATE
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HKNKUMI         TO  KANACHK-MAE-INPUT
           MOVE    50                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNKUMI
      *
           .
       5101-HKNKUMI-MEI-EXT.
           EXIT.
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       420-CLEAR-SEC        SECTION.
      *
           IF    ( SPA-NAI-SYORINO         =   SPACE )
            AND  ( SPA-GMN-SMEILIST-SEL    =  ZERO  )
               PERFORM 4201-CLEAR-SEC
           ELSE
               INITIALIZE              SPA-GMN-SYUMEI-AREA
                                       SPA-NAI-SYUMEI-AREA
                                       SPA-GMN-EDANUM
                                       SPA-GMN-SMEILIST-SEL
                                       SPA-GMN-SKYPRT-G
                                       SPA-GMN-SRYPRT-G
      *
               PERFORM 410011-INPUT-AREA-HEN-SEC
           END-IF
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       4201-CLEAR-SEC                  SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SAVE-SRYYM
           MOVE    SPA-GMN-MODE        TO  WRK-GMN-MODE
           MOVE    SPA-GMN-SORTKBN     TO  WRK-GMN-SORTKBN
           MOVE    SPA-GMN-PTID        TO  WRK-GMN-PTID
      *
           PERFORM 311-SPA-SET-SEC
      *
           IF    ( WRK-GMN-PTID    NOT =   ZERO )
               MOVE    WRK-GMN-SORTKBN    TO   SPA-GMN-SORTKBN
               PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
               MOVE    WRK-GMN-MODE    TO  S02-MODE
               MOVE    SPACE           TO  SPA-GMN-MODE
               MOVE    WRK-SAVE-SRYYM  TO  SPA-NAI-THSRYYM
               PERFORM 4106-MODE-CHK-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-S10-CHK
           PERFORM 3111-S10-CHK-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       4201-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       430-MAEKPTNUM-SEC               SECTION.
      *
           IF    ( SPA-LAST-PTNUM      =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-LAST-PTNUM  TO  S02-PTNUM
               PERFORM 4102-PTNUM-CHK-SEC
           END-IF
      *
           .
       430-MAEKPTNUM-EXT.
           EXIT.
      *****************************************************************
      *    画面モード変更
      *****************************************************************
       470-MODE-CHENGE-SEC             SECTION.
      *
           MOVE    SPACE                   TO  S02-MODE
           EVALUATE    SPA-GMN-MODE
           WHEN    CONST-MODE-NORMON
           WHEN    CONST-MODE-NORYEAR
           WHEN    CONST-MODE-NORALL
               MOVE    "3"                 TO  S02-MODE
           WHEN    CONST-MODE-MISYU
               MOVE    "4"                 TO  S02-MODE
           WHEN    OTHER
               MOVE    SPA-NAI-THSRYYM     TO  SPA-NAI-SRYYM
               MOVE    "0"                 TO  S02-MODE
           END-EVALUATE
      *
           PERFORM 4106-MODE-CHK-SEC
      *
           .
       470-MODE-CHENGE-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-PTNUM
           MOVE    ZERO                TO  SPA-PTID
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
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "S02"               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
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
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *----(01.01.19)--ADD-START---
      *****************************************************************
      *    支払証明書
      *****************************************************************
       500-SHIHARAI-SEC            SECTION.
      *
           IF       SPA-GMN-PTID   =   ZERO
               MOVE    "0071"      TO  SPA-ERRCD
               GO  TO  500-SHIHARAI-EXT
           END-IF
      *
           INITIALIZE                  SPA-S01KYOUTU-AREA
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
      *
           PERFORM  41011-S04-SET-SEC
      *
      *     MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "S04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE.
           MOVE    "S04    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                    TO  FLG-END
      *
           .
       500-SHIHARAI-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面編集処理
      *****************************************************************
       41011-S04-SET-SEC               SECTION.
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-S02             TO  SPA-FREE1
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGS04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-S01KYOUTU
           MOVE    SPA-FREE1           TO  SPA-S02
      *
           .
       41011-S04-SET-EXT.
           EXIT.
      *
      *----(01.01.19)--ADD-END-----
      **************************************************************
      *    エラーメッセージ表示理
      **************************************************************
       510-ERRSET-SEC              SECTION.
      *
           CALL    "ORCGSERRMSG"        USING
                                        SPA-AREA
                                        ORCSLOCKAREA
      *
           MOVE    SPACE                TO  SERR
           MOVE    SPA-ERRCD            TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  SERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *     
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-SIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SIDMSG
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2001"
               MOVE    "請求データを取り消します"
                                        TO  WRK-SIDMSG
           WHEN    "2002"
               MOVE    "外来収納の一括再計算を行います"
                                        TO  WRK-SIDMSG
           WHEN    "2003"
               MOVE    "過入金状態になりますがよろしいですか"
                                        TO  WRK-SIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  SID1
           INITIALIZE                       SID1
           MOVE    SPA-SIDCD            TO  SID1-ID1CODE
           MOVE    WRK-SIDMSG           TO  SID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-SIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-SID2SET-SEC              SECTION.
      *
           MOVE    SPACE                TO  SID2
      *
           INITIALIZE                       SID2
           EVALUATE    SPA-SIDCD
           WHEN    "2101"
               MOVE   "すべての入金記録を取り消します。よろしいですか"
                                        TO  SID2-ID1MSG
               MOVE   "F11:金額入力"
                                        TO  SID2-B11-LABEL
               MOVE   "F12:確定"        TO  SID2-B12-LABEL
           END-EVALUATE
           MOVE    SPA-SIDCD            TO  SID2-ID1CODE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-SID2SET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-SID3SET-SEC              SECTION.
      *
           MOVE    SPACE                TO  SID3
           INITIALIZE                       SID3
      *
           EVALUATE     SPA-SIDCD
           WHEN    "2102"
               PERFORM 5201-SKYPRTMSG-EDIT-SEC
               MOVE    WRK-SKYPRTMSG    TO  SID3-LBLID1SKYPRT
      *
               STRING "再発行を行います"
                                            DELIMITED BY SIZE
               INTO   SID3-ID1MSG
               END-STRING
           END-EVALUATE
           MOVE    SPA-SIDCD            TO  SID3-ID1CODE
           MOVE    "B12"                TO  MCP-WIDGET
           IF    ( SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                        =   "1" )
               MOVE    WIDGET-INSENSITIVE
                                        TO  SID3-B12S-STATE
               MOVE    "F"              TO  SID3-B12S-VISIBLE
           ELSE
               MOVE    "T"              TO  SID3-B12S-VISIBLE
               MOVE    WIDGET-NORMAL    TO  SID3-B12S-STATE
           END-IF
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SID3"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SID3"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-SID3SET-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行メッセージ編集処理
      *****************************************************************
       5201-SKYPRTMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SKYPRTMSG
      *
           EVALUATE SPA-NAI-SKYPRT
           WHEN    "0"
               MOVE    "【請求書・明細書不要】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "1"
               MOVE    "【請求書・明細書必要】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "2"
               MOVE    "【請求書・明細書必要（請求あり）】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "3"
               MOVE    "【請求書・明細書必要（訂正時なし）】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "4"
               MOVE    "【請求書必要（明細書不要）】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "5"
               MOVE    "【請求書必要（請求あり）（明細書不要）】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "6"
               MOVE    "【請求書必要（訂正時なし）（明細書不要）】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "7"
               MOVE    "【請求書必要（請求あり）（明細書必要）】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "8"
               MOVE    "【請求書不要・明細書必要】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "9"
               MOVE    "【請求書不要・明細書必要（請求あり）】"
                                   TO  WRK-SKYPRTMSG
           WHEN    "A"
               MOVE    "【請求書不要・明細書必要（訂正時なし）】"
                                   TO  WRK-SKYPRTMSG
           END-EVALUATE
      *
           .
       5201-SKYPRTMSG-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-SID4SET-SEC              SECTION.
      *
           MOVE    "SU1"           TO  WRK-GYOUMUID
                                       SPA-NAI-GYOUMUID
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID           TO  WRK-UID
                                       SPA-NAI-UID
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD    TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS    TO  WRK-FILEMEI-HMS
           MOVE    WRK-FILEMEI     TO  SPA-NAI-FILEMEI
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    SPA-AREA        TO  WRK-SPA-AREA
               MOVE    SPA-GMN-PTID    TO  WRK-SPA-PTID
               MOVE    "2"             TO  WRK-SPA-NYUGAIKBN
               MOVE    SPA-GMN-PTNUM   TO  WRK-SPA-PTNUM
               MOVE    SPA-GMN-NAME    TO  WRK-SPA-NAME
               MOVE    SPA-GMN-KANANAME TO WRK-SPA-KANANAME
      *
               INITIALIZE                  SPA-S01KYOUTU-AREA
               PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
               CALL    "ORCGSSUB01"    USING
                                       WRK-SPA-AREA
                                       SPA-S01KYOUTU
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               MOVE    SPA-S01KYOUTU   TO  SPA-WORK
               MOVE    SPA-AREA        TO  SPA-COMMON
               MOVE    SPA-S02         TO  SPA-FREE1
      *
               MOVE    "LINK"          TO  MCP-STATUS
               MOVE    SPACE           TO  MCP-EVENT
      *
               MOVE    SPACE           TO  SID4
               INITIALIZE                  SID4
      *
               PERFORM 5201-SKYPRTMSG-EDIT-SEC
               MOVE    WRK-SKYPRTMSG   TO  SID4-LBLID1SKYPRT
      *
               CALL    "ORCGSID4"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-WORK        TO  SPA-S01KYOUTU
               MOVE    SPA-FREE1       TO  SPA-S02
      *
               MOVE    "B12"           TO  MCP-WIDGET
      *
               MOVE    "S02"           TO  SPA-MOTOPG
               MOVE    "SID4"          TO  SPA-SAKIPG
      *
               MOVE    "NEW"           TO  MCP-PUTTYPE
               MOVE    "SID4"          TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       520-SID4SET-EXT.
           EXIT.
      **************************************************************
      *    分娩チェック
      **************************************************************
       800-BUNBEN-CHK-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-BUNBENCHK
      *
           IF    ( SPA-NAI-TNYUGAIKBN (IDXB)
                                       =   "2" )
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-BUNBENCHK
           END-IF
      *
           IF    (( SPA-NAI-SEX    =   "2" )
             AND  ( SPA-NAI-TRECESRYKA (IDXB)
                                   =   "23" OR "24" ))
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-BUNBENCHK
           END-IF
      *
           EVALUATE    SPA-NAI-THKNNUM (IDXB)
           WHEN    "002"
               IF   (  SPA-NAI-THOJOKBN (IDXB)
                               NOT =   SPACE )
                   MOVE    ZERO    TO  FLG-BUNBENCHK
               END-IF
           WHEN    "039"
           WHEN    "040"
           WHEN    "971"
           WHEN    "973"
           WHEN    "975"
               MOVE    ZERO        TO  FLG-BUNBENCHK
           END-EVALUATE
      *
           IF    ( SPA-NAI-TBUNBEN (IDXB)
                                   =   1 )
               MOVE    1           TO  FLG-BUNBENCHK
           END-IF
      *
           .
       800-BUNBEN-CHK-EXT.
           EXIT.
      **************************************************************
      *    西暦和暦編集処理
      **************************************************************
       800-HIZUKE-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-HENYMDG2
      *
           IF    ( WRK-SYMD            =   ALL "9"  OR   SPACE )
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
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG2
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       801-HIZUKE-CHK-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
           MOVE    ZERO                TO  FLG-HIZUKE-RC
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "2"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD            TO  SGDAY-INDAY
               CALL    "ORCSGDAY"              USING
                                               ORCSGDAYAREA
               IF  ( SGDAY-RC              =   ZERO )
      *
                   MOVE    SGDAY-SDAY      TO  WRK-SYMD
                   EVALUATE    TRUE
                   WHEN  ( WRK-SYMD (5:)   =   SPACE )
                       INITIALIZE              STS-AREA-DAY
                       INITIALIZE              LNK-DAY2-AREA
                       MOVE    "21"        TO  LNK-DAY2-IRAI
                       MOVE    "0101"      TO  WRK-SYMD  (5:)
                       MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                       MOVE    LNK-DAY2-EDTYMD1
                                           TO  WRK-HENYMDG (1:3)
                       MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HENYMDG2(1:10)
                       MOVE    SPACE       TO  WRK-SYMD  (5:)
                   WHEN  ( WRK-SYMD (7:)   =   SPACE )
                       INITIALIZE              STS-AREA-DAY
                       INITIALIZE              LNK-DAY2-AREA
                       MOVE    "21"        TO  LNK-DAY2-IRAI
                       MOVE    "01"        TO  WRK-SYMD  (7:)
                       MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                       MOVE    LNK-DAY2-EDTYMD1
                                           TO  WRK-HENYMDG (1:6)
                       MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HENYMDG2(1:16)
                       MOVE    SPACE       TO  WRK-SYMD  (7:)
                   WHEN    OTHER
                       INITIALIZE              STS-AREA-DAY
                       INITIALIZE              LNK-DAY2-AREA
                       MOVE    "21"        TO  LNK-DAY2-IRAI
                       MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                       MOVE    LNK-DAY2-EDTYMD1
                                           TO  WRK-HENYMDG
                       MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HENYMDG2
                   END-EVALUATE
               ELSE
                   MOVE    1               TO  FLG-HIZUKE-RC
               END-IF
           END-IF
      *
           .
       801-HIZUKE-CHK-EXT.
           EXIT.
      **************************************************************
      *    西暦和暦編集処理
      **************************************************************
       802-HIZUKE-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           END-IF
      *
           .
       802-HIZUKE-EXT.
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
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC                SECTION.
      *
           CALL    "ORCSDAILYKEY"      USING
                                       SDAILYKEYAREA
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
      *
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
      *    金額数値変換処理
      *****************************************************************
       800-KINGAKU-HEN-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-ORCSNUMERR
      *
           INITIALIZE                  WRK-KINGAKU9
                                       WRK-KINGAKUZ
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-KINGAKUX    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-SEISUU         >   7     )
            OR   ( SNUM-SYOKBN         =   "1"   )
               MOVE    1               TO  FLG-ORCSNUMERR
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKU9
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKUZ
           END-IF
      *
           .
       800-KINGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       800-CLIENT-PRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                          ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
           .
       800-CLIENT-PRT-EXT.
           EXIT.
      *****************************************************************
      *    クライアントダミー印刷　処理
      *****************************************************************
       800-CLIENT-DUMMY-PRT-SEC        SECTION.
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           .
       800-CLIENT-DUMMY-PRT-EXT.
           EXIT.
      **************************************************************
      *    収納マスター読込（主キー）
      **************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY27-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY27-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  SYU-SRYKA
           STRING  SPA-NAI-SRYYM       DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
           IF    ( SPA-NAI-SRYYM       =   SPACE )
               MOVE    "ALL"           TO  SYU-UPSRYYMD
               COMPUTE SYU-CHOSEI1 =   WRK-MAX + 1
               COMPUTE SYU-CHOSEI2 =   WRK-MAX * SPA-GMN-PAGE
           END-IF
           MOVE    SPA-GMN-SORTKBN     TO  SYU-SELNYUGAIKBN
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY73-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY73-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY53-SEL-SEC        SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key53"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key53"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY53-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY54-SEL-SEC        SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key54"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key54"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY54-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納検索処理
      **************************************************************
       900-SYUNOU-KEY61-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "%"             TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           STRING  SPA-NAI-SRYYM       DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"   TO  MCP-TABLE
           MOVE    "key61"        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY61-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納FETCH処理
      **************************************************************
       900-SYUNOU-KEY61-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"   TO  MCP-TABLE
           MOVE    "key61"        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY61-FET-EXT.
           EXIT.
      **************************************************************
      *    収納検索処理
      **************************************************************
       900-SYUNOU-KEY86-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "2"             TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM (1:6)
                                   TO  SYU-SRYYMD(1:6)
           MOVE    "__"            TO  SYU-SRYYMD(7:)
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key86"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key86"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY86-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細検索処理
      **************************************************************
       900-SYUMEI-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
           MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                       TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY7-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細検索処理
      **************************************************************
       900-SYUMEI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
           MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                       TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細FETCH処理
      **************************************************************
       900-SYUMEI-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-FET-EXT.
           EXIT.
      **************************************************************
      *    患者情報検索処理(KEY)
      **************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY21)
      *****************************************************************
       900-SYSKANRI-KEY21-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key21"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY21)
      *****************************************************************
       900-SYSKANRI-KEY21-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key21"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY21-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY4-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    WRK-GYOUMUID        TO  PARA-GYOUMUID
           MOVE    WRK-UID             TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PARA-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    WRK-GYOUMUID        TO  PARA-GYOUMUID
           MOVE    WRK-UID             TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *************************************************************
      *    収納合計マスター検索処理
      *************************************************************
       900-SYUTOTAL-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUTTL
           INITIALIZE                       SYUTOTAL-REC
           MOVE    SPA-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SYUTTL-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYUTTL-PTID
           MOVE    SPA-S01-GRP-DENPNUM TO  SYUTTL-DENPNUM
           MOVE    SYUTOTAL-REC        TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUTOTAL-REC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE                  SYUTOTAL-REC
           END-IF
      *
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUTOTAL-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    WRK-KUMINO          TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    WRK-HKNID           TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
     *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
       910-DBDELETE-SEC                SECTION.
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
      *    ＤＢインサート処理
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
      *    BLOBIMPORT処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    ORCA-BLOB       TO  MCPDATA-REC
      *
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    MCPDATA-REC     TO  ORCA-BLOB
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ORCA-BLOB-FILE
      *
           .
       910-BLOBIMPORT-EXT.
           EXIT.
      *************************************************************
      *    トランザクションキャンセル処理
      *************************************************************
       910-DBABORT-SEC               SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "abort"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
      *
           .
       910-DBABORT-EXT.
           EXIT.
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
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
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
      **************************************************************
      *    ＰＵＴ　処理
      **************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
