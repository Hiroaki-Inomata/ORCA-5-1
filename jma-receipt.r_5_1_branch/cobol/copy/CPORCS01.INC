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
      ************************************************************
      *    負担金額算定用パラメタ　　　　　 COPY (CPORCS01.INC)
      *                                使用プログラム  (ORCS01)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/04/16  自賠責・船員保険追加
      * 01.00.02     NACL-門脇    04/06/30  レイアウト修正
      * 01.00.03     NACL-門脇    04/10/01  自賠責特定器材等消費税対応
      * 01.00.04     NACL-門脇    06/03/22  自立支援再計算区分追加
      *                                     特殊計算再計算区分追加
      *                                     自立支援負担変更区分追加
      * 01.00.05     NACL-門脇    09/06/16  一括再計算順、一括再計算方法追加
      * 01.00.06     NACL-門脇    10/01/26  診察料算定フラグ２追加
      * 01.00.07     NACL-門脇    11/02/03  時間外受診フラグ追加
      * 01.00.08     NACL-門脇    12/03/09  現物給付計算フラグ追加
      *                                     現物給付計算まとめ解除フラグ追加
      *                                     現物給付メッセージフラグ追加
      *
      * 04.06.01     NACL-門脇    13/02/18  自賠責救急医療管理加算点数追加
      * 04.08.01     NACL-門脇    17/07/14  チェック用ＩＤＸ追加
      * 04.08.02     NACL-門脇    17/12/15  第三者行為現物給付に関わる
      *                                     負担金計算対応
      * 04.08.03     NACL-門脇    18/05/15  リトライフラグ追加
      * 04.08.04     NACL-門脇    18/05/31  計算金額桁数チェックフラグ追加
      *
      * 05.00.01     NACL-門脇    18/11/16  初診料算定フラグ２（地方公費用）
      *                                     追加
      * 05.00.02     NACL-門脇    19/10/01  一括再計算マル長公費負担優先計算
      *                                     フラグ追加
      ************************************************************
       01  LNK-ORCS01-REC.
      *
      *再計算処理区分
           03  LNK-S01-SAIKEISAN-KBN       PIC  X(01).
      *高齢者（在宅総合・在宅末期）算定患者区分
           03  LNK-S01-KOUREI-ZAITAKU-KBN  PIC  X(01).
      *特殊計算再計算区分
           03  LNK-S01-TOKUSYU-SAI-KBN     PIC  X(01).
      *複数科まとめ集計フラグ（０：まとめ計算する　１：しない）
           03  LNK-S01-KAGRPFLG            PIC  X(01).
      *初診料算定フラグ（地方公費用）
           03  LNK-S01-SYOSINFLG           PIC  X(01).
      *初診料算定フラグ２（地方公費用）
           03  LNK-S01-SYOSINFLG2          PIC  X(01).
      *診察料算定フラグ（地方公費用）
           03  LNK-S01-SINSATUFLG          PIC  X(01).
      *診察料算定フラグ２（地方公費用）
           03  LNK-S01-SINSATUFLG2         PIC  X(01).
      *時間外受診フラグ（地方公費用）
           03  LNK-S01-JIKANGAI            PIC  X(01).
      *第三者行為分計算優先フラグ
           03  LNK-S01-DAISANYUSENFLG      PIC  X(01).
      *一括再計算順
           03  LNK-S01-IKTSORTKBN          PIC  X(01).
      *一括再計算方法
           03  LNK-S01-IKTKEISANKBN        PIC  X(01).
      *分娩自費区分
           03  LNK-S01-BUNBENJIHIKBN       PIC  X(01).
      *
      *一括再計算：現物給付計算フラグ
           03  LNK-S01-IKTGENKEIFLG        PIC  X(01).
      *一括再計算：現物給付計算まとめ解除フラグ
           03  LNK-S01-IKTGENMKJFLG        PIC  X(01).
      *一括再計算：実施フラグ
           03  LNK-S01-IKTJISSIFLG         PIC  X(01).
      *一括再計算：該当者チェックフラグ
           03  LNK-S01-IKTCHECKFLG         PIC  X(01).
      *一括再計算：マル長公費負担優先計算フラグ
           03  LNK-S01-IKTCYOKIFLG         PIC  X(01).
      *
      *現物給付メッセージフラグ
           03  LNK-S01-GENBUTU-MSG-FLG     PIC  X(01).
      *リトライフラグ
           03  LNK-S01-RETRY-FLG           PIC  X(01).
      *計算金額桁数チェックフラグ
           03  LNK-S01-SKYCHKFLG           PIC  X(01).
      *
           03  LNK-S01-SINKOUI.
      *診療行為内容 
             04  LNK-S01-TBL1              OCCURS   3.
      *診療行為内容 
               05  LNK-S01-TBL             OCCURS  40.
      *診療区分
                 06  LNK-S01-SRYKBN        PIC  X(02).
      *剤番号
                 06  LNK-S01-ZAIBAN        PIC  9(08).
      *診療コード
                 06  LNK-S01-SRYCD         PIC  X(09).
      *数量
                 06  LNK-S01-SURYOU        PIC  9(05)V9(05).
      *回数
                 06  LNK-S01-KAISUU        PIC  9(03).
      *点数
                 06  LNK-S01-TENSUU        PIC  9(07).
      *
           03  LNK-S01-OCC                 OCCURS  15.
      *船員保険（被保険者）＋通勤災害＋初診時
             04  LNK-S01-SENINFLG          PIC  X(01).
      *自賠責手技点合計
             04  LNK-S01-SYUTEN            PIC  9(07).
      *自賠責診断書料金額
             04  LNK-S01-SINDAN            PIC  9(07).
      *自賠責明細書料金額
             04  LNK-S01-MEISAI            PIC  9(07).
      *自賠責療養担当手当点数
             04  LNK-S01-RYOYO             PIC  9(07).
      *自賠責その他コード（０９５９１．９２．９３）の診療行為コード等
             04  LNK-S01-SONOTA.
               05  LNK-S01-SONOTA-OCC         OCCURS  20.
                 06  LNK-S01-SONOTA-SRYCD     PIC  X(09).
                 06  LNK-S01-SONOTA-ZAIKAISU  PIC  9(07).
                 06  LNK-S01-SONOTA-TANKA     PIC  9(07).
      *自賠責救急医療管理加算点数
             04  LNK-S01-KYUKYU            PIC  9(07).
      *
      *計算対象の収納データＩＤＸ
           03  LNK-S01-IDX                 PIC  9(02).
      *
      *チェック用ＩＤＸ
           03  LNK-S01-IDX2                PIC  9(02).
      *
           03  LNK-S01-OCC2                OCCURS  15.
      *合計レコードをもつレコード識別フラグ
             04  LNK-S01-TTLARIFLG         PIC  X(01).
      *
      *復帰コード
           03  LNK-S01-RC                  PIC S9(09)   BINARY.
      *
