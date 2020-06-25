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
      *    負担金額算定用パラメタ（入院用）COPY (CPORCS02.INC)
      *                                使用プログラム (ORCS02)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-門脇    04/10/01  自賠責特定器材等消費税対応
      * 01.00.02     NACL-門脇    06/03/20  自立支援再計算区分追加
      * 04.01.01     NACL-門脇    07/12/19  入院料未算定日対応
      * 04.01.02     NACL-門脇    08/01/21  同日再入院食事対応
      * 04.04.01     NACL-門脇    09/11/05  高齢者他保険入院対応
      *
      * 04.05.01     NACL-門脇    09/12/04  食事（特別食）の他保険対応
      * 04.05.02     NACL-門脇    11/11/09  同日再入院食事対応
      *
      * 04.06.01     NACL-門脇    13/02/18  自賠責救急医療管理加算点数
      *                                     追加
      *
      * 04.07.01     NACL-門脇    15/02/02  システム管理２０１０機能追
      *                                     加対応
      * 04.07.02     NACL-門脇    15/06/02  短期滞在手術等基本料算定時の
      *                                     入院負担金計算対応
      * 04.08.01     NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
      * 04.08.02     NACL-門脇    17/12/15  第三者行為現物給付に関わる
      *                                     負担金計算対応
      * 04.08.03     NACL-門脇    18/05/15  リトライフラグ追加
      *                                     チェック用ＩＤＸ追加
      * 04.08.04     NACL-門脇    18/05/31  計算金額桁数チェックフラグ追加
      *
      * 05.00.01     NACL-門脇    18/11/16  初診料算定フラグ２（地方公費用）
      *                                     追加
      ************************************************************
      *01  LNK-ORCS02-REC.
      *医療機関ＩＤ
           03  LNK-S02-HOSPNUM             PIC  9(02).
      *入外区分
           03  LNK-S02-NYUGAIKBN           PIC  X(01).
      *患者ＩＤ
           03  LNK-S02-PTID                PIC  9(10).
      *処理区分（１：退院時　２：定期請求、入院レセプト）
           03  LNK-S02-SYORI-KBN           PIC  X(01).
      *定期請求区分
           03  LNK-S02-TEIKI-KBN           PIC  X(01).
      *請求開始日
           03  LNK-S02-SKYSTYMD            PIC  X(08).
      *請求終了日
           03  LNK-S02-SKYEDYMD            PIC  X(08).
      *請求開始日同日再入院フラグ（＊）
           03  LNK-S02-DOJITUFLG-ST        PIC  X(01).
      *開始日入院食事区分（０：食なし、１：朝から、２：昼から、３：夕から）（＊）
           03  LNK-S02-TIMEKBN-ST          PIC  X(01).
      *請求終了日同日再入院フラグ（＊）
           03  LNK-S02-DOJITUFLG-ED        PIC  X(01).
      *終了日入院食事区分（０：食なし、１：朝まで、２：昼まで、３：夕まで）（＊）
           03  LNK-S02-TIMEKBN-ED          PIC  X(01).
      *高齢者算定患者区分
           03  LNK-S02-KOUREI-NYUIN-KBN    PIC  X(01).
      *特殊計算再計算区分
           03  LNK-S02-TOKUSYU-SAI-KBN     PIC  X(01).
      *入院通算日数（＊）
           03  LNK-S02-NYUIN-TSUSAN-NISSU  PIC  9(04).
      *システム日付
           03  LNK-S02-SYSYMD              PIC  X(08).
      *リトライフラグ
           03  LNK-S02-RETRY-FLG           PIC  X(01).
      *計算金額桁数チェックフラグ
           03  LNK-S02-SKYCHKFLG           PIC  X(01).
      *
      *初診料算定フラグ（地方公費用）
           03  LNK-S02-SYOSINFLG           PIC  X(01).
           03  LNK-S02-SYOSINFLG-DAY-TBL.
              05  LNK-S02-SYOSINFLG-DAY    PIC  X(01)   OCCURS  31.
      *初診料算定フラグ２（地方公費用）
           03  LNK-S02-SYOSINFLG2          PIC  X(01).
      *
      *船員保険（被保険者）＋通勤災害＋初診時
           03  LNK-S02-SENINFLG            PIC  X(01).
      *自賠責手技点合計
           03  LNK-S02-SYUTEN              PIC  9(07).
      *自賠責診断書料金額
           03  LNK-S02-SINDAN              PIC  9(07).
      *自賠責明細書料金額
           03  LNK-S02-MEISAI              PIC  9(07).
      *自賠責療養担当手当点数
           03  LNK-S02-RYOYO               PIC  9(07).
      *自賠責その他コード（０９５９１．９２．９３）の診療行為コード等
           03  LNK-S02-SONOTA.
              05  LNK-S02-SONOTA-OCC         OCCURS  50.
                07  LNK-S02-SONOTA-SRYCD     PIC  X(09).
                07  LNK-S02-SONOTA-ZAIKAISU  PIC  9(07).
                07  LNK-S02-SONOTA-TANKA     PIC  9(07).
      *自賠責救急医療管理加算点数
           03  LNK-S02-KYUKYU              PIC  9(07).
      *
      *入院料未算定区分
           03  LNK-S02-NYUINDAY-TBL.
              05  LNK-S02-NYUINDAY-KBN     PIC  X(01)   OCCURS  31.
      *
      *短期滞在手術等基本料算定区分
           03  LNK-S02-NYUINDAY-TANKI-TBL.
              05  LNK-S02-NYUINDAY-TANKI-KBN
                                           PIC  X(01)   OCCURS  31.
      *
      *入院料算定識別区分
           03  LNK-S02-NYUSANTEI-TBL.
              05  LNK-S02-NYUSANTEI-KBN    PIC  X(01)   OCCURS  31.
      *
      *同日再入院区分
           03  LNK-S02-DOJITU.
              05  LNK-S02-DOJITU-DAY       PIC  9(01)   OCCURS  31.
              05  LNK-S02-DOJITU-DAY2      PIC  9(01)   OCCURS  31.
              05  LNK-S02-DOJITU-SKJST     PIC  X(01)   OCCURS  31.
              05  LNK-S02-DOJITU-SKJED     PIC  X(01)   OCCURS  31.
      *
      *入院料と異なる食事算定保険組み合わせ
           03  LNK-S02-SKJHKN.
              05  LNK-S02-SKJHKN-HKNCOMBI  PIC  X(04)   OCCURS  31.
              05  LNK-S02-SKJHKN-HKNCOMBI-DOU
                                           PIC  X(04)   OCCURS  31.
      *       入院分の保険組み合わせ
              05  LNK-S02-SKJHKN-HKNCOMBI-XXX
                                           PIC  X(04)   OCCURS  31.
      *       同日再入院分の保険組み合わせ
              05  LNK-S02-SKJHKN-HKNCOMBI-DOU-XXX
                                           PIC  X(04)   OCCURS  31.
      *
      *入院料と異なる保険組み合わせでの食事算定チェックテーブル
           03  LNK-S02-SKJ-SANTEI.
              05  LNK-S02-SKJ-SANTEI-OCC       OCCURS   2.
                07  LNK-S02-SKJHKN-SRYYM        PIC  X(06).
                07  LNK-S02-SKJHKN-SET          PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-AAA-TAIFLG   PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-AAA-ARIFLG   PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-BBB-TAIFLG   PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-BBB-ARIFLG   PIC  X(01)  OCCURS  31.
      *
      *高齢者他保険入院フラグ
           03  LNK-S02-KOREI-TANYUIN       PIC  X(01).
      *
      *第三者行為分の計算優先順位が上位となる診療月フラグ
           03  LNK-S02-DAISANYUSENFLG      PIC  X(01).
      *
      *日別点数
           03  LNK-S02-DAYTEN.
              05  LNK-S02-DAYTEN-OCC            OCCURS  10.
                07  LNK-S02-DAYTEN-SRYYM        PIC  X(06).
                07  LNK-S02-DAYTEN-HKNCOMBINUM  PIC  X(04).
                07  LNK-S02-DAYTEN-OCC2.
                  09  LNK-S02-DAYTEN-HKNTEN     PIC  9(07)  OCCURS  31.
      *
      *前日までのマル長の一部負担金の残
           03  LNK-S02-CYOKI.
              05  LNK-S02-CYOKI-OCC             OCCURS  10.
                07  LNK-S02-CYOKI-FLG           PIC  9(01).
                07  LNK-S02-CYOKI-ZAN1          PIC  9(07).
                07  LNK-S02-CYOKI-ZAN2          PIC  9(07).
                07  LNK-S02-CYOKI-ZAN3          PIC  9(07).
      *
      *計算対象の収納データＩＤＸ
           03  LNK-S02-IDX                      PIC  9(02).
      *
      *チェック用ＩＤＸ
           03  LNK-S02-IDX2                     PIC  9(02).
      *
      *復帰コード
           03  LNK-S02-RC                       PIC S9(09)   BINARY.
      *
