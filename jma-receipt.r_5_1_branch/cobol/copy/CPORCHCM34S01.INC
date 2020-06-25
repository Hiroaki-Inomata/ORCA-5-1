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
      *    入院会計作成用パラメタ　　　COPY (CPORCHCM34S01.INC)
      *                                使用プログラム (ORCHCM34S01)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月
      *      保険組合せ
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  HCM34S01-AREA.
      *
      *医療機関番号
           03  HCM34S01-HOSPNUM                PIC 9(2).
      *患者ＩＤ
           03  HCM34S01-PTID                   PIC 9(10).
      *算定年月
           03  HCM34S01-SANTEIYM               PIC X(06).
      *期間指定区分
           03  HCM34S01-KIKANKBN               PIC X(01).
      *算定期間（診療費明細書からの呼び出しの際は設定する）
           03  HCM34S01-SKYSTYMD               PIC X(08).
           03  HCM34S01-SKYEDYMD               PIC X(08).
      *請求開始日同日再入院フラグ
           03  HCM34S01-DOJITUFLG-ST           PIC  X(01).
      *開始日入院食事区分（０：食なし、１：朝から、２：昼から、３：夕から）
           03  HCM34S01-TIMEKBN-ST             PIC  X(01).
      *請求終了日同日再入院フラグ
           03  HCM34S01-DOJITUFLG-ED           PIC  X(01).
      *終了日入院食事区分（０：食なし、１：朝まで、２：昼まで、３：夕まで）
           03  HCM34S01-TIMEKBN-ED             PIC  X(01).
      *保険組合せ
           03  HCM34S01-HKNCOMBI               PIC 9(04).
      *保険番号
           03  HCM34S01-HKNNUM                 PIC X(03).
      *診療科区分
           03  HCM34S01-SRYKAKBN               PIC X(01).
      *診療科
           03  HCM34S01-SRYKA                  PIC X(02).
      *診療科日付
           03  HCM34S01-SRYKADAY-G.
               05  HCM34S01-SRYKADAY           PIC 9(01)
                                               OCCURS  31.
      *外泊編集パターン区分
           03  HCM34S01-EXGAIHAKU              PIC X(01).
      *食事編集パターン区分
           03  HCM34S01-EXSYOKUJI              PIC X(01).
      *標準負担額編集区分
           03  HCM34S01-SKJFTN                 PIC X(01).
      *返却項目
           03  HCM34S01-HENKYAKU-AREA.
      *
      *選定入院料患者自己負担額
               05  HCM34S01-SENTEI-PTFTN       PIC 9(08).
      *室料差額区分コード
               05  HCM34S01-SAGAKUCD           PIC  X(02)
                                               OCCURS  31.
      *
      *返却テーブル
               05  HCM34S01-HENKYAKU-MAX       PIC 9(05).
               05  HCM34S01-HENKYAKU-TBL       OCCURS  100.
      *入院基本料診療区分
                   07  HCM34S01-SRYKBN         PIC  X(02).
      *診療種別
                   07  HCM34S01-SRYSYUKBN      PIC  X(03).
      *剤識別区分（１：入院料　２：外泊　３：室料差額　４：食事
      *            ５：保険組合せ　６：入院料加算　７：療養担当手当）
                   07  HCM34S01-ZAISKBKBN      PIC  X(01).
      *並び優先順位
                   07  HCM34S01-PRIORITY-G.
                       09  HCM34S01-PRIORITY   PIC  9(03).
                       09  HCM34S01-PRIORITY2  PIC  9(03).
                       09  HCM34S01-PRIORITY3  PIC  9(03).
      *剤点数
                   07  HCM34S01-ZAITEN         PIC  9(08).
      *マイナスフラグ(剤点数がプラス:0/マイナス:1)
                   07  HCM34S01-ZAIMINUS       PIC  9(01).
      *剤回数
                   07  HCM34S01-ZAIKAISU       PIC  9(07).
      *明細数
                   07  HCM34S01-MEISAISU       PIC  9(07).
      *診療コード合計
                   07  HCM34S01-SRYCDTOTAL     PIC  9(14).
      *数量合計
                   07  HCM34S01-SURYOUTOTAL    PIC  9(07)V9(03).
      *累計合計
                   07  HCM34S01-RUIKEITOTAL    PIC  9(15)V9(03).
                   07  HCM34S01-SRYCD-OCC      OCCURS  20.
      *数量
                       09  HCM34S01-SURYO      PIC  9(05).
      *診療コード
                       09  HCM34S01-SRYCD      PIC  X(09).
      *診療回数テーブル
                   07  HCM34S01-DAY-TBL.
                       09  HCM34S01-DAY        PIC  9(03)
                                               OCCURS  31.
      *復帰コード（１：算定年月エラー　２：患者検索エラー　３：入院なし）
           03  HCM34S01-RC                     PIC S9(9) BINARY.
      *
