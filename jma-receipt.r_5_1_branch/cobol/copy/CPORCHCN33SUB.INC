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
      *    カルテ３号紙用入院会計取得パラメタ　COPY (CPORCHCN33SUB.INC)
      *                                使用プログラム (ORCHCN33SUB.CBL)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月
      *      診療科
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHCN33SUB-AREA.
      *
      *医療機関ＩＤ
           03  LNK-ORCHCN33SUB-HOSPNUM               PIC 9(2).
      *患者ＩＤ
           03  LNK-ORCHCN33SUB-PTID                  PIC  9(10).
      *算定年月
           03  LNK-ORCHCN33SUB-SANTEIYM              PIC  X(06).
      *診療科（００：全科）
           03  LNK-ORCHCN33SUB-SRYKA                 PIC  X(02).
      *処理区分（空白：カルテ３号紙用　１：診療科別入院点数返却　
      *          ２：診療科別入院日数返却）
           03  LNK-ORCHCN33SUB-SHORIKBN              PIC  X(01).
      *返却項目
           03  LNK-ORCHCN33SUB-HENKYAKU-AREA.
      *カルテ３号紙用入院返却テーブル
             04  LNK-ORCHCN33SUB-HENKYAKU-TABLE.
      *入院料返却テーブル
               05  LNK-ORCHCN33SUB-HENKYAKU-TBL      OCCURS  31.
      *入院点数
                   07  LNK-ORCHCN33SUB-TEN           PIC S9(07).
      *その他点数（療養担当手当）
                   07  LNK-ORCHCN33SUB-ETCTEN        PIC  9(07).
      *食事療養
                   07  LNK-ORCHCN33SUB-SKJ           PIC  9(07).
      *診療科別入院点数返却テーブル
             04  LNK-ORCHCN33SUB-HENKYAKU-TABLE2.
      *保険組合せ返却テーブル
               05  LNK-ORCHCN33SUB-HENKYAKU-TBL2     OCCURS  10.
      *保険組合せ番号
                   07  LNK-ORCHCN33SUB-HENKYAKU-HKNCOM  PIC 9(04).
      *診療科返却テーブル
                   07  LNK-ORCHCN33SUB-HENKYAKU-TBL3     OCCURS  50.
      *診療科
                       09  LNK-ORCHCN33SUB-NYUINKA   PIC  X(02).
      *開始日
                       09  LNK-ORCHCN33SUB-STDAY     PIC  9(02).
      *終了日
                       09  LNK-ORCHCN33SUB-EDDAY     PIC  9(02).
      *点数
                       09  LNK-ORCHCN33SUB-TENSU     PIC S9(07).
      *日数
                       09  LNK-ORCHCN33SUB-DAY       PIC  9(02).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー　３：診療科エラー
      *            ４：入院会計なし　　５：入院履歴なし）
           03  LNK-ORCHCN33SUB-RC                    PIC S9(9) BINARY.
      *
