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
      *    作業用保険者           COPY (CPHKNJAINFWK.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HKNJAINFWK-REC.
      *処理区分
           03  HKNJAWK-SYORIKBN              PIC  X(01).
           03  HKNJAWK-LEVEL                 PIC  X(01).
      *データ新旧
           03  HKNJAWK-TBL                   OCCURS  2.
      *医療機関ＩＤ
               05  HKNJAWK-HOSPID            PIC  X(24).
      *保険者番号
               05  HKNJAWK-HKNJANUM          PIC  X(08).
      *保険者名
               05  HKNJAWK-HKNJANAME         PIC  X(200).
      *保険者名略称１
               05  HKNJAWK-HKNJANAME-TAN1    PIC  X(100).
      *保険者名略称２
               05  HKNJAWK-HKNJANAME-TAN2    PIC  X(40).
      *保険者名略称３
               05  HKNJAWK-HKNJANAME-TAN3    PIC  X(40).
      *保険番号
               05  HKNJAWK-HKNNUM            PIC  X(03).
      *給付割合（本人・外来）
               05  HKNJAWK-HON-GAIKYURATE    PIC  9(03).
      *給付割合（本人・入院）
               05  HKNJAWK-HON-NYUKYURATE    PIC  9(03).
      *給付割合（家族・外来）
               05  HKNJAWK-KZK-GAIKYURATE    PIC  9(03).
      *給付割合（家族・入院）
               05  HKNJA-KZK-NYUKYURATE      PIC  9(03).
      *郵便番号
               05  HKNJAWK-POST              PIC  X(07).
      *住所
               05  HKNJAWK-ADRS              PIC  X(200).
      *番地方書
               05  HKNJAWK-BANTI             PIC  X(200).
      *電話番号
               05  HKNJAWK-TEL               PIC  X(15).
      *記号
               05  HKNJAWK-KIGO              PIC  X(80).
      *異動区分
               05  HKNJAWK-IDOKBN            PIC  X(01).
      *異動年月日
               05  HKNJAWK-IDOYMD            PIC  X(08).
      *端末ＩＤ
               05  HKNJAWK-TERMID            PIC  X(16).
      *オペーレータＩＤ
               05  HKNJAWK-OPID              PIC  X(16).
      *作成年月日
               05  HKNJAWK-CREYMD            PIC  X(08).
      *更新年月日
               05  HKNJAWK-UPYMD             PIC  X(08).
      *更新時間
               05  HKNJAWK-UPHMS             PIC  X(06).
