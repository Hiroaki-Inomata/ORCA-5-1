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
      *    保険者          COPY (CPHKNJA.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HKNJAINF-REC.
      *医療機関ＩＤ
           03  HKNJA-HOSPNUM                  PIC 9(2).
      *保険者番号
           03  HKNJA-HKNJANUM                PIC  X(08).
      *保険者名
           03  HKNJA-HKNJANAME               PIC  X(200).
      *保険者名略称１
           03  HKNJA-HKNJANAME-TAN1          PIC  X(100).
      *保険者名略称２
           03  HKNJA-HKNJANAME-TAN2          PIC  X(40).
      *保険者名略称３
           03  HKNJA-HKNJANAME-TAN3          PIC  X(40).
      *保険番号
           03  HKNJA-HKNNUM                  PIC  X(03).
      *給付割合（本人・外来）
           03  HKNJA-HON-GAIKYURATE          PIC  9(03).
      *給付割合（本人・入院）
           03  HKNJA-HON-NYUKYURATE          PIC  9(03).
      *給付割合（家族・外来）
           03  HKNJA-KZK-GAIKYURATE          PIC  9(03).
      *給付割合（家族・入院）
           03  HKNJA-KZK-NYUKYURATE          PIC  9(03).
      *郵便番号
           03  HKNJA-POST                    PIC  X(07).
      *住所
           03  HKNJA-ADRS                    PIC  X(200).
      *番地方書
           03  HKNJA-BANTI                   PIC  X(200).
      *電話番号
           03  HKNJA-TEL                     PIC  X(15).
      *記号
           03  HKNJA-KIGO                    PIC  X(80).
      *異動区分
           03  HKNJA-IDOKBN                  PIC  X(01).
      *異動年月日
           03  HKNJA-IDOYMD                  PIC  X(08).
      *端末ＩＤ
           03  HKNJA-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  HKNJA-OPID                    PIC  X(16).
      *作成年月日
           03  HKNJA-CREYMD                  PIC  X(08).
      *更新年月日
           03  HKNJA-UPYMD                   PIC  X(08).
      *更新時間
           03  HKNJA-UPHMS                   PIC  X(06).
