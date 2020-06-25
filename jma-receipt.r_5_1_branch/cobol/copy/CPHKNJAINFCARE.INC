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
      *    保険者（介護保険）      COPY (CPHKNJAINFCARE.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HKNJAINFCARE-REC.
      *保険者番号
           03  HKNJAC-HKNJANUM               PIC  X(08).
      *保険者名
           03  HKNJAC-HKNJANAME              PIC  X(200).
      *郵便番号
           03  HKNJAC-POST                   PIC  X(07).
      *住所
           03  HKNJAC-ADRS                   PIC  X(200).
      *番地方書
           03  HKNJAC-BANTI                  PIC  X(200).
      *電話番号
           03  HKNJAC-TEL                    PIC  X(15).
      *異動区分
           03  HKNJAC-IDOKBN                 PIC  X(01).
      *異動年月日
           03  HKNJAC-IDOYMD                 PIC  X(08).
      *端末ＩＤ
           03  HKNJAC-TERMID                 PIC  X(16).
      *オペーレータＩＤ
           03  HKNJAC-OPID                   PIC  X(16).
      *作成年月日
           03  HKNJAC-CREYMD                 PIC  X(08).
      *更新年月日
           03  HKNJAC-UPYMD                  PIC  X(08).
      *更新時間
           03  HKNJAC-UPHMS                  PIC  X(06).
