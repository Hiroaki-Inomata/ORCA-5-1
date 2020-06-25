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
      *    患者介護保険情報         COPY  (CPPTCARE-HKNINF.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTCARE-HKNINF-REC.
      *キー
           03  PTCRHKN-KEY.
      *医療機関ＩＤ
               05  PTCRHKN-HOSPNUM         PIC  9(02).
      *患者ＩＤ
               05  PTCRHKN-PTID            PIC  9(10).
      *適用開始年月日
               05  PTCRHKN-TEKSTYMD        PIC  X(08).
      *適用終了年月日
           03  PTCRHKN-TEKEDYMD            PIC  X(08).
      *保険者番号
           03  PTCRHKN-HKNJANUM            PIC  X(06).
      *被保険者名
           03  PTCRHKN-HIHKNJANUM          PIC  X(10).
      *端末ＩＤ
           03  PTCRHKN-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  PTCRHKN-OPID                PIC  X(16).
      *作成年月日
           03  PTCRHKN-CREYMD              PIC  X(08).
      *更新年月日
           03  PTCRHKN-UPYMD               PIC  X(08).
      *更新時間
           03  PTCRHKN-UPHMS               PIC  X(06).
