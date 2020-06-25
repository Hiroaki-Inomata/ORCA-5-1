      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *          医療費請求書（複写式レセ） COPY (CPRMF10.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF102-REC.
           03  MF102-KEY.
      *患者番号
               05  MF102-PTNUM           PIC  X(20).
      *保険者番号
               05  MF102-HKNJANUM        PIC  X(08).
      *適用開始年月日
               05  MF102-TEKSTYMD        PIC  X(08).
      *レセプト種別
               05  MF102-RECESYUBETU     PIC  X(04).
      *帳票ＩＤ
           03  MF102-FORMID              PIC  X(20).
      *帳票データ
           03  MF102-PRTDATA             PIC  X(10000).
