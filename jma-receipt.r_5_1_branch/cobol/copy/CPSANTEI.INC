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
      *    算定履歴          COPY (CPSANTEI.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SANTEI-REC.
           03  SANTEI-KEY.
      *医療機関ＩＤ
               05  SANTEI-HOSPNUM          PIC 9(2).
      *患者ＩＤ
               05  SANTEI-PTID            PIC  9(10).
      *診療年月
               05  SANTEI-SRYYM           PIC  X(06).
      *診療コード
               05  SANTEI-SRYCD           PIC  X(09).
      *入外区分
               05  SANTEI-NYUGAIKBN       PIC  X(01).
      *診療科
               05  SANTEI-SRYKA           PIC  X(02).
      *保険組合番号
               05  SANTEI-HKNCOMBINUM     PIC  9(04).
      *初回算定日
           03  SANTEI-FSANTEIYMD       PIC  X(08).
      *当月初回算定日
           03  SANTEI-MSANTEID         PIC  X(02).
      *当月算定回数
           03  SANTEI-MSANTEICNT       PIC  9(03).
      *診療回数テーブル（１日から３１日）
           03  SANTEI-DAY-AREA.
               05  SANTEI-DAY          PIC  9(03)
                                       OCCURS   31.
      *当月算定点数対象フラグ
           03  SANTEI-MSANTEIFLG       PIC  9(01).
      *当月算定点数
           03  SANTEI-MSANTEITEN       PIC  9(08).
      *当月算定点数区分
           03  SANTEI-MSANTEITENKBN    PIC  9(01).
      *端末ＩＤ
           03  SANTEI-TERMID           PIC  X(16).
      *オペーレータＩＤ
           03  SANTEI-OPID             PIC  X(16).
      *作成年月日
           03  SANTEI-CREYMD           PIC  X(08).
      *更新年月日
           03  SANTEI-UPYMD            PIC  X(08).
      *更新時間
           03  SANTEI-UPHMS            PIC  X(06).
      *
