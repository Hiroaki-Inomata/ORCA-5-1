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
      *    患者他院歴付加情報        COPY (CPPTTAINFUKA.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTTAINFUKA-REC.
      *キー
           03  PTTAINFUKA-KEY.
      *医療機関ＩＤ
               05  PTTAINFUKA-HOSPNUM              PIC 9(2).
      *患者ＩＤ
               05  PTTAINFUKA-PTID                PIC  9(10).
      *入院履歴番号（入院１回で＋１ずつ採番）
               05  PTTAINFUKA-RRKNUM              PIC  9(03).
      *病名
           03  PTTAINFUKA-BYOMEI                  PIC  X(80)
                                                  OCCURS   5.
      *入院基本料
           03  PTTAINFUKA-KHNRYO                  PIC  X(80).
      *端末ＩＤ
           03  PTTAINFUKA-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  PTTAINFUKA-OPID                    PIC  X(16).
      *作成年月日
           03  PTTAINFUKA-CREYMD                  PIC  X(08).
      *更新年月日
           03  PTTAINFUKA-UPYMD                   PIC  X(08).
      *更新時間
           03  PTTAINFUKA-UPHMS                   PIC  X(06).
