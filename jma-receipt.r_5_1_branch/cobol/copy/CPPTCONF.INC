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
      *    患者個別設定                  COPY (CPPTCONF.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTCONF-REC.
      *キー
           03  PTCONF-KEY.
      *医療機関ＩＤ
               05  PTCONF-HOSPNUM             PIC  9(2).
      *患者ＩＤ
               05  PTCONF-PTID                PIC  9(10).
      *設定ＫＥＹ (Configuration KEY)
               05  PTCONF-CKEY                PIC  X(25).
      *設定データ (Configuration DATA)
           03  PTCONF-CDATA                   PIC  X(10).
      *端末ＩＤ
           03  PTCONF-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  PTCONF-OPID                    PIC  X(16).
      *作成年月日
           03  PTCONF-CREYMD                  PIC  X(08).
      *更新年月日
           03  PTCONF-UPYMD                   PIC  X(08).
      *更新時間
           03  PTCONF-UPHMS                   PIC  X(06).
