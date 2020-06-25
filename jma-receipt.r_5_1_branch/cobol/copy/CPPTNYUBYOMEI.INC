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
      *    患者入院病名              COPY (CPPTNYUBYOMEI.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTNYUBYOMEI-REC.
      *キー
           03  PTNYUBYOMEI-KEY.
      *医療機関ＩＤ
               05  PTNYUBYOMEI-HOSPID              PIC  X(24).
      *患者ＩＤ
               05  PTNYUBYOMEI-PTID                PIC  9(10).
      *入院履歴番号（入院１回で＋１ずつ採番）
               05  PTNYUBYOMEI-RRKNUM              PIC  9(03).
      *病名コード
           03  PTNYUBYOMEI-BYOMEICD-1              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-2              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-3              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-4              PIC  X(07).
           03  PTNYUBYOMEI-BYOMEICD-5              PIC  X(07).
      *入力病名
           03  PTNYUBYOMEI-CHARTBYOMEI             PIC  X(160).
      *端末ＩＤ
           03  PTNYUBYOMEI-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  PTNYUBYOMEI-OPID                    PIC  X(16).
      *作成年月日
           03  PTNYUBYOMEI-CREYMD                  PIC  X(08).
      *更新年月日
           03  PTNYUBYOMEI-UPYMD                   PIC  X(08).
      *更新時間
           03  PTNYUBYOMEI-UPHMS                   PIC  X(06).
