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
      *----------------------------------------------------------*
      *    注射処方箋出力パラメタ         COPY (CPORCHC502.INC)
      *                                        (ORCHC502 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHC502AREA.
      *    処理区分（１：頭書）
           03  ORCHC502-SYORIKBN       PIC 9(01).
      *  帳票オーダーキー
           03  ORCHC502-ORDER-KEY.
      *  患者番号
               05  ORCHC502-PTNUM         PIC  X(20).
      *  診療科
               05  ORCHC502-SRYKA         PIC  X(02).
      *  保険組合せ
               05  ORCHC502-HKNCOMBI      PIC  X(04).
      *  診療日付
               05  ORCHC502-SRYYMD        PIC  X(08).
      *  システム日付
               05  ORCHC502-SYSYMD        PIC  X(08).
      *  発行日
               05  ORCHC502-PRINTYMD      PIC X(08).
      *  発行時間
               05  ORCHC502-PRINTHMS      PIC X(06).
      *  ドクター（頭書の時のみ）
           03  ORCHC502-DRCD           PIC X(05).
      *
      *    発行区分１から４
           03  ORCHC502-CHKPRT         PIC 9(01)
                                       OCCURS   4.
      *
           03  ORCHC502-OUT-AREA.
      *        リターンコード（０：正常／０以外：異常）
               05  ORCHC502-RC             PIC 9(02).
      *
