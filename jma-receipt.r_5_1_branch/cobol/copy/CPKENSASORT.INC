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
      ************************************************************
      ************************************************************
      *    検査分類　        　 COPY (CPKENSASORT.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  KENSASORT-REC.
      *キー
           03  KENSASORT-KEY.
      *検査分類コード
               05  KENSASORT-KNSBUNRUI     PIC  9(02).
      *診療コード
               05  KENSASORT-SRYCD         PIC  X(09).
      *表示連番
           03  KENSASORT-DSPSEQ            PIC  9(04).
      *端末ＩＤ
           03  KENSASORT-TERMID            PIC  X(32).
      *オペレータＩＤ
           03  KENSASORT-OPID              PIC  X(16).
      *作成年月日
           03  KENSASORT-CREYMD            PIC  X(08).
      *更新年月日
           03  KENSASORT-UPYMD             PIC  X(08).
      *更新時間
           03  KENSASORT-UPHMS             PIC  X(06).
