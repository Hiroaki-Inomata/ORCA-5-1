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
      *    入院診療行為更新パラメタ　　COPY (CPORCSCNYUIN.INC)
      *                           使用プログラム (ORCGK08,ORCSNYUACCT)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCNYUINAREA.
      *
      *区分（１：診療行為作成、２：診療行為削除（退院））
           03  ORCSCNYUIN-KBN                        PIC  X(01).
      *対象日付（退院日）
           03  ORCSCNYUIN-YMD                        PIC  X(08).
      *退院日
           03  ORCSCNYUIN-TAIINYMD                   PIC  X(08).
      *同日再入院区分
           03  ORCSCNYUIN-DOUJITSUKBN                PIC  X(01).
      *
