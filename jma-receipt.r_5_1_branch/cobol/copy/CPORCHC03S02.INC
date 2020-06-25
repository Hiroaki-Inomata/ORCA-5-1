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
      *    請求書兼領収書収納編集パラメタ  COPY (CPORCHC03S02.INC)
      *                                          (ORCHC03S02 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHC03S02AREA.
           03  ORCHC03S02-KEY.
               05  ORCHC03S02-HOSPNUM      PIC 9(2).
               05  ORCHC03S02-GYOUMUID     PIC X(03).
               05  ORCHC03S02-TERMID       PIC X(64).
               05  ORCHC03S02-FILEMEI.
                   07  ORCHC03S02-FILEMEI-YMD
                                           PIC X(08).
                   07  ORCHC03S02-FILEMEI-HMS
                                           PIC X(06).
                   07  ORCHC03S02-FILEMEI-CD
                                           PIC X(06).
           03  ORCHC03S02-CNT              PIC 9(05).
           03  ORCHC03S02-RC               PIC 9(02).
      *
