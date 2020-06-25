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
      *    外来月別請求書パラメタ      COPY (CPORCHCM43.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−太田   04/03/14  今回請求額を追加
      ************************************************************
      *
       01  ORCHCM43AREA.
      *
           03  ORCHCM43-IN.
      *
      *        呼出元区分（1:統計業務/2:収納業務）
               05  ORCHCM43-KBN            PIC X(01).
      *        開始診療年月
               05  ORCHCM43-STSRYYM        PIC X(06).
      *        終了診療年月
               05  ORCHCM43-EDSRYYM        PIC X(06).
      *        発行方法
               05  ORCHCM43-HAKUHOUFLG     PIC X(01).
      *        発行日
               05  ORCHCM43-HAKYMD         PIC X(08).
      *
      *        ゼロ円請求書発行フラグ
               05  ORCHCM43-ZEROSKYFLG     PIC X(01).
      *
           03  ORCHCM43-OT.
      *        編集ページ数
               05  ORCHCM43-PAGE           PIC 9(08).
      *        リターンコード（０：正常／０以外：異常）
               05  ORCHCM43-RC             PIC 9(02).
