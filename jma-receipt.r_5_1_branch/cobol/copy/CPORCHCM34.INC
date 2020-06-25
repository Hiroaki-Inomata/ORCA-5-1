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
      *    会計カードサブパラメタ　  COPY (CPORCHCM34.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHCM34AREA.
           03  ORCHCM34-IN.
      *        処理区分
      *       １：オンライン／２：一括
               05  ORCHCM34-KBN            PIC X(01).
      *        診療年月
               05  ORCHCM34-SRYYM          PIC X(06).
      *        入外区分
               05  ORCHCM34-NYUGAIKBN      PIC X(01).
      *        病名表示非表示区分
               05  ORCHCM34-HIDEBYOKBN     PIC X(01).
      *        診療科区分
               05  ORCHCM34-SRYKAKBN       PIC X(01).
      *        診療科
               05  ORCHCM34-SRYKA          PIC X(02).
      *        診療科日付
               05  ORCHCM34-SRYKADAY-G.
                   07  ORCHCM34-SRYKADAY   PIC 9(01)
                                           OCCURS  31.
      *        保険区分 0:保険毎に改頁1:社保2:国保3:その他
               05  ORCHCM34-HOKEN          PIC X(01).
      *        印刷区分 1:対象診療行為がなくても印刷する
               05  ORCHCM34-PRTCTRLKBN     PIC X(01).
      *        期間区分 1:期間指定あり
               05  ORCHCM34-KIKANKBN       PIC X(01).
               05  ORCHCM34-STDAY          PIC 9(02).
               05  ORCHCM34-EDDAY          PIC 9(02).
               05  ORCHCM34-STSRYYMD       PIC X(08).
               05  ORCHCM34-EDSRYYMD       PIC X(08).
      *        包括非表示区分
               05  ORCHCM34-HIDEHOKATSU    PIC X(01).
      *        医療区分、ＡＤＬ点数編集区分
               05  ORCHCM34-EDIT-MEDADL    PIC X(01).
      *        一括パラメタ
               05  ORCHCM34-PRTAREA        PIC X(25000).
      *
           03  ORCHCM34-OT.
      *        編集ページ数
               05  ORCHCM34-PAGE           PIC 9(08).
      *        リターンコード（０：正常／０以外：異常）
               05  ORCHCM34-RC             PIC 9(02).
