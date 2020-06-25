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
      *****************************************************************
      *  予約雛形マスター       (CPYYKEXAMPLE.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  YYKEXAMPLE-REC.
           03  YYKEXAMPLE-KEY.
      *医療機関ＩＤ
               05  YYKEXAMPLE-HOSPNUM       PIC 9(2).
      *連番
               05  YYKEXAMPLE-RENNUM        PIC 9(03).
      *表示区分
           03  YYKEXAMPLE-HYOKBN            PIC 9(01).
      *雛形
           03  YYKEXAMPLE-COMMENT           PIC X(100).
      *端末ＩＤ
           03  YYKEXAMPLE-TERMID            PIC X(16).
      *オペーレータＩＤ
           03  YYKEXAMPLE-OPID              PIC X(16).
      *作成年月日
           03  YYKEXAMPLE-CREYMD            PIC X(08).
      *更新年月日
           03  YYKEXAMPLE-UPYMD             PIC X(08).
      *更新時間
           03  YYKEXAMPLE-UPHMS             PIC X(06).
