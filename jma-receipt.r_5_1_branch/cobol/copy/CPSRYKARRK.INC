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
      *    診療科履歴　  　　　 COPY  (CPSRYKARRK.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SRYKARRK-REC.
      *受診履歴
           03  KARRK-KEY.
      *  医療機関ＩＤ
               05  KARRK-HOSPNUM            PIC 9(2).
      *  患者ＩＤ
               05  KARRK-PTID              PIC  9(10).
      *  診療科
               05  KARRK-SRYKA             PIC  X(02).
      *
      *  初診日１
           03  KARRK-SYOSINYMD1            PIC  X(08).
      *  初診日２
           03  KARRK-SYOSINYMD2            PIC  X(08).
      *  最終受診日
           03  KARRK-LASTYMD               PIC  X(08).
      *端末ＩＤ
           03  KARRK-TERMID                PIC  X(16).
      *オペーレータＩＤ
           03  KARRK-OPID                  PIC  X(16).
      *作成年月日
           03  KARRK-CREYMD                PIC  X(08).
      *更新年月日
           03  KARRK-UPYMD                 PIC  X(08).
      *更新時間
           03  KARRK-UPHMS                 PIC  X(06).
