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
      *    入院オーダー会計作成  COPY (CPORCSODRNACCT.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付     内容
      ************************************************************
       01  SODRNACCT-AREA.
      *    異動事由
           03  SODRNACCT-ORDERID       PIC X(04).
      *    入院料区分（01:一般／02:特定入院）
           03  SODRNACCT-NYUKBN        PIC X(02).
      *    入院料加算
           03  SODRNACCT-KSN-G.
               05  SODRNACCT-KSN-MAX   PIC 9(03).
               05  SODRNACCT-KSN-SRYCD PIC X(09)   OCCURS  3.
      *    外泊・食事起算日
           03  SODRNACCT-STARTYMD      PIC X(08).
      *    外泊終了日
           03  SODRNACCT-ENDYMD        PIC X(08).
      *    外泊・食事時間
           03  SODRNACCT-SKJTIME       PIC X(02).
      *    日数
           03  SODRNACCT-DAY-G.
               05  SODRNACCT-DAY       PIC 9(1)
                                       OCCURS  31.
      *    食事継続区分
           03  SODRNACCT-SAME-AFTER    PIC X(1).
      *
      *    診療科（転科歴）
           03  SODRNACCT-SRYKA-G.
               05  SODRNACCT-SRYKA-CNT PIC 9(05).
               05  SODRNACCT-OCC       OCCURS  50.
                   07  SODRNACCT-SRYKA PIC X(02).
      *
           03  SODRNACCT-OPID          PIC X(16).
           03  SODRNACCT-UPYMD         PIC X(08).
           03  SODRNACCT-UPHMS         PIC X(06).
      *
      *    返却値
           03  SODRNACCT-RC            PIC S9(02).
