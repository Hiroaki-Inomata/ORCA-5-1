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
      *          医療費請求書 COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/02/22  他法法別番号と他法負担額追加
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *法別番号
               05  MF100-HBTNUM          PIC  X(02).
      *データエリア
           03  MF100-DATA-AREA.
               05    MF100-NYUGAI-TBL  OCCURS 2.
      *件数
                   07  MF100-KENSU       PIC  9(04).
      *点数
                   07  MF100-TENSU       PIC  9(07).
      *公費分点数
                   07  MF100-KOHTENSU    PIC  9(07).
      *薬剤一部負担金
                   07  MF100-YKZFTN      PIC  9(07).
      *老人一部負担金
                   07  MF100-ITBFTN      PIC  9(07).
      *食事件数
               05  MF100-SKENSU          PIC  9(04).
      *食事金額
               05  MF100-SKINGK          PIC  9(07).
      *食事標準負担額
               05  MF100-SFTNGK          PIC  9(07).
      *他法法別番号
           03  MF100-TAHOUHBT            PIC  X(02).
      *他法負担額
           03  MF100-TAHOUFTN            PIC  9(07).
