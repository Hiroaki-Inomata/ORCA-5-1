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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者保険　補助区分名称編集サブパラメタ
      *                                         (CPORAPI12SUB1.INC)
      ******************************************************************
       01  ORAPI012SUB1AREA.
      *    保険種類番号
           03  API012SUB1-HKNNUM        PIC X(03).
      *    補助区分
           03  API012SUB1-HOJOKBN       PIC X(01).
      *    処理日付（未使用）
           03  API012SUB1-SYSYMD        PIC X(08).
      *
      *返却
           03  API012SUB1-OUT-AREA.
      *        補助区分名称
               05  API012SUB1-HOJOKBN-NAME  PIC X(20).
      *        負担割合（高齢者）
               05  API012SUB1-RATE          PIC 9(03).
      *        エラー区分（１：エラー）
               05  API012SUB1-ERRCD         PIC 9(01).
      *
      *
