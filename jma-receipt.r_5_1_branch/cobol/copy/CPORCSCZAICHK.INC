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
      *    診療行為剤有無チェックパラメタ　　COPY (CPORCSCZAICHK.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCZAICHKAREA.
      *
      *区分（１：チェック）
           03  ORCSCZAICHK-KBN                 PIC  X(01).
      *入力
           03  ORCSCZAICHK-IN-AREA.
      *診療日
               05  ORCSCZAICHK-SRYYM               PIC  X(06).
      *診療科
               05  ORCSCZAICHK-SRYKA               PIC  X(02).
      *診療区分
               05  ORCSCZAICHK-SRYKBN              PIC  X(02).
      *診療種別
               05  ORCSCZAICHK-SRYSYUKBN           PIC  X(03).
      *保険組合せ
               05  ORCSCZAICHK-HKNCOMBI            PIC  X(04).
      *剤点数
               05  ORCSCZAICHK-ZAITEN              PIC  9(08).
      *診療コード計
               05  ORCSCZAICHK-SRYCDTOTAL          PIC  9(14).
      *数量計
               05  ORCSCZAICHK-SURYOUTOTAL         PIC  9(07)V9(05).
      *明細数
               05  ORCSCZAICHK-MEISAISU            PIC  9(07).
      *剤回数
               05  ORCSCZAICHK-ZAIKAISU            PIC  9(07).
      *自保区分（包括剤）
               05  ORCSCZAICHK-JIHOKBN             PIC  X(01).
      *        明細内容
               05  ORCSCZAICHK-MEISAI-TBL.
                   07  ORCSCZAICHK-MEISAI-OCC      OCCURS   50.
                       09  ORCSCZAICHK-SRYCD       PIC  X(09).
                       09  ORCSCZAICHK-SRYSURYO    PIC  9(05)V9(05).
                       09  ORCSCZAICHK-SRYKAISU    PIC  9(03).
                       09  ORCSCZAICHK-INPUTCOMENT PIC  X(140).
                       09  ORCSCZAICHK-INPUTCHI-G.
                           11  ORCSCZAICHK-ATAI    PIC  X(10)
                                                        OCCURS  5.
      *                換算値数量
                       09  ORCSCZAICHK-KANSURYO    PIC  9(05)V9(05).
                       09  ORCSCZAICHK-INPUTKBN    PIC  X(01).
      *                自費金額
                       09  ORCSCZAICHK-JIHIMONEY   PIC  9(07).
      *出力
           03  ORCSCZAICHK-OUT-AREA.
      *    対象剤番号
               05  ORCSCZAICHK-ZAINUM              PIC  9(08).
      *
