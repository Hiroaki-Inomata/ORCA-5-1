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
      *         チェックマスタ４     (CPCK2002.INC)              *
      *         （２００２）                                     *
      *         前回確認項目                                     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CK4-2002-REC.
           03  CK4-2002-KEY.
               05  CK4-2002-HOSPNUM     PIC 9(2).
      *管理コード
               05  CK4-2002-KANRICD     PIC  X(04).
      *区分コード
               05  CK4-2002-KBNCD       PIC  X(08).
      *有効開始年月日
               05  CK4-2002-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  CK4-2002-EDYUKYMD    PIC  X(08).
      *患者ＩＤ
           03  CK4-2002-PTID            PIC  9(10).
      *管理テーブル
           03  CK4-2002-TBL.
               05  CK4-2002-NAME        PIC  X(30).
               05  CK4-2002-CHKKBN-G.
                   07  CK4-2002-CHKKBN  PIC  X(01)  OCCURS 50.
               05  CK4-2002-INGAIPROCKBN
                                        PIC  X(01).
               05  CK4-2002-FILLER      PIC  X(419).
      *端末ＩＤ
           03  CK4-2002-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  CK4-2002-OPID            PIC  X(16).
      *作成年月日
           03  CK4-2002-CREYMD          PIC  X(08).
      *更新年月日
           03  CK4-2002-UPYMD           PIC  X(08).
      *更新時間
           03  CK4-2002-UPHMS           PIC  X(06).
