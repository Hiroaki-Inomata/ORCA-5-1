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
      *         システム管理マスタ     (CPSK9104.INC)            *
      *         （９１０４）                                     *
      *         医療情報代理機関情報                             *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-9104-REC.
           03  SYS-9104-KEY.
               05  SYS-9104-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-9104-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-9104-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-9104-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-9104-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-9104-KANRITBL.
      *電子レセプトデータ
               05  SYS-9104-RECEDEN     PIC X(01).
      *日医診療情報データ
               05  SYS-9104-JMAMR       PIC X(01).
      *予備
               05  SYS-9104-YOBI        PIC X(498).
      *端末ＩＤ
           03  SYS-9104-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-9104-OPID            PIC  X(16).
      *作成年月日
           03  SYS-9104-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-9104-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-9104-UPHMS           PIC  X(06).
