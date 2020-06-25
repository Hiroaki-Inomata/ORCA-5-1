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
      *         システム管理マスタ     (CPSK2007.INC)            *
      *         （２００７）                                     *
      *         公費番号まとめ情報                               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    16/09/13  公費番号の追加
      ************************************************************
       01  SYS-2007-REC.
           03  SYS-2007-KEY.
               05  SYS-2007-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2007-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2007-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2007-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2007-EDYUKYMD    PIC  X(08).
      *公費情報
           03  SYS-2007-KOH-AREA.
      *公費番号
               05  SYS-2007-KOHNUM      PIC  X(03)   OCCURS  10.
      *名称
           03  SYS-2007-MEISYO-OCC      OCCURS  2.
      *
               05  SYS-2007-MEISYO      PIC  X(44).
      *公費情報
           03  SYS-2007-KOH1-AREA.
      *公費番号
               05  SYS-2007-KOHNUM1     PIC  X(03)   OCCURS  20.
      *予備
           03  SYS-2007-YOBI            PIC  X(322).
      *端末ＩＤ
           03  SYS-2007-TERMID          PIC  X(16).
      *オペレータＩＤ
           03  SYS-2007-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2007-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2007-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2007-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2007-UPD-KEY.
               05  SYS-2007-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2007-UPD-KANRICD     PIC X(04).
               05  SYS-2007-UPD-KBNCD       PIC X(08).
               05  SYS-2007-UPD-STYUKYMD    PIC X(08).
               05  SYS-2007-UPD-EDYUKYMD    PIC X(08).
