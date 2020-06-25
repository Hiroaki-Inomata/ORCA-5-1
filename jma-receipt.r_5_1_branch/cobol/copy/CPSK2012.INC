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
      *         システム管理マスタ     (CPSK2012.INC)            *
      *         （２０１２）                                     *
      *         保険者マスタ付加情報                             *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-2012-REC.
           03  SYS-2012-KEY.
               05  SYS-2012-HOSPNUM         PIC  9(02).
      *管理コード
               05  SYS-2012-KANRICD         PIC  X(04).
      *区分コード
               05  SYS-2012-KBNCD           PIC  X(08).
      *有効開始年月日
               05  SYS-2012-STYUKYMD        PIC  X(08).
      *有効終了年月日
               05  SYS-2012-EDYUKYMD        PIC  X(08).
      *
      *感３７の２（法別１０）
           03  SYS-2012-HBTNUM10KBN         PIC  X(01).
           03  SYS-2012-HBTNUM10-TAIKBN     PIC  X(01).
      *結核入院　（法別１１）
           03  SYS-2012-HBTNUM11KBN         PIC  X(01).
           03  SYS-2012-HBTNUM11-TAIKBN     PIC  X(01).
      *精神入院　（法別２０）
           03  SYS-2012-HBTNUM20KBN         PIC  X(01).
           03  SYS-2012-HBTNUM20-TAIKBN     PIC  X(01).
      *精神通院　（法別２１）
           03  SYS-2012-HBTNUM21KBN         PIC  X(01).
           03  SYS-2012-HBTNUM21-TAIKBN     PIC  X(01).
      *
      *予備
           03  SYS-2012-YOBI                PIC  X(492).
      *端末ＩＤ
           03  SYS-2012-TERMID              PIC  X(16).
      *オペレータＩＤ
           03  SYS-2012-OPID                PIC  X(16).
      *作成年月日
           03  SYS-2012-CREYMD              PIC  X(08).
      *更新年月日
           03  SYS-2012-UPYMD               PIC  X(08).
      *更新時間
           03  SYS-2012-UPHMS               PIC  X(06).
      *主キー更新用項目
           03  SYS-2012-UPD-KEY.
               05  SYS-2012-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2012-UPD-KANRICD     PIC X(04).
               05  SYS-2012-UPD-KBNCD       PIC X(08).
               05  SYS-2012-UPD-STYUKYMD    PIC X(08).
               05  SYS-2012-UPD-EDYUKYMD    PIC X(08).
