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
      *         システム管理マスタ     (CPSK1031.INC)  
      *         （１０３ １）                            
      *         出力先プリンタ名割り当て情報                  
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1031-REC.
           03  SYS-1031-KEY.
               05  SYS-1031-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1031-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1031-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1031-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1031-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1031-TBL.
      *県番号
               05  SYS-1031-PRTNM-TBL   OCCURS  20.
      *点数表区分
                   07  SYS-1031-PRTNM   PIC  X(16).
      *予備
           03  SYS-1031-YOBI            PIC  X(180).
      *端末ＩＤ
           03  SYS-1031-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1031-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1031-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1031-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1031-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1031-UPD-KEY.
               05  SYS-1031-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1031-UPD-KANRICD     PIC X(04).
               05  SYS-1031-UPD-KBNCD       PIC X(08).
               05  SYS-1031-UPD-STYUKYMD    PIC X(08).
               05  SYS-1031-UPD-EDYUKYMD    PIC X(08).
