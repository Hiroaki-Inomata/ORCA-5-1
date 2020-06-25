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
      *         システム管理マスタ     (CPSK1044.INC)  
      *         （１０４４）                            
      *         帳票オフセット情報                  
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1044-REC.
           03  SYS-1044-KEY.
               05  SYS-1044-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1044-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1044-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1044-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1044-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1044-TBL.
      *帳票ＩＤ（ｍｏｎｐｅ）
               05  SYS-1044-FORMID      PIC  X(20).
      *出力プリンタ名
               05  SYS-1044-PRTNM       PIC  X(16).
      *オフセット（ｘ方向）
               05  SYS-1044-OFFSET-X    PIC  X(05).
      *オフセット（ｙ方向）
               05  SYS-1044-OFFSET-Y    PIC  X(05).
      *予備
           03  SYS-1044-YOBI            PIC  X(454).
      *端末ＩＤ
           03  SYS-1044-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1044-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1044-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1044-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1044-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1044-UPD-KEY.
               05  SYS-1044-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1044-UPD-KANRICD     PIC X(04).
               05  SYS-1044-UPD-KBNCD       PIC X(08).
               05  SYS-1044-UPD-STYUKYMD    PIC X(08).
               05  SYS-1044-UPD-EDYUKYMD    PIC X(08).
