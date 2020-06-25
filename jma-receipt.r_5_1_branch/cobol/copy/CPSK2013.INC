      ******************************************************************
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
      ******************************************************************
      *         システム管理マスタ     CPSK2013.INC
      *         （２０１３） 
      *         レセプト公費記載順設定
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
       01  SYS-2013-REC.
           03  SYS-2013-KEY.
               05  SYS-2013-HOSPNUM    PIC 9(02).
      *管理コード
               05  SYS-2013-KANRICD    PIC X(04).
      *区分コード
               05  SYS-2013-KBNCD      PIC X(08).
      *有効開始年月日
               05  SYS-2013-STYUKYMD   PIC X(08).
      *有効終了年月日
               05  SYS-2013-EDYUKYMD   PIC X(08).
      *管理テーブル
           03  SYS-2013-TBL.
      *公費番号
               05  SYS-2013-KOHNUM-T   OCCURS  50.
                   07  SYS-2013-KOHNUM PIC X(03).
      *予備
           03  SYS-2013-YOBI           PIC X(350).
      *端末ＩＤ
           03  SYS-2013-TERMID         PIC X(16).
      *オペーレータＩＤ
           03  SYS-2013-OPID           PIC X(16).
      *作成年月日
           03  SYS-2013-CREYMD         PIC X(08).
      *更新年月日
           03  SYS-2013-UPYMD          PIC X(08).
      *更新時間
           03  SYS-2013-UPHMS          PIC X(06).
      *主キー更新用項目
           03  SYS-2013-UPD-KEY.
               05  SYS-2013-UPD-HOSPNUM
                                       PIC 9(02).
               05  SYS-2013-UPD-KANRICD
                                       PIC X(04).
               05  SYS-2013-UPD-KBNCD  PIC X(08).
               05  SYS-2013-UPD-STYUKYMD
                                       PIC X(08).
               05  SYS-2013-UPD-EDYUKYMD
                                       PIC X(08).
