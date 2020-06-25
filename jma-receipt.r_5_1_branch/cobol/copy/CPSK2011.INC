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
      *         システム管理マスタ     (CPSK2011.INC)            *
      *         （２０１１）                                     *
      *         レセ電記号番号区切り文字設定情報                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-2011-REC.
           03  SYS-2011-KEY.
               05  SYS-2011-HOSPNUM    PIC 9(2).
      *管理コード
               05  SYS-2011-KANRICD    PIC X(04).
      *区分コード
               05  SYS-2011-KBNCD      PIC X(08).
      *有効開始年月日
               05  SYS-2011-STYUKYMD   PIC X(08).
      *有効終了年月日
               05  SYS-2011-EDYUKYMD   PIC X(08).
      *区切り文字設定フラグ（記号）
           03  SYS-2011-KIGO-DELIMITER-FLG
                                       PIC X(01).
      *区切り文字設定対象保険者フラグ（記号）
           03  SYS-2011-KIGO-DELIMITER-HKNJANUM-FLG
                                       PIC X(01).
      *区切り文字設定フラグ（番号）
           03  SYS-2011-NUM-DELIMITER-FLG
                                       PIC X(01).
      *区切り文字設定対象保険者フラグ（番号）
           03  SYS-2011-NUM-DELIMITER-HKNJANUM-FLG
                                       PIC X(01).
      *予備
           03  SYS-2011-YOBI           PIC X(496).
      *端末ＩＤ
           03  SYS-2011-TERMID         PIC X(16).
      *オペレータＩＤ
           03  SYS-2011-OPID           PIC X(16).
      *作成年月日
           03  SYS-2011-CREYMD         PIC X(08).
      *更新年月日
           03  SYS-2011-UPYMD          PIC X(08).
      *更新時間
           03  SYS-2011-UPHMS          PIC X(06).
      *主キー更新用項目
           03  SYS-2011-UPD-KEY.
               05  SYS-2011-UPD-HOSPNUM
                                       PIC 9(2).
               05  SYS-2011-UPD-KANRICD
                                       PIC X(04).
               05  SYS-2011-UPD-KBNCD  PIC X(08).
               05  SYS-2011-UPD-STYUKYMD
                                       PIC X(08).
               05  SYS-2011-UPD-EDYUKYMD
                                       PIC X(08).
