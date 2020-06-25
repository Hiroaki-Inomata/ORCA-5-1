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
      *         システム管理マスタ     (CPSK5004.INC)   *
      *         （５０１０）                            *
      *         入院請求管理マスタ                      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-5010-REC.
           03  SYS-5010-KEY.
               05  SYS-5010-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-5010-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-5010-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-5010-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-5010-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-5010-TBL.
               05  SYS-5010-SEIKYU-DAY-VAL.
      *１期請求日
                   07  SYS-5010-SEIKYU-DAY1 PIC  9(02).
      *２期請求日
                   07  SYS-5010-SEIKYU-DAY2 PIC  9(02).
      *３期請求日
                   07  SYS-5010-SEIKYU-DAY3 PIC  9(02).
               05  SYS-5010-SEIKYU-DAY-R    REDEFINES
                   SYS-5010-SEIKYU-DAY-VAL.
                   07  SYS-5010-SEIKYU-DAY-OCC   OCCURS  3.
                       09  SYS-5010-SEIKYU-DAY
                                            PIC  9(02).
      *        予備
               05  SYS-5010-YOBI1           PIC  X(10).
      *        入金区分
               05  SYS-5010-NYUKINKBN       PIC  X(01).
      *        再処理区分
               05  SYS-5010-SAISYORIKBN     PIC  X(01).
      *        入金印字区分
               05  SYS-5010-NYUKINPRTKBN    PIC  X(01).
      *        削除区分
               05  SYS-5010-DELKBN          PIC  X(01).
      *        請求書発行方法区分
               05  SYS-5010-SKYSUMKBN       PIC  X(01).
      *        自動遷移区分
               05  SYS-5010-CHGWINDOWKBN    PIC  X(01).
      *        請求書印刷区分
               05  SYS-5010-SKYPRTKBN       PIC  X(01).
      *        明細書印刷区分
               05  SYS-5010-SRYPRTKBN       PIC  X(01).
      *        患者個別情報参照区分
               05  SYS-5010-PTINFREFKBN     PIC  X(01).
      *        出力順区分
               05  SYS-5010-PRTJYUN         PIC  X(02).
      *        退院患者集計区分
               05  SYS-5010-TAIINGRPKBN     PIC  X(01).
      *        予備
               05  SYS-5010-YOBI2           PIC  X(472).
      *端末ＩＤ
           03  SYS-5010-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-5010-OPID            PIC  X(16).
      *作成年月日
           03  SYS-5010-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-5010-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-5010-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-5010-UPD-KEY.
               05  SYS-5010-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5010-UPD-KANRICD     PIC X(04).
               05  SYS-5010-UPD-KBNCD       PIC X(08).
               05  SYS-5010-UPD-STYUKYMD    PIC X(08).
               05  SYS-5010-UPD-EDYUKYMD    PIC X(08).
