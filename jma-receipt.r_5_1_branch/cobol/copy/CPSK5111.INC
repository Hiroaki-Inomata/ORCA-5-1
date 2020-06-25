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
      *         システム管理マスタ     (CPSK5111.INC)   *
      *         （５１１１）                            *
      *         特定入院料（病室）                      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-5111-REC.
           03  SYS-5111-KEY.
               05  SYS-5111-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-5111-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-5111-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-5111-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-5111-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-5111-TBL.
      *診療コード
               05  SYS-5111-SRYCD       PIC  X(09).
      *特定入院料名称
               05  SYS-5111-BRM-TOKUTEINYUIN-NM
                                        PIC  X(60).
      *特定入院料名称２
               05  SYS-5111-BRM-TOKUTEINYUIN-NM2
                                        PIC  X(24).
      *入院料区分
               05  SYS-5111-BRM-NYUINRYOKBN
                                        PIC  X(02).
      *生活療養区分
               05  SYS-5111-SEIKATU-RYOKBN
                                        PIC  X(01).
      *予備
               05  SYS-5111-YOBI        PIC  X(404).
      *端末ＩＤ
           03  SYS-5111-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-5111-OPID            PIC  X(16).
      *作成年月日
           03  SYS-5111-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-5111-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-5111-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-5111-UPD-KEY.
               05  SYS-5111-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5111-UPD-KANRICD     PIC X(04).
               05  SYS-5111-UPD-KBNCD       PIC X(08).
               05  SYS-5111-UPD-STYUKYMD    PIC X(08).
               05  SYS-5111-UPD-EDYUKYMD    PIC X(08).
