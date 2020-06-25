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
      *         チェックマスタ４     (CPCK1201.INC)              *
      *         エラー情報詳細                                   *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CK4-1201-REC.
           03  CK4-1201-KEY.
               05  CK4-1201-HOSPNUM     PIC 9(2).
      *管理コード
               05  CK4-1201-KANRICD     PIC  X(04).
      *区分コード(管理コード1200の区分コードに対応）
               05  CK4-1201-KBNCD       PIC  X(08).
      *有効開始年月日
               05  CK4-1201-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  CK4-1201-EDYUKYMD    PIC  X(08).
      *患者ＩＤ
           03  CK4-1201-PTID            PIC  9(10).
      *管理テーブル
           03  CK4-1201-TBL.
               05  CK4-1201-DAY-G.
                   07  CK4-1201-DAY     PIC  9(01)
                                        OCCURS   31.
               05  CK4-1201-LASTYMD     PIC  X(08).
               05  CK4-1201-SRYCD       PIC  X(09).
               05  CK4-1201-SRYKA       PIC  X(02).
               05  CK4-1201-CHKKBN      PIC  X(01).
               05  CK4-1201-BYOMEI      PIC  X(80).
               05  CK4-1201-COMPKBN     PIC  X(1).
               05  CK4-1201-TEKCD       PIC  X(09).
               05  CK4-1201-TEKBYOMEI   PIC  X(80).
               05  CK4-1201-TEKCOMPKBN  PIC  X(1).
               05  CK4-1201-TANICD      PIC  9(3).
               05  CK4-1201-TANINAME    PIC  X(12).
               05  CK4-1201-SHONUM      PIC  9(3).
               05  CK4-1201-SRYSTYMD    PIC  X(08).
               05  CK4-1201-SRYEDYMD    PIC  X(08).
               05  CK4-1201-LIMIT       PIC  9(5).
               05  CK4-1201-TSUKISU     PIC  9(2).
               05  CK4-1201-FILLER      PIC  X(237).
      *端末ＩＤ
           03  CK4-1201-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  CK4-1201-OPID            PIC  X(16).
      *作成年月日
           03  CK4-1201-CREYMD          PIC  X(08).
      *更新年月日
           03  CK4-1201-UPYMD           PIC  X(08).
      *更新時間
           03  CK4-1201-UPHMS           PIC  X(06).
