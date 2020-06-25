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
      *         システム管理マスタ     (CPSK2008.INC)            *
      *         （２００８）                                     *
      *         レセプト編綴順情報                               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-2008-REC.
           03  SYS-2008-KEY.
               05  SYS-2008-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2008-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2008-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2008-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2008-EDYUKYMD    PIC  X(08).
      *印刷順指定 
          03  SYS-2008-PRTJYUN.
      *        県内県外
               05  SYS-2008-PREFKBN-PRTJYUN
                                        PIC  X(01).
      *        保険者
               05  SYS-2008-HKNJANUM-PRTJYUN
                                        PIC  X(01).
      *        レセプト種別
               05  SYS-2008-RECESYUBETU-PRTJYUN
                                        PIC  X(01).
      *在総診・在医総
           03  SYS-2008-ZAITAKU         PIC  X(01).
      *長期（長期高額療養費）
           03  SYS-2008-CHOUKI          PIC  X(01).
      *医療機関の所在地保険者
           03  SYS-2008-HKNJANUM        PIC  X(08).
      *医療機関の所在地保険者印刷位置
           03  SYS-2008-HKNJANUM-PRTKBN PIC  X(01).
      *予備
           03  SYS-2008-YOBI            PIC  X(486).
      *端末ＩＤ
           03  SYS-2008-TERMID          PIC  X(16).
      *オペレータＩＤ
           03  SYS-2008-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2008-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2008-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2008-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2008-UPD-KEY.
               05  SYS-2008-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2008-UPD-KANRICD     PIC X(04).
               05  SYS-2008-UPD-KBNCD       PIC X(08).
               05  SYS-2008-UPD-STYUKYMD    PIC X(08).
               05  SYS-2008-UPD-EDYUKYMD    PIC X(08).
