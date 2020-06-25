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
      *****************************************************************
      *         システム管理マスタ     (CPSK2003.INC)   
      *         （２００３）                           
      *         レセプト作成指示（印刷順指定１）        
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01   MCC-藤原      02.02.26  管理ファイルの変更により修正
      *  01.00.02   MCC-藤原      02/06/17  労災・自賠責に項目を変更
      *
      *  01.01.01   NACL-藤原     02/09/24  入外区分・表示順の追加
      *****************************************************************
       01  SYS-2003-REC.
           03  SYS-2003-KEY.
               05  SYS-2003-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2003-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2003-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2003-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2003-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-2003-TBL.
      *レセプト種類名称
               05  SYS-2003-SYURUINAME  PIC  X(40).
      *審査支払機関
               05  SYS-2003-TEISYUTUSAKI
                                        PIC  X(01).
      *レセプト種別
               05  SYS-2003-SYUBETU-G.
                   07  SYS-2003-SYUBETU PIC  X(04)  OCCURS 10.
      *入外区分
               05  SYS-2003-NYUGAIKBN   PIC  X(01).
      *表示順
               05  SYS-2003-DSPSEQ      PIC  9(03).
      *予備
           03  SYS-2003-YOBI            PIC  X(415).
      *端末ＩＤ
           03  SYS-2003-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-2003-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2003-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2003-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2003-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2003-UPD-KEY.
               05  SYS-2003-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2003-UPD-KANRICD     PIC X(04).
               05  SYS-2003-UPD-KBNCD       PIC X(08).
               05  SYS-2003-UPD-STYUKYMD    PIC X(08).
               05  SYS-2003-UPD-EDYUKYMD    PIC X(08).
