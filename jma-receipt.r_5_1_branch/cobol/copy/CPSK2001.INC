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
      *         システム管理マスタ     (CPSK2001.INC)            *
      *         （２００１）                                     *
      *         レセプト作成指示コード                           *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01   MCC-藤原      02.06.13  レセプト種類区分の追加
      *  01.00.02   NACL-藤原     02.12.11  入外区分の追加
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *                                     処理区分の追加
      *
      *  04.03.01    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *                                     レセプト作成区分追加 
      ************************************************************
       01  SYS-2001-REC.
           03  SYS-2001-KEY.
               05  SYS-2001-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2001-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2001-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2001-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2001-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-2001-TBL.
      *処理区分
               05  SYS-2001-SYOKBN      PIC  X(01).
      *診療年月
               05  SYS-2001-SRYYM       PIC  X(06).
      *レセプト種類区分
               05  SYS-2001-RECEKBN     PIC  X(01).
      *入外区分
               05  SYS-2001-NYUGAIKBN   PIC  X(01).
      *処理区分
               05  SYS-2001-SYOSBT      PIC  X(01).
      *レセプト作成区分（１：点検用レセ、８：ＥＦファイル、９：連係基盤用）
               05  SYS-2001-PRTKBN      PIC  X(01).
      *予備
           03  SYS-2001-YOBI            PIC  X(489).
      *端末ＩＤ
           03  SYS-2001-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-2001-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2001-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2001-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2001-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2001-UPD-KEY.
               05  SYS-2001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2001-UPD-KANRICD     PIC X(04).
               05  SYS-2001-UPD-KBNCD       PIC X(08).
               05  SYS-2001-UPD-STYUKYMD    PIC X(08).
               05  SYS-2001-UPD-EDYUKYMD    PIC X(08).
