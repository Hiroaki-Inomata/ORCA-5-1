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
      *         システム管理マスタ     (CPSK2009.INC)            *
      *         （２００９）                                     *
      *         主科設定情報                                     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *  03.02.01    NACL-藤原    06/08/01  国保主科対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正
      *                                     広域連合主科対応
      ************************************************************
       01  SYS-2009-REC.
           03  SYS-2009-KEY.
               05  SYS-2009-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2009-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2009-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2009-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2009-EDYUKYMD    PIC  X(08).
      *主科設定フラグ
           03  SYS-2009-SYUKA-KBN       PIC  X(01).
      *主科設定情報（入院）
           03  SYS-2009-SYUKA-NYUIN     PIC  X(01).
      *主科設定情報（入院外）
           03  SYS-2009-SYUKA-GAIRAI    PIC  X(01).
      *主科情報取得フラグ
           03  SYS-2009-SYUKA-MODE      PIC  X(01).
      *複数科同一選択
           03  SYS-2009-SYUKA-SELECT    PIC  X(01).
      *主科社保設定フラグ
           03  SYS-2009-SYUKA-SYAHO     PIC  X(01).
      *主科国保設定フラグ
           03  SYS-2009-SYUKA-KOKUHO    PIC  X(01).
      *主科広域連合設定フラグ
           03  SYS-2009-SYUKA-KOUIKI    PIC  X(01).
      *予備
           03  SYS-2009-YOBI            PIC  X(492).
      *端末ＩＤ
           03  SYS-2009-TERMID          PIC  X(16).
      *オペレータＩＤ
           03  SYS-2009-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2009-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2009-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2009-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2009-UPD-KEY.
               05  SYS-2009-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2009-UPD-KANRICD     PIC X(04).
               05  SYS-2009-UPD-KBNCD       PIC X(08).
               05  SYS-2009-UPD-STYUKYMD    PIC X(08).
               05  SYS-2009-UPD-EDYUKYMD    PIC X(08).
