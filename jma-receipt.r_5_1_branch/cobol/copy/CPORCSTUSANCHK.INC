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
      *    通算日数チェックサブ      COPY (CPORCSTUSANCHK.INC)   *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSTUSANCHKAREA.
      *  医療機関コード
           03  STUSANCHK-HOSPNUM           PIC 9(2).
      *  患者ＩＤ
           03  STUSANCHK-PTID              PIC  9(10).
      *    入院日
           03  STUSANCHK-NYUINYMD          PIC X(08).
      *    基準日
           03  STUSANCHK-KJNYMD            PIC X(08).
      *    履歴番号
           03  STUSANCHK-RRKNUM            PIC 9(03).
      *  出力領域
           03  STUSANCHK-OUT-AREA.
      *        自院入院通算日数
               05  STUSANCHK-TSUSAN        PIC 9(08).
      *        ９１日以上到達日状態
      *          0...入院登録で作成した歴の中で到達
      *          1...自院歴の中で到達
      *          9...取得不可
               05  STUSANCHK-DAY-91-STS    PIC 9(1).
      *        ９１日以上到達日
               05  STUSANCHK-DAY-91        PIC X(08).
      *        １８０日以上到達日状態
      *          0...入院登録で作成した歴の中で到達
      *          1...他院歴の中で到達
      *          2...自院歴の中で到達
      *          9...取得不可
               05  STUSANCHK-DAY-180-STS   PIC 9(1).
      *        １８０日以上到達日
               05  STUSANCHK-DAY-180       PIC X(08).
      *  リターンコード
           03  STUSANCHK-RC                PIC 9(02).
