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
      *         システム管理マスタ     (CPSK1900.INC)            *
      *         （１９０１）                                     *
      *         医療機関編集情報−帳票別指定情報                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1900-REC.
           03  SYS-1900-KEY.
               05  SYS-1900-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1900-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1900-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1900-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1900-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1900-TBL.
      *帳票別指定   1:医保・レセプト（入院外）
      *             2:労災・レセプト（入院外）
      *             3:自賠責・レセプト（入院外）
      *             4:医保・レセプト（入院）
      *             5:労災・レセプト（入院）
      *             6:自賠責・レセプト（入院）
      *             7:診療報酬請求書（社保）
      *             8:診療報酬請求書（国保）
      *             9:処方せん
      *            10:請求書兼領収書
      *            11:入院請求書兼領収書
      *            12:診療費明細書
      *            13:薬剤情報提供書
      *            14:公害・レセプト（入院外）
      *            15:公害・レセプト（入院）
      *            16:診療報酬請求書（公害）
      *            17:診療報酬請求書（広域連合）
      *            18:お薬手帳
      *            19:レセプト電算
      *            20:症状詳記
      *            21:光ディスク等送付書
      *            22:予約票
      *            23:診療報酬請求書（労災）
      *            24:支払証明書
      *            25:自費内訳明細書
               05  SYS-1900-PRTKBN-TBL  OCCURS  30.
      *
                   07  SYS-1900-PRTKBN  PIC  X(02).
      *予備
           03  SYS-1900-YOBI            PIC  X(440).
      *端末ＩＤ
           03  SYS-1900-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1900-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1900-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1900-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1900-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1900-UPD-KEY.
               05  SYS-1900-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1900-UPD-KANRICD     PIC X(04).
               05  SYS-1900-UPD-KBNCD       PIC X(08).
               05  SYS-1900-UPD-STYUKYMD    PIC X(08).
               05  SYS-1900-UPD-EDYUKYMD    PIC X(08).
