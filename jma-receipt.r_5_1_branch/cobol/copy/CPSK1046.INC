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
      *         システム管理マスタ     (CPSK1046.INC)   *
      *         （１０４６）                            *
      *         出産費用内訳情報                        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1046-REC.
           03  SYS-1046-KEY.
               05  SYS-1046-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1046-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1046-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1046-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1046-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1046-TBL.
      *その他自費緋付区分（入院）
               05  SYS-1046-ETCJIHIKBN  PIC  X(02)
                                        OCCURS 10.
      *保険適用外明緋付区分
               05  SYS-1046-MEISAIKBN   PIC  X(02)
                                        OCCURS 30.
      *その他自費緋付区分（外来）
               05  SYS-1046-GETCJIHIKBN PIC  X(02)
                                        OCCURS 10.
      *予備
               05  SYS-1046-YOBI        PIC  X(400).
      *端末ＩＤ
           03  SYS-1046-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1046-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1046-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1046-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1046-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1046-UPD-KEY.
               05  SYS-1046-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1046-UPD-KANRICD     PIC X(04).
               05  SYS-1046-UPD-KBNCD       PIC X(08).
               05  SYS-1046-UPD-STYUKYMD    PIC X(08).
               05  SYS-1046-UPD-EDYUKYMD    PIC X(08).
