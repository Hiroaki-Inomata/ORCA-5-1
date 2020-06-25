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
      *         レセプト編綴順詳細設定 (CPRECE-PUTH.INC)         *
      *         ＨＥＡＤ                                         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  RECE-PUTH-REC.
      *医療機関ＩＤ
            02  RECE-PUTH-HOSPNUM      PIC 9(2).
      *印刷順指定（県内県外）
            02  RECE-PUTH-PREFKBN-PRTJYUN
                                       PIC X(1).
      *印刷順指定（保険者）
            02  RECE-PUTH-HKNJANUM-PRTJYUN
                                       PIC X(1).
      *医療機関の所在地保険者
            02  RECE-PUTH-HKNJANUM     PIC X(8).
      *医療機関の所在地保険者印刷位置
            02  RECE-PUTH-HKNJANUM-PRTKBN
                                       PIC X(1).
      *印刷順設定（１：県内、２：県外）
            02  RECE-PUTH-PRTJYUN-TBL  OCCURS  2.
      *第一印刷順設定
                03  RECE-PUTH-SRT-01   PIC X(2).
      *一般国保設定
                03  RECE-PUTH-KKHKBN   PIC X(2).
      *長期（長期高額療養費）
                03  RECE-PUTH-CHOUKI   PIC X(1).
      *在総診・在医総
                03  RECE-PUTH-ZAITAKU  PIC X(1).
      *高額点数
                03  RECE-PUTH-KOUGAKU-TEN
                                       PIC 9(7).
      *最終出力順
                03  RECE-PUTH-SRT-LAST PIC X(1).
      *有効開始年月日
            02  RECE-PUTH-STYUKYMD     PIC X(8).
      *有効終了年月日
            02  RECE-PUTH-EDYUKYMD     PIC X(8).
      *端末ＩＤ
            02  RECE-PUTH-TERMID       PIC X(64).
      *オペレータＩＤ
            02  RECE-PUTH-OPID         PIC X(16).
      *作成年月日
            02  RECE-PUTH-CREYMD       PIC X(8).
      *更新年月日
            02  RECE-PUTH-UPYMD        PIC X(8).
      *更新時間
            02  RECE-PUTH-UPHMS        PIC X(6).
      *
      *主キー更新用項目
            02  RECE-PUTH-UPSTYUKYMD   PIC X(08).
            02  RECE-PUTH-UPEDYUKYMD   PIC X(08).
