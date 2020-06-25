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
      *    分娩管理                   COPY  (CPBUNBEN.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  BUNBEN-REC.
      *キー
           03  BUNBEN-KEY.
      *医療機関ＩＤ
               05  BUNBEN-HOSPNUM             PIC  9(02).
      *患者ＩＤ
               05  BUNBEN-PTID                PIC  9(10).
      *入外区分
               05  BUNBEN-NYUGAIKBN           PIC  X(01).
      *請求年月
               05  BUNBEN-SKYYM               PIC  X(06).
      *入院履歴番号
               05  BUNBEN-RRKNUM              PIC  9(07).
      *患者番号
           03  BUNBEN-PTNUM                   PIC  X(20).
      *請求区分
           03  BUNBEN-SKYKBN                  PIC  X(01).
      *直接支払制度利用区分
           03  BUNBEN-RIYOUKBN                PIC  X(01).
      *分娩区分
           03  BUNBEN-BUNBENKBN               PIC  9(01).
      *データ提出先区分
           03  BUNBEN-TEISYUTUSAKI            PIC  X(01).
      *加入制度区分
           03  BUNBEN-KANYUKBN                PIC  X(01).
      *本人家族区分
           03  BUNBEN-HONKZKKBN               PIC  X(01).
      *保険者番号
           03  BUNBEN-HKNJANUM                PIC  X(08).
      *被保険者証記号
           03  BUNBEN-KIGO                    PIC  X(40).
      *被保険者証番号
           03  BUNBEN-NUM                     PIC  X(40).
      *妊婦氏名（カナ氏名）
           03  BUNBEN-NAME                    PIC  X(40).
      *在胎週数
           03  BUNBEN-ZAIWEEK                 PIC  9(02).
      *出産年月日
           03  BUNBEN-BIRTHDAY                PIC  X(08).
      *死産有無区分
           03  BUNBEN-SIZANKBN                PIC  X(01).
      *出産数
           03  BUNBEN-BIRTHCNT                PIC  9(01).
      *入院日数
           03  BUNBEN-NYUIN-DAY               PIC  9(03).
      *産科医療補償制度区分
           03  BUNBEN-SANKAKBN                PIC  X(01).
      *明細情報
           03  BUNBEN-MEISAI-INF.
      *入院料
               05  BUNBEN-NYU-MONEY           PIC S9(08).
      *室料差額
               05  BUNBEN-RMSGK-MONEY         PIC S9(08).
      *分娩介助料
               05  BUNBEN-KAIJO-MONEY         PIC S9(08).
      *分娩料
               05  BUNBEN-BUNBEN-MONEY        PIC S9(08).
      *新生児管理保育料
               05  BUNBEN-SHIN-MONEY          PIC S9(08).
      *検査・薬剤料
               05  BUNBEN-KNS-MONEY           PIC S9(08).
      *処置・手当料
               05  BUNBEN-SYC-MONEY           PIC S9(08).
      *産科医療補償制度
               05  BUNBEN-SNK-MONEY           PIC S9(08).
      *その他
               05  BUNBEN-ETC-MONEY           PIC S9(08).
      *一部負担金等
               05  BUNBEN-ICBFTN-MONEY        PIC S9(08).
      *妊婦合計負担額
               05  BUNBEN-NINPUFTN-MONEY      PIC S9(08).
      *代理受取額
               05  BUNBEN-DAIRI-MONEY         PIC S9(08).
      *明細情報（再定義）
           03  BUNBEN-MEISAI-R   REDEFINES  BUNBEN-MEISAI-INF.
               05  BUNBEN-MONEY               PIC S9(08)  OCCURS 12.
      *備考
           03  BUNBEN-BIKOU                   PIC  X(60).
      *更新区分
           03  BUNBEN-UPDKBN                  PIC  X(01).
      *保険者番号
           03  BUNBEN-HKNJANUM-S              PIC  X(08).
      *提出日区分（１：１０日請求　２：２５日請求）
           03  BUNBEN-TEISYUTUBIKBN           PIC  X(01).
      *端末ＩＤ
           03  BUNBEN-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  BUNBEN-OPID                    PIC  X(16).
      *作成年月日
           03  BUNBEN-CREYMD                  PIC  X(08).
      *更新年月日
           03  BUNBEN-UPYMD                   PIC  X(08).
      *更新時間
           03  BUNBEN-UPHMS                   PIC  X(06).
