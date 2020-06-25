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
      *    定点設定                    COPY  (CPDASCONF.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  DASCF-REC.
           03  DASCF-KEY.
      *医療機関識別番号
               05  DASCF-HOSPNUM             PIC  9(02).
      *申請日
           03  DASCF-SINSEIYMD               PIC  X(08).
      *氏名（カタカナ）
           03  DASCF-KANANAME                PIC  X(50).
      *氏名
           03  DASCF-NAME                    PIC  X(50).
      *メールアドレス
           03  DASCF-EMAIL                   PIC  X(256).
      *医療機関種別１
           03  DASCF-HOSPSBT1                PIC  X(01).
      *医療機関種別２
           03  DASCF-HOSPSBT2                PIC  X(01).
      *病床数（一般）
           03  DASCF-BEDSUIPPAN              PIC  9(04).
      *病床数（療養）
           03  DASCF-BEDSURYOYO              PIC  9(04).
      *病床数（精神）
           03  DASCF-BEDSUSEISIN             PIC  9(04).
      *病床数（結核）
           03  DASCF-BEDSUKEKKAKU            PIC  9(04).
      *病床数（全体）
           03  DASCF-BEDSU                   PIC  9(04).
      *設置主体
           03  DASCF-SETTI                   PIC  X(01).
      *主たる診療科
           03  DASCF-SYUKA                   PIC  X(02).
      *地域医療支援病院
           03  DASCF-CHIIKI                  PIC  X(01).
      *ＤＰＣ対象病院
           03  DASCF-DPCKBN                  PIC  X(01).
      *在宅支援診療所
           03  DASCF-ZAITAKU                 PIC  X(01).
      *開院時期
           03  DASCF-KAIINYM                 PIC  X(06).
      *郵便番号
           03  DASCF-POST                    PIC  X(07).
      *定点参加番号
           03  DASCF-USERID                  PIC  X(15).
      *パスワード
           03  DASCF-PASSWORD                PIC  X(128).
      *同意
           03  DASCF-AGREEMENT               PIC  X(01).
      *送信結果
           03  DASCF-SENDRESULT              PIC  X(03).
      *送信日
           03  DASCF-SENDDATE                PIC  X(08).
      *端末ＩＤ
           03  DASCF-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  DASCF-OPID                    PIC  X(16).
      *作成年月日
           03  DASCF-CREYMD                  PIC  X(08).
      *更新年月日
           03  DASCF-UPYMD                   PIC  X(08).
      *更新時間
           03  DASCF-UPHMS                   PIC  X(06).
