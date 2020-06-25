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
      *    患者保険情報　    　 COPY  (CPPTHKNINF.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     多々納       02/02/07  資格取得年月日追加
      * 05.00.00     NACL-多々納  20/04/07  枝番追加
      ************************************************************
      *01  PTHKNINF-REC.
      *キー
           03  PTHKN-KEY.
      *医療機関ＩＤ
               05  PTHKN-HOSPNUM            PIC 9(2).
      *患者ＩＤ
               05  PTHKN-PTID              PIC  9(10).
      *保険ＩＤ
               05  PTHKN-HKNID             PIC  9(10).
      *保険番号
           03  PTHKN-HKNNUM                PIC  X(03).
      *
           03  PTHKN-HKNJA-G.
      *保険者番号
               05  PTHKN-HKNJANUM          PIC  X(08).
      *被保険者名
           03  PTHKN-HIHKNJANAME           PIC  X(100).
      *本人家族区分
           03  PTHKN-HONKZKKBN             PIC  X(01).
      *補助区分
           03  PTHKN-HOJOKBN               PIC  X(01).
      *継続区分
           03  PTHKN-CONTKBN               PIC  X(01).
      *記号
           03  PTHKN-KIGO                  PIC  X(80).
      *番号
           03  PTHKN-NUM                   PIC  X(80).
      *番号枝番
           03  PTHKN-EDABAN                PIC  X(02).
      *資格取得年月日
           03  PTHKN-SKKGETYMD             PIC  X(08).
      *適用開始年月日
           03  PTHKN-TEKSTYMD              PIC  X(08).
      *適用終了年月日
           03  PTHKN-TEKEDYMD              PIC  X(08).
      *確認年月日
           03  PTHKN-KAKUNINYMD            PIC  X(08).
      *削除区分
           03  PTHKN-SAKJOKBN              PIC  X(01).
      *端末ＩＤ
           03  PTHKN-TERMID                PIC  X(16).
      *オペーレータＩＤ
           03  PTHKN-OPID                  PIC  X(16).
      *作成年月日
           03  PTHKN-CREYMD                PIC  X(08).
      *更新年月日
           03  PTHKN-UPYMD                 PIC  X(08).
      *更新時間
           03  PTHKN-UPHMS                 PIC  X(06).
