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
      ******************************************************************
      *    ＡＰＩレスポンス患者一覧取得(xml2)
      *                                   COPY (CPPATIENTLST1V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST1RES.
            02  PATLSTRES-CONTEXT          PIC S9(9)   BINARY.
            02  PATLSTRES-OBJECT           PIC X(36).
            02  PATLSTRES-MODE             PIC S9(9)   BINARY.
            02  PATLSTRES-RECORDNAME       PIC X(128).
            02  PATLSTRES-PATIENTINFORES.
      *       実施日
              03  PATLSTRES-INFORMATION-DATE   PIC X(10).
      *       実施時間
              03  PATLSTRES-INFORMATION-TIME   PIC X(8).
      *       返却コード
              03  PATLSTRES-API-RESULT         PIC X(2).
      *       返却メッセージ
              03  PATLSTRES-API-RESULT-MSG     PIC X(100).
              03  PATLSTRES-RESKEY             PIC X(100).
      *       対象患者件数
              03  PATLSTRES-PATIENTID-CNT      PIC X(04).
      *       患者情報
              03  PATLSTRES-PATIENT-INFOMATION    OCCURS   1000.
      *           患者番号
                  04  PATLSTRES-PATIENTID          PIC X(20).
      *           漢字氏名
                  04  PATLSTRES-NAME               PIC X(100).
      *           カナ氏名
                  04  PATLSTRES-KANANAME           PIC X(100).
      *           生年月日
                  04  PATLSTRES-BIRTHDAY           PIC X(10).
      *           性別
                  04  PATLSTRES-SEX                PIC X(1).
      *           登録日付
                  04  PATLSTRES-CREYMD             PIC X(10).
      *           更新日付
                  04  PATLSTRES-UPYMD              PIC X(10).
      *           更新時間
                  04  PATLSTRES-UPTIME             PIC X(08).
      *           テスト患者番号区分
                  04  PATLSTRES-TSTPTNUMKBN        PIC X(01).
      *
