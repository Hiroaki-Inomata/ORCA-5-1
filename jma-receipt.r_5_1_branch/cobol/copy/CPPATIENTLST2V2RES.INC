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
      *    ＡＰＩレスポンス患者情報一覧取得(xml2)
      *                                   COPY (CPPATIENTLST2V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST2RES.
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
              03  PATLSTRES-PATIENTID-CNT      PIC X(03).
      *       エラー患者件数
              03  PATLSTRES-PATIENTID-ERR      PIC X(03).
      *       患者情報
              03  PATLSTRES-PATIENT-INFOMATION    OCCURS   100.
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
      *           患者自宅情報
                  04  PATLSTRES-ADDRESS-INFO.
      *               郵便番号
                      06  PATLSTRES-HOME-ZIP-CODE      PIC X(7).
      *               住所
                      06  PATLSTRES-HOME-ADDRESSES1    PIC X(200).
      *               番地番号
                      06  PATLSTRES-HOME-ADDRESSES2    PIC X(200).
      *               自宅電話番号（昼）
                      06  PATLSTRES-HOME-HPONE1        PIC X(15).
      *               自宅電話番号（昼）
                      06  PATLSTRES-HOME-HPONE2        PIC X(15).
      *           入院区分（１：入院中）
                  04  PATLSTRES-OUTPATIENT-CLASS       PIC X(01).
      *           保険情報
                  04  PATLSTRES-INSURANCE-INFO     OCCURS  3 TIMES.
      *               保険の種類
                      06  PATLSTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               保険の種類名称
                      06  PATLSTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               保険者番号
                      06  PATLSTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               記号
                      06  PATLSTRES-MAIN-MARK              PIC X(80).
      *               番号
                      06  PATLSTRES-MAIN-NUMBER            PIC X(80).
      *               継続区分
                      06  PATLSTRES-MAIN-CONTINUATION      PIC X(01).
      *               補助区分
                      06  PATLSTRES-MAIN-ASSISTANCE        PIC X(01).
      *               本人家族区分
                      06  PATLSTRES-MAIN-FAMILY            PIC X(01).
      *               被保険者名
                      06  PATLSTRES-MAIN-POLICYHOLDER      PIC X(100).
      *               有効開始日
                      06  PATLSTRES-MAIN-START-DATE        PIC X(10).
      *               有効終了日
                      06  PATLSTRES-MAIN-END-DATE          PIC X(10).
      *            公費情報
                   04  PATLSTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               公費の種類
                      06  PATLSTRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      06  PATLSTRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      06  PATLSTRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      06  PATLSTRES-RECIPIENT-NUMBER        PIC X(20).
      *               有効開始日
                      06  PATLSTRES-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      06  PATLSTRES-INSURANCE-END-DATE      PIC X(10).
      *
