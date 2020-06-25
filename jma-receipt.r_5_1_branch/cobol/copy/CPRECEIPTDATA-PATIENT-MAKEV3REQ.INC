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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩレセ電データ情報（個別レセ電データ作成）(xml2)
      *                        COPY (CPRECEIPTDATA-PATIENT-MAKEV3REQ.INC)
      ******************************************************************
       01  XML-RECEIPTDATA-PATIENT-MAKEREQ.
           03  RECEDMAKEQ-CONTEXT          PIC S9(9)   BINARY.
           03  RECEDMAKEQ-OBJECT           PIC X(36).
           03  RECEDMAKEQ-MODE             PIC S9(9)   BINARY.
           03  RECEDMAKEQ-RECORDNAME       PIC X(128).
           03  RECEDMAKEQ-PATIENTREQ.
      *        リクエスト番号
               05  RECEDMAKEQ-REQUEST-NUMBER
                                           PIC X(2).
      *        カルテUID
               05  RECEDMAKEQ-KARTE-UID    PIC X(36).
      *        オルカUID
               05  RECEDMAKEQ-ORCA-UID     PIC X(36).
      *        実施日
               05  RECEDMAKEQ-PERFORM-DATE PIC X(10).
      *        診療年月
               05  RECEDMAKEQ-PERFORM-MONTH
                                           PIC X(7).
      *        請求年月日
               05  RECEDMAKEQ-AC-DATE      PIC X(10).
      *        作成モード(Check:点検用 Ckeck以外:提出用)
               05  RECEDMAKEQ-CREATE-MODE  PIC X(6).
      *        提出先
               05  RECEDMAKEQ-SUBMISSION-MODE
                                           PIC X(2).
      *        レセ電データチェック(Yes:チェックする Yes以外:チェックしない)
               05  RECEDMAKEQ-CHECK-MODE   PIC X(3).
      *        保険者番号(00000000:直接請求する保険者以外)
               05  RECEDMAKEQ-HKNJANUM     PIC X(8).
      *        期間指定
               05  RECEDMAKEQ-START-MONTH  PIC X(7).
               05  RECEDMAKEQ-END-MONTH    PIC X(7).
      *        個別対象患者一覧
               05  RECEDMAKEQ-PATIENT-INFORMATION OCCURS  100.
      *            個別対象−患者番号
                   07 RECEDMAKEQ-PATIENT-ID       PIC X(20).
      *            個別対象−診療年月
                   07 RECEDMAKEQ-PATIENT-MONTH    PIC X(7).
      *            個別対象−入外区分
                   07 RECEDMAKEQ-PATIENT-INOUT    PIC X(2).
      *            個別対象−レセプト情報
                   07 RECEDMAKEQ-DETAIL-INFORMATION-G.
                     08 RECEDMAKEQ-DETAIL-INFORMATION   OCCURS  5.
      *                種別コード
                       09  RECEDMAKEQ-DETAIL-CODE
                                           PIC X(04).
      *                種別
                       09  RECEDMAKEQ-DETAIL-TYPE
                                           PIC X(10).
                       09  RECEDMAKEQ-DETAIL-INOUT
                                           PIC X(1).
      *                保険情報
                       09  RECEDMAKEQ-MAIN-INSURANCE.
      *                    保険ＩＤ
                           11  RECEDMAKEQ-HKNID
                                           PIC X(10).
      *                    保険番号
                           11  RECEDMAKEQ-INSURANCE-CLASS
                                           PIC X(03).
                           11  RECEDMAKEQ-MAIN-INSURANCE-1.
      *                        保険者番号
                               13  RECEDMAKEQ-INSURANCE-NUMBER
                                           PIC X(08).
      *                        記号
                               13  RECEDMAKEQ-MAIN-MARK
                                           PIC X(38).
      *                        番号
                               13  RECEDMAKEQ-MAIN-NUMBER
                                           PIC X(38).
      *                        補助区分
                               13  RECEDMAKEQ-MAIN-ASSISTANCE
                                           PIC X(01).
      *                公費情報
                       09  RECEDMAKEQ-PUBLIC-INSURANCE.
                           11  RECEDMAKEQ-PUBLIC-INSURANCE-O  OCCURS  4.
      *                        公費ＩＤ
                               13  RECEDMAKEQ-KOHID
                                               PIC X(10).
      *                        公費番号
                               13  RECEDMAKEQ-KOHNUM
                                           PIC X(03).
      *                        負担者番号
                               13  RECEDMAKEQ-PROVIDER-NUMBER
                                               PIC X(08).
      *                        受給者番号
                               13  RECEDMAKEQ-RECIPIENT-NUMBER
                                               PIC X(20).
      *                災害該当の有無
                       09  RECEDMAKEQ-ACCIDENT-TARGET
                                           PIC X(4).
      *                入所中の有無
                       09  RECEDMAKEQ-INCARE-CLASS
                                           PIC X(4).
      *                労災情報
                       09  RECEDMAKEQ-ACCIDENT-INSURANCE.
      *****************    傷病の部位
      *****************    11  RECEDMAKEQ-DISEASE-LOCATION
      *****************                    PIC X(100).
      *                    労災自賠保険区分
                           11  RECEDMAKEQ-ACCIDENT
                                           PIC X(01).
      *                    労働保険番号
                           11  RECEDMAKEQ-ACCIDENT-NUMBER
                                           PIC X(14).
      *                     年金証書番号
                           11  RECEDMAKEQ-PENSIONCERTIFICATE
                                           PIC X(09).
      *                    災害区分
                           11  RECEDMAKEQ-ACCIDENT-CLASS
                                           PIC X(1).
      *                実施確認指示（警告）
                       09  RECEDMAKEQ-RECEIPT-PERFORM-FLAG
                                           PIC X(4).
      *
