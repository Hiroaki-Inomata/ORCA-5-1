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
      *    ＡＰＩレスポンス患者病名情報(xml2)
      *                                     COPY (CPDISEASEINFV2REQ.INC)
      ******************************************************************
        01  XML-DISEASE-INFRES.
            02  DISERES-CONTEXT            PIC S9(9)   BINARY.
            02  DISERES-OBJECT             PIC X(36).
            02  DISERES-MODE               PIC S9(9)   BINARY.
            02  DISERES-RECORDNAME         PIC X(128).
            02  DISERES-DISEASE-INFRES.
      *       実施日
              03  DISERES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  DISERES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  DISERES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  DISERES-API-RESULT-MSG      PIC X(100).
      *
              03  DISERES-RESKEY              PIC X(100).
      *       病名情報オーバーフラグ
              03  DISERES-INFORMATION-OVERFLOW
                                              PIC X(5).
      *       患者情報
              03  DISERES-PATIENT-INFOMATION.
      *           患者番号
                  05  DISERES-PATIENTID         PIC X(20).
      *           患者氏名
                  05  DISERES-NAME              PIC X(100).
      *           患者カナ氏名
                  05  DISERES-KANANAME          PIC X(100).
      *           誕生日
                  05  DISERES-BIRTHDAY          PIC X(10).
      *           性別
                  05  DISERES-SEX               PIC X(1).
      *       基準年月
              03  DISERES-BASE-DATE        PIC X(07).
      *       病名情報
              03  DISERES-DISEASE-INF-G    OCCURS   200.
      *            入外区分
                   05  DISERES-NYUGAIKBN       PIC X(1).
      *            診療科
                   05  DISERES-SRYKA           PIC X(02).
      *            保険組合せ番号
                   05  DISERES-HKNCOMBI        PIC X(04).
      *            第三者行為フラグ（第三者行為の保険組合せのとき）
                   05  DISERES-THIRD-PARTY-MARK
                                               PIC X(04).
      *            病名
                   05  DISERES-DISEASE-NAME    PIC X(160).
      *            病名コード
                   05  DISERES-DISEASE-SINGLE-G.
                       07  DISERES-DISEASE-SINGLE  OCCURS   21.
      *                    病名コード
                           09  DISERES-SINGLE-CODE     PIC X(07).
      *                    病名
                           09  DISERES-SINGLE-NAME     PIC X(160).
      *                    状態コード
                           09  DISERES-SINGLE-FLG      PIC X(01).
      *            主病名フラグ
                   05  DISERES-CATEGORY            PIC X(02).
      *            疑いフラグ
                   05  DISERES-SUSPECTEDFLAG       PIC X(02).
      *            診療開始日
                   05  DISERES-START-DATE          PIC X(10).
      *            転帰日
                   05  DISERES-END-DATE            PIC X(10).
      *            転帰区分
                   05  DISERES-OUTCOME             PIC X(02).
      *            補足コメント
                   05  DISERES-SUPPLEMENT-NAME     PIC X(80).
      *            補足コメントコード
                   05  DISERES-SUPPLEMENT-SINGLE-G.
                       07  DISERES-SUPPLEMENT-SINGLE   OCCURS   3.
      *                    補足コメントコード
                           09  DISERES-SUPPLEMENT-SINGLE-CODE
                                                   PIC X(20).
      *                    補足コメント
                           09  DISERES-SUPPLEMENT-SINGLE-NAME
                                                   PIC X(80).
      *            カルテ病名
                   05  DISERES-KARTE-NAME          PIC X(160).
      *            疾患区分
                   05  DISERES-DISEASE-CLASS       PIC X(02).
      *            レセプト表示フラグ
                   05  DISERES-RECEIPT-PRINT       PIC X(01).
      *            レセプト表示月数
                   05  DISERES-RECEIPT-PERIOD      PIC X(02).
      *            保険病名フラグ
                   05  DISERES-INSURANCE-DISEASE   PIC X(05).
      *            原疾患区分
                   05  DISERES-CLASSIFICATION-NUMBER-MASTER
                                                   PIC X(02).
      *            合併症区分
                   05  DISERES-CLASSIFICATION-NUMBER-SERVANT
                                                   PIC X(02).
      *            退院証明書
                   05  DISERES-DISCHARGEFLG        PIC X(1).
