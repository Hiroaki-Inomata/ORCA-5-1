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
      ******************************************************************
      *    ＡＰＩレスポンス中途データ一覧情報取得(xml2)
      *                                      COPY (CPTMDCGETV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *****************************************************************
        01  XML-TMEDICALGETRES.
            02  TMDCRES-CONTEXT           PIC S9(9)   BINARY.
            02  TMDCRES-OBJECT            PIC X(36).
            02  TMDCRES-MODE              PIC S9(9)   BINARY.
            02  TMDCRES-RECORDNAME        PIC X(128).
            02  TMDCRES-MEDICALRES.
      *       実施日
              03  TMDCRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  TMDCRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  TMDCRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  TMDCRES-API-RESULT-MSG      PIC X(100).
      *
              03  TMDCRES-RESKEY              PIC X(100).
      *
      *       受診日付
              03  TMDCRES-PERFORM-DATE        PIC X(10).
      *       入外区分
              03  TMDCRES-OUTPATIENT-CLASS    PIC X(1).
      *
      *       中途データ一覧
              03  TMDCRES-MEDICAL-INF         OCCURS   500.
      *           患者情報
                  05  TMDCRES-PATIENT-INFOMATION.
      *               患者番号
                      07  TMDCRES-PATIENTID        PIC X(20).
      *               患者氏名
                      07  TMDCRES-NAME             PIC X(100).
      *               患者カナ氏名
                      07  TMDCRES-KANANAME         PIC X(100).
      *               誕生日
                      07  TMDCRES-BIRTHDAY         PIC X(10).
      *               性別
                      07  TMDCRES-SEX              PIC X(1).
      *           診療科
                  05  TMDCRES-DEP-CODE             PIC X(02).
      *           診療科短縮名称
                  05  TMDCRES-DEP-NAME             PIC X(20).
      *           ドクターコード
                  05  TMDCRES-DR-CODE              PIC X(5).
      *           ドクター氏名
                  05  TMDCRES-DR-NAME              PIC X(80).
      *           保険組合せ番号
                  05  TMDCRES-COMBINATION-NUMBER   PIC X(04).
      *           保険組合せ情報
                  05  TMDCRES-INSURANCE-INFO.
      *             保険の種類
                    07  TMDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険の種類名称
                    07  TMDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             保険者番号
                    07  TMDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             記号
                    07  TMDCRES-MAIN-MARK              PIC X(80).
      *             番号
                    07  TMDCRES-MAIN-NUMBER            PIC X(80).
      *             公費情報
                    07  TMDCRES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               公費の種類
                      09  TMDCRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      09  TMDCRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      09  TMDCRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      09  TMDCRES-RECIPIENT-NUMBER        PIC X(20).
      *           同日再入院
                  05  TMDCRES-DOUJITSUKBN           PIC X(01).
      *
      *           ＵＩＤ
                  05  TMDCRES-MEDICAL-UID          PIC X(36).
      *           更新時間
                  05  TMDCRES-UPHMS                PIC X(08).
      *           状態区分１（1:展開中）
                  05  TMDCRES-MODE-FLG             PIC X(01).
      *           状態区分２（1:中途終了登録）
                  05  TMDCRES-KARTE-FLG            PIC X(01).
      *
