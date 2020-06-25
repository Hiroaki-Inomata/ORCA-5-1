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
      *    ＡＰＩレスポンス来院一覧情報取得（来院日付）(xml2)
      *                                   COPY (CPVISITPTLST01V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-VISITPTLST01RES.
            02  VIS01RES-CONTEXT           PIC S9(9)   BINARY.
            02  VIS01RES-OBJECT            PIC X(36).
            02  VIS01RES-MODE              PIC S9(9)   BINARY.
            02  VIS01RES-RECORDNAME        PIC X(128).
            02  VIS01RES-MEDICALRES.
      *       実施日
              03  VIS01RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  VIS01RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  VIS01RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  VIS01RES-API-RESULT-MSG      PIC X(100).
      *
              03  VIS01RES-RESKEY              PIC X(100).
      *
      *       来院日付
              03  VIS01RES-VISIT-DATE          PIC X(10).
      *       受診履歴情報
              03  VIS01RES-MEDICAL-INF     OCCURS   1000.
      *           患者情報
                  05  VIS01RES-PATIENT-INFOMATION.
      *               患者番号
                      07  VIS01RES-PATIENTID         PIC X(20).
      *               患者氏名
                      07  VIS01RES-NAME              PIC X(100).
      *               患者カナ氏名
                      07  VIS01RES-KANANAME          PIC X(100).
      *               誕生日
                      07  VIS01RES-BIRTHDAY          PIC X(10).
      *               性別
                      07  VIS01RES-SEX               PIC X(1).
      *           診療科
                  05  VIS01RES-DEP-CODE          PIC X(02).
      *           診療科短縮名称
                  05  VIS01RES-DEP-NAME          PIC X(20).
      *           ドクターコード
                  05  VIS01RES-DR-CODE           PIC X(5).
      *           ドクター氏名
                  05  VIS01RES-DR-NAME           PIC X(80).
      *           伝票番号
                  05  VIS01RES-DENPNUM           PIC X(07).
      *           連番
                  05  VIS01RES-NUMBER            PIC X(01).
      *           保険組合せ番号
                  05  VIS01RES-COMBINATION-NUMBER  PIC X(04).
      *           保険組合せ情報
                  05  VIS01RES-INSURANCE-INFO.
      *             保険の種類
                    07  VIS01RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険の種類名称
                    07  VIS01RES-MAIN-INSURANCE-NAME    PIC X(20).
      *             保険者番号
                    07  VIS01RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             記号
                    07  VIS01RES-MAIN-MARK              PIC X(80).
      *             番号
                    07  VIS01RES-MAIN-NUMBER            PIC X(80).
      *             公費情報
                    07  VIS01RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               公費の種類
                      09  VIS01RES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      09  VIS01RES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      09  VIS01RES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      09  VIS01RES-RECIPIENT-NUMBER        PIC X(20).
      *           更新日
                  05  VIS01RES-UPDATE-DATE         PIC X(10).
      *           更新時間
                  05  VIS01RES-UPDATE-TIME         PIC X(08).
      *           患者情報　更新日
                  05  VIS01RES-PTNUM-UPDATE        PIC X(10).
      *           患者情報　更新時間
                  05  VIS01RES-PTNUM-UPTIME        PIC X(08).
      *
