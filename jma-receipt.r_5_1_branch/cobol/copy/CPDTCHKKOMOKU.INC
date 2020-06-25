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
      *    データチェック確認項目        COPY (CPDTCHKKOMOKU.INC)*
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  DTCHKKOMOKU-AREA.
           03  DTCHKKOMOKU-VAL.
               05                          PIC X(02)   VALUE
               "01".
               05                          PIC X(40)   VALUE
               "保険者番号".
               05                          PIC X(50)   VALUE
               "InsuranceProvider_Number".
               05                          PIC X(02)   VALUE
               "02".
               05                          PIC X(40)   VALUE
               "被保険者記号・番号".
               05                          PIC X(50)   VALUE
               "HealthInsuredPerson_Symbol_And_Number".
               05                          PIC X(02)   VALUE
               "03".
               05                          PIC X(40)   VALUE
               "公費負担者番号・受給者番号".
               05                          PIC X(50)   VALUE
               "PublicInsurance".
               05                          PIC X(02)   VALUE
               "04".
               05                          PIC X(40)   VALUE
               "保険適応年齢".
               05                          PIC X(50)   VALUE
               "Age".
               05                          PIC X(02)   VALUE
               "05".
               05                          PIC X(40)   VALUE
               "患者病名".
               05                          PIC X(50)   VALUE
               "Disease".
               05                          PIC X(02)   VALUE
               "06".
               05                          PIC X(40)   VALUE
               "診療開始日".
               05                          PIC X(50)   VALUE
               "Disease_StartDate".
               05                          PIC X(02)   VALUE
               "07".
               05                          PIC X(40)   VALUE
               "疑い病名・急性病名".
               05                          PIC X(50)   VALUE
               "Disease_Suspected_And_Disease_Acute".
               05                          PIC X(02)   VALUE
               "08".
               05                          PIC X(40)   VALUE
               "薬剤と病名".
               05                          PIC X(50)   VALUE
               "Medicine_With_Disease".
               05                          PIC X(02)   VALUE
               "09".
               05                          PIC X(40)   VALUE
               "診療行為と病名".
               05                          PIC X(50)   VALUE
               "Medication_With_Disease".
               05                          PIC X(02)   VALUE
               "10".
               05                          PIC X(40)   VALUE
               "薬剤と併用禁忌".
               05                          PIC X(50)   VALUE
               "Medicine_Interact".
               05                          PIC X(02)   VALUE
               "11".
               05                          PIC X(40)   VALUE
               "診療行為の併用算定".
               05                          PIC X(50)   VALUE
               "Medication_Combination".
               05                          PIC X(02)   VALUE
               "12".
               05                          PIC X(40)   VALUE
               "実日数".
               05                          PIC X(50)   VALUE
               "Actual_Days".
               05                          PIC X(02)   VALUE
               "13".
               05                          PIC X(40)   VALUE
               "患者氏名".
               05                          PIC X(50)   VALUE
               "WholeName".
               05                          PIC X(02)   VALUE
               "14".
               05                          PIC X(40)   VALUE
               "保険組合せ有効期間".
               05                          PIC X(50)   VALUE
               "Insurance_ExpiredDate".
               05                          PIC X(02)   VALUE
               "15".
               05                          PIC X(40)   VALUE
               "投与禁忌薬剤と病名".
               05                          PIC X(50)   VALUE
               "Medicine_Disease_Interact".
               05                          PIC X(02)   VALUE
               "16".
               05                          PIC X(40)   VALUE
               "診療行為どうしの算定漏れ".
               05                          PIC X(50)   VALUE
               "Medication_Combination_Leak".
               05                          PIC X(02)   VALUE
               "17".
               05                          PIC X(40)   VALUE
               "病名と薬剤".
               05                          PIC X(50)   VALUE
               "Disease_With_Medicine".
               05                          PIC X(02)   VALUE
               "18".
               05                          PIC X(40)   VALUE
               "病名と診療行為".
               05                          PIC X(50)   VALUE
               "Disease_With_Medication".
               05                          PIC X(02)   VALUE
               "19".
               05                          PIC X(40)   VALUE
               "薬剤の月上限回数・総投与量".
               05                          PIC X(50)   VALUE
               "Medicine_Dosage".
               05                          PIC X(02)   VALUE
               "20".
               05                          PIC X(40)   VALUE
               "診療行為算定回数".
               05                          PIC X(50)   VALUE
               "Medication_Count".
               05                          PIC X(02)   VALUE
               "21".
               05                          PIC X(40)   VALUE
               "選択式コメント".
               05                          PIC X(50)   VALUE
               "Selective_Comment".
           03  DTCHKKOMOKU-R   REDEFINES   DTCHKKOMOKU-VAL.
               05  DTCHKKOMOKU-CHKKOMOKU-OCC           OCCURS  21.
                   07  DTCHKKOMOKU-CHKNUM      PIC X(02).
                   07  DTCHKKOMOKU-CHKKOMOKU   PIC X(40).
                   07  DTCHKKOMOKU-CHKKOMOKUID PIC X(50).
           03  DTCHKKOMOKU-MAX             PIC 9(03)   VALUE   21.
      *
       01  DTCHKHOKEN-AREA.
           03  DTCHKHOKEN-VAL.
               05  DTCHKHOKEN-SHAHO            PIC X(40)   VALUE
               "社保".
               05  DTCHKHOKEN-SHAHOID          PIC X(50)   VALUE
               "Employees_Health_Insurance".
               05  DTCHKHOKEN-KOKUHO           PIC X(40)   VALUE
               "国保".
               05  DTCHKHOKEN-KOKUHOID         PIC X(50)   VALUE
               "National_Health_Insurance".
               05  DTCHKHOKEN-KOKI             PIC X(40)   VALUE
               "後期高齢者".
               05  DTCHKHOKEN-KOKIID           PIC X(50)   VALUE
               "Late_Elderly_Health_Insurance".
               05  DTCHKHOKEN-RSIJBI           PIC X(40)   VALUE
               "労災・自賠責・第三者行為".
               05  DTCHKHOKEN-RSIJBIID         PIC X(50)   VALUE
               "Accident_Insurance".
               05  DTCHKHOKEN-KOGAI            PIC X(40)   VALUE
               "公害".
               05  DTCHKHOKEN-KOGAIID          PIC X(50)   VALUE
               "Pollution_Insurance".
               05  DTCHKHOKEN-JIHI             PIC X(40)   VALUE
               "自費".
               05  DTCHKHOKEN-JIHIID           PIC X(50)   VALUE
               "Own_Expense".
           03  DTCHKHOKEN-R    REDEFINES   DTCHKHOKEN-VAL.
               05  DTCHKHOKEN-CHKHOKEN-OCC             OCCURS  6.
                   07  DTCHKHOKEN-CHKHOKEN     PIC X(40).
                   07  DTCHKHOKEN-CHKHOKENID   PIC X(50).
           03  DTCHKHOKEN-MAX              PIC 9(03)   VALUE   6.
