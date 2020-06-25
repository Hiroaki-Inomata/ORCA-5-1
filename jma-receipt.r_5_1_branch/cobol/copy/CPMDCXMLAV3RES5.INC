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
      *    ＡＰＩレスポンス入院調剤料一括変更情報 (一体化)
      *                                      COPY (CPMDCXMLAV3RE5.INC)
      *         診療行為内容（入院）
      ******************************************************************
       01  XML-MEDICALAV3RES5.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *        リクエスト番号
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        レスポンス番号
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID（電子カルテ）
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID（オルカ）
               05  MDCRES-ORCA-UID          PIC X(36).
      *        実施日
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        実施時間
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        返却コード
               05  MDCRES-API-RESULT        PIC X(3).
      *        返却メッセージ
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *
      *        患者情報
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      *        診療年月
               05  MDCRES-PERFORM-YM       PIC X(07).
      *
      *リクエスト＝０１
      *  投薬剤情報
               05  MDCRES-MEDICAL-INF-G.
                 07  MDCRES-MDC-INFO          OCCURS   100.
      *             診療科
                    09  MDCRES-MDC-DEP-CODE             PIC X(2).
      *             診療科名称
                    09  MDCRES-MDC-DEP-CODE-NAME        PIC X(20).
      *             保険組合せ番号
                    09  MDCRES-MDC-COMBINATION-NUMBER   PIC X(04).
      *             保険の種類名称
                    09  MDCRES-MDC-MAIN-INS-NAME        PIC X(20).
                    09  MDCRES-MDC-PUBLIC-INS-INFO.
      *             公費の種類名称
                        11  MDCRES-MDC-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *             保険組合せ番号状態
                    09  MDCRES-MDC-COMBINATION-MODE   PIC X(01).
      *             剤番号
      ****          09  MDCRES-MDC-ZAINUM            PIC X(08).
      *
      *             診療区分
                    09  MDCRES-MDC-CLASS-KBN         PIC X(2).
      *             診療種別区分
                    09  MDCRES-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *             算定日毎の回数
                    09  MDCRES-MDC-DAY-G.
      *                 算定履歴
                        11  MDCRES-MDC-RRK-G.
      *                 回数
                            13  MDCRES-MDC-CLASS-NUMBER  PIC X(03)
                                                     OCCURS   31.
      *             同日再入院の算定
                    09  MDCRES-MDC-REDAY-G.
                        11  MDCRES-MDC-REDAY-OCC    OCCURS   31.
      *                     同日再入院日
                            13  MDCRES-MDC-REDAY        PIC X(02).
      *                     回数
                            13  MDCRES-MDC-RECLASS-NUMBER  PIC X(03).
      *             剤点数
                    09  MDCRES-MDC-POINT             PIC X(08).
      *
      *             包括区分（True：包括剤）
                    09  MDCRES-INCLUSION-CLASS       PIC X(05).
      *
                    09  MDCRES-PRSCRPT-INFO-G.
                      10  MDCRES-PRESCRIPTION-INFO    OCCURS   50.
      *                 診療コード
                        11  MDCRES-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        11  MDCRES-PRSCRPT-NAME          PIC X(140).
      *                 数量
                        11  MDCRES-PRSCRPT-NUMBER        PIC X(11).
      *                 単位コード
                        11  MDCRES-PRSCRPT-TANICD        PIC X(03).
      *                 単位名称
                        11  MDCRES-PRSCRPT-TANICD-NAME   PIC X(24).
      *                 自動区分
                        11  MDCRES-PRSCRPT-AUTOKBN     PIC X(01).
      *
      *        入院調剤料返却
               05  MDCRES-MEDICAL-CHOZAIINF-G.
                   07  MDCRES-MDC-CHOZA-INF        OCCURS   10.
      *             診療科
                    09  MDCRES-CHZ-DEP-CODE        PIC X(2).
      *             診療科
                    09  MDCRES-CHZ-DEP-CODE-NAME   PIC X(20).
      *             保険組合せ番号
                    09  MDCRES-CHZ-COMBINATION     PIC X(4).
      *             保険の種類名称
                    09  MDCRES-CHZ-MAIN-INS-NAME   PIC X(20).
                    09  MDCRES-CHZ-PUBLIC-INS-INFO.
      *             公費の種類名称
                        11  MDCRES-CHZ-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *             保険組合せ番号状態
                    09  MDCRES-CHZ-COMBINATION-MODE   PIC X(01).
      *
      *             剤番号
                    09  MDCRES-CHZ-ZAINUM            PIC X(8).
      *
      *             診療区分
                    09  MDCRES-CHZ-CLASS-KBN         PIC X(2).
      *             診療種別区分
                    09  MDCRES-CHZ-CLASS             PIC X(3).
      *             診療種別名称
                    09  MDCRES-CHZ-CLASS-NAME        PIC X(40).
      *             算定日
                    09  MDCRES-CHZ-DAY-G.
      *                 算定履歴
                        11  MDCRES-CHZ-RRK-G.
      *                 回数
                            13  MDCRES-CHZ-CLASS-NUMBER  PIC X(03)
                                                     OCCURS   31.
      *             同日再入院の算定
                    09  MDCRES-CHZ-REDAY-G.
                        11  MDCRES-CHZ-REDAY-OCC    OCCURS   31.
      *                     同日再入院日
                            13  MDCRES-CHZ-REDAY        PIC X(02).
      *                     回数
                            13  MDCRES-CHZ-RECLASS-NUMBER  PIC X(03).
      *
      *             剤点数
                    09  MDCRES-CHZ-POINT             PIC X(08).
      *             包括区分（True：包括剤）
                    09  MDCRES-CHZ-INCLUSION-CLASS   PIC X(05).
      *
                    09  MDCRES-CHZ-INFO-G.
                        11  MDCRES-CHZ-INFO    OCCURS   5.
      *                 診療コード
                        13  MDCRES-CHZ-CODE        PIC X(9).
      *                 診療名称
                        13  MDCRES-CHZ-NAME        PIC X(140).
      *                 自動区分
                        13  MDCRES-CHZ-AUTOKBN     PIC X(01).
      *
      *
      *        メッセージ領域
               05  MDCRES-MEDICAL-MSG1-G.
      *           エラーコード
                  07  MDCRES-MEDI-RESULT         PIC X(04).
      *           エラーメッセージ
                  07  MDCRES-MEDI-RESULT-MSG.
                      09  MDCRES-MEDI-RESULT-MSG1        PIC X(100).
                      09  MDCRES-MEDI-RESULT-MSG2        PIC X(100).
      *           ワーニングメッセージ
                  07  MDCRES-MEDI-WARNING-TBL    OCCURS  40.
      *               ワーニングコード
                      09  MDCRES-WARNING             PIC X(04).
      *               ワーニングメッセージ
                      09  MDCRES-WARNING-MSG         PIC X(100).
      *
