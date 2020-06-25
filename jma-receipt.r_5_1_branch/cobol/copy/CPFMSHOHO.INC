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
        01  FMSHOHO-REC.
            03  FMSHOHO-CONTEXT                PIC S9(9)   BINARY.
            03  FMSHOHO-OBJECT                 PIC X(36).
            03  FMSHOHO-MODE                   PIC S9(9)   BINARY.
            03  FMSHOHO-RECORDNAME             PIC X(128).
            03  FMSHOHO-DATA.
              05  FMSHOHO-FORM-ID            PIC X(30).
              05  FMSHOHO-CUSTOM-ID            PIC X(30).
              05  FMSHOHO-PRINTER              PIC X(20).
      *       1:前回処方、空白：今回処方、2:頭書のみ、3:院内分
      *       4:中途終了時（院外）、5:中途終了時（院内）
      *       A:処方内容変更、B:前回処方（院外）
      *       C:前回処方（院内）、D:前回処方（選択）
              05  FMSHOHO-KBN                  PIC X(1).
      *       診療日
              05  FMSHOHO-SRYYMD               PIC X(10).
      *       交付日
              05  FMSHOHO-KOFYMD               PIC X(10).
      *       診療科（日レセ）
              05  FMSHOHO-SRYKA                PIC X(2).
              05  FMSHOHO-SRYKANAME            PIC X(40).
      *       ページ編集(1:編集する）
              05  FMSHOHO-PAGE                 PIC X(1).
      *H30.4
      *       分割調剤　分割数
              05  FMSHOHO-BUNKATU-MAX          PIC 9(1).
      *       分割調剤　回目
              05  FMSHOHO-BUNKATU-CNT          PIC 9(1).
      *
              05  FMSHOHO-PTINF.
                07  FMSHOHO-PTNUM              PIC X(20).
                07  FMSHOHO-NAME               PIC X(50).
                07  FMSHOHO-KANANAME           PIC X(50).
                07  FMSHOHO-BIRTHDAY           PIC X(10).
                07  FMSHOHO-SEX                PIC X(1).
                07  FMSHOHO-TABOO              PIC X(100)  OCCURS  2.
                07  FMSHOHO-ALLERGY            PIC X(100)  OCCURS  2.
                07  FMSHOHO-KANSENSYO          PIC X(100)  OCCURS  2.
                07  FMSHOHO-COMMENT            PIC X(100)  OCCURS  2.
              05  FMSHOHO-HKNCOMBI-AREA.
                07  FMSHOHO-HKNCOMBI           PIC X(4).
                07  FMSHOHO-HKNNUM             PIC X(3).
      *             保険の種類名称
                07  FMSHOHO-HKNNAME            PIC X(20).
                07  FMSHOHO-HOKEN-AGE          PIC X(3).
                07  FMSHOHO-HOKEN-RATE         PIC X(3).
      *         負担割合区分
                07  FMSHOHO-HOKEN-RATEKBN      PIC X(1).
      *         1:高齢者一般、2:高齢者７割、3:６歳未満
                07  FMSHOHO-BIKOU-FTN          PIC X(1).
                07  FMSHOHO-HOKEN-G.
                  09  FMSHOHO-HKNJANUM         PIC X(8).
                  09  FMSHOHO-KIGO             PIC X(80).
                  09  FMSHOHO-NUM              PIC X(80).
      *           枝番(V05から）
                  09  FMSHOHO-EDABAN           PIC X(2).
                  09  FMSHOHO-HOJOKBN          PIC X(1).
                  09  FMSHOHO-HONKZKKBN        PIC X(1).
                  09  FMSHOHO-TEKSTYMD         PIC X(10).
                  09  FMSHOHO-TEKEDYMD         PIC X(10).
                07  FMSHOHO-KOHI-AREA          OCCURS  4.
                  09  FMSHOHO-KOH-HKNNUM        PIC X(3).
                  09  FMSHOHO-KOH-TANSEIDONAME  PIC X(20).
                  09  FMSHOHO-KOH-FTNJANUM      PIC X(8).
                  09  FMSHOHO-KOH-JKYSNUM       PIC X(20).
                  09  FMSHOHO-KOH-TEKSTYMD      PIC X(10).
                  09  FMSHOHO-KOH-TEKEDYMD      PIC X(10).
                07  FMSHOHO-ROSAI-AREA.
                  09  FMSHOHO-RSI-HKNKBN       PIC X(1).
                  09  FMSHOHO-RSI-KOFUNUM      PIC X(14).
                  09  FMSHOHO-RSI-RYOSTYMD     PIC X(10).
                  09  FMSHOHO-RSI-RYOEDYMD     PIC X(10).
                  09  FMSHOHO-RSI-SHOBYOYMD    PIC X(10).
              05  FMSHOHO-HOSP-AREA.
                07  FMSHOHO-PREFNUM             PIC X(2).
                07  FMSHOHO-HOSPCD              PIC X(7).
                07  FMSHOHO-HOSP-NAME-G.
                  09  FMSHOHO-HOSP-NAME         PIC X(120)
                                                OCCURS   3.
                07  FMSHOHO-HOSP-POST           PIC X(7).
      *         住所
                07  FMSHOHO-HOSP-ADRS-G.
                  09  FMSHOHO-HOSP-ADRS         PIC X(200)
                                                OCCURS   3.
                07  FMSHOHO-HOSP-TEL            PIC X(15).
                07  FMSHOHO-HOSP-FAX            PIC X(15).
              05  FMSHOHO-DOCTOR-AREA.
                07  FMSHOHO-DR-CODE             PIC X(5).
                07  FMSHOHO-DR-NAME             PIC X(50).
                07  FMSHOHO-DR-KANANAME         PIC X(50).
                07  FMSHOHO-DR-MAYAKULICENSE    PIC X(15).
      *
      *       1:保険医療機関へ疑義照会した上で調剤
      *       2:保険医療機関へ情報提供
              05  FMSHOHO-ZANSIJI              PIC X(1).
      *       1:麻薬を記載した処方せん
              05  FMSHOHO-MAYAKU               PIC X(1).
      *       1:変更不可保険医名の記載が必要
              05  FMSHOHO-KOUHATUFUKA-IN       PIC X(1).
              05  FMSHOHO-ZAI-AREA              OCCURS  50 TIMES.
                07  FMSHOHO-SRYKBN              PIC X(2).
                07  FMSHOHO-ZAIKAISU            PIC X(3).
      *         日分、回分
                07  FMSHOHO-ZAITANINAME         PIC X(04).
      *H30.4
      *         分割総投与日数
                07  FMSHOHO-GOKZAIKAISU         PIC X(3).
                07  FMSHOHO-MEDICATION          OCCURS  50 TIMES.
      *           薬剤名
                  09  FMSHOHO-YKZNAME           PIC X(210).
      *           数量(ZZZZ9.99999) 一般名処方で換算値の設定がある場合換算後の数量
                  09  FMSHOHO-SURYO             PIC X(12).
      *           一般名処方で換算値の設定がある場合換算後の単位
                  09  FMSHOHO-TANINAME          PIC X(12).
                  09  FMSHOHO-SRYCD             PIC X(9).
      *           1:一般名記載
                  09  FMSHOHO-KISAIKBN          PIC X(1).
                  09  FMSHOHO-YAKKAKJNCD        PIC X(13).
      *           1:変更不可
                  09  FMSHOHO-FUKA              PIC X(1).
              05  FMSHOHO-MEMO2   PIC X(40)     OCCURS  4  TIMES.
              05  FMSHOHO-MEMO    PIC X(100)    OCCURS  50 TIMES.
              05  FMSHOHO-QR-ID   PIC X(80).
