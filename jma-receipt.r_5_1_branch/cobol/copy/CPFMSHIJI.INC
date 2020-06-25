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
      *    ＡＰＩ　入院指示箋帳票情報        COPY (CPFMSHIJI.INC)
      ******************************************************************
        01  FMSHIJI-REC.
            03  FMSHIJI-CONTEXT                PIC S9(9)   BINARY.
            03  FMSHIJI-OBJECT                 PIC X(36).
            03  FMSHIJI-MODE                   PIC S9(9)   BINARY.
            03  FMSHIJI-RECORDNAME             PIC X(128).
            03  FMSHIJI-DATA.
              05  FMSHIJI-FORM-ID              PIC X(30).
              05  FMSHIJI-CUSTOM-ID            PIC X(30).
              05  FMSHIJI-PRINTER              PIC X(20).
      *       帳票タイトル
              05  FMSHIJI-TITLE-AREA.
                  07  FMSHIJI-TITLE-OCC        OCCURS 4.
                      09  FMSHIJI-TITLE        PIC X(30).
                      09  FMSHIJI-MAISU        PIC X(2).
              05  FMSHIJI-HAKKOKBN             PIC X(1).
      *       指示日
              05  FMSHIJI-SRYYMD               PIC X(10).
      *       処方日
              05  FMSHIJI-SYSYMD               PIC X(10).
      *       処方時刻
              05  FMSHIJI-SYSTIME              PIC X(5).
      *       診療科（日レセ）
              05  FMSHIJI-SRYKA                PIC X(2).
              05  FMSHIJI-SRYKANAME            PIC X(40).
      *       病棟番号
              05  FMSHIJI-BTUNUM               PIC X(2).
              05  FMSHIJI-BTUNAME              PIC X(20).
              05  FMSHIJI-BRMNUM               PIC X(6).
      *       患者情報
              05  FMSHIJI-PTINF.
                07  FMSHIJI-PTNUM              PIC X(20).
                07  FMSHIJI-NAME               PIC X(50).
                07  FMSHIJI-KANANAME           PIC X(50).
                07  FMSHIJI-BIRTHDAY           PIC X(10).
                07  FMSHIJI-SEX                PIC X(1).
      *       ドクター
              05  FMSHIJI-DOCTOR-AREA.
                07  FMSHIJI-DR-CODE             PIC X(5).
                07  FMSHIJI-DR-NAME             PIC X(50).
                07  FMSHIJI-DR-KANANAME         PIC X(50).
                07  FMSHIJI-DR-MAYAKULICENSE    PIC X(15).
      *       実施日
              05  FMSHIJI-JISIYMD               PIC X(10).
      *
              05  FMSHIJI-ZAI-AREA              OCCURS  50 TIMES.
      *         診療区分（４０、８０のみ）
                07  FMSHIJI-SRYKBN              PIC X(2).
                07  FMSHIJI-KAISU               PIC X(3).
                07  FMSHIJI-MEDICATION          OCCURS  50 TIMES.
      *           診療名称（薬剤名）
                  09  FMSHIJI-YKZNAME           PIC X(200).
                  09  FMSHIJI-SURYO             PIC X(12).
                  09  FMSHIJI-TANINAME          PIC X(12).
                  09  FMSHIJI-SRYCD             PIC X(9).
      *       05  FMSHIJI-MEMO                  PIC X(110)
      *                                         OCCURS  3 TIMES.
              05  FMSHIJI-OPID                  PIC X(16).
