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
      *    ＡＰＩレスポンスユーザ管理情報  COPY (CPMANAGEUSERSV2RES.INC)
      ******************************************************************
        01  XML-MURES.
            02  MURES-CONTEXT                      PIC S9(9)   BINARY.
            02  MURES-OBJECT                       PIC X(36).
            02  MURES-MODE                         PIC S9(9)   BINARY.
            02  MURES-RECORDNAME                   PIC X(128).
            02  MURES-MANAGE-USERSRES.
              03  MURES-INFORMATION-DATE           PIC X(10).
              03  MURES-INFORMATION-TIME           PIC X(8).
              03  MURES-API-RESULT                 PIC X(4).
              03  MURES-API-RESULT-MSG             PIC X(100).
              03  MURES-REQNUM                     PIC X(2).
              03  MURES-BASE-DATE                  PIC X(10).
              03  MURES-USER-INFORMATION           OCCURS  600 TIMES.
      *     更新処理の場合、(n 1) ... 更新前／(n 2) ... 更新後
                04  MURES-USER-INF-OCC2            OCCURS  2.
                    05  MURES-USER-ID              PIC X(16).
                    05  MURES-HOSPNUM              PIC X(02).
                    05  MURES-GROUP-NUMBER         PIC X(1).
                    05  MURES-USER-NUMBER          PIC X(4).
                    05  MURES-FULL-NAME            PIC X(80).
                    05  MURES-KANA-NAME            PIC X(80).
                    05  MURES-STYUKYMD             PIC X(10).
                    05  MURES-EDYUKYMD             PIC X(10).
                04  MURES-ADMIN-PRIVILEGE          PIC X(01).
                04  MURES-MI-NUMBER-INFORMATION    OCCURS 50.
                    05  MURES-MI-NUMBER       PIC X(03).
                    05  MURES-MI-NAME         PIC X(30).
                    05  MURES-MI-PRIVILEGE    PIC X(01).
              03  MURES-MI-NAME-INFORMATION   OCCURS 50.
                04  MURES-MI-HNUMBER          PIC X(03).
                04  MURES-MI-HNAME            PIC X(30).
