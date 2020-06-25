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
      *    ＡＰＩリクエストユーザ管理情報  COPY (CPMANAGEUSERSV2RES.INC)
      ******************************************************************
        01  XML-MUREQ.
            02  MUREQ-CONTEXT                  PIC S9(9)   BINARY.
            02  MUREQ-OBJECT                   PIC X(36).
            02  MUREQ-MODE                     PIC S9(9)   BINARY.
            02  MUREQ-RECORDNAME               PIC X(128).
            02  MUREQ-MANAGE-USERSREQ.
              03  MUREQ-REQNUM                 PIC X(2).
              03  MUREQ-BASE-DATE              PIC X(10).
              03  MUREQ-USER-INFORMATION.
                04  MUREQ-USER-ID              PIC X(16).
                04  MUREQ-USER-PASSWORD        PIC X(16).
                04  MUREQ-GROUP-NUMBER         PIC X(1).
                04  MUREQ-FULL-NAME            PIC X(80).
                04  MUREQ-KANA-NAME            PIC X(80).
                04  MUREQ-NEW-USER-ID          PIC X(16).
                04  MUREQ-NEW-USER-PASSWORD    PIC X(16).
                04  MUREQ-NEW-FULL-NAME        PIC X(80).
                04  MUREQ-NEW-KANA-NAME        PIC X(80).
                04  MUREQ-ADMIN-PRIVILEGE      PIC X(01).
                04  MUREQ-MI-NUMBER-INFORMATION-G.
                  05  MUREQ-MI-NUMBER-INFORMATION-OCC OCCURS 50.
                    06  MUREQ-MI-NUMBER       PIC X(03).
                    06  MUREQ-MI-PRIVILEGE    PIC X(01).
