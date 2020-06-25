      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
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
      *  �v���O�����C������
      * Maj/Min/Rev  �C����       ���t      ���e
      * 01.00.01     @@@@-���    10/12/07  �R�����g�����폜
      ********************************************************************
       01  HC90.
              03  HC90-SYSYMD        PIC X(22).
              03  HC90-PAGE          PIC X(3).
              03  HC90-SRYYMD        PIC X(22).
              03  HC90-CMSG1         PIC X(60).
              03  HC90-MIDASI        PIC X(10).
              03  HC90-NYUGAIKBN     PIC X(4).
              03  HC90-TSRYKA        PIC X(20).
              03  HC90-TKENSU1       PIC X(20).
              03  HC90-TKENSU2       PIC X(20).
              03  HC90-TKENSU3       PIC X(20).
              03  HC90-THKN1         PIC X(20).
              03  HC90-TRYO          PIC X(20).
              03  HC90-TFUTAN1       PIC X(22).
              03  HC90-TFUTAN2       PIC X(22).
              03  HC90-TFUTAN3       PIC X(20).
              03  HC90-TJIHI-G                   OCCURS  10 TIMES.
                  05  HC90-TJIHI     PIC X(8).
              03  HC90-YOSO-G                    OCCURS  11 TIMES.
                  05  HC90-YOSO-OCC              OCCURS  35 TIMES.
                      07  HC90-YOSO  PIC X(12).