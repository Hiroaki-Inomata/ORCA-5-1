      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *****************************************************************
      *    ���쥻���򼰥ץ������������� COPY (ORCSORCAPG.INC)       *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  SORCAPG-AREA.
           03  SORCAPG-SYSYMD              PIC X(08).
           03  SORCAPG-TYPE                PIC X(16).
           03  SORCAPG-NYUGAIKBN           PIC X(01).
           03  SORCAPG-SET-DEFAULT         PIC X(01).
           03  SORCAPG-KBNCD               PIC X(08).
           03  SORCAPG-PGID                PIC X(20).
           03  SORCAPG-SITEKBN             PIC X(01).
           03  SORCAPG-PGLST-MAX           PIC 9(05).
           03  SORCAPG-PGLST.
               05  SORCAPG-PGLST-OCC       OCCURS 200.
                   07  SORCAPG-TTYPE       PIC X(16).
                   07  SORCAPG-TPGID       PIC X(20).
                   07  SORCAPG-TSITEKBN    PIC X(01).
           03  SORCAPG-RC                  PIC 9(02).