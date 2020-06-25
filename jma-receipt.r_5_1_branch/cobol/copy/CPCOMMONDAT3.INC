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
      *
      *     ����ե����� ̾���ΰ�      CPCOMMONDAT3.INC
      *
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 
      *****************************************************************
       01  TMPFLPARA-AREA.
         03  TMPFLPARA.
           05  TMPFLPARA-OLD.
               07  FILLER                  PIC X(05)   VALUE   "/tmp/".
               07  TMPFLPARA-BASENAME.
                   09  TMPFLPARA-HOSPNUM   PIC 9(2).
                   09  TMPFLPARA-SYOKBN1   PIC 9(01)   VALUE   0.
                   09  TMPFLPARA-SYOKBN2   PIC 9(01)   VALUE   0.
                   09  TMPFLPARA-FILE-ID   PIC X(08).
                   09  TMPFLPARA-TERMID    PIC X(64).
                   09  FILLER              PIC X(04)   VALUE   ".dat".
               07  FILLER                  PIC X(939)  VALUE   SPACE.
           05  TMPFLPARA-FULLNAME-R REDEFINES TMPFLPARA-OLD.
               07  TMPFLPARA-FULLNAME    PIC  X(1024).