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
      *    �ǡ��������оݥ������ް���  COPY (CPSTGTSCM.INC)           *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  STGTSCM-AREA.
           03  STGTSCM-OCC  OCCURS 200.
               05  STGTSCM-NO      PIC 9(03).
               05  STGTSCM-ID      PIC X(20).
               05  STGTSCM-NAME    PIC X(20).
           03  STGTSCM-MAX         PIC 9(05).