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
      ************************************************************
      *    �������������������å�����COPY (CPORCSS007.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������        ����      ����
      ************************************************************
       01  SS007-AREA.
           03  SS007-IN.
      *����ǯ��
               05  SS007-SRYYM             PIC X(06).
           03  SS007-OT.
      *�꥿���󥳡��� (0:���� / 1:�����礢��)
               05  SS007-RC                PIC 9(01).
      *����������������꥿���󥳡��� (0:���� / 1:�������ܤؤΰ�ư�оݼ�)
               05  SS007-RC2               PIC 9(01).
      *��Ʋ�û������ǽ����������о��� (0:�оݳ� / 1:�о�)
               05  SS007-RC3-TBL.
                 07    SS007-RC3           PIC X(01)    OCCURS  31.