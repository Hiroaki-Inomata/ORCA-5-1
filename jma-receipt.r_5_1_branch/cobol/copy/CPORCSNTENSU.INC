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
      ******************************************************************
      *    ���������ޥ����ղþ���
      ******************************************************************
        01 SNTENSU-AREA.
           03  SNTENSU-BASEYMD                 PIC X(8).
           03  SNTENSU-SRYCD                   PIC X(9).
           03  SNTENSU-RETURN-AREA.
               05  SNTENSU-KBNNUM              PIC 9(3).
               05  SNTENSU-EDANUM              PIC 9(2).
      *        �������������ʬ 1:�����������
               05  SNTENSU-DEKIDAKAKBN         PIC X(1).
      *        ������������������ʬ
      *          1:��������������
      *          2:��ʿ����л���Ķ�Ḻ��
      *          3:��л�����������������
      *          4:�žɴ��Գ����������������
               05  SNTENSU-TOKUBETSUKBN        PIC X(1).