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
      *  �ץ�����ཤ������
      * Maj/Min/Rev ������  ����        ����
      *  01.00.01   ����    2008/05/27  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02   ����    2008/06/06  �ڡ����ֹ��ɲ�
      *  01.00.03   ����    2010/10/01  ��������������б�
      *  01.00.04   ���    2018/11/12  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT1900.
      *����ǯ��
           03  SKT1900-SRYYM               PIC  X(16).
      *���ŵ��إ�����
           03  SKT1900-HOSPCDN             PIC  X(20).
      *����
           03  SKT1900-ADRS                PIC  X(100).
      *���ŵ���̾
           03  SKT1900-HOSPNAME            PIC  X(100).
      *���߼Ի�̾
           03  SKT1900-KAISETUNAME         PIC  X(40).
      *�ڡ����ֹ�
           03  SKT1900-NO                  PIC  X(04).
      *����������
           03  SKT1900-TOKUBETU            PIC  X(10).
      *����ơ��֥�
           03  SKT1900-SEIKYU  OCCURS  10.
      *�ݸ����ֹ�
               05  SKT1900-HKNJANUM        PIC  X(16).
      *�ݸ���̾
               05  SKT1900-HKNJANAME       PIC  X(20).
      *���ճ��(�����ȹ���)
               05  SKT1900-KYURATE         PIC  X(10).
      *�ɾ��ơ��֥�
               05  SKT1900-KANSYO-TBL  OCCURS  9.
      *�����������ơ��֥�
                   07  SKT1900-NYUGAI-SKJ-TBL OCCURS 3.
      *���
                       09 SKT1900-KENSU        PIC  X(05).
      *����
                       09 SKT1900-NISSU        PIC  X(05).
      *���������
                       09 SKT1900-TENSU        PIC  X(09).
      *��ץơ��֥�
           03  SKT1900-GOKEI-TBL. 
      *�ɾ��ơ��֥�
               05  SKT1900-G-KANSYO-TBL  OCCURS  9.
      *�����������ơ��֥�
                   07  SKT1900-GNYUGAI-SKJ-TBL OCCURS 3.
      *��׷��
                       09 SKT1900-GKENSU        PIC  X(06).
      *�������
                       09 SKT1900-GNISSU        PIC  X(06).
      *������������
                       09 SKT1900-GTENSU        PIC  X(10).