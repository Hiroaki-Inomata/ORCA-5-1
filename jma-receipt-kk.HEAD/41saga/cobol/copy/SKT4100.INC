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
      * Maj/Min/Rev  ������    ����      ����
      *  01.00.01    ����      08/12/24  ��������������б�
      *  01.00.02    ����      09/05/15  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.03    ����      17/04/14  �����η������
      *  01.00.04    ����      19/02/07  �������򥳥��Ⱥ��
      *****************************************************************
       01  SKT4100.
      *����ǯ��
           03  SKT4100-SRYYM               PIC  X(16).
      *���Ų�̾
           03  SKT4100-SRYKA               PIC  X(20) OCCURS 3.
      *���ŵ��ؽ����
           03  SKT4100-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT4100-HOSPNAME            PIC  X(100).
      *���ŵ��������ֹ�
           03  SKT4100-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT4100-KAISETUNAME         PIC  X(30).
      *���ŵ��إ�����
           03  SKT4100-HOSPCDN             PIC  X(20).
      *����������
           03  SKT4100-TOKUBETU            PIC  X(10).
      *�������
           03  SKT4100-SEIKYU  OCCURS 10.
      *�ݸ���̾
               05  SKT4100-HKNJANAME       PIC  X(20).
      *���ճ��
               05  SKT4100-KYURATE         PIC  X(08).
      *���ݡ��࿦�ԡ�Ϸ�ͥơ��֥�
               05  SKT4100-HKN-TBL  OCCURS  3.
      *����������ơ��֥�
                   07  SKT4100-NYUGAIKBN   OCCURS  2.
      *���
                       09  SKT4100-KENSU   PIC  X(05).
      *����
                       09  SKT4100-NISSU   PIC  X(05).
      *����
                       09  SKT4100-TENSU   PIC  X(10).
      *���
           03  SKT4100-SUM-TBL  OCCURS  2.
      *����������ơ��֥�
               05  SKT4100-GNYUGAI-TBL  OCCURS  2.
      *���
                   07  SKT4100-GKENSU      PIC  X(05).
      *����
                   07  SKT4100-GNISSU      PIC  X(05).
      *����
                   07  SKT4100-GTENSU      PIC  X(10).
      *���濫����
           03  SKT4100-AVG-TBL  OCCURS  2.
      *����������ơ��֥�
               05  SKT4100-ANYUGAI-TBL  OCCURS  2.
      *����
                   07  SKT4100-ATENSU      PIC  X(10).
      *���������������
           03  SKT4100-GKENGAI             PIC  X(06).
           03  SKT4100-GKENNAI             PIC  X(06).
      *������� ���⡦���������
           03  SKT4100-GKOUKENGAI          PIC  X(06).
           03  SKT4100-GKOUKENNAI          PIC  X(06).
