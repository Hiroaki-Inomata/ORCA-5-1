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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         12/08/14  �ڡ����������ڡ������η������
      *  01.00.02    ���         18/09/05  ���ץ����Ȥ���
      *  01.00.03    ����         19/05/10  �������դ˸����ɲ�
      *****************************************************************
       01  SC13.
      *��������
           03  SC13-PRTG              PIC  X(04).
           03  SC13-PRTY              PIC  X(04).
           03  SC13-PRTM              PIC  X(04).
      *�����
           03  SC13-GOKEI-TBL.
      *��סݷ��
               05  SC13-GKENSU        PIC  X(05).
      *��ס��ݸ�������
               05  SC13-GTENSU        PIC  X(09).
      *��סݷ�ˡ�������ô��
               05  SC13-GKESETEN      PIC  X(09).
      *���ŵ��إ�����
           03  SC13-HOSPCD            PIC  X(14).
      *��������
           03  SC13-SEIYMD            PIC  X(22).
      *���ŵ��ؽ����
           03  SC13-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC13-HOSPNAME          PIC  X(100).
      *������̾
           03  SC13-KAISETU           PIC  X(40).
      *�����ֹ�
           03  SC13-TEL               PIC  X(15).
      *���ڡ�����
           03  SC13-GPAGE             PIC  X(03).
      *�ڡ�����
           03  SC13-PAGE              PIC  X(03).
      *
           03  SC13-MEISAI-TBL OCCURS 10.
      *�����ʼԾڵ����ֹ�
               05  SC13-KIGO           PIC  X(04).
               05  SC13-NUM            PIC  X(14).
      *�ݸ����ֹ�
               05  SC13-HKNJANUM       PIC  X(16).
      *����Ի�̾
               05  SC13-KANANAME       PIC  X(30).
               05  SC13-NAME           PIC  X(20).
      *���������ʬ
               05  SC13-NYUGAIKBN      PIC  X(02) OCCURS 2.
      *�ݸ�������
               05  SC13-TENSU.
                   07  SC13-TENSU-OCC  PIC  X(02) OCCURS 5.
      *��ˡ�������ʬ
               05  SC13-KESEKBN        PIC  X(02) OCCURS 2.
      *��ˡ���������
               05  SC13-KESETEN        PIC  X(10).
      *���Ų���
               05  SC13-SRYKA          PIC  X(02).
      *����ǯ��
               05  SC13-SRYYY          PIC  X(04).
               05  SC13-SRYMM          PIC  X(04).
      *����
           03  SC13-SYOKEI-TBL.
      *
      *���ס��ݸ�������
               05  SC13-STENSU.
                   07  SC13-STENSU-OCC  PIC  X(02) OCCURS 5.
      *���סݷ�ˡ�������ô��
               05  SC13-SKESETEN       PIC  X(10).
