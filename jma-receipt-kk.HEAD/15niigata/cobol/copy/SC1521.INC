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
      *  01.00.01    ����         10/06/07  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02    ����         10/06/22  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.03    ����         18/11/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SC21.
      *����ǯ��
           03  SC21-SRYYY                 PIC  X(04).
           03  SC21-SRYMM                 PIC  X(04).
      *���ŵ���CD
           03  SC21-HOSPCD                PIC  X(14).
      *���ŵ��ؽ����
           03  SC21-ADRS                  PIC  X(100).
      *���ŵ���̾��
           03  SC21-HOSPNAME              PIC  X(100).
      *���߼Ի�̾
           03  SC21-KAISETU               PIC  X(50).
      *���ŵ��������ֹ�
           03  SC21-TEL                   PIC  X(15).
      *�쥻�ץȼ���
           03  SC21-SYUBETU               PIC  X(02) OCCURS 8.
      *Ϸ���ݷ���ô���ֹ�(��Į¼�ֹ�)
           03  SC21-CITYNUM               PIC  X(16).
      *Ϸ���ݷ������ֹ�
           03  SC21-JKYSNUM               PIC  X(14).
      *�ݸ����ֹ�
           03  SC21-HKNJANUM              PIC  X(16).
      *���ճ��
           03  SC21-KYURATE               PIC  X(02) OCCURS 3.
      *��Į¼̾
           03  SC21-CITYNAME              PIC  X(16).
      *����
           03  SC21-KIGO                  PIC  X(16).
      *�ֹ�
           03  SC21-NUM                   PIC  X(16).
      *��������
           03  SC21-SEIYMD                PIC  X(22).
      *��̾
           03  SC21-NAME                  PIC  X(24).
      *���ʻ�̾
           03  SC21-KANANAME              PIC  X(40).
      *����
           03  SC21-SEX                   PIC  X(02) OCCURS 2.
      *����
           03  SC21-GENGO                 PIC  X(02) OCCURS 3.
      *������
           03  SC21-TANJOYMD              PIC  X(18).
      *��̾��(��������ԤΤ�)
           03  SC21-NAME2                 PIC  X(24).
      *���Խ���
           03  SC21-PTADRS                PIC  X(60).
      *���������ֹ�
           03  SC21-PTTEL                 PIC  X(15).
      *�����ʬ
           03  SC21-KOHKBN                PIC  X(02) OCCURS 6.
      *������
           03  SC21-NISSU                 PIC  X(02).
      *����
           03  SC21-TENSU                 PIC  X(07).
      *�����
           03  SC21-SEIKYUGK              PIC  X(09).
      *����
           03  SC21-SRYKANAME             PIC  X(20).
