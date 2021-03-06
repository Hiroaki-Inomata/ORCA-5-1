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
      * Maj/Min/Rev  ������   ����         ����
      *  01.00.01    ����     2012/04/20   Ģɼ�ͼ����ѹ�
      *  01.00.02    ���     2018/12/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2500.
      *����ǯ��
           03  SKT2500-SRYYM               PIC  X(16).
      *���ŵ��إ�����
           03  SKT2500-HOSPCD              PIC  X(07).
      *�쥻�ץȵ���
           03  SKT2500-RECE                PIC  X(02)  OCCURS 2.
      *���ŵ��ؽ����
           03  SKT2500-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT2500-HOSPNAME            PIC  X(100).
      *���߼Ի�̾
           03  SKT2500-KAISETU             PIC  X(80).
      *�����ֹ�
           03  SKT2500-TEL                 PIC  X(15).
      *����ǯ����
           03  SKT2500-SEIYMD              PIC  X(22).
      *�������
           03  SKT2500-SEIKYU  OCCURS 32.
               05  SKT2500-KENSU           PIC  X(05).
      *¾�������
           03  SKT2500-ESEIKYU  OCCURS 7.
               05  SKT2500-EHKNJANAME1     PIC  X(14).
               05  SKT2500-EHKNJANAME2     PIC  X(36).
               05  SKT2500-EHKNJANUM       PIC  X(06).
               05  SKT2500-EKENSU          PIC  X(05).
      *����������
           03  SKT2500-TOKURYO             PIC  X(03).
      *����������ʸ������ԡ�
           03  SKT2500-TOKURYOK            PIC  X(03).
      *������������������
           03  SKT2500-KOUSEIKYU  OCCURS 5.
      *��ƻ�ܸ�̾
               05  SKT2500-KENNAME         PIC  X(10).
      *����������
               05  SKT2500-KOUKENSU        PIC  X(05).
      *����������
           03  SKT2500-GSEIKYU  OCCURS 2.
      *��׷��
               05  SKT2500-GKENSU          PIC  X(06).
      *��׷���ʸ������ԡ�
               05  SKT2500-GKENSUK         PIC  X(06).
      *ʡ��������
           03  SKT2500-FKENSUB             PIC  X(03).
      *ʡ������
           03  SKT2500-FKENSUK             PIC  X(03).
