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
      *                     COPY (CPSKT2510TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2009/06/01  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02    ����    2010/04/14  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.03    ����    2010/11/01  ��������������б�
      *  01.00.04    ���    2018/12/12  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT2510.
      *����ǯ��
           03  SKT2510-SRYYM               PIC  X(16).
      *���ŵ��إ�����
           03  SKT2510-HOSPCD              PIC  X(07).
      *���ŵ��ؽ����
           03  SKT2510-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT2510-HOSPNAME            PIC  X(100).
      *���߼Ի�̾
           03  SKT2510-KAISETU             PIC  X(80).
      *�����ֹ�
           03  SKT2510-TEL                 PIC  X(15).
      *����ǯ����
           03  SKT2510-SEIYMD              PIC  X(22).
      *����������
           03  SKT2510-TOKUBETU            PIC  X(10).
      *Ģɼ���ꥨ�ꥢ
           03  SKT2510-KOTEI-TBL  OCCURS 25.
               05  SKT2510-K-KENSU         PIC  X(05).
      *���ᥨ�ꥢ
           03  SKT2510-SEIKYU-TBL  OCCURS 50.
      *�ݸ���̾
               05  SKT2510-HKNJANAME1      PIC  X(12).
               05  SKT2510-HKNJANAME2      PIC  X(28).
      *�ݸ����ֹ�
               05  SKT2510-HKNJANUM        PIC  X(06).
      *���ճ��
               05  SKT2510-KYURATE         PIC  X(02).
      *���
               05  SKT2510-KENSU           PIC  X(05).
      *ʡ������������������
           03  SKT2510-FKSGMAISU           PIC  X(05).
      *��������������
           03  SKT2510-JIKIMAISU           PIC  X(05).
      *��׷��
           03  SKT2510-GKENSU              PIC  X(05).
      *�������ԥ��ꥢ
           03  SKT2510-KOUKI-TBL  OCCURS 24.
               05  SKT2510-TODOHUKEN       PIC  X(12).
               05  SKT2510-KENSUK          PIC  X(05).
      *��׷���ʸ������ԡ�
           03  SKT2510-GKENSUK             PIC  X(05).
