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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ͭ¼     03.03.07  ����
      ************************************************************
       01  SKT1105.
      *    ����ǯ��
           03  SKT05-SRYYM                   PIC  X(16).
      *    ���ŵ��إ�����
           03  SKT05-HOSPCDN                 PIC  X(40).
      *    ���ŵ��ؾ���
           03  SKT05-HOSP1.
      *    ���ŵ��ؽ���
               05  SKT05-HOSPADRS1           PIC  X(100).
      *    ���ŵ���̾��
               05  SKT05-HOSPNAME1           PIC  X(100).
      *    �س��߼�̾
               05  SKT05-KAISETUNAME1        PIC  X(80).
      *    ���ŵ��������ֹ�
               05  SKT05-TEL1                PIC  X(15).
      *    ���ŵ���FAX
               05  SKT05-FAX1                PIC  X(15).
      *    ���ŵ��ؾ���
           03  SKT05-HOSP2.
               05  SKT05-HOSPADRS2           PIC  X(100).
               05  SKT05-HOSPNAME2           PIC  X(100).
               05  SKT05-KAISETUNAME2        PIC  X(80).
               05  SKT05-TEL2                PIC  X(15).
               05  SKT05-FAX2                PIC  X(15).
      *    Ģɼ��������
           03  SKT05-LSRYYM                  PIC  X(02).
           03  SKT05-LHOSPCD                 PIC  X(07).
           03  SKT05-LSEIKYUGK               PIC  X(09).
      *    Ϸ���ݸ�����ǡ���
           03  SKT05-RJN-TBL.
               05  SKT05-RJN-NYUGAI OCCURS 2.
      *    ���ܤε��ա����
                   07  SKT05-RKENSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-RNISSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-RTENSU           PIC  X(09).
      *    �������ܡ����
               05  SKT05-S-RKENSU             PIC  X(05).
      *    �������ܡ�����
               05  SKT05-S-RNISSU             PIC  X(05).
      *    �������ܡ�����������
               05  SKT05-S-RMONEY             PIC  X(09).
      *    �࿦�԰�������ǡ���
           03  SKT05-TSK-TBL.
               05  SKT05-TSK-NYUGAI OCCURS 2.
      *    ���ܤε��ա����
                   07  SKT05-TKENSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-TNISSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-TTENSU           PIC  X(09).
      *    �������ܡ����
               05  SKT05-S-TKENSU             PIC  X(05).
      *    �������ܡ�����
               05  SKT05-S-TNISSU             PIC  X(05).
      *    �������ܡ�����������
               05  SKT05-S-TMONEY             PIC  X(09).
      *    ���
           03  SKT05-GKI-TBL.
               05  SKT05-GKI-NYUGAI OCCURS 2.
      *    ���ܤε��ա����
                   07  SKT05-GKENSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-GNISSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-GTENSU           PIC  X(09).
      *    �������ܡ����
               05  SKT05-S-GKENSU             PIC  X(05).
      *    �������ܡ�����
               05  SKT05-S-GNISSU             PIC  X(05).
      *    �������ܡ�����������
               05  SKT05-S-GMONEY             PIC  X(09).
      *    �����ʧ�ζ��
               05  SKT05-GSEIKYUGK          PIC  X(10).
      *    �ݸ��ԡ���������ǡ���
           03  SKT05-SEIKYU-TBL OCCURS 7.
               05  SKT05-HKN-NYUGAI OCCURS 2.
      *    ���ܤε��ա����
                   07  SKT05-KENSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-NISSU           PIC  X(05).
      *    ���ܤε��ա�����
                   07  SKT05-TENSU           PIC  X(09).
      *    �������ܡ����
               05  SKT05-S-KENSU             PIC  X(05).
      *    �������ܡ�����
               05  SKT05-S-NISSU             PIC  X(05).
      *    �������ܡ�����������
               05  SKT05-S-MONEY             PIC  X(09).
      *    �����ʧ�ζ��
               05  SKT05-SEIKYUGK            PIC  X(10).
      *    �ݸ��ԡ�����̾
               05  SKT05-NAME                PIC  X(20).
      *    ��ô���
               05  SKT05-WARI                PIC  X(10).
      *    ʿ��
           03  SKT05-HEIKIN1                 PIC  X(09).
           03  SKT05-HEIKIN2                 PIC  X(09).
