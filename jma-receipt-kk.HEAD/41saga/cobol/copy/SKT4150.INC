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
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2011/01/12  ��������������б�
      *  01.00.02    ����    2017/04/14  �����η������
      *  01.00.03    ����    2019/02/07  �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT4150.
      *�ڡ����ֹ�
            03  SKT4150-NO                           PIC X(04).
      *����ǯ��
            03  SKT4150-SRYYM                        PIC X(16).
      *����ǯ����
            03  SKT4150-SEIYMD                       PIC X(22).
      *���ŵ��ؽ���
            03  SKT4150-ADRS                         PIC X(100).
      *���ŵ���̾
            03  SKT4150-HOSPNAME                     PIC X(100).
      *���߼�̾
            03  SKT4150-KAISETUNAME                  PIC X(40).
      *�����ֹ�
            03  SKT4150-TEL                          PIC X(15).
      *�ݸ����ֹ�
            03  SKT4150-HKNJANUM                     PIC X(06).
      *���ŵ��إ�����
            03  SKT4150-HOSPCD                       PIC X(07).
      *�ݸ���̾
            03  SKT4150-HOKENJANAME                  PIC X(20).
      *����������
            03  SKT4150-TOKUBETU                     PIC X(10).
      *���ճ��
            03  SKT4150-KYURATE-TBL               OCCURS  4 TIMES.
                05  SKT4150-KYURATE                  PIC X(02).
      *����ơ��֥�
            03  SKT4150-SEIKYU-TBL                OCCURS  6 TIMES.
      *���
                05  SKT4150-KENSU                    PIC X(05).
      *����
                05  SKT4150-NISSU                    PIC X(05).
      *����
                05  SKT4150-TENSU                    PIC X(10).
      *������ô��
                05  SKT4150-ITBFTN                   PIC X(09).
      *
      *��������ơ��֥�
            03  SKT4150-KOUHI-TBL                 OCCURS  4 TIMES.
      *ˡ���ֹ�
                05  SKT4150-HBTNUM                   PIC X(02).
      *����̾
                05  SKT4150-KOHNAME                  PIC X(10).
      *���
                05  SKT4150-KKENSU                   PIC X(05).
      *����
                05  SKT4150-KNISSU                   PIC X(05).
      *����
                05  SKT4150-KTENSU                   PIC X(10).
      *������ô��
                05  SKT4150-KITBFTN                  PIC X(09).
      *������ô��
                05  SKT4150-KKANFTN                  PIC X(09).
      *Ϸ�͸���ˡ���ֹ�
            03  SKT4150-R-HBTNUM                     PIC X(02).
      *Ϸ�͸��񡼸���̾
            03  SKT4150-R-KOHNAME                    PIC X(08).
      *
      *Ϸ�͸�������ơ��֥�
            03  SKT4150-ROJINKOUHI-TBL            OCCURS  4 TIMES.
      *���
                05  SKT4150-R-KKENSU                 PIC X(05).
      *����
                05  SKT4150-R-KNISSU                 PIC X(05).
      *����
                05  SKT4150-R-KTENSU                 PIC X(10).
      *������ô
                05  SKT4150-R-KITBFTN                PIC X(09).
      *������
                05  SKT4150-R-KKOUJO                 PIC X(09).
