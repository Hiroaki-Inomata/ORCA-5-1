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
      *  01.00.01    ����      2010/12/08   ��������������б�
      ********************************************************************
        01  SKT3110.
      *���ŵ��إ�����
            03  SKT3110-HOSPCD                        PIC X(14).
      *�ݸ����ֹ�
            03  SKT3110-HKNJANUM                      PIC X(12).
      *���ŵ��ؽ���
            03  SKT3110-ADRS                          PIC X(100).
      *���ŵ���̾
            03  SKT3110-HOSPNAME                      PIC X(100).
      *�����ֹ�
            03  SKT3110-TEL                           PIC X(15).
      *���߼�̾
            03  SKT3110-KAISETU                       PIC X(60).
      *����ǯ��
            03  SKT3110-SRYYM                         PIC X(16).
      *����ǯ����
            03  SKT3110-SEIYMD                        PIC X(22).
      *����������
            03  SKT3110-TOKUBETU                      PIC X(10).
      *�ڡ����ֹ�
            03  SKT3110-NO                            PIC X(04).
      *����ơ��֥�
            03  SKT3110-SEIKYU-TBL                    OCCURS  7 TIMES.
      *������ʬ
              05  SKT3110-NYUGAI-TBL                  OCCURS  2 TIMES.
      *���
                07  SKT3110-KENSU                     PIC X(05).
      *������
                07  SKT3110-NISSU                     PIC X(05).
      *����
                07  SKT3110-TENSU                     PIC X(09).
      *���ް�����ô��
                07  SKT3110-YKZFTN                    PIC X(07).
      *������ô��
                07  SKT3110-ITBFTN                    PIC X(09).
      *���������
              05  SKT3110-SKENSU                      PIC X(05).
      *����������
              05  SKT3110-SNISSU                      PIC X(05).
      *���������
              05  SKT3110-SKINGK                      PIC X(09).
      *������ɸ����ô��
              05  SKT3110-SFTNGK                      PIC X(09).
      *����ơ��֥�
            03  SKT3110-ZAITAKU-TBL                   OCCURS  4 TIMES.
      *���𡼷��
              05  SKT3110-ZKENSU                      PIC X(05).
      *��������
              05  SKT3110-ZNISSU                      PIC X(05).
      *��������
              05  SKT3110-ZTENSU                      PIC X(09).
      *���𡼰�����ô��
              05  SKT3110-ZITBFTN                     PIC X(09).
