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
        01  SC05o.
      *����ǯ����
            03  SC05o-SRYYM                          PIC X(16).
      *���ŵ�������ɽ��ʬ
            03  SC05o-TENHYOKBN-TBL                  OCCURS  4 TIMES.
              05  SC05o-TENHYOKBN                    PIC X(02).
      *���ŵ��إ�����
            03  SC05o-HOSPCD                         PIC X(40).
      *���ŵ��ؽ���
            03  SC05o-ADRS                           PIC X(100).
      *���ŵ���̾��
            03  SC05o-HOSPNAME                       PIC X(100).
      *�����ֹ�
            03  SC05o-TEL                            PIC X(15).
      *���߼�̾
            03  SC05o-KANRINAME                      PIC X(40).
      *����ơ��֥�
            03  SC05o-TBL                            OCCURS  25 TIMES.
      *��Į¼̾
              05  SC05o-TOWNNAME                     PIC X(16).
      *����
              05  SC05o-KIGO                         PIC X(02).
      *��ʬ
              05  SC05o-KBN                          PIC X(01).
      *���ճ��
              05  SC05o-KYUFURATE-TBL                OCCURS  5 TIMES.
                07  SC05o-KYURATE                    PIC X(02).
      *�����ơ��֥�
              05  SC05o-NYUGAIKBN-TBL                OCCURS  2 TIMES.
                07  SC05o-NYUGAIKBN                  PIC X(02).
      *���
              05  SC05o-KENSU                        PIC X(03).
      *����
              05  SC05o-TENSU                        PIC X(09).
      *ʡ������������
              05  SC05o-FKSGAKU                      PIC X(10).
