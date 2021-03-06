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
        01  SC17.
      *����ʬ��ǯ��
            03  SC17-SRYYM                      PIC X(16).
      *����������
            03  SC17-SEIYMD                     PIC X(22).
      *���ŵ��إ�����
            03  SC17-HOSPCD                     PIC X(20).
      *���ŵ���̾��
            03  SC17-HOSPNAME                   PIC X(60).
      *���ŵ��ؽ����
            03  SC17-ADRS                       PIC X(60).
      *���ŵ��������ֹ�
            03  SC17-TEL                        PIC X(15).
      *��ɽ��̾
            03  SC17-KAISETU                    PIC X(60).
      *���̾
            03  SC17-GINKOU                     PIC X(20).
      *��Ź̾
            03  SC17-SITEN                      PIC X(20).
      *���¶�ʬ
            03  SC17-KOUZAKBN                   PIC X(02)
                                                OCCURS 2 TIMES.
      *�����ֹ�
            03  SC17-KOUZANUM                   PIC X(10).
      *�����ʬ
            03  SC17-KOHKBN
                                                PIC X(02)
                                                OCCURS  4 TIMES.
      *������ơ��֥�
            03  SC17-SEIKYU-TBL         OCCURS  10 TIMES.
      *������ֹ�
                05  SC17-JKYSNUM                PIC X(16).
      *����Ի�̾
                05  SC17-NAME                   PIC X(32).
      *��ǯ����
                05  SC17-TANJOYMD.
      *��ǯ
                    07  SC17-TANJOYY            PIC X(03).
      *����
                    07  SC17-TANJOMM            PIC X(02).
      *����
                    07  SC17-TANJODD            PIC X(02).
      *���Ų�
                05  SC17-SRYKA                  PIC X(08).
      *�ݸ�����
                05  SC17-HKNSYUBETU
                                                PIC X(02)
                                                OCCURS  4 TIMES.
      *����ǯ����
                05  SC17-NYUINYMD               PIC X(09).
      *������³
                05  SC17-KEIZOKU                PIC X(02).
      *��������
                05  SC17-NISSU                  PIC X(02).
      *����
                05  SC17-TENSU                  PIC X(09).
      *¾ˡ
                05  SC17-TAHO                   PIC X(07).
      *�����������
                05  SC17-SEIKYUGK               PIC X(09).
      *����
            03  SC17-SKENSU                     PIC X(03).
      *���
            03  SC17-GKENSU                     PIC X(03).
      *��������
            03  SC17-STENSU                     PIC X(09).
      *�������
            03  SC17-GTENSU                     PIC X(09).
      *¾ˡ����
            03  SC17-STAHO                      PIC X(09).
      *¾ˡ���
            03  SC17-GTAHO                      PIC X(09).
      *�����۾���
            03  SC17-SSEIKYUGK                  PIC X(09).
      *�����۹��
            03  SC17-GSEIKYUGK                  PIC X(09).
      *�����
            03  SC17-GPAGE                      PIC X(03).
      *�����
            03  SC17-PAGE                       PIC X(03).
      *��������
            03  SC17-GKSKGK                     PIC X(09).
