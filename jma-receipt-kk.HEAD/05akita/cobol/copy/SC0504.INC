      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        01  SC04.
      *��������
            03  SC04-PRTYM                  PIC X(16).
      *���ŵ��إ�����
            03  SC04-HOSPCDN                PIC X(20).
      *�ݸ���ʬ
            03  SC04-HKNKBN-TBL         OCCURS  4 TIMES.
                05  SC04-HKNKBN             PIC X(02).
      *ñ��ʻ�Ѷ�ʬ
            03  SC04-HEIKBN-TBL         OCCURS  3 TIMES.
                05  SC04-HEIKBN             PIC X(02).
      *�ܿͲ�²��ʬ
            03  SC04-HONKZKKBN-TBL      OCCURS  3 TIMES.
                05  SC04-HONKZKKBN          PIC X(02).
      *�����ʬ
            03  SC04-KOREIKBN-TBL       OCCURS  2 TIMES.
                05  SC04-KOREIKBN           PIC X(02).
      *Ϸ�ͻ�Į¼�ֹ�
            03  SC04-RJNFTNJANUM            PIC X(16).
      *������ô���ֹ�
            03  SC04-FTNJANUM
                                            PIC X(16) OCCURS  2 TIMES.
      *Ϸ�ͼ�����ֹ�
            03  SC04-RJNJKYSNUM             PIC X(14).
      *���������ֹ�
            03  SC04-JKYSNUM
                                            PIC X(14) OCCURS  2 TIMES.
      *�ݸ����ֹ�
            03  SC04-HKNJANUM               PIC X(16).
      *���ݸ��Ծڵ����ֹ�
            03  SC04-KIGO                   PIC X(34).
            03  SC04-BANGO                  PIC X(34).
      *�����ֹ�
            03  SC04-PTNUM                  PIC X(20).
      *����Ի�̾
            03  SC04-NAME                   PIC X(30).
      *��ǯ����
            03  SC04-BIRTHDAY               PIC X(08).
      *����
            03  SC04-SEX-TBL            OCCURS  2 TIMES.
                05  SC04-SEX                PIC X(02).
      *��ǯ������ǯ��
            03  SC04-NENGO-TBL          OCCURS  4 TIMES.
                05  SC04-NENGO              PIC X(02).
      *�õ�����
            03  SC04-TOKKI                  PIC X(08).
            03  SC04-ZAISOU                 PIC X(04).
            03  SC04-ZAIMARU                PIC X(02).
      *���ճ��
            03  SC04-KYURATE
                                            PIC X(02) OCCURS  4 TIMES.
      *���ŵ��ؽ����
            03  SC04-ADRS                   PIC X(100).
      *���ŵ���̾��
            03  SC04-HOSPNAME               PIC X(100).
      *���߼�̾
            03  SC04-KAISETUNAME            PIC X(80).
      *���ż�����
            03  SC04-NISSU
                                            PIC X(02) OCCURS  3 TIMES.
      *��������
            03  SC04-TENSU
                                            PIC X(09) OCCURS  3 TIMES.
      *������ô��
            03  SC04-FTNMONEY
                                            PIC X(07) OCCURS  3 TIMES.
      *����������
            03  SC04-SNISSU
                                            PIC X(02) OCCURS  3 TIMES.
      *������������
            03  SC04-SKINGK
                                            PIC X(09) OCCURS  3 TIMES.
      *������ɸ����ô��
            03  SC04-SFTNGK
                                            PIC X(09) OCCURS  3 TIMES.
