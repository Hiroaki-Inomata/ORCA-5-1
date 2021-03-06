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
      * 01.00.01     ����       11/07/13  ����̾�ɲ�
      *                                   ������˸����о������ɲ�
      * 01.00.02     ����       11/08/19  ���߼�̾�ΥХ��ȿ�����
      * 01.00.03     ����       11/09/22  ���ͣ��ɲ�(��������������ͳ)
      * 01.00.04     ����       12/07/05  ����̾����������ΥХ��ȿ�����
      * 01.00.05     ����       14/02/04  ���ͣ��ɲá�ʣ���������֡�
      * 01.00.06     ����       16/04/11  ���ͣ��ɲ�
      * 01.00.07     ���       18/09/05  ���ץ����Ȥ���
      *                                   ������ε��ܽ���
      * 01.00.08     ����       19/06/10  ��ǯ�����˿�����ι��ܤ��ɲ�
      *                                   ���������ౡ��������ι��ܤ��ɲ�
      *                                   ������¶�ʬ�ι��ܤ򣲤Ĥ��ѹ�
      ********************************************************************
       01  SC23.
      *ɽ��ǯ��
              03  SC23-PRTYM		PIC  X(16).
      *����ǯ����
              03  SC23-SEIYMD           PIC  X(22).
      *���ŵ��ؽ���
              03  SC23-ADRS             PIC  X(100).
      *���ŵ���̾��
              03  SC23-HOSPNAME         PIC  X(100).
      *���߼�̾
              03  SC23-KAISETU          PIC  X(60).
      *���ŵ��إ�����
              03  SC23-HOSPCD 		PIC  X(14).
      *���Ųʥ�����(�������±��Τ�)
              03  SC23-SRYKACD          PIC  X(02).
      *�ݸ���ʬ(������ ������ �������ȹ�)
              03  SC23-HKNBKN-TBL OCCURS 3.
                  05  SC23-HKNKBN       PIC X(02).
      *�ݸ�����(������ ��Ϸ�� ������ �����)
              03  SC23-HKNSYUBT-TBL OCCURS 4.
                  05  SC23-HKNSYUBT     PIC X(02).
      *�����������ô���
              03  SC23-ZNKKBN-TBL.
                  05  SC23-ZNK9         PIC  X(02).
                  05  SC23-ZNK8         PIC  X(02).
                  05  SC23-ZNK7         PIC  X(02).
      *������ô���
              03  SC23-ITBFTNWARI-TBL.
                  05  SC23-ITB1         PIC  X(02).
                  05  SC23-ITB2         PIC  X(02).
                  05  SC23-ITB3         PIC  X(02).
      *����
              03  SC23-ZAI1             PIC  X(02).
              03  SC23-ZAI2             PIC  X(02).
      *����
              03  SC23-BIKO             PIC  X(40).
      *���ԥǡ����ơ��֥�
              03  SC23-KANJA-TBL.
      *�ݸ����ֹ�
                  05  SC23-HKNJANUM     PIC  X(16).
      *���桦�ֹ�
                  05  SC23-KIGO         PIC  X(32).
                  05  SC23-BANGO        PIC  X(32).
      *��Į¼�ֹ�
                  05  SC23-CITYNUM      PIC  X(16).
      *Ϸ�Ͱ��š�������ֹ�
                  05  SC23-RJNJKYSNUM   PIC  X(14).
      *������ô���ֹ�
                  05  SC23-KOHFTNJANUM  PIC  X(16).
      *������ô���š�������ֹ�
                  05  SC23-KOHJKYSNUM   PIC  X(14).
      *���Ի�̾
                  05  SC23-PTNAME       PIC  X(24).
      *����
                  05  SC23-SEX1         PIC  X(02).
                  05  SC23-SEX2         PIC  X(02).
      *��ǯ����
                  05  SC23-TANJOG1      PIC X(02).
                  05  SC23-TANJOG2      PIC X(02).
                  05  SC23-TANJOG3      PIC X(02).
                  05  SC23-TANJOG4      PIC X(02).
                  05  SC23-TANJOG5      PIC X(02).
                  05  SC23-TANJOY       PIC X(02).
                  05  SC23-TANJOM       PIC X(02).
                  05  SC23-TANJOD       PIC X(02).
      *������ʬ(��������  ��������)
                  05  SC23-NYUGAIKBN1   PIC X(02).
                  05  SC23-NYUGAIKBN2   PIC X(02).
      *������������
                  05  SC23-N-SRYNISSU   PIC X(02).
      *������
                  05  SC23-NYUING       PIC X(04).
                  05  SC23-NYUINY       PIC X(02).
                  05  SC23-NYUINM       PIC X(02).
                  05  SC23-NYUIND       PIC X(02).
      *�ౡ��
                  05  SC23-TAIING       PIC X(04).
                  05  SC23-TAIINY       PIC X(02).
                  05  SC23-TAIINM       PIC X(02).
                  05  SC23-TAIIND       PIC X(02).
      *�����������
                  05  SC23-G-SRYNISSU   PIC X(02).
      *�������ơ��֥�
                  05  SC23-SRY-TBL.
                      07  SC23-DAY OCCURS 31 PIC X(02).
      *������¶�ʬ
                  05  SC23-TOKUTEI      OCCURS 2    PIC X(02) .
      *������������
                  05  SC23-TOTALTEN     PIC X(09).
      *������ô���
                  05  SC23-JIKOFTNGK    PIC X(07).
      *������ô��
                  05  SC23-KOHFTN       PIC X(07).
      *������
                  05  SC23-SEIKYUGK     PIC X(10).
      *������̶�ʬ
                  05  SC23-KOHKBN-TBL.
                      07  SC23-KOHKBN OCCURS 6 PIC X(02).
      *���٤����ް�����ô��
                  05  SC23-YAKUZAI-TBL.
                      07  SC23-YAKUZAI1 PIC X(20).
                      07  SC23-YAKUZAI2 PIC X(07).
                      07  SC23-YAKUZAI3 PIC X(02).
      *����̾�ڤӴ���
                  05  SC23-BYOMEI       PIC X(160).
      *�����о�����
                  05  SC23-TAHOTEN      PIC X(30).
      *���ͣ�
                  05  SC23-BIKO2        PIC X(40).
      *���ͣ�
                  05  SC23-BIKO3        PIC X(208).
      *���ͣ�
                  05  SC23-BIKO5        PIC X(90).
